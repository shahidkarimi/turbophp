unit tpGrid;

interface

uses
	SysUtils, Classes, Controls, Graphics, Forms,
	DB, ADODB,
	VirtualTrees,
	LrGraphics, LrTextPainter,
	htDatabases, htDocument, htComponent, htControls, htGeneric;

type
	TtpGrid = class(ThtCustomControl)
	private
		FDataSet: TAdoDataSet;
		FGrid: TVirtualStringTree;
		FDataSource: string;
		FConnectionStatus: string;
		FTableName: string;
	protected
		procedure Connect;
		procedure CreateConnection;
		procedure CreateGrid;
		procedure Disconnect;
		procedure Generate(const inContainer: string;
			inDocument: ThtDocument); override;
		function GetConnected: Boolean;
		procedure GridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
			Column: TColumnIndex; TextType: TVSTTextType;
			var CellText: WideString);
		procedure SetConnected(const Value: Boolean);
		procedure SetConnectionStatus(const Value: string);
		procedure SetDataSource(const Value: string);
		procedure SetTableName(const Value: string);
		procedure UpdateData;
		procedure UpdateGrid;
		procedure UpdateHeader;
	public
		constructor Create(inOwner: TComponent); override;
	published
		property Align;
		property Connected: Boolean read GetConnected write SetConnected;
		property ConnectionStatus: string read FConnectionStatus
			write SetConnectionStatus;
		property DataSource: string read FDataSource write SetDataSource;
		property TableName: string read FTableName write SetTableName;
		//property Outline;
		property Style;
	end;

implementation

uses
	Project, Controller;

{ TtpGrid }

constructor TtpGrid.Create(inOwner: TComponent);
begin
	inherited;
	CreateGrid;
	CreateConnection;
end;

procedure TtpGrid.CreateConnection;
begin
	FDataSet := TAdoDataSet.Create(Self);
	FDataSet.CommandType := cmdTable;
	FDataSet.CursorLocation := clUseClient;
	//FDataSet.LoginPrompt := false;
	Disconnect;
	if (DataSource = '') and (Controller.Project.Databases.Count > 0) then
		DataSource := Controller.Project.Databases[0].DisplayName;
end;

procedure TtpGrid.CreateGrid;
begin
	FGrid := TVirtualStringTree.Create(Self);
	with FGrid do
	begin
		Parent := Self;
		Visible := true;
		Align := alClient;
		BevelInner := bvNone;
		BevelKind := bkFlat;
		BorderStyle := bsNone;
		Header.Options := Header.Options + [ hoVisible ];
		Header.Style := hsXPStyle;
		TreeOptions.MiscOptions :=
			TreeOptions.MiscOptions + [toGridExtensions{, toReadOnly}];
		TreeOptions.PaintOptions :=
			TreeOptions.PaintOptions
				+ [ toHotTrack, toShowVertGridLines, toShowHorzGridLines ]
					- [ toShowButtons, toShowRoot ];
		OnGetText := GridGetText;
	end;
end;

procedure TtpGrid.Connect;
begin
	if not FDataSet.Active and (TableName <> '') then
	try
		FDataSet.Active := true;
		ConnectionStatus := 'Connected';
	except
		on E: Exception do
			ConnectionStatus := E.Message;
	end;
	UpdateGrid;
end;

procedure TtpGrid.Disconnect;
begin
	FDataSet.Active := false;
	ConnectionStatus := 'Disconnected';
	UpdateGrid;
end;

procedure TtpGrid.UpdateGrid;
begin
	if Connected then
	begin
		UpdateHeader;
		UpdateData;
	end else
	begin
		UpdateData;
		UpdateHeader;
	end;
end;

procedure TtpGrid.SetConnectionStatus(const Value: string);
begin
	FConnectionStatus := Value;
end;

function TtpGrid.GetConnected: Boolean;
begin
	Result := FDataSet.Active;
end;

procedure TtpGrid.SetConnected(const Value: Boolean);
begin
	if Value then
		Connect
	else
		Disconnect;
end;

procedure TtpGrid.SetDataSource(const Value: string);

	procedure SetDatabase(inDatabase: ThtDatabaseItem);
	begin
		if inDatabase <> nil then
		begin
			FDataSet.ConnectionString := inDatabase.ODBC;
			Connect;
		end;
	end;

begin
	if FDataSource <> Value then
	begin
		FDataSource := Value;
		Disconnect;
		SetDatabase(
			Controller.Project.Databases.DatabaseByDisplayName[FDataSource]);
	end;
end;

procedure TtpGrid.SetTableName(const Value: string);
begin
	if (Value <> FTableName) then
	begin
		FTableName := Value;
		Disconnect;
		FDataSet.CommandText := FTableName;
		Connect;
	end;
end;

procedure TtpGrid.UpdateHeader;
var
	i: Integer;
begin
	FGrid.Header.Columns.BeginUpdate;
	try
		FGrid.Header.Columns.Clear;
		if Connected then
		begin
			for i := 0 to Pred(FDataSet.FieldCount) do
				with FGrid.Header.Columns.Add do
					with FDataSet.FieldDefs[i] do
					begin
						Text := DisplayName;
						if (Size > 0) then
							Width := Size * 16;
					end;
		end;
	finally
		FGrid.Header.Columns.EndUpdate;
	end;
end;

procedure TtpGrid.UpdateData;
begin
	if Connected then
		FGrid.RootNodeCount := FDataSet.RecordCount
	else
		FGrid.RootNodeCount := 0;
end;

procedure TtpGrid.GridGetText(Sender: TBaseVirtualTree;
	Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
	var CellText: WideString);
begin
	FDataSet.RecNo := Node.Index + 1;
	CellText := FDataSet.Fields[Column].AsString;
end;

procedure TtpGrid.Generate(const inContainer: string;
	inDocument: ThtDocument);
var
	s: string;
begin
	with inDocument do
	begin
		Add(Format('<div id="%s"%s>', [ Name, ExtraAttributes ]));
		Add('<img src="images/table.png" border="0" dojoType="Grid" />');
		Add('</div>');
		Styles.Add(
			Format('#%s { position: relative; width: 98%%; height: %dpx; overflow: hidden;}',
				[ Name, Height ]));
		s := Style.InlineAttribute;
		if (s <> '') then
			inDocument.Styles.Add(Format('#%s { %s }', [ Name, s ]));
		//
		with CreateFunctionTag('loadGrid(inData)') do
		begin
			Add(
				'grid.DataSource.setData(inData);'#13 +
				'grid.buildGrid();'#13 +
				'// IE needs another chance to fix scrollbars'#13 +
				'window.setTimeout(grid.doResize, 100);'
			);
		end;
		//
		with InitCode do
		begin
			Add(
				'grid = dojo.webui.widgetManager.getWidgetsOfType("Grid")[0];'#13
				+ 'grid.DataSource = new dojo.dataSource();'#13
				+ 'turbo.rpc.call("taj/tajServer.php", [ [ "getRows", "' + TableName + '" ] ], loadGrid);'#13
				//'grid.DataSource.setData([ [ "Alpha", "Beta" ], [ "A", "B" ], [ "A", "B" ], [ "A", "B" ], [ "A", "B" ] ]);'#13 +
				//'grid.buildGrid();'#13
			);
		end;
	end;
end;

initialization
	RegisterClass(TVirtualStringTree);
end.
