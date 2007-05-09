unit ThDbListSource;

interface

uses
	Classes, DB, DBCtrls,
	ThListSource, ThDbData;

type
	TThDbListSource = class(TThListSource)
	private
		FData: TThDbData;
		FItems: TStringList;
		FItemsLoaded: Boolean;
	protected
		function GetDataSet: TDataSet;
		function GetDataSource: TDataSource;
		function GetFieldName: string;
		function GetItems: TStrings; override;
		procedure SetFieldName(const Value: string);
		procedure SetDataSource(const Value: TDataSource);
	protected
		procedure ActiveChange(Sender: TObject);
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
	protected
		property Data: TThDbData read FData;
		property DataSet: TDataSet read GetDataSet;
		property ItemsLoaded: Boolean read FItemsLoaded write FItemsLoaded;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure UpdateItems; override;
	published
		property FieldName: string read GetFieldName write SetFieldName;
		property DataSource: TDataSource read GetDataSource write SetDataSource;
	end;

implementation

{ TThDbListSource }

constructor TThDbListSource.Create(AOwner: TComponent);
begin
	inherited;
	FItems := TStringList.Create;
	FData := TThDbData.Create;
	FData.OnActiveChange := ActiveChange;
end;

destructor TThDbListSource.Destroy;
begin
	FItems.Free;
	FData.Free;
	inherited;
end;

function TThDbListSource.GetItems: TStrings;
begin
	Result := FItems;
end;

procedure TThDbListSource.UpdateItems;
var
	b: TBookmark;
begin
	ItemsLoaded := false;
	Items.BeginUpdate;
	try
		Items.Clear;
		if Data.Active then
		begin
			b := DataSet.GetBookmark;
			DataSet.First;
			while not DataSet.Eof do
			begin
				Items.Add(Data.FieldText);
				DataSet.Next;
			end;
			DataSet.GotoBookmark(b);
			DataSet.FreeBookmark(b);
			ItemsLoaded := true;
		end;
	finally
		Items.EndUpdate;
	end;
	Notifiers.Notify;
end;

procedure TThDbListSource.ActiveChange(Sender: TObject);
begin
	UpdateItems;
end;

function TThDbListSource.GetFieldName: string;
begin
	Result := Data.FieldName;
end;

function TThDbListSource.GetDataSet: TDataSet;
begin
	Result := Data.DataSet;
end;

function TThDbListSource.GetDataSource: TDataSource;
begin
	Result := Data.DataSource;
end;

procedure TThDbListSource.SetFieldName(const Value: string);
begin
	FData.FieldName := Value;
end;

procedure TThDbListSource.SetDataSource(const Value: TDataSource);
begin
	if DataSource <> nil then
		DataSource.RemoveFreeNotification(Self);
	Data.DataSource := Value;
	if DataSource <> nil then
		DataSource.FreeNotification(Self);
end;

procedure TThDbListSource.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	Data.Notification(AComponent, Operation);
end;

end.
