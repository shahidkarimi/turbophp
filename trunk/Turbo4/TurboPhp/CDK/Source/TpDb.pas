unit TpDb;

interface

uses
	SysUtils, Classes,
	Db, AdoDb, TypInfo,
	ThHtmlDocument, ThHeaderComponent, ThNotifierList, ThTag, ThListSource, ThDbData,
	ThDataConnection, ThRegExpr, TpControls, TpInterfaces;

type
	TTpDb = class(TThHeaderComponent, ITpIncludeLister)
	private
		FDesignConnection: TThDataConnection;
		FDebug: Boolean;
	protected
		procedure SetDesignConnection(const Value: TThDataConnection);
	protected
		procedure ListPhpIncludes(inIncludes: TStringList); virtual;
		procedure Tag(inTag: TThTag); override;
	public
		constructor Create(inOwner: TComponent); override;
	public
		property DesignConnection: TThDataConnection read FDesignConnection
			write SetDesignConnection;
	published
		property Debug: Boolean read FDebug write FDebug;
	end;
	//
	TTpDataSource = class(TThHeaderComponent, IThListSource)
	private
		FDataSource: TDataSource;
		FDb: TTpDb;
		FDebug: Boolean;
		FDesignOnly: Boolean;
		FItems: TStringList;
		FUpdating: Boolean;
		FNotifiers: TThNotifierList;
		FOnBeforeExecute: TTpEvent;
		FOnFailure: TTpEvent;
		FOnSuccess: TTpEvent;
		FWantsActive: Boolean;
	protected
		function GetActive: Boolean;
		function GetDataSet: TCustomADODataSet; virtual; abstract;
		function GetItems: TStrings;
		function GetNotifiers: TThNotifierList;
		procedure SetActive(const Value: Boolean);
		procedure SetDataSet(const Value: TCustomADODataSet);
		procedure SetDb(const Value: TTpDb); virtual;
		procedure SetDesignOnly(const Value: Boolean);
	protected
		procedure DataChange(Sender: TObject; Field: TField);
		procedure DbChanged; virtual;
		procedure Loaded; override;
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
		procedure Tag(inTag: TThTag); override;
		procedure UpdateItems;
	protected
		property Active: Boolean read GetActive write SetActive default false;
		property Db: TTpDb read FDb write SetDb;
	public
		constructor Create(inOwner: TComponent); override;
    destructor Destroy; override;
	public
		property DataSet: TCustomADODataSet read GetDataSet write SetDataSet;
		property DataSource: TDataSource read FDataSource;
		property DesignOnly: Boolean read FDesignOnly write SetDesignOnly
			default false;
		property OnBeforeExecute: TTpEvent read FOnBeforeExecute
			write FOnBeforeExecute;
		property OnFailure: TTpEvent read FOnFailure
			write FOnFailure;
		property OnSuccess: TTpEvent read FOnSuccess
			write FOnSuccess;
	published
		property Debug: Boolean read FDebug write FDebug;
	end;
	//
	TTpDataTable = class(TTpDataSource)
	private
		FTable: TAdoTable;
	protected
		function GetDataSet: TCustomADODataSet; override;
		function GetTableName: string;
		procedure SetTableName(const Value: string);
	protected
		procedure Tag(inTag: TThTag); override;
	public
		constructor Create(inOwner: TComponent); override;
	published
		property Active;
		property Db;
		property DesignOnly;
		property OnBeforeExecute;
		property OnFailure;
		property OnSuccess;
		property TableName: string read GetTableName write SetTableName;
	end;
	//
	TTpDataQuery = class(TTpDataSource)
	private
		FParams: TStringList;
		FQuery: TAdoQuery;
		FSQL: TStringList;
	protected
		function GetDataSet: TCustomADODataSet; override;
		procedure SetParams(const Value: TStringList);
		procedure SetSQL(const Value: TStringList);
	protected
		function DoParamReplace(ARegExpr: TRegExpr): string;
		function ParamsReplace(const inText: string): string;
		procedure SqlChange(inSender: TObject);
		procedure Tag(inTag: TThTag); override;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
	published
		property Active;
		property Db;
		property DesignOnly;
		property OnBeforeExecute;
		property OnFailure;
		property OnSuccess;
		property Params: TStringList read FParams write SetParams;
		property SQL: TStringList read FSQL write SetSQL;
	end;

procedure TpSetDataSource(inOwner: TComponent; Value: TTpDataSource;
	var ioSrcProp: TTpDataSource; inDataProp: TThDbData);

implementation

uses
	DCPbase64;

const
	cTpParamExpression = '\{%([^}]*)\}';

procedure TpSetDataSource(inOwner: TComponent; Value: TTpDataSource;
	var ioSrcProp: TTpDataSource; inDataProp: TThDbData);
begin
	if ioSrcProp <> nil then
		ioSrcProp.RemoveFreeNotification(inOwner);
	//
	ioSrcProp := Value;
	//
	if ioSrcProp <> nil then
		ioSrcProp.FreeNotification(inOwner);
	//
	if ioSrcProp <> nil then
		inDataProp.DataSource := ioSrcProp.DataSource
	else
		inDataProp.DataSource := nil;
end;

{ TTpDb }

constructor TTpDb.Create(inOwner: TComponent);
begin
	inherited;
	FDesignConnection := TThDataConnection.Create(Self);
end;

procedure TTpDb.SetDesignConnection(const Value: TThDataConnection);
begin
	FDesignConnection.Assign(Value);
end;

procedure TTpDb.ListPhpIncludes(inIncludes: TStringList);
begin
	inIncludes.Add('TpDb.php');
end;

procedure TTpDb.Tag(inTag: TThTag);
begin
	inTag.Attributes.Add('tpDebugFlag', Debug);
end;

{ TTpDataSource }

constructor TTpDataSource.Create(inOwner: TComponent);
begin
	inherited;
	FDataSource := TDataSource.Create(Self);
	FDataSource.OnDataChange := DataChange;
	FNotifiers := TThNotifierList.Create;
end;

destructor TTpDataSource.Destroy;
begin
	FNotifiers.Free;
	FItems.Free;
	inherited;
end;

procedure TTpDataSource.Loaded;
begin
	inherited;
	Active := FWantsActive;
end;

procedure TTpDataSource.DbChanged;
begin
	if (Db <> nil) and (DataSet <> nil) then
		DataSet.Connection := Db.DesignConnection.Connection;
end;

procedure TTpDataSource.SetDb(const Value: TTpDb);
begin
	if FDb <> Value then
	begin
		if FDb <> nil then
			FDb.RemoveFreeNotification(Self);
		FDb := Value;
		if FDb <> nil then
			FDb.FreeNotification(Self);
		DbChanged;
	end;
end;

procedure TTpDataSource.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	if Operation = opRemove then
		if AComponent = FDb then
		begin
			FDb := nil;
			DbChanged;
		end;
end;

function TTpDataSource.GetActive: Boolean;
begin
	if (DataSet = nil) then
		Result := false
	else
		Result := DataSet.Active;
end;

procedure TTpDataSource.SetActive(const Value: Boolean);
begin
	if (Active <> Value) then
		if csLoading in ComponentState then
			FWantsActive := true
		else
			if not Value then
				DataSet.Active := false
			else if (Db <> nil) and (DataSet <> nil) then
			try
				Db.DesignConnection.Connected := true;
				DataSet.Active := Db.DesignConnection.Connected;
			except
			end;
end;

procedure TTpDataSource.SetDataSet(const Value: TCustomADODataSet);
begin
	FDataSource.DataSet := Value;
end;

procedure TTpDataSource.SetDesignOnly(const Value: Boolean);
begin
	FDesignOnly := Value;
end;

procedure TTpDataSource.Tag(inTag: TThTag);
begin
	with inTag do
	begin
		if Db <> nil then
			Add('tpDb', Db.Name);
		Attributes.Add('tpDebugFlag', Debug);
		Add('tpOnBeforeExecute', OnBeforeExecute);
		Add('tpOnFailure', OnFailure);
		Add('tpOnSuccess', OnSuccess);
		Add('tpName', Name);
	end;
end;

procedure TTpDataSource.DataChange(Sender: TObject; Field: TField);
begin
	if not FUpdating then
	begin
		UpdateItems;
		FNotifiers.Notify;
	end;
end;

procedure TTpDataSource.UpdateItems;
var
	b: TBookmark;
begin
	if FItems <> nil then
	begin
		//ItemsLoaded := false;
		FItems.BeginUpdate;
		FUpdating := true;
		try
			FItems.Clear;
			if Active and (DataSet.FieldCount > 0) then
			begin
				b := DataSet.GetBookmark;
				DataSet.First;
				while not DataSet.Eof do
				begin
					FItems.Add(DataSet.Fields[0].AsString);
					DataSet.Next;
				end;
				DataSet.GotoBookmark(b);
				DataSet.FreeBookmark(b);
				//ItemsLoaded := true;
			end;
		finally
			FUpdating := false;
			FItems.EndUpdate;
		end;
		//Notifiers.Notify;
	end;
end;

function TTpDataSource.GetItems: TStrings;
begin
	if FItems = nil then
	begin
		FItems := TStringList.Create;
		UpdateItems;
	end;
	Result := FItems;
end;

function TTpDataSource.GetNotifiers: TThNotifierList;
begin
	Result := FNotifiers;
end;

{ TTpDataTable }

constructor TTpDataTable.Create(inOwner: TComponent);
begin
	inherited;
	FTable := TAdoTable.Create(Self);
	DataSet := FTable;
end;

procedure TTpDataTable.SetTableName(const Value: string);
begin
	FTable.TableName := Value;
end;

function TTpDataTable.GetDataSet: TCustomADODataSet;
begin
	Result := FTable;
end;

function TTpDataTable.GetTableName: string;
begin
	Result := FTable.TableName;
end;

procedure TTpDataTable.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpDataTable');
		Add('tpSQL', Base64EncodeStr('SELECT * from ' + TableName));
	end;
end;

{ TTpDataQuery }

constructor TTpDataQuery.Create(inOwner: TComponent);
begin
	inherited;
	FSql := TStringList.Create;
	FSql.OnChange := SqlChange;
	FParams := TStringList.Create;
	FParams.OnChange := SqlChange;
	FQuery := TAdoQuery.Create(Self);
	DataSet := FQuery;
end;

destructor TTpDataQuery.Destroy;
begin
	FParams.Free;
	FSql.Free;
	inherited;
end;

function TTpDataQuery.GetDataSet: TCustomADODataSet;
begin
	Result := FQuery;
end;

procedure TTpDataQuery.SetParams(const Value: TStringList);
begin
	FParams.Assign(Value);
end;

procedure TTpDataQuery.SetSQL(const Value: TStringList);
begin
	FSql.Assign(Value);
end;

function TTpDataQuery.DoParamReplace(ARegExpr: TRegExpr): string;
begin
	try
		Result := FParams.Values[ARegExpr.Match[1]];
	except
		Result := '';
	end;
	//ThFindContent(Owner, ARegExpr.Match[1], Result);
end;

function TTpDataQuery.ParamsReplace(const inText: string): string;
begin
	with TRegExpr.Create do
	try
		Expression := cTpParamExpression;
		Result := ReplaceEx(inText, DoParamReplace);
	finally
		Free;
	end;
end;

procedure TTpDataQuery.SqlChange(inSender: TObject);
begin
	//FQuery.SQL.Assign(Sql);
	FQuery.SQL.Text := ParamsReplace(Sql.Text);
end;

procedure TTpDataQuery.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpDataQuery');
		Add('tpSQL', Base64EncodeStr(FSql.Text));
		//Add('tpParams', Base64EncodeStr(FParams.Text));
	end;
end;

end.
