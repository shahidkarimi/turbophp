unit htDb;

interface

uses
	Classes, Messages, ADODB,
	htDatabaseProfile;

const
	DB_CHANGE = WM_USER + $100;

type
	ThtDb = class(TComponent)
	private
		FAdoConnection: TAdoConnection;
		FServerName: string;
		FUsers: TList;
	protected
		function GetConnected: Boolean;
		procedure SetConnected(const Value: Boolean);
		procedure SetProfile(const Value: ThtDatabaseProfile);
		procedure SetServerName(const Value: string);
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		procedure AddUser(inComponent: TComponent);
		procedure ListTables(inTables: TStrings);
		procedure RemoveUser(inComponent: TComponent);
		property AdoConnection: TAdoConnection read FAdoConnection;
		property Profile: ThtDatabaseProfile write SetProfile;
	published
		property Connected: Boolean read GetConnected write SetConnected;
		property ServerName: string read FServerName write SetServerName;
	end;
	//
	ThtDbTable = class(TComponent)
	private
		FColumns: TStringList;
		FDb: ThtDb;
		FAdoTable: TAdoTable;
	protected
		function GetActive: Boolean;
		function GetColumns: TStrings;
		function GetTableName: string;
		procedure DBChange(var inMessage: TMessage); message DB_CHANGE;
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
		procedure SetColumns(const Value: TStrings);
		procedure SetDb(const Value: ThtDb);
		procedure SetTableName(const Value: string);
		procedure SetActive(const Value: Boolean);
		procedure UpdateColumns;
		procedure UpdateTable;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		property AdoTable: TAdoTable read FAdoTable;
	published
		property Columns: TStrings read GetColumns write SetColumns;
		property Active: Boolean read GetActive write SetActive;
		property Db: ThtDb read FDb write SetDb;
		property TableName: string read GetTableName write SetTableName;
	end;

implementation

{ ThtDb }

constructor ThtDb.Create(inOwner: TComponent);
begin
	inherited;
	FAdoConnection := TAdoConnection.Create(inOwner);
	FUsers := TList.Create;
end;

destructor ThtDb.Destroy;
begin
	FUsers.Free;
	FAdoConnection.Free;
	inherited;
end;

procedure ThtDb.SetServerName(const Value: string);
begin
	if FServerName <> Value then
	begin
		FServerName := Value;
		Profile := DatabaseProfileMgr.DatabaseByName[ServerName];
	end;
end;

procedure ThtDb.SetProfile(const Value: ThtDatabaseProfile);
begin
	Connected := false;
	with Value do
		AdoConnection.ConnectionString := ODBC;
end;

function ThtDb.GetConnected: Boolean;
begin
	Result := AdoConnection.Connected;
end;

procedure ThtDb.SetConnected(const Value: Boolean);
begin
	AdoConnection.Connected := Value;
end;

procedure ThtDb.ListTables(inTables: TStrings);
begin
	AdoConnection.GetTableNames(inTables, true);
end;

procedure ThtDb.AddUser(inComponent: TComponent);
begin
	FUsers.Add(inComponent);
	FreeNotification(inComponent);
end;

procedure ThtDb.RemoveUser(inComponent: TComponent);
begin
	FUsers.Remove(inComponent);
	RemoveFreeNotification(inComponent);
end;

{ ThtDbTable }

constructor ThtDbTable.Create(inOwner: TComponent);
begin
	inherited;
	FAdoTable := TAdoTable.Create(Self);
	FColumns := TStringList.Create;
end;

destructor ThtDbTable.Destroy;
begin
	FColumns.Free;
	inherited;
end;

function ThtDbTable.GetColumns: TStrings;
begin
	Result := FColumns;
end;

procedure ThtDbTable.SetColumns(const Value: TStrings);
begin
	FColumns.Assign(Value);
end;

procedure ThtDbTable.SetDb(const Value: ThtDb);
begin
	if FDb <> Value then
	begin
		if FDb <> nil then
			FDb.RemoveUser(Self);
		FDb := Value;
		if FDb <> nil then
			FDb.AddUser(Self);
		UpdateTable;
	end;
end;

procedure ThtDbTable.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	if Operation = opRemove then
		if AComponent = FDb then
			FDb := nil;
end;

function ThtDbTable.GetActive: Boolean;
begin
	Result := AdoTable.Active;
//	Result := (Db <> nil) and Db.Active;
end;

procedure ThtDbTable.SetActive(const Value: Boolean);
begin
	AdoTable.Active := Value;
//	if (Db <> nil) and Value then
//		Db.Connected := Value;
end;

function ThtDbTable.GetTableName: string;
begin
	Result := AdoTable.TableName;
end;

procedure ThtDbTable.SetTableName(const Value: string);
begin
	AdoTable.TableName := Value;
	UpdateTable;
end;

procedure ThtDbTable.DBChange(var inMessage: TMessage);
begin
	UpdateTable;
end;

procedure ThtDbTable.UpdateTable;
begin
	Active := false;
	if Db = nil then
		AdoTable.Connection := nil
	else begin
		AdoTable.Connection := Db.AdoConnection;
		Active := true;
		UpdateColumns;
	end;
end;

procedure ThtDbTable.UpdateColumns;
begin
	if Active and (Columns.Count = 0) then
		Columns.Assign(AdoTable.FieldList);
end;

end.
