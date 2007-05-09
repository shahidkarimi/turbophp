unit htDatabases;

interface

uses
	Classes,
	LrProject;

type
	ThtDatabaseVendor = ( dvMySQL, dvFireBird, dvInterbase5, dvInterbase6,
		dvAccess, dvODBC, dvCustom );
	//
	ThtDatabaseItem = class(TLrProjectItem)
	private
		FADODB: string;
		FPassword: string;
		FDatabaseFile: string;
		FODBC: string;
		FDatabase: string;
		FUser: string;
		FVendor: ThtDatabaseVendor;
		FServerHost: string;
	protected
		procedure SetADODB(const Value: string);
		procedure SetDatabase(const Value: string);
		procedure SetDatabaseFile(const Value: string);
		procedure SetODBC(const Value: string);
		procedure SetPassword(const Value: string);
		procedure SetServerHost(const Value: string);
		procedure SetUser(const Value: string);
		procedure SetVendor(const Value: ThtDatabaseVendor);
	published
		property ADODB: string read FADODB write SetADODB;
		property Database: string read FDatabase write SetDatabase;
		property DatabaseFile: string read FDatabaseFile write SetDatabaseFile;
		property ODBC: string read FODBC write SetODBC;
		property Password: string read FPassword write SetPassword;
		property ServerHost: string read FServerHost write SetServerHost;
		property User: string read FUser write SetUser;
		property Vendor: ThtDatabaseVendor read FVendor write SetVendor;
	end;
	//
	ThtDatabasesItem = class(TLrProjectItem)
	protected
		function GetDatabaseByDisplayName(const inName: string): ThtDatabaseItem;
		function GetDatabaseByName(const inName: string): ThtDatabaseItem;
		function GetDatabases(inIndex: Integer): ThtDatabaseItem;
	public
		function AddDatabase: ThtDatabaseItem;
		property Databases[inIndex: Integer]: ThtDatabaseItem
			read GetDatabases; default;
		property DatabaseByName[const inName: string]: ThtDatabaseItem
			read GetDatabaseByName;
		property DatabaseByDisplayName[const inName: string]: ThtDatabaseItem
			read GetDatabaseByDisplayName;
	end;

implementation

{ ThtDatabaseItem }

procedure ThtDatabaseItem.SetADODB(const Value: string);
begin
	FADODB := Value;
end;

procedure ThtDatabaseItem.SetDatabase(const Value: string);
begin
	FDatabase := Value;
end;

procedure ThtDatabaseItem.SetDatabaseFile(const Value: string);
begin
	FDatabaseFile := Value;
end;

procedure ThtDatabaseItem.SetODBC(const Value: string);
begin
	FODBC := Value;
end;

procedure ThtDatabaseItem.SetPassword(const Value: string);
begin
	FPassword := Value;
end;

procedure ThtDatabaseItem.SetServerHost(const Value: string);
begin
	FServerHost := Value;
end;

procedure ThtDatabaseItem.SetUser(const Value: string);
begin
	FUser := Value;
end;

procedure ThtDatabaseItem.SetVendor(const Value: ThtDatabaseVendor);
begin
	FVendor := Value;
end;

{ ThtDatabasesItem }

function ThtDatabasesItem.GetDatabases(inIndex: Integer): ThtDatabaseItem;
begin
	Result := ThtDatabaseItem(Items[inIndex]);
end;

function ThtDatabasesItem.AddDatabase: ThtDatabaseItem;
begin
	Result := ThtDatabaseItem.Create;
	Add(Result);
end;

function ThtDatabasesItem.GetDatabaseByName(
	const inName: string): ThtDatabaseItem;
begin
	Result := ThtDatabaseItem(Find(inName));
end;

function ThtDatabasesItem.GetDatabaseByDisplayName(
	const inName: string): ThtDatabaseItem;
var
	i: Integer;
begin
	Result := nil;
	for i := 0 to Pred(Count) do
		if Databases[i].DisplayName = inName then
		begin
			Result := Databases[i];
			break;
		end;
end;

initialization
	RegisterClass(ThtDatabaseItem);
end.
