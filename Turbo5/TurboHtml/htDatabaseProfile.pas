unit htDatabaseProfile;

interface

uses
	LrProfiles;

type
	ThtDatabaseVendor = ( dvMySQL, dvFireBird, dvInterbase5, dvInterbase6,
		dvAccess, dvODBC, dvCustom );
	//
	ThtDatabaseProfile = class(TLrProfile)
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
	ThtDatabaseProfileMgr = class(TLrProfileMgr)
  private
		function GetDatabaseByName(const inName: string): ThtDatabaseProfile;
	protected
		function GetDatabases(inIndex: Integer): ThtDatabaseProfile;
		procedure SetDatabases(inIndex: Integer; const Value: ThtDatabaseProfile);
	public
		constructor Create(const inFilename: string = ''); reintroduce;
		function AddDatabase: ThtDatabaseProfile;
		property Databases[inIndex: Integer]: ThtDatabaseProfile read GetDatabases
			write SetDatabases; default;
		property DatabaseByName[const inName: string]: ThtDatabaseProfile
			read GetDatabaseByName;
	end;

var
	DatabaseProfileMgr: ThtDatabaseProfileMgr;

implementation

{ ThtDatabaseProfile }

procedure ThtDatabaseProfile.SetADODB(const Value: string);
begin
	FADODB := Value;
end;

procedure ThtDatabaseProfile.SetDatabase(const Value: string);
begin
	FDatabase := Value;
end;

procedure ThtDatabaseProfile.SetDatabaseFile(const Value: string);
begin
	FDatabaseFile := Value;
end;

procedure ThtDatabaseProfile.SetODBC(const Value: string);
begin
	FODBC := Value;
end;

procedure ThtDatabaseProfile.SetPassword(const Value: string);
begin
	FPassword := Value;
end;

procedure ThtDatabaseProfile.SetServerHost(const Value: string);
begin
	FServerHost := Value;
end;

procedure ThtDatabaseProfile.SetUser(const Value: string);
begin
	FUser := Value;
end;

procedure ThtDatabaseProfile.SetVendor(const Value: ThtDatabaseVendor);
begin
	FVendor := Value;
end;

{ ThtDatabaseProfileMgr }

constructor ThtDatabaseProfileMgr.Create(const inFilename: string);
begin
	inherited Create(ThtDatabaseProfile, inFilename);
end;

function ThtDatabaseProfileMgr.GetDatabases(inIndex: Integer): ThtDatabaseProfile;
begin
	Result := ThtDatabaseProfile(Profiles[inIndex]);
end;

procedure ThtDatabaseProfileMgr.SetDatabases(inIndex: Integer;
	const Value: ThtDatabaseProfile);
begin
	Profiles[inIndex] := Value;
end;

function ThtDatabaseProfileMgr.AddDatabase: ThtDatabaseProfile;
begin
	Result := ThtDatabaseProfile(Profiles.Add);
end;

function ThtDatabaseProfileMgr.GetDatabaseByName(
	const inName: string): ThtDatabaseProfile;
begin
	Result := ThtDatabaseProfile(GetProfileByName(inName));
end;

end.
