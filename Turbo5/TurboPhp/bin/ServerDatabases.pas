unit ServerDatabases;

interface

uses
	Classes, LrProfiles, htDatabaseProfile;

type
	TServerDatabaseProfile = class(TLrProfile)
	private
		FDatabases: ThtDatabaseProfileMgr;
		//FServer: TLrProfileIdent;
	protected
		procedure SetDatabases(const Value: ThtDatabaseProfileMgr);
		//procedure SetServer(const Value: TLrProfileIdent);
	public
		constructor Create(inCollection: TCollection); override;
		destructor Destroy; override;
	published
		//property Server: TLrProfileIdent read FServer write SetServer;
		property Databases: ThtDatabaseProfileMgr read FDatabases
			write SetDatabases;
	end;
	//
	TServerDatabaseProfileMgr = class(TLrProfileMgr)
	protected
		function GetDatabases(inIndex: Integer): TServerDatabaseProfile;
		procedure SetDatabases(inIndex: Integer;
			const Value: TServerDatabaseProfile);
	public
		constructor Create(const inFilename: string = ''); reintroduce;
		function AddDatabase: TServerDatabaseProfile;
		property Count: Integer read GetCount;
		property Databases[inIndex: Integer]: TServerDatabaseProfile read
			GetDatabases write SetDatabases; default;
	end;

implementation

uses
	Globals;

{ TServerDatabaseProfile }

constructor TServerDatabaseProfile.Create(inCollection: TCollection);
begin
	inherited;
	FDatabases := ThtDatabaseProfileMgr.Create;
	FDatabases.SetSubComponent(true);
end;

destructor TServerDatabaseProfile.Destroy;
begin
	FDatabases.Free;
	inherited;
end;

procedure TServerDatabaseProfile.SetDatabases(
	const Value: ThtDatabaseProfileMgr);
begin
	FDatabases := Value;
end;

{
procedure TServerDatabaseProfile.SetServer(const Value: TLrProfileIdent);
begin
	FServer := Value;
end;
}

{ TServerDatabaseProfileMgr }

constructor TServerDatabaseProfileMgr.Create(const inFilename: string);
begin
	inherited Create(TServerDatabaseProfile, inFilename);
end;

function TServerDatabaseProfileMgr.GetDatabases(
	inIndex: Integer): TServerDatabaseProfile;
begin
	Result := TServerDatabaseProfile(Profiles[inIndex]);
end;

procedure TServerDatabaseProfileMgr.SetDatabases(inIndex: Integer;
	const Value: TServerDatabaseProfile);
begin
	Profiles[inIndex] := Value;
end;

function TServerDatabaseProfileMgr.AddDatabase: TServerDatabaseProfile;
begin
	Result := TServerDatabaseProfile(Profiles.Add);
end;

end.
