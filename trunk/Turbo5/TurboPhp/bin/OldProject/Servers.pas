unit Servers;

interface

uses
	LrProfiles;

type
	TServerTarget = ( stDisk, stFTP );
	TServerProfile = class(TLrProfile)
	private
		FRoot: string;
		FHost: string;
		FFTPHost: string;
		FFTPPassword: string;
		FFTPUser: string;
		FTarget: TServerTarget;
	protected
		procedure SetFTPHost(const Value: string);
		procedure SetFTPPassword(const Value: string);
		procedure SetFTPUser(const Value: string);
		procedure SetHost(const Value: string);
		procedure SetRoot(const Value: string);
		procedure SetTarget(const Value: TServerTarget);
	published
		property FTPHost: string read FFTPHost write SetFTPHost;
		property FTPUser: string read FFTPUser write SetFTPUser;
		property FTPPassword: string read FFTPPassword write SetFTPPassword;
		property Host: string read FHost write SetHost;
		property Root: string read FRoot write SetRoot;
		property Target: TServerTarget read FTarget write SetTarget;
	end;
	//
	TServerProfileMgr = class(TLrProfileMgr)
	protected
		function GetServers(inIndex: Integer): TServerProfile;
		procedure SetServers(inIndex: Integer; const Value: TServerProfile);
	public
		constructor Create(const inFilename: string = ''); reintroduce;
		function AddServer: TServerProfile;
		property Servers[inIndex: Integer]: TServerProfile read GetServers write SetServers; default;
	end;

implementation

uses
	Globals;

{ TServerProfile }

procedure TServerProfile.SetFTPHost(const Value: string);
begin
	FFTPHost := Value;
end;

procedure TServerProfile.SetFTPPassword(const Value: string);
begin
	FFTPPassword := Value;
end;

procedure TServerProfile.SetFTPUser(const Value: string);
begin
  FFTPUser := Value;
end;

procedure TServerProfile.SetHost(const Value: string);
begin
	FHost := Value;
end;

procedure TServerProfile.SetRoot(const Value: string);
begin
	FRoot := Value;
end;

procedure TServerProfile.SetTarget(const Value: TServerTarget);
begin
	FTarget := Value;
end;

{ TServerProfileMgr }

constructor TServerProfileMgr.Create(const inFilename: string);
begin
	inherited Create(TServerProfile, inFilename);
end;

function TServerProfileMgr.GetServers(inIndex: Integer): TServerProfile;
begin
	Result := TServerProfile(Profiles[inIndex]);
end;

procedure TServerProfileMgr.SetServers(inIndex: Integer;
	const Value: TServerProfile);
begin
	Profiles[inIndex] := Value;
end;

function TServerProfileMgr.AddServer: TServerProfile;
begin
	Result := TServerProfile(Profiles.Add);
end;

end.
