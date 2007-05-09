unit Servers;

interface

uses
	Classes,
	LrProject;

type
	TServerTarget = ( stDisk, stFTP );
	//
	TServerItem = class(TLrProjectItem)
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
	TServersItem = class(TLrProjectItem)
	private
		FDefaultServer: TServerItem;
		FDefaultServerName: string;
	protected
		function GetDefaultServer: TServerItem;
		function GetServers(inIndex: Integer): TServerItem;
		procedure SetDefaultServer(const Value: TServerItem);
		procedure SetDefaultServerName(const Value: string);
	public
		constructor Create; override;
		property DefaultServer: TServerItem read GetDefaultServer
			write SetDefaultServer;
		property Servers[inIndex: Integer]: TServerItem read GetServers; default;
	published
		property DefaultServerName: string read FDefaultServerName
			write SetDefaultServerName;
	end;

implementation

{ TServerItem }

procedure TServerItem.SetFTPHost(const Value: string);
begin
	FFTPHost := Value;
end;

procedure TServerItem.SetFTPPassword(const Value: string);
begin
	FFTPPassword := Value;
end;

procedure TServerItem.SetFTPUser(const Value: string);
begin
	FFTPUser := Value;
end;

procedure TServerItem.SetHost(const Value: string);
begin
	FHost := Value;
end;

procedure TServerItem.SetRoot(const Value: string);
begin
	FRoot := Value;
	Source := Root;
end;

procedure TServerItem.SetTarget(const Value: TServerTarget);
begin
	FTarget := Value;
end;

{ TServersItem }

constructor TServersItem.Create;
begin
	inherited;
end;

function TServersItem.GetDefaultServer: TServerItem;
begin
	if (FDefaultServer = nil) and (Count > 0) then
	begin
		FDefaultServer := TServerItem(Find(FDefaultServerName));
		if FDefaultServer = nil then
			DefaultServer := Servers[0];
	end;
	Result := FDefaultServer
end;

function TServersItem.GetServers(inIndex: Integer): TServerItem;
begin
	Result := TServerItem(Items[inIndex]);
end;

procedure TServersItem.SetDefaultServer(const Value: TServerItem);
begin
	FDefaultServer := Value;
	FDefaultServerName := Value.Name;
end;

procedure TServersItem.SetDefaultServerName(const Value: string);
begin
	FDefaultServer := nil;
	FDefaultServerName := Value;
end;

initialization
	RegisterClass(TServerItem);
	RegisterClass(TServersItem);
end.
