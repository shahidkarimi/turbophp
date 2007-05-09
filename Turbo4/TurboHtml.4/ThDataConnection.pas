unit ThDataConnection;

interface

uses
	SysUtils, Classes, Forms,
	ADODB, Db,
	ThComponent;

type
	TThConnectionString = string;
	//
	TThConnectionStore = class(TThComponent)
	private
		FConnectionStrings: TStringList;
		FFilename: string;
	protected
		procedure SetConnectionStrings(const Value: TStringList);
		procedure SetFilename(const Value: string);
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		procedure Add(const inConnectionString: string);
		procedure Save;
		property Filename: string read FFilename write SetFilename;
	published
		property ConnectionStrings: TStringList read FConnectionStrings
			write SetConnectionStrings;
	end;
	//
	TThDataConnection = class(TPersistent)
	private
		FADOConnection: TADOConnection;
		FADOTable: TADOTable;
		FConnectionString: TThConnectionString;
		FDataSource: TDataSource;
	protected
		function GetConnected: Boolean;
		function GetLoginPrompt: Boolean;
		procedure SetConnected(const Value: Boolean);
		procedure SetConnectionString(const Value: TThConnectionString);
		procedure SetLoginPrompt(const Value: Boolean);
	public
		constructor Create(inOwner: TComponent); //override;
		destructor Destroy; override;
		property Connection: TADOConnection read FADOConnection;
	published
		property Connected: Boolean read GetConnected write SetConnected;
		property ConnectionString: TThConnectionString read FConnectionString
			write SetConnectionString;
		property LoginPrompt: Boolean read GetLoginPrompt write SetLoginPrompt;
	end;
	//
	TThDbConnect = class(TThComponent)
	end;

function NeedConnectionStore: TThConnectionStore;

implementation

const
	cTpConnectionStoreFilename = 'connections.turbophp.cfg';

var
	ConnectionStore: TThConnectionStore;

function NeedConnectionStore: TThConnectionStore;
begin
	if ConnectionStore = nil then
	begin
		ConnectionStore := TThConnectionStore.Create(nil);
		ConnectionStore.Filename :=
			ExtractFilePath(Application.ExeName) + cTpConnectionStoreFilename;
	end;
	Result := ConnectionStore
end;

{ TThConnectionStore }

constructor TThConnectionStore.Create(inOwner: TComponent);
begin
	inherited;
	FConnectionStrings := TStringList.Create;
	FConnectionStrings.Duplicates := dupIgnore;
end;

destructor TThConnectionStore.Destroy;
begin
	FConnectionStrings.Free;
	inherited;
end;

procedure TThConnectionStore.SetConnectionStrings(const Value: TStringList);
begin
	FConnectionStrings.Assign(Value);
end;

procedure TThConnectionStore.SetFilename(const Value: string);
begin
	FFilename := Value;
	LoadFromFile(FFilename);
end;

procedure TThConnectionStore.Add(const inConnectionString: string);
begin
	if FConnectionStrings.IndexOf(inConnectionString) < 0 then
	begin
		FConnectionStrings.Add(inConnectionString);
		Save;
	end;
end;

procedure TThConnectionStore.Save;
begin
	SaveToFile(FFilename);
end;

{ TThDataConnection }

constructor TThDataConnection.Create(inOwner: TComponent);
begin
	inherited Create;
	FAdoConnection := TAdoConnection.Create(inOwner);
	FAdoTable := TAdoTable.Create(inOwner);
	FAdoTable.Connection := FAdoConnection;
	FDataSource := TDataSource.Create(inOwner);
	FDataSource.DataSet := FAdoTable;
{
	inherited;
	FAdoConnection := TAdoConnection.Create(Self);
	FAdoTable := TAdoTable.Create(Self);
	FAdoTable.Connection := FAdoConnection;
	FDataSource := TDataSource.Create(Self);
	FDataSource.DataSet := FAdoTable;
}
	LoginPrompt := false;
end;

destructor TThDataConnection.Destroy;
begin
	FDataSource.Free;
	FAdoTable.Free;
	FAdoConnection.Free;
	inherited;
end;

function TThDataConnection.GetConnected: Boolean;
begin
	Result := FAdoConnection.Connected;
end;

function TThDataConnection.GetLoginPrompt: Boolean;
begin
	Result := FAdoConnection.LoginPrompt;
end;

procedure TThDataConnection.SetConnected(const Value: Boolean);
begin
	if (Connected <> Value) and (ConnectionString <> '') then
	begin
		FAdoConnection.ConnectionString := ConnectionString;
		FAdoConnection.Connected := Value;
		if Value then
			NeedConnectionStore.Add(FAdoConnection.ConnectionString);
	end;
end;

procedure TThDataConnection.SetConnectionString(
	const Value: TThConnectionString);
begin
	FConnectionString := Value;
end;

procedure TThDataConnection.SetLoginPrompt(const Value: Boolean);
begin
	FAdoConnection.LoginPrompt := Value;
end;

end.
