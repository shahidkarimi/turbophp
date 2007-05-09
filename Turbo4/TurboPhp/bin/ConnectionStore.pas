unit ConnectionStore;

interface

uses
	SysUtils, Classes, Forms,
	ADODB, Db,
	dcfdes, dcsystem, dcdsgnstuff,
	ThComponent, ThDataConnection, TpPersistComponent;

type
//	TTpConnectionString = string;
	//
	TConnectionStore = class(TTpPersistComponent)
	private
		FConnectionStrings: TStringList;
		FFilename: string;
	protected
		procedure SetConnectionStrings(const Value: TStringList);
		procedure SetFilename(const Value: string);
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		procedure Add(const inName, inConnectionString: string);
		procedure Save;
		property Filename: string read FFilename write SetFilename;
	published
		property ConnectionStrings: TStringList read FConnectionStrings
			write SetConnectionStrings;
	end;

var
	ConnectionConfigFolder: string;

function NeedConnectionStore: TConnectionStore;

implementation

const
	cConnectionStoreFilename = 'connections.turbophp.cfg';

var
	Connections: TConnectionStore;

function NeedConnectionStore: TConnectionStore;
begin
	if Connections = nil then
	begin
		Connections := TConnectionStore.Create(nil);
		Connections.Filename :=	ConnectionConfigFolder + cConnectionStoreFilename;
	end;
	Result := Connections
end;

{ TConnectionStore }

constructor TConnectionStore.Create(inOwner: TComponent);
begin
	inherited;
	FConnectionStrings := TStringList.Create;
	FConnectionStrings.Duplicates := dupIgnore;
end;

destructor TConnectionStore.Destroy;
begin
	FConnectionStrings.Free;
	inherited;
end;

procedure TConnectionStore.SetConnectionStrings(const Value: TStringList);
begin
	FConnectionStrings.Assign(Value);
end;

procedure TConnectionStore.SetFilename(const Value: string);
begin
	FFilename := Value;
	LoadFromFile(FFilename);
end;

procedure TConnectionStore.Add(const inName, inConnectionString: string);
begin
	if FConnectionStrings.IndexOfName(inName) < 0 then
	begin
		FConnectionStrings.Add(inName + '=' + inConnectionString);
		Save;
	end;
end;

procedure TConnectionStore.Save;
begin
	SaveToFile(FFilename);
end;

end.
