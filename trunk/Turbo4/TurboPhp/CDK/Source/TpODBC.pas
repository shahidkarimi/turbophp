unit TpODBC;

interface

uses
	SysUtils, Classes,
	ThHtmlDocument, ThHeaderComponent, ThTag,
	TpDb, TpControls, TpInterfaces;

type
	TTpODBC = class(TTpDb, ITpConfigWriter)
	private
		FDatabase: string;
		FHost: string;
		FOnGenerate: TTpEvent;
		FPassword: string;
		FPersistent: Boolean;
		FUserName: string;
	protected
		procedure SetHost(const Value: string);
		procedure SetPassword(const Value: string);
		procedure SetUserName(const Value: string);
	protected
		procedure ListPhpIncludes(inIncludes: TStringList); override;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		procedure Tag(inTag: TThTag); override;
		procedure WriteConfig(const inFolder: string);
	published
		property Database: string read FDatabase write FDatabase;
		property DesignConnection;
		property Host: string read FHost write SetHost;
		property UserName: string read FUserName write SetUserName;
		property Password: string read FPassword write SetPassword;
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
		property Persistent: Boolean read FPersistent write FPersistent;
	end;
	//
	TTpODBCTable = class(TTpDataTable)
	protected
		procedure Tag(inTag: TThTag); override;
	end;
	//
	TTpODBCQuery = class(TTpDataQuery)
	protected
		procedure Tag(inTag: TThTag); override;
	end;
	//
	TTpODBCDbList = class(TTpDataQuery)
	protected
		procedure Tag(inTag: TThTag); override;
	public
		constructor Create(inOwner: TComponent); override;
	end;
	//
	TTpODBCTableList = class(TTpDataQuery)
	protected
		procedure Tag(inTag: TThTag); override;
	public
		constructor Create(inOwner: TComponent); override;
	end;

implementation

{uses
	DCPbase64;}

{ TTpODBC }

constructor TTpODBC.Create(inOwner: TComponent);
begin
	inherited;
	FHost := 'localhost';
end;

destructor TTpODBC.Destroy;
begin
	inherited;
end;

procedure TTpODBC.SetHost(const Value: string);
begin
	FHost := Value;
end;

procedure TTpODBC.SetPassword(const Value: string);
begin
	FPassword := Value;
end;

procedure TTpODBC.SetUserName(const Value: string);
begin
	FUserName := Value;
end;

procedure TTpODBC.Tag(inTag: TThTag);
begin
	with inTag do
	begin
		//Add('name', Name);
		Add(tpClass, 'TTpODBC');
		Add('tpName', Name);
		//Add('tpDatabase', Database);
		if Persistent then
			Add('tpPersistent', 'true');
		Add('tpOnGenerate', OnGenerate);
	end;
end;

procedure TTpODBC.WriteConfig(const inFolder: string);
const
	cConfigExt = '.conf.php';
var
	n: string;
	s: TStringList;
begin
	s := nil;
	n := inFolder + Name + cConfigExt;
	//if not FileExists(n) then
	try
		s := TStringList.Create;
		s.Add('; <?php die("Unauthorized access.<br>" ?>');
		s.Add('[ODBC]');
//		s.Add('Host=' + Base64EncodeStr(Host));
//		s.Add('User=' + Base64EncodeStr(UserName));
//		s.Add('Password=' + Base64EncodeStr(Password));
		s.Add('Host=' + Host);
		s.Add('User=' + UserName);
		s.Add('Password=' + Password);
		s.SaveToFile(n);
	finally
		s.Free;
	end;
end;

procedure TTpODBC.ListPhpIncludes(inIncludes: TStringList);
begin
	inherited;
	inIncludes.Add('TpODBC.php');
end;

{ TTpODBCTable }

procedure TTpODBCTable.Tag(inTag: TThTag);
begin
	inherited;
	inTag.Attributes[tpClass] := 'TTpODBCQuery';
end;

{ TTpODBCQuery }

procedure TTpODBCQuery.Tag(inTag: TThTag);
begin
	inherited;
	inTag.Attributes[tpClass] := 'TTpODBCQuery';
end;

{ TTpODBCDbList }

constructor TTpODBCDbList.Create(inOwner: TComponent);
begin
	inherited;
	SQL.Text := 'SHOW DATABASES';
end;

procedure TTpODBCDbList.Tag(inTag: TThTag);
begin
	inherited;
	inTag.Attributes[tpClass] := 'TTpODBCDbList';
end;

{ TTpODBCTableList }

constructor TTpODBCTableList.Create(inOwner: TComponent);
begin
	inherited;
	SQL.Text := 'SHOW TABLES';
end;

procedure TTpODBCTableList.Tag(inTag: TThTag);
begin
	inherited;
	inTag.Attributes[tpClass] := 'TTpODBCTableList';
end;

end.
