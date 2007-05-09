unit TpMySql;

interface

uses
	SysUtils, Classes,
	ThHtmlDocument, ThHeaderComponent, ThTag,
	TpDb, TpControls, TpInterfaces;

type
	TTpMySql = class(TTpDb, ITpConfigWriter)
	private
		FDatabase: string;
		FHost: string;
		FOnGenerate: TTpEvent;
		FPassword: string;
		FPersistent: Boolean;
		FUserName: string;
	protected
		procedure SetDatabase(const Value: string);
		procedure SetHost(const Value: string);
		procedure SetPassword(const Value: string);
		procedure SetUserName(const Value: string);
	protected
		procedure BuildConnectionString;
		procedure ListPhpIncludes(inIncludes: TStringList); override;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		procedure Tag(inTag: TThTag); override;
		procedure WriteConfig(const inFolder: string);
	published
		property Database: string read FDatabase write SetDatabase;
		property DesignConnection;
		property Host: string read FHost write SetHost;
		property UserName: string read FUserName write SetUserName;
		property Password: string read FPassword write SetPassword;
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
		property Persistent: Boolean read FPersistent write FPersistent;
	end;
	//
	TTpMySqlTable = class(TTpDataTable)
	protected
		procedure Tag(inTag: TThTag); override;
	end;
	//
	TTpMySqlQuery = class(TTpDataQuery)
	protected
		procedure Tag(inTag: TThTag); override;
	end;
	//
	TTpMySqlDbList = class(TTpDataQuery)
	protected
		procedure Tag(inTag: TThTag); override;
	public
		constructor Create(inOwner: TComponent); override;
	end;
	//
	TTpMySqlTableList = class(TTpDataQuery)
	protected
		procedure Tag(inTag: TThTag); override;
	public
		constructor Create(inOwner: TComponent); override;
	end;

implementation

uses
	TpDbConnectionStrings;

{ TTpMySql }

constructor TTpMySql.Create(inOwner: TComponent);
begin
	inherited;
	FHost := 'localhost';
end;

destructor TTpMySql.Destroy;
begin
	inherited;
end;

procedure TTpMySql.SetHost(const Value: string);
begin
	FHost := Value;
	BuildConnectionString;
end;

procedure TTpMySql.SetPassword(const Value: string);
begin
	FPassword := Value;
	BuildConnectionString;
end;

procedure TTpMySql.SetUserName(const Value: string);
begin
	FUserName := Value;
	BuildConnectionString;
end;

procedure TTpMySql.SetDatabase(const Value: string);
begin
	FDatabase := Value;
	BuildConnectionString;
end;

procedure TTpMySql.BuildConnectionString;

	function Param(const inName, inParam: string): string;
	begin
		if (inParam <> '') then
			Result := inName + '=' + inParam + ';'
		else
			Result := '';
	end;

var
	s: string;
begin
	if not (csLoading in ComponentState) then
	begin
		s := BuildMySqlConnectionString(Host, Database, UserName, Password);
		DesignConnection.ConnectionString := s;
		DesignConnection.Connected := true;
	end;
end;

procedure TTpMySql.Tag(inTag: TThTag);
begin
	with inTag do
	begin
		Add(tpClass, 'TTpMySql');
		Add('tpName', Name);
		Add('tpDatabase', Database);
		if Persistent then
			Add('tpPersistent', 'true');
		Add('tpOnGenerate', OnGenerate);
	end;
end;

procedure TTpMySql.WriteConfig(const inFolder: string);
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
		s.Add('[MySql]');
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

procedure TTpMySql.ListPhpIncludes(inIncludes: TStringList);
begin
	inherited;
	inIncludes.Add('TpMySql.php');
end;

{ TTpMySqlTable }

procedure TTpMySqlTable.Tag(inTag: TThTag);
begin
	inherited;
	inTag.Attributes[tpClass] := 'TTpMySqlQuery';
end;

{ TTpMySqlQuery }

procedure TTpMySqlQuery.Tag(inTag: TThTag);
begin
	inherited;
	inTag.Attributes[tpClass] := 'TTpMySqlQuery';
end;

{ TTpMySqlDbList }

constructor TTpMySqlDbList.Create(inOwner: TComponent);
begin
	inherited;
	SQL.Text := 'SHOW DATABASES';
end;

procedure TTpMySqlDbList.Tag(inTag: TThTag);
begin
	inherited;
	inTag.Attributes[tpClass] := 'TTpMySqlDbList';
end;

{ TTpMySqlTableList }

constructor TTpMySqlTableList.Create(inOwner: TComponent);
begin
	inherited;
	SQL.Text := 'SHOW TABLES';
end;

procedure TTpMySqlTableList.Tag(inTag: TThTag);
begin
	inherited;
	inTag.Attributes[tpClass] := 'TTpMySqlTableList';
end;

end.
