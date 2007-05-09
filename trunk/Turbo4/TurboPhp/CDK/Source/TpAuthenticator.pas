unit TpAuthenticator;

interface

uses
	SysUtils, Classes,
	ThHtmlDocument, ThHeaderComponent, ThTag,
	TpInterfaces, TpControls, TpDb, TpInput;

type
	TTpCustomAuthenticator = class(TThHeaderComponent, ITpIncludeLister)
	private
		FAuto: Boolean;
		FFailureUrl: string;
		FLogoutUrl: string;
		FOnFailure: string;
		FOnLogout: string;
		FOnQueryFailure: string;
		FOnSuccess: string;
		FSuccessUrl: string;
		FLoginInput: TTpInput;
		FPasswordInput: TTpInput;
	protected
		procedure SetLoginInput(const Value: TTpInput);
		procedure SetPasswordInput(const Value: TTpInput);
	protected
		procedure ListPhpIncludes(inIncludes: TStringList); virtual;
		procedure Tag(inTag: TThTag); override;
		procedure UnassignComponent(AComponent: TComponent); override;
	public
		constructor Create(inOwner: TComponent); override;
	published
		property Auto: Boolean read FAuto write FAuto default true;
		property FailureUrl: string read FFailureUrl write FFailureUrl;
		property LoginInput: TTpInput read FLoginInput write SetLoginInput;
		property LogoutUrl: string read FLogoutUrl write FLogoutUrl;
		property PasswordInput: TTpInput read FPasswordInput write SetPasswordInput;
		property OnFailure: string read FOnFailure write FOnFailure;
		property OnLogout: string read FOnLogout write FOnLogout;
		property OnSuccess: string read FOnSuccess write FOnSuccess;
		property OnQueryFailure: string read FOnQueryFailure write FOnQueryFailure;
		property SuccessUrl: string read FSuccessUrl write FSuccessUrl;
	end;
	//
	TTpAuthenticator = class(TTpCustomAuthenticator)
	published
		property Auto;
		property FailureUrl;
		property LoginInput;
		property LogoutUrl;
		property PasswordInput;
		property OnFailure;
		property OnLogout;
		property OnSuccess;
		property OnQueryFailure;
		property SuccessUrl;
	end;
	//
	TTpDbAuthenticator = class(TTpCustomAuthenticator)
	private
		FDb: TTpDb;
		FUsersTable: string;
	protected
		procedure SetDb(const Value: TTpDb);
	protected
		procedure ListPhpIncludes(inIncludes: TStringList); override;
		procedure Tag(inTag: TThTag); override;
		procedure UnassignComponent(AComponent: TComponent); override;
	public
		constructor Create(inOwner: TComponent); override;
	published
		property Auto;
		property Db: TTpDb read FDb write SetDb;
		property FailureUrl;
		property LoginInput;
		property LogoutUrl;
		property PasswordInput;
		property OnFailure;
		property OnLogout;
		property OnSuccess;
		property OnQueryFailure;
		property UsersTable: string read FUsersTable write FUsersTable;
		property SuccessUrl;
	end;

implementation

{ TTpCustomAuthenticator }

constructor TTpCustomAuthenticator.Create(inOwner: TComponent);
begin
	inherited;
	FAuto := true;
end;

procedure TTpCustomAuthenticator.Tag(inTag: TThTag);
begin
	with inTag do
	begin
		Add(tpClass, 'TTpAuthenticator');
		Add('tpName', Name);
		Attributes.Add('tpAuto', Auto);
		if Assigned(LoginInput) then
			Add('tpLoginInput', LoginInput.Name);
		if Assigned(PasswordInput) then
			Add('tpPasswordInput', PasswordInput.Name);
		Add('tpFailureUrl', FailureUrl);
		Add('tpFailureUrl', FailureUrl);
		Add('tpLogoutUrl', LogoutUrl);
		Add('tpSuccessUrl', SuccessUrl);
		Add('tpOnFailure', OnFailure);
		Add('tpOnSuccess', OnSuccess);
		Add('tpOnQueryFailure', OnQueryFailure);
	end;
end;

procedure TTpCustomAuthenticator.ListPhpIncludes(inIncludes: TStringList);
begin
	inIncludes.Add('TpUserLib.php');
end;

procedure TTpCustomAuthenticator.SetLoginInput(const Value: TTpInput);
begin
	ChangeComponentProp(FLoginInput, Value);
end;

procedure TTpCustomAuthenticator.SetPasswordInput(const Value: TTpInput);
begin
	ChangeComponentProp(FPasswordInput, Value);
end;

procedure TTpCustomAuthenticator.UnassignComponent(AComponent: TComponent);
begin
	if AComponent = LoginInput then
		FLoginInput := nil;
	if AComponent = PasswordInput then
		FPasswordInput := nil;
end;

{ TTpDbAuthenticator }

constructor TTpDbAuthenticator.Create(inOwner: TComponent);
begin
	inherited;
end;

procedure TTpDbAuthenticator.ListPhpIncludes(inIncludes: TStringList);
begin
	inherited;
	inIncludes.Add('TpDb.php');
end;

procedure TTpDbAuthenticator.SetDb(const Value: TTpDb);
begin
	ChangeComponentProp(FDb, Value);
end;

procedure TTpDbAuthenticator.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Attributes[tpClass] := 'TTpDbAuthenticator';
		if Assigned(Db) then
			Add('tpDb', Db.Name);
		Add('tpUsersTbl', UsersTable);
	end;
end;

procedure TTpDbAuthenticator.UnassignComponent(AComponent: TComponent);
begin
	inherited;
	if AComponent = Db then
		FDb := nil;
end;

end.
