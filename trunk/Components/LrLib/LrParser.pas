unit LrParser;

interface

uses
	SysUtils, Classes, Contnrs;

const
	idNoToken = -1;

type
	TLrStateMethod = procedure of object;
	//
	TLrStateParser = class
	protected
		FD, FP, FE, FS: PChar;
		FL: Integer;
		FState: TLrStateMethod;
		FChar: Char;
		function TokenLength: Integer;
	public
		constructor Create; virtual;
		destructor Destroy; override;
		procedure DiscardChar;
		procedure DiscardToken;
		procedure Parse; virtual;
		procedure ParseText(const inText: string); virtual;
	end;
	//
	TLrToken = class
	private
		FText: string;
		FEnabled: Boolean;
	protected
		function GetText: string; virtual;
		procedure SetEnabled(const Value: Boolean);
		procedure SetText(const Value: string); virtual;
	public
		Kind: Integer;
		constructor Create; virtual;
		property Enabled: Boolean read FEnabled write SetEnabled;
		property Text: string read GetText write SetText;
	end;
	//
	TLrParsedToken = class(TLrToken)
	protected
		FTokens: TObjectList;
		function GetText: string; override;
		function GetCount: Integer;
		function GetTokens(inIndex: Integer): TLrToken;
		procedure SetTokens(inIndex: Integer; const Value: TLrToken);
	public
		constructor Create; override;
		destructor Destroy; override;
		procedure Add(inToken: TLrToken);
		procedure Clear;
		function Print: string;
	public
		property Count: Integer read GetCount;
		property Tokens[inIndex: Integer]: TLrToken read GetTokens write SetTokens;
	end;
	//
	TLrTokenParser = class(TLrStateParser)
	private
		FToken: TLrParsedToken;
	protected
		procedure SetToken(const Value: TLrParsedToken);
	protected
		function CreateToken(inKind: Integer = 0): TLrToken; virtual;
		procedure EndTokenInclude(inKind: Integer = 0);
		procedure EndTokenNoInclude(inKind: Integer = 0);
		function LastToken: TLrToken;
		function LastTokenKind: Integer;
		function NewToken(inKind: Integer = 0): TLrToken;
		procedure SetTokenText(inToken: TLrToken; inCount: Integer);
	public
		procedure Parse; override;
		property Token: TLrParsedToken read FToken write SetToken;
	end;

implementation

{ TLrStateParser }

constructor TLrStateParser.Create;
begin
	//
end;

destructor TLrStateParser.Destroy;
begin
	inherited;
end;

function TLrStateParser.TokenLength: Integer;
begin
	Result := FP - FS + 1;
end;

procedure TLrStateParser.DiscardChar;
begin
	FS := FP + 1;
end;

procedure TLrStateParser.DiscardToken;
begin
	FS := FP;
end;

procedure TLrStateParser.Parse;
begin
	if (not Assigned(FState)) or (FD = nil) then
		exit;
	FP := FD;
	FE := FP + FL;
	FS := FP;
	while (FP < FE) do
	begin
		FChar := FP^;
		FState;
		Inc(FP);
	end;
	FChar := #0;
	FState;
end;

procedure TLrStateParser.ParseText(const inText: string);
begin
	FD := @(inText[1]);
	FL := Length(inText);
	Parse;
end;

{ TLrToken }

constructor TLrToken.Create;
begin
	FEnabled := true;
end;

function TLrToken.GetText: string;
begin
	Result := FText;
end;

procedure TLrToken.SetEnabled(const Value: Boolean);
begin
	FEnabled := Value;
end;

procedure TLrToken.SetText(const Value: string);
begin
	FText := Value;
end;

{ TLrParsedToken }

constructor TLrParsedToken.Create;
begin
	FTokens := TObjectList.Create;
end;

destructor TLrParsedToken.Destroy;
begin
	FTokens.Free;
	inherited;
end;

procedure TLrParsedToken.Add(inToken: TLrToken);
begin
	FTokens.Add(inToken);
end;

function TLrParsedToken.GetCount: Integer;
begin
	Result := FTokens.Count;
end;

function TLrParsedToken.GetText: string;
var
	i: Integer;
begin
	Result := '';
	for i := 0 to Pred(Count) do
		with Tokens[i] do
			if Enabled then
				Result := Result + Text;
end;

function TLrParsedToken.Print: string;
var
	i: Integer;
begin
	Result := '';
	for i := 0 to Pred(Count) do
		Result := Result + '[' + Tokens[i].Text + '] ';
end;

function TLrParsedToken.GetTokens(inIndex: Integer): TLrToken;
begin
	Result := TLrToken(FTokens[inIndex]);
end;

procedure TLrParsedToken.SetTokens(inIndex: Integer;
	const Value: TLrToken);
begin
	FTokens[inIndex] := Value;
end;

procedure TLrParsedToken.Clear;
begin
	FTokens.Clear;
end;

{ TLrTokenParser }

function TLrTokenParser.CreateToken(inKind: Integer = 0): TLrToken;
begin
	Result := TLrToken.Create;
	Result.Kind := inKind;
end;

function TLrTokenParser.NewToken(inKind: Integer = 0): TLrToken;
begin
	Result := CreateToken(inKind);
	FToken.Add(Result);
end;

function TLrTokenParser.LastToken: TLrToken;
begin
	if Token.Count > 0 then
		Result := Token.Tokens[Token.Count - 1]
	else
		Result := nil;
end;

function TLrTokenParser.LastTokenKind: Integer;
begin
	if LastToken <> nil then
		Result := LastToken.Kind
	else
		Result := idNoToken;
end;

procedure TLrTokenParser.SetTokenText(inToken: TLrToken; inCount: Integer);
var
	t: string;
begin
	SetLength(t, inCount);
	Move(FS^, t[1], inCount);
	inToken.Text := t;
end;

procedure TLrTokenParser.EndTokenNoInclude(inKind: Integer = 0);
begin
	if TokenLength > 1 then
	begin
		SetTokenText(NewToken(inKind), TokenLength - 1);
		FS := FP;
	end;
end;

procedure TLrTokenParser.EndTokenInclude(inKind: Integer = 0);
begin
	if TokenLength > 0 then
	begin
		SetTokenText(NewToken(inKind), TokenLength);
		FS := FP + 1;
	end;
end;

procedure TLrTokenParser.Parse;
begin
	inherited;
end;

procedure TLrTokenParser.SetToken(const Value: TLrParsedToken);
begin
	FToken := Value;
end;

end.
