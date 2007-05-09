unit LrTagParser;

interface

uses
	SysUtils, Classes, Contnrs, LrParser;

const
	idText = 0;
	idTag = 1;
	idSpace = 2;
	idDelim = 3;
	idAttr = 4;
	idName = 5;
	idValue = 6;

type
	TLrTagToken = class(TLrParsedToken)
	private
		FNameCount: Integer;
	protected
		function GetElement: string;
		function GetName: string;
		function GetNamedValues(inName: string): string;
		function GetNames(inIndex: Integer): string;
		function GetText: string; override;
		function GetValue: string;
		function GetValues(inIndex: Integer): string;
		procedure SetElement(const Value: string);
		procedure SetName(const Value: string);
		procedure SetNamedValues(inName: string; const inValue: string);
		procedure SetNames(inIndex: Integer; const Value: string);
		procedure SetText(const Value: string); override;
		procedure SetValue(const Value: string);
		procedure SetValues(inIndex: Integer; const Value: string);
	protected
		procedure AddToken(inKind: Integer; const inText: string);
		procedure AfterParse;
		procedure CountKinds;
		function NameIndexToTokenIndex(inIndex: Integer): Integer;
	public
		constructor Create; override;
		function IndexOfName(const inName: string): Integer;
		function IsEndTag: Boolean;
		function IsSingle: Boolean;
		property Element: string read GetElement write SetElement;
		property Name: string read GetName write SetName;
		property NameCount: Integer read FNameCount;
		property NamedValues[inName: string]: string read GetNamedValues
			write SetNamedValues;
		property Names[inIndex: Integer]: string read GetNames write SetNames;
		property Value: string read GetValue write SetValue;
		property Values[inIndex: Integer]: string read GetValues write SetValues;
	end;
	//
	TLrTagParser = class(TLrTokenParser)
	private
		FNextAttrKind: Integer;
	protected
		procedure AttrState;
		procedure SpaceState;
		procedure QuoteState;
	public
		procedure ParseText(const inText: string); override;
	end;
	//
	TLrTaggedDocumentParser = class(TLrTokenParser)
	protected
		function CreateToken(inKind: Integer = idText): TLrToken; override;
		procedure TagState;
		procedure TextState;
	public
		procedure ParseText(const inText: string); override;
	end;
	//
	TLrTaggedDocument = class(TLrParsedToken)
	private
		FTag: TLrTagToken;
		FTagCount: Integer;
	protected
		function GetTags(inIndex: Integer): TLrTagToken;
		procedure SetTags(inIndex: Integer; const Value: TLrTagToken);
		procedure SetTagCount(const Value: Integer);
		procedure SetText(const Value: string); override;
	protected
		function TagIndex(inIndex: Integer): Integer;
		procedure CountTags;
		procedure ParseText(const inText: string);
	public
		function NextTag(var ioIndex: Integer): Boolean; overload;
		function NextTag(var ioIndex: Integer;
			out outToken: TLrTagToken): Boolean; overload;
		procedure Indent(inStrings: TStrings);
		property TagCount: Integer read FTagCount write SetTagCount;
		property Tags[inIndex: Integer]: TLrTagToken read GetTags
			write SetTags;
		property Tag: TLrTagToken read FTag;
	end;

const
	htName = 'name';
	htValue = 'value';

implementation

	function IsAlpha(inChar: char): Boolean;
	begin
		Result := ((inChar >= 'a') and (inChar <= 'z'))
			or ((inChar >= 'A') and (inChar <= 'Z'));
	end;

	function IsSpace(inChar: char): Boolean;
	begin
		Result := (inChar = ' ') or (inChar = #13) or (inChar = #10)
			or (inChar = #9);
	end;

{ TLrTagToken }

constructor TLrTagToken.Create;
begin
	inherited;
	Enabled := true;
end;

procedure TLrTagToken.AfterParse;
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		with Tokens[i] do
			case Kind of
				idSpace: FTokens[i] := nil;
				idName: Text := LowerCase(Text);
				idValue: Text := StringReplace(Text, '"', '', [ rfReplaceAll ]);
			end;
	Element := LowerCase(Element);
	FTokens.Pack;
	CountKinds;
end;

procedure TLrTagToken.CountKinds;
var
	i: Integer;
begin
	FNameCount := 0;
	for i := 0 to Pred(Count) do
		case Tokens[i].Kind of
			idName: Inc(FNameCount);
		end;
end;

function TLrTagToken.GetText: string;
var
	i: Integer;
begin
//	if not Enabled then
//		Result := ''
//	else
	if Count < 3 then
		Result := inherited GetText
	else begin
		Result := Tokens[0].Text + Tokens[1].Text;
		for i := 2 to Pred(Pred(Count)) do
			if Tokens[i].Kind = idValue then
				Result := Result + '="' + Tokens[i].Text + '"'
			else
				Result := Result + ' ' + Tokens[i].Text;
		Result := Result + Tokens[Count - 1].Text;
	end;
end;

function TLrTagToken.GetElement: string;
begin
	if Count < 2 then
		Result := ''
	else
		Result := LowerCase(Tokens[1].Text);
end;

procedure TLrTagToken.SetElement(const Value: string);
begin
	if Count > 1 then
		Tokens[1].Text := Value;
end;

function TLrTagToken.IsSingle: Boolean;
begin
	if Count < 3 then
		Result := false
	else
		Result := Tokens[Count - 2].Text = '/';
end;

function TLrTagToken.IsEndTag: Boolean;
begin
	Result := (Element <> '') and (Element[1] = '/');
end;

procedure TLrTagToken.SetText(const Value: string);
begin
	with TLrTagParser.Create do
	try
		Token := Self;
		ParseText(Value);
	finally
		Free;
	end;
	AfterParse;
end;

procedure TLrTagToken.AddToken(inKind: Integer; const inText: string);
var
	t: TLrToken;
begin
	t := TLrToken.Create;
	t.Kind := inKind;
	t.Text := inText;
	FTokens.Insert(Count - 1, t);
end;

function TLrTagToken.IndexOfName(const inName: string): Integer;
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		with Tokens[i] do
			if (Kind = idName) and (Text = inName) then
			begin
				Result := i;
				exit;
			end;
	Result := -1;
end;

function TLrTagToken.GetNamedValues(inName: string): string;
var
	i: Integer;
begin
	i := IndexOfName(inName);
	if (i < 0) or (i + 1 >= Count) then
		Result := ''
	else
		Result := Tokens[i + 1].Text;
end;

procedure TLrTagToken.SetNamedValues(inName: string; const inValue: string);
var
	i: Integer;
begin
	i := IndexOfName(inName);
	if (i < 0) then
	begin
		AddToken(idName, inName);
		AddToken(idValue, inValue);
	end
	else if (i + 1 < Count) then
		Tokens[i + 1].Text := inValue
	else
		AddToken(idValue, inValue);
end;

function TLrTagToken.NameIndexToTokenIndex(inIndex: Integer): Integer;
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		if (Tokens[i].Kind = idName) then
		begin
			if inIndex = 0 then
			begin
				Result := i;
				exit;
			end;
			Dec(inIndex);
		end;
	Result := -1;
end;

function TLrTagToken.GetNames(inIndex: Integer): string;
var
	i: Integer;
begin
	i := NameIndexToTokenIndex(inIndex);
	if (i < 0) then
		Result := ''
	else
		Result := Tokens[i].Text;
end;

function TLrTagToken.GetValues(inIndex: Integer): string;
var
	i: Integer;
begin
	i := NameIndexToTokenIndex(inIndex);
	if (i < 0) or (i + 1 >= Count)then
		Result := ''
	else
		Result := Tokens[i + 1].Text;
end;

procedure TLrTagToken.SetNames(inIndex: Integer; const Value: string);
var
	i: Integer;
begin
	i := NameIndexToTokenIndex(inIndex);
	if (i >= 0) then
		Tokens[i].Text := Value;
end;

procedure TLrTagToken.SetValues(inIndex: Integer; const Value: string);
var
	i: Integer;
begin
	i := NameIndexToTokenIndex(inIndex);
	if (i >= 0) and (i + 1 < Count)then
		Tokens[i + 1].Text := Value;
end;

function TLrTagToken.GetName: string;
begin
	Result := NamedValues[htName];
end;

function TLrTagToken.GetValue: string;
begin
	Result := NamedValues[htValue];
end;

procedure TLrTagToken.SetName(const Value: string);
begin
	NamedValues[htName] := Value;
end;

procedure TLrTagToken.SetValue(const Value: string);
begin
	NamedValues[htValue] := Value;
end;

{ TLrTagParser }

procedure TLrTagParser.SpaceState;
begin
	if (FChar = #0) then
		DiscardToken
	else if (FChar = '<') or (FChar = '>') then
	begin
		DiscardToken;
		EndTokenInclude(idDelim);
	end
	else if (FChar = '"') then
	begin
		DiscardToken;
		FState := QuoteState;
	end
	else if (FChar = '=') then
	begin
		DiscardToken;
		DiscardChar;
		if LastToken <> nil then
			LastToken.Kind := idName;
		FNextAttrKind := idValue;
	end
	else if not IsSpace(FChar) then
	begin
		DiscardToken;
		FState := AttrState;
	end;
end;

procedure TLrTagParser.AttrState;
begin
	if (FChar = #0) then
	begin
		EndTokenNoInclude(FNextAttrKind);
	end
	else if (FChar = '>') then
	begin
		EndTokenNoInclude(FNextAttrKind);
		EndTokenInclude(idDelim);
		FState := SpaceState;
		FNextAttrKind := idAttr;
	end
	else if (FChar = '"') then
	begin
		FState := QuoteState;
	end
	else if (FChar = '=') then
	begin
		EndTokenNoInclude(idName);
		DiscardChar;
		FNextAttrKind := idValue;
		//FState := AttrState;
	end
	else if IsSpace(FChar) then
	begin
		EndTokenNoInclude(FNextAttrKind);
		if (FNextAttrKind <> idValue) then
			FNextAttrKind := idAttr;
		FState := SpaceState;
	end;
end;

procedure TLrTagParser.QuoteState;
begin
	if (FChar = #0) then
	begin
		EndTokenNoInclude(FNextAttrKind);
		with Token do
			Tokens[Count - 1].Text := Tokens[Count - 1].Text + '"';
	end
	else if (FChar = '"') then
	begin
		EndTokenInclude(FNextAttrKind);
		FNextAttrKind := idAttr;
		FState := SpaceState;
	end;
end;

procedure TLrTagParser.ParseText(const inText: string);
begin
	FNextAttrKind := idAttr;
	FState := SpaceState;
	inherited;
end;

{ TLrTaggedDocumentParser }

function TLrTaggedDocumentParser.CreateToken(inKind: Integer): TLrToken;
begin
	case inKind of
		idTag: Result := TLrTagToken.Create;
		else Result := TLrToken.Create;
	end;
	Result.Kind := inKind;
end;

procedure TLrTaggedDocumentParser.TagState;
begin
	if (FChar = '>') or (FChar = #0) then
	begin
		EndTokenInclude(idTag);
		FState := TextState;
	end;
end;

procedure TLrTaggedDocumentParser.TextState;
begin
	if (FP^ = '<') or (FChar = #0) then
	begin
		EndTokenNoInclude;
		FState := TagState;
	end;
end;

procedure TLrTaggedDocumentParser.ParseText(const inText: string);
begin
	FState := TextState;
	inherited;
end;

{ TLrTaggedDocument }

procedure TLrTaggedDocument.ParseText(const inText: string);
begin
	with TLrTaggedDocumentParser.Create do
	try
		Token := Self;
		ParseText(inText);
	finally
		Free;
	end;
	CountTags;
end;

procedure TLrTaggedDocument.SetText(const Value: string);
begin
	//inherited;
	ParseText(Value);
end;

procedure TLrTaggedDocument.CountTags;
var
	i: Integer;
begin
	FTagCount := 0;
	for i := 0 to Pred(Count) do
		if Tokens[i].Kind = idTag then
			Inc(FTagCount);
end;

function TLrTaggedDocument.TagIndex(inIndex: Integer): Integer;
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		if Tokens[i].Kind = idTag then
		begin
			if inIndex = 0 then
			begin
				Result := i;
				exit;
			end;
			Dec(inIndex);
		end;
	Result := -1;
end;

function TLrTaggedDocument.GetTags(inIndex: Integer): TLrTagToken;
var
	i: Integer;
begin
	i := TagIndex(inIndex);
	if i < 0 then
		Result := nil
	else
		Result := TLrTagToken(Tokens[i]);
end;

procedure TLrTaggedDocument.SetTags(inIndex: Integer;
	const Value: TLrTagToken);
var
	i: Integer;
begin
	i := TagIndex(inIndex);
	if i >= 0 then
		Tokens[i] := Value;
end;

function TLrTaggedDocument.NextTag(var ioIndex: Integer;
	out outToken: TLrTagToken): Boolean;
begin
	outToken := nil;
	while (outToken = nil) and (ioIndex < Count) do
	begin
		if Tokens[ioIndex].Kind = idTag then
			outToken := TLrTagToken(Tokens[ioIndex]);
		Inc(ioIndex);
	end;
	Result := (outToken <> nil);
end;

function TLrTaggedDocument.NextTag(var ioIndex: Integer): Boolean;
begin
	FTag := nil;
	while (FTag = nil) and (ioIndex < Count) do
	begin
		if Tokens[ioIndex].Kind = idTag then
			FTag := TLrTagToken(Tokens[ioIndex]);
		Inc(ioIndex);
	end;
	Result := (FTag <> nil);
end;

procedure TLrTaggedDocument.SetTagCount(const Value: Integer);
begin
	FTagCount := Value;
end;

procedure TLrTaggedDocument.Indent(inStrings: TStrings);
var
	stack: array of string;
	indent, s: string;
	sp, i: Integer;
	tag: TLrTagToken;

	procedure Push(const inElt: string);
	begin
		SetLength(stack, sp + 1);
		stack[sp] := inElt;
		Inc(sp);
		indent := StringOfChar(' ', sp * 2);
	end;

	procedure Pop(const inElt: string);
	begin
		while (sp > 0) do
		begin
			Dec(sp);
			if stack[sp] = inElt then
				break;
		end;
		SetLength(stack, sp);
		indent := StringOfChar(' ', sp * 2);
	end;

	procedure Put;
	begin
		s := Trim(Tokens[i].Text);
		if s <> '' then
			inStrings.Add(indent + s);
	end;

begin
	indent := '';
	sp := 0;
	for i := 0 to Pred(Count) do
		case Tokens[i].Kind of
			idSpace: ;
			idTag:
			begin
				tag := TLrTagToken(Tokens[i]);
				if tag.IsEndTag then
				begin
					Pop(Copy(tag.Element, 2, MAXINT));
					Put;
				end
				else begin
					Put;
					if not tag.IsSingle then
						Push(tag.Element);
				end;
				indent := StringOfChar(' ', sp * 2);
			end;
			else Put;
		end;
end;

end.
