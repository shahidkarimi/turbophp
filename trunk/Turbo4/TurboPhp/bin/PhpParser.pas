unit PhpParser;

interface

uses
	Classes, SysUtils, EasyParser;

type
	TElementScope = ( sPublic, sGlobal, sEnum, sParam,
		sLocalVar, sGlobalDecl, sUsedVar, sField, sUnknown );
	//
	TClassDefinitionType = ( dtClass, dtEnum, dtFunction, dtSimpleType );
	//
	TElementInfo = class
	private
		FLineNo: Integer;
		FName: string;
		FInfoType: string;
		FScope: TElementScope;
	public
		destructor Destroy; override;
		property LineNo: Integer read FLineNo write FLineNo;
		property Name: string read FName write FName;
		property Scope: TElementScope read FScope write FScope;
		property InfoType: string read FInfoType write FInfoType;
	end;
	//
	TConstantInfo = class(TElementInfo)
	private
		FValue: string;
	public
		property Value: string read FValue write FValue;
	end;
	//
	TVariableInfo = class(TElementInfo)
	end;
	//
	TFunctionInfo = class(TElementInfo)
	private
		FReturnType: string;
		FParams: TStrings;
		FLocalVars: TStrings;
		FStartPos: Integer;
		FEndPos: Integer;
		FGlobalDecls: TStrings;
		FUsedVars: TStrings;
		procedure SetParams(Value: TStrings);
		procedure SetLocalVars(Value: TStrings);
		procedure SetGlobalDecls(const Value: TStrings);
    procedure SetUsedVars(const Value: TStrings);
	public
		constructor Create;
		destructor Destroy; override;
		function ParamText: string;
		property StartPos: Integer read FStartPos write FStartPos;
		property EndPos: Integer read FEndPos write FEndPos;
		property ReturnType: string read FReturnType write FReturnType;
		property Params: TStrings read FParams write SetParams;
		property LocalVars: TStrings read FLocalVars write SetLocalVars;
		property GlobalDecls: TStrings read FGlobalDecls write SetGlobalDecls;
		property UsedVars: TStrings read FUsedVars write SetUsedVars;
	end;
	//
	TClassInfo = class(TElementInfo)
	private
		FFields: TStrings;
		FMethods: TStrings;
		FStartPos: Integer;
		FEndPos: Integer;
	protected
		procedure SetFields(Value: TStrings);
		procedure SetMethods(Value: TStrings);
	public
		constructor Create;
		destructor Destroy; override;
	public
		property StartPos: Integer read FStartPos write FStartPos;
		property EndPos: Integer read FEndPos write FEndPos;
		property Fields: TStrings read FFields write SetFields;
		property Methods: TStrings read FMethods write SetMethods;
	end;
	//
	TUnitInfo = class
	private
		FVariables: TStrings;
		FFunctions: TStrings;
		FClasses: TStrings;
		FConstants: TStrings;
		FParser: TEasyEditorParser;
	protected
		function AddInfo(inInfo: TElementInfo; const AName: string;
			inScope: TElementScope; inList: TStrings): TElementInfo;
		function GetLinePos: Integer;
		function NextValidToken: Integer;
		function NextValidTokenStr: string;
		procedure ParseLocalVars(Info: TElementInfo);
		procedure ParseGlobalDecls(Info: TElementInfo);
		procedure ProcessFunction(const s: string; SkipToEnd: boolean;
			inList: TStrings);
		procedure ProcessVariable(const s: string);
		procedure ProcessClass(const s: string; SkipToEnd: boolean);
	public
		constructor Create;
		destructor Destroy; override;
		procedure ParseStrings(AStrings: TStrings);
		procedure ReparseStrings(Strings: TStrings);
		function AddClass(const AName: string): TClassInfo;
		function AddConst(const AName: string; AOwner: TElementInfo): TConstantInfo;
		function AddFunction(const AName: string; inList: TStrings): TFunctionInfo;
		function AddGlobalDecl(const AName: string; AOwner: TElementInfo): TVariableInfo;
		function AddParam(const AName: string; inList: TStrings): TVariableInfo;
		function AddUsedVar(const AName: string;
			AOwner: TElementInfo): TVariableInfo;
		function AddVariable(const AName: string; AOwner: TElementInfo): TVariableInfo;
		function IndexOf(const s: string): TElementInfo;
		property Variables: TStrings read FVariables;
		property Functions: TStrings read FFunctions;
		property Classes: TStrings read FClasses;
		property Constants: TStrings read FConstants;
		property Parser: TEasyEditorParser read FParser write FParser;
	end;

implementation

uses
	StrUtils, EasyUtils;

type
	TMParser = class(TEasyEditorParser);

const
	tnone = 0;
	tstring = 1;
	tcomment = 2;
	tident = 3;
	tInteger = 4;
	tfloat = 5;
	tresword = 6;
	twhitespace = 9;

	sClassStr = 'class';
	sFuncStr = 'function';
	sVarStr = 'var';
	sGlobalDeclStr = 'global';
	sGroupBegin = '{';
	sGroupEnd = '}';
	sLeftBracket = '(';
	sRightBracket = ')';

procedure ClearStrings(Strings: TStrings);
var
	i: Integer;
begin
	with Strings do
	begin
		for i := Count - 1 downto 0 do
			Objects[i].Free;
		Clear;
	end;
end;

procedure FreeStrings(var Strings: TStrings);
begin
	ClearStrings(Strings);
	Strings.Free;
	Strings := nil;
end;

{ TElementInfo }

destructor TElementInfo.Destroy;
begin
	inherited Destroy;
end;

{ TFunctionInfo }

constructor TFunctionInfo.Create;
begin
	inherited Create;
	FParams := TStringList.Create;
	FLocalVars := TStringList.Create;
	FGlobalDecls := TStringList.Create;
	FUsedVars := TStringList.Create;
end;

destructor TFunctionInfo.Destroy;
begin
	FreeStrings(FParams);
	FreeStrings(FLocalVars);
	FreeStrings(FGlobalDecls);
	FreeStrings(FUsedVars);
	inherited Destroy;
end;

procedure TFunctionInfo.SetParams(Value: TStrings);
begin
	FParams.Assign(Value);
end;

procedure TFunctionInfo.SetLocalVars(Value: TStrings);
begin
	FLocalVars.Assign(Value);
end;

procedure TFunctionInfo.SetGlobalDecls(const Value: TStrings);
begin
	FGlobalDecls.Assign(Value);
end;

procedure TFunctionInfo.SetUsedVars(const Value: TStrings);
begin
	FUsedVars.Assign(Value);
end;

function  TFunctionInfo.ParamText: string;
var
	i: Integer;
begin
	Result := '';
	for i := 0 to Params.Count - 1  do
		with TVariableInfo(Params.Objects[i]) do
		begin
			if Result <> '' then
				Result := Result + ',';
//      Result := Result + FInfoType + ' ' + FName;
			if FInfoType <> '' then
				Result := Result + FInfoType + ' ' + FName
			else
				Result := Result + FName;
		end;
	 Result := '(' + Result + ')';
end;

{ TClassInfo }

constructor TClassInfo.Create;
begin
	inherited Create;
	FFields := TStringList.Create;
	FMethods := TStringList.Create;
end;

destructor TClassInfo.Destroy;
begin
	FreeStrings(FMethods);
	FreeStrings(FFields);
	inherited;
end;

procedure TClassInfo.SetFields(Value: TStrings);
begin
	FFields.Assign(Value);
end;

procedure TClassInfo.SetMethods(Value: TStrings);
begin
	FMethods.Assign(Value);
end;

{ TUnitInfo }

constructor TUnitInfo.Create;
begin
	inherited Create;
	FVariables := CreateSortedStrings;
	FFunctions := CreateSortedStrings;
	FClasses := CreateSortedStrings;
	FConstants := CreateSortedStrings;
end;

destructor TUnitInfo.Destroy;
begin
	FreeStrings(FVariables);
	FreeStrings(FFunctions);
	FreeStrings(FClasses);
	FreeStrings(FConstants);
	inherited Destroy;
end;

function TUnitInfo.IndexOf(const s: string): TElementInfo;
var
	AInfo: TElementInfo;

	function _Check(Strings: TStrings): boolean;
	var
		Index: Integer;
	begin
		Index := Strings.IndexOf(s);
		Result := Index >= 0;
		if Result then
			AInfo := TElementInfo(Strings.Objects[Index]);
	end;

begin
	if _Check(FVariables) or _Check(FFunctions) or _Check(Constants) then
		Result := AInfo
	else
		Result := nil;
end;

function TUnitInfo.AddInfo(inInfo: TElementInfo; const AName: string;
	inScope: TElementScope; inList: TStrings): TElementInfo;
begin
	with inInfo do
	begin
		Name := AName;
		Scope := inScope;
		LineNo := GetLinePos;
	end;
	inList.AddObject(AName, inInfo);
	Result := inInfo;
end;

function TUnitInfo.AddClass(const AName: string): TClassInfo;
begin
	if AName = '' then
		Result := nil
	else begin
		Result := TClassInfo.Create;
		AddInfo(Result, AName, sPublic, Classes);
	end;
end;

function TUnitInfo.AddFunction(const AName: string;
	inList: TStrings): TFunctionInfo;
begin
	if AName = '' then
		Result := nil
	else begin
		Result := TFunctionInfo.Create;
		AddInfo(Result, AName, sPublic, inList);
	end;
end;

function TUnitInfo.AddParam(const AName: string;
	inList: TStrings): TVariableInfo;
begin
	if (AName = '') then
		Result := nil
	else begin
		Result := TVariableInfo.Create;
		AddInfo(Result, AName, sPublic, inList);
	end;
end;

function TUnitInfo.AddVariable(const AName: string;
	AOwner: TElementInfo): TVariableInfo;
begin
	if (AName = '')  then
		Result := nil
	else begin
		Result := TVariableInfo.Create;
		if AOwner = nil then
			AddInfo(Result, AName, sPublic, Variables)
		else if AOwner is TFunctionInfo then
			AddInfo(Result, AName, sLocalVar, TFunctionInfo(AOwner).LocalVars)
		else if AOwner is TClassInfo then
			AddInfo(Result, AName, sLocalVar, TClassInfo(AOwner).Fields);
	end;
end;

function TUnitInfo.AddGlobalDecl(const AName: string;
	AOwner: TElementInfo): TVariableInfo;
begin
	if (AName = '')  then
		Result := nil
	else begin
		Result := TVariableInfo.Create;
		if AOwner is TFunctionInfo then
			AddInfo(Result, AName, sGlobalDecl, TFunctionInfo(AOwner).GlobalDecls)
	end;
end;

function TUnitInfo.AddUsedVar(const AName: string;
	AOwner: TElementInfo): TVariableInfo;
var
	n: string;
begin
	Result := nil;
	if AOwner is TFunctionInfo then
		if (AName <> '')  then
		begin
			if AnsiEndsStr('->', AName) then
				n := Copy(AName, 1, Length(AName) - 2)
			else
				n := AName;
			if TFunctionInfo(AOwner).UsedVars.IndexOf(n) = -1 then
			begin
				Result := TVariableInfo.Create;
					AddInfo(Result, n, sUsedVar, TFunctionInfo(AOwner).UsedVars)
			end;
		end;
end;

function TUnitInfo.AddConst(const AName: string;
	AOwner: TElementInfo): TConstantInfo;
begin
	if (AName = '')  then
		Result := nil
	else begin
		Result := TConstantInfo.Create;
		if AOwner = nil then
			AddInfo(Result, AName, sPublic, Constants)
		else if AOwner is TFunctionInfo then
			AddInfo(Result, AName, sLocalVar, TFunctionInfo(AOwner).LocalVars);
		//else if AOwner is TClassInfo then
		//	AddInfo(Result, AName, sLocalVar, TClassInfo(AOwner).Fields)
	end;
end;

procedure  TUnitInfo.ParseStrings(AStrings: TStrings);
var
	s: string;
begin
	if FParser = nil then
		Exit;
	with FParser do
	begin
		Strings := AStrings;
		Reset;
		while not EndOfSource do
		begin
			case NextValidToken of
				tresword:
				begin
					s := TokenString;
					if s = sFuncStr then
						ProcessFunction(NextValidTokenStr, true, Functions)
					else if s = sClassStr then
						ProcessClass(NextValidTokenStr, true)
					else if (CompareText(s, sVarStr) = 0) then
						ProcessVariable(NextValidTokenStr);
				end;
			end;
		end;
		Strings := nil;
	end;
end;

procedure TUnitInfo.ReparseStrings(Strings: TStrings);
begin
	ClearStrings(FVariables);
	ClearStrings(FFunctions);
	ClearStrings(FClasses);
	ClearStrings(FConstants);
	if Strings <> nil then
		ParseStrings(Strings);
end;

function TUnitInfo.NextValidToken: Integer;
begin
	repeat
		Result := FParser.NextToken;
	until (Result <> tComment) and (Result <> tNone) and (Result <> twhitespace);
end;

function TUnitInfo.NextValidTokenStr: string;
begin
	NextValidToken;
	Result := FParser.TokenString;
end;

procedure TUnitInfo.ParseLocalVars(Info: TElementInfo);
var
	varAdded: boolean;
	i: Integer;
	ts: string;
begin
	varAdded := false;
	with FParser do
		while not EndOfSource do
		begin
			i := NextToken;
			ts := TokenString;
			if ts = ',' then
				varAdded := false;
			if ts = ';' then
				exit;
			if (i <> tComment) and (i <> tNone) and (i <> twhitespace) then
			begin
				if varAdded then
					exit;
				AddVariable(ts, info);
				varAdded := true;
			end;
		end;
end;

procedure TUnitInfo.ParseGlobalDecls(Info: TElementInfo);
var
	added: boolean;
	i: Integer;
	ts: string;
begin
	added := false;
	with FParser do
		while not EndOfSource do
		begin
			i := NextToken;
			ts := TokenString;
			if ts = ',' then
				added := false;
			if ts = ';' then
				exit;
			if (i <> tComment) and (i <> tNone) and (i <> twhitespace) then
			begin
				if added then
					exit;
				AddGlobalDecl(ts, Info);
				added := true;
			end;
		end;
end;

procedure TUnitInfo.ProcessFunction(const s: string; SkipToEnd: boolean;
	inList: TStrings);
var
	i    : Integer;
	Info : TFunctionInfo;
	Count: Integer;
	Temp : string;

	procedure ParseParams(Info: TFunctionInfo);
	begin
		FParser.NextToken;
		Temp := FParser.TokenString;
		if Temp <> sLeftBracket then
			exit;
		with FParser do
			while not EndOfSource do
			begin
				i := NextToken;
				Temp := TokenString;
				if Temp = sRightBracket then
					exit;
				if (i <> tComment) and (i <> tNone) and (i <> twhitespace) then
					AddParam(Temp, Info.Params);
			end;
	end;

begin
	Info := AddFunction(s, inList);
	if Info = nil then
		Exit;
	Info.LineNo := GetLinePos;
	ParseParams(Info);
	if SkipToEnd then
	begin
		Info.StartPos := GetLinePos;
//    ParseLocalVars(Info);
		Count := 0;
		with FParser do
			while not EndOfSource do
			begin
				i := NextToken;
				// SJM 2004
				if i = tstring then
					continue;
				Temp := TokenString;
				//if (Count = 1) and (Temp = sVarStr) then
				//	ParseLocalVars(Info);
				if (Count = 1) and (Temp = sGlobalDeclStr) then
					ParseGlobalDecls(Info)
				else if (Count = 1) and (Temp <> '') and (Temp[1] = '$') then
					AddUsedVar(temp, info)
				else if Temp = sGroupBegin then
					Inc(Count)
				else if Temp = sGroupEnd then
				begin
					Dec(Count);
					if Count = 0 then
						Break;
				end;
			end;
		Info.EndPos := GetLinePos;
	end;
end;

procedure TUnitInfo.ProcessVariable(const s: string);
var
	Info: TVariableInfo;
begin
	Info := AddVariable(s, nil);
	if Info = nil then
		Exit;
	Info.LineNo := GetLinePos;
end;

function TUnitInfo.GetLinePos: Integer;
begin
	Result := TMParser(FParser).LineIndex;
end;

procedure TUnitInfo.ProcessClass(const s: string; SkipToEnd: boolean);
var
	i: Integer;
	info: TClassInfo;
	count: Integer;
	ts: string;
begin
	info := AddClass(s);
	if info = nil then
		exit;
	info.LineNo := GetLinePos;
	if SkipToEnd then
	begin
		info.StartPos := GetLinePos;
		count := 0;
		with FParser do
			while not EndOfSource do
			begin
				i := NextToken;
				// SJM 2004
				if i = tstring then
					continue;
				ts := TokenString;
				if ts = sFuncStr then
					ProcessFunction(NextValidTokenStr, true, info.FMethods);
				if (Count = 1) and (ts = sVarStr) then
					ParseLocalVars(Info);
				if ts = sGroupBegin then
					Inc(count)
				else if ts = sGroupEnd then
				begin
					Dec(count);
					if count = 0 then
						break;
				end;
			end;
		Info.EndPos := GetLinePos;
	end;
end;

end.
