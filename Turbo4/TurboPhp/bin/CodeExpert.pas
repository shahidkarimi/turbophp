unit CodeExpert;

interface

uses
	Windows, Messages, SysUtils, Classes,
	EasyClasses, EasyEditSource, EasyEditor, EasyParser,
	PhpParser, EasyEditState;

type
	TCodeExpert = class
	private
		{ Private declarations }
		FEasyEdit: TEasyEdit;
		FEasyParser: TEasyEditorParser;
		FEasyEditState: TEasyEditState;
		FCodeInfo: TUnitInfo;
		FNames: TStringList;
	protected
		procedure SetEasyEdit(const Value: TEasyEdit);
	protected
		function GlobalsToDecl(inStrings: TStrings): string;
		procedure ParseCode;
		procedure ProcessFunctionGlobals(Strings: TStrings);
	public
		{ Public declarations }
		constructor Create(inEasyEdit: TEasyEdit = nil);
		destructor Destroy; override;
		procedure UpdateGlobals(inNames: TStringList);
	public
		property EasyEdit: TEasyEdit read FEasyEdit write SetEasyEdit;
		property EasyParser: TEasyEditorParser read FEasyParser
			write FEasyParser;
	end;

implementation

{ TCodeExpert }

constructor TCodeExpert.Create(inEasyEdit: TEasyEdit = nil);
begin
	FCodeInfo := TUnitInfo.Create;
	FEasyEditState := TEasyEditState.Create;
	EasyEdit := inEasyEdit;
end;

destructor TCodeExpert.Destroy;
begin
	FEasyEditState.Free;
	FCodeInfo.Free;
	inherited;
end;

procedure TCodeExpert.SetEasyEdit(const Value: TEasyEdit);
begin
	FEasyEdit := Value;
	if (EasyEdit <> nil) then
		EasyParser := TEasyEditorParser(EasyEdit.Parser);
end;

function TCodeExpert.GlobalsToDecl(inStrings: TStrings): string;
var
	i: Integer;
begin
	if inStrings.Count = 0 then
		Result := ''
	else begin
		Result := '  global ' + inStrings[0];
		for i := 1 to Pred(inStrings.Count) do
			Result := Result + ', ' + inStrings[i];
		Result := Result + ';'
	end;
end;

procedure TCodeExpert.ProcessFunctionGlobals(Strings: TStrings);
var
	i, line, added: Integer;
	info: TElementInfo;

	procedure AddMissingNamedGlobal(const inVar: string);
	begin
		with TFunctionInfo(info).GlobalDecls do
			if (FNames.IndexOfName(inVar) > -1) and (IndexOf(inVar) = -1) then
				Add(inVar);
	end;

	procedure InsertLine(inStrings: TStrings; inLineNo: Integer);
	var
		i: Integer;
		info: TElementInfo;
	begin
		with inStrings do
			for i := 0 to Count - 1 do
			begin
				info := TElementInfo(Objects[i]);
				if (info <> nil) and (info.LineNo >= inLineNo) then
				begin
					info.LineNo := info.LineNo + 1;
					if (info is TFunctionInfo) then
						InsertLine(TFunctionInfo(info).GlobalDecls, inLineNo);
				end;
			end;
	end;

	procedure ProcessFunction;
	var
		i: Integer;
	begin
		with TFunctionInfo(info) do
		begin
			if GlobalDecls.Count > 0 then
				line := TElementInfo(GlobalDecls.Objects[0]).LineNo + added
			else
				line := -1;
			//
			for i := 0 to Pred(UsedVars.Count) do
				AddMissingNamedGlobal(UsedVars[i]);
			//
			if GlobalDecls.Count > 0 then
			begin
				if line = -1 then
				begin
					line := LineNo + added + 2;
					InsertLine(Strings, line);
					EasyEdit.Lines.Insert(line, '');
					//Inc(added);
				end;
				EasyEdit.Lines[line] := GlobalsToDecl(GlobalDecls);
			end;
		end;
	end;

begin
	added := 0;
	with Strings do
		for i := 0 to Count - 1 do
		begin
			info := TElementInfo(Objects[i]);
			if (info <> nil) {and (Info.Scope = AScope)} then
				if (info is TFunctionInfo) then
					ProcessFunction;
		end;
end;

procedure TCodeExpert.ParseCode;
begin
	if EasyEdit <> nil then
	begin
		FCodeInfo.Parser := EasyParser;
		FCodeInfo.ReparseStrings(EasyEdit.Lines);
	end else
		FCodeInfo.ReparseStrings(nil);
end;

procedure TCodeExpert.UpdateGlobals(inNames: TStringList);
begin
	FEasyEditState.GetState(EasyEdit);
	try
		EasyEdit.Lines.BeginUpdate;
		try
				FNames := inNames;
				ParseCode;
				ProcessFunctionGlobals(FCodeInfo.Functions);
		finally
			EasyEdit.Lines.EndUpdate;
		end;
	finally
		FEasyEditState.SetState(EasyEdit);
	end;
end;

end.
