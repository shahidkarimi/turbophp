unit PhpCodeDesigner;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
	TypInfo,
	dcsystem, dcparser, dcstring, dccommon, dcmemo,	dccdes, dcdsgnstuff;

type
	TPhpCodeDesigner = class(TJSCodeDesigner)
	protected
		function GetClassEnd: Integer;
		function SkipToToken(inToken: string): Boolean;
	public
		function CreateEventName(const EventName: string; TypeData: PTypeData;
			OnlyType: boolean): string; override;
		function GetMethodStart(const pt: TPoint): TPoint; override;
		function GetSyntaxParserClass : TSimpleParserClass; override;
		function TypeToString(TypeCode: integer): string; override;
		procedure CreateMethod(const AMethodName: string;
			TypeData: PTypeData); override;
		procedure GetMethodTemplate(const AMethodName: string;
			TypeData: PTypeData; S: TStrings); override;
		procedure ValidateObjectEventProperties(inObject: TObject);
		procedure ValidateEventProperties(inContainer: TWinControl);
	end;

implementation

uses
	TpControls;

{ TPhpCodeDesigner }

function TPhpCodeDesigner.GetSyntaxParserClass: TSimpleParserClass;
begin
	Result := inherited GetSyntaxParserClass;
end;

function TPhpCodeDesigner.SkipToToken(inToken: string): Boolean;
begin
	with GetParser do
	begin
		while not ParserEof and (TokenString <> inToken) do
			NextToken;
		Result := Token <> tokEof;
	end;
end;

function TPhpCodeDesigner.GetClassEnd: Integer;
var
	c: Integer;
//	tname: string;
begin
	Result := -1;
{
	if ModuleOwner = nil then
		exit;
	tname := BuildClassName(ModuleOwner.Name);
}
	with GetParser do
		if SkipToToken('class') then
			if SkipToToken('{') then
			begin
				c := 1;
				while not ParserEof and (c > 0) do
				begin
					NextToken;
					if IsTokenChar('{') then
						Inc(c)
					else if IsTokenChar('}') then
						Dec(c);
				end;
				if not ParserEof then
					Result := LinePos
			end;
end;

function TPhpCodeDesigner.CreateEventName(const EventName: string;
	TypeData: PTypeData; OnlyType: boolean): string;
begin
	Result := 'function ' + EventName + '(&$inSender)';
end;

procedure TPhpCodeDesigner.GetMethodTemplate(const AMethodName: string;
	TypeData: PTypeData; S: TStrings);
begin
	with S do
	begin
		Add('  ' + CreateEventName(AMethodName, TypeData, false));
		Add('  ' + '{');
		Add('  ' + '');
		Add('  ' + '}');
//		Add('  ' + '');
	end;
	//inherited;
end;

procedure TPhpCodeDesigner.CreateMethod(const AMethodName: string;
	TypeData: PTypeData);
var
	mtemplate: TStringList;
	i, l: integer;
begin
	with Strings do
	begin
		BeginUpdate;
		try
			//l := Count - 1;
			l := GetClassEnd;
			if (l < 0) then
				l := 0;
			mtemplate := TStringList.Create;
			try
				GetMethodTemplate(AMethodName, TypeData, mtemplate);
				for i := 0 to Pred(mtemplate.Count) do
					Insert(l + i, mtemplate[i]);
				//Insert(Count - 1, '');
			finally
				mtemplate.Free;
			end;
		finally
			EndUpdate;
		end;
		ShowSource(4, l + 2);
	end;
end;

function TPhpCodeDesigner.TypeToString(TypeCode: integer): string;
begin
	Result := '';
end;

function TPhpCodeDesigner.GetMethodStart(const pt: TPoint): TPoint;
begin
	Result := Point(4, pt.y + 2);
end;

procedure TPhpCodeDesigner.ValidateObjectEventProperties(inObject: TObject);
var
	c, i: Integer;
	propList: PPropList;
	m: string;
begin
	c := GetPropList(inObject, propList);
	for i := 0 to Pred(c) do
		if PropertyIsPhpEvent(propList[i].Name) then
		begin
			m := GetStrProp(inObject, propList[i]);
			if (m <> '') and (not MethodExists(m)) then
				SetStrProp(inObject, propList[i], '');
		end;
end;

procedure TPhpCodeDesigner.ValidateEventProperties(inContainer: TWinControl);
var
	i: Integer;
begin
	for i := 0 to Pred(inContainer.ComponentCount) do
	begin
		ValidateObjectEventProperties(inContainer.Components[i]);
		if inContainer.Components[i] is TWinControl then
			ValidateEventProperties(TWinControl(inContainer.Components[i]));
	end;
{
	for i := 0 to Pred(inContainer.ControlCount) do
	begin
		ValidateObjectEventProperties(inContainer.Controls[i]);
		if inContainer.Controls[i] is TWinControl then
			ValidateEventProperties(TWinControl(inContainer.Controls[i]));
	end;
}
end;

end.
