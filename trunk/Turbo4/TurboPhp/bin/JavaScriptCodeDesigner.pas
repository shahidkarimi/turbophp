unit JavaScriptCodeDesigner;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
	TypInfo,
	dcsystem, dcparser, dcstring, dccommon, dcmemo,	dccdes, dcdsgnstuff;

type
	TJavaScriptCodeDesigner = class(TJSCodeDesigner)
	public
		function CreateEventName(const EventName: string; TypeData: PTypeData;
			OnlyType: boolean): string; override;
		procedure CreateMethod(const AMethodName: string;
			TypeData: PTypeData); override;
		function GetMethodStart(const pt: TPoint): TPoint; override;
		procedure GetMethodTemplate(const AMethodName: string;
			TypeData: PTypeData; S: TStrings); override;
		function GetSyntaxParserClass : TSimpleParserClass; override;
		function TypeToString(TypeCode: integer): string; override;
		procedure ValidateObjectEventProperties(inObject: TObject);
		procedure ValidateEventProperties(inContainer: TWinControl);
	end;

implementation

uses
	TpControls;

{ TJavaScriptCodeDesigner }

function TJavaScriptCodeDesigner.GetSyntaxParserClass: TSimpleParserClass;
begin
	Result := inherited GetSyntaxParserClass;
end;

function TJavaScriptCodeDesigner.CreateEventName(const EventName: string;
	TypeData: PTypeData; OnlyType: boolean): string;
begin
	Result := 'function ' + EventName + '(inSender, inIndex)';
end;

procedure TJavaScriptCodeDesigner.GetMethodTemplate(const AMethodName: string;
	TypeData: PTypeData; S: TStrings);
begin
	with S do
	begin
		Add(CreateEventName(AMethodName, TypeData, false));
		Add('{');
		Add('');
		Add('}');
	end;
end;

procedure TJavaScriptCodeDesigner.CreateMethod(const AMethodName: string;
	TypeData: PTypeData);
begin
	inherited;
	ShowMethod(AMethodName);
end;

function TJavaScriptCodeDesigner.TypeToString(TypeCode: integer): string;
begin
	Result := '';
end;

function TJavaScriptCodeDesigner.GetMethodStart(const pt: TPoint): TPoint;
begin
	Result := Point(2, pt.y + 2);
end;

procedure TJavaScriptCodeDesigner.ValidateObjectEventProperties(
	inObject: TObject);
var
	c, i: Integer;
	propList: PPropList;
	m: string;
begin
	c := GetPropList(inObject, propList);
	for i := 0 to Pred(c) do
		if PropertyIsJsEvent(propList[i].Name) then
		begin
			m := GetStrProp(inObject, propList[i]);
			if (m <> '') and (not MethodExists(m)) then
				SetStrProp(inObject, propList[i], '');
		end;
end;

procedure TJavaScriptCodeDesigner.ValidateEventProperties(
	inContainer: TWinControl);
var
	i: Integer;
begin
	for i := 0 to Pred(inContainer.ControlCount) do
	begin
		ValidateObjectEventProperties(inContainer.Controls[i]);
		if inContainer.Controls[i] is TWinControl then
			ValidateEventProperties(TWinControl(inContainer.Controls[i]));
	end;
end;

end.
