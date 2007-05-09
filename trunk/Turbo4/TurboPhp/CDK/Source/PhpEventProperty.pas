unit PhpEventProperty;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
	TypInfo,
	dcsystem, dcparser, dcstring, dccommon, dcmemo,	dccdes, dcdsgnstuff;

type
	TPhpEventProperty = class(TStringProperty)
	public
		class procedure SetCodeDesigner(inDesigner: TCodeDesigner);
	protected
		function GetCodeDesigner: TCodeDesigner;
		procedure RenameTpMethod(const inOldName, inNewName: string);
		procedure SetTpMethod(const inName: string);
	public
		procedure Edit; override;
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues(Proc: TGetStrProc); override;
		procedure SetValue(const Value: string); override;
	end;

procedure RegisterPhpEventPropertyEditor;

implementation

uses
	TpControls;

var
	CodeDesigner: TCodeDesigner;

procedure RegisterPhpEventPropertyEditor;
begin
	RegisterPropertyEditor(TypeInfo(TTpEvent), nil, '', TPhpEventProperty);
end;

{ TPhpEventProperty }

class procedure TPhpEventProperty.SetCodeDesigner(inDesigner: TCodeDesigner);
begin
	CodeDesigner := inDesigner;
end;

function TPhpEventProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes;
	if PropertyIsTpEvent(GetName) then
		Result := Result + [ paDialog, paValueList ];
//		Include(Result, paDialog);
end;

function TPhpEventProperty.GetCodeDesigner: TCodeDesigner;
begin
	Result := CodeDesigner;
	//Result := DocumentManagerForm.CurrentItem.CodeDesigner['php'];
end;

procedure TPhpEventProperty.SetTpMethod(const inName: string);
begin
	with GetCodeDesigner do
		if MethodExists(inName) then
			ShowMethod(inName)
		else
			CreateMethod(inName, GetTypeData(TypeInfo(TTpEventType)));
end;

procedure TPhpEventProperty.GetValues(Proc: TGetStrProc);
var
	s: TStringList;
	i: Integer;
begin
	s := TStringList.Create;
	try
		GetCodeDesigner.GetMethods(nil, s);
		for i := 0 to Pred(s.Count) do
			Proc(s[i]);
	finally
		s.Free;
	end;
end;

procedure TPhpEventProperty.Edit;
var
	n: string;
begin
	if PropertyIsTpEvent(GetName) then
		with GetCodeDesigner do
		begin
			n := GetStrValue;
			if n = '' then
				n := GetComponent(0).GetNamePath + Copy(GetName, 3, MAXINT);
			SetTpMethod(n);
			SetStrValue(n);
		end;
end;

procedure TPhpEventProperty.RenameTpMethod(const inOldName, inNewName: string);
begin
	with GetCodeDesigner do
		if MethodExists(inOldName) then
			RenameMethod(inOldName, inNewName)
		else
			CreateMethod(inNewName, GetTypeData(TypeInfo(TTpEventType)));
end;

procedure TPhpEventProperty.SetValue(const Value: string);
begin
	if PropertyIsTpEvent(GetName) then
		if (GetStrValue <> '') and (Value <> '') then
			RenameTpMethod(GetStrValue, Value)
		else if (Value <> '') then
			SetTpMethod(Value);
	inherited;
end;

end.
