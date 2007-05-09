unit ColorProperty;

interface

uses
	TypInfo, SysUtils, Classes, Controls, Graphics, Dialogs,
	dcedit, dcfdes, dcsystem, dcdsgnstuff;

type
	TColorProperty = class(TPropertyEditor)
		function GetAttributes: TPropertyAttributes; override;
		function GetValue: string; override;
		procedure SetValue(const Value: string); override;
		procedure Edit; override;
	end;

procedure RegisterColorPropertyEditor;

implementation

procedure RegisterColorPropertyEditor;
begin
	RegisterEditClass(TypeInfo(TColor), nil, '', TDCSimpleEdit);
	RegisterPropertyEditor(TypeInfo(TColor), nil, '', TColorProperty);
end;

{ TColorProperty }

function TColorProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [ paDialog ];
end;

function TColorProperty.GetValue: string;
var
	c: TColor;
begin
	c := GetOrdValue;
	if not ColorToIdent(c, Result) then
		Result := IntToHex(c, 6);
end;

procedure TColorProperty.SetValue(const Value: string);
var
	c: Integer;
begin
	if not IdentToColor(Value, c) then
		c := StrToIntDef('$' + Value, 0);
	SetOrdValue(c);
end;

procedure TColorProperty.Edit;
begin
	with TColorDialog.Create(nil) do
	try
		Color := GetOrdValue;
		if Execute then
			SetOrdValue(Color);
	finally
		Free;
	end;
end;

end.
