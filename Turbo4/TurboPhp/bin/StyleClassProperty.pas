unit StyleClassProperty;

interface

uses
	SysUtils, Classes, Controls,
	dcedit, dcfdes, dcsystem, dcdsgnstuff,
	ThStyleSheet, ThAnchorStyles;

type
	TStyleClassProperty = class(TStringProperty)
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues(Proc: TGetStrProc); override;
	end;

procedure RegisterStyleClassProperty;

implementation

procedure RegisterStyleClassProperty;
begin
	RegisterPropertyEditor(TypeInfo(string), nil, 'StyleClass',
		TStyleClassProperty);
	RegisterPropertyEditor(TypeInfo(string), TThAnchorStyles, '',
		TStyleClassProperty);
end;

{ TStyleClassProperty }

function TStyleClassProperty.GetAttributes: TPropertyAttributes;
begin
	Result := [ paMultiSelect, paValueList, paSortList, paRevertable  ];
end;

procedure TStyleClassProperty.GetValues(Proc: TGetStrProc);
var
	s: TThStyleSheet;
	i: Integer;
begin
	{$ifdef VER130}
	s := ThFindStyleSheet(TControl(Designer.GetRoot));
	{$else}
	s := ThFindStyleSheet(TControl(Designer.Root));
	{$endif}
	if (s <> nil) then
		for i := 0 to s.Styles.Count - 1 do
			Proc(s.Styles.StyleItem[i].Name);
end;

end.
