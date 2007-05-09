unit MyTpPropEdit;

interface

uses
	TypInfo, SysUtils, Classes, Controls, Graphics, Dialogs, ExtDlgs,
	dcedit, dcfdes, dcsystem, dcdsgnstuff;

type
	TMyTpPropEdit = class(TStringProperty)
	public
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues(Proc: TGetStrProc); override;
	end;

procedure Register;

implementation

procedure Register;
begin
	RegisterPropertyEditor(TypeInfo(string), nil, 'MyTpProperty',
		TMyTpPropEdit);
end;

{ TMyTpPropEdit }

function TMyTpPropEdit.GetAttributes: TPropertyAttributes;
begin
	Result := [ paMultiSelect, paValueList, paSortList, paRevertable  ];
end;

procedure TMyTpPropEdit.GetValues(Proc: TGetStrProc);
var
	i: Integer;
begin
	for i := 0 to 4 do
		Proc(Format('Value %d', [ i ]));
end;

end.
