unit ListSourceProperty;

interface

uses
	SysUtils, Classes, Controls,
	dcedit, dcfdes, dcsystem, dcdsgnstuff;

type
	TListSourceProperty = class(TComponentProperty)
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues(Proc: TGetStrProc); override;
	end;

procedure RegisterListSourceProperty;

implementation

uses
	ThVclUtils, ThComponentIterator, ThListSource;

procedure RegisterListSourceProperty;
begin
	RegisterPropertyEditor(TypeInfo(TComponent), nil, 'ListSource',
		TListSourceProperty);
end;

{ TListSourceProperty }

function TListSourceProperty.GetAttributes: TPropertyAttributes;
begin
	Result := [ paMultiSelect, paValueList, paSortList, paRevertable  ];
end;

procedure TListSourceProperty.GetValues(Proc: TGetStrProc);
var
	s: IThListSource;
begin
	with TThComponentIterator.Create(TWinControl(Designer.Root)) do
	try
	 while Next do
		if ThIsAs(Component, IThListSource, s) then
			Proc(Component.Name);
	finally
		Free;
	end;
end;

end.
