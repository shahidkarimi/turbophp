unit TpComponentProperty;

interface

uses
	Classes, Controls, TypInfo,
	dcsystem, dcdsgnstuff, dcdsgnutil,
	ThComponentIterator,
	TpModule;

type
	TTpComponentProperty = class(TDCDsgnComponentProperty)
	private
		Proc: TGetStrProc;
	protected
		procedure EnumModuleComps(inModule: TTpModule);
		procedure FindModules(inContainer: TWinControl);
	public
		procedure GetValues(Proc: TGetStrProc);override;
	end;

procedure RegisterComponentProperty;

implementation

procedure RegisterComponentProperty;
begin
	RegisterPropertyEditor(TypeInfo(TComponent), nil, '',	TTpComponentProperty);
end;

{ TTpComponentProperty }

procedure TTpComponentProperty.EnumModuleComps(inModule: TTpModule);
begin
	CompNamesToProc(inModule.ModuleForm, Designer.Root, GetTypeData(GetPropType),
		Proc);
end;

procedure TTpComponentProperty.FindModules(inContainer: TWinControl);
begin
	with TThComponentIterator.Create(inContainer) do
	try
		while Next do
		begin
			if (Component is TTpModule) then
				EnumModuleComps(TTpModule(Component));
			if (Component is TWinControl) then
				FindModules(TWinControl(Component));
		end;
	finally
		Free;
	end;
end;

procedure TTpComponentProperty.GetValues(Proc: TGetStrProc);
begin
	inherited;
	Self.Proc := Proc;
	FindModules(TWinControl(Designer.Root));
end;

{
procedure TDCDsgnComponentProperty.SetValue(const Value:string);
var
	Component:TComponent;
begin
	if Value='' then
		Component:=nil
	else
		begin
			Component:=Designer.GetComponent(Value);
			if not (Component is GetTypeData(GetPropType)^.ClassType) then
				raise EPropertyError.Create(SInvalidPropertyValue);
		end;
	SetOrdValue(LongInt(Component));
end;
}

end.
 