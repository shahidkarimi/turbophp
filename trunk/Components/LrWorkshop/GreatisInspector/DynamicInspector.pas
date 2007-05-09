unit DynamicInspector;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	TypInfo, Dialogs, StdCtrls,
	dcgen, dcsystem, dcdsgnstuff, dcdsgnutil, dcedit, oinspect,
	CustomInspector, DynamicProperties;

type
	TDynamicProperty = class(TCustomProperty)
	private
		FComponent: TPersistent;
		FItem: TDynamicPropertyItem;
	protected
		procedure SetItem(const Value: TDynamicPropertyItem);
	public
		function GetAttributes: TPropertyAttributes; override;
		function GetName: string; override;
		function GetValue: string; override;
		procedure Edit; override;
		procedure SetValue(const Value: string); override;
		property Component: TPersistent read FComponent write FComponent;
		property Item: TDynamicPropertyItem read FItem write SetItem;
	end;
	//
	TNewDynamicProperty = class(TCustomProperty)
	private
		FComponent: TPersistent;
		FInspector: TCustomInspector;
		FProperties: TDynamicProperties;
	public
		function GetAttributes: TPropertyAttributes; override;
		function GetName: string; override;
		function GetValue: string; override;
		procedure Edit; override;
		procedure SetValue(const Value: string); override;
		property Component: TPersistent read FComponent write FComponent;
		property Properties: TDynamicProperties read FProperties write FProperties;
		property Inspector: TCustomInspector read FInspector write FInspector;
	end;
	//
	TScriptEventProperty = class(TDynamicProperty)
	public
		class procedure SetCodeDesigner(inDesigner: TCodeDesigner);
	protected
		function GetCodeDesigner: TCodeDesigner;
		procedure RenameMethod(const inOldName, inNewName: string);
		procedure SetMethod(const inName: string);
	public
		procedure Edit; override;
		procedure SetValue(const Value: string); override;
	end;
	//
	TDynamicPropertyInspector = class(TCustomInspector)
	private
		FCollectionName: string;
	protected
		function CreateProperty(inType: TDynamicPropertyType): TDynamicProperty;
		function GetProperties(inComponent: TPersistent): TDynamicProperties;
		procedure AddNewProp(inComponent: TPersistent;
			inProperties: TDynamicProperties; Proc: TGetPropEditProc);
		procedure AddProps(inComponent: TPersistent;
			inProperties: TDynamicProperties; Proc: TGetPropEditProc);
		procedure GetAllPropertyEditors(Components: TComponentList;
			Filter: TTypeKinds; Designer: TFormDesigner;
			Proc: TGetPropEditProc); override;
	public
		constructor Create(inOwner: TComponent); override;
	published
		property BorderStyle;
		property CollectionName: string read FCollectionName write FCollectionName;
	end;

implementation

{ TDynamicProperty }

procedure TDynamicProperty.SetItem(const Value: TDynamicPropertyItem);
begin
	FItem := Value;
end;

function TDynamicProperty.GetName: string;
begin
	Result := Item.Name;
end;

function TDynamicProperty.GetValue: string;
begin
	Result := Item.Value;
end;

procedure TDynamicProperty.SetValue(const Value: string);
begin
	Item.Value := Value;
end;

function TDynamicProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes;
//	if PropertyIsJsEvent(GetName) then
//		Include(Result, paDialog);
end;

procedure TDynamicProperty.Edit;
//var
//	n: string;
begin
//	if CodeDesigner <> nil then
//		with CodeDesigner do
{
		begin
			n := GetValue;
			if n = '' then
				n := Component.GetNamePath + GetName;
			SetValue(n);
		end;
}
end;

{ TNewDynamicProperty }

function TNewDynamicProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes;
	Include(Result, paDialog);
end;

function TNewDynamicProperty.GetName: string;
begin
	Result := '(new property)';
end;

function TNewDynamicProperty.GetValue: string;
begin
	Result := '(use dialog)';
end;

procedure TNewDynamicProperty.SetValue(const Value: string);
begin
	//FNewEventName := Value;
end;

procedure TNewDynamicProperty.Edit;
begin
{
	with TAddJsEventForm.Create(nil) do
	try
		if ShowModal = mrOk then
			if Events.FindEvent(EventEdit.Text) = nil then
			begin
				Events.AddEvent.EventName := EventEdit.Text;
				Inspector.Rescan;
			end;
	finally
		Free;
	end;
}
end;

{ TScriptEventProperty }

var
	CodeDesigner: TCodeDesigner;

class procedure TScriptEventProperty.SetCodeDesigner(inDesigner: TCodeDesigner);
begin
	CodeDesigner := inDesigner;
end;

function TScriptEventProperty.GetCodeDesigner: TCodeDesigner;
begin
	Result := CodeDesigner;
end;

procedure TScriptEventProperty.SetValue(const Value: string);
begin
	if (GetValue <> '') and (Value <> '') then
		RenameMethod(GetValue, Value)
	else if (Value <> '') then
		SetMethod(Value);
	inherited;
end;

procedure TScriptEventProperty.SetMethod(const inName: string);
begin
	if CodeDesigner <> nil then
		with CodeDesigner do
			if MethodExists(inName) then
				ShowMethod(inName)
			else
				CreateMethod(inName, nil);
end;

procedure TScriptEventProperty.RenameMethod(const inOldName, inNewName: string);
begin
	if CodeDesigner <> nil then
		with CodeDesigner do
			if MethodExists(inOldName) then
			begin
				RenameMethod(inOldName, inNewName);
				ShowMethod(inNewName);
			end else
				CreateMethod(inNewName, nil);
end;

procedure TScriptEventProperty.Edit;
var
	n: string;
begin
	n := GetValue;
	if n = '' then
		n := Component.GetNamePath + Copy(GetName, 3, MAXINT);
	SetValue(n);
end;

{ TDynamicPropertyInspector }

constructor TDynamicPropertyInspector.Create(inOwner: TComponent);
begin
	inherited;
	Align := alClient;
end;

function TDynamicPropertyInspector.GetProperties(
	inComponent: TPersistent): TDynamicProperties;
var
	ppi: PPropInfo;
	obj: TObject;
begin
	Result := nil;
	if CollectionName = '' then
		ppi := nil
	else
		ppi := GetPropInfo(inComponent, CollectionName);
	if ppi <> nil then
	begin
		obj := GetObjectProp(inComponent, ppi);
		if (obj <> nil) and (obj is TDynamicProperties) then
			Result := TDynamicProperties(obj)
	end;
end;

function TDynamicPropertyInspector.CreateProperty(
	inType: TDynamicPropertyType): TDynamicProperty;
const
	cPropClasses: array[TDynamicPropertyType] of TClass = ( TDynamicProperty,
		TScriptEventProperty );
begin
	Result := TDynamicProperty(cPropClasses[inType].Create);
end;

procedure TDynamicPropertyInspector.AddProps(inComponent: TPersistent;
	inProperties: TDynamicProperties; Proc: TGetPropEditProc);
var
	i: Integer;
	prop: TDynamicProperty;
begin
	for i := 0 to Pred(inProperties.Count) do
	begin
		prop := CreateProperty(inProperties[i].PropertyType);
		prop.Component := inComponent;
		prop.Item := inProperties[i];
		PropList.Add(prop);
		Proc(prop);
	end;
end;

procedure TDynamicPropertyInspector.AddNewProp(inComponent: TPersistent;
	inProperties: TDynamicProperties; Proc: TGetPropEditProc);
var
	newprop: TNewDynamicProperty;
begin
	newprop := TNewDynamicProperty.Create;
	newprop.Component := inComponent;
	newprop.Properties := inProperties;
	newprop.Inspector := Self;
	PropList.Add(newprop);
	Proc(newprop);
end;

procedure TDynamicPropertyInspector.GetAllPropertyEditors(
	Components: TComponentList; Filter: TTypeKinds; Designer: TFormDesigner;
	Proc: TGetPropEditProc);
var
	c: TPersistent;
	p: TDynamicProperties;
begin
	PropList.Clear;
	if Components.Count > 0 then
	begin
		c := Components[0];
		p := GetProperties(c);
		if (p <> nil) then
		begin
			AddProps(c, p, Proc);
			if p.Extendable then
				AddNewProp(c, p, Proc);
		end;
	end;
end;

end.
