unit JsInspector;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	TypInfo, Dialogs, StdCtrls,
	dcgen, dcsystem, dcdsgnstuff, dcdsgnutil, dcedit, oinspect,
	CustomInspector, ThWebControl, ThJavaScript, ThInterfaces;

type
	TJsEventProperty = class(TCustomProperty)
	public
		class procedure SetCodeDesigner(inDesigner: TCodeDesigner);
	private
		FEvent: TThJavaScriptEventItem;
		FComponent: TPersistent;
	protected
		function GetCodeDesigner: TCodeDesigner;
		procedure SetEvent(const Value: TThJavaScriptEventItem);
		procedure SetMethod(const inName: string);
	public
		procedure Edit; override;
		function GetAttributes: TPropertyAttributes; override;
		function GetName: string; override;
		function GetValue: string; override;
		procedure RenameMethod(const inOldName, inNewName: string);
		procedure SetValue(const Value: string); override;
	public
		property Component: TPersistent read FComponent write FComponent;
		property Event: TThJavaScriptEventItem read FEvent write SetEvent;
	end;
	//
	TJsNewEventProperty = class(TCustomProperty)
	private
		FComponent: TPersistent;
		FEvents: TThJavaScriptEvents;
    FInspector: TCustomInspector;
	public
		procedure Edit; override;
		function GetAttributes: TPropertyAttributes; override;
		function GetName: string; override;
		function GetValue: string; override;
		procedure SetValue(const Value: string); override;
	public
		property Component: TPersistent read FComponent write FComponent;
		property Events: TThJavaScriptEvents read FEvents write FEvents;
		property Inspector: TCustomInspector read FInspector write FInspector;
	end;
	//
	TJsInspector = class(TCustomInspector)
	protected
		function GetEvents(inComponent: TPersistent): TThJavaScriptEvents;
		procedure AddNewProp(inComponent: TPersistent;
			inEvents: TThJavaScriptEvents; Proc: TGetPropEditProc);
		procedure AddProps(inComponent: TPersistent;
			inEvents: TThJavaScriptEvents; Proc: TGetPropEditProc);
		procedure GetAllPropertyEditors(Components: TComponentList;
			Filter: TTypeKinds; Designer: TFormDesigner;
			Proc: TGetPropEditProc); override;
	public
		constructor Create(inOwner: TComponent); override;
	published
		property BorderStyle;
	end;

implementation

uses
	ThVclUtils, TpControls, AddJsEventView;

var
	CodeDesigner: TCodeDesigner;

{ TJsEventProperty }

class procedure TJsEventProperty.SetCodeDesigner(inDesigner: TCodeDesigner);
begin
	CodeDesigner := inDesigner;
end;

procedure TJsEventProperty.SetEvent(const Value: TThJavaScriptEventItem);
begin
	FEvent := Value;
end;

function TJsEventProperty.GetName: string;
begin
	Result := Event.EventName;
end;

function TJsEventProperty.GetValue: string;
begin
	Result := Event.FunctionName;
end;

procedure TJsEventProperty.SetValue(const Value: string);
begin
	if (GetValue <> '') and (Value <> '') then
		RenameMethod(GetValue, Value)
	else if (Value <> '') then
		SetMethod(Value);
	Event.FunctionName := Value;
end;

function TJsEventProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes;
	if PropertyIsJsEvent(GetName) then
		Include(Result, paDialog);
end;

function TJsEventProperty.GetCodeDesigner: TCodeDesigner;
begin
	Result := CodeDesigner;
end;

procedure TJsEventProperty.SetMethod(const inName: string);
begin
	with GetCodeDesigner do
		if MethodExists(inName) then
			ShowMethod(inName)
		else
			CreateMethod(inName, nil);
end;

procedure TJsEventProperty.RenameMethod(const inOldName, inNewName: string);
begin
	with GetCodeDesigner do
		if MethodExists(inOldName) then
		begin
			RenameMethod(inOldName, inNewName);
			ShowMethod(inNewName);
		end else
			CreateMethod(inNewName, nil);
end;

procedure TJsEventProperty.Edit;
var
	n: string;
begin
	with GetCodeDesigner do
	begin
		n := GetValue;
		if n = '' then
			n := Component.GetNamePath + Copy(GetName, 3, MAXINT);
		SetValue(n);
	end;
end;

{ TJsNewEventProperty }

function TJsNewEventProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes;
	Include(Result, paDialog);
end;

function TJsNewEventProperty.GetName: string;
begin
	Result := '(new event)';
end;

function TJsNewEventProperty.GetValue: string;
begin
	Result := '(use dialog)';
end;

procedure TJsNewEventProperty.SetValue(const Value: string);
begin
	//FNewEventName := Value;
end;

procedure TJsNewEventProperty.Edit;
begin
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
end;

{ TJsInspector }

constructor TJsInspector.Create(inOwner: TComponent);
begin
	inherited;
	Options := Options - [ oiUseGroups ];
end;

function TJsInspector.GetEvents(inComponent: TPersistent): TThJavaScriptEvents;
var
	js: IThJavaScriptable;
begin
	if ThIsAs(TComponent(inComponent), IThJavaScriptable, js) then
		Result := js.GetJavaScript
	else
		Result := nil;
{
	if (inComponent is TThWebControl) then
		Result := TThWebControl(inComponent).JavaScript
	else if (inComponent is TThWebGraphicControl) then
		Result := TThWebGraphicControl(inComponent).JavaScript
	else
		Result := nil;
}
end;

procedure TJsInspector.AddProps(inComponent: TPersistent;
	inEvents: TThJavaScriptEvents; Proc: TGetPropEditProc);
var
	i: Integer;
	prop: TJSEventProperty;
begin
	for i := 0 to Pred(inEvents.Count) do
	begin
		prop := TJSEventProperty.Create;
		prop.Component := inComponent;
		prop.Event := inEvents.Event[i];
		PropList.Add(prop);
		Proc(prop);
	end;
end;

procedure TJsInspector.AddNewProp(inComponent: TPersistent;
	inEvents: TThJavaScriptEvents; Proc: TGetPropEditProc);
var
	newprop: TJSNewEventProperty;
begin
	newprop := TJsNewEventProperty.Create;
	newprop.Component := inComponent;
	newprop.Events := inEvents;
	newprop.Inspector := Self;
	PropList.Add(newprop);
	Proc(newprop);
end;

procedure TJsInspector.GetAllPropertyEditors(Components: TComponentList;
	Filter: TTypeKinds; Designer: TFormDesigner; Proc: TGetPropEditProc);
var
	c: TPersistent;
	j: TThJavaScriptEvents;
begin
	PropList.Clear;
	if Components.Count > 0 then
	begin
		c := Components[0];
		j := GetEvents(c);
		if (j <> nil) then
		begin
			AddProps(c, j, Proc);
			AddNewProp(c, j, Proc);
		end;
	end;
end;

end.
