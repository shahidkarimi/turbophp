unit Inspector;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids,
	dcgen, dcedit, dcpedit, oinspect,
	{JsInspector,}
	LMDDsgComboBox,
	JvTabBar;

type
	TInspectorForm = class(TForm)
		Panel1: TPanel;
    ObjectCombo: TLMDObjectComboBox;
    InspectorTabs: TJvTabBar;
		JvModernTabBarPainter2: TJvModernTabBarPainter;
		ObjectInspector: TObjectInspector;
		procedure FormCreate(Sender: TObject);
		procedure CompListChange(Sender: TObject);
		procedure InspectorTabsChange(Sender: TObject);
		procedure ObjectInspectorShowProperty(Sender: TObject;
			const PropEdit: TDCDsgnProperty; var show: Boolean);
		procedure ObjectInspectorChangedPropValue(Sender: TObject;
			const PropEdit: TDCDsgnProperty; var newval: String);
	private
		{ Private declarations }
		FDefaultComponent: TComponent;
		FOnValueChanged: TNotifyEvent;
		FOnSelectionChanged: TNotifyEvent;
		FRoot: TComponent;
		CompListLocked: Boolean;
	 //	JsEventInspector: TJsInspector;
	protected
		function GetSelected: TPersistent;
		procedure SetDefaultComponent(const Value: TComponent);
		procedure SetRoot(const Value: TComponent);
	protected
		procedure UpdateInspectorComponent(inComponent: TComponent);
		procedure UpdateInspectorList(inComponents: TList);
	public
		{ Public declarations }
		procedure ComboAdd(inObject: TObject);
		procedure ComboClear;
		procedure Inspect(inObject: TObject);
		procedure InspectComponents(inComponents: TList);
		procedure SelectionChange(Sender: TObject);
		property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged
			write FOnSelectionChanged;
		property OnValueChanged: TNotifyEvent read FOnValueChanged
			write FOnValueChanged;
		property Selected: TPersistent read GetSelected;
		//
		procedure InspectPersistent(inObject: TPersistent);
		property DefaultComponent: TComponent read FDefaultComponent
			write SetDefaultComponent;
		property Root: TComponent read FRoot
			write SetRoot;
	end;

var
	InspectorForm: TInspectorForm;

implementation

uses
	LrUtils{, ThPanel}, DesignManager;

{$R *.dfm}

procedure TInspectorForm.FormCreate(Sender: TObject);
begin
	DesignMgr.SelectionObservers.Add(SelectionChange);
{
	JsEventInspector := TJsInspector.Create(Self);
	JsEventInspector.Parent := InspectorTabs;
	JsEventInspector.BorderStyle := bsNone;
	JsEventInspector.Align := alClient;
	JsEventInspector.Visible := false;
	JsEventInspector.FixedColWidth := 100;
}
end;

procedure TInspectorForm.SelectionChange(Sender: TObject);
//var
//	i: Integer;
begin
{
	ClearSelection;
	for i := 0 to Pred(DesignMgr.SelectionCount) do
		AddToSelection(DesignMgr.Selection[i]);
	//TJvInspectorMultiPropData.New(JvInspector1.Root, CreateSelectionList);
	TJvInspectorPropData.New(JvInspector1.Root, CreateSelectionArray);
}
end;

procedure TInspectorForm.SetDefaultComponent(const Value: TComponent);
begin
	FDefaultComponent := Value;
end;

procedure TInspectorForm.SetRoot(const Value: TComponent);
begin
	FRoot := Value;
{
	CompListLocked := true;
	try
		CompList.OwnerComponent := Root;
	finally
		CompListLocked := false;
	end;
}
end;

procedure TInspectorForm.ComboAdd(inObject: TObject);
begin
	ObjectCombo.Objects.Add(TPersistent(inObject));
end;

procedure TInspectorForm.ComboClear;
begin
	ObjectCombo.Objects.Clear;
end;

procedure TInspectorForm.Inspect(inObject: TObject);
begin
	InspectPersistent(TPersistent(inObject));
end;

procedure TInspectorForm.InspectPersistent(inObject: TPersistent);
begin
	ObjectInspector.CurrentControl := inObject;
end;

procedure TInspectorForm.InspectComponents(inComponents: TList);
begin
	if (inComponents <> nil) and (inComponents.Count < 1)
		and (DefaultComponent <> nil) then
			UpdateInspectorComponent(DefaultComponent)
		else
			UpdateInspectorList(inComponents);
end;

procedure TInspectorForm.UpdateInspectorList(inComponents: TList);
begin
	ObjectInspector.CurrentControls := inComponents;
	//JsEventInspector.CurrentControls := inComponents;
{
	CompListLocked := true;
	try
		CompList.SelectedComponents := inComponents;
	finally
		CompListLocked := false;
	end;
}
end;

procedure TInspectorForm.UpdateInspectorComponent(inComponent: TComponent);
var
	list: TList;
begin
	list := TList.Create;
	try
		list.Add(inComponent);
		UpdateInspectorList(list);
	finally
		list.Free;
	end;
end;

procedure TInspectorForm.CompListChange(Sender: TObject);
begin
	if not CompListLocked and not (csDestroying in ComponentState)
	 {and not (csFreeNotification in CompList.ComponentState)} then
		if ObjectInspector.CanFocus then
		begin
			ObjectInspector.SetFocus;
			if Assigned(OnSelectionChanged) then
				OnSelectionChanged(Self);
		end;
end;

function TInspectorForm.GetSelected: TPersistent;
begin
//	if CompList.SelectedComponents.Count > 0 then
//		Result := CompList.SelectedComponents[0]
//	else
		Result := nil;
end;

procedure TInspectorForm.ObjectInspectorShowProperty(Sender: TObject;
	const PropEdit: TDCDsgnProperty; var show: Boolean);
begin
	show := Copy(PropEdit.GetName, 1, 2) = 'On';
	if InspectorTabs.SelectedTab.Index = 0 then
		show := not show;
end;

procedure TInspectorForm.InspectorTabsChange(Sender: TObject);
begin
	ObjectInspector.Visible := (InspectorTabs.SelectedTab.Index <= 1);
	if ObjectInspector.Visible then
		ObjectInspector.Rescan;
	{
	JsEventInspector.Visible := (InspectorTabs.TabIndex = 2);
	if JsEventInspector.Visible then
		JsEventInspector.Rescan;
	}
end;

procedure TInspectorForm.ObjectInspectorChangedPropValue(Sender: TObject;
	const PropEdit: TDCDsgnProperty; var newval: String);
begin
	if Assigned(OnValueChanged) then
		OnValueChanged(Self);
end;

end.
