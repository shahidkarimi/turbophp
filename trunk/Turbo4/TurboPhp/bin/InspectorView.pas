unit InspectorView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids,
	dcgen, dcedit, dcpedit, oinspect,
	JsInspector;

type
	TInspectorForm = class(TForm)
		Bevel1: TBevel;
		CompList: TCompList;
    Bevel2: TBevel;
    InspectorTabs: TTabControl;
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
		FOnPropChanged: TNotifyEvent;
		FOnSelect: TNotifyEvent;
		FOnSelect2: TNotifyEvent;
		FRoot: TComponent;
		CompListLocked: Boolean;
		JsEventInspector: TJsInspector;
	protected
		function GetSelected: TPersistent;
		procedure SetDefaultComponent(const Value: TComponent);
		procedure SetRoot(const Value: TComponent);
	protected
		procedure UpdateInspectorComponent(inComponent: TComponent);
		procedure UpdateInspectorList(inComponents: TList);
	public
		{ Public declarations }
		procedure InspectComponents(inComponents: TList);
		procedure InspectPersistent(inObject: TPersistent);
		property DefaultComponent: TComponent read FDefaultComponent
			write SetDefaultComponent;
		property OnPropChanged: TNotifyEvent read FOnPropChanged
			write FOnPropChanged;
		property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
		property OnSelect2: TNotifyEvent read FOnSelect2 write FOnSelect2;
		property Root: TComponent read FRoot
			write SetRoot;
		property Selected: TPersistent read GetSelected;
	end;

var
	InspectorForm: TInspectorForm;

implementation

uses
	LrUtils, ThPanel;

{$R *.dfm}

procedure TInspectorForm.FormCreate(Sender: TObject);
begin
	//InspectorTabs.DoubleBuffered := true;
	JsEventInspector := TJsInspector.Create(Self);
	JsEventInspector.Parent := InspectorTabs;
	JsEventInspector.BorderStyle := bsNone;
	JsEventInspector.Align := alClient;
	JsEventInspector.Visible := false;
	JsEventInspector.FixedColWidth := 100;
end;

procedure TInspectorForm.SetDefaultComponent(const Value: TComponent);
begin
	FDefaultComponent := Value;
end;

procedure TInspectorForm.SetRoot(const Value: TComponent);
begin
	FRoot := Value;
	CompListLocked := true;
	try
		CompList.OwnerComponent := Root;
	finally
		CompListLocked := false;
	end;
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
	JsEventInspector.CurrentControls := inComponents;
	CompListLocked := true;
	try
		CompList.SelectedComponents := inComponents;
	finally
		CompListLocked := false;
	end;
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

procedure TInspectorForm.InspectPersistent(inObject: TPersistent);
begin
	ObjectInspector.CurrentControl := inObject;
end;

procedure TInspectorForm.CompListChange(Sender: TObject);
begin
	if not CompListLocked and not (csDestroying in ComponentState)
	 {and not (csFreeNotification in CompList.ComponentState)} then
		if ObjectInspector.CanFocus then
		begin
			ObjectInspector.SetFocus;
			if Assigned(OnSelect) then
				OnSelect(Self);
			if Assigned(OnSelect2) then
				OnSelect2(Self);
		end;
end;

function TInspectorForm.GetSelected: TPersistent;
begin
	if CompList.SelectedComponents.Count > 0 then
		Result := CompList.SelectedComponents[0]
	else
		Result := nil;
end;

procedure TInspectorForm.ObjectInspectorShowProperty(Sender: TObject;
	const PropEdit: TDCDsgnProperty; var show: Boolean);
begin
	show := Copy(PropEdit.GetName, 1, 2) = 'On';
	if InspectorTabs.TabIndex = 0 then
		show := not show;
end;

procedure TInspectorForm.InspectorTabsChange(Sender: TObject);
begin
	ObjectInspector.Visible := (InspectorTabs.TabIndex <= 1);
	JsEventInspector.Visible := (InspectorTabs.TabIndex = 2);
	if ObjectInspector.Visible then
		ObjectInspector.Rescan;
	if JsEventInspector.Visible then
		JsEventInspector.Rescan;
end;

procedure TInspectorForm.ObjectInspectorChangedPropValue(Sender: TObject;
  const PropEdit: TDCDsgnProperty; var newval: String);
begin
	if Assigned(OnPropChanged) then
		OnPropChanged(Self);
end;

end.
