unit Inspector;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids,
	dcgen, dcedit, dcpedit, oinspect,
	LMDDsgComboBox,
	JvTabBar, InspCtrl, CompInsp;

type
	TObjectArray = array of TObject;
	//
	TInspectorForm = class(TForm)
		Panel1: TPanel;
		ObjectCombo: TLMDObjectComboBox;
		InspectorTabs: TJvTabBar;
		JvModernTabBarPainter2: TJvModernTabBarPainter;
		JvModernTabBarPainter1: TJvModernTabBarPainter;
		ComponentInspector1: TComponentInspector;
		procedure FormCreate(Sender: TObject);
		procedure ObjectComboCloseUp(Sender: TObject);
		procedure InspectorTabsTabSelected(Sender: TObject;
			Item: TJvTabBarItem);
		procedure FormDestroy(Sender: TObject);
	private
		{ Private declarations }
		FDefaultComponent: TComponent;
		FOnTabSelected: TNotifyEvent;
		Inspectors: TList;
	protected
		function GetObjectComboSelectedArray: TObjectArray;
		function GetSelected: TPersistent;
		procedure AddToDesignList(inObject: TPersistent);
		procedure AddToSelection(inObject: TPersistent);
		procedure ClearDesignList;
		procedure ClearSelection;
		procedure DesignChange(Sender: TObject);
		//procedure FormatInspector(inInspector: TCustomObjectInspector);
		procedure SelectionChange(Sender: TObject);
		procedure SetDefaultComponent(const Value: TComponent);
	public
		{ Public declarations }
		//procedure AddInspector(const inName: string;
		//	inInspector: TCustomObjectInspector);
		property DefaultComponent: TComponent read FDefaultComponent
			write SetDefaultComponent;
		property OnTabSelected: TNotifyEvent read FOnTabSelected
			write FOnTabSelected;
		property Selected: TPersistent read GetSelected;
	end;

var
	InspectorForm: TInspectorForm;

implementation

uses
	LrUtils, DesignManager;

{$R *.dfm}

procedure TInspectorForm.FormCreate(Sender: TObject);
begin
	Inspectors := TList.Create;
	//Inspectors.Add(ObjectInspector);
	DesignMgr.SelectionObservers.Add(SelectionChange);
	DesignMgr.PropertyObservers.Add(SelectionChange);
	DesignMgr.DesignObservers.Add(DesignChange);
end;

procedure TInspectorForm.FormDestroy(Sender: TObject);
begin
	Inspectors.Free;
end;

{
procedure TInspectorForm.FormatInspector(inInspector: TCustomObjectInspector);
begin
	with TObjectInspector(inInspector) do
	begin
		BorderStyle := bsNone;
		Visible := false;
		FixedColWidth := ObjectInspector.FixedColWidth;
		Style := ObjectInspector.Style;
		Options := ObjectInspector.Options;
		Colors.BackColor := ObjectInspector.Colors.BackColor;
		Colors.Col0Color := ObjectInspector.Colors.Col0Color;
		Colors.Col1Color := ObjectInspector.Colors.Col1Color;
		Colors.HighlightBkgnd := ObjectInspector.Colors.HighlightBkgnd;
		Colors.HighlightColor := ObjectInspector.Colors.HighlightColor;
		Colors.HorzLineColor := ObjectInspector.Colors.HorzLineColor;
		Colors.VertLineColor := ObjectInspector.Colors.VertLineColor;
	end;
end;

procedure TInspectorForm.AddInspector(const inName: string;
	inInspector: TCustomObjectInspector);
begin
	inInspector.Parent := Self;
	FormatInspector(inInspector);
	Inspectors.Add(inInspector);
	InspectorTabs.AddTab(inName);
end;

procedure TInspectorForm.ObjectInspectorChangedPropValue(Sender: TObject;
	const PropEdit: TDCDsgnProperty; var newval: String);
begin
	DesignMgr.PropertyChange(Self);
end;
}

function TInspectorForm.GetObjectComboSelectedArray: TObjectArray;
var
	i: Integer;
begin
	with ObjectCombo.SelectedObjects do
	begin
		SetLength(Result, Count);
		for i := 0 to Pred(Count) do
			Result[i] := Items[i];
	end;
end;

procedure TInspectorForm.ObjectComboCloseUp(Sender: TObject);
begin
	DesignMgr.ObjectsSelected(nil, GetObjectComboSelectedArray);
end;

procedure TInspectorForm.SetDefaultComponent(const Value: TComponent);
begin
	FDefaultComponent := Value;
end;

procedure TInspectorForm.ClearDesignList;
begin
	ClearSelection;
	ObjectCombo.Objects.Clear;
end;

procedure TInspectorForm.AddToDesignList(inObject: TPersistent);
begin
	ObjectCombo.Objects.Add(inObject);
end;

procedure TInspectorForm.DesignChange(Sender: TObject);
var
	i: Integer;
begin
	ClearDesignList;
	for i := 0 to Pred(DesignMgr.DesignCount) do
		AddToDesignList(TPersistent(DesignMgr.DesignObjects[i]));
end;

procedure TInspectorForm.ClearSelection;
//var
//	i: Integer;
begin
	ObjectCombo.SelectedObjects.Clear;
	ComponentInspector1.Instance := nil;
//	for i := 0 to Pred(Inspectors.Count) do
//		TCustomObjectInspector(Inspectors[i]).SetControls(nil);
end;

procedure TInspectorForm.AddToSelection(inObject: TPersistent);
//var
//	i: Integer;
begin
	ObjectCombo.SelectedObjects.Add(inObject);
	ComponentInspector1.AddInstance(TComponent(inObject));
//	for i := 0 to Pred(Inspectors.Count) do
//		TCustomObjectInspector(Inspectors[i]).AddControl(inObject);
end;

procedure TInspectorForm.SelectionChange(Sender: TObject);
var
	i: Integer;
begin
	ClearSelection;
	if (DesignMgr.SelectedCount = 0) and (DefaultComponent <> nil) then
		AddToSelection(DefaultComponent);
	for i := 0 to Pred(DesignMgr.SelectedCount) do
		AddToSelection(TPersistent(DesignMgr.SelectedObjects[i]));
end;

function TInspectorForm.GetSelected: TPersistent;
begin
//	if CompList.SelectedComponents.Count > 0 then
//		Result := CompList.SelectedComponents[0]
//	else
		Result := nil;
end;

procedure TInspectorForm.InspectorTabsTabSelected(Sender: TObject;
	Item: TJvTabBarItem);
//var
//	i: Integer;
begin
//	for i := 0 to Pred(Inspectors.Count) do
//		TCustomObjectInspector(Inspectors[i]).Visible := (i = Item.Index);
	//if Assigned(OnTabSelected) then
	//	OnTabSelected(Self);
end;

end.
