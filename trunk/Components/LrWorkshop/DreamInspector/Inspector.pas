unit Inspector;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids,
	dcgen, dcedit, dcpedit, dcdsgnstuff, oinspect,
	LMDDsgComboBox,
	JvTabBar;

type
	TObjectArray = array of TObject;
	//
	TInspectorForm = class(TForm)
		Panel1: TPanel;
		ObjectCombo: TLMDObjectComboBox;
		InspectorTabs: TJvTabBar;
		JvModernTabBarPainter2: TJvModernTabBarPainter;
		ObjectInspector: TObjectInspector;
    JvModernTabBarPainter1: TJvModernTabBarPainter;
		procedure FormCreate(Sender: TObject);
		procedure ObjectInspectorChangedPropValue(Sender: TObject;
			const PropEdit: TDCDsgnProperty; var newval: String);
		procedure ObjectComboCloseUp(Sender: TObject);
		procedure InspectorTabsTabSelected(Sender: TObject;
			Item: TJvTabBarItem);
		procedure FormDestroy(Sender: TObject);
		procedure ObjectInspectorDrawCell(Sender: TObject;
			const PropEdit: TDCDsgnProperty; Column: Integer; ARect: TRect;
			var Processed: Boolean);
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
		procedure FormatInspector(inInspector: TCustomObjectInspector);
		procedure SelectionChange(Sender: TObject);
		procedure SetDefaultComponent(const Value: TComponent);
	public
		{ Public declarations }
		procedure AddInspector(const inName: string;
			inInspector: TCustomObjectInspector);
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
	LrUtils, ColorProperty, DesignManager;

{$R *.dfm}

procedure TInspectorForm.FormCreate(Sender: TObject);
begin
	RegisterColorPropertyEditor;
	Inspectors := TList.Create;
	Inspectors.Add(ObjectInspector);
	DesignMgr.SelectionObservers.Add(SelectionChange);
	DesignMgr.PropertyObservers.Add(SelectionChange);
	DesignMgr.DesignObservers.Add(DesignChange);
end;

procedure TInspectorForm.FormDestroy(Sender: TObject);
begin
	Inspectors.Free;
end;

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
{
		FixedColWidth := 100;
		Style := isCustom;
		Options := Options - [ oiUseGroups, oiSunkenEditor ];
		Colors.BackColor := $00FCFCFC;
		Colors.Col0Color := $00454545;
		Colors.Col1Color := clBlack;
		Colors.HighlightBkgnd := clWindow;
		Colors.HighlightColor := clBlack;
		Colors.HorzLineColor := $00D1E6E9;
		Colors.VertLineColor := $00D1E6E9;
}
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
	//SelectionChange(Sender);
end;

procedure TInspectorForm.ClearSelection;
var
	i: Integer;
begin
	ObjectCombo.SelectedObjects.Clear;
	for i := 0 to Pred(Inspectors.Count) do
		TCustomObjectInspector(Inspectors[i]).SetControls(nil);
end;

procedure TInspectorForm.AddToSelection(inObject: TPersistent);
var
	i: Integer;
begin
	ObjectCombo.SelectedObjects.Add(inObject);
	for i := 0 to Pred(Inspectors.Count) do
		TCustomObjectInspector(Inspectors[i]).AddControl(inObject);
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
var
	i: Integer;
begin
	for i := 0 to Pred(Inspectors.Count) do
		with TCustomObjectInspector(Inspectors[i]) do
		begin
			Visible := (i = Item.Index);
			if Visible then
				Rescan;
		end;
	//if Assigned(OnTabSelected) then
	//	OnTabSelected(Self);
end;

procedure TInspectorForm.ObjectInspectorDrawCell(Sender: TObject;
	const PropEdit: TDCDsgnProperty; Column: Integer; ARect: TRect;
	var Processed: Boolean);
begin
	Processed := (Column = 1)
		and (PropEdit.GetPropInfo.PropType^.Name = 'TColor');
	if Processed then
		with ObjectInspector.Canvas do
		begin
			FillRect(ARect);
			Pen.Color := clBlack;
			Brush.Color := StringToColor(PropEdit.GetValue);
			with ARect do
				Rectangle(Left + 1, Top + 1, Left + 14, Bottom - 2);
			//	FillRect(Rect(Left + 1, Top + 1, Left + 8, Bottom - 4));
			Brush.Color := ObjectInspector.Colors.BackColor;
			TextOut(ARect.Left + 17, ARect.Top + 1, PropEdit.GetValue);
		end;
end;

end.
