unit Inspector;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls,
	JvComponent, JvInspector, JvExControls, JvTabBar,
	LMDDsgComboBox;

type
	TInspectorForm = class(TForm)
		JvInspector1: TJvInspector;
		JvInspectorDotNETPainter1: TJvInspectorDotNETPainter;
		Panel2: TPanel;
		JvTabBar1: TJvTabBar;
		Panel1: TPanel;
		ObjectCombo: TLMDObjectComboBox;
		JvModernTabBarPainter1: TJvModernTabBarPainter;
		procedure JvInspector1AfterItemCreate(Sender: TObject;
			Item: TJvCustomInspectorItem);
		procedure JvInspector1ItemValueChanged(Sender: TObject;
			Item: TJvCustomInspectorItem);
		procedure JvInspector1DataValueChanged(Sender: TObject;
			Data: TJvCustomInspectorData);
		procedure ObjectComboCloseUp(Sender: TObject);
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
		FOnValueChanged: TNotifyEvent;
	protected
		function GetSelected: TObject;
		function ObjectComboSelectedArray: TJvInstanceArray;
		procedure ListDesignObjects;
	public
		{ Public declarations }
		procedure DesignChange(Sender: TObject);
		procedure SelectionChange(Sender: TObject);
		property OnValueChanged: TNotifyEvent read FOnValueChanged
			write FOnValueChanged;
		property Selected: TObject read GetSelected;
	end;

var
	InspectorForm: TInspectorForm;

implementation

uses
	InspectorItems, DesignManager;

{$R *.dfm}

{ TInspectorForm }

procedure TInspectorForm.FormCreate(Sender: TObject);
begin
	DesignMgr.SelectionObservers.Add(SelectionChange);
	DesignMgr.PropertyObservers.Add(SelectionChange);
	DesignMgr.DesignObservers.Add(DesignChange);
end;

procedure TInspectorForm.DesignChange(Sender: TObject);
begin
	JvInspector1.Clear;
	ListDesignObjects;
end;

procedure TInspectorForm.ListDesignObjects;
var
	i: Integer;
begin
	ObjectCombo.SelectedObjects.Clear;
	ObjectCombo.Objects.Clear;
	for i := 0 to Pred(Length(DesignMgr.DesignObjects)) do
		ObjectCombo.Objects.Add(TPersistent(DesignMgr.DesignObjects[i]));
end;

procedure TInspectorForm.SelectionChange(Sender: TObject);
var
	i: Integer;
begin
	ObjectCombo.SelectedObjects.Clear;
	for i := 0 to Pred(Length(DesignMgr.Selected)) do
		ObjectCombo.SelectedObjects.Add(TPersistent(DesignMgr.Selected[i]));
	JvInspector1.Clear;
	TJvInspectorPropData.New(JvInspector1.Root, DesignMgr.Selected);
end;

function TInspectorForm.GetSelected: TObject;
begin
	if ObjectCombo.SelectedObjects.Count > 0 then
		Result := ObjectCombo.SelectedObjects[0]
	else
		Result := nil;
end;

procedure TInspectorForm.JvInspector1AfterItemCreate(Sender: TObject;
	Item: TJvCustomInspectorItem);
begin
	if Item is TJvInspectorBooleanItem then
		TJvInspectorBooleanItem(Item).ShowAsCheckbox := True;
end;

procedure TInspectorForm.JvInspector1ItemValueChanged(Sender: TObject;
	Item: TJvCustomInspectorItem);
begin
	if Assigned(OnValueChanged) then
		OnValueChanged(Self);
end;

procedure TInspectorForm.JvInspector1DataValueChanged(Sender: TObject;
	Data: TJvCustomInspectorData);
begin
	if Assigned(OnValueChanged) then
		OnValueChanged(Self);
	DesignMgr.PropertyChange(Self);
end;

function TInspectorForm.ObjectComboSelectedArray: TJvInstanceArray;
var
	i: Integer;
begin
	SetLength(Result, ObjectCombo.SelectedObjects.Count);
	for i := 0 to Pred(ObjectCombo.SelectedObjects.Count) do
		Result[i] := ObjectCombo.SelectedObjects[i];
end;

procedure TInspectorForm.ObjectComboCloseUp(Sender: TObject);
begin
	DesignMgr.ObjectsSelected(nil, ObjectComboSelectedArray);
end;

initialization
	RegisterInspectorItems;
end.
