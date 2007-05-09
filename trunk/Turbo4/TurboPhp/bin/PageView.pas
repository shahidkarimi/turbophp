unit PageView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls, Buttons, Menus,
	RsRuler,
	dxDockControl, dxDockPanel, 
	dccommon, dcfdes,
	EasyEditState,
	CodeExplorerView, ComponentTreeView, DebugView, DesignScrollBox, DesignView,
	HtmlEditView, IEView, InspectorView, JsEditView, PageDocument,
	PhpEditView, ComCtrls;

type
	TSelectComponentEvent = procedure(inComponent: TComponent) of object;
	//
	TPageViewForm = class(TForm)
		DocumentPanel: TPanel;
		ClientDockSite: TdxDockSite;
		dxLayoutDockSite1: TdxLayoutDockSite;
		dxHorizContainerDockSite1: TdxHorizContainerDockSite;
		dxVertContainerDockSite2: TdxVertContainerDockSite;
		ComponentListDock: TdxDockPanel;
		ControlTreeDock: TdxDockPanel;
		InspectorDock: TdxDockPanel;
		dxTabContainerDockSite2: TdxTabContainerDockSite;
		DesignDock: TdxDockPanel;
    DesignPages: TPageControl;
    DesignSheet: TTabSheet;
		RulersLeftPanel: TPanel;
		LeftRuler: TRsRuler;
		RulesTopPanel: TPanel;
		TopRuler: TRsRuler;
    RsRulerCorner: TRsRulerCorner;
    OutputSheet: TTabSheet;
    HideSheet: TTabSheet;
		PhpDock: TdxDockPanel;
		HtmlDock: TdxDockPanel;
		JsDock: TdxDockPanel;
		PreviewDock: TdxDockPanel;
		DebugDock: TdxDockPanel;
		PaletteDock: TdxDockPanel;
		procedure FormCreate(Sender: TObject);
		procedure InspectorDockResize(Sender: TObject);
	private
		{ Private declarations }
		FOnSelectComponent: TSelectComponentEvent;
		function ComponentListFilter(inObject: TObject): Boolean;
		procedure ComponentListSelect(Sender: TObject);
		procedure InspectorFormSelect(Sender: TObject);
		procedure InspectPersistent(inObject: TPersistent);
		procedure ScrollerCanResize(Sender: TObject; var NewWidth,
			NewHeight: Integer; var Resize: Boolean);
		procedure SelectComponent(inComponent: TComponent);
		procedure SelectObject(inObject: TPersistent);
		procedure ShowJsSource(Sender: TObject; inX, inY: Integer);
		procedure ShowPhpSource(Sender: TObject; inX, inY: Integer);
		procedure SetDesignHtml(Value: TStringList);
		procedure SetDockPopup(Value: TPopupMenu);
		procedure SetEditPopup(const Value: TPopupMenu);
	public
		{ Public declarations }
		PreviewForm: TIEForm;
		//ComponentListForm: TComponentTreeForm;
		ControlTreeForm: TComponentTreeForm;
		DebugForm: TDebugForm;
		HtmlEditForm: THtmlEditForm;
		//InspectorForm: TInspectorForm;
		JsEditForm: TJsEditForm;
		JsExplorerForm: TCodeExplorerForm;
		PhpEditForm: TPhpEditForm;
		PhpExplorerForm: TCodeExplorerForm;
		Scroller: TDesignScrollBox;
		function ControlTreeFilter(inObject: TObject): Boolean;
	public
		procedure DesignSelect(inList: TList;
			inRoot: TWinControl; inSelected, inDefault: TComponent);
		procedure Refresh;
		property DesignHtml: TStringList write SetDesignHtml;
		property DockPopup: TPopupMenu write SetDockPopup;
		property EditPopup: TPopupMenu write SetEditPopup;
		property OnSelectComponent: TSelectComponentEvent read FOnSelectComponent
			write FOnSelectComponent;
	end;

implementation

uses
	LrUtils, LrDockUtils, ThJavaScript, PhpEventProperty,
	JsInspector, PageController, Main;

{$R *.dfm}

procedure TPageViewForm.FormCreate(Sender: TObject);
begin
	Scroller := TDesignScrollBox.Create(Self);
	Scroller.Parent := DesignSheet;
	Scroller.OnCanResize := ScrollerCanResize;
	//
	AddForm(PhpEditForm, TPhpEditForm, PhpDock);
	PhpEditForm.CodeDesigner.OnShowSource := ShowPhpSource;
	TPhpEventProperty.SetCodeDesigner(PhpEditForm.CodeDesigner);
	//
	AddForm(HtmlEditForm, THtmlEditForm, OutputSheet);
	HtmlEditForm.Edit.ReadOnly := true;
	//
	AddForm(InspectorForm, TInspectorForm, InspectorDock);
	InspectorForm.OnSelect := InspectorFormSelect;
	//InspectorForm.ObjectInspector.OnActivePropertyChanged :=
	//	InspectorActivePropertyChanged;
	//
	//AddForm(ComponentListForm, TComponentTreeForm, ComponentListDock);
	//ComponentListForm.OnSelect := ComponentListSelect;
	//ComponentListForm.OnFilter := ComponentListFilter;
	AddForm(ControlTreeForm, TComponentTreeForm, ControlTreeDock);
	ControlTreeForm.OnSelect := ComponentListSelect;
	//ControlTreeForm.OnFilter := ControlTreeFilter;
	ControlTreeForm.OnFilter := ComponentListFilter;
	//
	AddForm(PreviewForm, TIEForm, PreviewDock);
	//
	AddForm(JsEditForm, TJsEditForm, JsDock);
	JsEditForm.CodeDesigner.OnShowSource := ShowJsSource;
	TJsEventProperty.SetCodeDesigner(JsEditForm.CodeDesigner);
	//
	AddForm(DebugForm, TDebugForm, DebugDock);
end;

procedure TPageViewForm.SetDockPopup(Value: TPopupMenu);
begin
	PhpDock.PopupMenu := Value;
	JsDock.PopupMenu := Value;
	InspectorDock.PopupMenu := Value;
	ComponentListDock.PopupMenu := Value;
	ControlTreeDock.PopupMenu := Value;
	PreviewDock.PopupMenu := Value;
end;

procedure TPageViewForm.SetEditPopup(const Value: TPopupMenu);
begin
	HtmlEditForm.Edit.PopupMenu := Value;
	JsEditForm.Edit.PopupMenu := Value;
	PhpEditForm.Edit.PopupMenu := Value;
	InspectorForm.ObjectInspector.PopupMenu := Value;
end;

procedure TPageViewForm.ScrollerCanResize(Sender: TObject; var NewWidth,
	NewHeight: Integer; var Resize: Boolean);
begin
	with TScrollBox(Sender) do
	begin
		TopRuler.Offset := HorzScrollBar.Position;
		LeftRuler.Offset := VertScrollBar.Position;
	end;
end;

function TPageViewForm.ComponentListFilter(
	inObject: TObject): Boolean;
begin
	Result := {(inObject is TControl) or} (inObject is TThJavaScriptEvents)
		or (inObject is TDCLiteDesigner);
end;

function TPageViewForm.ControlTreeFilter(inObject: TObject): Boolean;
begin
	Result := not (inObject is TControl);
end;

procedure TPageViewForm.SelectComponent(inComponent: TComponent);
begin
	if Assigned(OnSelectComponent) then
		OnSelectComponent(inComponent);
end;

procedure TPageViewForm.InspectPersistent(inObject: TPersistent);
begin
	SelectComponent(nil);
	InspectorForm.InspectPersistent(inObject);
end;

procedure TPageViewForm.SelectObject(inObject: TPersistent);
begin
	if inObject is TComponent then
		SelectComponent(TComponent(inObject))
	else
		InspectPersistent(inObject);
end;

procedure TPageViewForm.ComponentListSelect(Sender: TObject);
begin
	SelectObject(TComponentTreeForm(Sender).Selected);
end;

procedure TPageViewForm.InspectorFormSelect(Sender: TObject);
begin
	SelectObject(InspectorForm.Selected);
end;

procedure TPageViewForm.ShowJsSource(Sender: TObject;
	inX, inY: Integer);
begin
	ActivateDock(JsDock);
	JsEditForm.ShowSource(Sender, inX, inY);
end;

procedure TPageViewForm.ShowPhpSource(Sender: TObject;
	inX, inY: Integer);
begin
	ActivateDock(PhpDock);
	PhpEditForm.ShowSource(Sender, inX, inY);
end;

procedure TPageViewForm.SetDesignHtml(Value: TStringList);
var
	s: TEasyEditState;
begin
	try
		s := TEasyEditState.Create;
		try
			s.GetState(HtmlEditForm.Edit);
			HtmlEditForm.Source.Strings.Assign(Value);
			s.SetState(HtmlEditForm.Edit);
		finally
			s.Free;
		end;
		PostMessage(MainForm.Handle, CM_LAZY_FOCUS, Integer(HtmlEditForm.Edit), 0);
	finally
		Value.Free;
	end;
end;

procedure TPageViewForm.InspectorDockResize(Sender: TObject);
begin
{
	if InspectorDock.ParentDockControl is TdxVertContainerDockSite then
		with TdxVertContainerDockSite(InspectorDock.ParentDockControl) do
			if Width > 250 then
				Width := 250;
}
end;

procedure TPageViewForm.DesignSelect(inList: TList;
	inRoot: TWinControl; inSelected, inDefault: TComponent);
begin
	if (inList <> nil) then
	begin
		InspectorForm.DefaultComponent := inDefault;
		InspectorForm.Root := inRoot;
	end;
	InspectorForm.InspectComponents(inList);
	//
//		ComponentListForm.Root := inRoot;
//		ComponentListForm.Selected := inSelected;
	//
	ControlTreeForm.Root := inRoot;
	ControlTreeForm.Selected := inSelected;
end;

procedure TPageViewForm.Refresh;
begin
	//ComponentListForm.Refresh;
	ControlTreeForm.Refresh;
end;

end.
