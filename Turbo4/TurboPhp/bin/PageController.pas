unit PageController;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls,
	dcfdes,
	dxDockControl, dxDockPanel,
	EasyEditState,
	LrDockUtils, LrDocument,
	PageDocument, PageView,
	DesignScrollBox, IEView, InspectorView, ComponentTreeView, DesignView,
	PhpEditView, JsEditView, CodeExplorerView, HtmlEditView{, CodeExpert};

type
	TPageController = class(TLrController)
	private
		FDocument: TPageDocument;
		FOnSelectionChange: TNotifyEvent;
    FOnPropChange: TNotifyEvent;
	protected
		function GetView: TPageViewForm;
		procedure SetDocument(const Value: TPageDocument);
	protected
		//CodeExpert: TCodeExpert;
		JsState: TEasyEditState;
		LastFocus: TWinControl;
		NameList: TStringList;
		PhpState: TEasyEditState;
		TabState: TDockTabsState;
	protected
		procedure BuildNameList;
		procedure DesignPagesChange(Sender: TObject);
		procedure DesignSelect(Sender: TObject);
		procedure PageChange(inSender: TObject);
		procedure PropChanged(Sender: TObject);
		procedure SelectDesignerClick(Sender: TObject);
		procedure SelectComponent(inComponent: TComponent);
	public
		{ Public declarations }
		constructor Create(inDocument: TPageDocument); reintroduce;
		destructor Destroy; override;
		procedure Activate; override;
		procedure CodeExpertUpdate(inSender: TObject);
		procedure Deactivate; override;
		procedure LazyUpdate; override;
		procedure Publish;
		procedure ShowOutput;
		procedure SyncViewToPage;
		procedure UpdateCode;
	public
		property Document: TPageDocument read FDocument write SetDocument;
		property View: TPageViewForm read GetView;
		property OnPropChange: TNotifyEvent read FOnPropChange
			write FOnPropChange;
		property OnSelectionChange: TNotifyEvent read FOnSelectionChange
			write FOnSelectionChange;
	end;

implementation

uses
	LrUtils, LrTagParser,
	ThComponentIterator, ThCssStyle, ThJavaScript, ThPageControl,
	TpForm,
	PhpEventProperty, JsInspector,
	Main;

constructor TPageController.Create(inDocument: TPageDocument);
begin
	inherited Create(nil);
//	CodeExpert := TCodeExpert.Create(View.PhpEditForm.Edit);
	NameList := TStringList.Create;
	TabState := TDockTabsState.Create(View);
	PhpState := TEasyEditState.Create;
	JsState := TEasyEditState.Create;
	Document := inDocument;
end;

destructor TPageController.Destroy;
begin
//	CodeExpert.Free;
	NameList.Free;
	JsState.Free;
	PhpState.Free;
	TabState.Free;
	inherited;
end;

function TPageController.GetView: TPageViewForm;
begin
	Result := MainForm.PageView;
end;

procedure TPageController.SetDocument(const Value: TPageDocument);
begin
	FDocument := Value;
	InsertForm(Document.DesignForm, View.Scroller, alNone);
	Document.DesignForm.DCLiteDesigner.PopupMenu := MainForm.DesignPopup;
	Document.DesignForm.OnActivate := MainForm.EditorEnter;
	Document.DesignForm.OnSelectionChange := DesignSelect;
	Document.Page.OnChange := PageChange;
end;

procedure TPageController.SyncViewToPage;
begin
	with Document do
	begin
		DesignForm.BoundsRect := Page.BoundsRect;
		if ThVisibleColor(Page.Style.Color) then
			DesignForm.Color := Page.Style.Color
		else
			DesignForm.Color := clWhite;
		View.Scroller.Color := DesignForm.Color;
		with DesignForm.DCLiteDesigner do
		begin
			ShowComponents := Page.ShowComponents;
			ShowGrid := Page.ShowGrid;
			ShowCaptions := Page.ShowCaptions;
		end;
	end;
end;

procedure TPageController.Activate;
begin
	View.PhpEditForm.OnModified := Document.ChangeEvent;
	View.PhpEditForm.OnLazyUpdate := CodeExpertUpdate;
	View.PhpEditForm.Strings := Document.Page.PhpSource;
	//
	View.JsEditForm.OnModified := Document.ChangeEvent;
	View.JsEditForm.Strings := Document.Page.JsSource;
	//
	View.DesignPages.OnChange := DesignPagesChange;
	View.OnSelectComponent := SelectComponent;
	View.RsRulerCorner.OnClick := SelectDesignerClick;
	//
	InspectorForm.OnPropChanged := PropChanged;
	//
	Document.DesignForm.Parent := View.Scroller;
	Document.DesignForm.ActivateDesigner;
	//
	SyncViewToPage;
	//
	ActivateDock(View.DesignDock);
	//
	TabState.Restore;
	//
	if (LastFocus <> nil) and LastFocus.CanFocus then
		LastFocus.SetFocus;
	//
	JsState.SetState(View.JsEditForm.Edit);
	PhpState.SetState(View.PhpEditForm.Edit);
end;

procedure TPageController.Deactivate;
begin
	View.PhpEditForm.OnModified := nil;
	View.PhpEditForm.OnLazyUpdate := nil;
	View.JsEditForm.OnModified := nil;
	View.DesignPages.OnChange := nil;
	View.OnSelectComponent := nil;
	View.RsRulerCorner.OnClick := nil;
	InspectorForm.OnPropChanged := nil;
	//
	Document.DesignForm.DeactivateDesigner;
	Document.DesignForm.Parent := View.HideSheet;
	//
	LazyUpdate;
	TabState.Capture;
	LastFocus := MainForm.LastEditor;
	JsState.GetState(View.JsEditForm.Edit);
	PhpState.GetState(View.PhpEditForm.Edit);
end;

procedure TPageController.BuildNameList;
var
	i: Integer;
begin
	NameList.Clear;
	with TThComponentIterator.Create(Document.DesignForm) do
	try
		while Next do
		begin
			NameList.Add('$' + Component.Name + '=' + Component.ClassName);
			if (Component is TTpForm) then
				with TTpForm(Component) do
					for i := 0 to Pred(Inputs.Count) do
						NameList.Add('$' + Inputs[i].WebName + '=' + 'TTpInput');
		end;
	finally
		Free;
	end;
end;

procedure TPageController.CodeExpertUpdate(inSender: TObject);
begin
	BuildNameList;
	View.PhpEditForm.ObjectList := NameList;
end;

procedure TPageController.LazyUpdate;
begin
	//BuildNameList;
	//CodeExpert.UpdateGlobals(NameList);
	UpdateCode;
end;

procedure TPageController.UpdateCode;
begin
	View.PhpEditForm.ValidateMethods(Document.DesignForm);
	//
	Document.Page.PhpSource.Assign(View.PhpEditForm.Source.Strings);
	Document.UpdatePageName(View.PhpEditForm.Source.Strings);
	//
	View.JsEditForm.ValidateMethods(Document.DesignForm);
	Document.Page.JsSource.Assign(View.JsEditForm.Source.Strings);
end;

procedure TPageController.PageChange(inSender: TObject);
begin
	SyncViewToPage;
end;

procedure TPageController.PropChanged(Sender: TObject);
begin
	Document.Modified := true;
	View.Refresh;
	if Assigned(OnPropChange) then
		OnPropChange(Self);
end;

procedure TPageController.DesignSelect(Sender: TObject);
begin
	if Document = nil then
		begin
			View.DesignSelect(nil, nil, nil, nil);
		end
	else with Document do
		begin
			View.DesignSelect(DesignForm.SelectedComponents, DesignForm,
				DesignForm.Selection, Page);
			//
			if Assigned(OnSelectionChange) then
				OnSelectionChange(Self);
			//
			// This needs to be moved elsewhere
			if (DesignForm.Selection is TThSheet) then
				TThSheet(DesignForm.Selection).Select;
		end;
end;

procedure TPageController.SelectComponent(inComponent: TComponent);
begin
	Document.DesignForm.Selection := inComponent;
end;

procedure TPageController.DesignPagesChange(Sender: TObject);
begin
	if View.DesignPages.ActivePage = View.OutputSheet then
		ShowOutput;
end;

procedure TPageController.ShowOutput;
var
	s: TStringList;
begin
	s := Document.GenerateHtml;
	with TLrTaggedDocument.Create do
	try
		Text := s.Text;
		s.Clear;
		Indent(s);
	finally
		Free;
	end;
	View.DesignHtml := s; //Document.GenerateHtml;
end;

procedure TPageController.SelectDesignerClick(Sender: TObject);
begin
	Document.DesignForm.SetFocus;
end;

procedure TPageController.Publish;
begin
	View.DebugForm.Clear;
	View.DebugForm.Parent.Visible := Document.Page.Debug;
	PhpState.GetState(View.PhpEditForm.Edit);
	Document.Publish;
	View.PhpEditForm.Source.Strings.Assign(Document.Page.PhpSource);
	PhpState.SetState(View.PhpEditForm.Edit);
end;

end.
