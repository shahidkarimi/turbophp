unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ImgList, ComCtrls, ToolWin, ExtCtrls, Menus, ActnList, ActnMan,
	XPStyleActnCtrls,
	PngImageList,
	aqDockingUtils, aqDockingBase, aqDocking, aqDockingUI,
	dxBar, dxBarExtItems,
	madExceptVcl,
	LrIDEController, XPMan;

const
	TM_APPSTART = WM_USER + $01;

type
	TMainForm = class(TForm)
		DocumentsPanel: TPanel;
		aqDockingManager: TaqDockingManager;
		aqStyleManager1: TaqStyleManager;
		aqDockingSite1: TaqDockingSite;
		DocumentsDock: TaqDockingControl;
		ActionManager1: TActionManager;
		NewAction: TAction;
		dxBarManager1: TdxBarManager;
    FileMenu: TdxBarSubItem;
    NewButton: TdxBarButton;
		InspectorDock: TaqDockingControl;
		ComponentsDock: TaqDockingControl;
		ProjectDock: TaqDockingControl;
		QuickPropertiesDock: TaqDockingControl;
		PaletteDock: TaqDockingControl;
    SaveButton: TdxBarButton;
		OpenButton: TdxBarButton;
    SaveAsButton: TdxBarButton;
    MadExceptionHandler1: TMadExceptionHandler;
		CloseButton: TdxBarButton;
    CloseAllButton: TdxBarButton;
    ViewMenu: TdxBarSubItem;
    ViewProjectButton: TdxBarButton;
    ViewProjectAction: TAction;
    ViewComponentsAction: TAction;
		ViewInspectorDock: TAction;
		ViewPaletteDock: TAction;
		ViewQuickPropertiesAction: TAction;
		ViewComponentsButton: TdxBarButton;
		ViewInspectorButton: TdxBarButton;
    ViewPaletteButton: TdxBarButton;
    ViewQuickPropertiesButton: TdxBarButton;
    CodeExplorerDock: TaqDockingControl;
    ViewCodeExplorerAction: TAction;
    ViewCodeExplorerButton: TdxBarButton;
    QuickHelpDock: TaqDockingControl;
    ViewQuickHelpAction: TAction;
    ViewQuickHelpButton: TdxBarButton;
    NewAjaxAction: TAction;
    NewPhpAction: TAction;
    NewJsAction: TAction;
    NewDocumentItem: TdxBarSubItem;
    NewAjaxButton: TdxBarButton;
    NewPhpButton: TdxBarButton;
    NewJsButton: TdxBarButton;
    PngImageList: TPngImageList;
    dxBarSubItem1: TdxBarSubItem;
    PublishButton: TdxBarButton;
    PublishAction: TAction;
    dxBarButton2: TdxBarButton;
    dxBarButton1: TdxBarButton;
    PreviewDock: TaqDockingControl;
    ViewPreviewAction: TAction;
    dxBarButton3: TdxBarButton;
		procedure FormCreate(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure NewActionExecute(Sender: TObject);
		procedure aqDockingSite1CanUndock(Sender: TaqCustomDockingSite;
			Control: TaqCustomDockingControl; var Allow: Boolean);
		procedure aqDockingManagerUpdateActions(
			Sender: TaqCustomDockingManager; Control: TaqCustomDockingControl);
		procedure ViewActionUpdate(Sender: TObject);
		procedure ViewActionExecute(Sender: TObject);
    procedure NewAjaxActionExecute(Sender: TObject);
    procedure NewPhpActionExecute(Sender: TObject);
    procedure NewJsActionExecute(Sender: TObject);
    procedure PublishActionUpdate(Sender: TObject);
    procedure PublishActionExecute(Sender: TObject);
	private
		{ Private declarations }
		function GetViews(inIndex: Integer): TaqDockingControl;
		procedure CreateInspector;
		procedure CreateIDEController;
		procedure CreateViews;
		procedure DefaultDocking;
		procedure DesignFilter(inSender, inObject: TObject;
			var inFilter: Boolean);
		procedure DiscoverPaths;
		procedure TMAppStart(var Message: TMessage); message TM_APPSTART;
		procedure HostJavaScriptDocuments;
		procedure HostPhpDocuments;
		procedure HostTurboDocuments;
	public
		{ Public declarations }
	end;

var
	MainForm: TMainForm;
	Home: string;
	BinHome: string;
	ConfigHome: string;
	DockingConfigPath: string;

implementation

uses
	UxTheme, ShellApi, Jpeg, GraphicEx,
	LrUtils, LrDocument, LrProjectView,
	DesignManager, ComponentPalette, DynamicProperties,
	DynamicInspector, Inspector, ComponentTree, CodeExplorer,
	Browser, StyleActionBar,
	Registration,
	OpenDocumentsView,
	TurboDocumentHost, TurboDocumentController,
	JavaScriptDocumentHost, JavaScriptDocumentController,
	PhpDocumentHost, PhpDocumentController,
	TurboAjaxDocument;

{$R *.dfm}

const
	cConfigFolder = 'config\';
	cDockingFile = 'aq.docking.cfg';

procedure TMainForm.FormCreate(Sender: TObject);
begin
	DiscoverPaths;
	Registration.Register;
	CreateIDEController;
	DesignMgr.OnFilter := DesignFilter;
	DefaultDocking;
	CreateViews;
	CreateInspector;
	HostJavaScriptDocuments;
	HostPhpDocuments;
	HostTurboDocuments;
	PostMessage(Handle, TM_APPSTART, 0, 0);
end;

procedure TMainForm.DiscoverPaths;
begin
	BinHome := ExtractFilePath(Application.ExeName);
	Home := ExpandFileName(BinHome + '..\');
	ConfigHome := Home + cConfigFolder;
	DockingConfigPath := ConfigHome + cDockingFile;
end;

procedure TMainForm.CreateIDEController;
begin
	LrIdeControllerModule := TLrIdeControllerModule.Create(Application);
end;

procedure TMainForm.DefaultDocking;
begin
	with aqDockingManager do
	begin
		Dock(ProjectDock, DocumentsDock, drtRight, false);
		ProjectDock.Width := 128;
		Dock(QuickHelpDock, ProjectDock, drtInside, false);
		Dock(InspectorDock, DocumentsDock, drtLeft, false);
		InspectorDock.Width := 128;
		Dock(PaletteDock, InspectorDock, drtTop, false);
		PaletteDock.Height := 96;
		Dock(CodeExplorerDock, InspectorDock, drtTop, false);
		CodeExplorerDock.Height := 96;
		Dock(ComponentsDock, InspectorDock, drtTop, false);
		ComponentsDock.Height := 96;
		Dock(QuickPropertiesDock, DocumentsDock, drtTop, false);
		QuickPropertiesDock.Height := 64;
		Dock(PreviewDock, DocumentsDock, drtInside, false);
	end;
end;

procedure TMainForm.CreateViews;
begin
	AddForm(OpenDocumentsForm, TOpenDocumentsForm, DocumentsDock);
	AddForm(LrProjectForm, TLrProjectForm, ProjectDock);
	AddForm(ComponentPaletteForm, TComponentPaletteForm, PaletteDock);
	AddForm(ComponentTreeForm, TComponentTreeForm, ComponentsDock);
	AddForm(StyleActionBarForm, TStyleActionBarForm, QuickPropertiesDock);
	AddForm(CodeExplorerForm, TCodeExplorerForm, CodeExplorerDock);
	AddForm(BrowserForm, TBrowserForm, PreviewDock);
end;

procedure TMainForm.CreateInspector;
var
	dpi: TDynamicPropertyInspector;
begin
	AddForm(InspectorForm, TInspectorForm, InspectorDock);
	{
	InspectorForm := TInspectorForm.Create(Application);
	while InspectorForm.ControlCount > 0 do
		InspectorForm.Controls[0].Parent := InspectorDock;
	}
	dpi := TDynamicPropertyInspector.Create(InspectorForm);
	dpi.CollectionName := 'JavaScript';
	InspectorForm.AddInspector('JS Events', dpi);
end;

procedure TMainForm.HostTurboDocuments;
begin
	AddForm(TurboDocumentHostForm, TTurboDocumentHostForm, OpenDocumentsForm,
		alClient, false);
	LrIdeControllerModule.RegisterDocument(TTurboDocumentController);
	LrIdeControllerModule.RegisterDocument(TTurboAjaxController);
end;

procedure TMainForm.HostJavaScriptDocuments;
begin
	AddForm(JavaScriptDocumentHostForm, TJavaScriptDocumentHostForm,
		OpenDocumentsForm,	alClient, false);
	LrIdeControllerModule.RegisterDocument(TJavaScriptDocumentController);
end;

procedure TMainForm.HostPhpDocuments;
begin
	AddForm(PhpDocumentHostForm, TPhpDocumentHostForm,
		OpenDocumentsForm,	alClient, false);
	LrIdeControllerModule.RegisterDocument(TPhpDocumentController);
end;

procedure TMainForm.TMAppStart(var Message: TMessage);
begin
	if FileExists(DockingConfigPath) then
		aqDockingManager.LoadFromFile(DockingConfigPath);
	SetThemeAppProperties(STAP_ALLOW_NONCLIENT or STAP_ALLOW_CONTROLS or
		STAP_ALLOW_WEBCONTENT);
	PostMessage(BrowserForm.Handle, WM_THEMECHANGED, 0, 0);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	CanClose := LrIdeControllerModule.CloseAllDocuments;
//	CanClose := ControllerModule.CloseAll and ControllerModule.CloseProject;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	aqDockingManager.SaveToFile(DockingConfigPath);
{
	ControllerModule.Shutdown;
	SaveDockLayout(aqDockingManager1, ConfigHome + cDockingFile);
	TurboDocumentHostForm.SaveDockConfig;
}
end;

procedure TMainForm.aqDockingSite1CanUndock(Sender: TaqCustomDockingSite;
	Control: TaqCustomDockingControl; var Allow: Boolean);
begin
	Allow := (Control <> DocumentsDock);
end;

procedure TMainForm.aqDockingManagerUpdateActions(
	Sender: TaqCustomDockingManager; Control: TaqCustomDockingControl);
begin
	Control.Actions[idactUndock].Enabled := not (Control = DocumentsDock);
	Control.Actions[idactMaximize].Enabled := not (Control = QuickPropertiesDock);
end;

function TMainForm.GetViews(inIndex: Integer): TaqDockingControl;
begin
	case inIndex of
		0: Result := ComponentsDock;
		1: Result := InspectorDock;
		2: Result := PaletteDock;
		3: Result := ProjectDock;
		4: Result := QuickPropertiesDock;
		5: Result := CodeExplorerDock;
		6: Result := QuickHelpDock;
		7: Result := PreviewDock;
		else Result := ComponentsDock;
	end;
end;

procedure TMainForm.ViewActionUpdate(Sender: TObject);
begin
	with TAction(Sender) do
		Checked := GetViews(Tag).Visible;
end;

procedure TMainForm.ViewActionExecute(Sender: TObject);
begin
	with GetViews(TAction(Sender).Tag) do
		Visible := not Visible;
end;

procedure TMainForm.DesignFilter(inSender: TObject; inObject: TObject;
	var inFilter: Boolean);
begin
	inFilter := inFilter
						or (inObject = nil)
						or (inObject is TDynamicProperties)
						;
end;

procedure TMainForm.NewActionExecute(Sender: TObject);
begin
	LrIdeControllerModule.NewDocument(TTurboDocumentController.GetExt);
end;

procedure TMainForm.NewAjaxActionExecute(Sender: TObject);
begin
	LrIdeControllerModule.NewDocument(TTurboAjaxController.GetExt);
end;

procedure TMainForm.NewPhpActionExecute(Sender: TObject);
begin
	LrIdeControllerModule.NewDocument(TPhpDocumentController.GetExt);
end;

procedure TMainForm.NewJsActionExecute(Sender: TObject);
begin
	LrIdeControllerModule.NewDocument(TJavaScriptDocumentController.GetExt);
end;

procedure TMainForm.PublishActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled :=
		LrIdeControllerModule.Document is TTurboAjaxDocument;
end;

procedure TMainForm.PublishActionExecute(Sender: TObject);
begin
	TTurboAjaxDocument(LrIdeControllerModule.Document).Publish;
	//ShellExecute(0, '', 'C:\inetpub\wwwroot\test\test.html', '', '', 0);
	BrowserForm.Navigate('http://localhost/turboajax/TurboStudio/publish/test.html');
end;

end.
