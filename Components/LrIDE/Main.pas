unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ImgList, ComCtrls, ToolWin, ExtCtrls, Menus, ActnList, ActnMan,
	XPStyleActnCtrls,
	PngImageList,
	aqDockingUtils, aqDockingBase, aqDocking, aqDockingUI,
	dxBar, dxBarExtItems,
	LrIDEController;

const
	TM_APPSTART = WM_USER + $01;

type
	TMainForm = class(TForm)
		DocumentsPanel: TPanel;
    aqDockingManager: TaqDockingManager;
		aqStyleManager1: TaqStyleManager;
		aqDockingSite1: TaqDockingSite;
		DocumentsDock: TaqDockingControl;
		PngImageList1: TPngImageList;
		ActionManager1: TActionManager;
    NewAction: TAction;
    dxBarManager1: TdxBarManager;
    dxBarSubItem1: TdxBarSubItem;
    NewButton: TdxBarButton;
    SaveButton: TdxBarButton;
    OpenButton: TdxBarButton;
    SaveAsButton: TdxBarButton;
    InspectorDock: TaqDockingControl;
    ProjectDock: TaqDockingControl;
    dxBarSubItem2: TdxBarSubItem;
    dxBarButton1: TdxBarButton;
    dxBarButton2: TdxBarButton;
		procedure FormCreate(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure NewActionExecute(Sender: TObject);
	private
		{ Private declarations }
		procedure CreateDocumentsView;
		procedure CreateIDEController;
		procedure CreateInspectorView;
		procedure CreateProjectView;
		procedure DiscoverPaths;
		procedure TMAppStart(var Message: TMessage); message TM_APPSTART;
	public
		{ Public declarations }
	end;

var
	MainForm: TMainForm;
	//
	Home: string;
	BinHome: string;
	ConfigHome: string;
	DockingConfigPath: string;

implementation

uses
	LrUtils, LrDocument, LrProjectView, 
	OpenDocumentsView, MyDocumentHost, MyDocumentController;

{$R *.dfm}

const
	cConfigFolder = 'config\';
	cDockingFile = 'aq.docking.cfg';

procedure TMainForm.FormCreate(Sender: TObject);
begin
	DiscoverPaths;
	//
	CreateIdeController;
	//
	CreateDocumentsView;
	CreateProjectView;
	CreateInspectorView;
	//
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
	LrIdeControllerModule.RegisterDocument(TMyDocumentController);
end;

procedure TMainForm.CreateDocumentsView;
begin
	AddForm(OpenDocumentsForm, TOpenDocumentsForm, DocumentsDock);
	AddForm(MyDocumentHostForm, TMyDocumentHostForm, OpenDocumentsForm, alClient,
		false);
end;

procedure TMainForm.CreateProjectView;
begin
	AddForm(LrProjectForm, TLrProjectForm, ProjectDock);
	aqDockingManager.Dock(ProjectDock, DocumentsDock, drtRight, false);
end;

procedure TMainForm.CreateInspectorView;
begin
	aqDockingManager.Dock(InspectorDock, DocumentsDock, drtLeft, false);
end;

procedure TMainForm.TMAppStart(var Message: TMessage);
begin
	if FileExists(DockingConfigPath) then
		aqDockingManager.LoadFromFile(DockingConfigPath);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	CanClose := LrIdeControllerModule.CloseAllDocuments; // and ControllerModule.CloseProject;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	aqDockingManager.SaveToFile(DockingConfigPath);
end;

procedure TMainForm.NewActionExecute(Sender: TObject);
begin
	LrIdeControllerModule.NewDocument(TMyDocumentController.GetExt);
end;

end.
