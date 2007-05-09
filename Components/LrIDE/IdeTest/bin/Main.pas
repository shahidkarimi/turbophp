unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ImgList, ComCtrls, ToolWin, ExtCtrls, Menus, ActnList, ActnMan,
	XPStyleActnCtrls,
	PngImageList,
	aqDockingUtils, aqDockingBase, aqDocking, aqDockingUI,
	dxBar, dxBarExtItems;

const
	TM_APPSTART = WM_USER + $01;

type
	TMainForm = class(TForm)
		DocumentsPanel: TPanel;
		aqDockingManager1: TaqDockingManager;
		aqStyleManager1: TaqStyleManager;
		aqDockingSite1: TaqDockingSite;
		DocumentsDock: TaqDockingControl;
		PngImageList1: TPngImageList;
		ActionManager1: TActionManager;
    NewAction: TAction;
    dxBarManager1: TdxBarManager;
    dxBarSubItem1: TdxBarSubItem;
    dxBarButton1: TdxBarButton;
    dxBarButton2: TdxBarButton;
		procedure FormCreate(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure InspectorDockEnter(Sender: TObject);
		procedure InspectorDockExit(Sender: TObject);
		procedure NewActionExecute(Sender: TObject);
	private
		{ Private declarations }
		function CreateDock(const inName,
			inGuid: string): TaqCustomDockingControl;
		procedure CreateDocumentControllers;
		procedure CreateDocumentsView;
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
	LrUtils, LrDocument, LrProjectView, LrOpenDocumentsController,
	OpenDocumentsView, MyDocumentHost, MyDocumentController;

{$R *.dfm}

const
	cConfigFolder = 'config\';
	cDockingFile = 'aq.docking.cfg';

var
	MyDocumentController: TMyDocumentController;

procedure TMainForm.FormCreate(Sender: TObject);
begin
	DiscoverPaths;
	//
	CreateDocumentControllers;
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

procedure TMainForm.CreateDocumentControllers;
begin
	LrOpenDocuments := TLrOpenDocumentsController.Create;
	MyDocumentController := TMyDocumentController.Create;
end;

function TMainForm.CreateDock(
	const inName, inGuid: string): TaqCustomDockingControl;
begin
	// Creates a new panel
	Result := aqDockingManager1.Items.Add;
	Result.DockingKey := StringToGUID(inGuid);
	Result.Caption := inName;
end;

procedure TMainForm.CreateDocumentsView;
begin
	AddForm(OpenDocumentsForm, TOpenDocumentsForm, DocumentsDock);
	AddForm(MyDocumentHostForm, TMyDocumentHostForm, OpenDocumentsForm, alClient,
		false);
end;

procedure TMainForm.CreateProjectView;
var
	d: TaqCustomDockingControl;
begin
	d := CreateDock('Project', '{D2BC53B1-2DFB-42C5-BC7E-6A00BB935E00}');
	aqDockingManager1.Dock(d, DocumentsDock, drtRight, false);
	AddForm(LrProjectForm, TLrProjectForm, d);
	d.Visible := True;
end;

procedure TMainForm.CreateInspectorView;
var
	d: TaqCustomDockingControl;
begin
	d := CreateDock('Inspector', '{7AFDC068-31F4-49F5-8070-5F72F7BEEE41}');
	aqDockingManager1.Dock(d, DocumentsDock, drtLeft, false);
	d.Visible := True;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	aqDockingManager1.SaveToFile(DockingConfigPath);
{
	ControllerModule.Shutdown;
	SaveDockLayout(aqDockingManager1, ConfigHome + cDockingFile);
	TurboDocumentHostForm.SaveDockConfig;
}
end;

procedure TMainForm.TMAppStart(var Message: TMessage);
begin
	if FileExists(DockingConfigPath) then
		aqDockingManager1.LoadFromFile(DockingConfigPath);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
//	CanClose := ControllerModule.CloseAll and ControllerModule.CloseProject;
end;

procedure TMainForm.InspectorDockEnter(Sender: TObject);
begin
{
	with TaqCustomDockingControl(Sender) do
		if ActiveControl <> nil then
			ActiveControl.RemoveFreeNotification(TComponent(Sender));
}
end;

procedure TMainForm.InspectorDockExit(Sender: TObject);
begin
{
	with TaqCustomDockingControl(Sender) do
		if ActiveControl <> nil then
			ActiveControl.FreeNotification(TComponent(Sender));
}
end;

procedure TMainForm.NewActionExecute(Sender: TObject);
begin
	MyDocumentController.New;
end;

end.
