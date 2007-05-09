unit Controller;

interface

uses
	SysUtils, Classes, ActnList, Dialogs, Forms, Graphics,
	LrDocument, Project, Config;

type
	TControllerModule = class(TDataModule)
		ActionList1: TActionList;
    PublishAction: TAction;
		SaveAsAction: TAction;
		OpenAction: TAction;
		OpenDialog: TOpenDialog;
		SaveDialog: TSaveDialog;
		SetupDatabasesAction: TAction;
		SaveAction: TAction;
    NewTurboPhpAction: TAction;
		CloseAllAction: TAction;
		SetupServersAction: TAction;
		SaveProjectAction: TAction;
		SaveProjectAsAction: TAction;
		CloseProjectAction: TAction;
    ViewPreviewAction: TAction;
    NewTurboAjaxAction: TAction;
		procedure PublishActionExecute(Sender: TObject);
		procedure DataModuleCreate(Sender: TObject);
		procedure SaveAsActionExecute(Sender: TObject);
		procedure OpenActionExecute(Sender: TObject);
		procedure SetupDatabasesActionExecute(Sender: TObject);
		procedure NewTurboPhpActionExecute(Sender: TObject);
		procedure CloseAllActionExecute(Sender: TObject);
		procedure SaveActionExecute(Sender: TObject);
		procedure SetupServersActionExecute(Sender: TObject);
		procedure SaveProjectActionUpdate(Sender: TObject);
		procedure SaveProjectActionExecute(Sender: TObject);
		procedure SaveProjectAsActionExecute(Sender: TObject);
		procedure SaveActionUpdate(Sender: TObject);
		procedure CloseProjectActionExecute(Sender: TObject);
		procedure DataModuleDestroy(Sender: TObject);
    procedure ViewPreviewActionExecute(Sender: TObject);
    procedure NewTurboAjaxActionExecute(Sender: TObject);
	private
		{ Private declarations }
		function CreateTurboAjaxDocument: TLrDocument;
		function CreateTurboPhpDocument: TLrDocument;
		function GetDocument: TLrDocument;
		function OpenDocument(inClass: TLrDocumentClass;
			const inFilename: string): TLrDocument;
		function OpenImageDocument(const inFilename: string): TLrDocument;
		function OpenPhpDocument(const inFilename: string): TLrDocument;
		function OpenProject(const inFilename: string): TLrDocument;
		function OpenTurboAjaxDocument(const inFilename: string): TLrDocument;
		function OpenTurboPhpDocument(const inFilename: string): TLrDocument;
		procedure CreateStartPage;
		procedure NewDocument(inDocument: TLrDocument);
		procedure NewProject;
	public
		{ Public declarations }
		function CloseAll: Boolean;
		function CloseProject: Boolean;
		function Open(const inFilename: string): TLrDocument;
		function SaveDocumentAs: Boolean;
		function SaveProjectAs: Boolean;
		procedure Shutdown;
		procedure Startup;
		property Document: TLrDocument read GetDocument;
	end;

var
	ControllerModule: TControllerModule;
	Project: TProject;

const
	cTurboPhpDocuments = 'TurboPhp Documents';
	//cTurboPhpPages = 'TurboPhp Pages';
	cTurboPhpProjects = 'TurboPhp Projects';
	//cPageExt = '.tphp';
	cProjectExt = '.tprj';
	cPhpExt = '.php';
	cConfigProject = 'Project';

implementation

uses
	LrUtils,
	Globals,
	Documents, LiteBrowserDocument, PhpDocument, ImageDocument,
	TurboPhpDocument, TurboAjaxDocument,
	DesignHost, Inspector, ProjectView, BrowserView,
	DatabasesSetup, ServerSetup, TurboDocumentHost;

{$R *.dfm}

procedure TControllerModule.DataModuleCreate(Sender: TObject);
begin
	OpenDialog.Filter := MakeFilter(
		cTurboPhpDocuments,
		'*' + TTurboAjaxDocument.DocumentExt + ';' +
		'*' + TTurboPhpDocument.DocumentExt + ';' +
		'*' + cProjectExt + ';' +
		'*' + cPhpExt + ';' +
		GraphicFileMask(TGraphic)
	);
	OpenDialog.InitialDir := ProjectsHome;
	//
	SaveDialog.Filter := MakeFilter(
		TTurboAjaxDocument.DocumentDescription,
		'*' + TTurboAjaxDocument.DocumentExt
//		TTurboPhpDocument.DocumentDescription,
//		'*' + TTurboPhpDocument.DocumentExt
	);
	SaveDialog.DefaultExt := TTurboAjaxDocument.DocumentExt;
	//SaveDialog.DefaultExt := TTurboPhpDocument.DocumentExt;
	SaveDialog.InitialDir := ProjectsHome;
end;

procedure TControllerModule.DataModuleDestroy(Sender: TObject);
begin
	Project.Free;
end;

procedure TControllerModule.Startup;
begin
	DocumentsForm.DocumentPanel.Visible := false;
	NewProject;
	CreateStartPage;
	OpenProject(Configuration.Values[cConfigProject]);
end;

procedure TControllerModule.Shutdown;
begin
	Configuration.Values[cConfigProject] := Project.Filename;
end;

procedure TControllerModule.CreateStartPage;
begin
	with TLiteBrowserDocument.Create do
	begin
		Filename := 'Start';
		Html.Add('Welcome to TurboPhp!');
		Desktop.AddDocument(ThisDocument);
	end;
end;

function TControllerModule.GetDocument: TLrDocument;
begin
	Result := Desktop.Current;
end;

function TControllerModule.SaveProjectAs: Boolean;
begin
	Result := Project.SaveAs(SaveDialog);
end;

procedure TControllerModule.NewProject;
begin
	Project := TProject.Create;
	ProjectForm.Project := Project;
	Desktop.AddDocumentObserver(Project.DocumentChange);
end;

function TControllerModule.CloseProject: Boolean;
begin
	Result := Project.CanClose;
	if Result then
		if Project.Modified {and Project.Untitled} then
			Result := SaveProjectAs;
end;

function TControllerModule.SaveDocumentAs: Boolean;
begin
	Result := Document.SaveAs(SaveDialog);
end;

function TControllerModule.CloseAll: Boolean;
begin
	Result := Desktop.CloseAll;
end;

function TControllerModule.CreateTurboAjaxDocument: TLrDocument;
begin
	Result := TTurboAjaxDocument.Create;
	Result.Filename :=
		Project.Documents.GenerateUniqueSource(Result.DisplayName);
end;

function TControllerModule.CreateTurboPhpDocument: TLrDocument;
begin
	Result := TTurboPhpDocument.Create;
	Result.Filename :=
		Project.Documents.GenerateUniqueSource(Result.DisplayName);
end;

function TControllerModule.Open(const inFilename: string): TLrDocument;
var
	e: string;
begin
	e := LowerCase(ExtractFileExt(inFilename));
	if (e = TTurboAjaxDocument.DocumentExt) then
		Result := OpenTurboAjaxDocument(inFilename)
	else if (e = TTurboPhpDocument.DocumentExt) then
		Result := OpenTurboPhpDocument(inFilename)
	else if (e = cProjectExt) then
		Result := OpenProject(inFilename)
	else if (e = cPhpExt) then
		Result := OpenPhpDocument(inFilename)
	else
		Result := OpenImageDocument(inFilename);
end;

function TControllerModule.OpenProject(const inFilename: string): TLrDocument;
begin
	Project.Open(inFilename);
	Result := Project;
end;

function TControllerModule.OpenDocument(inClass: TLrDocumentClass;
	const inFilename: string): TLrDocument;
begin
	Result := Desktop.FindDocument(inFilename);
	if Result <> nil then
		Desktop.Current := Result
	else begin
		Result := inClass.Create;
		Result.Open(inFilename);
		Desktop.AddDocument(Result);
	end;
end;

function TControllerModule.OpenTurboAjaxDocument(
	const inFilename: string): TLrDocument;
begin
	Result := OpenDocument(TTurboAjaxDocument, inFilename);
end;

function TControllerModule.OpenTurboPhpDocument(
	const inFilename: string): TLrDocument;
begin
	Result := OpenDocument(TTurboPhpDocument, inFilename);
end;

function TControllerModule.OpenPhpDocument(
	const inFilename: string): TLrDocument;
begin
	Result := OpenDocument(TPhpDocument, inFilename);
end;

function TControllerModule.OpenImageDocument(
	const inFilename: string): TLrDocument;
begin
	Result := OpenDocument(TImageDocument, inFilename);
end;

procedure TControllerModule.PublishActionExecute(Sender: TObject);
begin
	if Desktop.Current is TPublishableDocument then
	begin
		TurboDocumentHostForm.LazyUpdate;
		with TPublishableDocument(Desktop.Current) do
			BrowserForm.ForceNavigate(Publish(Project));
	end;
//	with TTurboPhpDocument(Desktop.Current) do
//	begin
//		Generate(Project.PublishFolder[ThisDocument]);
//		BrowserForm.ForceNavigate(Project.PublishUrl[ThisDocument]);
//	end;
	//BrowserForm.ForceNavigate('http://localhost/turbophp5/test.html');
	//BrowserForm.ForceNavigate(Home + 'test.html');
end;

procedure TControllerModule.NewDocument(inDocument: TLrDocument);
begin
	inDocument.New;
	Desktop.AddDocument(inDocument);
	Project.AddDocumentItem(inDocument.Filename);
end;

procedure TControllerModule.NewTurboPhpActionExecute(Sender: TObject);
begin
	NewDocument(CreateTurboPhpDocument);
end;

procedure TControllerModule.NewTurboAjaxActionExecute(Sender: TObject);
begin
	NewDocument(CreateTurboAjaxDocument);
end;

procedure TControllerModule.OpenActionExecute(Sender: TObject);
begin
	with OpenDialog do
		if Execute then
			Open(Filename);
end;

procedure TControllerModule.SaveActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := Document.Modified;
end;

procedure TControllerModule.SaveActionExecute(Sender: TObject);
begin
	TurboDocumentHostForm.LazyUpdate;
	if Document.Untitled then
		SaveDocumentAs
	else
		Document.Save;
end;

procedure TControllerModule.SaveAsActionExecute(Sender: TObject);
begin
	TurboDocumentHostForm.LazyUpdate;
	SaveDocumentAs;
end;

procedure TControllerModule.CloseAllActionExecute(Sender: TObject);
begin
	CloseAll;
end;

procedure TControllerModule.SetupDatabasesActionExecute(Sender: TObject);
begin
	with TDatabasesSetupForm.Create(nil) do
	try
		Databases := Project.Databases;
		ShowModal;
	finally
		Free;
	end;
	Project.Modified := true;
end;

procedure TControllerModule.SetupServersActionExecute(Sender: TObject);
begin
	with TServerSetupForm.Create(nil) do
	try
		Servers := Project.Servers;
		ShowModal;
	finally
		Free;
	end;
	Project.Modified := true;
end;

procedure TControllerModule.SaveProjectActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := Project.Modified;
end;

procedure TControllerModule.SaveProjectActionExecute(Sender: TObject);
begin
	if Project.Untitled then
		SaveProjectAs
	else
		Project.Save;
end;

procedure TControllerModule.SaveProjectAsActionExecute(Sender: TObject);
begin
	SaveProjectAs;
end;

procedure TControllerModule.CloseProjectActionExecute(Sender: TObject);
begin
	if CloseProject then
		NewProject;
end;

procedure TControllerModule.ViewPreviewActionExecute(Sender: TObject);
begin
	TurboDocumentHostForm.PreviewDock.Visible := true;
end;

end.
