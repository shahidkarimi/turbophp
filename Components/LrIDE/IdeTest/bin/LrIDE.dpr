program LrIDE;

uses
  Forms,
  OpenDocumentsView in 'OpenDocumentsView.pas' {OpenDocumentsForm},
  Main in '..\..\Main.pas' {MainForm},
  MyDocumentHost in 'MyDocumentHost.pas' {MyDocumentHostForm},
  MyDocumentController in 'MyDocumentController.pas',
  LrDocumentList in '..\..\LrDocumentList.pas',
  LrOpenDocumentsController in '..\..\LrOpenDocumentsController.pas',
  LrDocument in '..\..\LrDocument.pas',
  LrProject in '..\..\LrProject.pas',
  LrProjectView in '..\..\LrProjectView.pas' {LrProjectForm},
  LrTreeData in '..\..\LrTreeData.pas',
  LrIDEController in '..\..\LrIDEController.pas' {LrIDEControllerModule: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
