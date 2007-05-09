program LrProjectTest;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  LrProjectView in 'LrProjectView.pas' {LrProjectForm},
  LrTreeData in 'LrTreeData.pas',
  LrUtils in '..\LrLib\LrUtils.pas',
  LrVclUtils in '..\LrLib\LrVclUtils.pas',
  LrProject in 'LrProject.pas',
  LrDocument in 'LrDocument.pas',
  Servers in 'Servers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
