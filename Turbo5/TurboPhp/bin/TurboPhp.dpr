program TurboPhp;

uses
  madExcept,
  madLinkDisAsm,
  MemCheck,
  Forms,
  DatabaseSetup in 'DatabaseSetup.pas' {DatabaseSetupForm},
  DatabasesSetup in 'DatabasesSetup.pas' {DatabasesSetupForm},
  Palette in 'Palette.pas' {ComponentPaletteForm},
  Registration in 'Registration.pas',
  DesignManager in 'DesignManager.pas',
  Generator in 'Generator.pas',
  Controller in 'Controller.pas' {ControllerModule: TDataModule},
  TurboPhpDocument in 'TurboPhpDocument.pas',
  Desktop in 'Desktop.pas',
  Documents in 'Documents.pas' {DocumentsForm},
  DesignHost in 'DesignHost.pas' {DesignHostForm},
  CodeEdit in 'CodeEdit.pas' {CodeEditForm},
  CodeExplorer in 'CodeExplorer.pas' {CodeExplorerForm},
  PhpParser in 'PhpParser.pas',
  LiteBrowser in 'LiteBrowser.pas' {LiteBrowserForm},
  LiteBrowserDocument in 'LiteBrowserDocument.pas',
  DockingUtils in 'AQDL\DockingUtils.pas',
  Globals in 'Globals.pas',
  PhpDocument in 'PhpDocument.pas',
  ImageView in 'ImageView.pas' {ImageViewForm},
  ImageDocument in 'ImageDocument.pas',
  Main in 'AQDL\Main.pas' {MainForm},
  TurboDocumentHost in 'AQDL\TurboDocumentHost.pas' {TurboDocumentHostForm},
  Servers in 'Servers.pas',
  ServerDatabases in 'ServerDatabases.pas',
  Design in 'LrDesign\Design.pas' {DesignForm},
  LrProjectView in '..\..\..\Components\LrProject\LrProjectView.pas' {LrProjectForm},
  Project in 'Project.pas',
  ProjectView in 'ProjectView.pas' {ProjectForm},
  ServerSetup in 'ServerSetup.pas' {ServerSetupForm},
  Config in 'Config.pas',
  StyleActionBar in 'StyleActionBar.pas' {StyleActionBarForm},
  Browser in '..\..\TurboAjax\bin\Browser.pas' {BrowserForm},
  tpTooltip in '..\CDK\Source\tpTooltip.pas',
  Inspector in 'JvInspector\Inspector.pas' {InspectorForm},
  InspectorItems in 'JvInspector\InspectorItems.pas',
  ComponentTreeView in 'ComponentTreeView.pas' {ComponentTreeForm},
  TurboAjaxDocument in 'TurboAjaxDocument.pas',
  tpGrid in '..\CDK\Source\tpGrid.pas',
  test in '..\CDK\Source\test.pas' {Form1};

{$R *.res}

begin
	//MemChk;
  Application.Initialize;
  Application.CreateForm(TControllerModule, ControllerModule);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
