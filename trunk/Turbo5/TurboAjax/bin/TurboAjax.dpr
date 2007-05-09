program TurboAjax;

uses
  madExcept,
  madLinkDisAsm,
  Forms,
  CodeEdit in 'CodeEdit.pas' {CodeEditForm},
  CodeExplorer in 'CodeExplorer.pas' {CodeExplorerForm},
  OpenDocumentsView in 'OpenDocumentsView.pas' {OpenDocumentsForm},
  Main in 'Main.pas' {MainForm},
  PhpParser in 'PhpParser.pas',
  TurboDocumentHost in 'TurboDocumentHost.pas' {TurboDocumentHostForm},
  TurboDocumentController in 'TurboDocumentController.pas',
  TurboDocument in 'TurboDocument.pas',
  Registration in 'Registration.pas',
  LrIDEController in '..\..\..\Components\LrIDE\LrIDEController.pas' {LrIDEControllerModule: TDataModule},
  TextDocument in 'TextDocument.pas',
  JavaScriptDocumentController in 'JavaScriptDocumentController.pas',
  JavaScriptDocumentHost in 'JavaScriptDocumentHost.pas',
  PhpDocumentController in 'PhpDocumentController.pas',
  PhpDocumentHost in 'PhpDocumentHost.pas',
  CodeDocumentController in 'CodeDocumentController.pas',
  Design in 'Design.pas' {DesignForm},
  TurboAjaxDocument in 'TurboAjaxDocument.pas',
  Browser in 'Browser.pas' {BrowserForm},
  StyleActionBar in 'StyleActionBar.pas' {StyleActionBarForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
