program LrWorkshop;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Design in '..\Design.pas' {DesignForm},
  DesignView in '..\DesignView.pas' {DesignViewForm},
  CustomInspector in '..\DreamInspector\CustomInspector.pas',
  DynamicInspector in '..\DreamInspector\DynamicInspector.pas',
  DesignManager in '..\DesignManager.pas',
  ComponentTree in '..\ComponentTree.pas' {ComponentTreeForm},
  PropertyBar in '..\PropertyBar.pas' {PropertyBarForm},
  DynamicProperties in '..\DynamicProperties.pas',
  ScriptPanel in '..\ScriptPanel.pas',
  ComponentPalette in '..\ComponentPalette.pas' {ComponentPaletteForm},
  Inspector in '..\DreamInspector\Inspector.pas' {InspectorForm},
  ClassInfo in '..\ClassInfo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
