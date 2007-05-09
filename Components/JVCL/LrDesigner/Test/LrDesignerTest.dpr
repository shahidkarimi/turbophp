program LrDesignerTest;

uses
  madExcept,
  Forms,
  Main in 'Main.pas' {MainForm},
  Design in 'Design.pas' {DesignForm},
  Utils in 'Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
