program LrPageControlDemo;

uses
  Forms,
  DemoMain in 'DemoMain.pas' {DemoMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDemoMainForm, DemoMainForm);
  Application.Run;
end.
