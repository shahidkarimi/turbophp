program AjaxRender;

uses
  Forms,
  FlowTestView in 'FlowTestView.pas' {FlowTestForm},
  Style in 'Style.pas',
  RenderTest in 'RenderTest.pas' {RenderTestForm},
  Flow in 'Flow.pas',
  Node in 'Node.pas',
  Box in 'Box.pas',
  FlowTest in 'FlowTest.pas',
  Render4 in 'Render4.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFlowTestForm, FlowTestForm);
  Application.Run;
end.
