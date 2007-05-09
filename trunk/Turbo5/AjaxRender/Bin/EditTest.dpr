program EditTest;

uses
  Forms,
  EditMain in 'EditMain.pas' {Form2},
  Box in 'Box.pas',
  Text in 'Text.pas',
  EditBox in 'EditBox.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
