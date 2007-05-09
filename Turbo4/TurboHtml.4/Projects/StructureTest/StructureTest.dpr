program StructureTest;

uses
  Forms,
  Main in 'Main.pas' {Form3},
  TpJsCodeDesigner in '..\..\..\TurboPhp.4\TpJsCodeDesigner.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
