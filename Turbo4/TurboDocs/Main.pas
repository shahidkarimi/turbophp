unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, EasyClasses, EasyParser, EasyEditor,
  EasyEditSource;

type
  TForm3 = class(TForm)
    EasyEdit1: TEasyEdit;
    EasyEditorParser1: TEasyEditorParser;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    EasyEditSource1: TEasyEditSource;
    procedure ToolButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

const
	cTestFile = 'F:\Turbo4\TurboHtml.4\ThLabel.pas';

procedure TForm3.ToolButton1Click(Sender: TObject);
begin
	EasyEditSource1.Strings.LoadFromFile(cTestFile);
end;

end.
