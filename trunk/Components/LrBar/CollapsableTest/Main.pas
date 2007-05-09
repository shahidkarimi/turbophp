unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, LrCollapsable, LMDControl, LMDBaseControl,
  LMDBaseGraphicControl, LMDBaseLabel, LMDCustomGlyphLabel, LMDGlyphLabel;

type
  TForm1 = class(TForm)
    LrCollapsable1: TLrCollapsable;
    LMDGlyphLabel1: TLMDGlyphLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
