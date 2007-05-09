unit ImageView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TImageViewForm = class(TForm)
    Image: TImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ImageViewForm: TImageViewForm;

implementation

uses
	GraphicEx;
 
{$R *.dfm}

end.
