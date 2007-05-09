unit AddJsEventView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TAddJsEventForm = class(TForm)
    EventEdit: TEdit;
    OkButton: TButton;
    Button1: TButton;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AddJsEventForm: TAddJsEventForm;

implementation

{$R *.dfm}

end.
