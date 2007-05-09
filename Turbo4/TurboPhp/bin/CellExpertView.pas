unit CellExpertView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ComCtrls, ThPanel;

type
  TCellExpertForm = class(TForm)
    UpDown1: TUpDown;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
  private
    { Private declarations }
  public
		{ Public declarations }
		Panel: TThCustomPanel;
  end;

var
  CellExpertForm: TCellExpertForm;

implementation

{$R *.dfm}

procedure TCellExpertForm.Button3Click(Sender: TObject);
begin
	Panel.Align := alTop;
end;

procedure TCellExpertForm.Button1Click(Sender: TObject);
begin
	Panel.Align := alLeft;
end;

procedure TCellExpertForm.Button2Click(Sender: TObject);
begin
	Panel.Align := alRight;
end;

procedure TCellExpertForm.Button4Click(Sender: TObject);
begin
	Panel.Align := alBottom;
end;

procedure TCellExpertForm.UpDown1Click(Sender: TObject;
	Button: TUDBtnType);
begin
	if Button = btNext then
		Panel.Tag := Panel.Tag + 1
	else
		Panel.Tag := Panel.Tag - 1;
	Panel.Parent.Realign;
end;

end.
