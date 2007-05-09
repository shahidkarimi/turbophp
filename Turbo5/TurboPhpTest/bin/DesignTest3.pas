unit DesignTest3;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls, StdCtrls, ComCtrls, ToolWin;

type
	TDesignForm2 = class(TForm)
		StatusBar1: TStatusBar;
		Panel2: TPanel;
		PageControl1: TPageControl;
		TabSheet1: TTabSheet;
		TabSheet2: TTabSheet;
		Button1: TButton;
		Panel1: TPanel;
		Label2: TLabel;
		Edit1: TEdit;
		Label1: TLabel;
		ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
	DesignForm2: TDesignForm2;

implementation

uses
	DesignController;

{$R *.dfm}

{ TDesignForm2 }

procedure TDesignForm2.FormCreate(Sender: TObject);
begin
	with TDesignController.Create(Self) do
	begin
		Container := Panel2;
		FullDrag := true;
		Active := true;
	end;
	Show;
end;

end.
