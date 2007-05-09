unit DemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ComCtrls, ToolWin, LrPageControl, ExtCtrls, StdCtrls;

type
	TDemoMainForm = class(TForm)
		ToolBar1: TToolBar;
		ToolButton1: TToolButton;
		ToolButton2: TToolButton;
    Pages: TLrPageControl;
    LrTabSheet1: TLrTabSheet;
    LrTabSheet2: TLrTabSheet;
    Panel1: TPanel;
    Button1: TButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    LrTabSheet3: TLrTabSheet;
    LrTabSheet4: TLrTabSheet;
    LrTabControl1: TLrTabControl;
		procedure FormCreate(Sender: TObject);
		procedure ToolButton1Click(Sender: TObject);
		procedure ToolButton2Click(Sender: TObject);
    procedure ToolBar1CustomDrawButton(Sender: TToolBar;
      Button: TToolButton; State: TCustomDrawState;
      var DefaultDraw: Boolean);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
	DemoMainForm: TDemoMainForm;

implementation

uses
	uxTheme;

{$R *.dfm}

procedure TDemoMainForm.FormCreate(Sender: TObject);
begin
{
	Pages := TLrPageControl.Create(Self);
	Pages.Parent := Self;
	Pages.Align := alClient;
	Pages.AddSheet.Color := clLime;
	Pages.AddSheet;
}
end;

procedure TDemoMainForm.ToolButton1Click(Sender: TObject);
begin
	Pages.ActivePageIndex := 0;
end;

procedure TDemoMainForm.ToolButton2Click(Sender: TObject);
begin
	Pages.ActivePageIndex := 1;
end;

procedure TDemoMainForm.ToolBar1CustomDrawButton(Sender: TToolBar;
	Button: TToolButton; State: TCustomDrawState; var DefaultDraw: Boolean);
var
	theme: HTHEME;
	r: TRect;
	s: Integer;
begin
	theme := OpenThemeData(Handle, 'tab');
	DefaultDraw := (theme = 0);
	if not DefaultDraw then
	begin
		s := TIS_NORMAL;
		if (cdsHot in State) then
			s := TIS_HOT;
		if (cdsSelected in State) or (Button.ImageIndex = 1) then
			s := TIS_SELECTED;
			// TIS_DISABLED
			// TIS_FOCUSED
			// TIS_SELECTED
		r := Button.BoundsRect;
{
		if (s <> TIS_SELECTED) then
			with r do
				r := Rect(Left, Top + 4, Right, Bottom)
		else
			with r do
				r := Rect(Left - 2, Top, Right + 2, Bottom);
}
		DrawThemeBackground(theme, Sender.Canvas.Handle,	TABP_TABITEM, s, r, nil);
	end;
end;

end.
