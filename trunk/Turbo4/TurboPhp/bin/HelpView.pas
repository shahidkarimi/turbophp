unit HelpView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, ImgList, OleCtrls, SHDocVw;

type
	THelpForm = class(TForm)
    WebBrowser: TWebBrowser;
		ImageList1: TImageList;
		ToolBar1: TToolBar;
		RefreshButton: TToolButton;
		BackButton: TToolButton;
		ForwardButton: TToolButton;
		ToolButton2: TToolButton;
		procedure BackButtonClick(Sender: TObject);
		procedure RefreshButtonClick(Sender: TObject);
    procedure ForwardButtonClick(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
		procedure Back;
		procedure Forward;
		procedure Navigate(const inUrl: string);
		procedure Refresh;
		procedure Stop;
	end;

var
	HelpForm: THelpForm;

implementation

{$R *.dfm}

procedure THelpForm.Stop;
var
	i: Integer;
begin
	if WebBrowser.Busy then
	begin
		WebBrowser.Stop;
		for i := 1 to 10 do
			if WebBrowser.Busy then
				Sleep(500);
		if WebBrowser.Busy then
			raise Exception.Create('WebBrowser busy.');
	end;
end;

procedure THelpForm.Refresh;
var
	flags: OleVariant;
begin
	if not WebBrowser.Busy then
	begin
		flags := REFRESH_COMPLETELY;
		WebBrowser.Refresh2(flags);
	end;
end;

procedure THelpForm.Back;
begin
	if not WebBrowser.Busy then
	try
		WebBrowser.GoBack;
	except
	end;
end;

procedure THelpForm.Forward;
begin
	if not WebBrowser.Busy then
	try
		WebBrowser.GoForward;
	except
	end;
end;

procedure THelpForm.Navigate(const inUrl: string);
var
	url: OleVariant;
begin
	Stop;
	url := inUrl;
	WebBrowser.Navigate2(url);
end;

procedure THelpForm.RefreshButtonClick(Sender: TObject);
begin
	Refresh;
end;

procedure THelpForm.BackButtonClick(Sender: TObject);
begin
	Back;
end;

procedure THelpForm.ForwardButtonClick(Sender: TObject);
begin
	Forward;
end;

end.
