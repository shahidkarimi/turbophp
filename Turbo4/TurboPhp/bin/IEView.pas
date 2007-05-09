unit IEView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ComCtrls, ToolWin, ExtCtrls, ImgList, OleCtrls, SHDocVw,
	dcedit,
	HtmlEditView, dxDockControl, dxDockPanel;

type
	TIEForm = class(TForm)
		ImageList1: TImageList;
    Pages: TPageControl;
    BrowserSheet: TTabSheet;
    WebBrowser: TWebBrowser;
    StatusBar: TStatusBar;
    Panel1: TPanel;
    BrowserToolBar: TToolBar;
    GoButton: TToolButton;
    ToolBar1: TToolBar;
    StopButton: TToolButton;
    RefreshButton: TToolButton;
    BackButton: TToolButton;
    UrlEdit: TDCEdit;
    SourceSheet: TTabSheet;
		procedure GoButtonClick(Sender: TObject);
		procedure StopButtonClick(Sender: TObject);
		procedure RefreshButtonClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure WebBrowserNavigateComplete2(Sender: TObject;
			const pDisp: IDispatch; var URL: OleVariant);
		procedure BackButtonClick(Sender: TObject);
		procedure WebBrowserStatusTextChange(Sender: TObject;
			const Text: WideString);
		procedure WebBrowserCommandStateChange(Sender: TObject;
			Command: Integer; Enable: WordBool);
		procedure WebBrowserNewWindow2(Sender: TObject; var ppDisp: IDispatch;
			var Cancel: WordBool);
    procedure PagesChange(Sender: TObject);
	private
		{ Private declarations }
		LastUrl: string;
	protected
		function GetSource: string;
		procedure SetParent(AParent: TWinControl); override;
		procedure SetSource(const Value: string);
	protected
		procedure NewBrowser;
		function Wait: Boolean;
	public
		{ Public declarations }
		HtmlEditForm: THtmlEditForm;
		procedure Back;
		procedure ForceNavigate(const inUrl: string);
		procedure Navigate(const inUrl: string);
		procedure Refresh;
		function Stop: Boolean;
		property Source: string read GetSource write SetSource;
	end;

var
	IEForm: TIEForm;

implementation

uses
 LrUtils, LrWebBrowserUtils;

{$R *.dfm}

procedure TIEForm.FormCreate(Sender: TObject);
begin
	HtmlEditForm := THtmlEditForm.Create(Self);
	InsertForm(HtmlEditForm, SourceSheet);
	Pages.ActivePage := BrowserSheet;
	//NewBrowser;
end;

procedure TIEForm.WebBrowserStatusTextChange(Sender: TObject;
	const Text: WideString);
begin
	StatusBar.SimpleText := Text;
end;

procedure TIEForm.WebBrowserCommandStateChange(Sender: TObject;
	Command: Integer; Enable: WordBool);
begin
	if Cardinal(Command) = CSC_NAVIGATEBACK	then
		BackButton.Enabled := Enable
	else if Cardinal(Command) = CSC_UPDATECOMMANDS then
		StopButton.Enabled := WebBrowser.Busy;
end;

procedure TIEForm.SetParent(AParent: TWinControl);
begin
//	inherited;
//	exit;
	//
	if (csLoading in ComponentState) or (csDestroying in ComponentState)
		or (AParent = nil) then
		inherited
	else begin
		try
			Stop;
		except
		end;
		inherited;
		NewBrowser;
		if LastUrl <> '' then
			Navigate(LastUrl);
	end;
end;

procedure TIEForm.NewBrowser;
var
	w: TWebBrowser;
begin
	w := TWebBrowser.Create(Self);
	w.Align := alClient;
	BrowserSheet.InsertControl(w);
	//BrowserDock.InsertControl(w);
	w.OnCommandStateChange := WebBrowserCommandStateChange;
	//w.OnDocumentComplete := WebBrowserDocumentComplete;
	w.OnNewWindow2 := WebBrowserNewWindow2;
	w.OnStatusTextChange := WebBrowserStatusTextChange;
	WebBrowser.Free;
	WebBrowser := w;
end;

function TIEForm.Wait: Boolean;
var
	i: Integer;
begin
	Result := false;
	if (WebBrowser <> nil) then
	begin
		for i := 1 to 100 do
			if (WebBrowser.ReadyState < READYSTATE_INTERACTIVE) then
			begin
				Sleep(100);
				Application.ProcessMessages;
			end;
		Result := not (WebBrowser.ReadyState < READYSTATE_INTERACTIVE);
	end;
end;

function TIEForm.Stop: Boolean;
var
	i: Integer;
begin
	Result := Wait;
	if Result and WebBrowser.Busy then
	begin
		WebBrowser.Stop;
		for i := 1 to 100 do
			if WebBrowser.Busy then
			begin
				Sleep(100);
				Application.ProcessMessages;
			end;
		Result := not WebBrowser.Busy;
	end;
end;

procedure TIEForm.Refresh;
var
	flags: OleVariant;
begin
	if Stop then
	begin
		flags := REFRESH_COMPLETELY;
		WebBrowser.Refresh2(flags);
	end;
end;

procedure TIEForm.Back;
begin
	if Stop then
	try
		WebBrowser.GoBack;
	except
	end;
end;

procedure TIEForm.Navigate(const inUrl: string);
var
	url: OleVariant;
begin
	if Stop then
	try
		UrlEdit.Text := inUrl;
		url := inUrl;
		WebBrowser.Navigate2(url);
	except
	end;
end;

procedure TIEForm.ForceNavigate(const inUrl: string);
begin
	Stop;
	NewBrowser;
//	if inUrl = LastUrl then
//		Refresh
//	else
		Navigate(inUrl);
end;

procedure TIEForm.WebBrowserNavigateComplete2(Sender: TObject;
	const pDisp: IDispatch; var URL: OleVariant);
begin
	UrlEdit.Text := URL;
	LastUrl := Url;
	//PreviewPagesChange(Self);
end;

procedure TIEForm.GoButtonClick(Sender: TObject);
begin
	Navigate(UrlEdit.Text);
end;

procedure TIEForm.StopButtonClick(Sender: TObject);
begin
	Stop;
end;

procedure TIEForm.RefreshButtonClick(Sender: TObject);
begin
	Refresh;
end;

procedure TIEForm.BackButtonClick(Sender: TObject);
begin
	Back;
end;

procedure TIEForm.WebBrowserNewWindow2(Sender: TObject;
  var ppDisp: IDispatch; var Cancel: WordBool);
begin
	//Cancel := true;
end;

procedure TIEForm.SetSource(const Value: string);
begin
	LrStringToBrowser(WebBrowser, Value);
end;

function TIEForm.GetSource: string;
begin
	Result := LrStringFromBrowser(WebBrowser);
end;

procedure TIEForm.PagesChange(Sender: TObject);
begin
	if Pages.ActivePage = SourceSheet then
		HtmlEditForm.Lines.Text := Source;
end;

end.
