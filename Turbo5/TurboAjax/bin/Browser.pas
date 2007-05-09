unit Browser;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ComCtrls, OleCtrls, SHDocVw, ActnList, StdCtrls, ToolWin, ImgList,
	ExtCtrls,
	dxBar,
	aqDockingBase, aqDocking, aqDockingUtils, aqDockingUI,
	LrWebBrowser,
	CodeEdit;

type
	TBrowserForm = class(TForm)
		aqDockingManager1: TaqDockingManager;
		aqStyleManager1: TaqStyleManager;
		aqDockingSite1: TaqDockingSite;
		DisplayDock: TaqDockingControl;
		SourceDock: TaqDockingControl;
		StatusBar: TStatusBar;
		dxBarManager1: TdxBarManager;
		dxBarDockControl1: TdxBarDockControl;
		SizeCombo: TdxBarCombo;
		ScrollBox: TScrollBox;
		CustomdxBarCombo1: TCustomdxBarCombo;
    dxBarCombo1: TdxBarCombo;
    dxBarButton1: TdxBarButton;
    Panel1: TPanel;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolBar2: TToolBar;
    ToolButton3: TToolButton;
    UrlBox: TComboBox;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ActionList1: TActionList;
		BackAction: TAction;
    ForwardAction: TAction;
    RefreshAction: TAction;
    StopAction: TAction;
    GoAction: TAction;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    dxBarButton4: TdxBarButton;
    dxBarButton5: TdxBarButton;
    UrlCombo: TdxBarCombo;
    dxBarButton6: TdxBarButton;
    dxBarButton7: TdxBarButton;
    dxBarButton8: TdxBarButton;
		procedure FormCreate(Sender: TObject);
		procedure SizeComboChange(Sender: TObject);
    procedure BackActionUpdate(Sender: TObject);
		procedure BackActionExecute(Sender: TObject);
		procedure ForwardActionUpdate(Sender: TObject);
		procedure ForwardActionExecute(Sender: TObject);
		procedure RefreshActionUpdate(Sender: TObject);
		procedure RefreshActionExecute(Sender: TObject);
		procedure StopActionExecute(Sender: TObject);
		procedure StopActionUpdate(Sender: TObject);
		procedure GoActionUpdate(Sender: TObject);
		procedure GoActionExecute(Sender: TObject);
    procedure dxBarButton7Click(Sender: TObject);
	private
		{ Private declarations }
		FBrowser: TLrWebBrowser;
		HtmlEditForm: THtmlEditForm;
		LastUrl: string;
	protected
		function GetSource: string;
		function GetUserUrl: string;
		procedure BrowserCanDoStateChange(inSender: TObject);
		procedure BrowserStatusTextChange(Sender: TObject;
			const Text: WideString);
		procedure SetParent(AParent: TWinControl); override;
		procedure SetSource(const Value: string);
		procedure SetUserUrl(const Value: string);
		procedure UpdateBrowserBounds;
		procedure WebBrowserDocumentComplete(Sender: TObject;
			const pDisp: IDispatch; var URL: OleVariant);
		procedure wmthemechanged (var msg: TMessage); message wm_themechanged;
		property UserUrl: string read GetUserUrl write SetUserUrl;
	public
		{ Public declarations }
		procedure ForceNavigate(const inUrl: string);
		procedure Navigate(const inUrl: string);
		procedure NewBrowser;
		property Source: string read GetSource write SetSource;
	end;

var
	BrowserForm: TBrowserForm;

implementation

uses
	LrUtils;

{$R *.dfm}

{ TBrowserForm }

procedure TBrowserForm.FormCreate(Sender: TObject);
begin
	NewBrowser;
	UpdateBrowserBounds;
	AddForm(HtmlEditForm, THtmlEditForm, SourceDock);
	HtmlEditForm.Source.ReadOnly := true;
end;

function TBrowserForm.GetUserUrl: string;
begin
	Result := UrlCombo.Text;
end;

procedure TBrowserForm.SetUserUrl(const Value: string);
begin
	UrlCombo.Text := Value;
end;

procedure TBrowserForm.UpdateBrowserBounds;
begin
	case SizeCombo.ItemIndex of
		0: FBrowser.Align := alClient;
		else FBrowser.Align := alNone;
	end;
	case SizeCombo.ItemIndex of
		1: FBrowser.SetBounds(0, 0, 640, 480);
		2: FBrowser.SetBounds(0, 0, 800, 600);
		3: FBrowser.SetBounds(0, 0, 1024, 768);
		4: FBrowser.SetBounds(0, 0, 1600, 1200);
	end;
end;

function TBrowserForm.GetSource: string;
begin
	Result := FBrowser.Source;
end;

procedure TBrowserForm.SetSource(const Value: string);
begin
	FBrowser.Source := Value;
end;

procedure TBrowserForm.SetParent(AParent: TWinControl);
begin
	if (csLoading in ComponentState) or (csDestroying in ComponentState)
		or (AParent = nil) then
			inherited
	else begin
		if FBrowser <> nil then
			FBrowser.Stop;
		inherited;
		NewBrowser;
		if LastUrl <> '' then
			FBrowser.SafeNavigate(LastUrl);
	end;
end;

procedure TBrowserForm.NewBrowser;
var
	b: TLrWebBrowser;
begin
	b := TLrWebBrowser.Create(Self);
	//b.Align := alClient;
	ScrollBox.InsertControl(b);
	b.OnCanDoStateChange := BrowserCanDoStateChange;
	b.OnDocumentComplete := WebBrowserDocumentComplete;
	//b.OnNewWindow2 := BrowserNewWindow2;
	b.OnStatusTextChange := BrowserStatusTextChange;
	FBrowser.Free;
	FBrowser := b;
	UpdateBrowserBounds;
end;

procedure TBrowserForm.BrowserCanDoStateChange(inSender: TObject);
begin
	//
end;

procedure TBrowserForm.BrowserStatusTextChange(Sender: TObject;
	const Text: WideString);
begin
	StatusBar.SimpleText := Text;
end;

procedure TBrowserForm.Navigate(const inUrl: string);
begin
	FBrowser.SafeNavigate(inUrl);
	LastUrl := inUrl;
	UserUrl := inUrl;
end;

procedure TBrowserForm.ForceNavigate(const inUrl: string);
begin
	if FBrowser.SafeStop then
	begin
		NewBrowser;
		Navigate(inUrl);
	end;
end;

procedure TBrowserForm.SizeComboChange(Sender: TObject);
begin
	UpdateBrowserBounds;
end;

procedure TBrowserForm.BackActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := FBrowser.CanGoBack;
end;

procedure TBrowserForm.BackActionExecute(Sender: TObject);
begin
	FBrowser.GoBack;
end;

procedure TBrowserForm.ForwardActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := FBrowser.CanGoForward;
end;

procedure TBrowserForm.ForwardActionExecute(Sender: TObject);
begin
	FBrowser.GoForward;
end;

procedure TBrowserForm.RefreshActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := not FBrowser.Busy and (FBrowser.LocationUrl <> '');
end;

procedure TBrowserForm.RefreshActionExecute(Sender: TObject);
begin
	FBrowser.SafeRefresh;
end;

procedure TBrowserForm.StopActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := FBrowser.CanStop;
end;

procedure TBrowserForm.StopActionExecute(Sender: TObject);
begin
	FBrowser.Stop;
end;

procedure TBrowserForm.GoActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := UserUrl <> '';
end;

procedure TBrowserForm.GoActionExecute(Sender: TObject);
begin
	ForceNavigate(UserUrl);
end;

procedure TBrowserForm.wmthemechanged(var msg: TMessage);
begin
//	PostMessage(FBrowser.HWND, WM_THEMECHANGED, 0, 0);
end;

procedure TBrowserForm.WebBrowserDocumentComplete(Sender: TObject;
	const pDisp: IDispatch; var URL: OleVariant);
begin
	if Url <> '' then
		HtmlEditForm.Source.Strings.Text := Source;
end;

procedure TBrowserForm.dxBarButton7Click(Sender: TObject);
begin
	//PostMessage(FBrowser.HWND, WM_THEMECHANGED, 0, 0);
	PostMessage(FBrowser.Handle, WM_THEMECHANGED, 0, 0);
end;

end.
