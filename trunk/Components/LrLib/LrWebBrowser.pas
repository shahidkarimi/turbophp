unit LrWebBrowser;

interface

uses
	SysUtils, Classes, Forms, Variants, ShDocVw;

type
	TLrWebBrowser = class(TWebBrowser)
	private
		FCanStop: Boolean;
		FCanGoBack: Boolean;
		FOnCanDoStateChange: TNotifyEvent;
    FCanGoForward: Boolean;
	protected
		function GetSource: string;
		procedure CanDoStateChange;
		procedure CommandStateChange(Sender: TObject; Command: Integer;
			Enable: WordBool);
		procedure SetCanGoBack(const Value: Boolean);
		procedure SetCanGoForward(const Value: Boolean);
		procedure SetCanStop(const Value: Boolean);
		procedure SetSource(const Value: string);
	public
		constructor Create(AOwner: TComponent); override;
		function SafeStop: Boolean;
		function Wait: Boolean;
		procedure SafeBack;
		procedure SafeNavigate(const inUrl: string);
		procedure SafeRefresh;
		property CanGoBack: Boolean read FCanGoBack write SetCanGoBack;
		property CanGoForward: Boolean read FCanGoForward write SetCanGoForward;
		property CanStop: Boolean read FCanStop write SetCanStop;
		property OnCanDoStateChange: TNotifyEvent read FOnCanDoStateChange
			write FOnCanDoStateChange;
		property Source: string read GetSource write SetSource;
	end;

implementation

uses
	LrWebBrowserUtils;

constructor TLrWebBrowser.Create(AOwner: TComponent);
begin
	inherited;
	OnCommandStateChange := CommandStateChange;
end;

function TLrWebBrowser.GetSource: string;
begin
	Result := LrStringFromBrowser(Self);
end;

procedure TLrWebBrowser.SetSource(const Value: string);
begin
	LrStringToBrowser(Self, Value);
end;

procedure TLrWebBrowser.CanDoStateChange;
begin
	if Assigned(OnCanDoStateChange) then
		OnCanDoStateChange(Self);
end;

procedure TLrWebBrowser.CommandStateChange(Sender: TObject;
	Command: Integer; Enable: WordBool);
begin
	if Cardinal(Command) = CSC_NAVIGATEBACK	then
		CanGoBack := Enable
	else if Cardinal(Command) = CSC_NAVIGATEFORWARD then
		CanGoForward := Enable
	else if Cardinal(Command) = CSC_UPDATECOMMANDS then
		CanStop := Busy;
	CanDoStateChange;
end;

{
procedure TLrWebBrowser.SetParent(AParent: TWinControl);
begin
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
}

	procedure MessagingWait;
	begin
		Sleep(100);
		Forms.Application.ProcessMessages;
	end;

function TLrWebBrowser.Wait: Boolean;
var
	i: Integer;
begin
	try
		for i := 1 to 100 do
			if (ReadyState < READYSTATE_INTERACTIVE) then
				MessagingWait;
		Result := not (ReadyState < READYSTATE_INTERACTIVE);
	except
		Result := false;
	end;
end;

function TLrWebBrowser.SafeStop: Boolean;
var
	i: Integer;
begin
	try
		Result := Wait;
		if Result and Busy then
		begin
			Stop;
			for i := 1 to 100 do
				if Busy then
					MessagingWait;
			Result := not Busy;
		end;
	except
		Result := false;
	end;
end;

procedure TLrWebBrowser.SafeRefresh;
var
	flags: OleVariant;
begin
	if SafeStop then
	begin
		flags := REFRESH_COMPLETELY;
		Refresh2(flags);
	end;
end;

procedure TLrWebBrowser.SafeBack;
begin
	if SafeStop then
		try
			GoBack;
		except
		end;
end;

procedure TLrWebBrowser.SafeNavigate(const inUrl: string);
var
	url: OleVariant;
begin
	if SafeStop then
	try
		url := inUrl;
		Navigate2(url);
	except
	end;
end;

{
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
}

procedure TLrWebBrowser.SetCanGoBack(const Value: Boolean);
begin
	FCanGoBack := Value;
end;

procedure TLrWebBrowser.SetCanStop(const Value: Boolean);
begin
	FCanStop := Value;
end;

procedure TLrWebBrowser.SetCanGoForward(const Value: Boolean);
begin
	FCanGoForward := Value;
end;

end.
