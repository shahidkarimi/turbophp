unit LrWebBrowserUtils;

interface

uses
	Classes, ShDocVw;

function LrStringFromBrowser(inWebBrowser: TWebBrowser): string;
procedure LrStringToBrowser(inWebBrowser: TWebBrowser; inHtml: string);

implementation

uses
	Forms, ActiveX, Variants;

procedure LrStringToBrowser(inWebBrowser: TWebBrowser; inHtml: string);
var
	sl: TStringList;
	ms: TMemoryStream;
begin
	inWebBrowser.Navigate('about:blank');
	while inWebBrowser.ReadyState < READYSTATE_INTERACTIVE do
		Application.ProcessMessages;
	if Assigned(inWebBrowser.Document) then
	begin
		sl := TStringList.Create;
		try
			ms := TMemoryStream.Create;
			try
				sl.Text := inHtml;
				sl.SaveToStream(ms);
				ms.Seek(0, 0);
				with inWebBrowser.Document as IPersistStreamInit do
					Load(TStreamAdapter.Create(ms));
			finally
				ms.Free;
			end;
		finally
			sl.Free;
		end;
	end;
end;

function LrStringFromBrowser(inWebBrowser: TWebBrowser): string;
var
	sl: TStringList;
	ms: TMemoryStream;
begin
	Result := '';
	try
		if inWebBrowser.ReadyState <> READYSTATE_COMPLETE then
			exit;
//		if inWebBrowser.Document = nil then
//			exit;
//		while inWebBrowser.ReadyState < READYSTATE_INTERACTIVE do
//			Application.ProcessMessages;
		if Assigned(inWebBrowser.Document) then
		begin
			sl := TStringList.Create;
			try
				ms := TMemoryStream.Create;
				try
					with inWebBrowser.Document as IPersistStreamInit do
						Save(TStreamAdapter.Create(ms), true);
					ms.Position := 0;
					sl.LoadFromStream(ms);
				finally
					ms.Free;
				end;
			finally
				Result := sl.Text;
				sl.Free;
			end;
		end;
	except
	end;
end;

end.
