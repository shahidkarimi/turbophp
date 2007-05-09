unit AboutView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, GIFImage, StdCtrls, JvGIF;

type
  TAboutForm = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    Image2: TImage;
    OkButton: TButton;
    Label1: TLabel;
		Label2: TLabel;
		Bevel1: TBevel;
		procedure DismissClick(Sender: TObject);
		procedure WebClick(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
	AboutForm: TAboutForm;

implementation

uses
	ShellApi;

{$R *.dfm}

function GetFileVersion: string;
{
const
	 InfoNum = 11;
	 InfoStr : array [1..InfoNum] of String =
		 ('CompanyName', 'FileDescription', 'FileVersion',
'InternalName', 'LegalCopyright', 'LegalTradeMarks',
'OriginalFilename', 'ProductName', 'ProductVersion',
'Comments', 'Author') ;
}
var
	s: string;
	n, len: Cardinal;
	buf: PChar;
	value: PChar;
begin
	Result := '';
	try
		s := Application.ExeName;
		n := GetFileVersionInfoSize(PChar(s), n);
		if n > 0 then
		begin
			buf := AllocMem(n);
			GetFileVersionInfo(PChar(s), 0, n, buf);
			if VerQueryValue(buf, PChar('StringFileInfo\040904E4\FileVersion'), Pointer(value), len) then
			begin
				value := PChar(Trim(value));
				if Length(value) > 0 then
					Result := value;
			end;
			FreeMem(buf, n);
		end
	except
	end;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
	Label2.Caption := GetFileVersion;
end;

procedure TAboutForm.DismissClick(Sender: TObject);
begin
	Close;
end;

procedure TAboutForm.WebClick(Sender: TObject);
begin
	ShellExecute(Handle, 'open', 'http://www.turbophp.com', '', '', 0);
end;

procedure TAboutForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Action := caFree;
end;

end.
