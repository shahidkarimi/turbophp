unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TForm3 = class(TForm)
    Memo1: TMemo;
    RichEdit1: TRichEdit;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
	ThStructuredHtml;

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
begin
	with TThStructuredHtml.Create do
	try
		with Section[shHead][shTitle] do
			Add('A Fresh Title');
		with Section[shHead][shStyles] do
			Add('body { margin: 0px; }');
		with Section[shJsLibs] do
			Add('TpLib.js');
		//
		with Section[shBody]['MyPanel'] do
		begin
			Add('<table border="0">');
			with Section['Rows'] do
			begin
				Add('<tr>');
				with Section['Cells'] do
					Add('<td>Single-celled organism</td>');
				Add('</tr>');
			end;
			Add('</table');
		end;
		//
		PublishToStrings(Memo1.Lines);
		//RichEdit1.Lines.Add('');
		RichPublish(RichEdit1);
	finally
		Free;
	end;
end;

end.
