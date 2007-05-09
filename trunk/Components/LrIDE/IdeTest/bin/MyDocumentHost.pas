unit MyDocumentHost;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls, StdCtrls;

type
	TMyDocumentHostForm = class(TForm)
    Memo: TMemo;
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
	MyDocumentHostForm: TMyDocumentHostForm;

implementation

{$R *.dfm}

procedure TMyDocumentHostForm.FormCreate(Sender: TObject);
begin
	//
end;

end.
