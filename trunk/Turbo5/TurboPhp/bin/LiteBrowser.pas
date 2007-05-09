unit LiteBrowser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HTMLLite;

type
  TLiteBrowserForm = class(TForm)
    htmlLite1: ThtmlLite;
  private
    procedure SetHtml(const Value: TStrings);
    { Private declarations }
  public
		{ Public declarations }
		property Html: TStrings write SetHtml;
  end;

var
  LiteBrowserForm: TLiteBrowserForm;

implementation

{$R *.dfm}

{ TLiteBrowserForm }

procedure TLiteBrowserForm.SetHtml(const Value: TStrings);
begin
	htmlLite1.LoadTextStrings(Value);
end;

end.
