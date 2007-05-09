unit JavaScriptDocumentHost;

interface

uses
	Forms,
	CodeEdit;

type
	TJavaScriptDocumentHostForm = class(TCodeEditForm)
	public
		procedure AfterConstruction; override;
	end;

var
	JavaScriptDocumentHostForm: TJavaScriptDocumentHostForm;

implementation

{ TJavaScriptDocumentHostForm }

procedure TJavaScriptDocumentHostForm.AfterConstruction;
begin
	inherited;
	Source.Parser := JsParser;
end;

end.
