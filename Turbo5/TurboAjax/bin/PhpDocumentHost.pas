unit PhpDocumentHost;

interface

uses
	Forms,
	CodeEdit;

type
	TPhpDocumentHostForm = class(TCodeEditForm)
	public
		procedure AfterConstruction; override;
	end;

var
	PhpDocumentHostForm: TPhpDocumentHostForm;

implementation

{ TPhpDocumentHostForm }

procedure TPhpDocumentHostForm.AfterConstruction;
begin
	inherited;
	Source.Parser := PhpParser;
end;

end.
