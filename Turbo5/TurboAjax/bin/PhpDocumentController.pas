unit PhpDocumentController;

interface

uses
	LrDocument,
	CodeDocumentController, CodeEdit;

type
	TPhpDocumentController = class(TCodeDocumentController)
	public
		class function GetDescription: string; override;
		class function GetExt: string; override;
	protected
		function GetEditForm: TCodeEditForm; override;
	end;

implementation

uses
	PhpDocumentHost;

const
	cDescription = 'PHP Document';
	cExt = '.php';

class function TPhpDocumentController.GetDescription: string;
begin
	Result := cDescription;
end;

class function TPhpDocumentController.GetExt: string;
begin
	Result := cExt;
end;

function TPhpDocumentController.GetEditForm: TCodeEditForm;
begin
	Result := PhpDocumentHostForm;
end;

end.
