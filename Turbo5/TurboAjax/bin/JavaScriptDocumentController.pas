unit JavaScriptDocumentController;

interface

uses
	LrDocument,
	CodeDocumentController, CodeEdit;

type
	TJavaScriptDocumentController = class(TCodeDocumentController)
	public
		class function GetDescription: string; override;
		class function GetExt: string; override;
	protected
		function GetEditForm: TCodeEditForm; override;
	end;

implementation

uses
	JavaScriptDocumentHost;

const
	cDescription = 'JavaScript Document';
	cExt = '.js';

class function TJavaScriptDocumentController.GetDescription: string;
begin
	Result := cDescription;
end;

class function TJavaScriptDocumentController.GetExt: string;
begin
	Result := cExt;
end;

function TJavaScriptDocumentController.GetEditForm: TCodeEditForm;
begin
	Result := JavaScriptDocumentHostForm;
end;

end.
