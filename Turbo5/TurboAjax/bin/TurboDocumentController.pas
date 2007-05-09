unit TurboDocumentController;

interface

uses
	LrDocument, 
	TurboDocument, TurboDocumentHost;

type
	TTurboDocumentController = class(TLrDocumentController)
	public
		class function GetDescription: string; override;
		class function GetExt: string; override;
	public
		function New: TLrDocument; override;
		procedure DocumentActivate(inDocument: TLrDocument); override;
		procedure DocumentDeactivate(inDocument: TLrDocument); override;
	end;

implementation

const
	cDescription = 'TurboIDE Document';
	cExt = '.tdo';

class function TTurboDocumentController.GetDescription: string;
begin
	Result := cDescription;
end;

class function TTurboDocumentController.GetExt: string;
begin
	Result := cExt;
end;

procedure TTurboDocumentController.DocumentActivate(inDocument: TLrDocument);
begin
	TurboDocumentHostForm.Document := TTurboDocument(inDocument);
end;

procedure TTurboDocumentController.DocumentDeactivate(inDocument: TLrDocument);
begin
	TurboDocumentHostForm.Document := nil;
end;

function TTurboDocumentController.New: TLrDocument;
begin
	Result := CreateDocument(TTurboDocument);
end;

end.
