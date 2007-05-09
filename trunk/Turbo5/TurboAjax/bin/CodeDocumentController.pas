unit CodeDocumentController;

interface

uses
	LrDocument,
	TextDocumentController;

type
	TCodeDocumentController = class(TTextDocumentController)
	public
		procedure DocumentActivate(inDocument: TLrDocument); override;
		procedure DocumentDeactivate(inDocument: TLrDocument); override;
	end;

implementation

uses
	CodeEdit, CodeExplorer;

{ TCodeDocumentController }

procedure TCodeDocumentController.DocumentActivate(inDocument: TLrDocument);
begin
	inherited;
	CodeExplorerForm.EasyEdit := EditForm.Edit;
end;

procedure TCodeDocumentController.DocumentDeactivate(inDocument: TLrDocument);
begin
	inherited;
	CodeExplorerForm.EasyEdit := nil;
end;

end.
