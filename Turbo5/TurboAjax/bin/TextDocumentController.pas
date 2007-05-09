unit TextDocumentController;

interface

uses
	LrDocument,
	TextDocument, CodeEdit;

type
	TTextDocumentController = class(TLrDocumentController)
	protected
		function GetEditForm: TCodeEditForm; virtual;
		property EditForm: TCodeEditForm read GetEditForm;
	public
		function New: TLrDocument; override;
		procedure DocumentActivate(inDocument: TLrDocument); override;
		procedure DocumentDeactivate(inDocument: TLrDocument); override;
		procedure DocumentUpdate(inDocument: TLrDocument); override;
	end;

implementation

uses
	CodeExplorer;

function TTextDocumentController.GetEditForm: TCodeEditForm;
begin
	Result := CodeEditForm;
end;

procedure TTextDocumentController.DocumentActivate(inDocument: TLrDocument);
begin
	EditForm.Strings := TTextDocument(inDocument).Text;
	EditForm.OnModified := inDocument.DoModified;
	EditForm.Show;
	CodeExplorerForm.EasyEdit := EditForm.Edit;
end;

procedure	TTextDocumentController.DocumentUpdate(inDocument: TLrDocument);
begin
	TTextDocument(inDocument).Text.Assign(EditForm.Strings);
end;

procedure TTextDocumentController.DocumentDeactivate(inDocument: TLrDocument);
begin
	EditForm.Hide;
	EditForm.OnModified := nil;
	CodeExplorerForm.EasyEdit := nil;
end;

function TTextDocumentController.New: TLrDocument;
begin
	Result := CreateDocument(TTextDocument);
end;

end.
