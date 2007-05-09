unit RawDocumentView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs,
	dxDockPanel, dxDockControl,
	RawDocument, PhpEditView, HtmlEditView, IEView;

type
	TRawDocumentForm = class(TForm)
		ClientDockSite: TdxDockSite;
		dxLayoutDockSite1: TdxLayoutDockSite;
		dxTabContainerDockSite: TdxTabContainerDockSite;
		CodeDock: TdxDockPanel;
		PreviewDock: TdxDockPanel;
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
		FDocument: TRawDocument;
		procedure SetDocument(const Value: TRawDocument);
	public
		{ Public declarations }
		EditForm: TPhpEditForm;
		PreviewForm: TIEForm;
		property Document: TRawDocument read FDocument write SetDocument;
	end;

implementation

uses
	LrUtils;

{$R *.dfm}

{ TRawDocumentForm }

procedure TRawDocumentForm.FormCreate(Sender: TObject);
begin
	dxTabContainerDockSite.ActiveChildIndex := 0;
	AddForm(EditForm, TPhpEditForm, CodeDock);
	AddForm(PreviewForm, TIEForm, PreviewDock);
end;

procedure TRawDocumentForm.SetDocument(const Value: TRawDocument);
begin
	FDocument := Value;
	if Document <> nil then
		EditForm.Source.Strings.Assign(Document.Strings);
end;

end.
