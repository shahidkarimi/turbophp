unit MyDocumentController;

interface

uses
	Classes,
	LrDocument, LrOpenDocumentsController,
	MyDocumentHost;

type
	TMyDocument = class(TLrDocument)
  private
		FText: TStringList;
	protected
		procedure SetText(const Value: TStringList);
	public
		constructor Create; override;
		destructor Destroy; override;
		procedure Load; override;
		procedure Save; override;
		property Text: TStringList read FText write SetText;
	end;
	//
	TMyDocumentController = class(TLrDocumentController)
	public
		class function GetDescription: string; override;
		class function GetExt: string; override;
	public
		function New: TLrDocument; override;
		procedure DocumentActivate(inDocument: TLrDocument); override;
		procedure DocumentDeactivate(inDocument: TLrDocument); override;
		procedure DocumentUpdate(inDocument: TLrDocument); override;
	end;

implementation

{ TMyDocument }

constructor TMyDocument.Create;
begin
	inherited;
	FText := TStringList.Create;
end;

destructor TMyDocument.Destroy;
begin
	FText.Free;
	inherited;
end;

procedure TMyDocument.SetText(const Value: TStringList);
begin
	FText.Assign(Value);
end;

procedure TMyDocument.Save;
begin
	inherited;
	FText.SaveToFile(Filename);
end;

procedure TMyDocument.Load;
begin
	inherited;
	FText.LoadFromFile(Filename);
end;

{ TMyDocumentController }

class function TMyDocumentController.GetDescription: string;
begin
	Result := 'My Document';
end;

class function TMyDocumentController.GetExt: string;
begin
	Result := '.txt';
end;

procedure TMyDocumentController.DocumentUpdate(inDocument: TLrDocument);
begin
	TMyDocument(inDocument).Text.Assign(MyDocumentHostForm.Memo.Lines);
end;

procedure TMyDocumentController.DocumentActivate(inDocument: TLrDocument);
begin
	with MyDocumentHostForm do
	begin
		Memo.Lines.Assign(TMyDocument(inDocument).Text);
		Memo.OnChange := inDocument.DoModified;
		Show;
	end;
end;

procedure TMyDocumentController.DocumentDeactivate(inDocument: TLrDocument);
begin
	with MyDocumentHostForm do
	begin
		Memo.OnChange := nil;
		Hide;
	end;
end;

function TMyDocumentController.New: TLrDocument;
begin
	Result := CreateDocument(TMyDocument);
end;

end.
