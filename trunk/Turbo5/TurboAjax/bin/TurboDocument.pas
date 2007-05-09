unit TurboDocument;

interface

uses
	LrDocument, Design;

type
	TTurboDocument = class(TLrDocument)
	private
		FDesign: TDesignForm;
	public
		constructor Create; override;
		destructor Destroy; override;
		procedure Open(const inFilename: string); override;
		procedure Save; override;
		property Design: TDesignForm read FDesign;
	end;

implementation

{ TTurboDocument }

constructor TTurboDocument.Create;
begin
	inherited;
	FDesign := TDesignForm.Create(nil);
end;

destructor TTurboDocument.Destroy;
begin
	Design.Free;
	inherited;
end;

procedure TTurboDocument.Save;
begin
	Design.SaveToFile(Filename);
	inherited;
end;

procedure TTurboDocument.Open(const inFilename: string);
begin
	Filename := inFilename;
	Design.LoadFromFile(Filename);
	inherited;
end;

end.
