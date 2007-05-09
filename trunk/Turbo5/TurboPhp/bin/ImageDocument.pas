unit ImageDocument;

interface

uses
	Classes, Graphics,
	LrDocument;

type
	TImageDocument = class(TLrDocument)
	public
		Picture: TPicture;
		constructor Create; override;
		destructor Destroy; override;
		procedure Activate; override;
		procedure Deactivate; override;
		procedure Open(const inFilename: string); override;
	end;

implementation

uses
	ImageView;

{ TImageDocument }

constructor TImageDocument.Create;
begin
	inherited;
	Picture := TPicture.Create;
end;

destructor TImageDocument.Destroy;
begin
	Picture.Free;
	inherited;
end;

procedure TImageDocument.Open(const inFilename: string);
begin
	Filename := inFilename;
	Picture.LoadFromFile(Filename);
end;

procedure TImageDocument.Activate;
begin
	inherited;
	ImageViewForm.Image.Picture := Picture;
	ImageViewForm.BringToFront;
end;

procedure TImageDocument.Deactivate;
begin
	inherited;
	ImageViewForm.Image.Picture := nil;
end;

end.
