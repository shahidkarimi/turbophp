unit PhpDocument;

interface

uses
	Classes, LrDocument;

type
	TPhpDocument = class(TLrDocument)
	protected
		procedure LazyUpdate;
	public
		Php: TStringList;
		constructor Create; override;
		destructor Destroy; override;
		procedure Activate; override;
		procedure Deactivate; override;
		procedure Open(const inFilename: string); override;
		procedure Save; override;
	end;

implementation

uses
	PhpEdit;

{ TPhpDocument }

constructor TPhpDocument.Create;
begin
	inherited;
	EnableSaveAs('PHP Files', '.php');
	Php := TStringList.Create;
end;

destructor TPhpDocument.Destroy;
begin
	Php.Free;
	inherited;
end;

procedure TPhpDocument.Open(const inFilename: string);
begin
	Filename := inFilename;
	Php.LoadFromFile(Filename);
end;

procedure TPhpDocument.Save;
begin
	if Active then
		LazyUpdate;
	Php.SaveToFile(Filename);
end;

procedure TPhpDocument.LazyUpdate;
begin
	inherited;
	Php.Assign(PhpEditForm.Strings);
end;

procedure TPhpDocument.Activate;
begin
	inherited;
	PhpEditForm.Strings := Php;
	PhpEditForm.BringToFront;
	PhpEditForm.OnModified := ModificationEvent;
end;

procedure TPhpDocument.Deactivate;
begin
	inherited;
	PhpEditForm.OnModified := nil;
	LazyUpdate;
end;

end.
