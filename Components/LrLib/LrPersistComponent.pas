unit LrPersistComponent;

interface

uses
	SysUtils, Classes,
	LrStreamable;

type
	TLrPersistComponent = class(TLrStreamable)
	private
		FFilename: string;
	protected
		procedure SetFilename(const Value: string); virtual;
	public
		procedure Load;
		procedure LoadFromFile(const inFilename: string); override;
		procedure SaveToFile(const inFilename: string); override;
		procedure Save;
		property Filename: string read FFilename write SetFilename;
	end;

implementation

{ TLrPersistComponent }

procedure TLrPersistComponent.SetFilename(const Value: string);
begin
	FFilename := Value;
end;

procedure TLrPersistComponent.Load;
begin
	LoadFromFile(Filename);
end;

procedure TLrPersistComponent.Save;
begin
	if Filename <> '' then
		SaveToFile(Filename);
end;

procedure TLrPersistComponent.LoadFromFile(const inFilename: string);
begin
	Filename := inFilename;
	inherited;
end;

procedure TLrPersistComponent.SaveToFile(const inFilename: string);
begin
	Filename := inFilename;
	inherited;
end;

end.
