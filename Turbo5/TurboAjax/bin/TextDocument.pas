unit TextDocument;

interface

uses
	Classes,
	LrDocument;

type
	TTextDocument = class(TLrDocument)
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

implementation

{ TTextDocument }

constructor TTextDocument.Create;
begin
	inherited;
	FText := TStringList.Create;
end;

destructor TTextDocument.Destroy;
begin
	FText.Free;
	inherited;
end;

procedure TTextDocument.SetText(const Value: TStringList);
begin
	FText.Assign(Value);
end;

procedure TTextDocument.Save;
begin
	inherited;
	FText.SaveToFile(Filename);
end;

procedure TTextDocument.Load;
begin
	inherited;
	FText.LoadFromFile(Filename);
end;

end.
