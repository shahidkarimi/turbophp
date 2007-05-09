unit RawDocument;

interface

uses
	Classes, Forms, Types,
	LrDocument;

type
	TRawDocument = class(TLrDocument)
	private
		FStrings: TStringList;
    FEditPos: TPoint;
	protected
		function GetExtension: string; override;
		function GetFilter: string; override;
		procedure SetStrings(const Value: TStringList);
	public
		constructor Create(inManager: TLrDocumentManager;
			const inPath: string = ''); override;
		destructor Destroy; override;
		procedure Open; override;
		procedure PerformAction(inAction: TLrDocumentAction); override;
		procedure Save; override;
		property EditPos: TPoint read FEditPos write FEditPos;
		property Strings: TStringList read FStrings write SetStrings;
	end;

implementation

{ TRawDocument }

constructor TRawDocument.Create(inManager: TLrDocumentManager;
	const inPath: string);
begin
	inherited;
	FStrings := TStringList.Create;
end;

destructor TRawDocument.Destroy;
begin
	FStrings.Free;
	inherited;
end;

procedure TRawDocument.SetStrings(const Value: TStringList);
begin
	FStrings.Assign(Value);
end;

function TRawDocument.GetExtension: string;
begin
	Result := '.php';
end;

function TRawDocument.GetFilter: string;
begin
	Result := 'Php Files (*.php)|*.php|Any File (*.*)|*.*';
end;

procedure TRawDocument.Open;
begin
	if not Untitled then
		Strings.LoadFromFile(Path);
end;

procedure TRawDocument.Save;
begin
	inherited;
	Strings.SaveToFile(Path);
end;

procedure TRawDocument.PerformAction(inAction: TLrDocumentAction);
begin
	inherited;
	if inAction.Name = 'PublishAction' then
	begin
	end;
end;

end.
