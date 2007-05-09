unit LrDocumentList;

interface

uses
	Classes, Controls,
	LrObserverList, LrDocument;

type
	TLrDocumentList = class(TList)
	private
		FOnChange: TNotifyEvent;
	protected
		function GetDocuments(inIndex: Integer): TLrDocument;
		function GoodIndex(inIndex: Integer): Boolean;
		procedure SetDocuments(inIndex: Integer; const Value: TLrDocument);
	public
		constructor Create; virtual;
		destructor Destroy; override;
		function FindDocument(const inDocument: TLrDocument): Integer; overload;
		function FindDocument(const inFilename: string): TLrDocument; overload;
		procedure Change; virtual;
		property Documents[inIndex: Integer]: TLrDocument read GetDocuments
			write SetDocuments; default;
		property OnChange: TNotifyEvent read FOnChange
			write FOnChange;
	end;

implementation

{ TLrDocumentList }

constructor TLrDocumentList.Create;
begin
end;

destructor TLrDocumentList.Destroy;
begin
	inherited;
end;

procedure TLrDocumentList.Change;
begin
	if Assigned(OnChange) then
		OnChange(Self);
end;

function TLrDocumentList.GoodIndex(inIndex: Integer): Boolean;
begin
	Result := (inIndex >= 0) and (inIndex < Count);
end;

function TLrDocumentList.GetDocuments(inIndex: Integer): TLrDocument;
begin
	Result := TLrDocument(Items[inIndex]);
end;

procedure TLrDocumentList.SetDocuments(inIndex: Integer;
	const Value: TLrDocument);
begin
	Items[inIndex] := Value;
end;

function TLrDocumentList.FindDocument(
	const inDocument: TLrDocument): Integer;
begin
	Result := IndexOf(inDocument);
end;

function TLrDocumentList.FindDocument(
	const inFilename: string): TLrDocument;
var
	i: Integer;
begin
	Result := nil;
	for i := 0 to Pred(Count) do
		if Documents[i].Filename = inFilename then
		begin
			Result := Documents[i];
			break;
		end;
end;

end.
