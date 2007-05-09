unit ThStructuredDocument;

interface

uses
	Classes, ComCtrls, Graphics;

type
	TThStructuredDocument = class(TStringList)
	protected
		function GetSection(const inName: string): TThStructuredDocument;
		function GetSubDocument(inIndex: Integer): TThStructuredDocument;
	protected
		function ShouldPublish(const inSection: string): Boolean; virtual;
	public
		destructor Destroy; override;
		procedure Clear; override;
		function PublishToString: string;
		procedure PublishToStrings(inStrings: TStrings); virtual;
		procedure RichPublish(inRichEdit: TRichEdit;
			inLevel: Integer = 0); virtual;
	public
		property SubDocument[inIndex: Integer]: TThStructuredDocument
			read GetSubDocument;
		property Section[const inName: string]: TThStructuredDocument
			read GetSection; default;
	end;

implementation

{ TThStructuredDocument }

destructor TThStructuredDocument.Destroy;
begin
	Clear;
	inherited;
end;

function TThStructuredDocument.GetSubDocument(
	inIndex: Integer): TThStructuredDocument;
begin
	Result := TThStructuredDocument(Objects[inIndex]);
end;

procedure TThStructuredDocument.Clear;
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		if Objects[i] <> nil then
			SubDocument[i].Free;
	inherited;
end;

function TThStructuredDocument.GetSection(
	const inName: string): TThStructuredDocument;
var
	i: Integer;
begin
	i := IndexOf(inName);
	if (i < 0) then
		i := AddObject(inName, TThStructuredDocument.Create);
	Result := SubDocument[i];
end;

function TThStructuredDocument.ShouldPublish(const inSection: string): Boolean;
begin
	Result := true;
end;

procedure TThStructuredDocument.PublishToStrings(inStrings: TStrings);
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
	begin
		if Objects[i] = nil then
			inStrings.Add(Strings[i])
		else begin
			if ShouldPublish(Strings[i]) then
				SubDocument[i].PublishToStrings(inStrings);
		end;
	end;
end;

procedure TThStructuredDocument.RichPublish(inRichEdit: TRichEdit;
	inLevel: Integer);
const
	cN = 5;
	cColors: array[0..Pred(cN)] of TColor =
		( clBlack, clNavy, clMaroon, clFuchsia, clGreen );
var
	i: Integer;

	procedure RichEmit;
	begin
		with inRichEdit do
		begin
			SelStart := MAXINT;
			SelAttributes.Color := cColors[inLevel mod cN];
			//SelAttributes.Style := inStyle;
			SelText := StringOfChar(' ', inLevel * 2) + Strings[i] + #13#10;
		end;
	end;

begin
	for i := 0 to Pred(Count) do
		if Objects[i] = nil then
			RichEmit
		else begin
			if ShouldPublish(Strings[i]) then
				SubDocument[i].RichPublish(inRichEdit, inLevel+1);
		end;
end;

function TThStructuredDocument.PublishToString: string;
var
	s: TStringList;
begin
	s := TStringList.Create;
	try
		PublishToStrings(s);
		Result := s.Text;
	finally
		s.Free;
	end;
end;

end.
