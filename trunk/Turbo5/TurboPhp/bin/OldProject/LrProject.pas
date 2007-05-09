unit LrProject;

interface

uses
	SysUtils, Classes, Contnrs,
	LrDocument;

type
	TLrProjectItem = class(TComponent)
	private
		FSource: string;
		FDestination: string;
		FDisplayName: string;
	protected
		function GetDestination: string; virtual;
		function GetDisplayName: string; virtual;
		function GetSource: string; virtual;
		procedure SetDestination(const Value: string); virtual;
		procedure SetDisplayName(const Value: string);
		procedure SetSource(const Value: string); virtual;
	public
		constructor Create; reintroduce; virtual;
	published
		property Destination: string read GetDestination write SetDestination;
		property DisplayName: string read GetDisplayName write SetDisplayName;
		property Source: string read GetSource write SetSource;
	end;
	//
	TLrProjectItems = class(TComponent)
	protected
		function GetCount: Integer;
		function GetItems(inIndex: Integer): TLrProjectItem;
		procedure Clear;
		procedure DefineProperties(Filer: TFiler); override;
		procedure LoadFromStream(inStream: TStream);
		procedure ReadItems(Stream: TStream);
		procedure SaveToStream(inStream: TStream);
		procedure WriteItems(Stream: TStream);
	public
		constructor Create(inOwner: TComponent); reintroduce;
		destructor Destroy; override;
		function Find(const inSource: string): TLrProjectItem;
		procedure Add(inItem: TLrProjectItem);
		property Count: Integer read GetCount;
		property Items[inIndex: Integer]: TLrProjectItem read GetItems; default;
	end;
	//
	TLrProjectDocument = class(TLrDocument)
	private
		FItems: TLrProjectItems;
	protected
		function GetUntitledName: string; override;
	public
		constructor Create; override;
		destructor Destroy; override;
		function Find(const inSource: string): TLrProjectItem;
		function GetUniqueName(const inName: string): string;
		procedure AddItem(inItem: TLrProjectItem);
		//procedure DocumentOpened(inDocument: TLrDocument);
		//procedure DocumentClosed(inDocument: TLrDocument);
		//procedure DocumentChange(Sender: TObject);
		procedure Open(const inFilename: string); override;
		procedure Save; override;
		property Items: TLrProjectItems read FItems;
	end;

implementation

uses
	LrVclUtils;

{ TLrProjectItem }

constructor TLrProjectItem.Create;
begin
	inherited Create(nil);
end;

function TLrProjectItem.GetDestination: string;
begin
	Result := FDestination;
end;

function TLrProjectItem.GetDisplayName: string;
begin
	if FDisplayName <> '' then
		Result := FDisplayName
	else begin
		Result := ExtractFileName(Source);
		if Result = '' then
			Result := '(untitled)';
	end;
end;

function TLrProjectItem.GetSource: string;
begin
	Result := FSource;
end;

procedure TLrProjectItem.SetDestination(const Value: string);
begin
	FDestination := Value;
end;

procedure TLrProjectItem.SetDisplayName(const Value: string);
begin
	FDisplayName := Value;
end;

procedure TLrProjectItem.SetSource(const Value: string);
begin
	FSource := Value;
end;

{ TLrProjectItems }

constructor TLrProjectItems.Create(inOwner: TComponent);
begin
	inherited Create(inOwner);
end;

destructor TLrProjectItems.Destroy;
begin
	inherited;
end;

procedure TLrProjectItems.Clear;
begin
	DestroyComponents;
end;

procedure TLrProjectItems.ReadItems(Stream: TStream);
var
	c, i: Integer;
begin
	Stream.Read(c, 4);
	for i := 0 to Pred(c) do
		Add(TLrProjectItem(Stream.ReadComponent(nil)));
end;

procedure TLrProjectItems.WriteItems(Stream: TStream);
var
	c, i: Integer;
begin
	c := Count;
	Stream.Write(c, 4);
	for i := 0 to Pred(Count) do
		Stream.WriteComponent(Items[i]);
end;

procedure TLrProjectItems.DefineProperties(Filer: TFiler);
begin
	Filer.DefineBinaryProperty('Items', ReadItems, WriteItems, true);
end;

procedure TLrProjectItems.SaveToStream(inStream: TStream);
begin
	LrSaveComponentToStream(Self, inStream);
end;

procedure TLrProjectItems.LoadFromStream(inStream: TStream);
begin
	Clear;
	LrLoadComponentFromStream(Self, inStream);
end;

function TLrProjectItems.GetCount: Integer;
begin
	Result := ComponentCount;
end;

function TLrProjectItems.GetItems(inIndex: Integer): TLrProjectItem;
begin
	Result := TLrProjectItem(Components[inIndex]);
end;

procedure TLrProjectItems.Add(inItem: TLrProjectItem);
begin
	InsertComponent(inItem);
end;

function TLrProjectItems.Find(const inSource: string): TLrProjectItem;
var
	i: Integer;
begin
	Result := nil;
	for i := 0 to Pred(Count) do
		if (Items[i].Source = inSource) then
		begin
			Result := Items[i];
			break;
		end;
end;

{ TLrProjectDocument }

constructor TLrProjectDocument.Create;
begin
	inherited;
	//EnableSaveAs('TurboPhp Projects', '.trboprj');
	FItems := TLrProjectItems.Create(nil);
end;

destructor TLrProjectDocument.Destroy;
begin
	Items.Free;
	inherited;
end;

function TLrProjectDocument.GetUntitledName: string;
begin
	Result := 'Untitled Project';
end;

procedure TLrProjectDocument.Open(const inFilename: string);
var
	s: TStream;
begin
	Filename := inFilename;
	s := TFileStream.Create(Filename, fmOpenRead);
	try
		Items.LoadFromStream(s);
	finally
		s.Free;
	end;
	inherited;
end;

procedure TLrProjectDocument.Save;
var
	s: TStream;
begin
	s := TFileStream.Create(Filename, fmCreate);
	try
		Items.SaveToStream(s);
	finally
		s.Free;
	end;
	inherited;
end;

function TLrProjectDocument.Find(const inSource: string): TLrProjectItem;
begin
	Result := Items.Find(inSource);
end;

function TLrProjectDocument.GetUniqueName(const inName: string): string;
var
	i: Integer;
begin
	if Find(inName) = nil then
		Result := inName
	else begin
		i := 0;
		repeat
			Inc(i);
			Result := inName + IntToStr(i);
		until Find(Result) = nil;
	end;
end;

procedure TLrProjectDocument.AddItem(inItem: TLrProjectItem);
begin
	Items.Add(inItem);
	Change;
end;

{
procedure TLrProjectDocument.AddFile(const inFilename: string);
var
	e: string;
begin
	e := LowerCase(ExtractFileExt(inFilename));
	if (e = cPageExt) then
		AddTurboPhpDocumentItem(inFilename);
end;

procedure TLrProjectDocument.DocumentOpened(inDocument: TLrDocument);
var
	item: TLrProjectItem;
begin
	item := Find(inDocument.Filename);
	if item is TDocumentItem then
		TDocumentItem(item).Document := inDocument;
end;

procedure TLrProjectDocument.DocumentClosed(inDocument: TLrDocument);
var
	item: TLrProjectItem;
begin
	item := Find(inDocument.Filename);
	if item is TDocumentItem then
		TDocumentItem(item).Document := nil;
end;

procedure TLrProjectDocument.DocumentChange(Sender: TObject);
begin
	Change;
end;
}

end.
