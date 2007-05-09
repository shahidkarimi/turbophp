unit ProjectDocument;

interface

uses
	SysUtils, Classes, Contnrs,
	LrDocument,
	Servers, ServerDatabases, TurboPhpDocument;

type
	TProjectItem = class(TComponent)
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
	TProjectItems = class(TComponent)
	protected
		function GetCount: Integer;
		function GetItems(inIndex: Integer): TProjectItem;
		procedure Clear;
		procedure DefineProperties(Filer: TFiler); override;
		procedure LoadFromStream(inStream: TStream);
		procedure ReadItems(Stream: TStream);
		procedure SaveToStream(inStream: TStream);
		procedure WriteItems(Stream: TStream);
	public
		constructor Create(inOwner: TComponent); reintroduce;
		destructor Destroy; override;
		procedure Add(inItem: TProjectItem);
		property Count: Integer read GetCount;
		property Items[inIndex: Integer]: TProjectItem read GetItems; default;
	end;
	//
	TSubitemsItem = class(TProjectItem)
	private
		FItems: TProjectItems;
	protected
		procedure SetItems(const Value: TProjectItems);
	public
		constructor Create; override;
		destructor Destroy; override;
	published
		property Items: TProjectItems read FItems write SetItems;
	end;
	//
	TDocumentItem = class(TProjectItem)
	private
		FDocument: TLrDocument;
	protected
		function GetSource: string; override;
		procedure SetDocument(const Value: TLrDocument);
	public
		property Document: TLrDocument read FDocument write SetDocument;
	end;
	//
	TTurboPhpDocumentItem = class(TDocumentItem)
	end;
	//
	TProjectDocument = class(TLrDocument)
	private
		FServers: TServerProfileMgr;
		FDatabases: TServerDatabaseProfileMgr;
		FItems: TProjectItems;
	protected
		function GetDatabasesFilename: string;
		function GetServersFilename: string;
		function GetUntitledName: string; override;
	public
		constructor Create; override;
		destructor Destroy; override;
		function Find(const inSource: string;
			inItems: TProjectItems = nil): TProjectItem;
		function GetUniqueName(const inName: string): string;
		function NewTurboPhpDocument: TTurboPhpDocument;
		procedure AddFile(const inFilename: string);
		procedure AddItem(inItem: TProjectItem);
		procedure AddTurboPhpDocumentItem(const inFilename: string);
		procedure DocumentOpened(inDocument: TLrDocument);
		procedure DocumentClosed(inDocument: TLrDocument);
		procedure DocumentChange(Sender: TObject);
		procedure Open(const inFilename: string); override;
		procedure Save; override;
		property Databases: TServerDatabaseProfileMgr read FDatabases;
		property Items: TProjectItems read FItems;
		property Servers: TServerProfileMgr read FServers;
	end;

implementation

uses
	LrVclUtils,
	Controller;

{ TProjectItem }

constructor TProjectItem.Create;
begin
	inherited Create(nil);
end;

function TProjectItem.GetDestination: string;
begin
	Result := FDestination;
end;

function TProjectItem.GetDisplayName: string;
begin
	if FDisplayName <> '' then
		Result := FDisplayName
	else begin
		Result := ExtractFileName(Source);
		if Result = '' then
			Result := '(untitled)';
	end;
end;

function TProjectItem.GetSource: string;
begin
	Result := FSource;
end;

procedure TProjectItem.SetDestination(const Value: string);
begin
	FDestination := Value;
end;

procedure TProjectItem.SetDisplayName(const Value: string);
begin
	FDisplayName := Value;
end;

procedure TProjectItem.SetSource(const Value: string);
begin
	FSource := Value;
end;

{ TProjectItems }

constructor TProjectItems.Create(inOwner: TComponent);
begin
	inherited Create(inOwner);
end;

destructor TProjectItems.Destroy;
begin
	inherited;
end;

procedure TProjectItems.Clear;
begin
	DestroyComponents;
end;

procedure TProjectItems.ReadItems(Stream: TStream);
var
	c, i: Integer;
begin
	Stream.Read(c, 4);
	for i := 0 to Pred(c) do
		Add(TProjectItem(Stream.ReadComponent(nil)));
end;

procedure TProjectItems.WriteItems(Stream: TStream);
var
	c, i: Integer;
begin
	c := Count;
	Stream.Write(c, 4);
	for i := 0 to Pred(Count) do
		Stream.WriteComponent(Items[i]);
end;

procedure TProjectItems.DefineProperties(Filer: TFiler);
begin
	Filer.DefineBinaryProperty('Items', ReadItems, WriteItems, true);
end;

procedure TProjectItems.SaveToStream(inStream: TStream);
begin
	LrSaveComponentToStream(Self, inStream);
end;

procedure TProjectItems.LoadFromStream(inStream: TStream);
begin
	Clear;
	LrLoadComponentFromStream(Self, inStream);
end;

function TProjectItems.GetCount: Integer;
begin
	Result := ComponentCount;
end;

function TProjectItems.GetItems(inIndex: Integer): TProjectItem;
begin
	Result := TProjectItem(Components[inIndex]);
end;

procedure TProjectItems.Add(inItem: TProjectItem);
begin
	InsertComponent(inItem);
end;

{ TSubitemsItem }

constructor TSubitemsItem.Create;
begin
	inherited;
	FItems := TProjectItems.Create(Self);
end;

destructor TSubitemsItem.Destroy;
begin
	inherited;
end;

procedure TSubitemsItem.SetItems(const Value: TProjectItems);
begin
	FItems.Assign(Value);
end;

{ TProjectDocument }

constructor TProjectDocument.Create;
begin
	inherited;
	EnableSaveAs('TurboPhp Projects', '.trboprj');
	FDatabases := TServerDatabaseProfileMgr.Create;
	FItems := TProjectItems.Create(nil);
	FServers := TServerProfileMgr.Create;
end;

destructor TProjectDocument.Destroy;
begin
	Servers.Free;
	Items.Free;
	Databases.Free;
	inherited;
end;

function TProjectDocument.GetUntitledName: string;
begin
	Result := 'Untitled Project';
end;

function TProjectDocument.GetDatabasesFilename: string;
begin
	Result := Filename + '.dbs';
end;

function TProjectDocument.GetServersFilename: string;
begin
	Result := Filename + '.servers';
end;

procedure TProjectDocument.Open(const inFilename: string);
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
	Servers.LoadFromFile(GetServersFilename);
	Databases.LoadFromFile(GetDatabasesFilename);
	inherited;
end;

procedure TProjectDocument.Save;
var
	s: TStream;
begin
	s := TFileStream.Create(Filename, fmCreate);
	try
		Items.SaveToStream(s);
	finally
		s.Free;
	end;
	Servers.SaveToFile(GetServersFilename);
	Databases.SaveToFile(GetDatabasesFilename);
	inherited;
end;

function TProjectDocument.Find(const inSource: string;
	inItems: TProjectItems): TProjectItem;
var
	i: Integer;
begin
	if inItems = nil then
		inItems := Items;
	Result := nil;
	for i := 0 to Pred(inItems.Count) do
	begin
		if (inItems[i].Source = inSource) then
			Result := inItems[i]
		else if (inItems[i] is TSubitemsItem) then
			Result := Find(inSource, TSubitemsItem(inItems[i]).Items);
		if Result <> nil then
			break;
	end;
end;

function TProjectDocument.GetUniqueName(const inName: string): string;
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

procedure TProjectDocument.AddItem(inItem: TProjectItem);
begin
	Items.Add(inItem);
	Change;
end;

procedure TProjectDocument.AddTurboPhpDocumentItem(const inFilename: string);
var
	item: TProjectItem;
begin
	item :=	TTurboPhpDocumentItem.Create;
	item.Source := inFilename;
	AddItem(item);
end;

function TProjectDocument.NewTurboPhpDocument: TTurboPhpDocument;
begin
	Result := TTurboPhpDocument.Create;
	Result.Filename := GetUniqueName(Result.DisplayName);
	AddTurboPhpDocumentItem(Result.Filename);
end;

procedure TProjectDocument.AddFile(const inFilename: string);
var
	e: string;
begin
	e := LowerCase(ExtractFileExt(inFilename));
	if (e = cPageExt) then
		AddTurboPhpDocumentItem(inFilename);
end;

procedure TProjectDocument.DocumentOpened(inDocument: TLrDocument);
var
	item: TProjectItem;
begin
	item := Find(inDocument.Filename);
	if item is TDocumentItem then
		TDocumentItem(item).Document := inDocument;
end;

procedure TProjectDocument.DocumentClosed(inDocument: TLrDocument);
var
	item: TProjectItem;
begin
	item := Find(inDocument.Filename);
	if item is TDocumentItem then
		TDocumentItem(item).Document := nil;
end;

{
function TProjectDocument.FindDocumentItem(const inDocument: TLrDocument;
	inItems: TProjectItems): TDocumentItem;
var
	i: Integer;
begin
	if inItems = nil then
		inItems := Items;
	Result := nil;
	for i := 0 to Pred(inItems.Count) do
	begin
		if (inItems[i] is TDocumentItem) then
		begin
			Result := TDocumentItem(inItems[i]);
			if Result.Document <> inDocument then
				Result := nil;
		end
		if (Result = nil) and (inItems[i] is TSubitemsItem) then
			Result := FindDocumentItem(inDocument, TSubitemsItem(inItems[i]).Items);
		if Result <> nil then
			break;
	end;
end;
}

procedure TProjectDocument.DocumentChange(Sender: TObject);
begin
	Change;
end;

{ TDocumentItem }

function TDocumentItem.GetSource: string;
begin
	if FDocument <> nil then
		Source := FDocument.Filename;
	Result := inherited GetSource;
end;

procedure TDocumentItem.SetDocument(const Value: TLrDocument);
begin
	FDocument := Value;
end;

initialization
	RegisterClass(TTurboPhpDocumentItem);
end.
