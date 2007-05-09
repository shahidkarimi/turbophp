unit LrTreeData;

interface

uses
	SysUtils, Classes, Contnrs,
	LrDocument;

type
	TLrLeafItem = class(TComponent)
	private
		FDisplayName: string;
	protected
		function GetDisplayName: string; virtual;
		procedure SetDisplayName(const Value: string); virtual;
	public
		constructor Create; reintroduce; virtual;
	published
		property DisplayName: string read GetDisplayName write SetDisplayName;
	end;
	//
	TLrDataItems = class(TComponent)
	protected
		function GetCount: Integer;
		function GetItems(inIndex: Integer): TLrDataItem;
		procedure Clear;
		procedure DefineProperties(Filer: TFiler); override;
		procedure ReadItems(Stream: TStream);
		procedure WriteItems(Stream: TStream);
	public
		constructor Create(inOwner: TComponent); reintroduce;
		destructor Destroy; override;
		function Find(const inName: string): TLrDataItem;
		function GetUniqueName(const inName: string): string;
		procedure Add(inItem: TLrDataItem);
		procedure LoadFromStream(inStream: TStream);
		procedure SaveToStream(inStream: TStream);
		//procedure LoadFromFile(const inFilename: string);
		//procedure SaveToFile(const inFilename: string);
		property Count: Integer read GetCount;
		property Items[inIndex: Integer]: TLrDataItem read GetItems; default;
	end;
	//
	TLrDataItem = class(TLeafItem)
	private
		FItems: TLrDataItems;
	protected
		procedure SetItems(const Value: TLrDataItems);
	public
		constructor Create; override;
		destructor Destroy; override;
	published
		property Items: TLrDataItems read FItems write SetItems;
	end;
	//
	TLrDataItem = class(TComponent)
	private
		FDisplayName: string;
	protected
		function GetDisplayName: string; virtual;
		procedure SetDisplayName(const Value: string); virtual;
	public
		constructor Create; reintroduce; virtual;
	published
		property DisplayName: string read GetDisplayName write SetDisplayName;
	end;
	//
	TLrTree = class(TLrDataItem)
	end;

implementation

uses
	LrVclUtils;

{ TLrDataItem }

constructor TLrDataItem.Create;
begin
	inherited Create(nil);
end;

function TLrDataItem.GetDestination: string;
begin
	Result := FDestination;
end;

function TLrDataItem.GetDisplayName: string;
begin
	if FDisplayName <> '' then
		Result := FDisplayName
	else begin
		Result := ExtractFileName(Source);
		if Result = '' then
			Result := '(untitled)';
	end;
end;

function TLrDataItem.GetSource: string;
begin
	Result := FSource;
end;

procedure TLrDataItem.SetDestination(const Value: string);
begin
	FDestination := Value;
end;

procedure TLrDataItem.SetDisplayName(const Value: string);
begin
	FDisplayName := Value;
end;

procedure TLrDataItem.SetSource(const Value: string);
begin
	FSource := Value;
end;

{ TLrDataItems }

constructor TLrDataItems.Create(inOwner: TComponent);
begin
	inherited Create(inOwner);
end;

destructor TLrDataItems.Destroy;
begin
	inherited;
end;

procedure TLrDataItems.Clear;
begin
	DestroyComponents;
end;

procedure TLrDataItems.ReadItems(Stream: TStream);
var
	c, i: Integer;
begin
	Stream.Read(c, 4);
	for i := 0 to Pred(c) do
		Add(TLrDataItem(Stream.ReadComponent(nil)));
end;

procedure TLrDataItems.WriteItems(Stream: TStream);
var
	c, i: Integer;
begin
	c := Count;
	Stream.Write(c, 4);
	for i := 0 to Pred(Count) do
		Stream.WriteComponent(Items[i]);
end;

procedure TLrDataItems.DefineProperties(Filer: TFiler);
begin
	Filer.DefineBinaryProperty('Items', ReadItems, WriteItems, true);
end;

procedure TLrDataItems.SaveToStream(inStream: TStream);
begin
	LrSaveComponentToStream(Self, inStream);
end;

procedure TLrDataItems.LoadFromStream(inStream: TStream);
begin
	Clear;
	LrLoadComponentFromStream(Self, inStream);
end;

function TLrDataItems.GetCount: Integer;
begin
	Result := ComponentCount;
end;

function TLrDataItems.GetItems(inIndex: Integer): TLrDataItem;
begin
	Result := TLrDataItem(Components[inIndex]);
end;

procedure TLrDataItems.Add(inItem: TLrDataItem);
begin
	InsertComponent(inItem);
end;

function TLrDataItems.Find(const inName: string): TLrDataItem;
//var
//	i: Integer;
begin
	Result := TLrDataItem(FindComponent(inName));
{
	Result := nil;
	for i := 0 to Pred(Count) do
		if (Items[i].Name = inName) then
		begin
			Result := Items[i];
			break;
		end;
}
end;

function TLrDataItems.GetUniqueName(const inName: string): string;
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

{ TLrTree }

constructor TLrTree.Create;
begin
	inherited;
	//EnableSaveAs('TurboPhp Projects', '.trboprj');
	FItems := TLrDataItems.Create(nil);
end;

destructor TLrTree.Destroy;
begin
	Items.Free;
	inherited;
end;

function TLrTree.GetUntitledName: string;
begin
	Result := 'Untitled Project';
end;

function TLrTree.Find(const inSource: string): TLrDataItem;
begin
	Result := Items.Find(inSource);
end;

function TLrTree.GetUniqueName(const inName: string): string;
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

procedure TLrTree.AddItem(inItem: TLrDataItem);
begin
	Items.Add(inItem);
	Change;
end;

end.
