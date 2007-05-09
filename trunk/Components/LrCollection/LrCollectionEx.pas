unit LrCollectionEx;

interface

uses
	SysUtils, Classes, Contnrs;

// Attempt to extend TLrCollection to use TPersistent items instead of
// TComponent items.
// The problem is that TReader has no mechanism to determine the class
// of the TPersistent property about to be read. TReader requires an
// instance of the object to load.
// TLrCollectionEx would need to store some kind of class info along with
// the actual object. I'm too lazy to do that now.

type
	TLrCustomReader = class(TReader)
	end;
	//
	TLrCustomWriter = class(TWriter)
	end;
	//
	TLrCustomCollectionEx = class(TPersistent)
	private
		FEditor: Pointer;
		FItems: TObjectList;
		FName: string;
		FOwner: TComponent;
	protected
		function FindPersistent(const inName: string): TPersistent;
		function GetCount: Integer;
		function GetItemClass(inIndex: Integer): TPersistentClass; virtual;
		function GetItems(inIndex: Integer): TPersistent;
		function GetOwner: TPersistent; override;
		function GetTypeCount: Integer; virtual;
		function GetTypeDisplayName(inIndex: Integer): string; virtual;
		function UniqueName: string;
		procedure DefineProperties(Filer: TFiler); override;
		procedure Initialize(inPersistent: TPersistent); virtual;
		procedure ReadItemsProperty(Stream: TStream); //Reader: TReader);
		procedure RegisterClasses; virtual;
		procedure SetEditor(const Value: Pointer);
		procedure SetItems(inIndex: Integer; const Value: TPersistent);
		procedure SetName(const Value: string);
		procedure WriteItemsProperty(Stream: TStream); //Writer: TWriter);
	public
		constructor Create(inOwner: TComponent); virtual;
		destructor Destroy; override;
		function Add(inIndex: Integer): TPersistent; overload; virtual;
		function IndexOf(inPersistent: TPersistent): Integer;
		procedure Add(inPersistent: TPersistent); overload;
		procedure Clear; virtual;
		procedure Delete(inIndex: Integer);
		property Count: Integer read GetCount;
		property Editor: Pointer read FEditor write SetEditor;
		property Name: string read FName write SetName;
		property ItemClass[inIndex: Integer]: TPersistentClass read GetItemClass;
		property Items[inIndex: Integer]: TPersistent read GetItems
			write SetItems; default;
		property Owner: TComponent read FOwner write FOwner;
		property TypeCount: Integer read GetTypeCount;
		property TypeDisplayName[inIndex: Integer]: string read GetTypeDisplayName;
	end;

implementation

{ TLrCustomCollectionEx }

constructor TLrCustomCollectionEx.Create(inOwner: TComponent);
begin
	inherited Create;
	FOwner := inOwner;
	FItems := TObjectList.Create;
	FName := 'Items';
end;

destructor TLrCustomCollectionEx.Destroy;
begin
	FItems.Free;
	inherited;
end;

function TLrCustomCollectionEx.GetOwner: TPersistent;
begin
	Result := FOwner;
end;

procedure TLrCustomCollectionEx.ReadPersistent(Stream: TStream;
	inPersistent: TPersistent);
var
	Reader: TLrReader;
begin
	Reader := TLrReader.Create(Self, 4096);
	try
		Result := Reader.ReadProperty(Instance);
	finally
		Reader.Free;
	end;
end;

end;

procedure TLrCustomCollectionEx.ReadItemsProperty(Stream: TStream);
var
	c, i: Integer;
begin
	Stream.Read(c, 4);
	Reader := TLrReader.Create(Self, 4096);
	try
		ReadListBegin;
	for i := 0 to Pred(c) do
		Add(Stream.ReadComponent(nil));
end;

procedure TLrCustomCollectionEx.WritePersistent(Stream: TStream;
	inPersistent: TPersistent);
begin
end;

procedure TLrCustomCollectionEx.WriteItemsProperty(Stream: TStream);
var
	c, i: Integer;
begin
	c := FItems.Count;
	Stream.Write(c, 4);
	for i := 0 to Pred(c) do
		Stream.WriteComponent(Items[i]);
end;

procedure TLrCustomCollectionEx.DefineProperties(Filer: TFiler);
begin
	inherited;
	Filer.DefineBinaryProperty('Items', ReadItemsProperty, WriteItemsProperty,
		true);
end;

function TLrCustomCollectionEx.GetItems(inIndex: Integer): TPersistent;
begin
	Result := TPersistent(FItems[inIndex]);
end;

procedure TLrCustomCollectionEx.SetItems(inIndex: Integer;
	const Value: TPersistent);
begin
	FItems[inIndex] := Value;
end;

function TLrCustomCollectionEx.GetCount: Integer;
begin
	Result := FItems.Count;
end;

procedure TLrCustomCollectionEx.Add(inPersistent: TPersistent);
begin
	FItems.Add(inPersistent);
	Initialize(inPersistent);
end;

function TLrCustomCollectionEx.FindPersistent(
	const inName: string): TPersistent;
var
	i: Integer;
begin
	Result := nil;
	for i := 0 to Pred(Count) do
		if Items[i].Name = inName then
		begin
			Result := Items[i];
			break;
		end;
end;

function TLrCustomCollectionEx.UniqueName: string;
var
	i: Integer;
begin
	i := Count;
	repeat
		Result := 'Item' + IntToStr(i);
		Inc(i);
	until FindPersistent(Result) = nil;
end;

function TLrCustomCollectionEx.Add(inIndex: Integer): TPersistent;
begin
	Result := ItemClass[inIndex].Create(FOwner);
	Add(Result);
	if Result.Name = '' then
		Result.Name := UniqueName;
end;

procedure TLrCustomCollectionEx.Clear;
begin
	FItems.Clear;
end;

function TLrCustomCollectionEx.GetItemClass(inIndex: Integer): TPersistentClass;
begin
	Result := TPersistent;
end;

function TLrCustomCollectionEx.GetTypeCount: Integer;
begin
	Result := 1;
end;

function TLrCustomCollectionEx.GetTypeDisplayName(inIndex: Integer): string;
begin
	Result := ItemClass[inIndex].ClassName;
end;

procedure TLrCustomCollectionEx.RegisterClasses;
var
	i: Integer;
begin
	for i := 0 to Pred(TypeCount) do
		RegisterClass(ItemClass[i]);
end;

procedure TLrCustomCollectionEx.SetEditor(const Value: Pointer);
begin
	FEditor := Value;
end;

procedure TLrCustomCollectionEx.SetName(const Value: string);
begin
	FName := Value;
end;

procedure TLrCustomCollectionEx.Delete(inIndex: Integer);
begin
	FItems.Delete(inIndex);
end;

procedure TLrCustomCollectionEx.Initialize(inPersistent: TPersistent);
begin
	//
end;

function TLrCustomCollectionEx.IndexOf(inPersistent: TPersistent): Integer;
var
	i: Integer;
begin
	Result := -1;
	for i := 0 to Pred(Count) do
		if Items[i] = inPersistent then
		begin
			Result := i;
			break;
		end;
end;

end.
