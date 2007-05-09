unit LrCollection;

interface

uses
	SysUtils, Classes, Contnrs;

type
	TLrCustomCollection = class(TPersistent)
	private
		FEditor: Pointer;
		FItems: TObjectList;
		FName: string;
		FOwner: TComponent;
	protected
		function FindComponent(const inName: string): TComponent;
		function GetCount: Integer;
		function GetItemClass(inIndex: Integer): TComponentClass; virtual;
		function GetItems(inIndex: Integer): TComponent;
		function GetOwner: TPersistent; override;
		function GetTypeCount: Integer; virtual;
		function GetTypeDisplayName(inIndex: Integer): string; virtual;
		function UniqueName: string;
		procedure DefineProperties(Filer: TFiler); override;
		procedure Initialize(inComponent: TComponent); virtual;
		procedure ReadItemsProperty(Stream: TStream); //Reader: TReader);
		procedure RegisterClasses; virtual;
		procedure SetEditor(const Value: Pointer);
		procedure SetItems(inIndex: Integer; const Value: TComponent);
		procedure SetName(const Value: string);
		procedure WriteItemsProperty(Stream: TStream); //Writer: TWriter);
	public
		constructor Create(inOwner: TComponent); virtual;
		destructor Destroy; override;
		function Add(inIndex: Integer): TComponent; overload; virtual;
		function IndexOf(inComponent: TComponent): Integer;
		procedure Add(inComponent: TComponent); overload;
		procedure Clear; virtual;
		procedure Delete(inIndex: Integer);
		property Count: Integer read GetCount;
		property Editor: Pointer read FEditor write SetEditor;
		property Name: string read FName write SetName;
		property ItemClass[inIndex: Integer]: TComponentClass read GetItemClass;
		property Items[inIndex: Integer]: TComponent read GetItems
			write SetItems; default;
		property Owner: TComponent read FOwner write FOwner;
		property TypeCount: Integer read GetTypeCount;
		property TypeDisplayName[inIndex: Integer]: string read GetTypeDisplayName;
	end;
	//
	TLrCollectionTest = class(TComponent)
	private
		FItems: TLrCustomCollection;
	protected
		function GetAdd: Boolean;
		function GetCount: Integer;
		procedure SetAdd(const Value: Boolean);
		procedure SetCount(const Value: Integer);
		procedure SetItems(const Value: TLrCustomCollection);
	public
		constructor Create(inOwner: TComponent); override;
	published
		property Items: TLrCustomCollection read FItems write SetItems;
		property Count: Integer read GetCount write SetCount;
		property Add: Boolean read GetAdd write SetAdd;
	end;

implementation

{ TLrCustomCollection }

constructor TLrCustomCollection.Create(inOwner: TComponent);
begin
	inherited Create;
	FOwner := inOwner;
	FItems := TObjectList.Create;
	FName := 'Items';
end;

destructor TLrCustomCollection.Destroy;
begin
	FItems.Free;
	inherited;
end;

function TLrCustomCollection.GetOwner: TPersistent;
begin
	Result := FOwner;
end;

procedure TLrCustomCollection.ReadItemsProperty(Stream: TStream);
var
	c, i: Integer;
begin
	Stream.Read(c, 4);
	for i := 0 to Pred(c) do
		Add(Stream.ReadComponent(nil));
end;

procedure TLrCustomCollection.WriteItemsProperty(Stream: TStream);
var
	c, i: Integer;
begin
	c := FItems.Count;
	Stream.Write(c, 4);
	for i := 0 to Pred(c) do
		Stream.WriteComponent(Items[i]);
end;

procedure TLrCustomCollection.DefineProperties(Filer: TFiler);
begin
	inherited;
	Filer.DefineBinaryProperty('Items', ReadItemsProperty, WriteItemsProperty,
		true);
end;

function TLrCustomCollection.GetItems(inIndex: Integer): TComponent;
begin
	Result := TComponent(FItems[inIndex]);
end;

procedure TLrCustomCollection.SetItems(inIndex: Integer;
	const Value: TComponent);
begin
	FItems[inIndex] := Value;
end;

function TLrCustomCollection.GetCount: Integer;
begin
	Result := FItems.Count;
end;

procedure TLrCustomCollection.Add(inComponent: TComponent);
begin
	FItems.Add(inComponent);
	Initialize(inComponent);
end;

function TLrCustomCollection.FindComponent(const inName: string): TComponent;
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

function TLrCustomCollection.UniqueName: string;
var
	i: Integer;
begin
	i := Count;
	repeat
		Result := 'Item' + IntToStr(i);
		Inc(i);
	until FindComponent(Result) = nil;
end;

function TLrCustomCollection.Add(inIndex: Integer): TComponent;
begin
	Result := ItemClass[inIndex].Create(FOwner);
	Add(Result);
	if Result.Name = '' then
		Result.Name := UniqueName;
end;

procedure TLrCustomCollection.Clear;
begin
	FItems.Clear;
end;

function TLrCustomCollection.GetItemClass(inIndex: Integer): TComponentClass;
begin
	Result := TComponent;
end;

function TLrCustomCollection.GetTypeCount: Integer;
begin
	Result := 1;
end;

function TLrCustomCollection.GetTypeDisplayName(inIndex: Integer): string;
begin
	Result := ItemClass[inIndex].ClassName;
end;

procedure TLrCustomCollection.RegisterClasses;
var
	i: Integer;
begin
	for i := 0 to Pred(TypeCount) do
		RegisterClass(ItemClass[i]);
end;

procedure TLrCustomCollection.SetEditor(const Value: Pointer);
begin
	FEditor := Value;
end;

procedure TLrCustomCollection.SetName(const Value: string);
begin
	FName := Value;
end;

procedure TLrCustomCollection.Delete(inIndex: Integer);
begin
	FItems.Delete(inIndex);
end;

procedure TLrCustomCollection.Initialize(inComponent: TComponent);
begin
	//
end;

function TLrCustomCollection.IndexOf(inComponent: TComponent): Integer;
var
	i: Integer;
begin
	Result := -1;
	for i := 0 to Pred(Count) do
		if Items[i] = inComponent then
		begin
			Result := i;
			break;
		end;
end;

{ TLrCollectionTest }

constructor TLrCollectionTest.Create(inOwner: TComponent);
begin
	inherited;
	FItems := TLrCustomCollection.Create(Self);
end;

function TLrCollectionTest.GetAdd: Boolean;
begin
	Result := true;
end;

procedure TLrCollectionTest.SetAdd(const Value: Boolean);
begin
	if not (csLoading in ComponentState) then
		Items.Add(0);
end;

function TLrCollectionTest.GetCount: Integer;
begin
	Result := Items.Count;
end;

procedure TLrCollectionTest.SetCount(const Value: Integer);
begin
	//
end;

procedure TLrCollectionTest.SetItems(const Value: TLrCustomCollection);
begin
	FItems.Assign(Value);
end;

end.
