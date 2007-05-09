unit ThWebDataDictionary;

interface

uses
	SysUtils, Classes, Controls, ThContent, ThWebVariable;

type
	TThDictionaryDatum = class(TCollectionItem)
	private
		FDatum: TThDatum;
	protected
		function GetDisplayName: string; override;
		function GetValidator: TThValidator;
		function GetWebName: string;
		function GetWebValue: string;
		procedure SetDisplayName(const Value: string); override;
		procedure SetValidator(const Value: TThValidator);
		procedure SetWebName(const Value: string);
		procedure SetWebValue(const Value: string);
	protected
		function OwnerComponent: TComponent;
	public
		constructor Create(Collection: TCollection); override;
		destructor Destroy; override;
		procedure Notification(AComponent: TComponent; Operation: TOperation);
	published
		property DisplayName;
		property WebName: string read GetWebName write SetWebName;
		property WebValue: string read GetWebValue write SetWebValue;
		property Validator: TThValidator read GetValidator write SetValidator;
	end;
	//
	TThDictionaryData = class(TOwnedCollection)
	private
		function GetDictionaryItems(inIndex: Integer): TThDictionaryDatum;
		procedure SetDictionaryItems(inIndex: Integer;
			const Value: TThDictionaryDatum);
    function GetNamedItems(const inName: string): TThDictionaryDatum;
    procedure SetNamedItems(const inName: string;
      const Value: TThDictionaryDatum);
	public
		constructor Create(AOwner: TComponent);
		function AddItem: TThDictionaryDatum;
		procedure ClearData;
		function Find(const inName: string): TThDictionaryDatum; overload;
		function Find(const inName: string;
			out outItem: TThDictionaryDatum): Boolean; overload;
		procedure Notification(AComponent: TComponent; Operation: TOperation);
		procedure UpdateFromParams(inParams: TStrings);
	public
		property DictionaryItems[inIndex: Integer]: TThDictionaryDatum
			read GetDictionaryItems write SetDictionaryItems; default;
		property NamedItems[const inName: string]: TThDictionaryDatum
			read GetNamedItems write SetNamedItems;
	end;
	//
	TThWebDataDictionary = class(TThContentBase)
	private
		FData: TThDictionaryData;
    function GetNamedItems(const inName: string): TThDictionaryDatum;
    procedure SetNamedItems(const inName: string;
      const Value: TThDictionaryDatum);
	protected
		procedure SetData(const Value: TThDictionaryData);
	protected
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure ContentFromParams(const inParams: TStrings); override;
		function HasNamedContent(const inName: string;
			out outContent: string): Boolean; override;
	public
		property NamedItems[const inName: string]: TThDictionaryDatum
			read GetNamedItems write SetNamedItems; default;
	published
		property Data: TThDictionaryData read FData
			write SetData;
	end;
	//
	TThDictionaryIterator = class(TList)
	private
		function GetDictionary: TThWebDataDictionary;
		procedure ListComponents(inClass: TClass);
		function GetDictionaries(inIndex: Integer): TThWebDataDictionary;
    procedure SetContainer(const Value: TComponent);
	protected
		FContainer: TComponent;
		FIndex: Integer;
	public
		constructor Create(inContainer: TComponent = nil);
		function Next: Boolean;
		procedure Reset;
		property Container: TComponent read FContainer write SetContainer;
		property Dictionary: TThWebDataDictionary read GetDictionary;
		property Dictionaries[inIndex: Integer]: TThWebDataDictionary
			read GetDictionaries;
		property Index: Integer read FIndex write FIndex;
	end;

implementation

{ TThDictionaryDatum }

constructor TThDictionaryDatum.Create(Collection: TCollection);
begin
	inherited;
	FDatum := TThDatum.Create;
end;

destructor TThDictionaryDatum.Destroy;
begin
	FDatum.Free;
	inherited;
end;

function TThDictionaryDatum.OwnerComponent: TComponent;
begin
	Result := TComponent(Collection.Owner);
end;

function TThDictionaryDatum.GetValidator: TThValidator;
begin
	Result := FDatum.Validator;
end;

procedure TThDictionaryDatum.SetValidator(const Value: TThValidator);
begin
	if Validator <> nil then
		Validator.RemoveFreeNotification(OwnerComponent);
	FDatum.Validator := Value;
	if Validator <> nil then
		Validator.FreeNotification(OwnerComponent);
end;

procedure TThDictionaryDatum.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	if (Operation = opRemove) and (AComponent = Validator) then
		FDatum.Validator := nil;
end;

function TThDictionaryDatum.GetWebName: string;
begin
	Result := FDatum.WebName;
end;

function TThDictionaryDatum.GetWebValue: string;
begin
	Result := FDatum.WebValue;
end;

procedure TThDictionaryDatum.SetWebName(const Value: string);
begin
	if Value = '' then
		FDatum.WebName := 'Unnamed'
	else
		FDatum.WebName := Value;
end;

procedure TThDictionaryDatum.SetWebValue(const Value: string);
begin
	FDatum.WebValue := Value;
end;

function TThDictionaryDatum.GetDisplayName: string;
begin
	Result := FDatum.DisplayName;
end;

procedure TThDictionaryDatum.SetDisplayName(const Value: string);
begin
	FDatum.DisplayName := Value;
end;

{ TThDictionaryData }

constructor TThDictionaryData.Create(AOwner: TComponent);
begin
	inherited Create(AOwner, TThDictionaryDatum);
end;

function TThDictionaryData.AddItem: TThDictionaryDatum;
begin
	Result := TThDictionaryDatum(Add);
end;

function TThDictionaryData.Find(
	const inName: string): TThDictionaryDatum;
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		if DictionaryItems[i].WebName = inName then
		begin
			Result := DictionaryItems[i];
			exit;
		end;
	Result := nil;
end;

function TThDictionaryData.Find(const inName: string;
	out outItem: TThDictionaryDatum): Boolean;
begin
	outItem := Find(inName);
	Result := (outItem <> nil);
end;

function TThDictionaryData.GetDictionaryItems(
	inIndex: Integer): TThDictionaryDatum;
begin
	Result := TThDictionaryDatum(Items[inIndex]);
end;

procedure TThDictionaryData.Notification(AComponent: TComponent;
	Operation: TOperation);
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		DictionaryItems[i].Notification(AComponent, Operation);
end;

procedure TThDictionaryData.SetDictionaryItems(inIndex: Integer;
	const Value: TThDictionaryDatum);
begin
	Items[inIndex] := Value;
end;

procedure TThDictionaryData.UpdateFromParams(inParams: TStrings);
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		with DictionaryItems[i] do
			if inParams.IndexOfName(WebName) >= 0 then
				WebValue := inParams.Values[WebName];
end;

procedure TThDictionaryData.ClearData;
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		with DictionaryItems[i] do
			WebValue := '';
end;

function TThDictionaryData.GetNamedItems(
	const inName: string): TThDictionaryDatum;
begin
	Result := Find(inName);
end;

procedure TThDictionaryData.SetNamedItems(const inName: string;
	const Value: TThDictionaryDatum);
begin
	Find(inName).Assign(Value);
end;

{ TThWebDataDictionary }

procedure TThWebDataDictionary.ContentFromParams(
	const inParams: TStrings);
begin
	Data.UpdateFromParams(inParams);
end;

constructor TThWebDataDictionary.Create(AOwner: TComponent);
begin
	inherited;
	FData := TThDictionaryData.Create(Self);
end;

destructor TThWebDataDictionary.Destroy;
begin
	inherited;
end;

function TThWebDataDictionary.HasNamedContent(const inName: string;
	out outContent: string): Boolean;
var
	datum: TThDictionaryDatum;
begin
	Result := Data.Find(inName, datum);
	if Result then
		outContent := datum.WebValue;
end;

procedure TThWebDataDictionary.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	if Data <> nil then
		Data.Notification(AComponent, Operation);
end;

procedure TThWebDataDictionary.SetData(const Value: TThDictionaryData);
begin
	FData.Assign(Value);
end;

function TThWebDataDictionary.GetNamedItems(
	const inName: string): TThDictionaryDatum;
begin
	Result := Data.NamedItems[inName];
end;

procedure TThWebDataDictionary.SetNamedItems(const inName: string;
	const Value: TThDictionaryDatum);
begin
	Data.NamedItems[inName] := Value;
end;

{ TThDictionaryIterator }

constructor TThDictionaryIterator.Create(inContainer: TComponent);
begin
	FIndex := -1;
	Container := inContainer;
end;

function TThDictionaryIterator.GetDictionaries(
	inIndex: Integer): TThWebDataDictionary;
begin
	Result := TThWebDataDictionary(Items[inIndex]);
end;

procedure TThDictionaryIterator.ListComponents(inClass: TClass);
var
	i: Integer;
begin
	Clear;
	if FContainer <> nil then
		for i := 0 to FContainer.ComponentCount - 1 do
			if FContainer.Components[i] is inClass then
				Add(FContainer.Components[i]);
end;

procedure TThDictionaryIterator.Reset;
begin
	Index := -1;
end;

function TThDictionaryIterator.Next: Boolean;
begin
	Inc(FIndex);
	Result := Index < Count;
	if not Result then
		Reset;
end;

function TThDictionaryIterator.GetDictionary: TThWebDataDictionary;
begin
	Result := Dictionaries[Index];
end;

procedure TThDictionaryIterator.SetContainer(const Value: TComponent);
begin
	FContainer := Value;
	ListComponents(TThWebDataDictionary);
end;

end.
