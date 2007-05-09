unit LrDynamicProperties;

interface

uses
	SysUtils, TypInfo, Classes;

type
	TLrDynamicPropertyType = ( ptString, ptEvent, ptLastType=MAXINT );
	//
	TLrDynamicPropertyItem = class(TCollectionItem)
	private
		FName: string;
		FPropertyType: TLrDynamicPropertyType;
		FValue: string;
		FData: TPersistent;
	protected
		function GetDisplayName: string; override;
		procedure SetData(const Value: TPersistent);
	public
		constructor Create(inCollection: TCollection); override;
		destructor Destroy; override;
	published
		property Data: TPersistent read FData write SetData;
		property Value: string read FValue write FValue;
		property Name: string read FName write FName;
		property PropertyType: TLrDynamicPropertyType read FPropertyType
			write FPropertyType;
	end;
	//
	TLrDynamicProperties = class(TCollection)
	private
		FOwner: TComponent;
		FExtendable: Boolean;
	protected
		function GetProps(inIndex: Integer): TLrDynamicPropertyItem;
		function GetOwner: TPersistent; override;
		procedure PropertyAdded(inItem: TLrDynamicPropertyItem); virtual;
	public
		constructor Create(AOwner: TComponent; inExtendable: Boolean = false);
		function AddProperty(
			inType: TLrDynamicPropertyType = ptString): TLrDynamicPropertyItem;
		function FindProperty(const inName: string): TLrDynamicPropertyItem;
		procedure Notify(Item: TCollectionItem;
			Action: TCollectionNotification); override;
	public
		property Extendable: Boolean read FExtendable write FExtendable;
		property Props[inIndex: Integer]: TLrDynamicPropertyItem read GetProps;
			default;
	end;

implementation

{ TLrDynamicPropertyItem }

constructor TLrDynamicPropertyItem.Create(inCollection: TCollection);
begin
	inherited;
end;

destructor TLrDynamicPropertyItem.Destroy;
begin
	inherited;
end;

function TLrDynamicPropertyItem.GetDisplayName: string;
begin
	Result := Name;
end;

procedure TLrDynamicPropertyItem.SetData(const Value: TPersistent);
begin
	if Data <> nil then
		Data.Assign(Value);
end;

{ TLrDynamicProperties }

constructor TLrDynamicProperties.Create(AOwner: TComponent;
	inExtendable: Boolean = false);
begin
	inherited Create(TLrDynamicPropertyItem);
	FOwner := AOwner;
	Extendable := inExtendable;
end;

function TLrDynamicProperties.GetOwner: TPersistent;
begin
	Result := FOwner;
end;

procedure TLrDynamicProperties.Notify(Item: TCollectionItem;
	Action: TCollectionNotification);
begin
	inherited;
	if Action = cnAdded then
		PropertyAdded(TLrDynamicPropertyItem(Item));
end;

procedure TLrDynamicProperties.PropertyAdded(inItem: TLrDynamicPropertyItem);
begin
	//
end;

function TLrDynamicProperties.AddProperty(
	inType: TLrDynamicPropertyType): TLrDynamicPropertyItem;
begin
	Result := TLrDynamicPropertyItem(Add);
	Result.PropertyType := inType;
end;

function TLrDynamicProperties.GetProps(inIndex: Integer): TLrDynamicPropertyItem;
begin
	Result := TLrDynamicPropertyItem(Items[inIndex]);
end;

function TLrDynamicProperties.FindProperty(
	const inName: string): TLrDynamicPropertyItem;
var
	i: Integer;
begin
	Result := nil;
	for i := 0 to Pred(Count) do
		if Props[i].Name = inName then
		begin
			Result := Props[i];
			break;
		end;
end;

end.
