unit DynamicProperties;

interface

uses
	SysUtils, TypInfo, Classes;

type
	TDynamicPropertyType = ( ptString, ptEvent );
	//
	TDynamicPropertyItem = class(TCollectionItem)
	private
		FName: string;
		FValue: string;
		FData: Integer;
		FPropertyType: TDynamicPropertyType;
	protected
		function GetDisplayName: string; override;
	public
		constructor Create(inCollection: TCollection); override;
		destructor Destroy; override;
	published
		property Data: Integer read FData write FData;
		property Value: string read FValue write FValue;
		property Name: string read FName write FName;
		property PropertyType: TDynamicPropertyType read FPropertyType
			write FPropertyType;
	end;
	//
	TDynamicProperties = class(TCollection)
	private
		FOwner: TComponent;
    FExtendable: Boolean;
	protected
		function GetProps(inIndex: Integer): TDynamicPropertyItem;
		function GetOwner: TPersistent; override;
	public
		constructor Create(AOwner: TComponent; inExtendable: Boolean = false);
		function AddEvent: TDynamicPropertyItem;
		function AddProperty: TDynamicPropertyItem;
		function FindProperty(const inName: string): TDynamicPropertyItem;
	public
		property Extendable: Boolean read FExtendable write FExtendable;
		property Props[inIndex: Integer]: TDynamicPropertyItem read GetProps;
			default;
	end;

implementation

{ TDynamicPropertyItem }

constructor TDynamicPropertyItem.Create(inCollection: TCollection);
begin
	inherited;
end;

destructor TDynamicPropertyItem.Destroy;
begin
	inherited;
end;

function TDynamicPropertyItem.GetDisplayName: string;
begin
	Result := Name;
end;

{ TDynamicProperties }

constructor TDynamicProperties.Create(AOwner: TComponent;
	inExtendable: Boolean = false);
begin
	inherited Create(TDynamicPropertyItem);
	FOwner := AOwner;
	Extendable := inExtendable;
end;

function TDynamicProperties.GetOwner: TPersistent;
begin
	Result := FOwner;
end;

function TDynamicProperties.AddProperty: TDynamicPropertyItem;
begin
	Result := TDynamicPropertyItem(Add);
end;

function TDynamicProperties.AddEvent: TDynamicPropertyItem;
begin
	Result := TDynamicPropertyItem(Add);
	Result.PropertyType := ptEvent;
end;

function TDynamicProperties.GetProps(inIndex: Integer): TDynamicPropertyItem;
begin
	Result := TDynamicPropertyItem(Items[inIndex]);
end;

function TDynamicProperties.FindProperty(const inName: string): TDynamicPropertyItem;
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
