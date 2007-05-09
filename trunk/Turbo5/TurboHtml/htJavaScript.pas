unit htJavaScript;

interface

uses
	SysUtils, Classes,
	LrDynamicProperties;

type
	ThtJavaScriptEventRegistry = class(TStringList)
		constructor Create;
	end;
	//
	ThtJavaScriptEvents = class(TLrDynamicProperties)
	protected
		procedure CreateDefaultEvents;
	public
		constructor Create(AOwner: TComponent); reintroduce;
		function AddEvent: TLrDynamicPropertyItem;
	end;
	//
{
	ThtJavaScriptEventItem = class(TCollectionItem)
	private
		FEventName: string;
		FCode: TStringList;
		FFunctionName: string;
	protected
		function GetTrimmedName: string;
		procedure SetCode(const Value: TStringList);
		procedure SetEventName(const Value: string);
		procedure SetFunctionName(const Value: string);
	protected
		function GetDisplayName: string; override;
	public
		constructor Create(inCollection: TCollection); override;
		destructor Destroy; override;
	published
		property FunctionName: string read FFunctionName write SetFunctionName;
		property EventName: string read FEventName write SetEventName;
		property Code: TStringList read FCode write SetCode;
		property TrimmedName: string read GetTrimmedName;
	end;
}
	//
{
	ThtJavaScriptEvents = class(TCollection)
	private
		FOwner: TComponent;
	protected
		function GetEvent(inIndex: Integer): ThtJavaScriptEventItem;
		function GetOwner: TPersistent; override;
	public
		constructor Create(AOwner: TComponent);
		procedure CreateDefaultEvents;
		function AddEvent: ThtJavaScriptEventItem;
		function FindEvent(inName: string): ThtJavaScriptEventItem;
		procedure GenerateFuncs(const inJavaName: string);
		//procedure ListAttributes(const inJavaName: string;
		//	inList: ThtAttributeList; inIndex: Integer = 0);
	public
		property Event[inIndex: Integer]: ThtJavaScriptEventItem read GetEvent;
			default;
	end;
}

var
	ThJavaScriptEventRegistry: ThtJavaScriptEventRegistry;

implementation

const
	cThJsNumEventNames = 22;
	cThJsEventNames: array[0..Pred(cThJsNumEventNames)] of string =
	(
		'onKeyDown', 'onKeyPress', 'onKeyUp',
		'onMouseDown', 'onMouseUp', 'onMouseMove', 'onMouseOver', 'onMouseOut',
		'onClick', 'onDblClick',
		'onLoad', 'onAbort', 'onError',
		'onMove','onResize', 'onScroll',
		'onReset', 'onSubmit', 'onBlur', 'onChange', 'onFocus',
		'onSelect'
	);

{ ThtJavaScriptEventRegistry }

constructor ThtJavaScriptEventRegistry.Create;
var
	i: Integer;
begin
	for i := 0 to Pred(cThJsNumEventNames) do
		Add(cThJsEventNames[i]);
end;

{ ThtJavaScriptEvents }

constructor ThtJavaScriptEvents.Create(AOwner: TComponent);
begin
	inherited Create(AOwner, true);
	CreateDefaultEvents;
end;

procedure ThtJavaScriptEvents.CreateDefaultEvents;
var
	i: Integer;
begin
	for i := 0 to Pred(ThJavaScriptEventRegistry.Count) do
		AddEvent.Name := ThJavaScriptEventRegistry[i];
end;

function ThtJavaScriptEvents.AddEvent: TLrDynamicPropertyItem;
begin
	Result := AddProperty(ptEvent);
end;

{ ThtJavaScriptEventItem }

{
constructor ThtJavaScriptEventItem.Create(inCollection: TCollection);
begin
	inherited;
	FCode := TStringList.Create;
end;

destructor ThtJavaScriptEventItem.Destroy;
begin
	FCode.Free;
	inherited;
end;

function ThtJavaScriptEventItem.GetDisplayName: string;
begin
	Result := EventName;
end;

procedure ThtJavaScriptEventItem.SetEventName(const Value: string);
begin
	FEventName := Value;
end;

procedure ThtJavaScriptEventItem.SetCode(const Value: TStringList);
begin
	FCode.Assign(Value);
end;

procedure ThtJavaScriptEventItem.SetFunctionName(const Value: string);
begin
	FFunctionName := Value;
end;

function ThtJavaScriptEventItem.GetTrimmedName: string;
begin
	Result := Copy(EventName, 3, MAXINT);
end;
}

{ ThtJavaScriptEvents }

{
constructor ThtJavaScriptEvents.Create(AOwner: TComponent);
begin
	inherited Create(ThtJavaScriptEventItem);
	FOwner := AOwner;
	CreateDefaultEvents;
end;

function ThtJavaScriptEvents.AddEvent: ThtJavaScriptEventItem;
begin
	Result := ThtJavaScriptEventItem(Add);
end;

function ThtJavaScriptEvents.FindEvent(inName: string): ThtJavaScriptEventItem;
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		if Event[i].EventName = inName then
		begin
			Result := Event[i];
			exit;
		end;
	Result := nil;
end;

procedure ThtJavaScriptEvents.CreateDefaultEvents;
var
	i: Integer;
begin
	for i := 0 to Pred(ThJavaScriptEventRegistry.Count) do
		AddEvent.EventName := ThJavaScriptEventRegistry[i];
end;

function ThtJavaScriptEvents.GetOwner: TPersistent;
begin
	Result := FOwner;
end;

function ThtJavaScriptEvents.GetEvent(
	inIndex: Integer): ThtJavaScriptEventItem;
begin
	Result := ThtJavaScriptEventItem(Items[inIndex]);
end;

procedure ThtJavaScriptEvents.GenerateFuncs(const inJavaName: string);
var
	i: Integer;
	c: string;
begin
	for i := 0 to Pred(Count) do
		with Event[i] do
			if Code.Count > 0 then
			begin
				if FunctionName <> '' then
					c := Format('function %s(inSender, inIndex) {'#10,
						[ FunctionName ])
				else
					c := Format('function %s%s(inSender, inIndex) {'#10,
						[ inJavaName, TrimmedName ]);
}
//				c := c + Code.Text + '}'#10;
{
		end;
end;
}

{
procedure ThtJavaScriptEvents.ListAttributes(const inJavaName: string;
	inList: ThtAttributeList; inIndex: Integer = 0);
var
	i: Integer;
	n: string;
begin
	for i := 0 to Count - 1 do
		with Event[i] do
			if (Code.Text <> '') or (FunctionName <> '') then
			begin
				if FunctionName <> '' then
					n := Format('return %s(this, %d);', [ FunctionName, inIndex ])
				else
					n := Format('return %s%s(this, %d);',
						[ inJavaName, TrimmedName, inIndex ]);
				inList.Add(EventName, n);
			end;
end;
}

initialization
	ThJavaScriptEventRegistry := ThtJavaScriptEventRegistry.Create;
finalization
	ThJavaScriptEventRegistry.Free;
end.

