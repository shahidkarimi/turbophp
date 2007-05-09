unit ThJavaScript;

interface

uses
	SysUtils, Classes, ThAttributeList, ThComponent;

type
	TThJavaScript = class(TThComponent)
	private
		FScript: TStringList;
	protected
		function GetScript: TStrings;
		procedure SetScript(const Value: TStrings);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		//procedure Publish(inPage: TThHtmlPage); override;
	published
		property Script: TStrings read GetScript write SetScript;
	end;
	//
	TThJavaScriptEventRegistry = class(TStringList)
		constructor Create;
	end;
	//
	TThJavaScriptEventItem = class(TCollectionItem)
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
	//
	TThJavaScriptEvents = class(TCollection)
	private
		FOwner: TComponent;
	protected
		function GetEvent(inIndex: Integer): TThJavaScriptEventItem;
		function GetOwner: TPersistent; override;
	public
		constructor Create(AOwner: TComponent);
		procedure CreateDefaultEvents;
		function AddEvent: TThJavaScriptEventItem; 
		function FindEvent(inName: string): TThJavaScriptEventItem;
		procedure GenerateFuncs(const inJavaName: string);
		procedure ListAttributes(const inJavaName: string;
			inList: TThAttributeList; inIndex: Integer = 0);
	public
		property Event[inIndex: Integer]: TThJavaScriptEventItem read GetEvent;
			default;
	end;

var
	ThJavaScriptEventRegistry: TThJavaScriptEventRegistry;

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

{ TThJavaScript }

constructor TThJavaScript.Create(AOwner: TComponent);
begin
	inherited;
	FScript := TStringList.Create;
end;

destructor TThJavaScript.Destroy;
begin
	FScript.Free;
	inherited;
end;

function TThJavaScript.GetScript: TStrings;
begin
	Result := FScript;
end;

{
procedure TThJavaScript.Publish(inPage: TThHtmlPage);
begin
	with inPage do
	begin
		if Script.Count > 0 then
			Code.AddContent(Script.Text);
		if ScriptOnLoad.Count > 0 then
			CodeBodyLoad.AddContent(ScriptOnLoad.Text);
		if ScriptOnUnload.Count > 0 then
			CodeBodyUnload.AddContent(ScriptOnUnload.Text);
	end;
end;
}

procedure TThJavaScript.SetScript(const Value: TStrings);
begin
	FScript.Assign(Value);
end;

{ TThJavaScriptEventRegistry }

constructor TThJavaScriptEventRegistry.Create;
var
	i: Integer;
begin
	for i := 0 to Pred(cThJsNumEventNames) do
		Add(cThJsEventNames[i]);
end;

{ TThJavaScriptEventItem }

constructor TThJavaScriptEventItem.Create(inCollection: TCollection);
begin
	inherited;
	FCode := TStringList.Create;
end;

destructor TThJavaScriptEventItem.Destroy;
begin
	FCode.Free;
	inherited;
end;

function TThJavaScriptEventItem.GetDisplayName: string;
begin
	Result := EventName;
end;

procedure TThJavaScriptEventItem.SetEventName(const Value: string);
begin
	FEventName := Value;
end;

procedure TThJavaScriptEventItem.SetCode(const Value: TStringList);
begin
	FCode.Assign(Value);
end;

procedure TThJavaScriptEventItem.SetFunctionName(const Value: string);
begin
	FFunctionName := Value;
end;

function TThJavaScriptEventItem.GetTrimmedName: string;
begin
	Result := Copy(EventName, 3, MAXINT);
end;

{ TThJavaScriptEvents }

constructor TThJavaScriptEvents.Create(AOwner: TComponent);
begin
	inherited Create(TThJavaScriptEventItem);
	FOwner := AOwner;
	CreateDefaultEvents;
end;

function TThJavaScriptEvents.AddEvent: TThJavaScriptEventItem;
begin
	Result := TThJavaScriptEventItem(Add);
end;

function TThJavaScriptEvents.FindEvent(inName: string): TThJavaScriptEventItem;
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

procedure TThJavaScriptEvents.CreateDefaultEvents;
var
	i: Integer;
begin
	for i := 0 to Pred(ThJavaScriptEventRegistry.Count) do
		AddEvent.EventName := ThJavaScriptEventRegistry[i];
end;

function TThJavaScriptEvents.GetOwner: TPersistent;
begin
	Result := FOwner;
end;

function TThJavaScriptEvents.GetEvent(
	inIndex: Integer): TThJavaScriptEventItem;
begin
	Result := TThJavaScriptEventItem(Items[inIndex]);
end;

procedure TThJavaScriptEvents.GenerateFuncs(const inJavaName: string);
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
				c := c + Code.Text + '}'#10;
		end;
end;

procedure TThJavaScriptEvents.ListAttributes(const inJavaName: string;
	inList: TThAttributeList; inIndex: Integer = 0);
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

initialization
	ThJavaScriptEventRegistry := TThJavaScriptEventRegistry.Create;
finalization
	ThJavaScriptEventRegistry.Free;
end.

