unit ThContent;

interface

uses
	Messages, SysUtils, Classes, Controls,
	ThComponent, ThRegExpr, ThMessages;

type
	TThCustomPage = class;
	//
	TThPageContent = class(TThComponent)
	private
		FPage: TThCustomPage;
		FEnabled: Boolean;
	protected
		procedure SetEnabled(const Value: Boolean); virtual;
		procedure SetPage(const Value: TThCustomPage); virtual;
	protected
		procedure UnassignComponent(AComponent: TComponent); override;
	protected
		property Enabled: Boolean read FEnabled write SetEnabled default true;
	public
		constructor Create(AOwner: TComponent); override;
	published
		property Page: TThCustomPage read FPage write SetPage;
	end;
	//
	TThAbstractContent = class(TThPageContent)
	public
		function HasNamedContent(const inName: string;
			out outContent: string): Boolean; virtual; abstract;
		procedure ContentFromParams(const inParams: TStrings); virtual; abstract;
	end;
	//
	TThContentBase = class(TThAbstractContent)
	private
		FInContentText: Boolean;
		FOnBeforeGetContentText: TNotifyEvent;
		FTemplate: Boolean;
		FViewer: TControl;
	protected
		FContentName: string;
		FContentText: string;
	protected
		function GetContentName: string; virtual;
		function GetBaseContentText: string;
		function GetContentText: string; virtual;
		procedure SetContentName(const Value: string); virtual;
		procedure SetContentText(const Value: string); virtual;
		procedure SetTemplate(const Value: Boolean);
		procedure SetViewer(const Value: TControl);
	protected
		procedure Changed; virtual;
		procedure DoBeforeGetContentText;
		function DoMacroReplace(ARegExpr: TRegExpr): string;
		function GetStoredContent: string;
		function MacroReplace(const inText: string): string;
		procedure UnassignComponent(AComponent: TComponent); override;
	protected
		property OnBeforeGetContentText: TNotifyEvent read FOnBeforeGetContentText
			write FOnBeforeGetContentText;
		property Template: Boolean read FTemplate write SetTemplate default false;
	public
		destructor Destroy; override;
		function HasNamedContent(const inName: string;
			out outContent: string): Boolean; override;
		procedure ContentFromParams(const inParams: TStrings); override;
	public
		property ContentName: string read GetContentName write SetContentName;
		property ContentText: string read GetBaseContentText write SetContentText;
		property Viewer: TControl read FViewer write SetViewer;
	end;
	//
	TThContent = class(TThContentBase)
	published
		property ContentName;
		property ContentText;
		property Enabled;
		property Template;
	end;
	//
	TThAbstractStringsContent = class(TThContentBase)
	protected
		function GetStrings: TStrings; virtual; abstract;
		procedure SetStrings(const Value: TStrings); virtual; abstract;
		property Strings: TStrings read GetStrings write SetStrings;
	end;
	//
	TThStringsContentBase = class(TThAbstractStringsContent)
	private
		FStrings: TStringList;
		FFileName: string;
	protected
		function GetContentText: string; override;
		function GetStrings: TStrings; override;
		procedure SetFileName(const Value: string); virtual;
		procedure SetStrings(const Value: TStrings); override;
	protected
		property FileName: string read FFileName write SetFileName;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	published
		property ContentName;
		property Enabled;
		property Template default true;
	end;
	//
	TThContentStrings = class(TThStringsContentBase)
	published
		property Strings;
	end;
	//
	TThFileContent = class(TThStringsContentBase)
	published
		property FileName;
	end;
	//
	TThSourcedContent = class(TThStringsContentBase)
	private
		FSource: TThContentBase;
	protected
		function GetContentText: string; override;
		procedure SetSource(const Value: TThContentBase); virtual;
	protected
		procedure UnassignComponent(AComponent: TComponent); override;
	protected
		property Enabled;
		property Source: TThContentBase read FSource write SetSource;
	published
		property FileName;
		//property Strings;
		//property Template;
	end;
	//
	TThCustomPage = class(TThSourcedContent)
	public
		property PageName: string read GetContentName write SetContentName;
	end;

type
	TThDoForFunc =
		function(inComponent: TComponent; inData: Pointer = nil): Boolean;

procedure ThDoForComponents(inOwner: TComponent; inFunc: TThDoForFunc;
	inData: Pointer = nil);

function ThFindContent(inOwner: TComponent; const inName: string;
	out outContent: string): Boolean;
procedure ThUpdateContent(inOwner: TComponent; inParams: TStrings);
function ThFindPage(inOwner: TComponent; const inName: string;
	out outPage: TThCustomPage): Boolean;

implementation

uses
	StrUtils, ThComponentIterator{, ThStreamContent, ThTemplateContent,
	ThWebDataDictionary};

const
	cThMacroExpression = '\{%([^}]*)\}';

	procedure ThDoForComponents(inOwner: TComponent; inFunc: TThDoForFunc;
		inData: Pointer = nil);
	var
		i: Integer;
	begin
		if inOwner <> nil then
			for i := 0 to Pred(inOwner.ComponentCount) do
				if not inFunc(inOwner.Components[i], inData) then
					break;
	end;

	function ThFindContent(inOwner: TComponent; const inName: string;
		out outContent: string): Boolean;
	var
		i: Integer;
	begin
		Result := true;
		//if (inOwner <> nil) {and (inName <> '')} then
		if (inOwner <> nil) and (inName <> '') then
			with inOwner do
				for i := 0 to Pred(ComponentCount) do
					if Components[i] is TThAbstractContent then
						with TThAbstractContent(Components[i]) do
							if HasNamedContent(inName, outContent) then
								exit;
		Result := false;
	end;

	procedure ThUpdateContent(inOwner: TComponent; inParams: TStrings);
	var
		i: Integer;
	begin
		if (inParams.Count > 0) and (inOwner <> nil) then
			with inOwner do
				for i := 0 to Pred(ComponentCount) do
					if Components[i] is TThAbstractContent then
						TThAbstractContent(Components[i]).ContentFromParams(inParams);
	end;

	function ThFindPage(inOwner: TComponent; const inName: string;
		out outPage: TThCustomPage): Boolean;
	var
		i: Integer;
	begin
		Result := true;
		if (inOwner <> nil) and (inName <> '') then
			with inOwner do
				for i := 0 to Pred(ComponentCount) do
					if Components[i] is TThCustomPage then
						if TThCustomPage(Components[i]).PageName = inName then
						begin
							outPage := TThCustomPage(Components[i]);
							exit;
						end;
		Result := false;
	end;

{ TThPageContent }

constructor TThPageContent.Create(AOwner: TComponent);
begin
	inherited;
	FEnabled := true;
end;

procedure TThPageContent.SetEnabled(const Value: Boolean);
begin
	FEnabled := Value;
end;

procedure TThPageContent.SetPage(const Value: TThCustomPage);
begin
	ChangeComponentProp(TComponent(FPage), Value);
end;

procedure TThPageContent.UnassignComponent(AComponent: TComponent);
begin
	if AComponent = FPage then
		FPage := nil;
end;

{ TThContentBase }

destructor TThContentBase.Destroy;
begin
	if FViewer <> nil then
		FViewer.Free;
	inherited;
end;

function TThContentBase.HasNamedContent(const inName: string;
	out outContent: string): Boolean;
begin
	Result := (inName = ContentName) or ((inName = '') and (ContentName = '*'));
	if Result then
		outContent := ContentText
end;

function TThContentBase.DoMacroReplace(ARegExpr: TRegExpr): string;
begin
	ThFindContent(Owner, ARegExpr.Match[1], Result);
end;

function TThContentBase.MacroReplace(const inText: string): string;
begin
	with TRegExpr.Create do
	try
		Expression := cThMacroExpression;
		Result := ReplaceEx(inText, DoMacroReplace);
	finally
		Free;
	end;
end;

procedure TThContentBase.DoBeforeGetContentText;
begin
	if Assigned(FOnBeforeGetContentText) then
		FOnBeforeGetContentText(Self);
end;

function TThContentBase.GetStoredContent: string;
begin
	if Template then
		Result := MacroReplace(FContentText)
	else
		Result := FContentText;
end;

function TThContentBase.GetBaseContentText: string;
begin
	if FInContentText then
		raise Exception.Create('Circular reference to content "' + Name + '"');
	FInContentText := true;
	try
		DoBeforeGetContentText;
		if not Enabled then
			Result := ''
		else
			Result := GetContentText;
			if Template then
				Result := MacroReplace(Result);
	except
		on E: Exception do
			Result := E.Message;
	end;
	FInContentText := false;
end;

function TThContentBase.GetContentName: string;
begin
	Result := FContentName;
end;

function TThContentBase.GetContentText: string;
begin
	Result := FContentText;
end;

procedure TThContentBase.ContentFromParams(const inParams: TStrings);
begin
	if (ContentName <> '') and (inParams.IndexOfName(ContentName) > 0) then
		ContentText := inParams.Values[ContentName];
end;

procedure TThContentBase.SetContentName(const Value: string);
begin
	FContentName := Value;
end;

procedure TThContentBase.SetContentText(const Value: string);
begin
	FContentText := Value;
end;

procedure TThContentBase.SetTemplate(const Value: Boolean);
begin
	FTemplate := Value;
end;

procedure TThContentBase.SetViewer(const Value: TControl);
begin
	ChangeComponentProp(TComponent(FViewer), Value);
end;

procedure TThContentBase.UnassignComponent(AComponent: TComponent);
begin
	inherited;
	if AComponent = FViewer then
		FViewer := nil;
end;

procedure TThContentBase.Changed;
begin
	if FViewer <> nil then
		FViewer.Perform(THM_CHANGE, 0, 0);
end;

{ TThStringsContentBase }

constructor TThStringsContentBase.Create(AOwner: TComponent);
begin
	inherited;
	FStrings := TStringList.Create;
	FTemplate := true;
end;

destructor TThStringsContentBase.Destroy;
begin
	FStrings.Free;
	inherited;
end;

function TThStringsContentBase.GetStrings: TStrings;
begin
	Result := FStrings;
end;

procedure TThStringsContentBase.SetStrings(const Value: TStrings);
begin
	FStrings.Assign(Value);
end;

function TThStringsContentBase.GetContentText: string;
begin
	Result := FStrings.Text;
end;

procedure TThStringsContentBase.SetFileName(const Value: string);
begin
	FFileName := Value;
	if FileExists(FFileName) then
		FStrings.LoadFromFile(FFileName)
	else
		FStrings.Clear;
end;

{ TThSourcedContent }

function TThSourcedContent.GetContentText: string;
begin
	if Source = nil then
		Result := inherited GetContentText
	else
		Result := FSource.ContentText;
end;

procedure TThSourcedContent.SetSource(const Value: TThContentBase);
begin
	ChangeComponentProp(TComponent(FSource), Value);
end;

procedure TThSourcedContent.UnassignComponent(AComponent: TComponent);
begin
	inherited;
	if AComponent = FSource then
		FSource := nil;
end;

end.
