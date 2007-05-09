unit PhpPage;

interface

uses
	SysUtils, Classes, Controls,
	ThHtmlPage, TpControls;

type
	TTpDebugFlag = ( dfDumpObjects, dfDumpRequest, dfDumpBlocks, dfDumpNodes );
	TTpDebugFlags = set of TTpDebugFlag;
	//
	TPhpPage = class(TThHtmlPage)
	private
		FDebug: Boolean;
		FDebugFlags: TTpDebugFlags;
		FJsSource: TStrings;
		FJsUrl: string;
		FNewName: string;
		FOnBeforeClicks: TTpEvent;
		FOnBeforeInput: TTpEvent;
		FOnGenerate: TTpEvent;
		FOnNameChange: TNotifyEvent;
		FPhpFilename: string;
		FPhpSource: TStrings;
		FPreviewOnPublish: Boolean;
		FShowCaptions: Boolean;
		FShowComponents: Boolean;
		FShowGrid: Boolean;
		FStyleUrl: string;
    FIsModule: Boolean;
	protected
		function GetAppTag: string;
		function GetJsTag(const inSrc: string): string;
		function GetStyleTag(const inHref: string): string;
		procedure SetJsSource(const Value: TStrings);
		procedure SetName(const NewName: TComponentName); override;
		procedure SetPhpSource(const Value: TStrings);
		procedure SetShowCaptions(const Value: Boolean);
		procedure SetShowComponents(const Value: Boolean);
		procedure SetShowGrid(const Value: Boolean);
	protected
		procedure GenerateHeaders; override;
		procedure GenerateStyles; override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		property IsModule: Boolean read FIsModule write FIsModule;
		property JsUrl: string read FJsUrl write FJsUrl;
		property NewName: string read FNewName;
		property OnNameChange: TNotifyEvent read FOnNameChange write FOnNameChange;
		property StyleUrl: string read FStyleUrl write FStyleUrl;
	published
		property Debug: Boolean read FDebug write FDebug default false;
		property DebugFlags: TTpDebugFlags read FDebugFlags	write FDebugFlags
			default [ dfDumpObjects, dfDumpRequest ];
		property PhpFilename: string read FPhpFilename
			write FPhpFilename;
		property JsSource: TStrings read FJsSource write SetJsSource;
		property OnBeforeClicks: TTpEvent read FOnBeforeClicks
			write FOnBeforeClicks;
		property OnBeforeInput: TTpEvent read FOnBeforeInput write FOnBeforeInput;
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
		property PreviewOnPublish: Boolean read FPreviewOnPublish
			write FPreviewOnPublish default true;
		property PhpSource: TStrings read FPhpSource write SetPhpSource;
		property ShowCaptions: Boolean read FShowCaptions write SetShowCaptions;
		property ShowGrid: Boolean read FShowGrid write SetShowGrid;
		property ShowComponents: Boolean read FShowComponents write SetShowComponents;
	end;
	//
	// Backward compat
	TPhpForm = class(TPhpPage)
	end;

implementation

uses
	ThTag;

{ TPhpPage }

constructor TPhpPage.Create(AOwner: TComponent);
begin
	inherited;
	FDebugFlags := [ dfDumpObjects, dfDumpRequest ];
	FJsSource := TStringList.Create;
	FPhpSource := TStringList.Create;
	FPreviewOnPublish := true;
	FShowCaptions := true;
	FShowComponents := true;
	FShowGrid := true;
	Name := 'Page';
end;

destructor TPhpPage.Destroy;
begin
	FPhpSource.Free;
	FJsSource.Free;
	inherited;
end;

procedure TPhpPage.SetName(const NewName: TComponentName);
begin
	FNewName := NewName;
	if Assigned(OnNameChange) then
		OnNameChange(Self);
	inherited;
end;

procedure TPhpPage.SetPhpSource(const Value: TStrings);
begin
	FPhpSource.Assign(Value);
end;

procedure TPhpPage.SetJsSource(const Value: TStrings);
begin
	FJsSource.Assign(Value);
end;

function TPhpPage.GetAppTag: string;
begin
	with TThTag.Create('meta') do
	try
		Add('tpOnGenerate', OnGenerate);
		Add('tpOnBeforeClicks', OnBeforeClicks);
		Add('tpOnBeforeInput', OnBeforeInput);
		Attributes.Add('tpDebug', Debug);
		Add('tpDebugFlags', PByte(@DebugFlags)^);
		Attributes.Add('tpIsModule', IsModule);
		if Attributes.Count = 0 then
			Result := ''
		else begin
			Add('tpClass', 'TTpPage');
			Add('tpName', 'page');
			Result := Html;
		end;
	finally
		Free;
	end;
end;

function TPhpPage.GetJsTag(const inSrc: string): string;
begin
	with TThTag.Create('script') do
	try
		Mono := false;
		Add('language', 'javascript');
		Add('type', 'text/javascript');
		Add('src', inSrc);
		Result := Html;
	finally
		Free;
	end;
end;

procedure TPhpPage.GenerateHeaders;
begin
	inherited;
	StructuredHtml.Headers.Add(GetAppTag);
	StructuredHtml.Headers.Add(GetJsTag(JsUrl))
end;

function TPhpPage.GetStyleTag(const inHref: string): string;
begin
	with TThTag.Create('link') do
	try
		Add('rel', 'stylesheet');
		Add('type', 'text/css');
		Add('href', inHref);
		Result := Html;
	finally
		Free;
	end;
end;

procedure TPhpPage.GenerateStyles;
begin
	StructuredHtml.Headers.Add(GetStyleTag(StyleUrl))
end;

procedure TPhpPage.SetShowCaptions(const Value: Boolean);
begin
	FShowCaptions := Value;
	Change;
end;

procedure TPhpPage.SetShowComponents(const Value: Boolean);
begin
	FShowComponents := Value;
	Change;
end;

procedure TPhpPage.SetShowGrid(const Value: Boolean);
begin
	FShowGrid := Value;
	Change;
end;

initialization
	RegisterClass(TPhpPage);
	RegisterClass(TPhpForm);
end.
