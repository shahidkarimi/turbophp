unit ThHtmlPage;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls,
	ThInterfaces, ThCssStyle, ThStyledCtrl, ThHtmlDocument, ThHeaderComponent,
	ThPanel, ThStructuredHtml, ThStyleSheet, ThAnchorStyles, ThMessages,
	ThJavaScript;

type
	TThPageLayout = ( plFixedLeft, plCenter, plElastic );
	//
	TThHtmlPage = class(TComponent, IThJavaScriptable)
	private
    FAnchorStyles: TThAnchorStyles;
		FDocument: TThHtmlDocument;
		FEmitHeaders: Boolean;
		FExtraHeaders: TStringList;
		FHeight: Integer;
		FJavaScript: TThJavaScriptEvents;
		FMarginLeft: Integer;
		FMarginTop: Integer;
		FOnChange: TNotifyEvent;
		FPageLayout: TThPageLayout;
		FPageTitle: string;
		FPanel: TThPanel;
		FStructuredHtml: TThStructuredHtml;
		FStyle: TThCssStyle;
		FUseMargins: Boolean;
		FWidth: Integer;
		FStyleSheet: TThStyleSheet;
    function GetBoundsRect: TRect;
	protected
		function GetBody: string;
		function GetBodyEnd: string;
		function GetBodyStart: string;
		function GetBodyStyle: string;
		function GetClientRect: TRect;
		function GetContainer: TWinControl;
		function GetGeneratorHtml: string;
		function GetHtml: string;
		function GetInlineBodyStyle: string;
		function GetJavaScript: TThJavaScriptEvents;
		function GetTitle: string;
		procedure SetAnchorStyles(const Value: TThAnchorStyles);
		procedure SetExtraHeaders(const Value: TStringList);
		procedure SetHeight(const Value: Integer);
		procedure SetJavaScript(const Value: TThJavaScriptEvents);
		procedure SetMarginLeft(const Value: Integer);
		procedure SetMarginTop(const Value: Integer);
		procedure SetPageLayout(const Value: TThPageLayout);
		procedure SetPageTitle(const Value: string);
		procedure SetPanel(const Value: TThPanel);
		procedure SetStyle(const Value: TThCssStyle);
		procedure SetStyleSheet(const Value: TThStyleSheet);
		procedure SetUseMargins(const Value: Boolean);
		procedure SetWidth(const Value: Integer);
	protected
		procedure CreateJavaScript; virtual;
		function GeneratePageTable(const inContent: string): string;
		procedure GenerateHeaders; virtual;
		procedure GenerateStyles; virtual;
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure Change;
		procedure PublishComponents(inContainer: TWinControl);
		procedure SetDefaultMargins;
		procedure StyleChanged(inSender: TObject);
	public
		property BoundsRect: TRect read GetBoundsRect;
		property ClientRect: TRect read GetClientRect;
		property Container: TWinControl read GetContainer;
		property Document: TThHtmlDocument read FDocument;
		property Html: string read GetHtml;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
		property StructuredHtml: TThStructuredHtml read FStructuredHtml;
	published
		property AnchorStyles: TThAnchorStyles read FAnchorStyles
			write SetAnchorStyles;
		property EmitHeaders: Boolean read FEmitHeaders write FEmitHeaders
			default true;
		property ExtraHeaders: TStringList read FExtraHeaders write SetExtraHeaders;
		property JavaScript: TThJavaScriptEvents read GetJavaScript
			write SetJavaScript;
		property MarginLeft: Integer read FMarginLeft write SetMarginLeft
			default 10;
		property MarginTop: Integer read FMarginTop write SetMarginTop
			default 16;
		property Height: Integer read FHeight write SetHeight
			default 800;
		property UseMargins: Boolean read FUseMargins write SetUseMargins
			default false;
		property Width: Integer read FWidth write SetWidth
			default 600;
		property PageLayout: TThPageLayout read FPageLayout write SetPageLayout;
		property PageTitle: string read FPageTitle write SetPageTitle;
		property Panel: TThPanel read FPanel write SetPanel;
		property Style: TThCssStyle read FStyle write SetStyle;
		property StyleSheet: TThStyleSheet read FStyleSheet write SetStyleSheet;
	end;

function ThFindHtmlPage(inCtrl: TControl): TThHtmlPage;

implementation

uses
	ThVclUtils, ThComponentIterator, ThTag, ThGenerator;

	function ThFindHtmlPage(inCtrl: TControl): TThHtmlPage;
	begin
		ThFindElderComponent(Result, inCtrl, TThHtmlPage);
	end;

{ TThHtmlPage }

constructor TThHtmlPage.Create(AOwner: TComponent);
begin
	inherited;
	FWidth := 600;
	FHeight := 800;
	FStructuredHtml := TThStructuredHtml.Create;
	FStyle := TThCssStyle.Create(Self);
	FStyle.OnChanged := StyleChanged;
	FExtraHeaders := TStringList.Create;
	FEmitHeaders := true;
	FAnchorStyles := TThAnchorStyles.Create;
	CreateJavaScript;
	SetDefaultMargins;
end;

destructor TThHtmlPage.Destroy;
begin
	FAnchorStyles.Free;
	FExtraHeaders.Free;
	FJavaScript.Free;
	FStructuredHtml.Free;
	FStyle.Free;
	inherited;
end;

procedure TThHtmlPage.CreateJavaScript;
begin
	FJavaScript := TThJavaScriptEvents.Create(Self);
	FJavaScript.Clear;
	FJavaScript.AddEvent.EventName := 'onBlur';
	FJavaScript.AddEvent.EventName := 'onFocus';
	FJavaScript.AddEvent.EventName := 'onLoad';
	FJavaScript.AddEvent.EventName := 'onUnload';
end;

procedure TThHtmlPage.Change;
begin
	if Assigned(OnChange) then
		OnChange(Self);
end;

function TThHtmlPage.GetTitle: string;
begin
	if PageTitle <> '' then
		Result := PageTitle
	else
		Result := TPanel(Container).Caption;
end;

function TThHtmlPage.GeneratePageTable(const inContent: string): string;
begin
	with TThTag.Create('table') do
	try
		Add('border', 0);
		Add('cellspacing', 0);
		Add('cellpadding', 0);
		case PageLayout of
			plElastic: Add('width', '100%');
			else Add('width', GetContainer.Width);
		end;
		if (PageLayout = plCenter) then
			Add('align', 'center');
{
		if PageLayout <> plElastic then
		begin
			Add('width', GetContainer.Width);
			if PageLayout = plCenter then
				Add('align', 'center');
		end;
}
		Content := '<tr><td>'#13 + inContent + #13'</td></tr>';
		Result := Html;
	finally
		Free;
	end;
end;

function TThHtmlPage.GetGeneratorHtml: string;
begin
	with TThGenerator.Create(GetContainer) do
	try
		with Margins do
			Margins := Rect(Left + MarginLeft, Top + MarginTop, Right - MarginLeft,
				Bottom - MarginTop);
		Result := Html;
	finally
		Free;
	end;
end;

function TThHtmlPage.GetBody: string;
begin
	if Panel <> nil then
		Result := Panel.HTML
	else
		Result := GetGeneratorHtml;
	//if PageLayout <> plElastic then
		Result := GeneratePageTable(Result);
end;

function TThHtmlPage.GetInlineBodyStyle: string;
begin
	Result := '';
	ThCat(Result, Style.Font.InlineAttribute);
	//ThCat(Result, InlineColorAttribute);
	//ThCat(Result, FBackground.InlineAttribute);
	ThCat(Result, Style.Border.InlineAttribute);
	ThCat(Result, Style.Padding.InlineAttribute);
end;

function TThHtmlPage.GetBodyStyle: string;
begin
	Result := Format(' margin: %dpx %dpx %dpx %dpx',
		[ MarginTop, MarginLeft, MarginTop, MarginLeft ]);
	//Result := 'td, body { '	+ Style.InlineAttribute	+ Result + ' }';
	Result := 'td, body { '	+ GetInlineBodyStyle + Result + ' }';
end;

procedure TThHtmlPage.PublishComponents(inContainer: TWinControl);
var
	p: IThPublishable;
begin
	with TThComponentIterator.Create(inContainer) do
	try
		while Next do
		begin
			if ThIsAs(Component, IThPublishable, p) then
				p.Publish(StructuredHtml);
			if Component is TWinControl then
				PublishComponents(TWinControl(Component));
		end;
	finally
		Free;
	end;
end;

procedure TThHtmlPage.GenerateHeaders;
begin
	StructuredHtml.Title.Text := GetTitle;
	StructuredHtml.Headers.AddStrings(ExtraHeaders);
	StructuredHtml.Styles.Add(GetBodyStyle);
end;

procedure TThHtmlPage.GenerateStyles;
begin
	if StyleSheet <> nil then
	begin
		StyleSheet.Styles.GenerateStyles(StructuredHtml.Styles);
		AnchorStyles.GenerateStyles(StructuredHtml.Styles, StyleSheet);
	end;
end;

function TThHtmlPage.GetBodyStart: string;
begin
	with TThTag.Create('body') do
	try
		Mono := false;
		Attributes.AddColor('bgcolor', Style.Color);
		JavaScript.ListAttributes('body', Attributes);
		Result := OpenTag;
	finally
		Free;
	end;
end;

function TThHtmlPage.GetBodyEnd: string;
begin
	Result := '</body>';
end;

function TThHtmlPage.GetHtml: string;
begin
	StructuredHtml.NewDocument;
	StructuredHtml.BodyOnly := not EmitHeaders;
	if EmitHeaders then
	begin
		GenerateHeaders;
		StructuredHtml.Body.Add(GetBodyStart);
	end;
	StructuredHtml.Body.Add(GetBody);
	if EmitHeaders then
		StructuredHtml.Body.Add(GetBodyEnd);
	PublishComponents(Container);
	GenerateStyles;
	Result := StructuredHtml.PublishToString;
end;

procedure TThHtmlPage.SetDefaultMargins;
begin
	FMarginLeft := 10;
	FMarginTop := 16;
end;

procedure TThHtmlPage.SetExtraHeaders(const Value: TStringList);
begin
	FExtraHeaders.Assign(Value);
end;

procedure TThHtmlPage.SetHeight(const Value: Integer);
begin
	FHeight := Value;
	Change;
end;

procedure TThHtmlPage.SetMarginLeft(const Value: Integer);
begin
	if (FMarginLeft <> Value) then
		UseMargins := true;
	FMarginLeft := Value;
	Change;
end;

procedure TThHtmlPage.SetMarginTop(const Value: Integer);
begin
	if (FMarginTop <> Value) then
		UseMargins := true;
	FMarginTop := Value;
	Change;
end;

procedure TThHtmlPage.SetPageLayout(const Value: TThPageLayout);
begin
	FPageLayout := Value;
end;

procedure TThHtmlPage.SetPageTitle(const Value: string);
begin
	FPageTitle := Value;
end;

procedure TThHtmlPage.SetStyle(const Value: TThCssStyle);
begin
	FStyle.Assign(Value);
	Change;
end;

procedure TThHtmlPage.SetUseMargins(const Value: Boolean);
begin
	FUseMargins := Value;
	if not Value then
		SetDefaultMargins;
	Change;
end;

procedure TThHtmlPage.SetWidth(const Value: Integer);
begin
	FWidth := Value;
	Change;
end;

procedure TThHtmlPage.StyleChanged(inSender: TObject);
begin
	Change;
	if Owner is TWinControl then
		ThNotifyAll(TWinControl(Owner), THM_STYLECHANGE, Style);
end;

procedure TThHtmlPage.SetPanel(const Value: TThPanel);
begin
	if FPanel <> nil then
		FPanel.RemoveFreeNotification(Self);
	FPanel := Value;
	if FPanel <> nil then
		FPanel.FreeNotification(Self);
end;

function TThHtmlPage.GetContainer: TWinControl;
begin
	Result := TWinControl(Self.Owner);
end;

procedure TThHtmlPage.SetAnchorStyles(const Value: TThAnchorStyles);
begin
	FAnchorStyles.Assign(Value);
end;

procedure TThHtmlPage.SetStyleSheet(const Value: TThStyleSheet);
begin
	if FStyleSheet <> nil then
		FStyleSheet.RemoveFreeNotification(Self);
	FStyleSheet := Value;
	if FStyleSheet <> nil then
		FStyleSheet.FreeNotification(Self);
	StyleChanged(Self);
end;

procedure TThHtmlPage.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	if Operation = opRemove then
	begin
		if AComponent = FStyleSheet then
			FStyleSheet := nil
		else if AComponent = FPanel then
			FPanel := nil;
	end;
end;

function TThHtmlPage.GetClientRect: TRect;
begin
	Result := Rect(MarginLeft, MarginTop,
		Width - MarginLeft - MarginLeft, Height - MarginTop - MarginTop);
end;

function TThHtmlPage.GetBoundsRect: TRect;
begin
	Result := Rect(MarginLeft, MarginTop,
		Width - MarginLeft, Height - MarginTop);
end;

procedure TThHtmlPage.SetJavaScript(const Value: TThJavaScriptEvents);
begin
	FJavaScript.Assign(Value);
end;

function TThHtmlPage.GetJavaScript: TThJavaScriptEvents;
begin
	Result := FJavaScript;
end;

end.
