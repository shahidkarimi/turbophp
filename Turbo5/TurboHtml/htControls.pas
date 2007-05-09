unit htControls;

interface

uses
	SysUtils, Types, Classes, Controls, Graphics,
	LrControls,
	htInterfaces, htMarkup, htStyle, htJavaScript;

type
	ThtGraphicControl = class(TLrGraphicControl, IhtControl)
	private
		FCtrlStyle: ThtStyle;
		FExtraAttributes: string;
		FJavaScript: ThtJavaScriptEvents;
		FOutline: Boolean;
		FStyle: ThtStyle;
		FStyleClass: string;
	protected
		function GetCtrlStyle: ThtStyle;
		function GetStyle: ThtStyle;
		function GetStyleClass: string;
		function GetBoxHeight: Integer; virtual;
		function GetBoxRect: TRect; virtual;
		function GetBoxWidth: Integer; virtual;
		//:$ <br>Build the Style property.
		//:: Builds the read-only Style property by combining the SheetStyle,
		//:: the Style, and the page default styles.
		procedure BuildStyle; virtual;
		procedure Generate(const inContainer: string;
			inMarkup: ThtMarkup); virtual;
		procedure GenerateStyle(const inSelector: string; inMarkup: ThtMarkup);
		procedure Paint; override;
		procedure SetExtraAttributes(const Value: string);
		procedure SetJavaScript(const Value: ThtJavaScriptEvents);
		procedure SetOutline(const Value: Boolean);
		procedure SetStyle(const Value: ThtStyle);
		procedure SetStyleClass(const Value: string);
		procedure StyleChange(inSender: TObject);
		procedure StyleControl; virtual;
		procedure StylePaint; virtual;
		procedure UpdateStyle;
		//:$ <br>Local style properties for the control.
		//:: The CtrlStyle for the control is a combination of the SheetStyle
		//:: and the Style. Style properties take precedence over
		//:: SheetStyle style properties.
		property Style: ThtStyle read GetStyle write SetStyle;
		//:$ <br>The CSS class to assign to this control.
		//:: If the specified class belongs to a StyleSheet on the same page,
		//:: the control is painted using the specified style.
		//:: The final control for the style (the CtrlStyle) is a combination of
		//:: the SheetStyle and the Style. Style property takes precendence.
		property StyleClass: string read GetStyleClass write SetStyleClass;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		//:$ <br>Combined style properties for the control.
		//:: This is the style resulting from a blend of the default styles,
		//:: the SheetStyle, and the Style.
		property CtrlStyle: ThtStyle read GetCtrlStyle;
		property Outline: Boolean read FOutline write SetOutline;
		property BoxHeight: Integer read GetBoxHeight;
		property BoxRect: TRect read GetBoxRect;
		property BoxWidth: Integer read GetBoxWidth;
	published
		property ExtraAttributes: string read FExtraAttributes
			write SetExtraAttributes;
		property JavaScript: ThtJavaScriptEvents read FJavaScript write
			SetJavaScript;
	end;
	//
	ThtCustomControl = class(TLrCustomControl, IhtControl)
	private
		FCtrlStyle: ThtStyle;
		FExtraAttributes: string;
		FJavaScript: ThtJavaScriptEvents;
		FOutline: Boolean;
		FStyle: ThtStyle;
		FStyleClass: string;
	protected
		function GetCtrlStyle: ThtStyle;
		function GetStyle: ThtStyle;
		function GetStyleClass: string;
		function GetBoxHeight: Integer; virtual;
		function GetBoxRect: TRect; virtual;
		function GetBoxWidth: Integer; virtual;
		//:$ <br>Build the Style property.
		//:: Builds the read-only Style property by combining the SheetStyle,
		//:: the Style, and the page default styles.
		procedure BuildStyle; virtual;
		procedure Generate(const inContainer: string;
			inMarkup: ThtMarkup); virtual;
		procedure GenerateStyle(const inSelector: string; inMarkup: ThtMarkup);
		procedure Paint; override;
		procedure PaintBackground(inCanvas: TCanvas; inOffset: TPoint);
		procedure SetExtraAttributes(const Value: string);
		procedure SetJavaScript(const Value: ThtJavaScriptEvents);
		procedure SetOutline(const Value: Boolean);
		procedure SetStyle(const Value: ThtStyle);
		procedure SetStyleClass(const Value: string);
		procedure StyleChange(inSender: TObject);
		procedure StyleControl; virtual;
		procedure StylePaint; virtual;
		procedure UpdateStyle;
		//:$ <br>Local style properties for the control.
		//:: The CtrlStyle for the control is a combination of the SheetStyle
		//:: and the Style. Style properties take precedence over
		//:: SheetStyle style properties.
		property Style: ThtStyle read GetStyle write SetStyle;
		//:$ <br>The CSS class to assign to this control.
		//:: If the specified class belongs to a StyleSheet on the same page,
		//:: the control is painted using the specified style.
		//:: The final control for the style (the CtrlStyle) is a combination of
		//:: the SheetStyle and the Style. Style property takes precendence.
		property StyleClass: string read GetStyleClass write SetStyleClass;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		//:$ <br>Combined style properties for the control.
		//:: This is the style resulting from a blend of the default styles,
		//:: the SheetStyle, and the Style.
		property CtrlStyle: ThtStyle read GetCtrlStyle;
		property Outline: Boolean read FOutline write SetOutline;
		property BoxHeight: Integer read GetBoxHeight;
		property BoxRect: TRect read GetBoxRect;
		property BoxWidth: Integer read GetBoxWidth;
	published
		property ExtraAttributes: string read FExtraAttributes
			write SetExtraAttributes;
		property JavaScript: ThtJavaScriptEvents read FJavaScript write
			SetJavaScript;
	end;

implementation

uses
	LrVclUtils, htUtils, htPaint, htStylePainter;

type
	TCrackedControl = class(TControl);

{ ThtGraphicControl }

constructor ThtGraphicControl.Create(inOwner: TComponent);
begin
	inherited;
	FJavaScript := ThtJavaScriptEvents.Create(Self);
	FStyle := ThtStyle.Create(Self);
	FStyle.OnChange := StyleChange;
	FCtrlStyle := ThtStyle.Create(Self);
	BuildStyle;
	SetBounds(0, 0, 86, 64);
end;

destructor ThtGraphicControl.Destroy;
begin
	FCtrlStyle.Free;
	FStyle.Free;
	FJavaScript.Free;
	inherited;
end;

function ThtGraphicControl.GetStyle: ThtStyle;
begin
	Result := FStyle;
end;

function ThtGraphicControl.GetStyleClass: string;
begin
	Result := FStyleClass;
end;

function ThtGraphicControl.GetCtrlStyle: ThtStyle;
begin
	Result := FCtrlStyle;
end;

procedure ThtGraphicControl.SetStyle(const Value: ThtStyle);
begin
	FStyle.Assign(Value);
	UpdateStyle;
end;

procedure ThtGraphicControl.SetStyleClass(const Value: string);
begin
	FStyleClass := Value;
	UpdateStyle;
end;

procedure ThtGraphicControl.SetOutline(const Value: Boolean);
begin
	FOutline := Value;
	Invalidate;
end;

procedure ThtGraphicControl.SetExtraAttributes(const Value: string);
begin
	FExtraAttributes := Value;
end;

procedure ThtGraphicControl.SetJavaScript(const Value: ThtJavaScriptEvents);
begin
	FJavaScript.Assign(Value);
end;

procedure ThtGraphicControl.BuildStyle;
begin
	CtrlStyle.Assign(Style);
	//
	//CtrlStyle.Inherit(SheetStyle);
	//CtrlStyle.Font.Inherit(PageStyle.Font);
	//
	//if not htVisibleColor(Color) {and DesignOpaque} and (Parent <> nil) then
	//	Color := TCrackedControl(Parent).Color;
end;

procedure ThtGraphicControl.StyleControl;
begin
	CtrlStyle.Font.ToFont(Font);
	Canvas.Font := Font;
	Color := CtrlStyle.Color;
end;

procedure ThtGraphicControl.UpdateStyle;
begin
	BuildStyle;
	StyleControl;
	Invalidate;
	AdjustSize;
	//Realign;
end;

procedure ThtGraphicControl.StyleChange(inSender: TObject);
begin
	UpdateStyle;
end;

function ThtGraphicControl.GetBoxRect: TRect;
begin
	Result := CtrlStyle.UnboxRect(ClientRect);
end;

function ThtGraphicControl.GetBoxHeight: Integer;
begin
	Result := ClientHeight - CtrlStyle.GetBoxHeightMargin;
end;

function ThtGraphicControl.GetBoxWidth: Integer;
begin
	Result := ClientWidth - CtrlStyle.GetBoxWidthMargin;
end;

procedure ThtGraphicControl.Paint;
begin
	inherited;
	StylePainter.Prepare(Canvas, Color, CtrlStyle, ClientRect);
	StylePaint;
end;

procedure ThtGraphicControl.StylePaint;
begin
	StylePainter.PaintBackground;
	StylePainter.PaintBorders;
	if Outline then
		htPaintOutline(Canvas, ClientRect);
end;

procedure ThtGraphicControl.GenerateStyle(const inSelector: string;
	inMarkup: ThtMarkup);
var
	s: string;
begin
	s := Style.InlineAttribute;
	if (s <> '') then
		inMarkup.Styles.Add(Format('%s { %s }', [ inSelector, s ]));
end;

procedure ThtGraphicControl.Generate(const inContainer: string;
	inMarkup: ThtMarkup);
begin
	//
end;

{ ThtCustomControl }

constructor ThtCustomControl.Create(inOwner: TComponent);
begin
	inherited;
	Transparent := true;
	FJavaScript := ThtJavaScriptEvents.Create(Self);
	FStyle := ThtStyle.Create(Self);
	FStyle.OnChange := StyleChange;
	FCtrlStyle := ThtStyle.Create(Self);
	BuildStyle;
	SetBounds(0, 0, 86, 64);
end;

destructor ThtCustomControl.Destroy;
begin
	FCtrlStyle.Free;
	FStyle.Free;
	FJavaScript.Free;
	inherited;
end;

function ThtCustomControl.GetStyle: ThtStyle;
begin
	Result := FStyle;
end;

function ThtCustomControl.GetStyleClass: string;
begin
	Result := FStyleClass;
end;

function ThtCustomControl.GetCtrlStyle: ThtStyle;
begin
	Result := FCtrlStyle;
end;

procedure ThtCustomControl.SetStyle(const Value: ThtStyle);
begin
	FStyle.Assign(Value);
	UpdateStyle;
end;

procedure ThtCustomControl.SetStyleClass(const Value: string);
begin
	FStyleClass := Value;
	UpdateStyle;
end;

procedure ThtCustomControl.SetOutline(const Value: Boolean);
begin
	FOutline := Value;
	Invalidate;
end;

procedure ThtCustomControl.SetExtraAttributes(const Value: string);
begin
	FExtraAttributes := Value;
end;

procedure ThtCustomControl.SetJavaScript(const Value: ThtJavaScriptEvents);
begin
	FJavaScript.Assign(Value);
end;

procedure ThtCustomControl.BuildStyle;
begin
	CtrlStyle.Assign(Style);
	//
	//CtrlStyle.Inherit(SheetStyle);
	//CtrlStyle.Font.Inherit(PageStyle.Font);
	//
	//	if not htVisibleColor(Color) and not Transparent and (Parent <> nil) then
	//		Color := TCrackedControl(Parent).Color;
end;

procedure ThtCustomControl.StyleControl;
begin
	CtrlStyle.Font.ToFont(Font);
	Canvas.Font := Font;
	Color := CtrlStyle.Color;
end;

procedure ThtCustomControl.UpdateStyle;
begin
	BuildStyle;
	StyleControl;
	Invalidate;
	AdjustSize;
	Realign;
end;

procedure ThtCustomControl.StyleChange(inSender: TObject);
begin
	UpdateStyle;
end;

function ThtCustomControl.GetBoxRect: TRect;
begin
	Result := CtrlStyle.UnboxRect(ClientRect);
end;

function ThtCustomControl.GetBoxHeight: Integer;
begin
	Result := ClientHeight - CtrlStyle.GetBoxHeightMargin;
end;

function ThtCustomControl.GetBoxWidth: Integer;
begin
	Result := ClientWidth - CtrlStyle.GetBoxWidthMargin;
end;

procedure ThtCustomControl.Paint;
begin
	inherited;
	PaintBackground(Canvas, Point(0, 0));
	StylePainter.Prepare(Canvas, Color, CtrlStyle, ClientRect);
	StylePaint;
end;

procedure ThtCustomControl.PaintBackground(inCanvas: TCanvas; inOffset: TPoint);
var
	c: ThtCustomControl;
	r: TRect;
begin
	r := ClientRect;
	OffsetRect(r, -inOffset.X, -inOffset.Y);
	StylePainter.Prepare(inCanvas, Color, CtrlStyle, r);
	//
	if htVisibleColor(Color) or CtrlStyle.Background.Picture.HasGraphic then
		StylePainter.PaintBackground
	else if (Parent is ThtCustomControl) then
	begin
		c := ThtCustomControl(Parent);
		inOffset := Point(inOffset.X + Left, inOffset.Y + Top);
		c.PaintBackground(inCanvas, inOffset);
	end else
	begin
		if (Parent <> nil) then
			StylePainter.Color := TCrackedControl(Parent).Color
		else
			StylePainter.Color := clWhite;
		StylePainter.PaintBackground;
	end;
end;

procedure ThtCustomControl.StylePaint;
begin
	StylePainter.PaintBorders;
	if Outline then
		htPaintOutline(Canvas, ClientRect);
end;

procedure ThtCustomControl.GenerateStyle(const inSelector: string;
	inMarkup: ThtMarkup);
var
	s: string;
begin
	s := Style.InlineAttribute;
	if (s <> '') then
		inMarkup.Styles.Add(Format('%s { %s }', [ inSelector, s ]));
end;

procedure ThtCustomControl.Generate(const inContainer: string;
	inMarkup: ThtMarkup);
begin
	//
end;

end.
