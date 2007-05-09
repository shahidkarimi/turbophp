unit ThStyledCtrl;

interface

uses
	Classes, Controls, Graphics, Messages, Types, ExtCtrls,
	ThMessages, ThCustomCtrl, ThGraphicCtrl, ThCssStyle,
	ThStylePainter;

type
	IThStyled = interface
	['{308F2155-A4B8-4347-B00C-100BF6624BBF}']
		function GetStyle: TThCssStyle;
		function GetStyleClass: string;
		procedure SetStyleClass(const inStyleClass: string);
	end;
	//
	//:$ Ancestor class for all TurboHtml Windowed controls, base class for
	//:$ TThHtmlCustomControl.
	//:: TThStyledCustomControl adds style machinery to TThCustomControl.
	TThStyledCustomControl = class(TThCustomControl, IThStyled)
	private
		FCtrlStyle: TThCssStyle;
		FStyle: TThCssStyle;
		FStyleClass: string;
		FStylePainter: TThStylePainter;
		FDesignOpaque: Boolean;
	protected
		function GetClientRect: TRect; override;
		function GetPageStyle: TThCssStyle; virtual;
		function GetSheetStyle: TThCssStyle; virtual;
		function GetStyle: TThCssStyle;
		function GetStyleClass: string;
		procedure SetDesignOpaque(const Value: Boolean);
		procedure SetStyle(const Value: TThCssStyle);
		procedure SetStyleClass(const Value: string);
	protected
		//:$ <br>Return the rect with margins adjusted by the CtrlStyle.
		procedure AdjustClientRect(var inRect: TRect); override;
		//:$ <br>Build the Style property.
		//:: Builds the read-only Style property by combining the SheetStyle,
		//:: the Style, and the page default styles.
		procedure BuildStyle; virtual;
		procedure CssStyleChanged; virtual;
		procedure Loaded; override;
		//:$ <br>Default painting operations.
		//:: Invokes the Painter object and sets the Canvas' brush color.
		procedure Paint; override;
		procedure StyleChanged(inSender: TObject); 
		procedure ThmStyleChange(var inMessage); message THM_STYLECHANGE;
		procedure ThmUpdatePicture(var inMessage); message THM_UPDATEPICTURE;
	protected
		//:$ <br>The default style for the page containing this control.
		property PageStyle: TThCssStyle read GetPageStyle;
		//:$ <br>Style painter object used for common style painting tasks.
		property Painter: TThStylePainter read FStylePainter;
		//:$ <br>Local CSS style properties for the control.
		//:: The CtrlStyle for the control is a combination of the SheetStyle
		//:: and the Style. Style properties take precedence over
		//:: SheetStyle style properties.
		property Style: TThCssStyle read GetStyle write SetStyle;
		//:$ <br>The CSS class to assign to this control.
		//:: If the specified class belongs to a StyleSheet on the same page,
		//:: the control is painted using the specified style.
		//:: The final control for the style (the CtrlStyle) is a combination of
		//:: the SheetStyle and the Style. Style property takes precendence.
		property StyleClass: string read GetStyleClass write SetStyleClass;
		//:$ <br>The StyleSheet style (or nil) identified by StyleClass.
		property SheetStyle: TThCssStyle read GetSheetStyle;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		//:$ <br>Return the client rect with margins adjusted by the CtrlStyle.
		function AdjustedClientRect: TRect;
		//:$ <br>Return the client height as adjusted by the CtrlStyle.
		function AdjustedClientHeight: Integer;
		//:$ <br>Return the client width as adjusted by the CtrlStyle.
		function AdjustedClientWidth: Integer;
	public
		//:$ <br>CSS style properties for the control.
		//:: This is the style resulting from a blend of the default styles,
		//:: the SheetStyle, and the Style.
		property CtrlStyle: TThCssStyle read FCtrlStyle;
		//:$ <br>Set true for opaque painting at design time.
		property DesignOpaque: Boolean read FDesignOpaque write SetDesignOpaque;
	end;
	//
	//:$ Ancestor class for all TurboHtml graphic controls, base class for
	//:$ TThHtmlGraphicControl.
	//:: TThStyledGraphicControl adds style machinery to TThGraphicControl.
	TThStyledGraphicControl = class(TThGraphicControl, IThStyled)
	private
		FCtrlStyle: TThCssStyle;
		FStyle: TThCssStyle;
		FStyleClass: string;
		FStylePainter: TThStylePainter;
		FDesignOpaque: Boolean;
    function GetStyle: TThCssStyle;
    function GetStyleClass: string;
	protected
		function GetPageStyle: TThCssStyle; virtual;
		function GetSheetStyle: TThCssStyle; virtual;
		procedure SetDesignOpaque(const Value: Boolean); virtual;
		procedure SetStyle(const Value: TThCssStyle); virtual;
		procedure SetStyleClass(const Value: string); virtual;
	protected
		//:$ <br>Return the rect with margins adjusted by the CtrlStyle.
		procedure AdjustClientRect(var inRect: TRect);
		//:$ <br>Build the Style property.
		//:: Builds the read-only Style property by combining the SheetStyle,
		//:: the Style, and the page default styles.
		procedure BuildStyle; virtual;
		procedure CssStyleChanged; virtual;
		procedure Loaded; override;
		//:$ <br>Default painting operations.
		//:: Invokes the Painter object and sets the Canvas' brush color.
		procedure Paint; override;
		procedure SetParent({$ifdef __ThClx__}const{$endif}
			inParent: TWinControl); override;
		procedure StyleChanged(inSender: TObject);
		procedure ThmStyleChange(var inMessage); message ThM_STYLECHANGE;
		procedure ThmUpdatePicture(var inMessage); message THM_UPDATEPICTURE;
	protected
		//:$ <br>The default style for the page containing this control.
		property PageStyle: TThCssStyle read GetPageStyle;
		//:$ <br>Style painter object used for common style painting tasks.
		property Painter: TThStylePainter read FStylePainter;
		//:$ <br>Local CSS style properties for the control.
		//:: The CtrlStyle for the control is a combination of the SheetStyle
		//:: and the Style. Style properties take precedence over
		//:: SheetStyle style properties.
		property Style: TThCssStyle read GetStyle write SetStyle;
		//:$ <br>The CSS class to assign to this control.
		//:: If the specified class belongs to a StyleSheet on the same page,
		//:: the control is painted using the specified style.
		//:: The final control for the style (the CtrlStyle) is a combination of
		//:: the SheetStyle and the Style. Style property takes precendence.
		property StyleClass: string read GetStyleClass write SetStyleClass;
		//:$ <br>The StyleSheet style (or nil) identified by StyleClass.
		property SheetStyle: TThCssStyle read GetSheetStyle;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		//:$ <br>Return the client rect with margins adjusted by the CtrlStyle.
		function AdjustedClientRect: TRect;
		//:$ <br>Return the client height as adjusted by the CtrlStyle.
		function AdjustedClientHeight: Integer;
		//:$ <br>Return the client width as adjusted by the CtrlStyle.
		function AdjustedClientWidth: Integer;
	public
		//:$ <br>CSS style properties for the control.
		//:: This is the style resulting from a blend of the default styles,
		//:: the SheetStyle, and the Style.
		property CtrlStyle: TThCssStyle read FCtrlStyle;
		//:$ <br>Set true for opaque painting at design time.
		property DesignOpaque: Boolean read FDesignOpaque write SetDesignOpaque;
	end;

implementation

uses
	ThHtmlPage, ThStyleSheet, ThVclUtils;

{ TThStyledCustomControl }

constructor TThStyledCustomControl.Create(AOwner: TComponent);
begin
	inherited;
	Transparent := true;
	FStyle := TThCssStyle.Create(Self);
	FStyle.OnChanged := StyleChanged;
	FCtrlStyle := TThCssStyle.Create(Self);
	FStylePainter := TThStylePainter.Create;
	BuildStyle;
	SetBounds(0, 0, 86, 64);
end;

destructor TThStyledCustomControl.Destroy;
begin
	FStylePainter.Free;
	FCtrlStyle.Free;
	FStyle.Free;
	inherited;
end;

procedure TThStyledCustomControl.Loaded;
begin
	inherited;
	CssStyleChanged;
end;

function TThStyledCustomControl.GetStyle: TThCssStyle;
begin
	Result := FStyle;
end;

function TThStyledCustomControl.GetStyleClass: string;
begin
	Result := FStyleClass;
end;

function TThStyledCustomControl.GetSheetStyle: TThCssStyle;
begin
	Result := ThFindStyleSheetStyle(Self, StyleClass);
end;

function TThStyledCustomControl.GetPageStyle: TThCssStyle;
var
	p: TThHtmlPage;
begin
	p := TThHtmlPage(ThFindElderComponent(Self, TThHtmlPage));
	if (p <> nil) then
		Result := p.Style
	else
		Result := NilStyle;
end;

procedure TThStyledCustomControl.BuildStyle;
begin
	CtrlStyle.Assign(Style);
	CtrlStyle.Inherit(SheetStyle);
	CtrlStyle.Font.Inherit(PageStyle.Font);
	CtrlStyle.Font.ToFont(Font);
	Canvas.Font := Font;
	Color := CtrlStyle.Color;
	if not ThVisibleColor(Color) and DesignOpaque then
		if (Parent <> nil) then
			Color := TPanel(Parent).Color;
end;

procedure TThStyledCustomControl.CssStyleChanged;
begin
	BuildStyle;
	Invalidate;
	AdjustSize;
	Realign;
end;

procedure TThStyledCustomControl.StyleChanged(inSender: TObject);
begin
	CssStyleChanged;
end;

procedure TThStyledCustomControl.ThmStyleChange(var inMessage);
begin
	CssStyleChanged;
end;

procedure TThStyledCustomControl.ThmUpdatePicture(var inMessage);
begin
	Style.Background.Picture.ResolvePicturePath;
	CssStyleChanged;
end;

procedure TThStyledCustomControl.SetStyle(const Value: TThCssStyle);
begin
	FStyle.Assign(Value);
	CssStyleChanged;
end;

procedure TThStyledCustomControl.SetStyleClass(const Value: string);
begin
	FStyleClass := Value;
	CssStyleChanged;
end;

function TThStyledCustomControl.GetClientRect: TRect;
begin
	Result := inherited GetClientRect;
	//AdjustClientRect(Result);
end;

procedure TThStyledCustomControl.AdjustClientRect(var inRect: TRect);
begin
	CtrlStyle.AdjustClientRect(inRect);
end;

function TThStyledCustomControl.AdjustedClientWidth: Integer;
begin
	Result := CtrlStyle.AdjustWidth(ClientWidth);
end;

function TThStyledCustomControl.AdjustedClientHeight: Integer;
begin
	Result := CtrlStyle.AdjustHeight(ClientHeight);
end;

function TThStyledCustomControl.AdjustedClientRect: TRect;
begin
	Result := ClientRect;
	AdjustClientRect(Result);
end;

procedure TThStyledCustomControl.Paint;
begin
	inherited;
	Painter.Prepare(Color, CtrlStyle, Canvas, Rect(0, 0, Width, Height));
	Painter.PaintBackground;
	Painter.PaintBorders;
	Canvas.Font := Font;
	Canvas.Brush.Color := Color;
end;

procedure TThStyledCustomControl.SetDesignOpaque(const Value: Boolean);
begin
	FDesignOpaque := Value;
	BuildStyle;
	Invalidate;
end;

{ TThStyledGraphicControl }

constructor TThStyledGraphicControl.Create(AOwner: TComponent);
begin
	inherited;
	FStyle := TThCssStyle.Create(Self);
	FStyle.OnChanged := StyleChanged;
	FCtrlStyle := TThCssStyle.Create(Self);
	FStylePainter := TThStylePainter.Create; //(Self);
	//BuildStyle;
	//Width := 86;
	//Height := 64;
	SetBounds(0, 0, 86, 64);
end;

destructor TThStyledGraphicControl.Destroy;
begin
	FStylePainter.Free;
	FStyle.Free;
	FCtrlStyle.Free;
	inherited;
end;

procedure TThStyledGraphicControl.Loaded;
begin
	inherited;
	CssStyleChanged;
end;

procedure TThStyledGraphicControl.SetParent({$ifdef __ThClx__}const{$endif}
	inParent: TWinControl);
begin
	inherited;
	if (inParent <> nil) and not (csDestroying in ComponentState)
		and not (csLoading in ComponentState) then
			CssStyleChanged;
end;

function TThStyledGraphicControl.GetStyle: TThCssStyle;
begin
	Result := FStyle;
end;

function TThStyledGraphicControl.GetStyleClass: string;
begin
	Result := FStyleClass;
end;

function TThStyledGraphicControl.GetSheetStyle: TThCssStyle;
begin
	Result := ThFindStyleSheetStyle(Self, StyleClass);
end;

function TThStyledGraphicControl.GetPageStyle: TThCssStyle;
var
	p: TThHtmlPage;
begin
	p := TThHtmlPage(ThFindElderComponent(Self, TThHtmlPage));
	if (p <> nil) then
		Result := p.Style
	else
		Result := NilStyle;
end;

procedure TThStyledGraphicControl.BuildStyle;
//var
//	p: TWinControl;
begin
	CtrlStyle.Assign(Style);
	CtrlStyle.Inherit(SheetStyle);
	CtrlStyle.Font.Inherit(PageStyle.Font);
	CtrlStyle.Font.ToFont(Font);
	Canvas.Font := Font;
	Color := CtrlStyle.Color;
	//
{
	p := Parent;
	while (p <> nil) and not ThVisibleColor(Color) and DesignOpaque do
	begin
		Color := TPanel(p).Color;
		p := p.Parent;
	end;
}
	if not ThVisibleColor(Color) and DesignOpaque then
		if (Parent <> nil) then
			Color := TPanel(Parent).Color;
end;

procedure TThStyledGraphicControl.CssStyleChanged;
begin
	Invalidate;
	BuildStyle;
	AdjustSize;
end;

procedure TThStyledGraphicControl.StyleChanged(inSender: TObject);
begin
	CssStyleChanged;
end;

procedure TThStyledGraphicControl.ThmStyleChange(var inMessage);
begin
	CssStyleChanged;
end;

procedure TThStyledGraphicControl.ThmUpdatePicture(var inMessage);
begin
	Style.Background.Picture.ResolvePicturePath;
	CssStyleChanged;
end;

procedure TThStyledGraphicControl.SetStyle(const Value: TThCssStyle);
begin
	FStyle.Assign(Value);
	CssStyleChanged;
end;

procedure TThStyledGraphicControl.SetStyleClass(const Value: string);
begin
	FStyleClass := Value;
	CssStyleChanged;
end;

procedure TThStyledGraphicControl.Paint;
begin
	Painter.Prepare(Color, CtrlStyle, Canvas, Rect(0, 0, Width, Height));
	Painter.PaintBackground;
	Painter.PaintBorders;
	Canvas.Brush.Color := Color;
end;

procedure TThStyledGraphicControl.AdjustClientRect(var inRect: TRect);
begin
	CtrlStyle.AdjustClientRect(inRect);
end;

function TThStyledGraphicControl.AdjustedClientWidth: Integer;
begin
	Result := CtrlStyle.AdjustWidth(ClientWidth);
end;

function TThStyledGraphicControl.AdjustedClientHeight: Integer;
begin
	Result := CtrlStyle.AdjustHeight(ClientHeight);
end;

function TThStyledGraphicControl.AdjustedClientRect: TRect;
begin
	Result := ClientRect;
	AdjustClientRect(Result);
end;

procedure TThStyledGraphicControl.SetDesignOpaque(const Value: Boolean);
begin
	FDesignOpaque := Value;
	BuildStyle;
	Invalidate;
end;

end.
