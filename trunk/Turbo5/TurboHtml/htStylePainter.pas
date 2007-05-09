unit htStylePainter;

interface

uses
	Windows, Types, Graphics,
	htStyle;

type
	ThtStylePainter = class
	private
		FPaintRect: TRect;
		FCanvas: TCanvas;
		FStyle: ThtStyle;
		FColor: TColor;
	protected
		procedure Borders4(inSide: ThtBorderSide; inL, inT, inR,
			inB: TColor);
		procedure PaintBorder(inSide: ThtBorderSide);
		procedure PaintDashEdge(inWidth: Integer; inSide: ThtBorderSide);
		procedure PaintDotEdge(inWidth: Integer; inSide: ThtBorderSide);
		procedure PaintDoubleEdge(inWidth: Integer; inSide: ThtBorderSide);
		procedure PaintInsetEdge(inWidth: Integer; inSide: ThtBorderSide);
		procedure PaintOutsetEdge(inWidth: Integer; inSide: ThtBorderSide);
		procedure PaintRectEdge(inStyle: TPenStyle; inWidth: Integer;
			inSide: ThtBorderSide);
	public
		constructor Create;
		procedure PaintBackground;
		procedure PaintBorders;
		procedure PaintColorBackground;
		procedure PaintInsetBorders;
		procedure PaintOutsetBorders;
		procedure PaintPictureBackground;
		procedure Prepare(inCanvas: TCanvas; inColor: TColor; inStyle: ThtStyle;
			const inRect: TRect);
	public
		property Color: TColor read FColor write FColor;
		property Canvas: TCanvas read FCanvas write FCanvas;
		property PaintRect: TRect read FPaintRect write FPaintRect;
		property Style: ThtStyle read FStyle write FStyle;
	end;

var
	StylePainter: ThtStylePainter;

implementation

uses
	LrVclUtils, LrGraphics, htUtils, htInterfaces;

	function htGetRectEdge(const inRect: TRect; inSide: ThtBorderSide;
		inWidth: Integer = 0): TRect;
	begin
		with inRect do
			case inSide of
				bsLeft:
					Result := Rect(Left, Top, Left, Bottom-1);
				bsTop:
					Result := Rect(Left, Top, Right-1, Top);
				bsRight:
					Result := Rect(Right-inWidth-1, Top, Right-inWidth-1,	Bottom-1);
				bsBottom:
					Result := Rect(Left, Bottom-inWidth-1, Right, Bottom-inWidth-1);
			end;
	end;

	procedure htInflateBySide(var ioRect: TRect; inD: Integer;
		inSide: ThtBorderSide);
	begin
		case inSide of
			bsLeft, bsRight: InflateRect(ioRect, inD, 0);
			bsTop, bsBottom: InflateRect(ioRect, 0, inD);
		end;
	end;

constructor ThtStylePainter.Create;
begin
	Color := clDefault;
end;

procedure ThtStylePainter.PaintDotEdge(inWidth: Integer;
	inSide: ThtBorderSide);
var
	r: TRect;
	x, y, dx, dy: Single;
	l, c, i: Integer;
begin
	r := PaintRect;
	InflateRect(r, -inWidth div 2, -inWidth div 2);
	r := htGetRectEdge(r, inSide);
	x := r.Left;
	dx := r.Right - x;
	y := r.Top;
	dy := r.Bottom - y;
	l := LrMax(Round(dx), Round(dy));
	c := l div (inWidth * 2);
	dx := dx / c;
	dy := dy / c;
	with Canvas do
	begin
		Pen.Style := psSolid;
		Pen.Width := inWidth;
		for i := 0 to c do
		begin
			MoveTo(Round(x), Round(y));
			LineTo(Round(x)+1, Round(y));
			x := x + dx;
			y := y + dy;
		end;
	end;
end;

procedure ThtStylePainter.PaintDashEdge(inWidth: Integer;
	inSide: ThtBorderSide);
const
	cDash = 6;
	cSpace = 3;
	cBolt = cDash + cSpace;
var
	r: TRect;
	x, y, dx, dy: Single;
	d, s, b: Integer;
	l, c, i, wx, wy: Integer;
begin
	r := PaintRect;
	Inc(r.Bottom);
	Inc(r.Right);
	r := htGetRectEdge(r, inSide, inWidth);
	//
	x := r.Left;
	y := r.Top;
	dx := r.Right - x;
	dy := r.Bottom - y;
	//
	d := inWidth * 2;
	if (d < 4) then
		d := 4;
	s := d div 2;
	b := d + s;
	//
	l := LrMax(Round(dx), Round(dy)) + s;
	c := (l + b div 2) div b;
	//
	if (dx < dy) then
	begin
		wx := inWidth;
		wy := d;
		dx := 0;
		dy := l / c;
	end
	else begin
		wx := d;
		wy := inWidth;
		dx := l / c;
		dy := 0;
	end;
	//
	with Canvas do
	begin
		for i := 0 to c do
		begin
			FillRect(Rect(Round(x), Round(y), Round(x) + wx, Round(y) + wy));
			x := x + dx;
			y := y + dy;
		end;
	end;
end;

procedure ThtStylePainter.PaintRectEdge(inStyle: TPenStyle; inWidth: Integer;
	inSide: ThtBorderSide);
var
	i: Integer;
	e, r: TRect;
begin
	r := PaintRect;
	Canvas.Pen.Style := inStyle;
	for i := 1 to inWidth do
	begin
		e := htGetRectEdge(r, inSide);
		Canvas.MoveTo(e.Left, e.Top);
		Canvas.LineTo(e.Right, e.Bottom);
		htInflateBySide(r, -1, inSide);
	end;
end;

procedure ThtStylePainter.PaintOutsetEdge(inWidth: Integer;
	inSide: ThtBorderSide);
begin
	case inSide of
		bsLeft, bsTop: Canvas.Pen.Color := $EEEEEE;
		bsRight, bsBottom: Canvas.Pen.Color := clBtnShadow;
	end;
	PaintRectEdge(psSolid, inWidth, inSide);
end;

procedure ThtStylePainter.PaintInsetEdge(inWidth: Integer;
	inSide: ThtBorderSide);
begin
	case inSide of
		bsLeft, bsTop: Canvas.Pen.Color := clBtnShadow;
		bsRight, bsBottom: Canvas.Pen.Color := $EEEEEE;
	end;
	PaintRectEdge(psSolid, inWidth, inSide);
end;

procedure ThtStylePainter.PaintDoubleEdge(inWidth: Integer;
	inSide: ThtBorderSide);
var
	wp, wx: Integer;
begin
	wp := inWidth div 3;
	wx := inWidth - wp * 3;
	if (wp < 0) then
		PaintRectEdge(psSolid, inWidth, inSide)
	else begin
		PaintRectEdge(psSolid, wp, inSide);
		htInflateBySide(FPaintRect, -wp - wp - wx, inSide);
		PaintRectEdge(psSolid, wp, inSide);
	end;
end;

procedure ThtStylePainter.PaintBorder(inSide: ThtBorderSide);
var
	w: Integer;
	c: TColor;
begin
	w := Style.Border.GetBorderPixels(inSide);
	c := Style.Border.GetBorderColor(inSide);
	Canvas.Pen.Color := c;
	Canvas.Brush.Color := c;
	case Style.Border.GetBorderStyle(inSide) of
		bsSolidBorder: PaintRectEdge(psSolid, w, inSide);
		bsDotted: PaintDotEdge(w, inSide);
		bsDashed: PaintDashEdge(w, inSide);
		bsDouble: PaintDoubleEdge(w, inSide);
		bsOutset: PaintOutsetEdge(w, inSide);
		bsInset: PaintInsetEdge(w, inSide);
	end;
	Canvas.Pen.Width := 1;
end;

procedure ThtStylePainter.Borders4(inSide: ThtBorderSide;
	inL, inT, inR, inB: TColor);
var
	r: TRect;
begin
	with Canvas.Pen do
		case inSide of
			bsLeft, bsTop: Color := inL;
			else Color := inT;
		end;
	r := PaintRect;
	PaintRectEdge(psSolid, 1, inSide);
	InflateRect(FPaintRect, -1, -1);
	with Canvas.Pen do
		case inSide of
			bsLeft, bsTop: Color := inR;
			else Color := inB;
		end;
	PaintRectEdge(psSolid, 1, inSide);
	PaintRect := r;
end;

procedure ThtStylePainter.PaintInsetBorders;
var
	i: ThtBorderSide;
begin
	for i := bsLeft to bsBottom do
		case Style.Border.GetBorderStyle(i) of
			bsDefault: Borders4(i, clBlack, $EEEEEE,	clBtnShadow, Color)
			else PaintBorder(i);
		end;
end;

procedure ThtStylePainter.PaintOutsetBorders;
var
	i: ThtBorderSide;
begin
	for i := bsLeft to bsBottom do
		case Style.Border.GetBorderStyle(i) of
			bsDefault: Borders4(i, clWhite,	clBtnShadow, $EEEEEE, clBlack)
			else PaintBorder(i);
		end;
end;

procedure ThtStylePainter.PaintBorders;
var
	i: ThtBorderSide;
begin
	for i := bsLeft to bsBottom do
		PaintBorder(i);
end;

procedure ThtStylePainter.PaintPictureBackground;
begin
	if Style.Background.Picture.HasGraphic then
		LrTileGraphic(Canvas, PaintRect, Style.Background.Picture.Graphic)
end;

procedure ThtStylePainter.PaintColorBackground;
begin
	if htVisibleColor(Color) then
		with Canvas do
		begin
			Brush.Color := Color;
			FillRect(PaintRect);
		end;
end;

procedure ThtStylePainter.PaintBackground;
begin
	if Style.Background.Picture.HasGraphic then
		PaintPictureBackground
	else
		PaintColorBackground;
end;

procedure ThtStylePainter.Prepare(inCanvas: TCanvas; inColor: TColor;
	inStyle: ThtStyle; const inRect: TRect);
begin
	Canvas := inCanvas;
	Color := inColor;
	Style := inStyle;
	PaintRect := inRect;
end;

initialization
	StylePainter := ThtStylePainter.Create;
finalization
	StylePainter.Free;
end.
