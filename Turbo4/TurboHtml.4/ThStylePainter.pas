unit ThStylePainter;

interface

uses
	Windows, Types, Graphics,
	ThCssStyle;

type
	TThStylePainter = class
	private
		FPaintRect: TRect;
		FCanvas: TCanvas;
		FStyle: TThCssStyle;
		FColor: TColor;
	protected
		procedure PaintRectEdge(inStyle: TPenStyle; inWidth: Integer;
			inSide: TThCssBorderSide);
		procedure PaintDotEdge(inWidth: Integer; inSide: TThCssBorderSide);
		procedure PaintDashEdge(inWidth: Integer; inSide: TThCssBorderSide);
		procedure PaintBorder(inSide: TThCssBorderSide);
		procedure Borders4(inSide: TThCssBorderSide; inL, inT, inR,
			inB: TColor);
	public
		constructor Create;
		procedure PaintBackground;
		procedure PaintBorders;
		procedure PaintColorBackground;
		procedure PaintInsetBorders;
		procedure PaintOutsetBorders;
		procedure PaintPictureBackground;
		procedure Prepare(inColor: TColor; inStyle: TThCssStyle; inCanvas: TCanvas;
			const inRect: TRect);
	public
		property Color: TColor read FColor write FColor;
		property Canvas: TCanvas read FCanvas write FCanvas;
		property PaintRect: TRect read FPaintRect write FPaintRect;
		property Style: TThCssStyle read FStyle write FStyle;
	end;

var
	StylePainter: TThStylePainter;

implementation

uses
	ThGraphicUtils;

	function TbhMax(inA, inB: Integer): Integer;
	begin
		if inA >= inB then
			Result := inA
		else
			Result := inB;
	end;

	function TbhGetRectEdge(const inRect: TRect;
		inSide: TThCssBorderSide; inWidth: Integer = 0): TRect;
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

	procedure TbhInflateBySide(var ioRect: TRect; inD: Integer;
		inSide: TThCssBorderSide);
	begin
		case inSide of
			bsLeft, bsRight: InflateRect(ioRect, inD, 0);
			bsTop, bsBottom: InflateRect(ioRect, 0, inD);
		end;
	end;

constructor TThStylePainter.Create;
begin
	Color := clDefault;
end;

procedure TThStylePainter.PaintDotEdge(inWidth: Integer;
	inSide: TThCssBorderSide);
var
	r: TRect;
	x, y, dx, dy: Single;
	l, c, i: Integer;
begin
	r := PaintRect;
	InflateRect(r, -inWidth div 2, -inWidth div 2);
	r := TbhGetRectEdge(r, inSide);
	x := r.Left;
	dx := r.Right - x;
	y := r.Top;
	dy := r.Bottom - y;
	l := TbhMax(Round(dx), Round(dy));
	c := l div (inWidth + inWidth);
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

procedure TThStylePainter.PaintDashEdge(inWidth: Integer;
	inSide: TThCssBorderSide);
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
	r := TbhGetRectEdge(r, inSide, inWidth);
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
	l := TbhMax(Round(dx), Round(dy)) + s;
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

procedure TThStylePainter.PaintRectEdge(inStyle: TPenStyle; inWidth: Integer;
	inSide: TThCssBorderSide);
var
	i: Integer;
	e, r: TRect;
begin
	with Canvas do
	begin
		r := PaintRect;
		Pen.Style := inStyle;
		for i := 1 to inWidth do
		begin
			e := TbhGetRectEdge(r, inSide);
			MoveTo(e.Left, e.Top);
			LineTo(e.Right, e.Bottom);
			TbhInflateBySide(r, -1, inSide);
		end;
	end;
end;

procedure TThStylePainter.PaintBorder(inSide: TThCssBorderSide);
var
	w, wp, wx: Integer;
	c: TColor;
begin
	with Style.Border, Canvas do
	begin
		c := GetBorderColor(inSide);
		Pen.Color := c;
		Brush.Color := c;
		w := GetBorderPixels(inSide);
		case GetBorderStyle(inSide) of
			//
			bsSolidBorder: PaintRectEdge(psSolid, w, inSide);
			//
			bsDotted: PaintDotEdge(w, inSide);
			//
			bsDashed: PaintDashEdge(w, inSide);
			//
			bsDouble:
			begin
				wp := w div 3;
				wx := w - wp * 3;
				if (wp < 0) then
					PaintRectEdge(psSolid, w, inSide)
				else begin
					PaintRectEdge(psSolid, wp, inSide);
					TbhInflateBySide(FPaintRect, -wp - wp - wx, inSide);
					PaintRectEdge(psSolid, wp, inSide);
				end;
			end;
			//
		end;
		Pen.Width := 1;
	end;
end;

procedure TThStylePainter.Borders4(inSide: TThCssBorderSide;
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

procedure TThStylePainter.PaintInsetBorders;
var
	i: TThCssBorderSide;
begin
	for i := bsLeft to bsBottom do
		case Style.Border.GetBorderStyle(i) of
			bsDefault: Borders4(i, clBlack, $EEEEEE,	clBtnShadow, Color)
			else PaintBorder(i);
		end;
end;

procedure TThStylePainter.PaintOutsetBorders;
var
	i: TThCssBorderSide;
begin
	for i := bsLeft to bsBottom do
		case Style.Border.GetBorderStyle(i) of
			bsDefault: Borders4(i, clWhite,	clBtnShadow, $EEEEEE, clBlack)
			else PaintBorder(i);
		end;
end;

procedure TThStylePainter.PaintBorders;
var
	i: TThCssBorderSide;
begin
	for i := bsLeft to bsBottom do
		PaintBorder(i);
end;

procedure TThStylePainter.PaintPictureBackground;
begin
	if Style.Background.Picture.HasGraphic then
		ThTileGraphic(Canvas, PaintRect,
			Style.Background.Picture.PictureData.Graphic)
end;

procedure TThStylePainter.PaintColorBackground;
begin
	//if ThVisibleColor(Style.Color) then
	if ThVisibleColor(Color) then
		with Canvas do
		begin
			Brush.Color := Color;
			FillRect(PaintRect);
		end;
end;

procedure TThStylePainter.PaintBackground;
begin
	if Style.Background.Picture.HasGraphic then
		PaintPictureBackground
	else
		PaintColorBackground;
end;

procedure TThStylePainter.Prepare(inColor: TColor; inStyle: TThCssStyle;
	inCanvas: TCanvas; const inRect: TRect);
begin
	Color := inColor;
	Style := inStyle;
	Canvas := inCanvas;
	PaintRect := inRect;
end;

initialization
	StylePainter := TThStylePainter.Create;
finalization
	StylePainter.Free;
end.
