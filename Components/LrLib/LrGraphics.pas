unit LrGraphics;

interface

uses
	Types, Classes, Graphics;

type
	TLrHAlign = ( haDefault, haLeft, haCenter, haRight );
	TLrVAlign = ( vaDefault, vaTop, vaMiddle, vaBottom {, vaBaseline} );

function LrCalcPaintPosition(
	inW, inH, inPictureW, inPictureH: Integer;
	inHAlign: TLrHAlign; inVAlign: TLrVAlign): TPoint;
procedure LrPaintPicture(inCanvas: TCanvas; inPicture: TPicture; inRect: TRect;
	inHAlign: TLrHAlign; inVAlign: TLrVAlign);
procedure LrAspectPaintPicture(inCanvas: TCanvas; inPicture: TPicture;
	const inRect: TRect; inHAlign: TLrHAlign; inVAlign: TLrVAlign);
function LrCalcAspectSize(inW, inH, inPictureW, inPictureH: Integer): TPoint;

procedure LrBitmapToJpgStream(inBitmap: TBitmap; outStream: TStream);
procedure LrTileGraphic(inCanvas: TCanvas; inR: TRect; inGraphic: TGraphic);

procedure LrPaintRules(inCanvas: TCanvas; const inRect: TRect;
	inDivs: Integer = 32; inSubDivs: Boolean = true);

implementation

uses
	Jpeg, LrVclUtils;

function LrCalcPaintPosition(
	inW, inH, inPictureW, inPictureH: Integer;
	inHAlign: TLrHAlign; inVAlign: TLrVAlign): TPoint;
begin
	case inHAlign of
		haLeft: Result.x := 0;
		haDefault, haCenter: Result.x := (inW - inPictureW) div 2;
		haRight: Result.x := inW - inPictureW;
	end;
	case inVAlign of
		vaDefault, vaTop: Result.y := 0;
		vaMiddle: Result.y := (inH - inPictureH) div 2;
		vaBottom: Result.y := inH - inPictureH;
	end;
end;

procedure LrPaintPicture(inCanvas: TCanvas; inPicture: TPicture; inRect: TRect;
	inHAlign: TLrHAlign; inVAlign: TLrVAlign);
var
	w, h: Integer;
begin
	w := LrWidth(inRect);
	h := LrHeight(inRect);
	with LrCalcPaintPosition(w, h, inPicture.Width, inPicture.Height,
		inHAlign, inVAlign) do
			inCanvas.Draw(inRect.Left + X, inRect.Top + Y, inPicture.Graphic)
end;

procedure LrAspectPaintPicture(inCanvas: TCanvas; inPicture: TPicture;
	const inRect: TRect; inHAlign: TLrHAlign; inVAlign: TLrVAlign);
var
	w, h: Integer;
	a: TPoint;
	r: TRect;
begin
	w := LrWidth(inRect);
	h := LrHeight(inRect);
	a := LrCalcAspectSize(w, h, inPicture.Width, inPicture.Height);
	r := Rect(0, 0, a.X, a.Y);
	//inRect.Bottom := LrMin(inRect.Bottom, h + inRect.Top);
	with LrCalcPaintPosition(w, h, a.X, a.Y, inHAlign, inVAlign) do
		OffsetRect(r, inRect.Left + X, inRect.Top + Y);
	inCanvas.StretchDraw(r, inPicture.Graphic);
end;

function LrCalcAspectSize(inW, inH, inPictureW, inPictureH: Integer): TPoint;
begin
	Result.X := inW;
	if (inPictureW = 0) or (inPictureH = 0) then
		Result.Y := 0
	else begin
		Result.Y := inW * inPictureH div inPictureW;
		if Result.Y > inH then
		begin
			Result.Y := inH;
			Result.X := inH * inPictureW div inPictureH;
		end;
	end;
end;

procedure LrBitmapToJpgStream(inBitmap: TBitmap; outStream: TStream);
var
	j: TJpegImage;
begin
	j := TJpegImage.Create;
	with j do
	try
		Assign(inBitmap);
		SaveToStream(outStream);
		outStream.Position := 0;
	finally
		Free;
	end;
end;

procedure LrTileGraphic(inCanvas: TCanvas; inR: TRect; inGraphic: TGraphic);
var
	i, j, l, t, w, h: Integer;
begin
	if (inGraphic <> nil) and (not inGraphic.Empty) then
		with inR, inGraphic do
		begin
			w := (Right - Left + Width) div Width;
			h := (Bottom - Top + Height) div Height;
			t := Top;
			for j := 0 to h - 1 do
			begin
				l := Left;
				for i := 0 to w - 1 do
				begin
					inCanvas.Draw(l, t, inGraphic);
					Inc(l, Width);
				end;
				Inc(t, Height);
			end;
		end;
end;

procedure LrPaintRules(inCanvas: TCanvas; const inRect: TRect;
	inDivs: Integer; inSubDivs: Boolean);
var
	d, d2, w, h, i: Integer;
begin
	d := inDivs;
	d2 := inDivs div 2;
	w := (inRect.Right - inRect.Left + d - 1) div d;
	h := (inRect.Bottom - inRect.Top + d - 1) div d;
	with inCanvas do
	begin
		Pen.Style := psDot;
		for i := 0 to w do
		begin
			Pen.Color := $DDDDDD;
			MoveTo(i * d, inRect.Top);
			LineTo(i * d, inRect.Bottom);
			if inSubDivs then
			begin
				Pen.Color := $F0F0F0;
				MoveTo(i * d + d2, inRect.Top);
				LineTo(i * d + d2, inRect.Bottom);
			end;
		end;
		for i := 0 to h do
		begin
			Pen.Color := $DDDDDD;
			MoveTo(inRect.Left, i * d);
			LineTo(inRect.Right, i * d);
			if inSubDivs then
			begin
				Pen.Color := $F0F0F0;
				MoveTo(inRect.Left, i * d + d2);
				LineTo(inRect.Right, i * d + d2);
			end;
		end;
	end;
end;

end.
