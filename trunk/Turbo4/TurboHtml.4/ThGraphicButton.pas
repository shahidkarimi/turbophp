unit ThGraphicButton;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThInterfaces, ThCssStyle, ThStyledCtrl, ThWebControl, ThPicture, ThTag,
	ThAnchor, ThTextPaint, ThLabel, ThAttributeList, ThStyleList, ThImage;

type
	TThGraphicButton = class(TThCustomImage, IThFormInput)
	private
		Bits: TBitmap;
		LeftBits: TBitmap;
		MidBits: TBitmap;
		RightBits: TBitmap;
		FPixelFormat: TPixelFormat;
		FTextOffset: Integer;
	protected
		procedure SetTextOffset(const Value: Integer);
	protected
		procedure BuildBits; virtual;
		procedure CreateSections;
		procedure CssStyleChanged; override;
		procedure FreeSections;
		procedure ImagesToBits;
		procedure PaintImage; override;
		procedure PerformAutoSize; override;
		procedure PictureChange(inSender: TObject); override;
		procedure Resize; override;
		procedure Tag(inTag: TThTag); override;
		procedure TextToBits;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		property PixelFormat: TPixelFormat read FPixelFormat write FPixelFormat;
	published
		property Align;
		property AltText;
		property Anchor;
		//property AutoAspect;
		property AutoSize;
		property Border;
		property Caption;
		property HAlign;
		property Picture;
		property Style;
		property StyleClass;
		property TextOffset: Integer read FTextOffset write SetTextOffset;
		property VAlign;
		property Visible;
	end;

implementation

constructor TThGraphicButton.Create(inOwner: TComponent);
begin
	inherited;
	Bits := TBitmap.Create;
	Pixelformat := pf24bit;
end;

destructor TThGraphicButton.Destroy;
begin
	Bits.Free;
	inherited;
end;

procedure TThGraphicButton.CssStyleChanged;
begin
	inherited;
	BuildBits;
end;

procedure TThGraphicButton.PictureChange(inSender: TObject);
begin
	inherited;
	BuildBits;
end;

procedure TThGraphicButton.Resize;
begin
	inherited;
	BuildBits;
end;

procedure TThGraphicButton.PerformAutoSize;
begin
	if (Picture.Height <> 0) then
		Height := Picture.Height;
end;

procedure TThGraphicButton.PaintImage;
begin
	with CalcPaintPosition(Bits.Width, Bits.Height) do
		Canvas.Draw(x, y, Bits);
end;

procedure TThGraphicButton.BuildBits;
begin
	if Bits <> nil then
	begin
		with Bits do
		begin
			Width := Self.Width;
			Height := Self.Height;
			PixelFormat := Self.PixelFormat;
		end;
		try
			if (Picture.HasGraphic) then
			begin
				CreateSections;
				ImagesToBits;
			end;
			TextToBits;
		finally
			FreeSections;
		end
	end;
end;

procedure TThGraphicButton.CreateSections;
var
	w, h: Integer;
begin
	w := Picture.Width div 2;
	h := Picture.Height;
	Picture.Graphic.Transparent := true;
	//
	LeftBits := TBitmap.Create;
	with LeftBits do
	begin
		Width := w;
		Height := h;
		PixelFormat := Self.PixelFormat;
		Canvas.Brush.Color := Self.Color;
		Canvas.FillRect(Rect(0, 0, Width, Height));
		Canvas.Draw(0, 0, Picture.Graphic);
		//FImageList.Draw(Canvas, 0, 0, PaintIndex);
	end;
	//
	MidBits := TBitmap.Create;
	with MidBits do
	begin
		Width := 1;
		Height := h;
		PixelFormat := Self.PixelFormat;
		Canvas.Brush.Color := Self.Color;
		Canvas.FillRect(Rect(0, 0, Width, Height));
		Canvas.Draw(-w, 0, Picture.Graphic);
		//FImageList.Draw(Canvas, -w, 0, PaintIndex);
	end;
	//
	RightBits := TBitmap.Create;
	with RightBits do
	begin
		Width := w;
		Height := h;
		PixelFormat := Self.PixelFormat;
		Canvas.Brush.Color := Self.Color;
		Canvas.FillRect(Rect(0, 0, Width, Height));
		Canvas.Draw(-w - 1, 0, Picture.Graphic);
		//FImageList.Draw(Canvas, -w - 1, 0, PaintIndex);
	end;
end;

procedure TThGraphicButton.FreeSections;
begin
	FreeAndNil(LeftBits);
	FreeAndNil(MidBits);
	FreeAndNil(RightBits);
end;

procedure TThGraphicButton.ImagesToBits;
var
	w, i, x: Integer;
begin
	if (MidBits.Width > 0) then
	begin
		w := Width - LeftBits.Width - RightBits.Width;
		w := w div MidBits.Width;
		with Bits do
		begin
			Width := LeftBits.Width + RightBits.Width + w * MidBits.Width;
			Height := Self.Height;
			//
			Transparent := false;
			//
{
			with Canvas do
			begin
				Draw(0, 0, LeftBits);
				for i := 0 to w - 1 do
					Draw(LeftBits.Width + MidBits.Width * i, 0, MidBits);
				Draw(Width - RightBits.Width, 0, RightBits);
			end;
}
			with Canvas do
			begin
				StretchDraw(Rect(0, 0, LeftBits.Width, Height), LeftBits);
				for i := 0 to w - 1 do
				begin
					x := LeftBits.Width + MidBits.Width * i;
					StretchDraw(Rect(x, 0, x + MidBits.Width, Height), MidBits);
				end;
				StretchDraw(Rect(Width - RightBits.Width, 0, Width, Height),
					RightBits);
			end;
		end;
	end;
end;

procedure TThGraphicButton.TextToBits;
var
	h: Integer;
	size: TSize;
begin
	with Bits, Bits.Canvas do
	begin
		Font := Self.Font;
		Brush.Style := bsClear;
		h := Height + TextOffset;
		size := TextExtent(Caption);
		TextRect(Rect(0, 0, Width, Height),
			(Width - size.cx) div 2, (h - size.cy) div 2,
				Caption);
	end;
end;

procedure TThGraphicButton.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Element := 'input';
		Add('type', 'image');
	end;
end;

procedure TThGraphicButton.SetTextOffset(const Value: Integer);
begin
	FTextOffset := Value;
	BuildBits;
	Invalidate;
end;

end.
