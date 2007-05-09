unit ThGraphicUtils;

interface

uses
	Types, Classes, Graphics;

procedure ThBitmapToJpgStream(inBitmap: TBitmap; outStream: TStream);
procedure ThTileGraphic(inCanvas: TCanvas; inR: TRect; inGraphic: TGraphic);

implementation

uses
	Jpeg;

procedure ThBitmapToJpgStream(inBitmap: TBitmap; outStream: TStream);
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

procedure ThTileGraphic(inCanvas: TCanvas; inR: TRect; inGraphic: TGraphic);
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

end.
