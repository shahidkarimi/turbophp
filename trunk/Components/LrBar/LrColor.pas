unit LrColor;

interface

uses
	SysUtils, Graphics;

type
	TRgb24 = Record
		b, g, r: Byte;
	end;
	//
	TRgb32 = Record
		case Integer of
			0: (r, g, b, a: Byte);
			1: (c: TColor);
	end;
	//
	TRgbf = Record
		r, g, b: Single;
	end;
	//
	EColorError = CLASS(Exception);

function MixColors(const inColor0, inColor1: TColor; inMix: Single): TColor;
function MultColors(const inColor0, inColor1: TColor): TColor;

function ColorSetHue(const inColor: TColor; inHue: Word): TColor;
function ColorSetLum(const inColor: TColor; inLum: Word): TColor;
function Colorize(const inSource, inColor: TColor): TColor;

implementation

uses
	Math, GraphUtil;

{$R-}

function ColorToRgb24(const inColor: TColor;
	inScalar1024: Integer = 0): TRgb24;
begin
	with TRgb24(Pointer(@inColor)^) do
	begin
		Result.r := b;
		Result.g := g;
		Result.b := r;
	end;
end;

function Clamp(inByte: Integer): Byte;
begin
		if (inByte < 0) then
			Result := 0
		else if (inByte > 255) then
			Result := 255
		else
			Result := inByte;
end;

function ScaledColorToRgb24(const inColor: TColor;
	inScalar1024: Integer = 0): TRgb24;
begin
	with TRgb24(Pointer(@inColor)^) do
	begin
		Result.r := Clamp((b * inScalar1024) shr 10);
		Result.g := Clamp((g * inScalar1024) shr 10);
		Result.b := Clamp((r * inScalar1024) shr 10);
	end;
end;

function MixColors(const inColor0, inColor1: TColor; inMix: Single): TColor;
var
	omm: Single;
	c0, c1: TRgb32;
begin
	omm := 1 - inMix;
	c0.c := inColor0;
	c1.c := inColor1;
	with c0 do
	begin
		r := Round(c1.r * inMix + r * omm);
		g := Round(c1.g * inMix + g * omm);
		b := Round(c1.b * inMix + b * omm);
	end;
	Result := c0.c;
	//Result := clYellow;
end;

function MultColors(const inColor0, inColor1: TColor): TColor;
var
	c0, c1: TRgb32;
begin
	c0.c := inColor0;
	c1.c := inColor1;
	with c0 do
	begin
		r := Clamp(r * c1.r{ div 255});
		g := Clamp(g * c1.g{ div 255});
		b := Clamp(b * c1.b{ div 255});
	end;
	Result := c0.c;
end;

function ColorToRgbf(const inColor: TColor): TRgbf;
begin
	with TRgb32(Pointer(@inColor)^) do
	begin
		Result.r := r / 255;
		Result.g := g / 255;
		Result.b := b / 255;
	end;
end;

function RgbfToColor(const inRgbf: TRgbf): TColor;
var
	r32: TRgb32;
begin
	with r32 do
	begin
		r := Round(inRgbf.r * 255);
		g := Round(inRgbf.g * 255);
		b := Round(inRgbf.b * 255);
	end;
	Result := r32.c;
end;

function Colorize(const inSource, inColor: TColor): TColor;
var
	sh, sl, ss, ch, cl, cs: Word;
begin
	ColorRGBToHLS(inSource, sh, sl, ss);
	ColorRGBToHLS(inColor, ch, cl, cs);
	if (cs = 0) then
		ss := cs;
	Result := ColorHLSToRGB(ch, sl, ss);
end;

function ColorSetHue(const inColor: TColor; inHue: Word): TColor;
var
	h, l, s: Word;
begin
	ColorRGBToHLS(inColor, h, l, s);
	Result := ColorHLSToRGB(inHue, l, s);
end;

function ColorSetLum(const inColor: TColor; inLum: Word): TColor;
var
	h, l, s: Word;
begin
	ColorRGBToHLS(inColor, h, l, s);
	Result := ColorHLSToRGB(h, inLum, s);
end;

end.
