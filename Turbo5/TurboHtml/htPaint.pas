unit htPaint;

interface

uses
	Windows, SysUtils, Types, Classes, Controls, Graphics;

procedure htPaintOutline(inCanvas: TCanvas; const inRect: TRect;
	inColor: TColor = clSilver);
procedure htPaintGuides(inCanvas: TCanvas; inContainer: TCustomControl);
procedure htPaintHash(inCanvas: TCanvas; const inRect: TRect; inColor: TColor);
procedure htPaintRules(inCanvas: TCanvas; const inRect: TRect;
	inDivs: Integer = 32; inSubDivs: Boolean = true);

implementation

uses
	LrControlIterator, LrVclUtils, htUtils;

procedure htPaintOutline(inCanvas: TCanvas; const inRect: TRect;
	inColor: TColor);
begin
	with inCanvas do
	begin
		Pen.Width := 1;
		Pen.Color := inColor;
		Pen.Style := psDot;
		Brush.Style := bsClear;
		Rectangle(inRect);
	end;
end;

procedure htPaintHash(inCanvas: TCanvas; const inRect: TRect;
	inColor: TColor);
begin
	with inCanvas do
	begin
		Brush.Style := bsBDiagonal;
		Brush.Color := ColorToRgb(clSilver);
		Pen.Style := psClear;
		if htVisibleColor(inColor) then
			SetBkColor(Handle, ColorToRgb(inColor))
		else
			SetBkMode(Handle, TRANSPARENT);
		Rectangle(inRect);
		SetBkMode(Handle, OPAQUE);
	end;
end;

procedure htPaintGuides(inCanvas: TCanvas; inContainer: TCustomControl);

	function Middle(inCtrl: TControl): Integer;
	begin
		Result := inCtrl.Top + inCtrl.Height div 2;
	end;

	function Center(inCtrl: TControl): Integer;
	begin
		Result := inCtrl.Left + inCtrl.Width div 2;
	end;

begin
	with inCanvas do
	begin
		Pen.Width := 0;
		Pen.Style := psDot;
		Pen.Color := clSilver;
		with TLrCtrlIterator.Create(inContainer) do
		try
			while Next([alNone]) do
			begin
				MoveTo(0, Middle(Ctrl));
				LineTo(inContainer.ClientWidth, Middle(Ctrl));
				MoveTo(Center(Ctrl), 0);
				LineTo(Center(Ctrl), inContainer.ClientHeight);
			end;
		finally
			Free;
		end;
	end;
end;

procedure htPaintRules(inCanvas: TCanvas; const inRect: TRect;
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
