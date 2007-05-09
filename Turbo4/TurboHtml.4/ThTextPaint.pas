unit ThTextPaint;

interface

uses
	Classes, StdCtrls, Types, Graphics;

type
	TThHAlign = ( haDefault, haLeft, haCenter, haRight );
	TThVAlign = ( vaDefault, vaTop, vaMiddle, vaBottom, vaBaseline );

function ThTextWidth(inCanvas: TCanvas; const inText: string): Integer;
function ThTextHeight(inCanvas: TCanvas;
	const inText: string): Integer; overload;
function ThTextHeight(inCanvas: TCanvas; const inText: string; inR: TRect;
	inHAlign: TThHAlign = haDefault;
	inWordWrap: Boolean = false): Integer; overload;
procedure ThPaintText(inCanvas: TCanvas; const inText: string; inR: TRect;
	inHAlign: TThHAlign = haDefault; inVAlign: TThVAlign = vaTop;
	inWordWrap: Boolean = false; inTransparent: Boolean = true);

type
	TThTextPainter = class
  private
		FTransparent: Boolean;
		FWordWrap: Boolean;
		FHAlign: TThHAlign;
		FVAlign: TThVAlign;
		FCanvas: TCanvas;
		procedure SetHAlign(const Value: TThHAlign);
		procedure SetVAlign(const Value: TThVAlign);
		procedure SetCanvas(const Value: TCanvas);
		procedure SetTransparent(const Value: Boolean);
		procedure SetWordWrap(const Value: Boolean);
	public
		function Width(const inText: string): Integer;
		function Height(const inText: string; inR: TRect): Integer;
		procedure PaintText(const inText: string; inR: TRect);
	public
		property HAlign: TThHAlign read FHAlign write SetHAlign;
		property Canvas: TCanvas read FCanvas write SetCanvas;
		property WordWrap: Boolean read FWordWrap write SetWordWrap;
		property Transparent: Boolean read FTransparent write SetTransparent;
		property VAlign: TThVAlign read FVAlign write SetVAlign;
	end;

implementation

uses
	Windows;

const
	cThAlignFlags: array[TThHAlign] of Integer =
		( DT_LEFT, DT_LEFT, DT_CENTER, DT_RIGHT );
	cThWrapFlags: array[Boolean] of Integer =
		( 0, DT_WORDBREAK );

function ThTextWidth(inCanvas: TCanvas; const inText: string): Integer;
begin
	Result := inCanvas.TextWidth(inText);
end;

function ThTextHeight(inCanvas: TCanvas; const inText: string; inR: TRect;
	inHAlign: TThHAlign; inWordWrap: Boolean): Integer;
begin
	Result := DrawText(inCanvas.Handle, PChar(inText), Length(inText), inR,
		DT_CALCRECT + cThAlignFlags[inHAlign] + cThWrapFlags[inWordWrap]);
end;

function ThTextHeight(inCanvas: TCanvas;
	const inText: string): Integer; overload;
begin
	Result := ThTextHeight(inCanvas, inText, Rect(0, 0, 9999, 0));
end;

procedure ThPaintText(inCanvas: TCanvas; const inText: string; inR: TRect;
	inHAlign: TThHAlign; inVAlign: TThVAlign; inWordWrap: Boolean;
	inTransparent: Boolean);
var
	t, h: Integer;
begin
	h := ThTextHeight(inCanvas, inText, inR, inHAlign, inWordWrap);
	t := (inR.Bottom - inR.Top) - h;
	case inVAlign of
		vaBottom: ;
		{vaDefault,} vaMiddle: t := t div 2;
		else t := 0;
	end;
	if (t < 0) then
		t := 0;
	inR.Top := inR.Top + t;
	if inTransparent then
		SetBkMode(inCanvas.Handle, Windows.TRANSPARENT);
	DrawText(inCanvas.Handle, PChar(inText), Length(inText), inR,
		cThAlignFlags[inHAlign] + cThWrapFlags[inWordWrap]);
end;

{ TThTextPainter }

function TThTextPainter.Height(const inText: string; inR: TRect): Integer;
begin
	if Canvas <> nil then
		Result := ThTextHeight(Canvas, inText, inR, HAlign, WordWrap)
	else
		Result := 0;
end;

function TThTextPainter.Width(const inText: string): Integer;
begin
	if Canvas <> nil then
		Result := ThTextWidth(Canvas, inText)
	else
		Result := 0;
end;

procedure TThTextPainter.PaintText(const inText: string; inR: TRect);
begin
	if Canvas <> nil then
		ThPaintText(Canvas, inText, inR, HAlign, VAlign, WordWrap, Transparent);
end;

procedure TThTextPainter.SetHAlign(const Value: TThHAlign);
begin
	FHAlign := Value;
end;

procedure TThTextPainter.SetVAlign(const Value: TThVAlign);
begin
	FVAlign := Value;
end;

procedure TThTextPainter.SetCanvas(const Value: TCanvas);
begin
	FCanvas := Value;
end;

procedure TThTextPainter.SetTransparent(const Value: Boolean);
begin
	FTransparent := Value;
end;

procedure TThTextPainter.SetWordWrap(const Value: Boolean);
begin
	FWordWrap := Value;
end;

end.
