unit LrTextPainter;

interface

uses
	Windows, Classes, Graphics,
	LrGraphics;

type
	TLrTextPainter = class(TPersistent)
	private
		FCanvas: TCanvas;
		FHAlign: TLrHAlign;
		FTransparent: Boolean;
		FVAlign: TLrVAlign;
		FWordWrap: Boolean;
		FOnChange: TNotifyEvent;
	protected
		function GetDC: HDC;
		function GetFlags: Integer;
		procedure AlignRect(const inText: string; var ioR: TRect);
		procedure Change;
		procedure SetHAlign(const Value: TLrHAlign);
		procedure SetTransparent(const Value: Boolean);
		procedure SetVAlign(const Value: TLrVAlign);
		procedure SetWordWrap(const Value: Boolean);
	public
		function Height(const inText: string; inR: TRect): Integer;
		function Width(const inText: string): Integer;
		procedure PaintText(const inText: string; inR: TRect;
			inCanvas: TCanvas = nil);
	public
		property Canvas: TCanvas read FCanvas write FCanvas;
		property Flags: Integer read GetFlags;
		property DC: HDC read GetDC;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
	published
		property HAlign: TLrHAlign read FHAlign write SetHAlign;
		property Transparent: Boolean read FTransparent write SetTransparent;
		property VAlign: TLrVAlign read FVAlign write SetVAlign;
		property WordWrap: Boolean read FWordWrap write SetWordWrap;
	end;

function TextPainter: TLrTextPainter;

implementation

uses
	LrVclUtils;

const
	cLrAlignFlags: array[TLrHAlign] of Integer = ( DT_LEFT, DT_LEFT, DT_CENTER,
		DT_RIGHT );
	cLrWrapFlags: array[Boolean] of Integer = ( 0, DT_WORDBREAK );
	cLrTxFlags: array[Boolean] of Integer = ( Windows.OPAQUE,
		Windows.TRANSPARENT );

var
	SingletonTextPainter: TLrTextPainter;

function TextPainter: TLrTextPainter;
begin
	if SingletonTextPainter = nil then
		SingletonTextPainter := TLrTextPainter.Create;
	Result := SingletonTextPainter;
end;

{ TLrTextPainter }

function TLrTextPainter.GetDC: HDC;
begin
	Result := Canvas.Handle;
end;

function TLrTextPainter.GetFlags: Integer;
begin
	Result := cLrAlignFlags[HAlign] + cLrWrapFlags[WordWrap] + DT_NOPREFIX;
end;

function TLrTextPainter.Height(const inText: string;
	inR: TRect): Integer;
begin
	if Canvas <> nil then
		Result := DrawText(DC, PChar(inText), Length(inText), inR,
			Flags + DT_CALCRECT)
	else
		Result := 0;
end;

function TLrTextPainter.Width(const inText: string): Integer;
begin
	if Canvas <> nil then
		Result := Canvas.TextWidth(inText)
	else
		Result := 0;
end;

procedure TLrTextPainter.AlignRect(const inText: string; var ioR: TRect);
var
	y: Integer;
begin
	y := (ioR.Bottom - ioR.Top) - Height(inText, ioR);
	case VAlign of
		vaBottom: ;
		vaMiddle: y := y div 2;
		else y := 0;
	end;
	Inc(ioR.Top, LrMax(0, y));
end;

procedure TLrTextPainter.PaintText(const inText: string; inR: TRect;
	inCanvas: TCanvas);
begin
	if inCanvas <> nil then
		Canvas := inCanvas;
	if Canvas <> nil then
	begin
		AlignRect(inText, inR);
		SetBkMode(DC, cLrtxFlags[Transparent]);
		DrawText(DC, PChar(inText), Length(inText), inR,	Flags);
	end;
end;

procedure TLrTextPainter.Change;
begin
	if Assigned(OnChange) then
		OnChange(Self);
end;

procedure TLrTextPainter.SetHAlign(const Value: TLrHAlign);
begin
	FHAlign := Value;
	Change;
end;

procedure TLrTextPainter.SetTransparent(const Value: Boolean);
begin
	FTransparent := Value;
	Change;
end;

procedure TLrTextPainter.SetVAlign(const Value: TLrVAlign);
begin
	FVAlign := Value;
	Change;
end;

procedure TLrTextPainter.SetWordWrap(const Value: Boolean);
begin
	FWordWrap := Value;
	Change;
end;

end.
