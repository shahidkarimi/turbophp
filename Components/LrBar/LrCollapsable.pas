unit LrCollapsable;

interface

uses
	SysUtils, Windows, Classes, Controls, Messages, StdCtrls, ExtCtrls, Forms,
	Graphics;

type
	TLrCollapsable = class(TCustomControl)
	private
		FBorderColor: TColor;
		FClosed: Boolean;
//		FClosedGlyph: TBitmap;
//		FHot: Boolean;
//		FOpenGlyph: TBitmap;
		FOpenHeight: Integer;
		FHeaderColor: TColor;
		FHeaderHeight: Integer;
	protected
		function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
		function GetGlyph: TBitmap;
		function GetHeaderRect: TRect;
		procedure AdjustClientRect(var Rect: TRect); override;
//		procedure AlignControls(AControl: TControl; var Rect: TRect); override;
//		procedure CMControlChange(
//			var Message: TCMControlChange); message CM_CONTROLCHANGE;
//		procedure CreateHeader;
//		procedure CreateTimer;
		procedure DefineProperties(Filer: TFiler); override;
		procedure LoadExtraProps(Reader: TReader);
		procedure MouseUp(Button: TMouseButton;	Shift: TShiftState;
			X, Y: Integer); override;
		procedure OpenClose;
		procedure Paint; override;
		procedure SetBorderColor(const Value: TColor);
		procedure SetClosed(const Value: Boolean);
		procedure SetHeaderColor(const Value: TColor);
		procedure SetHeaderHeight(const Value: Integer);
//		procedure SetHot(const Value: Boolean);
		procedure StoreExtraProps(Writer: TWriter);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		property HeaderRect: TRect read GetHeaderRect;
//		property Hot: Boolean read FHot write SetHot;
	published
		property Align;
		property AutoSize;
		property BorderColor: TColor read FBorderColor write SetBorderColor
			default clWhite;
		property Color;
		property Caption;
		property Closed: Boolean read FClosed write SetClosed default false;
		property Enabled;
		property Font;
		property HeaderColor: TColor read FHeaderColor write SetHeaderColor
			default clBtnFace;
		property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight
			default 18;
		property ParentFont;
		property Visible;
	end;

implementation

{$R LrBarImages.res}

var
	ResClosedGlyph: TBitmap;
	ResOpenGlyph: TBitmap;

procedure NeedResGlyphs;
begin
	if ResClosedGlyph = nil then
	begin
		ResClosedGlyph := TBitmap.Create;
		ResClosedGlyph.LoadFromResourceName(HInstance, 'LRCLOSED');
		ResOpenGlyph := TBitmap.Create;
		ResOpenGlyph.LoadFromResourceName(HInstance, 'LROPEN');
	end;
end;

{ TLrCollapsable }

constructor TLrCollapsable.Create(AOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle + [ csAcceptsControls ];
	NeedResGlyphs;
	FBorderColor := clWhite;
	FHeaderColor := clBtnFace;
	FHeaderHeight := 18;
end;

destructor TLrCollapsable.Destroy;
begin
	inherited;
end;

procedure TLrCollapsable.LoadExtraProps(Reader: TReader);
begin
	FOpenHeight := Reader.ReadInteger;
end;

procedure TLrCollapsable.StoreExtraProps(Writer: TWriter);
begin
	Writer.WriteInteger(FOpenHeight);
end;

procedure TLrCollapsable.DefineProperties(Filer: TFiler);
begin
	inherited; { allow base classes to define properties }
	Filer.DefineProperty('ExtraProps', LoadExtraProps, StoreExtraProps, true);
end;

function TLrCollapsable.GetHeaderRect: TRect;
begin
	Result := Rect(0, 0, ClientWidth, HeaderHeight);
end;

function TLrCollapsable.GetGlyph: TBitmap;
begin
	if Closed then
		Result := ResClosedGlyph
	else
		Result := ResOpenGlyph;
end;

procedure TLrCollapsable.Paint;
var
	r: TRect;
begin
	inherited;
	r := HeaderRect;
	Canvas.Brush.Color := HeaderColor;
	Canvas.Pen.Color := BorderColor;
	Canvas.Rectangle(r);
	InflateRect(r, -4, -2);
	Canvas.Draw(r.Left, r.Top + (r.Bottom - r.Top - GetGlyph.Height) div 2,
		GetGlyph);
	Inc(r.Left, 20);
	Canvas.Font := Font;
	SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
	DrawText(Canvas.Handle, PChar(Caption), Length(Caption), r, 0);
	SetBkMode(Canvas.Handle, OPAQUE);
end;

procedure TLrCollapsable.OpenClose;
begin
	if not Closed then
	begin
		FOpenHeight := Height;
		Height := HeaderHeight;
		FClosed := true;
	end
	else begin
		Height := FOpenHeight;
		FClosed := false;
	end;
	AdjustSize;
end;

procedure TLrCollapsable.AdjustClientRect(var Rect: TRect);
begin
	inherited;
	Inc(Rect.Top, HeaderHeight);
end;

function TLrCollapsable.CanAutoSize(var NewWidth,
	NewHeight: Integer): Boolean;
begin
	Result := inherited CanAutoSize(NewWidth, NewHeight); // and not Closed;
	if Closed then
		NewHeight := HeaderHeight;
end;

procedure TLrCollapsable.SetClosed(const Value: Boolean);
begin
	if (FClosed <> Value) then
		OpenClose;
end;

procedure TLrCollapsable.SetHeaderHeight(const Value: Integer);
begin
	FHeaderHeight := Value;
	Invalidate;
end;

procedure TLrCollapsable.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
	Y: Integer);
begin
	inherited;
	if Y < HeaderHeight then
		OpenClose;
end;

procedure TLrCollapsable.SetBorderColor(const Value: TColor);
begin
	BorderColor := Value;
	Invalidate;
end;

procedure TLrCollapsable.SetHeaderColor(const Value: TColor);
begin
	FHeaderColor := Value;
	Invalidate;
end;

{
procedure TLrCollapsable.SetHot(const Value: Boolean);
begin
	FHot := Value;
	Invalidate;
end;
}

end.
