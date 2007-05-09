unit ThSpacer;

interface

uses
	Windows, SysUtils, Types, Classes, Controls, Graphics,
	ThTag, ThWebControl;

type
	TThSpacer = class(TThWebGraphicControl)
	private
		FShowHash: Boolean;
		procedure SetShowHash(const Value: Boolean);
	protected
		procedure Paint; override;
	public
		constructor Create(AOwner: TComponent); override;
		procedure CellTag(inTag: TThTag); override;
	published
		property Align;
		property ShowHash: Boolean read FShowHash write SetShowHash default true;
		property Style;
		property StyleClass;
		property Visible;
	end;

implementation

uses
	ThCssStyle;

constructor TThSpacer.Create(AOwner: TComponent);
begin
	inherited;
	FShowHash := true;
	DesignOpaque := true;
end;

procedure TThSpacer.Paint;
var
	c: TColor;
begin
	inherited;
	if ShowHash then
		with Canvas do
		begin
			c := Brush.Color;
			Brush.Style := bsBDiagonal;
			Brush.Color := ColorToRgb(clSilver);
			Pen.Style := psClear;
			if ThVisibleColor(c) then
				SetBkColor(Handle, ColorToRgb(c))
			else
				SetBkMode(Handle, TRANSPARENT);
			Rectangle(AdjustedClientRect);
			Brush.Style := bsSolid;
			Pen.Style := psSolid;
			SetBkMode(Handle, OPAQUE);
		end;
end;

procedure TThSpacer.CellTag(inTag: TThTag);
begin
	inherited;
//	inTag.Add('height', Height);
//	StylizeTag(inTag);
	Tag(inTag);
end;

procedure TThSpacer.SetShowHash(const Value: Boolean);
begin
	FShowHash := Value;
	Invalidate;
end;

end.
