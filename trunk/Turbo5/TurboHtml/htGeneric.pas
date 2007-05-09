unit htGeneric;

interface

uses
	SysUtils, Classes, Controls, Graphics,
	LrGraphics, LrTextPainter,
	htControls, htMarkup;

type
	ThtGeneric = class(ThtGraphicControl)
	private
		FHAlign: TLrHAlign;
	protected
		procedure Generate(const inContainer: string;
			inMarkup: ThtMarkup); override;
		procedure GenerateStyles(const inContainer: string;
			inMarkup: ThtMarkup);
		procedure SetHAlign(const Value: TLrHAlign);
		procedure StylePaint; override;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
	published
		property Align;
		property HAlign: TLrHAlign read FHAlign write SetHAlign;
		property Outline;
		property Style;
	end;

implementation

uses
	htPaint;

{ ThtGeneric }

constructor ThtGeneric.Create(inOwner: TComponent);
begin
	inherited;
end;

destructor ThtGeneric.Destroy;
begin
	inherited;
end;

procedure ThtGeneric.SetHAlign(const Value: TLrHAlign);
begin
	FHAlign := Value;
	Invalidate;
end;

procedure ThtGeneric.StylePaint;
begin
	htPaintHash(Canvas, ClientRect, clSilver);
	inherited;
end;

procedure ThtGeneric.GenerateStyles(const inContainer: string;
	inMarkup: ThtMarkup);
var
	s: string;
begin
	s := CtrlStyle.InlineAttribute;
	if (s <> '') then
		inMarkup.Styles.Add(Format('#%s { %s }', [ inContainer, s ]));
end;

procedure ThtGeneric.Generate(const inContainer: string;
	inMarkup: ThtMarkup);
begin
	GenerateStyles(inContainer, inMarkup);
	inMarkup.Add(Caption);
end;

end.
