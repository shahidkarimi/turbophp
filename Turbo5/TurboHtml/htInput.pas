unit htInput;

interface

uses
	SysUtils, Classes, Controls, Graphics,
	LrGraphics, LrTextPainter,
	htControls, htMarkup;

type
	ThtInput = class(ThtGraphicControl)
	private
		FTextPainter: TLrTextPainter;
		FPassword: Boolean;
		FValue: string;
	protected
		function GetHAlign: TLrHAlign;
		function GetValueText: string;
		function GetWordWrap: Boolean;
		procedure BuildStyle; override;
		procedure Generate(const inContainer: string;
			inMarkup: ThtMarkup); override;
		procedure PerformAutoSize; override;
		procedure SetHAlign(const Value: TLrHAlign);
		procedure SetPassword(const Value: Boolean);
		procedure SetValue(const Value: string);
		procedure SetWordWrap(const Value: Boolean);
		procedure StyleControl; override;
		procedure StylePaint; override;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		property TextPainter: TLrTextPainter read FTextPainter;
		property ValueText: string read GetValueText;
	published
		property Align;
		property AutoSize;
		property HAlign: TLrHAlign read GetHAlign write SetHAlign;
		property Value: string read FValue write SetValue;
		property Outline;
		property Password: Boolean read FPassword write SetPassword;
		property Style;
		property WordWrap: Boolean read GetWordWrap write SetWordWrap;
	end;

implementation

uses
	htUtils, htPaint, htStyle, htStylePainter;

{ ThtInput }

constructor ThtInput.Create(inOwner: TComponent);
begin
	inherited;
	FTextPainter := TLrTextPainter.Create;
	TextPainter.Canvas := Canvas;
	TextPainter.Transparent := true;
	//TextPainter.OnChange := TextPainterChange;
end;

destructor ThtInput.Destroy;
begin
	TextPainter.Free;
	inherited;
end;

function ThtInput.GetHAlign: TLrHAlign;
begin
	Result := TextPainter.HAlign;
end;

function ThtInput.GetWordWrap: Boolean;
begin
	Result := TextPainter.WordWrap;
end;

procedure ThtInput.SetHAlign(const Value: TLrHAlign);
begin
	TextPainter.HAlign := Value;
	Invalidate;
end;

procedure ThtInput.SetWordWrap(const Value: Boolean);
begin
	TextPainter.WordWrap := Value;
	Invalidate;
	AdjustSize;
end;

procedure ThtInput.SetValue(const Value: string);
begin
	FValue := Value;
	Invalidate;
	AdjustSize;
end;

function ThtInput.GetValueText: string;
begin
	if Password then
		Result := StringOfChar('*', Length(Value))
	else
		Result := Value;
end;

procedure ThtInput.PerformAutoSize;
begin
	SetBounds(Left, Top,
		TextPainter.Width(ValueText) + Style.GetBoxWidthMargin,
		TextPainter.Height(ValueText, ClientRect) + Style.GetBoxHeightMargin);
end;

procedure ThtInput.BuildStyle;
begin
	inherited;
	with CtrlStyle do
	begin
		if not Border.BorderVisible then
		begin
			//Border.BorderStyle := bsInset;
			Border.BorderWidth := '1';
			Border.BorderStyle := bsSolidBorder;
			Border.BorderColor := $B99D7F;
		end;
		if not Padding.HasPadding then
			Padding.Padding := 2;
		if not htVisibleColor(Color) then
			Color := clWhite;
	end;
end;

procedure ThtInput.StyleControl;
begin
	inherited;
end;

procedure ThtInput.StylePaint;
begin
	StylePainter.PaintBackground;
	StylePainter.PaintBorders;
	with BoxRect do
		TextPainter.PaintText(ValueText, Rect(Left + 2, Top, Right, Bottom));
	if Outline then
		htPaintOutline(Canvas, ClientRect);
end;

procedure ThtInput.SetPassword(const Value: Boolean);
begin
	FPassword := Value;
end;

procedure ThtInput.Generate(const inContainer: string;
	inMarkup: ThtMarkup);
var
	n, s, t: string;
begin
	n := Name;
	s := {Ctrl}Style.InlineAttribute;
	if (s <> '') then
		inMarkup.Styles.Add(Format('#%s { %s }', [ n, s ]));
	if Password then
		t := 'password'
	else
		t := 'text';
	inMarkup.Add(
		Format('<input id="%s" name="%s" type="%s" value="%s" %s/>',
			[ n, n, t, Value, ExtraAttributes ]));
	//inMarkup.Add(Caption);
end;

end.
