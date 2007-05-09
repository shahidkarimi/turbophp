unit htButton;

interface

uses
	SysUtils, Classes, Controls, Graphics, Types,
	LrGraphics, LrTextPainter,
	htStyle, htControls, htMarkup;

type
	ThtButton = class(ThtGraphicControl)
	private
		FTextPainter: TLrTextPainter;
	protected
		procedure BuildStyle; override;
		procedure Generate(const inContainer: string;
			inMarkup: ThtMarkup); override;
		function GetBoxHeight: Integer; override;
		function GetBoxRect: TRect; override;
		function GetBoxWidth: Integer; override;
		procedure PerformAutoSize; override;
		procedure StylePaint; override;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		property TextPainter: TLrTextPainter read FTextPainter;
	published
		property Align;
		property AutoSize;
		property Caption;
		property Outline;
		property Style;
	end;

implementation

uses
	htUtils;

{ ThtButton }

constructor ThtButton.Create(inOwner: TComponent);
begin
	inherited;
	SetBounds(0, 0, 75, 25);
	FTextPainter := TLrTextPainter.Create;
	TextPainter.Canvas := Canvas;
	TextPainter.Transparent := true;
	TextPainter.HAlign := haCenter;
	TextPainter.VAlign := vaMiddle;
	//TextPainter.OnChange := TextPainterChange;
end;

destructor ThtButton.Destroy;
begin
	TextPainter.Free;
	inherited;
end;

function ThtButton.GetBoxHeight: Integer;
begin
	Result := ClientHeight;
end;

function ThtButton.GetBoxRect: TRect;
begin
	Result := ClientRect;
end;

function ThtButton.GetBoxWidth: Integer;
begin
	Result := ClientWidth;
end;

procedure ThtButton.BuildStyle;
begin
	inherited;
	if not htVisibleColor(CtrlStyle.Color) then
		CtrlStyle.Color := clBtnFace;
	if not CtrlStyle.Border.BorderVisible then
	begin
		CtrlStyle.Border.BorderStyle := bsOutset;
		CtrlStyle.Border.BorderWidth := '1';
	end;
	//
	//CtrlStyle.Font.ToFont(Font);
	//Canvas.Font := Font;
	Color := CtrlStyle.Color;
end;

procedure ThtButton.PerformAutoSize;
begin
	SetBounds(Left, Top, TextPainter.Width(Caption),
		TextPainter.Height(Caption, ClientRect));
end;

procedure ThtButton.StylePaint;
begin
	inherited;
	TextPainter.PaintText(Caption, ClientRect);
end;

procedure ThtButton.Generate(const inContainer: string;
	inMarkup: ThtMarkup);
var
	n, s: string;
begin
	n := Name;
	s := {Ctrl}Style.InlineAttribute;
	if (s <> '') then
		inMarkup.Styles.Add(Format('#%s { %s }', [ n, s ]));
	inMarkup.Add(
		Format('<input id="%s" name="%s" type="%s" value="%s" %s/>',
			[ n, n, 'button', Caption, ExtraAttributes ]));
end;

end.
