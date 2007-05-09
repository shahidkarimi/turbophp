unit htLabel;

interface

uses
	SysUtils, Classes, Controls, Graphics,
	LrGraphics, LrTextPainter,
	htControls, htMarkup;

type
	ThtLabel = class(ThtGraphicControl)
	private
		FTextPainter: TLrTextPainter;
	protected
		function GetHAlign: TLrHAlign;
		function GetWordWrap: Boolean;
		procedure Generate(const inContainer: string;
			inMarkup: ThtMarkup); override;
		procedure PerformAutoSize; override;
		procedure SetHAlign(const Value: TLrHAlign);
		procedure SetWordWrap(const Value: Boolean);
		procedure StylePaint; override;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		property TextPainter: TLrTextPainter read FTextPainter;
	published
		property Align;
		property AutoSize;
		property HAlign: TLrHAlign read GetHAlign write SetHAlign;
		property Caption;
		property Outline;
		property Style;
		property WordWrap: Boolean read GetWordWrap write SetWordWrap;
	end;

implementation

{ ThtLabel }

constructor ThtLabel.Create(inOwner: TComponent);
begin
	inherited;
	FTextPainter := TLrTextPainter.Create;
	TextPainter.Canvas := Canvas;
	TextPainter.Transparent := true;
	//TextPainter.OnChange := TextPainterChange;
end;

destructor ThtLabel.Destroy;
begin
	TextPainter.Free;
	inherited;
end;

function ThtLabel.GetHAlign: TLrHAlign;
begin
	Result := TextPainter.HAlign;
end;

function ThtLabel.GetWordWrap: Boolean;
begin
	Result := TextPainter.WordWrap;
end;

procedure ThtLabel.SetHAlign(const Value: TLrHAlign);
begin
	TextPainter.HAlign := Value;
	Invalidate;
end;

procedure ThtLabel.SetWordWrap(const Value: Boolean);
begin
	TextPainter.WordWrap := Value;
	Invalidate;
	AdjustSize;
end;

procedure ThtLabel.PerformAutoSize;
begin
	SetBounds(Left, Top, TextPainter.Width(Caption),
		TextPainter.Height(Caption, ClientRect));
end;

procedure ThtLabel.StylePaint;
begin
	inherited;
	TextPainter.PaintText(Caption, ClientRect);
end;

procedure ThtLabel.Generate(const inContainer: string;
	inMarkup: ThtMarkup);

	function Prespace(const inString: string): string;
	begin
		if inString = '' then
			Result := ''
		else
			Result := ' ' + inString;
	end;

begin
	GenerateStyle('#' + Name, inMarkup);
	inMarkup.Styles.Add(
		Format('#%s { width: %dpx; height: %dpx }',
			[ Name, Width, Height ]));
	inMarkup.Add(
		Format('<span id="%s"%s>%s</span>',
			[ Name, Prespace(ExtraAttributes), Caption ]));
	//inMarkup.Add(
	//	Format('<div id="%s"%s>%s</div>',
	//		[ Name, Prespace(ExtraAttributes), Caption ]));
	//inMarkup.Add(Caption);
end;

end.
