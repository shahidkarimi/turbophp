unit htTurboBox;

interface

uses
	Windows, SysUtils, Types, Classes, Controls, Graphics,
	htInterfaces, htTag, htMarkup, htControls, htPanel, htAjaxPanel;

type
	ThtTurboGraphicControl = class(ThtGraphicControl, IhtAjaxControl)
	protected
		procedure MarkupStyles(const inStyleBase: string; inMarkup: ThtMarkup);
	public
		procedure GenerateTag(inTag: ThtTag;
			inMarkup: ThtMarkup); virtual; abstract;
	end;
	//
	ThtTurboCustomControl = class(ThtCustomControl, IhtAjaxControl)
	protected
		procedure MarkupStyles(const inStyleBase: string; inMarkup: ThtMarkup);
	public
		procedure GenerateTag(inTag: ThtTag;
			inMarkup: ThtMarkup); virtual; abstract;
	end;
	//
	ThtTurboBox = class(ThtTurboCustomControl)
	private
		FBoxPicture: TPicture;
	protected
		procedure AdjustClientRect(var inRect: TRect); override;
		procedure Paint; override;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		procedure GenerateTag(inTag: ThtTag; inMarkup: ThtMarkup); override;
	published
		property Align;
		property Caption;
		property Outline;
		property Style;
		//property Transparent;
		property Visible;
	end;

implementation

uses
	LrVclUtils, LrControlIterator, htPaint;

{ ThtTurboGraphicControl }

procedure ThtTurboGraphicControl.MarkupStyles(const inStyleBase: string;
	inMarkup: ThtMarkup);
var
	s: string;
begin
	case Align of
		alLeft, alRight: s := Format(' width: %dpx;', [ ClientWidth ]);
		alTop, alBottom: s := Format(' height: %dpx;', [ ClientHeight ]);
		else s := '';
	end;
	s := inStyleBase + s;
	if (s <> '') then
		inMarkup.Styles.Add(Format('#%s { %s }', [ Name, s ]));
end;

{ ThtTurboCustomControl }

procedure ThtTurboCustomControl.MarkupStyles(const inStyleBase: string;
	inMarkup: ThtMarkup);
var
	s: string;
begin
	case Align of
		alLeft, alRight: s := Format(' width: %dpx;', [ ClientWidth ]);
		alTop, alBottom: s := Format(' height: %dpx;', [ ClientHeight ]);
		else s := '';
	end;
	s := inStyleBase + s;
	if (s <> '') then
		inMarkup.Styles.Add(Format('#%s { %s }', [ Name, s ]));
end;

{ ThtTurboBox }

constructor ThtTurboBox.Create(inOwner: TComponent);
begin
	inherited;
	FBoxPicture := TPicture.Create;
	FBoxPicture.LoadFromFile('C:\Inetpub\wwwroot\turboajax\test\images\big_box_white.gif');
	ControlStyle := ControlStyle + [ csAcceptsControls ];
end;

destructor ThtTurboBox.Destroy;
begin
	FBoxPicture.Free;
	inherited;
end;

procedure ThtTurboBox.AdjustClientRect(var inRect: TRect);
begin
	with inRect do
		inRect := Rect(Left + 14, Top + 27, Right - 14, Bottom - 18);
end;

procedure ThtTurboBox.GenerateTag(inTag: ThtTag; inMarkup: ThtMarkup);
begin
	with inTag do
	begin
		Element := 'div';
		Attributes['turboAlign'] := AlignToAjaxAlign(Align);
		Attributes['dojoType'] := 'TurboBox';
		Attributes['id'] := Name;
		with Add do
		begin
			Element := 'div';
			Add(Caption);
		end;
	end;
	MarkupStyles(Style.InlineAttribute, inMarkup);
	AjaxGenerateChildren(Self, inTag, inMarkup);
{
	if not included then
	begin
		included := true;
		inMarkup.AddJavaScriptInclude('/turboajax/turbo/widgets/TurboBox.js');
	end;
}
end;

procedure ThtTurboBox.Paint;
const
	margin = 14;
var
	r, src, dst: TRect;
	w, h: Integer;
begin
	inherited;
	r := BoxRect;
	w := BoxWidth;
	h := BoxHeight;
	//
	src := Rect(0, 0, margin, h - margin);
	dst := src;
	OffsetRect(dst, r.Left, r.Top);
	Canvas.CopyRect(dst, FBoxPicture.Bitmap.Canvas, src);
	//
	src := Rect(FBoxPicture.Width - (w - margin), 0, FBoxPicture.Width, h - margin);
	dst := Rect(margin, 0, w, h - margin);
	OffsetRect(dst, r.Left, r.Top);
	Canvas.CopyRect(dst, FBoxPicture.Bitmap.Canvas, src);
	//
	SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
	Canvas.TextOut(dst.Left + margin, dst.Top + 2, Caption);
	//
	src := Rect(0, FBoxPicture.Height - margin, margin, FBoxPicture.Height);
	dst := Rect(0, h - margin, margin, h);
	OffsetRect(dst, r.Left, r.Top);
	Canvas.CopyRect(dst, FBoxPicture.Bitmap.Canvas, src);
	//
	src := Rect(FBoxPicture.Width - (w- margin), FBoxPicture.Height - margin, FBoxPicture.Width, FBoxPicture.Height);
	dst := Rect(margin, h - margin, w, h);
	OffsetRect(dst, r.Left, r.Top);
	Canvas.CopyRect(dst, FBoxPicture.Bitmap.Canvas, src);
	//
end;

end.
