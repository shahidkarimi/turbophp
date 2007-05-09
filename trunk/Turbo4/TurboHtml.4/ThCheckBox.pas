unit ThCheckBox;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThInterfaces, ThTextPaint, ThWebControl, ThTag;

type
	TThCustomStateBox = class(TThWebGraphicControl, IThFormInput)
	private
		FChecked: Boolean;
	protected
		function GetHtmlAsString: string; override;
		procedure SetChecked(const Value: Boolean);
	protected
		function ButtonHeight: Integer;
		function ButtonWidth: Integer;
		function GetCaptionContent: string; virtual;
		function GetInputName: string; virtual;
		function GetInputType: string; virtual;
		procedure PaintCheck(inX, inY: Integer); virtual;
		procedure Paint; override;
		procedure PerformAutoSize; override;
		procedure Tag(inTag: TThTag); override;
	protected
		property AutoSize default true;
		property Checked: Boolean read FChecked write SetChecked;
	public
		constructor Create(AOwner: TComponent); override;
		procedure CellTag(inTag: TThTag); override;
	end;
	//
	TThCheckBox = class(TThCustomStateBox)
	published
		property Align;
		property AutoSize default true;
		property Caption;
		property Checked;
		property Style;
		property StyleClass;
		property Visible;
	end;
	//
	TThRadio = class(TThCustomStateBox)
	private
		FGroup: Integer;
	protected
		function GetInputName: string; override;
		function GetInputType: string; override;
		procedure SetGroup(const Value: Integer);
	protected
		procedure PaintCheck(inX, inY: Integer); override;
	published
		property Align;
		property AutoSize default true;
		property Caption;
		property Checked;
		property Group: Integer read FGroup write SetGroup;
		property Style;
		property StyleClass;
		property Visible;
	end;

implementation

{$R ThResources.res}

var
	ThCheckImages: TImageList;

function ThNeedCheckImages: Boolean;
var
	b: TBitmap;
begin
	if ThCheckImages = nil then
	try
		b := TBitmap.Create;
		try
			ThCheckImages := TImageList.Create(nil);
			with ThCheckImages do
			begin
				Width := 12;
				Height := 12;
				b.LoadFromResourceName(HInstance, 'TRBOCHECKOFF');
				AddMasked(b, clFuchsia);
				b.LoadFromResourceName(HInstance, 'TRBOCHECKON');
				AddMasked(b, clFuchsia);
				b.LoadFromResourceName(HInstance, 'TRBORADIOOFF');
				AddMasked(b, clFuchsia);
				b.LoadFromResourceName(HInstance, 'TRBORADIOON');
				AddMasked(b, clFuchsia);
			end;
		finally
			b.Free;
		end;
	except
	end;
	Result := (ThCheckImages <> nil);
end;

{ TThCustomStateBox }

constructor TThCustomStateBox.Create(AOwner: TComponent);
begin
	inherited;
	AutoSize := true;
	//CtrlStyle.Borders.SetDefaultBorderPixels(2);
	//CtrlStyle.Font.DefaultFontFamily := 'MS Sans Serif';
	//CtrlStyle.Font.DefaultFonThx := 14;
end;

function TThCustomStateBox.ButtonWidth: Integer;
begin
	if AutoSize then
	begin
		Result := ThTextWidth(Canvas, Caption);
		Result := Result * 140 div 100;
		Result := Result + Style.GetBoxWidthMargin;
	end	else
		Result := Width;
end;

function TThCustomStateBox.ButtonHeight: Integer;
begin
	if AutoSize then
	begin
		Result := ThTextHeight(Canvas, Caption) + 5;
		if (Result < 0) then
			Result := Height
		else
			Result := Result + Style.GetBoxHeightMargin;
	end	else
		Result := Height;
end;

procedure TThCustomStateBox.PerformAutoSize;
begin
	Canvas.Font := Font;
	SetBounds(Left, Top, ButtonWidth, ButtonHeight);
end;

procedure TThCustomStateBox.PaintCheck(inX, inY: Integer);
const
	cThCheckImageIndex: array[Boolean] of Integer = ( 0, 1 );
begin
	ThCheckImages.Draw(Canvas, inX, inY, cThCheckImageIndex[Checked]);
end;

procedure TThCustomStateBox.Paint;
var
	r: TRect;
begin
	inherited;
	ThNeedCheckImages;
	r := AdjustedClientRect;
	PaintCheck(r.Left + 2, r.Top + 4);
	r := Rect(r.Left + 18, r.Top + 1, r.Right, r.Bottom);
	ThPaintText(Canvas, Caption, r);
end;

{
procedure TThCustomStateBox.Paint;
var
	r: TRect;
begin
	r := ClientRect;
	if (Parent is TWinControl) then
		Canvas.Brush.Color := TPanel(Parent).Color
	else
		Canvas.Brush.Color := Color;
	Canvas.FillRect(r);
	Style.UnpadRect(r);
	//
	Painter.Prepare(Color, CtrlStyle, Canvas, r);
	if not TbhVisibleColor(CtrlStyle.Color) then
		Painter.Color := clBtnFace;
	Painter.PaintBackground;
	Painter.PaintOutsetBorders;
	//
	Canvas.Font := Font;
	TbhPaintText(Canvas, Caption, r, haCenter, vaMiddle);
end;
}

function TThCustomStateBox.GetInputName: string;
begin
	Result := Name;
end;

function TThCustomStateBox.GetInputType: string;
begin
	Result := 'checkbox';
end;

procedure TThCustomStateBox.CellTag(inTag: TThTag);
begin
	inherited;
	inTag.Attributes['valign'] := 'middle';
end;

function TThCustomStateBox.GetCaptionContent: string;
begin
	//Result := Caption;
	Result := Format('<label for="%s">%s</label>', [ Name, Caption ]);
end;

procedure TThCustomStateBox.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Element := 'input';
		Add('type', GetInputType);
		Add('name', GetInputName);
		Add('value', Name);
		if Checked then
			Add('checked', 'checked');
		Add('id', Name);
		//Content := GetCaptionContent;
	end;
end;

function TThCustomStateBox.GetHtmlAsString: string;
begin
	Result := Format('<label>%s%s</label>', [ GetTagHtml, Caption ]);
end;

procedure TThCustomStateBox.SetChecked(const Value: Boolean);
begin
	FChecked := Value;
	Invalidate;
end;

{ TThRadio }

function TThRadio.GetInputType: string;
begin
	Result := 'radio';
end;

function TThRadio.GetInputName: string;
begin
	Result := 'RadioGroup' + IntToStr(Group);
end;

procedure TThRadio.PaintCheck(inX, inY: Integer);
const
	cThRadioImageIndex: array[Boolean] of Integer = ( 2, 3 );
begin
	ThCheckImages.Draw(Canvas, inX, inY, cThRadioImageIndex[Checked]);
end;

procedure TThRadio.SetGroup(const Value: Integer);
begin
	FGroup := Value;
end;

end.
