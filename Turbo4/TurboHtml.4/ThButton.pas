unit ThButton;

interface

uses
	Windows, SysUtils, Types, Classes, Controls, Graphics, ExtCtrls,
	ThInterfaces, ThCssStyle, ThTextPaint, ThWebControl, ThTag;

type
	TThButtonType = ( btSubmit, btReset, btCustom );
	//
	TThCustomButton = class(TThWebGraphicControl, IThFormInput)
	private
		FButtonType: TThButtonType;
	protected
		function ButtonHeight: Integer;
		function ButtonWidth: Integer;
		procedure Paint; override;
		procedure PerformAutoSize; override;
		procedure Tag(inTag: TThTag); override;
	protected
		property AutoSize default true;
		property ButtonType: TThButtonType read FButtonType write FButtonType
			default btSubmit;
	public
		constructor Create(AOwner: TComponent); override;
		procedure CellTag(inTag: TThTag); override;
		//function GetCellStyleAttribute: string; override;
	end;
	//
	TThButton = class(TThCustomButton)
	published
		property Align;
		property AutoSize default true;
		property ButtonType;
		property Caption;
		property Style;
		property StyleClass;
		property Visible;
	public
		constructor Create(AOwner: TComponent); override;
	end;

const
	htThButtonTypes: array[TThButtonType] of string =
		( 'submit', 'reset', 'button' );

implementation

{ TThCustomButton }

constructor TThCustomButton.Create(AOwner: TComponent);
begin
	inherited;
	AutoSize := true;
	Style.Border.DefaultBorderPixels := 2;
	CtrlStyle.Font.DefaultFontFamily := 'MS Sans Serif';
	CtrlStyle.Font.DefaultFontPx := 14;
end;

function TThCustomButton.ButtonWidth: Integer;
begin
	if AutoSize then
	begin
		Result := ThTextWidth(Canvas, Caption);
		Result := Result * 140 div 100;
		Result := Result + Style.GetBoxWidthMargin;
	end	else
		Result := Width;
end;

function TThCustomButton.ButtonHeight: Integer;
begin
	if AutoSize then
	begin
		Result := ThTextHeight(Canvas, Caption) + 6; //8;
//		if Style.Borders.BordersVisible then
//			Result := Result + 5;
//		else
//			Result := Result + 9;
		if (Result < 0) then
			Result := Height
		else
			Result := Result + Style.GetBoxHeightMargin;
	end	else
		Result := Height;
end;

procedure TThCustomButton.PerformAutoSize;
begin
	Canvas.Font := Font;
	SetBounds(Left, Top, ButtonWidth, ButtonHeight);
end;

procedure TThCustomButton.Paint;
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
	if not ThVisibleColor(CtrlStyle.Color) then
		Painter.Color := clBtnFace;
	Painter.PaintBackground;
	Painter.PaintOutsetBorders;
	//
	Canvas.Font := Font;
	ThPaintText(Canvas, Caption, r, haCenter, vaMiddle);
end;

procedure TThCustomButton.CellTag(inTag: TThTag); 
begin
	inherited;
	Style.Font.ListStyles(inTag.Styles);
	Style.Padding.ListStyles(inTag.Styles);
end;

{
function TThCustomButton.GetCellStyleAttribute: string;
begin
	with Style do
		Result :=	Font.InlineAttribute + Padding.InlineAttribute;
	if Result <> '' then
		Result := ' style="' + Result + '"';
end;
}

procedure TThCustomButton.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Element := 'input';
		Add('name', Name);
		Add('type', htThButtonTypes[ButtonType]);
		Add('value', Caption);
		AddStyle('width', '100%');
		AddStyle('height', '100%');
{
		if not AutoSize then
		begin
			AddStyle('width', Width, 'px');
			AddStyle('height', Height, 'px');
		end;
}
		Style.Border.ListStyles(Styles);
	end;
end;

{ TThButton }

constructor TThButton.Create(AOwner: TComponent);
begin
	inherited;
	FAutoSize := true;
end;

end.
