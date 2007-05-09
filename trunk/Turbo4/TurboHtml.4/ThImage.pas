unit ThImage;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThCssStyle, ThStyledCtrl, ThWebControl, ThPicture, ThTag, ThAnchor,
	ThTextPaint, ThLabel, ThAttributeList, ThStyleList, ThMessages;

type
	TThCustomImage = class(TThWebGraphicControl)
	private
		FPicture: TThPicture;
		FAutoAspect: Boolean;
		FAnchor: TThAnchor;
		FBorder: Integer;
		FAltText: string;
		FHAlign: TThHAlign;
		FVAlign: TThVAlign;
	protected
		function GetHtmlAsString: string; override;
		function GetImageSrc: string; virtual;
		procedure SetAltText(const Value: string);
		procedure SetAnchor(const Value: TThAnchor);
		procedure SetAutoAspect(const Value: Boolean);
		procedure SetBorder(const Value: Integer);
		procedure SetHAlign(const Value: TThHAlign);
		procedure SetPicture(const Value: TThPicture);
		procedure SetVAlign(const Value: TThVAlign);
	protected
		function CalcPaintPosition(inW, inH: Integer): TPoint;
		function CreateAnchor: TThAnchor; virtual;
		function CreatePicture: TThPicture; virtual;
		function ShouldAutoSize: Boolean; override;
		procedure PerformAutoSize; override;
		procedure Paint; override;
		procedure PaintBorder; virtual;
		procedure PaintImage; virtual;
		procedure PictureChange(inSender: TObject); virtual;
		procedure Tag(inTag: TThTag); override;
		procedure ThmUpdatePicture(var inMessage); message THM_UPDATEPICTURE;
	protected
		property AltText: string read FAltText write SetAltText;
		property Anchor: TThAnchor read FAnchor write SetAnchor;
		property AutoAspect: Boolean read FAutoAspect write SetAutoAspect;
		property AutoSize default true;
		property Border: Integer read FBorder write SetBorder default 0;
		property HAlign: TThHAlign read FHAlign write SetHAlign default haDefault;
		property Picture: TThPicture read FPicture write SetPicture;
		property VAlign: TThVAlign read FVAlign write SetVAlign default vaDefault;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure CellTag(inTag: TThTag); override;
	end;
	//
	TThImage = class(TThCustomImage)
	published
		property Align;
		property AltText;
		property Anchor;
		property AutoAspect;
		property AutoSize;
		property Border;
		property HAlign;
		property Picture;
		property Style;
		property StyleClass;
		property VAlign;
		property Visible;
	end;

implementation

uses
	Math;

{ TThCustomImage }

constructor TThCustomImage.Create(AOwner: TComponent);
begin
	inherited;
	FAutoSize := true;
	FPicture := CreatePicture;
	FPicture.OnChange := PictureChange;
	FAnchor := CreateAnchor;
	FAnchor.OnChange := PictureChange;
end;

function TThCustomImage.CreateAnchor: TThAnchor;
begin
	Result := TThAnchor.Create;
end;

function TThCustomImage.CreatePicture: TThPicture;
begin
	Result := TThPicture.Create(Self);
end;

destructor TThCustomImage.Destroy;
begin
	FAnchor.Free;
	FPicture.Free;
	inherited;
end;

procedure TThCustomImage.PictureChange(inSender: TObject);
begin
	Invalidate;
	AdjustSize;
end;

procedure TThCustomImage.ThmUpdatePicture(var inMessage);
begin
	Picture.ResolvePicturePath;
	Invalidate;
end;

procedure TThCustomImage.CellTag(inTag: TThTag);
begin
	with inTag do
	begin
		if not AutoSize and not AutoAspect then
			case Align of
				alLeft, alClient, alRight: Add('height', '100%');
				else Add('height', Height);
			end;
		case Align of
			alLeft, alRight: Add('width', Width);
		end;
		Add('align', ssHAlign[HAlign]);
		Add('valign', ssVAlign[VAlign]);
		Style.Font.ListStyles(Styles);
		Style.Padding.ListStyles(Styles);
		Styles.AddColor('background-color', Color);
	end;
end;

function TThCustomImage.GetImageSrc: string;
begin
	Result := Picture.PictureUrl;
end;

procedure TThCustomImage.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Element := 'img';
		Add('src', GetImageSrc);
		if not AutoSize or AutoAspect then
			case Align of
				alTop, alBottom: ;
				else Add('width', AdjustedClientWidth);
			end;
		if AutoAspect then
			case Align of
				alTop, alBottom: Add('height', AdjustedClientHeight);
			end
		else if not AutoSize then
			Add('height', AdjustedClientHeight);
		if (AltText = '') then
			Add('alt', Name)
		else
			Add('alt', AltText);
		Add('border', Border);
		Style.Border.ListStyles(Styles);
	end;
end;

function TThCustomImage.GetHtmlAsString: string;
begin
	Result := Anchor.Wrap(inherited GetHtmlAsString);
end;

procedure TThCustomImage.PaintBorder;
begin
	if not CtrlStyle.Border.BorderVisible then
	begin
		Canvas.Pen.Color := clBlue;
		Canvas.Pen.Style := psDash;
		Canvas.Brush.Style := bsClear;
		Canvas.Rectangle(AdjustedClientRect);
	end;
end;

function TThCustomImage.CalcPaintPosition(inW, inH: Integer): TPoint;
begin
	case HAlign of
		haCenter: Result.x := (AdjustedClientWidth - inW) div 2;
		haRight: Result.x := AdjustedClientWidth - inW;
		else Result.x := 0;
	end;
	case VAlign of
		vaMiddle: Result.y := (AdjustedClientHeight - inH) div 2;
		vaBottom: Result.y := AdjustedClientHeight - inH;
		else Result.y := 0;
	end;
end;

procedure TThCustomImage.PaintImage;
var
	h: Integer;
	r: TRect;
begin
	if AutoSize then
		with Picture do
			with CalcPaintPosition(Width, Height) do
				Canvas.Draw(x, y, Graphic)
	else
		begin
			h := AdjustedClientWidth * Picture.Height div Picture.Width;
			r := AdjustedClientRect;
			r.Bottom := Min(r.Bottom, h + r.Top);
			with CalcPaintPosition(AdjustedClientWidth, h) do
				Inc(r.Top, y);
			Canvas.StretchDraw(r, Picture.Graphic);
			//Canvas.StretchDraw(AdjustedClientRect, Picture.Graphic);
		end;
end;

procedure TThCustomImage.Paint;
begin
	Painter.Prepare(Color, CtrlStyle, Canvas, Rect(0, 0, Width, Height));
	Painter.PaintBackground;
	Painter.PaintRect := AdjustedClientRect;
	Painter.PaintBorders;
	if Picture.HasGraphic then
		PaintImage
	else
		PaintBorder;
end;

function TThCustomImage.ShouldAutoSize: Boolean;
begin
	Result := inherited ShouldAutoSize or AutoAspect;
end;

procedure TThCustomImage.PerformAutoSize;
var
	w, h: Integer;
begin
	if (Picture.Width <> 0) and (Picture.Height <> 0) then
	begin
		if not AutoAspect and (Align in [ alLeft, alRight, alNone]) then
			w := Picture.Width
		else
			w := AdjustedClientWidth;
		if not (Align in [ alTop, alBottom, alNone]) then
			h := AdjustedClientHeight
		else if AutoAspect then
			h := AdjustedClientWidth * Picture.Height div Picture.Width
		else
			h := Picture.Height;
		SetBounds(Left, Top, w, h);
	end;
end;

procedure TThCustomImage.SetAutoAspect(const Value: Boolean);
begin
	FAutoAspect := Value;
	AdjustSize;
end;

procedure TThCustomImage.SetPicture(const Value: TThPicture);
begin
	FPicture.Assign(Value);
	Invalidate;
end;

procedure TThCustomImage.SetAltText(const Value: string);
begin
	FAltText := Value;
	Hint := FAltText;
	ShowHint := true;
end;

procedure TThCustomImage.SetAnchor(const Value: TThAnchor);
begin
	FAnchor.Assign(Value);
	Invalidate;
end;

procedure TThCustomImage.SetBorder(const Value: Integer);
begin
	FBorder := Value;
	Invalidate;
end;

procedure TThCustomImage.SetHAlign(const Value: TThHAlign);
begin
	FHAlign := Value;
	Invalidate;
end;

procedure TThCustomImage.SetVAlign(const Value: TThVAlign);
begin
	FVAlign := Value;
	Invalidate;
end;

end.
