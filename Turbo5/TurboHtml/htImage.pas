unit htImage;

interface

uses
	SysUtils, Types, Classes, Controls, Graphics,
	LrGraphics,
	htPicture, htControls, htMarkup;

type
	ThtImage = class(ThtGraphicControl)
	private
		FAltText: string;
		FHAlign: TLrHAlign;
		FPicture: ThtPicture;
		FAutoAspect: Boolean;
	protected
		function CalcSize: TPoint;
		function GetAltText: string;
		function GetHAlign: TLrHAlign;
		procedure Generate(const inContainer: string;
			inMarkup: ThtMarkup); override;
		procedure PerformAutoSize; override;
		procedure PictureChange(inSender: TObject);
		procedure SetAltText(const Value: string);
		procedure SetAutoAspect(const Value: Boolean);
		procedure SetHAlign(const Value: TLrHAlign);
		procedure SetPicture(const Value: ThtPicture);
		procedure StylePaint; override;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
	published
		property Align;
		property AltText: string read GetAltText write SetAltText;
		property AutoAspect: Boolean read FAutoAspect write SetAutoAspect;
		property AutoSize;
		property HAlign: TLrHAlign read GetHAlign write SetHAlign;
		property Caption;
		property Outline;
		property Picture: ThtPicture read FPicture write SetPicture;
		property Style;
	end;

implementation

{ ThtImage }

constructor ThtImage.Create(inOwner: TComponent);
begin
	inherited;
	FPicture := ThtPicture.Create;
	FPicture.OnChange := PictureChange;
	ShowHint := true;
end;

destructor ThtImage.Destroy;
begin
	FPicture.Free;
	inherited;
end;

procedure ThtImage.PictureChange(inSender: TObject);
begin
	AdjustSize;
	Invalidate;
end;

function ThtImage.GetHAlign: TLrHAlign;
begin
	Result := FHAlign;
end;

function ThtImage.GetAltText: string;
begin
	if FAltText <> '' then
		Result := FAltText
	else
		Result := Name;
end;

procedure ThtImage.SetAltText(const Value: string);
begin
	FAltText := Value;
	Hint := AltText;
end;

procedure ThtImage.SetAutoAspect(const Value: Boolean);
begin
	FAutoAspect := Value;
	Invalidate;
end;

procedure ThtImage.SetHAlign(const Value: TLrHAlign);
begin
	FHAlign := Value;
	Invalidate;
end;

procedure ThtImage.SetPicture(const Value: ThtPicture);
begin
	FPicture.Assign(Value);
	Invalidate;
end;

function ThtImage.CalcSize: TPoint;
begin
	with Result do
		if not Picture.HasGraphic then
		begin
			X := 0;
			Y := 0;
		end
		else begin
			X := Picture.Picture.Width;
			Y := Picture.Picture.Height;
			if AutoAspect then
				Result := LrCalcAspectSize(ClientWidth, ClientHeight, X, Y);
		end;
end;

procedure ThtImage.PerformAutoSize;
begin
	with CalcSize do
		SetBounds(Left, Top, X, Y)
end;

procedure ThtImage.StylePaint;
begin
	inherited;
	if Picture.HasGraphic then
		if AutoAspect then
			LrAspectPaintPicture(Canvas, Picture.Picture, ClientRect, HAlign, vaTop)
		else
			LrPaintPicture(Canvas, Picture.Picture, ClientRect, HAlign, vaTop);
	//Canvas.Draw(0, 0, Picture.Graphic);
end;

procedure ThtImage.Generate(const inContainer: string;
	inMarkup: ThtMarkup);
var
	s: string;
begin
	s := CtrlStyle.InlineAttribute;
	if (s <> '') then
		inMarkup.Styles.Add(Format('#%s {%s }', [ inContainer, s ]));
	if AutoAspect then
	begin
		with CalcSize do
			if X = ClientWidth then
				s := Format(' width: %dpx', [ X ])
			else
				s := Format(' height: %dpx', [ Y ]);
		inMarkup.Styles.Add(Format('#%s {%s }', [ Name, s ]));
	end;
	inMarkup.Add(
		Format('<img id="%s" src="file:///%s" alt="%s"/>',
			[ Name, Picture.Filename, AltText ])
	);
end;

end.
