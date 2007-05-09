unit LrBar;

interface

uses
	Windows, Classes, Controls, Messages, StdCtrls, ExtCtrls, Forms, Graphics;

const
	cLrDefaultHue = $00EDBA9E;
	cLrGroupMarginW = 8;

type
	TLrBar = class(TScrollingWinControl)
	private
		FColorSkin: TImageList;
		FGradient: TBitmap;
		FSpecialGradient: TBitmap;
		FSkin: TImageList;
		FSkinHue: TColor;
		FMargin: Integer;
	protected
		procedure SetMargin(const Value: Integer);
		procedure SetSkin(const Value: TImageList);
		procedure SetSkinHue(const Value: TColor);
	protected
		procedure AdjustClientRect(var Rect: TRect); override;
		procedure AlignControls(AControl: TControl; var Rect: TRect); override;
		procedure CreateColorSkin;
		procedure CreateGradient;
		procedure InvalidateChildren;
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		property ColorSkin: TImageList read FColorSkin;
	published
		property Align default alLeft;
		property Color;
		property Enabled;
		property Font;
		property Margin: Integer read FMargin write SetMargin default 16;
		property Skin: TImageList read FSkin write SetSkin;
		property SkinHue: TColor read FSkinHue write SetSkinHue
			default cLrDefaultHue;
		property Visible;
	end;
	//
	TLrGroupHeader = class(TCustomPanel)
	private
		FSpecial: Boolean;
		FGradient: TBitmap;
		FBackColor: TColor;
		FColorSkin: TImageList;
		FClosed: Boolean;
		FHot: Boolean;
		FSpecialFontColor: TColor;
		procedure SetSpecial(const Value: Boolean);
	protected
		procedure SetSpecialFontColor(const Value: TColor);
	protected
		procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure Paint; override;
		procedure PaintBlockHeader;
		procedure PaintGradientHeader;
		procedure SelectHeaderRgn;
	public
		constructor Create(AOwner: TComponent); override;
		property BackColor: TColor read FBackColor write FBackColor;
		property Closed: Boolean read FClosed write FClosed;
		property ColorSkin: TImageList read FColorSkin write FColorSkin;
		property Gradient: TBitmap read FGradient write FGradient;
		property Hot: Boolean read FHot write FHot;
		property Special: Boolean read FSpecial write SetSpecial;
		property SpecialFontColor: TColor read FSpecialFontColor
			write SetSpecialFontColor;
	end;
	//
	TLrGroup = class(TCustomPanel)
	private
		FBorderColor: TColor;
		FClosed: Boolean;
		FClosedHeight: Integer;
		FHeader: TLrGroupHeader;
		FHot: Boolean;
		FOpenHeight: Integer;
		FRollup: Integer;
		FRollupPer: Integer;
		FSpecial: Boolean;
		FTimer: TTimer;
    FMargin: Integer;
	protected
		function GetHeaderColor: TColor;
		function GetHeaderFont: TFont;
		function GetHeaderHeight: Integer;
		function GetHeaderParentFont: Boolean;
		function GetHeaderRect: TRect;
		function GetHeaderSpecialFontColor: TColor;
		procedure SetBorderColor(const Value: TColor);
		procedure SetClosed(const Value: Boolean);
		procedure SetHeaderColor(const Value: TColor);
		procedure SetHeaderFont(const Value: TFont);
		procedure SetHeaderParentFont(const Value: Boolean);
		procedure SetHeaderSpecialFontColor(const Value: TColor);
		procedure SetHot(const Value: Boolean);
    procedure SetMargin(const Value: Integer);
		procedure SetSpecial(const Value: Boolean);
	protected
		procedure AdjustClientRect(var Rect: TRect); override;
		procedure AlignControls(AControl: TControl; var Rect: TRect); override;
		function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
		procedure CMControlChange(
			var Message: TCMControlChange); message CM_CONTROLCHANGE;
		procedure CreateHeader;
		procedure CreateTimer;
		procedure HeaderMouseUp(BSender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure OpenClose;
		procedure Paint; override;
		procedure PrepareHeader;
		procedure Timer(inSender: TObject);
	public
		constructor Create(AOwner: TComponent); override;
		function LrBar: TLrBar;
		property Hot: Boolean read FHot write SetHot;
	published
		property Align;
		property AutoSize;
		property BorderColor: TColor read FBorderColor write SetBorderColor
			default clWhite;
		property Color;
		property Caption;
		property Closed: Boolean read FClosed write SetClosed;
		property Enabled;
		property Font;
		//property Header: TLrGroupHeader read FHeader;
		property HeaderFont: TFont read GetHeaderFont write SetHeaderFont;
		property HeaderParentFont: Boolean read GetHeaderParentFont
			write SetHeaderParentFont;
		property HeaderSpecialFontColor: TColor read GetHeaderSpecialFontColor
			write SetHeaderSpecialFontColor default clWhite;
		property HeaderColor: TColor read GetHeaderColor write SetHeaderColor;
		property Margin: Integer read FMargin write SetMargin
			default cLrGroupMarginW;
		property Special: Boolean read FSpecial write SetSpecial;
		property Visible;
	end;

implementation

uses
	Math, GraphUtil, LrColor;

const
	cLrHeaderMarginH = 6;
	//
	cGradientCount = 3;
	cGradientIndex = 0;
	cGlyphIndex = cGradientIndex + cGradientCount;
	cSpecialOffset = cGlyphIndex + 4;
	cSpecialGradientIndex = cGradientIndex + cSpecialOffset;
	cSpecialGlyphIndex = cSpecialGradientIndex + cGradientCount;
	//
	cClosedHeight = 25;
	cAnimSpeed = 5;
	cTicksToRollup = 8;
	//cRollupPixels = 8;

{ TLrGroupHeader }

constructor TLrGroupHeader.Create(AOwner: TComponent);
begin
	inherited;
	DoubleBuffered := true;
end;

procedure TLrGroupHeader.Paint;
var
	i: Integer;
	r: TRect;
begin
	Canvas.Brush.Color := BackColor;
	Canvas.FillRect(ClientRect);
	//
	SelectHeaderRgn;
	PaintGradientHeader;
	SelectClipRgn(Canvas.Handle, 0);
	//
	if (ColorSkin <> nil) then
	begin
		i := cGlyphIndex + Ord(Closed) * 2 + Ord(Hot)
			+ Ord(Special) * cSpecialOffset;
		ColorSkin.Draw(Canvas, ClientWidth - ColorSkin.Width - 3, 1, i)
	end;
	//
	Canvas.Font := Font;
	if Special then
		Canvas.Font.Color := SpecialFontColor;
	if Hot then
		Canvas.Font.Color :=
			ColorAdjustLuma(ColorToRGB(Canvas.Font.Color), 40, true);
//		Canvas.Font.Color := ColorSetLum(ColorToRGB(Canvas.Font.Color), 96);
	//
	r := ClientRect;
	OffsetRect(r, cLrGroupMarginW, cLrHeaderMarginH);
	SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
	DrawText(Canvas.Handle, PChar(Caption), Length(Caption), r, 0);
	SetBkMode(Canvas.Handle, OPAQUE);
end;

procedure TLrGroupHeader.SelectHeaderRgn;
var
	r, rs: HRGN;
begin
	r := CreateRoundRectRgn(0, 0, ClientWidth + 1, ClientHeight, 6, 6);
	try
		rs := CreateRectRgn(0, 8, ClientWidth + 1, ClientHeight);
		try
			CombineRgn(r, r, rs, RGN_OR);
		finally
			DeleteObject(rs);
		end;
		SelectClipRgn(Canvas.Handle, r);
	finally
		DeleteObject(r);
	end;
end;

procedure TLrGroupHeader.PaintBlockHeader;
begin
	Canvas.Brush.Color := Color;
	Canvas.FillRect(ClientRect);
end;

procedure TLrGroupHeader.PaintGradientHeader;
//var
//	r: TRect;
begin
//	r := ClientRect;
//	r.Right := r.Right - 26;
//	Canvas.StretchDraw(r, Gradient)
	Canvas.StretchDraw(ClientRect, Gradient)
end;

procedure TLrGroupHeader.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
	inherited;
	if not Hot then
	begin
		Hot := true;
		Invalidate;
	end;
end;

procedure TLrGroupHeader.CMMouseLeave(var Message: TMessage);
begin
	inherited;
	if Hot then
	begin
		Hot := false;
		Invalidate;
	end;
end;

procedure TLrGroupHeader.SetSpecialFontColor(const Value: TColor);
begin
	FSpecialFontColor := Value;
	Invalidate;
end;

procedure TLrGroupHeader.SetSpecial(const Value: Boolean);
begin
	FSpecial := Value;
	Invalidate;
end;

{ TLrGroup }

constructor TLrGroup.Create(AOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle + [ csAcceptsControls, csOpaque ];
	DoubleBuffered := true;
	FBorderColor := clWhite;
	FClosedHeight := cClosedHeight;
	FMargin := cLrGroupMarginW;
	CreateHeader;
	CreateTimer;
end;

procedure TLrGroup.CreateHeader;
begin
	FHeader := TLrGroupHeader.Create(Self);
	FHeader.Parent := Self;
	FHeader.OnMouseUp := HeaderMouseUp;
	FHeader.Cursor := crHandPoint;
	FHeader.Align := alNone;
	FHeader.Color := $C45519;
	FHeader.SpecialFontColor := clWhite;
end;

procedure TLrGroup.CreateTimer;
begin
	FTimer := TTimer.Create(Self);
	FTimer.Enabled := False;
	FTimer.Interval := cAnimSpeed;
	FTimer.OnTimer := Timer;
end;

function TLrGroup.CanAutoSize(var NewWidth,
	NewHeight: Integer): Boolean;
var
	i, b: Integer;
begin
	if not FTimer.Enabled then
	begin
		b := 0;
		for i := 0 to Pred(ControlCount) do
			b := Max(b, Controls[i].BoundsRect.Bottom);
		NewHeight := b + cLrHeaderMarginH;
	end;
	Result := true;
end;

function TLrGroup.GetHeaderHeight: Integer;
begin
	Canvas.Font := Font;
	Result := Canvas.TextHeight('W') + cLrHeaderMarginH + cLrHeaderMarginH;
end;

function TLrGroup.GetHeaderRect: TRect;
begin
	Result := Rect(0, 0, ClientWidth, GetHeaderHeight);
end;

procedure TLrGroup.AdjustClientRect(var Rect: TRect);
begin
	inherited;
	InflateRect(Rect, -Margin, -GetHeaderHeight - 4);
	Rect.Top := Rect.Top - FRollup;
end;

procedure TLrGroup.AlignControls(AControl: TControl; var Rect: TRect);
begin
	inherited;
	FHeader.SetBounds(0, 0, ClientWidth, GetHeaderHeight);
end;

procedure TLrGroup.CMControlChange(var Message: TCMControlChange);
begin
	inherited;
	with Message do
		if Inserting then
			with Control do
				Align := alTop;
end;

procedure TLrGroup.PrepareHeader;
begin
	if Parent <> nil then
		FHeader.BackColor := TLabel(Parent).Color;
	FHeader.Caption := Caption;
	FHeader.Closed := Closed;
	//FHeader.Color := HeadColor;
	FHeader.Special := Special;
	if LrBar = nil then
		begin
			FHeader.ColorSkin := nil;
			FHeader.Gradient := nil
		end
	else
		begin
			FHeader.ColorSkin := LrBar.ColorSkin;
			if Special then
				FHeader.Gradient := LrBar.FSpecialGradient
			else
				FHeader.Gradient := LrBar.FGradient;
		end;
end;

procedure TLrGroup.Paint;
begin
	PrepareHeader;
	Canvas.Pen.Color := BorderColor;
	if LrBar <> nil then
		Color := ColorSetLum(LrBar.SkinHue, 232);
	Canvas.Brush.Color := Color;
	Canvas.Rectangle(ClientRect);
end;

procedure TLrGroup.SetBorderColor(const Value: TColor);
begin
	FBorderColor := Value;
	Invalidate;
end;

procedure TLrGroup.SetSpecial(const Value: Boolean);
begin
	FSpecial := Value;
	FHeader.Invalidate;
	Invalidate;
end;

function TLrGroup.LrBar: TLrBar;
begin
	Result := TLrBar(Parent);
end;

procedure TLrGroup.OpenClose;
begin
	FTimer.Enabled := true;
	if not Closed then
	begin
		FOpenHeight := Height;
		FRollupPer := Max(1, Height div cTicksToRollup);
	end;
	FClosed := not FClosed;
end;

procedure TLrGroup.HeaderMouseUp(BSender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	OpenClose;
end;

procedure TLrGroup.SetClosed(const Value: Boolean);
begin
	if FClosed <> Value then
	begin
		FHeader.Invalidate;
		OpenClose;
	end;
end;

procedure TLrGroup.SetHot(const Value: Boolean);
begin
	if FHot <> Value then
	begin
		FHot := Value;
		FHeader.Invalidate;
		Invalidate;
	end;
end;

procedure TLrGroup.Timer(inSender: TObject);
begin
	if Closed then
	begin
		if Height > FClosedHeight then
			begin
				Inc(FRollup, FRollupPer); //cRollupPixels);
				Height := Max(FClosedHeight, Height - FRollupPer); //cRollupPixels);
			end
		else
			begin
				FTimer.Enabled := false;
				//FRollup := OpenHeight - FRollup;
			end;
	end
	else begin
		if Height < FOpenHeight then
			begin
				FRollup := Max(0, FRollup - FRollupPer); //cRollupPixels);
				Height := Min(FOpenHeight, Height + FRollupPer); //cRollupPixels);
			end
		else
			begin
				FTimer.Enabled := false;
				FRollup := 0;
				Realign;
			end;
	end;
end;

function TLrGroup.GetHeaderColor: TColor;
begin
	Result := FHeader.Color;
end;

function TLrGroup.GetHeaderFont: TFont;
begin
	Result := FHeader.Font;
end;

function TLrGroup.GetHeaderParentFont: Boolean;
begin
	Result := FHeader.ParentFont;
end;

function TLrGroup.GetHeaderSpecialFontColor: TColor;
begin
	Result := FHeader.SpecialFontColor;
end;

procedure TLrGroup.SetHeaderColor(const Value: TColor);
begin
	FHeader.Color := Value;
end;

procedure TLrGroup.SetHeaderFont(const Value: TFont);
begin
	FHeader.Font.Assign(Value);
end;

procedure TLrGroup.SetHeaderParentFont(const Value: Boolean);
begin
	FHeader.ParentFont := Value;
end;

procedure TLrGroup.SetHeaderSpecialFontColor(const Value: TColor);
begin
	FHeader.SpecialFontColor := Value;
end;

procedure TLrGroup.SetMargin(const Value: Integer);
begin
	FMargin := Value;
	if not (csLoading in ComponentState) then
	begin
		//FHeader.Invalidate;
		Invalidate;
		Realign;
	end;
end;

{ TLrBar }

constructor TLrBar.Create(AOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle + [ csAcceptsControls ];
	Align := alLeft;
	SkinHue := cLrDefaultHue;
	FMargin := 16;
end;

destructor TLrBar.Destroy;
begin
	ColorSkin.Free;
	FGradient.Free;
	FSpecialGradient.Free;
	inherited;
end;

procedure TLrBar.CreateColorSkin;
var
	b: TBitmap;
	i, x, y: Integer;
begin
	if Skin = nil then
		exit;
	if ColorSkin = nil then
		FColorSkin := TImageList.CreateSize(23, 23);
	ColorSkin.Clear;
	b := TBitmap.Create;
	with b do
	try
		Width := Skin.Width;
		Height := Skin.Height;
		for i := 0 to Pred(Skin.Count) do
		begin
			Skin.GetBitmap(i, b);
			for y := 0 to Pred(Height) do
				for x := 0 to Pred(Width) do
					Canvas.Pixels[x, y] := Colorize(Canvas.Pixels[x, y], SkinHue);
			ColorSkin.Add(b, nil);
		end;
	finally
		b.Free;
	end;
end;

procedure TLrBar.CreateGradient;
const
	cGW = 23;
var
	i, w: Integer;
	di: Single;
begin
	if ColorSkin <> nil then
		w := ColorSkin.Width
	else
		w := cGW;
	//
	if FGradient = nil then
	begin
		FGradient := TBitmap.Create;
		FGradient.Width := w * cGradientCount;
		FGradient.Height := w;
		FGradient.PixelFormat := pf24Bit;
		FSpecialGradient := TBitmap.Create;
		FSpecialGradient.Width := w * cGradientCount;
		FSpecialGradient.Height := w;
		FSpecialGradient.PixelFormat := pf24Bit;
	end;
	//
	if (ColorSkin <> nil) then
		with ColorSkin do
			for i := 0 to Pred(cGradientCount) do
			begin
				Draw(FGradient.Canvas, w*i, 0, cGradientIndex+i);
				Draw(FSpecialGradient.Canvas, w*i, 0, cSpecialGradientIndex+i);
			end
	else
		begin
			for i := 0 to Pred(FGradient.Width) do
			begin
				di := i / cGW;
				di := di * di * di;
				FGradient.Canvas.Pixels[i, 0] :=
					((255 - Round(di * 8)) shl 16)
					+ ((255 - Round(di * 42)) shl 8)
					+ (255 - Round(di * 55));
				FSpecialGradient.Canvas.Pixels[i, 0] :=
					(255 shl 16)
					+ (Round(di * 42) shl 8)
					+ Round(di * 55);
			end;
		end;
end;

procedure TLrBar.AdjustClientRect(var Rect: TRect);
begin
	inherited;
	InflateRect(Rect, -Margin, -Margin);
end;

procedure TLrBar.AlignControls(AControl: TControl; var Rect: TRect);
var
	r: TRect;
	t, w, i: Integer;
begin
	r := ClientRect;
	AdjustClientRect(r);
	t := r.Top;
	//
	w := ClientWidth - Margin - Margin;
	//
	for i := 0 to Pred(ControlCount) do
		with Controls[i] do
		begin
			SetBounds(Margin, t, w, Height);
			if Visible then
				Inc(t, Height + Margin);
		end;
end;

procedure TLrBar.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	if (Operation = opRemove) then
		if AComponent = FSkin then
			FSkin := nil;
end;

procedure TLrBar.SetSkin(const Value: TImageList);
begin
	if FSkin <> nil then
		FSkin.RemoveFreeNotification(Self);
	FSkin := Value;
	if FSkin <> nil then
	begin
		FSkin.FreeNotification(Self);
		CreateColorSkin;
		CreateGradient;
	end;
	Invalidate;
	InvalidateChildren;
end;

procedure TLrBar.SetSkinHue(const Value: TColor);
begin
	FSkinHue := Value;
	if not (csLoading in ComponentState) then
	begin
		Color := ColorSetLum(Value, 166);
		CreateColorSkin;
		CreateGradient;
		Invalidate;
		InvalidateChildren;
	end;
end;

procedure TLrBar.InvalidateChildren;
var
	i: Integer;
begin
	for i := 0 to Pred(ControlCount) do
		Controls[i].Invalidate;
end;

procedure TLrBar.SetMargin(const Value: Integer);
begin
	FMargin := Value;
	Invalidate;
	Realign;
end;

end.
