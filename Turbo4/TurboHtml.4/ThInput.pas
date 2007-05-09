unit ThInput;

interface

uses
	SysUtils, Windows, Types, Classes, Controls, Graphics, StdCtrls, ExtCtrls,
	ThInterfaces, ThCssStyle, ThTextPaint, ThWebControl, ThTag;

type
	TThCustomInput = class(TThWebGraphicControl, IThFormInput)
	private
		FAutoComplete: Boolean;
		FMaxLength: Integer;
		FPassword: Boolean;
		FReadOnly: Boolean;
		FSize: Integer;
		FOnInput: TNotifyEvent;
	protected
		function GetText: string;
		procedure SetText(const Value: string);
		procedure SetAutoComplete(const Value: Boolean);
		procedure SetMaxLength(const Value: Integer);
		procedure SetOnInput(const Value: TNotifyEvent);
		procedure SetPassword(const Value: Boolean);
		procedure SetReadOnly(const Value: Boolean);
		procedure SetSize(const Value: Integer);
	protected
		function DisplayText: string;
		procedure PerformAutoSize; override;
		procedure Input; virtual;
		function InputWidth: Integer;
		function InputHeight: Integer;
		procedure Paint; override;
		procedure Tag(inTag: TThTag); override;
	protected
		property AutoComplete: Boolean read FAutoComplete write SetAutoComplete
			default true;
		property AutoSize default true;
		property OnInput: TNotifyEvent read FOnInput write SetOnInput;
		property Password: Boolean read FPassword write SetPassword;
		property ReadOnly: Boolean read FReadOnly write SetReadOnly default false;
		property Size: Integer read FSize write SetSize default 0;
		property Text: string read GetText write SetText;
		property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
	public
		constructor Create(AOwner: TComponent); override;
		procedure CellTag(inTag: TThTag); override;
		//function GetCellStyleAttribute: string; override;
	end;
	//
	TThInput = class(TThCustomInput)
	published
		//property AccessKey;
		property Align;
		property AutoComplete;
		property AutoSize;
		property Style;
		property Enabled;
		property JavaScript;
		property MaxLength;
		//property OnBeforeGenerate;
		property OnInput;
		property Password;
		property Size;
		property StyleClass;
		//property TabIndex;
		property Text;
		property Visible;
	end;

implementation

const
	cThDefaultInputSize = 12;
	cThPasswordChar = '*';
	cThMetricChar = 'Q';

{ TThCustomInput }

constructor TThCustomInput.Create(AOwner: TComponent);
begin
	inherited;
	FAutoSize := true;
	FAutoComplete := true;
	FSize := 0;
end;

function TThCustomInput.DisplayText: string;
begin
	if Password then
		Result := StringOfChar(cThPasswordChar, Length(Caption))
	else
		Result := Caption;
end;

function TThCustomInput.InputWidth: Integer;
begin
	if AutoSize and not (Align in [ alTop, alBottom, alClient ]) then
	begin
		Result := ThTextWidth(Canvas, cThMetricChar);
		if Size = 0 then
			Result := Result * cThDefaultInputSize
		else
			Result := Result * Size;
		Result := Result * 120 div 100;
		Result := Result + CtrlStyle.GetBoxWidthMargin;
	end	else
		Result := Width;
end;

function TThCustomInput.InputHeight: Integer;
begin
	if AutoSize and not (Align in [ alLeft, alRight, alClient ]) then
	begin
		Result := ThTextHeight(Canvas, 'Tj', Rect(0, 0, 9999, 0)) + 6;
		if (Result < 0) then
			Result := Height
		else
			Result := Result + CtrlStyle.GetBoxHeightMargin;
	end	else
		Result := Height;
end;

procedure TThCustomInput.PerformAutoSize;
begin
	Canvas.Font := Font;
	SetBounds(Left, Top, InputWidth, InputHeight);
end;

procedure TThCustomInput.Paint;
var
	r: TRect;
begin
	r := ClientRect;
	if (Parent is TWinControl) then
		Canvas.Brush.Color := TPanel(Parent).Color //TThHackControl(Parent).Color
	else
		Canvas.Brush.Color := Color;
	Canvas.FillRect(r);
	//
	CtrlStyle.UnpadRect(r);
	Painter.Prepare(Color, CtrlStyle, Canvas, r);
	//
	if not ThVisibleColor(Style.Color) then
		Painter.Color := clWhite
	else
		Painter.Color := clDefault;
	//
	Painter.PaintBackground;
	Painter.PaintInsetBorders;
	//
	Inc(r.Left, 3);
	Canvas.Font := Font;
	ThPaintText(Canvas, DisplayText, r, haLeft, vaMiddle, true);
end;

procedure TThCustomInput.CellTag(inTag: TThTag);
begin
	inherited;
	Style.Font.ListStyles(inTag.Styles);
	Style.Padding.ListStyles(inTag.Styles);
end;

{
function TThCustomInput.GetCellStyleAttribute: string;
begin
	with Style do
		Result := Font.InlineAttribute + Padding.InlineAttribute;
	if Result <> '' then
		Result := ' style="' + Result + '"';
end;
}

procedure TThCustomInput.Tag(inTag: TThTag);
begin
	//inherited;
	with inTag do
	begin
		Element := 'input';
		if Password then
			Add('type', 'password')
		else
			Add('type', 'text');
		Add('id', Name);
		Add('name', Name);
		Add('value', Caption);
		if not AutoComplete then
			Add('autocomplete', 'off');
		if MaxLength > 0 then
			Add('maxlength', MaxLength);
		if Size > 0 then
			Add('size', Size);
		AddStyle('width', '100%');
		//AddStyle('height', '100%');
		AddStyle('height', Height, 'px');
		Add('class', StyleClass);
		Style.Border.ListStyles(Styles);
		ListJsAttributes(Attributes);
	end;
end;

procedure TThCustomInput.SetAutoComplete(const Value: Boolean);
begin
	FAutoComplete := Value;
end;

procedure TThCustomInput.SetMaxLength(const Value: Integer);
begin
	FMaxLength := Value;
	SetText(Text);
end;

procedure TThCustomInput.SetReadOnly(const Value: Boolean);
begin
	FReadOnly := Value;
end;

procedure TThCustomInput.SetSize(const Value: Integer);
begin
	FSize := Value;
	AdjustSize;
end;

procedure TThCustomInput.SetPassword(const Value: Boolean);
begin
	FPassword := Value;
	Invalidate;
end;

function TThCustomInput.GetText: string;
begin
	Result := Caption;
end;

procedure TThCustomInput.SetText(const Value: string);
begin
	if Maxlength > 0 then
		Caption := Copy(Value, 1, Maxlength)
	else
		Caption := Value;
end;

procedure TThCustomInput.Input;
begin
	if Assigned(FOnInput) then
		FOnInput(Self);
end;

procedure TThCustomInput.SetOnInput(const Value: TNotifyEvent);
begin
	FOnInput := Value;
end;

end.
