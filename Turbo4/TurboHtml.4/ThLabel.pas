unit ThLabel;

interface

uses
	Windows, SysUtils, Types, Classes, Controls, Graphics, ExtCtrls,
	ThInterfaces, ThTextPaint, ThWebControl, ThAttributeList,
	ThCssStyle, ThStyleList, ThTag, ThAnchor;

type
	TThCustomLabel = class(TThWebGraphicControl, IThStyleSource)
	private
		FWordWrap: Boolean;
		FHAlign: TThHAlign;
		FVAlign: TThVAlign;
		FFitStyleToText: Boolean;
		FAnchor: TThAnchor;
		FLabelFor: TControl;
	protected
		function GetAnchorStyle: TThCssStyle;
		function GetContent: string; virtual;
		function GetHtmlAsString: string; override;
		function GetString: string; virtual;
		procedure SetAnchor(const Value: TThAnchor);
		procedure SetFitStyleToText(const Value: Boolean);
		procedure SetHAlign(const Value: TThHAlign);
		procedure SetLabelFor(const Value: TControl);
		procedure SetVAlign(const Value: TThVAlign);
		procedure SetWordWrap(const Value: Boolean);
	protected
		function CreateAnchor: TThAnchor; virtual;
		procedure AnchorChange(inSender: TObject);
		procedure BuildStyle; override;
		procedure LabelTag(inTag: TThTag); virtual;
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
		procedure Paint; override;
		procedure PerformAutoSize; override;
		procedure PublishStyles(inStyles: TStringList);
		procedure Tag(inTag: TThTag); override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure CellTag(inTag: TThTag); override;
	public
		property Anchor: TThAnchor read FAnchor write SetAnchor;
		property AutoSize default true;
		property FitStyleToText: Boolean read FFitStyleToText
			write SetFitStyleToText;
		property HAlign: TThHAlign read FHAlign write SetHAlign default haDefault;
		property LabelFor: TControl read FLabelFor write SetLabelFor;
		property WordWrap: Boolean read FWordWrap write SetWordWrap default false;
		property VAlign: TThVAlign read FVAlign write SetVAlign default vaDefault;
	end;
	//
	TThLabel = class(TThCustomLabel)
	published
		property Align;
		property Anchor;
		property AutoSize;
		property Caption;
		property DesignOpaque;
		property FitStyleToText;
		property HAlign;
		property LabelFor;
		property Style;
		property StyleClass;
		property WordWrap;
		property VAlign;
		property Visible;
	end;
	//
	TThText = class(TThCustomLabel)
	private
		FText: TStringList;
    FUseBR: Boolean;
	protected
		function GetContent: string; override;
		function GetString: string; override;
		procedure SetText(const Value: TStringList);
		procedure SetUseBR(const Value: Boolean);
	protected
		procedure TextChange(inSender: TObject);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	published
		property Align;
		property Anchor;
		property AutoSize;
		property DesignOpaque;
		property FitStyleToText;
		property HAlign;
		property LabelFor;
		property Style;
		property StyleClass;
		property Text: TStringList read FText write SetText;
		property UseBR: Boolean read FUseBR write SetUseBR default true;
		property VAlign;
		property Visible;
	end;

const
	ssHAlign: array[TThHAlign] of string = ( '', 'left', 'center', 'right' );
	ssVAlign: array[TThVAlign] of string = ( '', 'top', 'middle', 'bottom',
		'baseline' );

implementation

uses
	ThStyleSheet, ThHtmlPage;

{ TThCustomLabel }

constructor TThCustomLabel.Create(AOwner: TComponent);
begin
	inherited;
	// Using AutoSize setter invokes PerformAutoSize below
	// which clobbers the control's design time display
	// for unknown reasons
	//AutoSize := true;
	FAutoSize := true;
	FAnchor := CreateAnchor;
	FAnchor.OnChange := AnchorChange;
end;

destructor TThCustomLabel.Destroy;
begin
	FAnchor.Free;
	inherited;
end;

function TThCustomLabel.CreateAnchor: TThAnchor;
begin
	Result := TThAnchor.Create;
end;

procedure TThCustomLabel.AnchorChange(inSender: TObject);
begin
	Invalidate;
end;

procedure TThCustomLabel.SetWordWrap(const Value: Boolean);
begin
	FWordWrap := Value;
	AdjustSize;
end;

function TThCustomLabel.GetString: string;
begin
	Result := Caption;
end;

procedure TThCustomLabel.PerformAutoSize;
var
	s: string;
begin
	s := GetString;
	if (Parent <> nil) and (Canvas <> nil) {and (Canvas.HandleAllocated)} then
	try
		case Align of
			alLeft, alRight, alNone:
				if (not WordWrap) and (s <> '') then
					Width := ThTextWidth(Canvas, s) + Style.GetBoxWidthMargin;
		end;
		case Align of
			alTop, alBottom, alNone:
				if (s <> '') then
					Height :=
						ThTextHeight(Canvas, s, AdjustedClientRect,	haDefault, WordWrap)
							+ Style.GetBoxHeightMargin;
		end;
	except
	end;
end;

function TThCustomLabel.GetAnchorStyle: TThCssStyle;
var
	p: TThHtmlPage;
	s: string;
begin
	p := ThFindHtmlPage(Self);
	if p <> nil then
	begin
		s := Anchor.Styles.Link;
		if (s = '') then
			s := p.AnchorStyles.Link;
		Result := ThFindStyleSheetStyle(Self, s);
	end else
		Result := nil;
end;

procedure TThCustomLabel.BuildStyle;
var
	s: TThCssStyle;
begin
	s := GetAnchorStyle;
	if (s = nil) or (Anchor.Empty) then
	begin
		CtrlStyle.Assign(Style);
		if not Anchor.Empty then
			CtrlStyle.Font.FontDecoration := fdUnderline;
	end
	else begin
		CtrlStyle.Assign(s);
		CtrlStyle.Inherit(Style);
	end;
	CtrlStyle.Inherit(SheetStyle);
	CtrlStyle.Font.Inherit(PageStyle.Font);
	CtrlStyle.Font.ToFont(Font);
	Canvas.Font := Font;
	Color := CtrlStyle.Color;
	if not ThVisibleColor(Color) and DesignOpaque then
		if (Parent <> nil) then
			Color := TPanel(Parent).Color;
end;

procedure TThCustomLabel.Paint;
begin
	inherited;
	ThPaintText(Canvas, GetString, AdjustedClientRect, HAlign, VAlign, WordWrap,
		true);
end;

procedure TThCustomLabel.LabelTag(inTag: TThTag);
begin
	with inTag do
	begin
		Attributes.Add('nowrap', not WordWrap);
		Add('class', StyleClass);
		Style.ListStyles(Styles);
		ListJsAttributes(Attributes);
	end;
end;

procedure TThCustomLabel.CellTag(inTag: TThTag);
begin
	with inTag do
	begin
		case Align of
			alLeft, alClient, alRight: Add('height', '100%');
			else Add('height', Height);
		end;
		case Align of
			alTop, alClient, alBottom: ;
			else Add('width', Width);
		end;
		Add('align', ssHAlign[HAlign]);
		Add('valign', ssVAlign[VAlign]);
		if not FitStyleToText then
			LabelTag(inTag);
	end;
end;

function TThCustomLabel.GetContent: string;
begin
	Result := GetString;
end;

procedure TThCustomLabel.Tag(inTag: TThTag);
begin
	with inTag do
	begin
		Content := GetContent;
		if FitStyleToText then
			Element := 'span';
		if LabelFor <> nil then
		begin
			Element := 'label';
			inTag.Add('for', LabelFor.Name);
		end
		else if FitStyleToText then
			Element := 'span';
		if FitStyleToText then
			LabelTag(inTag);
	end;
end;

function TThCustomLabel.GetHtmlAsString: string;
begin
 	Anchor.Name := Name + 'A';
	Result := Anchor.Wrap(inherited GetHtmlAsString);
end;

procedure TThCustomLabel.SetHAlign(const Value: TThHAlign);
begin
	FHAlign := Value;
	Invalidate;
end;

procedure TThCustomLabel.SetVAlign(const Value: TThVAlign);
begin
	FVAlign := Value;
	Invalidate;
end;

procedure TThCustomLabel.SetFitStyleToText(const Value: Boolean);
begin
	FFitStyleToText := Value;
end;

procedure TThCustomLabel.SetAnchor(const Value: TThAnchor);
begin
	FAnchor.Assign(Value);
end;

procedure TThCustomLabel.SetLabelFor(const Value: TControl);
begin
	if FLabelFor <> nil then
		FLabelFor.RemoveFreeNotification(Self);
	FLabelFor := Value;
	if FLabelFor <> nil then
		FLabelFor.FreeNotification(Self);
end;

procedure TThCustomLabel.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	if (Operation = opRemove) and (LabelFor = AComponent) then
		FLabelFor := nil;
end;

procedure TThCustomLabel.PublishStyles(inStyles: TStringList);
begin
	if not Anchor.Empty then
		Anchor.Styles.GenerateStyles(inStyles, ThFindStyleSheet(Self), Anchor.Name);
end;

{ TThText }

constructor TThText.Create(AOwner: TComponent);
begin
	inherited;
	FText := TStringList.Create;
	FText.OnChange := TextChange;
	WordWrap := true;
	FUseBr := true;
end;

destructor TThText.Destroy;
begin
	FText.Free;
	inherited;
end;

function TThText.GetString: string;
begin
	Result := Text.Text;
end;

function TThText.GetContent: string;
begin
	Result := inherited GetContent;
	if UseBr then
		Result := StringReplace(Result, #$D#$A, '<br>', [ rfReplaceAll ]);
	//Result := '<pre>' + inherited GetContent + '</pre>';
end;

procedure TThText.SetText(const Value: TStringList);
begin
	FText.Assign(Value);
end;

procedure TThText.TextChange(inSender: TObject);
begin
	Invalidate;
end;

procedure TThText.SetUseBR(const Value: Boolean);
begin
	FUseBR := Value;
end;

end.
