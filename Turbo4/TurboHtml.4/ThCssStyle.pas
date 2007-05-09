unit ThCssStyle;

interface

uses
	Classes, Messages, SysUtils, Types, Graphics,
	ThMessages, ThStyleList, ThPicture;

type
	TThCssBase = class(TPersistent)
	private
		FOnChanged: TNotifyEvent;
	protected
		procedure Changed; virtual;
	public
		//:$ OnChanged event is fired when a style property is changed.
		property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
	end;
	//
	TThCssFontWeight = ( fwDefault, fwNormal, fwBold );
	TThCssFontSizeRelEnum = ( fsDefault, fs_1_XxSmall, fs_2_XSmall, fs_3_Small,
		fs_4_Medium, fs_5_Large, fs_6_XLarge, fs_7_XxLarge );
	TThCssFontSizeRel = string;
	TThCssFontDecoration = ( fdDefault, fdInherit, fdNone, fdUnderline,
		fdOverline,	fdLineThrough, fdBlink );
	TThCssFontStyle = ( fstDefault, fstInherit, fstNormal, fstItalic,
		fstOblique );
	//
	//:$ Describes CSS font properties.
	TThCssFont = class(TThCssBase)
	private
		FFontSizeRel: TThCssFontSizeRel;
		FFontFamily: TFontName;
		FFontWeight: TThCssFontWeight;
		FFontSizePx: Integer;
		FFontSizePt: Integer;
		FFontColor: TColor;
		FFontDecoration: TThCssFontDecoration;
		FFontStyle: TThCssFontStyle;
		FDefaultFontPx: Integer;
		FDefaultFontFamily: string;
	protected
		procedure SetDefaultFontFamily(const Value: string);
		procedure SetDefaultFontPx(const Value: Integer);
		procedure SetFontColor(const Value: TColor);
		procedure SetFontFamily(const Value: TFontName);
		procedure SetRelFontSize(const Value: TThCssFontSizeRel);
		procedure SetFontWeight(const Value: TThCssFontWeight);
		procedure SetFontSizePx(const Value: Integer);
		procedure SetFontSizePt(const Value: Integer);
		procedure SetFontDecoration(const Value: TThCssFontDecoration);
		procedure SetFontStyle(const Value: TThCssFontStyle);
	protected
		function GetRelFontSize: Integer;
		function GetSizeValue: string;
	public
		//:$ Copy all properties from another style instance.
		procedure Assign(Source: TPersistent); override;
		constructor Create;
		//:$ Returns formatted style attributes.
		//:: InlineAttribute returns style attributes formatted
		//:: to be included in a style sheet or inline style declaration.
		//:: Returned string is a series of attribute:value pairs separated by
		//:: semicolons.
		//:: e.g. <attr>:<value>; <attr>:<value>;
		function InlineAttribute: string;
		//:$ <br>Copy style attributes to a list.
		//:: <br>Adds the attribute values for the style to a list of style
		//:: attributes.
		procedure ListStyles(inStyles: TThStyleList);
		//:$ Converts a TThCssFont to a TFont.
		procedure ToFont(inFont: TFont);
		//:$ Assign properties from an ancestor style.
		//:: Assigns properties from the input style into properties in this style
		//:: that are set to their defaults. Properties in the current style that
		//:: have been customized are not overriden.
		procedure Inherit(inFont: TThCssFont);
	public
		property DefaultFontFamily: string read FDefaultFontFamily write SetDefaultFontFamily;
		property DefaultFontPx: Integer read FDefaultFontPx write SetDefaultFontPx;
		//:$ Font size, in points. Not published to encourage use of 'pixel'
		//:: size instead. Pixel sizes translate better in browsers.
		property FontSizePt: Integer read FFontSizePt write SetFontSizePt
			default 0;
	published
		//:$ Font color.
		property FontColor: TColor read FFontColor write SetFontColor
			default clNone;
		//:$ Font decoration.
		property FontDecoration: TThCssFontDecoration read FFontDecoration
			write SetFontDecoration default fdDefault;
		//:$ Font family (face).
		property FontFamily: TFontName read FFontFamily write SetFontFamily;
		//:$ Font size, in pixels.
		property FontSizePx: Integer read FFontSizePx write SetFontSizePx
			default 0;
		//:$ Relative font size.
		property FontSizeRel: TThCssFontSizeRel read FFontSizeRel
			write SetRelFontSize;
		//:$ Font style.
		property FontStyle: TThCssFontStyle read FFontStyle write SetFontStyle
			default fstDefault;
		//:$ Font weight.
		property FontWeight: TThCssFontWeight read FFontWeight
			write SetFontWeight default fwDefault;
	end;
	//
	//:$ Describes CSS background properties.
	//:: Usually available as a member of a TThCssStyle.
	TThCssBackground = class(TThCssBase)
	private
		//FOwner: TComponent;
		FPicture: TThPicture;
	protected
		procedure SetPicture(const Value: TThPicture);
	protected
		procedure PictureChange(inSender: TObject);
	public
		constructor Create(AOwner: TComponent);
		destructor Destroy; override;
		//:$ Returns formatted style attributes.
		//:: InlineAttribute returns style attributes formatted
		//:: to be included in a style sheet or inline style declaration.
		//:: Returned string is a series of attribute:value pairs separated by
		//:: semicolons.
		//:: e.g. <attr>:<value>; <attr>:<value>;
		function InlineAttribute: string;
		//:$ Copy all properties from another style instance.
		procedure Assign(Source: TPersistent); override;
		//:$ Assign properties from an ancestor style.
		//:: Assigns properties from the input style into properties in this style
		//:: that are set to their defaults. Properties in the current style that
		//:: have been customized are not overriden.
		procedure Inherit(inBackground: TThCssBackground);
		//:$ <br>Copy style attributes to a list.
		//:: <br>Adds the attribute values for the style to a list of style
		//:: attributes.
		procedure ListStyles(inStyles: TThStyleList);
	published
		//$ Optional picture to be used as background.
		//:: Background pictures are tiled to fill the style object.
		property Picture: TThPicture read FPicture write SetPicture;
	end;
	//
	TThCssBorderProp = ( bpWidth, bpStyle, bpColor);
	TThCssBorderStyle = ( bsDefault, bsInherit, bsNone, bsHidden, bsDotted,
		bsDashed, bsSolidBorder, bsGroove, bsRidge, bsInset, bsOutset, bsDouble );
	TThCssBorderSide = ( bsAll, bsLeft, bsTop, bsRight, bsBottom );
	//
	//:$ Describes a CSS border.
	//:: Usually available as a member of a TThCssBorders object.
	TThCssBorder = class(TThCssBase)
	private
		FBorderColor: TColor;
		FBorderPrefix: string;
		FBorderSide: TThCssBorderSide;
		FBorderStyle: TThCssBorderStyle;
		FBorderWidth: string;
		FDefaultBorderPixels: Integer;
		FOwnerStyle: TThCssBase;
	protected
		procedure SetBorderColor(const Value: TColor);
		procedure SetBorderStyle(const Value: TThCssBorderStyle);
		procedure SetBorderWidth(const Value: string);
	public
		constructor Create(inOwner: TThCssBase; inSide: TThCssBorderSide);
		procedure Changed; override;
		function BorderVisible: Boolean; virtual;
		function GetBorderPixels: Integer; overload;
		//:$ Returns formatted style attributes.
		//:: InlineAttribute returns style attributes formatted
		//:: to be included in a style sheet or inline style declaration.
		//:: Returned string is a series of attribute:value pairs separated by
		//:: semicolons.
		//:: e.g. <attr>:<value>; <attr>:<value>;
		function InlineAttribute: string;
		//:$ <br>Copy style attributes to a list.
		//:: <br>Adds the attribute values for the style to a list of style
		//:: attributes.
		procedure ListStyles(inStyles: TThStyleList);
		//:$ Assign properties from an ancestor style.
		//:: Assigns properties from the input style into properties in this style
		//:: that are set to their defaults. Properties in the current style that
		//:: have been customized are not overriden.
		procedure Inherit(inBorder: TThCssBorder);
		procedure Assign(Source: TPersistent); override;
	public
		property BorderSide: TThCssBorderSide read FBorderSide write FBorderSide;
		property DefaultBorderPixels: Integer read FDefaultBorderPixels
			write FDefaultBorderPixels;
	published
		property BorderWidth: string read FBorderWidth write SetBorderWidth;
		property BorderColor: TColor read FBorderColor write SetBorderColor
			default clDefault;
		property BorderStyle: TThCssBorderStyle read FBorderStyle
			write SetBorderStyle default bsDefault;
	end;
	//
	//:$ Describes CSS border properties.
	//:: Aggregates TThCssBorder objects. Usually available as a member of a
	//:: TThCssStyle.
	TThCssBorderDetail = class(TThCssBase)
	private
		FBorderLeft: TThCssBorder;
		FBorderTop: TThCssBorder;
		FBorderRight: TThCssBorder;
		FBorderBottom: TThCssBorder;
	protected
		procedure SetBorderBottom(const Value: TThCssBorder);
		procedure SetBorderLeft(const Value: TThCssBorder);
		procedure SetBorderRight(const Value: TThCssBorder);
		procedure SetBorderTop(const Value: TThCssBorder);
		procedure SetDefaultBorderPixels(inPixels: Integer);
	public
		constructor Create(inOwnerStyle: TThCssBase);
		destructor Destroy; override;
		procedure Assign(Source: TPersistent); override;
		function BorderVisible: Boolean;
		function GetBorder(inSide: TThCssBorderSide): TThCssBorder;
		//:$ Assign properties from an ancestor style.
		//:: Assigns properties from the input style into properties in this style
		//:: that are set to their defaults. Properties in the current style that
		//:: have been customized are not overriden.
		procedure Inherit(inDetail: TThCssBorderDetail);
		//:$ Returns formatted style attributes.
		//:: InlineAttribute returns style attributes formatted
		//:: to be included in a style sheet or inline style declaration.
		//:: Returned string is a series of attribute:value pairs separated by
		//:: semicolons.
		//:: e.g. <attr>:<value>; <attr>:<value>;
		function InlineAttribute: string;
		//:$ <br>Copy style attributes to a list.
		//:: <br>Adds the attribute values for the style to a list of style
		//:: attributes.
		procedure ListStyles(inStyles: TThStyleList);
	public
		property DefaultBorderPixels: Integer write SetDefaultBorderPixels;
	published
		property BorderLeft: TThCssBorder read FBorderLeft write SetBorderLeft;
		property BorderRight: TThCssBorder read FBorderRight
			write SetBorderRight;
		property BorderTop: TThCssBorder read FBorderTop write SetBorderTop;
		property BorderBottom: TThCssBorder read FBorderBottom
			write SetBorderBottom;
	end;
	//
	//:$ Describes CSS border properties.
	//:: Aggregates TThCssBorder objects. Usually available as a member of a
	//:: TThCssStyle.
	TThCssBorders = class(TThCssBorder)
	private
		FEdges: TThCssBorderDetail;
	protected
		procedure SetEdges(const Value: TThCssBorderDetail);
		procedure SetDefaultBorderPixels(inPixels: Integer);
	public
		constructor Create;
		destructor Destroy; override;
		//procedure Changed; override;
		function BorderVisible: Boolean; override;
		function GetBorderColor(inSide: TThCssBorderSide): TColor;
		function GetBorderStyle(inSide: TThCssBorderSide): TThCssBorderStyle;
		function GetBorderPixels(inSide: TThCssBorderSide): Integer; overload;
		procedure Assign(Source: TPersistent); override;
		//:$ Returns formatted style attributes.
		//:: InlineAttribute returns style attributes formatted
		//:: to be included in a style sheet or inline style declaration.
		//:: Returned string is a series of attribute:value pairs separated by
		//:: semicolons.
		//:: e.g. <attr>:<value>; <attr>:<value>;
		function InlineAttribute: string;
		//:$ <br>Copy style attributes to a list.
		//:: <br>Adds the attribute values for the style to a list of style
		//:: attributes.
		procedure ListStyles(inStyles: TThStyleList);
		//:$ Assign properties from an ancestor style.
		//:: Assigns properties from the input style into properties in this style
		//:: that are set to their defaults. Properties in the current style that
		//:: have been customized are not overriden.
		procedure Inherit(inBox: TThCssBorders);
	public
		property DefaultBorderPixels: Integer write SetDefaultBorderPixels;
	published
		property Edges: TThCssBorderDetail read FEdges
			write SetEdges;
	end;
	//
	TThCssPadDetail = class(TThCssBase)
	private
		FOwnerStyle: TThCssBase;
		FPadLeft: Integer;
		FPadBottom: Integer;
		FPadTop: Integer;
		FPadRight: Integer;
	protected
		procedure SetPadding(const Value: Integer);
		procedure SetPadBottom(const Value: Integer);
		procedure SetPadLeft(const Value: Integer);
		procedure SetPadRight(const Value: Integer);
		procedure SetPadTop(const Value: Integer);
	public
		constructor Create(inOwnerStyle: TThCssBase);
		procedure Changed; override;
		//:$ Determine if any padding has been set.
		//:: Returns true if any of the padding properties are non-zero.
		function HasPadding: Boolean;
		//:$ Return the total width of padding.
		//:: Returns PadLeft + PadRight.
		function PadWidth: Integer;
		//:$ Return the total height of padding.
		//:: Returns PadTop + PadBottom.
		function PadHeight: Integer;
		//:$ Copy all properties from another style instance.
		procedure Assign(Source: TPersistent); override;
		//:$ Returns formatted style attributes.
		//:: InlineAttribute returns style attributes formatted
		//:: to be included in a style sheet or inline style declaration.
		//:: Returned string is a series of attribute:value pairs separated by
		//:: semicolons.
		//:: e.g. <attr>:<value>; <attr>:<value>;
		function InlineAttribute: string;
		//:$ <br>Copy style attributes to a list.
		//:: <br>Adds the attribute values for the style to a list of style
		//:: attributes.
		procedure ListStyles(inStyles: TThStyleList);
		//:$ Add attribute values to an HTML node.
		//:: Adds the attribute values for the style class to the list of
		//:: style attributes maintained by the node.
		//procedure Stylize(inNode: TThHtmlNode);
		//:$ Assign properties from an ancestor style.
		//:: Assigns properties from the input style into properties in this style
		//:: that are set to their defaults. Properties in the current style that
		//:: have been customized are not overriden.
		procedure Inherit(inPad: TThCssPadDetail);
	public
		property Padding: Integer write SetPadding;
	published
		property PadLeft: Integer read FPadLeft write SetPadLeft default 0;
		property PadRight: Integer read FPadRight write SetPadRight default 0;
		property PadTop: Integer read FPadTop write SetPadTop default 0;
		property PadBottom: Integer read FPadBottom write SetPadBottom default 0;
	end;
	//
	//:$ Describes CSS padding properties.
	//:: Usually available as a member of a TThCssStyle.
	TThCssPadding = class(TThCssBase)
	private
		FPadding: Integer;
		FPaddingDetails: TThCssPadDetail;
	protected
		procedure SetPadding(const Value: Integer);
		procedure SetPaddingDetails(const Value: TThCssPadDetail);
	public
		constructor Create; 
		destructor Destroy; override;
		//:$ Copy all properties from another style instance.
		procedure Assign(Source: TPersistent); override;
		procedure Changed; override;
		//:$ Determine if any padding has been set.
		//:: Returns true if any of the padding properties are non-zero.
		function HasPadding: Boolean;
		//:$ Assign properties from an ancestor style.
		//:: Assigns properties from the input style into properties in this style
		//:: that are set to their defaults. Properties in the current style that
		//:: have been customized are not overriden.
		procedure Inherit(inPad: TThCssPadding);
		//:$ Returns formatted style attribute.
		//:: InlineAttribute returns a style attribute formatted
		//:: to be included in a style sheet or inline style declaration.
		//:: Returned string is a series of attribute:value pairs separated by
		//:: semicolons.
		//:: e.g. <attr>:<value>; <attr>:<value>;
		function InlineAttribute: string;
		//:$ <br>Copy style attributes to a list.
		//:: <br>Adds the attribute values for the style to a list of style
		//:: attributes.
		procedure ListStyles(inStyles: TThStyleList);
		function PadBottom: Integer;
		//:$ Return the total height of padding.
		//:: Returns PadTop + PadBottom.
		function PadHeight: Integer;
		function PadLeft: Integer;
		function PadRight: Integer;
		function PadTop: Integer;
		//:$ Return the total width of padding.
		//:: Returns PadLeft + PadRight.
		function PadWidth: Integer;
	published
		property Padding: Integer read FPadding write SetPadding default 0;
		property PaddingDetails: TThCssPadDetail read FPaddingDetails
			write SetPaddingDetails;
	end;
	//
	//:$ TThCssStyle combines various style objects. Commonly used as
	//:$ a property in objects needing style information.
	TThCssStyle = class(TThCssBase)
	private
		FBackground: TThCssBackground;
		FBorders: TThCssBorders;
		FColor: TColor;
		FCursor: string;
		FExtraStyles: string;
		FFont: TThCssFont;
		FName: string;
		//FOverflow: TThCssOverflow;
		FOwner: TComponent;
		FPadding: TThCssPadding;
		FParentColor: Boolean;
		FParentFont: Boolean;
	protected
		procedure SetBackground(const Value: TThCssBackground);
		procedure SetBorders(const Value: TThCssBorders);
		procedure SetColor(const Value: TColor);
		procedure SetCursor(const Value: string);
		procedure SetExtraStyles(const Value: string);
		procedure SetFont(const Value: TThCssFont);
		procedure SetName(const Value: string);
		procedure SetPadding(const Value: TThCssPadding);
		//procedure SetOverflow(const Value: TThCssOverflow);
		procedure SetParentColor(const Value: Boolean);
		procedure SetParentFont(const Value: Boolean);
	protected
		procedure InternalChanged(inSender: TObject); virtual;
	public
		constructor Create(AOwner: TComponent); virtual;
		destructor Destroy; override;
		function InlineColorAttribute: string;
		function StyleAttribute: string;
		//:$ Returns formatted style attributes.
		//:: InlineAttribute returns style attributes formatted
		//:: to be included in a style sheet or inline style declaration.
		//:: Returned string is a series of attribute:value pairs separated by
		//:: semicolons.
		//:: e.g. <attr>:<value>; <attr>:<value>;
		function InlineAttribute: string;
		//:$ <br>Copy style attributes to a list.
		//:: <br>Adds the attribute values for the style to a list of style
		//:: attributes.
		procedure ListStyles(inStyles: TThStyleList);
		procedure Assign(Source: TPersistent); override;
		procedure AssignFromParent(inParent: TThCssStyle);
		procedure Inherit(inStyle: TThCssStyle);
		procedure ToFont(inFont: TFont);
		function GetBoxLeft: Integer;
		function GetBoxTop: Integer;
		function GetBoxRight: Integer;
		function GetBoxBottom: Integer;
//		procedure ExpandRect(var ioRect: TRect);
		procedure UnpadRect(var ioRect: TRect);
		function GetBoxHeightMargin: Integer;
		function GetBoxWidthMargin: Integer;
		procedure AdjustClientRect(var ioRect: TRect);
		function AdjustHeight(inHeight: Integer): Integer;
		function AdjustWidth(inWidth: Integer): Integer;
		function Owner: TComponent;
	public
		property Name: string read FName write SetName;
		property ParentFont: Boolean read FParentFont write SetParentFont;
		property ParentColor: Boolean read FParentColor write SetParentColor;
	published
		property Background: TThCssBackground read FBackground write SetBackground;
		property Border: TThCssBorders read FBorders write SetBorders;
		property Color: TColor read FColor write SetColor default clNone;
		property Cursor: string read FCursor write SetCursor;
		property ExtraStyles: string read FExtraStyles write SetExtraStyles;
		property Font: TThCssFont read FFont write SetFont;
		//property Overflow: TThCssOverflow read FOverflow write SetOverflow;
		property Padding: TThCssPadding read FPadding write SetPadding;
	end;

function ThVisibleColor(inColor: TColor): Boolean;
function ThColorToHtml(const Color: TColor): string;
function ThInlineStylesToAttribute(const inStyles: string): string;
procedure ThCat(var ioDst: string; const inAdd: string);

const
	ssPx = 'px';
	ssPerc = '%';
	ss100Percent = '100%';
	//
	ssHeight = 'height';
	ssWidth = 'width';
	ssLeft = 'left';
	ssTop = 'top';
	ssRight = 'right';
	ssBottom = 'bottom';
	ssCenter = 'center';
	ssMarginLeft = 'margin-left';
	ssMarginRight = 'margin-right';
	ssPosition = 'position';
	ssTextAlign = 'text-align';
	ssColor = 'color';
	ssBackgroundColor = 'background-color';
	ssBackgroundImage = 'background-image';
	ssBorderStyle = 'border-style';
	ssBorderWidth = 'border-width';
	ssBorderColor = 'border-color';
	ssFont = 'font';
	ssFontFamily = 'font-family';
	ssFontSize = 'font-size';
	ssFontWeight = 'font-weight';
	ssFontStyle = 'font-style';
	ssTextDecoration = 'text-decoration';
	ssPadding = 'padding';
	ssPaddingLeft = 'padding-left';
	ssPaddingRight = 'padding-right';
	ssPaddingTop = 'padding-top';
	ssPaddingBottom = 'padding-bottom';
	ssOverflow = 'overflow';
	ssOverflowX = 'overflow-x';
	ssOverflowY = 'overflow-y';
	ssCursor = 'cursor';
	//
	ssStyle = 'style';
	ssBorderPrefix = 'border';
	ssBorderLeftPrefix = 'border-left';
	ssBorderTopPrefix = 'border-top';
	ssBorderRightPrefix = 'border-right';
	ssBorderBottomPrefix = 'border-bottom';
	//
	ssBorderPrefixi: array[TThCssBorderSide] of string = ( ssBorderPrefix,
		ssBorderLeftPrefix, ssBorderTopPrefix, ssBorderRightPrefix,
		ssBorderBottomPrefix );
	//
	ssBorderStyles: array[TThCssBorderStyle] of string = ( '', 'inherit',
		'none', 'hidden', 'dotted', 'dashed', 'solid', 'groove', 'ridge', 'inset',
		'outset', 'double' );
	//
	//ssPositions: array[TThCssPosition] of string = ( 'absolute', 'relative' );
	//
	ssFontWeights: array[TThCssFontWeight] of string = ( '', 'normal',
		'bold' );
	ssRelFontSizes: array[TThCssFontSizeRelEnum] of string = ( '', 'xx-small',
		'x-small', 'small', 'medium', 'large', 'x-large', 'xx-large' );
	cTFontSizes: array[TThCssFontSizeRelEnum] of Integer =
		( 12, 7, 8, 12, 14, 18, 24, 36 );
	ssTextDecorations: array[TThCssFontDecoration] of string = ( '', 'inherit',
		'none', 'underline', 'overline', 'line-through', 'blink' );
	ssFontStyles: array[TThCssFontStyle] of string = ( '', 'inherit', 'normal',
		'italic', 'oblique' );
	//
//	ssOverflowStyles: array[TThCssOverflowStyle] of string = ( '', 'inherit',
//		'visible', 'hidden', 'scroll', 'auto' );
	//
	ssNumCursors = 19;
	ssCursors: array[0..18] of string = ( '', 'inherit', 'default', 'auto',
		'n-resize', 'ne-resize', 'e-resize', 'se-resize', 's-resize', 'sw-resize',
		'w-resize', 'nw-resize', 'crosshair', 'pointer', 'move', 'text', 'wait',
		'help', 'hand' );

const
	ThDefaultFontFamily: string = 'Times New Roman';
	ThDefaultFontPx: Integer = 16;

var
	NilStyle: TThCssStyle;

implementation

	function ThInlineStylesToAttribute(const inStyles: string): string;
	begin
		if inStyles = '' then
			Result := ''
		else
			Result := ' style="' + inStyles + '"';
	end;

	function ThStringIsDigits(const inString: string): Boolean;
	var
		i: Integer;
	begin
		Result := false;
		for i := 1 to Length(inString) do
			if (inString[i] < '0') or (inString[i] > '9') then
				exit;
		Result := inString <> '';
	end;

	function ThVisibleColor(inColor: TColor): Boolean;
	begin
		Result := (inColor <> clNone) and (inColor <> clDefault);
	end;

	function ThColorToHtml(const Color: TColor): string;
	var
		c: TColor;
	begin
		c := ColorToRGB(Color);
		{$ifndef __ThVcl__}
{
		if Integer(Color) < 0 then
			Result := '#' +
				IntToHex(Byte(c shr 16), 2) +
				IntToHex(Byte(c shr 8), 2) +
				IntToHex(Byte(c), 2)
		else
}
		{$endif}
		Result := '#' +
			IntToHex(Byte(c), 2) +
			IntToHex(Byte(c shr 8), 2) +
			IntToHex(Byte(c shr 16), 2);
	end;

	function ThStyleProp(const inProp, inValue: string): string; overload;
	begin
		if inValue = '' then
			Result := ''
		else
			Result := inProp + ':' + inValue + ';';
	end;

	function ThStyleProp(const inProp: string;
		inColor: TColor): string; overload;
	begin
		if ThVisibleColor(inColor) then
			Result := inProp + ':' + ThColorToHtml(inColor) + ';';
	end;

	function ThPercValue(inPerc: Integer): string;
	begin
		Result := IntToStr(inPerc) + ssPerc;
	end;

	function ThPxValue(inPx: Integer): string;
	begin
		Result := IntToStr(inPx) + ssPx;
	end;

	procedure ThCat(var ioDst: string; const inAdd: string);
	begin
		if (inAdd <> '') then
			if (ioDst = '') then
				ioDst := inAdd
			else
				ioDst := ioDst + ' ' + inAdd;
	end;

{ TThCssBase }

procedure TThCssBase.Changed;
begin
	if Assigned(FOnChanged) then
		FOnChanged(Self);
end;

{ TThCssFont }

constructor TThCssFont.Create;
begin
	FontColor := clNone;
	DefaultFontFamily := ThDefaultFontFamily;
	DefaultFontPx := ThDefaultFontPx;
end;

procedure TThCssFont.SetFontFamily(const Value: TFontName);
begin
	if FFontFamily <> Value then
	begin
		FFontFamily := Value;
		Changed;
	end;
end;

procedure TThCssFont.SetRelFontSize(const Value: TThCssFontSizeRel);
begin
	if FFontSizeRel <> Value then
	begin
		FFontSizeRel := Value;
		Changed;
	end;
end;

procedure TThCssFont.SetFontWeight(const Value: TThCssFontWeight);
begin
	if FFontWeight <> Value then
	begin
		FFontWeight := Value;
		Changed;
	end;
end;

procedure TThCssFont.SetFontSizePx(const Value: Integer);
begin
	if FFontSizePx <> Value then
	begin
		FFontSizePx := Value;
		Changed;
	end;
end;

procedure TThCssFont.SetFontSizePt(const Value: Integer);
begin
	if FFontSizePt <> Value then
	begin
		FFontSizePt := Value;
		Changed;
	end;
end;

procedure TThCssFont.SetFontColor(const Value: TColor);
begin
	if FFontColor <> Value then
	begin
		FFontColor := Value;
		Changed;
	end;
end;

procedure TThCssFont.SetFontDecoration(
	const Value: TThCssFontDecoration);
begin
	FFontDecoration := Value;
	Changed;
end;

procedure TThCssFont.SetFontStyle(const Value: TThCssFontStyle);
begin
	FFontStyle := Value;
	Changed;
end;

function TThCssFont.GetSizeValue: string;
begin
	if (FontSizeRel <> '') then
		Result := FontSizeRel
	else if (FontSizePt <> 0) then
		Result := Format('%dpt', [ FontSizePt ])
	else if (FontSizePx <> 0) then
		Result := ThPxValue(FontSizePx)
	else
		Result := '';
end;

{
procedure TThCssFont.Stylize(inNode: TThHtmlNode);
var
	fs, s: string;
begin
	fs := GetSizeValue;
	if (FontFamily <> '') and (fs <> '') then
	begin
		s := ssFontStyles[FontStyle];
		ThCat(s, ssFontWeights[FontWeight]);
		ThCat(s, fs);
		ThCat(s, FontFamily);
		inNode.AddStyle(ssFont, s);
	end
	else begin
		inNode.AddStyle(ssFontFamily, FontFamily);
		inNode.AddStyle(ssFontSize, fs);
		inNode.AddStyle(ssFontWeight, ssFontWeights[FontWeight]);
		inNode.AddStyle(ssFontStyle, ssFontStyles[FontStyle]);
	end;
	inNode.AddStyle(ssColor, FontColor);
	inNode.AddStyle(ssTextDecoration, ssTextDecorations[FontDecoration]);
end;
}

function TThCssFont.InlineAttribute: string;
var
	fs: string;
begin
	fs := GetSizeValue;
	if (FontFamily <> '') and (fs <> '') then
	begin
		Result := ssFontStyles[FontStyle];
		ThCat(Result, ssFontWeights[FontWeight]);
		ThCat(Result, fs);
		ThCat(Result, FontFamily);
		Result := ThStyleProp(ssFont, Result);
	end
	else begin
		Result := ThStyleProp(ssFontFamily, FontFamily);
		ThCat(Result, ThStyleProp(ssFontSize, fs));
		ThCat(Result, ThStyleProp(ssFontWeight, ssFontWeights[FontWeight]));
		ThCat(Result, ThStyleProp(ssFontStyle, ssFontStyles[FontStyle]));
	end;
	ThCat(Result, ThStyleProp(ssColor, FontColor));
	ThCat(Result, ThStyleProp(ssTextDecoration,
		ssTextDecorations[FontDecoration]));
end;

procedure TThCssFont.ListStyles(inStyles: TThStyleList);
var
	fs, s: string;
begin
	fs := GetSizeValue;
	if (FontFamily <> '') and (fs <> '') then
	begin
		s := ssFontStyles[FontStyle];
		ThCat(s, ssFontWeights[FontWeight]);
		ThCat(s, fs);
		ThCat(s, FontFamily);
		inStyles.Add(ssFont, s);
	end
	else begin
		inStyles.Add(ssFontFamily, FontFamily);
		inStyles.Add(ssFontSize, fs);
		inStyles.Add(ssFontWeight, ssFontWeights[FontWeight]);
		inStyles.Add(ssFontStyle, ssFontStyles[FontStyle]);
	end;
	inStyles.AddColor(ssColor, FontColor);
	inStyles.Add(ssTextDecoration, ssTextDecorations[FontDecoration]);
end;

function TThCssFont.GetRelFontSize: Integer;
var
	i: TThCssFontSizeRelEnum;
begin
	Result := cTFontSizes[fsDefault];
	for i := Low(TThCssFontSizeRelEnum) to High(TThCssFontSizeRelEnum) do
		if (ssRelFontSizes[i] = FontSizeRel) then
		begin
			Result := cTFontSizes[i];
			break;
		end;
end;

procedure TThCssFont.ToFont(inFont: TFont);
var
	s: TFontStyles;
begin
	with inFont do
	begin
		if FontFamily <> '' then
			Name := FontFamily
		else
			Name := DefaultFontFamily;
		s := [];
		if FontWeight = fwBold then
			s := s + [ fsBold ];
		if FontDecoration = fdUnderline then
			s := s + [ fsUnderline ];
		if FontDecoration = fdLineThrough then
			s := s + [ fsStrikeOut ];
		if (FontStyle = fstItalic) or (FontStyle = fstOblique) then
			s := s + [ fsItalic ];
		Style := s;
		if FontSizeRel <> '' then
			Size := GetRelFontSize
		else if (FontSizePt <> 0) then
			Height := -FontSizePt * 4 div 3
		else if (FontSizePx <> 0) then
			Height := -FontSizePx
		else
			Height := -DefaultFontPx;
		if ThVisibleColor(FontColor) then
			Color := FontColor
		else
			Color := clBlack;
	end;
end;

procedure TThCssFont.Inherit(inFont: TThCssFont);
begin
	if inFont = nil then
		exit;
	if (FFontFamily = '') then
		FFontFamily := inFont.FFontFamily;
	if (FontWeight = fwDefault) then
		FFontWeight := inFont.FFontWeight;
	//if (FFontColor = clDefault) then
	if not ThVisibleColor(FFontColor) then
		FFontColor := inFont.FFontColor;
	if (FFontDecoration = fdDefault) then
		FFontDecoration := inFont.FFontDecoration;
	if (FFontStyle = fstDefault) then
		FFontStyle := inFont.FFontStyle;
	if (FFontSizePx = 0) then
	begin
		FFontSizePx := inFont.FFontSizePx;
		if (FFontSizePt = 0) then
		begin
			FFontSizePt := inFont.FFontSizePt;
			if (FFontSizeRel = '') then
				FFontSizeRel := inFont.FFontSizeRel;
		end;
	end;
end;

procedure TThCssFont.Assign(Source: TPersistent);
begin
	if not (Source is TThCssFont) then
		inherited
	else with TThCssFont(Source) do
	begin
		Self.FFontSizeRel := FFontSizeRel;
		Self.FFontFamily := FFontFamily;
		Self.FFontWeight := FFontWeight;
		Self.FFontSizePx := FFontSizePx;
		Self.FFontSizePt := FFontSizePt;
		Self.FFontColor := FFontColor;
		Self.FFontDecoration := FFontDecoration;
		Self.FFontStyle := FFontStyle;
	end;
end;

procedure TThCssFont.SetDefaultFontFamily(const Value: string);
begin
	FDefaultFontFamily := Value;
end;

procedure TThCssFont.SetDefaultFontPx(const Value: Integer);
begin
	FDefaultFontPx := Value;
end;

{ TThCssBackground }

constructor TThCssBackground.Create(AOwner: TComponent);
begin
	inherited Create;
	FPicture := TThPicture.Create(AOwner);
	FPicture.OnChange := PictureChange;
end;

destructor TThCssBackground.Destroy;
begin
	FPicture.Free;
	inherited;
end;

procedure TThCssBackground.Assign(Source: TPersistent);
begin
	if not (Source is TThCssBackground) then
		inherited
	else with TThCssBackground(Source) do
		Self.Picture := Picture;
end;

procedure TThCssBackground.Inherit(inBackground: TThCssBackground);
begin
	if not Picture.HasGraphic then
		Picture := inBackground.Picture;
end;

function TThCssBackground.InlineAttribute: string;
begin
	if Picture.PictureUrl <> '' then
		Result := ThStyleProp(ssBackgroundImage, 'url(' + Picture.PictureUrl + ')')
	else
		Result := '';
end;

procedure TThCssBackground.ListStyles(inStyles: TThStyleList);
begin
	if Picture.PictureUrl <> '' then
		inStyles.Add(ssBackgroundImage, 'url(' + Picture.PictureUrl + ')');
end;

procedure TThCssBackground.PictureChange(inSender: TObject);
begin
	Changed;
end;

procedure TThCssBackground.SetPicture(const Value: TThPicture);
begin
	Picture.Assign(Value);
end;

{ TThCssBorder }

constructor TThCssBorder.Create(inOwner: TThCssBase;
	inSide: TThCssBorderSide);
begin
	FOwnerStyle := inOwner;
	BorderSide := inSide;
	FBorderPrefix := ssBorderPrefixi[inSide];
	FBorderColor := clDefault;
end;

procedure TThCssBorder.Changed;
begin
	inherited;
	if FOwnerStyle <> nil then
		FOwnerStyle.Changed;
end;

function TThCssBorder.BorderVisible: Boolean;
begin
	case BorderStyle of
		bsDefault: Result := DefaultBorderPixels <> 0;
		bsNone, bsHidden: Result := false;
		else Result := true;
	end;
	inherited;
end;

function TThCssBorder.GetBorderPixels: Integer;
const
	cThickBorder = 6;
	cMediumBorder = 4;
	cThinBorder = 2;
	cDefaultBorder = cMediumBorder;
begin
	Result := 0;
	if BorderVisible then
	begin
		if BorderStyle = bsDefault then
			Result := DefaultBorderPixels
		else if BorderWidth = '' then
			Result := cDefaultBorder
		else if BorderWidth = 'thin' then
			Result := cThinBorder
		else if BorderWidth = 'medium' then
			Result := cMediumBorder
		else if BorderWidth = 'thick' then
			Result := cThickBorder
		else
			Result := StrToIntDef(BorderWidth, cDefaultBorder);
	end;
end;

procedure TThCssBorder.SetBorderColor(const Value: TColor);
begin
	FBorderColor := Value;
	Changed;
end;

procedure TThCssBorder.SetBorderStyle(const Value: TThCssBorderStyle);
begin
	FBorderStyle := Value;
	Changed;
end;

procedure TThCssBorder.SetBorderWidth(const Value: string);
begin
	FBorderWidth := Value;
	Changed;
end;

function TThCssBorder.InlineAttribute: string;
var
	bws: string;
begin
	Result := '';
	if ThStringIsDigits(BorderWidth) then
		bws := BorderWidth + 'px'
	else
		bws := BorderWidth;
	if BorderStyle <> bsDefault then
		if (ssBorderStyles[BorderStyle] <> '') and (BorderWidth <> '') and
			ThVisibleColor(BorderColor) then
		begin
			Result := ThStyleProp(FBorderPrefix,
				bws + ' ' + ssBorderStyles[BorderStyle] + ' '
					+ ThColorToHtml(BorderColor));
		end else begin
			ThCat(Result,
				ThStyleProp(FBorderPrefix + '-' + ssStyle,
					ssBorderStyles[BorderStyle]));
			if bws <> '' then
				ThCat(Result,
					ThStyleProp(FBorderPrefix + '-' + ssWidth, bws));
			ThCat(Result,
				ThStyleProp(FBorderPrefix + '-' + ssColor, BorderColor));
		end;
end;

procedure TThCssBorder.ListStyles(inStyles: TThStyleList);
var
	bws: string;
begin
	if ThStringIsDigits(BorderWidth) then
		bws := BorderWidth + 'px'
	else
		bws := BorderWidth;
	if BorderStyle <> bsDefault then
		if (ssBorderStyles[BorderStyle] <> '') and (BorderWidth <> '') and
			ThVisibleColor(BorderColor) then
		begin
			inStyles.Add(FBorderPrefix,
				bws + ' ' + ssBorderStyles[BorderStyle] + ' '
					+ ThColorToHtml(BorderColor));
		end
		else begin
			inStyles.Add(FBorderPrefix + '-' + ssStyle,	ssBorderStyles[BorderStyle]);
			inStyles.Add(FBorderPrefix + '-' + ssWidth, bws);
			inStyles.AddColor(FBorderPrefix + '-' + ssColor, BorderColor);
		end;
end;

procedure TThCssBorder.Assign(Source: TPersistent);
begin
	if not (Source is TThCssBorder) then
		inherited
	else with TThCssBorder(Source) do
	begin
		Self.FBorderWidth := FBorderWidth;
		Self.FBorderStyle := FBorderStyle;
		Self.FBorderColor := FBorderColor;
	end;
end;

procedure TThCssBorder.Inherit(inBorder: TThCssBorder);
begin
	if inBorder.FBorderWidth <> '' then
		FBorderWidth := inBorder.FBorderWidth;
	if inBorder.FBorderStyle <> bsDefault then
		FBorderStyle := inBorder.FBorderStyle;
	if ThVisibleColor(inBorder.FBorderColor) then
		FBorderColor := inBorder.FBorderColor;
end;

{ TThCssBorderDetail }

constructor TThCssBorderDetail.Create(inOwnerStyle: TThCssBase);
begin
	inherited Create;
	FBorderLeft := TThCssBorder.Create(inOwnerStyle, bsLeft);
	FBorderTop := TThCssBorder.Create(inOwnerStyle, bsTop);
	FBorderRight := TThCssBorder.Create(inOwnerStyle, bsRight);
	FBorderBottom := TThCssBorder.Create(inOwnerStyle, bsBottom);
end;

destructor TThCssBorderDetail.Destroy;
begin
	FBorderLeft.Free;
	FBorderTop.Free;
	FBorderRight.Free;
	FBorderBottom.Free;
	inherited;
end;

function TThCssBorderDetail.BorderVisible: Boolean;
begin
	Result := BorderLeft.BorderVisible or
						BorderTop.BorderVisible or
						BorderRight.BorderVisible or
						BorderBottom.BorderVisible;
end;

procedure TThCssBorderDetail.Assign(Source: TPersistent);
begin
	if not (Source is TThCssBorderDetail) then
		inherited
	else with TThCssBorderDetail(Source) do
	begin
		Self.FBorderLeft.Assign(FBorderLeft);
		Self.FBorderTop.Assign(FBorderTop);
		Self.FBorderRight.Assign(FBorderRight);
		Self.FBorderBottom.Assign(FBorderBottom);
	end;
end;

procedure TThCssBorderDetail.Inherit(inDetail: TThCssBorderDetail);
begin
	FBorderLeft.Inherit(inDetail.FBorderLeft);
	FBorderTop.Inherit(inDetail.FBorderTop);
	FBorderRight.Inherit(inDetail.FBorderRight);
	FBorderBottom.Inherit(inDetail.FBorderBottom);
end;

function TThCssBorderDetail.InlineAttribute: string;
begin
	Result := BorderLeft.InlineAttribute
		+ BorderTop.InlineAttribute
		+ BorderRight.InlineAttribute
		+ BorderBottom.InlineAttribute;
end;

procedure TThCssBorderDetail.ListStyles(inStyles: TThStyleList);
begin
	BorderLeft.ListStyles(inStyles);
	BorderTop.ListStyles(inStyles);
	BorderRight.ListStyles(inStyles);
	BorderBottom.ListStyles(inStyles);
end;

function TThCssBorderDetail.GetBorder(inSide: TThCssBorderSide): TThCssBorder;
begin
	case inSide of
		bsLeft: Result := BorderLeft;
		bsTop: Result := BorderTop;
		bsRight: Result := BorderRight;
		bsBottom: Result := BorderBottom;
		else Result := BorderLeft;
	end;
end;

procedure TThCssBorderDetail.SetBorderBottom(const Value: TThCssBorder);
begin
	FBorderBottom.Assign(Value);
end;

procedure TThCssBorderDetail.SetBorderLeft(const Value: TThCssBorder);
begin
	FBorderLeft.Assign(Value);
end;

procedure TThCssBorderDetail.SetBorderRight(const Value: TThCssBorder);
begin
	FBorderRight.Assign(Value);
end;

procedure TThCssBorderDetail.SetBorderTop(const Value: TThCssBorder);
begin
	FBorderTop.Assign(Value);
end;

procedure TThCssBorderDetail.SetDefaultBorderPixels(inPixels: Integer);
begin
	BorderLeft.DefaultBorderPixels := inPixels;
	BorderTop.DefaultBorderPixels := inPixels;
	BorderRight.DefaultBorderPixels := inPixels;
	BorderBottom.DefaultBorderPixels := inPixels;
end;

{ TThCssBorders }

constructor TThCssBorders.Create;
begin
	inherited Create(nil, bsAll);
	FEdges := TThCssBorderDetail.Create(Self);
	Changed;
end;

destructor TThCssBorders.Destroy;
begin
	FEdges.Free;
	inherited;
end;

procedure TThCssBorders.Assign(Source: TPersistent);
begin
	inherited;
	if (Source is TThCssBorders) then
		with TThCssBorders(Source) do
			Self.Edges.Assign(Edges);
end;

procedure TThCssBorders.Inherit(inBox: TThCssBorders);
begin
	inherited Inherit(inBox);
	Edges.Inherit(inBox.Edges);
end;

function TThCssBorders.InlineAttribute: string;
begin
	if BorderStyle <> bsDefault then
		Result := inherited InlineAttribute
	else
		Result := Edges.InlineAttribute;
end;

procedure TThCssBorders.ListStyles(inStyles: TThStyleList);
begin
	if BorderStyle <> bsDefault then
		inherited ListStyles(inStyles)
	else
		Edges.ListStyles(inStyles);
end;

function TThCssBorders.GetBorderColor(inSide: TThCssBorderSide): TColor;
begin
	if BorderStyle <> bsDefault then
		Result := BorderColor
	else
		Result := Edges.GetBorder(inSide).BorderColor;
end;

function TThCssBorders.GetBorderStyle(
	inSide: TThCssBorderSide): TThCssBorderStyle;
begin
	if BorderStyle <> bsDefault then
		Result := BorderStyle
	else
		Result := Edges.GetBorder(inSide).BorderStyle;
end;

function TThCssBorders.GetBorderPixels(inSide: TThCssBorderSide): Integer;
begin
	if BorderStyle <> bsDefault then
		Result := GetBorderPixels
	else
		Result := Edges.GetBorder(inSide).GetBorderPixels;
end;

function TThCssBorders.BorderVisible: Boolean;
begin
	Result := inherited BorderVisible or Edges.BorderVisible;
end;

procedure TThCssBorders.SetDefaultBorderPixels(inPixels: Integer);
begin
	FDefaultBorderPixels := inPixels;
	Edges.DefaultBorderPixels := inPixels;
end;

procedure TThCssBorders.SetEdges(const Value: TThCssBorderDetail);
begin
	FEdges.Assign(Value);
end;

{ TThCssPadDetail }

constructor TThCssPadDetail.Create(inOwnerStyle: TThCssBase);
begin
	FOwnerStyle := inOwnerStyle;
end;

procedure TThCssPadDetail.Changed;
begin
	inherited;
	if FOwnerStyle <> nil then
		FOwnerStyle.Changed;
end;

function TThCssPadDetail.HasPadding: Boolean;
begin
	Result := (PadLeft <> 0) or (PadRight <> 0) or (PadTop <> 0)
		or (PadBottom <> 0);
end;

procedure TThCssPadDetail.Assign(Source: TPersistent);
begin
	if not (Source is TThCssPadDetail) then
		inherited
	else with TThCssPadDetail(Source) do
	begin
		Self.FPadLeft := FPadLeft;
		Self.FPadTop := FPadTop;
		Self.FPadRight := FPadRight;
		Self.FPadBottom := FPadBottom;
	end;
end;

procedure TThCssPadDetail.Inherit(inPad: TThCssPadDetail);
begin
	if (inPad.FPadLeft <> 0) then
		FPadLeft := inPad.FPadLeft;
	if (inPad.FPadTop <> 0) then
		FPadTop := inPad.FPadTop;
	if (inPad.FPadRight <> 0) then
		FPadRight := inPad.FPadRight;
	if (inPad.FPadBottom <> 0) then
		FPadBottom := inPad.FPadBottom;
end;

function TThCssPadDetail.InlineAttribute: string;
begin
	Result := '';
	if (PadLeft <> 0) then
		ThCat(Result, ThStyleProp(ssPaddingLeft, ThPxValue(PadLeft)));
	if (PadTop <> 0) then
		ThCat(Result, ThStyleProp(ssPaddingTop, ThPxValue(PadTop)));
	if (PadRight <> 0) then
		ThCat(Result, ThStyleProp(ssPaddingRight, ThPxValue(PadRight)));
	if (PadBottom <> 0) then
		ThCat(Result, ThStyleProp(ssPaddingBottom, ThPxValue(PadBottom)));
end;

procedure TThCssPadDetail.ListStyles(inStyles: TThStyleList);
begin
	with inStyles do
	begin
		AddIfNotZero(ssPaddingLeft, PadLeft, ssPx);
		AddIfNotZero(ssPaddingTop, PadTop, ssPx);
		AddIfNotZero(ssPaddingRight, PadRight, ssPx);
		AddIfNotZero(ssPaddingBottom, PadBottom, ssPx);
	end;
end;

function TThCssPadDetail.PadHeight: Integer;
begin
	Result := PadTop + PadBottom;
end;

function TThCssPadDetail.PadWidth: Integer;
begin
	Result := PadLeft + PadRight;
end;

procedure TThCssPadDetail.SetPadBottom(const Value: Integer);
begin
	FPadBottom := Value;
	Changed;
end;

procedure TThCssPadDetail.SetPadLeft(const Value: Integer);
begin
	FPadLeft := Value;
	Changed;
end;

procedure TThCssPadDetail.SetPadRight(const Value: Integer);
begin
	FPadRight := Value;
	Changed;
end;

procedure TThCssPadDetail.SetPadTop(const Value: Integer);
begin
	FPadTop := Value;
	Changed;
end;

procedure TThCssPadDetail.SetPadding(const Value: Integer);
begin
	FPadLeft := Value;
	FPadRight := Value;
	FPadTop := Value;
	FPadBottom := Value;
	Changed;
end;

{ TThCssPadding }

constructor TThCssPadding.Create;
begin
	inherited;
	FPaddingDetails := TThCssPadDetail.Create(Self);
end;

destructor TThCssPadding.Destroy;
begin
	FPaddingDetails.Free;
	inherited;
end;

procedure TThCssPadding.Changed;
begin
	if PaddingDetails.HasPadding then
		FPadding := 0;
	inherited;
end;

function TThCssPadding.HasPadding: Boolean;
begin
	Result := (Padding <> 0) or PaddingDetails.HasPadding;
end;

function TThCssPadding.PadHeight: Integer;
begin
	if Padding <> 0 then
		Result := Padding + Padding
	else
		Result := PaddingDetails.PadHeight;
end;

function TThCssPadding.PadWidth: Integer;
begin
	if Padding <> 0 then
		Result := Padding + Padding
	else
		Result := PaddingDetails.PadWidth;
end;

function TThCssPadding.PadLeft: Integer;
begin
	if Padding <> 0 then
		Result := Padding
	else
		Result := PaddingDetails.PadLeft;
end;

function TThCssPadding.PadTop: Integer;
begin
	if Padding <> 0 then
		Result := Padding
	else
		Result := PaddingDetails.PadTop;
end;

function TThCssPadding.PadRight: Integer;
begin
	if Padding <> 0 then
		Result := Padding
	else
		Result := PaddingDetails.PadRight;
end;

function TThCssPadding.PadBottom: Integer;
begin
	if Padding <> 0 then
		Result := Padding
	else
		Result := PaddingDetails.PadBottom;
end;

procedure TThCssPadding.Assign(Source: TPersistent);
begin
	if not (Source is TThCssPadding) then
		inherited
	else with TThCssPadding(Source) do
	begin
		Self.Padding := Padding;
		Self.PaddingDetails.Assign(PaddingDetails);
	end;
end;

procedure TThCssPadding.Inherit(inPad: TThCssPadding);
begin
	if (inPad.Padding <> 0) then
		Padding := inPad.Padding;
	PaddingDetails.Inherit(inPad.PaddingDetails);
end;

function TThCssPadding.InlineAttribute: string;
begin
	if (Padding <> 0) then
		Result := ThStyleProp(ssPadding, ThPxValue(Padding))
	else
		Result := PaddingDetails.InlineAttribute;
end;

procedure TThCssPadding.ListStyles(inStyles: TThStyleList);
begin
	if (Padding <> 0) then
		inStyles.Add(ssPadding, Padding, 'px')
	else
		PaddingDetails.ListStyles(inStyles);
end;

procedure TThCssPadding.SetPadding(const Value: Integer);
begin
	FPadding := Value;
	Changed;
end;

procedure TThCssPadding.SetPaddingDetails(const Value: TThCssPadDetail);
begin
	FPaddingDetails.Assign(Value);
end;

{ TThCssStyle }

constructor TThCssStyle.Create(AOwner: TComponent);
begin
	inherited Create;
	FOwner := AOwner;
	FBackground := TThCssBackground.Create(AOwner);
	FBackground.OnChanged := InternalChanged;
	FFont := TThCssFont.Create;
	FFont.OnChanged := InternalChanged;
	FBorders := TThCssBorders.Create;
	FBorders.OnChanged := InternalChanged;
	FPadding := TThCssPadding.Create;
	FPadding.OnChanged := InternalChanged;
//	FOverflow := TThCssOverflow.Create;
//	FOverflow.OnChanged := InternalChanged;
	FColor := clNone;
end;

destructor TThCssStyle.Destroy;
begin
//	FOverflow.Free;
	FPadding.Free;
	FBorders.Free;
	FFont.Free;
	FBackground.Free;
	inherited;
end;

function TThCssStyle.Owner: TComponent;
begin
	Result := FOwner;
end;

procedure TThCssStyle.InternalChanged(inSender: TObject);
begin
	Changed;
end;

function TThCssStyle.StyleAttribute: string;
begin
	Result := InlineAttribute;
	if Result <> '' then
		Result := ' style="' + Result + '"';
end;

function TThCssStyle.InlineColorAttribute: string;
begin
	Result := ThStyleProp(ssBackgroundColor, Color);
end;

function TThCssStyle.InlineAttribute: string;
begin
	Result := '';
	ThCat(Result, FFont.InlineAttribute);
	ThCat(Result, InlineColorAttribute);
	ThCat(Result, FBackground.InlineAttribute);
	ThCat(Result, FBorders.InlineAttribute);
	ThCat(Result, FPadding.InlineAttribute);
	//ThCat(Result, FOverflow.InlineAttribute);
	ThCat(Result, ExtraStyles);
end;

procedure TThCssStyle.ListStyles(inStyles: TThStyleList);
begin
	FFont.ListStyles(inStyles);
	inStyles.AddColor(ssBackgroundColor, Color);
	FBackground.ListStyles(inStyles);
	FBorders.ListStyles(inStyles);
	FPadding.ListStyles(inStyles);
	inStyles.Add(ssCursor, Cursor);
	inStyles.ExtraStyles := inStyles.ExtraStyles + ExtraStyles;
end;

procedure TThCssStyle.ToFont(inFont: TFont);
begin
	Font.ToFont(inFont);
end;

procedure TThCssStyle.AssignFromParent(inParent: TThCssStyle);
begin
	if FParentFont then
		FFont.Assign(inParent.FFont);
	if FParentColor then
		FColor := inParent.FColor;
end;

procedure TThCssStyle.Assign(Source: TPersistent);
begin
	if Source <> nil then
		if not (Source is TThCssStyle) then
			inherited
		else with TThCssStyle(Source) do
		begin
			Self.FBackground.Assign(FBackground);
			Self.FBorders.Assign(FBorders);
			Self.FFont.Assign(FFont);
			//Self.FOverflow.Assign(FOverflow);
			Self.FPadding.Assign(FPadding);
			Self.FColor := FColor;
			Self.FCursor := FCursor;
			Self.ExtraStyles := ExtraStyles;
		end;
end;

procedure TThCssStyle.Inherit(inStyle: TThCssStyle);
begin
	if (inStyle <> nil) then
	begin
		FBackground.Inherit(inStyle.FBackground);
		FBorders.Inherit(inStyle.FBorders);
		FFont.Inherit(inStyle.FFont);
		//FOverflow.Inherit(inStyle.FOverflow);
		FPadding.Inherit(inStyle.FPadding);
		if not ThVisibleColor(FColor) then
			FColor := inStyle.FColor;
		if (inStyle.FCursor <> '') then
			FCursor := inStyle.FCursor;
		//ExtraStyles := ThConcatWithDelim(ExtraStyles, inStyle.ExtraStyles, ';');
	end;
end;

procedure TThCssStyle.SetBorders(const Value: TThCssBorders);
begin
	FBorders.Assign(Value);
	Changed;
end;

procedure TThCssStyle.SetBackground(const Value: TThCssBackground);
begin
	FBackground.Assign(Value);
	Changed;
end;

procedure TThCssStyle.SetFont(const Value: TThCssFont);
begin
	FFont.Assign(Value);
	Changed;
end;

//procedure TThCssStyle.SetOverflow(const Value: TThCssOverflow);
//begin
//	FOverflow.Assign(Value);
//	Changed;
//end;

procedure TThCssStyle.SetPadding(const Value: TThCssPadding);
begin
	FPadding.Assign(Value);
	Changed;
end;

procedure TThCssStyle.SetName(const Value: string);
begin
	FName := Value;
end;

procedure TThCssStyle.SetColor(const Value: TColor);
begin
	FColor := Value;
	if (FOwner = nil) or not (csLoading in FOwner.ComponentState) then
	begin
		FParentColor := false;
		Changed;
	end;
end;

procedure TThCssStyle.SetExtraStyles(const Value: string);
begin
	FExtraStyles := Value;
end;

procedure TThCssStyle.SetCursor(const Value: string);
begin
	FCursor := Value;
	Changed;
end;

procedure TThCssStyle.SetParentColor(const Value: Boolean);
begin
	FParentColor := Value;
	Changed;
end;

procedure TThCssStyle.SetParentFont(const Value: Boolean);
begin
	FParentFont := Value;
	Changed;
	//Font.ParentFont := FParentFont;
end;

function TThCssStyle.GetBoxBottom: Integer;
begin
	Result := Border.GetBorderPixels(bsBottom) + Padding.PadBottom;
end;

function TThCssStyle.GetBoxLeft: Integer;
begin
	Result := Border.GetBorderPixels(bsLeft) + Padding.PadLeft;
end;

function TThCssStyle.GetBoxRight: Integer;
begin
	Result := Border.GetBorderPixels(bsRight) + Padding.PadRight;
end;

function TThCssStyle.GetBoxTop: Integer;
begin
	Result := Border.GetBorderPixels(bsTop) + Padding.PadTop;
end;

procedure TThCssStyle.UnpadRect(var ioRect: TRect);
begin
	with ioRect do
	begin
		Left := Left + Padding.PadLeft;
		Right := Right - Padding.PadRight;
		Top := Top + Padding.PadTop;
		Bottom := Bottom - Padding.PadBottom;
	end;
end;

{
procedure TThCssStyle.ExpandRect(var ioRect: TRect);
begin
	with ioRect do
	begin
		Left := Left - GetBoxLeft;
		Right := Right + GetBoxRight;
		Top := Top - GetBoxTop;
		Bottom := Bottom + GetBoxBottom;
	end;
end;
}

procedure TThCssStyle.AdjustClientRect(var ioRect: TRect);
begin
	with ioRect do
	begin
		Inc(Left, GetBoxLeft);
		Inc(Top, GetBoxTop);
		Dec(Right, GetBoxRight);
		Dec(Bottom, GetBoxBottom);
//		Left := Left + GetBoxLeft;
//		Right := Right - GetBoxRight;
//		Top := Top + GetBoxTop;
//		Bottom := Bottom - GetBoxBottom;
	end;
end;

function TThCssStyle.GetBoxWidthMargin: Integer;
begin
	Result := GetBoxLeft + GetBoxRight;
end;

function TThCssStyle.GetBoxHeightMargin: Integer;
begin
	Result := GetBoxTop + GetBoxBottom;
end;

function TThCssStyle.AdjustWidth(inWidth: Integer): Integer;
begin
	Result := inWidth - GetBoxWidthMargin;
end;

function TThCssStyle.AdjustHeight(inHeight: Integer): Integer;
begin
	Result := inHeight - GetBoxHeightMargin;
end;

initialization
	NilStyle := TThCssStyle.Create(nil);
finalization
	NilStyle.Free;
end.
