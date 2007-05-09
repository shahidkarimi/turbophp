unit htStyle;

interface

uses
	Classes, Messages, SysUtils, Types, Graphics,
	htPersistBase, htStyleList, htPicture;
	{ThMessages, ThStyleList, ThPicture;}

type
	ThtStyleBase = class(ThtPersistBase);
	//
	ThtFontWeight = ( fwDefault, fwNormal, fwBold );
	ThtFontSizeRelEnum = ( fsDefault, fs_1_XxSmall, fs_2_XSmall, fs_3_Small,
		fs_4_Medium, fs_5_Large, fs_6_XLarge, fs_7_XxLarge );
	ThtFontSizeRel = string;
	ThtFontDecorations = ( fdDefault, fdInherit, fdNone, fdUnderline,
		fdOverline,	fdLineThrough, fdBlink );
	ThtFontDecoration = set of ThtFontDecorations;
	ThtFontStyle = ( fstDefault, fstInherit, fstNormal, fstItalic,
		fstOblique );
	//
	//:$ Describes CSS font properties.
	ThtFont = class(ThtStyleBase)
	private
		FFontSizeRel: ThtFontSizeRel;
		FFontFamily: TFontName;
		FFontWeight: ThtFontWeight;
		FFontSizePx: Integer;
		FFontSizePt: Integer;
		FFontColor: TColor;
		FFontDecoration: ThtFontDecoration;
		FFontStyle: ThtFontStyle;
		FDefaultFontPx: Integer;
		FDefaultFontFamily: string;
	protected
		function GetFontDecorationValue: string;
		function GetRelFontSize: Integer;
		function GetSizeValue: string;
		procedure SetDefaultFontFamily(const Value: string);
		procedure SetDefaultFontPx(const Value: Integer);
		procedure SetFontColor(const Value: TColor);
		procedure SetFontFamily(const Value: TFontName);
		procedure SetRelFontSize(const Value: ThtFontSizeRel);
		procedure SetFontWeight(const Value: ThtFontWeight);
		procedure SetFontSizePx(const Value: Integer);
		procedure SetFontSizePt(const Value: Integer);
		procedure SetFontDecoration(const Value: ThtFontDecoration);
		procedure SetFontStyle(const Value: ThtFontStyle);
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
		procedure ListStyles(inStyles: ThtStyleList);
		//:$ Converts a ThtFont to a TFont.
		procedure ToFont(inFont: TFont);
		//:$ Assign properties from an ancestor style.
		//:: Assigns properties from the input style into properties in this style
		//:: that are set to their defaults. Properties in the current style that
		//:: have been customized are not overriden.
		procedure Inherit(inFont: ThtFont);
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
		property FontDecoration: ThtFontDecoration read FFontDecoration
			write SetFontDecoration default [ fdDefault ];
		//:$ Font family (face).
		property FontFamily: TFontName read FFontFamily write SetFontFamily;
		//:$ Font size, in pixels.
		property FontSizePx: Integer read FFontSizePx write SetFontSizePx
			default 0;
		//:$ Relative font size.
		property FontSizeRel: ThtFontSizeRel read FFontSizeRel
			write SetRelFontSize;
		//:$ Font style.
		property FontStyle: ThtFontStyle read FFontStyle write SetFontStyle
			default fstDefault;
		//:$ Font weight.
		property FontWeight: ThtFontWeight read FFontWeight
			write SetFontWeight default fwDefault;
	end;
	//
	//:$ Describes CSS background properties.
	//:: Usually available as a member of a ThtStyle.
	ThtBackground = class(ThtStyleBase)
	private
		//FOwner: TComponent;
		FPicture: ThtPicture;
	protected
		procedure SetPicture(const Value: ThtPicture);
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
		procedure Inherit(inBackground: ThtBackground);
		//:$ <br>Copy style attributes to a list.
		//:: <br>Adds the attribute values for the style to a list of style
		//:: attributes.
		procedure ListStyles(inStyles: ThtStyleList);
	published
		//$ Optional picture to be used as background.
		//:: Background pictures are tiled to fill the style object.
		property Picture: ThtPicture read FPicture write SetPicture;
	end;
	//
	ThtBorderProp = ( bpWidth, bpStyle, bpColor);
	ThtBorderStyle = ( bsDefault, bsInherit, bsNone, bsHidden, bsDotted,
		bsDashed, bsSolidBorder, bsGroove, bsRidge, bsInset, bsOutset, bsDouble );
	ThtBorderSide = ( bsAll, bsLeft, bsTop, bsRight, bsBottom );
	//
	//:$ Describes a CSS border.
	//:: Usually available as a member of a ThtBorders object.
	ThtBorder = class(ThtStyleBase)
	private
		FBorderColor: TColor;
		FBorderPrefix: string;
		FBorderSide: ThtBorderSide;
		FBorderStyle: ThtBorderStyle;
		FBorderWidth: string;
		FDefaultBorderPixels: Integer;
		FOwnerStyle: ThtStyleBase;
	protected
		function CanCompoundStyle: Boolean;
		function GetBorderWidthValue: string;
		procedure SetBorderColor(const Value: TColor);
		procedure SetBorderStyle(const Value: ThtBorderStyle);
		procedure SetBorderWidth(const Value: string);
	public
		constructor Create(inOwner: ThtStyleBase; inSide: ThtBorderSide);
		function BorderVisible: Boolean; virtual;
		function GetBorderPixels: Integer; overload;
		//:$ Returns formatted style attributes.
		//:: InlineAttribute returns style attributes formatted
		//:: to be included in a style sheet or inline style declaration.
		//:: Returned string is a series of attribute:value pairs separated by
		//:: semicolons.
		//:: e.g. <attr>:<value>; <attr>:<value>;
		function InlineAttribute: string;
		procedure Change; override;
		//:$ <br>Copy style attributes to a list.
		//:: <br>Adds the attribute values for the style to a list of style
		//:: attributes.
		procedure ListStyles(inStyles: ThtStyleList);
		//:$ Assign properties from an ancestor style.
		//:: Assigns properties from the input style into properties in this style
		//:: that are set to their defaults. Properties in the current style that
		//:: have been customized are not overriden.
		procedure Inherit(inBorder: ThtBorder);
		procedure Assign(Source: TPersistent); override;
	public
		property BorderSide: ThtBorderSide read FBorderSide write FBorderSide;
		property DefaultBorderPixels: Integer read FDefaultBorderPixels
			write FDefaultBorderPixels;
	published
		property BorderWidth: string read FBorderWidth write SetBorderWidth;
		property BorderColor: TColor read FBorderColor write SetBorderColor
			default clDefault;
		property BorderStyle: ThtBorderStyle read FBorderStyle
			write SetBorderStyle default bsDefault;
	end;
	//
	//:$ Describes CSS border properties.
	//:: Aggregates ThtBorder objects. Usually available as a member of a
	//:: ThtStyle.
	ThtBorderDetail = class(ThtStyleBase)
	private
		FBorderLeft: ThtBorder;
		FBorderTop: ThtBorder;
		FBorderRight: ThtBorder;
		FBorderBottom: ThtBorder;
	protected
		procedure SetBorderBottom(const Value: ThtBorder);
		procedure SetBorderLeft(const Value: ThtBorder);
		procedure SetBorderRight(const Value: ThtBorder);
		procedure SetBorderTop(const Value: ThtBorder);
		procedure SetDefaultBorderPixels(inPixels: Integer);
	public
		constructor Create(inOwnerStyle: ThtStyleBase);
		destructor Destroy; override;
		procedure Assign(Source: TPersistent); override;
		function BorderVisible: Boolean;
		function GetBorder(inSide: ThtBorderSide): ThtBorder;
		//:$ Assign properties from an ancestor style.
		//:: Assigns properties from the input style into properties in this style
		//:: that are set to their defaults. Properties in the current style that
		//:: have been customized are not overriden.
		procedure Inherit(inDetail: ThtBorderDetail);
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
		procedure ListStyles(inStyles: ThtStyleList);
	public
		property DefaultBorderPixels: Integer write SetDefaultBorderPixels;
	published
		property BorderLeft: ThtBorder read FBorderLeft write SetBorderLeft;
		property BorderRight: ThtBorder read FBorderRight
			write SetBorderRight;
		property BorderTop: ThtBorder read FBorderTop write SetBorderTop;
		property BorderBottom: ThtBorder read FBorderBottom
			write SetBorderBottom;
	end;
	//
	//:$ Describes CSS border properties.
	//:: Aggregates ThtBorder objects. Usually available as a member of a
	//:: ThtStyle.
	ThtBorders = class(ThtBorder)
	private
		FEdges: ThtBorderDetail;
	protected
		procedure SetEdges(const Value: ThtBorderDetail);
		procedure SetDefaultBorderPixels(inPixels: Integer);
	public
		constructor Create;
		destructor Destroy; override;
		//procedure Change; override;
		function BorderVisible: Boolean; override;
		function GetBorderColor(inSide: ThtBorderSide): TColor;
		function GetBorderStyle(inSide: ThtBorderSide): ThtBorderStyle;
		function GetBorderPixels(inSide: ThtBorderSide): Integer; overload;
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
		procedure ListStyles(inStyles: ThtStyleList);
		//:$ Assign properties from an ancestor style.
		//:: Assigns properties from the input style into properties in this style
		//:: that are set to their defaults. Properties in the current style that
		//:: have been customized are not overriden.
		procedure Inherit(inBox: ThtBorders);
	public
		property DefaultBorderPixels: Integer write SetDefaultBorderPixels;
	published
		property Edges: ThtBorderDetail read FEdges
			write SetEdges;
	end;
	//
	ThtPadDetail = class(ThtStyleBase)
	private
		FOwnerStyle: ThtStyleBase;
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
		constructor Create(inOwnerStyle: ThtStyleBase);
		procedure Change; override;
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
		procedure ListStyles(inStyles: ThtStyleList);
		//:$ Add attribute values to an HTML node.
		//:: Adds the attribute values for the style class to the list of
		//:: style attributes maintained by the node.
		//procedure Stylize(inNode: ThtHtmlNode);
		//:$ Assign properties from an ancestor style.
		//:: Assigns properties from the input style into properties in this style
		//:: that are set to their defaults. Properties in the current style that
		//:: have been customized are not overriden.
		procedure Inherit(inPad: ThtPadDetail);
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
	//:: Usually available as a member of a ThtStyle.
	ThtPadding = class(ThtStyleBase)
	private
		FPadding: Integer;
		FPaddingDetails: ThtPadDetail;
	protected
		procedure SetPadding(const Value: Integer);
		procedure SetPaddingDetails(const Value: ThtPadDetail);
	public
		constructor Create; 
		destructor Destroy; override;
		//:$ Copy all properties from another style instance.
		procedure Assign(Source: TPersistent); override;
		procedure Change; override;
		//:$ Determine if any padding has been set.
		//:: Returns true if any of the padding properties are non-zero.
		function HasPadding: Boolean;
		//:$ Assign properties from an ancestor style.
		//:: Assigns properties from the input style into properties in this style
		//:: that are set to their defaults. Properties in the current style that
		//:: have been customized are not overriden.
		procedure Inherit(inPad: ThtPadding);
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
		procedure ListStyles(inStyles: ThtStyleList);
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
		property PaddingDetails: ThtPadDetail read FPaddingDetails
			write SetPaddingDetails;
	end;
	//
	//:$ ThtStyle combines various style objects. Commonly used as
	//:$ a property in objects needing style information.
	ThtStyle = class(ThtStyleBase)
	private
		FBackground: ThtBackground;
		FBorders: ThtBorders;
		FColor: TColor;
		FCursor: string;
		FExtraStyles: string;
		FFont: ThtFont;
		FName: string;
		//FOverflow: ThtOverflow;
		FOwner: TComponent;
		FPadding: ThtPadding;
		FParentColor: Boolean;
		FParentFont: Boolean;
	protected
		procedure SetBackground(const Value: ThtBackground);
		procedure SetBorders(const Value: ThtBorders);
		procedure SetColor(const Value: TColor);
		procedure SetCursor(const Value: string);
		procedure SetExtraStyles(const Value: string);
		procedure SetFont(const Value: ThtFont);
		procedure SetName(const Value: string);
		procedure SetPadding(const Value: ThtPadding);
		//procedure SetOverflow(const Value: ThtOverflow);
		procedure SetParentColor(const Value: Boolean);
		procedure SetParentFont(const Value: Boolean);
	protected
		procedure InternalChange(inSender: TObject); virtual;
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
		procedure ListStyles(inStyles: ThtStyleList);
		procedure Assign(Source: TPersistent); override;
		procedure AssignFromParent(inParent: ThtStyle);
		procedure Inherit(inStyle: ThtStyle);
		procedure ToFont(inFont: TFont);
		function GetBoxLeft: Integer;
		function GetBoxTop: Integer;
		function GetBoxRight: Integer;
		function GetBoxBottom: Integer;
//		procedure ExpandRect(var ioRect: TRect);
		function UnboxRect(const inRect: TRect): TRect;
		procedure UnpadRect(var ioRect: TRect);
		function GetBoxHeightMargin: Integer;
		function GetBoxWidthMargin: Integer;
		procedure AdjustClientRect(var ioRect: TRect);
		function AdjustHeight(inHeight: Integer): Integer;
		function AdjustWidth(inWidth: Integer): Integer;
		function Owner: TComponent;
		property Name: string read FName write SetName;
		property ParentFont: Boolean read FParentFont write SetParentFont;
		property ParentColor: Boolean read FParentColor write SetParentColor;
	published
		property Background: ThtBackground read FBackground write SetBackground;
		property Border: ThtBorders read FBorders write SetBorders;
		property Color: TColor read FColor write SetColor default clNone;
		property Cursor: string read FCursor write SetCursor;
		property ExtraStyles: string read FExtraStyles write SetExtraStyles;
		property Font: ThtFont read FFont write SetFont;
		//property Overflow: ThtOverflow read FOverflow write SetOverflow;
		property Padding: ThtPadding read FPadding write SetPadding;
	end;

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
	ssBorderPrefixi: array[ThtBorderSide] of string =
		(ssBorderPrefix, ssBorderLeftPrefix, ssBorderTopPrefix,
			ssBorderRightPrefix, ssBorderBottomPrefix);
	//
	ssBorderStyles: array[ThtBorderStyle] of string =
		('', 'inherit', 'none', 'hidden', 'dotted', 'dashed', 'solid', 'groove',
				'ridge', 'inset',	'outset', 'double');
	//
	//ssPositions: array[ThtPosition] of string = ( 'absolute', 'relative' );
	//
	ssFontWeights: array[ThtFontWeight] of string =
		('', 'normal',	'bold' );
	ssRelFontSizes: array[ThtFontSizeRelEnum] of string =
		('', 'xx-small', 'x-small', 'small', 'medium', 'large', 'x-large',
			'xx-large');
	cTFontSizes: array[ThtFontSizeRelEnum] of Integer =
		(12, 7, 8, 12, 14, 18, 24, 36 );
	ssTextDecorations: array[ThtFontDecorations] of string =
		('', 'inherit', 'none', 'underline', 'overline', 'line-through', 'blink');
	ssFontStyles: array[ThtFontStyle] of string =
		('', 'inherit', 'normal',	'italic', 'oblique');
	//
//	ssOverflowStyles: array[ThtOverflowStyle] of string = ( '', 'inherit',
//		'visible', 'hidden', 'scroll', 'auto' );
	//
	ssNumCursors = 19;
	ssCursors: array[0..18] of string =
		('', 'inherit', 'default', 'auto', 'n-resize', 'ne-resize', 'e-resize',
			'se-resize', 's-resize', 'sw-resize', 'w-resize', 'nw-resize',
				'crosshair', 'pointer', 'move', 'text', 'wait', 'help', 'hand');

const
	chtDefaultFontFamily: string = 'Times New Roman';
	chtDefaultFontPx: Integer = 16;

var
	NilStyle: ThtStyle;

implementation

uses
	LrVclUtils, htUtils;

{ ThtFont }

constructor ThtFont.Create;
begin
	FontColor := clNone;
	DefaultFontFamily := chtDefaultFontFamily;
	DefaultFontPx := chtDefaultFontPx;
end;

procedure ThtFont.SetFontFamily(const Value: TFontName);
begin
	if FFontFamily <> Value then
	begin
		FFontFamily := Value;
		Change;
	end;
end;

procedure ThtFont.SetRelFontSize(const Value: ThtFontSizeRel);
begin
	if FFontSizeRel <> Value then
	begin
		FFontSizeRel := Value;
		Change;
	end;
end;

procedure ThtFont.SetFontWeight(const Value: ThtFontWeight);
begin
	if FFontWeight <> Value then
	begin
		FFontWeight := Value;
		Change;
	end;
end;

procedure ThtFont.SetFontSizePx(const Value: Integer);
begin
	if FFontSizePx <> Value then
	begin
		FFontSizePx := Value;
		Change;
	end;
end;

procedure ThtFont.SetFontSizePt(const Value: Integer);
begin
	if FFontSizePt <> Value then
	begin
		FFontSizePt := Value;
		Change;
	end;
end;

procedure ThtFont.SetFontColor(const Value: TColor);
begin
	if FFontColor <> Value then
	begin
		FFontColor := Value;
		Change;
	end;
end;

procedure ThtFont.SetFontDecoration(
	const Value: ThtFontDecoration);
begin
	FFontDecoration := Value;
	Change;
end;

procedure ThtFont.SetFontStyle(const Value: ThtFontStyle);
begin
	FFontStyle := Value;
	Change;
end;

function ThtFont.GetSizeValue: string;
begin
	if (FontSizeRel <> '') then
		Result := FontSizeRel
	else if (FontSizePt <> 0) then
		Result := Format('%dpt', [ FontSizePt ])
	else if (FontSizePx <> 0) then
		Result := htPxValue(FontSizePx)
	else
		Result := '';
end;

function ThtFont.GetFontDecorationValue: string;
var
	i: ThtFontDecorations;
begin
	for i := Low(ThtFontDecorations) to High(ThtFontDecorations) do
		if i in FontDecoration then
			Result := LrCat([Result, ssTextDecorations[i]]);
end;

function ThtFont.InlineAttribute: string;
var
	fs: string;
begin
	fs := GetSizeValue;
	if (FontFamily <> '') and (fs <> '') then
	begin
		Result :=	LrCat([ ssFontStyles[FontStyle], ssFontWeights[FontWeight], fs,
			FontFamily ]);
		Result := htStyleProp(ssFont, Result);
	end
	else begin
		Result :=	LrCat([ htStyleProp(ssFontFamily, FontFamily),
			htStyleProp(ssFontSize, fs),
			htStyleProp(ssFontWeight, ssFontWeights[FontWeight]),
			htStyleProp(ssFontStyle, ssFontStyles[FontStyle]) ]);
	end;
	Result := LrCat([ Result, htStyleProp(ssColor, FontColor),
		htStyleProp(ssTextDecoration, GetFontDecorationValue) ]);
end;

procedure ThtFont.ListStyles(inStyles: ThtStyleList);
var
	fs, s: string;
begin
	fs := GetSizeValue;
	if (FontFamily <> '') and (fs <> '') then
	begin
		s := LrCat([ ssFontStyles[FontStyle], ssFontWeights[FontWeight], fs,
			FontFamily ]);
		inStyles.Add(ssFont, s);
	end
	else begin
		inStyles.Add(ssFontFamily, FontFamily);
		inStyles.Add(ssFontSize, fs);
		inStyles.Add(ssFontWeight, ssFontWeights[FontWeight]);
		inStyles.Add(ssFontStyle, ssFontStyles[FontStyle]);
	end;
	inStyles.AddColor(ssColor, FontColor);
	inStyles.Add(ssTextDecoration, GetFontDecorationValue);
end;

function ThtFont.GetRelFontSize: Integer;
var
	i: ThtFontSizeRelEnum;
begin
	Result := cTFontSizes[fsDefault];
	for i := Low(ThtFontSizeRelEnum) to High(ThtFontSizeRelEnum) do
		if (ssRelFontSizes[i] = FontSizeRel) then
		begin
			Result := cTFontSizes[i];
			break;
		end;
end;

procedure ThtFont.ToFont(inFont: TFont);
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
		if fdUnderline in FontDecoration then
			s := s + [ fsUnderline ];
		if fdLineThrough in FontDecoration then
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
		if htVisibleColor(FontColor) then
			Color := FontColor
		else
			Color := clBlack;
	end;
end;

procedure ThtFont.Inherit(inFont: ThtFont);
begin
	if inFont <> nil then
	begin
		if (FFontFamily = '') then
			FFontFamily := inFont.FFontFamily;
		if (FontWeight = fwDefault) then
			FFontWeight := inFont.FFontWeight;
		if not htVisibleColor(FFontColor) then
			FFontColor := inFont.FFontColor;
		if (FFontDecoration = [ fdDefault ]) then
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
end;

procedure ThtFont.Assign(Source: TPersistent);
begin
	if not (Source is ThtFont) then
		inherited
	else with ThtFont(Source) do
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

procedure ThtFont.SetDefaultFontFamily(const Value: string);
begin
	FDefaultFontFamily := Value;
end;

procedure ThtFont.SetDefaultFontPx(const Value: Integer);
begin
	FDefaultFontPx := Value;
end;

{ ThtBackground }

constructor ThtBackground.Create(AOwner: TComponent);
begin
	inherited Create;
	FPicture := ThtPicture.Create; //(AOwner);
	FPicture.OnChange := PictureChange;
end;

destructor ThtBackground.Destroy;
begin
	FPicture.Free;
	inherited;
end;

procedure ThtBackground.Assign(Source: TPersistent);
begin
	if not (Source is ThtBackground) then
		inherited
	else with ThtBackground(Source) do
		Self.Picture := Picture;
end;

procedure ThtBackground.Inherit(inBackground: ThtBackground);
begin
	if not Picture.HasGraphic then
		Picture := inBackground.Picture;
end;

function ThtBackground.InlineAttribute: string;
begin
	if Picture.Url <> '' then
		Result := htStyleProp(ssBackgroundImage, 'url(' + Picture.Url + ')')
	else
		Result := '';
end;

procedure ThtBackground.ListStyles(inStyles: ThtStyleList);
begin
	if Picture.Url <> '' then
		inStyles.Add(ssBackgroundImage, 'url(' + Picture.Url + ')');
end;

procedure ThtBackground.PictureChange(inSender: TObject);
begin
	Change;
end;

procedure ThtBackground.SetPicture(const Value: ThtPicture);
begin
	Picture.Assign(Value);
end;

{ ThtBorder }

constructor ThtBorder.Create(inOwner: ThtStyleBase;
	inSide: ThtBorderSide);
begin
	FOwnerStyle := inOwner;
	BorderSide := inSide;
	FBorderPrefix := ssBorderPrefixi[inSide];
	FBorderColor := clDefault;
end;

procedure ThtBorder.Change;
begin
	inherited;
	if FOwnerStyle <> nil then
		FOwnerStyle.Change;
end;

function ThtBorder.BorderVisible: Boolean;
begin
	case BorderStyle of
		bsDefault: Result := DefaultBorderPixels <> 0;
		bsNone, bsHidden: Result := false;
		else Result := true;
	end;
	inherited;
end;

function ThtBorder.GetBorderPixels: Integer;
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

procedure ThtBorder.SetBorderColor(const Value: TColor);
begin
	FBorderColor := Value;
	Change;
end;

procedure ThtBorder.SetBorderStyle(const Value: ThtBorderStyle);
begin
	FBorderStyle := Value;
	Change;
end;

procedure ThtBorder.SetBorderWidth(const Value: string);
begin
	FBorderWidth := Value;
	Change;
end;

function ThtBorder.GetBorderWidthValue: string;
begin
	if LrStringIsDigits(BorderWidth) then
		Result := BorderWidth + 'px'
	else
		Result := BorderWidth;
end;

function ThtBorder.CanCompoundStyle: Boolean;
begin
	Result := (ssBorderStyles[BorderStyle] <> '') and (BorderWidth <> '') and
			htVisibleColor(BorderColor);
end;

function ThtBorder.InlineAttribute: string;
begin
	if BorderStyle = bsDefault then
		Result := ''
	else if CanCompoundStyle then
		Result := htStyleProp(FBorderPrefix,
			LrCat([ GetBorderWidthValue, ssBorderStyles[BorderStyle],
				htColorToHtml(BorderColor) ]))
	else
		Result := LrCat([
			htStyleProp(FBorderPrefix + '-' + ssStyle, ssBorderStyles[BorderStyle]),
				htStyleProp(FBorderPrefix + '-' + ssWidth, GetBorderWidthValue),
					htStyleProp(FBorderPrefix + '-' + ssColor, BorderColor) ]);
end;

procedure ThtBorder.ListStyles(inStyles: ThtStyleList);
begin
	if BorderStyle <> bsDefault then
		if CanCompoundStyle then
			inStyles.Add(FBorderPrefix,
				LrCat([ GetBorderWidthValue, ssBorderStyles[BorderStyle],
					htColorToHtml(BorderColor) ]))
		else begin
			inStyles.Add(FBorderPrefix + '-' + ssStyle,	ssBorderStyles[BorderStyle]);
			inStyles.Add(FBorderPrefix + '-' + ssWidth, GetBorderWidthValue);
			inStyles.AddColor(FBorderPrefix + '-' + ssColor, BorderColor);
		end;
end;

procedure ThtBorder.Assign(Source: TPersistent);
begin
	if not (Source is ThtBorder) then
		inherited
	else with ThtBorder(Source) do
	begin
		Self.FBorderWidth := FBorderWidth;
		Self.FBorderStyle := FBorderStyle;
		Self.FBorderColor := FBorderColor;
	end;
end;

procedure ThtBorder.Inherit(inBorder: ThtBorder);
begin
	if inBorder.FBorderWidth <> '' then
		FBorderWidth := inBorder.FBorderWidth;
	if inBorder.FBorderStyle <> bsDefault then
		FBorderStyle := inBorder.FBorderStyle;
	if htVisibleColor(inBorder.FBorderColor) then
		FBorderColor := inBorder.FBorderColor;
end;

{ ThtBorderDetail }

constructor ThtBorderDetail.Create(inOwnerStyle: ThtStyleBase);
begin
	inherited Create;
	FBorderLeft := ThtBorder.Create(inOwnerStyle, bsLeft);
	FBorderTop := ThtBorder.Create(inOwnerStyle, bsTop);
	FBorderRight := ThtBorder.Create(inOwnerStyle, bsRight);
	FBorderBottom := ThtBorder.Create(inOwnerStyle, bsBottom);
end;

destructor ThtBorderDetail.Destroy;
begin
	FBorderLeft.Free;
	FBorderTop.Free;
	FBorderRight.Free;
	FBorderBottom.Free;
	inherited;
end;

function ThtBorderDetail.BorderVisible: Boolean;
begin
	Result := BorderLeft.BorderVisible or
						BorderTop.BorderVisible or
						BorderRight.BorderVisible or
						BorderBottom.BorderVisible;
end;

procedure ThtBorderDetail.Assign(Source: TPersistent);
begin
	if not (Source is ThtBorderDetail) then
		inherited
	else with ThtBorderDetail(Source) do
	begin
		Self.FBorderLeft.Assign(FBorderLeft);
		Self.FBorderTop.Assign(FBorderTop);
		Self.FBorderRight.Assign(FBorderRight);
		Self.FBorderBottom.Assign(FBorderBottom);
	end;
end;

procedure ThtBorderDetail.Inherit(inDetail: ThtBorderDetail);
begin
	FBorderLeft.Inherit(inDetail.FBorderLeft);
	FBorderTop.Inherit(inDetail.FBorderTop);
	FBorderRight.Inherit(inDetail.FBorderRight);
	FBorderBottom.Inherit(inDetail.FBorderBottom);
end;

function ThtBorderDetail.InlineAttribute: string;
begin
	Result := BorderLeft.InlineAttribute
		+ BorderTop.InlineAttribute
		+ BorderRight.InlineAttribute
		+ BorderBottom.InlineAttribute;
end;

procedure ThtBorderDetail.ListStyles(inStyles: ThtStyleList);
begin
	BorderLeft.ListStyles(inStyles);
	BorderTop.ListStyles(inStyles);
	BorderRight.ListStyles(inStyles);
	BorderBottom.ListStyles(inStyles);
end;

function ThtBorderDetail.GetBorder(inSide: ThtBorderSide): ThtBorder;
begin
	case inSide of
		bsLeft: Result := BorderLeft;
		bsTop: Result := BorderTop;
		bsRight: Result := BorderRight;
		bsBottom: Result := BorderBottom;
		else Result := BorderLeft;
	end;
end;

procedure ThtBorderDetail.SetBorderBottom(const Value: ThtBorder);
begin
	FBorderBottom.Assign(Value);
end;

procedure ThtBorderDetail.SetBorderLeft(const Value: ThtBorder);
begin
	FBorderLeft.Assign(Value);
end;

procedure ThtBorderDetail.SetBorderRight(const Value: ThtBorder);
begin
	FBorderRight.Assign(Value);
end;

procedure ThtBorderDetail.SetBorderTop(const Value: ThtBorder);
begin
	FBorderTop.Assign(Value);
end;

procedure ThtBorderDetail.SetDefaultBorderPixels(inPixels: Integer);
begin
	BorderLeft.DefaultBorderPixels := inPixels;
	BorderTop.DefaultBorderPixels := inPixels;
	BorderRight.DefaultBorderPixels := inPixels;
	BorderBottom.DefaultBorderPixels := inPixels;
end;

{ ThtBorders }

constructor ThtBorders.Create;
begin
	inherited Create(nil, bsAll);
	FEdges := ThtBorderDetail.Create(Self);
	Change;
end;

destructor ThtBorders.Destroy;
begin
	FEdges.Free;
	inherited;
end;

procedure ThtBorders.Assign(Source: TPersistent);
begin
	inherited;
	if (Source is ThtBorders) then
		with ThtBorders(Source) do
			Self.Edges.Assign(Edges);
end;

procedure ThtBorders.Inherit(inBox: ThtBorders);
begin
	inherited Inherit(inBox);
	Edges.Inherit(inBox.Edges);
end;

function ThtBorders.InlineAttribute: string;
begin
	if BorderStyle <> bsDefault then
		Result := inherited InlineAttribute
	else
		Result := Edges.InlineAttribute;
end;

procedure ThtBorders.ListStyles(inStyles: ThtStyleList);
begin
	if BorderStyle <> bsDefault then
		inherited ListStyles(inStyles)
	else
		Edges.ListStyles(inStyles);
end;

function ThtBorders.GetBorderColor(inSide: ThtBorderSide): TColor;
begin
	if BorderStyle <> bsDefault then
		Result := BorderColor
	else
		Result := Edges.GetBorder(inSide).BorderColor;
end;

function ThtBorders.GetBorderStyle(
	inSide: ThtBorderSide): ThtBorderStyle;
begin
	if BorderStyle <> bsDefault then
		Result := BorderStyle
	else
		Result := Edges.GetBorder(inSide).BorderStyle;
end;

function ThtBorders.GetBorderPixels(inSide: ThtBorderSide): Integer;
begin
	if BorderStyle <> bsDefault then
		Result := GetBorderPixels
	else
		Result := Edges.GetBorder(inSide).GetBorderPixels;
end;

function ThtBorders.BorderVisible: Boolean;
begin
	Result := inherited BorderVisible or Edges.BorderVisible;
end;

procedure ThtBorders.SetDefaultBorderPixels(inPixels: Integer);
begin
	FDefaultBorderPixels := inPixels;
	Edges.DefaultBorderPixels := inPixels;
end;

procedure ThtBorders.SetEdges(const Value: ThtBorderDetail);
begin
	FEdges.Assign(Value);
end;

{ ThtPadDetail }

constructor ThtPadDetail.Create(inOwnerStyle: ThtStyleBase);
begin
	FOwnerStyle := inOwnerStyle;
end;

procedure ThtPadDetail.Change;
begin
	inherited;
	if FOwnerStyle <> nil then
		FOwnerStyle.Change;
end;

function ThtPadDetail.HasPadding: Boolean;
begin
	Result := (PadLeft <> 0) or (PadRight <> 0) or (PadTop <> 0)
		or (PadBottom <> 0);
end;

procedure ThtPadDetail.Assign(Source: TPersistent);
begin
	if not (Source is ThtPadDetail) then
		inherited
	else with ThtPadDetail(Source) do
	begin
		Self.FPadLeft := FPadLeft;
		Self.FPadTop := FPadTop;
		Self.FPadRight := FPadRight;
		Self.FPadBottom := FPadBottom;
	end;
end;

procedure ThtPadDetail.Inherit(inPad: ThtPadDetail);
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

function ThtPadDetail.InlineAttribute: string;
begin
	Result := '';
	if (PadLeft <> 0) then
		LrCat(Result, htStyleProp(ssPaddingLeft, htPxValue(PadLeft)));
	if (PadTop <> 0) then
		LrCat(Result, htStyleProp(ssPaddingTop, htPxValue(PadTop)));
	if (PadRight <> 0) then
		LrCat(Result, htStyleProp(ssPaddingRight, htPxValue(PadRight)));
	if (PadBottom <> 0) then
		LrCat(Result, htStyleProp(ssPaddingBottom, htPxValue(PadBottom)));
end;

procedure ThtPadDetail.ListStyles(inStyles: ThtStyleList);
begin
	with inStyles do
	begin
		AddIfNonZero(ssPaddingLeft, PadLeft, ssPx);
		AddIfNonZero(ssPaddingTop, PadTop, ssPx);
		AddIfNonZero(ssPaddingRight, PadRight, ssPx);
		AddIfNonZero(ssPaddingBottom, PadBottom, ssPx);
	end;
end;

function ThtPadDetail.PadHeight: Integer;
begin
	Result := PadTop + PadBottom;
end;

function ThtPadDetail.PadWidth: Integer;
begin
	Result := PadLeft + PadRight;
end;

procedure ThtPadDetail.SetPadBottom(const Value: Integer);
begin
	FPadBottom := Value;
	Change;
end;

procedure ThtPadDetail.SetPadLeft(const Value: Integer);
begin
	FPadLeft := Value;
	Change;
end;

procedure ThtPadDetail.SetPadRight(const Value: Integer);
begin
	FPadRight := Value;
	Change;
end;

procedure ThtPadDetail.SetPadTop(const Value: Integer);
begin
	FPadTop := Value;
	Change;
end;

procedure ThtPadDetail.SetPadding(const Value: Integer);
begin
	FPadLeft := Value;
	FPadRight := Value;
	FPadTop := Value;
	FPadBottom := Value;
	Change;
end;

{ ThtPadding }

constructor ThtPadding.Create;
begin
	inherited;
	FPaddingDetails := ThtPadDetail.Create(Self);
end;

destructor ThtPadding.Destroy;
begin
	FPaddingDetails.Free;
	inherited;
end;

procedure ThtPadding.Change;
begin
	if PaddingDetails.HasPadding then
		FPadding := 0;
	inherited;
end;

function ThtPadding.HasPadding: Boolean;
begin
	Result := (Padding <> 0) or PaddingDetails.HasPadding;
end;

function ThtPadding.PadHeight: Integer;
begin
	if Padding <> 0 then
		Result := Padding + Padding
	else
		Result := PaddingDetails.PadHeight;
end;

function ThtPadding.PadWidth: Integer;
begin
	if Padding <> 0 then
		Result := Padding + Padding
	else
		Result := PaddingDetails.PadWidth;
end;

function ThtPadding.PadLeft: Integer;
begin
	if Padding <> 0 then
		Result := Padding
	else
		Result := PaddingDetails.PadLeft;
end;

function ThtPadding.PadTop: Integer;
begin
	if Padding <> 0 then
		Result := Padding
	else
		Result := PaddingDetails.PadTop;
end;

function ThtPadding.PadRight: Integer;
begin
	if Padding <> 0 then
		Result := Padding
	else
		Result := PaddingDetails.PadRight;
end;

function ThtPadding.PadBottom: Integer;
begin
	if Padding <> 0 then
		Result := Padding
	else
		Result := PaddingDetails.PadBottom;
end;

procedure ThtPadding.Assign(Source: TPersistent);
begin
	if not (Source is ThtPadding) then
		inherited
	else with ThtPadding(Source) do
	begin
		Self.Padding := Padding;
		Self.PaddingDetails.Assign(PaddingDetails);
	end;
end;

procedure ThtPadding.Inherit(inPad: ThtPadding);
begin
	if (inPad.Padding <> 0) then
		Padding := inPad.Padding;
	PaddingDetails.Inherit(inPad.PaddingDetails);
end;

function ThtPadding.InlineAttribute: string;
begin
	if (Padding <> 0) then
		Result := htStyleProp(ssPadding, htPxValue(Padding))
	else
		Result := PaddingDetails.InlineAttribute;
end;

procedure ThtPadding.ListStyles(inStyles: ThtStyleList);
begin
	if (Padding <> 0) then
		inStyles.Add(ssPadding, Padding, 'px')
	else
		PaddingDetails.ListStyles(inStyles);
end;

procedure ThtPadding.SetPadding(const Value: Integer);
begin
	FPadding := Value;
	Change;
end;

procedure ThtPadding.SetPaddingDetails(const Value: ThtPadDetail);
begin
	FPaddingDetails.Assign(Value);
end;

{ ThtStyle }

constructor ThtStyle.Create(AOwner: TComponent);
begin
	inherited Create;
	FOwner := AOwner;
	FBackground := ThtBackground.Create(AOwner);
	FBackground.OnChange := InternalChange;
	FFont := ThtFont.Create;
	FFont.OnChange := InternalChange;
	FBorders := ThtBorders.Create;
	FBorders.OnChange := InternalChange;
	FPadding := ThtPadding.Create;
	FPadding.OnChange := InternalChange;
//	FOverflow := ThtOverflow.Create;
//	FOverflow.OnChange := InternalChange;
	FColor := clNone;
end;

destructor ThtStyle.Destroy;
begin
//	FOverflow.Free;
	FPadding.Free;
	FBorders.Free;
	FFont.Free;
	FBackground.Free;
	inherited;
end;

function ThtStyle.Owner: TComponent;
begin
	Result := FOwner;
end;

procedure ThtStyle.InternalChange(inSender: TObject);
begin
	Change;
end;

function ThtStyle.StyleAttribute: string;
begin
	Result := InlineAttribute;
	if Result <> '' then
		Result := ' style="' + Result + '"';
end;

function ThtStyle.InlineColorAttribute: string;
begin
	Result := htStyleProp(ssBackgroundColor, Color);
end;

function ThtStyle.InlineAttribute: string;
begin
	Result := LrCat([
		FFont.InlineAttribute,
		InlineColorAttribute,
		FBackground.InlineAttribute,
		FBorders.InlineAttribute,
		FPadding.InlineAttribute,
		{FOverflow.InlineAttribute,}
		ExtraStyles
	]);
end;

procedure ThtStyle.ListStyles(inStyles: ThtStyleList);
begin
	FFont.ListStyles(inStyles);
	inStyles.AddColor(ssBackgroundColor, Color);
	FBackground.ListStyles(inStyles);
	FBorders.ListStyles(inStyles);
	FPadding.ListStyles(inStyles);
	inStyles.Add(ssCursor, Cursor);
	inStyles.ExtraStyles := inStyles.ExtraStyles + ExtraStyles;
end;

procedure ThtStyle.ToFont(inFont: TFont);
begin
	Font.ToFont(inFont);
end;

procedure ThtStyle.AssignFromParent(inParent: ThtStyle);
begin
	if FParentFont then
		FFont.Assign(inParent.FFont);
	if FParentColor then
		FColor := inParent.FColor;
end;

procedure ThtStyle.Assign(Source: TPersistent);
begin
	if Source <> nil then
		if not (Source is ThtStyle) then
			inherited
		else with ThtStyle(Source) do
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

procedure ThtStyle.Inherit(inStyle: ThtStyle);
begin
	if (inStyle <> nil) then
	begin
		FBackground.Inherit(inStyle.FBackground);
		FBorders.Inherit(inStyle.FBorders);
		FFont.Inherit(inStyle.FFont);
		//FOverflow.Inherit(inStyle.FOverflow);
		FPadding.Inherit(inStyle.FPadding);
		if not htVisibleColor(FColor) then
			FColor := inStyle.FColor;
		if (inStyle.FCursor <> '') then
			FCursor := inStyle.FCursor;
		//ExtraStyles := ThConcatWithDelim(ExtraStyles, inStyle.ExtraStyles, ';');
	end;
end;

procedure ThtStyle.SetBorders(const Value: ThtBorders);
begin
	FBorders.Assign(Value);
	Change;
end;

procedure ThtStyle.SetBackground(const Value: ThtBackground);
begin
	FBackground.Assign(Value);
	Change;
end;

procedure ThtStyle.SetFont(const Value: ThtFont);
begin
	FFont.Assign(Value);
	Change;
end;

//procedure ThtStyle.SetOverflow(const Value: ThtOverflow);
//begin
//	FOverflow.Assign(Value);
//	Change;
//end;

procedure ThtStyle.SetPadding(const Value: ThtPadding);
begin
	FPadding.Assign(Value);
	Change;
end;

procedure ThtStyle.SetName(const Value: string);
begin
	FName := Value;
end;

procedure ThtStyle.SetColor(const Value: TColor);
begin
	FColor := Value;
	if (FOwner = nil) or not (csLoading in FOwner.ComponentState) then
	begin
		FParentColor := false;
		Change;
	end;
end;

procedure ThtStyle.SetExtraStyles(const Value: string);
begin
	FExtraStyles := Value;
end;

procedure ThtStyle.SetCursor(const Value: string);
begin
	FCursor := Value;
	Change;
end;

procedure ThtStyle.SetParentColor(const Value: Boolean);
begin
	FParentColor := Value;
	Change;
end;

procedure ThtStyle.SetParentFont(const Value: Boolean);
begin
	FParentFont := Value;
	Change;
	//Font.ParentFont := FParentFont;
end;

function ThtStyle.GetBoxBottom: Integer;
begin
	Result := Border.GetBorderPixels(bsBottom) + Padding.PadBottom;
end;

function ThtStyle.GetBoxLeft: Integer;
begin
	Result := Border.GetBorderPixels(bsLeft) + Padding.PadLeft;
end;

function ThtStyle.GetBoxRight: Integer;
begin
	Result := Border.GetBorderPixels(bsRight) + Padding.PadRight;
end;

function ThtStyle.GetBoxTop: Integer;
begin
	Result := Border.GetBorderPixels(bsTop) + Padding.PadTop;
end;

procedure ThtStyle.UnpadRect(var ioRect: TRect);
begin
	with ioRect do
	begin
		Left := Left + Padding.PadLeft;
		Right := Right - Padding.PadRight;
		Top := Top + Padding.PadTop;
		Bottom := Bottom - Padding.PadBottom;
	end;
end;

function ThtStyle.UnboxRect(const inRect: TRect): TRect;
begin
	with Result do
	begin
		Left := inRect.Left + GetBoxLeft;
		Right := inRect.Right - GetBoxRight;
		Top := inRect.Top + GetBoxTop;
		Bottom := inRect.Bottom - GetBoxBottom;
	end;
end;
{
procedure ThtStyle.ExpandRect(var ioRect: TRect);
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

procedure ThtStyle.AdjustClientRect(var ioRect: TRect);
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

function ThtStyle.GetBoxWidthMargin: Integer;
begin
	Result := GetBoxLeft + GetBoxRight;
end;

function ThtStyle.GetBoxHeightMargin: Integer;
begin
	Result := GetBoxTop + GetBoxBottom;
end;

function ThtStyle.AdjustWidth(inWidth: Integer): Integer;
begin
	Result := inWidth - GetBoxWidthMargin;
end;

function ThtStyle.AdjustHeight(inHeight: Integer): Integer;
begin
	Result := inHeight - GetBoxHeightMargin;
end;

initialization
	NilStyle := ThtStyle.Create(nil);
finalization
	NilStyle.Free;
end.
