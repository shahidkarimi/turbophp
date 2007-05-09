unit LrPageControl;

interface

uses
	Windows, Messages, SysUtils, Types, Classes, Controls, StdCtrls, ExtCtrls,
	Graphics, Forms;

type
	TLrCustomTabControl = class;
	//
	TLrTabPosition = ( ltpTop, ltpBottom );
	TLrGetCaptionEvent = procedure(inSender: TLrCustomTabControl;
		inIndex: Integer; var ioCaption: string) of object;
	TLrBeforeSelectEvent = procedure(inSender: TLrCustomTabControl;
		var ioSelected: Integer) of object;
	TLrTabState = ( tsEnabled, tsDisabled );
	//
	TLrCustomTabControl = class(TGraphicControl)
	private
		FCount: Integer;
		FGlyph: TBitmap;
		FGlyphVisible: Boolean;
		FHot: Integer;
		FHotGlyph: TBitmap;
		FOnBeforeSelect: TLrBeforeSelectEvent;
		FOnGetCaption: TLrGetCaptionEvent;
		FOnSelect: TNotifyEvent;
		FSelected: Integer;
		FTabPosition: TLrTabPosition;
		FTabStates: array of TLrTabState;
	protected
		function GetCount: Integer; virtual;
		function GetGlyphSpace: Integer;
		function GetTabCaption(inIndex: Integer): string; virtual;
		function GetTabHeight: Integer;
		function GetTabStates(inIndex: Integer): TLrTabState;
		function GetTabWidth(inIndex: Integer): Integer;
		procedure SetCount(const Value: Integer);
		procedure SetGlyphVisible(const Value: Boolean);
		procedure SetHot(const Value: Integer);
		procedure SetOnBeforeSelect(const Value: TLrBeforeSelectEvent);
		procedure SetOnSelect(const Value: TNotifyEvent);
		procedure SetSelected(const Value: Integer);
		procedure SetTabHeight(const Value: Integer);
		procedure SetTabPosition(const Value: TLrTabPosition);
		procedure SetTabStates(inIndex: Integer; const Value: TLrTabState);
	protected
		function HitTest(inX, inY: Integer): Integer;
		procedure CMDesignHitTest(var Message: TCMDesignHitTest);
			message CM_DESIGNHITTEST;
		//	CM_MOUSEENTER             = CM_BASE + 19;
		procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
		procedure LoadGlyphs;
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
			Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure Paint; override;
		procedure PaintBottomTab(inRect: TRect; inIndex: Integer);
		procedure PaintTabClient(inRect: TRect; inIndex: Integer);
		procedure PaintTopTab(inRect: TRect; inIndex: Integer);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		function IsGoodIndex(inIndex: Integer): Boolean;
	public
		property Color;
		property Count: Integer read GetCount write SetCount;
		property Font;
		property GlyphVisible: Boolean read FGlyphVisible write SetGlyphVisible
			default true;
		property Hot: Integer read FHot write SetHot;
		property Selected: Integer read FSelected write SetSelected;
		property OnBeforeSelect: TLrBeforeSelectEvent read FOnBeforeSelect
			write SetOnBeforeSelect;
		property OnGetCaption: TLrGetCaptionEvent read FOnGetCaption
			write FOnGetCaption;
		property OnSelect: TNotifyEvent read FOnSelect write SetOnSelect;
		property ParentColor;
		property ParentFont;
		property TabHeight: Integer read GetTabHeight write SetTabHeight;
		property TabPosition: TLrTabPosition read FTabPosition write SetTabPosition
			default ltpTop;
		property TabStates[inIndex: Integer]: TLrTabState read GetTabStates
			write SetTabStates;
	end;
	//
	TLrTabControl = class(TLrCustomTabControl)
	private
		FTabs: TStringList;
	protected
		function GetCount: Integer; override;
		function GetTabCaption(inIndex: Integer): string; override;
		procedure SetTabs(const Value: TStringList);
	protected
		procedure TabsChange(inSender: TObject);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	published
		property Align;
		property Color;
		property Font;
		property GlyphVisible;
		property OnBeforeSelect;
		property OnGetCaption;
		property OnSelect;
		property ParentColor;
		property ParentFont;
		property Selected;
		property TabHeight;
		property TabPosition;
		property Tabs: TStringList read FTabs write SetTabs;
		property Visible;
	end;
	//
	TLrTabSheet = class(TCustomControl)
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure Deselect;
		procedure Select;
	published
		property Align;
		property Caption;
		property Color;
		property Visible;
	end;
	//
	TLrCustomPageControl = class(TCustomPanel)
	private
		FActivePageIndex: Integer;
		FSheets: TList;
	protected
		function GetCount: Integer;
		function GetSheet(inIndex: Integer): TLrTabSheet;
		function GetSheetNamePrefix: string; virtual;
		procedure SetActivePageIndex(const Value: Integer);
		procedure SetCount(const Value: Integer);
	protected
		function CreateSheet: TLrTabSheet; virtual;
		procedure AdjustClientRect(var Rect: TRect); override;
		procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
		procedure Loaded; override;
		procedure Notification(AComponent: TComponent; Operation: TOperation); override;
		procedure Paint; override;
		procedure RenameSheets; virtual;
		procedure Resize; override;
		procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		function AddSheet: TLrTabSheet;
		function FindSheet(inSheet: TControl): Integer;
		function IsGoodIndex(inIndex: Integer): Boolean;
		procedure RemoveSheet(inSheet: TLrTabSheet);
		property Sheet[inIndex: Integer]: TLrTabSheet read GetSheet;
	published
		property ActivePageIndex: Integer read FActivePageIndex
			write SetActivePageIndex;
		property Align;
		property Visible;
		property Count: Integer read GetCount write SetCount;
		property Color;
	end;
	//
	TLrPageControl = class(TLrCustomPageControl)
	private
		FTabPosition: TLrTabPosition;
	protected
		procedure SetTabPosition(const Value: TLrTabPosition);
	protected
		function GetTabWidth(inIndex: Integer): Integer;
		function HitTest(inX, inY: Integer): Boolean;
		procedure AdjustClientRect(var Rect: TRect); override;
		procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
		procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
			Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
			Y: Integer); override;
		procedure Paint; override;
		procedure PaintTopTab(inRect: TRect; inState: Integer;
			const inCaption: string);
		procedure PaintBottomTab(inRect: TRect; inState: Integer;
			const inCaption: string);
		procedure ShowControl(AControl: TControl); override;
	public
		TabWidth, TabHeight: Integer;
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	published
		property TabPosition: TLrTabPosition read FTabPosition write SetTabPosition
			default ltpTop;
	end;

implementation

uses
	Math;

{$R LrPCGlyphs.res}

{ TLrCustomTabControl }

constructor TLrCustomTabControl.Create(AOwner: TComponent);
begin
	inherited;
	FGlyph := TBitmap.Create;
	FHotGlyph := TBitmap.Create;
	FSelected := -1;
	FHot := -1;
	FGlyphVisible := true;
	LoadGlyphs;
end;

destructor TLrCustomTabControl.Destroy;
begin
	FGlyph.Free;
	inherited;
end;

procedure TLrCustomTabControl.LoadGlyphs;
begin
	FGlyph.LoadFromResourceName(HInstance, 'LRPC_CLOSE');
	FHotGlyph.LoadFromResourceName(HInstance, 'LRPC_CLOSE_HOT');
end;

function TLrCustomTabControl.IsGoodIndex(inIndex: Integer): Boolean;
begin
	Result := (inIndex >= 0) and (inIndex < Count);
end;

procedure TLrCustomTabControl.CMDesignHitTest(var Message: TCMDesignHitTest);
var
	i: Integer;
begin
	Message.Result := 0;
	if (Message.Keys and MK_LBUTTON) = MK_LBUTTON then
	begin
		i := HitTest(Message.Pos.x, Message.Pos.y);
		if (i >= 0) then
		begin
			Selected := i;
			Message.Result := 1;
		end;
	end;
end;

function TLrCustomTabControl.GetTabCaption(inIndex: Integer): string;
begin
	if Assigned(OnGetCaption) then
		OnGetCaption(Self, inIndex, Result);
end;

function TLrCustomTabControl.GetGlyphSpace: Integer;
begin
	if not FGlyph.Empty then
		Result := FGlyph.Width + 6
	else
		Result := 0;
end;

function TLrCustomTabControl.GetTabWidth(inIndex: Integer): Integer;
begin
	Result := Canvas.TextWidth(GetTabCaption(inIndex)) + 12;
	if inIndex = Selected then
		Inc(Result, GetGlyphSpace);
end;

function TLrCustomTabControl.HitTest(inX, inY: Integer): Integer;
var
	i, x: Integer;
begin
	Result := -1;
	case TabPosition of
		ltpTop:
			if (inY > TabHeight) then
				exit;
		else
			if (inY < ClientHeight - TabHeight) then
				exit;
	end;
	x := 0;
	for i := 0 to Pred(Count) do
	begin
		Inc(x, GetTabWidth(i));
		if (inX < x) then
		begin
			Result := i;
			exit;
		end;
	end;
end;

procedure TLrCustomTabControl.MouseDown(Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
var
	i: Integer;
begin
	inherited;
	i := HitTest(X, Y);
	if (TabStates[i] <> tsDisabled) then
		Selected := i;
end;

procedure TLrCustomTabControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	i: Integer;
begin
	inherited;
	if not Enabled then
		Hot := -1
	else begin
		i := HitTest(X, Y);
		if TabStates[i] <> tsDisabled then
			Hot := i;
	end;
end;

procedure TLrCustomTabControl.Paint;
var
	i: Integer;
	r: TRect;
begin
	inherited;
	//
	r := Rect(0, 0, ClientWidth, TabHeight);
	//
	case TabPosition of
		ltpBottom: OffsetRect(r, 0, ClientHeight - TabHeight);
	end;
	//
	Canvas.Brush.Color := Color;
	Canvas.FillRect(r);
	//
	SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
	for i := 0 to Pred(Count) do
	begin
		if Enabled and (TabStates[i] = tsEnabled) then
			Canvas.Font.Color := clBlack
		else
			Canvas.Font.Color := clSilver;
		r.Right := r.Left + GetTabWidth(i);
		case TabPosition of
			ltpTop: PaintTopTab(r, i);
			ltpBottom: PaintBottomTab(r, i);
		end;
		PaintTabClient(r, i);
		r.Left := r.Right;
	end;
	//
	Canvas.Pen.Color := $BAB19D;
	//
	case TabPosition of
		ltpTop:
		begin
			Canvas.MoveTo(r.Left, TabHeight - 1);
			Canvas.LineTo(ClientWidth, TabHeight - 1);
		end;
		ltpBottom:
		begin
			Canvas.MoveTo(r.Left, r.Bottom - TabHeight + 1);
			Canvas.LineTo(ClientWidth, r.Bottom - TabHeight + 1);
		end;
	end;
end;

procedure TLrCustomTabControl.PaintTabClient(inRect: TRect; inIndex: Integer);
var
	c: string;
	x, h: Integer;

	function Offset(inHeight: Integer): Integer;
	var
		v: Integer;
	begin
		case TabPosition of
			ltpTop: v := 3;
			else v := 0;
		end;
		Result := inRect.Top + v + Max(1, ((TabHeight - 3) div 2) - inHeight);
	end;

begin
	c := GetTabCaption(inIndex);
	h := Offset(Canvas.TextHeight(c) div 2);
	if inIndex = Selected then
		x := GetGlyphSpace + 4
	else
		x := 6;
	Canvas.TextOut(inRect.Left + x, h, c);
	if inIndex = Selected then
	begin
		h := Offset(FGlyph.Height div 2);
		if inIndex = Hot then
			Canvas.Draw(inRect.Left + 5, h, FHotGlyph)
		else
			Canvas.Draw(inRect.Left + 5, h, FGlyph);
	end;
end;

procedure TLrCustomTabControl.PaintBottomTab(inRect: TRect; inIndex: Integer);

	procedure DrawNormalTab;
	begin
		with Canvas, inRect do
		begin
			Brush.Color := $F0EBEB;
			Pen.Color := $BAB19D;
			RoundRect(Left + 1, Top - 3, Right - 1, Bottom, 6, 6);
			MoveTo(Left, Top);
			LineTo(Right, Top);
		end;
	end;

	procedure DrawHighlightTab;
	begin
		with Canvas, inRect do
		begin
			Brush.Color := $FEFCFC;
			Pen.Color := $2C8BE6;
			RoundRect(Left, Top - 3, Right, Bottom, 6, 6);
			Pen.Color := $3CC7FF;
			MoveTo(Left + 2, Bottom - 2);
			LineTo(Right - 2, Bottom - 2);
			MoveTo(Left + 1, Bottom - 3);
			LineTo(Right - 1, Bottom - 3);
			Pen.Color := $BAB19D;
			MoveTo(Left, Top);
			LineTo(Left, Bottom - 4);
			MoveTo(Right - 1, Top);
			LineTo(Right - 1, Bottom - 4);
		end;
	end;

begin
	case Ord(inIndex = Selected) of
		0:
		begin
			Dec(inRect.Bottom, 3);
			if inIndex = Hot then
				DrawHighlightTab
			else
				DrawNormalTab;
		end;
		//
		else begin
			Dec(inRect.Left);
			Inc(inRect.Right);
			DrawHighlightTab;
		end;
	end;
end;

procedure TLrCustomTabControl.PaintTopTab(inRect: TRect; inIndex: Integer);

	procedure DrawNormalTab;
	begin
		with Canvas, inRect do
		begin
			Brush.Color := $F0EBEB;
			Pen.Color := $BAB19D;
			RoundRect(Left + 1, Top, Right - 1, Bottom + 3, 6, 6);
			MoveTo(Left, Bottom - 1);
			LineTo(Right, Bottom - 1);
		end;
	end;

	procedure DrawHighlightTab;
	begin
		with Canvas, inRect do
		begin
			Brush.Color := $FEFCFC;
			Pen.Color := $2C8BE6;
			RoundRect(Left, Top, Right, Bottom + 3, 6, 6);
			Pen.Color := $3CC7FF;
			MoveTo(Left + 2, Top + 1);
			LineTo(Right - 2, Top + 1);
			MoveTo(Left + 1, Top + 2);
			LineTo(Right - 1, Top + 2);
			Pen.Color := $BAB19D;
			MoveTo(Left, Top + 3);
			LineTo(Left, Bottom);
			MoveTo(Right - 1, Top + 3);
			LineTo(Right - 1, Bottom);
		end;
	end;

begin
	case Ord(inIndex = Selected) of
		0:
		begin
			Inc(inRect.Top, 3);
			if (inIndex = Hot) and Enabled and (TabStates[inIndex] = tsEnabled) then
				DrawHighlightTab
			else
				DrawNormalTab;
		end;
			//
		else begin
			Dec(inRect.Left);
			Inc(inRect.Right);
			//if Enabled and (TabStates[inIndex] = tsEnabled) then
				DrawHighlightTab
			//else
			//	DrawNormalTab;
		end;
	end;
end;

procedure TLrCustomTabControl.SetTabPosition(const Value: TLrTabPosition);
begin
	if (FTabPosition <> Value) then
	begin
		FTabPosition := Value;
		Invalidate;
	end;
end;

procedure TLrCustomTabControl.SetCount(const Value: Integer);
begin
	if (FCount <> Value) then
	begin
		FCount := Value;
		Invalidate;
	end;
end;

function TLrCustomTabControl.GetCount: Integer;
begin
	Result := FCount;
end;

procedure TLrCustomTabControl.SetSelected(const Value: Integer);
var
	s: Integer;
begin
	if (csLoading in ComponentState) then
		FSelected := Value
	else if (FSelected <> Value) then
	begin
		s := Value;
		if Assigned(OnBeforeSelect) then
			OnBeforeSelect(Self, s);
		if (s <> FSelected) and IsGoodIndex(s) then
		begin
			FSelected := s;
			Invalidate;
			Update;
			if Assigned(OnSelect) then
				OnSelect(Self);
		end;
	end;
end;

function TLrCustomTabControl.GetTabHeight: Integer;
begin
	Result := Height;
end;

procedure TLrCustomTabControl.SetTabHeight(const Value: Integer);
begin
	Height := Value;
end;

procedure TLrCustomTabControl.SetHot(const Value: Integer);
begin
	if (FHot <> Value) then
	begin
		FHot := Value;
		Invalidate;
	end;
end;

procedure TLrCustomTabControl.CMMouseLeave(var Message: TMessage);
begin
	Hot := -1;
end;

procedure TLrCustomTabControl.SetOnSelect(const Value: TNotifyEvent);
begin
	FOnSelect := Value;
end;

procedure TLrCustomTabControl.SetGlyphVisible(const Value: Boolean);
begin
	FGlyphVisible := Value;
	if Value then
		LoadGlyphs
	else begin
		FGlyph.Width := 0;
		FHotGlyph.Width := 0;
	end;
end;

procedure TLrCustomTabControl.SetOnBeforeSelect(
	const Value: TLrBeforeSelectEvent);
begin
	FOnBeforeSelect := Value;
end;

function TLrCustomTabControl.GetTabStates(inIndex: Integer): TLrTabState;
begin
	Result := tsEnabled;
	if IsGoodIndex(inIndex) and (inIndex < Length(FTabStates)) then
		Result := FTabStates[inIndex];
end;

procedure TLrCustomTabControl.SetTabStates(inIndex: Integer;
	const Value: TLrTabState);
begin
	if IsGoodIndex(inIndex) then
	begin
		if inIndex >= Length(FTabStates) then
			SetLength(FTabStates, inIndex + 1);
		FTabStates[inIndex] := Value;
		Invalidate;
	end;
end;

{ TLrTabControl }

constructor TLrTabControl.Create(AOwner: TComponent);
begin
	inherited;
	FTabs := TStringList.Create;
	FTabs.OnChange := TabsChange;
end;

destructor TLrTabControl.Destroy;
begin
	FTabs.Free;
	inherited;
end;

function TLrTabControl.GetCount: Integer;
begin
	Result := FTabs.Count;
end;

function TLrTabControl.GetTabCaption(inIndex: Integer): string;
begin
	Result := inherited GetTabCaption(inIndex);
	if Result = '' then
		Result := FTabs[inIndex];
end;

procedure TLrTabControl.TabsChange(inSender: TObject);
begin
	if not (csLoading in ComponentState) then
	begin
		Invalidate;
		Selected := Selected;
	end;
end;

procedure TLrTabControl.SetTabs(const Value: TStringList);
begin
	FTabs.Assign(Value);
end;

{ TLrTabSheet }

constructor TLrTabSheet.Create(AOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle + [ csAcceptsControls ];
end;

destructor TLrTabSheet.Destroy;
begin
	if (Parent is TLrCustomPageControl) then
		TLrCustomPageControl(Parent).RemoveSheet(Self);
	inherited;
end;

procedure TLrTabSheet.Deselect;
begin
//	Visible := false;
end;

procedure TLrTabSheet.Select;
begin
//	Visible := true;
	Top := 0;
	BringToFront;
end;

{ TLrCustomPageControl }

constructor TLrCustomPageControl.Create(AOwner: TComponent);
begin
	inherited;
	//	ControlStyle := ControlStyle - [ csAcceptsControls ];
	FSheets := TList.Create;
end;

destructor TLrCustomPageControl.Destroy;
begin
	FSheets.Destroy;
	inherited;
end;

procedure TLrCustomPageControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
//var
//	i: Integer;
begin
	inherited;
//	for i := 0 to Pred(FSheets.Count) do
//		Proc(TComponent(FSheets[i]));
end;

function TLrCustomPageControl.IsGoodIndex(inIndex: Integer): Boolean;
begin
	Result := (inIndex >= 0) and (inIndex < Count);
end;

function TLrCustomPageControl.GetSheet(inIndex: Integer): TLrTabSheet;
begin
	Result := TLrTabSheet(FSheets[inIndex]);
end;

function TLrCustomPageControl.FindSheet(inSheet: TControl): Integer;
begin
	Result := FSheets.IndexOf(inSheet);
end;

procedure TLrCustomPageControl.Loaded;
var
	i: Integer;
begin
	inherited;
	for i := 0 to Pred(ControlCount) do
		if Controls[i] is TLrTabSheet then
			FSheets.Add(Controls[i]);
	if IsGoodIndex(ActivePageIndex) then
		Sheet[ActivePageIndex].Select;
end;

procedure TLrCustomPageControl.Resize;
//var
//	i: Integer;
begin
//	for i := 0 to Pred(Count) do
//		Sheet[i].Height := Height - 4;
	inherited;
end;

procedure TLrCustomPageControl.AdjustClientRect(var Rect: TRect);
begin
	//Dec(Rect.Bottom, 4);
	//Inc(Rect.Top, TabHeight);
end;

procedure TLrCustomPageControl.SetActivePageIndex(const Value: Integer);
begin
	if IsGoodIndex(Value) and (ActivePageIndex <> Value) then
	begin
		FActivePageIndex := Value;
		Sheet[ActivePageIndex].Select;
		Invalidate;
	end;
end;

function TLrCustomPageControl.GetSheetNamePrefix: string;
begin
	Result := 'LrTabSheet';
end;

function TLrCustomPageControl.CreateSheet: TLrTabSheet;
begin
	Result := TLrTabSheet.Create(Owner);
	if TCustomForm(Owner).Designer <> nil then
		Result.Name := TCustomForm(Owner).Designer.UniqueName(GetSheetNamePrefix);
	Result.Align := alClient;
	Result.Color := clWhite;
	Result.Parent := Self;
end;

function TLrCustomPageControl.AddSheet: TLrTabSheet;
begin
	Result := CreateSheet;
	FSheets.Add(Result);
end;

procedure TLrCustomPageControl.RemoveSheet(inSheet: TLrTabSheet);
begin
	FSheets.Remove(inSheet);
end;

function TLrCustomPageControl.GetCount: Integer;
begin
	Result := FSheets.Count;
end;

procedure TLrCustomPageControl.SetCount(const Value: Integer);
begin
	if not (csLoading in ComponentState) then
	begin
		while FSheets.Count < Value do
			AddSheet;
		while (Value >= 0) and (Value < FSheets.Count) do
			Sheet[Pred(Count)].Free;
		ActivePageIndex := Pred(Value);
	end;
end;

procedure TLrCustomPageControl.Notification(AComponent: TComponent;
	Operation: TOperation);
//var
//	i: Integer;
begin
	inherited;
{
	if Operation = opRemove then
		for i := 0 to Pred(Count) do
			if Sheet[i] = AComponent then
			begin
				RemoveSheet(Sheet[i]);
				break;
			end;
}
end;

procedure TLrCustomPageControl.RenameSheets;
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		Sheet[i].Name := Format('%s%d', [ GetSheetNamePrefix, i + 1 ]);
end;

procedure TLrCustomPageControl.Paint;
begin
	//
end;

procedure TLrCustomPageControl.WMEraseBkGnd(var Msg: TWMEraseBkGnd);
begin
//	if Transparent or Opaque then
	if Count > 0 then
		Msg.Result := 0
	else
		inherited;
end;

{ TLrPageControl }

constructor TLrPageControl.Create(AOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle - [ csAcceptsControls ];
	TabWidth := 64;
	TabHeight := 18;
end;

destructor TLrPageControl.Destroy;
begin
	inherited;
end;

procedure TLrPageControl.GetChildren(Proc: TGetChildProc;
	Root: TComponent);
var
	i: Integer;
begin
	for i := 0 to Pred(FSheets.Count) do
		Proc(TComponent(FSheets[i]));
end;

procedure TLrPageControl.CMDesignHitTest(var Message: TCMDesignHitTest);
var
	p: TPoint;
begin
	Message.Result := 0;
	if (Message.Keys and MK_LBUTTON) = MK_LBUTTON then
	begin
		p := SmallPointToPoint(Message.Pos);
		if HitTest(p.x, p.y) then
			Message.Result := 1;
	end;
end;

procedure TLrPageControl.AdjustClientRect(var Rect: TRect);
begin
	inherited;
	case TabPosition of
		ltpTop: Inc(Rect.Top, TabHeight);
		ltpBottom: Dec(Rect.Bottom, TabHeight);
	end;
end;

procedure TLrPageControl.PaintTopTab(inRect: TRect; inState: Integer;
	const inCaption: string);
begin
	with Canvas, inRect do
		case inState of
			0:
			begin
				Inc(Top, 3);
				Brush.Color := $F0EBEB;
				Pen.Color := $BAB19D;
				RoundRect(Left + 1, Top, Right - 1, Bottom + 3, 6, 6);
				MoveTo(Left, Bottom - 1);
				LineTo(Right, Bottom - 1);
				Canvas.TextOut(Left + 6, Top + 1, inCaption);
			end;
			//
			else
			begin
				Dec(Left);
				Inc(Right);
				Brush.Color := $FEFCFC;
				Pen.Color := $2C8BE6;
				RoundRect(Left, Top, Right, Bottom + 3, 6, 6);
				Pen.Color := $3CC7FF;
				MoveTo(Left + 2, Top + 1);
				LineTo(Right - 2, Top + 1);
				MoveTo(Left + 1, Top + 2);
				LineTo(Right - 1, Top + 2);
				Pen.Color := $BAB19D;
				MoveTo(Left, Top + 3);
				LineTo(Left, Bottom);
				MoveTo(Right - 1, Top + 3);
				LineTo(Right - 1, Bottom);
				Canvas.TextOut(Left + 6, Top + 3, inCaption);
			end;
		end;
end;

procedure TLrPageControl.PaintBottomTab(inRect: TRect; inState: Integer;
	const inCaption: string);
begin
	with Canvas, inRect do
		case inState of
			0:
			begin
				Brush.Color := $F0EBEB;
				Pen.Color := $BAB19D;
				Dec(Bottom, 3);
				RoundRect(Left + 1, Top - 3, Right - 1, Bottom, 6, 6);
				MoveTo(Left, Top);
				LineTo(Right, Top);
				Canvas.TextOut(Left + 6, Top + 1, inCaption);
			end;
			//
			else
			begin
				Brush.Color := $FEFCFC;
				Pen.Color := $2C8BE6;
				Dec(Left);
				Inc(Right);
				RoundRect(Left, Top - 3, Right, Bottom, 6, 6);
				Pen.Color := $3CC7FF;
				MoveTo(Left + 2, Bottom - 1);
				LineTo(Right - 2, Bottom - 1);
				MoveTo(Left + 1, Bottom - 2);
				LineTo(Right - 1, Bottom - 2);
				Pen.Color := $BAB19D;
				MoveTo(Left, Top);
				LineTo(Left, Bottom - 3);
				MoveTo(Right - 1, Top);
				LineTo(Right - 1, Bottom - 3);
				Canvas.TextOut(Left + 6, Top + 3, inCaption);
			end;
		end;
end;

function TLrPageControl.GetTabWidth(inIndex: Integer): Integer;
begin
	Result := Canvas.TextWidth(Sheet[inIndex].Caption) + 12;
end;

procedure TLrPageControl.Paint;
var
	i: Integer;
	r: TRect;
begin
	r := Rect(0, 0, ClientWidth, TabHeight);
	case TabPosition of
		ltpBottom: OffsetRect(r, 0, ClientHeight - TabHeight);
	end;
	Canvas.Brush.Color := Color;
	Canvas.FillRect(r);
	//
	Canvas.Pen.Color := clBlack;
	SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
	//
	for i := 0 to Pred(Count) do
	begin
		r.Right := r.Left + GetTabWidth(i);
		case TabPosition of
			ltpTop: PaintTopTab(r, Ord(i = ActivePageIndex), Sheet[i].Caption);
			ltpBottom: PaintBottomTab(r, Ord(i = ActivePageIndex), Sheet[i].Caption);
		end;
		r.Left := r.Right;
	end;
	//
	Canvas.Pen.Color := $BAB19D;
	case TabPosition of
		ltpTop:
		begin
			Canvas.MoveTo(r.Left, TabHeight - 1);
			Canvas.LineTo(ClientWidth, TabHeight - 1);
		end;
		ltpBottom:
		begin
			Canvas.MoveTo(r.Left, r.Bottom - TabHeight + 1);
			Canvas.LineTo(ClientWidth, r.Bottom - TabHeight + 1);
		end;
	end;
end;

function TLrPageControl.HitTest(inX, inY: Integer): Boolean;
begin
	case TabPosition of
		ltpTop: Result := (inY < TabHeight);
		else Result := (inY > ClientHeight - TabHeight);
	end;
	if Result then
		ActivePageIndex := inX div TabWidth;
end;

procedure TLrPageControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	inherited;
	HitTest(X, Y);
end;

procedure TLrPageControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	inherited;
end;

procedure TLrPageControl.ShowControl(AControl: TControl);
var
	i: Integer;
begin
	inherited;
	i := FindSheet(AControl);
	if (i >= 0) then
		ActivePageIndex := i;
end;

procedure TLrPageControl.SetTabPosition(const Value: TLrTabPosition);
begin
	FTabPosition := Value;
	Realign;
	Invalidate;
end;

initialization
	RegisterClass(TLrTabSheet);
end.
