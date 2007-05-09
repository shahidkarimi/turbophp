unit DesignImp;

interface

uses
	Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, ExtCtrls,
	Contnrs,
	DesignSurface;

const
	cDesignDefaultHandleWidth = 8;

type
	TDesignHandle = class(TCustomControl)
	private
		FResizeable: Boolean;
	protected
		function HandleRect(inIndex: Integer): TRect;
		function HitRect(inPoint: TPoint): Integer;
		procedure Paint; override;
		procedure PaintEdge(const inRect: TRect);
		procedure PaintHandle(const inRect: TRect);
		procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
		property Resizeable: Boolean read FResizeable write FResizeable;
	end;
	//
	TDesignHandles = class(TComponent)
	private
		FContainer: TWinControl;
		FSelected: TControl;
		FResizeable: Boolean;
	protected
		function GetHandleWidth: Integer;
		function GetSelectionRect: TRect;
		function SelectedToScreenRect(const inRect: TRect): TRect;
		procedure CreateHandles;
		procedure SetContainer(const Value: TWinControl);
		procedure SetHandleRects(const inRect: TRect);
		procedure SetResizeable(const Value: Boolean);
		procedure SetSelected(const Value: TControl);
		procedure ShowHideHandles(inShow: Boolean);
	public
		Handles: array[0..3] of TDesignHandle;
		constructor Create(inOwner: TComponent); override;
		function HitRect(X, Y: Integer): TDesignHandleId;
		function SelectedToContainer(const inPt: TPoint): TPoint;
		procedure RepaintHandles;
		procedure UpdateHandleRects;
		procedure UpdateHandles;
		property Container: TWinControl read FContainer write SetContainer;
		property HandleWidth: Integer read GetHandleWidth;
		property Resizeable: Boolean read FResizeable write SetResizeable;
		property Selected: TControl read FSelected write SetSelected;
	end;
	//
	TDesignSelector = class(TDesignCustomSelector)
	private
		FHandles: TObjectList;
		FHandleWidth: Integer;
	protected
		function FindHandles(inValue: TControl): TDesignHandles;
		function GetCount: Integer; override;
		function GetHandles(inIndex: Integer): TDesignHandles;
		function GetSelection(inIndex: Integer): TControl; override;
		procedure SetHandles(inIndex: Integer; inValue: TDesignHandles);
		procedure SetHandleWidth(inValue: Integer);
		procedure SetSelection(inIndex: Integer; inValue: TControl); override;
		procedure ShowHideResizeHandles;
		property Handles[inIndex: Integer]: TDesignHandles read GetHandles
			write SetHandles;
	public
		constructor Create(inSurface: TDesignSurface); override;
		destructor Destroy; override;
		function GetClientControl(inControl: TControl): TControl; override;
		function GetCursor(inX, inY: Integer): TCursor; override;
		function GetHitHandle(inX, inY: Integer): TDesignHandleId; override;
		function IsSelected(inValue: TControl): Boolean; override;
		procedure AddToSelection(inValue: TControl); override;
		procedure ClearSelection; override;
		procedure RemoveFromSelection(inValue: TControl); override;
		procedure ShowHideSelection(inShow: Boolean); override;
		procedure Update; override;
	published
		property HandleWidth: Integer read FHandleWidth
			write SetHandleWidth default cDesignDefaultHandleWidth;
	end;
	//
	TDesignCustomMouseTool = class
	private
		FDragRect: TRect;
		FOnMouseMove: TNotifyEvent;
	public
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); virtual; abstract;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual; abstract;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);  virtual; abstract;
		property DragRect: TRect read FDragRect write FDragRect;
		property OnMouseMove: TNotifyEvent read FOnMouseMove write FOnMouseMove;
	end;
	//
	TDesignDragMode = ( dmNone, dmMove, dmResize, dmSelect, dmCreate );
	//
	TDesignAction = ( daSelectParent, daDelete, daCopy, daCut, daPaste,
		daNudgeLeft, daNudgeRight, daNudgeUp, daNudgeDown, daGrowWidth,
		daShrinkWidth, daGrowHeight, daShrinkHeight, daLastAction = MAXINT
	);
	//
	TDesignController = class(TDesignCustomController)
	private
		FClicked: TControl;
		FDragMode: TDesignDragMode;
		FDragRect: TRect;
		FKeyDownShift: TShiftState;
		FMouseIsDown: Boolean;
		FMouseTool: TDesignCustomMouseTool;
	protected
		function GetDragRect: TRect; override;
		function KeyDown(inKeycode: Cardinal): Boolean; override;
		function KeyUp(inKeycode: Cardinal): Boolean; override;
		function MouseDown(Button: TMouseButton; X, Y: Integer): Boolean; override;
		function MouseMove(X, Y: Integer): Boolean; override;
		function MouseUp(Button: TMouseButton; X, Y: Integer): Boolean; override;
		procedure Action(inAction: TDesignAction);
		procedure MouseDrag(inSender: TObject);
	end;
	//
	TDesignMouseTool = class(TDesignCustomMouseTool)
	private
		FSurface: TDesignSurface;
		FMouseLast: TPoint;
		FMouseStart: TPoint;
	protected
		function GetMouseDelta: TPoint; virtual;
	public
		constructor Create(AOwner: TDesignSurface); virtual;
		property Surface: TDesignSurface read FSurface write FSurface;
	end;
	//
	TDesignMover = class(TDesignMouseTool)
	private
		FDragRects: array of TRect;
	protected
		procedure ApplyDragRects;
		procedure CalcDragRects;
		procedure CalcPaintRects;
		procedure PaintDragRects;
	public
		constructor Create(AOwner: TDesignSurface); override;
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
	end;
	//
	TDesignBander = class(TDesignMouseTool)
	protected
		function GetClient: TControl; virtual;
		function GetPaintRect: TRect;
		procedure CalcDragRect; virtual;
		procedure PaintDragRect; virtual;
	public
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
	end;
  //
  TDesignSizer = class(TDesignBander)
  private
    FHandleId: TDesignHandleId;
  protected
    function GetClient: TControl; override;
    procedure ApplyDragRect;
    procedure ApplyMouseDelta(X, Y: Integer);
    procedure CalcDragRect; override;
  public
    constructor CreateSizer(AOwner: TDesignSurface;
      inHandle: TDesignHandleId);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  end;
  //
  TDesignDesigner = class(TComponent, IDesignerHook)
  private
    FMessenger: TDesignCustomMessenger;
  public
    constructor Create(inMessenger: TDesignCustomMessenger); reintroduce;
    function GetCustomForm: TCustomForm;
    function GetIsControl: Boolean;
    function GetRoot: TComponent;
    function IsDesignMsg(Sender: TControl; var Message: TMessage): Boolean;
    function UniqueName(const BaseName: string): string;
    procedure Modified;
    procedure Notification(AnObject: TPersistent; Operation: TOperation); reintroduce;
    procedure PaintGrid;
    procedure SetCustomForm(Value: TCustomForm);
    procedure SetIsControl(Value: Boolean);
    procedure ValidateRename(AComponent: TComponent;
      const CurName, NewName: string); reintroduce;
    property IsControl: Boolean read GetIsControl write SetIsControl;
    property Form: TCustomForm read GetCustomForm write SetCustomForm;
    property Messenger: TDesignCustomMessenger read FMessenger write FMessenger;
  end;
  //
  TDesignDesignerMessenger = class(TDesignCustomMessenger)
  private
    FDesignedForm: TCustomForm;
		FDesigner: TDesignDesigner;
  protected
    procedure SetComponentDesigning(inComponent: TComponent;
      inDesigning: Boolean);
    procedure SetContainer(inValue: TWinControl); override;
    procedure UndesignComponent(inComponent: TComponent);
	public
    constructor Create; override;
    destructor Destroy; override;
    procedure DesignComponent(inComponent: TComponent); override;
  end;
  //
  TDesignMessageHookList = class(TComponent)
  private
    FHooks: TObjectList;
    FUser: TDesignCustomMessenger;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(inUser: TDesignCustomMessenger); reintroduce;
    destructor Destroy; override;
    procedure Clear;
    procedure Hook(inClient: TWinControl);
    procedure Unhook(inComponent: TComponent);
  end;
  //
  TDesignWinControlHookMessenger = class(TDesignCustomMessenger)
  private
    FHooks: TDesignMessageHookList;
  protected
    procedure HookWinControl(inWinControl: TWinControl);
    procedure SetContainer(inValue: TWinControl); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure DesignComponent(inComponent: TComponent); override;
  end;

implementation

uses
  DesignUtils;

var
	ShadedBits: TBitmap;

function NeedShadedBits: TBitmap;
begin
  if ShadedBits = nil then
  begin
    ShadedBits := TBitmap.Create;
    with ShadedBits do
    begin
      Width := 4;
      Height := 2;
			Canvas.Pixels[0, 0] := clGray;
      Canvas.Pixels[1, 0] := clBtnFace;
      Canvas.Pixels[2, 0] := clBtnFace;
      Canvas.Pixels[3, 0] := clBtnFace;
      Canvas.Pixels[0, 1] := clBtnFace;
      Canvas.Pixels[1, 1] := clBtnFace;
      Canvas.Pixels[2, 1] := clGray;
      Canvas.Pixels[3, 1] := clBtnFace;
    end;
  end;
  Result := ShadedBits;
end;

procedure FreeShadedBits;
begin
  FreeAndNil(ShadedBits);
end;

{ TDesignHandle }

function TDesignHandle.HandleRect(inIndex: Integer): TRect;
var
  w: Integer;
begin
  w := TDesignHandles(Owner).HandleWidth;
  case inIndex of
    0: Result := Rect(0, 0, w, w); // left-top
    1: Result := Rect((Width - w) div 2, 0, (Width + w) div 2, w); // middle-top
    2: Result := Rect(Width - w, 0, Width, w); // right-top
    3: Result := Rect(0, (Height - w) div 2, w, (Height + w) div 2); // left-center
  end;
end;

procedure TDesignHandle.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
	Message.Result := 1;
end;

procedure TDesignHandle.PaintHandle(const inRect: TRect);
begin
  Canvas.Rectangle(inRect);
end;

procedure TDesignHandle.PaintEdge(const inRect: TRect);
begin
  Canvas.FillRect(ClientRect);
end;

procedure TDesignHandle.Paint;
begin
  with Canvas do
	begin
    Brush.Bitmap := NeedShadedBits;
    PaintEdge(ClientRect);
    Brush.Bitmap := nil;
    Brush.Color := clWhite;
    Pen.Color := clBlack;
    if Resizeable then
      if (Width > Height) then
      begin
        PaintHandle(HandleRect(0));
        PaintHandle(HandleRect(1));
        PaintHandle(HandleRect(2));
      end
      else
        PaintHandle(HandleRect(3));
  end;
end;

function TDesignHandle.HitRect(inPoint: TPoint): Integer;
begin
  Result := -1;
  if Width > Height then
    if PtInRect(HandleRect(0), inPoint) then
      Result := 0
    else if PtInRect(HandleRect(1), inPoint) then
      Result := 1
    else if PtInRect(HandleRect(2), inPoint) then
      Result := 2;
  if Result < 0 then
    if PtInRect(HandleRect(3), inPoint) then
			Result := 3;
end;

{ TDesignHandles }

constructor TDesignHandles.Create(inOwner: TComponent);
begin
	inherited;
  CreateHandles;
  Resizeable := true;
end;

procedure TDesignHandles.CreateHandles;
var
  i: Integer;
begin
  for i := 0 to 3 do
    Handles[i] := TDesignHandle.Create(Self);
end;

function TDesignHandles.GetHandleWidth: Integer;
begin
  Result := TDesignSelector(Owner).HandleWidth;
end;

procedure TDesignHandles.SetContainer(const Value: TWinControl);
var
  i: Integer;
begin
  FContainer := Value;
  for i := 0 to 3 do
    with Handles[i] do
    begin
      Visible := false;
			Parent := Container;
		end;
end;

procedure TDesignHandles.SetSelected(const Value: TControl);
begin
	if (Selected <> Value) then
	begin
		if (Value is TDesignHandle) then
			FSelected := nil
		else
			FSelected := Value;
		UpdateHandleRects;
		//UpdateHandles;
	end;
end;

procedure TDesignHandles.SetResizeable(const Value: Boolean);
var
	i: Integer;
begin
	FResizeable := Value;
	for i := 0 to 3 do
		Handles[i].Resizeable := Value;
end;

procedure TDesignHandles.ShowHideHandles(inShow: Boolean);
var
	i: Integer;
begin
	for i := 0 to 3 do
		with Handles[i] do
		begin
			Visible := inShow;
			if inShow then
			begin
				BringToFront;
				//Update;
			end;
		end;
end;

procedure TDesignHandles.UpdateHandleRects;
begin
	SetHandleRects(GetSelectionRect);
end;

procedure TDesignHandles.UpdateHandles;
begin
	if (Selected <> nil) and (Container <> nil) and (Selected <> Container) then
	begin
		UpdateHandleRects;
		ShowHideHandles(true);
		Container.Update;
	end else
		ShowHideHandles(false)
end;

procedure TDesignHandles.RepaintHandles;
var
	i: Integer;
begin
	for i := 0 to 3 do
		Handles[i].Repaint;
end;

function TDesignHandles.HitRect(X, Y: Integer): TDesignHandleId;
const
	cRectIds: array[0..3, 0..3] of TDesignHandleId = (
		( dhLeftTop, dhMiddleTop, dhRightTop, dhNone ),
		( dhNone, dhNone, dhNone, dhLeftMiddle ),
		( dhNone, dhNone, dhNone, dhRightMiddle ),
    ( dhLeftBottom, dhMiddleBottom, dhRightBottom, dhNone )
  );
var
  i, r: Integer;
begin
  for i := 0 to 3 do
  begin
		with Handles[i] do
      r := HitRect(Point(X - Left, Y - Top));
    if (r >= 0) then
    begin
      Result := cRectIds[i][r];
      exit;
    end;
  end;
  Result := dhNone;
end;

function TDesignHandles.SelectedToContainer(const inPt: TPoint): TPoint;
var
  c: TControl;
begin
  Result := inPt;
	c := Selected.Parent;
  while (c <> Container) and (c <> nil) do
  begin
    Inc(Result.X, c.Left);
    Inc(Result.Y, c.Top);
    c := c.Parent;
  end;
end;

function TDesignHandles.SelectedToScreenRect(const inRect: TRect): TRect;
var
  p: TWinControl;
begin
  if Selected = Container then
    p := Container
  else
    p := Selected.Parent;
  Result.topLeft := p.ClientToScreen(inRect.topLeft);
  Result.bottomRight := p.ClientToScreen(inRect.bottomRight);
end;

function TDesignHandles.GetSelectionRect: TRect;
var
  p: TPoint;
begin
  if (Selected = Container) then
    p := Point(0, 0)
  else
    p := SelectedToContainer(Selected.BoundsRect.topLeft);
  Result := Rect(p.X, p.Y, p.X + Selected.Width, p.y + Selected.Height);
  InflateRect(Result, -HandleWidth div 2, -HandleWidth div 2);
end;

procedure TDesignHandles.SetHandleRects(const inRect: TRect);
var
  w: Integer;
begin
  w := HandleWidth;
  with inRect do
  begin
    Handles[0].BoundsRect := Rect(Left - w, Top - w, Right + w, Top);
    Handles[1].BoundsRect := Rect(Left - w, Top, Left, Bottom);
    Handles[2].BoundsRect := Rect(Right, Top, Right + w, Bottom);
    Handles[3].BoundsRect := Rect(Left - w, Bottom, Right + w, Bottom + w);
  end;
end;

{ TDesignSelector }

constructor TDesignSelector.Create(inSurface: TDesignSurface);
begin
	inherited;
  //ControllerClass := TDesignController;
  FHandleWidth := cDesignDefaultHandleWidth;
  FHandles := TObjectList.Create;
end;

destructor TDesignSelector.Destroy;
begin
  FHandles.Free;
  inherited;
end;

procedure TDesignSelector.SetHandleWidth(inValue: Integer);
begin
  FHandleWidth := inValue;
  Update;
end;

function TDesignSelector.GetCount: Integer;
begin
  Result := FHandles.Count;
end;

function TDesignSelector.GetHandles(inIndex: Integer): TDesignHandles;
begin
  Result := TDesignHandles(FHandles[inIndex]);
end;

procedure TDesignSelector.SetHandles(inIndex: Integer; inValue: TDesignHandles);
begin
  FHandles[inIndex] := inValue;
end;

function TDesignSelector.GetSelection(inIndex: Integer): TControl;
begin
  Result := Handles[inIndex].Selected;
end;

procedure TDesignSelector.SetSelection(inIndex: Integer; inValue: TControl);
begin
  Handles[inIndex].Selected := inValue;
end;

function TDesignSelector.FindHandles(inValue: TControl): TDesignHandles;
var
  i: Integer;
begin
  for i := 0 to Pred(Count) do
  begin
    Result := Handles[i];
    if Result.Selected = inValue then
      exit;
  end;
  Result := nil;
end;

function TDesignSelector.IsSelected(inValue: TControl): Boolean;
begin
  Result := FindHandles(inValue) <> nil;
end;

procedure TDesignSelector.ClearSelection;
begin
	//if not (csDestroying in ComponentState) then
  FHandles.Clear;
end;

procedure TDesignSelector.ShowHideSelection(inShow: Boolean);
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		Handles[i].ShowHideHandles(inShow);
	Surface.Container.Update;
end;

procedure TDesignSelector.ShowHideResizeHandles;
var
	i: Integer;
begin
  for i := 0 to Pred(Count) do
    with Handles[i] do
    begin
      Resizeable := (Count = 1);
      RepaintHandles;
    end;
end;

procedure TDesignSelector.AddToSelection(inValue: TControl);
var
	h: TDesignHandles;
begin
	if inValue = nil then
		raise Exception.Create('Cannot add a nil selection.');
	if not IsSelected(inValue) then
	begin
		h := TDesignHandles.Create(Self);
		h.Container := Surface.Container;
		h.Resizeable := Count = 0;
		FHandles.Add(h);
		h.Selected := inValue;
		if (Count = 2) then
			ShowHideResizeHandles;
		//else
		//	h.UpdateHandleRects;
		Surface.Messenger.DesignComponent(h.Handles[0]);
		Surface.Messenger.DesignComponent(h.Handles[1]);
		Surface.Messenger.DesignComponent(h.Handles[2]);
		Surface.Messenger.DesignComponent(h.Handles[3]);
	end;
end;

procedure TDesignSelector.RemoveFromSelection(inValue: TControl);
begin
	if IsSelected(inValue) then
	begin
		FHandles.Remove(FindHandles(inValue));
		Surface.SelectionChange;
	end;
end;

function TDesignSelector.GetClientControl(inControl: TControl): TControl;
begin
	if (inControl is TDesignHandle) then
		Result := TDesignHandles(inControl.Owner).Selected
	else
		Result := inControl;
end;

procedure TDesignSelector.Update;
var
	i: Integer;
begin
  for i := 0 to Pred(Count) do
    Handles[i].UpdateHandles;
end;

function TDesignSelector.GetHitHandle(inX, inY: Integer): TDesignHandleId;
begin
  if (Count > 0) then
    Result := Handles[0].HitRect(inX, inY)
  else
    Result := dhNone;
end;

function TDesignSelector.GetCursor(inX, inY: Integer): TCursor;
const
  cCurs: array[TDesignHandleId] of TCursor =
		( crHandPoint, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeWE,
      crSizeNESW, crSizeNS, crSizeNWSE );
begin
  Result := cCurs[GetHitHandle(inX, inY)]
end;

{ TDesignController }

procedure TDesignController.Action(inAction: TDesignAction);
begin
  with Surface do
    case inAction of
			daSelectParent: SelectParent;
      daDelete: DeleteComponents;
      daCopy: CopyComponents;
      daCut: CutComponents;
      daPaste: PasteComponents;
      daNudgeLeft: NudgeComponents(-1, 0);
      daNudgeRight: NudgeComponents(1, 0);
      daNudgeUp: NudgeComponents(0, -1);
      daNudgeDown: NudgeComponents(0, 1);
      daGrowWidth: GrowComponents(1, 0);
      daShrinkWidth: GrowComponents(-1, 0);
      daGrowHeight: GrowComponents(0, 1);
      daShrinkHeight: GrowComponents(0, -1);
    end;
  Surface.UpdateDesigner;
end;

function TDesignController.GetDragRect: TRect;
begin
  Result := FDragRect;
end;

function TDesignController.KeyDown(inKeycode: Cardinal): Boolean;

  function CtrlKeys: Boolean;
  begin
    Result := true;
    case inKeycode of
			VK_LEFT: Action(daNudgeLeft);
			VK_RIGHT: Action(daNudgeRight);
			VK_UP: Action(daNudgeUp);
			VK_DOWN: Action(daNudgeDown);
			else Result := false;
		end;
	end;

	function ShiftKeys: Boolean;
	begin
		Result := true;
		case inKeycode of
			VK_LEFT: Action(daShrinkWidth);
			VK_RIGHT: Action(daGrowWidth);
			VK_UP: Action(daShrinkHeight);
			VK_DOWN: Action(daGrowHeight);
			else Result := false;
		end;
	end;

	function Keys: Boolean;
	begin
		Result := true;
		case inKeycode of
			VK_TAB: Surface.Selector.ShowHideSelection(false);
			else Result := false;
		end;
	end;

begin
	FKeyDownShift := Shift;
	if ssCtrl in FKeyDownShift then
		Result := CtrlKeys
	else if ssShift in FKeyDownShift then
		Result := ShiftKeys
	else
		Result := Keys;
end;

function TDesignController.KeyUp(inKeycode: Cardinal): Boolean;

	function Keys: Boolean;
  begin
    Result := true;
		case inKeycode of
			VK_ESCAPE: Action(daSelectParent);
			VK_DELETE: Action(daDelete);
			else Result := false;
		end;
	end;

	function CtrlKeys: Boolean;
	begin
		Result := true;
		case inKeycode of
			Ord('C'): Action(daCopy);
			Ord('X'): Action(daCut);
			Ord('V'): Action(daPaste);
			else Result := false;
		end;
	end;

	function ShiftKeys: Boolean;
	begin
		Result := false;
	end;

begin
	FKeyDownShift := FKeyDownShift + Shift;
	if ssCtrl in FKeyDownShift then
    Result := CtrlKeys
  else if ssShift in FKeyDownShift then
    Result := ShiftKeys
  else
    Result := Keys;
  FKeyDownShift := [];
end;

function TDesignController.MouseDown(Button: TMouseButton;
	X, Y: Integer): Boolean;
var
	handleId: TDesignHandleId;

	procedure CaptureMouse;
	begin
		FMouseIsDown := true;
		Mouse.Capture := Surface.Container.Handle;
	end;

	procedure FocusSurface;
	begin
		if not Surface.Container.Focused and Surface.Container.CanFocus then
			Surface.Container.SetFocus;
	end;

	procedure SelectDragMode;
	begin
		handleId := dhNone;
		if (ssCtrl in Shift) then
      // Ctrl-drag selection has highest priority
      FDragMode := dmSelect
    else begin
      handleId := Surface.GetHitHandle(X, Y);
      if (handleId <> dhNone) then
      begin
        FClicked := Surface.Selection[0];
        FDragMode := dmResize;
      end
      else begin
        FClicked := Surface.FindControl(X, Y);
        if (FClicked = Surface.Container) or (FClicked is TDesignHandle) then
          FClicked := nil;
        Surface.GetAddClass;
        if (Surface.AddClass <> '') then
					// then object creation
					FDragMode := dmCreate
				else if FClicked <> nil then
					// moving is last
					FDragMode := dmMove
				else
					// select by default
					FDragMode := dmSelect;
			end;
		end;
		if FClicked = nil then
			FClicked := Surface.Container;
		FClicked.Parent.DisableAlign;
	end;

	procedure CreateMouseTool;
	var
		onDrag: TNotifyEvent;
	begin
		case FDragMode of
			dmSelect, dmCreate:
			begin
				Surface.ClearSelection;
				FMouseTool := TDesignBander.Create(Surface);
			end;
			//
			dmMove:
			begin
				onDrag := nil;
				if (ssShift in Shift) then
				//begin
					//onDrag := MouseDrag;
					Surface.Selector.AddToSelection(FClicked)
				//end
				else begin
					if not Surface.Selector.IsSelected(FClicked) then
						Surface.Select(FClicked);
					Surface.Selector.ShowHideSelection(false);
				end;
				FMouseTool := TDesignMover.Create(Surface);
				//FMouseTool.OnMouseMove := onDrag;
			end;
			//
			dmResize:
			begin
				if not Surface.Selector.IsSelected(FClicked) then
					Surface.Select(FClicked);
				FMouseTool := TDesignSizer.CreateSizer(Surface, handleId);
				Surface.Selector.ShowHideSelection(false);
			end;
		end;
		if FMouseTool <> nil then
			FMouseTool.MouseDown(Button, Shift, X, Y);
	end;

begin
	FocusSurface;
	CaptureMouse;
	SelectDragMode;
	CreateMouseTool;
	Result := true;
end;

procedure TDesignController.MouseDrag(inSender: TObject);
begin
	TDesignCustomMouseTool(inSender).OnMouseMove := nil;
	Surface.Selector.ShowHideSelection(false);
end;

function TDesignController.MouseMove(X, Y: Integer): Boolean;
begin
	if not FMouseIsDown then
		Windows.SetCursor(Screen.Cursors[Surface.GetCursor(X, Y)])
	else
		if FMouseTool <> nil then
			FMouseTool.MouseMove(Shift, X, Y);
	Result := true;
end;

function TDesignController.MouseUp(Button: TMouseButton;
	X, Y: Integer): Boolean;

	procedure ReleaseMouse;
	begin
		FMouseIsDown := false;
		Mouse.Capture := 0;
	end;

	procedure EnableAlign;
	begin
		// If the debugger breaks in during a mouse operation,
		// AlignDisabled can become stuck.
		// This routine is to aid debugging only.
		if FClicked <> nil then
			while FClicked.Parent.AlignDisabled do
				FClicked.Parent.EnableAlign;
	end;

	procedure FinishMouseTool;
	begin
		if FMouseTool <> nil then
		try
			FMouseTool.MouseUp(Button, Shift, X, Y);
			FDragRect := DesignValidateRect(FMouseTool.DragRect);
			case FDragMode of
				dmCreate:
				begin
					if FClicked <> nil then
						Surface.Select(FClicked);
					Surface.AddComponent;
				end;
				else Surface.SelectionChange;
			end;
		finally
			FreeAndNil(FMouseTool);
		end;
	end;

begin
	if FMouseIsDown then
	begin
		ReleaseMouse;
		EnableAlign;
		FinishMouseTool;
		Surface.Selector.ShowHideSelection(true);
		FClicked := nil;
	end;
	Result := true;
end;

{ TDesignMouseTool }

constructor TDesignMouseTool.Create(AOwner: TDesignSurface);
begin
	Surface := AOwner;
end;

function TDesignMouseTool.GetMouseDelta: TPoint;
const
	GridX = 4;
	GridY = 4;
begin
	with Result do
	begin
		X := FMouseLast.X - FMouseStart.X;
		Dec(X, X mod GridX);
		Y := FMouseLast.Y - FMouseStart.Y;
		Dec(Y, Y mod GridY);
	end;
end;

{ TDesignMover }

constructor TDesignMover.Create(AOwner: TDesignSurface);
begin
	inherited;
	SetLength(FDragRects, Surface.Count);
end;

procedure TDesignMover.CalcDragRects;
var
	delta: TPoint;
	i: Integer;
begin
	delta := GetMouseDelta;
	for i := 0 to Pred(Surface.Count) do
		with Surface.Selection[i] do
		begin
			FDragRects[i] := BoundsRect;
			OffsetRect(FDragRects[i], delta.X, delta.Y);
		end;
end;

procedure TDesignMover.CalcPaintRects;
var
	i: Integer;
begin
	CalcDragRects;
	for i := 0 to Pred(Surface.Count) do
		with Surface.Selection[i] do
			with Parent.ClientToScreen(Point(0, 0)) do
				OffsetRect(FDragRects[i], X, Y);
end;

procedure TDesignMover.PaintDragRects;
var
	i: Integer;
begin
	for i := 0 to Pred(Surface.Count) do
		DesignPaintRubberbandRect(FDragRects[i], psDot);
end;

procedure TDesignMover.ApplyDragRects;
var
	i: Integer;
begin
	if (GetMouseDelta.X <> 0) or (GetMouseDelta.Y <> 0) then
	begin
		CalcDragRects;
		for i := 0 to Pred(Surface.Count) do
			Surface.Selection[i].BoundsRect := FDragRects[i];
		Surface.UpdateDesigner;
		Surface.Change;
	end;
end;

procedure TDesignMover.MouseDown(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	FMouseStart := Point(X, Y);
	FMouseLast := FMouseStart;
	CalcPaintRects;
	PaintDragRects;
end;

procedure TDesignMover.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
	PaintDragRects;
	if Assigned(OnMouseMove) and ((FMouseLast.X <> X) or (FMouseLast.Y <> Y)) then
		OnMouseMove(Self);
	FMouseLast := Point(X, Y);
	CalcPaintRects;
	PaintDragRects;
end;

procedure TDesignMover.MouseUp(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	PaintDragRects;
	FMouseLast := Point(X, Y);
	ApplyDragRects;
end;

{ TDesignBander }

procedure TDesignBander.CalcDragRect;
begin
  with GetMouseDelta do
  begin
    DragRect := Rect(0, 0, X, Y);
    OffsetRect(FDragRect, FMouseStart.X, FMouseStart.Y);
  end;
end;

function TDesignBander.GetClient: TControl;
begin
  Result := Surface.Container;
end;

function TDesignBander.GetPaintRect: TRect;
begin
  Result := FDragRect;
  with GetClient.ClientToScreen(Point(0, 0)) do
    OffsetRect(Result, X, Y);
end;

procedure TDesignBander.PaintDragRect;
begin
  DesignPaintRubberbandRect(GetPaintRect, psDot);
end;

procedure TDesignBander.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FMouseStart := Point(X, Y);
  FMouseLast := FMouseStart;
  CalcDragRect;
  PaintDragRect;
end;

procedure TDesignBander.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  PaintDragRect;
  FMouseLast := Point(X, Y);
  CalcDragRect;
  PaintDragRect;
end;

procedure TDesignBander.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  PaintDragRect;
  CalcDragRect;
end;

{ TDesignSizer }

constructor TDesignSizer.CreateSizer(AOwner: TDesignSurface;
  inHandle: TDesignHandleId);
begin
  inherited Create(AOwner);
  FHandleId := inHandle;
end;

procedure TDesignSizer.ApplyMouseDelta(X, Y: Integer);
begin
  case FHandleId of
    dhLeftTop, dhMiddleTop, dhRightTop: Inc(FDragRect.Top, Y);
    dhLeftBottom, dhMiddleBottom, dhRightBottom: Inc(FDragRect.Bottom, Y);
  end;
  case FHandleId of
    dhLeftTop, dhLeftMiddle, dhLeftBottom: Inc(FDragRect.Left, X);
    dhRightTop, dhRightMiddle, dhRightBottom: Inc(FDragRect.Right, X);
  end;
end;

procedure TDesignSizer.CalcDragRect;
begin
  FDragRect := Surface.Selection[0].BoundsRect;
  with GetMouseDelta do
    ApplyMouseDelta(X, Y);
  FDragRect := DesignValidateRect(FDragRect);
end;

function TDesignSizer.GetClient: TControl;
begin
  Result := Surface.Selection[0].Parent;
end;

procedure TDesignSizer.ApplyDragRect;
begin
  Surface.Selection[0].BoundsRect := FDragRect;
  Surface.UpdateDesigner;
  Surface.Change;
end;

procedure TDesignSizer.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  ApplyDragRect;
end;

{ TDesignDesigner }

constructor TDesignDesigner.Create(inMessenger: TDesignCustomMessenger);
begin
  inherited Create(nil);
  FMessenger := inMessenger;
end;

function TDesignDesigner.GetCustomForm: TCustomForm;
begin
  Result := nil;
end;

function TDesignDesigner.GetIsControl: Boolean;
begin
  Result := false;
end;

function TDesignDesigner.GetRoot: TComponent;
begin
  Result := nil;
end;

function TDesignDesigner.IsDesignMsg(Sender: TControl;
  var Message: TMessage): Boolean;
begin
  Result := Messenger.IsDesignMessage(Sender, Message);
end;

procedure TDesignDesigner.Modified;
begin
  //
end;

procedure TDesignDesigner.Notification(AnObject: TPersistent;
  Operation: TOperation);
begin
  //
end;

procedure TDesignDesigner.PaintGrid;
begin
  //
end;

procedure TDesignDesigner.SetCustomForm(Value: TCustomForm);
begin
  //
end;

procedure TDesignDesigner.SetIsControl(Value: Boolean);
begin
  //
end;

function TDesignDesigner.UniqueName(const BaseName: string): string;
begin
  //
end;

procedure TDesignDesigner.ValidateRename(AComponent: TComponent;
  const CurName, NewName: string);
begin
  //
end;

{ TDesignDesignerMessenger }

constructor TDesignDesignerMessenger.Create;
begin
  FDesigner := TDesignDesigner.Create(Self);
end;

destructor TDesignDesignerMessenger.Destroy;
begin
  if Container <> nil then
    SetComponentDesigning(Container, false);
  if (FDesignedForm <> nil) then
    FDesignedForm.Designer := nil;
  FDesigner.Free;
  inherited;
end;

type
  TCrackedComponent = class(TComponent)
  end;

procedure TDesignDesignerMessenger.SetComponentDesigning(inComponent: TComponent;
  inDesigning: Boolean);
begin
  TCrackedComponent(inComponent).SetDesigning(inDesigning);
end;

procedure TDesignDesignerMessenger.UndesignComponent(inComponent: TComponent);
begin
  SetComponentDesigning(inComponent, false);
end;

procedure TDesignDesignerMessenger.DesignComponent(inComponent: TComponent);
begin
  SetComponentDesigning(inComponent, true);
end;

procedure TDesignDesignerMessenger.SetContainer(inValue: TWinControl);

  function FindParentForm: TCustomForm;
  var
    p: TWinControl;
  begin
    p := Container;
    while (p.Parent <> nil) do
      p := p.Parent;
    if not (p is TCustomForm) then
      raise Exception.Create(ClassName + ': Oldest ancestor of Container must be a form.');
    Result := TCustomForm(p);
  end;

begin
  inherited;
  if (Container <> nil) then
  begin
    FDesignedForm := FindParentForm;
    FDesignedForm.Designer := FDesigner;
    DesignChildren(Container);
  end;
end;

{ TDesignMessageHookList }

constructor TDesignMessageHookList.Create(inUser: TDesignCustomMessenger);
begin
  inherited Create(nil);
  FUser := inUser;
  FHooks := TObjectList.Create;
  FHooks.OwnsObjects := true;
end;

destructor TDesignMessageHookList.Destroy;
begin
  FHooks.Free;
  inherited;
end;

procedure TDesignMessageHookList.Clear;
begin
  FHooks.Clear;
end;

procedure TDesignMessageHookList.Hook(inClient: TWinControl);
begin
  inClient.FreeNotification(Self);
  FHooks.Add(TDesignMessageHook.Create(FUser, inClient));
end;

procedure TDesignMessageHookList.Unhook(inComponent: TComponent);
var
  i: Integer;
begin
  for i := 0 to Pred(FHooks.Count) do
    if TDesignMessageHook(FHooks[i]).Client = inComponent then
    begin
      FHooks.Delete(i);
      break;
    end;
end;

procedure TDesignMessageHookList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
    Unhook(AComponent);
end;

{ TDesignWinControlHookMessenger }

constructor TDesignWinControlHookMessenger.Create;
begin
  FHooks := TDesignMessageHookList.Create(Self);
end;

destructor TDesignWinControlHookMessenger.Destroy;
begin
  FHooks.Free;
  inherited;
end;

procedure TDesignWinControlHookMessenger.Clear;
begin
  FHooks.Clear;
end;

procedure TDesignWinControlHookMessenger.DesignComponent(inComponent: TComponent);
begin
  if inComponent is TWinControl then
    HookWinControl(TWinControl(inComponent));
end;

procedure TDesignWinControlHookMessenger.HookWinControl(inWinControl: TWinControl);
begin
  FHooks.Hook(inWinControl);
  DesignChildren(inWinControl);
end;

procedure TDesignWinControlHookMessenger.SetContainer(inValue: TWinControl);
begin
  inherited;
  if (Container <> nil) then
    DesignChildren(Container);
end;

initialization
finalization
  FreeShadedBits;
end.
