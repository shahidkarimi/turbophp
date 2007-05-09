unit DesignSurface;

interface

uses
	Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, ExtCtrls,
	Contnrs;

type
  TDesignSurface = class;
  //
  TDesignMessage = function(inSender: TControl; var inMsg: TMessage;
    const inPt: TPoint): Boolean of object;
  //
  TDesignCustomMessenger = class
  private
    FContainer: TWinControl;
    FOnDesignMessage: TDesignMessage;
  protected
    procedure SetContainer(inValue: TWinControl); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsDesignMessage(inSender: TControl;
      var inMessage: TMessage): Boolean; virtual;
    procedure Clear; virtual;
    procedure DesignChildren(inContainer: TWinControl);
    procedure DesignComponent(inComponent: TComponent); virtual;
    property Container: TWinControl read FContainer write SetContainer;
    property OnDesignMessage: TDesignMessage read FOnDesignMessage
      write FOnDesignMessage;
  end;
  //
  TDesignCustomMessengerClass = class of TDesignCustomMessenger;
  //
  TDesignMessageHook = class
  private
    FClient: TWinControl;
    FOldProc: TWndMethod;
    FUser: TDesignCustomMessenger;
  protected
    procedure HookProc(var inMessage: TMessage);
    procedure Unhook;
  public
    constructor Create(inUser: TDesignCustomMessenger; inClient: TWinControl);
    destructor Destroy; override;
		property Client: TWinControl read FClient;
	end;
  //
  TDesignCustomController = class
	private
		FSurface: TDesignSurface;
  protected
    function GetDragRect: TRect; virtual; abstract;
		function GetShift: TShiftState;
		function KeyDown(inKeycode: Cardinal): Boolean; virtual; abstract;
		function KeyUp(inKeycode: Cardinal): Boolean; virtual; abstract;
		function MouseDown(Button: TMouseButton;
			X, Y: Integer): Boolean; virtual; abstract;
		function MouseMove(X, Y: Integer): Boolean; virtual; abstract;
		function MouseUp(Button: TMouseButton;
			X, Y: Integer): Boolean; virtual; abstract;
	public
		constructor Create(inSurface: TDesignSurface); virtual;
		property DragRect: TRect read GetDragRect;
		property Shift: TShiftState read GetShift;
    property Surface: TDesignSurface read FSurface;
  end;
  //
  TDesignCustomControllerClass = class of TDesignCustomController;
  //
  TDesignHandleId = ( dhNone, dhLeftTop, dhMiddleTop, dhRightTop, dhLeftMiddle,
    dhRightMiddle, dhLeftBottom, dhMiddleBottom, dhRightBottom );
  //
  TDesignCustomSelector = class(TComponent)
  private
    FSurface: TDesignSurface;
  protected
    function GetCount: Integer; virtual; abstract;
    function GetSelection(inIndex: Integer): TControl;  virtual; abstract;
    procedure SetSelection(inIndex: Integer; inValue: TControl); virtual; abstract;
  public
    constructor Create(inSurface: TDesignSurface); reintroduce; virtual;
    destructor Destroy; override;
		function IsSelected(inValue: TControl): Boolean; virtual; abstract;
    function GetClientControl(inControl: TControl): TControl; virtual; abstract;
    function GetCursor(inX, inY: Integer): TCursor; virtual; abstract;
    function GetHitHandle(inX, inY: Integer): TDesignHandleId; virtual; abstract;
    procedure AddToSelection(inValue: TControl); virtual; abstract;
    procedure ClearSelection; virtual; abstract;
    procedure RemoveFromSelection(inValue: TControl); virtual; abstract;
    procedure ShowHideSelection(inShow: Boolean); virtual; abstract;
		procedure ToggleSelection(inValue: TControl);
		procedure Update; virtual; abstract;
		property Count: Integer read GetCount;
		property Selection[inIndex: Integer]: TControl read GetSelection
			write SetSelection;
		property Surface: TDesignSurface read FSurface;
	end;
	//
	TDesignCustomSelectorClass = class of TDesignCustomSelector;
	//
	TDesignObjectArray = array of TObject;
	TDesignGetAddClassEvent = procedure(Sender: TObject;
		var ioClass: string) of object;
	TDesignOnControlAddedEvent = procedure(Sender: TObject;
		inControl: TControl) of object;
	{
	TDesignOwnerDrawGridEvent = procedure(inSender: TObject; inCanvas: TCanvas;
		inRect: TRect) of object;
	}
	//
	TDesignSurface = class(TComponent)
	private
		FActive: Boolean;
		FAddClass: string;
		FContainer: TWinControl;
		FContainerHook: TDesignMessageHook;
		FController: TDesignCustomController;
		FControllerClass: TDesignCustomControllerClass;
		//FDrawGrid: Boolean;
		FMessenger: TDesignCustomMessenger;
		FMessengerClass: TDesignCustomMessengerClass;
		FOnControlAdded: TDesignOnControlAddedEvent;
		FOnChange: TNotifyEvent;
		FOnGetAddClass: TDesignGetAddClassEvent;
		//FOnOwnerDrawGrid: TDesignOwnerDrawGridEvent;
		FOnSelectionChange: TNotifyEvent;
		FSelector: TDesignCustomSelector;
		FSelectorClass: TDesignCustomSelectorClass;
		FUpdateOwner: TComponent;
	protected
		function GetAddBounds: TRect;
		function GetCount: Integer;
		function GetSelected: TDesignObjectArray;
		function GetSelectedContainer: TWinControl;
		function GetSelection(inIndex: Integer): TControl;
		procedure BeginUpdate;
		procedure EndUpdate;
		procedure NeedContainer;
		procedure NeedController;
		procedure NeedMessenger;
		procedure NeedSelector;
		//procedure PaintContainerBkgnd(inDC: HDC);
		procedure ReaderError(Reader: TReader; const Message: string;
			var Handled: Boolean);
		procedure SetActive(inValue: Boolean);
		procedure SetContainer(inValue: TWinControl);
		//procedure SetDrawGrid(const Value: Boolean);
		procedure SetSelection(inIndex: Integer; inValue: TControl);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		function Clear: TDesignSurface;
		function ContainerToSelectedContainer(const inPt: TPoint): TPoint;
		function FindControl(inX, inY: Integer): TControl; virtual;
    function GetCursor(inX, inY: Integer): TCursor; virtual;
    function GetHitHandle(inX, inY: Integer): TDesignHandleId; virtual;
    function IsDesignMessage(inSender: TControl; var inMsg: TMessage;
      const inPt: TPoint): Boolean;
    function LoadFromFile(const inFilename: string): TDesignSurface;
    function LoadFromStream(inStream: TStream): TDesignSurface;
    procedure AddComponent;
    procedure Change;
    procedure ClearSelection;
    procedure CopyComponents;
    procedure CutComponents;
		procedure DeleteComponents;
		procedure GetAddClass;
		procedure GrowComponents(inGrowWidth, inGrowHeight: Integer);
		procedure NudgeComponents(inNudgeLeft, inNudgeTop: Integer);
		procedure PasteComponents;
		procedure SaveToFile(const inFilename: string);
		procedure SaveToStream(inStream: TStream);
		procedure Select(inControl: TControl);
		procedure SelectionChange;
		procedure SelectParent;
		procedure SetSelected(const inValue: array of TObject);
		procedure UpdateDesigner; virtual;
		property Active: Boolean read FActive write SetActive;
		property AddClass: string read FAddClass write FAddClass;
		property Controller: TDesignCustomController read FController;
		property ControllerClass: TDesignCustomControllerClass read FControllerClass
			write FControllerClass;
		property Count: Integer read GetCount;
		property Messenger: TDesignCustomMessenger read FMessenger;
		property MessengerClass: TDesignCustomMessengerClass read FMessengerClass
			write FMessengerClass;
		property Selected: TDesignObjectArray read GetSelected;
		property SelectedContainer: TWinControl read GetSelectedContainer;
		property Selection[inIndex: Integer]: TControl read GetSelection
			write SetSelection;
		property Selector: TDesignCustomSelector read FSelector;
		property SelectorClass: TDesignCustomSelectorClass read FSelectorClass
			write FSelectorClass;
	published
		property Container: TWinControl read FContainer write SetContainer;
		//property DrawGrid: Boolean read FDrawGrid write SetDrawGrid default true;
		property OnControlAdded: TDesignOnControlAddedEvent
			read FOnControlAdded write FOnControlAdded;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
		property OnGetAddClass: TDesignGetAddClassEvent read FOnGetAddClass
			write FOnGetAddClass;
		{
		property OnOwnerDrawGrid: TDesignOwnerDrawGridEvent
			read FOnOwnerDrawGrid write FOnOwnerDrawGrid;
		}
		property OnSelectionChange: TNotifyEvent read FOnSelectionChange
			write FOnSelectionChange;
	end;
	//
	TDesignPanel = class(TPanel)
	private
		FSurface: TDesignSurface;
		FOnPaint: TNotifyEvent;
		FDrawRules: Boolean;
		function GetActive: Boolean;
		function GetOnChange: TNotifyEvent;
		function GetOnGetAddClass: TDesignGetAddClassEvent;
		function GetOnSelectionChange: TNotifyEvent;
		procedure SetActive(const Value: Boolean);
		procedure SetOnChange(const Value: TNotifyEvent);
		procedure SetOnGetAddClass(const Value: TDesignGetAddClassEvent);
		procedure SetOnSelectionChange(const Value: TNotifyEvent);
	public
		constructor Create(inOwner: TComponent); override;
		procedure Clear;
		procedure LoadFromFile(const inFilename: string);
		procedure LoadFromStream(inStream: TStream);
		procedure Paint; override;
		procedure SaveToFile(const inFilename: string);
		procedure SaveToStream(inStream: TStream);
		procedure SetDrawRules(const Value: Boolean);
		property Active: Boolean read GetActive write SetActive;
		property Canvas;
		property Surface: TDesignSurface read FSurface;
	published
		property DrawRules: Boolean read FDrawRules write SetDrawRules default true;
		property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
		property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
		property OnGetAddClass: TDesignGetAddClassEvent read GetOnGetAddClass
			write SetOnGetAddClass;
		property OnSelectionChange: TNotifyEvent read GetOnSelectionChange
			write SetOnSelectionChange;
	end;
	//
	TDesignScrollBox = class(TScrollBox)
	protected
		procedure AutoScrollInView(AControl: TControl); override;
	end;

implementation

uses
  Clipbrd, DesignUtils, DesignClip, DesignImp;

{ TDesignCustomMessenger }

constructor TDesignCustomMessenger.Create;
begin
  //
end;

destructor TDesignCustomMessenger.Destroy;
begin
  //
end;

procedure TDesignCustomMessenger.Clear;
begin
  //
end;

procedure TDesignCustomMessenger.DesignComponent(inComponent: TComponent);
begin
  //
end;

procedure TDesignCustomMessenger.DesignChildren(inContainer: TWinControl);
var
  i: Integer;
begin
  for i := 0 to Pred(inContainer.ControlCount) do
    DesignComponent(inContainer.Controls[i]);
end;

procedure TDesignCustomMessenger.SetContainer(inValue: TWinControl);
begin
  FContainer := inValue;
end;

function TDesignCustomMessenger.IsDesignMessage(inSender: TControl;
  var inMessage: TMessage): Boolean;

  function MousePoint: TPoint;
  begin
    with TWMMouse(inMessage) do
      MousePoint := Point(XPos, YPos);
    Result := DesignClientToParent(Result, inSender, Container);
  end;

begin
  if not Assigned(OnDesignMessage) then
    Result := false
  else
    case inMessage.Msg of
      WM_MOUSEFIRST..WM_MOUSELAST:
        Result := OnDesignMessage(inSender, inMessage, MousePoint);
      WM_KEYDOWN, WM_KEYUP, WM_PAINT, WM_ERASEBKGND:
				Result := OnDesignMessage(inSender, inMessage, Point(0, 0));
			CN_KEYDOWN, CN_KEYUP:
				Result := true;
      else Result := false;
		end;
	//inMessage.Result := Ord(Result);
end;

{ TDesignMessageHook }

constructor TDesignMessageHook.Create(inUser: TDesignCustomMessenger;
  inClient: TWinControl);
begin
  FUser := inUser;
  FClient := inClient;
  FOldProc := FClient.WindowProc;
  FClient.WindowProc := HookProc;
end;

destructor TDesignMessageHook.Destroy;
begin
  Unhook;
  inherited;
end;

procedure TDesignMessageHook.Unhook;
begin
  FClient.WindowProc := FOldProc;
end;

procedure TDesignMessageHook.HookProc(var inMessage: TMessage);
begin
  if not FUser.IsDesignMessage(FClient, inMessage) then
    FOldProc(inMessage);
end;

{ TDesignCustomController }

constructor TDesignCustomController.Create(inSurface: TDesignSurface);
begin
  FSurface := inSurface;
end;

function TDesignCustomController.GetShift: TShiftState;
begin
  Result := KeyboardStateToShiftState;
end;

{ TDesignCustomSelector }

constructor TDesignCustomSelector.Create(inSurface: TDesignSurface);
begin
  inherited Create(nil);
  FSurface := inSurface;
end;

destructor TDesignCustomSelector.Destroy;
begin
  inherited;
end;

procedure TDesignCustomSelector.ToggleSelection(inValue: TControl);
begin
  if IsSelected(inValue) then
    RemoveFromSelection(inValue)
  else
		AddToSelection(inValue);
end;

{ TDesignSurface }

constructor TDesignSurface.Create(AOwner: TComponent);
begin
  inherited;
  FMessengerClass := TDesignDesignerMessenger;
  FControllerClass := TDesignController;
  FSelectorClass := TDesignSelector;
	//FDrawGrid := true;
end;

destructor TDesignSurface.Destroy;
begin
  FContainerHook.Free;
  Messenger.Free;
  Controller.Free;
  Selector.Free;
  inherited;
end;

procedure TDesignSurface.Change;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TDesignSurface.SetContainer(inValue: TWinControl);
begin
  FContainer := inValue;
end;

procedure TDesignSurface.NeedContainer;
begin
  if (Container = nil) and (Owner is TWinControl) then
    Container := TWinControl(Owner);
  if Container = nil then
    raise Exception.Create(ClassName + ': Container is nil');
end;

procedure TDesignSurface.NeedController;
begin
  if (Controller = nil) and (ControllerClass <> nil) then
    FController := ControllerClass.Create(Self);
  if Controller = nil then
    raise Exception.Create(ClassName + ': Controller is nil');
end;

procedure TDesignSurface.NeedMessenger;
begin
  if (Messenger = nil) and (MessengerClass <> nil) then
  begin
    FMessenger := MessengerClass.Create;
    Messenger.OnDesignMessage := IsDesignMessage;
  end;
	if Messenger = nil then
    raise Exception.Create(ClassName + ': Messenger is nil');
end;

procedure TDesignSurface.NeedSelector;
begin
  if (Selector = nil) and (SelectorClass <> nil) then
    FSelector := SelectorClass.Create(Self);
  if Selector = nil then
    raise Exception.Create(ClassName + ': Selector is nil');
end;

procedure TDesignSurface.SetActive(inValue: Boolean);

  procedure Activate;
  begin
    NeedContainer;
    NeedController;
    NeedSelector;
    NeedMessenger;
    Messenger.Container := Container;
    FContainerHook := TDesignMessageHook.Create(Messenger, Container);
  end;

  procedure Deactivate;
  begin
    FreeAndNil(FContainerHook);
    Selector.ClearSelection;
    FreeAndNil(FMessenger);
  end;

begin
  if FActive <> inValue then
  begin
    if inValue then
      Activate
    else
      Deactivate;
    FActive := inValue;
		SelectionChange;
	end;
end;

procedure TDesignSurface.UpdateDesigner;
begin
  Selector.Update;
end;

function TDesignSurface.GetCount: Integer;
begin
  Result := Selector.Count;
end;

function TDesignSurface.GetSelection(inIndex: Integer): TControl;
begin
  Result := Selector.Selection[inIndex];
end;

procedure TDesignSurface.SetSelection(inIndex: Integer; inValue: TControl);
begin
	Selector.Selection[inIndex] := inValue;
end;

procedure TDesignSurface.ClearSelection;
begin
  Selector.ClearSelection;
end;

procedure TDesignSurface.SelectionChange;
begin
  if not (csDestroying in ComponentState) and Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

function TDesignSurface.GetSelected: TDesignObjectArray;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Pred(Count) do
    Result[i] := Selector.Selection[i];
end;

procedure TDesignSurface.SetSelected(const inValue: array of TObject);
var
  i: Integer;
begin
  ClearSelection;
  for i := 0 to Pred(Length(inValue)) do
    if inValue[i] is TControl then
      Selector.AddToSelection(TControl(inValue[i]));
end;

procedure TDesignSurface.Select(inControl: TControl);
begin
	ClearSelection;
	if (inControl <> nil) and (inControl <> Container) then
		Selector.AddToSelection(inControl);
end;

function TDesignSurface.FindControl(inX, inY: Integer): TControl;
var
  c, c0: TControl;
  p: TPoint;
begin
  p := Point(inX, inY);
  c := Container.ControlAtPos(p, true, true);
  while (c <> nil) and (c is TWinControl) do
  begin
    Dec(p.X, c.Left);
    Dec(p.Y, c.Top);
    c0 := TWinControl(c).ControlAtPos(p, true, true);
    if (c0 = nil) or (c0.Owner <> c.Owner) then
      break;
    c := c0;
  end;
  if c = nil then
    c := Container;
  Result := Selector.GetClientControl(c);
end;

function TDesignSurface.GetSelectedContainer: TWinControl;
begin
  if (Count <> 1) then
    Result := Container
  else if (Selection[0] is TWinControl) and
    (csAcceptsControls in Selection[0].ControlStyle) then
      Result := TWinControl(Selection[0])
  else
    Result := Selection[0].Parent;
end;

function TDesignSurface.ContainerToSelectedContainer(
  const inPt: TPoint): TPoint;
var
  c: TControl;
begin
  Result := inPt;
	c := SelectedContainer;
  while (c <> Container) and (c <> nil) do
  begin
    Dec(Result.X, c.Left);
    Dec(Result.Y, c.Top);
    c := c.Parent;
  end;
end;

function TDesignSurface.GetAddBounds: TRect;
begin
  with Result, Controller do
  begin
    TopLeft := ContainerToSelectedContainer(DragRect.TopLeft);
    BottomRight := ContainerToSelectedContainer(DragRect.BottomRight);
  end;
end;

procedure TDesignSurface.GetAddClass;
begin
  if Assigned(OnGetAddClass) then
    OnGetAddClass(Self, FAddClass);
end;

procedure TDesignSurface.AddComponent;
var
  cc: TComponentClass;
  c: TComponent;
  co: TControl;

  function GetBounds: TRect;
  begin
    Result := GetAddBounds;
    if DesignRectWidth(Result) = 0 then
      Result.Right := Result.Left + co.Width;
    if DesignRectHeight(Result) = 0 then
      Result.Bottom := Result.Top + co.Height;
  end;

begin
  cc := TComponentClass(GetClass(AddClass));
  if (cc <> nil) and (SelectedContainer <> nil) then
  begin
    c := cc.Create(Container);
    c.Name := DesignUniqueName(Container, AddClass);
    if (c is TControl) then
		begin
			co := TControl(c);
			co.Parent := SelectedContainer;
			co.BoundsRect := GetBounds;
			if Assigned(OnControlAdded) then
				OnControlAdded(Self, co);
			Select(co);
		end;
		Messenger.DesignComponent(c);
		Change;
		SelectionChange;
		AddClass := '';
  end;
end;

procedure TDesignSurface.NudgeComponents(inNudgeLeft, inNudgeTop: Integer);
var
  i: Integer;
begin
  for i := 0 to Pred(Count) do
    with Selection[i] do
    begin
      Left := Left + inNudgeLeft;
      Top := Top + inNudgeTop;
    end;
	Change;
	SelectionChange;
end;

procedure TDesignSurface.GrowComponents(inGrowWidth, inGrowHeight: Integer);
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		with Selection[i] do
		begin
			Width := DesignMax(1, Width + inGrowWidth);
			Height := DesignMax(1, Height + inGrowHeight);
		end;
	Change;
	SelectionChange;
end;

procedure TDesignSurface.DeleteComponents;
var
  i: Integer;
begin
  if Count > 0 then
  begin
    for i := 0 to Pred(Count) do
      Selection[i].Free;
    ClearSelection;
		Change;
		SelectionChange;
	end;
end;

procedure TDesignSurface.CopyComponents;
var
  i: Integer;
begin
	if Count > 0 then
		with TDesignComponentClipboardWriter.Create do
		try
			for i := 0 to Pred(Count) do
				SetComponent(Selection[i]);
		finally
			Free;
		end;
end;

procedure TDesignSurface.CutComponents;
begin
  CopyComponents;
  DeleteComponents;
end;

procedure TDesignSurface.PasteComponents;
var
  co: TControl;
  c: TComponent;
  p: TWinControl;

  procedure KeepInParent;
  begin
    with p do
    begin
      if co.Left > ClientWidth then
        co.Left := ClientWidth - co.Width;
      if co.Top > ClientHeight then
        co.Top := ClientHeight - co.Height;
    end;
  end;

  procedure PasteComponent;
  begin
    c.Name := DesignUniqueName(Owner, c.ClassName);
    Container.InsertComponent(c);
		if c is TControl then
		begin
			co := TControl(c);
			KeepInParent;
			co.Parent := p;
			Selector.AddToSelection(co);
		end;
		Messenger.DesignComponent(c);
  end;

begin
	with TDesignComponentClipboardReader.Create do
	try
		c := GetComponent;
		if (c <> nil) then
		begin
			p := SelectedContainer;
			ClearSelection;
			repeat
				PasteComponent;
				c := GetComponent;
			until (c = nil);
			Change;
			SelectionChange;
		end;
	finally
		Free;
	end;
end;

procedure TDesignSurface.SelectParent;
begin
	if (Count > 0) then
	begin
		Select(Selection[0].Parent);
		SelectionChange;
	end;
end;

{
procedure TDesignSurface.PaintContainerBkgnd(inDC: HDC);
var
  r: TRect;
  canvas: TCanvas;
begin
  if DrawGrid then
  begin
    canvas := TCanvas.Create;
    try
      SelectClipRgn(inDC, 0);
      canvas.Handle := inDC;
      canvas.Brush.Color := Container.Brush.Color;
      r := canvas.ClipRect;
      if Assigned(OnOwnerDrawGrid) then
        OnOwnerDrawGrid(Self, canvas, Container.ClientRect)
      else begin
        canvas.FillRect(Container.ClientRect);
        DesignPaintRules(canvas, Container.ClientRect);
      end;
    finally
      canvas.Free;
    end;
  end;
end;
}

function TDesignSurface.IsDesignMessage(inSender: TControl;
  var inMsg: TMessage; const inPt: TPoint): Boolean;

  function VirtKey: Cardinal;
  begin
    Result := inMsg.WParam;
  end;

{
  function HandlePaint: Boolean;
  begin
    Result := false;
  end;

  function HandleEraseBkgnd: Boolean;
  begin
    if (inSender <> Container) then
      Result := false
    else begin
       PaintContainerBkgnd(TWMPaint(inMsg).DC);
       inMsg.Result := 1;
       Result := true;
    end;
  end;
}

begin
  if not Active then
    Result := false
  else
    case inMsg.Msg of
{
      WM_ERASEBKGND: Result := HandleEraseBkgnd;
      WM_PAINT: Result := HandlePaint;
}
			WM_LBUTTONDOWN: Result := Controller.MouseDown(mbLeft, inPt.X, inPt.Y);
			WM_LBUTTONUP: Result := Controller.MouseUp(mbLeft, inPt.X, inPt.Y);
			WM_MOUSEMOVE: Result := Controller.MouseMove(inPt.X, inPt.Y);
			WM_KEYDOWN, CN_KEYDOWN: Result := Controller.KeyDown(VirtKey);
			WM_KEYUP, CN_KEYUP: Result := Controller.KeyUp(VirtKey);
			else Result := false;
		end;
end;

function TDesignSurface.GetCursor(inX, inY: Integer): TCursor;
begin
	// Using FindControl is inefficient.
	// All we really want to know is if Selected[0] contains (inX, inY)
	if (Count > 0) and (FindControl(inX, inY) = Selected[0]) then
		Result := Selector.GetCursor(inX, inY)
	else
		Result := crDefault;
end;

function TDesignSurface.GetHitHandle(inX, inY: Integer): TDesignHandleId;
begin
	Result := Selector.GetHitHandle(inX, inY);
end;

procedure TDesignSurface.BeginUpdate;
begin
	NeedContainer;
  Active := false;
  FUpdateOwner := Owner;
  Owner.RemoveComponent(Self);
end;

procedure TDesignSurface.EndUpdate;
begin
  FUpdateOwner.InsertComponent(Self);
  Active := true;
end;

procedure TDesignSurface.ReaderError(Reader: TReader; const Message: string;
  var Handled: Boolean);
begin
  Handled := true;
end;

function TDesignSurface.Clear: TDesignSurface;
begin
	BeginUpdate;
	Container.DestroyComponents;
	EndUpdate;
	Result := Self;
end;

procedure TDesignSurface.SaveToStream(inStream: TStream);
begin
	BeginUpdate;
	DesignSaveComponentToStream(Container, inStream);
	EndUpdate;
end;

function TDesignSurface.LoadFromStream(inStream: TStream): TDesignSurface;
begin
	BeginUpdate;
	Container.DestroyComponents;
	DesignLoadComponentFromStream(Container, inStream, ReaderError);
	EndUpdate;
	Result := Self;
end;

procedure TDesignSurface.SaveToFile(const inFilename: string);
begin
	BeginUpdate;
	DesignSaveComponentToFile(Container, inFilename);
	EndUpdate;
end;

function TDesignSurface.LoadFromFile(
	const inFilename: string): TDesignSurface;
begin
	BeginUpdate;
	Container.DestroyComponents;
	DesignLoadComponentFromFile(Container, inFilename, ReaderError);
	EndUpdate;
	Result := Self;
end;

{
procedure TDesignSurface.SetDrawGrid(const Value: Boolean);
begin
	FDrawGrid := Value;
  if Active then
    Container.Invalidate;
end;
}

{ TDesignPanel }

constructor TDesignPanel.Create(inOwner: TComponent);
begin
  inherited;
	FDrawRules := true;
  FSurface := TDesignSurface.Create(Self);
  Surface.Name := 'Surface';
  Surface.Container := Self;
end;

procedure TDesignPanel.SetDrawRules(const Value: Boolean);
begin
  FDrawRules := Value;
  Invalidate;
end;

procedure TDesignPanel.Paint;
begin
  inherited;
  if Surface.Active or (csDesigning in ComponentState) then
  begin
    if DrawRules then
      DesignPaintRules(Canvas, ClientRect);
    if Assigned(OnPaint) then
      OnPaint(Self);
  end;
end;

procedure TDesignPanel.Clear;
begin
  // DesignSurface property value is lost on clear.
  // Restore it with the value returned from Clear.
  FSurface := Surface.Clear;
end;

procedure TDesignPanel.SaveToStream(inStream: TStream);
begin
  Surface.SaveToStream(inStream);
end;

procedure TDesignPanel.LoadFromStream(inStream: TStream);
begin
  // DesignSurface property value is lost on load.
  // Restore it with the value returned from LoadFromStream.
  FSurface := Surface.LoadFromStream(inStream);
end;

procedure TDesignPanel.SaveToFile(const inFilename: string);
begin
  Surface.SaveToFile(inFilename);
end;

procedure TDesignPanel.LoadFromFile(const inFilename: string);
begin
  // DesignSurface property value is lost on load.
  // Restore it with the value returned from LoadFromFile.
  FSurface := Surface.LoadFromFile(inFilename);
end;

function TDesignPanel.GetActive: Boolean;
begin
  Result := Surface.Active;
end;

function TDesignPanel.GetOnChange: TNotifyEvent;
begin
  Result := Surface.OnChange;
end;

function TDesignPanel.GetOnGetAddClass: TDesignGetAddClassEvent;
begin
  Result := Surface.OnGetAddClass;
end;

function TDesignPanel.GetOnSelectionChange: TNotifyEvent;
begin
  Result := Surface.OnSelectionChange;
end;

procedure TDesignPanel.SetActive(const Value: Boolean);
begin
  Surface.Active := Active;
end;

procedure TDesignPanel.SetOnChange(const Value: TNotifyEvent);
begin
  Surface.OnChange := Value;
end;

procedure TDesignPanel.SetOnGetAddClass(const Value: TDesignGetAddClassEvent);
begin
  Surface.OnGetAddClass := Value;
end;

procedure TDesignPanel.SetOnSelectionChange(const Value: TNotifyEvent);
begin
	Surface.OnSelectionChange := Value;
end;

{ TDesignScrollBox }

procedure TDesignScrollBox.AutoScrollInView(AControl: TControl);
begin
  //
end;

end.
