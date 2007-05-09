unit DesignController;

interface

uses
	Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, ExtCtrls,
	DesignHandles;

type
	TDesignController = class(TComponent)
	private
		FActive: Boolean;
		FContainer: TWinControl;
		FHandles: TDesignHandles;
		FHintWin: THintWindow;
		FMouseIsDown: Boolean;
		FMouseOffset: TPoint;
		FMouseLast: TPoint;
		FFullDrag: Boolean;
	protected
		function FindControl(inX, inY: Integer): TControl;
		procedure ApplicationMessage(var Msg: tagMSG; var Handled: Boolean);
		procedure DragControl(X, Y: Integer);
		procedure DragOutline(X, Y: Integer);
		procedure MouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure MouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer);
		procedure MouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure SetActive(const Value: Boolean);
		procedure SetContainer(const Value: TWinControl);
		procedure SetFullDrag(const Value: Boolean);
		procedure UpdateHint;
	public
		constructor Create(AOwner: TComponent); override;
		property Active: Boolean read FActive write SetActive;
		property Container: TWinControl read FContainer write SetContainer;
		property FullDrag: Boolean read FFullDrag write SetFullDrag;
	end;

implementation

type
	TControlCracker = class(TControl);

procedure SimplifyMessage(var ioMsg: TMessage;
	inHandler: TWinControl); overload;
var
	ctrlMsgProc: TWndMethod;
begin
	TMethod(ctrlMsgProc).code := @TControlCracker.WndProc;
	TMethod(ctrlMsgProc).data := inHandler;
	ctrlMsgProc(ioMsg);
end;

procedure SimplifyMessage(var ioMsg: TMsg; inHandler: TWinControl); overload;
var
	m: TMessage;
begin
	m.Msg := ioMsg.message;
	m.WParam := ioMsg.wParam;
	m.LParam := ioMsg.lParam;
	SimplifyMessage(m, inHandler);
end;

function FindHandle(inHandle: HWND; inContainer: TWinControl): TWinControl;
var
	i: Integer;
begin
	Result := nil;
	with inContainer do
		if Handle = inHandle then
			Result := inContainer
		else
			for i := 0 to Pred(ControlCount) do
				if (inContainer.Controls[i] is TWinControl) then
				begin
					Result := FindHandle(inHandle, TWinControl(Controls[i]));
					if Result <> nil then
						break;
				end;
end;

{ TDesignController }

constructor TDesignController.Create(AOwner: TComponent);
begin
	inherited;
	FHandles := TDesignHandles.Create(Self);
	FHintWin := HintWindowClass.Create(Self);
	FHintWin.Brush.Color := clInfoBk;
end;

procedure TDesignController.SetContainer(const Value: TWinControl);
begin
	if FContainer <> nil then
		with TPanel(FContainer) do
		begin
			OnMouseDown := nil;
			OnMouseMove := nil;
			OnMouseUp := nil;
		end;
	FContainer := Value;
	FHandles.Container := Value;
	if FContainer <> nil then
		with TPanel(FContainer) do
		begin
			OnMouseDown := MouseDown;
			OnMouseMove := MouseMove;
			OnMouseUp := MouseUp;
		end;
end;

procedure TDesignController.SetActive(const Value: Boolean);
begin
	FActive := Value;
	if FActive then
		Application.OnMessage := ApplicationMessage
	else
		Application.OnMessage := nil;
end;

procedure TDesignController.ApplicationMessage(var Msg: tagMSG;
	var Handled: Boolean);
begin
	case Msg.message of
		WM_MOUSEFIRST..WM_MOUSELAST:
		begin
			if FindHandle(Msg.hwnd, Container) <> nil then
			begin
				Handled := true;
				with Container.ScreenToClient(Msg.pt) do
					msg.lParam := X + Y shl 16;
				SimplifyMessage(msg, Container);
			end;
		end;
	end;
end;

function TDesignController.FindControl(inX, inY: Integer): TControl;
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
	if c = Container then
		c := nil;
	Result := c;
end;

procedure TDesignController.MouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	FHandles.Selected := FindControl(X, Y);
	Container.Update;
	FMouseIsDown := FHandles.Selected <> nil;
	if FHandles.Selected <> nil then
	begin
		FMouseOffset :=
			Point(X - FHandles.Selected.Left, Y - FHandles.Selected.Top);
		FHandles.Selected.Parent.DisableAlign;
		if not FullDrag then
		begin
			FMouseLast := FHandles.Selected.BoundsRect.topLeft;
			FHandles.PaintHandles(TCustomForm(Container.Parent).Canvas, FMouseLast);
		end;
	end;
end;

procedure TDesignController.MouseMove(Sender: TObject; Shift: TShiftState;
	X, Y: Integer);
var
	l, t: Integer;
begin
	if FMouseIsDown then
	begin
		l := X - FMouseOffset.X;
		l := l - (l and 3);
		t := Y - FMouseOffset.Y;
		t := t - (t and 3);
		if FullDrag then
			DragControl(l, t)
		else
			DragOutline(l, t);
	end;
end;

procedure TDesignController.DragControl(X, Y: Integer);
var
	r: TRect;
begin
	with FHandles.Selected do
		if (X <> Left) or (Y <> Top) then
		begin
			r := BoundsRect;
			BoundsRect := Rect(X, Y, X + Width, Y + Height);
			if not EqualRect(BoundsRect, r) then
			begin
				Parent.Update;
				UpdateHint;
				FHandles.UpdateHandles;
				Update;
			end;
	end;
end;

procedure TDesignController.DragOutline(X, Y: Integer);
begin
	with FHandles.Selected do
		if (X <> Left) or (Y <> Top) then
		begin
			UpdateHint;
			FHandles.PaintHandles(TCustomForm(Container.Parent).Canvas, FMouseLast);
			FMouseLast := Point(X, Y);
			FHandles.PaintHandles(TCustomForm(Container.Parent).Canvas, FMouseLast);
		end;
end;

procedure TDesignController.UpdateHint;
var
	h: string;
begin
	with FHandles.Selected do
	begin
		h := Format('%d, %d', [ Left, Top ]);
		with Mouse.CursorPos, FHintWin.Canvas do
			FHintWin.ActivateHint(
				Rect(X + 12, Y + 20, X + TextWidth(h) + 20, Y + TextHeight(h) + 20),
					h);
			FHintWin.Update;
	end;
end;

procedure TDesignController.MouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	FMouseIsDown := false;
	FHintWin.ReleaseHandle;
	if FHandles.Selected <> nil then
	begin
		if not FullDrag then
		begin
			FHandles.PaintHandles(TCustomForm(Container.Parent).Canvas, FMouseLast);
			with FHandles.Selected, FMouseLast do
				BoundsRect := Rect(X, Y, X + Width, Y + Height);
		end;
		FHandles.Selected.Parent.EnableAlign;
		FHandles.UpdateHandles;
	end;
end;

procedure TDesignController.SetFullDrag(const Value: Boolean);
begin
	FFullDrag := Value;
end;

end.
