unit DesignTest2;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls, StdCtrls, AppEvnts, ComCtrls,
	DesignHandles, ToolWin;

type
	TDesignController = class(TComponent)
	private
		FActive: Boolean;
		FContainer: TWinControl;
		FHandles: TDesignHandles;
		FMouseIsDown: Boolean;
		FMouseOffset: TPoint;
	protected
		function FindControl(inX, inY: Integer): TControl;
		procedure ApplicationMessage(var Msg: tagMSG; var Handled: Boolean);
		procedure MouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure MouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer);
		procedure MouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure SetActive(const Value: Boolean);
		procedure SetContainer(const Value: TWinControl);
	public
		constructor Create(AOwner: TComponent); override;
		property Active: Boolean read FActive write SetActive;
		property Container: TWinControl read FContainer write SetContainer;
	end;
	//
	TDesignForm2 = class(TForm)
		ApplicationEvents1: TApplicationEvents;
		StatusBar1: TStatusBar;
		Panel2: TPanel;
		PageControl1: TPageControl;
		TabSheet1: TTabSheet;
		TabSheet2: TTabSheet;
		Button1: TButton;
		Panel1: TPanel;
		Label2: TLabel;
		Edit1: TEdit;
		Label1: TLabel;
		ToolBar1: TToolBar;
		procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer);
		procedure FormCreate(Sender: TObject);
		procedure ApplicationEvents1Message(var Msg: tagMSG;
			var Handled: Boolean);
		procedure FormActivate(Sender: TObject);
		procedure FormDeactivate(Sender: TObject);
		procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
	private
		{ Private declarations }
	protected
		FMouseIsDown: Boolean;
		FMouseOffset: TPoint;
		function FindControl(inX, inY: Integer): TControl;
		function FindHandle(inHandle: HWND;
			inContainer: TWinControl): TWinControl;
	public
		{ Public declarations }
		Handles: TDesignHandles;
		HintWin: THintWindow;
	end;

var
	DesignForm2: TDesignForm2;

implementation

{$R *.dfm}

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
		//p := c.ParentToClient(p);
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
	end;
end;

procedure TDesignController.MouseMove(Sender: TObject; Shift: TShiftState;
	X, Y: Integer);
var
	l, t: Integer;
	h: string;
	r: TRect;
begin
	if FMouseIsDown then
	begin
		with FHandles.Selected do
		begin
			l := X - FMouseOffset.X;
			l := l - (l and 3);
			t := Y - FMouseOffset.Y;
			t := t - (t and 3);
			if (l <> Left) or (t <> Top) then
			begin
				r := BoundsRect;
				BoundsRect := Rect(l, t, l + Width, t + Height);
				if not EqualRect(BoundsRect, r) then
				begin
					Parent.Update;
					h := Format('%d, %d', [ Left, Top ]);
{
					//StatusBar1.SimpleText := h;
					//StatusBar1.Update;
					with Mouse.CursorPos, HintWin.Canvas do
						HintWin.ActivateHint(
							Rect(X + 12, Y + 20, X + TextWidth(h) + 20, Y + TextHeight(h) + 20), h);
					HintWin.Update;
}
					FHandles.UpdateHandles;
					Update;
				end;
			end;
		end;
	end;
end;

procedure TDesignController.MouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	FMouseIsDown := false;
//	HintWin.ReleaseHandle;
	if FHandles.Selected <> nil then
	begin
		FHandles.Selected.Parent.EnableAlign;
		FHandles.UpdateHandles;
	end;
end;

{ TDesignForm2 }

procedure TDesignForm2.FormCreate(Sender: TObject);
begin
	Handles := TDesignHandles.Create(Self);
	Handles.Container := Panel2;
	Handles.Selected := Label1;
	HintWin := HintWindowClass.Create(Self); //THintWindow.Create(Self);
	HintWin.Brush.Color := clInfoBk;
	with TDesignController.Create(Self) do
	begin
		Container := Panel2;
		Active := true;
	end;
	Show;
end;

procedure TDesignForm2.ApplicationEvents1Message(var Msg: tagMSG;
	var Handled: Boolean);
begin
	case Msg.message of
		WM_MOUSEFIRST..WM_MOUSELAST:
		begin
			if FindHandle(Msg.hwnd, Panel2) <> nil then
			begin
				Handled := true;
				with {Panel2.}ScreenToClient(Msg.pt) do
					msg.lParam := X + Y shl 16;
				SimplifyMessage(msg, Self);
			end;
		end;
	end;
end;

function TDesignForm2.FindHandle(inHandle: HWND;
	inContainer: TWinControl): TWinControl;
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

procedure TDesignForm2.FormActivate(Sender: TObject);
begin
	//ApplicationEvents1.OnMessage :=	ApplicationEvents1Message;
end;

procedure TDesignForm2.FormDeactivate(Sender: TObject);
begin
	//ApplicationEvents1.OnMessage :=	nil;
end;

function TDesignForm2.FindControl(inX, inY: Integer): TControl;
var
	c, c0: TControl;
	p: TPoint;
begin
	p := Point(inX, inY);
	c := ControlAtPos(p, true, true);
	while (c <> nil) and (c is TWinControl) do
	begin
		Dec(p.X, c.Left);
		Dec(p.Y, c.Top);
		//p := c.ParentToClient(p);
		c0 := TWinControl(c).ControlAtPos(p, true, true);
		if (c0 = nil) or (c0.Owner <> c.Owner) then
			break;
		c := c0;
	end;
	if c = Panel2 then
		c := nil;
	Result := c;
end;

procedure TDesignForm2.FormMouseDown(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	Handles.Selected := FindControl(X, Y);
	Update;
	FMouseIsDown := Handles.Selected <> nil;
	if Handles.Selected <> nil then
	begin
		FMouseOffset := Point(X - Handles.Selected.Left, Y - Handles.Selected.Top);
		Handles.Selected.Parent.DisableAlign;
	end;
end;

procedure TDesignForm2.FormMouseMove(Sender: TObject; Shift: TShiftState;
	X, Y: Integer);
var
	l, t: Integer;
	h: string;
	r: TRect;
begin
	if FMouseIsDown then
	begin
		with Handles.Selected do
		begin
			l := X - FMouseOffset.X;
			l := l - (l and 3);
			t := Y - FMouseOffset.Y;
			t := t - (t and 3);
			if (l <> Left) or (t <> Top) then
			begin
				r := BoundsRect;
				BoundsRect := Rect(l, t, l + Width, t + Height);
				if not EqualRect(BoundsRect, r) then
				begin
					Parent.Update;
					h := Format('%d, %d', [ Left, Top ]);
					//StatusBar1.SimpleText := h;
					//StatusBar1.Update;
					with Mouse.CursorPos, HintWin.Canvas do
						HintWin.ActivateHint(
							Rect(X + 12, Y + 20, X + TextWidth(h) + 20, Y + TextHeight(h) + 20), h);
					HintWin.Update;
					Handles.UpdateHandles;
					Update;
				end;
			end;
		end;
	end;
end;

procedure TDesignForm2.FormMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	FMouseIsDown := false;
	HintWin.ReleaseHandle;
	if Handles.Selected <> nil then
	begin
		Handles.Selected.Parent.EnableAlign;
		Handles.UpdateHandles;
	end;
end;

end.
