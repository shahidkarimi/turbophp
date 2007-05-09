unit DesignTest;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls;

type
	TDesignHandle = class(TCustomControl)
	protected
		FHitRect: Integer;
		FMouseIsDown: Boolean;
		FMouseOffset: TPoint;
	protected
		function HandleRect(inIndex: Integer): TRect;
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure Paint; override;
	end;
	//
	TDesignPanel = class(TCustomControl)
	protected
		FMouseIsDown: Boolean;
		FMouseButton: TMouseButton;
		FMouseOffset: TPoint;
	protected
		procedure AdjustClientRect(var Rect: TRect); override;
		procedure DoEnter; override;
		procedure DoExit; override;
		procedure DrawFocusRect;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure Paint; override;
		procedure WndProc(var Message: TMessage); override;
	public
		constructor Create(inOwner: TComponent); override;
	end;
	//
	TDesignTestForm = class(TForm)
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
		Handles: array[0..3] of TDesignHandle;
		Selected: TControl;
		procedure UpdateHandles;
	end;

var
	DesignTestForm: TDesignTestForm;

implementation

{$R *.dfm}

const
	S = 6;
	Q = 8;

var
	ShadedBits: TBitmap;

function NeedShadedBits: TBitmap;
begin
	if ShadedBits = nil then
	begin
		//ShadedBits := AllocPatternBitmap(clSilver, clGray);
		ShadedBits := TBitmap.Create;
		ShadedBits.Width := 4;
		ShadedBits.Height := 2;
		ShadedBits.Canvas.Pixels[0, 0] := clGray;
		ShadedBits.Canvas.Pixels[1, 0] := clBtnFace;
		ShadedBits.Canvas.Pixels[2, 0] := clBtnFace;
		ShadedBits.Canvas.Pixels[3, 0] := clBtnFace;
		ShadedBits.Canvas.Pixels[0, 1] := clBtnFace;
		ShadedBits.Canvas.Pixels[1, 1] := clBtnFace;
		ShadedBits.Canvas.Pixels[2, 1] := clGray;
		ShadedBits.Canvas.Pixels[3, 1] := clBtnFace;
	end;
	Result := ShadedBits;
end;

procedure DrawBitmapBrushFrame(inRect: TRect; inCanvas: TCanvas);
begin
	with inCanvas do
	begin
		Brush.Bitmap := NeedShadedBits;
		with inRect do
		begin
			FillRect(Rect(Left, Top, Left, Top + S));
			FillRect(Rect(Left, Top, Left + S, Bottom));
			FillRect(Rect(Right-S, Top, Right, Bottom));
			FillRect(Rect(Left, Bottom - S, Left, Bottom));
		end;
	end;
end;

{ TDesignHandle }

function TDesignHandle.HandleRect(inIndex: Integer): TRect;
begin
	case inIndex of
		0: Result := Rect(0, 0, Q, Q);
		1: Result := Rect((Width - Q) div 2, 0, (Width + Q) div 2, Q);
		2: Result := Rect(Width - Q, 0, Width, Q);
		3: Result := Rect(0, (Height - Q) div 2, Q, (Height + Q) div 2);
	end;
end;

procedure TDesignHandle.Paint;
begin
	inherited;
	with Canvas do
	begin
		Brush.Bitmap := NeedShadedBits;
		FillRect(ClientRect);
		Brush.Bitmap := nil;
		Brush.Color := clWhite;
		Pen.Color := clBlack;
		if (Width > Height) then
		begin
			Rectangle(HandleRect(0));
			Rectangle(HandleRect(1));
			Rectangle(HandleRect(2));
//			Rectangle(0, 0, Q, Q);
//			Rectangle((Width - Q) div 2, 0, (Width + Q) div 2, Q);
//			Rectangle(Width - Q, 0, Width, Q);
		end
		else
			Rectangle(HandleRect(3));
//			Rectangle(0, (Height - Q) div 2, Q, (Height + Q) div 2);
	end;
end;

procedure TDesignHandle.MouseDown(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);

	function HitRect(inR: Integer): Boolean;
	var
		r: TRect;
	begin
		r := HandleRect(inR);
		Result := PtInRect(r, Point(X, Y));
	end;

var
	i: Integer;
begin
	inherited;
	for i := 0 to 3 do
		if HitRect(i) then
		begin
			FHitRect := i;
			FMouseIsDown := true;
			FMouseOffset := Point(X, Y);
			break;
		end;
end;

procedure TDesignHandle.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
	inherited;
	if FMouseIsDown then
	begin
		with ClientToParent(Point(X, Y)) do
		begin
			case FHitRect of
				1:
				begin
					DesignTestForm.Selected.Height := DesignTestForm.Selected.Height
						- ((Y - FMouseOffset.Y) - DesignTestForm.Selected.Top);
					DesignTestForm.Selected.Top := Y - FMouseOffset.Y;
				end;
				3:
				begin
					DesignTestForm.Selected.Width := DesignTestForm.Selected.Width
						- ((X - FMouseOffset.X) - DesignTestForm.Selected.Left);
					DesignTestForm.Selected.Left := X - FMouseOffset.X;
				end;
			end;
			DesignTestForm.UpdateHandles;
		end;
	end;
end;

procedure TDesignHandle.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
	Y: Integer);
begin
	inherited;
	FMouseIsDown := false;
end;

{ TDesignPanel }

constructor TDesignPanel.Create(inOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle + [ csAcceptsControls ];
end;

type
	TControlCracker = class(TControl);

procedure TDesignPanel.WndProc(var Message: TMessage);
var
	ctrlWndProc: TWndMethod;
begin
	case Message.Msg of
		WM_MOUSEFIRST..WM_MOUSELAST:
		begin
			TMethod(ctrlWndProc).code := @TControlCracker.WndProc;
			TMethod(ctrlWndProc).data := Self;
			ctrlWndProc( Message );
		end
	else
		inherited WndProc( Message );
	end;
end;

procedure TDesignPanel.AdjustClientRect(var Rect: TRect);
begin
	inherited;
	//InflateRect(Rect, -8, -8);
end;

{
procedure TDesignPanel.DrawFocusRect;
var
	r: TRect;
	desktopWindow: HWND;
	dc: HDC;
	c: TCanvas;
begin
	with r do
	begin
		topLeft := ClientToScreen(Point(0, 0));
		bottomRight := ClientToScreen(Point(Width, Height));
	end;
	//
	desktopWindow := GetDesktopWindow;
	dc := GetDCEx(desktopWindow, 0, DCX_CACHE or DCX_LOCKWINDOWUPDATE);
	try
		c := TCanvas.Create;
		try
			c.Handle := dc;
			DrawBitmapBrushFrame(r, c);
		finally
			c.Free;
		end;
	finally
		ReleaseDC(desktopWindow, dc);
	end;
end;
}

procedure TDesignPanel.DrawFocusRect;
begin
	with Canvas do
	begin
		if Brush.Bitmap = nil then
		begin
			Brush.Bitmap := TBitmap.Create;
			Brush.Bitmap.Height := 2;
			Brush.Bitmap.Width := 2;
			Brush.Bitmap.Canvas.Pixels[0, 0] := clSilver;
			Brush.Bitmap.Canvas.Pixels[1, 0] := clGray;
			Brush.Bitmap.Canvas.Pixels[1, 1] := clSilver;
			Brush.Bitmap.Canvas.Pixels[0, 1] := clGray;
		end;
		//
		//Brush.Style := bsBDiagonal;
		//Brush.Color := clGray;
		//SetBkColor(Canvas.Handle, clSilver);
		FillRect(Rect(0, 0, Width, S));
		FillRect(Rect(0, 0, S, Height));
		FillRect(Rect(Width-S, 0, Width, Height));
		FillRect(Rect(0, Height - S, Width, Height));
	end;
end;

procedure TDesignPanel.Paint;
begin
	inherited;
//	if Focused then
//		DrawFocusRect;
end;

procedure TDesignPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	inherited;
	SetFocus;
	FMouseIsDown := true;
	FMouseOffset := Point(X, Y);
end;

procedure TDesignPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
	inherited;
	if FMouseIsDown then
	begin
		//FMouseLast := ClientToParent(X, Y);
		with ClientToParent(Point(X, Y)) do
		begin
			X := X - FMouseOffset.X;
			Y := Y - FMouseOffset.Y;
			BoundsRect := Rect(X, Y, X + Width, Y + Height);
			DesignTestForm.UpdateHandles;
		end;
	end;
end;

procedure TDesignPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
	Y: Integer);
begin
	inherited;
//	Mouse.Capture := 0;
	FMouseIsDown := false;
end;

procedure TDesignPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
	inherited;
	if FMouseIsDown and (Key = VK_ESCAPE) then
	begin
//		Mouse.Capture := 0;
		FMouseIsDown := false;
	end;
end;

procedure TDesignPanel.DoEnter;
begin
	inherited;
	DesignTestForm.Selected := Self;
	DesignTestForm.UpdateHandles;
	//Invalidate;
end;

procedure TDesignPanel.DoExit;
begin
	inherited;
	Invalidate;
end;

{ TDesignTestForm }

procedure TDesignTestForm.FormCreate(Sender: TObject);
var
	p: TDesignPanel;
begin
	Handles[0] := TDesignHandle.Create(Self);
	Handles[0].Parent := Self;
	Handles[1] := TDesignHandle.Create(Self);
	Handles[1].Parent := Self;
	Handles[2] := TDesignHandle.Create(Self);
	Handles[2].Parent := Self;
	Handles[3] := TDesignHandle.Create(Self);
	Handles[3].Parent := Self;
	//
	p := TDesignPanel.Create(Self);
	with p do
	begin
		Left := 24;
		Top := 24;
		Width := 96;
		Height := 84;
		Parent := Self;
	end;
	with TLabel.Create(p) do
	begin
		Caption := 'Test1';
		Align := alClient;
		Color := clWhite;
		Parent := p;
	end;
	p := TDesignPanel.Create(Self);
	with p do
	begin
		Left := 44;
		Top := 44;
		Width := 96;
		Height := 84;
		Parent := Self;
	end;
	with TLabel.Create(p) do
	begin
		Caption := 'Test2';
		Align := alClient;
		Color := clWhite;
		Parent := p;
	end;
	Selected := p;
	UpdateHandles;
	Show;
end;

procedure TDesignTestForm.UpdateHandles;
var
	i: Integer;
begin
	if Selected <> nil then
		with Selected.BoundsRect do
		begin
			Handles[0].BoundsRect := Rect(Left - Q, Top - Q, Right + Q, Top);
			Handles[1].BoundsRect := Rect(Left - Q, Top, Left, Bottom);
			Handles[2].BoundsRect := Rect(Right, Top, Right + Q, Bottom);
			Handles[3].BoundsRect := Rect(Left - Q, Bottom, Right + Q, Bottom + Q);
		end;
	for i := 0 to 3 do
	begin
		Handles[i].Visible := Selected <> nil;
		Handles[i].BringToFront;
	end;
	DesignTestForm.Update;
end;

end.
