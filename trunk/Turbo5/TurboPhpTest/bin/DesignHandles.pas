unit DesignHandles;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls;

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
	TDesignHandles = class(TComponent)
	private
		FContainer: TWinControl;
		FSelected: TControl;
		Handles: array[0..3] of TDesignHandle;
	protected
		procedure SetContainer(const Value: TWinControl);
		procedure SetSelected(const Value: TControl);
	public
		constructor Create(inOwner: TComponent); override;
		procedure PaintHandles(inCanvas: TCanvas; inPt: TPoint);
		procedure UpdateHandles;
		property Container: TWinControl read FContainer write SetContainer;
		property Selected: TControl read FSelected write SetSelected;
	end;

implementation

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
		end
		else
			Rectangle(HandleRect(3));
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
{
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
}
end;

procedure TDesignHandle.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
	Y: Integer);
begin
	inherited;
	FMouseIsDown := false;
end;

{ TDesignHandles }

constructor TDesignHandles.Create(inOwner: TComponent);
var
	i: Integer;
begin
	inherited;
	for i := 0 to 3 do
	begin
		Handles[i] := TDesignHandle.Create(Self);
		//Handles[i].Parent := TWinControl(inOwner);
		//Handles[i].Visible := false;
	end;
	//Blocker := TDesignBlocker.Create(Self);
	//Blocker.Parent := TWinControl(inOwner);
	//Blocker.Visible := false;
end;

procedure TDesignHandles.UpdateHandles;
var
	p: TPoint;
	r: TRect;
	c: TControl;
	i: Integer;
begin
	if (Selected <> nil) and (Container <> nil) then
	begin
{
		c := Selected;
		p := Selected.BoundsRect.topLeft;
		while (c.Parent <> Container) do
		begin
			p := c.ClientToParent(p);
			c := c.Parent;
		end;
}
		c := Selected.Parent;
		p := Selected.BoundsRect.topLeft;
		while (c <> Container) and (c <> nil) do
		begin
			Inc(p.X, c.Left);
			Inc(p.Y, c.Top);
			//p := c.ClientToParent(p);
			c := c.Parent;
		end;
		r := Rect(p.X, p.Y, p.X + Selected.Width, p.y + Selected.Height);
		with r do
		begin
			Handles[0].BoundsRect := Rect(Left - Q, Top - Q, Right + Q, Top);
			Handles[1].BoundsRect := Rect(Left - Q, Top, Left, Bottom);
			Handles[2].BoundsRect := Rect(Right, Top, Right + Q, Bottom);
			Handles[3].BoundsRect := Rect(Left - Q, Bottom, Right + Q, Bottom + Q);
		end;
		//Blocker.BoundsRect := Selected.BoundsRect;
		//Blocker.BringToFront;
	end;
	for i := 0 to 3 do
	begin
		Handles[i].Visible := Selected <> nil;
		Handles[i].BringToFront;
	end;
	//Blocker.Visible := Selected <> nil;
end;

{
procedure TDesignHandles.MoveHandles(inPt: TPoint);
var
	d: TPoint;
	i: Integer;
begin
	if (Container <> nil) then
	begin
		with Handles[0].BoundsRect.TopLeft do
		begin
			d.X := inPt.X - X;
			d.Y := inPt.Y - Y;
		end;
		for i := 0 to 3 do
			with Handles[i] do
			begin
				Left := Left + d.X;
				Top := Top + d.Y;
			end;
	end;
end;
}

procedure TDesignHandles.PaintHandles(inCanvas: TCanvas; inPt: TPoint);
var
	desktopWindow: HWND;
	dc: HDC;
	c: TCanvas;
begin
	inPt := Container.ClientToScreen(inPt);
	desktopWindow := GetDesktopWindow;
	dc := GetDCEx(desktopWindow, 0, DCX_CACHE or DCX_LOCKWINDOWUPDATE);
	try
		c := TCanvas.Create;
		with c do
		try
			Handle := dc;
//			c.Brush.Style := bsClear;
//			c.Pen.Mode := pmNotXor;
			Pen.Style := psSolid;
			Pen.Color := clWhite;
			Pen.Mode := pmXor;
			MoveTo(inPt.X, inPt.Y);
			LineTo(inPt.X + Selected.Width, inPt.Y);
			LineTo(inPt.X + Selected.Width, inPt.Y + Selected.Height);
			LineTo(inPt.X, inPt.Y + Selected.Height);
			LineTo(inPt.X, inPt.Y);
//			Rectangle(
//				Rect(inPt.X, inPt.Y, inPt.X + Selected.Width, inPt.Y + Selected.Height));
{
			Brush.Bitmap := NeedShadedBits;
			CopyMode := cmPatInvert;
			FillRect(
				Rect(inPt.X, inPt.Y, inPt.X + Selected.Width, inPt.Y + Selected.Height));
			Brush.Bitmap := nil;
			CopyMode := cmPatCopy;
}
		finally
			c.Free;
		end;
	finally
		ReleaseDC(desktopWindow, dc);
	end;
	//
{
	with inCanvas do
	begin
//		CopyMode := cmPatInvert;
//		Brush.Bitmap := NeedShadedBits;
		FillRect(
			Rect(inPt.X, inPt.Y, inPt.X + Selected.Width, inPt.Y + Selected.Height));
//		Brush.Bitmap := nil;
//		CopyMode := cmPatCopy;
	end;
}
end;

procedure TDesignHandles.SetSelected(const Value: TControl);
begin
	if (Selected <> Value) then
	begin
		if (Value is TDesignHandle) then
			FSelected := nil
		else
			FSelected := Value;
		UpdateHandles;
	end;
end;

procedure TDesignHandles.SetContainer(const Value: TWinControl);
var
	i: Integer;
begin
	FContainer := Value;
	for i := 0 to 3 do
	begin
		Handles[i].Visible := false;
		Handles[i].Parent := FContainer;
	end;
end;

end.
