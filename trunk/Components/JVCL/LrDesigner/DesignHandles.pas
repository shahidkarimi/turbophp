{-----------------------------------------------------------------------------

 Least-Resistance Designer Library

 The Initial Developer of the Original Code is Scott J. Miles
	<sjmiles (at) turbophp (dot) com>.
 Portions created by Scott J. Miles are
	Copyright (C) 2005 Least-Resistance Software.
 All Rights Reserved.

-------------------------------------------------------------------------------}

unit DesignHandles;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls;

type
	TDesignHandleId = (	dhNone, dhLeftTop, dhMiddleTop, dhRightTop, dhLeftMiddle,
		dhRightMiddle, dhLeftBottom, dhMiddleBottom, dhRightBottom );
	//
	TDesignHandle = class(TCustomControl)
	private
		FResizeable: Boolean;
	protected
		procedure PaintHandle(const inRect: TRect);
	protected
		function HandleRect(inIndex: Integer): TRect;
		function HitRect(X, Y: Integer): Integer;
		procedure Paint; override;
		property Resizeable: Boolean read FResizeable write FResizeable;
	end;
	//
	TDesignHandles = class(TComponent)
	private
		FContainer: TWinControl;
		FSelected: TControl;
		Handles: array[0..3] of TDesignHandle;
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
		constructor Create(inOwner: TComponent); override;
		function HitRect(X, Y: Integer): TDesignHandleId;
		function SelectedToContainer(const inPt: TPoint): TPoint;
		procedure RepaintHandles;
		procedure UpdateHandles;
		property Container: TWinControl read FContainer write SetContainer;
		property HandleWidth: Integer read GetHandleWidth;
		property Resizeable: Boolean read FResizeable write SetResizeable;
		property Selected: TControl read FSelected write SetSelected;
	end;

implementation

uses
	DesignController;

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

{ TDesignHandle }

function TDesignHandle.HandleRect(inIndex: Integer): TRect;
var
	w: Integer;
begin
	w := TDesignHandles(Owner).HandleWidth;
	case inIndex of
		0: Result := Rect(0, 0, w, w);
		1: Result := Rect((Width - w) div 2, 0, (Width + w) div 2, w);
		2: Result := Rect(Width - w, 0, Width, w);
		3: Result := Rect(0, (Height - w) div 2, w, (Height + w) div 2);
	end;
end;

procedure TDesignHandle.PaintHandle(const inRect: TRect);
begin
	Canvas.Rectangle(inRect);
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

function TDesignHandle.HitRect(X, Y: Integer): Integer;
var
	i: Integer;
	r: TRect;
begin
	Result := -1;
	for i := 0 to 3 do
	begin
		r := HandleRect(i);
		if PtInRect(r, Point(X, Y)) then
		begin
			Result := i;
			break;
		end;
	end;
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
	Result := TDesignController(Owner).HandleWidth;
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
				BringToFront;
			Update;
		end;
end;

procedure TDesignHandles.UpdateHandles;
begin
	if (Selected <> nil) and (Container <> nil) and	(Selected <> Container) then
	begin
		SetHandleRects(GetSelectionRect);
		ShowHideHandles(true);
	end else
		ShowHideHandles(false)
end;

procedure TDesignHandles.RepaintHandles;
var
	i: INteger;
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
			r := HitRect(X - Left, Y - Top);
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

end.
