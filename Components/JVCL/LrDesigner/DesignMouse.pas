{-----------------------------------------------------------------------------

 Least-Resistance Designer Library

 The Initial Developer of the Original Code is Scott J. Miles
	<sjmiles (at) turbophp (dot) com>.
 Portions created by Scott J. Miles are
	Copyright (C) 2005 Least-Resistance Software.
 All Rights Reserved.

-------------------------------------------------------------------------------}

unit DesignMouse;

interface

uses
	Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, ExtCtrls,
	Contnrs,
	DesignHandles, DesignController;

type
	TDesignCustomMouseTool = class(TDesignMouseTool)
	private
		FController: TDesignController;
		FMouseLast: TPoint;
		FMouseStart: TPoint;
	protected
		function GetMouseDelta: TPoint; virtual;
	public
		constructor Create(AOwner: TDesignController); virtual;
		property Controller: TDesignController read FController write FController;
	end;
	//
	TDesignMover = class(TDesignCustomMouseTool)
	private
		FDragRects: array of TRect;
	protected
		procedure ApplyDragRects;
		procedure CalcDragRects;
		procedure CalcPaintRects;
		procedure PaintDragRects;
	public
		constructor Create(AOwner: TDesignController); override;
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
	end;
	//
	TDesignBander = class(TDesignCustomMouseTool)
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
		constructor CreateSizer(AOwner: TDesignController;
			inHandle: TDesignHandleId);
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
	end;

implementation

	procedure LrPaintRubberbandRect(const inRect: TRect; inPenStyle: TPenStyle);
	var
		desktopWindow: HWND;
		dc: HDC;
		c: TCanvas;
	begin
		desktopWindow := GetDesktopWindow;
		dc := GetDCEx(desktopWindow, 0, DCX_CACHE or DCX_LOCKWINDOWUPDATE);
		try
			c := TCanvas.Create;
			with c do
			try
				Handle := dc;
				Pen.Style := inPenStyle;
				Pen.Color := clWhite;
				Pen.Mode := pmXor;
				Brush.Style := bsClear;
				Rectangle(inRect);
			finally
				c.Free;
			end;
		finally
			ReleaseDC(desktopWindow, dc);
		end;
	end;

{ TDesignCustomMouseTool }

constructor TDesignCustomMouseTool.Create(AOwner: TDesignController);
begin
	Controller := AOwner;
end;

function TDesignCustomMouseTool.GetMouseDelta: TPoint;
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

constructor TDesignMover.Create(AOwner: TDesignController);
begin
	inherited;
	SetLength(FDragRects, Controller.Count);
end;

procedure TDesignMover.CalcDragRects;
var
	delta: TPoint;
	i: Integer;
begin
	delta := GetMouseDelta;
	for i := 0 to Pred(Controller.Count) do
		with Controller.Selection[i] do
		begin
			FDragRects[i] := BoundsRect;
			OffsetRect(FDragRects[i],	delta.X, delta.Y);
		end;
end;

procedure TDesignMover.CalcPaintRects;
var
	i: Integer;
begin
	CalcDragRects;
	for i := 0 to Pred(Controller.Count) do
		with Controller.Selection[i] do
			with Parent.ClientToScreen(Point(0, 0)) do
				OffsetRect(FDragRects[i],	X, Y);
end;

procedure TDesignMover.PaintDragRects;
var
	i: Integer;
begin
	for i := 0 to Pred(Controller.Count) do
		LrPaintRubberbandRect(FDragRects[i], psDot);
end;

procedure TDesignMover.ApplyDragRects;
var
	i: Integer;
begin
	if (GetMouseDelta.X <> 0) or (GetMouseDelta.Y <> 0) then
	begin
		CalcDragRects;
		for i := 0 to Pred(Controller.Count) do
			Controller.Selection[i].BoundsRect := FDragRects[i];
		Controller.UpdateDesigner;
		Controller.Change;
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
	Result := Controller.Container;
end;

function TDesignBander.GetPaintRect: TRect;
begin
	Result := FDragRect;
	with GetClient.ClientToScreen(Point(0, 0)) do
		OffsetRect(Result, X, Y);
end;

procedure TDesignBander.PaintDragRect;
begin
	LrPaintRubberbandRect(GetPaintRect, psDot);
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

constructor TDesignSizer.CreateSizer(AOwner: TDesignController;
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
	FDragRect := Controller.Selected.BoundsRect;
	with GetMouseDelta do
		ApplyMouseDelta(X, Y);
end;

function TDesignSizer.GetClient: TControl;
begin
	Result := Controller.Selected.Parent;
end;

procedure TDesignSizer.ApplyDragRect;
begin
	Controller.Selected.BoundsRect := FDragRect;
	Controller.UpdateDesigner;
	Controller.Change;
end;

procedure TDesignSizer.MouseUp(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	inherited;
	ApplyDragRect;
end;

end.
