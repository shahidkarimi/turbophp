{-----------------------------------------------------------------------------

 Least-Resistance Designer Library

 The Initial Developer of the Original Code is Scott J. Miles
	<sjmiles (at) turbophp (dot) com>.
 Portions created by Scott J. Miles are
	Copyright (C) 2005 Least-Resistance Software.
 All Rights Reserved.

-------------------------------------------------------------------------------}

unit DesignController;

interface

uses
	Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, ExtCtrls,
	Contnrs,
	DesignSurface, DesignHandles;

const
	cDesignDefaultHandleWidth = 8;

type
	TDesignDragMode = ( dmNone, dmMove, dmResize, dmSelect, dmCreate );
	//
	TDesignController = class;
	//
	TDesignMouseTool = class
	protected
		FDragRect: TRect;
	public
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); virtual; abstract;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual; abstract;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer);  virtual; abstract;
		property DragRect: TRect read FDragRect write FDragRect;
	end;
	//
	TDesignGetAddClassEvent = procedure(Sender: TObject;
		var ioClass: string) of object;
	//
	TDesignController = class(TDesignSurface)
	private
		FAddClass: string;
		FClicked: TControl;
		FDragRect: TRect;
		FDragBounds: TRect;
		FDragMode: TDesignDragMode;
		FHandles: TObjectList;
		FHandleWidth: Integer;
		FKeyDownShift: TShiftState;
		FMouseIsDown: Boolean;
		FMouseTool: TDesignMouseTool;
		FOnGetAddClass: TDesignGetAddClassEvent;
		FOnSelectionChange: TNotifyEvent;
	protected
		function DesignKeyDown(inKeycode: Cardinal;
			inShift: TShiftState): Boolean; override;
		function DesignKeyUp(inKeycode: Cardinal;
			inShift: TShiftState): Boolean; override;
		function FindHandles(const Value: TControl): TDesignHandles;
		function FindControl(inX, inY: Integer): TControl;
		function GetCount: Integer;
		function GetHandles(inIndex: Integer): TDesignHandles;
		function GetSelected: TControl;
		function GetSelectedContainer: TWinControl;
		function GetSelection(inIndex: Integer): TControl;
		function IsSelected(const Value: TControl): Boolean;
		procedure AddComponent;
		procedure AddToSelection(const Value: TControl);
		procedure ClearSelection;
		function DesignMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer): Boolean; override;
		function DesignMouseMove(Sender: TObject; Shift: TShiftState;
			X, Y: Integer): Boolean; override;
		function DesignMouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer): Boolean; override;
		procedure DragRectToDragBounds;
		procedure GetAddClass;
		procedure RemoveFromSelection(const Value: TControl);
		procedure SelectionChange;
		procedure SetActive(const Value: Boolean); override;
		procedure SetAddClass(const Value: string);
		procedure SetContainer(const Value: TWinControl); override;
		procedure SetHandles(inIndex: Integer; const Value: TDesignHandles);
		procedure SetHandleWidth(const Value: Integer);
		procedure SetOnSelectionChange(const Value: TNotifyEvent);
		procedure SetSelected(const Value: TControl);
		procedure SetSelection(inIndex: Integer; const Value: TControl);
		procedure ShowHideResizeHandles;
		procedure ToggleSelection(const Value: TControl);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		function ContainerToSelectedContainer(const inPt: TPoint): TPoint;
		procedure CopyComponents;
		procedure CutComponents;
		procedure DeleteComponents;
		procedure PasteComponents;
		procedure SelectParent;
		procedure UpdateDesigner;
		property AddClass: string read FAddClass write SetAddClass;
		property Count: Integer read GetCount;
		property Selection[inIndex: Integer]: TControl read GetSelection
			write SetSelection;
		property Handles[inIndex: Integer]: TDesignHandles read GetHandles
			write SetHandles;
		property Selected: TControl read GetSelected write SetSelected;
		property SelectedContainer: TWinControl read GetSelectedContainer;
		property DragBounds: TRect read FDragBounds write FDragBounds;
	published
		property HandleWidth: Integer read FHandleWidth
			write SetHandleWidth default cDesignDefaultHandleWidth;
		property OnGetAddClass: TDesignGetAddClassEvent read FOnGetAddClass
			write FOnGetAddClass;
		property OnSelectionChange: TNotifyEvent read FOnSelectionChange
			write SetOnSelectionChange;
	end;

implementation

uses
	Clipbrd, DesignUtils, DesignClip, DesignMouse;

{ TDesignController }

constructor TDesignController.Create(AOwner: TComponent);
begin
	inherited;
	FHandleWidth := cDesignDefaultHandleWidth;
	FHandles := TObjectList.Create;
end;

destructor TDesignController.Destroy;
begin
	FHandles.Free;
	inherited;
end;

procedure TDesignController.SetHandleWidth(const Value: Integer);
begin
	FHandleWidth := Value;
	UpdateDesigner;
end;

procedure TDesignController.SetContainer(const Value: TWinControl);
begin
	ClearSelection;
	inherited;
end;

procedure TDesignController.SetActive(const Value: Boolean);
begin
	if not (csDestroying in ComponentState) then
		ClearSelection;
	inherited;
end;

procedure TDesignController.GetAddClass;
begin
	if Assigned(OnGetAddClass) then
		OnGetAddClass(Self, FAddClass);
end;

procedure TDesignController.SetAddClass(const Value: string);
begin
	FAddClass := Value;
end;

procedure TDesignController.SetOnSelectionChange(const Value: TNotifyEvent);
begin
	FOnSelectionChange := Value;
end;

procedure TDesignController.SelectionChange;
begin
	if Assigned(FOnSelectionChange) then
		FOnSelectionChange(Self);
end;

function TDesignController.GetCount: Integer;
begin
	Result := FHandles.Count;
end;

function TDesignController.GetHandles(inIndex: Integer): TDesignHandles;
begin
	Result := TDesignHandles(FHandles[inIndex]);
end;

procedure TDesignController.SetHandles(inIndex: Integer;
	const Value: TDesignHandles);
begin
	FHandles[inIndex] := Value;
end;

function TDesignController.GetSelection(inIndex: Integer): TControl;
begin
	Result := Handles[inIndex].Selected;
end;

procedure TDesignController.SetSelection(inIndex: Integer;
	const Value: TControl);
begin
	Handles[inIndex].Selected := Value;
end;

procedure TDesignController.ShowHideResizeHandles;
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

function TDesignController.FindHandles(const Value: TControl): TDesignHandles;
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
	begin
		Result := Handles[i];
		if Result.Selected = Value then
			exit;
	end;
	Result := nil;
end;

function TDesignController.IsSelected(const Value: TControl): Boolean;
begin
	Result := FindHandles(Value) <> nil;
end;

procedure TDesignController.ClearSelection;
begin
	FHandles.Clear;
end;

procedure TDesignController.AddToSelection(const Value: TControl);
var
	h: TDesignHandles;
begin
	if Value = nil then
		raise Exception.Create('Cannot add a nil selection.');
	if not IsSelected(Value) then
	begin
		h := TDesignHandles.Create(Self);
		h.Container := Container;
		h.Resizeable := Count = 0;
		FHandles.Add(h);
		h.Selected := Value;
		if (Count = 2) then
			ShowHideResizeHandles
		else
			h.UpdateHandles;
		SelectionChange;
	end;
end;

procedure TDesignController.RemoveFromSelection(const Value: TControl);
begin
	if IsSelected(Value) then
	begin
		FHandles.Remove(FindHandles(Value));
		SelectionChange;
	end;
end;

procedure TDesignController.ToggleSelection(const Value: TControl);
begin
	if IsSelected(Value) then
		RemoveFromSelection(Value)
	else
		AddToSelection(Value);
end;

function TDesignController.GetSelected: TControl;
begin
	if Count > 0 then
		Result := Handles[0].Selected
	else
		Result := nil;
end;

procedure TDesignController.SetSelected(const Value: TControl);
begin
	ClearSelection;
	if Value <> nil then
		AddToSelection(Value);
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
	if c = nil then
		c := Container
	else if (c is TDesignHandle) then
		c := TDesignHandles(c.Owner).Selected;
	Result := c;
end;

function TDesignController.DesignMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
	handleId: TDesignHandleId;
begin
	Result := true;
	//
	if not Container.Focused and Container.CanFocus then
		Container.SetFocus;
	//
	FMouseIsDown := true;
	Mouse.Capture := Container.Handle;
	//
	handleId := dhNone;
	if (ssCtrl in Shift) then
		// Ctrl-drag selection has highest priority
		FDragMode := dmSelect
	else begin
		if (Count > 0) then
			handleId := Handles[0].HitRect(X, Y);
		if (handleId <> dhNone) then
		begin
			FClicked := Handles[0].Selected;
			// Resizing is next
			FDragMode := dmResize;
		end
		else begin
			FClicked := FindControl(X, Y);
			if (FClicked = Container) or (FClicked is TDesignHandle) then
				FClicked := nil;
			GetAddClass;
			if (AddClass <> '') then
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
	//
	if FClicked = nil then
		FClicked := Container;
	FClicked.Parent.DisableAlign;
	//
	case FDragMode of
		dmSelect, dmCreate:
		begin
			ClearSelection;
			FMouseTool := TDesignBander.Create(Self);
		end;
		//
		dmMove:
		begin
			if (ssShift in Shift) then
				AddToSelection(FClicked)
			else if not IsSelected(FClicked) then
				Selected := FClicked;
			FMouseTool := TDesignMover.Create(Self);
		end;
		//
		dmResize:
		begin
			Selected := FClicked;
			FMouseTool := TDesignSizer.CreateSizer(Self, handleId);
		end;
	end;
	//
	if FMouseTool <> nil then
		FMouseTool.MouseDown(Button, Shift, X, Y);
end;

function TDesignController.DesignMouseMove(Sender: TObject;
	Shift: TShiftState;	X, Y: Integer): Boolean;
const
	cCurs: array[TDesignHandleId] of TCursor =
		( crHandPoint{crArrow}, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeWE,
			crSizeNESW, crSizeNS, crSizeNWSE );
begin
	Result := true;
	if FMouseIsDown then
	begin
		if FMouseTool <> nil then
			FMouseTool.MouseMove(Shift, X, Y);
	end else
		if (Count > 0) and (FindControl(X, Y) <> Container) then
			Windows.SetCursor(Screen.Cursors[cCurs[Handles[0].HitRect(X, Y)]])
		else
			Windows.SetCursor(Screen.Cursors[crDefault]);
end;

function TDesignController.DesignMouseUp(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
	Result := true;
	if FMouseIsDown then
	begin
		FMouseIsDown := false;
		Mouse.Capture := 0;
		if FClicked <> nil then
			FClicked.Parent.EnableAlign;
		if FMouseTool <> nil then
		try
			FMouseTool.MouseUp(Button, Shift, X, Y);
			FDragRect := LrValidateRect(FMouseTool.DragRect);
			case FDragMode of
				dmCreate:
				begin
					if FClicked <> nil then
						Selected := FClicked;
					AddComponent;
				end;
				//
				else SelectionChange;
			end;
		finally
			FreeAndNil(FMouseTool);
		end;
		FClicked := nil;
	end;
end;

function TDesignController.GetSelectedContainer: TWinControl;
begin
	if (Selected = nil) or (Count > 1) then
		Result := Container
	else if (Selected is TWinControl) and
		(csAcceptsControls in Selected.ControlStyle) then
			Result := TWinControl(Selected)
	else
		Result := Selected.Parent;
end;

procedure TDesignController.UpdateDesigner;
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		Handles[i].UpdateHandles;
end;

function TDesignController.ContainerToSelectedContainer(
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

procedure TDesignController.DragRectToDragBounds;
begin
	with FDragBounds do
	begin
		TopLeft := ContainerToSelectedContainer(FDragRect.TopLeft);
		BottomRight := ContainerToSelectedContainer(FDragRect.BottomRight);
	end;
end;

procedure TDesignController.AddComponent;
var
	cc: TComponentClass;
	c: TComponent;
	co: TControl;
begin
	cc := TComponentClass(GetClass(AddClass));
	if (cc <> nil) and (SelectedContainer <> nil) then
	begin
		c := cc.Create(Owner);
		c.Name := LrUniqueName(Owner, AddClass);
		if (c is TControl) then
		begin
			co := TControl(c);
			co.Parent := SelectedContainer;
			DragRectToDragBounds;
			if LrWidth(DragBounds) = 0 then
				FDragBounds.Right := FDragBounds.Left + co.Width;
			if LrHeight(DragBounds) = 0 then
				FDragBounds.Bottom := FDragBounds.Top + co.Height;
			co.BoundsRect := DragBounds;
			Selected := co;
		end;
		AddClass := '';
		Change;
	end;
end;

procedure TDesignController.DeleteComponents;
var
	i: Integer;
begin
	if Count > 0 then
	begin
		for i := 0 to Pred(Count) do
			Selection[i].Free;
		Selected := nil;
		Change;
	end;
end;

procedure TDesignController.CopyComponents;
var
	i: Integer;
begin
	with TDesignComponentClipboard.Create do
	try
		OpenWrite;
		try
			for i := 0 to Pred(Count) do
				SetComponent(Selection[i]);
		finally
			CloseWrite;
		end;
	finally
		Free;
	end;
end;

procedure TDesignController.CutComponents;
begin
	CopyComponents;
	DeleteComponents;
end;

procedure TDesignController.PasteComponents;
var
	c: TComponent;
	co: TControl;
	p: TWinControl;
begin
	with TDesignComponentClipboard.Create do
	try
		OpenRead;
		try
			c := GetComponent;
			if (c <> nil) then
			begin
				p := SelectedContainer;
				ClearSelection;
				repeat
					c.Name := LrUniqueName(Owner, c.ClassName);
					Owner.InsertComponent(c);
					if c is TControl then
					begin
						co := TControl(c);
						with p do
						begin
							if co.Left > ClientWidth then
								co.Left := ClientWidth - co.Width;
							if co.Top > ClientHeight then
								co.Top := ClientHeight - co.Height;
						end;
						co.Parent := p;
						AddToSelection(co);
					end;
					c := GetComponent;
				until (c = nil);
				Change;
			end;
		finally
			CloseRead;
		end;
	finally
		Free;
	end;
end;

procedure TDesignController.SelectParent;
begin
	if Selected <> nil then
		Selected := Selected.Parent;
end;

function TDesignController.DesignKeyUp(inKeycode: Cardinal;
	inShift: TShiftState): Boolean;
begin
	Result := true;
	case inKeycode of
		VK_ESCAPE: SelectParent;
		VK_DELETE: DeleteComponents;
		else Result := false;
	end;
	if not Result and ((ssCtrl in inShift) or (ssCtrl in FKeyDownShift)) then
	begin
		Result := true;
		case inKeycode of
			Ord('C'): CopyComponents;
			Ord('X'):	CutComponents;
			Ord('V'):	PasteComponents;
			else Result := false;
		end;
		if Result then
			Beep;
	end;
end;

function TDesignController.DesignKeyDown(inKeycode: Cardinal;
	inShift: TShiftState): Boolean;
begin
	Result := false;
	FKeyDownShift := inShift;
end;

end.
