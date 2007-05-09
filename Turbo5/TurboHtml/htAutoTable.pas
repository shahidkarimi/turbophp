unit htAutoTable;

interface

uses
	Windows, Messages, SysUtils, Types, Classes, Controls, Graphics, Forms,
	htInterfaces, htMarkup, htControls,
	LrControlIterator;

type
	ThtRow = class
	protected
		procedure FindMaxHeight(inControl: TControl);
	public
		RowHeight: Integer;
		Controls: TList;
		constructor Create;
		destructor Destroy; override;
		procedure Clear;
		procedure InsertCtrl(inC: TControl);
	end;
	//
	ThtCustomAutoTable = class(ThtCustomControl)
	protected
		Iterator: TLrSortedCtrlIterator;
		MouseIsDown: Boolean;
		MousePos: TPoint;
		MouseRow: Integer;
		Rows: TList;
		function AboveRowLine(inY: Integer; var outRow: Integer): Boolean;
		function FindRowCtrl(inB: Integer): TControl;
		function GetRowHeights(inIndex: Integer): Integer;
		function InsertRowCtrl(inRow: ThtRow; inC: TControl): Boolean;
		procedure AlignControls(AControl: TControl; var Rect: TRect); override;
		procedure ClearRows;
		procedure CMDesignHitTest(var Msg: TCMDesignHitTest);
			message CM_DESIGNHITTEST;
		procedure DrawDragLine(inY: Integer);
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
			Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,
			Y: Integer);  override;
		procedure NormalizeBounds;
		procedure NormalizeRows;
		procedure SetRowHeights(inIndex: Integer; const Value: Integer);
		procedure StylePaint; override;
		procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		procedure Generate(const inContainer: string;
			inMarkup: ThtMarkup); override;
		property RowHeights[inIndex: Integer]: Integer read GetRowHeights
			write SetRowHeights;
	end;
	//
	ThtAutoTable = class(ThtCustomAutoTable)
	published
		property Align;
		property Outline;
		property Style;
		property Visible;
	end;

implementation

uses
	LrVclUtils;

{ ThtRow }

constructor ThtRow.Create;
begin
	Controls := TList.Create;
end;

destructor ThtRow.Destroy;
begin
	Controls.Free;
	inherited;
end;

procedure ThtRow.Clear;
begin
	Controls.Clear;
	inherited;
end;

procedure ThtRow.FindMaxHeight(inControl: TControl);
begin
	inControl.Perform(CM_FONTCHANGED, 0, 0);
	RowHeight := LrMax(inControl.Height, RowHeight);
end;

procedure ThtRow.InsertCtrl(inC: TControl);
var
	i: Integer;
begin
	FindMaxHeight(inC);
	for i := 0 to Pred(Controls.Count) do
		if TControl(Controls[i]).Left > inC.Left then
		begin
			Controls.Insert(i, inC);
			exit;
		end;
	Controls.Add(inC);
end;

{ ThtCustomAutoTable }

constructor ThtCustomAutoTable.Create(inOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle + [ csAcceptsControls ];
	Rows := TList.Create;
	Transparent := true;
end;

destructor ThtCustomAutoTable.Destroy;
begin
	Rows.Free;
	inherited;
end;

procedure ThtCustomAutoTable.ClearRows;
var
	i: Integer;
begin
	for i := 0 to Pred(Rows.Count) do
		ThtRow(Rows[i]).Free;
	Rows.Clear;
end;

function ThtCustomAutoTable.FindRowCtrl(inB: Integer): TControl;
begin
	Result := nil;
	with Iterator do
	begin
		Reset;
		while Next do
			if Ctrl.Top < inB then
			begin
				Result := Ctrl;
				break;
			end;
	end;
end;

function ThtCustomAutoTable.InsertRowCtrl(inRow: ThtRow;
	inC: TControl): Boolean;
begin
	Result := inC <> nil;
	if Result then
	begin
		Iterator.Controls.Remove(inC);
		inRow.InsertCtrl(inC);
	end;
end;

procedure ThtCustomAutoTable.NormalizeRows;
var
	t, h: Integer;
	c: TControl;
	row: ThtRow;
begin
	ClearRows;
	Iterator := TLrSortedCtrlIterator.Create(Self);
	try
		while not Iterator.Eof do
		begin
			// New row
			row := ThTRow.Create;
			Rows.Add(row);
			// Get the first control
			Iterator.Next;
			// First available control is always the row starter
			c := Iterator.Ctrl;
			t := c.Top;
			h := c.Height;
			InsertRowCtrl(row, c);
			// Fill the rest of the row
			repeat
				c := FindRowCtrl(t + h); //row.RowHeight);
				InsertRowCtrl(row, c);
			until c = nil;
			// Start over with remaining controls
			Iterator.Reset;
		end;
	finally
		Iterator.Free;
	end;
end;

procedure ThtCustomAutoTable.NormalizeBounds;
var
	i, j, l, t: Integer;
	c: TControl;
begin
	t := 0;
	for i := 0 to Pred(Rows.Count) do
	begin
		l := 0;
		with ThtRow(Rows[i]) do
		begin
			for j := 0 to Pred(Controls.Count) do
			begin
				c := TControl(Controls[j]);
				c.SetBounds(l, t, c.Width, RowHeight);
				Inc(l, c.Width);
			end;
			Inc(t, RowHeight);
		end;
	end;
end;

procedure ThtCustomAutoTable.AlignControls(AControl: TControl;
	var Rect: TRect);
begin
	NormalizeRows;
	NormalizeBounds;
	Invalidate;
end;

function ThtCustomAutoTable.GetRowHeights(inIndex: Integer): Integer;
begin
	Result := ThtRow(Rows[inIndex]).RowHeight;
end;

procedure ThtCustomAutoTable.SetRowHeights(inIndex: Integer;
	const Value: Integer);
begin
	ThtRow(Rows[inIndex]).RowHeight := Value;
end;

procedure ThtCustomAutoTable.StylePaint;
var
	i, t: Integer;
begin
	inherited;
	with Canvas do
	begin
		Pen.Style := psDot;
		Pen.Color := clSilver;
		SetBkMode(Handle, Windows.TRANSPARENT);
		t := 0;
		for i := 0 to Pred(Rows.Count) do
		begin
			Inc(t, RowHeights[i]);
			MoveTo(0, t);
			LineTo(ClientWidth, t);
		end;
	end;
end;

function ThtCustomAutoTable.AboveRowLine(inY: Integer; var outRow: Integer): Boolean;
const
	cHysteresis = 3;
var
	t, i: Integer;
begin
	Result := true;
	t := 0;
	for i := 0 to Pred(Rows.Count) do
	begin
		outRow := i;
		Inc(t, Integer(RowHeights[i]));
		if (abs(inY - t) < cHysteresis) then
			exit;
	end;
	Result := false;
end;

procedure ThtCustomAutoTable.WMSetCursor(var Msg: TWMSetCursor);
var
	cur: HCURSOR;
	i: Integer;
begin
	cur := 0;
	if Msg.HitTest = HTCLIENT then
		if AboveRowLine(ScreenToClient(Mouse.CursorPos).Y, i) then
			cur := Screen.Cursors[crVSplit];
	if cur <> 0 then
		SetCursor(cur)
	else
		inherited;
end;

procedure ThtCustomAutoTable.DrawDragLine(inY: Integer);
begin
	with Canvas do
	begin
		//SelectClipRgn(Handle, NULLREGION);
		Pen.Color := clSilver;
		Pen.Style := psDot;
		//Pen.Mode := pmXor;
		//Pen.Width := 3;
		MoveTo(0, inY);
		LineTo(0, inY);
	end;
end;

procedure ThtCustomAutoTable.MouseDown(Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	MouseIsDown := true;
	MousePos := Point(X, Y);
end;

procedure ThtCustomAutoTable.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
	if MouseIsDown and (MousePos.Y <> Y) then
	begin
		DrawDragLine(MousePos.Y);
		RowHeights[MouseRow] := RowHeights[MouseRow] + Y - MousePos.Y;
		DisableAlign;
		NormalizeBounds;
		EnableAlign;
		MousePos := Point(X, Y);
		DrawDragLine(MousePos.Y);
	end;
end;

procedure ThtCustomAutoTable.MouseUp(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	MouseIsDown := false;
	DrawDragLine(MousePos.Y);
end;

procedure ThtCustomAutoTable.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
	ss: Boolean;
begin
	ss := MouseIsDown or AboveRowLine(Msg.Pos.Y, MouseRow);
	Msg.Result := Longint(ss);
end;

procedure ThtCustomAutoTable.Generate(const inContainer: string;
	inMarkup: ThtMarkup);
begin
	//
end;

end.
