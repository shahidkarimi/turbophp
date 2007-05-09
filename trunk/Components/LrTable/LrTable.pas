unit LrTable;

interface

uses
	SysUtils, Types, Windows, Classes, Controls, ExtCtrls, Graphics;

type
	TLrCell = class(TCustomControl)
	private
		FRowSpan: Integer;
		FCol: Integer;
		FColSpan: Integer;
		FRow: Integer;
		FFixedWidth: Boolean;
		FFixedHeight: Boolean;
	protected
		procedure SetFixedHeight(const Value: Boolean);
		procedure SetFixedWidth(const Value: Boolean);
		procedure SetCol(const Value: Integer);
		procedure SetColSpan(const Value: Integer);
		procedure SetRow(const Value: Integer);
		procedure SetRowSpan(const Value: Integer);
	protected
		procedure AdjustClientRect(var ioRect: TRect); override;
		procedure DefineProperties(Filer: TFiler); override;
		procedure ReadGrid(Reader: TReader);
		procedure WriteGrid(Writer: TWriter);
	public
		constructor Create(AOwner: TComponent); override;
		procedure Paint; override;
	public
		property Col: Integer read FCol write SetCol;
		property Row: Integer read FRow write SetRow;
	published
		property Align;
		property Color;
		property FixedHeight: Boolean read FFixedHeight write SetFixedHeight;
		property FixedWidth: Boolean read FFixedWidth write SetFixedWidth;
		property ColSpan: Integer read FColSpan write SetColSpan;
		property RowSpan: Integer read FRowSpan write SetRowSpan;
	end;
	//
	TLrTable = class(TCustomControl)
	private
		FColCount: Integer;
		FUpdating: Boolean;
		FRowCount: Integer;
		FWidths: array of Integer;
	protected
		function GetCells(inIndex: Integer): TLrCell;
		function GetGridCells(inCol, inRow: Integer): TLrCell;
		procedure SetColCount(const Value: Integer);
		procedure SetRowCount(const Value: Integer);
	protected
		procedure AdjustClientRect(var ioRect: TRect); override;
		procedure AlignControls(AControl: TControl; var Rect: TRect); override;
		procedure CalcWidths;
		function ColLeft(inCol: Integer): Integer;
		procedure DistributeCells;
		procedure RebuildCells;
		procedure RefreshCells;
	protected
		property GridCells[inCol: Integer; inRow: Integer]: TLrCell read
			GetGridCells;
		property Cells[inIndex: Integer]: TLrCell read GetCells;
	public
		constructor Create(AOwner: TComponent); override;
		procedure CellChanged(inCell: TLrCell);
	published
		property Align;
		property Color;
		property ColCount: Integer read FColCount write SetColCount;
		property RowCount: Integer read FRowCount write SetRowCount;
	end;

implementation

{ TLrCell }

constructor TLrCell.Create(AOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle + [ csAcceptsControls ];
end;

procedure TLrCell.ReadGrid(Reader: TReader);
begin
	Reader.ReadListBegin;
	FCol := Reader.ReadInteger;
	FRow := Reader.ReadInteger;
	Reader.ReadListEnd;
end;

procedure TLrCell.WriteGrid(Writer: TWriter);
begin
	Writer.WriteListBegin;
	Writer.WriteInteger(FCol);
	Writer.WriteInteger(FRow);
	Writer.WriteListEnd;
end;

procedure TLrCell.DefineProperties(Filer: TFiler);
begin
	inherited;
	Filer.DefineProperty('Coords', ReadGrid, WriteGrid, true);
end;

procedure TLrCell.AdjustClientRect(var ioRect: TRect);
begin
	InflateRect(ioRect, -2, -2);
end;

procedure TLrCell.Paint;
var
	r: TRect;
begin
	inherited;
	r := ClientRect;
	Frame3D(Canvas, r, clGray, clWhite, 1);
end;

procedure TLrCell.SetCol(const Value: Integer);
begin
	FCol := Value;
end;

procedure TLrCell.SetColSpan(const Value: Integer);
begin
	FColSpan := Value;
end;

procedure TLrCell.SetRow(const Value: Integer);
begin
	FRow := Value;
end;

procedure TLrCell.SetRowSpan(const Value: Integer);
begin
	FRowSpan := Value;
end;

procedure TLrCell.SetFixedWidth(const Value: Boolean);
begin
	FFixedWidth := Value;
	TLrTable(Parent).CellChanged(Self);
end;

procedure TLrCell.SetFixedHeight(const Value: Boolean);
begin
	FFixedHeight := Value;
	TLrTable(Parent).CellChanged(Self);
end;

{ TLrTable }

constructor TLrTable.Create(AOwner: TComponent);
begin
	inherited;
	//ControlStyle := ControlStyle + [ csAcceptsControls ];
end;

procedure TLrTable.AdjustClientRect(var ioRect: TRect);
begin
	InflateRect(ioRect, -2, -2);
end;

procedure TLrTable.RebuildCells;
var
	i, j: Integer;
	cell: TLrCell;
begin
	for i := 0 to Pred(ColCount) do
		for j := 0 to Pred(RowCount) do
		begin
			cell := GridCells[i, j];
			if (cell = nil) then
			begin
				cell := TLrCell.Create(Owner);
				cell.Name := Format('Cell_%d_%d', [ i, j]);
				cell.Parent := Self;
				cell.Col := i;
				cell.Row := j;
			end;
		end;
	i := 0;
	while (i < ControlCount) do
	begin
		cell := Cells[i];
		if (cell.Col >= ColCount) or (cell.Row >= RowCount) then
			cell.Free
		else
			Inc(i);
	end;
end;

procedure TLrTable.CalcWidths;
var
	i, c, cw, w, wx: Integer;
begin
	SetLength(FWidths, ColCount);
	FillChar(FWidths[0], ColCount * 4, 0);
	for i := 0 to Pred(ControlCount) do
		with Cells[i] do
		begin
			if (Width > FWidths[Col]) and FixedWidth then
				FWidths[Col] := Width;
		end;
	c := 0;
	cw := ClientWidth - 4;
	for i := 0 to Pred(ColCount) do
		if FWidths[i] = 0 then
			Inc(c)
		else
			Dec(cw, FWidths[i]);
	if (c > 0) then
	begin
		w := cw div c;
		if (w < 4) then
		begin
			w := 4;
			ClientWidth := ClientWidth + w * c;
			Inc(cw, w*c);
		end;
		wx := cw - (w * c);
		for i := 0 to Pred(ColCount) do
			if FWidths[i] = 0 then
			begin
				if (wx > 0) then
					FWidths[i] := w + 1
				else
					FWidths[i] := w;
				Dec(wx);
			end;
	end;
end;

function TLrTable.ColLeft(inCol: Integer): Integer;
var
	i: Integer;
begin
	Result := 2;
	for i := 0 to Pred(inCol) do
		Inc(Result, FWidths[i]);
end;

procedure TLrTable.DistributeCells;
var
	h, hx: Integer;
	i: Integer;
begin
	if (ColCount = 0) or (RowCount = 0) then
		exit;
	DisableAlign;
	try
		CalcWidths;
		h := (ClientHeight - 4) div RowCount;
		hx := (ClientHeight - 4) - (h * RowCount);
		for i := 0 to Pred(ControlCount) do
			with Cells[i] do
			begin
				Left := ColLeft(Col);
				Width := FWidths[Col];
				Top := h * Row + 2;
				if (Row = Pred(RowCount)) then
					Height := h + hx
				else
					Height := h;
			end;
	finally
		EnableAlign;
	end;
end;

procedure TLrTable.RefreshCells;
begin
	if not (csLoading in ComponentState) and not FUpdating then
	begin
		FUpdating := true;
		try
			RebuildCells;
		finally
			FUpdating := false;
		end;
		DistributeCells;
	end;
end;

procedure TLrTable.AlignControls(AControl: TControl; var Rect: TRect);
begin
	if not FUpdating then
		DistributeCells;
end;

function TLrTable.GetCells(inIndex: Integer): TLrCell;
begin
	Result := TLrCell(Controls[inIndex]);
end;

function TLrTable.GetGridCells(inCol, inRow: Integer): TLrCell;
var
	i: Integer;
begin
	for i := 0 to Pred(ControlCount) do
		with Cells[i] do
			if (Col = inCol) and (Row = inRow) then
			begin
				Result := Cells[i];
				exit;
			end;
	Result := nil;
//	raise Exception.Create(
//		Format('Bad cell grid index (%d, %d)', [ inCol, inRow ]));
end;

procedure TLrTable.SetRowCount(const Value: Integer);
begin
	FRowCount := Value;
	RefreshCells;
end;

procedure TLrTable.SetColCount(const Value: Integer);
begin
	FColCount := Value;
	RefreshCells;
end;

procedure TLrTable.CellChanged(inCell: TLrCell);
var
	i: Integer;
begin
	exit;
	if not FUpdating then
	begin
		FUpdating := true;
		try
			for i := 0 to Pred(ControlCount) do
				with Cells[i] do
					if Col = inCell.Col then
						FixedWidth := inCell.FixedWidth;
		finally
			FUpdating := false
		end;
	end;
end;

initialization
	Classes.RegisterClass(TLrCell);
end.
