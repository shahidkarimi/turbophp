unit ThCellSorter;

interface

uses
	Types, Classes, Controls, Contnrs, Graphics;

type
	TCellData = Record
		W, H: Integer;
		ColSpan, RowSpan: Integer;
		Control: TControl;
		Index: Integer;
	end;
	//
	TCellControl = class
	public
		Control: TControl;
		CellRect: TRect;
		Index: Integer;
		constructor Create(inControl: TControl; const inRect: TRect;
			inIndex: Integer);
	end;
	//
	TEdgeAxis = ( eaX, eaY );
	TEdgeSide = ( esStart, esEnd );
	//
	TCellTable = class
	protected
		Grid: array of array of Integer;
		ControlList: TList;
	protected
		procedure ClearGrid;
		function BuildSortedEdgeList(inAxis: TEdgeAxis;
			inSize: Integer = 0): TObjectList;
		procedure BuildGrid(inXL, inYL: TList);
		procedure BuildRow0(inXlist: TList);
		function CollapseCols(inI, inJ: Integer; inList: TList;
			var ioW: Integer): Integer;
		function CollapseRows(inI, inJ, inColSpan: Integer; inList: TList;
			var ioH: Integer): Integer;
		procedure BuildRows(inXlist, inYlist: TList);
	public
		Rows: array of array of TCellData;
		constructor Create;
		destructor Destroy; override;
		procedure AddControl(inControl: TControl; const inRect: TRect;
			inIndex: Integer);
		procedure Generate(inW, inH: Integer);
		function RowCount: Integer;
		function ColCount: Integer;
	end;
	//
	TCellEdge = class
		Pos: Integer;
		Span: Integer;
		constructor Create(inPos: Integer);
	end;

implementation

{ TCellControl }

constructor TCellControl.Create(inControl: TControl; const inRect: TRect;
	inIndex: Integer);
begin
	Control := inControl;
	CellRect := inRect;
	Index := inIndex;
end;

{ TCellData }

function CellData(inW, inH, inCols, inRows: Integer;
	inIndex: Integer {inControl: TControl}): TCellData;
begin
	with Result do
	begin
		W := inW;
		H := inH;
		ColSpan := inCols;
		RowSpan := inRows;
		Index := inIndex;
		//Control := inControl;
	end;
end;

{ TCellEdge }

constructor TCellEdge.Create(inPos: Integer);
begin
	Pos := inPos;
end;

{ TCellTable }

constructor TCellTable.Create;
begin
	ControlList := TObjectList.Create(true);
end;

destructor TCellTable.Destroy;
begin
	ControlList.Free;
	inherited;
end;

procedure TCellTable.AddControl(inControl: TControl; const inRect: TRect;
	inIndex: Integer);
begin
	ControlList.Add(TCellControl.Create(inControl, inRect, inIndex));
end;

	function CompareCellEdges(Item1, Item2: Pointer): Integer;
	begin
		Result := TCellEdge(Item1).Pos - TCellEdge(Item2).Pos;
	end;

function TCellTable.BuildSortedEdgeList(inAxis: TEdgeAxis;
	inSize: Integer = 0): TObjectList;

	function EdgeOf(inControl: TCellControl; inAxis: TEdgeAxis;
		inEdgeSide: TEdgeSide): Integer;
	var
		r: TRect;
	begin
		r := inControl.CellRect;
		case inAxis of
			eaX:
				case inEdgeSide of
					esStart: Result := r.Left;
					else Result := r.Right;
				end;
			else
				case inEdgeSide of
					esStart: Result := r.Top;
					else Result := r.Bottom;
				end;
		end;
	end;

	procedure CalcEdgeSpans(inList: TList);
	var
		i, p: Integer;
	begin
		p := 0;
		for i := 0 to inList.Count - 1 do
			with TCellEdge(inList[i]) do
			begin
				Span := Pos - p;
				p := Pos;
			end;
	end;

	procedure RemoveCoincidentEdges(inList: TList);
	var
		i: Integer;
	begin
		i := 1;
		while (i < inList.Count) do
		begin
			if TCellEdge(inList[i-1]).Pos = TCellEdge(inList[i]).Pos then
			begin
				//TCellEdge(inList[i]).Free;
				inList.Delete(i)
			end else
				Inc(i);
		end;
	end;

var
	i: Integer;
begin
	Result := TObjectList.Create;
	with Result do
	begin
		Add(TCellEdge.Create(0));
		Add(TCellEdge.Create(inSize));
		for i := 0 to ControlList.Count - 1 do
		begin
			Add(TCellEdge.Create(EdgeOf(ControlList[i], inAxis, esStart)));
			Add(TCellEdge.Create(EdgeOf(ControlList[i], inAxis, esEnd)));
		end;
		Sort(CompareCellEdges);
	end;
	RemoveCoincidentEdges(Result);
	CalcEdgeSpans(Result);
end;

procedure TCellTable.BuildGrid(inXL, inYL: TList);

	function FindCellEdge(inList: TList; inPos: Integer): Integer;
	var
		i: Integer;
	begin
		Result := 0;
		for i := 0 to inList.Count - 1 do
			with TCellEdge(inList[i]) do
				if Pos = inPos then
				begin
					Result := i + 1;
					exit;
				end;
	end;

var
	n, i, j: Integer;
	c: TCellControl;
	x0, y0, x1, y1: Integer;
	r: TRect;
begin
	SetLength(Grid, inYL.Count + 1);
	for j := 0 to inYL.Count do SetLength(Grid[j], inXL.Count + 1);
	//
	for n := 0 to ControlList.Count - 1 do
	begin
		c := TCellControl(ControlList[n]);
		r := c.CellRect;
		//
		x0 := FindCellEdge(inXL, r.Left);
		y0 := FindCellEdge(inYL, r.Top);
		x1 := FindCellEdge(inXL, r.Right);
		y1 := FindCellEdge(inYL, r.Bottom);
		//
		for j := y0 to y1 - 1 do
			for i := x0 to x1 - 1 do
				Grid[j][i] := n + 1;
	end;
end;

procedure TCellTable.BuildRow0(inXlist: TList);
var
	i, col, w: Integer;
begin
	col := 0;
	SetLength(Rows[0], inXlist.Count);
	for i := 0 to inXlist.Count - 1 do
	begin
		w := TCellEdge(inXlist[i]).Span;
		if w = 0 then
			continue;
		Rows[0][col] := CellData(w, 0, 1, 1, -1);
		Inc(col);
	end;
	SetLength(Rows[0], col);
end;

function TCellTable.CollapseCols(inI, inJ: Integer; inList: TList;
	var ioW: Integer): Integer;
var
	gc, si: Integer;
begin
	Result := 1;
	gc := Grid[inJ][inI];
	//
	if (gc <= 0) then
		exit;
	//
	si := inI + 1;
	while (si < inList.Count) and (Grid[inJ][si] = gc) do
	begin
		Grid[inJ][si] := -1;
		ioW := ioW + TCellEdge(inList[si]).Span;
		Inc(Result);
		Inc(si);
	end;
end;

function TCellTable.CollapseRows(inI, inJ, inColSpan: Integer; inList: TList;
	var ioH: Integer): Integer;
var
	gc, sj, k: Integer;
begin
	Result := 1;
	gc := Grid[inJ][inI];
	//
	if (gc <= 0) then
		exit;
	//
	sj := inJ + 1;
	while (sj < inList.Count) and (Grid[sj][inI] = gc) do
	begin
		Grid[sj][inI] := -1;
		for k := 1 to inColSpan - 1 do
			Grid[sj][inI+k] := -1;
		ioH := ioH + TCellEdge(inList[sj]).Span;
		Inc(Result);
		Inc(sj);
	end;
end;

procedure TCellTable.BuildRows(inXlist, inYlist: TList);
var
	i, j, col, row, h, w: Integer;
	gc, col_span, row_span, span_w, span_h: Integer;
	//c: TControl;
	c: Integer;
begin
	SetLength(Rows, inYlist.Count + 1);
	//
	row := 0;
	//
	//BuildRow0(inXlist);
	//row := 1;
	//
	for j := 0 to inYlist.Count - 1 do
	begin
		h := TCellEdge(inYlist[j]).Span;
		if (h = 0) then
			continue;
		//
		col := 0;
		SetLength(Rows[row], inXlist.Count);
		//
		for i := 0 to inXlist.Count - 1 do
		begin
			w := TCellEdge(inXlist[i]).Span;
			if w = 0 then
				continue;
			//
			gc := Grid[j][i];
			if (gc >= 0) then
			begin
				span_w := w;
				span_h := h;
				//
				col_span := CollapseCols(i, j, inXlist, span_w);
				row_span := CollapseRows(i, j, col_span, inYlist, span_h);
				//
				if (gc > 0) then
					c := TCellControl(ControlList[gc - 1]).Index
				else
					c := -1;
				//
				Rows[row][col] := CellData(span_w, span_h, col_span, row_span, c);
				//
				Inc(col);
			end;
		end;
		//
		SetLength(Rows[row], col);
		Inc(row);
	end;
	//
	SetLength(Rows, row);
end;

procedure TCellTable.ClearGrid;
var
	cj, j: Integer;
begin
	cj := Length(Grid);
	for j := 0 to cj - 1 do
		Grid[j] := nil;
	Grid := nil;
end;

procedure TCellTable.Generate(inW, inH: Integer);
var
	xlist, ylist: TList;
begin
	try
		xlist := BuildSortedEdgeList(eaX, inW);
		try
			ylist := BuildSortedEdgeList(eaY, inH);
			try
				BuildGrid(xlist, ylist);
				BuildRows(xlist, ylist);
			finally
				ylist.Free;
			end;
		finally
			xlist.Free;
		end;
	finally
		ClearGrid;
	end;
end;

function TCellTable.ColCount: Integer;
begin
	if RowCount > 0 then
		Result := Length(Rows[0])
	else
		Result := 0;
end;

function TCellTable.RowCount: Integer;
begin
	Result := Length(Rows);
end;

end. 
