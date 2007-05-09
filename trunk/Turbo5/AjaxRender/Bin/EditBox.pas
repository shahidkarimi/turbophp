unit EditBox;

interface

uses
	SysUtils, Types, Classes, Graphics,
	Style, Node, Box, Flow;

type
	TRefNode = class(TNode)
	public
		Ref: TNode;
		Text: string;
	end;
	//
	TTextBox = class(TBox)
		Text: string;
	end;
	//
	TEditBox = class(TBox)
	protected
		function FindCursorInBox(var inCursor: Integer; inBox: TBox): Boolean;
		procedure PaintLine(inLineBox: TBox);
		procedure ResetLines;
	public
		Canvas: TCanvas;
		CursorIndex: Integer;
		CursorBox: TBox;
		Lines: TBoxList;
		function FindCaret(inCursor: Integer): TPoint;
		procedure FindCursor(inCursor: Integer);
		procedure PaintLines;
		procedure WrapLines(inW: Integer);
	end;

implementation

uses
	Text;

procedure TEditBox.ResetLines;
begin
	Lines.Free;
	Lines := TBoxList.Create;
end;

procedure TEditBox.WrapLines(inW: Integer);
var
	i: Integer;
	w: Integer;
	line: TBox;

	procedure NewLine;
	begin
		line := Lines.AddBox;
		w := inW;
	end;

	procedure WrapText(const inNode: TNode);
	var
		t: string;
		b: Integer;
		node: TNode;
	begin
		t := inNode.Text;
		while (t <> '') do
		begin
			//node := TRefNode.Create;
			//node.Ref := inNode;
			node := TNode.Create;
			line.AddBox.Node := node;
			b := GetWordBreak(Canvas, w, t);
			if (b >= 0) then
			begin
				node.Text := Copy(t, 1, b);
				t := Copy(t, b + 1, MAXINT);
				NewLine;
			end else
			begin
				node.Text := t;
				w := w - Canvas.TextWidth(t);
				t := '';
			end;
		end;
	end;

begin
	ResetLines;
	NewLine;
	for i := 0 to Boxes.Max do
		WrapText(Boxes[i].Node);
end;

procedure TEditBox.PaintLine(inLineBox: TBox);
var
	p: TPoint;

	procedure PaintBox(inBox: TBox);
	begin
		Canvas.TextOut(p.X, p.Y, inBox.Node.Text);
		Inc(p.X, Canvas.TextWidth(inBox.Node.Text));
	end;

var
	i: Integer;
begin
	p := Point(inLineBox.Left, inLineBox.Top);
	for i := 0 to inLineBox.Boxes.Max do
		PaintBox(inLineBox.Boxes[i]);
end;

procedure TEditBox.PaintLines;
var
	p: TPoint;
	i: Integer;
begin
	p := Point(Left, Top);
	for i := 0 to Lines.Max do
	begin
		with Lines[i] do
		begin
			Left := 0;
			Top := p.Y;
		end;
		PaintLine(Lines[i]);
		Inc(p.Y, 16);
	end;
end;

function TEditBox.FindCaret(inCursor: Integer): TPoint;
var
	pos: TPoint;

	function FindCaretInBox(inBox: TBox): Boolean;
	var
		l: Integer;
	begin
		l := Length(inBox.Node.Text);
		if l > inCursor then
		begin
			Inc(pos.X, Canvas.TextWidth(Copy(inBox.Node.Text, 1, inCursor)));
			Result := true;
			exit;
		end;
		Dec(inCursor, l);
		Inc(pos.X, Canvas.TextWidth(inBox.Node.Text));
		Result := false;
	end;

	function FindCaretInLine(inBox: TBox): Boolean;
	var
		i: Integer;
	begin
		Result := true;
		pos.X := inBox.Left;
		for i := 0 to inBox.Boxes.Max do
			if FindCaretInBox(inBox.Boxes[i]) then
				exit;
		Result := false;
	end;

var
	i: Integer;
begin
	pos := Point(Left, Top);
	for i := 0 to Lines.Max do
	begin
		if FindCaretInLine(Lines[i]) then
			break;
		Inc(pos.Y, 16);
	end;
	Result := pos;
end;

function TEditBox.FindCursorInBox(var inCursor: Integer; inBox: TBox): Boolean;

	function FindCursorInBoxes(inBox: TBox): Boolean;
	var
		i: Integer;
	begin
		Result := false;
		if (inBox.Boxes <> nil) then
			for i := 0 to inBox.Boxes.Max do
				Result := FindCursorInBox(inCursor, inBox.Boxes[i]);
	end;

var
	l: Integer;
begin
	l := Length(inBox.Node.Text);
	if l > inCursor then
	begin
		CursorIndex := inCursor;
		CursorBox := inBox;
		Result := true;
		exit;
	end;
	Dec(inCursor, l);
	Result := FindCursorInBoxes(inBox);
end;

procedure TEditBox.FindCursor(inCursor: Integer);
var
	i: Integer;
begin
	CursorBox := nil;
	for i := 0 to Boxes.Max do
		if FindCursorInBox(inCursor, Boxes[i]) then
			break;
end;

end.
