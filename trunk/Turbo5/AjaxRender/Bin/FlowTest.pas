unit FlowTest;

interface

uses
	Types, Graphics,
	Node, Box, Flow, Render4;

function BuildNode: TNode;
function BuildBox(inCanvas: TCanvas; inNode: TNode): TBox;
procedure RenderBox(inBox: TBox; inCanvas: TCanvas; inRect: TRect);

implementation

function BlockNode(inNode: TNode; inBackColor: TColor; inW: Integer;
	inH: Integer = 0): TNode;
begin
	inNode.Style.BackgroundColor := inBackColor;
	inNode.Style.Width := inW;
	inNode.Style.Height := inH;
	Result := inNode;
end;

function InlineNode(inNode: TNode; const inText: string;
	inBackColor: TColor): TNode;
begin
	inNode.Text := inText;
	inNode.Style.BackgroundColor := inBackColor;
	Result := inNode;
end;

function BuildNode: TNode;
begin
	Result := TNode.Create;
	with Result do
	begin
		with BlockNode(Add(TBlockNode), clYellow, 300, 60) do
		begin
			InlineNode(Add(TInlineNode), 'Inline 1', clLime);
			with InlineNode(Add(TInlineNode), 'Inline 2', clAqua) do
				BlockNode(Add(TBlockNode), clBlue, 300);
			InlineNode(Add(TInlineNode), 'Inline 3', clLime);
		end;
		//
		with BlockNode(Add(TBlockNode), clFuchsia, 300, 80) do
		begin
			InlineNode(Add(TInlineNode), 'Inline 1', clLime);
			with InlineNode(Add(TInlineNode), 'Inline 2', clAqua) do
				with BlockNode(Add(TBlockNode), clBlue, 300) do
				begin
					Style.Left := 50;
					InlineNode(Add(TInlineNode), 'Inline 1', clLime);
					InlineNode(Add(TInlineNode), 'Inline 2', clAqua);
					InlineNode(Add(TInlineNode), 'Inline 3', clLime);
				end;
		InlineNode(Add(TInlineNode), 'Inline 3', clLime);
		end;
	end;
end;

function BuildBox(inCanvas: TCanvas; inNode: TNode): TBox;
begin
	Result := TBox.Create;
	TFlow.Create(Result, inNode, inCanvas).Free;
end;

procedure RenderBox(inBox: TBox; inCanvas: TCanvas; inRect: TRect);
begin
	TRenderer.Create(inBox.Boxes, inCanvas, inRect).Free;
end;

end.
