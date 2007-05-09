unit Render3Test;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls,
	pngimage,
	Node, Box
	{Layout};

type
	TRender3TestForm = class(TForm)
		BoxMemo: TMemo;
		Panel: TPanel;
		PaintBox: TPaintBox;
		Splitter1: TSplitter;
		DOMMemo: TMemo;
		Splitter3: TSplitter;
		Image1: TImage;
		procedure FormCreate(Sender: TObject);
		procedure FormResize(Sender: TObject);
		procedure PaintBoxPaint(Sender: TObject);
	private
		{ Private declarations }
		procedure DoDumpNodes(inNodes: TNodeList; inDent: string);
		procedure DumpBoxes(inBoxes: TBoxList; inDent: string);
		procedure DumpBoxStructure;
		procedure DumpDOMStructure;
	public
		{ Public declarations }
	end;

var
	Render3TestForm: TRender3TestForm;

implementation

uses
	{Render3, Nodes, LayoutTest} FlowTest;

var
	N: TNodeList;
	B: TBoxList;

{$R *.dfm}

procedure TRender3TestForm.FormCreate(Sender: TObject);
begin
	N := BuildNodeList;
	DumpDOMStructure;
	Show;
end;

procedure TRender3TestForm.FormResize(Sender: TObject);
begin
	//R.Width := ClientWidth - 32;
	B.Free;
	B := BuildBoxList(PaintBox.Canvas, N);
	DumpBoxStructure;
	PaintBox.Invalidate;
end;

procedure TRender3TestForm.PaintBoxPaint(Sender: TObject);
begin
	//RenderBoxList(B, PaintBox.Canvas, PaintBox.ClientRect);
end;

procedure TRender3TestForm.DoDumpNodes(inNodes: TNodeList; inDent: string);
var
	i: Integer;
	node: TNode;
	s: string;
begin
	for i := 0 to Pred(inNodes.Count) do
	begin
		node := inNodes[i];
		if (node <> nil) then
		begin
			s := Format('%s| (%d) [%s: %s] => [%s] ',
				[ inDent, i, {node.Element}'', node.ClassName, node.Text ]);
			DOMMemo.Lines.Add(s);
			if node.Nodes <> nil then
				DoDumpNodes(node.Nodes, inDent + '--');
		end;
	end;
end;

procedure TRender3TestForm.DumpDOMStructure;
begin
	DOMMemo.Lines.BeginUpdate;
	try
		DOMMemo.Lines.Clear;
		DOMMemo.Lines.Add('DOM Structure:');
		DoDumpNodes(N, '');
	finally
		DOMMemo.Lines.EndUpdate;
	end;
end;

procedure TRender3TestForm.DumpBoxes(inBoxes: TBoxList; inDent: string);
var
	i: Integer;
	box: TBox;
	s, t: string;
begin
	if inBoxes <> nil then
		for i := 0 to Pred(inBoxes.Count) do
		begin
			box := inBoxes[i];
			if (box <> nil) then
			begin
				t := '';
				if box.Node <> nil then
					t := box.Node.Text;
				s := Format('%s| (%d) [%s] [%d, %d => %d, %d] [%d, %d] => [%s] ',
					[ inDent, i, {box.Name}'', box.Left, box.Top, box.Width, box.Height,
						box.Offset.X, box.Offset.Y, t ]);
				//if (box.Node <> nil) then
				//	s := s + '(' + box.className + ')';
				BoxMemo.Lines.Add(s);
				DumpBoxes(box.Boxes, inDent + '--');
			end;
		end;
end;

procedure TRender3TestForm.DumpBoxStructure;
begin
	try
		BoxMemo.Lines.BeginUpdate;
		BoxMemo.Lines.Clear;
		BoxMemo.Lines.Add('Box Structure:');
		DumpBoxes(B, '');
	finally
		BoxMemo.Lines.EndUpdate;
	end;
end;

end.
