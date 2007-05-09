unit RenderTest;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, Render, ExtCtrls, pngimage;

type
	TRenderTestForm = class(TForm)
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
		procedure BuildRenderer;
		procedure BuildTestNodes;
		procedure DoDumpNodes(inNodes: TNodeList; inDent: string);
		procedure DumpBoxes(inBoxes: TBoxList; inDent: string);
		procedure DumpBoxStructure;
		procedure DumpDOMStructure;
		procedure RenderBoxes;
	public
		{ Public declarations }
	end;

var
	RenderTestForm: TRenderTestForm;

implementation

var
	R: TRenderer;
	N: TNodeList;

{$R *.dfm}

procedure TRenderTestForm.FormCreate(Sender: TObject);
begin
	BuildRenderer;
	BuildTestNodes;
	DumpDOMStructure;
	Show;
end;

procedure TRenderTestForm.BuildRenderer;
begin
	R := TRenderer.Create;
	R.Canvas := Canvas;
end;

procedure TRenderTestForm.BuildTestNodes;
var
	node, nd: TNode;
begin
	N := TNodeList.Create;
	node := TDivNode.Create;
	N.Add(node);
	with node.Nodes do
	begin
		nd := TTextNode.Create;
		nd.Text := 'Hello Top Div!';
		Add(nd);
	end;
	//
	node := TTextNode.Create;
	node.Text := 'Hello world. ';
	N.Add(node);
	//
	node := TTextNode.Create;
	node.Text :=
			'During a visit at the home of human spaceflight, he spoke'
		+ 'Tuesday with astronauts, flight directors and other top administrators. '
			;
	node.Style.BackgroundColor := clWhite;
	N.Add(node);
	//
	node := TImageNode.Create;
	TImageNode(node).Picture := Image1.Picture;
	N.Add(node);
	//
	node := TTextNode.Create;
	node.Text :=
		'Griffin said the agency has received a steady flow of funding that, when '
		+ 'adjusted for inflation, is comparable to the funding the agency had when '
		+ 'it first sent astronauts to the moon during the Apollo program of the'
		+ '1960s and early 1970s.'
		;
	node.Style.Color := clRed;
	N.Add(node);
	//
	node := TDivNode.Create;
	N.Add(node);
	with node.Nodes do
	begin
		nd := TTextNode.Create;
		nd.Text := 'Hello Div!';
		nd.Style.BackgroundColor := clBtnFace;
		nd.Style.Color := clBlack;
		Add(nd);
	end;
	//
	node := TDivNode.Create;
	N.Add(node);
	with node.Nodes do
	begin
		nd := TTextNode.Create;
		nd.Text := 'Hello Div 2!';
		nd.Style.BackgroundColor := clWhite;
		nd.Style.Color := clBlack;
		Add(nd);
	end;
end;

procedure TRenderTestForm.DumpBoxes(inBoxes: TBoxList; inDent: string);
var
	i: Integer;
	box: TBox;
	s: string;
begin
	for i := 0 to Pred(inBoxes.Count) do
	begin
		box := inBoxes[i];
		if (box <> nil) then
		begin
			s := Format('%s| box %d: %s - [%s] ', [ inDent, i, box.Name, box.Text ]);
			if (box.Node <> nil) then
				s := s + '(' + box.className + ')';
			BoxMemo.Lines.Add(s);
			DumpBoxes(box.Boxes, inDent + '--');
		end;
	end;
end;

procedure TRenderTestForm.DumpBoxStructure;
var
	boxes: TBoxList;
begin
	boxes := R.BuildBoxes(N);
	try
		BoxMemo.Lines.BeginUpdate;
		BoxMemo.Lines.Clear;
		BoxMemo.Lines.Add('Box Structure:');
		DumpBoxes(boxes, '');
	finally
		BoxMemo.Lines.EndUpdate;
		boxes.Free;
	end;
end;

procedure TRenderTestForm.DoDumpNodes(inNodes: TNodeList; inDent: string);
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
				[ inDent, i, node.Element, node.ClassName, node.Text ]);
			DOMMemo.Lines.Add(s);
			DoDumpNodes(node.Nodes, inDent + '--');
		end;
	end;
end;

procedure TRenderTestForm.DumpDOMStructure;
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

procedure TRenderTestForm.RenderBoxes;
var
	boxes: TBoxList;
begin
	boxes := R.BuildBoxes(N);
	try
		R.Canvas := PaintBox.Canvas;
		R.Width := Panel.ClientWidth;
		R.RenderBoxes(boxes);
	finally
		boxes.Free;
	end;
end;

procedure TRenderTestForm.FormResize(Sender: TObject);
begin
	R.Width := ClientWidth - 32;
	DumpBoxStructure;
	PaintBox.Invalidate;
end;

procedure TRenderTestForm.PaintBoxPaint(Sender: TObject);
begin
	RenderBoxes;
end;

end.
