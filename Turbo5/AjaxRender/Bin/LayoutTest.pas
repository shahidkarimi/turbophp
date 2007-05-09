unit LayoutTest;

interface

uses
	Types, Graphics,
	Layout, Render3, Nodes;

function BuildNodeList: TNodeList;
function BuildBoxList(inCanvas: TCanvas; inNodes: TNodeList): TBoxList;
procedure RenderBoxList(inBoxes: TBoxList; inCanvas: TCanvas; inRect: TRect);
procedure TestLayoutRenderer(inCanvas: TCanvas;	inRect: TRect);

implementation

function BuildNodeList: TNodeList;
var
	node, nd: TNode;
begin
	Result := TNodeList.Create;
	//
	node := TDivNode.Create;
	node.Text := 'Hello Top Div! Hello Top Div! Hello Top Div! Hello Top Div!';
	node.Style.BackgroundColor := clYellow;
	node.Style.Width := 300;
	Result.Add(node);
	//
	node.Nodes := TNodeList.Create;
	with node.Nodes do
	begin
		nd := TInlineNode.Create;
		nd.Text := 'Inline 1';
		nd.Style.BackgroundColor := clLime;
		Add(nd);
		//
		nd := TInlineNode.Create;
		nd.Text := 'Inline 2';
		nd.Style.BackgroundColor := clAqua;
		Add(nd);
		//
		nd := TInlineNode.Create;
		nd.Text := 'Inline 3';
		nd.Style.BackgroundColor := clAqua;
		Add(nd);
	end;
	//
	node := TDivNode.Create;
	node.Text := 'Hello world. ';
	node.Style.BackgroundColor := clFuchsia;
	node.Style.Width := 100;
	node.Style.Height := 100;
	Result.Add(node);
end;

function BuildBoxList(inCanvas: TCanvas; inNodes: TNodeList): TBoxList;
begin
	with TLayout.Create(inCanvas) do
	try
		Result := BuildBoxes(inNodes);
	finally
		Free;
	end;
end;

procedure RenderBoxList(inBoxes: TBoxList; inCanvas: TCanvas;
	inRect: TRect);
begin
	with TRenderer.Create do
	try
		Render(inBoxes, inCanvas, inRect);
	finally
		Free;
	end;
end;

procedure TestLayoutRenderer(inCanvas: TCanvas;	inRect: TRect);
var
	n: TNodeList;
	b: TBoxList;
begin
	n := BuildNodeList;
	b := BuildBoxList(inCanvas, n);
	RenderBoxList(b, inCanvas, inRect);
	b.Free;
	n.Free;
end;

end.
