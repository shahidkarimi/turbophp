unit Render2Test;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls;

type
	TRender2TestForm = class(TForm)
		PaintBox: TPaintBox;
		procedure PaintBoxPaint(Sender: TObject);
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
	Render2TestForm: TRender2TestForm;

implementation

uses
	Layout, Render3, Nodes;

{$R *.dfm}

	function BuildNodeList: TNodeList;
	var
		node, nd: TNode;
	begin
		Result := TNodeList.Create;
		//
		node := TDivNode.Create;
		node.Text := 'Hello Top Div!';
		Result.Add(node);
		//
		node.Nodes := TNodeList.Create;
		with node.Nodes do
		begin
			nd := TInlineNode.Create;
			nd.Text := 'Inline 1';
			nd.Style.BackgroundColor := clLime;
			nd.Style.Height := 100;
			Add(nd);
			//
			nd := TInlineNode.Create;
			nd.Text := 'Inline 2';
			nd.Style.BackgroundColor := clAqua;
			nd.Style.Height := 100;
			Add(nd);
		end;
		//
		node := TDivNode.Create;
		node.Text := 'Hello world. ';
		node.Style.BackgroundColor := clFuchsia;
		node.Style.Height := 100;
		Result.Add(node);
	end;

	function BuildBoxList(inNodes: TNodeList): TBoxList;
	begin
		with TLayout.Create do
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
		b := BuildBoxList(n);
		RenderBoxList(b, inCanvas, inRect);
		b.Free;
		n.Free;
	end;

procedure TRender2TestForm.FormCreate(Sender: TObject);
begin
	Show;
end;

procedure TRender2TestForm.PaintBoxPaint(Sender: TObject);
begin
	//TestBlockRenderer(PaintBox.Canvas, PaintBox.ClientRect);
	//TestBoxRenderer(PaintBox.Canvas, PaintBox.ClientRect);
	TestLayoutRenderer(PaintBox.Canvas, PaintBox.ClientRect);
end;

end.
