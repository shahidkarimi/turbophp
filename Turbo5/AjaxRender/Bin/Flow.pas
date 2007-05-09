unit Flow;

interface

uses
	SysUtils, Types, Graphics,
	Node, Box;

type
	TBlockContext = class(TContextBase)
	private
		Box: TBox;
	public
		constructor Create(inBox: TBox; inNode: TNode);
		procedure AddNodes(inNodes: TNodeList);
	end;
	//
	TInlineContext = class(TContextBase)
	private
		Context: TBlockContext;
		Line: TBox;
	public
		constructor Create(inContext: TBlockContext);
		procedure AddNode(inNode: TNode);
	end;
	//
	TFlow = class
	private
		InlineContext: TInlineContext;
	protected
		procedure AddBlock(inBox: TBox; inNode: TNode);
		procedure AddNode(inContext: TBlockContext; inNode: TNode);
		procedure AddInline(inContext: TBlockContext; inNode: TNode);
		procedure OpenInlineContext(inContext: TBlockContext);
	public
		Canvas: TCanvas;
		constructor Create(inBox: TBox; inNode: TNode; inCanvas: TCanvas);
		destructor Destroy; override;
		procedure CloseInlineContext;
	end;
	//
	TBlockBox = class(TBox)
	public
		procedure MeasureBox(inBox: TBox); override;
	end;
	//
	TLineBox = class(TBox)
	public
		procedure MeasureBox(inBox: TBox); override;
	end;
	//
	TRenderNode = class(TBoxNode)
	public
		procedure Render(inBox: TBox; inCanvas: TCanvas; inRect: TRect); override;
	end;
	//
	TBlockNode = class(TRenderNode)
	public
		class function GetContextClass: TContextClass; override;
		procedure Measure(inBox: TBox); override;
	end;
	//
	TInlineNode = class(TRenderNode)
	public
		class function GetContextClass: TContextClass; override;
		procedure Measure(inBox: TBox); override;
	end;

implementation

var
	SingletonFlow: TFlow;

{ TFlow }

constructor TFlow.Create(inBox: TBox; inNode: TNode; inCanvas: TCanvas);
begin
	SingletonFlow := Self;
	Canvas := inCanvas;
	AddBlock(inBox, inNode);
	inBox.Measure;
end;

destructor TFlow.Destroy;
begin
	CloseInlineContext;
	inherited;
end;

procedure TFlow.CloseInlineContext;
begin
	FreeAndNil(InlineContext);
end;

procedure TFlow.OpenInlineContext(inContext: TBlockContext);
begin
	if InlineContext = nil then
		InlineContext := TInlineContext.Create(inContext);
end;

procedure TFlow.AddNode(inContext: TBlockContext; inNode: TNode);
begin
	if inNode.ContextClass = TBlockContext then
		AddBlock(inContext.Box.AddBox(TBlockBox), inNode)
	else
		AddInline(inContext, inNode);
end;

procedure TFlow.AddBlock(inBox: TBox; inNode: TNode);
begin
	CloseInlineContext;
	TBlockContext.Create(inBox, inNode).Free;
end;

procedure TFlow.AddInline(inContext: TBlockContext; inNode: TNode);
begin
	OpenInlineContext(inContext);
	InlineContext.AddNode(inNode);
end;

{ TBlockContext }

constructor TBlockContext.Create(inBox: TBox; inNode: TNode);
begin
	Box := inBox;
	Box.Node := inNode;
	AddNodes(inNode.Nodes);
	SingletonFlow.CloseInlineContext;
end;

procedure TBlockContext.AddNodes(inNodes: TNodeList);
var
	i: Integer;
begin
	if (inNodes <> nil) then
		for i := 0 to inNodes.Max do
			SingletonFlow.AddNode(Self, inNodes[i]);
end;

{ TInlineContext }

constructor TInlineContext.Create(inContext: TBlockContext);
begin
	Context := inContext;
	Line := Context.Box.AddBox(TLineBox);
end;

procedure TInlineContext.AddNode(inNode: TNode);
begin
	Line.AddBox(TLineBox).Node := inNode;
	//Line.NewChild.Node := inNode;
	Context.AddNodes(inNode.Nodes);
end;

{ TBlockBox }

procedure TBlockBox.MeasureBox(inBox: TBox);
begin
	inherited;
	Inc(Height, inBox.Height);
end;

{ TLineBox }

procedure TLineBox.MeasureBox(inBox: TBox);
begin
	inherited;
	if (inBox.Height > Height) then
	begin
		Height := inBox.Height;
		Offset := Point(0, Height);
	end;
end;

{ TRenderNode }

procedure TRenderNode.Render(inBox: TBox; inCanvas: TCanvas; inRect: TRect);
begin
	if (Style.BackgroundColor <> clDefault) then
	begin
		inCanvas.Brush.Style := bsSolid;
		inCanvas.Brush.Color := Style.BackgroundColor;
		inCanvas.FillRect(inRect);
	end;
	if (Style.Color <> clDefault) then
		inCanvas.Font.Color := Style.Color;
	inCanvas.TextOut(inRect.Left, inRect.Top, Text);
	inCanvas.Brush.Style := bsClear;
	inCanvas.Rectangle(inRect);
end;

{ TBlockNode }

class function TBlockNode.GetContextClass: TContextClass;
begin
	Result := TBlockContext;
end;

procedure TBlockNode.Measure(inBox: TBox);
begin
	if Style.Width > 0 then
		inBox.Width := Style.Width;
	if Style.Height > 0 then
		inBox.Height := Style.Height;
	if Style.Left > 0 then
		inBox.Left := Style.Left;
	inBox.Offset := Point(0, inBox.Height);
end;

{ TInlineNode }

class function TInlineNode.GetContextClass: TContextClass;
begin
	Result := TInlineContext;
end;

procedure TInlineNode.Measure(inBox: TBox);
begin
	with SingletonFlow.Canvas do
	begin
		inBox.Width := TextWidth(Text);
		inBox.Height := TextHeight(Text);
	end;
	inBox.Offset := Point(inBox.Width, 0);
end;

end.
