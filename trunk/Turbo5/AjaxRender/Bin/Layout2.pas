unit Layout2;

interface

uses
	SysUtils, Node, Box;

type
	TBlockContext = class(TBoxContext)
	private
		Boxes: TBoxList;
	public
		constructor Create(inBox: TBox; inNode: TNode);
		procedure AddNodes(inNodes: TNodeList);
	end;
	//
	TInlineContext = class(TBoxContext)
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
	public
		constructor Create(inBox: TBox; inNode: TNode);
		destructor Destroy; override;
	end;

implementation

var
	Flow: TFlow;

{ TFlow }

constructor TFlow.Create(inBox: TBox; inNode: TNode);
begin
	Flow := Self;
	AddBlock(inBox, inNode);
	Free;
end;

destructor TFlow.Destroy;
begin
	FreeAndNil(InlineContext);
	inherited;
end;

procedure TFlow.AddNode(inContext: TBlockContext; inNode: TNode);
begin
	if inNode.ContextClass = TBlockContext then
		AddBlock(inContext.Boxes.NewBox, inNode)
	else
		AddInline(inContext, inNode);
end;

procedure TFlow.AddBlock(inBox: TBox; inNode: TNode);
begin
	FreeAndNil(InlineContext);
	TBlockContext.Create(inBox, inNode);
end;

procedure TFlow.AddInline(inContext: TBlockContext; inNode: TNode);
begin
	if InlineContext = nil then
		InlineContext := TInlineContext.Create(inContext);
	InlineContext.AddNode(inNode);
end;

{ TBlockContext }

constructor TBlockContext.Create(inBox: TBox; inNode: TNode);
begin
	inBox.Node := inNode;
	if (inNode.Nodes <> nil) and (inNode.Nodes.Count > 0) then
	begin
		Boxes := inBox.CreateBoxList;
		AddNodes(inNode.Nodes);
	end;
	Free;
end;

procedure TBlockContext.AddNodes(inNodes: TNodeList);
var
	i: Integer;
begin
	for i := 0 to inNodes.Max do
		Flow.AddNode(Self, inNodes[i]);
end;

{ TInlineContext }

constructor TInlineContext.Create(inContext: TBlockContext);
begin
	Context := inContext;
	Line := Context.Boxes.NewBox;
	Line.CreateBoxList;
end;

procedure TInlineContext.AddNode(inNode: TNode);
begin
	Line.Boxes.NewBox.Node := inNode;
	Context.AddNodes(inNode.Nodes);
end;

end.
