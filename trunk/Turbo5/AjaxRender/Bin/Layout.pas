unit Layout;

interface

uses
	Types, Classes, Graphics, Style;

type
	TNode = class;
	TNodeList = class;
	TBox = class;
	TBoxList = class;
	TContext = class;
	TContextClass = class of TContext;
	TLayout = class;
	//
	TBox = class
	public
		Node: TNode;
		Left: Integer;
		Top: Integer;
		Width: Integer;
		Height: Integer;
		Offset: TPoint;
		Boxes: TBoxList;
{
		Name: string;
		Text: string;
		Context: TDisplay;
}
		constructor Create;
		destructor Destroy; override;
	end;
	//
	TBoxList = class(TList)
	private
		function GetBox(inIndex: Integer): TBox;
		procedure SetBox(inIndex: Integer; const Value: TBox);
		function GetMax: Integer;
	public
		function NewBox: TBox;
		property Box[inIndex: Integer]: TBox read GetBox write SetBox; default;
		property Max: Integer read GetMax;
	end;
	//
	TNode = class
		class function GetContextClass: TContextClass; virtual;
	public
		Nodes: TNodeList;
		Style: TStyle;
		Text: string;
		constructor Create; virtual;
		destructor Destroy; override;
{
		Element: string;
		// attributes
		// styles
		function NextBox(inContext: TContext;
			var inBox: TBox): Boolean; virtual; abstract;
		procedure InitBoxes; virtual; abstract;
		procedure Render(inRenderer: TRenderer;
			const inBox: TBox); virtual; abstract;
}
		procedure InitBox(inLayout: TLayout; var inBox: TBox); virtual;
		property ContextClass: TContextClass read GetContextClass;
	end;
	//
	TNodeList = class(TList)
	protected
		function GetMax: Integer;
		function GetNode(inIndex: Integer): TNode;
		procedure SetNode(inIndex: Integer; const Value: TNode);
	public
		property Max: Integer read GetMax;
		property Node[inIndex: Integer]: TNode read GetNode write SetNode; default;
	end;
	//
	TContext = class
	private
		Layout: TLayout;
		Boxes: TBoxList;
	protected
		function AdjustSize(inSize: TPoint; inBox: TBox): TPoint; virtual; abstract;
		function BuildBox(inNode: TNode): TBox; virtual;
	public
		function BuildBoxes(inNodes: TNodeList; inIndex: Integer): Integer;
		procedure MeasureBox(inBox: TBox);
		constructor Create(inLayout: TLayout; inBoxes: TBoxList);
	end;
	//
	TBlockContext = class(TContext)
	protected
		function AdjustSize(inSize: TPoint; inBox: TBox): TPoint; override;
		function BuildBox(inNode: TNode): TBox; override;
	end;
	//
	TInlineContext = class(TContext)
	protected
		function AdjustSize(inSize: TPoint; inBox: TBox): TPoint; override;
		function BuildBox(inNode: TNode): TBox; override;
	end;
	//
	TLayout = class
	protected
		function BuildContextBoxes(inContextClass: TContextClass;
			inNodes: TNodeList; inIndex: Integer; inBoxes: TBoxList): Integer;
	public
		Canvas: TCanvas;
		constructor Create(inCanvas: TCanvas);
		function BuildBoxes(inNodes: TNodeList): TBoxList;
	end;

implementation

uses
	Math;

{ TBox }

constructor TBox.Create;
begin
end;

destructor TBox.Destroy;
begin
	Boxes.Free;
	inherited;
end;

{ TBoxList }

function TBoxList.NewBox: TBox;
begin
	Result := TBox.Create;
	Add(Result);
end;

function TBoxList.GetBox(inIndex: Integer): TBox;
begin
	Result := TBox(Items[inIndex]);
end;

function TBoxList.GetMax: Integer;
begin
	Result := Pred(Count);
end;

procedure TBoxList.SetBox(inIndex: Integer; const Value: TBox);
begin
	Items[inIndex] := Value;
end;

{ TNode }

class function TNode.GetContextClass: TContextClass;
begin
	Result := TBlockContext;
end;

constructor TNode.Create;
begin
	Style := TStyle.Create;
end;

destructor TNode.Destroy;
begin
	Style.Free;
	inherited;
end;

procedure TNode.InitBox(inLayout: TLayout; var inBox: TBox);
begin
	//
end;

{ TNodeList }

function TNodeList.GetMax: Integer;
begin
	Result := Pred(Count);
end;

function TNodeList.GetNode(inIndex: Integer): TNode;
begin
	Result := TNode(Items[inIndex]);
end;

procedure TNodeList.SetNode(inIndex: Integer; const Value: TNode);
begin
	Items[inIndex] := Value;
end;

{ TLayout }

constructor TLayout.Create(inCanvas: TCanvas);
begin
	Canvas := inCanvas;
end;

function TLayout.BuildContextBoxes(inContextClass: TContextClass;
	inNodes: TNodeList; inIndex: Integer; inBoxes: TBoxList): Integer;
begin
	with inContextClass.Create(Self, inBoxes) do
	try
		Result := BuildBoxes(inNodes, inIndex);
	finally
		Free;
	end;
end;

function TLayout.BuildBoxes(inNodes: TNodeList): TBoxList;

	procedure FillBoxes(inBoxes: TBoxList);
	var
		i: Integer;
	begin
		i := 0;
		while (i < inNodes.Count) do
			i := BuildContextBoxes(inNodes[i].ContextClass, inNodes, i, inBoxes);
	end;

begin
	Result := TBoxList.Create;
	if (inNodes <> nil) then
		FillBoxes(Result);
end;

{ TContext }

constructor TContext.Create(inLayout: TLayout; inBoxes: TBoxList);
begin
	Layout := inLayout;
	Boxes := inBoxes;
	//Width := Renderer.Width;
	//SetContext;
end;

function TContext.BuildBoxes(inNodes: TNodeList; inIndex: Integer): Integer;
begin
	while (inIndex < inNodes.Count) and	(Self is inNodes[inIndex].ContextClass) do
	begin
		BuildBox(inNodes[inIndex]);
		Inc(inIndex);
	end;
	Result := inIndex;
end;

function TContext.BuildBox(inNode: TNode): TBox;
begin
	Result := Boxes.NewBox;
	with Result do
	begin
		Node := inNode;
		Boxes := Layout.BuildBoxes(inNode.Nodes);
	end;
	MeasureBox(Result);
	inNode.InitBox(Layout, Result);
end;

procedure TContext.MeasureBox(inBox: TBox);

	function AddPt(inP0, inP1: TPoint): TPoint;
	begin
		Result := Point(inP0.X + inP1.X, inP1.Y + inP1.Y);
	end;

	procedure MeasureBoxes(inBoxes: TBoxList);
	var
		i: Integer;
	begin
		for i := 0 to inBoxes.Max do
			MeasureBox(inBoxes[i]);
	end;

	function CalcBoxSize(inBoxes: TBoxList): TPoint;
	var
		i: Integer;
	begin
		Result := Point(0, 0);
		for i := 0 to inBoxes.Max do
			Result := AddPt(Result, inBoxes[i].Offset);
//			Result := AdjustSize(Result, inBoxes[i]);
	end;

begin
	if {(inBox.Height <= 0) and} (inBox.Boxes.Count > 0) then
	begin
		MeasureBoxes(inBox.Boxes);
		with CalcBoxSize(inBox.Boxes) do
		begin
			inBox.Width := X;
			inBox.Height := Y;
		end;
	end;
end;

{ TBlockContext }

function TBlockContext.AdjustSize(inSize: TPoint; inBox: TBox): TPoint;
begin
	Result.X := inSize.X + inBox.Width;
	Result.Y := inSize.Y + inBox.Height;
end;

function TBlockContext.BuildBox(inNode: TNode): TBox;
begin
	Result := inherited BuildBox(inNode);
{
	with Result do
	begin
		//Height := 100;
		//Width := 300;
		//Offset := Point(0, Height);
	end;
}
end;

{ TInlineContext }

function TInlineContext.AdjustSize(inSize: TPoint; inBox: TBox): TPoint;
begin
	Result.X := inSize.X + inBox.Width;
	Result.Y := Max(inSize.Y, inBox.Height);
end;

function TInlineContext.BuildBox(inNode: TNode): TBox;
begin
	Result := inherited BuildBox(inNode);
{
	with Result do
	begin
		//Height := 100;
		//Width := 50;
		//Offset := Point(Width, 0);
	end;
}
end;

end.
