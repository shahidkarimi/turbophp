unit Render;

interface

uses
	Types, Classes, Graphics, Style;

type
	TNode = class;
	TNodeList = class;
	TBox = class;
	TBoxList = class;
	TContext = class;
	TRenderer = class;
	//
	TBox = class
		Left: Integer;
		Top: Integer;
		Width: Integer;
		Height: Integer;
		Name: string;
		Node: TNode;
		Text: string;
		Boxes: TBoxList;
		Context: TDisplay;
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
		property Box[inIndex: Integer]: TBox read GetBox write SetBox; default;
    property Max: Integer read GetMax;
	end;
	//
	TNode = class
	public
		Element: string;
		Nodes: TNodeList;
		Text: string;
		// attributes
		// styles
		Style: TStyle;
		constructor Create; virtual;
		destructor Destroy; override;
		function NextBox(inContext: TContext;
			var inBox: TBox): Boolean; virtual; abstract;
		procedure InitBoxes; virtual; abstract;
		procedure Render(inRenderer: TRenderer;
			const inBox: TBox); virtual; abstract;
	end;
	//
	TNodeList = class(TList)
	private
		function GetNode(inIndex: Integer): TNode;
		procedure SetNode(inIndex: Integer; const Value: TNode);
	public
		property Node[inIndex: Integer]: TNode read GetNode write SetNode; default;
	end;
	//
	TContext = class
	protected
		Boxes: TBoxList;
		Context: TDisplay;
		Pen: TPoint;
		Renderer: TRenderer;
		Width: Integer;
		function BuildBoxes(inNodes: TNodeList;
			inIndex: Integer): Integer; virtual;
		procedure SetContext; virtual; abstract;
	public
		constructor Create(inRenderer: TRenderer; inBoxes: TBoxList);
		procedure BuildBox(inNode: TNode); virtual; abstract;
	end;
	//
	TContextClass = class of TContext;
	//
	TBlockContext = class(TContext)
	protected
		procedure SetContext; override;
	public
		procedure BuildBox(inNode: TNode); override;
	end;
	//
	TInlineContext = class(TContext)
	protected
		Line: TBox;
		procedure SetContext; override;
		function BuildBoxes(inNodes: TNodeList;
			inIndex: Integer): Integer; override;
		procedure BuildLineBox;
		procedure BuildNodeBoxes(inNode: TNode);
		function FitNodeText(const inText: string): Integer;
	public
		procedure BuildBox(inNode: TNode); override;
	end;
	//
	TRenderer = class
	private
		function BuildContext(inContextClass: TContextClass; inBoxes: TBoxList;
			inNodes: TNodeList; inIndex: Integer): Integer;
		procedure DoRenderBoxes(inBoxes: TBoxList);
		procedure MeasureBox(inBox: TBox);
		procedure MeasureBoxes(inBoxes: TBoxList);
		procedure RenderBox(inBox: TBox);
		procedure RenderNode(inNode: TNode);
		procedure RenderNodes(inNodes: TNodeList);
	public
		Canvas: TCanvas;
		Pen: TPoint;
		Width, Height: Integer;
		function BuildBoxes(inNodes: TNodeList): TBoxList;
		procedure Render(inNodes: TNodeList; inCanvas: TCanvas; inRect: TRect);
		procedure RenderBoxes(inBoxes: TBoxList);
	end;
	//
	TDivNode = class(TNode)
	public
		constructor Create; override;
		function NextBox(inContext: TContext; var inBox: TBox): Boolean; override;
		procedure InitBoxes; override;
		procedure Render(inRenderer: TRenderer;	const inBox: TBox); override;
	end;
	//
	TTextNode = class(TNode)
	private
		Cursor: Integer;
		TextLength: Integer;
	public
		constructor Create; override;
		function NextBox(inContext: TContext; var inBox: TBox): Boolean; override;
		procedure InitBoxes; override;
		procedure Render(inRenderer: TRenderer;	const inBox: TBox); override;
	end;
	//
	TImageNode = class(TNode)
	protected
		function ImgHeight: Integer;
		function ImgWidth: Integer;
	public
		Picture: TPicture;
		Width, Height: Integer;
		constructor Create; override;
		function NextBox(inContext: TContext; var inBox: TBox): Boolean; override;
		procedure InitBoxes; override;
		procedure Render(inRenderer: TRenderer;	const inBox: TBox); override;
	end;

implementation

uses
	Math;

	function FitText(inCanvas: TCanvas; inWidth: Integer;
		const inText: string): Integer;
	var
		w, i: Integer;
		s: string;
	begin
		s := inText;
		w := inCanvas.TextWidth(s);
		if (w <= inWidth) then
			Result := -1
		else begin
			i := 0;
			while (w > inWidth) do
			begin
				i := Length(s);
				while (i > 0) and (s[i] <> ' ') do //(Pos(s[i], '- ') < 0) do
					Dec(i);
				if (i = 0) then
					break
				else begin
					s := Copy(s, 1, i - 1);
					w := inCanvas.TextWidth(s);
				end;
			end;
			Result := i;
		end;
	end;

{ TBox }

constructor TBox.Create;
begin
	Boxes := TBoxList.Create;
end;

destructor TBox.Destroy;
begin
	Boxes.Free;
	inherited;
end;

{ TBoxList }

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

constructor TNode.Create;
begin
	Nodes := TNodeList.Create;
	Style := TStyle.Create;
end;

destructor TNode.Destroy;
begin
	Nodes.Free;
	inherited;
end;

{ TNodeList }

function TNodeList.GetNode(inIndex: Integer): TNode;
begin
	Result := TNode(Items[inIndex]);
end;

procedure TNodeList.SetNode(inIndex: Integer; const Value: TNode);
begin
	Items[inIndex] := Value;
end;

{ TRenderer }

procedure TRenderer.Render(inNodes: TNodeList; inCanvas: TCanvas;
	inRect: TRect);
begin
	Canvas := inCanvas;
	Width := inRect.Right - inRect.Left;
	RenderNodes(inNodes);
	//RenderBoxes(inNodes);
end;

procedure TRenderer.RenderNode(inNode: TNode);
{
var
	box: TBox;
	break: Boolean;
}
begin
{
	box := TBox.Create;
	try
		inNode.InitBoxes;
		repeat
			box.Left := Pen.X;
			box.Top := Pen.Y;
			break := inNode.NextBox(Self, box);
			if (break) then
			begin
				Pen.Y := Pen.Y + box.Height;
				Pen.X := 0;
			end else
				Pen.X := Pen.X + box.Width;
			//
			Canvas.Brush.Color := clBtnFace;
			Canvas.Font.Color := clBlack;
			inNode.RenderBox(Canvas, box);
		until not break;
	finally
		box.Free;
	end;
}
end;

procedure TRenderer.RenderNodes(inNodes: TNodeList);
var
	i: Integer;
begin
	Pen := Point(0, 0);
	for i := 0 to Pred(inNodes.Count) do
		RenderNode(inNodes[i]);
end;

function TRenderer.BuildContext(inContextClass: TContextClass;
	inBoxes: TBoxList; inNodes: TNodeList; inIndex: Integer): Integer;
var
	context: TContext;
begin
	context := inContextClass.Create(Self, inBoxes);
	try
		Result := context.BuildBoxes(inNodes, inIndex);
	finally
		context.Free;
	end;
end;

function TRenderer.BuildBoxes(inNodes: TNodeList): TBoxList;
var
	i: Integer;
begin
	Result := TBoxList.Create;
	i := 0;
	while (i < inNodes.Count) do
		case inNodes[i].Style.Display of
			diBlock: i := BuildContext(TBlockContext, Result, inNodes, i);
			diInline: i := BuildContext(TInlineContext, Result, inNodes, i);
			else exit;
		end;
end;

procedure TRenderer.MeasureBox(inBox: TBox);

	function MaxHeight(inBoxes: TBoxList): Integer;
	var
		i: Integer;
	begin
		Result := 0;
		for i := 0 to Pred(inBoxes.Count) do
			Result := Max(inBoxes[i].Height, Result);
	end;

	function SumHeight(inBoxes: TBoxList): Integer;
	var
		i: Integer;
	begin
		Result := 0;
		for i := 0 to Pred(inBoxes.Count) do
			Result := Result + inBoxes[i].Height;
	end;

begin
	if (inBox.Height <= 0) and (inBox.Boxes.Count > 0)then
	begin
		MeasureBoxes(inBox.Boxes);
		if (inBox.Boxes[0].Context = diInline) then
			inBox.Height := MaxHeight(inBox.Boxes)
		else
			inBox.Height := SumHeight(inBox.Boxes);
	end;
end;

procedure TRenderer.MeasureBoxes(inBoxes: TBoxList);
var
	i: Integer;
begin
	for i := 0 to Pred(inBoxes.Count) do
		MeasureBox(inBoxes[i]);
end;

procedure TRenderer.RenderBox(inBox: TBox);
var
	y: Integer;
begin
	y := 0;
	if inBox.Node <> nil then
		inBox.Node.Render(Self, inBox);
	if (inBox.Node <> nil) and (inBox.Context = diBlock) then
		y := Pen.Y;
	DoRenderBoxes(inBox.Boxes);
	if (inBox.Node <> nil) and (inBox.Context = diBlock) then
		Pen.Y := y;
	if ((inBox.Node <> nil) or (inBox.Name='LINE')) and (inBox.Context = diBlock) then
		Inc(Pen.Y, inBox.Height);
end;

procedure TRenderer.DoRenderBoxes(inBoxes: TBoxList);
var
	i: Integer;
begin
	for i := 0 to Pred(inBoxes.Count) do
		RenderBox(inBoxes[i]);
end;

procedure TRenderer.RenderBoxes(inBoxes: TBoxList);
begin
	Pen.X := 0;
	Pen.Y := 0;
	MeasureBoxes(inBoxes);
	DoRenderBoxes(inBoxes);
end;

{ TContext }

constructor TContext.Create(inRenderer: TRenderer; inBoxes: TBoxList);
begin
	Renderer := inRenderer;
	Boxes := inBoxes;
	Width := Renderer.Width;
	SetContext;
end;

function TContext.BuildBoxes(inNodes: TNodeList; inIndex: Integer): Integer;
begin
	while (inIndex < inNodes.Count) and
		(inNodes[inIndex].Style.Display = Context) do
	begin
		BuildBox(inNodes[inIndex]);
		Inc(inIndex);
	end;
	Result := inIndex;
end;

{ TBlockContext }

procedure TBlockContext.SetContext;
begin
	Context := diBlock;
end;

procedure TBlockContext.BuildBox(inNode: TNode);
var
	box: TBox;
begin
	box := TBox.Create;
	box.Node := inNode;
	box.Name := 'BLOCK';
	box.Context := diBlock;
	box.Boxes.Free;
	box.Boxes := Renderer.BuildBoxes(inNode.Nodes);
	Boxes.Add(box);
end;

{ TInlineContext }

procedure TInlineContext.SetContext;
begin
	Context := diInline;
end;

function TInlineContext.FitNodeText(const inText: string): Integer;
begin
	Result := FitText(Renderer.Canvas, Width - Pen.X, inText);
	if (Result = 0) and (Pen.X = 0) then
		Result := Length(inText);
end;

procedure TInlineContext.BuildLineBox;
begin
	Line := TBox.Create;
	Line.Width := Renderer.Width;
	Line.Name := 'LINE';
	Line.Context := diBlock;
	Boxes.Add(Line);
end;

procedure TInlineContext.BuildNodeBoxes(inNode: TNode);
var
	box: TBox;
	wrap: Boolean;
begin
	inNode.InitBoxes;
	repeat
		box := TBox.Create;
		box.Left := Pen.X;
		box.Top := Pen.Y;
		box.Name := 'INLINE';
		box.Context := diInline;
		box.Node := inNode;
		wrap := inNode.NextBox(Self, box);
		Line.Boxes.Add(box);
		if wrap then
		begin
			BuildLineBox;
			Pen.X := 0;
		end else
			Pen.X := Pen.X + box.Width;
	until not wrap;
end;

procedure TInlineContext.BuildBox(inNode: TNode);
begin
	BuildNodeBoxes(inNode);
end;

function TInlineContext.BuildBoxes(inNodes: TNodeList;
	inIndex: Integer): Integer;
var
	box: TBox;
begin
	box := TBox.Create;
	box.Name := 'INLINE CONTAINER';
	box.Context := diBlock;
	Boxes.Add(box);
	Boxes := box.Boxes;
	BuildLineBox;
	Pen.Y := 0;
	Result := inherited BuildBoxes(inNodes, inIndex);
end;

{ TTextNode }

constructor TTextNode.Create;
begin
	inherited;
end;

procedure TTextNode.InitBoxes;
begin
	Cursor := 1;
	TextLength := Length(Text);
end;

{
function TTextNode.NextBox(inRenderer: TRenderer;
	var inBox: TBox): Boolean;
var
	e: Integer;
	t: string;
begin
	t := Copy(Text, Cursor, MAXINT);
	e := inRenderer.FitNodeText(t);
	if (e > -1) then
		t := Copy(t, 1, e);
	//
	inBox.Width := inRenderer.Canvas.TextWidth(t);
	inBox.Height := inRenderer.Canvas.TextHeight(t);
	inBox.Text := t;
	//
	if (e = -1) then
		Cursor := TextLength
	else
		Inc(Cursor, e);
	Result := (Cursor < TextLength);
end;
}

function TTextNode.NextBox(inContext: TContext;
	var inBox: TBox): Boolean;
var
	e: Integer;
	t: string;
begin
	t := Copy(Text, Cursor, MAXINT);
	e := TInlineContext(inContext).FitNodeText(t);
	if (e > -1) then
		t := Copy(t, 1, e);
	//
	inBox.Width := inContext.Renderer.Canvas.TextWidth(t);
	inBox.Height := inContext.Renderer.Canvas.TextHeight(t);
	inBox.Text := t;
	inBox.Name := 'TEXT';
	//
	if (e = -1) then
		Cursor := TextLength
	else
		Inc(Cursor, e);
	Result := (Cursor < TextLength);
end;

{
procedure TTextNode.RenderBox(inCanvas: TCanvas;
	const inBox: TBox);
begin
	with inBox, inCanvas do
	begin
		if (Style.BackgroundColor <> clDefault) then
		begin
			Brush.Style := bsSolid;
			Brush.Color := Style.BackgroundColor;
			FillRect(Rect(Left, Top, Left + Width, Top + Height));
		end;
		if (Style.Color <> clDefault) then
			Font.Color := Style.Color;
		TextOut(Left, Top, Text);
		Brush.Style := bsClear;
		Rectangle(Left, Top, Left + Width, Top + Height);
	end;
end;
}

procedure TTextNode.Render(inRenderer: TRenderer; const inBox: TBox);
var
	r: TRect;
begin
	with inBox, inRenderer, inRenderer.Canvas do
	begin
		r := Rect(Left, Top + inRenderer.Pen.Y,
			Left + Width, Top + inRenderer.Pen.Y + Height);
		if (Style.BackgroundColor <> clDefault) then
		begin
			Brush.Style := bsSolid;
			Brush.Color := Style.BackgroundColor;
			FillRect(r);
		end;
		if (Style.Color <> clDefault) then
			Font.Color := Style.Color;
		TextOut(r.Left, r.Top, Text);
		Brush.Style := bsClear;
		Rectangle(r);
	end;
end;

{ TDivNode }

constructor TDivNode.Create;
begin
	inherited;
	Style.Display := diBlock;
	Element := 'DIV';
end;

procedure TDivNode.InitBoxes;
begin
	//
end;

function TDivNode.NextBox(inContext: TContext; var inBox: TBox): Boolean;
begin
	inBox.Width := inContext.Width;
	inBox.Height := -1;
	inBox.Name := 'DIV';
	Result := false;
end;

procedure TDivNode.Render(inRenderer: TRenderer; const inBox: TBox);
begin
end;

{ TImageNode }

constructor TImageNode.Create;
begin
	inherited;
	Style.Display := diInline;
	Element := 'IMG';
	Width := -1;
	Height := -1;
end;

procedure TImageNode.InitBoxes;
begin
	inherited;
end;

function TImageNode.ImgWidth: Integer;
begin
	if (Width < 0) then
	begin
		if (Height >= 0) and (Picture.Height > 0) then
			Result := Picture.Width * Height div Picture.Height
		else
			Result := Picture.Width
	end else
		Result := Width;
end;

function TImageNode.ImgHeight: Integer;
begin
	if (Height < 0) then
	begin
		if (Width >= 0) and (Picture.Width > 0) then
			Result := Picture.Height * Width div Picture.Width
		else
			Result := Picture.Height
	end else
		Result := Height;
end;

function TImageNode.NextBox(inContext: TContext; var inBox: TBox): Boolean;
begin
	if (Picture <> nil) then
	begin
		inBox.Width := ImgWidth;
		inBox.Height := ImgHeight;
		inBox.Name := 'IMG';
	end;
	Result := false;
end;

procedure TImageNode.Render(inRenderer: TRenderer; const inBox: TBox);
var
	r: TRect;
begin
	with inBox, inRenderer, inRenderer.Canvas do
	begin
		r := Rect(Left, Top + inRenderer.Pen.Y,
			Left + Width, Top + inRenderer.Pen.Y + Height);
		if (Picture <> nil) then
			StretchDraw(r, Picture.Graphic);
	end;
end;

end.
