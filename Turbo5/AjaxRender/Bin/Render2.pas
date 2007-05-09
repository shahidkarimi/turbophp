unit Render2;

interface

uses
	SysUtils, Types, Graphics,
	Style, Render;

type
	TCustomRenderer = class
	private
		Blocks: TBoxList;
		Canvas: TCanvas;
		Pen: TPoint;
		Width: Integer;
	protected
		function GetContextRect(inBlock: TBox): TRect;
		procedure RenderBlock(inBlock: TBox); virtual;
		procedure RenderBlocks;
	public
		constructor Create;
		destructor Destroy; override;
		procedure Render(inBlocks: TBoxList; inCanvas: TCanvas; inRect: TRect);
	end;
	//
	TBlockRenderer = class(TCustomRenderer)
	protected
		procedure RenderBlock(inBlock: TBox); override;
	end;
	//
	TInlineRenderer = class(TCustomRenderer)
	protected
		procedure RenderBlock(inBlock: TBox); override;
	end;
	//
	TCustomNode = class(TNode)
	protected
		function CreateRenderer: TCustomRenderer; virtual; abstract;
		procedure Paint(inCanvas: TCanvas; inRect: TRect); virtual;
	public
		procedure Render(inBox: TBox; inCanvas: TCanvas;
			inRect: TRect);
	end;
	//
	TNodeClass = class of TCustomNode;
	//
	TBlockContextNode = class(TCustomNode)
	protected
		function CreateRenderer: TCustomRenderer; override;
	end;
	//
	TInlineContextNode = class(TCustomNode)
	protected
		function CreateRenderer: TCustomRenderer; override;
	end;

procedure TestBlockRenderer(inCanvas: TCanvas; inRect: TRect);

implementation

function BuildBlock(inClass: TNodeClass; inW, inH: Integer): TBox;

	function rc: Integer;
	begin
		Result := Random(192) + 64;
	end;

begin
	Result := TBox.Create;
	Result.Width := inW;
	Result.Height := inH;
	Result.Node := inClass.Create;
	Result.Node.Style.BackgroundColor :=	(rc shl 16) + (rc shl 8) + rc;
	Result.Node.Text := 'Box ' + IntToStr(Random(255));
end;

function BuildBlocks(inClass: TNodeClass; inCount, inW, inH: Integer): TBoxList;
var
	i: Integer;
begin
	Result := TBoxList.Create;
	for i := 1 to inCount do
		Result.Add(BuildBlock(inClass, inW, inH));
end;

function BuildTestBlocks: TBoxList;
begin
	RandSeed := 234535;
	Result := TBoxList.Create;
	Result.Add(BuildBlock(TBlockContextNode, 500, 100));
	Result.Add(BuildBlock(TBlockContextNode, 500, 100));
	Result.Add(BuildBlock(TBlockContextNode, 500, 100));
//	Result[2].Boxes := BuildBlocks(TBlockContextNode, 3, 500, 20);
	Result.Add(BuildBlock(TInlineContextNode, 500, 100));
	Result[3].Boxes := BuildBlocks(TBlockContextNode, 3, 50, 20);
	Result.Add(BuildBlock(TBlockContextNode, 500, 100));
//	Result[4].Boxes := BuildBlocks(TBlockContextNode, 5, 500, 20);
	Result.Add(BuildBlock(TBlockContextNode, 500, 100));
end;

procedure TestBlockRenderer(inCanvas: TCanvas; inRect: TRect);
var
	r: TBlockRenderer;
	b: TBoxList;
begin
	r := TBlockRenderer.Create;
	try
		b := BuildTestBlocks;
		try
			r.Render(b, inCanvas, inRect);
		finally
			b.Free;
		end;
	finally
		r.Free;
	end;
end;

{ TCustomRenderer }

constructor TCustomRenderer.Create;
begin
end;

destructor TCustomRenderer.Destroy;
begin
	inherited;
end;

procedure TCustomRenderer.RenderBlocks;
var
	i: Integer;
begin
	for i := 0 to Blocks.Max do
		RenderBlock(Blocks[i]);
end;

function TCustomRenderer.GetContextRect(inBlock: TBox): TRect;
begin
	with inBlock do
		Result := Rect(Left, Top, Left + Width, Top + Height);
	OffsetRect(Result, Pen.X, Pen.Y);
end;

procedure TCustomRenderer.RenderBlock(inBlock: TBox);
begin
	TCustomNode(inBlock.Node).Render(inBlock, Canvas, GetContextRect(inBlock));
end;

procedure TCustomRenderer.Render(inBlocks: TBoxList; inCanvas: TCanvas; inRect: TRect);
begin
	Blocks := inBlocks;
	Canvas := inCanvas;
	Width := inRect.Right - inRect.Left;
	Pen := Point(inRect.Left, inRect.Top);
	RenderBlocks;
end;

{ TBlockRenderer }

procedure TBlockRenderer.RenderBlock(inBlock: TBox);
begin
	inherited;
	Inc(Pen.Y, inBlock.Height);
end;

{ TInlineRenderer }

procedure TInlineRenderer.RenderBlock(inBlock: TBox);
begin
	inherited;
	Inc(Pen.X, inBlock.Width);
end;

{ TCustomNode }

procedure TCustomNode.Paint(inCanvas: TCanvas; inRect: TRect);
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

procedure TCustomNode.Render(inBox: TBox; inCanvas: TCanvas; inRect: TRect);

	procedure RenderBoxes;
	begin
		if (inBox.Boxes.Count > 0) then
			with CreateRenderer do
			try
				Render(inBox.Boxes, inCanvas, inRect);
			finally
				Free;
			end;
	end;

begin
	Paint(inCanvas, inRect);
	RenderBoxes;
end;

{ TBlockContextNode }

function TBlockContextNode.CreateRenderer: TCustomRenderer;
begin
	Result := TBlockRenderer.Create;
end;

{ TInlineContextNode }

function TInlineContextNode.CreateRenderer: TCustomRenderer;
begin
	Result := TInlineRenderer.Create;
end;

end.
