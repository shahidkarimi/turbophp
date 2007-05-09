unit Render3;

interface

uses
	SysUtils, Types, Graphics,
	Style, Layout;

type
	TRenderer = class
	private
		Boxes: TBoxList;
		Canvas: TCanvas;
		Pen: TPoint;
		Width: Integer;
	protected
		function GetContextRect(inBox: TBox): TRect;
		procedure RenderBox(inBox: TBox); virtual;
		procedure RenderBoxes;
	public
		constructor Create;
		destructor Destroy; override;
		procedure Render(inBoxes: TBoxList; inCanvas: TCanvas; inRect: TRect);
	end;

procedure TestBoxRenderer(inCanvas: TCanvas; inRect: TRect);

implementation

uses
	Nodes;

function BuildBox(inClass: TNodeClass; inW, inH, inX, inY: Integer): TBox;

	function rc: Integer;
	begin
		Result := Random(192) + 64;
	end;

begin
	Result := TBox.Create;
	Result.Width := inW;
	Result.Height := inH;
	Result.Offset := Point(inX, inY);
	Result.Node := inClass.Create;
	Result.Node.Style.BackgroundColor :=	(rc shl 16) + (rc shl 8) + rc;
	Result.Node.Text := 'Box ' + IntToStr(Random(255));
end;

function BuildBoxes(inClass: TNodeClass;
	inCount, inW, inH, inX, inY: Integer): TBoxList;
var
	i: Integer;
begin
	Result := TBoxList.Create;
	for i := 1 to inCount do
		Result.Add(BuildBox(inClass, inW, inH, inX, inY));
end;

function BuildTestBoxes: TBoxList;
begin
	RandSeed := 234535;
	Result := TBoxList.Create;
	Result.Add(BuildBox(TCustomNode, 500, 100, 0, 100));
	Result.Add(BuildBox(TCustomNode, 500, 100, 0, 100));
	Result.Add(BuildBox(TCustomNode, 500, 100, 0, 100));
	Result[2].Boxes := BuildBoxes(TCustomNode, 2, 150, 40, 150, 0);
	Result[2].Boxes[0].Boxes := BuildBoxes(TCustomNode, 1, 130, 40, 0, 30);
	Result[2].Boxes[0].Boxes[0].Boxes := BuildBoxes(TCustomNode, 3, 40, 15, 40, 0);
	Result.Add(BuildBox(TCustomNode, 500, 100, 0, 100));
	Result[3].Boxes := BuildBoxes(TCustomNode, 3, 50, 20, 50, 0);
	Result.Add(BuildBox(TCustomNode, 500, 100, 0, 100));
//	Result[4].Boxes := BuildBoxes(TBoxContextNode, 5, 500, 20);
	Result.Add(BuildBox(TCustomNode, 500, 100, 0, 100));
end;

procedure TestBoxRenderer(inCanvas: TCanvas; inRect: TRect);
var
	r: TRenderer;
	b: TBoxList;
begin
	r := TRenderer.Create;
	try
		b := BuildTestBoxes;
		try
			r.Render(b, inCanvas, inRect);
		finally
			b.Free;
		end;
	finally
		r.Free;
	end;
end;

{ TRenderer }

constructor TRenderer.Create;
begin
end;

destructor TRenderer.Destroy;
begin
	inherited;
end;

function TRenderer.GetContextRect(inBox: TBox): TRect;
begin
	with inBox do
		Result := Rect(Left, Top, Left + Width, Top + Height);
	OffsetRect(Result, Pen.X, Pen.Y);
end;

procedure TRenderer.RenderBox(inBox: TBox);
begin
	TCustomNode(inBox.Node).Render(inBox, Canvas, GetContextRect(inBox));
	Inc(Pen.X, inBox.Offset.X);
	Inc(Pen.Y, inBox.Offset.Y);
end;

procedure TRenderer.RenderBoxes;
var
	i: Integer;
begin
	for i := 0 to Boxes.Max do
		RenderBox(Boxes[i]);
end;

procedure TRenderer.Render(inBoxes: TBoxList; inCanvas: TCanvas;
	inRect: TRect);
begin
	Boxes := inBoxes;
	Canvas := inCanvas;
	Width := inRect.Right - inRect.Left;
	Pen := Point(inRect.Left, inRect.Top);
	RenderBoxes;
end;

end.
