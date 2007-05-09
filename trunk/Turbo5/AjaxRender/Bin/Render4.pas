unit Render4;

interface

uses
	SysUtils, Types, Graphics,
	Style, Box, Node;

type
	TRenderer = class
	private
		Boxes: TBoxList;
		Canvas: TCanvas;
		Pen: TPoint;
		//Width: Integer;
	protected
		function GetContextRect(inBox: TBox): TRect;
		procedure RenderBox(inBox: TBox); virtual;
		procedure RenderBoxes;
	public
		constructor Create(inBoxes: TBoxList; inCanvas: TCanvas; inRect: TRect);
	end;

implementation

{ TRenderer }

constructor TRenderer.Create(inBoxes: TBoxList; inCanvas: TCanvas;
	inRect: TRect);
begin
	Boxes := inBoxes;
	Canvas := inCanvas;
	//Width := inRect.Right - inRect.Left;
	Pen := Point(inRect.Left, inRect.Top);
	RenderBoxes;
end;

procedure TRenderer.RenderBoxes;
var
	i: Integer;
begin
	for i := 0 to Boxes.Max do
		RenderBox(Boxes[i]);
end;

function TRenderer.GetContextRect(inBox: TBox): TRect;
begin
	with inBox do
		Result := Rect(Left, Top, Left + Width, Top + Height);
	OffsetRect(Result, Pen.X, Pen.Y);
end;

procedure TRenderer.RenderBox(inBox: TBox);
var
	r: TRect;
begin
	r := GetContextRect(inBox);
	if (inBox.Node <> nil) then
		TBoxNode(inBox.Node).Render(inBox, Canvas, r);
	if (inBox.Boxes <> nil) and (inBox.Boxes.Count > 0) then
		TRenderer.Create(inBox.Boxes, Canvas, r).Free;
	Inc(Pen.X, inBox.Offset.X);
	Inc(Pen.Y, inBox.Offset.Y);
end;

end.
