unit OldNodes;

interface

uses
	SysUtils, Types, Graphics,
	Style, Layout, Render3;

type
	TCustomNode = class(TNode)
	protected
		function CreateRenderer: TRenderer; virtual;
		procedure Paint(inCanvas: TCanvas; inRect: TRect); virtual;
	public
		procedure Render(inBox: TBox; inCanvas: TCanvas;
			inRect: TRect);
	end;
	//
	TNodeClass = class of TCustomNode;
	//
	TDivNode = class(TCustomNode)
	public
		class function GetContextClass: TContextClass; override;
		procedure InitBox(inLayout: TLayout; var inBox: TBox); override;
	end;
	//
	TInlineNode = class(TCustomNode)
	public
		class function GetContextClass: TContextClass; override;
		procedure InitBox(inLayout: TLayout; var inBox: TBox); override;
	end;

implementation

{ TCustomNode }

function TCustomNode.CreateRenderer: TRenderer;
begin
	Result := TRenderer.Create;
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
	if (inBox.Boxes <> nil) then
		RenderBoxes;
end;

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

{ TDivNode }

class function TDivNode.GetContextClass: TContextClass;
begin
	Result := TBlockContext;
end;

procedure TDivNode.InitBox(inLayout: TLayout; var inBox: TBox);
begin
	with inBox do
	begin
		if Style.Width > 0 then
			Width := Style.Width;
		if Style.Height > 0 then
			Height := Style.Height;
		Offset := Point(0, Height);
	end;
end;

{ TInlineNode }

class function TInlineNode.GetContextClass: TContextClass;
begin
	Result := TInlineContext;
end;

procedure TInlineNode.InitBox(inLayout: TLayout; var inBox: TBox);
begin
	with inBox do
	begin
		Width := inLayout.Canvas.TextWidth(Text);
		Height := inLayout.Canvas.TextHeight(Text);
		Offset := Point(Width, 0);
	end;
end;

end.
