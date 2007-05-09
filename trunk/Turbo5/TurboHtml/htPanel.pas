unit htPanel;

interface

uses
	Windows, SysUtils, Types, Classes, Controls, Graphics,
	htInterfaces, htMarkup, htControls;

type
	ThtCustomPanel = class(ThtCustomControl)
	protected
		AlignMax: array[TAlign] of Integer;
		AlignSize: array[TAlign] of Integer;
	protected
		function GetInnerRect: TRect;
		procedure AlignControls(AControl: TControl; var Rect: TRect); override;
		procedure GenerateCtrl(inMarkup: ThtMarkup; const inContainer: string;
			inCtrl: TControl); virtual;
		procedure Measure;
		procedure StylePaint; override;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		procedure Generate(const inContainer: string;
			inMarkup: ThtMarkup); override;
	end;
	//
	ThtPanel = class(ThtCustomPanel)
	published
		property Align;
		property Outline;
		property Style;
		property Transparent;
		property Visible;
	end;

implementation

uses
	LrVclUtils, LrControlIterator, htPaint;

{ ThtCustomPanel }

constructor ThtCustomPanel.Create(inOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle + [ csAcceptsControls ];
	//Transparent := true;
end;

destructor ThtCustomPanel.Destroy;
begin
	inherited;
end;

procedure ThtCustomPanel.Measure;
var
	a: TAlign;
	i, h, w: Integer;
begin
	for a := Low(TAlign) to High(TAlign) do
	begin
		AlignMax[a] := 0;
		AlignSize[a] := 0;
	end;
	for i := 0 to Pred(ControlCount) do
		with Controls[i] do
		begin
			w := Width;
			h := Height;
			case Align of
				alLeft, alRight:
				begin
					AlignMax[Align] := LrMax(AlignMax[Align], w);
					Inc(AlignSize[Align], w);
				end;
				//
				alTop, alBottom:
				begin
					AlignMax[Align] := LrMax(AlignMax[Align], h);
					Inc(AlignSize[Align], h);
				end;
			end;
		end;
end;

procedure ThtCustomPanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
	Invalidate;
	inherited;
	Measure;
end;

function ThtCustomPanel.GetInnerRect: TRect;
begin
	Result := Rect(
			AlignSize[alLeft],
			AlignSize[alTop],
			Width - AlignSize[alRight],
			Height - AlignSize[alBottom]
		);
end;

procedure ThtCustomPanel.StylePaint;
begin
	inherited;
	if Outline then
		htPaintOutline(Canvas, GetInnerRect, clLime);
 	htPaintGuides(Canvas, Self);
end;

procedure ThtCustomPanel.Generate(const inContainer: string;
	inMarkup: ThtMarkup);
var
	s: string;
begin
	inMarkup.Add(Format('<div id="%s">', [ Name ]));
	inMarkup.Styles.Add(
		Format('#%s { position: relative; margin-right: 0; height: %dpx; }',
		//Format('#%s { position: relative; width: 100%%; height: %dpx; }',
			[ Name, Height ]));
	//
	s := {Ctrl}Style.InlineAttribute;
	if (s <> '') then
		inMarkup.Styles.Add(Format('#%s { %s }', [ Name, s ]));
  //
	with TLrCtrlIterator.Create(Self) do
	try
		while Next do
		begin
			s := 'position:absolute; ' +
				Format('left: %dpx; top: %dpx; width: %dpx; height:%dpx; ',
						[ Ctrl.Left, Ctrl.Top, htBoxWidth(Ctrl), htBoxHeight(Ctrl) ]);
			//s := s + 'border: 1px dashed silver; ';
			s := Format('#%s { %s }', [ Ctrl.Name, s ]);
			inMarkup.Styles.Add(s);
			//inMarkup.Add(Format('  <div id="%s">', [ Ctrl.Name ]));
			GenerateCtrl(inMarkup, Ctrl.Name, Ctrl);
			//inMarkup.Add('  </div>');
		end;
	finally
		Free;
	end;
	inMarkup.Add('</div>');
end;

procedure ThtCustomPanel.GenerateCtrl(inMarkup: ThtMarkup;
	const inContainer: string; inCtrl: TControl);
var
//	handled: Boolean;
	c: IhtControl;
//	g: IhtGenerator;
begin
//	handled := false;
//	if Assigned(OnGenerateCtrl) then
//		OnGenerateCtrl(Markup, inContainer, inCtrl, handled);
//	if not handled then
//		if (LrIsAs(inCtrl, IhtGenerator, g)) then
//			g.Generate(inMarkup)
//		else
		if LrIsAs(inCtrl, IhtControl, c) then
			c.Generate(inContainer, inMarkup)
//		else if (inCtrl is TWinControl) then
//			GenerateWinCtrl(inCtrl)
		else
			inMarkup.Add(inCtrl.Name);
end;

end.
