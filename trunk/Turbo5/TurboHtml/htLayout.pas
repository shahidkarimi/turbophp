unit htLayout;

interface

uses
	Windows, SysUtils, Types, Classes, Controls, Graphics,
	htInterfaces, htMarkup;

type
	ThtCustomLayout = class(TCustomControl, IhtGenerator)
	protected
		procedure AlignControls(AControl: TControl; var Rect: TRect); override;
		procedure GenerateCtrl(inMarkup: ThtMarkup;
			const inContainer: string; inCtrl: TControl);
		procedure Paint; override;
	public
		constructor Create(inOwner: TComponent); override;
		procedure Generate(inMarkup: ThtMarkup);
	end;
	//
	ThtLayout = class(ThtCustomLayout)
	published
		property Align;
		property Visible;
	end;

implementation

uses
	LrControlIterator, LrVclUtils;

{ ThtCustomLayout }

constructor ThtCustomLayout.Create(inOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle + [ csAcceptsControls ];
end;

procedure ThtCustomLayout.Paint;

	function Middle(inCtrl: TControl): Integer;
	begin
		Result := inCtrl.Top + inCtrl.Height div 2;
	end;

	function Center(inCtrl: TControl): Integer;
	begin
		Result := inCtrl.Left + inCtrl.Width div 2;
	end;

begin
	inherited;
	with Canvas do
	begin
		Pen.Width := 1;
		Pen.Color := clSilver;
		Pen.Style := psDot;
		Brush.Style := bsClear;
		Rectangle(ClientRect);
		///
		Pen.Style := psSolid;
		Pen.Color := clBlue;
		with TLrCtrlIterator.Create(Self) do
		try
			while Next do
			begin
				MoveTo(0, Middle(Ctrl));
				LineTo(ClientWidth, Middle(Ctrl));
				MoveTo(Center(Ctrl), 0);
				LineTo(Center(Ctrl), ClientHeight);
			end;
		finally
			Free;
		end;
	end;
end;

procedure ThtCustomLayout.AlignControls(AControl: TControl; var Rect: TRect);
var
	i: Integer;
begin
	Invalidate;
	for i := 0 to Pred(ControlCount) do
		Controls[i].Align := alNone;
	inherited;
end;

procedure ThtCustomLayout.Generate(inMarkup: ThtMarkup);
begin
	inMarkup.Add('<div style="position:relative; width:100%;">');
//	inMarkup.Add('<div style="position:relative; width:100%; height:100%;">');
	with TLrCtrlIterator.Create(Self) do
	try
		while Next do
		begin
//			inMarkup.Add(Format(
//				'  <div style="border: 1px solid black; background-color: #ddd; position:absolute; left: %dpx; top: %dpx; width: %dpx; height:%dpx;">',
//				[ Ctrl.Left, Ctrl.Top, Ctrl.Width, Ctrl.Height ]));
			inMarkup.Styles.Add(
				Format(
					'#%s { ' +
					'border: 1px solid black; ' +
					'background-color: #ddd; ' +
					'position:absolute; ' +
					'left: %dpx; top: %dpx; width: %dpx; height:%dpx; ' +
					'}',
						[ Ctrl.Name, Ctrl.Left, Ctrl.Top, Ctrl.Width, Ctrl.Height ])
			);
			inMarkup.Add(Format('  <div id="%s">', [ Ctrl.Name ]));
			GenerateCtrl(inMarkup, Ctrl.Name, Ctrl);
{
			if (ThIsAs(Ctrl, IhtGenerator, g)) then
				g.Generate(inMarkup)
			else
				inMarkup.Add('    ' + Ctrl.Name);
}
			inMarkup.Add('  </div>');
		end;
	finally
		Free;
	end;
	inMarkup.Add('</div>');
end;

procedure ThtCustomLayout.GenerateCtrl(inMarkup: ThtMarkup;
	const inContainer: string; inCtrl: TControl);
var
//	handled: Boolean;
	c: IhtControl;
	g: IhtGenerator;
begin
//	handled := false;
//	if Assigned(OnGenerateCtrl) then
//		OnGenerateCtrl(inMarkup, inContainer, inCtrl, handled);
//	if not handled then
		if (LrIsAs(inCtrl, IhtGenerator, g)) then
			g.Generate(inMarkup)
		else if LrIsAs(inCtrl, IhtControl, c) then
			c.Generate(inContainer, inMarkup)
//		else if (inCtrl is TWinControl) then
//			GenerateWinCtrl(inCtrl)
		else
			inMarkup.Add(inCtrl.Name);
end;

end.
