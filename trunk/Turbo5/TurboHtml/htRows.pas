unit htRows;

interface

uses
	Windows, SysUtils, Types, Classes, Controls, Graphics,
	htInterfaces, htMarkup;

type
	ThtCustomRows = class(TCustomControl, IhtGenerator)
	protected
		procedure AlignControls(AControl: TControl; var Rect: TRect); override;
		procedure Paint; override;
	public
		constructor Create(inOwner: TComponent); override;
		procedure Generate(inMarkup: ThtMarkup);
	end;
	//
	ThtRows = class(ThtCustomRows)
	published
		property Align;
		property Visible;
	end;

implementation

uses
	LrControlIterator, LrVclUtils;

{ ThtCustomRows }

constructor ThtCustomRows.Create(inOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle + [ csAcceptsControls ];
end;

procedure ThtCustomRows.Paint;
begin
	inherited;
	with Canvas do
	begin
		SetBkColor(Handle, ColorToRgb(Brush.Color));
		//SetBkMode(Handle, TRANSPARENT);
		Brush.Style := bsHorizontal;
		Brush.Color := ColorToRgb(clSilver);
		Pen.Style := psClear;
		Rectangle(ClientRect);
		Brush.Style := bsSolid;
		Pen.Style := psSolid;
		//SetBkMode(Handle, OPAQUE);
	end;
end;

procedure ThtCustomRows.AlignControls(AControl: TControl; var Rect: TRect);
var
	i: Integer;
begin
	for i := 0 to Pred(ControlCount) do
		Controls[i].Align := alTop;
	inherited;
end;

procedure ThtCustomRows.Generate(inMarkup: ThtMarkup);
var
	g: IhtGenerator;
begin
	with TLrSortedCtrlIterator.Create(Self, alTop) do
	try
		while Next do
		begin
			inMarkup.Styles.Add(Format(
				'#%s { width:100%%; height:%dpx; border:1px solid black; }',
					[ Ctrl.Name, Ctrl.Height ]));
			inMarkup.Add(Format('  <div id="%s">', [ Ctrl.Name ]));
			//
			if (LrIsAs(Ctrl, IhtGenerator, g)) then
				g.Generate(inMarkup)
			else
				inMarkup.Add(Ctrl.Name);
//				inMarkup.Add('    <span style=" vertical-align:middle;">' + Ctrl.Name + '</span>');
			//
			inMarkup.Add('  </div>');
		end;
	finally
		Free;
	end;
end;

end.

