unit htColumns;

interface

uses
	Windows, SysUtils, Types, Classes, Controls, Graphics,
	htInterfaces, htMarkup;

type
	ThtCustomColumns = class(TCustomControl, IhtGenerator)
	protected
		procedure AlignControls(AControl: TControl; var Rect: TRect); override;
		procedure Paint; override;
	public
		constructor Create(inOwner: TComponent); override;
		procedure Generate(inMarkup: ThtMarkup);
	end;
	//
	ThtColumns = class(ThtCustomColumns)
	published
		property Align;
//		property AutoSize default true;
//		property BorderColor;
//		property Borders;
//		property ContentColor;
//		property LeftPosition;
//		property Margins;
//		property OnGetContent;
//		property Padding;
//		property Position;
//		property TopPosition;
		property Visible;
	end;

implementation

uses
	LrControlIterator, LrVclUtils;

{ ThtCustomColumns }

constructor ThtCustomColumns.Create(inOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle + [ csAcceptsControls ];
end;

procedure ThtCustomColumns.Paint;
begin
	inherited;
	with Canvas do
	begin
		SetBkColor(Handle, ColorToRgb(Brush.Color));
		//SetBkMode(Handle, TRANSPARENT);
		Brush.Style := bsVertical;
		Brush.Color := ColorToRgb(clSilver);
		Pen.Style := psClear;
		Rectangle(ClientRect);
		Brush.Style := bsSolid;
		Pen.Style := psSolid;
		//SetBkMode(Handle, OPAQUE);
	end;
end;

procedure ThtCustomColumns.AlignControls(AControl: TControl; var Rect: TRect);
var
	i: Integer;
begin
	for i := 0 to Pred(ControlCount) do
		Controls[i].Align := alLeft;
	inherited;
end;

procedure ThtCustomColumns.Generate(inMarkup: ThtMarkup);
var
	g: IhtGenerator;
	w: string;
begin
//	inMarkup.Add(Format(
//		'<table cellpadding="0" cellspacing="0" border="1" style="height:%dpx; table-layout: fixed;">',
//		[ Height ])
//	);
	inMarkup.Styles.Add(Format(
		'#%s_table { height:%dpx; width:100%%; table-layout: fixed; }',
			[ Name, Height ])
	);
	inMarkup.Add(Format(
		'<table id="%s_table" cellpadding="0" cellspacing="0" border="1">',
			[ Name ])
	);
	inMarkup.Add('<tr>');
	with TLrSortedCtrlIterator.Create(Self, alLeft) do
	try
		while Next do
		begin
			if (Index = 2) then
				w := ''
			else
				w := Format('width="%dpx"', [ Ctrl.Width ]);
			//
			if (LrIsAs(Ctrl, IhtGenerator, g)) then
			begin
				inMarkup.Add(Format('<td %s>', [ w ]));
//					'    <td valign="top"%s>', [ w ]));
				g.Generate(inMarkup)
			end else
			begin
				inMarkup.Add(Format('<td %s>', [ w ]));
//					'    <td valign="top"%s>', [ w ]));
//					'    <td align="middle" valign="center" width="%dpx">', [ Ctrl.Width ]));
				inMarkup.Add(Ctrl.Name);
			end;
			//
			inMarkup.Add('</td>');
		end;
	finally
		Free;
	end;
	inMarkup.Add('</tr>');
	inMarkup.Add('</table>');
end;

end.
