unit htFlowPanel;

interface

uses
	Windows, SysUtils, Types, Classes, Controls, Graphics,
	htInterfaces, htMarkup, htControls;

type
	ThtCustomFlowPanel = class(ThtCustomControl)
  private
	protected
		procedure AlignControls(AControl: TControl; var Rect: TRect); override;
		procedure GenerateCtrls(inMarkup: ThtMarkup);
		procedure GenerateCtrl(inMarkup: ThtMarkup; const inContainer: string;
			inCtrl: TControl);
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		procedure Generate(const inContainer: string;
			inMarkup: ThtMarkup); override;
	end;
	//
	ThtFlowPanel = class(ThtCustomFlowPanel)
	published
		property Align;
		property Outline;
		property Style;
		property Transparent;
		property Visible;
	end;

implementation

uses
	LrVclUtils, LrControlIterator;

{ ThtCustomFlowPanel }

constructor ThtCustomFlowPanel.Create(inOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle + [ csAcceptsControls ];
end;

destructor ThtCustomFlowPanel.Destroy;
begin
	inherited;
end;

procedure ThtCustomFlowPanel.AlignControls(AControl: TControl;
	var Rect: TRect);
var
	lineHeight, x, y: Integer;
begin
	inherited;
	lineHeight := 0;
	x := 0;
	y := 0;
	with TLrCtrlIterator.Create(Self) do
	try
		while Next do
		begin
			lineHeight := LrMax(Ctrl.Height, lineHeight);
			if (x > 0) and (Ctrl.Width + x > ClientWidth) then
			begin
				Inc(y, lineHeight);
				lineHeight := 0;
				x := 0;
			end;
			Ctrl.Left := x;
			Ctrl.Top := y;
			Inc(x, Ctrl.Width);
		end;
	finally
		Free;
	end;
end;

procedure ThtCustomFlowPanel.GenerateCtrl(inMarkup: ThtMarkup;
	const inContainer: string; inCtrl: TControl);
var
	c: IhtControl;
begin
	if LrIsAs(inCtrl, IhtControl, c) then
		c.Generate(inContainer, inMarkup)
	else
		inMarkup.Add(inCtrl.Name);
end;

procedure ThtCustomFlowPanel.GenerateCtrls(inMarkup: ThtMarkup);
begin
	with TLrCtrlIterator.Create(Self) do
	try
		while Next do
			GenerateCtrl(inMarkup, Ctrl.Name, Ctrl);
	finally
		Free;
	end;
end;

procedure ThtCustomFlowPanel.Generate(const inContainer: string;
	inMarkup: ThtMarkup);
begin
	GenerateStyle('#' + Name, inMarkup);
	inMarkup.Styles.Add(
		Format('#%s { position: relative; margin-right: 0; height: %dpx; }',
			[ Name, Height ]));
	inMarkup.Add(Format('<div id="%s">', [ Name ]));
	GenerateCtrls(inMarkup);
	inMarkup.Add('</div>');
end;

end.

