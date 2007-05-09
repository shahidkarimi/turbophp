unit htColumnsPanel;

interface

uses
	Windows, SysUtils, Types, Messages, Classes, Controls, Graphics,
	htInterfaces, htMarkup, htControls, htPanel;

type
	ThtCustomColumnsPanel = class(ThtCustomControl)
	private
		FClientPanel: ThtPanel;
		FLeftPanel: ThtPanel;
		FLeftWidth: Integer;
	protected
		procedure AlignControls(AControl: TControl; var Rect: TRect); override;
		procedure CMControlChange(var Message: TMessage); message CM_CONTROLCHANGE;
		procedure GenerateCtrls(inMarkup: ThtMarkup);
		procedure GenerateCtrl(inMarkup: ThtMarkup; const inContainer: string;
			inCtrl: TControl);
		procedure SetLeftWidth(const Value: Integer);
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		procedure Generate(const inContainer: string;
			inMarkup: ThtMarkup); override;
		property LeftWidth: Integer read FLeftWidth write SetLeftWidth;
	end;
	//
	ThtColumnsPanel = class(ThtCustomColumnsPanel)
	published
		property Align;
		property LeftWidth;
		property Outline;
		property Style;
		property Transparent;
		property Visible;
	end;

implementation

uses
	LrVclUtils, LrControlIterator;

{ ThtCustomColumnsPanel }

constructor ThtCustomColumnsPanel.Create(inOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle + [ csAcceptsControls ];
	//
	FLeftPanel := ThtPanel.Create(Owner);
	FLeftPanel.Parent := Self;
	FLeftPanel.Align := alLeft;
	FLeftPanel.Outline := true;
	//FLeftPanel.SetSubcomponent(true);
	//
	FClientPanel := ThtPanel.Create(Self);
	FClientPanel.Align := alClient;
	FClientPanel.Parent := Self;
	//FClientPanel.SetSubcomponent(true);
	LeftWidth := 200;
end;

destructor ThtCustomColumnsPanel.Destroy;
begin
	inherited;
end;

procedure ThtCustomColumnsPanel.SetLeftWidth(const Value: Integer);
begin
	FLeftWidth := Value;
	FLeftPanel.Width := FLeftWidth;
end;

procedure ThtCustomColumnsPanel.GenerateCtrl(inMarkup: ThtMarkup;
	const inContainer: string; inCtrl: TControl);
var
	c: IhtControl;
begin
	if LrIsAs(inCtrl, IhtControl, c) then
		c.Generate(inContainer, inMarkup)
	else
		inMarkup.Add(inCtrl.Name);
end;

procedure ThtCustomColumnsPanel.GenerateCtrls(inMarkup: ThtMarkup);
begin
	with TLrCtrlIterator.Create(Self) do
	try
		while Next do
			GenerateCtrl(inMarkup, Ctrl.Name, Ctrl);
	finally
		Free;
	end;
end;

procedure ThtCustomColumnsPanel.Generate(const inContainer: string;
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

procedure ThtCustomColumnsPanel.AlignControls(AControl: TControl;
	var Rect: TRect);
begin
	with TLrCtrlIterator.Create(Self) do
	try
		while Next do
			if (Ctrl <> FLeftPanel) and (Ctrl <> FClientPanel) then
				if Ctrl.Left < LeftWidth then
					Ctrl.Parent := FLeftPanel
				else begin
					Ctrl.Parent := FClientPanel;
					Ctrl.Left := Ctrl.Left - LeftWidth;
				end;
	finally
		Free;
	end;
	inherited;
end;

procedure ThtCustomColumnsPanel.CMControlChange(var Message: TMessage);
begin
{
	if not (csLoading in ComponentState) then
		with TCMControlChange(Message) do
			if Inserting and (Control.Parent = Self) and (Control <> FLeftPanel) and (Control <> FClientPanel) then
			begin
				if Control.Left < LeftWidth then
					Control.Parent := FLeftPanel
				else begin
					Control.Parent := FClientPanel;
					Control.Left := Control.Left - LeftWidth;
				end;
			end;
}
	inherited;
end;

end.

