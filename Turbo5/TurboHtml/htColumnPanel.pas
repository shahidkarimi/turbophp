unit htColumnPanel;

interface

uses
	Windows, SysUtils, Types, Classes, Controls, Graphics,
	htInterfaces, htMarkup, htControls;

type
	ThtCustomColumnPanel = class(ThtCustomControl)
	private
		FLeftPanel: ThtPanel;
		FClientPanel: ThtPanel;
    FLeftWidth: Integer;
    procedure SetLeftWidth(const Value: Integer);
	protected
		procedure GenerateCtrls(inMarkup: ThtMarkup);
		procedure GenerateCtrl(inMarkup: ThtMarkup; const inContainer: string;
			inCtrl: TControl);
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		procedure Generate(const inContainer: string;
			inMarkup: ThtMarkup); override;
		property LeftWidth: Integer read FLeftWidth write SetLeftWidth;
	end;
	//
	ThtColumnPanel = class(ThtCustomColumnPanel)
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

{ ThtCustomColumnPanel }

constructor ThtCustomColumnPanel.Create(inOwner: TComponent);
begin
	inherited;
	//ControlStyle := ControlStyle + [ csAcceptsControls ];
	FLeftPanel := ThtPanel.Create(Self);
	FLeftPanel.Align := alLeft;
	FLeftPanel.Outline := true;
	FClientPanel := ThtPanel.Create(Self);
	FClientPanel.Align := alClient;
end;

destructor ThtCustomColumnPanel.Destroy;
begin
	inherited;
end;

procedure ThtCustomColumnPanel.SetLeftWidth(const Value: Integer);
begin
	FLeftWidth := Value;
	FLeftPanel.Width := FLeftWidth;
end;

procedure ThtCustomColumnPanel.GenerateCtrl(inMarkup: ThtMarkup;
	const inContainer: string; inCtrl: TControl);
var
	c: IhtControl;
begin
	if LrIsAs(inCtrl, IhtControl, c) then
		c.Generate(inContainer, inMarkup)
	else
		inMarkup.Add(inCtrl.Name);
end;

procedure ThtCustomColumnPanel.GenerateCtrls(inMarkup: ThtMarkup);
begin
	with TLrCtrlIterator.Create(Self) do
	try
		while Next do
			GenerateCtrl(inMarkup, Ctrl.Name, Ctrl);
	finally
		Free;
	end;
end;

procedure ThtCustomColumnPanel.Generate(const inContainer: string;
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

