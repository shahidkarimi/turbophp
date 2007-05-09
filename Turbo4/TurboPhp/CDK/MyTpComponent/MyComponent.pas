unit MyComponent;

interface

uses
	Classes, Graphics,
	ThTag, ThWebControl, TpControls;

type
	TMyComponent = class(TTpGraphicControl)
	private
		FOnMyPhpEvent: TTpEvent;
    FMyTpProperty: string;
	protected
		procedure Paint; override;
		procedure Tag(inTag: TThTag); override;
	published
		property OnMyPhpEvent: TTpEvent read FOnMyPhpEvent write FOnMyPhpEvent;
		property Style;
		property MyTpProperty: string read FMyTpProperty write FMyTpProperty;
	end;

procedure Register;

implementation

{$R MyTpComponentIcon.res}

procedure Register;
begin
	RegisterComponents('My', [ TMyComponent ]);
end;

procedure TMyComponent.Paint;
begin
	inherited;
	ThPaintOutline(Canvas, AdjustedClientRect, clGreen, psDashDot);
end;

procedure TMyComponent.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add('tpname', Name);
		Add('tpclass', 'mycomp');
		Add('tponmyphpevent', OnMyPhpEvent);
	end;
end;

end.
 