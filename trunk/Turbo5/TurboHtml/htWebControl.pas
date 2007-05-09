unit htWebControl;

interface

uses
	SysUtils, Classes, Controls, Graphics,
	LrControls,
	htInterfaces, htDocument;

type
	ThtCustomWebControl = class(TGraphicControl{, IhtControl})
	private
		FStyleColor: TColor;
	protected
		procedure Paint; override;
		procedure SetStyleColor(const Value: TColor);
	public
		constructor Create(AOwner: TComponent); override;
		procedure Generate(const inContainer: string; inDocument: ThtDocument);
		property StyleColor: TColor read FStyleColor write SetStyleColor
			default clLime;
	end;
	//
	ThtWebControl = class(ThtCustomWebControl)
	published
		property Align;
		property Caption;
		property StyleColor;
	end;

implementation

uses
	htUtils;

{ ThtCustomWebControl }

constructor ThtCustomWebControl.Create(AOwner: TComponent);
begin
	inherited;
	StyleColor := clLime;
	SetBounds(0, 0, 124, 68);
end;

procedure ThtCustomWebControl.SetStyleColor(const Value: TColor);
begin
	FStyleColor := Value;
	Color := Value;
end;

procedure ThtCustomWebControl.Generate(const inContainer: string;
	inDocument: ThtDocument);
begin
	inDocument.Styles.Add('#' + inContainer +
		'{ background-color: ' + htColorToHtml(StyleColor) + '; }');
	inDocument.Add(Caption);
end;

procedure ThtCustomWebControl.Paint;
begin
	//inherited;
	Canvas.Brush.Color := FStyleColor;
	Canvas.FillRect(ClientRect);
end;

end.
