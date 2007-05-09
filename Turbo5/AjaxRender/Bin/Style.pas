unit Style;

interface

uses
	Types, Classes, Graphics;

type
	TDisplay = ( diInline, diBlock );
	TTextAlign = ( taDefault, taLeft, taCenter, taRight ); 
	//
	TStyle = class
	private
		FDisplay: TDisplay;
		FColor: TColor;
		FBackgroundColor: TColor;
		FFontSize: Integer;
		FFontFamily: string;
		FHeight: Integer;
		FWidth: Integer;
		FLeft: Integer;
		FTop: Integer;
    FTextAlign: TTextAlign;
	public
		constructor Create;
		destructor Destroy; override;
		property BackgroundColor: TColor read FBackgroundColor
			write FBackgroundColor default clDefault;
		property Color: TColor read FColor write FColor default clDefault;
		property Display: TDisplay read FDisplay write FDisplay default diInline;
		property FontSize: Integer read FFontSize write FFontSize;
		property FontFamily: string read FFontFamily write FFontFamily;
		property Left: Integer read FLeft write FLeft;
		property Top: Integer read FTop write FTop;
		property Width: Integer read FWidth write FWidth;
		property Height: Integer read FHeight write FHeight;
		property TextAlign: TTextAlign read FTextAlign write FTextAlign;
	end;

implementation

{ TStyle }

constructor TStyle.Create;
begin
	Display := diInline;
	Color := clDefault;
	BackgroundColor := clDefault;
end;

destructor TStyle.Destroy;
begin
	inherited;
end;

end.
