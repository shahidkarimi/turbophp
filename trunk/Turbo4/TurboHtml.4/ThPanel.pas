unit ThPanel;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThWebControl, ThAttributeList, ThTag, ThTextPaint;

type
	TThCustomPanel = class(TThWebControl)
	private
		FShowOutline: Boolean;
		FShowGrid: Boolean;
    FVAlign: TThVAlign;
	protected
		function GetGeneratorHtml: string;
		function GetOutlineColor: TColor; virtual;
		procedure SetShowGrid(const Value: Boolean);
		procedure SetShowOutline(const Value: Boolean);
		procedure SetVAlign(const Value: TThVAlign);
	protected
		procedure AlignControls(AControl: TControl; var Rect: TRect); override;
		procedure Paint; override;
		procedure Tag(inTag: TThTag); override;
		procedure UnoverlapControls;
	protected
		property OutlineColor: TColor read GetOutlineColor;
		property ShowGrid: Boolean read FShowGrid write SetShowGrid default false;
		property ShowOutline: Boolean read FShowOutline write SetShowOutline
			default true;
		property VAlign: TThVAlign read FVAlign write SetVAlign default vaDefault;
	public
		constructor Create(AOwner: TComponent); override;
	end;
	//
	TThPanel = class(TThCustomPanel)
	public
		constructor Create(AOwner: TComponent); override;
		procedure CellTag(inTag: TThTag); override;
	published
		property Align;
		property DesignOpaque;
		property ShowGrid;
		property ShowOutline;
		property Style;
		property StyleClass;
		property VAlign;
		property Visible;
	end;

implementation

uses
	ThComponentIterator, {TbhContentView,} ThAlignUtils, ThStylePainter,
	ThGenerator, ThLabel;

{ TThCustomPanel }

constructor TThCustomPanel.Create(AOwner: TComponent);
begin
	inherited;
	FShowGrid := false;
	FShowOutline := true;
end;

procedure TThCustomPanel.Paint;
var
	l, t, i, j: Integer;
begin
	inherited;
	if ShowGrid then
	begin
		l := 0; //Left mod 4;
		t := 0; //Top mod 4;
		// Aligning with the form dots is tricky because the panel
		// doesn't redraw when moved (the pixels are blitted)
		for j := 0 to Height div 4 do
			for i := 0 to Width div 4 do
				Canvas.Pixels[i * 4 - l, j * 4 - t] := clGray;
	end;
	if ShowOutline then
		ThPaintOutline(Canvas, AdjustedClientRect, OutlineColor);
end;

procedure TThCustomPanel.Tag(inTag: TThTag);
begin
	with inTag do
	begin
		Element := '';
{
		Content := #13'<!--BEGIN m' + Name + ' -->' //#13
							+ GetGeneratorHtml
							+ #13'<!--END m' + Name + ' -->'#13;
}
		Content := GetGeneratorHtml;
	end;
end;

function TThCustomPanel.GetGeneratorHtml: string;
begin
	with TThGenerator.Create do
	try
		Container := Self;
		Result := Html;
	finally
		Free;
	end;
end;

procedure TThCustomPanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
	inherited;
	Changed;
	//UnoverlapControls;
end;

procedure TThCustomPanel.UnoverlapControls;
var
	i, j: TThCtrlIterator;
begin
	i := TThCtrlIterator.Create(Self);
	try
		j := TThCtrlIterator.Create(Self);
		try
			ThAlignUtils.UnoverlapControls(i, j);
		finally
			j.Free;
		end;
	finally
		i.Free;
	end;
end;

procedure TThCustomPanel.SetShowGrid(const Value: Boolean);
begin
	FShowGrid := Value;
	Invalidate;
end;

procedure TThCustomPanel.SetShowOutline(const Value: Boolean);
begin
	FShowOutline := Value;
	Invalidate;
end;

function TThCustomPanel.GetOutlineColor: TColor;
begin
	Result := clBlack;
end;

procedure TThCustomPanel.SetVAlign(const Value: TThVAlign);
begin
	FVAlign := Value;
end;

{ TThPanel }

constructor TThPanel.Create(AOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle + [ csAcceptsControls ];
end;

procedure TThPanel.CellTag(inTag: TThTag);
begin
	inherited;
	case VAlign of
		vaMiddle, vaBottom: inTag.Attributes['valign'] := ssVAlign[VAlign];
	end;
	StylizeTag(inTag);
	ListJsAttributes(inTag.Attributes);
//	case VAlign of
//		vaMiddle: inTag.Attributes['valign'] = 'middle';
//		vaBottom: inTag.Attributes['valign'] = 'Bottom';
//	end;
end;

end.
