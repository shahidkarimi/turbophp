unit htTurboSplitter;

interface

uses
	Windows, SysUtils, Types, Classes, Controls, Graphics,
	htInterfaces, htTag, htMarkup, htControls, htAjaxPanel, htTurboBox;

type
	ThtTurboSplitter = class(ThtTurboGraphicControl)
	protected
		procedure Paint; override;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		procedure GenerateTag(inTag: ThtTag; inMarkup: ThtMarkup); override;
	published
		property Align;
		//property Style;
		//property Transparent;
		property Visible;
	end;

implementation

uses
	LrVclUtils, LrControlIterator, htPaint;

{ ThtTurboSplitter }

constructor ThtTurboSplitter.Create(inOwner: TComponent);
begin
	inherited;
end;

destructor ThtTurboSplitter.Destroy;
begin
	inherited;
end;

procedure ThtTurboSplitter.GenerateTag(inTag: ThtTag; inMarkup: ThtMarkup);
begin
	with inTag do
	begin
		Element := 'div';
		Attributes['turboAlign'] := AlignToAjaxAlign(Align);
		Attributes['dojoType'] := 'TurboSplitter';
		Attributes['id'] := Name;
	end;
	MarkupStyles(Style.InlineAttribute, inMarkup);
	//AjaxGenerateChildren(Self, inTag, inMarkup);
end;

procedure ThtTurboSplitter.Paint;
begin
	Canvas.Brush.Color := clSilver;
	Canvas.FillRect(BoxRect);
	inherited;
end;

end.
