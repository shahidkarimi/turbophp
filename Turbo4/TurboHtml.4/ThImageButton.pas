unit ThImageButton;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThInterfaces, ThCssStyle, ThStyledCtrl, ThWebControl, ThPicture, ThTag,
	ThAnchor, ThTextPaint, ThLabel, ThAttributeList, ThStyleList, ThImage;

type
	TThImageButton = class(TThCustomImage, IThFormInput)
	protected
		procedure Tag(inTag: TThTag); override;
	published
		property Align;
		property AltText;
		property Anchor;
		property AutoAspect;
		property AutoSize;
		property Border;
		property HAlign;
		property Picture;
		property Style;
		property StyleClass;
		property VAlign;
		property Visible;
	end;

implementation

procedure TThImageButton.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Element := 'input';
		Add('type', 'image');
	end;
end;

end.
