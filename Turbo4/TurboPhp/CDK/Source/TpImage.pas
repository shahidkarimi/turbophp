unit TpImage;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThTag, ThImage, ThAnchor,
	TpControls, TpAnchor;

type
	TTpCustomImage = class(TThCustomImage)
	private
		FOnGenerate: TTpEvent;
    FUseAbsoluteUrl: Boolean;
	protected
		function GetImageSrc: string; override;
		procedure SetOnGenerate(const Value: TTpEvent);
	protected
		function CreateAnchor: TThAnchor; override;
		procedure Tag(inTag: TThTag); override;
	protected
		property OnGenerate: TTpEvent read FOnGenerate write SetOnGenerate;
		property UseAbsoluteUrl: Boolean read FUseAbsoluteUrl
			write FUseAbsoluteUrl;
	end;
	//
	TTpImage = class(TTpCustomImage)
	published
		property Align;
		property AltText;
		property Anchor;
		property AutoAspect;
		property AutoSize;
		property HAlign;
		property Picture;
		property OnGenerate;
		property Style;
		property StyleClass;
		property UseAbsoluteUrl;
		property VAlign;
		property Visible;
	end;

implementation

uses
	ThPathUtils{, TpProject, DocumentManager};

{ TTpCustomImage }

function TTpCustomImage.CreateAnchor: TThAnchor;
begin
	Result := TTpAnchor.Create(Self);
end;

procedure TTpCustomImage.SetOnGenerate(const Value: TTpEvent);
begin
	FOnGenerate := Value;
end;

function TTpCustomImage.GetImageSrc: string;
begin
	if Picture.PictureUrl <> '' then
		Result := Picture.PictureUrl
	else begin
//		Result := ExtractRelativePath(CurrentProject.Folder, Picture.PicturePath);
//		if UseAbsoluteUrl then
//			Result := CurrentProject.Url + Result;
	end;
{
	if UseAbsoluteUrl then
	begin
		Result := ExtractRelativePath(CurrentProject.Folder, Picture.PicturePath);
		Result := CurrentProject.Url + Result;
	end else
		Result := ExtractRelativePath(DocumentManagerForm.CurrentItem.Path,
			Picture.PicturePath);
}
	Result := ThPathToUrl(Result);
end;

procedure TTpCustomImage.Tag(inTag: TThTag);
begin
	inherited;
	inTag.Add(tpClass, 'TTpImage');
	inTag.Add('tpName', Name);
	inTag.Add('tpOnGenerate', OnGenerate);
end;

end.
