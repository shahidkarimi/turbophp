unit TpImageButton;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThTag, ThAnchor, ThImageButton,
	TpControls, TpAnchor;

type
	TTpImageButton = class(TThImageButton)
	private
		FOnClick: TTpEvent;
		FOnGenerate: TTpEvent;
		FUseAbsoluteUrl: Boolean;
	protected
//		function GetImageSrc: string; override;
	protected
		function CreateAnchor: TThAnchor; override;
		procedure Tag(inTag: TThTag); override;
	published
		property OnClick: TTpEvent read FOnClick write FOnClick;
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
		property UseAbsoluteUrl: Boolean read FUseAbsoluteUrl
			write FUseAbsoluteUrl;
	end;

implementation

uses
	ThPathUtils{, TpProject, DocumentManager};

{ TTpImageButton }

function TTpImageButton.CreateAnchor: TThAnchor;
begin
	Result := TTpAnchor.Create(Self);
end;

{
function TTpImageButton.GetImageSrc: string;
begin
	if UseAbsoluteUrl then
	begin
		Result := ExtractRelativePath(CurrentProject.Folder, Picture.PicturePath);
		Result := CurrentProject.Url + Result;
	end else
		Result := ExtractRelativePath(DocumentManagerForm.CurrentItem.Path,
			Picture.PicturePath);
	Result := ThPathToUrl(Result);
end;
}

procedure TTpImageButton.Tag(inTag: TThTag);
begin
	inherited;
	inTag.Add(tpClass, 'TTpImageButton');
	inTag.Add('tpName', Name);
	inTag.Add('tpOnClick', OnClick);
	inTag.Add('tpOnGenerate', OnGenerate);
end;

end.
