unit TurboAjaxDocument;

interface

uses
	SysUtils, Classes,
	htTag, htDocument, 
	LrDocument, TurboPhpDocument,
	Design, Project;

type
	TTurboAjaxDocument = class(TTurboPhpDocument)
	public
		class function DocumentDescription: string;
		class function DocumentExt: string;
	protected
		function CreateMarkup: TTurboMarkup; override;
		procedure CustomizeMarkup(inMarkup: TTurboMarkup); override;
	public
		constructor Create; override;
	end;

implementation

uses
	Globals;

{ TTurboAjaxDocument }

class function TTurboAjaxDocument.DocumentDescription: string;
begin
	Result := 'TurboAjax Pages';
end;

class function TTurboAjaxDocument.DocumentExt: string;
begin
	Result := '.tajx';
end;

constructor TTurboAjaxDocument.Create;
begin
	inherited;
	EnableSaveAs(DocumentDescription, DocumentExt);
end;

function TTurboAjaxDocument.CreateMarkup: TTurboMarkup;
begin
	Result := inherited CreateMarkup;
end;

procedure TTurboAjaxDocument.CustomizeMarkup(inMarkup: TTurboMarkup);

	function Include(const inSrc: string): ThtTag;
	begin
		Result := ThtTag.Create();
		Result.Content.LoadFromFile(inSrc);
	end;

{
	function JsIncludeTag(const inSrc: string): ThtTag;
	begin
		Result := ThtTag.Create('script');
		Result.Attributes['type'] := 'text/javascript';
		Result.Attributes['language'] := 'javascript';
		Result.Attributes['src'] := inSrc;
	end;

	function StyleLinkTag(const inSrc: string): ThtTag;
	begin
		Result := ThtTag.Create('link');
		Result.Attributes['type'] := 'text/css';
		Result.Attributes['rel'] := 'stylesheet';
		Result.Attributes['href'] := inSrc;
	end;
}

begin
	inMarkup.Includes.Add(Include(TemplatesHome + 'dojoboiler.html'));
	inMarkup.Script.Add('  dojo.hostenv.modulesLoadedListeners.push(init);');

{
	with inMarkup do
	begin
		Includes.Add(StyleLinkTag('lib/Grid.css'));
		Includes.Add(JsIncludeTag('lib/Grid.js'));
		Includes.Add(JsIncludeTag('lib/turbo_ajax_server.php?client'));
		Includes.Add(JsIncludeTag('support/' + ChangeFileExt(PublishName, '.js')));
		Includes.Add(JsIncludeTag('lib/tpGrid.js'));
	end;
}
end;

end.
