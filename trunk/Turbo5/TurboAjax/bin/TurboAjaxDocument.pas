unit TurboAjaxDocument;

interface

uses
	htMarkup,
	LrDocument,
	Design, TurboDocument, TurboDocumentController,
	Main;

type
	TTurboAjaxController = class(TTurboDocumentController)
	public
		class function GetDescription: string; override;
		class function GetExt: string; override;
	public
		function New: TLrDocument; override;
		//procedure DocumentActivate(inDocument: TLrDocument); override;
		//procedure DocumentDeactivate(inDocument: TLrDocument); override;
	end;
	//
	TTurboAjaxDocument = class(TTurboDocument)
  protected
		procedure IncludeTurbo(inMarkup: ThtMarkup);
	public
		constructor Create; override;
		destructor Destroy; override;
		//procedure Open(const inFilename: string); override;
		//procedure Save; override;
		//property Design: TDesignForm read FDesign;
		procedure Publish;
	end;

implementation

uses
	htAjaxPanel;

const
	cDescription = 'TurboAjax Document';
	cExt = '.ajx';

class function TTurboAjaxController.GetDescription: string;
begin
	Result := cDescription;
end;

class function TTurboAjaxController.GetExt: string;
begin
	Result := cExt;
end;

function TTurboAjaxController.New: TLrDocument;
begin
	Result := CreateDocument(TTurboAjaxDocument);
end;

{ TTurboAjaxDocument }

constructor TTurboAjaxDocument.Create;
begin
	inherited;
end;

destructor TTurboAjaxDocument.Destroy;
begin
	inherited;
end;

procedure TTurboAjaxDocument.IncludeTurbo(inMarkup: ThtMarkup);
begin
	with inMarkup do
	begin
		Styles.Add('body, html { margin: 0; padding: 0; overflow: hidden; }');
		AddJavaScript('djConfig = { isDebug: true, ieClobberMinimal: true }');
		AddJavaScriptInclude('/turboajax/dojo/dojo.js');
		AddJavaScriptInclude('/turboajax/turbo/turbo.js');
		AddJavaScriptInclude('/turboajax/turbo/lib/align.js');
		with AddJavaScript do
		begin
			Add(#13);
			Add('dojo.require("dojo.event.*");');
			Add('dojo.hostenv.setModulePrefix("turbo", "../turbo");');
			Add('dojo.require("turbo.widgets.TurboBox");');
			Add('dojo.require("turbo.widgets.TurboSplitter");');
			Add('');
			Add('dojo.addOnLoad(init);');
			Add('');
			Add('function init()');
			Add('{');
			Add('  window.setTimeout(delayedInit, 200);');
			Add('}');
			Add('function delayedInit()');
			Add('{');
			Add('  align();');
			Add('  dojo.event.connect(window, "onresize", "align");');
			Add('}');
			Add('function align()');
			Add('{');
			Add('  turbo.aligner.align();');
			Add('}');
		end;
	end;
end;

procedure TTurboAjaxDocument.Publish;
var
	markup: ThtMarkup;
begin
	markup := ThtMarkup.Create;
	try
		IncludeTurbo(markup);
		AjaxGenerateChildren(Design, markup.Body, markup);
		markup.SaveToFile('C:\Inetpub\wwwroot\turboajax\TurboStudio\publish\test.html');
	finally
		markup.Free;
	end;
end;

end.
