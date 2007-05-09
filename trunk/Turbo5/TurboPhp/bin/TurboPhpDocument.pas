unit TurboPhpDocument;

interface

uses
	SysUtils, Classes, Controls,
	LrDocument,
	htStyle, htDocument, htGenerator,
	Design, {Generator,} Project;

type
	TTurboPhpDocumentData = class(TComponent)
	private
		FJavaScript: TStringList;
		FPHP: TStringList;
		FBodyAttributes: string;
		FBodyStyle: ThtStyle;
	protected
		procedure SetBodyAttributes(const Value: string);
		procedure SetBodyStyle(const Value: ThtStyle);
		procedure SetJavaScript(const Value: TStringList);
		procedure SetPHP(const Value: TStringList);
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
	published
		property BodyStyle: ThtStyle read FBodyStyle write SetBodyStyle;
		property BodyAttributes: string read FBodyAttributes write SetBodyAttributes;
		property JavaScript: TStringList read FJavaScript write SetJavaScript;
		property PHP: TStringList read FPHP write SetPHP;
	end;
	//
	TTurboMarkup = class(ThtDocument)
	public
		procedure BuildHtml(inStrings: TStrings);
		procedure Generate(inDesignSurface: TWinControl);
	end;
	//
	TTurboJsPublisher = class
	private
		FJavaScript: TStringList;
	protected
		procedure NewJavaScriptSource;
		procedure PublishJavaScript(const inTarget: string);
	public
		constructor Create; 
		destructor Destroy; override;
	published
		property JavaScript: TStringList read FJavaScript write FJavaScript;
	end;
	//
	TTurboPhpDocument = class(TPublishableDocument)
	public
		class function DocumentExt: string;
		class function DocumentDescription: string;
	private
		FData: TTurboPhpDocumentData;
		FDesignForm: TDesignForm;
		PhpIncludes: TStringList;
		PublishPath: string;
	protected
		PublishName: string;
		function CreateMarkup: TTurboMarkup; virtual;
		function GetConfigTarget: string;
		function GetHtmlName: string;
		function GetHtmlTarget: string;
		function GetJavaScriptTarget: string;
		function GetPageName: string;
		function GetPhpName: string;
		function GetPhpTarget: string;
		procedure CustomizeMarkup(inMarkup: TTurboMarkup); virtual;
		procedure CreateData;
		procedure DoPublish; virtual;
		procedure InitializeIncludes;
		procedure NewPhpSource;
		procedure NewJavaScriptSource;
		procedure PublishHtml; virtual;
		procedure PublishJavaScript;
		procedure PublishPhp;
		procedure PublishPhpConfig;
	public
		constructor Create; override;
		destructor Destroy; override;
		function Publish(const inProject: TProject): string; override;
		procedure Activate; override;
		procedure Deactivate; override;
		procedure GenerateHtml(inStrings: TStrings);
		procedure New; override;
		procedure Open(const inFilename: string); override;
		procedure Save; override;
		property Data: TTurboPhpDocumentData read FData;
		property DesignForm: TDesignForm read FDesignForm;
		property PageName: string read GetPageName;
	end;

implementation

uses
	LrVclUtils, LrTagParser, Documents, TurboDocumentHost;

{ TTurboMarkup }

procedure TTurboMarkup.Generate(inDesignSurface: TWinControl);
begin
	with ThtGenerator.Create do
	try
		Generate(inDesignSurface, Self);
	finally
		Free;
	end;
end;

procedure TTurboMarkup.BuildHtml(inStrings: TStrings);
begin
	Build(inStrings);
	LrFormatHtml(inStrings);
end;

{ TTurboPhpDocumentData }

constructor TTurboPhpDocumentData.Create(inOwner: TComponent);
begin
	inherited;
	FBodyStyle := ThtStyle.Create(Self);
	FJavaScript := TStringList.Create;
	FPHP := TStringList.Create;
end;

destructor TTurboPhpDocumentData.Destroy;
begin
	FBodyStyle.Free;
	FPHP.Free;
	FJavaScript.Free;
	inherited;
end;

procedure TTurboPhpDocumentData.SetBodyAttributes(const Value: string);
begin
	FBodyAttributes := Value;
end;

procedure TTurboPhpDocumentData.SetBodyStyle(const Value: ThtStyle);
begin
	FBodyStyle.Assign(Value);
end;

procedure TTurboPhpDocumentData.SetJavaScript(const Value: TStringList);
begin
	FJavaScript.Assign(Value);
end;

procedure TTurboPhpDocumentData.SetPHP(const Value: TStringList);
begin
	FPHP.Assign(Value);
end;

{ TTurboJsPublisher }

constructor TTurboJsPublisher.Create;
begin
	inherited;
end;

destructor TTurboJsPublisher.Destroy;
begin
	inherited;
end;

procedure TTurboJsPublisher.NewJavaScriptSource;
begin
	with JavaScript do
		Add('// JavaScript');
end;

procedure TTurboJsPublisher.PublishJavaScript(const inTarget: string);
begin
	JavaScript.SaveToFile(ChangeFileExt(inTarget, '.js'));
end;

{ TTurboPhpDocument }

class function TTurboPhpDocument.DocumentDescription: string;
begin
	Result := 'TurboPhp Pages';
end;

class function TTurboPhpDocument.DocumentExt: string;
begin
	Result := '.tphp';
end;

constructor TTurboPhpDocument.Create;
begin
	inherited;
	EnableSaveAs(DocumentDescription, DocumentExt);
	FDesignForm := TDesignForm.Create(nil);
	DesignForm.Visible := false;
	DesignForm.SetBounds(12, 12, 800 - 12, 600 - 12);
	CreateData;
	PhpIncludes := TStringList.Create;
end;

destructor TTurboPhpDocument.Destroy;
begin
	PhpIncludes.Free;
	DesignForm.Free;
	inherited;
end;

procedure TTurboPhpDocument.CreateData;
begin
	FData := TTurboPhpDocumentData.Create(FDesignForm);
	FData.Name := 'Document';
end;

procedure TTurboPhpDocument.Activate;
begin
	inherited;
	TurboDocumentHostForm.Document := Self;
end;

procedure TTurboPhpDocument.Deactivate;
begin
	inherited;
	TurboDocumentHostForm.Document := nil;
end;

function TTurboPhpDocument.GetPageName: string;
begin
	Result := ChangeFileExt(DisplayName, '');
end;

procedure TTurboPhpDocument.NewPhpSource;
begin
	with Data.Php do
	begin
		Add('<?php');
		Add('');
		Add('// Configure');
		Add('');
		Add('include_once("support/Page%%.config.php");');
		Add('');
		Add('// Define page class');
		Add('');
		Add('class TPage%% extends TTpPage');
		Add('{');
		Add('  // Implementation');
		Add('}');
		Add('');
		Add('// Create and display page');
		Add('');
		Add('$Page%% = new TPage%%($tpTemplateFile);');
		Add('$Page%%->Run();');
		Add('');
		Add('?>');
	end;
end;

procedure TTurboPhpDocument.NewJavaScriptSource;
begin
	with Data.JavaScript do
	begin
		Add('// JavaScript');
	end;
end;

procedure TTurboPhpDocument.New;
begin
	NewJavaScriptSource;
	NewPhpSource;
end;

procedure TTurboPhpDocument.Open(const inFilename: string);
begin
	Filename := inFilename;
	DesignForm.LoadFromFile(Filename);
	LrFindComponentByClass(FData, DesignForm, TTurboPhpDocumentData);
	if FData = nil then
		CreateData;
	inherited;
end;

procedure TTurboPhpDocument.Save;
begin
	if Filename <> '' then
	begin
		DesignForm.SaveToFile(Filename);
		inherited;
	end;
end;

function TTurboPhpDocument.GetHtmlName: string;
begin
	Result := ChangeFileExt(PublishName, '.html');
end;

function TTurboPhpDocument.GetHtmlTarget: string;
begin
	Result := PublishPath + 'support\' + GetHtmlName;
end;

function TTurboPhpDocument.GetPhpName: string;
begin
	Result := ChangeFileExt(PublishName, '.php');
end;

function TTurboPhpDocument.GetPhpTarget: string;
begin
	Result := PublishPath + GetPhpName;
end;

function TTurboPhpDocument.GetJavaScriptTarget: string;
begin
	Result := PublishPath + 'support\' + ChangeFileExt(PublishName, '.js');
end;

function TTurboPhpDocument.GetConfigTarget: string;
begin
	Result := PublishPath + 'support\'
		+ ChangeFileExt(PublishName, '.config.php');
end;

procedure TTurboPhpDocument.InitializeIncludes;
begin
	PhpIncludes.Clear;
	PhpIncludes.Add('TpParser.php');
	PhpIncludes.Add('TpLib.php');
//	if Page.Debug then
//		PhpIncludes.Add('TpDebug.php');
	//
//	with TThComponentIterator.Create(DesignForm) do
//	try
//		while Next do
//			if IsAs(Component, ITpIncludeLister, l) then
//				l.ListPhpIncludes(PhpIncludes);
//	finally
//		Free;
//	end;
//	DeDupeStrings(PhpIncludes);
end;

procedure TTurboPhpDocument.PublishPhpConfig;
const
	cLibPath = '$tpLibPath';
	cSupportPath = '$tpSupportPath';
	cTemplate = '$tpTemplateFile';
var
	i: Integer;
begin
	with TStringList.Create do
	try
		Add('<?php');
		Add('');
		//Add(cSupportPath + ' = "' + SupportFolder + '";');
		//Add(cLibPath + ' = "' + RemoteLibFolder + '";');
		Add(cSupportPath + ' = "' + 'support/' + '";');
		Add(cLibPath + ' = "' + 'lib/' + '";');
		Add(cTemplate + ' = $tpSupportPath . "' + GetHtmlName + '";');
		Add('');
		for i := 0 to Pred(PhpIncludes.Count) do
			Add('include_once($tpLibPath . "' + PhpIncludes[i] + '");');
		Add('');
		Add('?>');
		SaveToFile(GetConfigTarget);
	finally
		Free;
	end;
end;

procedure TTurboPhpDocument.PublishPhp;
var
	i: Integer;
	s: TStringList;
begin
	s := TStringList.Create;
	with Data.Php do
	try
		for i := 0 to Pred(Count) do
			s.Add(
				StringReplace(Strings[i], 'Page%%', GetPageName, [ rfReplaceAll ]));
		s.SaveToFile(GetPhpTarget);
	finally
		s.Free;
	end;
end;

procedure TTurboPhpDocument.PublishJavaScript;
begin
	Data.JavaScript.SaveToFile(GetJavaScriptTarget);
end;

function TTurboPhpDocument.CreateMarkup: TTurboMarkup;
begin
	Result := TTurboMarkup.Create;
end;

procedure TTurboPhpDocument.CustomizeMarkup(inMarkup: TTurboMarkup);
begin
	//
end;

procedure TTurboPhpDocument.GenerateHtml(inStrings: TStrings);
var
	markup: TTurboMarkup;
begin
	markup := CreateMarkup;
	try
		markup.Generate(DesignForm);
		CustomizeMarkup(markup);
		inStrings.Clear;
		markup.BuildHtml(inStrings);
	finally
		markup.Free;
	end;
end;

procedure TTurboPhpDocument.PublishHtml;
var
	html: TStringList;
begin
	html := TStringList.Create;
	try
		GenerateHtml(html);
		html.SaveToFile(GetHtmlTarget);
	finally
		html.Free;
	end;
end;

procedure TTurboPhpDocument.DoPublish;
begin
	InitializeIncludes;
	PublishHtml;
	PublishPhpConfig;
	PublishPhp;
	PublishJavaScript;
end;

{
procedure TTurboPhpDocument.Generate(const inTarget: string);
begin
	PublishPath := ExtractFilePath(inTarget);
	PublishName := ExtractFileName(inTarget);
	InitializeIncludes;
	PublishHtml;
	PublishPhpConfig;
	PublishPhp;
	PublishJavaScript;
end;
}

function TTurboPhpDocument.Publish(const inProject: TProject): string;
begin
	PublishPath := inProject.PublishFolder[Self];
	PublishName := PageName;
	DoPublish;
	Result := inProject.PublishUrl[Self] + GetPhpName;
end;

initialization
	RegisterClass(TTurboPhpDocumentData);
end.
