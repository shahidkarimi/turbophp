unit PageDocument;

interface

uses
	SysUtils, Classes, Forms,
	dcfdes,
	LrDocument, ThComponentIterator, ThInterfaces, TpInterfaces,
	DesignView, PhpPage;

type
	TPageDocument = class(TLrDocument)
	private
		FPublishFolder: string;
		FLibFolder: string;
	protected
		function GetExtension: string; override;
		function GetFilter: string; override;
		function GetHtmlFilename: string;
		function GetJsFilename: string;
		function GetPageName: string;
		function GetPhpConfigFilename: string;
		function GetPhpFilename: string;
		function GetPublishFolder: string;
		function GetRemoteLibFolder: string;
		function GetStyleFilename: string;
		function GetSupportFolder: string;
	protected
		procedure GeneratePageSource;
		procedure ListPhpIncludes;
		procedure Load;
		procedure PageNameChange(inSender: TObject);
		procedure PublishConfigs;
		procedure PublishHtml;
		procedure PublishJavaScript;
		procedure PublishLibs;
		procedure PublishPhp;
		procedure PublishPhpConfig;
		procedure PublishStyles(inStyles: TStringList);
		procedure PublishStyleSheet;
	public
		DesignForm: TDesignForm;
		Html: TStringList;
		OldName: string;
		Page: TPhpPage;
		PhpIncludes: TStringList;
	public
		constructor Create(inManager: TLrDocumentManager;
			const inPath: string = ''); override;
		destructor Destroy; override;
		function GenerateHtml: TStringList;
		procedure LazyUpdate; override;
		procedure Open; override;
		procedure Publish;
		procedure Save; override;
		procedure UpdatePageName(inStrings: TStrings);
		procedure ValidateLimitInfos;
	public
		property HtmlFilename: string read GetHtmlFilename;
		property LibFolder: string read FLibFolder write FLibFolder;
		property PhpFilename: string read GetPhpFilename;
		property PublishFolder: string read GetPublishFolder write FPublishFolder;
		property RemoteLibFolder: string read GetRemoteLibFolder;
		property SupportFolder: string read GetSupportFolder;
	end;

const
	//cPageDocumentFileExtension = '.tppage';
	cPageDocumentFileExtension = '.tphp';

implementation

uses
	LrUtils, ThPathUtils, Config;

{ TPageDocument }

var
	singletonPlaceholderForm: TForm;

function PlaceholderForm: TForm;
begin
	if singletonPlaceholderForm = nil then
		singletonPlaceholderForm := TForm.Create(nil);
	Result := singletonPlaceholderForm;
end;

constructor TPageDocument.Create(inManager: TLrDocumentManager;
	const inPath: string);
begin
	inherited;
	//
	PhpIncludes := TStringList.Create;
	//PhpIncludes.Sorted := true;
	//PhpIncludes.Duplicates := dupIgnore;
	//
	DesignForm := TDesignForm.Create(Application);
	DesignForm.Parent := PlaceholderForm;
	DesignForm.DCLiteDesigner.OnChange := ChangeEvent;
	//
	Page := TPhpPage.Create(DesignForm);
	GeneratePageSource;
	//
	ValidateLimitInfos;
end;

destructor TPageDocument.Destroy;
begin
	DesignForm.Free;
	PhpIncludes.Free;
	inherited;
end;

function TPageDocument.GetExtension: string;
begin
	Result := cPageDocumentFileExtension;
end;

function TPageDocument.GetFilter: string;
begin
	Result := Format('TurboPhp Files (*%s)|*%0:s', [ Extension ]);
end;

procedure TPageDocument.ValidateLimitInfos;
begin
	DesignForm.SetLimitInfo(0, DesignForm);
	DesignForm.SetLimitInfo(1, Page, [ atSelect, atEdit ]);
end;

function TPageDocument.GetPageName: string;
begin
	Result := DisplayName;
end;

procedure TPageDocument.GeneratePageSource;
begin
	with Page.PhpSource do
	begin
		Add('<?php');
		Add('');
		Add('// Configure');
		Add('');
		Add('include_once("page.config.php");');
		Add('');
		Add('// Define page class');
		Add('');
		Add('class T' + GetPageName + ' extends TTpPage');
		Add('{');
		Add('}');
		Add('');
		Add('// Create and display page');
		Add('');
		Add('$' + GetPageName + ' = new T' + GetPageName + '($tpTemplateFile);');
		Add('$' + GetPageName + '->Run();');
		Add('');
{
		Add('// Initialize application');
		Add('');
		Add('$app = new TTpApp($tpTemplateFile);');
		Add('$app->ParseTemplate();');
		Add('');
		Add('// Generate output');
		Add('');
		Add('echo $app->Generate();');
		Add('');
		Add('// User event functions here');
		Add('');
}
		Add('?>');
	end;
	OldName := GetPageName;
end;

procedure TPageDocument.PageNameChange(inSender: TObject);
begin
//	if (OldName <> '') then
//		OldName := Page.Name;
end;

procedure TPageDocument.UpdatePageName(inStrings: TStrings);
var
	i: Integer;
begin
	if (OldName <> GetPageName) then
	begin
		with inStrings do
			for i := 0 to Pred(Count) do
			begin
				Strings[i] := StringReplace(Strings[i], '$' + OldName, '$' + GetPageName, [ rfReplaceAll ]);
				Strings[i] := StringReplace(Strings[i], 'T' + OldName, 'T' + GetPageName, [ rfReplaceAll ]);
			end;
		OldName := GetPageName;
	end;
end;

procedure TPageDocument.LazyUpdate;
begin
	inherited;
end;

procedure TPageDocument.Open;
begin
	inherited;
	if not Untitled then
		Load;
	OldName := GetPageName;
end;

procedure TPageDocument.Load;
begin
	FreeAndNil(Page);
	DesignForm.LoadFromFile(Path);
	FindComponentByClass(Page, TPhpPage, DesignForm);
	ValidateLimitInfos;
	inherited;
	DesignForm.DCLiteDesigner.OnChange := ChangeEvent;
	Modified := false;
end;

procedure TPageDocument.Save;
begin
	//UpdatePageName;
	inherited;
	DesignForm.SaveToFile(Path);
end;

function TPageDocument.GetPublishFolder: string;
begin
	Result := FPublishFolder;
end;

function TPageDocument.GetSupportFolder: string;
begin
	Result := 'support/';
end;

function TPageDocument.GetRemoteLibFolder: string;
begin
	Result := ThPathToUrl(ExtractRelativePath(PublishFolder, LibFolder));
end;

function TPageDocument.GetJsFilename: string;
begin
	Result := SupportFolder + ChangeFileExt(FileName, '.js')
end;

function TPageDocument.GetStyleFilename: string;
begin
	if Page.StyleSheet = nil then
		Result := ''
	else
		Result := SupportFolder + ChangeFileExt(FileName, '.css')
end;

function TPageDocument.GetHtmlFilename: string;
begin
	Result := ChangeFileExt(FileName, '.html');
end;

function TPageDocument.GetPhpFilename: string;
begin
	Result := Page.PhpFilename;
	if Result = '' then
		Result := FileName;
	Result := ChangeFileExt(Result, '.php');
end;

function TPageDocument.GetPhpConfigFilename: string;
begin
	Result := SupportFolder + ChangeFileExt(GetPhpFilename, '.config.php');
end;

procedure TPageDocument.ListPhpIncludes;
var
	l: ITpIncludeLister;
begin
	PhpIncludes.Clear;
	PhpIncludes.Add('TpParser.php');
	PhpIncludes.Add('TpLib.php');
	if Page.Debug then
		PhpIncludes.Add('TpDebug.php');
	//
	with TThComponentIterator.Create(DesignForm) do
	try
		while Next do
			if IsAs(Component, ITpIncludeLister, l) then
				l.ListPhpIncludes(PhpIncludes);
	finally
		Free;
	end;
	//
	DeDupeStrings(PhpIncludes);
end;

procedure TPageDocument.PublishLibs;
var
	s: TStringList;
	i: Integer;

	procedure PublishFile(const inFilename: string);
	begin
		if FileExists(LibSourceFolder + inFilename) then
		begin
			s.LoadFromFile(LibSourceFolder + inFilename);
			s.SaveToFile(LibFolder + inFilename);
		end;
	end;

begin
	s := TStringList.Create;
	try
		NeedFolder(LibFolder);
		for i := 0 to Pred(PhpIncludes.Count) do
			PublishFile(PhpIncludes[i]);
		PublishFile('TpDebug.php');
//		PublishFile('TpDb.php');
//		PublishFile('TpMySql.php');
	finally
		s.Free;
	end;
end;

function TPageDocument.GenerateHtml: TStringList;
begin
	Result := TStringList.Create;
	Result.Text := Page.Html;
end;

procedure TPageDocument.PublishHtml;
begin
	Page.JsUrl := ThPathToUrl(GetJsFilename);
	Page.StyleUrl := ThPathToUrl(GetStyleFilename);
	with GenerateHtml do
	try
		SaveToFile(PublishFolder + SupportFolder + HtmlFilename);
	finally
		Free;
	end;
end;

procedure TPageDocument.PublishJavaScript;
var
	j: TStringList;
	l: ITpJsWriter;
begin
	j := TStringList.Create;
	try
		with TThComponentIterator.Create(DesignForm) do
		try
			while Next do
				if IsAs(Component, ITpJsWriter, l) then
					l.WriteJavaScript(j);
		finally
			Free;
		end;
		j.AddStrings(Page.JsSource);
		j.SaveToFile(PublishFolder + GetJsFilename);
	finally
		j.Free;
	end;
end;

procedure TPageDocument.PublishStyles(inStyles: TStringList);
var
	s: IThStyleSource;
begin
	with TThComponentIterator.Create(DesignForm) do
	try
		while Next do
			if IsAs(Component, IThStyleSource, s) then
				s.PublishStyles(inStyles);
	finally
		Free;
	end;
end;

procedure TPageDocument.PublishStyleSheet;
var
	s: TStringList;
begin
	if Page.StyleSheet <> nil then
	begin
		s := TStringList.Create;
		try
			Page.StyleSheet.Styles.GenerateStyles(s);
			Page.AnchorStyles.GenerateStyles(s, Page.StyleSheet);
			PublishStyles(s);
			s.SaveToFile(PublishFolder + GetStyleFilename);
		finally
			s.Free;
		end;
	end;
end;

procedure TPageDocument.PublishConfigs;
var
	w: ITpConfigWriter;
begin
	with TThComponentIterator.Create(DesignForm) do
	try
		while Next do
			if IsAs(Component, ITpConfigWriter, w) then
				w.WriteConfig(PublishFolder + SupportFolder);
	finally
		Free;
	end;
{
	with TThComponentIterator.Create(DesignForm) do
	try
		while Next(TTpMySql) do
		begin
			TTpMySql(Component).WriteConfig(inFolder);
		end;
	finally
		Free;
	end;
}
end;

procedure TPageDocument.PublishPhpConfig;
const
	cLibPath = '$tpLibPath';
	cSupportPath = '$tpSupportPath';
	cTemplate = '$tpTemplateFile';
var
	config: TStringList;
	i: Integer;
begin
	config := TStringList.Create;
	try
		config.Add('<?php');
		config.Add('');
		config.Add(cSupportPath + ' = "' + SupportFolder + '";');
		config.Add(cLibPath + ' = "' + RemoteLibFolder + '";');
		config.Add(cTemplate + ' = $tpSupportPath . "' + HtmlFilename + '";');
		config.Add('');
		for i := 0 to Pred(PhpIncludes.Count) do
			config.Add('include_once($tpLibPath . "' + PhpIncludes[i] + '");');
		config.Add('');
		config.Add('?>');
		config.SaveToFile(PublishFolder + GetPhpConfigFilename);
	finally
		config.Free;
	end;
end;

procedure TPageDocument.PublishPhp;
begin
	Page.PhpSource.Strings[4] := 'include_once("' + GetPhpConfigFilename + '");';
	Page.PhpSource.SaveToFile(PublishFolder + GetPhpFilename);
end;

procedure TPageDocument.Publish;
begin
	LazyUpdate;
	//
	NeedFolder(LibFolder);
	NeedFolder(PublishFolder);
	NeedFolder(PublishFolder + SupportFolder);
	//
	ListPhpIncludes;
	//
	PublishConfigs;
	PublishHtml;
	PublishPhpConfig;
	PublishPhp;
	PublishLibs;
	PublishJavaScript;
	PublishStyleSheet;
//	PublishImages(PublishFolder + ImagesFolder);
end;

end.
