unit ThStructuredHtml;

interface

uses
	Classes,
	ThStructuredDocument;

type
	TThStructuredHtml = class(TThStructuredDocument)
	private
		FBodyOnly: Boolean;
		function GetHeaders: TStrings;
//		function GetScript: TStrings;
		function GetStyles: TStrings;
		function GetTitle: TStrings;
	protected
		function GetBody: TStrings;
	protected
		function ShouldPublish(const inSection: string): Boolean; override;
	public
		procedure NewDocument;
		procedure PublishToStrings(inStrings: TStrings); override;
	public
		property Body: TStrings read GetBody;
		property BodyOnly: Boolean read FBodyOnly write FBodyOnly;
		property Headers: TStrings read GetHeaders;
//		property Script: TStrings read GetScript;
		property Styles: TStrings read GetStyles;
		property Title: TStrings read GetTitle;
	end;

const
	shDocType = 'DocType';
	//shHtml = 'Html';
	shHead = 'Head';
	shTitle = 'Title';
	shHeaders = 'Headers';
	shStyles = 'Styles';
//	shScript = 'Script';
	shBody = 'Body';
	//shJsLibs = '*JsLibs';

implementation

{ TThStructuredHtml }

procedure TThStructuredHtml.NewDocument;
const
	cDocType = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">';
begin
	Clear;
	//Section[shDocType].
	Add(cDocType);
	Add('<html>');
	Add('<head>');
//	with Section[shHead] do
	begin
		Add('<title>');
		Section[shTitle];
		Add('</title>');
		Section[shHeaders];
		Add('<style type="text/css">');
		Add('<!--');
		Section[shStyles];
		Add('-->');
		Add('</style>');
//		Add('<script language="javascript" type="text/javascript">');
//		Add('<!--');
//		Section[shScript];
//		Add('-->');
//		Add('</script>');
	end;
	Add('</head>');
//	Add('<body>');
	Section[shBody];
//	Add('</body>');
	Add('</html>');
end;

function TThStructuredHtml.ShouldPublish(const inSection: string): Boolean;
begin
	Result := (inSection = '') or (inSection[1] <> '*');
	//Result := (inSection <> shJsLibs);
end;

procedure TThStructuredHtml.PublishToStrings(inStrings: TStrings);
begin
	if not BodyOnly then
		inherited
	else
		with Section[shBody] do
			PublishToStrings(inStrings);
end;

function TThStructuredHtml.GetBody: TStrings;
begin
	Result := Section[shBody];
end;

function TThStructuredHtml.GetHeaders: TStrings;
begin
	Result := Section[shHeaders];
end;

//function TThStructuredHtml.GetScript: TStrings;
//begin
//	Result := Section[shScript];
//end;

function TThStructuredHtml.GetStyles: TStrings;
begin
	Result := Section[shStyles];
end;

function TThStructuredHtml.GetTitle: TStrings;
begin
	Result := Section[shTitle];
end;

end.
