unit ThHtmlDocument;

interface

uses
	Classes;

type
	TThHtmlDocument = class
	private
		FBody: TStringList;
		FDocType: string;
		FEmitHeaders: Boolean;
		FHeaders: TStringList;
		FHtml: TStringList;
		FStyles: TStringList;
		FScript: TStringList;
		FTitle: string;
	protected
		function GetHtml: TStringList;
		procedure SetBody(const Value: TStringList);
		procedure SetDocType(const Value: string);
		procedure SetHeaders(const Value: TStringList);
		procedure SetScript(const Value: TStringList);
		procedure SetStyles(const Value: TStringList);
		procedure SetTitle(const Value: string);
	protected
		procedure AddFooter;
		procedure AddHeader;
		procedure AddScript;
		procedure AddStyles;
		procedure AddTitle;
	public
		constructor Create;
		destructor Destroy; override;
	public
		property DocType: string read FDocType write SetDocType;
		property EmitHeaders: Boolean read FEmitHeaders write FEmitHeaders;
		property Headers: TStringList read FHeaders write SetHeaders;
		property Title: string read FTitle write SetTitle;
		property Script: TStringList read FScript write SetScript;
		property Styles: TStringList read FStyles write SetStyles;
		property Body: TStringList read FBody write SetBody;
		property Html: TStringList read GetHtml;
	end;

implementation

{ TThHtmlDocument }

constructor TThHtmlDocument.Create;
begin
	FDocType := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">';
	FBody := TStringList.Create;
	FHeaders := TStringList.Create;
	FHtml := TStringList.Create;
	FScript := TStringList.Create;
	FStyles := TStringList.Create;
end;

destructor TThHtmlDocument.Destroy;
begin
	FStyles.Free;
	FScript.Free;
	FHtml.Free;
	FHeaders.Free;
	FBody.Free;
	inherited;
end;

procedure TThHtmlDocument.SetBody(const Value: TStringList);
begin
	FBody.Assign(Value);
end;

procedure TThHtmlDocument.SetDocType(const Value: string);
begin
	FDocType := Value;
end;

procedure TThHtmlDocument.SetHeaders(const Value: TStringList);
begin
	FHeaders.Assign(Value);
end;

procedure TThHtmlDocument.SetScript(const Value: TStringList);
begin
	FScript.Assign(Value);
end;

procedure TThHtmlDocument.SetStyles(const Value: TStringList);
begin
	FStyles.Assign(Value);
end;

procedure TThHtmlDocument.SetTitle(const Value: string);
begin
	FTitle := Value;
end;

procedure TThHtmlDocument.AddTitle;
begin
	if FTitle <> '' then
		FHtml.Add('<title>' + FTitle + '</title>');
end;

procedure TThHtmlDocument.AddStyles;
begin
	if Styles.Count > 0 then
		with FHtml do
		begin
			Add('<style type="text/css">');
			Add('<!--');
			AddStrings(Styles);
			Add('-->');
			Add('</style>');
		end;
end;

procedure TThHtmlDocument.AddScript;
begin
	if Script.Count > 0 then
		with FHtml do
		begin
			Add('<script language="javascript" type="text/javascript">');
			Add('<!--');
			AddStrings(Script);
			Add('-->');
			Add('</script>');
		end;
end;

procedure TThHtmlDocument.AddHeader;
begin
	with FHtml do
	begin
		Add(DocType);
		Add('<html>');
		Add('<head>');
		AddTitle;
		AddStrings(Headers);
		AddStyles;
		AddScript;
		Add('</head>');
		Add('<body>');
	end;
end;

procedure TThHtmlDocument.AddFooter;
begin
	with FHtml do
	begin
		Add('</body>');
		Add('</html>');
	end;
end;

function TThHtmlDocument.GetHtml: TStringList;
begin
	FHtml.Clear;
	if EmitHeaders then
		AddHeader;
	FHtml.AddStrings(Body);
	if EmitHeaders then
		AddFooter;
	Result := FHtml;
end;

end.
