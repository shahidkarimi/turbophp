unit htMarkup;

interface

uses
	Classes, Contnrs,
	htTag;

type
	ThtMarkup = class
	private
		FDocType: string;
	protected
		procedure InitMarkup;
	public
		Body: ThtTag;
		Head: ThtTag;
		Includes: ThtTag;
		Styles: ThtTag;
		Script: ThtTag;
		//InitCode: ThtTag;
		//ResizeCode: ThtTag;
		constructor Create;
		destructor Destroy; override;
		function AddJavaScriptInclude(const inSrc: string = ''): ThtTag;
		function AddJavaScript(const inCode: string = ''): ThtTag;
		function CreateFunctionTag(const inFuncSig: string): ThtTag;
		function CreateJavaScriptNode: ThtTag;
		function CreateStyleNode: ThtTag;
		procedure Add(const inString: string);
		procedure Build(inStrings: TStrings);
		procedure SaveToFile(const inFilename: string);
		property DocType: string read FDocType write FDocType;
	end;

implementation

{ ThtMarkup }

constructor ThtMarkup.Create;
begin
	InitMarkup;
end;

destructor ThtMarkup.Destroy;
begin
	Head.Free;
	Body.Free;
	inherited;
end;

function ThtMarkup.CreateJavaScriptNode: ThtTag;
begin
	Result := ThtTag.Create('script', [ tfLineBreak ]);
	Result.Attributes['type'] := 'text/javascript';
	Result.Attributes['language'] := 'javascript';
end;

function ThtMarkup.CreateStyleNode: ThtTag;
begin
	Result := ThtTag.Create('style');
	Result.Attributes['type'] := 'text/css';
end;

procedure ThtMarkup.InitMarkup;
begin
	DocType := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">';
	//
	Styles := CreateStyleNode;
	//Styles.Content.Add('td { vertical-align: top; }');
	//
	Includes := ThtTag.Create;
	//
	Script := CreateJavaScriptNode;
	Script.Flags := Script.Flags + [ tfHideIfEmpty ];
	//
	//InitCode := CreateFunctionTag('init()');
	//ResizeCode := CreateFunctionTag('resize');
	//
	Head := ThtTag.Create('head', [ tfLineBreak ]);
	Head.Add(Styles);
	Head.Add(Includes);
	Head.Add(Script);
	//
	Body := ThtTag.Create('body', [ tfLineBreak ]);
	//Body.Attributes['onload'] := 'init()';
	//Body.Attributes['onresize'] := 'resize()';
end;

function ThtMarkup.CreateFunctionTag(const inFuncSig: string): ThtTag;
var
	tag: ThtTag;
begin
	tag := ThtTag.Create;
	tag.Content.Add('function ' + inFuncSig);
	tag.Content.Add('{');
	Script.Add(tag);
	Result := ThtTag.Create;
	Script.Add(Result);
	tag := ThtTag.Create;
	Script.Add(tag);
	tag.Content.Add('}');
end;

procedure ThtMarkup.Add(const inString: string);
begin
	Body.Add(inString);
end;

function ThtMarkup.AddJavaScript(const inCode: string): ThtTag;
begin
	Result := CreateJavaScriptNode;
	Result.Content.Text := inCode;
	Head.Add(Result);
end;

function ThtMarkup.AddJavaScriptInclude(const inSrc: string): ThtTag;
begin
	Result := CreateJavaScriptNode;
	Result.Attributes['src'] := inSrc;
	Head.Add(Result);
end;

procedure ThtMarkup.Build(inStrings: TStrings);
begin
	with inStrings do
	begin
		Add(DocType);
		Add('<html>');
		Add(Head.Html);
		Add(Body.Html);
		Add('</html>');
	end;
end;

procedure ThtMarkup.SaveToFile(const inFilename: string);
var
	s: TStringList;
begin
	s := TStringList.Create;
	try
		Build(s);
		s.SaveToFile(inFilename);
	finally
		s.Free;
	end;
end;

end.
