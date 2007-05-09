unit ThAction;

interface

uses
	SysUtils, Classes, Forms, ThComponent;

type
	TThAction = class;
	//
	TThRequest = class
	public
		Document: string;
	end;
	//
	TThResponse = class
	public
		ContentText: string;
		ContentType: string;
		ContentStream: TStream;
	end;
	//
	TThCustomResource = class(TThComponent)
	protected
		function GetPrefix: string; virtual; abstract;
	public
		procedure Fulfill(inAction: TThAction); virtual; abstract;
	public
		property Prefix: string read GetPrefix;
	end;
	//
	TThSession = class(TThComponent)
	public
		procedure CloseSession; virtual; abstract;
		procedure OpenSession(inAction: TThAction); virtual; abstract;
	end;
	//
	TThThread = TThread;
	//
	TThAction = class
	public
		Document: string;
		Handled: Boolean;
		Request: TThRequest;
		Response: TThResponse;
		Session: TThSession;
		Target: TComponent;
		Thread: TThThread;
	protected
		procedure CloseSession;
		procedure OpenSession;
	public
		constructor Create(inThread: TThThread;
			inRequest: TThRequest; inResponse: TThResponse);
		destructor Destroy; override;
		procedure UseResource(inResource: TThCustomResource);
		procedure ServeStream(inStream: TStream; const inContentType: string);
	end;

//var
//	Action: TThAction;

implementation

{ TThAction }

constructor TThAction.Create(inThread: TThThread; inRequest: TThRequest;
	inResponse: TThResponse);
begin
	Thread := inThread;
	Response := inResponse;
	Request := inRequest;
	Document := Request.Document;
	//Document := Copy(Request.Document, 2, MAXINT);
end;

destructor TThAction.Destroy;
begin
	inherited;
end;

{
procedure TThAction.ExplodeDocumentPath(const inDocument: string);
var
	s, d: string;
	p: Integer;
begin
	s := '/';
	d := inDocument;
	p := Pos(s, d);
	while (p > 0) do
	begin
		DocumentPath.Add(Copy(d, 1, p - 1));
		d := Copy(d, p + 1, MAXINT);
		p := Pos(s, d);
	end;
end;
}


{
procedure TThAction.PopContentName;
begin
	ContentName := ThPopFromUrl(Document);
end;

procedure TThAction.Dispatch(inOwner: TComponent);
begin
	Target := inOwner;
	ContentName := TrbPopFromUrl(Document);
	Wrangle;
end;

procedure TThAction.Wrangle;
var
	w: TThActionWrangler;
begin
	w := TThActionWrangler(
		FindComponentByClass(Target, TThActionWrangler));
	if w <> nil then
		w.Wrangle(Self);
end;
}

procedure TThAction.OpenSession;
begin
	if Session <> nil then
		Session.OpenSession(Self);
end;

procedure TThAction.CloseSession;
begin
	if Session <> nil then
		Session.CloseSession;
end;

procedure TThAction.UseResource(inResource: TThCustomResource);
begin
	OpenSession;
	inResource.Fulfill(Self);
	CloseSession;
end;

procedure TThAction.ServeStream(inStream: TStream;
	const inContentType: string);
begin
	Response.ContentStream := inStream;
	Response.ContentType := inContentType;
	Handled := true;
end;

end.
