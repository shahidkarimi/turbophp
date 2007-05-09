unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, ComCtrls, ToolWin, IdSimpleServer, ExtCtrls, XDOM_3_1;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    IdSimpleServer1: TIdSimpleServer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    Panel1: TPanel;
    Edit1: TEdit;
    SendButton: TButton;
    DisconnectButton: TToolButton;
    XmlToDomParser1: TXmlToDomParser;
    DomImplementation1: TDomImplementation;
    procedure ToolButton1Click(Sender: TObject);
    procedure IdSimpleServer1Status(ASender: TObject;
      const AStatus: TIdStatus; const AStatusText: String);
    procedure SendButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
	private
		{ Private declarations }
		FTid: Integer;
    function GetTid: Integer;
    procedure ParseXml(const inXml: string);
	public
		{ Public declarations }
		function XdReadBurst: string;
		function XdReadPacket: string;
		procedure XdSend(const inMsg: string);
		property Tid: Integer read GetTid;
	end;

var
	Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.XdReadBurst: string;
var
	c: Char;
begin
	Result := '';
	while true do
	begin
		c := IdSimpleServer1.ReadChar;
		if (c = #0) then
			break;
		Result := Result + c;
	end;
end;

function TForm1.XdReadPacket: string;
begin
	XdReadBurst;
	Result := XdReadBurst;
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
var
//	c: Char;
	s: string;
//	l, i: Integer;
begin
	if IdSimpleServer1.Listen then
		Memo1.Lines.Add('Connected');
	//
	s := XdReadBurst;
	Memo1.Lines.Add('Packet length = [' + s + ']');
	Memo1.Lines.Add('Packet:');
	//
	s := XdReadBurst;
	Memo1.Lines.Add(s);
	//
	Memo1.Lines.Add('');
	ParseXml(s);
	Memo1.Lines.Add('');
	//
{
	l := StrToInt(s);
	s := '';
//	for i := 0 to Pred(l) do
//		s := s + IdSimpleServer1.ReadChar;
	while true do
	begin
		c := IdSimpleServer1.ReadChar;
		if (c = #0) then
			break;
		s := s + c;
	end;
	Memo1.Lines.Add(s);
}
	//
	Memo1.Lines.Add('');
end;

procedure TForm1.XdSend(const inMsg: string);
var
	s: string;
begin
	s := inMsg + ' -i ' + IntToStr(Tid) + #0;
	IdSimpleServer1.Write(s);
end;

procedure TForm1.SendButtonClick(Sender: TObject);
begin
	XdSend(Edit1.Text);
	Memo1.Lines.Add(XdReadPacket);
end;

procedure TForm1.DisconnectButtonClick(Sender: TObject);
begin
	IdSimpleServer1.EndListen;
//	IdSimpleServer1.Disconnect;
end;

procedure TForm1.IdSimpleServer1Status(ASender: TObject;
	const AStatus: TIdStatus; const AStatusText: String);
begin
	Memo1.Lines.Add(AStatusText);
end;

function TForm1.GetTid: Integer;
begin
	Result := FTid;
	Inc(FTid);
end;

procedure TForm1.ParseXml(const inXml: string);
var
	doc: TdomDocument;
	walker: TdomTreeWalker;
	node: TdomNode;
begin
	doc := XmlToDomParser1.parseString(inXml, '', '', nil) as TdomDocument;
	walker := doc.createTreeWalker(doc.documentElement, SHOW_ALL, nil, false);
	try
		node := walker.currentNode;
		while node <> nil do
		begin
			Memo1.Lines.Add(node.nodeName);
			node := walker.nextNode;
		end;
	finally
		walker.Free;
	end;
end;

end.
