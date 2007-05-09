unit DebugView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IdBaseComponent, IdComponent, IdTCPServer, IdFTPServer,
  EasyEditor, EasyRichEditor, ComCtrls, ExtCtrls, ImgList;

type
  TDebugForm = class(TForm)
    IdFTPServer1: TIdFTPServer;
    Tree: TTreeView;
    ImageList1: TImageList;
    procedure IdFTPServer1UserLogin(ASender: TIdFTPServerThread;
      const AUsername, APassword: String; var AAuthenticated: Boolean);
    procedure IdFTPServer1BeforeCommandHandler(ASender: TIdTCPServer;
			const AData: String; AThread: TIdPeerThread);
		procedure IdFTPServer1Connect(AThread: TIdPeerThread);
		procedure IdFTPServer1Disconnect(AThread: TIdPeerThread);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
	private
		{ Private declarations }
		procedure DoConnect;
		procedure DoDebug;
		procedure DoDisconnect;
		procedure ProcessLines(inLines: TStrings);
	public
		{ Public declarations }
		Data: string;
		Lines: TStringList;
		procedure Clear;
	end;

var
	DebugForm: TDebugForm;

implementation

uses
	StrUtils;

{$R *.dfm}

const
	cDbgCmd = 'SITE ';
	cDbgCmdLen = 5;

procedure TDebugForm.FormCreate(Sender: TObject);
begin
	Lines := TStringList.Create;
end;

procedure TDebugForm.FormDestroy(Sender: TObject);
begin
	Lines.Free;
end;

procedure TDebugForm.IdFTPServer1UserLogin(ASender: TIdFTPServerThread;
	const AUsername, APassword: String; var AAuthenticated: Boolean);
begin
	AAuthenticated := true;
end;

procedure TDebugForm.IdFTPServer1BeforeCommandHandler(ASender: TIdTCPServer;
	const AData: String; AThread: TIdPeerThread);
begin
	if AnsiStartsStr(cDbgCmd, AData) then
	begin
		Data := Copy(AData, cDbgCmdLen, MAXINT);
		AThread.Synchronize(DoDebug);
	end;
end;

procedure TDebugForm.IdFTPServer1Connect(AThread: TIdPeerThread);
begin
	AThread.Synchronize(DoConnect);
end;

procedure TDebugForm.IdFTPServer1Disconnect(AThread: TIdPeerThread);
begin
	AThread.Synchronize(DoDisconnect);
end;

procedure TDebugForm.Clear;
begin
	Tree.Items.Clear;
end;

procedure TDebugForm.DoDebug;
begin
	Lines.Add(Data);
end;

procedure TDebugForm.DoConnect;
begin
	Lines.BeginUpdate;
	Lines.Clear;
end;

procedure TDebugForm.DoDisconnect;
begin
	Lines.EndUpdate;
	ProcessLines(Lines);
	//DebugEdit.Lines.Assign(Lines);
end;

procedure TDebugForm.ProcessLines(inLines: TStrings);
var
	node, n: TTreeNode;
	i: Integer;
	s, t: string;
begin
	Tree.Items.BeginUpdate;
	Screen.Cursor := crHourglass;
	try
		Tree.Items.Clear;
		node := Tree.Items.Add(nil, s);
		for i := 0 to Pred(inLines.Count) do
		begin
			s := Trim(inLines[i]);
			t := Copy(s, 1, 3);
			node.text := Copy(s, 4, MAXINT);
			if (t = '{!}') then
			begin
				node.ImageIndex := 0;
				node.Data := Pointer(1);
				node := Tree.Items.AddChild(node, '');
			end
			else if (t = '{+}') then
			begin
				node.ImageIndex := 1;
				node := Tree.Items.AddChild(node, '');
			end
			else if (t = '{-}') then
			begin
				n := node;
				node := Tree.Items.Add(node.Parent, '');
				n.Free;
			end
			else begin
				node.Text := s;
				node.ImageIndex := 2;
				node := Tree.Items.Add(node, '');
			end;
		end;
		node.Free;
		for i := 0 to Pred(Tree.Items.Count) do
			Tree.Items[i].Expanded := (Tree.Items[i].Data <> nil);
	finally
		Screen.Cursor := crDefault;
		Tree.Items.EndUpdate;
	end;
end;

procedure TDebugForm.TreeGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
	Node.SelectedIndex := Node.ImageIndex;
end;

end.
