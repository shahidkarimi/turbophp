unit DebugThread;

interface

uses
	Classes, StdCtrls,
	IdBaseComponent, IdComponent, IdTCPConnection, IdSimpleServer,
	IdUDPBase, IdUDPServer, IdTrivialFTPServer;

type
	TServerType = TIdTrivialFTPServer;
	TDebugThread = class(TThread)
	private
		{ Private declarations }
		FMsg: string;
		FServer: TServerType;
	protected
		procedure SetServer(const Value: TServerType);
	protected
		procedure SMsg;
		procedure Execute; override;
		procedure Msg(const inMsg: string);
		procedure ServerStatus(ASender: TObject; const AStatus: TIdStatus;
			const AStatusText: String);
	public
		Memo: TMemo;
		procedure Listen;
		property Server: TServerType read FServer write SetServer;
	end;

implementation

{ Important: Methods and properties of objects in visual components can only be
	used in a method called using Synchronize, for example,

			Synchronize(UpdateCaption);

	and UpdateCaption could look like,

		procedure DebugThread.UpdateCaption;
		begin
			Form1.Caption := 'Updated in a thread';
		end; }

{ TDebugThread }

procedure TDebugThread.Execute;
begin
	{ Place thread code here }
	while not Terminated do
	begin
		Listen;
		Suspend;
	end;
end;

procedure TDebugThread.SMsg;
begin
	Memo.Lines.Add(FMsg);
end;

procedure TDebugThread.Msg(const inMsg: string);
begin
	FMsg := inMsg;
	Synchronize(SMsg);
end;

procedure TDebugThread.SetServer(const Value: TServerType);
begin
	FServer := Value;
	FServer.OnStatus := ServerStatus;
end;

procedure TDebugThread.ServerStatus(ASender: TObject; const AStatus: TIdStatus;
	const AStatusText: String);
begin
	Msg(AStatusText);
end;

procedure TDebugThread.Listen;
begin
	Msg('Listening...');
	//Server.Listen;
end;

end.
