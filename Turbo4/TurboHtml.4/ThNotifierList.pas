unit ThNotifierList;

interface

uses
	Classes;

type
	TThNotifierList = class(TPersistent)
	private
		FNotifierList: TList;
		FEventList: TList;
	protected
		property EventList: TList read FEventList;
		property NotifierList: TList read FNotifierList;
	public
		constructor Create;
		destructor Destroy; override;
		procedure Add(inEvent: TNotifyEvent);
		procedure Remove(inEvent: TNotifyEvent);
		procedure Notify;
	end;

implementation

{ TThNotifierList }

constructor TThNotifierList.Create;
begin
	FNotifierList := TList.Create;
	FEventList := TList.Create;
end;

destructor TThNotifierList.Destroy;
begin
	FNotifierList.Free;
	FEventList.Free;
	inherited;
end;

procedure TThNotifierList.Add(inEvent: TNotifyEvent);
begin
	NotifierList.Add(TMethod(inEvent).Data);
	EventList.Add(TMethod(inEvent).Code);
end;

procedure TThNotifierList.Remove(inEvent: TNotifyEvent);
var
	i: Integer;
begin
	i := NotifierList.IndexOf(TMethod(inEvent).Data);
	if i >= 0 then
	begin
		NotifierList.Delete(i);
		EventList.Delete(i);
	end;
end;

procedure TThNotifierList.Notify;
var
	i: Integer;
	e: TNotifyEvent;
begin
	for i := 0 to Pred(NotifierList.Count) do
	begin
		TMethod(e).Data := NotifierList[i];
		TMethod(e).Code := EventList[i];
		e(Self);
	end;
end;

end.
