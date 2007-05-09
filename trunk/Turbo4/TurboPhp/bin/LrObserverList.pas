unit LrObserverList;

interface

uses
	Classes;

type
	TLrObserverList = class(TPersistent)
	private
		FObserverList: TList;
		FEventList: TList;
	protected
		property EventList: TList read FEventList;
		property ObserverList: TList read FObserverList;
	public
		constructor Create;
		destructor Destroy; override;
		procedure Add(inEvent: TNotifyEvent);
		procedure Remove(inEvent: TNotifyEvent);
		procedure Notify;
	end;

implementation

{ TLrObserverList }

constructor TLrObserverList.Create;
begin
	FObserverList := TList.Create;
	FEventList := TList.Create;
end;

destructor TLrObserverList.Destroy;
begin
	FObserverList.Free;
	FEventList.Free;
	inherited;
end;

procedure TLrObserverList.Add(inEvent: TNotifyEvent);
begin
	ObserverList.Add(TMethod(inEvent).Data);
	EventList.Add(TMethod(inEvent).Code);
end;

procedure TLrObserverList.Remove(inEvent: TNotifyEvent);
var
	i: Integer;
begin
	i := ObserverList.IndexOf(TMethod(inEvent).Data);
	if i >= 0 then
	begin
		ObserverList.Delete(i);
		EventList.Delete(i);
	end;
end;

procedure TLrObserverList.Notify;
var
	i: Integer;
	e: TNotifyEvent;
begin
	for i := 0 to Pred(ObserverList.Count) do
	begin
		TMethod(e).Data := ObserverList[i];
		TMethod(e).Code := EventList[i];
		e(Self);
	end;
end;

end.
 