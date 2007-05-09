unit LrObserverList;

interface

uses
	SysUtils, Classes;

type
	TLrObserverList = class
	private
		FObservers: array of TNotifyEvent;
	protected
		function Find(inEvent: TNotifyEvent): Integer;
		function GetCount: Integer;
		procedure SetCount(const Value: Integer);
		property Count: Integer read GetCount write SetCount;
	public
		procedure Add(inEvent: TNotifyEvent);
		procedure Remove(inEvent: TNotifyEvent);
		procedure Notify(inSender: TObject);
		procedure NotifyExcept(inSender: TObject; inExcept: TObject);
	end;

implementation

{ TLrObserverList }

function TLrObserverList.GetCount: Integer;
begin
	Result := Length(FObservers);
end;

procedure TLrObserverList.SetCount(const Value: Integer);
begin
	SetLength(FObservers, Value);
end;

procedure TLrObserverList.Add(inEvent: TNotifyEvent);
begin
	if Find(inEvent) = -1 then
	begin
		Count := Count + 1;
		FObservers[Count - 1] := inEvent;
	end;
end;

	function LrCompareMethods(const inMethod0, inMethod1: TMethod): Boolean;
	begin
		Result := CompareMem(@inMethod0, @inMethod1, SizeOf(TMethod));
	end;

function TLrObserverList.Find(inEvent: TNotifyEvent): Integer;
var
	i: Integer;
begin
	Result := -1;
	for i := 0 to Pred(Count) do
	begin
		//if @FObservers[i] = @inEvent then
		//if TMethod(FObservers[i]) = TMethod(inEvent) then
		if LrCompareMethods(TMethod(FObservers[i]), TMethod(inEvent)) then
		begin
			Result := i;
			break;
		end;
	end;
end;

procedure TLrObserverList.Remove(inEvent: TNotifyEvent);
var
	i, j: Integer;
begin
	i := Find(inEvent);
	if (i >= 0) then
	begin
		for j := i to Pred(Pred(Count)) do
			FObservers[j] := FObservers[j + 1];
		Count := Count - 1;
	end;
end;

procedure TLrObserverList.Notify(inSender: TObject);
var
	i: Integer;
	e: TNotifyEvent;
begin
	for i := 0 to Pred(Count) do
	begin
		e := FObservers[i];
		e(inSender);
	end;
end;

procedure TLrObserverList.NotifyExcept(inSender, inExcept: TObject);
var
	i: Integer;
	e: TNotifyEvent;
begin
	for i := 0 to Pred(Count) do
	begin
		e := FObservers[i];
		if TMethod(e).Data <> inExcept then
			e(inSender);
	end;
end;

end.
