unit DesignManager;

interface

uses
	Windows, Messages, Classes, Controls, Forms,
	LrObserverList;

const
	LRM_PROPCHANGE = WM_USER + $01;

type
	TObjectArray = array of TObject;
	//
	TDesignFilterEvent = procedure(inSender: TObject; inObject: TObject;
		var inFilter: Boolean) of object;
	TDesignGetAddClassEvent = procedure(Sender: TObject;
		var ioClass: string) of object;
	//
	TDesignManager = class(TComponent)
	private
		FContainer: TComponent;
		FDesignObjects: TObjectArray;
		FDesignObservers: TLrObserverList;
		FOnChange: TNotifyEvent;
		FOnFilter: TDesignFilterEvent;
		FOnGetAddClass: TDesignGetAddClassEvent;
		FPropertyObservers: TLrObserverList;
		FSelected: TObjectArray;
		FSelectionObservers: TLrObserverList;
		Handle: HWND;
	protected
		function GetAddClass: string;
		function GetSelectedObject: TObject;
		procedure Change;
		procedure LrmPropChange(var inMessage: TMessage); message LRM_PROPCHANGE;
		procedure SetContainer(const Value: TComponent);
		procedure UpdateDesignList;
		procedure WndProc(var inMsg: TMessage);
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		procedure DesignChange;
		function Filter(inObject: TObject): Boolean;
		procedure ObjectSelected(inSender, inObject: TObject);
		procedure ObjectsSelected(inSender: TObject; inSelected: array of TObject);
		procedure PropertyChange(inSender: TObject);
		property AddClass: string read GetAddClass;
		property Container: TComponent read FContainer write SetContainer;
		property DesignObjects: TObjectArray read FDesignObjects;
		property DesignObservers: TLrObserverList read FDesignObservers;
		property OnFilter: TDesignFilterEvent read FOnFilter write FOnFilter;
		property OnGetAddClass: TDesignGetAddClassEvent read FOnGetAddClass
			write FOnGetAddClass;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
		property PropertyObservers: TLrObserverList read FPropertyObservers;
		property SelectionObservers: TLrObserverList read FSelectionObservers;
		property Selected: TObjectArray read FSelected;
		property SelectedObject: TObject read GetSelectedObject;
	end;

var
	DesignMgr: TDesignManager;

implementation

{ TDesignManager }

constructor TDesignManager.Create(inOwner: TComponent);
begin
	inherited;
	Handle := Classes.AllocateHWnd(WndProc);
	FSelectionObservers := TLrObserverList.Create;
	FDesignObservers := TLrObserverList.Create;
	FPropertyObservers := TLrObserverList.Create;
end;

destructor TDesignManager.Destroy;
begin
	FPropertyObservers.Free;
	FDesignObservers.Free;
	FSelectionObservers.Free;
	Classes.DeallocateHWnd(Handle);
	inherited;
end;

procedure TDesignManager.Change;
begin
	if Assigned(OnChange) then
		OnChange(Self);
end;

procedure TDesignManager.DesignChange;
begin
	UpdateDesignList;
	DesignObservers.Notify(Self);
	//Change;
end;

function TDesignManager.Filter(inObject: TObject): Boolean;
begin
	Result := false;
	if Assigned(OnFilter) then
		OnFilter(Self, inObject, Result);
end;

procedure TDesignManager.UpdateDesignList;
var
	c, i: Integer;
begin
	if Container = nil then
		FDesignObjects := nil
	else begin
		c := 0;
		SetLength(FDesignObjects, Container.ComponentCount);
		for i := 0 to Pred(Container.ComponentCount) do
			if not Filter(Container.Components[i]) then
			begin
				FDesignObjects[c] := Container.Components[i];
				Inc(c);
			end;
		SetLength(FDesignObjects, c);
	end;
end;

procedure TDesignManager.SetContainer(const Value: TComponent);
begin
	FContainer := Value;
	UpdateDesignList;
	DesignObservers.Notify(Self);
end;

procedure TDesignManager.ObjectSelected(inSender: TObject; inObject: TObject);
begin
	ObjectsSelected(inSender, [ inObject ]);
//	FSelectedObject := inObject;
//	SelectionObservers.NotifyExcept(Self, inSender);
end;

procedure TDesignManager.ObjectsSelected(inSender: TObject;
	inSelected: array of TObject);
var
	i: Integer;
begin
	SetLength(FSelected, Length(inSelected));
	for i := 0 to Pred(Length(inSelected)) do
		FSelected[i] := inSelected[i];
	SelectionObservers.NotifyExcept(Self, inSender);
end;

function TDesignManager.GetAddClass: string;
begin
	if Assigned(OnGetAddClass) then
		OnGetAddClass(Self, Result)
	else
		Result := '';
end;

procedure TDesignManager.PropertyChange(inSender: TObject);
begin
	PostMessage(Handle, LRM_PROPCHANGE, Integer(inSender), 0);
end;

procedure TDesignManager.WndProc(var inMsg: TMessage);
begin
	case inMsg.Msg of
		LRM_PROPCHANGE: LrmPropChange(inMsg);
		else
			inMsg.Result :=
				DefWindowProc(Handle, inMsg.Msg, inMsg.wParam, inMsg.lParam);
	end;
end;

procedure TDesignManager.LrmPropChange(var inMessage: TMessage);
begin
	PropertyObservers.NotifyExcept(Self, TObject(inMessage.wParam));
end;

function TDesignManager.GetSelectedObject: TObject;
begin
	if Length(Selected) > 0 then
		Result := Selected[0]
	else
		Result := nil;
end;

end.
