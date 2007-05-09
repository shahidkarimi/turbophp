unit DesignManager;

interface

uses
	Windows, Messages, Classes, Controls, Forms,
	LrObserverList, DesignSurface;

const
	LRM_PROPCHANGE = WM_USER + $01;

type
	TObjectArray = array of TObject;
	//
	TDesignFilterEvent = procedure(inSender: TObject; inObject: TObject;
		var inFilter: Boolean) of object;
	//
	TDesignManager = class(TComponent)
	private
		FDesigner: TDesignSurface;
		FDesignObjects: TObjectArray;
		FDesignObservers: TLrObserverList;
		FOnChange: TNotifyEvent;
		FOnFilter: TDesignFilterEvent;
		FOnGetAddClass: TDesignGetAddClassEvent;
		FPropertyObservers: TLrObserverList;
		FSelectedObjects: TObjectArray;
		FSelectionObservers: TLrObserverList;
		Handle: HWND;
	protected
		function GetContainer: TComponent;
		function GetDesignCount: Integer;
		function GetSelectedCount: Integer;
		function GetSelectedObject: TObject;
		procedure Change;
		procedure DesignSelectionChange(inSender: TObject);
		procedure GetAddClass(Sender: TObject; var ioClass: string);
		procedure HookDesigner;
		procedure LrmPropChange(var inMessage: TMessage); message LRM_PROPCHANGE;
		procedure SetDesigner(const Value: TDesignSurface);
		procedure UnhookDesigner;
		procedure UpdateDesignList;
		procedure WndProc(var inMsg: TMessage);
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		function Filter(inObject: TObject): Boolean;
		procedure DesignChange(inSender: TObject);
		procedure ObjectSelected(inSender, inObject: TObject);
		procedure ObjectsSelected(inSender: TObject; inSelected: array of TObject);
		procedure PropertyChange(inSender: TObject);
		property Container: TComponent read GetContainer;
		property Designer: TDesignSurface read FDesigner write SetDesigner;
		property DesignCount: Integer read GetDesignCount;
		property DesignObjects: TObjectArray read FDesignObjects;
		property DesignObservers: TLrObserverList read FDesignObservers;
		property OnFilter: TDesignFilterEvent read FOnFilter write FOnFilter;
		property OnGetAddClass: TDesignGetAddClassEvent read FOnGetAddClass
			write FOnGetAddClass;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
		property PropertyObservers: TLrObserverList read FPropertyObservers;
		property SelectionObservers: TLrObserverList read FSelectionObservers;
		property SelectedCount: Integer read GetSelectedCount;
		property SelectedObject: TObject read GetSelectedObject;
		property SelectedObjects: TObjectArray read FSelectedObjects;
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

procedure TDesignManager.UnhookDesigner;
begin
	with Designer do
	begin
		OnChange := nil;
		OnSelectionChange := nil;
		OnGetAddClass := nil;
	end;
end;

procedure TDesignManager.HookDesigner;
begin
	Designer.OnChange := DesignChange;
	Designer.OnSelectionChange := DesignSelectionChange;
	Designer.OnGetAddClass := GetAddClass;
end;

procedure TDesignManager.SetDesigner(const Value: TDesignSurface);
begin
	if Designer <> nil then
		UnhookDesigner;
	FDesigner := Value;
	if Designer <> nil then
		HookDesigner;
	DesignChange(Self);
end;

procedure TDesignManager.DesignChange(inSender: TObject);
begin
	UpdateDesignList;
	DesignObservers.Notify(Self);
end;

procedure TDesignManager.DesignSelectionChange(inSender: TObject);
begin
	ObjectsSelected(Self, Designer.Selected);
end;

procedure TDesignManager.GetAddClass(Sender: TObject; var ioClass: string);
begin
	if Assigned(OnGetAddClass) then
		OnGetAddClass(Sender, ioClass)
	else
		ioClass := '';
end;

function TDesignManager.GetContainer: TComponent;
begin
	if Designer = nil then
		Result := nil
	else
		Result := Designer.Container;
end;

procedure TDesignManager.Change;
begin
	if Assigned(OnChange) then
		OnChange(Self);
end;

function TDesignManager.GetDesignCount: Integer;
begin
	Result := Length(DesignObjects);
end;

function TDesignManager.GetSelectedCount: Integer;
begin
	Result := Length(SelectedObjects);
end;

function TDesignManager.GetSelectedObject: TObject;
begin
	if SelectedCount > 0 then
		Result := SelectedObjects[0]
	else
		Result := nil;
end;

function TDesignManager.Filter(inObject: TObject): Boolean;
begin
	Result := (inObject <> nil) and (inObject is TDesignSurface);
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
				DesignObjects[c] := Container.Components[i];
				Inc(c);
			end;
		SetLength(FDesignObjects, c);
	end;
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
	SetLength(FSelectedObjects, Length(inSelected));
	for i := 0 to Pred(SelectedCount) do
		SelectedObjects[i] := inSelected[i];
	SelectionObservers.NotifyExcept(Self, inSender);
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
	Designer.UpdateDesigner;
	PropertyObservers.NotifyExcept(Self, TObject(inMessage.wParam));
end;

initialization
	DesignMgr := TDesignManager.Create(Application);
finalization
	//Designer.Free;
end.
