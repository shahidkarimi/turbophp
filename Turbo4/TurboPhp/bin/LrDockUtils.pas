unit LrDockUtils;

interface

uses
	SysUtils, Types, Classes, Controls,
	dxDockControl, dxDockPanel;

type
	TTabState = class(TComponent)
	public
		ActiveChild: TdxCustomDockControl;
		constructor Create(inSite: TdxTabContainerDockSite); reintroduce;
		destructor Destroy; override;
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
		procedure Restore;
	end;
	//
	TDockTabsState = class
	private
		procedure FreeTabStates;
	public
		Tabs: TList;
		Container: TWinControl;
		constructor Create(inContainer: TWinControl);
		destructor Destroy; override;
		procedure Capture;
		procedure Restore;
	end;
	//
	TDockLayoutState = class
	private
		FLayout: TMemoryStream;
	public
		constructor Create;
		destructor Destroy; override;
		procedure Capture;
		procedure Restore;
	end;
	//
	TSimpleDockState = class
	public
		Control: TdxCustomDockControl;
		Bounds: TRect;
		Visible: Boolean;
	public
		constructor Create(inControl: TdxCustomDockControl);
		procedure Restore;
	end;
	//
	TSimpleDockLayoutState = class
	private
		procedure FreeStates;
	public
		States: TList;
		Container: TWinControl;
		constructor Create(inContainer: TWinControl);
		destructor Destroy; override;
		procedure Capture;
		procedure Restore;
	end;

procedure ActivateDock(inDock: TdxCustomDockControl);

implementation

procedure ActivateDock(inDock: TdxCustomDockControl);
begin
	with inDock do
		if (Container <> nil) then
			if (Container is TdxTabContainerDockSite) then
				Container.ActiveChild := inDock
			else if (Container is TdxCustomDockControl) then
				ActivateDock(TdxCustomDockControl(Container));
end;

{ TTabState }

constructor TTabState.Create(inSite: TdxTabContainerDockSite);
begin
	inherited Create(nil);
//	Site := inSite;
//	ActiveIndex := Site.ActiveChildIndex;
	ActiveChild := inSite.ActiveChild;
	ActiveChild.FreeNotification(Self);
end;

destructor TTabState.Destroy;
begin
	//ActiveChild.RemoveFreeNotification(Sefl);
	inherited;
end;

procedure TTabState.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	if (Operation = opRemove) and (AComponent = ActiveChild) then
		ActiveChild := nil;
end;

procedure TTabState.Restore;
begin
	if ActiveChild <> nil then
		ActivateDock(ActiveChild);
//	if Site.ChildCount > ActiveIndex then
//		Site.ActiveChildIndex := ActiveIndex;
end;

{ TDockTabsState }

constructor TDockTabsState.Create(inContainer: TWinControl);
begin
	Container := inContainer;
	Tabs := TList.Create;
end;

destructor TDockTabsState.Destroy;
begin
	FreeTabStates;
	Tabs.Free;
	inherited;
end;

procedure TDockTabsState.FreeTabStates;
var
	i: Integer;
begin
	for i := 0 to Pred(Tabs.Count) do
		TTabState(Tabs[i]).Free;
	Tabs.Clear;
end;

procedure TDockTabsState.Capture;
var
	i: Integer;
begin
	FreeTabStates;
	with Container do
		for i := 0 to Pred(ComponentCount) do
			if Components[i] is TdxTabContainerDockSite then
				Tabs.Add(TTabState.Create(TdxTabContainerDockSite(Components[i])));
end;

procedure TDockTabsState.Restore;
var
	i: Integer;
begin
	for i := 0 to Pred(Tabs.Count) do
		TTabState(Tabs[i]).Restore;
end;

{ TDockLayoutState }

constructor TDockLayoutState.Create;
begin
	FLayout := TMemoryStream.Create;
end;

destructor TDockLayoutState.Destroy;
begin
	FLayout.Free;
	inherited;
end;

procedure TDockLayoutState.Capture;
begin
	FLayout.Clear;
	dxDockingController.SaveLayoutToStream(FLayout);
end;

procedure TDockLayoutState.Restore;
begin
	if FLayout.Size > 0 then
	begin
		FLayout.Position := 0;
		dxDockingController.LoadLayoutFromStream(FLayout);
	end;
end;

{ TSimpleDockState }

constructor TSimpleDockState.Create(inControl: TdxCustomDockControl);
begin
	Control := inControl;
	Bounds := inControl.BoundsRect;
	Visible := inControl.Visible;
end;

procedure TSimpleDockState.Restore;
begin
	Control.BoundsRect := Bounds;
	Control.Visible := Visible;
end;

{ TSimpleDockLayoutState }

constructor TSimpleDockLayoutState.Create(inContainer: TWinControl);
begin
	Container := inContainer;
	States := TList.Create;
end;

destructor TSimpleDockLayoutState.Destroy;
begin
	FreeStates;
	States.Free;
	inherited;
end;

procedure TSimpleDockLayoutState.FreeStates;
var
	i: Integer;
begin
	for i := 0 to Pred(States.Count) do
		TSimpleDockState(States[i]).Free;
	States.Clear;
end;

procedure TSimpleDockLayoutState.Capture;
var
	i: Integer;
begin
	FreeStates;
	with Container do
		for i := 0 to Pred(ComponentCount) do
			if Components[i] is TdxDockPanel then
				States.Add(TSimpleDockState.Create(TdxDockPanel(Components[i])));
end;

procedure TSimpleDockLayoutState.Restore;
var
	i: Integer;
begin
	for i := 0 to Pred(States.Count) do
		TSimpleDockState(States[i]).Restore;
end;

end.
