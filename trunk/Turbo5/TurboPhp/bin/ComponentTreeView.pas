unit
	ComponentTreeView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ComCtrls, ImgList, TypInfo, Menus;

type
	TComponentTreeFilterEvent = function(inObject: TObject): Boolean of object;
	//
	TComponentTreeForm = class(TForm)
		TreeView: TTreeView;
		ExplorerImages: TImageList;
    PopupMenu1: TPopupMenu;
    Delete1: TMenuItem;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure TreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
		procedure TreeViewClick(Sender: TObject);
		procedure TreeViewKeyUp(Sender: TObject; var Key: Word;
			Shift: TShiftState);
    procedure Delete1Click(Sender: TObject);
	private
		FNodeCache: TList;
		FOnSelect: TNotifyEvent;
		FRoot: TComponent;
		FSelectCache: TPersistent;
		FTopCache: TPersistent;
		Locked: Boolean;
		FOnFilter: TComponentTreeFilterEvent;
	protected
		function GetSelected: TPersistent;
		procedure SetOnSelect(const Value: TNotifyEvent);
		procedure SetRoot(const Value: TComponent);
		procedure SetSelected(const Value: TPersistent);
	protected
		procedure AddCollectionItems(inNode: TTreeNode;
			inComponent: TComponent);
		procedure AddComponent(inNode: TTreeNode; inComponent: TComponent);
		procedure AddItem(inNode: TTreeNode; inComponent: TComponent);
		procedure AddItems(inNode: TTreeNode; inContainer: TComponent);
		procedure AddProp(inNode: TTreeNode; inObject: TObject;
			const inName: string);
		procedure AddPropInfo(inNode: TTreeNode; inComponent: TComponent;
			inInfo: PPropInfo);
		procedure BuildCache;
		procedure DesignChange(Sender: TObject);
		procedure ExpandIfCached(inNode: TTreeNode);
		function Filter(inObject: TObject): Boolean;
		function FindNode(inComponent: TPersistent): TTreeNode;
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
		procedure Select;
		procedure SelectionChange(Sender: TObject);
		procedure SetCollectionItem(inNode: TTreeNode;
			inCollection: TCollection);
		procedure SetItem(inNode: TTreeNode; inComponent: TComponent);
		function ShouldAddComponent(inComponent: TComponent): Boolean;
	public
		procedure Delete(inComponent: TComponent);
		procedure Refresh;
		property Root: TComponent read FRoot write SetRoot;
		property Selected: TPersistent read GetSelected write SetSelected;
		property OnFilter: TComponentTreeFilterEvent read FOnFilter write FOnFilter;
		property OnSelect: TNotifyEvent read FOnSelect write SetOnSelect;
	end;

var
	ComponentTreeForm: TComponentTreeForm;

implementation

uses
	DesignManager;

{$R *.dfm}

{ TComponentTreeForm }

procedure TComponentTreeForm.FormCreate(Sender: TObject);
begin
	FNodeCache := TList.Create;
	DesignMgr.DesignObservers.Add(DesignChange);
	DesignMgr.SelectionObservers.Add(SelectionChange);
end;

procedure TComponentTreeForm.FormDestroy(Sender: TObject);
begin
	FNodeCache.Free;
end;

procedure TComponentTreeForm.DesignChange(Sender: TObject);
begin
	Root := DesignMgr.Container;
end;

procedure TComponentTreeForm.SelectionChange(Sender: TObject);
begin
	Selected := TPersistent(DesignMgr.SelectedObject);
end;

procedure TComponentTreeForm.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	if (Operation = opRemove) and (AComponent = Root) then
		FRoot := nil;
end;

procedure TComponentTreeForm.SetRoot(const Value: TComponent);
begin
	if (Value <> FRoot) then
	begin
		if FRoot <> nil then
			FRoot.RemoveFreeNotification(Self);
		FRoot := Value;
		if FRoot <> nil then
			FRoot.FreeNotification(Self);
	end;
	if not Locked then
		Refresh;
end;

procedure TComponentTreeForm.BuildCache;
var
	i: Integer;
begin
	FSelectCache := Selected;
	if TreeView.TopItem <> nil then
		FTopCache := TreeView.TopItem.Data
	else
		FTopCache := nil;
	FNodeCache.Clear;
	for i := 0 to Pred(TreeView.Items.Count) do
		if TreeView.Items[i].Expanded then
			FNodeCache.Add(TreeView.Items[i].Data);
end;

procedure TComponentTreeForm.Refresh;
var
	n: TTreeNode;
begin
	with TreeView do
	try
		Items.BeginUpdate;
		BuildCache;
		Items.Clear;
		if Root <> nil then
			AddItems(nil, Root);
	finally
		Items.EndUpdate;
	end;
	n := FindNode(FTopCache);
	if n <> nil then
		TreeView.TopItem := n;
	n := FindNode(FSelectCache);
	if n <> nil then
		TreeView.Selected := n;
end;

function TComponentTreeForm.Filter(inObject: TObject): Boolean;
begin
	Result := DesignMgr.Filter(inObject);
	//Result := Assigned(OnFilter) and OnFilter(inObject);
end;

function TComponentTreeForm.ShouldAddComponent(
	inComponent: TComponent): Boolean;
begin
	Result := (inComponent <> nil) and (inComponent.Name <> '')
		and not Filter(inComponent);
end;

procedure TComponentTreeForm.ExpandIfCached(inNode: TTreeNode);
begin
	if FNodeCache.IndexOf(inNode.Data) >= 0 then
		inNode.Expanded := true;
end;

procedure TComponentTreeForm.SetItem(inNode: TTreeNode;
	inComponent: TComponent);
begin
	inNode.Data := inComponent;
	AddCollectionItems(inNode, inComponent);
	//if inComponent is TComponent then
	//	AddItems(inNode, TWinControl(inComponent));
	AddItems(inNode, inComponent);
	ExpandIfCached(inNode);
end;

procedure TComponentTreeForm.AddItem(inNode: TTreeNode;
	inComponent: TComponent);
begin
	if ShouldAddComponent(inComponent) then
		SetItem(TreeView.Items.AddChild(inNode, inComponent.Name), inComponent);
end;

procedure TComponentTreeForm.AddComponent(inNode: TTreeNode;
	inComponent: TComponent);
begin
	if not (inComponent is TControl) then
		AddItem(inNode, inComponent);
end;

procedure TComponentTreeForm.AddItems(inNode: TTreeNode;
	inContainer: TComponent);
var
	i: Integer;
begin
	for i := 0 to Pred(inContainer.ComponentCount) do
		AddComponent(inNode, inContainer.Components[i]);
	if inContainer is TWinControl then
		with TWinControl(inContainer) do
			for i := 0 to Pred(ControlCount) do
				AddItem(inNode, Controls[i]);
end;

procedure TComponentTreeForm.SetCollectionItem(inNode: TTreeNode;
	inCollection: TCollection);
var
	i: Integer;
begin
	inNode.Data := inCollection;
	with inCollection do
		for i := 0 to Pred(Count) do
			TreeView.Items.AddChild(inNode, Items[i].DisplayName).Data := Items[i];
	ExpandIfCached(inNode);
end;

procedure TComponentTreeForm.AddProp(inNode: TTreeNode;
	inObject: TObject; const inName: string);
begin
	if (inObject is TCollection) and not Filter(inObject) then
		SetCollectionItem(TreeView.Items.AddChild(inNode, inName),
			TCollection(inObject));
end;

procedure TComponentTreeForm.AddPropInfo(inNode: TTreeNode;
	inComponent: TComponent; inInfo: PPropInfo);
begin
	if inInfo.PropType^.Kind = tkClass then
		AddProp(inNode, GetObjectProp(inComponent, inInfo), inInfo.Name);
end;

procedure TComponentTreeForm.AddCollectionItems(inNode: TTreeNode;
	inComponent: TComponent);
var
	c, i: Integer;
	propList: PPropList;
begin
	c := GetPropList(inComponent, propList);
	for i := 0 to Pred(c) do
		AddPropInfo(inNode, inComponent, propList[i]);
end;

procedure TComponentTreeForm.TreeViewGetImageIndex(Sender: TObject;
	Node: TTreeNode);
var
	c: TComponent;
begin
	c := TComponent(Node.Data);
	if (c is TWinControl) then
		Node.ImageIndex := 0
	else if (c is TControl) then
		Node.ImageIndex := 1
	else
		Node.ImageIndex := 2;
	Node.SelectedIndex := Node.ImageIndex;
end;

function TComponentTreeForm.GetSelected: TPersistent;
begin
	if TreeView.Selected = nil then
		Result := nil
	else
		Result := TPersistent(TreeView.Selected.Data);
end;

function TComponentTreeForm.FindNode(inComponent: TPersistent): TTreeNode;
var
	i: Integer;
begin
	Result := nil;
	for i := 0 to Pred(TreeView.Items.Count) do
		if TreeView.Items[i].Data = inComponent then
		begin
			Result := TreeView.Items[i];
			break;
		end;
end;

procedure TComponentTreeForm.Delete(inComponent: TComponent);
var
	n: TTreeNode;
begin
	n := FindNode(inComponent);
	if n <> nil then
		n.Free;
end;

procedure TComponentTreeForm.SetSelected(const Value: TPersistent);
var
	n: TTreeNode;
begin
	if not Locked then
	begin
		n := FindNode(Value);
		if n <> nil then
			TreeView.Select(n);
	end;
end;

procedure TComponentTreeForm.SetOnSelect(const Value: TNotifyEvent);
begin
	FOnSelect := Value;
end;

procedure TComponentTreeForm.Select;
begin
	if (Selected <> nil) and not Locked then
		try
			Locked := true;
			DesignMgr.ObjectSelected(Self, Selected);
			if Assigned(OnSelect) then
				OnSelect(Self);
		finally
			Locked := false;
		end;
end;

procedure TComponentTreeForm.TreeViewClick(Sender: TObject);
begin
	Select;
end;

procedure TComponentTreeForm.TreeViewKeyUp(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	Select;
end;

procedure TComponentTreeForm.Delete1Click(Sender: TObject);
begin
	if (Selected <> nil) then
	begin
		Selected.Free;
		DesignMgr.DesignChange;
	end;
end;

end.
