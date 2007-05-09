unit ProjectView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, VirtualTrees, Project, ImgList, ActiveX, Menus;

type
	TProjectForm = class(TForm)
		Tree: TVirtualStringTree;
		ImageList1: TImageList;
		PopupMenu: TPopupMenu;
		NewFolderItem: TMenuItem;
		RemoveItem: TMenuItem;
    AddExistingPageItem: TMenuItem;
    NewPageItem: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
		procedure FormCreate(Sender: TObject);
		procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode,
			Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
		procedure TreeInitChildren(Sender: TBaseVirtualTree;
			Node: PVirtualNode; var ChildCount: Cardinal);
		procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
			Column: TColumnIndex; TextType: TVSTTextType;
			var CellText: WideString);
    procedure TreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TreeDragOver(Sender: TBaseVirtualTree; Source: TObject;
			Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
			var Effect: Integer; var Accept: Boolean);
		procedure TreeDragDrop(Sender: TBaseVirtualTree; Source: TObject;
			DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
			Pt: TPoint; var Effect: Integer; Mode: TDropMode);
		procedure TreeDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
			Column: TColumnIndex; var Allowed: Boolean);
    procedure PopupMenuPopup(Sender: TObject);
    procedure NewFolderItemClick(Sender: TObject);
    procedure TreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
			Column: TColumnIndex; var Allowed: Boolean);
		procedure AddExistingPageItemClick(Sender: TObject);
		procedure RemoveItemClick(Sender: TObject);
    procedure TreeNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: WideString);
    procedure TreeDblClick(Sender: TObject);
    procedure NewPageItemClick(Sender: TObject);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
	private
		{ Private declarations }
		FProject: TProject;
		FOnOpen: TNotifyEvent;
	protected
		function GetFocusedFolder: TProjectItem;
		function GetFocusedItem: TProjectItem;
		function GetFolderItem(inNode: PVirtualNode): TProjectItem;
		function GetNodeItem(inNode: PVirtualNode): TProjectItem;
		function GetNodeItems(inNode: PVirtualNode): TProjectItems;
		procedure SetProject(const Value: TProject);
	public
		{ Public declarations }
		procedure BuildTree;
		procedure MoveNode(inNode, inTarget: PVirtualNode);
		property FocusedItem: TProjectItem read GetFocusedItem;
		property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
		property Project: TProject read FProject write SetProject;
	end;

implementation

uses
	Main;

{$R *.dfm}

{ TProjectItems }

procedure TProjectForm.FormCreate(Sender: TObject);
begin
	//
end;

procedure TProjectForm.SetProject(const Value: TProject);
begin
	FProject := Value;
	BuildTree;
end;

procedure TProjectForm.BuildTree;
begin
	Tree.BeginUpdate;
	try
		Tree.Clear;
		if Project = nil then
			Tree.RootNodeCount := 0
		else
			Tree.RootNodeCount := Project.Items.Count;
		Tree.FullExpand;
	finally
		Tree.EndUpdate;
	end;
	//Tree.SortTree(0, sdAscending);
end;

function TProjectForm.GetNodeItems(inNode: PVirtualNode): TProjectItems;
begin
	if (inNode = nil) or (inNode = Tree.RootNode) then
		Result := Project.Items
	else
		Result := TProjectItems(Tree.GetNodeData(inNode)^);
end;

function TProjectForm.GetNodeItem(inNode: PVirtualNode): TProjectItem;
begin
	with GetNodeItems(inNode.Parent) do
		Result := TProjectItem(Items[inNode.Index])
end;

function TProjectForm.GetFocusedItem: TProjectItem;
begin
	Result := GetNodeItem(Tree.FocusedNode);
end;

function TProjectForm.GetFolderItem(inNode: PVirtualNode): TProjectItem;
begin
	Result := GetNodeItem(inNode);
	if not Result.IsFolder then
		Result := GetNodeItem(inNode.Parent);
end;

function TProjectForm.GetFocusedFolder: TProjectItem;
begin
	Result := GetFolderItem(Tree.FocusedNode);
end;

procedure TProjectForm.TreeInitNode(Sender: TBaseVirtualTree; ParentNode,
	Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
	items: TProjectItems;
begin
	items := GetNodeItems(ParentNode);
	with TProjectItem(items.Items[Node.Index]) do
		if IsFolder then
		begin
			Move(Items, Sender.GetNodeData(Node)^, 4);
			Include(InitialStates, ivsHasChildren);
		end;
end;

procedure TProjectForm.TreeInitChildren(Sender: TBaseVirtualTree;
	Node: PVirtualNode; var ChildCount: Cardinal);
begin
	with GetNodeItem(Node) do
		ChildCount := Items.Count;
end;

procedure TProjectForm.TreeGetText(Sender: TBaseVirtualTree;
	Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
	var CellText: WideString);
begin
	if (Node.Parent = Sender.RootNode) then
		case Column of
			0: CellText := ExtractFileName(Project.Filename);
			1: CellText := '';
		end
	else
		with GetNodeItem(Node) do
			case Column of
				0: CellText := ExtractFileName(Filename);
				1: if IsFolder then
						CellText := ''
					 else
						CellText := ExtractFilePath(Project.ProjectPath(Filename));
			end;
end;

procedure TProjectForm.TreeGetImageIndex(Sender: TBaseVirtualTree;
	Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
	var Ghosted: Boolean; var ImageIndex: Integer);
begin
	if Column = 0 then
		case Kind of
			ikNormal, ikSelected:
				with GetNodeItem(Node) do
					if IsFolder then
						ImageIndex := 0
					else
						ImageIndex := 1;
		end;
end;

procedure TProjectForm.TreeCompareNodes(Sender: TBaseVirtualTree; Node1,
	Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
begin
	Result := CompareText(GetNodeItem(Node1).Filename,
		GetNodeItem(Node2).Filename);
end;

procedure TProjectForm.TreeDragOver(Sender: TBaseVirtualTree;
	Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
	Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
	Accept := (Tree.DropTargetNode <> nil)
		and	GetNodeItem(Tree.DropTargetNode).IsFolder
		and (Mode = dmOnNode);
end;

procedure TProjectForm.MoveNode(inNode, inTarget: PVirtualNode);
begin
	Project.MoveItem(GetNodeItem(inNode), GetNodeItem(inTarget));
end;

procedure TProjectForm.TreeDragDrop(Sender: TBaseVirtualTree;
	Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
	Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
begin
	Effect := DROPEFFECT_NONE;
	if Source = Sender then
	begin
		MoveNode(Tree.FocusedNode, Tree.DropTargetNode);
		//BuildTree;
	end;
end;

procedure TProjectForm.TreeDragAllowed(Sender: TBaseVirtualTree;
	Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
	Allowed := Node.Parent <> Tree.RootNode;
end;

procedure TProjectForm.TreeEditing(Sender: TBaseVirtualTree;
	Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
	Allowed := GetNodeItem(Node).IsFolder;
end;

procedure TProjectForm.PopupMenuPopup(Sender: TObject);
begin
	if Tree.FocusedNode = nil then
		Tree.FocusedNode := Tree.GetFirst;
	RemoveItem.Enabled :=	GetFocusedItem <> Project.Items.Items[0];
end;

procedure TProjectForm.NewFolderItemClick(Sender: TObject);
begin
	Project.NewFolder(GetFocusedFolder);
//	BuildTree;
end;

procedure TProjectForm.AddExistingPageItemClick(Sender: TObject);
begin
	Project.AddExistingPage(GetFocusedFolder);
//	BuildTree;
end;

procedure TProjectForm.NewPageItemClick(Sender: TObject);
begin
//	Project.AddPage(GetFocusedFolder);
//	BuildTree;
end;

procedure TProjectForm.RemoveItemClick(Sender: TObject);
begin
	Project.RemoveItem(GetFocusedItem);
//	BuildTree;
end;

procedure TProjectForm.TreeNewText(Sender: TBaseVirtualTree;
	Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
begin
	Project.Rename(GetNodeItem(Node), NewText);
end;

procedure TProjectForm.TreeDblClick(Sender: TObject);
begin
	if (Tree.FocusedNode <> nil) and Assigned(OnOpen) then
		OnOpen(Self);
end;

end.
