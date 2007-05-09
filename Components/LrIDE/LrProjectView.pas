unit LrProjectView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, VirtualTrees, ActiveX,
	ImgList, PngImageList,
	LrTreeData, LrProject;

type
	TLrItemCanEvent = procedure(inItem: TLrProjectItem;
		var inAllowed: Boolean) of object;
	TLrCanDropEvent = procedure(inDragItem, inDropItem: TLrProjectItem;
		inShift: TShiftState; var inAccept: Boolean) of object;
	TLrDragDropEvent = procedure(inDragItem, inDropItem: TLrProjectItem;
		inShift: TShiftState) of object;
	TLrNewTextEvent = procedure(inItem: TLrProjectItem;
		const inNewText: WideString) of object;
	TLrGetImageIndexEvent = procedure(inItem: TLrProjectItem;
		var ImageIndex: Integer) of object;
	TLrOpenItemEvent = procedure(inItem: TLrProjectItem) of object;
	//
	TLrProjectForm = class(TForm)
		Tree: TVirtualStringTree;
		procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
			Column: TColumnIndex; TextType: TVSTTextType;
			var CellText: WideString);
		procedure TreeInitNode(Sender: TBaseVirtualTree;
			ParentNode, Node: PVirtualNode;
			var InitialStates: TVirtualNodeInitStates);
		procedure TreeInitChildren(Sender: TBaseVirtualTree;
			Node: PVirtualNode; var ChildCount: Cardinal);
		procedure TreeGetImageIndex(Sender: TBaseVirtualTree;
			Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
			var Ghosted: Boolean; var ImageIndex: Integer);
		procedure TreeDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
			Column: TColumnIndex; var Allowed: Boolean);
		procedure TreeDragOver(Sender: TBaseVirtualTree; Source: TObject;
			Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
			var Effect: Integer; var Accept: Boolean);
		procedure TreeDragDrop(Sender: TBaseVirtualTree; Source: TObject;
			DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
			Pt: TPoint; var Effect: Integer; Mode: TDropMode);
		procedure TreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
			Column: TColumnIndex; var Allowed: Boolean);
		procedure TreeNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
			Column: TColumnIndex; NewText: WideString);
		procedure TreeDblClick(Sender: TObject);
	private
		{ Private declarations }
		FOnCanDrag: TLrItemCanEvent;
		FOnCanDrop: TLrCanDropEvent;
		FOnCanEdit: TLrItemCanEvent;
		FOnDragDrop: TLrDragDropEvent;
		FOnGetImageIndex: TLrGetImageIndexEvent;
		FOnGetOverlayIndex: TLrGetImageIndexEvent;
		FOnNewText: TLrNewTextEvent;
		FOnOpenItem: TLrOpenItemEvent;
		FProject: TLrProject;
	protected
		function GetFolderItem(inItem: TLrTreeLeaf): TLrProjectItem;
		function GetNodeItem(inNode: PVirtualNode): TLrProjectItem;
		function GetNodeItems(inNode: PVirtualNode): TLrProjectItem;
		function GetSelectedFolderItem: TLrProjectItem;
		function GetSelectedItem: TLrProjectItem;
		procedure ProjectChange(inSender: TObject);
		procedure SetProject(const Value: TLrProject);
		procedure SetSelectedItem(const Value: TLrProjectItem);
	public
		{ Public declarations }
		DragItem: TLrProjectItem;
		DropItem: TLrProjectItem;
		procedure BuildTree;
		property FolderItem[inItem: TLrTreeLeaf]: TLrProjectItem
			read GetFolderItem;
		property OnCanDrag: TLrItemCanEvent read FOnCanDrag write FOnCanDrag;
		property OnCanDrop: TLrCanDropEvent read FOnCanDrop write FOnCanDrop;
		property OnCanEdit: TLrItemCanEvent read FOnCanEdit write FOnCanEdit;
		property OnDragDrop: TLrDragDropEvent read FOnDragDrop write FOnDragDrop;
		property OnGetImageIndex: TLrGetImageIndexEvent read FOnGetImageIndex
			write FOnGetImageIndex;
		property OnGetOverlayIndex: TLrGetImageIndexEvent read FOnGetOverlayIndex
			write FOnGetOverlayIndex;
		property OnNewText: TLrNewTextEvent read FOnNewText write FOnNewText;
		property OnOpenItem: TLrOpenItemEvent read FOnOpenItem write FOnOpenItem;
		property Project: TLrProject read FProject write SetProject;
		property SelectedItem: TLrProjectItem read GetSelectedItem
			write SetSelectedItem;
		property SelectedFolderItem: TLrProjectItem read GetSelectedFolderItem;
	end;

var
	LrProjectForm: TLrProjectForm;

implementation

{$R *.dfm}

procedure TLrProjectForm.SetProject(const Value: TLrProject);
begin
	FProject := Value;
	//Project.OnChange := ProjectChange;
	BuildTree;
end;

procedure TLrProjectForm.ProjectChange(inSender: TObject);
begin
	BuildTree;
end;

procedure TLrProjectForm.BuildTree;
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
end;

function TLrProjectForm.GetNodeItems(inNode: PVirtualNode): TLrProjectItem;
begin
	if (inNode = nil) or (inNode = Tree.RootNode) then
		Result := Project.Items
	else
		Result := TLrProjectItem(Tree.GetNodeData(inNode)^);
end;

function TLrProjectForm.GetNodeItem(inNode: PVirtualNode): TLrProjectItem;
begin
	Result := TLrProjectItem(GetNodeItems(inNode.Parent).Items[inNode.Index])
end;

procedure TLrProjectForm.TreeInitNode(Sender: TBaseVirtualTree;
	ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
	item: Pointer;
begin
	with GetNodeItems(ParentNode) do
	begin
		item := Items[Node.Index];
		Move(item, Sender.GetNodeData(Node)^, 4);
		if Count > 0 then
			Include(InitialStates, ivsHasChildren);
	end;
end;

procedure TLrProjectForm.TreeInitChildren(Sender: TBaseVirtualTree;
	Node: PVirtualNode; var ChildCount: Cardinal);
begin
	ChildCount := GetNodeItem(Node).Count;
end;

procedure TLrProjectForm.TreeGetText(Sender: TBaseVirtualTree;
	Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
	var CellText: WideString);
begin
	with GetNodeItem(Node) do
		case Column of
			0: CellText := DisplayName;
			1: CellText := Source;
		end;
end;

procedure TLrProjectForm.TreeGetImageIndex(Sender: TBaseVirtualTree;
	Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
	var Ghosted: Boolean; var ImageIndex: Integer);
begin
	case Column of
		0:
			case Kind of
				ikNormal, ikSelected:
				begin
					ImageIndex := GetNodeItem(Node).ImageIndex;
					if Assigned(OnGetImageIndex) then
						OnGetImageIndex(GetNodeItem(Node), ImageIndex);
				end;
				ikOverlay:
				begin
					if Assigned(OnGetOverlayIndex) then
						OnGetOverlayIndex(GetNodeItem(Node), ImageIndex);
				end;
			end;
	end;
end;

function TLrProjectForm.GetSelectedItem: TLrProjectItem;
begin
	if Tree.FocusedNode = nil then
		Result := nil
	else
		Result := GetNodeItem(Tree.FocusedNode);
end;

procedure TLrProjectForm.SetSelectedItem(const Value: TLrProjectItem);
begin
	//
end;

function TLrProjectForm.GetFolderItem(inItem: TLrTreeLeaf): TLrProjectItem;
begin
	if inItem is TLrProjectItem then
		Result := TLrProjectItem(inItem)
	else
		Result := nil;
	while (Result <> nil) and not (Result is TLrFolderItem) do
		Result := TLrProjectItem(Result.Parent);
	if Result = nil then
		Result := Project.Items;
end;

function TLrProjectForm.GetSelectedFolderItem: TLrProjectItem;
begin
	Result := GetFolderItem(SelectedItem);
end;

procedure TLrProjectForm.TreeDragAllowed(Sender: TBaseVirtualTree;
	Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
	if Assigned(OnCanDrag) then
	begin
		DragItem := GetNodeItem(Node);
		DragItem.Tag := Node.Index;
		OnCanDrag(DragItem, Allowed);
	end;
	//Allowed := true;
end;

procedure TLrProjectForm.TreeDragOver(Sender: TBaseVirtualTree;
	Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
	Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
	if Assigned(OnCanDrop) and ((Mode = dmOnNode) or (Mode = dmNowhere)) then
		with Sender do
		begin
			if (DropTargetNode = nil) then
				DropItem := nil
			else begin
				DropItem := GetNodeItem(DropTargetNode);
				DropItem.Tag := DropTargetNode.Index;
			end;
			if not HasAsParent(DropTargetNode, FocusedNode) then
				OnCanDrop(DragItem, DropItem, Shift, Accept);
		end;
//	Accept := (Tree.DropTargetNode <> nil)
			//and	GetNodeItem(Tree.DropTargetNode).IsFolder
//				and (Mode = dmOnNode);
end;

procedure TLrProjectForm.TreeDragDrop(Sender: TBaseVirtualTree;
	Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
	Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
begin
	Effect := DROPEFFECT_NONE;
	if Assigned(OnDragDrop) then
		OnDragDrop(DragItem, DropItem, Shift);
//	if Source = Sender then
//	begin
		//MoveNode(Tree.FocusedNode, Tree.DropTargetNode);
		//BuildTree;
//	end;
end;

procedure TLrProjectForm.TreeEditing(Sender: TBaseVirtualTree;
	Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
	item: TLrProjectItem;
begin
	if Assigned(OnCanEdit) then
	begin
		item := GetNodeItem(Node);
		item.Tag := Node.Index;
		OnCanEdit(item, Allowed);
	end;
end;

procedure TLrProjectForm.TreeNewText(Sender: TBaseVirtualTree;
	Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
begin
	if Assigned(OnNewText) then
		OnNewText(GetNodeItem(Node), NewText);
end;

procedure TLrProjectForm.TreeDblClick(Sender: TObject);
begin
	if Assigned(OnOpenItem) and (SelectedItem <> nil) then
		OnOpenItem(SelectedItem);
end;

end.
