unit ProjectView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ComCtrls, ImgList, ActnList, Menus,
	ProjectDocument, ServerDatabases;

type
	TProjectViewForm = class(TForm)
		Tree: TTreeView;
		Icons: TImageList;
		ProjectPopup: TPopupMenu;
		Actions: TActionList;
		SetupServers1: TMenuItem;
		SetupDatabases1: TMenuItem;
    AddFolderAction: TAction;
    N1: TMenuItem;
    AddFolder1: TMenuItem;
		procedure FormCreate(Sender: TObject);
		procedure TreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure AddFolderActionExecute(Sender: TObject);
	private
    FProject: TProjectDocument;
		{ Private declarations }
		function GetDatabasesNode: TTreeNode;
		function GetItemsNode: TTreeNode;
		function GetServersNode: TTreeNode;
		procedure ProjectChange(Sender: TObject);
		procedure UpdateDatabases;
		procedure UpdateItems;
		procedure UpdateServers;
		procedure UpdateTables(inProfile: TServerDatabaseProfile);
		procedure SetProject(const Value: TProjectDocument);
	protected
		property DatabasesNode: TTreeNode read GetDatabasesNode;
		property ItemsNode: TTreeNode read GetItemsNode;
		property ServersNode: TTreeNode read GetServersNode;
	public
		{ Public declarations }
		procedure UpdateProject;
		property Project: TProjectDocument read FProject write SetProject;
	end;

var
	ProjectViewForm: TProjectViewForm;

implementation

uses
	LrTreeUtils,
	htDatabaseProfile, htDb,
	ServerSetup, DatabasesSetup, Controller;

{$R *.dfm}

procedure TProjectViewForm.FormCreate(Sender: TObject);
begin
	Icons.Overlay(11, 0);
end;

procedure TProjectViewForm.SetProject(const Value: TProjectDocument);
begin
	FProject := Value;
	Project.OnChange := ProjectChange;
	UpdateProject;
end;

function TProjectViewForm.GetServersNode: TTreeNode;
begin
	Result := Tree.Items.GetFirstNode;
end;

function TProjectViewForm.GetDatabasesNode: TTreeNode;
begin
	Result := ServersNode.GetNextSibling;
end;

function TProjectViewForm.GetItemsNode: TTreeNode;
begin
	Result := DatabasesNode.GetNextSibling;
end;

procedure TProjectViewForm.TreeGetSelectedIndex(Sender: TObject;
	Node: TTreeNode);
begin
	Node.SelectedIndex := Node.ImageIndex;
end;

procedure TProjectViewForm.ProjectChange(Sender: TObject);
begin
	UpdateProject;
end;

procedure TProjectViewForm.UpdateProject;
begin
	UpdateServers;
	UpdateDatabases;
	UpdateItems;
end;

procedure TProjectViewForm.UpdateServers;
var
	i: Integer;
begin
	Tree.Items.BeginUpdate;
	try
		ServersNode.DeleteChildren;
		for i := 0 to Pred(Project.Servers.Count) do
			with Project.Servers[i] do
				with Tree.Items.AddChild(ServersNode, Name) do
				begin
					ImageIndex := Ord(Target);
					if Project.Servers.DefaultIdent = Ident then
						OverlayIndex := 0;
				end;
		ServersNode.AlphaSort(true);
		ServersNode.Expand(true);
	finally
		Tree.Items.EndUpdate;
	end;
end;

procedure TProjectViewForm.UpdateTables(inProfile: TServerDatabaseProfile);
var
	n: TTreeNode;
	db: ThtDb;
	s: TStringList;
	i: Integer;
begin
	n := Tree.Items.AddChild(DatabasesNode, inProfile.Name);
	n.ImageIndex := 4;
	if inProfile.Databases.Count > 0 then
	begin
		db := ThtDb.Create(nil);
		try
			db.Profile := inProfile.Databases[0] ;
			db.Connected := true;
			if db.Connected then
			begin
				s := TStringList.Create;
				try
					db.ListTables(s);
					for i := 0 to Pred(s.Count) do
						Tree.Items.AddChild(n, s[i]).ImageIndex := 10;
				finally
					s.Free;
				end;
			end;
		finally
			db.Free;
		end;
	end;
end;

procedure TProjectViewForm.UpdateDatabases;
var
	i: Integer;
begin
	Tree.Items.BeginUpdate;
	try
		DatabasesNode.DeleteChildren;
		for i := 0 to Pred(Project.Databases.Count) do
			UpdateTables(Project.Databases[i]);
		DatabasesNode.AlphaSort(true);
		DatabasesNode.Expand(true);
	finally
		Tree.Items.EndUpdate;
	end;
end;

procedure TProjectViewForm.UpdateItems;
var
	i: Integer;
	n: TTreeNode;
	//c: TLrNodeCache;
begin
	Tree.Items.BeginUpdate;
	try
		//c := LrCreateNodeCache(Tree);
		try
			ItemsNode.DeleteChildren;
			for i := 0 to Pred(Project.Items.Count) do
				with Project.Items[i] do
				begin
					n := Tree.Items.AddChild(ItemsNode, DisplayName);
					n.ImageIndex := 15; //ImageIndex;
				end;
			ItemsNode.AlphaSort(true);
			ItemsNode.Expand(true);
		finally
			//LrConsumeNodeCache(Tree, c);
		end;
	finally
		Tree.Items.EndUpdate;
	end;
end;

procedure TProjectViewForm.AddFolderActionExecute(Sender: TObject);
var
	item: TSubItemsItem;
begin
	with Tree do
		if (Selected <> nil) and  (Selected.HasAsParent(ItemsNode)) then
		begin
			item := TSubitemsItem.Create;
			Project.AddItem(item);
		end;
end;

end.
