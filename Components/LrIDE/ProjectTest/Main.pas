unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Menus, Dialogs, ExtDlgs,
	LrProject, ImgList, PngImageList;

type
	TMainForm = class(TForm)
		TreePopup: TPopupMenu;
		NewFolder1: TMenuItem;
		AddItem1: TMenuItem;
		OpenPictureDialog: TOpenPictureDialog;
		TreeImages: TPngImageList;
    N1: TMenuItem;
    AddServer1: TMenuItem;
    AddDatabase1: TMenuItem;
		procedure FormCreate(Sender: TObject);
		procedure NewFolder1Click(Sender: TObject);
		procedure AddItem1Click(Sender: TObject);
    procedure AddServer1Click(Sender: TObject);
    procedure AddDatabase1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
	private
		{ Private declarations }
		function GetFolderItem(inItem: TLrProjectItem): TLrProjectItem;
		function GetSelectedFolderItem: TLrProjectItem;
		procedure ProjectCanDrag(inItem: TLrProjectItem; var inAllowed: Boolean);
		procedure ProjectCanDrop(inDragItem, inDropItem: TLrProjectItem;
			inShift: TShiftState; var inAccept: Boolean);
		procedure ProjectCanEdit(inItem: TLrProjectItem; var inAllowed: Boolean);
		procedure ProjectDragDrop(inDragItem, inDropItem: TLrProjectItem;
			inShift: TShiftState);
		procedure ProjectNewText(inItem: TLrProjectItem;
			const inNewText: WideString);
	public
		{ Public declarations }
		procedure BuildSampleProject;
	end;

var
	MainForm: TMainForm;

implementation

uses
	JPEG,
	LrVclUtils, LrTreeData, LrProjectView,
	Servers;

{$R *.dfm}

type
	TVirtualItem = class(TLrProjectItem)
	end;
	//
	TCustomItem = class(TLrProjectItem)
	private
		FItem: TVirtualItem;
	protected
		function GetCount: Integer; override;
		function GetItems(inIndex: Integer): TLrTreeLeaf; override;
	public
		constructor Create; override;
		destructor Destroy; override;
	end;
	//
	TCustomProject = class(TLrProject)
	protected
		procedure CreateNodes;
		procedure SaveItem(inSender: TLrTreeLeaf; inIndex: Integer;
			var inAllow: Boolean); override;
	public
		Dbs: TLrProjectItem;
		Servers: TLrProjectItem;
		constructor Create; override;
		procedure LoadFromFile(const inFilename: string); override;
		procedure SaveToFile(const inFilename: string); override;
	end;

{ TCustomItem }

constructor TCustomItem.Create;
begin
	inherited;
	FItem := TVirtualItem.Create;
end;

destructor TCustomItem.Destroy;
begin
	FItem.Free;
	inherited;
end;

function TCustomItem.GetCount: Integer;
begin
	Result := 3;
end;

function TCustomItem.GetItems(inIndex: Integer): TLrTreeLeaf;
begin
	FItem.ImageIndex := ImageIndex;
	FItem.DisplayName := 'Custom Item #' + IntToStr(inIndex + 1);
	Result := FItem;
end;

{ TCustomProject }

constructor TCustomProject.Create;
begin
	inherited;
	Items.OnSaveItem := SaveItem;
	CreateNodes;
end;

procedure TCustomProject.CreateNodes;
begin
	Servers := TLrProjectItem.Create;
	Items.Add(Servers);
	Servers.ImageIndex := 4;
	Servers.DisplayName := 'Servers';
	Servers.ComponentIndex := 0;
	//
	Dbs := TLrProjectItem.Create;
	Items.Add(Dbs);
	Dbs.ImageIndex := 6;
	Dbs.DisplayName := 'Databases';
	Dbs.ComponentIndex := 1;
end;

procedure TCustomProject.LoadFromFile(const inFilename: string);
begin
//	Servers.Free;
//	Dbs.Free;
	inherited;
//	Servers := TLrProjectItem(Items[0]);
//	Dbs := TLrProjectItem(Items[1]);
	CreateNodes;
	Servers.LoadFromFile(inFilename + 'servers');
	Dbs.LoadFromFile(inFilename + 'dbs');
end;

procedure TCustomProject.SaveItem(inSender: TLrTreeLeaf; inIndex: Integer;
	var inAllow: Boolean);
begin
	inAllow := (inSender <> Items) or (inIndex > 1);
end;

procedure TCustomProject.SaveToFile(const inFilename: string);
begin
	inherited;
	Servers.SaveToFile(inFilename + 'servers');
	Dbs.SaveToFile(inFilename + 'dbs');
end;

var
	Project: TCustomProject;

	function GetProjectFilename: string;
	begin
		Result := ExtractFilePath(Application.ExeName) + 'Sample.project.txt';
	end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
	Project := TCustomProject.Create;
	//
	if not FileExists(GetProjectFilename) then
		BuildSampleProject
	else
		Project.LoadFromFile(GetProjectFilename);
	//
	LrAddForm(LrProjectForm, TLrProjectForm, Self);
	LrProjectForm.Project := Project;
	LrProjectForm.Tree.Images := TreeImages;
	LrProjectForm.Tree.PopupMenu := TreePopup;
	LrProjectForm.OnCanDrag := ProjectCanDrag;
	LrProjectForm.OnCanDrop := ProjectCanDrop;
	LrProjectForm.OnDragDrop := ProjectDragDrop;
	LrProjectForm.OnCanEdit := ProjectCanEdit;
	LrProjectForm.OnNewText := ProjectNewText;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Project.SaveToFile(GetProjectFilename);
end;

procedure TMainForm.BuildSampleProject;
var
	folder: TLrFolderItem;
	item: TLrProjectItem;
begin
	Project := TCustomProject.Create;
	//
	folder := TLrFolderItem.Create;
	folder.Source := 'C:\Program Files\RemObjects Software\Chrome\Samples\ClassRefs';
	Project.Items.Add(folder);
	//
	item := TLrProjectItem.Create;
	item.Source := 'C:\Program Files\RemObjects Software\Chrome\Samples\ClassRefs\ClassRefs.chrome';
	item.ImageIndex := 1;
	folder.Add(item);
	//
	item := TLrProjectItem.Create;
	item.Source := 'C:\Program Files\RemObjects Software\Chrome\Samples\ClassRefs\AssemblyInfo.pas';
	item.ImageIndex := 1;
	folder.Add(item);
	//
	item := TCustomItem.Create;
	item.Source := 'Custom Node';
	item.ImageIndex := 3;
	Project.Items.Add(item);
	//
	folder := TLrFolderItem.Create;
	folder.Source := 'D:\Pictures\Named\Andrea';
	Project.Items.Add(folder);
	//
	item := TLrProjectItem.Create;
	item.Source := 'D:\Pictures\Named\Andrea\028.jpg';
	item.ImageIndex := 2;
	folder.Add(item);
	//
	item := TLrProjectItem.Create;
	item.Source := 'D:\Pictures\Named\Andrea\075.jpg';
	item.ImageIndex := 2;
	folder.Add(item);
end;

function TMainForm.GetFolderItem(inItem: TLrProjectItem): TLrProjectItem;
begin
	Result := inItem;
	while (Result <> nil) and not (Result is TLrFolderItem) do
		Result := TLrProjectItem(Result.Parent);
	if Result = nil then
		Result := Project.Items;
end;

function TMainForm.GetSelectedFolderItem: TLrProjectItem;
begin
	Result := GetFolderItem(LrProjectForm.SelectedItem);
end;

procedure TMainForm.ProjectCanDrag(inItem: TLrProjectItem;
	var inAllowed: Boolean);
begin
	inAllowed := not (inItem is TVirtualItem);
end;

procedure TMainForm.ProjectCanDrop(inDragItem, inDropItem: TLrProjectItem;
	inShift: TShiftState; var inAccept: Boolean);
begin
	inAccept := (inDropItem = nil) or not (inDropItem is TVirtualItem);
end;

procedure TMainForm.ProjectDragDrop(inDragItem, inDropItem: TLrProjectItem;
	inShift: TShiftState);
var
	folder: TLrProjectItem;
begin
	Project.Items.BeginUpdate;
	try
		folder := GetFolderItem(TLrProjectItem(inDragItem.Parent));
		folder.Remove(inDragItem);
		folder := GetFolderItem(inDropItem);
		folder.Add(inDragItem);
	finally
		Project.Items.EndUpdate;
	end;
end;

procedure TMainForm.ProjectCanEdit(inItem: TLrProjectItem;
	var inAllowed: Boolean);
begin
	inAllowed := (inItem is TLrFolderItem);
end;

procedure TMainForm.ProjectNewText(inItem: TLrProjectItem;
	const inNewText: WideString);
begin
	inItem.Source := inNewText;
end;

procedure TMainForm.NewFolder1Click(Sender: TObject);
var
	folder: TLrFolderItem;
begin
	folder := TLrFolderItem.Create;
	folder.Source := 'New Folder';
	GetSelectedFolderItem.Add(folder);
end;

procedure TMainForm.AddItem1Click(Sender: TObject);
var
	item: TLrProjectItem;
begin
	if OpenPictureDialog.Execute then
	begin
		item := TLrProjectItem.Create;
		item.Source := OpenPictureDialog.Filename;
		item.ImageIndex := 2;
		GetSelectedFolderItem.Add(item);
	end;
end;

	function GetGuid: string;
	var
		guid: TGUID;
	begin
		CreateGUID(guid);
		Result := GUIDToString(guid);
	end;

procedure TMainForm.AddServer1Click(Sender: TObject);
var
	item: TServerItem;
begin
	item := TServerItem.Create;
	item.ImageIndex := 5;
	item.Host := GetGuid;
	item.Name := Project.Servers.GetUniqueName('server');
	Project.Servers.Add(item);
end;

procedure TMainForm.AddDatabase1Click(Sender: TObject);
var
	item: TServerItem;
begin
	item := TServerItem.Create;
	item.ImageIndex := 7;
	item.Host := GetGuid;
	Project.Dbs.Add(item);
end;

initialization
	RegisterClass(TVirtualItem);
	RegisterClass(TCustomItem);
end.
