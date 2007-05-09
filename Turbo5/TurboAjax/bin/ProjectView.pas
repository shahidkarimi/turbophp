unit ProjectView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, Menus, ImgList,
	PngImageList,
	LrProject, Project;

type
	TProjectForm = class(TForm)
		ProjectImages: TPngImageList;
		ProjectMenu: TPopupMenu;
		AddExistingPage1: TMenuItem;
		OpenDialog: TOpenDialog;
		Generate1: TMenuItem;
		SetupServers1: TMenuItem;
		SetupDatabases1: TMenuItem;
    AddFolder1: TMenuItem;
		procedure FormCreate(Sender: TObject);
		procedure AddExistingPage1Click(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure Generate1Click(Sender: TObject);
		procedure ProjectMenuPopup(Sender: TObject);
    procedure AddFolder1Click(Sender: TObject);
	private
		{ Private declarations }
		function GetProject: TProject;
		procedure GetImageIndex(inItem: TLrProjectItem;
			var ImageIndex: Integer);
		procedure GetOverlayIndex(inItem: TLrProjectItem;
			var ImageIndex: Integer);
		procedure ProjectCanDrag(inItem: TLrProjectItem;
			var inAllowed: Boolean);
		procedure ProjectCanDrop(inDragItem, inDropItem: TLrProjectItem;
			inShift: TShiftState; var inAccept: Boolean);
		procedure ProjectCanEdit(inItem: TLrProjectItem;
			var inAllowed: Boolean);
		procedure ProjectDragDrop(inDragItem, inDropItem: TLrProjectItem;
			inShift: TShiftState);
		procedure ProjectNewText(inItem: TLrProjectItem;
			const inNewText: WideString);
		procedure SetProject(const Value: TProject);
	public
		{ Public declarations }
		procedure OpenItem(inItem: TLrProjectItem);
		property Project: TProject read GetProject write SetProject;
	end;

var
	ProjectForm: TProjectForm;

implementation

uses
	LrUtils, LrDocument, LrProjectView,
	htDatabases, 
	Globals, Config, Controller, Servers, Documents, TurboPhpDocument;

const
	cColWidthValue = 'PrjColumn0Width';

{$R *.dfm}

procedure TProjectForm.FormCreate(Sender: TObject);
begin
	AddForm(LrProjectForm, TLrProjectForm, Self);
	LrProjectForm.Tree.Images := ProjectImages;
	LrProjectForm.Tree.PopupMenu := ProjectMenu;
	LrProjectForm.OnOpenItem := OpenItem;
	LrProjectForm.OnGetImageIndex := GetImageIndex;
	LrProjectForm.OnGetOverlayIndex := GetOverlayIndex;
	LrProjectForm.OnCanDrag := ProjectCanDrag;
	LrProjectForm.OnCanDrop := ProjectCanDrop;
	LrProjectForm.OnDragDrop := ProjectDragDrop;
	LrProjectForm.OnCanEdit := ProjectCanEdit;
	LrProjectForm.OnNewText := ProjectNewText;
	OpenDialog.Filter := MakeFilter(cTurboPhpDocuments,
		'*' + TTurboPhpDocument.DocumentExt + ';' +
		'*' + cPhpExt + ';' +
		GraphicFileMask(TGraphic));
	OpenDialog.InitialDir := ProjectsHome;
	ProjectImages.Overlay(7, 0);
	ProjectImages.Overlay(11, 1);
end;

procedure TProjectForm.FormDestroy(Sender: TObject);
begin
	Configuration.Integers[cColWidthValue] :=
		LrProjectForm.Tree.Header.Columns[0].Width;
end;

function TProjectForm.GetProject: TProject;
begin
	Result := TProject(LrProjectForm.Project);
end;

procedure TProjectForm.SetProject(const Value: TProject);
begin
	LrProjectForm.Project := Value;
	with LrProjectForm.Tree.Header.Columns[0] do
		Width := Configuration.GetIntegerDef(cColWidthValue, Width);
end;

procedure TProjectForm.GetImageIndex(inItem: TLrProjectItem;
	var ImageIndex: Integer);
begin
	if inItem is TServerItem then
//		if Project.Servers.DefaultServer = inItem then
//			ImageIndex := 7
//		else
			ImageIndex := 1
	else if inItem is ThtDatabaseItem then
		ImageIndex := 3;
end;

procedure TProjectForm.GetOverlayIndex(inItem: TLrProjectItem;
	var ImageIndex: Integer);
//var
//	d: TLrDocument;
begin
	if inItem is TServerItem then
	begin
		if Project.Servers.DefaultServer = inItem then
			ImageIndex := 1
{
	end else if inItem is TDocumentItem then
	begin
		d := Desktop.FindDocument(inItem.Source);
		if (d <> nil) and (d.Modified) then
			ImageIndex := 0;
}
	end;
end;

procedure TProjectForm.ProjectCanDrag(inItem: TLrProjectItem;
	var inAllowed: Boolean);
begin
	inAllowed := LrProjectForm.FolderItem[inItem] is TLrFolderItem;
end;

procedure TProjectForm.ProjectCanDrop(inDragItem, inDropItem: TLrProjectItem;
	inShift: TShiftState; var inAccept: Boolean);
begin
	inAccept := (inDropItem = nil) or
		(LrProjectForm.FolderItem[inDropItem] is TLrFolderItem);
end;

procedure TProjectForm.ProjectDragDrop(inDragItem, inDropItem: TLrProjectItem;
	inShift: TShiftState);
var
	folder: TLrProjectItem;
begin
	Project.Items.BeginUpdate;
	try
		folder := LrProjectForm.FolderItem[inDragItem.Parent];
		folder.Remove(inDragItem);
		folder := LrProjectForm.FolderItem[inDropItem];
		folder.Add(inDragItem);
	finally
		Project.Items.EndUpdate;
	end;
end;

procedure TProjectForm.ProjectCanEdit(inItem: TLrProjectItem;
	var inAllowed: Boolean);
begin
	inAllowed := (inItem is TLrFolderItem);
end;

procedure TProjectForm.ProjectNewText(inItem: TLrProjectItem;
	const inNewText: WideString);
begin
	inItem.Source := inNewText;
end;

procedure TProjectForm.ProjectMenuPopup(Sender: TObject);
begin
	with LrProjectForm do
	begin
		if SelectedItem <> nil then
		begin
			AddExistingPage1.Visible := (SelectedItem is TDocumentItem)
				or (SelectedItem = Self.Project.Documents);
			Generate1.Visible := SelectedItem is TDocumentItem;
			SetupServers1.Visible := (SelectedItem is TServersItem)
				or (SelectedItem is TServerItem);
			SetupDatabases1.Visible := (SelectedItem is ThtDatabasesItem)
				or (SelectedItem is ThtDatabaseItem);
		end;
		AddFolder1.Visible := SelectedFolderItem is TLrFolderItem;
	end;
end;

procedure TProjectForm.AddExistingPage1Click(Sender: TObject);
begin
	with OpenDialog do
		if Execute then
			Project.AddDocumentItem(ControllerModule.Open(Filename).Filename);
end;

procedure TProjectForm.OpenItem(inItem: TLrProjectItem);
begin
	if inItem is TDocumentItem then
		ControllerModule.Open(inItem.Source)
	else if inItem is TServerItem then
	begin
		Project.Servers.DefaultServer := TServerItem(inItem);
		Project.Modify;
	end;
end;

procedure TProjectForm.Generate1Click(Sender: TObject);
begin
	if LrProjectForm.SelectedItem is TDocumentItem then
		Project.PublishDocument(LrProjectForm.SelectedItem.Source);
end;

procedure TProjectForm.AddFolder1Click(Sender: TObject);
var
	folder: TLrFolderItem;
begin
	folder := TLrFolderItem.Create;
	folder.Source := 'New Folder';
	LrProjectForm.SelectedFolderItem.Add(folder);
end;

end.
