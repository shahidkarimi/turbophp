unit Project;

interface

uses
	SysUtils, Classes, LrProject;

type
	TProjectItems = class;
	//
	TProjectItem = class(TCollectionItem)
	private
		FIsFolder: Boolean;
		FFilename: string;
		FItems: TProjectItems;
	protected
		procedure SetFilename(const Value: string);
		procedure SetIsFolder(const Value: Boolean);
		procedure SetItems(const Value: TProjectItems);
	public
		constructor Create(inOwner: TCollection); override;
		destructor Destroy; override;
		function GetPublishPath: string;
		procedure Subsume(inItem: TProjectItem);
	published
		property Filename: string read FFilename write SetFilename;
		property IsFolder: Boolean read FIsFolder write SetIsFolder;
		property Items: TProjectItems read FItems write SetItems;
	end;
	//
	TProjectItems = class(TCollection)
	private
		FParentItem: TProjectItem;
	protected
		function GetProjectItems(inIndex: Integer): TProjectItem;
	protected
		procedure CompareNSwap(inItemA, inItemB: TProjectItem);
	public
		constructor Create;
		function FindItem(const inFilename: string): TProjectItem;
		procedure Sort;
		property ParentItem: TProjectItem read FParentItem write FParentItem;
		property ProjectItems[inIndex: Integer]: TProjectItem
			read GetProjectItems; default;
	end;
	//
	TProject = class(TLrProject)
	private
		FUrl: string;
		FPublishFolder: string;
		FItems: TProjectItems;
	protected
		function GetLibFolder: string;
		function GetRoot: TProjectItem;
		procedure SetFilename(const Value: string); override;
		procedure SetItems(const Value: TProjectItems);
		procedure SetPublishFolder(const Value: string);
		procedure SetUrl(const Value: string);
	protected
		function GetRelativePath(const inFilename: string): string;
		procedure Changed; override;
		procedure ManuallyLoaded; override;
		procedure UpdateFilenames(inItems: TProjectItems = nil);
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		function GetProjectFolder: string;
		function GetPublishedImagesFolder(const inFilename: string): string;
		function GetPublishFolder(const inFilename: string): string;
		function GetPublishUrl(const inFilename: string): string;
		function GetRelativePublishFolder(const inFilename: string): string;
		function GetSourceImagesFolder(const inFilename: string): string;
		function PathToProjectUrl(const inPath: string): string;
		function ProjectPath(const inFilename: string): string;
		procedure AddPage(inFilename: string; inParent: TProjectItem = nil);
		procedure AddExistingPage(inParent: TProjectItem = nil);
		procedure Clear;
		procedure MoveItem(inSrc, inDst: TProjectItem);
		procedure NewFolder(inParent: TProjectItem);
		procedure Remove(const inFilename: string);
		procedure RemoveItem(inItem: TProjectItem);
		procedure Rename(const inOld, inNew: string); overload;
		procedure Rename(inItem: TProjectItem; const inName: string); overload;
		procedure Sort(inItems: TProjectItems = nil);
	published
		property Items: TProjectItems read FItems write SetItems;
		property LibFolder: string read GetLibFolder;
		property ProjectFolder: string read GetProjectFolder;
		property PublishFolder: string read FPublishFolder write SetPublishFolder;
		property Root: TProjectItem read GetRoot;
		property Url: string read FUrl write SetUrl;
	end;

implementation

uses
	StrUtils, LrUtils, ThPathUtils, Config, Main;

const
	cLibFolder = 'libs\';
	cImagesFolder = 'images\';

{ TProjectItem }

constructor TProjectItem.Create(inOwner: TCollection);
begin
	inherited;
	FItems := TProjectItems.Create;
	FItems.ParentItem := Self;
end;

destructor TProjectItem.Destroy;
begin
	FItems.Free;
	inherited;
end;

procedure TProjectItem.SetFilename(const Value: string);
begin
	FFilename := Value;
end;

procedure TProjectItem.SetIsFolder(const Value: Boolean);
begin
	FIsFolder := Value;
end;

procedure TProjectItem.SetItems(const Value: TProjectItems);
begin
	if Value = nil then
		FItems := nil
	else
		FItems.Assign(Value);
end;

procedure TProjectItem.Subsume(inItem: TProjectItem);
begin
	FItems.Free;
	FItems := inItem.Items;
	Filename := inItem.Filename;
	IsFolder := inItem.IsFolder;
	inItem.Items := nil;
	inItem.Free;
end;

function TProjectItem.GetPublishPath: string;
var
	item: TProjectItem;
begin
	Result := '';
	item := TProjectItems(Collection).ParentItem;
	while (item <> nil) do
	begin
		Result := IncludeTrailingBackslash(item.Filename) + Result;
		item := TProjectItems(item.Collection).ParentItem;
	end;
end;

{ TProjectItems }

constructor TProjectItems.Create;
begin
	inherited Create(TProjectItem);
end;

function TProjectItems.GetProjectItems(inIndex: Integer): TProjectItem;
begin
	Result := TProjectItem(Items[inIndex]);
end;

function TProjectItems.FindItem(const inFilename: string): TProjectItem;
var
	i: Integer;
begin
	Result := nil;
	for i := 0 to Pred(Count) do
	begin
		if ProjectItems[i].Filename = inFilename then
			Result := ProjectItems[i]
		else
			Result := ProjectItems[i].Items.FindItem(inFilename);
		if Result <> nil then
			break;
	end;
end;

procedure TProjectItems.CompareNSwap(inItemA, inItemB: TProjectItem);
var
	i: Integer;
begin
	if (inItemB.IsFolder and not inItemA.IsFolder)
		or ((inItemA.IsFolder = inItemB.IsFolder) 
			and (CompareText(inItemA.Filename, inItemB.Filename) > 0)) then
	begin
		i := inItemA.Index;
		inItemA.Index := inItemB.Index;
		inItemB.Index := i;
	end;
end;

procedure TProjectItems.Sort;
var
	i, j: Integer;
begin
	for i := 0 to Pred(Count) do
		for j := 0 to Pred(Pred(Count)) do
			CompareNSwap(ProjectItems[j], ProjectItems[j + 1]);
end;

{ TProject }

constructor TProject.Create(inOwner: TComponent);
begin
	inherited;
	FItems := TProjectItems.Create;
	with TProjectItem(FItems.Add) do
		IsFolder := true;
end;

destructor TProject.Destroy;
begin
	FItems.Free;
	inherited;
end;

procedure TProject.Clear;
begin
	Root.Items.Clear;
	Url := '';
	PublishFolder := '';
end;

procedure TProject.ManuallyLoaded;
begin
//	if not DirectoryExists(PublishFolder) then
//		PublishFolder := '';
	UpdateFilenames;
	Sort;
end;

function TProject.GetRoot: TProjectItem;
begin
	Result := Items[0];
end;

function TProject.GetProjectFolder: string;
begin
	Result := ExtractFilePath(Filename);
end;

function TProject.GetRelativePath(const inFilename: string): string;
begin
	Result := ExtractRelativePath(ProjectFolder, inFilename);
end;

function TProject.GetPublishFolder(const inFilename: string): string;
var
	item: TProjectItem;
begin
	item := Items.FindItem(GetRelativePath(inFilename));
	if item <> nil then
		Result := item.GetPublishPath
	else
		Result := PublishFolder;
end;

function TProject.GetRelativePublishFolder(const inFilename: string): string;
begin
	Result := ExtractRelativePath(PublishFolder, inFilename);
end;

function TProject.PathToProjectUrl(const inPath: string): string;
begin
	Result := Url + ThPathToUrl(GetRelativePublishFolder(inPath));
	//Result := Url + ThPathToUrl(ExtractRelativePath(PublishFolder, inPath));
end;

function TProject.GetPublishUrl(const inFilename: string): string;
var
	item: TProjectItem;
begin
	item := Items.FindItem(GetRelativePath(inFilename));
	if item = nil then
		Result := Url
	else
		Result := PathToProjectUrl(item.GetPublishPath);
end;

function TProject.ProjectPath(const inFilename: string): string;
begin
	if ExtractFileDrive(inFilename) = '' then
		Result := ProjectFolder + inFilename
	else
		Result := inFilename;
end;

function TProject.GetSourceImagesFolder(const inFilename: string): string;
begin
	Result := ProjectFolder + cImagesFolder;
end;

function TProject.GetPublishedImagesFolder(const inFilename: string): string;
begin
	Result := GetPublishFolder(inFilename) + cImagesFolder;
end;

procedure TProject.Changed;
begin
	UpdateFilenames;
	Sort;
	inherited;
end;

procedure TProject.UpdateFilenames(inItems: TProjectItems);
var
	i: Integer;
begin
	if inItems = nil then
		inItems := Items;
	for i := 0 to Pred(inItems.Count) do
		with inItems[i] do
		begin
			if not IsFolder then
				Filename := GetRelativePath(Filename)
			else
				UpdateFilenames(Items);
		end;
end;

procedure TProject.Sort(inItems: TProjectItems);
var
	i: Integer;
begin
	if inItems = nil then
		inItems := Items;
	inItems.Sort;
	for i := 0 to Pred(inItems.Count) do
		with inItems[i] do
			if IsFolder then
				Sort(Items);
end;

procedure TProject.SetPublishFolder(const Value: string);
begin
	if Value <> FPublishFolder then
	begin
		if Value = '' then
			FPublishFolder := ''
		else
			FPublishFolder := IncludeTrailingBackslash(Value);
		Root.Filename := PublishFolder;
		Changed;
	end;
end;

procedure TProject.SetUrl(const Value: string);
begin
	if Value <> FUrl then
	begin
		if Value = '' then
			FUrl := ''
		else
			FUrl := IncludeTrailingFrontslash(Value);
		Changed;
	end;
end;

function TProject.GetLibFolder: string;
begin
	Result := IncludeTrailingBackslash(PublishFolder + cLibFolder);
end;

procedure TProject.SetItems(const Value: TProjectItems);
begin
	FItems.Assign(Value);
end;

procedure TProject.MoveItem(inSrc, inDst: TProjectItem);
begin
	TProjectItem(inDst.Items.Insert(0)).Subsume(inSrc);
	Changed;
end;

procedure TProject.NewFolder(inParent: TProjectItem);
begin
	with TProjectItem(inParent.Items.Add) do
	begin
		IsFolder := true;
		Filename := inParent.Filename + '\New Folder';
	end;
	Changed;
end;

procedure TProject.AddPage(inFilename: string; inParent: TProjectItem = nil);
begin
	inFilename := GetRelativePath(inFilename);
	if Items.FindItem(inFilename) = nil then
	begin
		if inParent = nil then
			inParent := Root;
		TProjectItem(inParent.Items.Add).Filename := inFilename;
		Changed;
	end;
end;

procedure TProject.AddExistingPage(inParent: TProjectItem = nil);
begin
	with MainForm.OpenDialog do
		if Execute then
			AddPage(Filename, inParent);
end;

procedure TProject.RemoveItem(inItem: TProjectItem);
begin
	inItem.Free;
	Changed;
end;

procedure TProject.Remove(const inFilename: string);
var
	item: TProjectItem;
begin
	item := Items.FindItem(GetRelativePath(inFilename));
	if item <> nil then
	begin
		item.Free;
		Changed;
	end;
end;

procedure TProject.Rename(inItem: TProjectItem; const inName: string);
begin
	inItem.Filename := inName;
	Changed;
end;

procedure TProject.Rename(const inOld, inNew: string);
var
	item: TProjectItem;
begin
	item := Items.FindItem(GetRelativePath(inOld));
	if item <> nil then
		Rename(item, GetRelativePath(inNew));
end;

procedure TProject.SetFilename(const Value: string);
begin
	inherited;
//	Changed;
end;

end.
