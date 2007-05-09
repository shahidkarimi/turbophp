unit Project;

interface

uses
	SysUtils, Classes, Contnrs,
	LrDocument, LrTreeData, LrProject,
	htDatabases, Servers;

type
	TDocumentItem = class(TLrProjectItem)
	end;
	//
	TProject = class(TLrProject)
	protected
		function GetDatabasesFilename: string;
		function GetPublishFolder(inDocument: TLrDocument): string;
		function GetPublishRoot: string;
		function GetPublishUrl(inDocument: TLrDocument): string;
		function GetUrlRoot: string;
		function GetServersFilename: string;
		procedure InitDocumentNode;
		procedure InstallDocumentNode;
		procedure InstallSpecialNodes;
		procedure SaveItem(inSender: TLrTreeLeaf; inIndex: Integer;
			var inAllow: Boolean); override;
	public
		Databases: ThtDatabasesItem;
		Servers: TServersItem;
		Documents: TLrFolderItem;
		constructor Create; override;
		procedure AddDocumentItem(const inFilename: string);
		procedure DocumentChange(Sender: TObject);
		procedure Open(const inFilename: string); override;
		procedure Save; override;
		procedure PublishDocument(const inFilename: string);
		procedure PublishDocuments(inItem: TLrProjectItem = nil);
		property PublishFolder[inDocument: TLrDocument]: string
			read GetPublishFolder;
		property PublishUrl[inDocument: TLrDocument]: string
			read GetPublishUrl;
	end;
	//
	TPublishableDocument = class(TLrDocument)
		//procedure Publish(const inTarget: string); overload; virtual; abstract;
		function Publish(const inProject: TProject): string; overload; virtual; abstract;
	end;

implementation

uses
	LrUtils, LrVclUtils, Controller, Documents, TurboPhpDocument;

{ TProject }

constructor TProject.Create;
begin
	inherited;
	EnableSaveAs('TurboPhp Projects', '.tprj');
	InstallSpecialNodes;
	InstallDocumentNode;
end;

procedure TProject.InstallSpecialNodes;
begin
	Servers := TServersItem.Create;
	Items.Add(Servers);
	Servers.ImageIndex := 0;
	Servers.DisplayName := 'Servers';
	Servers.ComponentIndex := 0;
	//
	Databases := ThtDatabasesItem.Create;
	Items.Add(Databases);
	Databases.ImageIndex := 2;
	Databases.DisplayName := 'Databases';
	Databases.ComponentIndex := 1;
end;

procedure TProject.InstallDocumentNode;
begin
	Items.Add(TLrFolderItem.Create);
	InitDocumentNode;
end;

procedure TProject.InitDocumentNode;
begin
	Documents := TLrFolderItem(Items[2]);
	Documents.ImageIndex := 4;
	Documents.DisplayName := 'Root';
end;

function TProject.GetDatabasesFilename: string;
begin
	Result := Filename + '.dbs';
end;

function TProject.GetServersFilename: string;
begin
	Result := Filename + '.servers';
end;

procedure TProject.Open(const inFilename: string);
begin
	if FileExists(inFilename) then
	begin
		Filename := inFilename;
		LoadFromFile(Filename);
		InstallSpecialNodes;
		Servers.LoadFromFile(GetServersFilename);
		Databases.LoadFromFile(GetDatabasesFilename);
		InitDocumentNode;
		inherited;
	end;
end;

procedure TProject.SaveItem(inSender: TLrTreeLeaf; inIndex: Integer;
	var inAllow: Boolean);
begin
	with inSender do
		inAllow := (Items[inIndex] <> Servers) and (Items[inIndex] <> Databases);
	//inAllow := (inSender <> Items) or (inIndex > 1);
end;

procedure TProject.Save;
begin
	SaveToFile(Filename);
	Servers.SaveToFile(GetServersFilename);
	Databases.SaveToFile(GetDatabasesFilename);
	inherited;
end;

procedure TProject.AddDocumentItem(const inFilename: string);
var
	item: TLrProjectItem;
begin
	if Documents.FindBySource(inFilename) = nil then
	begin
		item :=	TDocumentItem.Create;
		item.Source := inFilename;
		item.ImageIndex := 5;
		Documents.Add(item);
	end;
end;

procedure TProject.DocumentChange(Sender: TObject);
var
	d: TLrDocument;
	item: TLrProjectItem;
begin
	d := TLrDocument(Sender);
	item := Documents.FindBySource(d.OldFilename);
	if (item <> nil) then
	begin
		item.Source := d.Filename;
		d.OldFilename := d.Filename;
	end;
end;

function TProject.GetPublishRoot: string;
begin
	Result := IncludeTrailingBackslash(Servers.DefaultServer.Root);
end;

function TProject.GetUrlRoot: string;
begin
	Result := IncludeTrailingFrontslash(Servers.DefaultServer.Host);
end;

function TProject.GetPublishFolder(inDocument: TLrDocument): string;
var
	item: TLrProjectItem;
begin
	Result := GetPublishRoot;
	item := Documents.FindBySource(inDocument.Filename);
	if (item <> nil) then
		Result := Result + item.FolderPath;
	Result := IncludeTrailingBackslash(Result);  
//		+ ChangeFileExt(ExtractFileName(inDocument.Filename), '.html');
end;

function TProject.GetPublishUrl(inDocument: TLrDocument): string;
var
	item: TLrProjectItem;
begin
	Result := GetUrlRoot;
	item := Documents.FindBySource(inDocument.Filename);
	if (item <> nil) then
		Result := Result + item.FolderPath;
	Result := StringReplace(Result, '\', '/', [ rfReplaceAll ]);
	Result := IncludeTrailingFrontslash(Result);
//		+ ChangeFileExt(ExtractFileName(inDocument.Filename), '.html');
end;

procedure TProject.PublishDocument(const inFilename: string);
begin
	with TTurboPhpDocument.Create do
	try
		Open(inFilename);
		Publish(Self);
	finally
		Free;
	end;
end;

procedure TProject.PublishDocuments(inItem: TLrProjectItem);
var
	i: Integer;
	d: TLrDocument;
begin
	if inItem = nil then
		inItem := Documents;
	for i := 0 to Pred(inItem.Count) do
	begin
		d := Desktop.FindDocument(inItem[i].Source);
		if (d <> nil) and (d.Modified) and (d is TPublishableDocument) then
			TPublishableDocument(d).Publish(Self);
		PublishDocuments(inItem[i]);
	end;
end;

initialization
	RegisterClass(TDocumentItem);
end.
