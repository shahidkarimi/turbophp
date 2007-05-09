unit LrProject;

interface

uses
	SysUtils, Classes,
	LrTreeData, LrDocument;

type
	TLrProjectItem = class(TLrTreeNode)
	private
		FSource: string;
		FImageIndex: Integer;
	protected
		function GetDisplayName: string; override;
		function GetFolderPath: string;
		function GetProjectItem(inIndex: Integer): TLrProjectItem;
		procedure SetImageIndex(const Value: Integer);
		procedure SetSource(const Value: string);
	public
		function FindBySource(const inSource: string): TLrProjectItem;
		function GetParentItem: TLrProjectItem;
		function GenerateUniqueSource(const inSource: string): string;
		property FolderPath: string read GetFolderPath;
		property ParentItem: TLrProjectItem	read GetParentItem;
		property ProjectItems[inIndex: Integer]: TLrProjectItem
			read GetProjectItem; default;
	published
		property ImageIndex: Integer read FImageIndex write SetImageIndex;
		property Source: string read FSource write SetSource;
	end;
	//
	TLrFolderItem = class(TLrProjectItem);
	//
	TLrProject = class(TLrDocument)
	private
		FItems: TLrProjectItem;
	protected
		procedure CreateItems; virtual;
		procedure ItemsChange(inSender: TObject);
		procedure SaveItem(inSender: TLrTreeLeaf; inIndex: Integer;
			var inAllow: Boolean); virtual;
		procedure SetItems(const Value: TLrProjectItem);
	public
		constructor Create; override;
		destructor Destroy; override;
		procedure LoadFromFile(const inFilename: string); virtual;
		procedure SaveToFile(const inFilename: string); virtual;
	published
		property Items: TLrProjectItem read FItems write SetItems;
	end;

implementation

uses
	LrUtils;

{ TLrProjecItem }

function TLrProjectItem.GetProjectItem(inIndex: Integer): TLrProjectItem;
begin
	Result := TLrProjectItem(Items[inIndex]);
end;

function TLrProjectItem.GetDisplayName: string;
begin
	Result := inherited GetDisplayName;
	if Result = '' then
		Result := ExtractFileName(Source);
end;

procedure TLrProjectItem.SetImageIndex(const Value: Integer);
begin
	FImageIndex := Value;
	Change;
end;

procedure TLrProjectItem.SetSource(const Value: string);
begin
	if Source <> Value then
	begin
		FSource := Value;
		Change;
	end;
end;

function TLrProjectItem.FindBySource(const inSource: string): TLrProjectItem;
var
	i: Integer;
begin
	Result := nil;
	if inSource <> '' then
		if Source = inSource then
			Result := Self
		else
			for i := 0 to Pred(Count) do
				if Items[i] is TLrProjectItem then
				begin
					Result := TLrProjectItem(Items[i]).FindBySource(inSource);
					if Result <> nil then
						break;
				end;
end;

function TLrProjectItem.GenerateUniqueSource(const inSource: string): string;
var
	i: Integer;
begin
	if FindBySource(inSource) = nil then
		Result := inSource
	else begin
		i := 0;
		repeat
			Inc(i);
			Result := inSource + IntToStr(i);
		until FindBySource(Result) = nil;
	end;
end;

function TLrProjectItem.GetFolderPath: string;
var
	item: TLrTreeLeaf;
begin
	Result := '';
	item := Self;
	while (item <> nil) do
	begin
		if item is TLrFolderItem then
			Result := IncludeTrailingBackslash(TLrFolderItem(item).Source) + Result;
		item := item.Parent;
	end;
end;

{ TLrProject }

constructor TLrProject.Create;
begin
	inherited;
	CreateItems;
	Items.OnChange := ItemsChange;
	Items.OnSaveItem := SaveItem;
end;

destructor TLrProject.Destroy;
begin
	Items.Free;
	inherited;
end;

function TLrProjectItem.GetParentItem: TLrProjectItem;
begin
	Result := TLrProjectItem(Parent);
end;

procedure TLrProject.CreateItems;
begin
	FItems := TLrProjectItem.Create;
end;

procedure TLrProject.SetItems(const Value: TLrProjectItem);
begin
	FItems.Assign(Value);
end;

procedure TLrProject.ItemsChange(inSender: TObject);
begin
	Modify;
end;

procedure TLrProject.LoadFromFile(const inFilename: string);
begin
	Items.LoadFromFile(inFilename);
end;

procedure TLrProject.SaveToFile(const inFilename: string);
begin
	Items.SaveToFile(inFilename);
end;

procedure TLrProject.SaveItem(inSender: TLrTreeLeaf; inIndex: Integer;
	var inAllow: Boolean);
begin
	inAllow := true;
end;

initialization
	RegisterClass(TLrProjectItem);
	RegisterClass(TLrFolderItem);
end.
