unit LrTreeData;

interface

uses
	SysUtils, Classes, Contnrs;

type
	TLrTreeLeaf = class;
	TLrSaveItemEvent = procedure(inSender: TLrTreeLeaf; inIndex: Integer;
		var inAllow: Boolean) of object;
	//
	TLrTreeLeaf = class(TComponent)
	private
		FDisplayName: string;
		FOnChange: TNotifyEvent;
		FOnSaveItem: TLrSaveItemEvent;
		FParent: TLrTreeLeaf;
		FUpdating: Integer;
		FUpdateChange: Boolean;
	protected
		function GetCount: Integer; virtual;
		function GetDisplayName: string; virtual;
		function GetItems(inIndex: Integer): TLrTreeLeaf; virtual;
		procedure Change; virtual;
		procedure SetDisplayName(const Value: string); virtual;
		procedure SetOnChange(const Value: TNotifyEvent);
		procedure SetParent(const Value: TLrTreeLeaf);
	public
		constructor Create(inOwner: TComponent); overload; override;
		constructor Create; reintroduce; overload; virtual;
		procedure BeginUpdate;
		procedure EndUpdate;
		procedure LoadFromFile(const inFilename: string); virtual;
		procedure LoadFromStream(inStream: TStream); virtual;
		procedure SaveToFile(const inFilename: string); virtual;
		procedure SaveToStream(inStream: TStream); virtual;
		property Count: Integer read GetCount;
		property Items[inIndex: Integer]: TLrTreeLeaf read GetItems; default;
		property OnChange: TNotifyEvent read FOnChange write SetOnChange;
		property OnSaveItem: TLrSaveItemEvent read FOnSaveItem write FOnSaveItem;
		property Parent: TLrTreeLeaf read FParent write SetParent;
	published
		property DisplayName: string read GetDisplayName write SetDisplayName;
	end;
	//
	TLrTreeNode = class(TLrTreeLeaf)
	protected
		function GetChildOwner: TComponent; override;
		function GetCount: Integer; override;
		function GetItems(inIndex: Integer): TLrTreeLeaf; override;
		function ShouldWriteItem(inIndex: Integer): Boolean; virtual;
		procedure Clear; virtual;
		procedure DoOnSaveItem(inSender: TLrTreeLeaf; inIndex: Integer;
			var inAllow: Boolean);
		procedure ItemChange(inSender: TObject); virtual;
		procedure ItemSaveItem(inSender: TLrTreeLeaf; inIndex: Integer;
			var inAllow: Boolean);
		procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
	public
		function Find(const inName: string): TLrTreeLeaf;
		function GetUniqueName(const inName: string): string;
		procedure Add(inItem: TLrTreeLeaf); virtual;
		procedure LoadFromStream(inStream: TStream); override;
		procedure Remove(inItem: TLrTreeLeaf); virtual;
		procedure SaveToStream(inStream: TStream); override;
	end;
	//
	TLrTree = class(TLrTreeNode)
	end;

implementation

uses
	LrVclUtils;

{ TLrTreeLeaf }

procedure TLrTreeLeaf.BeginUpdate;
begin
	Inc(FUpdating);
end;

procedure TLrTreeLeaf.Change;
begin
	FUpdateChange := (FUpdating > 0);
	if not FUpdateChange and Assigned(OnChange) then
		OnChange(Self);
end;

constructor TLrTreeLeaf.Create;
begin
	inherited Create(nil);
end;

constructor TLrTreeLeaf.Create(inOwner: TComponent);
begin
	Create;
	// In general, inOnwer <> nil only when streaming
	if (inOwner <> nil) and (inOwner is TLrTreeNode) then
		TLrTreeNode(inOwner).Add(Self);
end;

procedure TLrTreeLeaf.EndUpdate;
begin
	Dec(FUpdating);
	if FUpdateChange and (FUpdating = 0) then
		Change;
end;

function TLrTreeLeaf.GetCount: Integer;
begin
	Result := 0;
end;

function TLrTreeLeaf.GetDisplayName: string;
begin
	Result := FDisplayName
end;

function TLrTreeLeaf.GetItems(inIndex: Integer): TLrTreeLeaf;
begin
	Result := nil;
end;

procedure TLrTreeLeaf.LoadFromFile(const inFilename: string);
var
	s: TFileStream;
begin
	if FileExists(inFilename) then
	begin
		s := TFileStream.Create(inFilename, fmOpenRead);
		try
			LoadFromStream(s);
		finally
			s.Free;
		end;
	end;
end;

procedure TLrTreeLeaf.LoadFromStream(inStream: TStream);
begin
	//
end;

procedure TLrTreeLeaf.SaveToFile(const inFilename: string);
var
	s: TFileStream;
begin
	s := TFileStream.Create(inFilename, fmCreate);
	try
		SaveToStream(s);
	finally
		s.Free;
	end;
end;

procedure TLrTreeLeaf.SaveToStream(inStream: TStream);
begin
	//
end;

procedure TLrTreeLeaf.SetDisplayName(const Value: string);
begin
	FDisplayName := Value;
end;

procedure TLrTreeLeaf.SetOnChange(const Value: TNotifyEvent);
begin
	FOnChange := Value;
end;

procedure TLrTreeLeaf.SetParent(const Value: TLrTreeLeaf);
begin
	FParent := Value;
end;

{ TLrTreeNode }

procedure TLrTreeNode.Clear;
begin
	DestroyComponents;
end;

procedure TLrTreeNode.DoOnSaveItem(inSender: TLrTreeLeaf; inIndex: Integer;
	var inAllow: Boolean);
begin
	if Assigned(OnSaveItem) then
		OnSaveItem(inSender, inIndex, inAllow);
end;

function TLrTreeNode.ShouldWriteItem(inIndex: Integer): Boolean;
begin
	Result := true;
	DoOnSaveItem(Self, inIndex, Result);
end;

procedure TLrTreeNode.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		if ShouldWriteItem(i) then
			Proc(Items[i]);
end;

function TLrTreeNode.GetChildOwner: TComponent;
begin
	Result := Self;
end;

procedure TLrTreeNode.SaveToStream(inStream: TStream);
begin
	LrSaveComponentToStream(Self, inStream);
end;

procedure TLrTreeNode.LoadFromStream(inStream: TStream);
begin
	Clear;
	LrLoadComponentFromStream(Self, inStream);
end;

function TLrTreeNode.GetCount: Integer;
begin
	Result := ComponentCount;
end;

function TLrTreeNode.GetItems(inIndex: Integer): TLrTreeLeaf;
begin
	Result := TLrTreeLeaf(Components[inIndex]);
end;

procedure TLrTreeNode.Add(inItem: TLrTreeLeaf);

	function RandomIdent: string;
	begin
		Result := '_' + IntToHex(Random($FFFF), 4);
	end;

begin
	while (inItem.Name = '') or (Find(inItem.Name) <> nil) do
		inItem.Name := RandomIdent;
	inItem.Parent := Self;
	inItem.OnChange := ItemChange;
	inItem.OnSaveItem := ItemSaveItem;
	InsertComponent(inItem);
	Change;
end;

procedure TLrTreeNode.Remove(inItem: TLrTreeLeaf);
begin
	inItem.Parent := nil;
	inItem.OnChange := nil;
	RemoveComponent(inItem);
	Change;
end;

procedure TLrTreeNode.ItemChange(inSender: TObject);
begin
	Change;
end;

procedure TLrTreeNode.ItemSaveItem(inSender: TLrTreeLeaf; inIndex: Integer;
	var inAllow: Boolean);
begin
	DoOnSaveItem(inSender, inIndex, inAllow);
end;

function TLrTreeNode.Find(const inName: string): TLrTreeLeaf;
var
	i: Integer;
begin
	Result := TLrTreeLeaf(FindComponent(inName));
	if Result = nil then
		for i := 0 to Pred(Count) do
			if Items[i] is TLrTreeNode then
			begin
				Result := TLrTreeNode(Items[i]).Find(inName);
				if Result <> nil then
					break;
			end;
end;

function TLrTreeNode.GetUniqueName(const inName: string): string;
var
	i: Integer;
begin
	if Find(inName) = nil then
		Result := inName
	else begin
		i := 0;
		repeat
			Inc(i);
			Result := inName + IntToStr(i);
		until Find(Result) = nil;
	end;
end;

end.
