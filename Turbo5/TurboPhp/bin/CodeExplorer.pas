unit CodeExplorer;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ComCtrls, ImgList, ExtCtrls, Menus,
	EasyClasses, EasyEditSource, EasyEditor, PhpParser;

type
	TCodeExplorerForm = class(TForm)
		TreeView: TTreeView;
		ExplorerImages: TImageList;
		PopupMenu1: TPopupMenu;
		ShowCode1: TMenuItem;
		procedure TreeViewDblClick(Sender: TObject);
		procedure FormHide(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
	private
		{ Private declarations }
		FEasyEdit: TEasyEdit;
		FEasyParser: TCustomEasyParser;
		//FNeedExpand: Boolean;
		FParser: TUnitInfo;
		FList: TStrings;
	protected
		function GetEasyParser: TCustomEasyParser;
		function GetNodeText(Node: TTreeNode): string;
		procedure AfterRefresh(inList: TStrings);
		procedure BeforeRefresh(inList: TStrings);
		procedure ExpandWithParent(Node: TTreeNode);
		procedure FillTree;
		procedure Parse;
	public
		{ Public declarations }
		procedure UpdateExplorer;
	public
		property EasyEdit: TEasyEdit read FEasyEdit write FEasyEdit;
		property EasyParser: TCustomEasyParser read GetEasyParser
			write FEasyParser;
	end;

implementation

uses
	EditUnit, EasyUtils, EasyParser, EasyKeyMap;

{$R *.DFM}

const
	sPublicStr = 'public';
	sClasses = 'classes';
	sConstants = 'constants';
	sVariables = 'variables';
	sFunctions = 'functions';

procedure TCodeExplorerForm.FormCreate(Sender: TObject);
begin
	FParser := TUnitInfo.Create;
end;

procedure TCodeExplorerForm.FormDestroy(Sender: TObject);
begin
	FParser.Free;
end;

function TCodeExplorerForm.GetNodeText(Node: TTreeNode): string;
begin
	Result := '';
	while Node <> nil do
	begin
		if Result = '' then
			Result := Node.Text
		else
			Result := Result + '.' + Node.Text;
		Node := Node.Parent;
	end;
end;

procedure TCodeExplorerForm.ExpandWithParent(Node: TTreeNode);
begin
	while Node <> nil do
	begin
		Node.Expand(False);
		Node := Node.Parent;
	end;
end;

procedure TCodeExplorerForm.BeforeRefresh(inList: TStrings);
var
	Node: TTreeNode;
begin
	with TreeView, Items do
	begin
		Node := GetFirstNode;
		while Node <> nil do
		begin
			if Node.Expanded then
//				if (Node.Data <> nil) and (TObject(Node.Data) is TElementInfo) then
//					inList.Add(TElementInfo(Node.Data).Name)
//				else
					inList.Add(GetNodeText(Node));
			Node := Node.GetNextVisible;
		end;
	end;
end;

procedure TCodeExplorerForm.AfterRefresh(inList : TStrings);
var
	Node: TTreeNode;
	s: string;
begin
	with TreeView, Items do
	begin
		Node := GetFirstNode;
		while Node <> nil do
		begin
//			if (Node.Data <> nil) and (TObject(Node.Data) is TElementInfo) then
//				s := TElementInfo(Node.Data).Name
//			else
				s := GetNodeText(Node);
			if (inList.IndexOf(s) >= 0) then
				Node.Expanded := true; //ExpandWithParent(Node);
			Node := Node.GetNext;
		end;
	end;
end;

procedure TCodeExplorerForm.Parse;
{$IFDEF EASY_WIDESTRINGS}
var
	i: integer;
	AStrings: TStrings;
{$ENDIF}
begin
	if EasyEdit <> nil then
		with EasyEdit do
		begin
			FParser.Parser := TEasyEditorParser(EasyParser);
			{$IFDEF EASY_WIDESTRINGS}
			AStrings := TStringList.Create;
			try
				for i := 0 to Lines.Count - 1 do
					AStrings.Add(Lines[i]);
				FParser.ReparseStrings(AStrings);
			finally
				AStrings.Free;
			end;
			{$ELSE}
			FParser.ReparseStrings(Lines);
			{$ENDIF}
		end
	else
		FParser.ReparseStrings(nil);
end;

procedure TCodeExplorerForm.UpdateExplorer;
begin
	Parse;
	//FNeedExpand := (Count = 0);
	FList := CreateSortedStrings;
	try
		BeforeRefresh(FList);
		FillTree;
		AfterRefresh(FList);
	finally
		FList.Free;
	end;
end;

procedure TCodeExplorerForm.FillTree;
var
	node: TTreeNode;

	function _AddNode(inNode: TTreeNode; const s: string; ImageIndex: integer;
		Data: Pointer; Expand: Boolean): TTreeNode;
	begin
		Result := TreeView.Items.AddChildObject(inNode, s, Data);
		Result.ImageIndex := ImageIndex;
		Result.SelectedIndex := ImageIndex;
		if Expand then
//			if Data = nil then
				FList.Add(GetNodeText(Result))
//			else
//				FList.Add(s);
	end;

	procedure ProcessScope(const ACaption: string; ImageIndex: integer;
		inNode: TTreeNode; AScope: TElementScope; Strings: TStrings;
		Expand: Boolean);
	var
		i: Integer;
		info: TElementInfo;
		aNode, n: TTreeNode;
	begin
		aNode := nil;
		with TreeView.Items, Strings do
			for i := 0 to Count - 1 do
			begin
				info := TElementInfo(Objects[i]);
				if (info <> nil) and (info.Scope = AScope) then
				begin
					if aNode = nil then
						aNode := _AddNode(inNode, ACaption, 0, nil, true);//Expand);
					if (info is TClassInfo) then
					begin
						n := _AddNode(aNode, info.Name, ImageIndex, info, false);
						ProcessScope('Fields', 2, n, sLocalVar,
							TClassInfo(info).Fields, false);
						ProcessScope('Methods', 1, n, sPublic,
							TClassInfo(info).Methods, true);
					end
					else if (info is TFunctionInfo) then
					begin
						{n :=} _AddNode(aNode, info.Name, ImageIndex, info, false);
						{
						ProcessScope('Globals', 2, n, sGlobalDecl,
							TFunctionInfo(info).GlobalDecls, true);
						ProcessScope('References', 2, n, sUsedVar,
							TFunctionInfo(info).UsedVars, true);
						}
					end	else
						_AddNode(aNode, info.Name, ImageIndex, info, Expand);
				end;
			end;
	end;

	procedure ProcessStrings(const inCaption: string; inImageIndex: integer;
		inStrings: TStrings);
	begin
		//ProcessScope(inCaption, inImageIndex, node, sPublic, inStrings, FNeedExpand);
		ProcessScope(inCaption, inImageIndex, node, sPublic, inStrings, false);
	end;

begin
	with TreeView, Items do
	begin
		BeginUpdate;
		try
			Clear;
			node := nil;
			with FParser do
			begin
				ProcessStrings(sConstants, 4, Constants);
				ProcessStrings(sClasses, 1, Classes);
				ProcessStrings(sVariables, 2, Variables);
				ProcessStrings(sFunctions, 1, Functions);
			end;
		finally
			EndUpdate;
		end;
	end;
end;

procedure TCodeExplorerForm.TreeViewDblClick(Sender: TObject);
begin
	with TreeView do
	try
		if (Selected <> nil) and (TObject(Selected.Data) is TElementInfo) then
		begin
			if EasyEdit <> nil then
				with EasyEdit do
				begin
					EditSource.BeginSourceUpdate(oprNavigate);
					try
						EditSource.JumpToLine(TElementInfo(Selected.Data).LineNo);
						Navigate(cCenterLine);
						SetFocus;
						Windows.SetFocus(Handle);
					finally
						EditSource.EndSourceUpdate;
					end;
				end;
		end;
	except
	end;
end;

procedure TCodeExplorerForm.FormHide(Sender: TObject);
begin
	if Parent is TPanel then
		Parent.Width := 1;
end;

function TCodeExplorerForm.GetEasyParser: TCustomEasyParser;
begin
	if FEasyParser = nil then
		Result := EasyEdit.Parser
	else
		Result := FEasyParser;
end;

end.
