unit TabExplorerView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ToolWin, VirtualTrees, VirtualExplorerTree,
  cxControls, cxPC;

type
  TTabExplorerForm = class(TForm)
		RecentDropdown: TPopupMenu;
    TabControl: TcxTabControl;
    ExplorerTree: TVirtualExplorerTree;
    ToolBar1: TToolBar;
		ToolButton2: TToolButton;
		ToolButton1: TToolButton;
		ToolButton3: TToolButton;
		procedure TabControlChange(Sender: TObject);
    procedure ExplorerTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
	private
		{ Private declarations }
		FAnyFolder: string;
		FSourceFolder: string;
		FPublishFolder: string;
		procedure SetAnyFolder(const Value: string);
		procedure SetPublishFolder(const Value: string);
		procedure SetSourceFolder(const Value: string);
		function Tab: string;
		procedure UpdateTabs;
	public
		{ Public declarations }
		procedure SelectTab(inIndex: Integer);
		property SourceFolder: string read FSourceFolder write SetSourceFolder;
		property PublishFolder: string read FPublishFolder write SetPublishFolder;
		property AnyFolder: string read FAnyFolder write SetAnyFolder;
	end;

var
	TabExplorerForm: TTabExplorerForm;

implementation

{$R *.dfm}

{ TExplorerForm }

function TTabExplorerForm.Tab: string;
begin
	with TabControl do
		Result := Tabs[TabIndex].Caption;
end;

procedure TTabExplorerForm.ExplorerTreeChange(Sender: TBaseVirtualTree;
	Node: PVirtualNode);
begin
	if Tab = 'All' then
		FAnyFolder := ExplorerTree.SelectedPath;
end;

procedure TTabExplorerForm.TabControlChange(Sender: TObject);
begin
	if Tab = 'Source' then
		ExplorerTree.RootFolderCustomPath := SourceFolder
	else if Tab = 'Publish' then
		ExplorerTree.RootFolderCustomPath := PublishFolder
	else if Tab = 'All' then
	begin
		ExplorerTree.RootFolder := rfDesktop;
		if AnyFolder <> '' then
			ExplorerTree.BrowseTo(AnyFolder, false);
	end;
end;

procedure TTabExplorerForm.UpdateTabs;
begin
	TabControl.Tabs.BeginUpdate;
	try
		TabControl.Tabs.Clear;
		if SourceFolder <> '' then
			TabControl.Tabs.Add('Source');
		if PublishFolder <> '' then
			TabControl.Tabs.Add('Publish');
		TabControl.Tabs.Add('All');
	finally
		TabControl.Tabs.EndUpdate;
	end;
end;

procedure TTabExplorerForm.SetAnyFolder(const Value: string);
begin
	FAnyFolder := Value;
	UpdateTabs;
end;

procedure TTabExplorerForm.SetPublishFolder(const Value: string);
begin
	FPublishFolder := Value;
	UpdateTabs;
end;

procedure TTabExplorerForm.SetSourceFolder(const Value: string);
begin
	FSourceFolder := Value;
	UpdateTabs;
end;

procedure TTabExplorerForm.SelectTab(inIndex: Integer);
begin
	TabControl.TabIndex := inIndex;
	TabControl.OnChange(TabControl);
end;

end.
