unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ImgList, ComCtrls, ToolWin,
	aqDockingUtils, aqDockingBase, aqDocking, aqDockingUI,
	TBXSwitcher, TB2Item, TBX, TB2Dock,	TB2Toolbar,
	JvTabBar,
	Design, DesignManager, Controller, TurboPhpDocument, Desktop, TBXDkPanels,
  ExtCtrls;

type
	TMainForm = class(TForm)
		TBXDock1: TTBXDock;
		TBXToolbar1: TTBXToolbar;
		TBXItem1: TTBXItem;
		TBXSwitcher1: TTBXSwitcher;
		TBXSeparatorItem1: TTBXSeparatorItem;
		TBXItem2: TTBXItem;
		TBXItem3: TTBXItem;
		TBXItem4: TTBXItem;
		TBXSeparatorItem2: TTBXSeparatorItem;
		JvModernTabBarPainter1: TJvModernTabBarPainter;
    TBXMultiDock1: TTBXMultiDock;
    TBXMultiDock2: TTBXMultiDock;
    PaletteDockable: TTBXDockablePanel;
    InspectorDockable: TTBXDockablePanel;
    ProjectManageDockable: TTBXDockablePanel;
    JvTabBar1: TJvTabBar;
    JvModernTabBarPainter2: TJvModernTabBarPainter;
    DocumentsPanel: TPanel;
    DocumentTabs: TJvTabBar;
		procedure FormCreate(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
	private
		{ Private declarations }
		procedure DocumentsChanged(Sender: TObject);
		procedure UpdateDocumentTabs;
	public
		{ Public declarations }
	end;

var
	DesignManager: TDesignManager;
	Document: TTurboPhpDocument;
	Desktop: TDesktop;
	Home: string;
	MainForm: TMainForm;

implementation

uses
	LrUtils, DatabaseSetup, Inspector, Palette, Registration, DesignHost, PhpEdit,
  Documents;

const
	cDockingFile = 'docking.cfg';

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
	Registration.Register;
	//
	Home := ExtractFilePath(Application.ExeName);
	//
//	if FileExists(Home + cDockingFile) then
//		aqDockingManager1.LoadFromFile(Home + cDockingFile);
	//
	AddForm(PaletteForm, TPaletteForm, PaletteDockable);
	AddForm(InspectorForm, TInspectorForm, InspectorDockable);
	//
	AddForm(DocumentsForm, TDocumentsForm, DocumentsPanel); //DocumentDock);
	AddForm(DesignHostForm, TDesignHostForm, DocumentsForm.DesignDock);
	AddForm(PhpEditForm, TPhpEditForm, DocumentsForm.PhpDock);
	//
	DesignManager := TDesignManager.Create(Self);
	DesignManager.Design := DesignHostForm.DesignForm;
	DesignManager.Inspector := InspectorForm;
	//
	Document := TTurboPhpDocument.Create;
	Document.DesignForm := DesignHostForm.DesignForm;
	//
	//if FileExists(Home + 'Default' + cPageExt) then
	//	Document.Open(Home + 'Default' + cPageExt);
	//
	Desktop := TDesktop.Create;
	Desktop.OnDocumentsChanged := DocumentsChanged;
	Desktop.AddDocument(Document);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//	aqDockingManager1.SaveToFile(Home + cDockingFile);
end;

procedure TMainForm.DocumentsChanged(Sender: TObject);
begin
	UpdateDocumentTabs;
end;

procedure TMainForm.UpdateDocumentTabs;
var
	i: Integer;
begin
	DocumentTabs.Tabs.BeginUpdate;
	try
		DocumentTabs.Tabs.Clear;
		for i := 0 to Pred(Desktop.Count) do
			DocumentTabs.AddTab(Desktop.Documents[i].DisplayName);
	finally
		DocumentTabs.Tabs.EndUpdate;
	end;
end;

end.
