unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ImgList, ComCtrls, ToolWin, ExtCtrls, Menus,
	aqDockingUtils, aqDockingBase, aqDocking, aqDockingUI,
	TBXSwitcher, TB2Item, TBX, TB2Dock, TB2Toolbar,
	Controller, PngImageList,
	dxBar, dxBarExtItems;

const
	CM_APPSTART = WM_USER + $01;

type
	TMainForm = class(TForm)
		TBXDock1: TTBXDock;
		TBXToolbar1: TTBXToolbar;
		TBXSwitcher1: TTBXSwitcher;
		DocumentsPanel: TPanel;
		aqDockingManager1: TaqDockingManager;
		aqStyleManager1: TaqStyleManager;
		aqDockingSite1: TaqDockingSite;
		PaletteDock: TaqDockingControl;
		InspectorDock: TaqDockingControl;
		DocumentsDock: TaqDockingControl;
    ProjectDock: TaqDockingControl;
    TBXSubmenuItem1: TTBXSubmenuItem;
    TBXItem9: TTBXItem;
    TBXItem10: TTBXItem;
    TBXItem11: TTBXItem;
    TBXItem12: TTBXItem;
    TBXItem13: TTBXItem;
    TBXSeparatorItem3: TTBXSeparatorItem;
    TBXItem14: TTBXItem;
    TBXSeparatorItem4: TTBXSeparatorItem;
    TBXItem15: TTBXItem;
    TBXToolbar2: TTBXToolbar;
    TBXItem2: TTBXItem;
		TBImageList1: TTBImageList;
    TBXItem3: TTBXItem;
    TBXItem5: TTBXItem;
    TBXItem6: TTBXItem;
    TBXSeparatorItem5: TTBXSeparatorItem;
    TBXSeparatorItem6: TTBXSeparatorItem;
    TBXItem7: TTBXItem;
    TBXItem4: TTBXItem;
    TBItem1: TTBItem;
    TBXSubmenuItem2: TTBXSubmenuItem;
    TBXItem1: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    TBXItem8: TTBXItem;
    TBXItem16: TTBXItem;
    dxBarManager1: TdxBarManager;
    dxBarDockControl1: TdxBarDockControl;
    dxBarButton1: TdxBarButton;
    dxBarSubItem1: TdxBarSubItem;
    dxBarButton2: TdxBarButton;
    dxBarSubItem2: TdxBarSubItem;
    dxBarInPlaceSubItem1: TdxBarInPlaceSubItem;
    PopupMenu1: TdxBarPopupMenu;
    New1: TdxBarButton;
    Save1: TdxBarButton;
    SaveAs1: TdxBarButton;
    dxBarToolbarsListItem1: TdxBarToolbarsListItem;
    dxBarSubItem3: TdxBarSubItem;
    dxBarButton3: TdxBarButton;
    dxBarLargeButton1: TdxBarLargeButton;
    dxBarButton4: TdxBarButton;
    dxBarButton5: TdxBarButton;
    dxBarButton6: TdxBarButton;
    dxBarButton7: TdxBarButton;
    dxBarButton8: TdxBarButton;
    dxBarButton9: TdxBarButton;
    dxBarButton10: TdxBarButton;
    dxBarButton11: TdxBarButton;
    dxBarSubItem4: TdxBarSubItem;
    dxBarButton12: TdxBarButton;
    ComponentsDock: TaqDockingControl;
    TBXItem17: TTBXItem;
    dxBarListItem1: TdxBarListItem;
    dxBarButton13: TdxBarButton;
		procedure FormCreate(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure InspectorDockEnter(Sender: TObject);
    procedure InspectorDockExit(Sender: TObject);
	private
		{ Private declarations }
		procedure CMAppStart(var Message: TMessage); message CM_APPSTART;
	public
		{ Public declarations }
	end;

var
	MainForm: TMainForm;

implementation

uses
	LrUtils, DockingUtils, Globals, Registration, Inspector, Palette, Documents,
	DesignManager, LiteBrowserDocument, TurboDocumentHost, ProjectView,
	ComponentTreeView;

const
	cDockingFile = 'aq.docking.cfg';

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
	Registration.Register;
	//
	DesignMgr := TDesignManager.Create(Self);
	//
	AddForm(PaletteForm, TPaletteForm, PaletteDock);
	AddForm(InspectorForm, TInspectorForm, InspectorDock);
	AddForm(ComponentTreeForm, TComponentTreeForm, ComponentsDock);
	AddForm(DocumentsForm, TDocumentsForm, DocumentsDock);
	AddForm(ProjectForm, TProjectForm, ProjectDock);
	//
	//DesignMgr.Inspector := InspectorForm;
	//DesignMgr.Palette := PaletteForm;
	//
	PostMessage(Handle, CM_APPSTART, 0, 0);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	ControllerModule.Shutdown;
	SaveDockLayout(aqDockingManager1, ConfigHome + cDockingFile);
	TurboDocumentHostForm.SaveDockConfig;
end;

procedure TMainForm.CMAppStart(var Message: TMessage);
begin
	LoadDockLayout(aqDockingManager1, ConfigHome + cDockingFile);
	ComponentsDock.Visible := true;
	ControllerModule.Startup;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	CanClose := ControllerModule.CloseAll and ControllerModule.CloseProject;
end;

procedure TMainForm.InspectorDockEnter(Sender: TObject);
begin
	with TaqCustomDockingControl(Sender) do
		if ActiveControl <> nil then
			ActiveControl.RemoveFreeNotification(TComponent(Sender));
end;

procedure TMainForm.InspectorDockExit(Sender: TObject);
begin
	with TaqCustomDockingControl(Sender) do
		if ActiveControl <> nil then
			ActiveControl.FreeNotification(TComponent(Sender));
end;

end.
