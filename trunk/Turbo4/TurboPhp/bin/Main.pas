unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, Buttons, StdCtrls, ExtCtrls, ComCtrls, ImgList, ToolWin, ActnList,
	Menus, TypInfo,
	madExceptVcl,
	dcsystem, DCPalette, dcpalet, dcfdes, dccommon, dcpload, DCGen, dcdreamlib,
	dfsSplitter,
	dxDockControl, dxDockPanel,
	VirtualTrees, VirtualExplorerTree,
	EasyEditor, EasyKeyMap,
	LMDCustomComponent, lmdcont, LMDControl,
	LMDBaseControl, LMDBaseGraphicControl, LMDBaseLabel, LMDCustomGlyphLabel,
	LMDGraph, LMDGlyphLabel,
	ovcmru, ovcbase, ovcfiler, ovcstore,
	JvExExtCtrls, JvComponent, JvRollOut,
	LrDockUtils, LrDocument, LrProject,
	ThInterfaces, ThCssStyle, ThStyledCtrl,
	Project,
	PhpEditView,
	PageDocument, PageController, PageView,
	RawDocument, PhpController, PhpView,
	HelpView, ExplorerView, DebugView, ProjectView,
	StyleActionBar, DropTarget, DropSource, LrCollapsable, LMDBaseController,
	LMDCustomContainer, LMDCustomImageList, LMDImageList;

const
	CM_APPSTART = WM_USER + $01;
	CM_LAZY_FOCUS = WM_USER + $02;

type
	TMainForm = class(TForm)
		dxDockingManager: TdxDockingManager;
		ToolImages: TImageList;
		ActionList1: TActionList;
		NewAction: TAction;
    OpenAction: TAction;
    SaveAction: TAction;
    SaveAsAction: TAction;
    PublishAction: TAction;
		ExitAction: TAction;
    CloseAction: TAction;
		SaveOrSaveAsAction: TAction;
		DataExplorerAction: TAction;
		ProjectOptionsAction: TAction;
    OpenProjectAction: TAction;
		NewProjectAction: TAction;
		SaveProjectAction: TAction;
		SaveProjectAsAction: TAction;
		InstallPackagesAction: TAction;
		CloseAllAction: TAction;
		DesignerCopyAction: TAction;
		DesignerCutAction: TAction;
		DesignerPasteAction: TAction;
		DesignerDeleteAction: TAction;
		PalettePanel: TPanel;
		Bevel2: TBevel;
		DCCompPalette1: TDCCompPalette;
		Panel6: TPanel;
		Bevel3: TBevel;
		Bevel4: TBevel;
		ToolBar2: TToolBar;
		ToolButton5: TToolButton;
		ToolButton6: TToolButton;
		ToolButton7: TToolButton;
		ToolButton8: TToolButton;
		ToolButton9: TToolButton;
		ToolButton10: TToolButton;
		ToolButton11: TToolButton;
		ToolButton12: TToolButton;
		ToolBar3: TToolBar;
		OpenDialog: TOpenDialog;
		DesignPopup: TPopupMenu;
		Cut1: TMenuItem;
		Copy1: TMenuItem;
		Paste1: TMenuItem;
		Delete1: TMenuItem;
		PropStore: TDCPropStore;
		SaveLayoutDialog: TSaveDialog;
		OpenLayoutDialog: TOpenDialog;
		SaveLayoutAction: TAction;
		LoadLayoutAction: TAction;
    MainMenu1: TMainMenu;
		File1: TMenuItem;
    Open1: TMenuItem;
    NewProject1: TMenuItem;
		Open2: TMenuItem;
    OpenProject1: TMenuItem;
    N1: TMenuItem;
		Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Close1: TMenuItem;
    N2: TMenuItem;
    CloseAll1: TMenuItem;
		SaveProjectAs1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
		View1: TMenuItem;
		SaveAs2: TMenuItem;
		SaveLayout1: TMenuItem;
		Project1: TMenuItem;
		ProjectOptions1: TMenuItem;
		Publish2: TMenuItem;
		Compeonnt1: TMenuItem;
		InstallPackages1: TMenuItem;
    Help1: TMenuItem;
		About1: TMenuItem;
    Publish1: TMenuItem;
		OpenProjectDialog: TOpenDialog;
    SaveProject1: TMenuItem;
		SaveProjectDialog: TSaveDialog;
		HelpDock: TdxDockPanel;
    PublishExplorerDock: TdxDockPanel;
    ClientDockSite: TdxDockSite;
    dxLayoutDockSite1: TdxLayoutDockSite;
    DocumentPanel: TPanel;
		CaptionPanel: TPanel;
    TabControl: TTabControl;
		CaptionButtonsPanel: TPanel;
		CloseButton: TDCSpeedButton;
    Shape1: TShape;
    HidePanel: TPanel;
		ObjectInspectorAction: TAction;
    N6: TMenuItem;
    ObjectInspector1: TMenuItem;
    ComponentListAction: TAction;
    ControlTreeAction: TAction;
    SourceExplorerAction: TAction;
    PublishExplorerAction: TAction;
		HelpPanelAction: TAction;
    ComponentList1: TMenuItem;
		ControlTree1: TMenuItem;
		DCPackageLoader: TDCPackageLoader;
		RevertLayoutAction: TAction;
		RevertLayout1: TMenuItem;
		N7: TMenuItem;
		SourceExplorer1: TMenuItem;
		PublishExplorer1: TMenuItem;
		HelpPanel1: TMenuItem;
    MadExceptionHandler1: TMadExceptionHandler;
		ProjectDock: TdxDockPanel;
    DocumentDock: TdxDockPanel;
    Edit1: TMenuItem;
    Copy2: TMenuItem;
    Cut2: TMenuItem;
    Delete2: TMenuItem;
    Paste2: TMenuItem;
    CutAction: TAction;
    CopyAction: TAction;
		PasteAction: TAction;
    DeleteAction: TAction;
		SelectAllAction: TAction;
    SelectAll1: TMenuItem;
		UndoAction: TAction;
		RedoAction: TAction;
		Undo1: TMenuItem;
    Redo1: TMenuItem;
		N8: TMenuItem;
    DesignerUndoAction: TAction;
    DesignerRedoAction: TAction;
    N9: TMenuItem;
    EditPopup: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    Undo2: TMenuItem;
    Redo2: TMenuItem;
    N10: TMenuItem;
		FactoryLayoutAction: TAction;
		FactoryLayout1: TMenuItem;
    FindAction: TAction;
    FindAgainAction: TAction;
    ReplaceAction: TAction;
    AddNewPageAction: TAction;
    AddExistingPageAction: TAction;
    AddNewPage1: TMenuItem;
    AddExistingPage1: TMenuItem;
    N11: TMenuItem;
    FactoryLayout2: TMenuItem;
    FindAgain1: TMenuItem;
    Replace1: TMenuItem;
    SelectAll2: TMenuItem;
    N12: TMenuItem;
		Find1: TMenuItem;
		FindAgain2: TMenuItem;
    Replace2: TMenuItem;
    DCFileOps: TDCFileOperation;
    N4: TMenuItem;
    Undo3: TMenuItem;
    Redo3: TMenuItem;
		DockablePopup: TPopupMenu;
    DockableItem: TMenuItem;
    AboutAction: TAction;
    dfsSplitter1: TdfsSplitter;
    dxDockSite1: TdxDockSite;
    dxLayoutDockSite2: TdxLayoutDockSite;
    dxTabContainerDockSite1: TdxTabContainerDockSite;
    dxVertContainerDockSite2: TdxVertContainerDockSite;
    HelpAction: TAction;
    HelpAction1: TMenuItem;
    RecentPages: TOvcMenuMRU;
    OpenRecent1: TMenuItem;
    MruItem: TMenuItem;
		OvcIniFileStore1: TOvcIniFileStore;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton13: TToolButton;
		ToolButton14: TToolButton;
		ToolButton15: TToolButton;
		ToolButton16: TToolButton;
		LMDImageList1: TLMDImageList;
		PaletteScroll: TScrollBox;
		DbExplorerDock: TdxDockPanel;
		DropTextSource1: TDropTextSource;
		DropTextTarget1: TDropTextTarget;
		RecentProjects: TOvcMenuMRU;
		RecentFiles: TOvcMenuMRU;
		LrCollapsable1: TLrCollapsable;
		procedure FormCreate(Sender: TObject);
		procedure OpenActionExecute(Sender: TObject);
		procedure PublishActionExecute(Sender: TObject);
		procedure SaveLayoutActionExecute(Sender: TObject);
		procedure LoadLayoutActionExecute(Sender: TObject);
		procedure NewActionExecute(Sender: TObject);
		procedure DesignerCopyActionExecute(Sender: TObject);
		procedure DesignerCutActionExecute(Sender: TObject);
		procedure DesignerPasteActionExecute(Sender: TObject);
		procedure DesignerDeleteActionExecute(Sender: TObject);
		procedure DesignerCopyActionUpdate(Sender: TObject);
		procedure DesignerCutActionUpdate(Sender: TObject);
		procedure DesignerPasteActionUpdate(Sender: TObject);
		procedure DesignerDeleteActionUpdate(Sender: TObject);
		procedure SaveActionExecute(Sender: TObject);
		procedure SaveActionUpdate(Sender: TObject);
		procedure SaveAsActionUpdate(Sender: TObject);
		procedure SaveAsActionExecute(Sender: TObject);
		procedure dxDockingManagerCreateFloatSite(
			Sender: TdxCustomDockControl; AFloatSite: TdxFloatDockSite);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure OpenProjectActionExecute(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure ActionRequiringProjectUpdate(Sender: TObject);
		procedure ProjectOptionsActionExecute(Sender: TObject);
		procedure NewProjectActionExecute(Sender: TObject);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure SaveProjectActionUpdate(Sender: TObject);
		procedure SaveProjectActionExecute(Sender: TObject);
		procedure SaveProjectAsActionExecute(Sender: TObject);
		procedure SaveOrSaveAsActionExecute(Sender: TObject);
		procedure ExplorerTreeDblClick(Sender: TObject);
		procedure CloseActionUpdate(Sender: TObject);
		procedure CloseActionExecute(Sender: TObject);
		procedure CloseAllActionExecute(Sender: TObject);
		procedure ExitActionExecute(Sender: TObject);
		procedure TabControlChange(Sender: TObject);
		procedure NeedPageDocActionUpdate(Sender: TObject);
		procedure dxDockingManagerCreateSideContainer(
			Sender: TdxCustomDockControl;
			ASideContainer: TdxSideContainerDockSite);
		procedure InstallPackagesActionExecute(Sender: TObject);
		procedure RevertLayoutActionExecute(Sender: TObject);
		procedure NoCloseQuery(Sender: TdxCustomDockControl;
			var CanClose: Boolean);
		procedure FactoryLayoutActionExecute(Sender: TObject);
		procedure PropStoreLoadChanges(Sender: TObject;
			var processed: Boolean);
		procedure CutCopyDeleteActionUpdate(Sender: TObject);
		procedure CopyActionExecute(Sender: TObject);
		procedure CutActionExecute(Sender: TObject);
		procedure PasteActionUpdate(Sender: TObject);
		procedure PasteActionExecute(Sender: TObject);
		procedure DeleteActionExecute(Sender: TObject);
		procedure SelectAllActionUpdate(Sender: TObject);
		procedure SelectAllActionExecute(Sender: TObject);
		procedure UndoActionUpdate(Sender: TObject);
		procedure DesignerUndoActionUpdate(Sender: TObject);
		procedure DesignerUndoActionExecute(Sender: TObject);
		procedure DesignerRedoActionUpdate(Sender: TObject);
		procedure DesignerRedoActionExecute(Sender: TObject);
		procedure UndoActionExecute(Sender: TObject);
		procedure RedoActionUpdate(Sender: TObject);
		procedure RedoActionExecute(Sender: TObject);
		procedure FindActionExecute(Sender: TObject);
		procedure EasyEditActionUpdate(Sender: TObject);
		procedure ReplaceActionExecute(Sender: TObject);
		procedure FindAgainActionExecute(Sender: TObject);
		procedure AddNewPageActionExecute(Sender: TObject);
		procedure AddExistingPageActionExecute(Sender: TObject);
		procedure DockableItemClick(Sender: TObject);
		procedure DockablePopupPopup(Sender: TObject);
		procedure AboutActionExecute(Sender: TObject);
		procedure HelpActionExecute(Sender: TObject);
		procedure RecentPagesClick(Sender: TObject; const ItemText: String;
			var Action: TOvcMRUClickAction);
    procedure PaletteLabelMouseEnter(Sender: TObject);
    procedure PaletteLabelMouseExit(Sender: TObject);
    procedure PaletteLabelClick(Sender: TObject);
    procedure PaletteLabelStartDrag(Sender: TObject;
      var DragObject: TDragObject);
		procedure PaletteLabelMouseDown(Sender: TObject;
			Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MadExceptionHandler1Exception(frozen: Boolean;
      exceptObject: TObject; exceptAddr: Pointer;
      crashedThreadId: Cardinal; var bugReport, screenShot: String;
      var canContinue, handled: Boolean);
	private
		{ Private declarations }
		FDefaultLayout: string;
		function ComponentHelp(inComponent: TPersistent;
			const inProperty: string): Boolean;
		function GetActiveDesigner: TDCLiteDesigner;
		function GetDocument: TLrDocument;
		function GetFileFilter: string;
		function GetPageDocument: TPageDocument;
		function GetPhpDocument: TPhpDocument;
		function GetProject: TProject;
		function IsDesignerEditTarget: Boolean;
		function IsPageDocument: Boolean;
		function IsPhpDocument: Boolean;
		function OpenDocument(inManager: TLrDocumentManager;
			const inPath: string): TLrDocument;
		function PropertyHelp(const inName, inProperty: string): Boolean;
		procedure AfterOpenDocument(inSender: TObject; inDocument: TLrDocument);
		procedure CloseDocument(inSender: TObject);
		procedure CopyDefaultProjectItems;
		procedure CurrentProjectChanged(inSender: TObject);
		procedure CurrentChanged(inSender: TObject);
		procedure SaveAsDocument(inSender: TObject; inDocument: TLrDocument;
			const inOldPath, inNewPath: string);
		procedure DocumentProjectSettingsChange;
		procedure FixFloatForm(inForm: TCustomForm);
		procedure FixFloatForms;
		procedure LoadDefaultLayout;
		procedure LoadLayout(const inFilename: string);
		procedure PageDocumentProjectSettingsChange(inDoc: TPageDocument);
		procedure ProjectChanged(inSender: TObject);
		procedure ProjectItemOpen(Sender: TObject);
		procedure PublishPageDocument(inDocument: TPageDocument);
		procedure PublishPhpDocument(inDocument: TPhpDocument);
		procedure ReactivateDesigner;
		procedure SaveLayout(const inFilename: string);
		procedure SelectRawDocument;
		procedure SelectPageDocument;
		procedure UpdateTabs;
		procedure VerifyPublishFolder;
		procedure NoDocument;
		function DisplayHelp(inFile: string = ''): Boolean;
		procedure StyleFontControlsUpdate;
		procedure OpenProject(const inFilename: string);
    procedure BuildPalette;
	protected
		procedure CMAppStart(var inMsg: TMessage); message CM_APPSTART;
		procedure CMLazyFocus(var inMsg: TMessage); message CM_LAZY_FOCUS;
	public
		{ Public declarations }
		DocMgr: TLrDocumentManager;
		HelpForm: THelpForm;
		LastEditor: TWinControl;
		PageView: TPageViewForm;
		PhpView: TPhpViewForm;
		ProjectForm: TProjectForm;
		ProjectMgr: TLrProjectManager;
		PublishExplorerForm: TExplorerForm;
		SourceExplorerForm: TExplorerForm;
		StyleBarForm: TStyleActionBarForm;
		SelectedLabel: TObject;
	public
		procedure EditorEnter(Sender: TObject);
		procedure DesignSelectionChange(Sender: TObject);
		procedure DesignPropChange(Sender: TObject);
		procedure InspectorActivePropertyChanged(Sender: TObject);
		procedure InspectorHelp;
		procedure Open(const inFilename: string);
	public
		property ActiveDesigner: TDCLiteDesigner read GetActiveDesigner;
		property DefaultLayout: string read FDefaultLayout write FDefaultLayout;
		property Document: TLrDocument read GetDocument;
		property PageDocument: TPageDocument read GetPageDocument;
		property PhpDocument: TPhpDocument read GetPhpDocument;
		property Project: TProject read GetProject;
	end;

var
	MainForm: TMainForm;

implementation

uses
	Clipbrd, StrUtils,
	GifImage, PngImage,
	LrUtils,
	ThVclUtils, ThMessages, ThPicture,
	TpClassInfo, TpPictureProperty,
	Strings, Registration, Config, ConnectionStore, ProjectOptions,
	PhpEventProperty, JsInspector, HtmlEditView,
	InspectorView, AboutView, StartView, DbExplorerView;

{$R *.dfm}

const
	SLRRegKey = 'Software\Least-Resistance Software\';
	STPRegKey = 'TurboPhp\';
	SVersionKey = '4';
	SRegKey = SLRRegKey + STPRegKey + SVersionKey;

const
	cDefaultLayoutFile = 'default.turbophp.cfg';
	cFactoryLayoutFile = 'factory.turbophp.cfg';
	cProjectManagerFile = 'projects.turbophp.cfg';
	cPropsFile = 'desk.turbophp.cfg';
	cMruFile = 'recent.turbophp.cfg';
	cDefaultHelpTopic = '..\li_TurboPhpLib';

procedure TMainForm.FormCreate(Sender: TObject);
begin
	try
		dcsystem.RegistryKey := SRegKey;
		//
		Registration.Register;
		DCPackageLoader.DefaultLoadConfig;
		DCCompPalette1.DoubleBuffered := true;
		//
		ProjectMgr := TLrProjectManager.Create;
		ProjectMgr.SaveDialog := SaveProjectDialog;
		ProjectMgr.Filename := ConfigFolder + cProjectManagerFile;
		ProjectMgr.OnCurrentProjectChanged := CurrentProjectChanged;
		ProjectMgr.OnProjectChanged := ProjectChanged;
		//
		//AddForm(DbExplorerForm, TDbExplorerForm, DbExplorerDock);
		//
		AddForm(HelpForm, THelpForm, HelpDock);
		//
		AddForm(ProjectForm, TProjectForm, ProjectDock);
		ProjectForm.OnOpen := ProjectItemOpen;
		ProjectForm.NewPageItem.Action := AddNewPageAction;
		//
		AddForm(PublishExplorerForm, TExplorerForm, PublishExplorerDock);
		PublishExplorerForm.ExplorerTree.OnDblClick := ExplorerTreeDblClick;
		//
		DocMgr := TLrDocumentManager.Create;
		DocMgr.OnOpen := OpenDocument;
		DocMgr.OnAfterOpen := AfterOpenDocument;
		DocMgr.OnClose := CloseDocument;
		DocMgr.OnSaveAs := SaveAsDocument;
		DocMgr.OnCurrentChanged := CurrentChanged;
		//
		AddForm(PhpView, TPhpViewForm, DocumentDock);
		DocMgr.RegisterExtensions([ '.php' ], TRawDocument);
		//
		AddForm(PageView, TPageViewForm, DocumentDock);
		PageView.EditPopup := EditPopup;
		PageView.DockPopup := DockablePopup;
		PageView.PhpEditForm.Edit.OnEnter := EditorEnter;
		PageView.JsEditForm.Edit.OnEnter := EditorEnter;
		//PageView.ComponentListForm.PopupMenu := DesignPopup;
		PageView.ControlTreeForm.PopupMenu := DesignPopup;
		DocMgr.RegisterExtensions([ '.tphp', '.tppage', '.turbophp' ],
			TPageDocument);
		//
		//PaletteBar.Parent := PageView.PaletteDock;
		//PaletteBar.Align := alClient;
		PaletteScroll.Parent := PageView.PaletteDock;
		PaletteScroll.Align := alClient;
		//
		AddForm(StartForm, TStartForm, DocumentDock);
		//DocMgr.RegisterExtensions([ '' ], TStartDocument);
		//Open('Start');
		//
		AddForm(StyleBarForm, TStyleActionBarForm, Self, alTop);
		//
		ConnectionConfigFolder := ConfigFolder;
		OpenDialog.InitialDir := HomeFolder;
		DocMgr.SaveDialog.InitialDir := HomeFolder;
		OpenProjectDialog.InitialDir := HomeFolder;
		SaveProjectDialog.InitialDir := HomeFolder;
		OpenDialog.Filter := GetFileFilter;
		SaveLayoutDialog.InitialDir := ConfigFolder;
		OpenLayoutDialog.InitialDir := ConfigFolder;
		//
		NewProjectAction.Execute;
		ProjectMgr.OpenDefaultProject;
		//NewAction.Execute;
		//Project.Modified := false;
		//
		PostMessage(Handle, CM_APPSTART, 0, 0);
	except
	end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
	DocMgr.Free;
	ProjectMgr.Free;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	ProjectMgr.Save;
	SaveLayout(ConfigFolder + cDefaultLayoutFile);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	CanClose := DocMgr.CloseAll and ProjectMgr.CloseProject;
end;

procedure TMainForm.PropStoreLoadChanges(Sender: TObject;
	var processed: Boolean);
begin
	PropStore.IniFile := ConfigFolder + cPropsFile;
end;

procedure TMainForm.CMAppStart(var inMsg: TMessage);
begin
	BuildPalette;
	LoadDefaultLayout;
	ActivateDock(PageView.DesignDock);
	DisplayHelp;
	//
	NoDocument;
	//
	DbExplorerDock.Visible := false;
	{
	if not DbExplorerDock.Visible and (dxDockSite1.ChildCount > 0) then
	begin
		// add the panel to the tab container
		DbExplorerDock.DockTo(dxDockSite1.Children[1], dtClient, 0);
		DbExplorerDock.Visible := true;
	end
	}
end;

procedure TMainForm.PaletteLabelMouseEnter(Sender: TObject);
begin
	with TLMDGlyphLabel(Sender) do
		Font.Style := Font.Style + [ fsUnderline ];
end;

procedure TMainForm.PaletteLabelMouseExit(Sender: TObject);
begin
	with TLMDGlyphLabel(Sender) do
		Font.Style := Font.Style - [ fsUnderline ];
end;

procedure TMainForm.PaletteLabelMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbLeft then
		TControl(Sender).BeginDrag(False);
end;

procedure TMainForm.PaletteLabelClick(Sender: TObject);
begin
	if (SelectedLabel <> nil) then
		TLMDGlyphLabel(SelectedLabel).Font.Style := [ ];
	with TLMDGlyphLabel(Sender) do
	begin
		Font.style := [ fsBold ];
		SelectedLabel := Sender;
		DCCompPalette1.SelectedComponent := Hint;
		//DesignerInsertClass := Hint;
		//DCCompPalette1.PermanentSelection := false;
		//BroadCastToNotifiers(
		//	DCCompPalette1, CM_PALETTECLICK, Integer(DesignerInsertClass), 0);
	end;
end;

procedure TMainForm.PaletteLabelStartDrag(Sender: TObject;
	var DragObject: TDragObject);
begin
	//
end;

procedure TMainForm.BuildPalette;
var
	i, j: Integer;
	//r: TJvRollOut;
	r: TLrCollapsable;
	l: TLMDGlyphLabel;

{
	procedure CreateGroup;
	begin
		r := TJvRollOut.Create(Self);
		r.ParentColor := false;
		r.Colors.ButtonBottom := clBlack;
		r.Colors.ButtonColor := $00BEF3CA; //$00FDF9F7;
		//r.Colors.ButtonTop := clRed;
		r.Colors.FrameBottom := clWhite;
		r.Colors.FrameTop := $0094EBA7; //$00F3D1BE;
		//r.Colors.HotTrackText := clGray;
		r.Colors.Color := clWhite; //$00EFF7F8;
		r.ChildOffset := 1;
		r.Parent := PaletteScroll;
		r.Top := 9999;
		r.Align := alTop;
		r.AutoSize := true;
		r.Caption := DCCompPalette1.Tabs[i];
	end;
}

	procedure CreateGroup;
	begin
		r := TLrCollapsable.Create(Self);
		//r.ParentColor := false;
		r.Parent := PaletteScroll;
		r.Top := 9999;
		r.Align := alTop;
		r.AutoSize := true;
		r.Caption := DCCompPalette1.Tabs[i];
	end;

	function AddImage(const inClass: TClass): Integer;
	var
		b, c: TBitmap;
	begin
		b := TBitmap.Create;
		try
			LoadBitmapForClass(b, inClass);
			b.Transparent := true;
			if (b.Width <> 16) then
			begin
				c := b;
				b := TBitmap.Create;
				try
					b.Width := c.Width;
					b.Height := c.Height;
					b.Canvas.Brush.Color := clWhite;
					b.Canvas.FillRect(Rect(0, 0, c.Width, c.Height));
					b.Canvas.Draw(0, 0, c);
				finally
					c.Free;
				end;
				c := b;
				b := TBitmap.Create;
				try
					b.Width := 16;
					b.Height := 16;
					b.Canvas.StretchDraw(Rect(0,0,15,15), c);
				finally
					c.Free;
				end;
			end;
			Result := LMDImageList1.Items[0].Add(b, nil);
		finally
			b.Free;
		end;
	end;

	procedure CreateLabel(inButton: TDCPaletteButton);
	begin
		l := TLMDGlyphLabel.Create(Self);
		//
		l.Caption := TurboClassInfo.DisplayName[inButton.ButtonName];
		l.Hint := inButton.ButtonName;
		//
		l.Align := alTop;
		l.Cursor := crHandPoint;
		l.Font.Style := [];
		l.Bevel.StandardStyle := lsSingle;
		l.Alignment.Alignment := agCenterLeft;
		l.Color := clLime;
		//l.DragMode := dmAutomatic;
		//
		l.OnMouseEnter := PaletteLabelMouseEnter;
		l.OnMouseExit := PaletteLabelMouseExit;
		l.OnClick := PaletteLabelClick;
		l.OnStartDrag := PaletteLabelStartDrag;
		l.OnMouseDown := PaletteLabelMouseDown;
		//
		l.ImageList := LMDImageList1;
		l.ListIndex := 0;
		//l.ImageIndex := inButton.ImageIndex;
		l.ImageIndex := AddImage(TDCCompButton(inButton).ComponentClass);
		//
		l.AutoSize := false;
		l.Height := l.Height + 4;
		l.Parent := r;
	end;

begin
	for i := 0 to Pred(DCCompPalette1.PageCount) do
	begin
		CreateGroup;
		with DCCompPalette1.ButtonTabs[i] do
			for j := Pred(ButtonCount) downto 0 do
				CreateLabel(Buttons[j]);
	end;
end;
{
	procedure CopyImages;
	var
		i: Integer;
		b0, b1: TBitmap;
	begin
		b0 := TBitmap.Create;
		b1 := TBitmap.Create;
		with DCCompPalette1 do
		try
			b1.Width := 16;
			b1.Height := 16;
			for i := 0 to Pred(ImageList.Count) do
			begin
				ImageList.GetBitmap(i, b0);
				b1.Canvas.StretchDraw(Rect(0,0,15,15), b0);
				LMDImageList1.Items[0].Add(b1, nil);
			end;
		finally
			b1.Free;
			b0.Free;
		end;
	end;

begin
	for i := 0 to Pred(DCCompPalette1.PageCount) do
	begin
		CreateGroup;
		with DCCompPalette1.ButtonTabs[i] do
			for j := Pred(ButtonCount) downto 0 do
			begin
				Buttons[j].InitImage;
				CreateLabel(Buttons[j]);
				l.Parent := r;
			end;
	end;
	CopyImages;
end;
}

procedure TMainForm.dxDockingManagerCreateSideContainer(
	Sender: TdxCustomDockControl; ASideContainer: TdxSideContainerDockSite);
begin
	ASideContainer.CaptionButtons := [ cbMaximize ];
end;

procedure TMainForm.dxDockingManagerCreateFloatSite(
	Sender: TdxCustomDockControl; AFloatSite: TdxFloatDockSite);
begin
	FixFloatForm(Sender.FloatForm);
end;

procedure TMainForm.NoDocument;
begin
	DocMgr.Current := DocMgr.NilDocument;
	StartForm.MRU := RecentPages.Items;
	StartForm.MRUProjects := RecentProjects.Items;
	StartForm.MRUFiles := RecentFiles.Items;
	StartForm.BringToFront;
end;

procedure TMainForm.ReactivateDesigner;
begin
	if not (csDestroying in ComponentState) then
		if (ActiveDesigner <> nil) then
			if (PageView.DesignDock.Parent <> nil) then
			begin
				ActiveDesigner.Active := false;
				ActiveDesigner.Active := true;
			end;
end;

procedure TMainForm.LoadDefaultLayout;
begin
	DefaultLayout := ConfigFolder + cDefaultLayoutFile;
	if not FileExists(DefaultLayout) then
		DefaultLayout := ConfigFolder + cFactoryLayoutFile;
	LoadLayout(DefaultLayout);
end;

procedure TMainForm.RevertLayoutActionExecute(Sender: TObject);
begin
	LoadLayout(DefaultLayout);
end;

procedure TMainForm.FactoryLayoutActionExecute(Sender: TObject);
begin
	LoadLayout(ConfigFolder + cFactoryLayoutFile);
end;

function TMainForm.GetFileFilter: string;
begin
	Result := ExtensionsToFilter(
		[ cPageDocumentFileExtension,	'.turbophp', '.php', '.html', '.htm' ]);
end;

procedure TMainForm.EditorEnter(Sender: TObject);
begin
	if Sender is TWinControl then
		LastEditor := TWinControl(Sender);
end;

procedure TMainForm.UpdateTabs;
var
	i, s: Integer;
begin
	with TabControl do
	try
		OnChange := nil;
		s := TabIndex;
		Tabs.BeginUpdate;
		Tabs.Clear;
		Tabs.Add('Start');
		for i := 0 to Pred(DocMgr.Count) do
		begin
			Tabs.Add(DocMgr.Items[i].DisplayName);
			if DocMgr.Items[i] = DocMgr.Current then
				s := i + 1;
		end;
		if (s < 0) then
			s := 0;
		if (s < Tabs.Count) then
			TabIndex := s;
	finally
		Tabs.EndUpdate;
		OnChange := TabControlChange;
	end;
	//
	CloseButton.Enabled := (TabControl.Tabs.Count > 1);
	Shape1.Visible := CloseButton.Enabled;
end;

procedure TMainForm.TabControlChange(Sender: TObject);
begin
	if TabControl.TabIndex > 0 then
		DocMgr.Current := DocMgr.Items[TabControl.TabIndex - 1]
	else
		NoDocument;
	//
	CloseButton.Enabled := (TabControl.TabIndex > 0);
	Shape1.Visible := CloseButton.Enabled;
end;

procedure TMainForm.Open(const inFilename: string);
var
	e: string;
begin
	e := LowerCase(ExtractFileExt(inFilename));
	if (e = '.tpprj') then
		OpenProject(inFilename)
	else
		{MainForm.}DocMgr.Open(inFilename);
end;

function TMainForm.GetProject: TProject;
begin
	Result := TProject(ProjectMgr.CurrentProject);
end;

function TMainForm.GetDocument: TLrDocument;
begin
	Result := DocMgr.Current;
end;

function TMainForm.IsPageDocument: Boolean;
begin
	Result := Document is TPageDocument;
end;

function TMainForm.GetPageDocument: TPageDocument;
begin
	if IsPageDocument then
		Result := TPageDocument(Document)
	else
		Result := nil;
end;

function TMainForm.IsPhpDocument: Boolean;
begin
	Result := Document is TPhpDocument;
end;

function TMainForm.GetPhpDocument: TPhpDocument;
begin
	if IsPhpDocument then
		Result := TPhpDocument(Document)
	else
		Result := nil;
end;

procedure TMainForm.NewActionExecute(Sender: TObject);
begin
	DocMgr.New(TPageDocument);
	UpdateTabs;
	Project.AddPage(Document.Path);
//	ActivateDock(DesignDock);
end;

procedure TMainForm.OpenActionExecute(Sender: TObject);
begin
	if OpenDialog.Execute then
		Open(OpenDialog.Filename);
end;

procedure TMainForm.RecentPagesClick(Sender: TObject;
	const ItemText: String; var Action: TOvcMRUClickAction);
begin
	Open(ItemText);
end;

function TMainForm.OpenDocument(inManager: TLrDocumentManager;
	const inPath: string): TLrDocument;
var
	e, m: string;
	c: TLrDocumentClass;
begin
	c := nil;
	e := LowerCase(ExtractFileExt(inPath));
	//
	m := GraphicFileMask(TGraphic);
	if (Pos(e, m) > 0) then
	;
//		c := TImageDocument
//	else if (e = '.turbohtml') then
//		c := THtmlPageDocument
//	else if (e = '.htm') or (e = '.html') then
//		c := TRawDocument;
{
	else if (e = '.turboprj') then
	begin
		OpenProject(inPath);
		Result := nil;
	end
}
//	if FileExists(inPath) then
//	begin
//		RecentPages.Add(inPath);
//		RecentFiles.AddSplit(inPath, apTop);
	//
	if c = nil then
		Result := nil
	else
		Result := c.Create(inManager, inPath);
	//
end;

procedure TMainForm.AfterOpenDocument(inSender: TObject;
	inDocument: TLrDocument);
begin
	if (inDocument is TPageDocument) then
		RecentPages.Add(inDocument.Path)
	else
		RecentFiles.Add(inDocument.Path);
end;

procedure TMainForm.SaveActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := DocMgr.EnableSave;
end;

procedure TMainForm.SaveAsActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := DocMgr.EnableSaveAs;
end;

procedure TMainForm.SaveActionExecute(Sender: TObject);
begin
	DocMgr.Save;
end;

procedure TMainForm.SaveAsActionExecute(Sender: TObject);
begin
	DocMgr.SaveAs;
	//SourceExplorerForm.ExplorerTree.RefreshTree;
	DocumentProjectSettingsChange;
end;

procedure TMainForm.SaveOrSaveAsActionExecute(Sender: TObject);
begin
	DocMgr.Save;
	//SourceExplorerForm.ExplorerTree.RefreshTree;
end;

procedure TMainForm.SaveAsDocument(inSender: TObject;
	inDocument: TLrDocument; const inOldPath, inNewPath: string);
begin
	Project.Rename(inOldPath, inNewPath);
end;

procedure TMainForm.PageDocumentProjectSettingsChange(inDoc: TPageDocument);
begin
	with inDoc do
	begin
		PublishFolder := Project.GetPublishFolder(Path);
		LibFolder := Project.LibFolder;
		TThCustomPicture.SetPictureFolders(PublishFolder, Project.ProjectFolder);
		TTpPicturePathProperty.SetPaths(PublishFolder,
			Project.GetPublishedImagesFolder(Path));
		ThNotifyAll(DesignForm, THM_UPDATEPICTURE);
	end;
end;

procedure TMainForm.DocumentProjectSettingsChange;
begin
	if PageDocument <> nil then
		PageDocumentProjectSettingsChange(PageDocument);
end;

procedure TMainForm.SelectRawDocument;
begin
	if Document.Controller = nil then
		Document.Controller := TPhpController.Create(PhpDocument);
	PhpView.BringToFront;
end;

procedure TMainForm.SelectPageDocument;
begin
	if Document.Controller = nil then
		Document.Controller := TPageController.Create(PageDocument);
	with TPageController(Document.Controller) do
	begin
		OnSelectionChange := DesignSelectionChange;
		OnPropChange := DesignPropChange;
	end;
	PageView.BringToFront;
end;

procedure TMainForm.CurrentChanged(inSender: TObject);
begin
	UpdateTabs;
	if IsPhpDocument then
		SelectRawDocument
	else if IsPageDocument then
		SelectPageDocument
	else
		NoDocument;
	DocumentProjectSettingsChange;
	Caption := Application.Title + ' - ' + DocMgr.Current.Path;
end;

procedure TMainForm.PublishPhpDocument(inDocument: TPhpDocument);
begin
	//ActivateDock(PreviewDock);
	//PreviewForm.Navigate(DocMgr.Current.Path);
end;

procedure TMainForm.PublishPageDocument(inDocument: TPageDocument);
begin
	with inDocument do
		if not DirectoryExists(Project.PublishFolder) then
			MessageDlg(SNeedProjectFolders, mtError, [ mbOk ], 0)
		else begin
			TPageController(Controller).Publish;
			//PageView.DebugForm.Clear;
			//Publish;
			//PageView.PhpEditForm.Source.Strings.Assign(Page.PhpSource);
			if Page.PreviewOnPublish and (Project.Url <> '') then
			begin
				ActivateDock(PageView.PreviewDock);
				PageView.PreviewForm.ForceNavigate(
					Project.GetPublishUrl(Path) + PhpFilename);
			end;
			PublishExplorerForm.ExplorerTree.RefreshTree;
		end;
end;

procedure TMainForm.PublishActionExecute(Sender: TObject);
begin
	if IsPhpDocument then
		PublishPhpDocument(PhpDocument)
	else if IsPageDocument then
		PublishPageDocument(PageDocument);
end;

procedure TMainForm.FixFloatForm(inForm: TCustomForm);
begin
	if inForm <> nil then
	begin
		inForm.BorderStyle := bsSizeable;
		TForm(inForm).FormStyle := fsStayOnTop;
	end;
end;

procedure TMainForm.FixFloatForms;
var
	i: Integer;
begin
	with dxDockingController do
		for i := 0 to Pred(DockControlCount) do
			if DockControls[i] is TdxDockPanel then
				FixFloatForm(TdxDockPanel(DockControls[i]).FloatForm);
end;

procedure TMainForm.LoadLayout(const inFilename: string);
begin
	if FileExists(inFilename) then
	try
		Screen.Cursor := crHourglass;
		//PropStore.LoadFromIniFile(inFilename, PropStore.IniSection);
		dxDockingController.LoadLayoutFromIniFile(inFilename);
		FixFloatForms;
		ReactivateDesigner;
	finally
		Screen.Cursor := crDefault;
	end;
end;

procedure TMainForm.SaveLayout(const inFilename: string);
begin
	dxDockingController.SaveLayoutToIniFile(inFilename);
	//PropStore.SaveToIniFile(inFilename, PropStore.IniSection);
end;

procedure TMainForm.SaveLayoutActionExecute(Sender: TObject);
begin
	if SaveLayoutDialog.Execute then
		SaveLayout(SaveLayoutDialog.FileName);
end;

procedure TMainForm.LoadLayoutActionExecute(Sender: TObject);
begin
	if OpenLayoutDialog.Execute then
	begin
		LoadLayout(OpenLayoutDialog.Filename);
		CurrentChanged(DocMgr);
	end;
end;

function TMainForm.GetActiveDesigner: TDCLiteDesigner;
begin
	if IsPageDocument then
		Result := PageDocument.DesignForm.DCLiteDesigner
	else
		Result := nil;
end;

procedure TMainForm.VerifyPublishFolder;
var
	s: string;
begin
	if not DirectoryExists(Project.PublishFolder) then
	begin
		s := Format(SCreatePublishFolderQuery, [ Project.PublishFolder ]);
		if (MessageDlg(s, mtConfirmation, [ mbOk, mbCancel ], 0) = mrOk) then
		try
			NeedFolder(Project.PublishFolder);
		except
			on E: Exception do
				MessageDlg(E.Message, mtError, [ mbOK ], 0);
		end;
	end;
	if not DirectoryExists(Project.PublishFolder) then
		Project.PublishFolder := '';
end;

procedure TMainForm.CurrentProjectChanged(inSender: TObject);
begin
	if Project <> NilProject then
		with Project do
		begin
			if Project.PublishFolder <> '' then
				VerifyPublishFolder;
			ProjectForm.Project := Project;
			PublishExplorerForm.RootFolder := PublishFolder;
			OpenDialog.InitialDir := ExtractFilePath(Filename);
			DocMgr.DefaultPath := ExtractFilePath(Filename);
			DocumentProjectSettingsChange;
			CopyDefaultProjectItems;
		end;
end;

procedure TMainForm.ProjectChanged(inSender: TObject);
begin
	ProjectForm.BuildTree;
end;

procedure TMainForm.CopyDefaultProjectItems;
var
	s: string;
begin
	if DirectoryExists(Project.PublishFolder) then
	begin
		s := Project.GetSourceImagesFolder('');
		if DirectoryExists(s) then
			with DCFileOps do
			begin
				DestFolder := Project.GetPublishedImagesFolder('');
				if not DirectoryExists(DestFolder) then
				begin
					NeedFolder(DestFolder);
					SourceFiles.Clear;
					SourceFiles.Add(s	+ '*.*');
					DCFileOps.Execute;
				end;
			end;
	end;
end;

procedure TMainForm.ProjectOptionsActionExecute(Sender: TObject);
begin
	with TProjectOptionsForm.Create(Self) do
	try
		Project := Self.Project;
		if ShowModal = mrOk then
			CurrentProjectChanged(nil);
	finally
		Free;
	end;
end;

procedure TMainForm.OpenProjectActionExecute(Sender: TObject);
begin
	with OpenProjectDialog do
		if Execute then
			OpenProject(Filename);
end;

procedure TMainForm.OpenProject(const inFilename: string);
begin
	if DocMgr.CloseAll then
	begin
		ProjectMgr.OpenProject(inFileName);
		if FileExists(inFilename) then
			RecentProjects.Add(inFileName);
	end;
end;

procedure TMainForm.ActionRequiringProjectUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := ProjectMgr.ProjectIsOpen;
end;

procedure TMainForm.NewProjectActionExecute(Sender: TObject);
begin
	if ProjectMgr.CloseProject then
	begin
		ProjectMgr.CurrentProject := TProject.Create(nil);
		Project.Filename := ExtractFilePath(Application.ExeName)
			+ 'Untitled.tpprj';
	end;
end;

procedure TMainForm.SaveProjectActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := ProjectMgr.CurrentProject.Modified;
end;

procedure TMainForm.SaveProjectActionExecute(Sender: TObject);
begin
	ProjectMgr.SaveProject;
end;

procedure TMainForm.SaveProjectAsActionExecute(Sender: TObject);
begin
	ProjectMgr.SaveProjectAs;
end;

procedure TMainForm.AddNewPageActionExecute(Sender: TObject);
begin
	NewAction.Execute;
end;

procedure TMainForm.AddExistingPageActionExecute(Sender: TObject);
begin
	Project.AddExistingPage(nil);
end;

procedure TMainForm.ExplorerTreeDblClick(Sender: TObject);
begin
	DocMgr.Open(TVirtualExplorerTree(Sender).SelectedPath);
end;

procedure TMainForm.ProjectItemOpen(Sender: TObject);
begin
	DocMgr.Open(Project.ProjectPath(ProjectForm.FocusedItem.Filename));
end;

procedure TMainForm.CloseActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := DocMgr.Current <> DocMgr.NilDocument;
end;

procedure TMainForm.CloseActionExecute(Sender: TObject);
begin
	if TabControl.TabIndex <> 0 then
		if DocMgr.Current.CanClose then
			DocMgr.Current.Close;
end;

procedure TMainForm.CloseAllActionExecute(Sender: TObject);
begin
	DocMgr.CloseAll;
end;

procedure TMainForm.CloseDocument(inSender: TObject);
var
	m: Boolean;
begin
	if DocMgr.Current.Untitled then
	begin
		m := Project.Modified;
		Project.Remove(DocMgr.Current.Path);
		Project.Modified := m;
	end;
end;

procedure TMainForm.ExitActionExecute(Sender: TObject);
begin
	Close;
end;

procedure TMainForm.HelpActionExecute(Sender: TObject);
begin
	ActivateDock(HelpDock);
	InspectorHelp;
end;

procedure TMainForm.InspectorActivePropertyChanged(Sender: TObject);
begin
	InspectorHelp;
end;

procedure TMainForm.InspectorHelp;
var
	handled: Boolean;
begin
	handled := false;
	if PageDocument <> nil then
		with InspectorForm.ObjectInspector do
			if CurrentControl <> nil then
			begin
				if not ComponentHelp(CurrentControl, ActiveProperty) then
					DisplayHelp('TTpObj');
				handled := true;
			end;
	if not handled then
//		DisplayHelp('_TpLib_php');
		DisplayHelp;
end;

	function HelpNames(inInstance: TObject; const inProp: string;
		var outName, outProp: string): string;
	var
		s: TStringList;
		o: TObject;
		i: Integer;
	begin
		s := TStringList.Create;
		try
			s.QuoteChar := #0;
			s.Delimiter := '.';
			s.DelimitedText := inProp;
			o := inInstance;
			for i := 0 to s.Count - 2 do
				o := GetObjectProp(o, s[i]);
			if (o <> nil) and (s.Count > 0) then
			begin
				outName := o.ClassName;
				outProp := s[s.Count - 1];
			end
			else begin
				outName := '';
				outProp := '';
			end;
		finally
			s.Free;
		end;
	end;

function TMainForm.ComponentHelp(inComponent: TPersistent;
	const inProperty: string): Boolean;
var
	n, p: string;
begin
	HelpNames(inComponent, inProperty, n, p);
	Result := PropertyHelp(n, p);
end;

function TMainForm.PropertyHelp(const inName, inProperty: string): Boolean;
begin
	Result := true;
	if not DisplayHelp(inName + '_' + inProperty) then
		if not DisplayHelp(inName) then
			Result := false;
{
	if not DisplayHelp(inName + '_' + inProperty) then
		if not DisplayHelp('Common_' + inProperty) then
			if not DisplayHelp(inName) then
				DisplayHelp('Common');
	Result := true;
}
end;

function TMainForm.DisplayHelp(inFile: string): Boolean;
const
	//cExt = '.htm';
	cExt = '.html';
begin
	if inFile = '' then
		inFile := cDefaultHelpTopic;
	inFile := TpLibDocsFolder + inFile + cExt;
	Result := FileExists(inFile);
	if Result then
		HelpForm.Navigate(inFile);
		//HTMLViewer1.LoadFromFile(inFile)
end;

procedure TMainForm.InstallPackagesActionExecute(Sender: TObject);
begin
	DCShowOnlyPackagesConfig;
end;

procedure TMainForm.NoCloseQuery(Sender: TdxCustomDockControl;
	var CanClose: Boolean);
begin
	CanClose := false;
end;

procedure TMainForm.NeedPageDocActionUpdate(Sender: TObject);
begin
	ObjectInspectorAction.Enabled := PageDocument <> nil;
end;

procedure TMainForm.MadExceptionHandler1Exception(frozen: Boolean;
	exceptObject: TObject; exceptAddr: Pointer; crashedThreadId: Cardinal;
	var bugReport, screenShot: String; var canContinue, handled: Boolean);
begin
	if (exceptObject is EConvertError) or (exceptObject is EPropertyError) then
		begin
			handled := true;
			canContinue := true;
		end
	//else if (exceptObject is EInOutError) or (exceptObject is EFOpenError) then
	else if (exceptObject is EStreamError) then
		begin
			MessageDlg(EStreamError(exceptObject).Message, mtError,
				[ mbOK, mbHelp ], 0);
			handled := true;
			canContinue := true;
		end;
end;

procedure TMainForm.CMLazyFocus(var inMsg: TMessage);
begin
	FocusControl(TWinControl(inMsg.WParam));
end;

procedure TMainForm.DesignerCopyActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := (ActiveDesigner <> nil)
		and ActiveDesigner.CanCopy;
end;

procedure TMainForm.DesignerCutActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := (ActiveDesigner <> nil)
		and ActiveDesigner.CanCut;
end;

procedure TMainForm.DesignerPasteActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := (ActiveDesigner <> nil)
		and ActiveDesigner.CanPaste;
end;

procedure TMainForm.DesignerDeleteActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := (ActiveDesigner <> nil)
		and ActiveDesigner.CanCopy;
end;

procedure TMainForm.DesignerCopyActionExecute(Sender: TObject);
begin
	ActiveDesigner.Designer.ClipboardCopy;
end;

procedure TMainForm.DesignerCutActionExecute(Sender: TObject);
begin
	ActiveDesigner.Designer.ClipboardCut;
end;

procedure TMainForm.DesignerPasteActionExecute(Sender: TObject);
begin
	ActiveDesigner.Designer.ClipboardPaste;
end;

procedure TMainForm.DesignerDeleteActionExecute(Sender: TObject);
begin
	ActiveDesigner.Designer.DeleteSelectedComponents;
end;

procedure TMainForm.DesignerUndoActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := (ActiveDesigner <> nil)
		and ActiveDesigner.Designer.UndoAvailable;
end;

procedure TMainForm.DesignerUndoActionExecute(Sender: TObject);
begin
	ActiveDesigner.Designer.Undo;
end;

procedure TMainForm.DesignerRedoActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := (ActiveDesigner <> nil)
		and ActiveDesigner.Designer.RedoAvailable;
end;

procedure TMainForm.DesignerRedoActionExecute(Sender: TObject);
begin
	ActiveDesigner.Designer.Redo;
end;

function TMainForm.IsDesignerEditTarget: Boolean;
begin
	Result := IsPageDocument and (ActiveControl = PageDocument.DesignForm)
	 //	or (ActiveControl = PageView.ComponentListForm.TreeView)
		or (ActiveControl = PageView.ControlTreeForm.TreeView);
end;

procedure TMainForm.CutCopyDeleteActionUpdate(Sender: TObject);
begin
	if ActiveControl is TEasyEdit then
		TAction(Sender).Enabled := TEasyEdit(ActiveControl).CanCopy
	else if ActiveControl is TCustomEdit then
		TAction(Sender).Enabled := TCustomEdit(Screen.ActiveControl).SelLength > 0
	else if IsDesignerEditTarget then
		DesignerCopyActionUpdate(Sender)
	else
		TAction(Sender).Enabled := false;
end;

procedure TMainForm.CopyActionExecute(Sender: TObject);
begin
	if ActiveControl is TEasyEdit then
		TEasyEdit(Screen.ActiveControl).CopyBlock
	else if ActiveControl is TCustomEdit then
		TCustomEdit(Screen.ActiveControl).CopyToClipboard
	else if IsDesignerEditTarget then
		DesignerCopyAction.Execute;
end;

procedure TMainForm.CutActionExecute(Sender: TObject);
begin
	if ActiveControl is TEasyEdit then
		TEasyEdit(Screen.ActiveControl).CutBlock
	else if ActiveControl is TCustomEdit then
		TCustomEdit(Screen.ActiveControl).CutToClipboard
	else if IsDesignerEditTarget then
		DesignerCutAction.Execute;
end;

procedure TMainForm.DeleteActionExecute(Sender: TObject);
begin
	if ActiveControl is TEasyEdit then
		TEasyEdit(Screen.ActiveControl).DeleteBlock
	else if ActiveControl is TCustomEdit then
		TCustomEdit(Screen.ActiveControl).SelText := ''
	else if IsDesignerEditTarget then
		DesignerDeleteAction.Execute;
end;

procedure TMainForm.PasteActionUpdate(Sender: TObject);
begin
	if ActiveControl is TEasyEdit then
		TAction(Sender).Enabled := Clipboard.HasFormat(CF_TEXT)
	else if ActiveControl is TCustomEdit then
		TAction(Sender).Enabled := Clipboard.HasFormat(CF_TEXT)
	else if IsDesignerEditTarget then
		TAction(Sender).Enabled := Clipboard.HasFormat(CF_TEXT)
	else
		TAction(Sender).Enabled := false;
end;

procedure TMainForm.PasteActionExecute(Sender: TObject);
begin
	if ActiveControl is TEasyEdit then
		TEasyEdit(Screen.ActiveControl).PasteBlock
	else if ActiveControl is TCustomEdit then
		TCustomEdit(Screen.ActiveControl).PasteFromClipboard
	else if IsDesignerEditTarget then
		DesignerPasteAction.Execute;
end;

procedure TMainForm.SelectAllActionUpdate(Sender: TObject);
begin
	if ActiveControl is TEasyEdit then
		TAction(Sender).Enabled := true
	else if ActiveControl is TCustomEdit then
		TAction(Sender).Enabled := true
	else
		TAction(Sender).Enabled := false;
end;

procedure TMainForm.SelectAllActionExecute(Sender: TObject);
begin
	if ActiveControl is TEasyEdit then
		TEasyEdit(Screen.ActiveControl).SelectAll
	else if ActiveControl is TCustomEdit then
		TCustomEdit(Screen.ActiveControl).SelectAll;
end;

procedure TMainForm.UndoActionUpdate(Sender: TObject);
begin
	if ActiveControl is TEasyEdit then
		TAction(Sender).Enabled := TEasyEdit(ActiveControl).CanUndo
	else if ActiveControl is TCustomEdit then
		TAction(Sender).Enabled := TCustomEdit(Screen.ActiveControl).CanUndo
	else if IsDesignerEditTarget then
		DesignerUndoActionUpdate(Sender)
	else
		TAction(Sender).Enabled := false;
end;

procedure TMainForm.UndoActionExecute(Sender: TObject);
begin
	if ActiveControl is TEasyEdit then
		TEasyEdit(ActiveControl).Undo
	else if ActiveControl is TCustomEdit then
		TCustomEdit(Screen.ActiveControl).Undo
	else if IsDesignerEditTarget then
		DesignerUndoAction.Execute;
end;

procedure TMainForm.RedoActionUpdate(Sender: TObject);
begin
	if ActiveControl is TEasyEdit then
		TAction(Sender).Enabled :=
			TEasyEdit(ActiveControl).EditSource.RedoAvailable
	else if IsDesignerEditTarget then
		DesignerRedoActionUpdate(Sender)
	else
		TAction(Sender).Enabled := false;
end;

procedure TMainForm.RedoActionExecute(Sender: TObject);
begin
	if ActiveControl is TEasyEdit then
		TEasyEdit(ActiveControl).EditSource.Redo
	else if IsDesignerEditTarget then
		DesignerRedoAction.Execute;
end;

procedure TMainForm.EasyEditActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := ActiveControl is TEasyEdit;
end;

procedure TMainForm.FindActionExecute(Sender: TObject);
begin
	if ActiveControl is TEasyEdit then
		TEasyEdit(ActiveControl).ExecuteDialog(cFind);
end;

procedure TMainForm.FindAgainActionExecute(Sender: TObject);
begin
	if ActiveControl is TEasyEdit then
		TEasyEdit(ActiveControl).FindNext(cFind);
end;

procedure TMainForm.ReplaceActionExecute(Sender: TObject);
begin
	if ActiveControl is TEasyEdit then
		TEasyEdit(ActiveControl).ExecuteDialog(cReplace);
end;

procedure TMainForm.DockablePopupPopup(Sender: TObject);
begin
	if not (TPopupMenu(Sender).PopupComponent is TDxDockPanel) then
		DockableItem.Enabled := false
	else
		DockableItem.Checked :=
			not (TDxDockPanel(TPopupMenu(Sender).PopupComponent).AllowDock = []);
end;

procedure TMainForm.DockableItemClick(Sender: TObject);
begin
	if DockablePopup.PopupComponent is TDxDockPanel then
		with TDxDockPanel(DockablePopup.PopupComponent) do
			if AllowDock = [] then
				AllowDock := [ dtClient, dtLeft, dtTop, dtRight, dtBottom ]
			else
				AllowDock := [];
end;

procedure TMainForm.AboutActionExecute(Sender: TObject);
begin
	TAboutForm.Create(Self).ShowModal;
end;

procedure TMainForm.DesignSelectionChange(Sender: TObject);
begin
	StyleFontControlsUpdate;
end;

procedure TMainForm.DesignPropChange(Sender: TObject);
begin
	StyleFontControlsUpdate;
end;

procedure TMainForm.StyleFontControlsUpdate;
begin
	StyleBarForm.UpdateStyle;
{
	with InspectorForm.ObjectInspector do
		if (CurrentControl is TComponent) then
			StyleBarForm.Component := TComponent(CurrentControl)
		else
			StyleBarForm.Component := nil;
}
end;

end.
