unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ImgList, ComCtrls, ToolWin, ExtCtrls,
	TB2Dock, TB2Toolbar, TB2Item, TB2ExtItems, TBX, TBXExtItems, TBXSwitcher, 
	dxDockControl, dxDockPanel,
	DesignManager, Controller;

const
	CM_APPSTART = WM_USER + $01;

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
		TBXItem5: TTBXItem;
		dxDockingManager1: TdxDockingManager;
		dxDockSite1: TdxDockSite;
		PaletteDock: TdxDockPanel;
		dxLayoutDockSite1: TdxLayoutDockSite;
		InspectorDock: TdxDockPanel;
		dxVertContainerDockSite1: TdxVertContainerDockSite;
		ProjectDock: TdxDockPanel;
		dxLayoutDockSite3: TdxLayoutDockSite;
		DocumentsDock: TdxDockPanel;
		dxHorizContainerDockSite1: TdxHorizContainerDockSite;
    DockStyleComboItem: TTBXComboBoxItem;
		TBXSeparatorItem3: TTBXSeparatorItem;
    TBXItem6: TTBXItem;
    TBXSeparatorItem4: TTBXSeparatorItem;
    TBXItem7: TTBXItem;
		procedure FormCreate(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure DockStyleComboItemChange(Sender: TObject; const Text: String);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
	private
		{ Private declarations }
		procedure CMAppStart(var Message: TMessage); message CM_APPSTART;
		procedure CreateStartPage;
    procedure LoadDockConfig;
		procedure SaveDockConfig;
	public
		{ Public declarations }
	end;

var
	DesignManager: TDesignManager;
	MainForm: TMainForm;

implementation

uses
	LrUtils, DockingUtils, Globals, Registration,
	Documents, Palette, Inspector,
	TurboDocumentHost,	LiteBrowserDocument;

const
	cDockConfig = 'maindock.cfg';

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
	Registration.Register;
	//
	AddForm(PaletteForm, TPaletteForm, PaletteDock);
	AddForm(InspectorForm, TInspectorForm, InspectorDock);
	AddForm(DocumentsForm, TDocumentsForm, DocumentsDock);
	//
	DesignManager := TDesignManager.Create(Self);
	DesignManager.Inspector := InspectorForm;
	//
	PostMessage(Handle, CM_APPSTART, 0, 0);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	CanClose := Desktop.CloseAll;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	SaveDockConfig;
	Desktop.Current.Deactivate;
end;

procedure TMainForm.CMAppStart(var Message: TMessage);
begin
	LoadDockConfig;
	DocumentsForm.DocumentPanel.Visible := false;
	CreateStartPage;
end;

procedure TMainForm.LoadDockConfig;
begin
	LoadDockLayout(dxDockingManager1, ConfigHome + cDockConfig);
	TurboDocumentHostForm.LoadDockConfig;
end;

procedure TMainForm.SaveDockConfig;
begin
	SaveDockLayout(dxDockingManager1, ConfigHome + cDockConfig);
	TurboDocumentHostForm.SaveDockConfig;
end;

procedure TMainForm.CreateStartPage;
var
	d: TLiteBrowserDocument;
begin
	d := TLiteBrowserDocument.Create;
	d.Filename := 'Start';
	d.Html.Add('Welcome to TurboPhp!');
	Desktop.AddDocument(d);
end;

procedure TMainForm.DockStyleComboItemChange(Sender: TObject;
	const Text: String);
begin
	with DockStyleComboItem do
		if (ItemIndex >= 0) then
		begin
			dxDockingManager1.ViewStyle := TdxDockingViewStyle(ItemIndex);
			TurboDocumentHostForm.dxDockingManager1.ViewStyle
				:= dxDockingManager1.ViewStyle;
		end;
end;

end.
