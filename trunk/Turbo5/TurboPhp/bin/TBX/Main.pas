unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ImgList, ExtCtrls,
	JvTabBar,
	TB2Dock, TBXDkPanels, TBXSwitcher, ActnList, TB2Item, TBX, TB2Toolbar,
	DesignManager, Generator;

type
	TMainForm = class(TForm)
    TBXMultiDock1: TTBXMultiDock;
    InspectorPanel: TTBXDockablePanel;
    InspectorScroll: TTBXPageScroller;
    PalettePanel: TTBXDockablePanel;
    ContentPanel: TPanel;
    JvTabBar1: TJvTabBar;
    TBXSwitcher1: TTBXSwitcher;
    TBXDock1: TTBXDock;
    TBXToolbar1: TTBXToolbar;
    TBXItem1: TTBXItem;
    ActionList1: TActionList;
    GenerateAction: TAction;
    TBXDock2: TTBXDock;
		procedure FormCreate(Sender: TObject);
    procedure GenerateActionExecute(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
		DesignManager: TDesignManager;
		Generator: TGenerator;
		Home: string;
	end;

var
	MainForm: TMainForm;

implementation

uses
	TbxStripesTheme, LrUtils, Registration, DatabaseSetup, Inspector, Palette,
	DesignHost;

const
	cDockingFile = 'docking.cfg';

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
	Registration.Register;
	//
	Home := ExtractFilePath(Application.ExeName);
	//
	AddForm(PaletteForm, TPaletteForm, PalettePanel);
	AddForm(InspectorForm, TInspectorForm, InspectorScroll);
	//
	AddForm(DatabaseSetupForm, TDatabaseSetupForm, ContentPanel);
	AddForm(DesignHostForm, TDesignHostForm, ContentPanel);
	//
	DesignManager := TDesignManager.Create(Self);
	DesignManager.Design := DesignHostForm.DesignForm;
	DesignManager.Inspector := InspectorForm;
	//
	Generator := TGenerator.Create;
	//
	Width := Width + 1;
end;

procedure TMainForm.GenerateActionExecute(Sender: TObject);
begin
	Generator.Content := DesignHostForm.DesignForm;
	Generator.Generate;
	Generator.Preview;
end;

end.
