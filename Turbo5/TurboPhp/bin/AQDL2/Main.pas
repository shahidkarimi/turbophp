unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ImgList, ComCtrls, ToolWin, ExtCtrls,
	aqDockingUtils, aqDockingBase, aqDocking, aqDockingUI,
	TBXSwitcher, TB2Item, TBX, TB2Dock, TB2Toolbar,
	DesignManager;

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
		procedure FormCreate(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
	DesignManager: TDesignManager;
	Home: string;
	MainForm: TMainForm;

implementation

uses
	LrUtils, Registration, Documents;

const
	cDockingFile = 'docking.cfg';

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
	Registration.Register;
	//
	Home := ExtractFilePath(Application.ExeName);
	//
	AddForm(DocumentsForm, TDocumentsForm, Self);
	if FileExists(Home + cDockingFile) then
		DocumentsForm.aqDockingManager1.LoadFromFile(Home + cDockingFile);
	//
	DesignManager := TDesignManager.Create(Self);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	DocumentsForm.aqDockingManager1.SaveToFile(Home + cDockingFile);
end;

end.
