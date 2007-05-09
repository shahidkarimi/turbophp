unit Documents;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs,
	aqDockingBase, aqDocking, aqDockingUtils, 
	JvTabBar,
	Desktop, aqDockingUI;

type
	TDocumentsForm = class(TForm)
		aqDockingSite1: TaqDockingSite;
		aqDockingManager1: TaqDockingManager;
		DesignDock: TaqDockingControl;
		PhpDock: TaqDockingControl;
		DocumentTabs: TJvTabBar;
    PaletteDock: TaqDockingControl;
    InspectorDock: TaqDockingControl;
    aqStyleManager1: TaqStyleManager;
		procedure FormCreate(Sender: TObject);
    procedure DocumentTabsTabClosing(Sender: TObject; Item: TJvTabBarItem;
      var AllowClose: Boolean);
    procedure DocumentTabsTabSelected(Sender: TObject;
      Item: TJvTabBarItem);
	private
		{ Private declarations }
		procedure CurrentChanged(Sender: TObject);
		procedure DocumentsChanged(Sender: TObject);
	public
		{ Public declarations }
		procedure UpdateDocumentTabs;
	end;

var
	DocumentsForm: TDocumentsForm;
	Desktop: TDesktop;

implementation

uses
	LrUtils, Palette, Inspector, DesignHost, PhpEdit, Design, TurboPhpDocument;

{$R *.dfm}

procedure TDocumentsForm.FormCreate(Sender: TObject);
begin
	AddForm(PaletteForm, TPaletteForm, PaletteDock);
	AddForm(InspectorForm, TInspectorForm, InspectorDock);
	AddForm(DesignHostForm, TDesignHostForm, DesignDock);
	AddForm(PhpEditForm, TPhpEditForm, PhpDock);
	Desktop := TDesktop.Create;
	Desktop.OnDocumentsChanged := DocumentsChanged;
	Desktop.OnCurrentChanged := CurrentChanged;
	//Desktop.AddDocument(Document);
end;

procedure TDocumentsForm.DocumentsChanged(Sender: TObject);
begin
	DocumentsForm.UpdateDocumentTabs;
end;

procedure TDocumentsForm.UpdateDocumentTabs;
var
	i: Integer;
begin
	DocumentTabs.OnTabSelected := nil;
	DocumentTabs.Tabs.BeginUpdate;
	try
		DocumentTabs.Tabs.Clear;
		for i := 0 to Pred(Desktop.Count) do
			DocumentTabs.AddTab(Desktop.Documents[i].DisplayName);
	finally
		DocumentTabs.Tabs.EndUpdate;
		DocumentTabs.OnTabSelected := DocumentTabsTabSelected;
	end;
end;

procedure TDocumentsForm.CurrentChanged(Sender: TObject);
begin
	if Desktop.Index >= 0 then
	begin
		try
			DocumentTabs.OnTabSelected := nil;
			DocumentTabs.SelectedTab := DocumentTabs.Tabs[Desktop.Index];
		finally
			DocumentTabs.OnTabSelected := DocumentTabsTabSelected;
		end;
		with TTurboPhpDocument(Desktop.Current) do
		begin
			DesignHostForm.DesignForm := DesignForm;
			DesignForm.Parent := DesignHostForm.Scroller;
			DesignForm.BringToFront;
			DesignHostForm.ActivateDesign;
		end;
	end;
end;

procedure TDocumentsForm.DocumentTabsTabClosing(Sender: TObject;
	Item: TJvTabBarItem; var AllowClose: Boolean);
begin
	AllowClose := false;
end;

procedure TDocumentsForm.DocumentTabsTabSelected(Sender: TObject;
	Item: TJvTabBarItem);
begin
	Desktop.Index := Item.Index;
end;

end.
