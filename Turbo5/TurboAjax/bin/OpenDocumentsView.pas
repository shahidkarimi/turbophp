unit OpenDocumentsView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls,
	JvTabBar,
	LrOpenDocumentsController;

const
	TM_CLOSE = WM_APP + $02;

type
	TOpenDocumentsForm = class(TForm)
		DocumentTabs: TJvTabBar;
		DocumentPanel: TPanel;
		procedure FormCreate(Sender: TObject);
		procedure DocumentTabsTabClosing(Sender: TObject; Item: TJvTabBarItem;
			var AllowClose: Boolean);
		procedure DocumentTabsTabSelected(Sender: TObject;
			Item: TJvTabBarItem);
		procedure DocumentTabsTabClosed(Sender: TObject; Item: TJvTabBarItem);
	private
		{ Private declarations }
		procedure BeginTabsUpdate;
		procedure EndTabsUpdate;
		procedure OpenDocumentsChange(inSender: TObject);
		procedure TMClose(var Message: TMessage); message TM_Close;
		procedure UpdateDocumentTabs;
	public
		{ Public declarations }
	end;

var
	OpenDocumentsForm: TOpenDocumentsForm;

implementation

{$R *.dfm}

procedure TOpenDocumentsForm.FormCreate(Sender: TObject);
begin
	LrOpenDocuments.OnChange := OpenDocumentsChange;
end;

procedure TOpenDocumentsForm.OpenDocumentsChange(inSender: TObject);
begin
	UpdateDocumentTabs;
end;

procedure TOpenDocumentsForm.BeginTabsUpdate;
begin
	DocumentTabs.OnTabSelected := nil;
	DocumentTabs.Tabs.BeginUpdate;
end;

procedure TOpenDocumentsForm.EndTabsUpdate;
begin
	DocumentTabs.Tabs.EndUpdate;
	DocumentTabs.OnTabSelected := DocumentTabsTabSelected;
end;

procedure TOpenDocumentsForm.UpdateDocumentTabs;
var
	i: Integer;
	tab: TJvTabBarItem;
begin
	BeginTabsUpdate;
	with LrOpenDocuments do
	try
		//DocumentPanel.Visible := Count > 0;
		DocumentTabs.Tabs.Clear;
		for i := 0 to Pred(Count) do
			with Documents[i] do
			begin
				tab := DocumentTabs.AddTab(DisplayName);
				tab.Modified := Modified;
				if (SelectedIndex = i) then
					DocumentTabs.SelectedTab := tab;
			end;
	finally
		EndTabsUpdate;
	end;
end;

procedure TOpenDocumentsForm.DocumentTabsTabSelected(Sender: TObject;
	Item: TJvTabBarItem);
begin
	LrOpenDocuments.SelectedIndex := Item.Index;
end;

procedure TOpenDocumentsForm.DocumentTabsTabClosing(Sender: TObject;
	Item: TJvTabBarItem; var AllowClose: Boolean);
begin
	AllowClose := true;
end;

procedure TOpenDocumentsForm.DocumentTabsTabClosed(Sender: TObject;
	Item: TJvTabBarItem);
begin
	if LrOpenDocuments.BeginCloseCurrent then
		PostMessage(Handle, TM_CLOSE, 0, 0);
end;

procedure TOpenDocumentsForm.TMClose(var Message: TMessage);
begin
	LrOpenDocuments.EndCloseCurrent;
end;

end.
