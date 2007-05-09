unit Documents;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls,
	JvTabBar,
	Desktop;

const
	CM_CLOSE = WM_APP + $02;

type
	TDocumentsForm = class(TForm)
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
		procedure CMClose(var Message: TMessage); message CM_Close;
		procedure CurrentChanged(Sender: TObject);
		procedure DocumentChanged(Sender: TObject);
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
	LrUtils, TurboDocumentHost, LiteBrowser, PhpEdit, ImageView;

{$R *.dfm}

procedure TDocumentsForm.FormCreate(Sender: TObject);
begin
	AddForm(TurboDocumentHostForm, TTurboDocumentHostForm, DocumentPanel);
	AddForm(LiteBrowserForm, TLiteBrowserForm, DocumentPanel);
	AddForm(PhpEditForm, TPhpEditForm, DocumentPanel);
	AddForm(ImageViewForm, TImageViewForm, DocumentPanel);
	//
	Desktop := TDesktop.Create;
	Desktop.OnDocumentsChanged := DocumentsChanged;
	Desktop.OnCurrentChanged := CurrentChanged;
	Desktop.AddDocumentObserver(DocumentChanged);
end;

procedure TDocumentsForm.UpdateDocumentTabs;
var
	i: Integer;
begin
	DocumentPanel.Visible := Desktop.Count > 0;
	with DocumentTabs do
	try
		OnTabSelected := nil;
		Tabs.BeginUpdate;
		Tabs.Clear;
		for i := 0 to Pred(Desktop.Count) do
			with Desktop.Documents[i] do
				AddTab(DisplayName).Modified := Modified;
		if (Desktop.Index >= 0) and (Desktop.Index < Tabs.Count) then
			SelectedTab := Tabs[Desktop.Index];
		//SelectedTab := nil;
	finally
		Tabs.EndUpdate;
		OnTabSelected := DocumentTabsTabSelected;
	end;
end;

procedure TDocumentsForm.DocumentsChanged(Sender: TObject);
begin
	CurrentChanged(Sender);
	//UpdateDocumentTabs;
end;

procedure TDocumentsForm.CurrentChanged(Sender: TObject);

	procedure SilentTabSelect(inIndex: Integer);
	begin
		with DocumentTabs do
		try
			OnTabSelected := nil;
			SelectedTab := Tabs[inIndex];
		finally
			OnTabSelected := DocumentTabsTabSelected;
		end;
	end;

begin
	UpdateDocumentTabs;
	if Desktop.Index >= 0 then
		SilentTabSelect(Desktop.Index);
end;

procedure TDocumentsForm.DocumentChanged(Sender: TObject);
begin
	UpdateDocumentTabs;
end;

procedure TDocumentsForm.DocumentTabsTabSelected(Sender: TObject;
	Item: TJvTabBarItem);
begin
	Desktop.Index := Item.Index;
end;

procedure TDocumentsForm.DocumentTabsTabClosing(Sender: TObject;
	Item: TJvTabBarItem; var AllowClose: Boolean);
begin
	AllowClose := true;
end;

procedure TDocumentsForm.DocumentTabsTabClosed(Sender: TObject;
	Item: TJvTabBarItem);
begin
	if Desktop.StartCloseCurrent then
		PostMessage(Handle, CM_CLOSE, 0, 0);
end;

procedure TDocumentsForm.CMClose(var Message: TMessage);
begin
	Desktop.FinishCloseCurrent;
end;

end.
