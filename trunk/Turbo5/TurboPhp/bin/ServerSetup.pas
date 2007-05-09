unit ServerSetup;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls, ComCtrls, ImgList,
	LMDCustomComponent, LMDBrowseDlg,
	Servers;

type
  TServerSetupForm = class(TForm)
		ServerList: TListView;
		Panel3: TPanel;
		Button1: TButton;
		NewButton: TButton;
    DeleteButton: TButton;
		ImageList1: TImageList;
    PropertiesPanel: TPanel;
    ServerPanel: TPanel;
    Label4: TLabel;
    HostLabel: TLabel;
    Label1: TLabel;
    Label10: TLabel;
    NameEdit: TEdit;
		HostEdit: TEdit;
		TargetCombo: TComboBox;
		Bevel1: TBevel;
		FtpPanel: TPanel;
		Label6: TLabel;
		Label7: TLabel;
		Label2: TLabel;
		Label3: TLabel;
		FTPUserEdit: TEdit;
		FTPPasswordEdit: TEdit;
		FTPHostEdit: TEdit;
		FtpRootEdit: TEdit;
		DiskPanel: TPanel;
		Label11: TLabel;
		RootEdit: TEdit;
		BrowseButton: TButton;
		Bevel3: TBevel;
		RootBrowseDialog: TLMDBrowseDlg;
		procedure NewButtonClick(Sender: TObject);
		procedure EditChange(Sender: TObject);
		procedure ServerListSelectItem(Sender: TObject; Item: TListItem;
			Selected: Boolean);
		procedure ServerListEdited(Sender: TObject; Item: TListItem;
			var S: String);
		procedure DeleteButtonClick(Sender: TObject);
		procedure BrowseButtonClick(Sender: TObject);
	private
		{ Private declarations }
		FUpdateLock: Boolean;
		FServers: TServersItem;
		function GetUpdateLock: Boolean;
		procedure BeginUpdate;
		procedure EndUpdate;
		procedure SelectServer(inIndex: Integer);
		procedure SetServers(const Value: TServersItem);
		procedure UpdateEdits;
		procedure UpdateItem;
		procedure UpdateServerList;
		procedure UpdateTarget;
		property UpdateLock: Boolean read GetUpdateLock write FUpdateLock;
	public
		{ Public declarations }
		property Servers: TServersItem read FServers write SetServers;
	end;

var
	ServerSetupForm: TServerSetupForm;

implementation

var
	Item: TListItem;
	Server: TServerItem;

{$R *.dfm}

procedure TServerSetupForm.SetServers(const Value: TServersItem);
begin
	FServers := Value;
	UpdateServerList;
end;

function TServerSetupForm.GetUpdateLock: Boolean;
begin
	Result := FUpdateLock or (csLoading in ComponentState);
end;

procedure TServerSetupForm.BeginUpdate;
begin
	UpdateLock := true;
end;

procedure TServerSetupForm.EndUpdate;
begin
	UpdateLock := false;
end;

procedure TServerSetupForm.ServerListSelectItem(Sender: TObject;
	Item: TListItem; Selected: Boolean);
begin
	if Selected then
		SelectServer(Item.Index);
end;

procedure TServerSetupForm.UpdateServerList;
var
	i, s: Integer;
begin
	with ServerList do
	begin
		Items.BeginUpdate;
		try
			if Selected <> nil then
				s := Selected.Index
			else
				s := 0;
			Clear;
			for i := 0 to Pred(Servers.Count) do
				with Items.Add, Servers[i] do
				begin
					Caption := DisplayName;
					ImageIndex := Ord(Target);
				end;
			if Items.Count > 0 then
			begin
				if Items.Count > s then
					Selected := Items[s]
				else
					Selected := Items[0];
			end;
		finally
			Items.EndUpdate;
		end;
		PropertiesPanel.Visible := (Items.Count > 0);
		PropertiesPanel.Height := 1;
	end;
end;

procedure TServerSetupForm.NewButtonClick(Sender: TObject);
begin
	Server := TServerItem.Create;
	Server.DisplayName := 'New Server';
	Servers.Add(Server);
	UpdateServerList;
	with ServerList do
		Selected := Items[Pred(Items.Count)];
	NameEdit.SetFocus;
	NameEdit.SelectAll;
end;

procedure TServerSetupForm.DeleteButtonClick(Sender: TObject);
begin
	if MessageDlg('Delete server "' + Server.DisplayName + '"?',
		mtConfirmation, mbYesNoCancel, 0) = mrYes then
	begin
		FreeAndNil(Server);
		UpdateServerList;
	end;
end;

procedure TServerSetupForm.SelectServer(inIndex: Integer);
begin
	Server := Servers[inIndex];
	Item := ServerList.Items[inIndex];
	UpdateEdits;
	UpdateTarget;
end;

procedure TServerSetupForm.UpdateEdits;
begin
	BeginUpdate;
	try
		with Server do
		begin
			NameEdit.Text := DisplayName;
			HostEdit.Text := Host;
			TargetCombo.ItemIndex := Ord(Target);
			RootEdit.Text := Root;
			FTPHostEdit.Text := FTPHost;
			FTPUserEdit.Text := FTPUser;
			FTPPasswordEdit.Text := FTPPassword;
		end;
	finally
		EndUpdate;
	end;
end;

procedure TServerSetupForm.UpdateItem;
begin
	Item.Caption := Server.DisplayName;
end;

procedure TServerSetupForm.UpdateTarget;
begin
	with Server do
	begin
		DiskPanel.Visible := (Target = stDisk);
		FtpPanel.Visible := (Target = stFTP);
	end;
end;

procedure TServerSetupForm.ServerListEdited(Sender: TObject;
	Item: TListItem; var S: String);
begin
	BeginUpdate;
	try
		NameEdit.Text := S;
		Server.DisplayName := S;
	finally
		EndUpdate;
	end;
end;

procedure TServerSetupForm.EditChange(Sender: TObject);
begin
	if not UpdateLock then
	begin
		Server.DisplayName := NameEdit.Text;
		Server.Host := HostEdit.Text;
		Server.Target := TServerTarget(TargetCombo.ItemIndex);
		case Server.Target of
			stDisk:
			begin
				Server.Root := RootEdit.Text;
			end;
			stFTP:
			begin
				Server.FTPHost := FTPHostEdit.Text;
				Server.FTPUser := FTPUserEdit.Text;
				Server.FTPPassword := FTPPasswordEdit.Text;
				Server.Root := FTPRootEdit.Text;
			end;
		end;
		UpdateTarget;
		UpdateItem;
	end;
end;

procedure TServerSetupForm.BrowseButtonClick(Sender: TObject);
begin
	RootBrowseDialog.SelectedFolder := RootEdit.Text;
	if RootBrowseDialog.Execute then
		RootEdit.Text := RootBrowseDialog.SelectedFolder;
end;

end.
