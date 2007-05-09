unit DatabasesSetup;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls, ComCtrls, ImgList,
	LMDCustomComponent, LMDBrowseDlg,
	ServerDatabases;

type
	TDatabasesSetupForm = class(TForm)
		Panel3: TPanel;
		Button1: TButton;
		NewButton: TButton;
		DeleteButton: TButton;
		ImageList1: TImageList;
		DatabaseList: TListView;
		PropertiesButton: TButton;
		procedure NewButtonClick(Sender: TObject);
		procedure DatabaseListSelectItem(Sender: TObject; Item: TListItem;
			Selected: Boolean);
		procedure FormCreate(Sender: TObject);
		procedure DatabaseListEdited(Sender: TObject; Item: TListItem;
			var S: String);
		procedure DeleteButtonClick(Sender: TObject);
    procedure PropertiesButtonClick(Sender: TObject);
	private
		{ Private declarations }
		FDatabaseMgr: TServerDatabaseProfileMgr;
	protected
		procedure SelectDatabase(inIndex: Integer);
		procedure UpdateDatabaseList;
		procedure SetDatabaseMgr(const Value: TServerDatabaseProfileMgr);
	public
		{ Public declarations }
		property DatabaseMgr: TServerDatabaseProfileMgr read FDatabaseMgr
			write SetDatabaseMgr;
	end;

var
	DatabasesSetupForm: TDatabasesSetupForm;

implementation

uses
	DatabaseSetup;

var
	Item: TListItem;
	Database: TServerDatabaseProfile;

{$R *.dfm}

procedure TDatabasesSetupForm.FormCreate(Sender: TObject);
begin
	//
end;

procedure TDatabasesSetupForm.DatabaseListSelectItem(Sender: TObject;
	Item: TListItem; Selected: Boolean);
begin
	if Selected then
		SelectDatabase(Item.Index);
end;

procedure TDatabasesSetupForm.UpdateDatabaseList;
var
	i, s: Integer;
begin
	with DatabaseList do
	begin
		Items.BeginUpdate;
		try
			if Selected <> nil then
				s := Selected.Index
			else
				s := 0;
			Clear;
			for i := 0 to Pred(DatabaseMgr.Profiles.Count) do
				with DatabaseMgr.Databases[i], Items.Add do
				begin
					Caption := Name;
					//ImageIndex := Ord(Target);
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
		PropertiesButton.Enabled := (Items.Count > 0);
	end;
end;

procedure TDatabasesSetupForm.NewButtonClick(Sender: TObject);
begin
	DatabaseMgr.Profiles.Add;
	UpdateDatabaseList;
	with DatabaseList do
		Selected := Items[Pred(Items.Count)];
end;

procedure TDatabasesSetupForm.DeleteButtonClick(Sender: TObject);
begin
	if MessageDlg('Delete server "' + Database.Name + '"?',
		mtConfirmation, mbYesNoCancel, 0) = mrYes then
	begin
		DatabaseMgr.Profiles.Delete(Database.Index);
		UpdateDatabaseList;
	end;
end;

procedure TDatabasesSetupForm.SelectDatabase(inIndex: Integer);
begin
	Database := DatabaseMgr.Databases[inIndex];
	Item := DatabaseList.Items[inIndex];
end;

procedure TDatabasesSetupForm.DatabaseListEdited(Sender: TObject;
	Item: TListItem; var S: String);
begin
	Database.Name := S;
end;

procedure TDatabasesSetupForm.SetDatabaseMgr(const
	Value: TServerDatabaseProfileMgr);
begin
	FDatabaseMgr := Value;
	UpdateDatabaseList;
end;

procedure TDatabasesSetupForm.PropertiesButtonClick(Sender: TObject);
begin
	with TDatabaseSetupForm.Create(nil) do
	try
		DatabaseMgr := Database.Databases;
		ShowModal;
	finally
		Free;
	end;
	UpdateDatabaseList;
end;

end.
