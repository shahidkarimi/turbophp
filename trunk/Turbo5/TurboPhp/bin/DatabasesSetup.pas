unit DatabasesSetup;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls, ComCtrls, ImgList,
	LMDCustomComponent, LMDBrowseDlg,
	htDatabases;

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
		Database: ThtDatabaseItem;
		FDatabases: ThtDatabasesItem;
		Item: TListItem;
	protected
		procedure SelectDatabase(inIndex: Integer);
		procedure SetDatabases(const Value: ThtDatabasesItem);
		procedure UpdateDatabaseList;
	public
		{ Public declarations }
		property Databases: ThtDatabasesItem read FDatabases write SetDatabases;
	end;

var
	DatabasesSetupForm: TDatabasesSetupForm;

implementation

uses
	DatabaseSetup;

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
			for i := 0 to Pred(Databases.Count) do
				with Items.Add, Databases[i] do
					Caption := DisplayName;
			if Items.Count > s then
				Selected := Items[s]
			else if Items.Count > 0 then
				Selected := Items[0];
		finally
			Items.EndUpdate;
		end;
		PropertiesButton.Enabled := (Items.Count > 0);
	end;
end;

procedure TDatabasesSetupForm.NewButtonClick(Sender: TObject);
begin
	Database := ThtDatabaseItem.Create;
	Database.DisplayName := 'New Database';
	Databases.Add(Database);
	UpdateDatabaseList;
	with DatabaseList do
		Selected := Items[Pred(Items.Count)];
end;

procedure TDatabasesSetupForm.DeleteButtonClick(Sender: TObject);
begin
	if MessageDlg('Delete database "' + Database.DisplayName + '"?',
		mtConfirmation, mbYesNoCancel, 0) = mrYes then
	begin
		FreeAndNil(Database);
		UpdateDatabaseList;
	end;
end;

procedure TDatabasesSetupForm.SelectDatabase(inIndex: Integer);
begin
	Database := Databases[inIndex];
	Item := DatabaseList.Items[inIndex];
end;

procedure TDatabasesSetupForm.DatabaseListEdited(Sender: TObject;
	Item: TListItem; var S: String);
begin
	Database.DisplayName := S;
end;

procedure TDatabasesSetupForm.PropertiesButtonClick(Sender: TObject);
begin
	with TDatabaseSetupForm.Create(nil) do
	try
		Database := Self.Database;
		ShowModal;
	finally
		Free;
	end;
	UpdateDatabaseList;
end;

procedure TDatabasesSetupForm.SetDatabases(const Value: ThtDatabasesItem);
begin
	FDatabases := Value;
	UpdateDatabaseList;
end;

end.
