unit DatabaseSetup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls, StdCtrls, DB, ADODB, ComCtrls,
	LrPageControl, htDatabases;

type
	TDatabaseSetupForm = class(TForm)
		Connection: TADOConnection;
		DbsQuery: TADOQuery;
		StatusBar: TStatusBar;
    OpenDialog: TOpenDialog;
    SettingsPanel: TPanel;
    Panel3: TPanel;
    GenerateButton: TButton;
		Bevel7: TBevel;
		Bevel5: TBevel;
    DatabasePanel: TPanel;
    Label3: TLabel;
    DatabaseCombo: TComboBox;
    UserPanel: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    UserEdit: TEdit;
    PasswordEdit: TEdit;
    TypePanel: TPanel;
    Label1: TLabel;
    TypeCombo: TComboBox;
    Bevel6: TBevel;
    SameAsPanel: TPanel;
    CheckBox3: TCheckBox;
    ComboBox1: TComboBox;
    FilePanel: TPanel;
    Label5: TLabel;
    FilenameEdit: TEdit;
    BrowseButton: TButton;
    Bevel2: TBevel;
    Panel6: TPanel;
    Label10: TLabel;
    DesignPanel: TPanel;
    Panel9: TPanel;
		Label11: TLabel;
    DesignODBCPanel: TPanel;
		ODBCLabel: TLabel;
    ConnectionStringEdit: TEdit;
    ConnectionEditButton: TButton;
    CustomODBCBox: TCheckBox;
    TestButton: TButton;
    ADODBPanel: TPanel;
    ADODBLabel: TLabel;
    DNSEdit: TEdit;
    Bevel3: TBevel;
		ODBCPanel: TPanel;
		Label2: TLabel;
		Edit2: TEdit;
		Button1: TButton;
		Bevel4: TBevel;
		ConnectedBox: TCheckBox;
		Button2: TButton;
		OkButton: TButton;
    Panel2: TPanel;
    Label8: TLabel;
    NameEdit: TEdit;
    Bevel8: TBevel;
		procedure FormCreate(Sender: TObject);
		procedure ConnectionEditButtonClick(Sender: TObject);
		procedure TypeComboChange(Sender: TObject);
		procedure BrowseButtonClick(Sender: TObject);
		procedure GenerateButtonClick(Sender: TObject);
		procedure ServerTabsSelect(Sender: TObject);
		procedure CustomODBCBoxClick(Sender: TObject);
		procedure CustomDNSBoxClick(Sender: TObject);
		procedure TestButtonClick(Sender: TObject);
		procedure AuthEditExit(Sender: TObject);
		procedure OkButtonClick(Sender: TObject);
	private
		{ Private declarations }
		ConnectionString: string;
    FDatabase: ThtDatabaseItem;
		function TryConnect: Boolean;
		procedure EditConnectionString;
		procedure TryConnectAccess;
		procedure TryConnectFirebird;
		procedure TryConnectInterbase5;
		procedure TryConnectInterbase6;
		procedure TryConnectMySql;
		procedure TryConnectODBC;
		procedure TryConnectOracle;
		procedure TryConnectSQLServer;
		procedure UpdateConnection;
		procedure UpdateDatabaseList;
		procedure UpdateUI;
    procedure DatabaseToUi;
    procedure SetDatabase(const Value: ThtDatabaseItem);
    procedure UiToDatabase;
	public
		{ Public declarations }
		property Database: ThtDatabaseItem read FDatabase write SetDatabase;
	end;

var
	DatabaseSetupForm: TDatabaseSetupForm;

implementation

uses
	AdoConEd, ShellApi,
	Controller;

const
	cMYSQL = 0;
	cFirebird = 1;
	cInterbase5 = 2;
	cInterbase6 = 3;
	cAccess = 6;
	cODBC = 7;
	cCustom = 8;

{$R *.dfm}

{ TDatabaseSetupForm }

procedure TDatabaseSetupForm.FormCreate(Sender: TObject);
begin
	AutoSize := true;
	UpdateUI;
	SettingsPanel.Height := 0;
	TypeComboChange(Self);
end;

function TDatabaseSetupForm.TryConnect: Boolean;
begin
	StatusBar.SimpleText := '';
	try
		Connection.ConnectionString := ConnectionString;
		Connection.Connected := true;
	except
		on E: Exception do
			StatusBar.SimpleText := E.Message;
	end;
	Result := Connection.Connected;
end;

procedure TDatabaseSetupForm.UpdateDatabaseList;
var
	ii: Integer;
	d: string;
begin
	try
		d := DatabaseCombo.Text;
		ii := 0;
		with DatabaseCombo do
		try
			Items.BeginUpdate;
			Items.Clear;
			if TypeCombo.ItemIndex = 0 then
				if Connection.Connected then
				begin
					DbsQuery.Active := true;
					while DbsQuery.Active and not DbsQuery.Eof do
					begin
						Items.Add(DbsQuery.Fields[0].AsString);
						if Items[Items.Count - 1] = d then
							ii := Items.Count - 1;
						DbsQuery.Next;
					end;
				end;
		finally
			Items.EndUpdate;
			if (ii < 0) or (ii >= Items.Count) then
				ii := 0;
			ItemIndex := ii;
		end;
	except
		on E: Exception do
			StatusBar.SimpleText := E.Message;
	end;
end;

procedure TDatabaseSetupForm.EditConnectionString;
begin
	Connection.Connected := false;
	AdoConEd.EditConnectionString(Connection);
end;

	procedure CatIf(inTest: Boolean; var inString: string; const inSuffix: string);
	begin
		if inTest then
			inString := inString + inSuffix;
	end;

procedure TDatabaseSetupForm.TryConnectMySql;
begin
	ConnectionString := 'DRIVER={MySQL ODBC 3.51 Driver};';
	CatIf(UserEdit.Text <> '', ConnectionString, 'USER=' + UserEdit.Text + ';');
	CatIf(PasswordEdit.Text <> '', ConnectionString,
		'PASSWORD=' + PasswordEdit.Text + ';');
	CatIf(DatabaseCombo.Text <> '', ConnectionString,
		'DATABASE=' + DatabaseCombo.Text + ';');
	if not TryConnect then
	begin
		ConnectionString := 'DRIVER={mySQL};';
		CatIf(UserEdit.Text <> '', ConnectionString, 'Uid=' + UserEdit.Text + ';');
		CatIf(PasswordEdit.Text <> '', ConnectionString,
			'Pwd=' + PasswordEdit.Text + ';');
		CatIf(DatabaseCombo.Text <> '', ConnectionString,
			'DATABASE=' + DatabaseCombo.Text + ';');
		if not TryConnect then
			ConnectionString := '';
	end;
end;

procedure TDatabaseSetupForm.TryConnectAccess;
begin
{
	Connection.ConnectionString :=
		'Provider=Microsoft.Jet.OLEDB.4.0;'
		+ 'Data Source=' + FileNameEdit.Text + ';'
		+ 'Persist Security Info=False'
		;
}
	ConnectionString :=
		'Driver={Microsoft Access Driver (*.mdb)};'
		+ 'Dbq=' + FileNameEdit.Text + ';'
//					 "Uid=admin;" & _
//					 "Pwd="
		;
	TryConnect;
end;

procedure TDatabaseSetupForm.TryConnectFirebird;
begin
	ConnectionString :=
		'DRIVER=Firebird/InterBase(r) driver;'
		//+ 'UID=SYSDBA;PWD=masterkey;'
		+ 'DBNAME=' + FileNameEdit.Text + ';'
		;
	TryConnect;
end;

procedure TDatabaseSetupForm.TryConnectInterbase5;
begin
	ConnectionString :=
		'Driver={INTERSOLV InterBase ODBC Driver (*.gdb)};'
		+ 'Server=localhost;'
		+ 'Database=localhost:' + FileNameEdit.Text + ';'
		//+ 'Uid=username;Pwd=password;'
		;
	TryConnect;
end;

procedure TDatabaseSetupForm.TryConnectInterbase6;
begin
	ConnectionString :=
		'Driver={Easysoft IB6 ODBC};'
		+ 'Server=localhost;'
		+ 'Database=localhost:' + FileNameEdit.Text + ';'
		//+ 'Uid=username;Pwd=password;'
		;
	TryConnect;
end;

procedure TDatabaseSetupForm.TryConnectSQLServer;
begin
	ConnectionString :=
		'Driver={SQL Server};'
		+ 'Server=server_name;'
		+ 'Database=database_name;'
		//+ 'Uid=username;Pwd=password;'
		;
	TryConnect;
end;

procedure TDatabaseSetupForm.TryConnectOracle;
begin
	// old version
//	Connection.ConnectionString :=
//		'Driver={Microsoft ODBC Driver for Oracle};'
//		+ 'ConnectString=OracleServer.world;'
//		//+ 'Uid=username;Pwd=password;'
//		;
	ConnectionString :=
		'Driver={Microsoft ODBC for Oracle};'
		+ 'Server=OracleServer.world;'
		//+ 'Uid=username;Pwd=password;'
		;
	TryConnect;
end;

procedure TDatabaseSetupForm.TryConnectODBC;
begin
	ConnectionString := Connection.ConnectionString;
	TryConnect;
end;

procedure TDatabaseSetupForm.UpdateConnection;
begin
	Connection.Connected := false;
	case TypeCombo.ItemIndex of
		cMYSQL: TryConnectMySql;
		cFirebird: TryConnectFirebird;
		cInterbase5: TryConnectInterbase5;
		cInterbase6: TryConnectInterbase6;
		4: TryConnectSQLServer;
		5: TryConnectOracle;
		cAccess: TryConnectAccess;
		cODBC, cCustom: TryConnectODBC;
	end;
	ConnectionStringEdit.Text := ConnectionString;
	ConnectionEditButton.Enabled := TypeCombo.ItemIndex >= cODBC;
	UpdateDatabaseList;
	ConnectedBox.Checked := Connection.Connected;
end;

procedure TDatabaseSetupForm.ConnectionEditButtonClick(Sender: TObject);
begin
	EditConnectionString;
end;

procedure TDatabaseSetupForm.TypeComboChange(Sender: TObject);
begin
	Connection.Connected := false;
	ConnectedBox.Checked := false;
	ConnectionString := '';
	UpdateUI;
	Update;
{
	if TypeCombo.ItemIndex = cODBC then
		EditConnectionString
	else
		Connection.ConnectionString := '';
}
	if TypeCombo.ItemIndex = cMYSQL then
		UpdateConnection;
	//ConnectionEditButton.Enabled := TypeCombo.ItemIndex = cODBC;
	//UpdateConnection;
end;

procedure TDatabaseSetupForm.AuthEditExit(Sender: TObject);
begin
	if TypeCombo.ItemIndex = cMYSQL then
		UpdateConnection;
end;

procedure TDatabaseSetupForm.BrowseButtonClick(Sender: TObject);
begin
	if OpenDialog.Execute then
		FilenameEdit.Text := OpenDialog.Filename;
	UpdateConnection;
end;

	function AddSlashes(const inString: string): string;
	var
		i: Integer;
		s: string;
	begin
		Result := '';
		for i := 1 to Pred(Length(inString)) do
		begin
			s := inString[i];
			if Pos(s, '\"') > 0 then
				s := '\' + s;
			Result := Result + s;
		end;
	end;

procedure TDatabaseSetupForm.GenerateButtonClick(Sender: TObject);
begin
	with TStringList.Create do
	try
		Add('<?php');
		Add('include "adodb.inc.php";');
		Add('echo "Hello from PHP!<br><br>";');
		//
		case TypeCombo.ItemIndex of
			cMYSQL:
			begin
				Add('$db = NewADOConnection("mysql");');
				Add('$db->Connect("", "", "", "test");');
				Add('echo "<pre>";');
				Add('$rs = $db->Execute("select * from sampletable");');
			end;
			//
			1:
			begin
				Add('$db = NewADOConnection("firebird");');
				Add('$db->Connect("filename", "", "");');
				Add('echo "<pre>";');
				Add('$rs = $db->Execute("select * from sampletable");');
			end;
			//
			2, 3:
			begin
				Add('$db = NewADOConnection("ibase");');
				Add('$db->Connect("localhost:filename", "", "");');
				Add('echo "<pre>";');
				Add('$rs = $db->Execute("select * from sampletable");');
			end;
			//
			4:
			begin
				Add('$db = NewADOConnection("mssql");');
				Add('$db->Connect("", "", "test");');
				Add('echo "<pre>";');
				Add('$rs = $db->Execute("select * from sampletable");');
			end;
			//
			5:
			begin
				Add('$db = NewADOConnection("oci8");');
				Add('$db->Connect("serverip:1521", "", "", "test");');
				Add('echo "<pre>";');
				Add('$rs = $db->Execute("select * from sampletable");');
			end;
			//
			6, cODBC:
			begin
				Add('$db = NewADOConnection("ado");');
				Add('$db->Connect("' + AddSlashes({Connection.}ConnectionString) + '");');
				Add('echo "<pre>";');
				Add('$rs = $db->Execute("select * from UsersTable");');
			end;
		end;
		//
		Add('while ($rs && !$rs->EOF)');
		Add('{');
		Add('	print_r($rs->fields);');
		Add('	$rs->MoveNext();');
		Add('}');
		Add('echo "</pre>";');
		Add('');
		Add('?>');
		//
		SaveToFile('F:\Web\TurboPhp4.Examples\libs\ado\atest.php');
		ShellExecute(0, '',
			'http://itch.homeip.net:88/web/TurboPhp4.Examples/libs/ado/atest.php',
				'', '',	0);
	finally
		Free;
	end;
end;

procedure TDatabaseSetupForm.UpdateUI;
begin
	SameAsPanel.Visible := false; //(ServerTabs.Selected <> 0);
	SameAsPanel.Top := 33;
	case TypeCombo.ItemIndex of
		cODBC, cCustom: UserPanel.Visible := false;
		else UserPanel.Visible := true;
	end;
	UserPanel.Top := TypePanel.BoundsRect.Bottom;
	DatabasePanel.Visible := (TypeCombo.ItemIndex = cMYSQL);
	DatabasePanel.Top := UserPanel.BoundsRect.Bottom;
	case TypeCombo.ItemIndex of
		cAccess, cFirebird, cInterbase5, cInterbase6: FilePanel.Visible := true;
		else FilePanel.Visible := false;
	end;
	FilePanel.Top := UserPanel.BoundsRect.Bottom;
	ADODBPanel.Visible := (TypeCombo.ItemIndex = cCustom);
	ADODBPanel.Top := TypePanel.BoundsRect.Bottom;
	ODBCPanel.Visible := (TypeCombo.ItemIndex = cODBC);
	ODBCPanel.Top := TypePanel.BoundsRect.Bottom;
	DesignPanel.Visible := true; //(ServerTabs.Selected = 0);
	DesignPanel.Top := 9999;
//	CustomODBCBox.Enabled := (ServerTabs.Selected = 0);
//	CustomODBCBox.Checked := (CustomODBCBox.Checked and CustomODBCBox.Enabled)
//		or (TypeCombo.ItemIndex = cODBC);
	ODBCLabel.Enabled := CustomODBCBox.Checked;
	ConnectionStringEdit.Text := ConnectionString;
	ConnectionStringEdit.Enabled := CustomODBCBox.Checked;
//	ADODBLabel.Enabled := CustomDNSBox.Checked;
//	DNSEdit.Enabled := CustomDNSBox.Checked;
end;

procedure TDatabaseSetupForm.ServerTabsSelect(Sender: TObject);
begin
	UpdateUI;
end;

procedure TDatabaseSetupForm.CustomODBCBoxClick(Sender: TObject);
begin
	UpdateUI;
end;

procedure TDatabaseSetupForm.CustomDNSBoxClick(Sender: TObject);
begin
	UpdateUI;
end;

procedure TDatabaseSetupForm.TestButtonClick(Sender: TObject);
begin
	UpdateConnection;
end;

procedure TDatabaseSetupForm.DatabaseToUi;
begin
	with Database do
	begin
		NameEdit.Text := DisplayName;
		TypeCombo.ItemIndex := Ord(Vendor);
		DatabaseCombo.Text := Database;
		FilenameEdit.Text := DatabaseFile;
		ConnectionString := ODBC;
		UserEdit.Text := User;
		PasswordEdit.Text := Password;
	end;
	if Visible and NameEdit.CanFocus then
		with NameEdit do
		begin
			SetFocus;
			SelectAll;
		end;
end;

procedure TDatabaseSetupForm.UiToDatabase;
begin
	with Database do
	begin
		DisplayName := NameEdit.Text;
		Vendor := ThtDatabaseVendor(TypeCombo.ItemIndex);
		Database := DatabaseCombo.Text;
		DatabaseFile := FilenameEdit.Text;
		ODBC := ConnectionString;
		User := UserEdit.Text;
		Password := PasswordEdit.Text;
	end;
end;

procedure TDatabaseSetupForm.SetDatabase(const Value: ThtDatabaseItem);
begin
	FDatabase := Value;
	DatabaseToUi;
	UpdateUi;
end;

procedure TDatabaseSetupForm.OkButtonClick(Sender: TObject);
begin
	UiToDatabase;
end;

end.
