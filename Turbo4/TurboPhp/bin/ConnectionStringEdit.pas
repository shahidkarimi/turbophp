unit ConnectionStringEdit;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, dcedit, DB, ADODB;

type
	TConnectionStringEditForm = class(TForm)
		Label1: TLabel;
		OkButton: TButton;
		ADOConnection: TADOConnection;
    NameEdit: TDCEdit;
    Label2: TLabel;
    ConnectMemo: TMemo;
    Button1: TButton;
    Button2: TButton;
		procedure FormShow(Sender: TObject);
		procedure ConnectionStringEditClick(Sender: TObject);
    procedure NameEditChange(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
	private
		{ Private declarations }
		procedure ConnectionStringsToEditPopup;
		function GetConnectionString: string;
		procedure SetConnectionString(const Value: string);
    function GetConnectionName: string;
    procedure SetConnectionName(const Value: string);
	public
		{ Public declarations }
		property ConnectionName: string read GetConnectionName
			write SetConnectionName;
		property ConnectionString: string read GetConnectionString
			write SetConnectionString;
	end;

var
	ConnectionStringEditForm: TConnectionStringEditForm;

implementation

uses
	AdoConEd, ConnectionStore;

{$R *.dfm}

procedure TConnectionStringEditForm.FormShow(Sender: TObject);
begin
	ConnectionStringsToEditPopup;
end;

procedure TConnectionStringEditForm.ConnectionStringsToEditPopup;
var
	i: Integer;
begin
	with NeedConnectionStore.ConnectionStrings do
		for i := 0 to Pred(Count) do
			TPopupListBox(NameEdit.PopupWindow).Items.Add(Names[i]);
end;

procedure TConnectionStringEditForm.ConnectionStringEditClick(
	Sender: TObject);
begin
	AdoConnection.ConnectionString := ConnectMemo.Text;
	EditConnectionString(AdoConnection);
	if AdoConnection.ConnectionString <> '' then
		ConnectMemo.Text := AdoConnection.ConnectionString;
end;

function TConnectionStringEditForm.GetConnectionString: string;
begin
	Result := ConnectMemo.Text;
end;

procedure TConnectionStringEditForm.SetConnectionString(const Value: string);
var
	i: Integer;
begin
	ConnectMemo.Text := Value;
	with NeedConnectionStore.ConnectionStrings do
	begin
		i := 0;
		while (i < Count) do
			if ValueFromIndex[i] = Value then
				break
			else
				Inc(i);
		if (i < Count) then
			NameEdit.Text := Names[i];
	end;
end;

function TConnectionStringEditForm.GetConnectionName: string;
begin
	Result := NameEdit.Text;
end;

procedure TConnectionStringEditForm.SetConnectionName(const Value: string);
begin
	NameEdit.Text := Value;
end;

procedure TConnectionStringEditForm.NameEditChange(Sender: TObject);
var
	i: Integer;
begin
	with NeedConnectionStore.ConnectionStrings do
	begin
		i := IndexOfName(NameEdit.Text);
		if (i >= 0) then
			ConnectMemo.Text := ValueFromIndex[i];
	end;
end;

procedure TConnectionStringEditForm.OkButtonClick(Sender: TObject);
begin
	if (NameEdit.Text <> '') and (ConnectMemo.Text <> '') then
		NeedConnectionStore.Add(NameEdit.Text, ConnectMemo.Text);
end;

end.
