unit ThConnectionStringEdit;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, dcedit, DB, ADODB;

type
	TThConnectionStringEditForm = class(TForm)
		Label1: TLabel;
		ConnectionStringEdit: TDCEdit;
		OkButton: TButton;
		ADOConnection: TADOConnection;
		procedure FormShow(Sender: TObject);
		procedure ConnectionStringEditButton2Click(Sender: TObject);
	private
		{ Private declarations }
		procedure ConnectionStringsToEditPopup;
		function GetConnectionString: string;
		procedure SetConnectionString(const Value: string);
	public
		{ Public declarations }
		property ConnectionString: string read GetConnectionString
			write SetConnectionString;
	end;

var
	ThConnectionStringEditForm: TThConnectionStringEditForm;

implementation

uses
	AdoConEd, ThDataConnection;

{$R *.dfm}

procedure TThConnectionStringEditForm.FormShow(Sender: TObject);
begin
	ConnectionStringsToEditPopup;
end;

procedure TThConnectionStringEditForm.ConnectionStringsToEditPopup;
begin
	with TPopupListBox(ConnectionStringEdit.PopupWindow) do
		Items.Assign(NeedConnectionStore.ConnectionStrings);
end;

procedure TThConnectionStringEditForm.ConnectionStringEditButton2Click(
	Sender: TObject);
begin
	EditConnectionString(AdoConnection);
	ConnectionStringEdit.Text := AdoConnection.ConnectionString;
end;

function TThConnectionStringEditForm.GetConnectionString: string;
begin
	Result := ConnectionStringEdit.Text;
end;

procedure TThConnectionStringEditForm.SetConnectionString(const Value: string);
begin
	ConnectionStringEdit.Text := Value;
end;

end.
