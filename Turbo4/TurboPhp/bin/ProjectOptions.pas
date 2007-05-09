unit ProjectOptions;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls, dcedit, dcpedit, ComCtrls, Menus,
	Project, dcapi;

type
	TProjectOptionsForm = class(TForm)
		PageControl1: TPageControl;
		PublishSheet: TTabSheet;
		Label2: TLabel;
		Label3: TLabel;
    PublishRootEdit: TDCEdit;
		Label1: TLabel;
		Label4: TLabel;
		Panel3: TPanel;
		OkButton: TButton;
		Button3: TButton;
		Button2: TButton;
		UrlRootEdit: TDCEdit;
    TabSheet1: TTabSheet;
		Label6: TLabel;
		DCEdit1: TDCEdit;
		Label7: TLabel;
		DCEdit2: TDCEdit;
		Label9: TLabel;
		DCEdit3: TDCEdit;
		Label11: TLabel;
		DCEdit4: TDCEdit;
    DCPathDialog1: TDCPathDialog;
		procedure FormCreate(Sender: TObject);
		procedure OkButtonClick(Sender: TObject);
		procedure PublishRootEditButton2Click(Sender: TObject);
		procedure RootEditChange(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
	private
		{ Private declarations }
		FProject: TProject;
		procedure SetProject(const Value: TProject);
    procedure LoadHistory;
    procedure SaveHistory;
	public
		{ Public declarations }
		PublishRootHistory: TPopupListBox;
		UrlRootHistory: TPopupListBox;
		property Project: TProject read FProject write SetProject;
	end;

implementation

uses
	IniFiles, LrFolderBrowseUnit, Config;

const
	cHistoryFile = 'history.turbophp.cfg';
	cHistorySection = 'History';

{$R *.dfm}

procedure TProjectOptionsForm.FormCreate(Sender: TObject);
begin
	PublishRootHistory := TPopupListBox(PublishRootEdit.PopupWindow);
	UrlRootHistory := TPopupListBox(UrlRootEdit.PopupWindow);
	LoadHistory;
end;

procedure TProjectOptionsForm.FormDestroy(Sender: TObject);
begin
	SaveHistory;
end;

procedure TProjectOptionsForm.LoadHistory;
begin
	with TIniFile.Create(ConfigFolder + cHistoryFile) do
	try
		PublishRootHistory.Items.Text :=
			ReadString(cHistorySection, 'PublishRoot', '');
		UrlRootHistory.Items.Text := ReadString(cHistorySection, 'UrlRoot', '');
	finally
		Free;
	end;
end;

procedure TProjectOptionsForm.SaveHistory;
begin
	with TIniFile.Create(ConfigFolder + cHistoryFile) do
	try
		WriteString(cHistorySection, 'PublishRoot', PublishRootHistory.Items.Text);
		WriteString(cHistorySection, 'UrlRoot', UrlRootHistory.Items.Text);
	finally
		Free;
	end;
end;

procedure TProjectOptionsForm.OkButtonClick(Sender: TObject);
begin
	Project.PublishFolder := PublishRootEdit.Text;
	PublishRootHistory.Items.Add(PublishRootEdit.Text);
	//
	Project.Url := UrlRootEdit.Text;
	UrlRootHistory.Items.Add(UrlRootEdit.Text);
end;

procedure TProjectOptionsForm.PublishRootEditButton2Click(Sender: TObject);
//var
//	p: string;
begin
	if DCPathDialog1.Execute then
		PublishRootEdit.Text := DCPathDialog1.SelectedItem;
//	p := LrFolderBrowse('Select Publish Folder', PublishRootEdit.Text);
//	if p <> '' then
//		PublishRootEdit.Text := p;
end;

procedure TProjectOptionsForm.RootEditChange(Sender: TObject);
begin
	//OkButton.Enabled :=	DirectoryExists(PublishRootEdit.Text);
end;

procedure TProjectOptionsForm.SetProject(const Value: TProject);
begin
	FProject := Value;
	PublishRootEdit.Text := Project.PublishFolder;
	UrlRootEdit.Text := Project.Url;
	RootEditChange(Self);
end;

end.
