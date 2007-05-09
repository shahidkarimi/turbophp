unit StartView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls, StdCtrls, GIFImage,
	LrDocument, LMDControl, LMDBaseControl, LMDBaseGraphicControl,
  LMDBaseLabel, LMDCustomSimpleLabel, LMDSimpleLabel;

const
	CM_OPEN = WM_USER + $10;

type
	TStartForm = class(TForm)
		Panel1: TPanel;
		Label1: TLabel;
		Label5: TLabel;
		Bevel1: TBevel;
		ProjectLabel: TLabel;
		Image1: TImage;
		Bevel4: TBevel;
		Bevel5: TBevel;
		Bevel2: TBevel;
		RecentPagesPanel: TPanel;
		RecentLabel: TLabel;
		Panel2: TPanel;
		Label2: TLabel;
		Bevel3: TBevel;
		RecentProjectsPanel: TPanel;
    Label3: TLabel;
		Bevel6: TBevel;
		RecentFilesPanel: TPanel;
		Bevel7: TBevel;
		Bevel8: TBevel;
		Bevel9: TBevel;
		procedure RecentLabelClick(Sender: TObject);
		procedure RecentLabelMouseEnter(Sender: TObject);
		procedure RecentLabelMouseLeave(Sender: TObject);
		procedure ProjectLabelClick(Sender: TObject);
	private
		{ Private declarations }
		procedure SetMRU(const Value: TStrings);
		procedure SetMRUProjects(const Value: TStrings);
		procedure SetMRUFiles(const Value: TStrings);
	protected
		FFileToOpen: string;
		procedure CMOpen(var inMsg: TMessage); message CM_OPEN;
		function CreateLabel(const inCaption: string; inParent: TWinControl;
			inClick: TNotifyEvent): TLMDSimpleLabel;
		procedure SetMRUItems(const Value: TStrings; inPanel: TWinControl;
			inClick: TNotifyEvent);
	public
		{ Public declarations }
		property MRU: TStrings write SetMRU;
		property MRUFiles: TStrings write SetMRUFiles;
		property MRUProjects: TStrings write SetMRUProjects;
	end;
	//
	TStartDocument = class(TLrDocument)
	public
		constructor Create(inManager: TLrDocumentManager;
			const inPath: string = ''); override;
		destructor Destroy; override;
	end;
var
	StartForm: TStartForm;

implementation

uses
	Config, Main;

{$R *.dfm}

{ TStartForm }

	procedure ClearPanel(inPanel: TWinControl);
	begin
		while inPanel.ControlCount > 0 do
			inPanel.Controls[0].Free;
	end;

function TStartForm.CreateLabel(const inCaption: string;
	inParent: TWinControl; inClick: TNotifyEvent): TLMDSimpleLabel;
begin
	Result := TLMDSimpleLabel.Create(Self);
	with Result do
	begin
		Cursor := crHandPoint;
		Caption := inCaption;
		AutoSize := false;
		Height := 26;
		Align := alTop;
		Parent := inParent;
		Top := 9999;
		ParentFont := true;
		Font.Color := clNavy;
		OnClick := inClick;
		OnMouseEnter := RecentLabelMouseEnter;
		OnMouseExit := RecentLabelMouseLeave;
		Options := [ loPathEllipsis ];
		//JumpMode := jmCustom;
	end;
end;

procedure TStartForm.SetMRUItems(const Value: TStrings; inPanel: TWinControl;
	inClick: TNotifyEvent);
var
	i: Integer;
	f: string;
begin
	ClearPanel(inPanel);
	for i := 0 to Pred(Value.Count) do
	begin
		f := ExpandFileName(Value[i]);
		if FileExists(f) then
			CreateLabel(f, inPanel, inClick)
	end;
end;

procedure TStartForm.SetMRU(const Value: TStrings);
//var
//	i: Integer;
//	e, f: string;
begin
	SetMRUItems(Value, RecentPagesPanel, RecentLabelClick)
{
	ClearPanel(RecentPagesPanel);
	//ClearPanel(RecentProjectsPanel);
	ClearPanel(RecentFilesPanel);
	for i := 0 to Pred(Value.Count) do
	begin
		f := ExpandFileName(Value[i]);
		if FileExists(f) then
		begin
			e := LowerCase(ExtractFileExt(f));
			if (e = '.tphp') then
				CreateLabel(f, RecentPagesPanel, RecentLabelClick);
}
{
			else if (e = '.tprj') then
				CreateLabel(f, RecentProjectsPanel)
			else
				CreateLabel(f, RecentFilesPanel);
}
//		end;
//	end;
end;

procedure TStartForm.SetMRUProjects(const Value: TStrings);
begin
	SetMRUItems(Value, RecentProjectsPanel, RecentLabelClick)
end;

procedure TStartForm.SetMRUFiles(const Value: TStrings);
begin
	SetMRUItems(Value, RecentFilesPanel, RecentLabelClick)
end;

procedure TStartForm.RecentLabelClick(Sender: TObject);
begin
	FFileToOpen := TLabel(Sender).Caption;
	PostMessage(Handle, CM_OPEN, 0, 0);
	//MainForm.Open(TLabel(Sender).Caption);
end;

procedure TStartForm.RecentLabelMouseEnter(Sender: TObject);
begin
	TLabel(Sender).Font.Style := [fsUnderline];
	TLabel(Sender).Font.Color := clBlue;
end;

procedure TStartForm.RecentLabelMouseLeave(Sender: TObject);
begin
	TLabel(Sender).Font.Style := [];
	TLabel(Sender).Font.Color := clNavy;
end;

procedure TStartForm.ProjectLabelClick(Sender: TObject);
begin
	FFileToOpen := HomeFolder + 'Examples\Examples.tpprj';
	PostMessage(Handle, CM_OPEN, 0, 0);
	//MainForm.Open(HomeFolder + 'Examples\Examples.tpprj');
end;

procedure TStartForm.CMOpen(var inMsg: TMessage);
begin
	MainForm.Open(FFileToOpen);
end;

{ TStartDocument }

constructor TStartDocument.Create(inManager: TLrDocumentManager;
	const inPath: string);
begin
	inherited;
end;

destructor TStartDocument.Destroy;
begin
	inherited;
end;

end.
