unit PhpEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls, ComCtrls, ToolWin,
	LMDCustomScrollBox, LMDScrollBox, LMDSplt,
	LMDCustomControl, LMDCustomPanel, LMDCustomBevelPanel,
	EasyEditor, EasyEditSource, EasyClasses, EasyParser,
	CodeExplorer;

type
  TPhpEditForm = class(TForm)
    PhpParser: TEasyEditorParser;
    Source: TEasyEditSource;
    ChangeTimer: TTimer;
    LMDSplitterPanel1: TLMDSplitterPanel;
    ExplorerPane: TLMDSplitterPane;
    LMDSplitterPane2: TLMDSplitterPane;
    Edit: TEasyEdit;
    ToolBar1: TToolBar;
    CollapseButton: TToolButton;
    procedure FormCreate(Sender: TObject);
		procedure EditSourceChanged(Sender: TObject;
			State: TEasyEditSourceStates);
		procedure ChangeTimerTimer(Sender: TObject);
    procedure EditAutoComplete(Sender: TObject; Strings: TStrings;
      AKey: Char; var AllowPopup: Boolean);
    procedure CollapseButtonClick(Sender: TObject);
	private
		FOnModified: TNotifyEvent;
    procedure SetStrings(const Value: TStrings);
	protected
		function GetStrings: TStrings;
		procedure Modified;
		procedure LazyUpdate;
	public
		CodeExplorerForm: TCodeExplorerForm;
		property OnModified: TNotifyEvent read FOnModified write FOnModified;
		property Strings: TStrings read GetStrings write SetStrings;
	end;

var
	PhpEditForm: TPhpEditForm;

implementation

uses
	LrUtils;

{$R *.dfm}

procedure TPhpEditForm.FormCreate(Sender: TObject);
begin
	AddForm(CodeExplorerForm, TCodeExplorerForm, ExplorerPane);
	CodeExplorerForm.EasyEdit := Edit;
end;

procedure TPhpEditForm.EditSourceChanged(Sender: TObject;
	State: TEasyEditSourceStates);
begin
	if (State <> [csPositionChanged]) or ChangeTimer.Enabled then
		if Edit.Modified then
		begin
			ChangeTimer.Enabled := false;
			ChangeTimer.Enabled := true;
			Modified;
		end;
end;

procedure TPhpEditForm.Modified;
begin
	if {Edit.Modified and} Assigned(OnModified) then
		OnModified(Self);
end;

procedure TPhpEditForm.ChangeTimerTimer(Sender: TObject);
begin
	ChangeTimer.Enabled := false;
	LazyUpdate;
end;

procedure TPhpEditForm.LazyUpdate;
begin
	CodeExplorerForm.UpdateExplorer;
end;

function TPhpEditForm.GetStrings: TStrings;
begin
	Result := Source.Strings;
end;

procedure TPhpEditForm.SetStrings(const Value: TStrings);
begin
	Source.Strings.Assign(Value);
	LazyUpdate;
end;

procedure TPhpEditForm.EditAutoComplete(Sender: TObject; Strings: TStrings;
  AKey: Char; var AllowPopup: Boolean);
begin
//
end;

procedure TPhpEditForm.CollapseButtonClick(Sender: TObject);
begin
	if CollapseButton.Down then
		Edit.CollapseCode([ '{' ], [ '}'], [ {'*'} ], [], true, true, true, true)
	else
		Edit.UnCollapseCode;
end;

end.
