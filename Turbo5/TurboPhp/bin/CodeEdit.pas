unit CodeEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls, ComCtrls, ToolWin,
	LMDCustomScrollBox, LMDScrollBox, LMDSplt,
	LMDCustomControl, LMDCustomPanel, LMDCustomBevelPanel,
	EasyEditor, EasyEditSource, EasyClasses, EasyParser,
	CodeExplorer;

type
  TCodeEditForm = class(TForm)
    PhpParser: TEasyEditorParser;
    Source: TEasyEditSource;
    ChangeTimer: TTimer;
    SplitterPanel: TLMDSplitterPanel;
    ExplorerPane: TLMDSplitterPane;
    LMDSplitterPane2: TLMDSplitterPane;
    Edit: TEasyEdit;
    ToolBar1: TToolBar;
    CollapseButton: TToolButton;
    JsParser: TEasyEditorParser;
    HtmlParser: TEasyEditorParser;
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

implementation

uses
	LrUtils;

{$R *.dfm}

procedure TCodeEditForm.FormCreate(Sender: TObject);
begin
	AddForm(CodeExplorerForm, TCodeExplorerForm, ExplorerPane);
	CodeExplorerForm.EasyEdit := Edit;
end;

procedure TCodeEditForm.EditSourceChanged(Sender: TObject;
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

procedure TCodeEditForm.Modified;
begin
	if {Edit.Modified and} Assigned(OnModified) then
		OnModified(Self);
end;

procedure TCodeEditForm.ChangeTimerTimer(Sender: TObject);
begin
	ChangeTimer.Enabled := false;
	LazyUpdate;
end;

procedure TCodeEditForm.LazyUpdate;
begin
	CodeExplorerForm.UpdateExplorer;
end;

function TCodeEditForm.GetStrings: TStrings;
begin
	Result := Source.Strings;
end;

procedure TCodeEditForm.SetStrings(const Value: TStrings);
begin
	Source.Strings.Assign(Value);
	LazyUpdate;
end;

procedure TCodeEditForm.EditAutoComplete(Sender: TObject; Strings: TStrings;
  AKey: Char; var AllowPopup: Boolean);
begin
//
end;

procedure TCodeEditForm.CollapseButtonClick(Sender: TObject);
begin
	if CollapseButton.Down then
		Edit.CollapseCode([ '{' ], [ '}'], [ {'*'} ], [], true, true, true, true)
	else
		Edit.UnCollapseCode;
end;

end.
