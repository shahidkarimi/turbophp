unit CodeEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls, ComCtrls, ToolWin,
	LMDCustomScrollBox, LMDScrollBox, LMDSplt,
	LMDCustomControl, LMDCustomPanel, LMDCustomBevelPanel,
	EasyStrings, EasyEditor, EasyEditSource, EasyClasses, EasyParser,
	CodeExplorer;

type
	TCodeEditForm = class(TForm)
		PhpParser: TEasyEditorParser;
		Source: TEasyEditSource;
		ChangeTimer: TTimer;
		ToolBar1: TToolBar;
		CollapseButton: TToolButton;
		JsParser: TEasyEditorParser;
		HtmlParser: TEasyEditorParser;
		Edit: TEasyEdit;
		procedure EditSourceChanged(Sender: TObject;
			State: TEasyEditSourceStates);
		procedure ChangeTimerTimer(Sender: TObject);
		procedure EditAutoComplete(Sender: TObject; Strings: TStrings;
			AKey: Char; var AllowPopup: Boolean);
		procedure CollapseButtonClick(Sender: TObject);
		procedure EditEnter(Sender: TObject);
		procedure EditExit(Sender: TObject);
	private
		FExplorer: TCodeExplorerForm;
		FOnModified: TNotifyEvent;
		FOnLazyUpdate: TNotifyEvent;
	protected
		function GetStrings: TStrings;
		procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
		procedure CreateHandle; override;
		procedure LazyUpdate;
		procedure Modified;
		procedure SetStrings(const Value: TStrings);
		procedure UpdateExplorer;
	public
		property Explorer: TCodeExplorerForm read FExplorer write FExplorer;
		property OnLazyUpdate: TNotifyEvent read FOnLazyUpdate write FOnLazyUpdate;
		property OnModified: TNotifyEvent read FOnModified write FOnModified;
		property Strings: TStrings read GetStrings write SetStrings;
	end;
	//
	THtmlEditForm = class(TCodeEditForm)
	public
		procedure AfterConstruction; override;
	end;

var
	CodeEditForm: TCodeEditForm;

function CreateHtmlViewForm: TCodeEditForm;

implementation

//uses
//	LrUtils;

{$R *.dfm}

function CreateHtmlViewForm: TCodeEditForm;
begin
	Result := TCodeEditForm.Create(Application);
	with Result do
	begin
		Source.Parser := HtmlParser;
		Source.ReadOnly := true;
		Edit.LineBreak := lbCR;
	end;
end;

procedure TCodeEditForm.CreateHandle;
begin
	inherited;
//	if Visible and Edit.CanFocus then
//		Edit.SetFocus;
end;

procedure TCodeEditForm.CMShowingChanged(var Message: TMessage);
begin
	inherited;
	if Showing and Edit.CanFocus then
		Edit.SetFocus;
end;

function TCodeEditForm.GetStrings: TStrings;
begin
	Result := Source.Strings;
end;

procedure TCodeEditForm.SetStrings(const Value: TStrings);
begin
	Source.Strings.Assign(Value);
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
	if Assigned(OnModified) then
		OnModified(Self);
end;

procedure TCodeEditForm.UpdateExplorer;
begin
	if Explorer <> nil then
		Explorer.EasyEdit := Edit;
end;

procedure TCodeEditForm.ChangeTimerTimer(Sender: TObject);
begin
	ChangeTimer.Enabled := false;
	LazyUpdate;
end;

procedure TCodeEditForm.LazyUpdate;
begin
	UpdateExplorer;
	if Assigned(OnLazyUpdate) then
		OnLazyUpdate(Self);
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

procedure TCodeEditForm.EditEnter(Sender: TObject);
begin
	UpdateExplorer;
end;

procedure TCodeEditForm.EditExit(Sender: TObject);
begin
	//
end;

{ THtmlEditForm }

procedure THtmlEditForm.AfterConstruction;
begin
	inherited;
	Source.Parser := HtmlParser;
	Source.ReadOnly := true;
	Edit.LineBreak := lbCR;
end;

end.
