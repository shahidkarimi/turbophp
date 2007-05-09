unit JsEditView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, Menus, StdCtrls, ExtCtrls,
	dcstring, dcsystem, dcparser, dccommon, dcmemo,
	EasyClasses, EasyParser, EasyEditor, EasyEditSource, {EasyEditorActions,}
	CodeExplorerView, dxDockControl, dxDockPanel, dfsSplitter;

type
	TJsEditForm = class(TForm)
    Source: TEasyEditSource;
		JsParser: TEasyEditorParser;
		ChangeTimer: TTimer;
    dfsSplitter1: TdfsSplitter;
    Edit: TEasyEdit;
		procedure EditSourceChanged(Sender: TObject;
			State: TEasyEditSourceStates);
		procedure FormCreate(Sender: TObject);
		procedure ChangeTimerTimer(Sender: TObject);
		procedure FormShow(Sender: TObject);
	private
		{ Private declarations }
		FOnModified: TNotifyEvent;
		FCodeDesigner: TCodeDesigner;
		ExplorerForm: TCodeExplorerForm;
	protected
		function GetStrings: TStrings;
		procedure SetCodeDesigner(const Value: TCodeDesigner);
		procedure SetStrings(const Value: TStrings);
	protected
		procedure DoModified;
		procedure LazyUpdate;
	public
		{ Public declarations }
		procedure ShowSource(Sender: TObject; inX, inY: Integer);
		procedure ValidateMethods(inContainer: TWinControl);
	public
		property CodeDesigner: TCodeDesigner read FCodeDesigner
			write SetCodeDesigner;
		property Strings: TStrings read GetStrings write SetStrings;
		property OnModified: TNotifyEvent read FOnModified write FOnModified;
	end;

implementation

uses
	LrUtils, JavaScriptCodeDesigner;

{$R *.dfm}

procedure TJsEditForm.FormCreate(Sender: TObject);
begin
	Source.Strings.Clear;
	//
	CodeDesigner := TJavaScriptCodeDesigner.Create(Self);
	CodeDesigner.OnShowSource := ShowSource;
	//
	AddForm(ExplorerForm, TCodeExplorerForm, Self, alLeft);
	ExplorerForm.Left := 0;
	ExplorerForm.EasyEdit := Edit;
end;

procedure TJsEditForm.FormShow(Sender: TObject);
begin
	LazyUpdate;
end;

function TJsEditForm.GetStrings: TStrings;
begin
	Result := Source.Strings;
end;

procedure TJsEditForm.SetStrings(const Value: TStrings);
begin
	Source.Strings.Assign(Value);
	LazyUpdate;
end;

procedure TJsEditForm.DoModified;
begin
	if Assigned(OnModified) then
		OnModified(Self);
end;

procedure TJsEditForm.EditSourceChanged(Sender: TObject;
	State: TEasyEditSourceStates);
begin
	if (State <> [csPositionChanged]) then
	begin
		ChangeTimer.Enabled := true;
		if Edit.Modified then
			DoModified;
	end;
end;

procedure TJsEditForm.ChangeTimerTimer(Sender: TObject);
begin
	ChangeTimer.Enabled := false;
	LazyUpdate;
end;

procedure TJsEditForm.LazyUpdate;
begin
	if (ExplorerForm <> nil) {and (ExplorerDock.Parent <> nil)} then
		ExplorerForm.UpdateExplorer;
end;

procedure TJsEditForm.ShowSource(Sender: TObject; inX, inY: Integer);
begin
	Source.JumpTo(inX, inY);
	if Edit.CanFocus then
		Edit.SetFocus;
end;

procedure TJsEditForm.SetCodeDesigner(const Value: TCodeDesigner);
begin
	FCodeDesigner := Value;
	FCodeDesigner.Strings := Strings;
end;

procedure TJsEditForm.ValidateMethods(inContainer: TWinControl);
begin
	with TJavaScriptCodeDesigner(CodeDesigner) do
	begin
		DeleteEmptyMethods;
		ValidateEventProperties(inContainer);
	end;
end;

end.
