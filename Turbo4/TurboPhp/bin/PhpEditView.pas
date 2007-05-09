unit PhpEditView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, Menus, StdCtrls, ExtCtrls,
	dcstring, dcsystem, dcparser, dccommon, dcmemo,
	dxDockControl, dxDockPanel,
	dfsSplitter,
	EasyClasses, EasyParser, EasyEditor, EasyEditSource, {EasyEditorActions,}
	CodeExplorerView;

type
	TPhpEditForm = class(TForm)
		Source: TEasyEditSource;
		PhpParser: TEasyEditorParser;
		ChangeTimer: TTimer;
		Edit: TEasyEdit;
		dfsSplitter1: TdfsSplitter;
		procedure EditSourceChanged(Sender: TObject;
			State: TEasyEditSourceStates);
		procedure FormCreate(Sender: TObject);
		procedure ChangeTimerTimer(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure EditAutoComplete(Sender: TObject; Strings: TStrings;
			AKey: Char; var AllowPopup: Boolean);
		procedure EditBeforeInsertPopup(Sender: TObject; var s: String);
	private
		{ Private declarations }
		FCodeDesigner: TCodeDesigner;
		FOnModified: TNotifyEvent;
		ExplorerForm: TCodeExplorerForm;
		FObjectList: TStrings;
    FOnLazyUpdate: TNotifyEvent;
	protected
		function GetStrings: TStrings;
		procedure SetCodeDesigner(const Value: TCodeDesigner);
		procedure SetStrings(const Value: TStrings);
	protected
		procedure DoModified;
		function FillCodeCompletion(inStrings: TStrings): Boolean;
		procedure FillStrings(inStrings: TStrings; const inClass: string);
		procedure GetPlainString(var ioString: string);
		procedure LazyUpdate;
	public
		{ Public declarations }
		procedure ShowSource(Sender: TObject; inX, inY: Integer);
		procedure ValidateMethods(inContainer: TWinControl);
	public
		property CodeDesigner: TCodeDesigner read FCodeDesigner
			write SetCodeDesigner;
		property ObjectList: TStrings read FObjectList write FObjectList;
		property Strings: TStrings read GetStrings write SetStrings;
		property OnLazyUpdate: TNotifyEvent read FOnLazyUpdate write FOnLazyUpdate;
		property OnModified: TNotifyEvent read FOnModified write FOnModified;
	end;

implementation

uses
	EasySearchDlg, EasyReplDlg, EasyGotoDlg, LrUtils, PhpCodeDesigner;

{$R *.dfm}

procedure TPhpEditForm.FormCreate(Sender: TObject);
begin
	Source.Strings.Clear;
	//
	CodeDesigner := TPhpCodeDesigner.Create(Self);
	CodeDesigner.OnShowSource := ShowSource;
	//
	//AddForm(ExplorerForm, TCodeExplorerForm, ExplorerDock);
	AddForm(ExplorerForm, TCodeExplorerForm, Self, alLeft);
	ExplorerForm.EasyEdit := Edit;
	ExplorerForm.Left := 0;
end;

procedure TPhpEditForm.FormShow(Sender: TObject);
begin
	LazyUpdate;
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

procedure TPhpEditForm.DoModified;
begin
	if Assigned(OnModified) then
		OnModified(Self);
end;

procedure TPhpEditForm.EditSourceChanged(Sender: TObject;
	State: TEasyEditSourceStates);
begin
	if (State <> [csPositionChanged]) then
	begin
		ChangeTimer.Enabled := true;
		if Edit.Modified then
			DoModified;
	end;
end;

procedure TPhpEditForm.ChangeTimerTimer(Sender: TObject);
begin
	ChangeTimer.Enabled := false;
	LazyUpdate;
end;

procedure TPhpEditForm.LazyUpdate;
begin
	if Assigned(OnLazyUpdate) then
		OnLazyUpdate(Self);
	if (ExplorerForm <> nil) then
		ExplorerForm.UpdateExplorer;
end;

procedure TPhpEditForm.SetCodeDesigner(const Value: TCodeDesigner);
begin
	FCodeDesigner := Value;
	FCodeDesigner.Strings := Strings;
end;

procedure TPhpEditForm.ShowSource(Sender: TObject; inX, inY: Integer);
begin
	Source.JumpTo(inX, inY);
	if Edit.CanFocus then
		Edit.SetFocus;
end;

procedure TPhpEditForm.ValidateMethods(inContainer: TWinControl);
begin
	with TPhpCodeDesigner(CodeDesigner) do
	begin
		DeleteEmptyMethods;
		ValidateEventProperties(inContainer);
	end;
end;

//var
//	lastKey: Char;
procedure TPhpEditForm.EditAutoComplete(Sender: TObject;
	Strings: TStrings; AKey: Char; var AllowPopup: Boolean);
begin
{
	if (AKey = '>') and (lastKey = '-') then
		AllowPopup :=	FillCodeCompletion(Strings);
	lastKey := AKey;
}
	if (AKey = '>') then
		AllowPopup :=	FillCodeCompletion(Strings);
end;

function TPhpEditForm.FillCodeCompletion(inStrings: TStrings): Boolean;
var
	s: string;
	aClass: string;
begin
	Result := false;
	//
	//Edit.PopupWindow.Images.Free;
	//
	with Edit, Source, CurrentPosition do
		s := GetTextAt(Point(X {- 1}, Y), false);
	if s = '' then
		exit;
	//
	if (ObjectList <> nil) then
		aClass := ObjectList.Values[s];
	//
	inStrings.Clear;
	FillStrings(inStrings, aClass);
	//
	Result := true;
{
	aClass := GetClass(s);
	//
	if AClass <> nil then
		FillStrings(Strings, AClass)
	else begin
		cmp := EasyEdit.Owner.FindComponent(s);
		if cmp <> nil then
			FillStrings(Strings, cmp.ClassType)
		else
			FillStrings(Strings, TWinControl);
	end;
}
end;

procedure TPhpEditForm.FillStrings(inStrings: TStrings; const inClass: string);
const
	sColorTable = '{\rtf{\colortbl\red0\green0\blue0;\red0\green128\blue128;\red128\green0\blue0;\red128\green128\blue0;\red0\green0\blue255;}';
	sTypeStr = '\cf2 Type \cf0 \b | %s';
	sPropStr = '\cf1 Property \cf0 | %s | \b %s}';
	sVarStr = '\cf2 Var \cf0 \b | %s';
	sParamStr = '\cf3 Param \cf0 \b | %s';
	sConstStr = '\cf2 Const \cf0 \b | %s';
	sFuncStr = '\cf4 Function \cf0 \b | %s \b0 %s';
	sNoParams = '\b *No parameters expected*';
	sStandardProps: array[0..7] of string =
		( 'App', 'Attributes', 'Content', 'Elt', 'Hidden', 'Name', 'OnGenerate',
			'Styles' );
var
	i         : integer;
{
	Count     : integer;
	RealCount : integer;
	PropInfo  : PPropInfo;
	PropList  : PPropList;
}
begin
	inStrings.Add(Format(sColorTable + sTypeStr, [ inClass ]));
	for i := 0 to 7 do
	begin
		inStrings.Add(Format(sColorTable + sPropStr, [ sStandardProps[i], '' ]));
	end;
{
	Count := GetTypeData(AClass.ClassInfo)^.PropCount;
	if Count > 0 then
	begin
		GetMem(PropList, Count * SizeOf(Pointer));
		try
			RealCount := GetPropList(AClass.ClassInfo, tkAny, PropList);
			for i := 0 to RealCount - 1 do
			begin
				PropInfo := PropList^[i];
				Strings.Add(Format(sColorTable + sPropStr, [PropInfo.Name, PropInfo.PropType^.Name]));
			end;
		finally
			FreeMem(PropList, Count * SizeOf(Pointer));
		end;
	end;
}
end;

procedure TPhpEditForm.EditBeforeInsertPopup(Sender: TObject;
  var s: String);
begin
	GetPlainString(s);
end;

procedure TPhpEditForm.GetPlainString(var ioString: string);
var
	P : integer;
begin
	//outIsFunction := Pos('function', ioString) > 0;
	p := Pos('|', ioString);
	if p <> 0 then
		Delete(ioString, 1, p);
	ioString := Trim(ioString);
	p := Pos(' ', ioString);
	if p <> 0 then
		Delete(ioString, p, MaxInt);
end;

end.
