unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, StdCtrls, ComCtrls, ExtCtrls, Menus, ImgList,
  DesignSurface, DesignUtils;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    LabelButton: TToolButton;
    PanelButton: TToolButton;
    SelectButton: TToolButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ButtonButton: TToolButton;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Active1: TMenuItem;
    N1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    ImageButton: TToolButton;
    New1: TMenuItem;
    N2: TMenuItem;
    csDesigning1: TMenuItem;
    WindowProcHook1: TMenuItem;
    N3: TMenuItem;
    Rules1: TMenuItem;
    Grid1: TMenuItem;
    N4: TMenuItem;
    VSSelector1: TMenuItem;
    DelphiSelector1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ActiveButtonClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure PaletteButtonClick(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure HookClick(Sender: TObject);
    procedure RulesClick(Sender: TObject);
  private
    { Private declarations }
    procedure GetAddClass(Sender: TObject; var ioClass: String);
    procedure GridPaint(Sender: TObject);
    procedure RulesPaint(Sender: TObject);
  public
    { Public declarations }
    DesignClass: string;
    DesignScrollBox: TDesignScrollBox;
    StickyClass: Boolean;
  end;

var
  MainForm: TMainForm;

implementation

uses
  DesignImp, Design;

{$R *.dfm}

  procedure AddForm(var outForm; inFormClass: TFormClass; inParent: TWinControl;
    inAlign: TAlign = alClient; inShow: Boolean = true);
  begin
    TForm(outForm) := inFormClass.Create(nil);
    with TForm(outForm) do
    begin
      BorderIcons := [];
      Caption := '';
      BorderStyle := bsNone;
      Align := inAlign;
      Parent := inParent;
      Visible := inShow;
    end;
  end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DesignScrollBox := TDesignScrollBox.Create(Self);
  DesignScrollBox.Parent := Self;
  DesignScrollBox.Align := alClient;
  DesignScrollBox.BevelInner := bvLowered;
  DesignScrollBox.BevelOuter := bvNone;
  DesignScrollBox.BorderStyle := bsNone;
  //
  AddForm(DesignForm, TDesignForm, DesignScrollbox);
  DesignForm.OnPaint := RulesPaint;
  DesignForm.DesignSurface.OnGetAddClass := GetAddClass;
  DesignForm.DesignSurface.Active := true;
  //
  OpenDialog.InitialDir := ExtractFilePath(Application.ExeName);
  SaveDialog.InitialDir := OpenDialog.InitialDir;
end;

procedure TMainForm.GetAddClass(Sender: TObject; var ioClass: String);
begin
  ioClass := DesignClass;
  if not StickyClass then
  begin
    DesignClass := '';
    SelectButton.Down := true;
  end;
end;

procedure TMainForm.PaletteButtonClick(Sender: TObject);
const
  cClasses: array[0..4] of string = ( '', 'TButton', 'TLabel', 'TPanel',
    'TImage' );
begin
  StickyClass := (GetKeyState(VK_SHIFT) < 0);
  DesignClass := cClasses[TControl(Sender).Tag];
end;

procedure TMainForm.New1Click(Sender: TObject);
begin
  DesignForm.Clear;
end;

procedure TMainForm.OpenButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    DesignForm.LoadFromFile(OpenDialog.Filename);
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    DesignForm.SaveToFile(SaveDialog.Filename);
end;

procedure TMainForm.ActiveButtonClick(Sender: TObject);
begin
  DesignForm.DesignSurface.Active := Active1.Checked;
  DesignForm.Invalidate;
end;

procedure TMainForm.HookClick(Sender: TObject);
begin
  DesignForm.DesignSurface.Active := false;
  if WindowProcHook1.Checked then
    DesignForm.DesignSurface.MessengerClass := TDesignWinControlHookMessenger
  else
    DesignForm.DesignSurface.MessengerClass := TDesignDesignerMessenger;
  DesignForm.DesignSurface.Active := true;
  DesignForm.Invalidate;
end;

procedure TMainForm.RulesClick(Sender: TObject);
begin
  if Rules1.Checked then
  begin
    DesignForm.Color := clWhite;
    DesignForm.OnPaint := RulesPaint;
  end else
  begin
    DesignForm.Color := clBtnFace;
    DesignForm.OnPaint := GridPaint;
  end;
  DesignForm.Invalidate;
end;

procedure TMainForm.RulesPaint(Sender: TObject);
begin
  with DesignForm do
    DesignPaintRules(Canvas, ClientRect);
end;

procedure TMainForm.GridPaint(Sender: TObject);
begin
  with DesignForm do
    DesignPaintGrid(Canvas, ClientRect, Color);
end;

initialization
  RegisterClass(TDesignSurface);
  RegisterClass(TButton);
  RegisterClass(TLabel);
  RegisterClass(TPanel);
  RegisterClass(TImage);
end.
