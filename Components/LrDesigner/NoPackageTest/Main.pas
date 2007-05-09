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
    procedure DesignPaint(Sender: TObject);
    procedure GetAddClass(Sender: TObject; var ioClass: String);
  public
    { Public declarations }
    DesignClass: string;
    DesignPanel: TDesignPanel;
    DesignScrollBox: TDesignScrollBox;
    StickyClass: Boolean;
  end;

var
  MainForm: TMainForm;

implementation

uses
  DesignImp;

{$R *.dfm}

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
  DesignPanel := TDesignPanel.Create(Self);
  DesignPanel.Parent := DesignScrollBox;
  DesignPanel.Align := alClient;
  DesignPanel.Color := clWhite;
  DesignPanel.Surface.OnGetAddClass := GetAddClass;
  DesignPanel.Surface.Active := true;
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
  DesignPanel.Clear;
end;

procedure TMainForm.OpenButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    DesignPanel.LoadFromFile(OpenDialog.Filename);
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    DesignPanel.SaveToFile(SaveDialog.Filename);
end;

procedure TMainForm.ActiveButtonClick(Sender: TObject);
begin
  DesignPanel.Surface.Active := Active1.Checked;
  DesignPanel.Invalidate;
end;

procedure TMainForm.HookClick(Sender: TObject);
begin
  DesignPanel.Surface.Active := false;
  if WindowProcHook1.Checked then
    DesignPanel.Surface.MessengerClass := TDesignWinControlHookMessenger
  else
    DesignPanel.Surface.MessengerClass := TDesignDesignerMessenger;
  DesignPanel.Surface.Active := true;
  DesignPanel.Invalidate;
end;

procedure TMainForm.RulesClick(Sender: TObject);
begin
  if Rules1.Checked then
  begin
    DesignPanel.Color := clWhite;
    DesignPanel.DrawRules := true;
    DesignPanel.OnPaint := nil;
  end else
  begin
    DesignPanel.Color := clBtnFace;
    DesignPanel.DrawRules := false;
    DesignPanel.OnPaint := DesignPaint;
  end;
  DesignPanel.Invalidate;
end;

procedure TMainForm.DesignPaint(Sender: TObject);
begin
  with DesignPanel do
    DesignPaintGrid(Canvas, ClientRect, Color);
end;

initialization
  RegisterClass(TDesignSurface);
  RegisterClass(TButton);
  RegisterClass(TLabel);
  RegisterClass(TPanel);
  RegisterClass(TImage);
end.
