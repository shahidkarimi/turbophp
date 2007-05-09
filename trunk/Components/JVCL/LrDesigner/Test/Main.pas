unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, StdCtrls, ComCtrls, ExtCtrls, JvComponent, JvInspector,
  JvExControls, DesignScrollBox;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
		ToolButton2: TToolButton;
		SelectButton: TToolButton;
		ToolButton3: TToolButton;
		ActiveButton: TToolButton;
		DesignScrollBox1: TDesignScrollBox;
		JvInspector1: TJvInspector;
		JvInspectorDotNETPainter1: TJvInspectorDotNETPainter;
    Splitter1: TSplitter;
		procedure FormCreate(Sender: TObject);
		procedure ToolButton1Click(Sender: TObject);
		procedure ToolButton2Click(Sender: TObject);
		procedure SelectButtonClick(Sender: TObject);
		procedure ActiveButtonClick(Sender: TObject);
    procedure JvInspector1DataValueChanged(Sender: TObject;
      Data: TJvCustomInspectorData);
	private
		{ Private declarations }
		procedure DesignSelectionChange(Sender: TObject);
		procedure GetAddClass(Sender: TObject; var ioClass: String);
	public
		{ Public declarations }
		DesignClass: string;
	end;

var
	MainForm: TMainForm;

implementation

uses
	Design, Utils;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
	AddForm(DesignForm, TDesignForm, Self);
	DesignForm.DesignController.OnSelectionChange := DesignSelectionChange;
	DesignForm.DesignController.OnGetAddClass := GetAddClass;
	DesignForm.DesignController.Active := true;
end;

procedure TMainForm.GetAddClass(Sender: TObject; var ioClass: String);
begin
	ioClass := DesignClass;
	DesignClass := '';
	SelectButton.Down := true; 
end;

procedure TMainForm.SelectButtonClick(Sender: TObject);
begin
	DesignClass := '';
end;

procedure TMainForm.ToolButton1Click(Sender: TObject);
begin
	DesignClass := 'TLabel';
end;

procedure TMainForm.ToolButton2Click(Sender: TObject);
begin
	DesignClass := 'TPanel';
end;

procedure TMainForm.ActiveButtonClick(Sender: TObject);
begin
	DesignForm.DesignController.Active := ActiveButton.Down;
end;

procedure TMainForm.DesignSelectionChange(Sender: TObject);
begin
	JvInspector1.BeginUpdate;
	try
		JvInspector1.Clear;
		TJvInspectorPropData.New(JvInspector1.Root,	DesignForm.CreateSelectionList);
	finally
		JvInspector1.EndUpdate;
	end;
end;

procedure TMainForm.JvInspector1DataValueChanged(Sender: TObject;
	Data: TJvCustomInspectorData);
begin
	DesignForm.DesignController.UpdateDesigner;
end;

initialization
	RegisterClass(TLabel);
	RegisterClass(TPanel);
end.
