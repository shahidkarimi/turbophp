unit PropertyBar;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LMDCustomComboBox, LMDFontComboBox;

type
	TPropertyBarForm = class(TForm)
		LMDFontComboBox1: TLMDFontComboBox;
		procedure FormCreate(Sender: TObject);
    procedure LMDFontComboBox1Change(Sender: TObject);
	private
		{ Private declarations }
		FControl: TControl;
		procedure PropertyChange;
		procedure SelectionChange(inSender: TObject);
		procedure SetControl(const Value: TControl);
	public
		{ Public declarations }
		property Control: TControl read FControl write SetControl;
	end;

var
	PropertyBarForm: TPropertyBarForm;

implementation

uses
	DesignManager;

{$R *.dfm}

type
	TCrackedControl = class(TControl)
	end;

procedure TPropertyBarForm.FormCreate(Sender: TObject);
begin
	DesignMgr.SelectionObservers.Add(SelectionChange);
	Height := 24;
end;

procedure TPropertyBarForm.SelectionChange(inSender: TObject);
begin
	with DesignMgr do
		if (SelectedObject <> nil) and (SelectedObject is TControl) then
			Control := TControl(SelectedObject)
		else
			Control := nil;
end;

procedure TPropertyBarForm.PropertyChange;
begin
	DesignMgr.PropertyChange(Self);
end;

procedure TPropertyBarForm.SetControl(const Value: TControl);
begin
	FControl := Value;
	LMDFontComboBox1.Enabled := Control <> nil;
	if Control <> nil then
		LMDFontComboBox1.SelectedFont := TCrackedControl(Control).Font.Name;
end;

procedure TPropertyBarForm.LMDFontComboBox1Change(Sender: TObject);
begin
	if Control <> nil then
	begin
		TCrackedControl(Control).Font.Name := LMDFontComboBox1.SelectedFont;
		PropertyChange;
	end;
end;

end.
