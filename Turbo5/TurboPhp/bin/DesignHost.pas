unit DesignHost;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls,
	RsRuler,
	Design, DesignScrollBox, dxBarExtItems, dxBar;

type
	TDesignHostForm = class(TForm)
		RulerPanel: TPanel;
		RsRuler1: TRsRuler;
		RsRulerCorner1: TRsRulerCorner;
		LeftRuler: TRsRuler;
    Scroller: TDesignScrollBox;
    BackPanel: TPanel;
    dxBarManager1: TdxBarManager;
    dxBarCombo1: TdxBarCombo;
    dxBarCombo2: TdxBarCombo;
    WidthSpin: TdxBarSpinEdit;
    HeightSpin: TdxBarSpinEdit;
    dxBarStatic1: TdxBarStatic;
		procedure FormCreate(Sender: TObject);
    procedure SizeSpinChange(Sender: TObject);
	private
		{ Private declarations }
		FDesignForm: TDesignForm;
	protected
		procedure CreateHandle; override;
		procedure ReactivateDesign;
		procedure SetDesignForm(const Value: TDesignForm);
	public
		{ Public declarations }
		procedure ActivateDesign;
		procedure DeactivateDesign;
		property DesignForm: TDesignForm read FDesignForm write SetDesignForm;
	end;

var
	DesignHostForm: TDesignHostForm;

implementation

uses
	LrUtils, StyleActionBar;

{$R *.dfm}

procedure TDesignHostForm.FormCreate(Sender: TObject);
begin
	RulerPanel.DoubleBuffered := true;
	AddForm(StyleActionBarForm, TStyleActionBarForm, Self, alTop);
	StyleActionBarForm.Top := 0;
end;

procedure TDesignHostForm.ActivateDesign;
begin
	ReactivateDesign;
	Scroller.HorzScrollBar.Position := 0;
	Scroller.VertScrollBar.Position := 0;
end;

procedure TDesignHostForm.DeactivateDesign;
begin
	if DesignForm <> nil then
	begin
		DesignForm.Active := false;
		DesignForm.Visible := false;
	end;
end;

procedure TDesignHostForm.ReactivateDesign;
begin
	if DesignForm <> nil then
	begin
		DesignForm.Active := false;
		DesignForm.Active := true;
		DesignForm.Visible := true;
	end;
end;

procedure TDesignHostForm.CreateHandle;
begin
	inherited;
	ReactivateDesign;
end;

procedure TDesignHostForm.SetDesignForm(const Value: TDesignForm);
begin
	DeactivateDesign;
	FDesignForm := Value;
	if DesignForm <> nil then
	begin
		if DesignForm.Parent <> BackPanel then
		begin
			DesignForm.Parent := BackPanel;
			DesignForm.Visible := true;
		end;
		DesignForm.BringToFront;
		WidthSpin.IntValue := DesignForm.Width;
		HeightSpin.IntValue := DesignForm.Height;
		DesignForm.SetPageSize(DesignForm.Width, DesignForm.Height);
	end;
	ActivateDesign;
end;

procedure TDesignHostForm.SizeSpinChange(Sender: TObject);
begin
	if (DesignForm <> nil) and DesignForm.Active then
		DesignForm.SetPageSize(WidthSpin.IntValue, HeightSpin.IntValue);
end;

end.
