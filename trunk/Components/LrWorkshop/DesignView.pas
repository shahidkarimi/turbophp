unit DesignView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls,
	RsRuler,
	DesignSurface,
  Design;

type
	TDesignViewForm = class(TForm)
		RulerPanel: TPanel;
		RsRuler1: TRsRuler;
		RsRulerCorner1: TRsRulerCorner;
		LeftRuler: TRsRuler;
		Scroller: TDesignScrollBox;
		BackPanel: TPanel;
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
	private
		{ Private declarations }
		FDesignForm: TDesignForm;
	protected
		procedure SetDesignForm(const Value: TDesignForm);
	public
		{ Public declarations }
		procedure ActivateDesign;
		procedure DeactivateDesign;
		property DesignForm: TDesignForm read FDesignForm write SetDesignForm;
	end;

var
	DesignViewForm: TDesignViewForm;

implementation

uses
	LrUtils, DesignManager;

{$R *.dfm}

const
	cMargin = 12;

procedure TDesignViewForm.FormCreate(Sender: TObject);
begin
	RulerPanel.DoubleBuffered := true;
end;

procedure TDesignViewForm.FormShow(Sender: TObject);
begin
	ActivateDesign;
end;

procedure TDesignViewForm.ActivateDesign;

	procedure ResetScrollbars;
	begin
		Scroller.HorzScrollBar.Position := 0;
		Scroller.VertScrollBar.Position := 0;
	end;

begin
	if {Showing and} (DesignForm <> nil) then
	begin
		DesignForm.Visible := true;
		DesignForm.Designer.Active := true;
	end;
	ResetScrollbars;
end;

procedure TDesignViewForm.DeactivateDesign;
begin
	if DesignForm <> nil then
	begin
		DesignForm.Designer.Active := false;
		DesignForm.Visible := false;
	end;
end;

procedure TDesignViewForm.SetDesignForm(const Value: TDesignForm);
begin
	DeactivateDesign;
	FDesignForm := Value;
	if DesignForm = nil then
		DesignMgr.Designer := nil
	else
	begin
		DesignMgr.Designer := DesignForm.Designer;
		DesignForm.Parent := BackPanel;
		DesignForm.Visible := true;
	end;
	ActivateDesign;
end;

end.
