unit Design;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs,
	DesignSurface;

type
	TDesignForm = class(TForm)
		Designer: TDesignSurface;
		procedure FormPaint(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormResize(Sender: TObject);
    procedure DesignerControlAdded(Sender: TObject; inControl: TControl);
	private
		{ Private declarations }
	protected
		procedure SetParent(AParent: TWinControl); override;
	public
		{ Public declarations }
		procedure LoadFromFile(const inFilename: string);
		procedure SaveToFile(const inFilename: string);
		procedure SetPageSize(inW, inH: Integer);
	end;

var
	DesignForm: TDesignForm;

implementation

uses
	DesignUtils, DesignImp;

{$R *.dfm}

procedure TDesignForm.FormCreate(Sender: TObject);
begin
	Designer.MessengerClass := TDesignWinControlHookMessenger;
	Left := 12;
	Top := 12;
end;

procedure TDesignForm.SetPageSize(inW, inH: Integer);
begin
	if Parent <> nil then
	begin
		Parent.ClientWidth := inW + 12 + 12;
		Parent.ClientHeight := inH + 12 + 12;
		//SetBounds(12, 12, inW, inH);
	end;
end;

procedure TDesignForm.FormPaint(Sender: TObject);
begin
	DesignPaintRules(Canvas, ClientRect);
end;

procedure TDesignForm.FormResize(Sender: TObject);
begin
	SetPageSize(Width, Height);
end;

procedure TDesignForm.SetParent(AParent: TWinControl);
begin
	inherited;
	SetPageSize(Width, Height);
end;

procedure TDesignForm.LoadFromFile(const inFilename: string);
begin
	Designer := Designer.LoadFromFile(inFilename);
	Name := 'DesignSurface';
end;

procedure TDesignForm.SaveToFile(const inFilename: string);
begin
	Visible := false;
	Designer.SaveToFile(inFilename);
	Visible := true;
end;

procedure TDesignForm.DesignerControlAdded(Sender: TObject;
	inControl: TControl);
begin
	if inControl.Parent = Self then
		inControl.Align := alTop;
end;

end.
