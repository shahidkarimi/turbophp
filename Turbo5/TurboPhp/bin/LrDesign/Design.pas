unit Design;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs,
	DesignController, DesignSurface, DesignManager;

type
	TDesignForm = class(TForm)
		DesignController1: TDesignController;
		procedure FormPaint(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure DesignController1Change(Sender: TObject);
		procedure DesignController1SelectionChange(Sender: TObject);
		procedure DesignController1GetAddClass(Sender: TObject;
			var ioClass: String);
	private
		{ Private declarations }
		FOnChange: TNotifyEvent;
	protected
		function GetActive: Boolean;
		procedure CreateParams(var Params: TCreateParams); override;
		procedure PropertyChange(Sender: TObject);
		procedure ReaderError(Reader: TReader; const Message: string;
			var Handled: Boolean);
		procedure SelectionChange(Sender: TObject);
		procedure SetActive(const Value: Boolean);
		procedure SetParent(AParent: TWinControl); override;
	public
		{ Public declarations }
		procedure LoadFromFile(const inFilename: string);
		procedure SaveToFile(const inFilename: string);
		procedure Change;
		procedure SetPageSize(inW, inH: Integer);
		procedure UpdateDesigner;
		property Active: Boolean read GetActive write SetActive;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
	end;

var
	DesignForm: TDesignForm;

implementation

uses
	LrVclUtils, htPaint;

{$R *.dfm}

procedure TDesignForm.FormCreate(Sender: TObject);
begin
	//
end;

procedure TDesignForm.FormPaint(Sender: TObject);
begin
	htPaintRules(Canvas, ClientRect);
end;

procedure TDesignForm.CreateParams(var Params: TCreateParams);
begin
	inherited;
	//Params.Style := Params.Style and not WS_CLIPSIBLINGS;
end;

procedure TDesignForm.SetPageSize(inW, inH: Integer);
begin
	if Parent <> nil then
	begin
		Parent.ClientWidth := inW + 12 + 12;
		Parent.ClientHeight := inH + 12 + 12;
		SetBounds(12, 12, inW, inH);
	end;
end;

procedure TDesignForm.SetParent(AParent: TWinControl);
begin
	inherited;
	SetPageSize(Width, Height);
{
	if Parent <> nil then
	begin
		Left := 12;
		Top := 12;
		Parent.ClientWidth := Width + 12 + 12;
		Parent.ClientHeight := Height + 12 + 12;
	end;
}
end;

function TDesignForm.GetActive: Boolean;
begin
	Result := DesignController1.Active;
end;

procedure TDesignForm.SetActive(const Value: Boolean);
begin
	if Value then
	begin
		DesignMgr.SelectionObservers.Add(SelectionChange);
		DesignMgr.PropertyObservers.Add(PropertyChange);
	end else
	begin
		DesignMgr.SelectionObservers.Remove(SelectionChange);
		DesignMgr.PropertyObservers.Remove(PropertyChange);
	end;
	DesignController1.Active := Value;
end;

procedure TDesignForm.UpdateDesigner;
begin
	DesignController1.UpdateDesigner;
end;

procedure TDesignForm.Change;
begin
	if Assigned(OnChange) then
		OnChange(Self);
end;

procedure TDesignForm.DesignController1Change(Sender: TObject);
begin
	DesignMgr.DesignChange;
end;

procedure TDesignForm.PropertyChange(Sender: TObject);
begin
	UpdateDesigner;
end;

procedure TDesignForm.SelectionChange(Sender: TObject);
begin
	DesignController1.SetSelected(DesignMgr.Selected);
end;

procedure TDesignForm.DesignController1SelectionChange(Sender: TObject);
begin
	if Active then
		DesignMgr.ObjectsSelected(Self, DesignController1.Selected);
end;

procedure TDesignForm.DesignController1GetAddClass(Sender: TObject;
	var ioClass: String);
begin
	ioClass := DesignMgr.AddClass;
end;

procedure TDesignForm.ReaderError(Reader: TReader; const Message: string;
	var Handled: Boolean);
begin
	Handled := true;
end;

procedure TDesignForm.LoadFromFile(const inFilename: string);
begin
	DestroyComponents;
	//DesignController1.Free;
	LrLoadComponentFromFile(Self, inFilename, ReaderError);
	Name := 'DesignSurface';
	//DesignController1.Active := true;
	DesignController1.OnSelectionChange := DesignController1SelectionChange;
	DesignController1.OnGetAddClass := DesignController1GetAddClass
end;

procedure TDesignForm.SaveToFile(const inFilename: string);
begin
	Visible := false;
	LrSaveComponentToFile(Self, inFilename);
	Visible := true;
end;

end.
