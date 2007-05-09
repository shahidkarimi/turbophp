unit Design;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, dcfdes;

type
	TDesignForm = class(TForm)
		DCDesigner: TDCLiteDesigner;
		procedure DCDesignerSelectionChanged(Sender: TObject);
		procedure FormPaint(Sender: TObject);
		procedure DCDesignerAfterInsertComponent(Sender: TObject;
			AControl: TComponent);
		procedure DCDesignerInsertComponent(Sender: TObject;
			AControl: TComponent);
		procedure DCDesignerMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure DCDesignerChange(Sender: TObject);
	private
		{ Private declarations }
		FOnChange: TNotifyEvent;
		FOnSelectionChanged: TNotifyEvent;
	protected
		{ Protected declarations }
		function GetActive: Boolean;
		function GetSelectedComponent: TComponent;
    procedure CMControlListChange(var Message: TMessage); message CM_CONTROLLISTCHANGE;
		procedure CreateHandle; override;
		procedure SetActive(const Value: Boolean);
		procedure SetOnSelectionChanged(const Value: TNotifyEvent);
		procedure SetSelectedComponent(const Value: TComponent);
	public
		{ Public declarations }
		procedure LoadFromFile(const inFilename: string);
		procedure SaveToFile(const inFilename: string);
		property Active: Boolean read GetActive write SetActive;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
		property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged
			write SetOnSelectionChanged;
		property SelectedComponent: TComponent read GetSelectedComponent
			write SetSelectedComponent;
	end;

implementation

uses
	htPaint, RichView;

{$R *.dfm}

{ TDreamDesignForm }

var
	Creating: Boolean;

procedure TDesignForm.CreateHandle;
begin
	Creating := true;
	try
		inherited;
		if Active and not Creating then
		begin
			Active := false;
			Active := true;
		end;
	finally
		Creating := false;
	end;
end;

procedure TDesignForm.FormPaint(Sender: TObject);
begin
	htPaintRules(Canvas, ClientRect);
end;

function TDesignForm.GetActive: Boolean;
begin
	Result := DCDesigner.Active;
end;

procedure TDesignForm.SetActive(const Value: Boolean);
begin
	DCDesigner.Active := Value;
end;

procedure TDesignForm.LoadFromFile(const inFilename: string);
var
	d: TDCLiteDesigner;
begin
	d := DCDesigner;
	RemoveComponent(d);
	try
		d.LoadFromFile(Self, inFilename);
		//Visible := false;
		//DCDesigner.Designer.PopupMenu := d.Designer.PopupMenu;
	finally
		d.Free;
	end;
end;

procedure TDesignForm.SaveToFile(const inFilename: string);
begin
	DCDesigner.SaveToFile(inFilename);
end;

function TDesignForm.GetSelectedComponent: TComponent;
begin
	with DCDesigner.Designer do
		if SelectedComponents.Count = 0 then
			Result := nil
		else
			Result := SelectedComponents[0];
end;

procedure TDesignForm.SetOnSelectionChanged(const Value: TNotifyEvent);
begin
	FOnSelectionChanged := Value;
end;

procedure TDesignForm.SetSelectedComponent(const Value: TComponent);
begin
	DCDesigner.Designer.SelectComponent(Value);
end;

procedure TDesignForm.DCDesignerSelectionChanged(Sender: TObject);
begin
	if CanFocus and not (csDestroying in ComponentState) then
	begin
		SetFocus;
		if Assigned(OnSelectionChanged) then
			OnSelectionChanged(Self);
	end;
end;

procedure TDesignForm.DCDesignerChange(Sender: TObject);
begin
	if Assigned(OnChange) then
		OnChange(Self);
end;

type
	TDesignComponent = class(TComponent);

procedure TDesignForm.DCDesignerInsertComponent(Sender: TObject;
	AControl: TComponent);
begin
	if AControl is TRichView then
		TDesignComponent(AControl).SetDesigning(false);
end;

procedure TDesignForm.DCDesignerAfterInsertComponent(Sender: TObject;
	AControl: TComponent);
begin
{
	if AControl is TControl then
		with TControl(AControl) do
			if Parent = Self then
				Align := alTop;
}
end;

procedure TDesignForm.DCDesignerMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//
end;

procedure TDesignForm.CMControlListChange(var Message: TMessage);
begin
//	if Boolean(Message.LParam) then
//		TControl(Message.WParam).Align := alTop;
	inherited;
end;

end.
