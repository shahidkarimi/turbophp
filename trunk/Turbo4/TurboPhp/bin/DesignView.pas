unit DesignView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls,
	dcsystem, dcfdes,
	LrObserverList;

type
	TDesignForm = class(TForm)
		DCLiteDesigner: TDCLiteDesigner;
		procedure DCLiteDesignerSelectionChanged(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
    procedure DCLiteDesignerDragDrop(Sender, Source, Target: TObject; X,
      Y: Integer);
	private
		{ Private declarations }
		FOnSelectionChange: TNotifyEvent;
		FObservers: TLrObserverList;
	protected
		function GetSelectedComponents: TList;
		function GetSelection: TComponent;
		procedure SetSelection(const Value: TComponent);
	protected
		procedure AlignControls(AControl: TControl; var Rect: TRect); override;
		procedure DesignerMouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure SelectionChange;
		procedure WMLButtonDown(var Message: TWMLButtonDown);
			message WM_LBUTTONDOWN;
	public
		{ Public declarations }
		procedure ActivateDesigner;
		procedure DeactivateDesigner;
		procedure LoadFromFile(const inFilename: string);
		procedure SaveToFile(const inFilename: string);
		procedure SetLimitInfo(inIndex: Integer; inComponent: TComponent;
			inActions: TAllowedActions = []);
	public
		property Observers: TLrObserverList read FObservers;
		property OnSelectionChange: TNotifyEvent read FOnSelectionChange
			write FOnSelectionChange;
		property Selection: TComponent read GetSelection write SetSelection;
		property SelectedComponents: TList read GetSelectedComponents;
	end;

implementation

{$R *.dfm}

procedure TDesignForm.FormCreate(Sender: TObject);
begin
	FObservers := TLrObserverList.Create;
end;

procedure TDesignForm.FormDestroy(Sender: TObject);
begin
	Observers.Free;
end;

procedure TDesignForm.SetLimitInfo(inIndex: Integer; inComponent: TComponent;
	inActions: TAllowedActions = []);
begin
	with DCLiteDesigner do
	begin
		while LimitInfos.Count <= inIndex do
			LimitInfos.Add;
		LimitInfos[inIndex].Component := inComponent;
		LimitInfos[inIndex].AllowedActions := inActions;
	end;
end;

procedure TDesignForm.LoadFromFile(const inFilename: string);
var
	d: TDCLiteDesigner;
begin
	d := DCLiteDesigner;
	RemoveComponent(d);
	d.LoadFromFile(Self, inFilename);
	Visible := false;
	DCLiteDesigner.Designer.PopupMenu := d.Designer.PopupMenu;
	d.Free;
end;

procedure TDesignForm.SaveToFile(const inFilename: string);
begin
	DCLiteDesigner.SaveToFile(inFilename);
end;

procedure TDesignForm.ActivateDesigner;
begin
	//DCLiteDesigner.OnDragOver := DCLiteDesignerDragOver;
	DCLiteDesigner.OnDragDrop := DCLiteDesignerDragDrop;
	DCLiteDesigner.Designer.OnMouseUp := DesignerMouseUp;
	DCLiteDesigner.Active := true;
	SelectionChange;
	DCLiteDesigner.Designer.BringContainersToFront;
end;

procedure TDesignForm.DeactivateDesigner;
begin
	DCLiteDesigner.Active := false;
end;

procedure TDesignForm.DesignerMouseUp(Sender: TObject; Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	SetFocus;
end;

procedure TDesignForm.AlignControls(AControl: TControl; var Rect: TRect);
var
	r: TRect;
	i: Integer;
begin
	inherited;
	r := ClientRect;
	AdjustClientRect(r);
	for i := 0 to Pred(ControlCount) do
		with Controls[i] do
		begin
			if Left < r.Left then
				Left := r.Left;
			if Top < r.Top then
				Top := r.Top;
		end;
end;

function TDesignForm.GetSelectedComponents: TList;
begin
	Result := DCLiteDesigner.Designer.SelectedComponents;
end;

procedure TDesignForm.SetSelection(const Value: TComponent);
begin
	with DCLiteDesigner.Designer do
		if Value <> nil then
			SelectComponent(Value)
		else
			ClearSelection;
end;

function TDesignForm.GetSelection: TComponent;
begin
	if (SelectedComponents <> nil) and (SelectedComponents.Count > 0) then
		Result := SelectedComponents[0]
	else
		Result := nil;
end;

procedure TDesignForm.DCLiteDesignerSelectionChanged(Sender: TObject);
begin
	SelectionChange;
end;

procedure TDesignForm.SelectionChange;
begin
	if Assigned(OnSelectionChange) then
		OnSelectionChange(Self);
end;

procedure TDesignForm.DCLiteDesignerDragDrop(Sender, Source,
	Target: TObject; X, Y: Integer);
begin
	while (Target <> nil) and (Target is TControl) and not (Target is TWinControl) do
		Target := TControl(Target).Parent;
	if (Target <> nil) and (Target is TWinControl) then
	begin
		DCLiteDesigner.Designer.CreateComponent(
			TComponentClass(GetClass(DesignerInsertClass)), TComponent(Target),
				X, Y, 0, 0);
		DesignerInsertClass := '';
	end;
end;

procedure TDesignForm.WMLButtonDown(var Message: TWMLButtonDown);
begin
	//
end;

end.
