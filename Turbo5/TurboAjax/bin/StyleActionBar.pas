unit StyleActionBar;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ImgList, StdCtrls, ToolWin, Menus,
	ActnMan, ActnCtrls, ActnList,	XPStyleActnCtrls,
	LMDFontComboBox, LMDCustomComboBox, LMDComboBox,
	dxBarExtItems, dxBar,
	htStyle, htInterfaces, htControls,
	ColorPickerButton, DFSClrBn;

type
	TStyleActionBarForm = class(TForm)
		ActionManager1: TActionManager;
		StyleBoldAction: TAction;
		StyleItalicAction: TAction;
		StyleUnderlineAction: TAction;
		StyleBordersAction: TAction;
    StyleLeftBorderAction: TAction;
		StyleRightBorderAction: TAction;
		StyleTopBorderAction: TAction;
		StyleBottomBorderAction: TAction;
		ToolImages: TImageList;
    ColorDialog: TColorDialog;
    dxBarDockControl1: TdxBarDockControl;
    dxBarManager1: TdxBarManager;
    dxBarFontNameCombo1: TdxBarFontNameCombo;
    dxColorCombo: TdxBarColorCombo;
    dxStyleCombo: TdxBarCombo;
    dxBarButton1: TdxBarButton;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    dxFontColorCombo: TdxBarColorCombo;
    dxFontNameCombo: TdxBarFontNameCombo;
    dxFontSizeSpin: TdxBarSpinEdit;
    dxBarButton4: TdxBarButton;
    dxBarButton5: TdxBarButton;
    dxBarButton6: TdxBarButton;
    dxBarButton7: TdxBarButton;
    dxBarButton8: TdxBarButton;
    dxBarListItem1: TdxBarListItem;
    dxBarStatic1: TdxBarStatic;
    dxBarButton9: TdxBarButton;
    dxBarControlContainerItem1: TdxBarControlContainerItem;
    ColorPickerButton: TColorPickerButton;
    FontColorPickerButton: TColorPickerButton;
    dxBarControlContainerItem2: TdxBarControlContainerItem;
		procedure FormCreate(Sender: TObject);
		procedure StyleBoldActionExecute(Sender: TObject);
		procedure StyleItalicActionExecute(Sender: TObject);
		procedure StyleUnderlineActionExecute(Sender: TObject);
		procedure StyleBordersActionExecute(Sender: TObject);
		procedure StyleLeftBorderActionExecute(Sender: TObject);
		procedure StyleRightBorderActionExecute(Sender: TObject);
		procedure StyleTopBorderActionExecute(Sender: TObject);
		procedure StyleBottomBorderActionExecute(Sender: TObject);
    procedure StyleComboChange(Sender: TObject; const Text: String);
    procedure dxBarDockControl1Resize(Sender: TObject);
		procedure dxBarManager1Docking(Sender: TdxBar;
      Style: TdxBarDockingStyle; DockControl: TdxDockControl;
      var CanDocking: Boolean);
    procedure dxColorComboChange(Sender: TObject);
    procedure dxFontColorComboChange(Sender: TObject);
    procedure dxFontNameComboChange(Sender: TObject);
    procedure dxFontSizeSpinChange(Sender: TObject);
    procedure dxFontSizeSpinCurChange(Sender: TObject);
    procedure ColorPickerButtonChange(Sender: TObject);
    procedure ColorPickerButtonDefaultSelect(Sender: TObject);
    procedure FontColorPickerButtonChange(Sender: TObject);
	private
		{ Private declarations }
		FComponent: TComponent;
		FStyle: ThtStyle;
		FStyleClass: string;
		FUpdating: Integer;
		function ChooseColor(inColor: TColor): TColor;
		function HaveStyle: Boolean;
		procedure BeginUpdate;
		procedure BorderExecute(inBorder: ThtBorder);
		procedure BorderUpdate(inSender: TObject; inBorder: ThtBorder);
		procedure EndUpdate;
		procedure SelectionChange(inSender: TObject);
		procedure SetComponent(const Value: TComponent);
		procedure StyleActionsUpdate;
		procedure StyleChange;
		procedure UpdateControls;
		procedure UpdateFontControls;
		procedure UpdateStyle;
		procedure UpdateStyleList;
	public
		{ Public declarations }
		procedure UpdateBar;
		property Component: TComponent read FComponent write SetComponent;
	end;

var
	StyleActionBarForm: TStyleActionBarForm;

implementation

uses
	LrVclUtils, DesignManager;

{$R *.dfm}

procedure TStyleActionBarForm.FormCreate(Sender: TObject);
begin
	//AutoSize := true;
	DesignMgr.SelectionObservers.Add(SelectionChange);
	UpdateBar;
end;

procedure TStyleActionBarForm.dxBarDockControl1Resize(Sender: TObject);
begin
	//ClientHeight := dxBarManager1.Bars.DockControls[dsTop].Height;
end;

procedure TStyleActionBarForm.dxBarManager1Docking(Sender: TdxBar;
	Style: TdxBarDockingStyle; DockControl: TdxDockControl;
	var CanDocking: Boolean);
begin
	//dxBarDockControl1Resize(Self);
end;

procedure TStyleActionBarForm.BeginUpdate;
begin
	Inc(FUpdating);
end;

procedure TStyleActionBarForm.EndUpdate;
begin
	Dec(FUpdating);
end;

procedure TStyleActionBarForm.StyleChange;
begin
	if FUpdating = 0 then
	begin
		DesignMgr.PropertyChange(Self);
		UpdateBar;
	end;
end;

procedure TStyleActionBarForm.SelectionChange(inSender: TObject);
begin
	with DesignMgr do
		if (SelectedObject <> nil) and (SelectedObject is TComponent) then
			Self.Component := TComponent(SelectedObject)
		else
			Self.Component := nil;
end;

procedure TStyleActionBarForm.SetComponent(const Value: TComponent);
begin
	if (FComponent <> Value) then
	begin
		FComponent := Value;
		UpdateBar;
	end;
end;

procedure TStyleActionBarForm.UpdateBar;
begin
	BeginUpdate;
	try
		UpdateStyle;
		UpdateControls;
	finally
		EndUpdate;
	end;
end;

procedure TStyleActionBarForm.UpdateStyle;
var
	e: Boolean;
	c: IhtControl;
begin
	e := (Component <> nil) and LrIsAs(Component, IhtControl, c);
	if e then
	begin
		UpdateStyleList;
		FStyle := c.Style;
		FStyleClass := c.StyleClass;
	end
	else begin
		FStyle := nil;
		FStyleClass := '';
	end;
end;

procedure TStyleActionBarForm.UpdateControls;
begin
	UpdateFontControls;
	StyleActionsUpdate;
	UpdateStyleList;
end;

function TStyleActionBarForm.HaveStyle: Boolean;
begin
	Result := FStyle <> nil;
end;

procedure TStyleActionBarForm.UpdateFontControls;
begin
	dxFontColorCombo.Enabled := HaveStyle;
	dxColorCombo.Enabled := HaveStyle;
	dxFontNameCombo.Enabled := HaveStyle;
	dxFontSizeSpin.Enabled := HaveStyle;
	dxStyleCombo.Enabled := HaveStyle;
	if HaveStyle then
	begin
		dxFontNameCombo.Text := FStyle.Font.FontFamily;
		dxFontSizeSpin.IntValue := FStyle.Font.FontSizePx;
		dxStyleCombo.Text := FStyleClass;
		ColorPickerButton.SelectionColor := FStyle.Color;
		FontColorPickerButton.SelectionColor := FStyle.Font.FontColor;
	end
	else begin
		dxFontNameCombo.Text := '';
		dxStyleCombo.Text := '';
	end;
end;

procedure TStyleActionBarForm.BorderUpdate(inSender: TObject;
	inBorder: ThtBorder);
begin
	TAction(inSender).Checked := HaveStyle and (inBorder.BorderStyle <> bsNone)
		and (inBorder.BorderStyle <> bsDefault);
end;

procedure TStyleActionBarForm.StyleActionsUpdate;
begin
	UpdateStyle;
	StyleBoldAction.Enabled := HaveStyle;
	StyleBoldAction.Checked := HaveStyle and (FStyle.Font.FontWeight = fwBold);
	StyleItalicAction.Enabled := HaveStyle;
	StyleItalicAction.Checked := HaveStyle
		and (FStyle.Font.FontStyle = fstItalic);
	StyleUnderlineAction.Enabled := HaveStyle;
	StyleUnderlineAction.Checked := HaveStyle
		and (fdUnderline in FStyle.Font.FontDecoration);
	StyleBordersAction.Enabled := HaveStyle;
	StyleLeftBorderAction.Enabled := HaveStyle;
	StyleRightBorderAction.Enabled := HaveStyle;
	StyleTopBorderAction.Enabled := HaveStyle;
	StyleBottomBorderAction.Enabled := HaveStyle;
	if HaveStyle then
	begin
		BorderUpdate(StyleBordersAction, FStyle.Border);
		BorderUpdate(StyleLeftBorderAction, FStyle.Border.Edges.BorderLeft);
		BorderUpdate(StyleRightBorderAction, FStyle.Border.Edges.BorderRight);
		BorderUpdate(StyleTopBorderAction, FStyle.Border.Edges.BorderTop);
		BorderUpdate(StyleBottomBorderAction, FStyle.Border.Edges.BorderBottom);
	end;
end;

procedure TStyleActionBarForm.UpdateStyleList;
{
var
	s: TThStyleSheet;
	i: Integer;
}
begin
{
	if (Component <> nil) and (Component is TControl) then
	begin
		s := ThFindStyleSheet(TControl(Component));
		if (s <> nil) then
		begin
			StyleCombo.Items.BeginUpdate;
			try
				StyleCombo.Items.Clear;
				for i := 0 to s.Styles.Count - 1 do
					StyleCombo.Items.Add(s.Styles.StyleItem[i].Name);
			finally
				StyleCombo.Items.EndUpdate;
			end;
		end;
	end;
}
end;

procedure TStyleActionBarForm.StyleBoldActionExecute(Sender: TObject);
begin
	if TAction(Sender).Checked then
		FStyle.Font.FontWeight := fwDefault
	else
		FStyle.Font.FontWeight := fwBold;
	StyleChange;
end;

procedure TStyleActionBarForm.StyleItalicActionExecute(Sender: TObject);
begin
	if TAction(Sender).Checked then
		FStyle.Font.FontStyle := fstDefault
	else
		FStyle.Font.FontStyle := fstItalic;
	StyleChange;
end;

procedure TStyleActionBarForm.StyleUnderlineActionExecute(Sender: TObject);
begin
	if TAction(Sender).Checked then
		FStyle.Font.FontDecoration := FStyle.Font.FontDecoration - [ fdUnderline ]
	else
		FStyle.Font.FontDecoration := FStyle.Font.FontDecoration + [ fdUnderline ];
	StyleChange;
end;

procedure TStyleActionBarForm.BorderExecute(inBorder: ThtBorder);
begin
	if inBorder.BorderStyle = bsSolidBorder then
		inBorder.BorderStyle := bsDefault
	else
		inBorder.BorderStyle := bsSolidBorder;
	StyleChange;
end;

procedure TStyleActionBarForm.StyleBordersActionExecute(Sender: TObject);
begin
	BorderExecute(FStyle.Border);
	StyleChange;
end;

procedure TStyleActionBarForm.StyleLeftBorderActionExecute(Sender: TObject);
begin
	BorderExecute(FStyle.Border.Edges.BorderLeft);
end;

procedure TStyleActionBarForm.StyleRightBorderActionExecute(Sender: TObject);
begin
	BorderExecute(FStyle.Border.Edges.BorderRight);
end;

procedure TStyleActionBarForm.StyleTopBorderActionExecute(Sender: TObject);
begin
	BorderExecute(FStyle.Border.Edges.BorderTop);
end;

procedure TStyleActionBarForm.StyleBottomBorderActionExecute(Sender: TObject);
begin
	BorderExecute(FStyle.Border.Edges.BorderBottom);
end;

procedure TStyleActionBarForm.dxFontNameComboChange(Sender: TObject);
begin
	if HaveStyle then
	begin
		FStyle.Font.FontFamily := dxFontNameCombo.Text;
		StyleChange;
	end;
end;

procedure TStyleActionBarForm.dxFontSizeSpinChange(Sender: TObject);
begin
	if HaveStyle then
	begin
		FStyle.Font.FontSizePx := dxFontSizeSpin.IntValue;
		StyleChange;
	end;
end;

procedure TStyleActionBarForm.dxFontSizeSpinCurChange(Sender: TObject);
begin
	if HaveStyle then
	begin
		if (dxFontSizeSpin.IntCurValue = 1) then
			dxFontSizeSpin.IntCurValue := 6
		else if (dxFontSizeSpin.IntCurValue < 6) then
			dxFontSizeSpin.IntCurValue := 0;
		FStyle.Font.FontSizePx := dxFontSizeSpin.IntCurValue;
		StyleChange;
	end;
end;

procedure TStyleActionBarForm.StyleComboChange(Sender: TObject;
	const Text: String);
//var
//	c: IhtControl;
begin
{
	if HaveStyle then
	begin
		if (Component <> nil) and LrIsAs(Component, IhtControl, c) then
			c.StyleClass := StyleCombo.Text;
		StyleChange;
	end;
}
end;

procedure TStyleActionBarForm.dxColorComboChange(Sender: TObject);
begin
	if HaveStyle then
	begin
		FStyle.Color := dxColorCombo.Color;
		StyleChange;
	end;
end;

procedure TStyleActionBarForm.dxFontColorComboChange(Sender: TObject);
begin
	if HaveStyle then
	begin
		FStyle.Font.FontColor := dxFontColorCombo.Color;
		StyleChange;
	end;
end;

function TStyleActionBarForm.ChooseColor(inColor: TColor): TColor;
begin
	ColorDialog.Color := inColor;
	if ColorDialog.Execute then
		inColor := ColorDialog.Color;
	Result := inColor;
end;

procedure TStyleActionBarForm.ColorPickerButtonChange(Sender: TObject);
begin
	if HaveStyle then
	begin
		FStyle.Color := ColorPickerButton.SelectionColor;
		StyleChange;
	end;
end;

procedure TStyleActionBarForm.ColorPickerButtonDefaultSelect(
	Sender: TObject);
begin
	TColorPickerButton(Sender).SelectionColor := clDefault;
end;

procedure TStyleActionBarForm.FontColorPickerButtonChange(Sender: TObject);
begin
	if HaveStyle then
	begin
		FStyle.Font.FontColor := FontColorPickerButton.SelectionColor;
		StyleChange;
	end;
end;

end.
