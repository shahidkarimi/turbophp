unit StyleActionBar;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ImgList, StdCtrls, ToolWin, ActnMan, ActnCtrls, ActnList, XPStyleActnCtrls,
	LMDFontComboBox, LMDCustomComboBox, LMDComboBox,
	ThCssStyle, ThStyledCtrl;

type
	TStyleActionBarForm = class(TForm)
		ActionManager1: TActionManager;
		StyleBoldAction: TAction;
		StyleItalicAction: TAction;
		StyleUnderlineAction: TAction;
		StyleBordersAction: TAction;
		StyleLeftBorder: TAction;
		StyleRightBorderAction: TAction;
		StyleTopBorderAction: TAction;
		StyleBottomBorderAction: TAction;
		ActionToolBar1: TActionToolBar;
		StyleCombo: TLMDComboBox;
		FontCombo: TLMDFontComboBox;
		FontSizeCombo: TLMDComboBox;
		ToolImages: TImageList;
		procedure FormCreate(Sender: TObject);
		procedure StyleItalicActionUpdate(Sender: TObject);
		procedure StyleBoldActionUpdate(Sender: TObject);
		procedure StyleBoldActionExecute(Sender: TObject);
		procedure StyleItalicActionExecute(Sender: TObject);
		procedure StyleUnderlineActionUpdate(Sender: TObject);
		procedure StyleUnderlineActionExecute(Sender: TObject);
		procedure StyleLeftBorderUpdate(Sender: TObject);
		procedure StyleRightBorderActionUpdate(Sender: TObject);
		procedure StyleTopBorderActionUpdate(Sender: TObject);
		procedure StyleBottomBorderActionUpdate(Sender: TObject);
		procedure StyleBordersActionUpdate(Sender: TObject);
		procedure StyleBordersActionExecute(Sender: TObject);
		procedure StyleLeftBorderExecute(Sender: TObject);
		procedure StyleRightBorderActionExecute(Sender: TObject);
		procedure StyleTopBorderActionExecute(Sender: TObject);
		procedure StyleBottomBorderActionExecute(Sender: TObject);
		procedure FontComboChange(Sender: TObject);
		procedure FontSizeComboChange(Sender: TObject);
		procedure StyleComboChange(Sender: TObject);
	private
		{ Private declarations }
		FComponent: TComponent;
		FStyle: TThCssStyle;
		FStyleClass: string;
		function HaveStyle: Boolean;
		procedure SetComponent(const Value: TComponent);
		procedure BorderExecute(inBorder: TThCssBorder);
		procedure BorderUpdate(inSender: TObject; inBorder: TThCssBorder);
		procedure StyleChange;
		procedure UpdateControls;
		procedure UpdateFontControls;
		procedure UpdateStyleList;
	public
		{ Public declarations }
		procedure UpdateStyle;
		property Component: TComponent read FComponent write SetComponent;
	end;

var
	StyleActionBarForm: TStyleActionBarForm;

implementation

uses
	LrUtils, ThStyleSheet, InspectorView;

{$R *.dfm}

procedure TStyleActionBarForm.FormCreate(Sender: TObject);
begin
	AutoSize := true;
end;

procedure TStyleActionBarForm.UpdateStyle;
begin
	with InspectorForm.ObjectInspector do
		if (CurrentControl is TComponent) then
			Component := TComponent(CurrentControl)
		else
			Component := nil;
	UpdateFontControls;
end;

procedure TStyleActionBarForm.SetComponent(const Value: TComponent);
begin
	if (FComponent <> Value) then
	begin
		FComponent := Value;
		UpdateControls;
	end;
end;

procedure TStyleActionBarForm.UpdateControls;
var
	e: Boolean;
	s: IThStyled;
begin
	//UpdateStyle;
	e := (Component <> nil) and IsAs(Component, IThStyled, s);
	if e then
	begin
		UpdateStyleList;
		FStyle := s.GetStyle;
		FStyleClass := s.GetStyleClass;
	end
	else begin
		FStyle := nil;
		FStyleClass := '';
	end;
	//UpdateFontControls;
end;

procedure TStyleActionBarForm.UpdateFontControls;
begin
	FontCombo.Enabled := HaveStyle;
	FontSizeCombo.Enabled := HaveStyle;
	StyleCombo.Enabled := HaveStyle;
	if HaveStyle then
	begin
		FontCombo.SelectedFont := FStyle.Font.FontFamily;
		if FStyle.Font.FontSizePx = 0 then
			FontSizeCombo.Text := ''
		else
			FontSizeCombo.Text := IntToStr(FStyle.Font.FontSizePx);
		StyleCombo.Text := FStyleClass;
	end
	else begin
		FontCombo.SelectedFont := '';
		FontSizeCombo.Text := '';
		StyleCombo.Text := '';
	end;
end;

procedure TStyleActionBarForm.UpdateStyleList;
var
	s: TThStyleSheet;
	i: Integer;
begin
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
end;

function TStyleActionBarForm.HaveStyle: Boolean;
begin
	Result := FStyle <> nil;
end;

procedure TStyleActionBarForm.StyleChange;
begin
	InspectorForm.ObjectInspector.Rescan;
end;

procedure TStyleActionBarForm.StyleBoldActionUpdate(Sender: TObject);
begin
	UpdateStyle;
	TAction(Sender).Enabled := HaveStyle;
	TAction(Sender).Checked := HaveStyle and (FStyle.Font.FontWeight = fwBold);
end;

procedure TStyleActionBarForm.StyleItalicActionUpdate(Sender: TObject);
begin
	UpdateStyle;
	TAction(Sender).Enabled := HaveStyle;
	TAction(Sender).Checked := HaveStyle and (FStyle.Font.FontStyle = fstItalic);
end;

procedure TStyleActionBarForm.StyleUnderlineActionUpdate(Sender: TObject);
begin
	UpdateStyle;
	TAction(Sender).Enabled := HaveStyle;
	TAction(Sender).Checked := HaveStyle and (FStyle.Font.FontDecoration = fdUnderline);
end;

procedure TStyleActionBarForm.BorderUpdate(inSender: TObject;
	inBorder: TThCssBorder);
begin
	UpdateStyle;
	TAction(inSender).Checked := (inBorder.BorderStyle <> bsNone)
		and (inBorder.BorderStyle <> bsDefault);
end;

procedure TStyleActionBarForm.StyleBordersActionUpdate(Sender: TObject);
begin
	UpdateStyle;
	TAction(Sender).Enabled := HaveStyle;
	if HaveStyle then
		BorderUpdate(Sender, FStyle.Border);
end;

procedure TStyleActionBarForm.StyleLeftBorderUpdate(Sender: TObject);
begin
	UpdateStyle;
	TAction(Sender).Enabled := HaveStyle;
	if HaveStyle then
		BorderUpdate(Sender, FStyle.Border.Edges.BorderLeft);
end;

procedure TStyleActionBarForm.StyleRightBorderActionUpdate(
	Sender: TObject);
begin
	UpdateStyle;
	TAction(Sender).Enabled := HaveStyle;
	if HaveStyle then
		BorderUpdate(Sender, FStyle.Border.Edges.BorderRight);
end;

procedure TStyleActionBarForm.StyleTopBorderActionUpdate(Sender: TObject);
begin
	UpdateStyle;
	TAction(Sender).Enabled := HaveStyle;
	if HaveStyle then
		BorderUpdate(Sender, FStyle.Border.Edges.BorderTop);
end;

procedure TStyleActionBarForm.StyleBottomBorderActionUpdate(
	Sender: TObject);
begin
	UpdateStyle;
	TAction(Sender).Enabled := HaveStyle;
	if HaveStyle then
		BorderUpdate(Sender, FStyle.Border.Edges.BorderBottom);
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
		FStyle.Font.FontDecoration := fdDefault
	else
		FStyle.Font.FontDecoration := fdUnderline;
	StyleChange;
end;

procedure TStyleActionBarForm.BorderExecute(inBorder: TThCssBorder);
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

procedure TStyleActionBarForm.StyleLeftBorderExecute(Sender: TObject);
begin
	BorderExecute(FStyle.Border.Edges.BorderLeft);
end;

procedure TStyleActionBarForm.StyleRightBorderActionExecute(
	Sender: TObject);
begin
	BorderExecute(FStyle.Border.Edges.BorderRight);
end;

procedure TStyleActionBarForm.StyleTopBorderActionExecute(Sender: TObject);
begin
	BorderExecute(FStyle.Border.Edges.BorderTop);
end;

procedure TStyleActionBarForm.StyleBottomBorderActionExecute(
	Sender: TObject);
begin
	BorderExecute(FStyle.Border.Edges.BorderBottom);
end;

procedure TStyleActionBarForm.FontComboChange(Sender: TObject);
begin
	if HaveStyle then
	begin
		FStyle.Font.FontFamily := FontCombo.SelectedFont;
		StyleChange;
	end;
end;

procedure TStyleActionBarForm.FontSizeComboChange(Sender: TObject);
begin
	if HaveStyle and (FontSizeCombo.Text <> '') then
	begin
		FStyle.Font.FontSizePx := StrToIntDef(FontSizeCombo.Text, FStyle.Font.FontSizePx);
		StyleChange;
	end;
end;

procedure TStyleActionBarForm.StyleComboChange(Sender: TObject);
var
	s: IThStyled;
begin
	if HaveStyle then
	begin
		if (Component <> nil) and IsAs(Component, IThStyled, s) then
			s.SetStyleClass(StyleCombo.Text);
		StyleChange;
	end;
end;

end.
