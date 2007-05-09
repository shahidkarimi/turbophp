unit Palette;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ImgList, ComCtrls, ExtCtrls,
	DCPalette, dcpalet, DCGen,
	LMDCustomComponent, LMDBaseController, LMDCustomContainer,
	LMDCustomImageList, LMDImageList, LMDControl, LMDBaseControl,
	LMDBaseGraphicControl, LMDBaseLabel, LMDCustomGlyphLabel, LMDGlyphLabel,
	LMDGraph,
	LrCollapsable;

type
	TPaletteForm = class(TForm)
		PaletteScroll: TScrollBox;
		DCCompPalette1: TDCCompPalette;
		LMDImageList1: TLMDImageList;
		LrCollapsable1: TLrCollapsable;
		LMDGlyphLabel3: TLMDGlyphLabel;
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
	private
		{ Private declarations }
		FSelectedLabel: TControl;
		procedure PaletteLabelClick(Sender: TObject);
		procedure PaletteLabelMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure PaletteLabelMouseEnter(Sender: TObject);
		procedure PaletteLabelMouseExit(Sender: TObject);
    procedure SetSelectedLabel(const Value: TControl);
	protected
		procedure CreateWnd; override;
	public
		{ Public declarations }
		procedure BuildPalette;
		procedure GetAddClass(Sender: TObject; var ioClass: String);
		property SelectedLabel: TControl read FSelectedLabel write SetSelectedLabel;
	end;

var
  PaletteForm: TPaletteForm;

implementation

uses
	LrUtils, DesignManager;

{$R *.dfm}

procedure TPaletteForm.FormCreate(Sender: TObject);
begin
	BuildPalette;
	Width := Width + 1;
	DesignMgr.OnGetAddClass := GetAddClass;
end;

procedure TPaletteForm.BuildPalette;
var
	i, j: Integer;
	l: TLMDGlyphLabel;
//	r: TJvRollOut;
//	r: TFoldingPanelForm;
//	r: TLrGroup;
	r: TLrCollapsable;
//	r: TPanel;

{
	procedure CreateGroup;
	begin
		r := TJvRollOut.Create(Self);
		r.ParentColor := false;
		//
		r.Colors.ButtonBottom := clBlack;
		r.Colors.ButtonColor := $00BEF3CA; //$00FDF9F7;
		//r.Colors.ButtonTop := clRed;
		r.Colors.FrameBottom := clWhite;
		r.Colors.FrameTop := $0094EBA7; //$00F3D1BE;
		//r.Colors.HotTrackText := clGray;
		r.Colors.Color := clWhite; //$00EFF7F8;
		//
		r.ChildOffset := 1;
		r.Parent := PaletteScroll;
		r.Top := 9999;
		r.Align := alTop;
		r.AutoSize := true;
		r.Caption := DCCompPalette1.Tabs[i];
	end;
}

{
	procedure CreateGroup;
	begin
		AddForm(r, TFoldingPanelForm, PaletteScroll);
		r.Top := 9999;
		r.Align := alTop;
	end;
}

{
	procedure CreateGroup(const inCaption: string);
	begin
		r := TLrGroup.Create(Self);
		r.Caption := inCaption;
		r.AutoSize := true;
		r.Margin := 2;
		r.Special := true;
		r.Parent := LrBar1;
		r.Top := 9999;
		r.Align := alTop;
	end;
}

	procedure CreateGroup(const inCaption: string);
	begin
		r := TLrCollapsable.Create(Self);
		r.Caption := inCaption;
		r.AutoSize := true;
		r.Parent := PaletteScroll;
		r.Color := $F9F9F9;
		r.HeaderColor := $E1E1E1;
		r.ParentFont := true;
		r.Font.Style := [ fsBold ];
		//r.Top := 9999;
		r.Align := alTop;
		SetBounds(0, 0, 120, 120);
		r.Visible := true;
	end;

{
	procedure CreateGroup(const inCaption: string);
	begin
		r := TPanel.Create(Self);
		r.AutoSize := true;
		r.BevelInner := TBevelCut(0);
		r.BevelOuter := TBevelCut(0);
		with LMDExplorerBar1.Sections.Add do
		begin
			Caption := inCaption;
			HeaderSize := 20;
			Style := ebsContainer;
			LinkedControl := r;
			Height := 120;
		end;
	end;
}

	function AddImage(const inClass: TClass): Integer;
	var
		b, c: TBitmap;
	begin
		b := TBitmap.Create;
		try
			LoadBitmapForClass(b, inClass);
			b.Transparent := true;
			if (b.Width <> 16) then
			begin
				c := b;
				b := TBitmap.Create;
				try
					b.Width := c.Width;
					b.Height := c.Height;
					b.Canvas.Brush.Color := clWhite;
					b.Canvas.FillRect(Rect(0, 0, c.Width, c.Height));
					b.Canvas.Draw(0, 0, c);
				finally
					c.Free;
				end;
				c := b;
				b := TBitmap.Create;
				try
					b.Width := 16;
					b.Height := 16;
					b.Canvas.StretchDraw(Rect(0,0,15,15), c);
				finally
					c.Free;
				end;
			end;
			Result := LMDImageList1.Items[0].Add(b, nil);
		finally
			b.Free;
		end;
	end;

	procedure CreateLabel(inButton: TDCPaletteButton);
	begin
		l := TLMDGlyphLabel.Create(Self);
		//
		//l.Caption := TurboClassInfo.DisplayName[inButton.ButtonName];
		l.Caption := inButton.ButtonName;
		l.Hint := inButton.ButtonName;
		//
		l.Align := alTop;
		l.Cursor := crHandPoint;
		l.Bevel.StandardStyle := lsSingle;
		l.Alignment.Alignment := agCenterLeft;
		l.Color := clLime;
		//l.DragMode := dmAutomatic;
		//
		l.OnMouseEnter := PaletteLabelMouseEnter;
		l.OnMouseExit := PaletteLabelMouseExit;
		l.OnClick := PaletteLabelClick;
		//l.OnStartDrag := PaletteLabelStartDrag;
		l.OnMouseDown := PaletteLabelMouseDown;
		//
		l.ImageList := LMDImageList1;
		l.ListIndex := 0;
		//l.ImageIndex := inButton.ImageIndex;
		l.ImageIndex := AddImage(TDCCompButton(inButton).ComponentClass);
		//
		l.AutoSize := false;
		l.Height := l.Height + 4;
		//
		l.Parent := r;
		l.Font.Style := [];
		//l.Parent := r.ContentPanel;
	end;

begin
	for i := 0 to Pred(DCCompPalette1.PageCount) do
	begin
		CreateGroup(DCCompPalette1.Tabs[i]);
		with DCCompPalette1.ButtonTabs[i] do
		begin
			//r.Height := ButtonCount * 16 + 20;
			for j := Pred(ButtonCount) downto 0 do
				CreateLabel(Buttons[j]);
			//r.Height := 0;
		end;
	end;
end;

procedure TPaletteForm.CreateWnd;
begin
	inherited;
//	if PaletteScroll <> nil then
//		PaletteScroll.Realign;
end;

procedure TPaletteForm.FormShow(Sender: TObject);
begin
	with PaletteScroll do
		Width := Width + 1;
end;

procedure TPaletteForm.SetSelectedLabel(const Value: TControl);
begin
	if (SelectedLabel <> nil) then
		TLMDGlyphLabel(SelectedLabel).Font.Style := [ ];
	FSelectedLabel := Value;
	if (SelectedLabel <> nil) then
		TLMDGlyphLabel(Value).Font.style := [ fsBold ];
end;

procedure TPaletteForm.PaletteLabelMouseEnter(Sender: TObject);
begin
	with TLMDGlyphLabel(Sender) do
		Font.Style := Font.Style + [ fsUnderline ];
end;

procedure TPaletteForm.PaletteLabelMouseExit(Sender: TObject);
begin
	with TLMDGlyphLabel(Sender) do
		Font.Style := Font.Style - [ fsUnderline ];
end;

procedure TPaletteForm.PaletteLabelMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbLeft then
		TControl(Sender).BeginDrag(False);
end;

procedure TPaletteForm.PaletteLabelClick(Sender: TObject);
begin
	SelectedLabel := TControl(Sender);
end;

procedure TPaletteForm.GetAddClass(Sender: TObject; var ioClass: String);
begin
	if SelectedLabel <> nil then
	begin
		ioClass := SelectedLabel.Hint;
		SelectedLabel := nil;
	end;
end;

end.
