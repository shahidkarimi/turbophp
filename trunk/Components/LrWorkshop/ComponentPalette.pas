unit ComponentPalette;

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
	TComponentPaletteForm = class(TForm)
		PaletteScroll: TScrollBox;
		DCCompPalette1: TDCCompPalette;
		LMDImageList1: TLMDImageList;
		LrCollapsable1: TLrCollapsable;
		LMDGlyphLabel3: TLMDGlyphLabel;
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
    procedure PaletteLabelStartDrag(Sender: TObject;
      var DragObject: TDragObject);
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
  ComponentPaletteForm: TComponentPaletteForm;

implementation

uses
	{Math,} LrUtils, ClassInfo, DesignManager;

{$R *.dfm}

type
	TAddClassDragObject = class(TDragObject)
	private
		FAddClass: string;
	public
		function GetName: string; override;
		procedure Finished(Target: TObject; X, Y: Integer;
			Accepted: Boolean); override;
		property AddClass: string read FAddClass write FAddClass;
	end;

{ TAddClassDragObject }

procedure TAddClassDragObject.Finished(Target: TObject; X, Y: Integer;
	Accepted: Boolean); 
begin
	inherited;
	Free;
end;

function TAddClassDragObject.GetName: string;
begin
	Result := ClassName;
end;

{ TComponentPaletteForm }

procedure TComponentPaletteForm.FormCreate(Sender: TObject);
begin
	BuildPalette;
	Width := Width + 1;
	DesignMgr.OnGetAddClass := GetAddClass;
end;

procedure TComponentPaletteForm.BuildPalette;
var
	i, j: Integer;
	l: TLMDGlyphLabel;
	r: TLrCollapsable;

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

	function AddImage(const inClass: TClass): Integer;

		procedure Matte(var inBitmap: TBitmap; inColor: TColor);
		var
			b: TBitmap;
		begin
			b := TBitmap.Create;
			try
				b.Width := inBitmap.Width;
				b.Height := inBitmap.Height;
				b.Canvas.Brush.Color := inColor;
				b.Canvas.FillRect(Rect(0, 0, inBitmap.Width, inBitmap.Height));
				b.Canvas.Draw(0, 0, inBitmap);
			finally
				inBitmap.Free;
				inBitmap := b;
			end;
		end;

		procedure Stretch(var inBitmap: TBitmap; inW, inH: Integer);
		var
			b: TBitmap;
		begin
			b := TBitmap.Create;
			try
				b.Width := inW;
				b.Height := inH;
				b.Canvas.StretchDraw(Rect(0, 0, inW - 1, inH - 1), inBitmap);
			finally
				inBitmap.Free;
				inBitmap := b;
			end;
		end;

	var
		b: TBitmap;
	begin
		b := TBitmap.Create;
		try
			LoadBitmapForClass(b, inClass);
			b.Transparent := true;
			if (b.Width <> 16) then
			begin
				Matte(b, clWhite);
				Stretch(b, 16, 16);
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
		l.Caption := ComponentRegistry.DisplayName[inButton.ButtonName];
		//l.Caption := inButton.ButtonName;
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
		l.OnStartDrag := PaletteLabelStartDrag;
		//l.OnEndDrag := PaletteLabelEndDrag;
		l.OnMouseDown := PaletteLabelMouseDown;
		//
		l.ImageList := LMDImageList1;
		l.ListIndex := 0;
		//l.ImageIndex := inButton.ImageIndex;
		l.ImageIndex := AddImage(TDCCompButton(inButton).ComponentClass);
		//
		l.AutoSize := false;
		//l.Height := l.Height + 2;
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
			for j := 0 to Pred(ButtonCount) do
				CreateLabel(Buttons[j]);
			//r.Height := 0;
		end;
	end;
end;

procedure TComponentPaletteForm.CreateWnd;
begin
	inherited;
//	if PaletteScroll <> nil then
//		PaletteScroll.Realign;
end;

procedure TComponentPaletteForm.FormShow(Sender: TObject);
begin
	with PaletteScroll do
		Width := Width + 1;
end;

procedure TComponentPaletteForm.SetSelectedLabel(const Value: TControl);
begin
	if (SelectedLabel <> nil) then
		TLMDGlyphLabel(SelectedLabel).Font.Style := [ ];
	FSelectedLabel := Value;
	if (SelectedLabel <> nil) then
		TLMDGlyphLabel(Value).Font.style := [ fsBold ];
end;

procedure TComponentPaletteForm.PaletteLabelMouseEnter(Sender: TObject);
begin
	with TLMDGlyphLabel(Sender) do
		Font.Style := Font.Style + [ fsUnderline ];
end;

procedure TComponentPaletteForm.PaletteLabelMouseExit(Sender: TObject);
begin
	with TLMDGlyphLabel(Sender) do
		Font.Style := Font.Style - [ fsUnderline ];
end;

procedure TComponentPaletteForm.PaletteLabelMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	//if Button = mbLeft then
	//	TControl(Sender).BeginDrag(False);
end;

procedure TComponentPaletteForm.PaletteLabelClick(Sender: TObject);
begin
	SelectedLabel := TControl(Sender);
end;

procedure TComponentPaletteForm.GetAddClass(Sender: TObject;
	var ioClass: String);
begin
	if SelectedLabel <> nil then
	begin
		ioClass := SelectedLabel.Hint;
		SelectedLabel := nil;
	end;
end;

procedure TComponentPaletteForm.PaletteLabelStartDrag(Sender: TObject;
	var DragObject: TDragObject);
begin
	DragObject := TAddClassDragObject.Create;
	TAddClassDragObject(DragObject).AddClass := TLMDGlyphLabel(Sender).Hint;
end;

end.
