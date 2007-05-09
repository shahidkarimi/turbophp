unit LrTabs;

interface

uses
	SysUtils, Windows, Types, Classes, Controls, Graphics, StdCtrls;

type
	TLrCustomTabs = class(TCustomControl)
	private
		FTabCount: Integer;
		FSelectedIndex: Integer;
		FAlignment: TAlignment;
	protected
		function GetMouseTab(var inX: Integer): Integer;
		function GetTabWidth: Integer; virtual; abstract;
		procedure SetAlignment(const Value: TAlignment);
		procedure SetSelectedIndex(const Value: Integer);
		procedure SetTabCount(const Value: Integer);
	protected
//		procedure Paint; override;
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure TabClick(inIndex, inX, inY: Integer); virtual;
	public
		constructor Create(inOwner: TComponent); override;
	published
		property Align;
		property Alignment: TAlignment read FAlignment write SetAlignment
			default taLeftJustify;
		property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex
			default 0;
		property TabCount: Integer read FTabCount write SetTabCount
			default 0;
		property Visible;
	end;
	//
	TLrTabs = class(TLrCustomTabs)
	private
		FImageList: TImageList;
	protected
		function GetImageWidth: Integer;
		function GetTabWidth: Integer; override;
		procedure SetImageList(const Value: TImageList);
	protected
		procedure Paint; override;
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
		procedure TabClick(inIndex, inX, inY: Integer); override;
	published
		property ImageList: TImageList read FImageList write SetImageList;
	end;
	//
	TLrTabs2 = class(TLrCustomTabs)
	protected
		function GetTabWidth: Integer; override;
	protected
		procedure Paint; override;
		procedure TabClick(inIndex, inX, inY: Integer); override;
	end;

implementation

uses
	Math;

{ TLrCustomTabs }

constructor TLrCustomTabs.Create(inOwner: TComponent);
begin
	inherited;
	FAlignment := taLeftJustify;
end;

function TLrCustomTabs.GetMouseTab(var inX: Integer): Integer;
var
	d, m: Word;
begin
	DivMod(inX, GetTabWidth, d, m);
	Result := d;
	inX := m;
end;

procedure TLrCustomTabs.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
	Y: Integer);
var
	t: Integer;
begin
	t := GetMouseTab(X);
	TabClick(t, X, Y);
end;

procedure TLrCustomTabs.TabClick(inIndex, inX, inY: Integer);
begin
	SelectedIndex := inIndex;
end;

procedure TLrCustomTabs.SetAlignment(const Value: TAlignment);
begin
	FAlignment := Value;
	Invalidate;
end;

procedure TLrCustomTabs.SetSelectedIndex(const Value: Integer);
begin
	FSelectedIndex := Value;
	Invalidate;
end;

procedure TLrCustomTabs.SetTabCount(const Value: Integer);
begin
	FTabCount := Value;
	Invalidate;
end;

{ TLrTabs }

const
	TT_SELSTART = 0;
	TT_SELBAR = 1;
	TT_SELEND = 2;
	TT_SELDIVIDE = 3;
	TT_START = 4;
	TT_BAR = 5;
	TT_END = 6;
	TT_DIVIDE = 7;
	TT_BACK = 8;
	BAR_SEGS = 4;

function TLrTabs.GetImageWidth: Integer;
begin
	if ImageList <> nil then
		Result := ImageList.Width
	else
		Result := 1;
end;

function TLrTabs.GetTabWidth: Integer;
begin
	Result := GetImageWidth * (BAR_SEGS + 1);
end;

procedure TLrTabs.TabClick(inIndex, inX, inY: Integer);
begin
	if (inX < GetTabWidth) and (inIndex > 0) and (inIndex <> SelectedIndex) then
		Dec(inIndex);
	inherited;
end;

procedure TLrTabs.Notification(AComponent: TComponent; Operation: TOperation);
begin
	inherited;
	if (Operation = opRemove) then
		if (AComponent = FImageList) then
			FImageList := nil;
end;

procedure TLrTabs.Paint;
var
	iw, w, x, i, j, iis, iib, iie: Integer;
begin
	inherited;
	if ImageList <> nil then
	begin
		iw := ImageList.Width;
		//
		case Alignment of
			taCenter:
			begin
				w := TabCount * (BAR_SEGS + 1) * iw;
				x := (Width - w) div 2;
				if (x < 0) then
					x := 0;
				i := 0;
				while (i < x) do
				begin
					ImageList.Draw(Canvas, i, 0, TT_BACK);
					Inc(i, iw);
				end;
			end;
			//
			else x := 0;
		end;
		//
		for i := 0 to Pred(TabCount) do
		begin
			if SelectedIndex = i then
			begin
				iis := TT_SELSTART;
				iib := TT_SELBAR;
				if (i < Pred(TabCount)) then
					iie := TT_SELDIVIDE
				else
					iie := TT_SELEND;
			end
			else begin
				if (i = 0) then
					iis := TT_START
				else
					iis := -1;
				iib := TT_BAR;
				if (i = Pred(TabCount)) then
					iie := TT_END
				else if (SelectedIndex <> i + 1) then
					iie := TT_DIVIDE
				else
					iie := -1;
			end;
			if iis <> -1 then
			begin
				ImageList.Draw(Canvas, x, 0, iis);
				Inc(x, iw);
			end;
			for j := 1 to BAR_SEGS do
			begin
				ImageList.Draw(Canvas, x, 0, iib);
				Inc(x, iw);
			end;
			if SelectedIndex = i then
				Canvas.Font.Style := [ fsBold ]
			else
				Canvas.Font.Style := [ ];
			SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
			Canvas.TextOut(x - iw*BAR_SEGS + 6, 2, 'Tab ' + IntToStr(i));
			if iie <> -1 then
			begin
				ImageList.Draw(Canvas, x, 0, iie);
				Inc(x, iw);
			end;
		end;
		while x < ClientWidth do
		begin
			ImageList.Draw(Canvas, x, 0, TT_BACK);
			Inc(x, iw);
		end;
	end;
end;

procedure TLrTabs.SetImageList(const Value: TImageList);
begin
	if FImageList <> nil then
		FImageList.RemoveFreeNotification(Self);
	FImageList := Value;
	if FImageList <> nil then
		FImageList.FreeNotification(Self);
	Invalidate;
end;

{ TLrTabs2 }

function TLrTabs2.GetTabWidth: Integer;
begin
	Result := 64;
end;

procedure TLrTabs2.Paint;
var
	r: TRect;
	i: Integer;
begin
	inherited;
	r := Rect(0, 0, GetTabWidth, 24);
	for i := 0 to Pred(TabCount) do
	begin
		if SelectedIndex = i then
			Canvas.Brush.Color := clWhite
		else
			Canvas.Brush.Color := clBtnFace;
		Canvas.RoundRect(r.Left, r.Top, r.Right, r.Bottom, 8, 8);
		OffsetRect(r, GetTabWidth, 0);
	end;
end;

procedure TLrTabs2.TabClick(inIndex, inX, inY: Integer);
begin
	inherited;
end;

end.
