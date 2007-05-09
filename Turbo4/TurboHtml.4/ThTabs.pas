unit ThTabs;

interface

uses
	SysUtils, Windows, Types, Classes, Controls, Graphics, StdCtrls,
	ThWebControl,	ThTextPaint, ThWebDataDictionary, ThImageList;

type
	TThTabs = class(TThWebControl)
	private
		FTabCount: Integer;
		FSelectedIndex: Integer;
		FImageList: TThImageList;
		FParams: TThWebDataDictionary;
	protected
		function GetHtmlAsString: string; override;
		function GetImages: TImageList;
		function GetImageUrl(inIndex: Integer): string; virtual;
		procedure SetParams(const Value: TThWebDataDictionary);
		procedure SetSelectedIndex(const Value: Integer);
		procedure SetTabCount(const Value: Integer);
		procedure SetImageList(const Value: TThImageList);
	protected
		procedure Paint; override;
		procedure ProcessParams;
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
		property Images: TImageList read GetImages;
	published
		property Align;
		//property HAlign;
		property ImageList: TThImageList read FImageList
			write SetImageList;
		property Params: TThWebDataDictionary read FParams write SetParams;
		property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
		property Style;
		property StyleClass;
		property TabCount: Integer read FTabCount write SetTabCount;
		property Visible;
	end;

implementation

{ TThTabs }

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
	TT_PARAM = 'tab';

procedure TThTabs.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	if (Operation = opRemove) then
		if (AComponent = FImageList) then
			FImageList := nil
		else if (AComponent = FParams) then
			FParams := nil;
end;

procedure TThTabs.Paint;
var
	iw, w, x, i, j, iis, iib, iie: Integer;
begin
	inherited;
	if Images <> nil then
	begin
//		x := 0;
		iw := Images.Width;
//		if (HAlign = haCenter) then
		begin
			w := TabCount * (BAR_SEGS + 1) * iw;
			x := (Width - w) div 2;
			if (x < 0) then
				x := 0;
			i := 0;
			while (i < x) do
			begin
				Images.Draw(Canvas, i, 0, TT_BACK);
				Inc(i, iw);
			end;
		end;
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
				Images.Draw(Canvas, x, 0, iis);
				Inc(x, iw);
			end;
			for j := 1 to BAR_SEGS do
			begin
				Images.Draw(Canvas, x, 0, iib);
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
				Images.Draw(Canvas, x, 0, iie);
				Inc(x, iw);
			end;
		end;
		while x < ClientWidth do
		begin
			Images.Draw(Canvas, x, 0, TT_BACK);
			Inc(x, iw);
		end;
	end;
end;

function TThTabs.GetImages: TImageList;
begin
	if ImageList <> nil then
		Result := ImageList.ImageList
	else
		Result := nil;
end;

procedure TThTabs.SetImageList(const Value: TThImageList);
begin
	if FImageList <> nil then
		FImageList.RemoveFreeNotification(Self);
	FImageList := Value;
	if FImageList <> nil then
		FImageList.FreeNotification(Self);
	Invalidate;
end;

procedure TThTabs.SetParams(const Value: TThWebDataDictionary);
begin
	if FParams <> nil then
		FParams.RemoveFreeNotification(Self);
	FParams := Value;
	if FParams <> nil then
	begin
		FParams.FreeNotification(Self);
		if FParams.Data.Find(TT_PARAM) = nil then
			FParams.Data.AddItem.WebName := TT_PARAM;
	end;
end;

procedure TThTabs.SetSelectedIndex(const Value: Integer);
begin
	FSelectedIndex := Value;
	Invalidate;
end;

procedure TThTabs.SetTabCount(const Value: Integer);
begin
	FTabCount := Value;
	Invalidate;
end;

procedure TThTabs.ProcessParams;
var
	d: TThDictionaryDatum;
begin
	if Params <> nil then
		if Params.Data.Find(TT_PARAM, d) then
			SelectedIndex := StrToIntDef(d.WebValue, SelectedIndex);
end;

function TThTabs.GetImageUrl(inIndex: Integer): string;
begin
	Result := ImageList.GetImageUrl(inIndex );
end;

function TThTabs.GetHtmlAsString: string;
var
	iw, ih, i, {j,} iis, iib, iie: Integer;

	function Img(inIdx: Integer; inSpan: Integer = 1; inText: string = ''): string;
	begin
		Result := Format(
			' style="background-image:url(%s); cursor:Pointer;"',
				[ GetImageUrl(inIdx) ]);
		//Result := Result + ' onclick="submit();"';
		Result := Format('%s onclick="window.location.href=''?%s=%d''"',
			[ Result, TT_PARAM, i ]);
		Result := Format('<td width="%d"%s>%s</td>',
			[ iw*inSpan, Result, inText ]);
//		Result := Format('<a href="?%s=%d">%s</a>', [ 'tab'{Name}, i, Result ]);
	end;

begin
	ProcessParams;
	Result := '';
	if Images <> nil then
	begin
		iw := Images.Width;
		ih := Images.Height;
		Result := Result + Format(
			'<div style="width:%d%s; background-image:url(%s);">'#13,
				[ 100, '%', GetImageUrl(TT_BACK) ]);
//		Result := Result + Format(
//			'<table cellspacing="0" cellpadding="0" height="%d" style="font-size:10px;">',
//				[ ih ]);
		Result := Result + Format(
			'<table cellspacing="0" cellpadding="0" height="%d"%s>'#13,
				[ ih, Style.StyleAttribute ]);
		Result := Result + '<tr>'#13;
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
				Result := Result + Img(iis);
			if (SelectedIndex = i) then
				Result := Result + Img(iib, 4, '<b>Tab ' + IntToStr(i) + '</b>')
			else
				Result := Result + Img(iib, 4, 'Tab ' + IntToStr(i));
			if iie <> -1 then
				Result := Result + Img(iie);
				Result := Result + #13;
		end;
		Result := Result + '</tr></table>'#13;
		Result := Result + '</div>'#13;
	end;
end;

end.
