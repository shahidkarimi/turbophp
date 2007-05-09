unit ThCellPanel;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThWebControl, ThTag, ThPanel;

type
	TThCellPanel = class(TThCustomPanel)
	protected
		procedure AlignCells(inAlign: TAlign; inLevel: Integer;
			var ioRect: TRect);
//		procedure Notification(AComponent: TComponent;
//			Operation: TOperation); override;
		procedure Tag(inTag: TThTag); override;
	public
		constructor Create(inOwner: TComponent); override;
		procedure AlignControls(AControl: TControl; var Rect: TRect); override;
	published
		property Align;
		property DesignOpaque;
		property ShowGrid;
		property ShowOutline default true;
		property Style;
		property StyleClass;
		property Visible;
	end;

implementation

uses
	ThComponentIterator;

{ TThCellPanel }

constructor TThCellPanel.Create(inOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle + [ csAcceptsControls ];
end;

procedure TThCellPanel.Tag(inTag: TThTag);
begin
	inherited;
end;

procedure AlignCell(inCtrl: TControl; var ioRect: TRect);
begin
	with inCtrl do
		case Align of
			alTop:
			begin
				Left := ioRect.Left;
				Width := ioRect.Right;
				Top := ioRect.Top;
				Inc(ioRect.Top, Height);
				Dec(ioRect.Bottom, Height);
			end;
			//
			alBottom:
			begin
				Left := ioRect.Left;
				Width := ioRect.Right;
				Dec(ioRect.Bottom, Height);
				Top := ioRect.Top + ioRect.Bottom;
			end;
			//
			alLeft:
			begin
				Top := ioRect.Top;
				Height := ioRect.Bottom;
				Left := ioRect.Left;
				if (Width > ioRect.Right) then
					Width := ioRect.Right;
				Inc(ioRect.Left, Width);
				Dec(ioRect.Right, Width);
			end;
			//
			alRight:
			begin
				Top := ioRect.Top;
				Height := ioRect.Bottom;
				Dec(ioRect.Right, Width);
				if (Width > ioRect.Right) then
					Width := ioRect.Right;
				Left := ioRect.Left + ioRect.Right;
			end;
		end;
end;

procedure TThCellPanel.AlignCells(inAlign: TAlign; inLevel: Integer;
	var ioRect: TRect);
begin
	with TThSortedCtrlIterator.Create(Self, inAlign) do
	try
		while Next([inAlign]) do
			if (Ctrl.Tag = inLevel) then
				AlignCell(Ctrl, ioRect);
	finally
		Free;
	end;
end;

procedure TThCellPanel.AlignControls(AControl: TControl; var Rect: TRect);
var
	mn, mx: Integer;
	r: TRect;
	l: Integer;
begin
	mn := 0;
	mx := 0;
	//
	with TThCtrlIterator.Create(Self) do
	try
		while Next do
			if (Ctrl.Tag < mn) then
				mn := Ctrl.Tag
			else if (Ctrl.Tag > mx) then
				mx := Ctrl.Tag;
	finally
		Free;
	end;
	//
	r := Rect;
	//
	for l := mn to mx do
	begin
		AlignCells(alTop, l, r);
		AlignCells(alBottom, l, r);
		AlignCells(alLeft, l, r);
		AlignCells(alRight, l, r);
	end;
end;

end.
