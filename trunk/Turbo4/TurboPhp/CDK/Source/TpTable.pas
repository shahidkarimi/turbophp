unit TpTable;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThWebControl,
	TpControls;

type
	TTpTable = class(TThWebControl)
	private
		FColCount: Integer;
		FRowCount: Integer;
		FOnCell: TTpEvent;
    FBorder: Integer;
    FOnGenerate: TTpEvent;
    procedure SetOnGenerate(const Value: TTpEvent);
	protected
    procedure SetBorder(const Value: Integer);
		procedure SetColCount(const Value: Integer);
		procedure SetOnCell(const Value: TTpEvent);
		procedure SetRowCount(const Value: Integer);
	public
		constructor Create(AOwner: TComponent); override;
		function GetHtmlAsString: string; override;
		procedure Paint; override;
	published
		property Align;
		property Border: Integer read FBorder write SetBorder;
		property ColCount: Integer read FColCount write SetColCount default 3;
		property OnCell: TTpEvent read FOnCell write SetOnCell;
		property OnGenerate: TTpEvent read FOnGenerate write SetOnGenerate;
		property RowCount: Integer read FRowCount write SetRowCount default 3;
		property Style;
		property StyleClass;
		property Visible;
	end;

implementation

constructor TTpTable.Create(AOwner: TComponent);
begin
	inherited;
	FColCount := 3;
	FRowCount := 3;
end;

procedure TTpTable.Paint;
begin
	inherited;
	ThPaintOutline(Canvas, AdjustedClientRect, clBlue, psDot);
end;

function TTpTable.GetHtmlAsString: string;
begin
	Result :=
		'<table'
		+ TpAttr('border', Border)
		+ TpAttr(tpClass, 'TTpTable')
		+ TpAttr('tpName', Name)
		+ TpAttr('tpCols', ColCount)
		+ TpAttr('tpRows', RowCount)
		+ TpAttr('tpOnGenerate', OnGenerate)
		+ TpAttr('tpOnCell', OnCell)
		+ CtrlStyle.StyleAttribute
		+ '>'#13
		+ '  <tr><td>Dynamic Table</td></tr>'#13
		+ '</table>'#13
		;
end;

procedure TTpTable.SetColCount(const Value: Integer);
begin
	FColCount := Value;
end;

procedure TTpTable.SetOnCell(const Value: TTpEvent);
begin
	FOnCell := Value;
end;

procedure TTpTable.SetRowCount(const Value: Integer);
begin
	FRowCount := Value;
end;

procedure TTpTable.SetBorder(const Value: Integer);
begin
	FBorder := Value;
end;

procedure TTpTable.SetOnGenerate(const Value: TTpEvent);
begin
	FOnGenerate := Value;
end;

end.
