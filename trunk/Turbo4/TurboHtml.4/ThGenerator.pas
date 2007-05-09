unit ThGenerator;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThCellSorter, ThComponentIterator, ThTag, ThInterfaces, ThWebControl;

type
	TThGenerator = class(TThSortedCtrlIterator)
	private
		FMargins: TRect;
		FTable: TCellTable;
	protected
		function GetClientAreaHeight: Integer;
		function GetClientAreaWidth: Integer;
		function GetHtml: string;
		procedure SetContainer(const Value: TWinControl); override;
	protected
		function AdjustCellRect(const inRect: TRect): TRect;
		procedure FillTable;
		procedure FindMargins;
		function GenerateCell(inCellData: TCellData): string;
		function GenerateCells: string;
		function GenerateClient: string;
		function GenerateClientBand: string;
		function GenerateCtrlCell: string;
		procedure GenerateCtrlTag(inTag: TThTag);
		function GenerateLeftRight(inAlign: TAlign): string;
		function GenerateLeftRightCell: string;
		function GenerateRowTable(const inContent: string;
			const inTableAttrs: string = ''; const inRowAttrs: string = ''): string;
		function GenerateTable(const inContent: string;
			inAttrs: string = ''): string;
		function GenerateTableHtml: string;
		function GenerateTopBottom(inAlign: TAlign): string;
		function GenerateTopBottomCell: string;
	public
		property Html: string read GetHtml;
		property Margins: TRect read FMargins write FMargins;
	end;

implementation

uses
	ThVclUtils;

const
//	cBorder = ' border="0"';
//	cBorder = ' border="1"';
	cBorder = '';
	cTableAttributes = ' border="0" cellpadding="0" cellspacing="0"';
	cTableFormat = '<table%s' + cTableAttributes + '>'#13'%s'#13'</table>';

procedure TThGenerator.FindMargins;
begin
	FMargins := Rect(AlignWidth([alLeft]), AlignHeight([alTop]),
		AlignWidth([alRight]), AlignHeight([alBottom]));
	if Container is TThWebControl then
		TThWebControl(Container).AdjustMargins(FMargins);
end;

function TThGenerator.GetClientAreaWidth: Integer;
begin
	Result := Container.ClientWidth;
	Dec(Result, FMargins.Right + FMargins.Left);
end;

function TThGenerator.GetClientAreaHeight: Integer;
begin
	Result := Container.ClientHeight;
	Dec(Result, FMargins.Top + FMargins.Bottom);
end;

procedure TThGenerator.SetContainer(const Value: TWinControl);
begin
	inherited;
	FindMargins;
end;

function TThGenerator.GetHtml: string;
begin
	Result := GenerateTopBottom(alTop)
		+ GenerateClientBand
		+ GenerateTopBottom(alBottom);
end;

procedure TThGenerator.GenerateCtrlTag(inTag: TThTag);
var
	s: IThHtmlSource;
begin
	if ThIsAs(Ctrl, IThHtmlSource, s) then
	begin
		inTag.Content := s.Html;
		s.CellTag(inTag);
	end;
end;

function TThGenerator.GenerateCtrlCell: string;
begin
	with TThTag.Create('td') do
	try
		GenerateCtrlTag(ThisTag);
		Result := Html;
	finally
		Free;
	end;
end;

function TThGenerator.GenerateTable(const inContent: string;
	inAttrs: string = ''): string;
begin
//	Result :=	Format('<table width="100%%" %s>'#13'%s'#13'</table>',
//		[ cTableAttributes, inContent ]);
//	Result :=	Format(STable, [ ' width="100%%"', inContent ]);
	Result :=	Format(cTableFormat, [ inAttrs, inContent ]);
end;

function TThGenerator.GenerateRowTable(const inContent: string;
	const inTableAttrs: string = ''; const inRowAttrs: string = ''): string;
begin
//	Result := GenerateTable(' width="100%%"',
//		'<tr' + inRowAttrs + '>'#13 + inContent + #13'</tr>');
	Result := GenerateTable('<tr' + inRowAttrs + '>'#13 + inContent + #13'</tr>',
		inTableAttrs);
end;

function TThGenerator.GenerateTopBottomCell: string;
begin
	Result := GenerateRowTable(GenerateCtrlCell, ' width="100%"');
end;

function TThGenerator.GenerateTopBottom(inAlign: TAlign): string;
begin
	Result := '';
	while Next([ inAlign ]) do
		Result := Result + GenerateTopBottomCell;
end;

function TThGenerator.GenerateLeftRightCell: string;
begin
	Result := GenerateCtrlCell;
end;

function TThGenerator.GenerateLeftRight(inAlign: TAlign): string;
begin
	Result := '';
	while Next([ inAlign ]) do
		Result := Result + GenerateLeftRightCell;
end;

function TThGenerator.GenerateClientBand: string;
begin
	if CountAligns([alClient, alNone, alLeft, alRight]) = 0 then
		Result := ''
	else begin
		if CountAligns([alClient, alNone]) = 0 then
			Result := '<td>&nbsp;</td>'
		else
			Result := GenerateClient;
		if CountAligns([alLeft, alRight]) > 0 then
		begin
			Result :=
					GenerateLeftRight(alLeft)
				+ Result
				+	GenerateLeftRight(alRight);
		end;
//		Result :=	Format(STable,
//			[' width="100%%" height="100%%" name="ClientBand"', Result ]);
		Result := GenerateRowTable(Result, ' width="100%" height="100%"',
			' name="ClientBand" valign="top"');
	end;
end;

function TThGenerator.GenerateClient: string;
begin
	if Next([ alClient ]) then
	begin
		Result := GenerateCtrlCell;
		Reset;
	end else
		Result := '<td>' + GenerateCells + '</td>';
end;

function TThGenerator.GenerateCells: string;
begin
	FTable := TCellTable.Create;
	try
		FillTable;
		FTable.Generate(GetClientAreaWidth,	GetClientAreaHeight);
		Result := GenerateTableHtml;
	finally
		FreeAndNil(FTable);
	end;
end;

function TThGenerator.AdjustCellRect(const inRect: TRect): TRect;
begin
	Result := inRect;
	OffsetRect(Result, -FMargins.Left, -FMargins.Top);
end;

procedure TThGenerator.FillTable;
begin
	while Next([alNone]) do
		if Ctrl.Visible then
			FTable.AddControl(Ctrl, AdjustCellRect(Ctrl.BoundsRect), Index);
end;

function TThGenerator.GenerateTableHtml: string;
var
	s: TStringList;
	i, j, c: Integer;
begin
	s := TStringList.Create;
	try
		s.Add('<table' + cTableAttributes + ' name="ClientCell"'	+ '>');
		for j := 0 to FTable.RowCount - 1 do
		begin
			s.Add('<tr valign="top">');
			c := Length(FTable.Rows[j]);
			for i := 0 to c - 1 do
				s.Add(GenerateCell(FTable.Rows[j][i]));
			s.Add('</tr>');
		end;
		s.Add('</table>');
		Result := s.Text;
	finally
		s.Free;
	end;
end;

function TThGenerator.GenerateCell(inCellData: TCellData): string;
begin
	Index := inCellData.Index;
	with inCellData do
	begin
		with TThTag.Create('td') do
		try
			if (ColSpan > 1) then
				Add('colspan', ColSpan);
			if (RowSpan > 1) then
				Add('rowspan', RowSpan);
			if (Index >= 0) then
				GenerateCtrlTag(ThisTag)
			else begin
				Add('width', W);
				Add('height', H);
			end;
			Result := Html;
		finally
			Free;
		end;
	end;
end;

end.

