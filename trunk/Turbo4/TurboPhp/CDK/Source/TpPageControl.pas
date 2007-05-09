unit TpPageControl;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types, Forms,
	ThTag, ThPageControl,
	TpControls, TpInterfaces;

type
	TTpSheet = class(TThSheet)
	private
		FOnGenerate: TTpEvent;
	public
		procedure CellTag(inTag: TThTag); override;
	published
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
	end;
	//
	TTpPageControl = class(TThPageControl, ITpJsWriter)
	private
		FOnGenerate: TTpEvent;
	protected
		function GetHtmlAsString: string; override;
		procedure WriteJavaScript(inScript: TStringList); 
	public
		procedure CellTag(inTag: TThTag); override;
		function CreateSheet: TThSheet; override;
	published
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
	end;

implementation

{ TTpSheet }

procedure TTpSheet.CellTag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpSheet');
		Add('tpName', Name);
		Add('tpOnGenerate', OnGenerate);
	end;
end;

{ TTpPageControl }

function TTpPageControl.CreateSheet: TThSheet;
begin
	Result := TTpSheet.Create(Owner);
	Result.Name := TCustomForm(Owner).Designer.UniqueName('TpSheet');
	Result.Align := alTop;
	Result.Parent := Self;
end;

procedure TTpPageControl.CellTag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpPageControl');
		Add('tpName', Name);
		Add('tpCount', Count);
		Add('tpOnGenerate', OnGenerate);
	end;
end;

function TTpPageControl.GetHtmlAsString: string;
begin
	Result := inherited GetHtmlAsString;
{
	with TThTag.Create('script') do
	try
		Add('language', 'javascript');
		Add('type', 'text/javascript');
		Content :=
#13 +
'function ' + Name + 'SelectSheet(inIndex)'#13 +
'{'#13 +
'  i = 0;'#13 +
'  while (true)'#13 +
'  {'#13 +
'		i++;'#13 +
'		sheet = document.getElementById(''' + GetSheetNamePrefix + ''' + i);'#13 +
'		if (!sheet)'#13 +
'			break;'#13 +
'  	sheet.style.display = (i==inIndex ? ''block'' : ''none'');'#13 +
}
//'  }'#13 +
//'}'#13
{
;
		Result := Html + Result;
	finally
		Free;
	end;
}
end;

procedure TTpPageControl.WriteJavaScript(inScript: TStringList);
begin
	with inScript do
	begin
		Add('function TpPageControlSelectSheet(inPageControl, inIndex)');
//		Add('function ' + Name + 'SelectSheet(inIndex)');
		Add('{');
		Add('  pages = document.getElementById(inPageControl);');
		Add('  if (!pages)');
		Add('    exit;');
		Add('  sheetPrefix = inPageControl + "Sheet_";');
		Add('  for (i=1; true; i++)');
		Add('  {');
		Add('    sheet = document.getElementById(sheetPrefix + i);');
		Add('    if (!sheet)');
		Add('      break;');
		Add('    sheet.style.display = (i==inIndex ? "" : "none");');
		Add('  }');
		Add('}');
	end;
end;

initialization
	RegisterClass(TTpSheet);
end.
