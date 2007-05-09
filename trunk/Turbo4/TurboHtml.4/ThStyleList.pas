unit ThStyleList;

interface

uses
	SysUtils, Classes, Graphics;

type
	TThStyleList = class(TStringList)
	private
		FExtraStyles: string;
	protected
		function GetHtmlStyles: string;
		function GetStyleAttribute: string;
		function GetThisList: TThStyleList;
	public
		procedure Add(const inName, inValue: string;
			const inUnits: string = ''); reintroduce; overload;
		procedure Add(const inName: string;	inValue: Integer;
			const inUnits: string = ''); reintroduce; overload;
		procedure AddIf(inIf: Boolean; const inName: string; inValue: Integer;
			const inUnits: string = '');
		procedure AddIfNotZero(const inName: string; inValue: Integer;
			const inUnits: string = '');
		procedure AddColor(const inName: string;
			inValue: TColor);
		property ExtraStyles: string read FExtraStyles write FExtraStyles;
		property HtmlStyles: string read GetHtmlStyles;
		property StyleAttribute: string read GetStyleAttribute;
		property ThisList: TThStyleList read GetThisList;
	end;

implementation

uses
	ThCssStyle;

{ TThStyleList }

procedure TThStyleList.Add(const inName, inValue: string;
	const inUnits: string = '');
begin
	if inValue <> '' then
		Add(inName + '=' + inValue + inUnits);
end;

procedure TThStyleList.Add(const inName: string; inValue: Integer;
	const inUnits: string);
begin
	Add(inName, IntToStr(inValue), inUnits);
end;

procedure TThStyleList.AddColor(const inName: string; inValue: TColor);
begin
	if ThVisibleColor(inValue) then
		Add(inName, ThColorToHtml(inValue));
end;

procedure TThStyleList.AddIf(inIf: Boolean; const inName: string;
	inValue: Integer; const inUnits: string);
begin
	if inIf then
		Add(inName, inValue, inUnits);
end;

procedure TThStyleList.AddIfNotZero(const inName: string; inValue: Integer;
	const inUnits: string);
begin
	if inValue <> 0 then
		Add(inName, inValue, inUnits);
end;

function TThStyleList.GetHtmlStyles: string;
var
	i: Integer;
begin
	Result := '';
	for i := 0 to Pred(Count) do
		Result := Result + Names[i] + ':' + Values[Names[i]] + '; ';
	Result := Trim(Result + ExtraStyles);
end;

function TThStyleList.GetStyleAttribute: string;
begin
	Result := GetHtmlStyles;
	if Result <> '' then
		Result := ' style="' + Result + '"';
end;

function TThStyleList.GetThisList: TThStyleList;
begin
	Result := Self;
end;

end.

