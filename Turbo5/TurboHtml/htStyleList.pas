unit htStyleList;

interface

uses
	SysUtils, Classes, Graphics;

type
	ThtStyleList = class(TStringList)
	private
		FExtraStyles: string;
	protected
		function GetHtmlStyles: string;
		function GetStyleAttribute: string;
		function GetThisList: ThtStyleList;
	public
		procedure Add(const inName, inValue: string;
			const inUnits: string = ''); reintroduce; overload;
		procedure Add(const inName: string;	inValue: Integer;
			const inUnits: string = ''); reintroduce; overload;
		procedure AddIf(inIf: Boolean; const inName: string; inValue: Integer;
			const inUnits: string = '');
		procedure AddIfNonZero(const inName: string; inValue: Integer;
			const inUnits: string = '');
		procedure AddColor(const inName: string;
			inValue: TColor);
		property ExtraStyles: string read FExtraStyles write FExtraStyles;
		property HtmlStyles: string read GetHtmlStyles;
		property StyleAttribute: string read GetStyleAttribute;
		property ThisList: ThtStyleList read GetThisList;
	end;

implementation

uses
	htUtils;

{ ThtStyleList }

procedure ThtStyleList.Add(const inName, inValue: string;
	const inUnits: string = '');
begin
	if inValue <> '' then
		Add(inName + '=' + inValue + inUnits);
end;

procedure ThtStyleList.Add(const inName: string; inValue: Integer;
	const inUnits: string);
begin
	Add(inName, IntToStr(inValue), inUnits);
end;

procedure ThtStyleList.AddColor(const inName: string; inValue: TColor);
begin
	if htVisibleColor(inValue) then
		Add(inName, htColorToHtml(inValue));
end;

procedure ThtStyleList.AddIf(inIf: Boolean; const inName: string;
	inValue: Integer; const inUnits: string);
begin
	if inIf then
		Add(inName, inValue, inUnits);
end;

procedure ThtStyleList.AddIfNonZero(const inName: string; inValue: Integer;
	const inUnits: string);
begin
	if inValue <> 0 then
		Add(inName, inValue, inUnits);
end;

function ThtStyleList.GetHtmlStyles: string;
var
	i: Integer;
begin
	Result := '';
	for i := 0 to Pred(Count) do
		Result := Result + Names[i] + ':' + Values[Names[i]] + '; ';
	Result := Trim(Result + ExtraStyles);
end;

function ThtStyleList.GetStyleAttribute: string;
begin
	Result := GetHtmlStyles;
	if Result <> '' then
		Result := ' style="' + Result + '"';
end;

function ThtStyleList.GetThisList: ThtStyleList;
begin
	Result := Self;
end;

end.

