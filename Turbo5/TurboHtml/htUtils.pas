unit htUtils;

interface

uses
	SysUtils, Graphics;

function htColorToHtml(const Color: TColor): string;
function htVisibleColor(inColor: TColor): Boolean;
function htPercValue(inPerc: Integer): string;
function htPxValue(inPx: Integer): string;
function htStyleProp(const inProp, inValue: string): string; overload;
function htStyleProp(const inProp: string; inColor: TColor): string; overload;
function htInlineStylesToAttribute(const inStyles: string): string;
function htForwardSlashes(const inString: string): string;
procedure htCat(var ioDst: string; const inAdd: string);

implementation

const
	chtPerc = '%';
	chtPx = 'px';

function htColorToHtml(const Color: TColor): string;
var
	c: TColor;
begin
	c := ColorToRGB(Color);
	Result := '#' +
		IntToHex(Byte(c), 2) +
		IntToHex(Byte(c shr 8), 2) +
		IntToHex(Byte(c shr 16), 2);
end;

function htInlineStylesToAttribute(const inStyles: string): string;
begin
	if inStyles = '' then
		Result := ''
	else
		Result := ' style="' + inStyles + '"';
end;

function htVisibleColor(inColor: TColor): Boolean;
begin
	Result := (inColor <> clNone) and (inColor <> clDefault);
end;

function htStyleProp(const inProp, inValue: string): string; overload;
begin
	if inValue <> '' then
		Result := inProp + ':' + inValue + ';'
	else
		Result := '';
end;

function htStyleProp(const inProp: string; inColor: TColor): string; overload;
begin
	if htVisibleColor(inColor) then
		Result := inProp + ':' + htColorToHtml(inColor) + ';';
end;

function htPercValue(inPerc: Integer): string;
begin
	Result := IntToStr(inPerc) + chtPerc;
end;

function htPxValue(inPx: Integer): string;
begin
	Result := IntToStr(inPx) + chtPx;
end;

procedure htCat(var ioDst: string; const inAdd: string);
begin
	if (inAdd <> '') then
		if (ioDst = '') then
			ioDst := inAdd
		else
			ioDst := ioDst + ' ' + inAdd;
end;

function htForwardSlashes(const inString: string): string;
begin
	Result := StringReplace(inString, '\', '/', [ rfReplaceAll ]);
end;

end.
