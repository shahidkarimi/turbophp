unit ThAttributeList;

interface

uses
	SysUtils, Classes, Graphics;

type
	TThAttributeList = class(TStringList)
	private
		function GetThisList: TThAttributeList;
		function GetAttribute(const inName: string): string;
		procedure SetAttribute(const inName, Value: string);
	protected
		function GetHtmlAttributes: string;
	public
		procedure Add(const inName, inValue: string); reintroduce; overload;
		procedure Add(const inName: string;
			inValue: Integer); reintroduce; overload;
		procedure Add(const inName: string;
			inValue: Boolean); reintroduce; overload;
		procedure AddColor(const inName: string;
			inValue: TColor);
		procedure Remove(const inName: string);
	public
		property Attribute[const inName: string]: string read GetAttribute
			write SetAttribute; default;
		property HtmlAttributes: string read GetHtmlAttributes;
		property ThisList: TThAttributeList read GetThisList;
	end;

implementation

uses
	ThCssStyle;

{ TThAttributeList }

procedure TThAttributeList.Add(const inName, inValue: string);
begin
	if inValue <> '' then
		Add(inName + '="' + inValue + '"');
end;

procedure TThAttributeList.Add(const inName: string; inValue: Integer);
begin
	Add(inName, IntToStr(inValue));
end;

procedure TThAttributeList.Add(const inName: string; inValue: Boolean);
begin
	if inValue then
		Add(inName, inName);
end;

procedure TThAttributeList.AddColor(const inName: string; inValue: TColor);
begin
	if ThVisibleColor(inValue) then
		Add(inName, ThColorToHtml(inValue));
end;

function TThAttributeList.GetAttribute(const inName: string): string;
begin
	if IndexOfName(inName) < 0 then
		Result := ''
	else
		Result := Values[inName];
end;

function TThAttributeList.GetHtmlAttributes: string;
var
	i: Integer;
begin
	Result := '';
	for i := 0 to Pred(Count) do
		Result := Result + ' ' + Strings[i];
end;

function TThAttributeList.GetThisList: TThAttributeList;
begin
	Result := Self;
end;

procedure TThAttributeList.Remove(const inName: string);
var
	i: Integer;
begin
	i := IndexOfName(inName);
	if i >= 0 then
		Delete(i);
end;

procedure TThAttributeList.SetAttribute(const inName, Value: string);
begin
	if IndexOfName(inName) < 0 then
		Add(inName, '');
	Values[inName] := '"' + Value + '"';
end;

end.
