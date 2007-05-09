unit htAttributeList;

interface

uses
	SysUtils, Classes, Graphics;

type
	ThtAttributeList = class(TStringList)
	private
		function GetThisList: ThtAttributeList;
		function GetAttribute(const inName: string): string;
		procedure SetAttribute(const inName, Value: string);
		procedure SetBoolean(const inName: string; const Value: Boolean);
		procedure SetColor(const inName: string; const Value: TColor);
		procedure SetInteger(const inName: string; const Value: Integer);
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
		property Color[const inName: string]: TColor write SetColor;
		property Boolean[const inName: string]: Boolean write SetBoolean;
		property Integer[const inName: string]: Integer write SetInteger;
		property HtmlAttributes: string read GetHtmlAttributes;
		property ThisList: ThtAttributeList read GetThisList;
	end;

implementation

uses
	htUtils;

{ ThtAttributeList }

procedure ThtAttributeList.SetAttribute(const inName, Value: string);
begin
//	if IndexOfName(inName) < 0 then
//		Add(inName, '');
//	Values[inName] := '"' + Value + '"';
	Values[inName] := Value;
end;

procedure ThtAttributeList.SetBoolean(const inName: string;
	const Value: Boolean);
begin
	if Value then
		Values[inName] := inName;
end;

procedure ThtAttributeList.SetColor(const inName: string;
	const Value: TColor);
begin
	if htVisibleColor(Value) then
		Values[inName] := htColorToHtml(Value)
	else
		Values[inName] := '';
end;

procedure ThtAttributeList.SetInteger(const inName: string;
	const Value: Integer);
begin
	Values[inName] := IntToStr(Value);
end;

procedure ThtAttributeList.Add(const inName, inValue: string);
begin
	if inValue <> '' then
		Attribute[inName] := inValue;
end;

procedure ThtAttributeList.Add(const inName: string; inValue: Integer);
begin
	Add(inName, IntToStr(inValue));
end;

procedure ThtAttributeList.Add(const inName: string; inValue: Boolean);
begin
	if inValue then
		Add(inName, inName);
end;

procedure ThtAttributeList.AddColor(const inName: string; inValue: TColor);
begin
	if htVisibleColor(inValue) then
		Add(inName, htColorToHtml(inValue));
end;

procedure ThtAttributeList.Remove(const inName: string);
//var
//	i: Integer;
begin
	Values[inName] := '';
//	i := IndexOfName(inName);
//	if i >= 0 then
//		Delete(i);
end;

function ThtAttributeList.GetAttribute(const inName: string): string;
begin
//	if IndexOfName(inName) < 0 then
//		Result := ''
//	else
//		Result := Values[inName];
	Result := Values[inName];
end;

function ThtAttributeList.GetHtmlAttributes: string;
var
	i: System.Integer;
begin
//	Result := '';
//	for i := 0 to Pred(Count) do
//		Result := Result + ' ' + Strings[i];
	Result := '';
	for i := 0 to Pred(Count) do
		Result := Result + ' ' + Names[i] + '="' + ValueFromIndex[i] + '"';
end;

function ThtAttributeList.GetThisList: ThtAttributeList;
begin
	Result := Self;
end;

end.
