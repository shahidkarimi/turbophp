unit ClassInfo;

interface

uses
	Classes;

type
	TComponentRegistry = class(TStringList)
	private
		function GetDisplayName(const inClassName: string): string;
		procedure SetDisplayName(const inClassName: string; const Value: string);
	public
		procedure RegisterComponents(const inPage: string;
			const inComponentClasses: array of TComponentClass;
			const inDisplayNames: array of string);
		property DisplayName[const inClassName: string]: string read GetDisplayName
			write SetDisplayName;
	end;

function ComponentRegistry: TComponentRegistry;

implementation

var
	SingletonComponentRegistry: TComponentRegistry;

function ComponentRegistry: TComponentRegistry;
begin
	if SingletonComponentRegistry = nil then
		SingletonComponentRegistry := TComponentRegistry.Create;
	Result := SingletonComponentRegistry;
end;

{ TComponentRegistry }

function TComponentRegistry.GetDisplayName(const inClassName: string): string;
var
	i: Integer;
begin
	i := IndexOfName(inClassName);
	if (i < 0) then
		Result := inClassName
	else
		Result := ValueFromIndex[i];
end;

procedure TComponentRegistry.SetDisplayName(const inClassName: string;
	const Value: string);
begin
	Values[inClassName] := Value;
end;

procedure TComponentRegistry.RegisterComponents(const inPage: string;
	const inComponentClasses: array of TComponentClass;
	const inDisplayNames: array of string);
var
	i: Integer;
begin
	Classes.RegisterComponents(inPage, inComponentClasses);
	for i := 0 to Pred(Length(inComponentClasses)) do
		DisplayName[inComponentClasses[i].ClassName] := inDisplayNames[i];
end;

end.
