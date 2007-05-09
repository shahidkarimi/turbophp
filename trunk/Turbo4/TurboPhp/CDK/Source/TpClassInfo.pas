unit TpClassInfo;

interface

uses
	Classes;

type
	TTpClassInfo = class(TStringList)
  private
		function GetDisplayName(const inClassName: string): string;
	public
		property DisplayName[const inClassName: string]: string read GetDisplayName;
	end;

function TurboClassInfo: TTpClassInfo;

implementation

var
	SingletonClassInfo: TTpClassInfo;

function TurboClassInfo: TTpClassInfo;
begin
	if SingletonClassInfo = nil then
		SingletonClassInfo := TTpClassInfo.Create;
	Result := SingletonClassInfo;
end;

{ TTpClassInfo }

function TTpClassInfo.GetDisplayName(const inClassName: string): string;
var
	i: Integer;
begin
	i := IndexOfName(inClassName);
	if (i < 0) then
		Result := inClassName
	else
		Result := ValueFromIndex[i];
end;

end.
