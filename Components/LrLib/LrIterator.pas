unit LrIterator;

interface

uses
	Classes, Controls;

type
	TLrIterator = class
	private
		FIndex: Integer;
	public
		function Eof: Boolean; virtual; abstract;
		function Next: Boolean; overload; virtual;
		procedure Reset;
	public
		property Index: Integer read FIndex write FIndex;
	end;

implementation

{ TLrIterator }

procedure TLrIterator.Reset;
begin
	Index := 0;
end;

function TLrIterator.Next: Boolean;
begin
	Result := not Eof;
	if Result then
		Inc(FIndex)
	else
		Reset;
end;

end.
