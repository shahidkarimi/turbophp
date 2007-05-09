unit Text;

interface

uses
	Graphics;

function GetWordBreak(inCanvas: TCanvas; inWidth: Integer;
	const inText: string): Integer;

implementation

function GetWordBreak(inCanvas: TCanvas; inWidth: Integer;
	const inText: string): Integer;
var
	w, i: Integer;
	s: string;
begin
	s := inText;
	w := inCanvas.TextWidth(s);
	if (w <= inWidth) then
		Result := -1
	else begin
		i := 0;
		while (w > inWidth) do
		begin
			i := Length(s);
			while (i > 0) and (s[i] <> ' ') do //(Pos(s[i], '- ') < 0) do
				Dec(i);
			if (i = 0) then
				break
			else begin
				s := Copy(s, 1, i - 1);
				w := inCanvas.TextWidth(s);
			end;
		end;
		Result := i;
	end;
end;

end.
