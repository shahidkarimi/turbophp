unit EasyEditState;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
	EasyClasses, EasyParser, EasyEditor, EasyEditSource;

type
	TEasyEditState = class
	public
		WindowChar: Integer;
		WindowLine: Integer;
		Cursor: TPoint;
		procedure GetState(inEdit: TEasyEdit);
		procedure SetState(inEdit: TEasyEdit);
	end;

implementation

{ TEasyEditState }

procedure TEasyEditState.GetState(inEdit: TEasyEdit);
begin
	WindowChar := inEdit.WindowChar;
	WindowLine := inEdit.WindowLine;
	Cursor := inEdit.CurrentPosition;
end;

procedure TEasyEditState.SetState(inEdit: TEasyEdit);
begin
	inEdit.WindowChar := WindowChar;
	inEdit.WindowLine := WindowLine;
	inEdit.CurrentPosition := Cursor;
end;

end.
 