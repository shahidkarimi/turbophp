unit TpInput;

interface

uses
	Windows, SysUtils, Classes, Controls, Graphics,	Types,
	ThWebControl, ThTag, ThInput,
	TpControls;

type
	TTpInput = class(TThCustomInput)
	private
		FOnInput: TTpEvent;
		FOnGenerate: TTpEvent;
	protected
		procedure Tag(inTag: TThTag); override;
	published
		//property AccessKey;
		property Align;
		property AutoComplete;
		property AutoSize;
		property Enabled;
		//property JavaScript;
		property MaxLength;
		//property OnBeforeGenerate;
		property OnInput: TTpEvent read FOnInput write FOnInput;
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
		property Password;
		property Size;
		property Style;
		property StyleClass;
		//property TabIndex;
		property Text;
	end;

implementation

procedure TTpInput.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpInput');
		Add('tpName', Name);
		Add('tpOnSubmit', OnInput);
		Add('tpOnGenerate', OnGenerate);
	end;
end;

end.
