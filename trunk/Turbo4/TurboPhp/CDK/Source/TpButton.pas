unit TpButton;

interface

uses
	Windows, SysUtils, Classes, Controls, Graphics,	Types,
	ThWebControl, ThTag, ThButton,
	TpControls;

type
	TTpButton = class(TThButton)
	private
		FOnClick: TTpEvent;
		FOnGenerate: TTpEvent;
	protected
		procedure Tag(inTag: TThTag); override;
	published
		property OnClick: TTpEvent read FOnClick write FOnClick;
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
	end;

implementation

procedure TTpButton.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		//Add('tpname', Name);
		Add(tpClass, 'TTpButton');
		Add('tpOnClick', OnClick);
		Add('tpOnGenerate', OnGenerate);
	end;
end;

end.
