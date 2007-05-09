unit TpCheckBox;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThTag, ThCheckBox,
	TpControls;

type
	TTpCheckBox = class(TThCheckBox)
  private
    FOnClick: TTpEvent;
    FOnGenerate: TTpEvent;
	published
		procedure Tag(inTag: TThTag); override;
		property OnClick: TTpEvent read FOnClick write FOnClick;
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
	end;
	//
	TTpRadio = class(TThRadio)
	private
		FOnClick: TTpEvent;
		FOnGenerate: TTpEvent;
	published
		procedure Tag(inTag: TThTag); override;
		property OnClick: TTpEvent read FOnClick write FOnClick;
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
	end;

implementation

{ TTpCheckBox }

procedure TTpCheckBox.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpCheckBox');
		Add('tpName', Name);
		Add('tpOnGenerate', OnGenerate);
		Add('tpOnClick', OnClick);
	end;
end;

{ TTpRadio }

procedure TTpRadio.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpRadio');
		Add('tpName', Name);
		Add('tpOnGenerate', OnGenerate);
		Add('tpOnClick', OnClick);
	end;
end;

end.
