unit TpPanel;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThTag, ThAttributeList, ThPanel,
	TpControls;

type
	TTpPanel = class(TThPanel)
	private
		FHidden: Boolean;
		FOnGenerate: TTpEvent;
	public
		procedure CellTag(inTag: TThTag); override;
	published
		property Hidden: Boolean read FHidden write FHidden default false;
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
	end;

implementation

{ TTpPanel }

procedure TTpPanel.CellTag(inTag: TThTag);
begin
	inherited;
	inTag.Add(tpClass, 'TTpPanel');
	inTag.Add('tpName', Name);
	inTag.Attributes.Add('tpHidden', Hidden);
	inTag.Add('tpOnGenerate', OnGenerate);
end;

end.
