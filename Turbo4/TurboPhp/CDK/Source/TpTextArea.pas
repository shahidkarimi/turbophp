unit TpTextArea;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThTag, ThTextArea,
	TpControls;

type
	TTpTextArea = class(TThTextArea)
	private
		FOnGenerate: TTpEvent;
	protected
		procedure SetOnGenerate(const Value: TTpEvent);
	public
		procedure Tag(inTag: TThTag); override;
	published
		property OnGenerate: TTpEvent read FOnGenerate write SetOnGenerate;
	end;

implementation

{ TTpTextArea }

procedure TTpTextArea.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpTextArea');
		Add('tpName', Name);
		Add('tpOnGenerate', OnGenerate);
	end;
end;

procedure TTpTextArea.SetOnGenerate(const Value: TTpEvent);
begin
	FOnGenerate := Value;
end;

end.
