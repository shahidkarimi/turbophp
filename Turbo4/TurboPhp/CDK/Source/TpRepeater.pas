unit TpRepeater;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThTag, ThPanel,
	TpControls;

type
	TTpRepeater = class(TThPanel)
	private
		FOnGenerate: TTpEvent;
		FCount: Integer;
		FOnRepeat: TTpEvent;
	protected
		procedure SetCount(const Value: Integer);
	public
		procedure CellTag(inTag: TThTag); override;
	published
		property Count: Integer read FCount write SetCount;
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
		property OnRepeat: TTpEvent read FOnRepeat write FOnRepeat;
	end;

implementation

{ TTpRepeater }

procedure TTpRepeater.SetCount(const Value: Integer);
begin
	FCount := Value;
end;

procedure TTpRepeater.CellTag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpRepeater');
		Add('tpName', Name);
		Add('tpCount', Count);
		Add('tpOnGenerate', OnGenerate);
		Add('tpOnRepeat', OnRepeat);
	end;
end;

end.
