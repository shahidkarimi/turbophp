unit TpLabel;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThTag, ThAttributeList, ThAnchor, ThLabel,
	TpControls, TpAnchor;

type
	TTpLabel = class(TThLabel)
	private
		FOnGenerate: TTpEvent;
	protected
		function CreateAnchor: TThAnchor; override;
		procedure LabelTag(inTag: TThTag); override;
	published
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
	end;
	//
	TTpText = class(TThText)
	private
		FOnGenerate: TTpEvent;
	protected
		procedure LabelTag(inTag: TThTag); override;
	published
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
	end;

implementation

{ TTpLabel }

function TTpLabel.CreateAnchor: TThAnchor;
begin
	Result := TTpAnchor.Create(Self);
end;

procedure TTpLabel.LabelTag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpLabel');
		Add('tpName', Name);
		Add('tpOnGenerate', OnGenerate);
	end;
end;

{ TTpText }

procedure TTpText.LabelTag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpLabel');
		Add('tpName', Name);
		Add('tpOnGenerate', OnGenerate);
	end;
end;

end.
