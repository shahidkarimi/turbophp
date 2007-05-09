unit TpAnchor;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThTag, ThAnchor,
	TpControls;

type
	TTpAnchor = class(TThAnchor)
	private
		FClient: TComponent;
	protected
		procedure Tag(inTag: TThTag); override;
	public
		constructor Create(inClient: TComponent);
	public
		property Client: TComponent read FClient write FClient;
	end;

implementation

constructor TTpAnchor.Create(inClient: TComponent);
begin
	inherited Create;
	Client := inClient;
end;

procedure TTpAnchor.Tag(inTag: TThTag);
begin
	inherited;
	if Client <> nil then
	begin
		inTag.Add(tpClass, 'TTpAnchor');
		inTag.Add('tpName', Client.Name + 'Anchor');
	end;
end;

end.
 