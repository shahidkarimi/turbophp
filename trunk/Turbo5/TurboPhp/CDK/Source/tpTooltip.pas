unit tpTooltip;

interface

uses
	SysUtils, Classes, Controls, Graphics,
	LrGraphics, LrTextPainter,
	htDocument, htComponent, htControls, htGeneric;

type
	TtpTooltip = class(ThtGeneric)
	protected
		procedure Generate(const inContainer: string;
			inDocument: ThtDocument); override;
	end;
	//
	TtpTooltipComponent = class(ThtComponent)
	public
		procedure Generate(inDocument: ThtDocument); override;
	end;

implementation

{ TtpTooltip }

procedure TtpTooltip.Generate(const inContainer: string;
	inDocument: ThtDocument);
begin
	with inDocument do
	begin
		with Head do
			Add('<script src="lib/jttl.js"></script>');
		with Script do
		begin
			Add('var objTooltip;');
			Add('function InitTooltips()');
			Add('{');
			Add('  if (typeof(Tooltips) == "function")');
			Add('  {');
			Add('    objTooltip = new Tooltips();');
			Add('    objTooltip.registerElements(document.body);');
			Add('    objTooltip.activate();');
			Add('  }');
			Add('}');
		end;
		InitCode.Add(' InitTooltips();');
	end;
end;

{ TtpTooltipComponent }

procedure TtpTooltipComponent.Generate(inDocument: ThtDocument);
begin
	with inDocument do
	begin
		with Head do
			Add('<script src="lib/jttl.js"></script>');
		with Script do
		begin
			Add('var objTooltip;');
			Add('function InitTooltips()');
			Add('{');
			Add('  if (typeof(Tooltips) == "function")');
			Add('  {');
			Add('    objTooltip = new Tooltips();');
			Add('    objTooltip.registerElements(document.body);');
			Add('    objTooltip.activate();');
			Add('  }');
			Add('}');
		end;
		InitCode.Add(' InitTooltips();');
	end;
end;

end.
