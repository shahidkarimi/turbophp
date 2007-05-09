unit Registration;

interface

procedure Register;

implementation

uses
	Classes, ComCtrls, ExtCtrls,
	htAutoTable, htPanel,
	htGeneric, htLabel, htImage,
	htInput, htButton,
	tpTooltip, tpGrid
	;

{$R TurboHtml5Palette.res}

procedure Register;
begin
	RegisterComponents('Containers', [ ThtPanel, ThtAutoTable ]);
	RegisterComponents('Controls', [ ThtGeneric, ThtLabel, ThtImage, TImage,
		ThtInput, ThtButton ]);
	RegisterComponents('Data', [ TTpGrid ]);
	RegisterComponents('Components', [ TTpTooltip, TTpTooltipComponent ]);
	RegisterComponents('Test', [ TImage ]);
	//RegisterComponents('Rich', [ TRichView, TRVStyle ]);
end;

end.
