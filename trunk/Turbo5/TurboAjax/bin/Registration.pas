unit Registration;

interface

procedure Register;

implementation

uses
	Classes, StdCtrls, ComCtrls, ExtCtrls,
	ClassInfo,
	htPanel, htFlowPanel, htColumnsPanel,
	htLabel, htImage, htGeneric,
	htButton, htInput, htSelect,
	htAjaxPanel, htTurboBox, htTurboSplitter;

procedure Register;
begin
	//RegisterComponents('Containers', [ TPanel ]);
	//RegisterComponents('Controls', [ TLabel, TButton, TEdit, TMemo, TImage ]);
	with ComponentRegistry do
	begin
		RegisterComponents(
			'TurboWidgets',
			[ ThtAjaxPanel, ThtTurboBox, ThtTurboSplitter ],
			[ 'TurboPanel', 'TurboBox', 'TurboSplitter' ]
		);
		RegisterComponents(
			'HTML Containers',
			[ ThtPanel, ThtFlowPanel, ThtColumnsPanel, ThtAjaxPanel ],
			[ 'Free Layout', 'Flow Layout', 'Columns Layout', 'Ajax Layout' ]
		);
		RegisterComponents(
			'HTML Controls',
			[ ThtLabel, ThtImage, ThtGeneric ],
			[ 'Label', 'Image', 'Generic Control' ]
		);
		RegisterComponents(
			'HTML Inputs',
			[ ThtButton, ThtInput, ThtSelect ],
			[ 'Button', 'Text Input', 'Select' ]
		);
	end;
end;

end.
