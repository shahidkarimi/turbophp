unit ThDbReg;

interface

uses
	Classes;

procedure Register;

implementation

uses
	ThDbText, ThDataConnectionProperty;

procedure Register;
begin
	RegisterComponents('TurboHtmlDb4',
		[
			TThDbText
		]);
	RegisterConnectionStringPropertyEditor;
end;

end.
