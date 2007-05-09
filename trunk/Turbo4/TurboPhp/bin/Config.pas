unit Config;

interface

uses
	SysUtils, Forms;

var
	BinFolder: string;
	ConfigFolder: string;
	HomeFolder: string;
	LibSourceFolder: string;
	RefFolder: string;
	TpLibDocsFolder: string;

implementation

const
	cHomeSuffix = '..\';
	cConfigSuffix = 'config\';
	cDocsSuffix = 'docs\TurboPhpLib\';
	cLibSuffix = 'libs\';
	cRefSuffix = 'ref\default\';

procedure InitGlobals;
begin
	BinFolder := ExtractFilePath(Application.ExeName);
	HomeFolder :=  ExpandFileName(BinFolder + cHomeSuffix);
	ConfigFolder := HomeFolder + cConfigSuffix;
	LibSourceFolder := HomeFolder + cLibSuffix;
	RefFolder := HomeFolder + cRefSuffix;
	TpLibDocsFolder := HomeFolder + cDocsSuffix;
end;

initialization
	InitGlobals;
end.
