unit Globals;

interface

uses
	SysUtils, Forms;

var
	Home: string;
	BinHome: string;
	ConfigHome: string;
	ProjectsHome: string;
	TemplatesHome: string;

implementation

uses
	Config;

const
	cConfigFolder = 'config\';
	cProjectsFolder = 'projects\';
	cConfigFile = 'application.cfg';
	cTemplatesFolder = 'templates\';

procedure InitGlobals;
begin
	BinHome := ExtractFilePath(Application.ExeName);
	Home := ExpandFileName(BinHome + '..\');
	ProjectsHome := Home + cProjectsFolder;
	TemplatesHome := BinHome + cTemplatesFolder;
	ConfigHome := Home + cConfigFolder;
	Configuration.LoadFromFile(ConfigHome + cConfigFile);
end;

initialization
	InitGlobals;
end.
