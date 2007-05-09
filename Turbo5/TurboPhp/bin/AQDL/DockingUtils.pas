unit DockingUtils;

interface

uses
	SysUtils,
	aqDocking;

procedure LoadDockLayout(inManager: TaqDockingManager;
	const inFilename: string);
procedure SaveDockLayout(inManager: TaqDockingManager;
	const inFilename: string);

implementation

procedure LoadDockLayout(inManager: TaqDockingManager;
	const inFilename: string);
begin
	if FileExists(inFilename) then
		inManager.LoadFromFile(inFilename);
end;

procedure SaveDockLayout(inManager: TaqDockingManager;
	const inFilename: string);
begin
	inManager.SaveToFile(inFilename);
end;

end.
