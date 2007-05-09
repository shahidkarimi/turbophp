unit DockingUtils;

interface

uses
	SysUtils,
	dxDockControl;

procedure LoadDockLayout(inManager: TdxDockingManager;
	const inFilename: string);
procedure SaveDockLayout(inManager: TdxDockingManager;
	const inFilename: string);

implementation

procedure LoadDockLayout(inManager: TdxDockingManager;
	const inFilename: string);
begin
	if FileExists(inFilename) then
		try
			dxClientSizingOff := true;
			inManager.LoadLayoutFromIniFile(inFilename);
		finally
			dxClientSizingOff := false;
		end;
end;

procedure SaveDockLayout(inManager: TdxDockingManager;
	const inFilename: string);
begin
	inManager.SaveLayoutToIniFile(inFilename);
end;

end.
