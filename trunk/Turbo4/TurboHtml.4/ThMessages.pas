unit ThMessages;

interface

uses
	Messages;

const
{$ifdef LINUX}
	THM_BASE = 4000 + 423;
{$else}
	THM_BASE = WM_APP + 27;
{$endif}
	THM_CHANGE = THM_BASE;
	THM_DESIGN_ADJUST = THM_BASE + 1;
	THM_STYLECHANGE = THM_BASE + 2;
	THM_UPDATEPICTURE = THM_BASE + 3;

implementation

end.
