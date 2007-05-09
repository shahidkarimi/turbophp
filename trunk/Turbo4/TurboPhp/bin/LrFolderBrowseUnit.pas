unit LrFolderBrowseUnit;

interface

function LrFolderBrowse(const inBrowseTitle: string;
	const inInitialFolder: string = ''; const inRootFolder: string = ''): string;

implementation

uses
	Windows, ShlObj, ShellBrowser;

var
	lg_StartFolder: String;

	function LrFolderBrowseCallBack(Wnd: HWND; uMsg: UINT;
		lParam, lpData: LPARAM): Integer stdcall;
	begin
		if uMsg = BFFM_INITIALIZED then
				SendMessage(Wnd, BFFM_SETSELECTION, 1, Integer(@lg_StartFolder[1]));
		Result := 0;
	end;

function LrFolderBrowse(const inBrowseTitle: string;
	const inInitialFolder: string = ''; const inRootFolder: string = ''): string;
var
	pidl: PItemIdList;
	browse_info: TBrowseInfo;
	folder: array[0..MAX_PATH] of char;
	find_context: PItemIDList;
begin
	Result := '';
	if inRootFolder <> '' then
		pidl := GetIdListFromPath(nil, inRootFolder)
	else
		pidl := nil;
	try
		FillChar(browse_info, SizeOf(browse_info), #0);
		lg_StartFolder := inInitialFolder;
		browse_info.pszDisplayName := @folder[0];
		browse_info.lpszTitle := PChar(inBrowseTitle);
		browse_info.pidlRoot := pidl;
		{$ifdef VER150}
		browse_info.ulFlags := BIF_RETURNONLYFSDIRS or BIF_USENEWUI;
		{$else}
		browse_info.ulFlags := BIF_RETURNONLYFSDIRS or $0050;
		{$endif}
		browse_info.lpfn := LrFolderBrowseCallBack;
		find_context := SHBrowseForFolder(browse_info);
	finally
		if pidl <> nil then
			Allocator.Free(pidl);
	end;
	if Assigned(find_context) then
		if SHGetPathFromIDList(find_context, folder) then
			Result := folder
end;

end.
