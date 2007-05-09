{-----------------------------------------------------------------------------

 Least-Resistance Designer Library

 The Initial Developer of the Original Code is Scott J. Miles
	<sjmiles (at) turbophp (dot) com>.
 Portions created by Scott J. Miles are
	Copyright (C) 2005 Least-Resistance Software.
 All Rights Reserved.

-------------------------------------------------------------------------------}

unit DesignUtils;

interface

uses
	SysUtils, Windows, Classes, Controls;

function LrFindHandle(inHandle: HWND; inContainer: TWinControl): TWinControl;

function LrWidth(const inRect: TRect): Integer;
function LrHeight(const inRect: TRect): Integer;
function LrValidateRect(const inRect: TRect): TRect;

function LrNameIsUnique(inOwner: TComponent; const inName: string): Boolean;
function LrUniqueName(inOwner: TComponent; const inClassName: string): string;

procedure LrSaveComponentToBinaryStream(inStream: TStream;
	inComp: TComponent);
function LrLoadComponentFromBinaryStream(inStream: TStream;
	inComp: TComponent = nil; inOnError: TReaderError = nil): TComponent;

procedure LrCopyStreamToClipboard(inFmt: Cardinal; inS: TStream);
procedure LrCopyStreamFromClipboard(inFmt: Cardinal; inS: TStream);

implementation

uses
	Clipbrd;

function LrFindHandle(inHandle: HWND; inContainer: TWinControl): TWinControl;
var
	i: Integer;
begin
	Result := nil;
	with inContainer do
		if Handle = inHandle then
			Result := inContainer
		else
			for i := 0 to Pred(ControlCount) do
				if (inContainer.Controls[i] is TWinControl) then
				begin
					Result := LrFindHandle(inHandle, TWinControl(Controls[i]));
					if Result <> nil then
						break;
				end;
end;

function LrWidth(const inRect: TRect): Integer;
begin
	Result := inRect.Right - inRect.Left;
end;

function LrHeight(const inRect: TRect): Integer;
begin
	Result := inRect.Bottom - inRect.Top;
end;

function LrValidateRect(const inRect: TRect): TRect;
begin
	with Result do
	begin
		if inRect.Right < inRect.Left then
		begin
			Left := inRect.Right;
			Right := inRect.Left;
		end
		else begin
			Left := inRect.Left;
			Right := inRect.Right;
		end;
		if inRect.Bottom < inRect.Top then
		begin
			Top := inRect.Bottom;
			Bottom := inRect.Top;
		end
		else begin
			Top := inRect.Top;
			Bottom := inRect.Bottom;
		end;
	end;
end;

function LrNameIsUnique(inOwner: TComponent; const inName: string): Boolean;
begin
	Result := true;
	while Result and (inOwner <> nil) do
	begin
		Result := inOwner.FindComponent(inName) = nil;
		inOwner := inOwner.Owner;
	end;
end;

function LrUniqueName(inOwner: TComponent; const inClassName: string): string;
var
	base: string;
	i: Integer;
begin
	base := Copy(inClassName, 2, MAXINT);
	i := 0;
	repeat
		Inc(i);
		Result := base + IntToStr(i);
	until LrNameIsUnique(inOwner, Result);
end;

procedure LrSaveComponentToBinaryStream(inStream: TStream; inComp: TComponent);
var
	ms: TMemoryStream;
	sz: Int64;
begin
	ms := TMemoryStream.Create;
	try
		ms.WriteComponent(inComp);
		ms.Position := 0;
		sz := ms.Size;
		inStream.Write(sz, 8);
		inStream.CopyFrom(ms, sz);
	finally
		ms.Free;
	end;
end;

function LrLoadComponentFromBinaryStream(inStream: TStream; inComp: TComponent;
	inOnError: TReaderError): TComponent;
var
	ms: TMemoryStream;
	sz: Int64;
begin
	inStream.Read(sz, 8);
	ms := TMemoryStream.Create;
	try
		ms.CopyFrom(inStream, sz);
		ms.Position := 0;
		with TReader.Create(ms, 4096) do
		try
			OnError := inOnError;
			Result := ReadRootComponent(inComp);
		finally
			Free;
		end;
	finally
		ms.Free;
	end;
end;

procedure LrCopyStreamToClipboard(inFmt: Cardinal; inS: TStream);
var
	hMem: THandle;
	pMem: Pointer;
begin
	inS.Position := 0;
	hMem := GlobalAlloc(GHND or GMEM_DDESHARE, inS.Size);
	if hMem <> 0 then
	begin
		pMem := GlobalLock(hMem);
		if pMem <> nil then
		begin
			inS.Read(pMem^, inS.Size);
			inS.Position := 0;
			GlobalUnlock(hMem);
			Clipboard.Open;
			try
				Clipboard.SetAsHandle(inFmt, hMem);
			finally
				Clipboard.Close;
			end;
		end
		else begin
			GlobalFree(hMem);
			OutOfMemoryError;
		end;
	end else
		OutOfMemoryError;
end;

procedure LrCopyStreamFromClipboard(inFmt: Cardinal; inS: TStream);
var
	hMem: THandle;
	pMem: Pointer;
begin
	hMem := Clipboard.GetAsHandle(inFmt);
	if hMem <> 0 then
	begin
		pMem := GlobalLock(hMem);
		if pMem <> nil then
		begin
			inS.Write(pMem^, GlobalSize(hMem));
			inS.Position := 0;
			GlobalUnlock(hMem);
		end;
	end;
end;

end.
