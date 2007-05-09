{-----------------------------------------------------------------------------

 Least-Resistance Designer Library

 The Initial Developer of the Original Code is Scott J. Miles
	<sjmiles (at) turbophp (dot) com>.
 Portions created by Scott J. Miles are
	Copyright (C) 2005 Least-Resistance Software.
 All Rights Reserved.

-------------------------------------------------------------------------------}

unit DesignHooks;

interface

uses
	Windows;

type
	TDesignMouseHook = function(inMsg: Cardinal;
		var inHookInfo: TMouseHookStruct): Boolean of object;
	//
	TDesignKeyHook = function(inKey: Cardinal; inFlags: Cardinal): Boolean of object;
	//
	TDesignHooks = class
	public
		class function InstallKeyHook(inKeyHook: TDesignKeyHook): Boolean;
		class function InstallMouseHook(inMouseHook: TDesignMouseHook): Boolean;
		class function RemoveKeyHook: Boolean;
		class function RemoveMouseHook: Boolean;
	end;

implementation

var
	KeyHook: TDesignKeyHook;
	KeyHookHandle: HHook;
	MouseHook: TDesignMouseHook;
	MouseHookHandle: HHook;

function KeyHookProc(nCode: Integer; wParam: WPARAM;
	lParam: LPARAM): LRESULT; stdcall;
begin
	if nCode < 0 then
		Result := CallNextHookEx(KeyHookHandle, nCode, wParam, lParam)
	else begin
		if KeyHook(Cardinal(wParam), Cardinal(lParam)) then
			Result := 1
		else
			Result := CallNextHookEx(KeyHookHandle, nCode, wParam, lParam);
	end;
end;

function MouseHookProc(nCode: Integer; wParam: WPARAM;
	lParam: LPARAM): LRESULT; stdcall;
begin
	if nCode < 0 then
		Result := CallNextHookEx(MouseHookHandle, nCode, wParam, lParam)
	else begin
		if MouseHook(wParam, PMouseHookStruct(lParam)^) then
			Result := 1
		else
			Result := CallNextHookEx(MouseHookHandle, nCode, wParam, lParam);
	end;
end;

class function TDesignHooks.InstallKeyHook(inKeyHook: TDesignKeyHook): Boolean;
begin
	if (KeyHookHandle <> 0) then
		Result := false
	else begin
		KeyHook := inKeyHook;
		KeyHookHandle := SetWindowsHookEx(WH_KEYBOARD	, @KeyHookProc, 0,
			GetCurrentThreadID());
		Result := (KeyHookHandle <> 0);
	end;
end;

class function TDesignHooks.InstallMouseHook(
	inMouseHook: TDesignMouseHook): Boolean;
begin
	if (MouseHookHandle <> 0) then
		Result := false
	else begin
		MouseHook := inMouseHook;
		MouseHookHandle := SetWindowsHookEx(WH_MOUSE, @MouseHookProc, 0,
			GetCurrentThreadID());
		Result := (MouseHookHandle <> 0);
	end;
end;

class function TDesignHooks.RemoveKeyHook: Boolean;
begin
	Result := UnhookWindowsHookEx(KeyHookHandle);
	KeyHookHandle := 0;
end;

class function TDesignHooks.RemoveMouseHook: Boolean;
begin
	Result := UnhookWindowsHookEx(MouseHookHandle);
	MouseHookHandle := 0;
end;

end.
