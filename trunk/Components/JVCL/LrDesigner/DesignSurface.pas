{-----------------------------------------------------------------------------

 Least-Resistance Designer Library

 The Initial Developer of the Original Code is Scott J. Miles
	<sjmiles (at) turbophp (dot) com>.
 Portions created by Scott J. Miles are
	Copyright (C) 2005 Least-Resistance Software.
 All Rights Reserved.

-------------------------------------------------------------------------------}

unit DesignSurface;

interface

uses
	Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, ExtCtrls,
	Contnrs;

type
	TDesignSurface = class(TComponent)
	private
		FActive: Boolean;
		FContainer: TWinControl;
		FHintWin: THintWindow;
		FOnChange: TNotifyEvent;
	protected
		function DesignKeyDown(inKeycode: Cardinal;
			inShift: TShiftState): Boolean; virtual;
		function DesignKeyUp(inKeycode: Cardinal;
			inShift: TShiftState): Boolean; virtual;
		function DesignMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer): Boolean; virtual;
		function DesignMouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer): Boolean; virtual;
		function DesignMouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer): Boolean; virtual;
		function KeyHook(inKey, inFlags: Cardinal): Boolean;
		function KeyMessage(inKeycode, inFlags: Cardinal): Boolean;
		function MouseHook(inMsg: Cardinal;	var inInfo: TMouseHookStruct): Boolean;
		function MouseMessage(inMsg: Cardinal; const inPt: TPoint): Boolean;
		procedure CreateHintWindow; virtual;
		procedure InstallHooks;
		procedure SetActive(const Value: Boolean); virtual;
		procedure SetContainer(const Value: TWinControl); virtual;
		procedure SetOnChange(const Value: TNotifyEvent); virtual;
		procedure UninstallHooks;
		procedure UpdateHint(const inHint: string); virtual;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure Change; virtual;
		property Active: Boolean read FActive write SetActive;
		property HintWin: THintWindow read FHintWin write FHintWin;
	published
		property Container: TWinControl read FContainer write SetContainer;
		property OnChange: TNotifyEvent read FOnChange write SetOnChange;
	end;

implementation

uses
	DesignHooks, DesignUtils;

{ TDesignSurface }

constructor TDesignSurface.Create(AOwner: TComponent);
begin
	inherited;
	CreateHintWindow;
end;

destructor TDesignSurface.Destroy;
begin
	UninstallHooks;
	inherited;
end;

procedure TDesignSurface.CreateHintWindow;
begin
	HintWin := HintWindowClass.Create(Self);
	HintWin.Brush.Color := clInfoBk;
end;

procedure TDesignSurface.SetOnChange(const Value: TNotifyEvent);
begin
	FOnChange := Value;
end;

procedure TDesignSurface.Change;
begin
	if Assigned(OnChange) then
		OnChange(Self);
end;

procedure TDesignSurface.SetContainer(const Value: TWinControl);
begin
	FContainer := Value;
end;

procedure TDesignSurface.SetActive(const Value: Boolean);
begin
	FActive := Value;
	if Active then
		InstallHooks
	else
		UninstallHooks;
end;

procedure TDesignSurface.InstallHooks;
begin
	if (Container = nil) and (Owner <> nil) and (Owner is TWinControl) then
		Container := TWinControl(Owner);
	TDesignHooks.InstallMouseHook(MouseHook);
	TDesignHooks.InstallKeyHook(KeyHook);
end;

procedure TDesignSurface.UninstallHooks;
begin
	TDesignHooks.RemoveMouseHook;
	TDesignHooks.RemoveKeyHook;
end;

function TDesignSurface.MouseHook(inMsg: Cardinal;
	var inInfo: TMouseHookStruct): Boolean;
var
	target: TWinControl;
begin
	Result := false;
	case inMsg of
		WM_MOUSEFIRST..WM_MOUSELAST:
		begin
			target := LrFindHandle(inInfo.hwnd, Container);
			if (target <> nil) then
				Result := MouseMessage(inMsg, Container.ScreenToClient(inInfo.pt));
		end;
	end;
end;

function TDesignSurface.KeyHook(inKey: Cardinal; inFlags: Cardinal): Boolean;
begin
	Result := KeyMessage(inKey, inFlags);
end;

procedure TDesignSurface.UpdateHint(const inHint: string);
var
	r: TRect;
begin
	with FHintWin	do
	begin
		r := Rect(-8, 0, Canvas.TextWidth(inHint), Canvas.TextHeight(inHint));
		with Mouse.CursorPos do
			OffsetRect(r, X + 20, Y + 20);
		ActivateHint(r, inHint);
		Update;
	end;
	if Assigned(Container) then
		Container.Update;
end;

function TDesignSurface.MouseMessage(inMsg: Cardinal;
	const inPt: TPoint): Boolean;

	function GetKeyShift: TShiftState;

		function IsKeyDown(inCode: Integer): Boolean;
		begin
			Result := Boolean(GetAsyncKeyState(inCode) shr 15);
		end;

	begin
		Result := [];
		if IsKeyDown(VK_SHIFT) then
			Include(Result, ssShift);
		if IsKeyDown(VK_CONTROL) then
			Include(Result, ssCTRL);
	end;

begin
	case inMsg of
		WM_LBUTTONDOWN:
			Result := DesignMouseDown(Self, mbLeft, GetKeyShift, inPt.X, inPt.Y);
		WM_LBUTTONUP:
		begin
			HintWin.ReleaseHandle;
			Result := DesignMouseUp(Self, mbLeft, GetKeyShift, inPt.X, inPt.Y);
		end;
		WM_MOUSEMOVE:
			Result := DesignMouseMove(Self, GetKeyShift, inPt.X, inPt.Y);
		else Result := false;
	end;
end;

function TDesignSurface.DesignMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
	Result := false;
end;

function TDesignSurface.DesignMouseMove(Sender: TObject;
	Shift: TShiftState; X, Y: Integer): Boolean;
begin
	Result := false;
end;

function TDesignSurface.DesignMouseUp(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
	Result := false;
end;

function TDesignSurface.KeyMessage(inKeycode: Cardinal;
	inFlags: Cardinal): Boolean;
var
	shift: TShiftState;
begin
	Result := false;
	shift := KeyDataToShiftState(Longint(inFlags));
	if Container.Focused then
		if ((inFlags shr 16) and KF_UP) = KF_UP then
			Result := DesignKeyUp(inKeycode, shift)
		else
			Result := DesignKeyDown(inKeycode, shift);
end;

function TDesignSurface.DesignKeyDown(inKeycode: Cardinal;
	inShift: TShiftState): Boolean;
begin
	Result := false;
end;

function TDesignSurface.DesignKeyUp(inKeycode: Cardinal;
	inShift: TShiftState): Boolean;
begin
	Result := false;
end;

end.
