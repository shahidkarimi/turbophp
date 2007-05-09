unit ThVclUtils;

interface

uses
 SysUtils, Messages, Classes, Controls, Forms;

function ThFindComponentByClass(inOwner: TComponent;
	inClass: TClass): TComponent;
function ThFindControlByClass(inOwner: TWinControl;
	inClass: TClass): TControl;

function ThFindElderComponent(inControl: TControl;
	const inClass: TClass): TComponent; overload;
procedure ThFindElderComponent(out outComponent; inControl: TControl;
	const inClass: TClass); overload;

procedure ThNotifyControls(inCtrl: TWinControl; Msg: Word;
	wParam: Pointer = nil);
procedure ThNotifyAll(inCtrl: TWinControl; Msg: Word; wParam: Pointer = nil);

function ThIsAs(inComponent: TComponent; const inIID: TGUID;
	out outObj): Boolean;

implementation

function ThFindComponentByClass(inOwner: TComponent;
	inClass: TClass): TComponent;
var
	i: Integer;
begin
	Result := nil;
	if inOwner <> nil then
		for i := 0 to Pred(inOwner.ComponentCount) do
			if inOwner.Components[i] is inClass then
			begin
				Result := inOwner.Components[i];
				break;
			end;
end;

function ThFindControlByClass(inOwner: TWinControl;
	inClass: TClass): TControl;
var
	i: Integer;
begin
	Result := nil;
	if inOwner <> nil then
		for i := 0 to Pred(inOwner.ControlCount) do
			if inOwner.Controls[i] is inClass then
			begin
				Result := inOwner.Controls[i];
				break;
			end;
end;

function ThFindParentFormOrFrame(inControl: TControl): TWinControl;
begin
	Result := TWinControl(inControl);
	while (Result <> nil) and not (Result is TCustomForm)
		and not (Result is TCustomFrame) do
			Result := TWinControl(Result.Parent);
end;

function ThFindElderComponent(inControl: TControl;
	const inClass: TClass): TComponent;
var
	f: TWinControl;
begin
	Result := nil;
	while (inControl <> nil) do
	begin
		f := ThFindParentFormOrFrame(inControl);
		if (f = nil) then
			break;
		Result := ThFindComponentByClass(f, inClass);
		if (Result <> nil) then
			break;
		inControl := f.Parent;
	end;
end;

procedure ThFindElderComponent(out outComponent; inControl: TControl;
	const inClass: TClass); 
begin
	TComponent(outComponent) := ThFindElderComponent(inControl, inClass);
end;

procedure ThNotifyControls(inCtrl: TWinControl; Msg: Word; wParam: Pointer);
var
	m: TMessage;
begin
	FillChar(m, SizeOf(TMessage), 0);
	m.Msg := Msg;
	m.wParam := Integer(wParam);
	inCtrl.Broadcast(m);
end;

procedure ThNotifyAll(inCtrl: TWinControl; Msg: Word; wParam: Pointer);
var
	i: Integer;
begin
	ThNotifyControls(inCtrl, Msg, wParam);
	with inCtrl do
		for i := 0 to ControlCount - 1 do
			if (Controls[i] is TWinControl) then
				ThNotifyAll(TWinControl(Controls[i]), Msg, wParam);
end;

function ThIsAs(inComponent: TComponent; const inIID: TGUID;
	out outObj): Boolean;
begin
	with inComponent as IInterface do
		Result := QueryInterface(inIID, outObj) = S_OK;
end;

end.
