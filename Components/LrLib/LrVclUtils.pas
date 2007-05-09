unit LrVclUtils;

interface

uses
 SysUtils, Windows, Types, Messages, Classes, Controls, Forms;

procedure LrAddForm(inForm: TForm; inParent: TWinControl;
	inAlign: TAlign = alClient; inShow: Boolean = true); overload;
procedure LrAddForm(out outForm; inFormClass: TFormClass;
	inParent: TWinControl; inAlign: TAlign = alClient;
	inShow: Boolean = true); overload;

procedure LrFindComponentByClass(out inComponent; inOwner: TComponent;
	inClass: TClass);
//function LrFindComponentByClass(inOwner: TComponent;
//	inClass: TClass): TComponent;
function LrFindControlByClass(inOwner: TWinControl;
	inClass: TClass): TControl;

function LrFindElderComponent(inControl: TControl;
	const inClass: TClass): TComponent; overload;
procedure LrFindElderComponent(out outComponent; inControl: TControl;
	const inClass: TClass); overload;

procedure LrNotifyControls(inCtrl: TWinControl; Msg: Word;
	wParam: Pointer = nil);
procedure LrNotifyAll(inCtrl: TWinControl; Msg: Word; wParam: Pointer = nil);

function LrIsAs(inComponent: TComponent; const inIID: TGUID;
	out outObj): Boolean;

function LrMin(inA, inB: Integer): Integer;
function LrMax(inA, inB: Integer): Integer;
function LrAbs(inValue: Integer): Integer;

function LrIsDigit(const inChar: Char): Boolean;
function LrStringIsDigits(const inString: string): Boolean;

function LrCat(const inAdd: array of string;
	const inDelim: string = ' '): string; overload;
function LrCat(const inPre, inPost: string;
	const inDelim: string = ' '): string; overload;

function LrWidth(const inRect: TRect): Integer;
function LrHeight(const inRect: TRect): Integer;
function LrValidateRect(const inRect: TRect): TRect;

function LrNameIsUnique(inOwner: TComponent; const inName: string): Boolean;
function LrUniqueName(inOwner: TComponent; const inClassName: string): string;

function LrLoadComponentFromStream(inComp: TComponent; inStream: TStream;
	inOnError: TReaderError = nil): TComponent;
function LrLoadComponentFromString(const inString: string;
	inComp: TComponent = nil;	inOnError: TReaderError = nil): TComponent;
procedure LrLoadComponentFromFile(inComp: TComponent; const inFilename: string;
	inOnError: TReaderError = nil);
function LrLoadComponentFromBinaryStream(inComp: TComponent; inStream: TStream;
	inOnError: TReaderError = nil): TComponent;

function LrSaveComponentToString(inComp: TComponent): string;
procedure LrSaveComponentToStream(inComp: TComponent; inStream: TStream);
procedure LrSaveComponentToFile(inComp: TComponent; const inFilename: string);
procedure LrSaveComponentToBinaryStream(inComp: TComponent; inStream: TStream);

procedure LrCopyStreamToClipboard(inFmt: Cardinal; inS: TStream);
procedure LrCopyStreamFromClipboard(inFmt: Cardinal; inS: TStream);

function LrKeyIsDown(inCode: Integer): Boolean;
function LrGetShiftState: TShiftState;

function LrFindHandle(inHandle: HWND; inContainer: TWinControl): TWinControl;

function LrCompareFileDates(const inFileA, inFileB: string): Integer;

implementation

uses
	Clipbrd;

procedure LrAddForm(inForm: TForm; inParent: TWinControl;
	inAlign: TAlign = alClient; inShow: Boolean = true);
begin
	with inForm do
	begin
		BorderIcons := [];
		Caption := '';
		BorderStyle := bsNone;
		Align := inAlign;
		Parent := inParent;
		//if inShow then
		//	Show;
		Visible := inShow;
	end;
end;

procedure LrAddForm(out outForm; inFormClass: TFormClass; inParent: TWinControl;
	inAlign: TAlign = alClient; inShow: Boolean = true);
begin
	TForm(outForm) := inFormClass.Create(nil);
	LrAddForm(TForm(outForm), inParent, inAlign, inShow);
end;

procedure LrFindComponentByClass(out inComponent; inOwner: TComponent;
	inClass: TClass);
var
	i: Integer;
begin
	TComponent(inComponent) := nil;
	if inOwner <> nil then
		for i := 0 to Pred(inOwner.ComponentCount) do
			if inOwner.Components[i] is inClass then
			begin
				TComponent(inComponent) := inOwner.Components[i];
				break;
			end;
end;

{
function LrFindComponentByClass(inOwner: TComponent;
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
}

function LrFindControlByClass(inOwner: TWinControl;
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

function LrFindParentFormOrFrame(inControl: TControl): TWinControl;
begin
	Result := TWinControl(inControl);
	while (Result <> nil) and not (Result is TCustomForm)
		and not (Result is TCustomFrame) do
			Result := TWinControl(Result.Parent);
end;

function LrFindElderComponent(inControl: TControl;
	const inClass: TClass): TComponent;
var
	f: TWinControl;
begin
	Result := nil;
	while (inControl <> nil) do
	begin
		f := LrFindParentFormOrFrame(inControl);
		if (f = nil) then
			break;
		LrFindComponentByClass(Result, f, inClass);
		if (Result <> nil) then
			break;
		inControl := f.Parent;
	end;
end;

procedure LrFindElderComponent(out outComponent; inControl: TControl;
	const inClass: TClass); 
begin
	TComponent(outComponent) := LrFindElderComponent(inControl, inClass);
end;

procedure LrNotifyControls(inCtrl: TWinControl; Msg: Word; wParam: Pointer);
var
	m: TMessage;
begin
	FillChar(m, SizeOf(TMessage), 0);
	m.Msg := Msg;
	m.wParam := Integer(wParam);
	inCtrl.Broadcast(m);
end;

procedure LrNotifyAll(inCtrl: TWinControl; Msg: Word; wParam: Pointer);
var
	i: Integer;
begin
	LrNotifyControls(inCtrl, Msg, wParam);
	with inCtrl do
		for i := 0 to ControlCount - 1 do
			if (Controls[i] is TWinControl) then
				LrNotifyAll(TWinControl(Controls[i]), Msg, wParam);
end;

function LrIsAs(inComponent: TComponent; const inIID: TGUID;
	out outObj): Boolean;
begin
	with inComponent as IInterface do
		Result := QueryInterface(inIID, outObj) = S_OK;
end;

function LrMax(inA, inB: Integer): Integer;
begin
	if inB > inA then
		Result := inB
	else
		Result := inA;
end;

function LrMin(inA, inB: Integer): Integer;
begin
	if inB < inA then
		Result := inB
	else
		Result := inA;
end;

function LrAbs(inValue: Integer): Integer;
begin
	if inValue < 0 then
		Result := -inValue
	else
		Result := inValue;
end;

function LrIsDigit(const inChar: Char): Boolean;
begin
	Result := (inChar >= '0') and (inChar <= '9');
end;

function LrStringIsDigits(const inString: string): Boolean;
var
	i: Integer;
begin
	Result := false;
	for i := 1 to Length(inString) do
		if not LrIsDigit(inString[i]) then
			exit;
	Result := inString <> '';
end;

function LrCat(const inAdd: array of string;
	const inDelim: string = ' '): string;
var
	i: Integer;
begin
	Result := '';
	for i := 0 to Pred(Length(inAdd)) do
		if inAdd[i] <> '' then
			if Result = '' then
				Result := inAdd[i]
			else
				Result := Result + inDelim + inAdd[i];
end;

function LrCat(const inPre, inPost: string;
	const inDelim: string = ' '): string;
begin
	if inPost = '' then
		Result := inPre
	else
		if inPre = '' then
			Result := inPost
		else
			Result := inPre + inDelim + inPost;
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

procedure LrSaveComponentToBinaryStream(inComp: TComponent; inStream: TStream);
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

function LrLoadComponentFromBinaryStream(inComp: TComponent; inStream: TStream;
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

procedure LrSaveComponentToStream(inComp: TComponent; inStream: TStream);
var
	ms: TMemoryStream;
begin
	ms := TMemoryStream.Create;
	try
		ms.WriteComponent(inComp);
		ms.Position := 0;
		ObjectBinaryToText(ms, inStream);
	finally
		ms.Free;
	end;
end;

function LrLoadComponentFromStream(inComp: TComponent; inStream: TStream;
	inOnError: TReaderError): TComponent;
var
	ms: TMemoryStream;
begin
	ms := TMemoryStream.Create;
	try
		ObjectTextToBinary(inStream, ms);
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

function LrSaveComponentToString(inComp: TComponent): string;
var
	s: TStringStream;
begin
	s := TStringStream.Create('');
	try
		LrSaveComponentToStream(inComp, s);
		Result := s.DataString;
	finally
		s.Free;
	end;
end;

function LrLoadComponentFromString(const inString: string;
	inComp: TComponent; inOnError: TReaderError): TComponent;
var
	s: TStringStream;
begin
	s := TStringStream.Create(inString);
	try
		//s.Position := 0;
		Result := LrLoadComponentFromStream(inComp, s, inOnError);
	finally
		s.Free;
	end;
end;

procedure LrSaveComponentToFile(inComp: TComponent; const inFilename: string);
var
	fs: TFileStream;
begin
	fs := TFileStream.Create(inFilename, fmCreate);
	try
		LrSaveComponentToStream(inComp, fs);
	finally
		fs.Free;
	end;
end;

procedure LrLoadComponentFromFile(inComp: TComponent; const inFilename: string;
	inOnError: TReaderError);
var
	fs: TFileStream;
begin
	if FileExists(inFilename) then
	begin
		fs := TFileStream.Create(inFilename, fmOpenRead);
		try
			LrLoadComponentFromStream(inComp, fs, inOnError);
		finally
			fs.Free;
		end;
	end;
end;

function LrKeyIsDown(inCode: Integer): Boolean;
begin
	Result := Boolean(GetAsyncKeyState(inCode) shr 15);
end;

function LrGetShiftState: TShiftState;
begin
	Result := [];
	if LrKeyIsDown(VK_SHIFT) then
		Include(Result, ssShift);
	if LrKeyIsDown(VK_CONTROL) then
		Include(Result, ssCTRL);
end;

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

function LrCompareFileDates(const inFileA, inFileB: string): Integer;
begin
	if not FileExists(inFileB) then
		Result := 1
	else if not FileExists(inFileA) then
		Result := -1
	else
		Result := FileAge(inFileA) - FileAge(inFileB);
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
		end	else
			raise Exception.Create(
				'CopyStreamFromClipboard: could not lock global handle ' +
				'obtained from clipboard!');
	end;
end;

end.
