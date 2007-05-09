unit LrUtils;

interface

uses
	SysUtils, StrUtils, Classes, Controls, Forms;

function AppFolder: string;
procedure NeedFolder(const inFolder: string);
function IsSubfolder(const inFolder, inRoot: string): Boolean;

procedure CopyFile(const inSrc, inDst: string);

procedure AddForm(out outForm; inFormClass: TFormClass; inParent: TWinControl;
	inAlign: TAlign = alClient; inShow: Boolean = true);
procedure InsertForm(inForm: TForm; inParent: TWinControl;
	inAlign: TAlign = alClient; inShow: Boolean = true);

function FindComponentByClass(out outComponent; inClass: TClass;
	inOwner: TComponent): Boolean;

function IsDelimiter(const inS, inDelim: string; inIndex: Integer): Boolean;

function IncludeTrailingDelimiter(const inS, inD: string): string;
function IncludeTrailingFrontslash(const inS: string): string;
function IncludeTrailingBackslash(const inS: string): string;

function ExtensionsToFilter(inExts: array of string): string; overload;
function ExtensionsToFilter(inExts: TStringList): string; overload;

function IsAs(inComponent: TComponent; const inIID: TGUID;
	out outObj): Boolean;

procedure DeDupeStrings(inStrings: TStrings);

implementation

function AppFolder: string;
begin
	Result := ExtractFilePath(Application.ExeName);
end;

procedure NeedFolder(const inFolder: string);
begin
	if not DirectoryExists(inFolder) then
		if not ForceDirectories(inFolder) then
			raise EStreamError.Create('Could not create folder: "' + inFolder + '"');
end;

function IsSubfolder(const inFolder, inRoot: string): Boolean;
begin
	Result := AnsiStartsText(inRoot, inFolder);
end;

function FindComponentByClass(out outComponent; inClass: TClass;
	inOwner: TComponent): Boolean;
var
	i: Integer;
begin
	Result := false;
	TComponent(outComponent) := nil;
	if inOwner <> nil then
		for i := 0 to Pred(inOwner.ComponentCount) do
			if inOwner.Components[i] is inClass then
			begin
				TComponent(outComponent) := inOwner.Components[i];
				Result := true;
				break;
			end;
end;

procedure InsertForm(inForm: TForm; inParent: TWinControl;
	inAlign: TAlign = alClient; inShow: Boolean = true);
begin
	with inForm do
	begin
		BorderIcons := [];
		Caption := '';
		BorderStyle := bsNone;
		Align := inAlign;
		Parent := inParent;
		if inShow then
			Show;
		//Visible := inShow;
	end;
end;

procedure AddForm(out outForm; inFormClass: TFormClass; inParent: TWinControl;
	inAlign: TAlign = alClient; inShow: Boolean = true);
begin
	TForm(outForm) := inFormClass.Create(nil);
	InsertForm(TForm(outForm), inParent, inAlign, inShow);
end;

function IsDelimiter(const inS, inDelim: string; inIndex: Integer): Boolean;
begin
	Result := (inIndex > 0) and (inS[inIndex] = inDelim);
end;

function IncludeTrailingDelimiter(const inS, inD: string): string;
begin
	if IsDelimiter(inS, inD, Length(inS)) then
		Result := inS
	else
		Result := inS + inD;
end;

function IncludeTrailingFrontslash(const inS: string): string;
begin
	Result := IncludeTrailingDelimiter(inS, '/');
end;

function IncludeTrailingBackslash(const inS: string): string;
begin
	Result := IncludeTrailingDelimiter(inS, '\');
end;

function ExtensionsToFilter(inExts: array of string): string;
var
	i: Integer;
	s0, s1: string;
begin
	for i := 0 to Pred(Length(inExts)) do
		if i = 0 then
		begin
			s0 := '(*' + inExts[i];
			s1 := '|*' + inExts[i];
		end else
		begin
			s0 := s0 + ', *' + inExts[i];
			s1 := s1 + ';*' + inExts[i];
		end;
	Result := s0 + ')' + s1;
end;

function ExtensionsToFilter(inExts: TStringList): string;
var
	i: Integer;
	s0, s1: string;
begin
	for i := 0 to Pred(inExts.Count) do
		if i = 0 then
		begin
			s0 := '(*' + inExts[i];
			s1 := '|*' + inExts[i];
		end else
		begin
			s0 := s0 + ', *' + inExts[i];
			s1 := s1 + ';*' + inExts[i];
		end;
	Result := s0 + ')' + s1;
end;

function IsAs(inComponent: TComponent; const inIID: TGUID;
	out outObj): Boolean;
begin
	with inComponent as IInterface do
		Result := QueryInterface(inIID, outObj) = S_OK;
end;

procedure CopyFile(const inSrc, inDst: string);
var
	s: TMemoryStream;
begin
	s := TMemoryStream.Create;
	try
		s.LoadFromFile(inSrc);
		s.Position := 0;
		s.SaveToFile(inDst);
	finally
		s.Free;
	end;
end;

procedure DeDupeStrings(inStrings: TStrings);
var
	i, j: Integer;
begin
	i := 0;
	while (i < inStrings.Count) do
	begin
		j := i + 1;
		while (j < inStrings.Count) do
			if inStrings[i] = inStrings[j] then
				inStrings.Delete(j)
			else
				Inc(j);
		Inc(i);
	end;
end;

end.
