unit ThPathUtils;

interface

uses
	SysUtils, Classes;

function ThAppendPath(const inPre, inSuff: string): string;
function ThPathToUrl(const inPath: string): string;
function ThUrlToPath(const inUrl: string): string;
function ThEscapePath(const inPath: string): string;
function ThIsFullPath(const inPath: string): Boolean;
procedure ThMemCopyFile(const inSrc, inDst: string);

const
	cThFrontSlash = '/';
	cThBackSlash = '\';
	{$ifdef LINUX}
	cThFileDelim = cThFrontSlash;
	{$else}
	cThFileDelim = cThBackSlash;
	{$endif}

implementation

function ThAppendPath(const inPre, inSuff: string): string;
const
	cDoubleDelim = cThFileDelim + cThFileDelim;
begin
	if inPre = '' then
		Result := inSuff
	else
		Result := StringReplace(inPre + cThFileDelim + inSuff,
			cDoubleDelim, cThFileDelim, [ rfReplaceAll ]);
end;

function ThPathToUrl(const inPath: string): string;
begin
	{$ifdef LINUX}
	Result := inPath;
	{$else}
	Result := StringReplace(inPath, cThBackSlash, cThFrontSlash,
		[ rfReplaceAll ]);
	{$endif}
end;

function ThUrlToPath(const inUrl: string): string;
begin
	{$ifdef LINUX}
	Result := inPath;
	{$else}
	Result := StringReplace(inUrl, cThFrontSlash, cThBackSlash,
		[ rfReplaceAll ]);
	{$endif}
end;

function ThEscapePath(const inPath: string): string;
const
	cDoubleBackslash = cThBackSlash + cThBackSlash;
begin
	Result := StringReplace(inPath, cThBackSlash,
		cDoubleBackslash, [ rfReplaceAll ]);
end;

function ThIsFullPath(const inPath: string): Boolean;
begin
	Result := ExtractFileDrive(inPath) <> '';
end;

procedure ThMemCopyFile(const inSrc, inDst: string);
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

end.
