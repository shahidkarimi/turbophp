unit TpPersistComponent;

interface

uses
	SysUtils, Classes;

type
	TTpPersistComponent = class(TComponent)
	public
		procedure SaveToStream(inStream: TStream); virtual;
		procedure SaveToFile(const inFilename: string); virtual;
		function SaveToText: string; virtual;
		procedure LoadFromStream(inStream: TStream); virtual;
		procedure LoadFromFile(const inFilename: string); virtual;
		procedure LoadFromText(inOptsText: string); virtual;
	end;

implementation

{ TTpPersistComponent }

procedure TTpPersistComponent.SaveToStream(inStream: TStream);
var
	ms: TMemoryStream;
begin
	ms := TMemoryStream.Create;
	try
		ms.WriteComponent(Self);
		ms.Position := 0;
		ObjectBinaryToText(ms, inStream);
	finally
		ms.Free;
	end;
end;

function TTpPersistComponent.SaveToText: string;
var
	ss: TStringStream;
begin
	ss := TStringStream.Create('');
	try
		SaveToStream(ss);
		Result := ss.DataString;
	finally
		ss.Free;
	end;
end;

procedure TTpPersistComponent.SaveToFile(const inFilename: string);
var
	fs: TFileStream;
begin
	fs := TFileStream.Create(inFilename, fmCreate);
	try
		SaveToStream(fs);
	finally
		fs.Free;
	end;
end;

procedure TTpPersistComponent.LoadFromStream(inStream: TStream);
var
	ms: TMemoryStream;
begin
	ms := TMemoryStream.Create;
	try
		ObjectTextToBinary(inStream, ms);
		ms.Position := 0;
		ms.ReadComponent(Self);
	finally
		ms.Free;
	end;
end;

procedure TTpPersistComponent.LoadFromText(inOptsText: string);
var
	ss: TStringStream;
begin
	ss := TStringStream.Create(inOptsText);
	try
		LoadFromStream(ss);
	finally
		ss.Free;
	end;
end;

procedure TTpPersistComponent.LoadFromFile(const inFilename: string);
var
	fs: TFileStream;
begin
	if not FileExists(inFilename) then
		exit;
	fs := TFileStream.Create(inFilename, fmOpenRead);
	try
		LoadFromStream(fs);
	finally
		fs.Free;
	end;
end;

end.
