unit LrPersistComponent;

interface

uses
	SysUtils, Classes, Components;

type
	TLrPersistComponent = class(TComponent)
	protected
		procedure ManuallyLoaded; virtual;
	public
		function SaveToText: string; virtual;
		procedure SaveToStream(inStream: TStream); virtual;
		procedure SaveToFile(const inFilename: string); virtual;
		procedure LoadFromStream(inStream: TStream); virtual;
		procedure LoadFromFile(const inFilename: string); virtual;
		procedure LoadFromText(inOptsText: string); virtual;
	end;

implementation

{ TLrPersistComponent }

procedure TLrPersistComponent.SaveToStream(inStream: TStream);
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

function TLrPersistComponent.SaveToText: string;
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

procedure TLrPersistComponent.SaveToFile(const inFilename: string);
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

procedure TLrPersistComponent.LoadFromStream(inStream: TStream);
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
	ManuallyLoaded;
end;

procedure TLrPersistComponent.LoadFromText(inOptsText: string);
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

procedure TLrPersistComponent.LoadFromFile(const inFilename: string);
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

procedure TLrPersistComponent.ManuallyLoaded;
begin
	//
end;

end.
