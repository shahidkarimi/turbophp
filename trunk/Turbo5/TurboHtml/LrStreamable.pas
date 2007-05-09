unit LrStreamable;

interface

uses
	SysUtils, Classes;

type
	TLrStreamable = class(TComponent)
	protected
		procedure ReadError(Reader: TReader; const Message: string;
			var Handled: Boolean);
	public
		function SaveToText: string;
		procedure LoadBinaryFromStream(inStream: TStream);
		procedure LoadFromFile(const inFilename: string);  virtual;
		procedure LoadFromStream(inStream: TStream);
		procedure LoadFromText(const inText: string);
		procedure SaveBinaryToStream(inStream: TStream);
		procedure SaveToFile(const inFilename: string); virtual;
		procedure SaveToStream(inStream: TStream);
	end;

implementation

uses
	LrVclUtils;

{ TLrStreamable }

procedure TLrStreamable.SaveToStream(inStream: TStream);
begin
	LrSaveComponentToStream(Self, inStream);
end;

function TLrStreamable.SaveToText: string;
begin
	Result := LrSaveComponentToString(Self);
end;

procedure TLrStreamable.SaveToFile(const inFilename: string);
begin
	LrSaveComponentToFile(Self, inFilename);
end;

procedure TLrStreamable.ReadError(Reader: TReader; const Message: string;
	var Handled: Boolean);
begin
	Handled := true;
end;

procedure TLrStreamable.LoadFromStream(inStream: TStream);
begin
	LrLoadComponentFromStream(Self, inStream, ReadError);
end;

procedure TLrStreamable.LoadFromText(const inText: string);
begin
	LrLoadComponentFromString(inText, Self, ReadError);
end;

procedure TLrStreamable.LoadFromFile(const inFilename: string);
begin
	LrLoadComponentFromFile(Self, inFilename, ReadError);
end;

procedure TLrStreamable.SaveBinaryToStream(inStream: TStream);
var
	ms: TMemoryStream;
	sz: Int64;
begin
	ms := TMemoryStream.Create;
	try
		ms.WriteComponent(Self);
		ms.Position := 0;
		with inStream do
		begin
			sz := ms.Size;
			Write(sz, 8);
			CopyFrom(ms, sz);
		end;
	finally
		ms.Free;
	end;
end;

procedure TLrStreamable.LoadBinaryFromStream(inStream: TStream);
var
	ms: TMemoryStream;
	sz: Int64;
begin
	ms := TMemoryStream.Create;
	try
		inStream.Read(sz, 8);
		ms.CopyFrom(inStream, sz);
		ms.Position := 0;
		ms.ReadComponent(Self);
	finally
		ms.Free;
	end;
end;

end.
