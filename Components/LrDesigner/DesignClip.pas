unit DesignClip;

interface

uses
  Windows, Classes;

type
  TDesignComponentClipboard = class
  protected
    Stream: TMemoryStream;
    procedure Close;
    procedure Open;
    procedure ReadError(Reader: TReader; const Message: string;
      var Handled: Boolean);
  public
    function GetComponent: TComponent;
    procedure CloseRead;
    procedure CloseWrite;
    procedure OpenRead;
    procedure OpenWrite;
		procedure SetComponent(inComponent: TComponent);
	end;
	//
	TDesignComponentClipboardReader = class(TDesignComponentClipboard)
	public
		constructor Create;
		destructor Destroy; override;
	end;
	//
	TDesignComponentClipboardWriter = class(TDesignComponentClipboard)
	public
		constructor Create;
		destructor Destroy; override;
	end;

function DesignLoadComponentFromBinaryStream(inStream: TStream;
  inComp: TComponent; inOnError: TReaderError): TComponent;
procedure DesignSaveComponentToBinaryStream(inStream: TStream;
  inComp: TComponent);
procedure DesignCopyStreamFromClipboard(inFmt: Cardinal; inS: TStream);
procedure DesignCopyStreamToClipboard(inFmt: Cardinal; inS: TStream);

implementation

uses
	SysUtils, Clipbrd, DesignUtils;

var
  CF_COMPONENTSTREAM: Cardinal;

procedure DesignSaveComponentToBinaryStream(inStream: TStream;
  inComp: TComponent);
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

function DesignLoadComponentFromBinaryStream(inStream: TStream;
  inComp: TComponent; inOnError: TReaderError): TComponent;
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

procedure DesignCopyStreamToClipboard(inFmt: Cardinal; inS: TStream);
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

procedure DesignCopyStreamFromClipboard(inFmt: Cardinal; inS: TStream);
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

{ TDesignComponentClipboard }

procedure TDesignComponentClipboard.Close;
begin
  Stream.Free;
  Clipboard.Close;
end;

procedure TDesignComponentClipboard.CloseRead;
begin
  Close;
end;

procedure TDesignComponentClipboard.CloseWrite;
begin
  DesignCopyStreamToClipboard(CF_COMPONENTSTREAM, Stream);
  Close;
end;

function TDesignComponentClipboard.GetComponent: TComponent;
begin
  if Stream.Position < Stream.Size then
    Result := DesignLoadComponentFromBinaryStream(Stream, nil, ReadError)
  else
    Result := nil;
end;

procedure TDesignComponentClipboard.Open;
begin
  Clipboard.Open;
  Stream := TMemoryStream.Create;
end;

procedure TDesignComponentClipboard.OpenRead;
begin
	Open;
	DesignCopyStreamFromClipboard(CF_COMPONENTSTREAM, Stream);
end;

procedure TDesignComponentClipboard.OpenWrite;
begin
	Open;
end;

procedure TDesignComponentClipboard.ReadError(Reader: TReader;
	const Message: string; var Handled: Boolean);
begin
	Handled := true;
end;

procedure TDesignComponentClipboard.SetComponent(inComponent: TComponent);
begin
	DesignSaveComponentToBinaryStream(Stream, inComponent);
end;

{ TDesignComponentClipboardReader }

constructor TDesignComponentClipboardReader.Create;
begin
	OpenRead;
end;

destructor TDesignComponentClipboardReader.Destroy;
begin
	CloseRead;
	inherited;
end;

{ TDesignComponentClipboardWriter }

constructor TDesignComponentClipboardWriter.Create;
begin
	OpenWrite;
end;

destructor TDesignComponentClipboardWriter.Destroy;
begin
	CloseWrite;
  inherited;
end;

initialization
	{ The following string should not be localized }
	CF_COMPONENTSTREAM := RegisterClipboardFormat('Delphi Components');
end.

