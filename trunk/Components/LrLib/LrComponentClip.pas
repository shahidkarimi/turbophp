unit LrComponentClip;

interface

uses
	Windows, Classes;

type
	TLrComponentClipboard = class
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

implementation

uses
	Clipbrd, LrVclUtils;

var
	CF_COMPONENTSTREAM: Cardinal;

{ TLrComponentClipboard }

procedure TLrComponentClipboard.Close;
begin
	Stream.Free;
	Clipboard.Close;
end;

procedure TLrComponentClipboard.CloseRead;
begin
	Close;
end;

procedure TLrComponentClipboard.CloseWrite;
begin
	LrCopyStreamToClipboard(CF_COMPONENTSTREAM, Stream);
	Close;
end;

function TLrComponentClipboard.GetComponent: TComponent;
begin
	if Stream.Position < Stream.Size then
		Result := LrLoadComponentFromBinaryStream(nil, Stream, ReadError)
	else
		Result := nil;
end;

procedure TLrComponentClipboard.Open;
begin
	Clipboard.Open;
	Stream := TMemoryStream.Create;
end;

procedure TLrComponentClipboard.OpenRead;
begin
	Open;
	LrCopyStreamFromClipboard(CF_COMPONENTSTREAM, Stream);
end;

procedure TLrComponentClipboard.OpenWrite;
begin
	Open;
end;

procedure TLrComponentClipboard.ReadError(Reader: TReader;
	const Message: string; var Handled: Boolean);
begin
	Handled := true;
end;

procedure TLrComponentClipboard.SetComponent(inComponent: TComponent);
begin
	LrSaveComponentToBinaryStream(inComponent, Stream);
end;

initialization
  { The following string should not be localized }
	CF_COMPONENTSTREAM := RegisterClipboardFormat('Delphi Components');
end.
