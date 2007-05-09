{-----------------------------------------------------------------------------

 Least-Resistance Designer Library

 The Initial Developer of the Original Code is Scott J. Miles
	<sjmiles (at) turbophp (dot) com>.
 Portions created by Scott J. Miles are
	Copyright (C) 2005 Least-Resistance Software.
 All Rights Reserved.

-------------------------------------------------------------------------------}

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

implementation

uses
	Clipbrd, DesignUtils;

var
	CF_COMPONENTSTREAM: Cardinal;

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
	LrCopyStreamToClipboard(CF_COMPONENTSTREAM, Stream);
	Close;
end;

function TDesignComponentClipboard.GetComponent: TComponent;
begin
	if Stream.Position < Stream.Size then
		Result := LrLoadComponentFromBinaryStream(Stream, nil, ReadError)
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
	LrCopyStreamFromClipboard(CF_COMPONENTSTREAM, Stream);
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
	LrSaveComponentToBinaryStream(Stream, inComponent);
end;

initialization
	{ The following string should not be localized }
	CF_COMPONENTSTREAM := RegisterClipboardFormat('Delphi Components');
end.
