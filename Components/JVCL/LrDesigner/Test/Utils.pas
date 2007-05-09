unit Utils;

interface

uses
	SysUtils, Windows, Classes, Controls, Forms, Graphics;

procedure AddForm(var outForm; inFormClass: TFormClass; inParent: TWinControl;
	inAlign: TAlign = alClient; inShow: Boolean = true);

procedure SaveComponentToStream(inComp: TComponent; inStream: TStream);
function LoadComponentFromStream(inComp: TComponent; inStream: TStream;
	inOnError: TReaderError): TComponent;

procedure SaveComponentToFile(inComp: TComponent; const inFilename: string);
procedure LoadComponentFromFile(inComp: TComponent; const inFilename: string;
	inOnError: TReaderError);

procedure PaintRules(inCanvas: TCanvas; const inRect: TRect;
	inDivs: Integer = 32; inSubDivs: Boolean = true);

implementation

procedure AddForm(var outForm; inFormClass: TFormClass; inParent: TWinControl;
	inAlign: TAlign = alClient; inShow: Boolean = true);
begin
	TForm(outForm) := inFormClass.Create(nil);
	with TForm(outForm) do
	begin
		BorderIcons := [];
		Caption := '';
		BorderStyle := bsNone;
		Align := inAlign;
		Parent := inParent;
		Visible := inShow;
	end;
end;

procedure SaveComponentToStream(inComp: TComponent; inStream: TStream);
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

procedure SaveComponentToFile(inComp: TComponent; const inFilename: string);
var
	fs: TFileStream;
begin
	fs := TFileStream.Create(inFilename, fmCreate);
	try
		SaveComponentToStream(inComp, fs);
	finally
		fs.Free;
	end;
end;

function LoadComponentFromStream(inComp: TComponent; inStream: TStream;
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

procedure LoadComponentFromFile(inComp: TComponent; const inFilename: string;
	inOnError: TReaderError);
var
	fs: TFileStream;
begin
	if FileExists(inFilename) then
	begin
		fs := TFileStream.Create(inFilename, fmOpenRead);
		try
			LoadComponentFromStream(inComp, fs, inOnError);
		finally
			fs.Free;
		end;
	end;
end;

procedure PaintRules(inCanvas: TCanvas; const inRect: TRect; inDivs: Integer;
	inSubDivs: Boolean);
var
	d, d2, w, h, i: Integer;
begin
	d := inDivs;
	d2 := inDivs div 2;
	w := (inRect.Right - inRect.Left + d - 1) div d;
	h := (inRect.Bottom - inRect.Top + d - 1) div d;
	with inCanvas do
	begin
		Pen.Style := psDot;
		for i := 0 to w do
		begin
			Pen.Color := $DDDDDD;
			MoveTo(i * d, inRect.Top);
			LineTo(i * d, inRect.Bottom);
			if inSubDivs then
			begin
				Pen.Color := $F0F0F0;
				MoveTo(i * d + d2, inRect.Top);
				LineTo(i * d + d2, inRect.Bottom);
			end;
		end;
		for i := 0 to h do
		begin
			Pen.Color := $DDDDDD;
			MoveTo(inRect.Left, i * d);
			LineTo(inRect.Right, i * d);
			if inSubDivs then
			begin
				Pen.Color := $F0F0F0;
				MoveTo(inRect.Left, i * d + d2);
				LineTo(inRect.Right, i * d + d2);
			end;
		end;
	end;
end;

end.
