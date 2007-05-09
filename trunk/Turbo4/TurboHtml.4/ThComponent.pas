unit ThComponent;

interface

uses
	SysUtils, Classes;

type
	TThComponent = class(TComponent)
	protected
		procedure ChangeComponentProp(var ioComponent; const inValue: TComponent);
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
		procedure UnassignComponent(AComponent: TComponent); virtual;
	public
		procedure LoadFromFile(const inFilename: string);
		procedure LoadFromStream(inStream: TStream);
		procedure LoadFromText(inOptsText: string);
		procedure SaveToFile(const inFilename: string);
		procedure SaveToStream(inStream: TStream);
		function SaveToText: string;
	end;

implementation

{ TThComponent }

procedure TThComponent.ChangeComponentProp(var ioComponent;
	const inValue: TComponent);
begin
	if TComponent(ioComponent) <> nil then
		TComponent(ioComponent).RemoveFreeNotification(Self);
	TComponent(ioComponent) := inValue;
	if TComponent(ioComponent) <> nil then
		TComponent(ioComponent).FreeNotification(Self);
end;

{
procedure TThComponent.ChangeComponentProp(var ioComponent: TComponent;
	const inValue: TComponent);
begin
	if ioComponent <> nil then
		ioComponent.RemoveFreeNotification(Self);
	ioComponent := inValue;
	if ioComponent <> nil then
		ioComponent.FreeNotification(Self);
end;
}

procedure TThComponent.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	if (Operation = opRemove) then
		UnassignComponent(AComponent);
end;

procedure TThComponent.UnassignComponent(AComponent: TComponent);
begin
	//
end;

procedure TThComponent.SaveToStream(inStream: TStream);
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

function TThComponent.SaveToText: string;
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

procedure TThComponent.SaveToFile(const inFilename: string);
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

procedure TThComponent.LoadFromStream(inStream: TStream);
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

procedure TThComponent.LoadFromText(inOptsText: string);
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

procedure TThComponent.LoadFromFile(const inFilename: string);
var
	fs: TFileStream;
begin
	if FileExists(inFilename) then
	begin
		fs := TFileStream.Create(inFilename, fmOpenRead);
		try
			LoadFromStream(fs);
		finally
			fs.Free;
		end;
	end;
end;

end.
