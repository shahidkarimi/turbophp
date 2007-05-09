unit TpControls;

interface

uses
	SysUtils, ThTag, ThWebControl;

const
	tpClass = 'tpClass';

type
	TTpEvent = string;
	TTpEventType = procedure(inSender: TObject) of object;
	//
	TTpGraphicControl = class(TThWebGraphicControl)
	private
		FOnGenerate: TTpEvent;
	protected
		procedure Tag(inTag: TThTag); override;
	protected
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
	end;
	//
	TTpControl = class(TThWebControl)
	private
		FOnGenerate: TTpEvent;
	protected
		procedure Tag(inTag: TThTag); override;
	protected
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
	end;

function TpAttr(const inName, inValue: string): string; overload;
function TpAttr(const inName: string; inValue: Integer): string; overload;

function PropertyIsJsEvent(const inName: string): Boolean;
function PropertyIsPhpEvent(const inName: string): Boolean;

implementation

{.$R TpPaletteIcons.res}

function TpAttr(const inName, inValue: string): string;
begin
	if (inValue = '') then
		Result := ''
	else
		Result := ' ' + inName + '="' + inValue + '"';
end;

function TpAttr(const inName: string; inValue: Integer): string;
begin
	Result := TpAttr(inName, IntToStr(inValue));
end;

function PropertyIsJsEvent(const inName: string): Boolean;
begin
	Result := Copy(inName, 1, 2) = 'on';
end;

function PropertyIsPhpEvent(const inName: string): Boolean;
begin
	Result := Copy(inName, 1, 2) = 'On';
end;

{ TTpGraphicControl }

procedure TTpGraphicControl.Tag(inTag: TThTag);
begin
	inherited;
	inTag.Add('tpOnGenerate', OnGenerate);
end;

{ TTpControl }

procedure TTpControl.Tag(inTag: TThTag);
begin
	inherited;
	inTag.Add('tpOnGenerate', OnGenerate);
end;

end.
