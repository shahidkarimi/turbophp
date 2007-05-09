unit Config;

interface

uses
	SysUtils, Classes;

type
	TConfig = class(TComponent)
	private
		Filename: string;
		FParams: TStringList;
	protected
		function GetIntegers(inName: string): Integer;
		function GetValues(inName: string): string;
		procedure SetIntegers(inName: string; const Value: Integer);
		procedure SetParams(const Value: TStringList);
		procedure SetValues(inName: string; const Value: string);
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		function GetIntegerDef(const inName: string; inDefault: Integer = 0): Integer;
		procedure LoadFromFile(const inFilename: string);
		procedure Save;
		procedure SaveToFile(const inFilename: string);
		property Integers[inName: string]: Integer read GetIntegers
			write SetIntegers;
		property Values[inName: string]: string read GetValues write SetValues;
	published
		property Params: TStringList read FParams write SetParams;
	end;

var
	Configuration: TConfig;

implementation

uses
	LrVclUtils;

{ TConfig }

constructor TConfig.Create(inOwner: TComponent);
begin
	inherited;
	FParams := TStringList.Create;
end;

destructor TConfig.Destroy;
begin
	Save;
	Params.Free;
	inherited;
end;

function TConfig.GetIntegerDef(const inName: string;
	inDefault: Integer): Integer;
begin
	if Params.IndexOfName(inName) < 0 then
		Result := inDefault
	else
		Result := StrToIntDef(Params.Values[inName], inDefault);
end;

function TConfig.GetIntegers(inName: string): Integer;
begin
	Result := GetIntegerDef(inName, 0);
end;

procedure TConfig.SetIntegers(inName: string; const Value: Integer);
begin
	Params.Values[inName] := IntToStr(Value);
end;

procedure TConfig.LoadFromFile(const inFilename: string);
begin
	Filename := inFilename;
	if FileExists(Filename) then
		LrLoadComponentFromFile(Self, Filename);
end;

procedure TConfig.Save;
begin
	if Filename <> '' then
		SaveToFile(Filename);
end;

procedure TConfig.SaveToFile(const inFilename: string);
begin
	LrSaveComponentToFile(Self, inFilename);
end;

procedure TConfig.SetParams(const Value: TStringList);
begin
	FParams.Assign(Value);
end;

function TConfig.GetValues(inName: string): string;
begin
	if Params.IndexOfName(inName) >= 0 then
		Result := Params.Values[inName]
	else
		Result := '';
end;

procedure TConfig.SetValues(inName: string; const Value: string);
begin
	Params.Values[inName] := Value;
end;

initialization
	Configuration := TConfig.Create(nil);
finalization
	Configuration.Free;
end.
