unit MoreControls;

interface

uses
	SysUtils, Classes, Graphics,
	ThTag, ThWebControl, TpControls, TpLabel, TpInterfaces;

type
	TTpFormatLabel = class(TTpLabel, ITpIncludeLister)
	private
		FFormatString: string;
		FOnFormat: TTpEvent;
	protected
		function GetContent: string; override;
		function GetString: string; override;
		procedure SetFormatString(const Value: string); virtual;
		procedure SetOnFormat(const Value: TTpEvent);
	protected
		procedure ListPhpIncludes(inIncludes: TStringList);
		procedure LabelTag(inTag: TThTag); override;
	published
		property FormatString: string read FFormatString write SetFormatString;
		property OnFormat: TTpEvent read FOnFormat write SetOnFormat;
	end;
	//
	TTpDateLabel = class(TTpFormatLabel)
	private
		FDelphiFormat: string;
	protected
		function GetString: string; override;
		procedure SetFormatString(const Value: string); override;
	protected
		procedure LabelTag(inTag: TThTag); override;
	public
		constructor Create(inOwner: TComponent); override;
	end;
	//
	TTpSizeLabel = class(TTpLabel, ITpIncludeLister)
	private
		FSize: Integer;
		FOnFormat: TTpEvent;
	protected
		function GetString: string; override;
		procedure SetOnFormat(const Value: TTpEvent);
		procedure SetSize(const Value: Integer);
	protected
		procedure ListPhpIncludes(inIncludes: TStringList);
		procedure LabelTag(inTag: TThTag); override;
	public
		constructor Create(inOwner: TComponent); override;
	published
		property Size: Integer read FSize write SetSize;
		property OnFormat: TTpEvent read FOnFormat write SetOnFormat;
	end;

procedure Register;

implementation

{ $R MoreControlsIcons.res}

procedure Register;
begin
	RegisterComponents('More', [ TTpFormatLabel, TTpDateLabel, TTpSizeLabel ]);
end;

{ TTpFormatLabel }

function TTpFormatLabel.GetString: string;
begin
	if FormatString <> '' then
		Result := Format(FormatString, [ Caption ])
	else
		Result := Caption;
end;

function TTpFormatLabel.GetContent: string;
begin
	Result := Caption;
end;

procedure TTpFormatLabel.ListPhpIncludes(inIncludes: TStringList);
begin
	inIncludes.Add('TpMoreLib.php');
end;

procedure TTpFormatLabel.SetFormatString(const Value: string);
begin
	FFormatString := Value;
	Invalidate;
	AdjustSize;
end;

procedure TTpFormatLabel.SetOnFormat(const Value: TTpEvent);
begin
	FOnFormat := Value;
end;

procedure TTpFormatLabel.LabelTag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Attributes['tpClass'] := 'TTpFmtLabel';
		Add('tpFormatString', FormatString);
		Add('tpOnFormat', OnFormat);
	end;
end;

{ TTpDateLabel }

constructor TTpDateLabel.Create(inOwner: TComponent);
begin
	inherited;
end;

procedure TTpDateLabel.SetFormatString(const Value: string);
begin
	FDelphiFormat := StringReplace(Value, 'i', 'nn', [ rfReplaceAll ]);
	inherited;
end;

function TTpDateLabel.GetString: string;
begin
	//Result := FormatDateTime('c', Now);
	if FDelphiFormat = '' then
		FDelphiFormat := 'd-M-Y H:i';
	Result := FormatDateTime(FDelphiFormat, Now);
end;

procedure TTpDateLabel.LabelTag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Attributes['tpClass'] := 'TTpDateLabel';
	end;
end;

{ TTpSizeLabel }

constructor TTpSizeLabel.Create(inOwner: TComponent);
begin
	inherited;
	Size := 0;
end;

function TTpSizeLabel.GetString: string;
begin
	if Size > 512 * 1024 then
		Result := Format('%0.2f Mb', [ Size / 1024 / 1024 ])
	else if Size > 1024 then
		Result := Format('%0.2f Kb', [ Size / 1024 ])
	else
		Result := Format('%d bytes', [ Size ]);
end;

procedure TTpSizeLabel.ListPhpIncludes(inIncludes: TStringList);
begin
	inIncludes.Add('TpMoreLib.php');
end;

procedure TTpSizeLabel.SetOnFormat(const Value: TTpEvent);
begin
	FOnFormat := Value;
end;

procedure TTpSizeLabel.SetSize(const Value: Integer);
begin
	FSize := Value;
	Invalidate;
	AdjustSize;
end;

procedure TTpSizeLabel.LabelTag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Attributes['tpClass'] := 'TTpSizeLabel';
		Add('tpOnFormat', OnFormat);
		Add('tpSize', Size);
	end;
end;

end.
