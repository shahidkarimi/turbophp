unit ThWebVariable;

interface

uses
	Classes, ThRegExpr;

type
	TThValidator = class;
	//
	TThCustomDatum = class(TPersistent)
	private
		FWebValue: string;
		FWebName: string;
		FDisplayName: string;
	protected
		procedure SetDisplayName(const Value: string);
		procedure SetWebName(const Value: string);
		procedure SetWebValue(const Value: string); virtual;
	published
		property DisplayName: string read FDisplayName write SetDisplayName;
		property WebName: string read FWebName write SetWebName;
		property WebValue: string read FWebValue write SetWebValue;
	end;
	//
	TThDatum = class(TThCustomDatum)
	private
		FValidator: TThValidator;
		procedure SetValidator(const Value: TThValidator);
	protected
		procedure SetWebValue(const Value: string); override;
	public
		property Validator: TThValidator read FValidator write SetValidator;
	end;
	//
	TThValidator = class(TComponent)
	private
		FWebDatum: TThDatum;
		FWebValue: string;
		FOnFailValidate: TNotifyEvent;
	protected
		procedure SetOnFailValidate(const Value: TNotifyEvent);
	protected
		procedure DoFailValidate;
		function ValidateString(var ioString: string): Boolean; virtual; abstract;
	public
		function Validate(inDatum: TThDatum): Boolean; virtual;
		property WebDatum: TThDatum read FWebDatum;
		property WebValue: string read FWebValue write FWebValue;
	published
		property OnFailValidate: TNotifyEvent read FOnFailValidate
			write SetOnFailValidate;
	end;
	//
	TThCustomWebVariable = class(TComponent)
	private
		FDatum: TThDatum;
	protected
		function GetDisplayName: string;
		function GetValidator: TThValidator;
		function GetWebName: string;
		function GetWebValue: string; virtual;
		procedure SetDisplayName(const Value: string);
		procedure SetValidator(const Value: TThValidator);
		procedure SetWebName(const Value: string);
		procedure SetWebValue(const Value: string);
	protected
		function DoMacroReplace(ARegExpr: TRegExpr): string;
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		function MacroReplace(const inValue: string): string;
	protected
		property DisplayName: string read GetDisplayName write SetDisplayName;
		property Validator: TThValidator read GetValidator write SetValidator;
	public
		property WebName: string read GetWebName write SetWebName;
		property WebValue: string read GetWebValue write SetWebValue;
	end;
	//
	TThWebVariable = class(TThCustomWebVariable)
	published
		property DisplayName;
		property Validator;
		property WebName;
		property WebValue;
	end;

implementation

{ TThCustomDatum }

procedure TThCustomDatum.SetDisplayName(const Value: string);
begin
	FDisplayName := Value;
end;

procedure TThCustomDatum.SetWebName(const Value: string);
begin
	if FWebName = FDisplayName then
		FDisplayName := Value;
	FWebName := Value;
end;

procedure TThCustomDatum.SetWebValue(const Value: string);
begin
	FWebValue := Value;
end;

{ TThDatum }

procedure TThDatum.SetValidator(const Value: TThValidator);
begin
	FValidator := Value;
end;

procedure TThDatum.SetWebValue(const Value: string);
begin
	inherited;
	if FValidator <> nil then
		if not FValidator.Validate(Self) then
			FWebValue := FValidator.WebValue;
end;

{ TThCustomWebVariable }

constructor TThCustomWebVariable.Create(AOwner: TComponent);
begin
	inherited;
	FDatum := TThDatum.Create;
end;

destructor TThCustomWebVariable.Destroy;
begin
	FDatum.Free;
	inherited;
end;

function TThCustomWebVariable.GetValidator: TThValidator;
begin
	Result := FDatum.FValidator;
end;

procedure TThCustomWebVariable.SetValidator(const Value: TThValidator);
begin
	if Validator <> nil then
		Validator.RemoveFreeNotification(Self);
	FDatum.FValidator := Value;
	if Validator <> nil then
		Validator.FreeNotification(Self);
end;

procedure TThCustomWebVariable.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited Notification(AComponent, Operation);
	if (Operation = opRemove) and (AComponent = Validator) then
		FDatum.FValidator := nil;
end;

function TThCustomWebVariable.GetWebName: string;
begin
	Result := FDatum.WebName;
end;

function TThCustomWebVariable.GetWebValue: string;
begin
	Result := MacroReplace(FDatum.WebValue);
end;

procedure TThCustomWebVariable.SetWebName(const Value: string);
begin
	FDatum.WebName := Value;
end;

procedure TThCustomWebVariable.SetWebValue(const Value: string);
begin
	FDatum.WebValue := Value;
end;

function TThCustomWebVariable.GetDisplayName: string;
begin
	Result := FDatum.DisplayName;
end;

procedure TThCustomWebVariable.SetDisplayName(const Value: string);
begin
	FDatum.DisplayName := Value;
end;

function TThCustomWebVariable.DoMacroReplace(ARegExpr: TRegExpr): string;
var
	i: Integer;
begin
	Result := '';
	if Owner <> nil then
		for i := 0 to Pred(Owner.ComponentCount) do
			if Owner.Components[i] is TThCustomWebVariable then
				with TThCustomWebVariable(Owner.Components[i]) do
					if ARegExpr.Match[1] = WebName then
					begin
						Result := WebValue;
						break;
					end;
end;

function TThCustomWebVariable.MacroReplace(const inValue: string): string;
begin
	with TRegExpr.Create do
	try
		Expression := '\{%([^}]*)\}';
		Result := ReplaceEx(inValue, DoMacroReplace);
	finally
		Free;
	end;
end;

{ TThValidator }

procedure TThValidator.SetOnFailValidate(const Value: TNotifyEvent);
begin
	FOnFailValidate := Value;
end;

procedure TThValidator.DoFailValidate;
begin
	if Assigned(OnFailValidate) then
		OnFailValidate(Self);
end;

function TThValidator.Validate(inDatum: TThDatum): Boolean;
begin
	FWebDatum := inDatum;
	FWebValue := inDatum.WebValue;
	Result := ValidateString(FWebValue);
	if not Result then
		DoFailValidate;
end;

end.
