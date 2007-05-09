unit ScriptPanel;

interface

uses
	Classes, ExtCtrls,
	DynamicProperties;

type
	TScriptPanel = class(TPanel)
	private
		FEvents: TDynamicProperties;
		FCustomProps: TDynamicProperties;
	protected
		procedure SetCustomProps(const Value: TDynamicProperties);
		procedure SetEvents(const Value: TDynamicProperties);
	public
		constructor Create(inOwner: TComponent); override;
	published
		property Events: TDynamicProperties read FEvents write SetEvents;
		property CustomProps: TDynamicProperties read FCustomProps
			write SetCustomProps;
	end;

implementation

{ TScriptPanel }

constructor TScriptPanel.Create(inOwner: TComponent);
begin
	inherited;
	//
	FEvents := TDynamicProperties.Create(Self, true);
	Events.AddEvent.Name := 'OnEvent';
	//
	FCustomProps := TDynamicProperties.Create(Self);
	CustomProps.AddProperty.Name := 'Attributes';
	CustomProps.AddProperty.Name := 'MetaTags';
end;

procedure TScriptPanel.SetCustomProps(const Value: TDynamicProperties);
begin
	FCustomProps.Assign(Value);
end;

procedure TScriptPanel.SetEvents(const Value: TDynamicProperties);
begin
	Events.Assign(Value);
end;

end.
