unit TpWebVariable;

interface

uses
	SysUtils, Classes,
	Db, AdoDb,
	ThHtmlDocument, ThHeaderComponent, ThTag,
	TpControls, TpForm;

type
	TTpWebVariable = class(TThHeaderComponent)
	private
		FDefaultValue: string;
		FForm: TTpForm;
	protected
		procedure SetDefaultValue(const Value: string);
		procedure SetForm(const Value: TTpForm);
	protected
		procedure Tag(inTag: TThTag); override;
		procedure UnassignComponent(AComponent: TComponent); override;
	published
		property DefaultValue: string read FDefaultValue write SetDefaultValue;
		property Form: TTpForm read FForm write SetForm;
	end;

implementation

procedure TTpWebVariable.SetDefaultValue(const Value: string);
begin
	FDefaultValue := Value;
end;

procedure TTpWebVariable.SetForm(const Value: TTpForm);
begin
	ChangeComponentProp(TComponent(FForm), Value);
end;

procedure TTpWebVariable.UnassignComponent(AComponent: TComponent);
begin
	if AComponent = Form then
		FForm := nil;
end;

procedure TTpWebVariable.Tag(inTag: TThTag);
begin
	with inTag do
	begin
		Add(tpClass, 'TTpWebVar');
		Add('tpName', Name);
		Add('tpValue', DefaultValue);
		if Assigned(Form) then
			Add('tpForm', Form.Name);
	end;
end;

end.
