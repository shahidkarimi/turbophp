unit TpDbText;

interface

uses
	Windows, SysUtils, Classes, Controls, Graphics,
	Types, TypInfo,
	ThWebControl, ThTag, ThDbText,
	TpControls, TpDb;

type
	TTpDbText = class(TThCustomDbText)
	private
		FDataSource: TTpDataSource;
		FOnGenerate: TTpEvent;
	protected
		procedure SetDataSource(const Value: TTpDataSource);
	protected
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
		procedure LabelTag(inTag: TThTag); override;
	published
		property FieldName;
		property DataSource: TTpDataSource read FDataSource write SetDataSource;
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
	end;

implementation

function GetModularName(inComponent: TComponent): string;
begin
	if inComponent = nil then
		Result := ''
	else begin
		Result := inComponent.Name;
		if (inComponent.Owner <> nil) and (inComponent.Owner is TWinControl) then
		begin
			if TWinControl(inComponent.Owner).Parent.ClassName = 'TTpModule' then
				Result := TWinControl(inComponent.Owner).Parent.Name + '.' + Result;
		end;
	end;
end;

{ TTpDbText }

procedure TTpDbText.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	if (Operation = opRemove) and (AComponent = FDataSource) then
	begin
		FDataSource := nil;
		Data.DataSource := nil;
	end;
end;

procedure TTpDbText.SetDataSource(const Value: TTpDataSource);
begin
	TpSetDataSource(Self, Value, FDataSource, Data);
{
	if FDataSource <> nil then
		FDataSource.RemoveFreeNotification(Self);
	//
	FDataSource := Value;
	//
	if FDataSource <> nil then
		FDataSource.FreeNotification(Self);
	//
	if FDataSource <> nil then
		Data.DataSource := FDataSource.DataSource
	else
		Data.DataSource := nil;
}
end;

procedure TTpDbText.LabelTag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpDbText');
		Add('tpName', Name);
		if (DataSource <> nil) and not DataSource.DesignOnly then
			Add('tpDataSource', GetModularName(DataSource));
//			Add('tpDataSource', DataSource.Name);
		Add('tpField', FieldName);
		Add('tpOnGenerate', OnGenerate);
	end;
end;

end.
