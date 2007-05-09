unit TpDbEdit;

interface

uses
	Windows, SysUtils, Classes, Controls, Graphics,
	Types,
	ThWebControl, ThTag, ThDbEdit,
	TpControls, TpDb;

type
	TTpDbEdit = class(TThCustomDbEdit)
	private
		FDataSource: TTpDataSource;
		FOnGenerate: TTpEvent;
    FOnInput: TTpEvent;
	protected
		procedure SetDataSource(const Value: TTpDataSource);
	protected
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
		procedure Tag(inTag: TThTag); override;
	published
		property FieldName;
		property DataSource: TTpDataSource read FDataSource write SetDataSource;
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
		property OnInput: TTpEvent read FOnInput write FOnInput;
	end;

implementation

{ TTpDbEdit }

procedure TTpDbEdit.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	if (Operation = opRemove) and (AComponent = FDataSource) then
	begin
		FDataSource := nil;
		Data.DataSource := nil;
	end;
end;

procedure TTpDbEdit.SetDataSource(const Value: TTpDataSource);
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

procedure TTpDbEdit.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add('tpClass', 'TTpDbEdit');
		Add('tpName', Name);
		if (DataSource <> nil) and not DataSource.DesignOnly then
			Add('tpDataSource', DataSource.Name);
		Add('tpField', FieldName);
		Add('tpOnSubmit', OnInput);
		Add('tpOnGenerate', OnGenerate);
	end;
end;

end.
