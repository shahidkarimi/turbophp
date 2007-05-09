unit TpDbTextArea;

interface

uses
	Windows, SysUtils, Classes, Controls, Graphics,
	Types,
	ThWebControl, ThTag, ThDbTextArea,
	TpControls, TpDb;

type
	TTpDbTextArea = class(TThCustomDbTextArea)
	private
		FDataSource: TTpDataSource;
		FOnGenerate: TTpEvent;
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
	end;

implementation

{ TTpDbTextArea }

procedure TTpDbTextArea.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	if (Operation = opRemove) and (AComponent = FDataSource) then
	begin
		FDataSource := nil;
		Data.DataSource := nil;
	end;
end;

procedure TTpDbTextArea.SetDataSource(const Value: TTpDataSource);
begin
	TpSetDataSource(Self, Value, FDataSource, Data);
end;

procedure TTpDbTextArea.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpDbTextArea');
		Add('tpName', Name);
		if (DataSource <> nil) and not DataSource.DesignOnly then
			Add('tpDataSource', DataSource.Name);
		Add('tpField', FieldName);
		Add('tpOnGenerate', OnGenerate);
	end;
end;

end.
