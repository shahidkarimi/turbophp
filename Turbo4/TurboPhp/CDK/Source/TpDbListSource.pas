unit TpDbListSource;

interface

uses
	Windows, SysUtils, Classes, Controls, Graphics,
	Types,
	ThWebControl, ThTag, ThDbListSource, 
	TpControls, TpDb;

type
	TTpDbListSource = class(TThDbListSource)
	private
		FDataSource: TTpDataSource;
	protected
		procedure SetDataSource(const Value: TTpDataSource);
	protected
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
		procedure Tag(inTag: TThTag); override;
	published
		property DataSource: TTpDataSource read FDataSource write SetDataSource;
	end;

implementation

{ TTpDbListSource }

procedure TTpDbListSource.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	if (Operation = opRemove) and (AComponent = FDataSource) then
	begin
		FDataSource := nil;
		Data.DataSource := nil;
	end;
end;

procedure TTpDbListSource.SetDataSource(const Value: TTpDataSource);
begin
	TpSetDataSource(Self, Value, FDataSource, Data);
end;

procedure TTpDbListSource.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpDbListSource');
		Add('tpName', Name);
		if (DataSource <> nil) and not DataSource.DesignOnly then
			Add('tpDataSource', DataSource.Name);
		Add('tpField', FieldName);
		//Add('tpOnGenerate', OnGenerate);
	end;
end;

end.
