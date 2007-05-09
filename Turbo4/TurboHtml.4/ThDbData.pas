unit ThDbData;

interface

uses
	Classes, DB, DBCtrls;

type
	TThDbData = class(TFieldDataLink)
	protected
		function GetFieldText: string;
	public
		procedure Notification(AComponent: TComponent; Operation: TOperation);
	public
		property FieldText: string read GetFieldText;
	end;

implementation

{ TThDbData }

function TThDbData.GetFieldText: string;
begin
	if Field <> nil then
	begin
		if Field.DataType = ftMemo then
			Result := Field.AsString
		else
			Result := Field.DisplayText;
	end else
		Result := '';
end;

procedure TThDbData.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	//if (Operation = opRemove) and	(AComponent = DataSource) then
	//	DataSource := nil;
end;

end.
