unit ThDbTextArea;

interface

uses
	Classes, DB, DBCtrls,
	ThTextArea, ThDbData;

type
	TThCustomDbTextArea = class(TThTextArea)
	private
		FData: TThDbData;
	protected
		function GetFieldName: string;
		function GetDataSource: TDataSource;
		procedure SetFieldName(const Value: string);
		procedure SetDataSource(const Value: TDataSource);
	protected
		procedure DataChange(Sender: TObject);
		function GetFieldText: string;
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	protected
		property Data: TThDbData read FData;
		property FieldName: string read GetFieldName write SetFieldName;
		property DataSource: TDataSource read GetDataSource write SetDataSource;
	end;
	//
	TThDbTextArea = class(TThCustomDbTextArea)
	published
		property FieldName;
		property DataSource;
	end;

implementation

{ TThCustomDbTextArea }

constructor TThCustomDbTextArea.Create(AOwner: TComponent);
begin
	inherited;
	FData := TThDbData.Create;
	FData.OnDataChange := DataChange;
end;

destructor TThCustomDbTextArea.Destroy;
begin
	FData.Free;
	inherited;
end;

procedure TThCustomDbTextArea.DataChange(Sender: TObject);
begin
	Lines.Text := GetFieldText;
	Invalidate;
end;

function TThCustomDbTextArea.GetFieldText: string;
begin
	Result := Data.FieldText;
end;

procedure TThCustomDbTextArea.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	Data.Notification(AComponent, Operation);
end;

function TThCustomDbTextArea.GetFieldName: string;
begin
	Result := Data.FieldName;
end;

function TThCustomDbTextArea.GetDataSource: TDataSource;
begin
	Result := Data.DataSource;
end;

procedure TThCustomDbTextArea.SetFieldName(const Value: string);
begin
	Data.FieldName := Value;
end;

procedure TThCustomDbTextArea.SetDataSource(const Value: TDataSource);
begin
	if DataSource <> nil then
		DataSource.RemoveFreeNotification(Self);
	Data.DataSource := Value;
	if DataSource <> nil then
		DataSource.FreeNotification(Self);
end;

end.
