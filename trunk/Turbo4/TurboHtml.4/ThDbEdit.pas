unit ThDbEdit;

interface

uses
	Classes, DB, DBCtrls,
	ThInput, ThDbData;

type
	TThCustomDbEdit = class(TThInput)
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
	TThDbEdit = class(TThCustomDbEdit)
	published
		property FieldName;
		property DataSource;
	end;

implementation

{ TThCustomDbEdit }

constructor TThCustomDbEdit.Create(AOwner: TComponent);
begin
	inherited;
	FData := TThDbData.Create;
	FData.OnDataChange := DataChange;
end;

destructor TThCustomDbEdit.Destroy;
begin
	FData.Free;
	inherited;
end;

procedure TThCustomDbEdit.DataChange(Sender: TObject);
begin
	Caption := GetFieldText;
	Invalidate;
end;

function TThCustomDbEdit.GetFieldText: string;
begin
	Result := Data.FieldText;
end;

procedure TThCustomDbEdit.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	Data.Notification(AComponent, Operation);
end;

function TThCustomDbEdit.GetFieldName: string;
begin
	Result := Data.FieldName;
end;

function TThCustomDbEdit.GetDataSource: TDataSource;
begin
	Result := Data.DataSource;
end;

procedure TThCustomDbEdit.SetFieldName(const Value: string);
begin
	Data.FieldName := Value;
end;

procedure TThCustomDbEdit.SetDataSource(const Value: TDataSource);
begin
	if DataSource <> nil then
		DataSource.RemoveFreeNotification(Self);
	Data.DataSource := Value;
	if DataSource <> nil then
		DataSource.FreeNotification(Self);
end;

end.
