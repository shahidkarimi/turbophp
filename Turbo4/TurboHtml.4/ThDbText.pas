unit ThDbText;

interface

uses
	Classes, DB, DBCtrls,
	ThLabel, ThDbData;

type
	TThCustomDbText = class(TThLabel)
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
	TThDbText = class(TThCustomDbText)
	published
		property FieldName;
		property DataSource;
	end;

implementation

{ TThCustomDbText }

constructor TThCustomDbText.Create(AOwner: TComponent);
begin
	inherited;
	FData := TThDbData.Create;
	FData.OnDataChange := DataChange;
end;

destructor TThCustomDbText.Destroy;
begin
	FData.Free;
	inherited;
end;

procedure TThCustomDbText.DataChange(Sender: TObject);
begin
	Caption := GetFieldText;
	Invalidate;
end;

function TThCustomDbText.GetFieldText: string;
begin
	Result := Data.FieldText;
end;

procedure TThCustomDbText.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	Data.Notification(AComponent, Operation);
end;

function TThCustomDbText.GetFieldName: string;
begin
	Result := Data.FieldName;
end;

function TThCustomDbText.GetDataSource: TDataSource;
begin
	Result := Data.DataSource;
end;

procedure TThCustomDbText.SetFieldName(const Value: string);
begin
	Data.FieldName := Value;
end;

procedure TThCustomDbText.SetDataSource(const Value: TDataSource);
begin
	if DataSource <> nil then
		DataSource.RemoveFreeNotification(Self);
	Data.DataSource := Value;
	if DataSource <> nil then
		DataSource.FreeNotification(Self);
end;

end.
