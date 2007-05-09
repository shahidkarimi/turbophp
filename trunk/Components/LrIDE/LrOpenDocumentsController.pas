unit LrOpenDocumentsController;

interface

uses
	Classes, Controls,
	LrObserverList, LrDocument, LrDocumentList;

type
	TLrOpenDocumentsController = class(TLrDocumentList)
	private
		FNilDocument: TLrDocument;
		FObservers: TLrObserverList;
		FSelectedIndex: Integer;
	protected
		function GetCurrent: TLrDocument;
		procedure FinalizeCurrent;
		procedure SetSelectedIndex(const Value: Integer);
	public
		constructor Create; override;
		destructor Destroy; override;
		function BeginCloseCurrent: Boolean;
		function CloseAll: Boolean;
		function EndCloseCurrent: Boolean;
		procedure AddDocument(inDocument: TLrDocument);
		procedure CloseCurrent;
		procedure RemoveDocument(inDocument: TLrDocument);
		property Current: TLrDocument read GetCurrent;
		property Observers: TLrObserverList read FObservers write FObservers;
		property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
		//property OnCurrentChanged: TNotifyEvent read FOnCurrentChanged
		//	write FOnCurrentChanged;
	end;

var
	LrOpenDocuments: TLrOpenDocumentsController;

implementation

{ TLrOpenDocumentsController }

constructor TLrOpenDocumentsController.Create;
begin
	inherited;
	FNilDocument := TLrDocumentController.Create.CreateDocument(TLrDocument);
	//FNilDocument := TLrDocument.Create;
	//FNilDocument.Controller := TLrDocumentController.Create;
	FObservers := TLrObserverList.Create;
end;

destructor TLrOpenDocumentsController.Destroy;
begin
	FObservers.Free;
	FNilDocument.Free;
	inherited;
end;

function TLrOpenDocumentsController.GetCurrent: TLrDocument;
begin
	if GoodIndex(SelectedIndex) then
		Result := Documents[SelectedIndex]
	else
		Result := FNilDocument;
end;

procedure TLrOpenDocumentsController.SetSelectedIndex(const Value: Integer);
begin
	Current.Deactivate;
	FSelectedIndex := Value;
	Change;
	Current.Activate;
end;

procedure TLrOpenDocumentsController.FinalizeCurrent;
begin
	if not GoodIndex(SelectedIndex) then
		FSelectedIndex := Pred(Count);
	Current.Activate;
	Change;
end;

function TLrOpenDocumentsController.BeginCloseCurrent: Boolean;
begin
	Result := Current.Close;
end;

function TLrOpenDocumentsController.EndCloseCurrent: Boolean;
begin
	Result := Current.Closed;
	if Result then
	begin
		Current.Deactivate;
		Current.Free;
		RemoveDocument(Current);
	end;
end;

procedure TLrOpenDocumentsController.CloseCurrent;
begin
	if BeginCloseCurrent then
		EndCloseCurrent;
end;

procedure TLrOpenDocumentsController.AddDocument(inDocument: TLrDocument);
begin
	if inDocument <> nil then
	begin
		Add(inDocument);
		SelectedIndex := Pred(Count);
	end;
end;

procedure TLrOpenDocumentsController.RemoveDocument(inDocument: TLrDocument);
begin
	Remove(inDocument);
	FinalizeCurrent;
end;

function TLrOpenDocumentsController.CloseAll: Boolean;
var
	i: Integer;
begin
	i := Pred(Count);
	Result := true;
	while (i >= 0) and Result do
	begin
		// True result to Close method indicates only that the close
		// was not cancelled. It does not indicate that the
		// document actually is closed.
		// Check Closed property to test for true closure.
		Result := Documents[i].Close;
		if Result then
		begin
			if Documents[i].Closed then
			begin
				Documents[i].Deactivate;
				Documents[i].Free;
				RemoveDocument(Documents[i]);
			end;
			Dec(i);
		end;
	end;
	FinalizeCurrent;
end;

{
procedure TLrOpenDocumentsController.DocumentListChange(inSender: TObject);
begin
	FObservers.Notify(inSender);
end;

procedure TLrOpenDocumentsController.CurrentChanged;
begin
	if Assigned(FOnCurrentChanged) then
		FOnCurrentChanged(Self);
end;

procedure TLrOpenDocumentsController.DocumentsChanged;
begin
	DocumentChange(Self);
//	if Assigned(OnDocumentsChanged) then
//		OnDocumentsChanged(Self);
end;

procedure TLrOpenDocumentsController.AddDocument(inDocument: TLrDocument);
begin
	Documents.AddDocument(inDocument);
end;

procedure TLrOpenDocumentsController.RemoveDocument(inDocument: TLrDocument);
begin
	Documents.RemoveDocument(inDocument);
end;

procedure TLrOpenDocumentsController.SetIndex(const Value: Integer);
begin
	Current.Deactivate;
	FIndex := Value;
	DocumentsChanged;
	//CurrentChanged;
	Current.Activate;
end;

procedure TLrOpenDocumentsController.SetCurrent(inDocument: TLrDocument);
begin
	Index := FindDocument(inDocument);
end;

procedure TLrOpenDocumentsController.ValidateCurrent;
begin
	if not GoodIndex(Index) then
		Index := Pred(Count)
	else begin
		DocumentsChanged;
		//CurrentChanged;
		Current.Activate;
	end;
end;
}

end.
