unit Desktop;

interface

uses
	Classes, Controls,
	LrObserverList, LrDocument;

type
	TDesktop = class(TList)
	private
		FIndex: Integer;
		FLastIndex: Integer;
		FNilDocument: TLrDocument;
		FDocumentObservers: TLrObserverList;
		FOnDocumentsChanged: TNotifyEvent;
		FOnCurrentChanged: TNotifyEvent;
	protected
		function GetCurrent: TLrDocument;
		function GetDocuments(inIndex: Integer): TLrDocument;
		function GoodIndex: Boolean;
		procedure CurrentChanged;
		procedure DocumentsChanged;
		procedure DocumentChange(inSender: TObject);
		procedure RemoveDocument(inDocument: TLrDocument);
		procedure SetCurrent(inDocument: TLrDocument);
		procedure SetDocuments(inIndex: Integer; const Value: TLrDocument);
		procedure SetIndex(const Value: Integer);
		procedure SetOnCurrentChanged(const Value: TNotifyEvent);
		procedure SetOnDocumentsChanged(const Value: TNotifyEvent);
		procedure ValidateCurrent;
	public
		constructor Create;
		destructor Destroy; override;
		function CloseAll: Boolean;
		function StartCloseCurrent: Boolean;
		function FindDocument(const inDocument: TLrDocument): Integer; overload;
		function FindDocument(const inFilename: string): TLrDocument; overload;
		function FinishCloseCurrent: Boolean;
		procedure AddDocument(inDocument: TLrDocument);
		procedure AddDocumentObserver(inEvent: TNotifyEvent);
		procedure RemoveDocumentObserver(inEvent: TNotifyEvent);
		property Current: TLrDocument read GetCurrent write SetCurrent;
		property Documents[inIndex: Integer]: TLrDocument read GetDocuments
			write SetDocuments;
		property Index: Integer read FIndex write SetIndex;
		property LastIndex: Integer read FLastIndex write FLastIndex;
		property OnCurrentChanged: TNotifyEvent read FOnCurrentChanged
			write SetOnCurrentChanged;
		property OnDocumentsChanged: TNotifyEvent read FOnDocumentsChanged
			write SetOnDocumentsChanged;
	end;

implementation

{ TDesktop }

constructor TDesktop.Create;
begin
	FIndex := -1;
	FNilDocument := TLrDocument.Create;
	FDocumentObservers := TLrObserverList.Create;
end;

destructor TDesktop.Destroy;
begin
	FDocumentObservers.Free;
	FNilDocument.Free;
	inherited;
end;

procedure TDesktop.AddDocumentObserver(inEvent: TNotifyEvent);
begin
	FDocumentObservers.Add(inEvent);
end;

procedure TDesktop.RemoveDocumentObserver(inEvent: TNotifyEvent);
begin
	FDocumentObservers.Remove(inEvent);
end;

procedure TDesktop.SetOnDocumentsChanged(const Value: TNotifyEvent);
begin
	FOnDocumentsChanged := Value;
end;

procedure TDesktop.SetOnCurrentChanged(const Value: TNotifyEvent);
begin
	FOnCurrentChanged := Value;
end;

procedure TDesktop.CurrentChanged;
begin
//	if Assigned(FOnCurrentChanged) then
//		FOnCurrentChanged(Self);
end;

procedure TDesktop.DocumentsChanged;
begin
	if Assigned(OnDocumentsChanged) then
		OnDocumentsChanged(Self);
end;

function TDesktop.GoodIndex: Boolean;
begin
	Result := (Index >= 0) and (Index < Count);
end;

procedure TDesktop.AddDocument(inDocument: TLrDocument);
begin
	Add(inDocument);
	//inDocument.OnChange := DocumentChange;
	DocumentsChanged;
	Index := Pred(Count);
	//Project.DocumentOpened(inDocument);
end;

procedure TDesktop.RemoveDocument(inDocument: TLrDocument);
begin
	//Project.DocumentClosed(inDocument);
	Remove(inDocument);
end;

function TDesktop.GetDocuments(inIndex: Integer): TLrDocument;
begin
	Result := TLrDocument(Items[inIndex]);
end;

procedure TDesktop.SetDocuments(inIndex: Integer;
	const Value: TLrDocument);
begin
	Items[inIndex] := Value;
end;

function TDesktop.FindDocument(const inFilename: string): TLrDocument;
var
	i: Integer;
begin
	Result := nil;
	for i := 0 to Pred(Count) do
		if Documents[i].Filename = inFilename then
		begin
			Result := Documents[i];
			break;
		end;
end;

function TDesktop.FindDocument(const inDocument: TLrDocument): Integer;
var
	i: Integer;
begin
	Result := -1;
	for i := 0 to Pred(Count) do
		if Documents[i] = inDocument then
		begin
			Result := i;
			break;
		end;
end;

procedure TDesktop.SetIndex(const Value: Integer);
begin
	Current.Deactivate;
	FIndex := Value;
	DocumentsChanged;
	//CurrentChanged;
	Current.Activate;
end;

function TDesktop.GetCurrent: TLrDocument;
begin
	if GoodIndex then
		Result := Documents[Index]
	else
		Result := FNilDocument;
end;

procedure TDesktop.SetCurrent(inDocument: TLrDocument);
begin
	Index := FindDocument(inDocument);
end;

procedure TDesktop.ValidateCurrent;
begin
	if not GoodIndex then
		Index := Pred(Count)
	else begin
		DocumentsChanged;
		//CurrentChanged;
		Current.Activate;
	end;
end;

function TDesktop.StartCloseCurrent: Boolean;
begin
	Result := Current.Close;
end;

function TDesktop.FinishCloseCurrent: Boolean;
begin
	Current.Deactivate;
	Result := Current.Closed;
	if Result then
	begin
		Current.Free;
		RemoveDocument(Current);
	end;
	ValidateCurrent;
end;

function TDesktop.CloseAll: Boolean;
var
	i: Integer;
begin
	Result := true;
	Current.Deactivate;
	i := Pred(Count);
	while (i > 0) and Result do
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
				Documents[i].Free;
				RemoveDocument(Documents[i]);
				//DocumentsChanged;
			end;
			Dec(i);
		end;
	end;
	//DocumentsChanged;
	ValidateCurrent;
end;

procedure TDesktop.DocumentChange(inSender: TObject);
begin
	FDocumentObservers.Notify(inSender);
end;

end.
