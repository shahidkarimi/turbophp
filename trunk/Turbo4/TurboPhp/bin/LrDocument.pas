unit LrDocument;

interface

uses
	SysUtils, Classes, Controls, ActnList, Contnrs, Dialogs;

type
	TLrDocumentManager = class;
	TLrDocumentAction = TAction;
	//
	TLrController = class(TComponent)
	public
		procedure Activate; virtual;
		procedure Deactivate; virtual;
		procedure LazyUpdate; virtual;
	end;
	//
	TLrDocument = class
	private
		FManager: TLrDocumentManager;
		FModified: Boolean;
		FPath: string;
		FReadOnly: Boolean;
		FUntitled: Boolean;
		FController: TLrController;
	protected
		function GetActive: Boolean;
		function GetDisplayName: string; virtual;
		function GetExtension: string; virtual;
		function GetFileName: string;
		function GetFilter: string; virtual;
		function GetPath: string; virtual;
		procedure SetActive(const Value: Boolean);
		procedure SetModified(const Value: Boolean); virtual;
		procedure SetPath(const Value: string); virtual;
		procedure SetReadOnly(const Value: Boolean); virtual;
		procedure SetUntitled(const Value: Boolean); virtual;
	protected
		procedure Change; virtual;
		procedure CloseItem;
	public
		constructor Create(inManager: TLrDocumentManager;
			const inPath: string = ''); virtual;
		destructor Destroy; override;
		function CanClose: Boolean; virtual;
		function EnableSave: Boolean; virtual;
		function EnableSaveAs: Boolean; virtual;
		function SaveAs: Boolean; virtual;
		procedure Activate; virtual;
		procedure ChangeEvent(inSender: TObject); virtual;
		procedure Close; virtual;
		procedure Deactivate; virtual;
		procedure DisEnableAction(inAction: TLrDocumentAction); virtual;
		procedure LazyUpdate; virtual;
		procedure Open; virtual;
		procedure PerformAction(inAction: TLrDocumentAction); virtual;
		procedure Save; virtual;
	public
		property Active: Boolean read GetActive write SetActive;
		property Controller: TLrController read FController write FController;
		property DisplayName: string read GetDisplayName;
		property Extension: string read GetExtension;
		property FileName: string read GetFileName;
		property Filter: string read GetFilter;
		property Manager: TLrDocumentManager read FManager write FManager;
		property Modified: Boolean read FModified write SetModified;
		property Path: string read GetPath write SetPath;
		property ReadOnly: Boolean read FReadOnly write SetReadOnly;
		property Untitled: Boolean read FUntitled write SetUntitled;
	end;
	//
	TLrDocumentClass = class of TLrDocument;
	TLrOpenDocumentEvent = function(inManager: TLrDocumentManager;
		const inPath: string): TLrDocument of object;
	TLrSaveAsDocumentEvent = procedure(inSender: TObject;
		inDocument: TLrDocument; const inOldPath, inNewPath: string) of object;
	TLrDocumentEvent = procedure(inSender: TObject;
		inDocument: TLrDocument) of object;
	//
	TLrDocumentProfile = class
		DocumentClass: TLrDocumentClass;
		Extensions: array of string;
	end;
	//
	TLrDocumentManager = class
	private
		{ Private declarations }
		FCurrent: TLrDocument;
		FDefaultPath: string;
		FDocumentTypes: TStringList;
		FItems: TObjectList;
		FOnAfterOpen: TLrDocumentEvent;
		FOnClose: TNotifyEvent;
		FOnCreateController: TLrDocumentEvent;
		FOnCurrentChanged: TNotifyEvent;
		FOnCurrentChanging: TNotifyEvent;
		FOnOpen: TLrOpenDocumentEvent;
		FOnSaveAs: TLrSaveAsDocumentEvent;
		FOnUpdateDocument: TLrDocumentEvent;
		FSaveDialog: TSaveDialog;
		FUpdate: Integer;
	protected
		function GetCount: Integer;
		function GetItems(inIndex: Integer): TLrDocument;
		procedure SetCurrent(const Value: TLrDocument);
		procedure SetDefaultPath(const Value: string);
		procedure SetItems(inIndex: Integer; const Value: TLrDocument);
	protected
		function GetFileFilter: string;
		function NewName: string;
		function OpenItem(const inPath: string): TLrDocument;
		function UntitledCount: Integer;
		procedure CloseItem(inItem: TLrDocument);
		procedure CloseUntitled;
		procedure CurrentChanged;
		procedure CurrentChanging;
		procedure CurrentClosing;
		procedure Select(inItem: TLrDocument);
		procedure ValidateCurrent(inPivot: Integer);
	public
		{ Public declarations }
		constructor Create;
		destructor Destroy; override;
		function CloseAll: Boolean;
		function DocumentClassFromExtension(const inExt: string): TLrDocumentClass;
		function EnableClose: Boolean;
		function EnableSave: Boolean; virtual;
		function EnableSaveAs: Boolean; virtual;
		function Find(const inPath: string): Integer;
		function Open(const inPath: string): Boolean;
		procedure BeginUpdate;
		procedure Close(inItem: TLrDocument);
		procedure DisEnableAction(inAction: TLrDocumentAction); virtual;
		procedure DocumentChanged(inItem: TLrDocument);
		procedure EndUpdate;
		procedure New(inClass: TLrDocumentClass);
		procedure PerformAction(inAction: TLrDocumentAction); virtual;
		procedure RegisterExtension(const inExt: string;
			inDocumentClass: TLrDocumentClass); overload;
		procedure RegisterExtensions(const inExts: array of string;
			inDocumentClass: TLrDocumentClass); overload;
		procedure Save(inItem: TLrDocument = nil);
		procedure SaveAs(inItem: TLrDocument = nil);
		procedure UpdateDocument(inDocument: TLrDocument);
	public
		NilDocument: TLrDocument;
		property Count: Integer read GetCount;
		property Current: TLrDocument read FCurrent write SetCurrent;
		property DefaultPath: string read FDefaultPath write SetDefaultPath;
		property DocumentTypes: TStringList read FDocumentTypes;
		property Items[inIndex: Integer]: TLrDocument read GetItems
			write SetItems; default;
		property OnClose: TNotifyEvent read FOnClose write FOnClose;
		property OnCreateController: TLrDocumentEvent read FOnCreateController
			write FOnCreateController;
		property OnCurrentChanged: TNotifyEvent read FOnCurrentChanged
			write FOnCurrentChanged;
		property OnCurrentChanging: TNotifyEvent read FOnCurrentChanging
			write FOnCurrentChanging;
		property OnOpen: TLrOpenDocumentEvent read FOnOpen write FOnOpen;
		property OnAfterOpen: TLrDocumentEvent read FOnAfterOpen write FOnAfterOpen;
		property OnSaveAs: TLrSaveAsDocumentEvent read FOnSaveAs write FOnSaveAs;
		property OnUpdateDocument: TLrDocumentEvent read FOnUpdateDocument
			write FOnUpdateDocument;
		property SaveDialog: TSaveDialog read FSaveDialog;
	end;

implementation

{ TLrController }

procedure TLrController.Activate;
begin
	//
end;

procedure TLrController.Deactivate;
begin
	//
end;

procedure TLrController.LazyUpdate;
begin
	//
end;

{ TLrDocument }

constructor TLrDocument.Create(inManager: TLrDocumentManager;
	const inPath: string);
begin
	FManager := inManager;
	FPath := inPath;
end;

destructor TLrDocument.Destroy;
begin
	Controller.Free;
	inherited;
end;

procedure TLrDocument.Change;
begin
	Modified := true;
end;

procedure TLrDocument.ChangeEvent(inSender: TObject);
begin
	Change;
end;

procedure TLrDocument.Activate;
begin
	if Controller <> nil then
		Controller.Activate;
end;

procedure TLrDocument.Deactivate;
begin
	if Controller <> nil then
		Controller.Deactivate;
end;

function TLrDocument.GetActive: Boolean;
begin
	Result := Manager.Current = Self;
end;

procedure TLrDocument.SetActive(const Value: Boolean);
begin
	if Value and not Active then
		Manager.Select(Self);
end;

procedure TLrDocument.LazyUpdate;
begin
	if Active and (Controller <> nil) then
		Controller.LazyUpdate;
end;

procedure TLrDocument.Open;
begin
	//
end;

function TLrDocument.CanClose: Boolean;
var
	mr: TModalResult;
begin
	Result := true;
	if Modified then
	begin
		mr := MessageDlg('Save changes to "' + DisplayName + '" before closing?',
			mtConfirmation, mbYesNoCancel, 0);
		if mr = mrCancel then
			Result := false
		else if mr = mrYes then
		begin
			if Untitled then
				Result := SaveAs
			else
				Save;
		end;
	end;
end;

procedure TLrDocument.CloseItem;
begin
	Deactivate;
	FManager.CloseItem(Self);
end;

procedure TLrDocument.Close;
begin
	Deactivate;
	FManager.Close(Self);
end;

function TLrDocument.GetFileName: string;
begin
	Result := ExtractFileName(Path);
end;

function TLrDocument.GetDisplayName: string;
begin
	Result := ChangeFileExt(FileName, '');
end;

procedure TLrDocument.SetPath(const Value: string);
begin
	FPath := Value;
	Change;
end;

procedure TLrDocument.DisEnableAction(inAction: TLrDocumentAction);
begin
	inAction.Enabled := false;
end;

procedure TLrDocument.PerformAction(inAction: TLrDocumentAction);
begin
	//
end;

procedure TLrDocument.SetUntitled(const Value: Boolean);
begin
	FUntitled := Value;
end;

procedure TLrDocument.SetModified(const Value: Boolean);
begin
	FModified := Value;
end;

function TLrDocument.EnableSaveAs: Boolean;
begin
	Result := not ReadOnly;
end;

function TLrDocument.EnableSave: Boolean;
begin
	Result := EnableSaveAs and Modified;
end;

procedure TLrDocument.Save;
begin
	LazyUpdate;
	Modified := false;
end;

function TLrDocument.SaveAs: Boolean;
begin
	//LazyUpdate;
	with Manager.SaveDialog do
	begin
		FileName := Path;
		DefaultExt := Extension;
		Filter := Self.Filter;
		Result := Execute;
		if Result then
		begin
			Untitled := false;
			Path := FileName;
			Save;
		end;
	end;
end;

procedure TLrDocument.SetReadOnly(const Value: Boolean);
begin
	FReadOnly := Value;
end;

function TLrDocument.GetPath: string;
begin
	Result := FPath;
end;

function TLrDocument.GetExtension: string;
begin
	Result := ExtractFileExt(Path);
end;

function TLrDocument.GetFilter: string;
begin
	if GetExtension = '' then
		Result := 'Any File (*.*)|*.*'
	else
	Result := Format('Document File (*.%s)|*.%0:s', [ GetExtension ]);
end;

{ TLrDocumentManager }

constructor TLrDocumentManager.Create;
begin
	FSaveDialog := TSaveDialog.Create(nil);
	FItems := TObjectList.Create;
	NilDocument := TLrDocument.Create(Self, '');
	NilDocument.ReadOnly := true;
	FCurrent := NilDocument;
	FDocumentTypes := TStringList.Create;
	//DefaultPath := HomeFolder;
end;

destructor TLrDocumentManager.Destroy;
begin
	FDocumentTypes.Free;
	FItems.Free;
	FSaveDialog.Free;
	inherited;
end;

procedure TLrDocumentManager.RegisterExtension(const inExt: string;
	inDocumentClass: TLrDocumentClass);
begin
	DocumentTypes.AddObject(inExt, TObject(inDocumentClass));
end;

procedure TLrDocumentManager.RegisterExtensions(const inExts: array of string;
	inDocumentClass: TLrDocumentClass);
var
	i: Integer;
begin
	for i := 0 to Pred(Length(inExts)) do
		RegisterExtension(inExts[i], inDocumentClass);
end;

function TLrDocumentManager.GetFileFilter: string;
var
	i: Integer;
	s0, s1: string;
begin
	for i := 0 to Pred(DocumentTypes.Count) do
	begin
		if i <> 0 then
		begin
			s0 := s0 + ', ';
			s1 := s1 + '; ';
		end;
		s0 := s0 + '*' + DocumentTypes[i];
		s1 := s1 + '*' + DocumentTypes[i];
	end;
	Result := '(' + s0 + ')|' + s1;
end;

function TLrDocumentManager.DocumentClassFromExtension(
	const inExt: string): TLrDocumentClass;
var
	i: Integer;
begin
	for i := 0 to Pred(DocumentTypes.Count) do
		if DocumentTypes[i] = inExt then
		begin
			Result := TLrDocumentClass(DocumentTypes.Objects[i]);
			exit;
		end;
	Result := nil;
end;

procedure TLrDocumentManager.SetDefaultPath(const Value: string);
begin
	if Value = '' then
		FDefaultPath := ''
	else
		FDefaultPath := IncludeTrailingPathDelimiter(Value);
	SaveDialog.InitialDir := FDefaultPath;
end;

function TLrDocumentManager.GetCount: Integer;
begin
	Result := FItems.Count;
end;

function TLrDocumentManager.GetItems(inIndex: Integer): TLrDocument;
begin
	Result := TLrDocument(FItems[inIndex]);
end;

procedure TLrDocumentManager.SetItems(inIndex: Integer;
	const Value: TLrDocument);
begin
	FItems[inIndex] := Value;
end;

function TLrDocumentManager.Find(const inPath: string): Integer;
var
	i: Integer;
begin
	Result := -1;
	for i := 0 to Count - 1 do
		if Items[i].Path = inPath then
		begin
			Result := i;
			break;
		end;
end;

procedure TLrDocumentManager.CurrentChanging;
begin
	if Assigned(OnCurrentChanging) then
		OnCurrentChanging(Self);
end;

procedure TLrDocumentManager.CurrentChanged;
begin
	if Assigned(OnCurrentChanged) then
		OnCurrentChanged(Self);
end;

procedure TLrDocumentManager.CurrentClosing;
begin
	if Assigned(OnClose) then
		OnClose(Self);
end;

procedure TLrDocumentManager.CloseUntitled;
begin
	if FCurrent.Untitled and not FCurrent.Modified then
	begin
		CurrentClosing;
		FItems.Extract(FCurrent);
		FCurrent.Free;
	end;
end;

procedure TLrDocumentManager.Select(inItem: TLrDocument);
begin
//	if (inItem <> FCurrent) then
//	begin
		CurrentChanging;
		FCurrent.Deactivate;
		if inItem = nil then
			inItem := NilDocument;
		if inItem <> NilDocument then
			CloseUntitled;
		FCurrent := inItem;
		CurrentChanged;
		FCurrent.Activate;
//	end;
end;

function TLrDocumentManager.UntitledCount: Integer;
var
	i: Integer;
begin
	Result := 0;
	for i := 0 to Count - 1 do
		if Items[i].Untitled then
			Inc(Result);
end;

function TLrDocumentManager.NewName: string;
begin
	Result := DefaultPath + 'Untitled' + IntToStr(UntitledCount + 1)
end;

procedure TLrDocumentManager.New(inClass: TLrDocumentClass);
var
	item: TLrDocument;
begin
	item := inClass.Create(Self, NewName);
	item.Untitled := true;
	item.Open;
	//item.Modified := true;
	FItems.Add(item);
	Select(item);
end;

function TLrDocumentManager.Open(const inPath: string): Boolean;
var
	i: Integer;
	item: TLrDocument;
begin
	Result := true;
	i := Find(inPath);
	if (i >= 0) then
		Select(Items[i])
	else begin
		item := OpenItem(inPath);
		if item = nil then
			Result := false
		else begin
			FItems.Add(item);
			Select(item);
		end;
	end;
end;

function TLrDocumentManager.OpenItem(const inPath: string): TLrDocument;
var
	dc: TLrDocumentClass;
begin
	if not Assigned(FOnOpen) then
		Result := nil
	else
		Result := FOnOpen(Self, inPath);
	if Result = nil then
	begin
		dc := DocumentClassFromExtension(ExtractFileExt(inPath));
		if (dc <> nil) then
			Result := dc.Create(Self, inPath);
	end;
	if Result <> nil then
	begin
		Result.Open;
		if Assigned(FOnAfterOpen) then
			FOnAfterOpen(Self, Result);
	end;
end;

procedure TLrDocumentManager.ValidateCurrent(inPivot: Integer);
begin
	FCurrent := NilDocument;
	if (inPivot >= 0) and (inPivot < Count) then
		Current := Items[inPivot]
	else if (inPivot > 0) and (inPivot - 1 < Count) then
		Current := Items[inPivot-1];
end;

procedure TLrDocumentManager.CloseItem(inItem: TLrDocument);
begin
	FItems.Extract(inItem);
	inItem.Free;
end;

procedure TLrDocumentManager.Close(inItem: TLrDocument);
var
	i: Integer;
begin
	i := FItems.IndexOf(inItem);
	CloseItem(inItem);
	if FCurrent = inItem then
		ValidateCurrent(i);
	CurrentChanged;
end;

procedure TLrDocumentManager.DocumentChanged(inItem: TLrDocument);
begin
	//
end;

procedure TLrDocumentManager.DisEnableAction(inAction: TLrDocumentAction);
begin
	Current.DisEnableAction(inAction);
end;

procedure TLrDocumentManager.PerformAction(inAction: TLrDocumentAction);
begin
	Current.PerformAction(inAction);
end;

procedure TLrDocumentManager.Save(inItem: TLrDocument = nil);
begin
	if inItem = nil then
		inItem := Current;
	if inItem.Untitled then
		SaveAs(inItem)
	else
		inItem.Save;
end;

procedure TLrDocumentManager.SaveAs(inItem: TLrDocument = nil);
var
	oldName: string;
begin
	if inItem = nil then
		inItem := Current;
	if inItem <> NilDocument then
	begin
		oldName := inItem.Path;
		if inItem.SaveAs then
		begin
			if Assigned(OnSaveAs) then
				OnSaveAs(Self, inItem, oldName, inItem.Path);
			CurrentChanged;
		end;
	end;
end;

procedure TLrDocumentManager.SetCurrent(const Value: TLrDocument);
begin
	if (Value <> Current) then
		Select(Value);
end;

function TLrDocumentManager.EnableClose: Boolean;
begin
	Result := Current <> NilDocument;
end;

function TLrDocumentManager.EnableSave: Boolean;
begin
	Result := Current.EnableSave;
end;

function TLrDocumentManager.EnableSaveAs: Boolean;
begin
	Result := Current.EnableSaveAs;
end;

function TLrDocumentManager.CloseAll: Boolean;
begin
	Result := true;
	FCurrent := NilDocument;
	while Count > 0 do
		if not Items[0].CanClose then
		begin
			Result := false;
			break;
		end else
			Items[0].CloseItem;
	Select(NilDocument);
end;

procedure TLrDocumentManager.UpdateDocument(inDocument: TLrDocument);
begin
	if Assigned(OnUpdateDocument) then
		OnUpdateDocument(Self, inDocument);
end;

procedure TLrDocumentManager.BeginUpdate;
begin
	Inc(FUpdate);
end;

procedure TLrDocumentManager.EndUpdate;
begin
	Dec(FUpdate);
	if FUpdate = 0 then
		ValidateCurrent(0);
end;

end.
