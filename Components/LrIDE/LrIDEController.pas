unit LrIDEController;

interface

uses
	SysUtils, Classes, ActnList, ImgList, Controls, Dialogs,
	PngImageList,
	LrDocument;

type
	TLrIDEControllerModule = class(TDataModule)
		ActionList: TActionList;
		PngImageList: TPngImageList;
		LrOpenAction: TAction;
		LrNewAction: TAction;
		LrSaveAction: TAction;
		LrSaveAsAction: TAction;
		LrCloseAction: TAction;
		LrCloseAllAction: TAction;
		SaveDialog: TSaveDialog;
		OpenDialog: TOpenDialog;
    LrSaveOrSaveAsAction: TAction;
		procedure LrSaveActionUpdate(Sender: TObject);
		procedure LrSaveActionExecute(Sender: TObject);
		procedure LrSaveAsActionExecute(Sender: TObject);
		procedure LrCloseActionExecute(Sender: TObject);
		procedure DataModuleCreate(Sender: TObject);
		procedure DataModuleDestroy(Sender: TObject);
		procedure LrOpenActionExecute(Sender: TObject);
		procedure LrSaveAsActionUpdate(Sender: TObject);
		procedure LrCloseAllActionExecute(Sender: TObject);
		procedure LrCloseActionUpdate(Sender: TObject);
	private
		{ Private declarations }
		FDocumentTypes: TStringList;
		FNewActions: TList;
	protected
		function BuildFileFilter: string;
		function CreateDocument(const inExt: string): TLrDocument;
		function GetControllers(inIndex: Integer): TLrDocumentController;
		function GetDocument: TLrDocument;
		function GetNewActionsList: TList;
		procedure CreateNewActions;
		procedure DocumentChange(inSender: TObject);
		procedure RegisterController(inController: TLrDocumentController);
	public
		{ Public declarations }
		function CloseAllDocuments: Boolean;
		function NewDocument(const inExt: string): TLrDocument;
		procedure OpenDocument(const inFilename: string);
		procedure RegisterDocument(inControllerClass: TLrDocumentControllerClass);
		procedure SaveDocumentAs;
		property Controllers[inIndex: Integer]: TLrDocumentController
			read GetControllers;
		property Document: TLrDocument read GetDocument;
		property NewActionsList: TList read GetNewActionsList;
	end;

var
	LrIDEControllerModule: TLrIDEControllerModule;

implementation

uses
	LrOpenDocumentsController;

{$R *.dfm}

procedure TLrIDEControllerModule.DataModuleCreate(Sender: TObject);
begin
	FDocumentTypes := TStringList.Create;
	LrOpenDocuments := TLrOpenDocumentsController.Create;
end;

procedure TLrIDEControllerModule.DataModuleDestroy(Sender: TObject);
begin
	FDocumentTypes.Free;
end;

procedure TLrIDEControllerModule.RegisterController(
	inController: TLrDocumentController);
begin
	inController.OnChange := DocumentChange;
	FDocumentTypes.AddObject(inController.GetExt, inController);
end;

procedure TLrIDEControllerModule.RegisterDocument(
	inControllerClass: TLrDocumentControllerClass);
begin
	RegisterController(inControllerClass.Create);
end;

function TLrIDEControllerModule.GetControllers(
	inIndex: Integer): TLrDocumentController;
begin
	Result := TLrDocumentController(FDocumentTypes.Objects[inIndex]);
end;

procedure TLrIDEControllerModule.CreateNewActions;
var
	i: Integer;
	a: TAction;
begin
	FNewActions := TList.Create;
	for i := 0 to Pred(FDocumentTypes.Count) do
	begin
		a := TAction.Create(Self);
		a.Category := 'new';
		a.Caption := 'New ' + Controllers[i].GetDescription;
		a.Tag := i;
		FNewActions.Add(a);
	end;
end;

function TLrIDEControllerModule.GetNewActionsList: TList;
begin
	if FNewActions = nil then
		CreateNewActions;
	Result := FNewActions;
end;

procedure TLrIDEControllerModule.DocumentChange(inSender: TObject);
begin
	LrOpenDocuments.Change;
end;

function TLrIDEControllerModule.GetDocument: TLrDocument;
begin
	Result := LrOpenDocuments.Current;
end;

function TLrIDEControllerModule.CreateDocument(
	const inExt: string): TLrDocument;
var
	i: Integer;
begin
	i := FDocumentTypes.IndexOf(inExt);
	if (i < 0) then
		Result := nil
	else
		Result := Controllers[i].New;
end;

function TLrIDEControllerModule.NewDocument(const inExt: string): TLrDocument;
begin
	Result := CreateDocument(inExt);
	LrOpenDocuments.AddDocument(Result);
end;

procedure TLrIDEControllerModule.OpenDocument(const inFilename: string);
var
	d: TLrDocument;
begin
	d := CreateDocument(ExtractFileExt(inFilename));
	if d <> nil then
	begin
		d.Open(inFilename);
		LrOpenDocuments.AddDocument(d);
	end;
end;

procedure TLrIDEControllerModule.SaveDocumentAs;

	function GetSaveFilter: string;
	begin
		with Document.Controller do
			Result := GetDescription + ' (*' + GetExt + ')|*' + GetExt + '|';
	end;

begin
	with SaveDialog do
	begin
		DefaultExt := Document.Controller.GetExt;
		Filter := GetSaveFilter;
		Filename := Document.Filename;
		if Execute then
			Document.SaveAs(Filename);
	end;
end;

function TLrIDEControllerModule.CloseAllDocuments: Boolean;
begin
	Result := LrOpenDocuments.CloseAll;
end;

function TLrIDEControllerModule.BuildFileFilter: string;
var
	i: Integer;
	e, exts: string;
begin
	Result := '';
	exts := '';
	with FDocumentTypes do
		for i := 0 to Pred(Count) do
		begin
			e := '*' + Strings[i];
			if (exts <> '') then
				exts := exts + ';';
			exts := exts + e;
			Result := Result + Format('%s (%s)|%1:s|',
				[	Controllers[i].GetDescription, e ]);
		end;
	Result := Format('IDE Documents (%s)|%0:s|', [ exts ]) + Result;
end;

procedure TLrIDEControllerModule.LrOpenActionExecute(Sender: TObject);
begin
	with OpenDialog do
	begin
		Filter := BuildFileFilter;
		if Execute then
			OpenDocument(Filename);
	end;
end;

procedure TLrIDEControllerModule.LrSaveActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := Document.Modified;
end;

procedure TLrIDEControllerModule.LrSaveActionExecute(Sender: TObject);
begin
	if Document.Untitled then
		SaveDocumentAs
	else
		Document.Save;
end;

procedure TLrIDEControllerModule.LrSaveAsActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := true;
end;

procedure TLrIDEControllerModule.LrSaveAsActionExecute(Sender: TObject);
begin
	SaveDocumentAs;
end;

procedure TLrIDEControllerModule.LrCloseActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := LrOpenDocuments.Count > 0;
end;

procedure TLrIDEControllerModule.LrCloseActionExecute(Sender: TObject);
begin
	LrOpenDocuments.CloseCurrent;
end;

procedure TLrIDEControllerModule.LrCloseAllActionExecute(Sender: TObject);
begin
	CloseAllDocuments;
end;

end.
