unit LrActionController;

interface

uses
	SysUtils, Classes, ActnList, ImgList, Controls, Dialogs,
	PngImageList,
	LrDocument;

type
	TLrActionControllerModule = class(TDataModule)
		ActionList: TActionList;
		PngImageList: TPngImageList;
		LrOpenAction: TAction;
		LrNewAction: TAction;
		LrSaveAction: TAction;
		LrSaveAsAction: TAction;
		LrCloseAction: TAction;
		LrCloseAll: TAction;
		SaveDialog: TSaveDialog;
		OpenDialog: TOpenDialog;
		procedure LrSaveActionUpdate(Sender: TObject);
		procedure LrSaveActionExecute(Sender: TObject);
		procedure LrSaveAsActionExecute(Sender: TObject);
		procedure LrCloseActionExecute(Sender: TObject);
		procedure DataModuleCreate(Sender: TObject);
		procedure DataModuleDestroy(Sender: TObject);
		procedure LrOpenActionExecute(Sender: TObject);
	private
		{ Private declarations }
		FDocumentTypes: TStringList;
		function BuildFileFilter: string;
		function GetDocument: TLrDocument;
		procedure SaveDocumentAs;
	public
		{ Public declarations }
		property Document: TLrDocument read GetDocument;
		procedure RegisterDocument(inDocumentType: TLrDocumentClass);
	end;

var
	LrActionControllerModule: TLrActionControllerModule;

implementation

uses
	LrOpenDocumentsController;

{$R *.dfm}

procedure TLrActionControllerModule.DataModuleCreate(Sender: TObject);
begin
	FDocumentTypes := TStringList.Create;
end;

procedure TLrActionControllerModule.DataModuleDestroy(Sender: TObject);
begin
	FDocumentTypes.Free;
end;

procedure TLrActionControllerModule.RegisterDocument(
	inDocumentType: TLrDocumentClass);
begin
	FDocumentTypes.AddObject(inDocumentType.GetExt, TObject(inDocumentType));
end;

function TLrActionControllerModule.GetDocument: TLrDocument;
begin
	Result := LrOpenDocuments.Current;
end;

procedure TLrActionControllerModule.SaveDocumentAs;
begin
	Document.SaveAs(SaveDialog);
end;

procedure TLrActionControllerModule.LrSaveActionUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled := Document.Modified;
end;

procedure TLrActionControllerModule.LrSaveActionExecute(Sender: TObject);
begin
	if Document.Untitled then
		SaveDocumentAs
	else
		Document.Save;
end;

procedure TLrActionControllerModule.LrSaveAsActionExecute(Sender: TObject);
begin
	SaveDocumentAs;
end;

procedure TLrActionControllerModule.LrCloseActionExecute(Sender: TObject);
begin
	LrOpenDocuments.CloseCurrent;
end;

function TLrActionControllerModule.BuildFileFilter: string;
var
	i: Integer;
	e, exts: string;
begin
	Result := '';
	exts := '';
	with FDocumentTypes do
		for i := 0 to Pred(Count) do
		begin
			e := '*.' + Strings[i];
			exts := exts + e + ';';
			Result := Result + Format('%s (%s)|%1:s|',
				[	TLrDocumentClass(Objects[i]).GetDescription, e ]);
		end;
	Result := Format('IDE Documents (%s)|%0:s|', [ exts ]) + Result;
end;

procedure TLrActionControllerModule.LrOpenActionExecute(Sender: TObject);
begin
	OpenDialog.Filter := BuildFileFilter;
	with OpenDialog do
		if Execute then
		begin
		end;
end;

end.
