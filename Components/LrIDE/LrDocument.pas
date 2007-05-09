unit LrDocument;

interface

uses
	SysUtils, Classes, Controls, Dialogs;

type
	TLrDocument = class;
	TLrDocumentClass = class of TLrDocument;
	TLrDocumentController = class;
	TLrDocumentControllerClass = class of TLrDocumentController;
	//
	TLrDocument = class
	private
		FActive: Boolean;
		FClosed: Boolean;
		FController: TLrDocumentController;
		FFilename: string;
		FModified: Boolean;
		FUntitled: Boolean;
		FOldFilename: string;
	protected
		function GetDisplayName: string; virtual;
		function GetThisDocument: TLrDocument;
		function GetUntitledName: string; virtual;
		procedure Change;
		procedure SetActive(const Value: Boolean); virtual;
		procedure SetFilename(const Value: string); virtual;
		procedure SetModified(const Value: Boolean); virtual;
		procedure SetUntitled(const Value: Boolean); virtual;
	public
		constructor Create; virtual;
		function CanClose: Boolean; virtual;
		function Close: Boolean; virtual;
		procedure Activate; virtual;
		procedure Deactivate; virtual;
		procedure DoModified(inSender: TObject); virtual;
		procedure Load; virtual;
		procedure Modify; virtual;
		procedure New; virtual;
		procedure Open(const inFilename: string); virtual;
		procedure Save; virtual;
		procedure SaveAs(const inFilename: string); virtual;
		procedure Update; virtual;
		property Active: Boolean read FActive write SetActive;
		property Closed: Boolean read FClosed write FClosed;
		property Controller: TLrDocumentController read FController
			write FController;
		property DisplayName: string read GetDisplayName;
		property Filename: string read FFilename write SetFilename;
		property OldFilename: string read FOldFilename write FOldFilename;
		property Modified: Boolean read FModified write SetModified;
		property ThisDocument: TLrDocument read GetThisDocument;
		property Untitled: Boolean read FUntitled write SetUntitled;
		property UntitledName: string read GetUntitledName;
	end;
	//
	TLrDocumentController = class
  private
    FOnChange: TNotifyEvent;
	public
		class function GetDescription: string; virtual;
		class function GetExt: string; virtual;
	protected
		procedure Change;
	public
		function CreateDocument(inClass: TLrDocumentClass): TLrDocument;
		function New: TLrDocument; virtual;
		procedure	DocumentActivate(inDocument: TLrDocument); virtual;
		procedure	DocumentChange(inDocument: TLrDocument); virtual;
		procedure	DocumentClose(inDocument: TLrDocument); virtual;
		procedure	DocumentDeactivate(inDocument: TLrDocument); virtual;
		procedure	DocumentUpdate(inDocument: TLrDocument); virtual;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
	end;

implementation

{ TLrDocument }

constructor TLrDocument.Create;
begin
	Untitled := true;
end;

procedure TLrDocument.Activate;
begin
	if not Active then
	begin
		FActive := true;
		Controller.DocumentActivate(Self);
	end;
end;

procedure TLrDocument.Deactivate;
begin
	if Active then
	begin
		Update;
		Controller.DocumentDeactivate(Self);
		FActive := false;
	end;
end;

procedure TLrDocument.Change;
begin
	Controller.DocumentChange(Self);
end;

procedure TLrDocument.SetModified(const Value: Boolean);
begin
	if FModified <> Value then
		FModified := Value;
	Change;
end;

procedure TLrDocument.Modify;
begin
	Modified := true;
end;

procedure TLrDocument.DoModified(inSender: TObject);
begin
	Modify;
end;

	function Confirm(const inMsg: string): TModalResult;
	begin
		Result := MessageDlg(inMsg, mtConfirmation, mbYesNoCancel, 0);
	end;

function TLrDocument.CanClose: Boolean;
begin
	Result := true;
	if Modified then
		case Confirm('Save changes to "' + DisplayName + '" before closing?') of
			mrNo:
				Modified := false;
			mrYes:
				if not Untitled then
					Save;
			mrCancel:
				Result := false;
		end;
end;

function TLrDocument.Close: Boolean;
begin
	Update;
	Closed := CanClose;
	Controller.DocumentClose(Self);
	Result := Closed;
end;

function TLrDocument.GetUntitledName: string;
begin
	Result := 'Untitled' + Controller.GetExt;
end;

function TLrDocument.GetDisplayName: string;
begin
	if Filename <> '' then
		Result := ExtractFileName(Filename)
	else
		Result := UntitledName;
end;

procedure TLrDocument.New;
begin
	//
end;

procedure TLrDocument.Load;
begin
	//
end;

procedure TLrDocument.Open(const inFilename: string);
begin
	Untitled := false;
	Modified := false;
	Filename := inFilename;
	OldFilename := inFilename;
	Load;
end;

procedure TLrDocument.Update;
begin
	Controller.DocumentUpdate(Self);
end;

procedure TLrDocument.Save;
begin
	Update;
	Modified := false;
end;

procedure TLrDocument.SaveAs(const inFilename: string);
begin
	OldFilename := Filename;
	Filename := inFilename;
	Untitled := false;
	Save;
end;

procedure TLrDocument.SetActive(const Value: Boolean);
begin
	if Active <> Value then
		if Value then
			Activate
		else
			Deactivate;
end;

procedure TLrDocument.SetUntitled(const Value: Boolean);
begin
	FUntitled := Value;
end;

procedure TLrDocument.SetFilename(const Value: string);
begin
	FFilename := Value;
	//Untitled := Filename = '';
end;

function TLrDocument.GetThisDocument: TLrDocument;
begin
	Result := Self;
end;

{ TLrDocumentController }

class function TLrDocumentController.GetDescription: string;
begin
	Result := '';
end;

class function TLrDocumentController.GetExt: string;
begin
	Result := '';
end;

procedure TLrDocumentController.Change;
begin
	if Assigned(FOnChange) then
		FOnChange(Self);
end;

function TLrDocumentController.CreateDocument(
	inClass: TLrDocumentClass): TLrDocument;
begin
	Result := inClass.Create;
	Result.Controller := Self;
end;

function TLrDocumentController.New: TLrDocument;
begin
	Result := nil;
end;

procedure TLrDocumentController.DocumentActivate(inDocument: TLrDocument);
begin
	//
end;

procedure TLrDocumentController.DocumentChange(inDocument: TLrDocument);
begin
	Change;
end;

procedure TLrDocumentController.DocumentClose(inDocument: TLrDocument);
begin
	//
end;

procedure TLrDocumentController.DocumentDeactivate(
	inDocument: TLrDocument);
begin
	//
end;

procedure TLrDocumentController.DocumentUpdate(inDocument: TLrDocument);
begin
	//
end;

end.
