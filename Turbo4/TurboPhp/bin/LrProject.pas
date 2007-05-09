unit LrProject;

interface

uses
	SysUtils, Classes, Components, Controls, Forms, Dialogs,
	LrPersistComponent;

type
	TLrProject = class(TLrPersistComponent)
	private
		FFilename: string;
		FModified: Boolean;
		FOnChanged: TNotifyEvent;
	protected
		function GetDisplayName: string;
		procedure SetFilename(const Value: string); virtual;
	protected
		procedure Changed; virtual;
	public
		procedure LoadFromFile(const inFilename: string); override;
		procedure SaveToFile(const inFilename: string); override;
		property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
	published
		property DisplayName: string read GetDisplayName;
		property Filename: string read FFilename write SetFilename;
		property Modified: Boolean read FModified write FModified;
	end;
	//
	TLrProjectManager = class(TLrPersistComponent)
	private
		FDefaultProject: string;
		FFilename: string;
		FLoading: Boolean;
		FCurrentProject: TLrProject;
		FOnCurrentProjectChanged: TNotifyEvent;
		FOnProjectChanged: TNotifyEvent;
		FSaveDialog: TSaveDialog;
	protected
		procedure SetCurrentProject(const Value: TLrProject);
		procedure SetFilename(const Value: string);
	protected
		procedure CurrentProjectChanged;
		procedure ProjectChanged(inSender: TObject);
	public
		constructor Create(AOwner: TComponent = nil); override;
		destructor Destroy; override;
		function CloseProject: Boolean;
		function ProjectIsOpen: Boolean;
		function SaveProjectAs: Boolean;
		function SaveProject: Boolean;
		procedure Load;
		procedure OpenDefaultProject;
		procedure OpenProject(const inFilename: string);
		procedure Save;
		property CurrentProject: TLrProject read FCurrentProject
			write SetCurrentProject;
		property Filename: string read FFilename write SetFilename;
		property OnCurrentProjectChanged: TNotifyEvent
			read FOnCurrentProjectChanged	write FOnCurrentProjectChanged;
		property OnProjectChanged: TNotifyEvent read FOnProjectChanged
			write FOnProjectChanged;
		property SaveDialog: TSaveDialog read FSaveDialog write FSaveDialog;
	published
		property DefaultProject: string read FDefaultProject
			write FDefaultProject;
	end;

var
	NilProject: TLrProject;

implementation

{ TLrProject }

procedure TLrProject.Changed;
begin
	Modified := true;
	if Assigned(OnChanged) then
		OnChanged(Self);
end;

procedure TLrProject.LoadFromFile(const inFilename: string);
begin
	inherited;
	Filename := inFilename;
	Modified := false;
end;

procedure TLrProject.SaveToFile(const inFilename: string);
begin
	Filename := inFilename;
	inherited;
	Modified := false;
end;

function TLrProject.GetDisplayName: string;
begin
	Result := ExtractFileName(Filename);
	if Result = '' then
		Result := 'Untitled';
end;

procedure TLrProject.SetFilename(const Value: string);
begin
	FFilename := Value;
	Changed;
end;

{ TLrProjectManager }

constructor TLrProjectManager.Create(AOwner: TComponent);
begin
	inherited;
	CurrentProject := NilProject;
end;

destructor TLrProjectManager.Destroy;
begin
	inherited;
end;

procedure TLrProjectManager.Save;
begin
	if not FLoading then
		SaveToFile(FFilename);
end;

procedure TLrProjectManager.Load;
begin
	FLoading := true;
	try
		LoadFromFile(Filename);
		inherited;
	finally
		FLoading := false;
	end;
end;

function TLrProjectManager.CloseProject: Boolean;
var
	mr: TModalResult;
begin
	Result := true;
	if CurrentProject <> NilProject then
	begin
		if CurrentProject.Modified then
		begin
			mr := MessageDlg('Save changes to project "' + CurrentProject.DisplayName
				+ '" before closing?', mtConfirmation, mbYesNoCancel, 0);
			if mr = mrCancel then
				Result := false
			else if mr = mrYes then
				Result := SaveProject;
		end;
		if Result then
		begin
			CurrentProject.Free;
			CurrentProject := NilProject;
		end;
	end;
end;

procedure TLrProjectManager.CurrentProjectChanged;
begin
	if CurrentProject.Filename <> '' then
		DefaultProject := CurrentProject.Filename;
	CurrentProject.OnChanged := ProjectChanged;
	if Assigned(OnCurrentProjectChanged) then
		OnCurrentProjectChanged(Self);
end;

procedure TLrProjectManager.SetFilename(const Value: string);
begin
	FFilename := Value;
	Load;
end;

procedure TLrProjectManager.SetCurrentProject(const Value: TLrProject);
begin
	FCurrentProject := Value;
	CurrentProjectChanged;
end;

procedure TLrProjectManager.ProjectChanged(inSender: TObject);
begin
	if (inSender = CurrentProject) and Assigned(OnProjectChanged) then
		OnProjectChanged(Self);
end;

procedure TLrProjectManager.OpenDefaultProject;
begin
	OpenProject(DefaultProject);
end;

procedure TLrProjectManager.OpenProject(const inFilename: string);
begin
	if FileExists(inFilename) then
	begin
		CurrentProject.LoadFromFile(inFilename);
		CurrentProjectChanged;
	end;
end;

function TLrProjectManager.SaveProjectAs: Boolean;
begin
	SaveDialog.Filename := CurrentProject.Filename;
	Result := SaveDialog.Execute;
	if Result then
	begin
		CurrentProject.SaveToFile(SaveDialog.Filename);
		CurrentProjectChanged;
	end;
end;

function TLrProjectManager.SaveProject: Boolean;
begin
	Result := CurrentProject.Filename <> '';
	if not Result then
		Result := SaveProjectAs
	else
		CurrentProject.SaveToFile(CurrentProject.Filename);
end;

function TLrProjectManager.ProjectIsOpen: Boolean;
begin
	Result := CurrentProject <> NilProject;
end;

initialization
	NilProject := TLrProject.Create(nil);
end.

