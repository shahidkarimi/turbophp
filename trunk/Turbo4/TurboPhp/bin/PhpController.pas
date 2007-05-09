unit PhpController;

interface

uses
	Controls,
	EasyEditState,
	LrDockUtils, LrDocument,
	RawDocument, PhpEditView, PhpView;

type
	TPhpDocument = TRawDocument;
	//
	TPhpController = class(TLrController)
	private
		FDocument: TPhpDocument;
	protected
		function GetView: TPhpViewForm;
		procedure SetDocument(const Value: TPhpDocument);
	protected
		TabState: TDockTabsState;
		EditState: TEasyEditState;
		LastFocus: TWinControl;
	public
		{ Public declarations }
		constructor Create(inDocument: TPhpDocument); reintroduce;
		destructor Destroy; override;
		procedure Activate; override;
		procedure Deactivate; override;
		procedure LazyUpdate; override;
	public
		property Document: TPhpDocument read FDocument write SetDocument;
		property View: TPhpViewForm read GetView;
	end;

implementation

uses
	LrUtils,
	Main;

constructor TPhpController.Create(inDocument: TPhpDocument);
begin
	inherited Create(nil);
	Document := inDocument;
	TabState := TDockTabsState.Create(View);
	EditState := TEasyEditState.Create;
end;

destructor TPhpController.Destroy;
begin
	EditState.Free;
	TabState.Free;
	inherited;
end;

function TPhpController.GetView: TPhpViewForm;
begin
	Result := MainForm.PhpView;
end;

procedure TPhpController.SetDocument(const Value: TPhpDocument);
begin
	FDocument := Value;
end;

procedure TPhpController.Activate;
begin
	View.PhpEditForm.Strings := Document.Strings;
	View.PhpEditForm.OnModified := Document.ChangeEvent;
	//
//	View.ShowDocks;
	//
	ActivateDock(View.CodeDock);
	//
	TabState.Restore;
	//
	if (LastFocus <> nil) and LastFocus.CanFocus then
		LastFocus.SetFocus;
	//
	EditState.SetState(View.PhpEditForm.Edit);
end;

procedure TPhpController.Deactivate;
begin
	LazyUpdate;
	TabState.Capture;
	LastFocus := MainForm.LastEditor;
	EditState.GetState(View.PhpEditForm.Edit);
end;

procedure TPhpController.LazyUpdate;
begin
	Document.Strings.Assign(View.PhpEditForm.Source.Strings)
end;

end.
