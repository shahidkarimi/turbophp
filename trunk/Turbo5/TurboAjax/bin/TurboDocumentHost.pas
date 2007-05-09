unit TurboDocumentHost;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls,
	aqDockingBase, aqDocking, aqDockingUtils, aqDockingUI,
	CodeEdit, DesignView, TurboDocument;

type
	TTurboDocumentHostForm = class(TForm)
		aqDockingSite1: TaqDockingSite;
		aqDockingManager: TaqDockingManager;
		ChangeTimer: TTimer;
		DesignDock: TaqDockingControl;
		JavaScriptDock: TaqDockingControl;
		PhpDock: TaqDockingControl;
		HtmlDock: TaqDockingControl;
		PreviewDock: TaqDockingControl;
		procedure FormCreate(Sender: TObject);
		procedure ChangeTimerTimer(Sender: TObject);
	private
		{ Private declarations }
		FOnCodeChanged: TNotifyEvent;
		FOnDesignChanged: TNotifyEvent;
	protected
		//function GetDesign: TDesignForm;
		procedure CodeChanged(inSender: TObject);
		procedure CreateDesignViewForm;
		procedure CreateHtmlEditForm;
		procedure CreateJavaScriptEditForm;
		procedure CreatePhpEditForm;
		procedure CreatePreviewForm;
		procedure DesignChange(inSender: TObject);
		procedure SetDocument(inDocument: TTurboDocument);
	public
		{ Public declarations }
		DesignViewForm: TDesignViewForm;
		HtmlEditForm: TCodeEditForm;
		JavaScriptEditForm: TCodeEditForm;
		PhpEditForm: TCodeEditForm;
		procedure LazyUpdate;
		procedure LoadDockConfig(const inFolder: string);
		procedure SaveDockConfig(const inFolder: string);
		property Document: TTurboDocument write SetDocument;
		property OnCodeChanged: TNotifyEvent read FOnCodeChanged
			write FOnCodeChanged;
		property OnDesignChanged: TNotifyEvent read FOnDesignChanged
			write FOnDesignChanged;
	end;

var
	TurboDocumentHostForm: TTurboDocumentHostForm;

implementation

uses
	EasyStrings, LrUtils,
	DesignManager, Inspector, CodeExplorer;

const
	cDockConfig = 'aq.turbohost.dock.cfg';

{$R *.dfm}

procedure TTurboDocumentHostForm.FormCreate(Sender: TObject);
begin
	CreateDesignViewForm;
	CreateJavaScriptEditForm;
	CreatePhpEditForm;
	CreateHtmlEditForm;
	CreatePreviewForm;
	DesignDock.ForceVisible;
	DesignMgr.PropertyObservers.Add(DesignChange);
	DesignMgr.DesignObservers.Add(DesignChange);
end;

procedure TTurboDocumentHostForm.CreateDesignViewForm;
begin
	AddForm(DesignViewForm, TDesignViewForm, DesignDock);
end;

procedure TTurboDocumentHostForm.CreateJavaScriptEditForm;
begin
	AddForm(JavaScriptEditForm, TCodeEditForm, JavaScriptDock);
	with JavaScriptEditForm do
	begin
		Source.Parser := JsParser;
		OnModified := CodeChanged;
		Explorer := CodeExplorerForm;
	end;
end;

procedure TTurboDocumentHostForm.CreatePhpEditForm;
begin
	AddForm(PhpEditForm, TCodeEditForm, PhpDock);
	with PhpEditForm do
	begin
		Source.Parser := JsParser;
		OnModified := CodeChanged;
		Explorer := CodeExplorerForm;
	end;
end;

procedure TTurboDocumentHostForm.CreateHtmlEditForm;
begin
	//HtmlEditForm := CreateHtmlViewForm;
	//HtmlEditForm.Explorer := CodeExplorerForm;
	//InsertForm(HtmlEditForm, HtmlDock);
	AddForm(HtmlEditForm, THtmlEditForm, HtmlDock);
	with HtmlEditForm do
	begin
		Source.ReadOnly := true;
		Explorer := CodeExplorerForm;
	end;
{
	AddForm(HtmlEditForm, TCodeEditForm, HtmlDock);
	with HtmlEditForm do
	begin
		Source.Parser := HtmlParser;
		Edit.LineBreak := lbCR;
		Source.ReadOnly := true;
		Explorer := CodeExplorerForm;
	end;
}
end;

procedure TTurboDocumentHostForm.CreatePreviewForm;
begin
	//AddForm(BrowserForm, TBrowserForm, PreviewDock);
end;

procedure TTurboDocumentHostForm.LoadDockConfig(const inFolder: string);
begin
	if FileExists(inFolder + cDockConfig) then
		aqDockingManager.LoadFromFile(inFolder + cDockConfig);
end;

procedure TTurboDocumentHostForm.SaveDockConfig(const inFolder: string);
begin
	aqDockingManager.SaveToFile(inFolder + cDockConfig);
end;

procedure TTurboDocumentHostForm.CodeChanged(inSender: TObject);
begin
	if Assigned(OnCodeChanged) then
		OnCodeChanged(Self);
end;

procedure TTurboDocumentHostForm.DesignChange(inSender: TObject);
begin
	if Assigned(OnDesignChanged) then
		OnDesignChanged(Self);
end;

procedure TTurboDocumentHostForm.SetDocument(inDocument: TTurboDocument);
begin
	if inDocument <> nil then
	begin
		InspectorForm.DefaultComponent := inDocument.Design;
		DesignViewForm.DesignForm := inDocument.Design;
		OnDesignChanged := inDocument.DoModified;
		OnCodeChanged := inDocument.DoModified;
		Show;
	end else
	begin
		Hide;
		OnCodeChanged := nil;
		OnDesignChanged := nil;
		DesignViewForm.DesignForm := nil;
		InspectorForm.DefaultComponent := nil;
		CodeExplorerForm.EasyEdit := nil;
	end;
end;

procedure TTurboDocumentHostForm.LazyUpdate;
begin
{
	if Document <> nil then
	begin
		Document.Data.Php.Assign(PhpEditForm.Strings);
		Document.Data.JavaScript.Assign(JavaScriptEditForm.Strings);
	end;
}
end;

procedure TTurboDocumentHostForm.ChangeTimerTimer(Sender: TObject);
begin
{
	ChangeTimer.Enabled := false;
	Document.GenerateHtml(HtmlEditForm.Strings);
}
end;

end.
