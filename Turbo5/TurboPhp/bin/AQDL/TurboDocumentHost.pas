unit TurboDocumentHost;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs,
	aqDockingBase, aqDocking, aqDockingUtils, aqDockingUI,
	TurboPhpDocument, Design, CodeEdit, ExtCtrls;

type
	TTurboDocumentHostForm = class(TForm)
		aqDockingSite1: TaqDockingSite;
		aqDockingManager1: TaqDockingManager;
		DesignDock: TaqDockingControl;
		PhpDock: TaqDockingControl;
		JavaScriptDock: TaqDockingControl;
		PreviewDock: TaqDockingControl;
    HtmlDock: TaqDockingControl;
    ChangeTimer: TTimer;
		procedure FormCreate(Sender: TObject);
		procedure ChangeTimerTimer(Sender: TObject);
	private
		{ Private declarations }
		FDocument: TTurboPhpDocument;
		HtmlEditForm: TCodeEditForm;
		JavaScriptEditForm: TCodeEditForm;
		PhpEditForm: TCodeEditForm;
	protected
		function GetDesign: TDesignForm;
		procedure DesignChange(inSender: TObject);
		procedure DesignFilter(inSender, inObject: TObject;
			var inFilter: Boolean);
		procedure SetDocument(const Value: TTurboPhpDocument);
		property Design: TDesignForm read GetDesign;
	public
		{ Public declarations }
		procedure LazyUpdate;
		procedure LoadDockConfig;
		procedure SaveDockConfig;
		property Document: TTurboPhpDocument read FDocument write SetDocument;
	end;

var
	TurboDocumentHostForm: TTurboDocumentHostForm;

implementation

uses
	EasyStrings, LrUtils, Globals, DesignHost, DockingUtils,
	Documents, DesignManager, BrowserView;

const
	cDockConfig = 'aq.turbodock.cfg';

{$R *.dfm}

procedure TTurboDocumentHostForm.FormCreate(Sender: TObject);
begin
	AddForm(DesignHostForm, TDesignHostForm, DesignDock);
	//
	AddForm(JavaScriptEditForm, TCodeEditForm, JavaScriptDock);
	JavaScriptEditForm.Source.Parser := JavaScriptEditForm.JsParser;
	//
	AddForm(HtmlEditForm, TCodeEditForm, HtmlDock);
	HtmlEditForm.Source.Parser := HtmlEditForm.HtmlParser;
	HtmlEditForm.Edit.LineBreak := lbCR;
	HtmlEditForm.Source.ReadOnly := true;
	HtmlEditForm.SplitterPanel.Bars[0].Minimize;
	//
	AddForm(PhpEditForm, TCodeEditForm, PhpDock);
	//
	AddForm(BrowserForm, TBrowserForm, PreviewDock);
	//
	LoadDockConfig;
	DesignDock.ForceVisible;
	//
	DesignMgr.PropertyObservers.Add(DesignChange);
	DesignMgr.DesignObservers.Add(DesignChange);
	//
	//PreviewDock.MakeFloating(Rect(100, 100, 200, 200));
	//HTMLDock.MakeFloating(Rect(100, 100, 200, 200));
end;

procedure TTurboDocumentHostForm.LoadDockConfig;
begin
	LoadDockLayout(aqDockingManager1, ConfigHome + cDockConfig);
end;

procedure TTurboDocumentHostForm.SaveDockConfig;
begin
	SaveDockLayout(aqDockingManager1, ConfigHome + cDockConfig);
end;

function TTurboDocumentHostForm.GetDesign: TDesignForm;
begin
	Result := Document.DesignForm;
end;

procedure TTurboDocumentHostForm.DesignFilter(inSender, inObject: TObject;
	var inFilter: Boolean);
begin
	inFilter := (inObject.ClassName = 'TDesignController');
end;

procedure TTurboDocumentHostForm.SetDocument(const Value: TTurboPhpDocument);
begin
	LazyUpdate;
	FDocument := Value;
	if Document <> nil then
	begin
		DesignHostForm.DesignForm := Design;
		DesignMgr.OnFilter := DesignFilter;
		DesignMgr.Container := Design;
		//DesignMgr.Design := Design;
		//DesignMgr.OnChange := Document.ModificationEvent; // DesignChange;
		Design.Visible := true;
		PhpEditForm.Strings := Document.Data.Php;
		PhpEditForm.OnModified := Document.ModificationEvent;
		JavaScriptEditForm.Strings := Document.Data.JavaScript;
		JavaScriptEditForm.OnModified := Document.ModificationEvent;
		BringToFront;
	end
	else begin
		//DesignMgr.Design := nil;
		DesignMgr.OnChange := nil;
		DesignHostForm.DesignForm := nil;
	end;
end;

procedure TTurboDocumentHostForm.LazyUpdate;
begin
	if Document <> nil then
	begin
		Document.Data.Php.Assign(PhpEditForm.Strings);
		Document.Data.JavaScript.Assign(JavaScriptEditForm.Strings);
	end;
end;

procedure TTurboDocumentHostForm.DesignChange(inSender: TObject);
begin
	if Document <> nil then
		Document.Modified := true;
	ChangeTimer.Enabled := false;
	ChangeTimer.Enabled := true;
end;

procedure TTurboDocumentHostForm.ChangeTimerTimer(Sender: TObject);
begin
	ChangeTimer.Enabled := false;
	Document.GenerateHtml(HtmlEditForm.Strings);
end;

end.
