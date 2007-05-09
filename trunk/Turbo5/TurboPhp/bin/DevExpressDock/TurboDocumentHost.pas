unit TurboDocumentHost;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls,
	dxDockControl, dxDockPanel,
	TurboPhpDocument, Design;

type
	TTurboDocumentHostForm = class(TForm)
		dxDockingManager1: TdxDockingManager;
		DesignDock: TdxDockPanel;
		dxDockSite1: TdxDockSite;
		dxLayoutDockSite1: TdxLayoutDockSite;
		PhpDock: TdxDockPanel;
		JavaScriptDock: TdxDockPanel;
		PreviewDock: TdxDockPanel;
		DebugDock: TdxDockPanel;
		dxTabContainerDockSite2: TdxTabContainerDockSite;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
	private
		{ Private declarations }
		FDocument: TTurboPhpDocument;
	protected
		procedure DesignChange(inSender: TObject);
		procedure SetDocument(const Value: TTurboPhpDocument);
	public
		{ Public declarations }
		procedure ActivateDesign;
		procedure DeactivateDesign;
		procedure LoadDockConfig;
		procedure SaveDockConfig;
		property Document: TTurboPhpDocument read FDocument write SetDocument;
	end;

var
	TurboDocumentHostForm: TTurboDocumentHostForm;

implementation

uses
	LrUtils, Globals, DesignHost, PhpEdit, JavaScriptEdit, DockingUtils,
  Documents;

const
	cDockConfig = 'turbodock.cfg';

{$R *.dfm}

procedure TTurboDocumentHostForm.FormCreate(Sender: TObject);
begin
	AddForm(DesignHostForm, TDesignHostForm, DesignDock);
	AddForm(JavaScriptEditForm, TJavaScriptEditForm, JavaScriptDock);
	AddForm(PhpEditForm, TPhpEditForm, PhpDock);
end;

procedure TTurboDocumentHostForm.FormDestroy(Sender: TObject);
begin
	//
end;

procedure TTurboDocumentHostForm.LoadDockConfig;
begin
	LoadDockLayout(dxDockingManager1, ConfigHome + cDockConfig);
end;

procedure TTurboDocumentHostForm.SaveDockConfig;
begin
	SaveDockLayout(dxDockingManager1, ConfigHome + cDockConfig);
end;

procedure TTurboDocumentHostForm.SetDocument(
	const Value: TTurboPhpDocument);
begin
	FDocument := Value;
	if Document <> nil then
	begin
		DesignHostForm.DesignForm := Document.DesignForm;
		Document.DesignForm.OnChange := DesignChange;
		BringToFront;
	end
	else
		DesignHostForm.DesignForm := nil;
end;

procedure TTurboDocumentHostForm.ActivateDesign;
begin
	DesignHostForm.ActivateDesign;
end;

procedure TTurboDocumentHostForm.DeactivateDesign;
begin
	DesignHostForm.DeactivateDesign;
end;

procedure TTurboDocumentHostForm.DesignChange(inSender: TObject);
begin
	if (Document <> nil) and not Document.Modified then
	begin
		Document.Modified := true;
		DocumentsForm.UpdateDocumentTabs;
	end;
end;

end.
