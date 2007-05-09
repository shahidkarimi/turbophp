unit PhpView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs,
	dxDockPanel, dxDockControl,
	PhpEditView, IEView, StdCtrls;

type
  TPhpViewForm = class(TForm)
    ClientDockSite: TdxDockSite;
    dxLayoutDockSite1: TdxLayoutDockSite;
    dxTabContainerDockSite2: TdxTabContainerDockSite;
    CodeDock: TdxDockPanel;
    HtmlDock: TdxDockPanel;
    PreviewDock: TdxDockPanel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
		{ Public declarations }
		PhpEditForm: TPhpEditForm;
		PreviewForm: TIEForm;
	end;

implementation

uses
	LrUtils, PhpEventProperty, Main;

{$R *.dfm}

procedure TPhpViewForm.FormCreate(Sender: TObject);
begin
	AddForm(PhpEditForm, TPhpEditForm, CodeDock);
	//PhpEditForm.CodeDesigner.OnShowSource := ShowPhpSource;
	PhpEditForm.Edit.OnEnter := MainForm.EditorEnter;
	TPhpEventProperty.SetCodeDesigner(PhpEditForm.CodeDesigner);
	//
	AddForm(PreviewForm, TIEForm, PreviewDock);
end;

end.
