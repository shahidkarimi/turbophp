unit JavaScriptEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvExControls, JvComponent, JvEditorCommon, JvEditor, JvHLEditor;

type
  TJavaScriptEditForm = class(TForm)
    JvHLEditor1: TJvHLEditor;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  JavaScriptEditForm: TJavaScriptEditForm;

implementation

{$R *.dfm}

end.
