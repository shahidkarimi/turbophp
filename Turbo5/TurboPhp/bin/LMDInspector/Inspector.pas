unit Inspector;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, Grids, ExtCtrls,
	JvTabBar,
	LMDDsgPropPage, LMDDsgPropInsp, LMDDsgComboBox,	LMDCustomControl,
	LMDCustomPanel, LMDCustomBevelPanel, LMDSimplePanel;

type
  TInspectorForm = class(TForm)
    LMDObjectComboBox1: TLMDObjectComboBox;
    LMDPropertyInspector1: TLMDPropertyInspector;
		Panel1: TPanel;
		Panel2: TPanel;
    JvTabBar1: TJvTabBar;
    JvModernTabBarPainter1: TJvModernTabBarPainter;
    LMDSimplePanel1: TLMDSimplePanel;
    JvModernTabBarPainter2: TJvModernTabBarPainter;
	private
		{ Private declarations }
	public
		{ Public declarations }
		procedure ComboAdd(inObject: TObject);
		procedure ComboClear;
		procedure Inspect(inObject: TObject);
	end;

var
	InspectorForm: TInspectorForm;

implementation

{$R *.dfm}

{ TInspectorForm }

procedure TInspectorForm.Inspect(inObject: TObject);
begin
	with LMDPropertyInspector1.Objects do
	begin
		Clear;
		if inObject <> nil then
			Add(TPersistent(inObject));
	end;
	with LMDObjectComboBox1.SelectedObjects do
	begin
		Clear;
		if inObject <> nil then
			Add(TPersistent(inObject));
	end;
end;

procedure TInspectorForm.ComboClear;
begin
	LMDObjectComboBox1.Objects.Clear;
end;

procedure TInspectorForm.ComboAdd(inObject: TObject);
begin
	if inObject <> nil then
		LMDObjectComboBox1.Objects.Add(TPersistent(inObject));
end;

end.
