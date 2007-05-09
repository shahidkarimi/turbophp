unit Main;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls,
	DCGen,
	DynamicProperties, DynamicInspector, dcedit;

type
	TMainForm = class(TForm)
		LeftPanel: TPanel;
		Splitter1: TSplitter;
		Splitter2: TSplitter;
		ClientPanel: TPanel;
		Splitter3: TSplitter;
    Splitter4: TSplitter;
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
		procedure CreateComponentPalette;
		procedure CreateComponentTree;
		procedure CreateDesigner;
		procedure CreateInspector;
		procedure CreatePropertyBar;
		procedure DesignFilter(inSender, inObject: TObject; var inFilter: Boolean);
		procedure InspectorShowProperty(Sender: TObject;
			const PropEdit: TDCDsgnProperty; var show: Boolean);
    procedure DoRegisterComponents;
	public
		{ Public declarations }
		DynamicInspector: TDynamicPropertyInspector;
		EventInspector: TDynamicPropertyInspector;
end;

var
	MainForm: TMainForm;

implementation

uses
	dcsystem,
	LrUtils,
	Design, DesignView, DesignManager, ClassInfo, ComponentPalette, Inspector,
	ComponentTree, PropertyBar,	ScriptPanel;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
	RegisterEditClass(TypeInfo(TColor), nil, '', TDCSimpleEdit);
	DesignMgr.OnFilter := DesignFilter;
	DoRegisterComponents;
	CreateComponentPalette;
	CreateComponentTree;
	CreateInspector;
	CreatePropertyBar;
	CreateDesigner;
	ComponentPaletteForm.Top := 0;
	Splitter4.Top := ComponentPaletteForm.BoundsRect.Bottom;
	Splitter2.Top := ComponentTreeForm.BoundsRect.Bottom;
end;

procedure TMainForm.DoRegisterComponents;
begin
	ComponentRegistry.RegisterComponents(
		'Containers',
		[ TPanel, TScriptPanel ],
		[ 'Container Panel', 'Scripted Container Panel' ]
	);
end;

procedure TMainForm.CreateDesigner;
begin
	DesignForm := TDesignForm.Create(Self);
	with TScriptPanel.Create(DesignForm) do
	begin
		Name := 'Panel1';
		Parent := DesignForm;
	end;
	AddForm(DesignViewForm, TDesignViewForm, ClientPanel);
	DesignViewForm.DesignForm := DesignForm;
	DesignMgr.Designer := DesignForm.Designer;
end;

procedure TMainForm.CreateComponentPalette;
begin
	AddForm(ComponentPaletteForm, TComponentPaletteForm, LeftPanel, alTop);
	//ComponentPaletteForm.Height := 128;
end;

procedure TMainForm.CreateComponentTree;
begin
	AddForm(ComponentTreeForm, TComponentTreeForm, LeftPanel, alTop);
	ComponentTreeForm.Height := 128;
end;

procedure TMainForm.CreatePropertyBar;
begin
	AddForm(PropertyBarForm, TPropertyBarForm, ClientPanel, alTop);
	Splitter3.Top := PropertyBarForm.BoundsRect.Bottom;
end;

procedure TMainForm.CreateInspector;
begin
	AddForm(InspectorForm, TInspectorForm, LeftPanel);
	InspectorForm.DefaultComponent := DesignForm;
	//InspectorForm.ObjectInspector.OnShowProperty := InspectorShowProperty;
	//
{
	DynamicInspector := TDynamicPropertyInspector.Create(InspectorForm);
	DynamicInspector.CollectionName := 'CustomProps';
	InspectorForm.AddInspector('Custom', DynamicInspector);
	//
	EventInspector := TDynamicPropertyInspector.Create(InspectorForm);
	EventInspector.CollectionName := 'Events';
	InspectorForm.AddInspector('Events', EventInspector);
}
end;

procedure TMainForm.DesignFilter(inSender: TObject; inObject: TObject;
	var inFilter: Boolean);
begin
	inFilter := inFilter
						or (inObject = nil)
						or (inObject is TDynamicProperties)
						;
end;

procedure TMainForm.InspectorShowProperty(Sender: TObject;
	const PropEdit: TDCDsgnProperty; var show: Boolean);
begin
//	show := Copy(PropEdit.GetName, 1, 2) = 'On';
//	if InspectorTabs.SelectedTab.Index = 0 then
//		show := not show;
end;

end.
