unit CustomInspector;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	TypInfo, Dialogs, StdCtrls,
	dcgen, dcsystem, dcdsgnstuff, dcdsgnutil, dcedit, oinspect;

type
	TCustomInplaceEdit = class(TOInplaceEdit)
	protected
		function GetEditType: TControlClass; override;
		function GetPopupType: TWinControlClass; override;
	end;
	//
	TCustomProperty = class(TInterfacedObject, IDcDsgnProperty)
	public
		procedure GetProperties(Proc: TDCDsgnGetPropProc); virtual;
		function GetPropInfo: PPropInfo;  virtual;
		function GetName: string; virtual;
		procedure Edit; virtual;
		function AllEqual: Boolean; virtual;
		function GetValue: string; virtual;
		procedure GetValues(Proc: TGetStrProc); virtual;
		procedure SetValue(const Value: string); virtual;
		function GetPropType: PTypeInfo; virtual;
		function AutoFill: Boolean; virtual;
		procedure Activate; virtual;
		function ValueAvailable: Boolean; virtual;
		function GetAttributes: TDCDsgnPropertyAttributes; virtual;
		function HasInstance(Instance: TPersistent): Boolean; virtual;
		function GetEditLimit: Integer; virtual;
		function GetEditValue(out Value: string): Boolean; virtual;
		procedure Revert; virtual;
		destructor Destroy; override;
	end;
	//
	TCustomInspector = class(TCustomObjectInspector)
	protected
		PropList: TList;
	protected
{
		procedure GetAllPropertyEditors(Components: TComponentList;
			Filter: TTypeKinds; Designer: TFormDesigner;
			Proc: TGetPropEditProc); override;
}
		function GetEditorClass(const PropEdit: TDCDsgnProp): TControlClass; override;
		function GetInplaceEditClass: TInplaceEditClass; override;
		function GetPopupClass(const PropEdit: TDCDsgnProp): TWinControlClass; override;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
	end;

implementation

{ TCustomInplaceEdit }

function TCustomInplaceEdit.GetEditType: TControlClass;
begin
	Result := TEdit; //TDCSimpleEdit;
end;

function TCustomInplaceEdit.GetPopupType: TWinControlClass;
begin
	Result := TPopupListBox;
end;

{ TCustomProperty }

procedure TCustomProperty.Activate;
begin
	//
end;

function TCustomProperty.AllEqual: Boolean;
begin
	Result := false;
end;

function TCustomProperty.AutoFill: Boolean;
begin
	Result := true;
end;

destructor TCustomProperty.Destroy;
begin
	inherited;
end;

procedure TCustomProperty.Edit;
begin
	//
end;

function TCustomProperty.GetAttributes: TDCDsgnPropertyAttributes;
begin
	Result := [ paDialog ];
end;

function TCustomProperty.GetEditLimit: Integer;
begin
	Result := 65;
end;

function TCustomProperty.GetEditValue(out Value: string): Boolean;
begin
	Value := 'Edit Value';
	Result := true;
end;

function TCustomProperty.GetName: string;
begin
	Result := 'Property';
end;

procedure TCustomProperty.GetProperties(Proc: TDCDsgnGetPropProc);
begin
	//
end;

function TCustomProperty.GetPropInfo: PPropInfo;
begin
	Result := nil;
end;

function TCustomProperty.GetPropType: PTypeInfo;
begin
	Result := nil;
end;

function TCustomProperty.GetValue: string;
begin
	Result := 'Value';
end;

procedure TCustomProperty.GetValues(Proc: TGetStrProc);
begin
	//
end;

function TCustomProperty.HasInstance(Instance: TPersistent): Boolean;
begin
	Result := false;
end;

procedure TCustomProperty.Revert;
begin
	//
end;

procedure TCustomProperty.SetValue(const Value: string);
begin
	//
end;

function TCustomProperty.ValueAvailable: Boolean;
begin
	Result := true;
end;

{ TCustomInspector }

constructor TCustomInspector.Create(inOwner: TComponent);
begin
	inherited;
	CurrentControl := Self;
	PropList := TList.Create;
end;

destructor TCustomInspector.Destroy;
begin
	PropList.Free;
	inherited;
end;

function TCustomInspector.GetEditorClass(
	const PropEdit: TDCDsgnProp): TControlClass;
begin
	Result := TDCSimpleEdit;
end;

{
procedure TCustomInspector.GetAllPropertyEditors(Components: TComponentList;
	Filter: TTypeKinds; Designer: TFormDesigner; Proc: TGetPropEditProc);
var
	i: Integer;
	prop: TCustomProperty;
begin
	PropList.Clear;
	for i := 0 to 5 do
	begin
		prop := TCustomProperty.Create;
		PropList.Add(prop);
		Proc(prop);
	end;
end;
}

function TCustomInspector.GetPopupClass(
	const PropEdit: TDCDsgnProp): TWinControlClass;
begin
	Result := TPopupListBox;
end;

function TCustomInspector.GetInplaceEditClass: TInplaceEditClass;
begin
	Result := TCustomInplaceEdit;
end;

end.
 