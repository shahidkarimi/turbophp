unit InspectorItems;

interface

uses
	SysUtils, Classes, Dialogs, ExtDlgs, Forms, Graphics, 
	JvInspector, JvInspExtraEditors,
	htPicture;

type
	TInspectorPictureItem = class(TJvInspectorClassItem)
		class procedure RegisterAsDefaultItem;
		//class procedure UnregisterAsDefaultItem;
	protected
		FFilename: string;
		function ExecutePictureDialog: Boolean;
		procedure Edit; override;
		procedure SetFlags(const Value: TInspectorItemFlags); override;
	end;
	//
	TInspectorHtPictureItem = class(TInspectorPictureItem)
		class procedure RegisterAsDefaultItem;
		//class procedure UnregisterAsDefaultItem;
	protected
		procedure Edit; override;
	end;
	//
	TInspectorColorItemEx = class(TJvInspectorColorItem)
		class procedure RegisterAsDefaultItem;
	private
		function GetPropColor: TColor;
		procedure SetPropColor(const Value: TColor);
		//class procedure UnregisterAsDefaultItem;
	protected
		FDialogColor: TColor;
		function ExecuteColorDialog: Boolean;
		procedure Edit; override;
		procedure SetFlags(const Value: TInspectorItemFlags); override;
		property PropColor: TColor read GetPropColor write SetPropColor;
	end;

procedure RegisterInspectorItems;

implementation

uses
	JvFullColorDialogs;

procedure RegisterInspectorItems;
begin
	TInspectorPictureItem.RegisterAsDefaultItem;
	TInspectorHtPictureItem.RegisterAsDefaultItem;
	TInspectorColorItemEx.RegisterAsDefaultItem;
end;

{ TInspectorPictureItem }

class procedure TInspectorPictureItem.RegisterAsDefaultItem;
begin
	TJvCustomInspectorData.ItemRegister.Add(
		TJvInspectorTypeInfoRegItem.Create(
			TInspectorPictureItem, TypeInfo(TPicture)));
//	TJvInspectorMultiPropData.ItemRegister.Add(
//		TJvInspectorTypeInfoRegItem.Create(
//			TInspectorPictureItem, TypeInfo(TPicture)));
end;

function TInspectorPictureItem.ExecutePictureDialog: Boolean;
begin
	with TOpenPictureDialog.Create(GetParentForm(Inspector)) do
		try
			Result := Execute;
			FFilename := Filename;
		finally
			Free;
		end;
end;

procedure TInspectorPictureItem.Edit;
begin
	if ExecutePictureDialog then
		TPicture(Data.AsOrdinal).LoadFromFile(FFilename);
end;

procedure TInspectorPictureItem.SetFlags(const Value: TInspectorItemFlags);
var
	NewValue: TInspectorItemFlags;
begin
	NewValue := Value + [ iifEditButton, iifEditFixed ];
	inherited SetFlags(NewValue);
end;

{ TInspectorHtPictureItem }

class procedure TInspectorHtPictureItem.RegisterAsDefaultItem;
begin
	TJvCustomInspectorData.ItemRegister.Add(
		TJvInspectorTypeInfoRegItem.Create(
			TInspectorHtPictureItem, TypeInfo(ThtPicture)));
//	TJvInspectorMultiPropData.ItemRegister.Add(
//		TJvInspectorTypeInfoRegItem.Create(
//			TInspectorHtPictureItem, TypeInfo(ThtPicture)));
end;

procedure TInspectorHtPictureItem.Edit;
begin
	if ExecutePictureDialog then
		ThtPicture(Data.AsOrdinal).Filename := FFilename;
end;

{ TInspectorColorItemEx }

var
	SharedColorDialog: TColorDialog;

	function ColorDialog: TColorDialog;
	begin
		if SharedColorDialog = nil then
		begin
			//	TJvFullColorDialog.Create(Application)
			SharedColorDialog := TColorDialog.Create(Application);
			SharedColorDialog.Options := [ cdFullOpen, cdAnyColor ];
		end;
		Result := SharedColorDialog;
	end;

class procedure TInspectorColorItemEx.RegisterAsDefaultItem;
begin
	TJvCustomInspectorData.ItemRegister.Add(
		TJvInspectorTypeInfoRegItem.Create(
			TInspectorColorItemEx, TypeInfo(TColor)));
end;

function TInspectorColorItemEx.GetPropColor: TColor;
begin
	Result := Data.AsOrdinal;
end;

procedure TInspectorColorItemEx.SetPropColor(const Value: TColor);
begin
	Data.AsOrdinal := Value;
end;

function TInspectorColorItemEx.ExecuteColorDialog: Boolean;
begin
	with ColorDialog do
	begin
		Color := PropColor;
		Result := Execute;
		if Result then
			PropColor := Color;
	end;
end;

procedure TInspectorColorItemEx.Edit;
begin
	ExecuteColorDialog;
end;

procedure TInspectorColorItemEx.SetFlags(const Value: TInspectorItemFlags);
var
	NewValue: TInspectorItemFlags;
begin
	NewValue := Value + [ iifEditButton, iifEditFixed ];
	inherited SetFlags(NewValue);
end;

end.

