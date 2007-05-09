unit TpPictureProperty;

interface

uses
	TypInfo, SysUtils, Classes, Controls, Graphics, Dialogs, ExtDlgs,
	dcedit, dcfdes, dcsystem, dcdsgnstuff,
	ThPicture;

type
	TTpPictureProperty = class(TPropertyEditor)
	protected
		function GetDialog: TOpenPictureDialog;
		function GetPicture: TThPicture;
	public
		procedure Edit; override;
		function GetAttributes: TPropertyAttributes; override;
		function GetValue: string; override;
		property Picture: TThPicture read GetPicture;
	end;
	//
	TTpPicturePathProperty = class(TStringProperty)
	protected
		function GetDialog: TOpenPictureDialog;
		function GetPicture: TThPicture;
	public
		procedure Edit; override;
		function GetAttributes: TPropertyAttributes; override;
		property Picture: TThPicture read GetPicture;
	end;
	//
	TTpPictureUrlProperty = class(TStringProperty)
	public
		function GetValue: string; override;
	end;

procedure RegisterPicturePropertyEditor;

implementation

uses
	ThPathUtils{, DocumentManager, TpProject};

var
	OpenPictureDialog: TOpenPictureDialog;
	LastProjectRoot: string;

procedure RegisterPicturePropertyEditor;
begin
	//RegisterEditClass(TypeInfo(TPicture), nil, '', TDCSimpleEdit);
	//RegisterPropertyEditor(TypeInfo(TThPicture), nil, '', TTpPictureProperty);
	RegisterPropertyEditor(TypeInfo(string), nil, 'PicturePath',
		TTpPicturePathProperty);
	RegisterPropertyEditor(TypeInfo(string), nil, 'PictureUrl',
		TTpPictureUrlProperty);
end;

{ TTpPictureProperty }

function TTpPictureProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [ paDialog ];
end;

function TTpPictureProperty.GetDialog: TOpenPictureDialog;
begin
	if OpenPictureDialog = nil then
	begin
		OpenPictureDialog := TOpenPictureDialog.Create(nil);
		LastProjectRoot := '';
	end;
{
	if LastProjectRoot <> CurrentProject.Folder then
	begin
		OpenPictureDialog.InitialDir := CurrentProject.Folder;
		LastProjectRoot := CurrentProject.Folder;
	end;
}
	Result := OpenPictureDialog;
end;

function TTpPictureProperty.GetPicture: TThPicture;
begin
	//Result := TThPicture(GetComponent(0));
	Result := TThPicture(GetOrdValue);
end;

procedure TTpPictureProperty.Edit;
//var
//	p: string;
begin
{
	with GetDialog do
		if Execute then
		begin
			p := ExtractRelativePath(DocumentManagerForm.CurrentItem.Path, FileName);
			Picture.SetPaths(FileName, ThPathToUrl(p));
		end
		else if MessageDlg('Clear picture?', mtConfirmation, mbYesNoCancel, 0)
			= mrYes then
				Picture.SetPaths('', '');
}
end;

function TTpPictureProperty.GetValue: string;
begin
	Result := '(picture)';
end;

{ TTpPicturePathProperty }

function TTpPicturePathProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [ paDialog ];
end;

function TTpPicturePathProperty.GetPicture: TThPicture;
begin
	Result := TThPicture(GetComponent(0));
end;

procedure TTpPicturePathProperty.Edit;
begin
{
	with GetDialog do
		if Execute then
		begin
			SetStrValue(FileName);
			Picture.PictureUrl := ThPathToUrl(
				ExtractRelativePath(CurrentProject.Folder, FileName));
		end
//		else if MessageDlg('Clear picture?', mtConfirmation, mbYesNoCancel, 0)
//			= mrYes then
//				SetStrValue('');
}
end;

function TTpPicturePathProperty.GetDialog: TOpenPictureDialog;
begin
	if OpenPictureDialog = nil then
	begin
		OpenPictureDialog := TOpenPictureDialog.Create(nil);
		LastProjectRoot := '';
	end;
{
	if LastProjectRoot <> CurrentProject.Folder then
	begin
		OpenPictureDialog.InitialDir := CurrentProject.Folder;
		LastProjectRoot := CurrentProject.Folder;
	end;
}
	Result := OpenPictureDialog;
end;

{ TTpPictureUrlProperty }

function TTpPictureUrlProperty.GetValue: string;
begin
	Result := GetStrValue;
	if Result = '' then
		Result := '(auto)';
end;

end.
