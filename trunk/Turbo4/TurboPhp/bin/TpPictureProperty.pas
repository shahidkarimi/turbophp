unit TpPictureProperty;

interface

uses
	TypInfo, SysUtils, Classes, Controls, Graphics, Dialogs, ExtDlgs,
	dcedit, dcfdes, dcsystem, dcdsgnstuff,
	ThPicture;

type
{
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
}
	//
	TTpPicturePathProperty = class(TStringProperty)
	public
		class procedure SetPaths(const inRoot, inFolder: string);
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
	StrUtils, LrUtils, ThPathUtils, TpStrings;

var
	OpenPictureDialog: TOpenPictureDialog;
	RootFolder: string;
	PictureFolder: string;
	LastPictureFolder: string;

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
{
function TTpPictureProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [ paDialog ];
end;

function TTpPictureProperty.GetDialog: TOpenPictureDialog;
begin
	if OpenPictureDialog = nil then
	begin
		OpenPictureDialog := TOpenPictureDialog.Create(nil);
		LastPictureFolder := '';
	end;
	if LastPictureFolder <> PictureFolder then
	begin
		OpenPictureDialog.InitialDir := PictureFolder;
		LastPictureFolder := PictureFolder;
	end;
	Result := OpenPictureDialog;
end;

function TTpPictureProperty.GetPicture: TThPicture;
begin
	//Result := TThPicture(GetComponent(0));
	Result := TThPicture(GetOrdValue);
end;

procedure TTpPictureProperty.Edit;
var
	p: string;
begin
	with GetDialog do
		if Execute then
		begin
			p := ExtractRelativePath(DocumentManagerForm.CurrentItem.Path, FileName);
			Picture.SetPaths(FileName, ThPathToUrl(p));
		end
		else if MessageDlg('Clear picture?', mtConfirmation, mbYesNoCancel, 0)
			= mrYes then
				Picture.SetPaths('', '');
end;

function TTpPictureProperty.GetValue: string;
begin
	Result := '(picture)';
end;
}

{ TTpPicturePathProperty }

class procedure TTpPicturePathProperty.SetPaths(const inRoot, inFolder: string);
begin
	RootFolder := inRoot;
	PictureFolder := inFolder;
	//NeedFolder(PictureFolder);
end;

function TTpPicturePathProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [ paDialog ];
end;

function TTpPicturePathProperty.GetPicture: TThPicture;
begin
	Result := TThPicture(GetComponent(0));
end;

procedure TTpPicturePathProperty.Edit;
var
	d: string;
begin
	with GetDialog do
		if Execute then
		begin
			if not IsSubfolder(FileName, RootFolder) then
				if MessageDlg(SCopyPictureMsg, mtConfirmation, mbYesNoCancel, 0)
					=	mrYes then
				begin
					NeedFolder(PictureFolder);
					d := PictureFolder + ExtractFileName(FileName);
					CopyFile(FileName, d);
					FileName := d;
				end;
			if not IsSubfolder(FileName, RootFolder) then
				SetStrValue(FileName)
			else begin
				d := ExtractRelativePath(RootFolder, FileName);
				SetStrValue(d);
				Picture.PictureUrl :=	ThPathToUrl(d);
			end;
		end
//		else if MessageDlg('Clear picture?', mtConfirmation, mbYesNoCancel, 0)
//			= mrYes then
//				SetStrValue('');
end;

function TTpPicturePathProperty.GetDialog: TOpenPictureDialog;
begin
	if OpenPictureDialog = nil then
	begin
		OpenPictureDialog := TOpenPictureDialog.Create(nil);
		LastPictureFolder := '';
	end;
	if LastPictureFolder <> PictureFolder then
	begin
		OpenPictureDialog.InitialDir := PictureFolder;
		LastPictureFolder := PictureFolder;
	end;
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
