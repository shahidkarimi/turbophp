unit TpImageList;

interface

uses
	SysUtils, Classes, Graphics,
	dcfdes, dcsystem, dcdsgnstuff,
	Dialogs, ImgList, ThImageList;

type
	TTpImageList = class(TThImageList)
	protected
		procedure SetImageHeight(const Value: Integer);
		procedure SetImageWidth(const Value: Integer);
		function GetImageHeight: Integer;
		function GetImageWidth: Integer;
	public
		constructor Create(inOwner: TComponent); override;
		procedure DumpImages(const inFolder: string);
		function GetImageName(inIndex: Integer): string;
		function GetImageUrl(inIndex: Integer): string; override;
	published
		property ImageHeight: Integer read GetImageHeight write SetImageHeight;
		property ImageWidth: Integer read GetImageWidth write SetImageWidth;
	end;
	//
	TTpImageListPropertyEditor = class(TStringProperty)
	public
		function GetAttributes: TPropertyAttributes; override;
		procedure Edit; override;
	end;

procedure RegisterImageListPropertyEditor;

const
	cTpAutoImageFolder = 'auto_images/';

implementation

uses
	DcimgEd;

procedure RegisterImageListPropertyEditor;
begin
	RegisterPropertyEditor(TypeInfo(TCustomImageList), TTpImageList,
		'ImageList', TTpImageListPropertyEditor);
end;

{ TTpImageList }

constructor TTpImageList.Create(inOwner: TComponent);
begin
	inherited;
end;

function TTpImageList.GetImageHeight: Integer;
begin
	Result := ImageList.Height;
end;

function TTpImageList.GetImageWidth: Integer;
begin
	Result := ImageList.Width;
end;

procedure TTpImageList.SetImageHeight(const Value: Integer);
begin
	ImageList.Height := Value;
end;

procedure TTpImageList.SetImageWidth(const Value: Integer);
begin
	ImageList.Width := Value;
end;

function TTpImageList.GetImageName(inIndex: Integer): string;
begin
	Result := Format('%s_%.3d.bmp', [ Name, inIndex ]);
end;

function TTpImageList.GetImageUrl(inIndex: Integer): string;
begin
	Result := cTpAutoImageFolder + GetImageName(inIndex);
end;

procedure TTpImageList.DumpImages(const inFolder: string);
var
	i: Integer;
begin
	for i := 0 to Pred(ImageList.Count) do
	begin
		with GetJpegStream(i) do
		try
			SaveToFile(inFolder + GetImageName(i));
		finally
			Free;
		end;
	end;
end;

{
function TThImageList.GetJpegStream(inIndex: Integer): TStream;
var
	b: TBitmap;
begin
	Result := TMemoryStream.Create;
	b := TBitmap.Create;
	try
		ImageList.GetBitmap(inIndex, b);
		ThBitmapToJpgStream(b, Result);
	finally
		b.Free;
	end;
end;
}

{ TTpImageListPropertyEditor }

function TTpImageListPropertyEditor.GetAttributes: TPropertyAttributes;
begin
	Result := [ paDialog ];
end;

procedure TTpImageListPropertyEditor.Edit;
var
	s: Integer;
begin
	s := 0;
	EditCustomImageList(TCustomImageList(GetOrdValue), s);
end;

end.
