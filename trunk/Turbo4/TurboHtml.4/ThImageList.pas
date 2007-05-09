unit ThImageList;

interface

uses
	SysUtils, Classes, Controls, Graphics,
	ThComponent;

type
	TThImageList = class(TThComponent)
	private
		FImageList: TImageList;
	protected
		function GetJpegStream(inIndex: Integer): TStream;
		procedure SetImageList(const Value: TImageList);
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		function GetImageUrl(inIndex: Integer): string; virtual; abstract;
	published
		property ImageList: TImageList read FImageList write SetImageList;
		//property Url;
	end;

implementation

uses
	ThGraphicUtils;

{ TThImageList }

constructor TThImageList.Create(inOwner: TComponent);
begin
	inherited;
	FImageList := TImageList.Create(Self);
	FImageList.SetSubComponent(true);
end;

destructor TThImageList.Destroy;
begin
	FImageList.Free;
	inherited;
end;

procedure TThImageList.SetImageList(const Value: TImageList);
begin
	//FImageList := Value;
end;

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

{
procedure TThImageList.Fulfill(inAction: TThAction);
var
	ii: Integer;
begin
	if ImageList <> nil then
	begin
		ii := StrToIntDef(Copy(inAction.Document, 2, MAXINT), 0);
		inAction.ServeStream(GetJpegStream(ii), 'image/jpg');
	end;
end;

function TThImageList.GetImageUrl(inIndex: Integer): string;
begin
	Result := Format('%s/%d', [ Url, inIndex ]);
end;
}

end.
