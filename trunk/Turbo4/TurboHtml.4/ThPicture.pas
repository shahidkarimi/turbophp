unit ThPicture;

interface

uses
	SysUtils, Classes, Graphics,
	ThChangeNotifier;

type
	TThCustomPicture = class(TThChangeNotifier)
	public
		class procedure SetPictureFolders(
			const inPublishFolder, inProjectFolder: string);
	private
		FHeight: Integer;
		FPictureData: TPicture;
		FPicturePath: string;
		FPictureUrl: string;
		FOwner: TComponent;
		FResolvedPicturePath: string;
		FUseDesignSize: Boolean;
		FWidth: Integer;
	protected
		function GetPictureData: TPicture;
		function GetGraphic: TGraphic;
		procedure SetHeight(const Value: Integer);
		procedure SetPicturePath(const Value: string);
		procedure SetPictureUrl(const Value: string);
		procedure SetPictureData(const Value: TPicture);
		procedure SetUseDesignSize(const Value: Boolean);
		procedure SetWidth(const Value: Integer);
	protected
		property Owner: TComponent read FOwner;
		property PictureData: TPicture read GetPictureData write SetPictureData;
		property PictureUrl: string read FPictureUrl write SetPictureUrl;
		property PicturePath: string read FPicturePath write SetPicturePath;
		property ResolvedPicturePath: string read FResolvedPicturePath;
	public
		constructor Create(AOwner: TComponent);
		destructor Destroy; override;
		function AspectHeight(inWidth: Integer): Integer;
		function AspectWidth(inHeight: Integer): Integer;
		function HasGraphic: Boolean;
		procedure Assign(Source: TPersistent); override;
		procedure ResolvePicturePath;
		procedure SetPaths(const inPath, inUrl: string);
	public
		property Graphic: TGraphic read GetGraphic;
		property Width: Integer read FWidth write SetWidth;
		property Height: Integer read FHeight write SetHeight;
		property UseDesignSize: Boolean read FUseDesignSize
			write SetUseDesignSize default true;
	end;
	//
	TThPicture = class(TThCustomPicture)
	public
		property PictureData;
	published
		property PictureUrl;
		property PicturePath;
	end;

implementation

uses
	ThPathUtils;

var
	PublishedImagesFolder: string;
	ProjectImagesFolder: string;

{ TThCustomPicture }

class procedure TThCustomPicture.SetPictureFolders(
	const inPublishFolder, inProjectFolder: string);
begin
	PublishedImagesFolder := inPublishFolder;
	ProjectImagesFolder := inProjectFolder;
end;

constructor TThCustomPicture.Create(AOwner: TComponent);
begin
	inherited Create;
	FOwner := AOwner;
	FPictureData := TPicture.Create;
	FUseDesignSize := true;
end;

destructor TThCustomPicture.Destroy;
begin
	FPictureData.Free;
	inherited;
end;

procedure TThCustomPicture.Assign(Source: TPersistent);
begin
	if not (Source is TThCustomPicture) then
		inherited
	else with TThCustomPicture(Source) do
	begin
		Self.PictureUrl := PictureUrl;
		Self.PicturePath := PicturePath;
		Self.Width := Width;
		Self.Height := Height;
		//Self.PictureData.Assign(PictureData);
	end;
end;

	function PictureHasGraphic(inPicture: TPicture): Boolean;
	begin
		Result := (inPicture.Graphic <> nil) and not (inPicture.Graphic.Empty);
	end;

function TThCustomPicture.HasGraphic: Boolean;
begin
	Result := PictureHasGraphic(PictureData);
end;

function TThCustomPicture.GetGraphic: TGraphic;
begin
	Result := PictureData.Graphic;
end;

{
procedure TThCustomPicture.MarshallPicture;
var
	f: string;
begin
	try
		f := ThAppendPath(PictureSource, ExtractFileName(ResolvedPicturePath));
		if FileExists(f) then
		begin
			ForceDirectories(ExtractFilePath(ResolvedPicturePath));
			ThMemCopyFile(f, ResolvedPicturePath);
		end;
	except
	end;
end;
}

function TThCustomPicture.GetPictureData: TPicture;
begin
	if PicturePath <> '' then
	begin
		//if not FileExists(ResolvedPicturePath) then
		//	MarshallPicture;
		if not PictureHasGraphic(FPictureData)
			and FileExists(ResolvedPicturePath) then
		try
			// Will repeat this part over and over if the file is somehow
			// not loadable (e.g. wrong format).
			// Should be a trap for that here.
			FPictureData.LoadFromFile(ResolvedPicturePath);
			if UseDesignSize and PictureHasGraphic(FPictureData) then
			begin
				Width := FPictureData.Width;
				Height := FPictureData.Height;
			end;
		except
			FPictureData.Graphic := nil;
		end;
	end;
	Result := FPictureData;
end;

procedure TThCustomPicture.ResolvePicturePath;
begin
	if ThIsFullPath(PicturePath) then
		FResolvedPicturePath := PicturePath
	else begin
		FResolvedPicturePath := ThAppendPath(PublishedImagesFolder, PicturePath);
		if not FileExists(FResolvedPicturePath) then
			FResolvedPicturePath := ThAppendPath(ProjectImagesFolder, PicturePath);
	end;
//		FResolvedPicturePath := ThAppendPath(PictureRoot, PicturePath);
end;

procedure TThCustomPicture.SetPaths(const inPath, inUrl: string);
begin
	FPicturePath := inPath;
	ResolvePicturePath;
	FPictureUrl := inUrl;
	FPictureData.Graphic := nil;
	Change;
end;

procedure TThCustomPicture.SetPicturePath(const Value: string);
begin
	if (FPicturePath <> Value) then
	begin
		FPicturePath := Value;
		ResolvePicturePath;
		FPictureData.Graphic := nil;
		if (csDesigning in Owner.ComponentState) then
			GetPictureData;
		Change;
	end;
end;

procedure TThCustomPicture.SetPictureData(const Value: TPicture);
begin
	FPictureData.Assign(Value);
	Change;
end;

procedure TThCustomPicture.SetPictureUrl(const Value: string);
begin
	FPictureUrl := Value;
	Change;
end;

function TThCustomPicture.AspectHeight(inWidth: Integer): Integer;
begin
	Result := inWidth * Height div Width;
end;

function TThCustomPicture.AspectWidth(inHeight: Integer): Integer;
begin
	Result := inHeight * Width div Height;
end;

procedure TThCustomPicture.SetHeight(const Value: Integer);
begin
	FHeight := Value;
end;

procedure TThCustomPicture.SetUseDesignSize(const Value: Boolean);
begin
	FUseDesignSize := Value;
end;

procedure TThCustomPicture.SetWidth(const Value: Integer);
begin
	FWidth := Value;
end;

end.
