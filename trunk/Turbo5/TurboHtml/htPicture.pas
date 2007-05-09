unit htPicture;

interface

uses
	SysUtils, Classes, Graphics,
	htPersistBase, htTypes;

type
	ThtCustomPicture = class(ThtPersistBase)
	private
		FFilename: string;
		FPictureValid: Boolean;
		FPicture: TPicture;
		FUrl: string;
	protected
		function GetGraphic: TGraphic;
		function GetPicture: TPicture;
		function GetUrl: string;
		procedure SetFilename(const Value: string);
		procedure SetPicture(const Value: TPicture);
		procedure ValidatePicture;
	public
		constructor Create;
		destructor Destroy; override;
		function HasGraphic: Boolean;
		procedure Assign(Source: TPersistent); override;
		property Filename: string read FFilename write SetFilename;
		property Picture: TPicture read GetPicture write SetPicture;
		property Graphic: TGraphic read GetGraphic;
		property Url: string read GetUrl write FUrl;
	end;
	//
	ThtPicture = class(ThtCustomPicture)
	published
		property Filename;
	end;

implementation

uses
	htUtils;

{ ThtCustomPicture }

constructor ThtCustomPicture.Create;
begin
	FPicture := TPicture.Create;
end;

destructor ThtCustomPicture.Destroy;
begin
	FPicture.Free;
	inherited;
end;

procedure ThtCustomPicture.Assign(Source: TPersistent);
begin
	if not (Source is ThtCustomPicture) then
		inherited
	else with ThtCustomPicture(Source) do
		Self.Filename := Filename;
end;

procedure ThtCustomPicture.SetFilename(const Value: string);
begin
	if Url = htForwardSlashes(Filename) then
		Url := htForwardSlashes(Value);
	FFilename := Value;
	FPictureValid := false;
	Change;
end;

function ThtCustomPicture.GetPicture: TPicture;
begin
	ValidatePicture;
	Result := FPicture;
end;

procedure ThtCustomPicture.SetPicture(const Value: TPicture);
begin
	FPicture.Assign(Value);
end;

procedure ThtCustomPicture.ValidatePicture;
begin
	if not FPictureValid then
		try
			FPictureValid := true;
			if FileExists(Filename) then
				FPicture.LoadFromFile(Filename);
		except
		end;
end;

function ThtCustomPicture.GetGraphic: TGraphic;
begin
	Result := Picture.Graphic;
end;

function ThtCustomPicture.HasGraphic: Boolean;
begin
	Result := (Graphic <> nil) and not (Graphic.Empty);
end;

function ThtCustomPicture.GetUrl: string;
begin
	if FUrl <> '' then
		Result := FUrl
	else
		Result := htForwardSlashes(FFilename);
end;

end.
