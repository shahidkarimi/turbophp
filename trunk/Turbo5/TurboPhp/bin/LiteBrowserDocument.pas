unit LiteBrowserDocument;

interface

uses
	Classes, LrDocument;

type
	TLiteBrowserDocument = class(TLrDocument)
	public
		Html: TStringList;
		constructor Create; override;
		destructor Destroy; override;
		function Close: Boolean; override;
		procedure Activate; override;
		//procedure Deactivate; override;
		//procedure Open(const inFilename: string); override;
		//procedure Save; override;
		//procedure SaveAs(const inFilename: string); override;
	end;

implementation

uses
	LiteBrowser;

{ TLiteBrowserDocument }

constructor TLiteBrowserDocument.Create;
begin
	inherited;
	Html := TStringList.Create;
end;

destructor TLiteBrowserDocument.Destroy;
begin
	Html.Free;
	inherited;
end;

procedure TLiteBrowserDocument.Activate;
begin
	inherited;
	LiteBrowserForm.Html := Html;
	LiteBrowserForm.BringToFront;
end;

function TLiteBrowserDocument.Close: Boolean;
begin
	Result := true;
end;

end.
