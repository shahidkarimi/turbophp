unit ThAnchor;

interface

uses
	SysUtils, Classes, Graphics,
	ThChangeNotifier, ThTag, ThAnchorStyles;

type
	TThCustomAnchor = class(TThChangeNotifier)
	private
		FTarget: string;
		FHref: string;
		FStyles: TThAnchorStyles;
    FName: string;
    procedure SetName(const Value: string);
	protected
		procedure SetHref(const Value: string);
		procedure SetTarget(const Value: string);
    procedure SetStyles(const Value: TThAnchorStyles);
	protected
		procedure Tag(inTag: TThTag); virtual;
	protected
		property Href: string read FHref write SetHref;
		property Name: string read FName write SetName;
		property Target: string read FTarget write SetTarget;
		property Styles: TThAnchorStyles read FStyles write SetStyles;
	public
		constructor Create; 
		destructor Destroy; override;
		function Wrap(const inContent: string): string;
		function Empty: Boolean;
	end;
	//
	TThAnchor = class(TThCustomAnchor)
	public
		property Name;
	published
		property Href;
		property Styles;
		property Target;
	end;

implementation

{ TThCustomAnchor }

constructor TThCustomAnchor.Create;
begin
	FStyles := TThAnchorStyles.Create;
end;

destructor TThCustomAnchor.Destroy;
begin
	FStyles.Free;
	inherited;
end;

function TThCustomAnchor.Empty: Boolean;
begin
	Result := (Href = '') and (FTarget = '');
end;

procedure TThCustomAnchor.Tag(inTag: TThTag);
begin
	inTag.Add('href', Href);
	inTag.Add('target', Target);
end;

function TThCustomAnchor.Wrap(const inContent: string): string;
begin
	if Empty then
		Result := inContent
	else
		with TThTag.Create('a') do
		try
			if Self.Styles.HasStyles and (Name <> '') then
				Add('class', Name);
			Content := inContent;
			Tag(ThisTag);
			Result := HTML;
		finally
			Free;
		end;
end;

procedure TThCustomAnchor.SetHref(const Value: string);
begin
	FHref := Value;
	Change;
end;

procedure TThCustomAnchor.SetTarget(const Value: string);
begin
	FTarget := Value;
	Change;
end;

procedure TThCustomAnchor.SetStyles(const Value: TThAnchorStyles);
begin
	FStyles.Assign(Value);
end;

procedure TThCustomAnchor.SetName(const Value: string);
begin
  FName := Value;
end;

end.
