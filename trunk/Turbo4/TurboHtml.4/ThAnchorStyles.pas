unit ThAnchorStyles;

interface

uses
	SysUtils, Classes, Graphics,
	ThChangeNotifier, ThCssStyle, ThStyleSheet;

type
	TThCssStyleName = string;
	//
	TThAnchorStyles = class(TThChangeNotifier)
	private
		FVisited: TThCssStyleName;
		FLink: TThCssStyleName;
		FActive: TThCssStyleName;
		FHover: TThCssStyleName;
	protected
		procedure SetActive(const Value: TThCssStyleName);
		procedure SetHover(const Value: TThCssStyleName);
		procedure SetLink(const Value: TThCssStyleName);
		procedure SetVisited(const Value: TThCssStyleName);
		function InlineStyle(inSheet: TThStyleSheet;
			const inName: string): string;
		function BuildIdList(var inClasses: array of string; const inName: string;
			inI: Integer): string;
	public
		function HasStyles: Boolean;
		procedure GenerateStyles(inStyles: TStrings; inSheet: TThStyleSheet;
			const inClass: string = '');
	published
		property Link: TThCssStyleName read FLink write SetLink;
		property Hover: TThCssStyleName read FHover write SetHover;
		property Visited: TThCssStyleName read FVisited write SetVisited;
		property Active: TThCssStyleName read FActive write SetActive;
	end;

implementation

{ TThAnchorStyles }

function TThAnchorStyles.InlineStyle(inSheet: TThStyleSheet;
	const inName: string): string;
var
	s: TThCssStyle;
begin
	s := inSheet.Styles.GetStyleByName(inName);
	if (s = nil) then
		Result := ''
	else
		Result := s.InlineAttribute;
end;

function TThAnchorStyles.BuildIdList(var inClasses: array of string;
	const inName: string; inI: Integer): string;
const
	cPseudoClass: array[0..3] of string = ( 'link', 'hover', 'visited',
		'active' );
var
	c: string;
	i: Integer;

	function Selector: string;
	begin
		Result := 'a';
		if inName <> '' then
			Result := Result + '.' + inName;
		Result := Result + ':' + cPseudoClass[i];
	end;

begin
	c := inClasses[inI];
	if c = '' then
		Result := ''
	else begin
		i := inI;
		Result := Selector;
		for i := inI + 1 to 3 do
			if (inClasses[i] = c) then
			begin
				inClasses[i] := '';
				Result := Result + ', ' + Selector;
			end;
	end;
end;

procedure TThAnchorStyles.GenerateStyles(inStyles: TStrings;
	inSheet: TThStyleSheet; const inClass: string = '');
var
	c: array[0..3] of string;
	i: Integer;
	s, id: string;
begin
	c[0] := Link;
	c[1] := Hover;
	c[2] := Visited;
	c[3] := Active;
	for i := 0 to 3 do
	begin
		id := BuildIdList(c, inClass, i);
		if (id <> '') then
		begin
			s := InlineStyle(inSheet, c[i]);
			if (s <> '') then
				inStyles.Add(id + ' { ' + s + ' }');
		end;
	end;
end;

function TThAnchorStyles.HasStyles: Boolean;
begin
	Result := (Link <> '') or (Hover <> '') or (Visited <> '') or (Active <> '');
end;

procedure TThAnchorStyles.SetActive(const Value: TThCssStyleName);
begin
	FActive := Value;
end;

procedure TThAnchorStyles.SetHover(const Value: TThCssStyleName);
begin
	FHover := Value;
end;

procedure TThAnchorStyles.SetLink(const Value: TThCssStyleName);
begin
	FLink := Value;
end;

procedure TThAnchorStyles.SetVisited(const Value: TThCssStyleName);
begin
	FVisited := Value;
end;

end.
