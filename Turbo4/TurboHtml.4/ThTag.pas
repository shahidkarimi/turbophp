unit ThTag;

interface

uses
	SysUtils, Classes,
	ThStyledCtrl,	ThAttributeList, ThStyleList;

type
	//
	// This is only here so that ThTag unit is automatically included by the
	// Delphi designer in forms that use WebControls
	//
	TThWebControlBase = class(TThStyledCustomControl)
	end;
	//
	TThTag = class
	private
		FElement: string;
		FAttributes: TThAttributeList;
		FStyles: TThStyleList;
		FMono: Boolean;
		FContent: string;
	protected
		function GetCloseTag: string;
		function GetHtml: string;
		function GetOpenTag: string;
		procedure SetContent(const Value: string);
		procedure SetElement(const Value: string);
	protected
		function GetBaseTag: string;
	public
		constructor Create(const inElement: string = '');
		destructor Destroy; override;
		procedure Add(const inName, inValue: string); overload;
		procedure Add(const inName: string; inValue: Integer); overload;
		procedure AddStyle(const inName, inValue: string;
			const inUnits: string = ''); overload;
		procedure AddStyle(const inName: string; inValue: Integer;
			const inUnits: string = ''); overload;
		procedure Clear;
		procedure Remove(const inName: string);
		function ThisTag: TThTag;
	public
		property Attributes: TThAttributeList read FAttributes;
		property Content: string read FContent write SetContent;
		property CloseTag: string read GetCloseTag;
		property Element: string read FElement write SetElement;
		property Html: string read GetHtml;
		property Mono: Boolean read FMono write FMono;
		property OpenTag: string read GetOpenTag;
		property Styles: TThStyleList read FStyles;
	end;

implementation

{ TThTag }

constructor TThTag.Create(const inElement: string = '');
begin
	FAttributes := TThAttributeList.Create;
	FStyles := TThStyleList.Create;
	FMono := true;
	Element := inElement;
end;

destructor TThTag.Destroy;
begin
	FStyles.Free;
	FAttributes.Free;
	inherited;
end;

function TThTag.ThisTag: TThTag;
begin
	Result := Self;
end;

procedure TThTag.Clear;
begin
	FStyles.Clear;
	FAttributes.Clear;
	FMono := true;
	FElement := '';
	FContent := '';
end;

procedure TThTag.Add(const inName, inValue: string);
begin
	Attributes.Add(inName, inValue);
end;

procedure TThTag.Add(const inName: string; inValue: Integer);
begin
	Attributes.Add(inName, inValue);
end;

procedure TThTag.AddStyle(const inName: string; inValue: Integer;
	const inUnits: string);
begin
	Styles.Add(inName, inValue, inUnits);
end;

procedure TThTag.AddStyle(const inName, inValue, inUnits: string);
begin
	Styles.Add(inName, inValue, inUnits);
end;

procedure TThTag.Remove(const inName: string);
begin
	Attributes.Remove(inName);
end;

procedure TThTag.SetContent(const Value: string);
begin
	FContent := Value;
	Mono := false;
end;

procedure TThTag.SetElement(const Value: string);
begin
	FElement := LowerCase(Value);
	//Mono := Mono and (FElement <> 'td');
end;

function TThTag.GetBaseTag: string;
begin
	Result := '<' + Element + Attributes.HtmlAttributes +
		Styles.StyleAttribute;
end;

function TThTag.GetOpenTag: string;
begin
	if Element = '' then
		Result := ''
	else if Element = '!--' then
		Result := GetBaseTag + '-->'
	else if Mono then
		Result := GetBaseTag + '/>'
	else
		Result := GetBaseTag + '>';
end;

function TThTag.GetCloseTag: string;
begin
	if Mono or (Element = '') then
		Result := ''
	else 
		Result := '</' + Element + '>';
end;

function TThTag.GetHtml: string;
begin
{
	if Element = '' then
		Result := Content
	else if Element = '!--' then
		Result := GetBaseTag + '-->'
	else if Mono then
		Result := GetBaseTag + '/>'
	else
}
		Result := OpenTag + Content + CloseTag;
end;

end.
