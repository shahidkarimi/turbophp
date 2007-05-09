unit htTag;

interface

uses
	SysUtils, Classes, Contnrs,
	htAttributeList, htStyleList;

type
	ThtTag = class;
	//
	ThtTagList = class(TObjectList)
	private
		function GetTags(inIndex: Integer): ThtTag;
		procedure SetTags(inIndex: Integer; const Value: ThtTag);
	public
		property Tags[inIndex: Integer]: ThtTag read GetTags write SetTags;
	end;
	//
	ThtTagFlag = ( tfMono, tfHideIfEmpty, tfLineBreak );
	ThtTagFlags = set of ThtTagFlag;
	//
	ThtTag = class(ThtTagList)
	private
		FContent: TStringList;
		FElement: string;
		FAttributes: ThtAttributeList;
		FStyles: ThtStyleList;
		FFlags: ThtTagFlags;
    FExtraAttributes: string;
	protected
		function GetCloseTag: string;
		function GetContent: TStrings;
		function GetHtml: string;
		function GetOpenTag: string;
		procedure SetAttributes(const Value: ThtAttributeList);
		procedure SetContent(const Value: TStrings);
		procedure SetElement(const Value: string);
		procedure SetExtraAttributes(const Value: string);
		procedure SetFlags(const Value: ThtTagFlags);
		procedure SetStyles(const Value: ThtStyleList);
	public
		constructor Create(const inElement: string = ''; inFlags: ThtTagFlags = []);
		destructor Destroy; override;
		function Add: ThtTag; overload;
		function HasContent: Boolean;
		function HasTags: Boolean;
		procedure Add(const inString: string); overload;
		procedure Add(inTag: ThtTag); overload;
		property Attributes: ThtAttributeList read FAttributes write SetAttributes;
		property Element: string read FElement write SetElement;
		property ExtraAttributes: string read FExtraAttributes write SetExtraAttributes;
		property Flags: ThtTagFlags read FFlags write SetFlags;
		property Content: TStrings read GetContent write SetContent;
		property Styles: ThtStyleList read FStyles write SetStyles;
		property Html: string read GetHtml;
	end;

implementation

{ ThtTagList }

function ThtTagList.GetTags(inIndex: Integer): ThtTag;
begin
	Result := ThtTag(Items[inIndex]);
end;

procedure ThtTagList.SetTags(inIndex: Integer; const Value: ThtTag);
begin
	Items[inIndex] := Value;
end;

{ ThtTag }

procedure ThtTag.Add(const inString: string);
begin
	Content.Add(inString);
end;

procedure ThtTag.Add(inTag: ThtTag);
begin
	inherited Add(inTag);
end;

function ThtTag.Add: ThtTag;
begin
	Result := ThtTag.Create;
	inherited Add(Result);
end;

constructor ThtTag.Create(const inElement: string = '';
	inFlags: ThtTagFlags = []);
begin
	FFlags := inFlags;
	FAttributes := ThtAttributeList.Create;
	FStyles := ThtStyleList.Create;
	Element := inElement;
end;

destructor ThtTag.Destroy;
begin
	FContent.Free;
	FStyles.Free;
	FAttributes.Free;
	inherited;
end;

function ThtTag.GetContent: TStrings;
begin
	if FContent = nil then
		FContent := TStringList.Create;
	Result := FContent;
end;

function ThtTag.HasContent: Boolean;
begin
	Result := (FContent <> nil) and (Content.Count > 0);
end;

function ThtTag.HasTags: Boolean;
begin
	Result := Count > 0;
end;

function ThtTag.GetOpenTag: string;
begin
	if Element = '' then
		Result := ''
	else begin
		Result := '<' + Element + Attributes.HtmlAttributes;
		if ExtraAttributes <> '' then
			Result := Result + ' ' + ExtraAttributes;
		if tfMono in Flags then
			Result := Result + ' />'
		else
			Result := Result + '>';
	end;
end;

function ThtTag.GetCloseTag: string;
begin
	if (Element = '') or (tfMono in Flags) then
		Result := ''
	else
		Result := '</' + Element + '>';
end;

function ThtTag.GetHtml: string;
var
	i: Integer;
begin
	if (tfHideIfEmpty in Flags) and not HasContent and not HasTags then
		Result := ''
	else begin
		Result := GetOpenTag;
		for i := 0 to Pred(Count) do
			Result := Result + Tags[i].Html;
		if HasContent then
			Result := Result + Content.Text;
		Result := Result + GetCloseTag;
	end;
	if (tfLineBreak in Flags) then
		Result := Result + #13;
end;

procedure ThtTag.SetAttributes(const Value: ThtAttributeList);
begin
	FAttributes.Assign(Value);
end;

procedure ThtTag.SetContent(const Value: TStrings);
begin
	FContent.Assign(Value);
end;

procedure ThtTag.SetElement(const Value: string);
begin
	FElement := LowerCase(Value);
	if (Element = 'img') or (Element = 'input') then
		Include(FFlags, tfMono);
end;

procedure ThtTag.SetFlags(const Value: ThtTagFlags);
begin
	FFlags := Value;
end;

procedure ThtTag.SetStyles(const Value: ThtStyleList);
begin
	FStyles.Assign(Value);
end;

procedure ThtTag.SetExtraAttributes(const Value: string);
begin
	FExtraAttributes := Value;
end;

end.
