unit ThStyleSheet;

interface

uses
	SysUtils, Classes, Controls,
	ThCssStyle, ThStructuredHtml, ThHeaderComponent, ThMessages;

type
	TThStyles = class;
	//
	TThStylesItem = class(TCollectionItem)
	private
		FName: string;
		FStyle: TThCssStyle;
		FStyles: TThStyles;
	protected
		function GetDisplayName: string; override;
		procedure SetName(const Value: string);
		procedure SetStyle(const Value: TThCssStyle);
		procedure SetStyles(const Value: TThStyles);
	protected
		procedure ParentChanged(inStyle: TThStylesItem);
		procedure StyleChanged(inSender: TObject);
		procedure UpdateStyle;
	public
		constructor Create(inCollection: TCollection); override;
		destructor Destroy; override;
		procedure Emit(inStrings: TStrings);
		procedure ListClasses(var inClasses: string);
	public
		property Styles: TThStyles read FStyles write SetStyles;
	published
		property Name: string read FName write SetName;
		property Style: TThCssStyle read FStyle write SetStyle;
	end;
	//
	TThStyles = class(TOwnedCollection)
	private
		FOnStyleChange: TNotifyEvent;
	protected
		function GetStyleItem(inIndex: Integer): TThStylesItem;
		procedure SetStyleItem(inIndex: Integer; const Value: TThStylesItem);
		procedure SetOnStyleChange(const Value: TNotifyEvent);
	public
		constructor Create(inOwner: TPersistent);
		procedure GenerateStyles(inStrings: TStrings);
		function GetStyleItemByName(const inName: string): TThStylesItem;
		function GetStyleByName(const inName: string): TThCssStyle;
		procedure ListClasses(var inClasses: string);
		procedure ParentChanged(inStyle: TThStylesItem);
		procedure StyleChanged;
	public
		property StyleItem[inIndex: Integer]: TThStylesItem
			read GetStyleItem write SetStyleItem; default;
		property OnStyleChange: TNotifyEvent read FOnStyleChange write SetOnStyleChange;
	end;
	//
	TThStyleSheet = class(TComponent) //TThHeaderComponent)
	private
		FStyles: TThStyles;
	protected
		procedure SetStyles(const Value: TThStyles);
		procedure StylesStyleChange(inSender: TObject);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure AutoAttach;
		procedure Publish(inHtml: TThStructuredHtml);
		procedure SaveToFile(const inFilename: string);
	published
		property Styles: TThStyles read FStyles write SetStyles;
	end;

function ThFindStyleSheet(inCtrl: TControl): TThStyleSheet;
function ThFindStyleSheetStyle(inCtrl: TControl;
	const inStyleName: string): TThCssStyle;

implementation

uses
	ThVclUtils, ThHtmlPage;

	function ThFindStyleSheet(inCtrl: TControl): TThStyleSheet;
	var
		p: TThHtmlPage;
	begin
		p := ThFindHtmlPage(inCtrl);
		if (p = nil) then
			Result := nil
		else
			Result := p.StyleSheet;
	end;

{	function ThFindStyleSheet(inCtrl: TControl): TThStyleSheet;
	begin
		ThFindElderComponent(Result, inCtrl, TThStyleSheet);
	end;}

	function ThFindStyleSheetStyle(inCtrl: TControl;
			const inStyleName: string): TThCssStyle;
	var
		s: TThStyleSheet;
	begin
		Result := nil;
		if (inStyleName <> '') then
		begin
			s := ThFindStyleSheet(inCtrl);
			if (s <> nil) then
				Result := s.Styles.GetStyleByName(inStyleName);
		end;
		if Result = nil then
			Result := NilStyle;
	end;

{ TThStylesItem }

constructor TThStylesItem.Create(inCollection: TCollection);
begin
	inherited;
	FStyle := TThCssStyle.Create(TComponent(Collection.Owner));
	FStyle.OnChanged := StyleChanged;
	FStyles := TThStyles.Create(Self);
end;

destructor TThStylesItem.Destroy;
begin
	FStyle.Free;
	FStyles.Free;
	inherited;
end;

procedure TThStylesItem.ListClasses(var inClasses: string);
begin
	inClasses := inClasses + ', .' + Name;
	FStyles.ListClasses(inClasses);
end;

procedure TThStylesItem.Emit(inStrings: TStrings);
var
	classes: string;
begin
	classes := Name;
	FStyles.ListClasses(classes);
	inStrings.Add(Format('.%s { %s }', [ classes, Style.InlineAttribute ]));
	FStyles.GenerateStyles(inStrings);
end;

procedure TThStylesItem.StyleChanged(inSender: TObject);
begin
	if (Collection.Owner is TThStylesItem) then
		TThStylesItem(Collection.Owner).UpdateStyle
	else
		UpdateStyle;
end;

procedure TThStylesItem.UpdateStyle;
begin
	FStyles.ParentChanged(Self);
	TThStyles(Collection).StyleChanged;
end;

procedure TThStylesItem.ParentChanged(inStyle: TThStylesItem);
begin
	FStyle.Inherit(inStyle.FStyle);
	FStyles.ParentChanged(Self);
end;

function TThStylesItem.GetDisplayName: string;
begin
	Result := FName;
end;

procedure TThStylesItem.SetName(const Value: string);
begin
	FName := Value;
end;

procedure TThStylesItem.SetStyle(const Value: TThCssStyle);
begin
	FStyle.Assign(Value);
end;

procedure TThStylesItem.SetStyles(const Value: TThStyles);
begin
	FStyles.Assign(Value);
end;

{ TThStyles }

constructor TThStyles.Create(inOwner: TPersistent);
begin
	inherited Create(inOwner, TThStylesItem);
end;

function TThStyles.GetStyleItem(inIndex: Integer): TThStylesItem;
begin
	Result := TThStylesItem(Items[inIndex]);
end;

procedure TThStyles.SetStyleItem(inIndex: Integer;
	const Value: TThStylesItem);
begin
	Items[inIndex] := Value;
end;

function TThStyles.GetStyleItemByName(const inName: string): TThStylesItem;
var
	i: Integer;
begin
	Result := nil;
	for i := 0 to Count - 1 do
		if StyleItem[i].Name = inName then
		begin
			Result := StyleItem[i];
			break;
		end;
end;

function TThStyles.GetStyleByName(const inName: string): TThCssStyle;
var
	item: TThStylesItem;
begin
	item := GetStyleItemByName(inName);
	if item = nil then
		Result := nil
	else
		Result := item.Style;
end;

procedure TThStyles.StyleChanged;
begin
	if Assigned(FOnStyleChange) then
		FOnStyleChange(Self);
end;

procedure TThStyles.SetOnStyleChange(const Value: TNotifyEvent);
begin
	FOnStyleChange := Value;
end;

procedure TThStyles.ParentChanged(inStyle: TThStylesItem);
var
	i: Integer;
begin
	for i := 0 to Count - 1 do
		StyleItem[i].ParentChanged(inStyle);
end;

procedure TThStyles.ListClasses(var inClasses: string);
var
	i: Integer;
begin
	for i := 0 to Count - 1 do
		StyleItem[i].ListClasses(inClasses);
end;

procedure TThStyles.GenerateStyles(inStrings: TStrings);
var
	i: Integer;
begin
	for i := 0 to Count - 1 do
		StyleItem[i].Emit(inStrings);
end;

{ TThStyleSheet }

constructor TThStyleSheet.Create(AOwner: TComponent);
begin
	inherited;
	FStyles := TThStyles.Create(Self);
	FStyles.OnStyleChange := StylesStyleChange;
	AutoAttach;
end;

destructor TThStyleSheet.Destroy;
begin
	FStyles.Free;
	inherited;
end;

procedure TThStyleSheet.AutoAttach;
var
	p: TThHtmlPage;
begin
	p := TThHtmlPage(ThFindComponentByClass(Owner, TThHtmlPage));
	if (p <> nil) and (p.StyleSheet = nil) then
		p.StyleSheet := Self;
end;

procedure TThStyleSheet.Publish(inHtml: TThStructuredHtml);
begin
	Styles.GenerateStyles(inHtml.Styles);
end;

procedure TThStyleSheet.SaveToFile(const inFilename: string);
var
	s: TStringList;
begin
	s := TStringList.Create;
	try
		Styles.GenerateStyles(s);
		s.SaveToFile(inFilename);
	finally
		s.Free;
	end;
end;

procedure TThStyleSheet.SetStyles(const Value: TThStyles);
begin
	FStyles.Assign(Value);
end;

procedure TThStyleSheet.StylesStyleChange(inSender: TObject);
begin
	if (Owner <> nil) and (Owner is TWinControl) then
		ThNotifyAll(TWinControl(Owner), THM_STYLECHANGE);
end;

end.

