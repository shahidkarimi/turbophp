unit ThSelect;

interface

uses
	SysUtils, Windows, Types, Classes, Controls, Graphics, StdCtrls,
	ThInterfaces, ThTag, ThHeaderComponent, ThWebControl, ThTextPaint,
	ThListSource;

type
	TThCustomSelect = class(TThWebControl, IThFormInput)
	private
		FListBox: TListBox;
		FComboBox: TComboBox;
		FRows: Integer;
		FItems: TStringList;
		FItemIndex: Integer;
	protected
		FSource: IThListSource;
		function GetOptionsHtml: string;
		function GetSource: IThListSource; virtual;
		procedure SetItemIndex(const Value: Integer);
		procedure SetItems(const Value: TStringList);
		procedure SetRows(const Value: Integer); virtual;
		procedure SetSource(const Value: IThListSource);
	protected
		procedure AddSourceNotifier;
		procedure CreateComboBox;
		procedure CreateListBox;
		procedure ItemsChanged(inSender: TObject = nil);
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
		procedure PerformAutoSize; override;
		procedure RemoveSourceNotifier;
		procedure SourceChanged(inSender: TObject);
		procedure SourceRemoved; virtual;
		procedure Tag(inTag: TThTag); override;
		function ValidatedItemIndex(inList: TStrings): Integer;
	protected
		property ItemIndex: Integer read FItemIndex write SetItemIndex;
		property Items: TStringList read FItems write SetItems;
		property Rows: Integer read FRows write SetRows;
		property Source: IThListSource read GetSource write SetSource;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure UpdateItems;
		//function GetCellAttributes: string; override;
	end;
	//
	TThSelect = class(TThCustomSelect)
	public
		constructor Create(AOwner: TComponent); override;
	published
		property Align;
		property AutoSize default true;
		property ItemIndex;
		property Items;
		property Source;
		property Style;
		property StyleClass;
		property Visible;
	end;
	//
	TThListBox = class(TThCustomSelect)
	public
		constructor Create(AOwner: TComponent); override;
	published
		property Align;
		property AutoSize default true;
		property ItemIndex;
		property Items;
		property Rows;
		property Source;
		property Style;
		property StyleClass;
		property Visible;
	end;

implementation

{ TThCustomSelect }

constructor TThCustomSelect.Create(AOwner: TComponent);
begin
	inherited;
	FAutoSize := true;
	FItems := TStringList.Create;
	FItems.OnChange := ItemsChanged;
	CtrlStyle.Font.DefaultFontFamily := 'MS Sans Serif';
	CtrlStyle.Font.DefaultFontPx := 14;
	BuildStyle;
end;

destructor TThCustomSelect.Destroy;
begin
	RemoveSourceNotifier;
	FComboBox.Free;
	FListBox.Free;
	FItems.Free;
	inherited;
end;

procedure TThCustomSelect.RemoveSourceNotifier;
begin
	if (Source <> nil) then
		Source.Notifiers.Remove(SourceChanged);
end;

procedure TThCustomSelect.AddSourceNotifier;
begin
	if (Source <> nil) then
		Source.Notifiers.Add(SourceChanged);
end;

procedure TThCustomSelect.PerformAutoSize;
const
	Margin = 8;
begin
	if FComboBox <> nil then
		Height := FComboBox.Height + CtrlStyle.GetBoxHeightMargin
	else if FListBox <> nil then
		Height := FListBox.ItemHeight * Rows + Margin
			+ CtrlStyle.GetBoxHeightMargin;
end;

procedure TThCustomSelect.CreateComboBox;
begin
	FreeAndNil(FListBox);
	FComboBox := TComboBox.Create(Self);
	with FComboBox do
	begin
		Parent := Self;
		Align := alClient;
	end;
end;

procedure TThCustomSelect.CreateListBox;
begin
	FreeAndNil(FComboBox);
	FListBox := TListBox.Create(Self);
	with FListBox do
	begin
		Parent := Self;
		Align := alClient;
	end;
end;

function TThCustomSelect.ValidatedItemIndex(inList: TStrings): Integer;
begin
	if ItemIndex < inList.Count then
		Result := ItemIndex
	else if inList.Count = 0 then
		Result := -1
	else
		Result := 0;
end;

procedure TThCustomSelect.ItemsChanged(inSender: TObject);
begin
	if FListBox <> nil then
	begin
		FListBox.Items.Assign(Items);
		FListBox.ItemIndex := ValidatedItemIndex(FListBox.Items);
	end
	else if FComboBox <> nil then
	begin
		FComboBox.Items.Assign(Items);
		FComboBox.ItemIndex := ValidatedItemIndex(FComboBox.Items);
	end;
end;

procedure TThCustomSelect.SetItems(const Value: TStringList);
begin
	FItems.Assign(Value);
	ItemsChanged;
end;

procedure TThCustomSelect.SetRows(const Value: Integer);
begin
{
	if (FRows = 0) and (Value > 0) then
		CreateListBox
	else if (FRows > 0) and (Value = 0) then
		CreateComboBox;
}
	if Value > 0 then
	begin
		FRows := Value;
		ItemsChanged;
		AdjustSize;
	end;
end;

function TThCustomSelect.GetOptionsHtml: string;
var
	i: Integer;
	s: string;
begin
	Result := '';
	for i := 0 to Pred(Items.Count) do
	begin
		if ItemIndex = i then
			s := ' selected="selected"'
		else
			s := '';
		Result := Result + Format('<option%s>%s</option>', [ s, Items[i] ]);
	end;
end;

procedure TThCustomSelect.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Element := 'select';
		Content := GetOptionsHtml;
		if Rows > 0 then
			Add('size', Rows);
		AddStyle('width', '100%');
		AddStyle('height', '100%');
		//CtrlStyle.Font.ListStyles(Styles);
		//AddStyle('width', Width, 'px');
		//AddStyle('height', Height, 'px');
		//AddStyle('height', AdjustedClientHeight, 'px');
	end;
end;

procedure TThCustomSelect.SetItemIndex(const Value: Integer);
begin
	FItemIndex := Value;
	ItemsChanged;
end;

procedure TThCustomSelect.UpdateItems;
begin
	if Source <> nil then
		Items.Assign(Source.Items)
	else
		Items.Clear;
end;

procedure TThCustomSelect.SetSource(const Value: IThListSource);
begin
	if Source <> Value then
	begin
		if (Source <> nil) then
		begin
			ReferenceInterface(FSource, opRemove);
			//Source.RemoveFreeNotification(Self);
			RemoveSourceNotifier;
		end;
		FSource := Value;
		if (Source <> nil) then
		begin
			ReferenceInterface(FSource, opInsert);
			//Source.FreeNotification(Self);
			AddSourceNotifier;
			UpdateItems;
		end;
	end;
end;

procedure TThCustomSelect.SourceRemoved;
begin
	FSource := nil;
	UpdateItems;
end;

procedure TThCustomSelect.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
//	if (Operation = opRemove) and (AComponent = FSource) then
//		FSource := nil;
	if (Operation = opRemove) and Assigned(Source)
		and AComponent.IsImplementorOf(Source) then
			SourceRemoved;
end;

procedure TThCustomSelect.SourceChanged(inSender: TObject);
begin
	UpdateItems;
end;

function TThCustomSelect.GetSource: IThListSource;
begin
	Result := FSource;
end;

{ TThSelect }

constructor TThSelect.Create(AOwner: TComponent);
begin
	inherited;
	CreateComboBox;
end;

{ TThListBox }

constructor TThListBox.Create(AOwner: TComponent);
begin
	inherited;
	FRows := 8;
	CreateListBox;
end;

end.
