unit ThPageControl;

interface

uses
	Windows, Messages, SysUtils, Types, Classes, Controls, StdCtrls, ExtCtrls,
	Graphics, Forms,
	ThWebControl, ThAttributeList, ThTag, ThPanel;

type
	TThSheet = class(TThPanel)
	public
		destructor Destroy; override;
		procedure Select;
	end;
	//
	TThPageControl = class(TThPanel)
	private
		FActivePageIndex: Integer;
		FCount: Integer;
		FSheets: TList;
	protected
		procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
		function GetHtmlAsString: string; override;
		function GetSheet(inIndex: Integer): TThSheet;
		function GetSheetNamePrefix: string; virtual;
		procedure SetActivePageIndex(const Value: Integer);
		procedure SetCount(const Value: Integer);
	protected
		procedure AdjustClientRect(var Rect: TRect); override;
		function CreateSheet: TThSheet; virtual;
		function IsGoodIndex: Boolean;
		procedure Loaded; override;
		procedure RenameSheets; virtual;
		procedure Resize; override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure RemoveSheet(inSheet: TThSheet);
		property Sheet[inIndex: Integer]: TThSheet read GetSheet;
	published
		property ActivePageIndex: Integer read FActivePageIndex
			write SetActivePageIndex;
		property Count: Integer read FCount write SetCount;
	end;

implementation

{ TThSheet }

destructor TThSheet.Destroy;
begin
	if (Parent is TThPageControl) then
		TThPageControl(Parent).RemoveSheet(Self);
	inherited;
end;

procedure TThSheet.Select;
begin
	Top := 0;
end;

{ TThPageControl }

constructor TThPageControl.Create(AOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle - [ csAcceptsControls ];
	FSheets := TList.Create;
	//Style.Color := clLime;
end;

destructor TThPageControl.Destroy;
begin
	FSheets.Destroy;
	inherited;
end;

procedure TThPageControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
	i: Integer;
begin
	for i := 0 to Pred(FSheets.Count) do
		Proc(TComponent(FSheets[i]));
end;

procedure TThPageControl.Loaded;
var
	i: Integer;
begin
	inherited;
	for i := 0 to Pred(ControlCount) do
		if Controls[i] is TThSheet then
		begin
			FSheets.Add(Controls[i]);
			Controls[i].Align := alTop;
		end;
	if Count = 0 then
		Count := 1;
end;

procedure TThPageControl.Resize;
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		Sheet[i].Height := Height - 4;
	inherited;
end;

procedure TThPageControl.AdjustClientRect(var Rect: TRect);
begin
	//Dec(Rect.Bottom, 4);
	Inc(Rect.Top, 4);
end;

function TThPageControl.IsGoodIndex: Boolean;
begin
	Result := (ActivePageIndex >= 0) and (ActivePageIndex < Count);
end;

function TThPageControl.GetSheet(inIndex: Integer): TThSheet;
begin
	Result := TThSheet(FSheets[inIndex]);
end;

procedure TThPageControl.SetActivePageIndex(const Value: Integer);
begin
	FActivePageIndex := Value;
	if IsGoodIndex then
		Sheet[ActivePageIndex].Select;
end;

function TThPageControl.CreateSheet: TThSheet;
begin
	Result := TThSheet.Create(Owner);
	Result.Name := TCustomForm(Owner).Designer.UniqueName('ThPage');
	Result.Align := alTop;
	Result.Style.Color := clWhite;
	Result.Parent := Self;
end;

procedure TThPageControl.SetCount(const Value: Integer);
begin
	FCount := Value;
	if not (csLoading in ComponentState) then
	begin
		while FSheets.Count < FCount do
			FSheets.Add(CreateSheet);
		ActivePageIndex := Pred(FCount);
	end;
end;

procedure TThPageControl.RemoveSheet(inSheet: TThSheet);
begin
	FSheets.Remove(inSheet);
	FCount := FSheets.Count;
end;

function TThPageControl.GetSheetNamePrefix: string;
begin
	Result := Name + 'Sheet_';
end;

procedure TThPageControl.RenameSheets;
var
	i: Integer;
begin
	for i := 0 to Pred(Count) do
		Sheet[i].Name := Format('%s%d', [ GetSheetNamePrefix, i + 1 ]);
end;

function TThPageControl.GetHtmlAsString: string;
begin
	RenameSheets;
	Result := inherited GetHtmlAsString;
end;

initialization
	RegisterClass(TThSheet);
end.
