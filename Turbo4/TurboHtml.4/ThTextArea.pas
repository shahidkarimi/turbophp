unit ThTextArea;

interface

uses
	SysUtils, Windows, Types, Classes, Controls, Graphics, StdCtrls, Forms,
	ThInterfaces, ThTextPaint, ThWebControl, ThTag;

type
	TThCustomTextArea = class(TThWebControl, IThFormInput)
	private
		FMemo: TMemo;
    FMargin: Integer;
    procedure SetMargin(const Value: Integer);
	protected
		function GetLines: TStrings;
		function GetWordWrap: Boolean;
		procedure SetLines(const Value: TStrings);
		procedure SetWordWrap(const Value: Boolean);
	protected
		procedure AdjustClientRect(var inRect: TRect); override;
		procedure CreateMemo; virtual;
		procedure CssStyleChanged; override;
		procedure LinesChange(inSender: TObject);
		procedure Tag(inTag: TThTag); override;
	protected
		property Lines: TStrings read GetLines write SetLines;
		property Memo: TMemo read FMemo write FMemo;
		property Margin:	Integer read FMargin write SetMargin;
		property WordWrap: Boolean read GetWordWrap write SetWordWrap;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	end;
	//
	TThTextArea = class(TThCustomTextArea)
	published
		property Align;
		property Lines;
		property Margin;
		property Style;
		property StyleClass;
		property Visible;
		property WordWrap;
	end;

implementation

{ TThCustomTextArea }

constructor TThCustomTextArea.Create(AOwner: TComponent);
begin
	inherited;
	CreateMemo;
	CtrlStyle.Font.DefaultFontFamily := 'Courier New';
	CtrlStyle.Font.DefaultFontPx := 13;
	Style.Color := clWhite;
	FMargin := 3;
end;

destructor TThCustomTextArea.Destroy;
begin
	FMemo.Free;
	inherited;
end;

procedure TThCustomTextArea.CreateMemo;
begin
	Memo := TMemo.Create(Self);
	with Memo do
	begin
		Parent := Self;
		Align := alClient;
		ReadOnly := true;
		Scrollbars := ssBoth;
		ParentColor := true;
	end;
end;

procedure TThCustomTextArea.AdjustClientRect(var inRect: TRect);
begin
	inherited;
	with inRect do
		inRect := Rect(Left + 2, Top + 1, Right - 6, Bottom);
	if CtrlStyle.Padding.PadBottom >= FMargin then
		Inc(inRect.Bottom, FMargin);
end;

procedure TThCustomTextArea.CssStyleChanged;
begin
	inherited;
	if CtrlStyle.Border.BorderVisible then
		Memo.BorderStyle := bsNone
	else
		Memo.BorderStyle := bsSingle;
end;

procedure TThCustomTextArea.LinesChange(inSender: TObject);
begin
	Invalidate;
end;

function TThCustomTextArea.GetLines: TStrings;
begin
	Result := Memo.Lines;
end;

procedure TThCustomTextArea.SetLines(const Value: TStrings);
begin
	Memo.Lines.Assign(Value);
end;

function TThCustomTextArea.GetWordWrap: Boolean;
begin
	Result := Memo.WordWrap;
end;

procedure TThCustomTextArea.SetWordWrap(const Value: Boolean);
begin
	Memo.WordWrap := Value;
	if Value then
		Memo.ScrollBars := ssVertical
	else
		Memo.ScrollBars := ssBoth;
	Invalidate;
end;

procedure TThCustomTextArea.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Element := 'textarea';
		Content := Lines.Text;
		if WordWrap then
			Add('wrap', 'soft')
		else
			Add('wrap', 'off');
		AddStyle('width', '100%');
		//AddStyle('width', Width, 'px');
		AddStyle('height', Height, 'px');
	end;
end;

procedure TThCustomTextArea.SetMargin(const Value: Integer);
begin
	FMargin := Value;
end;

end.
