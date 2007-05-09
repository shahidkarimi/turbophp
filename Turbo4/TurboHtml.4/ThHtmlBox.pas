unit ThHtmlBox;

interface

uses
	Windows, SysUtils, Types, Classes, Controls, Graphics, ExtCtrls,
	ThInterfaces, ThTextPaint, ThWebControl, ThAttributeList,
	ThCssStyle, ThStyleList, ThTag;

type
	TThHtmlBox = class(TThWebControl)
	private
		FText: TStringList;
	protected
		procedure SetText(const Value: TStringList);
	protected
		procedure TextChange(inSender: TObject);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	published
		property Align;
		property Text: TStringList read FText write SetText;
		property Visible;
	end;

implementation

uses
	ThStyleSheet, ThHtmlPage;

{ TThHtmlBox }

constructor TThHtmlBox.Create(AOwner: TComponent);
begin
	inherited;
	FText := TStringList.Create;
	FText.OnChange := TextChange;
end;

destructor TThHtmlBox.Destroy;
begin
	FText.Free;
	inherited;
end;

procedure TThHtmlBox.SetText(const Value: TStringList);
begin
	FText.Assign(Value);
end;

procedure TThHtmlBox.TextChange(inSender: TObject);
begin
	Invalidate;
end;

end.
