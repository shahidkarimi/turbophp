unit htSelect;

interface

uses
	SysUtils, Classes, Controls, Graphics, StdCtrls,
	LrGraphics,
	htControls, htMarkup;

type
	ThtSelect = class(ThtCustomControl)
	private
		FComboBox: TComboBox;
	protected
		procedure CreateComboBox;
		//procedure BuildStyle; override;
		procedure Generate(const inContainer: string;
			inMarkup: ThtMarkup); override;
		procedure PerformAutoSize; override;
		//procedure StyleControl; override;
		//procedure StylePaint; override;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
	published
		property Align;
		property AutoSize default true;
		property Outline;
		property Style;
	end;

implementation

{ ThtSelect }

constructor ThtSelect.Create(inOwner: TComponent);
begin
	inherited;
	CreateComboBox;
	AutoSize := true;
end;

destructor ThtSelect.Destroy;
begin

	inherited;
end;

procedure ThtSelect.CreateComboBox;
begin
	//FreeAndNil(FListBox);
	FComboBox := TComboBox.Create(Self);
	with FComboBox do
	begin
		Parent := Self;
		Align := alClient;
		Enabled := false;
		Items.Add('Test');
	end;
end;

procedure ThtSelect.Generate(const inContainer: string;
	inMarkup: ThtMarkup);
var
	n, s, t: string;
begin
	n := Name;
	s := Style.InlineAttribute;
	if (s <> '') then
		inMarkup.Styles.Add(Format('#%s { %s }', [ n, s ]));
	inMarkup.Add(
		Format('<select id="%s" name="%s" type="%s" %s>',
			[ n, n, t, ExtraAttributes ]));
	inMarkup.Add('<option>Test</option>');
	inMarkup.Add('</select>');
end;

procedure ThtSelect.PerformAutoSize;
begin
	inherited;
	//Width := FComboBox.Width + CtrlStyle.GetBoxWidthMargin;
	Height := FComboBox.Height + CtrlStyle.GetBoxHeightMargin;
end;

end.
