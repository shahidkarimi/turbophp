unit htAjaxPanel;

interface

uses
	Windows, SysUtils, Types, Classes, Controls, Graphics,
	htInterfaces, htTag, htMarkup, htControls, htPanel;

type
	IhtAjaxControl = interface
		['{F5E67CB7-D1B1-44A7-A709-37F66146B2D2}']
		procedure GenerateTag(inTag: ThtTag; inMarkup: ThtMarkup);
	end;
	//
	ThtAjaxPanel = class(ThtCustomPanel, IhtAjaxControl)
	public
		procedure Generate(const inContainer: string;
			inMarkup: ThtMarkup); override;
		procedure GenerateTag(inTag: ThtTag; inMarkup: ThtMarkup);
	published
		property Align;
		property Outline;
		property Style;
		property Transparent;
		property Visible;
	end;

function AlignToAjaxAlign(inAlign: TAlign): string;
procedure AjaxGenerateChildren(inOwner: TWinControl; inParent: ThtTag;
	inMarkup: ThtMarkup);

implementation

uses
	LrVclUtils, LrControlIterator, htPaint;

	function AlignToAjaxAlign(inAlign: TAlign): string;
	const
		cAjaxAligns: array[TAlign] of string = ( '', 'top', 'bottom', 'left',
			'right', 'client', 'custom' );
	begin
		Result := cAjaxAligns[inAlign];
	end;

	procedure AjaxGenerateCtrlTag(inParent: ThtTag;
		inCtrl: TControl; inMarkup: ThtMarkup);
	var
		c: IhtAjaxControl;
	begin
		if LrIsAs(inCtrl, IhtAjaxControl, c) then
			c.GenerateTag(inParent.Add, inMarkup);
	end;

	procedure AjaxGenerateChildren(inOwner: TWinControl; inParent: ThtTag;
		inMarkup: ThtMarkup);
	begin
		with TLrCtrlIterator.Create(inOwner) do
		try
			while Next do
				AjaxGenerateCtrlTag(inParent, Ctrl, inMarkup);
		finally
			Free;
		end;
	end;

	procedure AjaxStylize(inControl: TControl; inStyleBase: string;
		inMarkup: ThtMarkup);
	var
		s: string;
	begin
		case  inControl.Align of
			alLeft, alRight: s := Format(' width: %dpx;', [ inControl.ClientWidth ]);
			alTop, alBottom: s := Format(' height: %dpx;', [ inControl.ClientHeight ]);
			else s := '';
		end;
		s := inStyleBase + s;
		if (s <> '') then
			inMarkup.Styles.Add(Format('#%s { %s }', [ inControl.Name, s ]));
	end;

{ ThtAjaxPanel }

procedure ThtAjaxPanel.Generate(const inContainer: string;
	inMarkup: ThtMarkup);
var
	tag: ThtTag;
begin
	tag := ThtTag.Create;
	try
		GenerateTag(tag, inMarkup);
		inMarkup.Add(tag.HTML);
	finally
		tag.Free;
	end;
end;

procedure ThtAjaxPanel.GenerateTag(inTag: ThtTag; inMarkup: ThtMarkup);
var
	s: string;
begin
	inTag.Element := 'div';
	inTag.Attributes['turboAlign'] := AlignToAjaxAlign(Align);
	inTag.Attributes['id'] := Name;
	s := Style.InlineAttribute;
	case Align of
		alLeft, alRight: s := s + Format(' width: %dpx;', [ ClientWidth ]);
		alTop, alBottom: s := s + Format(' height: %dpx;', [ ClientHeight ]);
	end;
	if (s <> '') then
		inMarkup.Styles.Add(Format('#%s { %s }', [ Name, s ]));
	AjaxGenerateChildren(Self, inTag, inMarkup);
end;

end.
