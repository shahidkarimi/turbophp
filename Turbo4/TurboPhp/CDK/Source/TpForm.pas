unit TpForm;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThTag, ThForm,
	TpControls;

type
	TTpForm = class(TThForm)
	private
		FOnGenerate: TTpEvent;
		FOnSubmit: TTpEvent;
	protected
		function GetInputsHtml: string; override;
	protected
		procedure Tag(inTag: TThTag); override;
	published
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
		property OnSubmit: TTpEvent read FOnSubmit write FOnSubmit;
	end;

implementation

{ TTpForm }

function TTpForm.GetInputsHtml: string;
const
	SHiddenInput =
	 '<input type="hidden" name="%s" value="%s" tpName="%0:s" tpClass="TTpInput" />';
var
	i: Integer;
begin
	//Result := Format('<input type="hidden" name="%s" value="%s" />',
	//	[ Name + '_s', 'submit' ]);
	Result := Format(SHiddenInput, [ 'tpsubmit', Name ]);
	for i := 0 to Pred(Inputs.Count) do
		with Inputs[i] do
			Result := Result + Format(SHiddenInput, [ WebName, WebValue ]);
end;

procedure TTpForm.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpForm');
		Add('tpName', Name);
		if DefaultButton <> nil then
			Add('tpDefaultButton', DefaultButton.Name);
		Add('tpOnGenerate', OnGenerate);
		Add('tpOnSubmit', OnSubmit);
	end;
end;

end.
