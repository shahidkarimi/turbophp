unit ThForm;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types,
	ThWebControl, ThTag, ThPanel, ThWebDataDictionary;

type
	TThFormMethod = ( fmPost, fmGet );
	TThForm = class(TThCustomPanel)
	private
		FAction: string;
		FMethod: TThFormMethod;
		FInputs: TThWebDataDictionary;
		FDefaultButton: TControl;
	protected
		function GetInputs: TThDictionaryData;
		function GetInputsHtml: string; virtual;
		function GetOutlineColor: TColor; override;
		procedure SetDefaultButton(const Value: TControl); virtual;
		procedure SetInputs(const Value: TThDictionaryData);
	protected
		function IsButton(const Value: TControl): Boolean;
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
		procedure Tag(inTag: TThTag); override;
	public
		constructor Create(inOwner: TComponent); override;
	published
		property Action: string read FAction write FAction;
		property Align;
		property DefaultButton: TControl read FDefaultButton
			write SetDefaultButton;
		property DesignOpaque;
		property Inputs: TThDictionaryData read GetInputs write SetInputs;
		property Method: TThFormMethod read FMethod write FMethod;
		property ShowGrid;
		property ShowOutline default true;
		property Style;
		property StyleClass;
		property Visible;
	end;

const
	htMethod: array[TThFormMethod] of string = ( 'post', 'get' );

implementation

uses
	ThButton, ThImageButton;

const
	cFormSpace = 32;

{ TThForm }

constructor TThForm.Create(inOwner: TComponent);
begin
	inherited;
	ControlStyle := ControlStyle + [ csAcceptsControls ];
	FInputs := TThWebDataDictionary.Create(Self);
	ShowOutline := true;
end;

function TThForm.GetOutlineColor: TColor;
begin
	Result := clGreen;
end;

function TThForm.GetInputsHtml: string;
const
	SHiddenInput = '<input type="hidden" name="%s" value="%s"/>';
var
	i: Integer;
begin
	Result := '';
	for i := 0 to Pred(Inputs.Count) do
		with Inputs[i] do
			Result := Result + Format(SHiddenInput, [ WebName, WebValue ]);
end;

procedure TThForm.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Content := GetInputsHtml + Html;
		//
		Element := 'form';
		//
		Attributes.Clear;
		Add('name', Name);
		Add('method', htMethod[Method]);
		// works even if Action=''
		Attributes.Add(Format('action="%s"', [ Action ]));
		//
		Styles.Clear;
		AddStyle('margin', 0);
	end;
	StylizeTag(inTag);
	ListJsAttributes(inTag.Attributes);
end;

procedure TThForm.SetInputs(const Value: TThDictionaryData);
begin
	FInputs.Data.Assign(Value);
end;

function TThForm.GetInputs: TThDictionaryData;
begin
	Result := FInputs.Data;
end;

procedure TThForm.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	if (Operation = opRemove) and (AComponent = DefaultButton) then
		FDefaultButton := nil;
end;

function TThForm.IsButton(const Value: TControl): Boolean;
begin
	Result := (Value <> nil) and
		((Value is TThButton) or (Value is TThImageButton));
end;

procedure TThForm.SetDefaultButton(const Value: TControl);
begin
	if IsButton(Value) then
	begin
		if FDefaultButton <> nil then
			FDefaultButton.RemoveFreeNotification(Self);
		FDefaultButton := Value;
		if FDefaultButton <> nil then
			FDefaultButton.FreeNotification(Self);
	end;
end;

end.
