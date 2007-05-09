unit ThGraphicCtrl;

interface

uses
	Classes, Controls, Messages;

type
	//:$ Ancestor class for all TurboHtml graphic controls, base class for
	//:$ TThStyledGraphicControl.
	//:: TThGraphicControl adds auto-sizing and text handling machinery to
	//:: TGraphicControl.
	TThGraphicControl = class(TGraphicControl)
	protected
		FAutoSize: Boolean;
		//:$ Property setter for AutoSize.
		//:: TurboHtml defines a custom AutoSize property so that it will be
		//:: available for both VCL and CLX classes.
		procedure SetCustomAutoSize(const Value: Boolean); virtual;
		//:$ Specifies whether the control should size itself based on it's contents.
		//:: If AutoSize is true, then the PerformAutoSize procedure is called when
		//:: the object should size itself. Descendant classes should override
		//:: PerformAutoSize.
		property AutoSize: Boolean read FAutoSize write SetCustomAutoSize;
	protected
		Resizing: Boolean;
		{$ifdef __ThClx__}
		FCaption: string;
		function GetText: TCaption; override;
		procedure SetText(const inText: TCaption); override;
		//:$ Hook for the control to take action if it's text is changed (CLX).
		//:: TextChanged calls AdjustSize and Invalidate. Descendant classes can
		//:: override TextChanged for different behavior.
		procedure TextChanged; override;
		{$else}
		procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
		//:$ Hook for the control to take action if it's text is changed (VCL).
		//:: TextChanged calls AdjustSize and Invalidate. Descendant classes can
		//:: override TextChanged for different behavior.
		procedure TextChanged; virtual;
		{$endif}
		function ShouldAutoSize: Boolean; virtual;
		procedure Resize; override;
		procedure AdjustSize; override;
		//:$ Adjusts the controls size based on it's contents.
		//:: If AutoSize is true, then the PerformAutoSize procedure is called when
		//:: the object should size itself. Descendant classes should override
		//:: PerformAutoSize in order to implement AutoSizing.
		procedure PerformAutoSize; virtual;
	end;

implementation

{ TThGraphicControl }

{$ifdef __ThClx__}

function TThGraphicControl.GetText: TCaption;
begin
	Result := FCaption;
end;

procedure TThGraphicControl.SetText(const inText: TCaption);
begin
	if (FCaption <> inText) then
	begin
		FCaption := inText;
		TextChanged;
	end;
end;

{$else}

procedure TThGraphicControl.CMTextChanged(var Message: TMessage);
begin
	inherited;
	TextChanged;
end;

{$endif}

procedure TThGraphicControl.SetCustomAutoSize(const Value: Boolean);
begin
	FAutoSize := Value;
	if FAutoSize then
		AdjustSize
	else if (Parent <> nil) then
		Parent.Realign;
end;

procedure TThGraphicControl.Resize;
begin
	inherited;
	if not Resizing then
	try
		Resizing := true;
		AdjustSize;
	finally
		Resizing := false;
	end;
end;

procedure TThGraphicControl.TextChanged;
begin
	AdjustSize;
	Invalidate;
end;

function TThGraphicControl.ShouldAutoSize: Boolean;
begin
	Result := AutoSize;
end;

procedure TThGraphicControl.AdjustSize;
begin
	if ShouldAutoSize
		and not (csLoading in ComponentState)
		and not (csDestroying in ComponentState) then
		begin
			if Parent <> nil then
				PerformAutoSize
		end else
			inherited;
end;

procedure TThGraphicControl.PerformAutoSize;
begin
	//
end;

end.
