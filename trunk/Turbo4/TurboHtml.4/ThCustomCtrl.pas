unit ThCustomCtrl;

interface

uses
	Windows, Classes, Controls, Messages;

{$R-}

type
	//:$ Ancestor class for all TurboHtml Windowed controls, base class for
	//:$ TThStyledCustomControl.
	//:: TThCustomControl adds auto-sizing and text handling machinery to
	//:: TCustomControl.
	TThCustomControl = class(TCustomControl)
	private
		{$ifdef __ThClx__}
		FCaption: string;
		{$endif}
		FResizing: Boolean;
		FTransparent: Boolean;
    FOpaque: Boolean;
	protected
		// Protected not private so subclasses can set a default value
		FAutoSize: Boolean;
		//:$ Property setter for AutoSize.
		//:: TurboHtml defines a custom AutoSize property so that it will be
		//:: available for both VCL and CLX classes.
		procedure SetCustomAutoSize(const Value: Boolean); virtual;
		procedure SetOpaque(const Value: Boolean);
		{$ifdef __ThClx__}
		function GetText: TCaption; override;
		procedure SetText(const inText: TCaption); override;
		{$endif}
		procedure SetTransparent(const Value: Boolean);
	protected
		procedure AdjustSize; override;
		procedure Paint; override;
		//:$ Adjusts the controls size based on it's contents.
		//:: If AutoSize is true, then the PerformAutoSize procedure is called when
		//:: the object should size itself. Descendant classes should override
		//:: PerformAutoSize in order to implement AutoSizing.
		procedure PerformAutoSize; virtual;
		procedure Resize; override;
		function ShouldAutoSize: Boolean; virtual;
		{$ifdef __ThClx__}
		procedure TextChanged; override;
		{$else}
		procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
		//:$ Hook for the control to take action if it's text is changed (VCL).
		//:: TextChanged calls AdjustSize and Invalidate. Descendant classes can
		//:: override TextChanged for different behavior.
		procedure TextChanged; virtual;
		{$endif}
		procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
		procedure WMMove(var Message: TWMMove); message WM_MOVE;
	protected
		//:$ Specifies whether the control should size itself based on it's contents.
		//:: If AutoSize is true, then the PerformAutoSize procedure is called when
		//:: the object should size itself. Descendant classes should override
		//:: PerformAutoSize.
		property AutoSize: Boolean read FAutoSize write SetCustomAutoSize;
		property Opaque: Boolean read FOpaque write SetOpaque;
		property Transparent: Boolean read FTransparent write SetTransparent;
	end;

implementation

{ TThCustomControl }

procedure TThCustomControl.Paint;
var
	saveIndex: Integer;
	canvasHandle: Integer;
begin
	if Transparent then
	begin
		{ This code will re-paint the parent panel }
		{ This way, you may have 1 trans panel on another }
		saveIndex := SaveDC(Canvas.Handle);
		canvasHandle := Integer(Canvas.Handle);
		MoveWindowOrg(canvasHandle, -Left, -Top);
		Parent.Perform(WM_PAINT, canvasHandle, 0);
		RestoreDC(Canvas.Handle, saveIndex);
	end;
end;

procedure TThCustomControl.WMEraseBkGnd(var Msg : TWMEraseBkGnd);
begin
	if Transparent or Opaque then
		Msg.Result := 0
	else
		inherited;
end;

procedure TThCustomControl.WMMove(Var Message:TWMMove);
begin
	if Transparent then
		Invalidate
	else
		inherited;
end;

{$ifdef __ThClx__}

function TThCustomControl.GetText: TCaption;
begin
	Result := FCaption;
end;

procedure TThCustomControl.SetText(const inText: TCaption);
begin
	if (FCaption <> inText) then
	begin
		FCaption := inText;
		TextChanged;
	end;
end;

{$else}

procedure TThCustomControl.CMTextChanged(var Message: TMessage);
begin
	inherited;
	TextChanged;
end;

{$endif}

procedure TThCustomControl.SetCustomAutoSize(const Value: Boolean);
begin
	FAutoSize := Value;
	if FAutoSize then
		AdjustSize
	else if (Parent <> nil) then
		Parent.Realign;
end;

procedure TThCustomControl.Resize;
begin
	inherited;
	if not FResizing then
	try
		FResizing := true;
		AdjustSize;
	finally
		FResizing := false;
	end;
end;

procedure TThCustomControl.TextChanged;
begin
	AdjustSize;
	Invalidate;
end;

function TThCustomControl.ShouldAutoSize: Boolean;
begin
	Result := AutoSize;
end;

procedure TThCustomControl.AdjustSize;
begin
	if ShouldAutoSize
		and not (csLoading in ComponentState)
		and not (csDestroying in ComponentState) then
			PerformAutoSize
		else
			inherited;
end;

procedure TThCustomControl.PerformAutoSize;
begin
	//
end;

procedure TThCustomControl.SetTransparent(const Value: Boolean);
begin
	FTransparent := Value;
	Invalidate;
end;

procedure TThCustomControl.SetOpaque(const Value: Boolean);
begin
	FOpaque := Value;
	Invalidate;
end;

end.
