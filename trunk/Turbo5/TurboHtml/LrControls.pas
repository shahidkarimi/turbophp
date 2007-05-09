unit LrControls;

interface

uses
	Windows, Messages, Classes, Controls;

type
	//:$ Enhanced TGraphicControl.
	//:: TLrGraphicControl adds auto-sizing and text handling machinery.
	TLrGraphicControl = class(TGraphicControl)
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
		{$ifdef __LrClx__}
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
		//:: PerformAutoSize in order to implement auto sizing.
		procedure PerformAutoSize; virtual;
	end;
	//
	//:$ Enhanced TCusomControl.
	//:: TLrCustomControl adds machinery for auto-sizing, text handling, and
	//:: transparency.
	TLrCustomControl = class(TCustomControl)
	private
		{$ifdef __LrClx__}
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
		{$ifdef __LrClx__}
		function GetText: TCaption; override;
		procedure SetText(const inText: TCaption); override;
		{$endif}
		procedure SetParent(inParent: TWinControl); override;
		procedure SetTransparent(const Value: Boolean);
	protected
		function ShouldAutoSize: Boolean; virtual;
		procedure AdjustSize; override;
		procedure Paint; override;
		//:$ Adjusts the controls size based on it's contents.
		//:: If AutoSize is true, then the PerformAutoSize procedure is called when
		//:: the object should size itself. Descendant classes should override
		//:: PerformAutoSize in order to implement AutoSizing.
		procedure PerformAutoSize; virtual;
		procedure Resize; override;
		{$ifdef __LrClx__}
		procedure TextChanged; override;
		{$else}
		procedure CMInvalidate(var Message: TMessage); message CM_INVALIDATE;
		procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
		procedure CreateParams(var Params: TCreateParams); override;
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
	public
		procedure Invalidate; override;
	end;

implementation

{ TLrGraphicControl }

{$ifdef __LrClx__}

function TLrGraphicControl.GetText: TCaption;
begin
	Result := FCaption;
end;

procedure TLrGraphicControl.SetText(const inText: TCaption);
begin
	if (FCaption <> inText) then
	begin
		FCaption := inText;
		TextChanged;
	end;
end;

{$else}

procedure TLrGraphicControl.CMTextChanged(var Message: TMessage);
begin
	inherited;
	TextChanged;
end;

{$endif}

procedure TLrGraphicControl.SetCustomAutoSize(const Value: Boolean);
begin
	FAutoSize := Value;
	if FAutoSize then
		AdjustSize
	else if (Parent <> nil) then
		Parent.Realign;
end;

procedure TLrGraphicControl.Resize;
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

procedure TLrGraphicControl.TextChanged;
begin
	AdjustSize;
	Invalidate;
end;

function TLrGraphicControl.ShouldAutoSize: Boolean;
begin
	Result := AutoSize;
end;

procedure TLrGraphicControl.AdjustSize;
begin
	if not ShouldAutoSize	or ([csLoading, csDestroying] * ComponentState <> []) then
		inherited
	else if Parent <> nil then
		PerformAutoSize
end;

procedure TLrGraphicControl.PerformAutoSize;
begin
	//
end;

{ TLrCustomControl }

procedure TLrCustomControl.CreateParams(var Params: TCreateParams);
begin
	inherited CreateParams(Params);
{
	if Transparent then
	begin
		Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
		ControlStyle := ControlStyle - [csOpaque];
	end
	else begin
		Params.ExStyle := Params.ExStyle and not WS_EX_TRANSPARENT;
		ControlStyle := ControlStyle + [csOpaque];
	end;
}
end;

procedure TLrCustomControl.SetTransparent(const Value: Boolean);
begin
	FTransparent := Value;
{
	if not (csCreating in ControlState) then
	begin
		RecreateWnd;
		Invalidate;
	end;
}
end;

procedure TLrCustomControl.SetOpaque(const Value: Boolean);
begin
	FOpaque := Value;
	Invalidate;
end;

procedure TLrCustomControl.Paint;
//var
//	cache: HDC;
begin
{
	if Transparent then
	begin
		if Tag <> 99 then
		try
			Tag := 99;
			Parent.PaintTo(Canvas, -Left, -Top);
		finally
			Tag := 0;
		end;
	end;
}
{
	if Transparent then
	begin
		cache := SaveDC(Canvas.Handle);
		try
			MoveWindowOrg(Canvas.Handle, -Left, -Top);
			Parent.Perform(WM_PAINT, Canvas.Handle, 0);
		finally
			RestoreDC(Canvas.Handle, cache);
		end;
	end;
}
end;

procedure TLrCustomControl.WMEraseBkGnd(var Msg : TWMEraseBkGnd);
begin
	if Transparent or Opaque then
		Msg.Result := 0
	else
		inherited;
end;

procedure TLrCustomControl.WMMove(Var Message:TWMMove);
begin
	if Transparent then
		Invalidate
//	else
//		inherited;
end;

procedure TLrCustomControl.CMInvalidate(var Message: TMessage);
var
	i: Integer;
begin
	inherited;
	if HandleAllocated then
	begin
		if Parent <> nil then
			Parent.Perform(CM_INVALIDATE, 1, 0);
		if Message.WParam = 0 then
		begin
			//InvalidateRect(Handle, nil, not (csOpaque in ControlStyle));
			for i := 0 to ControlCount - 1 do
				if Controls[i] is TLrCustomControl then
					Controls[i].Invalidate;
		end;
	end;
end;

procedure TLrCustomControl.SetParent(inParent: TWinControl);
begin
	inherited;
{
	The trick needed to make it all work!
	I don't know if changing the parent's style is a good idea, but it only
	removes the  WS_CLIPCHILDREN style which shouldn't cause
	any problems.
}
{
	if Parent <> nil then
		SetWindowLong(Parent.Handle, GWL_STYLE,
			 GetWindowLong(Parent.Handle, GWL_STYLE) and not WS_ClipChildren);
}
end;

procedure TLrCustomControl.Invalidate;
//var
//	Rect :TRect;
begin
{
	Rect:= BoundsRect;
	if (Parent <> nil) and Parent.HandleAllocated then
		InvalidateRect(Parent.Handle, @Rect, True)
	else
}
		inherited;
end;

{$ifdef __LrClx__}

function TLrCustomControl.GetText: TCaption;
begin
	Result := FCaption;
end;

procedure TLrCustomControl.SetText(const inText: TCaption);
begin
	if (FCaption <> inText) then
	begin
		FCaption := inText;
		TextChanged;
	end;
end;

{$else}

procedure TLrCustomControl.CMTextChanged(var Message: TMessage);
begin
	inherited;
	TextChanged;
end;

{$endif}

procedure TLrCustomControl.SetCustomAutoSize(const Value: Boolean);
begin
	FAutoSize := Value;
	if FAutoSize then
		AdjustSize
	else if (Parent <> nil) then
		Parent.Realign;
end;

procedure TLrCustomControl.Resize;
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

procedure TLrCustomControl.TextChanged;
begin
	AdjustSize;
	Invalidate;
end;

function TLrCustomControl.ShouldAutoSize: Boolean;
begin
	Result := AutoSize;
end;

procedure TLrCustomControl.AdjustSize;
begin
	if ShouldAutoSize	and ([csLoading, csDestroying] * ComponentState = []) then
		PerformAutoSize
	else
		inherited;
end;

procedure TLrCustomControl.PerformAutoSize;
begin
	//
end;

end.
