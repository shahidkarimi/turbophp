unit htGenerator;

interface

uses
	Windows, SysUtils, Types, Classes, Controls, Graphics,
	htInterfaces, htControls, htMarkup;

type
	ThtGenerateCtrlEvent = procedure(inMarkup: ThtMarkup;
		const inContainer: string; inCtrl: TControl;
		var ioHandled: Boolean) of object;
	//
	ThtGenerator = class
	private
		Container: TWinControl;
		Markup: ThtMarkup;
		FOnGenerateCtrl: ThtGenerateCtrlEvent;
	protected
		procedure GenerateCtrl(const inContainer: string; inCtrl: TControl);
		procedure GenerateColumns;
		procedure GenerateComponents;
		procedure GenerateInnerColumns(inAlign: TAlign);
		procedure GenerateRows(inAlign: TAlign);
		procedure GenerateWinCtrl(inCtrl: TControl);
	public
		procedure Generate(inContainer: TWinControl; inMarkup: ThtMarkup);
		property OnGenerateCtrl: ThtGenerateCtrlEvent read FOnGenerateCtrl
			write FOnGenerateCtrl;
	end;

implementation

uses
	LrComponentIterator, LrControlIterator, LrVclUtils;

{ ThtGenerator }

procedure ThtGenerator.Generate(inContainer: TWinControl;
	inMarkup: ThtMarkup);
begin
	Container := inContainer;
	Markup := inMarkup;
	GenerateRows(alTop);
	GenerateColumns;
	GenerateRows(alBottom);
	GenerateComponents;
end;

	function PrioritySort(inA, inB: Pointer): Integer;
	var
		a, b: IhtComponent;
	begin
		if LrIsAs(inA, IhtComponent, a) and LrIsAs(inB, IhtComponent, b) then
			Result := a.Priority - b.Priority
		else
			Result := 0;
	end;

procedure ThtGenerator.GenerateComponents;
var
	g: IhtGenerator;
begin
	with TLrSortedComponentIterator.Create(Container, PrioritySort) do
	try
		while Next do
			if LrIsAs(Component, IhtGenerator, g) then
				g.Generate(Markup);
	finally
		Free;
	end;
end;

procedure ThtGenerator.GenerateWinCtrl(inCtrl: TControl);
begin
	if TWinControl(inCtrl).ControlCount = 0 then
		Markup.Add(inCtrl.Name)
//		Markup.Add('<span style=" vertical-align:middle;">' + inCtrl.Name + '</span>')
	else
		with ThtGenerator.Create do
		try
			OnGenerateCtrl := Self.OnGenerateCtrl;
			Generate(TWinControl(inCtrl), Self.Markup);
		finally
			Free;
		end
end;

procedure ThtGenerator.GenerateCtrl(const inContainer: string;
	inCtrl: TControl);
var
	handled: Boolean;
	c: IhtControl;
begin
	handled := false;
	if Assigned(OnGenerateCtrl) then
		OnGenerateCtrl(Markup, inContainer, inCtrl, handled);
	if not handled then
		if LrIsAs(inCtrl, IhtControl, c) then
			c.Generate(inContainer, Markup)
		else if (inCtrl is TWinControl) then
			GenerateWinCtrl(inCtrl)
		else
			Markup.Add(inCtrl.Name);
end;

procedure ThtGenerator.GenerateRows(inAlign: TAlign);
var
	j: Integer;
	n: string;
begin
	with TLrSortedCtrlIterator.Create(Container, inAlign) do
	try
		j := 0;
		while Next([inAlign]) do
		begin
			if (Ctrl is TWinControl) then
				GenerateCtrl('', Ctrl)
			else begin
				Inc(j);
				n := Format('%s_Row_%d', [ Container.Name, j ]);
				Markup.Styles.Add(
					//Format('#%s { width:99%%; height:%dpx; }', [ n, htBoxHeight(Ctrl) ]));
					Format('#%s { margin-left: auto; margin-right: auto; height:%dpx; }', [ n, htBoxHeight(Ctrl) ]));
					//Format('#%s { position:relative; width:99%%; height:%dpx; }', [ n, Ctrl.Height ]));
				Markup.Add(Format('<div id="%s">', [ n ]));
				GenerateCtrl(n, Ctrl);
				Markup.Add('</div>');
			end;
		end;
	finally
		Free;
	end;
end;

procedure ThtGenerator.GenerateColumns;
var
	h: Integer;
begin
	with TLrCtrlIterator.Create(Container) do
	try
		h := AlignMaxHeight([alLeft, alClient, alRight]);
	finally
		Free;
	end;
	if h > 0 then
	begin
		Markup.Styles.Add(
			Format(
			//'#%sColumns { height:%dpx; table-layout: fixed; background-color: #FAFAD2; }',
				'#%sColumns { height:%dpx; table-layout: fixed; ' +
				//'border: 1px dashed silver; ' +
				'}',
					[ Container.Name, h ])
			);
		Markup.Add(
			Format(
				//'<table id="%sColumns" cellpadding="0" cellspacing="0" border="1">',
				'<table id="%sColumns" cellpadding="0" cellspacing="0" border="0">',
					[ Container.Name ])
			);
		Markup.Add('  <tr>');
		GenerateInnerColumns(alLeft);
		GenerateInnerColumns(alClient);
		GenerateInnerColumns(alRight);
		Markup.Add('  </tr>');
		Markup.Add('</table>');
	end;
end;

procedure ThtGenerator.GenerateInnerColumns(inAlign: TAlign);
var
	n, s: string;
begin
	with TLrSortedCtrlIterator.Create(Container, inAlign) do
	try
		while Next([inAlign]) do
		begin
			n := Format('%s_Cell_%d', [ Container.Name, Index]);
			//
			if inAlign = alClient then
				s := ''
			else
				s := Format(' width:%dpx;', [ Ctrl.Width ]);
			//
			//s := s + ' border: 1px dashed silver; ';
			//
			if (s <> '') then
				Markup.Styles.Add(Format('#%s {%s }', [ n, s ]));
			//
			Markup.Add(Format('<td id="%s">', [ n ]));
			GenerateCtrl(n, Ctrl);
			Markup.Add('</td>');
		end;
	finally
		Free;
	end;
end;

end.
