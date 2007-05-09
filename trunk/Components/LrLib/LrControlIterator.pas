unit LrControlIterator;

interface

uses
	Classes, Controls,
	LrIterator;

type
	TLrCtrlIterator = class(TLrIterator)
	private
		FContainer: TWinControl;
	protected
		function GetCtrl: TControl; virtual;
		function GetCtrls(inIndex: Integer): TControl; virtual;
		procedure SetContainer(const Value: TWinControl); virtual;
	public
		constructor Create(inContainer: TWinControl = nil);
		function AlignHeight(inAligns: TAlignSet): Integer;
		function AlignMaxHeight(inAligns: TAlignSet): Integer;
		function AlignWidth(inAligns: TAlignSet): Integer;
		function CountAligns(inAligns: TAlignSet): Integer;
		function Eof: Boolean; override;
		function Next(inAligns: TAlignSet): Boolean; overload;
		function ListCtrls: TList;
		property Ctrl: TControl read GetCtrl;
		property Ctrls[inIndex: Integer]: TControl read GetCtrls;
		property Container: TWinControl read FContainer write SetContainer;
	end;
	//
	TLrSortedCtrlIterator = class(TLrCtrlIterator)
	private
		FCtrls: TList;
		FSortFunc: TListSortCompare;
	protected
		function GetCtrl: TControl; override;
		function GetCtrls(inIndex: Integer): TControl; override;
		procedure SetContainer(const Value: TWinControl); override;
	protected
		procedure ListCtrls;
		procedure SortCtrls;
	public
		constructor Create(inContainer: TWinControl = nil;
			inDirection: TAlign = alTop);
		destructor Destroy; override;
		function Controls: TList;
		function Eof: Boolean; override;
	 	//function Next(inAligns: TAlignSet): Boolean; overload;
	end;

implementation

	function ThTopCompareCtrls(Item1, Item2: Pointer): Integer;
	begin
		Result := TControl(Item1).Top - TControl(Item2).Top;
		if Result = 0 then
			Result := TControl(Item1).Left - TControl(Item2).Left;
	end;

	function ThLeftCompareCtrls(Item1, Item2: Pointer): Integer;
	begin
		Result := TControl(Item1).Left - TControl(Item2).Left;
		if Result = 0 then
			Result := TControl(Item1).Top - TControl(Item2).Top;
	end;

	function ThBottomCompareCtrls(Item1, Item2: Pointer): Integer;
	begin
		Result := ThTopCompareCtrls(Item2, Item1);
	end;

	function ThRightCompareCtrls(Item1, Item2: Pointer): Integer;
	begin
		Result := ThLeftCompareCtrls(Item2, Item1);
	end;

{ TLrCtrlIterator }

constructor TLrCtrlIterator.Create(inContainer: TWinControl);
begin
	Container := inContainer;
end;

function TLrCtrlIterator.AlignHeight(inAligns: TAlignSet): Integer;
begin
	Reset;
	Result := 0;
	while Next(inAligns) do
		Result := Result + Ctrl.Height;
end;

function TLrCtrlIterator.AlignMaxHeight(inAligns: TAlignSet): Integer;
begin
	Reset;
	Result := 0;
	while Next(inAligns) do
		if Ctrl.Height > Result then
			Result := Ctrl.Height;
end;

function TLrCtrlIterator.AlignWidth(inAligns: TAlignSet): Integer;
begin
	Reset;
	Result := 0;
	while Next(inAligns) do
		Result := Result + Ctrl.Width;
end;

function TLrCtrlIterator.CountAligns(inAligns: TAlignSet): Integer;
begin
	Reset;
	Result := 0;
	while Next(inAligns) do
		Inc(Result);
end;

function TLrCtrlIterator.Eof: Boolean;
begin
	Result := (Index >= Container.ControlCount);
end;

function TLrCtrlIterator.GetCtrls(inIndex: Integer): TControl;
begin
	Result := Container.Controls[inIndex - 1];
end;

function TLrCtrlIterator.GetCtrl: TControl;
begin
	Result := Ctrls[Index];
end;

function TLrCtrlIterator.ListCtrls: TList;
begin
	Reset;
	Result := TList.Create;
	while Next do
		Result.Add(Ctrl);
end;

function TLrCtrlIterator.Next(inAligns: TAlignSet): Boolean;
begin
	Result := true;
	while Next do
		if Ctrl.Align in inAligns then
			exit;
	Result := false;
end;

procedure TLrCtrlIterator.SetContainer(const Value: TWinControl);
begin
	FContainer := Value;
end;

{ TLrSortedCtrlIterator }

constructor TLrSortedCtrlIterator.Create(inContainer: TWinControl;
	inDirection: TAlign);
begin
	FCtrls := TList.Create;
	case inDirection of
		alLeft: FSortFunc := ThLeftCompareCtrls;
		alRight: FSortFunc := ThRightCompareCtrls;
		alBottom: FSortFunc := ThBottomCompareCtrls;
		else FSortFunc := ThTopCompareCtrls;
	end;
	Container := inContainer;
end;

destructor TLrSortedCtrlIterator.Destroy;
begin
	FCtrls.Free;
	inherited;
end;

function TLrSortedCtrlIterator.Eof: Boolean;
begin
	Result := (Index >= FCtrls.Count);
end;

function TLrSortedCtrlIterator.GetCtrls(inIndex: Integer): TControl;
begin
	Result := TControl(FCtrls[inIndex]);
end;

function TLrSortedCtrlIterator.GetCtrl: TControl;
begin
	Result := Ctrls[Index - 1];
end;

{
function TLrSortedCtrlIterator.Next(inAligns: TAlignSet): Boolean;
begin
	Result := true;
	while Next do
		if Ctrl.Align in inAligns then
			exit;
	Result := false;
end;
}

procedure TLrSortedCtrlIterator.ListCtrls;
var
	i: Integer;
begin
	FCtrls.Clear;
	if Container <> nil then
		for i := 0 to Pred(Container.ControlCount) do
			if Container.Controls[i].Visible then
				FCtrls.Add(Container.Controls[i]);
end;

procedure TLrSortedCtrlIterator.SortCtrls;
begin
	FCtrls.Sort(FSortFunc);
end;

procedure TLrSortedCtrlIterator.SetContainer(const Value: TWinControl);
begin
	inherited;
	ListCtrls;
	SortCtrls;
end;

function TLrSortedCtrlIterator.Controls: TList;
begin
	Result := FCtrls;
end;

end.
