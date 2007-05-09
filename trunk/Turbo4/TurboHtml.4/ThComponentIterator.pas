unit ThComponentIterator;

interface

uses
	Classes, Controls;

type
	TThIterator = class
	private
		FIndex: Integer;
	public
		function Eof: Boolean; virtual; abstract;
		function Next: Boolean; overload; virtual;
		procedure Reset;
	public
		property Index: Integer read FIndex write FIndex;
	end;
	//
	TThComponentIterator = class(TThIterator)
	private
		FContainer: TComponent;
	protected
		function GetComponent: TComponent; virtual;
		procedure SetContainer(const Value: TComponent); virtual;
	public
		constructor Create(inContainer: TComponent = nil); virtual;
		function Eof: Boolean; override;
		function Next(inClass: TClass): Boolean; overload;
		property Component: TComponent read GetComponent;
		property Container: TComponent read FContainer write SetContainer;
	end;
	//
	TThCtrlIterator = class(TThIterator)
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
	TThSortedCtrlIterator = class(TThCtrlIterator)
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
		function Next(inAligns: TAlignSet): Boolean; overload;
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

{ TThIterator }

procedure TThIterator.Reset;
begin
	Index := 0;
end;

function TThIterator.Next: Boolean;
begin
	Result := not Eof;
	if Result then
		Inc(FIndex)
	else
		Reset;
end;

{ TThComponentIterator }

constructor TThComponentIterator.Create(inContainer: TComponent);
begin
	Container := inContainer;
end;

function TThComponentIterator.Eof: Boolean;
begin
	Result := (Index >= Container.ComponentCount);
end;

function TThComponentIterator.GetComponent: TComponent;
begin
	Result := Container.Components[Index - 1];
end;

function TThComponentIterator.Next(inClass: TClass): Boolean;
begin
	Result := true;
	while Next do
		if Component is inClass then
			exit;
	Result := false;
end;

procedure TThComponentIterator.SetContainer(const Value: TComponent);
begin
	FContainer := Value;
end;

{ TThCtrlIterator }

constructor TThCtrlIterator.Create(inContainer: TWinControl);
begin
	Container := inContainer;
end;

function TThCtrlIterator.AlignHeight(inAligns: TAlignSet): Integer;
begin
	Reset;
	Result := 0;
	while Next(inAligns) do
		Result := Result + Ctrl.Height;
end;

function TThCtrlIterator.AlignMaxHeight(inAligns: TAlignSet): Integer;
begin
	Reset;
	Result := 0;
	while Next(inAligns) do
		if Ctrl.Height > Result then
			Result := Ctrl.Height;
end;

function TThCtrlIterator.AlignWidth(inAligns: TAlignSet): Integer;
begin
	Reset;
	Result := 0;
	while Next(inAligns) do
		Result := Result + Ctrl.Width;
end;

function TThCtrlIterator.CountAligns(inAligns: TAlignSet): Integer;
begin
	Reset;
	Result := 0;
	while Next(inAligns) do
		Inc(Result);
end;

function TThCtrlIterator.Eof: Boolean;
begin
	Result := (Index >= Container.ControlCount);
end;

function TThCtrlIterator.GetCtrls(inIndex: Integer): TControl;
begin
	Result := Container.Controls[inIndex - 1];
end;

function TThCtrlIterator.GetCtrl: TControl;
begin
	Result := Ctrls[Index];
end;

function TThCtrlIterator.ListCtrls: TList;
begin
	Reset;
	Result := TList.Create;
	while Next do
		Result.Add(Ctrl);
end;

function TThCtrlIterator.Next(inAligns: TAlignSet): Boolean;
begin
	Result := true;
	while Next do
		if Ctrl.Align in inAligns then
			exit;
	Result := false;
end;

procedure TThCtrlIterator.SetContainer(const Value: TWinControl);
begin
	FContainer := Value;
end;

{ TThSortedCtrlIterator }

constructor TThSortedCtrlIterator.Create(inContainer: TWinControl;
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

destructor TThSortedCtrlIterator.Destroy;
begin
	FCtrls.Free;
	inherited;
end;

function TThSortedCtrlIterator.Eof: Boolean;
begin
	Result := (Index >= FCtrls.Count);
end;

function TThSortedCtrlIterator.GetCtrl: TControl;
begin
	Result := Ctrls[Index];
end;

function TThSortedCtrlIterator.GetCtrls(inIndex: Integer): TControl;
begin
	Result := TControl(FCtrls[inIndex - 1]);
end;

function TThSortedCtrlIterator.Next(inAligns: TAlignSet): Boolean;
begin
	Result := true;
	while Next do
		if Ctrl.Align in inAligns then
			exit;
	Result := false;
end;

procedure TThSortedCtrlIterator.ListCtrls;
var
	i: Integer;
begin
	FCtrls.Clear;
	if Container <> nil then
		for i := 0 to Pred(Container.ControlCount) do
			if Container.Controls[i].Visible then
				FCtrls.Add(Container.Controls[i]);
end;

procedure TThSortedCtrlIterator.SortCtrls;
begin
	FCtrls.Sort(FSortFunc);
end;

procedure TThSortedCtrlIterator.SetContainer(const Value: TWinControl);
begin
	inherited;
	ListCtrls;
	SortCtrls;
end;

function TThSortedCtrlIterator.Controls: TList;
begin
	Result := FCtrls;
end;

end.
