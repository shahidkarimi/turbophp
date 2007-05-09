unit LrComponentIterator;

interface

uses
	Classes, Controls,
	LrIterator;

type
	TLrComponentIterator = class(TLrIterator)
	private
		FContainer: TComponent;
	protected
		function GetComponent: TComponent; virtual;
		procedure SetContainer(const Value: TComponent); virtual;
	public
		constructor Create(inContainer: TComponent = nil);
		function Eof: Boolean; override;
		function Next(inClass: TClass): Boolean; overload;
		property Component: TComponent read GetComponent;
		property Container: TComponent read FContainer write SetContainer;
	end;
	//
	TLrSortedComponentIterator = class(TLrComponentIterator)
	private
		FComponents: TList;
		FSortFunc: TListSortCompare;
	protected
		function GetComponent: TComponent; override;
		function GetComponents(inIndex: Integer): TComponent;
		procedure SetContainer(const Value: TComponent); override;
	protected
		procedure ListComponents;
		procedure SortComponents;
	public
		constructor Create(inContainer: TComponent = nil;
			inSortFunction: TListSortCompare = nil);
		destructor Destroy; override;
		function Eof: Boolean; override;
		property Components[inIndex: Integer]: TComponent read GetComponents;
	end;

implementation

{ TLrComponentIterator }

constructor TLrComponentIterator.Create(inContainer: TComponent);
begin
	Container := inContainer;
end;

function TLrComponentIterator.Eof: Boolean;
begin
	Result := (Index >= Container.ComponentCount);
end;

function TLrComponentIterator.GetComponent: TComponent;
begin
	Result := Container.Components[Index - 1];
end;

function TLrComponentIterator.Next(inClass: TClass): Boolean;
begin
	Result := true;
	while Next do
		if Component is inClass then
			exit;
	Result := false;
end;

procedure TLrComponentIterator.SetContainer(const Value: TComponent);
begin
	FContainer := Value;
end;

{ TLrSortedComponentIterator }

constructor TLrSortedComponentIterator.Create(inContainer: TComponent;
	inSortFunction: TListSortCompare);
begin
	FComponents := TList.Create;
	FSortFunc := inSortFunction;
	Container := inContainer;
end;

destructor TLrSortedComponentIterator.Destroy;
begin
	FComponents.Free;
	inherited;
end;

function TLrSortedComponentIterator.Eof: Boolean;
begin
	Result := (Index >= FComponents.Count);
end;

function TLrSortedComponentIterator.GetComponents(
	inIndex: Integer): TComponent;
begin
	Result := FComponents[inIndex - 1];
end;

function TLrSortedComponentIterator.GetComponent: TComponent;
begin
	Result := Components[Index];
end;

procedure TLrSortedComponentIterator.ListComponents;
var
	i: Integer;
begin
	FComponents.Clear;
	if Container <> nil then
		for i := 0 to Pred(Container.ComponentCount) do
			if not (Container.Components[i] is TControl) then
				FComponents.Add(Container.Components[i]);
end;

procedure TLrSortedComponentIterator.SortComponents;
begin
	FComponents.Sort(FSortFunc);
end;

procedure TLrSortedComponentIterator.SetContainer(
	const Value: TComponent);
begin
	inherited;
	ListComponents;
	SortComponents;
end;

end.
