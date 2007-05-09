unit Box;

interface

uses
	Types, Classes, Contnrs, Graphics,
	Node;

type
	TBox = class;
	TBoxClass = class of TBox;
	TBoxList = class;
	//
	TBoxNode = class(TNode)
	public
		procedure Measure(inBox: TBox); virtual; abstract;
		procedure Render(inBox: TBox; inCanvas: TCanvas;
			inRect: TRect); virtual; abstract;
	end;
	//
	TBox = class
	protected
		function CreateBoxList: TBoxList;
		procedure MeasureBox(inBox: TBox); virtual;
		procedure MeasureBoxes;
	public
		Node: TNode;
		Left: Integer;
		Top: Integer;
		Width: Integer;
		Height: Integer;
		Offset: TPoint;
		Boxes: TBoxList;
		constructor Create;
		destructor Destroy; override;
		function AddBox: TBox; overload;
		function AddBox(inClass: TBoxClass): TBox; overload;
		procedure Measure;
	end;
	//
	TBoxList = class(TObjectList)
	protected
		function GetBox(inIndex: Integer): TBox;
		function GetMax: Integer;
		procedure SetBox(inIndex: Integer; const Value: TBox);
	public
		function AddBox: TBox; overload;
		function AddBox(inClass: TBoxClass): TBox; overload;
		property Box[inIndex: Integer]: TBox read GetBox write SetBox; default;
		property Max: Integer read GetMax;
	end;

implementation

{ TBox }

constructor TBox.Create;
begin
end;

destructor TBox.Destroy;
begin
	Boxes.Free;
	inherited;
end;

function TBox.CreateBoxList: TBoxList;
begin
	if Boxes = nil then
		Boxes := TBoxList.Create;
	Result := Boxes;
end;

function TBox.AddBox(inClass: TBoxClass): TBox;
begin
	Result := CreateBoxList.AddBox(inClass);
end;

function TBox.AddBox: TBox;
begin
	Result := AddBox(TBox);
end;

procedure TBox.MeasureBox(inBox: TBox);
begin
	inBox.Measure;
end;

procedure TBox.MeasureBoxes;
var
	i: Integer;
begin
	if Boxes <> nil then
		for i := 0 to Boxes.Max do
			MeasureBox(Boxes[i]);
end;

procedure TBox.Measure;
begin
	MeasureBoxes;
	if (Node <> nil) and (Node is TBoxNode) then
		TBoxNode(Node).Measure(Self);
end;

{ TBoxList }

function TBoxList.AddBox(inClass: TBoxClass): TBox;
begin
	Result := inClass.Create;
	Add(Result);
end;

function TBoxList.AddBox: TBox;
begin
	Result := AddBox(TBox);
end;

function TBoxList.GetBox(inIndex: Integer): TBox;
begin
	Result := TBox(Items[inIndex]);
end;

function TBoxList.GetMax: Integer;
begin
	Result := Pred(Count);
end;

procedure TBoxList.SetBox(inIndex: Integer; const Value: TBox);
begin
	Items[inIndex] := Value;
end;

end.
