unit Node;

interface

uses
	Types, Classes, Graphics, Style;

type
	TContextBase = class
	end;
	TContextClass = class of TContextBase;
	//
	TNode = class;
	TNodeClass = class of TNode;
	TNodeList = class;
	//
	TNode = class
		class function GetContextClass: TContextClass; virtual; 
	public
		Nodes: TNodeList;
		Style: TStyle;
		Text: string;
		constructor Create; virtual;
		destructor Destroy; override;
		function Add(inNodeClass: TNodeClass): TNode;
		property ContextClass: TContextClass read GetContextClass;
	end;
	//
	TNodeList = class(TList)
	protected
		function GetMax: Integer;
		function GetNode(inIndex: Integer): TNode;
		procedure SetNode(inIndex: Integer; const Value: TNode);
	public
		property Max: Integer read GetMax;
		property Node[inIndex: Integer]: TNode read GetNode write SetNode; default;
	end;
	//
	TNodeIterator = class
	private
		FIndex: Integer;
		FNodes: TNodeList;
	protected
		function GetNext: Boolean;
		function GetNode: TNode;
	public
		constructor Create(inNodes: TNodeList);
		property Index: Integer read FIndex;
		property Next: Boolean read GetNext;
		property Node: TNode read GetNode;
	end;

implementation

{ TNode }

class function TNode.GetContextClass: TContextClass;
begin
	Result := TContextBase;
end;

constructor TNode.Create;
begin
	Style := TStyle.Create;
end;

destructor TNode.Destroy;
begin
	Style.Free;
	Nodes.Free;
	inherited;
end;

function TNode.Add(inNodeClass: TNodeClass): TNode;
begin
	if Nodes = nil then
		Nodes := TNodeList.Create;
	Result := inNodeClass.Create;
	Nodes.Add(Result);
end;

{ TNodeList }

function TNodeList.GetMax: Integer;
begin
	Result := Pred(Count);
end;

function TNodeList.GetNode(inIndex: Integer): TNode;
begin
	Result := TNode(Items[inIndex]);
end;

procedure TNodeList.SetNode(inIndex: Integer; const Value: TNode);
begin
	Items[inIndex] := Value;
end;

{ TNodeIterator }

constructor TNodeIterator.Create(inNodes: TNodeList);
begin
	FNodes := inNodes;
	FIndex := -1;
end;

function TNodeIterator.GetNext: Boolean;
begin
	Inc(FIndex);
	Result := Index < FNodes.Count;
end;

function TNodeIterator.GetNode: TNode;
begin
	Result := FNodes[FIndex];
end;

end.
