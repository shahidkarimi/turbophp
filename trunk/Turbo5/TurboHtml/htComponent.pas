unit htComponent;

interface

uses
	Classes,
	htInterfaces, htMarkup;

type
	ThtComponent = class(TComponent, IhtComponent, IhtGenerator)
	private
		FPriority: Integer;
	protected
		function GetPriority: Integer;
		procedure SetPriority(const Value: Integer);
	public
		procedure Generate(inMarkup: ThtMarkup); virtual;
	published
		property Priority: Integer read GetPriority write SetPriority;
	end;

implementation

{ ThtComponent }

function ThtComponent.GetPriority: Integer;
begin
	Result := FPriority;
end;

procedure ThtComponent.SetPriority(const Value: Integer);
begin
	FPriority := Value;
end;

procedure ThtComponent.Generate(inMarkup: ThtMarkup);
begin
	//
end;

end.
