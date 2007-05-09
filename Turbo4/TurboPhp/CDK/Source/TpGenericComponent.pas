unit TpGenericComponent;

interface

uses
	SysUtils, Classes,
	ThHeaderComponent, ThTag,
	TpControls;

type
	TTpGenericComponent = class(TThHeaderComponent)
	private
		FTpClassName: string;
		FParams: TStringList;
	protected
    procedure SetParams(const Value: TStringList);
    procedure SetTpClassName(const Value: string);
	protected
		procedure Tag(inTag: TThTag); override;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
	published
		property TpClassName: string read FTpClassName write SetTpClassName;
		property Params: TStringList read FParams write SetParams;
	end;

implementation

constructor TTpGenericComponent.Create(inOwner: TComponent);
begin
  inherited;
	FParams := TStringList.Create;
end;

destructor TTpGenericComponent.Destroy;
begin
	FParams.Free;
	inherited;
end;

procedure TTpGenericComponent.SetParams(const Value: TStringList);
begin
	FParams.Assign(Value);
end;

procedure TTpGenericComponent.SetTpClassName(const Value: string);
begin
  FTpClassName := Value;
end;

procedure TTpGenericComponent.Tag(inTag: TThTag);
begin
	with inTag do
	begin
		Add(tpClass, TpClassName);
		Add('tpName', Name);
		Attributes.AddStrings(Params);
	end;
end;

end.
