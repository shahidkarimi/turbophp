unit TpGenericControl;

interface

uses
	SysUtils, Classes, Controls,
	ThWebControl, ThTag, TpControls, TpPanel;

type
	TTpGenericControl = class(TTpPanel)
  private
    FTpClassName: string;
		FParams: TStringList;
		FOnGenerate: TTpEvent;
	protected
		procedure SetParams(const Value: TStringList);
		procedure SetTpClassName(const Value: string);
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
		procedure CellTag(inTag: TThTag); override;
	published
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
		property Params: TStringList read FParams write SetParams;
		property TpClassName: string read FTpClassName write SetTpClassName;
	end;

implementation

constructor TTpGenericControl.Create(inOwner: TComponent);
begin
	inherited;
	FParams := TStringList.Create;
end;

destructor TTpGenericControl.Destroy;
begin
	FParams.Free;
	inherited;
end;

procedure TTpGenericControl.SetParams(const Value: TStringList);
begin
	FParams.Assign(Value);
end;

procedure TTpGenericControl.SetTpClassName(const Value: string);
begin
	FTpClassName := Value;
end;

procedure TTpGenericControl.CellTag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Attributes[tpClass] := TpClassName;
		Attributes['tpName'] := Name;
		Attributes.AddStrings(Params);
	end;
end;

end.
