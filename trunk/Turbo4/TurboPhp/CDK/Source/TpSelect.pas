unit TpSelect;

interface

uses
	Windows, SysUtils, Classes, Controls, Graphics,
	Types,
	ThWebControl, ThTag, ThSelect,
	TpControls;

type
	TTpSelect = class(TThSelect)
	private
		FListSource: TComponent;
		FOnSubmit: TTpEvent;
		FOnGenerate: TTpEvent;
	protected
		procedure SetListSource(const Value: TComponent);
	protected
		procedure SourceRemoved; override;
		procedure Tag(inTag: TThTag); override;
	published
		property ListSource: TComponent read FListSource write SetListSource;
		property OnSubmit: TTpEvent read FOnSubmit write FOnSubmit;
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
	end;
	//
	TTpListBox = class(TThListBox)
	private
		FListSource: TComponent;
		FOnSubmit: TTpEvent;
		FOnGenerate: TTpEvent;
	protected
		procedure SetListSource(const Value: TComponent);
	protected
		procedure SourceRemoved; override;
		procedure Tag(inTag: TThTag); override;
	published
		property ListSource: TComponent read FListSource write SetListSource;
		property OnSubmit: TTpEvent read FOnSubmit write FOnSubmit;
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
	end;

implementation

uses
	ThVclUtils, ThListSource;

{ TTpSelect }

procedure TTpSelect.SetListSource(const Value: TComponent);
begin
	FListSource := Value;
	if (FListSource = nil) or (not ThIsAs(Value, IThListSource, FSource)) then
	begin
		FListSource := nil;
		FSource := nil;
	end;
	UpdateItems;
end;

procedure TTpSelect.SourceRemoved;
begin
	FListSource := nil;
	inherited;
end;

procedure TTpSelect.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpSelect');
		Add('tpName', Name);
		if (Source <> nil) then
			Add('tpSource', Source.Name);
		Add('tpOnSubmit', OnSubmit);
		Add('tpOnGenerate', OnGenerate);
	end;
end;

{ TTpListBox }

procedure TTpListBox.SetListSource(const Value: TComponent);
begin
	FListSource := Value;
	if (FListSource = nil) or (not ThIsAs(Value, IThListSource, FSource)) then
	begin
		FListSource := nil;
		FSource := nil;
	end;
	UpdateItems;
end;

procedure TTpListBox.SourceRemoved;
begin
	FListSource := nil;
	inherited;
end;

procedure TTpListBox.Tag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		Add(tpClass, 'TTpSelect');
		Add('tpName', Name);
		if (Source <> nil) then
			Add('tpSource', Source.Name);
		Add('tpOnSubmit', OnSubmit);
		Add('tpOnGenerate', OnGenerate);
	end;
end;

end.
