unit ThDataConnectionProperty;

interface

uses
	Classes, DesignIntf, DesignEditors,
	ThDataConnection;

type
	TThConnnectionStringPropertyEditor = class(TStringProperty)
	public
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues(Proc: TGetStrProc); override;
		procedure Edit; override;
	end;

procedure RegisterConnectionStringPropertyEditor;

implementation

uses
	ThConnectionStringEdit;

procedure RegisterConnectionStringPropertyEditor;
begin
	RegisterPropertyEditor(TypeInfo(TThConnectionString), TThDataConnection,
		'ConnectionString', TThConnnectionStringPropertyEditor);
end;

{ TThConnnectionStringPropertyEditor }

function TThConnnectionStringPropertyEditor.GetAttributes: TPropertyAttributes;
begin
	Result := [ paDialog ];
end;

procedure TThConnnectionStringPropertyEditor.Edit;
begin
	with TThConnectionStringEditForm.Create(nil) do
	try
		ConnectionString := Value;
		ShowModal;
		Value := ConnectionString;
	finally
		Free;
	end;
end;

procedure TThConnnectionStringPropertyEditor.GetValues(Proc: TGetStrProc);
var
	i: Integer;
begin
	with NeedConnectionStore do
		for i := 0 to Pred(ConnectionStrings.Count) do
			Proc(ConnectionStrings[i]);
end;

end.
