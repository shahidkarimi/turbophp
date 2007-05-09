unit ConnectionProperty;

interface

uses
	SysUtils, Classes, Forms,
	ADODB, Db,
	dcfdes, dcsystem, dcdsgnstuff,
	ThComponent, ThDataConnection, TpPersistComponent;

type
	TConnnectionStringPropertyEditor = class(TStringProperty)
	public
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues(Proc: TGetStrProc); override;
		procedure Edit; override;
	end;

procedure RegisterConnectionStringPropertyEditor;

implementation

uses
	ConnectionStringEdit, Config;

procedure RegisterConnectionStringPropertyEditor;
begin
	RegisterPropertyEditor(TypeInfo(string), TThDataConnection,
		'ConnectionString', TConnnectionStringPropertyEditor);
end;

{ TConnnectionStringPropertyEditor }

function TConnnectionStringPropertyEditor.GetAttributes: TPropertyAttributes;
begin
	Result := [ paDialog ];
end;

procedure TConnnectionStringPropertyEditor.Edit;
begin
	with TConnectionStringEditForm.Create(nil) do
	try
		ConnectionString := Value;
		ShowModal;
		Value := ConnectionString;
	finally
		Free;
	end;
end;

procedure TConnnectionStringPropertyEditor.GetValues(Proc: TGetStrProc);
var
	i: Integer;
begin
	with NeedConnectionStore do
		for i := 0 to Pred(ConnectionStrings.Count) do
			Proc(ConnectionStrings[i]);
end;

end.
