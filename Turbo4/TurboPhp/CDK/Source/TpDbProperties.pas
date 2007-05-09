unit TpDbProperties;

interface

uses
	SysUtils, Classes, Controls, TypInfo,
	dcedit, dcfdes, dcsystem, dcdsgnstuff,
	Db;

type
	TTpListingProperty = class(TStringProperty)
	protected
		procedure GetValueList(List: TStrings); virtual; abstract;
	public
		procedure GetValues(Proc: TGetStrProc); override;
	end;
	//
	TTpTableNameProperty = class(TTpListingProperty)
	protected
		function GetConnectionPropName: string;
		procedure GetValueList(List: TStrings); override;
	public
		function GetAttributes: TPropertyAttributes; override;
	end;
	//
	TTpFieldNameProperty = class(TTpListingProperty)
	protected
		function GetDataSourcePropName: string;
		procedure GetValueList(List: TStrings); override;
	public
		function GetAttributes: TPropertyAttributes; override;
	end;

procedure RegisterDbProperties;

implementation

uses
	TpDataConnection, TpDb;

procedure RegisterDbProperties;
begin
	RegisterPropertyEditor(TypeInfo(string), nil, 'TableName',
		TTpTableNameProperty);
	RegisterPropertyEditor(TypeInfo(string), nil, 'FieldName',
		TTpFieldNameProperty);
end;

{ TTpListingProperty }

procedure TTpListingProperty.GetValues(Proc: TGetStrProc);
var
	i: Integer;
	values: TStringList;
begin
	values := TStringList.Create;
	try
		GetValueList(values);
		for i := 0 to Pred(values.Count) do
			Proc(values[I]);
	finally
		values.Free;
	end;
end;

{ TTpTableNameProperty }

function TTpTableNameProperty.GetAttributes: TPropertyAttributes;
begin
	Result := [ paValueList ];
end;

function TTpTableNameProperty.GetConnectionPropName: string;
begin
	Result := 'Db';
end;

procedure TTpTableNameProperty.GetValueList(List: TStrings);
var
	db: TTpDb;
begin
	try
		db :=	GetObjectProp(GetComponent(0), GetConnectionPropName)
			as TTpDb;
		if (db <> nil) and (db.DesignConnection.Connected) then
			db.DesignConnection.Connection.GetTableNames(List);
	except
	end;
end;

{ TTpFieldNameProperty }

function TTpFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
	Result := [ paValueList ];
end;

function TTpFieldNameProperty.GetDataSourcePropName: string;
begin
	Result := 'DataSource';
end;

procedure TTpFieldNameProperty.GetValueList(List: TStrings);
var
	ps: TTpDataSource;
begin
	try
		ps := GetObjectProp(GetComponent(0), GetDataSourcePropName)
			as TTpDataSource;
		if (ps <> nil) and (ps.DataSet <> nil) then
			ps.DataSet.GetFieldNames(List);
	except
	end;
end;

end.
