unit DbProperties;

interface

uses
	SysUtils, Classes, Controls, TypInfo,
	Db,
	dcedit, dcfdes, dcsystem, dcdsgnstuff;

type
	TListingProperty = class(TStringProperty)
	protected
		procedure GetValueList(List: TStrings); virtual; abstract;
	public
		procedure GetValues(Proc: TGetStrProc); override;
	end;
	//
	TTableNameProperty = class(TListingProperty)
	protected
		function GetConnectionPropName: string;
		procedure GetValueList(List: TStrings); override;
	public
		function GetAttributes: TPropertyAttributes; override;
	end;
	//
	TFieldNameProperty = class(TListingProperty)
	protected
		function GetDataSourcePropName: string;
		procedure GetValueList(List: TStrings); override;
	public
		function GetAttributes: TPropertyAttributes; override;
	end;

procedure RegisterDbProperties;

implementation

uses
	TpDb;

procedure RegisterDbProperties;
begin
	RegisterPropertyEditor(TypeInfo(string), nil, 'TableName',
		TTableNameProperty);
	RegisterPropertyEditor(TypeInfo(string), nil, 'FieldName',
		TFieldNameProperty);
end;

{ TListingProperty }

procedure TListingProperty.GetValues(Proc: TGetStrProc);
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

{ TTableNameProperty }

function TTableNameProperty.GetAttributes: TPropertyAttributes;
begin
	Result := [ paValueList ];
end;

function TTableNameProperty.GetConnectionPropName: string;
begin
	Result := 'Db';
end;

procedure TTableNameProperty.GetValueList(List: TStrings);
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

{ TFieldNameProperty }

function TFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
	Result := [ paValueList ];
end;

function TFieldNameProperty.GetDataSourcePropName: string;
begin
	Result := 'DataSource';
end;

procedure TFieldNameProperty.GetValueList(List: TStrings);
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
