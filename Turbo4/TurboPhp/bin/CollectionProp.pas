unit CollectionProp;

interface

uses
	Classes, dcedit, dcfdes, dcsystem, dcdsgnstuff;

type
	TCollectionProp = class(TPropertyEditor)
	public
		function Collection: TCollection;
		procedure Edit; override;
		function GetAttributes: TPropertyAttributes; override;
		function GetValue: string; override;
	end;

procedure RegisterCollectionPropertyEditor;

implementation

uses
	CollectionEditor;

procedure RegisterCollectionPropertyEditor;
begin
	RegisterPropertyEditor(TypeInfo(TCollection), nil, '', TCollectionProp);
end;

{ TCollectionProp }

function TCollectionProp.Collection: TCollection;
begin
	Result := TCollection(GetOrdValue);
end;

procedure TCollectionProp.Edit;
var
	n: string;
begin
	n := GetComponent(0).GetNamePath + '.' + GetName;
	//n := Collection.Owner.GetNamePath + '.' + GetName;
	//n := Collection.GetNamePath + '.Items';
	// if cached editor, show it
//	if Collection.Editor <> nil then
//		TCollectionEditorForm(Collection.Editor).Show
//	else
		with TCollectionEditorForm.Create(nil) do
		begin
			Designer := Self.Designer;
			Collection := Self.Collection;
			CollectionName := n;
			Show;
		end;
end;

function TCollectionProp.GetAttributes: TPropertyAttributes;
begin
	Result := [ paDialog ];
end;

function TCollectionProp.GetValue: string;
begin
	Result := '(' + Collection.ClassName + ')';
end;

end.
