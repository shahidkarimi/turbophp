unit LrCollectionEditor;

interface

uses
	DesignIntf, DesignEditors, LrCollection;

type
	TLrCollectionEditor = class(TPropertyEditor)
	public
		function Collection: TLrCustomCollection;
		procedure Edit; override;
		function GetAttributes: TPropertyAttributes; override;
		function GetName: string; override;
		function GetValue: string; override;
	end;

procedure Register;

implementation

uses
	LrCollectionEditorView;

procedure Register;
begin
	RegisterPropertyEditor(
		TypeInfo(TLrCustomCollection), nil, '', TLrCollectionEditor);
end;

{ TLrCollectionEditor }

function TLrCollectionEditor.Collection: TLrCustomCollection;
begin
	Result := TLrCustomCollection(GetOrdValue);
end;

function TLrCollectionEditor.GetAttributes: TPropertyAttributes;
begin
	Result := [ paDialog	];
end;

function TLrCollectionEditor.GetName: string;
begin
	Result := inherited GetName;
//	Result := Collection.Name;
//	Result := 'Items';
end;

procedure TLrCollectionEditor.Edit;
var
	n: string;
begin
//	inherited;
	//n := GetName;
	n := Collection.Owner.Name + '.' + GetName;
	if Collection.Editor <> nil then
		TLrCollectionEditorForm(Collection.Editor).Show
	else
		with TLrCollectionEditorForm.Create(nil) do
		begin
			Designer := Self.Designer;
			Collection := Self.Collection;
			//DisplayName := Self.Collection.Owner.Name + '.' + GetName;
			//CollectionName := Self.Collection.ClassName; //'Some Name'; //Self.GetName;
			CollectionName := n; //'Some Name'; //Self.GetName;
			Show;
		end;
end;

function TLrCollectionEditor.GetValue: string;
begin
	Result := '(' + Collection.ClassName + ')';
end;

end.
