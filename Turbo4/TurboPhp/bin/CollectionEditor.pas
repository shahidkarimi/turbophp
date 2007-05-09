unit CollectionEditor;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ComCtrls, ToolWin, Menus, ImgList,
	dcedit, dcfdes, dcsystem, dcdsgnstuff;

type
	TCollectionEditorForm = class(TForm)
		ItemsView: TListView;
		ToolBar1: TToolBar;
		AddItemButton: TToolButton;
		DeleteButton: TToolButton;
		ToolButton1: TToolButton;
		LoadButton: TToolButton;
		SaveButton: TToolButton;
		DropMenu: TPopupMenu;
		ToolImages: TImageList;
		DisabledImages: TImageList;
		procedure ItemsViewData(Sender: TObject; Item: TListItem);
		procedure ItemsViewSelectItem(Sender: TObject; Item: TListItem;
			Selected: Boolean);
		procedure AddItemClick(Sender: TObject);
		procedure DeleteButtonClick(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
	private
		{ Private declarations }
		FCollectionName: string;
		FCollection: TCollection;
	protected
		function GetItemName(inItem: TCollectionItem): string;
		procedure SetCollection(const Value: TCollection);
		procedure SetCollectionName(const Value: string);
	protected
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
		procedure RefreshList;
		procedure UpdateDeleteButton;
		procedure UpdateDesigner;
	public
		{ Public declarations }
		Designer: IDesigner;
		property Collection: TCollection read FCollection
			write SetCollection;
		property CollectionName: string read FCollectionName
			write SetCollectionName;
	end;

implementation

uses
	InspectorView;

{$R *.dfm}

{ CollectionEditorForm }

procedure TCollectionEditorForm.FormClose(Sender: TObject;
	var Action: TCloseAction);
begin
//	if Collection <> nil then
//		Collection.Editor := nil;
	Action := caFree;
end;

procedure TCollectionEditorForm.SetCollection(
	const Value: TCollection);

	function GoodOwner: Boolean;
	begin
		Result := (FCollection <> nil) and (FCollection.Owner <> nil)
			and (FCollection.Owner is TComponent);
	end;

begin
	if GoodOwner then
		TComponent(FCollection.Owner).RemoveFreeNotification(Self);
	FCollection := Value;
	if GoodOwner then
		TComponent(FCollection.Owner).FreeNotification(Self);
	CollectionName := Collection.GetNamePath;
//	Collection.Editor := Self;
	RefreshList;
end;

procedure TCollectionEditorForm.SetCollectionName(const Value: string);
begin
	FCollectionName := Value;
	Caption := 'Editing ' + FCollectionName;
end;

procedure TCollectionEditorForm.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
//	if (Operation = opRemove) and (AComponent = Collection) then
	if (Operation = opRemove) then
		if (FCollection <> nil) and (FCollection.Owner = AComponent) then
		begin
			FCollection := nil;
			Close;
		end;
end;

procedure TCollectionEditorForm.UpdateDeleteButton;
begin
	DeleteButton.Enabled := ItemsView.Selected <> nil;
end;

procedure TCollectionEditorForm.UpdateDesigner;
var
	item: TCollectionItem;
begin
	if (ItemsView.Selected <> nil) then
	begin
		item := Collection.Items[ItemsView.Selected.Index];
		InspectorForm.InspectPersistent(item);
	end;
end;

procedure TCollectionEditorForm.RefreshList;
var
	i: Integer;
begin
	if (ItemsView.Selected <> nil) then
		i := ItemsView.Selected.Index
	else
		i := 0;
	ItemsView.Items.Count := Collection.Count;
	if (i > ItemsView.Items.Count) then
		i := ItemsView.Items.Count - 1;
	if (i >= 0) then
		ItemsView.Selected := ItemsView.Items[i];
	ItemsView.Invalidate;
	UpdateDeleteButton;
	UpdateDesigner;
end;

function TCollectionEditorForm.GetItemName(inItem: TCollectionItem): string;
begin
	if inItem.DisplayName <> '' then
		Result := inItem.DisplayName
	else
		Result := '(no name)';
end;

procedure TCollectionEditorForm.ItemsViewData(Sender: TObject;
	Item: TListItem);
begin
	if Item.Index >= Collection.Count then
		Item.Caption := 'Bad Index'
	else
		Item.Caption := Format('%d - %s',
			[ Item.Index, GetItemName(Collection.Items[Item.Index]) ]);
end;

procedure TCollectionEditorForm.AddItemClick(Sender: TObject);
begin
	//AddItemButton.Tag := TComponent(Sender).Tag;
	Collection.Add;
	RefreshList;
end;

procedure TCollectionEditorForm.DeleteButtonClick(Sender: TObject);
begin
	Collection.Items[ItemsView.Selected.Index].Free;
	RefreshList;
end;

procedure TCollectionEditorForm.ItemsViewSelectItem(Sender: TObject;
	Item: TListItem; Selected: Boolean);
begin
	UpdateDeleteButton;
	UpdateDesigner;
//	if (Item <> nil) and Selected then
//		Designer.SelectComponent(Collection.Components[Item.Index]);
end;

end.
