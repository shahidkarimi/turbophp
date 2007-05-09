unit LrCollectionEditorView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ComCtrls, ToolWin, Menus, ImgList,
	DesignIntf, DesignEditors,
	LrCollection;

type
	TLrCollectionEditorForm = class(TForm)
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
		FCollection: TLrCustomCollection;
	protected
		procedure BuildAddMenu;
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
		procedure RefreshList;
		procedure SetCollection(const Value: TLrCustomCollection);
		procedure SetCollectionName(const Value: string);
		procedure UpdateDeleteButton;
		procedure UpdateDesigner;
	public
		{ Public declarations }
		Designer: IDesigner;
		property Collection: TLrCustomCollection read FCollection
			write SetCollection;
		property CollectionName: string write SetCollectionName;
	end;

var
	LrCollectionEditorForm: TLrCollectionEditorForm;

implementation

{$R *.dfm}

{ TLrCollectionEditorForm }

procedure TLrCollectionEditorForm.FormClose(Sender: TObject;
	var Action: TCloseAction);
begin
	if Collection <> nil then
		Collection.Editor := nil;
	Action := caFree;
end;

procedure TLrCollectionEditorForm.SetCollection(
	const Value: TLrCustomCollection);
begin
	if FCollection <> nil then
		FCollection.Owner.RemoveFreeNotification(Self);
	FCollection := Value;
	Collection.Owner.FreeNotification(Self);
	Collection.Editor := Self;
	RefreshList;
	BuildAddMenu;
end;

procedure TLrCollectionEditorForm.Notification(AComponent: TComponent;
	Operation: TOperation);
begin
	inherited;
	if (Operation = opRemove) and (AComponent = Collection.Owner) then
	begin
		FCollection := nil;
		Close;
	end;
end;

procedure TLrCollectionEditorForm.BuildAddMenu;
var
	i: Integer;
	item: TMenuItem;
begin
	DropMenu.Items.Clear;
	for i := 0 to Pred(Collection.TypeCount) do
	begin
		item := TMenuItem.Create(Self);
		item.Caption := Collection.TypeDisplayName[i];
		item.Tag := i;
		item.OnClick := AddItemClick;
		DropMenu.Items.Add(item);
	end;
end;

procedure TLrCollectionEditorForm.UpdateDeleteButton;
begin
	DeleteButton.Enabled := ItemsView.Selected <> nil;
end;

procedure TLrCollectionEditorForm.UpdateDesigner;
begin
	if (ItemsView.Selected <> nil) then
		Designer.SelectComponent(Collection.Items[ItemsView.Selected.Index])
	else
		Designer.NoSelection;
end;

procedure TLrCollectionEditorForm.RefreshList;
begin
	ItemsView.Items.Count := Collection.Count;
	ItemsView.Invalidate;
	UpdateDeleteButton;
	UpdateDesigner;
end;

procedure TLrCollectionEditorForm.ItemsViewData(Sender: TObject;
	Item: TListItem);
begin
	if Item.Index >= Collection.Count then
		Item.Caption := 'Bad Index'
	else
		with Collection.Items[Item.Index] do
			Item.Caption := Format('%d - %s: %s', [ Item.Index, Name, ClassName ]);
end;

procedure TLrCollectionEditorForm.AddItemClick(Sender: TObject);
begin
	AddItemButton.Tag := TComponent(Sender).Tag;
	Collection.Add(TComponent(Sender).Tag);
	RefreshList;
end;

procedure TLrCollectionEditorForm.DeleteButtonClick(Sender: TObject);
begin
	Collection.Delete(ItemsView.Selected.Index);
	RefreshList;
end;

procedure TLrCollectionEditorForm.ItemsViewSelectItem(Sender: TObject;
	Item: TListItem; Selected: Boolean);
begin
	UpdateDeleteButton;
	UpdateDesigner;
end;

procedure TLrCollectionEditorForm.SetCollectionName(const Value: string);
begin
	Caption := 'Editing ' + Value;
end;

end.
