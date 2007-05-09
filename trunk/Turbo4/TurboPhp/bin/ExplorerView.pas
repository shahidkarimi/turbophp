unit ExplorerView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, Menus, ComCtrls, ToolWin,
	VirtualTrees, VirtualExplorerTree, StdCtrls;

type
	TExplorerForm = class(TForm)
		ExplorerTree: TVirtualExplorerTree;
    NoFolderLabel: TLabel;
    procedure FormResize(Sender: TObject);
	private
		{ Private declarations }
		procedure SetRootFolder(const Value: string);
	public
		{ Public declarations }
		property RootFolder: string write SetRootFolder;
	end;

implementation

{$R *.dfm}

{ TExplorerForm }

procedure TExplorerForm.SetRootFolder(const Value: string);
begin
	ExplorerTree.Visible := (Value <> '');
	NoFolderLabel.Visible := not ExplorerTree.Visible;
	if Value <> '' then
		ExplorerTree.RootFolderCustomPath := Value;
end;

procedure TExplorerForm.FormResize(Sender: TObject);
begin
	if Height < 10 then
		Beep;
end;

end.
