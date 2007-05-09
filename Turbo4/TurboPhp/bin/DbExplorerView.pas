unit DbExplorerView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ADODB, VirtualTrees, Grids, DBGrids, ComCtrls, ImgList;

type
  TDbExplorerForm = class(TForm)
    MySql: TADOConnection;
		DataSource1: TDataSource;
		MySqlDbsQuery: TADOQuery;
		Tree: TTreeView;
		MySqlTablesQuery: TADOQuery;
    ImageList1: TImageList;
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
		function Connect(const inConnectionString: string): Boolean;
		procedure InitTree;
    procedure ListAllTables(inNode: TTreeNode);
    procedure ListTables(inNode: TTreeNode);
	public
		{ Public declarations }
	end;

var
	DbExplorerForm: TDbExplorerForm;

implementation

uses
	TpDbConnectionStrings;

{$R *.dfm}

procedure TDbExplorerForm.FormCreate(Sender: TObject);
begin
	//Ado.ConnectionString := BuildMySqlConnectionString('', '', '', '');
	InitTree;
end;

function TDbExplorerForm.Connect(const inConnectionString: string): Boolean;
begin
	MySql.Connected := false;
	try
		MySql.ConnectionString := inConnectionString;
		MySql.Connected := true;
	except
	end;
	Result := MySql.Connected;
end;

procedure TDbExplorerForm.ListTables(inNode: TTreeNode);
begin
	if Connect(BuildMySqlConnectionString('', inNode.Text, '', '')) then
	try
		MySqlTablesQuery.Active := true;
		while not MySqlTablesQuery.Eof do
		begin
			Tree.Items.AddChild(inNode, MySqlTablesQuery.Fields[0].AsString)
				.ImageIndex := 2;
			MySqlTablesQuery.Next;
		end;
	finally
	end;
end;

procedure TDbExplorerForm.ListAllTables(inNode: TTreeNode);
var
	i: Integer;
begin
	for i := 0 to Pred(inNode.Count) do
		ListTables(inNode[i]);
end;

procedure TDbExplorerForm.InitTree;
var
	root: TTreeNode;
begin
	try
		Tree.Items.BeginUpdate;
		try
			Tree.Items.Clear;
			root := Tree.Items.Add(nil, 'MySql Databases');
			if Connect(BuildMySqlConnectionString('', '', '', '')) then
			try
				MySqlDbsQuery.Active := true;
				while not MySqlDbsQuery.Eof do
				begin
					Tree.Items.AddChild(root, MySqlDbsQuery.Fields[0].AsString)
						.ImageIndex := 1;
					MySqlDbsQuery.Next;
				end;
			except
			end;
			root.Expanded := true;
			ListAllTables(root);
		finally
			Tree.Items.EndUpdate;
		end;
	except
	end;
end;

end.
