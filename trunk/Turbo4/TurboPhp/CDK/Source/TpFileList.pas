unit TpFileList;

interface

uses
	SysUtils, Classes,
	ThHeaderComponent, ThTag,
	TpControls;

type
	TTpFileList = class(TThHeaderComponent)
	private
		FFolder: string;
    FFullPath: Boolean;
    FOnFilter: TTpEvent;
	protected
		procedure Tag(inTag: TThTag); override;
	public
		constructor Create(inOwner: TComponent); override;
		destructor Destroy; override;
	published
		property Folder: string read FFolder write FFolder;
		property FullPath: Boolean read FFullPath write FFullPath;
		property OnFilter: TTpEvent read FOnFilter write FOnFilter;
	end;

implementation

uses
	DCPbase64;

constructor TTpFileList.Create(inOwner: TComponent);
begin
	inherited;
end;

destructor TTpFileList.Destroy;
begin
	inherited;
end;

procedure TTpFileList.Tag(inTag: TThTag);
begin
	with inTag do
	begin
		Add(tpClass, 'TTpFileList');
		Add('tpName', Name);
		Add('tpFolder', Folder);
		Attributes.Add('tpFullPath', FullPath);
		Add('tpOnFilter', OnFilter);
	end;
end;

end.
