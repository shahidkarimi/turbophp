unit ThListSource;

interface

uses
	Classes,
	ThHeaderComponent, ThNotifierList;

type
	IThListSource = interface
	['{0B9415E1-4421-46A1-992A-BDA5622899DB}']
		function GetItems: TStrings;
		function GetNamePath: string;
		function GetNotifiers: TThNotifierList;
		property Items: TStrings read GetItems;
		property Name: string read GetNamePath;
		property Notifiers: TThNotifierList read GetNotifiers;
	end;
	//
	TThListSource = class(TThHeaderComponent, IThListSource)
	private
		FNotifiers: TThNotifierList;
	protected
		function GetItems: TStrings; virtual; abstract;
		function GetNotifiers: TThNotifierList;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure UpdateItems; virtual; abstract;
	public
		property Notifiers: TThNotifierList read GetNotifiers;
		property Items: TStrings read GetItems;
	end;

implementation

{ TThListSource }

constructor TThListSource.Create(AOwner: TComponent);
begin
	inherited;
	FNotifiers := TThNotifierList.Create;
end;

destructor TThListSource.Destroy;
begin
	FNotifiers.Free;
	inherited;
end;

function TThListSource.GetNotifiers: TThNotifierList;
begin
	Result := FNotifiers;
end;

end.
