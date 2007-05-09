unit htPersistBase;

interface

uses
	Classes;

type
	ThtPersistBase = class(TPersistent)
	private
		FOnChange: TNotifyEvent;
	protected
		procedure Change; virtual;
	public
		//:$ OnChange event is fired when a style property is changed.
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
	end;

implementation

{ ThtPersistBase }

procedure ThtPersistBase.Change;
begin
	if Assigned(FOnChange) then
		FOnChange(Self);
end;

end.
