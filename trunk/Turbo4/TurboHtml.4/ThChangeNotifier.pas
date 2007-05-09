unit ThChangeNotifier;

interface

uses
	SysUtils, Classes, Graphics;

type
	TThChangeNotifier = class(TPersistent)
	private
		FOnChange: TNotifyEvent;
	public
		procedure Change;
	public
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
	end;

implementation

{ TThChangeNotifier }

procedure TThChangeNotifier.Change;
begin
	if Assigned(OnChange) then
		OnChange(Self);
end;

end.
