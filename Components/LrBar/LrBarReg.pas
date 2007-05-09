unit LrBarReg;

interface

procedure Register;

implementation

uses
	Classes, LrBar, LrCollapsable;

procedure Register;
begin
	RegisterComponents('LR', [ TLrBar, TLrGroup, TLrCollapsable ]);
end;

end.
