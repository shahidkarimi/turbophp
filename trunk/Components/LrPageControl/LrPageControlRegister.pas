unit LrPageControlRegister;

interface

procedure Register;

implementation

uses
	Classes, LrPageControl;

procedure Register;
begin
	RegisterComponents('LR', [ TLrTabControl, TLrCustomPageControl, TLrPageControl ]);
end;

end.
