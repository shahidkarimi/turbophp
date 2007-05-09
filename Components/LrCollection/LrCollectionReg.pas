unit LrCollectionReg;

interface

uses
	Classes;

procedure Register;

implementation

uses
	LrCollection;

procedure Register;
begin
	RegisterComponents('LR', [ TLrCollectionTest ]);
end;

end.
