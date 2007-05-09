unit DesignReg;

interface

procedure Register;

implementation

uses
  Classes, DesignSurface;

{$R LrDesignerImages.res }

procedure Register;
begin
  RegisterComponents('LR', [ TDesignSurface, TDesignScrollBox, TDesignPanel ]);
end;

end.
