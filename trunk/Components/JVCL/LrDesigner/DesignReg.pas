{-----------------------------------------------------------------------------

 Least-Resistance Designer Library

 The Initial Developer of the Original Code is Scott J. Miles
	<sjmiles (at) turbophp (dot) com>.
 Portions created by Scott J. Miles are
	Copyright (C) 2005 Least-Resistance Software.
 All Rights Reserved.

-------------------------------------------------------------------------------}

unit DesignReg;

interface

procedure Register;

implementation

uses
	Classes, DesignController, DesignScrollBox;

procedure Register;
begin
	RegisterComponents('LR', [ TDesignController, TDesignScrollBox ]);
end;

end.
