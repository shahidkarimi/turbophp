{-----------------------------------------------------------------------------

 Least-Resistance Designer Library

 The Initial Developer of the Original Code is Scott J. Miles
	<sjmiles (at) turbophp (dot) com>.
 Portions created by Scott J. Miles are
	Copyright (C) 2005 Least-Resistance Software.
 All Rights Reserved.

-------------------------------------------------------------------------------}

unit DesignScrollBox;

interface

uses
	Controls, Forms;

type
	TDesignScrollBox = class(TScrollBox)
	protected
		procedure AutoScrollInView(AControl: TControl); override;
	end;

implementation

procedure TDesignScrollBox.AutoScrollInView(AControl: TControl);
begin
	//
end;

end.
