unit DesignScrollBox;

interface

uses
	Classes, Controls, Forms, Graphics;

type
	TDesignScrollBox = class(TScrollBox)
		constructor Create(inOwner: TComponent); override;
		procedure AutoScrollInView(AControl: TControl); override;
	end;

implementation

{ TDesignScrollBox }

constructor TDesignScrollBox.Create(inOwner: TComponent);
begin
	inherited;
	Align := alClient;
	HorzScrollBar.Tracking := true;
	VertScrollBar.Tracking := true;
	BevelInner := bvNone;
	BevelOuter := bvNone;
	BorderStyle := bsNone;
	Color := clWhite;
end;

procedure TDesignScrollBox.AutoScrollInView(AControl: TControl);
begin
	//
end;

end.
