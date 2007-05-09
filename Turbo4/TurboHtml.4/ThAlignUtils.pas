unit ThAlignUtils;

interface

uses
	Controls, ThComponentIterator;

function OverlapX(inI, inJ: TControl): Boolean;
function OverlapY(inI, inJ: TControl): Boolean;

function AdjustX(i, j: TThCtrlIterator): Boolean;
function AdjustY(i, j: TThCtrlIterator): Boolean;

procedure UnoverlapControls(i, j: TThCtrlIterator);

implementation

function OverlapX(inI, inJ: TControl): Boolean;
begin
	Result :=
		((inI.Left < inJ.BoundsRect.Right) and (inJ.Left < inI.BoundsRect.Right))
	or
		((inJ.Left < inI.BoundsRect.Right) and (inI.Left < inJ.BoundsRect.Right));
end;

function OverlapY(inI, inJ: TControl): Boolean;
begin
	Result :=
		((inI.Top < inJ.BoundsRect.Bottom) and (inI.BoundsRect.Bottom > inJ.Top))
	or
		((inJ.Top < inI.BoundsRect.Bottom) and (inJ.BoundsRect.Bottom > inI.Top));
end;

function AdjustX(i, j: TThCtrlIterator): Boolean;
var
	dx, dy: Integer;
begin
	Result := false;
	while i.Next do
	begin
		if (i.Ctrl.Align <> alNone) then
			continue;
		if (i.Ctrl.Left < 0) then
			dx := 1
		else if (i.Ctrl.Left > 0) then
			dx := -1
		else
			dx := 0;
		while (dx < 1) and j.Next do
		begin
			//if (j.Ctrl.Align <> alNone) then
			//	continue;
			if (i.Index = j.Index) then
				continue;
			if OverlapY(i.Ctrl, j.Ctrl) then
			begin
				if (i.Ctrl.Left > j.Ctrl.Left)
					and (i.Ctrl.Left < j.Ctrl.BoundsRect.Right) then
					begin
						if (j.Ctrl.Top >= i.Ctrl.Top) then
							dy := 0
						else
							dy := j.Ctrl.BoundsRect.Bottom - i.Ctrl.Top;
						if (dy <= 0) or (dy > j.Ctrl.BoundsRect.Right - i.Ctrl.Left) then
							dx := 1
						else if (dx = -1) then
							dx := 0;
					end
					else if j.Ctrl.BoundsRect.Right = i.Ctrl.Left then
						dx := 0;
			end;
		end;
		j.Reset;
		if (dx <> 0) then
		begin
			Result := true;
			i.Ctrl.Left := i.Ctrl.Left + dx;
			//i.Ctrl.Update;
			//Sleep(1);
		end;
	end;
end;

function AdjustY(i, j: TThCtrlIterator): Boolean;
var
	dy: Integer;
begin
	Result := false;
	while i.Next do
	begin
		if (i.Ctrl.Align <> alNone) then
			continue;
		if (i.Ctrl.Top < 0) then
			dy := 1
		else if (i.Ctrl.Top > 0) then
			dy := -1
		else
			dy := 0;
		while (dy < 1) and j.Next do
		begin
			//if (j.Ctrl.Align <> alNone) then
			//	continue;
			if (i.Index = j.Index) then
				continue;
			if OverlapX(i.Ctrl, j.Ctrl) then
			begin
				if (i.Ctrl.Top > j.Ctrl.Top)
					and (i.Ctrl.Top < j.Ctrl.BoundsRect.Bottom) then
						dy := 1
					else if j.Ctrl.BoundsRect.Bottom = i.Ctrl.Top then
						dy := 0;
			end;
		end;
		j.Reset;
		if (dy <> 0) then
		begin
			Result := true;
			i.Ctrl.Top := i.Ctrl.Top + dy;
			//i.Ctrl.Update;
			//Sleep(1);
		end;
	end;
end;

procedure UnoverlapControls(i, j: TThCtrlIterator);
var
	dx, dy: Integer;
	work: Boolean;
begin
	repeat
		work := false;
		while i.Next do
		begin
			j.Reset;
			while j.Next do
			begin
				if i.Index = j.Index then
					continue;
				if OverlapX(i.Ctrl, j.Ctrl) and OverlapY(i.Ctrl, j.Ctrl) then
				begin
					dx := j.Ctrl.BoundsRect.Right - i.Ctrl.Left;
					dy := j.Ctrl.BoundsRect.Bottom - i.Ctrl.Top;
					if (dx > 0) or (dy > 0) then
					begin
						work := true;
						if (abs(dx) < abs(dy)) then
						begin
							if dx > 0 then
								i.Ctrl.Left := i.Ctrl.Left + dx //1
						end else
							if dy > 0 then
								i.Ctrl.Top := i.Ctrl.Top + dy; //1;
					end;
				end; // Overlap
			end; // j.Next
		end; // i.Next
	until not work;
end;

end.
