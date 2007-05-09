unit htInterfaces;

interface

uses
	Classes, Types, Controls,
	htMarkup, htStyle;

type
	IhtGenerator = interface
		['{68C6D14D-FBD4-4CD5-AAEC-ED939492EBDC}']
		procedure Generate(inMarkup: ThtMarkup);
	end;
	//
	IhtComponent = interface
		['{9549CF05-8732-4893-9DB1-8592AC7502D7}']
		function GetPriority: Integer;
		property Priority: Integer read GetPriority;
	end;
	//
	IhtControl = interface
		['{840D6640-4D1A-4F5D-A57C-AF4DD7D4CB52}']
		function GetBoxHeight: Integer;
		function GetBoxRect: TRect;
		function GetBoxWidth: Integer;
		function GetCtrlStyle: ThtStyle;
		function GetStyle: ThtStyle;
		function GetStyleClass: string;
		//
		procedure Generate(const inContainer: string; inMarkup: ThtMarkup);
		procedure SetStyleClass(const Value: string);
		//
		property BoxHeight: Integer read GetBoxHeight;
		property BoxRect: TRect read GetBoxRect;
		property BoxWidth: Integer read GetBoxWidth;	
		property CtrlStyle: ThtStyle read GetCtrlStyle;
		property Style: ThtStyle read GetStyle;
		property StyleClass: string read GetStyleClass write SetStyleClass;
	end;

function htBoxHeight(inCtrl: TControl): Integer;
function htBoxWidth(inCtrl: TControl): Integer;

implementation

uses
	LrVclUtils;

function htBoxHeight(inCtrl: TControl): Integer;
var
	c: IhtControl;
begin
	if LrIsAs(inCtrl, IhtControl, c) then
		Result := c.BoxHeight
	else
		Result := inCtrl.Height;
end;

function htBoxWidth(inCtrl: TControl): Integer;
var
	c: IhtControl;
begin
	if LrIsAs(inCtrl, IhtControl, c) then
		Result := c.BoxWidth
	else
		Result := inCtrl.Width;
end;

end.
