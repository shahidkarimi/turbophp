unit Design;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs,
	DesignController, DesignSurface;

type
	TDesignForm = class(TForm)
		DesignController: TDesignController;
		procedure FormPaint(Sender: TObject);
	private
		{ Private declarations }
	protected
		procedure ReaderError(Reader: TReader; const Message: string;
			var Handled: Boolean);
	public
		{ Public declarations }
		function CreateSelectionList: TList;
		procedure LoadFromFile(const inFilename: string);
		procedure SaveToFile(const inFilename: string);
	end;

var
	DesignForm: TDesignForm;

implementation

uses
	Utils;

{$R *.dfm}

procedure TDesignForm.FormPaint(Sender: TObject);
begin
	PaintRules(Canvas, ClientRect);
end;

function TDesignForm.CreateSelectionList: TList;
var
	i: Integer;
begin
	Result := TList.Create;
	for i := 0 to Pred(DesignController.Count) do
		Result.Add(DesignController.Selection[i]);
end;

procedure TDesignForm.ReaderError(Reader: TReader; const Message: string;
	var Handled: Boolean);
begin
	Handled := true;
end;

procedure TDesignForm.LoadFromFile(const inFilename: string);
begin
	DesignController.Free;
	LoadComponentFromFile(Self, inFilename, ReaderError);
	DesignController.Active := true;
end;

procedure TDesignForm.SaveToFile(const inFilename: string);
begin
	SaveComponentToFile(Self, inFilename);
end;

end.
