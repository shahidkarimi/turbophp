unit HtmlEditView;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, Menus, StdCtrls, ExtCtrls,
	dcstring, dcsystem, dcparser, dccommon, dcmemo,
	EasyClasses, EasyParser, EasyEditor, EasyEditSource, {EasyEditorActions,}
	CodeExplorerView;

type
	THtmlEditForm = class(TForm)
    Source: TEasyEditSource;
    Edit: TEasyEdit;
		HtmlParser: TEasyEditorParser;
		procedure EditSourceChanged(Sender: TObject;
			State: TEasyEditSourceStates);
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
		FOnModified: TNotifyEvent;
	protected
		function GetLines: TStrings;
		procedure SetLines(const Value: TStrings);
	protected
		procedure Modified;
	public
		{ Public declarations }
		property Lines: TStrings read GetLines write SetLines;
		property OnModified: TNotifyEvent read FOnModified write FOnModified;
	end;

implementation

{uses
	Utils;}

{$R *.dfm}

procedure THtmlEditForm.FormCreate(Sender: TObject);
begin
	Source.Strings.Clear;
end;

function THtmlEditForm.GetLines: TStrings;
begin
	Result := Source.Strings;
end;

procedure THtmlEditForm.SetLines(const Value: TStrings);
begin
	Source.Strings.Assign(Value);
end;

procedure THtmlEditForm.Modified;
begin
	if Assigned(OnModified) then
		OnModified(Self);
end;

procedure THtmlEditForm.EditSourceChanged(Sender: TObject;
	State: TEasyEditSourceStates);
begin
	if (State <> [csPositionChanged]) and (Edit.Modified) then
		Modified;
end;

end.
