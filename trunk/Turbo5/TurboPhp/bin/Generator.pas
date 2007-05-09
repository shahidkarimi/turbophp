unit Generator;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls, ComCtrls, ToolWin, StdCtrls,
	LrTagParser,
	htColumns, htRows, htLayout, htAutoTable, htDocument, htGenerator,
	htWebControl;

type
	// This is a stupid class
	TGenerator = class
	private
		FDocument: ThtDocument;
		FContent: TWinControl;
		FHtml: TStringList;
		HtmlGenerator: ThtGenerator;
	protected
		procedure FormatHtml;
		procedure SetContent(const Value: TWinControl);
		procedure SetHtml(const Value: TStringList);
	public
		constructor Create;
		destructor Destroy; override;
		procedure GenerateDocument;
		procedure GenerateHtml;
		property Content: TWinControl read FContent write SetContent;
		property Document: ThtDocument read FDocument;
		property Html: TStringList read FHtml write SetHtml;
	end;

implementation

uses
	ShellApi,
	Globals;

{ TGenerator }

constructor TGenerator.Create;
begin
	FHtml := TStringList.Create;
	HtmlGenerator := ThtGenerator.Create;
	//HtmlGenerator.OnGenerateCtrl := GenerateCtrl;
	FDocument := ThtDocument.Create;
end;

destructor TGenerator.Destroy;
begin
	Document.Free;
	HtmlGenerator.Free;
	Html.Free;
	inherited;
end;

procedure TGenerator.SetContent(const Value: TWinControl);
begin
	FContent := Value;
end;

procedure TGenerator.SetHtml(const Value: TStringList);
begin
	FHtml.Assign(Value);
end;

procedure TGenerator.GenerateDocument;
begin
	Document.Free;
	FDocument := ThtDocument.Create;
	HtmlGenerator.Generate(Content, Document);
end;

procedure TGenerator.GenerateHtml;
begin
	Html.Clear;
	Document.Build(Html);
	FormatHtml;
end;

procedure TGenerator.FormatHtml;
var
	tags: TLrTaggedDocument;
begin
	tags := TLrTaggedDocument.Create;
	try
		tags.Text := Html.Text;
		Html.Clear;
		tags.Indent(Html);
	finally
		tags.Free;
	end;
end;

end.
