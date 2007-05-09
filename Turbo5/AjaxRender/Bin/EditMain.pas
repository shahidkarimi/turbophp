unit EditMain;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls, AppEvnts,
	Node, Box, Flow, EditBox;

type
	TForm2 = class(TForm)
		ApplicationEvents1: TApplicationEvents;
		procedure FormPaint(Sender: TObject);
		procedure FormKeyPress(Sender: TObject; var Key: Char);
		procedure FormDeactivate(Sender: TObject);
		procedure FormActivate(Sender: TObject);
		procedure FormKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure FormCreate(Sender: TObject);
		procedure FormResize(Sender: TObject);
	private
		{ Private declarations }
		procedure AddKey(Key: Char);
		procedure ValidateCursor;
		procedure SetCaret;
		procedure FindCaret;
		procedure PaintBlock;
	public
		{ Public declarations }
		Block: TEditBox;
		Node: TNode;
		Text: string;
		Cursor: Integer;
		CaretPos: TPoint;
	end;

var
	Form2: TForm2;

implementation

uses
	Text;

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
	DoubleBuffered := true;
	Block := TEditBox.Create;
	Node := TNode.Create;
	Node.Text := 'Now is the time for all good men to come to the aid of their party. ';
	Block.AddBox.Node := Node;
	Node := TNode.Create;
	Node.Text := 'The quick brown fox jumped over the lazy dog.';
	Block.AddBox.Node := Node;
	Block.Top := 100;
	Block.Canvas := Canvas;
	Block.WrapLines(ClientWidth);
	FindCaret;
end;

procedure TForm2.FormActivate(Sender: TObject);
begin
	CreateCaret(Handle, 0, 2, 13);
	SetCaret;
	ShowCaret(Handle);
end;

procedure TForm2.FormDeactivate(Sender: TObject);
begin
	HideCaret(Handle);
	DestroyCaret;
end;

procedure TForm2.FormResize(Sender: TObject);
begin
	Block.WrapLines(ClientWidth);
	ValidateCursor;
	FindCaret;
	SetCaret;
	Invalidate;
end;

procedure TForm2.FormPaint(Sender: TObject);
begin
	PaintBlock;
end;

procedure TForm2.FindCaret;
begin
	CaretPos := Block.FindCaret(Cursor);
end;

procedure TForm2.SetCaret;
begin
	SetCaretPos(CaretPos.X, CaretPos.Y);
end;

procedure TForm2.ValidateCursor;
begin
	if Cursor < 0 then
		Cursor := 0
//	else if Cursor > Length(Text) then
//		Cursor := Length(Text);
end;

procedure TForm2.PaintBlock;
begin
	HideCaret(Handle);
	Block.PaintLines;
	ShowCaret(Handle);
end;

procedure TForm2.AddKey(Key: Char);
begin
	case Key of
		#$08: // backspace
		begin
			with Block, Block.CursorBox.Node do
				Text := Copy(Text, 1, CursorIndex - 1) + Copy(Text, CursorIndex + 1, MAXINT);
			Dec(Cursor);
		end;
		#$01:; // linefeed
		#$1b:; // escape
		#$09:; // tab
		#$0D:; // return
		else
		begin
			with Block, Block.CursorBox.Node do
				Text := Copy(Text, 1, CursorIndex) + Key + Copy(Text, CursorIndex + 1, MAXINT);
			Inc(Cursor);
		end;
	end;
	Block.WrapLines(ClientWidth);
	ValidateCursor;
	FindCaret;
	SetCaret;
	Repaint;
end;

procedure TForm2.FormKeyPress(Sender: TObject; var Key: Char);
begin
	Block.FindCursor(Self.Cursor);
	if Block.CursorBox <> nil then
		AddKey(Key);
end;

procedure TForm2.FormKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	case Key of
		VK_HOME: Cursor := 0;
		VK_END: Cursor := MAXINT;
		VK_LEFT: Dec(Cursor);
		VK_RIGHT: Inc(Cursor);
		else exit;
	end;
	ValidateCursor;
	FindCaret;
	SetCaret;
end;

end.
