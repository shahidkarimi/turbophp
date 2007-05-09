unit TpModule;

interface

uses
	Windows, Classes, Controls, StdCtrls, ExtCtrls, Graphics, Messages, SysUtils,
	Types, Forms,
	dcfdes,
	ThWebControl, ThTag, ThAttributeList, ThTextPaint, ThCssStyle,
	TpControls, TpAnchor,
	DesignView;

type
	TTpModule = class(TThWebControl) //GraphicControl)
	private
		FModuleForm: TForm;
		FModuleName: string;
		FOnGenerate: TTpEvent;
	protected
		procedure SetModuleName(const Value: string);
	protected
		procedure CellTag(inTag: TThTag); override;
		procedure CreateWnd; override;
		procedure Loaded; override;
		procedure LoadModuleForm;
		procedure Paint; override;
		procedure Tag(inTag: TThTag); override;
	public
		constructor Create(inOwner: TComponent); override;
		property ModuleForm: TForm read FModuleForm write FModuleForm;
	published
		property Align;
		property ModuleName: string read FModuleName write SetModuleName;
		property OnGenerate: TTpEvent read FOnGenerate write FOnGenerate;
		property Style;
		property StyleClass;
		property Visible;
	end;

implementation

uses
	Main;

{ TTpModule }

constructor TTpModule.Create(inOwner: TComponent);
begin
	inherited;
	//Transparent := false;
{
	ModuleForm := TForm.Create(Owner);
	ModuleForm.Name := 'ModuleForm';
	ModuleForm.Parent := Self;
	ModuleForm.Align := alClient;
	ModuleForm.Enabled := false;
	//ModuleForm.Visible := true;
	ModuleForm.Color := clLime;
}
{
	with TPanel.Create(Owner) do
	begin
		Name := 'ModulePanel';
		Parent := Self;
		Align := alClient;
		Enabled := false;
		//ModuleForm.Visible := true;
		Color := clLime;
	end;
}
end;

procedure TTpModule.CreateWnd;
begin
	inherited;
//	ModuleForm.Visible := true;
end;

procedure TTpModule.Loaded;
begin
	inherited;
	LoadModuleForm;
end;

procedure TTpModule.Paint;
var
	c: TColor;
begin
	inherited;
	if ControlCount = 0 then
//	if ShowHash then
		with Canvas do
		begin
			c := Brush.Color;
			Brush.Style := bsDiagCross;
			Brush.Color := ColorToRgb(clSilver);
			Pen.Style := psClear;
			if ThVisibleColor(c) then
				SetBkColor(Handle, ColorToRgb(c))
			else
				SetBkMode(Handle, Windows.TRANSPARENT);
			Rectangle(AdjustedClientRect);
			Brush.Style := bsSolid;
			Pen.Style := psSolid;
			SetBkMode(Handle, Windows.OPAQUE);
		end;
end;

procedure TTpModule.SetModuleName(const Value: string);
begin
	FModuleName := Value;
	if not (csLoading in ComponentState) then
		LoadModuleForm;
end;

{.$define __TEST__}
{$ifdef __TEST__}

procedure TTpModule.LoadModuleForm;

	function GetBinaryStream(const inPath: string): TStream;
	var
		fs: TFileStream;
	begin
		Result := TMemoryStream.Create;
		fs := TFileStream.Create(inPath, fmOpenRead);
		try
			ObjectTextToBinary(fs, Result);
			Result.Seek(0, 0);
		finally
			fs.Free;
		end
	end;

var
	p: string;
	s: TStream;
begin
	p := MainForm.Project.ProjectFolder + ModuleName + '.tphp';
	if FileExists(p) then
	begin
		s := GetBinaryStream(p);
		try
			with TReader.Create(s, 4096) do
			try
				ReadComponents(Owner, Self, nil);
			finally
				Free;
			end;
		finally
			s.Free;
		end;
//		ModuleForm := TForm.Create(Owner);
//		with TDCLiteDesigner.Create(nil) do
//		try
//			LoadFromFile(ModuleForm, p);
//		finally
//			Free;
//		end;
//		ModuleForm.Parent := Self;
//		ModuleForm.Align := alClient;
//		ModuleForm.Enabled := false;
//		ModuleForm.Visible := true;
	end;
end;

{$ELSE}

procedure TTpModule.LoadModuleForm;
var
	p: string;
begin
	p := MainForm.Project.ProjectFolder + ModuleName + '.tphp';
	if FileExists(p) then
	begin
		ModuleForm.Free;
		//Invalidate;
		Application.CreateForm(TDesignForm, FModuleForm);
		TDesignForm(ModuleForm).LoadFromFile(p);
		ModuleForm.Parent := Self;
		ModuleForm.Align := alClient;
		ModuleForm.Enabled := false;
		ModuleForm.Visible := true;
		ModuleForm.Name := Name + 'Objects';
	end;
end;

{$ENDIF+}

procedure TTpModule.CellTag(inTag: TThTag);
begin
	inherited;
	with inTag do
	begin
		//Attributes.Clear;
		Add('id', Name);
		Add(tpClass, 'TTpModule');
		Add('tpName', Name);
		Add('tpModuleName', ModuleName + '.php');
		Add('tpOnGenerate', OnGenerate);
	end;
end;

procedure TTpModule.Tag(inTag: TThTag);
begin
	inherited;
end;

end.
