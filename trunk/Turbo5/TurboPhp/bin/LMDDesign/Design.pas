unit Design;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	LMDDsgClass, LMDDsgDesigner, LMDDsgManager;

type
	TDesignForm = class(TForm)
		LMDDesigner1: TLMDDesigner;
		LMDDesignManager1: TLMDDesignManager;
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
		FOnSelectionChanged: TNotifyEvent;
    FOnChange: TNotifyEvent;
		function GetActive: Boolean;
		function GetSelectedComponent: TComponent;
		procedure SetActive(const Value: Boolean);
		procedure SetOnSelectionChanged(const Value: TNotifyEvent);
		procedure SetSelectedComponent(const Value: TComponent);
	public
		{ Public declarations }
		procedure LoadFromFile(const inFilename: string);
		procedure SaveToFile(const inFilename: string);
		property Active: Boolean read GetActive write SetActive;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
		property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged
			write SetOnSelectionChanged;
		property SelectedComponent: TComponent read GetSelectedComponent
			write SetSelectedComponent;
	end;

var
	DesignForm: TDesignForm;

implementation

{$R *.dfm}

procedure TDesignForm.FormCreate(Sender: TObject);
begin
	LMDDesigner1.DesignControl := Self;
end;

function TDesignForm.GetActive: Boolean;
begin
	Result := LMDDesigner1.Active;
end;

procedure TDesignForm.SetActive(const Value: Boolean);
begin
	LMDDesigner1.Active := Value;
end;

function TDesignForm.GetSelectedComponent: TComponent;
begin
	Result := nil;
end;

procedure TDesignForm.SetSelectedComponent(const Value: TComponent);
begin
	//
end;

procedure TDesignForm.SetOnSelectionChanged(const Value: TNotifyEvent);
begin
	FOnSelectionChanged := Value;
end;

procedure TDesignForm.LoadFromFile(const inFilename: string);
begin
	//
end;

procedure TDesignForm.SaveToFile(const inFilename: string);
begin
	//
end;

end.
