object AddJsEventForm: TAddJsEventForm
  Left = 293
  Top = 110
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Add JavaScript Event'
  ClientHeight = 122
  ClientWidth = 261
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 20
    Width = 107
    Height = 13
    Caption = 'Add JavaScript event:'
  end
  object EventEdit: TEdit
    Left = 16
    Top = 40
    Width = 225
    Height = 21
    TabOrder = 0
  end
  object OkButton: TButton
    Left = 84
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button1: TButton
    Left = 168
    Top = 72
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
