object CellExpertForm: TCellExpertForm
  Left = 293
  Top = 110
  Width = 151
  Height = 161
  Caption = 'CellExpertForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object UpDown1: TUpDown
    Left = 60
    Top = 40
    Width = 21
    Height = 45
    Min = -1000
    Max = 1000
    TabOrder = 0
    OnClick = UpDown1Click
  end
  object Button1: TButton
    Left = 12
    Top = 48
    Width = 42
    Height = 25
    Caption = 'Left'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 88
    Top = 48
    Width = 42
    Height = 25
    Caption = 'Right'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 48
    Top = 8
    Width = 42
    Height = 25
    Caption = 'Top'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 48
    Top = 92
    Width = 42
    Height = 25
    Caption = 'Bottom'
    TabOrder = 4
    OnClick = Button4Click
  end
end
