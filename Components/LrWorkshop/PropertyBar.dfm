object PropertyBarForm: TPropertyBarForm
  Left = 331
  Top = 110
  Width = 445
  Height = 70
  Caption = 'PropertyBarForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LMDFontComboBox1: TLMDFontComboBox
    Left = 0
    Top = 8
    Width = 145
    Height = 21
    ExampleText = 'Abc'
    TabOrder = 0
    OnChange = LMDFontComboBox1Change
  end
end
