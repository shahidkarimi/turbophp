object MainForm: TMainForm
  Left = 331
  Top = 110
  Width = 610
  Height = 379
  Caption = 'MainForm'
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
  object Splitter1: TSplitter
    Left = 201
    Top = 21
    Height = 324
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 602
    Height = 21
    AutoSize = True
    ButtonHeight = 19
    ButtonWidth = 42
    Caption = 'ToolBar1'
    EdgeBorders = [ebBottom]
    Flat = True
    List = True
    ShowCaptions = True
    TabOrder = 0
    Wrapable = False
    object SelectButton: TToolButton
      Left = 0
      Top = 0
      AutoSize = True
      Caption = 'Select'
      Down = True
      Grouped = True
      ImageIndex = 2
      Style = tbsCheck
      OnClick = SelectButtonClick
    end
    object ToolButton1: TToolButton
      Left = 45
      Top = 0
      AutoSize = True
      Caption = 'Label'
      Grouped = True
      ImageIndex = 0
      Style = tbsCheck
      OnClick = ToolButton1Click
    end
    object ToolButton2: TToolButton
      Left = 86
      Top = 0
      AutoSize = True
      Caption = 'Panel'
      Grouped = True
      ImageIndex = 1
      Style = tbsCheck
      OnClick = ToolButton2Click
    end
    object ToolButton3: TToolButton
      Left = 128
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object ActiveButton: TToolButton
      Left = 136
      Top = 0
      AutoSize = True
      Caption = 'Active'
      Down = True
      ImageIndex = 2
      Style = tbsCheck
      OnClick = ActiveButtonClick
    end
  end
  object DesignScrollBox1: TDesignScrollBox
    Left = 204
    Top = 21
    Width = 398
    Height = 324
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 1
  end
  object JvInspector1: TJvInspector
    Left = 0
    Top = 21
    Width = 201
    Height = 324
    Align = alLeft
    Divider = 100
    ItemHeight = 16
    Painter = JvInspectorDotNETPainter1
    OnDataValueChanged = JvInspector1DataValueChanged
  end
  object JvInspectorDotNETPainter1: TJvInspectorDotNETPainter
    DrawNameEndEllipsis = False
    Left = 217
    Top = 41
  end
end
