object MainForm: TMainForm
  Left = 344
  Top = 185
  Width = 688
  Height = 582
  Caption = 'Design Workshop'
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
    Left = 185
    Top = 0
    Width = 4
    Height = 548
  end
  object LeftPanel: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 548
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'Left'
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 0
      Top = 0
      Width = 185
      Height = 4
      Cursor = crVSplit
      Align = alTop
    end
    object Splitter4: TSplitter
      Left = 0
      Top = 4
      Width = 185
      Height = 4
      Cursor = crVSplit
      Align = alTop
    end
  end
  object ClientPanel: TPanel
    Left = 189
    Top = 0
    Width = 491
    Height = 548
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Client'
    TabOrder = 1
    object Splitter3: TSplitter
      Left = 0
      Top = 0
      Width = 491
      Height = 4
      Cursor = crVSplit
      Align = alTop
    end
  end
end
