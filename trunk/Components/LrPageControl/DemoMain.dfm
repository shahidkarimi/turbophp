object DemoMainForm: TDemoMainForm
  Left = 293
  Top = 110
  Width = 581
  Height = 610
  Caption = 'DemoMainForm'
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
  object LrTabControl1: TLrTabControl
    Left = 0
    Top = 215
    Width = 573
    Height = 21
    Align = alTop
    TabHeight = 21
    Tabs.Strings = (
      'Alpha'
      'Beta'
      'Gamma')
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 573
    Height = 22
    AutoSize = True
    ButtonWidth = 64
    Caption = 'ToolBar1'
    EdgeBorders = []
    Flat = True
    TabOrder = 0
    OnCustomDrawButton = ToolBar1CustomDrawButton
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Caption = 'ToolButton1'
      ImageIndex = 0
      OnClick = ToolButton1Click
    end
    object ToolButton2: TToolButton
      Left = 64
      Top = 0
      Caption = 'ToolButton2'
      ImageIndex = 1
      OnClick = ToolButton2Click
    end
    object ToolButton3: TToolButton
      Left = 128
      Top = 0
      Caption = 'ToolButton3'
      ImageIndex = 2
    end
    object ToolButton4: TToolButton
      Left = 192
      Top = 0
      Caption = 'ToolButton4'
      ImageIndex = 3
    end
  end
  object Pages: TLrPageControl
    Left = 0
    Top = 328
    Width = 573
    Height = 248
    ActivePageIndex = 0
    Align = alClient
    Count = 4
    object LrTabSheet2: TLrTabSheet
      Left = 0
      Top = 18
      Width = 573
      Height = 230
      Align = alClient
      Caption = 'LrTabSheet2'
      Color = clWhite
    end
    object LrTabSheet1: TLrTabSheet
      Left = 0
      Top = 18
      Width = 573
      Height = 230
      Align = alClient
      Caption = 'LrTabSheet1'
      Color = 16710908
      object Button1: TButton
        Left = 40
        Top = 48
        Width = 75
        Height = 25
        Caption = 'Button1'
        TabOrder = 0
      end
    end
    object LrTabSheet3: TLrTabSheet
      Left = 0
      Top = 18
      Width = 573
      Height = 230
      Align = alClient
      Caption = 'LrTabSheet3'
      Color = clWhite
    end
    object LrTabSheet4: TLrTabSheet
      Left = 0
      Top = 18
      Width = 573
      Height = 230
      Align = alClient
      Caption = 'LrTabSheet4'
      Color = clWhite
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 236
    Width = 573
    Height = 92
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 2
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 22
    Width = 573
    Height = 193
    ActivePage = TabSheet1
    Align = alTop
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
    end
  end
end
