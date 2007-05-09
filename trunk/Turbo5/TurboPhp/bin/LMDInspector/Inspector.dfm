object InspectorForm: TInspectorForm
  Left = 331
  Top = 114
  Width = 248
  Height = 378
  Caption = 'InspectorForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 240
    Height = 21
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      240
      21)
    object LMDObjectComboBox1: TLMDObjectComboBox
      Left = 0
      Top = 0
      Width = 241
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BevelInner = bvLowered
      BevelOuter = bvNone
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 21
    Width = 240
    Height = 323
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object JvTabBar1: TJvTabBar
      Left = 0
      Top = 0
      Width = 240
      CloseButton = False
      AutoFreeClosed = False
      Margin = 0
      Tabs = <
        item
          Caption = 'Properties'
        end
        item
          Caption = 'PHP'
        end
        item
          Caption = 'JS'
        end>
      Painter = JvModernTabBarPainter2
    end
    object LMDSimplePanel1: TLMDSimplePanel
      Left = 0
      Top = 23
      Width = 240
      Height = 300
      Align = alClient
      Bevel.BorderColor = clBtnText
      Bevel.BorderSides = [fsBottom, fsLeft, fsRight]
      Bevel.Mode = bmEdge
      TabOrder = 1
      object LMDPropertyInspector1: TLMDPropertyInspector
        Left = 2
        Top = 2
        Width = 236
        Height = 296
        CategoryFont.Charset = DEFAULT_CHARSET
        CategoryFont.Color = clHighlightText
        CategoryFont.Height = -11
        CategoryFont.Name = 'MS Sans Serif'
        CategoryFont.Style = [fsBold]
        Splitter = 60
        Align = alClient
        BorderStyle = bsNone
        Color = clWhite
        Ctl3D = True
        TabOrder = 0
      end
    end
  end
  object JvModernTabBarPainter1: TJvModernTabBarPainter
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Small Fonts'
    Font.Style = []
    DisabledFont.Charset = DEFAULT_CHARSET
    DisabledFont.Color = clGrayText
    DisabledFont.Height = -9
    DisabledFont.Name = 'Small Fonts'
    DisabledFont.Style = []
    Left = 100
    Top = 93
  end
  object JvModernTabBarPainter2: TJvModernTabBarPainter
    TabColor = clWhite
    Color = clLime
    ControlDivideColor = clSilver
    CloseColor = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Small Fonts'
    Font.Style = []
    DisabledFont.Charset = DEFAULT_CHARSET
    DisabledFont.Color = clGrayText
    DisabledFont.Height = -9
    DisabledFont.Name = 'Small Fonts'
    DisabledFont.Style = []
    Left = 104
    Top = 144
  end
end
