object InspectorForm: TInspectorForm
  Left = 515
  Top = 376
  Width = 250
  Height = 447
  Caption = 'Property Inspector'
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
  object Panel2: TPanel
    Left = 0
    Top = 21
    Width = 242
    Height = 392
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object JvTabBar1: TJvTabBar
      Left = 0
      Top = 0
      Width = 242
      CloseButton = False
      AutoFreeClosed = False
      Margin = 0
      Tabs = <
        item
          Caption = 'Properties '
        end
        item
          Caption = 'PHP'
        end
        item
          Caption = 'JS'
        end>
      Painter = JvModernTabBarPainter1
    end
    object JvInspector1: TJvInspector
      Left = 0
      Top = 23
      Width = 242
      Height = 369
      Align = alClient
      BevelEdges = [beLeft, beRight, beBottom]
      BevelKind = bkFlat
      ItemHeight = 16
      Painter = JvInspectorDotNETPainter1
      AfterItemCreate = JvInspector1AfterItemCreate
      OnDataValueChanged = JvInspector1DataValueChanged
      OnItemValueChanged = JvInspector1ItemValueChanged
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 242
    Height = 21
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      242
      21)
    object ObjectCombo: TLMDObjectComboBox
      Left = 0
      Top = 0
      Width = 243
      Height = 21
      OnCloseUp = ObjectComboCloseUp
      Anchors = [akLeft, akTop, akRight]
      BevelInner = bvNone
      BevelOuter = bvNone
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 0
    end
  end
  object JvInspectorDotNETPainter1: TJvInspectorDotNETPainter
    DrawNameEndEllipsis = False
    Left = 84
    Top = 88
  end
  object JvModernTabBarPainter1: TJvModernTabBarPainter
    TabColor = clWhite
    Color = clBtnFace
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
    SelectedFont.Charset = DEFAULT_CHARSET
    SelectedFont.Color = clWindowText
    SelectedFont.Height = -9
    SelectedFont.Name = 'Small Fonts'
    SelectedFont.Style = []
    Left = 88
    Top = 144
  end
end
