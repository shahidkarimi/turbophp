object InspectorForm: TInspectorForm
  Left = 719
  Top = 261
  Width = 237
  Height = 459
  Caption = 'InspectorForm'
  Color = clBtnFace
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 229
    Height = 21
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      229
      21)
    object ObjectCombo: TLMDObjectComboBox
      Left = 0
      Top = 0
      Width = 230
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
  object InspectorTabs: TJvTabBar
    Left = 0
    Top = 21
    Width = 229
    CloseButton = False
    AutoFreeClosed = False
    Margin = 0
    Tabs = <
      item
        Caption = 'Properties'
      end>
    Painter = JvModernTabBarPainter1
    OnTabSelected = InspectorTabsTabSelected
  end
  object ComponentInspector1: TComponentInspector
    Left = 0
    Top = 44
    Width = 229
    Height = 381
    Align = alClient
    BorderStyle = bsNone
    Color = clBtnFace
    TabOrder = 2
  end
  object JvModernTabBarPainter2: TJvModernTabBarPainter
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
    Left = 60
    Top = 84
  end
  object JvModernTabBarPainter1: TJvModernTabBarPainter
    Color = clWhite
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
    Left = 60
    Top = 140
  end
end
