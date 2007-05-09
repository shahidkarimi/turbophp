object DesignHostForm: TDesignHostForm
  Left = 330
  Top = 114
  Width = 698
  Height = 502
  Caption = 'DesignHostForm'
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object LeftRuler: TRsRuler
    Left = 0
    Top = 46
    Width = 22
    Height = 422
    Units = ruPixel
    Flat = True
    ScaleColor = clWindow
    TickColor = clWindowText
    VersionInfo = 'Version 4.0 (c) Roos Software 2003'
    Direction = rdLeft
    ScaleDir = rsdNormal
    Scale = 100
    HairLine = False
    HairLinePos = -1
    HairLineStyle = hlsLine
    Offset = -12.000000000000000000
    ShowMinus = True
    Align = alLeft
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Color = 14285307
    ParentFont = False
  end
  object RulerPanel: TPanel
    Left = 0
    Top = 24
    Width = 690
    Height = 22
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object RsRuler1: TRsRuler
      Left = 22
      Top = 0
      Width = 668
      Height = 22
      Units = ruPixel
      Flat = True
      ScaleColor = clWindow
      TickColor = clWindowText
      VersionInfo = 'Version 4.0 (c) Roos Software 2003'
      Direction = rdTop
      ScaleDir = rsdNormal
      Scale = 100
      HairLine = False
      HairLinePos = -1
      HairLineStyle = hlsLine
      Offset = -12.000000000000000000
      ShowMinus = True
      Align = alClient
      Color = 14285307
    end
    object RsRulerCorner1: TRsRulerCorner
      Left = 0
      Top = 0
      Width = 22
      Height = 22
      Hint = 'pixel'
      Units = ruPixel
      Flat = True
      ScaleColor = clWindow
      TickColor = clWindowText
      VersionInfo = 'Version 4.0 (c) Roos Software 2003'
      Align = alLeft
      Position = cpLeftTop
      Color = 14285307
    end
  end
  object Scroller: TDesignScrollBox
    Left = 22
    Top = 46
    Width = 668
    Height = 422
    HorzScrollBar.Smooth = True
    HorzScrollBar.Style = ssFlat
    HorzScrollBar.Tracking = True
    VertScrollBar.Smooth = True
    VertScrollBar.Style = ssFlat
    VertScrollBar.Tracking = True
    Align = alClient
    BevelKind = bkTile
    BorderStyle = bsNone
    TabOrder = 1
    object BackPanel: TPanel
      Left = 0
      Top = 0
      Width = 581
      Height = 353
      BevelOuter = bvNone
      Color = 15987699
      TabOrder = 0
    end
  end
  object dxBarManager1: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Bars = <
      item
        AllowClose = False
        AllowCustomizing = False
        AllowQuickCustomizing = False
        AllowReset = False
        Caption = 'Custom 1'
        DockedDockingStyle = dsTop
        DockedLeft = 0
        DockedTop = 0
        DockingStyle = dsTop
        FloatLeft = 280
        FloatTop = 114
        FloatClientWidth = 0
        FloatClientHeight = 0
        ItemLinks = <
          item
            Item = WidthSpin
            UserDefine = [udWidth]
            UserWidth = 70
            Visible = True
          end
          item
            Item = HeightSpin
            UserDefine = [udWidth]
            UserWidth = 71
            Visible = True
          end>
        Name = 'Custom 1'
        NotDocking = [dsNone, dsLeft, dsRight, dsBottom]
        OneOnRow = True
        Row = 0
        SizeGrip = False
        UseOwnFont = False
        UseRestSpace = True
        Visible = True
        WholeRow = False
      end>
    Categories.Strings = (
      'Default')
    Categories.ItemsVisibles = (
      2)
    Categories.Visibles = (
      True)
    PopupMenuLinks = <>
    Style = bmsXP
    UseSystemFont = True
    Left = 70
    Top = 62
    DockControlHeights = (
      0
      0
      24
      0)
    object dxBarCombo1: TdxBarCombo
      Caption = 'New Item'
      Category = 0
      Hint = 'New Item'
      Visible = ivAlways
      Width = 100
      ItemIndex = -1
    end
    object dxBarCombo2: TdxBarCombo
      Caption = 'New Item'
      Category = 0
      Hint = 'New Item'
      Visible = ivAlways
      Width = 100
      ItemIndex = -1
    end
    object WidthSpin: TdxBarSpinEdit
      Caption = 'New Item'
      Category = 0
      Hint = 'Page Width'
      Visible = ivAlways
      OnChange = SizeSpinChange
      Width = 100
      Increment = 4.000000000000000000
      Prefix = ' px'
      Value = 800.000000000000000000
    end
    object HeightSpin: TdxBarSpinEdit
      Caption = 'New Item'
      Category = 0
      Hint = 'Page Height'
      Visible = ivAlways
      OnChange = SizeSpinChange
      Width = 100
      Prefix = ' px'
      Value = 600.000000000000000000
    end
    object dxBarStatic1: TdxBarStatic
      Category = 0
      Visible = ivAlways
    end
  end
end
