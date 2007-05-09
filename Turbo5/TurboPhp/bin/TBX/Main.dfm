object MainForm: TMainForm
  Left = 483
  Top = 236
  Width = 680
  Height = 570
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
  object TBXMultiDock1: TTBXMultiDock
    Left = 0
    Top = 23
    Width = 132
    Height = 504
    Position = dpLeft
    object InspectorPanel: TTBXDockablePanel
      Left = 0
      Top = 244
      Caption = 'Inspector'
      DockPos = 244
      SupportedDocks = [dkStandardDock, dkMultiDock]
      TabOrder = 0
      object InspectorScroll: TTBXPageScroller
        Left = 0
        Top = 0
        Width = 128
        Height = 222
        Align = alClient
        DoubleBuffered = False
        Range = 0
        TabOrder = 0
      end
    end
    object PalettePanel: TTBXDockablePanel
      Left = 0
      Top = 0
      Caption = 'PalettePanel'
      DockPos = 0
      SupportedDocks = [dkStandardDock, dkMultiDock]
      TabOrder = 1
    end
  end
  object ContentPanel: TPanel
    Left = 132
    Top = 23
    Width = 540
    Height = 504
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object JvTabBar1: TJvTabBar
      Left = 0
      Top = 0
      Width = 540
      Tabs = <
        item
          Caption = 'Designer'
        end
        item
          Caption = 'DB Setup'
        end>
    end
  end
  object TBXDock1: TTBXDock
    Left = 0
    Top = 0
    Width = 672
    Height = 23
    object TBXToolbar1: TTBXToolbar
      Left = 0
      Top = 0
      Caption = 'TBXToolbar1'
      TabOrder = 0
      object TBXItem1: TTBXItem
        Action = GenerateAction
      end
    end
  end
  object TBXDock2: TTBXDock
    Left = 0
    Top = 527
    Width = 672
    Height = 9
    Position = dpBottom
  end
  object TBXSwitcher1: TTBXSwitcher
    Theme = 'Stripes'
    Left = 172
    Top = 72
  end
  object ActionList1: TActionList
    Left = 248
    Top = 71
    object GenerateAction: TAction
      Caption = 'Generate'
      OnExecute = GenerateActionExecute
    end
  end
end
