object MainForm: TMainForm
  Left = 483
  Top = 236
  Width = 619
  Height = 465
  Caption = 'MainForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TBXDock1: TTBXDock
    Left = 0
    Top = 0
    Width = 611
    Height = 23
    object TBXToolbar1: TTBXToolbar
      Left = 0
      Top = 0
      Caption = 'TBXToolbar1'
      TabOrder = 0
      object TBXItem3: TTBXItem
        Action = ControllerModule.LoadAction
      end
      object TBXItem2: TTBXItem
        Action = ControllerModule.SaveAsAction
      end
      object TBXSeparatorItem1: TTBXSeparatorItem
      end
      object TBXItem1: TTBXItem
        Action = ControllerModule.GenerateAction
      end
      object TBXSeparatorItem2: TTBXSeparatorItem
      end
      object TBXItem4: TTBXItem
        Action = ControllerModule.SetupDatabaseAction
      end
    end
  end
  object TBXMultiDock1: TTBXMultiDock
    Left = 0
    Top = 23
    Width = 132
    Height = 408
    Position = dpLeft
    object PaletteDockable: TTBXDockablePanel
      Left = 0
      Top = 0
      Caption = 'Palette'
      DockPos = 0
      SplitHeight = 195
      SupportedDocks = [dkStandardDock, dkMultiDock]
      TabOrder = 0
    end
    object InspectorDockable: TTBXDockablePanel
      Left = 0
      Top = 195
      Caption = 'Inspector'
      DockPos = 195
      SplitHeight = 197
      SupportedDocks = [dkStandardDock, dkMultiDock]
      TabOrder = 1
    end
  end
  object TBXMultiDock2: TTBXMultiDock
    Left = 454
    Top = 23
    Width = 157
    Height = 408
    Position = dpRight
    object ProjectManageDockable: TTBXDockablePanel
      Left = 0
      Top = 0
      Caption = 'Information'
      CaptionRotation = dpcrAlwaysVert
      DockedWidth = 153
      DockPos = 0
      ShowCaptionWhenDocked = False
      SupportedDocks = [dkStandardDock, dkMultiDock]
      TabOrder = 0
      object JvTabBar1: TJvTabBar
        Left = 0
        Top = 0
        Width = 153
        CloseButton = False
        Tabs = <
          item
            Caption = 'Project'
          end
          item
            Caption = 'Help'
          end
          item
            Caption = 'Publish'
          end>
        Painter = JvModernTabBarPainter2
      end
    end
  end
  object DocumentsPanel: TPanel
    Left = 132
    Top = 23
    Width = 322
    Height = 408
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    object DocumentTabs: TJvTabBar
      Left = 0
      Top = 0
      Width = 322
      Tabs = <
        item
          Caption = 'Untitled'
        end>
      Painter = JvModernTabBarPainter1
    end
  end
  object TBXSwitcher1: TTBXSwitcher
    Theme = 'Default'
    Left = 196
    Top = 76
  end
  object JvModernTabBarPainter1: TJvModernTabBarPainter
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    DisabledFont.Charset = DEFAULT_CHARSET
    DisabledFont.Color = clGrayText
    DisabledFont.Height = -11
    DisabledFont.Name = 'MS Sans Serif'
    DisabledFont.Style = []
    Left = 196
    Top = 128
  end
  object JvModernTabBarPainter2: TJvModernTabBarPainter
    TabColor = clWhite
    Color = clBtnFace
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
    Left = 484
    Top = 95
  end
end
