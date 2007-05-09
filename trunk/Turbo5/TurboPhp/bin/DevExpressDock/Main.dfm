object MainForm: TMainForm
  Left = 483
  Top = 236
  Width = 619
  Height = 465
  Caption = 'TurboPhp'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TBXDock1: TTBXDock
    Left = 0
    Top = 0
    Width = 611
    Height = 25
    object TBXToolbar1: TTBXToolbar
      Left = 0
      Top = 0
      Caption = 'TBXToolbar1'
      TabOrder = 0
      object TBXItem5: TTBXItem
        Action = ControllerModule.NewAction
      end
      object TBXItem3: TTBXItem
        Action = ControllerModule.OpenAction
      end
      object TBXItem2: TTBXItem
        Action = ControllerModule.SaveAsAction
      end
      object TBXItem7: TTBXItem
        Action = ControllerModule.CloseAllAction
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
      object TBXSeparatorItem3: TTBXSeparatorItem
      end
      object DockStyleComboItem: TTBXComboBoxItem
        EditWidth = 88
        OnChange = DockStyleComboItemChange
        DropDownList = True
        Strings.Strings = (
          'Standard'
          'Net'
          'Office 11'
          'XP')
      end
      object TBXSeparatorItem4: TTBXSeparatorItem
      end
      object TBXItem6: TTBXItem
        Caption = 'Dock'
      end
    end
  end
  object dxDockSite1: TdxDockSite
    Left = 0
    Top = 25
    Width = 611
    Height = 406
    Align = alClient
    DockType = 0
    OriginalWidth = 611
    OriginalHeight = 406
    object dxLayoutDockSite1: TdxLayoutDockSite
      Left = 185
      Top = 0
      Width = 426
      Height = 406
      DockType = 1
      OriginalWidth = 300
      OriginalHeight = 200
      object dxLayoutDockSite3: TdxLayoutDockSite
        Left = 0
        Top = 0
        Width = 426
        Height = 406
        DockType = 1
        OriginalWidth = 300
        OriginalHeight = 200
      end
      object dxHorizContainerDockSite1: TdxHorizContainerDockSite
        Left = 0
        Top = 0
        Width = 426
        Height = 406
        ActiveChildIndex = -1
        AllowFloating = True
        AutoHide = False
        CaptionButtons = [cbMaximize, cbClose]
        DockType = 1
        OriginalWidth = 185
        OriginalHeight = 140
        object DocumentsDock: TdxDockPanel
          Left = 0
          Top = 0
          Width = 260
          Height = 406
          AllowFloating = True
          AutoHide = False
          Caption = 'Documents'
          CaptionButtons = [cbMaximize, cbClose]
          ShowCaption = False
          DockType = 2
          OriginalWidth = 260
          OriginalHeight = 140
        end
        object ProjectDock: TdxDockPanel
          Left = 260
          Top = 0
          Width = 166
          Height = 406
          AllowFloating = True
          AutoHide = False
          Caption = 'Project'
          CaptionButtons = [cbMaximize, cbClose]
          DockType = 2
          OriginalWidth = 166
          OriginalHeight = 140
        end
      end
    end
    object dxVertContainerDockSite1: TdxVertContainerDockSite
      Left = 0
      Top = 0
      Width = 185
      Height = 406
      ActiveChildIndex = -1
      AllowFloating = True
      AutoHide = False
      CaptionButtons = [cbMaximize, cbClose]
      DockType = 2
      OriginalWidth = 185
      OriginalHeight = 140
      object PaletteDock: TdxDockPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 177
        AllowFloating = True
        AutoHide = False
        Caption = 'Palette'
        CaptionButtons = [cbMaximize, cbClose]
        DockType = 3
        OriginalWidth = 185
        OriginalHeight = 177
      end
      object InspectorDock: TdxDockPanel
        Left = 0
        Top = 177
        Width = 185
        Height = 229
        AllowFloating = True
        AutoHide = False
        Caption = 'Inspector'
        CaptionButtons = [cbMaximize, cbClose]
        DockType = 3
        OriginalWidth = 185
        OriginalHeight = 229
      end
    end
  end
  object TBXSwitcher1: TTBXSwitcher
    Theme = 'Default'
    Left = 64
    Top = 120
  end
  object dxDockingManager1: TdxDockingManager
    Color = clBtnFace
    DefaultHorizContainerSiteProperties.CaptionButtons = [cbMaximize, cbClose]
    DefaultHorizContainerSiteProperties.Dockable = True
    DefaultHorizContainerSiteProperties.ImageIndex = -1
    DefaultVertContainerSiteProperties.CaptionButtons = [cbMaximize, cbClose]
    DefaultVertContainerSiteProperties.Dockable = True
    DefaultVertContainerSiteProperties.ImageIndex = -1
    DefaultTabContainerSiteProperties.CaptionButtons = [cbMaximize, cbClose]
    DefaultTabContainerSiteProperties.Dockable = True
    DefaultTabContainerSiteProperties.ImageIndex = -1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [doActivateAfterDocking, doDblClickDocking, doFloatingOnTop, doTabContainerHasCaption, doTabContainerCanAutoHide, doSideContainerCanClose, doSideContainerCanAutoHide, doTabContainerCanInSideContainer]
    ViewStyle = vsXP
    Left = 64
    Top = 68
  end
end
