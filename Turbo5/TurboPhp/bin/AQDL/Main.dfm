object MainForm: TMainForm
  Left = 483
  Top = 236
  Width = 619
  Height = 465
  Caption = 'TurboAjax for PHP'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TBXDock1: TTBXDock
    Left = 0
    Top = 0
    Width = 611
    Height = 49
    object TBXToolbar1: TTBXToolbar
      Left = 0
      Top = 0
      Caption = 'TBXToolbar1'
      DockPos = 0
      Images = TBImageList1
      TabOrder = 0
      object TBXSubmenuItem1: TTBXSubmenuItem
        Caption = 'File'
        object TBXItem14: TTBXItem
          Action = ControllerModule.NewTurboAjaxAction
        end
        object TBXItem17: TTBXItem
          Action = ControllerModule.NewTurboPhpAction
        end
        object TBXItem9: TTBXItem
          Action = ControllerModule.OpenAction
        end
        object TBXItem10: TTBXItem
          Action = ControllerModule.SaveAction
          Caption = '&Save'
        end
        object TBXItem11: TTBXItem
          Action = ControllerModule.SaveAsAction
        end
        object TBXSeparatorItem3: TTBXSeparatorItem
        end
        object TBItem1: TTBItem
          Action = ControllerModule.CloseProjectAction
        end
        object TBXItem12: TTBXItem
          Action = ControllerModule.SaveProjectAction
        end
        object TBXItem13: TTBXItem
          Action = ControllerModule.SaveProjectAsAction
        end
        object TBXSeparatorItem4: TTBXSeparatorItem
        end
        object TBXItem15: TTBXItem
          Action = ControllerModule.CloseAllAction
        end
      end
      object TBXSubmenuItem2: TTBXSubmenuItem
        Caption = 'Project'
        object TBXItem1: TTBXItem
          Action = ControllerModule.PublishAction
        end
        object TBXSeparatorItem1: TTBXSeparatorItem
        end
        object TBXItem16: TTBXItem
          Action = ControllerModule.SetupServersAction
        end
        object TBXItem8: TTBXItem
          Action = ControllerModule.SetupDatabasesAction
        end
      end
    end
    object TBXToolbar2: TTBXToolbar
      Left = 0
      Top = 23
      Caption = 'TBXToolbar2'
      DockRow = 1
      Images = TBImageList1
      TabOrder = 1
      object TBXItem2: TTBXItem
        Action = ControllerModule.NewTurboAjaxAction
        DisplayMode = nbdmTextOnlyInMenus
        ImageIndex = 0
      end
      object TBXItem3: TTBXItem
        Action = ControllerModule.OpenAction
      end
      object TBXItem5: TTBXItem
        Action = ControllerModule.SaveAction
      end
      object TBXSeparatorItem5: TTBXSeparatorItem
      end
      object TBXItem6: TTBXItem
        Action = ControllerModule.PublishAction
      end
      object TBXSeparatorItem6: TTBXSeparatorItem
      end
      object TBXItem4: TTBXItem
        Action = ControllerModule.SetupServersAction
      end
      object TBXItem7: TTBXItem
        Action = ControllerModule.SetupDatabasesAction
      end
    end
  end
  object DocumentsPanel: TPanel
    Left = 0
    Top = 72
    Width = 611
    Height = 359
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object aqDockingSite1: TaqDockingSite
      Left = 0
      Top = 0
      Width = 611
      Height = 359
      Align = alClient
      DockingManager = aqDockingManager1
      Key = '{6F28736B-9286-4B12-9A2E-726493309901}'
    end
  end
  object TBXSwitcher1: TTBXSwitcher
    Theme = 'Default'
    Left = 172
    Top = 80
  end
  object aqDockingManager1: TaqDockingManager
    AutoDragKey = [dssCtrl, dssLeft]
    DockingStyleClassName = 'VS2005'
    DockingStyle.TabOrientation = dtoBottom
    DockingStyle.ActiveDockZone.ActiveTransparency = 255
    DockingStyle.ActiveDockZone.FadeInDelay = 200
    DockingStyle.ActiveDockZone.FadeOutDelay = 250
    DockingStyle.ActiveDockZone.Transparency = 128
    DockingStyle.DockingFrame.Brush.Color = clHighlight
    DockingStyle.DockZone.ActiveTransparency = 128
    DockingStyle.DockZone.FadeInDelay = 50
    DockingStyle.DockZone.FadeOutDelay = 1000
    DockingStyle.DockZone.Transparency = 0
    ShowImages = [ctCaption, ctTab]
    StoreOptions = [dsoFormPosition, dsoFormState, dsoFormConstraints]
    StyleManager = aqStyleManager1
    Left = 80
    Top = 79
    Style = MainForm.aqStyleManager1.aqThemedUIStyle1
    object PaletteDock: TaqDockingControl
      Width = 611
      Height = 317
      Caption = 'Palette'
      ImageIndex = -1
      MinWidth = 0
      MinHeight = 0
      ShowCaption = bvDefault
      ShowImage = bvDefault
      Visible = True
      Key = '{4263E1B0-B7AF-4BB5-B2C5-5326C1F374E3}'
      DesignClientHeight = 317
      DesignClientWidth = 611
      object dxBarDockControl1: TdxBarDockControl
        Left = 0
        Top = 20
        Width = 611
        Height = 3
        Align = dalTop
        BarManager = dxBarManager1
      end
    end
    object InspectorDock: TaqDockingControl
      Width = 611
      Height = 317
      Caption = 'Inspector'
      ImageIndex = -1
      MinWidth = 0
      MinHeight = 0
      ShowCaption = bvDefault
      ShowImage = bvDefault
      Visible = True
      OnEnter = InspectorDockEnter
      OnExit = InspectorDockExit
      Key = '{EF9BC418-7B00-46E5-A4FD-C203E142D047}'
      DesignClientHeight = 317
      DesignClientWidth = 611
    end
    object DocumentsDock: TaqDockingControl
      Width = 611
      Height = 317
      Caption = 'Documents'
      ImageIndex = -1
      MinWidth = 0
      MinHeight = 0
      ShowCaption = bvFalse
      ShowImage = bvDefault
      Visible = True
      Key = '{288C089E-38C5-40F3-99A7-D3D60E4A23CF}'
      DesignClientHeight = 317
      DesignClientWidth = 611
    end
    object ProjectDock: TaqDockingControl
      Width = 611
      Height = 317
      Caption = 'Project'
      ImageIndex = -1
      MinWidth = 0
      MinHeight = 0
      ShowCaption = bvDefault
      ShowImage = bvDefault
      Visible = False
      Key = '{0C1697D0-CBCB-4FA9-89E2-8716F46000B7}'
      DesignClientHeight = 317
      DesignClientWidth = 611
    end
    object ComponentsDock: TaqDockingControl
      Width = 611
      Height = 317
      Caption = 'Components'
      ImageIndex = -1
      MinWidth = 0
      MinHeight = 0
      ShowCaption = bvDefault
      ShowImage = bvDefault
      Visible = False
      Key = '{BD4FEC01-1477-4171-87CA-29B7666ABC31}'
      DesignClientHeight = 317
      DesignClientWidth = 611
    end
  end
  object aqStyleManager1: TaqStyleManager
    Left = 80
    Top = 127
    object aqDefaultUIStyle1: TaqDefaultUIStyle
      ActiveCaptionColor.Bands = 256
      ActiveCaptionColor.EndColor = clWindow
      ActiveCaptionColor.FillType = gtSolid
      ActiveCaptionColor.StartColor = clBtnFace
      ActiveCaptionFont.Charset = DEFAULT_CHARSET
      ActiveCaptionFont.Color = clWindowText
      ActiveCaptionFont.Height = -11
      ActiveCaptionFont.Name = 'MS Sans Serif'
      ActiveCaptionFont.Style = []
      ActiveHiddenTabColor.Bands = 256
      ActiveHiddenTabColor.EndColor = clWindow
      ActiveHiddenTabColor.FillType = gtSolid
      ActiveHiddenTabColor.StartColor = clBtnFace
      ActiveHiddenTabFont.Charset = DEFAULT_CHARSET
      ActiveHiddenTabFont.Color = clWindowText
      ActiveHiddenTabFont.Height = -11
      ActiveHiddenTabFont.Name = 'MS Shell Dlg 2'
      ActiveHiddenTabFont.Style = []
      ActiveTabColor.Bands = 256
      ActiveTabColor.EndColor = clWindow
      ActiveTabColor.FillType = gtSolid
      ActiveTabColor.StartColor = clBtnFace
      ActiveTabFont.Charset = DEFAULT_CHARSET
      ActiveTabFont.Color = clWindowText
      ActiveTabFont.Height = -11
      ActiveTabFont.Name = 'MS Sans Serif'
      ActiveTabFont.Style = []
      CaptionButtonSize = 14
      CaptionColor.Bands = 256
      CaptionColor.EndColor = clWindow
      CaptionColor.FillType = gtSolid
      CaptionColor.StartColor = clBtnFace
      CaptionFont.Charset = DEFAULT_CHARSET
      CaptionFont.Color = clWindowText
      CaptionFont.Height = -11
      CaptionFont.Name = 'MS Sans Serif'
      CaptionFont.Style = []
      HiddenTabColor.Bands = 256
      HiddenTabColor.EndColor = clWindow
      HiddenTabColor.FillType = gtSolid
      HiddenTabColor.StartColor = clBtnFace
      HiddenTabFont.Charset = DEFAULT_CHARSET
      HiddenTabFont.Color = clBtnShadow
      HiddenTabFont.Height = -11
      HiddenTabFont.Name = 'MS Shell Dlg 2'
      HiddenTabFont.Style = []
      HideZoneColor.Bands = 256
      HideZoneColor.EndColor = clWindow
      HideZoneColor.FillType = gtSolid
      HideZoneColor.StartColor = clWindow
      SplitterColor = clBtnFace
      SplitterHeight = 4
      SplitterWidth = 4
      TabColor.Bands = 256
      TabColor.EndColor = clWindow
      TabColor.FillType = gtSolid
      TabColor.StartColor = clBtnHighlight
      TabFont.Charset = DEFAULT_CHARSET
      TabFont.Color = clWindowText
      TabFont.Height = -11
      TabFont.Name = 'MS Sans Serif'
      TabFont.Style = []
      TabIndent = 3
      TabPaneColor.Bands = 256
      TabPaneColor.EndColor = clWindow
      TabPaneColor.FillType = gtSolid
      TabPaneColor.StartColor = clBtnHighlight
      Predefined = True
    end
    object aqStandardUIStyle1: TaqStandardUIStyle
      ActiveCaptionColor.Bands = 256
      ActiveCaptionColor.EndColor = clWindow
      ActiveCaptionColor.FillType = gtSolid
      ActiveCaptionColor.StartColor = clBtnFace
      ActiveCaptionFont.Charset = DEFAULT_CHARSET
      ActiveCaptionFont.Color = clWindowText
      ActiveCaptionFont.Height = -11
      ActiveCaptionFont.Name = 'MS Sans Serif'
      ActiveCaptionFont.Style = []
      ActiveHiddenTabColor.Bands = 256
      ActiveHiddenTabColor.EndColor = clWindow
      ActiveHiddenTabColor.FillType = gtSolid
      ActiveHiddenTabColor.StartColor = clBtnFace
      ActiveHiddenTabFont.Charset = DEFAULT_CHARSET
      ActiveHiddenTabFont.Color = clWindowText
      ActiveHiddenTabFont.Height = -11
      ActiveHiddenTabFont.Name = 'MS Shell Dlg 2'
      ActiveHiddenTabFont.Style = []
      ActiveTabColor.Bands = 256
      ActiveTabColor.EndColor = clWindow
      ActiveTabColor.FillType = gtSolid
      ActiveTabColor.StartColor = clBtnFace
      ActiveTabFont.Charset = DEFAULT_CHARSET
      ActiveTabFont.Color = clWindowText
      ActiveTabFont.Height = -11
      ActiveTabFont.Name = 'MS Sans Serif'
      ActiveTabFont.Style = []
      CaptionButtonSize = 14
      CaptionColor.Bands = 256
      CaptionColor.EndColor = clWindow
      CaptionColor.FillType = gtSolid
      CaptionColor.StartColor = clBtnFace
      CaptionFont.Charset = DEFAULT_CHARSET
      CaptionFont.Color = clWindowText
      CaptionFont.Height = -11
      CaptionFont.Name = 'MS Sans Serif'
      CaptionFont.Style = []
      HiddenTabColor.Bands = 256
      HiddenTabColor.EndColor = clWindow
      HiddenTabColor.FillType = gtSolid
      HiddenTabColor.StartColor = clBtnFace
      HiddenTabFont.Charset = DEFAULT_CHARSET
      HiddenTabFont.Color = clBtnShadow
      HiddenTabFont.Height = -11
      HiddenTabFont.Name = 'MS Shell Dlg 2'
      HiddenTabFont.Style = []
      HideZoneColor.Bands = 256
      HideZoneColor.EndColor = clWindow
      HideZoneColor.FillType = gtSolid
      HideZoneColor.StartColor = clWindow
      SplitterColor = clBtnFace
      SplitterHeight = 4
      SplitterWidth = 4
      TabColor.Bands = 256
      TabColor.EndColor = clWindow
      TabColor.FillType = gtSolid
      TabColor.StartColor = clBtnFace
      TabFont.Charset = DEFAULT_CHARSET
      TabFont.Color = clWindowText
      TabFont.Height = -11
      TabFont.Name = 'MS Sans Serif'
      TabFont.Style = []
      TabIndent = 3
      TabPaneColor.Bands = 256
      TabPaneColor.EndColor = clWindow
      TabPaneColor.FillType = gtSolid
      TabPaneColor.StartColor = clBtnFace
      Predefined = True
    end
    object aqEnhancedUIStyle1: TaqEnhancedUIStyle
      ActiveCaptionColor.Bands = 256
      ActiveCaptionColor.EndColor = clWindow
      ActiveCaptionColor.FillType = gtSolid
      ActiveCaptionColor.StartColor = clBtnFace
      ActiveCaptionFont.Charset = DEFAULT_CHARSET
      ActiveCaptionFont.Color = clWindowText
      ActiveCaptionFont.Height = -11
      ActiveCaptionFont.Name = 'MS Sans Serif'
      ActiveCaptionFont.Style = []
      ActiveHiddenTabColor.Bands = 256
      ActiveHiddenTabColor.EndColor = clWindow
      ActiveHiddenTabColor.FillType = gtSolid
      ActiveHiddenTabColor.StartColor = clBtnFace
      ActiveHiddenTabFont.Charset = DEFAULT_CHARSET
      ActiveHiddenTabFont.Color = clWindowText
      ActiveHiddenTabFont.Height = -11
      ActiveHiddenTabFont.Name = 'MS Shell Dlg 2'
      ActiveHiddenTabFont.Style = []
      ActiveTabColor.Bands = 256
      ActiveTabColor.EndColor = clWindow
      ActiveTabColor.FillType = gtSolid
      ActiveTabColor.StartColor = clBtnFace
      ActiveTabFont.Charset = DEFAULT_CHARSET
      ActiveTabFont.Color = clWindowText
      ActiveTabFont.Height = -11
      ActiveTabFont.Name = 'MS Sans Serif'
      ActiveTabFont.Style = []
      CaptionButtonSize = 14
      CaptionColor.Bands = 256
      CaptionColor.EndColor = clWindow
      CaptionColor.FillType = gtSolid
      CaptionColor.StartColor = clBtnFace
      CaptionFont.Charset = DEFAULT_CHARSET
      CaptionFont.Color = clWindowText
      CaptionFont.Height = -11
      CaptionFont.Name = 'MS Sans Serif'
      CaptionFont.Style = []
      HiddenTabColor.Bands = 256
      HiddenTabColor.EndColor = clWindow
      HiddenTabColor.FillType = gtSolid
      HiddenTabColor.StartColor = clBtnFace
      HiddenTabFont.Charset = DEFAULT_CHARSET
      HiddenTabFont.Color = clBtnShadow
      HiddenTabFont.Height = -11
      HiddenTabFont.Name = 'MS Shell Dlg 2'
      HiddenTabFont.Style = []
      HideZoneColor.Bands = 256
      HideZoneColor.EndColor = clWindow
      HideZoneColor.FillType = gtSolid
      HideZoneColor.StartColor = clWindow
      SplitterColor = clBtnFace
      SplitterHeight = 4
      SplitterWidth = 4
      TabColor.Bands = 256
      TabColor.EndColor = clWindow
      TabColor.FillType = gtSolid
      TabColor.StartColor = clBtnFace
      TabFont.Charset = DEFAULT_CHARSET
      TabFont.Color = clWindowText
      TabFont.Height = -11
      TabFont.Name = 'MS Sans Serif'
      TabFont.Style = []
      TabIndent = 3
      TabPaneColor.Bands = 256
      TabPaneColor.EndColor = clWindow
      TabPaneColor.FillType = gtSolid
      TabPaneColor.StartColor = clBtnFace
      Predefined = True
    end
    object aqFlatUIStyle1: TaqFlatUIStyle
      ActiveCaptionColor.Bands = 256
      ActiveCaptionColor.EndColor = clWindow
      ActiveCaptionColor.FillType = gtSolid
      ActiveCaptionColor.StartColor = clActiveCaption
      ActiveCaptionFont.Charset = DEFAULT_CHARSET
      ActiveCaptionFont.Color = clCaptionText
      ActiveCaptionFont.Height = -11
      ActiveCaptionFont.Name = 'MS Sans Serif'
      ActiveCaptionFont.Style = []
      ActiveHiddenTabColor.Bands = 256
      ActiveHiddenTabColor.EndColor = clWindow
      ActiveHiddenTabColor.FillType = gtSolid
      ActiveHiddenTabColor.StartColor = clBtnFace
      ActiveHiddenTabFont.Charset = DEFAULT_CHARSET
      ActiveHiddenTabFont.Color = clWindowText
      ActiveHiddenTabFont.Height = -11
      ActiveHiddenTabFont.Name = 'MS Shell Dlg 2'
      ActiveHiddenTabFont.Style = []
      ActiveTabColor.Bands = 256
      ActiveTabColor.EndColor = clWindow
      ActiveTabColor.FillType = gtSolid
      ActiveTabColor.StartColor = clBtnFace
      ActiveTabFont.Charset = DEFAULT_CHARSET
      ActiveTabFont.Color = clWindowText
      ActiveTabFont.Height = -11
      ActiveTabFont.Name = 'MS Sans Serif'
      ActiveTabFont.Style = []
      CaptionButtonSize = 14
      CaptionColor.Bands = 256
      CaptionColor.EndColor = clWindow
      CaptionColor.FillType = gtSolid
      CaptionColor.StartColor = clBtnFace
      CaptionFont.Charset = DEFAULT_CHARSET
      CaptionFont.Color = clWindowText
      CaptionFont.Height = -11
      CaptionFont.Name = 'MS Sans Serif'
      CaptionFont.Style = []
      HiddenTabColor.Bands = 256
      HiddenTabColor.EndColor = clWindow
      HiddenTabColor.FillType = gtSolid
      HiddenTabColor.StartColor = clBtnFace
      HiddenTabFont.Charset = DEFAULT_CHARSET
      HiddenTabFont.Color = clBtnShadow
      HiddenTabFont.Height = -11
      HiddenTabFont.Name = 'MS Shell Dlg 2'
      HiddenTabFont.Style = []
      HideZoneColor.Bands = 256
      HideZoneColor.EndColor = clWindow
      HideZoneColor.FillType = gtSolid
      HideZoneColor.StartColor = clWindow
      SplitterColor = clBtnFace
      SplitterHeight = 4
      SplitterWidth = 4
      TabColor.Bands = 256
      TabColor.EndColor = clWindow
      TabColor.FillType = gtSolid
      TabColor.StartColor = 15465727
      TabFont.Charset = DEFAULT_CHARSET
      TabFont.Color = clWindowText
      TabFont.Height = -11
      TabFont.Name = 'MS Sans Serif'
      TabFont.Style = []
      TabIndent = 3
      TabPaneColor.Bands = 256
      TabPaneColor.EndColor = clWindow
      TabPaneColor.FillType = gtSolid
      TabPaneColor.StartColor = 15465727
      Predefined = True
    end
    object aqThemedUIStyle1: TaqThemedUIStyle
      ActiveCaptionFont.Charset = DEFAULT_CHARSET
      ActiveCaptionFont.Color = clWindowText
      ActiveCaptionFont.Height = -11
      ActiveCaptionFont.Name = 'MS Sans Serif'
      ActiveCaptionFont.Style = []
      ActiveHiddenTabFont.Charset = DEFAULT_CHARSET
      ActiveHiddenTabFont.Color = clWindowText
      ActiveHiddenTabFont.Height = -11
      ActiveHiddenTabFont.Name = 'MS Shell Dlg 2'
      ActiveHiddenTabFont.Style = []
      CaptionButtonSize = 14
      CaptionFont.Charset = DEFAULT_CHARSET
      CaptionFont.Color = clWindowText
      CaptionFont.Height = -11
      CaptionFont.Name = 'MS Sans Serif'
      CaptionFont.Style = []
      HiddenTabFont.Charset = DEFAULT_CHARSET
      HiddenTabFont.Color = clBtnShadow
      HiddenTabFont.Height = -11
      HiddenTabFont.Name = 'MS Shell Dlg 2'
      HiddenTabFont.Style = []
      SplitterHeight = 4
      SplitterWidth = 4
      TabFont.Charset = DEFAULT_CHARSET
      TabFont.Color = clWindowText
      TabFont.Height = -11
      TabFont.Name = 'MS Sans Serif'
      TabFont.Style = []
      ActiveTabFont.Charset = DEFAULT_CHARSET
      ActiveTabFont.Color = clWindowText
      ActiveTabFont.Height = -11
      ActiveTabFont.Name = 'MS Sans Serif'
      ActiveTabFont.Style = []
      TabIndent = 1
      Predefined = True
      object TaqCaptionButtonWidgets
        HideButton.PartIndex = bwCloseButton
        UndockButton.PartIndex = bwDropDown
        AutoHideButton.PartIndex = bwPin
        UndoAutoHideButton.PartIndex = bwRotatedPin
        MaximizeButton.PartIndex = bwMaxButton
        RestoreButton.PartIndex = bwRestoreButton
        HelpButton.PartIndex = bwHelpButton
        MenuButton.PartIndex = bwDropDown
        CustomButton.PartIndex = bwNone
      end
    end
  end
  object TBImageList1: TTBImageList
    Left = 76
    Top = 178
    Bitmap = {
      494C010103000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      00000000000000000000000000000000000000000000F7F7F700E0E0E000D8D8
      D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8D800D8D8
      D800D8D8D800E0E0E000F7F7F700000000000000000000000000FCFCFC00EEEE
      EE00E0E0E000DEDEDE00E9E9E900F9F9F900FEFEFE00FEFEFE00000000000000
      00000000000000000000000000000000000000000000FAFAFA00EAEAEA00DEDE
      DE00DBDBDB00DADADA00DADADA00D9D9D900D9D9D900D9D9D900D8D8D800DADA
      DA00DBDBDB00E0E0E000EDEDED00FBFBFB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E0E0E0008C8C8C006F6F
      6F006D6D6D006D6D6D006D6D6D006D6D6D006D6D6D006D6D6D006D6D6D006D6D
      6D006F6F6F008C8C8C00E0E0E000000000000000000000000000EDEDED00B3B3
      B30083838300818181009F9F9F00CBCBCB00DCDCDC00E6E6E600F5F5F500FDFD
      FD00FEFEFE00000000000000000000000000F9F9F900DBDBDB00A3A3A3007F7F
      7F00767676007474740072727200727272007272720071717100707070007474
      74007878780083838300B2B2B200EDEDED000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009B7C6B009D7E6D009C7E
      6D009C7E6D009C7E6D009C7D6D009C7D6C009B7C6B009B7C6B009A7C6A00997B
      68009B7C6B006F6F6F00D8D8D8000000000000000000FBFBFB009EC0D2000F71
      A0001D77A000497A9100595959006C6C6C007C7C7C0096969600BFBFBF00D8D8
      D800E6E6E600F8F8F800FEFEFE0000000000E6E7E600B17B5E00A64B1800885F
      4800A7ABAB00B8B1AB00C7C6C200C7C7CA00AFAFAF0099A2A800823B13009F49
      19009F4E2700806558007E7E7E00DDDDDD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEFEFE009B77660000000000FAF4
      E900FAF4E900FAF4E900FAF3E800FAF3E700FAF2E600FAF1E400F9EFE000F8ED
      DA00977967006D6D6D00D8D8D800FEFEFE0000000000F0F0F0003095BD00007A
      AD004CD9F7000CA3D2000581B3001878A7003D7B99005F686C00676767007777
      770097979700C8C8C800E2E2E200F8F8F800CE8D5E00BE631700CD7927009E87
      7000B9875900CE700D00DCAF7A00FEFFFF00FAFAFA00D9E2EA00AD702400DD94
      3500ECBE7500A34611006F6F6F00D8D8D8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEFEFE00A27F6F0000000000FCF6
      ED00FCF6ED00FBF6ED00FBF5EC00FBF5EB00FBF4EA00FBF3E800FAF2E600FAF1
      E200987968006D6D6D00D8D8D800FEFEFE00FCFCFC008BBACD001A8AB700007D
      B00090EFFF0030E0FF0029E2FF001DCBEE000FA4D2000584B6001079A800397A
      9A00575757006B6B6B0093939300E3E3E300C9680D00C0702800C36C1E00A787
      6B00AE7C5A00BA580900C69A6C00D9DFE500F8F8F800F8FFFF00A3631D00D587
      2D00E5AF6900A5460A006E6E6E00D8D8D8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEFEFE00A380700000000000FCF8
      F100FCF8F100FCF7F000FCF7EF00FCF6EE00FBF6EC00FBF5EB00FBF4E900FAF3
      E600997A6A006D6D6D00D8D8D800FEFEFE00F2F2F2004D9DBC003B9FC600007F
      B2009FF1FF0046E2FF0040E1FF003AE0FF0034E0FF0033E2FF002DD0EE0020B1
      DD000C86B60018749E0071717100D9D9D900CB690C00BB682300BF661900B490
      6E00B37C6000AF490000B5875E00BFC6CD00DEE0E20000000000A5641F00D484
      2D00E4B06C00A44409006D6D6D00D8D8D8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEFEFE00A987780000000000FDF9
      F400FDF9F400FCF9F300FCF8F200FCF8F100FCF7EF00FBF6ED00FBF5EB00FBF4
      E800997B6B006D6D6D00D8D8D800FEFEFE009FC9D9001487B900B6F5FB000081
      B400B0F7FF005EECFF0058E9FF0052E7FF004BE4FF0045E2FF0045E0FF0046E2
      FF003FCFF000006DA0006E6E6E00D8D8D800C9690A00B8631F00BC621900B07F
      5500C1BFBD00B0ACA80098918B009A8E8400A3978A00AEA79F0090581F00A767
      2300DA954200A6450D006D6D6D00D8D8D8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEFEFE00AB897A0000000000FDFB
      F700FDFBF700FDFAF600FDFAF500FCF9F400FCF8F200FCF7EF00FBF6ED00FBF5
      EA009A7C6B006D6D6D00D8D8D800FEFEFE0060ADCB0045A6CA00DDFFFF000084
      B700C0FCFF0077F4FF0070F1FF006AEFFF0063ECFF005CEAFF0058E9FF0056E4
      FE0064DAED00006FA2006D6D6D00D8D8D800C9670A00B55D1B00B65D1700B85B
      0F00BA580800BE5D0A00C2620E00C5671000C8691300C96E1600CE782400D07F
      2A00D3893200A5450E006D6D6D00D8D8D8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEFEFE00AF8E7F00F7F8FF00EDEE
      FB00F1F4FA00FCFBFA00FDFBF800FDFAF600FDF9F400FCF8F100FCF7EF00FBF5
      EC009B7C6B006E6E6E00D8D8D800FEFEFE00379DC400ABDCEA00E5FFFF000086
      B900D1FFFF008EFDFF0089FAFF0082F6FF007AF4FF0073F3FF006CF0FF004CC9
      E300AAFFFF000072A5006D6D6D00D8D8D800C8680900B0551400B1693300AF71
      4200AE703F00AF734100B1734000B0764200B2774100B47A4300B47D4600BD80
      4100D1842D00A6460E006D6D6D00D8D8D8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FAFAFB00A69DA900A7B3FF008391
      FE00A6D1FF00C7D1FD00F7F6FB00FDFBF800FDFAF600FCF9F300FCF7F000FCF6
      ED009B7C6C006E6E6E00D8D8D800FEFEFE000F90BF00CAF5FA00F1FFFF000088
      BB00DCFFFF00A1FFFF009CFBFF0094F9FF008CF7FF0085F6FF007EF5FF005DCB
      DF00D1FFFF000074A7006D6D6D00D8D8D800C7650800AC4B0900B59C8900E6F2
      FB00E4EDF300E6EEF500E6EDF400E1E9EF00DDE5EC00D5DEE400D1DCE600AD9A
      8A00D07F2600A4450E006D6D6D00D8D8D8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E2E5F7007890CE005497FF005DB4
      FF004FA1FF006176FF00C9D2FD00FEFCFA00FDFBF700FDF9F400FCF8F100FCF6
      EE009C7C6D006F6F6F00D8D8D800000000000590C3001094C3007EC4DD00008A
      BD00C3EDF5007ADBEA0085E3EF0092F0F8009CFAFF0094F8FF0065D2E700AAF4
      F900DAFFFF000077AA006D6D6D00D8D8D800C8650900A8430300B8998400F4F9
      FC00D5D4D200BEBDBC00C0BFBE00C1C0BF00C2C0BF00CBCAC900DFE3E600B19B
      8500CF7C2200A4440D006D6D6D00D8D8D8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C3CCF7003653E20060E1FF001DE5
      FF005BEDFF005FB3FF00A1C0FE00F4F5FB00FDFBF800FCF9F400F9F4EE00F0E8
      E0009E7F70007D7D7D00DDDDDD000000000000000000FEFEFE00FEFEFE00008D
      C000F2FFFF00C8F7FB00ABE7F10081D4E6006BC7DF0065C6DF005FC2DB00D3FF
      FF00E7FFFF000079AC006D6D6D00D8D8D800C7650800A23C0000BA9B8500FAFF
      FF00F1F1F100ECECEC00EBEBEB00E8E8E800E4E4E400E1E1E100E2E5E900B29B
      8800CD792000A5440D006E6E6E00D8D8D8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BAC5FA005997EE0057EDFF0000CD
      FF003CECFF003A82FF00899BFE00F1F2FB00FDFBF900A7827000A7827000A782
      7000A7827000B2B2B200EEEEEE000000000000000000000000000000000026A2
      D10031A5CD004AB0D30083CDE200D0EFF600E6FCFC00F1FFFF00E6FFFF00DEFF
      FF00F3FFFF00007CAF006E6E6E00D8D8D800C66207009E370000BC9C8A000000
      0000DDDCDB00C2C1C000C4C3C200C4C3C200C2C1C000CECDCC00E4E7EB00B39D
      8900CD771F00A4440E006F6F6F00D8D8D8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D5E3FC004960DC00499EFF0063F8
      FF006DE2FF00437BFF00ADBDFE00FAF8FA00FDFBF900A7827000F5E2D900B18E
      7E00B2A8A300E6E6E600FCFCFC00000000000000000000000000000000000000
      0000FEFEFE00C2E7F4007EC4DF0047ABD100319DC8001690BF00000000000000
      000000000000007EB10077777700DBDBDB00C56206009A310000BF9D8A000000
      0000FBFBFB00EEEEEE00EFEFEF00E9E9E900E4E4E400E4E4E400E6EAED00B5A0
      8900C2701D00A9480D0079797900DDDDDD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6FE008E88BB00668DFF005D9B
      FF004E71FF00A6CDFF00E3E8FF000000000000000000A7827000B18E7E00B4AA
      A500E6E6E600FCFCFC0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEFEFE00088EBF00EDF6FA000000
      000000000000007EB100A3A3A300E9E9E900C563090093280000BE9D8D000000
      0000E5E4E300D0CFCE00D1D0CF00CFCECD00CDCCCB00D4D3D200E9EDF100B9A3
      8C008A4F1400B0521200ABABAB00EBEBEB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0A8A300ACABB500958A
      B3009D91AF00B29B9F00C0A59700AC887700AC887700A7827000CDC1BC00EEEE
      EE00FCFCFC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000044ADD8000086B9000084
      B7000082B5005D9FBE00E8E8E800FAFAFA00E4B27E00C2560400C8A48300D5D7
      D700D6D3D000D6D3D000D1CFCC00CCCAC800C7C5C300C1BEBB00BDBDBD00B194
      7B00C15A0400C9997700EBEBEB00FBFBFB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF008001C03F800000008001C00700000000
      8001800100000000200080000000000020000000000000002000000000400000
      2000000000000000200000000000000000000000000000000000000000000000
      000100000000000000018000000000000001E000100000000001F03810000000
      0183FF18100000008007FF800000000000000000000000000000000000000000
      000000000000}
  end
  object dxBarManager1: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Bars = <
      item
        Caption = 'MainMenuBar'
        DockedDockingStyle = dsTop
        DockedLeft = 0
        DockedTop = 0
        DockingStyle = dsTop
        FloatLeft = 458
        FloatTop = 312
        FloatClientWidth = 23
        FloatClientHeight = 22
        IsMainMenu = True
        ItemLinks = <
          item
            Item = dxBarSubItem3
            Visible = True
          end
          item
            Item = dxBarSubItem4
            Visible = True
          end>
        MultiLine = True
        Name = 'MainMenuBar'
        OneOnRow = True
        Row = 0
        UseOwnFont = False
        Visible = True
        WholeRow = True
      end>
    Categories.Strings = (
      'Default'
      'File'
      'PopupMenu1')
    Categories.ItemsVisibles = (
      2
      2
      2)
    Categories.Visibles = (
      True
      True
      True)
    Images = TBImageList1
    PopupMenuLinks = <>
    Style = bmsXP
    UseSystemFont = True
    Left = 172
    Top = 129
    DockControlHeights = (
      0
      0
      23
      0)
    object dxBarButton1: TdxBarButton
      Action = ControllerModule.OpenAction
      Category = 1
    end
    object dxBarSubItem1: TdxBarSubItem
      Caption = 'New Item'
      Category = 0
      Visible = ivAlways
      ItemLinks = <>
    end
    object dxBarButton2: TdxBarButton
      Caption = '&File'
      Category = 0
      Hint = 'File'
      Visible = ivAlways
      DropDownMenu = PopupMenu1
    end
    object dxBarSubItem2: TdxBarSubItem
      Caption = 'New Item'
      Category = 0
      Visible = ivAlways
      ItemLinks = <>
    end
    object dxBarInPlaceSubItem1: TdxBarInPlaceSubItem
      Caption = 'New Item'
      Category = 0
      Visible = ivAlways
      ItemLinks = <>
      KeepBeginGroupWhileExpanded = False
    end
    object New1: TdxBarButton
      Category = 2
      Visible = ivAlways
    end
    object Save1: TdxBarButton
      Action = ControllerModule.SaveAction
      Category = 2
    end
    object SaveAs1: TdxBarButton
      Action = ControllerModule.SaveAsAction
      Category = 2
    end
    object dxBarToolbarsListItem1: TdxBarToolbarsListItem
      Caption = 'New Item'
      Category = 0
      Visible = ivAlways
    end
    object dxBarSubItem3: TdxBarSubItem
      Caption = '&File'
      Category = 0
      Visible = ivAlways
      ItemLinks = <
        item
          Item = dxBarButton11
          Visible = True
        end
        item
          Item = dxBarButton13
          Visible = True
        end
        item
          Item = dxBarButton4
          Visible = True
        end
        item
          BeginGroup = True
          Item = dxBarButton5
          Visible = True
        end
        item
          Item = dxBarButton6
          Visible = True
        end
        item
          Item = dxBarButton8
          Visible = True
        end
        item
          Item = dxBarButton9
          Visible = True
        end
        item
          Item = dxBarButton10
          Visible = True
        end
        item
          Item = dxBarButton7
          Visible = True
        end>
    end
    object dxBarButton3: TdxBarButton
      Category = 0
      Visible = ivAlways
    end
    object dxBarLargeButton1: TdxBarLargeButton
      Category = 0
      Visible = ivAlways
      PaintStyle = psCaptionInMenu
      AutoGrayScale = False
      SyncImageIndex = False
      ImageIndex = 0
    end
    object dxBarButton4: TdxBarButton
      Action = ControllerModule.OpenAction
      Category = 0
    end
    object dxBarButton5: TdxBarButton
      Action = ControllerModule.SaveAction
      Category = 0
    end
    object dxBarButton6: TdxBarButton
      Action = ControllerModule.SaveAsAction
      Category = 0
    end
    object dxBarButton7: TdxBarButton
      Action = ControllerModule.CloseProjectAction
      Category = 0
    end
    object dxBarButton8: TdxBarButton
      Action = ControllerModule.SaveProjectAction
      Category = 0
    end
    object dxBarButton9: TdxBarButton
      Action = ControllerModule.SaveProjectAsAction
      Category = 0
    end
    object dxBarButton10: TdxBarButton
      Action = ControllerModule.CloseAllAction
      Category = 0
    end
    object dxBarButton11: TdxBarButton
      Action = ControllerModule.NewTurboAjaxAction
      Category = 0
    end
    object dxBarSubItem4: TdxBarSubItem
      Caption = '&View'
      Category = 0
      Visible = ivAlways
      ItemLinks = <
        item
          Item = dxBarButton12
          Visible = True
        end>
    end
    object dxBarButton12: TdxBarButton
      Action = ControllerModule.ViewPreviewAction
      Category = 0
    end
    object dxBarListItem1: TdxBarListItem
      Caption = 'New Item'
      Category = 0
      Visible = ivAlways
    end
    object dxBarButton13: TdxBarButton
      Action = ControllerModule.NewTurboPhpAction
      Category = 0
    end
  end
  object PopupMenu1: TdxBarPopupMenu
    BarManager = dxBarManager1
    ItemLinks = <
      item
        Item = New1
        Visible = True
      end
      item
        Item = dxBarButton1
        Visible = True
      end
      item
        Item = Save1
        Visible = True
      end
      item
        Item = SaveAs1
        Visible = True
      end>
    UseOwnFont = False
    Left = 76
    Top = 232
  end
end
