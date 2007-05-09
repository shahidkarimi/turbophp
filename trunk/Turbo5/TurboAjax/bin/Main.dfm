object MainForm: TMainForm
  Left = 344
  Top = 185
  Width = 780
  Height = 580
  Caption = 'TurboIDE'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DocumentsPanel: TPanel
    Left = 0
    Top = 51
    Width = 772
    Height = 495
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object aqDockingSite1: TaqDockingSite
      Left = 0
      Top = 0
      Width = 772
      Height = 495
      Align = alClient
      DockingManager = aqDockingManager
      OnCanUndock = aqDockingSite1CanUndock
      Key = '{6F28736B-9286-4B12-9A2E-726493309901}'
    end
  end
  object aqDockingManager: TaqDockingManager
    AutoDragKey = [dssCtrl, dssLeft]
    CaptionButtons = [dbiHide, dbiAutoHide, dbiMaximizeRestore, dbiMenu]
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
    FloatingFormOnTop = True
    FloatingFormType = fftNormal
    ShowImages = [ctCaption, ctTab]
    StoreOptions = [dsoFormPosition, dsoFormState, dsoFormConstraints]
    StyleManager = aqStyleManager1
    OnUpdateActions = aqDockingManagerUpdateActions
    Left = 80
    Top = 79
    Style = MainForm.aqStyleManager1.aqThemedUIStyle1
    object DocumentsDock: TaqDockingControl
      Width = 772
      Height = 453
      Caption = 'Documents'
      ImageIndex = -1
      MinWidth = 0
      MinHeight = 0
      ShowCaption = bvTrue
      ShowImage = bvDefault
      Visible = True
      Key = '{288C089E-38C5-40F3-99A7-D3D60E4A23CF}'
      DesignClientHeight = 453
      DesignClientWidth = 772
    end
    object InspectorDock: TaqDockingControl
      Width = 772
      Height = 453
      Caption = 'Inspector'
      ImageIndex = -1
      MinWidth = 0
      MinHeight = 0
      ShowCaption = bvDefault
      ShowImage = bvDefault
      Visible = True
      Key = '{A5F2DDD0-A975-4268-860C-2465FC33A7D9}'
      DesignClientHeight = 453
      DesignClientWidth = 772
    end
    object ComponentsDock: TaqDockingControl
      Width = 772
      Height = 453
      Caption = 'Components'
      ImageIndex = -1
      MinWidth = 0
      MinHeight = 0
      ShowCaption = bvDefault
      ShowImage = bvDefault
      Visible = True
      Key = '{4F597113-DBA2-4FAF-BCC8-42A764F355A5}'
      DesignClientHeight = 453
      DesignClientWidth = 772
    end
    object ProjectDock: TaqDockingControl
      Width = 772
      Height = 453
      Caption = 'Project'
      ImageIndex = -1
      MinWidth = 0
      MinHeight = 0
      ShowCaption = bvDefault
      ShowImage = bvDefault
      Visible = True
      Key = '{EEE182A2-2A89-4EFB-B8A6-35D87321697F}'
      DesignClientHeight = 453
      DesignClientWidth = 772
    end
    object QuickPropertiesDock: TaqDockingControl
      Width = 772
      Height = 453
      Caption = 'QuickProperties'
      ImageIndex = -1
      MinWidth = 0
      MinHeight = 0
      ShowCaption = bvDefault
      ShowImage = bvDefault
      Visible = True
      Key = '{C11B0ECD-D7E1-40F7-A857-980E684DB483}'
      DesignClientHeight = 453
      DesignClientWidth = 772
    end
    object PaletteDock: TaqDockingControl
      Tag = 2
      Width = 772
      Height = 453
      Caption = 'Palette'
      ImageIndex = -1
      MinWidth = 0
      MinHeight = 0
      ShowCaption = bvDefault
      ShowImage = bvDefault
      Visible = True
      Key = '{08504CB2-1CEF-4A27-A1D5-CD6BFFCEABCD}'
      DesignClientHeight = 453
      DesignClientWidth = 772
    end
    object CodeExplorerDock: TaqDockingControl
      Width = 772
      Height = 453
      Caption = 'Code Explorer'
      ImageIndex = -1
      MinWidth = 0
      MinHeight = 0
      ShowCaption = bvDefault
      ShowImage = bvDefault
      Visible = True
      Key = '{191CB80A-3A14-427C-80BA-C26D9442B96D}'
      DesignClientHeight = 453
      DesignClientWidth = 772
    end
    object QuickHelpDock: TaqDockingControl
      Width = 772
      Height = 453
      Caption = 'Quick Help'
      ImageIndex = -1
      MinWidth = 0
      MinHeight = 0
      ShowCaption = bvDefault
      ShowImage = bvDefault
      Visible = False
      Key = '{2259F163-E9CC-4E5F-8C1E-2B1D4E35226B}'
      DesignClientHeight = 453
      DesignClientWidth = 772
    end
    object PreviewDock: TaqDockingControl
      Width = 772
      Height = 453
      Caption = 'Preview'
      ImageIndex = -1
      MinWidth = 0
      MinHeight = 0
      ShowCaption = bvDefault
      ShowImage = bvDefault
      Visible = False
      Key = '{A88D9A39-5285-491E-B036-E09DFA472638}'
      DesignClientHeight = 453
      DesignClientWidth = 772
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
  object ActionManager1: TActionManager
    Images = PngImageList
    Left = 184
    Top = 79
    StyleName = 'XP Style'
    object NewAction: TAction
      Category = 'File'
      Caption = 'New Document'
      OnExecute = NewActionExecute
    end
    object ViewComponentsAction: TAction
      Category = 'View'
      Caption = 'Components'
      OnExecute = ViewActionExecute
      OnUpdate = ViewActionUpdate
    end
    object ViewInspectorDock: TAction
      Tag = 1
      Category = 'View'
      Caption = 'Inspector'
      OnExecute = ViewActionExecute
      OnUpdate = ViewActionUpdate
    end
    object ViewPaletteDock: TAction
      Tag = 2
      Category = 'View'
      Caption = 'Palette'
      OnExecute = ViewActionExecute
      OnUpdate = ViewActionUpdate
    end
    object ViewProjectAction: TAction
      Tag = 3
      Category = 'View'
      Caption = 'Project'
      OnExecute = ViewActionExecute
      OnUpdate = ViewActionUpdate
    end
    object ViewQuickPropertiesAction: TAction
      Tag = 4
      Category = 'View'
      Caption = 'Quick Properties'
      OnExecute = ViewActionExecute
      OnUpdate = ViewActionUpdate
    end
    object ViewCodeExplorerAction: TAction
      Tag = 5
      Category = 'View'
      Caption = 'Code Explorer'
      OnExecute = ViewActionExecute
      OnUpdate = ViewActionUpdate
    end
    object ViewQuickHelpAction: TAction
      Tag = 6
      Category = 'View'
      Caption = 'Quick Help'
      OnExecute = ViewActionExecute
      OnUpdate = ViewActionUpdate
    end
    object NewAjaxAction: TAction
      Category = 'File'
      Caption = 'AJAX'
      ImageIndex = 0
      OnExecute = NewAjaxActionExecute
    end
    object NewPhpAction: TAction
      Category = 'File'
      Caption = 'PHP'
      OnExecute = NewPhpActionExecute
    end
    object NewJsAction: TAction
      Category = 'File'
      Caption = 'JavaScript'
      OnExecute = NewJsActionExecute
    end
    object PublishAction: TAction
      Category = 'File'
      Caption = 'Publish'
      ImageIndex = 3
      OnExecute = PublishActionExecute
      OnUpdate = PublishActionUpdate
    end
    object ViewPreviewAction: TAction
      Tag = 7
      Category = 'View'
      Caption = 'Preview'
      OnExecute = ViewActionExecute
      OnUpdate = ViewActionUpdate
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
        AllowCustomizing = False
        AllowQuickCustomizing = False
        Caption = 'MainMenu'
        DockedDockingStyle = dsTop
        DockedLeft = 0
        DockedTop = 0
        DockingStyle = dsTop
        FloatLeft = 564
        FloatTop = 429
        FloatClientWidth = 23
        FloatClientHeight = 22
        IsMainMenu = True
        ItemLinks = <
          item
            Item = FileMenu
            Visible = True
          end
          item
            Item = ViewMenu
            Visible = True
          end>
        MultiLine = True
        Name = 'MainMenu'
        OneOnRow = True
        Row = 0
        UseOwnFont = False
        Visible = True
        WholeRow = True
      end
      item
        AllowCustomizing = False
        AllowQuickCustomizing = False
        Caption = 'Toolbar'
        DockedDockingStyle = dsTop
        DockedLeft = 0
        DockedTop = 23
        DockingStyle = dsTop
        FloatLeft = 564
        FloatTop = 429
        FloatClientWidth = 23
        FloatClientHeight = 22
        ItemLinks = <
          item
            Item = NewButton
            Visible = True
          end
          item
            Item = OpenButton
            Visible = True
          end
          item
            Item = SaveButton
            Visible = True
          end
          item
            BeginGroup = True
            Item = dxBarButton1
            Visible = True
          end>
        Name = 'Toolbar'
        OneOnRow = True
        Row = 1
        UseOwnFont = False
        Visible = True
        WholeRow = False
      end>
    Categories.Strings = (
      'Default')
    Categories.ItemsVisibles = (
      2)
    Categories.Visibles = (
      True)
    Images = PngImageList
    LargeImages = PngImageList
    PopupMenuLinks = <>
    Style = bmsXP
    UseSystemFont = True
    Left = 184
    Top = 128
    DockControlHeights = (
      0
      0
      51
      0)
    object FileMenu: TdxBarSubItem
      Caption = '&File'
      Category = 0
      Visible = ivAlways
      ItemLinks = <
        item
          Item = NewDocumentItem
          Visible = True
        end
        item
          Item = OpenButton
          Visible = True
        end
        item
          Item = SaveButton
          Visible = True
        end
        item
          Item = SaveAsButton
          Visible = True
        end
        item
          Item = CloseButton
          Visible = True
        end
        item
          Item = CloseAllButton
          Visible = True
        end
        item
          BeginGroup = True
          Item = PublishButton
          Visible = True
        end>
    end
    object NewButton: TdxBarButton
      Action = NewAjaxAction
      Category = 0
      Hint = 'AJAX'
    end
    object SaveButton: TdxBarButton
      Action = LrIDEControllerModule.LrSaveOrSaveAsAction
      Category = 0
      Hint = 'Save'
    end
    object OpenButton: TdxBarButton
      Action = LrIDEControllerModule.LrOpenAction
      Category = 0
    end
    object SaveAsButton: TdxBarButton
      Action = LrIDEControllerModule.LrSaveAsAction
      Category = 0
    end
    object CloseButton: TdxBarButton
      Action = LrIDEControllerModule.LrCloseAction
      Category = 0
    end
    object CloseAllButton: TdxBarButton
      Action = LrIDEControllerModule.LrCloseAllAction
      Category = 0
    end
    object ViewMenu: TdxBarSubItem
      Caption = '&View'
      Category = 0
      Visible = ivAlways
      ItemLinks = <
        item
          Item = ViewProjectButton
          Visible = True
        end
        item
          Item = ViewPaletteButton
          Visible = True
        end
        item
          Item = ViewComponentsButton
          Visible = True
        end
        item
          Item = ViewCodeExplorerButton
          Visible = True
        end
        item
          Item = ViewInspectorButton
          Visible = True
        end
        item
          Item = ViewQuickPropertiesButton
          Visible = True
        end
        item
          Item = ViewQuickHelpButton
          Visible = True
        end
        item
          Item = dxBarButton3
          Visible = True
        end>
    end
    object ViewProjectButton: TdxBarButton
      Action = ViewProjectAction
      Category = 0
      ButtonStyle = bsChecked
    end
    object ViewComponentsButton: TdxBarButton
      Action = ViewComponentsAction
      Category = 0
      ButtonStyle = bsChecked
    end
    object ViewInspectorButton: TdxBarButton
      Action = ViewInspectorDock
      Category = 0
      ButtonStyle = bsChecked
    end
    object ViewPaletteButton: TdxBarButton
      Action = ViewPaletteDock
      Category = 0
      ButtonStyle = bsChecked
    end
    object ViewQuickPropertiesButton: TdxBarButton
      Action = ViewQuickPropertiesAction
      Category = 0
      ButtonStyle = bsChecked
    end
    object ViewCodeExplorerButton: TdxBarButton
      Action = ViewCodeExplorerAction
      Category = 0
      ButtonStyle = bsChecked
    end
    object ViewQuickHelpButton: TdxBarButton
      Action = ViewQuickHelpAction
      Category = 0
      ButtonStyle = bsChecked
    end
    object NewDocumentItem: TdxBarSubItem
      Caption = 'New Document'
      Category = 0
      Visible = ivAlways
      ItemLinks = <
        item
          Item = NewAjaxButton
          Visible = True
        end
        item
          Item = NewJsButton
          Visible = True
        end
        item
          Item = NewPhpButton
          Visible = True
        end>
    end
    object NewAjaxButton: TdxBarButton
      Action = NewAjaxAction
      Category = 0
      Hint = 'AJAX'
    end
    object NewPhpButton: TdxBarButton
      Action = NewPhpAction
      Category = 0
      Hint = 'PHP'
    end
    object NewJsButton: TdxBarButton
      Action = NewJsAction
      Category = 0
      Hint = 'JavaScript'
    end
    object dxBarSubItem1: TdxBarSubItem
      Caption = 'Project'
      Category = 0
      Visible = ivAlways
      ItemLinks = <>
    end
    object PublishButton: TdxBarButton
      Action = PublishAction
      Category = 0
    end
    object dxBarButton2: TdxBarButton
      Caption = 'New Item'
      Category = 0
      Hint = 'New Item'
      Visible = ivAlways
    end
    object dxBarButton1: TdxBarButton
      Action = PublishAction
      Category = 0
    end
    object dxBarButton3: TdxBarButton
      Action = ViewPreviewAction
      Category = 0
      ButtonStyle = bsChecked
    end
  end
  object MadExceptionHandler1: TMadExceptionHandler
    Left = 184
    Top = 183
  end
  object PngImageList: TPngImageList
    PngImages = <
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000002B744558744372656174696F6E2054696D65004D6F20313520417567
          20323030352031313A33313A3134202B30313030D9FE197A0000000774494D45
          000000000000000973942E000000097048597300000AF000000AF00142AC3498
          0000024F4944415478DAA5925F4853511CC7BF77B335C75A4FB9FE8822512148
          041133FBABBEF8523D1896655160A616582F2921DBC2B9ABD09F41391C9293A4
          9816F8D69345A0884145BD0C021B837A70F6106A3ABADBBDBF7EE76E3AAFBED5
          81C3F79C7B7FBFCFF9FECEEF48F8CF21AD2C8623EFA864EB12289E407AB31571
          D58ED8A2064549EBFFFF282921F501777D6403809CB0BEEF0C265DCA67C09104
          A2F340C37960D731C059A807DEEE8AC061B761E1F7B2019201B85D077FCC167C
          282CE7D32EEC04E66C40280AAA6A45FAC82990D98C7679142D574EE3F9AB7103
          2403D88FCA859B17DF3A6A19101F018A8F02410DB4E30CD473CDD0F2ED3A60ED
          6080940394C19528AB9876F6EE05F2DE00D6E3C0541E684B15D443B5502DF948
          6B0425A5F2E94904FAC728E0BD6ACA014AE1FCDAE69EDD4751A4B65BA0FD9CC7
          F2473B123597F17D5B2914952F91A7C6B1D5E545F0F887F0D8D7B8C601EBE8D3
          175A5D81048ACD403AB99B834BA0ED390075130339402582AA090503C278E2BB
          2619DAE8793846DED61A7CF9F40D64312365B6E1D7A202768D14270A25C984EA
          0A76200FA16F3DE06EEF4BEABE731693D333C211D70C43B22803260995878B71
          4F0E33A0C908E8904748EEA8C38400502E31A7A43B10802E79107DDDD78D805B
          DE617AE469C0F864C681A85B24EA4E7823D610002EC1C70E82FE758036CF336E
          CD25BC9E8865BF10D26AE6D2741065BA70C255047FCF20FAE56623A0A97D8042
          3D8D985B4AAD3E16510A655538D2DF02CF07F7C308C92D46C08DCE0132994C99
          A6AE12560094DD128308662E654317FE75FC05BF6525203DFDDC660000000049
          454E44AE426082}
        Name = 'PngImage9'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000002A744558744372656174696F6E2054696D650044692034204D727A20
          323030332031393A31383A3035202B30313030E27A682B0000000774494D4500
          0000000000000973942E000000097048597300000AF000000AF00142AC349800
          0002D04944415478DAA5936D48536114C7FFD7EBA69BDEA9732ECB3152A25024
          FD607D88A41790308696E9D24A4D4B1412A25414A1C2CA540AF325B3A2B43E54
          A24992042621F9213033324DCB97D2652BCDD6A69BD3BBEDEE76EFDA928820EA
          C01F1E9EE79CDF39CFF39C43E03F8D702DC425D009ECA488B1DBAD60409B56B0
          ED08420E1261E5CFFB6AD0A93321C66E0778CD1BD0905289433F01A8468242E8
          D75A9B930521C7CD29BF8CA960A308C958E28FBBCEC3BCF9884E2410491DEE0F
          4E11883F0DE207A014411EACDBEB9C8C585F39258192F2476169233A22B65F35
          BD6B575A2D90B34259E496A3B3A42BDF2F00C131B46F4B8A5029036508F4F181
          856670EB46275EAAEBE12B8F8490928324BD38CF054E7C4101683B4161F75917
          A0D06D2E3A295C1220F6464FF75B50B41FC60D1A4C67DF8559370CDAF805602D
          B0D92C58139DCF45C8D05ABC1289654E80B4806043768540DB3B8B26691CA254
          D9886B4EC53D55164414050FB1987BB9254C8FBE8234240642AF48B414AD85BA
          C209D894475846C3BC059446820A5A8BA4F2716EF72960B33A021D6269872687
          27B03A2A1F4D051148B9E0043CBA9FAA49E86A51FAC83DD048CE615E5D05A3AD
          071B64AB102EF65D0630343E8E4C42B1310D77F2D4D85FE9047496E1DAE0BACC
          C3A503CDC46D7713449997C010FDE81F1F436E5828842C5F095F810DACD50EBD
          59828775D79156ED047494424D3064FDC48E62A9A2E30CDC53CBE0291AC4026D
          8671EA13F6060770000B27CED98DC09C8E465B43370ED6BAFA80879CF36C5B19
          BE2F5EF3A201D49E128C7C6E868D64B98C8BC80DF5E70259E8BF59A0D7183132
          F415B3D3266D7A0D14C47263205BE81558B7649C26E38F57017337B9201AFA19
          1377EF796835667661DE6AE07EF259F245C4FE360B8E76AD5B6FD36B07C89DE9
          2A8CF53EC187F78BCC928999B133E893FBA268EB49BCF9E330F1F6FC4A54C6D0
          505FA5BB3B0C60F1F84015B2FE7A1AFFD5BE03F78224208E0023B40000000049
          454E44AE426082}
        Name = 'PngImage10'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000002B744558744372656174696F6E2054696D6500536F203137204E6F76
          20323030322031393A31373A3436202B3031303041C0B5F10000000774494D45
          000000000000000973942E000000097048597300000AF000000AF00142AC3498
          000003004944415478DAA5935B48145118C7FFE7CCECECCEB4BBAEE966BAE876
          A5A8E8FA16A25BD1430F51411665949892422108D2851E8A124A307AB00B74B1
          207A29A820A28B45449962A545A6A459965B6BABA8B3BA3B3B33BBA7B3A341EF
          1DF8CE0787F3FDCEEFDC08FEB39154E7A8EA38A905D58327364BE2D6552EC44D
          6B18A669C0340C183CB4B80ECA4C3CEB1A47EDCB74538E8F9D8EDDD978D49A69
          2B79A91B2DEDB6FA9AC5A82E5D8381E04F84C3611042904C26ADB0DBEDF0E778
          71E941074EDD7E0F9D2C30D4BB9B240B2095BE627A7B27EAF6CF474D6980AF6C
          22128958005DD72D835493258ACB0FDAD1D0E1C268F72F441E6E2716C05EDACC
          E29DDDA82BCF43CDDE75E8FDD2876030084AA9559832703A9DDC6006AE72C0F9
          CF391879DB05B569D724C051FE9A695D3DA8DD331347CA0250D598B52A630C89
          44C2324A658748D0C8B770B1CF87E1B66E449A8A270172790BD3FBBEE1F8360F
          0E95E4E3534F3F7E0F0E5A06C49A42A0283272B333D1F8F003AEF4E721DCD2C9
          01FF18909F411CDB22A37ADB0A68461294506E0018FC2612DC408B6BA0491337
          9EF5E272702E42CD1FA13E9902C8152D6CDA7008FE4C82344712095E490502DE
          F3CCC0482A530802070A3286C44CF4BFE884FA78E72440D9D7CAD2A34330B9AA
          96A4882778814D84C883DA040894878DC0ADD820D9291C8A80DEA60FDC60EA0C
          94CA56E6D547A03311F9B9E3A808A4233C3C821FC190059364195ECF341CBBF7
          1D625A16FC4B57A1EDEE1B449E4C19382BDB58161B453429A268D104D421866B
          6F4DEC2E905055A8C09BE347AE7F366695DF847BD632142EF1E04AC373C4FE1E
          A2ABA295F91C31444D011B7D031899B0E34EAF137BD73A71787D0632B2FDFC25
          027965B7E099B712F90B3DB87EE139A28FA7B6602B7AD538DD1D2FF1CC48C306
          F77B0CC287FB835E1C2874A376F31CFE264C8C86BE62F5998F489FB71C814569
          3857FB488B3E2D96277F4D5D35753C0A9CD5F4E8D602DF57A9AA688E047B06B2
          B33C502841FFC080101B0B293BEA7F0D51D75CC325221151C7EB13EF2ACF92FF
          FDCE7F00FBA84F203089644D0000000049454E44AE426082}
        Name = 'PngImage10'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000002B744558744372656174696F6E2054696D65004D6F20313720466562
          20323030332031383A31383A3530202B303130301B8DB1030000000774494D45
          000000000000000973942E000000097048597300000AF000000AF00142AC3498
          0000022B4944415478DAA5D14D6813511007F0FF24BB9B6CB691106B684CA231
          6D63C52A9A100F5210841CB4C52A78A93729AD274FBDD88317D1A2A0770FB512
          41F0033F2E168A27518C07452B8229C662DA34D1D69A6AD2D866935D678D076B
          5303FA607687C7ECEFCD9B25FCE722E31133615CD1B0FF2830FF4FC00D09B72C
          66B8E93BA287812563AFE48122CDA0C8A95E17B86BC30157C0315A9C5EB819FD
          8A9E386076FA296E879ED8F801BD5C54FA2BF0803F70046CA9D6CEAD9EB7D75E
          5E700858F4F747CE48D934724FB38FDCCB384229E4D6048CF57C1D86B69F0C0F
          7E1ECFE8E542B9E2EF6E16309D41B9A4437D924EC8D03BE91526D704D28D08AA
          A1A6843F2810B66D019229C02A0036B15AF67072166AA99BE278A647D10111A7
          68145DF4BB36B5991EFB8EB5746099AFCC53D539C840AC8CC81C6313457C2C5C
          C1EEC613C8E466E85E25B00298F5A157DEE71E6E08DA911F9BD22C3EBB49DAEB
          06043383FC33E43230A7011105B8F4E23DDDD15A560085F5B0AB6DD68CDC6455
          2C6F168EEB1BA4B3D417F6425D04449511A33346647E5FCE26E93E5AE9CFA1E8
          3B71D51809BDC6693D8421F4EF1984F28DBB60403400B59A8FCC27E9762D80CB
          7853FD9947308083ED1711E62BA85FAA5D481C268E583E49D7F5D5C02FC4CC73
          3E8F886B00873611F29FF8D452F56403D00C60E91DC510AC0D84E04403CE41C1
          2E38C41D700A0A5C5CEA65DACB43B4F31C862B133482B69AC0AA6EBA1084C698
          C0217278380A9863A0BD2E506FFD003A3AB711CB8B91170000000049454E44AE
          426082}
        Name = 'PngImage3'
        Background = clWindow
      end>
    Left = 80
    Top = 183
    Bitmap = {}
  end
end
