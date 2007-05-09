object BrowserForm: TBrowserForm
  Left = 353
  Top = 173
  Width = 605
  Height = 430
  Caption = 'BrowserForm'
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
  object aqDockingSite1: TaqDockingSite
    Left = 0
    Top = 28
    Width = 597
    Height = 368
    Align = alClient
    DockingManager = aqDockingManager1
    Key = '{48D32F04-0B0E-49CB-A1E0-E3ACBD8D8A33}'
  end
  object aqDockingManager1: TaqDockingManager
    AutoDragKey = [dssCtrl, dssLeft]
    CaptionButtons = [dbiUndock, dbiMaximizeRestore, dbiHelp]
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
    ShowContainerCaption = [cntVertical, cntHorizontal]
    ShowImages = [ctCaption, ctTab]
    StyleManager = aqStyleManager1
    Left = 52
    Top = 92
    Style = BrowserForm.aqStyleManager1.aqThemedUIStyle1
    object DisplayDock: TaqDockingControl
      Width = 597
      Height = 326
      Caption = 'Display'
      ImageIndex = -1
      MinWidth = 0
      MinHeight = 0
      ShowCaption = bvFalse
      ShowImage = bvDefault
      Visible = True
      Key = '{AAC572D7-C076-4E2C-99EC-EFCD424B2D92}'
      DesignClientHeight = 326
      DesignClientWidth = 597
      object StatusBar: TStatusBar
        Left = 0
        Top = 307
        Width = 597
        Height = 19
        Panels = <>
        SizeGrip = False
      end
      object dxBarDockControl1: TdxBarDockControl
        Left = 0
        Top = 0
        Width = 597
        Height = 3
        Align = dalTop
        BarManager = dxBarManager1
        Visible = False
      end
      object ScrollBox: TScrollBox
        Left = 0
        Top = 3
        Width = 597
        Height = 304
        HorzScrollBar.Smooth = True
        HorzScrollBar.Style = ssFlat
        HorzScrollBar.Tracking = True
        VertScrollBar.Smooth = True
        VertScrollBar.Style = ssFlat
        VertScrollBar.Tracking = True
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 2
        object Panel1: TPanel
          Left = 0
          Top = 0
          Width = 597
          Height = 24
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          Visible = False
          DesignSize = (
            597
            24)
          object ToolBar1: TToolBar
            Left = 0
            Top = 0
            Width = 92
            Height = 24
            Align = alLeft
            AutoSize = True
            Caption = 'ToolBar1'
            EdgeBorders = []
            Flat = True
            Images = ImageList1
            TabOrder = 0
            Wrapable = False
            object ToolButton5: TToolButton
              Left = 0
              Top = 0
              Action = BackAction
            end
            object ToolButton1: TToolButton
              Left = 23
              Top = 0
              Action = ForwardAction
            end
            object ToolButton4: TToolButton
              Left = 46
              Top = 0
              Action = RefreshAction
            end
            object ToolButton2: TToolButton
              Left = 69
              Top = 0
              Action = StopAction
            end
          end
          object ToolBar2: TToolBar
            Left = 574
            Top = 0
            Width = 23
            Height = 24
            Align = alRight
            AutoSize = True
            Caption = 'ToolBar1'
            EdgeBorders = []
            Flat = True
            Images = ImageList1
            TabOrder = 1
            Wrapable = False
            object ToolButton3: TToolButton
              Left = 0
              Top = 0
              Action = GoAction
            end
          end
          object UrlBox: TComboBox
            Left = 96
            Top = 2
            Width = 476
            Height = 20
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -9
            Font.Name = 'Arial'
            Font.Style = []
            ItemHeight = 12
            ParentFont = False
            TabOrder = 2
          end
        end
      end
    end
    object SourceDock: TaqDockingControl
      Width = 597
      Height = 326
      Caption = 'Source'
      ImageIndex = -1
      MinWidth = 0
      MinHeight = 0
      ShowCaption = bvDefault
      ShowImage = bvDefault
      Visible = True
      Key = '{F8872BE6-0BF0-4C26-9A3E-0618AEB7C029}'
      DesignClientHeight = 326
      DesignClientWidth = 597
    end
  end
  object aqStyleManager1: TaqStyleManager
    Left = 52
    Top = 140
    object aqDefaultUIStyle1: TaqDefaultUIStyle
      ActiveCaptionColor.Bands = 256
      ActiveCaptionColor.EndColor = clWindow
      ActiveCaptionColor.FillType = gtSolid
      ActiveCaptionColor.StartColor = clBtnFace
      ActiveCaptionFont.Charset = DEFAULT_CHARSET
      ActiveCaptionFont.Color = clWindowText
      ActiveCaptionFont.Height = -11
      ActiveCaptionFont.Name = 'MS Shell Dlg 2'
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
      ActiveTabFont.Name = 'MS Shell Dlg 2'
      ActiveTabFont.Style = []
      CaptionButtonSize = 14
      CaptionColor.Bands = 256
      CaptionColor.EndColor = clWindow
      CaptionColor.FillType = gtSolid
      CaptionColor.StartColor = clBtnFace
      CaptionFont.Charset = DEFAULT_CHARSET
      CaptionFont.Color = clWindowText
      CaptionFont.Height = -11
      CaptionFont.Name = 'MS Shell Dlg 2'
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
      TabFont.Name = 'MS Shell Dlg 2'
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
      ActiveCaptionFont.Name = 'MS Shell Dlg 2'
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
      ActiveTabFont.Name = 'MS Shell Dlg 2'
      ActiveTabFont.Style = []
      CaptionButtonSize = 14
      CaptionColor.Bands = 256
      CaptionColor.EndColor = clWindow
      CaptionColor.FillType = gtSolid
      CaptionColor.StartColor = clBtnFace
      CaptionFont.Charset = DEFAULT_CHARSET
      CaptionFont.Color = clWindowText
      CaptionFont.Height = -11
      CaptionFont.Name = 'MS Shell Dlg 2'
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
      TabFont.Name = 'MS Shell Dlg 2'
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
      ActiveCaptionFont.Name = 'MS Shell Dlg 2'
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
      ActiveTabFont.Name = 'MS Shell Dlg 2'
      ActiveTabFont.Style = []
      CaptionButtonSize = 14
      CaptionColor.Bands = 256
      CaptionColor.EndColor = clWindow
      CaptionColor.FillType = gtSolid
      CaptionColor.StartColor = clBtnFace
      CaptionFont.Charset = DEFAULT_CHARSET
      CaptionFont.Color = clWindowText
      CaptionFont.Height = -11
      CaptionFont.Name = 'MS Shell Dlg 2'
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
      TabFont.Name = 'MS Shell Dlg 2'
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
      ActiveCaptionFont.Name = 'MS Shell Dlg 2'
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
      ActiveTabFont.Name = 'MS Shell Dlg 2'
      ActiveTabFont.Style = []
      CaptionButtonSize = 14
      CaptionColor.Bands = 256
      CaptionColor.EndColor = clWindow
      CaptionColor.FillType = gtSolid
      CaptionColor.StartColor = clBtnFace
      CaptionFont.Charset = DEFAULT_CHARSET
      CaptionFont.Color = clWindowText
      CaptionFont.Height = -11
      CaptionFont.Name = 'MS Shell Dlg 2'
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
      TabFont.Name = 'MS Shell Dlg 2'
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
      ActiveCaptionFont.Name = 'MS Shell Dlg 2'
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
      CaptionFont.Name = 'MS Shell Dlg 2'
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
      TabFont.Name = 'MS Shell Dlg 2'
      TabFont.Style = []
      ActiveTabFont.Charset = DEFAULT_CHARSET
      ActiveTabFont.Color = clWindowText
      ActiveTabFont.Height = -11
      ActiveTabFont.Name = 'MS Shell Dlg 2'
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
        DockedDockingStyle = dsTop
        DockedLeft = 0
        DockedTop = 0
        DockingStyle = dsTop
        FloatLeft = 0
        FloatTop = 0
        FloatClientWidth = 0
        FloatClientHeight = 0
        ItemLinks = <
          item
            Item = SizeCombo
            Visible = True
          end>
        OneOnRow = True
        Row = 0
        UseOwnFont = False
        Visible = False
        WholeRow = False
      end
      item
        AllowCustomizing = False
        AllowQuickCustomizing = False
        Caption = 'BrowserBar'
        DockedDockingStyle = dsTop
        DockedLeft = 0
        DockedTop = 0
        DockingStyle = dsTop
        FloatLeft = 564
        FloatTop = 429
        FloatClientWidth = 24
        FloatClientHeight = 24
        ItemLinks = <
          item
            Item = dxBarButton7
            Visible = True
          end
          item
            BeginGroup = True
            Item = dxBarButton2
            Visible = True
          end
          item
            Item = dxBarButton3
            Visible = True
          end
          item
            Item = dxBarButton4
            Visible = True
          end
          item
            Item = dxBarButton5
            Visible = True
          end
          item
            BeginGroup = True
            Item = dxBarButton6
            Visible = True
          end
          item
            Item = UrlCombo
            UserDefine = [udWidth]
            UserWidth = 419
            Visible = True
          end>
        Name = 'BrowserBar'
        OneOnRow = True
        Row = 0
        UseOwnFont = False
        UseRestSpace = True
        Visible = True
        WholeRow = True
      end>
    Categories.Strings = (
      'Default')
    Categories.ItemsVisibles = (
      2)
    Categories.Visibles = (
      True)
    Images = ImageList1
    LargeImages = ImageList1
    PopupMenuLinks = <>
    Style = bmsXP
    UseSystemFont = True
    Left = 148
    Top = 192
    DockControlHeights = (
      0
      0
      28
      0)
    object SizeCombo: TdxBarCombo
      Caption = 'New Item'
      Category = 0
      Hint = 'New Item'
      Visible = ivAlways
      Text = 'Fit to Client'
      OnChange = SizeComboChange
      Width = 100
      Items.Strings = (
        'Fit to Client'
        '640 x 480'
        '800 x 600'
        '1024 x 768'
        '1600 x 1200')
      ItemIndex = 0
    end
    object CustomdxBarCombo1: TCustomdxBarCombo
      Caption = 'New Item'
      Category = 0
      Hint = 'New Item'
      Visible = ivAlways
      Width = 100
    end
    object dxBarCombo1: TdxBarCombo
      Caption = 'New Item'
      Category = 0
      Hint = 'New Item'
      Visible = ivAlways
      Width = 100
      ItemIndex = -1
    end
    object dxBarButton1: TdxBarButton
      Caption = 'New Item'
      Category = 0
      Hint = 'New Item'
      Visible = ivAlways
    end
    object dxBarButton2: TdxBarButton
      Action = BackAction
      Category = 0
    end
    object dxBarButton3: TdxBarButton
      Action = ForwardAction
      Category = 0
    end
    object dxBarButton4: TdxBarButton
      Action = RefreshAction
      Category = 0
    end
    object dxBarButton5: TdxBarButton
      Action = StopAction
      Category = 0
    end
    object UrlCombo: TdxBarCombo
      Align = iaClient
      Caption = 'New Item'
      Category = 0
      Hint = 'New Item'
      Visible = ivAlways
      Width = 100
      ItemIndex = -1
    end
    object dxBarButton6: TdxBarButton
      Action = GoAction
      Align = iaRight
      Category = 0
    end
    object dxBarButton7: TdxBarButton
      Caption = 'Theme'
      Category = 0
      Hint = 'Theme'
      Visible = ivAlways
      OnClick = dxBarButton7Click
    end
    object dxBarButton8: TdxBarButton
      Caption = 'Source'
      Category = 0
      Hint = 'Source'
      Visible = ivAlways
    end
  end
  object ImageList1: TImageList
    Left = 148
    Top = 140
    Bitmap = {
      494C010105000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B5848400B584
      8400B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B5848400B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6A59C00FFEF
      D600F7E7C600F7DEBD00F7DEB500F7D6AD00F7D6A500EFCE9C00EFCE9400EFCE
      9400EFCE9400F7D69C00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6A59C00FFEF
      D600C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A5
      9C00C6A59C00EFCE9C00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6ADA500FFEF
      E700F7E7D600F7E7CE00F7DEC600F7DEBD00F7D6B500F7D6AD00EFCE9C00EFCE
      9C00EFCE9400EFCE9C00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6ADA500FFF7
      E700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEB500F7D6B500F7D6AD00EFCE
      9C00EFCE9C00EFCE9400B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CEB5AD00FFFF
      F700C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A5
      9C00C6A59C00EFCE9C00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6B5AD00FFFF
      FF00FFF7EF00FFEFE700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEBD00F7D6
      AD00F7D6A500F7D6A500B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6BDB500FFFF
      FF00FFF7F700FFF7EF00FFEFDE00F7E7D600F7E7CE00F7E7C600F7DEC600F7DE
      BD00F7D6B500F7D6AD00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6BDB500FFFF
      FF00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A5
      9C00C6A59C00F7DEB500B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEBDB500FFFF
      FF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00F7E7D600F7E7CE00F7DE
      C600F7DEC600F7D6B500B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEC6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00FFEFDE00FFEF
      D600E7DEC600C6BDAD00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500FFFF
      FF00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00FFF7EF00F7E7D600C6A5
      9400B5948C00B58C8400B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700E7CECE00BD8C
      7300EFB57300EFA54A00C6846B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EFCEBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7D6CE00C694
      7B00FFC67300CE94730000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500FFF7
      F700FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00E7CECE00C694
      7B00CE9C84000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500EFCE
      B500EFCEB500EFCEB500EFCEB500E7C6B500E7C6B500EFCEB500D6BDB500BD84
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5636B00A563
      6B00A5636B00A5636B00A5636B00A5636B00A5636B00A5636B00A5636B00A563
      6B00A5636B00A5636B00A5636B00000000000000000000000000000000000000
      0000000000000031000000420000005200000052000000420000003100000000
      000000000000000000000000000000000000FFFFFF0084B58400107310001873
      18001873180018731800187B1800187B1800107B1000107B1000087B0800087B
      0800007B0000007300007BB57B00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5636B00FFEF
      C600C6CE9400D6CE9400EFCE9C00E7CE9400EFC68400EFBD8400EFBD7B00EFBD
      8400EFBD8400EFC68400A5636B00000000000000000000000000000000000042
      000000420000007B0800009C0800009C0800009C0800009C0800007B0800004A
      0000004A0000000000000000000000000000FFFFFF0010841000218C21002994
      2900319431003194310029942900299C2900219C210018A5180018A5180010A5
      100008A5080000940000006B0000FFFFFF00000000005A6BEF001029A5000010
      9C0000109C0000109C0000109C0000109C0000109C0000109C0000109C000821
      9C005A6BEF000000000000000000000000000000000000000000A5636B00FFEF
      CE009CBD7300299C21006BAD4A0021941000219410005AA53900CEB57300EFBD
      7B00EFBD7B00EFC68400A5636B00000000000000000000000000004A08000063
      080000A5100000A50800009C0800009C0800009C0800009C080000A5080000A5
      0800006B0800003100000000000000000000FFFFFF00188C180029942900399C
      3900399C3900399C390039A53900A5D6A500FFFFFF0021AD210018AD180010B5
      100008AD0800009C0000007B0000FFFFFF00000000001029C6000018C6000821
      C6001029C6001029C6000829CE001029CE001029CE000021CE000018CE000010
      AD0010219C000000000000000000000000000000000000000000A5635A00FFEF
      DE00BDCE9C00108C08000084000000840000008400000084000029941800DEBD
      7B00EFBD7B00EFC68400A5636B000000000000000000004A0800006B100008A5
      210008A51800009C0800009C0800009C0800009C0800009C0800009C0800009C
      080000A50800006B0800004A000000000000FFFFFF0021942100399C390042A5
      42004AA54A0042A5420042A54200FFFFFF00FFFFFF00FFFFFF0021B5210018B5
      180010AD100008A50800087B0800FFFFFF00000000000018CE001031D6001831
      D6002139E7002942E7002142E7001842E7001039E7000831E7000029E7000018
      CE0000109C000000000000000000000000000000000000000000A5635A00FFF7
      E700BDCE9C00189410000084000018941000ADBD730073AD4A000084000073AD
      4A00EFBD8400EFC68400A5636B000000000000000000004A080010AD310008AD
      290008A5180000A510004AC65200E7F7EF00D6F7DE0010AD2100009C0800009C
      0800009C080000A50800004A000000000000FFFFFF0029942900429C42004AA5
      4A0052A552004AA54A0042A5420042AD4200FFFFFF00FFFFFF00FFFFFF0018B5
      180010AD100010A5100008840800FFFFFF00000000000021D6001831D6002942
      E700314AE700294AE700294AE7001842E7001042E7001039E7000831E7000021
      CE0000109C000000000000000000000000000000000000000000A5736B00FFF7
      EF00BDD6A500088C0800008400000084000084B55A00EFCEA500A5B56B006BAD
      4A00EFC68C00EFC68400A5636B0000000000005200000884210010B5420010A5
      3100089C180042BD4A00F7FFF700FFFFFF0084D68C0000A51000009C0800009C
      0800009C080000A50800007B080000420000FFFFFF00319C31004AA54A0052A5
      520052A552004AA54A004AA54A0042A5420039AD3900FFFFFF00FFFFFF00FFFF
      FF0018AD180018A5180010841000FFFFFF00000000001031D6002142E7003952
      E7003152E700314AE700294AE7001842E7001839E7001039E7000831E7001031
      CE0000109C000000000000000000000000000000000000000000A5736B00FFFF
      FF00E7E7D600A5CE94009CC6840094BD73009CBD7300EFD6AD00EFCEA5009CC6
      7B00EFC69400EFC68C00A5636B00000000000052000010A5420018B54A0010AD
      310042BD4A00F7FFF700FFFFFF0073D67B00009C0800009C0800009C0800009C
      0800009C080000A508000094080000420000FFFFFF00429C420052A55200FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00219C210018841800FFFFFF00000000002139E700314AE7003952
      E7003152E700314AE700294AE7001842E7001039E7001031E7000831E7001031
      CE0000109C000000000000000000000000000000000000000000BD846B00FFFF
      FF00A5DEA500FFEFE700F7EFD6009CC6840094BD730094BD73009CBD7300EFCE
      A500EFCE9C00F7CE9400A5636B0000000000006B100021B5520021B5520073D6
      9400EFFFEF00FFFFFF00F7FFF700B5EFC600B5E7BD00ADE7B500ADE7B500ADE7
      B500ADE7B500009C0800009C0800004A0000FFFFFF0042A542005AAD5A00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00219C2100217B2100FFFFFF0000000000314AE700425AE7004252
      E7003152E700314AE7002942E7001839DE001031DE001031DE001031DE001031
      CE0000109C000000000000000000000000000000000000000000BD846B00FFFF
      FF0073C67300ADD6A500FFEFE70084C673000084000000840000088C0800EFD6
      AD00EFCEA500F7D6A500A5636B00000000000873100039BD6B0029BD5A00BDEF
      CE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00009C0800009C080000520000FFFFFF0052A5520063AD630063AD
      63005AAD5A0052A552004AA54A00429C4200399C3900FFFFFF00FFFFFF00FFFF
      FF00299C290029942900217B2100FFFFFF00000000003952E7004A63E700425A
      E7003952E7003142E7002942DE001839DE001031D6001031DE001031DE001031
      CE0000109C000000000000000000000000000000000000000000D6946B00FFFF
      FF0084CE8400008400007BC67300ADD6A5001894180000840000108C0800F7D6
      B500F7D6AD00EFCEA500A5636B00000000000873100052C67B0042C6730029BD
      5A00A5E7BD00FFFFFF00F7FFF70084DEA50042BD5A0042BD5A0042BD5A0042BD
      5A0042BD5A0008A51800009C080000420000FFFFFF0052A552006BB56B006BB5
      6B005AAD5A0052A552004AA54A00429C4200FFFFFF00FFFFFF00FFFFFF002994
      29002994290029942900217B2100FFFFFF00000000004252E700526BEF004A63
      E7004252DE00314AE7002942DE002139DE001839D6001831DE001031DE001031
      CE0000109C000000000000000000000000000000000000000000D6946B00FFFF
      FF00F7F7EF0029A5290000840000008400000084000000840000108C0800FFEF
      CE00DECEB500B5AD9400A5636B0000000000087310004ABD6B0084DEA50021B5
      520021B5520094DEB500FFFFFF00E7F7EF0052C6730010AD310010AD310010AD
      310008A5290008A51800008C080000420000FFFFFF005AAD5A007BBD7B0073BD
      730063AD63005AAD5A0052A55200FFFFFF00FFFFFF00FFFFFF00319C3100319C
      31003194310031943100217B2100FFFFFF00000000004A63E7006B84EF005A73
      EF004A63E7004252E7003152E700314ADE002942DE002142DE002139D6001031
      CE0008189C000000000000000000000000000000000000000000DE9C7300FFFF
      FF00FFFFFF00DEF7DE0063BD6300219C2100219C210073BD6B00299C2100946B
      5200A56B5A00A56B5A00A5636B00000000000000000021A53100ADE7C6006BCE
      8C0010AD4A0018B54A008CDEAD00FFFFFF00F7FFF70029BD520010A5310008A5
      290008A5210008AD1800006B080000000000FFFFFF006BB56B008CC68C007BBD
      7B006BB56B0063B5630063AD6300B5DEB500FFFFFF004AA54A004AA54A0042A5
      4200399C390031943100217B2100FFFFFF00000000005A73EF008C94EF006B7B
      EF005273EF005263E7004A63E7004A5AE700425AE7003952E700294AE7001031
      CE001831A5000000000000000000000000000000000000000000DE9C7300FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00DEF7DE00DEF7DE00FFFFF700ADB594008C6B
      5200E79C5200E78C3100B56B4A00000000000000000021A5310052C67300BDEF
      D60063CE8C0021B5520018B5520094DEB500ADE7C60021B5520010AD390010AD
      310010AD3100109C2100006B080000000000FFFFFF0073BD73009CCE9C008CC6
      8C007BBD7B0073BD73006BB56B0063B5630063AD63005AAD5A0052AD52004AA5
      4A0042A542003194310018731800FFFFFF00000000005A73EF005A73EF004A5A
      E7003952E700314AE700314AE7002942E7002939E7002139D6001839D6001831
      C6005A6BEF000000000000000000000000000000000000000000E7AD7B00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00DEC6C600A56B
      5A00FFB55A00BD7B5A0000000000000000000000000000000000109C21006BCE
      8C00D6F7E7009CE7B50052C67B0039BD630029BD5A0031BD630031BD630021BD
      4A0010A53100006308000000000000000000FFFFFF00B5DEB50073BD730063B5
      63005AAD5A0052A5520052A552004AA54A004AA54A0042A54200429C4200399C
      390031943100218C21008CBD8C00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7AD7B00F7F7
      EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00DEC6C600A56B
      5A00BD846B000000000000000000000000000000000000000000000000004ABD
      63004ABD630094DEB500BDEFD600A5E7C6008CDEAD007BDE9C004AC67B00189C
      3900189C390000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7AD7B00D694
      6B00D6946B00D6946B00D6946B00D6946B00D6946B00D6946B00D6946B00A56B
      5A00000000000000000000000000000000000000000000000000000000000000
      00000000000021A5310039B5520042BD630042BD630029A54A00108C29000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000C001000000000000C001000000000000
      C001000000000000C001000000000000C001000000000000C001000000000000
      C001000000000000C001000000000000C001000000000000C001000000000000
      C001000000000000C001000000000000C001000000000000C003000000000000
      C007000000000000C00F0000000000008001FFFFC001F81F0000FFFFC001E007
      00008007C001C00300008007C001800100008007C001800100008007C0010000
      00008007C001000000008007C001000000008007C001000000008007C0010000
      00008007C001000000008007C001800100008007C001800100008007C003C003
      0000FFFFC007E0078001FFFFC00FF81F00000000000000000000000000000000
      000000000000}
  end
  object ActionList1: TActionList
    Images = ImageList1
    Left = 148
    Top = 91
    object BackAction: TAction
      Caption = 'BackAction'
      ImageIndex = 3
      OnExecute = BackActionExecute
      OnUpdate = BackActionUpdate
    end
    object ForwardAction: TAction
      Caption = 'ForwardAction'
      ImageIndex = 0
      OnExecute = ForwardActionExecute
      OnUpdate = ForwardActionUpdate
    end
    object RefreshAction: TAction
      Caption = 'RefreshAction'
      ImageIndex = 2
      OnExecute = RefreshActionExecute
      OnUpdate = RefreshActionUpdate
    end
    object StopAction: TAction
      Caption = 'StopAction'
      ImageIndex = 1
      OnExecute = StopActionExecute
      OnUpdate = StopActionUpdate
    end
    object GoAction: TAction
      Caption = 'GoAction'
      ImageIndex = 4
      OnExecute = GoActionExecute
      OnUpdate = GoActionUpdate
    end
  end
end
