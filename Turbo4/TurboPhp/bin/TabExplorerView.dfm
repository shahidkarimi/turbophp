object TabExplorerForm: TTabExplorerForm
  Left = 293
  Top = 110
  Width = 298
  Height = 508
  Caption = 'Explorer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl: TcxTabControl
    Left = 0
    Top = 0
    Width = 290
    Height = 474
    Align = alClient
    TabIndex = 0
    TabOrder = 0
    Tabs.Strings = (
      'All')
    OnChange = TabControlChange
    ClientRectBottom = 474
    ClientRectRight = 290
    ClientRectTop = 23
    object ExplorerTree: TVirtualExplorerTree
      Left = 0
      Top = 42
      Width = 290
      Height = 432
      Active = True
      Align = alClient
      ColumnDetails = cdUser
      ColumnMenuItemCount = 8
      DefaultNodeHeight = 17
      DragHeight = 250
      DragWidth = 150
      FileObjects = [foFolders, foNonFolders]
      FileSizeFormat = fsfExplorer
      FileSort = fsFileType
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Shell Dlg 2'
      Header.Font.Style = []
      Header.MainColumn = -1
      Header.Options = [hoColumnResize, hoDrag]
      HintMode = hmHint
      ParentColor = False
      RootFolder = rfDesktop
      TabOrder = 0
      TabStop = True
      TreeOptions.PaintOptions = [toShowButtons, toShowTreeLines, toUseBlendedImages, toGhostedIfUnfocused]
      TreeOptions.VETShellOptions = [toRightAlignSizeColumn, toContextMenus]
      TreeOptions.VETSyncOptions = [toCollapseTargetFirst, toExpandTarget, toSelectTarget]
      TreeOptions.VETMiscOptions = [toBrowseExecuteFolder, toBrowseExecuteFolderShortcut, toChangeNotifierThread, toRightButtonSelect, toAutoScrollHorz]
      TreeOptions.VETImageOptions = [toImages, toMarkCutAndCopy]
      OnChange = ExplorerTreeChange
      Columns = <>
    end
    object ToolBar1: TToolBar
      Left = 0
      Top = 23
      Width = 290
      Height = 19
      AutoSize = True
      ButtonHeight = 19
      ButtonWidth = 66
      Caption = 'ToolBar1'
      EdgeBorders = []
      Flat = True
      List = True
      ShowCaptions = True
      TabOrder = 1
      Visible = False
      Wrapable = False
      object ToolButton2: TToolButton
        Left = 0
        Top = 0
        AutoSize = True
        Caption = 'Recent'
        DropdownMenu = RecentDropdown
        ImageIndex = 1
        Style = tbsDropDown
      end
      object ToolButton1: TToolButton
        Left = 63
        Top = 0
        AutoSize = True
        Caption = 'New Folder'
        ImageIndex = 0
      end
      object ToolButton3: TToolButton
        Left = 133
        Top = 0
        AutoSize = True
        Caption = 'Refresh'
        ImageIndex = 1
      end
    end
  end
  object RecentDropdown: TPopupMenu
    Left = 160
    Top = 32
  end
end
