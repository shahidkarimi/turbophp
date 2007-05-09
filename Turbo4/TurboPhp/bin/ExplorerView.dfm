object ExplorerForm: TExplorerForm
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
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object NoFolderLabel: TLabel
    Left = 0
    Top = 0
    Width = 290
    Height = 49
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'No Folder Selected'
    Layout = tlCenter
  end
  object ExplorerTree: TVirtualExplorerTree
    Left = 0
    Top = 49
    Width = 290
    Height = 425
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
    Visible = False
    Columns = <>
  end
end
