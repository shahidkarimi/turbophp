object LrProjectForm: TLrProjectForm
  Left = 351
  Top = 117
  Width = 669
  Height = 393
  Caption = 'LrProjectForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Tree: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 661
    Height = 359
    Align = alClient
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoVisible]
    Header.Style = hsXPStyle
    NodeDataSize = 4
    ParentBackground = False
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.SelectionOptions = [toRightClickSelect]
    TreeOptions.StringOptions = [toAutoAcceptEditChange]
    OnDblClick = TreeDblClick
    OnDragAllowed = TreeDragAllowed
    OnDragOver = TreeDragOver
    OnDragDrop = TreeDragDrop
    OnEditing = TreeEditing
    OnGetText = TreeGetText
    OnGetImageIndex = TreeGetImageIndex
    OnInitChildren = TreeInitChildren
    OnInitNode = TreeInitNode
    OnNewText = TreeNewText
    Columns = <
      item
        Position = 0
        Width = 320
        WideText = 'Name'
      end
      item
        Position = 1
        Width = 364
        WideText = 'Path'
      end>
  end
end
