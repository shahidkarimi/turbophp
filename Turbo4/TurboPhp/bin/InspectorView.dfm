object InspectorForm: TInspectorForm
  Left = 293
  Top = 110
  Width = 237
  Height = 459
  Caption = 'InspectorForm'
  Color = clBtnFace
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 229
    Height = 2
    Align = alTop
    Shape = bsSpacer
  end
  object Bevel2: TBevel
    Left = 0
    Top = 23
    Width = 229
    Height = 2
    Align = alTop
    Shape = bsSpacer
  end
  object CompList: TCompList
    Left = 0
    Top = 2
    Width = 229
    Height = 21
    Align = alTop
    Flat = False
    DreamBorderStyle = dbsSunkenBorder
    UseDreamBorder = True
    OnChange = CompListChange
    object TPopupListBox
      Left = 0
      Top = 0
      Width = 121
      Height = 93
      TabStop = False
      ItemHeight = 13
    end
  end
  object InspectorTabs: TTabControl
    Left = 0
    Top = 25
    Width = 229
    Height = 400
    Align = alClient
    TabOrder = 1
    Tabs.Strings = (
      'Properties'
      'PHP'
      'JavaScript')
    TabIndex = 0
    OnChange = InspectorTabsChange
    object ObjectInspector: TObjectInspector
      Left = 4
      Top = 24
      Width = 221
      Height = 372
      Aliases.Strings = (
        '[Appearance]'
        'Color'
        'Caption'
        'Cursor'
        'Alignment'
        'ParentColor'
        'Transparent'
        'WordWrap'
        'Flat'
        'Glyph'
        'Ctl3d'
        ''
        '[Behavior]'
        'OnEditButtonClick'
        'OnEnter'
        'OnExit'
        'TabOrder'
        'TabStop'
        'DragCursor'
        'DragMode'
        'Cancel'
        'Default'
        'Enabled'
        'Visible'
        'GroupIndex'
        'ImeMode'
        'ImeName'
        ''
        '[List]'
        'IntegralHeight'
        'ItemHeight'
        'Items'
        'MultiSelect'
        ''
        '[Font]'
        'Font'
        'ParentFont'
        ''
        '[Misc]'
        'Name'
        'PopupMenu'
        'HelpContext'
        'Tag'
        'FocusControl'
        ''
        '[Hint]'
        'ParentShowHint'
        'ShowHint'
        'Hint'
        ''
        '[Position]'
        'Height'
        'Top'
        'Left'
        'Width'
        'Autosize'
        'Align'
        ''
        '[Data]'
        'DataSource'
        'DataSet'
        'DataField'
        ''
        '[Keyboard]'
        'OnKeyDown'
        'OnKeyPress'
        'OnKeyUp'
        ''
        '[Mouse]'
        'OnClick'
        'OnDblClick'
        'OnMouseDown'
        'OnMouseMove'
        'OnMouseUp'
        ''
        '[DragDrop]'
        'OnDragDrop'
        'OnDragOver'
        'OnEndDrag'
        'OnStartDrag')
      Align = alClient
      AutoSizeNames = False
      BorderStyle = bsNone
      Colors.BackColor = clBtnFace
      Colors.Col0Color = clBlack
      Colors.Col1Color = clNavy
      Colors.VertLineColor = clBtnShadow
      Colors.HorzLineColor = clBtnShadow
      Colors.HighlightColor = clBlack
      Colors.HighlightBkgnd = clBtnFace
      DefaultRowHeight = 16
      Filter = [tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkSet, tkClass, tkWChar, tkLString, tkWString, tkVariant, tkArray, tkRecord, tkInt64, tkDynArray]
      FixedColWidth = 100
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      GroupFont.Charset = DEFAULT_CHARSET
      GroupFont.Color = clWindowText
      GroupFont.Height = -11
      GroupFont.Name = 'MS Shell Dlg 2'
      GroupFont.Style = [fsBold]
      HiddenProps.Strings = (
        'HelpContext'
        'HelpKeyword'
        'Cursor'
        'HelpType'
        'Hint'
        'Tag'
        'PhpSource'
        'JsSource'
        'JsFilename')
      Style = isDelphi
      Options = [oiHorzLine, oiVertLine, oiSunkenEditor, oiVertLine3D, oiUseAliases, oiSmartPopup, oiShowActiveX, oiShowAxHelp]
      ParentFont = False
      TabOrder = 0
      TrackDestroy = True
      OnChangedPropValue = ObjectInspectorChangedPropValue
      OnShowProperty = ObjectInspectorShowProperty
      ColWidths = (
        100
        120)
    end
  end
end
