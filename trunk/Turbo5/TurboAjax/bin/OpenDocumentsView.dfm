object OpenDocumentsForm: TOpenDocumentsForm
  Left = 350
  Top = 248
  Width = 641
  Height = 491
  Caption = 'OpenDocumentsForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DocumentTabs: TJvTabBar
    Left = 0
    Top = 0
    Width = 633
    AutoFreeClosed = False
    SelectBeforeClose = True
    Tabs = <>
    OnTabClosing = DocumentTabsTabClosing
    OnTabClosed = DocumentTabsTabClosed
    OnTabSelected = DocumentTabsTabSelected
  end
  object DocumentPanel: TPanel
    Left = 0
    Top = 23
    Width = 633
    Height = 434
    Align = alClient
    TabOrder = 1
  end
end
