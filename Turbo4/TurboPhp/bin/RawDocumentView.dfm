object RawDocumentForm: TRawDocumentForm
  Left = 304
  Top = 165
  Width = 460
  Height = 298
  Caption = 'RawDocumentForm'
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
  object ClientDockSite: TdxDockSite
    Left = 0
    Top = 0
    Width = 452
    Height = 264
    Align = alClient
    DockType = 0
    OriginalWidth = 452
    OriginalHeight = 264
    object dxLayoutDockSite1: TdxLayoutDockSite
      Left = 0
      Top = 0
      Width = 452
      Height = 264
      DockType = 1
      OriginalWidth = 300
      OriginalHeight = 200
    end
    object dxTabContainerDockSite: TdxTabContainerDockSite
      Left = 0
      Top = 0
      Width = 452
      Height = 264
      ActiveChildIndex = 0
      AllowFloating = True
      AutoHide = False
      CaptionButtons = []
      ShowCaption = False
      DockType = 1
      OriginalWidth = 252
      OriginalHeight = 140
      object CodeDock: TdxDockPanel
        Left = 0
        Top = 0
        Width = 448
        Height = 230
        AllowFloating = True
        AutoHide = False
        Caption = 'Code'
        DockType = 1
        OriginalWidth = 185
        OriginalHeight = 140
      end
      object PreviewDock: TdxDockPanel
        Left = 0
        Top = 0
        Width = 448
        Height = 230
        AllowFloating = True
        AutoHide = False
        Caption = 'Preview'
        DockType = 1
        OriginalWidth = 305
        OriginalHeight = 272
      end
    end
  end
end
