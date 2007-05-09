object PhpViewForm: TPhpViewForm
  Left = 293
  Top = 110
  Width = 520
  Height = 457
  Caption = 'PhpViewForm'
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
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 512
    Height = 21
    Hint = 
      'Warning: editing raw PHP file. Publishing a page will overwrite ' +
      'generated files.'
    Align = alTop
    AutoSize = False
    Caption = 
      ' Warning: editing raw PHP file. Publishing a page will overwrite' +
      ' generated files.'
    Color = clYellow
    ParentColor = False
    Layout = tlCenter
  end
  object ClientDockSite: TdxDockSite
    Left = 0
    Top = 21
    Width = 512
    Height = 402
    Align = alClient
    DockType = 0
    OriginalWidth = 512
    OriginalHeight = 402
    object dxLayoutDockSite1: TdxLayoutDockSite
      Left = 0
      Top = 0
      Width = 512
      Height = 402
      DockType = 1
      OriginalWidth = 300
      OriginalHeight = 200
    end
    object dxTabContainerDockSite2: TdxTabContainerDockSite
      Left = 0
      Top = 0
      Width = 512
      Height = 402
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
        Width = 508
        Height = 368
        AllowFloating = True
        AutoHide = False
        Caption = 'Code'
        DockType = 1
        OriginalWidth = 626
        OriginalHeight = 140
      end
      object HtmlDock: TdxDockPanel
        Left = 0
        Top = 0
        Width = 508
        Height = 368
        Visible = False
        AllowFloating = True
        AutoHide = False
        Caption = 'HTML'
        DockType = 1
        OriginalWidth = 626
        OriginalHeight = 140
      end
      object PreviewDock: TdxDockPanel
        Left = 0
        Top = 0
        Width = 508
        Height = 368
        AllowFloating = True
        AutoHide = False
        Caption = 'Preview'
        DockType = 1
        OriginalWidth = 626
        OriginalHeight = 140
      end
    end
  end
end
