object DesignForm2: TDesignForm2
  Left = 331
  Top = 110
  Hint = 'Test Hint'
  AutoScroll = False
  Caption = 'DesignForm2'
  ClientHeight = 452
  ClientWidth = 533
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 433
    Width = 533
    Height = 19
    Panels = <>
  end
  object Panel2: TPanel
    Left = 0
    Top = 29
    Width = 533
    Height = 404
    Align = alClient
    TabOrder = 1
    object Label1: TLabel
      Left = 64
      Top = 16
      Width = 32
      Height = 13
      Caption = 'Label1'
    end
    object PageControl1: TPageControl
      Left = 164
      Top = 216
      Width = 173
      Height = 153
      ActivePage = TabSheet2
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'TabSheet1'
      end
      object TabSheet2: TTabSheet
        Caption = 'TabSheet2'
        ImageIndex = 1
      end
    end
    object Button1: TButton
      Left = 60
      Top = 196
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 1
    end
    object Panel1: TPanel
      Left = 60
      Top = 60
      Width = 145
      Height = 125
      Caption = 'Panel1'
      TabOrder = 2
      object Label2: TLabel
        Left = 1
        Top = 1
        Width = 143
        Height = 13
        Align = alTop
        Caption = 'Label2'
        Color = 33023
        ParentColor = False
      end
      object Edit1: TEdit
        Left = 12
        Top = 92
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'Edit1'
      end
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 533
    Height = 29
    Caption = 'ToolBar1'
    TabOrder = 2
  end
  object ApplicationEvents1: TApplicationEvents
    Left = 200
    Top = 8
  end
end
