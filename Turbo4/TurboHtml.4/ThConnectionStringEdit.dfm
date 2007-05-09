object ThConnectionStringEditForm: TThConnectionStringEditForm
  Left = 293
  Top = 110
  Width = 384
  Height = 150
  Caption = 'Edit Connection String'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 31
    Top = 17
    Width = 85
    Height = 13
    Caption = 'Connection String'
    ParentShowHint = False
    ShowHint = False
    Layout = tlCenter
  end
  object ConnectionStringEdit: TDCEdit
    Left = 30
    Top = 35
    Width = 312
    Height = 21
    Button1.Width = 0
    Button1.Cursor = crDefault
    Button1.Kind = bkDropDown
    Button1.NumGlyphs = 1
    Button1.ParentShowHint = True
    Button1.ShowHint = False
    Button1.Visible = True
    Button2.Width = 0
    Button2.Cursor = crDefault
    Button2.Kind = bkDots
    Button2.NumGlyphs = 1
    Button2.ParentShowHint = True
    Button2.ShowHint = False
    Button2.Visible = True
    Caption = 'ConnectionStringEdit'
    DropOnClick = True
    Flat = False
    DreamBorderStyle = dbsSunkenBorder
    UseDreamBorder = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    NumButtons = 2
    ParentColor = False
    ParentFont = False
    PopupWindowClass = 'TPopupListBox'
    ReadOnly = False
    TabOrder = 0
    TabStop = True
    OnButton2Click = ConnectionStringEditButton2Click
    object TPopupListBox
      Left = 0
      Top = 0
      Width = 121
      Height = 93
      TabStop = False
      ItemHeight = 13
    end
  end
  object OkButton: TButton
    Left = 273
    Top = 69
    Width = 69
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object ADOConnection: TADOConnection
    Left = 40
    Top = 64
  end
end
