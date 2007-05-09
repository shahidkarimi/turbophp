object ConnectionStringEditForm: TConnectionStringEditForm
  Left = 293
  Top = 110
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Edit Connection String'
  ClientHeight = 244
  ClientWidth = 376
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
    Left = 28
    Top = 72
    Width = 85
    Height = 13
    Caption = 'Connection String'
    ParentShowHint = False
    ShowHint = False
    Layout = tlCenter
  end
  object Label2: TLabel
    Left = 28
    Top = 20
    Width = 84
    Height = 13
    Caption = 'Connection Name'
    ParentShowHint = False
    ShowHint = False
    Layout = tlCenter
  end
  object OkButton: TButton
    Left = 272
    Top = 200
    Width = 69
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = OkButtonClick
  end
  object NameEdit: TDCEdit
    Left = 28
    Top = 36
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
    Button2.Visible = False
    Caption = 'NameEdit'
    Flat = False
    DreamBorderStyle = dbsSunkenBorder
    UseDreamBorder = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    PopupWindowClass = 'TPopupListBox'
    ReadOnly = False
    TabOrder = 1
    TabStop = True
    OnButton2Click = ConnectionStringEditClick
    OnChange = NameEditChange
    object TPopupListBox
      Left = 0
      Top = 0
      Width = 121
      Height = 93
      TabStop = False
      ItemHeight = 13
    end
  end
  object ConnectMemo: TMemo
    Left = 28
    Top = 88
    Width = 312
    Height = 97
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Button1: TButton
    Left = 28
    Top = 200
    Width = 69
    Height = 25
    Caption = 'Edit...'
    TabOrder = 3
    OnClick = ConnectionStringEditClick
  end
  object Button2: TButton
    Left = 192
    Top = 200
    Width = 69
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object ADOConnection: TADOConnection
    Left = 336
    Top = 8
  end
end
