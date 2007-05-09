object Form1: TForm1
  Left = 293
  Top = 113
  Width = 529
  Height = 309
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 60
    Width = 521
    Height = 215
    Align = alClient
    TabOrder = 0
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 521
    Height = 19
    AutoSize = True
    ButtonHeight = 19
    ButtonWidth = 64
    Caption = 'ToolBar1'
    EdgeBorders = []
    Flat = True
    List = True
    ShowCaptions = True
    TabOrder = 1
    Wrapable = False
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      AutoSize = True
      Caption = 'Listen...'
      ImageIndex = 0
      OnClick = ToolButton1Click
    end
    object DisconnectButton: TToolButton
      Left = 56
      Top = 0
      AutoSize = True
      Caption = 'Disconnect'
      ImageIndex = 1
      OnClick = DisconnectButtonClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 19
    Width = 521
    Height = 41
    Align = alTop
    TabOrder = 2
    DesignSize = (
      521
      41)
    object Edit1: TEdit
      Left = 8
      Top = 10
      Width = 421
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'run'
    end
    object SendButton: TButton
      Left = 440
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Send'
      TabOrder = 1
      OnClick = SendButtonClick
    end
  end
  object IdSimpleServer1: TIdSimpleServer
    OnStatus = IdSimpleServer1Status
    MaxLineAction = maException
    ReadTimeout = 0
    BoundPort = 9000
    Left = 84
    Top = 132
  end
  object XmlToDomParser1: TXmlToDomParser
    DOMImpl = DomImplementation1
    Left = 80
    Top = 200
  end
  object DomImplementation1: TDomImplementation
    Left = 192
    Top = 200
  end
end
