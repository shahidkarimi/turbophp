object PaletteForm: TPaletteForm
  Left = 368
  Top = 115
  Width = 185
  Height = 388
  Caption = 'PaletteForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PaletteScroll: TScrollBox
    Left = 0
    Top = 0
    Width = 177
    Height = 333
    VertScrollBar.Smooth = True
    VertScrollBar.Style = ssFlat
    VertScrollBar.Tracking = True
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clBtnShadow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 0
    object LrCollapsable1: TLrCollapsable
      Left = 0
      Top = 0
      Width = 177
      Height = 34
      Align = alTop
      AutoSize = True
      Color = 16382457
      Caption = 'LrCollapsable1'
      HeaderColor = 16053492
      Visible = False
      ExtraProps = 33
      object LMDGlyphLabel3: TLMDGlyphLabel
        Left = 0
        Top = 18
        Width = 177
        Height = 16
        Align = alTop
        Alignment.Alignment = agTopLeft
        Bevel.Mode = bmCustom
        Caption = 'LMDGlyphLabel1'
        Color = 14019316
        ImageList = LMDImageList1
        ParentColor = False
      end
    end
  end
  object DCCompPalette1: TDCCompPalette
    Left = 0
    Top = 333
    Width = 177
    Height = 21
    Align = alBottom
    MouseGlyph.Data = {
      96010000424D9601000000000000760000002800000018000000180000000100
      0400000000002001000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333333333333333333333333333333333333333333333
      333333333333333333333333333333333007333333333333333333330FF07333
      33333333333333330FF073333333333333307330FF0733333333333333300770
      FF073333333333333330F00FF0733333333333333330FFFFF077733333333333
      3330FFFFF0000733333333333330FFFFFFF07333333333333330FFFFFF073333
      333333333330FFFFF0733333333333333330FFFF07333333333333333330FFF0
      73333333333333333330FF0733333333333333333330F0733333333333333333
      3330073333333333333333333333333333333333333333333333333333333333
      3333333333333333333333333333333333333333333333333333}
    ParentShowHint = False
    TabOrder = 1
    Visible = False
    AutoSize = False
    RegistryKey = 'Palette'
  end
  object LMDImageList1: TLMDImageList
    Left = 20
    Top = 72
    object TLMDImageListItem
      Internal = {001000000010000000040000000000000000}
    end
  end
end
