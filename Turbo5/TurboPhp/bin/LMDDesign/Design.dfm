object DesignForm: TDesignForm
  Left = 351
  Top = 127
  Width = 633
  Height = 388
  Caption = 'DesignForm'
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
  object LMDDesigner1: TLMDDesigner
    Grid.XStep = 4
    Grid.YStep = 4
    ShowNonvisualComponents = True
    Left = 468
    Top = 16
  end
  object LMDDesignManager1: TLMDDesignManager
    Designers = <
      item
        Designer = LMDDesigner1
      end>
    AlwaysShowNonvisualComponents = True
    ExcludeDesignControl = True
    Left = 468
    Top = 52
  end
end
