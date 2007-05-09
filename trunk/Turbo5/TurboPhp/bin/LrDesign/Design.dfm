object DesignForm: TDesignForm
  Left = 404
  Top = 260
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 438
  ClientWidth = 553
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object DesignController1: TDesignController
    OnChange = DesignController1Change
    OnGetAddClass = DesignController1GetAddClass
    OnSelectionChange = DesignController1SelectionChange
    Left = 40
    Top = 20
  end
end
