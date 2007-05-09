object DesignForm: TDesignForm
  Left = 468
  Top = 172
  Width = 385
  Height = 324
  AlphaBlendValue = 0
  Caption = 'DesignForm'
  Color = clBtnFace
  TransparentColorValue = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object DCLiteDesigner: TDCLiteDesigner
    AllowUndo = True
    GridStepX = 4
    GridStepY = 4
    LimitInfos = <
      item
        AllowedActions = []
      end
      item
        AllowedActions = []
      end>
    ShowInspector = False
    ShowCaptions = True
    SnapToGrid = True
    StartHotKey = 16507
    ShowHints = True
    StopHotKey = 16507
    ShowPalette = False
    ShowAlignPalette = False
    IsStored = True
    OnDragDrop = DCLiteDesignerDragDrop
    OnSelectionChanged = DCLiteDesignerSelectionChanged
    Left = 28
    Top = 16
  end
end
