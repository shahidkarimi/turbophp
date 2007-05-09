object MainForm: TMainForm
  Left = 483
  Top = 236
  Width = 619
  Height = 465
  Caption = 'MainForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TBXDock1: TTBXDock
    Left = 0
    Top = 0
    Width = 611
    Height = 23
    object TBXToolbar1: TTBXToolbar
      Left = 0
      Top = 0
      Caption = 'TBXToolbar1'
      TabOrder = 0
      object TBXItem5: TTBXItem
        Action = ControllerModule.NewAction
      end
      object TBXItem3: TTBXItem
        Action = ControllerModule.LoadAction
      end
      object TBXItem2: TTBXItem
        Action = ControllerModule.SaveAsAction
      end
      object TBXSeparatorItem1: TTBXSeparatorItem
      end
      object TBXItem1: TTBXItem
        Action = ControllerModule.GenerateAction
      end
      object TBXSeparatorItem2: TTBXSeparatorItem
      end
      object TBXItem4: TTBXItem
        Action = ControllerModule.SetupDatabaseAction
      end
    end
  end
  object TBXSwitcher1: TTBXSwitcher
    Theme = 'Default'
    Left = 196
    Top = 80
  end
end
