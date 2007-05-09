object Form1: TForm1
  Left = 338
  Top = 111
  Width = 794
  Height = 410
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
  object VirtualStringTree1: TVirtualStringTree
    Left = 0
    Top = 207
    Width = 786
    Height = 169
    Align = alBottom
    BevelInner = bvNone
    BevelKind = bkFlat
    BorderStyle = bsNone
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Shell Dlg 2'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoVisible]
    Header.Style = hsXPStyle
    ParentBackground = False
    RootNodeCount = 3
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toReadOnly]
    TreeOptions.PaintOptions = [toHotTrack, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    Columns = <
      item
        Position = 0
        Width = 128
        WideText = 'Alpha'
      end
      item
        Position = 1
        Width = 128
        WideText = 'Beta'
      end
      item
        Position = 2
        Width = 128
        WideText = 'Gamma'
      end>
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 786
    Height = 207
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Shell Dlg 2'
    TitleFont.Style = []
  end
  object DataSource1: TDataSource
    DataSet = ADODataSet1
    Left = 500
    Top = 212
  end
  object ADODataSet1: TADODataSet
    Active = True
    ConnectionString = 
      'Provider=MSDASQL.1;Password=;Extended Properties="DATAB' +
      'ASE=agilent;DRIVER={MySQL ODBC 3.51 Driver};OPTION=0;PWD=d0gbrea' +
      'th;PORT=0;UID=root"'
    CursorType = ctStatic
    CommandText = 'certtbl'
    CommandType = cmdTableDirect
    ParamCheck = False
    Parameters = <>
    Left = 428
    Top = 212
  end
end
