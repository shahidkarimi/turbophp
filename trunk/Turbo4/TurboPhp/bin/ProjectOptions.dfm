object ProjectOptionsForm: TProjectOptionsForm
  Left = 314
  Top = 112
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Project Options'
  ClientHeight = 319
  ClientWidth = 407
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    407
    319)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 11
    Width = 390
    Height = 254
    ActivePage = PublishSheet
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 0
    object PublishSheet: TTabSheet
      Caption = 'Publishing'
      object Label2: TLabel
        Left = 12
        Top = 29
        Width = 354
        Height = 13
        AutoSize = False
        Caption = 'Publish Folder'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label3: TLabel
        Left = 12
        Top = 123
        Width = 354
        Height = 13
        AutoSize = False
        Caption = 'Server URL'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label1: TLabel
        Left = 12
        Top = 45
        Width = 354
        Height = 32
        AutoSize = False
        Caption = 
          'When project files are Published, output files (and folders) wil' +
          'l be placed in this folder.'
        WordWrap = True
      end
      object Label4: TLabel
        Left = 12
        Top = 139
        Width = 354
        Height = 16
        AutoSize = False
        Caption = 
          'If the Publish Folder is accessible from a web server, put the U' +
          'RL here.'
        WordWrap = True
      end
      object PublishRootEdit: TDCEdit
        Left = 12
        Top = 77
        Width = 354
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
        Button2.Kind = bkCustom
        Button2.Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF078DBE
          078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078D
          BEFF00FFFF00FFFF00FF078DBE25A1D171C6E884D7FA66CDF965CDF965CDF965
          CDF965CDF865CDF965CDF866CEF93AADD81999C9FF00FFFF00FF078DBE4CBCE7
          39A8D1A0E2FB6FD4FA6FD4F96ED4FA6FD4F96FD4FA6FD4FA6FD4FA6ED4F93EB1
          D9C9F0F3078DBEFF00FF078DBE72D6FA078DBEAEE9FC79DCFB79DCFB79DCFB79
          DCFB79DCFB7ADCFB79DCFA79DCFA44B5D9C9F0F3078DBEFF00FF078DBE79DDFB
          1899C79ADFF392E7FC84E4FB83E4FC83E4FC84E4FC83E4FC83E4FB84E5FC48B9
          DAC9F0F31496C4FF00FF078DBE82E3FC43B7DC65C2E0ABF0FC8DEBFC8DEBFC8D
          EBFD8DEBFD8DEBFC8DEBFD8DEBFC4CBBDAC9F0F3C9F0F3078DBE078DBE8AEAFC
          77DCF3219CC7FEFFFFC8F7FDC9F7FDC9F7FDC9F7FEC8F7FEC9F7FDC8F7FE9BD5
          E6EAFEFED2F3F8078DBE078DBE93F0FE93F0FD1697C5078DBE078DBE078DBE07
          8DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE9BF5FE
          9AF6FE9AF6FE9BF5FD9BF6FE9AF6FE9BF5FE9AF6FD9BF5FE9AF6FE9AF6FE0989
          BAFF00FFFF00FFFF00FF078DBEFEFEFEA0FBFFA0FBFEA0FBFEA1FAFEA1FBFEA0
          FAFEA1FBFEA1FBFFA0FBFFA1FBFF0989BAFF00FFFF00FFFF00FFFF00FF078DBE
          FEFEFEA5FEFFA5FEFFA5FEFF078DBE078DBE078DBE078DBE078DBE078DBEFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FF078DBE078DBE078DBE078DBEFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        Button2.NumGlyphs = 1
        Button2.ParentShowHint = True
        Button2.ShowHint = False
        Button2.Visible = True
        Caption = 'ProjectFolderEdit'
        Flat = False
        DreamBorderStyle = dbsSunkenBorder
        UseDreamBorder = True
        NumButtons = 2
        ParentColor = False
        PopupWindowClass = 'TPopupListBox'
        ReadOnly = False
        TabOrder = 0
        TabStop = True
        OnButton2Click = PublishRootEditButton2Click
        OnChange = RootEditChange
        object TPopupListBox
          Left = 0
          Top = 0
          Width = 121
          Height = 93
          TabStop = False
          ItemHeight = 13
        end
      end
      object UrlRootEdit: TDCEdit
        Left = 12
        Top = 159
        Width = 354
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
        Button2.Kind = bkCustom
        Button2.Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF078DBE
          078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078D
          BEFF00FFFF00FFFF00FF078DBE25A1D171C6E884D7FA66CDF965CDF965CDF965
          CDF965CDF865CDF965CDF866CEF93AADD81999C9FF00FFFF00FF078DBE4CBCE7
          39A8D1A0E2FB6FD4FA6FD4F96ED4FA6FD4F96FD4FA6FD4FA6FD4FA6ED4F93EB1
          D9C9F0F3078DBEFF00FF078DBE72D6FA078DBEAEE9FC79DCFB79DCFB79DCFB79
          DCFB79DCFB7ADCFB79DCFA79DCFA44B5D9C9F0F3078DBEFF00FF078DBE79DDFB
          1899C79ADFF392E7FC84E4FB83E4FC83E4FC84E4FC83E4FC83E4FB84E5FC48B9
          DAC9F0F31496C4FF00FF078DBE82E3FC43B7DC65C2E0ABF0FC8DEBFC8DEBFC8D
          EBFD8DEBFD8DEBFC8DEBFD8DEBFC4CBBDAC9F0F3C9F0F3078DBE078DBE8AEAFC
          77DCF3219CC7FEFFFFC8F7FDC9F7FDC9F7FDC9F7FEC8F7FEC9F7FDC8F7FE9BD5
          E6EAFEFED2F3F8078DBE078DBE93F0FE93F0FD1697C5078DBE078DBE078DBE07
          8DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE9BF5FE
          9AF6FE9AF6FE9BF5FD9BF6FE9AF6FE9BF5FE9AF6FD9BF5FE9AF6FE9AF6FE0989
          BAFF00FFFF00FFFF00FF078DBEFEFEFEA0FBFFA0FBFEA0FBFEA1FAFEA1FBFEA0
          FAFEA1FBFEA1FBFFA0FBFFA1FBFF0989BAFF00FFFF00FFFF00FFFF00FF078DBE
          FEFEFEA5FEFFA5FEFFA5FEFF078DBE078DBE078DBE078DBE078DBE078DBEFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FF078DBE078DBE078DBE078DBEFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        Button2.NumGlyphs = 1
        Button2.ParentShowHint = True
        Button2.ShowHint = False
        Button2.Visible = False
        Caption = 'ProjectFolderEdit'
        Flat = False
        DreamBorderStyle = dbsSunkenBorder
        UseDreamBorder = True
        ParentColor = False
        PopupWindowClass = 'TPopupListBox'
        ReadOnly = False
        TabOrder = 1
        TabStop = True
        object TPopupListBox
          Left = 0
          Top = 0
          Width = 121
          Height = 93
          TabStop = False
          ItemHeight = 13
        end
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Other Folders'
      ImageIndex = 1
      object Label6: TLabel
        Left = 12
        Top = 13
        Width = 354
        Height = 13
        AutoSize = False
        Caption = 'Library Folder'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label7: TLabel
        Left = 12
        Top = 61
        Width = 354
        Height = 13
        AutoSize = False
        Caption = 'Images Folder'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label9: TLabel
        Left = 12
        Top = 113
        Width = 354
        Height = 13
        AutoSize = False
        Caption = 'Support Folder'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label11: TLabel
        Left = 12
        Top = 165
        Width = 354
        Height = 13
        AutoSize = False
        Caption = 'Configuration Folder'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object DCEdit1: TDCEdit
        Left = 12
        Top = 31
        Width = 354
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
        Button2.Kind = bkCustom
        Button2.Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF078DBE
          078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078D
          BEFF00FFFF00FFFF00FF078DBE25A1D171C6E884D7FA66CDF965CDF965CDF965
          CDF965CDF865CDF965CDF866CEF93AADD81999C9FF00FFFF00FF078DBE4CBCE7
          39A8D1A0E2FB6FD4FA6FD4F96ED4FA6FD4F96FD4FA6FD4FA6FD4FA6ED4F93EB1
          D9C9F0F3078DBEFF00FF078DBE72D6FA078DBEAEE9FC79DCFB79DCFB79DCFB79
          DCFB79DCFB7ADCFB79DCFA79DCFA44B5D9C9F0F3078DBEFF00FF078DBE79DDFB
          1899C79ADFF392E7FC84E4FB83E4FC83E4FC84E4FC83E4FC83E4FB84E5FC48B9
          DAC9F0F31496C4FF00FF078DBE82E3FC43B7DC65C2E0ABF0FC8DEBFC8DEBFC8D
          EBFD8DEBFD8DEBFC8DEBFD8DEBFC4CBBDAC9F0F3C9F0F3078DBE078DBE8AEAFC
          77DCF3219CC7FEFFFFC8F7FDC9F7FDC9F7FDC9F7FEC8F7FEC9F7FDC8F7FE9BD5
          E6EAFEFED2F3F8078DBE078DBE93F0FE93F0FD1697C5078DBE078DBE078DBE07
          8DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE9BF5FE
          9AF6FE9AF6FE9BF5FD9BF6FE9AF6FE9BF5FE9AF6FD9BF5FE9AF6FE9AF6FE0989
          BAFF00FFFF00FFFF00FF078DBEFEFEFEA0FBFFA0FBFEA0FBFEA1FAFEA1FBFEA0
          FAFEA1FBFEA1FBFFA0FBFFA1FBFF0989BAFF00FFFF00FFFF00FFFF00FF078DBE
          FEFEFEA5FEFFA5FEFFA5FEFF078DBE078DBE078DBE078DBE078DBE078DBEFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FF078DBE078DBE078DBE078DBEFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        Button2.NumGlyphs = 1
        Button2.ParentShowHint = True
        Button2.ShowHint = False
        Button2.Visible = False
        Caption = 'ProjectFolderEdit'
        Enabled = False
        Flat = False
        DreamBorderStyle = dbsSunkenBorder
        UseDreamBorder = True
        ParentColor = False
        PopupWindowClass = 'TPopupListBox'
        ReadOnly = False
        TabOrder = 0
        TabStop = True
        Text = 'libs\'
        object TPopupListBox
          Left = 0
          Top = 0
          Width = 121
          Height = 93
          TabStop = False
          ItemHeight = 13
        end
      end
      object DCEdit2: TDCEdit
        Left = 12
        Top = 79
        Width = 354
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
        Button2.Kind = bkCustom
        Button2.Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF078DBE
          078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078D
          BEFF00FFFF00FFFF00FF078DBE25A1D171C6E884D7FA66CDF965CDF965CDF965
          CDF965CDF865CDF965CDF866CEF93AADD81999C9FF00FFFF00FF078DBE4CBCE7
          39A8D1A0E2FB6FD4FA6FD4F96ED4FA6FD4F96FD4FA6FD4FA6FD4FA6ED4F93EB1
          D9C9F0F3078DBEFF00FF078DBE72D6FA078DBEAEE9FC79DCFB79DCFB79DCFB79
          DCFB79DCFB7ADCFB79DCFA79DCFA44B5D9C9F0F3078DBEFF00FF078DBE79DDFB
          1899C79ADFF392E7FC84E4FB83E4FC83E4FC84E4FC83E4FC83E4FB84E5FC48B9
          DAC9F0F31496C4FF00FF078DBE82E3FC43B7DC65C2E0ABF0FC8DEBFC8DEBFC8D
          EBFD8DEBFD8DEBFC8DEBFD8DEBFC4CBBDAC9F0F3C9F0F3078DBE078DBE8AEAFC
          77DCF3219CC7FEFFFFC8F7FDC9F7FDC9F7FDC9F7FEC8F7FEC9F7FDC8F7FE9BD5
          E6EAFEFED2F3F8078DBE078DBE93F0FE93F0FD1697C5078DBE078DBE078DBE07
          8DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE9BF5FE
          9AF6FE9AF6FE9BF5FD9BF6FE9AF6FE9BF5FE9AF6FD9BF5FE9AF6FE9AF6FE0989
          BAFF00FFFF00FFFF00FF078DBEFEFEFEA0FBFFA0FBFEA0FBFEA1FAFEA1FBFEA0
          FAFEA1FBFEA1FBFFA0FBFFA1FBFF0989BAFF00FFFF00FFFF00FFFF00FF078DBE
          FEFEFEA5FEFFA5FEFFA5FEFF078DBE078DBE078DBE078DBE078DBE078DBEFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FF078DBE078DBE078DBE078DBEFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        Button2.NumGlyphs = 1
        Button2.ParentShowHint = True
        Button2.ShowHint = False
        Button2.Visible = False
        Caption = 'ProjectFolderEdit'
        Enabled = False
        Flat = False
        DreamBorderStyle = dbsSunkenBorder
        UseDreamBorder = True
        ParentColor = False
        PopupWindowClass = 'TPopupListBox'
        ReadOnly = False
        TabOrder = 1
        TabStop = True
        Text = 'images\'
        object TPopupListBox
          Left = 0
          Top = 0
          Width = 121
          Height = 93
          TabStop = False
          ItemHeight = 13
        end
      end
      object DCEdit3: TDCEdit
        Left = 12
        Top = 131
        Width = 354
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
        Button2.Kind = bkCustom
        Button2.Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF078DBE
          078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078D
          BEFF00FFFF00FFFF00FF078DBE25A1D171C6E884D7FA66CDF965CDF965CDF965
          CDF965CDF865CDF965CDF866CEF93AADD81999C9FF00FFFF00FF078DBE4CBCE7
          39A8D1A0E2FB6FD4FA6FD4F96ED4FA6FD4F96FD4FA6FD4FA6FD4FA6ED4F93EB1
          D9C9F0F3078DBEFF00FF078DBE72D6FA078DBEAEE9FC79DCFB79DCFB79DCFB79
          DCFB79DCFB7ADCFB79DCFA79DCFA44B5D9C9F0F3078DBEFF00FF078DBE79DDFB
          1899C79ADFF392E7FC84E4FB83E4FC83E4FC84E4FC83E4FC83E4FB84E5FC48B9
          DAC9F0F31496C4FF00FF078DBE82E3FC43B7DC65C2E0ABF0FC8DEBFC8DEBFC8D
          EBFD8DEBFD8DEBFC8DEBFD8DEBFC4CBBDAC9F0F3C9F0F3078DBE078DBE8AEAFC
          77DCF3219CC7FEFFFFC8F7FDC9F7FDC9F7FDC9F7FEC8F7FEC9F7FDC8F7FE9BD5
          E6EAFEFED2F3F8078DBE078DBE93F0FE93F0FD1697C5078DBE078DBE078DBE07
          8DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE9BF5FE
          9AF6FE9AF6FE9BF5FD9BF6FE9AF6FE9BF5FE9AF6FD9BF5FE9AF6FE9AF6FE0989
          BAFF00FFFF00FFFF00FF078DBEFEFEFEA0FBFFA0FBFEA0FBFEA1FAFEA1FBFEA0
          FAFEA1FBFEA1FBFFA0FBFFA1FBFF0989BAFF00FFFF00FFFF00FFFF00FF078DBE
          FEFEFEA5FEFFA5FEFFA5FEFF078DBE078DBE078DBE078DBE078DBE078DBEFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FF078DBE078DBE078DBE078DBEFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        Button2.NumGlyphs = 1
        Button2.ParentShowHint = True
        Button2.ShowHint = False
        Button2.Visible = False
        Caption = 'ProjectFolderEdit'
        Enabled = False
        Flat = False
        DreamBorderStyle = dbsSunkenBorder
        UseDreamBorder = True
        ParentColor = False
        PopupWindowClass = 'TPopupListBox'
        ReadOnly = False
        TabOrder = 2
        TabStop = True
        Text = 'support\'
        object TPopupListBox
          Left = 0
          Top = 0
          Width = 121
          Height = 93
          TabStop = False
          ItemHeight = 13
        end
      end
      object DCEdit4: TDCEdit
        Left = 12
        Top = 183
        Width = 354
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
        Button2.Kind = bkCustom
        Button2.Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF078DBE
          078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078D
          BEFF00FFFF00FFFF00FF078DBE25A1D171C6E884D7FA66CDF965CDF965CDF965
          CDF965CDF865CDF965CDF866CEF93AADD81999C9FF00FFFF00FF078DBE4CBCE7
          39A8D1A0E2FB6FD4FA6FD4F96ED4FA6FD4F96FD4FA6FD4FA6FD4FA6ED4F93EB1
          D9C9F0F3078DBEFF00FF078DBE72D6FA078DBEAEE9FC79DCFB79DCFB79DCFB79
          DCFB79DCFB7ADCFB79DCFA79DCFA44B5D9C9F0F3078DBEFF00FF078DBE79DDFB
          1899C79ADFF392E7FC84E4FB83E4FC83E4FC84E4FC83E4FC83E4FB84E5FC48B9
          DAC9F0F31496C4FF00FF078DBE82E3FC43B7DC65C2E0ABF0FC8DEBFC8DEBFC8D
          EBFD8DEBFD8DEBFC8DEBFD8DEBFC4CBBDAC9F0F3C9F0F3078DBE078DBE8AEAFC
          77DCF3219CC7FEFFFFC8F7FDC9F7FDC9F7FDC9F7FEC8F7FEC9F7FDC8F7FE9BD5
          E6EAFEFED2F3F8078DBE078DBE93F0FE93F0FD1697C5078DBE078DBE078DBE07
          8DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE9BF5FE
          9AF6FE9AF6FE9BF5FD9BF6FE9AF6FE9BF5FE9AF6FD9BF5FE9AF6FE9AF6FE0989
          BAFF00FFFF00FFFF00FF078DBEFEFEFEA0FBFFA0FBFEA0FBFEA1FAFEA1FBFEA0
          FAFEA1FBFEA1FBFFA0FBFFA1FBFF0989BAFF00FFFF00FFFF00FFFF00FF078DBE
          FEFEFEA5FEFFA5FEFFA5FEFF078DBE078DBE078DBE078DBE078DBE078DBEFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FF078DBE078DBE078DBE078DBEFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        Button2.NumGlyphs = 1
        Button2.ParentShowHint = True
        Button2.ShowHint = False
        Button2.Visible = False
        Caption = 'ProjectFolderEdit'
        Enabled = False
        Flat = False
        DreamBorderStyle = dbsSunkenBorder
        UseDreamBorder = True
        ParentColor = False
        PopupWindowClass = 'TPopupListBox'
        ReadOnly = False
        TabOrder = 3
        TabStop = True
        Text = 'support\'
        object TPopupListBox
          Left = 0
          Top = 0
          Width = 121
          Height = 93
          TabStop = False
          ItemHeight = 13
        end
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 272
    Width = 407
    Height = 47
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object OkButton: TButton
      Left = 159
      Top = 10
      Width = 73
      Height = 27
      Caption = 'Ok'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ModalResult = 1
      ParentFont = False
      TabOrder = 0
      OnClick = OkButtonClick
    end
    object Button3: TButton
      Left = 319
      Top = 10
      Width = 73
      Height = 27
      Caption = 'Help'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object Button2: TButton
      Left = 239
      Top = 10
      Width = 73
      Height = 27
      Cancel = True
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ModalResult = 2
      ParentFont = False
      TabOrder = 2
    end
  end
  object DCPathDialog1: TDCPathDialog
    Flags = [pdfReturnOnlyFSDirs]
    Root = pdrNone
    Left = 272
    Top = 223
  end
end
