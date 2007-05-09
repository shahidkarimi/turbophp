object DatabaseSetupForm: TDatabaseSetupForm
  Left = 531
  Top = 143
  Width = 536
  Height = 577
  AutoSize = True
  BorderWidth = 8
  Caption = 'Database Setup'
  Color = clBtnFace
  Constraints.MaxWidth = 536
  Constraints.MinWidth = 536
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ServerTabs: TLrTabControl
    Left = 0
    Top = 45
    Width = 512
    Height = 24
    Align = alTop
    GlyphVisible = False
    OnSelect = ServerTabsSelect
    Selected = 0
    TabHeight = 24
    Tabs.Strings = (
      'Local'
      'Remote')
  end
  object Bevel1: TBevel
    Left = 0
    Top = 41
    Width = 512
    Height = 4
    Align = alTop
    Shape = bsSpacer
    Visible = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 512
    Height = 41
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Color = clWhite
    TabOrder = 0
    object Label4: TLabel
      Left = 8
      Top = 12
      Width = 87
      Height = 13
      Caption = 'Connection name:'
    end
    object Edit1: TEdit
      Left = 100
      Top = 10
      Width = 173
      Height = 21
      TabOrder = 0
      Text = 'Default MySql'
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 508
    Width = 512
    Height = 19
    Align = alTop
    Panels = <>
  end
  object SettingsPanel: TPanel
    Left = 0
    Top = 69
    Width = 512
    Height = 398
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 2
    object Bevel7: TBevel
      Left = 0
      Top = 137
      Width = 512
      Height = 4
      Align = alTop
      Shape = bsSpacer
      Visible = False
    end
    object Bevel5: TBevel
      Left = 0
      Top = 245
      Width = 512
      Height = 4
      Align = alTop
      Shape = bsSpacer
      Visible = False
    end
    object Bevel6: TBevel
      Left = 0
      Top = 65
      Width = 512
      Height = 4
      Align = alTop
      Shape = bsSpacer
      Visible = False
    end
    object Bevel2: TBevel
      Left = 0
      Top = 101
      Width = 512
      Height = 4
      Align = alTop
      Shape = bsSpacer
      Visible = False
    end
    object Bevel3: TBevel
      Left = 0
      Top = 173
      Width = 512
      Height = 4
      Align = alTop
      Shape = bsSpacer
      Visible = False
    end
    object Bevel4: TBevel
      Left = 0
      Top = 209
      Width = 512
      Height = 4
      Align = alTop
      Shape = bsSpacer
      Visible = False
    end
    object DatabasePanel: TPanel
      Left = 0
      Top = 141
      Width = 512
      Height = 32
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 0
      object Label3: TLabel
        Left = 40
        Top = 8
        Width = 50
        Height = 13
        Alignment = taRightJustify
        Caption = 'Database:'
      end
      object DatabaseCombo: TComboBox
        Left = 96
        Top = 6
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object UserPanel: TPanel
      Left = 0
      Top = 105
      Width = 512
      Height = 32
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 1
      object Label6: TLabel
        Left = 38
        Top = 8
        Width = 52
        Height = 13
        Alignment = taRightJustify
        Caption = 'Username:'
      end
      object Label7: TLabel
        Left = 256
        Top = 8
        Width = 50
        Height = 13
        Caption = 'Password:'
      end
      object UserEdit: TEdit
        Left = 96
        Top = 6
        Width = 145
        Height = 21
        TabOrder = 0
        OnExit = AuthEditExit
      end
      object PasswordEdit: TEdit
        Left = 316
        Top = 6
        Width = 145
        Height = 21
        PasswordChar = '*'
        TabOrder = 1
        OnExit = AuthEditExit
      end
    end
    object TypePanel: TPanel
      Left = 0
      Top = 69
      Width = 512
      Height = 32
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 2
      object Label1: TLabel
        Left = 27
        Top = 8
        Width = 63
        Height = 13
        Alignment = taRightJustify
        Caption = 'Server Type:'
      end
      object TypeCombo: TComboBox
        Left = 96
        Top = 6
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 0
        Text = 'MySql'
        OnChange = TypeComboChange
        Items.Strings = (
          'MySql'
          'Firebird'
          'Interbase5'
          'Interbase6'
          'SQL Server'
          'Oracle'
          'MS Access'
          'ODBC'
          'Custom')
      end
    end
    object SameAsPanel: TPanel
      Left = 0
      Top = 33
      Width = 512
      Height = 32
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 3
      object CheckBox3: TCheckBox
        Left = 28
        Top = 8
        Width = 65
        Height = 17
        Caption = 'Same As:'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object ComboBox1: TComboBox
        Left = 96
        Top = 6
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 1
        Text = 'Design'
        OnChange = TypeComboChange
        Items.Strings = (
          'Design')
      end
    end
    object FilePanel: TPanel
      Left = 0
      Top = 177
      Width = 512
      Height = 32
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 4
      object Label5: TLabel
        Left = 21
        Top = 8
        Width = 69
        Height = 13
        Alignment = taRightJustify
        Caption = 'Database File:'
      end
      object FilenameEdit: TEdit
        Left = 96
        Top = 6
        Width = 333
        Height = 19
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Small Fonts'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object BrowseButton: TButton
        Left = 436
        Top = 5
        Width = 65
        Height = 20
        Caption = 'Browse'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Small Fonts'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = BrowseButtonClick
      end
    end
    object Panel6: TPanel
      Left = 0
      Top = 0
      Width = 512
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 5
      DesignSize = (
        512
        33)
      object Label10: TLabel
        Left = 8
        Top = 8
        Width = 495
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '  Properties'
        Color = 15987699
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
    end
    object DesignPanel: TPanel
      Left = 0
      Top = 281
      Width = 512
      Height = 117
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 6
      object Panel9: TPanel
        Left = 0
        Top = 0
        Width = 512
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        DesignSize = (
          512
          33)
        object Label11: TLabel
          Left = 8
          Top = 6
          Width = 495
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = '  Design Time Connection'
          Color = 15987699
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Shell Dlg 2'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Layout = tlCenter
        end
      end
      object DesignODBCPanel: TPanel
        Left = 0
        Top = 33
        Width = 512
        Height = 84
        Align = alTop
        BevelOuter = bvNone
        Color = clWhite
        TabOrder = 1
        object ODBCLabel: TLabel
          Left = 27
          Top = 56
          Width = 63
          Height = 13
          Alignment = taRightJustify
          Caption = 'ODBC String:'
        end
        object ConnectionStringEdit: TEdit
          Left = 96
          Top = 52
          Width = 333
          Height = 19
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Small Fonts'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object ConnectionEditButton: TButton
          Left = 436
          Top = 50
          Width = 45
          Height = 20
          Caption = 'Edit'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Small Fonts'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = ConnectionEditButtonClick
        end
        object CustomODBCBox: TCheckBox
          Left = 96
          Top = 30
          Width = 209
          Height = 17
          Caption = 'Custom ODBC Connection String'
          TabOrder = 2
          OnClick = CustomODBCBoxClick
        end
        object TestButton: TButton
          Left = 228
          Top = 5
          Width = 37
          Height = 20
          Caption = 'Test'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Small Fonts'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnClick = TestButtonClick
        end
        object ConnectedBox: TCheckBox
          Left = 96
          Top = 7
          Width = 133
          Height = 17
          Caption = 'Connection Successful'
          Enabled = False
          TabOrder = 4
        end
      end
    end
    object ADODBPanel: TPanel
      Left = 0
      Top = 213
      Width = 512
      Height = 32
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 7
      object ADODBLabel: TLabel
        Left = 28
        Top = 8
        Width = 62
        Height = 13
        Alignment = taRightJustify
        Caption = 'ADODB DNS:'
      end
      object DNSEdit: TEdit
        Left = 96
        Top = 6
        Width = 333
        Height = 19
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Small Fonts'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
    object ODBCPanel: TPanel
      Left = 0
      Top = 249
      Width = 512
      Height = 32
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 8
      object Label2: TLabel
        Left = 27
        Top = 8
        Width = 63
        Height = 13
        Alignment = taRightJustify
        Caption = 'ODBC String:'
      end
      object Edit2: TEdit
        Left = 96
        Top = 6
        Width = 333
        Height = 19
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Small Fonts'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object Button1: TButton
        Left = 436
        Top = 5
        Width = 45
        Height = 20
        Caption = 'Edit'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Small Fonts'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = ConnectionEditButtonClick
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 467
    Width = 512
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object GenerateButton: TButton
      Left = 0
      Top = 8
      Width = 129
      Height = 25
      Caption = 'Test PHP Connection'
      TabOrder = 0
      OnClick = GenerateButtonClick
    end
    object Button2: TButton
      Left = 436
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object OkButton: TButton
      Left = 353
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 2
      OnClick = OkButtonClick
    end
  end
  object Connection: TADOConnection
    ConnectionString = 'DRIVER={MySQL ODBC 3.51 Driver};'
    LoginPrompt = False
    Left = 372
    Top = 16
  end
  object DbsQuery: TADOQuery
    Connection = Connection
    CursorType = ctStatic
    Parameters = <>
    SQL.Strings = (
      'SHOW DATABASES')
    Left = 432
    Top = 16
  end
  object OpenDialog: TOpenDialog
    Left = 312
    Top = 15
  end
end
