object DragTestForm: TDragTestForm
  Left = 293
  Top = 110
  Width = 368
  Height = 308
  Caption = 'DragTestForm'
  Color = clBtnFace
  UseDockManager = True
  DockSite = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDockDrop = FormDockDrop
  OnDockOver = FormDockOver
  OnDragDrop = FormDragDrop
  OnDragOver = FormDragOver
  OnGetSiteInfo = FormGetSiteInfo
  OnUnDock = FormUnDock
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 92
    Top = 36
    Width = 31
    Height = 13
    Caption = 'Label1'
    DragMode = dmAutomatic
    OnEndDrag = Panel1EndDrag
  end
  object Panel1: TPanel
    Left = 76
    Top = 68
    Width = 185
    Height = 41
    Caption = 'Panel1'
    DragMode = dmAutomatic
    TabOrder = 0
    OnEndDock = Panel1EndDock
    OnEndDrag = Panel1EndDrag
  end
end
