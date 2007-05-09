object ControllerModule: TControllerModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 331
  Top = 114
  Height = 153
  Width = 220
  object ActionList1: TActionList
    Left = 28
    Top = 11
    object PublishAction: TAction
      Caption = 'Publish'
      OnExecute = PublishActionExecute
    end
    object SaveAsAction: TAction
      Caption = 'Save As...'
      OnExecute = SaveAsActionExecute
    end
    object OpenAction: TAction
      Caption = 'Open...'
      ImageIndex = 1
      OnExecute = OpenActionExecute
    end
    object SetupDatabasesAction: TAction
      Caption = 'Setup Databases...'
      OnExecute = SetupDatabasesActionExecute
    end
    object SaveAction: TAction
      Caption = 'Save'
      ImageIndex = 2
      OnExecute = SaveActionExecute
      OnUpdate = SaveActionUpdate
    end
    object NewTurboPhpAction: TAction
      Caption = 'New TurboPhp Document'
      ImageIndex = 0
      OnExecute = NewTurboPhpActionExecute
    end
    object CloseAllAction: TAction
      Caption = 'Close All'
      OnExecute = CloseAllActionExecute
    end
    object SetupServersAction: TAction
      Caption = 'Setup Servers...'
      OnExecute = SetupServersActionExecute
    end
    object SaveProjectAction: TAction
      Caption = 'Save Project'
      OnExecute = SaveProjectActionExecute
      OnUpdate = SaveProjectActionUpdate
    end
    object SaveProjectAsAction: TAction
      Caption = 'Save Project As...'
      OnExecute = SaveProjectAsActionExecute
    end
    object CloseProjectAction: TAction
      Caption = 'Close Project'
      OnExecute = CloseProjectActionExecute
    end
    object ViewPreviewAction: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Preview'
      Checked = True
      OnExecute = ViewPreviewActionExecute
    end
    object NewTurboAjaxAction: TAction
      Caption = 'New TurboAjax Document'
      OnExecute = NewTurboAjaxActionExecute
    end
  end
  object OpenDialog: TOpenDialog
    Left = 100
    Top = 68
  end
  object SaveDialog: TSaveDialog
    Left = 32
    Top = 68
  end
end
