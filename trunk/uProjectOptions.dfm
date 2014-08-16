object fmProjectOptions: TfmProjectOptions
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Project options'
  ClientHeight = 225
  ClientWidth = 454
  Color = clBtnFace
  Constraints.MaxHeight = 253
  Constraints.MaxWidth = 460
  Constraints.MinHeight = 250
  Constraints.MinWidth = 460
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pcProjectOpt: TPageControl
    Left = 0
    Top = 0
    Width = 454
    Height = 185
    ActivePage = tsProject
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object tsProject: TTabSheet
      Caption = 'Project settings'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      object lbeApplication: TLabeledEdit
        Left = 7
        Top = 24
        Width = 378
        Height = 21
        EditLabel.Width = 52
        EditLabel.Height = 13
        EditLabel.Caption = 'Application'
        TabOrder = 0
      end
      object lbeProjectName: TLabeledEdit
        Left = 7
        Top = 72
        Width = 378
        Height = 21
        EditLabel.Width = 63
        EditLabel.Height = 13
        EditLabel.Caption = 'Project name'
        TabOrder = 2
      end
      object lbeProjectStorage: TLabeledEdit
        Left = 7
        Top = 120
        Width = 378
        Height = 21
        EditLabel.Width = 112
        EditLabel.Height = 13
        EditLabel.Caption = 'Project session storage'
        ReadOnly = True
        TabOrder = 4
      end
      object btnOpenApplication: TBitBtn
        Left = 384
        Top = 22
        Width = 33
        Height = 25
        Action = acOpenApplication
        TabOrder = 1
      end
      object btnSaveProjectName: TBitBtn
        Left = 384
        Top = 70
        Width = 33
        Height = 25
        Action = acSaveProjectName
        TabOrder = 3
      end
      object btnSaveProjectStorage: TBitBtn
        Left = 384
        Top = 118
        Width = 33
        Height = 25
        Action = acSaveProjectStorage
        TabOrder = 5
      end
    end
    object tsSources: TTabSheet
      Caption = 'Source settings'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageIndex = 1
      ParentFont = False
      object lbeProjectSource: TLabeledEdit
        Left = 7
        Top = 24
        Width = 378
        Height = 21
        EditLabel.Width = 69
        EditLabel.Height = 13
        EditLabel.Caption = 'Project source'
        TabOrder = 0
      end
      object btnProjectSource: TBitBtn
        Left = 384
        Top = 22
        Width = 33
        Height = 25
        Action = acProjectSource
        TabOrder = 1
      end
      object lbeDelphiSource: TLabeledEdit
        Left = 7
        Top = 72
        Width = 378
        Height = 21
        EditLabel.Width = 97
        EditLabel.Height = 13
        EditLabel.Caption = 'Delphi library source'
        TabOrder = 2
      end
      object btnDelphiSource: TBitBtn
        Left = 384
        Top = 70
        Width = 33
        Height = 25
        Action = acDelphiSource
        TabOrder = 3
      end
    end
    object tsRunParams: TTabSheet
      Caption = 'Run parameters'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageIndex = 2
      ParentFont = False
      object lbeParameters: TLabeledEdit
        Left = 7
        Top = 24
        Width = 378
        Height = 21
        EditLabel.Width = 55
        EditLabel.Height = 13
        EditLabel.Caption = 'Parameters'
        TabOrder = 0
      end
      object lbeWorkDir: TLabeledEdit
        Left = 7
        Top = 72
        Width = 378
        Height = 21
        EditLabel.Width = 85
        EditLabel.Height = 13
        EditLabel.Caption = 'Working directory'
        TabOrder = 1
      end
      object btnSelWorkDir: TBitBtn
        Left = 384
        Top = 70
        Width = 33
        Height = 25
        Action = acSelWorkDir
        TabOrder = 2
      end
    end
  end
  object pActions: TPanel
    Left = 0
    Top = 185
    Width = 454
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'pActions'
    ShowCaption = False
    TabOrder = 1
    DesignSize = (
      454
      40)
    object btnOk: TBitBtn
      Left = 282
      Top = 8
      Width = 75
      Height = 25
      Action = acSave
      Anchors = [akRight, akBottom]
      Caption = 'Save'
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 369
      Top = 8
      Width = 75
      Height = 25
      Action = acCancel
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      TabOrder = 1
    end
  end
  object alProjectOpt: TActionList
    Images = dmShareData.ilActionsSmall
    Left = 248
    Top = 8
    object acSave: TAction
      Caption = 'Save'
      ImageIndex = 1
      OnExecute = acSaveExecute
    end
    object acCancel: TAction
      Caption = 'Cancel'
      ImageIndex = 2
      OnExecute = acCancelExecute
    end
    object acOpenApplication: TAction
      ImageIndex = 0
      OnExecute = acOpenApplicationExecute
    end
    object acSaveProjectName: TAction
      ImageIndex = 0
      OnExecute = acSaveProjectNameExecute
    end
    object acSaveProjectStorage: TAction
      ImageIndex = 0
      Visible = False
      OnExecute = acSaveProjectStorageExecute
    end
    object acDelphiSource: TAction
      ImageIndex = 0
      OnExecute = acDelphiSourceExecute
    end
    object acProjectSource: TAction
      ImageIndex = 0
      OnExecute = acProjectSourceExecute
    end
    object acSelWorkDir: TAction
      ImageIndex = 0
      OnExecute = acSelWorkDirExecute
    end
  end
  object odApplication: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Application (*.exe)'
        FileMask = '*.exe'
      end
      item
        DisplayName = 'Debug info (*.map)'
        FileMask = '*.map'
      end>
    Options = []
    Left = 128
    Top = 176
  end
  object sdProjectName: TFileSaveDialog
    DefaultExtension = '.spider'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Spider project (*.spider)'
        FileMask = '*.spider'
      end>
    Options = [fdoFileMustExist, fdoCreatePrompt]
    Left = 48
    Top = 176
  end
  object odSelectWorkDir: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoPathMustExist]
    Title = 'Select working directory'
    Left = 208
    Top = 176
  end
end
