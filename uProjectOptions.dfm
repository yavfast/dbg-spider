object fmProjectOptions: TfmProjectOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Project options'
  ClientHeight = 225
  ClientWidth = 454
  Color = clWindow
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
  object pcBackground: TPageControl
    Left = 0
    Top = 0
    Width = 454
    Height = 225
    ActivePage = ts1
    Align = alClient
    TabOrder = 0
    object ts1: TTabSheet
      Caption = 'ts1'
      TabVisible = False
      object p1: TPanel
        Left = 0
        Top = 0
        Width = 446
        Height = 215
        Align = alClient
        BevelEdges = []
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 0
        object pcProjectOpt: TPageControl
          Left = 0
          Top = 0
          Width = 446
          Height = 183
          ActivePage = tsSources
          Align = alTop
          TabOrder = 0
          object tsProject: TTabSheet
            Caption = 'Project settings'
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
            ImageIndex = 1
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
        end
        object cbActions: TCoolBar
          Left = 0
          Top = 185
          Width = 446
          Height = 30
          Align = alBottom
          AutoSize = True
          Bands = <
            item
              Control = actbActions
              HorizontalOnly = True
              ImageIndex = -1
              MinHeight = 26
              Width = 440
            end>
          object actbActions: TActionToolBar
            Left = 11
            Top = 0
            Width = 431
            Height = 26
            ActionManager = actmgrProjectOpt
            AllowHiding = False
            BiDiMode = bdRightToLeft
            Caption = 'actbActions'
            ColorMap.BtnSelectedColor = clBtnFace
            ColorMap.UnusedColor = 13684944
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            Orientation = boRightToLeft
            ParentBiDiMode = False
            ParentFont = False
            Spacing = 4
          end
        end
      end
    end
  end
  object actmgrProjectOpt: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = acCancel
            Caption = '&Cancel'
            ImageIndex = 2
          end
          item
            Action = acSave
            Caption = '&Save'
            Default = True
            ImageIndex = 1
          end>
      end
      item
        Items = <
          item
            Action = acCancel
            Caption = '&Cancel'
            ImageIndex = 2
          end
          item
            Action = acSave
            Caption = '&Save'
            ImageIndex = 1
          end>
        ActionBar = actbActions
      end>
    LinkedActionLists = <
      item
        ActionList = alProjectOpt
        Caption = 'alProjectOpt'
      end>
    Images = dmShareData.ilActionsSmall
    Left = 192
    Top = 8
    StyleName = 'Ribbon - Silver'
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
  end
  object odApplication: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Application (*.exe)'
        FileMask = '*.exe'
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
end
