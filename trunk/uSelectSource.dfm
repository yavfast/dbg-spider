object fmSelectSource: TfmSelectSource
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Select source'
  ClientHeight = 347
  ClientWidth = 537
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object cbTop: TCoolBar
    Left = 0
    Top = 0
    Width = 537
    Height = 30
    AutoSize = True
    Bands = <
      item
        Control = actbTop
        ImageIndex = -1
        MinHeight = 26
        Width = 531
      end>
    object actbTop: TActionToolBar
      Left = 11
      Top = 0
      Width = 522
      Height = 26
      ActionManager = acmgrSelectSource
      Caption = 'actbTop'
      ColorMap.BtnSelectedColor = clBtnFace
      ColorMap.UnusedColor = 13684944
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Spacing = 0
    end
  end
  object cbActions: TCoolBar
    Left = 0
    Top = 317
    Width = 537
    Height = 30
    Align = alBottom
    AutoSize = True
    Bands = <
      item
        Control = actbActions
        ImageIndex = -1
        MinHeight = 26
        Width = 531
      end>
    object actbActions: TActionToolBar
      Left = 11
      Top = 0
      Width = 522
      Height = 26
      ActionManager = acmgrSelectSource
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
      Spacing = 0
    end
  end
  object sgSource: TStringGrid
    Left = 0
    Top = 30
    Width = 537
    Height = 287
    Align = alClient
    BorderStyle = bsNone
    ColCount = 1
    DefaultRowHeight = 18
    FixedCols = 0
    FixedRows = 0
    TabOrder = 2
    ColWidths = (
      500)
  end
  object alSelectSource: TActionList
    Images = dmShareData.ilActionsSmall
    OnUpdate = alSelectSourceUpdate
    Left = 392
    Top = 80
    object acOk: TAction
      Caption = 'Save'
      ImageIndex = 1
      OnExecute = acOkExecute
    end
    object acCancel: TAction
      Caption = 'Cancel'
      ImageIndex = 2
      OnExecute = acCancelExecute
    end
    object acAdd: TAction
      Caption = 'Add'
      ImageIndex = 5
      OnExecute = acAddExecute
    end
    object acRemove: TAction
      Caption = 'Remove'
      ImageIndex = 6
      OnExecute = acRemoveExecute
    end
    object acEdit: TAction
      Caption = 'Edit'
      ImageIndex = 0
      OnExecute = acEditExecute
    end
  end
  object acmgrSelectSource: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = acCancel
            Caption = '&Cancel'
            ImageIndex = 2
          end
          item
            Action = acOk
            Caption = '&Save'
            ImageIndex = 1
          end>
        ActionBar = actbActions
      end
      item
        Items = <
          item
            Action = acAdd
            Caption = '&Add'
            ImageIndex = 5
          end
          item
            Action = acEdit
            Caption = '&Edit'
            ImageIndex = 0
          end
          item
            Action = acRemove
            Caption = '&Remove'
            ImageIndex = 6
          end>
        ActionBar = actbTop
      end>
    DisabledImages = dmShareData.ilActionsSmall
    LinkedActionLists = <
      item
        ActionList = alSelectSource
        Caption = 'alSelectSource'
      end>
    Images = dmShareData.ilActionsSmall
    Left = 392
    Top = 136
    StyleName = 'Ribbon - Silver'
  end
  object odSelectSource: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoPathMustExist]
    Title = 'Select source folder'
    Left = 392
    Top = 192
  end
end
