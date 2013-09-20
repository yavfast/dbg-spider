object frmProcessList: TfrmProcessList
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Process list'
  ClientHeight = 384
  ClientWidth = 569
  Color = clBtnFace
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
  object sgProcessList: TStringGrid
    Left = 0
    Top = 26
    Width = 569
    Height = 331
    Align = alClient
    BorderStyle = bsNone
    ColCount = 1
    DefaultRowHeight = 18
    FixedCols = 0
    TabOrder = 0
    ExplicitHeight = 332
    ColWidths = (
      519)
  end
  object cbTop: TCoolBar
    Left = 0
    Top = 0
    Width = 569
    Height = 26
    AutoSize = True
    Bands = <
      item
        Control = actbTop
        ImageIndex = -1
        MinHeight = 26
        Width = 567
      end>
    EdgeBorders = []
    EdgeInner = esNone
    EdgeOuter = esNone
    FixedSize = True
    FixedOrder = True
    object actbTop: TActionToolBar
      Left = 2
      Top = 0
      Width = 567
      Height = 26
      ActionManager = actmgrProcessList
      Caption = 'actbTop'
      ColorMap.MenuColor = clMenu
      ColorMap.BtnSelectedColor = clBtnFace
      ColorMap.UnusedColor = 13684944
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = True
      ParentFont = False
      Spacing = 0
    end
  end
  object cbActions: TCoolBar
    Left = 0
    Top = 357
    Width = 569
    Height = 27
    Align = alBottom
    AutoSize = True
    Bands = <
      item
        Control = actbActions
        ImageIndex = -1
        MinHeight = 26
        Width = 567
      end>
    EdgeBorders = [ebTop]
    EdgeOuter = esNone
    FixedSize = True
    FixedOrder = True
    object actbActions: TActionToolBar
      Left = 2
      Top = 0
      Width = 567
      Height = 26
      ActionManager = actmgrProcessList
      AllowHiding = False
      BiDiMode = bdRightToLeft
      Caption = 'actbActions'
      ColorMap.MenuColor = clMenu
      ColorMap.BtnSelectedColor = clBtnFace
      ColorMap.UnusedColor = 13684944
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Orientation = boRightToLeft
      ParentBiDiMode = False
      ParentBackground = True
      ParentFont = False
      Spacing = 0
    end
  end
  object AL: TActionList
    Images = dmShareData.ilActionsSmall
    Left = 432
    Top = 72
    object acOk: TAction
      Caption = 'Attach'
      ImageIndex = 1
      ShortCut = 13
      OnExecute = acOkExecute
    end
    object acCancel: TAction
      Caption = 'Cancel'
      ImageIndex = 2
      ShortCut = 27
      OnExecute = acCancelExecute
    end
    object acRefresh: TAction
      Caption = 'Refresh'
      ImageIndex = 3
      ShortCut = 116
      OnExecute = acRefreshExecute
    end
  end
  object actmgrProcessList: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = acRefresh
            Caption = '&Refresh'
            ImageIndex = 3
            ShortCut = 116
          end>
        ActionBar = actbTop
      end
      item
        Items = <
          item
            Action = acCancel
            Caption = '&Cancel'
            ImageIndex = 2
            ShortCut = 27
          end
          item
            Action = acOk
            Caption = '&Attach'
            Default = True
            ImageIndex = 1
            ShortCut = 13
          end>
        ActionBar = actbActions
      end>
    DisabledImages = dmShareData.ilActionsSmall
    LinkedActionLists = <
      item
        ActionList = AL
        Caption = 'AL'
      end>
    Images = dmShareData.ilActionsSmall
    Left = 416
    Top = 176
    StyleName = 'Ribbon - Silver'
  end
end
