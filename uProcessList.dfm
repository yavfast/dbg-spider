object frmProcessList: TfrmProcessList
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Process list'
  ClientHeight = 413
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
    Height = 342
    Align = alClient
    BorderStyle = bsNone
    ColCount = 1
    DefaultRowHeight = 18
    FixedCols = 0
    TabOrder = 0
    ExplicitHeight = 331
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
    FixedOrder = True
    object actbTop: TActionToolBar
      Left = 2
      Top = 0
      Width = 567
      Height = 26
      ActionManager = acmgr1
      Caption = 'actbTop'
      ColorMap.MenuColor = clMenu
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
  object pActions: TPanel
    Left = 0
    Top = 368
    Width = 569
    Height = 45
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'pActions'
    ShowCaption = False
    TabOrder = 2
    DesignSize = (
      569
      45)
    object btnAttach: TBitBtn
      Left = 387
      Top = 11
      Width = 75
      Height = 25
      Action = acOk
      Anchors = [akRight, akBottom]
      Caption = 'Attach'
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 475
      Top = 11
      Width = 75
      Height = 25
      Action = acCancel
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      TabOrder = 1
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
  object acmgr1: TActionManager
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
      end>
    LargeImages = dmShareData.imlMain
    LinkedActionLists = <
      item
        ActionList = AL
        Caption = 'AL'
      end>
    Images = dmShareData.imlMainSmall
    Left = 432
    Top = 152
    StyleName = 'Platform Default'
  end
end
