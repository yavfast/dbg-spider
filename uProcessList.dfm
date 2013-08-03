object frmProcessList: TfrmProcessList
  Left = 0
  Top = 0
  Caption = 'Process list'
  ClientHeight = 374
  ClientWidth = 559
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object sgProcessList: TStringGrid
    Left = 0
    Top = 0
    Width = 559
    Height = 341
    Align = alClient
    ColCount = 1
    DefaultRowHeight = 18
    FixedCols = 0
    TabOrder = 0
    ColWidths = (
      488)
  end
  object pnl1: TPanel
    Left = 0
    Top = 341
    Width = 559
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      559
      33)
    object btnOK: TBitBtn
      Left = 403
      Top = 6
      Width = 73
      Height = 25
      Action = acOk
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      Left = 484
      Top = 6
      Width = 73
      Height = 25
      Action = acCancel
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnRefresh: TBitBtn
      Left = 3
      Top = 6
      Width = 72
      Height = 25
      Action = acRefresh
      Anchors = [akLeft, akBottom]
      Caption = 'Refresh'
      TabOrder = 2
    end
  end
  object AL: TActionList
    Left = 432
    Top = 72
    object acOk: TAction
      Caption = 'OK'
      OnExecute = acOkExecute
    end
    object acCancel: TAction
      Caption = 'Cancel'
      OnExecute = acCancelExecute
    end
    object acRefresh: TAction
      Caption = 'Refresh'
      ShortCut = 116
      OnExecute = acRefreshExecute
    end
  end
end
