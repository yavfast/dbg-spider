object frmFeedback: TfrmFeedback
  Left = 0
  Top = 0
  ActiveControl = mMessage
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Add comment'
  ClientHeight = 334
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
  PixelsPerInch = 96
  TextHeight = 13
  object lbFeedbackType: TLabel
    Left = 8
    Top = 8
    Width = 70
    Height = 13
    Caption = 'Comment type'
  end
  object lbMessage: TLabel
    Left = 8
    Top = 64
    Width = 42
    Height = 13
    Caption = 'Message'
  end
  object lbEmail: TLabel
    Left = 232
    Top = 8
    Width = 77
    Height = 13
    Caption = 'E-mail (optional)'
  end
  object cbbType: TComboBoxEx
    Left = 8
    Top = 24
    Width = 193
    Height = 22
    ItemsEx = <
      item
        Caption = 'Have question'
      end
      item
        Caption = 'Error found'
      end
      item
        Caption = 'New idea'
      end
      item
        Caption = 'Other'
      end>
    Style = csExDropDownList
    TabOrder = 0
  end
  object mMessage: TMemo
    Left = 8
    Top = 80
    Width = 513
    Height = 201
    Lines.Strings = (
      'mMessage')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object btnSend: TBitBtn
    Left = 440
    Top = 295
    Width = 81
    Height = 28
    Action = acSend
    Caption = 'Send'
    TabOrder = 3
  end
  object eEMail: TEdit
    Left = 232
    Top = 24
    Width = 289
    Height = 21
    TabOrder = 1
  end
  object AL: TActionList
    Images = dmShareData.ilActionsSmall
    Left = 320
    Top = 136
    object acSend: TAction
      Caption = 'Send'
      ImageIndex = 1
      OnExecute = acSendExecute
    end
  end
end
