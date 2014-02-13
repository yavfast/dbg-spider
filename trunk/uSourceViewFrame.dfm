object SourceViewFrame: TSourceViewFrame
  Left = 0
  Top = 0
  Width = 675
  Height = 360
  Align = alClient
  TabOrder = 0
  object synmSourceView: TSynMemo
    Left = 0
    Top = 21
    Width = 675
    Height = 339
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    Gutter.AutoSize = True
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Highlighter = dmShareData.synPas1
    ReadOnly = True
    RightEdge = 0
    FontSmoothing = fsmClearType
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitWidth = 262
    ExplicitHeight = 185
  end
  object eSrcFileName: TEdit
    Left = 0
    Top = 0
    Width = 675
    Height = 21
    Align = alTop
    BevelEdges = []
    BevelInner = bvNone
    BevelOuter = bvNone
    ReadOnly = True
    TabOrder = 1
    Text = 'eSrcFileName'
  end
end
