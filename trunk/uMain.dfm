object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Spider'
  ClientHeight = 556
  ClientWidth = 965
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
  object pActions: TPanel
    Left = 0
    Top = 0
    Width = 965
    Height = 73
    Align = alTop
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 376
      Top = 8
      Width = 75
      Height = 25
      Action = acAppOpen
      Caption = 'Open...'
      TabOrder = 0
    end
    object eAppPath: TMaskEdit
      Left = 8
      Top = 8
      Width = 361
      Height = 21
      TabOrder = 1
      Text = 'C:\Program Files\4Sync\4sync.exe'
    end
    object BitBtn2: TBitBtn
      Left = 8
      Top = 40
      Width = 75
      Height = 25
      Action = acRun
      Caption = 'Run'
      TabOrder = 2
    end
    object BitBtn3: TBitBtn
      Left = 97
      Top = 40
      Width = 75
      Height = 25
      Action = acStop
      Caption = 'Stop'
      TabOrder = 3
    end
    object Button1: TButton
      Left = 208
      Top = 40
      Width = 75
      Height = 25
      Action = acDebugInfo
      TabOrder = 4
    end
    object btnAttach: TBitBtn
      Left = 464
      Top = 8
      Width = 75
      Height = 25
      Action = acAttachProcess
      Caption = 'Attach...'
      TabOrder = 5
    end
    object cbCPUTimeLine: TCheckBox
      Left = 312
      Top = 48
      Width = 97
      Height = 17
      Caption = 'CPU TimeLine'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = cbCPUTimeLineClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 73
    Width = 965
    Height = 464
    ActivePage = tsThreads1
    Align = alClient
    TabOrder = 1
    object tsLog: TTabSheet
      Caption = 'Log'
      object mLog: TMemo
        Left = 0
        Top = 0
        Width = 957
        Height = 436
        Align = alClient
        Lines.Strings = (
          'mLog')
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object tsDebugInfo: TTabSheet
      Caption = 'DebugInfo'
      ImageIndex = 1
      object Splitter1: TSplitter
        Left = 279
        Top = 0
        Height = 436
      end
      object pUnits: TPanel
        Left = 0
        Top = 0
        Width = 279
        Height = 436
        Align = alLeft
        TabOrder = 0
        object lbUnits: TListBox
          Left = 1
          Top = 1
          Width = 277
          Height = 434
          Align = alClient
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbUnitsClick
        end
      end
      object pUnitInfo: TPanel
        Left = 282
        Top = 0
        Width = 675
        Height = 436
        Align = alClient
        TabOrder = 1
        object PageControl2: TPageControl
          Left = 1
          Top = 1
          Width = 673
          Height = 434
          ActivePage = tsFunctions
          Align = alClient
          TabOrder = 0
          object tsConsts: TTabSheet
            Caption = 'Consts'
            object mConsts: TMemo
              Left = 0
              Top = 0
              Width = 665
              Height = 406
              Align = alClient
              Lines.Strings = (
                'mConsts')
              ReadOnly = True
              ScrollBars = ssBoth
              TabOrder = 0
              WantReturns = False
              WordWrap = False
            end
          end
          object tsTypes: TTabSheet
            Caption = 'Types'
            ImageIndex = 1
            object mTypes: TMemo
              Left = 0
              Top = 0
              Width = 665
              Height = 406
              Align = alClient
              Lines.Strings = (
                'mConsts')
              ReadOnly = True
              ScrollBars = ssBoth
              TabOrder = 0
              WantReturns = False
              WordWrap = False
            end
          end
          object tsVars: TTabSheet
            Caption = 'Vars'
            ImageIndex = 3
            object mVars: TMemo
              Left = 0
              Top = 0
              Width = 665
              Height = 406
              Align = alClient
              Lines.Strings = (
                'mConsts')
              ReadOnly = True
              ScrollBars = ssBoth
              TabOrder = 0
              WordWrap = False
            end
          end
          object tsFunctions: TTabSheet
            Caption = 'Functions'
            ImageIndex = 2
            object mFunctions: TMemo
              Left = 0
              Top = 0
              Width = 665
              Height = 406
              Align = alClient
              Lines.Strings = (
                'mConsts')
              ReadOnly = True
              ScrollBars = ssBoth
              TabOrder = 0
            end
          end
        end
      end
    end
    object tsThreads1: TTabSheet
      Caption = 'Threads'
      ImageIndex = 3
      object vstThreads: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 315
        Height = 436
        Align = alLeft
        BorderStyle = bsNone
        DrawSelectionMode = smBlendedRectangle
        Header.AutoSizeIndex = -1
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoColumnResize, hoDrag, hoVisible]
        Header.Style = hsPlates
        ScrollBarOptions.AlwaysVisible = True
        ScrollBarOptions.ScrollBars = ssHorizontal
        TabOrder = 0
        TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseBlendedSelection]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
        OnChange = vdtTimeLineChange
        OnCollapsed = vstThreadsCollapsed
        OnDrawText = vstThreadsDrawText
        OnExpanded = vstThreadsExpanded
        OnGetText = vstThreadsGetText
        OnGetNodeDataSize = vstThreadsGetNodeDataSize
        OnScroll = vstThreadsScroll
        Columns = <
          item
            CaptionAlignment = taCenter
            Options = [coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coUseCaptionAlignment]
            Position = 0
            Width = 200
            WideText = 'Thread name'
          end
          item
            CaptionAlignment = taCenter
            Options = [coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coUseCaptionAlignment]
            Position = 1
            Width = 40
            WideText = 'ID'
          end
          item
            CaptionAlignment = taCenter
            Options = [coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coUseCaptionAlignment]
            Position = 2
            Width = 75
            WideText = 'CPU time'
          end>
      end
      object vdtTimeLine: TVirtualDrawTree
        Left = 316
        Top = 0
        Width = 641
        Height = 436
        Align = alClient
        BorderStyle = bsNone
        DrawSelectionMode = smBlendedRectangle
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoOwnerDraw, hoVisible, hoFullRepaintOnResize, hoDisableAnimatedResize]
        Header.Style = hsPlates
        ScrollBarOptions.AlwaysVisible = True
        TabOrder = 1
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toShowBackground, toThemeAware, toUseBlendedSelection]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
        OnAdvancedHeaderDraw = vdtTimeLineAdvancedHeaderDraw
        OnChange = vdtTimeLineChange
        OnCollapsed = vstThreadsCollapsed
        OnDrawNode = vdtTimeLineDrawNode
        OnExpanded = vstThreadsExpanded
        OnGetNodeDataSize = vstThreadsGetNodeDataSize
        OnHeaderDrawQueryElements = vdtTimeLineHeaderDrawQueryElements
        OnPaintBackground = vdtTimeLinePaintBackground
        OnScroll = vdtTimeLineScroll
        Columns = <
          item
            BiDiMode = bdLeftToRight
            Options = [coEnabled, coParentColor, coResizable, coVisible, coFixed, coAllowFocus]
            Position = 0
            Style = vsOwnerDraw
            Width = 10000
          end>
      end
      object sp1: TJvxSplitter
        Left = 315
        Top = 0
        Width = 1
        Height = 436
        ControlFirst = vstThreads
        ControlSecond = vdtTimeLine
        Align = alLeft
        BevelOuter = bvNone
      end
    end
  end
  object sbInfo: TJvStatusBar
    Left = 0
    Top = 537
    Width = 965
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Text = 'NO_DBG_INFO'
        Width = 80
      end
      item
        Alignment = taCenter
        Text = 'DBG_STATUS'
        Width = 75
      end
      item
        Alignment = taRightJustify
        Text = 'PERF_INFO'
        Width = 65
      end
      item
        Width = 50
      end>
  end
  object AL: TActionList
    Left = 592
    Top = 8
    object acAppOpen: TAction
      Caption = 'Open...'
      OnExecute = acAppOpenExecute
    end
    object acRun: TAction
      Caption = 'Run'
      OnExecute = acRunExecute
    end
    object acStop: TAction
      Caption = 'Stop'
      OnExecute = acStopExecute
    end
    object acDebugInfo: TAction
      Caption = 'DebugInfo'
      OnExecute = acDebugInfoExecute
    end
    object acAttachProcess: TAction
      Caption = 'Attach...'
      OnExecute = acAttachProcessExecute
    end
  end
  object OD: TFileOpenDialog
    DefaultExtension = '*.exe'
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 552
    Top = 8
  end
  object tmrThreadsUpdate: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmrThreadsUpdateTimer
    Left = 664
    Top = 8
  end
  object sm1: TJvNavPaneStyleManager
    Theme = nptStandard
    Left = 744
    Top = 8
  end
end
