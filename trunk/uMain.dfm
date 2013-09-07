object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Spider'
  ClientHeight = 737
  ClientWidth = 1218
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pcMain: TPageControl
    Left = 0
    Top = 167
    Width = 1218
    Height = 545
    ActivePage = tsLog
    Align = alClient
    TabOrder = 0
    object tsLog: TTabSheet
      Caption = 'Log'
      object vstLog: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 1210
        Height = 517
        Align = alClient
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoColumnResize, hoShowSortGlyphs, hoVisible]
        Indent = 0
        ScrollBarOptions.AlwaysVisible = True
        TabOrder = 0
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
        OnColumnResize = vstLogColumnResize
        OnDrawText = vstLogDrawText
        OnGetText = vstLogGetText
        OnGetNodeDataSize = vstThreadsGetNodeDataSize
        OnResize = vstLogResize
        Columns = <
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
            Position = 0
            Width = 150
            WideText = 'Event time'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
            Position = 1
            Width = 800
            WideText = 'Message'
          end>
      end
    end
    object tsDebugInfo2: TTabSheet
      Caption = 'DebugInfo2'
      Highlighted = True
      ImageIndex = 5
      object vstDbgInfoUnits: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 347
        Height = 517
        Align = alLeft
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoColumnResize, hoShowSortGlyphs, hoVisible]
        ScrollBarOptions.AlwaysVisible = True
        TabOrder = 0
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
        OnColumnResize = vstThreadsColumnResize
        OnFocusChanged = vstDbgInfoUnitsFocusChanged
        OnGetText = vstDbgInfoUnitsGetText
        OnGetNodeDataSize = vstThreadsGetNodeDataSize
        Columns = <
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
            Position = 0
            Width = 210
            WideText = 'Unit name'
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
            Position = 1
            Width = 60
            WideText = 'Code size'
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
            Position = 2
            Width = 60
            WideText = 'Data size'
          end>
      end
      object pDbgInfoDetail: TPanel
        Left = 347
        Top = 0
        Width = 863
        Height = 517
        Align = alClient
        BevelEdges = []
        BevelOuter = bvNone
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 1
        object pcDbgInfoDetail: TPageControl
          Left = 0
          Top = 0
          Width = 863
          Height = 517
          ActivePage = tsDbgUnitFunctions
          Align = alClient
          TabOrder = 0
          object tsDbgUnitConsts: TTabSheet
            Caption = 'Consts'
            object vstDbgInfoConsts: TVirtualStringTree
              Left = 0
              Top = 0
              Width = 855
              Height = 489
              Align = alClient
              BevelEdges = []
              BevelInner = bvNone
              BevelOuter = bvNone
              BorderStyle = bsNone
              Header.AutoSizeIndex = 0
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clWindowText
              Header.Font.Height = -11
              Header.Font.Name = 'Tahoma'
              Header.Font.Style = []
              Header.Options = [hoColumnResize, hoShowSortGlyphs, hoVisible]
              ScrollBarOptions.AlwaysVisible = True
              TabOrder = 0
              TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
              TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
              TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
              OnGetText = vstDbgInfoConstsGetText
              OnGetNodeDataSize = vstThreadsGetNodeDataSize
              Columns = <
                item
                  CaptionAlignment = taCenter
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
                  Position = 0
                  Width = 250
                  WideText = 'Const name'
                end
                item
                  Alignment = taRightJustify
                  CaptionAlignment = taCenter
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                  Position = 1
                  Width = 200
                  WideText = 'Const value'
                end
                item
                  CaptionAlignment = taCenter
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                  Position = 2
                  Width = 150
                  WideText = 'Const type'
                end>
            end
          end
          object tsDbgUnitTypes: TTabSheet
            Caption = 'Types'
            ImageIndex = 1
            object vstDbgInfoTypes: TVirtualStringTree
              Left = 0
              Top = 0
              Width = 855
              Height = 489
              Align = alClient
              BevelEdges = []
              BevelInner = bvNone
              BevelOuter = bvNone
              BorderStyle = bsNone
              Header.AutoSizeIndex = 0
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clWindowText
              Header.Font.Height = -11
              Header.Font.Name = 'Tahoma'
              Header.Font.Style = []
              Header.Options = [hoColumnResize, hoShowSortGlyphs, hoVisible]
              ScrollBarOptions.AlwaysVisible = True
              TabOrder = 0
              TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
              TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
              TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
              OnGetText = vstDbgInfoTypesGetText
              OnGetNodeDataSize = vstThreadsGetNodeDataSize
              Columns = <
                item
                  CaptionAlignment = taCenter
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
                  Position = 0
                  Width = 210
                  WideText = 'Type name'
                end
                item
                  CaptionAlignment = taCenter
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                  Position = 1
                  Width = 400
                  WideText = 'Base type'
                end>
            end
          end
          object tsDbgUnitVars: TTabSheet
            Caption = 'Vars'
            ImageIndex = 2
            object vstDbgInfoVars: TVirtualStringTree
              Left = 0
              Top = 0
              Width = 855
              Height = 489
              Align = alClient
              BevelEdges = []
              BevelInner = bvNone
              BevelOuter = bvNone
              BorderStyle = bsNone
              Header.AutoSizeIndex = 0
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clWindowText
              Header.Font.Height = -11
              Header.Font.Name = 'Tahoma'
              Header.Font.Style = []
              Header.Options = [hoColumnResize, hoShowSortGlyphs, hoVisible]
              ScrollBarOptions.AlwaysVisible = True
              TabOrder = 0
              TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
              TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
              TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
              OnGetText = vstDbgInfoVarsGetText
              OnGetNodeDataSize = vstThreadsGetNodeDataSize
              Columns = <
                item
                  CaptionAlignment = taCenter
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
                  Position = 0
                  Width = 250
                  WideText = 'Var name'
                end
                item
                  CaptionAlignment = taCenter
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                  Position = 1
                  Width = 200
                  WideText = 'Var type'
                end
                item
                  Alignment = taRightJustify
                  CaptionAlignment = taCenter
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                  Position = 2
                  Width = 70
                  WideText = 'Var address'
                end>
            end
          end
          object tsDbgUnitFunctions: TTabSheet
            Caption = 'Functions'
            ImageIndex = 3
            object vstDbgInfoFunctions: TVirtualStringTree
              Left = 0
              Top = 0
              Width = 547
              Height = 489
              Align = alLeft
              BevelEdges = []
              BevelInner = bvNone
              BevelOuter = bvNone
              BorderStyle = bsNone
              Header.AutoSizeIndex = 0
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clWindowText
              Header.Font.Height = -11
              Header.Font.Name = 'Tahoma'
              Header.Font.Style = []
              Header.Options = [hoColumnResize, hoShowSortGlyphs, hoVisible]
              ScrollBarOptions.AlwaysVisible = True
              TabOrder = 0
              TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
              TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
              TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
              OnColumnResize = vstThreadsColumnResize
              OnFocusChanged = vstDbgInfoFunctionsFocusChanged
              OnGetText = vstDbgInfoFunctionsGetText
              OnGetNodeDataSize = vstThreadsGetNodeDataSize
              Columns = <
                item
                  CaptionAlignment = taCenter
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
                  Position = 0
                  Width = 400
                  WideText = 'Function name'
                end
                item
                  Alignment = taRightJustify
                  CaptionAlignment = taCenter
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                  Position = 1
                  Width = 70
                  WideText = 'Address'
                end
                item
                  Alignment = taRightJustify
                  CaptionAlignment = taCenter
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                  Position = 2
                  Width = 60
                  WideText = 'Code size'
                end>
            end
            object pDbgInfoFuncAdv: TPanel
              Left = 547
              Top = 0
              Width = 308
              Height = 489
              Align = alClient
              BevelEdges = []
              BevelOuter = bvNone
              Caption = 'pDbgInfoFuncAdv'
              ShowCaption = False
              TabOrder = 1
              object splDbgInfoFuncAdv: TSplitter
                Left = 0
                Top = 193
                Width = 308
                Height = 3
                Cursor = crVSplit
                Align = alTop
              end
              object vstDbgInfoFuncVars: TVirtualStringTree
                Left = 0
                Top = 0
                Width = 308
                Height = 193
                Align = alTop
                BevelEdges = []
                BevelInner = bvNone
                BevelOuter = bvNone
                BorderStyle = bsNone
                Header.AutoSizeIndex = 0
                Header.Font.Charset = DEFAULT_CHARSET
                Header.Font.Color = clWindowText
                Header.Font.Height = -11
                Header.Font.Name = 'Tahoma'
                Header.Font.Style = []
                Header.Options = [hoColumnResize, hoShowSortGlyphs, hoVisible]
                ScrollBarOptions.AlwaysVisible = True
                TabOrder = 0
                TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
                TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
                TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
                OnGetText = vstDbgInfoFuncVarsGetText
                OnGetNodeDataSize = vstThreadsGetNodeDataSize
                Columns = <
                  item
                    CaptionAlignment = taCenter
                    Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
                    Position = 0
                    Width = 200
                    WideText = 'Param name'
                  end
                  item
                    CaptionAlignment = taCenter
                    Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                    Position = 1
                    Width = 150
                    WideText = 'Param type'
                  end
                  item
                    Position = 2
                    Width = 70
                    WideText = 'Param kind'
                  end>
              end
              object synmDbgInfoFuncAdv: TSynMemo
                Left = 0
                Top = 196
                Width = 308
                Height = 293
                Align = alClient
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -13
                Font.Name = 'Courier New'
                Font.Style = []
                TabOrder = 1
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
                SearchEngine = dmShareData.synRegexSearch1
                FontSmoothing = fsmNone
              end
            end
          end
          object tsDbgUnitSource: TTabSheet
            Caption = 'Source'
            ImageIndex = 4
            object synmDbgInfoUnitSource: TSynMemo
              Left = 0
              Top = 0
              Width = 855
              Height = 489
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
              SearchEngine = dmShareData.synRegexSearch1
              FontSmoothing = fsmNone
            end
          end
        end
      end
    end
    object tsThreads1: TTabSheet
      Caption = 'Threads timeline'
      ImageIndex = 3
      object vstThreads: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 447
        Height = 517
        Align = alLeft
        BorderStyle = bsNone
        Colors.FocusedSelectionColor = clBtnFace
        Colors.FocusedSelectionBorderColor = clBtnFace
        Colors.SelectionRectangleBlendColor = clBtnFace
        Colors.SelectionRectangleBorderColor = clBtnFace
        Colors.SelectionTextColor = clWindowText
        DrawSelectionMode = smBlendedRectangle
        Header.AutoSizeIndex = -1
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoColumnResize, hoVisible]
        Header.Style = hsPlates
        ScrollBarOptions.AlwaysVisible = True
        ScrollBarOptions.ScrollBars = ssHorizontal
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoTristateTracking, toAutoDeleteMovedNodes]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
        TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus, toFullRowSelect]
        OnChange = vdtTimeLineChange
        OnCollapsed = vstThreadsCollapsed
        OnColumnResize = vstThreadsColumnResize
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
            Width = 300
            WideText = 'Thread name'
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coUseCaptionAlignment]
            Position = 1
            Width = 75
            WideText = 'ID'
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coUseCaptionAlignment]
            Position = 2
            Width = 70
            WideText = 'CPU time'
          end>
      end
      object vdtTimeLine: TVirtualDrawTree
        Left = 447
        Top = 0
        Width = 763
        Height = 517
        Align = alClient
        BorderStyle = bsNone
        Colors.FocusedSelectionColor = clBtnFace
        Colors.FocusedSelectionBorderColor = clBtnFace
        Colors.SelectionRectangleBlendColor = clBtnFace
        Colors.SelectionRectangleBorderColor = clBtnFace
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
        TreeOptions.AutoOptions = [toAutoTristateTracking, toAutoDeleteMovedNodes]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toShowBackground, toThemeAware]
        TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus, toFullRowSelect]
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
    end
    object tsMemInfo: TTabSheet
      Caption = 'Memory Info'
      ImageIndex = 3
      object vstMemInfoThreads: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 470
        Height = 517
        Align = alLeft
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoColumnResize, hoShowSortGlyphs, hoVisible]
        Header.Style = hsFlatButtons
        ScrollBarOptions.AlwaysVisible = True
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
        OnColumnResize = vstThreadsColumnResize
        OnDrawText = vstThreadsDrawText
        OnFocusChanged = vstMemInfoThreadsFocusChanged
        OnGetText = vstMemInfoThreadsGetText
        OnGetNodeDataSize = vstThreadsGetNodeDataSize
        Columns = <
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
            Position = 0
            Width = 250
            WideText = 'Thread name'
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
            Position = 1
            Width = 75
            WideText = 'ID'
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
            Position = 2
            WideText = 'Count'
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
            Position = 3
            Width = 75
            WideText = 'Size'
          end>
      end
      object pnl1: TPanel
        Left = 470
        Top = 0
        Width = 740
        Height = 517
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object vstMemList: TVirtualStringTree
          Left = 0
          Top = 0
          Width = 305
          Height = 517
          Align = alLeft
          Header.AutoSizeIndex = 0
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'Tahoma'
          Header.Font.Style = []
          Header.Options = [hoColumnResize, hoVisible]
          Header.SortDirection = sdDescending
          Header.Style = hsFlatButtons
          ScrollBarOptions.AlwaysVisible = True
          TabOrder = 0
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
          TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
          TreeOptions.SelectionOptions = [toFullRowSelect]
          OnColumnResize = vstThreadsColumnResize
          OnFocusChanged = vstMemListFocusChanged
          OnGetText = vstMemListGetText
          OnGetNodeDataSize = vstThreadsGetNodeDataSize
          Columns = <
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
              Position = 0
              Width = 150
              WideText = 'Object type'
            end
            item
              Alignment = taRightJustify
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
              Position = 1
              Width = 70
              WideText = 'Pointer'
            end
            item
              Alignment = taRightJustify
              CaptionAlignment = taCenter
              DefaultSortDirection = sdDescending
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
              Position = 2
              Width = 65
              WideText = 'Size'
            end>
        end
        object pMemoryInfoAdv: TPanel
          Left = 305
          Top = 0
          Width = 435
          Height = 517
          Align = alClient
          BevelOuter = bvNone
          Caption = 'pMemoryInfoAdv'
          ShowCaption = False
          TabOrder = 1
          object splMemInfoAdv: TSplitter
            Left = 0
            Top = 225
            Width = 435
            Height = 3
            Cursor = crVSplit
            Align = alTop
          end
          object vstMemStack: TVirtualStringTree
            Left = 0
            Top = 0
            Width = 435
            Height = 225
            Align = alTop
            Header.AutoSizeIndex = 0
            Header.Font.Charset = DEFAULT_CHARSET
            Header.Font.Color = clWindowText
            Header.Font.Height = -11
            Header.Font.Name = 'Tahoma'
            Header.Font.Style = []
            Header.Options = [hoColumnResize, hoShowSortGlyphs, hoVisible]
            Header.Style = hsFlatButtons
            ScrollBarOptions.AlwaysVisible = True
            TabOrder = 0
            TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
            TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
            TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
            OnFocusChanged = vstMemStackFocusChanged
            OnGetText = vstMemStackGetText
            OnGetNodeDataSize = vstThreadsGetNodeDataSize
            Columns = <
              item
                Alignment = taRightJustify
                CaptionAlignment = taCenter
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coSmartResize, coAllowFocus, coUseCaptionAlignment]
                Position = 0
                Width = 70
                WideText = 'Address'
              end
              item
                CaptionAlignment = taCenter
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                Position = 1
                Width = 120
                WideText = 'Unit'
              end
              item
                Alignment = taRightJustify
                CaptionAlignment = taCenter
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                Position = 2
                WideText = 'Line'
              end
              item
                Position = 3
                Width = 500
                WideText = 'Call stack function'
              end>
          end
          object synmMemInfoSource: TSynMemo
            Left = 0
            Top = 228
            Width = 435
            Height = 289
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 1
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
            SearchEngine = dmShareData.synRegexSearch1
            FontSmoothing = fsmNone
          end
        end
      end
    end
    object tsExceptions: TTabSheet
      Caption = 'Exceptions'
      ImageIndex = 4
      object vstExceptionThreads: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 396
        Height = 517
        Align = alLeft
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoColumnResize, hoShowSortGlyphs, hoVisible]
        Header.Style = hsFlatButtons
        ScrollBarOptions.AlwaysVisible = True
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
        OnColumnResize = vstThreadsColumnResize
        OnDrawText = vstThreadsDrawText
        OnFocusChanged = vstExceptionThreadsFocusChanged
        OnGetText = vstExceptionThreadsGetText
        OnGetNodeDataSize = vstThreadsGetNodeDataSize
        Columns = <
          item
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
            Position = 0
            Width = 250
            WideText = 'Thread name'
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
            Position = 1
            Width = 75
            WideText = 'ID'
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
            Position = 2
            WideText = 'Count'
          end>
      end
      object pnl2: TPanel
        Left = 396
        Top = 0
        Width = 814
        Height = 517
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object vstExceptionList: TVirtualStringTree
          Left = 0
          Top = 0
          Width = 511
          Height = 517
          Align = alLeft
          Header.AutoSizeIndex = 0
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'Tahoma'
          Header.Font.Style = []
          Header.Options = [hoColumnResize, hoVisible]
          Header.SortDirection = sdDescending
          Header.Style = hsFlatButtons
          ScrollBarOptions.AlwaysVisible = True
          TabOrder = 0
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
          TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
          TreeOptions.SelectionOptions = [toFullRowSelect]
          OnColumnResize = vstThreadsColumnResize
          OnFocusChanged = vstExceptionListFocusChanged
          OnGetText = vstExceptionListGetText
          OnGetNodeDataSize = vstThreadsGetNodeDataSize
          Columns = <
            item
              Alignment = taRightJustify
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
              Position = 0
              Width = 70
              WideText = 'Pointer'
            end
            item
              CaptionAlignment = taCenter
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
              Position = 1
              Width = 120
              WideText = 'Exception type'
            end
            item
              CaptionAlignment = taCenter
              DefaultSortDirection = sdDescending
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coSmartResize, coAllowFocus, coUseCaptionAlignment]
              Position = 2
              Width = 300
              WideText = 'Message'
            end>
        end
        object pExceptInfoAdv: TPanel
          Left = 511
          Top = 0
          Width = 303
          Height = 517
          Align = alClient
          BevelEdges = []
          BevelOuter = bvNone
          Caption = 'pExceptInfoAdv'
          ShowCaption = False
          TabOrder = 1
          object splExceptInfoAdv: TSplitter
            Left = 0
            Top = 217
            Width = 303
            Height = 3
            Cursor = crVSplit
            Align = alTop
          end
          object vstExceptionCallStack: TVirtualStringTree
            Left = 0
            Top = 0
            Width = 303
            Height = 217
            Align = alTop
            Header.AutoSizeIndex = 0
            Header.Font.Charset = DEFAULT_CHARSET
            Header.Font.Color = clWindowText
            Header.Font.Height = -11
            Header.Font.Name = 'Tahoma'
            Header.Font.Style = []
            Header.Options = [hoColumnResize, hoShowSortGlyphs, hoVisible]
            Header.Style = hsFlatButtons
            ScrollBarOptions.AlwaysVisible = True
            TabOrder = 0
            TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
            TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
            TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
            OnFocusChanged = vstExceptionCallStackFocusChanged
            OnGetText = vstExceptionCallStackGetText
            OnGetNodeDataSize = vstThreadsGetNodeDataSize
            Columns = <
              item
                Alignment = taRightJustify
                CaptionAlignment = taCenter
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coSmartResize, coAllowFocus, coUseCaptionAlignment]
                Position = 0
                Width = 70
                WideText = 'Address'
              end
              item
                CaptionAlignment = taCenter
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                Position = 1
                Width = 120
                WideText = 'Unit'
              end
              item
                Alignment = taRightJustify
                CaptionAlignment = taCenter
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                Position = 2
                WideText = 'Line'
              end
              item
                Position = 3
                Width = 500
                WideText = 'Call stack function'
              end>
          end
          object synmExceptInfoSource: TSynMemo
            Left = 0
            Top = 220
            Width = 303
            Height = 297
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 1
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
            FontSmoothing = fsmNone
          end
        end
      end
    end
  end
  object rbnMain: TRibbon
    Left = 0
    Top = 0
    Width = 1218
    Height = 143
    ActionManager = amMain
    ApplicationMenu.Caption = 'Recent projects'
    ApplicationMenu.CommandType = ctCommands
    ApplicationMenu.Icon.Data = {
      0000010001002020000001002000A81000001600000028000000200000004000
      0000010020000000000080100000000000000000000000000000000000007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E707E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7EAF7E7E7E307E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E107E7E7EBF7E7E7E107E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7EBF7E7E7E407E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E9F7E7E7EDF7E7E7E107E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E107E7E7ECF7E7E7E8F7E7E7E107E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E507E7E
      7EFF7E7E7E607E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E007E7E7E107E7E7EAF7E7E7EEF7E7E7E9F7E7E
      7E507E7E7E207E7E7E007E7E7E007E7E7E407E7E7E707E7E7EDF7E7E7EFF7E7E
      7EBF7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E407E7E7EBF7E7E
      7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E
      7E307E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E207E7E7E707E7E7E8F7E7E7EBF7E7E7EBF7E7E7EAF7E7E7EFF7E7E7EBF7E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E807E7E7EEF7E7E7E8F7E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E207E7E7EEF7E7E7E807E7E7E707E7E
      7E507E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E507E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E9F7E7E7EDF7E7E7E007E7E7E307E7E
      7EAF7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7EDF7E7E7E507E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E407E7E7EFF7E7E7E507E7E7E007E7E7E007E7E
      7EDF7E7E7E407E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E507E7E7ECF7E7E7EBF7E7E7E507E7E7E007E7E7E007E7E
      7E007E7E7E307E7E7E9F7E7E7EFF7E7E7EBF7E7E7E007E7E7E007E7E7E007E7E
      7E607E7E7EDF7E7E7E107E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E107E7E7E8F7E7E7EFF7E7E7EFF7E7E7EFF7E7E
      7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7E207E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7ECF7E7E7ECF7E7E7E107E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E007E7E7E007E7E7E207E7E7E707E7E7EBF7E7E
      7EBF7E7E7ECF7E7E7EFF7E7E7E9F7E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E207E7E7EEF7E7E7EEF7E7E7E507E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E407E7E7ECF7E7E7ECF7E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E807E7E7EEF7E7E7E9F7E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E307E7E7EEF7E7E7EFF7E7E7ECF7E7E7E707E7E7E307E7E
      7E007E7E7E207E7E7EAF7E7E7EFF7E7E7EBF7E7E7E207E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E207E7E7EFF7E7E7E707E7E7E9F7E7E7E407E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E107E7E7EAF7E7E7EFF7E7E7EFF7E7E7EFF7E7E
      7EFF7E7E7EFF7E7E7EDF7E7E7E407E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7EAF7E7E7EDF7E7E7E007E7E7E507E7E7EDF7E7E7E107E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E407E7E7EDF7E7E7EFF7E7E
      7EFF7E7E7E707E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E407E7E
      7EFF7E7E7E407E7E7E007E7E7E007E7E7ECF7E7E7ECF7E7E7E207E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E407E7E7EBF7E7E7EFF7E7E7E8F7E7E
      7EBF7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7EDF7E7E
      7EAF7E7E7E007E7E7E007E7E7E007E7E7E307E7E7EEF7E7E7EEF7E7E7E8F7E7E
      7E307E7E7E007E7E7E207E7E7E9F7E7E7EFF7E7E7EBF7E7E7E307E7E7E307E7E
      7E807E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E707E7E7EFF7E7E
      7E207E7E7E007E7E7E007E7E7E007E7E7E007E7E7E307E7E7EDF7E7E7EFF7E7E
      7EFF7E7E7EFF7E7E7EFF7E7E7EDF7E7E7E507E7E7E007E7E7E007E7E7E807E7E
      7E407E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E007E7E7E007E7E7E107E7E7EEF7E7E7E807E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E107E7E7EBF7E7E
      7EFF7E7E7EEF7E7E7E807E7E7E007E7E7E007E7E7E007E7E7E007E7E7E9F7E7E
      7E407E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E007E7E7E007E7E7E8F7E7E7EEF7E7E7E107E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E307E7E7EBF7E7E7EFF7E7E
      7EBF7E7E7E8F7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7EBF7E7E
      7E407E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E007E7E7E207E7E7EFF7E7E7E607E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E107E7E7E9F7E7E7EFF7E7E7EBF7E7E7E407E7E
      7E707E7E7E507E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7EBF7E7E
      7E807E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E007E7E7EBF7E7E7ECF7E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E707E7E7EEF7E7E7EDF7E7E7E607E7E7E007E7E7E007E7E
      7E807E7E7E507E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E8F7E7E
      7EAF7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7E507E7E7EFF7E7E7E407E7E7E007E7E7E007E7E
      7E407E7E7EDF7E7E7EFF7E7E7E807E7E7E107E7E7E007E7E7E007E7E7E007E7E
      7E807E7E7E807E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E707E7E
      7EFF7E7E7E207E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7E007E7E7EDF7E7E7E9F7E7E7E007E7E7E207E7E7EBF7E7E
      7EFF7E7E7EAF7E7E7E207E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E807E7E7ECF7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E207E7E
      7EFF7E7E7EAF7E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7EBF7E7E7EEF7E7E7E307E7E7E807E7E7EFF7E7E7ECF7E7E
      7E407E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E607E7E7EFF7E7E7E407E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7EBF7E7E7EFF7E7E7E607E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7EFF7E7E7ECF7E7E7EEF7E7E7EEF7E7E7E607E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E107E7E7EFF7E7E7EEF7E7E7E107E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E407E7E7EFF7E7E7EEF7E7E7E507E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7EFF7E7E7EFF7E7E7ECF7E7E7E507E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EBF7E7E7EFF7E7E7EEF7E7E7E507E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EBF7E7E7EFF7E7E7EFF7E7E7E807E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E
      7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E
      7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E
      7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7EFF7E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E
      7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E007E7E7E00FFFF
      FFFF9FFFFFFF8FFF1FFFA7FF1FFFA1FE3FFFB0307FFFBC007FFFBF00FFFFBFF8
      FFFFBFF07FFF9FF27FFF8FE33FFF83871FFFA0078FFFB80F87E3BF8FC083BF07
      E00FBF23F83FBE31F07FBE70407FBC78067FB8FC1E7FB8F83E7FB1E03E7FB3C3
      3E7FA3073E3FA41F3E3F807F1F1F81FF0F0F83FF878780000001FFFFFFFF}
    ApplicationMenu.IconSize = isLarge
    ApplicationMenu.Menu = rbambMain
    Caption = 'Empty'
    QuickAccessToolbar.ActionBar = rbqtbMain
    ShowHelpButton = False
    Tabs = <
      item
        Caption = 'Menu'
        Page = rbpMain
      end>
    UseCustomFrame = False
    DesignSize = (
      1218
      143)
    StyleName = 'Ribbon - Silver'
    object rbpMain: TRibbonPage
      Left = 0
      Top = 50
      Width = 1217
      Height = 93
      Caption = 'Menu'
      Index = 0
      object rbgProject: TRibbonGroup
        Left = 4
        Top = 3
        Width = 197
        Height = 86
        ActionManager = amMain
        Caption = 'Project'
        GroupIndex = 0
      end
      object rbgApplication: TRibbonGroup
        Left = 203
        Top = 3
        Width = 122
        Height = 86
        ActionManager = amMain
        Caption = 'Application'
        GroupIndex = 1
        Rows = 2
      end
      object rbngrpDebug: TRibbonGroup
        Left = 327
        Top = 3
        Width = 121
        Height = 86
        ActionManager = amMain
        Caption = 'Debug'
        GroupIndex = 2
        Rows = 2
      end
      object rbngrpTimeLineSettings: TRibbonGroup
        Left = 450
        Top = 3
        Width = 97
        Height = 86
        ActionManager = amMain
        Caption = 'Timeline'
        GroupIndex = 3
        Rows = 2
      end
      object rbngrpViewOptions: TRibbonGroup
        Left = 549
        Top = 3
        Width = 118
        Height = 86
        ActionManager = amMain
        Caption = 'View options'
        GroupIndex = 4
      end
    end
    object rbambMain: TRibbonApplicationMenuBar
      Margins.Left = 32
      Margins.Right = 32
      ActionManager = amMain
      OptionItems = <>
      RecentItems = <
        item
          Action = acRecent0
          Caption = 'acRecent0'
          Tag = 0
        end
        item
          Action = acRecent1
          Caption = 'acRecent1'
          Tag = 0
        end
        item
          Action = acRecent2
          Caption = 'acRecent2'
          Tag = 0
        end
        item
          Action = acRecent3
          Caption = 'acRecent3'
          Tag = 0
        end
        item
          Action = acRecent4
          Caption = 'acRecent4'
          Tag = 0
        end
        item
          Action = acRecent5
          Caption = 'acRecent5'
          Tag = 0
        end
        item
          Action = acRecent6
          Caption = 'acRecent6'
          Tag = 0
        end
        item
          Action = acRecent7
          Caption = 'acRecent7'
          Tag = 0
        end
        item
          Action = acRecent8
          Caption = 'acRecent8'
          Tag = 0
        end
        item
          Action = acRecent9
          Caption = 'acRecent9'
          Tag = 0
        end>
    end
    object rbqtbMain: TRibbonQuickAccessToolbar
      Left = 49
      Top = 1
      Width = 48
      Height = 24
      ActionManager = amMain
    end
  end
  object cbMainTabs: TCoolBar
    Left = 0
    Top = 143
    Width = 1218
    Height = 24
    AutoSize = True
    Bands = <
      item
        Control = actbMainTabs
        ImageIndex = -1
        MinHeight = 24
        Width = 1216
      end>
    EdgeBorders = []
    object actbMainTabs: TActionToolBar
      Left = 11
      Top = 0
      Width = 1207
      Height = 24
      ActionManager = amMain
      AllowHiding = False
      Caption = 'actbMainTabs'
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
  object cbStatusInfo: TCoolBar
    Left = 0
    Top = 712
    Width = 1218
    Height = 25
    Align = alBottom
    AutoSize = True
    BandBorderStyle = bsNone
    Bands = <
      item
        Break = False
        Control = actbStatusInfo2
        ImageIndex = -1
        Width = 831
      end
      item
        Break = False
        Control = actbStatusInfo
        ImageIndex = -1
        MinHeight = 24
        Width = 383
      end>
    EdgeBorders = []
    EdgeInner = esNone
    EdgeOuter = esNone
    object actbStatusInfo: TActionToolBar
      Left = 844
      Top = 0
      Width = 374
      Height = 24
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      ActionManager = amMain
      Caption = 'actbStatusInfo'
      ColorMap.BtnSelectedColor = clBtnFace
      ColorMap.UnusedColor = 13684944
      EdgeInner = esNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      HorzSeparator = False
      Orientation = boRightToLeft
      ParentFont = False
      Spacing = 0
    end
    object actbStatusInfo2: TActionToolBar
      Left = 11
      Top = 0
      Width = 818
      Height = 25
      ActionManager = amMain
      Align = alNone
      AllowHiding = False
      Caption = 'actbStatusInfo2'
      ColorMap.BtnSelectedColor = clBtnFace
      ColorMap.UnusedColor = 13684944
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      HorzSeparator = False
      ParentFont = False
      Spacing = 0
      object pbProgress: TProgressBar
        Left = 5
        Top = 2
        Width = 200
        Height = 17
        Position = 20
        Smooth = True
        TabOrder = 0
      end
      object pStatusAction: TPanel
        Left = 209
        Top = 2
        Width = 316
        Height = 17
        Alignment = taLeftJustify
        BevelEdges = []
        BevelOuter = bvNone
        Caption = 'Action'
        TabOrder = 1
      end
    end
  end
  object AL: TActionList
    Images = dmShareData.imlMainSmall
    Left = 912
    Top = 88
    object acAppOpen: TAction
      Category = 'Project'
      Caption = 'Open application'
      ImageIndex = 2
      OnExecute = acAppOpenExecute
    end
    object acAttachProcess: TAction
      Category = 'Project'
      Caption = 'Attach to process'
      ImageIndex = 3
      OnExecute = acAttachProcessExecute
    end
    object acDebugInfo: TAction
      Category = 'Debug'
      Caption = 'Debug Info'
      ImageIndex = 5
    end
    object acRunStop: TAction
      Category = 'Debug'
      Caption = 'Run'
      Enabled = False
      ImageIndex = 4
    end
    object acRun: TAction
      Category = 'Debug'
      Caption = 'Run'
      Enabled = False
      ImageIndex = 4
      OnExecute = acRunExecute
    end
    object acStop: TAction
      Category = 'Debug'
      Caption = 'Stop'
      Enabled = False
      ImageIndex = 11
      OnExecute = acStopExecute
    end
    object acNewProject: TAction
      Category = 'Project'
      Caption = 'New'
      ImageIndex = 0
      OnExecute = acNewProjectExecute
    end
    object acOpenProject: TAction
      Category = 'Project'
      Caption = 'Open'
      ImageIndex = 1
      OnExecute = acOpenProjectExecute
    end
    object acCloseProject: TAction
      Category = 'Project'
      Caption = 'Close'
      ImageIndex = 8
      OnExecute = acCloseProjectExecute
    end
    object acPause: TAction
      Category = 'Debug'
      Caption = 'Pause'
      ImageIndex = 6
    end
    object acOptions: TAction
      Category = 'Spider'
      Caption = 'Options'
      OnExecute = acOptionsExecute
    end
    object acExit: TAction
      Category = 'Spider'
      Caption = 'Exit'
      ImageIndex = 7
      OnExecute = acExitExecute
    end
    object acAbout: TAction
      Category = 'Spider'
      Caption = 'About'
    end
    object acSave: TAction
      Category = 'Project'
      Caption = 'Save'
      ImageIndex = 9
    end
    object acSaveCopy: TAction
      Category = 'Project'
      Caption = 'Save copy'
      ImageIndex = 10
      OnExecute = acSaveCopyExecute
    end
    object acCPUTimeLine: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'CPU timeline'
      Checked = True
      GroupIndex = 1
      OnExecute = acRealTimeLineExecute
    end
    object acRealTimeLine: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Real timeline'
      GroupIndex = 1
      OnExecute = acRealTimeLineExecute
    end
    object acTabLog: TAction
      Category = 'MainTabs'
      Caption = 'Log'
      Checked = True
      OnExecute = acMainTabExecute
    end
    object acTabDebugInfo: TAction
      Tag = 1
      Category = 'MainTabs'
      Caption = 'Debug Info'
      OnExecute = acMainTabExecute
    end
    object acTabTimeline: TAction
      Tag = 2
      Category = 'MainTabs'
      Caption = 'Process Timeline'
      OnExecute = acMainTabExecute
    end
    object acTabMemoryInfo: TAction
      Tag = 3
      Category = 'MainTabs'
      Caption = 'Memory Info'
      OnExecute = acMainTabExecute
    end
    object acTabExceptions: TAction
      Tag = 4
      Category = 'MainTabs'
      Caption = 'Exceptions'
      OnExecute = acMainTabExecute
    end
    object acStatusDebuger: TAction
      Category = 'StatusInfo'
      Caption = 'NONE'
    end
    object acStatusDbgInfo: TAction
      Category = 'StatusInfo'
      Caption = 'NONE'
    end
    object acStausEventCount: TAction
      Category = 'StatusInfo'
      Caption = 'NONE'
    end
    object acEditProject: TAction
      Category = 'Project'
      Caption = 'Settings'
      ImageIndex = 14
      OnExecute = acEditProjectExecute
    end
    object acUseShortNames: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Use short names'
      Checked = True
      OnExecute = acUseShortNamesExecute
    end
  end
  object OD: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'All supported'
        FileMask = '*.spider;*.exe;*.tds'
      end
      item
        DisplayName = 'Spider project (*.spider)'
        FileMask = '*.spider'
      end
      item
        DisplayName = 'Application (*.exe)'
        FileMask = '*.exe'
      end
      item
        DisplayName = 'External debug info (*.tds)'
        FileMask = '*.tds'
      end>
    Options = [fdoFileMustExist]
    Left = 808
    Top = 88
  end
  object tmrThreadsUpdate: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmrThreadsUpdateTimer
    Left = 808
    Top = 32
  end
  object amMain: TActionManager
    ActionBars = <
      item
        Items = <
          item
            ChangesAllowed = [caModify]
            BackgroundLayout = blRightBanner
            Items = <
              item
                Action = acNewProject
                Caption = '&New'
                ImageIndex = 0
              end
              item
                Action = acOpenProject
                Caption = '&Open'
                ImageIndex = 1
              end
              item
                Action = acEditProject
                Caption = '&Settings'
                ImageIndex = 14
              end
              item
                Action = acSaveCopy
                Caption = 'Sa&ve copy'
                ImageIndex = 10
              end
              item
                Action = acCloseProject
                Caption = '&Close'
                ImageIndex = 8
              end
              item
                Caption = '-'
                CommandStyle = csSeparator
                CommandProperties.Width = -1
                CommandProperties.Font.Charset = DEFAULT_CHARSET
                CommandProperties.Font.Color = clWindowText
                CommandProperties.Font.Height = -11
                CommandProperties.Font.Name = 'Tahoma'
                CommandProperties.Font.Style = []
                CommandProperties.ParentFont = False
              end
              item
                Action = acAppOpen
                Caption = 'O&pen application'
                ImageIndex = 2
              end
              item
                Action = acAttachProcess
                Caption = '&Attach to process'
                ImageIndex = 3
              end
              item
                Caption = '-'
                CommandStyle = csSeparator
                CommandProperties.Width = -1
                CommandProperties.Font.Charset = DEFAULT_CHARSET
                CommandProperties.Font.Color = clWindowText
                CommandProperties.Font.Height = -11
                CommandProperties.Font.Name = 'Tahoma'
                CommandProperties.Font.Style = []
                CommandProperties.ParentFont = False
              end
              item
                Action = acExit
                Caption = '&Exit'
                ImageIndex = 7
              end>
            Caption = '&MainMenu'
            ImageIndex = 0
            KeyTip = 'F'
            CommandProperties.ButtonSize = bsLarge
          end>
        ActionBar = rbambMain
        AutoSize = False
      end
      item
        ActionBar = rbqtbMain
        AutoSize = False
      end
      item
        Items = <
          item
            Action = acNewProject
            Caption = '&New'
            ImageIndex = 0
            CommandProperties.ButtonSize = bsLarge
          end
          item
            Caption = '-'
          end
          item
            Action = acOpenProject
            Caption = '&Open'
            ImageIndex = 1
            CommandProperties.ButtonSize = bsLarge
          end
          item
            Caption = '-'
          end
          item
            Action = acCloseProject
            Caption = '&Close'
            ImageIndex = 8
          end
          item
            Action = acEditProject
            Caption = '&Settings'
            ImageIndex = 14
          end
          item
            Action = acSaveCopy
            Caption = 'S&ave copy'
            ImageIndex = 10
          end>
        ActionBar = rbgProject
      end
      item
        Items = <
          item
            Action = acRun
            Caption = '&Run'
            CommandProperties.ButtonSize = bsLarge
          end
          item
            Action = acDebugInfo
            Caption = '&DebugInfo'
          end
          item
            Action = acPause
            Caption = '&Pause'
          end>
      end
      item
        Items = <
          item
            Action = acAppOpen
            Caption = '&Open application'
            ImageIndex = 2
          end
          item
            Action = acAttachProcess
            Caption = '&Attach to process'
            ImageIndex = 3
          end>
        ActionBar = rbgApplication
      end
      item
        Items = <
          item
            Action = acRunStop
            Caption = '&Run'
            ImageIndex = 4
            CommandProperties.ButtonSize = bsLarge
          end
          item
            Caption = '-'
          end
          item
            Action = acPause
            Caption = '&Pause'
            ImageIndex = 6
          end>
        ActionBar = rbngrpDebug
      end
      item
        Items = <
          item
            Action = acCPUTimeLine
            Caption = '&CPU timeline'
            CommandStyle = csRadioButton
            CommandProperties.Width = -1
          end
          item
            Action = acRealTimeLine
            Caption = '&Real timeline'
            CommandStyle = csRadioButton
            CommandProperties.Width = -1
          end>
        ActionBar = rbngrpTimeLineSettings
      end
      item
        Items = <
          item
            Action = acTabLog
            Caption = '&Log'
          end
          item
            Caption = '-'
          end
          item
            Action = acTabDebugInfo
            Caption = '&Debug Info'
          end
          item
            Caption = '-'
          end
          item
            Action = acTabTimeline
            Caption = '&Process Timeline'
          end
          item
            Caption = '-'
          end
          item
            Action = acTabMemoryInfo
            Caption = '&Memory Info'
          end
          item
            Caption = '-'
          end
          item
            Action = acTabExceptions
            Caption = '&Exceptions'
          end>
        ActionBar = actbMainTabs
      end
      item
        Items.AutoHotKeys = False
        Items = <
          item
            Caption = 'none'
            CommandStyle = csText
            CommandProperties.Width = -1
            CommandProperties.Font.Charset = DEFAULT_CHARSET
            CommandProperties.Font.Color = clWindowText
            CommandProperties.Font.Height = -11
            CommandProperties.Font.Name = 'Tahoma'
            CommandProperties.Font.Style = [fsBold]
            CommandProperties.ParentFont = False
          end
          item
            Caption = 'DbgInfo:'
            CommandStyle = csText
            CommandProperties.Width = -1
            CommandProperties.Font.Charset = DEFAULT_CHARSET
            CommandProperties.Font.Color = clWindowText
            CommandProperties.Font.Height = -11
            CommandProperties.Font.Name = 'Tahoma'
            CommandProperties.Font.Style = []
            CommandProperties.ParentFont = False
          end
          item
            Caption = '-'
          end
          item
            Caption = 'none'
            CommandStyle = csText
            CommandProperties.Width = -1
            CommandProperties.Font.Charset = DEFAULT_CHARSET
            CommandProperties.Font.Color = clWindowText
            CommandProperties.Font.Height = -11
            CommandProperties.Font.Name = 'Tahoma'
            CommandProperties.Font.Style = []
            CommandProperties.ParentFont = False
          end
          item
            Caption = 'DbgStatus:'
            CommandStyle = csText
            CommandProperties.Width = -1
            CommandProperties.Font.Charset = DEFAULT_CHARSET
            CommandProperties.Font.Color = clWindowText
            CommandProperties.Font.Height = -11
            CommandProperties.Font.Name = 'Tahoma'
            CommandProperties.Font.Style = []
            CommandProperties.ParentFont = False
          end
          item
            Caption = '-'
          end
          item
            Caption = '0'
            CommandStyle = csText
            CommandProperties.Width = -1
            CommandProperties.Font.Charset = DEFAULT_CHARSET
            CommandProperties.Font.Color = clWindowText
            CommandProperties.Font.Height = -11
            CommandProperties.Font.Name = 'Tahoma'
            CommandProperties.Font.Style = []
            CommandProperties.ParentFont = False
          end
          item
            Caption = 'Events:'
            CommandStyle = csText
            CommandProperties.Width = -1
            CommandProperties.Font.Charset = DEFAULT_CHARSET
            CommandProperties.Font.Color = clWindowText
            CommandProperties.Font.Height = -11
            CommandProperties.Font.Name = 'Tahoma'
            CommandProperties.Font.Style = []
            CommandProperties.ParentFont = False
          end
          item
            Caption = '-'
          end>
        ActionBar = actbStatusInfo
      end
      item
      end
      item
        ChangesAllowed = []
        Items.AutoHotKeys = False
        Items.Customizable = False
        Items.HideUnused = False
        Items.CaptionOptions = coNone
        Items = <
          item
            Items.HideUnused = False
            Items = <>
            Caption = ' '
            CommandStyle = csControl
            ShowGlyph = False
            ShowShortCut = False
            CommandProperties.Width = 204
            CommandProperties.ContainedControl = pbProgress
            CommandProperties.Text = ' '
          end
          item
            Items.HideUnused = False
            Items = <>
            Caption = ' '
            CommandStyle = csControl
            CommandProperties.Width = 320
            CommandProperties.ContainedControl = pStatusAction
          end>
        ActionBar = actbStatusInfo2
      end
      item
        Items = <
          item
            Action = acUseShortNames
            Caption = '&Use short names'
            CommandStyle = csCheckBox
            CommandProperties.Width = -1
          end>
        ActionBar = rbngrpViewOptions
      end>
    DisabledImages = dmShareData.imlMainSmall
    LargeDisabledImages = dmShareData.imlMain
    LargeImages = dmShareData.imlMain
    LinkedActionLists = <
      item
        ActionList = AL
        Caption = 'AL'
      end
      item
        ActionList = ALRecent
        Caption = 'ALRecent'
      end>
    Images = dmShareData.imlMainSmall
    Left = 976
    Top = 88
    StyleName = 'Ribbon - Silver'
  end
  object ALRecent: TActionList
    Left = 912
    Top = 32
    object acRecent0: TAction
      Caption = 'acRecent0'
      OnExecute = acRecentExecute
    end
    object acRecent1: TAction
      Caption = 'acRecent1'
      OnExecute = acRecentExecute
    end
    object acRecent2: TAction
      Caption = 'acRecent2'
      OnExecute = acRecentExecute
    end
    object acRecent3: TAction
      Caption = 'acRecent3'
      OnExecute = acRecentExecute
    end
    object acRecent4: TAction
      Caption = 'acRecent4'
      OnExecute = acRecentExecute
    end
    object acRecent5: TAction
      Caption = 'acRecent5'
      OnExecute = acRecentExecute
    end
    object acRecent6: TAction
      Caption = 'acRecent6'
      OnExecute = acRecentExecute
    end
    object acRecent7: TAction
      Caption = 'acRecent7'
      OnExecute = acRecentExecute
    end
    object acRecent8: TAction
      Caption = 'acRecent8'
      OnExecute = acRecentExecute
    end
    object acRecent9: TAction
      Caption = 'acRecent9'
      OnExecute = acRecentExecute
    end
  end
end
