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
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pcMain: TPageControl
    Left = 0
    Top = 167
    Width = 1218
    Height = 545
    ActivePage = tsMemInfo
    Align = alClient
    TabOrder = 0
    OnChange = pcMainChange
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
        Width = 362
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
            Width = 75
            WideText = 'Offset'
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
            Position = 2
            Width = 60
            WideText = 'Size'
          end>
      end
      object pDbgInfoDetail: TPanel
        Left = 362
        Top = 0
        Width = 848
        Height = 517
        Align = alClient
        BevelEdges = []
        BevelOuter = bvNone
        TabOrder = 1
        object pcDbgInfoDetail: TPageControl
          Left = 0
          Top = 0
          Width = 848
          Height = 517
          ActivePage = tsDbgUnitConsts
          Align = alClient
          TabOrder = 0
          object tsDbgUnitConsts: TTabSheet
            Caption = 'Consts'
            object vstDbgInfoConsts: TVirtualStringTree
              Left = 0
              Top = 0
              Width = 840
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
              Width = 840
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
                end
                item
                  Alignment = taRightJustify
                  CaptionAlignment = taCenter
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                  Position = 2
                  Width = 60
                  WideText = 'Data size'
                end
                item
                  Alignment = taRightJustify
                  CaptionAlignment = taCenter
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                  Position = 3
                  Width = 80
                  WideText = 'Offset'
                end>
            end
          end
          object tsDbgUnitVars: TTabSheet
            Caption = 'Vars'
            ImageIndex = 2
            object vstDbgInfoVars: TVirtualStringTree
              Left = 0
              Top = 0
              Width = 840
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
              Width = 293
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
                Width = 293
                Height = 3
                Cursor = crVSplit
                Align = alTop
                ExplicitWidth = 257
              end
              object vstDbgInfoFuncVars: TVirtualStringTree
                Left = 0
                Top = 0
                Width = 293
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
                Width = 293
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
              Width = 840
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
        Width = 451
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
            Width = 80
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
        Left = 451
        Top = 0
        Width = 759
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
        Width = 476
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
            Width = 80
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
      object pMemInfoClient: TPanel
        Left = 476
        Top = 0
        Width = 734
        Height = 517
        Align = alClient
        BevelEdges = []
        BevelOuter = bvNone
        Caption = 'pMemInfoClient'
        ShowCaption = False
        TabOrder = 1
        object cbMemInfo: TCoolBar
          Left = 0
          Top = 0
          Width = 734
          Height = 24
          AutoSize = True
          BandBorderStyle = bsNone
          Bands = <
            item
              Control = actbMemInfo
              ImageIndex = -1
              MinHeight = 24
              Width = 732
            end>
          EdgeBorders = []
          EdgeInner = esNone
          EdgeOuter = esNone
          FixedOrder = True
          object actbMemInfo: TActionToolBar
            Left = 2
            Top = 0
            Width = 732
            Height = 24
            ActionManager = amMain
            Caption = 'actbMemInfo'
            ColorMap.MenuColor = clMenu
            ColorMap.BtnSelectedColor = clBtnFace
            ColorMap.UnusedColor = 13684944
            EdgeInner = esNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            Spacing = 0
          end
        end
        object pcMemInfo: TPageControl
          Left = 0
          Top = 24
          Width = 734
          Height = 493
          ActivePage = tsMemInfoTreeView
          Align = alClient
          TabOrder = 1
          OnChange = pcMemInfoChange
          object tsMemInfoViewStack: TTabSheet
            Caption = 'Simple view'
            object pnl1: TPanel
              Left = 0
              Top = 0
              Width = 726
              Height = 465
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              object pMemoryInfoAdv: TPanel
                Left = 305
                Top = 0
                Width = 421
                Height = 465
                Align = alClient
                BevelOuter = bvNone
                Caption = 'pMemoryInfoAdv'
                ShowCaption = False
                TabOrder = 0
                object splMemInfoAdv: TSplitter
                  Left = 0
                  Top = 225
                  Width = 421
                  Height = 3
                  Cursor = crVSplit
                  Align = alTop
                  ExplicitWidth = 427
                end
                object vstMemStack: TVirtualStringTree
                  Left = 0
                  Top = 0
                  Width = 421
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
                  Width = 421
                  Height = 237
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
              object vstMemList: TVirtualStringTree
                Left = 0
                Top = 0
                Width = 305
                Height = 465
                Align = alLeft
                Header.AutoSizeIndex = 0
                Header.Font.Charset = DEFAULT_CHARSET
                Header.Font.Color = clWindowText
                Header.Font.Height = -11
                Header.Font.Name = 'Tahoma'
                Header.Font.Style = []
                Header.Options = [hoColumnResize, hoVisible, hoHeaderClickAutoSort]
                Header.SortColumn = 2
                Header.SortDirection = sdDescending
                Header.Style = hsFlatButtons
                ScrollBarOptions.AlwaysVisible = True
                TabOrder = 1
                TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
                TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
                TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
                TreeOptions.SelectionOptions = [toFullRowSelect]
                OnColumnResize = vstThreadsColumnResize
                OnCompareNodes = vstMemInfoObjectsCompareNodes
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
            end
          end
          object tsMemInfoTreeView: TTabSheet
            Caption = 'Tree view'
            ImageIndex = 1
            object spl2: TSplitter
              Left = 0
              Top = 213
              Width = 726
              Height = 3
              Cursor = crVSplit
              Align = alBottom
              ExplicitWidth = 732
            end
            object pMemInfoTreeLeft: TPanel
              Left = 0
              Top = 0
              Width = 726
              Height = 213
              Align = alClient
              BevelEdges = []
              BevelOuter = bvNone
              Caption = 'pMemInfoTreeLeft'
              ShowCaption = False
              TabOrder = 0
              object vstMemInfoFuncTree: TVirtualStringTree
                Left = 0
                Top = 0
                Width = 456
                Height = 213
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
                Header.Options = [hoColumnResize, hoVisible, hoHeaderClickAutoSort]
                Header.SortColumn = 2
                Header.SortDirection = sdDescending
                ScrollBarOptions.AlwaysVisible = True
                TabOrder = 0
                TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
                TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
                TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
                TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
                OnColumnResize = vstThreadsColumnResize
                OnCompareNodes = vstMemInfoFuncTreeCompareNodes
                OnDrawText = vstTrackFuncsDrawText
                OnFocusChanged = vstMemInfoFuncTreeFocusChanged
                OnGetText = vstMemInfoFuncTreeGetText
                OnGetNodeDataSize = vstThreadsGetNodeDataSize
                Columns = <
                  item
                    CaptionAlignment = taCenter
                    Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
                    Position = 0
                    Width = 300
                    WideText = 'Function name'
                  end
                  item
                    Alignment = taRightJustify
                    CaptionAlignment = taCenter
                    DefaultSortDirection = sdDescending
                    Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                    Position = 1
                    Width = 70
                    WideText = 'Count'
                  end
                  item
                    Alignment = taRightJustify
                    CaptionAlignment = taCenter
                    DefaultSortDirection = sdDescending
                    Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                    Position = 2
                    Width = 70
                    WideText = 'Size'
                  end>
              end
              object pcMemInfoFuncInfo: TPageControl
                Left = 456
                Top = 0
                Width = 270
                Height = 213
                ActivePage = tsMemInfoFuncLinks
                Align = alClient
                TabOrder = 1
                object tsMemInfoFuncLinks: TTabSheet
                  Caption = 'Links'
                  object pMemInfoFuncLinks: TPanel
                    Left = 0
                    Top = 0
                    Width = 262
                    Height = 185
                    Align = alClient
                    BevelOuter = bvNone
                    ShowCaption = False
                    TabOrder = 0
                    OnResize = pMemInfoFuncLinksResize
                    object spl1: TSplitter
                      Left = 0
                      Top = 100
                      Width = 262
                      Height = 3
                      Cursor = crVSplit
                      Align = alTop
                      ExplicitWidth = 268
                    end
                    object vstMemInfoFuncParents: TVirtualStringTree
                      Left = 0
                      Top = 0
                      Width = 262
                      Height = 100
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
                      OnDblClick = vstMemInfoFuncParentsDblClick
                      OnDrawText = vstTrackFuncLinksDrawText
                      OnGetText = vstMemInfoFuncParentsGetText
                      OnGetNodeDataSize = vstThreadsGetNodeDataSize
                      Columns = <
                        item
                          CaptionAlignment = taCenter
                          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
                          Position = 0
                          Width = 300
                          WideText = 'Parent function name'
                        end
                        item
                          Alignment = taRightJustify
                          CaptionAlignment = taCenter
                          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                          Position = 1
                          WideText = 'LineNo'
                        end
                        item
                          Alignment = taRightJustify
                          CaptionAlignment = taCenter
                          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                          Position = 2
                          Width = 70
                          WideText = 'Count'
                        end
                        item
                          Alignment = taRightJustify
                          CaptionAlignment = taCenter
                          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                          Position = 3
                          Width = 70
                          WideText = 'Size'
                        end>
                    end
                    object vstMemInfoFuncChilds: TVirtualStringTree
                      Left = 0
                      Top = 103
                      Width = 262
                      Height = 82
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
                      TabOrder = 1
                      TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
                      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
                      TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
                      OnDblClick = vstMemInfoFuncChildsDblClick
                      OnDrawText = vstTrackFuncLinksDrawText
                      OnGetText = vstMemInfoFuncChildsGetText
                      OnGetNodeDataSize = vstThreadsGetNodeDataSize
                      Columns = <
                        item
                          CaptionAlignment = taCenter
                          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
                          Position = 0
                          Width = 300
                          WideText = 'Child function name'
                        end
                        item
                          Alignment = taRightJustify
                          CaptionAlignment = taCenter
                          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                          Position = 1
                          WideText = 'LineNo'
                        end
                        item
                          Alignment = taRightJustify
                          CaptionAlignment = taCenter
                          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                          Position = 2
                          Width = 70
                          WideText = 'Count'
                        end
                        item
                          Alignment = taRightJustify
                          CaptionAlignment = taCenter
                          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                          Position = 3
                          Width = 70
                          WideText = 'Size'
                        end>
                    end
                  end
                end
                object tsMemInfoFuncSrc: TTabSheet
                  Caption = 'Source'
                  ImageIndex = 1
                  object synmMemInfoFuncSrc: TSynMemo
                    Left = 0
                    Top = 0
                    Width = 262
                    Height = 185
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
                    FontSmoothing = fsmNone
                  end
                end
              end
            end
            object pMemInfoButtom: TPanel
              Left = 0
              Top = 216
              Width = 726
              Height = 249
              Align = alBottom
              BevelEdges = []
              BevelOuter = bvNone
              Caption = 'pMemInfoButtom'
              ShowCaption = False
              TabOrder = 1
              object vstMemInfoObjects: TVirtualStringTree
                Left = 0
                Top = 0
                Width = 356
                Height = 249
                Align = alLeft
                Header.AutoSizeIndex = 0
                Header.Font.Charset = DEFAULT_CHARSET
                Header.Font.Color = clWindowText
                Header.Font.Height = -11
                Header.Font.Name = 'Tahoma'
                Header.Font.Style = []
                Header.Options = [hoColumnResize, hoVisible, hoHeaderClickAutoSort]
                Header.SortColumn = 2
                Header.SortDirection = sdDescending
                Header.Style = hsFlatButtons
                ScrollBarOptions.AlwaysVisible = True
                TabOrder = 0
                TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
                TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
                TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
                TreeOptions.SelectionOptions = [toFullRowSelect]
                OnColumnResize = vstThreadsColumnResize
                OnCompareNodes = vstMemInfoObjectsCompareNodes
                OnFocusChanged = vstMemInfoObjectsFocusChanged
                OnGetText = vstMemInfoObjectsGetText
                OnGetNodeDataSize = vstThreadsGetNodeDataSize
                Columns = <
                  item
                    CaptionAlignment = taCenter
                    Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
                    Position = 0
                    Width = 200
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
              object vstMemInfoObjStack: TVirtualStringTree
                Left = 356
                Top = 0
                Width = 370
                Height = 249
                Align = alClient
                Header.AutoSizeIndex = 0
                Header.Font.Charset = DEFAULT_CHARSET
                Header.Font.Color = clWindowText
                Header.Font.Height = -11
                Header.Font.Name = 'Tahoma'
                Header.Font.Style = []
                Header.Options = [hoColumnResize, hoShowSortGlyphs, hoVisible]
                Header.Style = hsFlatButtons
                ScrollBarOptions.AlwaysVisible = True
                TabOrder = 1
                TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
                TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
                TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
                OnDblClick = vstMemInfoObjStackDblClick
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
            end
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
        Width = 401
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
            Width = 80
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
      object pExceptionInfo: TPanel
        Left = 401
        Top = 0
        Width = 809
        Height = 517
        Align = alClient
        BevelEdges = []
        BevelOuter = bvNone
        Caption = 'pExceptionInfo'
        ShowCaption = False
        TabOrder = 1
        object pnl2: TPanel
          Left = 0
          Top = 24
          Width = 809
          Height = 493
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object vstExceptionList: TVirtualStringTree
            Left = 0
            Top = 0
            Width = 511
            Height = 493
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
            Width = 298
            Height = 493
            Align = alClient
            BevelEdges = []
            BevelOuter = bvNone
            Caption = 'pExceptInfoAdv'
            ShowCaption = False
            TabOrder = 1
            object splExceptInfoAdv: TSplitter
              Left = 0
              Top = 217
              Width = 298
              Height = 3
              Cursor = crVSplit
              Align = alTop
              ExplicitWidth = 303
            end
            object vstExceptionCallStack: TVirtualStringTree
              Left = 0
              Top = 0
              Width = 298
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
              Width = 298
              Height = 273
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
        object cbExceptionInfo: TCoolBar
          Left = 0
          Top = 0
          Width = 809
          Height = 24
          AutoSize = True
          BandBorderStyle = bsNone
          Bands = <
            item
              Control = actbExceptionInfo
              ImageIndex = -1
              MinHeight = 24
              Width = 807
            end>
          EdgeBorders = [ebTop]
          EdgeInner = esNone
          EdgeOuter = esNone
          FixedOrder = True
          object actbExceptionInfo: TActionToolBar
            Left = 2
            Top = 0
            Width = 807
            Height = 24
            ActionManager = amMain
            Caption = 'actbExceptionInfo'
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
      end
    end
    object tsCodeTracking: TTabSheet
      Caption = 'Code tracking'
      ImageIndex = 5
      object vstTrackThreads: TVirtualStringTree
        Left = 0
        Top = 0
        Width = 532
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
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoTristateTracking, toAutoDeleteMovedNodes]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
        TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus, toFullRowSelect]
        OnColumnResize = vstThreadsColumnResize
        OnDrawText = vstThreadsDrawText
        OnFocusChanged = vstTrackThreadsFocusChanged
        OnFocusChanging = vstTrackThreadsFocusChanging
        OnGetText = vstTrackThreadsGetText
        OnGetNodeDataSize = vstThreadsGetNodeDataSize
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
            Width = 80
            WideText = 'ID'
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
            Position = 2
            Width = 65
            WideText = 'Call Count'
          end
          item
            Alignment = taRightJustify
            CaptionAlignment = taCenter
            Options = [coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coUseCaptionAlignment]
            Position = 3
            Width = 70
            WideText = 'CPU time'
          end>
      end
      object pCodeTrackingInfo: TPanel
        Left = 532
        Top = 0
        Width = 678
        Height = 517
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pCodeTrackingInfo'
        ShowCaption = False
        TabOrder = 1
        object pTrackAdv: TPanel
          Left = 0
          Top = 26
          Width = 678
          Height = 491
          Align = alClient
          BevelOuter = bvNone
          Caption = 'pTrackAdv'
          ShowCaption = False
          TabOrder = 0
          object vstTrackFuncs: TVirtualStringTree
            Left = 0
            Top = 0
            Width = 457
            Height = 491
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
            Header.Options = [hoColumnResize, hoVisible, hoHeaderClickAutoSort]
            Header.SortColumn = 2
            Header.SortDirection = sdDescending
            ScrollBarOptions.AlwaysVisible = True
            TabOrder = 0
            TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
            TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
            TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
            TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
            OnColumnResize = vstThreadsColumnResize
            OnCompareNodes = vstTrackFuncsCompareNodes
            OnDrawText = vstTrackFuncsDrawText
            OnFocusChanged = vstTrackFuncsFocusChanged
            OnFocusChanging = vstTrackFuncsFocusChanging
            OnGetText = vstTrackFuncsGetText
            OnGetNodeDataSize = vstThreadsGetNodeDataSize
            Columns = <
              item
                CaptionAlignment = taCenter
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
                Position = 0
                Width = 300
                WideText = 'Function name'
              end
              item
                Alignment = taRightJustify
                CaptionAlignment = taCenter
                DefaultSortDirection = sdDescending
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                Position = 1
                Width = 70
                WideText = 'Call count'
              end
              item
                Alignment = taRightJustify
                CaptionAlignment = taCenter
                DefaultSortDirection = sdDescending
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                Position = 2
                Width = 70
                WideText = 'CPU time'
              end>
          end
          object pcTrackFuncAdv: TPageControl
            Left = 457
            Top = 0
            Width = 221
            Height = 491
            ActivePage = tsTrackFuncAdvLinks
            Align = alClient
            TabOrder = 1
            object tsTrackFuncAdvLinks: TTabSheet
              Caption = 'Links'
              object pTrackFuncAdv: TPanel
                Left = 0
                Top = 0
                Width = 213
                Height = 463
                Align = alClient
                BevelOuter = bvNone
                Caption = 'pTrackFuncAdv'
                ShowCaption = False
                TabOrder = 0
                OnResize = pTrackFuncAdvResize
                object splTrackFuncAdv: TSplitter
                  Left = 0
                  Top = 329
                  Width = 213
                  Height = 3
                  Cursor = crVSplit
                  Align = alTop
                  ExplicitWidth = 218
                end
                object vstTrackFuncParent: TVirtualStringTree
                  Left = 0
                  Top = 0
                  Width = 213
                  Height = 329
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
                  PopupMenu = pmTrackFuncAdvParents
                  ScrollBarOptions.AlwaysVisible = True
                  TabOrder = 0
                  TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
                  TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
                  TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
                  OnDblClick = vstTrackFuncParentDblClick
                  OnDrawText = vstTrackFuncLinksDrawText
                  OnGetText = vstTrackFuncParentGetText
                  OnGetNodeDataSize = vstThreadsGetNodeDataSize
                  Columns = <
                    item
                      CaptionAlignment = taCenter
                      Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
                      Position = 0
                      Width = 300
                      WideText = 'Parent function name'
                    end
                    item
                      Alignment = taRightJustify
                      CaptionAlignment = taCenter
                      Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                      Position = 1
                      WideText = 'LineNo'
                    end
                    item
                      Alignment = taRightJustify
                      CaptionAlignment = taCenter
                      Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                      Position = 2
                      Width = 70
                      WideText = 'Call count'
                    end
                    item
                      Alignment = taRightJustify
                      CaptionAlignment = taCenter
                      Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                      Position = 3
                      Width = 70
                      WideText = 'CPU time'
                    end>
                end
                object vstTrackFuncChilds: TVirtualStringTree
                  Left = 0
                  Top = 332
                  Width = 213
                  Height = 131
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
                  TabOrder = 1
                  TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
                  TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
                  TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
                  OnDblClick = vstTrackFuncChildsDblClick
                  OnDrawText = vstTrackFuncLinksDrawText
                  OnGetText = vstTrackFuncChildsGetText
                  OnGetNodeDataSize = vstThreadsGetNodeDataSize
                  Columns = <
                    item
                      CaptionAlignment = taCenter
                      Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus, coUseCaptionAlignment]
                      Position = 0
                      Width = 300
                      WideText = 'Child function name'
                    end
                    item
                      Alignment = taRightJustify
                      CaptionAlignment = taCenter
                      Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                      Position = 1
                      WideText = 'LineNo'
                    end
                    item
                      Alignment = taRightJustify
                      CaptionAlignment = taCenter
                      Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                      Position = 2
                      Width = 70
                      WideText = 'Call count'
                    end
                    item
                      Alignment = taRightJustify
                      CaptionAlignment = taCenter
                      Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAllowFocus, coUseCaptionAlignment]
                      Position = 3
                      Width = 70
                      WideText = 'CPU time'
                    end>
                end
              end
            end
            object tsTrackFuncAdvSrc: TTabSheet
              Caption = 'Source'
              ImageIndex = 1
              object synmTrackFuncAdvSource: TSynMemo
                Left = 0
                Top = 0
                Width = 213
                Height = 463
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
                FontSmoothing = fsmNone
              end
            end
          end
        end
        object cbCodeTrackingInfo: TCoolBar
          Left = 0
          Top = 0
          Width = 678
          Height = 26
          AutoSize = True
          BandBorderStyle = bsNone
          Bands = <
            item
              Control = actbCodeTrackingInfo
              ImageIndex = -1
              MinHeight = 26
              Width = 676
            end>
          EdgeBorders = []
          FixedSize = True
          FixedOrder = True
          object actbCodeTrackingInfo: TActionToolBar
            Left = 2
            Top = 0
            Width = 676
            Height = 26
            ActionManager = amMain
            Caption = 'actbCodeTrackingInfo'
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
      end
    end
    object tsUpdateInfo: TTabSheet
      Caption = 'Update info'
      ImageIndex = 6
      object vstUpdateInfo: TVirtualStringTree
        Left = 0
        Top = 24
        Width = 1210
        Height = 493
        Align = alClient
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
        TabOrder = 0
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
        OnDrawText = vstUpdateInfoDrawText
        OnGetText = vstUpdateInfoGetText
        OnGetNodeDataSize = vstThreadsGetNodeDataSize
        Columns = <
          item
            Position = 0
            Width = 1206
            WideText = 'Info'
          end>
      end
      object cbUpdateInfo: TCoolBar
        Left = 0
        Top = 0
        Width = 1210
        Height = 24
        AutoSize = True
        BandBorderStyle = bsNone
        Bands = <
          item
            Control = actbUpdateInfo
            ImageIndex = -1
            MinHeight = 24
            Width = 1208
          end>
        EdgeInner = esNone
        EdgeOuter = esNone
        FixedOrder = True
        object actbUpdateInfo: TActionToolBar
          Left = 2
          Top = 0
          Width = 1208
          Height = 24
          ActionManager = amMain
          Caption = 'actbUpdateInfo'
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
      object rbngrpDbgInfoOptions: TRibbonGroup
        Left = 450
        Top = 3
        Width = 118
        Height = 86
        ActionManager = amMain
        Caption = 'Debug info options'
        GroupIndex = 3
      end
      object rbngrpTimeLineSettings: TRibbonGroup
        Left = 570
        Top = 3
        Width = 114
        Height = 86
        ActionManager = amMain
        Caption = 'Timeline options'
        GroupIndex = 4
      end
      object rbngrpMemInfoOptions: TRibbonGroup
        Left = 686
        Top = 3
        Width = 154
        Height = 86
        ActionManager = amMain
        Caption = 'Memory info options'
        GroupIndex = 5
      end
      object rbngrpExceptionOptions: TRibbonGroup
        Left = 842
        Top = 3
        Width = 131
        Height = 86
        ActionManager = amMain
        Caption = 'Exception options'
        GroupIndex = 6
      end
      object rbngrpCodeTracking: TRibbonGroup
        Left = 975
        Top = 3
        Width = 127
        Height = 86
        ActionManager = amMain
        Caption = 'Code tracking options'
        GroupIndex = 7
      end
      object rbngrpFeedback: TRibbonGroup
        Left = 1104
        Top = 3
        Width = 61
        Height = 86
        ActionManager = amMain
        Caption = 'Feedback'
        GroupIndex = 8
        Rows = 1
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
    FixedSize = True
    FixedOrder = True
    object actbMainTabs: TActionToolBar
      Left = 2
      Top = 0
      Width = 1216
      Height = 24
      ActionManager = amMain
      AllowHiding = False
      Caption = 'actbMainTabs'
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
  object cbStatusInfo: TCoolBar
    Left = 0
    Top = 712
    Width = 1218
    Height = 25
    Align = alBottom
    AutoSize = True
    Bands = <
      item
        Break = False
        Control = pStatusBar
        HorizontalOnly = True
        ImageIndex = -1
        Width = 1216
      end>
    EdgeBorders = []
    EdgeInner = esNone
    EdgeOuter = esNone
    FixedSize = True
    FixedOrder = True
    object pStatusBar: TPanel
      Left = 2
      Top = 0
      Width = 1216
      Height = 25
      BevelEdges = []
      BevelOuter = bvNone
      Caption = 'pStatusBar'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      ShowCaption = False
      TabOrder = 0
      object lbStatusAction: TLabel
        Left = 208
        Top = 6
        Width = 69
        Height = 13
        Caption = 'lbStatusAction'
      end
      object pbProgress: TProgressBar
        Left = 2
        Top = 3
        Width = 200
        Height = 17
        Position = 20
        Smooth = True
        TabOrder = 0
      end
      object pStatusDbgInfo: TPanel
        Left = 1080
        Top = 0
        Width = 136
        Height = 25
        Align = alRight
        Caption = 'pStatusDbgInfo'
        ShowCaption = False
        TabOrder = 1
        object lbStatusDbgInfoValue: TLabel
          AlignWithMargins = True
          Left = 56
          Top = 3
          Width = 70
          Height = 19
          Margins.Left = 1
          Margins.Top = 2
          Margins.Right = 5
          Margins.Bottom = 2
          Align = alLeft
          AutoSize = False
          Caption = 'External (TDS)'
          Layout = tlCenter
        end
        object lbStatusDbgInfoLabel: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 3
          Width = 44
          Height = 19
          Margins.Left = 5
          Margins.Top = 2
          Margins.Right = 5
          Margins.Bottom = 2
          Align = alLeft
          Caption = 'Dbg info:'
          Layout = tlCenter
          ExplicitHeight = 13
        end
      end
      object pStatusDbgState: TPanel
        Left = 960
        Top = 0
        Width = 120
        Height = 25
        Align = alRight
        Caption = 'pStatusDbgState'
        ShowCaption = False
        TabOrder = 2
        object lbStatusDbgStateLabel: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 3
          Width = 51
          Height = 19
          Margins.Left = 5
          Margins.Top = 2
          Margins.Right = 5
          Margins.Bottom = 2
          Align = alLeft
          Caption = 'Dbg state:'
          Layout = tlCenter
          ExplicitHeight = 13
        end
        object lbStatusDbgStateValue: TLabel
          AlignWithMargins = True
          Left = 63
          Top = 3
          Width = 50
          Height = 19
          Margins.Left = 1
          Margins.Top = 2
          Margins.Right = 5
          Margins.Bottom = 2
          Align = alLeft
          AutoSize = False
          Caption = 'Debug Fail'
          Layout = tlCenter
        end
      end
      object pStatusEventCnt: TPanel
        Left = 840
        Top = 0
        Width = 120
        Height = 25
        Align = alRight
        Caption = 'pStatusEventCnt'
        ShowCaption = False
        TabOrder = 3
        object lbStateEventCntLabel: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 3
          Width = 59
          Height = 19
          Margins.Left = 5
          Margins.Top = 2
          Margins.Right = 5
          Margins.Bottom = 2
          Align = alLeft
          Caption = 'Dbg events:'
          Layout = tlCenter
          ExplicitHeight = 13
        end
        object lbStatusEventsCntValue: TLabel
          AlignWithMargins = True
          Left = 71
          Top = 3
          Width = 42
          Height = 19
          Margins.Left = 1
          Margins.Top = 2
          Margins.Right = 5
          Margins.Bottom = 2
          Align = alLeft
          Alignment = taRightJustify
          AutoSize = False
          Caption = '0000000'
          Layout = tlCenter
        end
      end
      object pStatusTrackEventCnt: TPanel
        Left = 709
        Top = 0
        Width = 131
        Height = 25
        Align = alRight
        Caption = 'pStatusEventCnt'
        ShowCaption = False
        TabOrder = 4
        object lbStatusTrackEventCntLabel: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 3
          Width = 66
          Height = 19
          Margins.Left = 5
          Margins.Top = 2
          Margins.Right = 5
          Margins.Bottom = 2
          Align = alLeft
          Caption = 'Track events:'
          Layout = tlCenter
          ExplicitHeight = 13
        end
        object lbStatusTrackEventCntValue: TLabel
          AlignWithMargins = True
          Left = 78
          Top = 3
          Width = 48
          Height = 19
          Margins.Left = 1
          Margins.Top = 2
          Margins.Right = 5
          Margins.Bottom = 2
          Align = alLeft
          Alignment = taRightJustify
          AutoSize = False
          Caption = '00000000'
          Layout = tlCenter
        end
      end
    end
  end
  object AL: TActionList
    Images = dmShareData.imlMainSmall
    Left = 32
    Top = 288
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
      Caption = 'Debug info'
      OnExecute = acMainTabExecute
    end
    object acTabTimeline: TAction
      Tag = 2
      Category = 'MainTabs'
      Caption = 'Process timeline'
      OnExecute = acMainTabExecute
    end
    object acTabMemoryInfo: TAction
      Tag = 3
      Category = 'MainTabs'
      Caption = 'Memory info'
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
    object acTabCodeTracking: TAction
      Tag = 5
      Category = 'MainTabs'
      Caption = 'Code tracking'
      OnExecute = acMainTabExecute
    end
    object acCodeTracking: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Code tracking'
      OnExecute = acCodeTrackingExecute
    end
    object acTrackSystemUnits: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Track system units'
      OnExecute = acTrackSystemUnitsExecute
    end
    object acParentViewSource: TAction
      Category = 'PopupMenu'
      Caption = 'View source'
      OnExecute = acParentViewSourceExecute
    end
    object acRecentProjects: TAction
      Category = 'Project'
      Caption = 'Recent projects'
      ImageIndex = 1
    end
    object acMemoryInfo: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Memory info'
      Checked = True
      OnExecute = acMemoryInfoExecute
    end
    object acMemInfoCallStack: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'GetMem call stack'
      Checked = True
      OnExecute = acMemInfoCallStackExecute
    end
    object acMemInfoDblFree: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Double FreeMem control'
      Enabled = False
      OnExecute = acMemInfoDblFreeExecute
    end
    object acProcessTimeline: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Process timeline'
      Checked = True
      OnExecute = acProcessTimelineExecute
    end
    object acExceptions: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Exceptions'
      Checked = True
      OnExecute = acExceptionsExecute
    end
    object acExceptionCallStack: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Exception call stack'
      Checked = True
      OnExecute = acExceptionCallStackExecute
    end
    object acCodeTrackRefresh: TAction
      Category = 'CodeTrack'
      Caption = 'Refresh'
      ImageIndex = 17
      OnExecute = acCodeTrackRefreshExecute
    end
    object acCodeTrackHistoryBack: TAction
      Category = 'CodeTrack'
      Caption = 'History'
      ImageIndex = 15
      OnExecute = acCodeTrackHistoryBackExecute
    end
    object acMemInfoRefresh: TAction
      Category = 'MemInfo'
      Caption = 'Refresh'
      ImageIndex = 17
      OnExecute = acMemInfoRefreshExecute
    end
    object acMemInfoHistory: TAction
      Category = 'MemInfo'
      Caption = 'History'
      Enabled = False
      ImageIndex = 15
      OnExecute = acMemInfoHistoryExecute
    end
    object acAddressInfo: TAction
      Category = 'ExceptInfo'
      Caption = 'Address info'
      ImageIndex = 5
      OnExecute = acAddressInfoExecute
    end
    object acTabUpdateInfo: TAction
      Tag = 6
      Category = 'MainTabs'
      Caption = 'Update Info'
      Visible = False
      OnExecute = acMainTabExecute
    end
    object acOpenSite: TAction
      Category = 'Spider'
      Caption = 'http://dbg-spider.net'
      ImageIndex = 18
      OnExecute = acOpenSiteExecute
    end
    object acFeedback: TAction
      Category = 'Spider'
      Caption = 'Add comment'
      ImageIndex = 19
      OnExecute = acFeedbackExecute
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
    Left = 112
    Top = 288
  end
  object tmrThreadsUpdate: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmrThreadsUpdateTimer
    Left = 112
    Top = 232
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
                Action = acOpenSite
                Caption = '&http://dbg-spider.net'
                ImageIndex = 18
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
            Items = <
              item
                Action = acRecent0
                Caption = '&acRecent0'
              end
              item
                Action = acRecent1
                Caption = 'a&cRecent1'
              end
              item
                Action = acRecent2
                Caption = 'ac&Recent2'
              end
              item
                Action = acRecent3
                Caption = 'acR&ecent3'
              end
              item
                Action = acRecent4
                Caption = 'acRece&nt4'
              end
              item
                Action = acRecent5
                Caption = 'acRecen&t5'
              end
              item
                Action = acRecent6
                Caption = 'acRecent&6'
              end
              item
                Action = acRecent7
                Caption = 'acRecent&7'
              end
              item
                Action = acRecent8
                Caption = 'acRecent&8'
              end
              item
                Action = acRecent9
                Caption = 'acRecent&9'
              end>
            Action = acOpenProject
            Caption = '&Open'
            ImageIndex = 1
            CommandProperties.ButtonSize = bsLarge
            CommandProperties.ButtonType = btSplit
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
            Action = acProcessTimeline
            Caption = '&Process timeline'
            CommandStyle = csCheckBox
            CommandProperties.Width = -1
          end
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
            Caption = '&Debug info'
          end
          item
            Caption = '-'
          end
          item
            Action = acTabTimeline
            Caption = '&Process timeline'
          end
          item
            Caption = '-'
          end
          item
            Action = acTabMemoryInfo
            Caption = '&Memory info'
          end
          item
            Caption = '-'
          end
          item
            Action = acTabExceptions
            Caption = '&Exceptions'
          end
          item
            Caption = '-'
          end
          item
            Action = acTabCodeTracking
            Caption = '&Code tracking'
          end
          item
            Caption = '-'
          end
          item
            Visible = False
            Action = acTabUpdateInfo
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
            ShowGlyph = False
            ShowShortCut = False
          end
          item
            Items.HideUnused = False
            Items = <>
            Caption = ' '
          end>
      end
      item
        Items = <
          item
            Action = acUseShortNames
            Caption = '&Use short names'
            CommandStyle = csCheckBox
            CommandProperties.Width = -1
          end>
        ActionBar = rbngrpDbgInfoOptions
      end
      item
        Items = <
          item
            Action = acCodeTracking
            Caption = '&Code tracking'
            CommandStyle = csCheckBox
            CommandProperties.Width = -1
          end
          item
            Action = acTrackSystemUnits
            Caption = '&Track system units'
            CommandStyle = csCheckBox
            CommandProperties.Width = -1
          end>
        ActionBar = rbngrpCodeTracking
      end
      item
        Items = <
          item
            Action = acMemoryInfo
            Caption = '&Memory info'
            CommandStyle = csCheckBox
            CommandProperties.Width = -1
          end
          item
            Action = acMemInfoCallStack
            Caption = '&GetMem call stack'
            CommandStyle = csCheckBox
            CommandProperties.Width = -1
          end
          item
            Action = acMemInfoDblFree
            Caption = '&Double FreeMem control'
            CommandStyle = csCheckBox
            CommandProperties.Width = -1
          end>
        ActionBar = rbngrpMemInfoOptions
      end
      item
        Items = <
          item
            Action = acExceptions
            Caption = '&Exceptions'
            CommandStyle = csCheckBox
            CommandProperties.Width = -1
          end
          item
            Action = acExceptionCallStack
            Caption = 'E&xception call stack'
            CommandStyle = csCheckBox
            CommandProperties.Width = -1
          end>
        ActionBar = rbngrpExceptionOptions
      end
      item
        Items = <
          item
            Items = <
              item
                Action = acFunc0
                Caption = '&acFunc0'
              end
              item
                Action = acFunc1
                Caption = 'a&cFunc1'
              end
              item
                Action = acFunc2
                Caption = 'ac&Func2'
              end
              item
                Action = acFunc3
                Caption = 'acF&unc3'
              end
              item
                Action = acFunc4
                Caption = 'acFu&nc4'
              end
              item
                Action = acFunc5
                Caption = 'acFunc&5'
              end
              item
                Action = acFunc6
                Caption = 'acFunc&6'
              end
              item
                Action = acFunc7
                Caption = 'acFunc&7'
              end
              item
                Action = acFunc8
                Caption = 'acFunc&8'
              end
              item
                Action = acFunc9
                Caption = 'acFunc&9'
              end>
            Action = acCodeTrackHistoryBack
            Caption = '&History'
            ImageIndex = 15
            CommandProperties.ButtonType = btSplit
          end
          item
            Action = acCodeTrackRefresh
            Caption = '&Refresh'
            ImageIndex = 17
          end>
        ActionBar = actbCodeTrackingInfo
      end
      item
        Items = <
          item
            Action = acMemInfoHistory
            Caption = '&History'
            ImageIndex = 15
            CommandProperties.ButtonType = btSplit
          end
          item
            Action = acMemInfoRefresh
            Caption = '&Refresh'
            ImageIndex = 17
          end>
        ActionBar = actbMemInfo
      end
      item
        Items = <
          item
            Action = acAddressInfo
            Caption = '&Address info'
            ImageIndex = 5
          end>
        ActionBar = actbExceptionInfo
      end
      item
        Items = <
          item
            Action = acOpenSite
            Caption = '&http://dbg-spider.net'
            ImageIndex = 18
          end>
        ActionBar = actbUpdateInfo
      end
      item
        Items = <
          item
            Action = acFeedback
            Caption = '&Add comment'
            ImageIndex = 19
            CommandProperties.ButtonSize = bsLarge
          end>
        ActionBar = rbngrpFeedback
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
      end
      item
        ActionList = alCodeTrackHistory
        Caption = 'alCodeTrackHistory'
      end>
    Images = dmShareData.imlMainSmall
    Left = 32
    Top = 232
    StyleName = 'Ribbon - Silver'
  end
  object ALRecent: TActionList
    Left = 112
    Top = 352
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
  object pmTrackFuncAdvParents: TPopupMenu
    Left = 112
    Top = 408
    object Viewsource1: TMenuItem
      Action = acParentViewSource
    end
  end
  object alCodeTrackHistory: TActionList
    Left = 200
    Top = 352
    object acFunc0: TAction
      Caption = 'acFunc0'
      OnExecute = acFuncExecute
    end
    object acFunc1: TAction
      Caption = 'acFunc1'
      OnExecute = acFuncExecute
    end
    object acFunc2: TAction
      Caption = 'acFunc2'
      OnExecute = acFuncExecute
    end
    object acFunc3: TAction
      Caption = 'acFunc3'
      OnExecute = acFuncExecute
    end
    object acFunc4: TAction
      Caption = 'acFunc4'
      OnExecute = acFuncExecute
    end
    object acFunc5: TAction
      Caption = 'acFunc5'
      OnExecute = acFuncExecute
    end
    object acFunc6: TAction
      Caption = 'acFunc6'
      OnExecute = acFuncExecute
    end
    object acFunc7: TAction
      Caption = 'acFunc7'
      OnExecute = acFuncExecute
    end
    object acFunc8: TAction
      Caption = 'acFunc8'
      OnExecute = acFuncExecute
    end
    object acFunc9: TAction
      Caption = 'acFunc9'
      OnExecute = acFuncExecute
    end
  end
end
