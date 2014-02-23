unit uMain;

interface

uses
  Windows, uShareData, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Mask, ExtCtrls, ComCtrls, ActnList, DebugInfo,
  Grids, VirtualTrees, GdiPlus, GdiPlusHelpers,
  Debuger, DebugerTypes, DelphiDebugInfo,
  PlatformDefaultStyleActnCtrls, ActnMan, Ribbon, RibbonLunaStyleActnCtrls,
  RibbonSilverStyleActnCtrls, ToolWin, ActnCtrls, ActnMenus,
  RibbonActnMenus, ImgList, ActnColorMaps, XPMan,
  uActionController, uSpiderOptions, System.Actions,
  Vcl.Menus, uUpdateInfo, uSourceViewFrame;

type
  TProgectType = (ptEmpty, ptSpider, ptApplication);

  TLinkType = (ltNone = 0, ltProject, ltProcess, ltThread, ltMemInfo, ltMemStack, ltExceptInfo, ltExceptStack,
    ltDbgUnitGroup, ltDbgUnitInfo, ltDbgConstInfo, ltDbgTypeInfo, ltDbgVarInfo, ltDbgFuncInfo, ltDbgStructMemberInfo,
    ltDbgFuncParamInfo, ltDbgLogItem, ltTrackFuncInfo, ltTrackUnitInfo, ltTrackCallFuncInfo,
    ltSpiderInfo, ltVersionInfo, ltChangeLogItemInfo, ltSyncObjInfo, ltSyncObjStack);

  PLinkData = ^TLinkData;
  TLinkData = record
    SyncNode: PVirtualNode;
    case LinkType: TLinkType of
      ltNone:
        ();
      ltProject:
        ();
      ltProcess:
        (ProcessData: PProcessData);
      ltThread:
        (ThreadData: PThreadData);
      ltMemInfo:
        (MemPtr: Pointer);
      ltMemStack:
        (MemStackPtr: Pointer);
      ltExceptInfo:
        (ExceptInfo: TExceptInfo);
      ltExceptStack:
        (ExceptStackEntry: TStackEntry);
      ltDbgUnitGroup:
        (DbgUnitGroupType: TUnitType);
      ltDbgUnitInfo:
        (DbgUnitInfo: TUnitInfo);
      ltDbgConstInfo:
        (DbgConstInfo: TConstInfo);
      ltDbgTypeInfo:
        (DbgTypeInfo: TTypeInfo);
      ltDbgVarInfo:
        (DbgVarInfo: TVarInfo);
      ltDbgFuncInfo:
        (DbgFuncInfo: TFuncInfo);
      ltDbgStructMemberInfo:
        (DbgStructMemberInfo: TStructMember);
      ltDbgFuncParamInfo:
        (DbgFuncParamInfo: TVarInfo);
      ltDbgLogItem:
        (DbgLogItem: TDbgLogItem);
      ltTrackFuncInfo:
        (TrackFuncInfo: TTrackFuncInfo);
      ltTrackUnitInfo:
        (TrackUnitInfo: TTrackUnitInfo);
      ltTrackCallFuncInfo:
        (TrackCallFuncInfo: PCallFuncInfo);
      ltSpiderInfo:
        ();
      ltVersionInfo:
        (VersionInfo: TChangeLogVersionInfo);
      ltChangeLogItemInfo:
        (ChangeLogItem: TChangeLogItem);
      ltSyncObjInfo:
        (SyncObjItem: PSyncObjsInfo);
      ltSyncObjStack:
        (SyncObjStackPtr: Pointer);
  end;

  TCheckFunc = function(LinkData: PLinkData; CmpData: Pointer): Boolean;

  TMainForm = class(TForm)
    AL: TActionList;
    acAppOpen: TAction;
    acRun: TAction;
    acStop: TAction;
    acPause: TAction;
    OD: TFileOpenDialog;
    acDebugInfo: TAction;
    pcMain: TPageControl;
    tsLog: TTabSheet;
    tmrThreadsUpdate: TTimer;
    tsThreads1: TTabSheet;
    vstThreads: TVirtualStringTree;
    vdtTimeLine: TVirtualDrawTree;
    acAttachProcess: TAction;
    tsMemInfo: TTabSheet;
    vstMemInfoThreads: TVirtualStringTree;
    tsExceptions: TTabSheet;
    vstExceptionThreads: TVirtualStringTree;
    amMain: TActionManager;
    rbnMain: TRibbon;
    rbpMain: TRibbonPage;
    rbambMain: TRibbonApplicationMenuBar;
    rbgProject: TRibbonGroup;
    rbqtbMain: TRibbonQuickAccessToolbar;
    acNewProject: TAction;
    acOpenProject: TAction;
    acCloseProject: TAction;
    acOptions: TAction;
    acExit: TAction;
    acAbout: TAction;
    rbgApplication: TRibbonGroup;
    rbngrpDebug: TRibbonGroup;
    acSave: TAction;
    acSaveCopy: TAction;
    rbngrpTimeLineSettings: TRibbonGroup;
    acCPUTimeLine: TAction;
    acRealTimeLine: TAction;
    acRunStop: TAction;
    cbMainTabs: TCoolBar;
    actbMainTabs: TActionToolBar;
    acTabDebugInfo: TAction;
    acTabTimeline: TAction;
    acTabMemoryInfo: TAction;
    acTabExceptions: TAction;
    acTabLog: TAction;
    cbStatusInfo: TCoolBar;
    acStatusDebuger: TAction;
    acStatusDbgInfo: TAction;
    acStausEventCount: TAction;
    tsDebugInfo: TTabSheet;
    vstDbgInfoUnits: TVirtualStringTree;
    pDbgInfoDetail: TPanel;
    pcDbgInfoDetail: TPageControl;
    tsDbgUnitConsts: TTabSheet;
    tsDbgUnitTypes: TTabSheet;
    tsDbgUnitVars: TTabSheet;
    tsDbgUnitFunctions: TTabSheet;
    vstDbgInfoConsts: TVirtualStringTree;
    vstDbgInfoTypes: TVirtualStringTree;
    vstDbgInfoVars: TVirtualStringTree;
    vstDbgInfoFunctions: TVirtualStringTree;
    vstLog: TVirtualStringTree;
    ALRecent: TActionList;
    acRecent1: TAction;
    acRecent2: TAction;
    acRecent3: TAction;
    acRecent4: TAction;
    acRecent5: TAction;
    acRecent6: TAction;
    acRecent7: TAction;
    acRecent8: TAction;
    acRecent9: TAction;
    acRecent0: TAction;
    acEditProject: TAction;
    tsDbgUnitSource: TTabSheet;
    pDbgInfoFuncAdv: TPanel;
    vstDbgInfoFuncVars: TVirtualStringTree;
    splDbgInfoFuncAdv: TSplitter;
    acUseShortNames: TAction;
    rbngrpDbgInfoOptions: TRibbonGroup;
    tsCodeTracking: TTabSheet;
    acTabCodeTracking: TAction;
    rbngrpCodeTracking: TRibbonGroup;
    acCodeTracking: TAction;
    acTrackSystemUnits: TAction;
    vstTrackThreads: TVirtualStringTree;
    pmTrackFuncAdvParents: TPopupMenu;
    acParentViewSource: TAction;
    Viewsource1: TMenuItem;
    acRecentProjects: TAction;
    pStatusBar: TPanel;
    pbProgress: TProgressBar;
    lbStatusAction: TLabel;
    pStatusDbgInfo: TPanel;
    pStatusDbgState: TPanel;
    pStatusEventCnt: TPanel;
    lbStatusDbgInfoValue: TLabel;
    lbStatusDbgInfoLabel: TLabel;
    lbStatusDbgStateLabel: TLabel;
    lbStatusDbgStateValue: TLabel;
    lbStateEventCntLabel: TLabel;
    lbStatusEventsCntValue: TLabel;
    pStatusTrackEventCnt: TPanel;
    lbStatusTrackEventCntLabel: TLabel;
    lbStatusTrackEventCntValue: TLabel;
    rbngrpMemInfoOptions: TRibbonGroup;
    acMemoryInfo: TAction;
    acMemInfoCallStack: TAction;
    acMemInfoDblFree: TAction;
    acProcessTimeline: TAction;
    rbngrpExceptionOptions: TRibbonGroup;
    acExceptions: TAction;
    acExceptionCallStack: TAction;
    pCodeTrackingInfo: TPanel;
    pTrackAdv: TPanel;
    vstTrackFuncs: TVirtualStringTree;
    pcTrackFuncAdv: TPageControl;
    tsTrackFuncAdvLinks: TTabSheet;
    pTrackFuncAdv: TPanel;
    splTrackFuncAdv: TSplitter;
    vstTrackFuncParent: TVirtualStringTree;
    vstTrackFuncChilds: TVirtualStringTree;
    tsTrackFuncAdvSrc: TTabSheet;
    cbCodeTrackingInfo: TCoolBar;
    actbCodeTrackingInfo: TActionToolBar;
    acCodeTrackHistoryBack: TAction;
    alCodeTrackHistory: TActionList;
    acFunc1: TAction;
    acFunc2: TAction;
    acFunc3: TAction;
    acFunc4: TAction;
    acFunc5: TAction;
    acFunc6: TAction;
    acFunc7: TAction;
    acFunc8: TAction;
    acFunc9: TAction;
    acFunc0: TAction;
    acCodeTrackRefresh: TAction;
    pMemInfoClient: TPanel;
    cbMemInfo: TCoolBar;
    actbMemInfo: TActionToolBar;
    acMemInfoRefresh: TAction;
    acMemInfoHistory: TAction;
    pcMemInfo: TPageControl;
    tsMemInfoViewStack: TTabSheet;
    pnl1: TPanel;
    pMemoryInfoAdv: TPanel;
    splMemInfoAdv: TSplitter;
    vstMemStack: TVirtualStringTree;
    vstMemList: TVirtualStringTree;
    tsMemInfoTreeView: TTabSheet;
    pMemInfoTreeLeft: TPanel;
    vstMemInfoFuncTree: TVirtualStringTree;
    pcMemInfoFuncInfo: TPageControl;
    tsMemInfoFuncLinks: TTabSheet;
    pMemInfoFuncLinks: TPanel;
    spl1: TSplitter;
    vstMemInfoFuncParents: TVirtualStringTree;
    vstMemInfoFuncChilds: TVirtualStringTree;
    tsMemInfoFuncSrc: TTabSheet;
    spl2: TSplitter;
    pMemInfoButtom: TPanel;
    vstMemInfoObjects: TVirtualStringTree;
    vstMemInfoObjStack: TVirtualStringTree;
    pExceptionInfo: TPanel;
    pnl2: TPanel;
    vstExceptionList: TVirtualStringTree;
    pExceptInfoAdv: TPanel;
    splExceptInfoAdv: TSplitter;
    vstExceptionCallStack: TVirtualStringTree;
    cbExceptionInfo: TCoolBar;
    actbExceptionInfo: TActionToolBar;
    acAddressInfo: TAction;
    acTabUpdateInfo: TAction;
    tsUpdateInfo: TTabSheet;
    vstUpdateInfo: TVirtualStringTree;
    acOpenSite: TAction;
    cbUpdateInfo: TCoolBar;
    actbUpdateInfo: TActionToolBar;
    acFeedback: TAction;
    rbngrpFeedback: TRibbonGroup;
    acContinue: TAction;
    acPauseContinue: TAction;
    acStepInto: TAction;
    acStepOver: TAction;
    acStepOut: TAction;
    acExcepInfoRefresh: TAction;
    tsLockTracking: TTabSheet;
    acTabLockTracking: TAction;
    vstLockThreads: TVirtualStringTree;
    pLockTrackingInfo: TPanel;
    cbLockTracking: TCoolBar;
    actbLockTracking: TActionToolBar;
    pLockTrackingAdv: TPanel;
    vstLockTrackingList: TVirtualStringTree;
    acCopy: TAction;
    svfDbgInfoFuncAdv: TSourceViewFrame;
    svfDbgInfoUnitSource: TSourceViewFrame;
    svfMemInfoSource: TSourceViewFrame;
    svfMemInfoFuncSrc: TSourceViewFrame;
    svfExceptInfoSource: TSourceViewFrame;
    svfTrackFuncAdvSource: TSourceViewFrame;
    pcLockTrackingLinks: TPageControl;
    ts1: TTabSheet;
    pLockTrackingLinks: TPanel;
    spl3: TSplitter;
    vstLockTrackingParents: TVirtualStringTree;
    vstLockTrackingChilds: TVirtualStringTree;
    ts2: TTabSheet;
    svfLockTrackingSource: TSourceViewFrame;
    p2: TPanel;
    vstLockTrackingSyncObjs: TVirtualStringTree;
    vstLockTrackingSyncObjStack: TVirtualStringTree;
    spl4: TSplitter;
    acLockTrackingRefresh: TAction;
    rbngrpLockTracking: TRibbonGroup;
    acLockTracking: TAction;
    acViewSyncObjsOnTimeLine: TAction;
    rbnpgOptions: TRibbonPage;
    acDebugOptions: TAction;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure vstThreadsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstThreadsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstThreadsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure vstThreadsScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
    procedure vstThreadsCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstThreadsExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstThreadsColumnResize(Sender: TVTHeader; Column: TColumnIndex);

    procedure vdtTimeLineDrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
    procedure vdtTimeLineAdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure vdtTimeLineHeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure vdtTimeLineScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
    procedure vdtTimeLineChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vdtTimeLinePaintBackground(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; R: TRect; var Handled: Boolean);

    procedure vstMemInfoThreadsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstMemInfoThreadsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

    procedure vstMemListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstMemListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

    procedure vstMemInfoFuncTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstMemInfoFuncTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstMemInfoFuncTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);

    procedure vstMemInfoFuncLinksGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure vstMemInfoFuncParentsDblClick(Sender: TObject);

    procedure vstMemInfoObjectsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstMemInfoObjectsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstMemInfoObjectsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);

    procedure vstMemStackGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstMemStackFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

    procedure vstExceptionThreadsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstExceptionThreadsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

    procedure vstExceptionListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstExceptionListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

    procedure vstExceptionCallStackGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstExceptionCallStackFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

    procedure vstDbgInfoUnitsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstDbgInfoUnitsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstDbgInfoUnitsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstDbgInfoUnitsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);

    procedure vstDbgInfoConstsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure vstDbgInfoTypesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure vstDbgInfoVarsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure vstDbgInfoFunctionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstDbgInfoFunctionsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

    procedure vstDbgInfoFuncVarsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure vstLogGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstLogDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);

    procedure vstLogResize(Sender: TObject);
    procedure vstLogColumnResize(Sender: TVTHeader; Column: TColumnIndex);

    procedure vstTrackThreadsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstTrackThreadsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstTrackThreadsFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);

    procedure vstTrackFuncsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstTrackFuncsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);

    procedure vstTrackFuncsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstTrackFuncsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstTrackFuncsFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);

    procedure vstTrackFuncParentGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstTrackFuncParentDblClick(Sender: TObject);

    procedure vstTrackFuncChildsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstTrackFuncChildsDblClick(Sender: TObject);

    procedure vstTrackFuncLinksDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);

    procedure vstUpdateInfoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstUpdateInfoDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);

    procedure vstLockThreadsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstLockThreadsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

    procedure vstLockTrackingListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vstLockTrackingListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

    procedure vstLockTrackingLinksGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);

    procedure vstLockTrackingSyncObjsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vstLockTrackingSyncObjsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);


    procedure tmrThreadsUpdateTimer(Sender: TObject);
    procedure cbCPUTimeLineClick(Sender: TObject);

    procedure acAppOpenExecute(Sender: TObject);
    procedure acAttachProcessExecute(Sender: TObject);

    procedure acRunExecute(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure acPauseExecute(Sender: TObject);
    procedure acContinueExecute(Sender: TObject);
    procedure acStepIntoExecute(Sender: TObject);
    procedure acStepOverExecute(Sender: TObject);
    procedure acStepOutExecute(Sender: TObject);

    procedure acOptionsExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acCPUTimeLineExecute(Sender: TObject);
    procedure acRealTimeLineExecute(Sender: TObject);
    procedure acMainTabExecute(Sender: TObject);
    procedure acOpenProjectExecute(Sender: TObject);
    procedure acCloseProjectExecute(Sender: TObject);
    procedure acNewProjectExecute(Sender: TObject);
    procedure acRecentExecute(Sender: TObject);
    procedure acEditProjectExecute(Sender: TObject);
    procedure acSaveCopyExecute(Sender: TObject);
    procedure acUseShortNamesExecute(Sender: TObject);
    procedure acCodeTrackingExecute(Sender: TObject);
    procedure acTrackSystemUnitsExecute(Sender: TObject);
    procedure pTrackFuncAdvResize(Sender: TObject);
    procedure acParentViewSourceExecute(Sender: TObject);
    procedure acMemoryInfoExecute(Sender: TObject);
    procedure acProcessTimelineExecute(Sender: TObject);
    procedure acMemInfoDblFreeExecute(Sender: TObject);
    procedure acMemInfoCallStackExecute(Sender: TObject);
    procedure acExceptionsExecute(Sender: TObject);
    procedure acExceptionCallStackExecute(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure acCodeTrackHistoryBackExecute(Sender: TObject);
    procedure acCodeTrackRefreshExecute(Sender: TObject);
    procedure acFuncExecute(Sender: TObject);
    procedure acMemInfoRefreshExecute(Sender: TObject);
    procedure acMemInfoHistoryExecute(Sender: TObject);
    procedure vstMemInfoFuncChildsDblClick(Sender: TObject);
    procedure pcMemInfoChange(Sender: TObject);
    procedure pMemInfoFuncLinksResize(Sender: TObject);
    procedure acAddressInfoExecute(Sender: TObject);
    procedure vstMemInfoObjStackDblClick(Sender: TObject);
    procedure acOpenSiteExecute(Sender: TObject);
    procedure acFeedbackExecute(Sender: TObject);
    procedure acExcepInfoRefreshExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);

    procedure pLockTrackingLinksResize(Sender: TObject);
    procedure vstLockTrackingSyncObjStackGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstLockTrackingParentsDblClick(Sender: TObject);
    procedure vstLockTrackingChildsDblClick(Sender: TObject);
    procedure vstLockTrackingSyncObjStackDblClick(Sender: TObject);
    procedure vstLockTrackingListCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure vstLockTrackingSyncObjsCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure acLockTrackingRefreshExecute(Sender: TObject);
    procedure acLockTrackingExecute(Sender: TObject);
    procedure acViewSyncObjsOnTimeLineExecute(Sender: TObject);
    procedure acDebugOptionsExecute(Sender: TObject);

  private
    FSpiderOptions: TSpiderOptions;
    FProjectType: TProgectType;
    FTrackHistory: TList;

    FPID: DWORD;

    FCloseApp: Boolean;
    procedure WMClose(var Message: TWMClose); message WM_CLOSE;

    procedure SetProjectName(const ProjectName: String);

    procedure ProgressAction(const Action: String; const Progress: Integer);

    function GetLineTimeOffset: Cardinal;

    procedure HidePCTabs(PC: TPageControl);

    procedure SendGAEvent(const Category, Action: String; const ELabel: String = '');
    procedure SendGAException(E: Exception);
    procedure SendGAFeedback(const FeedbackType, FeedbackText: String);
    procedure StartGASession;
    procedure FinishGASession;

    procedure LoadUpdateInfo;

    procedure ClearProject;
    procedure ClearTrees;
    procedure ClearDbgTrees;
    procedure ClearDbgInfoTrees;
    procedure ClearTrackTrees;
    procedure ClearMemInfoTrees;
    procedure ClearLockInfoTrees;

    procedure UpdateTrees;
    procedure UpdateStatusInfo;
    procedure UpdateDebugActions;
    procedure UpdateLog;
    procedure InitLog(const RootMsg: String);

    procedure UpdateMainActions;

    procedure LoadUnits;
    procedure LoadConsts(UnitInfo: TUnitInfo; UnitNode: PVirtualNode);
    procedure LoadTypes(UnitInfo: TUnitInfo; UnitNode: PVirtualNode);
    procedure LoadVars(UnitInfo: TUnitInfo; UnitNode: PVirtualNode);
    procedure LoadFunctions(UnitInfo: TUnitInfo; UnitNode: PVirtualNode);
    procedure LoadFunctionParams(FuncInfo: TFuncInfo; FuncNode: PVirtualNode);
    function LoadFunctionSource(SrcView: TSourceViewFrame; FuncInfo: TFuncInfo; LineNo: Integer = 0): Boolean; overload;
    procedure LoadUnitSource(UnitInfo: TUnitInfo; UnitNode: PVirtualNode);

    procedure LoadTrackProcessFunctions(ProcData: PProcessData; ThreadNode: PVirtualNode);
    procedure LoadTrackThreadFunctions(ThData: PThreadData; ThreadNode: PVirtualNode);
    procedure LoadTrackParentFunctions(TrackFuncInfo: TTrackFuncInfo; TrackFuncNode: PVirtualNode);
    procedure LoadTrackChildFunctions(TrackFuncInfo: TTrackFuncInfo; TrackFuncNode: PVirtualNode);

    procedure LoadLockTrackThreadFunctions(ThData: PThreadData; ThreadNode: PVirtualNode);

    procedure LoadMemInfoThreadFunctions(ThData: PThreadData; ThreadNode: PVirtualNode);
    procedure LoadMemInfoParentFunctions(Tree: TBaseVirtualTree; TrackFuncInfo: TTrackFuncInfo; TrackFuncNode: PVirtualNode);
    procedure LoadMemInfoChildFunctions(Tree: TBaseVirtualTree; TrackFuncInfo: TTrackFuncInfo; TrackFuncNode: PVirtualNode);
    procedure LoadMemInfoObjects(Tree: TBaseVirtualTree; MemInfo: TGetMemInfoList; SyncNode: PVirtualNode);
    procedure LoadMemInfoObjectStack(Tree: TBaseVirtualTree; MemInfo: TGetMemInfo; SyncNode: PVirtualNode);

    procedure LoadSyncObjsInfoObjects(Tree: TBaseVirtualTree; SyncObjsInfo: TFuncSyncObjsInfoList; SyncNode: PVirtualNode);
    procedure LoadSyncObjsInfoStack(Tree: TBaseVirtualTree; SyncObjInfo: PSyncObjsInfo; SyncNode: PVirtualNode);

    procedure AddTrackHistory(TrackFuncInfo: TTrackFuncInfo);
    procedure UpdateTrackHistoryList;
    procedure ClearTrackHistoryList;

    procedure DrawTimeLineHeaderEx(GP: IGPGraphics; const R: TRect; const Offset: Integer);

    procedure DrawThreadTimeLine(GP: IGPGraphics; const R: TRect; ThData: PThreadData; const CurOffset: Cardinal);
    procedure DrawThreadCPUTimeLine(GP: IGPGraphics; const R: TRect; ThData: PThreadData; const CurOffset: Cardinal);

    procedure DrawProcessTimeLine(GP: IGPGraphics; const R: TRect; ProcData: PProcessData; const CurOffset: Cardinal);
    procedure DrawProcessCPUTimeLine(GP: IGPGraphics; const R: TRect; ProcData: PProcessData; const CurOffset: Cardinal);

    procedure DrawBackgroundEx(GP: IGPGraphics; const R: TRect; const BkColor: TColor);

    procedure AddProcess(const ProcessID: Cardinal);
    function AddProcessToTree(Tree: TBaseVirtualTree): PVirtualNode;
    procedure AddThread(const ThreadID: Cardinal);
    function AddThreadToTree(Tree: TBaseVirtualTree; ThData: PThreadData): PVirtualNode;
    procedure SyncNodes(Tree: TBaseVirtualTree; Node: PVirtualNode);

    function ElapsedToTime(const Elapsed: UInt64): String;
    function FuncElapsedToTime(const FullCPUTime, FullElapsed, Elapsed: UInt64): String; overload;

    function ElapsedTimeToStr(Tree: TBaseVirtualTree; Data: PLinkData; const Elapsed: UInt64): String;

    function ThreadIDToStr(const ThreadID: TThreadId): String;
    function ProcessIDToStr(const ProcessID: TProcessId): String;

    function FindThreadNode(vTree: TBaseVirtualTree; ThData: PThreadData): PVirtualNode;
    function FindThreadNodeById(vTree: TBaseVirtualTree; const ThreadId: TThreadId): PVirtualNode;
    function FindTrackUnitNode(vTree: TBaseVirtualTree; const UnitInfo: TUnitInfo): PVirtualNode;
    function FindTrackFuncNode(vTree: TBaseVirtualTree; const FuncInfo: TFuncInfo): PVirtualNode;


    function FindNode(vTree: TBaseVirtualTree; Node: PVirtualNode; CheckFunc: TCheckFunc; CmpData: Pointer): PVirtualNode;

    procedure LoadGUIOptions;
    procedure LoadRecentProjects;
    function GetDebugOptions: TDbgOptions;
    function GetAppID: String;
    procedure FillUpdateInfo(Sender: TObject);
  public
    procedure OnException(Sender: TObject; E: Exception);
    procedure DoAction(Action: TacAction; const Args: array of Variant);
    procedure ViewDebugInfo(DebugInfo: TDebugInfo);
  end;

var
  MainForm: TMainForm = nil;

implementation

{$R *.dfm}

uses Math, {EvaluateTypes, }ClassUtils, uProcessList, uDebugerThread,
  uProjectOptions, WinAPIUtils, System.UITypes, System.Types,
  uGA, System.Win.Registry, Winapi.ActiveX, Winapi.ShellAPI, uFeedback,
  DbgHookTypes, Collections.Dictionaries;

const
  _TrackingID_web = 'UA-44820931-1';
  _TrackingID_app = 'UA-44820931-2';
  _AppName = 'Spider';

type
  THookBaseVirtualTree = class(TBaseVirtualTree);

procedure TMainForm.acAddressInfoExecute(Sender: TObject);
var
  AddressListStr: String;
  AddressList: TStringList;
  ExceptInfo: TExceptInfo;
  I: Integer;
  StackEntry: TStackEntry;
  AddressStr: String;
  Address: Integer;
begin
  AddressListStr := '';
  if InputQuery('Get address info', 'Address', AddressListStr) then
  begin
    AddressList := TStringList.Create;
    try
      AddressList.DelimitedText := AddressListStr;
      if AddressList.Count > 0 then
      begin
        ExceptInfo := TExceptInfo.Create();
        ExceptInfo.ExceptionName := '### DBG_ADDRESS_INFO';
        ExceptInfo.Message := AddressListStr;

        ExceptInfo.Stack.Capacity := AddressList.Count;
        for I := 0 to AddressList.Count - 1 do
        begin
          AddressStr := AddressList[I];
          if AddressStr <> '' then
          begin
            if AddressStr[1] <> '$' then
              AddressStr := '$' + AddressStr;

            if TryStrToInt(AddressStr, Address) then
            begin
              StackEntry := TStackEntry.Create;
              StackEntry.UpdateInfo(Pointer(Address));

              ExceptInfo.Stack.Add(StackEntry);
            end;
          end;
        end;

        gvDebuger.ProcessData^.DbgExceptions.Add(ExceptInfo);

        vstExceptionThreadsFocusChanged(nil, nil, 0);
      end;
    finally
      FreeAndNil(AddressList);
    end;
  end;
end;

procedure TMainForm.acAppOpenExecute(Sender: TObject);
begin
  if OD.Execute then
    SetProjectName(OD.FileName);
end;

procedure TMainForm.acAttachProcessExecute(Sender: TObject);
var
  F: TfrmProcessList;
begin
  Application.CreateForm(TfrmProcessList, F);
  try
    if F.ShowModal = mrOk then
    begin
      FPID := TProcessId(F.GetSelProcessID);
      SetProjectName(F.GetSelProcessName);
    end;
  finally
    F.Release;
  end;
end;

procedure TMainForm.acCloseProjectExecute(Sender: TObject);
begin
  ClearProject;
end;

procedure TMainForm.acCodeTrackHistoryBackExecute(Sender: TObject);
begin
  vstTrackFuncs.OnFocusChanging := nil;
  try
    acFunc0.Execute;
  finally
    vstTrackFuncs.OnFocusChanging := vstTrackFuncsFocusChanging;
  end;
end;

procedure TMainForm.acCodeTrackingExecute(Sender: TObject);
begin
  if Assigned(gvDebuger) then
  begin
    gvDebuger.CodeTracking := acCodeTracking.Checked;
    acTrackSystemUnits.Enabled := gvDebuger.CodeTracking;
  end;
end;

procedure TMainForm.acCodeTrackRefreshExecute(Sender: TObject);
begin
  vstTrackThreadsFocusChanged(vstTrackThreads, vstTrackThreads.FocusedNode, 0);
end;

procedure TMainForm.acContinueExecute(Sender: TObject);
begin
  _AC.TraceDebug(dtsContinue);
end;

procedure TMainForm.acCopyExecute(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    ActiveControl.Perform(WM_COPY, 0, 0);
end;

procedure TMainForm.acCPUTimeLineExecute(Sender: TObject);
begin
  UpdateTrees;
end;

procedure TMainForm.acDebugOptionsExecute(Sender: TObject);
begin
  rbnMain.TabIndex := rbnpgOptions.Index;
end;

procedure TMainForm.acEditProjectExecute(Sender: TObject);
begin
  OpenProjectOptions(otEdit);
end;

procedure TMainForm.acExcepInfoRefreshExecute(Sender: TObject);
begin
  vstExceptionThreadsFocusChanged(vstExceptionThreads, vstExceptionThreads.FocusedNode, 0);
end;

procedure TMainForm.acExceptionCallStackExecute(Sender: TObject);
begin
  //
end;

procedure TMainForm.acExceptionsExecute(Sender: TObject);
begin
  //
end;

procedure TMainForm.acExitExecute(Sender: TObject);
begin
  FCloseApp := True;
  Close;
end;

procedure TMainForm.acFeedbackExecute(Sender: TObject);
var
  F: TfrmFeedback;
begin
  Application.CreateForm(TfrmFeedback, F);
  try
    if F.ShowModal = mrOk then
    begin
      //SendGAEvent('Feedback', F.FeedbackType, F.FeedbackText);
      SendGAFeedback(F.FeedbackType, F.FeedbackText);
    end;
  finally
    FreeAndNil(F);
  end;
end;

procedure TMainForm.acFuncExecute(Sender: TObject);
var
  Action: TAction;
  TrackFuncInfo: TTrackFuncInfo;
  FuncNode: PVirtualNode;
begin
  if Sender is TAction then
  begin
    Action := TAction(Sender);
    if Action.Tag <> 0 then
    begin
      TrackFuncInfo := TTrackFuncInfo(Action.Tag);

      FTrackHistory.Remove(TrackFuncInfo);
      UpdateTrackHistoryList;

      FuncNode := FindTrackFuncNode(vstTrackFuncs, TFuncInfo(TrackFuncInfo.FuncInfo));
      if Assigned(FuncNode) then
      begin
        vstTrackFuncs.ClearSelection;
        vstTrackFuncs.FocusedNode := FuncNode;
        vstTrackFuncs.Selected[FuncNode] := True;
      end;
    end;
  end;
end;

procedure TMainForm.acLockTrackingExecute(Sender: TObject);
begin
  //
end;

procedure TMainForm.acLockTrackingRefreshExecute(Sender: TObject);
begin
  vstLockThreadsFocusChanged(vstLockThreads, vstLockThreads.FocusedNode, 0);
end;

procedure TMainForm.acOpenProjectExecute(Sender: TObject);
begin
  if OD.Execute then
    SetProjectName(OD.FileName);
end;

procedure TMainForm.acOpenSiteExecute(Sender: TObject);
const
  _SPIDER_URL = 'http://dbg-spider.net';
begin
  ShellExecute(WindowHandle, 'open', _SPIDER_URL, nil, nil, SW_SHOWNORMAL);
end;

procedure TMainForm.acOptionsExecute(Sender: TObject);
begin
  //
end;

procedure TMainForm.acRealTimeLineExecute(Sender: TObject);
begin
  UpdateTrees;
end;

procedure TMainForm.acRecentExecute(Sender: TObject);
var
  PName: String;
begin
  if Sender is TAction then
  begin
    PName := TAction(Sender).Caption;
    UniqueString(PName);
    SetProjectName(PName);
  end;
end;

function TMainForm.GetDebugOptions: TDbgOptions;
begin
  Result := [];

  if acProcessTimeline.Checked then
    Include(Result, doProfiler);

  if acMemoryInfo.Checked then
  begin
    Include(Result, doMemProfiler);

    if acMemInfoCallStack.Checked then
      Include(Result, doMemCallStack);

    if acMemInfoDblFree.Checked then
      Include(Result, doMemCheckDoubleFree);
  end;

  if acExceptions.Checked then
  begin
    Include(Result, doExceptions);

    if acExceptionCallStack.Checked then
      Include(Result, doExceptionCallStack);
  end;

  if acCodeTracking.Checked then
  begin
    Include(Result, doCodeTracking);

    if acTrackSystemUnits.Checked then
      Include(Result, doTrackSystemUnits);
  end;

  if acLockTracking.Checked then
  begin
    Include(Result, doSyncObjsTracking);
  end;
end;

procedure TMainForm.acRunExecute(Sender: TObject);
begin
  SendGAEvent('Run Spider project', GetAppID, 'Run');

  acRun.Enabled := False;

  ClearTrees;

  tmrThreadsUpdate.Enabled := True;

  if Assigned(gvDebuger) then
    gvDebuger.ClearDbgInfo;

  UpdateStatusInfo;
  UpdateMainActions;

  _AC.RunDebug([doRun, doDebugInfo] + GetDebugOptions, FPID);
end;

procedure TMainForm.acSaveCopyExecute(Sender: TObject);
begin
  OpenProjectOptions(otSaveAs);
end;

procedure TMainForm.acStepIntoExecute(Sender: TObject);
begin
  _AC.TraceDebug(dtsStepIn);
end;

procedure TMainForm.acStepOutExecute(Sender: TObject);
begin
  _AC.TraceDebug(dtsStepOut);
end;

procedure TMainForm.acStepOverExecute(Sender: TObject);
begin
  _AC.TraceDebug(dtsStepOver);
end;

procedure TMainForm.acStopExecute(Sender: TObject);
begin
  acStop.Enabled := False;

  _AC.StopDebug;
end;

procedure TMainForm.acProcessTimelineExecute(Sender: TObject);
begin
  //
end;

procedure TMainForm.acTrackSystemUnitsExecute(Sender: TObject);
begin
  if Assigned(gvDebuger) then
  begin
    gvDebuger.CodeTracking := acTrackSystemUnits.Checked;
  end;
end;

procedure TMainForm.acUseShortNamesExecute(Sender: TObject);
begin
  if Assigned(gvDebugInfo) then
  begin
    gvDebugInfo.UseShortNames := acUseShortNames.Checked;
    UpdateTrees;
  end;
end;

procedure TMainForm.acViewSyncObjsOnTimeLineExecute(Sender: TObject);
begin
  UpdateTrees;
end;

procedure TMainForm.acParentViewSourceExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PLinkData;
begin
  Node := vstTrackFuncParent.FocusedNode;
  if Assigned(Node) then
  begin
    Data := vstTrackFuncParent.GetNodeData(Node);
    if Data^.LinkType = ltTrackCallFuncInfo then
      if LoadFunctionSource(svfTrackFuncAdvSource, TFuncInfo(Data^.TrackCallFuncInfo^.FuncInfo), Data^.TrackCallFuncInfo^.LineNo) then
        pcTrackFuncAdv.ActivePage := tsTrackFuncAdvSrc;
  end;
end;

procedure TMainForm.acPauseExecute(Sender: TObject);
begin
  _AC.PauseDebug;
end;

procedure TMainForm.HidePCTabs(PC: TPageControl);
var
  I: Integer;
begin
  for I := 0 to PC.PageCount - 1 do
    PC.Pages[I].TabVisible := False;
end;

procedure TMainForm.InitLog(const RootMsg: String);
var
  Node: PVirtualNode;
  Data: PLinkData;
begin
  vstLog.Clear;

  Node := vstLog.AddChild(nil);
  Data := vstLog.GetNodeData(Node);

  Data^.LinkType := ltProject;
end;

procedure TMainForm.acMainTabExecute(Sender: TObject);
var
  CurTag: Integer;
begin
  CurTag := TAction(Sender).Tag;

  acTabLog.Checked := (CurTag = acTabLog.Tag);
  acTabDebugInfo.Checked := (CurTag = acTabDebugInfo.Tag);
  acTabTimeline.Checked := (CurTag = acTabTimeline.Tag);
  acTabMemoryInfo.Checked := (CurTag = acTabMemoryInfo.Tag);
  acTabExceptions.Checked := (CurTag = acTabExceptions.Tag);
  acTabCodeTracking.Checked := (CurTag = acTabCodeTracking.Tag);
  acTabLockTracking.Checked := (CurTag = acTabLockTracking.Tag);
  acTabUpdateInfo.Checked := (CurTag = acTabUpdateInfo.Tag);

  pcMain.ActivePageIndex := CurTag;

  UpdateTrees;
end;

procedure TMainForm.acMemInfoCallStackExecute(Sender: TObject);
begin
  //
end;

procedure TMainForm.acMemInfoDblFreeExecute(Sender: TObject);
begin
  //
end;

procedure TMainForm.acMemInfoHistoryExecute(Sender: TObject);
begin
  //
end;

procedure TMainForm.acMemInfoRefreshExecute(Sender: TObject);
begin
  vstMemInfoThreadsFocusChanged(vstMemInfoThreads, vstMemInfoThreads.FocusedNode, 0);
end;

procedure TMainForm.acMemoryInfoExecute(Sender: TObject);
begin
  //
end;

procedure TMainForm.acNewProjectExecute(Sender: TObject);
begin
  OpenProjectOptions(otNew);
end;

procedure TMainForm.AddProcess(const ProcessID: Cardinal);
var
  LinkData: PLinkData;
  NameNode: PVirtualNode;
  TimeLineNode: PVirtualNode;
begin
  // Threads timeline
  vstThreads.BeginUpdate;
  vdtTimeLine.BeginUpdate;
  try
    NameNode := AddProcessToTree(vstThreads);
    TimeLineNode := AddProcessToTree(vdtTimeLine);

    LinkData := vstThreads.GetNodeData(NameNode);
    LinkData^.SyncNode := TimeLineNode;

    LinkData := vdtTimeLine.GetNodeData(TimeLineNode);
    LinkData^.SyncNode := NameNode;
  finally
    vstThreads.EndUpdate;
    vdtTimeLine.EndUpdate;
  end;

  // Memory Info
  AddProcessToTree(vstMemInfoThreads);

  // Exceptions
  AddProcessToTree(vstExceptionThreads);

  // Code Tracking
  AddProcessToTree(vstTrackThreads);

  // Lock Tracking
  AddProcessToTree(vstLockThreads);
end;

function TMainForm.AddProcessToTree(Tree: TBaseVirtualTree): PVirtualNode;
var
  LinkData: PLinkData;
begin
  Result := Tree.AddChild(Nil);
  LinkData := Tree.GetNodeData(Result);
  LinkData^.SyncNode := nil;
  LinkData^.ProcessData := gvDebuger.ProcessData;
  LinkData^.LinkType := ltProcess;
end;

function TMainForm.FindNode(vTree: TBaseVirtualTree; Node: PVirtualNode; CheckFunc: TCheckFunc; CmpData: Pointer): PVirtualNode;
var
  CurNode: PVirtualNode;
  LinkData: PLinkData;
begin
  Result := Nil;
  CurNode := Node^.FirstChild;
  if CurNode <> Nil then
  repeat
    LinkData := vTree.GetNodeData(CurNode);
    if CheckFunc(LinkData, CmpData) then
    begin
      Result := CurNode;
      Break;
    end;

    Result := FindNode(vTree, CurNode, CheckFunc, CmpData);
    CurNode := CurNode^.NextSibling;
  until (CurNode = nil) or (Result <> Nil);
end;

function TMainForm.FindThreadNode(vTree: TBaseVirtualTree; ThData: PThreadData): PVirtualNode;

  function _Cmp(LinkData: PLinkData; CmpData: Pointer): Boolean;
  begin
    Result := (LinkData^.LinkType = ltThread) and (LinkData^.ThreadData = CmpData);
  end;

begin
  Result := FindNode(vTree, vTree.RootNode, @_Cmp, ThData);
end;

function TMainForm.FindThreadNodeById(vTree: TBaseVirtualTree; const ThreadId: TThreadId): PVirtualNode;

  function _Cmp(LinkData: PLinkData; CmpData: Pointer): Boolean;
  begin
    Result := (LinkData^.LinkType = ltThread) and (LinkData^.ThreadData^.ThreadID = TThreadId(CmpData));
  end;

begin
  Result := FindNode(vTree, vTree.RootNode, @_Cmp, Pointer(ThreadId));
end;

function TMainForm.FindTrackFuncNode(vTree: TBaseVirtualTree; const FuncInfo: TFuncInfo): PVirtualNode;

  function _Cmp(LinkData: PLinkData; CmpData: Pointer): Boolean;
  begin
    Result := (LinkData^.LinkType = ltTrackFuncInfo) and (LinkData^.TrackFuncInfo.FuncInfo = CmpData);
  end;

begin
  Result := FindNode(vTree, vTree.RootNode, @_Cmp, FuncInfo);
end;

function TMainForm.FindTrackUnitNode(vTree: TBaseVirtualTree; const UnitInfo: TUnitInfo): PVirtualNode;

  function _Cmp(LinkData: PLinkData; CmpData: Pointer): Boolean;
  begin
    case LinkData^.LinkType of
      ltTrackUnitInfo:
        Result := (LinkData^.TrackUnitInfo = CmpData);
      ltDbgUnitInfo:
        Result := (LinkData^.DbgUnitInfo = CmpData);
    else
      Result := False;
    end;
  end;

begin
  Result := FindNode(vTree, vTree.RootNode, @_Cmp, UnitInfo);
end;

procedure TMainForm.FinishGASession;
var
  GA: TGA;
begin
  GA := TGA.Create;
  try
    GA.TrackingID := _TrackingID_app;
    GA.AppName := _AppName;
    GA.ClientID := GetAppID;
    GA.AppVersion := GetFileVersion(Application.ExeName);

    GA.SessionEnd;
  finally
    FreeAndNil(GA);
  end;
end;

procedure TMainForm.AddThread(const ThreadID: Cardinal);
var
  ThData: PThreadData;
  NameNode: PVirtualNode;
  TimeLineNode: PVirtualNode;
  LinkData: PLinkData;
begin
  if gvDebuger = nil then Exit;

  ThData := gvDebuger.GetThreadData(ThreadID);
  if ThData = nil then Exit;

  // Timeline
  vstThreads.BeginUpdate;
  vdtTimeLine.BeginUpdate;
  try
    NameNode := AddThreadToTree(vstThreads, ThData);
    TimeLineNode := AddThreadToTree(vdtTimeLine, ThData);

    LinkData := vstThreads.GetNodeData(NameNode);
    LinkData^.SyncNode := TimeLineNode;

    LinkData := vdtTimeLine.GetNodeData(TimeLineNode);
    LinkData^.SyncNode := NameNode;
  finally
    vstThreads.EndUpdate;
    vdtTimeLine.EndUpdate;
  end;

  // Memory Info
  AddThreadToTree(vstMemInfoThreads, ThData);

  // Exceptions
  AddThreadToTree(vstExceptionThreads, ThData);

  // Code Tracking
  AddThreadToTree(vstTrackThreads, ThData);

  // Lock Tracking
  AddThreadToTree(vstLockThreads, ThData);
end;

function TMainForm.AddThreadToTree(Tree: TBaseVirtualTree; ThData: PThreadData): PVirtualNode;
var
  LinkData: PLinkData;
  ParentThData: PThreadData;
  ParentId: Cardinal;
  ParentNode: PVirtualNode;
  CurNode: PVirtualNode;
begin
  Tree.BeginUpdate;
  CurNode := Tree.FocusedNode;
  try
    ParentNode := Nil;

    ParentId := ThData^.ThreadAdvInfo^.ThreadParentId;

    if ParentId <> 0 then
    begin
      ParentThData := gvDebuger.GetThreadData(ParentId, True);

      if ParentThData <> nil then
        ParentNode := FindThreadNode(Tree, ParentThData);

      (*
      if ParentThData = nil then
        // Если родительский поток завершился раньше старта дочернего
        ParentNode := FindThreadNodeById(Tree, ParentId)
      else
        ParentNode := FindThreadNode(Tree, ParentThData);
      *)
    end;

    if ParentNode = Nil then
      ParentNode := Tree.RootNode^.FirstChild;

    Result := Tree.AddChild(ParentNode);
    Tree.Expanded[ParentNode] := True;

    LinkData := Tree.GetNodeData(Result);
    LinkData^.SyncNode := nil;
    LinkData^.ThreadData := ThData;
    LinkData^.LinkType := ltThread;
  finally
    Tree.FocusedNode := CurNode;
    Tree.EndUpdate;
  end;
end;

procedure TMainForm.AddTrackHistory(TrackFuncInfo: TTrackFuncInfo);
begin
  FTrackHistory.Remove(TrackFuncInfo);
  FTrackHistory.Insert(0, TrackFuncInfo);

  UpdateTrackHistoryList;
end;

procedure TMainForm.cbCPUTimeLineClick(Sender: TObject);
begin
  UpdateTrees;
end;

procedure TMainForm.ClearDbgInfoTrees;
begin
  vstDbgInfoUnits.Clear;
  vstDbgInfoConsts.Clear;
  vstDbgInfoTypes.Clear;
  vstDbgInfoVars.Clear;
  vstDbgInfoFunctions.Clear;
  vstDbgInfoFuncVars.Clear;
  svfDbgInfoUnitSource.Clear;
  svfDbgInfoFuncAdv.Clear;
end;

procedure TMainForm.ClearDbgTrees;
begin
  vstThreads.Clear;
  vdtTimeLine.Clear;

  ClearMemInfoTrees;

  vstExceptionThreads.Clear;
  vstExceptionList.Clear;
  vstExceptionCallStack.Clear;
  svfExceptInfoSource.Clear;
end;

procedure TMainForm.ClearMemInfoTrees;
begin
  vstMemInfoThreads.Clear;

  vstMemList.Clear;
  vstMemStack.Clear;
  svfMemInfoSource.Clear;

  vstMemInfoFuncTree.Clear;
  vstMemInfoObjects.Clear;
  vstMemInfoObjStack.Clear;
  vstMemInfoFuncParents.Clear;
  vstMemInfoFuncChilds.Clear;
  svfMemInfoFuncSrc.Clear;
end;

procedure TMainForm.ClearProject;
begin
  if Assigned(gvDebuger) and gvDebuger.Active then
  begin
    acStop.Execute;

    while gvDebuger.Active do
      Application.ProcessMessages;
  end;

  ClearTrees;

  _AC.ClearDebug(True);

  if gvProjectOptions.ProjectName <> '' then
    FSpiderOptions.AddRecentProject(gvProjectOptions.ProjectName);

  LoadRecentProjects;

  FProjectType := ptEmpty;
  rbnMain.Caption := 'Empty';
  gvProjectOptions.Clear;

  UpdateStatusInfo;
  UpdateMainActions;
end;

procedure TMainForm.ClearTrackHistoryList;
begin
  FTrackHistory.Clear;

  UpdateTrackHistoryList;
end;

procedure TMainForm.ClearTrackTrees;
begin
  vstTrackThreads.Clear;
  vstTrackFuncs.Clear;
  vstTrackFuncParent.Clear;
  vstTrackFuncChilds.Clear;
end;

procedure TMainForm.ClearLockInfoTrees;
begin
  vstLockThreads.Clear;
  vstLockTrackingList.Clear;
  vstLockTrackingParents.Clear;
  vstLockTrackingChilds.Clear;
  vstLockTrackingSyncObjs.Clear;
  vstLockTrackingSyncObjStack.Clear;
  svfLockTrackingSource.Clear;
end;

procedure TMainForm.ClearTrees;
begin
  vstLog.Clear;

  ClearDbgTrees;
  ClearDbgInfoTrees;
  ClearTrackTrees;
  ClearLockInfoTrees;
end;

procedure TMainForm.DoAction(Action: TacAction; const Args: array of Variant);
begin
  if not Visible then Exit;

  case Action of
    acAddThread:
      AddThread(Args[0]);
    acCreateProcess:
      AddProcess(Args[0]);
    acProgress:
      ProgressAction(Args[0], Args[1]);
    acSetProjectName:
      SetProjectName(Args[0]);
    acChangeDbgState:
      UpdateDebugActions;
  end;

  // Включаем таймер, который обновит окна и определит свой статус
  tmrThreadsUpdate.Enabled := True;

  (*
  UpdateStatusInfo;

  tmrThreadsUpdate.Enabled := Assigned(gvDebugInfo) and Assigned(gvDebuger) and gvDebuger.Active;

  if not tmrThreadsUpdate.Enabled then
    UpdateTrees;
  *)
end;

function OffsetToTime(const Offset: Cardinal): String;
var
  Sec: Cardinal;
  Min: Cardinal;
  Hour: Cardinal;
  Day: Cardinal;
begin
  Sec := Offset;
  Min := Sec div 60; Sec := Sec mod 60;
  Result := IntToStr(Sec) + 's ';
  if Min > 0 then
  begin
    Hour := Min div 60; Min := Min mod 60;
    Result := IntToStr(Min) + 'm ' + Result;
    if Hour > 0 then
    begin
      Day := Hour div 24; Hour := Hour mod 24;
      Result := IntToStr(Hour) + 'h ' + Result;
      if Day > 0 then
        Result := IntToStr(Day) + 'd ' + Result;
    end;
  end;
end;

const
  _TicksPerSec = 100;

(*
procedure TMainForm.DrawBackground(TargetCanvas: TCanvas; const R: TRect; BkColor: TColor);
var
  Cnt: Integer;
  X: Integer;
begin
  TargetCanvas.Brush.Color := BkColor;
  TargetCanvas.Brush.Style := bsSolid;

  TargetCanvas.FillRect(R);

  TargetCanvas.Pen.Color := vdtTimeLine.Colors.GridLineColor;
  TargetCanvas.Pen.Style := psDot;
  TargetCanvas.Pen.Mode := pmMergePenNot;

  For Cnt := 0 to ((R.Right - R.Left + 1) div 100) + 1 do
  begin
    X := R.Left + Cnt * 100 + 1;

    TargetCanvas.MoveTo(X, R.Top);
    TargetCanvas.LineTo(X, R.Bottom);
  end;

  TargetCanvas.Pen.Mode := pmCopy;
end;
*)

procedure TMainForm.DrawBackgroundEx(GP: IGPGraphics; const R: TRect; const BkColor: TColor);
const
  DashValues: array [0..1] of Single = (4, 2);
var
  Brush: IGPBrush;
  Pen: IGPPen;

  Cnt: Integer;
  X: Integer;
begin
  Brush := TGPSolidBrush.Create(TGPColor.Create(ColorToRGB(BkColor)));
  Pen := TGPPen.Create(TGPColor.Silver);
  Pen.SetDashPattern(DashValues);

  GP.FillRectangle(Brush, TGPRect.Create(R));

  For Cnt := 0 to ((R.Right - R.Left + 1) div 100) + 1 do
  begin
    X := R.Left + Cnt * 100 + 1;

    GP.DrawLine(Pen, X, R.Top, X, R.Bottom);
  end;
end;

procedure TMainForm.DrawProcessCPUTimeLine(GP: IGPGraphics; const R: TRect; ProcData: PProcessData; const CurOffset: Cardinal);
var
  X1, X2, Y1, Y2: Integer;
  T1, T2: Int64;
  I: Cardinal;
  ProcPoint: PProcessPoint;
begin
  if (ProcData = nil) or (ProcData^.DbgPointsCount = 0) then Exit;

  T1 := 0;
  T2 := ProcData^.DbgPointsCount - 1;

  X1 := R.Left + Integer(T1 - CurOffset);
  X2 := R.Left + Integer(T2 - CurOffset);

  if (X1 < R.Left) and (X2 < R.Left) then Exit;
  if (X1 > R.Right) and (X2 > R.Right) then Exit;

  if X1 < R.Left then X1 := R.Left;
  if X1 > R.Right then X1 := R.Right;

  if X2 < R.Left then X2 := R.Left;
  if X2 > R.Right then X2 := R.Right;

  Y1 := R.Top + 3;
  Y2 := R.Bottom - 3;

  DrawVGradientRect(GP, Rect(X1, Y1, X2, Y2), FSpiderOptions.TimelineColors[ptWait]);

  if ProcData^.DbgPointsCount > 0 then
  begin
    for I := CurOffset to ProcData^.DbgPointsCount - 1 do
    begin
      ProcPoint := ProcData^.DbgPointByIdx(I);
      if ((ProcPoint^.PointType = ptPerfomance) and (ProcPoint^.DeltaTime > 0)) or
        (ProcPoint^.PointType in [ptException, ptThreadInfo, ptTraceInfo])
      then begin
        X1 := R.Left + Integer(I - CurOffset) - 1;
        if (X1 < R.Left) then Continue;
        if (X1 > R.Right) then Break;

        DrawVGradientRect(GP, Rect(X1, Y1, X1, Y2), FSpiderOptions.TimelineColors[ProcPoint^.PointType]);
      end;
    end;
  end;
end;

procedure TMainForm.DrawProcessTimeLine(GP: IGPGraphics; const R: TRect; ProcData: PProcessData; const CurOffset: Cardinal);
var
  X1, X2, Y1, Y2: Integer;
  T1, T2, F: Int64;
  OffsetT1, OffsetT2, Offset: Cardinal;
  IdxL, IdxR, Idx: Cardinal;
  I: Cardinal;
  ProcPoint: PProcessPoint;
begin
  if ProcData = nil then Exit;

  T1 := 0;
  if ProcData^.State <> psActive then
    T2 := ProcData^.Elapsed
  else
    T2 := _QueryPerformanceCounter;

  T2 := T2 - ProcData^.Started;

  Offset := CurOffset * _TicksPerSec;

  F := _QueryPerformanceFrequency; // in 1 sec

  F := F div _TicksPerSec;

  OffsetT1 := T1 div F;
  OffsetT2 := T2 div F;

  X1 := R.Left + Integer(OffsetT1 - Offset);
  X2 := R.Left + Integer(OffsetT2 - Offset);

  if (X1 < R.Left) and (X2 < R.Left) then Exit;
  if (X1 > R.Right) and (X2 > R.Right) then Exit;

  if X1 < R.Left then X1 := R.Left;
  if X1 > R.Right then X1 := R.Right;

  if X2 < R.Left then X2 := R.Left;
  if X2 > R.Right then X2 := R.Right;

  Y1 := R.Top + 3;
  Y2 := R.Bottom - 3;

  DrawVGradientRect(GP, Rect(X1, Y1, X2, Y2), FSpiderOptions.TimelineColors[ptWait]);

  if ProcData^.DbgPointsCount > 0 then
  begin
    IdxL := 0;

    // Ищем начальный индекс для первого видимого события
    if Offset > 0 then
    begin
      IdxR := ProcData^.DbgPointsCount - 1;

      repeat
        Idx := (IdxL + IdxR) div 2;
        ProcPoint := ProcData^.DbgPointByIdx(Idx);

        if (ProcPoint^.FromStart div F) > Offset then
          IdxR := Idx
        else
          IdxL := Idx;
      until IdxR - IdxL <= 1;
    end;

    for I := IdxL to ProcData^.DbgPointsCount - 1 do
    begin
      ProcPoint := ProcData^.DbgPointByIdx(I);
      if ((ProcPoint^.PointType = ptPerfomance) and (ProcPoint^.DeltaTime > 0)) or
        (ProcPoint^.PointType in [ptException, ptThreadInfo, ptTraceInfo])
      then begin
        OffsetT1 := ProcPoint^.FromStart div F;
        X1 := R.Left + Integer(OffsetT1 - Offset) - 1;

        if (X1 < R.Left) then
          Continue;
        if (X1 > R.Right) then
          Break;

        DrawVGradientRect(GP, Rect(X1, Y1, X1, Y2), FSpiderOptions.TimelineColors[ProcPoint^.PointType]);
      end;
    end;
  end;
end;

procedure TMainForm.DrawThreadCPUTimeLine(GP: IGPGraphics; const R: TRect; ThData: PThreadData; const CurOffset: Cardinal);
var
  X1, X2, X3, Y1, Y2: Integer;
  T1, T2, T3: Int64;
  I: Cardinal;

  IdxL, IdxR, Idx: Cardinal;
  ThPoint: PThreadPoint;

  IdxL1, IdxR1: Cardinal;
  SyncObjsInfo: PSyncObjsInfo;

  procedure _DrawSyncObjs(SyncObjsInfo: PSyncObjsInfo);
  var
    XL, XR: Integer;
    TL, TR: Int64;

    DR: TRect;

    SyncObjsInfoL: PSyncObjsInfo;
    SyncObjsInfoR: PSyncObjsInfo;

    SyncObjsColor: TColor;
  begin
    SyncObjsInfoL := nil;
    SyncObjsInfoR := nil;

    case SyncObjsInfo.SyncObjsInfo.SyncObjsStateType of
      sosEnter:
        begin
          SyncObjsInfoL := SyncObjsInfo;
          SyncObjsInfoR := SyncObjsInfo.Link;
        end;
      sosLeave:
        begin
          SyncObjsInfoL := SyncObjsInfo.Link;
          SyncObjsInfoR := SyncObjsInfo;
        end;
    end;

    if Assigned(SyncObjsInfoL) then
    begin
      TL := SyncObjsInfoL.PerfIdx;
      XL := R.Left + Integer(TL - CurOffset) - 1;
    end
    else
      XL := X1;

    if Assigned(SyncObjsInfoR) then
    begin
      TR := SyncObjsInfoR.PerfIdx;
      XR := R.Left + Integer(TR - CurOffset) - 1;
    end
    else
      XR := X2;

    if XL <> XR then
    begin
      SyncObjsColor := FSpiderOptions.SyncObjsColors[SyncObjsInfo^.SyncObjsInfo.SyncObjsType];

      case SyncObjsInfo^.SyncObjsInfo.SyncObjsType of
        soInCriticalSection:
          begin
            DR := Rect(XL, Y1 - 2, XR, Y1);
            DrawHInterval(GP, DR, SyncObjsColor);

            (*
            DR := Rect(XL, Y1, XL, Y2);
            DrawVGradientRect(GP, DR, SyncObjsColor);

            DR := Rect(XR, Y1, XR, Y2);
            DrawVGradientRect(GP, DR, SyncObjsColor);
            *)
          end;
      else
        begin
          DR := Rect(XL, Y1, XR, Y2);
          DrawVGradientRect(GP, DR, SyncObjsColor);
        end;
      end;
    end;
  end;

begin
  if (ThData = nil) or (ThData^.DbgPointsCount = 0) then Exit;

  T1 := ThData^.DbgPointByIdx(0)^.PerfIdx;
  if ThData^.State = tsFinished then
    T2 := ThData^.DbgPointByIdx(ThData^.DbgPointsCount - 1)^.PerfIdx
  else
    T2 := gvDebuger.ProcessData.CurDbgPointIdx;

  X1 := R.Left + Integer(T1 - CurOffset);
  X2 := R.Left + Integer(T2 - CurOffset);

  if (X1 < R.Left) and (X2 < R.Left) then Exit;
  if (X1 > R.Right) and (X2 > R.Right) then Exit;

  if X1 < R.Left then X1 := R.Left;
  if X1 > R.Right then X1 := R.Right;

  if X2 < R.Left then X2 := R.Left;
  if X2 > R.Right then X2 := R.Right;

  Y1 := R.Top + 3;
  Y2 := R.Bottom - 3;

  DrawVGradientRect(GP, Rect(X1, Y1, X2, Y2), FSpiderOptions.TimelineColors[ptWait]);

  if ThData^.DbgPointsCount > 0 then
  begin
    IdxL := 0;

    // Отрисовка SyncObjs
    if acLockTracking.Checked and acViewSyncObjsOnTimeLine.Checked then
    begin
      if ThData^.DbgSyncObjsInfo.Count > 0 then
      begin
        IdxL1 := 0;

        // Поиск первого левого элемента
        if IdxL > 0 then
        begin
          IdxR1 := ThData^.DbgSyncObjsInfo.Count - 1;

          repeat
            Idx := (IdxL1 + IdxR1) div 2;
            SyncObjsInfo := ThData^.DbgSyncObjsByIdx(Idx);

            if SyncObjsInfo^.PerfIdx > IdxL then
              IdxR1 := Idx
            else
            begin
              IdxL1 := Idx;

              if IdxL1 = IdxL then
              begin
                while (IdxL1 > 0) do
                begin
                  SyncObjsInfo := ThData^.DbgSyncObjsByIdx(IdxL1);

                  if SyncObjsInfo^.PerfIdx <> IdxL then
                    Break;

                  Dec(IdxL1);
                end;

                Break;
              end;
            end;
          until (IdxR1 - IdxL1 <= 1);
        end;

        for I := IdxL1 to ThData^.DbgSyncObjsInfo.Count - 1 do
        begin
          SyncObjsInfo := ThData^.DbgSyncObjsByIdx(I);

          T3 := SyncObjsInfo^.PerfIdx;
          X3 := R.Left + Integer(T3 - CurOffset) - 1;

          if (X3 > R.Right) then
            Break;

          _DrawSyncObjs(SyncObjsInfo);
        end;
      end;
    end;

    // Ищем начальный индекс для первого видимого события
    if CurOffset > 0 then
    begin
      IdxR := ThData^.DbgPointsCount - 1;

      repeat
        Idx := (IdxL + IdxR) div 2;
        ThPoint := ThData^.DbgPointByIdx(Idx);

        if ThPoint^.PerfIdx > CurOffset then
          IdxR := Idx
        else
          IdxL := Idx;
      until IdxR - IdxL <= 1;
    end;

    // Отрисовка событий
    for I := IdxL to ThData^.DbgPointsCount - 1 do
    begin
      ThPoint := ThData^.DbgPointByIdx(I);

      if ThPoint = nil then
        Continue;

      T3 := ThPoint^.PerfIdx;
      X3 := R.Left + Integer(T3 - CurOffset) - 1;

      if (X3 < R.Left) then
        Continue;
      if (X3 > R.Right) then
        Break;

      DrawVGradientRect(GP, Rect(X3, Y1, X3, Y2), FSpiderOptions.TimelineColors[ThPoint^.PointType]);
    end;
  end;
end;

procedure TMainForm.DrawThreadTimeLine(GP: IGPGraphics; const R: TRect; ThData: PThreadData; const CurOffset: Cardinal);
var
  X1, X2, X3, Y1, Y2: Integer;
  T1, T2, T3, F: Int64;
  OffsetT1, OffsetT2, Offset: Cardinal;
  IdxL, IdxR, Idx: Cardinal;
  I: Cardinal;
  ThPoint: PThreadPoint;
  ProcPoint: PProcessPoint;

  procedure _DrawSyncObjs(SyncObjsInfo: PSyncObjsInfo);
  var
    XL, XR: Integer;
    TL, TR: Int64;

    DR: TRect;

    SyncObjsInfoL: PSyncObjsInfo;
    SyncObjsInfoR: PSyncObjsInfo;

    SyncObjsColor: TColor;
  begin
    SyncObjsInfoL := nil;
    SyncObjsInfoR := nil;

    case SyncObjsInfo.SyncObjsInfo.SyncObjsStateType of
      sosEnter:
        begin
          SyncObjsInfoL := SyncObjsInfo;
          SyncObjsInfoR := SyncObjsInfo.Link;
        end;
      sosLeave:
        begin
          SyncObjsInfoL := SyncObjsInfo.Link;
          SyncObjsInfoR := SyncObjsInfo;
        end;
    end;

    if Assigned(SyncObjsInfoL) then
    begin
      TL := (SyncObjsInfoL^.SyncObjsInfo.CurTime - ThData^.Started) div F;
      XL := R.Left + Integer(OffsetT1 + TL - Offset);
    end
    else
      XL := X1;

    if Assigned(SyncObjsInfoR) then
    begin
      TR := (SyncObjsInfoR^.SyncObjsInfo.CurTime - ThData^.Started) div F;
      XR := R.Left + Integer(OffsetT1 + TR - Offset);
    end
    else
      XR := X2;

    if XL <> XR then
    begin
      SyncObjsColor := FSpiderOptions.SyncObjsColors[SyncObjsInfo^.SyncObjsInfo.SyncObjsType];

      case SyncObjsInfo^.SyncObjsInfo.SyncObjsType of
        soInCriticalSection:
          begin
            DR := Rect(XL, Y1 - 2, XR, Y1);
            DrawHInterval(GP, DR, SyncObjsColor);

            DR := Rect(XL, Y1, XL, Y2);
            DrawVGradientRect(GP, DR, FSpiderOptions.SyncObjsColors[soEnterCriticalSection]);

            DR := Rect(XR, Y1, XR, Y2);
            DrawVGradientRect(GP, DR, FSpiderOptions.SyncObjsColors[soLeaveCriticalSection]);
          end;
      else
        begin
          DR := Rect(XL, Y1, XR, Y2);
          DrawVGradientRect(GP, DR, SyncObjsColor);
        end;
      end;
    end;
  end;

var
  IdxL1, IdxR1: Cardinal;
  SyncObjsInfo: PSyncObjsInfo;
begin
  if ThData = nil then Exit;

  T1 := ThData^.Started;
  if ThData^.State = tsFinished then
    T2 := T1 + ThData^.Elapsed
  else
    T2 := _QueryPerformanceCounter;

  T1 := T1 - gvDebuger.ProcessData.Started;
  T2 := T2 - gvDebuger.ProcessData.Started;

  Offset := CurOffset * _TicksPerSec;

  F := _QueryPerformanceFrequency; // in 1 sec

  F := F div _TicksPerSec;

  OffsetT1 := T1 div F;
  OffsetT2 := T2 div F;

  X1 := R.Left + Integer(OffsetT1 - Offset);
  X2 := R.Left + Integer(OffsetT2 - Offset);

  if (X1 < R.Left) and (X2 < R.Left) then Exit;
  if (X1 > R.Right) and (X2 > R.Right) then Exit;

  if X1 < R.Left then X1 := R.Left;
  if X1 > R.Right then X1 := R.Right;

  if X2 < R.Left then X2 := R.Left;
  if X2 > R.Right then X2 := R.Right;

  Y1 := R.Top + 3;
  Y2 := R.Bottom - 3;

  DrawVGradientRect(GP, Rect(X1, Y1, X2, Y2), FSpiderOptions.TimelineColors[ptWait]);

  if ThData^.DbgPointsCount > 0 then
  begin
    IdxL := 0;

    // Отрисовка SyncObjs
    if acLockTracking.Checked and acViewSyncObjsOnTimeLine.Checked then
    begin
      if ThData^.DbgSyncObjsInfo.Count > 0 then
      begin
        IdxL1 := 0;

        // Поиск первого левого элемента
        if IdxL > 0 then
        begin
          IdxR1 := ThData^.DbgSyncObjsInfo.Count - 1;

          repeat
            Idx := (IdxL1 + IdxR1) div 2;
            SyncObjsInfo := ThData^.DbgSyncObjsByIdx(Idx);
            ProcPoint := gvDebuger.ProcessData.DbgPointByIdx(SyncObjsInfo^.PerfIdx);

            if (ProcPoint^.FromStart div F) > IdxL then
              IdxR1 := Idx
            else
            begin
              IdxL1 := Idx;

              if IdxL1 = IdxL then
              begin
                while (IdxL1 > 0) do
                begin
                  SyncObjsInfo := ThData^.DbgSyncObjsByIdx(IdxL1);
                  ProcPoint := gvDebuger.ProcessData.DbgPointByIdx(SyncObjsInfo^.PerfIdx);

                  // TODO: optimize
                  if (ProcPoint^.FromStart div F) <> IdxL then
                    Break;

                  Dec(IdxL1);
                end;

                Break;
              end;
            end;
          until (IdxR1 - IdxL1 <= 1);
        end;

        for I := IdxL1 to ThData^.DbgSyncObjsInfo.Count - 1 do
        begin
          SyncObjsInfo := ThData^.DbgSyncObjsByIdx(I);
          ProcPoint := gvDebuger.ProcessData.DbgPointByIdx(SyncObjsInfo^.PerfIdx);

          T3 := ProcPoint^.FromStart div F;
          X3 := R.Left + Integer(T3 - Offset) - 1;

          if (X3 > R.Right) then
            Break;

          _DrawSyncObjs(SyncObjsInfo);
        end;
      end;
    end;

    // Ищем начальный индекс для первого видимого события
    if Offset > 0 then
    begin
      IdxR := ThData^.DbgPointsCount - 1;

      repeat
        Idx := (IdxL + IdxR) div 2;
        ThPoint := ThData^.DbgPointByIdx(Idx);
        ProcPoint := gvDebuger.ProcessData.DbgPointByIdx(ThPoint^.PerfIdx);

        if (ProcPoint^.FromStart div F) > Offset then
          IdxR := Idx
        else
          IdxL := Idx;
      until IdxR - IdxL <= 1;
    end;

    for I := IdxL to ThData^.DbgPointsCount - 1 do
    begin
      ThPoint := ThData^.DbgPointByIdx(I);

      if ThPoint = nil then
        Continue;

      ProcPoint := gvDebuger.ProcessData.DbgPointByIdx(ThPoint^.PerfIdx);

      T3 := ProcPoint^.FromStart div F;
      X3 := R.Left + Integer(T3 - Offset) - 1;

      if (X3 < R.Left) then
        Continue;
      if (X3 > R.Right) then
        Break;

      DrawVGradientRect(GP, Rect(X3, Y1, X3, Y2), FSpiderOptions.TimelineColors[ThPoint^.PointType]);
    end;
  end;
end;

(*
procedure TMainForm.DrawTimeLineHeader(C: TCanvas; const R: TRect; const Offset: Integer);
var
  Cnt: Cardinal;
  X, Y: Integer;
  T: String;
  Idx: Cardinal;
  ProcPoint: PProcessPoint;
begin
  C.Font.Color := clWindowText;
  C.Font.Size := 8;

  C.Brush.Color := clWhite;
  C.Brush.Style := bsClear;

  C.Pen.Color := clWindowText;
  C.Pen.Style := psSolid;

  For Cnt := 0 to ((R.Right - R.Left + 1) div 100) + 1 do
  begin
    X := R.Left + Integer(Cnt) * 100;
    Y := R.Bottom;

    T := '';

    if acCPUTimeLine.Checked then
    begin
      if Assigned(gvDebuger) then
      begin
        Idx := Offset + Integer(Cnt) * 100;

        if Idx < gvDebuger.ProcessData.DbgPointsCount then
        begin
          ProcPoint := gvDebuger.ProcessData.DbgPointByIdx(Idx);
          T := ElapsedToTime(ProcPoint^.CPUTime);
        end;
      end;
    end
    else
      T := OffsetToTime(Cardinal(Offset + Integer(Cnt)));

    if T <> '' then
      C.TextOut(X + 2, R.Top - 3, T);

    C.MoveTo(X - 1, Y - 1);
    C.LineTo(X - 1, Y - 8);

    C.MoveTo(X + 50, Y - 1);
    C.LineTo(X + 50, Y - 4);
  end;
end;
*)

procedure TMainForm.DrawTimeLineHeaderEx(GP: IGPGraphics; const R: TRect; const Offset: Integer);
var
  Font: IGPFont;
  Pen: IGPPen;
  Brush: IGPBrush;

  Cnt: Cardinal;
  X, Y: Integer;
  T: String;
  Idx: Cardinal;
  ProcPoint: PProcessPoint;
begin
  Font := TGPFont.Create('Tahoma', 10, FontStyleRegular, UnitPixel);
  Pen := TGPPen.Create(TGPColor.Black);
  Brush := TGPSolidBrush.Create(TGPColor.Black);

  For Cnt := 0 to ((R.Right - R.Left + 1) div 100) + 1 do
  begin
    X := R.Left + Integer(Cnt) * 100;
    Y := R.Bottom;

    T := '';

    if acCPUTimeLine.Checked then
    begin
      if Assigned(gvDebuger) then
      begin
        Idx := Offset + Integer(Cnt) * 100;

        if Idx < gvDebuger.ProcessData.DbgPointsCount then
        begin
          ProcPoint := gvDebuger.ProcessData.DbgPointByIdx(Idx);
          T := ElapsedToTime(ProcPoint^.CPUTime);
        end;
      end;
    end
    else
      T := OffsetToTime(Cardinal(Offset + Integer(Cnt)));

    if T <> '' then
      GP.DrawString(T, Font, TGPPointF.Create(X + 2, R.Top - 3), Brush);

    GP.DrawLine(Pen, X - 1, Y - 1, X - 1, Y - 8);
    GP.DrawLine(Pen, X + 50, Y - 1, X + 50, Y - 4);
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCloseApp;

  if CanClose then
  begin
    FinishGASession;

    if acStop.Enabled then
    begin
      acStop.Execute;
      while Assigned(gvDebuger) and gvDebuger.Active do
      begin
        Sleep(10);
        Application.ProcessMessages;
      end;
    end;

    acCloseProject.Execute;
  end;
end;

procedure TMainForm.OnException(Sender: TObject; E: Exception);
begin
  SendGAException(E);
end;

procedure TMainForm.pcMainChange(Sender: TObject);
begin
  UpdateTrees;
end;

procedure TMainForm.pcMemInfoChange(Sender: TObject);
begin
  acMemInfoRefresh.Execute;
end;

procedure TMainForm.pLockTrackingLinksResize(Sender: TObject);
begin
  vstLockTrackingParents.Height := pLockTrackingLinks.ClientHeight div 2;
end;

procedure TMainForm.pMemInfoFuncLinksResize(Sender: TObject);
begin
  vstMemInfoFuncParents.Height := pMemInfoFuncLinks.ClientHeight div 2;
end;

procedure TMainForm.LoadRecentProjects;
var
  RL: TStringList;
  I: Integer;
  Item: TOptionItem;
  Action: TContainedAction;
begin
  RL := TStringList.Create;
  try
    FSpiderOptions.GetRecentProjects(RL);

    for I := 0 to ALRecent.ActionCount - 1 do
    begin
      Action := ALRecent.Actions[I];
      if I < RL.Count then
      begin
        TAction(Action).Enabled := True;
        TAction(Action).Visible := True;
        TAction(Action).Caption := RL[I];

        Item := rbambMain.RecentItems[I];
        Item.Caption := Format('&%d: %s', [I, RL[I]]);
      end
      else
      begin
        TAction(Action).Enabled := False;
        TAction(Action).Visible := False;
      end;
    end;
  finally
    FreeAndNil(RL);
  end;
end;

procedure TMainForm.LoadSyncObjsInfoObjects(Tree: TBaseVirtualTree; SyncObjsInfo: TFuncSyncObjsInfoList; SyncNode: PVirtualNode);
var
  SyncObjsItem: PSyncObjsInfo;
  SyncObjsNode: PVirtualNode;
  Data: PLinkData;
  I: Integer;
begin
  if SyncObjsInfo = Nil then Exit;

  Tree.BeginUpdate;
  try
    Tree.Clear;

    SyncObjsInfo.Lock.BeginRead;
    try
      for I := 0 to SyncObjsInfo.Count - 1 do
      begin
        SyncObjsItem := PRPSyncObjsInfo(SyncObjsInfo[I])^.SyncObjsInfo;

        SyncObjsNode := Tree.AddChild(nil);

        Data := Tree.GetNodeData(SyncObjsNode);

        Data^.SyncNode := SyncNode;
        Data^.SyncObjItem := SyncObjsItem; // Адрес в стеке
        Data^.LinkType := ltSyncObjInfo;
      end;
    finally
      SyncObjsInfo.Lock.EndRead;
    end;
  finally
    Tree.EndUpdate;
  end;
end;

procedure TMainForm.LoadSyncObjsInfoStack(Tree: TBaseVirtualTree; SyncObjInfo: PSyncObjsInfo; SyncNode: PVirtualNode);
var
  Idx: Integer;
  StackNode: PVirtualNode;
  StackData: PLinkData;
  Ptr: Pointer;
begin
  Tree.BeginUpdate;
  try
    Tree.Clear;

    for Idx := 0 to High(SyncObjInfo^.SyncObjsInfo.Stack) do
    begin
      Ptr := SyncObjInfo^.SyncObjsInfo.Stack[Idx];
      if Ptr = nil then Break;

      StackNode := Tree.AddChild(nil);
      StackData := Tree.GetNodeData(StackNode);

      StackData^.SyncObjStackPtr := Ptr;
      StackData^.SyncNode := SyncNode;
      StackData^.LinkType := ltSyncObjStack;
    end;
  finally
    Tree.EndUpdate;
  end;
end;

procedure TMainForm.LoadGUIOptions;
begin
  LoadRecentProjects;
end;

procedure TMainForm.LoadLockTrackThreadFunctions(ThData: PThreadData; ThreadNode: PVirtualNode);
var
  Data: PLinkData;

  TrackFuncInfoPair: TTrackFuncInfoPair;
  TrackUnitInfoPair: TTrackUnitInfoPair;

  UnitInfo: TUnitInfo;

  BaseNode: PVirtualNode;
  UnitNode: PVirtualNode;
  Node: PVirtualNode;
begin
  vstLockTrackingList.Clear;

  vstLockTrackingList.BeginUpdate;
  try
    BaseNode := vstLockTrackingList.AddChild(nil);
    Data := vstLockTrackingList.GetNodeData(BaseNode);
    Data^.ThreadData := ThData;
    Data^.SyncNode := ThreadNode;
    Data^.LinkType := ltThread;

    ThData^.DbgSyncObjsUnitList.LockForRead;
    try
      for TrackUnitInfoPair in ThData^.DbgSyncObjsUnitList do
      begin
        UnitInfo := TUnitInfo(TrackUnitInfoPair.Value.UnitInfo);
        if UnitInfo = nil then Continue;

        UnitNode := vstLockTrackingList.AddChild(BaseNode);
        Data := vstLockTrackingList.GetNodeData(UnitNode);

        Data^.SyncNode := ThreadNode;
        Data^.TrackUnitInfo := TrackUnitInfoPair.Value;
        Data^.LinkType := ltTrackUnitInfo;

        TrackUnitInfoPair.Value.FuncInfoList.LockForRead;
        try
          for TrackFuncInfoPair in TrackUnitInfoPair.Value.FuncInfoList do
          begin
            Node := vstLockTrackingList.AddChild(UnitNode);
            Data := vstLockTrackingList.GetNodeData(Node);

            Data^.SyncNode := ThreadNode;
            Data^.TrackFuncInfo := TrackFuncInfoPair.Value;
            Data^.LinkType := ltTrackFuncInfo;
          end;
        finally
          TrackUnitInfoPair.Value.FuncInfoList.UnLockForRead;
        end;

        vstLockTrackingList.Expanded[UnitNode] := True;
      end;
    finally
      ThData^.DbgSyncObjsUnitList.UnLockForRead;
    end;

    vstLockTrackingList.Expanded[BaseNode] := True;
  finally
    vstLockTrackingList.EndUpdate;
  end;
end;

procedure TMainForm.LoadMemInfoChildFunctions(Tree: TBaseVirtualTree; TrackFuncInfo: TTrackFuncInfo; TrackFuncNode: PVirtualNode);
var
  Data: PLinkData;
  FuncInfo: TFuncInfo;
  BaseNode: PVirtualNode;
  UnitNode: PVirtualNode;
  UnitData: PLinkData;
  Node: PVirtualNode;
  CallFuncCounterPair: TCallFuncCounterPair;
begin
  Tree.Clear;

  if TrackFuncInfo.ChildFuncs.Count = 0 then Exit;

  Tree.BeginUpdate;
  try
    BaseNode := Tree.AddChild(nil);
    Data := Tree.GetNodeData(BaseNode);
    Data^.TrackFuncInfo := TrackFuncInfo;
    Data^.SyncNode := TrackFuncNode;
    Data^.LinkType := ltTrackFuncInfo;

    TrackFuncInfo.ChildFuncs.LockForRead;
    try
      for CallFuncCounterPair in TrackFuncInfo.ChildFuncs do
      begin
        FuncInfo := TFuncInfo(CallFuncCounterPair.Value^.FuncInfo);
        if FuncInfo = nil then Continue;

        UnitNode := FindTrackUnitNode(Tree, FuncInfo.UnitInfo);

        if UnitNode = nil then
        begin
          UnitNode := Tree.AddChild(BaseNode);
          UnitData := Tree.GetNodeData(UnitNode);

          UnitData^.DbgUnitInfo := FuncInfo.UnitInfo;
          UnitData^.SyncNode := TrackFuncNode;
          UnitData^.LinkType := ltDbgUnitInfo;
        end;

        Node := Tree.AddChild(UnitNode);
        Data := Tree.GetNodeData(Node);

        Data^.TrackCallFuncInfo := CallFuncCounterPair.Value;
        Data^.SyncNode := TrackFuncNode;
        Data^.LinkType := ltTrackCallFuncInfo;

        Tree.Expanded[UnitNode] := True;
      end;
    finally
      TrackFuncInfo.ChildFuncs.UnLockForRead;
    end;

    Tree.Expanded[BaseNode] := True;
  finally
    Tree.EndUpdate;
  end;
end;

procedure TMainForm.LoadMemInfoObjects(Tree: TBaseVirtualTree; MemInfo: TGetMemInfoList; SyncNode: PVirtualNode);
var
  MItem: TGetMemInfoItem;
  MemNode: PVirtualNode;
  Data: PLinkData;
begin
  if MemInfo = Nil then Exit;

  Tree.BeginUpdate;
  try
    Tree.Clear;

    MemInfo.Lock.BeginRead;
    try
      for MItem in MemInfo do
      begin
        MemNode := Tree.AddChild(nil);
        Data := Tree.GetNodeData(MemNode);
        Data^.SyncNode := SyncNode;
        Data^.MemPtr := MItem.Key;
        Data^.LinkType := ltMemInfo;
      end;
    finally
      MemInfo.Lock.EndRead;
    end;
  finally
    Tree.EndUpdate;
  end;
end;

procedure TMainForm.LoadMemInfoObjectStack(Tree: TBaseVirtualTree; MemInfo: TGetMemInfo; SyncNode: PVirtualNode);
var
  Idx: Integer;
  StackNode: PVirtualNode;
  StackData: PLinkData;
  Ptr: Pointer;
begin
  Tree.BeginUpdate;
  try
    Tree.Clear;

    for Idx := 0 to High(MemInfo.Stack) do
    begin
      Ptr := MemInfo.Stack[Idx];
      if Ptr = nil then Break;

      StackNode := Tree.AddChild(nil);
      StackData := Tree.GetNodeData(StackNode);

      StackData^.MemStackPtr := Ptr;
      StackData^.SyncNode := SyncNode;
      StackData^.LinkType := ltMemStack;
    end;
  finally
    Tree.EndUpdate;
  end;
end;

procedure TMainForm.LoadMemInfoParentFunctions(Tree: TBaseVirtualTree; TrackFuncInfo: TTrackFuncInfo; TrackFuncNode: PVirtualNode);
var
  Data: PLinkData;
  FuncInfo: TFuncInfo;
  BaseNode: PVirtualNode;
  UnitNode: PVirtualNode;
  Node: PVirtualNode;
  CallFuncCounterPair: TCallFuncCounterPair;
begin
  Tree.Clear;

  if TrackFuncInfo.ParentFuncs.Count = 0 then Exit;

  Tree.BeginUpdate;
  try
    BaseNode := Tree.AddChild(nil);
    Data := Tree.GetNodeData(BaseNode);
    Data^.TrackFuncInfo := TrackFuncInfo;
    Data^.SyncNode := TrackFuncNode;
    Data^.LinkType := ltTrackFuncInfo;

    TrackFuncInfo.ParentFuncs.LockForRead;
    try
      for CallFuncCounterPair in TrackFuncInfo.ParentFuncs do
      begin
        FuncInfo := TFuncInfo(CallFuncCounterPair.Value^.FuncInfo);
        if FuncInfo = nil then Continue;

        UnitNode := FindTrackUnitNode(Tree, FuncInfo.UnitInfo);

        if UnitNode = nil then
        begin
          UnitNode := Tree.AddChild(BaseNode);
          Data := Tree.GetNodeData(UnitNode);

          Data^.DbgUnitInfo := FuncInfo.UnitInfo;
          Data^.LinkType := ltDbgUnitInfo;
        end;

        Node := Tree.AddChild(UnitNode);
        Data := Tree.GetNodeData(Node);

        Data^.TrackCallFuncInfo := CallFuncCounterPair.Value;
        Data^.SyncNode := TrackFuncNode;
        Data^.LinkType := ltTrackCallFuncInfo;

        Tree.Expanded[UnitNode] := True;
      end;
    finally
      TrackFuncInfo.ParentFuncs.UnLockForRead;
    end;

    Tree.Expanded[BaseNode] := True;
  finally
    Tree.EndUpdate;
  end;
end;

procedure TMainForm.LoadMemInfoThreadFunctions(ThData: PThreadData; ThreadNode: PVirtualNode);
var
  Th: TThread;
begin
  vstMemInfoFuncTree.Clear;
  vstMemInfoFuncParents.Clear;
  vstMemInfoFuncChilds.Clear;

  Th := TThread.CreateAnonymousThread(
    procedure
    begin
      ThData^.UpdateGetMemUnitList;

      TThread.Synchronize(nil,
        procedure
        var
          Data: PLinkData;

          TrackFuncInfoPair: TTrackFuncInfoPair;
          TrackUnitInfoPair: TTrackUnitInfoPair;

          UnitInfo: TUnitInfo;

          BaseNode: PVirtualNode;
          UnitNode: PVirtualNode;
          Node: PVirtualNode;
        begin
          vstMemInfoFuncTree.Clear;
          vstMemInfoFuncParents.Clear;
          vstMemInfoFuncChilds.Clear;

          vstMemInfoFuncTree.BeginUpdate;
          try
            BaseNode := vstMemInfoFuncTree.AddChild(nil);
            Data := vstMemInfoFuncTree.GetNodeData(BaseNode);
            Data^.ThreadData := ThData;
            Data^.SyncNode := ThreadNode;
            Data^.LinkType := ltThread;

            ThData^.DbgGetMemUnitList.LockForRead;
            try
              for TrackUnitInfoPair in ThData^.DbgGetMemUnitList do
              begin
                UnitInfo := TUnitInfo(TrackUnitInfoPair.Value.UnitInfo);
                if UnitInfo = nil then Continue;

                UnitNode := vstMemInfoFuncTree.AddChild(BaseNode);
                Data := vstMemInfoFuncTree.GetNodeData(UnitNode);

                Data^.SyncNode := ThreadNode;
                Data^.TrackUnitInfo := TrackUnitInfoPair.Value;
                Data^.LinkType := ltTrackUnitInfo;

                for TrackFuncInfoPair in TrackUnitInfoPair.Value.FuncInfoList do
                begin
                  Node := vstMemInfoFuncTree.AddChild(UnitNode);
                  Data := vstMemInfoFuncTree.GetNodeData(Node);

                  Data^.SyncNode := ThreadNode;
                  Data^.TrackFuncInfo := TrackFuncInfoPair.Value;
                  Data^.LinkType := ltTrackFuncInfo;
                end;

                vstMemInfoFuncTree.Expanded[UnitNode] := True;
              end;
            finally
              ThData^.DbgGetMemUnitList.UnLockForRead;
            end;

            vstMemInfoFuncTree.Expanded[BaseNode] := True;
          finally
            vstMemInfoFuncTree.EndUpdate;
          end;
        end
      );
    end
  );
  Th.Suspended := False;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnException := OnException;

  FSpiderOptions := TSpiderOptions.Create(ChangeFileExt(Application.ExeName, '.xcfg'));
  LoadGUIOptions;

  FCloseApp := False;
  FProjectType := ptEmpty;

  FTrackHistory := TList.Create;

  TThread.NameThreadForDebugging(AnsiString(ClassName), MainThreadID);

  actbMainTabs.ParentBackground := True;

  HidePCTabs(pcMain);
  acTabLog.Execute;

  ProgressAction('', 0);

  //LoadLibrary('DbgHook32.dll'); // Для дебага этой самой DLL
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSpiderOptions);
  FreeAndNil(FTrackHistory);

  _AC.AppClose;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  StartGASession;
  SendGAEvent('Run Spider', GetAppID, 'Start');

  acRunStop.Assign(acRun);
  UpdateMainActions;
  UpdateStatusInfo;

  vstThreadsColumnResize(vstThreads.Header, 0);
  vstThreadsColumnResize(vstMemInfoThreads.Header, 0);

  LoadUpdateInfo;
end;

function TMainForm.FuncElapsedToTime(const FullCPUTime, FullElapsed, Elapsed: UInt64): String;
var
  FTime: UInt64;
begin
  Result := ' ';

  if (Elapsed = 0) or (FullElapsed = 0) then
    Exit;

  FTime := Trunc(FullCPUTime * (Elapsed / FullElapsed));

  if FTime > 0 then
    Result := ElapsedToTime(FTime)
  else
    Result := '00:00.000';
end;

function TMainForm.GetLineTimeOffset: Cardinal;
var
  F: Double;
  PW: Integer;
begin
  Result := 0;
  if Assigned(gvDebugInfo) and Assigned(gvDebuger) and
    (gvDebuger.ProcessData.DbgPointsCount > 0)
  then
  begin
    PW := vdtTimeLine.Header.Columns[0].Width - vdtTimeLine.ClientWidth;
    F := (-vdtTimeLine.OffsetX) / PW;
    if acCPUTimeLine.Checked then
      Result := (Trunc(gvDebuger.ProcessData.CurDbgPointIdx * F) div 100) * 100
    else
      Result := Trunc((gvDebuger.ProcessData.Elapsed_MSec div 1000) * F);
  end;
end;

procedure TMainForm.LoadConsts(UnitInfo: TUnitInfo; UnitNode: PVirtualNode);
var
  I: Integer;
  C: TConstInfo;
  BaseNode: PVirtualNode;
  Node: PVirtualNode;
  Data: PLinkData;
begin
  vstDbgInfoConsts.Clear;

  if UnitInfo.Consts.Count = 0 then Exit;

  vstDbgInfoConsts.BeginUpdate;
  try
    BaseNode := vstDbgInfoConsts.AddChild(nil);
    Data := vstDbgInfoConsts.GetNodeData(BaseNode);

    Data^.SyncNode := UnitNode;
    Data^.DbgUnitInfo := UnitInfo;
    Data^.LinkType := ltDbgUnitInfo;

    for I := 0 to UnitInfo.Consts.Count - 1 do
    begin
      C := TConstInfo(UnitInfo.Consts[I]);

      Node := vstDbgInfoConsts.AddChild(BaseNode);
      Data := vstDbgInfoConsts.GetNodeData(Node);

      Data^.SyncNode := UnitNode;
      Data^.DbgConstInfo := C;
      Data^.LinkType := ltDbgConstInfo;
    end;

    vstDbgInfoConsts.Expanded[BaseNode] := True;
  finally
    vstDbgInfoConsts.EndUpdate;
  end;
end;

procedure TMainForm.LoadFunctionParams(FuncInfo: TFuncInfo; FuncNode: PVirtualNode);
var
  I: Integer;
  V: TVarInfo;
  BaseNode: PVirtualNode;
  Node: PVirtualNode;
  Data: PLinkData;
begin
  vstDbgInfoFuncVars.Clear;

  if FuncInfo.Params.Count = 0 then Exit;

  vstDbgInfoFuncVars.BeginUpdate;
  try
    BaseNode := vstDbgInfoFuncVars.AddChild(nil);
    Data := vstDbgInfoFuncVars.GetNodeData(BaseNode);

    Data^.SyncNode := FuncNode;
    Data^.DbgFuncInfo := FuncInfo;
    Data^.LinkType := ltDbgFuncInfo;

    for I := 0 to FuncInfo.Params.Count - 1 do
    begin
      V := TVarInfo(FuncInfo.Params[I]);

      Node := vstDbgInfoFuncVars.AddChild(BaseNode);
      Data := vstDbgInfoFuncVars.GetNodeData(Node);

      Data^.SyncNode := FuncNode;
      Data^.DbgFuncParamInfo := V;
      Data^.LinkType := ltDbgFuncParamInfo;
    end;

    vstDbgInfoFuncVars.Expanded[BaseNode] := True;
  finally
    vstDbgInfoFuncVars.EndUpdate;
  end;
end;

procedure TMainForm.LoadFunctions(UnitInfo: TUnitInfo; UnitNode: PVirtualNode);
var
  I: Integer;
  F: TFuncInfo;
  BaseNode: PVirtualNode;
  Node: PVirtualNode;
  Data: PLinkData;
begin
  vstDbgInfoFuncVars.Clear;
  vstDbgInfoFunctions.Clear;
  svfDbgInfoFuncAdv.Clear;

  if UnitInfo.Funcs.Count = 0 then Exit;

  vstDbgInfoFunctions.BeginUpdate;
  try
    BaseNode := vstDbgInfoFunctions.AddChild(nil);
    Data := vstDbgInfoFunctions.GetNodeData(BaseNode);

    Data^.SyncNode := UnitNode;
    Data^.DbgUnitInfo := UnitInfo;
    Data^.LinkType := ltDbgUnitInfo;

    for I := 0 to UnitInfo.Funcs.Count - 1 do
    begin
      F := TFuncInfo(UnitInfo.Funcs[I]);

      Node := vstDbgInfoFunctions.AddChild(BaseNode);
      Data := vstDbgInfoFunctions.GetNodeData(Node);

      Data^.SyncNode := UnitNode;
      Data^.DbgFuncInfo := F;
      Data^.LinkType := ltDbgFuncInfo;
    end;

    vstDbgInfoFunctions.Expanded[BaseNode] := True;
  finally
    vstDbgInfoFunctions.EndUpdate;
  end;
end;

function TMainForm.LoadFunctionSource(SrcView: TSourceViewFrame; FuncInfo: TFuncInfo; LineNo: Integer): Boolean;
var
  UnitInfo: TUnitInfo;
  StartLine: TLineInfo;
  LineIdx: Integer;
  PrevLine: TLineInfo;
begin
  Result := False;

  if not Assigned(FuncInfo) then
  begin
    SrcView.Clear;
    Exit;
  end;

  UnitInfo := FuncInfo.UnitInfo;

  SrcView.BeginUpdate;
  try
    SrcView.SourceFileName := UnitInfo.FullUnitName;

    if LineNo = 0 then
    begin
      if FuncInfo.Lines.Count > 0 then
      begin
        StartLine := FuncInfo.Lines[0];
        LineIdx := UnitInfo.Lines.IndexOf(StartLine) - 1;
        if LineIdx >= 0 then
        begin
          PrevLine := UnitInfo.Lines[LineIdx];
          LineNo := PrevLine.LineNo + 1;
        end
        else
          LineNo := StartLine.LineNo - 2;

        if Abs(StartLine.LineNo - LineNo) < 10 then
          SrcView.GotoLine(LineNo, taAlignTop)
        else
          SrcView.GotoLine(LineNo, taVerticalCenter);

        SrcView.SelectLine(StartLine.LineNo);
      end;
    end
    else
    begin
      SrcView.GotoLine(LineNo, taVerticalCenter);

      SrcView.SelectLine(LineNo);
    end;

    Result := True;
  finally
    SrcView.EndUpdate;
  end;
end;

procedure TMainForm.LoadTrackChildFunctions(TrackFuncInfo: TTrackFuncInfo; TrackFuncNode: PVirtualNode);
var
  Data: PLinkData;
  FuncInfo: TFuncInfo;
  BaseNode: PVirtualNode;
  UnitNode: PVirtualNode;
  UnitData: PLinkData;
  Node: PVirtualNode;
  CallFuncCounterPair: TCallFuncCounterPair;
begin
  vstTrackFuncChilds.Clear;

  if TrackFuncInfo.ChildFuncs.Count = 0 then Exit;

  vstTrackFuncChilds.BeginUpdate;
  try
    BaseNode := vstTrackFuncChilds.AddChild(nil);
    Data := vstTrackFuncChilds.GetNodeData(BaseNode);
    Data^.TrackFuncInfo := TrackFuncInfo;
    Data^.SyncNode := TrackFuncNode;
    Data^.LinkType := ltTrackFuncInfo;

    for CallFuncCounterPair in TrackFuncInfo.ChildFuncs do
    begin
      FuncInfo := TFuncInfo(CallFuncCounterPair.Value^.FuncInfo);
      if FuncInfo = nil then Continue;

      UnitNode := FindTrackUnitNode(vstTrackFuncChilds, FuncInfo.UnitInfo);

      if UnitNode = nil then
      begin
        UnitNode := vstTrackFuncChilds.AddChild(BaseNode);
        UnitData := vstTrackFuncChilds.GetNodeData(UnitNode);

        UnitData^.DbgUnitInfo := FuncInfo.UnitInfo;
        UnitData^.SyncNode := TrackFuncNode;
        UnitData^.LinkType := ltDbgUnitInfo;
      end;

      Node := vstTrackFuncChilds.AddChild(UnitNode);
      Data := vstTrackFuncChilds.GetNodeData(Node);

      Data^.TrackCallFuncInfo := CallFuncCounterPair.Value;
      Data^.SyncNode := TrackFuncNode;
      Data^.LinkType := ltTrackCallFuncInfo;

      vstTrackFuncChilds.Expanded[UnitNode] := True;
    end;

    vstTrackFuncChilds.Expanded[BaseNode] := True;
  finally
    vstTrackFuncChilds.EndUpdate;
  end;
end;

procedure TMainForm.LoadTrackParentFunctions(TrackFuncInfo: TTrackFuncInfo; TrackFuncNode: PVirtualNode);
var
  Data: PLinkData;
  FuncInfo: TFuncInfo;
  BaseNode: PVirtualNode;
  UnitNode: PVirtualNode;
  Node: PVirtualNode;
  CallFuncCounterPair: TCallFuncCounterPair;
begin
  vstTrackFuncParent.Clear;

  if TrackFuncInfo.ParentFuncs.Count = 0 then Exit;

  vstTrackFuncParent.BeginUpdate;
  try
    BaseNode := vstTrackFuncParent.AddChild(nil);
    Data := vstTrackFuncParent.GetNodeData(BaseNode);
    Data^.TrackFuncInfo := TrackFuncInfo;
    Data^.SyncNode := TrackFuncNode;
    Data^.LinkType := ltTrackFuncInfo;

    for CallFuncCounterPair in TrackFuncInfo.ParentFuncs do
    begin
      FuncInfo := TFuncInfo(CallFuncCounterPair.Value^.FuncInfo);
      if FuncInfo = nil then Continue;

      UnitNode := FindTrackUnitNode(vstTrackFuncParent, FuncInfo.UnitInfo);

      if UnitNode = nil then
      begin
        UnitNode := vstTrackFuncParent.AddChild(BaseNode);
        Data := vstTrackFuncParent.GetNodeData(UnitNode);

        Data^.DbgUnitInfo := FuncInfo.UnitInfo;
        Data^.LinkType := ltDbgUnitInfo;
      end;

      Node := vstTrackFuncParent.AddChild(UnitNode);
      Data := vstTrackFuncParent.GetNodeData(Node);

      Data^.TrackCallFuncInfo := CallFuncCounterPair.Value;
      Data^.SyncNode := TrackFuncNode;
      Data^.LinkType := ltTrackCallFuncInfo;

      vstTrackFuncParent.Expanded[UnitNode] := True;
    end;

    vstTrackFuncParent.Expanded[BaseNode] := True;
  finally
    vstTrackFuncParent.EndUpdate;
  end;
end;

procedure TMainForm.LoadTrackProcessFunctions(ProcData: PProcessData; ThreadNode: PVirtualNode);
var
  Data: PLinkData;

  TrackFuncInfoPair: TTrackFuncInfoPair;
  TrackUnitInfoPair: TTrackUnitInfoPair;

  UnitInfo: TUnitInfo;

  BaseNode: PVirtualNode;
  UnitNode: PVirtualNode;
  Node: PVirtualNode;
begin
  vstTrackFuncs.Clear;
  vstTrackFuncParent.Clear;
  vstTrackFuncChilds.Clear;

  vstTrackFuncs.BeginUpdate;
  try
    BaseNode := vstTrackFuncs.AddChild(nil);
    Data := vstTrackFuncs.GetNodeData(BaseNode);
    Data^.ProcessData := ProcData;
    Data^.SyncNode := ThreadNode;
    Data^.LinkType := ltProcess;

    for TrackUnitInfoPair in ProcData^.DbgTrackUnitList do
    begin
      UnitInfo := TUnitInfo(TrackUnitInfoPair.Value.UnitInfo);
      if UnitInfo = nil then Continue;

      UnitNode := vstTrackFuncs.AddChild(BaseNode);
      Data := vstTrackFuncs.GetNodeData(UnitNode);

      Data^.SyncNode := ThreadNode;
      Data^.TrackUnitInfo := TrackUnitInfoPair.Value;
      Data^.LinkType := ltTrackUnitInfo;

      for TrackFuncInfoPair in TrackUnitInfoPair.Value.FuncInfoList do
      begin
        Node := vstTrackFuncs.AddChild(UnitNode);
        Data := vstTrackFuncs.GetNodeData(Node);

        Data^.SyncNode := ThreadNode;
        Data^.TrackFuncInfo := TrackFuncInfoPair.Value;
        Data^.LinkType := ltTrackFuncInfo;
      end;

      vstTrackFuncs.Expanded[UnitNode] := True;
    end;

    vstTrackFuncs.Expanded[BaseNode] := True;
  finally
    vstTrackFuncs.EndUpdate;
  end;
end;

procedure TMainForm.LoadTrackThreadFunctions(ThData: PThreadData; ThreadNode: PVirtualNode);
var
  Data: PLinkData;

  TrackFuncInfoPair: TTrackFuncInfoPair;
  TrackUnitInfoPair: TTrackUnitInfoPair;

  UnitInfo: TUnitInfo;

  BaseNode: PVirtualNode;
  UnitNode: PVirtualNode;
  Node: PVirtualNode;
begin
  vstTrackFuncs.Clear;
  vstTrackFuncParent.Clear;
  vstTrackFuncChilds.Clear;

  vstTrackFuncs.BeginUpdate;
  try
    BaseNode := vstTrackFuncs.AddChild(nil);
    Data := vstTrackFuncs.GetNodeData(BaseNode);
    Data^.ThreadData := ThData;
    Data^.SyncNode := ThreadNode;
    Data^.LinkType := ltThread;

    ThData^.DbgTrackUnitList.LockForRead;
    for TrackUnitInfoPair in ThData^.DbgTrackUnitList do
    begin
      UnitInfo := TUnitInfo(TrackUnitInfoPair.Value.UnitInfo);
      if UnitInfo = nil then Continue;

      UnitNode := vstTrackFuncs.AddChild(BaseNode);
      Data := vstTrackFuncs.GetNodeData(UnitNode);

      Data^.SyncNode := ThreadNode;
      Data^.TrackUnitInfo := TrackUnitInfoPair.Value;
      Data^.LinkType := ltTrackUnitInfo;

      TrackUnitInfoPair.Value.FuncInfoList.LockForRead;
      for TrackFuncInfoPair in TrackUnitInfoPair.Value.FuncInfoList do
      begin
        Node := vstTrackFuncs.AddChild(UnitNode);
        Data := vstTrackFuncs.GetNodeData(Node);

        Data^.SyncNode := ThreadNode;
        Data^.TrackFuncInfo := TrackFuncInfoPair.Value;
        Data^.LinkType := ltTrackFuncInfo;
      end;
      TrackUnitInfoPair.Value.FuncInfoList.UnLockForRead;

      vstTrackFuncs.Expanded[UnitNode] := True;
    end;
    ThData^.DbgTrackUnitList.UnLockForRead;

    vstTrackFuncs.Expanded[BaseNode] := True;
  finally
    vstTrackFuncs.EndUpdate;
  end;
end;

procedure TMainForm.LoadTypes(UnitInfo: TUnitInfo; UnitNode: PVirtualNode);
var
  I, J: Integer;
  BaseNode: PVirtualNode;

  T: TTypeInfo;
  Node: PVirtualNode;
  Data: PLinkData;

  ChildNode: PVirtualNode;
  ChildData: PLinkData;
  Member: TStructMember;
begin
  vstDbgInfoTypes.Clear;

  if UnitInfo.Types.Count = 0 then Exit;

  vstDbgInfoTypes.BeginUpdate;
  try
    BaseNode := vstDbgInfoTypes.AddChild(nil);
    Data := vstDbgInfoTypes.GetNodeData(BaseNode);

    Data^.SyncNode := UnitNode;
    Data^.DbgUnitInfo := UnitInfo;
    Data^.LinkType := ltDbgUnitInfo;

    for I := 0 to UnitInfo.Types.Count - 1 do
    begin
      T := TTypeInfo(UnitInfo.Types[I]);

      Node := vstDbgInfoTypes.AddChild(BaseNode);
      Data := vstDbgInfoTypes.GetNodeData(Node);

      Data^.SyncNode := UnitNode;
      Data^.DbgTypeInfo := T;
      Data^.LinkType := ltDbgTypeInfo;

      if Assigned(T.Members) then
      begin
        for J := 0 to T.Members.Count - 1 do
        begin
          Member := TStructMember(T.Members[J]);

          ChildNode := vstDbgInfoTypes.AddChild(Node);
          ChildData := vstDbgInfoTypes.GetNodeData(ChildNode);

          ChildData^.SyncNode := UnitNode;
          ChildData^.DbgStructMemberInfo := Member;
          ChildData^.LinkType := ltDbgStructMemberInfo;
        end;
      end;
    end;

    vstDbgInfoTypes.Expanded[BaseNode] := True;
  finally
    vstDbgInfoTypes.EndUpdate;
  end;
end;

procedure TMainForm.LoadUnits;
var
  I: Integer;
  UnitGroupType: TUnitType;
  UnitGroupNodes: array[Low(TUnitType)..High(TUnitType)] of PVirtualNode;
  UnitNode: PVirtualNode;
  UnitGroupNode: PVirtualNode;
  LinkData: PLinkData;
  UnitInfo: TUnitInfo;
begin
  vstDbgInfoUnits.Clear;

  vstDbgInfoUnits.BeginUpdate;
  try
    for UnitGroupType := Low(TUnitType) to High(TUnitType) do
    begin
      UnitGroupNode := vstDbgInfoUnits.AddChild(nil);
      UnitGroupNode.CheckType := ctTriStateCheckBox;

      LinkData := vstDbgInfoUnits.GetNodeData(UnitGroupNode);
      LinkData^.DbgUnitGroupType := UnitGroupType;
      LinkData^.LinkType := ltDbgUnitGroup;

      UnitGroupNodes[UnitGroupType] := UnitGroupNode;
    end;

    for I := 0 to gvDebugInfo.Units.Count - 1 do
    begin
      UnitInfo := TUnitInfo(gvDebugInfo.Units.Objects[I]);

      UnitGroupNode := UnitGroupNodes[UnitInfo.UnitType];

      UnitNode := vstDbgInfoUnits.AddChild(UnitGroupNode);
      UnitNode.CheckType := ctTriStateCheckBox;

      LinkData := vstDbgInfoUnits.GetNodeData(UnitNode);

      LinkData^.DbgUnitInfo := UnitInfo;
      LinkData^.LinkType := ltDbgUnitInfo;
    end;

    for UnitGroupType := Low(TUnitType) to High(TUnitType) do
    begin
      vstDbgInfoUnits.Expanded[UnitGroupNodes[UnitGroupType]] := True;
    end;
  finally
    vstDbgInfoUnits.EndUpdate;
  end;
end;

procedure TMainForm.LoadUnitSource(UnitInfo: TUnitInfo; UnitNode: PVirtualNode);
begin
  svfDbgInfoUnitSource.SourceFileName := UnitInfo.FullUnitName;
end;

procedure TMainForm.FillUpdateInfo(Sender: TObject);
var
  AllVersions: TStringList;
  I, J: Integer;
  Ver: String;
  Info: TChangeLogVersionInfo;
  Data: PLinkData;
  RootNode: PVirtualNode;
  VerNode: PVirtualNode;
  ItemNode: PVirtualNode;
begin
  vstUpdateInfo.Clear;

  if gvUpdateInfo.LastVersion = gvUpdateInfo.CurrentVersion then Exit;

  AllVersions := TStringList.Create;
  try
    if gvUpdateInfo.GetAllVersions(AllVersions) then
    begin
      vstUpdateInfo.BeginUpdate;
      try
        RootNode := vstUpdateInfo.AddChild(nil);
        Data := vstUpdateInfo.GetNodeData(RootNode);

        Data^.LinkType := ltSpiderInfo;

        for I := 0 to AllVersions.Count - 1 do
        begin
          Ver := AllVersions[I];

          Info := TChangeLogVersionInfo.Create(False);
          if gvUpdateInfo.GetVersionInfo(Ver, Info) then
          begin
            VerNode := vstUpdateInfo.AddChild(RootNode);
            Data := vstUpdateInfo.GetNodeData(VerNode);

            Data^.LinkType := ltVersionInfo;
            Data^.VersionInfo := Info;

            for J := 0 to Info.Count - 1 do
            begin
              ItemNode := vstUpdateInfo.AddChild(VerNode);
              Data := vstUpdateInfo.GetNodeData(ItemNode);

              Data^.LinkType := ltChangeLogItemInfo;
              Data^.ChangeLogItem := Info[J];
            end;

            vstUpdateInfo.Expanded[VerNode] := True;
          end;
        end;

        vstUpdateInfo.Expanded[RootNode] := True;
      finally
        vstUpdateInfo.EndUpdate;
      end;

      acTabUpdateInfo.Caption := Format('New version: %s', [gvUpdateInfo.LastVersion]);
      acTabUpdateInfo.Visible := True;
    end;
  finally
    FreeAndNil(AllVersions);
  end;
end;

procedure TMainForm.LoadUpdateInfo;

var
  Th: TThread;
begin
  Th := TThread.CreateAnonymousThread(
    procedure
    begin
      CoInitializeEx(nil, COINIT_MULTITHREADED);
      try
        gvUpdateInfo.Load;
      finally
        CoUninitialize;
      end;
    end
  );
  Th.OnTerminate := FillUpdateInfo;
  Th.Start;
end;

procedure TMainForm.LoadVars(UnitInfo: TUnitInfo; UnitNode: PVirtualNode);
var
  I: Integer;
  V: TVarInfo;
  BaseNode: PVirtualNode;
  Node: PVirtualNode;
  Data: PLinkData;
begin
  vstDbgInfoVars.Clear;

  if UnitInfo.Vars.Count = 0 then Exit;

  vstDbgInfoVars.BeginUpdate;
  try
    BaseNode := vstDbgInfoVars.AddChild(nil);
    Data := vstDbgInfoVars.GetNodeData(BaseNode);

    Data^.SyncNode := UnitNode;
    Data^.DbgUnitInfo := UnitInfo;
    Data^.LinkType := ltDbgUnitInfo;

    for I := 0 to UnitInfo.Vars.Count - 1 do
    begin
      V := TVarInfo(UnitInfo.Vars[I]);

      Node := vstDbgInfoVars.AddChild(BaseNode);
      Data := vstDbgInfoVars.GetNodeData(Node);

      Data^.SyncNode := UnitNode;
      Data^.DbgVarInfo := V;
      Data^.LinkType := ltDbgVarInfo;
    end;

    vstDbgInfoVars.Expanded[BaseNode] := True;
  finally
    vstDbgInfoVars.EndUpdate;
  end;
end;

function TMainForm.ProcessIDToStr(const ProcessID: TProcessId): String;
begin
  Result := Format('%d(%x)', [ProcessID, ProcessID]);
end;

procedure TMainForm.ProgressAction(const Action: String; const Progress: Integer);
begin
  pbProgress.Visible := (Progress > 0);
  pbProgress.Position := Progress;

  if Action <> '' then
    lbStatusAction.Caption := Action
  else
    lbStatusAction.Caption := '';
end;

procedure TMainForm.pTrackFuncAdvResize(Sender: TObject);
begin
  vstTrackFuncParent.Height := pTrackFuncAdv.ClientHeight div 2;
end;

var
  _AppID: String = '';

function TMainForm.GetAppID: String;
const
  _RegKey = 'Software\Spider\';
  _RegAppID = 'AppID';
var
  Reg: TRegistry;
begin
  Result := _AppID;

  if Result <> '' then Exit;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(_RegKey, not Reg.KeyExists(_RegKey)) then
    try
      _AppID := Reg.GetDataAsString(_RegAppID);
      if _AppID = '' then
      begin
        _AppID := GetGUID;
        Reg.WriteString(_RegAppID, _AppID);
      end;
    finally
      Reg.CloseKey;
    end
    else
      _AppID := GetGUID;
  finally
    FreeAndNil(Reg);
  end;

  Result := _AppID;
end;

procedure TMainForm.SendGAEvent(const Category, Action, ELabel: String);
var
  GA: TGA;
begin
  GA := TGA.Create;
  try
    GA.TrackingID := _TrackingID_app;
    GA.AppName := _AppName;
    GA.ClientID := GetAppID;
    GA.AppVersion := GetFileVersion(Application.ExeName);

    GA.SendEvent(Category, Action, ELabel);
  finally
    FreeAndNil(GA);
  end;
end;

procedure TMainForm.SendGAException(E: Exception);
var
  StackTrace: String;
  Msg: String;
begin
  if Assigned(E) then
  begin
    Msg := E.Message;

    StackTrace := E.StackTrace;
    if StackTrace <> '' then
      Msg := Msg + Format(' [%s]', [StackTrace]);

    _AC.Log(dltError, '%s: %s', [E.ClassName, Msg]);

    SendGAEvent('Exception', E.ClassName, Msg);
  end;
end;

procedure TMainForm.SendGAFeedback(const FeedbackType, FeedbackText: String);
var
  GA: TGA;
begin
  GA := TGA.Create;
  try
    GA.TrackingID := _TrackingID_app;
    GA.AppName := _AppName;
    GA.ClientID := GetAppID;
    GA.AppVersion := GetFileVersion(Application.ExeName);

    GA.SendEvent(FeedbackType, FeedbackText, GetAppID);
  finally
    FreeAndNil(GA);
  end;
end;

procedure TMainForm.SetProjectName(const ProjectName: String);
var
  Ext: String;
begin
  if AnsiSameText(gvProjectOptions.ProjectName, ProjectName) then
    Exit;

  ClearProject;

  Ext := ExtractFileExt(ProjectName);
  if AnsiSameText(Ext, '.spider') then
    FProjectType := ptSpider
  else
  if AnsiSameText(Ext, '.exe') then
    FProjectType := ptApplication
  else
    Exit;

  rbnMain.Caption := ProjectName;
  InitLog(ProjectName);

  case FProjectType of
    ptSpider:
        gvProjectOptions.Open(ProjectName);
    ptApplication:
      begin
        gvProjectOptions.Open(_DEFAULT_PROJECT);
        gvProjectOptions.ApplicationName := ProjectName;
        gvProjectOptions.ProjectSource := TProjectOptions.GetDefProjectSource(ProjectName);
        gvProjectOptions.DelphiSource := TProjectOptions.GetDefDelphiSource;
      end;
  end;

  _AC.RunDebug([doDebugInfo]);

  UpdateMainActions;
end;

procedure TMainForm.StartGASession;
var
  GA: TGA;
begin
  GA := TGA.Create;
  try
    GA.TrackingID := _TrackingID_app;
    GA.AppName := _AppName;
    GA.ClientID := GetAppID;
    GA.AppVersion := GetFileVersion(Application.ExeName);

    GA.SessionStart;
  finally
    FreeAndNil(GA);
  end;
end;

procedure TMainForm.SyncNodes(Tree: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PLinkData;
  OtherTree: TBaseVirtualTree;
begin
  if (Tree = Nil) or (Node = Nil) then
    Exit;

  Data := Tree.GetNodeData(Node);
  if Tree = vstThreads then
    OtherTree := vdtTimeLine
  else
    OtherTree := vstThreads;

  OtherTree.Expanded[Data.SyncNode] := Tree.Expanded[Node];
  OtherTree.Selected[Data.SyncNode] := Tree.Selected[Node];

  if Tree.FocusedNode = Node then
    OtherTree.FocusedNode := Data.SyncNode;
end;

function TMainForm.ThreadIDToStr(const ThreadID: TThreadId): String;
begin
  Result := Format('%d(%x)', [ThreadID, ThreadID]);
end;

procedure TMainForm.tmrThreadsUpdateTimer(Sender: TObject);
begin
  UpdateDebugActions;
  UpdateStatusInfo;
  UpdateTrees;

  tmrThreadsUpdate.Enabled := (gvDebugInfo <> nil) and (gvDebuger <> nil) and gvDebuger.Active;
end;

procedure TMainForm.UpdateMainActions;
begin
  if FProjectType = ptEmpty then
  begin
    acRun.Enabled := False;
    acStop.Enabled := False;
    acRunStop.Enabled := False;
    acPause.Enabled := False;

    acCloseProject.Enabled := False;
    acEditProject.Enabled := False;
    acSave.Enabled := False;
    acSaveCopy.Enabled := False;
  end
  else
  begin
    acCloseProject.Enabled := True;
    acSave.Enabled := True;
    acSaveCopy.Enabled := True;
    acEditProject.Enabled := True;
  end;
end;

procedure TMainForm.UpdateDebugActions;
var
  DebugInfoLoaded: Boolean;
  DebugerStoped: Boolean;
  DebugePaused: Boolean;
begin
  DebugInfoLoaded := Assigned(gvDebugInfo) and gvDebugInfo.DebugInfoLoaded;
  DebugerStoped := (gvDebuger = Nil) or not gvDebuger.Active;

  acRun.Enabled := DebugInfoLoaded and DebugerStoped;
  acStop.Enabled := not DebugerStoped;

  if acRun.Enabled then
    acRunStop.Assign(acRun)
  else
  if acStop.Enabled then
    acRunStop.Assign(acStop)
  else
  begin
    acRunStop.Assign(acPause);
    acRunStop.Enabled := False;
  end;

  DebugePaused := Assigned(gvDebuger) and (gvDebuger.DbgTraceState = dtsPause);
  acPause.Enabled := acStop.Enabled and not(DebugePaused);
  acContinue.Enabled := DebugePaused;

  if acPause.Enabled then
    acPauseContinue.Assign(acPause)
  else
  if acContinue.Enabled then
    acPauseContinue.Assign(acContinue)
  else
  begin
    acPauseContinue.Assign(acPause);
    acPauseContinue.Enabled := False;
  end;

  acStepInto.Enabled := acContinue.Enabled;
  acStepOver.Enabled := acContinue.Enabled;
  acStepOut.Enabled := acContinue.Enabled;
end;

procedure TMainForm.UpdateLog;
var
  CurCount: Integer;
  LogCount: Integer;
  I: Integer;
  Node: PVirtualNode;
  Data: PLinkData;
begin
  if gvDebugInfo = nil then Exit;

  if vstLog.RootNode.FirstChild = nil then
    InitLog(gvProjectOptions.ApplicationName);

  CurCount := vstLog.RootNode.FirstChild.ChildCount;

  gvDebugInfo.DbgLog.Lock.BeginRead;
  try
    LogCount := gvDebugInfo.DbgLog.Count;
    if CurCount <> LogCount then
    begin
      vstLog.BeginUpdate;
      try
        for I := CurCount to LogCount - 1 do
        begin
          Node := vstLog.AddChild(vstLog.RootNode.FirstChild);
          Data := vstLog.GetNodeData(Node);

          Data^.DbgLogItem := gvDebugInfo.DbgLog[I];
          Data^.LinkType := ltDbgLogItem;
        end;

        vstLog.Expanded[vstLog.RootNode.FirstChild] := True;
      finally
        vstLog.EndUpdate;
      end;
    end;
  finally
    gvDebugInfo.DbgLog.Lock.EndRead;
  end;
end;

procedure TMainForm.UpdateStatusInfo;
var
  Msg: String;
begin
  if Assigned(gvDebuger) then
  begin
    if Assigned(gvDebugInfo) and (gvDebugInfo.DebugInfoLoaded) then
      lbStatusDbgInfoValue.Caption := gvDebugInfo.DebugInfoType
    else
      lbStatusDbgInfoValue.Caption := 'Not found';

    case gvDebuger.DbgState of
      dsNone: Msg := 'none';
      dsStarted: Msg := 'Started';
      dsWait: Msg := 'Active';
      dsPerfomance: Msg := 'Active';
      dsTrace: Msg := 'Trace';
      dsEvent: Msg := 'Active';
      dsPause: Msg := 'Pause';
      dsStoping: Msg := 'Stoping';
      dsStoped: Msg := 'Stoped';
      dsDbgFail: Msg := 'Debug Fail';
      else
        Msg := '';
    end;
    lbStatusDbgStateValue.Caption := Msg;

    if gvDebuger.PerfomanceMode and not(gvDebuger.DbgState in [dsNone]) and Assigned(gvDebuger.ProcessData.DbgPoints) then
      lbStatusEventsCntValue.Caption := IntToStr(gvDebuger.ProcessData.DbgPoints.Count)
    else
      lbStatusEventsCntValue.Caption := '0';

    lbStatusTrackEventCntValue.Caption := IntToStr(gvDebuger.ProcessData.DbgTrackEventCount);
  end
  else
  begin
    lbStatusDbgInfoValue.Caption := 'None';
    lbStatusDbgStateValue.Caption := 'None';
    lbStatusEventsCntValue.Caption := '0';
    lbStatusTrackEventCntValue.Caption := '0';
  end;
end;

procedure TMainForm.UpdateTrackHistoryList;
var
  I: Integer;
  Action: TContainedAction;
  TrackFuncInfo: TTrackFuncInfo;
  F: Boolean;
begin
  F := False;
  for I := 0 to alCodeTrackHistory.ActionCount - 1 do
  begin
    Action := alCodeTrackHistory.Actions[I];
    if I < FTrackHistory.Count then
    begin
      TrackFuncInfo := TTrackFuncInfo(FTrackHistory[I]);

      TAction(Action).Caption := TFuncInfo(TrackFuncInfo.FuncInfo).ShortName;
      TAction(Action).Tag := NativeInt(TrackFuncInfo);

      TAction(Action).Enabled := True;
      TAction(Action).Visible := True;

      F := True;
    end
    else
    begin
      TAction(Action).Enabled := False;
      TAction(Action).Visible := False;
    end;
  end;

  acCodeTrackHistoryBack.Enabled := F;
end;

procedure TMainForm.UpdateTrees;
begin
  case pcMain.ActivePageIndex of
    0: begin
      UpdateLog;
    end;
    1: begin
      vstDbgInfoUnits.Invalidate;
      vstDbgInfoConsts.Invalidate;
      vstDbgInfoTypes.Invalidate;
      vstDbgInfoVars.Invalidate;
      vstDbgInfoFunctions.Invalidate;
      vstDbgInfoFuncVars.Invalidate;
    end;
    2: begin
      vstThreads.Invalidate;
      vdtTimeLine.Invalidate;
      vdtTimeLine.Header.Invalidate(nil);
    end;
    3: begin
      vstMemInfoThreads.Invalidate;
      vstMemList.Invalidate;
      vstMemStack.Invalidate;

      vstMemInfoFuncTree.Invalidate;
      vstMemInfoObjects.Invalidate;
      vstMemInfoFuncParents.Invalidate;
      vstMemInfoFuncChilds.Invalidate;
    end;
    4: begin
      vstExceptionThreads.Invalidate;
      vstExceptionList.Invalidate;
      vstExceptionCallStack.Invalidate;
    end;
    5: begin
      vstTrackThreads.Invalidate;
      vstTrackFuncs.Invalidate;
      vstTrackFuncParent.Invalidate;
      vstTrackFuncChilds.Invalidate;

      UpdateTrackHistoryList;
    end;
    6: begin
      vstLockThreads.Invalidate;
      vstLockTrackingList.Invalidate;
      vstLockTrackingParents.Invalidate;
      vstLockTrackingChilds.Invalidate;
      vstLockTrackingSyncObjs.Invalidate;
    end;
  end;
end;

procedure TMainForm.vdtTimeLineAdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
begin
  if (PaintInfo.Column <> Nil) and (PaintInfo.Column.Index = 0)then
    DrawTimeLineHeaderEx(PaintInfo.TargetCanvas.ToGPGraphics, PaintInfo.PaintRectangle, GetLineTimeOffset);
end;

procedure TMainForm.vdtTimeLineChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  SyncNodes(Sender, Node);
end;

procedure TMainForm.vdtTimeLineDrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
var
  GP: IGPGraphics;
  LinkData: PLinkData;
  R: TRect;
begin
  GP := PaintInfo.Canvas.ToGPGraphics;

  R := PaintInfo.CellRect;
  R.Width := vdtTimeLine.ClientWidth;

  DrawBackgroundEx(GP, R, PaintInfo.Canvas.Brush.Color);

  LinkData := Sender.GetNodeData(PaintInfo.Node);
  case LinkData^.LinkType of
    ltProcess:
    begin
      if acCPUTimeLine.Checked then
        DrawProcessCPUTimeLine(GP, R, LinkData^.ProcessData, GetLineTimeOffset)
      else
        DrawProcessTimeLine(GP, R, LinkData^.ProcessData, GetLineTimeOffset);
    end;
    ltThread:
    begin
      if acCPUTimeLine.Checked then
        DrawThreadCPUTimeLine(GP, R, LinkData^.ThreadData, GetLineTimeOffset)
      else
        DrawThreadTimeLine(GP, R, LinkData^.ThreadData, GetLineTimeOffset);
    end;
  end;
end;

procedure TMainForm.vdtTimeLineHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  Include(Elements, hpeText);
end;

procedure TMainForm.vdtTimeLinePaintBackground(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; R: TRect; var Handled: Boolean);
begin
  R.Width := vdtTimeLine.ClientWidth;

  DrawBackgroundEx(TargetCanvas.ToGPGraphics, R, vdtTimeLine.Canvas.Brush.Color);

  Handled := True;
end;

procedure TMainForm.vdtTimeLineScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
begin
  if DeltaY <> 0 then
    vstThreads.OffsetY := vdtTimeLine.OffsetY;

  if DeltaX <> 0 then
    vdtTimeLine.Invalidate;
end;

procedure TMainForm.ViewDebugInfo(DebugInfo: TDebugInfo);
begin
  gvDebugInfo := DebugInfo;

  LoadUnits;
end;

procedure TMainForm.vstLockTrackingSyncObjStackDblClick(Sender: TObject);
var
  Data: PLinkData;
  StackEntry: TStackEntry;
begin
  svfLockTrackingSource.Clear;

  if vstLockTrackingSyncObjStack.FocusedNode = Nil then
    Exit;

  Data := vstLockTrackingSyncObjStack.GetNodeData(vstLockTrackingSyncObjStack.FocusedNode);
  if Data^.LinkType = ltSyncObjStack then
  begin
    StackEntry := TStackEntry.Create;
    try
      StackEntry.UpdateInfo(Data^.SyncObjStackPtr);

      if Assigned(StackEntry.FuncInfo) then
      begin
        if Assigned(StackEntry.LineInfo) then
          LoadFunctionSource(svfLockTrackingSource, StackEntry.FuncInfo, StackEntry.LineInfo.LineNo)
        else
          LoadFunctionSource(svfLockTrackingSource, StackEntry.FuncInfo);

        pcLockTrackingLinks.ActivePageIndex := 1;
      end;
    finally
      FreeAndNil(StackEntry);
    end;
  end;
end;

procedure TMainForm.vstLockTrackingSyncObjStackGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PLinkData;
  StackEntry: TStackEntry;
begin
  CellText := ' ';

  Data := Sender.GetNodeData(Node);
  if Data^.LinkType = ltSyncObjStack then
  begin
    StackEntry := TStackEntry.Create;
    StackEntry.UpdateInfo(Data^.SyncObjStackPtr);
    try
      case Column of
        0: CellText := Format('%p', [Data^.SyncObjStackPtr]);
        1: if Assigned(StackEntry.UnitInfo) then
             CellText := StackEntry.UnitInfo.ShortName
           else
             CellText := 'unknown';
        2: if Assigned(StackEntry.LineInfo) then
             CellText := IntToStr(StackEntry.LineInfo.LineNo);
        3: if Assigned(StackEntry.FuncInfo) then
             CellText := StackEntry.FuncInfo.ShortName
           else
             CellText := 'unknown';
      end;
    finally
      FreeAndNil(StackEntry);
    end;
  end;
end;

procedure TMainForm.vstDbgInfoConstsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;
  ConstInfo: TConstInfo;
  UnitInfo: TUnitInfo;
begin
  Data := vstDbgInfoConsts.GetNodeData(Node);
  case Data^.LinkType of
    ltDbgUnitInfo:
      begin
        UnitInfo := Data^.DbgUnitInfo;
        case Column of
          0: CellText := UnitInfo.ShortName;
        else
          CellText := ' ';
        end;
      end;
    ltDbgConstInfo:
      begin
        ConstInfo := Data^.DbgConstInfo;

        case Column of
          0: CellText := ConstInfo.ShortName;
          1: CellText := ConstInfo.ValueAsString;
          2: CellText := String(ConstInfo.TypeInfo.ShortName);
        end;
      end;
  end;
end;

procedure TMainForm.vstDbgInfoFunctionsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;
  FuncInfo: TFuncInfo;
begin
  Data := vstDbgInfoFunctions.GetNodeData(Node);
  if Data^.LinkType = ltDbgFuncInfo then
  begin
    FuncInfo := Data^.DbgFuncInfo;

    LoadFunctionParams(FuncInfo, Node);
    LoadFunctionSource(svfDbgInfoFuncAdv, FuncInfo);
  end;
end;

procedure TMainForm.vstDbgInfoFunctionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;
  FuncInfo: TFuncInfo;
  UnitInfo: TUnitInfo;
begin
  Data := vstDbgInfoFunctions.GetNodeData(Node);
  case Data^.LinkType of
    ltDbgUnitInfo:
      begin
        UnitInfo := Data^.DbgUnitInfo;
        case Column of
          0: CellText := UnitInfo.ShortName;
        else
          CellText := ' ';
        end;
      end;
    ltDbgFuncInfo:
      begin
        FuncInfo := Data^.DbgFuncInfo;

        case Column of
          0: CellText := FuncInfo.ShortName;
          1: CellText := Format('%p', [FuncInfo.Address]);
          2: CellText := Format('%d', [FuncInfo.Size]);
        end;
      end;
  end;
end;

procedure TMainForm.vstDbgInfoFuncVarsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;
  FuncInfo: TFuncInfo;
  VarInfo: TVarInfo;
begin
  Data := vstDbgInfoTypes.GetNodeData(Node);
  case Data^.LinkType of
    ltDbgFuncInfo:
      begin
        FuncInfo := Data^.DbgFuncInfo;
        case Column of
          0: CellText := FuncInfo.ShortName;
        else
          CellText := ' ';
        end;
      end;
    ltDbgFuncParamInfo:
      begin
        VarInfo := Data^.DbgFuncParamInfo;
        case Column of
          0: CellText := VarInfo.ShortName;
          1: CellText := VarInfo.DataType.ShortName;
          2:
            begin
              case VarInfo.VarKind of
                vkGlobal:
                  CellText := 'Global';
                vkStack:
                  CellText := 'Stack';
                vkRegister:
                  CellText := 'Reg';
                vkLink:
                  CellText := 'Link';
              end;
            end;
        end;
      end;
  end;
end;

procedure TMainForm.vstDbgInfoTypesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;
  UnitInfo: TUnitInfo;
  TypeInfo: TTypeInfo;
  MemberInfo: TStructMember;
begin
  CellText := ' ';

  Data := vstDbgInfoTypes.GetNodeData(Node);
  case Data^.LinkType of
    ltDbgUnitInfo:
      begin
        UnitInfo := Data^.DbgUnitInfo;
        case Column of
          0: CellText := UnitInfo.ShortName;
        end;
      end;
    ltDbgTypeInfo:
      begin
        TypeInfo := Data^.DbgTypeInfo;

        case Column of
          0: CellText := TypeInfo.ShortName;
          1: CellText := TypeInfo.TypeOf;
          2: CellText := IntToStr(TypeInfo.DataSize);
        end;
      end;
    ltDbgStructMemberInfo:
      begin
        MemberInfo := Data^.DbgStructMemberInfo;

        case Column of
          0: CellText := MemberInfo.ShortName;
          1: CellText := MemberInfo.DataType.ShortName;
          2: CellText := IntToStr(MemberInfo.DataSize);
          3: CellText := IntToStr(MemberInfo.Offset);
        end;
      end;
  end;

end;

procedure TMainForm.vstDbgInfoUnitsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PLinkData;
  Name1, Name2: String;
  ValueU1, ValueU2: NativeUInt;
  ValueS1, ValueS2: Int64;
begin
  Data1 := vstDbgInfoUnits.GetNodeData(Node1);
  Data2 := vstDbgInfoUnits.GetNodeData(Node2);

  case Column of
    0:
      begin
        Name1 := '';
        Name2 := '';

        if (Data1^.LinkType = ltDbgUnitInfo) and (Data1^.LinkType = ltDbgUnitInfo) then
        begin
          Name1 := Data1^.DbgUnitInfo.ShortName;
          Name2 := Data2^.DbgUnitInfo.ShortName;
        end;

        Result := CompareText(Name1, Name2);
      end;
    1:
      begin
        ValueU1 := 0;
        ValueU2 := 0;

        if (Data1^.LinkType = ltDbgUnitInfo) and (Data1^.LinkType = ltDbgUnitInfo) then
        begin
          ValueU1 := NativeUInt(Data1^.DbgUnitInfo.Address);
          ValueU2 := NativeUInt(Data2^.DbgUnitInfo.Address);
        end;

        Result := Compare(ValueU1, ValueU2);
      end;
    2:
      begin
        ValueS1 := 0;
        ValueS2 := 0;

        if (Data1^.LinkType = ltDbgUnitInfo) and (Data2^.LinkType = ltDbgUnitInfo) then
        begin
          ValueS1 := Data1^.DbgUnitInfo.Size;
          ValueS2 := Data2^.DbgUnitInfo.Size;
        end;

        Result := Compare(ValueS1, ValueS2);
      end;
  end;
end;

procedure TMainForm.vstDbgInfoUnitsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PLinkData;
begin
  Data := vstDbgInfoUnits.GetNodeData(Node);
  case Data^.LinkType of
    ltDbgUnitGroup:
      begin
        TargetCanvas.Font.Style := [fsBold];
      end;
  end;
end;

procedure TMainForm.vstDbgInfoUnitsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;
  UnitInfo: TUnitInfo;
begin
  Data := vstDbgInfoUnits.GetNodeData(Node);
  case Data^.LinkType of
    ltDbgUnitInfo:
      begin
        UnitInfo := Data^.DbgUnitInfo;

        LoadConsts(UnitInfo, Node);
        LoadTypes(UnitInfo, Node);
        LoadVars(UnitInfo, Node);
        LoadFunctions(UnitInfo, Node);
        LoadUnitSource(UnitInfo, Node);
      end;
  end;
end;

procedure TMainForm.vstDbgInfoUnitsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;
  UnitInfo: TUnitInfo;
begin
  CellText := ' ';
  Data := vstDbgInfoUnits.GetNodeData(Node);
  case Data^.LinkType of
    ltDbgUnitGroup:
      begin
        if Column = 0 then
        begin
          case Data^.DbgUnitGroupType of
            utUnknown:
              CellText := 'Other';
            utProject:
              CellText := 'Project units';
            utSystem:
              CellText := 'System units';
            utComponentLib:
              CellText := 'Components';
            utExternal:
              CellText := 'External';
          end;
        end;
      end;
    ltDbgUnitInfo:
      begin
        UnitInfo := Data^.DbgUnitInfo;
        case Column of
          0: CellText := UnitInfo.ShortName;
          1: CellText := Format('%p ', [UnitInfo.Address]);
          2: CellText := Format('%d ', [UnitInfo.Size]);
        end;
      end;
  end;
end;

procedure TMainForm.vstDbgInfoVarsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;
  VarInfo: TVarInfo;
  UnitInfo: TUnitInfo;
begin
  Data := vstDbgInfoVars.GetNodeData(Node);
  case Data^.LinkType of
    ltDbgUnitInfo:
      begin
        UnitInfo := Data^.DbgUnitInfo;
        case Column of
          0: CellText := UnitInfo.ShortName;
        else
          CellText := ' ';
        end;
      end;
    ltDbgVarInfo:
      begin
        VarInfo := Data^.DbgVarInfo;

        case Column of
          0: CellText := VarInfo.ShortName;
          1: CellText := VarInfo.DataType.ShortName;
          2: CellText := Format('%p', [Pointer(VarInfo.Offset)]);
        end;
      end;
  end;
end;

procedure TMainForm.vstExceptionCallStackFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;
  StackEntry: TStackEntry;
begin
  svfExceptInfoSource.Clear;

  Data := vstExceptionCallStack.GetNodeData(Node);
  if Data^.LinkType = ltExceptStack then
  begin
    StackEntry := Data^.ExceptStackEntry;

    if Assigned(StackEntry) and Assigned(StackEntry.FuncInfo) then
    begin
      if Assigned(StackEntry.LineInfo) then
        LoadFunctionSource(svfExceptInfoSource, StackEntry.FuncInfo, StackEntry.LineInfo.LineNo)
      else
        LoadFunctionSource(svfExceptInfoSource, StackEntry.FuncInfo);
    end;
  end;
end;

procedure TMainForm.vstExceptionCallStackGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;
  StackEntry: TStackEntry;
begin
  CellText := ' ';

  Data := vstExceptionCallStack.GetNodeData(Node);
  if Data^.LinkType = ltExceptStack then
  begin
    StackEntry := Data^.ExceptStackEntry;
    if StackEntry <> nil then
    begin
      case Column of
        0: CellText := Format('%p', [StackEntry.EIP]);
        1: if Assigned(StackEntry.UnitInfo) then
             CellText := StackEntry.UnitInfo.ShortName
           else
             CellText := 'unknown';
        2: if Assigned(StackEntry.LineInfo) then
             CellText := IntToStr(StackEntry.LineInfo.LineNo);
        3: if Assigned(StackEntry.FuncInfo) then
             CellText := StackEntry.FuncInfo.ShortName
           else
             CellText := 'unknown';
      end;
    end;
  end;
end;

procedure TMainForm.vstExceptionListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;
  Stack: TList;
  I: Integer;
  StackNode: PVirtualNode;
  StackData: PLinkData;
begin
  vstExceptionCallStack.Clear;
  svfExceptInfoSource.Clear;

  Data := vstExceptionList.GetNodeData(Node);
  if Data^.LinkType = ltExceptInfo then
  begin
    vstExceptionCallStack.BeginUpdate;
    try
      Stack := Data^.ExceptInfo.Stack;
      for I := 0 to Stack.Count - 1 do
      begin
        StackNode := vstExceptionCallStack.AddChild(nil);
        StackData := vstExceptionCallStack.GetNodeData(StackNode);
        StackData^.ExceptStackEntry := TStackEntry(Stack[I]);
        StackData^.LinkType := ltExceptStack;
      end;
    finally
      vstExceptionCallStack.EndUpdate;
    end;
  end;
end;

procedure TMainForm.vstExceptionListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;
begin
  Data := vstExceptionList.GetNodeData(Node);
  case Column of
    0: CellText := Format('%p', [Data^.ExceptInfo.Address]);
    1: CellText := Data^.ExceptInfo.ExceptionName;
    2: CellText := Data^.ExceptInfo.Message;
  end;
end;

procedure TMainForm.vstExceptionThreadsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;
  ThData: PThreadData;
  ProcData: PProcessData;
  ExceptList: TThreadList;
  ExceptNode: PVirtualNode;
  I: Integer;
  L: TList;
begin
  vstExceptionList.Clear;
  vstExceptionCallStack.Clear;
  svfExceptInfoSource.Clear;

  if gvDebuger = Nil then Exit;

  ExceptList := Nil;
  if Assigned(Node) then
  begin
    Data := Sender.GetNodeData(Node);
    case Data^.LinkType of
      ltProcess:
      begin
        ProcData := Data^.ProcessData;
        ExceptList := ProcData^.DbgExceptions;
      end;
      ltThread:
      begin
        ThData := Data^.ThreadData;
        ExceptList := ThData^.DbgExceptions;
      end;
    end;
  end
  else
  begin
    // Для получения списка Address info
    ProcData := gvDebuger.ProcessData;
    ExceptList := ProcData^.DbgExceptions;
  end;

  vstExceptionList.BeginUpdate;
  try
    if ExceptList <> Nil then
    begin
      L := ExceptList.LockList;
      try
        for I := 0 to L.Count - 1 do
        begin
          ExceptNode := vstExceptionList.AddChild(nil);
          Data := vstExceptionList.GetNodeData(ExceptNode);
          Data^.SyncNode := Node;
          Data^.ExceptInfo := TExceptInfo(L[I]);
          Data^.LinkType := ltExceptInfo;
        end;
      finally
        ExceptList.UnlockList;
      end;
    end;
  finally
    vstExceptionList.EndUpdate;
  end;
end;

procedure TMainForm.vstExceptionThreadsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PLinkData;
  ThData: PThreadData;
  ProcData: PProcessData;
begin
  CellText := ' ';
  Data := Sender.GetNodeData(Node);
  case Data^.LinkType of
    ltProcess:
      begin
        ProcData := Data^.ProcessData;
        if ProcData <> nil then
          case Column of
            0: CellText := ExtractFileName(gvProjectOptions.ApplicationName);
            1: CellText := ProcessIDToStr(ProcData^.ProcessID);
            2: if ProcData^.DbgExceptionsCount > 0 then
                 CellText := Format('%d', [ProcData^.DbgExceptionsCount]);
          end;
      end;
    ltThread:
      begin
        ThData := Data^.ThreadData;
        if ThData <> nil then
          case Column of
            0: CellText := ThData^.ThreadAdvInfo^.AsString;
            1: CellText := ThreadIDToStr(ThData^.ThreadID);
            2: if ThData^.DbgExceptionsCount > 0 then
                 CellText := Format('%d', [ThData^.DbgExceptionsCount]);
          end;
      end;
  end;
end;

procedure TMainForm.vstLockThreadsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;
begin
  if Node = nil then Exit;

  vstLockTrackingList.Clear;
  vstLockTrackingParents.Clear;
  vstLockTrackingChilds.Clear;
  vstLockTrackingSyncObjs.Clear;
  vstLockTrackingSyncObjStack.Clear;
  svfLockTrackingSource.Clear;

  Data := vstLockThreads.GetNodeData(Node);
  case Data^.LinkType of
    //ltProcess:
    //  LoadTrackProcessFunctions(Data^.ProcessData, Node);
    ltThread:
      LoadLockTrackThreadFunctions(Data^.ThreadData, Node);
  end;
end;

procedure TMainForm.vstLockThreadsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PLinkData;
  ThData: PThreadData;
  ProcData: PProcessData;
begin
  CellText := ' ';

  Data := Sender.GetNodeData(Node);
  case Data^.LinkType of
    ltProcess:
      begin
        ProcData := Data^.ProcessData;
        if ProcData <> nil then
          case Column of
            0: CellText := ExtractFileName(gvProjectOptions.ApplicationName);
            1: CellText := ProcessIDToStr(ProcData^.ProcessID);
          end;
      end;
    ltThread:
      begin
        ThData := Data^.ThreadData;
        if ThData <> nil then
          case Column of
            0: CellText := ThData^.ThreadAdvInfo^.AsString;
            1: CellText := ThreadIDToStr(ThData^.ThreadID);
            2: if ThData^.WaitTime <> 0 then
              CellText := ElapsedToTime(ThData^.WaitTime);
          end;
      end;
  end;
end;

procedure TMainForm.vstLockTrackingListCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PLinkData;
  Name1, Name2: String;
  ValueU1, ValueU2: UInt64;
  ValueS1, ValueS2: Int64;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  case Column of
    0:
      begin
        Name1 := '';
        Name2 := '';

        if (Data1^.LinkType = ltTrackFuncInfo) and (Data2^.LinkType = ltTrackFuncInfo) then
        begin
          Name1 := TFuncInfo(Data1^.TrackFuncInfo.FuncInfo).ShortName;
          Name2 := TFuncInfo(Data2^.TrackFuncInfo.FuncInfo).ShortName;
        end
        else
        if (Data1^.LinkType = ltTrackUnitInfo) and (Data2^.LinkType = ltTrackUnitInfo) then
        begin
          Name1 := TUnitInfo(Data1^.TrackUnitInfo.UnitInfo).ShortName;
          Name2 := TUnitInfo(Data2^.TrackUnitInfo.UnitInfo).ShortName;
        end;

        Result := CompareText(Name1, Name2);
      end;
    1:
      begin
        ValueU1 := 0;
        ValueU2 := 0;

        if (Data1^.LinkType = ltTrackFuncInfo) and (Data2^.LinkType = ltTrackFuncInfo) then
        begin
          ValueU1 := Data1^.TrackFuncInfo.CallCount;
          ValueU2 := Data2^.TrackFuncInfo.CallCount;
        end
        else
        if (Data1^.LinkType = ltTrackUnitInfo) and (Data2^.LinkType = ltTrackUnitInfo) then
        begin
          ValueU1 := Data1^.TrackUnitInfo.CallCount;
          ValueU2 := Data2^.TrackUnitInfo.CallCount;
        end;

        Result := Compare(ValueU1, ValueU2);
      end;
    2:
      begin
        ValueS1 := 0;
        ValueS2 := 0;

        if (Data1^.LinkType = ltTrackFuncInfo) and (Data2^.LinkType = ltTrackFuncInfo) then
        begin
          ValueS1 := TSyncObjsTrackFuncInfo(Data1^.TrackFuncInfo).WaitTime;
          ValueS2 := TSyncObjsTrackFuncInfo(Data2^.TrackFuncInfo).WaitTime;
        end;

        Result := Compare(ValueS1, ValueS2);
      end;
  end;
end;

procedure TMainForm.vstLockTrackingListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;
  TrackFuncInfo: TSyncObjsTrackFuncInfo;
begin
  Data := vstLockTrackingList.GetNodeData(Node);
  case Data^.LinkType of
    ltTrackFuncInfo:
      begin
        vstLockTrackingSyncObjStack.Clear;

        TrackFuncInfo := TSyncObjsTrackFuncInfo(Data^.TrackFuncInfo);

        LoadSyncObjsInfoObjects(vstLockTrackingSyncObjs, TrackFuncInfo.SyncObjsList, Node);
        LoadMemInfoParentFunctions(vstLockTrackingParents, TrackFuncInfo, Node);
        LoadMemInfoChildFunctions(vstLockTrackingChilds, TrackFuncInfo, Node);

        LoadFunctionSource(svfLockTrackingSource, TFuncInfo(TrackFuncInfo.FuncInfo));
      end;
  else
    begin
      vstLockTrackingParents.Clear;
      vstLockTrackingChilds.Clear;
      vstLockTrackingSyncObjStack.Clear;
      vstLockTrackingSyncObjs.Clear;
      svfLockTrackingSource.Clear;
    end;
  end;
end;

procedure TMainForm.vstLockTrackingListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PLinkData;
  ThData: PThreadData;
  TrackFuncInfo: TSyncObjsTrackFuncInfo;
  TrackUnitInfo: TSyncObjsTrackUnitInfo;
begin
  CellText := ' ';
  Data := vstLockTrackingList.GetNodeData(Node);
  case Data^.LinkType of
    ltProcess:
      begin
        case Column of
          0: CellText := ExtractFileName(gvProjectOptions.ApplicationName);
        end;
      end;
    ltThread:
      begin
        ThData := Data^.ThreadData;
        case Column of
          0: CellText := ThData^.ThreadAdvInfo^.AsString;
          1: ;
          2: CellText := ElapsedToTime(ThData^.WaitTime);
        end;
      end;
    ltTrackFuncInfo:
      begin
        TrackFuncInfo := TSyncObjsTrackFuncInfo(Data^.TrackFuncInfo);
        case Column of
          0: CellText := TFuncInfo(TrackFuncInfo.FuncInfo).ShortName;
          1: CellText := IntToStr(TrackFuncInfo.CallCount);
          2: CellText := ElapsedToTime(TrackFuncInfo.WaitTime);
        end;
      end;
    ltTrackUnitInfo:
      begin
        TrackUnitInfo := TSyncObjsTrackUnitInfo(Data^.TrackUnitInfo);
        case Column of
          0: CellText := TUnitInfo(TrackUnitInfo.UnitInfo).ShortName;
        end;
      end;
  end;
end;

procedure TMainForm.vstLockTrackingParentsDblClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PLinkData;
  FuncInfo: TFuncInfo;
  FuncNode: PVirtualNode;
  LineNo: Cardinal;
begin
  Node := vstLockTrackingParents.FocusedNode;
  if Assigned(Node) then
  begin
    Data := vstLockTrackingParents.GetNodeData(Node);
    if Data^.LinkType = ltTrackCallFuncInfo then
    begin
      FuncInfo := TFuncInfo(Data^.TrackCallFuncInfo^.FuncInfo);
      if Assigned(FuncInfo) then
      begin
        LineNo := Data^.TrackCallFuncInfo^.LineNo;

        FuncNode := FindTrackFuncNode(vstLockTrackingList, FuncInfo);
        if Assigned(FuncNode) then
        begin
          vstLockTrackingList.ClearSelection;
          vstLockTrackingList.FocusedNode := FuncNode;
          vstLockTrackingList.Selected[FuncNode] := True;

          if LineNo <> 0 then
            LoadFunctionSource(svfLockTrackingSource, FuncInfo, LineNo);
        end;
      end;
    end;
  end;
end;

procedure TMainForm.vstLockTrackingSyncObjsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PLinkData;
  Name1, Name2: String;
  Th1, Th2: NativeUInt;
  WaitTime1, WaitTime2: Int64;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  case Column of
    0:
      begin
        Name1 := '';
        Name2 := '';

        if (Data1^.LinkType = ltSyncObjInfo) and (Data1^.LinkType = ltSyncObjInfo) then
        begin
          vstLockTrackingSyncObjsGetText(Sender, Node1, Column, ttNormal, Name1);
          vstLockTrackingSyncObjsGetText(Sender, Node2, Column, ttNormal, Name2);
        end;

        if TVirtualStringTree(Sender).Header.SortDirection = sdAscending then
          Result := Compare(Name1, Name2, 1)
        else
          Result := Compare(Name1, Name2, -1);
      end;
    1:
      begin
        Th1 := 0;
        Th2 := 0;

        if (Data1^.LinkType = ltSyncObjInfo) and (Data1^.LinkType = ltSyncObjInfo) then
        begin
          Th1 := NativeUInt(Data1^.SyncObjItem^.SyncObjsInfo.ThreadId);
          Th2 := NativeUInt(Data2^.SyncObjItem^.SyncObjsInfo.ThreadId);
        end;

        Result := Compare(Th1, Th2);
      end;
    2:
      begin
        WaitTime1 := 0;
        WaitTime2 := 0;

        if (Data1^.LinkType = ltSyncObjInfo) and (Data1^.LinkType = ltSyncObjInfo) then
        begin
          WaitTime1 := NativeUInt(Data1^.SyncObjItem^.WaitTime);
          WaitTime2 := NativeUInt(Data2^.SyncObjItem^.WaitTime);
        end;

        Result := Compare(WaitTime1, WaitTime2);
      end;
  end;
end;

procedure TMainForm.vstLockTrackingSyncObjsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;
begin
  Data := Sender.GetNodeData(Node);
  case Data^.LinkType of
    ltSyncObjInfo:
      LoadSyncObjsInfoStack(vstLockTrackingSyncObjStack, Data^.SyncObjItem, Node);
  end;
end;

procedure TMainForm.vstLockTrackingSyncObjsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;
begin
  CellText := ' ';

  Data := Sender.GetNodeData(Node);
  case Data^.LinkType of
    ltSyncObjInfo:
      case Column of
        0: begin
          case Data^.SyncObjItem^.SyncObjsInfo.SyncObjsType of
            soEnterCriticalSection:
              CellText := 'EnterCriticalSection';
            soSendMessage:
              CellText := 'SendMessage';
          end;
        end;
        1: begin
          case Data^.SyncObjItem^.SyncObjsInfo.SyncObjsType of
            soEnterCriticalSection:
              CellText := ThreadIDToStr(Data^.SyncObjItem^.SyncObjsInfo.OwningThreadId);
          end;
        end;
        2: begin
          case Data^.SyncObjItem^.SyncObjsInfo.SyncObjsType of
            soEnterCriticalSection, soSendMessage:
              CellText := ElapsedToTime(Data^.SyncObjItem^.WaitTime);
          end;
        end;
      end;
  end;
end;

procedure TMainForm.vstLockTrackingChildsDblClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PLinkData;
  FuncInfo: TFuncInfo;
  FuncNode: PVirtualNode;
  LineNo: Cardinal;
begin
  Node := vstLockTrackingChilds.FocusedNode;
  if Assigned(Node) then
  begin
    Data := vstLockTrackingChilds.GetNodeData(Node);
    if Data^.LinkType = ltTrackCallFuncInfo then
    begin
      FuncInfo := TFuncInfo(Data^.TrackCallFuncInfo^.FuncInfo);
      if Assigned(FuncInfo) then
      begin
        LineNo := Data^.TrackCallFuncInfo^.LineNo;

        FuncNode := FindTrackFuncNode(vstLockTrackingList, FuncInfo);
        if Assigned(FuncNode) then
        begin
          vstLockTrackingList.ClearSelection;
          vstLockTrackingList.FocusedNode := FuncNode;
          vstLockTrackingList.Selected[FuncNode] := True;

          if LineNo <> 0 then
            LoadFunctionSource(svfLockTrackingSource, FuncInfo, LineNo);
        end;
      end;
    end;
  end;
end;

procedure TMainForm.vstLockTrackingLinksGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PLinkData;
  TrackCallFuncInfo: PCallFuncInfo;
  TrackFuncInfo: TSyncObjsTrackFuncInfo;
begin
  CellText := ' ';

  Data := Sender.GetNodeData(Node);
  case Data^.LinkType of
    ltDbgUnitInfo:
      begin
        case Column of
          0: CellText := Data^.DbgUnitInfo.ShortName;
        end;
      end;
    ltTrackFuncInfo:
      begin
        TrackFuncInfo := TSyncObjsTrackFuncInfo(Data^.TrackFuncInfo);
        case Column of
          0: CellText := TFuncInfo(TrackFuncInfo.FuncInfo).ShortName;
          2: CellText := IntToStr(TrackFuncInfo.CallCount);
          3: CellText := ElapsedToTime(TrackFuncInfo.WaitTime);
        end;
      end;
    ltTrackCallFuncInfo:
      begin
        TrackCallFuncInfo := Data^.TrackCallFuncInfo;
        case Column of
          0: CellText := TFuncInfo(TrackCallFuncInfo^.FuncInfo).ShortName;
          1: if TrackCallFuncInfo^.LineNo > 0 then
            CellText := IntToStr(TrackCallFuncInfo^.LineNo);
          2: CellText := IntToStr(TrackCallFuncInfo^.CallCount);
          3: CellText := ElapsedToTime(TrackCallFuncInfo^.Data);
        end;
      end;
  end;
end;

procedure TMainForm.vstLogColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  vstLogResize(Sender);
end;

procedure TMainForm.vstLogDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PLinkData;
  Item: TDbgLogItem;
begin
  Data := vstLog.GetNodeData(Node);
  case Data^.LinkType of
    ltDbgLogItem:
      begin
        Item := Data^.DbgLogItem;
        TargetCanvas.Font.Color := FSpiderOptions.LogColors[Item.LogType];
      end;
  end;
end;

procedure TMainForm.vstLogGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;
  Item: TDbgLogItem;
begin
  Data := vstLog.GetNodeData(Node);
  case Data^.LinkType of
    ltProject:
      begin
        case Column of
          1: CellText := gvProjectOptions.ApplicationName;
        else
          CellText := 'Application:';
        end;
      end;
    ltDbgLogItem:
      begin
        Item := Data^.DbgLogItem;
        case Column of
          0: CellText := FormatDateTime('yyyy.dd.mm hh:nn:ss.zzz', Item.DateTime);
          1: CellText := Item.LogMessage;
        end;
      end;
  end;
end;

procedure TMainForm.vstLogResize(Sender: TObject);
begin
  vstLog.Header.Columns[1].Width := vstLog.ClientWidth - vstLog.Header.Columns[0].Width - 1;
end;

procedure TMainForm.vstMemInfoFuncChildsDblClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PLinkData;
  FuncInfo: TFuncInfo;
  FuncNode: PVirtualNode;
  LineNo: Cardinal;
begin
  Node := vstMemInfoFuncChilds.FocusedNode;
  if Assigned(Node) then
  begin
    Data := vstMemInfoFuncChilds.GetNodeData(Node);
    if Data^.LinkType = ltTrackCallFuncInfo then
    begin
      FuncInfo := TFuncInfo(Data^.TrackCallFuncInfo^.FuncInfo);
      if Assigned(FuncInfo) then
      begin
        LineNo := Data^.TrackCallFuncInfo^.LineNo;

        FuncNode := FindTrackFuncNode(vstMemInfoFuncTree, FuncInfo);
        if Assigned(FuncNode) then
        begin
          vstMemInfoFuncTree.ClearSelection;
          vstMemInfoFuncTree.FocusedNode := FuncNode;
          vstMemInfoFuncTree.Selected[FuncNode] := True;

          if LineNo <> 0 then
            LoadFunctionSource(svfMemInfoFuncSrc, FuncInfo, LineNo);
        end;
      end;
    end;
  end;
end;

procedure TMainForm.vstMemInfoFuncParentsDblClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PLinkData;
  FuncInfo: TFuncInfo;
  FuncNode: PVirtualNode;
  LineNo: Cardinal;
begin
  Node := vstMemInfoFuncParents.FocusedNode;
  if Assigned(Node) then
  begin
    Data := vstMemInfoFuncParents.GetNodeData(Node);
    if Data^.LinkType = ltTrackCallFuncInfo then
    begin
      FuncInfo := TFuncInfo(Data^.TrackCallFuncInfo^.FuncInfo);
      if Assigned(FuncInfo) then
      begin
        LineNo := Data^.TrackCallFuncInfo^.LineNo;

        FuncNode := FindTrackFuncNode(vstMemInfoFuncTree, FuncInfo);
        if Assigned(FuncNode) then
        begin
          vstMemInfoFuncTree.ClearSelection;
          vstMemInfoFuncTree.FocusedNode := FuncNode;
          vstMemInfoFuncTree.Selected[FuncNode] := True;

          if LineNo <> 0 then
            LoadFunctionSource(svfMemInfoFuncSrc, FuncInfo, LineNo);
        end;
      end;
    end;
  end;
end;

procedure TMainForm.vstMemInfoFuncLinksGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PLinkData;
  TrackCallFuncInfo: PCallFuncInfo;
  TrackFuncInfo: TMemInfoTrackFuncInfo;
begin
  CellText := ' ';

  Data := Sender.GetNodeData(Node);
  case Data^.LinkType of
    ltDbgUnitInfo:
      begin
        case Column of
          0: CellText := Data^.DbgUnitInfo.ShortName;
        end;
      end;
    ltTrackFuncInfo:
      begin
        TrackFuncInfo := TMemInfoTrackFuncInfo(Data^.TrackFuncInfo);
        case Column of
          0: CellText := TFuncInfo(TrackFuncInfo.FuncInfo).ShortName;
          2: CellText := IntToStr(TrackFuncInfo.CurCount);
          3: CellText := IntToStr(TrackFuncInfo.Size);
        end;
      end;
    ltTrackCallFuncInfo:
      begin
        TrackCallFuncInfo := Data^.TrackCallFuncInfo;
        case Column of
          0: CellText := TFuncInfo(TrackCallFuncInfo^.FuncInfo).ShortName;
          1: if TrackCallFuncInfo^.LineNo > 0 then
            CellText := IntToStr(TrackCallFuncInfo^.LineNo);
          2: CellText := IntToStr(TrackCallFuncInfo^.CallCount);
          3: CellText := IntToStr(TrackCallFuncInfo^.Size);
        end;
      end;
  end;
end;

procedure TMainForm.vstMemInfoFuncTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PLinkData;
  Name1, Name2: String;
  ValueU1, ValueU2: UInt64;
  ValueS1, ValueS2: Int64;
begin
  Data1 := vstMemInfoFuncTree.GetNodeData(Node1);
  Data2 := vstMemInfoFuncTree.GetNodeData(Node2);

  case Column of
    0:
      begin
        Name1 := '';
        Name2 := '';

        if (Data1^.LinkType = ltTrackFuncInfo) and (Data2^.LinkType = ltTrackFuncInfo) then
        begin
          Name1 := TFuncInfo(Data1^.TrackFuncInfo.FuncInfo).ShortName;
          Name2 := TFuncInfo(Data2^.TrackFuncInfo.FuncInfo).ShortName;
        end
        else
        if (Data1^.LinkType = ltTrackUnitInfo) and (Data2^.LinkType = ltTrackUnitInfo) then
        begin
          Name1 := TUnitInfo(Data1^.TrackUnitInfo.UnitInfo).ShortName;
          Name2 := TUnitInfo(Data2^.TrackUnitInfo.UnitInfo).ShortName;
        end;

        Result := CompareText(Name1, Name2);
      end;
    1:
      begin
        ValueU1 := 0;
        ValueU2 := 0;

        if (Data1^.LinkType = ltTrackFuncInfo) and (Data2^.LinkType = ltTrackFuncInfo) then
        begin
          ValueU1 := Data1^.TrackFuncInfo.CallCount;
          ValueU2 := Data2^.TrackFuncInfo.CallCount;
        end
        else
        if (Data1^.LinkType = ltTrackUnitInfo) and (Data2^.LinkType = ltTrackUnitInfo) then
        begin
          ValueU1 := Data1^.TrackUnitInfo.CallCount;
          ValueU2 := Data2^.TrackUnitInfo.CallCount;
        end;

        Result := Compare(ValueU1, ValueU2);
      end;
    2:
      begin
        ValueS1 := 0;
        ValueS2 := 0;

        if (Data1^.LinkType = ltTrackFuncInfo) and (Data2^.LinkType = ltTrackFuncInfo) then
        begin
          ValueS1 := TMemInfoTrackFuncInfo(Data1^.TrackFuncInfo).Size;
          ValueS2 := TMemInfoTrackFuncInfo(Data2^.TrackFuncInfo).Size;
        end;

        Result := Compare(ValueS1, ValueS2);
      end;
  end;
end;

procedure TMainForm.vstMemInfoFuncTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;
  TrackFuncInfo: TMemInfoTrackFuncInfo;
begin
  Data := vstMemInfoFuncTree.GetNodeData(Node);
  case Data^.LinkType of
    ltTrackFuncInfo:
      begin
        vstMemInfoObjStack.Clear;

        TrackFuncInfo := TMemInfoTrackFuncInfo(Data^.TrackFuncInfo);

        LoadMemInfoObjects(vstMemInfoObjects, TrackFuncInfo.GetMemList, Node);
        LoadMemInfoParentFunctions(vstMemInfoFuncParents, TrackFuncInfo, Node);
        LoadMemInfoChildFunctions(vstMemInfoFuncChilds, TrackFuncInfo, Node);

        LoadFunctionSource(svfMemInfoFuncSrc, TFuncInfo(TrackFuncInfo.FuncInfo));
      end;
  else
    begin
      vstMemInfoObjects.Clear;
      vstMemInfoObjStack.Clear;
      vstMemInfoFuncParents.Clear;
      vstMemInfoFuncChilds.Clear;
      svfMemInfoFuncSrc.Clear;
    end;
  end;
end;

procedure TMainForm.vstMemInfoFuncTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PLinkData;
  ThData: PThreadData;
  ProcData: PProcessData;
  TrackFuncInfo: TMemInfoTrackFuncInfo;
  TrackUnitInfo: TTrackUnitInfo;
begin
  CellText := ' ';
  Data := vstMemInfoFuncTree.GetNodeData(Node);
  case Data^.LinkType of
    ltProcess:
      begin
        ProcData := Data^.ProcessData;
        case Column of
          0: CellText := ExtractFileName(gvProjectOptions.ApplicationName);
          1: CellText := IntToStr(ProcData^.DbgGetMemInfo.Count);
          2: CellText := IntToStr(ProcData^.DbgGetMemInfoSize);
        end;
      end;
    ltThread:
      begin
        ThData := Data^.ThreadData;
        case Column of
          0: CellText := ThData^.ThreadAdvInfo^.AsString;
          1: CellText := IntToStr(ThData^.DbgGetMemInfo.Count);
          2: CellText := IntToStr(ThData^.DbgGetMemInfoSize);
        end;
      end;
    ltTrackFuncInfo:
      begin
        TrackFuncInfo := TMemInfoTrackFuncInfo(Data^.TrackFuncInfo);
        case Column of
          0: CellText := TFuncInfo(TrackFuncInfo.FuncInfo).ShortName;
          1: CellText := IntToStr(TrackFuncInfo.CurCount);
          2: CellText := IntToStr(TrackFuncInfo.Size);
        end;
      end;
    ltTrackUnitInfo:
      begin
        TrackUnitInfo := Data^.TrackUnitInfo;
        case Column of
          0: CellText := TUnitInfo(TrackUnitInfo.UnitInfo).ShortName;
          //1: CellText := IntToStr(TrackUnitInfo.CurCount);
        end;
      end;
  end;
end;

procedure TMainForm.vstMemInfoObjectsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PLinkData;
  Name1, Name2: String;
  Ptr1, Ptr2: NativeUInt;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  case Column of
    0:
      begin
        Name1 := '';
        Name2 := '';

        if (Data1^.LinkType = ltMemInfo) and (Data1^.LinkType = ltMemInfo) then
        begin
          if Sender = vstMemInfoObjects then
          begin
            vstMemInfoObjectsGetText(Sender, Node1, Column, ttNormal, Name1);
            vstMemInfoObjectsGetText(Sender, Node2, Column, ttNormal, Name2);
          end
          else
          if Sender = vstMemList then
          begin
            vstMemListGetText(Sender, Node1, Column, ttNormal, Name1);
            vstMemListGetText(Sender, Node2, Column, ttNormal, Name2);
          end;
        end;

        if TVirtualStringTree(Sender).Header.SortDirection = sdAscending then
          Result := Compare(Name1, Name2, 1)
        else
          Result := Compare(Name1, Name2, -1);
      end;
    1:
      begin
        Ptr1 := 0;
        Ptr2 := 0;

        if (Data1^.LinkType = ltMemInfo) and (Data1^.LinkType = ltMemInfo) then
        begin
          Ptr1 := NativeUInt(Data1^.MemPtr);
          Ptr2 := NativeUInt(Data2^.MemPtr);
        end;

        Result := Compare(Ptr1, Ptr2);
      end;
    2:
      begin
        Name1 := '';
        Name2 := '';

        if (Data1^.LinkType = ltMemInfo) and (Data1^.LinkType = ltMemInfo) then
        begin
          if Sender = vstMemInfoObjects then
          begin
            vstMemInfoObjectsGetText(Sender, Node1, Column, ttNormal, Name1);
            vstMemInfoObjectsGetText(Sender, Node2, Column, ttNormal, Name2);
          end
          else
          if Sender = vstMemList then
          begin
            vstMemListGetText(Sender, Node1, Column, ttNormal, Name1);
            vstMemListGetText(Sender, Node2, Column, ttNormal, Name2);
          end;
        end;

        Result := CompareNumberStr(Name1, Name2);
      end;
  end;
end;

procedure TMainForm.vstMemInfoObjectsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;

  FuncNode: PVirtualNode;
  FuncLinkData: PLinkData;

  ThNode: PVirtualNode;
  ThLinkData: PLinkData;

  ThData: PThreadData;
  ProcData: PProcessData;
  MemInfo: TGetMemInfoList;
  GetMemInfo: TGetMemInfo;
begin
  vstMemInfoObjStack.Clear;

  Data := vstMemInfoObjects.GetNodeData(Node);

  FuncNode := Data^.SyncNode;
  if FuncNode = Nil then Exit;

  FuncLinkData := vstMemInfoFuncTree.GetNodeData(FuncNode);

  ThNode := FuncLinkData^.SyncNode;
  if ThNode = Nil then Exit;

  ThLinkData := vstMemInfoThreads.GetNodeData(ThNode);
  if ThLinkData = Nil then Exit;

  MemInfo := Nil;
  case ThLinkData^.LinkType of
    ltProcess:
    begin
      ProcData := ThLinkData^.ProcessData;
      MemInfo := ProcData^.DbgGetMemInfo;
    end;
    ltThread:
    begin
      ThData := ThLinkData^.ThreadData;
      MemInfo := ThData^.DbgGetMemInfo;
    end;
  end;

  if (MemInfo <> Nil) then
  begin
    MemInfo.Lock.BeginRead;
    try
      if MemInfo.TryGetValue(Data^.MemPtr, GetMemInfo) then
        LoadMemInfoObjectStack(vstMemInfoObjStack, GetMemInfo, Node);
    finally
      MemInfo.Lock.EndRead;
    end;
  end;
end;

procedure TMainForm.vstMemInfoObjectsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;

  FuncNode: PVirtualNode;
  FuncLinkData: PLinkData;

  ThNode: PVirtualNode;
  ThLinkData: PLinkData;
  ThData: PThreadData;
  ProcData: PProcessData;

  MemInfo: TGetMemInfoList;
  GetMemInfo: TGetMemInfo;
begin
  MemInfo := Nil;
  CellText := '';

  Data := vstMemInfoObjects.GetNodeData(Node);

  FuncNode := Data^.SyncNode;
  if FuncNode = Nil then Exit;

  FuncLinkData := vstMemInfoFuncTree.GetNodeData(FuncNode);

  //if FuncLinkData^.LinkType <> ltTrackFuncInfo then Exit;

  ThNode := FuncLinkData^.SyncNode;
  if ThNode = Nil then Exit;

  ThLinkData := vstMemInfoThreads.GetNodeData(ThNode);
  if ThLinkData = Nil then Exit;

  case ThLinkData^.LinkType of
    ltProcess:
    begin
      ProcData := ThLinkData^.ProcessData;
      MemInfo := ProcData^.DbgGetMemInfo;
    end;
    ltThread:
    begin
      ThData := ThLinkData^.ThreadData;
      MemInfo := ThData^.DbgGetMemInfo;
    end;
  end;

  case Column of
    0:
      begin
        if (MemInfo <> Nil) then
        begin
          MemInfo.Lock.BeginRead;
          if MemInfo.TryGetValue(Data^.MemPtr, GetMemInfo) then
            CellText := GetMemInfo.GetObjectType;
          MemInfo.Lock.EndRead;
        end;
      end;
    1: CellText := Format('%p', [Data^.MemPtr]);
    2:
      begin
        if (MemInfo <> Nil) then
        begin
          MemInfo.Lock.BeginRead;
          if MemInfo.TryGetValue(Data^.MemPtr, GetMemInfo) then
            CellText := IntToStr(GetMemInfo.Size);
          MemInfo.Lock.EndRead;
        end;
      end;
  end;
end;

procedure TMainForm.vstMemInfoObjStackDblClick(Sender: TObject);
var
  Data: PLinkData;
  StackEntry: TStackEntry;
begin
  svfMemInfoFuncSrc.Clear;

  if vstMemInfoObjStack.FocusedNode = Nil then
    Exit;

  Data := vstMemInfoObjStack.GetNodeData(vstMemInfoObjStack.FocusedNode);
  if Data^.LinkType = ltMemStack then
  begin
    StackEntry := TStackEntry.Create;
    try
      StackEntry.UpdateInfo(Data^.MemStackPtr);

      if Assigned(StackEntry.FuncInfo) then
      begin
        if Assigned(StackEntry.LineInfo) then
          LoadFunctionSource(svfMemInfoFuncSrc, StackEntry.FuncInfo, StackEntry.LineInfo.LineNo)
        else
          LoadFunctionSource(svfMemInfoFuncSrc, StackEntry.FuncInfo);

        pcMemInfoFuncInfo.ActivePageIndex := 1;
      end;
    finally
      FreeAndNil(StackEntry);
    end;
  end;
end;

procedure TMainForm.vstMemInfoThreadsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;
  ThData: PThreadData;
  ProcData: PProcessData;
  MemInfo: TGetMemInfoList;
begin
  vstMemList.Clear;
  vstMemStack.Clear;
  svfMemInfoSource.Clear;

  vstMemInfoFuncTree.Clear;
  vstMemInfoObjects.Clear;
  vstMemInfoObjStack.Clear;
  vstMemInfoFuncParents.Clear;
  vstMemInfoFuncChilds.Clear;
  svfMemInfoFuncSrc.Clear;

  if Node = nil then Exit;

  Data := vstMemInfoThreads.GetNodeData(Node);
  case Data^.LinkType of
    ltProcess:
    begin
      ProcData := Data^.ProcessData;
      MemInfo := ProcData^.DbgGetMemInfo;

      case pcMemInfo.ActivePageIndex of
        0: LoadMemInfoObjects(vstMemList, MemInfo, Node);
        //1: LoadMemInfoThreadFunctions(ThData, Node);
      end;
    end;
    ltThread:
    begin
      ThData := Data^.ThreadData;
      MemInfo := ThData^.DbgGetMemInfo;

      case pcMemInfo.ActivePageIndex of
        0: LoadMemInfoObjects(vstMemList, MemInfo, Node);
        1: LoadMemInfoThreadFunctions(ThData, Node);
      end;
    end;
  end;
end;

procedure TMainForm.vstMemInfoThreadsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PLinkData;
  ThData: PThreadData;
  ProcData: PProcessData;
begin
  CellText := ' ';

  Data := Sender.GetNodeData(Node);
  case Data^.LinkType of
    ltProcess:
      begin
        ProcData := Data^.ProcessData;
        if ProcData <> nil then
          case Column of
            0: CellText := ExtractFileName(gvProjectOptions.ApplicationName);
            1: CellText := ProcessIDToStr(ProcData^.ProcessID);
            2: if ProcData^.ProcessGetMemCount > 0 then
                 CellText := Format('%d', [ProcData^.ProcessGetMemCount]);
            3: if ProcData^.ProcessGetMemCount > 0 then
                 CellText := Format('%d', [ProcData^.ProcessGetMemSize]);
          end;
      end;
    ltThread:
      begin
        ThData := Data^.ThreadData;
        if ThData <> nil then
          case Column of
            0: CellText := ThData^.ThreadAdvInfo^.AsString;
            1: CellText := ThreadIDToStr(ThData^.ThreadID);
            2: if ThData^.DbgGetMemInfo.Count > 0 then
                 CellText := Format('%d', [ThData^.DbgGetMemInfo.Count]);
            3: if ThData^.DbgGetMemInfo.Count > 0 then
                 CellText := Format('%d', [ThData^.DbgGetMemInfoSize]);
          end;
      end;
  end;
end;

procedure TMainForm.vstMemListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;
  ThNode: PVirtualNode;
  ThLinkData: PLinkData;

  ThData: PThreadData;
  ProcData: PProcessData;
  MemInfo: TGetMemInfoList;
  GetMemInfo: TGetMemInfo;
begin
  vstMemStack.Clear;
  svfMemInfoSource.Clear;

  Data := vstMemList.GetNodeData(Node);

  MemInfo := Nil;
  ThNode := Data^.SyncNode;
  if ThNode = Nil then Exit;

  ThLinkData := vstMemInfoThreads.GetNodeData(ThNode);
  if ThLinkData = Nil then Exit;

  case ThLinkData^.LinkType of
    ltProcess:
    begin
      ProcData := ThLinkData^.ProcessData;
      MemInfo := ProcData^.DbgGetMemInfo;
    end;
    ltThread:
    begin
      ThData := ThLinkData^.ThreadData;
      MemInfo := ThData^.DbgGetMemInfo;
    end;
  end;

  if (MemInfo <> Nil) then
  begin
    MemInfo.Lock.BeginRead;
    try
      if MemInfo.TryGetValue(Data^.MemPtr, GetMemInfo) then
        LoadMemInfoObjectStack(vstMemStack, GetMemInfo, Node);
    finally
      MemInfo.Lock.EndRead;
    end;
  end;
end;

procedure TMainForm.vstMemListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;
  ThNode: PVirtualNode;
  ThLinkData: PLinkData;

  ThData: PThreadData;
  ProcData: PProcessData;
  MemInfo: TGetMemInfoList;
  GetMemInfo: TGetMemInfo;
begin
  MemInfo := Nil;
  CellText := '';

  Data := vstMemList.GetNodeData(Node);
  ThNode := Data^.SyncNode;
  if ThNode = Nil then Exit;

  ThLinkData := vstMemInfoThreads.GetNodeData(ThNode);
  if ThLinkData = Nil then Exit;

  case ThLinkData^.LinkType of
    ltProcess:
    begin
      ProcData := ThLinkData^.ProcessData;
      MemInfo := ProcData^.DbgGetMemInfo;
    end;
    ltThread:
    begin
      ThData := ThLinkData^.ThreadData;
      MemInfo := ThData^.DbgGetMemInfo;
    end;
  end;

  case Column of
    0:
      begin
        if (MemInfo <> Nil) then
        begin
          MemInfo.Lock.BeginRead;
          if MemInfo.TryGetValue(Data^.MemPtr, GetMemInfo) then
            CellText := GetMemInfo.GetObjectType;
          MemInfo.Lock.EndRead;
        end;
      end;
    1: CellText := Format('%p', [Data^.MemPtr]);
    2:
      begin
        if (MemInfo <> Nil) then
        begin
          MemInfo.Lock.BeginRead;
          if MemInfo.TryGetValue(Data^.MemPtr, GetMemInfo) then
            CellText := IntToStr(GetMemInfo.Size);
          MemInfo.Lock.EndRead;
        end;
      end;
  end;
end;

procedure TMainForm.vstMemStackFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;
  StackEntry: TStackEntry;
begin
  svfMemInfoSource.Clear;

  Data := vstMemStack.GetNodeData(Node);
  if Data^.LinkType = ltMemStack then
  begin
    StackEntry := TStackEntry.Create;
    StackEntry.UpdateInfo(Data^.MemStackPtr);
    try
      if Assigned(StackEntry.FuncInfo) then
      begin
        if Assigned(StackEntry.LineInfo) then
          LoadFunctionSource(svfMemInfoSource, StackEntry.FuncInfo, StackEntry.LineInfo.LineNo)
        else
          LoadFunctionSource(svfMemInfoSource, StackEntry.FuncInfo);
      end;
    finally
      FreeAndNil(StackEntry);
    end;
  end;
end;

procedure TMainForm.vstMemStackGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;
  StackEntry: TStackEntry;
begin
  CellText := ' ';

  Data := Sender.GetNodeData(Node);
  if Data^.LinkType = ltMemStack then
  begin
    StackEntry := TStackEntry.Create;
    StackEntry.UpdateInfo(Data^.MemStackPtr);
    try
      case Column of
        0: CellText := Format('%p', [Data^.MemStackPtr]);
        1: if Assigned(StackEntry.UnitInfo) then
             CellText := StackEntry.UnitInfo.ShortName
           else
             CellText := 'unknown';
        2: if Assigned(StackEntry.LineInfo) then
             CellText := IntToStr(StackEntry.LineInfo.LineNo);
        3: if Assigned(StackEntry.FuncInfo) then
             CellText := StackEntry.FuncInfo.ShortName
           else
             CellText := 'unknown';
      end;
    finally
      FreeAndNil(StackEntry);
    end;
  end;
end;

procedure TMainForm.vstThreadsCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  SyncNodes(Sender, Node);
end;

procedure TMainForm.vstThreadsColumnResize(Sender: TVTHeader; Column: TColumnIndex);
var
  W: Integer;
  I: Integer;
  C: TVirtualTreeColumn;
begin
  W := 0;
  for I := 0 to Sender.Columns.Count - 1 do
  begin
    C := Sender.Columns[I];
    if coVisible in C.Options then
      Inc(W, C.Width);
  end;

  Sender.Treeview.ClientWidth := W;
end;

procedure TMainForm.vstThreadsDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PLinkData;
  ThData: PThreadData;
  ProcData: PProcessData;
begin
  Data := Sender.GetNodeData(Node);
  case Data^.LinkType of
    ltProcess:
      begin
        ProcData := Data^.ProcessData;
        if (ProcData <> nil) and (ProcData^.State = psActive) then
          TargetCanvas.Font.Style := [fsBold];
      end;
    ltThread:
      begin
        ThData := Data^.ThreadData;
        if (ThData <> nil) and (ThData^.State = tsActive) then
          TargetCanvas.Font.Style := [fsBold];
      end;
  end;
end;

procedure TMainForm.vstThreadsExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  SyncNodes(Sender, Node);
end;

procedure TMainForm.vstThreadsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TLinkData);
end;

function TMainForm.ElapsedToTime(const Elapsed: UInt64): String;
begin
  Result := FormatDateTime('nn:ss.zzz', Int64ToDateTime(Elapsed));
end;

procedure TMainForm.vstThreadsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PLinkData;
  ThData: PThreadData;
  ProcData: PProcessData;
begin
  Data := Sender.GetNodeData(Node);
  case Data^.LinkType of
    ltProcess:
      begin
        ProcData := Data^.ProcessData;
        if ProcData <> nil then
          case Column of
            0: CellText := ExtractFileName(gvProjectOptions.ApplicationName);
            1: CellText := ProcessIDToStr(ProcData^.ProcessID);
            2: CellText := ElapsedToTime(ProcData^.CPUTime);
          end;
      end;
    ltThread:
      begin
        ThData := Data^.ThreadData;
        if ThData <> nil then
          case Column of
            0: CellText := ThData^.ThreadAdvInfo^.AsString;
            1: CellText := ThreadIDToStr(ThData^.ThreadID);
            2: CellText := ElapsedToTime(ThData^.CPUTime);
          end;
      end;
  end;
end;

procedure TMainForm.vstThreadsScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
begin
  if DeltaY <> 0 then
    vdtTimeLine.OffsetY := vstThreads.OffsetY;
end;

procedure TMainForm.vstTrackFuncChildsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PLinkData;
  TrackCallFuncInfo: PCallFuncInfo;
  TrackFuncInfo: TCodeTrackFuncInfo;
begin
  CellText := ' ';

  Data := vstTrackFuncChilds.GetNodeData(Node);
  case Data^.LinkType of
    ltDbgUnitInfo:
      begin
        case Column of
          0: CellText := Data^.DbgUnitInfo.ShortName;
        end;
      end;
    ltTrackFuncInfo:
      begin
        TrackFuncInfo := TCodeTrackFuncInfo(Data^.TrackFuncInfo);
        case Column of
          0: CellText := TFuncInfo(TrackFuncInfo.FuncInfo).ShortName;
          2: CellText := IntToStr(TrackFuncInfo.CallCount);
          3: CellText := ElapsedTimeToStr(vstTrackFuncChilds, Data, TrackFuncInfo.CPUElapsed);
        end;
      end;
    ltTrackCallFuncInfo:
      begin
        TrackCallFuncInfo := Data^.TrackCallFuncInfo;
        case Column of
          0: CellText := TFuncInfo(TrackCallFuncInfo^.FuncInfo).ShortName;
          1: CellText := IntToStr(TrackCallFuncInfo^.LineNo);
          2: CellText := IntToStr(TrackCallFuncInfo^.CallCount);
          3: CellText := ElapsedTimeToStr(vstTrackFuncChilds, Data, TrackCallFuncInfo.Elapsed);
        end;
      end;
  end;
end;

procedure TMainForm.vstTrackFuncParentDblClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PLinkData;
  FuncInfo: TFuncInfo;
  FuncNode: PVirtualNode;
  LineNo: Cardinal;
begin
  Node := vstTrackFuncParent.FocusedNode;
  if Assigned(Node) then
  begin
    Data := vstTrackFuncParent.GetNodeData(Node);
    if Data^.LinkType = ltTrackCallFuncInfo then
    begin
      FuncInfo := TFuncInfo(Data^.TrackCallFuncInfo^.FuncInfo);
      if Assigned(FuncInfo) then
      begin
        LineNo := Data^.TrackCallFuncInfo^.LineNo;

        FuncNode := FindTrackFuncNode(vstTrackFuncs, FuncInfo);
        if Assigned(FuncNode) then
        begin
          vstTrackFuncs.ClearSelection;
          vstTrackFuncs.FocusedNode := FuncNode;
          vstTrackFuncs.Selected[FuncNode] := True;

          if LineNo <> 0 then
            LoadFunctionSource(svfTrackFuncAdvSource, FuncInfo, LineNo);
        end;
      end;
    end;
  end;
end;

procedure TMainForm.vstTrackFuncLinksDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PLinkData;
begin
  Data := Sender.GetNodeData(Node);
  case Data^.LinkType of
    ltTrackFuncInfo, ltDbgUnitInfo, ltTrackUnitInfo:
      TargetCanvas.Font.Style := [fsBold];
  end;
end;

procedure TMainForm.vstTrackFuncChildsDblClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PLinkData;
  FuncInfo: TFuncInfo;
  FuncNode: PVirtualNode;
begin
  Node := vstTrackFuncChilds.FocusedNode;
  if Assigned(Node) then
  begin
    Data := vstTrackFuncChilds.GetNodeData(Node);
    if Data^.LinkType = ltTrackCallFuncInfo then
    begin
      FuncInfo := TFuncInfo(Data^.TrackCallFuncInfo^.FuncInfo);
      if Assigned(FuncInfo) then
      begin
        FuncNode := FindTrackFuncNode(vstTrackFuncs, FuncInfo);
        if Assigned(FuncNode) then
        begin
          vstTrackFuncs.ClearSelection;
          vstTrackFuncs.FocusedNode := FuncNode;
          vstTrackFuncs.Selected[FuncNode] := True;
        end;
      end;
    end;
  end;
end;

Function TMainForm.ElapsedTimeToStr(Tree: TBaseVirtualTree; Data: PLinkData; const Elapsed: UInt64): String;
var
  SyncNode: PVirtualNode;
  SyncData: PLinkData;

  ThData: PThreadData;
  ProcData: PProcessData;
begin
  Result := ' ';

  SyncNode := Data^.SyncNode;
  SyncData := Tree.GetNodeData(SyncNode);

  if SyncData^.LinkType = ltTrackFuncInfo then
  begin
    SyncNode := SyncData^.SyncNode;
    SyncData := Tree.GetNodeData(SyncNode);

    case SyncData^.LinkType of
      ltThread:
        begin
          ThData := SyncData^.ThreadData;
          Result := FuncElapsedToTime(ThData^.CPUTime, ThData^.CPUElapsed, Elapsed);
        end;
      ltProcess:
        begin
          ProcData := SyncData^.ProcessData;
          Result := FuncElapsedToTime(ProcData^.CPUTime, ProcData^.CPUElapsed, Elapsed);
        end;
    end;
  end;
end;

procedure TMainForm.vstTrackFuncParentGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PLinkData;
  TrackCallFuncInfo: PCallFuncInfo;
  TrackFuncInfo: TCodeTrackFuncInfo;
begin
  CellText := ' ';

  Data := vstTrackFuncParent.GetNodeData(Node);
  case Data^.LinkType of
    ltDbgUnitInfo:
      begin
        case Column of
          0: CellText := Data^.DbgUnitInfo.ShortName;
        end;
      end;
    ltTrackFuncInfo:
      begin
        TrackFuncInfo := TCodeTrackFuncInfo(Data^.TrackFuncInfo);
        case Column of
          0: CellText := TFuncInfo(TrackFuncInfo.FuncInfo).ShortName;
          2: CellText := IntToStr(TrackFuncInfo.CallCount);
          3: CellText := ElapsedTimeToStr(vstTrackFuncParent, Data, TrackFuncInfo.CPUElapsed);
        end;
      end;
    ltTrackCallFuncInfo:
      begin
        TrackCallFuncInfo := Data^.TrackCallFuncInfo;
        case Column of
          0: CellText := TFuncInfo(TrackCallFuncInfo^.FuncInfo).ShortName;
          1: CellText := IntToStr(TrackCallFuncInfo^.LineNo);
          2: CellText := IntToStr(TrackCallFuncInfo^.CallCount);
          3: CellText := ElapsedTimeToStr(vstTrackFuncParent, Data, TrackCallFuncInfo.Elapsed);
        end;
      end;
  end;
end;

procedure TMainForm.vstTrackFuncsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PLinkData;
  Value1, Value2: UInt64;
  Name1, Name2: String;
begin
  Data1 := vstTrackFuncs.GetNodeData(Node1);
  Data2 := vstTrackFuncs.GetNodeData(Node2);

  case Column of
    0:
      begin
        Name1 := '';
        Name2 := '';

        if (Data1^.LinkType = ltTrackFuncInfo) and (Data2^.LinkType = ltTrackFuncInfo) then
        begin
          Name1 := TFuncInfo(Data1^.TrackFuncInfo.FuncInfo).ShortName;
          Name2 := TFuncInfo(Data2^.TrackFuncInfo.FuncInfo).ShortName;
        end
        else
        if (Data1^.LinkType = ltTrackUnitInfo) and (Data2^.LinkType = ltTrackUnitInfo) then
        begin
          Name1 := TUnitInfo(Data1^.TrackUnitInfo.UnitInfo).ShortName;
          Name2 := TUnitInfo(Data2^.TrackUnitInfo.UnitInfo).ShortName;
        end;

        Result := CompareText(Name1, Name2);
      end;
    1:
      begin
        Value1 := 0;
        Value2 := 0;

        if (Data1^.LinkType = ltTrackFuncInfo) and (Data2^.LinkType = ltTrackFuncInfo) then
        begin
          Value1 := Data1^.TrackFuncInfo.CallCount;
          Value2 := Data2^.TrackFuncInfo.CallCount;
        end
        else
        if (Data1^.LinkType = ltTrackUnitInfo) and (Data2^.LinkType = ltTrackUnitInfo) then
        begin
          // Для юнитов считаем по кол-ву вызовов функций
          Value1 := Data1^.TrackUnitInfo.CallCount;
          Value2 := Data2^.TrackUnitInfo.CallCount;
        end;

        Result := Compare(Value1, Value2);
      end;
    2:
      begin
        Value1 := 0;
        Value2 := 0;

        if (Data1^.LinkType = ltTrackFuncInfo) and (Data2^.LinkType = ltTrackFuncInfo) then
        begin
          Value1 := TCodeTrackFuncInfo(Data1^.TrackFuncInfo).CPUElapsed;
          Value2 := TCodeTrackFuncInfo(Data2^.TrackFuncInfo).CPUElapsed;
        end
        else
        if (Data1^.LinkType = ltTrackUnitInfo) and (Data2^.LinkType = ltTrackUnitInfo) then
        begin
          // Для юнитов считаем по кол-ву вызовов функций
          Value1 := Data1^.TrackUnitInfo.CallCount;
          Value2 := Data2^.TrackUnitInfo.CallCount;
        end;

        Result := Compare(Value1, Value2);
      end;
  end;
end;

procedure TMainForm.vstTrackFuncsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PLinkData;
begin
  Data := Sender.GetNodeData(Node);
  case Data^.LinkType of
    ltProcess, ltThread, ltTrackUnitInfo:
      TargetCanvas.Font.Style := [fsBold];
  end;
end;

procedure TMainForm.vstTrackFuncsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;
begin
  Data := vstTrackFuncs.GetNodeData(Node);
  case Data^.LinkType of
    ltTrackFuncInfo:
      begin
        LoadTrackParentFunctions(Data^.TrackFuncInfo, Node);
        LoadTrackChildFunctions(Data^.TrackFuncInfo, Node);

        LoadFunctionSource(svfTrackFuncAdvSource, TFuncInfo(Data^.TrackFuncInfo.FuncInfo));
      end;
  else
    begin
      vstTrackFuncParent.Clear;
      vstTrackFuncChilds.Clear;
      svfTrackFuncAdvSource.Clear;
    end;
  end;
end;

procedure TMainForm.vstTrackFuncsFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
var
  Data: PLinkData;
begin
  if Assigned(OldNode) then
  begin
    Data := vstTrackFuncs.GetNodeData(OldNode);
    case Data^.LinkType of
      ltTrackFuncInfo:
        AddTrackHistory(Data^.TrackFuncInfo);
    end;
  end;
end;

procedure TMainForm.vstTrackFuncsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PLinkData;
  SyncData: PLinkData;
  ThData: PThreadData;
  ProcData: PProcessData;
  TrackFuncInfo: TCodeTrackFuncInfo;
  TrackUnitInfo: TTrackUnitInfo;
begin
  CellText := ' ';
  Data := vstTrackFuncs.GetNodeData(Node);
  case Data^.LinkType of
    ltProcess:
      begin
        ProcData := Data^.ProcessData;
        case Column of
          0: CellText := ExtractFileName(gvProjectOptions.ApplicationName);
          1: CellText := IntToStr(ProcData^.DbgTrackEventCount);
          2: CellText := ElapsedToTime(ProcData^.CPUTime);
        end;
      end;
    ltThread:
      begin
        ThData := Data^.ThreadData;
        case Column of
          0: CellText := ThData^.ThreadAdvInfo^.AsString;
          1: CellText := IntToStr(ThData^.DbgTrackEventCount);
          2: CellText := ElapsedToTime(ThData^.CPUTime);
        end;
      end;
    ltTrackFuncInfo:
      begin
        TrackFuncInfo := TCodeTrackFuncInfo(Data^.TrackFuncInfo);
        case Column of
          0: CellText := TFuncInfo(TrackFuncInfo.FuncInfo).ShortName;
          1: CellText := IntToStr(TrackFuncInfo.CallCount);
          2:
            begin
              SyncData := vstTrackFuncs.GetNodeData(Data^.SyncNode);
              case SyncData^.LinkType of
                ltThread:
                  begin
                    ThData := SyncData^.ThreadData;
                    CellText := FuncElapsedToTime(ThData^.CPUTime, ThData^.CPUElapsed, TrackFuncInfo.CPUElapsed);
                  end;
                ltProcess:
                  begin
                    ProcData := SyncData^.ProcessData;
                    CellText := FuncElapsedToTime(ProcData^.CPUTime, ProcData^.CPUElapsed, TrackFuncInfo.CPUElapsed);
                  end;
              end;
            end;
        end;
      end;
    ltTrackUnitInfo:
      begin
        TrackUnitInfo := Data^.TrackUnitInfo;
        case Column of
          0: CellText := TUnitInfo(TrackUnitInfo.UnitInfo).ShortName;
          1: CellText := IntToStr(TrackUnitInfo.CallCount);
          (*
          2:
            begin
              SyncData := vstTrackFuncs.GetNodeData(Data^.SyncNode);
              case SyncData^.LinkType of
                ltThread:
                  begin
                    ThData := SyncData^.ThreadData;
                    CellText := FuncElapsedToTime(ThData^.CPUTime, ThData^.Elapsed, TrackUnitInfo.Elapsed);
                  end;
                ltProcess:
                  begin
                    ProcData := SyncData^.ProcessData;
                    CellText := FuncElapsedToTime(ProcData^.CPUTime, ProcData^.Elapsed, TrackUnitInfo.Elapsed);
                  end;
              end;
            end;
          *)
        end;
      end;
  end;
end;

procedure TMainForm.vstTrackThreadsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;
begin
  if Node = nil then Exit;
  
  Data := vstTrackThreads.GetNodeData(Node);
  case Data^.LinkType of
    ltProcess:
      LoadTrackProcessFunctions(Data^.ProcessData, Node);
    ltThread:
      LoadTrackThreadFunctions(Data^.ThreadData, Node);
  end;
end;

procedure TMainForm.vstTrackThreadsFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  ClearTrackHistoryList;
end;

procedure TMainForm.vstTrackThreadsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;
  ThData: PThreadData;
  ProcData: PProcessData;
begin
  Data := Sender.GetNodeData(Node);
  case Data^.LinkType of
    ltProcess:
      begin
        ProcData := Data^.ProcessData;
        if ProcData <> nil then
          case Column of
            0: CellText := ExtractFileName(gvProjectOptions.ApplicationName);
            1: CellText := ProcessIDToStr(ProcData^.ProcessID);
            2: CellText := IntToStr(ProcData^.DbgTrackEventCount);
            3: CellText := ElapsedToTime(ProcData^.CPUTime);
          end;
      end;
    ltThread:
      begin
        ThData := Data^.ThreadData;
        if ThData <> nil then
          case Column of
            0: CellText := ThData^.ThreadAdvInfo^.AsString;
            1: CellText := ThreadIDToStr(ThData^.ThreadID);
            2: CellText := IntToStr(ThData^.DbgTrackEventCount);
            3: CellText := ElapsedToTime(ThData^.CPUTime);
          end;
      end;
  end;
end;

procedure TMainForm.vstUpdateInfoDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: PLinkData;
begin
  Data := vstUpdateInfo.GetNodeData(Node);
  case Data^.LinkType of
    ltSpiderInfo:
      TargetCanvas.Font.Style := [fsBold];
    ltVersionInfo:
      TargetCanvas.Font.Style := [fsBold];
  end;
end;

procedure TMainForm.vstUpdateInfoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PLinkData;
begin
  CellText := ' ';

  Data := vstUpdateInfo.GetNodeData(Node);
  case Data^.LinkType of
    ltSpiderInfo:
      CellText := Format('Current version: %s; Last version: %s', [gvUpdateInfo.CurrentVersion, gvUpdateInfo.LastVersion]);
    ltVersionInfo:
      CellText := Format('Changes for version: %s (%s)', [Data^.VersionInfo.Version, Data^.VersionInfo.Date]);
    ltChangeLogItemInfo:
      CellText := Format('%s: %s', [TChangeLogItem.ItemTypeAsStr(Data^.ChangeLogItem.ItemType), Data^.ChangeLogItem.ItemText]);
  end;
end;

procedure TMainForm.WMClose(var Message: TWMClose);
begin
  FCloseApp := True;
  inherited;
end;

end.
