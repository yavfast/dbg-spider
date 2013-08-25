unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Mask, ExtCtrls, ComCtrls, ActnList, DebugInfo,
  Grids, JvExGrids, JvGrids, JvgStringGrid, VirtualTrees, JvComponentBase,
  JvExExtCtrls, JvExtComponent, JvSplit,
  Debuger, DebugerTypes, DelphiDebugInfo, JvExComCtrls, JvExControls,
  JvEditorCommon, JvUnicodeEditor, JvUnicodeHLEditor,
  PlatformDefaultStyleActnCtrls, ActnMan, Ribbon, RibbonLunaStyleActnCtrls,
  RibbonSilverStyleActnCtrls, ToolWin, ActnCtrls, ActnMenus,
  RibbonActnMenus, ImgList, JvImageList, ActnColorMaps, XPMan,
  uActionController;

type
  TProgectType = (ptEmpty, ptSpider, ptApplication);

  TLinkType = (ltProject, ltProcess, ltThread, ltMemInfo, ltMemStack, ltExceptInfo, ltExceptStack,
    ltDbgUnitInfo, ltDbgConstInfo, ltDbgTypeInfo, ltDbgVarInfo, ltDbgFuncInfo, ltDbgStructMemberInfo,
    ltDbgFuncParamInfo, ltDbgLogItem);

  PLinkData = ^TLinkData;
  TLinkData = record
    SyncNode: PVirtualNode;
    case LinkType: TLinkType of
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
  end;

  TCheckFunc = function(LinkData: PLinkData; CmpData: Pointer): Boolean;

  TMainForm = class(TForm)
    AL: TActionList;
    acAppOpen: TAction;
    acRun: TAction;
    acStop: TAction;
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
    pnl1: TPanel;
    vstMemList: TVirtualStringTree;
    vstMemStack: TVirtualStringTree;
    tsExceptions: TTabSheet;
    vstExceptionThreads: TVirtualStringTree;
    pnl2: TPanel;
    vstExceptionList: TVirtualStringTree;
    vstExceptionCallStack: TVirtualStringTree;
    amMain: TActionManager;
    rbnMain: TRibbon;
    rbpMain: TRibbonPage;
    rbambMain: TRibbonApplicationMenuBar;
    rbgProject: TRibbonGroup;
    rbqtbMain: TRibbonQuickAccessToolbar;
    acNewProject: TAction;
    acOpenProject: TAction;
    acCloseProject: TAction;
    acPause: TAction;
    acOptions: TAction;
    acExit: TAction;
    acAbout: TAction;
    imlMain: TJvImageList;
    imlMainSmall: TJvImageList;
    rbgApplication: TRibbonGroup;
    rbngrpDebug: TRibbonGroup;
    acSave: TAction;
    acSaveCopy: TAction;
    rbngrpTimeLineSettings: TRibbonGroup;
    acCPUTimeLine: TAction;
    acRealTimeLine: TAction;
    acRunStop: TAction;
    scm1: TStandardColorMap;
    cbMainTabs: TCoolBar;
    actbMainTabs: TActionToolBar;
    xpmnfst1: TXPManifest;
    acTabDebugInfo: TAction;
    acTabTimeline: TAction;
    acTabMemoryInfo: TAction;
    acTabExceptions: TAction;
    acTabLog: TAction;
    cbStatusInfo: TCoolBar;
    actbStatusInfo: TActionToolBar;
    acStatusDebuger: TAction;
    acStatusDbgInfo: TAction;
    acStausEventCount: TAction;
    tsDebugInfo2: TTabSheet;
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
    vstDbgInfoFuncVars: TVirtualStringTree;
    actbStatusInfo2: TActionToolBar;
    pbProgress: TProgressBar;
    pStatusAction: TPanel;
    vstLog: TVirtualStringTree;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure acAppOpenExecute(Sender: TObject);
    procedure acAttachProcessExecute(Sender: TObject);
    procedure acRunExecute(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure acDebugInfoExecute(Sender: TObject);
    procedure acOptionsExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acCPUTimeLineExecute(Sender: TObject);
    procedure acRealTimeLineExecute(Sender: TObject);
    procedure acMainTabExecute(Sender: TObject);

    procedure tmrThreadsUpdateTimer(Sender: TObject);
    procedure cbCPUTimeLineClick(Sender: TObject);

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

    procedure vstMemStackGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure vstExceptionThreadsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstExceptionThreadsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

    procedure vstExceptionListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstExceptionListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

    procedure vstExceptionCallStackGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure vstDbgInfoUnitsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstDbgInfoUnitsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

    procedure vstDbgInfoConstsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure vstDbgInfoTypesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure vstDbgInfoVarsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure vstDbgInfoFunctionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstDbgInfoFunctionsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

    procedure vstDbgInfoFuncVarsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure vstLogGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure vstLogResize(Sender: TObject);
    procedure vstLogColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    procedure acOpenProjectExecute(Sender: TObject);
    procedure acCloseProjectExecute(Sender: TObject);
  private
    FProjectFileName: String;
    FProjectType: TProgectType;

    FPID: DWORD;

    FCloseApp: Boolean;
    procedure WMClose(var Message: TWMClose); message WM_CLOSE;

    procedure SetProjectName(const Name: String);

    procedure ProgressAction(const Action: String; const Progress: Integer);

    function GetLineTimeOffset: Cardinal;

    procedure HidePCTabs(PC: TPageControl);

    procedure ClearProject;
    procedure ClearTrees;
    procedure ClearDbgTrees;
    procedure ClearDbgInfoTrees;

    procedure UpdateTrees;
    procedure UpdateStatusInfo;
    procedure UpdateLog;
    procedure InitLog(const RootMsg: String);

    procedure UpdateActions;

    procedure LoadUnits;
    procedure LoadConsts(UnitInfo: TUnitInfo; UnitNode: PVirtualNode);
    procedure LoadTypes(UnitInfo: TUnitInfo; UnitNode: PVirtualNode);
    procedure LoadVars(UnitInfo: TUnitInfo; UnitNode: PVirtualNode);
    procedure LoadFunctions(UnitInfo: TUnitInfo; UnitNode: PVirtualNode);
    procedure LoadFunctionParams(FuncInfo: TFuncInfo; FuncNode: PVirtualNode);

    procedure DrawTimeLineHeader(C: TCanvas; const R: TRect; const Offset: Integer);
    procedure DrawThreadTimeLine(C: TCanvas; const R: TRect; ThData: PThreadData; const CurOffset: Cardinal);
    procedure DrawThreadCPUTimeLine(C: TCanvas; const R: TRect; ThData: PThreadData; const CurOffset: Cardinal);
    procedure DrawProcessTimeLine(C: TCanvas; const R: TRect; ProcData: PProcessData; const CurOffset: Cardinal);
    procedure DrawProcessCPUTimeLine(C: TCanvas; const R: TRect; ProcData: PProcessData; const CurOffset: Cardinal);
    procedure DrawBackground(TargetCanvas: TCanvas; const R: TRect; BkColor: TColor);

    procedure AddProcess(const ProcessID: Cardinal);
    procedure AddThread(const ThreadID: Cardinal);
    procedure SyncNodes(Tree: TBaseVirtualTree; Node: PVirtualNode);

    function EllapsedToTime(const Ellapsed: UInt64): String;

    function FindThreadNode(vTree: TBaseVirtualTree; ThData: PThreadData): PVirtualNode;
    function FindThreadNodeById(vTree: TBaseVirtualTree; const ThreadId: TThreadId): PVirtualNode;
    function FindNode(vTree: TBaseVirtualTree; Node: PVirtualNode; CheckFunc: TCheckFunc; CmpData: Pointer): PVirtualNode;
  public
    procedure Log(const Msg: String);
    procedure DoAction(Action: TacAction; const Args: array of Variant);
    procedure ViewDebugInfo(DebugInfo: TDebugInfo);
  end;

var
  MainForm: TMainForm = nil;

implementation

{$R *.dfm}

uses Math, EvaluateTypes, ClassUtils, uProcessList, uDebugerThread;


type
  THookBaseVirtualTree = class(TBaseVirtualTree);

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

procedure TMainForm.acCPUTimeLineExecute(Sender: TObject);
begin
  UpdateTrees;
end;

procedure TMainForm.acDebugInfoExecute(Sender: TObject);
begin
  _AC.RunDebug(FProjectFileName, [doDebugInfo], FPID);
end;

procedure TMainForm.acExitExecute(Sender: TObject);
begin
  FCloseApp := True;
  Close;
end;

procedure TMainForm.acOpenProjectExecute(Sender: TObject);
begin
  if OD.Execute then
    SetProjectName(OD.FileName);
end;

procedure TMainForm.acOptionsExecute(Sender: TObject);
begin
  //
end;

procedure TMainForm.acRealTimeLineExecute(Sender: TObject);
begin
  UpdateTrees;
end;

procedure TMainForm.acRunExecute(Sender: TObject);
begin
  acRun.Enabled := False;

  ClearDbgTrees;

  _AC.RunDebug(FProjectFileName, [{doDebugInfo, }doRun, doProfiler], FPID);
end;

procedure TMainForm.acStopExecute(Sender: TObject);
begin
  acStop.Enabled := False;

  _AC.StopDebug;
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

  pcMain.ActivePageIndex := CurTag;
end;

procedure TMainForm.AddProcess(const ProcessID: Cardinal);
var
  LinkData: PLinkData;
  NameNode: PVirtualNode;
  TimeLineNode: PVirtualNode;
begin
  // Threads timeline
  NameNode := vstThreads.AddChild(Nil);
  TimeLineNode := vdtTimeLine.AddChild(Nil);

  LinkData := vstThreads.GetNodeData(NameNode);
  LinkData^.SyncNode := TimeLineNode;
  LinkData^.LinkType := ltProcess;
  LinkData^.ProcessData := @gvDebuger.ProcessData;

  LinkData := vdtTimeLine.GetNodeData(TimeLineNode);
  LinkData^.SyncNode := NameNode;
  LinkData^.LinkType := ltProcess;
  LinkData^.ProcessData := @gvDebuger.ProcessData;

  // Memory Info
  NameNode := vstMemInfoThreads.AddChild(Nil);
  LinkData := vstMemInfoThreads.GetNodeData(NameNode);
  LinkData^.SyncNode := nil;
  LinkData^.LinkType := ltProcess;
  LinkData^.ProcessData := @gvDebuger.ProcessData;

  // Exceptions
  NameNode := vstExceptionThreads.AddChild(Nil);
  LinkData := vstExceptionThreads.GetNodeData(NameNode);
  LinkData^.SyncNode := nil;
  LinkData^.LinkType := ltProcess;
  LinkData^.ProcessData := @gvDebuger.ProcessData;
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

procedure TMainForm.AddThread(const ThreadID: Cardinal);
var
  LinkData: PLinkData;
  NameNode: PVirtualNode;
  TimeLineNode: PVirtualNode;
  ThData: PThreadData;
  ParentThData: PThreadData;
  ParentId: Cardinal;
  ParentNode: PVirtualNode;
  CurNode: PVirtualNode;
begin
  vstThreads.BeginUpdate;
  vdtTimeLine.BeginUpdate;
  CurNode := vstThreads.FocusedNode;
  try
    ThData := gvDebuger.GetThreadData(ThreadID);

    if ThData = nil then Exit;

    ParentId := ThData^.ThreadAdvInfo^.ThreadParentId;

    // Threads timeline
    ParentNode := Nil;
    if ParentId <> 0 then
    begin
      ParentThData := gvDebuger.GetThreadData(ParentId);

      if ParentThData = nil then
        // ≈сли родительский поток завершилс€ раньше старта дочернего
        ParentNode := FindThreadNodeById(vstThreads, ParentId)
      else
        ParentNode := FindThreadNode(vstThreads, ParentThData);
    end;

    if ParentNode = Nil then
      ParentNode := vstThreads.RootNode^.FirstChild;

    NameNode := vstThreads.AddChild(ParentNode);
    vstThreads.Expanded[ParentNode] := True;

    LinkData := vstThreads.GetNodeData(ParentNode);
    ParentNode := LinkData^.SyncNode;

    TimeLineNode := vdtTimeLine.AddChild(ParentNode);
    vdtTimeLine.Expanded[ParentNode] := True;

    LinkData := vstThreads.GetNodeData(NameNode);
    LinkData^.SyncNode := TimeLineNode;
    LinkData^.LinkType := ltThread;
    LinkData^.ThreadData := ThData;

    LinkData := vdtTimeLine.GetNodeData(TimeLineNode);
    LinkData^.SyncNode := NameNode;
    LinkData^.LinkType := ltThread;
    LinkData^.ThreadData := ThData;
  finally
    vstThreads.FocusedNode := CurNode;
    vstThreads.EndUpdate;
    vdtTimeLine.EndUpdate;
  end;

  // Memory Info
  vstMemInfoThreads.BeginUpdate;
  CurNode := vstMemInfoThreads.FocusedNode;
  try
    ParentNode := Nil;
    if ParentId <> 0 then
    begin
      if ParentThData = nil then
        // ≈сли родительский поток завершилс€ раньше старта дочернего
        ParentNode := FindThreadNodeById(vstMemInfoThreads, ParentId)
      else
        ParentNode := FindThreadNode(vstMemInfoThreads, ParentThData);
    end;

    if ParentNode = Nil then
      ParentNode := vstMemInfoThreads.RootNode^.FirstChild;

    NameNode := vstMemInfoThreads.AddChild(ParentNode);
    vstMemInfoThreads.Expanded[ParentNode] := True;

    LinkData := vstMemInfoThreads.GetNodeData(NameNode);
    LinkData^.SyncNode := nil;
    LinkData^.LinkType := ltThread;
    LinkData^.ThreadData := ThData;
  finally
    vstMemInfoThreads.FocusedNode := CurNode;
    vstMemInfoThreads.EndUpdate;
  end;

  // Exceptions
  vstExceptionThreads.BeginUpdate;
  CurNode := vstExceptionThreads.FocusedNode;
  try
    ParentNode := Nil;
    if ParentId <> 0 then
    begin
      if ParentThData = nil then
        // ≈сли родительский поток завершилс€ раньше старта дочернего
        ParentNode := FindThreadNodeById(vstExceptionThreads, ParentId)
      else
        ParentNode := FindThreadNode(vstExceptionThreads, ParentThData);
    end;

    if ParentNode = Nil then
      ParentNode := vstExceptionThreads.RootNode^.FirstChild;

    NameNode := vstExceptionThreads.AddChild(ParentNode);
    vstExceptionThreads.Expanded[ParentNode] := True;

    LinkData := vstExceptionThreads.GetNodeData(NameNode);
    LinkData^.SyncNode := nil;
    LinkData^.LinkType := ltThread;
    LinkData^.ThreadData := ThData;
  finally
    vstExceptionThreads.FocusedNode := CurNode;
    vstExceptionThreads.EndUpdate;
  end;
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
end;

procedure TMainForm.ClearDbgTrees;
begin
  vstThreads.Clear;
  vdtTimeLine.Clear;

  vstMemInfoThreads.Clear;
  vstMemList.Clear;
  vstMemStack.Clear;

  vstExceptionThreads.Clear;
  vstExceptionList.Clear;
  vstExceptionCallStack.Clear;
end;

procedure TMainForm.ClearProject;
begin
  FProjectFileName := '';
  FProjectType := ptEmpty;
  rbnMain.Caption := 'Empty';

  ClearTrees;
  UpdateStatusInfo;
  UpdateActions;
end;

procedure TMainForm.ClearTrees;
begin
  vstLog.Clear;

  ClearDbgTrees;
  ClearDbgInfoTrees;
end;

procedure TMainForm.DoAction(Action: TacAction; const Args: array of Variant);
begin
  if not Visible then Exit;

  case Action of
    acRunEnabled:
    begin
      acRun.Enabled := Args[0];
    end;
    acStopEnabled:
    begin
      acStop.Enabled := Args[0];

      tmrThreadsUpdate.Enabled := acStop.Enabled;
    end;
    acAddThread:
      AddThread(Args[0]);
    acCreateProcess:
      AddProcess(Args[0]);
    acProgress:
      ProgressAction(Args[0], Args[1]);
  end;

  case Action of
    acRunEnabled, acStopEnabled:
      if acStop.Enabled then
        acRunStop.Assign(acStop)
      else
        acRunStop.Assign(acRun);
  end;

  UpdateStatusInfo;

  if not tmrThreadsUpdate.Enabled then
    UpdateTrees;
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

procedure TMainForm.DrawProcessCPUTimeLine(C: TCanvas; const R: TRect; ProcData: PProcessData; const CurOffset: Cardinal);
var
  X1, X2, Y1, Y2: Integer;
  T1, T2: Int64;
  I: Cardinal;
  ProcPoint: PProcessPoint;
begin
  DrawBackground(C, R, C.Brush.Color);

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

  C.Brush.Color := clScrollBar;
  C.FillRect(Rect(X1, Y1, X2, Y2));

  C.Pen.Style := psSolid;

  if ProcData^.DbgPointsCount > 0 then
  begin
    I := 0;
    While I < ProcData^.DbgPointsCount do
    begin
      try
        ProcPoint := ProcData^.DbgPointByIdx(I);
        if ((ProcPoint^.PointType = ptPerfomance) and (ProcPoint^.DeltaTime > 0)) or
          (ProcPoint^.PointType = ptException) then
        begin
          T1 := I;
          X1 := R.Left + Integer(T1 - CurOffset) - 1;
          if (X1 < R.Left) then Continue;
          if (X1 > R.Right) then Continue;

          case ProcPoint^.PointType of
            ptPerfomance:
              C.Pen.Color := clGreen;
            ptException:
              C.Pen.Color := clRed;
            ptThreadInfo:
              C.Pen.Color := clGreen;
          end;

          C.MoveTo(X1, Y1);
          C.LineTo(X1, Y2);
        end;
      finally
        Inc(I);
      end;
    end;
  end;
end;

procedure TMainForm.DrawProcessTimeLine(C: TCanvas; const R: TRect; ProcData: PProcessData; const CurOffset: Cardinal);
var
  X1, X2, Y1, Y2: Integer;
  T1, T2, F: Int64;
  OffsetT1, OffsetT2, Offset: Cardinal;
  I: Cardinal;
  ProcPoint: PProcessPoint;
begin
  DrawBackground(C, R, C.Brush.Color);

  if ProcData = nil then Exit;

  T1 := 0;
  if ProcData^.State <> psActive then
    T2 := ProcData^.Ellapsed
  else
    QueryPerformanceCounter(T2);

  T2 := T2 - ProcData^.Started;

  Offset := CurOffset * _TicksPerSec;

  QueryPerformanceFrequency(F); // in 1 sec

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

  C.Brush.Color := clScrollBar;
  C.FillRect(Rect(X1, Y1, X2, Y2));

  C.Pen.Color := clGreen;
  C.Pen.Style := psSolid;

  if ProcData^.DbgPointsCount > 0 then
  begin
    I := 0;
    While I < ProcData^.DbgPointsCount do
    begin
      try
        ProcPoint := ProcData^.DbgPointByIdx(I);
        if (ProcPoint^.PointType = ptPerfomance) and (ProcPoint^.DeltaTime > 0) then
        begin
          T1 := ProcPoint^.FromStart;
          OffsetT1 := T1 div F;
          X1 := R.Left + Integer(OffsetT1 - Offset) - 1;
          if (X1 < R.Left) then
            Continue;
          if (X1 > R.Right) then
            Continue;

          C.MoveTo(X1, Y1);
          C.LineTo(X1, Y2);
        end;
      finally
        Inc(I);
      end;
    end;
  end;
end;

procedure TMainForm.DrawThreadCPUTimeLine(C: TCanvas; const R: TRect; ThData: PThreadData; const CurOffset: Cardinal);
var
  X1, X2, Y1, Y2: Integer;
  T1, T2: Int64;
  I: Cardinal;
  ThPoint: PThreadPoint;
  ProcPoint: PProcessPoint;
begin
  DrawBackground(C, R, C.Brush.Color);

  if (ThData = nil) or (ThData^.DbgPointsCount = 0) then Exit;

  T1 := ThData^.DbgPointByIdx(0)^.PerfIdx;
  if ThData^.State = tsFinished then
    T2 := ThData^.DbgPointByIdx(ThData^.DbgPointsCount - 1)^.PerfIdx
  else
    T2 := gvDebugInfo.Debuger.ProcessData.CurDbgPointIdx;

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

  C.Brush.Color := clScrollBar;
  C.Brush.Style := bsSolid;
  C.FillRect(Rect(X1, Y1, X2, Y2));

  C.Pen.Style := psSolid;

  if ThData^.DbgPointsCount > 0 then
  begin
    I := 0;
    While I < ThData^.DbgPointsCount do
    begin
      try
        ThPoint := ThData^.DbgPointByIdx(I);

        if ThPoint = nil then
          Continue;

        case ThPoint^.PointType of
          ptStart, ptStop:
            C.Pen.Color := clGreen;
          ptException:
            C.Pen.Color := clRed;
          ptPerfomance:
            C.Pen.Color := clGreen;
        end;

        T1 := ThPoint^.PerfIdx;
        X1 := R.Left + Integer(T1 - CurOffset) - 1;
        if (X1 < R.Left) then Continue;
        if (X1 > R.Right) then Continue;

        C.MoveTo(X1, Y1);
        C.LineTo(X1, Y2);
      finally
        Inc(I);
      end;
    end;
  end;
end;

procedure TMainForm.DrawThreadTimeLine(C: TCanvas; const R: TRect; ThData: PThreadData; const CurOffset: Cardinal);
var
  X1, X2, Y1, Y2: Integer;
  T1, T2, F: Int64;
  OffsetT1, OffsetT2, Offset: Cardinal;
  I: Cardinal;
  ThPoint: PThreadPoint;
  ProcPoint: PProcessPoint;
begin
  DrawBackground(C, R, C.Brush.Color);

  if ThData = nil then Exit;

  T1 := ThData^.Started;
  if ThData^.State = tsFinished then
    T2 := T1 + ThData^.Ellapsed
  else
    QueryPerformanceCounter(T2);

  T1 := T1 - gvDebugInfo.Debuger.ProcessData.Started;
  T2 := T2 - gvDebugInfo.Debuger.ProcessData.Started;

  Offset := CurOffset * _TicksPerSec;

  QueryPerformanceFrequency(F); // in 1 sec

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

  C.Brush.Color := clScrollBar;
  C.Brush.Style := bsSolid;
  C.FillRect(Rect(X1, Y1, X2, Y2));

  C.Pen.Style := psSolid;

  if ThData^.DbgPointsCount > 0 then
  begin
    I := 0;
    While I < ThData^.DbgPointsCount do
    begin
      try
        ThPoint := ThData^.DbgPointByIdx(I);

        if ThPoint = nil then
          Continue;

        ProcPoint := gvDebuger.ProcessData.DbgPointByIdx(ThPoint^.PerfIdx);

        case ThPoint^.PointType of
          ptStart, ptStop:
            C.Pen.Color := clGreen;
          ptException:
            C.Pen.Color := clRed;
          ptPerfomance:
            C.Pen.Color := clGreen;
        end;

        T1 := ProcPoint^.FromStart;
        OffsetT1 := T1 div F;
        X1 := R.Left + Integer(OffsetT1 - Offset) - 1;
        if (X1 < R.Left) then
          Continue;
        if (X1 > R.Right) then
          Continue;

        C.MoveTo(X1, Y1);
        C.LineTo(X1, Y2);
      finally
        Inc(I);
      end;
    end;
  end;
end;

procedure TMainForm.DrawTimeLineHeader(C: TCanvas; const R: TRect; const Offset: Integer);
var
  Cnt: Integer;
  X, Y: Integer;
  T: String;
  Idx: Integer;
  ProcPoint: PProcessPoint;
begin
  C.Font.Color := clWindowText;
  C.Font.Size := 8;

  C.Brush.Style := bsClear;

  C.Pen.Color := clWindowText;
  C.Pen.Style := psSolid;

  For Cnt := 0 to ((R.Right - R.Left + 1) div 100) + 1 do
  begin
    X := R.Left + Cnt * 100;
    Y := R.Bottom;

    T := '';

    if acCPUTimeLine.Checked then
    begin
      if Assigned(gvDebuger) then
      begin
        Idx := Offset + Cnt * 100;

        if Idx < gvDebuger.ProcessData.DbgPointsCount then
        begin
          ProcPoint := gvDebuger.ProcessData.DbgPointByIdx(Idx);
          T := EllapsedToTime(ProcPoint^.CPUTime);
        end;
      end;
    end
    else
      T := OffsetToTime(Offset + Cnt);

    if T <> '' then
      C.TextOut(X + 2, R.Top - 3, T);

    C.MoveTo(X - 1, Y - 1);
    C.LineTo(X - 1, Y - 8);

    C.MoveTo(X + 50, Y - 1);
    C.LineTo(X + 50, Y - 4);
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCloseApp;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FProjectFileName := '';
  FCloseApp := False;

  TThread.NameThreadForDebugging(AnsiString(ClassName), MainThreadID);

  actbMainTabs.ParentBackground := True;
  actbStatusInfo.ParentBackground := True;
  actbStatusInfo2.ParentBackground := True;

  HidePCTabs(pcMain);
  acTabLog.Execute;

  ProgressAction('', 0);

  //LoadLibrary('DbgHook32.dll'); // ƒл€ дебага этой самой DLL
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  acRunStop.Assign(acRun);

  vstThreadsColumnResize(vstThreads.Header, 0);
  vstThreadsColumnResize(vstMemInfoThreads.Header, 0);
end;

function TMainForm.GetLineTimeOffset: Cardinal;
var
  F: Double;
  PW: Integer;
begin
  Result := 0;
  if Assigned(gvDebugInfo) and Assigned(gvDebugInfo.Debuger) and
    (gvDebugInfo.Debuger.ProcessData.DbgPointsCount > 0)
  then
  begin
    PW := vdtTimeLine.Header.Columns[0].Width - vdtTimeLine.ClientWidth;
    F := (-vdtTimeLine.OffsetX) / PW;
    if acCPUTimeLine.Checked then
      Result := (Trunc(gvDebugInfo.Debuger.ProcessData.CurDbgPointIdx * F) div 100) * 100
    else
      Result := Trunc((gvDebugInfo.Debuger.ProcessData.Ellapsed_MSec div 1000) * F);
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
    Data^.LinkType := ltDbgUnitInfo;
    Data^.DbgUnitInfo := UnitInfo;

    for I := 0 to UnitInfo.Consts.Count - 1 do
    begin
      C := TConstInfo(UnitInfo.Consts[I]);

      Node := vstDbgInfoConsts.AddChild(BaseNode);
      Data := vstDbgInfoConsts.GetNodeData(Node);

      Data^.SyncNode := UnitNode;
      Data^.LinkType := ltDbgConstInfo;
      Data^.DbgConstInfo := C;
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
    Data^.LinkType := ltDbgFuncInfo;
    Data^.DbgFuncInfo := FuncInfo;

    for I := 0 to FuncInfo.Params.Count - 1 do
    begin
      V := TVarInfo(FuncInfo.Params[I]);

      Node := vstDbgInfoFuncVars.AddChild(BaseNode);
      Data := vstDbgInfoFuncVars.GetNodeData(Node);

      Data^.SyncNode := FuncNode;
      Data^.LinkType := ltDbgFuncParamInfo;
      Data^.DbgFuncParamInfo := V;
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

  if UnitInfo.Funcs.Count = 0 then Exit;

  vstDbgInfoFunctions.BeginUpdate;
  try
    BaseNode := vstDbgInfoFunctions.AddChild(nil);
    Data := vstDbgInfoFunctions.GetNodeData(BaseNode);

    Data^.SyncNode := UnitNode;
    Data^.LinkType := ltDbgUnitInfo;
    Data^.DbgUnitInfo := UnitInfo;

    for I := 0 to UnitInfo.Funcs.Count - 1 do
    begin
      F := TFuncInfo(UnitInfo.Funcs[I]);

      Node := vstDbgInfoFunctions.AddChild(BaseNode);
      Data := vstDbgInfoFunctions.GetNodeData(Node);

      Data^.SyncNode := UnitNode;
      Data^.LinkType := ltDbgFuncInfo;
      Data^.DbgFuncInfo := F;
    end;

    vstDbgInfoFunctions.Expanded[BaseNode] := True;
  finally
    vstDbgInfoFunctions.EndUpdate;
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
    Data^.LinkType := ltDbgUnitInfo;
    Data^.DbgUnitInfo := UnitInfo;

    for I := 0 to UnitInfo.Types.Count - 1 do
    begin
      T := TTypeInfo(UnitInfo.Types[I]);

      Node := vstDbgInfoTypes.AddChild(BaseNode);
      Data := vstDbgInfoTypes.GetNodeData(Node);

      Data^.SyncNode := UnitNode;
      Data^.LinkType := ltDbgTypeInfo;
      Data^.DbgTypeInfo := T;

      if Assigned(T.Members) then
      begin
        for J := 0 to T.Members.Count - 1 do
        begin
          Member := TStructMember(T.Members[J]);

          ChildNode := vstDbgInfoTypes.AddChild(Node);
          ChildData := vstDbgInfoTypes.GetNodeData(ChildNode);

          ChildData^.SyncNode := UnitNode;
          ChildData^.LinkType := ltDbgStructMemberInfo;
          ChildData^.DbgStructMemberInfo := Member;
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
  UnitNode: PVirtualNode;
  LinkData: PLinkData;
begin
  vstDbgInfoUnits.Clear;

  vstDbgInfoUnits.BeginUpdate;
  try
    for I := 0 to gvDebugInfo.Units.Count - 1 do
    begin
      UnitNode := vstDbgInfoUnits.AddChild(nil);
      LinkData := vstDbgInfoUnits.GetNodeData(UnitNode);

      LinkData^.LinkType := ltDbgUnitInfo;
      LinkData^.DbgUnitInfo := TUnitInfo(gvDebugInfo.Units.Objects[I]);
    end;
  finally
    vstDbgInfoUnits.EndUpdate;
  end;
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
    Data^.LinkType := ltDbgUnitInfo;
    Data^.DbgUnitInfo := UnitInfo;

    for I := 0 to UnitInfo.Vars.Count - 1 do
    begin
      V := TVarInfo(UnitInfo.Vars[I]);

      Node := vstDbgInfoVars.AddChild(BaseNode);
      Data := vstDbgInfoVars.GetNodeData(Node);

      Data^.SyncNode := UnitNode;
      Data^.LinkType := ltDbgVarInfo;
      Data^.DbgVarInfo := V;
    end;

    vstDbgInfoVars.Expanded[BaseNode] := True;
  finally
    vstDbgInfoVars.EndUpdate;
  end;
end;

procedure TMainForm.Log(const Msg: String);
begin
  //mLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ': ' + Msg);
end;

procedure TMainForm.ProgressAction(const Action: String; const Progress: Integer);
begin
  pbProgress.Visible := (Progress > 0);
  pbProgress.Position := Progress;

  if Action <> '' then
    //actbStatusInfo2.ActionClient.Items[1].Caption := Action
    pStatusAction.Caption := Action
  else
    //actbStatusInfo2.ActionClient.Items[1].Caption := ' ';
    pStatusAction.Caption := '';
end;

procedure TMainForm.SetProjectName(const Name: String);
var
  Ext: String;
begin
  ClearProject;

  Ext := ExtractFileExt(Name);
  if SameText(Ext, '.spider') then
    FProjectType := ptSpider
  else
  if SameText(Ext, '.exe') then
    FProjectType := ptApplication
  else
    Exit;

  rbnMain.Caption := Name;
  InitLog(Name);

  FProjectFileName := Name;
  case FProjectType of
    ptSpider:;
    ptApplication:
      _AC.RunDebug(FProjectFileName, [doDebugInfo]); // «агрузка DebugInfo
  end;

  UpdateActions;
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
  OtherTree.Selected[Data.SyncNode] := True;
  OtherTree.FocusedNode := Data.SyncNode;
end;

procedure TMainForm.tmrThreadsUpdateTimer(Sender: TObject);
begin
  tmrThreadsUpdate.Enabled := False;

  UpdateStatusInfo;

  if not Assigned(gvDebugInfo) or not Assigned(gvDebugInfo.Debuger) or
    (gvDebugInfo.Debuger.DbgState in [dsNone, dsStoped]) then Exit;

  UpdateTrees;

  tmrThreadsUpdate.Enabled := True;
end;

procedure TMainForm.UpdateActions;
begin
  if FProjectType = ptEmpty then
  begin
    acRun.Enabled := False;
    acStop.Enabled := False;
    acRunStop.Enabled := False;
    acPause.Enabled := False;

    acCloseProject.Enabled := False;
    acSave.Enabled := False;
    acSaveCopy.Enabled := False;
  end
  else
  begin
    acCloseProject.Enabled := True;
  end;
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
    InitLog(FProjectFileName);

  CurCount := vstLog.RootNode.FirstChild.ChildCount;
  LogCount := gvDebugInfo.DbgLog.Count;
  if CurCount <> LogCount then
  begin
    vstLog.BeginUpdate;
    try
      for I := CurCount to LogCount - 1 do
      begin
        Node := vstLog.AddChild(vstLog.RootNode.FirstChild);
        Data := vstLog.GetNodeData(Node);

        Data^.LinkType := ltDbgLogItem;
        Data^.DbgLogItem := gvDebugInfo.DbgLog[I];
      end;

      vstLog.Expanded[vstLog.RootNode.FirstChild] := True;
    finally
      vstLog.EndUpdate;
    end;
  end;
end;

procedure TMainForm.UpdateStatusInfo;
Const
  _DBG_INFO_IDX = 0;
  _DBG_STATE_IDX = 3;
  _DBG_EVENTS_IDX = 6;
var
  Msg: String;
begin
  if Assigned(gvDebuger) then
  begin
    if Assigned(gvDebugInfo) and (gvDebugInfo.DebugInfoLoaded) then
      actbStatusInfo.ActionClient.Items[_DBG_INFO_IDX].Caption := gvDebugInfo.DebugInfoType
    else
      actbStatusInfo.ActionClient.Items[_DBG_INFO_IDX].Caption := 'Not found';

    case gvDebuger.DbgState of
      dsNone: Msg := 'none';
      dsStarted: Msg := 'Started';
      dsWait: Msg := 'Active';
      dsPerfomance: Msg := 'Active';
      dsTrace: Msg := 'Trace';
      dsEvent: Msg := 'Active';
      dsStoping: Msg := 'Stoping';
      dsStoped: Msg := 'Stoped';
      dsDbgFail: Msg := 'Debug Fail';
      else
        Msg := '';
    end;
    actbStatusInfo.ActionClient.Items[_DBG_STATE_IDX].Caption := Msg;

    if gvDebuger.PerfomanceMode and not(gvDebuger.DbgState in [dsNone]) then
      actbStatusInfo.ActionClient.Items[_DBG_EVENTS_IDX].Caption := IntToStr(gvDebuger.ProcessData.CurDbgPointIdx)
    else
      actbStatusInfo.ActionClient.Items[_DBG_EVENTS_IDX].Caption := '0';
  end
  else
  begin
    actbStatusInfo.ActionClient.Items[_DBG_INFO_IDX].Caption := 'None';
    actbStatusInfo.ActionClient.Items[_DBG_STATE_IDX].Caption := 'None';
    actbStatusInfo.ActionClient.Items[_DBG_EVENTS_IDX].Caption := '0';
  end;
end;

procedure TMainForm.UpdateTrees;
begin
  UpdateLog;

  vstThreads.Invalidate;

  vdtTimeLine.Invalidate;
  vdtTimeLine.Header.Invalidate(nil);

  vstMemInfoThreads.Invalidate;
  vstExceptionThreads.Invalidate;
end;

procedure TMainForm.vdtTimeLineAdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
begin
  if (PaintInfo.Column <> Nil) and (PaintInfo.Column.Index = 0)then
    DrawTimeLineHeader(PaintInfo.TargetCanvas, PaintInfo.PaintRectangle, GetLineTimeOffset);
end;

procedure TMainForm.vdtTimeLineChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  SyncNodes(Sender, Node);
end;

procedure TMainForm.vdtTimeLineDrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
var
  LinkData: PLinkData;
begin
  LinkData := Sender.GetNodeData(PaintInfo.Node);
  case LinkData^.LinkType of
    ltProcess:
    begin
      if acCPUTimeLine.Checked then
        DrawProcessCPUTimeLine(PaintInfo.Canvas, PaintInfo.CellRect, LinkData^.ProcessData, GetLineTimeOffset)
      else
        DrawProcessTimeLine(PaintInfo.Canvas, PaintInfo.CellRect, LinkData^.ProcessData, GetLineTimeOffset);
    end;
    ltThread:
    begin
      if acCPUTimeLine.Checked then
        DrawThreadCPUTimeLine(PaintInfo.Canvas, PaintInfo.CellRect, LinkData^.ThreadData, GetLineTimeOffset)
      else
        DrawThreadTimeLine(PaintInfo.Canvas, PaintInfo.CellRect, LinkData^.ThreadData, GetLineTimeOffset);
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
  DrawBackground(TargetCanvas, R, clWindow);

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
          0: CellText := UnitInfo.Name;
        else
          CellText := ' ';
        end;
      end;
    ltDbgConstInfo:
      begin
        ConstInfo := Data^.DbgConstInfo;

        case Column of
          0: CellText := ConstInfo.Name;
          1: CellText := ConstInfo.ValueAsString;
          2: CellText := String(ConstInfo.TypeInfo.Name);
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
  FuncInfo := Data^.DbgFuncInfo;

  LoadFunctionParams(FuncInfo, Node);
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
          0: CellText := UnitInfo.Name;
        else
          CellText := ' ';
        end;
      end;
    ltDbgFuncInfo:
      begin
        FuncInfo := Data^.DbgFuncInfo;

        case Column of
          0: CellText := FuncInfo.Name;
          1: CellText := Format('%p', [FuncInfo.Address]);
          2: CellText := Format('%d', [FuncInfo.CodeSize]);
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
          0: CellText := FuncInfo.Name;
        else
          CellText := ' ';
        end;
      end;
    ltDbgFuncParamInfo:
      begin
        VarInfo := Data^.DbgFuncParamInfo;
        case Column of
          0: CellText := VarInfo.Name;
          1: CellText := VarInfo.DataType.Name;
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
  Data := vstDbgInfoTypes.GetNodeData(Node);
  case Data^.LinkType of
    ltDbgUnitInfo:
      begin
        UnitInfo := Data^.DbgUnitInfo;
        case Column of
          0: CellText := UnitInfo.Name;
        else
          CellText := ' ';
        end;
      end;
    ltDbgTypeInfo:
      begin
        TypeInfo := Data^.DbgTypeInfo;

        case Column of
          0: CellText := TypeInfo.Name;
          1: CellText := TypeInfo.TypeOf;
        end;
      end;
    ltDbgStructMemberInfo:
      begin
        MemberInfo := Data^.DbgStructMemberInfo;

        case Column of
          0: CellText := MemberInfo.Name;
          1: CellText := MemberInfo.DataType.Name;
        end;
      end;
  end;

end;

procedure TMainForm.vstDbgInfoUnitsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;
  UnitInfo: TUnitInfo;
begin
  Data := vstDbgInfoUnits.GetNodeData(Node);
  UnitInfo := Data^.DbgUnitInfo;

  LoadConsts(UnitInfo, Node);
  LoadTypes(UnitInfo, Node);
  LoadVars(UnitInfo, Node);
  LoadFunctions(UnitInfo, Node);
end;

procedure TMainForm.vstDbgInfoUnitsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;
  UnitInfo: TUnitInfo;
begin
  Data := vstDbgInfoUnits.GetNodeData(Node);
  UnitInfo := Data^.DbgUnitInfo;
  case Column of
    0: CellText := UnitInfo.Name;
    1: CellText := Format('%d', [UnitInfo.CodeSize]);
    2: CellText := Format('%d', [UnitInfo.DataSize]);
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
          0: CellText := UnitInfo.Name;
        else
          CellText := ' ';
        end;
      end;
    ltDbgVarInfo:
      begin
        VarInfo := Data^.DbgVarInfo;

        case Column of
          0: CellText := VarInfo.Name;
          1: CellText := VarInfo.DataType.Name;
          2: CellText := Format('%p', [Pointer(VarInfo.Offset)]);
        end;
      end;
  end;
end;

procedure TMainForm.vstExceptionCallStackGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;
  StackEntry: TStackEntry;
begin
  Data := vstExceptionCallStack.GetNodeData(Node);
  StackEntry := Data^.ExceptStackEntry;
  if StackEntry <> nil then
  begin
    case Column of
      0: CellText := StackEntry.GetInfo;
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
  Data := vstExceptionList.GetNodeData(Node);

  vstExceptionCallStack.BeginUpdate;
  try
    vstExceptionCallStack.Clear;
    Stack := Data^.ExceptInfo.Stack;
    for I := 0 to Stack.Count - 1 do
    begin
      StackNode := vstExceptionCallStack.AddChild(nil);
      StackData := vstExceptionCallStack.GetNodeData(StackNode);
      StackData^.LinkType := ltExceptStack;
      StackData^.ExceptStackEntry := TStackEntry(Stack[I]);
    end;
  finally
    vstExceptionCallStack.EndUpdate;
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
  vstExceptionCallStack.Clear;

  ExceptList := Nil;
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

  vstMemList.BeginUpdate;
  try
    if ExceptList <> Nil then
    begin
      vstExceptionList.Clear;

      L := ExceptList.LockList;
      try
        for I := 0 to L.Count - 1 do
        begin
          ExceptNode := vstExceptionList.AddChild(nil);
          Data := vstExceptionList.GetNodeData(ExceptNode);
          Data^.SyncNode := Node;
          Data^.LinkType := ltExceptInfo;
          Data^.ExceptInfo := TExceptInfo(L[I]);
        end;
      finally
        ExceptList.UnlockList;
      end;
    end;
  finally
    vstMemList.EndUpdate;
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
            0: CellText := ExtractFileName(FProjectFileName);
            1: CellText := Format('%d(%x)', [ProcData^.ProcessID, ProcData^.ProcessID]);
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
            1: CellText := Format('%d(%x)', [ThData^.ThreadID, ThData^.ThreadID]);
            2: if ThData^.DbgExceptionsCount > 0 then
                 CellText := Format('%d', [ThData^.DbgExceptionsCount]);
          end;
      end;
  end;
end;

procedure TMainForm.vstLogColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  vstLogResize(Sender);
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
          1: CellText := FProjectFileName;
        else
          CellText := 'Project:';
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

procedure TMainForm.vstMemInfoThreadsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PLinkData;
  ThData: PThreadData;
  ProcData: PProcessData;
  MemInfo: TGetMemInfo;
  MItem: TGetMemInfoItem;
  MemNode: PVirtualNode;
begin
  MemInfo := Nil;
  Data := Sender.GetNodeData(Node);
  case Data^.LinkType of
    ltProcess:
    begin
      ProcData := Data^.ProcessData;
      MemInfo := ProcData^.DbgGetMemInfo;
    end;
    ltThread:
    begin
      ThData := Data^.ThreadData;
      MemInfo := ThData^.DbgGetMemInfo;
    end;
  end;

  vstMemStack.Clear;

  vstMemList.BeginUpdate;
  try
    if MemInfo <> Nil then
    begin
        vstMemList.Clear;

        for MItem in MemInfo do
        begin
          MemNode := vstMemList.AddChild(nil);
          Data := vstMemList.GetNodeData(MemNode);
          Data^.SyncNode := Node;
          Data^.LinkType := ltMemInfo;
          Data^.MemPtr := MItem.Key;
        end;
    end;
  finally
    vstMemList.EndUpdate;
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
            0: CellText := ExtractFileName(FProjectFileName);
            1: CellText := Format('%d(%x)', [ProcData^.ProcessID, ProcData^.ProcessID]);
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
            1: CellText := Format('%d(%x)', [ThData^.ThreadID, ThData^.ThreadID]);
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
  MemInfo: TGetMemInfo;
  GetMemInfo: PGetMemInfo;

  StackNode: PVirtualNode;
  StackData: PLinkData;
  Idx: Integer;
  Ptr: Pointer;
begin
  vstMemStack.Clear;

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

  if (MemInfo <> Nil) and MemInfo.TryGetValue(Data^.MemPtr, GetMemInfo) then
  begin
    vstMemStack.BeginUpdate;
    try
      for Idx := 0 to High(GetMemInfo.Stack) do
      begin
        Ptr := GetMemInfo.Stack[Idx];
        if Ptr = nil then Break;

        StackNode := vstMemStack.AddChild(nil);
        StackData := vstMemStack.GetNodeData(StackNode);
        StackData^.LinkType := ltMemStack;
        StackData^.MemStackPtr := Ptr;
      end;
    finally
      vstMemStack.EndUpdate;
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
  MemInfo: TGetMemInfo;
  GetMemInfo: PGetMemInfo;
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
        if (MemInfo <> Nil) and MemInfo.TryGetValue(Data^.MemPtr, GetMemInfo) then
          CellText := GetMemInfo.GetObjectType(Data^.MemPtr);
      end;
    1: CellText := Format('%p', [Data^.MemPtr]);
    2:
      begin
        if (MemInfo <> Nil) and MemInfo.TryGetValue(Data^.MemPtr, GetMemInfo) then
          CellText := IntToStr(GetMemInfo.Size);
      end;
  end;
end;

procedure TMainForm.vstMemStackGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PLinkData;
  StackEntry: TStackEntry;
begin
  Data := vstMemStack.GetNodeData(Node);
  case Column of
    0: begin
      StackEntry := TStackEntry.Create(gvDebugInfo);
      try
        if StackEntry.UpdateInfo(Data^.MemStackPtr) <> slNotFound then
          CellText := StackEntry.GetInfo
        else
          CellText := Format('[$%p] unknown', [Data^.MemStackPtr]);
      finally
        FreeAndNil(StackEntry);
      end;
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

//  if Sender.Treeview.Parent is TPanel then
//  begin
//    TPanel(Sender.Treeview.Parent).Width := Sender.Treeview.Width;
//  end;
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

function TMainForm.EllapsedToTime(const Ellapsed: UInt64): String;
var
  FT: TFileTime;
  DT: TDateTime;
begin
  FT := Int64ToFileTime(Ellapsed);
  DT := FileTimeToDateTime(FT);

  Result := FormatDateTime('nn:ss.zzz', DT);
end;

procedure TMainForm.vstThreadsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
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
            0: CellText := ExtractFileName(FProjectFileName);
            1: CellText := Format('%d(%x)', [ProcData^.ProcessID, ProcData^.ProcessID]);
            2: CellText := EllapsedToTime(ProcData^.CPUTime);
          end;
      end;
    ltThread:
      begin
        ThData := Data^.ThreadData;
        if ThData <> nil then
          case Column of
            0: CellText := ThData^.ThreadAdvInfo^.AsString;
            1: CellText := Format('%d(%x)', [ThData^.ThreadID, ThData^.ThreadID]);
            2: CellText := EllapsedToTime(ThData^.CPUTime);
          end;
      end;
  end;
end;

procedure TMainForm.vstThreadsScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
begin
  if DeltaY <> 0 then
    vdtTimeLine.OffsetY := vstThreads.OffsetY;
end;

procedure TMainForm.WMClose(var Message: TWMClose);
begin
  FCloseApp := True;
  inherited;
end;

end.
