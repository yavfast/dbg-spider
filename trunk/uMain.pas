unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Mask, ExtCtrls, ComCtrls, ActnList, DebugInfo,
  Grids, JvExGrids, JvGrids, JvgStringGrid, VirtualTrees, JvComponentBase,
  JvNavigationPane, JvExExtCtrls, JvExtComponent, JvSplit,
  Debuger, DelphiDebugInfo, JvExComCtrls, JvStatusBar, JvExControls,
  JvEditorCommon, JvUnicodeEditor, JvUnicodeHLEditor, JvSplitter;

type
  TacAction = (acRunEnabled, acStopEnabled, acCreateProcess, acAddThread, acUpdateInfo);

  TMainForm = class(TForm)
    pActions: TPanel;
    BitBtn1: TBitBtn;
    eAppPath: TMaskEdit;
    AL: TActionList;
    acAppOpen: TAction;
    acRun: TAction;
    acStop: TAction;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    OD: TFileOpenDialog;
    Button1: TButton;
    acDebugInfo: TAction;
    PageControl1: TPageControl;
    tsLog: TTabSheet;
    tsDebugInfo: TTabSheet;
    mLog: TMemo;
    pUnits: TPanel;
    Splitter1: TSplitter;
    pUnitInfo: TPanel;
    lbUnits: TListBox;
    PageControl2: TPageControl;
    tsConsts: TTabSheet;
    tsTypes: TTabSheet;
    tsFunctions: TTabSheet;
    tsVars: TTabSheet;
    mConsts: TMemo;
    mTypes: TMemo;
    mVars: TMemo;
    mFunctions: TMemo;
    tmrThreadsUpdate: TTimer;
    btnAttach: TBitBtn;
    tsThreads1: TTabSheet;
    vstThreads: TVirtualStringTree;
    sm1: TJvNavPaneStyleManager;
    vdtTimeLine: TVirtualDrawTree;
    acAttachProcess: TAction;
    sbInfo: TJvStatusBar;
    cbCPUTimeLine: TCheckBox;
    tsCodeView: TTabSheet;
    vstModules: TVirtualStringTree;
    spl1: TJvSplitter;
    hleCodeView: TJvWideHLEditor;
    procedure FormCreate(Sender: TObject);

    procedure acAppOpenExecute(Sender: TObject);
    procedure acAttachProcessExecute(Sender: TObject);
    procedure acRunExecute(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure acDebugInfoExecute(Sender: TObject);

    procedure tmrThreadsUpdateTimer(Sender: TObject);
    procedure UpdateTrees;
    procedure UpdateStatusInfo;

    procedure lbUnitsClick(Sender: TObject);

    procedure vstThreadsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstThreadsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstThreadsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure vstThreadsScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
    procedure vstThreadsCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstThreadsExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);

    procedure vdtTimeLineDrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
    procedure vdtTimeLineAdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure vdtTimeLineHeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure vdtTimeLineScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
    procedure vdtTimeLineChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vdtTimeLinePaintBackground(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; R: TRect; var Handled: Boolean);

    procedure cbCPUTimeLineClick(Sender: TObject);
    procedure vstThreadsColumnResize(Sender: TVTHeader;
      Column: TColumnIndex);
  private
    FPID: DWORD;
    FAppName: String;

    function GetLineTimeOffset: Cardinal;

    procedure LoadUnits;
    procedure LoadConsts(UnitInfo: TUnitInfo);
    procedure LoadTypes(UnitInfo: TUnitInfo);
    procedure LoadVars(UnitInfo: TUnitInfo);
    procedure LoadFunctions(UnitInfo: TUnitInfo);

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
    function FindThreadNode(ThData: PThreadData): PVirtualNode;
  public
    procedure Log(const Msg: String);
    procedure DoAction(Action: TacAction; Args: array of Variant);
    procedure ViewDebugInfo(DebugInfo: TDebugInfo);
  end;

var
  MainForm: TMainForm = nil;

implementation

{$R *.dfm}

uses Math, EvaluateTypes, ClassUtils, uProcessList;


type
  TDbgOption = (doDebugInfo, doRun, doProfiler, doMemLeaks);
  TDbgOptions = set of TDbgOption;

  TActionController = class
  public
    class procedure RunDebug(const AppName: String; ADbgOptions: TDbgOptions; AProcessID: TProcessId = 0); static;
    class procedure StopDebug; static;
    class procedure PauseDebug; static;

    class procedure Log(const Msg: String); overload; static;
    class procedure Log(const Msg: String; Args: array of const); overload; static;

    class procedure DoAction(const Action: TacAction; Args: array of Variant); static;
    class procedure ViewDebugInfo(DebugInfo: TDebugInfo); static;
  end;

  TDebugerThread = class(TThread)
  private
    FAppName: String;
    FProcessID: TProcessId;
    FDbgOptions: TDbgOptions;

    FDbgInfoLoaded: Boolean;
    FDbgStarted: Boolean;

    procedure OnEndDebug(Sender: TObject);
    procedure OnRip(Sender: TObject; ThreadId: TThreadId; Data: PRIPInfo);
    procedure OnCreateThread(Sender: TObject; ThreadId: TThreadId; Data: PCreateThreadDebugInfo);
    procedure OnExitThread(Sender: TObject; ThreadId: TThreadId; Data: PExitThreadDebugInfo);
    procedure OnCreateProcess(Sender: TObject; ProcessId: TProcessId; Data: PCreateProcessDebugInfo);
    procedure OnExitProcess(Sender: TObject; ProcessId: TProcessId; Data: PExitProcessDebugInfo);
    procedure OnLoadDll(Sender: TObject; ThreadId: TThreadId; Data: PLoadDLLDebugInfo);
    procedure OnUnLoadDll(Sender: TObject; ThreadId: TThreadId; Data: PUnloadDLLDebugInfo);
    procedure OnDebugString(Sender: TObject; ThreadId: TThreadId; Data: POutputDebugStringInfo);
    procedure OnUnknownException(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord);
    procedure OnUnknownBreakPoint(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord);
    procedure OnBreakPoint(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord; BreakPointIndex: Integer; var ReleaseBreakpoint: Boolean);

    procedure InitDebuger;
    procedure LoadDebugInfo;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create(const AppName: String; ADbgOptions: TDbgOptions; AProcessID: TProcessId = 0);
    destructor Destroy; override;
  end;

  THookBaseVirtualTree = class(TBaseVirtualTree);

var
  _DbgThread: TDebugerThread = nil;
  _AC: TActionController = nil;

type
  TLinkType = (ltProcess, ltThread);

  PLinkData = ^TLinkData;
  TLinkData = record
    SyncNode: PVirtualNode;
    case LinkType: TLinkType of
      ltProcess:
        (ProcessData: PProcessData);
      ltThread:
        (ThreadData: PThreadData);
  end;

procedure TMainForm.acAppOpenExecute(Sender: TObject);
begin
  if OD.Execute then
    eAppPath.Text := OD.FileName;
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
      eAppPath.Text := F.GetSelProcessName;
    end;
  finally
    F.Release;
  end;
end;

procedure TMainForm.acDebugInfoExecute(Sender: TObject);
begin
  FAppName := eAppPath.Text;

  _AC.RunDebug(FAppName, [doDebugInfo], FPID);
end;

procedure TMainForm.acRunExecute(Sender: TObject);
begin
  acRun.Enabled := False;

  mLog.Clear;
  vdtTimeLine.Clear;
  vstThreads.Clear;

  FAppName := eAppPath.Text;

  _AC.RunDebug(FAppName, [doDebugInfo, doRun, doProfiler], FPID);
end;

procedure TMainForm.acStopExecute(Sender: TObject);
begin
  acStop.Enabled := False;

  _AC.StopDebug;
end;

procedure TMainForm.AddProcess(const ProcessID: Cardinal);
var
  LinkData: PLinkData;
  NameNode: PVirtualNode;
  TimeLineNode: PVirtualNode;
begin
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
end;

function TMainForm.FindThreadNode(ThData: PThreadData): PVirtualNode;

  function _Find(Node: PVirtualNode): PVirtualNode;
  var
    CurNode: PVirtualNode;
    LinkData: PLinkData;
  begin
    Result := Nil;
    CurNode := Node^.FirstChild;
    if CurNode <> Nil then
    repeat
      LinkData := vstThreads.GetNodeData(CurNode);
      if (LinkData^.LinkType = ltThread) and (LinkData^.ThreadData = ThData) then
      begin
        Result := CurNode;
        Break;
      end;

      Result := _Find(CurNode);
      CurNode := CurNode^.NextSibling;
    until (CurNode = nil) or (Result <> Nil);
  end;

begin
  Result := _Find(vstThreads.RootNode);
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
begin
  ThData := gvDebuger.GetThreadData(ThreadID);
  ParentId := ThData^.ThreadAdvInfo^.ThreadParentId;

  ParentNode := Nil;
  if ParentId <> 0 then
  begin
    ParentThData := gvDebuger.GetThreadData(ParentId);
    if ParentThData <> Nil then
      ParentNode := FindThreadNode(ParentThData);
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
end;

procedure TMainForm.cbCPUTimeLineClick(Sender: TObject);
begin
  UpdateTrees;
end;

procedure TMainForm.DoAction(Action: TacAction; Args: array of Variant);
begin
  case Action of
    acRunEnabled:
      acRun.Enabled := Args[0];
    acStopEnabled:
    begin
      acStop.Enabled := Args[0];
      tmrThreadsUpdate.Enabled := acStop.Enabled;
    end;
    acAddThread:
      AddThread(Args[0]);
    acCreateProcess:
      AddProcess(Args[0]);
  end;

  UpdateStatusInfo;
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
  TargetCanvas.Pen.Mode := pmMaskPenNot;

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
  T1, T2, F: Int64;
  OffsetT1, OffsetT2, Offset: Cardinal;
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

  C.Brush.Color := clHotLight;
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
          T1 := I;
          X1 := R.Left + Integer(T1 - CurOffset) - 1;
          if (X1 < R.Left) then Continue;
          if (X1 > R.Right) then Continue;

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

  C.Brush.Color := clHotLight;
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
  T1, T2, F: Int64;
  //OffsetT1, OffsetT2, Offset: Cardinal;
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

  C.Brush.Color := clHotLight;
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
            C.Pen.Color := clWindowText;
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

  C.Brush.Color := clHotLight;
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
            C.Pen.Color := clWindowText;
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

  C.Brush.Color := clWindow;
  C.Brush.Style := bsSolid;

  C.FillRect(R);

  C.Pen.Color := clWindowText;
  C.Pen.Style := psSolid;

  For Cnt := 0 to ((R.Right - R.Left + 1) div 100) + 1 do
  begin
    X := R.Left + Cnt * 100;
    Y := R.Bottom;

    T := '';

    if cbCPUTimeLine.Checked then
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
      C.TextOut(X + 3, R.Top, T);

    C.MoveTo(X + 1, Y - 1);
    C.LineTo(X + 1, Y - 8);

    C.MoveTo(X + 50, Y - 1);
    C.LineTo(X + 50, Y - 4);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TThread.NameThreadForDebugging(AnsiString(ClassName), MainThreadID);
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
    if cbCPUTimeLine.Checked then
      Result := (Trunc(gvDebugInfo.Debuger.ProcessData.CurDbgPointIdx * F) div 100) * 100
    else
      Result := Trunc((gvDebugInfo.Debuger.ProcessData.Ellapsed_MSec div 1000) * F);
  end;
end;

procedure TMainForm.lbUnitsClick(Sender: TObject);
var
  UnitInfo: TUnitInfo;
begin
  if lbUnits.ItemIndex >= 0 then
  begin
    UnitInfo := TUnitInfo(lbUnits.Items.Objects[lbUnits.ItemIndex]);

    LoadConsts(UnitInfo);
    LoadTypes(UnitInfo);
    LoadVars(UnitInfo);
    LoadFunctions(UnitInfo);
  end;
end;

procedure TMainForm.LoadConsts(UnitInfo: TUnitInfo);
var
  I: Integer;
  C: TConstInfo;
begin
  mConsts.Lines.BeginUpdate;
  try
    mConsts.Clear;
    for I := 0 to UnitInfo.Consts.Count - 1 do
    begin
      C := TConstInfo(UnitInfo.Consts[I]);
      mConsts.Lines.Add(Format('%s: %s = %s', [C.Name, C.TypeInfo.Name, C.ValueAsString]));
    end;
  finally
    mConsts.Lines.EndUpdate;
  end;
end;

procedure TMainForm.LoadFunctions(UnitInfo: TUnitInfo);
var
  I: Integer;
  F: TFuncInfo;
begin
  mFunctions.Lines.BeginUpdate;
  try
    mFunctions.Clear;
    for I := 0 to UnitInfo.Funcs.Count - 1 do
    begin
      F := TFuncInfo(UnitInfo.Funcs[I]);
      mFunctions.Lines.Add(Format('%s(%s): %s', [F.Name, F.ParamsAsString, F.ResultType.Name]));
    end;
  finally
    mFunctions.Lines.EndUpdate;
  end;
end;

procedure TMainForm.LoadTypes(UnitInfo: TUnitInfo);
var
  I: Integer;
  T: TTypeInfo;
begin
  mTypes.Lines.BeginUpdate;
  try
    mTypes.Clear;

    for I := 0 to UnitInfo.Types.Count - 1 do
    begin
      T := TTypeInfo(UnitInfo.Types[I]);
      if T.NameId > 0 then
        mTypes.Lines.Add(Format('%s = %s', [T.Name, T.TypeOf]))
    end;
  finally
    mTypes.Lines.EndUpdate;
  end;
end;

procedure TMainForm.LoadUnits;
var
  I: Integer;
  UnitInfo: TUnitInfo;
begin
  lbUnits.Items.BeginUpdate;
  try
    lbUnits.Clear;

    lbUnits.Items.Capacity := gvDebugInfo.Units.Count;
    for I := 0 to gvDebugInfo.Units.Count - 1 do
    begin
      UnitInfo := TUnitInfo(gvDebugInfo.Units.Objects[I]);
      lbUnits.AddItem(String(UnitInfo.Name), UnitInfo);
    end;
  finally
    lbUnits.Items.EndUpdate;
  end;
end;

procedure TMainForm.LoadVars(UnitInfo: TUnitInfo);
var
  I: Integer;
  V: TVarInfo;
begin
  mVars.Lines.BeginUpdate;
  try
    mVars.Clear;
    for I := 0 to UnitInfo.Vars.Count - 1 do
    begin
      V := TVarInfo(UnitInfo.Vars[I]);
      mVars.Lines.Add(V.AsString);
    end;
  finally
    mVars.Lines.EndUpdate;
  end;
end;

procedure TMainForm.Log(const Msg: String);
begin
  mLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ': ' + Msg);
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

procedure TMainForm.UpdateStatusInfo;
var
  Msg: String;
begin
  if Assigned(gvDebuger) then
  begin
    if Assigned(gvDebugInfo) and (gvDebugInfo.DebugInfoLoaded) then
      sbInfo.Panels[0].Text := 'DBG_INFO'
    else
      sbInfo.Panels[0].Text := 'NO_DBG_INFO';

    case gvDebuger.DbgState of
      dsNone: Msg := 'NONE';
      dsStarted: Msg := 'STARTED';
      dsWait: Msg := 'ACTIVE';
      dsPerfomance: Msg := 'ACTIVE';
      dsTrace: Msg := 'TRACE';
      dsEvent: Msg := 'ACTIVE';
      dsStoping: Msg := 'STOPING';
      dsStoped: Msg := 'STOPED';
      dsDbgFail: Msg := 'DBG_FAIL';
      else
        Msg := '';
    end;
    sbInfo.Panels[1].Text := Msg;

    if gvDebuger.PerfomanceMode and not(gvDebuger.DbgState in [dsNone]) then
      sbInfo.Panels[2].Text := IntToStr(gvDebuger.ProcessData.CurDbgPointIdx)
    else
      sbInfo.Panels[2].Text := 'NO_PERF';
  end
  else
  begin
    sbInfo.Panels[0].Text := 'NO_DBG_INFO';
    sbInfo.Panels[1].Text := 'NONE';
    sbInfo.Panels[2].Text := 'NO_PERF';
  end;
end;

procedure TMainForm.UpdateTrees;
begin
  vstThreads.Invalidate;

  vdtTimeLine.Invalidate;
  vdtTimeLine.Header.Invalidate(nil);
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
      if cbCPUTimeLine.Checked then
        DrawProcessCPUTimeLine(PaintInfo.Canvas, PaintInfo.CellRect, LinkData^.ProcessData, GetLineTimeOffset)
      else
        DrawProcessTimeLine(PaintInfo.Canvas, PaintInfo.CellRect, LinkData^.ProcessData, GetLineTimeOffset);
    end;
    ltThread:
    begin
      if cbCPUTimeLine.Checked then
        DrawThreadCPUTimeLine(PaintInfo.Canvas, PaintInfo.CellRect, LinkData^.ThreadData, GetLineTimeOffset)
      else
        DrawThreadTimeLine(PaintInfo.Canvas, PaintInfo.CellRect, LinkData^.ThreadData, GetLineTimeOffset);
    end;
  end;
end;

procedure TMainForm.vdtTimeLineHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  Include(Elements, hpeBackground);
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

  vstThreads.Width := W;
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

procedure TMainForm.vstThreadsGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
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
            0: CellText := ExtractFileName(FAppName);
            1: CellText := Format('%x', [ProcData^.ProcessID]);
            2: CellText := EllapsedToTime(ProcData^.CPUTime);
          end;
      end;
    ltThread:
      begin
        ThData := Data^.ThreadData;
        if ThData <> nil then
          case Column of
            0: CellText := ThData^.ThreadAdvInfo^.AsString;
            1: CellText := Format('%d', [ThData^.ThreadID]);
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

{ TDebugerThread }

constructor TDebugerThread.Create(const AppName: String; ADbgOptions: TDbgOptions; AProcessID: TProcessId = 0);
begin
  inherited Create(True);
  FreeOnTerminate := True;

  FAppName := AppName;
  FDbgOptions := ADbgOptions;
  FProcessID := AProcessID;

  FDbgInfoLoaded := False;
  FDbgStarted := False;

  Priority := tpHighest;

  Suspended := False;
end;

destructor TDebugerThread.Destroy;
begin
  _DbgThread := nil;
  inherited;
end;

procedure TDebugerThread.DoTerminate;
begin
  inherited;

  _DbgThread := Nil;
end;

procedure TDebugerThread.Execute;
var
  FRun: Boolean;
begin
  NameThreadForDebugging(AnsiString(ClassName), ThreadId);

  InitDebuger;

  if doDebugInfo in FDbgOptions then
    LoadDebugInfo
  else
    FreeAndNil(gvDebugInfo);

  if doRun in FDbgOptions then
  begin
    if FProcessID = 0 then
    begin
      _AC.Log('Run application "%s"', [FAppName]);
      FRun := gvDebuger.DebugNewProcess(FAppName, False);
    end
    else
    begin
      _AC.Log('Attach to process [%d]', [FProcessID]);
      FRun := gvDebuger.AttachToProcess(FProcessID, False);
    end;

    if FRun then
    begin
      gvDebuger.PerfomanceMode := (doProfiler in FDbgOptions);

      _AC.Log('Start debug process');
      try
        gvDebuger.ProcessDebugEvents;
      except
        on E: Exception do
          _AC.Log('Fail debug process: "%s"', [E.Message]);
      end;
    end;
  end;
end;

procedure TDebugerThread.InitDebuger;
begin
  if gvDebuger <> nil then
    FreeAndNil(gvDebuger);

  _AC.Log('Init debuger for "%s"', [FAppName]);

  gvDebuger := TDebuger.Create();

  gvDebuger.OnEndDebug := OnEndDebug;
  gvDebuger.OnRip := OnRip;
  gvDebuger.OnCreateProcess := OnCreateProcess;
  gvDebuger.OnExitProcess := OnExitProcess;
  gvDebuger.OnCreateThread := OnCreateThread;
  gvDebuger.OnExitThread := OnExitThread;
  gvDebuger.OnLoadDll := OnLoadDll;
  gvDebuger.OnUnloadDll := OnUnLoadDll;
  gvDebuger.OnDebugString := OnDebugString;
  gvDebuger.OnUnknownException := OnUnknownException;
  gvDebuger.OnUnknownBreakPoint := OnUnknownBreakPoint;
  gvDebuger.OnBreakPoint := OnBreakPoint;
end;

procedure TDebugerThread.LoadDebugInfo;
begin
  if gvDebugInfo <> nil then
    FreeAndNil(gvDebugInfo);

  _AC.Log('Load debug info for "%s"', [FAppName]);

  gvDebugInfo := TDelphiDebugInfo.Create(gvDebuger);
  FDbgInfoLoaded := gvDebugInfo.ReadDebugInfo(FAppName, nil);

  if FDbgInfoLoaded then
  begin
    _AC.Log('Loaded debug info for "%s"', [FAppName]);
    _AC.ViewDebugInfo(gvDebugInfo);
  end
  else
    _AC.Log('No debug info for "%s"', [FAppName]);

  _AC.DoAction(acUpdateInfo, []);
end;

procedure TDebugerThread.OnBreakPoint(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord;
      BreakPointIndex: Integer; var ReleaseBreakpoint: Boolean);
begin
  if BreakPointIndex = -1 then
    _AC.Log('Perfomance ThreadID: %d', [ThreadId]);
end;

procedure TDebugerThread.OnCreateProcess(Sender: TObject; ProcessId: TProcessId; Data: PCreateProcessDebugInfo);
begin
  _AC.Log('Process Start ID: %d', [ProcessId]);

  _AC.DoAction(acStopEnabled, [True]);
  _AC.DoAction(acCreateProcess, [ProcessId]);
end;

procedure TDebugerThread.OnCreateThread(Sender: TObject; ThreadId: TThreadId; Data: PCreateThreadDebugInfo);
begin
  _AC.Log('Thread Create ID: %d', [ThreadID]);
  _AC.DoAction(acAddThread, [ThreadID]);
end;

procedure TDebugerThread.OnDebugString(Sender: TObject; ThreadId: TThreadId; Data: POutputDebugStringInfo);
begin
  if Data^.fUnicode = 1 then
    _AC.Log('Debug String: ' + PWideChar(gvDebuger.ReadStringW(Data^.lpDebugStringData, Data^.nDebugStringLength)))
  else
    _AC.Log('Debug String: ' + PAnsiChar(gvDebuger.ReadStringA(Data^.lpDebugStringData, Data^.nDebugStringLength)));

  _AC.DoAction(acUpdateInfo, []);
end;

procedure TDebugerThread.OnExitProcess(Sender: TObject; ProcessId: TProcessId; Data: PExitProcessDebugInfo);
begin
  _AC.Log('Process Exit ID: %d', [ProcessID]);

  _AC.DoAction(acUpdateInfo, []);
end;

procedure TDebugerThread.OnExitThread(Sender: TObject; ThreadId: TThreadId; Data: PExitThreadDebugInfo);
begin
  if Data <> Nil then
    _AC.Log('Thread Exit ID: %d (%d)', [ThreadID, Data^.dwExitCode])
  else
    _AC.Log('Thread Exit ID: %d', [ThreadID]);

  _AC.DoAction(acUpdateInfo, []);
end;

procedure TDebugerThread.OnEndDebug(Sender: TObject);
begin
  _AC.Log('Finish debug');

  _AC.DoAction(acStopEnabled, [False]);
  _AC.DoAction(acRunEnabled, [True]);
end;

procedure TDebugerThread.OnLoadDll(Sender: TObject; ThreadId: TThreadId; Data: PLoadDLLDebugInfo);
const
  FormatStrKnownDLL = 'Load Dll at instance %p handle %d "%s"';
  FormatStrUnknownDLL = 'Load unknown Dll at instance %p handle %d';
var
  DllName: AnsiString;
  IsUnicodeData: Boolean;
begin
  //FDebuger.ContinueStatus := DBG_EXCEPTION_NOT_HANDLED;
  IsUnicodeData := Data^.fUnicode = 1;
  DllName := gvDebuger.GetDllName(Data^.lpImageName, Data^.lpBaseOfDll, IsUnicodeData);
  if DllName <> '' then
  begin
    if IsUnicodeData then
      _AC.Log(FormatStrKnownDLL, [Data^.lpBaseOfDll, Data^.hFile, PWideChar(@DllName[1])])
    else
      _AC.Log(Format(FormatStrKnownDLL, [Data^.lpBaseOfDll, Data^.hFile, PAnsiChar(@DllName[1])]));
  end
  else
    _AC.Log(Format(FormatStrUnknownDLL, [Data^.lpBaseOfDll, Data^.hFile]));
end;

procedure TDebugerThread.OnRip(Sender: TObject; ThreadId: TThreadId; Data: PRIPInfo);
begin
  _AC.Log('Debug fail [error: %d; type: %d]', [Data^.dwError, Data^.dwType]);
  _AC.DoAction(acUpdateInfo, []);
end;

procedure TDebugerThread.OnUnknownBreakPoint(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord);
begin
  _AC.Log('OnUnknownBreakPoint ThreadID: %d', [ThreadId]);
end;

procedure TDebugerThread.OnUnknownException(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord);
var
  StackItems: TList;
  StackInfo: TStackEntry;
  I: Integer;
  ThData: PThreadData;
begin
  gvDebuger.ContinueStatus := DBG_EXCEPTION_NOT_HANDLED;

  _AC.Log(gvDebugInfo.GetExceptionMessage(ExceptionRecord, ThreadId));

  if gvDebuger.ProcessData.State = psActive then
  begin
    ThData := gvDebuger.GetThreadData(ThreadId);
    if Assigned(ThData) and (ThData^.State <> tsFinished) then
    begin
      StackItems := TList.Create;
      try
        gvDebugInfo.GetCallStackItems(ThreadId,
          gvDebugInfo.GetExceptionAddress(ExceptionRecord),
          gvDebugInfo.GetExceptionFrame(ExceptionRecord),
          StackItems);

        for I := 0 to StackItems.Count - 1 do
        begin
          StackInfo := TStackEntry(StackItems[I]);
          _AC.Log(StackInfo.GetInfo);
        end;
      finally
        FreeList(StackItems);
      end;
    end;
  end;
end;

procedure TDebugerThread.OnUnLoadDll(Sender: TObject; ThreadId: TThreadId; Data: PUnloadDLLDebugInfo);
const
  FormatStrDLL = 'UnLoad Dll at instance %p';
begin
  _AC.Log(FormatStrDLL, [Data^.lpBaseOfDll]);
end;

{ TActionController }

class procedure TActionController.Log(const Msg: String);
var
  _Msg: String;
begin
  _Msg := Msg;
  if Assigned(MainForm) then
    TThread.Synchronize(nil,
      procedure
      begin
        MainForm.Log(_Msg);
      end
    );
end;

class procedure TActionController.DoAction(const Action: TacAction; Args: array of Variant);
var
  _Action: TacAction;
  _Args: array of Variant;
  i: Integer;
begin
  _Action := Action;

  SetLength(_Args, Length(Args));
  for i := 0 to High(Args) do
    _Args[i] := Args[i];

  if Assigned(MainForm) then
  begin
    TThread.Synchronize(nil,
      procedure
      begin
        MainForm.DoAction(_Action, _Args);
      end
    );
  end;
end;

class procedure TActionController.Log(const Msg: String; Args: array of const);
begin
  Log(Format(Msg, Args));
end;

class procedure TActionController.PauseDebug;
begin
  //
end;

class procedure TActionController.RunDebug(const AppName: String; ADbgOptions: TDbgOptions; AProcessID: TProcessId = 0);
begin
  if not Assigned(_DbgThread) then
    _DbgThread := TDebugerThread.Create(AppName, ADbgOptions, AProcessID);
end;

class procedure TActionController.StopDebug;
begin
  if Assigned(gvDebuger) then
    gvDebuger.StopDebug;
end;

class procedure TActionController.ViewDebugInfo(DebugInfo: TDebugInfo);
var
  DI: TDebugInfo;
begin
  DI := DebugInfo;
  TThread.Synchronize(nil,
    procedure
    begin
      MainForm.ViewDebugInfo(DI);
    end
  );
end;

initialization

finalization
//  if FDebugInfo <> Nil then
//    FreeAndNil(FDebugInfo);

  if gvDebuger <> Nil then
    FreeAndNil(gvDebuger);

end.
