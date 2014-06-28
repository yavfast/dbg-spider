unit Debuger;

interface

uses
  WinApi.Windows, System.Classes, System.SysUtils, System.SyncObjs,
  ClassUtils, JclPeImage, JclDebug, DebugerTypes, DbgHookTypes,
  Collections.Queues, Collections.Dictionaries, DbgMemoryProfiler,
  DbgSyncObjsProfiler, DbgSamplingProfiler, DbgCodeProfiler;

type
  TDebuger = class
  strict private
    FProcessData: TProcessData;          // ��������� ���������� �� ������������ ��������
  private
    FThreadList: TDbgThreadList;              // ������ � ���� ������� ������������� ��������
    FThreadAdvInfoList: TThreadAdvInfoList;   // �������������� ���������� � �������
    FActiveThreadList: TDbgActiveThreadList;  // ������ �������� �������

    FSetEntryPointBreakPoint: LongBool;   // ���� ����������� ���������, ���������� �� ������� �� �� ��
    FBreakpointList: TBreakpointList;    // ������ �� � ���
    FRestoreBPIndex: Integer;            // ������ ��� �������������� ��
    FRestoreMBPIndex: Integer;           // ������ ��� �������������� ���
    FRestoredHWBPIndex: Integer;         // ������� ��� �������������� ���
    FRestoredThread: TThreadId;
    FCloseDebugProcess: LongBool;         // ���� ����������� ����� �� ��������� ������������ ������� ��� ���������� �������
    FContinueStatus: DWORD;              // ������ � ������� ���������� ContinueDebugEvent
    FResumeAction: TResumeAction;        // ���� ��������� ��������� ����� ��������� ���������� �������
    FRemoveCurrentBreakpoint: LongBool;   // ���� �������� �������� ��

    FCurThreadId: TThreadId;
    FCurThreadData: PThreadData;

    FDbgState: TDbgState;

    FDbgTraceState: TDbgTraceState;
    FTraceEvent: TEvent;
    FTraceCounter: Cardinal;

    // Debug options
    FPerfomanceMode: LongBool;

    FExceptionCheckMode: LongBool;
    FExceptionCallStack: LongBool;

    FCodeTracking: LongBool;
    FTrackSystemUnits: LongBool;
    FSamplingMethod: LongBool;
    // ---

    FMemoryBPCheckMode: LongBool;

    FPerfomanceCheckPtr: Pointer;

    // ������� �������
    FMainLoopFailed: TNotifyEvent;
    FCreateThread: TCreateThreadEvent;
    FCreateProcess: TCreateProcessEvent;
    FExitThread: TExitThreadEvent;
    FExitProcess: TExitProcessEvent;
    FLoadDll: TLoadDllEvent;
    FUnLoadDll: TUnLoadDllEvent;
    FDebugString: TDebugStringEvent;
    FRip: TRipEvent;
    FEndDebug: TNotifyEvent;
    FChangeDebugState: TNotifyEvent;

    FDbgLog: TDbgLogEvent;
    FDbgLogMode: LongBool; // �������� �����

    FExceptionEvents: TExceptionEvents;
    FBreakPoint: TBreakPointEvent;
    FHardwareBreakpoint: THardwareBreakpointEvent;

    FDbgMemoryProfiler: TDbgMemoryProfiler;
    FDbgSyncObjsProfiler: TDbgSyncObjsProfiler;
    FDbgSamplingProfiler: TDbgSamplingProfiler;
    FDbgCodeProfiler: TDbgCodeProfiler;

    function GetExceptionEvent(const Index: TExceptionCode): TDefaultExceptionEvent;
    procedure SetExceptionEvent(const Index: TExceptionCode; const Value: TDefaultExceptionEvent);
    procedure SetCloseDebugProcess(const Value: LongBool);

    procedure SetPerfomanceMode(const Value: LongBool);
    procedure SetCodeTracking(const Value: LongBool);
    procedure SetTrackSystemUnits(const Value: LongBool);
    procedure SetExceptionCallStack(const Value: LongBool);
    procedure SetExceptionCheckMode(const Value: LongBool);

    procedure DoSetBreakpoint(const Address: Pointer; var SaveByte: Byte);
    procedure DoSetBreakpointF(const Address: Pointer; var SaveByte: Byte);
    //procedure DoRemoveBreakpoint(const Address: Pointer; const SaveByte: Byte);
    procedure DoRemoveBreakpointF(const Address: Pointer; const SaveByte: Byte);
    //procedure DoRestoreBreakpoint(const Address: Pointer);
    procedure DoRestoreBreakpointF(const Address: Pointer);

    procedure SetDbgTraceState(const Value: TDbgTraceState);
    procedure SetDbgState(const Value: TDbgState);
    procedure SetSamplingMethod(const Value: LongBool);

    function GetActive: LongBool; inline;
  protected
    // ������ � ������� � ����� ������������� ����������
    function AddThread(const ThreadID: TThreadId; ThreadHandle: THandle): PThreadData;
    procedure RemoveThread(const ThreadID: TThreadId);

    function GetThreadIndex(const ThreadID: TThreadId; const UseFinished: LongBool = False): Integer;

    function GetThreadInfoIndex(const ThreadId: TThreadId): Integer;
    function AddThreadInfo(const ThreadId: TThreadId): PThreadAdvInfo;
    function GetThreadInfo(const ThreadId: TThreadId): PThreadAdvInfo;
    function SetThreadInfo(const ThreadId: TThreadId): PThreadAdvInfo;

    // ����������� ���������� ������� ������ �������
    procedure DoCreateProcess(DebugEvent: PDebugEvent);
    procedure DoExitProcess(DebugEvent: PDebugEvent);

    procedure DoCreateThread(DebugEvent: PDebugEvent);
    procedure DoExitThread(DebugEvent: PDebugEvent);

    procedure DoLoadDll(DebugEvent: PDebugEvent);
    procedure DoUnLoadDll(DebugEvent: PDebugEvent);

    procedure DoDebugString(DebugEvent: PDebugEvent);
    procedure DoRip(DebugEvent: PDebugEvent);
    procedure DoEndDebug;
    procedure DoDebugerFailed;
    procedure DoResumeAction(const ThreadID: TThreadId);

    procedure DoDbgLog(const ThreadId: TThreadId; const LogData: String);

    // ����������� ���������� ������� ������ �������
    procedure CallUnhandledExceptionEvents(const Code: TExceptionCode; DebugEvent: PDebugEvent);
    procedure CallUnhandledBreakPointEvents(const Code: TExceptionCode; DebugEvent: PDebugEvent);

    procedure ProcessExceptionBreakPoint(DebugEvent: PDebugEvent);

    function ProcessUserBreakPoint(DebugEvent: PDebugEvent): LongBool;

    function ProcessTraceBreakPoint(DebugEvent: PDebugEvent): LongBool;

    procedure ProcessExceptionSingleStep(DebugEvent: PDebugEvent);
    procedure ProcessExceptionGuardPage(DebugEvent: PDebugEvent);

    procedure SetThreadName(DebugEvent: PDebugEvent);

    procedure ProcessDbgException(DebugEvent: PDebugEvent);
    procedure ProcessDbgThreadInfo(DebugEvent: PDebugEvent);
    procedure ProcessDbgMemoryInfo(DebugEvent: PDebugEvent);
    procedure ProcessDbgPerfomance(DebugEvent: PDebugEvent);
    procedure ProcessDbgSyncObjsInfo(DebugEvent: PDebugEvent);
    procedure ProcessDbgTraceInfo(DebugEvent: PDebugEvent);
    procedure ProcessDbgSamplingInfo(DebugEvent: PDebugEvent);

    function ProcessHardwareBreakpoint(DebugEvent: PDebugEvent): LongBool;

    // ������ � ������� ���������

    function AddNewBreakPoint(var Value: TBreakpoint): LongBool;
    procedure CheckBreakpointIndex(Value: Integer);
    function CheckIsAddrInRealMemoryBPRegion(BreakPointIndex: Integer; AAddr: Pointer): LongBool;
    function GetBPIndex(BreakPointAddr: Pointer; const ThreadID: TThreadId = 0): Integer;
    function GetMBPIndex(BreakPointAddr: Pointer; FromIndex: Integer = 0): Integer;
    function IsBreakpointPresent(const Value: TBreakpoint): LongBool;
    procedure ToggleInt3Breakpoint(Index: Integer; Active: LongBool);
    procedure ToggleMemoryBreakpoint(Index: Integer; Active: LongBool);
    procedure UpdateHardwareBreakpoints(const ThreadID: TThreadId);

    function PerfomancePauseDebug: LongBool;

    function AddThreadPointInfo(ThreadData: PThreadData; const PointType: TDbgPointType; DebugEvent: PDebugEvent = nil): LongBool;
    function AddProcessPointInfo(const PointType: TDbgPointType): LongBool;

  public
    constructor Create;
    destructor Destroy; override;

    procedure ClearDbgInfo;

    procedure Log(const Msg: String);

    // ������/��������� �������
    function AttachToProcess(const ProcessID: TProcessId; SentEntryPointBreakPoint: LongBool): LongBool;
    function DebugNewProcess(const AppPath: string; var ErrInfo: String; const RunParams: String = ''; const WorkingDirectory: String = ''): LongBool;

    function StopDebug: LongBool;
    function PauseDebug: LongBool;
    function ContinueDebug: LongBool;
    function TraceDebug(const TraceType: TDbgTraceState): LongBool;

    // �������� ���� ��������� �������� �������
    procedure ProcessDebugEvents;

    // ������ ������ ������
    Function ProcAllocMem(const Size: Cardinal): Pointer;
    Procedure ProcFreeMem(Data : Pointer; const Size: Cardinal = 0);

    procedure InjectThread(hProcess: THandle; Func: Pointer; FuncSize: Cardinal; aParams: Pointer;
      aParamsSize: Cardinal; WaitAndFree: LongBool = True);
    function InjectFunc(Func: Pointer; const CodeSize: Cardinal): Pointer;

    procedure InjectPerfThread;
    procedure InjectPerfFunc;

    function ReadData(const AddrPrt, ResultPtr: Pointer; const DataSize: Integer): LongBool;

    function ReadStringA(AddrPrt: Pointer; Len: Integer = 0): AnsiString;
    function ReadStringW(AddrPrt: Pointer; Len: Integer = 0): WideString;
    function ReadStringP(AddrPrt: Pointer; Len: Byte = 0): ShortString;

    function WriteData(AddrPrt, DataPtr: Pointer; const DataSize: Cardinal): LongBool;

    procedure SetFlag(const ThreadID: TThreadId; Flag: DWORD; Value: LongBool);
    function GetFlag(const ThreadID: TThreadId; Flag: DWORD): LongBool;

    function UpdateThreadContext(const ThreadID: TThreadId; const ContextFlags: Cardinal = CONTEXT_FULL): PThreadData; overload;
    function UpdateThreadContext(ThreadData: PThreadData; const ContextFlags: Cardinal = CONTEXT_FULL): LongBool; overload;

    function UpdateCurThreadContext(const ContextFlags: Cardinal = CONTEXT_FULL): LongBool;

    function GetRegisters(const ThreadID: TThreadId): TContext;
    procedure SetRegisters(const ThreadID: TThreadId; var Context: TContext);

    procedure SetSingleStepMode(const ThreadID: TThreadId; const RestoreEIPAfterBP: LongBool); overload;
    procedure SetSingleStepMode(ThData: PThreadData; const RestoreEIPAfterBP: LongBool); overload;

    Function IsValidAddr(Const Addr: Pointer): LongBool;
    Function IsValidCodeAddr(Const Addr: Pointer): LongBool;
    Function IsValidProcessCodeAddr(Const Addr: Pointer): LongBool;

    procedure GetCallStack(ThData: PThreadData; var Stack: TDbgInfoStack);
    procedure GetCallStackEx(ThData: PThreadData; var Stack: TDbgInfoStack);

    function GetThreadData(const ThreadID: TThreadId; const UseFinished: LongBool = False): PThreadData;
    function CurThreadId: TThreadId;
    function CurThreadData: PThreadData;
    function GetThreadCount: Integer;
    function GetThreadDataByIdx(const Idx: Integer): PThreadData;
    procedure GetActiveThreads(var Res: TDbgActiveThreads);

    // ���������� ����
    Procedure ExecuteCode(AddrPtr: Pointer; const TimeOut: Cardinal);

    function GetDllName(lpImageName, lpBaseOfDll: Pointer; var Unicode: LongBool): AnsiString;

    // ������ � ������� ���������
    function SetUserBreakpoint(Address: Pointer; const ThreadId: TThreadId = 0; const Description: string = ''): LongBool;
    function SetMemoryBreakpoint(Address: Pointer; Size: Cardinal; BreakOnWrite: LongBool; const Description: string): LongBool;

    procedure RemoveBreakpoint(const Address: Pointer; const SaveByte: Byte); overload; inline;
    procedure SetBreakpoint(const Address: Pointer; var SaveByte: Byte); inline;
    procedure RestoreBreakpoint(const Address: Pointer); inline;

    procedure RemoveBreakpoint(Index: Integer); overload;
    procedure ToggleBreakpoint(Index: Integer; Active: LongBool);

    function BreakpointCount: Integer;
    function BreakpointItem(Index: Integer): TBreakpoint;

    procedure RemoveCurrentBreakpoint;

    // ������ � ����������� ������� ���������
    procedure SetHardwareBreakpoint(const ThreadId: TThreadID; Address: Pointer; Size: THWBPSize; Mode: THWBPMode; HWIndex: THWBPIndex; const Description: string);
    procedure ToggleHardwareBreakpoint(const ThreadId: TThreadID; Index: THWBPIndex; Active: LongBool);
    procedure DropHardwareBreakpoint(const ThreadId: TThreadID; Index: THWBPIndex);
    procedure DropAllHardwareBreakpoint(const ThreadId: TThreadID);

    // ���������� ������� ���������
    property OnMainLoopFailed: TNotifyEvent read FMainLoopFailed write FMainLoopFailed;
    property OnEndDebug: TNotifyEvent read FEndDebug write FEndDebug;
    property OnChangeDebugState: TNotifyEvent read FChangeDebugState write FChangeDebugState;

    // ����������� ���������� �������
    property OnCreateThread: TCreateThreadEvent read FCreateThread write FCreateThread;
    property OnCreateProcess: TCreateProcessEvent read FCreateProcess write FCreateProcess;
    property OnExitThread: TExitThreadEvent read FExitThread write FExitThread;
    property OnExitProcess: TExitProcessEvent read FExitProcess write FExitProcess;
    property OnLoadDll: TLoadDllEvent read FLoadDll write FLoadDll;
    property OnUnloadDll: TUnLoadDllEvent read FUnLoadDll write FUnLoadDll;
    property OnDebugString: TDebugStringEvent read FDebugString write FDebugString;
    property OnRip: TRipEvent read FRip write FRip;
    property OnDbgLog: TDbgLogEvent read FDbgLog write FDbgLog;

    property DbgLogMode: LongBool read FDbgLogMode write FDbgLogMode;

    // ����������� ����������
    property OnBreakPoint: TBreakPointEvent read FBreakPoint write FBreakPoint;
    property OnHardwareBreakpoint: THardwareBreakpointEvent read FHardwareBreakpoint write FHardwareBreakpoint;
    property OnUnknownException: TDefaultExceptionEvent index ecUnknown read GetExceptionEvent write SetExceptionEvent;
    property OnUnknownBreakPoint: TDefaultExceptionEvent index ecBreakpoint read GetExceptionEvent write SetExceptionEvent;
    property OnSingleStep: TDefaultExceptionEvent index ecSingleStep read GetExceptionEvent write SetExceptionEvent;
    property OnCtrlC: TDefaultExceptionEvent index ecCtrlC read GetExceptionEvent write SetExceptionEvent;
    property OnNonContinuable: TDefaultExceptionEvent index ecNonContinuable read GetExceptionEvent write SetExceptionEvent;
    property OnPageGuard: TDefaultExceptionEvent index ecGuard read GetExceptionEvent write SetExceptionEvent;

    // ����������� �������� ���������
    property ContinueStatus: DWORD read FContinueStatus write FContinueStatus;
    property CloseDebugProcessOnFree: LongBool read FCloseDebugProcess write SetCloseDebugProcess;
    property ProcessData: TProcessData read FProcessData;
    property ResumeAction: TResumeAction read FResumeAction write FResumeAction;
    property DbgState: TDbgState read FDbgState write SetDbgState;
    property DbgTraceState: TDbgTraceState read FDbgTraceState write SetDbgTraceState;

    property Active: LongBool read GetActive;

    // ����� ����������
    property PerfomanceMode: LongBool read FPerfomanceMode write SetPerfomanceMode;

    property ExceptionCheckMode: LongBool read FExceptionCheckMode write SetExceptionCheckMode;
    property ExceptionCallStack: LongBool read FExceptionCallStack write SetExceptionCallStack;

    property CodeTracking: LongBool read FCodeTracking write SetCodeTracking;
    property TrackSystemUnits: LongBool read FTrackSystemUnits write SetTrackSystemUnits;
    property SamplingMethod: LongBool read FSamplingMethod write SetSamplingMethod;

    property MemoryBPCheckMode: LongBool read FMemoryBPCheckMode write FMemoryBPCheckMode;

    property DbgMemoryProfiler: TDbgMemoryProfiler read FDbgMemoryProfiler;
    property DbgSysncObjsProfiler: TDbgSyncObjsProfiler read FDbgSyncObjsProfiler;
    property DbgSamplingProfiler: TDbgSamplingProfiler read FDbgSamplingProfiler;
    property DbgCodeProfiler: TDbgCodeProfiler read FDbgCodeProfiler;
  end;

var
  gvDebuger: TDebuger = nil;

implementation

uses
  RTLConsts, Math, DebugHook, DebugInfo, WinAPIUtils, Winapi.TlHelp32, Winapi.ImageHlp,
  System.Contnrs, System.AnsiStrings, CollectList, Collections.Base,
  DbgWorkerThread;

function _DbgPerfomanceHook(pvParam: Pointer): DWORD; stdcall;
begin
  Result := DWORD(@_DbgPerfomanceHook);
end;

function CodeDataToExceptionCode(const Value: DWORD): TExceptionCode;
const
  EXCEPTION_UNKNOWN = 0;
  ExceptionCodeData: array [TExceptionCode] of DWORD = (
    EXCEPTION_UNKNOWN,
    EXCEPTION_BREAKPOINT,
    EXCEPTION_SINGLE_STEP,
    DBG_CONTROL_C,
    EXCEPTION_NONCONTINUABLE_EXCEPTION,
    EXCEPTION_GUARD_PAGE,
    EXCEPTION_SET_THREAD_NAME
  );
begin
  for Result := Low(TExceptionCode) to High(TExceptionCode) do
    if Value = ExceptionCodeData[Result] then
      Break;

  Result := ecUnknown;
end;

{ TDebuger }

function TDebuger.AddNewBreakPoint(var Value: TBreakpoint): LongBool;
var
  Len: Integer;
begin
  Result := not IsBreakpointPresent(Value);
  if Result then
  begin
    Value.Active := True;
    Len := BreakpointCount;
    SetLength(FBreakpointList, Len + 1);
    FBreakpointList[Len] := Value;
  end;
end;

function TDebuger.AddThreadPointInfo(ThreadData: PThreadData; const PointType: TDbgPointType; DebugEvent: PDebugEvent = nil): LongBool;
var
  Cur: UInt64;
  //Prev: UInt64;
  PrevTime: UInt64;
  Delta: UInt64;
  ThPoint: PThreadPoint;
begin
  Result := False;

  if ThreadData = Nil then Exit;

  //Delta := 0;
  //Prev := 0;
  //Cur := 0;

  case PointType of
    ptStart:
      Result := True;
    ptStop:
      Result := True;
    ptException:
      Result := True;
    ptPerfomance:
      begin
        // ������������� ����� ����������
        ThreadData^.Elapsed := FProcessData.Elapsed - ThreadData^.Started;

        // ��������� ����� CPU
        PrevTime := ThreadData^.CPUTime;
        ThreadData^.CPUTime := GetThreadCPUTime(ThreadData^.ThreadHandle);
        Delta := ThreadData^.CPUTime - PrevTime;

        // ������� ������� CPU
        Cur := _QueryThreadCycleTime(ThreadData^.ThreadHandle);
        //Prev := ThreadData^.CPUElapsed;
        ThreadData^.CPUElapsed := Cur;

        // ��������� ����, ����� ����� �������
        Result := (Delta > 0);
      end;
    ptSyncObjsInfo:
      Result := True;
    ptTraceInfo:
      Result := True;
  end;

  if Result then
  begin
    //ThreadData^.DbgPoints.BeginRead;
    try
      ThPoint := PThreadPoint(ThreadData^.DbgPoints.Add);

      ThPoint^.PerfIdx := FProcessData.CurDbgPointIdx;

      ThPoint^.PointType := PointType;
      case PointType of
        ptStart:
          begin
            ThreadData^.Started :=
              FProcessData.Started + FProcessData.DbgPointByIdx(ThPoint^.PerfIdx)^.FromStart;
          end;
        ptStop:
          begin
            ThreadData^.Elapsed :=
              (FProcessData.Started + FProcessData.DbgPointByIdx(ThPoint^.PerfIdx)^.FromStart) - ThreadData^.Started;

            ThreadData^.CPUTime := GetThreadCPUTime(ThreadData^.ThreadHandle);

            ThreadData^.CPUElapsed := _QueryThreadCycleTime(ThreadData^.ThreadHandle);
          end;
        ptException:
          begin
            ThPoint^.ExceptInfo := TExceptInfo.Create(DebugEvent);
            ThreadData^.DbgExceptions.Add(ThPoint^.ExceptInfo);

            FProcessData.DbgExceptions.Add(ThPoint^.ExceptInfo);
          end;
        ptPerfomance:
          begin
            ThPoint^.PerfInfo := Nil;
            (* TODO:
            ThPoint^.PerfInfo := TPerfInfo.Create;
            ThPoint^.PerfInfo.DeltaTickCPU := Cur - Prev;
            ThPoint^.PerfInfo.DeltaTime := Delta;
            *)
          end;
        ptSyncObjsInfo:
          begin
            ThPoint^.SyncObjsInfo := TSyncObjsInfo.Create(DebugEvent, ThreadData, ThPoint^.PerfIdx);
          end;
        ptTraceInfo:
          begin
            if FDbgTraceState = dtsPause then
            begin
              ThPoint^.ExceptInfo := TExceptInfo.Create(ThreadData);
              ThPoint^.ExceptInfo.ExceptionName := Format('### DBG_TRACE #%d', [FTraceCounter]);

              ThreadData^.DbgExceptions.Add(ThPoint^.ExceptInfo);

              FProcessData.DbgExceptions.Add(ThPoint^.ExceptInfo);
            end;
          end;
      end;
    finally
      ThreadData^.DbgPoints.Commit;
      //ThreadData^.DbgPoints.EndRead;
    end;
  end;
end;

function TDebuger.AddProcessPointInfo(const PointType: TDbgPointType): LongBool;
var
  ProcPoint: PProcessPoint;
  Cur: UInt64;
  PCur: Int64;
  PrevTime: UInt64;
  CurTime: UInt64;
  Delta: UInt64;
begin
  Result := False;

  PCur := _QueryPerformanceCounter;

  CurTime := GetProcessCPUTime(FProcessData.AttachedProcessHandle);

  Delta := 0;

  case PointType of
    ptStart, ptException, ptThreadInfo, ptTraceInfo {, ptMemoryInfo}:
      begin
        Result := True;
      end;
    ptStop:
      begin
        FProcessData.Elapsed := PCur;
        FProcessData.CPUElapsed := _QueryProcessCycleTime(FProcessData.AttachedProcessHandle);
        FProcessData.CPUTime := CurTime;

        Result := True;
      end;
    ptPerfomance:
      begin
        // ������ ����������� �������
        FProcessData.Elapsed := PCur;

        // ������ �������� ������� CPU
        Cur := _QueryProcessCycleTime(FProcessData.AttachedProcessHandle);
        FProcessData.CPUElapsed := Cur;

        // ����� CPU ��������
        PrevTime := FProcessData.CPUTime;
        FProcessData.CPUTime := CurTime;
        Delta := CurTime - PrevTime;

        // ��������� ������ ���� ������� �������
        Result := (Delta > 0); // TODO: ���������� ����������� �����
      end;
  end;

  if Result then
  begin
    ProcPoint := FProcessData.DbgPoints.Add;

    ProcPoint^.FromStart := PCur - FProcessData.Started;
    ProcPoint^.CPUTime := CurTime;

    ProcPoint^.PointType := PointType;
    case PointType of
      ptPerfomance:
        begin
          ProcPoint^.DeltaTime := Delta;
        end;
    end;

    FProcessData.DbgPoints.Commit;
  end;
end;

function TDebuger.AddThread(const ThreadID: TThreadId; ThreadHandle: THandle): PThreadData;
begin
  Result := FThreadList.Add;

  Result^.Init;

  Result^.ThreadID := ThreadID;
  Result^.State := tsActive;
  Result^.ThreadHandle := ThreadHandle;

  FActiveThreadList.AddOrSetValue(ThreadId, Result);

  Result^.ThreadAdvInfo := SetThreadInfo(ThreadId);
  Result^.ThreadAdvInfo^.ThreadData := Result;

  FThreadList.Commit;

  if AddProcessPointInfo(ptThreadInfo) then
    AddThreadPointInfo(Result, ptStart);
end;

function TDebuger.AddThreadInfo(const ThreadId: TThreadId): PThreadAdvInfo;
begin
  Result := FThreadAdvInfoList.Add;

  Result^.ThreadId := ThreadId;
  Result^.ThreadData := Nil;

  FThreadAdvInfoList.Commit;
end;

function TDebuger.ProcAllocMem(const Size: Cardinal): Pointer;
begin
  // TODO: ��������� ��������� ������ ��� ��������� Size
  Result := VirtualAllocEx(FProcessData.AttachedProcessHandle, Nil, Size, MEM_COMMIT Or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
  If Result = nil Then
    RaiseLastOsError;
end;

function TDebuger.AttachToProcess(const ProcessID: TProcessId; SentEntryPointBreakPoint: LongBool): LongBool;
begin
  LoadLibrary('DbgHook32.dll'); // ��� ������� �������� � ��������

  Result := False;

  if FProcessData.State = psActive then
    Exit;

  FSetEntryPointBreakPoint := SentEntryPointBreakPoint;

  FProcessData.ProcessID := ProcessID;

  Result := DebugActiveProcess(Cardinal(ProcessID));
end;

function TDebuger.BreakpointCount: Integer;
begin
  Result := Length(FBreakpointList);
end;

function TDebuger.BreakpointItem(Index: Integer): TBreakpoint;
begin
  CheckBreakpointIndex(Index);
  Result := FBreakpointList[Index];
end;

procedure TDebuger.CallUnhandledBreakPointEvents(const Code: TExceptionCode; DebugEvent: PDebugEvent);
begin
  //ContinueStatus := DBG_EXCEPTION_NOT_HANDLED;

  if Assigned(FExceptionEvents[Code]) then
    FExceptionEvents[Code](Self, DebugEvent^.dwThreadId, @DebugEvent^.Exception.ExceptionRecord);
end;

procedure TDebuger.CallUnhandledExceptionEvents(const Code: TExceptionCode; DebugEvent: PDebugEvent);
var
  IsTraceException: LongBool;
begin
  if gvDebugInfo.CheckDebugException(@DebugEvent^.Exception.ExceptionRecord, IsTraceException) then
  begin
    if IsTraceException then
    begin
      // TODO:
    end;

    ContinueStatus := DBG_CONTINUE;
  end
  else
  begin
    if DebugEvent^.Exception.dwFirstChance = 1 then
    begin
      ContinueStatus := DBG_EXCEPTION_NOT_HANDLED;

      if AddProcessPointInfo(ptException) then
        AddThreadPointInfo(CurThreadData, ptException, DebugEvent);

      if Assigned(FExceptionEvents[Code]) then
        FExceptionEvents[Code](Self, DebugEvent^.dwThreadId, @DebugEvent^.Exception.ExceptionRecord);
    end
    else
      ContinueStatus := DBG_CONTINUE;
  end;
end;

procedure TDebuger.CheckBreakpointIndex(Value: Integer);
begin
  if (Value < 0) or (Value >= BreakpointCount) then
    raise EDebugCoreException.CreateFmt(SListIndexError, [Value]);
end;

function TDebuger.CheckIsAddrInRealMemoryBPRegion(BreakPointIndex: Integer; AAddr: Pointer): LongBool;
begin
  CheckBreakpointIndex(BreakPointIndex);
  Result := Cardinal(AAddr) >= Cardinal(FBreakpointList[BreakPointIndex].Memory.Address);
  if Result then
    Result := Cardinal(AAddr) < Cardinal(FBreakpointList[BreakPointIndex].Memory.Address) + FBreakpointList[BreakPointIndex].Memory.Size;
end;

procedure TDebuger.ClearDbgInfo;
var
  I: Integer;
  ThData: PThreadData;
begin
  DbgState := dsNone;

  FDbgMemoryProfiler.Clear;
  FDbgSyncObjsProfiler.Clear;
  FDbgSamplingProfiler.Clear;
  FDbgCodeProfiler.Clear;

  FProcessData.Clear;

  try
    FActiveThreadList.Clear;

    FThreadList.BeginWrite;
    try
      for I := 0 to FThreadList.Count - 1 do
      begin
        ThData := FThreadList[I];
        ThData.Clear;
      end;
    finally
      FThreadList.Clear;
      FThreadList.EndWrite;
    end;
  finally
    FThreadAdvInfoList.Clear;

    FTraceCounter := 0;
  end;
end;

function TDebuger.ContinueDebug: LongBool;
begin
  Result := False;

  if DbgTraceState = dtsPause then
  begin
    DbgTraceState := dtsContinue;

    FTraceEvent.SetEvent;
    Result := True;
  end;
end;

constructor TDebuger.Create();

  function SetDebugPriv: LongBool;
  var
    Token: THandle;
    tkp: TTokenPrivileges;
  begin
    Result := False;
    if OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, Token) then
    begin
      if LookupPrivilegeValue(nil, PChar('SeDebugPrivilege'), tkp.Privileges[0].Luid) then
      begin
        tkp.PrivilegeCount := 1;
        tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        Result := AdjustTokenPrivileges(Token, False, tkp, 0, PTokenPrivileges(nil)^, PCardinal(nil)^);
      end;
    end;
  end;

begin
  inherited Create;

  if not SetDebugPriv then
    RaiseLastOSError;

  FDbgState := dsNone;
  FDbgTraceState := dtsContinue;

  FTraceEvent := TEvent.Create(nil, True, False, '');

  FRestoreBPIndex := -1;
  FRestoreMBPIndex := -1;
  FRestoredHWBPIndex := -1;
  FRestoredThread := 0;
  FCloseDebugProcess := True;
  FSetEntryPointBreakPoint := False;
  FDbgLogMode := False;
  FMemoryBPCheckMode := False;

  FThreadList := TCollectList<TThreadData>.Create;
  FThreadAdvInfoList := TCollectList<TThreadAdvInfo>.Create;
  FActiveThreadList := TDbgActiveThreadList.Create(512, True);

  FProcessData := TProcessData.Create;

  FPerfomanceMode := False;
  FPerfomanceCheckPtr := Nil; //Pointer($76FED315);

  FDbgSamplingProfiler := TDbgSamplingProfiler.Create;
  FDbgMemoryProfiler := TDbgMemoryProfiler.Create;
  FDbgSyncObjsProfiler := TDbgSyncObjsProfiler.Create;
  FDbgCodeProfiler := TDbgCodeProfiler.Create;
end;

function TDebuger.CurThreadData: PThreadData;
begin
  if FCurThreadData = Nil then
    UpdateCurThreadContext;

  Result := FCurThreadData;
end;

function TDebuger.CurThreadId: TThreadId;
begin
  Result := FCurThreadId;
end;

function TDebuger.DebugNewProcess(const AppPath: string; var ErrInfo: String; const RunParams: String = ''; const WorkingDirectory: String = ''): LongBool;
var
  PI: PProcessInformation;
  SI: PStartupInfo;
  CmdLine: String;
  PCmdLine: PChar;
  PAppName: PChar;
  PWorkDir: PChar;
begin
  LoadLibrary('DbgHook32.dll'); // ��� ������� �������� � ��������

  Result := False;
  if FProcessData.State = psActive then
    Exit;

  //FSetEntryPointBreakPoint := SentEntryPointBreakPoint;
  FSetEntryPointBreakPoint := False;

  PI := AllocMem(SizeOf(TProcessInformation));
  SI := AllocMem(SizeOf(TStartupInfo));
  try
    SI.cb := SizeOf(TStartupInfo);
    SI.dwFlags := STARTF_USESHOWWINDOW;
    SI.wShowWindow := SW_SHOWNORMAL;

    PAppName := nil;
    PCmdLine := nil;
    PWorkDir := nil;

    if RunParams <> '' then
    begin
      CmdLine := Format('"%s" %s', [AppPath, RunParams]);
      PCmdLine := PChar(CmdLine);
    end
    else
      PAppName := PChar(AppPath);

    if (WorkingDirectory <> '') and (DirectoryExists(WorkingDirectory)) then
      PWorkDir := PChar(WorkingDirectory);

    Result := CreateProcess(PAppName, PCmdLine, nil, nil, False, DEBUG_PROCESS or DEBUG_ONLY_THIS_PROCESS, nil,
      PWorkDir, SI^, PI^);

    if Result then
    begin
      FProcessData.ProcessID := TProcessId(PI.dwProcessId);
      FProcessData.CreatedProcessHandle := PI.hProcess;
      FProcessData.CreatedThreadHandle := PI.hThread;
    end
    else
      ErrInfo := SysErrorMessage(GetLastError);
  finally
    FreeMemory(PI);
    FreeMemory(SI);
  end;
end;

destructor TDebuger.Destroy;
begin
  StopDebug;

  ClearDbgInfo;

  FreeAndNil(FActiveThreadList);
  FreeAndNil(FThreadList);
  FreeAndNil(FThreadAdvInfoList);

  FreeAndNil(FProcessData);

  FreeAndNil(FTraceEvent);

  FreeAndNil(FDbgMemoryProfiler);
  FreeAndNil(FDbgSyncObjsProfiler);
  FreeAndNil(FDbgSamplingProfiler);
  FreeAndNil(FDbgCodeProfiler);

  inherited;
end;

procedure TDebuger.DoCreateProcess(DebugEvent: PDebugEvent);
var
  CreateThreadInfo: PCreateThreadDebugInfo;
begin
  DbgState := dsStarted;

  FProcessData.State := psActive;

  // ��������� ������ � ��������
  FProcessData.AttachedFileHandle := DebugEvent^.CreateProcessInfo.hFile;
  FProcessData.AttachedProcessHandle := DebugEvent^.CreateProcessInfo.hProcess;
  FProcessData.AttachedThreadHandle := DebugEvent^.CreateProcessInfo.hThread;

  FProcessData.StartAddress := DebugEvent^.CreateProcessInfo.lpStartAddress;
  FProcessData.BaseOfImage := DebugEvent^.CreateProcessInfo.lpBaseOfImage;
  FProcessData.MainThreadID := DebugEvent^.dwThreadId;

  FProcessData.Started := _QueryPerformanceCounter;
  FProcessData.DbgPoints := TCollectList<TProcessPoint>.Create;
  FProcessData.DbgGetMemInfo := TGetMemInfoList.Create(1024, True);
  FProcessData.DbgGetMemInfo.OwnsValues := True;

  FProcessData.ProcessGetMemCount := 0;
  FProcessData.ProcessGetMemSize := 0;

  FProcessData.DbgExceptions.Clear;

  FProcessData.DbgTrackEventCount := 0;
  FProcessData.DbgTrackUnitList := TCodeTrackUnitInfoList.Create(4096);
  FProcessData.DbgTrackUnitList.OwnsValues := True;
  FProcessData.DbgTrackFuncList := TCodeTrackFuncInfoList.Create(4096);
  FProcessData.DbgTrackFuncList.OwnsValues := True;
  FProcessData.DbgTrackUsedUnitList := TTrackUnitInfoList.Create(64);
  FProcessData.DbgTrackUsedUnitList.OwnsKeys := False;
  FProcessData.DbgTrackUsedUnitList.OwnsValues := False;

  //DbgTrackBreakpoints := nil;
  //DbgTrackRETBreakpoints := nil;

  // ����� ������ ��������
  AddProcessPointInfo(ptStart);

  // ������������� �����
  //LoadLibrary('DbgHook32.dll'); // ??? ���������� �� ��������������� ��������

  if Assigned(gvDebugInfo) then
    gvDebugInfo.InitDebugHook;

  // ������������� BreakPoint �� ����� ����� ��������
  if FSetEntryPointBreakPoint then
    SetUserBreakpoint(FProcessData.StartAddress, 0, 'Process Entry Point Breakpoint');

  if Assigned(FCreateProcess) then
    FCreateProcess(Self, DebugEvent^.dwProcessId, @DebugEvent^.CreateProcessInfo);

  AddThread(DebugEvent^.dwThreadId, FProcessData.AttachedThreadHandle);

  with SetThreadInfo(DebugEvent^.dwThreadId)^ do
  begin
    ThreadName := 'Main thread';
    ThreadAdvType := tatNormal;
  end;

  if Assigned(FCreateThread) then
  begin
    CreateThreadInfo := AllocMem(SizeOf(TCreateThreadDebugInfo));
    try
      CreateThreadInfo.hThread := FProcessData.AttachedThreadHandle;
      FCreateThread(Self, DebugEvent^.dwThreadId, CreateThreadInfo);
    finally
      FreeMemory(CreateThreadInfo);
    end;
  end;

  // ������ ������ �� ��������� ������
  if CodeTracking and SamplingMethod then
   DbgSamplingProfiler.InitSamplingTimer;

  TDbgWorkerThread.Init;

  DoResumeAction(DebugEvent^.dwThreadId);
end;

procedure TDebuger.DoCreateThread(DebugEvent: PDebugEvent);
begin
  AddThread(DebugEvent^.dwThreadId, DebugEvent^.CreateThread.hThread);

  if Assigned(FCreateThread) then
    FCreateThread(Self, DebugEvent^.dwThreadId, @DebugEvent^.CreateThread);
end;

procedure TDebuger.DoDebugString(DebugEvent: PDebugEvent);
begin
  if Assigned(FDebugString) then
    FDebugString(Self, DebugEvent.dwThreadId, @DebugEvent^.DebugString);
end;

procedure TDebuger.DoExitProcess(DebugEvent: PDebugEvent);
begin
  DbgSamplingProfiler.ResetSamplingTimer;

  TDbgWorkerThread.Reset;

  DbgState := dsStoping;
  FProcessData.State := psFinished;

  // ����� ���������� ��������
  AddProcessPointInfo(ptStop);

  // ������� ������� �����
  if Assigned(FExitThread) then
    FExitThread(Self, FProcessData.MainThreadID, nil);
  RemoveThread(FProcessData.MainThreadID);

  if Assigned(FExitProcess) then
    FExitProcess(Self, FProcessData.ProcessID, @DebugEvent^.ExitProcess);

  if FProcessData.AttachedFileHandle <> 0 then
  begin
    CloseHandle(FProcessData.AttachedFileHandle);
    FProcessData.AttachedFileHandle := 0;
  end;

  //FreeLibrary('DbgHook32.dll');
end;

procedure TDebuger.DoExitThread(DebugEvent: PDebugEvent);
begin
  if FPerfomanceMode and (DebugEvent^.ExitThread.dwExitCode = Cardinal(@_DbgPerfomanceHook)) then
    Exit;

  if Assigned(FExitThread) then
    FExitThread(Self, DebugEvent^.dwThreadId, @DebugEvent^.ExitThread);

  RemoveThread(DebugEvent^.dwThreadId);
end;

type
  PThreadRec = ^TThreadRec;
  TThreadRec = record
    Func: TThreadFunc;
    Parameter: Pointer;
  end;

procedure TDebuger.DoEndDebug;
begin
  DbgState := dsStoped;

  if Assigned(FEndDebug) then
    FEndDebug(Self);
end;

procedure TDebuger.DoLoadDll(DebugEvent: PDebugEvent);
begin
  if Assigned(FLoadDll) then
  begin
    FLoadDll(Self, DebugEvent^.dwThreadId, @DebugEvent^.LoadDll);
    DoResumeAction(DebugEvent^.dwThreadId);
  end;
  //CloseHandle(DebugEvent^.LoadDll.hFile); ???
end;

procedure TDebuger.DoDbgLog(const ThreadId: TThreadId; const LogData: String);
begin
  if FDbgLogMode and Assigned(FDbgLog) then
    FDbgLog(Self, ThreadId, LogData);
end;

procedure TDebuger.DoDebugerFailed;
begin
  DbgState := dsDbgFail;

  if Assigned(FMainLoopFailed) then
    FMainLoopFailed(Self);
end;

(*
procedure TDebuger.DoRestoreBreakpoint(const Address: Pointer);
var
  OldProtect: DWORD;
  Dummy: TSysUInt;
begin
  Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, Address, 1, PAGE_READWRITE, OldProtect));
  try
    Check(WriteProcessMemory(FProcessData.AttachedProcessHandle, Address, @BPOpcode, 1, Dummy));
  finally
    Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, Address, 1, OldProtect, OldProtect));
  end;
end;
*)

procedure TDebuger.DoRestoreBreakpointF(const Address: Pointer);
var
  Dummy: TSysUInt;
begin
  Check(WriteProcessMemory(FProcessData.AttachedProcessHandle, Address, @BPOpcode, 1, Dummy));
end;

procedure TDebuger.DoResumeAction(const ThreadID: TThreadId);
begin
  // ����� ����� ��������� ���������� ���������
  // ����� ���� ��� ������������ ��������� ����� �� �������
  // �������� ��������� ��� ����� ������� TraceIn ��� StepOver � �.�.
  // � ����������� �� ���� ��� ������ ��� ������������

  case ResumeAction of
    raTraceInto:
      SetSingleStepMode(ThreadID, False);

    raStepOver:
      { ������������� �� �� ��������� ���������� } ;

    { ��� ���������� ����� ������ ����������� ������, �.�. StepOver
      ����������� ��� ����� ���� ����������, ��� ������:
      CALL, INT, LOOP, LOOPZ, LOOPNZ, REP, REPZ, REPNZ, CMPS, CMPSB, CMPSW,
      LODSB, LODSW, MOVS, MOVSB, MOVSW, SCAS, SCASB, SCASW, STOS, STOSB, STOSW
      }

    raRunUntilReturn:
      { ����� �� ����� ����� �������� � ������ ��, ���� ���� �� ����������� } ;

    raStop:
      ContinueStatus := DBG_CONTROL_C;
  end;
end;

procedure TDebuger.DoRip(DebugEvent: PDebugEvent);
begin
  DbgState := dsDbgFail;

  if Assigned(FRip) then
    FRip(Self, DebugEvent^.dwThreadId, @DebugEvent^.RipInfo);
end;

procedure TDebuger.DoUnLoadDll(DebugEvent: PDebugEvent);
begin
  if Assigned(FUnLoadDll) then
  begin
    FUnLoadDll(Self, DebugEvent^.dwThreadId, @DebugEvent^.UnloadDll);
    DoResumeAction(DebugEvent^.dwThreadId);
  end;
end;

procedure TDebuger.DropAllHardwareBreakpoint(const ThreadId: TThreadID);
var
  I: THWBPIndex;
  NeedUpdate: LongBool;
  ThData: PThreadData;
begin
  ThData := GetThreadData(ThreadID);
  if ThData <> nil then
  begin
    NeedUpdate := False;
    for I := 0 to 3 do
      if ThData^.Breakpoint.Address[I] <> nil then
      begin
        NeedUpdate := True;
        ThData^.Breakpoint.Address[I] := nil;
      end;
    if NeedUpdate then
      UpdateHardwareBreakpoints(ThreadId);
  end;
end;

procedure TDebuger.DropHardwareBreakpoint(const ThreadId: TThreadID; Index: THWBPIndex);
var
  ThData: PThreadData;
begin
  ThData := GetThreadData(ThreadID);
  if ThData <> nil then
  begin
    if ThData^.Breakpoint.Address[Index] = nil then
      Exit;
    ThData^.Breakpoint.Address[Index] := nil;
    UpdateHardwareBreakpoints(ThreadId);
  end;
end;

procedure TDebuger.ExecuteCode(AddrPtr: Pointer; const TimeOut: Cardinal);
begin

end;

procedure TDebuger.ProcFreeMem(Data: Pointer; const Size: Cardinal = 0);
begin
  if VirtualFreeEx(FProcessData.AttachedProcessHandle, Data, Size, MEM_RELEASE) = nil then
    RaiseLastOSError;
end;

function TDebuger.GetActive: LongBool;
const
  DBG_STOPED_STATE = [dsNone, dsStoped, dsDbgFail];
begin
  Result := not(FDbgState in DBG_STOPED_STATE);
end;

procedure TDebuger.GetActiveThreads(var Res: TDbgActiveThreads);
begin
  FActiveThreadList.LockForRead;

  SetLength(Res, FActiveThreadList.Count);
  FActiveThreadList.Values.CopyTo(Res);

  FActiveThreadList.UnLockForRead;
end;

function TDebuger.GetBPIndex(BreakPointAddr: Pointer; const ThreadID: TThreadId = 0): Integer;
var
  BP: PBreakpoint;
begin
  for Result := 0 to BreakpointCount - 1 do
  begin
    BP := @FBreakpointList[Result];
    if BP^.bpType <> btUser then
      Continue;

    if (BP^.Int3.Address = BreakPointAddr) and ((ThreadID = 0) or (BP^.ThreadId = ThreadId)) then
      Exit;
  end;

  Result := -1;
end;

procedure TDebuger.GetCallStack(ThData: PThreadData; var Stack: TDbgInfoStack);
const
  _MAX_STACK_CNT = 64;

  function AddStackEntry(Const Addr: Pointer; var Cnt: Integer): LongBool;
  begin
    Result := (Cnt < _MAX_STACK_CNT) and IsValidAddr(Addr);

    if Result then
    begin
      Stack[Cnt] := Addr;
      Inc(Cnt);
    end;
  end;

Var
  EIP : Pointer;
  EBP : Pointer;
  Cnt: Integer;
Begin
  EIP := Pointer(ThData^.Context^.Eip);
  EBP := Pointer(ThData^.Context^.Ebp);

  SetLength(Stack, _MAX_STACK_CNT);
  Cnt := 0;

  if AddStackEntry(EIP, Cnt) then
  begin
    while IsValidAddr(EBP) Do
    begin
      if not ReadData(IncPointer(EBP, SizeOf(Pointer)), @EIP, SizeOf(Pointer)) then
        Break;

      if not ReadData(EBP, @EBP, SizeOf(Pointer)) then
        Break;

      if not AddStackEntry(EIP, Cnt) then
        Break;
    end;
  end;

  SetLength(Stack, Cnt);
end;

procedure TDebuger.GetCallStackEx(ThData: PThreadData; var Stack: TDbgInfoStack);
const
  _MAX_STACK_CNT = 64;
var
  {$IFDEF WIN32}
  StackFrame: TStackFrame;
  {$ELSE}
  StackFrame: TStackFrame64;
  {$ENDIF}
  ThreadContext: PContext;
  MachineType: DWORD;
  Cnt: Integer;
begin
  ZeroMemory(@StackFrame, SizeOf(TStackFrame));

  StackFrame.AddrPC.Mode := AddrModeFlat;
  StackFrame.AddrStack.Mode := AddrModeFlat;
  StackFrame.AddrFrame.Mode := AddrModeFlat;

  ThreadContext := ThData^.Context;

  {$IFDEF WIN32}
  StackFrame.AddrPC.Offset := ThreadContext.Eip;
  StackFrame.AddrStack.Offset := ThreadContext.Esp;
  StackFrame.AddrFrame.Offset := ThreadContext.Ebp;
  MachineType := IMAGE_FILE_MACHINE_I386;
  {$ELSE}
  StackFrame.AddrPC.Offset := ThreadContext.Rip;
  StackFrame.AddrStack.Offset := ThreadContext.Rsp;
  StackFrame.AddrFrame.Offset := ThreadContext.Rbp;
  MachineType := IMAGE_FILE_MACHINE_AMD64;
  {$ENDIF}

  SetLength(Stack, _MAX_STACK_CNT); // TODO: ���������� �� ����������� ������

  Cnt := 0;
  while Cnt < Length(Stack) do
  begin
    {$IFDEF WIN32}
    if not StackWalk(MachineType, ProcessData.AttachedProcessHandle, ThData^.ThreadHandle, @StackFrame, ThreadContext, nil, nil, nil, nil) then
      Break;
    {$ELSE}
    if not StackWalk64(MachineType, hProcess, hThread, StackFrame, ThreadContext, nil, nil, nil, nil) then
      Break;
    {$ENDIF}
    Stack[Cnt] := Pointer(StackFrame.AddrPC.Offset);

    Inc(Cnt);
  end;

  SetLength(Stack, Cnt);
end;

function GetMappedFileNameA(hProcess: THandle; lpv: Pointer; lpFilename: LPSTR; nSize: DWORD): DWORD; stdcall; external 'psapi.dll';

function TDebuger.GetDllName(lpImageName, lpBaseOfDll: Pointer; var Unicode: LongBool): AnsiString;
var
  DllNameAddr: Pointer;
  MappedName: array [0 .. MAX_PATH - 1] of AnsiChar;
begin
  Result := '';

  if ReadData(lpImageName, @DllNameAddr, 4) then
  begin
    SetLength(Result, MAX_PATH shl 1);
    if not ReadData(DllNameAddr, @Result[1], MAX_PATH shl 1) then
      Result := '';
  end;

  if Result = '' then
  begin
    if GetMappedFileNameA(FProcessData.AttachedProcessHandle, lpBaseOfDll, @MappedName[0], MAX_PATH) > 0 then
    begin
      Result := PAnsiChar(@MappedName[0]);
      Unicode := False;
    end;
  end;
end;

function TDebuger.GetExceptionEvent(const Index: TExceptionCode): TDefaultExceptionEvent;
begin
  Result := FExceptionEvents[Index];
end;

function TDebuger.GetFlag(const ThreadID: TThreadId; Flag: DWORD): LongBool;
var
  Context: TContext;
  ThData: PThreadData;
begin
  Result := False;
  ThData := GetThreadData(ThreadId);
  if ThData <> nil then
  begin
    ZeroMemory(@Context, SizeOf(Context));
    Context.ContextFlags := CONTEXT_FULL;
    Check(GetThreadContext(ThData^.ThreadHandle, Context));
    Result := Context.EFlags and Flag = 1;
  end;
end;

function TDebuger.GetMBPIndex(BreakPointAddr: Pointer; FromIndex: Integer): Integer;

  function CheckStartAddr(Data: Pointer): LongBool;
  begin
    Result := Cardinal(Data) <= Cardinal(BreakPointAddr);
  end;

  function CheckEndAddr(Data: Pointer; Size: DWORD): LongBool;
  begin
    Result := Cardinal(Data) + Size > Cardinal(BreakPointAddr);
  end;

var
  I: Integer;
begin
  Result := -1;
  for I := FromIndex to BreakpointCount - 1 do
  begin
    if FBreakpointList[I].bpType <> btMemory then
      Continue;
    if CheckStartAddr(FBreakpointList[I].Memory.RegionStart) and CheckEndAddr(FBreakpointList[I].Memory.RegionStart,
      FBreakpointList[I].Memory.RegionSize) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TDebuger.GetRegisters(const ThreadID: TThreadId): TContext;
var
  ThData: PThreadData;
begin
  if (ThreadID = FCurThreadId) then
    Result := CurThreadData^.Context^
  else
  begin
    ThData := UpdateThreadContext(ThreadId);
    if ThData <> nil then
      Result := ThData^.Context^
    else
      RaiseDebugCoreException();
  end;
end;

function TDebuger.GetThreadIndex(const ThreadID: TThreadId; const UseFinished: LongBool = False): Integer;
var
  ThData: PThreadData;
begin
  for Result := FThreadList.Count - 1 downto 0 do
  begin
    ThData := FThreadList[Result];
    if (ThData^.ThreadID = ThreadID) and ((ThData^.State <> tsFinished) or UseFinished) then
      Exit;
  end;

  Result := -1;
end;

function TDebuger.GetThreadInfo(const ThreadId: TThreadId): PThreadAdvInfo;
var
  Idx: Integer;
begin
  Result := Nil;

  FThreadAdvInfoList.BeginRead;
  try
    Idx := GetThreadInfoIndex(ThreadId);
    if Idx >= 0 then
      Result := FThreadAdvInfoList[Idx];
  finally
    FThreadAdvInfoList.EndRead;
  end;
end;

function TDebuger.GetThreadInfoIndex(const ThreadId: TThreadId): Integer;
var
  ThInfo: PThreadAdvInfo;
begin
  FThreadAdvInfoList.BeginRead;
  try
    for Result := FThreadAdvInfoList.Count - 1 downto 0 do
    begin
      ThInfo := FThreadAdvInfoList[Result];
      if (ThInfo^.ThreadID = ThreadID) and
        ((ThInfo^.ThreadData = Nil) or (ThInfo^.ThreadData^.State <> tsFinished))
      then
        Exit;
    end;
  finally
    FThreadAdvInfoList.EndRead;
  end;

  Result := -1;
end;

function TDebuger.GetThreadCount: Integer;
begin
  Result := FThreadList.Count;
end;

function TDebuger.GetThreadData(const ThreadID: TThreadId; const UseFinished: LongBool = False): PThreadData;
var
  Index: Integer;
begin
  Result := nil;

  if not FActiveThreadList.TryGetValue(ThreadID, Result) then
  begin
    if UseFinished then
    begin
      Index := GetThreadIndex(ThreadId, UseFinished);
      if Index >= 0 then
        Result := FThreadList[Index];
    end;
  end;
end;

function TDebuger.GetThreadDataByIdx(const Idx: Integer): PThreadData;
begin
  Result := Nil;

  if Idx < FThreadList.Count then
    Result := FThreadList[Idx];
end;

function TDebuger.IsBreakpointPresent(const Value: TBreakpoint): LongBool;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to BreakpointCount - 1 do
    case FBreakpointList[I].bpType of
      btUser:
        if FBreakpointList[I].Int3.Address = Value.Int3.Address then
        begin
          Result := True;
          Break;
        end;
      btMemory:
        if CheckIsAddrInRealMemoryBPRegion(I, Value.Memory.Address) or
          CheckIsAddrInRealMemoryBPRegion(I, Pointer(Cardinal(Value.Memory.Address) + Value.Memory.Size - 1)) then
        begin
          Result := True;
          Break;
        end;
    end;
end;

threadvar
  _mbi: TMemoryBasicInformation;

function TDebuger.IsValidAddr(const Addr: Pointer): LongBool;
Var
  mbi: PMemoryBasicInformation;
Begin
  Result := False;

  if (Addr = nil) or (Addr = Pointer(-1)) then Exit;

  mbi := @_mbi;

  Result := (VirtualQueryEx(FProcessData.AttachedProcessHandle, Addr, mbi^, SizeOf(TMemoryBasicInformation)) <> 0);
end;

function TDebuger.IsValidCodeAddr(const Addr: Pointer): LongBool;
Const
  _PAGE_CODE = DWORD(PAGE_EXECUTE Or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE Or PAGE_EXECUTE_WRITECOPY);
Var
  mbi: PMemoryBasicInformation;
Begin
  Result := False;

  mbi := @_mbi;

  if (VirtualQueryEx(FProcessData.AttachedProcessHandle, Addr, mbi^, SizeOf(TMemoryBasicInformation)) <> 0) then
    Result := ((mbi^.Protect And _PAGE_CODE) <> 0);
end;

function TDebuger.IsValidProcessCodeAddr(const Addr: Pointer): LongBool;
Begin
  Result := False;

  if FProcessData.PEImage <> Nil then
    Result := (Cardinal(Addr) >= Cardinal(FProcessData.BaseOfImage)) and
      (Cardinal(Addr) <= (Cardinal(FProcessData.BaseOfImage) + FProcessData.PEImage.OptionalHeader32.SizeOfCode));
end;

function TDebuger.PauseDebug: LongBool;
begin
  if Active then
  begin
    DbgTraceState := dtsPause;
    Result := DebugBreakProcess(FProcessData.AttachedProcessHandle);
  end
  else
    Result := True;
end;

function TDebuger.InjectFunc(Func: Pointer; const CodeSize: Cardinal): Pointer;
begin
  Result := VirtualAllocEx(FProcessData.AttachedProcessHandle, nil, CodeSize, MEM_COMMIT, PAGE_READWRITE);

  if not(Assigned(Result) and WriteData(Result, Func, CodeSize)) then
    RaiseDebugCoreException(Format('Fail inject func [%p]: %d', [Func, GetLastError]));

  //VirtualFreeEx(ProcessID, ThreadAddr, 0, MEM_RELEASE);   // TODO: on Stop debug
end;

procedure TDebuger.InjectPerfFunc;
begin
  FPerfomanceCheckPtr := InjectFunc(@_DbgPerfomanceHook, 256);
end;

procedure TDebuger.InjectPerfThread;
var
  hThread: THandle;
  lpThreadId: Cardinal;
begin
  if (FProcessData.AttachedProcessHandle <> 0) and (DbgState = dsWait) then
  begin
    if not Assigned(FPerfomanceCheckPtr) then
      InjectPerfFunc;

    hThread := CreateRemoteThread(FProcessData.AttachedProcessHandle, nil, 0, FPerfomanceCheckPtr, Nil, 0, lpThreadId);
    if hThread <> 0 then
    begin
      WaitForSingleObject(hThread, INFINITE);

      CloseHandle(hThread);
    end;
  end
end;

procedure TDebuger.InjectThread(hProcess: THandle; Func: Pointer; FuncSize: Cardinal; aParams: Pointer;
  aParamsSize: Cardinal; WaitAndFree: LongBool = True);
var
  hThread: THandle;
  lpNumberOfBytes: TSysUInt;
  lpThreadId: Cardinal;
  ThreadAddr, ParamAddr: Pointer;
begin
  // �������� ����� � ������ ��������, � ���������� ���� ���� �������
  ThreadAddr := VirtualAllocEx(hProcess, nil, FuncSize, MEM_COMMIT, PAGE_READWRITE);
  if not WriteProcessMemory(hProcess, ThreadAddr, Func, FuncSize, lpNumberOfBytes) then
    RaiseDebugCoreException();

  // ����� ������� ��������� � ���
  if (aParams <> nil) and (aParamsSize > 0) then
  begin
    ParamAddr := VirtualAllocEx(hProcess, nil, aParamsSize, MEM_COMMIT, PAGE_READWRITE);
    if not WriteProcessMemory(hProcess, ParamAddr, aParams, aParamsSize, lpNumberOfBytes) then
      RaiseDebugCoreException();
  end
  else
    ParamAddr := Nil;

  // ������� �����, � ������� ��� ��� ����� �����������.
  hThread := CreateRemoteThread(hProcess, nil, 2048, ThreadAddr, ParamAddr, CREATE_SUSPENDED, lpThreadId);

  if not SetThreadPriority(hThread, THREAD_PRIORITY_NORMAL{THREAD_PRIORITY_TIME_CRITICAL}) then
    RaiseDebugCoreException();

  if ResumeThread(hThread) = Cardinal(-1) then
    RaiseDebugCoreException();

  if WaitAndFree then
  begin
    // ������� ���������� �������
    WaitForSingleObject(hThread, INFINITE);

    // ��������� �� �����
    CloseHandle(hThread);
    VirtualFreeEx(hProcess, ParamAddr, 0, MEM_RELEASE);
    VirtualFreeEx(hProcess, ThreadAddr, 0, MEM_RELEASE);
  end;
end;

function TDebuger.PerfomancePauseDebug: LongBool;
begin
  // ��� ������� ������� ����� � �������� � ��������� ��������, ������� �������� DebugBreak
  // �������� � ���, ��� �� ���������, ���� ��������� ������������
  //Result := DebugBreakProcess(FProcessInfo.AttachedProcessHandle);

  InjectPerfThread;
  Result := True;
end;

procedure TDebuger.ProcessExceptionBreakPoint(DebugEvent: PDebugEvent);
begin
  if FCodeTracking or FMemoryBPCheckMode then
  begin
    if DbgCodeProfiler.ProcessTrackRETBreakPoint(DebugEvent) then
      Exit;

    if DbgCodeProfiler.ProcessTrackBreakPoint(DebugEvent) then
      Exit;
  end;

  if ProcessUserBreakPoint(DebugEvent) then
    Exit;

  if ProcessTraceBreakPoint(DebugEvent) then
    Exit;

  CallUnhandledBreakPointEvents(ecBreakpoint, DebugEvent);
end;

procedure TDebuger.ProcessExceptionGuardPage(DebugEvent: PDebugEvent);
var
  CurrentMBPIndex: Integer;

  function CheckWriteMode: LongBool;
  begin
    Result := not FBreakpointList[CurrentMBPIndex].Memory.BreakOnWrite;
    if not Result then
      Result := DebugEvent^.Exception.ExceptionRecord.ExceptionInformation[0] = 1;
  end;

var
  MBPIndex: Integer;
  ReleaseMBP: LongBool;
  dwGuardedAddr: Pointer;
begin
  ReleaseMBP := False;
  FRemoveCurrentBreakpoint := False;
  dwGuardedAddr := Pointer(DebugEvent^.Exception.ExceptionRecord.ExceptionInformation[1]);
  MBPIndex := GetMBPIndex(dwGuardedAddr);
  if MBPIndex >= 0 then
  begin
    CurrentMBPIndex := MBPIndex;
    while not CheckIsAddrInRealMemoryBPRegion(CurrentMBPIndex, dwGuardedAddr) do
    begin
      CurrentMBPIndex := GetMBPIndex(dwGuardedAddr, CurrentMBPIndex + 1);
      if CurrentMBPIndex < 0 then
        Break;
    end;

    if CurrentMBPIndex >= 0 then
    begin
      MBPIndex := CurrentMBPIndex;
      if Assigned(FBreakPoint) and CheckWriteMode then
        FBreakPoint(Self, DebugEvent^.dwThreadId, @DebugEvent^.Exception.ExceptionRecord, MBPIndex, ReleaseMBP)
      else
        CallUnhandledExceptionEvents(ecGuard, DebugEvent);
    end
    else
      CallUnhandledExceptionEvents(ecGuard, DebugEvent);

    FBreakpointList[MBPIndex].Active := False;
    SetSingleStepMode(DebugEvent^.dwThreadId, False);
    if ReleaseMBP or FRemoveCurrentBreakpoint then
      RemoveBreakpoint(MBPIndex)
    else
      FRestoreMBPIndex := MBPIndex;
  end
  else
    CallUnhandledExceptionEvents(ecGuard, DebugEvent);
end;

procedure TDebuger.ProcessExceptionSingleStep(DebugEvent: PDebugEvent);
//var
  //Handled: LongBool;
  //ThData: PThreadData;
begin
  //ThData := CurThreadData;

  //if Assigned(ThData) then
  //begin
    if Assigned(DbgCodeProfiler.DbgCurTrackAddress) then
    begin
      DoRestoreBreakpointF(DbgCodeProfiler.DbgCurTrackAddress);
      DbgCodeProfiler.DbgCurTrackAddress := nil;
    end;

    Exit;
  //end;



  (*
  // ������������ HWBP
  Handled := ProcessHardwareBreakpoint(DebugEvent);

  // ���� ������� �� ��-�� HWPB ��������������� ���������� HWBP
  if not Handled and (Cardinal(FRestoredThread) <> 0) and (FRestoredHWBPIndex >= 0) then
  begin
    ToggleHardwareBreakpoint(FRestoredThread, FRestoredHWBPIndex, True);
    FRestoredThread := 0;
    FRestoredHWBPIndex := -1;
  end;

  // ��������������� ��
  if FRestoreBPIndex >= 0 then
  begin
    CheckBreakpointIndex(FRestoreBPIndex);
    if FBreakpointList[FRestoreBPIndex].bpType = btUser then
      ToggleInt3Breakpoint(FRestoreBPIndex, True);
    FRestoreBPIndex := -1;
  end;

  // ��������������� M��
  if FRestoreMBPIndex >= 0 then
  begin
    CheckBreakpointIndex(FRestoreMBPIndex);
    if FBreakpointList[FRestoreMBPIndex].bpType = btMemory then
      ToggleMemoryBreakpoint(FRestoreMBPIndex, True);
    FRestoreMBPIndex := -1;
  end;

  // ���� �� ���������� �������� ��� ��������� ����� �����������
  // ���������� � ��� ������������
  if ResumeAction <> raRun then
  begin
    CallUnhandledExceptionEvents(ecSingleStep, DebugEvent);

    // ����� ���� ����������� �������� � ����������� �� ������� ������������
    DoResumeAction(DebugEvent^.dwThreadId);
  end;
  *)
end;

function TDebuger.ProcessHardwareBreakpoint(DebugEvent: PDebugEvent): LongBool;
var
  Index: Integer;
  ReleaseBP: LongBool;
  //ThData: PThreadData;
  Context: PContext;
begin
  FRemoveCurrentBreakpoint := False;

//  ThData := GetThreadData(DebugEvent^.dwThreadId);
//
//  ZeroMemory(@Context, SizeOf(TContext));
//  Context.ContextFlags := CONTEXT_DEBUG_REGISTERS;
//  Check(GetThreadContext(ThData^.ThreadHandle, Context));

  if CurThreadData = Nil then
    RaiseDebugCoreException();

  Context := CurThreadData^.Context;

  //UpdateThreadContext()

  Result := Context^.Dr6 and $F <> 0;
  if not Result then
    Exit;

  Index := -1;
  if Context^.Dr6 and 1 <> 0 then
    Index := 0;
  if Context^.Dr6 and 2 <> 0 then
    Index := 1;
  if Context^.Dr6 and 4 <> 0 then
    Index := 2;
  if Context^.Dr6 and 8 <> 0 then
    Index := 3;

  if Index < 0 then
    Exit;

  ReleaseBP := False;

  if Assigned(FHardwareBreakpoint) then
    FHardwareBreakpoint(Self, DebugEvent^.dwThreadId, @DebugEvent^.Exception.ExceptionRecord, Index, ReleaseBP);

  ToggleHardwareBreakpoint(DebugEvent^.dwThreadId, Index, False);
  SetSingleStepMode(DebugEvent^.dwThreadId, False);

  if ReleaseBP or FRemoveCurrentBreakpoint then
    DropHardwareBreakpoint(DebugEvent^.dwThreadId, Index)
  else
  begin
    // ���� ��� HWBP ���� ���� �� ������,
    // �� �.�. �������������� ���������� ����� �������
    // � ProcessExceptionSingleStep, ������ ����������� HWBP ����� �������
    // ������� ����� ������������ �������� ����� ������������ ���������� HWBP
    if (Cardinal(FRestoredThread) <> 0) and (FRestoredHWBPIndex >= 0) then
      ToggleHardwareBreakpoint(FRestoredThread, FRestoredHWBPIndex, True);

    FRestoredHWBPIndex := Index;
    FRestoredThread := DebugEvent^.dwThreadId;
  end;
end;

function TDebuger.ProcessTraceBreakPoint(DebugEvent: PDebugEvent): LongBool;
begin
  Result := False;

  if DbgTraceState in [dtsPause..dtsStepOut] then
  begin
    DbgState := dsPause;

    Inc(FTraceCounter);
    ProcessDbgTraceInfo(DebugEvent);

    // ���� ������� �� ����������� ������
    FTraceEvent.ResetEvent;
    FTraceEvent.WaitFor;

    ProcessDbgTraceInfo(DebugEvent);

    Result := True;
  end;
end;

function TDebuger.ProcessUserBreakPoint(DebugEvent: PDebugEvent): LongBool;
var
  Address: Pointer;
  ReleaseBP: LongBool;
  BreakPointIndex: Integer;
begin
  Result := False;

  ReleaseBP := False;
  FRemoveCurrentBreakpoint := False;

  Address := DebugEvent^.Exception.ExceptionRecord.ExceptionAddress;
  BreakPointIndex := GetBPIndex(Address, DebugEvent^.dwThreadId);
  if BreakPointIndex >= 0 then
  begin
    if Assigned(FBreakPoint) then
      FBreakPoint(Self, DebugEvent^.dwThreadId, @DebugEvent^.Exception.ExceptionRecord, BreakPointIndex, ReleaseBP)
    else
      CallUnhandledBreakPointEvents(ecBreakpoint, DebugEvent);

    ToggleInt3Breakpoint(BreakPointIndex, False);
    SetSingleStepMode(DebugEvent^.dwThreadId, True);
    if ReleaseBP or FRemoveCurrentBreakpoint then
      RemoveBreakpoint(BreakPointIndex)
    else
      FRestoreBPIndex := BreakPointIndex;

    Result := True;
  end;
end;

procedure TDebuger.ProcessDbgPerfomance(DebugEvent: PDebugEvent);
var
  ThData: PThreadData;
  I: Integer;
begin
  DbgState := dsPerfomance;

  // ��������� ���� ��� ��������� ��������
  if AddProcessPointInfo(ptPerfomance) then
  begin
    // ���� ������� �������, �� ��������� ���� ��� �������� ������
    for I := 0 to FThreadList.Count - 1 do
    begin
      ThData := FThreadList[I];
      if ThData^.State = tsActive then
        AddThreadPointInfo(ThData, ptPerfomance);
    end;
  end;
end;

procedure TDebuger.ProcessDbgSamplingInfo(DebugEvent: PDebugEvent);
begin
  DbgSamplingProfiler.ProcessDbgSamplingInfo;
end;

procedure TDebuger.ProcessDbgSyncObjsInfo(DebugEvent: PDebugEvent);
var
  ER: PExceptionRecord;
  DbgInfoType: TDbgInfoType;
  Ptr: Pointer;
  Size: Cardinal;
begin
  ER := @DebugEvent^.Exception.ExceptionRecord;
  DbgInfoType := TDbgInfoType(ER^.ExceptionInformation[0]);

  case DbgInfoType of
    dstSyncObjsInfo:
      begin
        Ptr := Pointer(ER^.ExceptionInformation[1]);
        Size := ER^.ExceptionInformation[2];

        DbgSysncObjsProfiler.LoadSyncObjsInfoPackEx(Ptr, Size);
      end;
    dstPerfomanceAndInfo:
      begin
        Ptr := Pointer(ER^.ExceptionInformation[3]);
        Size := ER^.ExceptionInformation[4];

        DbgSysncObjsProfiler.LoadSyncObjsInfoPackEx(Ptr, Size);
      end;
  end;
end;

function TDebuger.ReadData(const AddrPrt, ResultPtr: Pointer; const DataSize: Integer): LongBool;
var
  Dummy: TSysUInt;
begin
  Result := ReadProcessMemory(FProcessData.AttachedProcessHandle, AddrPrt, ResultPtr, DataSize, Dummy) and
    (Integer(Dummy) = DataSize);
end;

function TDebuger.ReadStringA(AddrPrt: Pointer; Len: Integer = 0): AnsiString;
var
  C: AnsiChar;
begin
  Result := '';

  if Len = -1 then
  begin
    // ������� ��������� �� PAnsiChar. ������ �� ������� #0
    // TODO: ���������� ��� �� ������ �������
    repeat
      C := #0;

      if not ReadData(AddrPrt, @C, SizeOf(AnsiChar)) then Exit;

      if C <> #0 then
      begin
        Result := Result + C;

        AddrPrt := IncPointer(AddrPrt, SizeOf(AnsiChar));
      end;

    until C = #0;
  end
  else
  begin
    if Len = 0 then
      ReadData(IncPointer(AddrPrt, -SizeOf(Pointer)), @Len, SizeOf(Pointer));

    if (Len > 0) then
    begin
      SetLength(Result, Len);
      if not ReadData(AddrPrt, @Result[1], Len) then
        Result := '';
    end;
  end;
end;

function TDebuger.ReadStringP(AddrPrt: Pointer; Len: Byte = 0): ShortString;
begin
  Result := '';

  if Len = 0 then
    ReadData(IncPointer(AddrPrt, -SizeOf(Byte)), @Len, SizeOf(Byte));

  if (Len > 0) then
  begin
    SetLength(Result, Len);
    if not ReadData(AddrPrt, @Result[1], Len) then
      Result := '';
  end;
end;

function TDebuger.ReadStringW(AddrPrt: Pointer; Len: Integer = 0): WideString;
begin
  Result := '';

  if Len = 0 then
    ReadData(IncPointer(AddrPrt, -SizeOf(Pointer)), @Len, SizeOf(Pointer));

  if (Len > 0) then
  begin
    SetLength(Result, Len);
    if not ReadData(AddrPrt, @Result[1], Len * SizeOf(WideChar)) then
      Result := '';
  end;
end;

procedure TDebuger.RemoveBreakpoint(Index: Integer);
var
  Len: Integer;
begin
  ToggleBreakpoint(Index, False);
  Len := BreakpointCount;
  if Len = 1 then
    SetLength(FBreakpointList, 0)
  else
  begin
    FBreakpointList[Index] := FBreakpointList[Len - 1];
    SetLength(FBreakpointList, Len - 1);
  end;
end;

procedure TDebuger.RemoveBreakpoint(const Address: Pointer; const SaveByte: Byte);
begin
  DoRemoveBreakpointF(Address, SaveByte);
end;

procedure TDebuger.RemoveCurrentBreakpoint;
begin
  FRemoveCurrentBreakpoint := True;
end;

procedure TDebuger.RemoveThread(const ThreadID: TThreadId);
var
  ThData: PThreadData;
begin
  ThData := GetThreadData(ThreadID);
  if ThData <> nil then
  begin
    FActiveThreadList.Remove(ThreadId);

    if ThData^.Breakpoint <> nil then
    begin
      FreeMemory(ThData^.Breakpoint);
      ThData^.Breakpoint := nil;
    end;

    ThData^.State := tsFinished;

    if AddProcessPointInfo(ptThreadInfo) then
      AddThreadPointInfo(ThData, ptStop);

    DbgMemoryProfiler.UpdateMemoryInfoObjectTypesOfThread(ThData);
  end;
end;

procedure TDebuger.RestoreBreakpoint(const Address: Pointer);
begin
  DoRestoreBreakpointF(Address);
end;

procedure TDebuger.ProcessDbgException(DebugEvent: PDebugEvent);
var
  ER: PExceptionRecord;
begin
  ER := @DebugEvent^.Exception.ExceptionRecord;
  case TDbgInfoType(ER^.ExceptionInformation[0]) of
    dstThreadInfo:
      ProcessDbgThreadInfo(DebugEvent);
    dstMemInfo:
      ProcessDbgMemoryInfo(DebugEvent);
    dstPerfomance:
      ProcessDbgPerfomance(DebugEvent);
    dstPerfomanceAndInfo:
      begin
        ProcessDbgPerfomance(DebugEvent);
        ProcessDbgMemoryInfo(DebugEvent);
        ProcessDbgSyncObjsInfo(DebugEvent);
      end;
    dstMemHookStatus:
      ProcessDbgMemoryInfo(DebugEvent);
    dstSyncObjsInfo:
      ProcessDbgSyncObjsInfo(DebugEvent);
    dstSampling:
      ProcessDbgSamplingInfo(DebugEvent);
  end;
end;

procedure TDebuger.Log(const Msg: String);
begin
  DoDbgLog(CurThreadId, Msg);
end;

procedure TDebuger.ProcessDbgMemoryInfo(DebugEvent: PDebugEvent);
var
  ER: PExceptionRecord;
  DbgInfoType: TDbgInfoType;
  Ptr: Pointer;
  Size: Cardinal;
begin
  ER := @DebugEvent^.Exception.ExceptionRecord;
  DbgInfoType := TDbgInfoType(ER^.ExceptionInformation[0]);

  case DbgInfoType of
    dstMemInfo, dstPerfomanceAndInfo:
      begin
        Ptr := Pointer(ER^.ExceptionInformation[1]);
        Size := ER^.ExceptionInformation[2];

        //LoadMemoryInfoPack(Ptr, Size);
        DbgMemoryProfiler.LoadMemoryInfoPackEx(Ptr, Size);
      end;
    dstMemHookStatus:
      begin
        case ER^.ExceptionInformation[1] of
          0: gvDebugInfo.ResetMemoryManagerBreakpoints;
          1: gvDebugInfo.SetMemoryManagerBreakpoints;
        end;
      end;
  end;
end;

procedure TDebuger.ProcessDbgThreadInfo(DebugEvent: PDebugEvent);
var
  ER: PExceptionRecord;
  ThreadID: Cardinal;
  StrAddr: Pointer;
  Str: ShortString;
  ThInfo: PThreadAdvInfo;
begin
  ER := @DebugEvent^.Exception.ExceptionRecord;
  ThreadID := ER^.ExceptionInformation[1];
  if ThreadID <> 0 then
  begin
    ThInfo := SetThreadInfo(ThreadID);

    //ThInfo^.ThreadParentId := DebugEvent^.dwThreadId;
    ThInfo^.ThreadParentId := ER^.ExceptionInformation[3];

    StrAddr := Pointer(ER^.ExceptionInformation[2]);
    if StrAddr <> Nil then
    begin
      Str := ReadStringP(StrAddr, 0);
      if Str <> '' then
      begin
        ThInfo^.ThreadClassName := String(Str);
        ThInfo^.ThreadAdvType := tatNormal;
      end;
    end;
  end;
end;

procedure TDebuger.ProcessDbgTraceInfo(DebugEvent: PDebugEvent);
var
  ThData: PThreadData;
  I: Integer;
begin
  if AddProcessPointInfo(ptTraceInfo) then
  begin
    for I := 0 to FThreadList.Count - 1 do
    begin
      ThData := FThreadList[I];
      if ThData^.State = tsActive then
        AddThreadPointInfo(ThData, ptTraceInfo);
    end;
  end;
end;

procedure TDebuger.ProcessDebugEvents;
var
  DebugEvent: PDebugEvent;
  ExceptionCode: DWORD;
  CallNextLoopIteration: LongBool;
begin
  DebugEvent := AllocMem(SizeOf(TDebugEvent));
  try
    repeat
      ContinueStatus := DBG_CONTINUE;

      DbgState := dsWait;
      FCurThreadData := nil;
      FCurThreadId := 0;

      if not WaitForDebugEvent(DebugEvent^, INFINITE) then
      begin
        DoDebugerFailed;
        Exit;
      end;

      DbgState := dsEvent;

      FCurThreadData := nil;
      FCurThreadId := TThreadId(DebugEvent^.dwThreadId);

      case DebugEvent^.dwDebugEventCode of
        EXCEPTION_DEBUG_EVENT:
          begin
            ExceptionCode := DebugEvent^.Exception.ExceptionRecord.ExceptionCode;

            case ExceptionCode of
              EXCEPTION_BREAKPOINT:
                ProcessExceptionBreakPoint(DebugEvent);

              EXCEPTION_SINGLE_STEP:
                ProcessExceptionSingleStep(DebugEvent);

              DBG_EXCEPTION:
                ProcessDbgException(DebugEvent);

              EXCEPTION_SET_THREAD_NAME:
                SetThreadName(DebugEvent);

              EXCEPTION_GUARD_PAGE:
                ProcessExceptionGuardPage(DebugEvent);
            else
              CallUnhandledExceptionEvents(CodeDataToExceptionCode(ExceptionCode), DebugEvent);
            end;
          end;

        CREATE_THREAD_DEBUG_EVENT:
          DoCreateThread(DebugEvent);

        CREATE_PROCESS_DEBUG_EVENT:
          DoCreateProcess(DebugEvent);

        EXIT_THREAD_DEBUG_EVENT:
          DoExitThread(DebugEvent);

        EXIT_PROCESS_DEBUG_EVENT:
          DoExitProcess(DebugEvent);

        LOAD_DLL_DEBUG_EVENT:
          DoLoadDll(DebugEvent);

        UNLOAD_DLL_DEBUG_EVENT:
          DoUnLoadDll(DebugEvent);

        OUTPUT_DEBUG_STRING_EVENT:
          DoDebugString(DebugEvent);

        RIP_EVENT:
          DoRip(DebugEvent);
      end;

      CallNextLoopIteration := ContinueDebugEvent(DebugEvent^.dwProcessId, DebugEvent^.dwThreadId, ContinueStatus);

    until not(CallNextLoopIteration) or (DebugEvent^.dwDebugEventCode = EXIT_PROCESS_DEBUG_EVENT);
  finally
    FreeMemory(DebugEvent);
  end;

  DoEndDebug;
end;

procedure TDebuger.DoSetBreakpoint(const Address: Pointer; var SaveByte: Byte);
var
  Dummy: TSysUInt;
  OldProtect: DWORD;
begin
  Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, Address, 1, PAGE_EXECUTE_READWRITE, OldProtect));

  Check(ReadProcessMemory(FProcessData.AttachedProcessHandle, Address, @SaveByte, 1, Dummy));
  Check(WriteProcessMemory(FProcessData.AttachedProcessHandle, Address, @BPOpcode, 1, Dummy));

  Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, Address, 1, OldProtect, OldProtect));
end;

procedure TDebuger.DoSetBreakpointF(const Address: Pointer; var SaveByte: Byte);
var
  Dummy: TSysUInt;
begin
  Check(ReadProcessMemory(FProcessData.AttachedProcessHandle, Address, @SaveByte, 1, Dummy));
  Check(WriteProcessMemory(FProcessData.AttachedProcessHandle, Address, @BPOpcode, 1, Dummy));
end;

function TDebuger.SetUserBreakpoint(Address: Pointer; const ThreadId: TThreadId = 0; const Description: string = ''): LongBool;
var
  Breakpoint: TBreakpoint;
  //OldProtect: DWORD;
  //Dummy: TSysUInt;
begin
  ZeroMemory(@Breakpoint, SizeOf(TBreakpoint));

  Breakpoint.bpType := btUser;
  Breakpoint.ThreadId := ThreadId;
  Breakpoint.Active := True;
  Breakpoint.Int3.Address := Address;
  Breakpoint.Description := ShortString(Description);

  DoSetBreakpoint(Address, Breakpoint.Int3.SaveByte);

  (*
  Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, Address, 1, PAGE_READWRITE, OldProtect));
  try
    Check(ReadProcessMemory(FProcessData.AttachedProcessHandle, Address, @Breakpoint.Int3.ByteCode, 1, Dummy));
    Check(WriteProcessMemory(FProcessData.AttachedProcessHandle, Address, @BPOpcode, 1, Dummy));
  finally
    Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, Address, 1, OldProtect, OldProtect));
  end;
  *)

  Result := AddNewBreakPoint(Breakpoint);
end;

procedure TDebuger.SetBreakpoint(const Address: Pointer; var SaveByte: Byte);
begin
  DoSetBreakpointF(Address, SaveByte);
end;

procedure TDebuger.SetCloseDebugProcess(const Value: LongBool);
begin
  FCloseDebugProcess := Value;
  DebugSetProcessKillOnExit(CloseDebugProcessOnFree);
end;

procedure TDebuger.SetCodeTracking(const Value: LongBool);
begin
  FCodeTracking := Value;
end;

procedure TDebuger.SetDbgState(const Value: TDbgState);
const
  _UpdateState: set of TDbgState = [dsNone, dsStarted, dsTrace, dsPause, dsStoping, dsStoped, dsDbgFail];
var
  Update: LongBool;
begin
  if Value <> FDbgState then
  begin
    Update := (FDbgState in _UpdateState) or (Value in _UpdateState);

    FDbgState := Value;

    if Assigned(FChangeDebugState) and Update then
      FChangeDebugState(Self);
  end;
end;

procedure TDebuger.SetDbgTraceState(const Value: TDbgTraceState);
begin
  if Value <> FDbgTraceState then
  begin
    FDbgTraceState := Value;

    if Assigned(FChangeDebugState) then
      FChangeDebugState(Self);
  end;
end;

procedure TDebuger.SetExceptionCallStack(const Value: LongBool);
begin
  FExceptionCallStack := Value;
end;

procedure TDebuger.SetExceptionCheckMode(const Value: LongBool);
begin
  FExceptionCheckMode := Value;
end;

procedure TDebuger.SetExceptionEvent(const Index: TExceptionCode; const Value: TDefaultExceptionEvent);
begin
  FExceptionEvents[Index] := Value;
end;

procedure TDebuger.SetFlag(const ThreadID: TThreadId; Flag: DWORD; Value: LongBool);
var
  ThData: PThreadData;
  Context: TContext;
begin
  ThData := GetThreadData(ThreadId);
  if ThData <> nil then
  begin
    ZeroMemory(@Context, SizeOf(Context));
    Context.ContextFlags := CONTEXT_FULL;
    Check(GetThreadContext(ThData^.ThreadHandle, Context));
    if Value then
      Context.EFlags := Context.EFlags or Flag
    else
      Context.EFlags := Context.EFlags and not Flag;
    Check(SetThreadContext(ThData^.ThreadHandle, Context));
  end;
end;

procedure TDebuger.SetHardwareBreakpoint(const ThreadId: TThreadID; Address: Pointer; Size: THWBPSize; Mode: THWBPMode; HWIndex: THWBPIndex;
  const Description: string);
var
  ThData: PThreadData;
begin
  ThData := GetThreadData(ThreadID);
  if ThData <> nil then
  begin
    ThData^.Breakpoint.Address[HWIndex] := Address;
    ThData^.Breakpoint.Size[HWIndex] := Size;
    ThData^.Breakpoint.Mode[HWIndex] := Mode;
    ThData^.Breakpoint.Description[HWIndex] := ShortString(Description);
    ThData^.Breakpoint.Active[HWIndex] := True;

    UpdateHardwareBreakpoints(ThreadId);
  end;
end;

function TDebuger.SetMemoryBreakpoint(Address: Pointer; Size: Cardinal; BreakOnWrite: LongBool; const Description: string): LongBool;
var
  Breakpoint: TBreakpoint;
  MBI: TMemoryBasicInformation; // TODO: GetMemory
  Index: Integer;
begin
  Index := GetMBPIndex(Address);
  if (Index >= 0) and (FBreakpointList[Index].bpType = btMemory) then
  begin
    MBI.BaseAddress := FBreakpointList[Index].Memory.RegionStart;
    MBI.RegionSize := FBreakpointList[Index].Memory.RegionSize;
    MBI.Protect := FBreakpointList[Index].Memory.PreviosRegionProtect;
  end
  else
    Check(VirtualQueryEx(ProcessData.AttachedProcessHandle, Address, MBI, SizeOf(TMemoryBasicInformation)) > 0);
  ZeroMemory(@Breakpoint, SizeOf(TBreakpoint));
  Breakpoint.bpType := btMemory;
  Breakpoint.Description := ShortString(Description);
  Breakpoint.Memory.Address := Address;
  Breakpoint.Memory.Size := Size;
  Breakpoint.Memory.BreakOnWrite := BreakOnWrite;
  Breakpoint.Memory.RegionStart := MBI.BaseAddress;
  Breakpoint.Memory.RegionSize := MBI.RegionSize;
  if Size = 0 then
    Inc(Size);
  Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, Address, Size, MBI.Protect or PAGE_GUARD, Breakpoint.Memory.PreviosRegionProtect));
  if Index >= 0 then
    Breakpoint.Memory.PreviosRegionProtect := MBI.Protect;
  Result := AddNewBreakPoint(Breakpoint);
end;

procedure TDebuger.SetPerfomanceMode(const Value: LongBool);
begin
  if FPerfomanceMode <> Value then
  begin
    FPerfomanceMode := Value;
    (*
    if FPerfomanceMode then
      FDbgPerfomanceThread.Suspended := False
    else
      FDbgPerfomanceThread.Suspended := True;
    *)
  end;
end;

procedure TDebuger.SetRegisters(const ThreadID: TThreadId; var Context: TContext);
var
  ThData: PThreadData;
begin
  ThData := GetThreadData(ThreadId);
  if ThData <> nil then
  begin
    Context.ContextFlags := CONTEXT_FULL;
    Check(SetThreadContext(ThData^.ThreadHandle, Context));
  end;
end;

procedure TDebuger.SetSamplingMethod(const Value: LongBool);
begin
  FSamplingMethod := Value;
end;

procedure TDebuger.SetSingleStepMode(ThData: PThreadData; const RestoreEIPAfterBP: LongBool);
begin
  // !!! ThData^.Context ��� ������ ���� ����������
  if RestoreEIPAfterBP then
    Dec(ThData^.Context^.Eip);

  ThData^.Context^.EFlags := ThData^.Context^.EFlags or EFLAGS_TF;

  Check(SetThreadContext(ThData^.ThreadHandle, ThData^.Context^));
end;

procedure TDebuger.SetSingleStepMode(const ThreadID: TThreadId; const RestoreEIPAfterBP: LongBool);
var
  ThData: PThreadData;
begin
  ThData := GetThreadData(ThreadID);

  ZeroMemory(ThData^.Context, SizeOf(TContext));

  ThData^.Context^.ContextFlags := CONTEXT_FULL;
  Check(GetThreadContext(ThData^.ThreadHandle, ThData^.Context^));

  if RestoreEIPAfterBP then
    Dec(ThData^.Context^.Eip);

  ThData^.Context^.EFlags := ThData^.Context^.EFlags or EFLAGS_TF;

  Check(SetThreadContext(ThData^.ThreadHandle, ThData^.Context^));
end;

function TDebuger.SetThreadInfo(const ThreadId: TThreadId): PThreadAdvInfo;
begin
  Result := GetThreadInfo(ThreadId);
  if Result = Nil then
    Result := AddThreadInfo(ThreadId);
end;

procedure TDebuger.SetThreadName(DebugEvent: PDebugEvent);
var
  StrAddr: Pointer;
  Str: AnsiString;
  ThreadAdvInfo: PThreadAdvInfo;
begin
  StrAddr := Pointer(DebugEvent^.Exception.ExceptionRecord.ExceptionInformation[1]);
  Str := ReadStringA(StrAddr, -1);
  if Str <> '' then
  begin
    ThreadAdvInfo := SetThreadInfo(DebugEvent^.dwThreadId);

    ThreadAdvInfo^.ThreadName := String(Str);

    if ThreadAdvInfo^.ThreadName <> '' then
    begin
      if Copy(ThreadAdvInfo^.ThreadName, 1, 3) = _SERVICE_THREAD_PREFIX then
        ThreadAdvInfo^.ThreadAdvType := tatService
      else
        ThreadAdvInfo^.ThreadAdvType := tatNormal;
    end;
  end;
end;

procedure TDebuger.SetTrackSystemUnits(const Value: LongBool);
begin
  FTrackSystemUnits := Value;
end;

function TDebuger.StopDebug: LongBool;
begin
  Result := False;

  if FDbgState = dsPause then
    ContinueDebug;

  if Active then
  begin
    UnLoadDbgHookDll(ProcessData.AttachedProcessHandle, 'DbgHook32.dll');

    if CloseDebugProcessOnFree then
    begin
      if FProcessData.CreatedProcessHandle <> 0 then
        Result := TerminateProcess(FProcessData.CreatedProcessHandle, 0);
    end
    else
      Result := DebugActiveProcessStop(Cardinal(FProcessData.ProcessID));
  end;

  if FProcessData.CreatedProcessHandle <> 0 then
  begin
    CloseHandle(FProcessData.CreatedProcessHandle);
    FProcessData.CreatedProcessHandle := 0;
  end;

  if FProcessData.CreatedThreadHandle <> 0 then
  begin
    CloseHandle(FProcessData.CreatedThreadHandle);
    FProcessData.CreatedThreadHandle := 0;
  end;

  if FProcessData.AttachedFileHandle <> 0 then
  begin
    CloseHandle(FProcessData.AttachedFileHandle);
    FProcessData.AttachedFileHandle := 0;
  end;
end;

procedure TDebuger.ToggleBreakpoint(Index: Integer; Active: LongBool);
begin
  CheckBreakpointIndex(Index);
  case FBreakpointList[Index].bpType of
    btUser:
      ToggleInt3Breakpoint(Index, Active);
    btMemory:
      ToggleMemoryBreakpoint(Index, Active);
  end;
end;

procedure TDebuger.ToggleHardwareBreakpoint(const ThreadId: TThreadID; Index: THWBPIndex; Active: LongBool);
var
  ThData: PThreadData;
begin
  ThData := GetThreadData(ThreadID);
  if ThData <> nil then
  begin
    if ThData^.Breakpoint.Active[Index] = Active then
      Exit;
    ThData^.Breakpoint.Active[Index] := Active;
    UpdateHardwareBreakpoints(ThreadId);
  end;
end;

(*
procedure TDebuger.DoRemoveBreakpoint(const Address: Pointer; const SaveByte: Byte);
var
  Dummy: TSysUInt;
  OldProtect: DWORD;
begin
  Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, Address, 1, PAGE_READWRITE, OldProtect));
  try
    Check(WriteProcessMemory(FProcessData.AttachedProcessHandle, Address, @SaveByte, 1, Dummy));
  finally
    Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, Address, 1, OldProtect, OldProtect));
  end;
end;
*)

procedure TDebuger.DoRemoveBreakpointF(const Address: Pointer; const SaveByte: Byte);
var
  Dummy: TSysUInt;
begin
  Check(WriteProcessMemory(FProcessData.AttachedProcessHandle, Address, @SaveByte, 1, Dummy));
end;

procedure TDebuger.ToggleInt3Breakpoint(Index: Integer; Active: LongBool);
var
  OldProtect: DWORD;
  Dummy: TSysUInt;
begin
  CheckBreakpointIndex(Index);

  if FBreakpointList[Index].bpType <> btUser then
    Exit;

  if FBreakpointList[Index].Active = Active then
    Exit;

  Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, FBreakpointList[Index].Int3.Address, 1, PAGE_READWRITE, OldProtect));
  try
    if Active then
      Check(WriteProcessMemory(FProcessData.AttachedProcessHandle, FBreakpointList[Index].Int3.Address, @BPOpcode, 1, Dummy))
    else
      Check(WriteProcessMemory(FProcessData.AttachedProcessHandle, FBreakpointList[Index].Int3.Address, @FBreakpointList[Index].Int3.SaveByte, 1, Dummy));
  finally
    Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, FBreakpointList[Index].Int3.Address, 1, OldProtect, OldProtect));
  end;

  FBreakpointList[Index].Active := Active;
end;

procedure TDebuger.ToggleMemoryBreakpoint(Index: Integer; Active: LongBool);
var
  Dummy, TmpSize: DWORD;
begin
  CheckBreakpointIndex(Index);
  if FBreakpointList[Index].bpType <> btMemory then
    Exit;
  if FBreakpointList[Index].Active = Active then
    Exit;
  TmpSize := FBreakpointList[Index].Memory.Size;
  if TmpSize = 0 then
    Inc(TmpSize);
  if Active then
    Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, FBreakpointList[Index].Memory.Address, TmpSize,
        FBreakpointList[Index].Memory.PreviosRegionProtect or PAGE_GUARD, Dummy))
  else
    Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, FBreakpointList[Index].Memory.Address, TmpSize,
        FBreakpointList[Index].Memory.PreviosRegionProtect, Dummy));
  FBreakpointList[Index].Active := Active;
end;

function TDebuger.TraceDebug(const TraceType: TDbgTraceState): LongBool;
begin
  Result := False;

  case TraceType of
    dtsContinue:
      Result := ContinueDebug;
    dtsPause:
      Result := PauseDebug;
    dtsStepIn:;
    dtsStepOver:;
    dtsStepOut:;
  end;

  SwitchToThread;
end;

const
  DR7_SET_LOC_DR0 = $01;
  DR7_SET_GLB_DR0 = $02;

  DR7_SET_LOC_DR1 = $04;
  DR7_SET_GLB_DR1 = $08;

  DR7_SET_LOC_DR2 = $10;
  DR7_SET_GLB_DR2 = $20;

  DR7_SET_LOC_DR3 = $40;
  DR7_SET_GLB_DR3 = $80;

  DR7_SET_LOC_ON = $100;
  DR7_SET_GLB_ON = $200;

  DR_On: array [THWBPIndex] of DWORD =
   (DR7_SET_LOC_DR0, DR7_SET_LOC_DR1, DR7_SET_LOC_DR2, DR7_SET_LOC_DR3);

  DR7_PROTECT = $2000;

  DR_SIZE_BYTE = 0;
  DR_SIZE_WORD = 1;
  DR_SIZE_DWORD = 3;

  DR_MODE_E = 0;
  DR_MODE_W = 1;
  DR_MODE_I = 2;
  DR_MODE_R = 3;

  DR7_MODE_DR0_E = DR_MODE_E shl 16;
  DR7_MODE_DR0_W = DR_MODE_W shl 16;
  DR7_MODE_DR0_I = DR_MODE_I shl 16;
  DR7_MODE_DR0_R = DR_MODE_R shl 16;

  DR7_SIZE_DR0_B = DR_SIZE_BYTE shl 18;
  DR7_SIZE_DR0_W = DR_SIZE_WORD shl 18;
  DR7_SIZE_DR0_D = DR_SIZE_DWORD shl 18;

  DR7_MODE_DR1_E = DR_MODE_E shl 20;
  DR7_MODE_DR1_W = DR_MODE_W shl 20;
  DR7_MODE_DR1_I = DR_MODE_I shl 20;
  DR7_MODE_DR1_R = DR_MODE_R shl 20;

  DR7_SIZE_DR1_B = DR_SIZE_BYTE shl 22;
  DR7_SIZE_DR1_W = DR_SIZE_WORD shl 22;
  DR7_SIZE_DR1_D = DR_SIZE_DWORD shl 22;

  DR7_MODE_DR2_E = DR_MODE_E shl 24;
  DR7_MODE_DR2_W = DR_MODE_W shl 24;
  DR7_MODE_DR2_I = DR_MODE_I shl 24;
  DR7_MODE_DR2_R = DR_MODE_R shl 24;

  DR7_SIZE_DR2_B = DR_SIZE_BYTE shl 26;
  DR7_SIZE_DR2_W = DR_SIZE_WORD shl 26;
  DR7_SIZE_DR2_D = DR_SIZE_DWORD shl 26;

  DR7_MODE_DR3_E = DR_MODE_E shl 28;
  DR7_MODE_DR3_W = DR_MODE_W shl 28;
  DR7_MODE_DR3_I = DR_MODE_I shl 28;
  DR7_MODE_DR3_R = DR_MODE_R shl 28;

  DR7_SIZE_DR3_B = DR_SIZE_BYTE shl 30;
  DR7_SIZE_DR3_W = DR_SIZE_WORD shl 30;
  DR7_SIZE_DR3_D = $C0000000; // DR_SIZE_DWORD shl 30;

  DR_Mode: array [THWBPIndex] of array [THWBPMode] of DWORD = (
    (DR7_MODE_DR0_E, DR7_MODE_DR0_W, DR7_MODE_DR0_I, DR7_MODE_DR0_R),
    (DR7_MODE_DR1_E, DR7_MODE_DR1_W, DR7_MODE_DR1_I, DR7_MODE_DR1_R),
    (DR7_MODE_DR2_E, DR7_MODE_DR2_W, DR7_MODE_DR2_I, DR7_MODE_DR2_R),
    (DR7_MODE_DR3_E, DR7_MODE_DR3_W, DR7_MODE_DR3_I, DR7_MODE_DR3_R)
  );

  DR_Size: array [THWBPIndex] of array [THWBPSize] of DWORD = (
    (DR7_SIZE_DR0_B, DR7_SIZE_DR0_W, DR7_SIZE_DR0_D),
    (DR7_SIZE_DR1_B, DR7_SIZE_DR1_W, DR7_SIZE_DR1_D),
    (DR7_SIZE_DR2_B, DR7_SIZE_DR2_W, DR7_SIZE_DR2_D),
    (DR7_SIZE_DR3_B, DR7_SIZE_DR3_W, DR7_SIZE_DR3_D)
  );

function TDebuger.UpdateCurThreadContext(const ContextFlags: Cardinal = CONTEXT_FULL): LongBool;
begin
  Result := True;

  if (FCurThreadData = nil) or (FCurThreadData^.Context^.ContextFlags <> ContextFlags) then
  begin
    FCurThreadData := UpdateThreadContext(FCurThreadId, ContextFlags);
    Result := Assigned(FCurThreadData);
  end;
end;

procedure TDebuger.UpdateHardwareBreakpoints(const ThreadID: TThreadId);
var
  Context: PContext;
  I: THWBPIndex;
  ThData: PThreadData;
  Breakpoint: PHardwareBreakpoint;
begin
  ThData := GetThreadData(ThreadID);
  if ThData = nil then
    Exit;

  Context := AllocMem(SizeOf(TContext));
  try
    Context.ContextFlags := CONTEXT_DEBUG_REGISTERS;

    Breakpoint := ThData^.Breakpoint;

    for I := 0 to 3 do
    begin
      if not Breakpoint.Active[I] then
        Continue;

      if Breakpoint.Address[I] <> nil then
      begin
        Context.Dr7 := Context.Dr7 or DR7_SET_LOC_ON;

        case I of
          0: Context.Dr0 := DWORD(Breakpoint.Address[I]);
          1: Context.Dr1 := DWORD(Breakpoint.Address[I]);
          2: Context.Dr2 := DWORD(Breakpoint.Address[I]);
          3: Context.Dr3 := DWORD(Breakpoint.Address[I]);
        end;

        Context.Dr7 := Context.Dr7 or DR_On[I];
        Context.Dr7 := Context.Dr7 or DR_Mode[I, Breakpoint.Mode[I]];
        Context.Dr7 := Context.Dr7 or DR_Size[I, Breakpoint.Size[I]];
      end;
    end;

    Check(SetThreadContext(ThData^.ThreadHandle, Context^));
  finally
    FreeMem(Context);
  end;
end;

function TDebuger.UpdateThreadContext(ThreadData: PThreadData; const ContextFlags: Cardinal = CONTEXT_FULL): LongBool;
begin
  Result := False;

  if ThreadData <> Nil then
  begin
    //ZeroMemory(ThreadData^.Context, SizeOf(TContext));

    ThreadData^.Context^.ContextFlags := ContextFlags;
    Result := GetThreadContext(ThreadData^.ThreadHandle, ThreadData^.Context^);
  end;
end;

function TDebuger.UpdateThreadContext(const ThreadID: TThreadId; const ContextFlags: Cardinal = CONTEXT_FULL): PThreadData;
begin
  Result := GetThreadData(ThreadId);
  if Result <> nil then
    if not UpdateThreadContext(Result, ContextFlags) then
      RaiseDebugCoreException();
end;

function TDebuger.WriteData(AddrPrt, DataPtr: Pointer; const DataSize: Cardinal): LongBool;
var
  Dummy: TSysUInt;
begin
  Result :=
    WriteProcessMemory(FProcessData.AttachedProcessHandle, AddrPrt, DataPtr, DataSize, Dummy) and (Dummy = DataSize);

  if Result then
    Result := FlushInstructionCache(FProcessData.AttachedProcessHandle, AddrPrt, DataSize);
end;

initialization

finalization
  if gvDebuger <> Nil then
    FreeAndNil(gvDebuger);
end.
