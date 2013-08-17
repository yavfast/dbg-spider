unit Debuger;

interface

uses
  Windows, Classes, SysUtils, ClassUtils, SyncObjs, JclPeImage, JclDebug, DebugerTypes;

type
  TDebuger = class
  private
    FProcessData: TProcessData;          // Служебная информация об отлаживаемом процессе
    FThreadList: TDbgThreadList;            // Данные о потоках отлаживаемого процесса
    FThreadAdvInfoList: TThreadAdvInfoList; // Дополнительная информация о потоках

    FSetEntryPointBreakPoint: Boolean;   // Флаг указывающий отладчику, необходимо ли ставить ВР на ЕР
    //FMainLoopWaitPeriod: Cardinal;       // Время ожидания отладочного события
    FBreakpointList: TBreakpointList;    // Список ВР и МВР
    FRestoreBPIndex: Integer;            // Индекс для восстановления ВР
    FRestoreMBPIndex: Integer;           // Индекс для восстановления МВР
    FRestoredHWBPIndex: Integer;         // Индексы для восстановления НВР
    FRestoredThread: TThreadId;
    FCloseDebugProcess: Boolean;         // Флаг указывающий нужно ли закрывать отлаживаемый процесс при завершении отладки
    FContinueStatus: DWORD;              // Статус с которым вызывается ContinueDebugEvent
    FResumeAction: TResumeAction;        // Флаг поведения отладчика после обработки очередного события
    FRemoveCurrentBreakpoint: Boolean;   // Флаг удаления текущего ВР
    FCurThreadId: TThreadId;
    FCurThreadData: PThreadData;
    FDbgState: TDbgState;

    //FDbgPerfomanceThread: TDbgPerfomanceThread;
    FPerfomanceMode: Boolean;
    FPerfCallStacks: Boolean;

    FPerfomanceCheckPtr: Pointer;
    //FPerfomanceThreadId: TThreadId;

    FDbgShareMem: THandle;

    // внешние события
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

    FExceptioEvents: array [TExceptionCode] of TDefaultExceptionEvent;
    FBreakPoint: TBreakPointEvent;
    FHardwareBreakpoint: THardwareBreakpointEvent;

    function GetExceptionEvent(const Index: TExceptionCode): TDefaultExceptionEvent;
    procedure SetExceptionEvent(const Index: TExceptionCode; const Value: TDefaultExceptionEvent);
    procedure SetCloseDebugProcess(const Value: Boolean);

    procedure SetPerfomanceMode(const Value: Boolean);
    procedure SetPerfCallStacks(const Value: Boolean);

    function FindMemoryPointer(const Ptr: Pointer): PThreadData;
    procedure LoadMemoryInfoPack(MemInfoPack: Pointer; const Count: Cardinal);
    procedure UpdateMemoryInfoObjectTypes;
  protected
    // работа с данными о нитях отлаживаемого приложения
    function AddThread(const ThreadID: TThreadId; ThreadHandle: THandle): PThreadData;
    procedure RemoveThread(const ThreadID: TThreadId);

    function GetThreadIndex(const ThreadID: TThreadId; const UseFinished: Boolean = False): Integer;

    function GetThreadInfoIndex(const ThreadId: TThreadId): Integer;
    function AddThreadInfo(const ThreadId: TThreadId): PThreadAdvInfo;
    function GetThreadInfo(const ThreadId: TThreadId): PThreadAdvInfo;
    function SetThreadInfo(const ThreadId: TThreadId): PThreadAdvInfo;

    // обработчики отладочных событий первой очереди
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

    // обработчики отладочных событий второй очереди
    procedure CallUnhandledExceptionEvents(const Code: TExceptionCode; DebugEvent: PDebugEvent);
    procedure CallUnhandledBreakPointEvents(const Code: TExceptionCode; DebugEvent: PDebugEvent);

    procedure ProcessExceptionBreakPoint(DebugEvent: PDebugEvent);
    procedure ProcessExceptionSingleStep(DebugEvent: PDebugEvent);
    procedure ProcessExceptionGuardPage(DebugEvent: PDebugEvent);

    procedure SetThreadName(DebugEvent: PDebugEvent);

    procedure ProcessDbgException(DebugEvent: PDebugEvent);
    procedure ProcessDbgThreadInfo(DebugEvent: PDebugEvent);
    procedure ProcessDbgMemoryInfo(DebugEvent: PDebugEvent);
    procedure ProcessDbgPerfomance(DebugEvent: PDebugEvent);

    function ProcessHardwareBreakpoint(DebugEvent: PDebugEvent): Boolean;

    // работа с точками остановки:

    // регистрация нового обработчика в списке
    function AddNewBreakPoint(var Value: TBreakpoint): Boolean;
    // проверка допутимости индекса обработчика
    procedure CheckBreakpointIndex(Value: Integer);
    // проверка попадания адреса в область памяти контролируемую Memory BP
    function CheckIsAddrInRealMemoryBPRegion(BreakPointIndex: Integer; AAddr: Pointer): Boolean;
    // получение обработчика BP обрабатывающего указанный адрес
    function GetBPIndex(BreakPointAddr: Pointer; const ThreadID: TThreadId = 0): Integer;
    // получение обработчика MBP обрабатывающего указанный адрес
    function GetMBPIndex(BreakPointAddr: Pointer; FromIndex: Integer = 0): Integer;
    // проверка установлен ли уже аналогичный BP
    function IsBreakpointPresent(const Value: TBreakpoint): Boolean;
    // переключение активности Int3 брякпойнта
    procedure ToggleInt3Breakpoint(Index: Integer; Active: Boolean);
    // переключение активности брякпойна та область памяти
    procedure ToggleMemoryBreakpoint(Index: Integer; Active: Boolean);
    // обновление аппаратных точек останова нити
    procedure UpdateHardwareBreakpoints(const ThreadID: TThreadId);

    // перевод нити отлаживаемого приложения в режим трассировки
    procedure SetSingleStepMode(const ThreadID: TThreadId; RestoreEIPAfterBP: Boolean);

    function PerfomancePauseDebug: Boolean;

    function AddThreadPointInfo(ThreadData: PThreadData; const PointType: TPointType; DebugEvent: PDebugEvent = nil): Boolean;
    function AddProcessPointInfo(const PointType: TPointType): Boolean;

    procedure ClearDbgInfo;

    function GetProcessCPUTime: UInt64;
    function GetThreadCPUTime(ThreadData: PThreadData): UInt64;
  public
    constructor Create();
    destructor Destroy; override;

    // запуск/остановка отладки
    function AttachToProcess(const ProcessID: TProcessId; SentEntryPointBreakPoint: Boolean): Boolean;
    function DebugNewProcess(const FilePath: string; SentEntryPointBreakPoint: Boolean): Boolean;

    function StopDebug: Boolean;
    function PauseDebug: Boolean;

    procedure ProcessDebugEvents;

    // чтение запись данных
    Function AllocMem(const Size: Cardinal): Pointer;
    Procedure FreeMem(Data : Pointer; const Size: Cardinal = 0);

    procedure InjectThread(hProcess: THandle; Func: Pointer; FuncSize: Cardinal; aParams: Pointer;
      aParamsSize: Cardinal; WaitAndFree: Boolean = True);
    function InjectFunc(Func: Pointer; const CodeSize: Cardinal): Pointer;

    procedure InjectPerfThread;
    procedure InjectPerfFunc;

    function ReadData(AddrPrt, ResultPtr: Pointer; DataSize: Integer): Boolean;

    function ReadStringA(AddrPrt: Pointer; Len: Integer = 0): AnsiString;
    function ReadStringW(AddrPrt: Pointer; Len: Integer = 0): WideString;
    function ReadStringP(AddrPrt: Pointer; Len: Byte = 0): ShortString;

    function WriteData(AddrPrt, DataPtr: Pointer; const DataSize: Cardinal): Boolean;

    procedure SetFlag(const ThreadID: TThreadId; Flag: DWORD; Value: Boolean);
    function GetFlag(const ThreadID: TThreadId; Flag: DWORD): Boolean;

    function UpdateThreadContext(const ThreadID: TThreadId): PThreadData; overload;
    function UpdateThreadContext(ThreadData: PThreadData): Boolean; overload;

    function GetRegisters(const ThreadID: TThreadId): TContext;
    procedure SetRegisters(const ThreadID: TThreadId; var Context: TContext);

    Function IsValidAddr(Const Addr: Pointer): Boolean;
    Function IsValidCodeAddr(Const Addr: Pointer): Boolean;
    Function IsValidProcessCodeAddr(Const Addr: Pointer): Boolean;

    procedure GetCallStack(ThData: PThreadData; var Stack: TStackPointList);

    function GetThreadData(const ThreadID: TThreadId; const UseFinished: Boolean = False): PThreadData;
    function CurThreadId: TThreadId;
    function CurThreadData: PThreadData;
    function GetThreadCount: Integer;
    function GetThreadDataByIdx(const Idx: Cardinal): PThreadData;

    // выполнение кода
    Procedure ExecuteCode(AddrPtr: Pointer; const TimeOut: Cardinal);

    function GetDllName(lpImageName, lpBaseOfDll: Pointer; var Unicode: Boolean): AnsiString;

    // работа с точками остановки
    function SetBreakpoint(Address: Pointer; const ThreadId: TThreadId = 0; const Description: string = ''): Boolean;
    function SetMemoryBreakpoint(Address: Pointer; Size: Cardinal; BreakOnWrite: Boolean; const Description: string): Boolean;

    procedure RemoveBreakpoint(Index: Integer);

    procedure ToggleBreakpoint(Index: Integer; Active: Boolean);

    function BreakpointCount: Integer;
    function BreakpointItem(Index: Integer): TBreakpoint;

    procedure RemoveCurrentBreakpoint;

    // работа с аппаратными точками остановки
    procedure SetHardwareBreakpoint(const ThreadId: TThreadID; Address: Pointer; Size: THWBPSize; Mode: THWBPMode; HWIndex: THWBPIndex; const Description: string);
    procedure ToggleHardwareBreakpoint(const ThreadId: TThreadID; Index: THWBPIndex; Active: Boolean);
    procedure DropHardwareBreakpoint(const ThreadId: TThreadID; Index: THWBPIndex);
    procedure DropAllHardwareBreakpoint(const ThreadId: TThreadID);

    // внутренние события отладчика
    property OnMainLoopFailed: TNotifyEvent read FMainLoopFailed write FMainLoopFailed;
    property OnEndDebug: TNotifyEvent read FEndDebug write FEndDebug;

    // обработчики отладочных событий
    property OnCreateThread: TCreateThreadEvent read FCreateThread write FCreateThread;
    property OnCreateProcess: TCreateProcessEvent read FCreateProcess write FCreateProcess;
    property OnExitThread: TExitThreadEvent read FExitThread write FExitThread;
    property OnExitProcess: TExitProcessEvent read FExitProcess write FExitProcess;
    property OnLoadDll: TLoadDllEvent read FLoadDll write FLoadDll;
    property OnUnloadDll: TUnLoadDllEvent read FUnLoadDll write FUnLoadDll;
    property OnDebugString: TDebugStringEvent read FDebugString write FDebugString;
    property OnRip: TRipEvent read FRip write FRip;

    // обработчики исключений
    property OnBreakPoint: TBreakPointEvent read FBreakPoint write FBreakPoint;
    property OnHardwareBreakpoint: THardwareBreakpointEvent read FHardwareBreakpoint write FHardwareBreakpoint;
    property OnUnknownException: TDefaultExceptionEvent index ecUnknown read GetExceptionEvent write SetExceptionEvent;
    property OnUnknownBreakPoint: TDefaultExceptionEvent index ecBreakpoint read GetExceptionEvent write SetExceptionEvent;
    property OnSingleStep: TDefaultExceptionEvent index ecSingleStep read GetExceptionEvent write SetExceptionEvent;
    property OnCtrlC: TDefaultExceptionEvent index ecCtrlC read GetExceptionEvent write SetExceptionEvent;
    property OnNonContinuable: TDefaultExceptionEvent index ecNonContinuable read GetExceptionEvent write SetExceptionEvent;
    property OnPageGuard: TDefaultExceptionEvent index ecGuard read GetExceptionEvent write SetExceptionEvent;

    // расширенные свойства отладчика
    property ContinueStatus: DWORD read FContinueStatus write FContinueStatus;
    property CloseDebugProcessOnFree: Boolean read FCloseDebugProcess write SetCloseDebugProcess;
    property ProcessData: TProcessData read FProcessData;
    //property MainLoopWaitPeriod: DWORD read FMainLoopWaitPeriod write FMainLoopWaitPeriod;
    property ResumeAction: TResumeAction read FResumeAction write FResumeAction;
    property DbgState: TDbgState read FDbgState;

    property PerfomanceMode: Boolean read FPerfomanceMode write SetPerfomanceMode;
    property PerfCallStacks: Boolean read FPerfCallStacks write SetPerfCallStacks;

    property DbgShareMem: THandle read FDbgShareMem;
  end;

var
  gvDebuger: TDebuger = nil;

implementation

uses
  RTLConsts, Math, DebugHook, DebugInfo, DbgHookTypes;

type
  TCollectList<T> = class(TBaseCollectList)
  private
    FSegSize: Cardinal;
    FSegList: TSegList<T>;
  protected
    function GetItem(const Index: Cardinal): PData; override;
    procedure CheckSeg(const Seg: Integer); override;

    function IndexToSegment(const Index: Cardinal; var Seg, Offset: Integer): Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;

    function Add: PData; override;
    procedure Clear; override;
  end;

{ TCollectList<T> }

function TCollectList<T>.Add: PData;
var
  Idx: Cardinal;
  Seg, Offset: Integer;
begin
  Idx := Count;
  IndexToSegment(Idx, Seg, Offset);
  CheckSeg(Seg);
  inherited Add;

  Result := @FSegList[Seg][Offset];

  FillChar(Result^, SizeOf(T), 0);
end;

procedure TCollectList<T>.CheckSeg(const Seg: Integer);
begin
  if Length(FSegList) <= Seg then
  begin
    SetLength(FSegList, Seg + 1);
    SetLength(FSegList[Seg], FSegSize);
  end;
end;

procedure TCollectList<T>.Clear;
begin
  SetLength(FSegList, 0);
  inherited Clear;
end;

constructor TCollectList<T>.Create;
begin
  inherited Create;

  FSegSize := _SEGMENT_SIZE div SizeOf(T);
  SetLength(FSegList, 0);
end;

destructor TCollectList<T>.Destroy;
begin
  Clear;

  inherited;
end;

function TCollectList<T>.GetItem(const Index: Cardinal): PData;
var
  Seg, Offset: Integer;
begin
  if IndexToSegment(Index, Seg, Offset) then
    Result := @FSegList[Seg][Offset]
  else
    RaiseError(@EIndexError, [Index]);
end;

function TCollectList<T>.IndexToSegment(const Index: Cardinal; var Seg, Offset: Integer): Boolean;
begin
  Result := Index < Count;

  Seg := Index div FSegSize;
  Offset := Index mod FSegSize;
end;


function QueryThreadCycleTime(ThreadHandle: THandle; CycleTime: PUInt64): BOOL; stdcall; external kernel32 name 'QueryThreadCycleTime';
function QueryProcessCycleTime(ProcessHandle: THandle; CycleTime: PUInt64): BOOL; stdcall; external kernel32 name 'QueryProcessCycleTime';
function DebugBreakProcess(Process: THandle): BOOL; stdcall; external kernel32 name 'DebugBreakProcess';

function _DbgPerfomanceHook(pvParam: Pointer): DWORD; stdcall;
begin
  Result := DWORD(@_DbgPerfomanceHook);
end;

const
  BPOpcode: Byte = $CC;

var
  _FreqPerSec: Int64 = 0;
  _FreqPerMSec: Int64 = 0;


function Check(const Value: Boolean): Boolean; //inline;
begin
  if not Value then
    RaiseLastOSError;
  Result := Value;
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

function TDebuger.AddNewBreakPoint(var Value: TBreakpoint): Boolean;
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

function TDebuger.AddThreadPointInfo(ThreadData: PThreadData; const PointType: TPointType; DebugEvent: PDebugEvent = nil): Boolean;
var
  Cur: Int64;
  Prev: UInt64;
  PrevTime: UInt64;
  Delta: UInt64;
  ThPoint: PThreadPoint;
begin
  Result := False;

  if ThreadData = Nil then Exit;

  Delta := 0;
  Prev := 0;

  case PointType of
    ptStart:
      Result := True;
    ptStop:
      Result := True;
    ptException:
      Result := True;
    ptPerfomance:
      begin
        // Относительное время выполнения
        Cur := FProcessData.Ellapsed;
        ThreadData^.Ellapsed := Cur - ThreadData^.Started;

        // Сохраняем время CPU
        PrevTime := ThreadData^.CPUTime;
        ThreadData^.CPUTime := GetThreadCPUTime(ThreadData);
        Delta := ThreadData^.CPUTime - PrevTime;

        // Счетчик таймера CPU
        if QueryThreadCycleTime(ThreadData^.ThreadHandle, @Cur) then
        begin
          Prev := ThreadData^.ThreadEllapsed;
          ThreadData^.ThreadEllapsed := Cur;
        end;

        // Добавляем инфу, когда поток активен
        Result := (Delta > 0);
      end;
  end;

  if Result then
  begin
    ThreadData^.DbgPoints.Lock;
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
            ThreadData^.Ellapsed :=
              (FProcessData.Started + FProcessData.DbgPointByIdx(ThPoint^.PerfIdx)^.FromStart) - ThreadData^.Started;

            ThreadData^.CPUTime := GetThreadCPUTime(ThreadData);

            if QueryThreadCycleTime(ThreadData^.ThreadHandle, @Cur) then
              ThreadData^.ThreadEllapsed := Cur;
          end;
        ptException:
          begin
            ThPoint^.ExceptInfo := TExceptInfo.Create(DebugEvent);
            ThreadData^.DbgExceptions.Add(ThPoint^.ExceptInfo);

            FProcessData.DbgExceptions.Add(ThPoint^.ExceptInfo);
          end;
        ptPerfomance:
          begin
            ThPoint^.DeltaTickCPU := Cur - Prev;
            ThPoint^.DeltaTime := Delta;
          end;
      end;
    finally
      ThreadData^.DbgPoints.UnLock;
    end;
  end;
end;

function TDebuger.AddProcessPointInfo(const PointType: TPointType): Boolean;
var
  ProcPoint: PProcessPoint;
  Prev: UInt64;
  Cur: UInt64;
  PPrev: Int64;
  PCur: Int64;
  PrevTime: UInt64;
  CurTime: UInt64;
  Delta: UInt64;
begin
  Result := False;

  if not QueryPerformanceCounter(PCur) then
    RaiseDebugCoreException();

  CurTime := GetProcessCPUTime;

  Delta := 0;
  PPrev := 0;
  Prev := 0;

  case PointType of
    ptStart, ptException, ptThreadInfo{, ptMemoryInfo}:
      begin
        Result := True;
      end;
    ptStop:
      begin
        FProcessData.Ellapsed := PCur;

        If QueryProcessCycleTime(FProcessData.AttachedProcessHandle, @Cur) then
          FProcessData.CPUEllapsed := Cur;

        FProcessData.CPUTime := CurTime;

        Result := True;
      end;
    ptPerfomance:
      begin
        // дельта абсолютного времени
        PPrev := FProcessData.Ellapsed;
        FProcessData.Ellapsed := PCur;

        // дельта счетчика таймера CPU
        If QueryProcessCycleTime(FProcessData.AttachedProcessHandle, @Cur) then
        begin
          Prev := FProcessData.CPUEllapsed;
          FProcessData.CPUEllapsed := Cur;
        end
        else
          RaiseDebugCoreException();

        // Время CPU процесса
        PrevTime := FProcessData.CPUTime;
        FProcessData.CPUTime := CurTime;
        Delta := CurTime - PrevTime;

        // Добавляем только если процесс активен
        Result := (Delta > 0);
      end;
  end;

  if Result then
  begin
    FProcessData.DbgPoints.Lock;
    try
      ProcPoint := FProcessData.DbgPoints.Add;

      ProcPoint^.FromStart := PCur - FProcessData.Started;
      ProcPoint^.CPUTime := CurTime;

      ProcPoint^.PointType := PointType;
      case PointType of
        ptPerfomance:
          begin
            ProcPoint^.DeltaTick := PCur - PPrev;
            ProcPoint^.DeltaTickCPU := Cur - Prev;
            ProcPoint^.DeltaTime := Delta;
          end;
      end;
    finally
      FProcessData.DbgPoints.UnLock;
    end;
  end;
end;

function TDebuger.AddThread(const ThreadID: TThreadId; ThreadHandle: THandle): PThreadData;
begin
  FThreadList.Lock;
  try
    Result := FThreadList.Add;

    Result^.ThreadID := ThreadID;
    Result^.State := tsActive;
    Result^.ThreadHandle := ThreadHandle;
    Result^.ThreadAdvInfo := SetThreadInfo(ThreadId);
    Result^.ThreadAdvInfo^.ThreadData := Result;
    Result^.Context := GetMemory(SizeOf(TContext));
    Result^.Breakpoint := GetMemory(SizeOf(THardwareBreakpoint));
    Result^.Started := 0;
    Result^.Ellapsed := 0;
    Result^.ThreadEllapsed := 0;
    Result^.DbgPoints := TCollectList<TThreadPoint>.Create;
    //Result^.DbgMemInfo := TCollectList<TMemInfo>.Create;
    Result^.DbgGetMemInfo := TGetMemInfo.Create(1024);
    Result^.DbgExceptions := TThreadList.Create;

    if AddProcessPointInfo(ptThreadInfo) then
      AddThreadPointInfo(Result, ptStart);
  finally
    FThreadList.UnLock;
  end;
end;

function TDebuger.AddThreadInfo(const ThreadId: TThreadId): PThreadAdvInfo;
begin
  FThreadAdvInfoList.Lock;
  try
    Result := FThreadAdvInfoList.Add;
    Result^.ThreadId := ThreadId;
    Result^.ThreadData := Nil;
  finally
    FThreadAdvInfoList.UnLock;
  end;
end;

function TDebuger.AllocMem(const Size: Cardinal): Pointer;
begin
  // TODO: Проверить выделение памяти для маленьких Size
  Result := VirtualAllocEx(FProcessData.AttachedProcessHandle, Nil, Size, MEM_COMMIT Or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
  If Result = nil Then
    RaiseLastOsError;
end;

function TDebuger.AttachToProcess(const ProcessID: TProcessId; SentEntryPointBreakPoint: Boolean): Boolean;
begin
  Result := False;

  if FProcessData.State = psActive then
    Exit;

  FSetEntryPointBreakPoint := SentEntryPointBreakPoint;

  //FProcessInfo.State := psActive;
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
  ContinueStatus := DBG_EXCEPTION_NOT_HANDLED;

  if Assigned(FExceptioEvents[Code]) then
    FExceptioEvents[Code](Self, DebugEvent^.dwThreadId, @DebugEvent^.Exception.ExceptionRecord);
end;

procedure TDebuger.CallUnhandledExceptionEvents(const Code: TExceptionCode; DebugEvent: PDebugEvent);
begin
  ContinueStatus := DBG_EXCEPTION_NOT_HANDLED;

  if AddProcessPointInfo(ptException) then
    AddThreadPointInfo(CurThreadData, ptException, DebugEvent);

  if Assigned(FExceptioEvents[Code]) then
    FExceptioEvents[Code](Self, DebugEvent^.dwThreadId, @DebugEvent^.Exception.ExceptionRecord);
end;

procedure TDebuger.CheckBreakpointIndex(Value: Integer);
begin
  if (Value < 0) or (Value >= BreakpointCount) then
    raise EDebugCoreException.CreateFmt(SListIndexError, [Value]);
end;

function TDebuger.CheckIsAddrInRealMemoryBPRegion(BreakPointIndex: Integer; AAddr: Pointer): Boolean;
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
  FProcessData.Clear;

  for I := 0 to FThreadList.Count - 1 do
  begin
    ThData := FThreadList[I];
    ThData.Clear;
  end;

  FThreadList.Clear;

  FThreadAdvInfoList.Clear;
end;

constructor TDebuger.Create();

  function SetDebugPriv: Boolean;
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
        Result := AdjustTokenPrivileges(Token, False, tkp, 0, PTokenPrivileges(nil)^, PDWord(nil)^);
      end;
    end;
  end;

begin
  if not SetDebugPriv then
    RaiseLastOSError;

  FDbgState := dsNone;
  FRestoreBPIndex := -1;
  FRestoreMBPIndex := -1;
  FRestoredHWBPIndex := -1;
  FRestoredThread := 0;
  FCloseDebugProcess := True;
  FSetEntryPointBreakPoint := False;

  FThreadList := TCollectList<TThreadData>.Create;
  FThreadAdvInfoList := TCollectList<TThreadAdvInfo>.Create;

  ZeroMemory(@FProcessData, SizeOf(TProcessData));
  FProcessData.State := psNone;
  FProcessData.DbgPoints := Nil;

  //FDbgPerfomanceThread := TDbgPerfomanceThread.Create(Self, 10);
  FPerfomanceMode := False;
  FPerfCallStacks := False;
  FPerfomanceCheckPtr := Nil; //Pointer($76FED315);

  FDbgShareMem :=
    CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, 4 * 1024, 'DBG_SHARE_MEM');
end;

function TDebuger.CurThreadData: PThreadData;
begin
  if FCurThreadData = Nil then
    FCurThreadData := UpdateThreadContext(FCurThreadId);

  Result := FCurThreadData;
end;

function TDebuger.CurThreadId: TThreadId;
begin
  Result := FCurThreadId;
end;

function TDebuger.DebugNewProcess(const FilePath: string; SentEntryPointBreakPoint: Boolean): Boolean;
var
  PI: TProcessInformation;
  SI: TStartupInfo;
begin
  Result := False;
  if FProcessData.State = psActive then
    Exit;

  FSetEntryPointBreakPoint := SentEntryPointBreakPoint;

  ZeroMemory(@SI, SizeOf(TStartupInfo));
  SI.cb := SizeOf(TStartupInfo);
  SI.dwFlags := STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_SHOWNORMAL;

  Result := CreateProcess(PChar(FilePath), nil, nil, nil, False, DEBUG_PROCESS or DEBUG_ONLY_THIS_PROCESS, nil, nil, SI, PI);

  if Result then
  begin
    FProcessData.ProcessID := TProcessId(PI.dwProcessId);
    FProcessData.CreatedProcessHandle := PI.hProcess;
    FProcessData.CreatedThreadHandle := PI.hThread;
  end;
end;

destructor TDebuger.Destroy;
begin
  StopDebug;

  ClearDbgInfo;
  FreeAndNil(FThreadList);
  FreeAndNil(FThreadAdvInfoList);

  CloseHandle(FDbgShareMem);
  FDbgShareMem := 0;

  inherited;
end;

procedure TDebuger.DoCreateProcess(DebugEvent: PDebugEvent);
var
  CreateThreadInfo: TCreateThreadDebugInfo;
begin
  FDbgState := dsStarted;

  FProcessData.State := psActive;

  // Сохраняем данные о процессе
  FProcessData.AttachedFileHandle := DebugEvent^.CreateProcessInfo.hFile;
  FProcessData.AttachedProcessHandle := DebugEvent^.CreateProcessInfo.hProcess;
  FProcessData.AttachedThreadHandle := DebugEvent^.CreateProcessInfo.hThread;

  FProcessData.StartAddress := DebugEvent^.CreateProcessInfo.lpStartAddress;
  FProcessData.BaseOfImage := DebugEvent^.CreateProcessInfo.lpBaseOfImage;
  FProcessData.MainThreadID := DebugEvent^.dwThreadId;

  QueryPerformanceCounter(FProcessData.Started);
  FProcessData.DbgPoints := TCollectList<TProcessPoint>.Create;
  FProcessData.DbgGetMemInfo := TGetMemInfo.Create(1000);

  FProcessData.DbgExceptions := TThreadList.Create;

  // Метка старта процесса
  AddProcessPointInfo(ptStart);

  // Инициализация хуков
  //LoadLibrary('DbgHook32.dll'); // ??? Блокировка от преждевременной выгрузки
  if Assigned(gvDebugInfo) then
    gvDebugInfo.InitDebugHook;

  // Устанавливаем BreakPoint на точку входа процесса
  if FSetEntryPointBreakPoint then
    SetBreakpoint(FProcessData.StartAddress, 0, 'Process Entry Point Breakpoint');

  if Assigned(FCreateProcess) then
    FCreateProcess(Self, DebugEvent^.dwProcessId, @DebugEvent^.CreateProcessInfo);

  AddThread(DebugEvent^.dwThreadId, FProcessData.AttachedThreadHandle);
  SetThreadInfo(DebugEvent^.dwThreadId)^.ThreadName := 'Main thread';

  if Assigned(FCreateThread) then
  begin
    ZeroMemory(@CreateThreadInfo, SizeOf(CreateThreadInfo));
    CreateThreadInfo.hThread := FProcessData.AttachedThreadHandle;
    FCreateThread(Self, DebugEvent^.dwThreadId, @CreateThreadInfo);
  end;

  DoResumeAction(DebugEvent^.dwThreadId);
end;

procedure TDebuger.DoCreateThread(DebugEvent: PDebugEvent);
begin
  AddThread(DebugEvent^.dwThreadId, DebugEvent^.CreateThread.hThread);

  if Assigned(FCreateThread) then
  begin
    FCreateThread(Self, DebugEvent^.dwThreadId, @DebugEvent^.CreateThread);
    //DoResumeAction(DebugEvent^.dwThreadId);
  end;
end;

procedure TDebuger.DoDebugString(DebugEvent: PDebugEvent);
var
  Data: POutputDebugStringInfo;
  DbgStr: String;
begin
  Data := @DebugEvent^.DebugString;
  if Data^.fUnicode = 1 then
    DbgStr := String(PWideChar(gvDebuger.ReadStringW(Data^.lpDebugStringData, Data^.nDebugStringLength)))
  else
    DbgStr := String(PAnsiChar(gvDebuger.ReadStringA(Data^.lpDebugStringData, Data^.nDebugStringLength)));

  if Assigned(FDebugString) then
  begin
    FDebugString(Self, DebugEvent.dwThreadId, Data);
    //DoResumeAction(DebugEvent.dwThreadId);
  end;
end;

procedure TDebuger.DoExitProcess(DebugEvent: PDebugEvent);
begin
  FDbgState := dsStoping;
  FProcessData.State := psFinished;

  // Метка завершения процесса
  AddProcessPointInfo(ptStop);

  // Удаляем главный поток
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
  FDbgState := dsStoped;

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

procedure TDebuger.DoDebugerFailed;
begin
  FDbgState := dsDbgFail;

  if Assigned(FMainLoopFailed) then
    FMainLoopFailed(Self);
end;

procedure TDebuger.DoResumeAction(const ThreadID: TThreadId);
begin
  // здесь будем управлять поведением отладчика
  // после того как пользователь обработал любое из событий
  // например указывать что нужно сделать TraceIn или StepOver и т.п.
  // в зависимости от того что указал нам пользователь

  case ResumeAction of
    raTraceInto:
      SetSingleStepMode(ThreadID, False);

    raStepOver:
      { устанавливаем ВР на следующей инструкции } ;

    { для реализации нужно писать полноценный дизасм, т.к. StepOver
      применяется для целой кучи инструкций, вот список:
      CALL, INT, LOOP, LOOPZ, LOOPNZ, REP, REPZ, REPNZ, CMPS, CMPSB, CMPSW,
      LODSB, LODSW, MOVS, MOVSB, MOVSW, SCAS, SCASB, SCASW, STOS, STOSB, STOSW
      }

    raRunUntilReturn:
      { берем со стека адрес возврата и ставим ВР, тоже пока не реализовано } ;

    raStop:
      ContinueStatus := DBG_CONTROL_C;
  end;
end;

procedure TDebuger.DoRip(DebugEvent: PDebugEvent);
begin
  FDbgState := dsDbgFail;

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
  NeedUpdate: Boolean;
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

procedure TDebuger.FreeMem(Data: Pointer; const Size: Cardinal = 0);
begin
  if VirtualFreeEx(FProcessData.AttachedProcessHandle, Data, Size, MEM_RELEASE) = nil then
    RaiseLastOSError;
end;

function TDebuger.GetBPIndex(BreakPointAddr: Pointer; const ThreadID: TThreadId = 0): Integer;
var
  BP: PBreakpoint;
begin
  for Result := 0 to BreakpointCount - 1 do
  begin
    BP := @FBreakpointList[Result];
    if BP^.bpType <> btBreakpoint then
      Continue;

    if (BP^.Int3.Address = BreakPointAddr) and ((ThreadID = 0) or (BP^.ThreadId = ThreadId)) then
      Exit;
  end;

  Result := -1;
end;

procedure TDebuger.GetCallStack(ThData: PThreadData; var Stack: TStackPointList);

    Function AddStackEntry(Const Addr, EBP: Pointer) : PStackPoint;
    Begin
        Result := Nil;

        if Length(Stack) > 0 then
          if Stack[High(Stack)].EBP = EBP then
              Exit;

        If not IsBadCodePtr(Addr) Then
        Begin
            SetLength(Stack, Length(Stack) + 1);
            Result := @Stack[High(Stack)];

            Result.EIP := Addr;
            Result.EBP := EBP;
        End
    End;

Var
    EIP : Pointer;
    EBP : Pointer;
Begin
    EIP := Pointer(ThData^.Context.Eip);
    EBP := Pointer(ThData^.Context.Ebp);

    AddStackEntry(EIP, EBP);

    While IsValidAddr(EBP) Do
    Begin
        if not ReadData(IncPointer(EBP, SizeOf(Pointer)), @EIP, SizeOf(Pointer)) then
            Break;

        if not ReadData(EBP, @EBP, SizeOf(Pointer)) then
            Break;

        If AddStackEntry(EIP, EBP) = Nil Then
            Break;
    End;
end;

function GetMappedFileNameA(hProcess: THandle; lpv: Pointer; lpFilename: LPSTR; nSize: DWORD): DWORD; stdcall; external 'psapi.dll';

function TDebuger.GetDllName(lpImageName, lpBaseOfDll: Pointer; var Unicode: Boolean): AnsiString;
var
  DllNameAddr: Pointer;
  MappedName: array [0 .. MAX_PATH - 1] of AnsiChar;
begin
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
  Result := FExceptioEvents[Index];
end;

function TDebuger.GetFlag(const ThreadID: TThreadId; Flag: DWORD): Boolean;
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

  function CheckStartAddr(Data: Pointer): Boolean;
  begin
    Result := Cardinal(Data) <= Cardinal(BreakPointAddr);
  end;

  function CheckEndAddr(Data: Pointer; Size: DWORD): Boolean;
  begin
    Result := Cardinal(Data) + Size > Cardinal(BreakPointAddr);
  end;

var
  I: Integer;
begin
  Result := -1;
  for I := FromIndex to BreakpointCount - 1 do
  begin
    if FBreakpointList[I].bpType <> btMemoryBreakpoint then
      Continue;
    if CheckStartAddr(FBreakpointList[I].Memory.RegionStart) and CheckEndAddr(FBreakpointList[I].Memory.RegionStart,
      FBreakpointList[I].Memory.RegionSize) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TDebuger.GetProcessCPUTime: UInt64;
var
  CT, ET, KT, UT: TFileTime;
begin
  Result := 0;
  if GetProcessTimes(FProcessData.AttachedProcessHandle, CT, ET, KT, UT) then
    Result := FileTimeToInt64(KT) + FileTimeToInt64(UT);
end;

function TDebuger.GetRegisters(const ThreadID: TThreadId): TContext;
var
  ThData: PThreadData;
begin
  if (FCurThreadData <> Nil) and (ThreadID = FCurThreadData^.ThreadID) then
    Result := FCurThreadData^.Context^
  else
  begin
    ThData := UpdateThreadContext(ThreadId);
    if ThData <> nil then
      Result := ThData^.Context^
    else
      RaiseDebugCoreException();
  end;
end;

function TDebuger.GetThreadIndex(const ThreadID: TThreadId; const UseFinished: Boolean = False): Integer;
var
  ThData: PThreadData;
begin
  FThreadList.Lock;
  try
    for Result := FThreadList.Count - 1 downto 0 do
    begin
      ThData := FThreadList[Result];
      if (ThData^.ThreadID = ThreadID) and ((ThData^.State <> tsFinished) or UseFinished) then
        Exit;
    end;
  finally
    FThreadList.UnLock;
  end;

  Result := -1;
end;

function TDebuger.GetThreadInfo(const ThreadId: TThreadId): PThreadAdvInfo;
var
  Idx: Integer;
begin
  Result := Nil;

  FThreadAdvInfoList.Lock;
  try
    Idx := GetThreadInfoIndex(ThreadId);
    if Idx >= 0 then
      Result := FThreadAdvInfoList[Idx];
  finally
    FThreadAdvInfoList.UnLock;
  end;
end;

function TDebuger.GetThreadInfoIndex(const ThreadId: TThreadId): Integer;
var
  ThInfo: PThreadAdvInfo;
begin
  FThreadAdvInfoList.Lock;
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
    FThreadAdvInfoList.UnLock;
  end;

  Result := -1;
end;

function TDebuger.GetThreadCount: Integer;
begin
  Result := FThreadList.Count;
end;

function TDebuger.GetThreadCPUTime(ThreadData: PThreadData): UInt64;
var
  CT, ET, KT, UT: TFileTime;
begin
  Result := 0;
  if GetThreadTimes(ThreadData^.ThreadHandle, CT, ET, KT, UT) then
    Result := FileTimeToInt64(KT) + FileTimeToInt64(UT);
end;

function TDebuger.GetThreadData(const ThreadID: TThreadId; const UseFinished: Boolean = False): PThreadData;
var
  Index: Integer;
begin
  Index := GetThreadIndex(ThreadId);
  if Index >= 0 then
    Result := FThreadList[Index]
  else
    Result := nil;
end;

function TDebuger.GetThreadDataByIdx(const Idx: Cardinal): PThreadData;
begin
  Result := Nil;
  FThreadList.Lock;
  try
    if Idx < FThreadList.Count then
      Result := FThreadList[Idx];
  finally
    FThreadList.UnLock;
  end;
end;

function TDebuger.IsBreakpointPresent(const Value: TBreakpoint): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to BreakpointCount - 1 do
    case FBreakpointList[I].bpType of
      btBreakpoint:
        if FBreakpointList[I].Int3.Address = Value.Int3.Address then
        begin
          Result := True;
          Break;
        end;
      btMemoryBreakpoint:
        if CheckIsAddrInRealMemoryBPRegion(I, Value.Memory.Address) or
          CheckIsAddrInRealMemoryBPRegion(I, Pointer(Cardinal(Value.Memory.Address) + Value.Memory.Size - 1)) then
        begin
          Result := True;
          Break;
        end;
    end;
end;

function TDebuger.IsValidAddr(const Addr: Pointer): Boolean;
Var
  mbi: PMemoryBasicInformation;
Begin
  Result := False;
  mbi := GetMemory(SizeOf(TMemoryBasicInformation));
  try
    Result := (VirtualQueryEx(FProcessData.AttachedProcessHandle, Addr, mbi^, SizeOf(TMemoryBasicInformation)) <> 0);
  finally
    FreeMemory(mbi);
  end;
end;

function TDebuger.IsValidCodeAddr(const Addr: Pointer): Boolean;
Const
  _PAGE_CODE: Cardinal = PAGE_EXECUTE Or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE Or PAGE_EXECUTE_WRITECOPY;
Var
  mbi: PMemoryBasicInformation;
Begin
  Result := False;
  mbi := GetMemory(SizeOf(TMemoryBasicInformation));
  try
    if (VirtualQueryEx(FProcessData.AttachedProcessHandle, Addr, mbi^, SizeOf(TMemoryBasicInformation)) <> 0) then
      Result := ((mbi^.Protect And _PAGE_CODE) <> 0);
  finally
    FreeMemory(mbi);
  end;
end;

function TDebuger.IsValidProcessCodeAddr(const Addr: Pointer): Boolean;
Begin
  Result := False;

  if FProcessData.PEImage <> Nil then
    Result := (Cardinal(Addr) >= Cardinal(FProcessData.BaseOfImage)) and
      (Cardinal(Addr) <= (Cardinal(FProcessData.BaseOfImage) + FProcessData.PEImage.OptionalHeader32.SizeOfCode));
end;

function TDebuger.PauseDebug: Boolean;
begin
  Result := DebugBreakProcess(FProcessData.AttachedProcessHandle);
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
  aParamsSize: Cardinal; WaitAndFree: Boolean = True);
var
  hThread: THandle;
  lpNumberOfBytes: NativeUInt;
  lpThreadId: Cardinal;
  ThreadAddr, ParamAddr: Pointer;
begin
  // Выделяем место в памяти процесса, и записываем туда нашу функцию
  ThreadAddr := VirtualAllocEx(hProcess, nil, FuncSize, MEM_COMMIT, PAGE_READWRITE);
  if not WriteProcessMemory(hProcess, ThreadAddr, Func, FuncSize, Cardinal(lpNumberOfBytes)) then
    RaiseDebugCoreException();

  // Также запишем параметры к ней
  if (aParams <> nil) and (aParamsSize > 0) then
  begin
    ParamAddr := VirtualAllocEx(hProcess, nil, aParamsSize, MEM_COMMIT, PAGE_READWRITE);
    if not WriteProcessMemory(hProcess, ParamAddr, aParams, aParamsSize, Cardinal(lpNumberOfBytes)) then
      RaiseDebugCoreException();
  end
  else
    ParamAddr := Nil;

  // Создаем поток, в котором все это будет выполняться.
  hThread := CreateRemoteThread(hProcess, nil, 0, ThreadAddr, ParamAddr, 0, lpThreadId);

  if WaitAndFree then
  begin
    // Ожидаем завершения функции
    WaitForSingleObject(hThread, INFINITE);

    // подчищаем за собой
    CloseHandle(hThread);
    VirtualFreeEx(hProcess, ParamAddr, 0, MEM_RELEASE);
    VirtualFreeEx(hProcess, ThreadAddr, 0, MEM_RELEASE);
  end;
end;

function TDebuger.PerfomancePauseDebug: Boolean;
begin
  // Эта функция создает поток с функцией в контексте процесса, которая вызывает DebugBreak
  // Проблема в том, что не ожидается, пока брекпоинт отработается
  //Result := DebugBreakProcess(FProcessInfo.AttachedProcessHandle);

  InjectPerfThread;
  Result := True;
end;

procedure TDebuger.ProcessExceptionBreakPoint(DebugEvent: PDebugEvent);
var
  ReleaseBP: Boolean;
  BreakPointIndex: Integer;
begin
  ReleaseBP := False;
  FRemoveCurrentBreakpoint := False;

  BreakPointIndex := GetBPIndex(DebugEvent^.Exception.ExceptionRecord.ExceptionAddress, DebugEvent^.dwThreadId);
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
  end
  else
    CallUnhandledBreakPointEvents(ecBreakpoint, DebugEvent);
end;

procedure TDebuger.ProcessExceptionGuardPage(DebugEvent: PDebugEvent);
var
  CurrentMBPIndex: Integer;

  function CheckWriteMode: Boolean;
  begin
    Result := not FBreakpointList[CurrentMBPIndex].Memory.BreakOnWrite;
    if not Result then
      Result := DebugEvent^.Exception.ExceptionRecord.ExceptionInformation[0] = 1;
  end;

var
  MBPIndex: Integer;
  ReleaseMBP: Boolean;
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
var
  Handled: Boolean;
begin
  // ОБрабатываем HWBP
  Handled := ProcessHardwareBreakpoint(DebugEvent);

  // Если событие по из-за HWPB восстанавливаем предыдущий HWBP
  if not Handled and (Cardinal(FRestoredThread) <> 0) and (FRestoredHWBPIndex >= 0) then
  begin
    ToggleHardwareBreakpoint(FRestoredThread, FRestoredHWBPIndex, True);
    FRestoredThread := 0;
    FRestoredHWBPIndex := -1;
  end;

  // Восстанавливаем ВР
  if FRestoreBPIndex >= 0 then
  begin
    CheckBreakpointIndex(FRestoreBPIndex);
    if FBreakpointList[FRestoreBPIndex].bpType = btBreakpoint then
      ToggleInt3Breakpoint(FRestoreBPIndex, True);
    FRestoreBPIndex := -1;
  end;

  // Восстанавливаем MВР
  if FRestoreMBPIndex >= 0 then
  begin
    CheckBreakpointIndex(FRestoreMBPIndex);
    if FBreakpointList[FRestoreMBPIndex].bpType = btMemoryBreakpoint then
      ToggleMemoryBreakpoint(FRestoreMBPIndex, True);
    FRestoreMBPIndex := -1;
  end;

  // если на предыдущий итерации был выставлен режим трассировки
  // уведомляем о нем пользователя
  if ResumeAction <> raRun then
  begin
    CallUnhandledExceptionEvents(ecSingleStep, DebugEvent);

    // после чего настраиваем отладчик в зависимости от команды пользователя
    DoResumeAction(DebugEvent^.dwThreadId);
  end;
end;

function TDebuger.ProcessHardwareBreakpoint(DebugEvent: PDebugEvent): Boolean;
var
  Index: Integer;
  ReleaseBP: Boolean;
  //ThData: PThreadData;
  Context: PContext;
begin
  Result := False;

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
    // если два HWBP идут друг за другом,
    // то т.к. восстановление происходит через индексы
    // в ProcessExceptionSingleStep, индекс предыдущего HWBP будет претерт
    // поэтому перед перетиранием индексов нужно восстановить предыдущий HWBP
    if (Cardinal(FRestoredThread) <> 0) and (FRestoredHWBPIndex >= 0) then
      ToggleHardwareBreakpoint(FRestoredThread, FRestoredHWBPIndex, True);

    FRestoredHWBPIndex := Index;
    FRestoredThread := DebugEvent^.dwThreadId;
  end;
end;

procedure TDebuger.ProcessDbgPerfomance(DebugEvent: PDebugEvent);
var
  ThData: PThreadData;
  I: Integer;
begin
  FDbgState := dsPerfomance;

  // Добавляем инфу про состояние процесса
  if AddProcessPointInfo(ptPerfomance) then
  begin
    // Если процесс активен, то добавляем инфу про активные потоки
    FThreadList.Lock;
    try
      for I := 0 to FThreadList.Count - 1 do
      begin
        ThData := FThreadList[I];
        if ThData^.State = tsActive then
          AddThreadPointInfo(ThData, ptPerfomance);
      end;
    finally
      FThreadList.UnLock;
    end;
  end;
end;

function TDebuger.ReadData(AddrPrt, ResultPtr: Pointer; DataSize: Integer): Boolean;
var
  Dummy: NativeUInt;
begin
  Result := ReadProcessMemory(FProcessData.AttachedProcessHandle, AddrPrt, ResultPtr, DataSize, Cardinal(Dummy)) and
    (Integer(Dummy) = DataSize);
end;

function TDebuger.ReadStringA(AddrPrt: Pointer; Len: Integer = 0): AnsiString;
var
  C: AnsiChar;
begin
  Result := '';

  if Len = -1 then
  begin
    // передан указатель на PAnsiChar. Читаем до первого #0
    // TODO: Переписать это на чтение буфером
    repeat
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

procedure TDebuger.RemoveCurrentBreakpoint;
begin
  FRemoveCurrentBreakpoint := True;
end;

procedure TDebuger.RemoveThread(const ThreadID: TThreadId);
var
  ThData: PThreadData;
begin
  FThreadList.Lock;
  try
    ThData := GetThreadData(ThreadID);
    if ThData <> nil then
    begin
      if ThData^.Breakpoint <> nil then
      begin
        FreeMemory(ThData^.Breakpoint);
        ThData^.Breakpoint := nil;
      end;

      ThData^.State := tsFinished;

      if AddProcessPointInfo(ptThreadInfo) then
        AddThreadPointInfo(ThData, ptStop);

      UpdateMemoryInfoObjectTypes;
    end;
  finally
    FThreadList.UnLock;
  end;
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
  end;
end;

var
  _DbgMemInfoList: TDbgMemInfoList;

function TDebuger.FindMemoryPointer(const Ptr: Pointer): PThreadData;
var
  Idx: Integer;
begin
  Idx := 0;
  repeat
    Result := GetThreadDataByIdx(Idx);
    if Result <> Nil then
    begin
      if Result^.DbgGetMemInfo.ContainsKey(Ptr) then
        Exit;

      Inc(Idx);
    end;
  until Result = Nil;
end;

procedure TDebuger.LoadMemoryInfoPack(MemInfoPack: Pointer; const Count: Cardinal);
var
  Idx: Integer;
  DbgMemInfo: PDbgMemInfo;
  CurPerfIdx: Cardinal;
  ThData: PThreadData;
  MemInfo: PGetMemInfo;
  NewMemInfo: PGetMemInfo;
begin
  if ReadData(MemInfoPack, @_DbgMemInfoList, Count * SizeOf(TDbgMemInfo)) then
  begin
    CurPerfIdx := ProcessData.CurDbgPointIdx;

    // TODO: Можно вынести обработку в отдельный поток
    ThData := Nil;
    for Idx := 0 to Count - 1 do
    begin
      DbgMemInfo := @_DbgMemInfoList[Idx];
      if (ThData = Nil) or (ThData^.ThreadID <> DbgMemInfo^.ThreadId) then
        ThData := GetThreadData(DbgMemInfo^.ThreadId, True);

      if ThData <> Nil then
        case DbgMemInfo^.MemInfoType of
          miGetMem:
          begin
            // Если такой указатель ещё есть, то это потеряшка
            if ThData^.DbgGetMemInfo.TryGetValue(DbgMemInfo^.Ptr, MemInfo) then
            begin
              // Переносим инфу в процесс
              Dec(ThData^.DbgGetMemInfoSize, MemInfo.Size);

              NewMemInfo := GetMemory(SizeOf(RGetMemInfo));
              NewMemInfo^.PerfIdx := MemInfo^.PerfIdx;
              NewMemInfo^.Size := MemInfo^.Size;
              NewMemInfo^.Stack := MemInfo^.Stack;
              NewMemInfo^.ObjectType := MemInfo^.ObjectType;

              ThData^.DbgGetMemInfo.Remove(DbgMemInfo^.Ptr);

              ProcessData.DbgGetMemInfo.AddOrSetValue(DbgMemInfo^.Ptr, NewMemInfo);
              Inc(FProcessData.DbgGetMemInfoSize, NewMemInfo^.Size);
            end;

            NewMemInfo := GetMemory(SizeOf(RGetMemInfo));
            NewMemInfo^.PerfIdx := CurPerfIdx;
            NewMemInfo^.Size := DbgMemInfo^.Size;
            NewMemInfo^.Stack := DbgMemInfo^.Stack;
            NewMemInfo^.ObjectType := ''; // На этот момент тип ещё может быть неопределен

            ThData^.DbgGetMemInfo.AddOrSetValue(DbgMemInfo^.Ptr, NewMemInfo);
            Inc(ThData^.DbgGetMemInfoSize, NewMemInfo^.Size);
          end;
          miFreeMem:
          begin
            // Ищем в своем потоке
            if ThData^.DbgGetMemInfo.TryGetValue(DbgMemInfo^.Ptr, MemInfo) then
            begin
              Dec(ThData^.DbgGetMemInfoSize, MemInfo^.Size);
              ThData^.DbgGetMemInfo.Remove(DbgMemInfo^.Ptr);
            end
            else
            begin
              // Ищем во всех потоках
              ThData := FindMemoryPointer(DbgMemInfo^.Ptr);
              if ThData <> Nil then
              begin
                if ThData^.DbgGetMemInfo.TryGetValue(DbgMemInfo^.Ptr, MemInfo) then
                begin
                  Dec(ThData^.DbgGetMemInfoSize, MemInfo^.Size);
                  ThData^.DbgGetMemInfo.Remove(DbgMemInfo^.Ptr);
                end
                else
                begin
                  // Ищем в потеряшках
                  if ProcessData.DbgGetMemInfo.TryGetValue(DbgMemInfo^.Ptr, MemInfo) then
                  begin
                    Dec(FProcessData.DbgGetMemInfoSize, MemInfo^.Size);
                    ProcessData.DbgGetMemInfo.Remove(DbgMemInfo^.Ptr);
                  end;
                end;
              end;
            end;
          end;
        end;
    end;
  end
  else
    RaiseDebugCoreException();
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
  Ptr := Pointer(ER^.ExceptionInformation[1]);
  Size := ER^.ExceptionInformation[2];

  case DbgInfoType of
    dstMemInfo:
      LoadMemoryInfoPack(Ptr, Size);
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
        ThInfo^.ThreadClassName := String(Str);
    end;
  end;
end;

procedure TDebuger.ProcessDebugEvents;
var
  DebugEvent: PDebugEvent; // !!! Необходимо выравнивание по памяти
  ExceptionCode: DWORD;
  CallNextLoopIteration: Boolean;
begin
  DebugEvent := GetMemory(SizeOf(TDebugEvent));
  try
    ZeroMemory(DebugEvent, SizeOf(TDebugEvent));

    repeat
      ContinueStatus := DBG_CONTINUE;

      FDbgState := dsWait;
      FCurThreadData := nil;
      FCurThreadId := 0;

      if not WaitForDebugEvent(DebugEvent^, INFINITE) then
      begin
        DoDebugerFailed;
        Exit;
      end;

      FDbgState := dsEvent;

      FCurThreadData := nil;
      FCurThreadId := TThreadId(DebugEvent^.dwThreadId);

      case DebugEvent^.dwDebugEventCode of
        EXCEPTION_DEBUG_EVENT:
          begin
            ExceptionCode := DebugEvent^.Exception.ExceptionRecord.ExceptionCode;

            case ExceptionCode of
              DBG_EXCEPTION:
                ProcessDbgException(DebugEvent);

              EXCEPTION_SINGLE_STEP:
                ProcessExceptionSingleStep(DebugEvent);

              EXCEPTION_BREAKPOINT:
                ProcessExceptionBreakPoint(DebugEvent);

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

function TDebuger.SetBreakpoint(Address: Pointer; const ThreadId: TThreadId = 0; const Description: string = ''): Boolean;
var
  Breakpoint: TBreakpoint;
  OldProtect: DWORD;
  Dummy: NativeUInt;
begin
  ZeroMemory(@Breakpoint, SizeOf(TBreakpoint));

  Breakpoint.bpType := btBreakpoint;
  Breakpoint.ThreadId := ThreadId;
  Breakpoint.Active := True;
  Breakpoint.Int3.Address := Address;
  Breakpoint.Description := ShortString(Description);

  Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, Address, 1, PAGE_READWRITE, OldProtect));
  try
    Check(ReadProcessMemory(FProcessData.AttachedProcessHandle, Address, @Breakpoint.Int3.ByteCode, 1, Cardinal(Dummy)));
    Check(WriteProcessMemory(FProcessData.AttachedProcessHandle, Address, @BPOpcode, 1, Cardinal(Dummy)));
  finally
    Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, Address, 1, OldProtect, OldProtect));
  end;

  Result := AddNewBreakPoint(Breakpoint);
end;

function DebugSetProcessKillOnExit(KillOnExit: BOOL): BOOL; stdcall; external kernel32;

procedure TDebuger.SetCloseDebugProcess(const Value: Boolean);
begin
  FCloseDebugProcess := Value;
  DebugSetProcessKillOnExit(CloseDebugProcessOnFree);
end;

procedure TDebuger.SetExceptionEvent(const Index: TExceptionCode; const Value: TDefaultExceptionEvent);
begin
  FExceptioEvents[Index] := Value;
end;

procedure TDebuger.SetFlag(const ThreadID: TThreadId; Flag: DWORD; Value: Boolean);
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

function TDebuger.SetMemoryBreakpoint(Address: Pointer; Size: Cardinal; BreakOnWrite: Boolean; const Description: string): Boolean;
var
  Breakpoint: TBreakpoint;
  MBI: TMemoryBasicInformation;
  Index: Integer;
begin
  Index := GetMBPIndex(Address);
  if (Index >= 0) and (FBreakpointList[Index].bpType = btMemoryBreakpoint) then
  begin
    MBI.BaseAddress := FBreakpointList[Index].Memory.RegionStart;
    MBI.RegionSize := FBreakpointList[Index].Memory.RegionSize;
    MBI.Protect := FBreakpointList[Index].Memory.PreviosRegionProtect;
  end
  else
    Check(VirtualQueryEx(ProcessData.AttachedProcessHandle, Address, MBI, SizeOf(TMemoryBasicInformation)) > 0);
  ZeroMemory(@Breakpoint, SizeOf(TBreakpoint));
  Breakpoint.bpType := btMemoryBreakpoint;
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

procedure TDebuger.SetPerfCallStacks(const Value: Boolean);
begin
  FPerfCallStacks := Value;
end;

procedure TDebuger.SetPerfomanceMode(const Value: Boolean);
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

function DebugActiveProcessStop(dwProcessId: DWORD): BOOL; stdcall; external kernel32;

procedure TDebuger.SetSingleStepMode(const ThreadID: TThreadId; RestoreEIPAfterBP: Boolean);
var
  Context: TContext;
  ThData: PThreadData;
begin
  ThData := GetThreadData(ThreadID);

  ZeroMemory(@Context, SizeOf(TContext));

  Context.ContextFlags := CONTEXT_FULL;
  Check(GetThreadContext(ThData^.ThreadHandle, Context));
  if RestoreEIPAfterBP then
    Dec(Context.Eip);
  Context.EFlags := Context.EFlags or EFLAGS_TF;
  Check(SetThreadContext(ThData^.ThreadHandle, Context));
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
begin
  StrAddr := Pointer(DebugEvent^.Exception.ExceptionRecord.ExceptionInformation[1]);
  Str := ReadStringA(StrAddr, -1);
  if Str <> '' then
    SetThreadInfo(DebugEvent^.dwThreadId)^.ThreadName := String(Str);
end;

function TDebuger.StopDebug: Boolean;
begin
  Result := False;

  if not(FDbgState in [dsNone, dsStoped, dsDbgFail]) then
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

procedure TDebuger.ToggleBreakpoint(Index: Integer; Active: Boolean);
begin
  CheckBreakpointIndex(Index);
  case FBreakpointList[Index].bpType of
    btBreakpoint:
      ToggleInt3Breakpoint(Index, Active);
    btMemoryBreakpoint:
      ToggleMemoryBreakpoint(Index, Active);
  end;
end;

procedure TDebuger.ToggleHardwareBreakpoint(const ThreadId: TThreadID; Index: THWBPIndex; Active: Boolean);
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

procedure TDebuger.ToggleInt3Breakpoint(Index: Integer; Active: Boolean);
var
  OldProtect: DWORD;
  Dummy: NativeUInt;
begin
  CheckBreakpointIndex(Index);

  if FBreakpointList[Index].bpType <> btBreakpoint then
    Exit;

  if FBreakpointList[Index].Active = Active then
    Exit;

  Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, FBreakpointList[Index].Int3.Address, 1, PAGE_READWRITE, OldProtect));
  try
    if Active then
      Check(WriteProcessMemory(FProcessData.AttachedProcessHandle, FBreakpointList[Index].Int3.Address, @BPOpcode, 1, Cardinal(Dummy)))
    else
      Check(WriteProcessMemory(FProcessData.AttachedProcessHandle, FBreakpointList[Index].Int3.Address, @FBreakpointList[Index].Int3.ByteCode, 1,
          Cardinal(Dummy)));
  finally
    Check(VirtualProtectEx(FProcessData.AttachedProcessHandle, FBreakpointList[Index].Int3.Address, 1, OldProtect, OldProtect));
  end;

  FBreakpointList[Index].Active := Active;
end;

procedure TDebuger.ToggleMemoryBreakpoint(Index: Integer; Active: Boolean);
var
  Dummy, TmpSize: DWORD;
begin
  CheckBreakpointIndex(Index);
  if FBreakpointList[Index].bpType <> btMemoryBreakpoint then
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

procedure TDebuger.UpdateHardwareBreakpoints(const ThreadID: TThreadId);
var
  Context: TContext;
  I: THWBPIndex;
  ThData: PThreadData;
begin
  ThData := GetThreadData(ThreadID);
  if ThData = nil then
    Exit;

  ZeroMemory(@Context, SizeOf(TContext));
  Context.ContextFlags := CONTEXT_DEBUG_REGISTERS;

  for I := 0 to 3 do
  begin
    if not ThData^.Breakpoint.Active[I] then
      Continue;
    if ThData^.Breakpoint.Address[I] <> nil then
    begin
      Context.Dr7 := Context.Dr7 or DR7_SET_LOC_ON;
      case I of
        0: Context.Dr0 := DWORD(ThData^.Breakpoint.Address[I]);
        1: Context.Dr1 := DWORD(ThData^.Breakpoint.Address[I]);
        2: Context.Dr2 := DWORD(ThData^.Breakpoint.Address[I]);
        3: Context.Dr3 := DWORD(ThData^.Breakpoint.Address[I]);
      end;
      Context.Dr7 := Context.Dr7 or DR_On[I];
      Context.Dr7 := Context.Dr7 or DR_Mode[I, ThData^.Breakpoint.Mode[I]];
      Context.Dr7 := Context.Dr7 or DR_Size[I, ThData^.Breakpoint.Size[I]];
    end;
  end;

  Check(SetThreadContext(ThData^.ThreadHandle, Context));
end;

procedure TDebuger.UpdateMemoryInfoObjectTypes;
var
  Idx: Integer;
  I: Integer;
  ThData: PThreadData;
  GetMemInfo: TGetMemInfo;
  GetMemInfoItem: TGetMemInfoItem;
begin
  Idx := 0;
  repeat
    ThData := GetThreadDataByIdx(Idx);
    if ThData <> Nil then
    begin
      GetMemInfo := ThData^.DbgGetMemInfo;
      if GetMemInfo.Count > 0 then
      begin
        for GetMemInfoItem in GetMemInfo do
          GetMemInfoItem.Value^.ObjectType := GetMemInfoItem.Value^.GetObjectType(GetMemInfoItem.Key);
      end;

      Inc(Idx);
    end;
  until ThData = Nil;

  // Потеряшки
  GetMemInfo := ProcessData.DbgGetMemInfo;
  if GetMemInfo.Count > 0 then
  begin
    for GetMemInfoItem in GetMemInfo do
      GetMemInfoItem.Value^.ObjectType := GetMemInfoItem.Value^.GetObjectType(GetMemInfoItem.Key);
  end;
end;

function TDebuger.UpdateThreadContext(ThreadData: PThreadData): Boolean;
begin
  Result := False;
  if ThreadData <> Nil then
  begin
    ZeroMemory(ThreadData^.Context, SizeOf(TContext));

    ThreadData^.Context^.ContextFlags := CONTEXT_FULL or CONTEXT_DEBUG_REGISTERS;
    Result := GetThreadContext(ThreadData^.ThreadHandle, ThreadData^.Context^);
  end;
end;

function TDebuger.UpdateThreadContext(const ThreadID: TThreadId): PThreadData;
begin
  Result := GetThreadData(ThreadId);
  if Result <> nil then
    if not UpdateThreadContext(Result) then
      RaiseDebugCoreException();
end;

function TDebuger.WriteData(AddrPrt, DataPtr: Pointer; const DataSize: Cardinal): Boolean;
var
  Dummy: NativeUInt;
begin
  Result :=
    WriteProcessMemory(FProcessData.AttachedProcessHandle, AddrPrt, DataPtr, DataSize, Cardinal(Dummy)) and (Dummy = DataSize);

  if Result then
    Result := FlushInstructionCache(FProcessData.AttachedProcessHandle, AddrPrt, DataSize);
end;


initialization
  QueryPerformanceFrequency(_FreqPerSec);
  _FreqPerMSec := _FreqPerSec div 1000;

end.
