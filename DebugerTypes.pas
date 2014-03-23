unit DebugerTypes;

interface

uses SysUtils, Windows, Classes, JclPeImage, SyncObjs, ClassUtils, DbgHookTypes, Contnrs,
  Generics.Collections, Collections.Dictionaries, CollectList;

type
  TSysUInt = NativeUInt;

const
  EXCEPTION_SET_THREAD_NAME = $406D1388;

// Флаги процессора
const
  EFLAGS_CF = $001;
  EFLAGS_PF = $004;
  EFLAGS_AF = $010;
  EFLAGS_ZF = $040;
  EFLAGS_SF = $080;
  EFLAGS_TF = $100;
  EFLAGS_IF = $200;
  EFLAGS_DF = $400;
  EFLAGS_OF = $800;

const
  BPOpcode: Byte = $CC;

type
  TThreadId = type Cardinal;
  TProcessId = type Cardinal;

  EDebugCoreException = class(Exception);

  // Действия выполняемые отладчиком после генерации события
  TResumeAction = (
    raRun, // продолжить выполнение
    raTraceInto, // выполнить трассировку одной инструкции с заходом внутрь функций
    raStepOver, // выполнить трассировку одной инструкции без захода внутрь функций
    raRunUntilReturn, // продолжить выполнение до выхода из текущей процедуры
    raStop // остановить отладку
  );

  // Декларация основных обработчиков
  TCreateThreadEvent = procedure(Sender: TObject; ThreadId: TThreadId; Data: PCreateThreadDebugInfo) of object;
  TCreateProcessEvent = procedure(Sender: TObject; ProcessId: TProcessId; Data: PCreateProcessDebugInfo) of object;
  TExitThreadEvent = procedure(Sender: TObject; ThreadId: TThreadId; Data: PExitThreadDebugInfo) of object;
  TExitProcessEvent = procedure(Sender: TObject; ProcessId: TProcessId; Data: PExitProcessDebugInfo) of object;
  TLoadDllEvent = procedure(Sender: TObject; ThreadId: TThreadId; Data: PLoadDLLDebugInfo) of object;
  TUnLoadDllEvent = procedure(Sender: TObject; ThreadId: TThreadId; Data: PUnloadDLLDebugInfo) of object;
  TDebugStringEvent = procedure(Sender: TObject; ThreadId: TThreadId; Data: POutputDebugStringInfo) of object;
  TRipEvent = procedure(Sender: TObject; ThreadId: TThreadId; Data: PRIPInfo) of object;
  TDbgLogEvent = procedure(Sender: TObject; ThreadId: TThreadId; const Data: String) of object;

  // Декларация обработчиков отладочных исключений
  TDefaultExceptionEvent = procedure(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord) of object;
  TBreakPointEvent = procedure(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord;
    BreakPointIndex: Integer; var ReleaseBreakpoint: Boolean) of object;

  // Список типов отладочных исключений (для внутренней работы отладчика,
  // пользователь с ними не работает)

  TExceptionCode = (ecUnknown, ecBreakpoint, ecSingleStep, ecCtrlC, ecNonContinuable, ecGuard, ecSetThreadName);

  // Список поддерживаемых типов точек остановки (далее ВР)

  TBreakpointType = (
    btUser,
    btTemp,
    btCodeTrack,
    btMemory
  );

  // структуры для хранения данных об известных отладчику ВР

  TInt3Breakpoint = record
    Address: Pointer;
    SaveByte: Byte;
  end;

  TMemotyBreakPoint = record
    Address: Pointer;
    Size: Cardinal;
    BreakOnWrite: Boolean;
    RegionStart: Pointer;
    RegionSize: Cardinal;
    PreviosRegionProtect: Cardinal;
  end;

  PBreakpoint = ^TBreakpoint;
  TBreakpoint = packed record
    bpType: TBreakpointType;
    ThreadId: TThreadId;
    Description: ShortString;
    Active: Boolean;
    case Integer of
      0: (Int3: TInt3Breakpoint);
      1: (Memory: TMemotyBreakPoint);
  end;

  TBreakpointList = array of TBreakpoint;

  TTrackBreakpointType = (tbTrackFunc, tbMemInfo);
  TTrackBreakpointTypes = set of TTrackBreakpointType;

  PTrackBreakpoint = ^TTrackBreakpoint;
  TTrackBreakpoint = record
    FuncInfo: TObject;
    SaveByte: Byte;
    BPType: TTrackBreakpointTypes;
  end;

  TTrackBreakpointList = TPointerDictionary<Pointer,PTrackBreakpoint>;

  PTrackRETBreakpoint = ^TTrackRETBreakpoint;
  TTrackRETBreakpoint = record
    FuncInfo: TObject;
    Count: Cardinal;
    SaveByte: Byte;
    BPType: TTrackBreakpointTypes;
  end;

  TTrackRETBreakpointList = TPointerDictionary<Pointer,PTrackRETBreakpoint>;

  THWBPIndex = 0..3;
  THWBPSize = (hsByte, hdWord, hsDWord);
  THWBPMode = (hmExecute, hmWrite, hmIO, hmReadWrite);

  PHardwareBreakpoint = ^THardwareBreakpoint;
  THardwareBreakpoint = packed record
    Address: array [THWBPIndex] of Pointer;
    Size: array [THWBPIndex] of THWBPSize;
    Mode: array [THWBPIndex] of THWBPMode;
    Description: array [THWBPIndex] of ShortString;
    Active: array [THWBPIndex] of Boolean;
  end;

  (*
  PStackPoint = ^TStackPoint;
  TStackPoint = packed record
    EIP: Pointer;
    EBP: Pointer;
  end;

  TStackPointList = Array of TStackPoint;
  *)

  TDbgInfoStack = array of Pointer;

  TMemAction = (maGetMem = 0, maFreeMem);

  PMemInfo = ^TMemInfo;
  TMemInfo = packed record
    //PerfIdx: Cardinal;
    case MemAction: TMemAction of
      maGetMem: (
        GetMemPtr: Pointer;
        GetMemSize: Cardinal;
        //Stack: TStackPointList;
      );
      maFreeMem: (
        FreeMemPtr: Pointer;
        //ObjType: Cardinal;
      );
  end;

  //PGetMemInfo = ^TGetMemInfo;
  TGetMemInfo = class
  public
    PerfIdx: Cardinal;
    ObjAddr: Pointer;
    Size: Cardinal;
    Stack: TDbgInfoStack;
    ObjectType: String;

    function GetObjectType: String;
    procedure CheckObjectType;
    procedure LoadStack(const DbgStack: PDbgHookInfoStack);
  end;

  TGetMemInfoList = TObjectDictionary<Pointer,TGetMemInfo>;
  TGetMemInfoItem = TPair<Pointer,TGetMemInfo>;

  PDbgSyncObjsInfoEx = ^TDbgSyncObjsInfoEx;
  TDbgSyncObjsInfoEx = packed record
  public
    procedure LoadStack(const DbgStack: PDbgHookInfoStack);
    procedure Init(const SyncObjsInfo: PDbgSyncObjsInfo);
  public
    Id: NativeUInt;
    ThreadId: Cardinal;
    CurTime: Int64;
    Stack: TDbgInfoStack;
    SyncObjsStateType: TDbgSyncObjsStateType;
    case SyncObjsType: TDbgSyncObjsType of
      soUnknown:
      ();
      soSleep:
      (
        MSec: NativeUInt;
      );
      soWaitForSingleObject:
      (
        Handle: THandle;
      );
      soWaitForMultipleObjects:
      (
        Handles: PWOHandleArray;
      );
      soEnterCriticalSection,
      soLeaveCriticalSection,
      soInCriticalSection:
      (
        CS: PRTLCriticalSection;
        OwningThreadId: Cardinal;
      );
      soSendMessage:
      ();
  end;

  PSyncObjsInfo = ^RSyncObjsInfo;
  RSyncObjsInfo = record
    PerfIdx: Cardinal;
    Link: PSyncObjsInfo;
    SyncObjsInfo: TDbgSyncObjsInfoEx;
  public
    function WaitTime: Int64;
  end;

  TSyncObjsInfoList = TBaseCollectList; // TCollectList<RSyncObjsInfo>;

  PRPSyncObjsInfo = ^RPSyncObjsInfo;
  RPSyncObjsInfo = record
    SyncObjsInfo: PSyncObjsInfo;
  end;

  // Список SyncObjs, которые вызываются из текущей функции или вложенных
  TFuncSyncObjsInfoList = TCollectList<RPSyncObjsInfo>;

  PThreadData = ^TThreadData;

  TExceptInfo = class
  public
    ThreadID: TThreadId;
    Address: Pointer;
    Frame: Pointer;
    ExceptionName: String;
    Message: String;
    Stack: TList;

    constructor Create(DebugEvent: PDebugEvent); overload;
    constructor Create(ThreadData: PThreadData); overload;
    constructor Create(); overload;

    destructor Destroy; override;
  end;

  TSyncObjsInfo = class
    SyncObjsType: TDbgSyncObjsType;
    SyncObjsStateType: TDbgSyncObjsStateType;
    PerfIdx: Cardinal;
    Id: NativeUInt;
    Data: NativeUInt;
    Link: TSyncObjsInfo;

    constructor Create(const DebugEvent: PDebugEvent; const ThreadData: PThreadData; const APerfIdx: Cardinal);
    destructor Destroy; override;

    function FindLink(const ThreadData: PThreadData): TSyncObjsInfo;
  end;

  TDbgPointType = (ptNone = 0, ptWait, ptStart, ptStop, ptException, ptPerfomance, ptThreadInfo, ptMemoryInfo,
    ptSyncObjsInfo, ptTraceInfo);

  PThreadPoint = ^TThreadPoint;
  TThreadPoint = packed record
  public
    procedure Clear;
  public
    PerfIdx: Cardinal;
    case PointType: TDbgPointType of
      ptStart: ();
      ptStop: ();
      ptException: (
        ExceptInfo: TExceptInfo;
      );
      ptPerfomance: (
        DeltaTickCPU: UInt64;   // загрузка CPU
        DeltaTime: UInt64;
        //StackPoint: TStackPointList;
      );
      ptMemoryInfo: (
      );
      ptSyncObjsInfo: (
        SyncObjsInfo: TSyncObjsInfo;
      );
  end;

  PThreadAdvInfo = ^TThreadAdvInfo;
  TThreadAdvInfo = record
    ThreadId: TThreadId;
    ThreadParentId: TThreadId;
    ThreadData: PThreadData;
    ThreadClassName: String;
    ThreadName: String;

    function AsString: String;
  end;

  PCallFuncInfo = ^TCallFuncInfo;
  TCallFuncInfo = record
    FuncInfo: TObject;
    LineNo: Cardinal;
    CallCount: UInt64;
    Data: UInt64;
  public
    property Elapsed: UInt64 read Data write Data;
    property Size: UInt64 read Data write Data;
  end;

  TCallFuncCounter = class(TObjectDictionary<Pointer,PCallFuncInfo>)
  private
    function AddNewCallFunc(const Addr: Pointer): PCallFuncInfo;
  public
    function AddCallFunc(const Addr: Pointer): PCallFuncInfo;
    function GetCallFunc(const Addr: Pointer): PCallFuncInfo;
  end;
  TCallFuncCounterPair = TPair<Pointer,PCallFuncInfo>;

  TTrackUnitInfo = class;
  TTrackFuncInfo = class;

  TTrackUnitInfoBaseList = TObjectDictionary<TObject,TTrackUnitInfo>;
  TTrackFuncInfoBaseList = TObjectDictionary<TObject,TTrackFuncInfo>;

  TTrackUnitInfo = class
  private
    FUnitInfo: TObject;
    FFuncInfoList: TTrackFuncInfoBaseList;

    FCallCount: UInt64;
  public
    constructor Create(AUnitInfo: TObject);
    destructor Destroy; override;

    procedure IncCallCount; inline;

    property UnitInfo: TObject read FUnitInfo;

    property FuncInfoList: TTrackFuncInfoBaseList read FFuncInfoList;

    property CallCount: UInt64 read FCallCount;
  end;

  TCodeTrackUnitInfo = class(TTrackUnitInfo)
  private
    FElapsed: UInt64;
  public
    procedure GrowElapsed(const Value: UInt64); inline;

    // TODO: высчитать общее время выполнения функций юнита с учетом вложенности
    // property Elapsed: UInt64 read FElapsed;
  end;

  TMemInfoTrackUnitInfo = class(TTrackUnitInfo)
  private
    FCurCount: Int64;
    FSize: Int64;
  public
    procedure GrowSize(const Value: Int64); inline;

    procedure IncCurCount; inline;
    procedure DecCurCount; inline;

    property CurCount: Int64 read FCurCount;
  end;

  TSyncObjsTrackUnitInfo = class(TTrackUnitInfo);

  TTrackUnitInfoList = class(TTrackUnitInfoBaseList)
  protected
    function CreateTrackUnitInfo(const UnitInfo: TObject): TTrackUnitInfo; virtual;
  public
    function GetTrackUnitInfo(const UnitInfo: TObject): TTrackUnitInfo;
    procedure CheckTrackFuncInfo(TrackFuncInfo: TTrackFuncInfo);
  end;
  TTrackUnitInfoPair = TPair<TObject,TTrackUnitInfo>;

  TCodeTrackUnitInfoList = class(TTrackUnitInfoList)
  protected
    function CreateTrackUnitInfo(const UnitInfo: TObject): TTrackUnitInfo; override;
  public
  end;

  TMemInfoTrackUnitInfoList = class(TTrackUnitInfoList)
  protected
    function CreateTrackUnitInfo(const UnitInfo: TObject): TTrackUnitInfo; override;
  public
    procedure LoadStack(const GetMemInfo: TGetMemInfo);
  end;

  TSyncObjsTrackUnitInfoList = class(TTrackUnitInfoList)
  protected
    function CreateTrackUnitInfo(const UnitInfo: TObject): TTrackUnitInfo; override;
  public
    procedure LoadStack(const SyncObjsInfo: PSyncObjsInfo);
  end;

  TTrackFuncInfo = class
  private
    FFuncInfo: TObject;
    FTrackUnitInfo: TTrackUnitInfo;

    FCallCount: UInt64;

    FParentFuncs: TCallFuncCounter;
    FChildFuncs: TCallFuncCounter;
  public
    constructor Create(AFuncInfo: TObject);
    destructor Destroy; override;

    function AddParentCall(const Addr: Pointer): PCallFuncInfo; inline;
    function AddChildCall(const Addr: Pointer): PCallFuncInfo; inline;

    procedure IncCallCount; inline;

    property FuncInfo: TObject read FFuncInfo;

    property TrackUnitInfo: TTrackUnitInfo read FTrackUnitInfo write FTrackUnitInfo;
    property ParentFuncs: TCallFuncCounter read FParentFuncs;
    property ChildFuncs: TCallFuncCounter read FChildFuncs;

    property CallCount: UInt64 read FCallCount;
  end;

  TMemInfoTrackFuncInfo = class(TTrackFuncInfo)
  private
    FCurCount: Int64;
    FSize: Int64;
    FGetMemList: TGetMemInfoList;
  public
    constructor Create(AFuncInfo: TObject);
    destructor Destroy; override;

    procedure GrowSize(const Value: Int64); inline;

    procedure IncCurCount; inline;
    procedure DecCurCount; inline;

    procedure AddGetMemInfo(const GetMemInfo: TGetMemInfo);

    property CurCount: Int64 read FCurCount;
    property Size: Int64 read FSize;

    property GetMemList: TGetMemInfoList read FGetMemList;
  end;

  TCodeTrackFuncInfo = class(TTrackFuncInfo)
  private
    FCPUElapsed: UInt64;
  public
    procedure GrowElapsed(const Value: UInt64); inline;

    property CPUElapsed: UInt64 read FCPUElapsed;
  end;

  TSyncObjsTrackFuncInfo = class(TTrackFuncInfo)
  private
    FWaitTime: Int64;
    FSyncObjsList: TFuncSyncObjsInfoList;
  public
    constructor Create(AFuncInfo: TObject);
    destructor Destroy; override;

    procedure GrowWaitTime(const Value: Int64); inline;

    property WaitTime: Int64 read FWaitTime;
    property SyncObjsList: TFuncSyncObjsInfoList read FSyncObjsList;
  end;

  TTrackFuncInfoList = class(TTrackFuncInfoBaseList)
  protected
    function CreateTrackFuncInfo(const FuncInfo: TObject): TTrackFuncInfo; virtual;
  public
    function GetTrackFuncInfo(const FuncInfo: TObject): TTrackFuncInfo;
  end;
  TTrackFuncInfoPair = TPair<TObject,TTrackFuncInfo>;

  TMemInfoTrackFuncInfoList = class(TTrackFuncInfoList)
  protected
    function CreateTrackFuncInfo(const FuncInfo: TObject): TTrackFuncInfo; override;
  end;

  TCodeTrackFuncInfoList = class(TTrackFuncInfoList)
  protected
    function CreateTrackFuncInfo(const FuncInfo: TObject): TTrackFuncInfo; override;
  end;

  TSyncObjsTrackFuncInfoList = class(TTrackFuncInfoList)
  protected
    function CreateTrackFuncInfo(const FuncInfo: TObject): TTrackFuncInfo; override;
  end;

  PTrackStackPoint = ^TTrackStackPoint;
  TTrackStackPoint = record
  private
    function GetLeave: UInt64;
    procedure SetLeave(const Value: UInt64);
  public
    TrackFuncInfo: TCodeTrackFuncInfo;
    ParentTrackFuncInfo: TCodeTrackFuncInfo;

    ProcTrackFuncInfo: TCodeTrackFuncInfo;
    ProcParentTrackFuncInfo: TCodeTrackFuncInfo;

    TrackRETBreakpoint: PTrackRETBreakpoint;

    Enter: UInt64;
    Elapsed: UInt64;

    property Leave: UInt64 read GetLeave write SetLeave;
  end;

  TTrackStack = class(TStack);

  TThreadAdvInfoList = TBaseCollectList; //TCollectList<TThreadAdvInfo>;

  TThreadPointList = TBaseCollectList; //TCollectList<TThreadPoint>;

  TThreadMemInfoList = TBaseCollectList; //TCollectList<TMemInfo>;

  TThreadState = (tsNone, tsActive, tsFinished, tsSuspended, tsLocked);

  TThreadData = packed record
    ThreadID: TThreadId;
    State: TThreadState;
    ThreadHandle: THandle;
    ThreadAdvInfo: PThreadAdvInfo;
    Context: PContext; // Указатель должен быть выровнен по памяти 32 бит
    Breakpoint: PHardwareBreakpoint;
    Started: Int64;         // момент запуска
    Elapsed: Int64;         // время выполнения

    CPUElapsed: UInt64;     // циклы CPU
    CPUTime: UInt64;        // время использования CPU

    WaitTime: Int64;        // Время блокировок

    DbgPoints: TThreadPointList;

    DbgGetMemInfo: TGetMemInfoList;
    DbgGetMemInfoSize: Cardinal;

    DbgSyncObjsInfo: TSyncObjsInfoList;

    DbgGetMemUnitList: TMemInfoTrackUnitInfoList;
    DbgSyncObjsUnitList: TSyncObjsTrackUnitInfoList;
    //DbgGetMemFuncList: TTrackFuncInfoList;

    DbgExceptions: TThreadList;

    DbgTrackEventCount: UInt64;
    DbgTrackUnitList: TCodeTrackUnitInfoList;
    DbgTrackFuncList: TCodeTrackFuncInfoList;
    DbgTrackStack: TTrackStack;

    function DbgPointsCount: Cardinal;
    function DbgPointByIdx(const Idx: Cardinal): PThreadPoint;

    function DbgExceptionsCount: Cardinal;
    function DbgExceptionsByIdx(const Idx: Cardinal): TExceptInfo;

    function DbgSyncObjsCount: Cardinal;
    function DbgSyncObjsByIdx(const Idx: Cardinal): PSyncObjsInfo;

    procedure UpdateGetMemUnitList;

    procedure Clear;
  end;

  TDbgThreadList = TBaseCollectList; //TCollectList<TThreadData>;

  PProcessPoint = ^TProcessPoint;
  TProcessPoint = packed record
    FromStart: Int64;        // кол-во тиков со старта
    CPUTime: UInt64;         // текущее время CPU
    case PointType: TDbgPointType of
      ptStart: ();
      ptStop: ();
      ptException: ();
      ptPerfomance: (
        DeltaTick: Int64;
        DeltaTickCPU: UInt64;
        DeltaTime: UInt64;
      );
      ptMemoryInfo: ();
  end;

  TProcessPointList = TBaseCollectList; //TCollectList<TProcessPoint>;

  // структура хранящая данные о отлаживаемом процессе.
  // используется отладчиком, доступна пользователю через свойство DebugProcessData
  // Внимание !!!
  // Данная информация может использоваться пользователем только на чтение,
  // не закрывайте хэндлы описанные в данной структуре через CloseHandle()

  TProcessState = (psNone, psActive, psFinished);

  PProcessData = ^TProcessData;
  TProcessData = record
    ProcessID: TProcessId;
    State: TProcessState;
    StartAddress: Pointer;
    BaseOfImage: Pointer;
    MainThreadID: TThreadId;
    PEImage: TJclPeImage;

    Started: Int64;
    Elapsed: Int64;
    CPUTime: UInt64;
    CPUElapsed: UInt64; // время использования CPU

    DbgPoints: TProcessPointList;

    DbgGetMemInfo: TGetMemInfoList; // Указатели с коллизиями
    DbgGetMemInfoSize: Cardinal;

    ProcessGetMemCount: Cardinal;
    ProcessGetMemSize: Cardinal;

    DbgExceptions: TThreadList;

    DbgTrackEventCount: UInt64;
    DbgTrackUnitList: TCodeTrackUnitInfoList;
    DbgTrackFuncList: TCodeTrackFuncInfoList;

    CreatedProcessHandle: THandle;
    CreatedThreadHandle: THandle;
    AttachedProcessHandle: THandle;
    AttachedThreadHandle: THandle;
    AttachedFileHandle: THandle;

    //DbgShareMem: THandle; // FileMap для взаимодействия с процессом

    function Elapsed_MSec: Cardinal; // msec
    function DbgPointsCount: Cardinal;
    function DbgPointByIdx(const Idx: Cardinal): PProcessPoint;
    function CurDbgPointIdx: Cardinal;

    function DbgExceptionsCount: Cardinal;

    procedure Clear;

    procedure SetPEImage(APEImage: TJclPeImage);
  end;

  THardwareBreakpointEvent = procedure(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord;
    BreakPointIndex: THWBPIndex; var ReleaseBreakpoint: Boolean) of object;

  TDbgState = (dsNone, dsStarted, dsWait, dsPerfomance, dsTrace, dsEvent, dsPause, dsStoping, dsStoped, dsDbgFail);

  TDbgTraceState = (dtsContinue, dtsPause, dtsStepIn, dtsStepOver, dtsStepOut);

  TDbgLogType = (dltInfo, dltWarning, dltError, dltDebugOutput, dltProcessEvent, dltThreadEvent, dltExceptionEvent,
    dltBreakPointEvent, dltDLLEvent);

  TDbgLogItem = Class
    LogType: TDbgLogType;
    DateTime: TDateTime;
    LogMessage: String;
  End;

  TDbgLog = class(TList)
  private
    FLock: TMREWSync;
    function GetItem(const Index: Integer): TDbgLogItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ClearLog;

    procedure Add(const LogType: TDbgLogType; const Msg: String); overload;
    procedure Add(const LogType: TDbgLogType; const FmtMsg: String; const Args: array of Const); overload;

    property Items[const Index: Integer]: TDbgLogItem read GetItem; default;
    property Lock: TMREWSync read FLock;
  end;

procedure RaiseDebugCoreException(const Msg: String = '');

implementation

uses Debuger, DebugInfo, WinAPIUtils;

procedure RaiseDebugCoreException(const Msg: String);
begin
  raise EDebugCoreException.Create(Msg);
end;


{ TDebugProcessData }

procedure TProcessData.Clear;
begin
  if DbgPoints <> Nil then
    FreeAndNil(DbgPoints);

  FreeAndNil(DbgGetMemInfo);
  DbgExceptions.Clear;

  FreeAndNil(DbgTrackUnitList);
  FreeAndNil(DbgTrackFuncList);
end;

function TProcessData.CurDbgPointIdx: Cardinal;
begin
  Result := 0;
  if Assigned(DbgPoints) and (DbgPoints.Count > 0) then
    Result := DbgPoints.Count - 1
  else
    RaiseDebugCoreException();
end;

function TProcessData.DbgExceptionsCount: Cardinal;
var
  L: TList;
begin
  L := DbgExceptions.LockList;
  try
    Result := Cardinal(L.Count);
  finally
    DbgExceptions.UnlockList;
  end;
end;

function TProcessData.DbgPointByIdx(const Idx: Cardinal): PProcessPoint;
begin
  Result := DbgPoints[Idx];
end;

function TProcessData.DbgPointsCount: Cardinal;
begin
  if Assigned(DbgPoints) then
    Result := DbgPoints.Count
  else
    Result := 0;
end;

function TProcessData.Elapsed_MSec: Cardinal;
var
  Cur: Int64;
  Freq: Int64;
begin
  if State = psActive then
    Cur := _QueryPerformanceCounter
  else
    Cur := Elapsed;

  Freq := _QueryPerformanceFrequency;
  Result := ((Cur - Started) * 1000) div Freq;
end;

procedure TProcessData.SetPEImage(APEImage: TJclPeImage);
begin
  PEImage := APEImage;
end;

{ TThreadData }

procedure TThreadData.Clear;
var
  I: Integer;
begin
  if Breakpoint <> Nil then
  begin
    FreeMemory(Breakpoint);
    Breakpoint := Nil;
  end;

  if DbgPoints <> Nil then
  begin
    for I := 0 to DbgPointsCount - 1 do
      DbgPointByIdx(I).Clear;

    FreeAndNil(DbgPoints);
  end;

  FreeAndNil(DbgGetMemInfo);
  FreeAndNil(DbgGetMemUnitList);
  FreeAndNil(DbgSyncObjsUnitList);
  //FreeAndNil(DbgGetMemFuncList);
  FreeAndNil(DbgSyncObjsInfo);
  FreeAndNil(DbgExceptions);
  FreeAndNil(DbgTrackUnitList);
  FreeAndNil(DbgTrackFuncList);
  FreeAndNil(DbgTrackStack);

  FreeMemory(Context);

  ThreadAdvInfo := Nil;
end;

function TThreadData.DbgExceptionsByIdx(const Idx: Cardinal): TExceptInfo;
var
  L: TList;
begin
  Result := nil;

  L := DbgExceptions.LockList;
  try
    if (L.Count > 0) and (Idx < Cardinal(L.Count)) then
      Result := TExceptInfo(L[Idx]);
  finally
    DbgExceptions.UnlockList;
  end;
end;

function TThreadData.DbgExceptionsCount: Cardinal;
var
  L: TList;
begin
  L := DbgExceptions.LockList;
  try
    Result := Cardinal(L.Count);
  finally
    DbgExceptions.UnlockList;
  end;
end;

function TThreadData.DbgPointByIdx(const Idx: Cardinal): PThreadPoint;
begin
  if Idx < DbgPoints.Count then
    Result := DbgPoints[Idx]
  else
    Result := Nil;
end;

function TThreadData.DbgPointsCount: Cardinal;
begin
  Result := DbgPoints.Count;
end;

function TThreadData.DbgSyncObjsByIdx(const Idx: Cardinal): PSyncObjsInfo;
begin
  if Idx < DbgSyncObjsInfo.Count then
    Result := DbgSyncObjsInfo[Idx]
  else
    Result := Nil;
end;

function TThreadData.DbgSyncObjsCount: Cardinal;
begin
  Result := DbgSyncObjsInfo.Count;
end;

procedure TThreadData.UpdateGetMemUnitList;
var
  GetMemInfoItem: TGetMemInfoItem;
begin
  DbgGetMemUnitList.Clear;

  DbgGetMemInfo.LockForRead;
  try
    for GetMemInfoItem in DbgGetMemInfo do
      DbgGetMemUnitList.LoadStack(GetMemInfoItem.Value);
  finally
    DbgGetMemInfo.UnLockForRead;
  end;
end;

{ TThreadPoint }

procedure TThreadPoint.Clear;
begin
  case PointType of
    ptException:
      FreeAndNil(ExceptInfo);
    ptSyncObjsInfo:
      FreeAndNil(SyncObjsInfo);
  end;
end;

{ TThreadAdvInfo }

function TThreadAdvInfo.AsString: String;
begin
  if ThreadName <> '' then
    Result := ThreadName
  else
    if ThreadClassName <> '' then
      Result := ThreadClassName
    else
      Result := 'unknown';
end;

{ RGetMemInfo }

procedure TGetMemInfo.CheckObjectType;
begin
  if ObjectType = '' then
  begin
    if gvDebuger.Active then
      ObjectType := gvDebugInfo.GetClassName(ObjAddr);
  end;
end;

function TGetMemInfo.GetObjectType: String;
begin
  CheckObjectType;

  Result := String(ObjectType);
end;

procedure TGetMemInfo.LoadStack(const DbgStack: PDbgHookInfoStack);
var
  I: Integer;
  Ptr: Pointer;
begin
  I := 0;
  while I < Length(DbgStack^) do
  begin
    Ptr := DbgStack^[I];

    if (Ptr = nil) or (Ptr = Pointer(-1)) then
    begin
      SetLength(Stack, I);
      Move(DbgStack^[0], Stack[0], I * SizeOf(Pointer));

      Exit;
    end;

    Inc(I);
  end;
end;

{ TExceptInfo }

constructor TExceptInfo.Create(DebugEvent: PDebugEvent);
var
  ER: PExceptionRecord;
begin
  Create;

  if Assigned(DebugEvent) then
  begin
    ThreadID := DebugEvent^.dwThreadId;

    ER := @DebugEvent^.Exception.ExceptionRecord;
    Address := gvDebugInfo.GetExceptionAddress(ER);
    Frame := gvDebugInfo.GetExceptionFrame(ER);
    ExceptionName := gvDebugInfo.GetExceptionName(ER);
    Message := gvDebugInfo.GetExceptionMessage(ER, ThreadID);

    gvDebugInfo.GetCallStackItems(ThreadID, Address, Frame, Stack);
  end;
end;

constructor TExceptInfo.Create(ThreadData: PThreadData);
begin
  Create;

  if Assigned(ThreadData) then
  begin
    gvDebuger.UpdateThreadContext(ThreadData);
    ThreadID := ThreadData^.ThreadID;
    Address := Pointer(ThreadData^.Context^.Eip);
    Frame := Pointer(ThreadData^.Context^.Ebp);

    gvDebugInfo.GetCallStackItems(ThreadID, Address, Frame, Stack);
  end;
end;

constructor TExceptInfo.Create;
begin
  inherited Create;

  Stack := TObjectList.Create;
end;

destructor TExceptInfo.Destroy;
begin
  FreeAndNil(Stack);

  inherited;
end;

{ TDbgLog }

procedure TDbgLog.Add(const LogType: TDbgLogType; const Msg: String);
var
  LogItem: TDbgLogItem;
begin
  LogItem := TDbgLogItem.Create;
  LogItem.LogType := LogType;
  LogItem.DateTime := Now;
  LogItem.LogMessage := Msg;

  FLock.BeginWrite;
  try
    inherited Add(LogItem);
  finally
    FLock.EndWrite;
  end;
end;

procedure TDbgLog.Add(const LogType: TDbgLogType; const FmtMsg: String; const Args: array of Const);
begin
  Add(LogType, Format(FmtMsg, Args));
end;

procedure TDbgLog.ClearLog;
begin
  FLock.BeginWrite;
  try
    ClearList(Self);
  finally
    FLock.EndWrite;
  end;
end;

constructor TDbgLog.Create;
begin
  inherited Create;

  Capacity := 1000;
  FLock := TMREWSync.Create;
end;

destructor TDbgLog.Destroy;
begin
  ClearLog;

  FreeAndNil(FLock);

  inherited;
end;

function TDbgLog.GetItem(const Index: Integer): TDbgLogItem;
begin
  FLock.BeginRead;
  try
    Result := TDbgLogItem(List[Index]);
  finally
    FLock.EndRead;
  end;
end;

{ TTrackFuncInfo }

function TTrackFuncInfo.AddChildCall(const Addr: Pointer): PCallFuncInfo;
begin
  Result := FChildFuncs.AddCallFunc(Addr);
end;

function TTrackFuncInfo.AddParentCall(const Addr: Pointer): PCallFuncInfo;
begin
  Result := FParentFuncs.AddCallFunc(Addr)
end;

constructor TTrackFuncInfo.Create(AFuncInfo: TObject);
begin
  inherited Create;

  FFuncInfo := AFuncInfo;
  FTrackUnitInfo := nil;
  FParentFuncs := TCallFuncCounter.Create(256);
  FParentFuncs.OwnsValues := True;
  FChildFuncs := TCallFuncCounter.Create(256);
  FChildFuncs.OwnsValues := True;
end;

destructor TTrackFuncInfo.Destroy;
begin
  FreeAndNil(FParentFuncs);
  FreeAndNil(FChildFuncs);

  inherited;
end;

procedure TTrackFuncInfo.IncCallCount;
begin
  Inc(FCallCount);
  FTrackUnitInfo.IncCallCount;
end;

{ TCodeTrackFuncInfo }

procedure TCodeTrackFuncInfo.GrowElapsed(const Value: UInt64);
begin
  Inc(FCPUElapsed, Value);
  TCodeTrackUnitInfo(FTrackUnitInfo).GrowElapsed(Value);
end;

{ TMemInfoTrackFuncInfo }

procedure TMemInfoTrackFuncInfo.AddGetMemInfo(const GetMemInfo: TGetMemInfo);
begin
  if FGetMemList = Nil then
  begin
    FGetMemList := TGetMemInfoList.Create(256, True);
    FGetMemList.OwnsValues := False;
  end;

  FGetMemList.AddOrSetValue(GetMemInfo.ObjAddr, GetMemInfo);
end;

destructor TMemInfoTrackFuncInfo.Destroy;
begin
  FreeAndNil(FGetMemList);

  inherited;
end;

constructor TMemInfoTrackFuncInfo.Create(AFuncInfo: TObject);
begin
  inherited;

  FGetMemList := Nil;
end;

procedure TMemInfoTrackFuncInfo.DecCurCount;
begin
  Dec(FCurCount);
  TMemInfoTrackUnitInfo(FTrackUnitInfo).DecCurCount;
end;

procedure TMemInfoTrackFuncInfo.GrowSize(const Value: Int64);
begin
  Inc(FSize, Value);
  TMemInfoTrackUnitInfo(FTrackUnitInfo).GrowSize(Value);
end;

procedure TMemInfoTrackFuncInfo.IncCurCount;
begin
  Inc(FCurCount);
  TMemInfoTrackUnitInfo(FTrackUnitInfo).IncCurCount;
end;

{ TTrackFuncInfoList }

function TTrackFuncInfoList.CreateTrackFuncInfo(const FuncInfo: TObject): TTrackFuncInfo;
begin
  Result := TTrackFuncInfo.Create(FuncInfo);
end;

function TTrackFuncInfoList.GetTrackFuncInfo(const FuncInfo: TObject): TTrackFuncInfo;
begin
  Assert(Assigned(FuncInfo));

  if not TryGetValue(FuncInfo, Result) then
  begin
    Result := CreateTrackFuncInfo(FuncInfo);
    Add(FuncInfo, Result);
  end;
end;

{ TCallCounter }

function TCallFuncCounter.AddCallFunc(const Addr: Pointer): PCallFuncInfo;
begin
  Result := Nil;

  if (Addr = Nil) or (Addr = Pointer(-1)) then
    Exit;

  if TryGetValue(Addr, Result) then
    Inc(Result^.CallCount)
  else
  begin
    Result := AddNewCallFunc(Addr);
    Result^.CallCount := 1;

    Add(Addr, Result);
  end;
end;

function TCallFuncCounter.AddNewCallFunc(const Addr: Pointer): PCallFuncInfo;
var
  UnitInfo: TUnitInfo;
  FuncInfo: TFuncInfo;
  LineInfo: TLineInfo;
begin
  Result := AllocMem(SizeOf(TCallFuncInfo));

  FuncInfo := nil;
  LineInfo := nil;
  if gvDebugInfo.GetLineInfo(Addr, UnitInfo, FuncInfo, LineInfo, False) <> slNotFound then
  begin
    Result^.FuncInfo := FuncInfo;
    if Assigned(LineInfo) then
      Result^.LineNo := LineInfo.LineNo;
  end;
end;

function TCallFuncCounter.GetCallFunc(const Addr: Pointer): PCallFuncInfo;
begin
  if not TryGetValue(Addr, Result) then
    Result := Nil;
end;

{ TTrackUnitInfoList }

procedure TTrackUnitInfoList.CheckTrackFuncInfo(TrackFuncInfo: TTrackFuncInfo);
var
  FuncInfo: TFuncInfo;
begin
  FuncInfo := TFuncInfo(TrackFuncInfo.FuncInfo);

  if Assigned(FuncInfo) then
  begin
    if TrackFuncInfo.TrackUnitInfo = nil then
      TrackFuncInfo.TrackUnitInfo := GetTrackUnitInfo(FuncInfo.UnitInfo);

    TrackFuncInfo.TrackUnitInfo.FuncInfoList.AddOrSetValue(FuncInfo, TrackFuncInfo);
  end;
end;

function TTrackUnitInfoList.CreateTrackUnitInfo(const UnitInfo: TObject): TTrackUnitInfo;
begin
  Result := TTrackUnitInfo.Create(UnitInfo);
end;

function TTrackUnitInfoList.GetTrackUnitInfo(const UnitInfo: TObject): TTrackUnitInfo;
begin
  if not TryGetValue(UnitInfo, Result) then
  begin
    Result := CreateTrackUnitInfo(UnitInfo);

    Add(UnitInfo, Result);
  end;
end;

{ TMemInfoTrackUnitInfoList }

function TMemInfoTrackUnitInfoList.CreateTrackUnitInfo(const UnitInfo: TObject): TTrackUnitInfo;
begin
  Result := TMemInfoTrackUnitInfo.Create(UnitInfo);
end;

procedure TMemInfoTrackUnitInfoList.LoadStack(const GetMemInfo: TGetMemInfo);
var
  StackEntry: TStackEntry;
  I: Integer;
  Addr: Pointer;
  TrackUnitInfo: TMemInfoTrackUnitInfo;
  TrackFuncInfo: TTrackFuncInfo;
  CallFuncInfo: PCallFuncInfo;
begin
  StackEntry := TStackEntry.Create;
  try
    for I := 0 to High(GetMemInfo.Stack) do
    begin
      Addr := GetMemInfo.Stack[I];

      if (Addr = nil) or (Addr = Pointer(-1)) then Break;

      if StackEntry.UpdateInfo(Addr) <> slNotFound then
      begin
        TrackUnitInfo := TMemInfoTrackUnitInfo(GetTrackUnitInfo(StackEntry.UnitInfo));

        if not TrackUnitInfo.FuncInfoList.TryGetValue(StackEntry.FuncInfo, TrackFuncInfo) then
        begin
          TrackFuncInfo := TMemInfoTrackFuncInfo.Create(StackEntry.FuncInfo);
          TrackFuncInfo.TrackUnitInfo := TrackUnitInfo;

          TrackUnitInfo.FuncInfoList.AddOrSetValue(StackEntry.FuncInfo, TrackFuncInfo);
        end;

        TMemInfoTrackFuncInfo(TrackFuncInfo).IncCallCount;
        TMemInfoTrackFuncInfo(TrackFuncInfo).IncCurCount;
        TMemInfoTrackFuncInfo(TrackFuncInfo).GrowSize(GetMemInfo.Size);
        TMemInfoTrackFuncInfo(TrackFuncInfo).AddGetMemInfo(GetMemInfo);

        // TODO: Искать функцию с валидным Addr
        if I > 0 then
        begin
          Addr := GetMemInfo.Stack[I - 1];
          CallFuncInfo := TrackFuncInfo.AddChildCall(Addr);
          if Assigned(CallFuncInfo) then
            Inc(CallFuncInfo^.Data, GetMemInfo.Size);
        end;

        // TODO: Искать функцию с валидным Addr
        if I < High(GetMemInfo.Stack) then
        begin
          Addr := GetMemInfo.Stack[I + 1];
          CallFuncInfo := TrackFuncInfo.AddParentCall(Addr);
          if Assigned(CallFuncInfo) then
            Inc(CallFuncInfo^.Data, GetMemInfo.Size);
        end;
      end;
    end;
  finally
    FreeAndNil(StackEntry);
  end;
end;

{ TTrackUnitInfo }

constructor TTrackUnitInfo.Create(AUnitInfo: TObject);
begin
  inherited Create;

  FFuncInfoList := TTrackFuncInfoBaseList.Create(128);

  FUnitInfo := AUnitInfo;
  FCallCount := 0;
end;

destructor TTrackUnitInfo.Destroy;
begin
  FUnitInfo := nil;
  FreeAndNil(FFuncInfoList);

  inherited;
end;

procedure TTrackUnitInfo.IncCallCount;
begin
  Inc(FCallCount);
end;

procedure TCodeTrackUnitInfo.GrowElapsed(const Value: UInt64);
begin
  Inc(FElapsed, Value);
end;

{ TTrackStackPoint }

function TTrackStackPoint.GetLeave: UInt64;
begin
  Result := Enter + Elapsed;
end;

procedure TTrackStackPoint.SetLeave(const Value: UInt64);
begin
  Elapsed := (Value - Enter) + 1;
end;

{ TSyncObjsInfo }

constructor TSyncObjsInfo.Create(const DebugEvent: PDebugEvent; const ThreadData: PThreadData; const APerfIdx: Cardinal);
var
  ER: PExceptionRecord;
begin
  inherited Create;

  PerfIdx := APerfIdx;

  ER := @DebugEvent^.Exception.ExceptionRecord;
  SyncObjsType := TDbgSyncObjsType(ER^.ExceptionInformation[1]);
  SyncObjsStateType := TDbgSyncObjsStateType(ER^.ExceptionInformation[2]);
  Id := ER^.ExceptionInformation[3];
  Data := ER^.ExceptionInformation[4];
  Link := FindLink(ThreadData);
  if Assigned(Link) then
    Link.Link := Self;
end;

destructor TSyncObjsInfo.Destroy;
begin

  inherited;
end;

function TSyncObjsInfo.FindLink(const ThreadData: PThreadData): TSyncObjsInfo;
var
  I: Integer;
  ThPoint: PThreadPoint;
begin
  Result := nil;

  if SyncObjsStateType = sosLeave then
  begin
    ThreadData^.DbgPoints.BeginRead;
    try
      I := ThreadData^.DbgPoints.Count - 2;
      while I >= 0 do
      begin
        ThPoint := ThreadData^.DbgPoints[I];

        if (ThPoint^.PointType = ptSyncObjsInfo) and (ThPoint^.SyncObjsInfo.Id = Id) then
        begin
          Result := ThPoint^.SyncObjsInfo;
          Exit;
        end;

        Dec(I);
      end;
    finally
      ThreadData^.DbgPoints.EndRead;
    end;
  end;
end;

{ TMemInfoTrackUnitInfo }

procedure TMemInfoTrackUnitInfo.DecCurCount;
begin
  Dec(FCurCount);
end;

procedure TMemInfoTrackUnitInfo.GrowSize(const Value: Int64);
begin
  Inc(FSize, Value);
end;

procedure TMemInfoTrackUnitInfo.IncCurCount;
begin
  Inc(FCurCount);
end;

{ TMemInfoTrackFuncInfoList }

function TMemInfoTrackFuncInfoList.CreateTrackFuncInfo(const FuncInfo: TObject): TTrackFuncInfo;
begin
  Result := TMemInfoTrackFuncInfo.Create(FuncInfo);
end;

{ TCodeTrackFuncInfoList }

function TCodeTrackFuncInfoList.CreateTrackFuncInfo(const FuncInfo: TObject): TTrackFuncInfo;
begin
  Result := TCodeTrackFuncInfo.Create(FuncInfo);
end;

function TCodeTrackUnitInfoList.CreateTrackUnitInfo(const UnitInfo: TObject): TTrackUnitInfo;
begin
  Result := TCodeTrackUnitInfo.Create(UnitInfo);
end;

{ TSyncObjsTrackUnitInfoList }

function TSyncObjsTrackUnitInfoList.CreateTrackUnitInfo(const UnitInfo: TObject): TTrackUnitInfo;
begin
  Result := TSyncObjsTrackUnitInfo.Create(UnitInfo);
end;

procedure TSyncObjsTrackUnitInfoList.LoadStack(const SyncObjsInfo: PSyncObjsInfo);
var
  StackEntry: TStackEntry;
  I: Integer;
  Addr: Pointer;
  TrackUnitInfo: TSyncObjsTrackUnitInfo;
  TrackFuncInfo: TTrackFuncInfo;
  CallFuncInfo: PCallFuncInfo;
  DbgSyncObjsInfo: PDbgSyncObjsInfoEx;
  WaitTime: Int64;
  ThData: PThreadData;
  SyncObjsListItem: PRPSyncObjsInfo;
begin
  if SyncObjsInfo = Nil then Exit;

  DbgSyncObjsInfo := @SyncObjsInfo^.SyncObjsInfo;

  // Если нет стека для выхода из SyncObj, то берем стек для входа
  if (DbgSyncObjsInfo^.SyncObjsStateType = sosLeave) and (Length(DbgSyncObjsInfo^.Stack) = 0) then
    if Assigned(SyncObjsInfo^.Link) then
      DbgSyncObjsInfo := @SyncObjsInfo^.Link^.SyncObjsInfo;

  // TODO: Добавить опцию дебагера
  // Проверка на игноры
  case DbgSyncObjsInfo^.SyncObjsType of
    soEnterCriticalSection:
      begin
        // Не логируем критические секции без блокировки
        if DbgSyncObjsInfo^.OwningThreadId = 0 then
          Exit;

        // Игнорим рекурсивную блокировку в своем потоке
        if (DbgSyncObjsInfo^.ThreadId = DbgSyncObjsInfo^.OwningThreadId) then
          Exit;
      end;
    soSendMessage:
      begin
        // Игнорим SendMessage в главном потоке
        if DbgSyncObjsInfo^.ThreadId = gvDebuger.ProcessData^.MainThreadID then
          Exit;
      end;
  end;

  StackEntry := TStackEntry.Create;
  try
    // Проход по стеку
    for I := 0 to High(DbgSyncObjsInfo^.Stack) do
    begin
      Addr := DbgSyncObjsInfo^.Stack[I];

      //if (Addr = nil) or (Addr = Pointer(-1)) then Break;

      if StackEntry.UpdateInfo(Addr) <> slNotFound then
      begin
        TrackUnitInfo := TSyncObjsTrackUnitInfo(GetTrackUnitInfo(StackEntry.UnitInfo));

        // TrackFuncInfo
        if not TrackUnitInfo.FuncInfoList.TryGetValue(StackEntry.FuncInfo, TrackFuncInfo) then
        begin
          TrackFuncInfo := TSyncObjsTrackFuncInfo.Create(StackEntry.FuncInfo);
          TrackFuncInfo.TrackUnitInfo := TrackUnitInfo;

          TrackUnitInfo.FuncInfoList.AddOrSetValue(StackEntry.FuncInfo, TrackFuncInfo);
        end;

        case SyncObjsInfo^.SyncObjsInfo.SyncObjsStateType of
          sosEnter:
            begin
              // Регистрируем вход в SyncObj
              TSyncObjsTrackFuncInfo(TrackFuncInfo).IncCallCount;

              // Добавляем SyncObj в список текущей функции
              SyncObjsListItem := TSyncObjsTrackFuncInfo(TrackFuncInfo).SyncObjsList.Add;
              SyncObjsListItem^.SyncObjsInfo := SyncObjsInfo;

              // TODO: Искать функцию с валидным Addr
              if I > 0 then
              begin
                Addr := DbgSyncObjsInfo^.Stack[I - 1];
                TrackFuncInfo.AddChildCall(Addr);
              end;

              // TODO: Искать функцию с валидным Addr
              if I < High(DbgSyncObjsInfo^.Stack) then
              begin
                Addr := DbgSyncObjsInfo^.Stack[I + 1];
                TrackFuncInfo.AddParentCall(Addr);
              end;
            end;
          sosLeave:
            begin
              // Регистрируем выход из SyncObj
              if Assigned(SyncObjsInfo^.Link) then
              begin
                WaitTime := SyncObjsInfo^.WaitTime;
                TSyncObjsTrackFuncInfo(TrackFuncInfo).GrowWaitTime(WaitTime);

                // Увеличиваем счетчик потока
                if I = 0 then
                begin
                  ThData := gvDebuger.GetThreadData(SyncObjsInfo^.SyncObjsInfo.ThreadId, True);
                  TInterlocked.Add(ThData^.WaitTime, WaitTime);
                end;

                // TODO: Искать функцию с валидным Addr
                if I > 0 then
                begin
                  Addr := DbgSyncObjsInfo^.Stack[I - 1];
                  CallFuncInfo := TrackFuncInfo.ChildFuncs.GetCallFunc(Addr);
                  if Assigned(CallFuncInfo) then
                    Inc(CallFuncInfo^.Data, WaitTime);
                end;

                // TODO: Искать функцию с валидным Addr
                if I < High(DbgSyncObjsInfo^.Stack) then
                begin
                  Addr := DbgSyncObjsInfo^.Stack[I + 1];
                  CallFuncInfo := TrackFuncInfo.ParentFuncs.GetCallFunc(Addr);
                  if Assigned(CallFuncInfo) then
                    Inc(CallFuncInfo^.Data, WaitTime);
                end;
              end;
            end;
        end;
      end;
    end;
  finally
    FreeAndNil(StackEntry);
  end;
end;

{ TSyncObjsTrackFuncInfoList }

function TSyncObjsTrackFuncInfoList.CreateTrackFuncInfo(const FuncInfo: TObject): TTrackFuncInfo;
begin
  Result := TSyncObjsTrackFuncInfo.Create(FuncInfo);
end;

{ TSyncObjsTrackFuncInfo }

constructor TSyncObjsTrackFuncInfo.Create(AFuncInfo: TObject);
begin
  inherited Create(AFuncInfo);

  FSyncObjsList := TFuncSyncObjsInfoList.Create(32 * SizeOf(Pointer));
end;

destructor TSyncObjsTrackFuncInfo.Destroy;
begin
  FreeAndNil(FSyncObjsList);

  inherited;
end;

procedure TSyncObjsTrackFuncInfo.GrowWaitTime(const Value: Int64);
begin
  Inc(FWaitTime, Value);
end;

{ RSyncObjsInfo }

function RSyncObjsInfo.WaitTime: Int64;
begin
  Result := 0;

  if Assigned(Link) then
    Result := Abs(SyncObjsInfo.CurTime - Link^.SyncObjsInfo.CurTime);
end;

{ TDbgSyncObjsInfoEx }

procedure TDbgSyncObjsInfoEx.Init(const SyncObjsInfo: PDbgSyncObjsInfo);
begin
  Id := SyncObjsInfo^.Id;
  ThreadId := SyncObjsInfo^.ThreadId;
  CurTime := SyncObjsInfo^.CurTime;
  SyncObjsStateType := SyncObjsInfo^.SyncObjsStateType;
  SyncObjsType := SyncObjsInfo^.SyncObjsType;
  case SyncObjsType of
    soUnknown: ;
    soSleep:
      MSec := SyncObjsInfo^.MSec;
    soWaitForSingleObject:
      Handle := SyncObjsInfo^.Handle;
    soWaitForMultipleObjects:
      Handles := SyncObjsInfo^.Handles;
    soEnterCriticalSection,
    soLeaveCriticalSection,
    soInCriticalSection:
      begin
        CS := SyncObjsInfo^.CS;
        OwningThreadId := SyncObjsInfo^.OwningThreadId;
      end;
    soSendMessage: ;
  end;

  LoadStack(@SyncObjsInfo^.Stack);
end;

procedure TDbgSyncObjsInfoEx.LoadStack(const DbgStack: PDbgHookInfoStack);
var
  I: Integer;
  Ptr: Pointer;
begin
  I := 0;
  while I < Length(DbgStack^) do
  begin
    Ptr := DbgStack^[I];

    if (Ptr = nil) or (Ptr = Pointer(-1)) then
    begin
      SetLength(Stack, I);
      Move(DbgStack^[0], Stack[0], I * SizeOf(Pointer));

      Exit;
    end;

    Inc(I);
  end;
end;

end.
