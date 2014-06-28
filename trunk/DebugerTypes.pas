unit DebugerTypes;

interface

uses System.SysUtils, WinApi.Windows, System.Classes, JclPeImage, System.SyncObjs, ClassUtils, DbgHookTypes, System.Contnrs,
  System.Generics.Collections, Collections.Dictionaries, Collections.Queues, CollectList,
  uSharedObject;

type
  TSysUInt = NativeUInt;
  PReal48 = ^Real48;
  PReal = ^Real;


const
  EXCEPTION_SET_THREAD_NAME = $406D1388;

const
  _SERVICE_THREAD_PREFIX = '###';

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
    BreakPointIndex: Integer; var ReleaseBreakpoint: LongBool) of object;

  // Список типов отладочных исключений (для внутренней работы отладчика,
  // пользователь с ними не работает)

  TExceptionCode = (ecUnknown, ecBreakpoint, ecSingleStep, ecCtrlC, ecNonContinuable, ecGuard, ecSetThreadName);
  TExceptionEvents = array[TExceptionCode] of TDefaultExceptionEvent;

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
    BreakOnWrite: LongBool;
    RegionStart: Pointer;
    RegionSize: Cardinal;
    PreviosRegionProtect: Cardinal;
  end;

  PBreakpoint = ^TBreakpoint;
  TBreakpoint = packed record
    bpType: TBreakpointType;
    ThreadId: TThreadId;
    Description: ShortString;
    Active: LongBool;
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
    Active: array [THWBPIndex] of LongBool;
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

  PDbgInfoStackRec = ^TDbgInfoStackRec;
  TDbgInfoStackRec = record
    Stack: TDbgInfoStack;
  end;

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
  TGetMemInfo = class(TSharedObject)
  public
    PerfIdx: Integer;
    ObjAddr: Pointer;
    Size: Integer;
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
    ThreadId: TThreadId;
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
        OwningThreadId: TThreadId;
      );
      soSendMessage:
      ();
  end;

  PSyncObjsInfo = ^RSyncObjsInfo;
  RSyncObjsInfo = record
    PerfIdx: Integer;
    Link: PSyncObjsInfo; // Линк на пару
    LinkExt: PSyncObjsInfo; // Внешний линк на другую пару
    SyncObjsInfo: TDbgSyncObjsInfoEx;
  public
    function WaitTime: Int64;
    function IsShortLock: LongBool;

    function Enter: PSyncObjsInfo; inline;
    function Leave: PSyncObjsInfo; inline;

    function EnterExt: PSyncObjsInfo;
    function LeaveExt: PSyncObjsInfo;
  end;

  TSyncObjsInfoList = TBaseCollectList; // TCollectList<RSyncObjsInfo>;

  PRPSyncObjsInfo = ^RPSyncObjsInfo;
  RPSyncObjsInfo = record
    SyncObjsInfo: PSyncObjsInfo;
  end;

  TSyncObjsInfoListByID = TDictionary<NativeUInt, PSyncObjsInfo>;
  TSyncObjsInfoListByCS = TDictionary<Pointer, PSyncObjsInfo>;

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
  public
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

  TPerfInfo = class
  public
    DeltaTickCPU: UInt64;   // загрузка CPU
    DeltaTime: UInt64;
    //StackPoint: TStackPointList;
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
        PerfInfo: TPerfInfo;
      );
      ptMemoryInfo: (
      );
      ptSyncObjsInfo: (
        SyncObjsInfo: TSyncObjsInfo;
      );
  end;

  TThreadAdvType = (tatUnknown = 0, tatService, tatNormal);

  PThreadAdvInfo = ^TThreadAdvInfo;
  TThreadAdvInfo = record
    ThreadId: TThreadId;
    ThreadParentId: TThreadId;
    ThreadData: PThreadData;
    ThreadClassName: String;
    ThreadName: String;
    ThreadAdvType: TThreadAdvType;

    function AsString: String;
  end;

  TCallFuncInfo = class
    FuncInfo: TObject;
    LineNo: Cardinal;
    CallCount: UInt64;
    Data: UInt64;
  public
    property Elapsed: UInt64 read Data write Data;
    property Size: UInt64 read Data write Data;
  end;

  TCallFuncCounter = class(TObjectDictionary<Pointer,TCallFuncInfo>)
  private
    function AddNewCallFunc(const Addr: Pointer): TCallFuncInfo;
  public
    function AddCallFunc(const Addr: Pointer): TCallFuncInfo;
    function GetCallFunc(const Addr: Pointer): TCallFuncInfo;
  end;
  TCallFuncCounterPair = TPair<Pointer,TCallFuncInfo>;

  TTrackUnitInfo = class;
  TTrackFuncInfo = class;

  TTrackUnitInfoBaseList = TObjectDictionary<TObject,TTrackUnitInfo>;
  TTrackFuncInfoBaseList = TObjectDictionary<TObject,TTrackFuncInfo>;

  TTrackUnitInfo = class
  private
    FUnitInfo: TObject;
    FFuncInfoList: TTrackFuncInfoBaseList;

    FCallCount: Int64;
  public
    constructor Create(AUnitInfo: TObject);
    destructor Destroy; override;

    procedure IncCallCount; inline;

    property UnitInfo: TObject read FUnitInfo;

    property FuncInfoList: TTrackFuncInfoBaseList read FFuncInfoList;

    property CallCount: Int64 read FCallCount;
  end;

  TCodeTrackUnitInfo = class(TTrackUnitInfo)
  private
    FElapsed: Int64;
  public
    procedure GrowElapsed(const Value: Int64); inline;

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

    function AddParentCall(const Addr: Pointer): TCallFuncInfo; inline;
    function AddChildCall(const Addr: Pointer): TCallFuncInfo; inline;

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

    SamplingCPUTime: UInt64;
    SamplingCount: Cardinal;
    SamplingQueue: TQueue<PDbgInfoStackRec>;

    WaitTime: Int64;        // Время блокировок

    DbgPoints: TThreadPointList;

    DbgGetMemInfo: TGetMemInfoList;
    DbgGetMemInfoSize: Int64;

    DbgSyncObjsInfo: TSyncObjsInfoList;
    DbgSyncObjsInfoByID: TSyncObjsInfoListByID;

    DbgGetMemUnitList: TMemInfoTrackUnitInfoList;
    DbgSyncObjsUnitList: TSyncObjsTrackUnitInfoList;
    //DbgGetMemFuncList: TTrackFuncInfoList;

    DbgExceptions: TThreadList;

    DbgTrackEventCount: Int64;
    DbgTrackUnitList: TCodeTrackUnitInfoList;
    DbgTrackFuncList: TCodeTrackFuncInfoList;
    DbgTrackStack: TTrackStack;
    DbgTrackUsedUnitList: TTrackUnitInfoList; // Временный список используемых юнитов при обработке стека

    function DbgPointsCount: Cardinal;
    function DbgPointByIdx(const Idx: Integer): PThreadPoint;

    function DbgExceptionsCount: Cardinal;
    function DbgExceptionsByIdx(const Idx: Cardinal): TExceptInfo;

    function DbgSyncObjsCount: Cardinal;
    function DbgSyncObjsByIdx(const Idx: Integer): PSyncObjsInfo;

    procedure UpdateGetMemUnitList;

    procedure Init;
    procedure Clear;
  end;

  TDbgThreadList = TBaseCollectList; //TCollectList<TThreadData>;
  TDbgActiveThreadList = TDictionary<TThreadId, PThreadData>;
  TDbgActiveThreads = array of PThreadData;

  PProcessPoint = ^TProcessPoint;
  TProcessPoint = packed record
    FromStart: Int64;        // кол-во тиков со старта
    CPUTime: UInt64;         // текущее время CPU
    case PointType: TDbgPointType of
      ptStart: ();
      ptStop: ();
      ptException: ();
      ptPerfomance: (
        //DeltaTick: Int64;
        //DeltaTickCPU: UInt64;
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

  //PProcessData = ^TProcessData;
  TProcessData = class
  public
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

    SamplingCPUTime: UInt64;
    SamplingCount: Int64;

    DbgPoints: TProcessPointList;

    DbgGetMemInfo: TGetMemInfoList; // Указатели с коллизиями
    DbgGetMemInfoSize: Cardinal;

    ProcessGetMemCount: Int64;
    ProcessGetMemSize: Int64;

    DbgExceptions: TThreadList;

    DbgTrackEventCount: Int64;
    DbgTrackUnitList: TCodeTrackUnitInfoList;
    DbgTrackFuncList: TCodeTrackFuncInfoList;
    DbgTrackUsedUnitList: TTrackUnitInfoList; // Временный список используемых юнитов при обработке стека

    CreatedProcessHandle: THandle;
    CreatedThreadHandle: THandle;
    AttachedProcessHandle: THandle;
    AttachedThreadHandle: THandle;
    AttachedFileHandle: THandle;

    constructor Create;
    destructor Destroy; override;

    //DbgShareMem: THandle; // FileMap для взаимодействия с процессом

    function Elapsed_MSec: Cardinal; // msec
    function DbgPointsCount: Cardinal;
    function DbgPointByIdx(const Idx: Cardinal): PProcessPoint;
    function CurDbgPointIdx: Integer;

    function DbgExceptionsCount: Cardinal;

    procedure Clear;

    procedure SetPEImage(APEImage: TJclPeImage);
  end;

  THardwareBreakpointEvent = procedure(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord;
    BreakPointIndex: THWBPIndex; var ReleaseBreakpoint: LongBool) of object;

  TDbgState = (dsNone, dsStarted, dsWait, dsPerfomance, dsTrace, dsEvent, dsPause, dsStoping, dsStoped, dsDbgFail);

  TDbgTraceState = (dtsContinue, dtsPause, dtsStepIn, dtsStepOver, dtsStepOut);

  TDbgLogType = (dltUnknown = 0, dltInfo, dltWarning, dltError, dltDebugOutput, dltProcessEvent, dltThreadEvent, dltExceptionEvent,
    dltBreakPointEvent, dltDLLEvent);

  TDbgLogItem = Class
  public
    LogType: TDbgLogType;
    DateTime: TDateTime;
    LogMessage: String;

    destructor Destroy; override;
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

uses Debuger, DebugInfo, WinAPIUtils, Collections.Base;

procedure RaiseDebugCoreException(const Msg: String);
begin
  raise EDebugCoreException.Create(Msg);
end;


{ TDebugProcessData }

procedure TProcessData.Clear;
begin
  SamplingCPUTime := 0;
  SamplingCount := 0;

  if DbgPoints <> Nil then
    FreeAndNil(DbgPoints);

  FreeAndNil(DbgGetMemInfo);
  DbgExceptions.Clear;

  FreeAndNil(DbgTrackFuncList);
  FreeAndNil(DbgTrackUnitList);
  FreeAndNil(DbgTrackUsedUnitList);
end;

constructor TProcessData.Create;
begin
  inherited;

  State := psNone;
  DbgPoints := Nil;
  DbgExceptions := TThreadList.Create;
end;

function TProcessData.CurDbgPointIdx: Integer;
begin
  if Assigned(DbgPoints) and (DbgPoints.Count > 0) then
    Result := DbgPoints.Count - 1
  else
  begin
    Result := 0;
    RaiseDebugCoreException();
  end;
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

destructor TProcessData.Destroy;
begin
  Clear;

  FreeAndNil(DbgExceptions);

  inherited;
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
  FreeAndNil(DbgSyncObjsInfoByID);
  FreeAndNil(DbgExceptions);

  FreeAndNil(DbgTrackUnitList);
  FreeAndNil(DbgTrackFuncList);
  FreeAndNil(DbgTrackStack);

  FreeAndNil(SamplingQueue);
  FreeAndNil(DbgTrackUsedUnitList);

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

function TThreadData.DbgPointByIdx(const Idx: Integer): PThreadPoint;
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

function TThreadData.DbgSyncObjsByIdx(const Idx: Integer): PSyncObjsInfo;
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

procedure TThreadData.Init;
begin
  ThreadID := 0;
  State := tsNone;
  ThreadHandle := 0;

  ThreadAdvInfo := Nil;

  Context := GetMemory(SizeOf(TContext));
  Breakpoint := GetMemory(SizeOf(THardwareBreakpoint));

  Started := 0;
  Elapsed := 0;
  CPUElapsed := 0;

  SamplingCPUTime := 0;
  SamplingCount := 0;
  SamplingQueue := TQueue<PDbgInfoStackRec>.Create(4096, True);
  DbgTrackUsedUnitList := TTrackUnitInfoList.Create(64);
  DbgTrackUsedUnitList.OwnsKeys := False;
  DbgTrackUsedUnitList.OwnsValues := False;

  DbgPoints := TCollectList<TThreadPoint>.Create;

  DbgGetMemInfo := TGetMemInfoList.Create(1024, True);
  DbgGetMemInfo.OwnsValues := True;

  DbgGetMemUnitList := TMemInfoTrackUnitInfoList.Create(512, True);
  DbgGetMemUnitList.OwnsValues := True;

  DbgSyncObjsUnitList := TSyncObjsTrackUnitInfoList.Create(512, True);
  DbgSyncObjsUnitList.OwnsValues := True;

  DbgExceptions := TThreadList.Create;

  DbgSyncObjsInfo := TCollectList<RSyncObjsInfo>.Create;

  DbgSyncObjsInfoByID := TSyncObjsInfoListByID.Create(4096, True);

  DbgTrackEventCount := 0;
  DbgTrackUnitList := TCodeTrackUnitInfoList.Create(512, True);
  DbgTrackUnitList.OwnsValues := True;

  DbgTrackFuncList := TCodeTrackFuncInfoList.Create(4096, True);
  DbgTrackFuncList.OwnsValues := True;

  DbgTrackStack := TTrackStack.Create;
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
    ptPerfomance:
      FreeAndNil(PerfInfo);
    ptSyncObjsInfo:
      FreeAndNil(SyncObjsInfo);
  end;
end;

{ TThreadAdvInfo }

function TThreadAdvInfo.AsString: String;
const
  _UNKNOWN = 'unknown';
begin
  if ThreadName <> '' then
    Result := ThreadName
  else
    if ThreadClassName <> '' then
      Result := ThreadClassName
    else
      Result := _UNKNOWN;
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
  Result := Nil;

  FLock.BeginRead;
  try
    if (Index >= 0) and (Index < Count) then
      Result := TDbgLogItem(List[Index]);
  finally
    FLock.EndRead;
  end;
end;

{ TTrackFuncInfo }

function TTrackFuncInfo.AddChildCall(const Addr: Pointer): TCallFuncInfo;
begin
  Result := FChildFuncs.AddCallFunc(Addr);
end;

function TTrackFuncInfo.AddParentCall(const Addr: Pointer): TCallFuncInfo;
begin
  Result := FParentFuncs.AddCallFunc(Addr)
end;

constructor TTrackFuncInfo.Create(AFuncInfo: TObject);
begin
  inherited Create;

  FFuncInfo := AFuncInfo;
  FTrackUnitInfo := nil;
  FParentFuncs := TCallFuncCounter.Create(128);
  FParentFuncs.OwnsValues := True;
  FChildFuncs := TCallFuncCounter.Create(128);
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

  // В стеке может быть несколько функций из одного модуля
  // Инкрементить можно только один раз на стек
  //FTrackUnitInfo.IncCallCount;
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

function TCallFuncCounter.AddCallFunc(const Addr: Pointer): TCallFuncInfo;
begin
  Result := Nil;

  if (Addr = Nil) or (Addr = Pointer(-1)) then
    Exit;

  if TryGetValue(Addr, Result) then
    Inc(Result.CallCount)
  else
  begin
    Result := AddNewCallFunc(Addr);
    Result.CallCount := 1;

    Add(Addr, Result);
  end;
end;

function TCallFuncCounter.AddNewCallFunc(const Addr: Pointer): TCallFuncInfo;
var
  UnitInfo: TUnitInfo;
  FuncInfo: TFuncInfo;
  LineInfo: TLineInfo;
begin
  Result := TCallFuncInfo.Create;

  FuncInfo := nil;
  LineInfo := nil;
  if gvDebugInfo.GetLineInfo(Addr, UnitInfo, FuncInfo, LineInfo, False) <> slNotFound then
  begin
    Result.FuncInfo := FuncInfo;
    if Assigned(LineInfo) then
      Result.LineNo := LineInfo.LineNo;
  end;
end;

function TCallFuncCounter.GetCallFunc(const Addr: Pointer): TCallFuncInfo;
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
    LockForWrite;
    try
      if not TryGetValue(UnitInfo, Result) then
      begin
        Result := CreateTrackUnitInfo(UnitInfo);

        Add(UnitInfo, Result);
      end;
    finally
      UnLockForWrite;
    end;
  end;
end;

{ TMemInfoTrackUnitInfoList }

function TMemInfoTrackUnitInfoList.CreateTrackUnitInfo(const UnitInfo: TObject): TTrackUnitInfo;
begin
  Result := TMemInfoTrackUnitInfo.Create(UnitInfo);
  Result.FuncInfoList.OwnsValues := True;
end;

procedure TMemInfoTrackUnitInfoList.LoadStack(const GetMemInfo: TGetMemInfo);
var
  StackEntry: TStackEntry;
  I: Integer;
  Addr: Pointer;
  TrackUnitInfo: TMemInfoTrackUnitInfo;
  TrackFuncInfo: TTrackFuncInfo;
  CallFuncInfo: TCallFuncInfo;
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
            Inc(CallFuncInfo.Data, GetMemInfo.Size);
        end;

        // TODO: Искать функцию с валидным Addr
        if I < High(GetMemInfo.Stack) then
        begin
          Addr := GetMemInfo.Stack[I + 1];
          CallFuncInfo := TrackFuncInfo.AddParentCall(Addr);
          if Assigned(CallFuncInfo) then
            Inc(CallFuncInfo.Data, GetMemInfo.Size);
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
  FFuncInfoList.OwnsValues := False;
  FFuncInfoList.OwnsKeys := False;

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
  TInterlocked.Add(FCallCount, 1);
end;

procedure TCodeTrackUnitInfo.GrowElapsed(const Value: Int64);
begin
  TInterlocked.Add(FElapsed, Value);
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
  Result.FuncInfoList.OwnsValues := True;
end;

procedure TSyncObjsTrackUnitInfoList.LoadStack(const SyncObjsInfo: PSyncObjsInfo);
var
  StackEntry: TStackEntry;
  I: Integer;
  Addr: Pointer;
  TrackUnitInfo: TSyncObjsTrackUnitInfo;
  TrackFuncInfo: TTrackFuncInfo;
  CallFuncInfo: TCallFuncInfo;
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

  // Не логируем короткие критические секции
  // Для Enter ещё нет информации. Такое будет фильтроваться уже только при выводе на форму
  // TODO: Определить для этого отдельную опцию
  if SyncObjsInfo^.IsShortLock then
    Exit;

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
    soInCriticalSection:
      begin
        //
      end;
    soSendMessage:
      begin
        // Игнорим SendMessage в главном потоке
        if DbgSyncObjsInfo^.ThreadId = gvDebuger.ProcessData.MainThreadID then
          Exit;
      end;
  end;

  StackEntry := TStackEntry.Create;
  try
    if DbgSyncObjsInfo^.SyncObjsType = soInCriticalSection then
    begin
      // Регистрируем только сам факт использования критической секции, без WaitTime
      // WaitTime будет отображаться в списке объектов

      // Так как вход и выход из критической секции могут быть в разных функциях,
      // то регистрировать надо оба случая

      for I := 0 to High(DbgSyncObjsInfo^.Stack) do
      begin
        Addr := DbgSyncObjsInfo^.Stack[I];

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

          // Регистрируем вход в SyncObj
          //TSyncObjsTrackFuncInfo(TrackFuncInfo).IncCallCount;

          // Добавляем SyncObj в список текущей функции
          SyncObjsListItem := TSyncObjsTrackFuncInfo(TrackFuncInfo).SyncObjsList.Add;
          SyncObjsListItem^.SyncObjsInfo := SyncObjsInfo;
          TSyncObjsTrackFuncInfo(TrackFuncInfo).SyncObjsList.Commit;

          // Отрабатываем только первую функцию по которой есть инфа
          Break;
        end;
      end;
    end
    else
    begin
      // Проход по стеку
      for I := 0 to High(DbgSyncObjsInfo^.Stack) do
      begin
        Addr := DbgSyncObjsInfo^.Stack[I];

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
                TSyncObjsTrackFuncInfo(TrackFuncInfo).SyncObjsList.Commit;

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
                      Inc(CallFuncInfo.Data, WaitTime);
                  end;

                  // TODO: Искать функцию с валидным Addr
                  if I < High(DbgSyncObjsInfo^.Stack) then
                  begin
                    Addr := DbgSyncObjsInfo^.Stack[I + 1];
                    CallFuncInfo := TrackFuncInfo.ParentFuncs.GetCallFunc(Addr);
                    if Assigned(CallFuncInfo) then
                      Inc(CallFuncInfo.Data, WaitTime);
                  end;
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

function RSyncObjsInfo.Enter: PSyncObjsInfo;
begin
  if SyncObjsInfo.SyncObjsStateType = sosEnter then
    Result := @Self
  else
    Result := Link;
end;

function RSyncObjsInfo.EnterExt: PSyncObjsInfo;
begin
  if (LinkExt <> Nil) and (SyncObjsInfo.SyncObjsType = soInCriticalSection) then
    Result := LinkExt.Enter
  else
    Result := Enter;
end;

function RSyncObjsInfo.IsShortLock: LongBool;
var
  _Leave: PSyncObjsInfo;
  _Enter: PSyncObjsInfo;
begin
  Result := False;

  _Leave := LeaveExt;
  if Assigned(_Leave) then
  begin
    _Enter := EnterExt;
    if Assigned(_Enter) then
      Result := ((_Leave^.PerfIdx - _Enter^.PerfIdx) <= 1);
  end;
end;

function RSyncObjsInfo.Leave: PSyncObjsInfo;
begin
  if SyncObjsInfo.SyncObjsStateType = sosLeave then
    Result := @Self
  else
    Result := Link;
end;

function RSyncObjsInfo.LeaveExt: PSyncObjsInfo;
begin
  if (LinkExt <> Nil) and (SyncObjsInfo.SyncObjsType = soEnterCriticalSection) then
    Result := LinkExt.Leave
  else
    Result := Leave;
end;

function RSyncObjsInfo.WaitTime: Int64;
var
  _Leave: PSyncObjsInfo;
  _Enter: PSyncObjsInfo;
  _LeaveTime: Int64;
begin
  Result := 0;

  _Leave := Leave;
  if Assigned(_Leave) then
    _LeaveTime := _Leave.SyncObjsInfo.CurTime
  else
    _LeaveTime := _QueryPerformanceCounter;

  _Enter := Enter;
  begin
    if Assigned(_Enter) then
      Result := _LeaveTime - _Enter.SyncObjsInfo.CurTime;
  end;
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
  for I := 0 to High(DbgStack^) do
  begin
    Ptr := DbgStack^[I];

    if (Ptr = nil) or (Ptr = Pointer(-1)) then
    begin
      SetLength(Stack, I);
      if I > 0 then
        Move(DbgStack^[0], Stack[0], I * SizeOf(Pointer));

      Exit;
    end;
  end;
end;

{ TDbgLogItem }

destructor TDbgLogItem.Destroy;
begin
  LogType := dltUnknown;

  inherited;
end;

end.
