unit DebugerTypes;

interface

uses SysUtils, Windows, Classes, JclPeImage, SyncObjs, ClassUtils, Generics.Collections, DbgHookTypes, Contnrs;

const
  _SEGMENT_SIZE = 16 * 1024;

type
  TSysUInt = NativeUInt;

  TSegment<T> = Array of T;

  TSegList<T> = Array of TSegment<T>;

  TCollectListError = class(Exception);

  PData = Pointer;

  TBaseCollectList = class
  private
    FCount: Cardinal;
    FLock: TCriticalSection;
  protected
    function GetItem(const Index: Cardinal): PData; virtual; abstract;
    procedure CheckSeg(const Seg: Integer); virtual; abstract;

    function IndexToSegment(const Index: Cardinal; var Seg, Offset: Integer): Boolean; virtual; abstract;
    procedure RaiseError(Msg: PString; const Args: Array of const);
  public
    constructor Create;
    destructor Destroy; override;

    function Add: PData; virtual;
    procedure Clear; virtual;

    procedure Lock;
    procedure UnLock;

    property Count: Cardinal read FCount;
    property Items[const Index: Cardinal]: PData read GetItem; default;
  end;

  (*
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
  *)

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

  PTrackBreakpoint = ^TTrackBreakpoint;
  TTrackBreakpoint = record
    FuncInfo: TObject;
    SaveByte: Byte;
  end;

  TTrackBreakpointList = class(TDictionary<Pointer,PTrackBreakpoint>)
  protected
    procedure ValueNotify(const Value: PTrackBreakpoint; Action: TCollectionNotification); override;
  end;

  PTrackRETBreakpoint = ^TTrackRETBreakpoint;
  TTrackRETBreakpoint = record
    Count: Cardinal;
    SaveByte: Byte;
  end;

  TTrackRETBreakpointList = class(TDictionary<Pointer,PTrackRETBreakpoint>)
  protected
    procedure ValueNotify(const Value: PTrackRETBreakpoint; Action: TCollectionNotification); override;
  end;


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

  PStackPoint = ^TStackPoint;
  TStackPoint = packed record
    EIP: Pointer;
    EBP: Pointer;
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

  PGetMemInfo = ^RGetMemInfo;
  RGetMemInfo = record
    PerfIdx: Cardinal;
    Size: Cardinal;
    Stack: TDbgMemInfoStack;
    ObjectType: ShortString;

    function GetObjectType(const Ptr: Pointer): String;
  end;

  TGetMemInfo = class(TDictionary<Pointer,PGetMemInfo>)
  private
    FLock: TCriticalSection;
  protected
    procedure ValueNotify(const Value: PGetMemInfo; Action: TCollectionNotification); override;
  public
    constructor Create(ACapacity: Integer = 0);
    destructor Destroy; override;

    procedure Lock; inline;
    procedure UnLock; inline;
  end;
  TGetMemInfoItem = TPair<Pointer,PGetMemInfo>;

  TStackPointList = Array of TStackPoint;

  TExceptInfo = class
  public
    ThreadID: TThreadId;
    Address: Pointer;
    Frame: Pointer;
    ExceptionName: String;
    Message: String;
    Stack: TList;

    constructor Create(DebugEvent: PDebugEvent);
    destructor Destroy; override;
  end;

  TDbgPointType = (ptNone, ptWait, ptStart, ptStop, ptException, ptPerfomance, ptThreadInfo, ptMemoryInfo);

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
  end;

  PThreadData = ^TThreadData;

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
    Ellapsed: UInt64;
  end;

  TCallFuncCounter = class(TDictionary<Pointer,PCallFuncInfo>)
  private
    function AddNewCallFunc(const Addr: Pointer): PCallFuncInfo;
  protected
    procedure ValueNotify(const Value: PCallFuncInfo; Action: TCollectionNotification); override;
  public
    function AddCallFunc(const Addr: Pointer): PCallFuncInfo;
  end;
  TCallFuncCounterPair = TPair<Pointer,PCallFuncInfo>;

  TTrackUnitInfo = class;
  TTrackFuncInfo = class;

  TTrackUnitInfoBaseList = TDictionary<TObject,TTrackUnitInfo>;
  TTrackFuncInfoBaseList = TDictionary<TObject,TTrackFuncInfo>;

  TTrackUnitInfo = class
  private
    FUnitInfo: TObject;
    FFuncInfoList: TTrackFuncInfoBaseList;

    FCallCount: UInt64;
    FCurCount: Int64;

    FTrackData: Int64;
    FTrackDataU: UInt64;
  public
    constructor Create(AUnitInfo: TObject);
    destructor Destroy; override;

    procedure GrowEllapsed(const Value: UInt64); inline;
    procedure GrowSize(const Value: Int64); inline;

    procedure IncCallCount; inline;

    procedure IncCurCount; inline;
    procedure DecCurCount; inline;

    property UnitInfo: TObject read FUnitInfo;

    property CallCount: UInt64 read FCallCount;
    property CurCount: Int64 read FCurCount;

    property TrackDataU: UInt64 read FTrackDataU;
    property TrackData: Int64 read FTrackData;

    // TODO: высчитать общее время выполнения функций юнита с учетом вложенности
    // property Ellapsed: UInt64 read FEllapsed;

    property FuncInfoList: TTrackFuncInfoBaseList read FFuncInfoList;
  end;

  TTrackUnitInfoList = class(TTrackUnitInfoBaseList)
  protected
    procedure ValueNotify(const Value: TTrackUnitInfo; Action: TCollectionNotification); override;
  public
    function GetTrackUnitInfo(const UnitInfo: TObject): TTrackUnitInfo;
    procedure CheckTrackFuncInfo(TrackFuncInfo: TTrackFuncInfo);
    procedure LoadStack(var Stack: TDbgMemInfoStack; const IsInc: Boolean = True);
  end;
  TTrackUnitInfoPair = TPair<TObject,TTrackUnitInfo>;

  TTrackFuncInfo = class
  private
    FFuncInfo: TObject;
    FTrackUnitInfo: TTrackUnitInfo;

    FCallCount: UInt64;
    FCurCount: Int64;

    FTrackData: Int64;
    FTrackDataU: UInt64;

    FParentFuncs: TCallFuncCounter;
    FChildFuncs: TCallFuncCounter;
  public
    constructor Create(AFuncInfo: TObject);
    destructor Destroy; override;

    function AddParentCall(const Addr: Pointer): PCallFuncInfo;
    function AddChildCall(const Addr: Pointer): PCallFuncInfo;

    procedure GrowEllapsed(const Value: UInt64); inline;
    procedure GrowSize(const Value: Int64); inline;

    procedure IncCallCount; inline;

    procedure IncCurCount; inline;
    procedure DecCurCount; inline;

    property FuncInfo: TObject read FFuncInfo;

    property CallCount: UInt64 read FCallCount;
    property CurCount: Int64 read FCurCount;

    property CPUEllapsed: UInt64 read FTrackDataU;
    property Size: Int64 read FTrackData;

    property TrackDataU: UInt64 read FTrackDataU;
    property TrackData: Int64 read FTrackData;

    property TrackUnitInfo: TTrackUnitInfo read FTrackUnitInfo write FTrackUnitInfo;
    property ParentFuncs: TCallFuncCounter read FParentFuncs;
    property ChildFuncs: TCallFuncCounter read FChildFuncs;
  end;

  TTrackFuncInfoList = class(TTrackFuncInfoBaseList)
  protected
    procedure ValueNotify(const Value: TTrackFuncInfo; Action: TCollectionNotification); override;
  public
    function GetTrackFuncInfo(const FuncInfo: TObject): TTrackFuncInfo;
  end;
  TTrackFuncInfoPair = TPair<TObject,TTrackFuncInfo>;

  PTrackStackPoint = ^TTrackStackPoint;
  TTrackStackPoint = record
  private
    function GetLeave: UInt64;
    procedure SetLeave(const Value: UInt64);
  public
    TrackFuncInfo: TTrackFuncInfo;
    ParentTrackFuncInfo: TTrackFuncInfo;

    ProcTrackFuncInfo: TTrackFuncInfo;
    ProcParentTrackFuncInfo: TTrackFuncInfo;

    TrackRETBreakpoint: PTrackRETBreakpoint;

    Enter: UInt64;
    Ellapsed: UInt64;

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
    Ellapsed: Int64;        // время выполнения

    CPUEllapsed: UInt64;    // циклы CPU
    CPUTime: UInt64;        // время использования CPU

    DbgPoints: TThreadPointList;

    DbgGetMemInfo: TGetMemInfo;
    DbgGetMemInfoSize: Cardinal;

    DbgGetMemUnitList: TTrackUnitInfoList;
    //DbgGetMemFuncList: TTrackFuncInfoList;

    DbgExceptions: TThreadList;

    DbgTrackEventCount: UInt64;
    DbgTrackUnitList: TTrackUnitInfoList;
    DbgTrackFuncList: TTrackFuncInfoList;
    DbgTrackStack: TTrackStack;

    function DbgPointsCount: Cardinal;
    function DbgPointByIdx(const Idx: Cardinal): PThreadPoint;

    function DbgExceptionsCount: Cardinal;
    function DbgExceptionsByIdx(const Idx: Cardinal): TExceptInfo;

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
    Ellapsed: Int64;
    CPUTime: UInt64;
    CPUEllapsed: UInt64; // время использования CPU

    DbgPoints: TProcessPointList;

    DbgGetMemInfo: TGetMemInfo; // Указатели с коллизиями
    DbgGetMemInfoSize: Cardinal;

    ProcessGetMemCount: Cardinal;
    ProcessGetMemSize: Cardinal;

    DbgExceptions: TThreadList;

    DbgTrackEventCount: UInt64;
    DbgTrackUnitList: TTrackUnitInfoList;
    DbgTrackFuncList: TTrackFuncInfoList;

    CreatedProcessHandle: THandle;
    CreatedThreadHandle: THandle;
    AttachedProcessHandle: THandle;
    AttachedThreadHandle: THandle;
    AttachedFileHandle: THandle;

    //DbgShareMem: THandle; // FileMap для взаимодействия с процессом

    function Ellapsed_MSec: Cardinal; // msec
    function DbgPointsCount: Cardinal;
    function DbgPointByIdx(const Idx: Cardinal): PProcessPoint;
    function CurDbgPointIdx: Cardinal;

    function DbgExceptionsCount: Cardinal;

    procedure Clear;

    procedure SetPEImage(APEImage: TJclPeImage);
  end;

  THardwareBreakpointEvent = procedure(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord;
    BreakPointIndex: THWBPIndex; var ReleaseBreakpoint: Boolean) of object;

  TDbgState = (dsNone, dsStarted, dsWait, dsPerfomance, dsTrace, dsEvent, dsStoping, dsStoped, dsDbgFail);

  TDbgLogType = (dltInfo, dltWarning, dltError, dltDebugOutput, dltProcessEvent, dltThreadEvent, dltExceptionEvent,
    dltBreakPointEvent, dltDLLEvent);

  TDbgLogItem = Class
    LogType: TDbgLogType;
    DateTime: TDateTime;
    LogMessage: String;
  End;

  TDbgLog = class(TThreadList)
  private
    function GetItem(const Index: Integer): TDbgLogItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ClearLog;

    function Count: Integer;

    procedure Add(const LogType: TDbgLogType; const Msg: String); overload;
    procedure Add(const LogType: TDbgLogType; const FmtMsg: String; const Args: array of Const); overload;

    property Items[const Index: Integer]: TDbgLogItem read GetItem; default;
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
  FreeAndNil(DbgExceptions);

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

function TProcessData.Ellapsed_MSec: Cardinal;
var
  Cur: Int64;
  Freq: Int64;
begin
  if State = psActive then
    Cur := _QueryPerformanceCounter
  else
    Cur := Ellapsed;

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
  //FreeAndNil(DbgGetMemFuncList);
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

procedure TThreadData.UpdateGetMemUnitList;
var
  GetMemInfoItem: TGetMemInfoItem;
begin
  DbgGetMemUnitList.Clear;

  DbgGetMemInfo.Lock;
  try
    for GetMemInfoItem in DbgGetMemInfo do
      DbgGetMemUnitList.LoadStack(GetMemInfoItem.Value^.Stack);
  finally
    DbgGetMemInfo.UnLock;
  end;
end;

{ TThreadPoint }

procedure TThreadPoint.Clear;
begin
  if PointType = ptException then
    FreeAndNil(ExceptInfo);
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

{ TCollectList<T> }
(*
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
*)

{ TBaseCollectList }

function TBaseCollectList.Add: PData;
begin
  Result := Nil;
  Inc(FCount);
end;

procedure TBaseCollectList.Clear;
begin
  FCount := 0;
end;

constructor TBaseCollectList.Create;
begin
  inherited;

  FCount := 0;
  FLock := TCriticalSection.Create;
end;

destructor TBaseCollectList.Destroy;
begin
  Clear;
  FreeAndNil(FLock);

  inherited;
end;

procedure TBaseCollectList.Lock;
begin
  FLock.Enter;
end;

procedure TBaseCollectList.RaiseError(Msg: PString; const Args: array of const);
begin
  raise TCollectListError.CreateFmt(Msg^, Args);
end;

procedure TBaseCollectList.UnLock;
begin
  FLock.Leave;
end;

{ TGetMemInfo }

constructor TGetMemInfo.Create(ACapacity: Integer);
begin
  inherited Create(ACapacity);

  FLock := TCriticalSection.Create;
end;

destructor TGetMemInfo.Destroy;
begin
  Lock;
  Clear;
  UnLock;

  FreeAndNil(FLock);

  inherited;
end;

procedure TGetMemInfo.Lock;
begin
  FLock.Enter;
end;

procedure TGetMemInfo.UnLock;
begin
  FLock.Leave;
end;

procedure TGetMemInfo.ValueNotify(const Value: PGetMemInfo; Action: TCollectionNotification);
begin
  inherited;

  if Action = cnRemoved then
  begin
    Lock;
    FreeMemory(Value);
    UnLock;
  end;
end;

{ RGetMemInfo }

function RGetMemInfo.GetObjectType(const Ptr: Pointer): String;
begin
  Result := '';
  if ObjectType <> '' then
    Result := String(ObjectType)
  else
  begin
    if not (gvDebuger.DbgState in [dsNone, dsStoped, dsDbgFail]) then
      Result := gvDebugInfo.GetClassName(Ptr);
  end;
end;

{ TExceptInfo }

constructor TExceptInfo.Create(DebugEvent: PDebugEvent);
var
  ER: PExceptionRecord;
begin
  inherited Create;

  ThreadID := DebugEvent^.dwThreadId;
  ER := @DebugEvent^.Exception.ExceptionRecord;
  Address := gvDebugInfo.GetExceptionAddress(ER);
  Frame := gvDebugInfo.GetExceptionFrame(ER);
  ExceptionName := gvDebugInfo.GetExceptionName(ER);
  Message := gvDebugInfo.GetExceptionMessage(ER, ThreadID);

  Stack := TObjectList.Create;
  gvDebugInfo.GetCallStackItems(ThreadID, Address, Frame, Stack);
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

  inherited Add(LogItem);
end;

procedure TDbgLog.Add(const LogType: TDbgLogType; const FmtMsg: String; const Args: array of Const);
begin
  Add(LogType, Format(FmtMsg, Args));
end;

procedure TDbgLog.ClearLog;
var
  L: TList;
begin
  L := LockList;
  try
    ClearList(L);
  finally
    UnlockList;
  end;
end;

function TDbgLog.Count: Integer;
var
  L: TList;
begin
  L := LockList;
  Result := L.Count;
  UnlockList;
end;

constructor TDbgLog.Create;
var
  L: TList;
begin
  inherited Create;

  L := LockList;
  L.Capacity := 1000;
  UnlockList;
end;

destructor TDbgLog.Destroy;
begin
  ClearLog;

  inherited;
end;

function TDbgLog.GetItem(const Index: Integer): TDbgLogItem;
var
  L: TList;
begin
  L := LockList;
  try
    Result := L[Index];
  finally
    UnlockList;
  end;
end;

{ TTrackFuncInfo }

function TTrackFuncInfo.AddChildCall(const Addr: Pointer): PCallFuncInfo;
begin
  Result := FChildFuncs.AddCallFunc(Addr)
end;

function TTrackFuncInfo.AddParentCall(const Addr: Pointer): PCallFuncInfo;
begin
  Result := FParentFuncs.AddCallFunc(Addr)
end;

constructor TTrackFuncInfo.Create(AFuncInfo: TObject);
begin
  inherited Create;

  FFuncInfo := AFuncInfo;
  FCallCount := 0;
  FTrackData := 0;
  FTrackUnitInfo := nil;
  FParentFuncs := TCallFuncCounter.Create(256);
  FChildFuncs := TCallFuncCounter.Create(256);
end;

procedure TTrackFuncInfo.DecCurCount;
begin
  Dec(FCurCount);
  FTrackUnitInfo.DecCurCount;
end;

destructor TTrackFuncInfo.Destroy;
begin
  FreeAndNil(FParentFuncs);
  FreeAndNil(FChildFuncs);

  inherited;
end;

procedure TTrackFuncInfo.GrowEllapsed(const Value: UInt64);
begin
  Inc(FTrackDataU, Value);
  FTrackUnitInfo.GrowEllapsed(Value);
end;

procedure TTrackFuncInfo.GrowSize(const Value: Int64);
begin
  Inc(FTrackData, Value);
  FTrackUnitInfo.GrowSize(Value);
end;

procedure TTrackFuncInfo.IncCallCount;
begin
  Inc(FCallCount);
  FTrackUnitInfo.IncCallCount;
end;

procedure TTrackFuncInfo.IncCurCount;
begin
  Inc(FCurCount);
  FTrackUnitInfo.IncCurCount;
end;

{ TTrackFuncInfoList }

function TTrackFuncInfoList.GetTrackFuncInfo(const FuncInfo: TObject): TTrackFuncInfo;
begin
  Assert(Assigned(FuncInfo));

  if not TryGetValue(FuncInfo, Result) then
  begin
    Result := TTrackFuncInfo.Create(FuncInfo);
    Add(FuncInfo, Result);
  end;
end;

procedure TTrackFuncInfoList.ValueNotify(const Value: TTrackFuncInfo; Action: TCollectionNotification);
begin
  inherited;

  if Action = cnRemoved then
    Value.Free;
end;

{ TCallCounter }

function TCallFuncCounter.AddCallFunc(const Addr: Pointer): PCallFuncInfo;
begin
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
  Result := GetMemory(SizeOf(TCallFuncInfo));
  ZeroMemory(Result, SizeOf(TCallFuncInfo));

  FuncInfo := nil;
  LineInfo := nil;
  if gvDebugInfo.GetLineInfo(Addr, UnitInfo, FuncInfo, LineInfo, False) <> slNotFound then
  begin
    Result^.FuncInfo := FuncInfo;
    if Assigned(LineInfo) then
      Result^.LineNo := LineInfo.LineNo;
  end;
end;

procedure TCallFuncCounter.ValueNotify(const Value: PCallFuncInfo; Action: TCollectionNotification);
begin
  inherited;

  if Action = cnRemoved then
    FreeMemory(Value);
end;

{ TTrackBreakpointList }

procedure TTrackBreakpointList.ValueNotify(const Value: PTrackBreakpoint; Action: TCollectionNotification);
begin
  inherited;

  if Action = cnRemoved then
    FreeMem(Value);
end;

{ TTrackRETBreakpointList }

procedure TTrackRETBreakpointList.ValueNotify(const Value: PTrackRETBreakpoint; Action: TCollectionNotification);
begin
  inherited;

  if Action = cnRemoved then
    FreeMem(Value);
end;

{ TTrackStackPoint }


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

    if not TrackFuncInfo.TrackUnitInfo.FuncInfoList.ContainsKey(FuncInfo) then
      TrackFuncInfo.TrackUnitInfo.FuncInfoList.Add(FuncInfo, TrackFuncInfo);
  end;
end;

function TTrackUnitInfoList.GetTrackUnitInfo(const UnitInfo: TObject): TTrackUnitInfo;
begin
  if not TryGetValue(UnitInfo, Result) then
  begin
    Result := TTrackUnitInfo.Create(UnitInfo);

    Add(UnitInfo, Result);
  end;
end;

procedure TTrackUnitInfoList.LoadStack(var Stack: TDbgMemInfoStack; const IsInc: Boolean = True);
var
  I: Integer;
  StackEntry: TStackEntry;
  TrackUnitInfo: TTrackUnitInfo;
  TrackFuncInfo: TTrackFuncInfo;
begin
  StackEntry := TStackEntry.Create;
  try
    for I := 0 to High(Stack) do
    begin
      if StackEntry.UpdateInfo(Stack[I]) in [slFoundExact, slFoundNotExact] then
      begin
        TrackUnitInfo := GetTrackUnitInfo(StackEntry.UnitInfo);

        if not TrackUnitInfo.FuncInfoList.TryGetValue(StackEntry.FuncInfo, TrackFuncInfo) then
        begin
          TrackFuncInfo := TTrackFuncInfo.Create(StackEntry.FuncInfo);
          TrackFuncInfo.TrackUnitInfo := TrackUnitInfo;

          TrackUnitInfo.FuncInfoList.Add(StackEntry.FuncInfo, TrackFuncInfo);
        end;

        if IsInc then
        begin
          TrackFuncInfo.IncCallCount;
          TrackFuncInfo.IncCurCount;

          if I > 0 then
            TrackFuncInfo.AddChildCall(Stack[I - 1]);

          if I < High(Stack) then
            TrackFuncInfo.AddParentCall(Stack[I + 1]);
        end
        else
          TrackFuncInfo.DecCurCount;
      end;
    end;
  finally
    FreeAndNil(StackEntry);
  end;
end;

procedure TTrackUnitInfoList.ValueNotify(const Value: TTrackUnitInfo; Action: TCollectionNotification);
begin
  inherited;

  if Action = cnRemoved then
    Value.Free;
end;

{ TTrackUnitInfo }

constructor TTrackUnitInfo.Create(AUnitInfo: TObject);
begin
  inherited Create;

  FFuncInfoList := TTrackFuncInfoBaseList.Create(128);

  FUnitInfo := AUnitInfo;
  FCallCount := 0;
  FTrackData := 0;
end;

procedure TTrackUnitInfo.DecCurCount;
begin
  Dec(FCurCount);
end;

destructor TTrackUnitInfo.Destroy;
begin
  FUnitInfo := nil;
  FreeAndNil(FFuncInfoList);

  inherited;
end;

procedure TTrackUnitInfo.GrowEllapsed(const Value: UInt64);
begin
  Inc(FTrackDataU, Value);
end;

procedure TTrackUnitInfo.GrowSize(const Value: Int64);
begin
  Inc(FTrackData, Value);
end;

procedure TTrackUnitInfo.IncCallCount;
begin
  Inc(FCallCount);
end;

procedure TTrackUnitInfo.IncCurCount;
begin
  Inc(FCurCount);
end;

{ TTrackStackPoint }

function TTrackStackPoint.GetLeave: UInt64;
begin
  Result := Enter + Ellapsed;
end;

procedure TTrackStackPoint.SetLeave(const Value: UInt64);
begin
  Ellapsed := (Value - Enter) + 1;
end;

end.
