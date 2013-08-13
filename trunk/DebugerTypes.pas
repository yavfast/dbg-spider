unit DebugerTypes;

interface

uses Windows, Classes, SysUtils, JclPeImage, SyncObjs, ClassUtils, Generics.Collections;

const
  _SEGMENT_SIZE = 16 * 1024;

type
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

// ����� ����������
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

type
  TThreadId = type Cardinal;
  TProcessId = type Cardinal;

  EDebugCoreException = class(Exception);

  // �������� ����������� ���������� ����� ��������� �������
  TResumeAction = (
    raRun, // ���������� ����������
    raTraceInto, // ��������� ����������� ����� ���������� � ������� ������ �������
    raStepOver, // ��������� ����������� ����� ���������� ��� ������ ������ �������
    raRunUntilReturn, // ���������� ���������� �� ������ �� ������� ���������
    raStop // ���������� �������
  );

  // ���������� �������� ������������
  TCreateThreadEvent = procedure(Sender: TObject; ThreadId: TThreadId; Data: PCreateThreadDebugInfo) of object;
  TCreateProcessEvent = procedure(Sender: TObject; ProcessId: TProcessId; Data: PCreateProcessDebugInfo) of object;
  TExitThreadEvent = procedure(Sender: TObject; ThreadId: TThreadId; Data: PExitThreadDebugInfo) of object;
  TExitProcessEvent = procedure(Sender: TObject; ProcessId: TProcessId; Data: PExitProcessDebugInfo) of object;
  TLoadDllEvent = procedure(Sender: TObject; ThreadId: TThreadId; Data: PLoadDLLDebugInfo) of object;
  TUnLoadDllEvent = procedure(Sender: TObject; ThreadId: TThreadId; Data: PUnloadDLLDebugInfo) of object;
  TDebugStringEvent = procedure(Sender: TObject; ThreadId: TThreadId; Data: POutputDebugStringInfo) of object;
  TRipEvent = procedure(Sender: TObject; ThreadId: TThreadId; Data: PRIPInfo) of object;

  // ���������� ������������ ���������� ����������
  TDefaultExceptionEvent = procedure(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord) of object;
  TBreakPointEvent = procedure(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord;
    BreakPointIndex: Integer; var ReleaseBreakpoint: Boolean) of object;

  // ������ ����� ���������� ���������� (��� ���������� ������ ���������,
  // ������������ � ���� �� ��������)

  TExceptionCode = (ecUnknown, ecBreakpoint, ecSingleStep, ecCtrlC, ecNonContinuable, ecGuard, ecSetThreadName);

  // ������ �������������� ����� ����� ��������� (����� ��)

  TBreakpointType = (
    btBreakpoint, // WriteProcessMemoryEx + 0xCC
    btMemoryBreakpoint // VirtualProtectEx + PAGE_GUARD
  );

  // ��������� ��� �������� ������ �� ��������� ��������� ��

  TInt3Breakpoint = record
    Address: Pointer;
    ByteCode: Byte;
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

  // ��������� �������� ������ �������� ������������ ��������� �����,
  // � ����� ������ � ������������� ���������� ������ ���������.
  // ������������ ����������, �������� ������������ ����� ������� GetThreadData
  // �������� !!!
  // ������ ���������� ����� �������������� ������������� ������ �� ������,
  // �� ���������� ������ ��������� � ������ ��������� ����� CloseHandle()

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

  TStackPointList = class
  public
    List: array of TStackPoint;
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

  TGetMemInfo = TDictionary<Pointer,Cardinal>;
  TGetMemInfoItem = TPair<Pointer,Cardinal>;

  TPointType = (ptStart, ptStop, ptException, ptPerfomance, ptThreadInfo, ptMemoryInfo);

  PThreadPoint = ^TThreadPoint;
  TThreadPoint = packed record
  public
    procedure Clear;
  public
    PerfIdx: Cardinal;
    case PointType: TPointType of
      ptStart: ();
      ptStop: ();
      ptException: (
        Stack: TStackPointList;
      );
      ptPerfomance: (
        DeltaTickCPU: UInt64;   // �������� CPU
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

  TThreadAdvInfoList = TBaseCollectList; //TCollectList<TThreadAdvInfo>;

  TThreadPointList = TBaseCollectList; //TCollectList<TThreadPoint>;

  TThreadMemInfoList = TBaseCollectList; //TCollectList<TMemInfo>;

  TThreadState = (tsNone, tsActive, tsFinished, tsSuspended, tsLocked);

  TThreadData = packed record
    ThreadID: TThreadId;
    State: TThreadState;
    ThreadHandle: THandle;
    ThreadAdvInfo: PThreadAdvInfo;
    Context: PContext; // ��������� ������ ���� �������� �� ������ 32 ���
    Breakpoint: PHardwareBreakpoint;
    Started: Int64;         // ������ �������
    Ellapsed: Int64;        // ����� ����������
    ThreadEllapsed: UInt64; // ����� ������������� CPU
    CPUTime: UInt64;
    DbgPoints: TThreadPointList;
    //DbgMemInfo: TThreadMemInfoList;
    DbgGetMemInfo: TGetMemInfo;
    DbgGetMemInfoSize: Cardinal;

    function DbgPointsCount: Cardinal;
    function DbgPointByIdx(const Idx: Cardinal): PThreadPoint;

    function DbgMemInfoCount: Cardinal;
    function DbgMemInfoByIdx(const Idx: Cardinal): PThreadPoint;

    procedure Clear;
  end;

  TThreadList = TBaseCollectList; //TCollectList<TThreadData>;

  PProcessPoint = ^TProcessPoint;
  TProcessPoint = packed record
    FromStart: Int64;        // ���-�� ����� �� ������
    CPUTime: UInt64;         // ������� ����� CPU
    case PointType: TPointType of
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

  // ��������� �������� ������ � ������������ ��������.
  // ������������ ����������, �������� ������������ ����� �������� DebugProcessData
  // �������� !!!
  // ������ ���������� ����� �������������� ������������� ������ �� ������,
  // �� ���������� ������ ��������� � ������ ��������� ����� CloseHandle()

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
    CPUEllapsed: UInt64; // ����� ������������� CPU
    DbgPoints: TProcessPointList;
    DbgGetMemInfo: TGetMemInfo; // ��������� � ����������
    DbgGetMemInfoSize: Cardinal;

    CreatedProcessHandle: THandle;
    CreatedThreadHandle: THandle;
    AttachedProcessHandle: THandle;
    AttachedThreadHandle: THandle;
    AttachedFileHandle: THandle;

    //DbgShareMem: THandle; // FileMap ��� �������������� � ���������

    function Ellapsed_MSec: Cardinal; // msec
    function DbgPointsCount: Cardinal;
    function DbgPointByIdx(const Idx: Cardinal): PProcessPoint;
    function CurDbgPointIdx: Cardinal;
    procedure Clear;
    procedure SetPEImage(APEImage: TJclPeImage);
  end;

  THardwareBreakpointEvent = procedure(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord;
    BreakPointIndex: THWBPIndex; var ReleaseBreakpoint: Boolean) of object;

  TDbgState = (dsNone, dsStarted, dsWait, dsPerfomance, dsTrace, dsEvent, dsStoping, dsStoped, dsDbgFail);

procedure RaiseDebugCoreException(const Msg: String = '');

implementation

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
end;

function TProcessData.CurDbgPointIdx: Cardinal;
begin
  Result := 0;
  if Assigned(DbgPoints) and (DbgPoints.Count > 0) then
    Result := DbgPoints.Count - 1
  else
    RaiseDebugCoreException();
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
    QueryPerformanceCounter(Cur)
  else
    Cur := Ellapsed;

  QueryPerformanceFrequency(Freq);
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

  //FreeAndNil(DbgMemInfo);
  FreeAndNil(DbgGetMemInfo);
  FreeMemory(Context);

  ThreadAdvInfo := Nil;
end;

function TThreadData.DbgMemInfoByIdx(const Idx: Cardinal): PThreadPoint;
begin
//  if Idx < DbgMemInfo.Count then
//    Result := DbgMemInfo[Idx]
//  else
//    Result := Nil;
end;

function TThreadData.DbgMemInfoCount: Cardinal;
begin
//  Result := DbgMemInfo.Count;
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

{ TThreadPoint }

procedure TThreadPoint.Clear;
begin
  if PointType = ptException then
    FreeAndNil(Stack);
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

end.
