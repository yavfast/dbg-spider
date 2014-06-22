unit DbgMemoryProfiler;

interface

uses System.Classes, WinApi.Windows, Collections.Queues, DbgHookTypes,
  System.SysUtils, System.SyncObjs, DebugerTypes;

type
  TProcessMemoryQueue = TQueue<PDbgMemInfoListBuf>;

  TDbgMemoryProfiler = class
  private
    FProcessMemoryQueue: TProcessMemoryQueue;

    FMemoryCheckMode: LongBool;
    FMemoryCallStack: LongBool;
    FMemoryCheckDoubleFree: LongBool;

    procedure SetMemoryCallStack(const Value: LongBool);
    procedure SetMemoryCheckDoubleFree(const Value: LongBool);
    procedure SetMemoryCheckMode(const Value: LongBool);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function ProcessMemoryInfoQueue: LongBool;
    procedure ProcessMemoryInfoBuf(const Buf: PDbgMemInfoListBuf);

    function FindMemoryPointer(const Ptr: Pointer; var ThData: PThreadData; var MemInfo: TGetMemInfo): LongBool;

    procedure LoadMemoryInfoPackEx(const MemInfoPack: Pointer; const Count: Cardinal);

    procedure UpdateMemoryInfoObjectTypes;
    procedure UpdateMemoryInfoObjectTypesOfThread(ThData: PThreadData);

    property MemoryCheckMode: LongBool read FMemoryCheckMode write SetMemoryCheckMode;
    property MemoryCallStack: LongBool read FMemoryCallStack write SetMemoryCallStack;
    property MemoryCheckDoubleFree: LongBool read FMemoryCheckDoubleFree write SetMemoryCheckDoubleFree;
  end;

implementation

uses Debuger;

const
  _MAX_MEM_INFO_BUF_COUNT = 512;


{ TDbgMemoryProfiler }

procedure TDbgMemoryProfiler.Clear;
begin
  FProcessMemoryQueue.Clear;
end;

constructor TDbgMemoryProfiler.Create;
begin
  inherited;

  FProcessMemoryQueue := TProcessMemoryQueue.Create(True);
  FProcessMemoryQueue.Capacity := _MAX_MEM_INFO_BUF_COUNT + 1;
end;

destructor TDbgMemoryProfiler.Destroy;
begin
  Clear;

  FreeAndNil(FProcessMemoryQueue);

  inherited;
end;

function TDbgMemoryProfiler.FindMemoryPointer(const Ptr: Pointer; var ThData: PThreadData; var MemInfo: TGetMemInfo): LongBool;
var
  Idx: Integer;
begin
  Result := False;

  // Ищем в текущем потоке
  if ThData <> Nil then
    Result := ThData^.DbgGetMemInfo.TryGetValue(Ptr, MemInfo);

  if not Result then
  begin
    // Ищем в других потоках
    Idx := 0;
    repeat
      ThData := gvDebuger.GetThreadDataByIdx(Idx);
      if ThData <> Nil then
      begin
        Result := ThData^.DbgGetMemInfo.TryGetValue(Ptr, MemInfo);

        Inc(Idx);
      end;
    until Result or (ThData = Nil);
  end;
end;

procedure TDbgMemoryProfiler.LoadMemoryInfoPackEx(const MemInfoPack: Pointer; const Count: Cardinal);
var
  Buf: PDbgMemInfoListBuf;
begin
  if not MemoryCheckMode then
    Exit;

  while FProcessMemoryQueue.Count >= _MAX_MEM_INFO_BUF_COUNT do
    SwitchToThread;

  Buf := AllocMem(SizeOf(TDbgMemInfoListBuf));
  Buf^.Count := Count;
  Buf^.DbgMemInfoList := AllocMem(Count * SizeOf(TDbgMemInfo));
  Buf^.DbgPointIdx := gvDebuger.ProcessData.CurDbgPointIdx;

  if gvDebuger.ReadData(MemInfoPack, Buf^.DbgMemInfoList, Count * SizeOf(TDbgMemInfo)) then
    FProcessMemoryQueue.Enqueue(Buf)
  else
    RaiseDebugCoreException();
end;

procedure TDbgMemoryProfiler.ProcessMemoryInfoBuf(const Buf: PDbgMemInfoListBuf);
var
  Idx: Integer;
  DbgMemInfo: PDbgMemInfo;
  ThData: PThreadData;
  FoundThData: PThreadData;
  MemInfo: TGetMemInfo;
  NewMemInfo: TGetMemInfo;
begin
  ThData := Nil;

  for Idx := 0 to Buf^.Count - 1 do
  begin
    DbgMemInfo := @Buf^.DbgMemInfoList^[Idx];
    if (ThData = Nil) or (ThData^.ThreadID <> DbgMemInfo^.ThreadId) then
      ThData := gvDebuger.GetThreadData(DbgMemInfo^.ThreadId, True);

    if ThData = Nil then
      RaiseDebugCoreException();

    case DbgMemInfo^.MemInfoType of
      miGetMem:
      begin
        //DoDbgLog(DbgMemInfo^.ThreadId, Format('%s: %p (%d)', ['GetMem', DbgMemInfo^.Ptr, DbgMemInfo^.Size]));

        // Если найден ещё неосвобожденный объект, то он уже был кем-то освобожден
        // TODO: Если есть такие объекты, то это мы где-то пропустили FreeMem
        (*
        FoundThData := ThData;
        if FindMemoryPointer(DbgMemInfo^.Ptr, FoundThData, MemInfo) then
        begin
          //DoDbgLog(FoundThData^.ThreadId, Format('<<< ERROR!!! FOUND BEFORE GETMEM (%d)', [MemInfo^.Size]));

          Dec(FoundThData^.DbgGetMemInfoSize, MemInfo^.Size);

          Dec(FProcessData.ProcessGetMemCount);
          Dec(FProcessData.ProcessGetMemSize, MemInfo^.Size);

          FoundThData^.DbgGetMemInfo.Remove(DbgMemInfo^.Ptr);
        end;
        *)

        // Добавляем инфу про новый объект
        NewMemInfo := TGetMemInfo.Create;

        NewMemInfo.PerfIdx := Buf^.DbgPointIdx;
        NewMemInfo.ObjAddr := DbgMemInfo^.Ptr;
        NewMemInfo.Size := DbgMemInfo^.Size;
        NewMemInfo.ObjectType := ''; // На этот момент тип ещё может быть неопределен

        NewMemInfo.LoadStack(@DbgMemInfo^.Stack);

        ThData^.DbgGetMemInfo.AddOrSetValue(DbgMemInfo^.Ptr, NewMemInfo);
        TInterlocked.Add(ThData^.DbgGetMemInfoSize, NewMemInfo.Size);

        TInterlocked.Add(gvDebuger.ProcessData.ProcessGetMemCount, 1);
        TInterlocked.Add(gvDebuger.ProcessData.ProcessGetMemSize, NewMemInfo.Size);
      end;
      miFreeMem:
      begin
        //DoDbgLog(DbgMemInfo^.ThreadId, Format('%s: %p (%d)', ['FreeMem', DbgMemInfo^.Ptr, DbgMemInfo^.Size]));

        FoundThData := ThData;
        if FindMemoryPointer(DbgMemInfo^.Ptr, FoundThData, MemInfo) then
        begin
          TInterlocked.Add(FoundThData^.DbgGetMemInfoSize, -MemInfo.Size);

          TInterlocked.Add(gvDebuger.ProcessData.ProcessGetMemCount, -1);
          TInterlocked.Add(gvDebuger.ProcessData.ProcessGetMemSize, -MemInfo.Size);

          FoundThData^.DbgGetMemInfo.Remove(DbgMemInfo^.Ptr);
        end
        else
        begin
          // Сюда может зайти, если объект создался раньше установки хука на менеджер памяти
          //RaiseDebugCoreException();
          //DoDbgLog(DbgMemInfo^.ThreadId, '<<< ERROR!!! NOT FOUND FOR FREEMEM');

          // TODO: Double free ???
        end;
      end;
    end;
  end;
end;

function TDbgMemoryProfiler.ProcessMemoryInfoQueue: LongBool;
var
  Buf: PDbgMemInfoListBuf;
begin
  Result := False;

  if not MemoryCheckMode then
    Exit;

  try
    if FProcessMemoryQueue.Count > 0 then
    begin
      Buf := FProcessMemoryQueue.Dequeue;
      try
        ProcessMemoryInfoBuf(Buf);
      finally
        FreeMemory(Buf^.DbgMemInfoList);
        FreeMemory(Buf);
      end;

      Result := True;
    end;
  except
    on E: Exception do ; // TODO:
  end;
end;

procedure TDbgMemoryProfiler.SetMemoryCallStack(const Value: LongBool);
begin
  FMemoryCallStack := Value;
end;

procedure TDbgMemoryProfiler.SetMemoryCheckDoubleFree(const Value: LongBool);
begin
  FMemoryCheckDoubleFree := Value;
end;

procedure TDbgMemoryProfiler.SetMemoryCheckMode(const Value: LongBool);
begin
  FMemoryCheckMode := Value;
end;

procedure TDbgMemoryProfiler.UpdateMemoryInfoObjectTypes;
var
  Idx: Integer;
  ThData: PThreadData;
begin
  Idx := 0;
  repeat
    ThData := gvDebuger.GetThreadDataByIdx(Idx);
    if ThData <> Nil then
    begin
      UpdateMemoryInfoObjectTypesOfThread(ThData);
      Inc(Idx);
    end;
  until ThData = Nil;

  // Потеряшки
  (*
  GetMemInfo := ProcessData.DbgGetMemInfo;
  if GetMemInfo.Count > 0 then
  begin
    for GetMemInfoItem in GetMemInfo do
      GetMemInfoItem.Value^.ObjectType := GetMemInfoItem.Value^.GetObjectType(GetMemInfoItem.Key);
  end;
  *)
end;

procedure TDbgMemoryProfiler.UpdateMemoryInfoObjectTypesOfThread(ThData: PThreadData);
var
  GetMemInfo: TGetMemInfoList;
  GetMemInfoItem: TGetMemInfoItem;
begin
  GetMemInfo := ThData^.DbgGetMemInfo;
  if GetMemInfo.Count > 0 then
  begin
    GetMemInfo.LockForRead;
    try
      for GetMemInfoItem in GetMemInfo do
        GetMemInfoItem.Value.CheckObjectType;
    finally
      GetMemInfo.UnLockForRead;
    end;
  end;
end;

end.
