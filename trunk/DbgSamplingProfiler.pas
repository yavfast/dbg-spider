unit DbgSamplingProfiler;

interface

uses System.Classes, WinApi.Windows, Collections.Queues, DbgHookTypes,
  System.SysUtils, System.SyncObjs, DebugerTypes;

type
  TDbgSamplingProfiler = class
  private
    // Timers
    FTimerQueue: THandle;
    FSamplingTimer: THandle;
    FSamplingLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure InitSamplingTimer;
    procedure ResetSamplingTimer;
    procedure DoSamplingEvent;

    procedure ProcessDbgSamplingInfo;
    function ProcessSamplingInfo: LongBool;

    procedure AddThreadSamplingInfo(ThreadData: PThreadData);
    function ProcessThreadSamplingInfo(ThreadData: PThreadData): LongBool;
    procedure ProcessThreadSamplingStack(ThreadData: PThreadData; var Stack: TDbgInfoStack);
    procedure ProcessThreadSamplingAddress(ThData: PThreadData; FuncAddr, ParentFuncAddr: Pointer);
  end;

implementation

uses
  Debuger, WinAPIUtils, DebugInfo;

procedure _DbgSamplingEvent(Context: Pointer; Success: LongBool); stdcall;
begin
  if Assigned(gvDebuger) then
    gvDebuger.DbgSamplingProfiler.DoSamplingEvent;
end;


{ TDbgSamplingProfiler }

procedure TDbgSamplingProfiler.AddThreadSamplingInfo(ThreadData: PThreadData);
var
  ThCPU: UInt64;
  FreqCPU: Int64;
  Stack: TDbgInfoStack;
  StackInfo: PDbgInfoStackRec;
  Res: DWORD;
begin
  if Assigned(ThreadData^.ThreadAdvInfo) and (ThreadData^.ThreadAdvInfo.ThreadAdvType = tatNormal) then
  begin
    ThCPU := _QueryThreadCycleTime(ThreadData^.ThreadHandle);
    FreqCPU := _QueryPerformanceFrequency;

    // FreqCPU - количество циклов CPU за 1 сек
    // Обрабатываем только те потоки, которые получили более 10% циклов за 1 мсек
    if (ThCPU - ThreadData^.SamplingCPUTime) > (FreqCPU div 10000) then
    begin
      ThreadData^.SamplingCPUTime := ThCPU;
      Inc(ThreadData^.SamplingCount);

      SetLength(Stack, 0);

      Res := SuspendThread(ThreadData^.ThreadHandle);
      if Res = 0 then
      begin
        if gvDebuger.UpdateThreadContext(ThreadData, CONTEXT_CONTROL) then
          gvDebuger.GetCallStackEx(ThreadData, Stack);
      end;

      ResumeThread(ThreadData^.ThreadHandle);

      if Length(Stack) > 0 then
      begin
        New(StackInfo);
        StackInfo^.Stack := Stack;

        ThreadData^.SamplingQueue.Add(StackInfo);
      end;
    end
    else
      ThreadData^.SamplingCPUTime := ThCPU;
  end;
end;

procedure TDbgSamplingProfiler.Clear;
begin

end;

constructor TDbgSamplingProfiler.Create;
begin
  inherited;

  FTimerQueue := 0;
  FSamplingTimer := 0;
  FSamplingLock := TCriticalSection.Create;
end;

destructor TDbgSamplingProfiler.Destroy;
begin
  Clear;

  inherited;
end;

procedure TDbgSamplingProfiler.DoSamplingEvent;
begin
  if gvDebuger.DbgState <> dsWait then Exit;

  // Игнорим обработку, если не успели за отведенное время
  if FSamplingLock.TryEnter then
  begin
    ProcessDbgSamplingInfo;

    FSamplingLock.Leave;
  end;
end;

procedure TDbgSamplingProfiler.InitSamplingTimer;
begin
  FTimerQueue := CreateTimerQueue;
  if FTimerQueue <> 0 then
  begin
    if CreateTimerQueueTimer(FSamplingTimer, FTimerQueue, @_DbgSamplingEvent, nil, 100, 1, WT_EXECUTEINPERSISTENTTHREAD) then
    begin
      gvDebuger.Log('Init sampling timer - ok');
      Exit;
    end;
  end;

  gvDebuger.Log('Init sampling timer - fail');
end;

procedure TDbgSamplingProfiler.ProcessDbgSamplingInfo;
var
  CPUTime: UInt64;
  ThData: PThreadData;
  I: Integer;
  Threads: TDbgActiveThreads;
begin
  CPUTime := _QueryProcessCycleTime(gvDebuger.ProcessData^.AttachedProcessHandle);
  // TODO: Контроль загрузки CPU
  if CPUTime > gvDebuger.ProcessData^.SamplingCPUTime then
  begin
    gvDebuger.ProcessData^.SamplingCPUTime := CPUTime;
    TInterlocked.Increment(gvDebuger.ProcessData^.SamplingCount);

    gvDebuger.GetActiveThreads(Threads);

    for I := 0 to High(Threads) do
    begin
      ThData := Threads[I];
      if ThData^.State = tsActive then
        AddThreadSamplingInfo(ThData);
    end;
  end;
end;

function TDbgSamplingProfiler.ProcessSamplingInfo: LongBool;
var
  I: Integer;
  ThData: PThreadData;
begin
  Result := False;

  if not(gvDebuger.CodeTracking and gvDebuger.SamplingMethod) then
    Exit;

  try
    for I := gvDebuger.GetThreadCount - 1 downto 0 do
    begin
      ThData := gvDebuger.GetThreadDataByIdx(I);
      Result := ProcessThreadSamplingInfo(ThData) or Result;
    end;
  except
    on E: Exception do ; // TODO:
  end;
end;

procedure TDbgSamplingProfiler.ProcessThreadSamplingAddress(ThData: PThreadData; FuncAddr, ParentFuncAddr: Pointer);
var
  UnitInfo: TUnitInfo;
  FuncInfo: TFuncInfo;
  LineInfo: TLineInfo;

  TrackFuncInfo: TCodeTrackFuncInfo;
  ParentCallFuncInfo: TCallFuncInfo;
  ParentFuncInfo: TFuncInfo;
  ParentTrackFuncInfo: TCodeTrackFuncInfo;
begin
  // --- Регистрируем вызываемую функцию в текущем потоке --- //
  if gvDebugInfo.GetLineInfo(FuncAddr, UnitInfo, FuncInfo, LineInfo, False) = slNotFound then
    Exit;

  TrackFuncInfo := TCodeTrackFuncInfo(ThData^.DbgTrackFuncList.GetTrackFuncInfo(FuncInfo));
  ThData^.DbgTrackUnitList.CheckTrackFuncInfo(TrackFuncInfo);

  TrackFuncInfo.IncCallCount;

  // Добавление в список активных юнитов
  ThData.DbgTrackUsedUnitList.AddOrSetValue(UnitInfo, TrackFuncInfo.TrackUnitInfo);

  // Добавляем линк с текущей функции на родительскую
  ParentCallFuncInfo := TrackFuncInfo.AddParentCall(ParentFuncAddr);

  // Добавляем линк с родительской функции на текущую
  if Assigned(ParentCallFuncInfo) then
  begin
    ParentFuncInfo := TFuncInfo(ParentCallFuncInfo.FuncInfo);
    if Assigned(ParentFuncInfo) then
    begin
      ParentTrackFuncInfo := TCodeTrackFuncInfo(ThData^.DbgTrackFuncList.GetTrackFuncInfo(ParentFuncInfo));
      ThData^.DbgTrackUnitList.CheckTrackFuncInfo(ParentTrackFuncInfo);

      ParentTrackFuncInfo.AddChildCall(FuncAddr);
    end;
  end;

  // --- Регистрируем вызываемую функцию в процессе --- //
  TrackFuncInfo := TCodeTrackFuncInfo(gvDebuger.ProcessData^.DbgTrackFuncList.GetTrackFuncInfo(FuncInfo));
  gvDebuger.ProcessData^.DbgTrackUnitList.CheckTrackFuncInfo(TrackFuncInfo);

  TrackFuncInfo.IncCallCount;

  // Добавление в список активных юнитов
  gvDebuger.ProcessData^.DbgTrackUsedUnitList.AddOrSetValue(UnitInfo, TrackFuncInfo.TrackUnitInfo);

  // Добавляем линк с текущей функции на родительскую
  ParentCallFuncInfo := TrackFuncInfo.AddParentCall(ParentFuncAddr);

  // Добавляем линк с родительской функции на текущую
  if Assigned(ParentCallFuncInfo) then
  begin
    ParentFuncInfo := TFuncInfo(ParentCallFuncInfo.FuncInfo);
    if Assigned(ParentFuncInfo) then
    begin
      ParentTrackFuncInfo := TCodeTrackFuncInfo(gvDebuger.ProcessData^.DbgTrackFuncList.GetTrackFuncInfo(ParentFuncInfo));
      gvDebuger.ProcessData^.DbgTrackUnitList.CheckTrackFuncInfo(ParentTrackFuncInfo);

      ParentTrackFuncInfo.AddChildCall(FuncAddr);
    end;
  end;
end;

function TDbgSamplingProfiler.ProcessThreadSamplingInfo(ThreadData: PThreadData): LongBool;
var
  StackInfo: PDbgInfoStackRec;
begin
  if ThreadData.SamplingQueue.Count > 0 then
  begin
    try
      while ThreadData.SamplingQueue.Count > 0 do
      begin
        StackInfo := ThreadData.SamplingQueue.Dequeue;
        try
          if Length(StackInfo^.Stack) > 0 then
            ProcessThreadSamplingStack(ThreadData, StackInfo^.Stack);
        finally
          Dispose(StackInfo);
        end;
      end;
    except
      on E: Exception do ;
    end;

    Result := True;
  end
  else
    Result := False;
end;

procedure TDbgSamplingProfiler.ProcessThreadSamplingStack(ThreadData: PThreadData; var Stack: TDbgInfoStack);
var
  Idx: Integer;
  TrackUnitInfoPair: TTrackUnitInfoPair;
begin
  TInterlocked.Add(gvDebuger.ProcessData.DbgTrackEventCount, 1);
  TInterlocked.Add(ThreadData^.DbgTrackEventCount, 1);

  try
    for Idx := 0 to High(Stack) - 1 do
      ProcessThreadSamplingAddress(ThreadData, Stack[Idx], Stack[Idx + 1]);

    for TrackUnitInfoPair in ThreadData.DbgTrackUsedUnitList do
      TrackUnitInfoPair.Value.IncCallCount;

    for TrackUnitInfoPair in gvDebuger.ProcessData.DbgTrackUsedUnitList do
      TrackUnitInfoPair.Value.IncCallCount;
  finally
    SetLength(Stack, 0);
    ThreadData.DbgTrackUsedUnitList.Clear;
    gvDebuger.ProcessData.DbgTrackUsedUnitList.Clear;
  end;
end;

procedure TDbgSamplingProfiler.ResetSamplingTimer;
begin
  if FTimerQueue <> 0 then
  begin
    if DeleteTimerQueue(FTimerQueue) then
      gvDebuger.Log('Reset timer queue - ok')
    else
      gvDebuger.Log('Reset timer queue - fail');

    FSamplingTimer := 0;
    FTimerQueue := 0;
  end;
end;

end.
