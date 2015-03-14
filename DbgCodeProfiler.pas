unit DbgCodeProfiler;

interface

uses System.Classes, WinApi.Windows, Collections.Queues, DbgHookTypes,
  System.SysUtils, System.SyncObjs, DebugerTypes;

type
  TTrackPointInfo = class
    ThData: PThreadData;
    Address: Pointer;
    TrackRETBp: PTrackRETBreakpoint;
    CurThTime: UInt64;
    IsRetTrackPoint: LongBool;
    ParentFuncAddr: Pointer;
    FuncInfo: TObject;
  end;

  TTrackPointInfoQueue = class(TQueue<TTrackPointInfo>);

  TDbgCodeProfiler = class
  private
    FTrackPointInfoQueue: TTrackPointInfoQueue;
    FTrackPointInfoQueueEvent: TEvent;

    FWorkerThread: TThread;

    DbgTrackBreakpoints: TTrackBreakpointList;
    DbgTrackRETBreakpoints: TTrackRETBreakpointList;
  public
    DbgCurTrackAddress: Pointer;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure SetTrackBreakpoint(const Address: Pointer; FuncInfo: TObject; const BPType: TTrackBreakpointType = tbTrackFunc);
    function SetTrackRETBreakpoint(const Address: Pointer): PTrackRETBreakpoint;

    procedure RemoveTrackBreakpoint(const Address: Pointer; const BPType: TTrackBreakpointType = tbTrackFunc);
    function ProcessTrackBreakPoint(DebugEvent: PDebugEvent): LongBool;
    function ProcessTrackRETBreakPoint(DebugEvent: PDebugEvent): LongBool;

    procedure RegisterRETTrackPoint(ThData: PThreadData; Address: Pointer; TrackRETBp: PTrackRETBreakpoint; const CurThTime: UInt64);
    procedure RegisterTrackPoint(ThData: PThreadData; Address, ParentFuncAddr: Pointer; FuncInfo: TObject; TrackRETBp: PTrackRETBreakpoint; const CurThTime: UInt64);

    procedure InitDbgTracking(const Capacity: Integer);
    procedure ClearDbgTracking;

    procedure StartWorkerThread;
    procedure StopWorkerThread;

    property TrackPointInfoQueue: TTrackPointInfoQueue read FTrackPointInfoQueue;
    property TrackPointInfoQueueEvent: TEvent read FTrackPointInfoQueueEvent;
  end;

  TDbgCodeProfilerWorkerThread = class(TThread)
  private
    FOwner: TDbgCodeProfiler;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TDbgCodeProfiler);
  end;

implementation

uses
  DebugInfo, WinAPIUtils, Debuger, System.Contnrs;

{ TDbgCodeProfiler }

procedure TDbgCodeProfiler.Clear;
begin
  ClearDbgTracking;
end;

procedure TDbgCodeProfiler.ClearDbgTracking;
begin
  StopWorkerThread;

  if Assigned(DbgTrackBreakpoints) then
  begin
    DbgTrackBreakpoints.Clear;
    FreeAndNil(DbgTrackBreakpoints);
  end;

  if Assigned(DbgTrackRETBreakpoints) then
  begin
    DbgTrackRETBreakpoints.Clear;
    FreeAndNil(DbgTrackRETBreakpoints);
  end;

  if Assigned(FTrackPointInfoQueue) then
  begin
    FTrackPointInfoQueue.Clear;
    //FreeAndNil(FTrackPointInfoQueue);
  end;
end;

constructor TDbgCodeProfiler.Create;
begin
  inherited;

  FTrackPointInfoQueue := TTrackPointInfoQueue.Create(4096, True);
  FTrackPointInfoQueueEvent := TEvent.Create(nil, False, False, '');
end;

destructor TDbgCodeProfiler.Destroy;
begin
  Clear;

  FreeAndNil(FTrackPointInfoQueue);
  FreeAndNil(FTrackPointInfoQueueEvent);

  inherited;
end;

procedure TDbgCodeProfiler.InitDbgTracking(const Capacity: Integer);
begin
  DbgTrackBreakpoints := TTrackBreakpointList.Create(Capacity * 2);
  DbgTrackBreakpoints.OwnsValues := True;

  DbgTrackRETBreakpoints := TTrackRETBreakpointList.Create(Capacity * 2);
  DbgTrackRETBreakpoints.OwnsValues := True;

  StartWorkerThread;
end;

procedure TDbgCodeProfiler.RegisterTrackPoint(ThData: PThreadData; Address, ParentFuncAddr: Pointer; FuncInfo: TObject;
  TrackRETBp: PTrackRETBreakpoint; const CurThTime: UInt64);
var
  TrackFuncInfo: TCodeTrackFuncInfo;
  ParentCallFuncInfo: TCallFuncInfo;
  ParentFuncInfo: TFuncInfo;
  ParentTrackFuncInfo: TCodeTrackFuncInfo;

  TrackStackPoint: PTrackStackPoint;
begin
  // --- Регистрируем вызываемую функцию в текущем потоке --- //
  Inc(ThData^.DbgTrackEventCount);

  TrackFuncInfo := TCodeTrackFuncInfo(ThData^.DbgTrackFuncList.GetTrackFuncInfo(FuncInfo));
  ThData^.DbgTrackUnitList.CheckTrackFuncInfo(TrackFuncInfo);

  TrackFuncInfo.IncCallCount;
  TrackFuncInfo.TrackUnitInfo.IncCallCount;

  // Добавляем линк с текущей функции на родительскую
  ParentCallFuncInfo := TrackFuncInfo.AddParentCall(ParentFuncAddr);

  // Добавляем линк с родительской функции на текущую
  ParentTrackFuncInfo := nil;

  if Assigned(ParentCallFuncInfo) then
  begin
    ParentFuncInfo := TFuncInfo(ParentCallFuncInfo.FuncInfo);
    if Assigned(ParentFuncInfo) then
    begin
      ParentTrackFuncInfo := TCodeTrackFuncInfo(ThData^.DbgTrackFuncList.GetTrackFuncInfo(ParentFuncInfo));
      ThData^.DbgTrackUnitList.CheckTrackFuncInfo(ParentTrackFuncInfo);

      ParentTrackFuncInfo.AddChildCall(Address);
    end;
  end;

  // Добавляем в Track Stack
  TrackStackPoint := ThData^.DbgTrackStack.Push;

  TrackStackPoint^.TrackFuncInfo := TrackFuncInfo;
  TrackStackPoint^.ParentTrackFuncInfo := ParentTrackFuncInfo;
  TrackStackPoint^.TrackRETBreakpoint := TrackRETBp; // Только для сравнения по указателю
  TrackStackPoint^.Enter := CurThTime;
  TrackStackPoint^.Elapsed := 0;

  // --- Регистрируем вызываемую функцию в процессе --- //
  TInterlocked.Increment(gvDebuger.ProcessData.DbgTrackEventCount);

  TrackFuncInfo := TCodeTrackFuncInfo(gvDebuger.ProcessData.DbgTrackFuncList.GetTrackFuncInfo(FuncInfo));
  gvDebuger.ProcessData.DbgTrackUnitList.CheckTrackFuncInfo(TrackFuncInfo);

  TrackFuncInfo.IncCallCount;
  TrackFuncInfo.TrackUnitInfo.IncCallCount;

  // Добавляем линк с текущей функции на родительскую
  ParentCallFuncInfo := TrackFuncInfo.AddParentCall(ParentFuncAddr);

  // Добавляем линк с родительской функции на текущую
  ParentTrackFuncInfo := nil;

  if Assigned(ParentCallFuncInfo) then
  begin
    ParentFuncInfo := TFuncInfo(ParentCallFuncInfo.FuncInfo);
    if Assigned(ParentFuncInfo) then
    begin
      ParentTrackFuncInfo := TCodeTrackFuncInfo(gvDebuger.ProcessData.DbgTrackFuncList.GetTrackFuncInfo(ParentFuncInfo));
      gvDebuger.ProcessData.DbgTrackUnitList.CheckTrackFuncInfo(ParentTrackFuncInfo);

      ParentTrackFuncInfo.AddChildCall(Address);
    end;
  end;

  // Записываем инфу для процесса
  TrackStackPoint^.ProcTrackFuncInfo := TrackFuncInfo;
  TrackStackPoint^.ProcParentTrackFuncInfo := ParentTrackFuncInfo;
end;

function TDbgCodeProfiler.ProcessTrackBreakPoint(DebugEvent: PDebugEvent): LongBool;
var
  ThData: PThreadData;
  Address: Pointer;
  TrackBp: PTrackBreakpoint;
  ParentFuncAddr: Pointer;
  TrackRETBreakpoint: PTrackRETBreakpoint;

  procedure _RegisterTrackPoint;
  var
    TrackPointInfo: TTrackPointInfo;
  begin
    TrackPointInfo := TTrackPointInfo.Create;

    TrackPointInfo.ThData := ThData;
    TrackPointInfo.Address := Address;
    TrackPointInfo.TrackRETBp := TrackRETBreakpoint;
    TrackPointInfo.CurThTime := _QueryThreadCycleTime(ThData^.ThreadHandle);
    TrackPointInfo.IsRetTrackPoint := False;
    TrackPointInfo.ParentFuncAddr := ParentFuncAddr;
    TrackPointInfo.FuncInfo := TrackBp^.FuncInfo;

    FTrackPointInfoQueue.Add(TrackPointInfo);
    FTrackPointInfoQueueEvent.SetEvent;
  end;

  procedure _RegisterFreeMemInfoPoint;
  var
    FuncInfo: TFuncInfo;
    MemInfo: TGetMemInfo;
    Param: TVarInfo;
    Addr: Pointer;
  begin
    FuncInfo := TFuncInfo(TrackBp^.FuncInfo);
    if (gvDebugInfo.MemoryManagerInfo.FreeMem = FuncInfo) or
      (gvDebugInfo.MemoryManagerInfo.ReallocMem = FuncInfo)
    then
    begin
      Param := TVarInfo(FuncInfo.Params[0]);
      Addr := Pointer(Integer(Param.Value));

      if gvDebuger.DbgMemoryProfiler.FindMemoryPointer(Addr, ThData, MemInfo) then
      begin
        Dec(ThData^.DbgGetMemInfoSize, MemInfo.Size);

        Dec(gvDebuger.ProcessData.ProcessGetMemCount);
        Dec(gvDebuger.ProcessData.ProcessGetMemSize, MemInfo.Size);

        ThData^.DbgGetMemInfo.Remove(Addr);
      end;
    end;
  end;

begin
  if gvDebuger.UpdateCurThreadContext then
  begin
    ThData := gvDebuger.CurThreadData;

    Address := DebugEvent^.Exception.ExceptionRecord.ExceptionAddress;
    if DbgTrackBreakpoints.TryGetValue(Address, TrackBp) then
    begin
      // Получаем адресс выхода в родительской функции
      ParentFuncAddr := nil;
      Check(gvDebuger.ReadData(Pointer(ThData^.Context^.Esp), @ParentFuncAddr, SizeOf(Pointer)));

      // Устанавливаем точку останова на выход
      TrackRETBreakpoint := SetTrackRETBreakpoint(ParentFuncAddr);
      TrackRETBreakpoint^.FuncInfo := TrackBp^.FuncInfo;
      TrackRETBreakpoint^.BPType := TrackBp^.BPType;

      // Восстанавливаем Code byte для продолжения выполнения
      DbgCurTrackAddress := Address;
      gvDebuger.RemoveBreakpoint(Address, TrackBp^.SaveByte);
      gvDebuger.SetSingleStepMode(ThData, True);

      // --- Регистрация --- //
      if tbTrackFunc in TrackBp^.BPType then
        _RegisterTrackPoint;

      if tbMemInfo in TrackBp^.BPType then
        _RegisterFreeMemInfoPoint;

      // Выходим с признаком успешной регистрации
      Exit(True);
    end;
  end;

  // Это не Track Breakpoint
  Exit(False);
end;

procedure TDbgCodeProfiler.RegisterRETTrackPoint(ThData: PThreadData; Address: Pointer; TrackRETBp: PTrackRETBreakpoint; const CurThTime: UInt64);
var
  TrackStackPoint: PTrackStackPoint;
  FuncAddress: Pointer;
  CallFuncInfo: TCallFuncInfo;
begin
  // Обработка Track-стека текущего потока
  while ThData^.DbgTrackStack.Count > 0 do
  begin
    TrackStackPoint := ThData^.DbgTrackStack.Pop;

    // Увеличиваем счетчик самой функции
    TrackStackPoint^.Leave := CurThTime;
    // Thread
    TrackStackPoint^.TrackFuncInfo.GrowElapsed(TrackStackPoint^.Elapsed);
    // Proc
    TrackStackPoint^.ProcTrackFuncInfo.GrowElapsed(TrackStackPoint^.Elapsed);

    // Увеличиваем счетчик родителя
    // Thread
    if TrackStackPoint^.TrackFuncInfo.ParentFuncs.TryGetValue(Address, CallFuncInfo) then
      Inc(CallFuncInfo.Data, TrackStackPoint^.Elapsed);

    // Proc
    if TrackStackPoint^.ProcTrackFuncInfo.ParentFuncs.TryGetValue(Address, CallFuncInfo) then
      Inc(CallFuncInfo.Data, TrackStackPoint^.Elapsed);

    // Увеличиваем свой счетчик у родителя
    // Thread
    if Assigned(TrackStackPoint^.ParentTrackFuncInfo) then
    begin
      FuncAddress := TFuncInfo(TrackStackPoint^.TrackFuncInfo.FuncInfo).Address;
      if TrackStackPoint^.ParentTrackFuncInfo.ChildFuncs.TryGetValue(FuncAddress, CallFuncInfo) then
        Inc(CallFuncInfo.Data, TrackStackPoint^.Elapsed);
    end;

    // Proc
    if Assigned(TrackStackPoint^.ProcParentTrackFuncInfo) then
    begin
      FuncAddress := TFuncInfo(TrackStackPoint^.ProcTrackFuncInfo.FuncInfo).Address;
      if TrackStackPoint^.ProcParentTrackFuncInfo.ChildFuncs.TryGetValue(FuncAddress, CallFuncInfo) then
        Inc(CallFuncInfo.Data, TrackStackPoint^.Elapsed);
    end;

    // Если это вершина стека - выходим
    if TrackStackPoint^.TrackRETBreakpoint = TrackRETBp then
    begin
      // Dec(TrackRETBp.Count);
      Break;
    end;
  end;
end;

function TDbgCodeProfiler.ProcessTrackRETBreakPoint(DebugEvent: PDebugEvent): LongBool;
var
  ThData: PThreadData;
  Address: Pointer;
  TrackRETBp: PTrackRETBreakpoint;

  procedure _RegisterRETTrackPoint;
  var
    TrackPointInfo: TTrackPointInfo;
  begin
    TrackPointInfo := TTrackPointInfo.Create;

    TrackPointInfo.ThData := ThData;
    TrackPointInfo.Address := Address;
    TrackPointInfo.TrackRETBp := TrackRETBp;
    TrackPointInfo.CurThTime := _QueryThreadCycleTime(ThData^.ThreadHandle);
    TrackPointInfo.IsRetTrackPoint := True;

    FTrackPointInfoQueue.Add(TrackPointInfo);
    FTrackPointInfoQueueEvent.SetEvent;
  end;

  procedure _RegisterGetMemInfoPoint;
  var
    FuncInfo: TFuncInfo;
    ParamSize: TVarInfo;
    ParamAddr: TVarInfo;
    Addr: Pointer;
    Size: Cardinal;
    NewMemInfo: TGetMemInfo;
  begin
    FuncInfo := TFuncInfo(TrackRETBp^.FuncInfo);

    // Dec(TrackRETBp^.Count);

    ParamAddr := nil;
    ParamSize := nil;

    if (gvDebugInfo.MemoryManagerInfo.GetMem = FuncInfo) or
      (gvDebugInfo.MemoryManagerInfo.AllocMem = FuncInfo)
    then
      begin
        // GetMem: function(Size: NativeInt): Pointer;
        // AllocMem: function(Size: NativeInt): Pointer;

        ParamSize := TVarInfo(FuncInfo.Params[0]);

        ParamAddr := TVarInfo.Create;
        ParamAddr.DataType := FuncInfo.ResultType;
        ParamAddr.VarKind := vkRegister;
      end
    else
    if (gvDebugInfo.MemoryManagerInfo.ReallocMem = FuncInfo)
    then
      begin
        // ReallocMem: function(P: Pointer; Size: NativeInt): Pointer;

        ParamSize := TVarInfo(FuncInfo.Params[1]);

        ParamAddr := TVarInfo.Create;
        ParamAddr.DataType := FuncInfo.ResultType;
        ParamAddr.VarKind := vkRegister;
      end;

    if Assigned(ParamSize) and Assigned(ParamAddr) then
    begin
      Size := 1; //ParamSize.Value;
      Addr := Pointer(Integer(ParamAddr.Value));

      FreeAndNil(ParamAddr);

      // Добавляем инфу про новый объект
      //NewMemInfo := AllocMem(SizeOf(RGetMemInfo));
      NewMemInfo := TGetMemInfo.Create;

      NewMemInfo.PerfIdx := gvDebuger.ProcessData.CurDbgPointIdx;
      NewMemInfo.ObjAddr := Addr;
      NewMemInfo.Size := Size;

      //NewMemInfo^.Stack := DbgMemInfo^.Stack;
      NewMemInfo.Stack[0] := nil;

      NewMemInfo.ObjectType := ''; // На этот момент тип ещё может быть неопределен

      ThData^.DbgGetMemInfo.AddOrSetValue(Addr, NewMemInfo);
      Inc(ThData^.DbgGetMemInfoSize, NewMemInfo.Size);

      Inc(gvDebuger.ProcessData.ProcessGetMemCount);
      Inc(gvDebuger.ProcessData.ProcessGetMemSize, NewMemInfo.Size);
    end;
  end;

begin
  if gvDebuger.UpdateCurThreadContext then
  begin
    ThData := gvDebuger.CurThreadData;

    Address := DebugEvent^.Exception.ExceptionRecord.ExceptionAddress;
    if DbgTrackRETBreakpoints.TryGetValue(Address, TrackRETBp) and (TrackRETBp.Count > 0){???} then
    begin
      if tbTrackFunc in TrackRETBp^.BPType then
        _RegisterRETTrackPoint;

      if tbMemInfo in TrackRETBp^.BPType then
        _RegisterGetMemInfoPoint;

      // Уменьшаем счетчик
      if TrackRETBp.Count > 0 then
        Dec(TrackRETBp.Count);

      // Восстанавливаем breakpoint в случае рекурсивного вызова функции
      if TrackRETBp.Count > 0 then
        DbgCurTrackAddress := Address;

      // Восстанавливаем byte-code для продолжения выполнения
      gvDebuger.RemoveBreakpoint(Address, TrackRETBp^.SaveByte);

      //if TrackRETBp^.Count = 0 then
      //  DbgTrackRETBreakpoints.Remove(Address);

      gvDebuger.SetSingleStepMode(ThData, True);

      Exit(True);
    end;
  end;

  Exit(False);
end;

procedure TDbgCodeProfiler.RemoveTrackBreakpoint(const Address: Pointer; const BPType: TTrackBreakpointType);
var
  TrackBp: PTrackBreakpoint;
begin
  if DbgTrackBreakpoints.TryGetValue(Address, TrackBp) then
  begin
    Exclude(TrackBp^.BPType, BPType);

    if TrackBp^.BPType = [] then
    begin
      gvDebuger.RemoveBreakpoint(Address, TrackBp^.SaveByte);
      //DbgTrackBreakpoints.Remove(Address);
    end;
  end
  else
    RaiseDebugCoreException();
end;

procedure TDbgCodeProfiler.SetTrackBreakpoint(const Address: Pointer; FuncInfo: TObject; const BPType: TTrackBreakpointType);
var
  TrackBk: PTrackBreakpoint;
begin
  if not DbgTrackBreakpoints.TryGetValue(Address, TrackBk) then
  begin
    TrackBk := AllocMem(SizeOf(TTrackBreakpoint));

    TrackBk^.FuncInfo := FuncInfo;
    TrackBk^.SaveByte := 0;

    TrackBk^.BPType := [];
    Include(TrackBk^.BPType, BPType);

    gvDebuger.SetBreakpoint(Address, TrackBk^.SaveByte);

    DbgTrackBreakpoints.Add(Address, TrackBk);
  end
  else
    Include(TrackBk^.BPType, BPType);
end;

function TDbgCodeProfiler.SetTrackRETBreakpoint(const Address: Pointer): PTrackRETBreakpoint;
begin
  if DbgTrackRETBreakpoints.TryGetValue(Address, Result) then
  begin
    Inc(Result^.Count);

    gvDebuger.RestoreBreakpoint(Address);
  end
  else
  begin
    Result := AllocMem(SizeOf(TTrackRETBreakpoint));

    Result^.Count := 1;

    Result^.SaveByte := 0;
    gvDebuger.SetBreakpoint(Address, Result^.SaveByte);

    Result^.BPType := [];

    DbgTrackRETBreakpoints.Add(Address, Result);
  end;
end;

procedure TDbgCodeProfiler.StartWorkerThread;
begin
  if FWorkerThread = Nil then
    FWorkerThread := TDbgCodeProfilerWorkerThread.Create(Self);
end;

procedure TDbgCodeProfiler.StopWorkerThread;
begin
  if Assigned(FWorkerThread) then
  begin
    FWorkerThread.Terminate;
    FTrackPointInfoQueueEvent.SetEvent;

    FWorkerThread.WaitFor;
    FreeAndNil(FWorkerThread);
  end;
end;

{ TDbgCodeProfilerWorkerThread }

constructor TDbgCodeProfilerWorkerThread.Create(AOwner: TDbgCodeProfiler);
begin
  inherited Create(False);
  FreeOnTerminate := False;

  FOwner := AOwner;
end;

procedure TDbgCodeProfilerWorkerThread.Execute;
var
  TrackPointInfo: TTrackPointInfo;
begin
  while not Terminated do
  begin
    FOwner.TrackPointInfoQueueEvent.WaitFor;

    while not Terminated and (FOwner.TrackPointInfoQueue.Count > 0) do
    begin
      TrackPointInfo := FOwner.TrackPointInfoQueue.Dequeue;
      try
        with TrackPointInfo do
        begin
          if IsRetTrackPoint then
            FOwner.RegisterRETTrackPoint(ThData, Address, TrackRETBp, CurThTime)
          else
            FOwner.RegisterTrackPoint(ThData, Address, ParentFuncAddr, FuncInfo, TrackRETBp, CurThTime);
        end;
      finally
        FreeAndNil(TrackPointInfo);
      end;
    end;

    if not Terminated then
      Sleep(500);
  end;
end;

end.
