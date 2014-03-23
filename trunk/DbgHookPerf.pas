unit DbgHookPerf;

interface

procedure InitPerfomance(Delta: Cardinal); stdcall;
procedure ResetPerfomance; stdcall;

implementation

uses Windows, Classes, SysUtils, DbgHookTypes, DbgHookMemory, DbgHookSyncObjs, DbgHookUtils;

type
  TPerfThread = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

var
  _PerfThread: TPerfThread = Nil;
  _Delta: Cardinal = 0;

  _TimerQueue: THandle = 0;
  _OutDbgInfoTimer: THandle = 0;
  _SamplingTimer: THandle = 0;

var
  _IsSetOutDbgInfoThreadName: Boolean = False;
  _DbgInfoPerfomance: NativeUInt = NativeUInt(dstPerfomance);

procedure _OutDbgInfo(Context: Pointer; Success: Boolean); stdcall;
begin
  if not _IsSetOutDbgInfoThreadName then
  begin
    _IsSetOutDbgInfoThreadName := True;
    TThread.NameThreadForDebugging('### DbgInfo profiler thread', GetCurrentThreadId);
  end;

  try
    // —брос буфера по пам€ти
    if not _OutMemInfoBuf(dstPerfomanceAndInfo) then
      RaiseException(DBG_EXCEPTION, 0, 1, @_DbgInfoPerfomance);

    // —брос буфера по локам
    if SyncObjsHooked then
      _OutSyncObjsInfo;
  except
    on E: Exception do
      _Log(Format('Fail perfomance timer proc: [%s] %s', [E.ClassName, E.Message]));
  end;
end;

var
  _IsSetOutSamplingInfoThreadName: Boolean = False;
  _DbgInfoSampling: NativeUInt = NativeUInt(dstSampling);

procedure _OutSamplingInfo(Context: Pointer; Success: Boolean); stdcall;
begin
  if not _IsSetOutSamplingInfoThreadName then
  begin
    _IsSetOutSamplingInfoThreadName := True;
    TThread.NameThreadForDebugging('### DbgInfo sampling thread', GetCurrentThreadId);
  end;

  try
    RaiseException(DBG_EXCEPTION, 0, 1, @_DbgInfoSampling);
  except
    on E: Exception do
      _Log(Format('Fail sampling timer proc: [%s] %s', [E.ClassName, E.Message]));
  end;
end;

procedure InitPerfomance(Delta: Cardinal); stdcall;
begin
  _Delta := Delta;

  _TimerQueue := CreateTimerQueue;
  if _TimerQueue <> 0 then
  begin
    (*
    if CreateTimerQueueTimer(_SamplingTimer, _TimerQueue, @_OutSamplingInfo, nil, 100, 1, WT_EXECUTEDEFAULT) then
      _Log(Format('Init sampling timer (%d msec) - ok', [1]))
    else
      _Log(Format('Init sampling timer (%d msec) - fail', [1]));
    *)

    if CreateTimerQueueTimer(_OutDbgInfoTimer, _TimerQueue, @_OutDbgInfo, nil, _Delta, _Delta, WT_EXECUTEDEFAULT) then
      _Log(Format('Init perfomance timer (%d msec) - ok', [_Delta]))
    else
      _Log(Format('Init perfomance timer (%d msec) - fail', [_Delta]));
  end
  else
    _Log(Format('Init timer queue - fail', [_Delta]))

  //_PerfThread := TPerfThread.Create;
  //_Log(Format('Init perfomance thread (%d msec) - ok', [_Delta]));
end;

procedure ResetPerfomance; stdcall;
begin
  try
    if DeleteTimerQueue(_TimerQueue) then
      _Log('Reset timer queue - ok')
    else
      _Log('Reset timer queue - fail');

    (*
    if _PerfThread = nil then Exit;

    _PerfThread.FreeOnTerminate := False;
    _PerfThread.Terminate;
    _PerfThread.WaitFor;
    FreeAndNil(_PerfThread);

    _Log('Reset perfomance thread - ok');
    *)
  except
    on E: Exception do
      _Log('Reset perfomance thread fail: ' + E.Message);
  end;
end;

{ TPerfThread }

constructor TPerfThread.Create;
begin
  inherited Create(True);

  FreeOnTerminate := True;
  Priority := tpTimeCritical;

  Suspended := False;
end;

procedure TPerfThread.Execute;
var
  DbgInfo: Pointer;
begin
  NameThreadForDebugging('### Dbg control thread', GetCurrentThreadId);

  DbgInfo := AllocMem(SizeOf(Pointer)); // дл€ выравнивани€ по пам€ти
  DWORD(DbgInfo^) := DWORD(dstPerfomance);
  try
    while not Terminated do
    begin
      Sleep(_Delta);
      //RaiseException(DBG_EXCEPTION, 0, 1, DbgInfo);

      // —брос буфера по пам€ти
      //_OutMemInfoBuf;
      if not _OutMemInfoBuf(dstPerfomanceAndInfo) then
        RaiseException(DBG_EXCEPTION, 0, 1, DbgInfo);

      // —брос буфера по локам
      if SyncObjsHooked then
        _OutSyncObjsInfo;
    end;
  except
    on E: Exception do
      _Log(Format('Fail perfomance thread: [%s] %s', [E.ClassName, E.Message]));
  end;
  FreeMemory(DbgInfo);
end;

end.
