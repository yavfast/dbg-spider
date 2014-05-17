unit DbgHookPerf;

interface

procedure InitPerfomance(Delta: Cardinal); stdcall;
procedure ResetPerfomance; stdcall;

implementation

uses Windows, Classes, SysUtils, DbgHookTypes, DbgHookMemory, DbgHookSyncObjs, DbgHookUtils,
  DbgHookCS;

var
  _Delta: Cardinal = 0;

  _TimerQueue: THandle = 0;
  _OutDbgInfoTimer: THandle = 0;
  _SamplingTimer: THandle = 0;

  _IsSetOutDbgInfoThreadName: Boolean = False;
  _DbgInfoPerfomance: NativeUInt = NativeUInt(dstPerfomance);
  _DbgOutLock: TDbgCriticalSection = Nil;

procedure _OutDbgInfo(Context: Pointer; Success: Boolean); stdcall;
const
  _DBG_THREAD_NAME = '### DbgInfo control thread';
begin
  if _DbgOutLock.TryEnter then // »гнорим событи€, если не успеваем их отрабатывать
    try
      if not _IsSetOutDbgInfoThreadName then
      begin
        _IsSetOutDbgInfoThreadName := True;
        TThread.NameThreadForDebugging(_DBG_THREAD_NAME, GetCurrentThreadId);
      end;

      // —брос буфера по пам€ти
      if not _OutMemInfoBuf(dstPerfomanceAndInfo) then
        RaiseException(DBG_EXCEPTION, 0, 1, @_DbgInfoPerfomance);

      // —брос буфера по локам
      // TODO: —оединить с предыдущей операцией
      if SyncObjsHooked then
        _OutSyncObjsInfo;
    finally
      _DbgOutLock.Leave;
    end;
end;

procedure InitPerfomance(Delta: Cardinal); stdcall;
begin
  _Delta := Delta;

  _DbgOutLock := TDbgCriticalSection.Create;

  _TimerQueue := CreateTimerQueue;
  if _TimerQueue <> 0 then
  begin
    if CreateTimerQueueTimer(_OutDbgInfoTimer, _TimerQueue, @_OutDbgInfo, nil, _Delta, _Delta, WT_EXECUTEINTIMERTHREAD or WT_EXECUTEINPERSISTENTTHREAD) then
      _Log(Format('Init perfomance timer (%d msec) - ok', [_Delta]))
    else
      _Log(Format('Init perfomance timer (%d msec) - fail', [_Delta]));
  end
  else
    _Log('Init timer queue - fail');
end;

procedure ResetPerfomance; stdcall;
begin
  try
    if DeleteTimerQueue(_TimerQueue) then
      _Log('Reset perfomance timer queue - ok')
    else
      _Log('Reset perfomance timer queue - fail');

    FreeAndNil(_DbgOutLock);
  except
    on E: Exception do
      _Log('Reset perfomance thread fail: ' + E.Message);
  end;
end;

end.
