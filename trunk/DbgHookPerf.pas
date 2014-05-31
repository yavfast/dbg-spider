unit DbgHookPerf;

interface

procedure InitPerfomance(Delta: Cardinal); stdcall;
procedure ResetPerfomance; stdcall;

implementation

uses WinApi.Windows, System.Classes, System.SysUtils, DbgHookTypes, DbgHookMemory, DbgHookSyncObjs, DbgHookUtils,
  DbgHookCS;

type
  POutDbgInfo = ^TOutDbgInfo;
  TOutDbgInfo = array[0..4] of NativeUInt;

var
  _Delta: Cardinal = 0;

  _TimerQueue: THandle = 0;
  _OutDbgInfoTimer: THandle = 0;
  _SamplingTimer: THandle = 0;

  _IsSetOutDbgInfoThreadName: LongBool = False;
  _DbgInfoPerfomance: NativeUInt = NativeUInt(dstPerfomance);
  _DbgOutLock: TDbgCriticalSection = Nil;

  _OutDbgInfoRec: TOutDbgInfo;

procedure _OutDbgInfo(Context: Pointer; Success: LongBool); stdcall;
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

      ZeroMemory(@_OutDbgInfoRec[0], SizeOf(TOutDbgInfo));

      MemInfoLock.Enter;
      SyncObjsInfoLock.Enter;
      try
        _OutDbgInfoRec[0] := NativeUInt(dstPerfomanceAndInfo);

        if (MemInfoListCnt > 0) and (MemInfoList <> Nil) then
        begin
          _OutDbgInfoRec[1] := NativeUInt(@MemInfoList^[0]);
          _OutDbgInfoRec[2] := NativeUInt(MemInfoListCnt);
        end;

        if (SyncObjsInfoListCnt > 0) and (SyncObjsInfoList <> Nil) then
        begin
          _OutDbgInfoRec[3] := NativeUInt(@SyncObjsInfoList^[0]);
          _OutDbgInfoRec[4] := NativeUInt(SyncObjsInfoListCnt);
        end;

        RaiseException(DBG_EXCEPTION, 0, 5, @_OutDbgInfoRec[0]);

        MemInfoListCnt := 0;
        SyncObjsInfoListCnt := 0;
      finally
        SyncObjsInfoLock.Leave;
        MemInfoLock.Leave;
      end;
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
