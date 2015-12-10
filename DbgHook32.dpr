library DbgHook32;

uses
  Windows,
  DbgHookTypes in 'DbgHookTypes.pas',
  DbgHookThread in 'DbgHookThread.pas',
  DbgHookPerf in 'DbgHookPerf.pas',
  DbgHookMemory in 'DbgHookMemory.pas',
  DbgHookSyncObjs in 'DbgHookSyncObjs.pas',
  DbgHookCS in 'DbgHookCS.pas',
  DbgHookUtils in 'DbgHookUtils.pas',
  WinAPIUtils in 'WinAPIUtils.pas',
  KOLDetours in 'External\KOLDetours.pas';

exports
  InitThreadHook,
  InitSyncObjsHook,
  InitMemoryHook,
  InitPerfomance,

  ResetThreadHook,
  ResetSyncObjsHook,
  ResetMemoryHook,
  ResetPerfomance;

procedure _HookDLLProc(Reason: Integer);
begin
  if Reason = DLL_PROCESS_DETACH then
  begin
    ResetPerfomance;
    ResetMemoryHook;
    ResetSyncObjsHook;
    ResetThreadHook;
  end;
end;

begin
  DllProc := @_HookDLLProc;
end.
