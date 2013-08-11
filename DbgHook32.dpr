library DbgHook32;

uses
  Windows,
  DbgHookTypes in 'DbgHookTypes.pas',
  DbgHookThread in 'DbgHookThread.pas',
  DbgHookPerf in 'DbgHookPerf.pas',
  DbgHookMemory in 'DbgHookMemory.pas';

exports
  InitThreadHook,
  InitMemoryHook,
  InitPerfomance,

  ResetThreadHook,
  ResetMemoryHook,
  ResetPerfomance;

procedure _HookDLLProc(Reason: Integer);
begin
  if Reason = DLL_PROCESS_DETACH then
  begin
    ResetPerfomance;
    ResetMemoryHook;
    ResetThreadHook;
  end;
end;

begin
  DllProc := @_HookDLLProc;
end.
