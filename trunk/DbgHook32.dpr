library DbgHook32;

uses
  DbgHookTypes in 'DbgHookTypes.pas',
  DbgHookThread in 'DbgHookThread.pas',
  DbgHookMemory in 'DbgHookMemory.pas',
  DbgHookPerf in 'DbgHookPerf.pas';

exports InitThreadHook;
exports InitMemoryHook;
exports InitPerfomance;

//TODO: Сброс буфера памяти и освобождение хуков

end.
