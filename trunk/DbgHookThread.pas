unit DbgHookThread;

interface

function InitThreadHook(ImageBase: Pointer): Boolean; stdcall;
procedure ResetThreadHook; stdcall;

implementation

uses Windows, SysUtils, Classes, DbgHookTypes, DbgHookCS, JclPEImage{TODO: Remove JCL};

type
  TKernel32_CreateThread = function(SecurityAttributes: Pointer; StackSize: LongWord;
    ThreadFunc: TThreadFunc; Parameter: Pointer;
    CreationFlags: LongWord; var ThreadId: LongWord): Integer; stdcall;

  PThreadRec = ^TThreadRec;
  TThreadRec = record
    Func: TThreadFunc;
    Parameter: Pointer;
  end;

var
  ThreadsLock: TDbgCriticalSection = nil;
  ThreadsHooked: Boolean = False;
  Kernel32_CreateThread: TKernel32_CreateThread = nil;

procedure _OutCreateThreadInfo(const ParentThreadId, ThreadId: Cardinal; ThName: PShortString);
var
  Args: array[0..3] of NativeUInt;
begin
  Args[0] := NativeUInt(dstThreadInfo);
  Args[1] := ThreadId;
  Args[2] := NativeUInt(@ThName^[1]);
  Args[3] := ParentThreadId;

  RaiseException(DBG_EXCEPTION, 0, 4, @Args[0]);
end;

function _HookedCreateThread(SecurityAttributes: Pointer; StackSize: LongWord;
  ThreadFunc: TThreadFunc; Parameter: Pointer;
  CreationFlags: LongWord; var ThreadId: LongWord): Integer; stdcall;
var
  ThRec: PThreadRec;
  Th: TObject;
  ParentId: Cardinal;
  ThName: ShortString;
begin
  Th := Nil;
  ThName := '';

  ThreadsLock.Enter;
  try
    ParentId := GetCurrentThreadId;

    if Assigned(Parameter) then
    begin
      ThRec := PThreadRec(Parameter);
      try
        Th := TObject(ThRec^.Parameter);
        ThName := PShortString(PPointer(Integer(Th.ClassType) + vmtClassName)^)^;
      except
        Th := Nil;
      end;
    end;

    Result := Kernel32_CreateThread(SecurityAttributes, StackSize, ThreadFunc, Parameter, CreationFlags, ThreadId);

    if (Result <> 0) and (Th <> nil) and (ThName <> '') then
      _OutCreateThreadInfo(ParentId, ThreadId, @ThName);
  finally
    ThName := '';
    ThreadsLock.Leave;
  end;
end;

var
  _PeMapImgHooks: TJclPeMapImgHooks = Nil;

function _HookThreads(ImageBase: Pointer): Boolean;
var
  ProcAddr: Pointer;
begin
  if not ThreadsHooked then
  begin
    ThreadsLock := TDbgCriticalSection.Create;

    _PeMapImgHooks := TJclPeMapImgHooks.Create;

    ProcAddr := GetProcAddress(GetModuleHandle(kernel32), 'CreateThread');
    OutputDebugStringA(PAnsiChar(AnsiString(Format('CreateThread: %p', [ProcAddr]))));

    ThreadsLock.Enter;
    try
      Result := _PeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddr, @_HookedCreateThread);

      if Result then
        @Kernel32_CreateThread := ProcAddr;

      ThreadsHooked := Result;
    finally
      ThreadsLock.Leave;
    end;
  end
  else
    Result := True;
end;

procedure _UnHookThreads;
begin
  if ThreadsHooked then
  begin
    while not ThreadsLock.TryEnter do
      SwitchToThread;
    try
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_CreateThread);
      FreeAndNil(_PeMapImgHooks);

      ThreadsHooked := False;
    finally
      ThreadsLock.Leave;
    end;

    while not ThreadsLock.TryEnter do
      SwitchToThread;
  end;

  FreeAndNil(ThreadsLock);
end;

function InitThreadHook(ImageBase: Pointer): Boolean; stdcall;
begin
  OutputDebugStringA('Init debug hooks...');

  Result := _HookThreads(ImageBase);
  if Result then
    OutputDebugStringA('Init thread hook - ok')
  else
    OutputDebugStringA('Init thread hook - fail')
end;

procedure ResetThreadHook; stdcall;
begin
  try
    _UnHookThreads;

    OutputDebugStringA('Reset thread hook - ok');
  except
    on E: Exception do
      OutputDebugStringA(PAnsiChar('Reset thread hook fail: ' + E.Message));
  end;
end;

end.