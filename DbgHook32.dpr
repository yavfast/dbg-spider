library DbgHook32;

uses
  Windows, SysUtils, Classes, JclPEImage, SyncObjs;

type
  TKernel32_CreateThread = function(SecurityAttributes: Pointer; StackSize: LongWord;
    ThreadFunc: TThreadFunc; Parameter: Pointer;
    CreationFlags: LongWord; var ThreadId: LongWord): Integer; stdcall;

  PThreadRec = ^TThreadRec;
  TThreadRec = record
    Func: TThreadFunc;
    Parameter: Pointer;
  end;

  TDbgInfoType = (dstUnknown = 0, dstThreadInfo, dstGetMem, dstFreeMem);

var
  ThreadsHooked: Boolean;
  Kernel32_CreateThread: TKernel32_CreateThread = nil;

  _Lock: TCriticalSection = nil;
  _MemoryMgr: PMemoryManagerEx = nil;
  _BaseGetMem: function(Size: Integer): Pointer;
  _BaseFreeMem: function(P: Pointer): Integer;

const
  DBG_EXCEPTION = $0EEDFFF0;

procedure _DbgOutInfo(const ArgsCnt: Cardinal; Args: Pointer);
begin
  try
    RaiseException(DBG_EXCEPTION, 0, ArgsCnt, PDWord(@Args));
  except
  end;
end;

function _HookGetMem(Size: Integer): Pointer; forward;
function _HookFreeMem(P: Pointer): Integer; forward;

function _HookedCreateThread(SecurityAttributes: Pointer; StackSize: LongWord;
  ThreadFunc: TThreadFunc; Parameter: Pointer;
  CreationFlags: LongWord; var ThreadId: LongWord): Integer; stdcall;
var
  ThRec: PThreadRec;
  Th: TObject;
  ParentId: Cardinal;
  Args: array[0..2] of Cardinal;
  ThName: ShortString;
begin
  ParentId := 0;
  Th := Nil;

  if Assigned(Parameter) then
  begin
    ThRec := PThreadRec(Parameter);
    try
      Th := TObject(ThRec^.Parameter);
      ThName := PShortString(PPointer(Integer(Th.ClassType) + vmtClassName)^)^;
      ParentId := GetCurrentThreadId;
    except
      Th := Nil;
      ParentId := 0;
    end;
  end;

  Result := Kernel32_CreateThread(SecurityAttributes, StackSize, ThreadFunc, Parameter, CreationFlags, ThreadId);

  if (Result <> 0) and (Th <> Nil) then
  begin
    Args[0] := Cardinal(dstThreadInfo);
    Args[1] := ThreadId;
    Args[2] := Cardinal(@ThName[1]);

    try
      RaiseException(DBG_EXCEPTION, 0, 3, @Args[0]);
    except
    end;
  end;
end;

function _HookThreads(ImageBase: Pointer): Boolean;
var
  ProcAddrCache: Pointer;
begin
  if not ThreadsHooked then
  begin
    ProcAddrCache := GetProcAddress(GetModuleHandle(kernel32), 'CreateThread');
    OutputDebugStringA(PAnsiChar(AnsiString(Format('ProcAddrCache: %p', [ProcAddrCache]))));
    OutputDebugStringA(PAnsiChar(AnsiString(Format('ImageBase: %p', [ImageBase]))));

    Result := TJclPeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddrCache, @_HookedCreateThread);

    if Result then
    begin
      @Kernel32_CreateThread := ProcAddrCache;
    end;

    ThreadsHooked := Result;
  end
  else
    Result := True;
end;


procedure _MemOutInfo(const DbgStrType: TDbgInfoType; Ptr: Pointer; const Size: Cardinal);
var
  DbgInfo: array[0..2] of Cardinal;
begin
  DbgInfo[0] := Cardinal(DbgStrType);
  DbgInfo[1] := Cardinal(Ptr);
  DbgInfo[2] := Size;

  try
    RaiseException(DBG_EXCEPTION, 0, 3, @DbgInfo[0]);
  except
  end;
end;

function _HookGetMem(Size: Integer): Pointer;
begin
  Result := _BaseGetMem(Size);

  _MemOutInfo(dstGetMem, Result, Size);
end;

function _HookFreeMem(P: Pointer): Integer;
begin
  Result := _BaseFreeMem(P);

  _MemOutInfo(dstFreeMem, P, Result);
end;

{-------------------------------------------------------------------------------------------------------}

function InitHook(ImageBase: Pointer): Boolean; stdcall;
begin
  OutputDebugStringA('Init debug hooks...');

  Result := _HookThreads(ImageBase);
  if Result then
    OutputDebugStringA('Init thread hook - ok')
  else
    OutputDebugStringA('Init thread hook - fail')
end;

procedure InitMemoryHook(MemoryMgr: Pointer); stdcall;
begin
  OutputDebugStringA('Init memory hooks...');
  _MemoryMgr := MemoryMgr;
  with _MemoryMgr^ do
  begin
    _Lock := TCriticalSection.Create;
    _Lock.Enter;
    try
      _BaseGetMem := GetMem;
      GetMem := _HookGetMem;

      _BaseFreeMem := FreeMem;
      FreeMem := _HookFreeMem;
    finally
      _Lock.Leave;
    end;
  end;
  OutputDebugStringA('Init memory hooks - ok');
end;

exports InitHook;
exports InitMemoryHook;

end.
