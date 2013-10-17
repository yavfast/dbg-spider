unit DbgHookThread;

interface

function InitThreadHook(ImageBase: Pointer): Boolean; stdcall;
procedure ResetThreadHook; stdcall;

implementation

uses Windows, SysUtils, Classes, SyncObjs, DbgHookTypes, JclPEImage{TODO: Remove JCL};

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
  ThreadsLock: TCriticalSection = nil;
  ThreadsHooked: Boolean;
  Kernel32_CreateThread: TKernel32_CreateThread = nil;

function _HookedCreateThread(SecurityAttributes: Pointer; StackSize: LongWord;
  ThreadFunc: TThreadFunc; Parameter: Pointer;
  CreationFlags: LongWord; var ThreadId: LongWord): Integer; stdcall;
var
  ThRec: PThreadRec;
  Th: TObject;
  ParentId: Cardinal;
  //ClassNamePtr: Pointer;
  Args: array[0..3] of Cardinal;
  //ThName: array[0..255] of AnsiChar;
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
        //ThName := PShortString(PPointer(Integer(Th.ClassType) + vmtClassName)^);
        //ClassNamePtr := PPointer(Integer(Th.ClassType) + vmtClassName)^;
        //Move(ClassNamePtr^, ThName, PByte(Cardinal(ClassNamePtr) - 1)^);
      except
        Th := Nil;
      end;
    end;

    Result := Kernel32_CreateThread(SecurityAttributes, StackSize, ThreadFunc, Parameter, CreationFlags, ThreadId);

    if (Result <> 0) then
    begin
      Args[0] := Cardinal(dstThreadInfo);
      Args[1] := ThreadId;
      if (Th <> Nil) and (ThName <> '') then
        Args[2] := Cardinal(@ThName[1])
        //Args[2] := Cardinal(ThName)
      else
        Args[2] := 0;
      Args[3] := ParentId;

      RaiseException(DBG_EXCEPTION, 0, 4, @Args[0]);
    end;
  finally
    ThName := '';
    ThreadsLock.Leave;
  end;
end;

function _HookThreads(ImageBase: Pointer): Boolean;
var
  ProcAddrCache: Pointer;
begin
  if not ThreadsHooked then
  begin
    ThreadsLock := TCriticalSection.Create;

    ProcAddrCache := GetProcAddress(GetModuleHandle(kernel32), 'CreateThread');
    OutputDebugStringA(PAnsiChar(AnsiString(Format('ProcAddrCache: %p', [ProcAddrCache]))));
    OutputDebugStringA(PAnsiChar(AnsiString(Format('ImageBase: %p', [ImageBase]))));

    ThreadsLock.Enter;
    try
      Result := TJclPeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddrCache, @_HookedCreateThread);

      if Result then
        @Kernel32_CreateThread := ProcAddrCache;

      ThreadsHooked := Result;
    finally
      ThreadsLock.Leave;
    end;
  end
  else
    Result := True;
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
  OutputDebugStringA('Reset thread hook - skip')
  // TODO:
end;

end.
