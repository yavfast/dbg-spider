unit DbgHookMemory;

interface

procedure InitMemoryHook(MemoryMgr: Pointer); stdcall;
procedure ResetMemoryHook; stdcall;

procedure _OutMemInfoBuf;

implementation

uses Windows, SyncObjs, DbgHookTypes, JclWin32, JclBase, JclDebug, SysUtils;

var
  _MemoryMgr: PMemoryManagerEx = nil;
  _BaseGetMem: function(Size: NativeInt): Pointer;
  _BaseFreeMem: function(P: Pointer): Integer;
  _BaseReallocMem: function(P: Pointer; Size: NativeInt): Pointer;
  _BaseAllocMem: function(Size: NativeInt): Pointer;

  _BaseMemoryMgr: TMemoryManagerEx;

  MemInfoList: PDbgMemInfoList = nil;
  MemInfoListCnt: Integer = 0;
  MemInfoLock: TCriticalSection = nil;

function _HookGetMem(Size: NativeInt): Pointer; forward;
function _HookFreeMem(P: Pointer): Integer; forward;
function _HookReallocMem(P: Pointer; Size: NativeInt): Pointer; forward;
function _HookAllocMem(Size: NativeInt): Pointer; forward;


procedure _MemOutInfo(const DbgInfoType: TDbgInfoType; Ptr: Pointer; const Count: Cardinal);
var
  DbgInfo: array[0..2] of Cardinal;
begin
  DbgInfo[0] := Cardinal(DbgInfoType);
  DbgInfo[1] := Cardinal(Ptr);
  DbgInfo[2] := Count;

  RaiseException(DBG_EXCEPTION, 0, 3, @DbgInfo[0]);
end;

function IsValidCodeAddr(const Addr: Pointer): Boolean;
Var
  mbi: TMemoryBasicInformation;
Begin
  Result := (VirtualQuery(Pointer(Addr), mbi, SizeOf(mbi)) <> 0) And
    ((mbi.Protect And (PAGE_EXECUTE Or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE Or PAGE_EXECUTE_WRITECOPY)) <> 0);
end;

function IsValidAddr(const Addr: Pointer): Boolean;
Var
  Buf: TMemoryBasicInformation;
Begin
  Result := (VirtualQuery(Pointer(Addr), Buf, SizeOf(Buf)) <> 0);
end;

function _GetObjClassType(Obj: Pointer; var ObjClassName: ShortString): Boolean;
var
  ClassTypePtr: Pointer;
  ClassNamePtr: Pointer;
begin
  Result := False;
  try
    if not IsValidAddr(Obj) then Exit;

    ClassTypePtr := PPointer(Obj)^;
    if not IsValidCodeAddr(ClassTypePtr) then Exit;
    ClassNamePtr := Pointer(Integer(ClassTypePtr) + vmtClassName);
    if not IsValidCodeAddr(ClassNamePtr) then Exit;
    ClassNamePtr := PPointer(ClassNamePtr)^;
    if not IsValidCodeAddr(ClassNamePtr) then Exit;
    ObjClassName := PShortString(ClassNamePtr)^;
    Result := True;
  except
    Result := False;
  end;
end;

function GetFramePointer: Pointer; assembler;
asm
  MOV     EAX, EBP
end;

function GetStackTop: TJclAddr; assembler;
asm
  MOV     EAX, FS:[0].NT_TIB32.StackBase
end;

function GetCallStack(var Stack: TDbgMemInfoStack): Boolean;
var
  TopOfStack: TJclAddr;
  BaseOfStack: TJclAddr;
  StackFrame: PStackFrame;
  Level: Integer;
begin
  Result := True;
  try
    ZeroMemory(@Stack[0], Length(Stack) * SizeOf(Pointer));
    Level := 0;
    StackFrame := GetFramePointer;
    BaseOfStack := TJclAddr(StackFrame) - 1;
    TopOfStack := GetStackTop;
    while (Level < Length(Stack)) and (BaseOfStack < TJclAddr(StackFrame)) and (TJclAddr(StackFrame) < TopOfStack) and
      IsValidAddr(StackFrame) and IsValidCodeAddr(Pointer(StackFrame^.CallerAddr))
    do begin
      Stack[Level] := Pointer(StackFrame^.CallerAddr - 1);
      StackFrame := PStackFrame(StackFrame^.CallerFrame);
      Inc(Level);
    end;
  except
    Result := False;
  end;
end;

procedure _OutMemInfoBuf;
begin
  if MemInfoList = Nil then Exit;
  if MemInfoLock = Nil then Exit;

  MemInfoLock.Enter;
  try
    if MemInfoListCnt > 0 then
    begin
      _MemOutInfo(dstMemInfo, @MemInfoList^[0], MemInfoListCnt);
      MemInfoListCnt := 0; // сброс указателя на нулевой элемент
    end;
  finally
    MemInfoLock.Leave;
  end;
end;

procedure _AddMemInfo(const _MemInfoType: TDbgMemInfoType; _Ptr: Pointer; const _Size: Cardinal);
var
  DbgMemInfo: PDbgMemInfo;
  //ObjClassName: ShortString;
begin
  try
    // TODO: Переделать на многопоточный вызов
    MemInfoLock.Enter;
    try
      if MemInfoList = Nil then Exit;

      DbgMemInfo := @MemInfoList^[MemInfoListCnt];

      DbgMemInfo^.Ptr := _Ptr;
      DbgMemInfo^.ThreadId := GetCurrentThreadId;
      DbgMemInfo^.MemInfoType := _MemInfoType;
      case DbgMemInfo^.MemInfoType of
        miGetMem:
        begin
          DbgMemInfo^.Size := _Size;
          //GetCallStack(DbgMemInfo^.Stack);
        end;
        miFreeMem:
        begin
          DbgMemInfo^.ObjClassType[0] := #0;
          (*
          if _GetObjClassType(_Ptr, ObjClassName) then
          begin
            Move(ObjClassName, DbgMemInfo^.ObjClassType[0], Length(ObjClassName) + 1);
          end
          else
            DbgMemInfo^.ObjClassType[0] := #0;
          *)
        end;
      end;

      Inc(MemInfoListCnt);

      if MemInfoListCnt = _DbgMemListLength then
        _OutMemInfoBuf;
    finally
      MemInfoLock.Leave;
    end;
  except
    // TODO:
  end;
end;

function _HookGetMem(Size: NativeInt): Pointer;
begin
  Result := _BaseGetMem(Size);

  _AddMemInfo(miGetMem, Result, Size);
end;

function _HookFreeMem(P: Pointer): Integer;
begin
  _AddMemInfo(miFreeMem, P, 0);

   Result := _BaseFreeMem(P);
end;

function _HookReallocMem(P: Pointer; Size: NativeInt): Pointer;
begin
  _AddMemInfo(miFreeMem, P, 0);

  Result := _BaseReallocMem(P, Size);

  _AddMemInfo(miGetMem, Result, Size);
end;

function _HookAllocMem(Size: NativeInt): Pointer;
begin
   Result := _BaseAllocMem(Size);

  _AddMemInfo(miGetMem, Result, Size);
end;

procedure InitMemoryHook(MemoryMgr: Pointer); stdcall;
begin
  OutputDebugStringA('Init memory hooks...');
  MemInfoListCnt := 0;
  MemInfoList := GetMemory(SizeOf(TDbgMemInfoList));
  MemInfoLock := TCriticalSection.Create;
  MemInfoLock.Enter;
  try
    _MemoryMgr := MemoryMgr;
    _BaseMemoryMgr := _MemoryMgr^;
    with _MemoryMgr^ do
    begin
      _BaseGetMem := GetMem;
      GetMem := _HookGetMem;

      _BaseFreeMem := FreeMem;
      FreeMem := _HookFreeMem;

      _BaseReallocMem := ReallocMem;
      ReallocMem := _HookReallocMem;

      _BaseAllocMem := AllocMem;
      AllocMem := _HookAllocMem;
    end;
  finally
    MemInfoLock.Leave;
  end;
  OutputDebugStringA('Init memory hooks - ok');
end;

procedure ResetMemoryHook; stdcall;
begin
  if _MemoryMgr = nil then Exit;
  
  //TODO: Сброс буфера памяти и освобождение хуков

  OutputDebugStringA('Reset memory hooks...');
  MemInfoLock.Enter;
  try
    _MemoryMgr^ := _BaseMemoryMgr;
    _MemoryMgr := Nil;

    FreeMemory(MemInfoList);
    MemInfoList := Nil;
  finally
    MemInfoLock.Leave;
  end;

  //Sleep(100);
  FreeAndNil(MemInfoLock);

  OutputDebugStringA('Reset memory hooks - ok');
end;

end.
