unit DbgHookMemory;

interface

uses DbgHookTypes;

procedure InitMemoryHook(MemoryMgr: Pointer; MemoryCallStack: Boolean); stdcall;
procedure ResetMemoryHook; stdcall;

function _OutMemInfoBuf(const DbgInfoType: TDbgInfoType = dstMemInfo): Boolean;

implementation

uses Windows, SyncObjs, SysUtils{, JclWin32, JclBase, JclDebug};

var
  _BaseMemoryMgr: TMemoryManagerEx;

  MemInfoList: PDbgMemInfoList = nil;
  MemInfoListCnt: Integer = 0;
  MemCallStack: Boolean = False;
  MemInfoLock: TCriticalSection = nil;

  _MemoryMgr: PMemoryManagerEx = nil;

type
  TMemSize = NativeInt;
  //TMemUSize = NativeUInt;

var
  _BaseGetMem: function(Size: TMemSize): Pointer;
  _BaseFreeMem: function(P: Pointer): Integer;
  _BaseReallocMem: function(P: Pointer; Size: TMemSize): Pointer;
  _BaseAllocMem: function(Size: TMemSize): Pointer;

function _HookGetMem(Size: TMemSize): Pointer; forward;
function _HookFreeMem(P: Pointer): Integer; forward;
function _HookReallocMem(P: Pointer; Size: TMemSize): Pointer; forward;
function _HookAllocMem(Size: TMemSize): Pointer; forward;

type
  PMemOutDbgInfo = ^TMemOutDbgInfo;
  TMemOutDbgInfo = array[0..2] of Cardinal;

threadvar
  _MemOutDbgInfo: TMemOutDbgInfo;

{ --- From JCL --- }
type
  NT_TIB32 = packed record
    ExceptionList: DWORD;
    StackBase: DWORD;
    StackLimit: DWORD;
    SubSystemTib: DWORD;
    case Integer of
      0 : (
        FiberData: DWORD;
        ArbitraryUserPointer: DWORD;
        Self: DWORD;
      );
      1 : (
        Version: DWORD;
      );
  end;

  TJclAddr = NativeInt;

  PStackFrame = ^TStackFrame;
  TStackFrame = record
    CallerFrame: TJclAddr;
    CallerAddr: TJclAddr;
  end;
{ --- From JCL --- }

procedure _MemOutInfo(const DbgInfoType: TDbgInfoType; Ptr: Pointer; const Count: Cardinal);
var
  MemOutDbgInfo: PMemOutDbgInfo;
begin
  MemOutDbgInfo := @_MemOutDbgInfo;
  MemOutDbgInfo[0] := Cardinal(DbgInfoType);
  MemOutDbgInfo[1] := Cardinal(Ptr);
  MemOutDbgInfo[2] := Count;

  RaiseException(DBG_EXCEPTION, 0, 3, @MemOutDbgInfo[0]);
end;

threadvar
  _Buf: TMemoryBasicInformation;

function IsValidCodeAddr(const Addr: Pointer): Boolean;
const
  _PAGE_CODE: Cardinal = (PAGE_EXECUTE Or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE Or PAGE_EXECUTE_WRITECOPY);
Begin
  Result := (VirtualQuery(Addr, _Buf, SizeOf(TMemoryBasicInformation)) <> 0) And ((_Buf.Protect And _PAGE_CODE) <> 0);
end;

function IsValidAddr(const Addr: Pointer): Boolean;
Begin
  Result := (VirtualQuery(Addr, _Buf, SizeOf(TMemoryBasicInformation)) <> 0);
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

procedure GetCallStack(var Stack: TDbgMemInfoStack); stdcall;
var
  TopOfStack: TJclAddr;
  BaseOfStack: TJclAddr;
  StackFrame: PStackFrame;
  Level: Integer;
begin
  try
    ZeroMemory(@Stack[0], Length(Stack) * SizeOf(Pointer));

    Level := -2; // это в нашей dll-ке
    StackFrame := GetFramePointer;
    BaseOfStack := TJclAddr(StackFrame) - 1;
    TopOfStack := GetStackTop;

    while (Level < Length(Stack)) and (
      (Level < 0) or (
        (BaseOfStack < TJclAddr(StackFrame)) and
        (TJclAddr(StackFrame) < TopOfStack) and
        IsValidAddr(StackFrame)
        // TODO: По какой-то причине эта проверка сильно тупит
        // and IsValidCodeAddr(Pointer(StackFrame^.CallerAddr))
        )
      )
    do begin
      if Level >= 0 then
        Stack[Level] := Pointer(StackFrame^.CallerAddr - 1);

      StackFrame := PStackFrame(StackFrame^.CallerFrame);

      Inc(Level);
    end;
  except
    //
  end;
end;

function _OutMemInfoBuf(const DbgInfoType: TDbgInfoType = dstMemInfo): Boolean;
begin
  Result := False;

  if MemInfoList = Nil then Exit;
  if MemInfoLock = Nil then Exit;

  MemInfoLock.Enter;
  try
    if MemInfoListCnt > 0 then
    begin
      _MemOutInfo(DbgInfoType, @MemInfoList^[0], MemInfoListCnt);
      MemInfoListCnt := 0; // сброс указателя на нулевой элемент

      Result := True;
    end;
  finally
    MemInfoLock.Leave;
  end;
end;

threadvar
  _DbgMemInfo: TDbgMemInfo;

procedure _AddMemInfo(const _MemInfoType: TDbgMemInfoType; const _Ptr: Pointer; const _Size: Cardinal); stdcall;
var
  DbgMemInfo: PDbgMemInfo;
begin
  DbgMemInfo := @_DbgMemInfo;
  ZeroMemory(DbgMemInfo, SizeOf(TDbgMemInfo));

  DbgMemInfo.Ptr := _Ptr;
  DbgMemInfo.ThreadId := GetCurrentThreadId;
  DbgMemInfo.MemInfoType := _MemInfoType;
  case DbgMemInfo.MemInfoType of
    miGetMem:
    begin
      DbgMemInfo.Size := _Size;
      GetCallStack(DbgMemInfo.Stack);
    end;
    miFreeMem:
    begin
      DbgMemInfo.ObjClassType[0] := #0;
    end;
  end;

  MemInfoLock.Enter;
  if MemInfoList <> Nil then
  begin
    MemInfoList^[MemInfoListCnt] := DbgMemInfo^;
    Inc(MemInfoListCnt);

    if MemInfoListCnt = _DbgMemListLength then
      _OutMemInfoBuf;
  end;
  MemInfoLock.Leave;
end;

function _HookGetMem(Size: TMemSize): Pointer;
begin
  Result := _BaseGetMem(Size);
  PCardinal(Result)^ := $00000000; // Очистка для TObject

  _AddMemInfo(miGetMem, Result, Size);
end;

function _HookFreeMem(P: Pointer): Integer;
begin
  _AddMemInfo(miFreeMem, P, 0);

  // !!! Указатель может быть уже невалидным
  //PCardinal(P)^ := $00000000; // Очистка для TObject

  Result := _BaseFreeMem(P);
end;

function _HookReallocMem(P: Pointer; Size: TMemSize): Pointer;
begin
  _AddMemInfo(miFreeMem, P, 0);

  Result := _BaseReallocMem(P, Size);

  _AddMemInfo(miGetMem, Result, Size);
end;

function _HookAllocMem(Size: TMemSize): Pointer;
begin
   Result := _BaseAllocMem(Size);

  _AddMemInfo(miGetMem, Result, Size);
end;

procedure InitMemoryHook(MemoryMgr: Pointer; MemoryCallStack: Boolean); stdcall;
begin
  OutputDebugStringA('Init memory hooks...');
  MemInfoListCnt := 0;
  MemCallStack := MemoryCallStack;

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
