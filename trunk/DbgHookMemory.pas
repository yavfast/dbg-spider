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
  _HookMemoryMgr: TMemoryManagerEx;

  MemInfoList: PDbgMemInfoList = nil;
  MemInfoListCnt: Integer = 0;
  MemCallStack: Boolean = False;
  MemInfoLock: TCriticalSection = nil;

  MemLock: TCriticalSection = nil;

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
  TMemOutDbgInfo = array[0..2] of NativeUInt;

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

procedure _LogException(E: Exception; const Code: Integer = 0);
begin
  OutputDebugString(PChar(Format('Hook memory error (%d): %s', [Code, E.Message])));
end;

procedure _MemOutInfo(const DbgInfoType: TDbgInfoType; Ptr: Pointer; const Count: NativeUInt);
var
  MemOutDbgInfo: PMemOutDbgInfo;
begin
  MemOutDbgInfo := @_MemOutDbgInfo;
  MemOutDbgInfo[0] := NativeUInt(DbgInfoType);
  MemOutDbgInfo[1] := NativeUInt(Ptr);
  MemOutDbgInfo[2] := NativeUInt(Count);

  RaiseException(DBG_EXCEPTION, 0, 3, @MemOutDbgInfo[0]);
end;

procedure _SetMemHookStatus(const Status: NativeUInt);
var
  MemOutDbgInfo: PMemOutDbgInfo;
begin
  MemOutDbgInfo := @_MemOutDbgInfo;
  MemOutDbgInfo[0] := NativeUInt(dstMemHookStatus);
  MemOutDbgInfo[1] := Status;
  MemOutDbgInfo[2] := 0;

  RaiseException(DBG_EXCEPTION, 0, 2, @MemOutDbgInfo[0]);
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
    on E: Exception do
      _LogException(E, _EHOOK_GetObjClassType);
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
    on E: Exception do
      _LogException(E, _EHOOK_GetCallStack);
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

{.$DEFINE MEMLOCK}

function _HookGetMem(Size: TMemSize): Pointer;
begin
  {$IFDEF MEMLOCK}
  MemLock.Enter;
  try
  {$ENDIF}
    Result := _BaseMemoryMgr.GetMem(Size);

    if Size >= SizeOf(Cardinal) then
      PCardinal(Result)^ := $00000000; // Очистка для TObject

    _AddMemInfo(miGetMem, Result, Size);
  {$IFDEF MEMLOCK}
  finally
    MemLock.Leave;
  end;
  {$ENDIF}
end;

function _HookFreeMem(P: Pointer): Integer;
begin
  {$IFDEF MEMLOCK}
  MemLock.Enter;
  try
  {$ENDIF}
    _AddMemInfo(miFreeMem, P, 0);

    // !!! Указатель может быть уже невалидным
    //PCardinal(P)^ := $00000000; // Очистка для TObject

    Result := _BaseMemoryMgr.FreeMem(P);
  {$IFDEF MEMLOCK}
  finally
    MemLock.Leave;
  end;
  {$ENDIF}
end;

function _HookReallocMem(P: Pointer; Size: TMemSize): Pointer;
begin
  {$IFDEF MEMLOCK}
  MemLock.Enter;
  try
  {$ENDIF}
    _AddMemInfo(miFreeMem, P, 0);

    Result := _BaseMemoryMgr.ReallocMem(P, Size);

    _AddMemInfo(miGetMem, Result, Size);
  {$IFDEF MEMLOCK}
  finally
    MemLock.Leave;
  end;
  {$ENDIF}
end;

function _HookAllocMem(Size: TMemSize): Pointer;
begin
  {$IFDEF MEMLOCK}
  MemLock.Enter;
  try
  {$ENDIF}
    Result := _BaseMemoryMgr.AllocMem(Size);

    _AddMemInfo(miGetMem, Result, Size);
  {$IFDEF MEMLOCK}
  finally
    MemLock.Leave;
  end;
  {$ENDIF}
end;

procedure InitMemoryHook(MemoryMgr: Pointer; MemoryCallStack: Boolean); stdcall;
begin
  OutputDebugStringA('Init memory hooks...');
  MemInfoListCnt := 0;
  MemCallStack := MemoryCallStack;

  MemLock := TCriticalSection.Create;

  MemInfoList := AllocMem(SizeOf(TDbgMemInfoList));
  MemInfoLock := TCriticalSection.Create;

  _HookMemoryMgr.GetMem := _HookGetMem;
  _HookMemoryMgr.FreeMem := _HookFreeMem;
  _HookMemoryMgr.ReallocMem := _HookReallocMem;
  _HookMemoryMgr.AllocMem := _HookAllocMem;

  _MemoryMgr := MemoryMgr;
  _BaseMemoryMgr := _MemoryMgr^;

  MemInfoLock.Enter;
  _MemoryMgr^ := _HookMemoryMgr;
  MemInfoLock.Leave;

  _SetMemHookStatus(0);

  OutputDebugStringA('Init memory hooks - ok');
end;

procedure ResetMemoryHook; stdcall;
begin
  if _MemoryMgr = nil then Exit;
  
  OutputDebugStringA('Reset memory hooks...');

  // Восстанавливаем оригинальный менеджер памяти
  MemInfoLock.Enter;
  try
    _MemoryMgr^ := _BaseMemoryMgr;
    _MemoryMgr := Nil;

    _SetMemHookStatus(1);
  finally
    MemInfoLock.Leave;
  end;

  // Освобождаем локи
  SwitchToThread;

  // Сбрасываем буффер
  MemInfoLock.Enter;
  try
    _OutMemInfoBuf(dstMemInfo);

    FreeMemory(MemInfoList);
    MemInfoList := Nil;
  finally
    MemInfoLock.Leave;
  end;

  FreeAndNil(MemInfoLock);

  FreeAndNil(MemLock);

  OutputDebugStringA('Reset memory hooks - ok');
end;

end.
