unit DbgHookMemory;

interface

procedure InitMemoryHook(MemoryMgr: Pointer); stdcall;
procedure ResetMemoryHook; stdcall;

procedure _OutMemInfoBuf;

implementation

uses Windows, SyncObjs, DbgHookTypes, JclWin32, JclBase, JclDebug, SysUtils;

var
  _BaseMemoryMgr: TMemoryManagerEx;

  MemInfoList: PDbgMemInfoList = nil;
  MemInfoListCnt: Integer = 0;
  MemInfoLock: TCriticalSection = nil;

  _MemoryMgr: PMemoryManagerEx = nil;

type
{$IFDEF VER230}
  TMemSize = NativeUInt;
  TMemUSize = NativeUInt;
{$ELSE}
  TMemSize = Integer;
  TMemUSize = Cardinal;
{$ENDIF}

var
  _BaseGetMem: function(Size: TMemSize): Pointer;
  _BaseFreeMem: function(P: Pointer): Integer;
  _BaseReallocMem: function(P: Pointer; Size: TMemSize): Pointer;
  _BaseAllocMem: function(Size: TMemUSize): Pointer;

function _HookGetMem(Size: TMemSize): Pointer; forward;
function _HookFreeMem(P: Pointer): Integer; forward;
function _HookReallocMem(P: Pointer; Size: TMemSize): Pointer; forward;
function _HookAllocMem(Size: TMemUSize): Pointer; forward;


procedure _MemOutInfo(const DbgInfoType: TDbgInfoType; Ptr: Pointer; const Count: Cardinal);
var
  DbgInfo: array[0..2] of Cardinal;
begin
  DbgInfo[0] := Cardinal(DbgInfoType);
  DbgInfo[1] := Cardinal(Ptr);
  DbgInfo[2] := Count;

  RaiseException(DBG_EXCEPTION, 0, 3, @DbgInfo[0]);
end;

threadvar
  _Buf: TMemoryBasicInformation;

function IsValidCodeAddr(const Addr: Pointer): Boolean;
const
  _PAGE_CODE: Cardinal = (PAGE_EXECUTE Or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE Or PAGE_EXECUTE_WRITECOPY);
//Var
//  Buf: TMemoryBasicInformation;
Begin
  Result := (VirtualQuery(Addr, _Buf, SizeOf(TMemoryBasicInformation)) <> 0) And ((_Buf.Protect And _PAGE_CODE) <> 0);
end;

function IsValidAddr(const Addr: Pointer): Boolean;
//Var
//  Buf: TMemoryBasicInformation;
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
    Level := -2; // ��� � ����� dll-��
    StackFrame := GetFramePointer;
    BaseOfStack := TJclAddr(StackFrame) - 1;
    TopOfStack := GetStackTop;
    while (Level < Length(Stack)) and (
      (Level < 0) or (
        (BaseOfStack < TJclAddr(StackFrame)) and
        (TJclAddr(StackFrame) < TopOfStack) and
        IsValidAddr(StackFrame)
        // TODO: �� �����-�� ������� ��� �������� ������ �����
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
      MemInfoListCnt := 0; // ����� ��������� �� ������� �������
    end;
  finally
    MemInfoLock.Leave;
  end;
end;

procedure _AddMemInfo(const _MemInfoType: TDbgMemInfoType; _Ptr: Pointer; const _Size: Cardinal);
var
  DbgMemInfo: TDbgMemInfo;
  //ObjClassName: ShortString;
begin
  try
    // TODO: ���������� �� ������������� �����

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

    MemInfoLock.Enter;
    try
      if MemInfoList = Nil then Exit;

      MemInfoList^[MemInfoListCnt] := DbgMemInfo;
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

function _HookGetMem(Size: TMemSize): Pointer;
begin
  Result := _BaseGetMem(Size);
  PCardinal(Result)^ := $00000000; // ������� ��� TObject

  _AddMemInfo(miGetMem, Result, Size);
end;

function _HookFreeMem(P: Pointer): Integer;
begin
  _AddMemInfo(miFreeMem, P, 0);
  PCardinal(P)^ := $00000000; // ������� ��� TObject

  Result := _BaseFreeMem(P);
end;

function _HookReallocMem(P: Pointer; Size: TMemSize): Pointer;
begin
  _AddMemInfo(miFreeMem, P, 0);

  Result := _BaseReallocMem(P, Size);

  _AddMemInfo(miGetMem, Result, Size);
end;

function _HookAllocMem(Size: TMemUSize): Pointer;
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
  
  //TODO: ����� ������ ������ � ������������ �����

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