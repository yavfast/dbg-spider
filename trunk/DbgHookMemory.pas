unit DbgHookMemory;

interface

uses DbgHookTypes;

procedure InitMemoryHook(MemoryMgr: Pointer; MemoryCallStack: Boolean); stdcall;
procedure ResetMemoryHook; stdcall;

function _OutMemInfoBuf(const DbgInfoType: TDbgInfoType = dstMemInfo): Boolean;

implementation

uses Windows, DbgHookCS, SysUtils, DbgHookUtils;

var
  _BaseMemoryMgr: TMemoryManagerEx;
  _HookMemoryMgr: TMemoryManagerEx;

  MemInfoList: PDbgMemInfoList = nil;
  MemInfoListCnt: Integer = 0;
  MemCallStack: Boolean = False;
  MemInfoLock: TDbgCriticalSection = nil;

  MemLock: TDbgCriticalSection = nil;

  _MemoryMgr: PMemoryManagerEx = nil;

type
  TMemSize = NativeInt;

function _HookGetMem(Size: TMemSize): Pointer; forward;
function _HookFreeMem(P: Pointer): Integer; forward;
function _HookReallocMem(P: Pointer; Size: TMemSize): Pointer; forward;
function _HookAllocMem(Size: TMemSize): Pointer; forward;

type
  PMemOutDbgInfo = ^TMemOutDbgInfo;
  TMemOutDbgInfo = array[0..2] of NativeUInt;

threadvar
  _MemOutDbgInfo: TMemOutDbgInfo;

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
      GetCallStack(DbgMemInfo.Stack, -2);
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
  _Log('Init memory hooks...');

  MemInfoListCnt := 0;
  MemCallStack := MemoryCallStack;

  MemLock := TDbgCriticalSection.Create;

  MemInfoList := AllocMem(SizeOf(TDbgMemInfoList));
  MemInfoLock := TDbgCriticalSection.Create;

  _HookMemoryMgr.GetMem := _HookGetMem;
  _HookMemoryMgr.FreeMem := _HookFreeMem;
  _HookMemoryMgr.ReallocMem := _HookReallocMem;
  _HookMemoryMgr.AllocMem := _HookAllocMem;

  _MemoryMgr := MemoryMgr;

  _BaseMemoryMgr.GetMem := _MemoryMgr^.GetMem;
  _BaseMemoryMgr.FreeMem := _MemoryMgr^.FreeMem;
  _BaseMemoryMgr.ReallocMem := _MemoryMgr^.ReallocMem;
  _BaseMemoryMgr.AllocMem := _MemoryMgr^.AllocMem;

  MemLock.Enter;
  MemInfoLock.Enter;

  _MemoryMgr^.GetMem := _HookMemoryMgr.GetMem;
  _MemoryMgr^.FreeMem := _HookMemoryMgr.FreeMem;
  _MemoryMgr^.ReallocMem := _HookMemoryMgr.ReallocMem;
  _MemoryMgr^.AllocMem := _HookMemoryMgr.AllocMem;

  MemInfoLock.Leave;
  MemLock.Leave;

  // _SetMemHookStatus(0);

  _Log('Init memory hooks - ok');
end;

procedure ResetMemoryHook; stdcall;
begin
  try
    if _MemoryMgr = nil then Exit;
    if MemInfoLock = nil then Exit;

    // Восстанавливаем оригинальный менеджер памяти
    MemInfoLock.Enter;
    try
      _MemoryMgr^ := _BaseMemoryMgr;
      _MemoryMgr := Nil;

      _SetMemHookStatus(1);
    finally
      MemInfoLock.Leave;
    end;

    while not MemInfoLock.TryEnter do
      SwitchToThread;
    try
      // Сбрасываем буффер
      _OutMemInfoBuf(dstMemInfo);

      FreeMemory(MemInfoList);
      MemInfoList := Nil;
    finally
      MemInfoLock.Leave;
    end;

    FreeAndNil(MemInfoLock);

    FreeAndNil(MemLock);

    _Log('Reset memory hooks - ok');
  except
    on E: Exception do
      _Log('Reset memory hooks fail: ' + E.Message);
  end;
end;

end.
