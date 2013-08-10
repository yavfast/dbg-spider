unit DbgHookMemory;

interface

procedure InitMemoryHook(MemoryMgr: Pointer); stdcall;

implementation

uses Windows, SyncObjs, DbgHookTypes;

var
  _Lock: TCriticalSection = nil;
  _MemoryMgr: PMemoryManagerEx = nil;
  _BaseGetMem: function(Size: Integer): Pointer;
  _BaseFreeMem: function(P: Pointer): Integer;

function _HookGetMem(Size: Integer): Pointer; forward;
function _HookFreeMem(P: Pointer): Integer; forward;

type
  TMemInfoType = (miGetMem, miFreeMem);

  TMemInfo = packed record
    MemInfoType: TMemInfoType;
    Ptr: Pointer;
    Size: Cardinal;
  end;

const
  _MemListLength = $FFFF div SizeOf(TMemInfo);
type
  TMemInfoList = array[0.._MemListLength - 1] of TMemInfo;

var
  MemInfoList: TMemInfoList;
  MemInfoListCnt: Integer = 0;
  MemInfoLock: TCriticalSection = nil;

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

procedure _AddMemInfo(const _MemInfoType: TMemInfoType; _Ptr: Pointer; const _Size: Cardinal);
begin
  MemInfoLock.Enter;

  with MemInfoList[MemInfoListCnt] do
  begin
    MemInfoType := _MemInfoType;
    Ptr := _Ptr;
    Size := _Size;
  end;

  Inc(MemInfoListCnt);

  if MemInfoListCnt = _MemListLength then
  begin
    _MemOutInfo(dstMemInfo, @MemInfoList[0], MemInfoListCnt);

    MemInfoListCnt := 0; // сброс указателя на нулевой элемент
  end;

  MemInfoLock.Leave;
end;

function _HookGetMem(Size: Integer): Pointer;
begin
  Result := _BaseGetMem(Size);

  _AddMemInfo(miGetMem, Result, Size);
end;

function _HookFreeMem(P: Pointer): Integer;
begin
  _AddMemInfo(miFreeMem, P, 0);

  Result := _BaseFreeMem(P);
end;

procedure InitMemoryHook(MemoryMgr: Pointer); stdcall;
begin
  OutputDebugStringA('Init memory hooks...');
  MemInfoListCnt := 0;
  _MemoryMgr := MemoryMgr;
  with _MemoryMgr^ do
  begin
    MemInfoLock := TCriticalSection.Create;
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

end.
