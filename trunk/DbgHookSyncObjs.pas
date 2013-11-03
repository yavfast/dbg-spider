unit DbgHookSyncObjs;

interface

uses Windows;

type
  TKernel32_Sleep =
    procedure (milliseconds: Cardinal); stdcall;
  TKernel32_EnterCriticalSection =
    procedure (var lpCriticalSection: TRTLCriticalSection); stdcall;
  TKernel32_LeaveCriticalSection =
    procedure (var lpCriticalSection: TRTLCriticalSection); stdcall;
  TKernel32_WaitForSingleObject =
    function (hHandle: THandle; dwMilliseconds: DWORD): DWORD; stdcall;
  TKernel32_WaitForMultipleObjects =
    function (nCount: DWORD; lpHandles: PWOHandleArray; bWaitAll: BOOL; dwMilliseconds: DWORD): DWORD; stdcall;

var
  SyncObjsHooked: Boolean = False;

  Kernel32_Sleep: TKernel32_Sleep = nil;
  Kernel32_EnterCriticalSection: TKernel32_EnterCriticalSection = nil;
  Kernel32_LeaveCriticalSection: TKernel32_LeaveCriticalSection = nil;
  Kernel32_WaitForSingleObject: TKernel32_WaitForSingleObject = nil;
  Kernel32_WaitForMultipleObjects: TKernel32_WaitForMultipleObjects = nil;

function InitSyncObjsHook(ImageBase: Pointer): Boolean; stdcall;
procedure ResetSyncObjsHook; stdcall;

procedure _OutSyncObjsInfo;

implementation

uses DbgHookCS, SysUtils, Classes, DbgHookTypes, JclPEImage{TODO: Remove JCL};

var
  _SyncObjsInfoList: PDbgSyncObjsInfoList = nil;
  _SyncObjsInfoListCnt: Integer = 0;

  SyncObjsLock: TDbgCriticalSection = nil;
  SyncObjsId: Integer = 0;

type
  PSyncObjsOutDbgInfo = ^TSyncObjsOutDbgInfo;
  TSyncObjsOutDbgInfo = array[0..2] of NativeUInt;

threadvar
  _SyncObjsOutDbgInfo: TSyncObjsOutDbgInfo;

procedure _SyncObjsOutInfo(const DbgInfoType: TDbgInfoType; Ptr: Pointer; const Count: NativeUInt);
var
  SyncObjsOutDbgInfo: PSyncObjsOutDbgInfo;
begin
  SyncObjsOutDbgInfo := @_SyncObjsOutDbgInfo;
  SyncObjsOutDbgInfo[0] := NativeUInt(DbgInfoType);
  SyncObjsOutDbgInfo[1] := NativeUInt(Ptr);
  SyncObjsOutDbgInfo[2] := NativeUInt(Count);

  RaiseException(DBG_EXCEPTION, 0, 3, @SyncObjsOutDbgInfo[0]);
end;

procedure _OutSyncObjsInfo;
begin
  if _SyncObjsInfoList = Nil then Exit;
  if SyncObjsLock = Nil then Exit;

  SyncObjsLock.Enter;
  try
    if _SyncObjsInfoListCnt > 0 then
    begin
      _SyncObjsOutInfo(dstSyncObjsInfo, @_SyncObjsInfoList^[0], _SyncObjsInfoListCnt);
      _SyncObjsInfoListCnt := 0; // сброс указателя на нулевой элемент
    end;
  finally
    SyncObjsLock.Leave;
  end;
end;

threadvar
  _SyncObjsInfo: TDbgSyncObjsInfo;

procedure _AddSyncObjsInfo(const DbgSyncObjsType: TDbgSyncObjsType; const DbgSyncObjsStateType: TDbgSyncObjsStateType;
  const Id: NativeUInt; const Data: NativeUInt);
var
  SyncObjsInfo: PDbgSyncObjsInfo;
begin
  SyncObjsInfo := @_SyncObjsInfo;
  SyncObjsInfo^.ThreadId := GetCurrentThreadId;
  SyncObjsInfo^.SyncObjsType := DbgSyncObjsType;
  SyncObjsInfo^.SyncObjsStateType := DbgSyncObjsStateType;
  SyncObjsInfo^.Id := Id;
  SyncObjsInfo^.Data := Data;

  SyncObjsLock.Enter;
  if _SyncObjsInfoList <> Nil then
  begin
    _SyncObjsInfoList^[_SyncObjsInfoListCnt] := SyncObjsInfo^;
    Inc(_SyncObjsInfoListCnt);

    if _SyncObjsInfoListCnt = _DbgMemListLength then
      _OutSyncObjsInfo;
  end;
  SyncObjsLock.Leave;
end;

procedure _HookSleep(milliseconds: Cardinal); stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(InterlockedIncrement(SyncObjsId));

  _AddSyncObjsInfo(soSleep, sosEnter, Id, milliseconds);

  Kernel32_Sleep(milliseconds);

  _AddSyncObjsInfo(soSleep, sosLeave, Id, milliseconds);
end;

procedure _HookEnterCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(InterlockedIncrement(SyncObjsId));

  _AddSyncObjsInfo(soEnterCriticalSection, sosEnter, Id, NativeUInt(@lpCriticalSection));

  Kernel32_EnterCriticalSection(lpCriticalSection);

  _AddSyncObjsInfo(soEnterCriticalSection, sosLeave, Id, NativeUInt(@lpCriticalSection));
end;

procedure _HookLeaveCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(InterlockedIncrement(SyncObjsId));

  _AddSyncObjsInfo(soLeaveCriticalSection, sosEnter, Id, NativeUInt(@lpCriticalSection));

  Kernel32_LeaveCriticalSection(lpCriticalSection);

  _AddSyncObjsInfo(soLeaveCriticalSection, sosLeave, Id, NativeUInt(@lpCriticalSection));
end;

function _HookWaitForSingleObject(hHandle: THandle; dwMilliseconds: DWORD): DWORD; stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(InterlockedIncrement(SyncObjsId));

  _AddSyncObjsInfo(soWaitForSingleObject, sosEnter, Id, NativeUInt(hHandle));

  Result := Kernel32_WaitForSingleObject(hHandle, dwMilliseconds);

  _AddSyncObjsInfo(soLeaveCriticalSection, sosLeave, Id, NativeUInt(hHandle));
end;

function _HookWaitForMultipleObjects(nCount: DWORD; lpHandles: PWOHandleArray; bWaitAll: BOOL; dwMilliseconds: DWORD): DWORD; stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(InterlockedIncrement(SyncObjsId));

  _AddSyncObjsInfo(soWaitForSingleObject, sosEnter, Id, NativeUInt(Pointer(lpHandles)));

  Result := Kernel32_WaitForMultipleObjects(nCount, lpHandles, bWaitAll, dwMilliseconds);

  _AddSyncObjsInfo(soLeaveCriticalSection, sosLeave, Id, NativeUInt(Pointer(lpHandles)));
end;

var
  _PeMapImgHooks: TJclPeMapImgHooks = Nil;

function _HookSyncObjs(ImageBase: Pointer): Boolean;
var
  ProcAddr: Pointer;
  H: THandle;
begin
  if not SyncObjsHooked then
  begin
    SyncObjsLock := TDbgCriticalSection.Create;

    H := GetModuleHandle(kernel32);

    _PeMapImgHooks := TJclPeMapImgHooks.Create;

    SyncObjsLock.Enter;
    try
      ProcAddr := GetProcAddress(H, 'Sleep');
      if _PeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddr, @_HookSleep) then
      begin
        @Kernel32_Sleep := ProcAddr;
        OutputDebugStringA('Hook Sleep - ok');
      end;

      ProcAddr := GetProcAddress(H, 'WaitForSingleObject');
      if _PeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddr, @_HookWaitForSingleObject) then
      begin
        @Kernel32_WaitForSingleObject := ProcAddr;
        OutputDebugStringA('Hook WaitForSingleObject - ok');
      end;

      ProcAddr := GetProcAddress(H, 'WaitForMultipleObjects');
      if _PeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddr, @_HookWaitForMultipleObjects) then
      begin
        @Kernel32_WaitForMultipleObjects := ProcAddr;
        OutputDebugStringA('Hook WaitForMultipleObjects - ok');
      end;

      ProcAddr := GetProcAddress(H, 'EnterCriticalSection');
      if _PeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddr, @_HookEnterCriticalSection) then
      begin
        @Kernel32_EnterCriticalSection := ProcAddr;
        OutputDebugStringA('Hook EnterCriticalSection - ok');
      end;

      ProcAddr := GetProcAddress(H, 'LeaveCriticalSection');
      if _PeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddr, @_HookLeaveCriticalSection) then
      begin
        @Kernel32_LeaveCriticalSection := ProcAddr;
        OutputDebugStringA('Hook LeaveCriticalSection - ok');
      end;

      SyncObjsHooked := True;
      Result := True;
    finally
      SyncObjsLock.Leave;
    end;
  end
  else
    Result := True;
end;

procedure _UnHookSyncObjs;
begin
  if SyncObjsHooked then
  begin
    _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_Sleep);
    _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_EnterCriticalSection);
    _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_LeaveCriticalSection);
    _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_WaitForSingleObject);
    _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_WaitForMultipleObjects);
  end;

  FreeAndNil(_PeMapImgHooks);
end;

function InitSyncObjsHook(ImageBase: Pointer): Boolean; stdcall;
begin
  _SyncObjsInfoList := AllocMem(SizeOf(TDbgSyncObjsInfoList));
  SyncObjsLock := TDbgCriticalSection.Create;

  Result := _HookSyncObjs(ImageBase);
  if Result then
    OutputDebugStringA('Init SyncObjs hooks - ok')
  else
    OutputDebugStringA('Init SyncObjs hooks - fail')
end;

procedure ResetSyncObjsHook; stdcall;
begin
  if SyncObjsHooked then
  begin
    while not SyncObjsLock.TryEnter do
      SwitchToThread;
    try
      _UnHookSyncObjs;
    finally
      SyncObjsLock.Leave;
    end;

    while not SyncObjsLock.TryEnter do
      SwitchToThread;
    try
      _OutSyncObjsInfo;

      FreeMemory(_SyncObjsInfoList);
      _SyncObjsInfoList := nil;
    finally
      SyncObjsLock.Leave;
    end;
  end;

  FreeAndNil(SyncObjsLock);

  OutputDebugStringA('Reset SyncObjs hook - ok')
end;

end.
