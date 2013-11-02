unit DbgHookSyncObjs;

interface

function InitSyncObjsHook(ImageBase: Pointer): Boolean; stdcall;
procedure ResetSyncObjsHook; stdcall;

implementation

uses Windows, SysUtils, Classes, SyncObjs, DbgHookTypes, JclPEImage{TODO: Remove JCL};

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
  Kernel32_Sleep: TKernel32_Sleep = nil;
  Kernel32_EnterCriticalSection: TKernel32_EnterCriticalSection = nil;
  Kernel32_LeaveCriticalSection: TKernel32_LeaveCriticalSection = nil;
  Kernel32_WaitForSingleObject: TKernel32_WaitForSingleObject = nil;
  Kernel32_WaitForMultipleObjects: TKernel32_WaitForMultipleObjects = nil;

var
  SyncObjsLock: TCriticalSection = nil;
  SyncObjsHooked: Boolean = False;
  SyncObjsId: Int64 = 0;

type
  PSyncObjsOutDbgInfo = ^TSyncObjsOutDbgInfo;
  TSyncObjsOutDbgInfo = array[0..4] of NativeUInt;

threadvar
  _SyncObjsOutDbgInfo: TSyncObjsOutDbgInfo;

procedure _OutSyncObjsInfo(const DbgSyncObjsType: TDbgSyncObjsType; const DbgSyncObjsStateType: TDbgSyncObjsStateType;
  const Id: NativeUInt; const Data: NativeUInt);
var
  SyncObjsOutDbgInfo: PSyncObjsOutDbgInfo;
begin
  SyncObjsOutDbgInfo := @_SyncObjsOutDbgInfo;
  SyncObjsOutDbgInfo[0] := NativeUInt(dstSyncObjsInfo);
  SyncObjsOutDbgInfo[1] := NativeUInt(DbgSyncObjsType);
  SyncObjsOutDbgInfo[2] := NativeUInt(DbgSyncObjsStateType);
  SyncObjsOutDbgInfo[3] := Id;
  SyncObjsOutDbgInfo[4] := Data;

  RaiseException(DBG_EXCEPTION, 0, 5, @SyncObjsOutDbgInfo[0]);
end;

procedure _HookSleep(milliseconds: Cardinal); stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(TInterlocked.Increment(SyncObjsId));

  _OutSyncObjsInfo(soSleep, sosEnter, Id, milliseconds);

  Kernel32_Sleep(milliseconds);

  _OutSyncObjsInfo(soSleep, sosLeave, Id, milliseconds);
end;

procedure _HookEnterCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(TInterlocked.Increment(SyncObjsId));

  _OutSyncObjsInfo(soEnterCriticalSection, sosEnter, Id, NativeUInt(@lpCriticalSection));

  Kernel32_EnterCriticalSection(lpCriticalSection);

  _OutSyncObjsInfo(soEnterCriticalSection, sosLeave, Id, NativeUInt(@lpCriticalSection));
end;

procedure _HookLeaveCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(TInterlocked.Increment(SyncObjsId));

  _OutSyncObjsInfo(soLeaveCriticalSection, sosEnter, Id, NativeUInt(@lpCriticalSection));

  Kernel32_LeaveCriticalSection(lpCriticalSection);

  _OutSyncObjsInfo(soLeaveCriticalSection, sosLeave, Id, NativeUInt(@lpCriticalSection));
end;

function _HookWaitForSingleObject(hHandle: THandle; dwMilliseconds: DWORD): DWORD; stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(TInterlocked.Increment(SyncObjsId));

  _OutSyncObjsInfo(soWaitForSingleObject, sosEnter, Id, NativeUInt(hHandle));

  Result := Kernel32_WaitForSingleObject(hHandle, dwMilliseconds);

  _OutSyncObjsInfo(soLeaveCriticalSection, sosLeave, Id, NativeUInt(hHandle));
end;

function _HookWaitForMultipleObjects(nCount: DWORD; lpHandles: PWOHandleArray; bWaitAll: BOOL; dwMilliseconds: DWORD): DWORD; stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(TInterlocked.Increment(SyncObjsId));

  _OutSyncObjsInfo(soWaitForSingleObject, sosEnter, Id, NativeUInt(Pointer(lpHandles)));

  Result := Kernel32_WaitForMultipleObjects(nCount, lpHandles, bWaitAll, dwMilliseconds);

  _OutSyncObjsInfo(soLeaveCriticalSection, sosLeave, Id, NativeUInt(Pointer(lpHandles)));
end;

function _HookSyncObjs(ImageBase: Pointer): Boolean;
var
  ProcAddr: Pointer;
  H: THandle;
begin
  if not SyncObjsHooked then
  begin
    SyncObjsLock := TCriticalSection.Create;

    H := GetModuleHandle(kernel32);

    SyncObjsLock.Enter;
    try
      ProcAddr := GetProcAddress(H, 'Sleep');
      if TJclPeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddr, @_HookSleep) then
      begin
        @Kernel32_Sleep := ProcAddr;
        OutputDebugStringA('Hook Sleep - ok');
      end;

      ProcAddr := GetProcAddress(H, 'WaitForSingleObject');
      if TJclPeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddr, @_HookWaitForSingleObject) then
      begin
        @Kernel32_WaitForSingleObject := ProcAddr;
        OutputDebugStringA('Hook WaitForSingleObject - ok');
      end;

      ProcAddr := GetProcAddress(H, 'WaitForMultipleObjects');
      if TJclPeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddr, @_HookWaitForMultipleObjects) then
      begin
        @Kernel32_WaitForMultipleObjects := ProcAddr;
        OutputDebugStringA('Hook WaitForMultipleObjects - ok');
      end;

      ProcAddr := GetProcAddress(H, 'EnterCriticalSection');
      if TJclPeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddr, @_HookEnterCriticalSection) then
      begin
        @Kernel32_EnterCriticalSection := ProcAddr;
        OutputDebugStringA('Hook EnterCriticalSection - ok');
      end;

      ProcAddr := GetProcAddress(H, 'LeaveCriticalSection');
      if TJclPeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddr, @_HookLeaveCriticalSection) then
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

function InitSyncObjsHook(ImageBase: Pointer): Boolean; stdcall;
begin
  Result := _HookSyncObjs(ImageBase);
  if Result then
    OutputDebugStringA('Init SyncObjs hook - ok')
  else
    OutputDebugStringA('Init SyncObjs hook - fail')
end;

procedure ResetSyncObjsHook; stdcall;
begin
  OutputDebugStringA('Reset SyncObjs hook - skip')
  // TODO:
end;

procedure Test;
begin
  //Sleep(1);
  //TryEnterCriticalSection()
end;


end.
