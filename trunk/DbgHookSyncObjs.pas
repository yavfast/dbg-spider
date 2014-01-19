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

uses DbgHookCS, SysUtils, Classes, DbgHookTypes, WinAPIUtils, JclPEImage{TODO: Remove JCL}, DBGHookUtils;

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

      // сброс указателей на нулевой элемент
      _SyncObjsInfoListCnt := 0;
    end;
  finally
    SyncObjsLock.Leave;
  end;
end;

(*
procedure _AddToSyncObjsAdvInfo(const Data: NativeUInt);
begin
  _SyncObjsAdvInfoList^[_SyncObjsAdvInfoListCnt] := Data;

  Inc(_SyncObjsAdvInfoListCnt);
end;

procedure _AddCSAdvInfo(const Id: NativeUInt; const Data: PRTLCriticalSection);
var
  SizeIdx: NativeUInt;

  procedure _AddCSDebugInfo(const CSDebugInfo: PRTLCriticalSectionDebug);
  var
    NextCS: PRTLCriticalSection;
  begin
    if Assigned(CSDebugInfo) then
    begin
      _AddToSyncObjsAdvInfo(NativeUInt(CSDebugInfo^.CriticalSection));

      Inc(_SyncObjsAdvInfoList^[SizeIdx]);

      if Assigned(CSDebugInfo^.ProcessLocksList.Flink) then
      begin
        NextCS := PRTLCriticalSection(NativeUInt(CSDebugInfo^.ProcessLocksList.Flink) - SizeOf(PRTLCriticalSection));
        _AddCSDebugInfo(NextCS^.DebugInfo);
      end;
    end;
  end;

begin
  if Assigned(Data) and (Data^.OwningThread <> 0) and (Data^.OwningThread <> GetCurrentThreadId) then
  begin
    _AddToSyncObjsAdvInfo(Id);

    SizeIdx := _SyncObjsAdvInfoListCnt;
    _AddToSyncObjsAdvInfo(0); // Size buf

    _AddCSDebugInfo(Data^.DebugInfo);
  end;
end;

procedure _AddSyncObjsAdvInfo(const DbgSyncObjsType: TDbgSyncObjsType; const Id: NativeUInt; const Data: NativeUInt);
begin
  case DbgSyncObjsType of
    soEnterCriticalSection:
      _AddCSAdvInfo(Id, Pointer(Data));
  end;
end;
*)

procedure _AddCSAdvInfo(const SyncObjsInfo: PDbgSyncObjsInfo);
begin
  case SyncObjsInfo^.SyncObjsType of
    soEnterCriticalSection:
      case SyncObjsInfo^.SyncObjsStateType of
        sosEnter:
          begin
            // Определение потока, который лочит CS
            if IsValidAddr(SyncObjsInfo^.CS) then
              SyncObjsInfo^.OwningThread := SyncObjsInfo^.CS^.OwningThread;
          end;
      end;
    soLeaveCriticalSection:
      case SyncObjsInfo^.SyncObjsStateType of
        sosLeave:
          begin
            // Определение потока, следующего в очереди на CS
            if IsValidAddr(SyncObjsInfo^.CS) then
              SyncObjsInfo^.OwningThread := SyncObjsInfo^.CS^.OwningThread;
          end;
      end;
  end;
end;

procedure _AddSyncObjsAdvInfo(const SyncObjsInfo: PDbgSyncObjsInfo);
begin
  case SyncObjsInfo^.SyncObjsType of
    soEnterCriticalSection, soLeaveCriticalSection:
      _AddCSAdvInfo(SyncObjsInfo);
  end;
end;

procedure _AddSyncObjsInfo(const DbgSyncObjsType: TDbgSyncObjsType; const DbgSyncObjsStateType: TDbgSyncObjsStateType;
  const Id: NativeUInt; const Data: NativeUInt); stdcall;
var
  SyncObjsInfo: PDbgSyncObjsInfo;
  CurTime: Int64;
begin
  CurTime := _QueryPerformanceCounter;

  SyncObjsLock.Enter;
  if _SyncObjsInfoList <> Nil then
  begin
    SyncObjsInfo := @_SyncObjsInfoList^[_SyncObjsInfoListCnt];
    ZeroMemory(SyncObjsInfo, SizeOf(TDbgSyncObjsInfo));

    SyncObjsInfo^.Id := Id;
    SyncObjsInfo^.ThreadId := GetCurrentThreadId;
    SyncObjsInfo^.CurTime := CurTime;
    SyncObjsInfo^.SyncObjsStateType := DbgSyncObjsStateType;
    SyncObjsInfo^.SyncObjsType := DbgSyncObjsType;

    case SyncObjsInfo^.SyncObjsType of
      soSleep:
        SyncObjsInfo^.MSec := Data;
      soWaitForSingleObject:
        SyncObjsInfo^.Handle := THandle(Data);
      soWaitForMultipleObjects:
        SyncObjsInfo^.Handles := PWOHandleArray(Data);
      soEnterCriticalSection,
      soLeaveCriticalSection,
      soInCriticalSection:
        SyncObjsInfo^.CS := PRTLCriticalSection(Data);
    end;

    if DbgSyncObjsStateType = sosEnter then
      GetCallStack(SyncObjsInfo^.Stack, -2);

    _AddSyncObjsAdvInfo(SyncObjsInfo);

    Inc(_SyncObjsInfoListCnt);

    if (_SyncObjsInfoListCnt = _DbgSyncObjsListLength) then
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
  //TODO:
  // Debug CriticalSections: http://msdn.microsoft.com/en-us/magazine/cc164040.aspx

  Id := NativeUInt(InterlockedIncrement(SyncObjsId));

  _AddSyncObjsInfo(soEnterCriticalSection, sosEnter, Id, NativeUInt(@lpCriticalSection));

  Kernel32_EnterCriticalSection(lpCriticalSection);

  _AddSyncObjsInfo(soEnterCriticalSection, sosLeave, Id, NativeUInt(@lpCriticalSection));

  _AddSyncObjsInfo(soInCriticalSection, sosEnter, 0, NativeUInt(@lpCriticalSection));
end;

procedure _HookLeaveCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall;
var
  Id: NativeUInt;
begin
  _AddSyncObjsInfo(soInCriticalSection, sosLeave, 0, NativeUInt(@lpCriticalSection));

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

  _AddSyncObjsInfo(soWaitForSingleObject, sosLeave, Id, NativeUInt(hHandle));
end;

function _HookWaitForMultipleObjects(nCount: DWORD; lpHandles: PWOHandleArray; bWaitAll: BOOL; dwMilliseconds: DWORD): DWORD; stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(InterlockedIncrement(SyncObjsId));

  _AddSyncObjsInfo(soWaitForMultipleObjects, sosEnter, Id, NativeUInt(Pointer(lpHandles)));

  Result := Kernel32_WaitForMultipleObjects(nCount, lpHandles, bWaitAll, dwMilliseconds);

  _AddSyncObjsInfo(soWaitForMultipleObjects, sosLeave, Id, NativeUInt(Pointer(lpHandles)));
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
    Result := False;

    H := GetModuleHandle(kernel32);
    if H = 0 then
    begin
      _Log('GetModuleHandle(kernel32) - fail');
      Exit;
    end;

    SyncObjsLock := TDbgCriticalSection.Create;

    _PeMapImgHooks := TJclPeMapImgHooks.Create;

    SyncObjsLock.Enter;
    try
      ProcAddr := GetProcAddress(H, 'Sleep');
      if Assigned(ProcAddr) and _PeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddr, @_HookSleep) then
      begin
        @Kernel32_Sleep := ProcAddr;
        _Log('Hook Sleep - ok');
      end;

      ProcAddr := GetProcAddress(H, 'WaitForSingleObject');
      if Assigned(ProcAddr) and _PeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddr, @_HookWaitForSingleObject) then
      begin
        @Kernel32_WaitForSingleObject := ProcAddr;
        _Log('Hook WaitForSingleObject - ok');
      end;

      ProcAddr := GetProcAddress(H, 'WaitForMultipleObjects');
      if Assigned(ProcAddr) and _PeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddr, @_HookWaitForMultipleObjects) then
      begin
        @Kernel32_WaitForMultipleObjects := ProcAddr;
        _Log('Hook WaitForMultipleObjects - ok');
      end;

      ProcAddr := GetProcAddress(H, 'EnterCriticalSection');
      if Assigned(ProcAddr) and _PeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddr, @_HookEnterCriticalSection) then
      begin
        @Kernel32_EnterCriticalSection := ProcAddr;
        _Log('Hook EnterCriticalSection - ok');
      end;

      ProcAddr := GetProcAddress(H, 'LeaveCriticalSection');
      if Assigned(ProcAddr) and _PeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddr, @_HookLeaveCriticalSection) then
      begin
        @Kernel32_LeaveCriticalSection := ProcAddr;
        _Log('Hook LeaveCriticalSection - ok');
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
  if SyncObjsHooked and Assigned(_PeMapImgHooks) then
  begin
    if Assigned(@Kernel32_Sleep) then
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_Sleep);
    if Assigned(@Kernel32_EnterCriticalSection) then
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_EnterCriticalSection);
    if Assigned(@Kernel32_LeaveCriticalSection) then
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_LeaveCriticalSection);
    if Assigned(@Kernel32_WaitForSingleObject) then
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_WaitForSingleObject);
    if Assigned(@Kernel32_WaitForMultipleObjects) then
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_WaitForMultipleObjects);

    FreeAndNil(_PeMapImgHooks);
  end;
end;

function InitSyncObjsHook(ImageBase: Pointer): Boolean; stdcall;
begin
  _SyncObjsInfoList := AllocMem(SizeOf(TDbgSyncObjsInfoList));

  SyncObjsLock := TDbgCriticalSection.Create;

  Result := _HookSyncObjs(ImageBase);
  if Result then
    _Log('Init SyncObjs hooks - ok')
  else
    _Log('Init SyncObjs hooks - fail')
end;

procedure ResetSyncObjsHook; stdcall;
begin
  try
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

    _Log('Reset SyncObjs hooks - ok');
  except
    on E: Exception do
      _Log('Reset SyncObjs hooks fail: ' + E.Message);
  end;
end;

end.
