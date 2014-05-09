unit DbgHookSyncObjs;

interface

uses Windows;

type
  TKernel32_Sleep =
    procedure (milliseconds: Cardinal); stdcall;
  TKernel32_SleepEx =
    function (dwMilliseconds: DWORD; bAlertable: BOOL): DWORD; stdcall;

  TKernel32_EnterCriticalSection =
    procedure (var lpCriticalSection: TRTLCriticalSection); stdcall;
  TKernel32_LeaveCriticalSection =
    procedure (var lpCriticalSection: TRTLCriticalSection); stdcall;

  TKernel32_WaitForSingleObject =
    function (hHandle: THandle; dwMilliseconds: DWORD): DWORD; stdcall;
  TKernel32_WaitForSingleObjectEx =
    function (hHandle: THandle; dwMilliseconds: DWORD; bAlertable: BOOL): DWORD; stdcall;
  TKernel32_WaitForMultipleObjects =
    function (nCount: DWORD; lpHandles: PWOHandleArray; bWaitAll: BOOL; dwMilliseconds: DWORD): DWORD; stdcall;
  TKernel32_WaitForMultipleObjectsEx =
    function (nCount: DWORD; lpHandles: PWOHandleArray; bWaitAll: BOOL; dwMilliseconds: DWORD; bAlertable: BOOL): DWORD; stdcall;

  TKernel32_SendMessageA =
    function (hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
  TKernel32_SendMessageW =
    function (hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
  TKernel32_SendMessageTimeoutA =
    function (hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; fuFlags, uTimeout: UINT; lpdwResult: PDWORD_PTR): LRESULT; stdcall;
  TKernel32_SendMessageTimeoutW =
    function (hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; fuFlags, uTimeout: UINT; lpdwResult: PDWORD_PTR): LRESULT; stdcall;


var
  SyncObjsHooked: Boolean = False;

  Kernel32_Sleep: TKernel32_Sleep = nil;
  Kernel32_SleepEx: TKernel32_SleepEx = nil;

  Kernel32_EnterCriticalSection: TKernel32_EnterCriticalSection = nil;
  Kernel32_LeaveCriticalSection: TKernel32_LeaveCriticalSection = nil;

  Kernel32_WaitForSingleObject: TKernel32_WaitForSingleObject = nil;
  Kernel32_WaitForSingleObjectEx: TKernel32_WaitForSingleObjectEx = nil;
  Kernel32_WaitForMultipleObjects: TKernel32_WaitForMultipleObjects = nil;
  Kernel32_WaitForMultipleObjectsEx: TKernel32_WaitForMultipleObjectsEx = nil;

  Kernel32_SendMessageA: TKernel32_SendMessageA = Nil;
  Kernel32_SendMessageW: TKernel32_SendMessageW = Nil;
  Kernel32_SendMessageTimeoutA: TKernel32_SendMessageTimeoutA = Nil;
  Kernel32_SendMessageTimeoutW: TKernel32_SendMessageTimeoutW = Nil;

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
              SyncObjsInfo^.OwningThreadId := SyncObjsInfo^.CS^.OwningThread;
          end;
      end;
    soLeaveCriticalSection:
      case SyncObjsInfo^.SyncObjsStateType of
        sosLeave:
          begin
            // Определение потока, следующего в очереди на CS
            if IsValidAddr(SyncObjsInfo^.CS) then
              SyncObjsInfo^.OwningThreadId := SyncObjsInfo^.CS^.OwningThread;
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
        begin
          SyncObjsInfo^.CS := PRTLCriticalSection(Data);
          SyncObjsInfo^.OwningThreadId := 0;
        end;
      soSendMessage:
        SyncObjsInfo^.Msg := UINT(Data);
    end;

    if (DbgSyncObjsStateType = sosEnter) or (SyncObjsInfo^.SyncObjsType = soInCriticalSection) then
    begin
      //GetCallStack(SyncObjsInfo^.Stack, -2);
      GetCallStackOS(SyncObjsInfo^.Stack, 3);
    end;

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

function _HookSleepEx(dwMilliseconds: DWORD; bAlertable: BOOL): DWORD; stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(InterlockedIncrement(SyncObjsId));

  _AddSyncObjsInfo(soSleep, sosEnter, Id, dwMilliseconds);

  Result := Kernel32_SleepEx(dwMilliseconds, bAlertable);

  _AddSyncObjsInfo(soSleep, sosLeave, Id, dwMilliseconds);
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

function _HookWaitForSingleObjectEx(hHandle: THandle; dwMilliseconds: DWORD; bAlertable: BOOL): DWORD; stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(InterlockedIncrement(SyncObjsId));

  _AddSyncObjsInfo(soWaitForSingleObject, sosEnter, Id, NativeUInt(hHandle));

  Result := Kernel32_WaitForSingleObjectEx(hHandle, dwMilliseconds, bAlertable);

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

function _HookWaitForMultipleObjectsEx(nCount: DWORD; lpHandles: PWOHandleArray; bWaitAll: BOOL; dwMilliseconds: DWORD; bAlertable: BOOL): DWORD; stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(InterlockedIncrement(SyncObjsId));

  _AddSyncObjsInfo(soWaitForMultipleObjects, sosEnter, Id, NativeUInt(Pointer(lpHandles)));

  Result := Kernel32_WaitForMultipleObjectsEx(nCount, lpHandles, bWaitAll, dwMilliseconds, bAlertable);

  _AddSyncObjsInfo(soWaitForMultipleObjects, sosLeave, Id, NativeUInt(Pointer(lpHandles)));
end;

function _HookSendMessageA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(InterlockedIncrement(SyncObjsId));

  _AddSyncObjsInfo(soSendMessage, sosEnter, Id, NativeUInt(Msg));

  Result := Kernel32_SendMessageA(hWnd, Msg, wParam, lParam);

  _AddSyncObjsInfo(soSendMessage, sosLeave, Id, NativeUInt(Msg));
end;

function _HookSendMessageW(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(InterlockedIncrement(SyncObjsId));

  _AddSyncObjsInfo(soSendMessage, sosEnter, Id, NativeUInt(Msg));

  Result := Kernel32_SendMessageW(hWnd, Msg, wParam, lParam);

  _AddSyncObjsInfo(soSendMessage, sosLeave, Id, NativeUInt(Msg));
end;

function _HookSendMessageTimeoutA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; fuFlags, uTimeout: UINT; lpdwResult: PDWORD_PTR): LRESULT; stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(InterlockedIncrement(SyncObjsId));

  _AddSyncObjsInfo(soSendMessage, sosEnter, Id, NativeUInt(Msg));

  Result := Kernel32_SendMessageTimeoutA(hWnd, Msg, wParam, lParam, fuFlags, uTimeout, lpdwResult);

  _AddSyncObjsInfo(soSendMessage, sosLeave, Id, NativeUInt(Msg));
end;

function _HookSendMessageTimeoutW(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; fuFlags, uTimeout: UINT; lpdwResult: PDWORD_PTR): LRESULT; stdcall;
var
  Id: NativeUInt;
begin
  Id := NativeUInt(InterlockedIncrement(SyncObjsId));

  _AddSyncObjsInfo(soSendMessage, sosEnter, Id, NativeUInt(Msg));

  Result := Kernel32_SendMessageTimeoutW(hWnd, Msg, wParam, lParam, fuFlags, uTimeout, lpdwResult);

  _AddSyncObjsInfo(soSendMessage, sosLeave, Id, NativeUInt(Msg));
end;


var
  _PeMapImgHooks: TJclPeMapImgHooks = Nil;
  _hKernel32: THandle = 0;
  _hUser32: THandle = 0;
  _ImageBase: Pointer = Nil;

function _ReplaceImport(ModuleName: PWideChar; lpProcName: LPCSTR; HookProc: Pointer; var BaseProc: Pointer): Boolean;
var
  ProcAddr: Pointer;
  hModule: THandle;
begin
  Result := False;

  hModule := GetModuleHandle(ModuleName);
  if hModule = 0 then
  begin
    _Log(Format('GetModuleHandle "%s" - fail', [String(ModuleName)]));
    Exit;
  end;

  ProcAddr := GetProcAddress(hModule, lpProcName);
  if ProcAddr = Nil then
  begin
    _Log(Format('Hook %s - fail GetProcAddress', [String(lpProcName)]));
    Exit;
  end;

  if _PeMapImgHooks.ReplaceImport(_ImageBase, ModuleName, ProcAddr, HookProc) then
  begin
    BaseProc := ProcAddr;
    Result := True;

    _Log(Format('Hook %s - ok', [String(lpProcName)]));
  end
  else
    // Функция не используется в проекте
    _Log(Format('Hook %s - skip', [String(lpProcName)]));
end;

function _HookSyncObjs(ImageBase: Pointer): Boolean;
begin
  if not SyncObjsHooked then
  begin
    SyncObjsLock := TDbgCriticalSection.Create;

    _PeMapImgHooks := TJclPeMapImgHooks.Create;

    _ImageBase := ImageBase;

    SyncObjsLock.Enter;
    try
      _ReplaceImport(kernel32, 'Sleep', @_HookSleep, @Kernel32_Sleep);
      _ReplaceImport(kernel32, 'SleepEx', @_HookSleepEx, @Kernel32_SleepEx);

      _ReplaceImport(kernel32, 'EnterCriticalSection', @_HookEnterCriticalSection, @Kernel32_EnterCriticalSection);
      _ReplaceImport(kernel32, 'LeaveCriticalSection', @_HookLeaveCriticalSection, @Kernel32_LeaveCriticalSection);

      _ReplaceImport(kernel32, 'WaitForSingleObject', @_HookWaitForSingleObject, @Kernel32_WaitForSingleObject);
      _ReplaceImport(kernel32, 'WaitForSingleObjectEx', @_HookWaitForSingleObjectEx, @Kernel32_WaitForSingleObjectEx);
      _ReplaceImport(kernel32, 'WaitForMultipleObjects', @_HookWaitForMultipleObjects, @Kernel32_WaitForMultipleObjects);
      _ReplaceImport(kernel32, 'WaitForMultipleObjectsEx', @_HookWaitForMultipleObjectsEx, @Kernel32_WaitForMultipleObjectsEx);

      _ReplaceImport(user32, 'SendMessageA', @_HookSendMessageA, @Kernel32_SendMessageA);
      _ReplaceImport(user32, 'SendMessageW', @_HookSendMessageW, @Kernel32_SendMessageW);
      _ReplaceImport(user32, 'SendMessageTimeoutA', @_HookSendMessageTimeoutA, @Kernel32_SendMessageTimeoutA);
      _ReplaceImport(user32, 'SendMessageTimeoutW', @_HookSendMessageTimeoutW, @Kernel32_SendMessageTimeoutW);

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
    if Assigned(@Kernel32_SleepEx) then
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_SleepEx);

    if Assigned(@Kernel32_EnterCriticalSection) then
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_EnterCriticalSection);
    if Assigned(@Kernel32_LeaveCriticalSection) then
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_LeaveCriticalSection);

    if Assigned(@Kernel32_WaitForSingleObject) then
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_WaitForSingleObject);
    if Assigned(@Kernel32_WaitForSingleObjectEx) then
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_WaitForSingleObjectEx);
    if Assigned(@Kernel32_WaitForMultipleObjects) then
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_WaitForMultipleObjects);
    if Assigned(@Kernel32_WaitForMultipleObjectsEx) then
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_WaitForMultipleObjectsEx);

    if Assigned(@Kernel32_SendMessageA) then
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_SendMessageA);
    if Assigned(@Kernel32_SendMessageW) then
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_SendMessageW);
    if Assigned(@Kernel32_SendMessageTimeoutA) then
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_SendMessageTimeoutA);
    if Assigned(@Kernel32_SendMessageTimeoutW) then
      _PeMapImgHooks.UnhookByBaseAddress(@Kernel32_SendMessageTimeoutW);

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
