unit DebugHook;

interface

uses Windows, DebugInfo;

function LoadDbgHookDll(hProcess: THandle; const DllPath: String; ImageBase: Pointer; MemoryMgr: TVarInfo;
  MemoryCallStack: LongBool; SyncObjsHook: LongBool): LongBool;
function UnLoadDbgHookDll(hProcess: THandle; const DllPath: String): LongBool;

implementation

uses Debuger;

type
  TInfoName = array[0..31] of AnsiChar;
  TInfoPathName = array[0..(MAX_PATH - 1)] of AnsiChar;
  TStrTemp = array[0..255] of AnsiChar;

  PDbgLoaderInfo = ^TDbgLoaderInfo;
  TDbgLoaderInfo = record
    LoadLibrary    : function(lpLibFileName: PAnsiChar): HMODULE; stdcall;
    GetProcAddress : function(hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;
    OutputDebugStringA : procedure (lpOutputString: LPCSTR); stdcall;
    lstrcpyA: function (lpString1, lpString2: LPCSTR): LPSTR; stdcall;
    lstrlenA: function (lpString: LPCSTR): Integer; stdcall;

    sKernel32      : TInfoName;
    sUser32        : TInfoName;
    sExitThread    : TInfoName;
    sDllPath       : TInfoPathName;

    sDllProcThreadHook: TInfoName;
    sDllProcMemoryHook: TInfoName;
    sDllProcPerfomance: TInfoName;
    sDllProcSyncObjsHook: TInfoName;

    slstrcpyA: TInfoName;
    slstrlenA: TInfoName;

    sTemp: TStrTemp;

    ImageBase        : Pointer;
    MemoryMgr        : Pointer;
    MemoryCallStack  : LongBool;
    PerfDelta        : Cardinal;

    SyncObjsHook     : LongBool;
  end;

procedure _DbgLoader(DbgLoaderInfo: PDbgLoaderInfo); stdcall;
var
  HLib: HMODULE;

  ExitThread: procedure(uExitCode: UINT); stdcall;
  InitThreadHook: function(ImageBase: Pointer): LongBool; stdcall;
  InitSyncObjsHook: function(ImageBase: Pointer): LongBool; stdcall;
  InitMemoryHook: procedure(MemoryMgr: Pointer; MemoryCallStack: LongBool); stdcall;
  InitPerfomance: procedure(Delta: Cardinal); stdcall;
begin
  if DbgLoaderInfo = nil then Exit;

  with DbgLoaderInfo^ do
  begin
    HLib := LoadLibrary(sKernel32);

    if HLib = 0 then Exit;

    @ExitThread := GetProcAddress(HLib, sExitThread);

    if @ExitThread = nil then Exit;

    HLib := LoadLibrary(sDllPath);
    if HLib <> 0 then
    begin
      @InitThreadHook := GetProcAddress(HLib, sDllProcThreadHook);
      @InitSyncObjsHook := GetProcAddress(HLib, sDllProcSyncObjsHook);
      @InitMemoryHook := GetProcAddress(HLib, sDllProcMemoryHook);
      @InitPerfomance := GetProcAddress(HLib, sDllProcPerfomance);

      if (@InitThreadHook <> nil) and InitThreadHook(ImageBase) then
      begin
        // 1 - хуки на системные функции
        if SyncObjsHook and (@InitSyncObjsHook <> nil) then
          InitSyncObjsHook(ImageBase);

        // 2 - перекрываем менеджер памяти
        if (@InitMemoryHook <> nil) and (MemoryMgr <> Nil) then
          InitMemoryHook(MemoryMgr, MemoryCallStack);

        // 3 - запускаем поток обработки дебажной информации
        if (@InitPerfomance <> nil) then
          InitPerfomance(PerfDelta);
      end;

      ExitThread(0);
    end
    else
      ExitThread(1);
  end;
end;
procedure _DbgLoaderEnd; begin end;

function LoadDbgHookDll(hProcess: THandle; const DllPath: String; ImageBase: Pointer; MemoryMgr: TVarInfo;
  MemoryCallStack: LongBool; SyncObjsHook: LongBool): LongBool;
var
  DbgLoaderInfo: TDbgLoaderInfo;
  hKernel32: THandle;
begin
  ZeroMemory(@DbgLoaderInfo, SizeOf(TDbgLoaderInfo));

  DbgLoaderInfo.ImageBase := ImageBase;

  if Assigned(MemoryMgr) then
    DbgLoaderInfo.MemoryMgr := Pointer(MemoryMgr.Offset)
  else
    DbgLoaderInfo.MemoryMgr := nil;

  DbgLoaderInfo.MemoryCallStack := MemoryCallStack;
  DbgLoaderInfo.PerfDelta := 10;

  DbgLoaderInfo.SyncObjsHook := SyncObjsHook;

  hKernel32 := GetModuleHandle('kernel32.dll');
  @DbgLoaderInfo.LoadLibrary    := GetProcAddress(hKernel32, 'LoadLibraryA');
  @DbgLoaderInfo.GetProcAddress := GetProcAddress(hKernel32, 'GetProcAddress');
  @DbgLoaderInfo.OutputDebugStringA := GetProcAddress(hKernel32, 'OutputDebugStringA');;
  @DbgLoaderInfo.lstrcpyA := GetProcAddress(hKernel32, 'lstrcpyA');
  @DbgLoaderInfo.lstrlenA := GetProcAddress(hKernel32, 'lstrlenA');

  lstrcpyA(DbgLoaderInfo.sKernel32, 'kernel32.dll');
  lstrcpyA(DbgLoaderInfo.sUser32, 'user32.dll');
  lstrcpyA(DbgLoaderInfo.sExitThread, 'ExitThread');
  lstrcpyA(DbgLoaderInfo.sDllPath, PAnsiChar(AnsiString(DllPath)));
  lstrcpyA(DbgLoaderInfo.sDllProcThreadHook, 'InitThreadHook');
  lstrcpyA(DbgLoaderInfo.sDllProcSyncObjsHook, 'InitSyncObjsHook');
  lstrcpyA(DbgLoaderInfo.sDllProcMemoryHook, 'InitMemoryHook');
  lstrcpyA(DbgLoaderInfo.sDllProcPerfomance, 'InitPerfomance');

  try
    gvDebuger.InjectThread(hProcess,
      @_DbgLoader, Cardinal(@_DbgLoaderEnd) - Cardinal(@_DbgLoader),
      @DbgLoaderInfo, SizeOf(TDbgLoaderInfo), False);

    Result := True;
  except
    Raise;
  end;
end;

type
  PDbgUnLoaderInfo = ^TDbgLoaderInfo;
  TDbgUnLoaderInfo = record
    LoadLibrary    : function(lpLibFileName: PAnsiChar): HMODULE; stdcall;
    GetProcAddress : function(hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;
    sKernel32      : TInfoName;
    sUser32        : TInfoName;
    sExitThread    : TInfoName;
    sDllPath       : TInfoPathName;

    sDllResetThreadHook: TInfoName;
    sDllResetSyncObjsHook: TInfoName;
    sDllResetMemoryHook: TInfoName;
    sDllResetPerfomance: TInfoName;
  end;

procedure _DbgUnLoader(DbgLoaderInfo: PDbgLoaderInfo); stdcall;
var
  HLib: HMODULE;

  ExitThread: procedure(uExitCode: UINT); stdcall;
  ResetThreadHook: procedure; stdcall;
  ResetSyncObjsHook: procedure; stdcall;
  ResetMemoryHook: procedure; stdcall;
  ResetPerfomance: procedure; stdcall;
begin
  with DbgLoaderInfo^ do
  begin
    HLib := LoadLibrary(sKernel32);
    @ExitThread := GetProcAddress(HLib, sExitThread);

    HLib := LoadLibrary(sDllPath);
    @ResetThreadHook := GetProcAddress(HLib, sDllProcThreadHook);
    @ResetSyncObjsHook := GetProcAddress(HLib, sDllProcSyncObjsHook);
    @ResetMemoryHook := GetProcAddress(HLib, sDllProcMemoryHook);
    @ResetPerfomance := GetProcAddress(HLib, sDllProcPerfomance);

    if @ResetPerfomance <> Nil then
      ResetPerfomance();

    if @ResetMemoryHook <> Nil then
      ResetMemoryHook();

    if @ResetSyncObjsHook <> Nil then
      ResetSyncObjsHook();

    if @ResetThreadHook <> Nil then
      ResetThreadHook();

    ExitThread(0);
  end;
end;
procedure _DbgUnLoaderEnd; begin end;

function UnLoadDbgHookDll(hProcess: THandle; const DllPath: String): LongBool;
var
  DbgUnLoaderInfo: TDbgLoaderInfo;
begin
  Result := False;

  ZeroMemory(@DbgUnLoaderInfo, SizeOf(TDbgUnLoaderInfo));

  @DbgUnLoaderInfo.LoadLibrary    := GetProcAddress(GetModuleHandle('kernel32.dll'), 'LoadLibraryA');
  @DbgUnLoaderInfo.GetProcAddress := GetProcAddress(GetModuleHandle('kernel32.dll'), 'GetProcAddress');

  lstrcpyA(DbgUnLoaderInfo.sKernel32, 'kernel32.dll');
  lstrcpyA(DbgUnLoaderInfo.sUser32, 'user32.dll');
  lstrcpyA(DbgUnLoaderInfo.sExitThread, 'ExitThread');
  lstrcpyA(DbgUnLoaderInfo.sDllPath, PAnsiChar(AnsiString(DllPath)));
  lstrcpyA(DbgUnLoaderInfo.sDllProcThreadHook, 'ResetThreadHook');
  lstrcpyA(DbgUnLoaderInfo.sDllProcSyncObjsHook, 'ResetSyncObjsHook');
  lstrcpyA(DbgUnLoaderInfo.sDllProcMemoryHook, 'ResetMemoryHook');
  lstrcpyA(DbgUnLoaderInfo.sDllProcPerfomance, 'ResetPerfomance');

  try
    gvDebuger.InjectThread(hProcess,
      @_DbgLoader, Cardinal(@_DbgLoaderEnd) - Cardinal(@_DbgLoader),
      @DbgUnLoaderInfo, SizeOf(TDbgUnLoaderInfo), False);

    Result := True;
  except
    // Raise;
  end;
end;

end.
