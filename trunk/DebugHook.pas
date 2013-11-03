unit DebugHook;

interface

uses Windows, DebugInfo;

function LoadDbgHookDll(hProcess: THandle; const DllPath: String; ImageBase: Pointer; MemoryMgr: TVarInfo; MemoryCallStack: Boolean): Boolean;
function UnLoadDbgHookDll(hProcess: THandle; const DllPath: String): Boolean;

implementation

uses Debuger;

type
  TInfoName = array[0..31] of AnsiChar;
  TInfoPathName = array[0..(MAX_PATH - 1)] of AnsiChar;

  PDbgLoaderInfo = ^TDbgLoaderInfo;
  TDbgLoaderInfo = record
    LoadLibrary    : function(lpLibFileName: PAnsiChar): HMODULE; stdcall;
    GetProcAddress : function(hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;
    sKernel32      : TInfoName;
    sUser32        : TInfoName;
    sExitThread    : TInfoName;
    sDllPath       : TInfoPathName;

    sDllProcThreadHook: TInfoName;
    sDllProcMemoryHook: TInfoName;
    sDllProcPerfomance: TInfoName;
    sDllProcSyncObjsHook: TInfoName;

    ImageBase        : Pointer;
    MemoryMgr        : Pointer;
    MemoryCallStack  : Boolean;
    PerfDelta        : Cardinal;
  end;

procedure _DbgLoader(DbgLoaderInfo: PDbgLoaderInfo); stdcall;
var
  HLib: HMODULE;

  ExitThread: procedure(uExitCode: UINT); stdcall;
  InitThreadHook: function(ImageBase: Pointer): Boolean; stdcall;
  InitSyncObjsHook: function(ImageBase: Pointer): Boolean; stdcall;
  InitMemoryHook: procedure(MemoryMgr: Pointer; MemoryCallStack: Boolean); stdcall;
  InitPerfomance: procedure(Delta: Cardinal); stdcall;
begin
  with DbgLoaderInfo^ do
  begin
    HLib := LoadLibrary(sKernel32);
    @ExitThread := GetProcAddress(HLib, sExitThread);

    HLib := LoadLibrary(sDllPath);
    @InitThreadHook := GetProcAddress(HLib, sDllProcThreadHook);
    @InitSyncObjsHook := GetProcAddress(HLib, sDllProcSyncObjsHook);
    @InitMemoryHook := GetProcAddress(HLib, sDllProcMemoryHook);
    @InitPerfomance := GetProcAddress(HLib, sDllProcPerfomance);

    if InitThreadHook(ImageBase) then
    begin
      InitSyncObjsHook(ImageBase);
      InitPerfomance(PerfDelta);

      if MemoryMgr <> Nil then
        InitMemoryHook(MemoryMgr, MemoryCallStack);
    end;

    ExitThread(0);
  end;
end;
procedure _DbgLoaderEnd; begin end;

function LoadDbgHookDll(hProcess: THandle; const DllPath: String; ImageBase: Pointer; MemoryMgr: TVarInfo; MemoryCallStack: Boolean): Boolean;
var
  DbgLoaderInfo: TDbgLoaderInfo;
begin
  ZeroMemory(@DbgLoaderInfo, SizeOf(TDbgLoaderInfo));

  DbgLoaderInfo.ImageBase := ImageBase;
  DbgLoaderInfo.MemoryMgr := Pointer(MemoryMgr.Offset);
  DbgLoaderInfo.MemoryCallStack := MemoryCallStack;
  DbgLoaderInfo.PerfDelta := 10;

  @DbgLoaderInfo.LoadLibrary    := GetProcAddress(GetModuleHandle('kernel32.dll'), 'LoadLibraryA');
  @DbgLoaderInfo.GetProcAddress := GetProcAddress(GetModuleHandle('kernel32.dll'), 'GetProcAddress');

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

function UnLoadDbgHookDll(hProcess: THandle; const DllPath: String): Boolean;
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
