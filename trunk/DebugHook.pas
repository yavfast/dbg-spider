unit DebugHook;

interface

uses Windows;

function LoadDbgHookDll(hProcess: THandle; const DllPath: String; ImageBase: Pointer; MemoryMgr: Pointer): Boolean;

implementation

uses Debuger;

type
  PDbgLoaderInfo = ^TDbgLoaderInfo;
  TDbgLoaderInfo = record
    LoadLibrary    : function(lpLibFileName: PAnsiChar): HMODULE; stdcall;
    GetProcAddress : function(hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;
    sKernel32      : array[0..16] of AnsiChar;
    sUser32        : array[0..16] of AnsiChar;
    sExitThread    : array[0..16] of AnsiChar;
    sDllPath       : array[0..MAX_PATH] of AnsiChar;

    sDllInitThreadHook: array[0..16] of AnsiChar;
    sDllInitMemoryHook: array[0..16] of AnsiChar;
    sDllInitPerfomance: array[0..16] of AnsiChar;

    ImageBase      : Pointer;
    MemoryMgr      : Pointer;
    PerfDelta      : Cardinal;
  end;

procedure _DbgLoader(DbgLoaderInfo: PDbgLoaderInfo); stdcall;
var
  HLib: HMODULE;

  ExitThread: procedure(uExitCode: UINT); stdcall;
  InitThreadHook: function(ImageBase: Pointer): Boolean; stdcall;
  InitMemoryHook: procedure(MemoryMgr: Pointer); stdcall;
  InitPerfomance: procedure(Delta: Cardinal); stdcall;
begin
  with DbgLoaderInfo^ do
  begin
    HLib := LoadLibrary(sKernel32);
    @ExitThread := GetProcAddress(HLib, sExitThread);

    HLib := LoadLibrary(sDllPath);
    @InitThreadHook := GetProcAddress(HLib, sDllInitThreadHook);
    @InitMemoryHook := GetProcAddress(HLib, sDllInitMemoryHook);
    @InitPerfomance := GetProcAddress(HLib, sDllInitPerfomance);

    if InitThreadHook(ImageBase) then
    begin
      InitPerfomance(PerfDelta);

      if MemoryMgr <> Nil then
        InitMemoryHook(MemoryMgr);
    end;

    ExitThread(0);
  end;
end;
procedure _DbgLoaderEnd; begin end;

function LoadDbgHookDll(hProcess: THandle; const DllPath: String; ImageBase: Pointer; MemoryMgr: Pointer): Boolean;
var
  DbgLoaderInfo: TDbgLoaderInfo;
begin
  ZeroMemory(@DbgLoaderInfo, SizeOf(TDbgLoaderInfo));

  DbgLoaderInfo.ImageBase := ImageBase;
  DbgLoaderInfo.MemoryMgr := MemoryMgr;
  DbgLoaderInfo.PerfDelta := 10;

  @DbgLoaderInfo.LoadLibrary    := GetProcAddress(GetModuleHandle('kernel32.dll'), 'LoadLibraryA');
  @DbgLoaderInfo.GetProcAddress := GetProcAddress(GetModuleHandle('kernel32.dll'), 'GetProcAddress');

  lstrcpyA(DbgLoaderInfo.sKernel32, 'kernel32.dll');
  lstrcpyA(DbgLoaderInfo.sUser32, 'user32.dll');
  lstrcpyA(DbgLoaderInfo.sExitThread, 'ExitThread');
  lstrcpyA(DbgLoaderInfo.sDllPath, PAnsiChar(AnsiString(DllPath)));
  lstrcpyA(DbgLoaderInfo.sDllInitThreadHook, 'InitThreadHook');
  lstrcpyA(DbgLoaderInfo.sDllInitMemoryHook, 'InitMemoryHook');
  lstrcpyA(DbgLoaderInfo.sDllInitPerfomance, 'InitPerfomance');

  try
    gvDebuger.InjectThread(hProcess,
      @_DbgLoader, Cardinal(@_DbgLoaderEnd) - Cardinal(@_DbgLoader),
      @DbgLoaderInfo, SizeOf(TDbgLoaderInfo), False);

    Result := True;
  except
    Raise;
  end;
end;

end.
