unit DebugHook;

interface

uses Windows;

function RemoteLoadDll(hProcess: THandle; const DllPath, InitProc: String; ImageBase: Pointer): Boolean;

implementation

uses Debuger, JclPeImage;

type
  PDbgLoaderInfo = ^TDbgLoaderInfo;
  TDbgLoaderInfo = record
    ImageBase      : Pointer;
    LoadLibrary    : function(lpLibFileName: PAnsiChar): HMODULE; stdcall;
    GetProcAddress : function(hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;
    sKernel32      : array[0..16] of AnsiChar;
    sUser32        : array[0..16] of AnsiChar;
    sExitThread    : array[0..16] of AnsiChar;
    sDllPath       : array[0..MAX_PATH] of AnsiChar;
    sDllInitProc   : array[0..16] of AnsiChar;
  end;

procedure _DbgLoader(DbgLoaderInfo: PDbgLoaderInfo); stdcall;
var
  ExitThread: procedure(uExitCode: UINT); stdcall;
  InitProc: procedure(ImageBase: Pointer); stdcall;
begin
  with DbgLoaderInfo^ do
  begin
    @ExitThread := GetProcAddress(LoadLibrary(sKernel32), sExitThread);
    @InitProc := GetProcAddress(LoadLibrary(sDllPath), sDllInitProc);
    InitProc(ImageBase);
    ExitThread(0);
  end;
end;
procedure _DbgLoaderEnd; begin end;

function RemoteLoadDll(hProcess: THandle; const DllPath, InitProc: String; ImageBase: Pointer): Boolean;
var
  DbgLoaderInfo: TDbgLoaderInfo;
begin
  ZeroMemory(@DbgLoaderInfo, SizeOf(TDbgLoaderInfo));

  DbgLoaderInfo.ImageBase := ImageBase;

  @DbgLoaderInfo.LoadLibrary    := GetProcAddress(GetModuleHandle('kernel32.dll'), 'LoadLibraryA');
  @DbgLoaderInfo.GetProcAddress := GetProcAddress(GetModuleHandle('kernel32.dll'), 'GetProcAddress');

  lstrcpyA(DbgLoaderInfo.sKernel32, 'kernel32.dll');
  lstrcpyA(DbgLoaderInfo.sUser32, 'user32.dll');
  lstrcpyA(DbgLoaderInfo.sExitThread, 'ExitThread');
  lstrcpyA(DbgLoaderInfo.sDllPath, PAnsiChar(AnsiString(DllPath)));
  lstrcpyA(DbgLoaderInfo.sDllInitProc, PAnsiChar(AnsiString(InitProc)));

  gvDebuger.InjectThread(hProcess,
    @_DbgLoader, Cardinal(@_DbgLoaderEnd) - Cardinal(@_DbgLoader),
    @DbgLoaderInfo, SizeOf(TDbgLoaderInfo));
end;

end.
