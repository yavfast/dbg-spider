unit WinAPIUtils;

interface

uses Windows, SysUtils;

function _QueryPerformanceCounter: Int64;
function _QueryPerformanceFrequency: Int64;

function _QueryThreadCycleTime(const ThreadHandle: THandle): UInt64;
function _QueryProcessCycleTime(const ProcessHandle: THandle): UInt64;

function _GetThreadId(ThreadHandle: THandle): DWORD;

function GetProcessCPUTime(const hProcess: THandle): UInt64;
function GetThreadCPUTime(const hThread: THandle): UInt64;

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
function FileTimeToInt64(const FileTime: TFileTime): UInt64; inline;
function Int64ToFileTime(const Value: UInt64): TFileTime; inline;
function Int64ToDateTime(const Value: UInt64): TDateTime; inline;

function DebugBreakProcess(Process: THandle): BOOL; stdcall;
function DebugSetProcessKillOnExit(KillOnExit: BOOL): BOOL; stdcall;
function DebugActiveProcessStop(dwProcessId: DWORD): BOOL; stdcall;

function SuspendProcess(const PID: DWORD): LongBool;
function ResumeProcess(const PID: DWORD): LongBool;

function GetFileVersion(const AFileName: string): string;

function GetGUID: String;

implementation

type
  NTSTATUS = LongInt;
  TProcFunction = function(ProcHandle: THandle): NTSTATUS; stdcall;

const
  STATUS_SUCCESS = $00000000;
  PROCESS_SUSPEND_RESUME = $0800;

function QueryThreadCycleTime(ThreadHandle: THandle; CycleTime: PUInt64): BOOL; stdcall; external kernel32 name 'QueryThreadCycleTime';
function QueryProcessCycleTime(ProcessHandle: THandle; CycleTime: PUInt64): BOOL; stdcall; external kernel32 name 'QueryProcessCycleTime';
//function SuspendProcess(ProcessHandle: THandle): NTSTATUS; stdcall; external kernel32 name 'NtSuspendProcess';
//function ResumeProcess(ProcessHandle: THandle): NTSTATUS; stdcall; external kernel32 name 'NtResumeProcess';
function DebugBreakProcess(Process: THandle): BOOL; stdcall; external kernel32 name 'DebugBreakProcess';
function DebugSetProcessKillOnExit(KillOnExit: BOOL): BOOL; stdcall; external kernel32;
function DebugActiveProcessStop(dwProcessId: DWORD): BOOL; stdcall; external kernel32;
function GetThreadId(ThreadHandle: THandle): DWORD; stdcall; external kernel32 name 'GetThreadId';
function CoCreateGuid(out guid: TGUID): HResult; stdcall; external 'ole32.dll' name 'CoCreateGuid';

var
  _KernelLibHandle: THandle = 0;
  NtSuspendProcess: TProcFunction = Nil;
  NtResumeProcess: TProcFunction = Nil;

procedure _LoadKernelProcs;
begin
  _KernelLibHandle := SafeLoadLibrary('ntdll.dll');
  if _KernelLibHandle <> 0 then
  begin
    @NtSuspendProcess := GetProcAddress(_KernelLibHandle, 'NtSuspendProcess');
    @NtResumeProcess := GetProcAddress(_KernelLibHandle, 'NtResumeProcess');
  end;
end;

function SuspendProcess(const PID: DWORD): LongBool;
var
  ProcHandle: THandle;
begin
  Result := False;

  if @NtSuspendProcess <> nil then
  begin
    ProcHandle := OpenProcess(PROCESS_SUSPEND_RESUME, False, PID);
    if ProcHandle <> 0 then
    begin
      Result := NtSuspendProcess(ProcHandle) = STATUS_SUCCESS;
      CloseHandle(ProcHandle);
    end;
  end;
end;

function ResumeProcess(const PID: DWORD): LongBool;
var
  ProcHandle: THandle;
begin
  Result := False;

  if @NtResumeProcess <> nil then
  begin
    ProcHandle := OpenProcess(PROCESS_SUSPEND_RESUME, False, PID);
    if ProcHandle <> 0 then
    begin
      Result := NtResumeProcess(ProcHandle) = STATUS_SUCCESS;
      CloseHandle(ProcHandle);
    end;
  end;
end;

function GetGUID: String;
var
  G: TGUID;
begin
  CoCreateGuid(G);
  Result := GUIDToString(G);
  Result := Copy(Result, 2, Length(Result) - 2);
end;

var
  _AppVersion: String = '';

function GetFileVersion(const AFileName: string): String;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
  Major1, Major2, Minor1, Minor2: Cardinal;
begin
  Result := _AppVersion;

  if Result <> '' then Exit;
  
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
        begin
          Major1 := FI.dwFileVersionMS shr 16;
          Major2 := FI.dwFileVersionMS and $FFFF;
          Minor1 := FI.dwFileVersionLS shr 16;
          Minor2 := FI.dwFileVersionLS and $FFFF;

          _AppVersion := Format('%d.%d.%d.%d', [Major1, Major2, Minor1, Minor2]);
          Result := _AppVersion;
        end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

type
  EWinAPIException = class(Exception);

procedure RaiseWinAPIException;
begin
  raise EWinAPIException.Create('');
end;

function _QueryThreadCycleTime(const ThreadHandle: THandle): UInt64;
begin
  Result := 0;

  if not QueryThreadCycleTime(ThreadHandle, @Result) then
    RaiseWinAPIException;
end;

function _QueryProcessCycleTime(const ProcessHandle: THandle): UInt64;
begin
  Result := 0;

  if not QueryProcessCycleTime(ProcessHandle, @Result) then
    RaiseWinAPIException;
end;

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
var
  SystemTime: TSystemTime;
begin
  Result := 0;

  if FileTimeToSystemTime(@FileTime, SystemTime) then
  begin
    Result := SystemTimeToDateTime(SystemTime);
    Exit;
  end;

  RaiseWinAPIException;
end;

function FileTimeToInt64(const FileTime: TFileTime): UInt64;
begin
  Result := UInt64(UInt64(FileTime.dwHighDateTime) shl 32) or FileTime.dwLowDateTime;
end;

function Int64ToFileTime(const Value: UInt64): TFileTime;
begin
  Result.dwLowDateTime := DWORD(Value);
  Result.dwHighDateTime := DWORD(Value shr 32);
end;

function Int64ToDateTime(const Value: UInt64): TDateTime;
begin
  Result := FileTimeToDateTime(Int64ToFileTime(Value));
end;

type
  PCPUTime = ^RCPUTime;
  RCPUTime = record
    CT, ET, KT, UT: TFileTime;
  end;

threadvar
  _CPUTimeRes: RCPUTime;

function GetProcessCPUTime(const hProcess: THandle): UInt64;
begin
  Result := 0;

  with _CPUTimeRes do
    if GetProcessTimes(hProcess, CT, ET, KT, UT) then
      Result := FileTimeToInt64(KT) + FileTimeToInt64(UT)
    else
      RaiseWinAPIException;
end;

function GetThreadCPUTime(const hThread: THandle): UInt64;
begin
  Result := 0;

  with _CPUTimeRes do
    if GetThreadTimes(hThread, CT, ET, KT, UT) then
      Result := FileTimeToInt64(KT) + FileTimeToInt64(UT)
    else
      RaiseWinAPIException;
end;

function _QueryPerformanceCounter: Int64;
begin
  Result := 0;

  if not QueryPerformanceCounter(Result) then
    RaiseWinAPIException;
end;

function _QueryPerformanceFrequency: Int64;
begin
  Result := 0;

  if not QueryPerformanceFrequency(Result) then
    RaiseWinAPIException;
end;

function _GetThreadId(ThreadHandle: THandle): DWORD;
begin
  Result := GetThreadId(ThreadHandle);
end;

initialization
  _LoadKernelProcs;

end.
