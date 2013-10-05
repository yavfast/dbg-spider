unit WinAPIUtils;

interface

uses Windows, SysUtils;

function _QueryPerformanceCounter: Int64;
function _QueryPerformanceFrequency: Int64;

function _QueryThreadCycleTime(const ThreadHandle: THandle): UInt64;
function _QueryProcessCycleTime(const ProcessHandle: THandle): UInt64;

function GetProcessCPUTime(const hProcess: THandle): UInt64;
function GetThreadCPUTime(const hThread: THandle): UInt64;

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
function FileTimeToInt64(const FileTime: TFileTime): UInt64;
function Int64ToFileTime(const Value: UInt64): TFileTime;
function Int64ToDateTime(const Value: UInt64): TDateTime;

function DebugBreakProcess(Process: THandle): BOOL; stdcall;
function DebugSetProcessKillOnExit(KillOnExit: BOOL): BOOL; stdcall;
function DebugActiveProcessStop(dwProcessId: DWORD): BOOL; stdcall;

implementation

function QueryThreadCycleTime(ThreadHandle: THandle; CycleTime: PUInt64): BOOL; stdcall; external kernel32 name 'QueryThreadCycleTime';
function QueryProcessCycleTime(ProcessHandle: THandle; CycleTime: PUInt64): BOOL; stdcall; external kernel32 name 'QueryProcessCycleTime';
function DebugBreakProcess(Process: THandle): BOOL; stdcall; external kernel32 name 'DebugBreakProcess';
function DebugSetProcessKillOnExit(KillOnExit: BOOL): BOOL; stdcall; external kernel32;
function DebugActiveProcessStop(dwProcessId: DWORD): BOOL; stdcall; external kernel32;

type
  EWinAPIException = class(Exception);

procedure RaiseWinAPIException;
begin
  raise EWinAPIException.Create('');
end;

function _QueryThreadCycleTime(const ThreadHandle: THandle): UInt64;
var
  Res: PUInt64;
begin
  GetMem(Res, SizeOf(UInt64));

  if QueryThreadCycleTime(ThreadHandle, Res) then
    Result := Res^
  else
    RaiseWinAPIException;

  FreeMem(Res);
end;

function _QueryProcessCycleTime(const ProcessHandle: THandle): UInt64;
var
  Res: PUInt64;
begin
  GetMem(Res, SizeOf(UInt64));

  if QueryProcessCycleTime(ProcessHandle, Res) then
    Result := Res^
  else
    RaiseWinAPIException;

  FreeMem(Res);
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

function GetProcessCPUTime(const hProcess: THandle): UInt64;
var
  FT: PCPUTime;
begin
  Result := 0;
  GetMem(FT, SizeOf(RCPUTime));

  with FT^ do
    if GetProcessTimes(hProcess, CT, ET, KT, UT) then
      Result := FileTimeToInt64(KT) + FileTimeToInt64(UT)
    else
      RaiseWinAPIException;

  FreeMem(FT);
end;

function GetThreadCPUTime(const hThread: THandle): UInt64;
var
  FT: PCPUTime;
begin
  Result := 0;
  GetMem(FT, SizeOf(RCPUTime));

  with FT^ do
    if GetThreadTimes(hThread, CT, ET, KT, UT) then
      Result := FileTimeToInt64(KT) + FileTimeToInt64(UT)
    else
      RaiseWinAPIException;

  FreeMem(FT);
end;

function _QueryPerformanceCounter: Int64;
var
  Res: PInt64;
begin
  GetMem(Res, SizeOf(Int64));

  if QueryPerformanceCounter(Res^) then
    Result := Res^
  else
    RaiseWinAPIException;

  FreeMem(Res);
end;

function _QueryPerformanceFrequency: Int64;
var
  Res: PInt64;
begin
  GetMem(Res, SizeOf(Int64));

  if QueryPerformanceFrequency(Res^) then
    Result := Res^
  else
    RaiseWinAPIException;

  FreeMem(Res);
end;


end.
