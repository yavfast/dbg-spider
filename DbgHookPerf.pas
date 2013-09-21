unit DbgHookPerf;

interface

procedure InitPerfomance(Delta: Cardinal); stdcall;
procedure ResetPerfomance; stdcall;

implementation

uses Windows, Classes, SysUtils, DbgHookTypes, DbgHookMemory;

type
  TPerfThread = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

var
  _PerfThread: TPerfThread = Nil;
  _Delta: Cardinal = 0;

procedure InitPerfomance(Delta: Cardinal); stdcall;
begin
  _Delta := Delta;
  _PerfThread := TPerfThread.Create;
  OutputDebugString(PWideChar(Format('Init perfomance thread (%d msec) - ok', [_Delta])));
end;

procedure ResetPerfomance; stdcall;
begin
  if _PerfThread = nil then Exit;
  
  OutputDebugStringA('Reset perfomance thread...');

  _PerfThread.Terminate;
  _PerfThread := Nil;

  OutputDebugStringA('Reset perfomance thread - ok');
end;

{ TPerfThread }

constructor TPerfThread.Create;
begin
  inherited Create(True);

  FreeOnTerminate := True;
  Priority := tpTimeCritical;

  Suspended := False;
end;

procedure TPerfThread.Execute;
var
  DbgInfo: Pointer;
begin
  NameThreadForDebugging('### Dbg control thread', GetCurrentThreadId);

  DbgInfo := GetMemory(SizeOf(Pointer)); // для выравнивания по памяти
  DWORD(DbgInfo^) := DWORD(dstPerfomance);
  try
    while not Terminated do
    begin
      Sleep(_Delta);
      //RaiseException(DBG_EXCEPTION, 0, 1, DbgInfo);

      // Сброс буфера по памяти
      if not _OutMemInfoBuf(dstPerfomanceAndMemInfo) then
        RaiseException(DBG_EXCEPTION, 0, 1, DbgInfo);
    end;
  except
    on E: Exception do
      OutputDebugStringW(PWideChar(Format('Fail perfomance thread: [%s] %s', [E.ClassName, E.Message])));
  end;
  FreeMemory(DbgInfo);
end;

end.
