unit DbgHookPerf;

interface

procedure InitPerfomance(Delta: Cardinal); stdcall;

implementation

uses Windows, Classes, SysUtils, DbgHookTypes;

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
  DbgInfo: array[0..0] of Cardinal;
begin
  NameThreadForDebugging('###Perfomance thread', GetCurrentThreadId);

  DbgInfo[0] := Cardinal(dstPerfomance);
  while not Terminated do
  begin
    Sleep(_Delta);

    try
      RaiseException(DBG_EXCEPTION, 0, 1, @DbgInfo[0]);
    except
    end;
  end;
end;

end.
