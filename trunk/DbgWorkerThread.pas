unit DbgWorkerThread;

interface

uses System.Classes, System.SysUtils;

type
  TDbgWorkerThread = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Stop;

    class procedure Init; static;
    class procedure Reset; static;
  end;

var
  gvDbgWorkerThread: TDbgWorkerThread = Nil;

implementation

uses Debuger;

{ TDbgWorkerThread }

constructor TDbgWorkerThread.Create;
begin
  inherited Create(False);
  FreeOnTerminate := False;
end;

destructor TDbgWorkerThread.Destroy;
begin
  inherited;
end;

procedure TDbgWorkerThread.Execute;
var
  HasNext: LongBool;
begin
  NameThreadForDebugging(ClassName);

  repeat
    HasNext := False;

    if Assigned(gvDebuger) then
    begin
      HasNext := gvDebuger.DbgSamplingProfiler.ProcessSamplingInfo;

      HasNext := gvDebuger.DbgMemoryProfiler.ProcessMemoryInfoQueue or HasNext;
      HasNext := gvDebuger.DbgSysncObjsProfiler.ProcessSyncObjsInfoQueue or HasNext;

      if not HasNext then
        Sleep(10);
    end;
  until Terminated and not(HasNext);
end;

class procedure TDbgWorkerThread.Init;
begin
  if gvDebuger.CodeTracking or
    gvDebuger.DbgMemoryProfiler.MemoryCheckMode or
    gvDebuger.DbgSysncObjsProfiler.SyncObjsTracking
  then
  begin
    if gvDbgWorkerThread = Nil then
      gvDbgWorkerThread := TDbgWorkerThread.Create;
  end;
end;

class procedure TDbgWorkerThread.Reset;
begin
  if Assigned(gvDbgWorkerThread) then
  begin
    gvDbgWorkerThread.Stop;
    FreeAndNil(gvDbgWorkerThread);
  end;
end;

procedure TDbgWorkerThread.Stop;
begin
  Terminate;
  WaitFor;
end;


end.
