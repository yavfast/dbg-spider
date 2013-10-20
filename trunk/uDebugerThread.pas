unit uDebugerThread;

interface

uses SysUtils, Windows, Classes, DebugerTypes, uActionController;

type
  TDebugerThread = class(TThread)
  private
    FProcessID: TProcessId;
    FDbgOptions: TDbgOptions;

    FDbgInfoLoaded: Boolean;
    FDbgStarted: Boolean;

    procedure OnEndDebug(Sender: TObject);
    procedure OnRip(Sender: TObject; ThreadId: TThreadId; Data: PRIPInfo);
    procedure OnCreateThread(Sender: TObject; ThreadId: TThreadId; Data: PCreateThreadDebugInfo);
    procedure OnExitThread(Sender: TObject; ThreadId: TThreadId; Data: PExitThreadDebugInfo);
    procedure OnCreateProcess(Sender: TObject; ProcessId: TProcessId; Data: PCreateProcessDebugInfo);
    procedure OnExitProcess(Sender: TObject; ProcessId: TProcessId; Data: PExitProcessDebugInfo);
    procedure OnLoadDll(Sender: TObject; ThreadId: TThreadId; Data: PLoadDLLDebugInfo);
    procedure OnUnLoadDll(Sender: TObject; ThreadId: TThreadId; Data: PUnloadDLLDebugInfo);
    procedure OnDebugString(Sender: TObject; ThreadId: TThreadId; Data: POutputDebugStringInfo);
    procedure OnUnknownException(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord);
    procedure OnUnknownBreakPoint(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord);
    procedure OnBreakPoint(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord; BreakPointIndex: Integer; var ReleaseBreakpoint: Boolean);
    procedure OnDbgLog(Sender: TObject; ThreadId: TThreadId; const Data: String);
    procedure OnProgress(const Action: String; const Progress: Integer);

    procedure InitDebuger;
    procedure InitDebugInfo;
    procedure LoadDebugInfo;
    function GetAppName: String;
    function GetProjectSourceDirs: String;
    function GetDelphiSourceDirs: String;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;

    property AppName: String read GetAppName;
    property ProjectSourceDirs: String read GetProjectSourceDirs;
    property DelphiSourceDirs: String read GetDelphiSourceDirs;
  public
    constructor Create(ADbgOptions: TDbgOptions; const AProcessID: TProcessId = 0);
    destructor Destroy; override;
  end;

var
  _DbgThread: TDebugerThread = nil;

implementation

uses Debuger, DebugInfo, DelphiDebugInfo;

{ TDebugerThread }

constructor TDebugerThread.Create(ADbgOptions: TDbgOptions; const AProcessID: TProcessId = 0);
begin
  inherited Create(True);
  FreeOnTerminate := True;

  FDbgOptions := ADbgOptions;
  FProcessID := AProcessID;

  FDbgInfoLoaded := False;
  FDbgStarted := False;

  Priority := tpHighest;

  Suspended := False;
end;

destructor TDebugerThread.Destroy;
begin
  _DbgThread := nil;
  inherited;
end;

procedure TDebugerThread.DoTerminate;
begin
  inherited;

  _DbgThread := Nil;
end;

procedure TDebugerThread.Execute;
var
  FRun: Boolean;
begin
  NameThreadForDebugging(AnsiString(ClassName), ThreadId);

  InitDebuger;
  gvDebuger.ClearDbgInfo;

  InitDebugInfo;
  if doDebugInfo in FDbgOptions then
    LoadDebugInfo;

  if doRun in FDbgOptions then
  begin
    if FProcessID = 0 then
    begin
      _AC.Log(dltInfo, 'Run application "%s"', [AppName]);
      FRun := gvDebuger.DebugNewProcess(AppName, False);
    end
    else
    begin
      _AC.Log(dltInfo, 'Attach to process [%d]', [FProcessID]);
      FRun := gvDebuger.AttachToProcess(FProcessID, False);
    end;

    if FRun then
    begin
      gvDebuger.PerfomanceMode := (doProfiler in FDbgOptions);

      gvDebuger.MemoryCheckMode := (doMemProfiler in FDbgOptions);
      gvDebuger.MemoryCallStack := gvDebuger.MemoryCheckMode and (doMemCallStack in FDbgOptions);
      gvDebuger.MemoryCheckDoubleFree := gvDebuger.MemoryCheckMode and (doMemCheckDoubleFree in FDbgOptions);

      gvDebuger.ExceptionCheckMode := (doExceptions in FDbgOptions);
      gvDebuger.ExceptionCallStack := gvDebuger.ExceptionCheckMode and (doExceptionCallStack in FDbgOptions);

      gvDebuger.CodeTracking := (doCodeTracking in FDbgOptions);
      gvDebuger.TrackSystemUnits := gvDebuger.CodeTracking and (doTrackSystemUnits in FDbgOptions);

      _AC.Log(dltInfo, 'Start debug process');
      try
        gvDebuger.ProcessDebugEvents;
      except
        on E: Exception do
          _AC.Log(dltError, 'Fail debug process: "%s"', [E.Message]);
      end;
    end;
  end
  else
    _AC.DoAction(acRunEnabled, [True]);
end;

function TDebugerThread.GetAppName: String;
begin
  Result := gvProjectOptions.ApplicationName;
end;

function TDebugerThread.GetDelphiSourceDirs: String;
begin
  Result := gvProjectOptions.DelphiSource;
end;

function TDebugerThread.GetProjectSourceDirs: String;
begin
  Result := gvProjectOptions.ProjectSource;
end;

procedure TDebugerThread.InitDebuger;
begin
  if gvDebuger = nil then
    gvDebuger := TDebuger.Create();

  gvDebuger.OnEndDebug := OnEndDebug;
  gvDebuger.OnRip := OnRip;
  gvDebuger.OnCreateProcess := OnCreateProcess;
  gvDebuger.OnExitProcess := OnExitProcess;
  gvDebuger.OnCreateThread := OnCreateThread;
  gvDebuger.OnExitThread := OnExitThread;
  gvDebuger.OnLoadDll := OnLoadDll;
  gvDebuger.OnUnloadDll := OnUnLoadDll;
  gvDebuger.OnDebugString := OnDebugString;
  gvDebuger.OnUnknownException := OnUnknownException;
  gvDebuger.OnUnknownBreakPoint := OnUnknownBreakPoint;
  gvDebuger.OnBreakPoint := OnBreakPoint;
  gvDebuger.OnDbgLog := OnDbgLog;
end;

procedure TDebugerThread.InitDebugInfo;
begin
  if gvDebugInfo = nil then
    gvDebugInfo := TDelphiDebugInfo.Create;

  gvDebugInfo.DebugInfoProgressCallback := OnProgress;
end;

procedure TDebugerThread.LoadDebugInfo;
begin
  gvDebugInfo.ClearDebugInfo;
  _AC.DoAction(acUpdateInfo, []);

  _AC.DoAction(acProgress, ['Load debug info...', 1]);
  try
    _AC.Log(dltInfo, 'Scan source dirs');
    gvDebugInfo.UpdateSourceDirs(utSystem, DelphiSourceDirs);
    gvDebugInfo.UpdateSourceDirs(utProject, ProjectSourceDirs);

    _AC.Log(dltInfo, 'Load debug info for "%s"', [AppName]);
    FDbgInfoLoaded := gvDebugInfo.ReadDebugInfo(AppName);

    if FDbgInfoLoaded then
    begin
      _AC.Log(dltInfo, 'Loaded %s debug info for "%s"', [gvDebugInfo.DebugInfoType, AppName]);
      _AC.ViewDebugInfo(gvDebugInfo);

      _AC.Log(dltWarning, 'Hint: Set "Stack frames" to "True" in project options, for view full call stack');

      if gvDebugInfo.Units.IndexOf('system.pas') = -1 then
        _AC.Log(dltError, 'Debug info for unit "system.pas" not found. Please, set "Use debug .dcus" to "False" in project options.');
    end
    else
      _AC.Log(dltWarning, 'No debug info for "%s"', [AppName]);

    _AC.DoAction(acUpdateInfo, []);
  finally
    _AC.DoAction(acProgress, ['', 0]);
  end;
end;

procedure TDebugerThread.OnBreakPoint(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord;
      BreakPointIndex: Integer; var ReleaseBreakpoint: Boolean);
begin
  if BreakPointIndex = -1 then
    _AC.Log(dltThreadEvent, 'Perfomance ThreadID: %d', [ThreadId]);
end;

procedure TDebugerThread.OnCreateProcess(Sender: TObject; ProcessId: TProcessId; Data: PCreateProcessDebugInfo);
begin
  _AC.Log(dltProcessEvent, 'Process Start ID: %d', [ProcessId]);

  _AC.DoAction(acStopEnabled, [True]);
  _AC.DoAction(acCreateProcess, [ProcessId]);
end;

procedure TDebugerThread.OnCreateThread(Sender: TObject; ThreadId: TThreadId; Data: PCreateThreadDebugInfo);
begin
  _AC.Log(dltThreadEvent, 'Thread Create ID: %d', [ThreadID]);
  _AC.DoAction(acAddThread, [ThreadID]);
end;

procedure TDebugerThread.OnDbgLog(Sender: TObject; ThreadId: TThreadId; const Data: String);
begin
  _AC.Log(dltDebugOutput, Format('Debug log: [%d] %s', [ThreadId, Data]));
end;

procedure TDebugerThread.OnDebugString(Sender: TObject; ThreadId: TThreadId; Data: POutputDebugStringInfo);
var
  Msg: String;
begin
  if Data^.fUnicode = 1 then
    Msg := String(gvDebuger.ReadStringW(Data^.lpDebugStringData, Data^.nDebugStringLength))
  else
    Msg := String(gvDebuger.ReadStringA(Data^.lpDebugStringData, Data^.nDebugStringLength));

  _AC.Log(dltDebugOutput, 'Debug String: ' + Msg);

  if Msg = '### DBG_MODE_ON ###' then
    gvDebuger.DbgLogMode := True;

  if Msg = '### DBG_MODE_OFF ###' then
    gvDebuger.DbgLogMode := False;
end;

procedure TDebugerThread.OnExitProcess(Sender: TObject; ProcessId: TProcessId; Data: PExitProcessDebugInfo);
begin
  _AC.Log(dltProcessEvent, 'Process Exit ID: %d', [ProcessID]);
end;

procedure TDebugerThread.OnExitThread(Sender: TObject; ThreadId: TThreadId; Data: PExitThreadDebugInfo);
begin
  if Data <> Nil then
    _AC.Log(dltThreadEvent, 'Thread Exit ID: %d (%d)', [ThreadID, Data^.dwExitCode])
  else
    _AC.Log(dltThreadEvent, 'Thread Exit ID: %d', [ThreadID]);
end;

procedure TDebugerThread.OnEndDebug(Sender: TObject);
begin
  _AC.Log(dltInfo, 'Finish debug');

  _AC.DoAction(acStopEnabled, [False]);
  _AC.DoAction(acRunEnabled, [True]);
end;

procedure TDebugerThread.OnLoadDll(Sender: TObject; ThreadId: TThreadId; Data: PLoadDLLDebugInfo);
const
  FormatStrKnownDLL = 'Load Dll at instance $%p handle %d "%s"';
  FormatStrUnknownDLL = 'Load unknown Dll at instance $%p handle %d';
var
  DllName: AnsiString;
  IsUnicodeData: Boolean;
begin
  //FDebuger.ContinueStatus := DBG_EXCEPTION_NOT_HANDLED;
  IsUnicodeData := Data^.fUnicode = 1;
  DllName := gvDebuger.GetDllName(Data^.lpImageName, Data^.lpBaseOfDll, IsUnicodeData);
  if DllName <> '' then
  begin
    if IsUnicodeData then
      _AC.Log(dltDLLEvent, FormatStrKnownDLL, [Data^.lpBaseOfDll, Data^.hFile, PWideChar(@DllName[1])])
    else
      _AC.Log(dltDLLEvent, Format(FormatStrKnownDLL, [Data^.lpBaseOfDll, Data^.hFile, PAnsiChar(@DllName[1])]));
  end
  else
    _AC.Log(dltDLLEvent, Format(FormatStrUnknownDLL, [Data^.lpBaseOfDll, Data^.hFile]));
end;

procedure TDebugerThread.OnProgress(const Action: String; const Progress: Integer);
begin
  _AC.DoAction(acProgress, [Action, Progress]);
end;

procedure TDebugerThread.OnRip(Sender: TObject; ThreadId: TThreadId; Data: PRIPInfo);
begin
  _AC.Log(dltError, 'Debug fail [error: %d; type: %d]', [Data^.dwError, Data^.dwType]);
end;

procedure TDebugerThread.OnUnknownBreakPoint(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord);
begin
  _AC.Log(dltBreakPointEvent, 'OnUnknownBreakPoint ThreadID: %d', [ThreadId]);
end;

procedure TDebugerThread.OnUnknownException(Sender: TObject; ThreadId: TThreadId; ExceptionRecord: PExceptionRecord);
begin
  //_AC.Log(gvDebugInfo.GetExceptionMessage(ExceptionRecord, ThreadId));
end;

procedure TDebugerThread.OnUnLoadDll(Sender: TObject; ThreadId: TThreadId; Data: PUnloadDLLDebugInfo);
const
  FormatStrDLL = 'UnLoad Dll at instance $%p';
begin
  _AC.Log(dltDLLEvent, FormatStrDLL, [Data^.lpBaseOfDll]);
end;

end.
