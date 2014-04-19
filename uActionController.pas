unit uActionController;

interface

uses Classes, DebugInfo, DebugerTypes, XMLDoc, XMLIntf, System.Generics.Collections,
  System.SyncObjs;

type
  TacAction = (acCreateProcess, acAddThread, acUpdateInfo, acProgress, acSetProjectName, acChangeDbgState);

  TDbgOption = (
    doDebugInfo,
    doRun,
    doProfiler,
    doMemProfiler, doMemCallStack, doMemCheckDoubleFree,
    doExceptions, doExceptionCallStack,
    doCodeTracking, doTrackSystemUnits, doSamplingMethod,
    doSyncObjsTracking
  );

  TDbgOptions = set of TDbgOption;

  TArgsList = array of Variant;

  TActionItem = class
  private
    FAction: TacAction;
    FArgs: TArgsList;
  public
    constructor Create(const AAction: TacAction; const AArgs: TArgsList);

    property Action: TacAction read FAction;
    property Args: TArgsList read FArgs;
  end;

  TActionQueue = class(TQueue<TActionItem>)
  private
    FLock: TCriticalSection;
    procedure Lock;
    procedure UnLock;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function GetAction: TActionItem;
    procedure AddAction(const Action: TacAction; const Args: TArgsList);
  end;

  TActionThread = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TActionController = class
  public
    class procedure RunDebug(const ADbgOptions: TDbgOptions; const AProcessID: TProcessId = 0); static;
    class procedure StopDebug; static;
    class procedure PauseDebug; static;
    class procedure TraceDebug(const TraceType: TDbgTraceState); static;

    class procedure Log(const LogType: TDbgLogType; const Msg: String); overload; static;
    class procedure Log(const LogType: TDbgLogType; const Msg: String; const Args: array of const); overload; static;

    class procedure DoAction(const Action: TacAction; const Args: array of Variant); overload; static;
    class procedure DoAction(const Action: TacAction; const Args: TArgsList); overload; static;
    class procedure DoSyncAction(const Action: TacAction; const Args: array of Variant); overload; static;
    class procedure DoSyncAction(const Action: TacAction; const Args: TArgsList); overload; static;
    class procedure ViewDebugInfo(DebugInfo: TDebugInfo); static;

    class procedure ClearDebug(const DbgFree: Boolean); static;

    class procedure AppClose; static;
  end;

  TProjectOptions = class
  private
    FProjectName: String;
    FProjectXML: IXMLDocument;
    FUpdateCount: Integer;

    function GetXMLValue(const NodeName: String): String;
    procedure SetXMLValue(const NodeName, NodeValue: String);

    function GetApplicationName: String;
    function GetProjectName: String;
    function GetProjectStorage: String;
    function GetDelphiSource: String;
    function GetProjectSource: String;
    function GetRunParams: String;
    function GetWorkingDirectory: String;

    procedure SetApplicationName(const Value: String);
    procedure SetProjectStorage(const Value: String);
    procedure SetDelphiSource(const Value: String);
    procedure SetProjectSource(const Value: String);
    procedure SetRunParams(const Value: String);
    procedure SetWorkingDirectory(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Open(const AProjectName: String);
    procedure NewConfig;
    procedure Save;

    class function GetDefProjectSource(const ProjectName: String): String; static;
    class function GetDefDelphiSource: String; static;

    property ProjectName: String read GetProjectName;
    property ApplicationName: String read GetApplicationName write SetApplicationName;
    property ProjectStorage: String read GetProjectStorage write SetProjectStorage;
    property ProjectSource: String read GetProjectSource write SetProjectSource;
    property DelphiSource: String read GetDelphiSource write SetDelphiSource;
    property RunParams: String read GetRunParams write SetRunParams;
    property WorkingDirectory: String read GetWorkingDirectory write SetWorkingDirectory;
  end;

const
  _DEFAULT_PROJECT = '_default.spider';

var
  _AC: TActionController = nil;
  gvProjectOptions: TProjectOptions = nil;

implementation

uses SysUtils, uMain, Debuger, uDebugerThread, ClassUtils, JclIDEUtils,
  System.IOUtils, System.Types, Vcl.Dialogs;

var
  gvActionThread: TActionThread = nil;
  gvActionQueue: TActionQueue = nil;

{ TActionController }

class procedure TActionController.Log(const LogType: TDbgLogType; const Msg: String);
begin
  if Assigned(gvDebugInfo) then
    gvDebugInfo.DbgLog.Add(LogType, Msg);

  DoAction(acUpdateInfo, []);
end;

class procedure TActionController.AppClose;
begin
  gvActionThread.Terminate;

  Sleep(500);
end;

class procedure TActionController.ClearDebug(const DbgFree: Boolean);
begin
  gvActionQueue.Clear;

  if Assigned(gvDebugInfo) then
  begin
    gvDebugInfo.ClearDebugInfo;
    if DbgFree then
      FreeAndNil(gvDebugInfo);
  end;

  if Assigned(gvDebuger) then
  begin
    gvDebuger.ClearDbgInfo;
    if DbgFree then
      FreeAndNil(gvDebuger);
  end;
end;

class procedure TActionController.DoAction(const Action: TacAction; const Args: TArgsList);
begin
  gvActionQueue.AddAction(Action, Args);
end;

class procedure TActionController.DoSyncAction(const Action: TacAction; const Args: array of Variant);
var
  _Args: TArgsList;
  i: Integer;
begin
  SetLength(_Args, Length(Args));
  for i := 0 to High(Args) do
    _Args[i] := Args[i];

  TActionController.DoSyncAction(Action, _Args);
end;

class procedure TActionController.DoAction(const Action: TacAction; const Args: array of Variant);
var
  _Args: TArgsList;
  i: Integer;
begin
  SetLength(_Args, Length(Args));
  for i := 0 to High(Args) do
    _Args[i] := Args[i];

  TActionController.DoAction(Action, _Args);
end;

class procedure TActionController.DoSyncAction(const Action: TacAction; const Args: TArgsList);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      if Assigned(MainForm) then
        MainForm.DoAction(Action, Args);
    end
  );
end;

class procedure TActionController.Log(const LogType: TDbgLogType; const Msg: String; const Args: array of const);
begin
  Log(LogType, Format(Msg, Args));
end;

class procedure TActionController.PauseDebug;
begin
  TActionController.TraceDebug(dtsPause);
end;

class procedure TActionController.RunDebug(const ADbgOptions: TDbgOptions; const AProcessID: TProcessId = 0);
begin
  if not Assigned(_DbgThread) then
    _DbgThread := TDebugerThread.Create(ADbgOptions, AProcessID);
end;

class procedure TActionController.StopDebug;
begin
  if Assigned(gvDebuger) then
    gvDebuger.StopDebug;
end;

class procedure TActionController.TraceDebug(const TraceType: TDbgTraceState);
begin
  if Assigned(gvDebuger) then
  begin
    gvDebuger.TraceDebug(TraceType);
    TActionController.DoAction(acChangeDbgState, []);
  end;
end;

class procedure TActionController.ViewDebugInfo(DebugInfo: TDebugInfo);
var
  DI: TDebugInfo;
begin
  DI := DebugInfo;
  TThread.Synchronize(nil,
    procedure
    begin
      MainForm.ViewDebugInfo(DI);
    end
  );
end;


{ TProjectOptions }

procedure TProjectOptions.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TProjectOptions.Clear;
begin
  FProjectName := '';
  FProjectXML := nil;
  FUpdateCount := 0;
end;

constructor TProjectOptions.Create;
begin
  inherited Create;

  FProjectXML := nil;
  FUpdateCount := 0;
end;

destructor TProjectOptions.Destroy;
begin

  inherited;
end;

procedure TProjectOptions.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Save;
end;

function TProjectOptions.GetProjectName: String;
begin
  Result := FProjectName;
end;

procedure TProjectOptions.NewConfig;
var
  DocNode: IXMLNode;
begin
  FProjectXML := TXMLDocument.Create(nil);
  FProjectXML.NodeIndentStr := '  ';
  FProjectXML.Options := FProjectXML.Options + [doNodeAutoIndent, doNodeAutoCreate];
  FProjectXML.Active := True;
  FProjectXML.Encoding := 'utf-8';

  DocNode := FProjectXML.AddChild('spider');
  DocNode.Attributes['version'] := '1.0';
end;

procedure TProjectOptions.Open(const AProjectName: String);
begin
  if AProjectName <> FProjectName then
    Clear;

  FProjectName := AProjectName;
  if FileExists(FProjectName) then
    FProjectXML := TXMLDocument.Create(FProjectName)
  else
    NewConfig;
end;

procedure TProjectOptions.Save;
begin
  if Assigned(FProjectXML) and (ExtractFileName(FProjectName) <> _DEFAULT_PROJECT) then
  try
    FProjectXML.SaveToFile(FProjectName);
  except
    on E: Exception do
      ShowMessageFmt('Save to "%s" fail: %s', [FProjectName, E.Message]);
  end;
end;

function TProjectOptions.GetApplicationName: String;
begin
  Result := GetXMLValue('application_name');
end;

procedure TProjectOptions.SetApplicationName(const Value: String);
begin
  SetXMLValue('application_name', Value);
end;

class function TProjectOptions.GetDefDelphiSource: String;
var
  Installations: TJclBorRADToolInstallations;
  DelphiRoot: String;
begin
  Result := '';

  Installations := TJclBorRADToolInstallations.Create;
  try
    if Installations.Count > 0 then
    begin
      DelphiRoot := Installations.Installations[Installations.Count - 1].RootDir;
      Result := IncludeTrailingPathDelimiter(DelphiRoot) + 'source';
    end;
  finally
    FreeAndNil(Installations);
  end;
end;

class function TProjectOptions.GetDefProjectSource(const ProjectName: String): String;
var
  DprName: String;
  DprPathName: String;
  Find: TStringDynArray;
begin
  Result := '';

  if ProjectName <> '' then
  begin
    DprName := ExtractFileName(ProjectName);
    DprName := ChangeFileExt(DprName, '.dpr');

    Result := ExtractFileDir(ProjectName);

    while (Result <> '') and TDirectory.Exists(Result) do
    begin
      DprPathName := Result + PathDelim + DprName;
      if TFile.Exists(DprPathName) then
        Break;

      Find := TDirectory.GetFiles(Result, '*.dpr');
      if Length(Find) > 0 then
        Break;

      Result := ExtractFileDir(Result);

      // Если в рутовом каталоге, то выходим
      if Result = IncludeTrailingPathDelimiter(ExtractFileDrive(Result)) then
      begin
        Result := '';
        Break;
      end;
    end;

    if Result <> '' then
    begin
      // TODO: Source dirs from DPR
    end;
  end;
end;

function TProjectOptions.GetDelphiSource: String;
begin
  Result := GetXMLValue('delphi_source');
end;

procedure TProjectOptions.SetDelphiSource(const Value: String);
begin
  SetXMLValue('delphi_source', Value);
end;

function TProjectOptions.GetProjectSource: String;
begin
  Result := GetXMLValue('project_source');
end;

procedure TProjectOptions.SetProjectSource(const Value: String);
begin
  SetXMLValue('project_source', Value);
end;

function TProjectOptions.GetProjectStorage: String;
begin
  Result := GetXMLValue('project_storage');
end;

function TProjectOptions.GetRunParams: String;
begin
  Result := GetXMLValue('run_parameters');
end;

function TProjectOptions.GetWorkingDirectory: String;
begin
  Result := GetXMLValue('working_directory');
end;

procedure TProjectOptions.SetProjectStorage(const Value: String);
begin
  SetXMLValue('project_storage', Value);
end;

procedure TProjectOptions.SetRunParams(const Value: String);
begin
  SetXMLValue('run_parameters', Value);
end;

procedure TProjectOptions.SetWorkingDirectory(const Value: String);
begin
  SetXMLValue('working_directory', Value);
end;

function TProjectOptions.GetXMLValue(const NodeName: String): String;
begin
  Result := '';
  if Assigned(FProjectXML) then
    Result := ClassUtils.GetXMLValue(FProjectXML.DocumentElement, NodeName);
end;

procedure TProjectOptions.SetXMLValue(const NodeName, NodeValue: String);
begin
  if Assigned(FProjectXML) then
  begin
    BeginUpdate;
    ClassUtils.SetXMLValue(FProjectXML.DocumentElement, NodeName, NodeValue);
    EndUpdate;
  end;
end;

{ TActionQueue }

procedure TActionQueue.AddAction(const Action: TacAction; const Args: TArgsList);
var
  ActionItem: TActionItem;
begin
  Lock;
  try
    ActionItem := TActionItem.Create(Action, Args);
    Enqueue(ActionItem);
  finally
    UnLock;
  end;
end;

procedure TActionQueue.Clear;
var
  Action: TActionItem;
begin
  Lock;
  try
    while Count > 0 do
    begin
      Action := GetAction;
      FreeAndNil(Action);
    end;
  finally
    UnLock;
  end;
end;

constructor TActionQueue.Create;
begin
  inherited;

  FLock := TCriticalSection.Create;
end;

destructor TActionQueue.Destroy;
begin
  FreeAndNil(FLock);

  inherited;
end;

function TActionQueue.GetAction: TActionItem;
begin
  Lock;
  try
    Result := Dequeue;
  finally
    UnLock;
  end;
end;

procedure TActionQueue.Lock;
begin
  FLock.Enter;
end;

procedure TActionQueue.UnLock;
begin
  FLock.Leave;
end;

{ TActionThread }

constructor TActionThread.Create;
begin
  inherited Create(True);

  FreeOnTerminate := True;

  Suspended := False;
end;

destructor TActionThread.Destroy;
begin
  FreeAndNil(gvActionQueue);
  gvActionThread := nil;

  inherited;
end;

procedure TActionThread.Execute;
var
  ActionItem: TActionItem;
begin
  NameThreadForDebugging(ClassName);

  while not Terminated do
  begin
    while not Terminated and (gvActionQueue.Count > 0) do
    begin
      ActionItem := gvActionQueue.GetAction;
      if Assigned(ActionItem) then
        try
          _AC.DoSyncAction(ActionItem.Action, ActionItem.Args);
        finally
          FreeAndNil(ActionItem);
        end;
    end;

    if not Terminated then
      Sleep(500);
  end;
end;

{ TActionItem }

constructor TActionItem.Create(const AAction: TacAction; const AArgs: TArgsList);
begin
  inherited Create;

  FAction := AAction;
  FArgs := Copy(AArgs, 0, Length(AArgs));
end;

initialization
  gvProjectOptions := TProjectOptions.Create;
  gvActionQueue := TActionQueue.Create;
  gvActionThread := TActionThread.Create;

finalization
  // Destroys in gvActionThread
  //FreeAndNil(gvActionThread);
  //FreeAndNil(gvActionQueue);

  FreeAndNil(gvProjectOptions);

end.
