unit uActionController;

interface
  uses Classes, DebugInfo, DebugerTypes;

type
  TacAction = (acRunEnabled, acStopEnabled, acCreateProcess, acAddThread, acUpdateInfo, acProgress);

  TDbgOption = (doDebugInfo, doRun, doProfiler, doMemLeaks);
  TDbgOptions = set of TDbgOption;

  TActionController = class
  public
    class procedure RunDebug(const AppName: String; ADbgOptions: TDbgOptions; AProcessID: TProcessId = 0); static;
    class procedure StopDebug; static;
    class procedure PauseDebug; static;

    class procedure Log(const Msg: String); overload; static;
    class procedure Log(const Msg: String; const Args: array of const); overload; static;

    class procedure DoAction(const Action: TacAction; const Args: array of Variant); static;
    class procedure ViewDebugInfo(DebugInfo: TDebugInfo); static;
  end;

var
  _AC: TActionController = nil;

implementation

uses SysUtils, uMain, Debuger, uDebugerThread;

{ TActionController }

class procedure TActionController.Log(const Msg: String);
//var
//  _Msg: String;
begin
  if Assigned(gvDebugInfo) then
    gvDebugInfo.DbgLog.Add(Msg);

  DoAction(acUpdateInfo, []);

  (*
  _Msg := Msg;
  if Assigned(MainForm) then
    TThread.Synchronize(nil,
      procedure
      begin
        if Assigned(MainForm) and MainForm.Visible then
          MainForm.Log(_Msg);
      end
    );
  *)
end;

class procedure TActionController.DoAction(const Action: TacAction; const Args: array of Variant);
var
  _Action: TacAction;
  _Args: array of Variant;
  i: Integer;
begin
  _Action := Action;

  SetLength(_Args, Length(Args));
  for i := 0 to High(Args) do
    _Args[i] := Args[i];

  if Assigned(MainForm) then
  begin
    TThread.Synchronize(nil,
      procedure
      begin
        MainForm.DoAction(_Action, _Args);
      end
    );
  end;
end;

class procedure TActionController.Log(const Msg: String; const Args: array of const);
begin
  Log(Format(Msg, Args));
end;

class procedure TActionController.PauseDebug;
begin
  //
end;

class procedure TActionController.RunDebug(const AppName: String; ADbgOptions: TDbgOptions; AProcessID: TProcessId = 0);
begin
  if not Assigned(_DbgThread) then
    _DbgThread := TDebugerThread.Create(AppName, ADbgOptions, AProcessID);
end;

class procedure TActionController.StopDebug;
begin
  if Assigned(gvDebuger) then
    gvDebuger.StopDebug;
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


end.
