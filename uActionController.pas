unit uActionController;

interface
  uses Classes, DebugInfo, DebugerTypes, XMLDoc, XMLIntf;

type
  TacAction = (acRunEnabled, acStopEnabled, acCreateProcess, acAddThread, acUpdateInfo, acProgress,
    acSetProjectName);

  TDbgOption = (doDebugInfo, doRun, doProfiler, doMemLeaks);
  TDbgOptions = set of TDbgOption;

  TActionController = class
  public
    class procedure RunDebug(ADbgOptions: TDbgOptions; const AProcessID: TProcessId = 0); static;
    class procedure StopDebug; static;
    class procedure PauseDebug; static;

    class procedure Log(const Msg: String); overload; static;
    class procedure Log(const Msg: String; const Args: array of const); overload; static;

    class procedure DoAction(const Action: TacAction; const Args: array of Variant); static;
    class procedure ViewDebugInfo(DebugInfo: TDebugInfo); static;
  end;

  TProjectOptions = class
  private
    FProjectName: String;
    FProjectXML: IXMLDocument;
    FUpdateCount: Integer;

    function GetXMLValue(const ParentNode: IXMLNode; const NodeName: String): String; overload;
    procedure SetXMLValue(const ParentNode: IXMLNode; const NodeName, NodeValue: String); overload;

    function GetXMLValue(const NodeName: String): String; overload;
    procedure SetXMLValue(const NodeName, NodeValue: String); overload;

    procedure SetApplicationName(const Value: String);
    procedure SetProjectStorage(const Value: String);
    function GetApplicationName: String;
    function GetProjectName: String;
    function GetProjectStorage: String;
    function GetDelphiSource: String;
    function GetProjectSource: String;
    procedure SetDelphiSource(const Value: String);
    procedure SetProjectSource(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Open(const AProjectName: String);
    procedure NewConfig;
    procedure Save;

    property ProjectName: String read GetProjectName;
    property ApplicationName: String read GetApplicationName write SetApplicationName;
    property ProjectStorage: String read GetProjectStorage write SetProjectStorage;
    property ProjectSource: String read GetProjectSource write SetProjectSource;
    property DelphiSource: String read GetDelphiSource write SetDelphiSource;
  end;

const
  _DEFAULT_PROJECT = '_default.spider';

var
  _AC: TActionController = nil;
  gvProjectOptions: TProjectOptions = nil;

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

class procedure TActionController.RunDebug(ADbgOptions: TDbgOptions; const AProcessID: TProcessId = 0);
begin
  if not Assigned(_DbgThread) then
    _DbgThread := TDebugerThread.Create(ADbgOptions, AProcessID);
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
    FProjectXML.SaveToFile(FProjectName);
end;

function TProjectOptions.GetApplicationName: String;
begin
  Result := GetXMLValue('application_name');
end;

procedure TProjectOptions.SetApplicationName(const Value: String);
begin
  SetXMLValue('application_name', Value);
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

procedure TProjectOptions.SetProjectStorage(const Value: String);
begin
  SetXMLValue('project_storage', Value);
end;

function TProjectOptions.GetXMLValue(const ParentNode: IXMLNode; const NodeName: String): String;
var
  ResNode: IXMLNode;
begin
  Result := '';
  if Assigned(ParentNode) then
  begin
    ResNode := ParentNode.ChildNodes.FindNode(NodeName);
    if Assigned(ResNode) and ResNode.IsTextElement then
      Result := ResNode.Text;
  end;
end;

function TProjectOptions.GetXMLValue(const NodeName: String): String;
begin
  Result := '';
  if Assigned(FProjectXML) then
    Result := GetXMLValue(FProjectXML.DocumentElement, NodeName);
end;

procedure TProjectOptions.SetXMLValue(const ParentNode: IXMLNode; const NodeName, NodeValue: String);
begin
  if Assigned(ParentNode) then
    ParentNode.ChildValues[NodeName] := NodeValue;
end;

procedure TProjectOptions.SetXMLValue(const NodeName, NodeValue: String);
begin
  if Assigned(FProjectXML) then
  begin
    BeginUpdate;
    SetXMLValue(FProjectXML.DocumentElement, NodeName, NodeValue);
    EndUpdate;
  end;
end;

initialization
  gvProjectOptions := TProjectOptions.Create;

finalization
  FreeAndNil(gvProjectOptions);

end.
