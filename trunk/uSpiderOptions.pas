unit uSpiderOptions;

interface

uses Classes, XMLDoc, XMLIntf, DebugerTypes, DbgHookTypes, Graphics,
  System.UITypes;

type
  TColorOptions = class
  private
    FXMLNode: IXMLNode;
    function GetColor(const Name: String): TColor;
    procedure SetColor(const Name: String; const Value: TColor);
  public
    constructor Create(const OwnerNode: IXMLNode);

    property Colors[const Name: String]: TColor read GetColor write SetColor;
  end;

  TLogColorOptions = class(TColorOptions)
  private
    function GetLogColor(const LogType: TDbgLogType): TColor;
    procedure SetLogColor(const LogType: TDbgLogType; const Value: TColor);
  public
    property LogColors[const LogType: TDbgLogType]: TColor read GetLogColor write SetLogColor; default;
  end;

  TTimelineColorOptions = class(TColorOptions)
  private
    function GetEventColor(const EventType: TDbgPointType): TColor;
    procedure SetEventColor(const EventType: TDbgPointType; const Value: TColor);
  public
    property EventColors[const EventType: TDbgPointType]: TColor read GetEventColor write SetEventColor; default;
  end;

  TSyncObjsColorOptions = class(TColorOptions)
  private
    function GetSyncObjsColor(const SyncObjsType: TDbgSyncObjsType): TColor;
    procedure SetSyncObjsColor(const SyncObjsType: TDbgSyncObjsType; const Value: TColor);
  public
    property SyncObjsColors[const SyncObjsType: TDbgSyncObjsType]: TColor read GetSyncObjsColor write SetSyncObjsColor; default;
  end;

  TSpiderOptions = class
  private
    FXMLFileName: String;
    FXML: IXMLDocument;
    FUpdateCount: Integer;

    FLogColors: TLogColorOptions;
    FTimelineColors: TTimelineColorOptions;
    FSyncObjsColors: TSyncObjsColorOptions;
  protected
    procedure Open;
    procedure CreateNew;
    procedure Save;
  public
    constructor Create(const AXMLFileName: String);
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure AddRecentProject(const ProjectName: String);
    procedure GetRecentProjects(var Projects: TStringList);

    property LogColors: TLogColorOptions read FLogColors;
    property TimelineColors: TTimelineColorOptions read FTimelineColors;
    property SyncObjsColors: TSyncObjsColorOptions read FSyncObjsColors;
  end;

implementation

uses
  Windows, SysUtils, SyncObjs, ClassUtils;

const
  _RECENT = 'recent';
  _RECENT_ITEM = 'item';

  _DefLogColors: array[Low(TDbgLogType) .. High(TDbgLogType)] of TColor = (
    clBlack, // dltInfo
    clPurple, //dltWarning
    clRed, // dltError
    clNavy, // dltDebugOutput
    clBlack, // dltProcessEvent
    clBlack, // dltThreadEvent
    clRed, // dltExceptionEvent
    clMaroon, // dltBreakPointEvent
    clBlack // dltDLLEvent
  );

  _LogColorNames: array[Low(TDbgLogType) .. High(TDbgLogType)] of String = (
    'info', // dltInfo
    'warning', //dltWarning
    'error', // dltError
    'debug_output', // dltDebugOutput
    'process_event', // dltProcessEvent
    'thread_event', // dltThreadEvent
    'exception_event', // dltExceptionEvent
    'breakpoint_event', // dltBreakPointEvent
    'dll_event' // dltDLLEvent
  );

  _DefEventColors: array[Low(TDbgPointType) .. High(TDbgPointType)] of TColor = (
    clWhite, // ptNone
    clSkyBlue, // ptWait
    clGreen, // ptStart
    clGreen, // ptStop
    clRed, // ptException
    clGreen, // ptPerfomance
    clGreen, // ptThreadInfo
    clGreen, // ptMemoryInfo
    clYellow, // ptSyncObjsInfo
    clWhite // ptTraceInfo
  );

  _EventColorNames: array[Low(TDbgPointType) .. High(TDbgPointType)] of String = (
    'none', // ptNone
    'wait', // ptWait
    'start', // ptStart
    'stop', // ptStop
    'exception', // ptException
    'active', // ptPerfomance
    'thread', // ptThreadInfo
    'memory', // ptMemoryInfo
    'syncobjs', // ptSyncObjsInfo
    'trace' // ptTraceInfo
  );

  _DefSyncObjsColors: array[Low(TDbgSyncObjsType) .. High(TDbgSyncObjsType)] of TColor = (
    clDefault,
    clSilver, // soSleep
    clGray, // soWaitForSingleObject
    clGray, // soWaitForMultipleObjects
    clYellow, // soEnterCriticalSection
    clLime, // soLeaveCriticalSection
    clMaroon, // soInCriticalSection
    clYellow // soSendMessage
  );

  _SyncObjsColorNames: array[Low(TDbgSyncObjsType) .. High(TDbgSyncObjsType)] of String = (
    'unknown',
    'sleep', // soSleep
    'waitforsingleobject', // soWaitForSingleObject
    'waitformultipleobjects', // soWaitForMultipleObjects
    'entercriticalsection', // soEnterCriticalSection
    'leavecriticalsection', // soLeaveCriticalSection
    'incriticalsection', // soInCriticalSection
    'sendmessage' // soSendMessage
  );

{ TSpiderOptions }

procedure TSpiderOptions.AddRecentProject(const ProjectName: String);
var
  RecentNode: IXMLNode;
  Node: IXMLNode;
  RP: TStringList;
  Idx: Integer;
begin
  Assert(Assigned(FXML));

  RP := TStringList.Create;
  RP.Duplicates := dupIgnore;
  RP.CaseSensitive := False;

  BeginUpdate;
  try
    RecentNode := FXML.DocumentElement.ChildNodes.FindNode(_RECENT);
    if RecentNode = nil then
      RecentNode := FXML.DocumentElement.AddChild(_RECENT)
    else
      GetRecentProjects(RP);

    if (RP.Count > 0) then
      repeat
        Idx := RP.IndexOf(ProjectName);
        if Idx >= 0 then
          RP.Delete(Idx);
      until Idx = -1;

    RP.Insert(0, ProjectName);

    RecentNode.ChildNodes.Clear;

    for Idx := 0 to RP.Count - 1 do
    begin
      Node := RecentNode.AddChild(_RECENT_ITEM);
      Node.Text := RP[Idx];
    end;
  finally
    EndUpdate;

    FreeAndNil(RP);
  end;
end;

procedure TSpiderOptions.GetRecentProjects(var Projects: TStringList);
var
  RecentNode: IXMLNode;
  Node: IXMLNode;
  I: Integer;
begin
  Assert(Assigned(FXML));

  Projects.Clear;

  RecentNode := FXML.DocumentElement.ChildNodes.FindNode(_RECENT);
  if Assigned(RecentNode) then
  begin
    for I := 0 to RecentNode.ChildNodes.Count - 1 do
    begin
      Node := RecentNode.ChildNodes[I];
      if Node.IsTextElement then
        Projects.Add(Node.Text);
    end;
  end;
end;


procedure TSpiderOptions.BeginUpdate;
begin
  InterlockedIncrement(FUpdateCount);
end;

constructor TSpiderOptions.Create(const AXMLFileName: String);
begin
  inherited Create;

  FXML := nil;
  FUpdateCount := 0;
  FXMLFileName := AXMLFileName;

  Open;
end;

procedure TSpiderOptions.CreateNew;
var
  DocNode: IXMLNode;
begin
  FXML := TXMLDocument.Create(nil);
  FXML.NodeIndentStr := '  ';
  FXML.Options := FXML.Options + [{doNodeAutoIndent, }doNodeAutoCreate];
  FXML.Active := True;
  FXML.Encoding := 'utf-8';

  DocNode := FXML.AddChild('spider_gui');
  DocNode.Attributes['version'] := '1.0';

  Save;
end;

destructor TSpiderOptions.Destroy;
begin
  Save;

  FreeAndNil(FLogColors);
  FreeAndNil(FTimelineColors);
  FreeAndNil(FSyncObjsColors);

  FXML := nil;

  inherited;
end;

procedure TSpiderOptions.EndUpdate;
begin
  if InterlockedDecrement(FUpdateCount) = 0 then
    Save;
end;

procedure TSpiderOptions.Open;
var
  Node1, Node2: IXMLNode;
begin
  BeginUpdate;
  try
    if FileExists(FXMLFileName) then
    begin
      FXML := TXMLDocument.Create(FXMLFileName);
      //FXML.NodeIndentStr := '  ';
      FXML.Options := FXML.Options + [doNodeAutoIndent, doNodeAutoCreate];
    end
    else
      CreateNew;

    Node1 := GetXMLChildNode(FXML.DocumentElement, 'colors');

    Node2 := GetXMLChildNode(Node1, 'log');
    FLogColors := TLogColorOptions.Create(Node2);

    Node2 := GetXMLChildNode(Node1, 'timeline');
    FTimelineColors := TTimelineColorOptions.Create(Node2);

    Node2 := GetXMLChildNode(Node1, 'syncobjs');
    FSyncObjsColors := TSyncObjsColorOptions.Create(Node2);
  finally
    EndUpdate;
  end;
end;

procedure TSpiderOptions.Save;
begin
  if Assigned(FXML) then
  begin
    try
      // Пробуем сохранить рядом с приложением
      FXML.SaveToFile(FXMLFileName);
    except
      // TODO: Сохранять в профиль пользователя
    end;
  end;
end;

{ TColorOptions }

constructor TColorOptions.Create(const OwnerNode: IXMLNode);
begin
  inherited Create;

  FXMLNode := OwnerNode;
end;

function TColorOptions.GetColor(const Name: String): TColor;
var
  Str: String;
begin
  Result := clDefault;

  Str := GetXMLValue(FXMLNode, Name);
  if Str <> '' then
    Result := StringToColor(Str);
end;

procedure TColorOptions.SetColor(const Name: String; const Value: TColor);
begin
  SetXMLValue(FXMLNode, Name, ColorToString(Value));
end;

{ TLogColorOptions }

function TLogColorOptions.GetLogColor(const LogType: TDbgLogType): TColor;
var
  N: String;
begin
  N := _LogColorNames[LogType];
  Result := Colors[N];

  if Result = clDefault then
    Result := _DefLogColors[LogType];
end;

procedure TLogColorOptions.SetLogColor(const LogType: TDbgLogType; const Value: TColor);
var
  N: String;
begin
  N := _LogColorNames[LogType];
  Colors[N] := Value;
end;

{ TTimelineOptions }

function TTimelineColorOptions.GetEventColor(const EventType: TDbgPointType): TColor;
var
  N: String;
begin
  N := _EventColorNames[EventType];
  Result := Colors[N];

  if Result = clDefault then
    Result := _DefEventColors[EventType];
end;

procedure TTimelineColorOptions.SetEventColor(const EventType: TDbgPointType; const Value: TColor);
var
  N: String;
begin
  N := _EventColorNames[EventType];
  Colors[N] := Value;
end;

{ TSyncObjsColorOptions }

function TSyncObjsColorOptions.GetSyncObjsColor(const SyncObjsType: TDbgSyncObjsType): TColor;
var
  N: String;
begin
  N := _SyncObjsColorNames[SyncObjsType];
  Result := Colors[N];

  if Result = clDefault then
    Result := _DefSyncObjsColors[SyncObjsType];
end;

procedure TSyncObjsColorOptions.SetSyncObjsColor(const SyncObjsType: TDbgSyncObjsType; const Value: TColor);
var
  N: String;
begin
  N := _SyncObjsColorNames[SyncObjsType];
  Colors[N] := Value;
end;

end.
