unit uSpiderOptions;

interface

uses Classes, XMLDoc, XMLIntf;

type
  TSpiderOptions = class
  private
    FXMLFileName: String;
    FXML: IXMLDocument;
    FUpdateCount: Integer;
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
  end;

implementation

uses
  Windows, SysUtils, SyncObjs;

const
  _RECENT = 'recent';
  _RECENT_ITEM = 'item';

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
  FXML.Options := FXML.Options + [doNodeAutoIndent, doNodeAutoCreate];
  FXML.Active := True;
  FXML.Encoding := 'utf-8';

  DocNode := FXML.AddChild('spider_gui');
  DocNode.Attributes['version'] := '1.0';

  Save;
end;

destructor TSpiderOptions.Destroy;
begin
  Save;
  FXML := nil;

  inherited;
end;

procedure TSpiderOptions.EndUpdate;
begin
  if InterlockedDecrement(FUpdateCount) = 0 then
    Save;
end;

procedure TSpiderOptions.Open;
begin
  if FileExists(FXMLFileName) then
  begin
    FXML := TXMLDocument.Create(FXMLFileName);
    //FXML.NodeIndentStr := '  ';
    FXML.Options := FXML.Options + [doNodeAutoIndent, doNodeAutoCreate];
  end
  else
    CreateNew;
end;

procedure TSpiderOptions.Save;
begin
  if Assigned(FXML) then
    FXML.SaveToFile(FXMLFileName);
end;

end.
