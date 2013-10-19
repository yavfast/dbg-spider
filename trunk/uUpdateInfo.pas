unit uUpdateInfo;

interface
  uses Classes, SysUtils, XMLDoc, XMLIntf, System.Generics.Collections;

type
  TChangeLogItemType = (cliInfo = 0, cliFix, cliAdd, cliUpdate);

  TChangeLogItem = class
    ItemType: TChangeLogItemType;
    ItemText: String;

    class function StrToItemType(const Str: String): TChangeLogItemType; static;
    class function ItemTypeAsStr(const ItemType: TChangeLogItemType): String; static;
  end;

  TChangeLogVersionInfo = class(TObjectList<TChangeLogItem>)
  private
    FVersion: String;
    FDate: TDateTime;
    function GetData: String;
    procedure SetDate(const Value: String);
  public
    property Version: String read FVersion write FVersion;
    property Date: String read GetData write SetDate;
  end;

  TUpdateInfo = class
  private
    FXML: IXMLDocument;
    FCurVer: String;

    function GetLastVersion: String;
  public
    constructor Create;
    destructor Destroy; override;

    function Load: Boolean;
    function GetAllVersions(var SL: TStringList): Boolean;
    function GetVersionInfo(const Version: String; var Info: TChangeLogVersionInfo): Boolean;

    property LastVersion: String read GetLastVersion;
    property CurrentVersion: String read FCurVer;
  end;

var
  gvUpdateInfo: TUpdateInfo = nil;

implementation

uses
  Forms, IdHTTP, ClassUtils, WinAPIUtils;

const
  _UpdateInfoURL = 'http://dbg-spider.net/update_info.txt';

(*
<?xml version="1.0" encoding="utf-8"?>
<spider last_version="1.0.3.0">
  <change_log>
    <release version="1.0.3.0">
      <item type="fix"></item>
      <item type="add"></item>
    </release>
  </change_log>
</spider>
*)


{ TUpdateInfo }

constructor TUpdateInfo.Create;
begin
  inherited Create;

  FXML := nil;
  FCurVer := GetFileVersion(Application.ExeName)
end;

destructor TUpdateInfo.Destroy;
begin
  FXML := nil;

  inherited;
end;

function TUpdateInfo.GetAllVersions(var SL: TStringList): Boolean;
var
  I: Integer;
  ChangeLogNode: IXMLNode;
  ReleaseNode: IXMLNode;
  Ver: String;
begin
  Result := False;

  SL.Clear;

  if Assigned(FXML) then
  begin
    ChangeLogNode := FXML.DocumentElement.ChildNodes.Nodes['change_log'];
    if Assigned(ChangeLogNode) then
    begin
      for I := 0 to ChangeLogNode.ChildNodes.Count - 1 do
      begin
        ReleaseNode := ChangeLogNode.ChildNodes[I];
        Ver := ReleaseNode.Attributes['version'];
        if Ver <> '' then
          SL.Add(Ver);
      end;

      Result := True;
    end;
  end;
end;

function TUpdateInfo.GetLastVersion: String;
begin
  Result := '';

  if Assigned(FXML) then
  begin
    Result := FXML.DocumentElement.Attributes['last_version'];
  end;
end;

function TUpdateInfo.GetVersionInfo(const Version: String; var Info: TChangeLogVersionInfo): Boolean;
var
  I, J: Integer;
  ChangeLogNode: IXMLNode;
  ReleaseNode: IXMLNode;
  ItemNode: IXMLNode;
  Ver: String;
  ChangeLogItem: TChangeLogItem;
begin
  Result := False;

  Info.Clear;
  Info.Version := Version;

  if Assigned(FXML) then
  begin
    ChangeLogNode := FXML.DocumentElement.ChildNodes.Nodes['change_log'];
    if Assigned(ChangeLogNode) then
    begin
      for I := 0 to ChangeLogNode.ChildNodes.Count - 1 do
      begin
        ReleaseNode := ChangeLogNode.ChildNodes[I];
        Ver := ReleaseNode.Attributes['version'];
        if Ver = Version then
        begin
          Info.Date := ReleaseNode.Attributes['date'];

          for J := 0 to ReleaseNode.ChildNodes.Count - 1 do
          begin
            ItemNode := ReleaseNode.ChildNodes[J];

            ChangeLogItem := TChangeLogItem.Create;
            ChangeLogItem.ItemType := TChangeLogItem.StrToItemType(ItemNode.Attributes['type']);
            if ItemNode.IsTextElement then
              ChangeLogItem.ItemText := ItemNode.Text;

            Info.Add(ChangeLogItem);
          end;
        end;
      end;

      Result := True;
    end;
  end;
end;

function TUpdateInfo.Load: Boolean;
var
  IdHttp: TIdHTTP;
  Res: String;
begin
  Result := False;
  FXML := nil;

  IdHttp := TIdHTTP.Create(nil);
  try
    try
      Res := IdHttp.Get(_UpdateInfoURL);

      if Res <> '' then
      begin
        FXML := TXMLDocument.Create(nil);
        FXML.LoadFromXML(Res);
        Result := FXML.Active;
      end;
    except
      FXML := Nil;
    end;
  finally
    FreeAndNil(IdHttp);
  end;
end;

{ TChangeLogItem }

class function TChangeLogItem.ItemTypeAsStr(const ItemType: TChangeLogItemType): String;
begin
  case ItemType of
    cliInfo: Result := 'Info';
    cliFix: Result := 'Fix';
    cliAdd: Result := 'Add';
    cliUpdate: Result := 'Update';
  else
    Result := '';
  end;
end;

class function TChangeLogItem.StrToItemType(const Str: String): TChangeLogItemType;
begin
  for Result := Low(TChangeLogItemType) to High(TChangeLogItemType) do
    if SameText(Str, ItemTypeAsStr(Result)) then
      Exit;

  Result := cliInfo;
end;

{ TChangeLogVersionInfo }

function TChangeLogVersionInfo.GetData: String;
begin
  Result := DateToStr(FDate);
end;

procedure TChangeLogVersionInfo.SetDate(const Value: String);
var
  FS: TFormatSettings;
begin
  FS.ShortDateFormat := 'yyyy-mm-dd';
  FS.DateSeparator := '-';
  FS.LongDateFormat := 'yyyy-mm-dd';

  TryStrToDate(Value, FDate, FS);
end;

initialization
  gvUpdateInfo := TUpdateInfo.Create;

finalization
  FreeAndNil(gvUpdateInfo);

end.
