unit uGA;

interface

uses System.Classes, System.SysUtils, System.Generics.Collections, IdHTTP;

type
  TGAType = (gatPageview, gatAppview, gatEvent, gatTransaction, gatItem, gatSocial, gatException, gatTiming);

  TGAParamType = (
    // General
    gapVersion, gapTrackingID, gapAnonymizeIP, gapQueueTime, gapCacheBuster,
    // Visitor
    gapClientID,
    // Session
    gapSessionControl,
    // Traffic Sources
    gapDocumentReferrer, gapCampaignName, gapCampaignSource, gapCampaignMedium, gapCampaignKeyword, gapCampaignContent,
    gapCampaignID, gapGoogleAdWordsID, gapGoogleDisplayAdsID,
    // System Info
    gapScreenResolution, gapViewportSize, gapDocumentEncoding, gapScreenColors, gapUserLanguage, gapJavaEnabled, gapFlashVersion,
    // Hit
    gapHitType, gapNonInteractionHit,
    // Content Information
    gapDocumentLocationURL, gapDocumentHostName, gapDocumentPath, gapDocumentTitle, gapContentDescription,
    // App Tracking
    gapApplicationName, gapApplicationVersion,
    // Event Tracking
    gapEventCategory, gapEventAction, gapEventLabel, gapEventValue,
    // E-Commerce
    gapTransactionID, gapTransactionAffiliation, gapTransactionRevenue, gapTransactionShipping, gapTransactionTax,
    gapItemName, gapItemPrice, gapItemQuantity, gapItemCode, gapItemCategory, gapCurrencyCode,
    // Social Interactions
    gapSocialNetwork, gapSocialAction, gapSocialActionTarget,
    // Timing
    gapUserTimingCategory, gapUserTimingVariableName, gapUserTimingTime, gapUserTimingLabel, gapPageLoadTime,
    gapDNSTime, gapPageDownloadTime, gapRedirectResponseTime, gapTCPConnectTime, gapServerResponseTime,
    // Exceptions
    gapExceptionDescription, gapIsExceptionFatal,
    // Custom Dimensions / Metrics
    gapCustomDimension1, gapCustomMetric1
  );

  TGAParam = TPair<TGAParamType, string>;
  TGAParams = TDictionary<TGAParamType, string>;

  TGA = class
  private
    FBaseParams: TGAParams;
    FParams: TGAParams;
    FSendFilterList: TStringList;

    function GetTrackingID: string;
    procedure SetTrackingID(const Value: string);
    function GetAppName: string;
    procedure SetAppName(const Value: string);
    function GetAppVersion: string;
    procedure SetAppVersion(const Value: string);
    function GetClientID: string;
    procedure SetClientID(const Value: string);
    function GetUserLanguage: string;
    procedure SetUserLanguage(const Value: string);
    function GetHTTP: TIdHTTP;
    function GetSendFilter: string;
    procedure SetSendFilter(const Value: string);
  protected
    function GetParamStr: string;
    procedure DoSend; virtual;

    function CheckFilter(const Str: String): Boolean; virtual;

    property Params: TGAParams read FParams;
    property BaseParams: TGAParams read FBaseParams;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SendEvent(const Category, Action: string; const ELabel: string = ''; const Value: Integer = 0;
      const ExtParam1: String = '');
    procedure SendException(const Description: string; const IsFatal: Boolean = False);

    procedure SendFeedback(const SocialNetwork, SocialAction, SocialActionTarget: String);

    procedure SessionStart;
    procedure SessionEnd;

    property HTTP: TIdHTTP read GetHTTP;

    property TrackingID: string read GetTrackingID write SetTrackingID;
    property ClientID: string read GetClientID write SetClientID;
    property AppName: string read GetAppName write SetAppName;
    property AppVersion: string read GetAppVersion write SetAppVersion;
    property UserLanguage: string read GetUserLanguage write SetUserLanguage;
    property SendFilter: string read GetSendFilter write SetSendFilter;
  end;

implementation

uses IdGlobal, IdUriUtils, System.SyncObjs;

const
  GA_URL = 'http://www.google-analytics.com/collect';
  GA_VERSION = '1';

  GA_HIT_TYPES: array[Low(TGAType)..High(TGAType)] of string =
    ('pageview', 'appview', 'event', 'transaction', 'item', 'social', 'exception', 'timing');

  GA_PARAMS: array[Low(TGAParamType)..High(TGAParamType)] of string = (
    'v', 'tid', 'aip', 'qt', 'z', 'cid', 'sc', 'dr', 'cn', 'cs', 'cm', 'ck', 'cc', 'ci', 'gclid', 'dclid', 'sr',
    'vp', 'de', 'sd', 'ul', 'je', 'fl', 't', 'ni', 'dl', 'dh', 'dp', 'dt', 'cd', 'an', 'av', 'ec', 'ea', 'el', 'ev',
    'ti', 'ta', 'tr', 'ts', 'tt', 'in', 'ip', 'iq', 'ic', 'iv', 'cu', 'sn', 'sa', 'st', 'utc', 'utv', 'utt', 'utl',
    'plt', 'dns', 'pdt', 'rrt', 'tcp', 'srt', 'exd', 'exf', 'cd1', 'cm1'
  );

  GA_TIMEOUT = 1000;

type
  TGAQueue = TQueue<string>;

  TGASender = class(TThread)
  private
    FQueue: TGAQueue;
    FQueueEvent: TEvent;
    FQueueCS: TCriticalSection;
    FHTTP: TIdHTTP;
    procedure ThSleep(MSec: Integer);
  protected
    function DoSend(const Data: string): Boolean;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Send(const Data: string);

    property HTTP: TIdHTTP read FHTTP;
  end;

var
  gvGASender: TGASender = nil;

function ParamEncode(const ASrc: string): string;
const
  _VALID_CHARS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-_~';
var
  I, J, CharLen, ByteLen: Integer;
  Buf: TIdBytes;
  LChar: WideChar;
  AByteEncoding: IIdTextEncoding;
begin
  Result := '';

  if ASrc = '' then
    Exit;

  AByteEncoding := nil;
  EnsureEncoding(AByteEncoding, encUTF8);

  SetLength(Buf, AByteEncoding.GetMaxByteCount(2));

  I := 1;
  while I <= Length(ASrc) do
  begin
    LChar := ASrc[I];

    if Pos(LChar, _VALID_CHARS) = 0 then
    begin
      CharLen := CalcUTF16CharLength(ASrc, I);

      ByteLen := AByteEncoding.GetBytes(ASrc, I, CharLen, Buf, 0);
      for J := 0 to ByteLen - 1 do
        Result := Result + '%' + IntToHex(Ord(Buf[J]), 2);

      Inc(I, CharLen);
    end
    else
    begin
      Result := Result + Char(LChar);
      Inc(I);
    end;
  end;
end;


{ TGA }

function TGA.CheckFilter(const Str: String): Boolean;
begin
  Result := (FSendFilterList.IndexOf(Str) = -1);
end;

constructor TGA.Create;
begin
  inherited Create;

  FBaseParams := TGAParams.Create;
  FParams := TGAParams.Create;

  FSendFilterList := TStringList.Create;
  FSendFilterList.Delimiter := ',';
  FSendFilterList.StrictDelimiter := True;
  FSendFilterList.CaseSensitive := False;

  FBaseParams.Add(gapVersion, GA_VERSION);
end;

destructor TGA.Destroy;
begin
  FreeAndNil(FBaseParams);
  FreeAndNil(FParams);
  FreeAndNil(FSendFilterList);

  inherited;
end;

procedure TGA.DoSend;
begin
  gvGASender.Send(GetParamStr);
end;

function TGA.GetAppName: string;
begin
  Result := FBaseParams[gapApplicationName];
end;

function TGA.GetAppVersion: string;
begin
  Result := FBaseParams[gapApplicationVersion];
end;

function TGA.GetClientID: string;
begin
  Result := FBaseParams[gapClientID];
end;

function TGA.GetHTTP: TIdHTTP;
begin
  Result := gvGASender.HTTP;
end;

function TGA.GetParamStr: string;
var
  P: TGAParam;
begin
  Result := '';

  for P in FBaseParams Do
  begin
    if Result <> '' then
      Result := Result + '&';

    Result := Result + GA_PARAMS[P.Key] + '=' + ParamEncode(P.Value);
  end;

  for P in FParams Do
  begin
    if Result <> '' then
      Result := Result + '&';

    Result := Result + GA_PARAMS[P.Key] + '=' + ParamEncode(P.Value);
  end;
end;

function TGA.GetSendFilter: string;
begin
  Result := FSendFilterList.DelimitedText;
end;

function TGA.GetTrackingID: string;
begin
  Result := FBaseParams[gapTrackingID];
end;

function TGA.GetUserLanguage: string;
begin
  Result := FBaseParams[gapUserLanguage];
end;

procedure TGA.SendEvent(const Category, Action: string; const ELabel: string = ''; const Value: Integer = 0;
  const ExtParam1: String = '');
begin
  // SYNCWIN-1720
  if not CheckFilter(Category) or not CheckFilter(Action) or not CheckFilter(ELabel) then Exit;

  Params.Clear;
  Params.Add(gapHitType, GA_HIT_TYPES[gatEvent]);

  Params.Add(gapEventCategory, Category);
  Params.Add(gapEventAction, Action);
  Params.Add(gapEventLabel, ELabel);
  Params.Add(gapEventValue, IntToStr(Value));

  if ExtParam1 <> '' then
  begin
    Params.Add(gapCustomDimension1, ExtParam1);
  end;

  DoSend;
end;

procedure TGA.SendException(const Description: string; const IsFatal: Boolean);
begin
  Params.Clear;
  Params.Add(gapHitType, GA_HIT_TYPES[gatException]);

  Params.Add(gapExceptionDescription, Description);
  if IsFatal then
    Params.Add(gapIsExceptionFatal, '1')
  else
    Params.Add(gapIsExceptionFatal, '0');

  DoSend;
end;

procedure TGA.SendFeedback(const SocialNetwork, SocialAction, SocialActionTarget: String);
begin
  Params.Clear;
  Params.Add(gapHitType, GA_HIT_TYPES[gatSocial]);

  Params.Add(gapSocialNetwork, SocialNetwork);
  Params.Add(gapSocialAction, SocialAction);
  Params.Add(gapSocialActionTarget, SocialActionTarget);

  DoSend;
end;

procedure TGA.SessionEnd;
begin
  Params.Clear;
  Params.Add(gapHitType, GA_HIT_TYPES[gatEvent]); //???
  Params.Add(gapSessionControl, 'end');

  DoSend;
end;

procedure TGA.SessionStart;
begin
  Params.Clear;
  Params.Add(gapHitType, GA_HIT_TYPES[gatEvent]); //???
  Params.Add(gapSessionControl, 'start');

  DoSend;
end;

procedure TGA.SetAppName(const Value: string);
begin
  FBaseParams.AddOrSetValue(gapApplicationName, Value);
end;

procedure TGA.SetAppVersion(const Value: string);
begin
  FBaseParams.AddOrSetValue(gapApplicationVersion, Value);
end;

procedure TGA.SetClientID(const Value: string);
begin
  FBaseParams.AddOrSetValue(gapClientID, Value);
end;

procedure TGA.SetSendFilter(const Value: string);
var
  S: String;
begin
  FSendFilterList.Clear;

  if Value <> '' then
  begin
    S := StringReplace(Value, ' ', '', [rfReplaceAll]);
    if S <> '' then
    begin
      if (S[1] = '(') and (S[Length(S)] = ')') then
        S := Copy(S, 2, Length(S) - 2);

      if S <> '' then
        FSendFilterList.DelimitedText := S;
    end;
  end;
end;

procedure TGA.SetTrackingID(const Value: string);
begin
  FBaseParams.AddOrSetValue(gapTrackingID, Value);
end;

procedure TGA.SetUserLanguage(const Value: string);
begin
  FBaseParams.AddOrSetValue(gapUserLanguage, Value);
end;

{ TGASender }

constructor TGASender.Create;
begin
  inherited Create(True);

  FreeOnTerminate := False; // free on finalization

  //FHTTP := TIdHTTP.Create(nil);
  FQueue := TGAQueue.Create;
  FQueueEvent := TEvent.Create;
  FQueueCS := TCriticalSection.Create;

  Suspended := False;
end;

destructor TGASender.Destroy;
begin
  try
    FQueueCS.Enter;
    try
      FQueue.Clear;
      FreeAndNil(FQueue);
    finally
      FQueueCS.Leave;
    end;

    FreeAndNil(FQueueCS);
    FreeAndNil(FQueueEvent);

    //FreeAndNil(FHTTP);
  except
    // Здесь Log уже убитый
  end;

  inherited;
end;

function TGASender.DoSend(const Data: string): Boolean;
var
  S: TStringStream;

begin
  {.$IFDEF DEBUG}
  //Result := True;
  {.$ELSE}
  if (Self = Nil) or Terminated then
  begin
    Result := True;
    Exit;
  end;

  Result := False;

  S := TStringStream.Create(Data);
  try
    try
      FHTTP.Post(GA_URL, S);

      if (Self <> Nil) and not Terminated then
        Result := (FHTTP.ResponseCode = 200);
    except
      // Игнорим все ошибки
    end;
  finally
    FreeAndNil(S);
  end;
  {.$ENDIF}
end;

procedure TGASender.Execute;
var
  Data: string;
begin
  NameThreadForDebugging('TGASender');
  try
    FHTTP := TIdHTTP.Create(nil);
    try
      while (Self <> nil) and not Terminated do
      begin
        if FQueueEvent.WaitFor(100) = wrSignaled then
          while not Terminated and (FQueue.Count > 0) do
          begin
            FQueueCS.Enter;
            try
              Data := FQueue.Dequeue;
              FQueueEvent.ResetEvent;
            finally
              FQueueCS.Leave;
            end;

            while not DoSend(Data) do
            begin
              if (Self = Nil) or Terminated then Exit;
              ThSleep(GA_TIMEOUT * 60);
            end;

            ThSleep(GA_TIMEOUT);
          end;
      end;
    finally
      FreeAndNil(FHTTP);
    end;
  except
    // Игнорим все ошибки
  end;
end;

procedure TGASender.Send(const Data: string);
begin
  FQueueCS.Enter;
  try
    FQueue.Enqueue(Data);
    FQueueEvent.SetEvent;
  finally
    FQueueCS.Leave;
  end;
end;

procedure TGASender.ThSleep(MSec: Integer);
begin
  while (Self <> Nil) and not Terminated and (MSec > 0) do
  begin
    Sleep(50);
    Dec(MSec, 50);
  end;
end;

initialization
  gvGASender := TGASender.Create;

finalization
  try
    gvGASender.Terminate;
    FreeAndNil(gvGASender);
  except end;

end.
