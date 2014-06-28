unit DbgSyncObjsProfiler;

interface

uses System.Classes, WinApi.Windows, Collections.Queues, DbgHookTypes,
  System.SysUtils, System.SyncObjs, DebugerTypes;

type
  TSyncObjsInfoQueue = TQueue<PDbgSyncObjsInfoListBuf>;

  TDbgSyncObjsProfiler = class
  private
    FSyncObjsInfoQueue: TSyncObjsInfoQueue;

    FSyncObjsTracking: LongBool;
    procedure SetSyncObjsTracking(const Value: LongBool);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function ProcessSyncObjsInfoQueue: LongBool;
    procedure LoadSyncObjsInfoPackEx(const SyncObjsInfoPack: Pointer; const Count: Cardinal);
    procedure ProcessSyncObjsInfoBuf(const Buf: PDbgSyncObjsInfoListBuf);

    property SyncObjsInfoQueue: TSyncObjsInfoQueue read FSyncObjsInfoQueue;
    property SyncObjsTracking: LongBool read FSyncObjsTracking write SetSyncObjsTracking;
  end;

implementation

uses Debuger, CollectList;

const
  _MAX_SYNC_OBJS_INFO_BUF_COUNT = 512;

{ TDbgSyncObjsProfiler }

procedure TDbgSyncObjsProfiler.Clear;
begin
  FSyncObjsInfoQueue.Clear;
end;

constructor TDbgSyncObjsProfiler.Create;
begin
  inherited;

  FSyncObjsInfoQueue := TSyncObjsInfoQueue.Create(True);
  FSyncObjsInfoQueue.Capacity := _MAX_SYNC_OBJS_INFO_BUF_COUNT + 1;
end;

destructor TDbgSyncObjsProfiler.Destroy;
begin

  inherited;
end;

procedure TDbgSyncObjsProfiler.LoadSyncObjsInfoPackEx(const SyncObjsInfoPack: Pointer; const Count: Cardinal);
var
  Buf: PDbgSyncObjsInfoListBuf;
begin
  if not SyncObjsTracking then
    Exit;

  while FSyncObjsInfoQueue.Count >= _MAX_SYNC_OBJS_INFO_BUF_COUNT do
    SwitchToThread;

  Buf := AllocMem(SizeOf(TDbgSyncObjsInfoListBuf));
  Buf^.Count := Count;
  Buf^.DbgSyncObjsInfoList := AllocMem(Count * SizeOf(TDbgSyncObjsInfo));
  Buf^.DbgPointIdx := gvDebuger.ProcessData.CurDbgPointIdx;

  if gvDebuger.ReadData(SyncObjsInfoPack, Buf^.DbgSyncObjsInfoList, Count * SizeOf(TDbgSyncObjsInfo)) then
    FSyncObjsInfoQueue.Enqueue(Buf)
  else
    RaiseDebugCoreException();
end;

procedure TDbgSyncObjsProfiler.ProcessSyncObjsInfoBuf(const Buf: PDbgSyncObjsInfoListBuf);
var
  ThData: PThreadData;

  function FindCSLink(const CSData: PRTLCriticalSection): PSyncObjsInfo;
  var
    Idx: Integer;
  begin
    for Idx := ThData^.DbgSyncObjsInfo.Count - 1 downto 0 do
    begin
      Result := ThData^.DbgSyncObjsInfo[Idx];
      if (Result^.SyncObjsInfo.SyncObjsType = soInCriticalSection) and
        (Result^.Link = nil) and
        (Result^.SyncObjsInfo.CS = CSData) and
        (Result^.SyncObjsInfo.SyncObjsStateType = sosEnter)
      then
        Exit;
    end;

    Result := nil;
  end;

var
  Idx: Integer;
  SyncObjsInfo: PDbgSyncObjsInfo;
  ThSyncObjsInfo: PSyncObjsInfo;
  SyncObjsLink: PSyncObjsInfo;
  SyncObjsLinkExt: PSyncObjsInfo;
begin
  ThData := Nil;

  for Idx := 0 to Buf^.Count - 1 do
  begin
    SyncObjsInfo := @Buf^.DbgSyncObjsInfoList^[Idx];
    if (ThData = Nil) or (ThData^.ThreadID <> SyncObjsInfo^.ThreadId) then
      ThData := gvDebuger.GetThreadData(SyncObjsInfo^.ThreadId, True);

    if ThData = Nil then
      Continue; // TODO: � �����-�� ������� ���� �������
      //RaiseDebugCoreException();

    case SyncObjsInfo^.SyncObjsType of
      soSleep, soWaitForSingleObject, soWaitForMultipleObjects, soEnterCriticalSection, soInCriticalSection, soSendMessage:
        begin
          ThData^.DbgSyncObjsInfo.BeginRead;
          try
            SyncObjsLink := nil;
            SyncObjsLinkExt := nil;

            if SyncObjsInfo^.SyncObjsStateType = sosLeave then
            begin
              // ����� sosEnter ������
              if SyncObjsInfo^.SyncObjsType = soInCriticalSection then
              begin
                // ��� ��� Id ������� ������ �� ��������� � Id �����, �� ���� �� ��������� CS
                // ���������� ����� ��������� ������� �� CS � SyncObjsStateType = sosEnter

                SyncObjsLink := FindCSLink(SyncObjsInfo^.CS);
              end
              else
              begin
                // � ��������� ����� Id ������� ����� � ������ ����� ���������

                if ThData^.DbgSyncObjsInfoByID.TryGetValue(SyncObjsInfo^.Id, SyncObjsLink) then
                begin
                  // ������� ������������ Id �� �������, ����� EnterCriticalSection,
                  // ������� ��� ����� ��� soInCriticalSection

                  if SyncObjsInfo^.SyncObjsType <> soEnterCriticalSection then
                    ThData^.DbgSyncObjsInfoByID.Remove(SyncObjsInfo^.Id);
                end;
              end;
            end
            else // sosEnter
            begin
              if SyncObjsInfo^.SyncObjsType = soInCriticalSection then
              begin
                // ���� ���� �� soEnterCriticalSection
                if ThData^.DbgSyncObjsInfoByID.TryGetValue(SyncObjsInfo^.Id, SyncObjsLinkExt) then
                  ThData^.DbgSyncObjsInfoByID.Remove(SyncObjsInfo^.Id);
              end;
            end;

            // ��������� ���� ��� ����� �������
            ThSyncObjsInfo := ThData^.DbgSyncObjsInfo.Add;

            if ThData^.State = tsFinished then
              ThSyncObjsInfo^.PerfIdx := PThreadPoint(ThData^.DbgPoints[ThData^.DbgPoints.Count - 1])^.PerfIdx
            else
              ThSyncObjsInfo^.PerfIdx := Buf^.DbgPointIdx;

            // ���� �� ����
            ThSyncObjsInfo^.Link := SyncObjsLink;
            if SyncObjsLink <> nil then
              SyncObjsLink^.Link := ThSyncObjsInfo;

            // ������� ����
            ThSyncObjsInfo^.LinkExt := SyncObjsLinkExt;
            if SyncObjsLinkExt <> nil then
              SyncObjsLinkExt^.LinkExt := ThSyncObjsInfo;

            // �������� ���� �� ������, ��� ��� �� ����� ����� ���������
            ThSyncObjsInfo^.SyncObjsInfo.Init(SyncObjsInfo);

            ThData^.DbgSyncObjsInfo.Commit;

            // ��������� ���� ��� sosEnter ������
            if SyncObjsInfo^.SyncObjsStateType = sosEnter then
            begin
              if SyncObjsInfo^.SyncObjsType <> soInCriticalSection then
                ThData^.DbgSyncObjsInfoByID.AddOrSetValue(SyncObjsInfo^.Id, ThSyncObjsInfo);
            end;

            // ��������� ���� ������
            case SyncObjsInfo^.SyncObjsType of
              soEnterCriticalSection, soInCriticalSection,
              soSendMessage,
              soWaitForSingleObject, soWaitForMultipleObjects:
                begin
                  ThData^.DbgSyncObjsUnitList.LoadStack(ThSyncObjsInfo);
                end;
            end;
          finally
            ThData^.DbgSyncObjsInfo.EndRead;
          end;
        end;
      soLeaveCriticalSection:
        begin
          // TODO:
        end;
    end;
  end;
end;

function TDbgSyncObjsProfiler.ProcessSyncObjsInfoQueue: LongBool;
var
  Buf: PDbgSyncObjsInfoListBuf;
begin
  Result := False;

  if not SyncObjsTracking then
    Exit;

  if FSyncObjsInfoQueue.Count > 0 then
  begin
    try
      // ���������� �������� ������� ��� ���������� ��������� �������� ����������� ������
      if FSyncObjsInfoQueue.Count < _MAX_SYNC_OBJS_INFO_BUF_COUNT then
      begin
        Buf := FSyncObjsInfoQueue.First;
        if (gvDebuger.ProcessData.CurDbgPointIdx - Buf^.DbgPointIdx) <= 2 then
          Exit;
      end;

      Buf := FSyncObjsInfoQueue.Dequeue;
      try
        ProcessSyncObjsInfoBuf(Buf);
      finally
        FreeMemory(Buf^.DbgSyncObjsInfoList);
        FreeMemory(Buf);
      end;

      Result := True;
    except
      on E: Exception do ; // TODO:
    end;
  end;
end;

procedure TDbgSyncObjsProfiler.SetSyncObjsTracking(const Value: LongBool);
begin
  FSyncObjsTracking := Value;
end;

end.
