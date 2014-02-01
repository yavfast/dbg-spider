unit uRWLock;

interface

uses
  System.Classes, System.SyncObjs, System.SysUtils;

type
  TRWNodeState = (ntReader, ntWriter);

  TRWThreadNode = class
  private
    FThreadID: TThreadID;
    FReaderAcquires: Integer;
    FWriterAcquires: Integer;
    FState: TRWNodeState;

    function GetAcquires: Integer; inline;
  public
    constructor Create(const AThreadID: TThreadID);

    procedure IncAcquires; inline;
    procedure DecAcquires; inline;

    property ThreadID: TThreadID read FThreadID;
    property Acquires: Integer read GetAcquires;
    property State: TRWNodeState read FState write FState;
  end;

  ERWLockError = class(Exception);

  TRWLock = class(TObject)
  private
    FLock: TCriticalSection;
    FWaiters: TList;

    function GetRWNode(const Index: Integer): TRWThreadNode; inline;
    function GetRWNodeCount: Integer; inline;

    function GetFirstWriter: Integer;
    function GetWaitIndex(const ThreadID: TThreadID): Integer;

    procedure RaiseRWLockException(const Msg: String);

    function AddRWNode(const AThreadID: TThreadID): TRWThreadNode;
    function GetRWThreadNode(const AThreadID: TThreadID): TRWThreadNode;

    procedure Wait;

    property RWNode[const Index: Integer]: TRWThreadNode read GetRWNode;
    property RWNodeCount: Integer read GetRWNodeCount;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Lock(const AState: TRWNodeState);
    procedure UnLock;
  end;

implementation

{ TRWNode }

constructor TRWThreadNode.Create(const AThreadID: TThreadID);
begin
  inherited Create;

  FThreadID := AThreadID;
  FReaderAcquires := 0;
  FWriterAcquires := 0;
  FState := ntReader;
end;

procedure TRWThreadNode.IncAcquires;
begin
  case FState of
    ntReader:
      Inc(FReaderAcquires);
    ntWriter:
      Inc(FWriterAcquires);
  end;
end;

procedure TRWThreadNode.DecAcquires;
begin
  case FState of
    ntReader:
      Dec(FReaderAcquires);
    ntWriter:
      begin
        Dec(FWriterAcquires);
        if FWriterAcquires = 0 then
          FState := ntReader;
      end;
  end;
end;

function TRWThreadNode.GetAcquires: Integer;
begin
  Result := FReaderAcquires + FWriterAcquires;
end;

{ TRWLock }

constructor TRWLock.Create;
begin
  inherited Create;

  FLock := TCriticalSection.Create;
  FWaiters := TList.Create;
  FWaiters.Capacity := 8;
end;

destructor TRWLock.Destroy;
begin
  {$IFDEF DEBUG}
  if FWaiters.Count > 0 then
    RaiseRWLockException('lock in use');
  {$ENDIF}

  FreeAndNil(FWaiters);
  FreeAndNil(FLock);

  inherited;
end;

function TRWLock.GetFirstWriter: Integer;
begin
  Result := 0;

  while Result < RWNodeCount do
  begin
    if RWNode[Result].State = ntWriter then
      Exit;

    Inc(Result);
  end;

  Result := MaxInt;
end;

function TRWLock.GetWaitIndex(const ThreadID: TThreadID): Integer;
begin
  Result := 0;

  while Result < RWNodeCount do
  begin
    if RWNode[Result].ThreadID = ThreadID then
      Exit;

    Inc(Result);
  end;

  Result := -1;
end;

function TRWLock.GetRWNode(const Index: Integer): TRWThreadNode; // inline
begin
  Result := TRWThreadNode(FWaiters.List[Index]);
end;

function TRWLock.GetRWNodeCount: Integer; // inline
begin
  Result := FWaiters.Count;
end;

function TRWLock.AddRWNode(const AThreadID: TThreadID): TRWThreadNode;
begin
  Result := TRWThreadNode.Create(AThreadID);
  FWaiters.Add(Result);
end;

function TRWLock.GetRWThreadNode(const AThreadID: TThreadID): TRWThreadNode;
var
  Idx: Integer;
begin
  Idx := GetWaitIndex(AThreadID);

  if Idx >= 0 then
    Result := RWNode[Idx]
  else
    Result := AddRWNode(AThreadID);
end;

procedure TRWLock.Lock(const AState: TRWNodeState);
var
  CurThread: TThreadID;
  Node: TRWThreadNode;
begin
  CurThread := TThread.CurrentThread.ThreadID;

  FLock.Enter;

  Node := GetRWThreadNode(CurThread);

  case AState of
    ntReader:
      begin
        while GetWaitIndex(CurThread) > GetFirstWriter do
          Wait;
      end;
    ntWriter:
      begin
        Node.State := ntWriter;

        while GetWaitIndex(CurThread) <> 0 do
          Wait;
      end;
  end;

  Node.IncAcquires;

  FLock.Leave;
end;

procedure TRWLock.Wait;
begin
  FLock.Leave;
  Sleep(1); // Switch thread
  FLock.Enter;
end;

procedure TRWLock.UnLock;
var
  Node: TRWThreadNode;
  CurThread: TThreadID;
  Idx: Integer;
begin
  CurThread := TThread.CurrentThread.ThreadID;

  FLock.Enter;

  Idx := GetWaitIndex(CurThread);

  if Idx >= 0 then
  begin
    Node := RWNode[Idx];

    Node.DecAcquires;

    if Node.Acquires = 0 then
    begin
      FWaiters.Delete(Idx);
      FreeAndNil(Node);
    end;
  end;

  {$IFDEF DEBUG}
  if Idx < 0 then
    RaiseRWLockException('lock already released');
  {$ENDIF}

  FLock.Leave;
end;

procedure TRWLock.RaiseRWLockException(const Msg: String);
begin
  FLock.Leave;

  raise ERWLockError.Create(Msg);
end;

end.
