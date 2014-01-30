unit uRWLock;

interface

uses
  System.Classes, System.SyncObjs, System.SysUtils, System.Contnrs;

type
  TRWNodeState = (ntReader, ntWriter);

  TRWNode = class
  private
    FThreadID: TThreadID;
    FState: TRWNodeState;
    FAcquires: Integer;
  public
    constructor Create(const AThreadID: TThreadID; const AState: TRWNodeState);

    procedure IncAcquires; inline;
    procedure DecAcquires; inline;

    property ThreadID: TThreadID read FThreadID;
    property State: TRWNodeState read FState write FState;
    property Acquires: Integer read FAcquires;
  end;

  ERWLockError = class(Exception);

  TRWLock = class(TObject)
  private
    FLock: TCriticalSection;
    FWaiters: TObjectList;

    function GetFirstWriter: Integer;
    function GetIndex(const ThreadID: TThreadID): Integer;

    procedure RaiseException(const Msg: String);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LockRead;
    procedure LockWrite;
    procedure UnLock;
  end;

  TRWLockEx = class(TRWLock)
  public
    procedure BeginRead; inline;
    procedure BeginWrite; inline;
    procedure EndRead; inline;
    procedure EndWrite; inline;
  end;

implementation

{ TRWNode }

constructor TRWNode.Create(const AThreadID: TThreadID; const AState: TRWNodeState);
begin
  inherited Create;

  FThreadID := AThreadID;
  FState := AState;
  FAcquires := 0;
end;

procedure TRWNode.DecAcquires;
begin
  Dec(FAcquires);
end;

procedure TRWNode.IncAcquires;
begin
  Inc(FAcquires);
end;

{ TRWLock }

constructor TRWLock.Create;
begin
  inherited Create;

  FLock := TCriticalSection.Create;
  FWaiters := TObjectList.Create;
end;

destructor TRWLock.Destroy;
begin
  {$IFDEF DEBUG}
  if FWaiters.Count > 0 then
    RaiseException('lock in use');
  {$ENDIF}

  FreeAndNil(FWaiters);
  FreeAndNil(FLock);

  inherited;
end;

function TRWLock.GetFirstWriter: Integer;
var
  RWNode: TRWNode;
begin
  Result := 0;
  while Result < FWaiters.Count do
  begin
    RWNode := TRWNode(FWaiters[Result]);

    if RWNode.State = ntWriter then
      Exit;

    Inc(Result);
  end;

  Result := MaxInt;
end;

function TRWLock.GetIndex(const ThreadID: TThreadID): Integer;
var
  RWNode: TRWNode;
begin
  Result := 0;
  while result < FWaiters.Count do
  begin
    RWNode := TRWNode(FWaiters[Result]);

    if RWNode.ThreadID = ThreadID then
      Exit;

    Inc(Result);
  end;

  Result := -1;
end;

procedure TRWLock.LockRead;
var
  RWNode: TRWNode;
  CurThread: TThreadID;
  Idx: Integer;
begin
  FLock.Enter;

  CurThread := TThread.CurrentThread.ThreadID;

  Idx := GetIndex(CurThread);
  if Idx = -1 then
  begin
    RWNode := TRWNode.Create(CurThread, ntReader);
    FWaiters.Add(RWNode);
  end
  else
    RWNode := TRWNode(FWaiters[Idx]);

  while GetIndex(CurThread) > GetFirstWriter do
  begin
    FLock.Leave;
    Sleep(1); // SwitchThread
    FLock.Enter;
  end;

  RWNode.IncAcquires;
  FLock.Leave;
end;

procedure TRWLock.LockWrite;
var
  RWNode: TRWNode;
  CurThread: TThreadID;
  Idx: Integer;
begin
  FLock.Enter;

  CurThread := TThread.CurrentThread.ThreadID;

  Idx := GetIndex(CurThread);
  if Idx = -1 then
  begin
    RWNode := TRWNode.Create(CurThread, ntWriter);
    FWaiters.Add(RWNode);
  end
  else
  begin
    RWNode := TRWNode(FWaiters[Idx]);

    //if RWNode.State = ntReader then
    //  RaiseException('Upgrade lock');

    RWNode.State := ntWriter;
  end;

  while GetIndex(CurThread) <> 0 do
  begin
    FLock.Leave;
    Sleep(1); // Switch thread
    FLock.Enter;
  end;

  RWNode.IncAcquires;
  FLock.Leave;
end;

procedure TRWLock.RaiseException(const Msg: String);
begin
  FLock.Leave;

  raise ERWLockError.Create(Msg);
end;

procedure TRWLock.UnLock;
var
  RWNode: TRWNode;
  CurThread: TThreadID;
  Idx: Integer;
begin
  FLock.Enter;
  CurThread := TThread.CurrentThread.ThreadID;

  Idx := GetIndex(CurThread);
  //if Idx > GetFirstWriter then
  //  RaiseException('Lock not held');

  RWNode := TRWNode(FWaiters[Idx]);

  RWNode.DecAcquires;

  if RWNode.Acquires = 0 then
    FWaiters.Delete(Idx);

  FLock.Leave;
end;

{ TRWLockEx }

procedure TRWLockEx.BeginRead;
begin
  LockRead;
end;

procedure TRWLockEx.BeginWrite;
begin
  LockWrite;
end;

procedure TRWLockEx.EndRead;
begin
  UnLock;
end;

procedure TRWLockEx.EndWrite;
begin
  UnLock;
end;

end.
