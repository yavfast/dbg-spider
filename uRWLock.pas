unit uRWLock;

interface

uses
  System.Classes, System.SyncObjs, System.SysUtils;

type
  TRWNodeState = (nsReader, nsWriter);

  {$IFDEF DEBUG}
  TRWLock = class;
  TRWThreadNode = class;

  TStack = Array[0..63] of Pointer;
  TLockInfo = class
  public
    ThreadID: TThreadID;
    EnterTime: TDateTime;
    Stack: TStack;

    function Clone: TLockInfo;
    function StackToStr: String;
  end;

  TLockInfoList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function Clone: TLockInfoList;
  end;

  TRWThreadLockInfoList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

  TLockEvent = procedure(Sender: TRWLock; const LockInfo: TLockInfo) of object;
  TLockWaitEvent = procedure(Sender: TRWLock) of object;
  {$ENDIF}

  TRWThreadNode = class
  strict private
    FThreadID: TThreadID;
    FReaderAcquires: Integer;
    FWriterAcquires: Integer;
    FState: TRWNodeState;

    function GetAcquires: Integer; inline;
  protected
    function Clone: TRWThreadNode;

    procedure IncAcquires;
    procedure DecAcquires;
  public
    constructor Create(const AThreadID: TThreadID);

    property ThreadID: TThreadID read FThreadID;
    property ReaderAcquires: Integer read FReaderAcquires;
    property WriterAcquires: Integer read FWriterAcquires;
    property Acquires: Integer read GetAcquires;
    property State: TRWNodeState read FState write FState;
  end;

  ERWLockError = class(Exception);

  TRWLock = class(TObject)
  strict private
    FLock: TCriticalSection;
    FWaiters: TList;

    {$IFDEF DEBUG}
    FLockInfoList: TLockInfoList;
    FLockEvent: TLockEvent;
    FLockWaitEvent: TLockWaitEvent;

    procedure AddLockInfo(const ThreadID: TThreadID);
    procedure DelLockInfo(const ThreadID: TThreadID);
    {$ENDIF}

    function GetRWNode(const Index: Integer): TRWThreadNode; inline;
    function GetRWNodeCount: Integer; inline;

    function GetFirstWriter: Integer;
    function GetWaitIndex(const ThreadID: TThreadID): Integer;
    function GetNodeIndex(const ThreadID: TThreadID): Integer;

    function AddRWNode(const AThreadID: TThreadID): TRWThreadNode;
    function GetRWThreadNode(const AThreadID: TThreadID): TRWThreadNode;

    procedure Wait;
    procedure WaitForReader(const Thread: TThreadID);
    procedure WaitForWriter(const Thread: TThreadID);

    property RWNode[const Index: Integer]: TRWThreadNode read GetRWNode;
    property RWNodeCount: Integer read GetRWNodeCount;
  protected
    procedure RaiseRWLockException(const Msg: String);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Lock(const AState: TRWNodeState);
    function TryLock(const AState: TRWNodeState): Boolean;
    procedure UnLock;

    {$IFDEF DEBUG}
    function GetStatus: TRWThreadLockInfoList;
    function GetLockStack: TLockInfoList;

    property LockEvent: TLockEvent read FLockEvent write FLockEvent;
    property LockWaitEvent: TLockWaitEvent read FLockWaitEvent write FLockWaitEvent;
    {$ENDIF}
  end;

implementation

type
  TWaitersList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create;
  end;

{ TRWNode }

function TRWThreadNode.Clone: TRWThreadNode;
begin
  Result := TRWThreadNode.Create(FThreadID);

  Result.FReaderAcquires := FReaderAcquires;
  Result.FWriterAcquires := FWriterAcquires;
  Result.FState := FState;
end;

constructor TRWThreadNode.Create(const AThreadID: TThreadID);
begin
  inherited Create;

  FThreadID := AThreadID;
  FReaderAcquires := 0;
  FWriterAcquires := 0;
  FState := nsReader;
end;

procedure TRWThreadNode.IncAcquires;
begin
  case FState of
    nsReader:
      Inc(FReaderAcquires);
    nsWriter:
      Inc(FWriterAcquires);
  end;
end;

procedure TRWThreadNode.DecAcquires;
begin
  case FState of
    nsReader:
      Dec(FReaderAcquires);
    nsWriter:
      begin
        Dec(FWriterAcquires);
        if FWriterAcquires = 0 then
          FState := nsReader;
      end;
  end;
end;

function TRWThreadNode.GetAcquires: Integer; // inline
begin
  Result := FReaderAcquires + FWriterAcquires;
end;

{ TRWLock }

constructor TRWLock.Create;
begin
  inherited Create;

  FLock := TCriticalSection.Create;
  FWaiters := TWaitersList.Create;

  {$IFDEF DEBUG}
  FLockInfoList := TLockInfoList.Create;
  FLockEvent := Nil;
  FLockWaitEvent := Nil;
  {$ENDIF}
end;

destructor TRWLock.Destroy;
begin
  {$IFDEF DEBUG}
  if FWaiters.Count > 0 then
    RaiseRWLockException('lock in use');

  FreeAndNil(FLockInfoList);
  {$ENDIF}

  FreeAndNil(FWaiters);
  FreeAndNil(FLock);

  inherited;
end;

function TRWLock.GetFirstWriter: Integer;
var
  Node: TRWThreadNode;
begin
  Result := 0;

  while Result < RWNodeCount do
  begin
    Node := RWNode[Result];

    if (Node.State = nsWriter) and (Node.WriterAcquires > 0) then
      Exit;

    Inc(Result);
  end;

  Result := MaxInt;
end;

function TRWLock.GetWaitIndex(const ThreadID: TThreadID): Integer;
var
  Idx: Integer;
  Node: TRWThreadNode;
begin
  Result := 0;
  Idx := 0;

  while Idx < RWNodeCount do
  begin
    Node := RWNode[Idx];

    if (Node.ThreadID = ThreadID) then
      Exit
    else
      if Node.Acquires <> 0 then
      begin
        Inc(Result);
        Exit;
      end;

    Inc(Idx);
  end;
end;

function TRWLock.GetNodeIndex(const ThreadID: TThreadID): Integer;
var
  Node: TRWThreadNode;
begin
  Result := 0;

  while Result < RWNodeCount do
  begin
    Node := RWNode[Result];

    if (Node.ThreadID = ThreadID) then
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
  Idx := GetNodeIndex(AThreadID);

  if Idx >= 0 then
    Result := RWNode[Idx]
  else
    Result := AddRWNode(AThreadID);
end;

{$IFDEF DEBUG}
procedure TRWLock.AddLockInfo(const ThreadID: TThreadID);
var
  LockInfo: TLockInfo;
begin
  LockInfo := TLockInfo.Create;
  LockInfo.ThreadID := ThreadID;
  LockInfo.EnterTime := Now;

  if Assigned(FLockEvent) then
    FLockEvent(Self, LockInfo);

  FLockInfoList.Add(LockInfo);
end;

procedure TRWLock.DelLockInfo(const ThreadID: TThreadID);
var
  I: Integer;
  LockInfo: TLockInfo;
begin
  for I := FLockInfoList.Count - 1 downto 0 do
  begin
    LockInfo := FLockInfoList[I];

    if LockInfo.ThreadID = ThreadID then
    begin
      FLockInfoList.Delete(I);
      Exit;
    end;
  end;
end;

function TRWLock.GetStatus: TRWThreadLockInfoList;
var
  I: Integer;
begin
  Result := TRWThreadLockInfoList.Create;

  FLock.Enter;

  for I := 0 to GetRWNodeCount - 1 do
    Result.Add(GetRWNode(I).Clone);

  FLock.Leave;
end;

function TRWLock.GetLockStack: TLockInfoList;
begin
  FLock.Enter;

  Result := FLockInfoList.Clone;

  FLock.Leave;
end;
{$ENDIF}

function TRWLock.TryLock(const AState: TRWNodeState): Boolean;
var
  CurThread: TThreadID;
  Node: TRWThreadNode;
begin
  Result := False;

  CurThread := TThread.CurrentThread.ThreadID;

  FLock.Enter;

  Node := GetRWThreadNode(CurThread);

  case AState of
    nsReader:
      Result := (GetWaitIndex(CurThread) <= GetFirstWriter);
    nsWriter:
      begin
        Result := (GetNodeIndex(CurThread) = 0);
        if Result then
          Node.State := nsWriter;
      end;
  end;

  if Result then
  begin
    Node.IncAcquires;

    {$IFDEF DEBUG}
    AddLockInfo(CurThread);
    {$ENDIF}
  end
  else
  begin
    if Node.Acquires = 0 then
      FWaiters.Remove(Node);
  end;

  FLock.Leave;
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
    nsReader:
      WaitForReader(CurThread);
    nsWriter:
      begin
        Node.State := nsWriter;
        WaitForWriter(CurThread);
      end;
  end;

  Node.IncAcquires;

  {$IFDEF DEBUG}
  AddLockInfo(CurThread);
  {$ENDIF}

  FLock.Leave;
end;

procedure TRWLock.WaitForReader(const Thread: TThreadID);
begin
  while GetWaitIndex(Thread) > GetFirstWriter do
    Wait;
end;

procedure TRWLock.WaitForWriter(const Thread: TThreadID);
begin
  while GetNodeIndex(Thread) <> 0 do
    Wait;
end;

procedure TRWLock.Wait;
begin
  {$IFDEF DEBUG}
  if Assigned(FLockWaitEvent) then
    FLockWaitEvent(Self);
  {$ENDIF}

  FLock.Leave;
  Sleep(1); // Switch thread
  FLock.Enter;
end;

procedure TRWLock.UnLock;
var
  CurThread: TThreadID;
  Idx: Integer;
  Node: TRWThreadNode;
begin
  CurThread := TThread.CurrentThread.ThreadID;

  FLock.Enter;

  Idx := GetNodeIndex(CurThread);

  if Idx >= 0 then
  begin
    Node := RWNode[Idx];

    Node.DecAcquires;

    if Node.Acquires = 0 then
      FWaiters.Delete(Idx);
  end;

  {$IFDEF DEBUG}
  if Idx < 0 then
    RaiseRWLockException('lock already released');

  DelLockInfo(CurThread);
  {$ENDIF}

  FLock.Leave;
end;

procedure TRWLock.RaiseRWLockException(const Msg: String);
begin
  FLock.Leave;

  raise ERWLockError.Create(Msg);
end;

{$IFDEF DEBUG}

{ TRWThreadLockInfoList }

procedure TRWThreadLockInfoList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    TObject(Ptr).Free;
end;

{ TLockInfoList }

function TLockInfoList.Clone: TLockInfoList;
var
  I: Integer;
  LockInfo: TLockInfo;
begin
  Result := TLockInfoList.Create;

  for I := 0 to Count - 1 do
  begin
    LockInfo := TLockInfo(Items[I]);
    Result.Add(LockInfo.Clone);
  end;
end;

procedure TLockInfoList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    TObject(Ptr).Free;
end;

{ TLockInfo }

function TLockInfo.Clone: TLockInfo;
begin
  Result := TLockInfo.Create;

  Result.ThreadID := ThreadID;
  Result.EnterTime := EnterTime;
  Result.Stack := Stack;
end;

function TLockInfo.StackToStr: String;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to High(Stack) do
  begin
    if Stack[I] = Nil then
      Break;

    Result := Result + Format(' $%p', [Stack[I]])
  end;
end;
{$ENDIF}

{ TWaitersList }

constructor TWaitersList.Create;
begin
  inherited;

  Capacity := 8;
end;

procedure TWaitersList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    TObject(Ptr).Free;
end;

end.
