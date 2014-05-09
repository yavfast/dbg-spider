unit uRWLock;

interface

uses
  System.Classes, System.SyncObjs, System.SysUtils, System.Types;

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
    FWakeUpEvent: TEvent;
    FState: TRWNodeState;
    FIsWait: Boolean;

    function GetAcquires: Integer; inline;
  protected
    function Clone: TRWThreadNode;

    procedure IncAcquires;
    procedure DecAcquires;
  public
    constructor Create(const AThreadID: TThreadID);
    destructor Destroy; override;

    procedure WakeUp; inline;
    procedure Wait; inline;

    property ThreadID: TThreadID read FThreadID;
    property ReaderAcquires: Integer read FReaderAcquires;
    property WriterAcquires: Integer read FWriterAcquires;
    property Acquires: Integer read GetAcquires;
    property State: TRWNodeState read FState write FState;
    property IsWait: Boolean read FIsWait write FIsWait;
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

    function GetNodeIndex(const ThreadID: TThreadID): Integer;

    function HasActiveWriter(CurThreadNode: TRWThreadNode): Boolean;
    function HasActiveNode(CurThreadNode: TRWThreadNode): Boolean;

    function AddRWNode(const AThreadID: TThreadID): TRWThreadNode;
    function GetRWThreadNode(const AThreadID: TThreadID): TRWThreadNode;

    procedure WakeUpWaiters;
    procedure Wait(ThreadNode: TRWThreadNode);
    procedure WaitForReader(ThreadNode: TRWThreadNode);
    procedure WaitForWriter(ThreadNode: TRWThreadNode);

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
  Result.FIsWait := FIsWait;
end;

constructor TRWThreadNode.Create(const AThreadID: TThreadID);
begin
  inherited Create;

  FThreadID := AThreadID;
  FReaderAcquires := 0;
  FWriterAcquires := 0;
  FWakeUpEvent := TEvent.Create(nil, False, True, '');
  FState := nsReader;
  FIsWait := True;
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

procedure TRWThreadNode.Wait; // inline;
begin
  FWakeUpEvent.WaitFor(INFINITE);
end;

procedure TRWThreadNode.WakeUp; // inline;
begin
  FWakeUpEvent.SetEvent;
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

destructor TRWThreadNode.Destroy;
begin
  FreeAndNil(FWakeUpEvent);

  inherited;
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

function TRWLock.HasActiveNode(CurThreadNode: TRWThreadNode): Boolean;
var
  Idx: Integer;
  Node: TRWThreadNode;
begin
  Result := True;
  Idx := 0;

  while Idx < RWNodeCount do
  begin
    Node := RWNode[Idx];

    if Node <> CurThreadNode then
      if (Node.WriterAcquires > 0) or ((Node.ReaderAcquires > 0) and not(Node.IsWait)) then
        Exit;

    Inc(Idx);
  end;

  Result := False;
end;

function TRWLock.HasActiveWriter(CurThreadNode: TRWThreadNode): Boolean;
var
  Idx: Integer;
  Node: TRWThreadNode;
begin
  Result := True;
  Idx := 0;

  while Idx < RWNodeCount do
  begin
    Node := RWNode[Idx];

    if Node <> CurThreadNode then
      if (Node.State = nsWriter) and (Node.WriterAcquires > 0) then
        Exit;

    Inc(Idx);
  end;

  Result := False;
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
  ThreadNode: TRWThreadNode;
begin
  Result := False;

  FLock.Enter;

  ThreadNode := GetRWThreadNode(TThread.CurrentThread.ThreadID);

  case AState of
    nsReader:
      Result := not(HasActiveWriter(ThreadNode));
    nsWriter:
      begin
        Result := not(HasActiveNode(ThreadNode));
        if Result then
          ThreadNode.State := nsWriter;
      end;
  end;

  if Result then
  begin
    ThreadNode.IncAcquires;

    {$IFDEF DEBUG}
    AddLockInfo(ThreadNode.ThreadID);
    {$ENDIF}
  end
  else
  begin
    if ThreadNode.Acquires = 0 then
      FWaiters.Remove(ThreadNode);
  end;

  FLock.Leave;
end;

procedure TRWLock.Lock(const AState: TRWNodeState);
var
  ThreadNode: TRWThreadNode;
begin
  FLock.Enter;

  ThreadNode := GetRWThreadNode(TThread.CurrentThread.ThreadID);
  ThreadNode.IsWait := True;

  case AState of
    nsReader:
      begin
        WaitForReader(ThreadNode);
      end;
    nsWriter:
      begin
        ThreadNode.State := nsWriter;
        WaitForWriter(ThreadNode);
      end;
  end;

  ThreadNode.IncAcquires;
  ThreadNode.IsWait := False;

  {$IFDEF DEBUG}
  AddLockInfo(ThreadNode.ThreadID);
  {$ENDIF}

  FLock.Leave;
end;

procedure TRWLock.WaitForReader(ThreadNode: TRWThreadNode);
begin
  // Ќе должно быть активных писателей
  while HasActiveWriter(ThreadNode) do
    Wait(ThreadNode);
end;

procedure TRWLock.WaitForWriter(ThreadNode: TRWThreadNode);
begin
  // Ќе должно быть активных читателей или писателей
  while HasActiveNode(ThreadNode) do
    Wait(ThreadNode);
end;

procedure TRWLock.WakeUpWaiters;
var
  Idx: Integer;
begin
  for Idx := 0 to RWNodeCount - 1 do
    RWNode[Idx].WakeUp;
End;

procedure TRWLock.Wait(ThreadNode: TRWThreadNode);
begin
  {$IFDEF DEBUG}
  if Assigned(FLockWaitEvent) then
    FLockWaitEvent(Self);
  {$ENDIF}

  FLock.Leave;

  ThreadNode.Wait;

  FLock.Enter;
end;

procedure TRWLock.UnLock;
var
  Idx: Integer;
  ThreadNode: TRWThreadNode;
begin
  FLock.Enter;

  Idx := GetNodeIndex(TThread.CurrentThread.ThreadID);

  if Idx >= 0 then
  begin
    ThreadNode := RWNode[Idx];

    ThreadNode.DecAcquires;

    if ThreadNode.Acquires = 0 then
      FWaiters.Delete(Idx);
  end;

  {$IFDEF DEBUG}
  if Idx < 0 then
    RaiseRWLockException('lock already released');

  DelLockInfo(TThread.CurrentThread.ThreadID);
  {$ENDIF}

  WakeUpWaiters;

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
