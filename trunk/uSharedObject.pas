unit uSharedObject;

interface

uses System.Classes;

type
  TSpinLock = record
  private
    const
      SHORT_WAIT_COUNT = 40;
  private
    FLockCount: NativeInt;
    FOwner: TThreadID;
    procedure ShortWait; inline;
    procedure LongWait; inline;
  public
    procedure Create; inline;

    procedure Enter;
    function TryEnter: Boolean;
    procedure Leave;
  end;

  TSharedObject = class(TInterfacedObject)
  private
    FSpinLock: TSpinLock;
  protected
    function IsFinallyDestroy: Boolean; inline;
  public
    constructor Create;

    procedure BeforeDestruction; override;
    procedure FreeInstance; override;

    procedure CreateRef(var DestObject);

    procedure Lock;
    procedure UnLock;
    function TryLock: Boolean;
  end;

implementation

{ TSharedObject }

constructor TSharedObject.Create;
begin
  inherited Create;

  FSpinLock.Create;

  _AddRef;
end;

function TSharedObject.IsFinallyDestroy: Boolean;
begin
  Result := (FRefCount = 0);
end;

procedure TSharedObject.BeforeDestruction;
begin
  Lock;
  AtomicDecrement(FRefCount);
end;

procedure TSharedObject.FreeInstance;
begin
  if IsFinallyDestroy then
    inherited
  else
    UnLock;
end;

procedure TSharedObject.CreateRef(var DestObject);
begin
  Lock;
  try
    Pointer(DestObject) := Pointer(Self);
    _AddRef;
  finally
    UnLock;
  end;
end;

procedure TSharedObject.Lock;
begin
  FSpinLock.Enter;
end;

procedure TSharedObject.UnLock;
begin
  FSpinLock.Leave;
end;

function TSharedObject.TryLock: Boolean;
begin
  Result := FSpinLock.TryEnter;
end;

{ TSpinLock }

procedure TSpinLock.Create;
begin
  FLockCount := 0;
  FOwner := 0;
end;

procedure TSpinLock.LongWait;
begin
  TThread.Sleep(1);
end;

procedure TSpinLock.ShortWait;
begin
  YieldProcessor;
end;

procedure TSpinLock.Enter;
var
  CurThreadId: TThreadID;
  WaitCount: Integer;
begin
  CurThreadId := TThread.CurrentThread.ThreadID;

  if CurThreadId = FOwner then
    AtomicIncrement(FLockCount)
  else
  begin
    WaitCount := 0;
    while AtomicCmpExchange(FLockCount, 1, 0) <> 0 do
    begin
      if (WaitCount < SHORT_WAIT_COUNT) and (CPUCount > 1) then
      begin
        ShortWait;
        Inc(WaitCount);
      end
      else
        LongWait;
    end;

    FOwner := CurThreadId;
  end;
end;

function TSpinLock.TryEnter: Boolean;
var
  CurThreadId: TThreadID;
begin
  Result := True;

  CurThreadId := TThread.CurrentThread.ThreadID;

  if CurThreadId = FOwner then
    AtomicIncrement(FLockCount)
  else
    if AtomicCmpExchange(FLockCount, 1, 0) = 0 then
      FOwner := CurThreadId
    else
      Result := False;
end;

procedure TSpinLock.Leave;
var
  CurThreadId: TThreadID;
begin
  CurThreadId := TThread.CurrentThread.ThreadID;

  if (FOwner = CurThreadId) and (FLockCount > 0) then
  begin
    if FLockCount = 1 then
    begin
      FOwner := 0;
      AtomicExchange(FLockCount, 0);
    end
    else
      AtomicDecrement(FLockCount);
  end;
end;

end.
