unit uSharedObject;

interface

uses System.Classes;

type
  TSharedObject = class(TInterfacedObject)
  public
    constructor Create;
    destructor Destroy; override;

    procedure BeforeDestruction; override;
    procedure FreeInstance; override;

    procedure CreateRef(var DestObject);
    function IsFinallyDestroy: Boolean;

    procedure Lock;
    procedure UnLock;
    function TryLock: Boolean;
  end;

implementation

{ TSharedObject }

procedure TSharedObject.BeforeDestruction;
begin
  // Skip TInterfacedObject.BeforeDestruction
end;

constructor TSharedObject.Create;
begin
  inherited Create;
  _AddRef;
end;

destructor TSharedObject.Destroy;
begin
  Lock;
  if FRefCount > 0 then
    if AtomicDecrement(FRefCount) > 0 then
      UnLock;
end;

function TSharedObject.IsFinallyDestroy: Boolean;
begin
  Lock;
  Result := (FRefCount = 1);
  if not Result then
    UnLock;
end;

procedure TSharedObject.FreeInstance;
begin
  Lock;
  if FRefCount = 0 then
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
  TMonitor.Enter(Self);
end;

procedure TSharedObject.UnLock;
begin
  TMonitor.Exit(Self);
end;

function TSharedObject.TryLock: Boolean;
begin
  Result := TMonitor.TryEnter(Self);
end;

end.
