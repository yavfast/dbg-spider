unit DbgHookCS;

interface

uses Windows;

type
  TDbgCriticalSection = class
  private
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Enter;
    procedure Leave;

    function TryEnter: Boolean;
  end;


implementation

uses DbgHookSyncObjs;

{ TDbgCriticalSection }

constructor TDbgCriticalSection.Create;
begin
  inherited;

  InitializeCriticalSection(FLock);
end;

destructor TDbgCriticalSection.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TDbgCriticalSection.Enter;
begin
  if SyncObjsHooked and Assigned(Kernel32_EnterCriticalSection) then
    Kernel32_EnterCriticalSection(FLock)
  else
    EnterCriticalSection(FLock);
end;

procedure TDbgCriticalSection.Leave;
begin
  if SyncObjsHooked and Assigned(Kernel32_LeaveCriticalSection) then
    Kernel32_LeaveCriticalSection(FLock)
  else
    LeaveCriticalSection(FLock);
end;

function TDbgCriticalSection.TryEnter: Boolean;
begin
  Result := TryEnterCriticalSection(FLock);
end;

end.
