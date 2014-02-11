unit CollectList;

interface

uses Classes, SysUtils, SyncObjs, ClassUtils;

const
  _DEF_SEGMENT_SIZE = 16 * 1024;

type
  TSegment<T> = Array of T;

  TSegList<T> = Array of TSegment<T>;

  TCollectListError = class(Exception);

  PData = Pointer;

  TBaseCollectList = class
  private
    FCount: Cardinal;
    FLock: TMREWSync;
  protected
    FSegLength: Cardinal;

    function GetItem(const Index: Cardinal): PData; virtual; abstract;
    procedure CheckSeg(const Seg: Integer); virtual; abstract;

    function IndexToSegment(const Index: Cardinal; var Seg, Offset: Integer): Boolean;
    procedure RaiseError(Msg: PString; const Args: Array of const);
  public
    constructor Create;
    destructor Destroy; override;

    function Add: PData; virtual;
    procedure Clear; virtual;

    procedure BeginRead; inline;
    procedure EndRead; inline;
    procedure BeginWrite; inline;
    procedure EndWrite; inline;

    property Count: Cardinal read FCount;
    property Items[const Index: Cardinal]: PData read GetItem; default;
    property Lock: TMREWSync read FLock;
  end;

  TCollectList<T> = class(TBaseCollectList)
  private
    FSegList: TSegList<T>;
  protected
    function GetItem(const Index: Cardinal): PData; override;
    procedure CheckSeg(const Seg: Integer); override;
  public
    constructor Create(const SegSize: Cardinal = _DEF_SEGMENT_SIZE);
    destructor Destroy; override;

    function Add: PData; override;
    procedure Clear; override;
  end;

implementation

{ TBaseCollectList }

function TBaseCollectList.Add: PData;
begin
  Result := Nil;
  Inc(FCount);
end;

procedure TBaseCollectList.Clear;
begin
  FCount := 0;
end;

constructor TBaseCollectList.Create;
begin
  inherited;

  FCount := 0;
  FLock := TMREWSync.Create;
end;

destructor TBaseCollectList.Destroy;
begin
  Clear;
  FreeAndNil(FLock);

  inherited;
end;

procedure TBaseCollectList.BeginRead;
begin
  FLock.BeginRead;
end;

procedure TBaseCollectList.BeginWrite;
begin
  FLock.BeginWrite;
end;

procedure TBaseCollectList.RaiseError(Msg: PString; const Args: array of const);
begin
  raise TCollectListError.CreateFmt(Msg^, Args);
end;

procedure TBaseCollectList.EndRead;
begin
  FLock.EndRead;
end;

procedure TBaseCollectList.EndWrite;
begin
  FLock.EndWrite;
end;

function TBaseCollectList.IndexToSegment(const Index: Cardinal; var Seg, Offset: Integer): Boolean;
begin
  Result := Index < Count;

  Seg := Index div FSegLength;
  Offset := Index mod FSegLength;
end;

{ TCollectList<T> }

function TCollectList<T>.Add: PData;
var
  Idx: Cardinal;
  Seg, Offset: Integer;
begin
  BeginWrite;
  Idx := Count;
  IndexToSegment(Idx, Seg, Offset);
  CheckSeg(Seg);
  inherited Add;

  Result := @FSegList[Seg][Offset];

  FillChar(Result^, SizeOf(T), 0);
  EndWrite;
end;

procedure TCollectList<T>.CheckSeg(const Seg: Integer);
begin
  BeginRead;
  if Length(FSegList) <= Seg then
  begin
    BeginWrite;
    SetLength(FSegList, Seg + 1);
    SetLength(FSegList[Seg], FSegLength);
    EndWrite;
  end;
  EndRead;
end;

procedure TCollectList<T>.Clear;
begin
  BeginWrite;
  SetLength(FSegList, 0);
  inherited Clear;
  EndWrite;
end;

constructor TCollectList<T>.Create(const SegSize: Cardinal = _DEF_SEGMENT_SIZE);
begin
  inherited Create;

  FSegLength := SegSize div SizeOf(T);
  SetLength(FSegList, 0);
end;

destructor TCollectList<T>.Destroy;
begin
  Clear;

  inherited;
end;

function TCollectList<T>.GetItem(const Index: Cardinal): PData;
var
  Seg, Offset: Integer;
begin
  Result := nil;

  if IndexToSegment(Index, Seg, Offset) then
  begin
    BeginRead;
    Result := @FSegList[Seg][Offset];
    EndRead;
  end
  else
    RaiseError(@EIndexError, [Index]);
end;

end.
