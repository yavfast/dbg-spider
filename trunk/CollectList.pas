unit CollectList;

interface

uses Classes, SysUtils, SyncObjs, ClassUtils;

const
  _DEF_SEGMENT_SIZE = 16 * 1024;
  _SEG_LIST_GROW = 16;

type
  TSegment<T> = Array of T;

  TSegList<T> = Array of TSegment<T>;

  TCollectListError = class(Exception);

  PData = Pointer;

  TBaseCollectList = class
  private
    FCount: Integer;
    FAddCount: Integer;
    FLock: TMREWSync;
  protected
    FSegLength: Integer;

    function GetItem(const Index: Integer): PData; virtual; abstract;
    procedure CheckSeg(const Seg: Integer); virtual; abstract;

    function IndexToSegment(const Index: Integer; var Seg, Offset: Integer): Boolean;
    procedure RaiseError(Msg: PString; const Args: Array of const);
  public
    constructor Create;
    destructor Destroy; override;

    function Add: PData; virtual;
    procedure Commit; virtual;
    procedure Clear; virtual;

    procedure BeginRead; inline;
    procedure EndRead; inline;
    procedure BeginWrite; inline;
    procedure EndWrite; inline;

    property Count: Integer read FCount;
    property Items[const Index: Integer]: PData read GetItem; default;
    property Lock: TMREWSync read FLock;
  end;

  TCollectList<T> = class(TBaseCollectList)
  private
    FSegList: TSegList<T>;
  protected
    function GetItem(const Index: Integer): PData; override;
    procedure CheckSeg(const Seg: Integer); override;
  public
    constructor Create(const SegSize: Integer = _DEF_SEGMENT_SIZE);
    destructor Destroy; override;

    function Add: PData; override;
    procedure Clear; override;
  end;

implementation

{ TBaseCollectList }

function TBaseCollectList.Add: PData;
begin
  Result := Nil;
  AtomicIncrement(FAddCount);
end;

procedure TBaseCollectList.Clear;
begin
  FCount := 0;
end;

procedure TBaseCollectList.Commit;
begin
  BeginWrite;
  AtomicIncrement(FCount, FAddCount);
  AtomicExchange(FAddCount, 0);
  EndWrite;
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

function TBaseCollectList.IndexToSegment(const Index: Integer; var Seg, Offset: Integer): Boolean;
begin
  Result := (Index < FCount) and (Index >= 0);

  Seg := Index div FSegLength;
  Offset := Index mod FSegLength;
end;

{ TCollectList<T> }

function TCollectList<T>.Add: PData;
var
  Seg, Offset: Integer;
  NextIdx: Integer;
begin
  BeginRead;

  // Резервируем следующий элемент
  NextIdx := FCount + AtomicIncrement(FAddCount) - 1;

  // Проверяем доступность сегмента
  IndexToSegment(NextIdx, Seg, Offset);
  CheckSeg(Seg);

  // Получаем указатель на новый элемент
  Result := @FSegList[Seg][Offset];

  EndRead;

  FillChar(Result^, SizeOf(T), 0);
  //Initialize(T(Result^));
end;

procedure TCollectList<T>.CheckSeg(const Seg: Integer);
begin
  //BeginRead;

  if Length(FSegList) <= Seg then
  begin
    BeginWrite;
    SetLength(FSegList, Seg + _SEG_LIST_GROW);
    SetLength(FSegList[Seg], FSegLength);
    EndWrite;
  end;

  if Length(FSegList[Seg]) = 0 then
  begin
    BeginWrite;
    SetLength(FSegList[Seg], FSegLength);
    EndWrite;
  end;

  //EndRead;
end;

procedure TCollectList<T>.Clear;
begin
  BeginWrite;
  inherited Clear;
  SetLength(FSegList, 0);
  EndWrite;
end;

constructor TCollectList<T>.Create(const SegSize: Integer = _DEF_SEGMENT_SIZE);
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

function TCollectList<T>.GetItem(const Index: Integer): PData;
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
