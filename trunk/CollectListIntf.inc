const
  _SEGMENT_SIZE = 16 * 1024;

type
  TSegment<T> = Array of T;

  TSegList<T> = Array of TSegment<T>;

  TCollectListError = class(Exception);

  PData = Pointer;

  TCollectList<T> = class
  private
    FCount: Cardinal;
    FSegSize: Cardinal;
    FLock: TCriticalSection;
    FSegList: TSegList<T>;
    function GetItem(const Index: Cardinal): PData;
    procedure CheckSeg(const Seg: Integer);
  protected
    function IndexToSegment(const Index: Cardinal; var Seg, Offset: Integer): Boolean;
    procedure RaiseError(Msg: PString; const Args: Array of const);
  public

    constructor Create;
    destructor Destroy; override;

    function Add: PData;

    procedure Clear;

    procedure Lock;
    procedure UnLock;

    property Count: Cardinal read FCount;
    property Items[const Index: Cardinal]: PData read GetItem; default;
  end;
