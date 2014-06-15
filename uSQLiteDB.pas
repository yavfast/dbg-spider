unit uSQLiteDB;

interface

uses
  System.Contnrs, System.SysUtils, System.Classes, System.Types, System.Sqlite,
  uRWLock;

type
  HSQLCONTEXT = Pointer;
  HSQLDB = Pointer;
  HSQLQUERY = Pointer;
  HSQLVALUE = Pointer;
  
  TSQLBase = class;
  TSQLColumnType = (sctUnknown = 0, sctInteger = 1, sctFloat = 2, sctText = 3, sctBlob = 4, sctNull = 5);

  { TAIMPSqlColumn }

  TSQLColumn = class(TObject)
  private
    FDataType: TSQLColumnType;
    FName: String;
  public
    property DataType: TSQLColumnType read FDataType;
    property Name: String read FName;
  end;

  { TAIMPSqlTable }

  TSQLTableClass = class of TSQLTable;
  TSQLTable = class(TObject)
  private
    FDataBase: TSQLBase;
    FQuery: HSQLQUERY;

    FColumns: TObjectList;
    FDataTypesFetched: Boolean;

    function FetchDataTypes: Boolean;
    function GetColumn(const Index: Integer): TSQLColumn;
    function GetColumnCount: Integer;
    function GetFieldIndex(const Name: String): Integer;
    function GetValue(const ColumnName: String): Variant;

    function GetActive: Boolean; inline;
    procedure SetActive(const Value: Boolean);
  protected
    procedure CheckActive;
    procedure CheckDBActive;
    procedure UpdateQuery; virtual;
    procedure ClearQuery; virtual;
  public
    constructor Create(ADataBase: TSQLBase; AQuery: HSQLQUERY);
    destructor Destroy; override;

    function NextRecord: Boolean;
    // I/O
    function ReadBlob(const AIndex: Integer; AData: TMemoryStream): Integer; overload;
    function ReadBlob(const AName: String; AData: TMemoryStream): Integer; overload;
    function ReadDouble(const AIndex: Integer): Double; overload;
    function ReadDouble(const AName: String): Double; overload;
    function ReadInt(const AIndex: Integer): Integer; overload;
    function ReadInt(const AName: String): Integer; overload;
    function ReadStr(const AIndex: Integer): String; overload;
    function ReadStr(const AName: String): String; overload;
    function ReadInt64(const AIndex: Integer): Int64; overload;
    function ReadInt64(const AName: String): Int64; overload;
    function ReadDateTime(const AIndex: Integer): TDateTime; overload;
    function ReadDateTime(const AName: String): TDateTime; overload;
    // Properties
    property Column[const Index: Integer]: TSQLColumn read GetColumn; default;
    property ColumnCount: Integer read GetColumnCount;
    property Value[const ColumnName: String]: Variant read GetValue;
    property DataBase: TSQLBase read FDataBase;

    function FieldExists(const Name: String): Boolean;

    property Active: Boolean read GetActive write SetActive;
  end;

  TSQLView = class(TSQLTable)
  private
    FParamCount: Integer;
    FQueryStr: String;
    FLock: TRWLock;

    procedure InitParams;
  protected
    procedure UpdateQuery; override;
    procedure Reset;
    procedure SetParamAsText(const ParamIdx: Integer; const Str: String);

    property Lock: TRWLock read FLock;
  public
    constructor Create(ADataBase: TSQLBase; AQuery: HSQLQUERY; const AQueryStr: String = ''); overload;
    constructor Create(ADataBase: TSQLBase; const AQueryStr: String); overload;

    destructor Destroy; override;

    procedure SetParam(const ParamName: String; const Value: Variant); overload;
    procedure SetParam(const ParamIdx: Integer; const Value: Variant); overload;

    procedure BeginExecute;
    procedure EndExecute;
    function Execute: Boolean;
  end;

  TSQLConnectMgr = class;

  TOnErrorEvent = procedure(const ErrorCode: Integer; const ErrorText, Query: String) of object;

  // PRAGMA journal_mode = DELETE | TRUNCATE | PERSIST | MEMORY | WAL | OFF
  TSQLJournalModeType = (ttDefault = 0, ttDelete, ttTruncate, ttPersist, ttMemory, ttWAL, ttOff);

  // PRAGMA synchronous = 0 | OFF | 1 | NORMAL | 2 | FULL;
  TSQLSynchronousType = (stDefault = 0, stOff, stNormal, stFull);

  TSQLBeginTransactionType = (btGlobal = 0, btDeferred, btImmediate, btExclusive);

  TSQLCodes = set of Byte;

  { TAIMPSqlBase }

  TSQLBase = class(TObject)
  private
    FDB: HSQLDB;
    FConnectMgr: TSQLConnectMgr;
    FLinkedTables: TObjectList;

    FLock: TRWLock;

    FTransactions: Integer;

    FOnError: TOnErrorEvent;
    FJournalModeType: TSQLJournalModeType;
    FSynchronousType: TSQLSynchronousType;

    FGlobalTransactionLock: TRWLock;
    FInGlobalTransaction: Boolean;

    function CheckError(const ErrorCode: Integer; const ValidCodes: TSQLCodes = [SQLITE_OK]; const Query: String = ''): Boolean;
    function PrepareQuery(const AQueryStr: String; out AQuery: HSQLQUERY): Boolean;
    Function DoExecStep(AQueryHandle: HSQLQUERY): Integer;

    function GetFileName: String;

    procedure SetJournalModeType(const Value: TSQLJournalModeType);
    procedure SetSynchronousType(const Value: TSQLSynchronousType);

    procedure AddLinkedTable(Table: TSQLTable);
    procedure RemoveLinkedTable(Table: TSQLTable);

    function GetActive: Boolean; inline;
    procedure SetActive(const Value: Boolean);
  protected
    procedure DataBaseInitTables; virtual;
    procedure ErrorMsg(const AText: String; const ErrorCode: Integer; const Query: String = ''); overload; virtual;
    procedure ErrorMsg(AText: PChar; const ErrorCode: Integer; const Query: String = ''); overload;

    procedure Open;
    procedure Close;

    procedure CheckActive;
  public
    constructor Create(AConnectMgr: TSQLConnectMgr);
    destructor Destroy; override;

    function ClearAll: Boolean; virtual;
    procedure Compress;
    function PopulateTableNames(out AList: TStrings): Boolean;

    procedure BeginTransaction(const ATransactionType: TSQLBeginTransactionType = btDeferred);
    procedure EndTransaction;
    procedure CancelTransaction;

    function ExecSQL(const AQuery: String): Boolean; overload;
    function ExecSQL(const AQuery: String; out ATable: TSQLTable): Boolean; overload;

    function CreateView(const AQuery: String): TSQLView;
    procedure UpdatePreparedStatements;
    procedure ClosePreparedStatements;

    function GetLastInsertRowID: Int64;

    // Blobs: Use "?" symbol in Query for set data position
    function ExecInsertBlob(const AQuery: String; AData: TMemoryStream): Boolean;
    // Properties
    property FileName: String read GetFileName;

    //кол-во модифицированных строк
    Function GetRowAffected : Integer;
    //кол-во столбцов в таблице
    Function GetColCount(Const ATableName: String): Integer;
    //пустая таблица
    Function IsEmpty(Const ATableName: String): Boolean;
    Function DeleteTable(Const ATableName: String): Boolean;

    Procedure Lock(const LockType: TRWNodeState = nsWriter);
    Procedure UnLock;

    Procedure GlobalLock; inline;
    procedure GlobalUnLock; inline;

    Function GetVersion: Integer;

    property ConnectMgr: TSQLConnectMgr read FConnectMgr;
    property OnError: TOnErrorEvent read FOnError write FOnError;
    property JournalModeType: TSQLJournalModeType read FJournalModeType write SetJournalModeType;
    property SynchronousType: TSQLSynchronousType read FSynchronousType write SetSynchronousType;

    property Active: Boolean read GetActive write SetActive;
  end;

  TSQLConnectMgr = class(TObject)
  private
    FDBFileName: String;
    FLock: TRWLock;
    FDBConnections: TList;
    FActive: Boolean;

    procedure SetDBFileName(const Value: String);
    procedure SetActive(const Value: Boolean);
  public
    constructor Create(const ADBFileName: String);
    destructor Destroy; override;

    function GetNewConnection: TSQLBase;
    function GetDefaultConnection: TSQLBase;

    procedure Remove(Connect: TSQLBase);
    procedure Reset(const FreeConnections: Boolean = True);

    property DBFileName: String read FDBFileName write SetDBFileName;
    property Active: Boolean read FActive write SetActive;
  end;

  TDBException = class(Exception);

function DBConnectMgr(const DBName: String): TSQLConnectMgr;
function GetDBConnections: TStringList;
procedure DBResetAll(const FreeConnections: Boolean = True);

function sqlite_DateTimeToStr(const DateTime: TDateTime): string;
function sqlite_TryStrToDateTime(const Str: String; var Res: TDateTime): Boolean;
function sqlite_StrToDateTime(const Str: String; const DefValue: TDateTime = 0): TDateTime;
function sqlite_Str(const Str: String; var Count: Integer): PWideChar;

implementation

uses
  System.Variants, System.StrUtils, System.WideStrUtils;

type
  TSQLCompare = function (P1: PChar; P1Size: Integer; P2: PChar; P2Size: Integer): Integer; cdecl;
  TSQLFunction = procedure (Context: HSQLCONTEXT; ArgCount: Integer; ArgVars: PPointer); cdecl;
  TSQLFunctionEnd = procedure (Context: HSQLCONTEXT); cdecl;
  TSQLiteBusyHandlerCallback = function(UserData: Pointer; P2: integer): integer; cdecl;

  TDBConnectMgrList = TStringList;

var
  _DBConnectMgrList: TDBConnectMgrList = nil;
  _DBConnectMgrLock: TRWLock = nil;

function DBConnectMgr(const DBName: String): TSQLConnectMgr;
var
  Idx: Integer;
  DBAlias: string;
begin
  Result := Nil;

  if _DBConnectMgrLock = Nil then Exit;

  DBAlias := AnsiLowerCase(ExtractFileName(DBName));

  _DBConnectMgrLock.Lock(nsReader);
  try
    Idx := -1;
    if _DBConnectMgrList <> nil then
      Idx := _DBConnectMgrList.IndexOf(DBAlias);

    if Idx >= 0 then
      Result := TSQLConnectMgr(_DBConnectMgrList.Objects[Idx])
    else
    begin
      _DBConnectMgrLock.Lock(nsWriter);
      try
        if _DBConnectMgrList.IndexOf(DBAlias) < 0 then
        begin
          Result := TSQLConnectMgr.Create(DBName);
          _DBConnectMgrList.AddObject(DBAlias, Result);
          Result.Active := True;
        end
        else
          Result := DBConnectMgr(DBName);
      finally
        _DBConnectMgrLock.UnLock;
      end;
    end;

    // На случай, если сменили путь к базе
    if not AnsiSameText(Result.DBFileName, DBName) then
      Result.DBFileName := DBName;
  finally
    _DBConnectMgrLock.UnLock;
  end;
end;

function GetDBConnections: TStringList;
begin
  Result := TStringList.Create;

  _DBConnectMgrLock.Lock(nsReader);
  try
    Result.Assign(_DBConnectMgrList);
  finally
    _DBConnectMgrLock.UnLock;
  end;
end;

procedure DBResetAll(const FreeConnections: Boolean = True);
var
  Mgr: TSQLConnectMgr;
  I: Integer;
begin
  _DBConnectMgrLock.Lock(nsWriter);
  try
    if Assigned(_DBConnectMgrList) then
    begin
      for I := _DBConnectMgrList.Count - 1 downto 0 do
      begin
        Mgr := TSQLConnectMgr(_DBConnectMgrList.Objects[I]);
        Mgr.Active := False;

        if FreeConnections then
        begin
          _DBConnectMgrList.Objects[I] := nil;
          if Assigned(Mgr) then
            FreeAndNil(Mgr);
        end;
      end;

      if FreeConnections then
        _DBConnectMgrList.Clear;
    end;
  finally
    _DBConnectMgrLock.UnLock;
  end;
end;

const
  SQLITE_DATE_FMT = 'yyyy-mm-dd';
  SQLITE_TIME_FMT = 'hh:nn:ss';
  SQLITE_DATETIME_FMT = SQLITE_DATE_FMT + ' ' + SQLITE_TIME_FMT;

function sqlite_DateTimeToStr(const DateTime: TDateTime): string;
begin
  if DateTime <> 0 then
    Result := FormatDateTime(SQLITE_DATETIME_FMT, DateTime)
  else
    Result := '';
end;

var
  _sqlite_DateTimeFormat: TFormatSettings;

function sqlite_TryStrToDateTime(const Str: String; var Res: TDateTime): Boolean;
begin
  Result := TryStrToDateTime(Str, Res, _sqlite_DateTimeFormat);
end;

function sqlite_StrToDateTime(const Str: String; const DefValue: TDateTime = 0): TDateTime;
begin
  if not sqlite_TryStrToDateTime(Str, Result) then
    Result := DefValue;
end;

function sqlite_Str(const Str: String; var Count: Integer): PWideChar;
var
  StrBuf: RawByteString;
begin
  StrBuf := UTF8Encode(Str);

  Count := Length(StrBuf);
  if Count > 0 then
    Result := PWideChar(Pointer(@StrBuf[1]))
  else
    Result := Nil;
end;

function sqlite_StrToString(Str: PChar; Count: Integer): String;
var
  Buf: RawByteString;
begin
  Result := '';

  if Count > 0 then
  begin
    SetLength(Buf, Count);
    Move(Str^, PAnsiChar(Buf)^, Count);

    Result := UTF8ToString(Buf);
  end;
end;

function UnicodeCompare(UserData: Pointer; P1Size: Integer; P1: PChar; P2Size: Integer; P2: PChar): Integer; cdecl;
var
  S1, S2: String;
begin
  S1 := sqlite_StrToString(P1, P1Size);
  S2 := sqlite_StrToString(P2, P2Size);

  Result := AnsiCompareText(S1, S2);
end;

procedure SQLiteUpper(Context: HSQLCONTEXT; ArgCount: Integer; ArgVars: PPointerArray); cdecl;
var
  Arg: Pointer;
  Cnt: Integer;
  BufIn: Pointer;
  StrRes: String;
  BufOut: PWideChar;
begin
  Arg := ArgVars^[0];
  if (sqlite3_value_type(Arg) <> SQLITE_NULL) then
  begin
    Cnt := sqlite3_value_bytes16(Arg);
    if Cnt > 0 then
    begin
      BufIn := sqlite3_value_text16(Arg);

      StrRes := sqlite_StrToString(BufIn, Cnt);
      StrRes := AnsiUpperCase(StrRes);
      BufOut := sqlite_Str(StrRes, Cnt); // TODO: MemLeak?

      sqlite3_result_text16(Context, BufOut, Cnt, nil);
    end
    else
      sqlite3_result_text16(Context, nil, 0, nil);
  end
  else
    sqlite3_result_null(Context);
end;

{ TAIMPSqlTable }

procedure TSQLTable.CheckActive;
begin
  if not Active then
    raise TDBException.Create('Table is not active');
end;

procedure TSQLTable.CheckDBActive;
begin
  if not FDataBase.Active then
    raise TDBException.Create('Linked DB is not active');
end;

procedure TSQLTable.ClearQuery;
begin
  if Active then
  try
    CheckDBActive;

    FreeAndNil(FColumns);

    FDataBase.GlobalLock;
    try
      FDataBase.CheckError(sqlite3_finalize(FQuery), [SQLITE_OK, SQLITE_ABORT]);
    finally
      FDataBase.GlobalUnLock;
    end;
  finally
    FQuery := nil;
  end;
end;

constructor TSQLTable.Create(ADataBase: TSQLBase; AQuery: HSQLQUERY);
begin
  inherited Create;
  FColumns := TObjectList.Create;
  FDataBase := ADataBase;
  FQuery := AQuery;

  FDataBase.AddLinkedTable(Self);
end;

destructor TSQLTable.Destroy;
begin
  FDataBase.RemoveLinkedTable(Self);

  ClearQuery;

  inherited Destroy;
end;

function TSQLTable.FetchDataTypes: Boolean;
var
  AColumn   : TSQLColumn;
  ADataType: Integer;
  I, ACount : Integer;
begin
  FColumns.Clear;

  CheckActive;
  CheckDBActive;

  FDataBase.GlobalLock;
  try
    ACount := sqlite3_column_count(FQuery);
    FColumns.Capacity := ACount;
    for I := 0 to ACount - 1 do
    begin
      AColumn := TSQLColumn.Create;
      AColumn.FName := sqlite3_column_name16(FQuery, I);

      ADataType := sqlite3_column_type(FQuery, I);
      AColumn.FDataType := TSQLColumnType(ADataType);

      FColumns.Add(AColumn);
    end;
  finally
    FDataBase.GlobalUnLock;
  end;

  Result := True;
end;

function TSQLTable.FieldExists(const Name: String): Boolean;
begin
  Result := GetFieldIndex(Name) >= 0;
end;

function TSQLTable.NextRecord: Boolean;
var
  ResCode: Integer;
begin
  CheckActive;
  CheckDBActive;

  FDataBase.GlobalLock;
  try
    ResCode := FDataBase.DoExecStep(FQuery);
    Result := FDataBase.CheckError(ResCode, [SQLITE_ROW, SQLITE_DONE]) and (ResCode = SQLITE_ROW);
  finally
    FDataBase.GlobalUnLock;
  end;
end;

function TSQLTable.ReadBlob(const AIndex: Integer; AData: TMemoryStream): Integer;
var
  ABlobBuffer: PByte;
begin
  CheckActive;
  CheckDBActive;

  Result := sqlite3_column_bytes16(FQuery, AIndex);
  if Assigned(AData) then
  begin
    ABlobBuffer := sqlite3_column_blob(FQuery, AIndex);
    if Assigned(ABlobBuffer) then
    begin
      AData.Size := Result;
      Move(ABlobBuffer^, AData.Memory^, AData.Size);
    end
    else
      AData.Size := 0;
  end;
end;

function TSQLTable.ReadBlob(const AName: String; AData: TMemoryStream): Integer;
begin
  Result := ReadBlob(GetFieldIndex(AName), AData);
end;

function TSQLTable.ReadDouble(const AIndex: Integer): Double;
begin
  CheckActive;
  CheckDBActive;

  Result := sqlite3_column_double(FQuery, AIndex);
end;

function TSQLTable.ReadDouble(const AName: String): Double;
begin
  Result := ReadDouble(GetFieldIndex(AName));
end;

function TSQLTable.ReadInt(const AIndex: Integer): Integer;
begin
  CheckActive;
  CheckDBActive;

  Result := sqlite3_column_int(FQuery, AIndex);
end;

function TSQLTable.ReadInt(const AName: String): Integer;
begin
  Result := ReadInt(GetFieldIndex(AName));
end;

function TSQLTable.ReadInt64(const AIndex: Integer): Int64;
begin
  CheckActive;
  CheckDBActive;

  Result := sqlite3_column_int64(FQuery, AIndex);
end;

function TSQLTable.ReadInt64(const AName: String): Int64;
begin
  Result := ReadInt64(GetFieldIndex(AName));
end;

function TSQLTable.ReadDateTime(const AIndex: Integer): TDateTime;
begin
  Result := sqlite_StrToDateTime(ReadStr(AIndex));
end;

function TSQLTable.ReadDateTime(const AName: String): TDateTime;
begin
  Result := ReadDateTime(GetFieldIndex(AName));
end;

function TSQLTable.ReadStr(const AIndex: Integer): String;
var
  textResult : PChar;
begin
  Result := '';

  CheckActive;
  CheckDBActive;

  textResult := sqlite3_column_text16(FQuery, AIndex);
  if textResult <> nil then
    Result := String(textResult);
end;

function TSQLTable.ReadStr(const AName: String): String;
begin
  Result := ReadStr(GetFieldIndex(AName));
end;

procedure TSQLTable.SetActive(const Value: Boolean);
begin
  if Active <> Value then
  begin
    if Value then
      UpdateQuery
    else
      ClearQuery;
  end;
end;

procedure TSQLTable.UpdateQuery;
begin
  // TODO:
end;

function TSQLTable.GetActive: Boolean;
begin
  Result := Assigned(FQuery);
end;

function TSQLTable.GetColumn(const Index: Integer): TSQLColumn;
begin
  CheckActive;

  if not FDataTypesFetched then
    FDataTypesFetched := FetchDataTypes;

  Result := TSQLColumn(FColumns[Index]);
end;

function TSQLTable.GetColumnCount: Integer;
begin
  CheckActive;

  if not FDataTypesFetched then
    FDataTypesFetched := FetchDataTypes;

  Result := FColumns.Count;
end;

function TSQLTable.GetFieldIndex(const Name: String): Integer;
var
  I: Integer;
  Column: TSQLColumn;
begin
  CheckActive;

  if not FDataTypesFetched then
    FDataTypesFetched := FetchDataTypes;

  Result := -1;
  for I := 0 to FColumns.Count - 1 do
  begin
    Column := TSQLColumn(FColumns.List[I]);

    if SameText(Column.Name, Name) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TSQLTable.GetValue(const ColumnName: String): Variant;
var
  Idx: Integer;
  Column: TSQLColumn;
begin
  CheckActive;

  Idx := GetFieldIndex(ColumnName);

  if Idx >= 0 then
  begin
    Column := GetColumn(Idx);
    case Column.DataType of
      sctInteger:
        Result := ReadInt64(Idx);
      sctFloat:
        Result := ReadDouble(Idx);
      sctText:
        Result := ReadStr(Idx);
      sctBlob:
        Result := ReadStr(Idx);
      sctNull:
        Result := Null;
    else
      raise TDBException.CreateFmt('Unknown type %d for field "%s"', [Integer(Column.DataType), ColumnName]);
    end;
  end
  else
    raise TDBException.CreateFmt('Field "%s" not found', [ColumnName]);
end;

{ TAIMPSqlBase }

function busy(UserData: Pointer; P2: integer): integer; cdecl;
begin
  Sleep(100);
  Result := 0;
end;

constructor TSQLBase.Create(AConnectMgr: TSQLConnectMgr);
begin
  inherited Create;

  FJournalModeType := ttDefault;
  FSynchronousType := stDefault;
  FOnError := Nil;
  FConnectMgr := AConnectMgr;
  FLock := TRWLock.Create;
  FLinkedTables := TObjectList.Create;
  FTransactions := 0;
  FGlobalTransactionLock := TRWLock.Create;;
  FInGlobalTransaction := False;

  Open;

  DataBaseInitTables;
end;

function TSQLBase.CreateView(const AQuery: String): TSQLView;
begin
  Result := TSQLView.Create(Self, AQuery);
end;

function TSQLBase.DeleteTable(const ATableName: String): Boolean;
begin
  Result := ExecSQL('DROP TABLE IF EXIST ' + ATableName);
end;

destructor TSQLBase.Destroy;
begin
  Active := False;

  FLinkedTables.Clear;
  FreeAndNil(FLinkedTables);

  FConnectMgr.Remove(Self);
  FConnectMgr := Nil;

  FreeAndNil(FGlobalTransactionLock);
  FreeAndNil(FLock);

  inherited Destroy;
end;

procedure TSQLBase.DataBaseInitTables;
begin
end;

procedure TSQLBase.ErrorMsg(AText: PChar; const ErrorCode: Integer; const Query: String = '');
begin
  ErrorMsg(String(AText), ErrorCode, Query);
end;

procedure TSQLBase.AddLinkedTable(Table: TSQLTable);
begin
  FLock.Lock(nsWriter);
  try
    FLinkedTables.Add(Table);
  finally
    FLock.UnLock;
  end;
end;

procedure TSQLBase.BeginTransaction(const ATransactionType: TSQLBeginTransactionType = btDeferred);
var
  TypeStr: String;
begin
  //ExecSQL(Format('SAVEPOINT sp_%d;', [TInterlocked.Increment(FTransactions) - 1]));

  FGlobalTransactionLock.Lock(nsReader);

  // Запрещаем все действия по базе с других потоков
  if ATransactionType = btGlobal then
  begin
    if not FInGlobalTransaction then
    begin
      FGlobalTransactionLock.Lock(nsWriter);
      FInGlobalTransaction := True;
    end;
  end;

  Lock(nsReader);
  try
    // Тип транзакции btGlobal использовать для CancelTransaction
    if ATransactionType = btGlobal then
    begin
      // Ждем, когда другие потоки отвалятся
      while FTransactions > 0 do
        Sleep(10);
    end;

    if FTransactions = 0 then
      begin
        Lock;
        try
          if FTransactions = 0 then
            begin
              case ATransactionType of
                btDeferred:
                  TypeStr := 'DEFERRED';
                btImmediate:
                  TypeStr := 'IMMEDIATE';
                btExclusive, btGlobal:
                  TypeStr := 'EXCLUSIVE';
              else
                TypeStr := '';
              end;

              if ExecSQL(Format('BEGIN %s TRANSACTION;', [TypeStr])) then
                AtomicIncrement(FTransactions);
            end
          else
            AtomicIncrement(FTransactions);
        finally
          UnLock;
        end;
      end
    else
      AtomicIncrement(FTransactions);
  finally
    UnLock;
  end;
end;

procedure TSQLBase.EndTransaction;
begin
  //ExecSQL(Format('RELEASE SAVEPOINT sp_%d;', [TInterlocked.Decrement(FTransactions)]));

  Lock(nsReader);
  try
    if FTransactions = 1 then
      begin
        Lock(nsWriter);
        try
          if FTransactions = 1 then
            begin
              if ExecSQL('END TRANSACTION;') then
                AtomicExchange(FTransactions, 0);

              if FInGlobalTransaction then
              begin
                FInGlobalTransaction := False;
                FGlobalTransactionLock.UnLock;

                Exit;
              end;
            end
          else
            AtomicDecrement(FTransactions);
        finally
          UnLock;
        end;
      end
    else
      AtomicDecrement(FTransactions);
  finally
    UnLock;

    FGlobalTransactionLock.UnLock;
  end;
end;

procedure TSQLBase.CancelTransaction;
begin
  //ExecSQL(Format('ROLLBACK TO SAVEPOINT sp_%d;', [TInterlocked.Decrement(FTransactions)]));

  Lock;
  try
    {$IFDEF DEBUG}
    if not FInGlobalTransaction then
      ErrorMsg('TSQLBase.CancelTransaction: Only for Global transactions!', -1);
    {$ENDIF}

    if AtomicDecrement(FTransactions) = 0 then
    begin
      ExecSQL('ROLLBACK TRANSACTION;');

      FInGlobalTransaction := False;

      FGlobalTransactionLock.UnLock;
    end;
  finally
    UnLock;
  end;
end;

function TSQLBase.ClearAll: Boolean;
var
  AList: TStrings;
  I: Integer;
begin
  Result := PopulateTableNames(AList);
  if Result then
  try
    for I := 0 to AList.Count - 1 do
      Result := Result and ExecSQL('DROP TABLE IF EXISTS ' + AList[I]);

    DataBaseInitTables;
  finally
    FreeAndNil(AList);
  end;
end;

procedure TSQLBase.Close;
begin
  // Коммитим незакрытые транзакции. Все ROLLBACK должны отрабатываться локально в блоке except
  Lock;
  try
    if Active then
    begin
      if (FTransactions > 0) then
      begin
        // Ждем завершения всех транзакций
        while FTransactions > 0 do
        begin
          UnLock;
          Sleep(1);
          Lock;
        end;
      end;

      // Закрываем открытые запросы
      ClosePreparedStatements;

      // принудительный сброс журнала для WAL
      if JournalModeType = ttWAL then
        JournalModeType := ttDelete;

      // закрываем соединение
      CheckError(sqlite3_close(FDB));
    end;
  finally
    FDB := nil;
    UnLock;
  end;
end;

procedure TSQLBase.ClosePreparedStatements;
var
  I: Integer;
  Table: TSQLTable;
  stmt: sqlite3_stmt;
begin
  CheckActive;

  Lock;
  try
    // Очистка прилинкованных Views
    if Assigned(FLinkedTables) then
    begin
      for I := 0 to FLinkedTables.Count - 1 do
      begin
        Table := TSQLTable(FLinkedTables[I]);
        Table.Active := False;
      end;
    end;

    // Очистка открытых запросов
    repeat
      stmt := sqlite3_next_stmt(FDB, nil);

      if Assigned(stmt) then
        CheckError(sqlite3_finalize(stmt), [SQLITE_OK, SQLITE_ABORT]);
    until (stmt = nil);
  finally
    UnLock;
  end;
end;

procedure TSQLBase.Compress;
begin
  // TODO: http://www.sqlite.org/lang_vacuum.html
  // ExecSQL('VACUUM');
end;

function TSQLBase.PopulateTableNames(out AList: TStrings): Boolean;
const
  SQLITE_GET_TABLES_QUERY = 'SELECT name FROM sqlite_master WHERE (type=''table'') ORDER BY name;';
var
  ATable: TSQLTable;
begin
  Result := ExecSQL(SQLITE_GET_TABLES_QUERY, ATable);
  if Result then
  try
    AList := TStringList.Create;
    repeat
      AList.Add(ATable.ReadStr(0));
    until not ATable.NextRecord;
  finally
    FreeAndNil(ATable);
  end;
end;

procedure TSQLBase.CheckActive;
begin
  if not Active then
    raise TDBException.Create('Active = false');
end;

function TSQLBase.CheckError(const ErrorCode: Integer; const ValidCodes: TSQLCodes = [SQLITE_OK]; const Query: String = ''): Boolean;
begin
  Result := (ErrorCode in ValidCodes);
  if not Result then
    ErrorMsg(sqlite3_errmsg16(FDB), ErrorCode, Query);
end;

procedure TSQLBase.ErrorMsg(const AText: String; const ErrorCode: Integer; const Query: String = '');
begin
  if Assigned(FOnError) then
    FOnError(ErrorCode, AText, Query);
end;

function TSQLBase.ExecInsertBlob(const AQuery: String; AData: TMemoryStream): Boolean;
var
  AQueryHandle: HSQLQUERY;
  ResCode: Integer;
begin
  GlobalLock;
  try
    Result := PrepareQuery(AQuery, AQueryHandle);
    if Result then
    try
      Result := CheckError(sqlite3_bind_blob(AQueryHandle, 1, AData.Memory, AData.Size, Pointer(SQLITE_STATIC)));
      if Result then
      begin
        ResCode := DoExecStep(AQueryHandle);
        Result := CheckError(ResCode, [SQLITE_ROW, SQLITE_DONE]) and (ResCode = SQLITE_ROW);
      end;
    finally
      CheckError(sqlite3_finalize(AQueryHandle));
    end;
  finally
    GlobalUnLock;
  end;
end;

function TSQLBase.ExecSQL(const AQuery: String; out ATable: TSQLTable): Boolean;
var
  AQueryHandle: HSQLQUERY;
  ResCode: Integer;
begin
  ATable := nil;

  CheckActive;

  AQueryHandle := nil;

  GlobalLock;
  try
    Result := PrepareQuery(AQuery, AQueryHandle);
    if Result then
      begin
        ResCode := DoExecStep(AQueryHandle);

        Result := CheckError(ResCode, [SQLITE_ROW, SQLITE_DONE], AQuery) and (ResCode = SQLITE_ROW);
        if Result then
          ATable := TSQLTable.Create(Self, AQueryHandle)
        else
          CheckError(sqlite3_finalize(AQueryHandle));
      end;
  finally
    GlobalUnLock;
  end;
end;

function TSQLBase.ExecSQL(const AQuery: String): Boolean;
var
  AQueryHandle: HSQLQUERY;
begin
  CheckActive;

  GlobalLock;
  try
    Result := PrepareQuery(AQuery, AQueryHandle);
    if Result then
      try
        Result := CheckError(DoExecStep(AQueryHandle), [SQLITE_ROW, SQLITE_DONE], AQuery);
      finally
        CheckError(sqlite3_finalize(AQueryHandle));
      end;
  finally
    GlobalUnLock;
  end;
end;

function TSQLBase.PrepareQuery(const AQueryStr: String; out AQuery: HSQLQUERY): Boolean;
var
  QueryBufStr: PChar;
  Count: Integer;
  ANext: PWideChar;
  prepareResult : integer;
begin
  Result := False;
  AQuery := nil;
  if AQueryStr = '' then
    Exit;

  QueryBufStr := sqlite_Str(AQueryStr, Count);
  try
    ANext := nil;

    prepareResult := sqlite3_prepare16_v2(FDB, QueryBufStr, Count, AQuery, ANext);

    Result := CheckError(prepareResult, [SQLITE_OK], AQueryStr);
  finally
    FreeMemory(QueryBufStr);
  end;
end;

function TSQLBase.DoExecStep(AQueryHandle: HSQLQUERY): Integer;
begin
  GlobalLock;
  try
    Result := sqlite3_step(AQueryHandle);
  finally
    GlobalUnLock;
  end;
end;

procedure TSQLBase.RemoveLinkedTable(Table: TSQLTable);
begin
  Lock;
  try
    FLinkedTables.OwnsObjects := False;
    FLinkedTables.Remove(Table);
    FLinkedTables.OwnsObjects := True;
  finally
    UnLock;
  end;
end;

procedure TSQLBase.SetActive(const Value: Boolean);
begin
  if Active <> Value then
  begin
    if Value then
      Open
    else
      Close;
  end;
end;

procedure TSQLBase.SetJournalModeType(const Value: TSQLJournalModeType);
const
  _JOURNAL_MODE_VALUES: array[TSQLJournalModeType] of string =
    ('DELETE', 'DELETE', 'TRUNCATE', 'PERSIST', 'MEMORY', 'WAL', 'OFF');

  _QUERY = 'PRAGMA journal_mode = %s;';
var
  Query: string;
begin
  if FJournalModeType <> Value then
  begin
    // http://www.sqlite.org/pragma.html#pragma_journal_mode
    Query := Format(_QUERY, [_JOURNAL_MODE_VALUES[Value]]);

    if ExecSQL(Query) then
    begin
      FJournalModeType := Value;

      case FJournalModeType of
        ttDelete:
          SynchronousType := stFull;
        ttMemory:
          SynchronousType := stFull;
        ttWAL:
          begin
            // http://www.sqlite.org/wal.html
            // Для WAL рекомендуется режим синхронизации NORMAL
            SynchronousType := stNormal;
          end;
      end;
    end;
  end;
end;

procedure TSQLBase.SetSynchronousType(const Value: TSQLSynchronousType);
const
  // PRAGMA synchronous = 0 | OFF | 1 | NORMAL | 2 | FULL;
  _SYNC_TYPE_VALUES: Array[TSQLSynchronousType] of string =
    ('FULL', 'OFF', 'NORMAL', 'FULL');
  _QUERY = 'PRAGMA synchronous = %s;';
var
  Query: string;
begin
  if FSynchronousType <> Value then
  begin
    // http://www.sqlite.org/pragma.html#pragma_synchronous
    Query := Format(_QUERY, [_SYNC_TYPE_VALUES[Value]]);

    if ExecSQL(Query) then
      FSynchronousType := Value;
  end;
end;

function TSQLBase.GetFileName: String;
begin
  Result := FConnectMgr.DBFileName;
end;

function TSQLBase.GetLastInsertRowID: Int64;
begin
  GlobalLock;
  try
    Result := sqlite3_last_insert_rowid(FDB);
  finally
    GlobalUnLock;
  end;
end;

function TSQLBase.GetActive: Boolean;
begin
  Result := Assigned(FDB);
end;

function TSQLBase.GetColCount(const ATableName: String): Integer;
var
  ATable: TSQLTable;
begin
  Result := 0;
  try
    if ExecSQL('SELECT * FROM ' + ATableName + ' LIMIT 1;', ATable) then
      try
        Result := ATable.ColumnCount;
      finally
        FreeAndNil(ATable);
      end
    else
      if ExecSQL('SELECT * FROM PRAGMA table_info(' + ATableName + ');', ATable) then
        try
          GlobalLock;
          try
            repeat
              if ATable.GetColumn(Result).FName <> '' then
              Inc(Result);
            until Not ATable.NextRecord;
          finally
            GlobalUnLock;
          end;
        finally
          FreeAndNil(ATable);
        end
      else
        Result := -1;
  except
    Result := -1;
  end;
end;

Function TSQLBase.IsEmpty(Const ATableName: String): Boolean;
var
  ATable: TSQLTable;
begin
  if ExecSQL('SELECT * FROM ' + ATableName + ' LIMIT 1;', ATable) then
     try
       Result := False;
     finally
       FreeAndNil(ATable);
     end
  else
    Result := True;
end;

function TSQLBase.GetRowAffected: Integer;
begin
  GlobalLock;
  try
    Result := sqlite3_changes(FDB);
  finally
    GlobalUnLock;
  end;
end;

function TSQLBase.GetVersion: Integer;
begin
  Result := sqlite3_libversion_number;
end;

procedure TSQLBase.GlobalLock;
begin
  Lock(nsReader);
end;

procedure TSQLBase.GlobalUnLock;
begin
  UnLock;
end;

Procedure TSQLBase.Lock(const LockType: TRWNodeState = nsWriter);
Begin
  FLock.Lock(LockType);
End;

procedure TSQLBase.Open;
begin
  Lock;
  try
    if not Active then
    begin
      if not CheckError(sqlite3_open16(PChar(FileName), FDB)) then
      begin
        raise TDBException.CreateFmt('Fail open DB: %s', [FileName]);
      end;

      CheckError(sqlite3_create_collation16(FDB, 'UNICODE', SQLITE_UTF16, nil, @UnicodeCompare));
      // SYNCWIN-1382
      CheckError(sqlite3_create_function16(FDB, 'upper', 1, SQLITE_UTF16, nil, @SQLiteUpper, nil, nil));
      //CheckError(sqlite3_create_function16(FDB, 'HasKey', 2, SQLITE_UTF16, nil, @HasKeyFunc, nil, nil));

      // TODO: http://www.sqlite.org/sharedcache.html
      //CheckError(sqlite3_enable_shared_cache(1));
      //ExecSQL('PRAGMA read_uncommitted = True;');

      CheckError(sqlite3_busy_handler(FDB, @busy, nil));
      CheckError(sqlite3_busy_timeout(FDB, 2000));
    end;
  finally
    UnLock;
  end;
end;

Procedure TSQLBase.UnLock;
Begin
  FLock.UnLock;
End;

procedure TSQLBase.UpdatePreparedStatements;
var
  I: Integer;
  Table: TSQLTable;
begin
  CheckActive;

  Lock;
  try
    for I := 0 to FLinkedTables.Count - 1 do
    begin
      Table := TSQLTable(FLinkedTables[I]);
      Table.Active := False;
      Table.Active := True;
    end;
  finally
    UnLock;
  end;
end;

{ TAIMPSqlConnectMgr }

constructor TSQLConnectMgr.Create(const ADBFileName: String);
begin
  inherited Create;

  FDBFileName := ADBFileName;
  FLock := TRWLock.Create;
  FDBConnections := TList.Create;
  FActive := True;
end;

destructor TSQLConnectMgr.Destroy;
begin
  Reset;

  FreeAndNil(FDBConnections);
  FreeAndNil(FLock);

  inherited Destroy;
end;

function TSQLConnectMgr.GetNewConnection: TSQLBase;
begin
  Result := TSQLBase.Create(Self);
  FDBConnections.Add(Result);

  Result.Active := Active;
end;

function TSQLConnectMgr.GetDefaultConnection: TSQLBase;
begin
  FLock.Lock(nsReader);
  try
    if FDBConnections.Count > 0 then
      Result := TSQLBase(FDBConnections[0])
    else
      begin
        FLock.Lock(nsWriter);
        try
          if FDBConnections.Count = 0 then
            Result := GetNewConnection
          else
            Result := GetDefaultconnection;
        finally
          FLock.UnLock;
        end;
      end;
  finally
    FLock.UnLock;
  end;
end;

procedure TSQLConnectMgr.Remove(Connect: TSQLBase);
begin
  FLock.Lock(nsWriter);
  try
    FDBConnections.Remove(Connect);
  finally
    FLock.UnLock;
  end;
end;

procedure TSQLConnectMgr.Reset(const FreeConnections: Boolean = True);
var
  DBConnection: TSQLBase;
begin
  FLock.Lock(nsWriter);
  try
    Active := False;

    if FreeConnections then
    begin
      while FDBConnections.Count > 0 do
      begin
        DBConnection := TSQLBase(FDBConnections[0]);

        FreeAndNil(DBConnection); // При удалении должно удалить себя из списка
      end;
    end;
  finally
    FLock.UnLock;
  end;
end;

procedure TSQLConnectMgr.SetActive(const Value: Boolean);
var
  DBConnection: TSQLBase;
  I: Integer;
begin
  if FActive <> Value then
  begin
    FLock.Lock(nsWriter);
    try
      for I := 0 to FDBConnections.Count - 1 do
      begin
        DBConnection := TSQLBase(FDBConnections[I]);
        DBConnection.Active := Value;
      end;
    finally
      FLock.UnLock;
    end;

    FActive := Value;
  end;
end;

procedure TSQLConnectMgr.SetDBFileName(const Value: String);
var
  OldActive: Boolean;
begin
  FLock.Lock(nsWriter);
  try
    if not AnsiSameText(FDBFileName, Value) then
    begin
      OldActive := Active;
      Active := False;

      FDBFileName := Value;

      Active := OldActive;
    end;
  finally
    FLock.UnLock;
  end;
end;

function _Reset: Boolean;
begin
  Result := True;

  DBResetAll;

  FreeAndNil(_DBConnectMgrList);
  FreeAndNil(_DBConnectMgrLock);
end;

procedure _Init;
begin
  _DBConnectMgrList := TDBConnectMgrList.Create;
  _DBConnectMgrList.Duplicates := dupError;

  _DBConnectMgrLock := TRWLock.Create;

  _sqlite_DateTimeFormat := TFormatSettings.Create;
  _sqlite_DateTimeFormat.DateSeparator := '-';
  _sqlite_DateTimeFormat.TimeSeparator := ':';
  _sqlite_DateTimeFormat.ShortDateFormat := SQLITE_DATE_FMT;
  _sqlite_DateTimeFormat.ShortTimeFormat := SQLITE_TIME_FMT;
end;

{ TAIMPSqlView }

constructor TSQLView.Create(ADataBase: TSQLBase; AQuery: HSQLQUERY; const AQueryStr: String = '');
begin
  inherited Create(ADataBase, AQuery);

  FLock := TRWLock.Create;
  FQueryStr := AQueryStr;

  InitParams;
end;

procedure TSQLView.SetParam(const ParamName: String; const Value: Variant);
var
  ParamIdx: Integer;
begin
  CheckActive;
  CheckDBActive;

  ParamIdx := sqlite3_bind_parameter_index(FQuery, PAnsiChar(AnsiString(ParamName)));

  if ParamIdx > 0 then
    SetParam(ParamIdx, Value)
  else
    raise TDBException.CreateFmt('Parameter "%s" not found in view', [ParamName]);
end;

procedure TSQLView.BeginExecute;
begin
  CheckActive;

  Lock.Lock(nsWriter);
  FDataBase.GlobalLock;
  try
    CheckDBActive;
    Reset;
  except
    on E: Exception do
    begin
      FDataBase.GlobalUnLock;
      Lock.UnLock;
      raise;
    end;
  end;
end;

constructor TSQLView.Create(ADataBase: TSQLBase; const AQueryStr: String);
var
  AQuery: HSQLQUERY;
begin
  ADataBase.CheckActive;

  ADataBase.GlobalLock;
  try
    if ADataBase.PrepareQuery(AQueryStr, AQuery) then
      Create(ADataBase, AQuery, AQueryStr)
    else
      raise TDBException.CreateFmt('Fail create view: "%s"', [AQueryStr]);
  finally
    ADataBase.GlobalUnLock;
  end;
end;

destructor TSQLView.Destroy;
begin
  FreeAndNil(FLock);

  inherited;
end;

procedure TSQLView.EndExecute;
begin
  try
    Reset;
  finally
    FDataBase.GlobalUnLock;
    Lock.UnLock;
  end;
end;

function TSQLView.Execute: Boolean;
begin
  Result := NextRecord;
end;

procedure TSQLView.InitParams;
begin
  CheckActive;
  CheckDBActive;

  FParamCount := sqlite3_bind_parameter_count(FQuery);
end;

procedure TSQLView.Reset;
begin
  CheckActive;
  CheckDBActive;

  FDataBase.GlobalLock;
  try
    FDataBase.CheckError(sqlite3_reset(FQuery));
    FDataBase.CheckError(sqlite3_clear_bindings(FQuery));
  finally
    FDataBase.GlobalUnLock;
  end;
end;

procedure TSQLView.SetParam(const ParamIdx: Integer; const Value: Variant);
begin
  CheckActive;
  CheckDBActive;

  if (ParamIdx > 0) and (ParamIdx <= FParamCount) then
  begin
    case VarType(Value) of
      varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64, varUInt64:
        FDataBase.CheckError(sqlite3_bind_int64(FQuery, ParamIdx, Int64(Value)), [SQLITE_OK], FQueryStr);
      varSingle, varDouble, varCurrency:
        FDataBase.CheckError(sqlite3_bind_double(FQuery, ParamIdx, Double(Value)), [SQLITE_OK], FQueryStr);
      varDate:
        SetParamAsText(ParamIdx, sqlite_DateTimeToStr(TDateTime(Value)));
      varBoolean:
        SetParamAsText(ParamIdx, BoolToStr(Boolean(Value)));
      varString, varUString:
        SetParamAsText(ParamIdx, String(Value));
    else
      begin
        if VarIsNull(Value) then
          FDataBase.CheckError(sqlite3_bind_null(FQuery, ParamIdx))
        else
          raise TDBException.CreateFmt('Invalid value type for parameter "%d"', [ParamIdx]);
      end;
    end;
  end
  else
    raise TDBException.CreateFmt('Parameter "%d" not found for view', [ParamIdx]);
end;

procedure TSQLView.SetParamAsText(const ParamIdx: Integer; const Str: String);
var
  StrBuf: PWideChar;
  Count: Integer;
begin
  CheckActive;
  CheckDBActive;

  StrBuf := sqlite_Str(Str, Count);
  try
    FDataBase.CheckError(
      sqlite3_bind_text16(FQuery, ParamIdx, StrBuf, Count, TBindDestructor(SQLITE_TRANSIENT)),
      [SQLITE_OK],
      FQueryStr
    );
  finally
    FreeMemory(StrBuf);
  end;
end;

procedure TSQLView.UpdateQuery;
begin
  CheckDBActive;

  FDataBase.GlobalLock;
  try
    // Проверяем, чтобы предыдущий запрос был закрыт
    ClearQuery;

    if FDataBase.PrepareQuery(FQueryStr, FQuery) then
      InitParams
    else
      raise TDBException.CreateFmt('Fail update view: "%s"', [FQueryStr]);
  finally
    FDataBase.GlobalUnLock;
  end;
end;

initialization
  _Init;

finalization
  _Reset;
end.

