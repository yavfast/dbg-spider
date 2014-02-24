unit uMacroParser;

interface

uses
  Classes, SysUtils, StrUtils;

type
  TMacroParser = class;
  TMacroItem = class;

  TMacroParams = class(TList)
  private
    function GetParam(const Index: Integer): TMacroItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    property Params[const Index: Integer]: TMacroItem read GetParam; default;
  end;

  TMacroParserEvent = procedure(const FunName: String; Params: TMacroParams; var FunResult: String; var Done: Boolean) of object;

  // Для разбора и вычисления элемента макроса
  TMacroItem = class
  private
    FMacroParser: TMacroParser;
    FMacroStr: String;

    FFunName: String; // Название функции
    FConst: String;
    FParams: TMacroParams;

    FErrPos: Integer;
    FErrText: String;

    procedure ClearParams;
    function GetFunName(const Str: String): String;
    function GetParamsStr(const Str: String): String;
    function GetConst(const Str: String): String;
  protected
    procedure Execute(const FunName: String; Params: TMacroParams; var FunResult: String);

    function BaseFun(const FunName: String; Params: TMacroParams; var FunResult: String): Boolean; virtual;

    procedure RaiseException(const Msg: String); overload;
    procedure RaiseException(const Msg: String; const Args: array of const); overload;
  public
    constructor Create(AOwner: TMacroParser);
    destructor Destroy; override;

    // Разбор строки
    function Parse(const Str: String): Boolean;

    // Вычисление значения макроса
    function Calc: String;

    property ErrPos: Integer read FErrPos;
    property ErrText: String read FErrText;
  end;

  // Для разбора и вычисления макроса
  TMacroParser = class
  private
    FMacroStr: String;
    FRootItem: TMacroItem;
    FOnCalcFun: TMacroParserEvent;

    procedure SetMacroStr(const Value: String);
    function GetErrText: String;
  protected
    function Parse(const Str: String = ''): Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    // Проверка синтаксиса
    function Check(const Macro: String = ''): Boolean;

    // Вычисление значения выражения
    function Calc(const Macro: String = ''): String;

    property ErrText: String read GetErrText;
  published
    property MacroStr: String read FMacroStr write SetMacroStr;
    property OnCalcFun: TMacroParserEvent read FOnCalcFun write FOnCalcFun;
  end;

  EMacroParser = class(Exception);

implementation

const
  ERR_PARAM_COUNT = 'Недопустимое количество параметров';
  RES_EMPTY = '{!EMPTY!}';
  ERR_CALC_FUN = 'Unknown function: %s';
  ERR_FUN_NAME = 'Недопустимое название функции';
  ERR_PARAM_STR = 'Недопустимый список параметров';
  ERR_BRACKET = 'Несоответствие количества открывающих и закрывающих скобок';

function _StrToBool(const Str: String): Boolean;
begin
  Result := (Str = '1') or ((Str <> '') and (Str[1] = 'T'));
end;

function _IF(Params: TMacroParams; var FunResult: String): Boolean;
var
  Condition: String;
begin
  Result := False;

  if Params.Count in [2, 3] then
  begin
    Condition := Params[0].Calc;

    if _StrToBool(Condition) then
      FunResult := Params[1].Calc
    else
      if Params.Count = 3 then
        FunResult := Params[2].Calc;

    Result := True;
  end;
end;

{ TMacroItem }

procedure TMacroItem.Execute(const FunName: String; Params: TMacroParams; var FunResult: String);
var
  Done: Boolean;
begin
  FunResult := '';

  if not BaseFun(FunName, Params, FunResult) then
  begin
    Done := False;

    if Assigned(FMacroParser.OnCalcFun) then
      FMacroParser.OnCalcFun(FunName, Params, FunResult, Done);

    if not Done then
      RaiseException(ERR_CALC_FUN, [FunName]);
  end;
end;

function TMacroItem.BaseFun(const FunName: String; Params: TMacroParams; var FunResult: String): Boolean;
begin
  Result := False;

  // @(Fun1; Fun2; ...)
  if FunName = '@' then
    Result := True
  else
  if FunName = 'IF' then
    Result := _IF(Params, FunResult)
end;

function TMacroItem.Calc: String;
begin
  Result := '';

  if FMacroStr <> '' then
    if not Parse(FMacroStr) then
      Exit;

  if FFunName = '' then
    Result := FConst
  else
    Execute(FFunName, FParams, Result);
end;

procedure TMacroItem.ClearParams;
begin
  FFunName := '';
  FConst := '';

  FParams.Clear;
end;

constructor TMacroItem.Create(AOwner: TMacroParser);
begin
  inherited Create;

  FMacroParser := AOwner;

  FFunName := '';
  FConst := '';

  FParams := TMacroParams.Create;

  FErrPos := 0;
  FErrText := '';
end;

destructor TMacroItem.Destroy;
begin
  ClearParams;
  FreeAndNil(FParams);

  inherited;
end;

function TMacroItem.GetConst(const Str: String): String;
begin
  if (Str[1] in ['"', '''']) and (Str[Length(Str)] in ['"', '''']) then
    Result := Copy(Str, 2, Length(Str) - 2)
  else
    Result := Str;
end;

function TMacroItem.GetFunName(const Str: String): String;
var
  i: Integer;
begin
  Result := '';

  if (Str <> '') and not(Str[1] in ['"', '''']) then
  begin
    i := Pos('(', Str);
    if i > 0 then
    begin
      if i > 1 then
      begin
        // Проверка на первый символ
        if Str[1] in ['0' .. '9'] then
          RaiseException(ERR_FUN_NAME);

        // Проверка на правильное имя функции
        i := 1;
        while (Str[i] <> '(') do
        begin
          if not(Str[i] in ['a'..'z', 'A' .. 'Z', '0' .. '9', '_', '@']) then
            RaiseException(ERR_FUN_NAME);

          Inc(i);
        end;

        // Получение имени функции
        if Str[i] = '(' then
          Result := AnsiUpperCase(Copy(Str, 1, i - 1));
      end
      else
        RaiseException(ERR_FUN_NAME);
    end;
  end;
end;

function TMacroItem.GetParamsStr(const Str: String): String;
var
  i, j: Integer;
begin
  Result := '';

  // Получение первой скобки
  i := Pos('(', Str);

  // Проверка на последнюю скобку
  j := Length(Str);
  if Str[j] <> ')' then
    raise Exception.Create(ERR_PARAM_STR);

  // Получение строки параметров
  Result := Copy(Str, i + 1, j - i - 1);
end;

function TMacroItem.Parse(const Str: String): Boolean;
var
  ParamsStr: String;
  _Cur: Integer;

  // Определение текущего параметра
  function GetParam(var ResParam: String): Boolean;
  var
    _OldCur: Integer;
    Level: Integer;
  begin
    Result := True;
    ResParam := '';

    _OldCur := _Cur;
    Level := 0;

    while (_Cur <= Length(ParamsStr)) and ((ParamsStr[_Cur] <> ';') or (Level > 0)) do
    begin
      case ParamsStr[_Cur] of
        '"':
          _Cur := PosEx('"', ParamsStr, _Cur + 1);
        '''':
          _Cur := PosEx('''', ParamsStr, _Cur + 1);
        '(':
          Inc(Level);
        ')':
          Dec(Level);
      end;

      Inc(_Cur);
    end;

    if Level = 0 then
    begin
      if _OldCur <> _Cur then
      begin
        ResParam := Copy(ParamsStr, _OldCur, _Cur - _OldCur);
        Inc(_Cur);
      end
      else
        Result := False;
    end
    else
    begin
      // Неправильная скобка
      RaiseException(ERR_BRACKET);
    end;
  end;

var
  _CurParam: String;
  _ChildItem: TMacroItem;
begin
  Result := False;

  ClearParams;

  // Удаление ведущих и концевых пробелов
  FMacroStr := Trim(Str);

  // Название функции
  FFunName := GetFunName(FMacroStr);
  if FFunName <> '' then
  begin
    // Разбор параметров функции
    ParamsStr := GetParamsStr(FMacroStr);

    _Cur := 1;
    try
      while Result and GetParam(_CurParam) do
      begin
        _ChildItem := TMacroItem.Create(FMacroParser);
        FParams.Add(_ChildItem);

        Result := _ChildItem.Parse(_CurParam);
        if not Result then
        begin
          // Формирование ошибки
          FErrPos := Pos('(', FMacroStr) + _ChildItem.ErrPos;
          FErrText := _ChildItem.ErrText;
        end;
      end;
    except
      on E: Exception do
      begin
        // Формирование ошибки
        FErrPos := Pos('(', FMacroStr) + _Cur;
        FErrText := E.Message;
      end;
    end;

    if not Result then
      ClearParams;
  end
  else
  begin
    // Константа
    FConst := GetConst(FMacroStr);
    Result := True;
  end;
end;

procedure TMacroItem.RaiseException(const Msg: String; const Args: array of const);
begin
  raise EMacroParser.CreateFmt(Msg, Args);
end;

procedure TMacroItem.RaiseException(const Msg: String);
begin
  raise EMacroParser.Create(Msg);
end;

{ TMacroParser }

function TMacroParser.Calc(const Macro: String): String;
begin
  Result := '';

  if Parse(Trim(Macro)) then
    Result := FRootItem.Calc;
end;

function TMacroParser.Check(const Macro: String): Boolean;
begin
  Result := Parse(Macro);
end;

constructor TMacroParser.Create;
begin
  inherited Create;

  FRootItem := TMacroItem.Create(Self);
end;

destructor TMacroParser.Destroy;
begin
  FreeAndNil(FRootItem);

  inherited;
end;

function TMacroParser.GetErrText: String;
begin
  Result := '';

  if Assigned(FRootItem) and (FRootItem.ErrPos > 0) then
  begin
    Result := Format('Syntax error [%d]: %s', [FRootItem.ErrPos, FRootItem.ErrText]);
  end;
end;

function TMacroParser.Parse(const Str: String): Boolean;
begin
  Result := FRootItem.Parse(Str);
end;

procedure TMacroParser.SetMacroStr(const Value: String);
begin
  FMacroStr := Value;
  Parse(FMacroStr);
end;

{ TMacroParams }

procedure TMacroParams.Clear;
var
  i: Integer;
  Obj: TObject;
begin
  for i := 0 to Count - 1 do
  begin
    Obj := Items[i];
    Items[i] := Nil;
    FreeAndNil(Obj);
  end;

  inherited Clear;
end;

constructor TMacroParams.Create;
begin
  inherited Create;
end;

destructor TMacroParams.Destroy;
begin

  inherited;
end;

function TMacroParams.GetParam(const Index: Integer): TMacroItem;
begin
  Result := TMacroItem(Items[Index]);
end;

end.
