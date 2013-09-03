Unit DebugInfo;

Interface

Uses
    SysUtils, Windows, Classes, Debuger, DebugerTypes, Generics.Collections;
{..............................................................................}

Type
  TFindResult = (slNotFound, slFoundExact, slFoundNotExact, slFoundWithoutLine);

{..............................................................................}
Type
    TSegmentCodeInfo = Class;
    TUnitInfo = Class;
    TFuncInfo = Class;
    TTypeInfo = Class;

{...............................................................................}
    TLineInfo = Class
    public
        LineNo  : Integer;
        Address : Pointer;
    End;
{..............................................................................}

{...............................................................................}
  TTypeKind = (tkBoolean, tkWordBool, tkLongBool, tkShortInt,
    tkSmallInt, tkInteger, tkInt64, tkByte, tkWord, tkCardinal, tkUInt64,
    tkSingle, tkReal48, tkReal, tkExtended, tkCurrency, tkComplex, tkPString,
    tkLString, tkWString, tkChar, tkPointer, tkSubRange, tkArray, tkEnum,
    tkStructure, tkClass, tkSet, tkVariant, tkProperty, tkFieldList, tkClosure,
    tkClassRef, tkWideChar, tkProcedure, tkArgList, tkMFunction, tkVoid,
    tkObject, tkDynamicArray);
{..............................................................................}

    TNameId = type Integer;

    TNameInfo = Class(TObject)
    public
        NameId : Integer;
        SymbolInfo : TObject; // ��������� �� TJclTD32SymbolInfo

        function Name: AnsiString; virtual; abstract;
        function ShortName: String; virtual; abstract;
    End;

    TNameIdList = TDictionary<TNameId,TNameInfo>;

    TNameList = Class(TList)
    private
        FNameIdList: TNameIdList;
        FFreeItems: Boolean;
    function GetNameInfoItem(const Index: Integer): TNameInfo;
    protected
        procedure Notify(Ptr: Pointer; Action: TListNotification); override;
        procedure CheckNameIdList;
    public
        constructor Create;
        destructor Destroy; override;

        procedure Clear; override;

        function FindByName(Const Name: AnsiString): TNameInfo;
        function FindByNameId(Const NameId: TNameId): TNameInfo;

        property NameInfoItems[const Index: Integer]: TNameInfo read GetNameInfoItem; default;
        property FreeItems: Boolean read FFreeItems write FFreeItems;
    End;

{..............................................................................}
    TTypeInfo = Class(TNameInfo)
    public
        Kind   : TTypeKind;
        BaseType : TTypeInfo;
        DataSize : Integer;
        MinValue : Integer;
        MaxValue : Integer;
        IndexType : TTypeInfo;

        Members  : TNameList;
        Elements : TNameList;

        UnitInfo: TUnitInfo;
        TypeInfoIdx: Integer; // Index in UnitInfo.Types

        Constructor Create;
        Destructor Destroy; Override;

        function Name : AnsiString; override;
        function ShortName: String; override;

        function KindAsString : String;
        function TypeOf: String;
        function ElementsToString: String;
    end;
{..............................................................................}

    TEnumInfo = Class(TNameInfo)
    public
      TypeInfo: TTypeInfo;
      OrderValue: Integer;

      function Name : AnsiString; override;
      function ShortName: String; override;
    End;


{..............................................................................}
    TConstInfo = Class(TNameInfo)
    public
        TypeInfo : TTypeInfo;
        OwnerInfo: TSegmentCodeInfo;

        Value : Variant;

        function Name: AnsiString; override;
        function ShortName: String; override;

        function UnitInfo: TUnitInfo;
        function ValueAsString: String;
    End;
{..............................................................................}

{..............................................................................}
    TRegInfo = Class
    public
        StartOffset   : Cardinal;
        EndOffset     : Cardinal;
        RegisterIndex : Integer;
    End;
{..............................................................................}

{..............................................................................}
    TVarKind = (vkGlobal, vkStack, vkRegister, vkLink);
{..............................................................................}

{..............................................................................}
    TVarInfo = Class(TNameInfo)
    public
        DataType       : TTypeInfo;
        Owner          : TSegmentCodeInfo;
        VarKind        : TVarKind;
        //IsPointer      : Boolean;
        //ByRef          : Boolean;
        Offset         : Integer;
        RegisterRanges : TList;

        Constructor Create;
        Destructor  Destroy; Override;

        function UnitInfo: TUnitInfo;

        function Name: AnsiString; override;
        function ShortName: String; override;

        function AsString: String;
    End;
{..............................................................................}

{..............................................................................}
    TMemberScope = (msPrivate, msProtected, msPublic);
{..............................................................................}

{..............................................................................}
    TStructMember = Class(TNameInfo)
    public
        DataType      : TTypeInfo;
        BitOffset     : Integer;
        BitLength     : Integer;
        Scope         : TMemberScope;
        AliasNameId   : TNameId;
        MethodNameId  : TNameId;
        Method        : TFuncInfo;  // read function for properties
        IsDefault     : Boolean;    // true for default property

        function Alias : AnsiString; // read field for properties
        function MethodName : AnsiString; // read function name for properties

        function Name: AnsiString; override;
        function ShortName: String; override;
    end;
{..............................................................................}

{..............................................................................}
    TUnitSegmentType = (ustData = $0000, ustCode = $0001);

    TUnitSegmentInfo = Class
    public
        Offset : Cardinal;
        Size   : Cardinal;
        SegType: TUnitSegmentType;
    End;
{..............................................................................}

{..............................................................................}
    TDebugInfo = Class;

    TSegmentCodeInfo = Class(TNameInfo)
    public
        Address     : Pointer;
        CodeSize    : Cardinal;

        Consts      : TNameList;
        Types       : TNameList;
        Vars        : TNameList;
        Funcs       : TNameList;

        Lines       : TList;

        Constructor Create;
        Destructor  Destroy; Override;

        Procedure   Clear; Virtual;

        function FindTypeByName(const TypeName: AnsiString): TTypeInfo;
        function FindFuncByName(const FuncName: AnsiString): TFuncInfo;
        function FindFuncByNameId(const FuncNameId: Integer): TFuncInfo;
        function FindConstByName(const ConstName: AnsiString): TConstInfo;
        function FindVarByName(const VarName: AnsiString): TVarInfo;
    End;

    TFuncInfo = Class(TSegmentCodeInfo)
        UnitInfo   : TUnitInfo;
        Params     : TNameList; // TODO: �������� ��������� �� Vars

        ResultType : TTypeInfo;

        Parent     : TFuncInfo;
        ID         : TObject;
        ParentID   : TObject;

        Constructor Create;
        Destructor  Destroy; Override;

        function Name: AnsiString; Override;
        function ShortName: String; override;

        function ParamsAsString: String;
    End;

    TUnitInfo = Class(TSegmentCodeInfo)
    public
        DebugInfo   : TDebugInfo;
        Segments    : TList;
        UsedUnits   : TStringList;

        CodeSize    : Cardinal;
        DataSize    : Cardinal;

        Constructor Create(ADebugInfo: TDebugInfo);
        Destructor  Destroy; Override;

        Procedure   Clear; Override;

        function Name : AnsiString; Override;
        function ShortName: String; Override;

        function FullUnitName: String;
    end;

    TDLLInfo = Class(TSegmentCodeInfo)

    End;
{...............................................................................}

{...............................................................................}
    TStackEntry = Class
    Public
        UnitInfo        : TUnitInfo;
        FuncInfo        : TFuncInfo;
        LineInfo        : TLineInfo;
        EIP             : Pointer;
        RET             : Pointer;
        EBP             : Pointer;

        Constructor Create;
        Function    GetInfo : String;
        Function    UpdateInfo(Const Addr: Pointer = nil) : TFindResult;
    End;
{..............................................................................}

    TDebugInfoProgressCallback = procedure(const Action: String; const Progress: Integer) of object;

    TDbgSourceDirs = TDictionary<String,String>;

    TDebugInfoClass = Class Of TDebugInfo;

    TDebugInfo = Class
    Private
        FDebuger   : TDebuger;
        FSourceDirs: String;
        FDirs      : TDbgSourceDirs;
        FUnits     : TStringList;
        FDbgLog    : TDbgLog;

        FExeFileName : String;
        FDebugInfoLoaded : Boolean;

        FDebugInfoProgressCallback: TDebugInfoProgressCallback;
    Protected
        FDebugInfoType : String;
        FUseShortNames: Boolean;

        procedure DoProgress(const Action: String; const Progress: Integer); virtual;
        Function DoReadDebugInfo(Const FileName : String; ALoadDebugInfo : Boolean) : Boolean; Virtual; abstract;

        function ParseUnitName(UnitInfo: TUnitInfo; const WithExt: Boolean = True): String; virtual;
        function ParseFuncName(FuncInfo: TFuncInfo): String; virtual;
        function ParseTypeName(TypeInfo: TTypeInfo): String; virtual;
        function ParseConstName(ConstInfo: TConstInfo): String; virtual;
        function ParseVarName(VarInfo: TVarInfo): String; virtual;
        function ParseStructMemberName(StructMember: TStructMember): String; virtual;
    Public
        Constructor Create(ADebuger: TDebuger);
        Destructor  Destroy; Override;

        Procedure ClearDebugInfo; Virtual;
        Function  HasDebugInfo(Const FileName : String) : Boolean; Virtual; abstract;
        Function  ReadDebugInfo(Const FileName : String; const SourceDirs : String = '') : Boolean; Virtual;
        Function  GetFileCount : Integer; Virtual;
        Function  GetFile(Index : Integer) : String; Virtual;
        Function  GetTypeInfo(Const TypeName : String) : TTypeInfo; Virtual;
        Function  GetAddrInfo(Var Addr: Pointer; Const FileName : String; Line : Cardinal) : TFindResult; Virtual; abstract;
        Procedure GetCallStackItems(const ThreadID: TThreadId; Const ExceptAddr, ExceptFrame: Pointer; StackItems : TList); Virtual;

        Function  GetLineInfo(Addr : Pointer; Var UnitInfo : TUnitInfo;
          Var FuncInfo : TFuncInfo; Var LineInfo : TLineInfo; GetPrevLine : Boolean) : TFindResult; Virtual; abstract;
        Function  GetLineInformation(Addr : Pointer; Var UnitName : String;
          Var FuncName : String; Var Line : LongInt; GetPrevLine : Boolean) : TFindResult; Virtual; abstract;

        procedure UpdateSourceDirs; virtual;
        procedure AddSourceDir(const Dir: String; const Recursive: Boolean = True); virtual;
        function FullUnitName(const UnitName: String): String;

        Function  MakeFuncDbgFullName(Const ClassName, MethodName : AnsiString) : AnsiString; Virtual; abstract;
        Function  MakeFuncShortName(Const MethodName : AnsiString) : AnsiString; Virtual; abstract;
        Function  MakeFuncNativeName(Const MethodName : AnsiString) : AnsiString; Virtual; abstract;

        Function FuncByName(const FuncName: AnsiString): TFuncInfo;

        Function  Evaluate(BriefMode : Boolean; Const Expression : String;
          Const TimeOut : Cardinal = INFINITE) : String; Virtual; abstract;

        Function  VarValueAsString(const Value: Variant): String; virtual; abstract;

        Procedure InitDebugHook; Virtual; abstract;

        Function GetNameById(const Idx: TNameId): AnsiString; virtual; abstract;

        Function  CheckAddr(Const Addr : Pointer) : Boolean; Virtual;
        Function  DumpLineInformation(Const Addr : Pointer) : String;
        Function  GetParamsStr(FuncInfo : TFuncInfo; Const EBP : Pointer; IsTopStack : Boolean) : String;

        Function  GetClassName(ObjectPtr: Pointer): String; Virtual; abstract;
        Function  GetExceptionName   (ExceptionRecord: PExceptionRecord) : String; Virtual;
        Function  GetExceptionMessage(ExceptionRecord: PExceptionRecord; const ThreadId: TThreadId) : String; Virtual;
        Function  GetExceptionAddress(ExceptionRecord: PExceptionRecord) : Pointer; Virtual;
        Function  GetExceptionFrame  (ExceptionRecord: PExceptionRecord) : Pointer; Virtual;
        Function  CheckDebugException(ExceptionRecord: PExceptionRecord; Var IsTraceException: Boolean): Boolean; Virtual;
        Function  CheckSystemFile    (Const FileName : String) : Boolean; Virtual; abstract;

        Function  IsSystemException(Const ExceptionCode: DWORD) : Boolean;

        Function  CheckDebugOutputMessage(DebugEvent : PDebugEvent): Boolean;
        Function  ProcessDebugOutputMessage(Const Msg : WideString; DebugEvent : PDebugEvent) : Boolean; Virtual;

        Function  IsValidAddr     (Const Addr: Pointer) : Boolean;
        Function  IsValidCodeAddr (Const Addr: Pointer) : Boolean;
        Function  IsValidStackAddr(Const Addr: Pointer; const ThreadID: TThreadId) : Boolean;
        Function  IsValidDataAddr (Const Addr: Pointer; const ThreadID: TThreadId) : Boolean;

        property Debuger: TDebuger read FDebuger;
        Property SourceDirs: String read FSourceDirs;
        Property Dirs: TDbgSourceDirs Read FDirs;
        Property Units: TStringList Read FUnits;

        Property DbgLog: TDbgLog read FDbgLog;

        property DebugInfoLoaded: Boolean read FDebugInfoLoaded;
        property DebugInfoType: String read FDebugInfoType;
        property DebugInfoProgressCallback: TDebugInfoProgressCallback read FDebugInfoProgressCallback write FDebugInfoProgressCallback;
        property UseShortNames: Boolean read FUseShortNames write FUseShortNames;
    End;
{...............................................................................}

var
  gvDebugInfo: TDebugInfo = nil;

{...............................................................................}
Implementation

Uses
    ClassUtils,
    //ApiConsts,
    //EvaluateProcs,
    //EvaluateTypes,
    Variants, IOUtils, Types;
{...............................................................................}

function IncPointer(Ptr: Pointer; Offset: Integer): Pointer; inline;
begin
  Result := Pointer(Integer(Ptr) + Offset);
end;


{ TDebugInfo }

{..............................................................................}
Constructor TDebugInfo.Create(ADebuger: TDebuger);
Begin
    Inherited Create;

    FDebuger := ADebuger;

    FDirs := TDbgSourceDirs.Create(4096);

    FUnits := TStringList.Create;

    FDbgLog := TDbgLog.Create;

    FExeFileName := '';
    FDebugInfoLoaded := False;
    FDebugInfoType := '';

    FDebugInfoProgressCallback := Nil;
    FUseShortNames := True;
End;
{..............................................................................}

{..............................................................................}
Destructor TDebugInfo.Destroy;
Begin
    ClearDebugInfo;

    FreeAndNil(FUnits);
    FreeAndNil(FDirs);

    FreeAndNil(FDbgLog);

    Inherited;
End;

procedure TDebugInfo.DoProgress(const Action: String; const Progress: Integer);
begin
  if Assigned(FDebugInfoProgressCallback) then
    FDebugInfoProgressCallback(Action, Progress);
end;
{..............................................................................}

{..............................................................................}
procedure TDebugInfo.AddSourceDir(const Dir: String; const Recursive: Boolean);
const
  _PAS_EXTS: array[0..2] of String = ('*.pas', '*.inc', '*.dpr');
var
  ChildDirs: TStringDynArray;
  Files: TStringDynArray;
  J, I: Integer;
  FileName: String;
  ShortFileName: String;
begin
  for J := 0 to High(_PAS_EXTS) do
  begin
    Files := TDirectory.GetFiles(Dir, _PAS_EXTS[J]);

    if Length(Files) > 0 then
      for I := 0 to High(Files) do
      begin
        FileName := Files[I];
        ShortFileName := AnsiLowerCase(ExtractFileName(FileName));

        FDirs.AddOrSetValue(ShortFileName, FileName);
      end;
  end;

  ChildDirs := TDirectory.GetDirectories(Dir);
  if Length(ChildDirs) > 0 then
    for I := 0 to High(ChildDirs) do
      AddSourceDir(ChildDirs[I], True);
end;

Function TDebugInfo.CheckAddr(Const Addr : Pointer): Boolean;
Var
    UnitInfo : TUnitInfo;
    FuncInfo : TFuncInfo;
    LineInfo : TLineInfo;
Begin
    Result := GetLineInfo(Addr, UnitInfo, FuncInfo, LineInfo, False) <> slNotFound;
End;
{..............................................................................}

{..............................................................................}
Function TDebugInfo.CheckDebugException(ExceptionRecord: PExceptionRecord; Var IsTraceException: Boolean): Boolean;
Begin
     IsTraceException := False;
     Case ExceptionRecord^.ExceptionCode Of
         EXCEPTION_SET_THREAD_NAME,
         STATUS_NONCONTINUABLE_EXCEPTION :
             Result := True;
     Else
         Result := False;
     End;
End;
{..............................................................................}

{..............................................................................}
Procedure TDebugInfo.ClearDebugInfo;
Begin
    If FDebugInfoLoaded Then
    Begin
        FDebugInfoLoaded := False;

        FDirs.Clear;
        ClearStringList(FUnits);

        FDbgLog.ClearLog;

        FExeFileName := '';
    End;
End;
{..............................................................................}

{..............................................................................}
Function TDebugInfo.ReadDebugInfo(Const FileName : String; const SourceDirs : String = '') : Boolean;
Begin
    Result := True;

    If Not(FDebugInfoLoaded And SameText(FExeFileName, FileName)) Then
    Begin
        FDirs.Clear;
        FSourceDirs := SourceDirs;

        Result := DoReadDebugInfo(FileName, True);

        If Result Then
        Begin
            FExeFileName := FileName;
            FDebugInfoLoaded := True;

            UpdateSourceDirs;
        End;
    End;
End;

procedure TDebugInfo.UpdateSourceDirs;
var
  SL: TStringList;
  I: Integer;
  S: String;
begin
  FDirs.Clear;

  SL := TStringList.Create;
  try
    SL.Delimiter := ';';
    SL.StrictDelimiter := True;
    SL.Duplicates := dupIgnore;

    SL.DelimitedText := FSourceDirs;

    for I := 0 to SL.Count - 1 do
    begin
      S := Trim(SL[I]);
      if (S <> '') then
      begin
        S := ExcludeTrailingPathDelimiter(S);
        if DirectoryExists(S) then
          AddSourceDir(S, True);
      end;
    end;
  finally
    FreeAndNil(SL);
  end;
end;
{..............................................................................}

{..............................................................................}
Function TDebugInfo.DumpLineInformation(Const Addr: Pointer): String;
Var
    UnitName : String;
    FuncName : String;
    Line     : Integer;
Begin
    If GetLineInformation(Addr, UnitName, FuncName, Line, False) <> slNotFound Then
        Result := Format('%p: %s@%s(%d)', [Pointer(Addr), UnitName, FuncName, Line])
    Else
        Result := Format('%p: no source info', [Pointer(Addr)]);
End;


function TDebugInfo.FullUnitName(const UnitName: String): String;
var
  Res: String;
begin
  Res := ExtractFileName(UnitName);

  if not FDirs.TryGetValue(AnsiLowerCase(Res), Result) then
  begin
    if not SameText(ExtractFileExt(Res), '.pas') then
    begin
      Res := Res + '.pas';
      if FDirs.TryGetValue(AnsiLowerCase(Res), Result) then Exit;
    end;

    Result := Res;
  end;
end;

function TDebugInfo.FuncByName(const FuncName: AnsiString): TFuncInfo;
var
  I: Integer;
begin
  for I := 0 to Units.Count - 1 do
  begin
    Result := TUnitInfo(Units.Objects[I]).FindFuncByName(FuncName);
    if Assigned(Result) then
      Exit;
  end;

  Result := Nil;
end;

{..............................................................................}

{..............................................................................}
Function TDebugInfo.GetFileCount : Integer;
Begin
    Result := FUnits.Count;
End;
{..............................................................................}

{..............................................................................}
Function TDebugInfo.GetFile(Index : Integer) : String;
Begin
    Result := TUnitInfo(FUnits[Index]).FullUnitName;
End;
{..............................................................................}

{..............................................................................}
Function TDebugInfo.GetParamsStr(FuncInfo : TFuncInfo; Const EBP : Pointer; IsTopStack : Boolean) : String;
//Var
//    I            : Integer;
//    ToStringData : TToStringData;
//    ParamName    : String;
//    ParamValue   : TVarInfo;
//    ParamEval    : Variant;
//    ParamRes     : String;
Begin
//    Result := '';
//
//    If (FuncInfo <> Nil) And (FuncInfo.Params.Count > 0) Then
//    Begin
//        ToStringData.DebuggeeControl := DebuggeeControl;
//        ToStringData.DebugInfo       := Self;
//        ToStringData.Mode            := tsmBrief;
//        ToStringData.RecursionLevel  := 0;
//
//        For I := 0 To FuncInfo.Params.Count - 1 Do
//        Begin
//            ParamName := FuncInfo.Params[I];
//            ParamValue := TVarInfo(FuncInfo.Params.Objects[I]);
//
//            ParamRes := '???';
//
//            If (EBP <> 0) And ((ParamValue.VarKind In [vkGlobal, vkStack]) Or (IsTopStack And (ParamValue.VarKind = vkRegister))) Then
//            Begin
//                Try
//                    ParamEval := EvaluateVariable(DebuggeeControl, ParamValue, EBP, False);
//                    ParamRes  := VariantToString(ParamEval, ToStringData);
//                Except
//                    ParamRes := '[error]';
//                End;
//            End;
//
//            If Result <> '' Then Result := Result + ', ';
//            Result := Result + Format('%s=%s', [ParamName, ParamRes]);
//        End;
//
//        If Result <> '' Then
//            Result := '(' + Result + ')';
//    End;
End;
{..............................................................................}

{..............................................................................}
Function TDebugInfo.GetTypeInfo(Const TypeName: String): TTypeInfo;
Var
    UnitInfo : TUnitInfo;
    I        : Integer;
Begin
    Result := Nil;
    For I := 0 To Units.Count - 1 Do
    Begin
        UnitInfo := TUnitInfo(Units.Objects[I]);
        Result := UnitInfo.FindTypeByName(AnsiString(TypeName));
        if Result <> Nil then
          Exit;
    End;
End;
{..............................................................................}

{..............................................................................}
Function TDebugInfo.GetExceptionName(ExceptionRecord: PExceptionRecord) : String;
Begin
    case ExceptionRecord^.ExceptionCode of
        STATUS_ACCESS_VIOLATION              : Result := 'EACCESS_VIOLATION';
        STATUS_ARRAY_BOUNDS_EXCEEDED         : Result := 'EARRAY_BOUNDS_EXCEEDED';
        STATUS_FLOAT_DENORMAL_OPERAND        : Result := 'EFLOAT_DENORMAL_OPERAND';
        STATUS_FLOAT_DIVIDE_BY_ZERO          : Result := 'EFLOAT_DIVIDE_BY_ZERO';
        STATUS_FLOAT_INEXACT_RESULT          : Result := 'EFLOAT_INEXACT_RESULT';
        STATUS_FLOAT_INVALID_OPERATION       : Result := 'EFLOAT_INVALID_OPERATION';
        STATUS_FLOAT_OVERFLOW                : Result := 'EFLOAT_OVERFLOW';
        STATUS_FLOAT_STACK_CHECK             : Result := 'EFLOAT_STACK_CHECK';
        STATUS_FLOAT_UNDERFLOW               : Result := 'EFLOAT_UNDERFLOW';
        STATUS_INTEGER_DIVIDE_BY_ZERO        : Result := 'EINTEGER_DIVIDE_BY_ZERO';
        STATUS_INTEGER_OVERFLOW              : Result := 'EINTEGER_OVERFLOW';
        STATUS_PRIVILEGED_INSTRUCTION        : Result := 'EPRIVILEGED_INSTRUCTION';
        STATUS_STACK_OVERFLOW                : Result := 'ESTACK_OVERFLOW';
        STATUS_CONTROL_C_EXIT                : Result := 'ECONTROL_C_EXIT';
    else
        Result := Format('$%x', [ExceptionRecord^.ExceptionCode]);
    end;
End;
{..............................................................................}

{..............................................................................}
Function TDebugInfo.GetExceptionAddress(ExceptionRecord: PExceptionRecord): Pointer;
Begin
    Result := ExceptionRecord^.ExceptionAddress;
End;
{..............................................................................}

{..............................................................................}
function TDebugInfo.GetExceptionFrame(ExceptionRecord: PExceptionRecord): Pointer;
begin
  Result := Nil;
end;
{..............................................................................}

{..............................................................................}
Function TDebugInfo.GetExceptionMessage(ExceptionRecord: PExceptionRecord; const ThreadId: TThreadId): String;
Begin
    Result := '';
//    Result := Format('Exception "%s($%x)" at $%p, TID = $%x', [
//         GetExceptionName(ExceptionRecord),
//         ExceptionRecord^.ExceptionCode,
//         GetExceptionAddress(ExceptionRecord),
//         ThreadId]);
End;
{..............................................................................}

{..............................................................................}
procedure TDebugInfo.GetCallStackItems(const ThreadID: TThreadId; Const ExceptAddr, ExceptFrame: Pointer; StackItems : TList);

    Function AddStackEntry(Const Addr, EBP: Pointer) : TStackEntry;
    var
        LastStackEntry: TStackEntry;
    Begin
        Result := Nil;

        if StackItems.Count > 0 then
        begin
            LastStackEntry := TStackEntry(StackItems[StackItems.Count - 1]);
            if LastStackEntry.EBP = EBP then
                Exit;
        end;

        If IsValidCodeAddr(Addr) Then
        Begin
            Result := TStackEntry.Create;
            Result.UpdateInfo(Addr);
            Result.EBP := EBP;

            StackItems.Add(Result);
        End
    End;

Var
    EIP : Pointer;
    EBP : Pointer;
    ESP : Pointer;
    ESPV : Pointer;
    OpCode : Byte;
    StackEntry : TStackEntry;
    ThData: PThreadData;
Begin
    If (ExceptAddr <> nil) And (ExceptFrame <> nil) Then
    Begin
        EIP := ExceptAddr;
        EBP := ExceptFrame;
        ESP := nil;
    End
    Else
    Begin
        ThData := FDebuger.UpdateThreadContext(ThreadID);

        if ThData = Nil then Exit;

        EIP := Pointer(ThData^.Context.Eip);
        EBP := Pointer(ThData^.Context.Ebp);
        ESP := Pointer(ThData^.Context.Esp);
    End;

    StackEntry := AddStackEntry(EIP, EBP);

    If (ESP <> nil) And (StackEntry <> Nil) And (StackEntry.FuncInfo <> Nil) And (StackEntry.LineInfo <> Nil) Then
    Begin
        If (StackEntry.LineInfo = TLineInfo(StackEntry.FuncInfo.Lines[0])) Then
        Begin
            If (EIP = StackEntry.FuncInfo.Address) Then
            Begin
                StackEntry.EBP := nil;
                FDebuger.ReadData(ESP, @ESPV, SizeOf(ESPV));
                AddStackEntry(ESPV, EBP);
            End
            Else
            Begin
                StackEntry.EBP := nil;
                FDebuger.ReadData(ESP, @ESPV, SizeOf(ESPV));
                // push ebp; move ebp, esp;
                If (ESPV = EBP) Then
                Begin
                    FDebuger.ReadData(Pointer(Cardinal(ESP) + 4), @ESPV, SizeOf(ESPV));
                    AddStackEntry(ESPV, EBP);
                End;
            End;
        End
        Else
        If (StackEntry.LineInfo = TLineInfo(StackEntry.FuncInfo.Lines[StackEntry.FuncInfo.Lines.Count - 1])) Then
        Begin
            StackEntry.EBP := nil;
            // ret;
            FDebuger.ReadData(EIP, @OpCode, SizeOf(Byte));
            If OpCode In [$C3, $CB] Then
            Begin
                FDebuger.ReadData(ESP, @ESPV, SizeOf(ESPV));
                AddStackEntry(ESPV, EBP);
            End;
        End;
    End;

    While IsValidAddr(EBP) Do
    Begin
        FDebuger.ReadData(IncPointer(EBP, 4), @EIP, SizeOf(Pointer));
        FDebuger.ReadData(EBP, @EBP, SizeOf(Pointer));

        If AddStackEntry(EIP, EBP) = Nil Then
            Break;
    End;
End;
{..............................................................................}

{..............................................................................}
function TDebugInfo.ParseConstName(ConstInfo: TConstInfo): String;
begin
  Result := String(ConstInfo.Name);
end;

function TDebugInfo.ParseFuncName(FuncInfo: TFuncInfo): String;
begin
  Result := String(FuncInfo.Name);
end;

function TDebugInfo.ParseStructMemberName(StructMember: TStructMember): String;
begin
  Result := String(StructMember.Name);
end;

function TDebugInfo.ParseTypeName(TypeInfo: TTypeInfo): String;
begin
  Result := String(TypeInfo.Name);
end;

function TDebugInfo.ParseUnitName(UnitInfo: TUnitInfo; const WithExt: Boolean = True): String;
begin
  Result := ExtractFileName(UnitInfo.FullUnitName);

  if not WithExt then
    Result := ChangeFileExt(Result, '');
end;

function TDebugInfo.ParseVarName(VarInfo: TVarInfo): String;
begin
  Result := String(VarInfo.Name);
end;

Function TDebugInfo.ProcessDebugOutputMessage(Const Msg: WideString; DebugEvent : PDebugEvent): Boolean;
Begin
    Result := False;

    //IDEAPI_AddToOutputPanel(PWideChar(Msg), False, False);
End;
{..............................................................................}

{..............................................................................}
Function TDebugInfo.CheckDebugOutputMessage(DebugEvent : PDebugEvent): Boolean;
Var
    OutputStringW: WideString;
    OutputStringA: AnsiString;

    isUnicode: Boolean;
    StrAddr: Pointer;
    StrSize: Word;
Begin
    Result := False;

    If DebugEvent^.dwDebugEventCode <> OUTPUT_DEBUG_STRING_EVENT Then
        Exit;

    isUnicode := WordBool(DebugEvent^.DebugString.fUnicode);
    StrAddr := DebugEvent^.DebugString.lpDebugStringData;
    StrSize := DebugEvent^.DebugString.nDebugStringLength - 1;

    if isUnicode then
    begin
      SetLength(OutputStringW, StrSize div SizeOf(WideChar));
      if not FDebuger.ReadData(StrAddr, @OutputStringW, StrSize) then
        OutputStringW := '';
    end
    else
    begin
      SetLength(OutputStringA, StrSize);
      if not FDebuger.ReadData(StrAddr, @OutputStringA, StrSize) then
        OutputStringA := '';

      OutputStringW := WideString(OutputStringA);
    end;

    Result := ProcessDebugOutputMessage(OutputStringW, DebugEvent);
End;
{..............................................................................}

{..............................................................................}
Function TDebugInfo.IsSystemException(Const ExceptionCode: DWORD): Boolean;
Begin
    Case ExceptionCode Of
        STATUS_ACCESS_VIOLATION,
        STATUS_ARRAY_BOUNDS_EXCEEDED,
        STATUS_FLOAT_DENORMAL_OPERAND,
        STATUS_FLOAT_DIVIDE_BY_ZERO,
        STATUS_FLOAT_INEXACT_RESULT,
        STATUS_FLOAT_INVALID_OPERATION,
        STATUS_FLOAT_OVERFLOW,
        STATUS_FLOAT_STACK_CHECK,
        STATUS_FLOAT_UNDERFLOW,
        STATUS_INTEGER_DIVIDE_BY_ZERO,
        STATUS_INTEGER_OVERFLOW,
        STATUS_PRIVILEGED_INSTRUCTION,
        STATUS_STACK_OVERFLOW,
        STATUS_CONTROL_C_EXIT :
            Result := True;
    Else
        Result := False;
    End;
End;

function TDebugInfo.IsValidAddr(const Addr: Pointer): Boolean;
Begin
    Result := Debuger.IsValidAddr(Addr);
end;

function TDebugInfo.IsValidCodeAddr(const Addr: Pointer): Boolean;
Begin
    Result := Debuger.IsValidCodeAddr(Addr);
end;

function TDebugInfo.IsValidDataAddr(const Addr: Pointer; const ThreadID: TThreadId): Boolean;
begin
    Result := IsValidAddr(Addr) And Not(IsValidCodeAddr(Addr) Or IsValidStackAddr(Addr, ThreadID));
end;

function TDebugInfo.IsValidStackAddr(const Addr: Pointer; const ThreadID: TThreadId): Boolean;
Var
    TIB : Pointer;
    TopStack : Pointer;
    ThreadData: PThreadData;
    ThreadContext: TContext;
    ldtSel : LDT_ENTRY;
Begin
    Result := False;

    ThreadData := FDebuger.GetThreadData(ThreadID);

    if ThreadData <> nil then
    begin
        ThreadContext := FDebuger.GetRegisters(ThreadID);
        If GetThreadSelectorEntry(ThreadData^.ThreadHandle, ThreadContext.SegFs, ldtSel) Then
        Begin
            TIB := Pointer((ldtSel.BaseHi shl 24) Or (ldtSel.BaseMid shl 16) Or (ldtSel.BaseLow));
            TopStack := nil;
            if FDebuger.ReadData(Pointer(Cardinal(TIB) + 4), @TopStack, SizeOf(Pointer)) { fs:[4] } then
              Result := (TopStack <> nil) And (Cardinal(Addr) <= Cardinal(TopStack)) And
                (Cardinal(Addr) >= (ThreadContext.Esp));
        End;
    end;
end;

{..............................................................................}

{ TFuncInfo }

{...............................................................................}
Constructor TFuncInfo.Create;
Begin
    Inherited;

    Params := TNameList.Create;
    Params.FreeItems := False;
End;
{...............................................................................}

{...............................................................................}
Destructor TFuncInfo.Destroy;
Begin
    FreeAndNil(Params);

    Inherited;
End;

function TFuncInfo.Name: AnsiString;
begin
    Result := gvDebugInfo.GetNameById(NameId);
end;

function TFuncInfo.ParamsAsString: String;
//const
//  _Self = 'Self';
//  _Result = 'Result';
var
  I: Integer;
  Param: TVarInfo;
  Res: TStringList;
begin
  Result := '';

  Res := TStringList.Create;
  try
    for I := 0 to Params.Count - 1 do
    begin
      Param := TVarInfo(Params[I]);

      // TODO: ���������, ��� ��������� ��������� ������� �� � ����������
      //if (I = 0) and SameText(String(Param.Name), _Self) then
      //  Continue;

      //if SameText(String(Param.Name), _Result) then
      //  Break;

      Res.Add(Param.AsString);
    end;

    for I := 0 to Res.Count - 1 do
    begin
      if Result <> '' then
        Result := Result + '; ';

      Result := Result + Res[I];
    end;
  finally
    FreeAndNil(Res);
  end;
end;

function TFuncInfo.ShortName: String;
begin
  Result := gvDebugInfo.ParseFuncName(Self);
end;

{...............................................................................}

{ TTypeInfo }

constructor TTypeInfo.Create;
begin
  Inherited;

  NameId := -1;
  Members := Nil;
  Elements := Nil;
end;

destructor TTypeInfo.Destroy;
begin
    FreeAndNil(Members);
    FreeAndNil(Elements);

    Inherited;
end;

function TTypeInfo.ElementsToString: String;
var
  I: Integer;
begin
  Result := '';

  if (Kind = tkEnum) And (Elements <> Nil) then
  begin
    for I := 0 to Elements.Count - 1 do
    begin
      if Result <> '' then
        Result := Result + ', ';

      Result := Result + Elements[I].ShortName;
    end;
  end;
end;

function TTypeInfo.KindAsString: String;
begin
  case Kind of
    tkBoolean         : Result := 'Boolean';
    tkWordBool        : Result := 'WordBool';
    tkLongBool        : Result := 'LongBool';
    tkShortInt        : Result := 'ShortInt';
    tkSmallInt        : Result := 'SmallInt';
    tkInteger         : Result := 'Integer';
    tkInt64           : Result := 'Int64';
    tkByte            : Result := 'Byte';
    tkWord            : Result := 'Word';
    tkCardinal        : Result := 'Cardinal';
    tkUInt64          : Result := 'UInt64';
    tkSingle          : Result := 'Single';
    tkReal48          : Result := 'Real48';
    tkReal            : Result := 'Real';
    tkExtended        : Result := 'Extended';
    tkCurrency        : Result := 'Currency';
    tkComplex         : Result := 'Complex';
    tkPString         : Result := 'ShortString';
    tkLString         : Result := 'String';
    tkWString         : Result := 'WideString';
    tkChar            : Result := 'Char';
    tkPointer         : Result := 'Pointer';
    tkSubRange        : Result := 'SubRange';
    tkArray           : Result := 'Array';
    tkEnum            : Result := '';
    tkStructure       : Result := 'Record';
    tkClass           : Result := 'TClass';
    tkSet             : Result := 'Set';
    tkVariant         : Result := 'Variant';
    tkProperty        : Result := 'Property';
    tkFieldList       : Result := 'FieldList';
    tkClosure         : Result := 'Closure';
    tkClassRef        : Result := 'ClassRef';
    tkWideChar        : Result := 'WideChar';
    tkProcedure       : Result := 'Procedure';
    tkArgList         : Result := 'ArgList';
    tkMFunction       : Result := 'MFunction';
    tkVoid            : Result := 'Void';
    tkObject          : Result := 'TObject';
    tkDynamicArray    : Result := 'DynArray';
  end;
end;

function TTypeInfo.Name: AnsiString;
begin
  Result := '';

  if NameId > 0 then
    Result := gvDebugInfo.GetNameById(NameId)
  else
    if (Kind = tkObject) and (BaseType <> Nil) then
      Result := BaseType.Name
    else
      Result := KindAsString;
end;

function TTypeInfo.ShortName: String;
begin
  Result := gvDebugInfo.ParseTypeName(Self);
end;

function TTypeInfo.TypeOf: String;
begin
  Result := '';

  if BaseType <> nil then
  begin
    case Kind of
      tkArray,
      tkSet,
      tkDynamicArray:
        Result := Format('%s Of %s', [KindAsString, BaseType.ShortName]);
      tkObject, tkClass:
        Result := Format('%s(%s)', [KindAsString, BaseType.ShortName]);
    else
      Result := Format('(%s)', [BaseType.ShortName])
    end;
  end
  else
  begin
    case Kind of
      tkEnum:
        Result := Format('(%s)', [ElementsToString]);
    else
      Result := KindAsString;
    end;
  end;
end;

{ TVarInfo }

function TVarInfo.AsString: String;
begin
  Result := Format('%s: %s', [ShortName, DataType.ShortName]);
end;

Constructor TVarInfo.Create;
Begin
    inherited;

    // ����� ����������� �� �������������
    //RegisterRanges := TList.Create;
    RegisterRanges := Nil;
End;

Destructor TVarInfo.Destroy;
Begin
    if Assigned(RegisterRanges) then
      FreeList(RegisterRanges);

    inherited;
End;

function TVarInfo.UnitInfo: TUnitInfo;
begin
  Result := Nil;

  if Owner is TFuncInfo then
    Result := TFuncInfo(Owner).UnitInfo
  else
  if Owner is TUnitInfo then
    Result := TUnitInfo(Owner);
end;

function TVarInfo.Name: AnsiString;
begin
  Result := gvDebugInfo.GetNameById(NameId)
end;

function TVarInfo.ShortName: String;
begin
  Result := gvDebugInfo.ParseVarName(Self)
end;

{ TUnitInfo }

procedure TUnitInfo.Clear;
begin
    ClearList(Segments);
    
    if Assigned(UsedUnits) then
      UsedUnits.Clear;

    ClearList(Lines);

    inherited Clear;
end;

constructor TUnitInfo.Create(ADebugInfo: TDebugInfo);
begin
    Inherited Create;

    DebugInfo := ADebugInfo;

    UsedUnits := TStringList.Create;
    Segments  := TList.Create;

    CodeSize := 0;
    DataSize := 0;
end;

destructor TUnitInfo.Destroy;
begin
    Clear;

    FreeAndNil(UsedUnits);
    FreeAndNil(Segments);

    Inherited;
end;

function TUnitInfo.FullUnitName: String;
begin
  Result := DebugInfo.FullUnitName(Name);
end;

function TUnitInfo.Name: AnsiString;
begin
    Result := gvDebugInfo.GetNameById(NameId);
end;

function TUnitInfo.ShortName: String;
begin
  Result := gvDebugInfo.ParseUnitName(Self);
end;

{ TStackEntry }

constructor TStackEntry.Create;
begin
    Inherited Create;

    UnitInfo        := Nil;
    FuncInfo        := Nil;
    LineInfo        := Nil;
    EIP             := Nil;
    RET             := Nil;
    EBP             := Nil;
end;

function TStackEntry.GetInfo: String;
begin
    Result := Format('[$%p] ', [EIP]);
    If UnitInfo <> Nil Then
    Begin
        // � XE4 ��� ������ ��� � �������� �������
        //If UnitInfo <> Nil Then
        //    Result := Result + String(UnitInfo.Name);
        If FuncInfo <> Nil Then
        begin
            Result := Result + FuncInfo.ShortName;
        end;
        If LineInfo <> Nil Then
            Result := Result + Format(' (%d)', [LineInfo.LineNo]);
    End
    Else
        Result := Result + 'no source';
end;

function TStackEntry.UpdateInfo(const Addr: Pointer): TFindResult;
begin
    EIP := Addr;
    Result := gvDebugInfo.GetLineInfo(EIP, UnitInfo, FuncInfo, LineInfo, False);
end;

{ TConstInfo }

function TConstInfo.Name: AnsiString;
begin
  Result := gvDebugInfo.GetNameById(NameId);
end;

function TConstInfo.ShortName: String;
begin
  Result := gvDebugInfo.ParseConstName(Self);
end;

function TConstInfo.UnitInfo: TUnitInfo;
begin
  if OwnerInfo is TUnitInfo then
    Result := TUnitInfo(OwnerInfo)
  else
    if OwnerInfo is TFuncInfo then
      Result := TFuncInfo(OwnerInfo).UnitInfo
    else
      Result := Nil;
end;

function TConstInfo.ValueAsString: String;
begin
  Result := UnitInfo.DebugInfo.VarValueAsString(Value);
end;

{ TSegmentCodeInfo }

procedure TSegmentCodeInfo.Clear;
begin
  Consts.Clear;
  Types.Clear;
  Vars.Clear;
  Funcs.Clear;

  Lines.Clear;
end;

constructor TSegmentCodeInfo.Create;
begin
  inherited;

  Consts := TNameList.Create;
  Types  := TNameList.Create;
  Vars   := TNameList.Create;
  Funcs  := TNameList.Create;

  Lines  := TList.Create;
end;

destructor TSegmentCodeInfo.Destroy;
begin
  Clear;

  FreeAndNil(Consts);
  FreeAndNil(Types);
  FreeAndNil(Vars);
  FreeAndNil(Funcs);

  FreeAndNil(Lines);

  inherited;
end;

function TSegmentCodeInfo.FindConstByName(const ConstName: AnsiString): TConstInfo;
begin
  Result := TConstInfo(Consts.FindByName(ConstName));
end;

function TSegmentCodeInfo.FindFuncByName(const FuncName: AnsiString): TFuncInfo;
begin
  Result := TFuncInfo(Funcs.FindByName(FuncName));
end;

function TSegmentCodeInfo.FindFuncByNameId(const FuncNameId: Integer): TFuncInfo;
begin
  Result := TFuncInfo(Funcs.FindByNameId(FuncNameId));
end;

function TSegmentCodeInfo.FindTypeByName(const TypeName: AnsiString): TTypeInfo;
begin
  Result := TTypeInfo(Types.FindByName(TypeName));
end;

function TSegmentCodeInfo.FindVarByName(const VarName: AnsiString): TVarInfo;
begin
  Result := TVarInfo(Vars.FindByName(VarName));
end;

{ TNameList }

procedure TNameList.CheckNameIdList;
var
  I: Integer;
  NameInfo: TNameInfo;
begin
  if FNameIdList = nil then
  begin
    FNameIdList := TNameIdList.Create(Capacity);

    for I := 0 to Count - 1 do
    begin
      NameInfo := TNameInfo(List[I]);

      FNameIdList.AddOrSetValue(NameInfo.NameId, NameInfo);
    end;
  end;
end;

procedure TNameList.Clear;
var
  I: Integer;
  Obj: TObject;
begin
  if Assigned(FNameIdList) then
    FreeAndNil(FNameIdList);

  if FFreeItems then
  begin
    for I := 0 to Count - 1 do
    begin
      Obj := List[I];
      if Obj <> nil then
      begin
        List[I] := nil;
        FreeAndNil(Obj);
      end;
    end;
  end;

  inherited Clear;
end;

constructor TNameList.Create;
begin
  inherited;

  FNameIdList := Nil;
  FFreeItems := True;
  //Capacity := 16;
end;

destructor TNameList.Destroy;
begin
  Clear;

  inherited;
end;

function TNameList.FindByName(const Name: AnsiString): TNameInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TNameInfo(List[I]);
    if (Result.NameId > 0) And SameText(Name, Result.Name) then
      Exit;
  end;

  Result := Nil;
end;

function TNameList.FindByNameId(const NameId: TNameId): TNameInfo;
begin
  CheckNameIdList;

  if not FNameIdList.TryGetValue(NameId, Result) then
    Result := Nil;
end;

function TNameList.GetNameInfoItem(const Index: Integer): TNameInfo;
begin
  Result := TNameInfo(Items[Index]);
end;

procedure TNameList.Notify(Ptr: Pointer; Action: TListNotification);
var
  NameInfo: TNameInfo;
begin
  if Assigned(FNameIdList) then
  begin
    NameInfo := TNameInfo(Ptr);

    case Action of
      lnAdded:
        FNameIdList.AddOrSetValue(NameInfo.NameId, NameInfo);
      lnDeleted:
        FNameIdList.Remove(NameInfo.NameId);
    end;
  end;
end;

{ TStructMember }

function TStructMember.Alias: AnsiString;
begin
  Result := DataType.UnitInfo.DebugInfo.GetNameById(AliasNameId);
end;

function TStructMember.MethodName: AnsiString;
begin
  Result := DataType.UnitInfo.DebugInfo.GetNameById(MethodNameId);
end;

function TStructMember.Name: AnsiString;
begin
  Result := gvDebugInfo.GetNameById(NameId);
end;

function TStructMember.ShortName: String;
begin
  Result := gvDebugInfo.ParseStructMemberName(Self);
end;

{ TEnumInfo }

function TEnumInfo.Name: AnsiString;
begin
  Result := gvDebugInfo.GetNameById(NameId);
end;

function TEnumInfo.ShortName: String;
begin
  Result := String(Name);
end;

End.
