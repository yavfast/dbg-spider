Unit Evaluator;

Interface

Uses
    Classes,
    DebugInfo,
    Debuger,
    EvaluateTypes;
{..............................................................................}

Const
    cDefTimeOut = 500;
    cDebuggeeMemorySize = 64 * 1024;

{..............................................................................}
Type
    TParamKind = (pkValue, pkVariant);

    TParam = Class
    Public
        Kind         : TParamKind;
        FuncParam    : TVarInfo;
        Value        : TUIntPtr;
        VariantValue : Variant;
    End;
{..............................................................................}

    TDebugerMemory = Class
    Private
        FDebuger         : TDebuger;
        FMemory          : TPointer;
        FCurrentData     : TPointer;
        FDataLeft        : TSize;
    Public
        Constructor Create(Debuger : TDebuger);
        Destructor  Destroy; override;

        Procedure   CheckFreeSpace(const Size : TSize);

        Procedure   Reset;
        Procedure   Skip(const Size : TSize);
        Procedure   Write(const Data : Byte); overload;
        Procedure   Write(const Data : TPointer); overload;
        Procedure   Write(Const Data : Array Of Byte); overload;

        Property    CurrentData : TPointer read FCurrentData;
    End;

{..............................................................................}
    TEvaluator = Class
    Private
        FCodeAddr         : TPointer;
        FDebuger          : TDebuger;
        FMemory           : TDebugerMemory;
        FFuncInfo         : TFuncInfo;
        FParams           : TCalculateParams;
        FSelfAddress      : TPointer;
        FPreparedParams   : TList;
        FResultAddr       : TPointer;
        FTimeOut          : Cardinal;

        FCodeProtected    : TPointer;
        FFailedAddr       : TPointer;
        FExceptionClass   : TPointer;
        FExceptionMessage : TPointer;

        Procedure   AddResultParameter;
        Procedure   AllocAndWriteAnsiString(Param : TParam; Const Value : Variant);
        Procedure   AllocAndWriteShortString(Param : TParam; Const Value : Variant);
        Procedure   AllocAndWriteWideString(Param : TParam; Const Value : Variant);
        Procedure   AllocateExceptionStuff;
        Function    AllocateParam(Const Param : TParam; Const Value : Variant; FuncParam : TVarInfo) : Boolean;
        Function    AllocateMemForType(TypeInfo : TTypeInfo) : TPointer;
        Procedure   CheckResult;
        Procedure   ExecuteCode;
        Procedure   GenerateCall;
        Procedure   GenerateCallProtected;
        Procedure   GenerateSaveResult;
        Procedure   GenerateRetCode;
        // Generate code to return control to debugger
        Procedure   GenerateStopCode;
        Function    GetResult : Variant;

        Procedure   InitMemory;
        Procedure   InitResultMemory(TypeInfo : TTypeInfo; Address : TPointer);

        Procedure   PassCurrencyParam(Const Value : Variant);
        Procedure   PassDoubleParam(Const Value : Variant);
        Procedure   PassExtendedParam(Const Value : Variant);
        Procedure   PassInt64Param(Const Value : Variant);
        Procedure   PassParameters;
        Procedure   PassSingleParam(Const Value : Variant);
        Procedure   PrepareParameters;
    Public
        Constructor Create(Debuger: TDebuger);
        Destructor  Destroy; override;

        Function    Evaluate(SelfAddress : TPointer; FuncInfo : TFuncInfo; Const Params : TCalculateParams; Const TimeOut : Cardinal) : Variant;
    End;
{..............................................................................}

{..............................................................................}
Function EvaluateFunction(Debuger: TDebuger; SelfAddress : TPointer; FuncInfo : TFuncInfo; Const Params : TCalculateParams;
    Const TimeOut : Cardinal = cDefTimeOut) : Variant;
{..............................................................................}

{..............................................................................}
Implementation

Uses
    SysUtils,
    Variants,
    Contnrs,
    //System.Debug,
    //System.Strings,
    EvaluateConsts,
    EvaluateProcs, ClassUtils;
    //Logger;
{..............................................................................}

{..............................................................................}
Const
    // We assume that any debuggee control need not more than 16 bytes for stop code
    cMaxStopCodeSize       = 16;
    cMaxRegParams          = 3;
    c__Eval__CallProtected = '__Eval__CallProtected';
{..............................................................................}

{..............................................................................}
Type
    Instructions = Class
    Public
        Const
            cCallRelative            : Byte = $E8;

            cAssignDataToRegister    : Array[0 .. cMaxRegParams - 1 ] Of Byte = ($B8, $BA, $B9);
            cAssignDataToEax         : Byte = $B8;
            cAssignDataToEdx         : Byte = $BA;
            cAssignDataToEcx         : Byte = $B9;

            cPushData                : Byte = $68;
            cRet                     : Byte = $C3;
            cBreakpointOpCode        : Byte = $CC;
            cFWait                   : Byte = $9B;

            cLoadRegisterEaxAbsolute : Array[0 .. 1] Of Byte = ($8B, $05);
            cLoadRegisterEdxAbsolute : Array[0 .. 1] Of Byte = ($8B, $15);
            cLoadRegisterEcxAbsolute : Array[0 .. 1] Of Byte = ($8B, $0D);

            cSaveRegisterEaxAbsolute : Array[0 .. 1] Of Byte = ($89, $05);
            cSaveRegisterEDxAbsolute : Array[0 .. 1] Of Byte = ($89, $15);

            cSaveSingleSt0Absolute   : Array[0 .. 1] Of Byte = ($D9, $1D);
            cSaveDoubleSt0Absolute   : Array[0 .. 1] Of Byte = ($DD, $1D);
            cSaveExtendedSt0Absolute : Array[0 .. 1] Of Byte = ($DB, $3D);
            cSaveCurrencySt0Absolute : Array[0 .. 1] Of Byte = ($DF, $3D);

    End;
{..............................................................................}

type
  EInternalEvaluatorError = class(Exception);

Procedure RaiseInternalError(Const Message: String);
Begin
    Raise EInternalEvaluatorError.Create(Message);
End;


{..............................................................................}
Procedure ZeroString(Debuger: TDebuger; const Address : TPointer);
Var
    Buf : TPointer;
Begin
    Buf := nil;
    Debuger.WriteData(@Buf, Address, SizeOf(TPointer));
End;
{..............................................................................}

{..............................................................................}
Constructor TEvaluator.Create(Debuger: TDebuger);
Begin
    Inherited Create;

    FDebuger := Debuger;
    FMemory := Nil;
End;
{..............................................................................}

{..............................................................................}
Destructor TEvaluator.Destroy;
Begin
    FreeAndNil(FPreparedParams);

    Inherited;
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.AddResultParameter;
Var
    Param    : TParam;
    TypeInfo : TTypeInfo;
Begin
    TypeInfo := FFuncInfo.ResultType;
    Case TypeInfo.Kind Of
        tkInteger,
        tkInt64,
        tkUInt64,
        //tkFloat,
        tkChar,
        tkWideChar,
        tkBoolean,
        tkEnum,
        tkPointer,
        tkObject,
        tkClass :
            ; // We don't need to pass result of this type
        tkPString,
        tkLString,
        tkWString,
        tkSet,
        tkStructure,
        tkArray :
        Begin
            Case TypeInfo.Kind Of
                tkSet :
                    If TypeInfo.DataSize <= SizeOf(TInteger) Then
                        Exit;
                tkStructure :
                    If False{(TypeInfo.DataSize <= SizeOf(TInteger))} Then //TODO: Later we need to decide how to pass result of struct type depending on size (Delphi) or always as out parameter (FPC)
                        Exit;
            End;
            Param := TParam.Create;
            Param.Kind := pkValue;
            Param.Value := FResultAddr;
            FPreparedParams.Add(Param);
        End;
        tkVoid :
            ; // We don't need to to pass result for procedures
    Else
        RaiseInternalError(cUnsupportedDataType);
        //TODO: Add check if this is procedure
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.AllocAndWriteAnsiString(Param : TParam; Const Value : Variant);
Const
    StrRectSize = SizeOf(TInternalAnsiStringRec);
Var
    Len          : LongWord;
    ValueToWrite : AnsiString;
    PRefCount    : PByte;
    SavedRef     : LongWord;
Begin
    // Allocate memory
    ValueToWrite := AnsiString(Value);
    Len := (Length(ValueToWrite) + 1) + StrRectSize;
    Param.Value := FDebuger.AllocMem(Len);

    PRefCount := PByte(ValueToWrite);
    If PRefCount <> Nil Then
    Begin
        Dec(PRefCount, StrRectSize);
        SavedRef := PLongWord(PRefCount)^;
        PLongWord(PRefCount)^ := $FFFFFFFF;
        FDebuger.WriteData(PRefCount, Param.Value, Len);
        PLongWord(PRefCount)^ := SavedRef;
    End;

    Param.Value := IncPointer(Param.Value, StrRectSize); //???
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.AllocAndWriteShortString(Param : TParam; Const Value : Variant);
Var
    Len          : LongWord;
    ValueToWrite : ShortString;
Begin
    Len := SizeOf(ShortString);
    Param.Value := FDebuger.AllocMem(Len);
    ValueToWrite := ShortString(AnsiString(Value));
    FDebuger.WriteData(@ValueToWrite, Param.Value, Len);
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.AllocAndWriteWideString(Param : TParam; Const Value : Variant);
Const
    StrRectSize = SizeOf(TInternalWideStringRec);
Var
    Data         : PByte;
    Len          : LongWord;
    ValueToWrite : WideString;
Begin
    // Allocate memory
    ValueToWrite := Value;
    Len := (Length(ValueToWrite) + 1) * SizeOf(WideChar) + StrRectSize;
    Param.Value := FDebuger.AllocMem(Len);

    Data := PByte(ValueToWrite);
    If Data <> Nil Then
    Begin
        Dec(Data, StrRectSize);
        FDebuger.WriteData(Data, Param.Value, Len);
    End;

    Param.Value := IncPointer(Param.Value, StrRectSize);
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.AllocateExceptionStuff;
Begin
    FFailedAddr := FDebuger.AllocMem(SizeOf(Boolean));

    FExceptionClass := FDebuger.AllocMem(SizeOf(Pointer));
    ZeroString(FDebuger, FExceptionClass);

    FExceptionMessage := FDebuger.AllocMem(SizeOf(Pointer));
    ZeroString(FDebuger, FExceptionMessage);
End;
{..............................................................................}

{...............................................................................}
Function FirstChar(Const S : AnsiString) : AnsiChar;
Begin
    If S <> '' Then
      Result := S[1]
    Else
      Result := #0;
End;
{...............................................................................}

{...............................................................................}
Function FirstWideChar(Const S : WideString) : WideChar;
Begin
    If S <> '' Then
      Result := S[1]
    Else
      Result := #0;
End;
{...............................................................................}

{..............................................................................}
Function TEvaluator.AllocateParam(Const Param : TParam; Const Value : Variant; FuncParam : TVarInfo) : Boolean;
Begin
    Case FuncParam.DataType.Kind Of
        tkShortInt, tkSmallInt, tkInteger, tkByte, tkWord, tkCardinal, tkBoolean, tkWordBool, tkLongBool :
        Begin
            Param.Value := Pointer(Cardinal(Value));
            Result := True;
        End;
        tkChar :
        Begin
            Param.Value := Pointer(Cardinal(FirstChar(AnsiString(Value))));
            Result := True;
        End;
        tkWideChar :
        Begin
            Param.Value := Pointer(Cardinal(FirstWideChar(Value)));
            Result := True;
        End;
        tkEnum :
        Begin
            Param.Value := Pointer(RequireOrdinalValue(Value));
            Result := True;
        End;
        tkSingle, tkReal48, tkReal, tkExtended, tkCurrency,
        tkInt64, tkUInt64 : // These passed by value
            Result := False;
{        tkSet :
            If FuncParam.DataType.DataSize <= SizeOf(TInteger) Then
                Param.Value := GetSetValueAsInteger(Value)
            Else
                AllocAndWriteSet(Param, Value);}
        tkPString :
        Begin
            AllocAndWriteShortString(Param, Value);
            Result := True;
        End;
        tkLString :
        Begin
            AllocAndWriteAnsiString(Param, Value);
            Result := True;
        End;
        tkWString :
        Begin
            AllocAndWriteWideString(Param, Value);
            Result := True;
        End;
        tkObject :
        Begin
            If (Param.Value <> nil) And Not TryGetValueAddress(Value, Param.Value) Then
                ExpressionError(cCannotEvaluateAddress);
            Result := True;
        End
    Else
        RaiseInternalError(cUnsupportedDataType);
        Result := False;
    End;
End;
{..............................................................................}

{..............................................................................}
Function TEvaluator.AllocateMemForType(TypeInfo : TTypeInfo) : TPointer;
var
    Size: Cardinal;
Begin
    Size := 0;
    Case TypeInfo.Kind Of
        tkShortInt, tkSmallInt, tkInteger, tkByte, tkWord, tkCardinal, tkBoolean,
        tkWordBool, tkLongBool, tkChar, tkWideChar, tkEnum : // I don't think enums could be greater that Max(LongInt)
        Begin
            // For all integer types we allocate 4 bytes
            If TypeInfo.DataSize > SizeOf(TInteger) Then
                RaiseInternalError(cInvalidIntegerSize);
            Size := SizeOf(TInteger);
        End;
        tkSet :
            Size := TypeInfo.DataSize;
        tkInt64, tkUInt64 :
            Size := SizeOf(Int64);
        tkSingle, tkReal48, tkReal, tkExtended, tkCurrency :
            Size := Abs(TypeInfo.DataSize);
        tkPString :
            Size := SizeOf(ShortString);
        tkLString, tkWString :
        Begin
            // For AnsiString and WideStrings we have to clear memory
            Size := SizeOf(TPointer);
        End;
        tkStructure :
            Size := TypeInfo.DataSize;
        tkArray :
            Size := TypeInfo.DataSize;
        tkPointer,
        tkClass,
        tkObject :
            Size := SizeOf(TPointer);
        tkVoid :
             // We don't need to allocate result for procedures ???
            Size := SizeOf(TPointer);
    Else
        RaiseInternalError(cUnsupportedDataType);
        //TODO: Add check if this is procedure
    End;

    Result := FDebuger.AllocMem(Size);
    InitResultMemory(TypeInfo, Result);
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.CheckResult;
Var
    Addr             : TPointer;
    Failed           : Boolean;
    ExceptionClass   : AnsiString;
    ExceptionMessage : AnsiString;
    Msg              : String;
Begin
    FDebuger.ReadData(FFailedAddr, @Failed, SizeOf(Byte));
    If Failed Then
    Begin
        Addr := ReadAddressValue(FDebuger, FExceptionClass);
        ExceptionClass := ReadAnsiStringValue(FDebuger, Addr, False);
        Addr := ReadAddressValue(FDebuger, FExceptionMessage);
        ExceptionMessage := ReadAnsiStringValue(FDebuger, Addr, False);
        Msg := String(ExceptionClass);
        If Msg = '' Then
            Msg := String(ExceptionMessage)
        Else
            If ExceptionMessage <> '' Then
                Msg := Msg + ' : ' + String(ExceptionMessage);
        ExpressionError(Msg);
    End;
End;
{..............................................................................}

{..............................................................................}
Function TEvaluator.Evaluate(SelfAddress : TPointer; FuncInfo : TFuncInfo; Const Params : TCalculateParams; Const TimeOut : Cardinal) : Variant;
Begin
    FSelfAddress    := SelfAddress;
    FFuncInfo       := FuncInfo;
    FParams         := Params;
    FTimeOut        := TimeOut;

    InitMemory;
    Try
        PrepareParameters;
        FResultAddr := AllocateMemForType(FFuncInfo.ResultType);
        AllocateExceptionStuff;

        AddResultParameter;
        FCodeAddr := FMemory.CurrentData;
        PassParameters;
        GenerateCall;
        GenerateSaveResult;
        GenerateRetCode;

        GenerateCallProtected;

        ExecuteCode;

        CheckResult;

        Result := GetResult;
    Finally
        FParams   := Nil;
        FFuncInfo := Nil;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.ExecuteCode;
Begin
    FDebuger.ExecuteCode(FCodeProtected, FTimeOut);
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.GenerateCall;
Var
    RelAddr : TPointer;
Begin
    //$E8 RelAddr
    FMemory.Write(Instructions.cCallRelative);
    RelAddr := Pointer(Cardinal(FFuncInfo.Address) - Cardinal(FMemory.CurrentData) - SizeOf(TPointer));
    FMemory.Write(RelAddr);
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.GenerateCallProtected;
Var
    CallProtectedFuncInfo : TFuncInfo;
    RelAddr               : TPointer;
Begin
    FCodeProtected := FMemory.CurrentData;
    FMemory.Write(Instructions.cAssignDataToRegister[0]);
    FMemory.Write(FCodeAddr);

    FMemory.Write(Instructions.cAssignDataToRegister[1]);
    FMemory.Write(FFailedAddr);

    FMemory.Write(Instructions.cAssignDataToRegister[2]);
    FMemory.Write(FExceptionClass);

    FMemory.Write(Instructions.cPushData);
    FMemory.Write(FExceptionMessage);

    CallProtectedFuncInfo := FFuncInfo.UnitInfo.DebugInfo.FuncByName(c__Eval__CallProtected);
    if CallProtectedFuncInfo = Nil then
        RaiseInternalError(cCannotFindEvalSupportFunctions);

    FMemory.Write(Instructions.cCallRelative);
    RelAddr := TPointer(Cardinal(CallProtectedFuncInfo.Address) - Cardinal(FMemory.CurrentData) - SizeOf(TPointer));
    FMemory.Write(RelAddr);

    GenerateStopCode;
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.GenerateSaveResult;
Var
    TypeInfo : TTypeInfo;
Begin
    TypeInfo := FFuncInfo.ResultType;
    Case TypeInfo.Kind Of
        tkShortInt, tkSmallInt, tkInteger, tkByte, tkWord, tkCardinal, tkBoolean,
        tkWordBool, tkLongBool, tkChar, tkWideChar, tkEnum, tkPointer, tkClass, tkObject :
        Begin
            FMemory.Write(Instructions.cSaveRegisterEaxAbsolute);
            FMemory.Write(FResultAddr);
        End;
        tkInt64, tkUInt64 :
        Begin
            FMemory.Write(Instructions.cSaveRegisterEaxAbsolute);
            FMemory.Write(FResultAddr);
            FMemory.Write(Instructions.cSaveRegisterEdxAbsolute);
            FMemory.Write(Pointer(Cardinal(FResultAddr) + SizeOf(TInteger)));
        End;
        tkSingle,
        tkReal48,
        tkReal,
        tkExtended,
        tkCurrency :
        Begin
            Case TypeInfo.Kind Of
                tkCurrency : FMemory.Write(Instructions.cSaveCurrencySt0Absolute);
                tkReal,
                tkSingle   : FMemory.Write(Instructions.cSaveSingleSt0Absolute);
                tkReal48   : FMemory.Write(Instructions.cSaveDoubleSt0Absolute);
                tkExtended : FMemory.Write(Instructions.cSaveExtendedSt0Absolute);
            Else
                RaiseInternalError(cInvalidFloatNumberSize);
            End;
            FMemory.Write(FResultAddr);
            FMemory.Write(Instructions.cFWait);
        End;
        tkPString, tkLString, tkWString :
            ; // We don't need to copy result because function will save string to proper address
        tkSet :
            If TypeInfo.DataSize <= SizeOf(TInteger) Then
            Begin
                FMemory.Write(Instructions.cSaveRegisterEaxAbsolute);
                FMemory.Write(FResultAddr);
            End;
        tkStructure :
            ; //TODO: Later we need to decide how to pass result of struct type depending on size (Delphi) or always as out parameter (FPC)
        tkArray :
            ; // We don't need to copy result because we pass address of result into function
        tkVoid :
            ; // We don't need to save result of procedures
    Else
        RaiseInternalError(cUnsupportedDataType);
        //TODO: Add check if this is procedure
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.GenerateRetCode;
Begin
    FMemory.Write(Instructions.cRet);
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.GenerateStopCode;
Begin
    FMemory.CheckFreeSpace(cMaxStopCodeSize);
    FMemory.Write(Instructions.cBreakpointOpCode);

    //FEvalBreakpoint := FMemory.CurrentData;
End;
{..............................................................................}

{..............................................................................}
Function TEvaluator.GetResult : Variant;
Var
    CalculateData : TCalculateData;
Begin
    If FFuncInfo.ResultType.Kind <> tkVoid Then
    Begin
        Result := GetValue(FDebuger, FFuncInfo.ResultType, FResultAddr, True, False);
        // Function always return value so we can't assign to it
        CalculateData.BriefMode := False;
        CalculateData.DebugInfo := FFuncInfo.UnitInfo.DebugInfo;
        RequireValue(CalculateData, Result);
    End
    Else
        TVarData(Result).VType := varUnknown;
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.InitMemory;
Begin
    FMemory := TDebugerMemory.Create(FDebuger);
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.InitResultMemory(TypeInfo : TTypeInfo; Address : TPointer);
Var
    I      : Integer;
    Member : TStructMember;
Begin
    Case TypeInfo.Kind Of
        tkLString,
        tkWString :
            Begin
                // For AnsiString and WideString we need to clear memory
                ZeroString(FDebuger, Address);
            End;
        tkStructure :
            Begin
                For I := 0 To TypeInfo.Members.Count - 1 Do
                Begin
                    Member := TStructMember(TypeInfo.Members[I]);
                    InitResultMemory(Member.DataType, IncPointer(Address, Member.BitOffset Div 8));
                End;
            End;
        tkArray :
            Begin
                For I := TypeInfo.MinValue To TypeInfo.MaxValue Do
                    InitResultMemory(TypeInfo.BaseType, IncPointer(Address, (I - TypeInfo.MinValue) * TypeInfo.BaseType.DataSize));
            End;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.PassCurrencyParam(Const Value : Variant);
Var
    Data         : PLongWord;
    ValueToWrite : Currency;
Begin
    ValueToWrite := Value;
    Data := @ValueToWrite;
    // Write high DWORD
    Inc(Data);
    FMemory.Write(Instructions.cPushData);
    FMemory.Write(Data^);
    // Write low DWORD
    Dec(Data);
    FMemory.Write(Instructions.cPushData);
    FMemory.Write(Data^);
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.PassDoubleParam(Const Value : Variant);
Var
    Data         : PLongWord;
    ValueToWrite : Double;
Begin
    ValueToWrite := Value;
    Data := @ValueToWrite;
    // Write high DWORD
    Inc(Data);
    FMemory.Write(Instructions.cPushData);
    FMemory.Write(Data^);
    // Write low DWORD
    Dec(Data);
    FMemory.Write(Instructions.cPushData);
    FMemory.Write(Data^);
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.PassExtendedParam(Const Value : Variant);
Var
    Data         : PByte;
    ValueToWrite : Extended;
Begin
    ValueToWrite := Value;
    Data := @ValueToWrite;
    // Write high WORD
    Inc(Data, 8);
    FMemory.Write(Instructions.cPushData);
    FMemory.Write(PWord(Data)^);
    // Write high DWORD
    Dec(Data, 4);
    FMemory.Write(Instructions.cPushData);
    FMemory.Write(PLongWord(Data)^);
    // Write low DWORD
    Dec(Data, 4);
    FMemory.Write(Instructions.cPushData);
    FMemory.Write(PLongWord(Data)^);
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.PassInt64Param(Const Value : Variant);
Var
    Data         : PLongWord;
    ValueToWrite : Int64;
Begin
    ValueToWrite := Value;
    Data := @ValueToWrite;
    // Write high DWORD
    Inc(Data);
    FMemory.Write(Instructions.cPushData);
    FMemory.Write(Data^);
    // Write low DWORD
    Dec(Data);
    FMemory.Write(Instructions.cPushData);
    FMemory.Write(Data^);
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.PassParameters;
Var
    CurrRegIndex : Integer;
    FuncParam    : TVarInfo;
    I            : Integer;
    Param        : TParam;
Begin
    CurrRegIndex := 0;
    I := 0;
    // Real, method-pointer, variant, Int64, and structured types do not qualify as register parameters
    While (FPreparedParams.Count > I) And (CurrRegIndex < cMaxRegParams) Do
    Begin
        Param := FPreparedParams[I];
        FuncParam := Param.FuncParam;
        If Param.Kind = pkValue Then
        Begin
            FMemory.Write(Instructions.cAssignDataToRegister[CurrRegIndex]);
            FMemory.Write(Param.Value);
        End
        Else
        Begin
            // Check if this parameter qualified as register parameter
            If FuncParam.DataType.Kind In [tkSingle, tkReal48, tkReal, tkExtended, tkCurrency, tkInt64, tkUInt64, tkStructure] Then
            Begin
                Inc(I);
                Continue;
            End;
            RaiseInternalError(cParameterDoesNotHaveAssignedValue);
        End;

        Inc(CurrRegIndex);
        FPreparedParams.Delete(I);
    End;

    For I := 0 To FPreparedParams.Count - 1 Do
    Begin
        Param     := FPreparedParams[I];
        FuncParam := Param.FuncParam;
        If Param.Kind = pkValue Then
        Begin
            FMemory.Write(Instructions.cPushData);
            FMemory.Write(Param.Value);
        End
        Else
        Begin
            Case FuncParam.DataType.Kind Of
                tkReal,
                tkSingle: PassSingleParam(Param.VariantValue);
                tkReal48: PassDoubleParam(Param.VariantValue);
                tkExtended: PassExtendedParam(Param.VariantValue);
                tkCurrency: PassCurrencyParam(Param.VariantValue);
                tkInt64, tkUInt64 :
                    PassInt64Param(Param.VariantValue);
            Else
                RaiseInternalError(cUnsupportedDataType);
            End;
        End;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.PassSingleParam(Const Value : Variant);
Var
    Data         : PLongWord;
    ValueToWrite : Single;
Begin
    ValueToWrite := Value;
    Data := @ValueToWrite;
    FMemory.Write(Instructions.cPushData);
    FMemory.Write(Data^);
End;
{..............................................................................}

{..............................................................................}
Procedure TEvaluator.PrepareParameters;
Var
    FuncParam     : TVarInfo;
    FuncSelf      : TVarInfo;
    I             : Integer;
    Param         : TParam;
    ParamsLength  : Integer;
    //TODO: Probably replace TCalculateParams to TList later
    Value         : Variant;
    CalculateData : TCalculateData;
Begin
    ParamsLength := Length(FParams);
    If ParamsLength > FFuncInfo.Params.Count Then
        ExpressionError(cTooManyActualParameters);
    If ParamsLength < FFuncInfo.Params.Count Then
        ExpressionError(cNotEnoughActualParameters);
    FPreparedParams := TObjectList.Create;

    {$MESSAGE WARN 'Add type validation check'}
    For I:= 0 To ParamsLength - 1 Do
    Begin
        FuncParam := TVarInfo(FFuncInfo.Params[I]);
        Param := TParam.Create;
        Try
            Param.FuncParam := FuncParam;
            Value := FParams[I];
            If FuncParam.ByRef Then
            Begin
                If Not TryGetRefValueAddress(Value, Param.Value) Then
                    // Parameter passed by reference but we don't have address
                    If FuncParam.DataType.Name = 'ShortString' Then
                    Begin
                        //TODO: We need to fix compiler so it will generate if this is by val or by ref
                        AllocateParam(Param, Value, FuncParam);
                    End
                    Else
                        ExpressionError(cCannotEvaluateAddress);
                Param.Kind := pkValue;
            End
            Else
            If FuncParam.DataType.Kind In [tkPointer, tkClass] Then
            Begin
                Param.Kind := pkValue;
                TryGetRefValueAddress(Value, Param.Value);
            End
            Else
            Begin
                CalculateData.BriefMode := False;
                CalculateData.DebugInfo := FFuncInfo.UnitInfo.DebugInfo;
                RequireValue(CalculateData, Value);
                If AllocateParam(Param, Value, FuncParam) Then
                    Param.Kind := pkValue
                Else
                Begin
                    Param.VariantValue := Value;
                    Param.Kind := pkVariant;
                End;
            End;
        Except
            FreeAndNil(Param);
            Raise;
        End;
        FPreparedParams.Add(Param);
    End;

    If (FSelfAddress <> Nil) Then
    Begin
        FuncSelf := FFuncInfo.FindVarByName(cSelf);
        if Assigned(FuncSelf) then
        begin
            Param := TParam.Create;
            Param.Kind := pkValue;

            If FuncSelf.DataType.Kind = tkVoid Then
                Param.Value := ReadAddressValue(FDebuger, FSelfAddress)
            Else
                Param.Value := FSelfAddress;

            FPreparedParams.Insert(0, Param);
        end;
    End;
End;
{..............................................................................}

{..............................................................................}
Function EvaluateFunction(Debuger: TDebuger; SelfAddress : TPointer; FuncInfo : TFuncInfo;
    Const Params : TCalculateParams; Const TimeOut : Cardinal = cDefTimeOut) : Variant;
Var
    Evaluator : TEvaluator;
Begin
    Evaluator := TEvaluator.Create(Debuger);
    Try
        Result := Evaluator.Evaluate(SelfAddress, FuncInfo, Params, TimeOut);
    Finally
        FreeAndNil(Evaluator);
    End;
End;
{..............................................................................}

{..............................................................................}
{ TDebugerMemory }

procedure TDebugerMemory.CheckFreeSpace(const Size: TSize);
Const
    cNotEnoughData = 'Not enough data allocated';
begin
    If FDataLeft < Size Then
        RaiseInternalError(cNotEnoughData);
end;

constructor TDebugerMemory.Create(Debuger: TDebuger);
begin
    Inherited Create;

    FDebuger := Debuger;
    FMemory := Debuger.AllocMem(cDebuggeeMemorySize);
    FCurrentData := FMemory;
    FDataLeft := cDebuggeeMemorySize;
end;

destructor TDebugerMemory.Destroy;
begin
    If FMemory <> Nil Then
    Begin
       FDebuger.FreeMem(FMemory);
       FMemory := Nil;
    End;

    Inherited;
end;

procedure TDebugerMemory.Reset;
begin
    FCurrentData := FMemory;
    FDataLeft    := cDebuggeeMemorySize;
end;

procedure TDebugerMemory.Skip(const Size: TSize);
begin
    CheckFreeSpace(Size);
    FCurrentData := IncPointer(FCurrentData, Size);
    Dec(FDataLeft, Size);
end;

procedure TDebugerMemory.Write(const Data: Byte);
begin
    CheckFreeSpace(SizeOf(Data));
    FDebuger.WriteData(@Data, FCurrentData, SizeOf(Data));
    FCurrentData := IncPointer(FCurrentData, SizeOf(Data));
end;

procedure TDebugerMemory.Write(const Data: TPointer);
begin
    CheckFreeSpace(SizeOf(Data));
    FDebuger.WriteData(@Data, FCurrentData, SizeOf(Data));
    FCurrentData := IncPointer(FCurrentData, SizeOf(Data));
end;

procedure TDebugerMemory.Write(const Data: array of Byte);
begin
    CheckFreeSpace(Length(Data));
    FDebuger.WriteData(@Data[0], FCurrentData, Length(Data));
    FCurrentData := IncPointer(FCurrentData, Length(Data));
end;

end.
