Unit EvaluateProcs;

Interface

Uses
    Classes,
    //StringsHash,
    //Platform.Win32,
    //DebugInfoInterfaces,
    //DebuggeeControl,
    DebugInfo,
    Debuger,
    EvaluateTypes;
{..............................................................................}
Type
    TMakeFunction = Function(Const ClassName, MethodName : AnsiString) : AnsiString Of Object;
{..............................................................................}

{..............................................................................}
Function ReadAddressValue    (Debuger: TDebuger; Address : TPointer) : TPointer;
Function ReadIntegerValue    (Debuger: TDebuger; TypeInfo : TTypeInfo; Address : TPointer) : Variant; Overload;
Function ReadInt64Value      (Debuger: TDebuger; TypeInfo : TTypeInfo; Address : TPointer) : Variant;
Function ReadFloatValue      (Debuger: TDebuger; TypeInfo : TTypeInfo; Address : TPointer) : Variant;
Function ReadShortStringValue(Debuger: TDebuger; Address : TPointer) : AnsiString;
Function ReadAnsiStringValue (Debuger: TDebuger; Address : TPointer; BriefMode : Boolean) : AnsiString;
Function ReadWideStringValue (Debuger: TDebuger; Address : TPointer; BriefMode : Boolean) : WideString;

Procedure WriteIntegerValue  (Debuger: TDebuger; TypeInfo : TTypeInfo; Address : TPointer; Const Data : Variant);
Procedure WriteInt64Value    (Debuger: TDebuger; TypeInfo : TTypeInfo; Address : TPointer; Const Data : Variant);
Procedure WriteFloatValue    (Debuger: TDebuger; TypeInfo : TTypeInfo; Address : TPointer; Const Data : Variant);

Function GetValueRef        (Debuger: TDebuger; TypeInfo : TTypeInfo; Address : TPointer) : Variant;
Function GetValueNonRef     (Debuger: TDebuger; TypeInfo : TTypeInfo; IntPtr : TUIntPtr; BriefMode : Boolean) : Variant;
Function GetValue           (Debuger: TDebuger; TypeInfo : TTypeInfo; IntPtr : TUIntPtr; IsPointer : Boolean; BriefMode : Boolean) : Variant;
Function GetVoidValue       : Variant;
Function StructToString     (TypeInfo : TTypeInfo; Address : TPointer; Var ToStringData : TToStringData) : String;
Function VariantToString    (Const Value : Variant; ToStringData : TToStringData) : String;
Function CalculateValue     (Const Value : Variant; CalculateData : TCalculateData) : Variant;
Function ModifyValue        (Const ToValue, FromValue : Variant; CalculateData : TCalculateData) : Boolean;
{..............................................................................}

{..............................................................................}
Function EvaluateVariable(Debuger: TDebuger; VarInfo : TVarInfo; EBP : TPointer; BriefMode : Boolean) : Variant;

Function TryGetConstant(UnitInfo : TUnitInfo; Const ConstName : AnsiString; Out Value : Variant) : Boolean; Overload;
Function TryGetConstant(FuncInfo : TFuncInfo; Const ConstName : AnsiString; Out Value : Variant) : Boolean; Overload;

Function TryGetEnumValue(UnitInfo : TUnitInfo; Const VarName : AnsiString; Out Value : Variant) : Boolean;

Function TryGetVariable(Debuger: TDebuger; UnitInfo : TUnitInfo; Const VarName : AnsiString; Out Value : Variant; BriefMode : Boolean) : Boolean; Overload;
Function TryGetVariable(Debuger: TDebuger; Func : TFuncInfo; Const VarName : AnsiString; Out Value : Variant; BriefMode : Boolean) : Boolean; Overload;

Function TryGetFuncInfo(UnitInfo : TUnitInfo; Const FuncName : AnsiString; Out FuncInfo : TFuncInfo) : Boolean; Overload;
Function TryGetFuncInfo(Funcs : TStringList; Const FuncName : AnsiString; Out FuncInfo : TFuncInfo) : Boolean; Overload;
Function TryGetFunction(UnitInfo : TUnitInfo; Const FuncName : AnsiString; Out Value : Variant) : Boolean; Overload;
Function TryGetFunction(Funcs : TStringList; Const FuncName : AnsiString; Out Value : Variant) : Boolean; Overload;

Function TryGetSelf(Debuger: TDebuger; FuncInfo : TFuncInfo; Out Value : Variant) : Boolean;
Function TryEvaluateClassField(Debuger: TDebuger; FuncInfo : TFuncInfo; Const FieldName : AnsiString; Out Value : Variant; BriefMode : Boolean) : Boolean;
Function TryEvaluateClassMethod(Debuger: TDebuger; MakeFunction : TMakeFunction;
         ParentFuncInfo : TFuncInfo; Const MethodName : AnsiString; Out Value : Variant) : Boolean;
Function TryGetRefValueAddress(Const Value : Variant; Out Address : TPointer) : Boolean;
Function TryGetValueAddress(Const Value : Variant; Out Address : TPointer) : Boolean;
{..............................................................................}

{..............................................................................}
Procedure RequireValue(CalculateData : TCalculateData; Var Value : Variant);
Function  RequireOrdinalValue(Const Value : Variant) : TOrdinalValue;
Procedure FinishCalculate(CalculateData : TCalculateData; Var Value : Variant);
Procedure ExpressionError(Const Err : String);
{..............................................................................}

{..............................................................................}
Function  ConvertToInteger(Const Value : Variant) : Integer;
Function  ConvertToType   (Const Value : Variant; TypeInfo : TTypeInfo) : Variant;
{..............................................................................}

function IncPointer(Ptr: Pointer; Offset: Integer): Pointer;

Procedure RaiseInternalError(Const Message : String);

{..............................................................................}
Implementation

Uses
    Windows,
    SysUtils,
    Variants,
    //System.Strings,
    //System.Debug,
    EvaluateConsts,
    //APIConsts,
    Math;
{..............................................................................}

{..............................................................................}
Const
    cMaxLongStringLen = 1024;
{..............................................................................}

{..............................................................................}
Var
    gv_VoidValue : Variant;
{..............................................................................}

type
  EEvaluateInternalError = class(Exception);

Procedure RaiseInternalError(Const Message : String);
begin
  raise EEvaluateInternalError.Create(Message);
end;

function IncPointer(Ptr: Pointer; Offset: Integer): Pointer;
begin
  Result := Pointer(Integer(Ptr) + Offset);
end;

{..............................................................................}
Procedure SetTrailDots(Var S : AnsiString); Overload;
Var
    I, L : Integer;
Begin
    L := Length(S);
    If L > 2 Then
        For I := L - 2 To L Do
            S[I] := '.';
End;
{..............................................................................}

{..............................................................................}
Procedure SetTrailDots(Var S : WideString); Overload;
Var
    I, L : Integer;
Begin
    L := Length(S);
    If L > 2 Then
        For I := L - 2 To L Do
            S[I] := '.';
End;
{..............................................................................}

{..............................................................................}
Function ReadAddressValue(Debuger: TDebuger; Address : TPointer) : TPointer;
Begin
    Debuger.ReadData(Address, @Result, SizeOf(Result));
End;
{...............................................................................}

{...............................................................................}
Function GetIntValue(TypeInfo : TTypeInfo; Value : TInteger) : Variant; Overload;
Begin
    if TypeInfo = Nil Then
        Result := PLongWord(@Value)^
    Else
        Case TypeInfo.Kind Of
            tkShortInt : Result := PShortInt(@Value)^;
            tkByte     : Result := PByte(@Value)^;
            tkSmallInt : Result := PSmallInt(@Value)^;
            tkWord     : Result := PWord(@Value)^;
            tkInteger  : Result := PLongInt(@Value)^;
            tkCardinal : Result := PLongWord(@Value)^;
            tkChar     : Result := PAnsiChar(@Value)^;
            tkWideChar : Result := PWideChar(@Value)^;
            tkBoolean  : Result := PBoolean(@Value)^;
            tkWordBool : Result := PWordBool(@Value)^;
            tkLongBool : Result := PLongBool(@Value)^;
            tkEnum     :
                Case TypeInfo.DataSize Of
                    1 : Result := PByte(@Value)^;
                    2 : Result := PWord(@Value)^;
                    4 : Result := PLongWord(@Value)^;
                Else
                    RaiseInternalError(cUnsupportedDataType);
                End;
        End;
End;
{...............................................................................}

{...............................................................................}
Function GetIntSize(TypeInfo : TTypeInfo) : Integer;
Begin
    Result := SizeOf(Integer);

    If TypeInfo <> Nil Then
        Case TypeInfo.Kind Of
            tkShortInt : Result := SizeOf(ShortInt);
            tkByte     : Result := SizeOf(Byte);
            tkSmallInt : Result := SizeOf(SmallInt);
            tkWord     : Result := SizeOf(Word);
            tkInteger  : Result := SizeOf(LongInt);
            tkCardinal : Result := SizeOf(LongWord);
            tkChar     : Result := SizeOf(AnsiChar);
            tkWideChar : Result := SizeOf(WideChar);
            tkBoolean  : Result := SizeOf(Boolean);
            tkWordBool : Result := SizeOf(WordBool);
            tkLongBool : Result := SizeOf(LongBool);
            tkEnum     :
                Case TypeInfo.DataSize Of
                    1 : Result := SizeOf(Byte);
                    2 : Result := SizeOf(Word);
                    4 : Result := SizeOf(LongWord);
                Else
                    RaiseInternalError(cUnsupportedDataType);
                End;
        End;
End;
{...............................................................................}

{..............................................................................}
Function ReadIntegerValue(Debuger: TDebuger; TypeInfo : TTypeInfo; Address : TPointer) : Variant; Overload;
Var
    Value : TInteger;
Begin
    Debuger.ReadData(Address, @Value, SizeOf(Value));
    Result := GetIntValue(TypeInfo, Value);
End;
{...............................................................................}

{...............................................................................}
Procedure WriteIntegerValue(Debuger: TDebuger; TypeInfo : TTypeInfo; Address : TPointer; Const Data : Variant);
Begin
    If VarIsOrdinal(Data) Then
        Debuger.WriteData(@TVarData(Data).VInteger, Address, GetIntSize(TypeInfo))
    Else
        RaiseInternalError('WriteIntegerValue');
End;
{...............................................................................}

{...............................................................................}
Function ReadInt64Value(Debuger: TDebuger; TypeInfo : TTypeInfo; Address : TPointer) : Variant;
Var
    Data : Int64;
Begin
    Debuger.ReadData(Address, @Data, SizeOf(Data));
    if (TypeInfo = Nil) Or (TypeInfo.Kind <> tkUInt64) Then
        Result := Data
    Else
        Result := UInt64(Data);
End;
{..............................................................................}

{..............................................................................}
Procedure WriteInt64Value(Debuger: TDebuger; TypeInfo: TTypeInfo; Address: TPointer; Const Data: Variant);
Begin
    If VarIsType(Data, [varInt64, varUInt64]) Then
        Debuger.WriteData(@TVarData(Data).VInt64, Address, SizeOf(Int64))
    Else
        RaiseInternalError('WriteInt64Value');

End;
{..............................................................................}

{..............................................................................}
Function ReadFloatValueSingle(Debuger: TDebuger; Address : TPointer) : Single;
Begin
    Debuger.ReadData(Address, @Result, SizeOf(Result));
End;
{..............................................................................}

{..............................................................................}
Function ReadFloatValueReal48(Debuger: TDebuger; Address : TPointer) : Real48;
Begin
    Debuger.ReadData(Address, @Result, SizeOf(Result));
End;
{..............................................................................}

{..............................................................................}
Function ReadFloatValueDouble(Debuger: TDebuger; Address : TPointer) : Double;
Begin
    Debuger.ReadData(Address, @Result, SizeOf(Result));
End;
{..............................................................................}

{..............................................................................}
Function ReadFloatValueExtended(Debuger: TDebuger; Address : TPointer) : Extended;
Begin
    Debuger.ReadData(Address, @Result, SizeOf(Result));
End;
{..............................................................................}

{..............................................................................}
Function ReadFloatValueCurrency(Debuger: TDebuger; Address : TPointer) : Currency;
Begin
    Debuger.ReadData(Address, @Result, SizeOf(Result));
End;
{..............................................................................}

{..............................................................................}
Function ReadFloatValue(Debuger: TDebuger; TypeInfo : TTypeInfo; Address : TPointer) : Variant;
Begin
    Case TypeInfo.DataSize Of
        -8 : Result := ReadFloatValueCurrency(Debuger, Address);
         4 : Result := ReadFloatValueSingle  (Debuger, Address);
         6 : Result := ReadFloatValueReal48  (Debuger, Address);
         8 : Result := ReadFloatValueDouble  (Debuger, Address);
        10 : Result := ReadFloatValueExtended(Debuger, Address);
    Else
        Result := 0;
        RaiseInternalError('ReadFloatValue');
    End;
End;
{...............................................................................}

{...............................................................................}
Function GetFloatSize(TypeInfo : TTypeInfo) : Integer;
Begin
    Case TypeInfo.DataSize Of
        -8 : Result := SizeOf(Currency);
         4 : Result := SizeOf(Single);
         6 : Result := SizeOf(Real48);
         8 : Result := SizeOf(Double);
        10 : Result := SizeOf(Extended);
    Else
        Result := 0;
        RaiseInternalError('GetFloatSize');
    End;
End;
{...............................................................................}

{...............................................................................}
Procedure WriteFloatValue(Debuger: TDebuger; TypeInfo: TTypeInfo; Address: TPointer; Const Data: Variant);
Begin
    If VarIsType(Data, [varSingle, varDouble, varCurrency]) Then
        Debuger.WriteData(@TVarData(Data).VDouble, Address, GetFloatSize(TypeInfo))
    Else
        RaiseInternalError('WriteFloatValue');
End;
{...............................................................................}

{...............................................................................}
Function ReadShortStringValue(Debuger: TDebuger; Address : TPointer) : AnsiString;
Var
    Len : Byte;
Begin
    Debuger.ReadData(Address, @Len, 1);
    SetLength(Result, Len);
    Debuger.ReadData(Pointer(Cardinal(Address) + 1), Pointer(Result), Len);
End;
{..............................................................................}

{..............................................................................}
Function ReadAnsiStringValue(Debuger: TDebuger; Address : TPointer; BriefMode : Boolean) : AnsiString;
Var
    Len : Integer;
Begin
    If Address = nil Then
        Result := ''
    Else
    Begin
        Debuger.ReadData(Pointer(Cardinal(Address) - 4), @Len, 4);

        If BriefMode And (Len > cMaxLongStringLen) Then
            Len := cMaxLongStringLen;

        SetLength(Result, Len);
        Debuger.ReadData(Address, Pointer(Result), Len);

        If BriefMode And (Len = cMaxLongStringLen) Then
            SetTrailDots(Result);
    End;
End;
{...............................................................................}

{...............................................................................}
Function ReadWideStringValue(Debuger: TDebuger; Address : TPointer; BriefMode : Boolean) : WideString;
Var
    Len : Integer;
Begin
    If Address = nil Then
        Result := ''
    Else
    Begin
        Debuger.ReadData(Pointer(Cardinal(Address) - 4), @Len, 4);

        If BriefMode And (Len > cMaxLongStringLen) Then
            Len := cMaxLongStringLen;

        SetLength(Result, Len Div SizeOf(WideChar));
        Debuger.ReadData(Address, Pointer(Result), Len);

        If BriefMode And (Len = cMaxLongStringLen) Then
            SetTrailDots(Result);
    End;
End;
{..............................................................................}

{...............................................................................}
Function GetShortStringValue(Debuger: TDebuger; Address : TPointer) : AnsiString;
Begin
    Result := ReadShortStringValue(Debuger, Address);
End;
{...............................................................................}

{...............................................................................}
Function GetAnsiStringValue(Debuger: TDebuger; Address : TPointer; BriefMode : Boolean) : AnsiString;
Begin
    Result := ReadAnsiStringValue(Debuger, Address, BriefMode);
End;
{...............................................................................}

{...............................................................................}
Function GetWideStringValue(Debuger: TDebuger; Address : TPointer; BriefMode : Boolean) : WideString;
Begin
    Result := ReadWideStringValue(Debuger, Address, BriefMode);
End;
{..............................................................................}

{..............................................................................}
Function GetEnumValue(TypeInfo : TTypeInfo; Value : TUInteger) : Variant;
Begin
    Result := TEnumVariantValue.Create(TypeInfo, Value) As IUnknown;
End;
{..............................................................................}

{..............................................................................}
Function GetSetValue(TypeInfo : TTypeInfo; Value : TUInteger) : Variant;
Begin
    Result := TSetVariantValue.Create(TypeInfo, Pointer(Value)) As IUnknown;
End;
{..............................................................................}

{..............................................................................}
Function GetStructValue(TypeInfo : TTypeInfo; Address : TPointer) : Variant;
Begin
    Result := TStructVariantValue.Create(TypeInfo, Address) As IUnknown;
End;
{..............................................................................}

{..............................................................................}
Function GetArrayValue(TypeInfo : TTypeInfo; Address : TPointer) : Variant;
Begin
    Result := TArrayVariantValue.Create(TypeInfo, Address) As IUnknown;
End;
{..............................................................................}

{..............................................................................}
Function GetPointerValue(TypeInfo : TTypeInfo; Address : TPointer) : Variant;
Begin
    Result := TPointerVariantValue.Create(TypeInfo, Address) As IUnknown;
End;
{..............................................................................}

{..............................................................................}
Function GetFunctionValue(SelfAddress : TPointer; FuncInfo : TFuncInfo) : Variant;
Begin
    Result := TFunctionVariantValue.Create(SelfAddress, FuncInfo) As IUnknown;
End;
{..............................................................................}

{..............................................................................}
Function GetObjectValue(TypeInfo : TTypeInfo; Address : TPointer) : Variant;
Begin
    Result := TObjectVariantValue.Create(TypeInfo, Address) As IUnknown;
End;
{..............................................................................}

{..............................................................................}
Function CreateReference(ReferenceClass : TCustomReferenceVariantDataClass; TypeInfo : TTypeInfo; Address : TPointer) : Variant;
Begin
    Result := ReferenceClass.Create(TypeInfo, Address) As IUnknown
End;
{..............................................................................}

{..............................................................................}
Function GetValueRef(Debuger: TDebuger; TypeInfo : TTypeInfo; Address : TPointer) : Variant;
Begin
    If TypeInfo = Nil Then
        Result := CreateReference(TPointerRefVariantValue, Nil, Address)
    Else
        Case TypeInfo.Kind Of
            tkShortInt,
            tkSmallInt,
            tkInteger,
            tkByte,
            tkWord,
            tkCardinal,
            tkChar,
            tkWideChar,
            tkBoolean,
            tkWordBool,
            tkLongBool     : Result := CreateReference(TIntegerRefVariantValue, TypeInfo, Address);
            tkUInt64,
            tkInt64        : Result := CreateReference(TInt64RefVariantValue, TypeInfo, Address);
            tkSingle,
            tkReal48,
            tkReal,
            tkExtended,
            tkCurrency     : Result := CreateReference(TFloatRefVariantValue, TypeInfo, Address);
            tkPString      : Result := CreateReference(TShortStringRefVariantValue, TypeInfo, Address);
            tkLString      : Result := CreateReference(TAnsiStringRefVariantValue, TypeInfo, Address);
            tkWString      : Result := CreateReference(TWideStringRefVariantValue, TypeInfo, Address);
            tkEnum         : Result := CreateReference(TEnumRefVariantValue, TypeInfo, Address);
            tkSet          : Result := CreateReference(TSetRefVariantValue, TypeInfo, Address);
            tkStructure    : Result := CreateReference(TStructRefVariantValue, TypeInfo, Address);
            tkArray,
            tkDynamicArray : Result := CreateReference(TArrayRefVariantValue, TypeInfo, Address);
            tkPointer      : Result := CreateReference(TPointerRefVariantValue, TypeInfo, Address);
            tkObject       : Result := CreateReference(TObjectRefVariantValue, TypeInfo, Address);
            tkClass        : Result := CreateReference(TPointerRefVariantValue, Nil, Address);
            tkVoid         : Result := GetVoidValue;
        Else
            RaiseInternalError(cUnsupportedDataType);
        End;
End;
{..............................................................................}

{..............................................................................}
// This called when IntPtr comes directly from register and contains value of object
// IntPtr is address for strings, for everything else it is address
Function GetValueNonRef(Debuger: TDebuger; TypeInfo : TTypeInfo; IntPtr : TUIntPtr; BriefMode : Boolean) : Variant;
Begin
    If TypeInfo = Nil Then
        Result := GetPointerValue(Nil, IntPtr)
    Else
        Case TypeInfo.Kind Of
            tkShortInt,
            tkSmallInt,
            tkInteger,
            tkByte,
            tkWord,
            tkCardinal,
            tkChar,
            tkWideChar,
            tkBoolean,
            tkWordBool,
            tkLongBool    : Result := GetIntValue(TypeInfo, Integer(IntPtr));
            tkUInt64,
            tkInt64       : RaiseInternalError(cUnsupportedDataType);
            tkSingle,
            tkReal48,
            tkReal,
            tkExtended,
            tkCurrency    : RaiseInternalError(cUnsupportedDataType);
            tkPString     : Result := GetShortStringValue(Debuger, IntPtr);
            tkLString     : Result := GetAnsiStringValue (Debuger, IntPtr, BriefMode);
            tkWString     : Result := GetWideStringValue (Debuger, IntPtr, BriefMode);
            tkEnum        : Result := GetEnumValue       (TypeInfo, Cardinal(IntPtr));
            tkSet         : Result := GetSetValue        (TypeInfo, Cardinal(IntPtr));
            tkStructure   : Result := GetStructValue     (TypeInfo, IntPtr);
            tkArray       : Result := GetArrayValue      (TypeInfo, IntPtr);
            tkPointer     : Result := GetPointerValue    (TypeInfo, IntPtr);
            tkObject      : Result := GetObjectValue     (TypeInfo, IntPtr);
            tkClass       : Result := GetPointerValue    (Nil     , IntPtr);
            tkVoid        : Result := GetVoidValue;
        Else
            RaiseInternalError(cUnsupportedDataType);
        End;
End;
{..............................................................................}

{..............................................................................}
Function GetValue(Debuger: TDebuger; TypeInfo : TTypeInfo; IntPtr : TUIntPtr; IsPointer : Boolean; BriefMode : Boolean) : Variant;
Begin
    If Debuger.IsValidAddr(IntPtr) Then
    Begin
        If IsPointer Then
            Result := GetValueRef(Debuger, TypeInfo, IntPtr)
        Else
            Result := GetValueNonRef(Debuger, TypeInfo, IntPtr, BriefMode);
    End
    Else
        RaiseInternalError(cCannotEvaluateAddress);
End;
{..............................................................................}

{..............................................................................}
Function GetVoidValue : Variant;
Begin
    If VarIsEmpty(gv_VoidValue) Then
        gv_VoidValue := TVoidVariantValue.Create As IUnknown;
    Result := gv_VoidValue;
End;
{..............................................................................}

{..............................................................................}
Function StructToString(TypeInfo : TTypeInfo; Address : TPointer; Var ToStringData : TToStringData) : String;
Var
    I            : Integer;
    Member       : TStructMember;
    MemberName   : String;
    MemberValue  : String;
    MemberAddr   : TUIntPtr;
    TotalMembers : Integer;
    Value        : Variant;
    BriefMode    : Boolean;

    Function GetBriefStructInfo(StructTypeInfo : TTypeInfo; StructAddress : TPointer) : String;
    Var
        StructTypeInfoName : String;
    Begin
        While (StructTypeInfo.Name = '') And (StructTypeInfo.BaseType <> Nil) Do
            StructTypeInfo := StructTypeInfo.BaseType;

        StructTypeInfoName := String(StructTypeInfo.Name);
        If StructTypeInfoName = '' Then
        Begin
            Case StructTypeInfo.Kind Of
                tkClass   : StructTypeInfoName := 'TClass';
                tkObject  : StructTypeInfoName := 'TObject';
                tkPointer : StructTypeInfoName := 'Pointer';
            End;
        End;

        Result := Format('Pointer($%.8x) as %s', [StructAddress, StructTypeInfoName]);
    End;

Begin

    If (ToStringData.Mode = tsmBrief) Or (ToStringData.RecursionLevel <= 0) Then
    Begin
        Result := GetBriefStructInfo(TypeInfo, Address);
        Exit;
    End;

    Result := '';

    Dec(ToStringData.RecursionLevel);
    Try
        BriefMode := (ToStringData.RecursionLevel <= 0);

        While TypeInfo <> Nil Do
        Begin
            TotalMembers := 0;
            If TypeInfo.Kind = tkPointer Then
                TypeInfo := TypeInfo.BaseType;

            If TypeInfo.Members <> Nil Then
            Begin
                For I := 0 To TypeInfo.Members.Count - 1 Do
                Begin
                    MemberName := TypeInfo.Members[I];
                    If (MemberName = '') Or Not CharInSet(MemberName[1], ['a'..'z', 'A'..'Z']) Then
                        Continue;

                    Member := TStructMember(TypeInfo.Members.Objects[I]);

                    If (Member = Nil) Or (Member.Alias <> '') Or (Member.MethodName <> '') Then
                        Continue;

                    Try
                        If Member.DataType <> Nil Then
                        Begin
                            MemberAddr := Pointer(Cardinal(Address) + (TUInteger(Member.BitOffset) Div 8));
                            Value := GetValue(ToStringData.DebugInfo.Debuger, Member.DataType, MemberAddr, True, BriefMode);
                            MemberValue := VariantToString(Value, ToStringData);
                        End
                        Else MemberValue := cPrefixWarning + '''???'''
                    Except
                        MemberValue := cPrefixError + '''err''';
                    End;

                    If Result <> '' Then
                        Result := Result + '; ';

                    Result := Result + MemberName + ': ' + MemberValue;

                    Inc(TotalMembers);

                    If TotalMembers > cMaxMembers Then
                        Break;
                End;
            End;

            If TotalMembers > cMaxMembers Then
            Begin
                Result := Result + '; [...]';
                Break;
            End;

            TypeInfo := TypeInfo.BaseType;
        End;
    Finally
        Inc(ToStringData.RecursionLevel);
    End;
    Result := '[' + Result + ']';
End;
{..............................................................................}

{..............................................................................}
Function GetStringDisplayValue(Const S : String) : String;
Var
    SLen : Integer;
    I    : Integer;
Begin
    SLen := Length(S);

    If SLen = 0 Then
    Begin
        Result := ''''''; // ''
        Exit;
    End
    Else
        Result := '';

    I := 1;
    Repeat
        If (S[I] >= ' ') And (S[I] <> '''') And (S[I] <> '|') Then
        Begin
            Result := Result + '''';

            Repeat
                Result := Result + S[I];
                Inc(I);
            Until (S[I] < ' ') Or (S[I] = '''') Or (S[I] = '|');

            Result := Result + '''';
        End
        Else
        Begin
            Result := Result + Format('#%d', [Ord(S[I])]);
            Inc(I);
        End;
    Until I > SLen;
End;
{...............................................................................}

{...............................................................................}
Function VariantToString(Const Value : Variant; ToStringData : TToStringData) : String;
Var
    CustomVariantData : ICustomVariantData;
Begin
    Case VarType(Value) Of
        varOleStr,
        varString,
        varUString :
            Result := GetStringDisplayValue(String(Value));
        varUnknown :
            Begin
                If Supports(IUnknown(TVarData(Value).VUnknown), ICustomVariantData, CustomVariantData) Then
                    Result := CustomVariantData.AsString(ToStringData)
                Else
                    Result := 'Unsupported data type';
            End
    Else
        Result := String(Value);
    End;
End;
{..............................................................................}

{..............................................................................}
Function CalculateValue(Const Value : Variant; CalculateData : TCalculateData) : Variant;
Var
    CalculateValue : ICalculateValue;
Begin
    If VarType(Value) = varUnknown Then
        If Supports(IUnknown(TVarData(Value).VUnknown), ICalculateValue, CalculateValue) Then
        Begin
            Result := CalculateValue.Calculate(CalculateData);
            Exit;
        End;

    Result := Value;
End;
{..............................................................................}

{..............................................................................}
Function ModifyValue(Const ToValue, FromValue: Variant; CalculateData: TCalculateData): Boolean;
Var
    ModifyValue : IModifyValue;
Begin
    Result := False;
    If (VarType(ToValue) = varUnknown) And Supports(IUnknown(TVarData(ToValue).VUnknown), IModifyValue, ModifyValue) Then
    Begin
        ModifyValue.Modify(CalculateData, FromValue);
        Result := True;
    End;
End;
{..............................................................................}

{..............................................................................}
Function GetRegisterValue(Debuger: TDebuger; RegisterIndex : Integer) : Integer;
Var
    Context : TContext;
Begin
    Result := 0;
    Context := Debuger.GetRegisters(Debuger.CurThreadId);
    Case RegisterIndex and 7 Of
        0 : Result := Context.EAX;
        1 : Result := Context.ECX;
        2 : Result := Context.EDX;
        3 : Result := Context.EBX;
        4 : Result := Context.ESP;
        5 : Result := Context.EBP;
        6 : Result := Context.ESI;
        7 : Result := Context.EDI;
        8 : Result := Context.EIP;
    End;
    Case RegisterIndex shr 4 Of
        1  : Result := Result and $F;         // AL, BL, CL, DL
        2  : Result := (Result shr 8) and $F; // AH, BH, CH, DH
        3  : Result := Result and $FF;        // AX, BX, CX, DX, SP, BP, SI, DI, IP
    End;
End;
{...............................................................................}

{...............................................................................}
Procedure GetVariableInfo(Debuger: TDebuger; VarInfo : TVarInfo; EBP : TPointer;
    Out IntPtrValue : TUIntPtr; Out IsPointer : Boolean);
Var
    Index : Integer;
    EIP   : Cardinal;
    I     : Integer;
Begin
    Case VarInfo.VarKind Of
        vkGlobal   :
            Begin
                IsPointer   := True;
                IntPtrValue := Pointer(VarInfo.Offset);
            End;
        vkStack    :
            Begin
                IsPointer   := True;
                IntPtrValue := IncPointer(EBP, VarInfo.Offset);
            End;
        vkRegister :
            Begin
                IsPointer   := False;
                Index := VarInfo.Offset;
                If Index = -1 Then
                Begin
                    EIP := Debuger.GetRegisters(Debuger.CurThreadId).Eip;

                    For I := 0 To VarInfo.RegisterRanges.Count - 1 Do
                        With TRegInfo(VarInfo.RegisterRanges[I]) Do
                            If (EIP >= StartOffset) and (EIP <= EndOffset) Then
                            Begin
                                Index := RegisterIndex;
                                Break;
                            End;

                    If Index = -1 Then
                        ExpressionError(cOptimizedValue);
                End;
                IntPtrValue := Pointer(GetRegisterValue(Debuger, Index));
            End;
    Else
        Raise Exception.Create('Invalid var kind');
    End;

    If VarInfo.ByRef Then
    Begin
        If IsPointer Then
            IntPtrValue := ReadAddressValue(Debuger, IntPtrValue)
        Else
            IsPointer := True;
    End;
End;
{..............................................................................}

{..............................................................................}
Function EvaluateVariable(Debuger: TDebuger; VarInfo : TVarInfo; EBP : TPointer; BriefMode : Boolean) : Variant;
Var
    IntPtrValue : TUIntPtr;
    IsPointer   : Boolean;
Begin
    GetVariableInfo(Debuger, VarInfo, EBP, IntPtrValue, IsPointer);
    Result := GetValue(Debuger, VarInfo.DataType, IntPtrValue, IsPointer, BriefMode)
End;
{..............................................................................}

{..............................................................................}
Function TryGetConstant(UnitInfo : TUnitInfo; Const ConstName : AnsiString; Out Value : Variant) : Boolean;
Var
    I : Integer;
    C : TConstInfo;
    UInfo: TUnitInfo;
Begin
    C := UnitInfo.FindConstByName(String(ConstName));
    if C = Nil then
    Begin
        For I := UnitInfo.UsedUnits.Count - 1 downto 0 do
        Begin
            UInfo := TUnitInfo(UnitInfo.UsedUnits.Objects[I]);
            C := UInfo.FindConstByName(String(ConstName));
            if Assigned(C) then
              Break;
        End;
    End;

    Result := Assigned(C);
    If Result Then
        Value := C.Value;
End;
{..............................................................................}

{..............................................................................}
Function TryGetConstant(FuncInfo : TFuncInfo; Const ConstName : AnsiString; Out Value : Variant) : Boolean;
Var
    Index   : Integer;
    TryFunc : TFuncInfo;
    C: TConstInfo;
Begin
    TryFunc := FuncInfo;
    Repeat
        C := TryFunc.FindConstByName(ConstName);
        If C = Nil Then
            TryFunc := TryFunc.Parent;
    Until Assigned(C) Or (TryFunc = Nil);

    Result := Assigned(C);
    If Result Then
        Value := C.Value;
End;
{..............................................................................}

{..............................................................................}
Function FindGetEnumValue(Types : TList; Const VarName : AnsiString; Out Value : Variant) : Boolean;
Var
    I        : Integer;
    Index    : Integer;
    TypeInfo : TTypeInfo;
Begin
    For I := 0 To Types.Count - 1 Do
    Begin
        TypeInfo := Types[I];
        If (TypeInfo <> Nil) And (TypeInfo.Kind = tkEnum) And (TypeInfo.Elements <> Nil) Then
        Begin
            Index := TypeInfo.Elements.IndexOf(String(VarName));
            If Index >= 0 Then
            Begin
                Value := TEnumVariantValue.Create(TypeInfo, TInteger(TypeInfo.Elements.Objects[Index])) As IUnknown;
                Result := True;
                Exit;
            End;
        End;
    End;
    Result := False;
End;
{..............................................................................}

{..............................................................................}
Function TryGetEnumValue(UnitInfo : TUnitInfo; Const VarName : AnsiString; Out Value : Variant) : Boolean;
Var
    I : Integer;
Begin
    Result := FindGetEnumValue(UnitInfo.Types, VarName, Value);
    I := UnitInfo.UsedUnits.Count - 1;
    While (Not Result) And (I > -1) Do
    Begin
        Result := FindGetEnumValue(TUnitInfo(UnitInfo.UsedUnits.Objects[I]).Types, VarName, Value);
        Dec(I);
    End;
End;
{..............................................................................}

{..............................................................................}
Function GetVariable(Debuger: TDebuger; VarInfo : TVarInfo; EBP : TPointer; BriefMode : Boolean) : Variant;
Var
    IntPtrValue : TUIntPtr;
    IsPointer   : Boolean;
Begin
    GetVariableInfo(Debuger, VarInfo, EBP, IntPtrValue, IsPointer);
    Result := GetValue(Debuger, VarInfo.DataType, IntPtrValue, IsPointer, BriefMode);
End;
{..............................................................................}

{..............................................................................}
Function TryGetParentFunc(Debuger: TDebuger; Var Func : TFuncInfo; Var EBP : Pointer) : Boolean;
Var
    I : Integer;
    VarInfo: TVarInfo;
Begin
    Result := False;
    For I := 0 To Func.Vars.Count - 1 Do
    Begin
        VarInfo := TVarInfo(Func.Vars[I]);
        Result := (VarInfo.VarKind = vkLink) And (Func.Parent <> Nil);
        If Result Then
        Begin
            EBP := ReadAddressValue(Debuger, IncPointer(EBP, VarInfo.Offset));
            Func := Func.Parent;
            Exit;
        End;
    End;
End;
{..............................................................................}

{..............................................................................}
Function TryGetVariable(Debuger: TDebuger; UnitInfo : TUnitInfo; Const VarName : AnsiString; Out Value : Variant; BriefMode : Boolean) : Boolean;
Var
    I : Integer;
    VarInfo : TVarInfo;
    UInfo: TUnitInfo;
Begin
    VarInfo := UnitInfo.FindVarByName(String(VarName));
    if VarInfo = Nil then
    begin
        For I := UnitInfo.UsedUnits.Count - 1 downto 0 do
        Begin
            UInfo := TUnitInfo(UnitInfo.UsedUnits.Objects[I]);
            VarInfo := UInfo.FindVarByName(String(VarName));
            if Assigned(VarInfo) then
                Break;
        End;
    end;

    Result := Assigned(VarInfo);
    If Result Then
        Value := GetVariable(Debuger, VarInfo, Pointer(Debuger.GetRegisters(Debuger.CurThreadId).Ebp), BriefMode);
End;
{..............................................................................}

{..............................................................................}
Function TryGetVariable(Debuger: TDebuger; Func : TFuncInfo; Const VarName : AnsiString; Out Value : Variant; BriefMode : Boolean) : Boolean;
Var
    TryFunc : TFuncInfo;
    EBP     : Pointer;
    V: TVarInfo;
    Param: TVarInfo;
Begin
    TryFunc := Func;
    EBP := Pointer(Debuger.GetRegisters(Debuger.CurThreadId).Ebp);
    Repeat
        // Try to find in the parameters
        Param := TVarInfo(TryFunc.Params.FindByName(VarName));
        Result := Assigned(Param);
        If Result Then
        Begin
            Value := GetVariable(Debuger, Param, EBP, BriefMode);
            Exit;
        End;
        // Try to find in the variables
        If TryFunc.Vars <> Nil Then
        Begin
            V := TryFunc.FindVarByName(VarName);
            Result := Assigned(V);
            If Result Then
            Begin
                Value := GetVariable(Debuger, V, EBP, BriefMode);
                Exit;
            End;
        End;
    Until Not TryGetParentFunc(Debuger, TryFunc, EBP);
    Result := False;
End;
{..............................................................................}

{..............................................................................}
Function TryGetFuncInfo(UnitInfo : TUnitInfo; Const FuncName : AnsiString; Out FuncInfo : TFuncInfo) : Boolean;
Var
    I : Integer;
    UInfo: TUnitInfo;
Begin
    FuncInfo := UnitInfo.FindFuncByName(String(FuncName));
    if FuncInfo = Nil then
    begin
        For I := UnitInfo.UsedUnits.Count - 1 downto 0 do
        Begin
            UInfo := TUnitInfo(UnitInfo.UsedUnits.Objects[I]);
            FuncInfo := UInfo.FindFuncByName(String(FuncName));
            if Assigned(FuncInfo) then
              Break;
        End;
    end;

    Result := Assigned(FuncInfo);
End;
{..............................................................................}

{..............................................................................}
Function TryGetFuncInfo(Funcs : TStringList; Const FuncName : AnsiString; Out FuncInfo : TFuncInfo) : Boolean;
var
  Idx: Integer;
Begin
    Result := Funcs.Find(String(FuncName), Idx);

    If Result Then
        FuncInfo := TFuncInfo(Funcs.Objects[Idx]);
End;
{..............................................................................}

{..............................................................................}
Function TryGetFunction(UnitInfo : TUnitInfo; Const FuncName : AnsiString; Out Value : Variant) : Boolean;
Var
    FuncInfo : TFuncInfo;
Begin
    Result := TryGetFuncInfo(UnitInfo, FuncName, FuncInfo);

    If Result Then
        Value := GetFunctionValue(Nil, FuncInfo);
End;
{..............................................................................}

{..............................................................................}
Function TryGetFunction(Funcs : TStringList; Const FuncName : AnsiString; Out Value : Variant) : Boolean;
Var
    FuncInfo : TFuncInfo;
Begin
    Result := TryGetFuncInfo(Funcs, FuncName, FuncInfo);
    If Result Then
        Value := GetFunctionValue(Nil, FuncInfo);
End;
{..............................................................................}

{..............................................................................}
Function IsClassMethod(Const AName : AnsiString; Var AClassName : AnsiString) : Boolean;
Const
    _dbg_prefix : AnsiString = '__';
    _dbg_delim : AnsiString = '@';
Var
    P : Integer;
    S : AnsiString;
Begin
    P := Pos(_dbg_prefix, AName);
    Result := P > 0;

    If Result Then
        AClassName := Copy(AName, 1, P - 1)
    Else
        If Pos(_dbg_delim, AName) = 1 Then
        Begin
            S := Copy(AName, 2);
            P := Pos(_dbg_delim, S);
            If P > 0 Then
            Begin
                AClassName := Copy(S, 1, P - 1);
                Result := True;
            End;
        End;
End;
{...............................................................................}

{...............................................................................}
Function TryGetClass(FuncInfo : TFuncInfo; Out TypeInfo : TTypeInfo) : Boolean;
Var
    ClassName : AnsiString;
    TryFunc   : TFuncInfo;
Begin
    TryFunc := FuncInfo;
    Repeat
        Result := IsClassMethod(TryFunc.Name, ClassName);
        If Not Result Then
            TryFunc := TryFunc.Parent;
    Until Result Or (TryFunc = Nil);

    If Result Then
    begin
        TypeInfo := TryFunc.UnitInfo.FindTypeByName(String(ClassName));
        Result := Assigned(TypeInfo);
    end;
End;
{..............................................................................}

{..............................................................................}
Function TryGetSelfClass(FuncInfo : TFuncInfo; Out TypeInfo : TTypeInfo) : Boolean;
Var
    V   : TVarInfo;
Begin
    Result := False;
    V := FuncInfo.FindVarByName(cSelf);
    If Assigned(V) Then
    Begin
        If V.DataType.Kind = tkObject Then
        Begin
            TypeInfo := V.DataType.BaseType;
            Result := True;
        End;
    End;
End;
{..............................................................................}

{..............................................................................}
Function TryGetSelf(Debuger: TDebuger; FuncInfo : TFuncInfo; Out Value : Variant) : Boolean;
Var
    ClassType : TTypeInfo;
Begin
    Result := (FuncInfo <> Nil) And TryGetClass(FuncInfo, ClassType);
    If Result Then
        Result := TryGetVariable(Debuger, FuncInfo, cSelf, Value, True);
End;
{..............................................................................}

{..............................................................................}
Function TryEvaluateClassField(Debuger: TDebuger; FuncInfo : TFuncInfo; Const FieldName : AnsiString; Out Value : Variant; BriefMode : Boolean) : Boolean;
Var
    ReferenceAccess : IReferenceAccess;
    CalculateData   : TCalculateData;
Begin
    Result := TryGetSelf(Debuger, FuncInfo, Value);
    If Result Then
        Case VarType(Value) Of
            varUnknown :
                If Supports(IUnknown(TVarData(Value).VUnknown), IReferenceAccess, ReferenceAccess) Then
                Begin
                    CalculateData.BriefMode := BriefMode;
                    CalculateData.DebugInfo := FuncInfo.UnitInfo.DebugInfo;
                    Result := ReferenceAccess.TryReference(CalculateData, FieldName, Value);
                End;
        Else
            Result := False;
        End;
End;
{..............................................................................}

{..............................................................................}
Function TryEvaluateClassMethod(Debuger: TDebuger; MakeFunction : TMakeFunction;
    ParentFuncInfo : TFuncInfo; Const MethodName : AnsiString; Out Value : Variant) : Boolean;
Var
    ClassType     : TTypeInfo;
    SelfValue     : Variant;
    TryClass      : TTypeInfo;
    FuncInfo      : TFuncInfo;
    Address       : TPointer;
    CalculateData : TCalculateData;
Begin
    Result := TryGetSelfClass(ParentFuncInfo, ClassType) And TryGetSelf(Debuger, ParentFuncInfo, SelfValue);
    If Result Then
    Begin
        TryClass := ClassType;
        Repeat
            Result := TryGetFuncInfo(ParentFuncInfo.UnitInfo, MakeFunction(TryClass.Name, MethodName), FuncInfo);
            TryClass := TryClass.BaseType;
        Until Result Or (TryClass = Nil);
        If Result Then
        Begin
            CalculateData.BriefMode := False;
            CalculateData.DebugInfo := FuncInfo.UnitInfo.DebugInfo;
            RequireValue(CalculateData, SelfValue);
            Result := TryGetValueAddress(SelfValue, Address);
            If Result Then
                Value := GetFunctionValue(Address, FuncInfo);
        End;
    End;
End;
{..............................................................................}

{..............................................................................}
Function TryGetRefValueAddress(Const Value : Variant; Out Address : TPointer) : Boolean;
Var
    GetAddress : IGetReferenceAddress;
Begin
    If (VarType(Value) = varUnknown) And Supports(IUnknown(TVarData(Value).VUnknown), IGetReferenceAddress, GetAddress) Then
    Begin
        Address := GetAddress.GetReferenceAddress;
        Result := True;
    End
    Else
        Result := False;
End;
{..............................................................................}

{..............................................................................}
Function TryGetValueAddress(Const Value : Variant; Out Address : TPointer) : Boolean;
Var
    GetAddress : IGetValueAddress;
Begin
    If (VarType(Value) = varUnknown) And Supports(IUnknown(TVarData(Value).VUnknown), IGetValueAddress, GetAddress) Then
    Begin
        Address := GetAddress.GetValueAddress;
        Result := True;
    End
    Else
        Result := False;
End;
{..............................................................................}

{..............................................................................}
Procedure RequireValue(CalculateData : TCalculateData; Var Value : Variant);
Var
    CalculateValue : ICalculateValue;
Begin
    If (VarType(Value) = varUnknown) And Supports(IUnknown(TVarData(Value).VUnknown), ICalculateValue, CalculateValue) Then
        Value := CalculateValue.Calculate(CalculateData);
End;
{..............................................................................}

{..............................................................................}
Function RequireOrdinalValue(Const Value : Variant) : TOrdinalValue;
Var
    GetOrdinalValue : IGetOrdinalValue;
Begin
    If (VarType(Value) = varUnknown) And Supports(IUnknown(TVarData(Value).VUnknown), IGetOrdinalValue, GetOrdinalValue) Then
        Result := GetOrdinalValue.GetOrdinalValue
    Else
        Result := ConvertToInteger(Value);
End;
{..............................................................................}

{..............................................................................}
Procedure FinishCalculate(CalculateData : TCalculateData; Var Value : Variant);
Var
    DelayedCalculate : ICalculateValue;
Begin
    If (VarType(Value) = varUnknown) And Supports(IUnknown(TVarData(Value).VUnknown), IDelayedCalculate, DelayedCalculate) Then
        Value := DelayedCalculate.Calculate(CalculateData);
End;
{..............................................................................}

{..............................................................................}
Procedure ExpressionError(Const Err : String);
Begin
    Raise EExpressionParser.Create(Err);
End;
{..............................................................................}

{..............................................................................}
Function ConvertToInteger(Const Value : Variant) : Integer;
Begin
    //TODO: To it properly with proper message
    Case VarType(Value) Of
        varBoolean :
            If Value Then
                Result := 1
            Else
                Result := 0;
    Else
        Result := Value;
    End;
End;
{..............................................................................}

{..............................................................................}
Function ConvertToType(Const Value : Variant; TypeInfo : TTypeInfo) : Variant;
//Const
//    cTypeKindToVarKind : Array[Low(TTypeKind)..High(TTypeKind)] Of Word =
//    (
//        varUnknown,  //tkVoid,
//        varUnknown,  //tkClass,
//        varUnknown,  //tkObject,
//        varUnknown,  //tkStruct,
//        varUnknown,  //tkEnum,
//        varUnknown,  //tkSet,
//        varByRef,    //tkPointer,
//        varShortInt, //tkShortInt,
//        varSmallint, //tkSmallInt,
//        varInteger,  //tkInteger,
//        varByte,     //tkByte,
//        varWord,     //tkWord,
//        varInteger,  //tkCardinal,
//        varDouble,   //tkFloat,
//        varBoolean,  //tkBoolean,
//        varBoolean,  //tkWordBool,
//        varBoolean,  //tkLongBool,
//        varString,   //tkChar,
//        varString,   //tkWidechar,
//        varString,   //tkShortString,
//        varString,   //tkLongString,
//        varUString,  //tkWideString,
//        varArray,    //tkArray,
//        varInt64,    //tkInt64,
//        varUInt64,   //tkUInt64,
//        varArray     //tkDynamicArray
//    );
//
//Var
//    VarType : TVarType;
Begin
//    Result := Null;
//
//    If TypeInfo = Nil Then Exit;
//
//    Case TypeInfo.Kind Of
//        tkFloat :
//          If TypeInfo.DataSize = SizeOf(Single) Then
//              VarType := varSingle
//          Else
//              VarType := varDouble;
//    Else
//        VarType := cTypeKindToVarKind[TypeInfo.Kind];
//    End;
//
//    Result := VarAsType(Value, VarType);
End;
{..............................................................................}

End.
