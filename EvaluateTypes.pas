Unit EvaluateTypes;

Interface

Uses
    Windows,
    SysUtils,
    DebugInfo,
    Debuger;
{..............................................................................}

{..............................................................................}
Type
    PUInt64 = ^UInt64;
    PReal48 = ^Real48;
    PReal = ^Real;
    PLongBool = ^LongBool;

    TPointer = Pointer;
    TUIntPtr = Pointer;
    TSize = Cardinal;
    TInteger = Integer;
    TUInteger = Cardinal;
    TOrdinalValue = TInteger;

    TInternalAnsiStringRec = Packed Record
        refCnt: Longint;
        length: Longint;
    End;
{..............................................................................}

{..............................................................................}
    TInternalWideStringRec = Packed Record
        length: Longint;
    End;
{..............................................................................}

{..............................................................................}
    TToStringMode = (tsmNormal, tsmBrief);
    TToStringData = Record
        DebugInfo       : TDebugInfo;
        Mode            : TToStringMode;
        RecursionLevel  : Integer;
    End;
{..............................................................................}

{..............................................................................}
    TCalculateData = Record
        BriefMode       : Boolean;
        DebugInfo       : TDebugInfo;
    End;
{..............................................................................}

{..............................................................................}
    TCalculateParams = Array Of Variant;
{..............................................................................}

{..............................................................................}
    ICustomVariantData = Interface
    ['{A389B3EA-634B-4572-BE3C-0883D3D3C8FE}']
        Function AsString(Var ToStringData : TToStringData) : String;
    End;
{..............................................................................}

{..............................................................................}
    ITypeInfo = Interface
    ['{577E23A9-87B9-4AD2-B97C-9EE6355F5720}']
        Function GetTypeInfo : TTypeInfo;
    End;
{..............................................................................}

{..............................................................................}
    IIndexAccess = Interface
    ['{F5D0F00B-809A-4F8B-8D90-B697874D1F09}']
        Function CalculateIndex(Const CalculateData : TCalculateData; Const IndexValue : Variant) : Variant;
    End;
{..............................................................................}

{..............................................................................}
    IDefaultProperty = Interface
    ['{E1967C8A-7426-4548-BCD9-8B02E744E8FC}']
        Function GetDefaultPropertyIdentifier(Const CalculateData : TCalculateData; Out Address : TPointer; Out MethodName : AnsiString) : Boolean;
    End;
{..............................................................................}

{..............................................................................}
    // To access to structure or object fields
    IReferenceAccess = Interface
    ['{49E8E461-FD2C-4686-8A59-BDA3DB7B21CF}']
        Function TryReference(Const CalculateData : TCalculateData; Const ItemName : AnsiString; Out Value : Variant) : Boolean;
    End;
{..............................................................................}

{..............................................................................}
    // To convert by ref value to value
    ICalculateValue = Interface
        ['{D3D8DA7E-E25D-4A49-BA4D-1B28A34AB677}']
        Function Calculate(Const CalculateData : TCalculateData) : Variant;
    End;
{..............................................................................}

{..............................................................................}
    // To convert by ref value to value
    IModifyValue = Interface
        ['{9EFE80E4-02BC-4CF5-8D99-78109130258F}']
        Procedure Modify (Const CalculateData : TCalculateData; Const NewValue : Variant);
        Procedure SetData(Const CalculateData : TCalculateData; Const Data : Variant);
    End;
{..............................................................................}

    IConvertValue = Interface
        ['{17206143-FA00-4362-B478-D96E2F44B5ED}']
        Function ConvertToType(Const CalculateData : TCalculateData; TypeInfo : TTypeInfo) : Variant;
    End;

{..............................................................................}
    // To dereference pointer
    IDereference = Interface
        ['{A28AD4B4-84DD-4958-A23C-2D575F13C93E}']
        Function Dereference(Const CalculateData : TCalculateData) : Variant;
    End;
{..............................................................................}

{..............................................................................}
    IFunction = Interface
        ['{8525C91E-D34E-483F-8330-387BFD18B5AF}']

        Function CalculateFunction(Const CalculateData : TCalculateData; Const Params : TCalculateParams) : Variant;
    End;
{..............................................................................}

{..............................................................................}
    // Used for functions because when we found function symbol we can't calculate it immediatelly so we will delay it until we need value
    IDelayedCalculate = Interface
        ['{069FF183-3EF0-49AB-A9B5-5C97E4300E8C}']

        Function Calculate(Const CalculateData : TCalculateData) : Variant;
    End;
{..............................................................................}

{..............................................................................}
    IGetReferenceAddress = Interface
        ['{871474C2-885B-4DC1-AA61-35306214AA59}']

        Function GetReferenceAddress : Pointer;
    End;
{..............................................................................}

{..............................................................................}
    IGetValueAddress = Interface
        ['{41FF2015-78AE-4A52-AA50-4844E6A3BAEA}']

        Function GetValueAddress : Pointer;
    End;
{..............................................................................}

{..............................................................................}
    IGetOrdinalValue = Interface
        ['{595493B4-5155-43CA-B67D-045C985389C4}']

        Function GetOrdinalValue : TOrdinalValue;
    End;
{..............................................................................}

{..............................................................................}
    TCustomVariantValue = Class(TInterfacedObject, ICustomVariantData)
    Public
        Function AsString(Var ToStringData : TToStringData) : String; virtual; abstract;
    End;
{..............................................................................}

{..............................................................................}
    TExtendedConstantValue = Class(TCustomVariantValue)
    Private
        FValue : Extended;
    Public
        Constructor Create(const AValue : Extended);
        Function AsString(Var ToStringData : TToStringData) : String; Override;
    End;
{..............................................................................}

{..............................................................................}
    TPointerConstantValue = Class(TCustomVariantValue)
    Private
        FValue : Pointer;
    Public
        Constructor Create(AValue : Pointer); Virtual;
        Function AsString(Var ToStringData : TToStringData) : String; Override;
    End;
{..............................................................................}

{..............................................................................}
    TCustomTypedVariantValue = Class(TCustomVariantValue, ITypeInfo)
    Private
        FTypeInfo : TTypeInfo;
    Public
        Constructor Create(TypeInfo : TTypeInfo);

        Function GetTypeInfo : TTypeInfo;
    End;
{..............................................................................}

{..............................................................................}
    TCustomReferenceVariantValue = Class(TCustomTypedVariantValue, ICalculateValue, IGetReferenceAddress, IModifyValue)
    Private
        FAddress  : TPointer;
    Protected
        Procedure   SetData(Const CalculateData : TCalculateData; Const Data : Variant); Virtual;
    Public
        Constructor Create(TypeInfo : TTypeInfo; Address : TPointer);

        Function    Calculate(Const CalculateData : TCalculateData) : Variant; virtual; abstract;

        Procedure   Modify(Const CalculateData : TCalculateData; Const NewValue : Variant); Virtual;

        Function    GetReferenceAddress : TPointer;

        Function    AsString(Var ToStringData : TToStringData) : String; override;
    End;
    TCustomReferenceVariantDataClass = Class Of TCustomReferenceVariantValue;
{..............................................................................}

{..............................................................................}
    TCustomByRefReferenceVariantValue = Class(TCustomReferenceVariantValue)
    Private
        Function GetAddressToUse(Const CalculateData : TCalculateData) : TPointer;
    End;
    {..............................................................................}

{..............................................................................}
    TIntegerRefVariantValue = Class(TCustomReferenceVariantValue)
    Protected
        Procedure   SetData(Const CalculateData : TCalculateData; Const Data : Variant); Override;
    Public
        Function    Calculate(Const CalculateData : TCalculateData) : Variant; override;
    End;
{..............................................................................}

{..............................................................................}
    TInt64RefVariantValue = Class(TCustomReferenceVariantValue)
    Protected
        Procedure   SetData(Const CalculateData : TCalculateData; Const Data : Variant); Override;
    Public
        Function    Calculate(Const CalculateData : TCalculateData) : Variant; override;
    End;
{..............................................................................}

{..............................................................................}
    TFloatRefVariantValue = Class(TCustomReferenceVariantValue)
    Protected
        Procedure   SetData(Const CalculateData : TCalculateData; Const Data : Variant); Override;
    Public
        Function    Calculate(Const CalculateData : TCalculateData) : Variant; override;
    End;
{..............................................................................}

{..............................................................................}
    TShortStringRefVariantValue = Class(TCustomReferenceVariantValue, IIndexAccess)
    Public
        Function    Calculate     (Const CalculateData : TCalculateData) : Variant; override;
        Function    CalculateIndex(Const CalculateData : TCalculateData; Const IndexValue : Variant) : Variant;
    End;
{..............................................................................}

{..............................................................................}
    TAnsiStringRefVariantValue = Class(TCustomByRefReferenceVariantValue, IIndexAccess)
    Protected
        Procedure   SetData(Const CalculateData : TCalculateData; Const Data : Variant); Override;
    Public
        Function    Calculate     (Const CalculateData : TCalculateData) : Variant; override;
        Function    CalculateIndex(Const CalculateData : TCalculateData; Const IndexValue : Variant) : Variant;
    End;
{..............................................................................}

{..............................................................................}
    TWideStringRefVariantValue = Class(TCustomByRefReferenceVariantValue, IIndexAccess)
    Protected
        Procedure   SetData(Const CalculateData : TCalculateData; Const Data : Variant); Override;
    Public
        Function    Calculate     (Const CalculateData : TCalculateData) : Variant; override;
        Function    CalculateIndex(Const CalculateData : TCalculateData; Const IndexValue : Variant) : Variant;
    End;
{..............................................................................}

{..............................................................................}
    TEnumRefVariantValue = Class(TCustomReferenceVariantValue)
    Public
        Function    Calculate(Const CalculateData : TCalculateData) : Variant; override;
    End;
{..............................................................................}

{..............................................................................}
    TEnumVariantValue = Class(TCustomTypedVariantValue, IGetOrdinalValue)
    Private
        FValue : TOrdinalValue;
    Public
        Constructor Create(TypeInfo : TTypeInfo; Value : TOrdinalValue);

        Function    GetOrdinalValue : TOrdinalValue;

        Function    AsString(Var ToStringData : TToStringData) : String; override;
    End;
{..............................................................................}

{..............................................................................}
    TSetRefVariantValue = Class(TCustomReferenceVariantValue)
    Public
        Function    Calculate(Const CalculateData : TCalculateData) : Variant; override;
    End;
{..............................................................................}

{..............................................................................}
    TSetValue = Array Of Byte;
    TSetVariantValue = Class(TCustomTypedVariantValue)
    Private
        FValue : TSetValue;
    Public
        Constructor Create(TypeInfo : TTypeInfo; Const Value : TSetValue); overload;
        Constructor Create(TypeInfo : TTypeInfo;       Value : TUIntPtr ); overload;

        Function    AsString(Var ToStringData : TToStringData) : String; override;
    End;
{..............................................................................}

{..............................................................................}
    TStructRefVariantValue = Class(TCustomReferenceVariantValue, IReferenceAccess)
    Public
        Function    Calculate   (Const CalculateData : TCalculateData) : Variant; override;
        Function    TryReference(Const CalculateData : TCalculateData; Const ItemName : AnsiString; Out Value : Variant) : Boolean;
    End;
{..............................................................................}

{..............................................................................}
    TStructVariantValue = Class(TCustomTypedVariantValue, IReferenceAccess)
    Private
        FAddress   : TUIntPtr;
    Public
        Constructor Create(TypeInfo : TTypeInfo; Address : TUIntPtr);

        Function    AsString(Var ToStringData : TToStringData) : String; override;

        Function    TryReference(Const CalculateData : TCalculateData; Const ItemName : AnsiString; Out Value : Variant) : Boolean;
    End;
{..............................................................................}

{..............................................................................}
    TArrayRefVariantValue = Class(TCustomByRefReferenceVariantValue, IIndexAccess)
    Public
        Function    Calculate     (Const CalculateData : TCalculateData) : Variant; override;
        Function    CalculateIndex(Const CalculateData : TCalculateData; Const IndexValue : Variant) : Variant;
    End;
{..............................................................................}

{..............................................................................}
    TArrayVariantValue = Class(TCustomTypedVariantValue, IIndexAccess)
    Private
        FAddress   : TUIntPtr;
    Public
        Constructor Create(TypeInfo : TTypeInfo; Address : TUIntPtr);

        Function    CalculateIndex(Const CalculateData : TCalculateData; Const IndexValue : Variant) : Variant;

        Function    AsString(Var ToStringData : TToStringData) : String; override;
    End;
{..............................................................................}

{..............................................................................}
    TPointerRefVariantValue = Class(TCustomByRefReferenceVariantValue, IReferenceAccess, IDereference)
    Public
        Function    Calculate   (Const CalculateData : TCalculateData) : Variant; override;

        Function    Dereference (Const CalculateData : TCalculateData) : Variant;

        Function    TryReference(Const CalculateData : TCalculateData; Const ItemName : AnsiString; Out Value : Variant) : Boolean;
    End;
{..............................................................................}

{..............................................................................}
    TPointerVariantValue = Class(TCustomTypedVariantValue, IReferenceAccess, IDereference)
    Private
        FAddress   : TUIntPtr;
    Public
        Constructor Create(TypeInfo : TTypeInfo; Address : TUIntPtr);

        Function    Dereference(Const CalculateData : TCalculateData) : Variant;

        Function    AsString(Var ToStringData : TToStringData) : String; override;

        Function    TryReference(Const CalculateData : TCalculateData; Const ItemName : AnsiString; Out Value : Variant) : Boolean;
    End;
{..............................................................................}

{..............................................................................}
    TObjectRefVariantValue = Class(TCustomByRefReferenceVariantValue, IReferenceAccess, IDefaultProperty)
    Public
        Function Calculate(Const CalculateData : TCalculateData) : Variant; override;
        Function TryReference(Const CalculateData : TCalculateData; Const ItemName : AnsiString; Out Value : Variant) : Boolean;
        Function GetDefaultPropertyIdentifier(Const CalculateData : TCalculateData; Out Address : TPointer; Out MethodName : AnsiString) : Boolean;
    End;
{..............................................................................}

{..............................................................................}
    TObjectVariantValue = Class(TCustomTypedVariantValue, IReferenceAccess, IGetValueAddress, IDefaultProperty)
    Private
        FAddress   : TUIntPtr;
    Public
        Constructor Create(TypeInfo : TTypeInfo; Address : TUIntPtr);

        Function GetValueAddress : TPointer;
        Function AsString(Var ToStringData : TToStringData) : String; override;
        Function TryReference(Const CalculateData : TCalculateData; Const ItemName : AnsiString; Out Value : Variant) : Boolean;
        Function GetDefaultPropertyIdentifier(Const CalculateData : TCalculateData; Out Address : TPointer; Out MethodName : AnsiString) : Boolean;
    End;
{..............................................................................}

{..............................................................................}
    TFunctionVariantValue = Class(TCustomVariantValue, ICalculateValue, IDelayedCalculate, IFunction, IIndexAccess)
    Private
        FSelfAddress    : TPointer;
        FFuncInfo       : TFuncInfo;
        FParams         : TCalculateParams;
    Public
        Constructor Create(SelfAddress : TPointer; FuncInfo : TFuncInfo);
        Destructor  Destroy; Override;

        Function Calculate(Const CalculateData : TCalculateData) : Variant;
        Function CalculateFunction(Const CalculateData : TCalculateData; Const Params : TCalculateParams) : Variant;
        Function CalculateIndex(Const CalculateData : TCalculateData; Const IndexValue : Variant) : Variant;
        Function AsString(Var ToStringData : TToStringData) : String; override;
    End;
{..............................................................................}

{..............................................................................}
    TVoidVariantValue = Class(TCustomVariantValue)
    Public
        Function    AsString(Var ToStringData : TToStringData) : String; override;
    End;
{..............................................................................}

{..............................................................................}
    EExpressionParser = Class(Exception);
{..............................................................................}

{..............................................................................}
Implementation
Uses
    Classes,
    Math,
    Variants,

    //System.Strings,
    //System.Debug,

    EvaluateConsts,
    EvaluateProcs,

    Evaluator, ClassUtils;
{..............................................................................}

{..............................................................................}
Procedure AddToCommaResult(Var Result : String; Const Value : String);
Begin
    If Length(Result) > 0 Then
        Result := Result + ', ';
    Result := Result + Value;
End;
{..............................................................................}

{..............................................................................}
Function CharValueToString(Index : Integer) : String;
Begin
    If Index < 32 Then
        Result := '#' + IntToStr(Index)
    Else
        Result := '''' + Chr(Index) + '''';
End;
{..............................................................................}

{..............................................................................}
Function IntToEnumValue(TypeInfo : TTypeInfo; Value : Integer) : String;
Var
    Index : Integer;
Begin
    Index := TypeInfo.Elements.IndexOfObject(TObject(Value));

    If Index >= 0 Then
        Result := TypeInfo.Elements[Index]
    Else
        Result := Format('%s(%d)', [TypeInfo.Name, Value]);
End;
{..............................................................................}

{..............................................................................}
Function GetSetTypeElement(TypeInfo : TTypeInfo; Index : Integer) : String;
Begin
    Case TypeInfo.Kind Of
        tkShortInt, tkSmallInt, tkInteger, tkByte, tkWord, tkCardinal :
            Result := IntToStr(Index);
        tkChar, tkWideChar :
            Result := CharValueToString(Index);
        tkBoolean, tkWordBool, tkLongBool :
            Result := BoolToStr(Index <> 0, True);
        tkEnum :
            Result := IntToEnumValue(TypeInfo, Index);
    Else
        RaiseInternalError(cUnsupportedDataType);
        Result := '';
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure AddRangeToCommandResult(Var Result : String; TypeInfo : TTypeInfo; StartValue, CurrentValue : Integer);
Begin
    If StartValue = CurrentValue Then
        AddToCommaResult(Result, GetSetTypeElement(TypeInfo, StartValue))
    Else
        If StartValue + 1 = CurrentValue Then
        Begin
            AddToCommaResult(Result, GetSetTypeElement(TypeInfo, StartValue));
            AddToCommaResult(Result, GetSetTypeElement(TypeInfo, CurrentValue));
        End
        Else
            AddToCommaResult(Result, GetSetTypeElement(TypeInfo, StartValue) + ' .. ' + GetSetTypeElement(TypeInfo, CurrentValue));
End;
{..............................................................................}

{..............................................................................}
Function SetToString(Data : PByte; TypeInfo : TTypeInfo) : String;
Var
    I : Integer;
    Mask : Byte;
    StartValue : Integer;
    CurrentValue : Integer;
    MinValue : Integer;
    MaxValue : Integer;
    Delta    : Integer;
Begin
    StartValue := -1;
    CurrentValue := -1;
    MinValue := TypeInfo.MinValue;
    MaxValue := TypeInfo.MaxValue;
    Delta := MinValue - (MinValue Mod 8);

    {$IFDEF TESTFPC}
    Inc(Data, MinValue Div 8);
    {$ENDIF}

    For I := MinValue To MaxValue Do
    Begin
        Mask := 1 Shl ((I - Delta) And $7);
        If (Data^ And Mask) <> 0 Then
        Begin
            If StartValue < 0 Then
                StartValue := I;
            CurrentValue := I;
        End
        Else
        Begin
            If StartValue >= 0 Then
            Begin
                AddRangeToCommandResult(Result, TypeInfo, StartValue, CurrentValue);
                StartValue := -1;
            End;
        End;
        If Mask = $80 Then
            Inc(Data);
    End;
    If StartValue >= 0 Then
        AddRangeToCommandResult(Result, TypeInfo, StartValue, CurrentValue);
    Result := '[' + Result + ']';
End;
{..............................................................................}

{..............................................................................}
Function TryFindClassMethod(Const CalculateData : TCalculateData; Address : TPointer;
    Const MethodName : AnsiString; Out Value : Variant) : Boolean;
Var
    Func : TFuncInfo;
Begin
    Func := CalculateData.DebugInfo.FuncByName(MethodName);
    Result := Assigned(Func);
    If Result Then
        Value := TFunctionVariantValue.Create(Address, Func) As IUnknown;
End;
{..............................................................................}

{..............................................................................}
Function TryCalculateMember(Const CalculateData : TCalculateData; TypeInfo : TTypeInfo;
    Address : TPointer; Const ItemName : AnsiString; Out Value : Variant) : Boolean;
Var
    Index       : Integer;
    Member      : TStructMember;
    CurTypeInfo : TTypeInfo;
    ClassType   : TTypeInfo;
    MethodName  : AnsiString;
Begin
    CurTypeInfo := TypeInfo;
    Repeat
        Result := (CurTypeInfo.Members <> Nil) And CurTypeInfo.Members.Find(String(ItemName), Index);
        If Result Then
        Begin
            Member := TStructMember(CurTypeInfo.Members.Objects[Index]);
            If Member.Alias <> '' Then
            Begin
                Result := CurTypeInfo.Members.Find(String(Member.Alias), Index);
                If Result Then
                    Member := TStructMember(CurTypeInfo.Members.Objects[Index])
                Else
                    Exit;
            End;
            If Result Then
                If Member.MethodName <> '' Then
                Begin
                    MethodName := CalculateData.DebugInfo.MakeFuncShortName(Member.MethodName);
                    ClassType := TypeInfo;
                    Repeat
                        Result := TryFindClassMethod(CalculateData, Address, CalculateData.DebugInfo.MakeFuncDbgFullName(ClassType.Name, MethodName), Value);
                        ClassType := ClassType.BaseType;
                    Until Result Or (ClassType = Nil);
                End
                Else
                    Value := GetValueRef(CalculateData.DebugInfo.Debuger, Member.DataType, IncPointer(Address, Member.BitOffset Div 8));
        End
        Else
            Result := TryFindClassMethod(CalculateData, Address, CalculateData.DebugInfo.MakeFuncDbgFullName(CurTypeInfo.Name, ItemName), Value);
        CurTypeInfo := CurTypeInfo.BaseType;
    Until Result Or (CurTypeInfo = Nil);
End;
{..............................................................................}

{..............................................................................}
Function GetArrayValue(DebugInfo: TDebugInfo; TypeInfo : TTypeInfo; Address : TPointer; Const IndexValue : Variant; BriefMode : Boolean) : Variant;
Var
    VarIndexValue : Variant;
    IntIndexValue : TOrdinalValue;
    MinValue      : Integer;
    MaxValue      : Integer;
    Info          : TTypeInfo;
    CalculateData : TCalculateData;
Begin
    VarIndexValue := IndexValue;
    CalculateData.DebugInfo := DebugInfo;
    RequireValue(CalculateData, VarIndexValue);
    IntIndexValue := RequireOrdinalValue(VarIndexValue);

    If TypeInfo.Kind = tkDynamicArray Then
    Begin
        MinValue := 0;
        MaxValue := Integer(ReadAddressValue(DebugInfo.Debuger, IncPointer(Address, -4)));
        Info := TypeInfo.BaseType;
        Inc(MaxValue, Info.MaxValue);
    End
    Else
    Begin
        MinValue := TypeInfo.MinValue;
        MaxValue := TypeInfo.MaxValue;
        Info := TypeInfo;
    End;

    If (IntIndexValue < MinValue) Or (IntIndexValue > MaxValue) Then
        ExpressionError(cConstantExpressionViolatesSubrangeBounds);

    Result := GetValue(DebugInfo.Debuger, Info.BaseType,
      IncPointer(Address, (IntIndexValue - MinValue) * Info.BaseType.DataSize), True, BriefMode);
End;
{..............................................................................}

{..............................................................................}
Function TryGetDefaultPropertyIdentifier(TypeInfo : TTypeInfo) : AnsiString;
Var
    I      : Integer;
    Info   : TTypeInfo;
Begin
    Result := '';
    Info := TypeInfo;
    Repeat
        If Info.Members <> Nil Then
        Begin
            I := Info.Members.Count - 1;
            While (I > -1) And Not TStructMember(Info.Members.Objects[I]).IsDefault Do
                Dec(I);
            If I > -1 Then
                Result := TStructMember(Info.Members.Objects[I]).MethodName;
        End;
        If Result = '' Then
            Info := Info.BaseType;
    Until (Result <> '') Or (Info = Nil);
End;
{..............................................................................}

{..............................................................................}
Constructor TPointerConstantValue.Create(AValue : Pointer);
Begin
    Inherited Create;
    FValue := AValue;
End;
{..............................................................................}

{..............................................................................}
Function TPointerConstantValue.AsString(Var ToStringData : TToStringData) : String;
Begin
    If FValue = Nil Then
        Result := 'Nil'
    Else
        Result := '$' + IntToHex(Cardinal(FValue), 8);
End;
{..............................................................................}

{..............................................................................}
Constructor TCustomTypedVariantValue.Create(TypeInfo : TTypeInfo);
Begin
    Inherited Create;

    FTypeInfo := TypeInfo;
End;
{..............................................................................}

{..............................................................................}
Function TCustomTypedVariantValue.GetTypeInfo : TTypeInfo;
Begin
    Result := FTypeInfo;
End;
{..............................................................................}

{..............................................................................}
Constructor TCustomReferenceVariantValue.Create(TypeInfo : TTypeInfo; Address : TPointer);
Begin
    Inherited Create(TypeInfo);

    FAddress := Address;
End;
{..............................................................................}

{..............................................................................}
Function TCustomReferenceVariantValue.GetReferenceAddress : TPointer;
Begin
    Result := FAddress;
End;
{..............................................................................}

{..............................................................................}
Function TCustomReferenceVariantValue.AsString(Var ToStringData : TToStringData) : String;
Var
    CalculateData  : TCalculateData;
    CalculateValue : Variant;
Begin
    CalculateData.BriefMode := ToStringData.Mode = tsmBrief;
    CalculateData.DebugInfo := ToStringData.DebugInfo;

    CalculateValue := Calculate(CalculateData);

    Result := VariantToString(CalculateValue, ToStringData);
End;
{..............................................................................}

{..............................................................................}
Procedure TCustomReferenceVariantValue.Modify(Const CalculateData : TCalculateData; Const NewValue : Variant);
Var
    ConvertValue : IConvertValue;
    NewData      : Variant;
Begin
    If (TVarData(NewValue).VType = varUnknown) And Supports(IUnknown(TVarData(NewValue).VUnknown), IConvertValue, ConvertValue) Then
        NewData := ConvertValue.ConvertToType(CalculateData, FTypeInfo)
    Else
        NewData := ConvertToType(NewValue, FTypeInfo);

    SetData(CalculateData, NewData);
End;
{..............................................................................}

{..............................................................................}
Procedure TCustomReferenceVariantValue.SetData(Const CalculateData : TCalculateData; Const Data : Variant);
Begin
    ExpressionError(Format('Unssuported convert to %s', [FTypeInfo.Name]));
End;
{..............................................................................}


{..............................................................................}
Function TCustomByRefReferenceVariantValue.GetAddressToUse(Const CalculateData : TCalculateData) : TPointer;
Begin
    If FTypeInfo.Kind = tkArray Then
        Result := FAddress
    Else
        Result := ReadAddressValue(CalculateData.DebugInfo.Debuger, FAddress);
End;
{..............................................................................}

{..............................................................................}
Function TIntegerRefVariantValue.Calculate(const CalculateData: TCalculateData) : Variant;
Begin
    Result := ReadIntegerValue(CalculateData.DebugInfo.Debuger, FTypeInfo, FAddress);
End;
{..............................................................................}

{..............................................................................}
Procedure TIntegerRefVariantValue.SetData(Const CalculateData : TCalculateData; Const Data : Variant);
Begin
    WriteIntegerValue(CalculateData.DebugInfo.Debuger, FTypeInfo, FAddress, Data);
End;
{..............................................................................}

{..............................................................................}
Function TInt64RefVariantValue.Calculate(const CalculateData: TCalculateData) : Variant;
Begin
    Result := ReadInt64Value(CalculateData.DebugInfo.Debuger, FTypeInfo,  FAddress);
End;
{..............................................................................}

{..............................................................................}
Procedure TInt64RefVariantValue.SetData(Const CalculateData : TCalculateData; Const Data : Variant);
Begin
    WriteInt64Value(CalculateData.DebugInfo.Debuger, FTypeInfo, FAddress, Data);
End;
{..............................................................................}

{..............................................................................}
Function TFloatRefVariantValue.Calculate(const CalculateData: TCalculateData) : Variant;
Begin
    Result := ReadFloatValue(CalculateData.DebugInfo.Debuger, FTypeInfo, FAddress);
End;
{..............................................................................}

{..............................................................................}
Procedure TFloatRefVariantValue.SetData(Const CalculateData: TCalculateData; Const Data: Variant);
Begin
    WriteFloatValue(CalculateData.DebugInfo.Debuger, FTypeInfo, FAddress, Data);
End;
{..............................................................................}

{..............................................................................}
Function GetStringIndexRefValue(TypeInfo : TTypeInfo;Const IndexValue : Variant; Address : TPointer) : Variant;
Var
    Index : Integer;
Begin
    Index := ConvertToInteger(IndexValue);
    Result := TIntegerRefVariantValue.Create(TypeInfo, IncPointer(Address, (Index - 1) * TypeInfo.DataSize)) As IUnknown;
End;
{..............................................................................}

{..............................................................................}
Function TShortStringRefVariantValue.Calculate(Const CalculateData : TCalculateData) : Variant;
Begin
    Result := ReadShortStringValue(CalculateData.DebugInfo.Debuger, FAddress);
End;
{..............................................................................}

{..............................................................................}
Function TShortStringRefVariantValue.CalculateIndex(Const CalculateData : TCalculateData; Const IndexValue : Variant) : Variant;
Begin
    Result := GetStringIndexRefValue(FTypeInfo.BaseType, IndexValue, FAddress);
End;
{..............................................................................}

{..............................................................................}
Function TAnsiStringRefVariantValue.Calculate(Const CalculateData : TCalculateData) : Variant;
Begin
    Result := ReadAnsiStringValue(CalculateData.DebugInfo.Debuger, GetAddressToUse(CalculateData), CalculateData.BriefMode);
End;
{..............................................................................}

{..............................................................................}
Function TAnsiStringRefVariantValue.CalculateIndex(Const CalculateData : TCalculateData; Const IndexValue : Variant) : Variant;
Begin
    Result := GetStringIndexRefValue(FTypeInfo.BaseType, IndexValue, GetAddressToUse(CalculateData));
End;
{..............................................................................}

{..............................................................................}
Procedure TAnsiStringRefVariantValue.SetData(Const CalculateData: TCalculateData; Const Data: Variant);
Var
    StrData : AnsiString;
    Params  : TCalculateParams;
    F : TFuncInfo;
Begin
    StrData := AnsiString(Data);

    F := CalculateData.DebugInfo.FuncByName('__Eval__SetAnsiString');
    If Assigned(F) Then
    Begin
        SetLength(Params, 2);
        Params[0] := Self As IUnknown;
        Params[1] := StrData;
        EvaluateFunction(CalculateData.DebugInfo.Debuger, Nil, F, Params);
    End;
End;
{..............................................................................}

{..............................................................................}
Function TWideStringRefVariantValue.Calculate(Const CalculateData : TCalculateData) : Variant;
Begin
    Result := ReadWideStringValue(CalculateData.DebugInfo.Debuger, GetAddressToUse(CalculateData), CalculateData.BriefMode);
End;
{..............................................................................}

{..............................................................................}
Function TWideStringRefVariantValue.CalculateIndex(Const CalculateData : TCalculateData; Const IndexValue : Variant) : Variant;
Begin
    Result := GetStringIndexRefValue(FTypeInfo.BaseType, IndexValue, GetAddressToUse(CalculateData));
End;
{..............................................................................}

{..............................................................................}
Procedure TWideStringRefVariantValue.SetData(Const CalculateData: TCalculateData; Const Data: Variant);
Var
    StrData : AnsiString;
    Params  : TCalculateParams;
    F : TFuncInfo;
Begin
    StrData := AnsiString(Data);

    F := CalculateData.DebugInfo.FuncByName('__Eval__SetWideString');
    If Assigned(F) Then
    Begin
        SetLength(Params, 2);
        Params[0] := Self As IUnknown;
        Params[1] := StrData;
        EvaluateFunction(CalculateData.DebugInfo.Debuger, Nil, F, Params);
    End;
End;
{..............................................................................}

{..............................................................................}
Function TEnumRefVariantValue.Calculate(Const CalculateData : TCalculateData) : Variant;
Var
    Value : TOrdinalValue;
Begin
    Value := ReadIntegerValue(CalculateData.DebugInfo.Debuger, FTypeInfo, FAddress);
    Result := TEnumVariantValue.Create(FTypeInfo, Value) As IUnknown;
End;
{..............................................................................}

{..............................................................................}
Constructor TEnumVariantValue.Create(TypeInfo : TTypeInfo; Value : Integer);
Begin
    Inherited Create(TypeInfo);

    FValue := Value;
End;
{..............................................................................}

{..............................................................................}
Function TEnumVariantValue.GetOrdinalValue : TOrdinalValue;
Begin
    Result := FValue;
End;
{..............................................................................}

{..............................................................................}
Function TEnumVariantValue.AsString(Var ToStringData : TToStringData) : String;
Begin
    Result := IntToEnumValue(FTypeInfo, FValue)
End;
{..............................................................................}

{..............................................................................}
Function TSetRefVariantValue.Calculate(Const CalculateData : TCalculateData) : Variant;
Var
    Value : TSetValue;
Begin
    SetLength(Value, FTypeInfo.DataSize);
    CalculateData.DebugInfo.Debuger.ReadData(FAddress, @Value[0], FTypeInfo.DataSize);
    Result := TSetVariantValue.Create(FTypeInfo, Value) As IUnknown;
End;
{..............................................................................}

{..............................................................................}
Constructor TSetVariantValue.Create(TypeInfo : TTypeInfo; Const Value : TSetValue);
Begin
    Inherited Create(TypeInfo);

    FValue := Value;
End;
{..............................................................................}

{..............................................................................}
Constructor TSetVariantValue.Create(TypeInfo : TTypeInfo; Value : TUIntPtr);
Var
    InternalValue : TSetValue;
Begin
    SetLength(InternalValue, TypeInfo.DataSize);
    Move(Value, InternalValue[0], TypeInfo.DataSize);
    Create(TypeInfo, InternalValue);
End;
{..............................................................................}

{..............................................................................}
Function TSetVariantValue.AsString(Var ToStringData : TToStringData) : String;
Var
    TypeInfo : TTypeInfo;
Begin
    TypeInfo := FTypeInfo.BaseType;
    Result := SetToString(@FValue[0], TypeInfo);
End;
{..............................................................................}

{..............................................................................}
Function TStructRefVariantValue.Calculate(Const CalculateData : TCalculateData) : Variant;
Begin
    Result := TStructVariantValue.Create(FTypeInfo, FAddress) As IUnknown;
End;
{..............................................................................}

{..............................................................................}
Function TStructRefVariantValue.TryReference(Const CalculateData : TCalculateData; Const ItemName : AnsiString; Out Value : Variant) : Boolean;
Begin
    Result := TryCalculateMember(CalculateData, FTypeInfo, FAddress, ItemName, Value);
End;
{..............................................................................}

{..............................................................................}
Constructor TStructVariantValue.Create(TypeInfo : TTypeInfo; Address : TUIntPtr);
Begin
    Inherited Create(TypeInfo);

    FAddress   := Address;
End;
{..............................................................................}

{..............................................................................}
Function TStructVariantValue.AsString(Var ToStringData : TToStringData) : String;
Begin
    Result := StructToString(FTypeInfo, FAddress, ToStringData);
End;
{..............................................................................}

{..............................................................................}
Function TStructVariantValue.TryReference(Const CalculateData : TCalculateData; Const ItemName : AnsiString; Out Value : Variant) : Boolean;
Begin
    Result := TryCalculateMember(CalculateData, FTypeInfo, FAddress, ItemName, Value);
    // Because it is value we need to return value as well
    RequireValue(CalculateData, Value);
End;
{..............................................................................}

{..............................................................................}
Function TArrayRefVariantValue.Calculate(Const CalculateData : TCalculateData) : Variant;
Begin
    Result := TArrayVariantValue.Create(FTypeInfo, FAddress) As IUnknown;
End;
{..............................................................................}

{..............................................................................}
Function TArrayRefVariantValue.CalculateIndex(Const CalculateData : TCalculateData; Const IndexValue : Variant) : Variant;
Begin
    Result := GetArrayValue(CalculateData.DebugInfo, FTypeInfo, GetAddressToUse(CalculateData), IndexValue, CalculateData.BriefMode);
End;
{..............................................................................}

{..............................................................................}
Constructor TArrayVariantValue.Create(TypeInfo : TTypeInfo; Address : TUIntPtr);
Begin
    Inherited Create(TypeInfo);

    FAddress   := Address;
End;
{..............................................................................}

{..............................................................................}
Function TArrayVariantValue.CalculateIndex(Const CalculateData : TCalculateData; Const IndexValue : Variant) : Variant;
Begin
    Result := GetArrayValue(CalculateData.DebugInfo, FTypeInfo, FAddress, IndexValue, CalculateData.BriefMode);
    // Result must be value not reference
    RequireValue(CalculateData, Result);
End;
{..............................................................................}

{..............................................................................}
Function TArrayVariantValue.AsString(Var ToStringData : TToStringData) : String;
Var
    I            : Integer;
    TypeInfo     : TTypeInfo;
    MinValue     : Integer;
    MaxValue     : Integer;
    Addr         : TUIntPtr;
    MaxMembers   : Integer;
    TotalMembers : Integer;
Begin
    Result := '';

    If (ToStringData.RecursionLevel > cMaxEvalRecursionLevel) Then
    Begin
        Result := '...';
        Exit;
    End;

    Inc(ToStringData.RecursionLevel);
    Try
        TotalMembers := 0;
        If ToStringData.Mode = tsmBrief Then
            MaxMembers := cMaxMembersInBriefMode
        Else
            MaxMembers := cMaxMembers;

        TypeInfo := FTypeInfo;

        If TypeInfo.Kind = tkDynamicArray Then
        Begin
            Addr := ReadAddressValue(ToStringData.DebugInfo.Debuger, FAddress);
            If Addr = Nil Then
            Begin
                Result := 'Unassigned';
                Exit;
            End;

            MinValue := 0;

            If ToStringData.DebugInfo.IsValidDataAddr(Addr, ToStringData.DebugInfo.Debuger.CurThreadId) Then
            Begin
                MaxValue := Integer(ReadAddressValue(ToStringData.DebugInfo.Debuger, IncPointer(Addr, -4)));
                Inc(MaxValue, TypeInfo.MaxValue);
            End
            Else
            Begin
                //! TODO: load elements from stack
                Result := '(...)';
                Exit;
            End;

            TypeInfo := TypeInfo.BaseType;
        End
        Else
        Begin
            MinValue := TypeInfo.MinValue;
            MaxValue := TypeInfo.MaxValue;
            Addr := FAddress;
        End;

        For I := MinValue To Min(MaxValue - 1, MinValue + cMaxArrayItems) Do
        Begin
            Inc(TotalMembers);
            If TotalMembers > MaxMembers Then
                Break;

            If Result <> '' Then
                Result := Result + ', ';

            Result := Result + VariantToString(
              GetValueRef(
                ToStringData.DebugInfo.Debuger,
                TypeInfo.BaseType,
                IncPointer(Addr, (I - MinValue) * TypeInfo.BaseType.DataSize)
              ),
              ToStringData);
        End;

        If TotalMembers > MaxMembers Then
            Result := Result + ', ...';
    Finally
        Dec(ToStringData.RecursionLevel);
    End;
    Result := '(' + Result + ')';
End;
{..............................................................................}

{..............................................................................}
Function PointerTryToCalculate(Const CalculateData : TCalculateData; TypeInfo : TTypeInfo; Address : TPointer; Const ItemName : AnsiString; Out Value : Variant) : Boolean;
Var
    ReferenceAccess : IReferenceAccess;
Begin
    Result := (TypeInfo.BaseType <> Nil) And (TypeInfo.BaseType.Kind = tkStructure);
    If Result Then
    Begin
        Value := GetValueRef(CalculateData.DebugInfo.Debuger, TypeInfo.BaseType, Address);
        Result := Supports(IUnknown(TVarData(Value).VUnknown), IReferenceAccess, ReferenceAccess);
        If Result Then
            Result := ReferenceAccess.TryReference(CalculateData, ItemName, Value);
    End;
End;
{..............................................................................}

{..............................................................................}
Function TPointerRefVariantValue.Calculate(Const CalculateData : TCalculateData) : Variant;
Begin
    Result := TPointerVariantValue.Create(FTypeInfo, GetAddressToUse(CalculateData)) As IUnknown;
End;
{..............................................................................}

{..............................................................................}
Function TPointerRefVariantValue.Dereference (Const CalculateData : TCalculateData) : Variant;
Begin
    If FTypeInfo = Nil Then
        Result := GetVoidValue
    Else
        Result := GetValueRef(CalculateData.DebugInfo.Debuger, FTypeInfo.BaseType, GetAddressToUse(CalculateData));
End;
{..............................................................................}

{..............................................................................}
Function TPointerRefVariantValue.TryReference(Const CalculateData : TCalculateData; Const ItemName : AnsiString; Out Value : Variant) : Boolean;
Begin
    Result := PointerTryToCalculate(CalculateData, FTypeInfo, GetAddressToUse(CalculateData), ItemName, Value);
End;
{..............................................................................}

{..............................................................................}
Constructor TPointerVariantValue.Create(TypeInfo : TTypeInfo; Address : TUIntPtr);
Begin
    Inherited Create(TypeInfo);

    FAddress   := Address;
End;
{..............................................................................}

{..............................................................................}
Function TPointerVariantValue.Dereference(Const CalculateData : TCalculateData) : Variant;
Begin
    If FTypeInfo = Nil Then
        Result := GetVoidValue
    Else
        Result := GetValueRef(CalculateData.DebugInfo.Debuger, FTypeInfo.BaseType, FAddress);
End;
{..............................................................................}

{..............................................................................}
Function TPointerVariantValue.AsString(Var ToStringData : TToStringData) : String;
Var
    Address : TPointer;
    Value   : Variant;
Begin
    Address := FAddress;

    If Address = Nil Then
    Begin
        Result := cNil;
        Exit;
    End;

    If (FTypeInfo <> Nil) And (FTypeInfo.BaseType <> Nil) Then
    Begin
        Value := GetValue(ToStringData.DebugInfo.Debuger, FTypeInfo.BaseType, Address, True, (ToStringData.Mode = tsmBrief));
        Result := VariantToString(Value, ToStringData);
    End
    Else
        Result := Format('$%.8x', [Address]);
End;
{..............................................................................}

{..............................................................................}
Function TPointerVariantValue.TryReference(Const CalculateData : TCalculateData; Const ItemName : AnsiString; Out Value : Variant) : Boolean;
Begin
    Result := PointerTryToCalculate(CalculateData, FTypeInfo, FAddress, ItemName, Value);
End;
{..............................................................................}

{..............................................................................}
Function TObjectRefVariantValue.Calculate(Const CalculateData : TCalculateData) : Variant;
Var
    Address : TPointer;
Begin
    Address := ReadAddressValue(CalculateData.DebugInfo.Debuger, FAddress);
    Result := TObjectVariantValue.Create(FTypeInfo, Address) As IUnknown;
End;
{..............................................................................}

{..............................................................................}
Function TObjectRefVariantValue.GetDefaultPropertyIdentifier(Const CalculateData : TCalculateData; Out Address : TPointer; Out MethodName : AnsiString) : Boolean;
Var
    S : AnsiString;
Begin
    S := TryGetDefaultPropertyIdentifier(FTypeInfo);
    Result := S <> '';
    If Result Then
    Begin
        Address := GetAddressToUse(CalculateData);
        MethodName := S;
    End;
End;
{..............................................................................}

{..............................................................................}
Function TObjectRefVariantValue.TryReference(Const CalculateData : TCalculateData; Const ItemName : AnsiString; Out Value : Variant) : Boolean;
Var
    V             : Variant;
    ToStringData  : TToStringData;
    ClassTypeName : String;
    ClassTypeInfo : TTypeInfo;
Begin
    // we need to pass base type to it because it actually describes class
    Result := False;
    If TryCalculateMember(CalculateData, FTypeInfo.BaseType, GetAddressToUse(CalculateData), 'ClassName', V) Then
    Begin
        ToStringData.DebugInfo := CalculateData.DebugInfo;
        ToStringData.Mode := tsmBrief;
        ToStringData.RecursionLevel := 0;
        ClassTypeName := AnsiDequotedStr(VariantToString(V, ToStringData), '''');

        ClassTypeInfo := CalculateData.DebugInfo.GetTypeInfo(ClassTypeName);
        If ClassTypeInfo <> Nil Then
            Result := TryCalculateMember(CalculateData, ClassTypeInfo, GetAddressToUse(CalculateData), ItemName, Value);
    End;
End;
{..............................................................................}

{..............................................................................}
Constructor TObjectVariantValue.Create(TypeInfo : TTypeInfo; Address : TUIntPtr);
Begin
    Inherited Create(TypeInfo);

    FAddress   := Address;
End;
{..............................................................................}

{..............................................................................}
Function TObjectVariantValue.GetDefaultPropertyIdentifier(Const CalculateData : TCalculateData; Out Address : TPointer; Out MethodName : AnsiString) : Boolean;
Var
    S : AnsiString;
Begin
    S := TryGetDefaultPropertyIdentifier(FTypeInfo);
    Result := S <> '';
    If Result Then
    Begin
        Address := GetValueAddress;
        MethodName := S;
    End;
End;
{..............................................................................}

{..............................................................................}
Function TObjectVariantValue.GetValueAddress : TPointer;
Begin
    Result := FAddress;
End;
{..............................................................................}

{..............................................................................}
Function TObjectVariantValue.AsString(Var ToStringData : TToStringData) : String;
Begin
    If FAddress = Nil Then
    Begin
        Result := cNil;
        Exit;
    End;

    Result := StructToString(FTypeInfo.BaseType, FAddress, ToStringData);
End;
{..............................................................................}

{..............................................................................}
Function TObjectVariantValue.TryReference(Const CalculateData : TCalculateData; Const ItemName : AnsiString; Out Value : Variant) : Boolean;
Var
    V             : Variant;
    ToStringData  : TToStringData;
    ClassTypeName : String;
    ClassTypeInfo : TTypeInfo;
Begin
    // we need to pass base type to it because it actually describes class
    Result := False;
    If TryCalculateMember(CalculateData, FTypeInfo.BaseType, FAddress, 'ClassName', V) Then
    Begin
        ToStringData.DebugInfo := CalculateData.DebugInfo;
        ToStringData.Mode := tsmBrief;
        ToStringData.RecursionLevel := 0;
        ClassTypeName := AnsiDequotedStr(VariantToString(V, ToStringData), '''');

        ClassTypeInfo := CalculateData.DebugInfo.GetTypeInfo(ClassTypeName);
        If ClassTypeInfo <> Nil Then
            Result := TryCalculateMember(CalculateData, ClassTypeInfo, FAddress, ItemName, Value);
    End;
    // We had value so we have to return value as well
    If Result Then
        RequireValue(CalculateData, Value);
End;
{..............................................................................}

{..............................................................................}
Constructor TFunctionVariantValue.Create(SelfAddress: TPointer; FuncInfo : TFuncInfo);
Begin
    Inherited Create;

    FSelfAddress := SelfAddress;
    FFuncInfo    := FuncInfo;
End;
{..............................................................................}

{..............................................................................}
Destructor TFunctionVariantValue.Destroy;
Begin
    Finalize(FParams);
    inherited;
End;
{..............................................................................}

{..............................................................................}
Function TFunctionVariantValue.Calculate(Const CalculateData : TCalculateData) : Variant;
Begin
    Try
        Result := CalculateFunction(CalculateData, FParams);
    Finally
        Finalize(FParams);
    End;
End;
{..............................................................................}

{..............................................................................}
Function TFunctionVariantValue.CalculateFunction(Const CalculateData : TCalculateData; Const Params : TCalculateParams) : Variant;
Begin
    Result := EvaluateFunction(CalculateData.DebugInfo.Debuger, FSelfAddress, FFuncInfo, Params);
End;
{..............................................................................}

{..............................................................................}
Function TFunctionVariantValue.CalculateIndex(Const CalculateData : TCalculateData; Const IndexValue : Variant) : Variant;
Var
    I : Integer;
Begin
    I := Length(FParams);
    SetLength(FParams, I + 1);
    FParams[I] := IndexValue;
    Result := IUnknown(Self);
End;
{..............................................................................}

{..............................................................................}
Function TFunctionVariantValue.AsString(Var ToStringData : TToStringData) : String;
Var
    CalculateData : TCalculateData;
Begin
    CalculateData.BriefMode := ToStringData.Mode = tsmBrief;
    CalculateData.DebugInfo := ToStringData.DebugInfo;
    Result := VariantToString(Calculate(CalculateData), ToStringData);
End;
{..............................................................................}

{..............................................................................}
Function TVoidVariantValue.AsString(Var ToStringData : TToStringData) : String;
Begin
    Result := cNoData;
End;
{..............................................................................}

{ TExtendedConstantValue }

{..............................................................................}
constructor TExtendedConstantValue.Create(const AValue: Extended);
begin
  Inherited Create;

  FValue := AValue;
end;
{..............................................................................}

{..............................................................................}
function TExtendedConstantValue.AsString(var ToStringData: TToStringData): String;
begin
  Result := FloatToStr(FValue);
end;
{..............................................................................}

End.
