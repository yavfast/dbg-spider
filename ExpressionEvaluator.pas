Unit ExpressionEvaluator;

Interface

Uses
    SysUtils,
    Classes,
    Morfik.dcParser,
    DebugInfoInterfaces,
    Platform.Win32,
    EvaluateTypes,
    DebuggeeControl;
{..............................................................................}

{..............................................................................}
Type
    TCalcExpressionProc = Function : Variant of object;
{..............................................................................}

{..............................................................................}
    TExprParser = Class(TCustomDCParser)
    private
        FBriefMode       : Boolean;
        FDebugInfo       : TAbstractDebugInfo;
        FDebuggeeControl : TDebuggeeControl;

        FUnit            : TUnitInfo;
        FFunc            : TFuncInfo;

        Constructor Create(DebuggeeControl : TDebuggeeControl); Reintroduce; Overload;

        Procedure Error(Const Err : String);
        Function  CalcSimpleExpression : Variant;
        Function  CalcTerm : Variant;
        Procedure CalcDesignatorItem(Var Value : Variant);
        Function  CalcFactor : Variant;
        Function  CalcIdentifier : Variant;
        Procedure DoCalc(OpCode : integer; var CurValue : Variant; Proc : TCalcExpressionProc);

        Procedure Expected(Symbol : Char);
        Function  CalcExpression : Variant;
        Procedure CalcDesignator(Out Value : Variant);
        Procedure CalcReferenceAccess(Var Value : Variant);
        Procedure GetIdentifierValue(Const AName : AnsiString; Var Value : Variant);

        Procedure CalcFunction(Var Value : Variant);
        Procedure CalcIndexedValue(Var Value : Variant);
        Procedure DereferencePointer(Var Value : Variant);
    Public
        Constructor Create(DebuggeeControl : TDebuggeeControl; DebugInfo : TAbstractDebugInfo; UnitInfo : TUnitInfo;
                    FuncInfo : TFuncInfo; BriefMode : Boolean; Const Expression : String); Reintroduce; Overload;

        Function    Calculate : Variant;
        Function    CalculateAsString : String;
    end;
{..............................................................................}

{..............................................................................}
Implementation

Uses
    Math,
    RTLConsts,
    Variants,
    Morfik.dcSystem,
    MorfikIDEAPI,
    StringsHash,
    DebuggerDebuggeeControl,
    Evaluator,
    EvaluateConsts,
    EvaluateProcs,
    System.Debug;
{..............................................................................}

{..............................................................................}
Const
  ocNop            = 0;
  ocAdd            = ocNop + 1;
  ocSub            = ocAdd + 1;
  ocMul            = ocSub + 1;
  ocDiv            = ocMul + 1;
  ocMod            = ocDiv + 1;
  ocLike           = ocMod + 1;
  ocBetween        = ocLike + 1;
  ocShl            = ocBetween + 1;
  ocShr            = ocShl + 1;
  ocNot            = ocShr + 1;
  ocOr             = ocNot + 1;
  ocXor            = ocOr + 1;
  ocAnd            = ocXor + 1;

  ocSlash          = ocAnd + 1;

  ocGreaterEqual   = ocSlash + 1;
  ocEqual          = ocGreaterEqual + 1;
  ocLessEqual      = ocEqual + 1;
  ocNotEqual       = ocLessEqual + 1;
  ocGreater        = ocNotEqual + 1;
  ocLess           = ocGreater + 1;

  ocAssign         = ocLess + 1;
{..............................................................................}

{..............................................................................}
Const
    SErrInvalidConstant = 'Invalid constant';
    SErrSymbol = 'Unexpected symbol : ';
    SErrObjectOrStructExpected = 'Object or structure expected';
    SUnknownProperty = 'Property %s does not exist';
{..............................................................................}

{...............................................................................}
Constructor TExprParser.Create(DebuggeeControl : TDebuggeeControl);
begin
    Inherited Create(Nil);
    InitDelphiSyntax;
    FDebuggeeControl := DebuggeeControl;
end;
{..............................................................................}

{..............................................................................}
Constructor TExprParser.Create(DebuggeeControl : TDebuggeeControl; DebugInfo : TAbstractDebugInfo;
    UnitInfo : TUnitInfo; FuncInfo : TFuncInfo; BriefMode : Boolean; Const Expression : String);
Begin
    Create(DebuggeeControl);

    FDebugInfo := DebugInfo;
    FUnit := UnitInfo;
    FFunc := FuncInfo;
    FBriefMode := BriefMode;

    LinePtr := PChar(Expression);
    NextToken;
End;
{..............................................................................}

{..............................................................................}
Procedure TExprParser.Error(Const Err : String);
Begin
    ExpressionError(Err);
End;
{..............................................................................}

{..............................................................................}
Procedure TExprParser.Expected(Symbol : Char);
Begin
    If Not IsTokenChar(Symbol) Then
        Error(Format('%s expected',[Symbol]));
    NextToken;
End;

{-----------------------------------------------------------}

Procedure TExprParser.DoCalc(OpCode : integer; var CurValue : Variant; Proc : TCalcExpressionProc);
Var
    Val1 : Variant;
    Val2 : Variant;
    CalculateData : TCalculateData;
begin
    NextToken;

    CalculateData.BriefMode       := FBriefMode;
    CalculateData.DebuggeeControl := FDebuggeeControl;
    CalculateData.DebugInfo       := FDebugInfo;

    If OpCode = ocAssign Then
    Begin
        If Not ModifyValue(CurValue, Proc, CalculateData) Then
            ExpressionError('Value can not be modified');

        Exit;
    End;

    Val1 := CalculateValue(CurValue, CalculateData);
    Val2 := CalculateValue(Proc,     CalculateData);

    Case OpCode of
        ocadd   : CurValue := Val1 + val2;
        ocsub   : CurValue := Val1 - val2;
        ocmul   : CurValue := Val1 * val2;
        ocSlash : CurValue := Val1 / val2;
        ocdiv   : CurValue := Val1 div val2;
        ocmod   : CurValue := Val1 mod val2;
        ocshl   : CurValue := Val1 shl val2;
        ocshr   : CurValue := Val1 shr val2;
        ocor    : CurValue := Val1 or val2;
        ocxor   : CurValue := Val1 xor val2;
        ocAnd   : CurValue := Val1 and val2;
        ocGreaterEqual   : CurValue := Val1 >= val2;
        ocEqual          : CurValue := Val1 =  val2;
        ocLessEqual      : CurValue := Val1 <= val2;
        ocNotEqual       : CurValue := Val1 <> val2;
        ocGreater        : CurValue := Val1 >  val2;
        ocLess           : CurValue := Val1 <  val2;
    end;
end;

{-----------------------------------------------------------}

Function TExprParser.CalcExpression : Variant;

    Procedure DoProc(ACode: Integer);
    Begin
        DoCalc(ACode, Result, CalcSimpleExpression);
    End;

begin
    Result := CalcSimpleExpression;

    If IsTokenChar(':') Then
    Begin
        If IsTokenChar2('=') Then
        Begin
            NextToken;
            DoProc(ocAssign);
        End;
    End
    Else If IsTokenChar('=') Then
        DoProc(ocEqual)
    Else If IsTokenChar('>') Then
    Begin
        If IsTokenChar2('=') Then
        Begin
            NextToken;
            DoProc(ocgreaterequal);
        End
        Else
            DoProc(ocgreater);
    End
    Else If IsTokenChar('<') Then
    Begin
        If IsTokenChar2('=') Then
        Begin
            NextToken;
            DoProc(oclessequal)
        End
        Else If IsTokenChar2('>') Then
        Begin
            NextToken;
            DoProc(ocnotequal)
        End
        Else
            DoProc(ocless);
    End;
end;
{..............................................................................}

{..............................................................................}
Function TExprParser.Calculate : Variant;
Begin
    Result := CalcExpression;
End;
{..............................................................................}

{..............................................................................}
Function TExprParser.CalculateAsString : String;
Var
    Value : Variant;
    ToStringData : TToStringData;
Begin
    Value := Calculate;
    ToStringData.DebuggeeControl := FDebuggeeControl;
    ToStringData.DebugInfo := FDebugInfo;
    If FBriefMode Then
    Begin
        ToStringData.Mode := tsmBrief;
        ToStringData.RecursionLevel := 0;
    End
    Else
    Begin
        ToStringData.Mode := tsmNormal;
        ToStringData.RecursionLevel := 1;
    End;
    Result := VariantToString(Value, ToStringData);
End;
{..............................................................................}

{..............................................................................}
Function TExprParser.CalcSimpleExpression : Variant;

  Procedure DoProc(ACode : Integer);
  begin
    DoCalc(ACode, result, CalcTerm);
  end;

begin
  result := CalcTerm;

  while True do
    begin
      if IsTokenChar('+') then
        DoProc(ocAdd)
      else if IsTokenChar('-') then
        DoProc(ocSub)
      else if IsTokenResWord(id_Or) then
        DoProc(ocOr)
      else if IsTokenResWord(id_Xor) then
        DoProc(ocXor)
      else
        break;
    end;
end;

{-----------------------------------------------------------}

Function TExprParser.CalcTerm : Variant;

  Procedure DoProc(ACode : Integer);
  begin
    DoCalc(ACode, result, CalcFactor);
  end;

begin
  result := CalcFactor;

  while True do
    begin
      if IsTokenResWord(id_Shl) then
        DoProc(ocshl)
      else if IsTokenResWord(id_Shr) then
        DoProc(ocshr)
      else if IsTokenResWord(id_And) then
        DoProc(ocAnd)
      else if IsTokenResWord(id_Div) then
        DoProc(ocDiv)
      else if IsTokenResWord(id_Mod) then
        DoProc(ocMod)
      else if IsTokenResWord(id_Like) then
        DoProc(ocLike)
      else if IsTokenChar('*') then
        DoProc(ocmul)
      else if IsTokenChar('/') then
        DoProc(ocSlash)
      else
        break;
    end;
end;
{..............................................................................}

{..............................................................................}
Procedure TExprParser.CalcIndexedValue(Var Value : Variant);
Var
    DefaultProperty : IDefaultProperty;
    ClassTypeInfo   : ITypeInfo;
    CalculateData   : TCalculateData;
    Address         : TPointer;
    MethodName      : AnsiString;
    TypeInfo        : TTypeInfo;
    FuncInfo        : TFuncInfo;
    IndexAccess     : IIndexAccess;
    IndexValue      : Variant;
Begin
    CalculateData.BriefMode := FBriefMode;
    CalculateData.DebuggeeControl := FDebuggeeControl;
    CalculateData.DebugInfo := FDebugInfo;
    If Not Supports(IUnknown(TVarData(Value).VUnknown), IFunction) Then
    Begin
        FinishCalculate(CalculateData, Value);
        If (TVarData(Value).VType = varUnknown)
            And Supports(IUnknown(TVarData(Value).VUnknown), IDefaultProperty, DefaultProperty)
            And Supports(IUnknown(TVarData(Value).VUnknown), ITypeInfo, ClassTypeInfo)
            And DefaultProperty.GetDefaultPropertyIdentifier(CalculateData, Address, MethodName)
        Then
        Begin
            MethodName := FDebugInfo.MakeFuncShortName(MethodName);
            TypeInfo := ClassTypeInfo.GetTypeInfo;
            FuncInfo := Nil;
            While (TypeInfo <> Nil) And Not TryGetFuncInfo(Self.FUnit, FDebugInfo.MakeFuncDbgFullName(TypeInfo.Name, MethodName), FuncInfo) Do
                TypeInfo := TypeInfo.BaseType;
            If FuncInfo <> Nil Then
                Value := TFunctionVariantValue.Create(Address, FuncInfo) As IUnknown;
        End;
    End;
    While True Do
    Begin
        IndexValue := CalcSimpleExpression;
        Case VarType(Value) Of
            varString :
                Value := String(Value)[ConvertToInteger(IndexValue)];
            varOleStr :
                Value := WideString(Value)[ConvertToInteger(IndexValue)];
            varUnknown :
                If Supports(IUnknown(TVarData(Value).VUnknown), IIndexAccess, IndexAccess) Then
                    Value := IndexAccess.CalculateIndex(CalculateData, IndexValue)
                Else
                    Error(cArrayTypeRequired);
        Else
            Error(cArrayTypeRequired);
        End;
        If Not IsTokenChar(',') Then
            Break;
        NextToken;
    End;
    Expected(']');
End;

{-----------------------------------------------------------}

Procedure TExprParser.DereferencePointer(Var Value : Variant);
Var
    CalculateData : TCalculateData;
    Dereference   : IDereference;
Begin
    CalculateData.BriefMode := FBriefMode;
    CalculateData.DebuggeeControl := FDebuggeeControl;
    CalculateData.DebugInfo := FDebugInfo;
    FinishCalculate(CalculateData, Value);
    If (VarType(Value) = varUnknown) And Supports(IUnknown(TVarData(Value).VUnknown), IDereference, Dereference) Then
        Value := Dereference.Dereference(CalculateData)
    Else
        Error(cPointerRequired);
End;
{..............................................................................}

{..............................................................................}
Procedure TExprParser.CalcFunction(Var Value : Variant);
Var
    CalculateData : TCalculateData;
    FunctionInft  : IFunction;
    LenParams     : Integer;
    Params        : TCalculateParams;

    Procedure _AddParam;
    Begin
        SetLength(Params, LenParams + 1);
        Params[LenParams] := CalcExpression;
        Inc(LenParams);
    End;
Begin
    If (VarType(Value) <> varUnknown) Or Not Supports(IUnknown(TVarData(Value).VUnknown), IFunction, FunctionInft) Then
        Error(cTypeOrFunctionRequired);

    SetLength(Params, 0);
    LenParams := 0;
    If Not IsTokenChar(')') Then
    Begin
        _AddParam;
        While IsTokenChar(',') Do
        Begin
            NextToken;
            _AddParam;
        End;
    End;
    Expected(')');
    CalculateData.BriefMode := FBriefMode;
    CalculateData.DebuggeeControl := FDebuggeeControl;
    CalculateData.DebugInfo := FDebugInfo;
    Value := FunctionInft.CalculateFunction(CalculateData, Params);
End;
{..............................................................................}

{..............................................................................}
Procedure TExprParser.CalcDesignatorItem(Var Value : Variant);
Begin
    While True Do
    Begin
        If IsTokenChar('[') Then
        Begin
            NextToken;
            CalcIndexedValue(Value);
        End
      else if IsTokenChar('.') then
        begin
          NextToken; // Get ident token
          CalcReferenceAccess(Value);
        end
      else if IsTokenChar('^') then
        begin
          NextToken;
          DereferencePointer(Value);
        end
      else if IsTokenChar('(') then
      Begin
        NextToken;
        CalcFunction(Value);
      End
      else
        break;
    End;
End;
{..............................................................................}

{..............................................................................}
// We can't require value at the end of function because @Factor suppose to return value
Function TExprParser.CalcFactor : Variant;
var
  ConvertResult : integer;
  i             : integer;
begin
    Case Token Of
        //TODO: Control string may follow quoted string
        tokString :
            Begin
                Result := ExtractQuotedStr(TokenString, '''');
                NextToken;
            End;
        tokInteger :
            Begin
                Result := StrToInt(TokenString);
                NextToken;
            End;
        tokFloat :
            Begin
                Result := GetFloatFromString(TokenString);
                NextToken;
            End;
        tokWhiteChar :
            Case TokenPtr^ Of
                //TODO: Add support for @
                //TODO: Add support for sets
                '(':
                    Begin
                        NextToken; // Skip (
                        result := CalcExpression;
                        Expected(')');
                    End;
                '+' :
                    Begin
                        NextToken;
                        result := CalcFactor;
                    End;
                '-' :
                    Begin
                        NextToken;
                        result := - CalcFactor;
                    End;
                '#' :
                    begin
                        //TODO Check that #13#10 works fine
                        //TODO After #13 maybe normal quoted string
                        NextToken; // Skip #
                        if Token <> tokInteger then
                            Error(SErrInvalidConstant);

                        Val(TokenString, i, ConvertResult);
                        if ConvertResult <> 0 then
                            Error(SErrInvalidConstant);
                        Result := i;
                        NextToken;
                    end
                else
                  Error(SErrSymbol + TokenPtr^);
              end;
        tokResWord :
            case ResWordID of
                id_nil :
                    Begin
                        Result := GetValue(Nil, Nil, 0, False, FBriefMode);
                        NextToken;
                    End;
                id_not :
                    Begin
                        NextToken; // Skip Not
                        Result := Not CalcFactor;
                        NextToken;
                    End
                else
                    Error(SErrSymbol + TokenString);
            end;
        tokSymbol :
            result := CalcIdentifier;
    End;
end;

{-----------------------------------------------------------}

Function TExprParser.CalcIdentifier : Variant;
Var
  ts : String;
Begin
    ts := TokenString;
    if CompareText(ts, 'True') = 0 then //don't resource
    Begin
        result := True;
        NextToken;
    End
    else if CompareText(ts, 'False') = 0 then //don't resource
    Begin
        result := False;
        NextToken;
    End
    else
        CalcDesignator(result);
End;

{-----------------------------------------------------------}

Procedure TExprParser.CalcDesignator(Out Value : Variant);
Var
    vname : AnsiString;
    CalculateData : TCalculateData;
Begin
    vname := AnsiString(TokenString);
    GetIdentifierValue(vName, Value);
    NextToken;
    CalcDesignatorItem(Value);
    CalculateData.BriefMode := FBriefMode;
    CalculateData.DebuggeeControl := FDebuggeeControl;
    CalculateData.DebugInfo := FDebugInfo;
    FinishCalculate(CalculateData, Value);
End;

{-----------------------------------------------------------}

Procedure TExprParser.CalcReferenceAccess(Var Value : Variant);
Var
    CalculateData   : TCalculateData;
    Identifier      : AnsiString;
    ReferenceAccess : IReferenceAccess;
Begin
    CalculateData.BriefMode := FBriefMode;
    CalculateData.DebuggeeControl := FDebuggeeControl;
    CalculateData.DebugInfo := FDebugInfo;
    If Token <> tokSymbol Then
        Error(cIdentifierExpected);
    FinishCalculate(CalculateData, Value);
    If (TVarData(Value).VType <> varUnknown) Or Not Supports(IUnknown(TVarData(Value).VUnknown), IReferenceAccess, ReferenceAccess) Then
        Error(cRecordObjectOrClassTypeRequired);
    Identifier := AnsiString(TokenString);
    If Not ReferenceAccess.TryReference(CalculateData, Identifier, Value) Then
        Error(Format(cUndeclaredIdentifier, [Identifier]));
    NextToken;
End;
{-----------------------------------------------------------}

{...............................................................................}
Procedure TExprParser.GetIdentifierValue(Const AName : AnsiString; Var Value : Variant);

    Procedure CheckConstExpr;
    Var
        ConstExpr : String;
    Begin
        If (VarType(Value) = varUString) And (Value = '#CONST#') Then
            If IDEAPI_GetConstExpression(String(AName), ConstExpr) Then
            Begin
                Value := FDebugInfo.Evaluate(FDebuggeeControl, FBriefMode, ConstExpr, INFINITE);
                Case VarType(Value) Of varString, varUString :
                    Value := AnsiDequotedStr(Value, '''');
                End;
            End;
    End;

Begin
    If FUnit <> Nil Then
    Begin
        If FFunc <> Nil Then
        Begin
            // Firstly we are trying to check if this is local const
            If TryGetConstant(FFunc, AName, Value) Then
            Begin
                CheckConstExpr;
                Exit;
            End;
            // Then we are trying to check if this is local variable
            If TryGetVariable(FDebuggeeControl, FFunc, AName, Value, FBriefMode) Then
                Exit;
            // Then we are trying to check if this is class field
            If TryEvaluateClassField(FDebuggeeControl, FFunc, AName, Value, FBriefMode) Then
                Exit;
            // Then we are trying to check if this is class method
            If (Not FBriefMode) And TryEvaluateClassMethod(FDebuggeeControl, FDebugInfo.MakeFuncDbgFullName, FFunc, AName, Value) Then
                Exit;
        End;
        // Next we are trying to check if this is unit constant
        If TryGetConstant(FUnit, AName, Value) Then
        Begin
            CheckConstExpr;
            Exit;
        End;
        // Next we are trying to check if this is unit enum value
        If TryGetEnumValue(FUnit, AName, Value) Then
            Exit;
        // Next we are trying to check if this is unit variable
        If TryGetVariable(FDebuggeeControl, FUnit, AName, Value, FBriefMode) Then
            Exit;
        // Next we are trying to check if this is unit function
        If TryGetFunction(FUnit, AName, Value) Then
            Exit;
    End;

    // Next we are trying to check if this global function
    If TryGetFunction(FDebugInfo.GetFuncs, AName, Value) Then
        Exit;

    Raise TMorfikDebugInternalException.CreateFmt(cUndeclaredIdentifier, [AName]);
End;
{...............................................................................}

{...............................................................................}
End.
