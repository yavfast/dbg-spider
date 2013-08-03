Unit EvaluateConsts;

Interface

Const
    cMaxEvalRecursionLevel =  4;
    cMaxMembers            = 32;
    cMaxMembersInBriefMode = 16;
    cMaxArrayItems         = 64;

    cNil                   = 'Nil';
    cResult                = 'Result';
    cSelf                  = 'Self';

    cPrefixError   = '[!]';
    cPrefixWarning = '[?]';

    cUnsupportedDataType = 'Unsupported data type';
    cInvalidIntegerSize  = 'Invalid integer size';
    cInvalidFloatNumberSize = 'Invalid size of floating-point type';
    cNotEnoughActualParameters = 'Not enough actual parameters';
    cTooManyActualParameters = 'Too many actual parameters';
    cUndeclaredIdentifier = 'Undeclared identifier or optimization: ''%s''';
    cArrayTypeRequired = 'Array type required';
    cRecordObjectOrClassTypeRequired = 'Record, object or class type required';
    cIdentifierExpected = 'Identifier expected';
    cPointerRequired = 'Pointer type is required';
    cConstantExpressionViolatesSubrangeBounds = 'Constant expression violates subrange bounds';
    cTypeOrFunctionRequired = 'Type or function required';
    cNoData = '(no data)';
    cCannotEvaluateAddress = 'Cannot evaluate address';
    cParameterDoesNotHaveAssignedValue = 'Parameter does not have assigned value';
    cClassDoesNotHaveADefaultProperty = 'Class does not have a default property';
    cCannotFindEvalSupportFunctions = 'Cannot find evaluation support functions';
    cSetExpected = 'Set expression expected';
    cEvaluateTimeout = 'Evaluate timed out';
    cOptimizedValue = 'Cannot evaluate optimized value';

Implementation

End.
