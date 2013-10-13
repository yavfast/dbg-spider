unit DelphiDebugInfo;

Interface

Uses
  SysUtils, Windows, Classes, DebugInfo, Debuger, DebugerTypes, JclTD32Ex;
{ .............................................................................. }

{ .............................................................................. }
Type
  TDelphiVersion = (dvAuto = 0, dvD1 = 8, dvD2 = 9, dvD3 = 10, dvD4 = 12, dvD5 = 13, dvD6 = 14, dvD7 = 15, dvD8 = 16, dvD2005 = 17, dvD2006_7 = 18,
    dvD2009 = 20, dvD2010 = 21, dvDXE = 22, dvDXE2 = 23, dvDXE3 = 24, dvDXE4 = 25);

  TDelphiDebugInfo = Class(TDebugInfo)
  Private
    FImage: TJclPeBorTD32Image;
    FDelphiVersion: TDelphiVersion;
    FSystemUnits: TStringList;
    FAddressInfoList: TAddressInfoList;

    Function ImageBase: Cardinal;
    Function ImageNames(const Index: TNameId): AnsiString;
    function LoadVar(UnitInfo: TUnitInfo; VarSymbol: TJclTD32NamedSymbol; Func: TFuncInfo): TVarInfo;
    procedure LoadFunc(UnitInfo: TUnitInfo; FuncSymbol: TJclTD32ProcSymbolInfo);
    procedure LoadSymbols(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
    function GetUnitFileName(const UnitName: String): String;
    procedure LoadConst(OwnerInfo: TSegmentCodeInfo; ConstSymbol: TJclTD32ConstantSymbolInfo);
    procedure LoadLines(UnitInfo: TUnitInfo; Source: TJclTD32SourceModuleInfo);
    procedure LoadSegments(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
    function LoadType(UnitInfo: TUnitInfo; const TypeIndex: Integer; out DstType: TTypeInfo): Integer;
    procedure LoadUsedUnits(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
    function RegisterIndex(const Index: Byte): Integer;

    Function ParseUnit(Module: TJclTD32ModuleInfo): TUnitInfo;

    Procedure ResolveUnits;

    Function FindUnitByAddr(const Addr: Pointer): TUnitInfo;
    Function FindFuncByAddr(const UnitInfo: TUnitInfo; const Addr: Pointer): TFuncInfo;
    Function FindLineByAddr(const FuncInfo: TFuncInfo; const Addr: Pointer; const GetPrevLine: Boolean = False): TLineInfo;

    function CustomVariantAsString(const Value: Variant): String;
    procedure SetDelphiVersion(const Value: TDelphiVersion);
    procedure InitCodeTracking;
    procedure FillSystemUnits;
  Protected
    Function DoReadDebugInfo(Const FileName: String; ALoadDebugInfo: Boolean): Boolean; Override;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Function GetNameById(const Idx: TNameId): AnsiString; override;

    function ParseUnitName(UnitInfo: TUnitInfo; const WithExt: Boolean = True): String; override;
    function ParseFuncName(FuncInfo: TFuncInfo): String; override;
    function ParseTypeName(TypeInfo: TTypeInfo): String; override;
    function ParseConstName(ConstInfo: TConstInfo): String; override;
    function ParseVarName(VarInfo: TVarInfo): String; override;
    function ParseStructMemberName(StructMember: TStructMember): String; override;

    Procedure ClearDebugInfo; Override;

    Function HasDebugInfo(Const FileName: String): Boolean; Override;

    Function GetAddrInfo(Var Addr: Pointer; Const FileName: String; Line: Cardinal): TFindResult; Override;

    Function GetLineInfo(const Addr: Pointer; Var UnitInfo: TUnitInfo; Var FuncInfo: TFuncInfo; Var LineInfo: TLineInfo; GetPrevLine: Boolean): TFindResult; Override;
    Function GetLineInformation(const Addr: Pointer; Var UnitName: String; Var FuncName: String; Var Line: LongInt; GetPrevLine: Boolean): TFindResult; Override;

    Function MakeFuncDbgFullName(Const ClassName, MethodName: AnsiString): AnsiString; Override;
    Function MakeFuncShortName(Const MethodName: AnsiString): AnsiString; Override;
    Function MakeFuncNativeName(Const MethodName: AnsiString): AnsiString; Override;

    Function Evaluate(BriefMode: Boolean; Const Expression: String; Const TimeOut: Cardinal = INFINITE): String; Override;

    Function VarValueAsString(const Value: Variant): String; override;

    Procedure InitDebugHook; Override;

    Function CheckAddr(Const Addr: Pointer): Boolean; Override;

    Function GetClassName(ObjectPtr: Pointer): String; Override;
    Function GetExceptionName(ExceptionRecord: PExceptionRecord): String; Override;
    Function GetExceptionMessage(ExceptionRecord: PExceptionRecord; Const ThreadId: TThreadId): String; Override;
    Function GetExceptionAddress(ExceptionRecord: PExceptionRecord): Pointer; Override;
    Function GetExceptionFrame(ExceptionRecord: PExceptionRecord): Pointer; Override;
    Function IsDelphiException(ExceptionRecord: PExceptionRecord): Boolean;
    Function IsDelphiTraceException(ExceptionRecord: PExceptionRecord): Boolean;
    Function CheckDebugException(ExceptionRecord: PExceptionRecord; Var IsTraceException: Boolean): Boolean; Override;
    Function CheckSystemFile(Const FileName: String): Boolean; Override;

    property DelphiVersion: TDelphiVersion read FDelphiVersion write SetDelphiVersion;
  End;
  { .............................................................................. }

  { .............................................................................. }
Function HasDelphiDebugInfo(Const AFileName: String): Boolean;
{ .............................................................................. }

{ .............................................................................. }
Implementation

Uses
  JclDebug,
  JclPeImage,
  // ApiConsts,
  // DebugInfoUtils,
  EvaluateProcs,
  EvaluateTypes,
  // ExpressionEvaluator,
  Math, Variants,
  // Morfik.dcSystem,
  ClassUtils, DebugHook, StrUtils, System.Contnrs;
{ .............................................................................. }

{ .............................................................................. }
Const
  cContinuable = 0;
  cNonContinuable = 1;
  cDelphiException = DWORD($0EEDFADE);
  cDelphiReRaise = DWORD($0EEDFADF);
  cDelphiExcept = DWORD($0EEDFAE0);
  cDelphiFinally = DWORD($0EEDFAE1);
  cDelphiTerminate = DWORD($0EEDFAE2);
  cDelphiUnhandled = DWORD($0EEDFAE3);
  cNonDelphiException = DWORD($0EEDFAE4);
  cDelphiExitFinally = DWORD($0EEDFAE5);
  { .............................................................................. }

  { ............................................................................... }
Function HasDelphiDebugInfo(Const AFileName: String): Boolean;
Var
  PEImage: TJclPeBorTD32Image;
Begin
  Result := FileExists(AFileName);
  If Result Then
  Begin
    PEImage := TJclPeBorTD32Image.Create(True);
    Try
      PEImage.FileName := AFileName;
      Result := PEImage.IsTD32DebugPresent;
    Finally
      PEImage.Free;
    End;
  End;
End;
{ ............................................................................... }

{ TDelphiDebugInfo }

{ ............................................................................... }
Constructor TDelphiDebugInfo.Create;
Begin
  Inherited Create;

  FImage := Nil;
  FDelphiVersion := dvAuto;
  FSystemUnits := TStringList.Create;
  FAddressInfoList := TAddressInfoList.Create(16 * 1024);

  FillSystemUnits;
End;

function TDelphiDebugInfo.CustomVariantAsString(const Value: Variant): String;
var
  CustomVariantData: ICustomVariantData;
  ToStringData: TToStringData;
begin
  If Supports(IUnknown(TVarData(Value).VUnknown), ICustomVariantData, CustomVariantData) Then
  begin
    ToStringData.DebugInfo := Self;
    ToStringData.Mode := tsmBrief;
    ToStringData.RecursionLevel := 0;

    Result := CustomVariantData.AsString(ToStringData);
  end
  Else
    Result := 'Unsupported data type';
end;
{ ............................................................................... }

{ ............................................................................... }
Destructor TDelphiDebugInfo.Destroy;
Begin
  FreeAndNil(FImage);
  FreeAndNil(FSystemUnits);
  FreeAndNil(FAddressInfoList);

  Inherited;
End;
{ ............................................................................... }

{ ............................................................................... }
function TDelphiDebugInfo.ParseConstName(ConstInfo: TConstInfo): String;
var
  SL: TStringArray;
begin
  Result := inherited;

  if FUseShortNames then
  begin
    SplitStr(Result, '@', SL);
    Result := SL[ High(SL)];
  end;
end;

function TDelphiDebugInfo.ParseFuncName(FuncInfo: TFuncInfo): String;
var
  SL: TStringArray;
  Idx: Integer;
  S: String;
  P: Integer;
begin
  Result := inherited;

  if FUseShortNames then
  begin
    SplitStr(Result, '@', SL);

    Result := '';
    for Idx := 0 to High(SL) do
    begin
      S := SL[Idx];
      if S <> '' then
      begin
        if Result <> '' then
          Result := Result + '.';

        P := Pos('$qq', S);
        if P > 0 then
          SetLength(S, P - 1);

        Result := Result + S;

        if P > 0 then
          Break;
      end;
    end;
  end;
end;

function TDelphiDebugInfo.ParseStructMemberName(StructMember: TStructMember): String;
begin
  Result := inherited;
end;

function TDelphiDebugInfo.ParseTypeName(TypeInfo: TTypeInfo): String;
var
  SL: TStringArray;
  Idx: Integer;
  S: String;
  P: Integer;
begin
  Result := inherited;

  if FUseShortNames then
  begin
    SplitStr(Result, '@', SL);

    Result := '';
    for Idx := 0 to High(SL) do
    begin
      S := SL[Idx];
      if S <> '' then
      begin
        if Result <> '' then
          Result := Result + '.';

        repeat
          P := Pos('$', S);
          if P > 0 then
            Delete(S, P, 3);
        until P <= 0;

        Result := Result + S;
      end;
    end;
  end;
end;

Function TDelphiDebugInfo.ParseUnit(Module: TJclTD32ModuleInfo): TUnitInfo;
Var
  I: Integer;
  // UnitFileName: String;
  // ModuleName: AnsiString;
  SourceModuleInfo: TJclTD32SourceModuleInfo;
Begin
  Result := TUnitInfo.Create;
  Result.SymbolInfo := Module;

  If Module.SourceModuleCount > 0 Then
    Result.NameId := Module.SourceModules[0].NameIndex
  Else
    Result.NameId := Module.NameIndex;

  // Result.FullUnitName := GetUnitFileName(String(Result.Name));
  Units.AddObject(Result.ShortName, Result);
  UnitsByAddr.Add(Result);

  LoadSegments(Result, Module);
  LoadUsedUnits(Result, Module);
  LoadSymbols(Result, Module);

  If Module.SourceModuleCount > 0 Then
  Begin
    For I := 0 To Module.SourceModuleCount - 1 Do
    Begin
      SourceModuleInfo := Module.SourceModules[I];
      LoadLines(Result, SourceModuleInfo);
    End;
  End;
End;

function TDelphiDebugInfo.ParseUnitName(UnitInfo: TUnitInfo; const WithExt: Boolean = True): String;
begin
  Result := inherited;
end;

function TDelphiDebugInfo.ParseVarName(VarInfo: TVarInfo): String;
var
  SL: TStringArray;
  Idx: Integer;
begin
  Result := inherited;

  if FUseShortNames then
  begin
    SplitStr(Result, '@', SL);

    Result := '';
    for Idx := 0 to High(SL) do
      if SL[Idx] <> '' then
      begin
        if Result <> '' then
          Result := Result + '.';

        Result := Result + SL[Idx];
      end;
  end;
end;
{ ............................................................................... }

Function TDelphiDebugInfo.GetUnitFileName(Const UnitName: String): String;
Var
  S: String;
  Ext: String;
  ST: TUnitType;
Begin
  S := AnsiLowerCase(ExtractFileName(UnitName));

  Ext := ExtractFileExt(S);
  If (Ext <> '.pas') and (Ext <> '.inc') Then
    S := S + '.pas';

  for ST := Low(TUnitType) to High(TUnitType) do
    if Dirs[ST].TryGetValue(S, Result) then
      Exit;

  Result := S;
End;

function TDelphiDebugInfo.GetNameById(const Idx: TNameId): AnsiString;
begin
  Result := ImageNames(Idx);
end;

Procedure TDelphiDebugInfo.LoadSegments(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
Var
  I: Integer;
  SegmentInfo: TSegmentInfo;
  S: TUnitSegmentInfo;
Begin
  UnitInfo.Segments.Capacity := Module.SegmentCount;
  For I := 0 To Module.SegmentCount - 1 Do
  Begin
    SegmentInfo := Module.Segment[I];

    S := TUnitSegmentInfo.Create;
    S.Offset := SegmentInfo.Offset + FImage.ImageSectionHeaders[SegmentInfo.Segment - 1].VirtualAddress + ImageBase;

    S.Size := SegmentInfo.Size;
    S.SegType := TUnitSegmentType(SegmentInfo.Flags);

    if (S.SegType = ustCode) and ((UnitInfo.Address = Nil) or (Cardinal(UnitInfo.Address) > S.Offset)) then
      UnitInfo.Address := Pointer(S.Offset);

    case S.SegType of
      ustData:
        Inc(UnitInfo.DataSize, S.Size);
      ustCode:
        Inc(UnitInfo.CodeSize, S.Size);
      else
        RaiseDebugCoreException('');
    end;

    UnitInfo.Segments.Add(S);
  End;
End;

Procedure TDelphiDebugInfo.LoadUsedUnits(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
Var
  I: Integer;
  Idx: Integer;
  Name: String;
  UName: String;
Begin
  UnitInfo.UsedUnits.Capacity := Module.UsedModuleNameIndexCount;
  For I := 0 To Module.UsedModuleNameIndexCount - 1 Do
  Begin
    Idx := Module.UsedModuleNameIndices[I];
    Name := String(ImageNames(Idx));
    UName := GetUnitFileName(Name);
    UnitInfo.UsedUnits.Add(UName);
  End;
End;

Procedure TDelphiDebugInfo.LoadLines(UnitInfo: TUnitInfo; Source: TJclTD32SourceModuleInfo);
Var
  I: Integer;
  LineInfo: TJclTD32LineInfo;
  L: TLineInfo;
  F: TFuncInfo;
Begin
  UnitInfo.Lines.Capacity := UnitInfo.Lines.Capacity + Source.LineCount;
  For I := 0 To Source.LineCount - 1 Do
  Begin
    LineInfo := Source.Line[I];

    L := TLineInfo.Create;
    L.LineNo := LineInfo.LineNo; // - 1; ???
    L.Address := Pointer(LineInfo.Offset + FImage.ImageSectionHeaders[LineInfo.Segment - 1].VirtualAddress + ImageBase);
    UnitInfo.Lines.Add(L);

    F := FindFuncByAddr(UnitInfo, L.Address);
    If F <> Nil Then
      F.Lines.Add(L);
  End;
End;

Function TDelphiDebugInfo.LoadType(UnitInfo: TUnitInfo; const TypeIndex: Integer; out DstType: TTypeInfo): Integer;
Var
  SrcType: TJclSymbolTypeInfo;
  SrcList: TJclSymbolTypeInfo;
  SrcMember: TJclTD32MemberSymbolInfo;
  SrcMemberType: TJclSymbolTypeInfo;
  DstMember: TStructMember;
  DstTypeMember: TStructMember;
  SrcEnum: TJclEnumerateSymbolInfo;
  EnumMember: TEnumInfo;
  I, J: Integer;
Begin
  SrcType := FImage.TD32Scanner.SymbolTypes[TypeIndex];

  If (SrcType <> Nil) and (SrcType.UnitInfo = UnitInfo) Then
  begin
    Result := SrcType.UnitInfoIndex;
    DstType := TTypeInfo(UnitInfo.Types[Result]);
    Exit;
  end;

  DstType := TTypeInfo.Create;
  DstType.Kind := tkVoid;
  DstType.UnitInfo := UnitInfo;
  DstType.NameId := -1;
  DstType.SymbolInfo := SrcType;

  DstType.TypeInfoIdx := UnitInfo.Types.Add(DstType);
  Result := DstType.TypeInfoIdx;

  if SrcType = Nil then
    Exit;

  SrcType.UnitInfo := UnitInfo;
  SrcType.UnitInfoIndex := DstType.TypeInfoIdx;

  DstType.NameId := SrcType.NameIndex;
  Case SrcType.Kind Of
    stkBoolean:
      Begin
        DstType.Kind := tkBoolean;
        DstType.DataSize := SrcType.DataSize;
        DstType.MinValue := 0;
        DstType.MaxValue := 1;
      End;
    stkWordBool:
      Begin
        DstType.Kind := tkWordBool;
        DstType.DataSize := SrcType.DataSize;
        DstType.MinValue := 0;
        DstType.MaxValue := 1;
      End;
    stkLongBool:
      Begin
        DstType.Kind := tkLongBool;
        DstType.DataSize := SrcType.DataSize;
        DstType.MinValue := 0;
        DstType.MaxValue := 1;
      End;
    stkPString:
      Begin
        DstType.Kind := tkPString;
        DstType.DataSize := SizeOf(ShortString);
        LoadType(UnitInfo, SrcType.IndexType, DstType.BaseType);
      End;
    stkLString:
      Begin
        DstType.Kind := tkLString;
        DstType.DataSize := SizeOf(AnsiString);
        LoadType(UnitInfo, SrcType.ElementType, DstType.BaseType);
      End;
    stkWString:
      Begin
        DstType.Kind := tkWString;
        DstType.DataSize := SizeOf(WideString);
        LoadType(UnitInfo, SrcType.ElementType, DstType.BaseType);
      End;
    stkShortInt:
      Begin
        DstType.Kind := tkShortInt;
        DstType.DataSize := SrcType.DataSize;
        DstType.MinValue := Low(ShortInt);
        DstType.MaxValue := High(ShortInt);
      End;
    stkSmallInt:
      Begin
        DstType.Kind := tkSmallInt;
        DstType.DataSize := SrcType.DataSize;
        DstType.MinValue := Low(SmallInt);
        DstType.MaxValue := High(SmallInt);
      End;
    stkInteger:
      Begin
        DstType.Kind := tkInteger;
        DstType.DataSize := SrcType.DataSize;
        DstType.MinValue := Low(Integer);
        DstType.MaxValue := High(Integer);
      End;
    stkByte:
      Begin
        DstType.Kind := tkByte;
        DstType.DataSize := SrcType.DataSize;
        DstType.MinValue := Low(Byte);
        DstType.MaxValue := High(Byte);
      End;
    stkWord:
      Begin
        DstType.Kind := tkWord;
        DstType.DataSize := SrcType.DataSize;
        DstType.MinValue := Low(Word);
        DstType.MaxValue := High(Word);
      End;
    stkCardinal:
      Begin
        DstType.Kind := tkCardinal;
        DstType.DataSize := SrcType.DataSize;
        DstType.MinValue := Low(Cardinal);
        Cardinal(DstType.MaxValue) := High(Cardinal);
      End;
    stkInt64:
      Begin
        DstType.Kind := tkInt64;
        DstType.DataSize := SrcType.DataSize;
      End;
    stkUInt64:
      Begin
        DstType.Kind := tkUInt64;
        DstType.DataSize := SrcType.DataSize;
      End;
    stkSingle:
      Begin
        DstType.Kind := tkSingle;
        DstType.DataSize := SrcType.DataSize;
      End;
    stkReal48:
      Begin
        DstType.Kind := tkReal48;
        DstType.DataSize := SrcType.DataSize;
      End;
    stkReal:
      Begin
        DstType.Kind := tkReal;
        DstType.DataSize := SrcType.DataSize;
      End;
    stkExtended:
      Begin
        DstType.Kind := tkExtended;
        DstType.DataSize := SrcType.DataSize;
      End;
    stkCurrency:
      Begin
        DstType.Kind := tkCurrency;
        DstType.DataSize := SrcType.DataSize;
      End;
    stkChar:
      Begin
        DstType.Kind := tkChar;
        DstType.DataSize := SrcType.DataSize;
        DstType.MinValue := 0;
        DstType.MaxValue := Ord( High(AnsiChar));
      End;
    stkWideChar:
      Begin
        DstType.Kind := tkWideChar;
        DstType.DataSize := SrcType.DataSize;
        DstType.MinValue := Low(Word);
        DstType.MaxValue := High(Word);
      End;
    stkPointer:
      Begin
        DstType.Kind := tkPointer;
        DstType.DataSize := SrcType.DataSize;
        If SrcType.ElementType <> 0 Then
        Begin
          LoadType(UnitInfo, SrcType.ElementType, DstType.BaseType);
          If DstType.BaseType.Kind = tkClass Then
            DstType.Kind := tkObject
          Else If (DstType.BaseType.Kind = tkArray) And (DstType.BaseType.DataSize = -1) Then
            DstType.Kind := tkDynamicArray;
        End;
      End;
    stkClosure:
      Begin
        DstType.Kind := tkPointer;
        DstType.DataSize := SrcType.DataSize;
      End;
    stkClass:
      Begin
        DstType.Kind := tkClass;
        SrcList := FImage.TD32Scanner.SymbolTypes[SrcType.Elements];
        If SrcList.ElementType <> 0 Then
          LoadType(UnitInfo, SrcList.ElementType, DstType.BaseType);

        DstType.Members := TNameList.Create;
        DstType.Members.Capacity := SrcList.Members.Count;
        For I := 0 To SrcList.Members.Count - 1 Do
        Begin
          //DstMember := Nil;
          SrcMember := TJclTD32MemberSymbolInfo(SrcList.Members[I]);

          SrcMemberType := FImage.TD32Scanner.SymbolTypes[SrcMember.TypeIndex];

          if SrcMemberType = Nil then
          begin
            // TODO: Что-то здесь непонятное в XE4 появилось
            Continue;
          end;

          DstMember := TStructMember.Create;
          DstMember.NameId := SrcMember.NameIndex;
          DstMember.SymbolInfo := SrcMember;

          Case SrcMember.Flags And 3 Of
            0, 3:
              DstMember.Scope := msPublic;
            1:
              DstMember.Scope := msPrivate;
            2:
              DstMember.Scope := msProtected;
          End;

          If SrcMemberType.Kind = stkClassRef Then
            LoadType(UnitInfo, SrcMemberType.ElementType, DstMember.DataType);

          If SrcMemberType.Kind <> stkProperty Then
          Begin
            If SrcMemberType.Kind <> stkClassRef Then
              LoadType(UnitInfo, SrcMember.TypeIndex, DstMember.DataType);
            DstMember.BitOffset := SrcMember.Offset * 8;
            DstMember.BitLength := DstMember.DataType.DataSize * 8;
          End
          Else
          Begin
            LoadType(UnitInfo, SrcMemberType.ElementType, DstMember.DataType);

            DstMember.IsDefault := (SrcMemberType.Flags And 1) = 1;

            If (SrcMemberType.Flags And 2) = 2 Then
              DstMember.MethodNameId := SrcMemberType.MinValue
            Else
            Begin
              DstMember.BitOffset := SrcMemberType.MinValue * 8;
              DstMember.BitLength := DstMember.DataType.DataSize * 8;
            End;

            For J := 0 To DstType.Members.Count - 1 Do
            Begin
              DstTypeMember := TStructMember(DstType.Members[J]);
              If (DstTypeMember.BitOffset Div 8) = SrcMemberType.MinValue Then
              Begin
                // TODO: Возможно, надо здесь надо указывать на всю структуру DstTypeMember
                DstMember.AliasNameId := DstTypeMember.NameId;
                Break;
              End;
            End;
          End;

          If DstMember <> Nil Then
            DstType.Members.Add(DstMember);
        End;
      End;
    stkStructure:
      Begin
        DstType.Kind := tkStructure;
        DstType.DataSize := SrcType.DataSize;
        SrcList := FImage.TD32Scanner.SymbolTypes[SrcType.Elements];

        DstType.Members := TNameList.Create;
        DstType.Members.Capacity := SrcList.Members.Count;
        For I := 0 To SrcList.Members.Count - 1 Do
        Begin
          SrcMember := TJclTD32MemberSymbolInfo(SrcList.Members[I]);

          DstMember := TStructMember.Create;
          DstMember.NameId := SrcMember.NameIndex;
          DstMember.SymbolInfo := SrcMember;
          DstMember.Scope := msPublic;

          LoadType(UnitInfo, SrcMember.TypeIndex, DstMember.DataType);

          DstMember.BitOffset := SrcMember.Offset * 8;
          DstMember.BitLength := DstMember.DataType.DataSize * 8;

          DstType.Members.Add(DstMember);
        End;
      End;
    stkEnum:
      Begin
        DstType.Kind := tkEnum;

        DstType.Elements := TNameList.Create;

        DstType.DataSize := FImage.TD32Scanner.SymbolTypes[SrcType.ElementType].DataSize;
        DstType.MinValue := High(DstType.MinValue);
        DstType.MaxValue := Low(DstType.MaxValue);

        SrcList := FImage.TD32Scanner.SymbolTypes[SrcType.Elements];
        DstType.Elements.Capacity := SrcList.Members.Count;
        For I := 0 To SrcList.Members.Count - 1 Do
        Begin
          SrcEnum := TJclEnumerateSymbolInfo(SrcList.Members[I]);

          EnumMember := TEnumInfo.Create;
          EnumMember.NameId := SrcEnum.NameIndex;
          EnumMember.SymbolInfo := SrcEnum;

          EnumMember.TypeInfo := DstType;
          EnumMember.OrderValue := SrcEnum.Value;

          DstType.Elements.Add(EnumMember);

          If SrcEnum.Value < DstType.MinValue Then
            DstType.MinValue := SrcEnum.Value;
          If SrcEnum.Value > DstType.MaxValue Then
            DstType.MaxValue := SrcEnum.Value;
        End;
      End;
    stkSet:
      Begin
        DstType.Kind := tkSet;
        LoadType(UnitInfo, SrcType.ElementType, DstType.BaseType);
        DstType.DataSize := SrcType.DataSize;
      End;
    stkSubRange:
      Begin
        DstType.DataSize := SrcType.DataSize;
        DstType.MinValue := SrcType.MinValue;
        DstType.MaxValue := SrcType.MaxValue;
        SrcList := FImage.TD32Scanner.SymbolTypes[SrcType.IndexType];
        Case SrcList.Kind Of
          stkBoolean, stkWordBool, stkLongBool:
            Case DstType.DataSize Of
              1:
                DstType.Kind := tkBoolean;
              2:
                DstType.Kind := tkWordBool;
              4:
                DstType.Kind := tkLongBool;
            End;
          stkChar, stkWideChar:
            Case DstType.DataSize Of
              1:
                DstType.Kind := tkChar;
              2:
                DstType.Kind := tkWideChar;
            End;
        Else
          Case DstType.DataSize Of
            1:
              Case SrcList.Kind Of
                stkShortInt, stkSmallInt, stkInteger:
                  DstType.Kind := tkShortInt;
              Else
                DstType.Kind := tkByte;
              End;
            2:
              Case SrcList.Kind Of
                stkShortInt, stkSmallInt, stkInteger:
                  DstType.Kind := tkSmallInt;
              Else
                DstType.Kind := tkWord;
              End;
            4:
              Case SrcList.Kind Of
                stkShortInt, stkSmallInt, stkInteger:
                  DstType.Kind := tkInteger;
              Else
                DstType.Kind := tkCardinal;
              End;
          End;
        End;
      End;
    stkArray:
      Begin
        DstType.Kind := tkArray;
        LoadType(UnitInfo, SrcType.IndexType, DstType.IndexType);
        LoadType(UnitInfo, SrcType.ElementType, DstType.BaseType);
        DstType.DataSize := SrcType.DataSize;
        SrcList := FImage.TD32Scanner.SymbolTypes[SrcType.IndexType];
        Case SrcList.Kind Of
          stkSubRange:
            Begin
              DstType.MinValue := SrcList.MinValue;
              DstType.MaxValue := SrcList.MaxValue;
            End
          Else
          begin
            DstType.MinValue := DstType.IndexType.MinValue;
            DstType.MaxValue := DstType.IndexType.MaxValue;
          end;
        End;
      End;
  End;
End;

Procedure TDelphiDebugInfo.LoadConst(OwnerInfo: TSegmentCodeInfo; ConstSymbol: TJclTD32ConstantSymbolInfo);
Var
  ConstInfo: TConstInfo;
  TypeInfo: TJclSymbolTypeInfo;
  ConstName: String;

  procedure LoadExtended;
  var
    ExtValue: Extended;
  begin
    ExtValue := PExtended(ConstSymbol.Value)^;
    ConstInfo.Value := IUnknown(TExtendedConstantValue.Create(ExtValue));
  end;

  procedure LoadSet;
  var
    SetValue: TBytes;
  begin
    SetLength(SetValue, 32);
    Move(ConstSymbol.Value^, SetValue[TypeInfo.MinValue], ConstSymbol.Size);
    LoadType(ConstInfo.UnitInfo, ConstSymbol.TypeIndex, ConstInfo.TypeInfo);
    ConstInfo.Value := IUnknown(TSetVariantValue.Create(ConstInfo.TypeInfo, SetValue));
  end;

  procedure LoadSubRange;
  begin
    LoadType(ConstInfo.UnitInfo, ConstSymbol.TypeIndex, ConstInfo.TypeInfo);
    ConstInfo.Value := GetValueNonRef(Nil, ConstInfo.TypeInfo, TUIntPtr(ConstSymbol.Value^), False);
  end;

Begin
  ConstInfo := Nil;
  TypeInfo := FImage.TD32Scanner.SymbolTypes[ConstSymbol.TypeIndex];
  If TypeInfo <> Nil Then
    try
      ConstInfo := TConstInfo.Create;
      ConstInfo.OwnerInfo := OwnerInfo;
      ConstInfo.NameId := ConstSymbol.NameIndex;
      ConstInfo.SymbolInfo := ConstSymbol;
      LoadType(ConstInfo.UnitInfo, ConstSymbol.TypeIndex, ConstInfo.TypeInfo);

      Case TypeInfo.Kind Of
        stkBoolean:
          ConstInfo.Value := PBoolean(ConstSymbol.Value)^;
        stkWordBool:
          ConstInfo.Value := PWordBool(ConstSymbol.Value)^;
        stkLongBool:
          ConstInfo.Value := PBool(ConstSymbol.Value)^;
        stkShortInt:
          ConstInfo.Value := PShortInt(ConstSymbol.Value)^;
        stkSmallInt:
          ConstInfo.Value := PSmallInt(ConstSymbol.Value)^;
        stkInteger:
          ConstInfo.Value := PInteger(ConstSymbol.Value)^;
        stkInt64:
          ConstInfo.Value := PInt64(ConstSymbol.Value)^;
        stkByte:
          ConstInfo.Value := PByte(ConstSymbol.Value)^;
        stkWord:
          ConstInfo.Value := PWord(ConstSymbol.Value)^;
        stkCardinal:
          ConstInfo.Value := PCardinal(ConstSymbol.Value)^;
        stkUInt64:
          ConstInfo.Value := PUInt64(ConstSymbol.Value)^;
        stkSingle:
          ConstInfo.Value := PSingle(ConstSymbol.Value)^;
        stkReal48:
          ConstInfo.Value := PReal48(ConstSymbol.Value)^;
        stkReal:
          ConstInfo.Value := PReal(ConstSymbol.Value)^;
        stkExtended:
          LoadExtended;
        stkCurrency:
          ConstInfo.Value := PCurrency(ConstSymbol.Value)^;
        stkPointer:
          ConstInfo.Value := IUnknown(TPointerConstantValue.Create(PPointer(ConstSymbol.Value)^));
        stkLString, stkWString:
          ConstInfo.Value := '#STR_CONST#';
        stkSet:
          LoadSet;
        stkSubRange:
          LoadSubRange;
      Else
        FreeAndNil(ConstInfo);
      End;
    except
      on E: Exception do
      begin
        ConstName := String(ImageNames(ConstSymbol.NameIndex));
        RaiseInternalError(Format('%s.%s', [ConstInfo.UnitInfo.Name, ConstName]));
      end;
    end;

  If ConstInfo <> Nil Then
  Begin
    If ConstInfo.OwnerInfo is TFuncInfo Then
      TFuncInfo(ConstInfo.OwnerInfo).Consts.Add(ConstInfo)
    Else
      ConstInfo.UnitInfo.Consts.Add(ConstInfo);
  End;
End;

Function TDelphiDebugInfo.RegisterIndex(const Index: Byte): Integer;
Begin
  Case Index Of
    1, 5, 9, 17:
      Result := 0;
    2, 6, 10, 18:
      Result := 1;
    3, 7, 11, 19:
      Result := 2;
    4, 8, 12, 20:
      Result := 3;
    13, 21:
      Result := 4;
    14, 22:
      Result := 5;
    15, 23:
      Result := 6;
    16, 24:
      Result := 7;
    31, 33:
      Result := 8;
  Else
    Result := -1;
  End;

  Case Index Of
    1, 2, 3, 4:
      Result := Result Or (1 Shl 4);
    5, 6, 7, 8:
      Result := Result Or (2 Shl 4);
    9, 10, 11, 12, 13, 14, 15, 16, 31:
      Result := Result Or (3 Shl 4);
  End;
End;

Function TDelphiDebugInfo.LoadVar(UnitInfo: TUnitInfo; VarSymbol: TJclTD32NamedSymbol; Func: TFuncInfo): TVarInfo;

  procedure LoadRegister(VarInfo: TVarInfo);
  Var
    I: Integer;
    RegInfo: TRegInfo;
    RegRange: PRegisterRange;
  begin
    VarInfo.VarKind := vkRegister;
    VarInfo.Offset := RegisterIndex(TJclTD32RegisterSymbolInfo(VarSymbol).Registers);
    VarInfo.RegisterRanges := TList.Create;

    For I := 0 To TJclTD32RegisterSymbolInfo(VarSymbol).RangeCount - 1 Do
    begin
      RegRange := TJclTD32RegisterSymbolInfo(VarSymbol).Range[I];

      RegInfo := TRegInfo.Create;
      RegInfo.StartOffset := Cardinal(Func.Address) + RegRange^.Start;
      RegInfo.EndOffset := RegInfo.StartOffset + RegRange^.Len;
      RegInfo.RegisterIndex := RegisterIndex(RegRange^.Registers);

      VarInfo.RegisterRanges.Add(RegInfo);
    end;
  end;

Begin
  Result := TVarInfo.Create;
  Result.SymbolInfo := VarSymbol;

  Case VarSymbol.SymbolType Of
    SYMBOL_TYPE_REGISTER:
      Begin
        LoadRegister(Result);
      End;
    SYMBOL_TYPE_BPREL32:
      Begin
        Result.VarKind := vkStack;
        Result.Offset := TJclTD32BPRel32SymbolInfo(VarSymbol).Offset;
      End;
    SYMBOL_TYPE_LDATA32, SYMBOL_TYPE_GDATA32:
      Begin
        Result.VarKind := vkGlobal;
        Result.Offset := TJclTD32DataSymbolInfo(VarSymbol).Offset + FImage.ImageSectionHeaders[TJclTD32DataSymbolInfo(VarSymbol).Segment - 1]
          .VirtualAddress + ImageBase;
      End;
    SYMBOL_TYPE_SLINK32:
      Begin
        Result.VarKind := vkLink;
        Result.Offset := TJclTD32LinkSymbolInfo(VarSymbol).Offset;
      End;
  End;

  If VarSymbol.SymbolType = SYMBOL_TYPE_SLINK32 Then
  begin
    Result.NameId := -1;
  end
  Else
  Begin
    Result.NameId := VarSymbol.NameIndex;
    LoadType(UnitInfo, VarSymbol.TypeIndex, Result.DataType);
  End;

  If Func <> Nil Then
  begin
    Result.Owner := Func;
    Func.Vars.Add(Result)
  end
  Else
  begin
    Result.Owner := UnitInfo;
    UnitInfo.Vars.Add(Result);
  end;
End;

Procedure TDelphiDebugInfo.LoadSymbols(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
Var
  I: Integer;
  SymbolInfo: TJclTD32SymbolInfo;
Begin
  UnitInfo.Types.Capacity := 128;
  UnitInfo.Funcs.Capacity := 128;
  UnitInfo.FuncsByAddr.Capacity := 128;
  UnitInfo.Vars.Capacity := 32;
  UnitInfo.Consts.Capacity := 32;

  For I := 0 To Module.SymbolCount - 1 Do
  begin
    SymbolInfo := Module.Symbols[I];
    Case SymbolInfo.SymbolType Of
      SYMBOL_TYPE_PCONSTANT:
        LoadConst(UnitInfo, TJclTD32ConstantSymbolInfo(SymbolInfo));
      SYMBOL_TYPE_BPREL32, SYMBOL_TYPE_LDATA32, SYMBOL_TYPE_GDATA32, SYMBOL_TYPE_SLINK32:
        LoadVar(UnitInfo, TJclTD32NamedSymbol(SymbolInfo), Nil);
      SYMBOL_TYPE_LPROC32, SYMBOL_TYPE_GPROC32:
        LoadFunc(UnitInfo, TJclTD32ProcSymbolInfo(SymbolInfo));
    End;
  end;
End;

Procedure TDelphiDebugInfo.LoadFunc(UnitInfo: TUnitInfo; FuncSymbol: TJclTD32ProcSymbolInfo);
Var
  I: Integer;
  ProcInfo: TJclSymbolTypeInfo;
  FuncInfo: TFuncInfo;
  SymbolInfo: TJclTD32SymbolInfo;
  VarInfo: TVarInfo;
Begin
  FuncInfo := TFuncInfo.Create;
  FuncInfo.NameId := FuncSymbol.NameIndex;
  FuncInfo.SymbolInfo := FuncSymbol;
  FuncInfo.Address := Pointer(FuncSymbol.Offset + FImage.ImageSectionHeaders[FuncSymbol.Segment - 1].VirtualAddress + ImageBase);
  FuncInfo.CodeSize := FuncSymbol.Size;
  FuncInfo.UnitInfo := UnitInfo;
  FuncInfo.ID := FuncSymbol;
  FuncInfo.ParentID := FuncSymbol.Parent;

  ProcInfo := FImage.TD32Scanner.SymbolTypes[FuncSymbol.TypeIndex];
  LoadType(UnitInfo, ProcInfo.IndexType, FuncInfo.ResultType);

  FuncInfo.Vars.Capacity := 8;

  For I := 0 To FuncSymbol.SymbolCount - 1 Do
  begin
    SymbolInfo := FuncSymbol.Symbols[I];
    Case SymbolInfo.SymbolType Of
      SYMBOL_TYPE_PCONSTANT:
        LoadConst(FuncInfo, TJclTD32ConstantSymbolInfo(SymbolInfo));
      SYMBOL_TYPE_SLINK32:
        LoadVar(UnitInfo, TJclTD32NamedSymbol(SymbolInfo), FuncInfo);
      SYMBOL_TYPE_REGISTER, SYMBOL_TYPE_BPREL32, SYMBOL_TYPE_LDATA32, SYMBOL_TYPE_GDATA32:
        Begin
          VarInfo := LoadVar(UnitInfo, TJclTD32NamedSymbol(SymbolInfo), FuncInfo);
          FuncInfo.Params.Add(VarInfo);
        End;
    End;
  end;

  UnitInfo.Funcs.Add(FuncInfo);
  UnitInfo.FuncsByAddr.Add(FuncInfo);
End;

Procedure TDelphiDebugInfo.ResolveUnits;
Var
  I, J, U: Integer;
  Member: TStructMember;
  UInfo: TUnitInfo;
  TInfo: TTypeInfo;
  FuncJ, FuncU: TFuncInfo;
Begin
  if Units.Count = 0 then
    Exit;

  Units.Sorted := True;

  For I := 0 To Units.Count - 1 Do
  Begin
    UInfo := TUnitInfo(Units.Objects[I]);

    // DoProgress(Format('Check unit "%s"', [UInfo.Name]), 90 + Round((I + 1) * Delta));

    For J := 0 To UInfo.UsedUnits.Count - 1 Do
    Begin
      U := Units.IndexOf(UInfo.UsedUnits[J]);
      If U <> -1 Then
        UInfo.UsedUnits.Objects[J] := Units.Objects[U];
    End;

    For J := 0 To UInfo.Types.Count - 1 Do
    Begin
      TInfo := TTypeInfo(UInfo.Types[J]);
      If TInfo.Members <> Nil Then
        For U := 0 To TInfo.Members.Count - 1 Do
        Begin
          Member := TStructMember(TInfo.Members[U]);
          If (Member.MethodNameId <> 0) then
            Member.Method := UInfo.FindFuncByNameId(Member.MethodNameId);
        End;
    End;

    UInfo.FuncsByAddr.Capacity := UInfo.Funcs.Count;
    For J := 0 To UInfo.Funcs.Count - 1 Do
    begin
      FuncJ := TFuncInfo(UInfo.Funcs[J]);

      For U := 0 To UInfo.Funcs.Count - 1 Do
      begin
        FuncU := TFuncInfo(UInfo.Funcs[U]);
        If FuncJ.ID = FuncU.ParentID Then
          FuncU.Parent := FuncJ;
      end;
    end;
  End;
End;

procedure TDelphiDebugInfo.SetDelphiVersion(const Value: TDelphiVersion);
begin
  FDelphiVersion := Value;
end;

function TDelphiDebugInfo.VarValueAsString(const Value: Variant): String;
begin
  if VarType(Value) = varUnknown then
    Result := CustomVariantAsString(Value)
  else
    Result := VarToStrDef(Value, '');
end;

{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.FindUnitByAddr(const Addr: Pointer): TUnitInfo;
Begin
  Result := TUnitInfo(UnitsByAddr.FindByAddress(Addr));
End;
{ ............................................................................... }

{ ............................................................................... }
procedure TDelphiDebugInfo.FillSystemUnits;
begin
  FSystemUnits.Clear;
  FSystemUnits.Sorted := False;
  FSystemUnits.CaseSensitive := False;

  FSystemUnits.Add('System');
  FSystemUnits.Add('Classes');
  FSystemUnits.Add('Windows');
  FSystemUnits.Add('SysUtils');
  FSystemUnits.Add('Variants');
  FSystemUnits.Add('StrUtils');
  FSystemUnits.Add('WideStrUtils');
  FSystemUnits.Add('XMLDoc');
  FSystemUnits.Add('XMLIntf');
  FSystemUnits.Add('Graphics');
  FSystemUnits.Add('Forms');
  FSystemUnits.Add('Controls');
  FSystemUnits.Add('StdCtrls');
  FSystemUnits.Add('ExtCtrls');
  FSystemUnits.Add('ComCtrls');
  FSystemUnits.Add('Buttons');
  FSystemUnits.Add('ActnList');
  FSystemUnits.Add('Mask');
  FSystemUnits.Add('Dialogs');

  FSystemUnits.Add('WinApi');
  FSystemUnits.Add('Vcl');
  FSystemUnits.Add('Soap');
  FSystemUnits.Add('Xml');
  FSystemUnits.Add('Web');
  FSystemUnits.Add('Data');

  FSystemUnits.Add('acPNG');
  FSystemUnits.Add('sCommonData');
  FSystemUnits.Add('acZLibEx');
  FSystemUnits.Add('sVclUtils');
  FSystemUnits.Add('sLabel');
  FSystemUnits.Add('sSkinProvider');
  FSystemUnits.Add('sSkinManager');
  FSystemUnits.Add('sGraphUtils');
  FSystemUnits.Add('acntUtils');

  FSystemUnits.Sorted := True;
end;

Function TDelphiDebugInfo.FindFuncByAddr(const UnitInfo: TUnitInfo; const Addr: TPointer): TFuncInfo;
Begin
  Result := TFuncInfo(UnitInfo.FuncsByAddr.FindByAddress(Addr));
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.FindLineByAddr(const FuncInfo: TFuncInfo; const Addr: Pointer; const GetPrevLine: Boolean = False): TLineInfo;
Var
  LineIdx: Integer;
  //SearchLine: TLineInfo;
Begin
  (*
  Result := Nil;

  SearchLine := TLineInfo.Create;
  try
    SearchLine.Address := Addr;
    FuncInfo.Lines.BinarySearch(SearchLine, LineIdx);
  finally
    FreeAndNil(SearchLine);
  end;

  if (LineIdx >= 0) and (LineIdx < FuncInfo.Lines.Count) then
  begin
    if (LineIdx > 0) and GetPrevLine then
      Dec(LineIdx);

    Result := FuncInfo.Lines[LineIdx];
  end;
  *)

  LineIdx := FuncInfo.Lines.Count - 1;

  While (LineIdx >= 0) Do
  Begin
    Result := TLineInfo(FuncInfo.Lines[LineIdx]);
    If Cardinal(Addr) >= Cardinal(Result.Address) Then
    Begin
      If GetPrevLine And (LineIdx > 0) Then
        Result := TLineInfo(FuncInfo.Lines[LineIdx - 1]);

      Exit;
    End;
    Dec(LineIdx);
  End;

  Result := Nil;
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.DoReadDebugInfo(Const FileName: String; ALoadDebugInfo: Boolean): Boolean;
Var
  I: Integer;
  Module: TJclTD32ModuleInfo;
  Delta: Double;
Begin
  Result := FileExists(FileName);
  If Result Then
  Begin
    DoProgress('Prepare', 4);
    if Assigned(FImage) then
      FreeAndNil(FImage);

    DoProgress('Init image', 5);
    FImage := TJclPeBorTD32Image.Create(True);
    DoProgress('Load image', 5);

    FImage.FileName := FileName;
    DoProgress('Load debug info', 10);
    Result := FImage.IsTD32DebugPresent;
    If Result And ALoadDebugInfo and (FImage.TD32Scanner.ModuleCount > 0) Then
    Begin
      case FImage.TD32DebugDataType of
        ddtInImage:
          FDebugInfoType := 'Internal';
        ddtTDS:
          FDebugInfoType := 'External(TDS)';
      end;

      Delta := 70 / FImage.TD32Scanner.ModuleCount;
      For I := 0 To FImage.TD32Scanner.ModuleCount - 1 Do
      Begin
        Module := FImage.TD32Scanner.Modules[I];
        ParseUnit(Module);

        DoProgress('Load debug info', 10 + Round((I + 1) * Delta));
      End;

      DoProgress('Check debug info', 80);
      ResolveUnits;
    End;
    DoProgress('Debug info loaded', 99);
  End;
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.CheckAddr(Const Addr: TPointer): Boolean;
Begin
  Result := FindUnitByAddr(Addr) <> Nil;
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.CheckDebugException(ExceptionRecord: PExceptionRecord; Var IsTraceException: Boolean): Boolean;
Begin
  Result := Inherited CheckDebugException(ExceptionRecord, IsTraceException);

  If Not Result And IsDelphiException(ExceptionRecord) Then
  Begin
    IsTraceException := IsDelphiTraceException(ExceptionRecord);
    Result := ExceptionRecord^.ExceptionFlags = cContinuable;
  End;
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.CheckSystemFile(Const FileName: String): Boolean;
Var
  FN: String;
  SL: TStringArray;
Begin
  Result := False;
  FN := ExtractFileName(FileName);
  SplitStr(FN, '.', SL);
  if Length(SL) > 0 then
    Result := (FSystemUnits.IndexOf(SL[0]) >= 0);
End;
{ ............................................................................... }

{ ............................................................................... }
Procedure TDelphiDebugInfo.ClearDebugInfo;
Begin
  if Assigned(FImage) then
    FreeAndNil(FImage);

  FDelphiVersion := dvAuto;
  FSystemUnits.Clear;
  FAddressInfoList.Clear;

  Inherited;
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.HasDebugInfo(Const FileName: String): Boolean;
Begin
  Result := HasDelphiDebugInfo(FileName);
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.GetAddrInfo(Var Addr: Pointer; Const FileName: String; Line: Cardinal): TFindResult;
Var
  Index: Integer;
  UnitInfo: TUnitInfo;
  ExactMatch: Boolean;
Begin
  Result := slNotFound;

  Index := Units.IndexOf(LowerCase(FileName));
  If Index <> -1 Then
  Begin
    UnitInfo := TUnitInfo(Units.Objects[Index]);

    // TODO: переделать на неточный поиск (ближний по адресу)
    Index := UnitInfo.Lines.IndexOf(Pointer(Line));
    ExactMatch := (Index >= 0);
    If (Index >= 0) And (Index < UnitInfo.Lines.Count) Then
    Begin
      Addr := TLineInfo(UnitInfo.Lines[Index]).Address;
      If ExactMatch Then
        Result := slFoundExact
      Else
        Result := slFoundNotExact;
    End;
  End;
End;
{ ............................................................................... }

{ ............................................................................... }
function TDelphiDebugInfo.GetClassName(ObjectPtr: Pointer): String;
Const
  _ValidChars = ['_', 'a' .. 'z', 'A' .. 'Z', '0' .. '9'];
Var
  ObjTypePtr: Pointer;
  ClassNamePtr: Pointer;
  ClassName: ShortString;
  I: Integer;
begin
  Result := '';
  if gvDebuger.ReadData(ObjectPtr, @ObjTypePtr, SizeOf(Pointer)) then
    if gvDebuger.ReadData(IncPointer(ObjTypePtr, vmtClassName), @ClassNamePtr, SizeOf(Pointer)) then
    begin
      ClassName := gvDebuger.ReadStringP(IncPointer(ClassNamePtr, SizeOf(Byte)));
      for I := 1 to Length(ClassName) do
        if not(ClassName[I] in _ValidChars) then
          Exit;

      Result := String(ClassName);
    end;
end;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.GetExceptionAddress(ExceptionRecord: PExceptionRecord): TPointer;
Begin
  If IsDelphiException(ExceptionRecord) And (ExceptionRecord^.NumberParameters > 0) Then
    Result := TPointer(ExceptionRecord^.ExceptionInformation[0])
  Else
    Result := Inherited GetExceptionAddress(ExceptionRecord);
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.GetExceptionFrame(ExceptionRecord: PExceptionRecord): TPointer;
Begin
  If ExceptionRecord^.ExceptionCode = cDelphiException Then
    Result := TPointer(ExceptionRecord^.ExceptionInformation[5])
  Else
    Result := Inherited GetExceptionFrame(ExceptionRecord);
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.GetExceptionMessage(ExceptionRecord: PExceptionRecord; const ThreadId: TThreadId): String;
Var
  ExceptTypeAddr: TPointer;
  ExceptMsgPtr: TPointer;
  //ExceptMsg: String;
Begin
  Result := '';
  If ExceptionRecord^.ExceptionCode = cDelphiException Then
  Begin
    ExceptTypeAddr := Pointer(ExceptionRecord^.ExceptionInformation[1]);

    // ExceptMsgPtr   := ReadAddressValue(Debuger, TPointer(@Exception(ExceptTypeAddr).Message));
    // ExceptMsg      := String(ReadAnsiStringValue(Debuger, ExceptMsgPtr, False));

    if gvDebuger.ReadData(@Exception(ExceptTypeAddr).Message, @ExceptMsgPtr, SizeOf(Pointer)) then
      Result := gvDebuger.ReadStringW(ExceptMsgPtr);

    // Result := Format('Exception [%s] at $%p: %s', [GetExceptionName(ExceptionRecord), GetExceptionAddress(ExceptionRecord), ExceptMsg]);
  End
  Else
    Result := Inherited GetExceptionMessage(ExceptionRecord, ThreadId);
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.GetExceptionName(ExceptionRecord: PExceptionRecord): String;
Var
  ExceptTypeAddr: TPointer;
Begin
  If ExceptionRecord^.ExceptionCode = cDelphiException Then
  Begin
    ExceptTypeAddr := Pointer(ExceptionRecord^.ExceptionInformation[1]);
    Result := GetClassName(ExceptTypeAddr);
  End
  Else
    Result := Inherited GetExceptionName(ExceptionRecord);
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.GetLineInfo(const Addr: TPointer; Var UnitInfo: TUnitInfo; Var FuncInfo: TFuncInfo; Var LineInfo: TLineInfo;
  GetPrevLine: Boolean): TFindResult;
var
  AddressInfo: PAddressInfo;
Begin
  FuncInfo := Nil;
  Result := slNotFound;

  FAddressInfoList.Lock.BeginRead;
  try
    if FAddressInfoList.TryGetValue(Addr, AddressInfo) then
    begin
      UnitInfo := AddressInfo.UnitInfo;
      FuncInfo := AddressInfo.FuncInfo;
      LineInfo := AddressInfo.LineInfo;
      Result := AddressInfo.FindResult;
    end
    else
    begin
      UnitInfo := FindUnitByAddr(Addr);
      If UnitInfo <> Nil Then
      Begin
        FuncInfo := FindFuncByAddr(UnitInfo, Addr);
        If FuncInfo <> Nil Then
        Begin
          LineInfo := FindLineByAddr(FuncInfo, Addr, GetPrevLine);
          If LineInfo = Nil Then
            Result := slFoundWithoutLine
          Else
          Begin
            If LineInfo.Address = Addr Then
              Result := slFoundExact
            Else
              Result := slFoundNotExact;
          End;
        End;
      End;

      AddressInfo := AllocMem(SizeOf(RAddressInfo));
      AddressInfo.Addr := Addr;
      AddressInfo.UnitInfo := UnitInfo;
      AddressInfo.FuncInfo := FuncInfo;
      AddressInfo.LineInfo := LineInfo;
      AddressInfo.FindResult := Result;

      FAddressInfoList.Lock.BeginWrite;
      try
        FAddressInfoList.AddOrSetValue(Addr, AddressInfo);
      finally
        FAddressInfoList.Lock.EndWrite;
      end;
    end;
  finally
    FAddressInfoList.Lock.EndRead;
  end;
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.GetLineInformation(const Addr: TPointer; Var UnitName: String; Var FuncName: String; Var Line: LongInt;
  GetPrevLine: Boolean): TFindResult;
Var
  UnitInfo: TUnitInfo;
  FuncInfo: TFuncInfo;
  LineInfo: TLineInfo;
Begin
  UnitName := '';
  FuncName := '';
  Line := -1;

  Result := GetLineInfo(Addr, UnitInfo, FuncInfo, LineInfo, GetPrevLine);
  If Result <> slNotFound Then
  Begin
    UnitName := UnitInfo.FullUnitName;
    FuncName := String(FuncInfo.Name);
    If LineInfo <> Nil Then
      Line := LineInfo.LineNo;
  End;
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.MakeFuncDbgFullName(Const ClassName, MethodName: AnsiString): AnsiString;
Begin
  Result := '@' + ClassName + '@' + MethodName;
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.MakeFuncShortName(Const MethodName: AnsiString): AnsiString;
Var
  I: Integer;
Begin
  Result := MethodName;
  I := Pos(AnsiString('@'), Result);
  If I = 1 Then
  Begin
    Delete(Result, 1, 1);
    I := Pos(AnsiString('@'), Result);
    If I > -1 Then
      Delete(Result, 1, I)
    Else
      Insert('@', Result, 1);
  End;
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.MakeFuncNativeName(Const MethodName: AnsiString): AnsiString;
Begin
  Result := MethodName;
  If Result <> '' Then
  Begin
    If Result[1] = '@' Then
      Delete(Result, 1, 1);
    Result := AnsiString(StringReplace(String(Result), '@', '.', [rfReplaceAll]));
  End;
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.Evaluate(BriefMode: Boolean; Const Expression: String; Const TimeOut: Cardinal = INFINITE): String;
// Var
// Parser   : TExprParser;
// UnitInfo : TUnitInfo;
// FuncInfo : TFuncInfo;
// LineInfo : TLineInfo;
Begin
  // Result := '';
  // If Expression <> '' Then
  // Try
  // GetLineInfo(Debuger.GetRegisters.EIP, UnitInfo, FuncInfo, LineInfo, False);
  // Parser := TExprParser.Create(DebuggeeControl, Self, UnitInfo, FuncInfo, BriefMode, Expression);
  // Try
  // Result := Parser.CalculateAsString;
  // Finally
  // Parser.Free;
  // End;
  // Except
  // On E : EDebugException Do
  // Raise;
  // On E : Exception Do
  // Raise EEvaluateException.Create(E.Message);
  // End;
End;
{ ............................................................................... }

{ ............................................................................... }
function TDelphiDebugInfo.ImageBase: Cardinal;
begin
  Result := FImage.OptionalHeader32.ImageBase;
end;

function TDelphiDebugInfo.ImageNames(const Index: TNameId): AnsiString;
begin
  if (Index >= 0) and (FImage <> Nil) and (FImage.TD32Scanner <> Nil) and (Index < FImage.TD32Scanner.NameCount) then
    Result := FImage.TD32Scanner.Names[Index]
  else
    Result := '';
end;

procedure TDelphiDebugInfo.InitCodeTracking;
var
  FuncCount: Integer;
  I, J: Integer;
  UnitInfo: TUnitInfo;
  FuncInfo: TFuncInfo;
begin
  FuncCount := 0;
  for I := 0 to Units.Count - 1 do
    Inc(FuncCount, TUnitInfo(Units.Objects[I]).Funcs.Count);

  gvDebuger.ClearDbgTracking;
  gvDebuger.InitDbgTracking(FuncCount);

  for I := 0 to Units.Count - 1 do
  begin
    UnitInfo := TUnitInfo(Units.Objects[I]);

    if not gvDebuger.TrackSystemUnits and (UnitInfo.GetUnitType = utSystem) then
      Continue;

    for J := 0 to UnitInfo.Funcs.Count - 1 do
    begin
      FuncInfo := TFuncInfo(UnitInfo.Funcs[J]);
      gvDebuger.SetTrackBreakpoint(FuncInfo.Address, FuncInfo);
    end;
  end;
end;

Procedure TDelphiDebugInfo.InitDebugHook;
var
  USystem: TUnitInfo;

  function FindSystemPas: Boolean;
  const
    _SystemUnit: String = 'system.pas';
  var
    I: Integer;
    UInfo: TUnitInfo;
  begin
    USystem := Nil;
    Result := False;
    for I := 0 to Units.Count - 1 do
    begin
      UInfo := TUnitInfo(Units.Objects[I]);
      if SameText(_SystemUnit, UInfo.ShortName) then
      begin
        USystem := UInfo;
        Result := True;
        Exit;
      end;
    end;
  end;

  procedure SetDebugHook;
  Const
    _DebugHook: AnsiString = '@@DebugHook';
    // 1 to notify debugger of non-Delphi exceptions
    // >1 to notify debugger of exception unwinding
    Enable: Byte = 2;
  Var
    DebugHook: TVarInfo;
  begin
    DebugHook := USystem.FindVarByName(_DebugHook);
    If Assigned(DebugHook) Then
      gvDebuger.WriteData(Pointer(DebugHook.Offset), @Enable, 1);
  end;

  function GetMemoryManagerVar: Pointer;
  const
    _MemoryManagerStrD10: AnsiString = '@@MemoryManager';
    _MemoryManagerStrXE: AnsiString = '@System@MemoryManager';
    _MemoryManagerStrXE4: AnsiString = '_MemoryManager';
  Var
    _MemoryManager: TVarInfo;
  begin
    Result := Nil;

    if gvDebuger.MemoryCheckMode and Assigned(USystem) then
    begin
      // TODO: add support delphi version
      _MemoryManager := USystem.FindVarByName(_MemoryManagerStrD10);
      if _MemoryManager = nil then
        _MemoryManager := USystem.FindVarByName(_MemoryManagerStrXE);
      if _MemoryManager = nil then
        _MemoryManager := USystem.FindVarByName(_MemoryManagerStrXE4);

      If Assigned(_MemoryManager) Then
        Result := Pointer(_MemoryManager.Offset);
    end;
  end;

Begin
  gvDebuger.ProcessData.SetPEImage(FImage);

  if gvDebuger.CodeTracking then
    InitCodeTracking;

  if FindSystemPas then
  Begin
    // SetDebugHook;

  End;

  LoadDbgHookDll(
    gvDebuger.ProcessData.AttachedProcessHandle,
    'DbgHook32.dll',
    Pointer(FImage.OptionalHeader32.ImageBase),
    GetMemoryManagerVar,
    gvDebuger.MemoryCallStack
  );

End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.IsDelphiException(ExceptionRecord: PExceptionRecord): Boolean;
Begin
  Case ExceptionRecord^.ExceptionCode Of
    cDelphiUnhandled, cDelphiTerminate, cDelphiException, cDelphiReRaise, cDelphiExcept, cDelphiFinally, cNonDelphiException, cDelphiExitFinally:
      Result := True;
  Else
    Result := False;
  End;
End;
{ ............................................................................... }

{ ............................................................................... }
Function TDelphiDebugInfo.IsDelphiTraceException(ExceptionRecord: PExceptionRecord): Boolean;
Begin
  Case ExceptionRecord^.ExceptionCode Of
    // cDelphiUnhandled,
    // cDelphiTerminate,
    cDelphiException, cDelphiReRaise, cDelphiExcept, cDelphiFinally,
    // cNonDelphiException,
    cDelphiExitFinally:
      Result := True;
  Else
    Result := False;
  End;
End;
{ ............................................................................... }

initialization

// _HookThreads;

finalization

// _UnhookThreads;

End.
