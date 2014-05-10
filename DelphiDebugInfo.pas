unit DelphiDebugInfo;

Interface

Uses
  SysUtils, Windows, Classes, DebugInfo, Debuger, DebugerTypes, JclTD32Ex;

Type
  TDelphiVersion = (dvAuto = 0, dvD1 = 8, dvD2 = 9, dvD3 = 10, dvD4 = 12, dvD5 = 13, dvD6 = 14, dvD7 = 15, dvD8 = 16, dvD2005 = 17, dvD2006_7 = 18,
    dvD2009 = 20, dvD2010 = 21, dvDXE = 22, dvDXE2 = 23, dvDXE3 = 24, dvDXE4 = 25);

  TDelphiDebugInfo = Class(TDebugInfo)
  Private
    FDelphiVersion: TDelphiVersion;
    FSystemUnits: TStringList;
    FAddressInfoList: TAddressInfoList;
    FIsHookSet: Boolean;

    Function ImageBase: Cardinal;
    Function ImageNames(const Index: TNameId): AnsiString;
    function LoadVar(UnitInfo: TUnitInfo; VarSymbol: TJclTD32NamedSymbol; Func: TFuncInfo): TVarInfo;
    procedure LoadFunc(UnitInfo: TUnitInfo; FuncSymbol: TJclTD32ProcSymbolInfo);
    procedure LoadSymbols(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
    function GetUnitFileName(const UnitName: String): String;
    procedure LoadConst(OwnerInfo: TSegmentCodeInfo; ConstSymbol: TJclTD32ConstantSymbolInfo);
    //procedure LoadLines(UnitInfo: TUnitInfo; Source: TJclTD32SourceModuleInfo);
    procedure LoadSourceLines(UnitInfo: TUnitInfo; UnitSourceModuleInfo: TUnitSourceModuleInfo; Source: TJclTD32SourceModuleInfo);
    procedure LoadSegments(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
    procedure LoadSourceModules(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
    function LoadType(UnitInfo: TUnitInfo; const TypeIndex: Integer; out DstType: TTypeInfo): Integer;
    procedure LoadUsedUnits(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
    function RegisterIndex(const Index: Byte): Integer;

    procedure InitSegments;

    Function ParseUnit(Module: TJclTD32ModuleInfo): TUnitInfo;

    Procedure ResolveUnits;

    Function FindUnitByAddr(const Addr: Pointer): TUnitInfo;
    Function FindFuncByAddr(const UnitInfo: TUnitInfo; const Addr: Pointer): TFuncInfo;
    Function FindLineByAddr(const FuncInfo: TFuncInfo; const Addr: Pointer; const GetPrevLine: Boolean = False): TLineInfo;

    function CustomVariantAsString(const Value: Variant): String;
    procedure SetDelphiVersion(const Value: TDelphiVersion);
    procedure InitCodeTracking(const SetBP: Boolean);
    procedure FillSystemUnits;

  Protected
    FImage: TJclPeBorTD32Image;

    function GetDBGFileName(const FileName: String): String;

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

    Function MakeFuncDbgFullName(Const ClassName, MethodName: AnsiString): AnsiString; Override;
    Function MakeFuncShortName(Const MethodName: AnsiString): AnsiString; Override;
    Function MakeFuncNativeName(Const MethodName: AnsiString): AnsiString; Override;

    Function Evaluate(BriefMode: Boolean; Const Expression: String; Const TimeOut: Cardinal = INFINITE): String; Override;
    Function EvaluateVariable(VarInfo: TVarInfo): Variant; override;

    Function VarValueAsString(const Value: Variant): String; override;

    function GetSystemUnit: TUnitInfo;
    function GetMemoryManager: TVarInfo; virtual;
    function SetDebugHook(const Value: Byte): Boolean;

    procedure SetMemoryManagerBreakpoints; Override;
    procedure ResetMemoryManagerBreakpoints; Override;

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

Function HasDelphiDebugInfo(Const AFileName: String): Boolean;

Implementation

Uses
  JclDebug, JclPeImage, JclWin32,
  Math, Variants, ClassUtils, DebugHook, System.StrUtils, System.Contnrs, Vcl.Forms;

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

{ TDelphiDebugInfo }

Constructor TDelphiDebugInfo.Create;
Begin
  Inherited Create;

  FImage := Nil;
  FDelphiVersion := dvAuto;
  FSystemUnits := TStringList.Create;
  FAddressInfoList := TAddressInfoList.Create(16 * 1024);
  FIsHookSet := False;

  FillSystemUnits;
End;

function TDelphiDebugInfo.CustomVariantAsString(const Value: Variant): String;
//var
//  CustomVariantData: ICustomVariantData;
//  ToStringData: TToStringData;
begin
  Result := '#VALUE#';

  //TODO:

//  If Supports(IUnknown(TVarData(Value).VUnknown), ICustomVariantData, CustomVariantData) Then
//  begin
//    ToStringData.DebugInfo := Self;
//    ToStringData.Mode := tsmBrief;
//    ToStringData.RecursionLevel := 0;
//
//    Result := CustomVariantData.AsString(ToStringData);
//  end
//  Else
//    Result := 'Unsupported data type';
end;

Destructor TDelphiDebugInfo.Destroy;
Begin
  ClearDebugInfo;

  FreeAndNil(FImage);
  FreeAndNil(FSystemUnits);
  FreeAndNil(FAddressInfoList);

  Inherited Destroy;
End;

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
Begin
  Result := TUnitInfo.Create;
  Result.SymbolInfo := Module;

  Result.NameId := Module.NameIndex;

  Units.AddObject(Result.ShortName, Result);
  UnitsByAddr.Add(Result);

  LoadSegments(Result, Module);
  LoadUsedUnits(Result, Module);
  LoadSymbols(Result, Module);
  LoadSourceModules(Result, Module);
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
  If (Ext <> '.pas') and (Ext <> '.inc') and (Ext <> '.dpr') Then
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

function TDelphiDebugInfo.GetSystemUnit: TUnitInfo;
const
  _SystemUnit: String = 'system.pas';
var
  I: Integer;
begin
  for I := 0 to Units.Count - 1 do
  begin
    Result := TUnitInfo(Units.Objects[I]);
    if SameText(_SystemUnit, Result.ShortName) then
      Exit;
  end;

  Result := Nil;
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
    S.Address := Pointer(SegmentInfo.Offset + FImage.ImageSectionHeaders[SegmentInfo.Segment - 1].VirtualAddress + ImageBase);

    S.Size := SegmentInfo.Size;

    //   $0000  Data segment
    //   $0001  Code segment
    case SegmentInfo.Flags of
      $0000:
        S.SegmentClassInfo := GetSegmentByType(ustData);
      $0001:
        S.SegmentClassInfo := GetSegmentByType(ustCode);
    else
      RaiseDebugCoreException();
    end;

    if Assigned(S.SegmentClassInfo) then
    begin
      if (S.SegmentClassInfo.SegType = ustCode) and
        ((UnitInfo.Address = Nil) or (Cardinal(UnitInfo.Address) > Cardinal(S.Address)))
      then
        UnitInfo.Address := Pointer(S.Address);

      case S.SegmentClassInfo.SegType of
        ustData:
          Inc(UnitInfo.Size, S.Size);
        ustCode:
          Inc(UnitInfo.Size, S.Size);
      end;
    end
    else
      RaiseDebugCoreException('');

    UnitInfo.Segments.Add(S);
  End;
End;

procedure TDelphiDebugInfo.LoadSourceLines(UnitInfo: TUnitInfo; UnitSourceModuleInfo: TUnitSourceModuleInfo; Source: TJclTD32SourceModuleInfo);
Var
  I: Integer;
  LineInfo: TJclTD32LineInfo;
  L: TLineInfo;
  F: TFuncInfo;
Begin
  UnitSourceModuleInfo.Lines.Capacity := Source.LineCount;
  For I := 0 To Source.LineCount - 1 Do
  Begin
    LineInfo := Source.Line[I];

    L := TLineInfo.Create;
    L.LineNo := LineInfo.LineNo; // - 1; ???
    L.Address := Pointer(LineInfo.Offset + FImage.ImageSectionHeaders[LineInfo.Segment - 1].VirtualAddress + ImageBase);
    L.SrcSegment := UnitSourceModuleInfo;

    UnitSourceModuleInfo.Lines.Add(L);

    UnitInfo.Lines.Add(L);

    F := FindFuncByAddr(UnitInfo, L.Address);
    If F <> Nil Then
      F.Lines.Add(L);
  End;
end;

procedure TDelphiDebugInfo.LoadSourceModules(UnitInfo: TUnitInfo; Module: TJclTD32ModuleInfo);
Var
  I: Integer;
  SourceModuleInfo: TJclTD32SourceModuleInfo;
  SM: TUnitSourceModuleInfo;
begin
  For I := 0 To Module.SourceModuleCount - 1 Do
  Begin
    SourceModuleInfo := Module.SourceModules[I];

    SM := TUnitSourceModuleInfo.Create;
    SM.UnitInfo := UnitInfo;
    SM.NameId := SourceModuleInfo.NameIndex;
    SM.SymbolInfo := SourceModuleInfo;

    //LoadLines(UnitInfo, SourceModuleInfo);
    LoadSourceLines(UnitInfo, SM, SourceModuleInfo);

    UnitInfo.SourceSegments.Add(SM);
  End;
end;

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

(*
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
*)

const
  _DefJclSymbolTypeKindToTypeKind: array[Low(TJclSymbolTypeKind) .. High(TJclSymbolTypeKind)] of TTypeKind = (
    tkBoolean, tkWordBool, tkLongBool, tkShortInt,
    tkSmallInt, tkInteger, tkInt64, tkByte, tkWord, tkCardinal, tkUInt64,
    tkSingle, tkReal48, tkReal, tkExtended, tkCurrency, tkComplex, tkPString,
    tkLString, tkWString, tkChar, tkPointer, tkSubRange, tkArray, tkEnum,
    tkStructure, tkClass, tkSet, tkVariant, tkProperty, tkFieldList, tkClosure,
    tkClassRef, tkWideChar, tkProcedure, tkArgList, tkMFunction, tkVoid);


Function TDelphiDebugInfo.LoadType(UnitInfo: TUnitInfo; const TypeIndex: Integer; out DstType: TTypeInfo): Integer;
Var
  SrcType: TJclSymbolTypeInfo;

  procedure _LoadPointerType;
  begin
    If SrcType.ElementType <> 0 Then
    Begin
      LoadType(UnitInfo, SrcType.ElementType, DstType.BaseType);
      If DstType.BaseType.Kind = tkClass Then
        DstType.Kind := tkObject
      Else If (DstType.BaseType.Kind = tkArray) And (DstType.BaseType.DataSize = -1) Then
        DstType.Kind := tkDynamicArray;
    End;
  end;

  procedure _LoadClassType;
  var
    I, J: Integer;
    SrcList: TJclSymbolTypeInfo;
    SrcMember: TJclTD32MemberSymbolInfo;
    SrcMemberType: TJclSymbolTypeInfo;
    DstMember: TStructMember;
    DstTypeMember: TStructMember;
  begin
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
        DstMember.Offset := SrcMember.Offset;
        DstMember.DataSize := DstMember.DataType.DataSize;
      End
      Else
      Begin
        LoadType(UnitInfo, SrcMemberType.ElementType, DstMember.DataType);

        DstMember.IsDefault := (SrcMemberType.Flags And 1) = 1;

        If (SrcMemberType.Flags And 2) = 2 Then
          DstMember.MethodNameId := SrcMemberType.MinValue
        Else
        Begin
          DstMember.Offset := SrcMemberType.MinValue;
          DstMember.DataSize := DstMember.DataType.DataSize;
        End;

        For J := 0 To DstType.Members.Count - 1 Do
        Begin
          DstTypeMember := TStructMember(DstType.Members[J]);
          If DstTypeMember.Offset = SrcMemberType.MinValue Then
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
  end;

  procedure _LoadStructureType;
  var
    I: Integer;
    SrcList: TJclSymbolTypeInfo;
    DstMember: TStructMember;
    SrcMember: TJclTD32MemberSymbolInfo;
  begin
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

      DstMember.Offset := SrcMember.Offset;
      DstMember.DataSize := DstMember.DataType.DataSize;

      DstType.Members.Add(DstMember);
    End;
  end;

  procedure _LoadEnumType;
  var
    I: Integer;
    SrcList: TJclSymbolTypeInfo;
    SrcEnum: TJclEnumerateSymbolInfo;
    EnumMember: TEnumInfo;
  begin
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
  end;

  procedure _LoadSubRangeType;
  var
    SrcList: TJclSymbolTypeInfo;
  begin
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
  end;

  procedure _LoadArrayType;
  var
    SrcList: TJclSymbolTypeInfo;
  begin
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
  end;

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
  DstType.DataSize := SizeOf(Pointer);
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
  DstType.Kind := _DefJclSymbolTypeKindToTypeKind[SrcType.Kind];
  DstType.DataSize := SrcType.DataSize;

  Case SrcType.Kind Of
    stkBoolean:
      Begin
        DstType.MinValue := 0;
        DstType.MaxValue := 1;
      End;
    stkWordBool:
      Begin
        DstType.MinValue := 0;
        DstType.MaxValue := 1;
      End;
    stkLongBool:
      Begin
        DstType.MinValue := 0;
        DstType.MaxValue := 1;
      End;
    stkShortInt:
      Begin
        DstType.MinValue := Low(ShortInt);
        DstType.MaxValue := High(ShortInt);
      End;
    stkSmallInt:
      Begin
        DstType.MinValue := Low(SmallInt);
        DstType.MaxValue := High(SmallInt);
      End;
    stkInteger:
      Begin
        DstType.MinValue := Low(Integer);
        DstType.MaxValue := High(Integer);
      End;
    stkInt64: ;
    stkByte:
      Begin
        DstType.MinValue := Low(Byte);
        DstType.MaxValue := High(Byte);
      End;
    stkWord:
      Begin
        DstType.MinValue := Low(Word);
        DstType.MaxValue := High(Word);
      End;
    stkCardinal:
      Begin
        DstType.MinValue := Low(Cardinal);
        Cardinal(DstType.MaxValue) := High(Cardinal);
      End;
    stkUInt64: ;
    stkSingle: ;
    stkReal48: ;
    stkReal: ;
    stkExtended: ;
    stkCurrency: ;
    stkComplex: ;
    stkPString:
      Begin
        DstType.DataSize := SizeOf(ShortString);
        LoadType(UnitInfo, SrcType.IndexType, DstType.BaseType);
      End;
    stkLString:
      Begin
        DstType.DataSize := SizeOf(AnsiString);
        LoadType(UnitInfo, SrcType.ElementType, DstType.BaseType);
      End;
    stkWString:
      Begin
        DstType.DataSize := SizeOf(WideString);
        LoadType(UnitInfo, SrcType.ElementType, DstType.BaseType);
      End;
    stkChar:
      Begin
        DstType.MinValue := 0;
        DstType.MaxValue := Ord(High(AnsiChar));
      End;
    stkPointer:
      Begin
        _LoadPointerType;
      End;
    stkSubRange:
      Begin
        _LoadSubRangeType;
      End;
    stkArray:
      Begin
        _LoadArrayType;
      End;
    stkEnum:
      Begin
        _LoadEnumType;
      End;
    stkStructure:
      Begin
        _LoadStructureType;
      End;
    stkClass:
      Begin
        _LoadClassType;
      End;
    stkSet:
      Begin
        LoadType(UnitInfo, SrcType.ElementType, DstType.BaseType);
        DstType.DataSize := SrcType.DataSize;
      End;
    stkVariant: ; // ???
    stkProperty:
      Begin
        // TODO:
      End;
    stkFieldList: ; // ???
    stkClosure:
      Begin
        DstType.Kind := tkPointer;
      End;
    stkClassRef:
      Begin
        // TODO:
      End;
    stkWideChar:
      Begin
        DstType.MinValue := Low(Word);
        DstType.MaxValue := High(Word);
      End;
    stkProcedure:
      Begin
        // TODO: Params
      End;
    stkArgList: ;
    stkMFunction: ;
    stkVoid: ;
    else
      Begin
        SrcType.Kind := SrcType.Kind;
        RaiseDebugCoreException();
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
    //TODO: ConstInfo.Value := IUnknown(TExtendedConstantValue.Create(ExtValue));
  end;

  procedure LoadSet;
  var
    SetValue: TBytes;
  begin
    SetLength(SetValue, 32);
    Move(ConstSymbol.Value^, SetValue[TypeInfo.MinValue], ConstSymbol.Size);
    LoadType(ConstInfo.UnitInfo, ConstSymbol.TypeIndex, ConstInfo.TypeInfo);
    //TODO: ConstInfo.Value := IUnknown(TSetVariantValue.Create(ConstInfo.TypeInfo, SetValue));
  end;

  procedure LoadSubRange;
  begin
    LoadType(ConstInfo.UnitInfo, ConstSymbol.TypeIndex, ConstInfo.TypeInfo);
    //TODO: ConstInfo.Value := GetValueNonRef(Nil, ConstInfo.TypeInfo, TUIntPtr(ConstSymbol.Value^), False);
  end;

Begin
  ConstInfo := Nil;
  TypeInfo := FImage.TD32Scanner.SymbolTypes[ConstSymbol.TypeIndex];
  If TypeInfo <> Nil Then
    try
      ConstInfo := TConstInfo.Create;
      ConstInfo.Owner := OwnerInfo;
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
          //TODO: ConstInfo.Value := IUnknown(TPointerConstantValue.Create(PPointer(ConstSymbol.Value)^));
          ConstInfo.Value := '#PTR_CONST#';
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
        RaiseDebugCoreException(Format('%s.%s', [ConstInfo.UnitInfo.Name, ConstName]));
      end;
    end;

  If ConstInfo <> Nil Then
  Begin
    If ConstInfo.Owner is TFuncInfo Then
      TFuncInfo(ConstInfo.Owner).Consts.Add(ConstInfo)
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
  FuncInfo.Size := FuncSymbol.Size;
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

procedure TDelphiDebugInfo.ResetMemoryManagerBreakpoints;
begin
  // gvDebuger.MemoryBPCheckMode := False;

  if MemoryManagerInfo.VarInfo = nil then Exit;

  if MemoryManagerInfo.GetMem <> nil then
    gvDebuger.RemoveTrackBreakpoint(MemoryManagerInfo.GetMem.Address, tbMemInfo);
  if MemoryManagerInfo.FreeMem <> nil then
    gvDebuger.RemoveTrackBreakpoint(MemoryManagerInfo.FreeMem.Address, tbMemInfo);
  if MemoryManagerInfo.ReallocMem <> nil then
    gvDebuger.RemoveTrackBreakpoint(MemoryManagerInfo.ReallocMem.Address, tbMemInfo);
  if MemoryManagerInfo.AllocMem <> nil then
    gvDebuger.RemoveTrackBreakpoint(MemoryManagerInfo.AllocMem.Address, tbMemInfo);

  gvDebuger.Log('Reset slow memory manager hook - ok');
end;

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

function TDelphiDebugInfo.SetDebugHook(const Value: Byte): Boolean;
Const
  _DebugHook: AnsiString = 'DebugHook';

  // Value:
  // 1 to notify debugger of non-Delphi exceptions
  // >1 to notify debugger of exception unwinding
Var
  USystem: TUnitInfo;
  DebugHook: TVarInfo;
begin
  Result := False;

  USystem := GetSystemUnit;
  if Assigned(USystem) then
  begin
    DebugHook := USystem.FindVarByName(_DebugHook, True);
    If Assigned(DebugHook) Then
      gvDebuger.WriteData(Pointer(DebugHook.Offset), @Value, SizeOf(Byte));
  end;
end;

procedure TDelphiDebugInfo.SetDelphiVersion(const Value: TDelphiVersion);
begin
  FDelphiVersion := Value;
end;


procedure TDelphiDebugInfo.SetMemoryManagerBreakpoints;
var
  Members: TNameList;
  Member: TStructMember;
  Addr: Pointer;

  function _GetFuncPtr: Pointer;
  var
    Offset: Pointer;
  begin
    if Member = nil then
      RaiseDebugCoreException();

    Result := nil;
    Offset := Pointer(MemoryManagerInfo.VarInfo.Offset + Member.Offset);
    if not gvDebuger.ReadData(Offset, @Result, SizeOf(Pointer)) then
      RaiseDebugCoreException();
  end;

  function _SetTrackBreakpoint(Addr: Pointer): TFuncInfo;
  var
    UnitInfo: TUnitInfo;
    LineInfo: TLineInfo;
  begin
    Result := Nil;

    if GetLineInfo(Addr, UnitInfo, Result, LineInfo, False) <> slNotFound then
      gvDebuger.SetTrackBreakpoint(Addr, Result, tbMemInfo)
    else
      RaiseDebugCoreException();
  end;

begin
  if MemoryManagerInfo.VarInfo = nil then Exit;

  if MemoryManagerInfo.VarInfo.DataType = nil then Exit;

  Members := MemoryManagerInfo.VarInfo.DataType.Members;
  if Assigned(Members) and (Members.Count > 0) then
  begin
    gvDebuger.MemoryBPCheckMode := True;

    Member := TStructMember(Members.FindByName('GetMem'));
    Addr := _GetFuncPtr;
    MemoryManagerInfo.GetMem := _SetTrackBreakpoint(Addr);

    Member := TStructMember(Members.FindByName('FreeMem'));
    Addr := _GetFuncPtr;
    MemoryManagerInfo.FreeMem := _SetTrackBreakpoint(Addr);

    Member := TStructMember(Members.FindByName('ReallocMem'));
    Addr := _GetFuncPtr;
    MemoryManagerInfo.ReallocMem := _SetTrackBreakpoint(Addr);

    Member := TStructMember(Members.FindByName('AllocMem'));
    Addr := _GetFuncPtr;
    MemoryManagerInfo.AllocMem := _SetTrackBreakpoint(Addr);

    gvDebuger.Log('Set slow memory manager hook - ok');
  end;
end;

function TDelphiDebugInfo.VarValueAsString(const Value: Variant): String;
begin
  if VarType(Value) = varUnknown then
    Result := CustomVariantAsString(Value)
  else
    Result := VarToStrDef(Value, '');
end;

Function TDelphiDebugInfo.FindUnitByAddr(const Addr: Pointer): TUnitInfo;
var
  I, J: Integer;
  Segment: TUnitSegmentInfo;
Begin
  // TODO:
  //Result := TUnitInfo(UnitsByAddr.FindByAddress(Addr));

  for I := 0 to Units.Count - 1 do
  begin
    Result := TUnitInfo(Units.Objects[I]);
    for J := 0 to Result.Segments.Count - 1 do
    begin
      Segment := Result.Segments[J];

      if Assigned(Segment.SegmentClassInfo) and (Segment.SegmentClassInfo.SegType = ustCode) and
        (Cardinal(Addr) >= Cardinal(Segment.Address)) and (Cardinal(Addr) <= (Cardinal(Segment.Address) + Segment.Size))
      then
        Exit;
    end;
  end;

  Result := Nil;
End;

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

  (*
  FSystemUnits.Add('acPNG');
  FSystemUnits.Add('sCommonData');
  FSystemUnits.Add('acZLibEx');
  FSystemUnits.Add('sVclUtils');
  FSystemUnits.Add('sLabel');
  FSystemUnits.Add('sSkinProvider');
  FSystemUnits.Add('sSkinManager');
  FSystemUnits.Add('sGraphUtils');
  FSystemUnits.Add('acntUtils');
  *)

  FSystemUnits.Sorted := True;
end;

Function TDelphiDebugInfo.FindFuncByAddr(const UnitInfo: TUnitInfo; const Addr: Pointer): TFuncInfo;
Var
  I: Integer;
Begin
  // TODO:
  //Result := TFuncInfo(UnitInfo.FuncsByAddr.FindByAddress(Addr));

  For I := 0 To UnitInfo.Funcs.Count - 1 Do
  Begin
    Result := TFuncInfo(UnitInfo.Funcs[I]);

    if (Cardinal(Result.Address) <= Cardinal(Addr)) and (Cardinal(Addr) < Cardinal(Result.Address) + Result.Size) then
      Exit;
  End;
  Result := Nil;
End;

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

    FImage.FileName := GetDBGFileName(FileName);

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

      InitSegments;

      Delta := 70 / FImage.TD32Scanner.ModuleCount;
      For I := 0 To FImage.TD32Scanner.ModuleCount - 1 Do
      Begin
        Module := FImage.TD32Scanner.Modules[I];
        ParseUnit(Module);

        DoProgress('Load debug info', 10 + Round((I + 1) * Delta));
      End;

      DoProgress('Check debug info', 80);
      ResolveUnits;

      // Выгружать нельзя, так как используется для DebugInfo
      //UnMapAndLoad(FImage.LoadedImage);
    End;
    DoProgress('Debug info loaded', 99);
  End;
End;

Function TDelphiDebugInfo.CheckAddr(Const Addr: Pointer): Boolean;
Begin
  Result := FindUnitByAddr(Addr) <> Nil;
End;

Function TDelphiDebugInfo.CheckDebugException(ExceptionRecord: PExceptionRecord; Var IsTraceException: Boolean): Boolean;
Begin
  Result := Inherited CheckDebugException(ExceptionRecord, IsTraceException);

  If Not Result And IsDelphiException(ExceptionRecord) Then
  Begin
    IsTraceException := IsDelphiTraceException(ExceptionRecord);
    Result := ExceptionRecord^.ExceptionFlags = cContinuable;
  End;
End;

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

Procedure TDelphiDebugInfo.ClearDebugInfo;
var
  DbgFileName: String;
Begin
  FIsHookSet := False;

  if Assigned(FImage) then
  begin
    DbgFileName := '';

    if DebugInfoLoaded then
      DbgFileName := FImage.FileName;

    FreeAndNil(FImage);

    if DebugInfoLoaded and (DbgFileName <> '') then
      DeleteFile(PWideChar(DbgFileName));
  end;

  FDelphiVersion := dvAuto;

  if Assigned(FSystemUnits) then
    FSystemUnits.Clear;

  if Assigned(FAddressInfoList) then
    FAddressInfoList.Clear;

  Inherited ClearDebugInfo;
End;

Function TDelphiDebugInfo.HasDebugInfo(Const FileName: String): Boolean;
Begin
  Result := HasDelphiDebugInfo(FileName);
End;

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

function TDelphiDebugInfo.GetDBGFileName(const FileName: String): String;
var
  I: Integer;
begin
  // Создаем копию исполняемого файла для загрузки дебажной информации
  // TODO: Мониторилка за изменением исполняемого файла и обновление дебажной инфы
  I := 0;
  repeat
    Result := ChangeFileExt(FileName, '.~dbg');
    if FileExists(Result) then
    begin
      if DeleteFile(PWideChar(Result)) then
        Break
      else
        Result := ChangeFileExt(FileName, '.~dbg' + IntToStr(I));

      Inc(I);

      if I >= 10 then
      begin
        Result := FileName;
        Break;
      end;
    end
    else
      Break;
  until False;

  if Result <> FileName then
    if not CopyFile(PWideChar(FileName), PWideChar(Result), True) then
      Result := FileName;
end;

Function TDelphiDebugInfo.GetExceptionAddress(ExceptionRecord: PExceptionRecord): Pointer;
Begin
  If IsDelphiException(ExceptionRecord) And (ExceptionRecord^.NumberParameters > 0) Then
    Result := Pointer(ExceptionRecord^.ExceptionInformation[0])
  Else
    Result := Inherited GetExceptionAddress(ExceptionRecord);
End;

Function TDelphiDebugInfo.GetExceptionFrame(ExceptionRecord: PExceptionRecord): Pointer;
Begin
  If ExceptionRecord^.ExceptionCode = cDelphiException Then
    Result := Pointer(ExceptionRecord^.ExceptionInformation[5])
  Else
    Result := Inherited GetExceptionFrame(ExceptionRecord);
End;

Function TDelphiDebugInfo.GetExceptionMessage(ExceptionRecord: PExceptionRecord; const ThreadId: TThreadId): String;
Var
  ExceptTypeAddr: Pointer;
  ExceptMsgPtr: Pointer;
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

Function TDelphiDebugInfo.GetExceptionName(ExceptionRecord: PExceptionRecord): String;
Var
  ExceptTypeAddr: Pointer;
Begin
  If ExceptionRecord^.ExceptionCode = cDelphiException Then
  Begin
    ExceptTypeAddr := Pointer(ExceptionRecord^.ExceptionInformation[1]);
    Result := GetClassName(ExceptTypeAddr);
  End
  Else
    Result := Inherited GetExceptionName(ExceptionRecord);
End;

Function TDelphiDebugInfo.GetLineInfo(const Addr: Pointer; Var UnitInfo: TUnitInfo; Var FuncInfo: TFuncInfo; Var LineInfo: TLineInfo;
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

function TDelphiDebugInfo.GetMemoryManager: TVarInfo;
const
  _TMemoryManager: AnsiString = 'TMemoryManager';
Var
  USystem: TUnitInfo;
  MMType: TTypeInfo;
  J: Integer;
begin
  Result := Nil;

  USystem := GetSystemUnit;
  if Assigned(USystem) then
  begin
    MMType := USystem.FindTypeByName(_TMemoryManager, True);
    if Assigned(MMType) then
    begin
      for J := 0 to USystem.Vars.Count - 1 do
      begin
        Result := TVarInfo(USystem.Vars[J]);
        if Result.DataType = MMType then
          Exit;
      end;
      Result := nil;
    end;
  end;
end;

Function TDelphiDebugInfo.MakeFuncDbgFullName(Const ClassName, MethodName: AnsiString): AnsiString;
Begin
  Result := '@' + ClassName + '@' + MethodName;
End;

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

function TDelphiDebugInfo.EvaluateVariable(VarInfo: TVarInfo): Variant;
var
  EBP: Pointer;
  Value: Variant;
begin
  EBP := Pointer(gvDebuger.GetRegisters(gvDebuger.CurThreadId).Ebp);
  //TODO: Value := EvaluateProcs.EvaluateVariable(gvDebuger, VarInfo, EBP, True);

  //TODO: Result := EvaluateProcs.CalculateValue(Value, CalculateData);
  Result := Unassigned;
end;

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

procedure TDelphiDebugInfo.InitCodeTracking(const SetBP: Boolean);
var
  FuncCount: Integer;
  I, J: Integer;
  UnitInfo: TUnitInfo;
  FuncInfo: TFuncInfo;
  Segment: TUnitSegmentInfo;
  OldProtect: Cardinal;
begin
  FuncCount := 128;

  if SetBP then
    for I := 0 to Units.Count - 1 do
      Inc(FuncCount, TUnitInfo(Units.Objects[I]).Funcs.Count);

  gvDebuger.ClearDbgTracking;
  gvDebuger.InitDbgTracking(FuncCount);

  if SetBP then
  begin
    for I := 0 to Units.Count - 1 do
    begin
      UnitInfo := TUnitInfo(Units.Objects[I]);

      for J := 0 to UnitInfo.Segments.Count - 1 do
      begin
        Segment := UnitInfo.Segments[J];
        if Assigned(Segment.SegmentClassInfo) and (Segment.SegmentClassInfo.SegType = ustCode) then
            Assert(
              VirtualProtectEx(
                gvDebuger.ProcessData.AttachedProcessHandle, Pointer(Segment.Address), Segment.Size, PAGE_EXECUTE_READWRITE, OldProtect
              )
            );
      end;

      if not gvDebuger.TrackSystemUnits and (UnitInfo.UnitType = utSystem) then
        Continue;

      for J := 0 to UnitInfo.Funcs.Count - 1 do
      begin
        FuncInfo := TFuncInfo(UnitInfo.Funcs[J]);
        gvDebuger.SetTrackBreakpoint(FuncInfo.Address, FuncInfo);
      end;
    end;
  end;
end;

Procedure TDelphiDebugInfo.InitDebugHook;
Begin
  if not FIsHookSet then
  begin
    FIsHookSet := True;

    gvDebuger.ProcessData.SetPEImage(FImage);

    InitCodeTracking(gvDebuger.CodeTracking and not gvDebuger.SamplingMethod);

    MemoryManagerInfo.VarInfo := GetMemoryManager;

    // Установка перехвата вызовов GetMem и FreeMem
    // SetMemoryManagerBreakpoints;

    // Инициализация дебажного потока в процессе
    // !!! Поток запустится не сразу, а через некоторое время
    LoadDbgHookDll(
      gvDebuger.ProcessData.AttachedProcessHandle,
      Format('%s\DbgHook32.dll', [ExtractFileDir(Application.ExeName)]),
      Pointer(FImage.OptionalHeader32.ImageBase),
      MemoryManagerInfo.VarInfo,
      gvDebuger.MemoryCallStack,
      gvDebuger.SyncObjsTracking
    );
  end;
End;

procedure TDelphiDebugInfo.InitSegments;
var
  Idx: Integer;
  Segment: TSegmentClassInfo;
  ImageSectionHeader: TImageSectionHeader;
begin
  Segments.Clear;

  for Idx := 0 to FImage.ImageSectionCount - 1 do
  begin
    Segment := TSegmentClassInfo.Create;

    Segment.SegType := TSegmentClassInfo.StrToSegmentType(FImage.ImageSectionNames[Idx]);

    ImageSectionHeader := FImage.ImageSectionHeaders[Idx];
    Segment.Address := Pointer(ImageSectionHeader.VirtualAddress + ImageBase);
    Segment.Size := ImageSectionHeader.SizeOfRawData;
    Segment.ID := Idx + 1;

    Segments.AddObject(FImage.ImageSectionNames[Idx], Segment);
  end;

  (*
  Segment := TSegmentClassInfo.Create;
  Segment.ID := $0000;
  Segment.SegType := ustData;

  Segments.AddObject('DATA', Segment);

  Segment := TSegmentClassInfo.Create;
  Segment.ID := $0001;
  Segment.SegType := ustCode;

  Segments.AddObject('CODE', Segment);
  *)
end;

Function TDelphiDebugInfo.IsDelphiException(ExceptionRecord: PExceptionRecord): Boolean;
Begin
  Case ExceptionRecord^.ExceptionCode Of
    cDelphiUnhandled, cDelphiTerminate, cDelphiException, cDelphiReRaise, cDelphiExcept, cDelphiFinally, cNonDelphiException, cDelphiExitFinally:
      Result := True;
  Else
    Result := False;
  End;
End;

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

initialization

// _HookThreads;

finalization

// _UnhookThreads;

End.
