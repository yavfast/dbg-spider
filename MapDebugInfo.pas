unit MapDebugInfo;

interface

uses System.Classes, System.SysUtils, DebugInfo, JclDebug, Windows, DelphiDebugInfo;

type
  TMapScanner = class;

  TMapDebugInfo = class(TDelphiDebugInfo)
  private
    FMapScanner: TMapScanner;

    procedure LoadSegmentClasses;

    procedure LoadSegment(Segment: PJclMapSegment);
    procedure LoadSource(Source: PJclMapProcName);
    procedure LoadSegments;

    procedure LoadProc(Proc: PJclMapProcName);
    procedure LoadProcs;

    procedure CalcFuncsSize;
    procedure CalcLastFuncSize(FuncInfo: TFuncInfo);

    function LoadLine(Line: PJclMapLineNumber): TLineInfo;
    procedure LoadLines;

    function FindSegmentByAddr(const Addr: Pointer; const SegmentID: Word = 0): TUnitSegmentInfo;
    function FindFuncByAddr(const Addr: Pointer; const SegmentID: Word = 0): TFuncInfo; overload;
    Function FindFuncByAddr(const UnitInfo: TUnitInfo; const Addr: Pointer): TFuncInfo; overload;
    Function FindLineByAddr(const FuncInfo: TFuncInfo; const Addr: Pointer; const GetPrevLine: LongBool = False): TLineInfo;
  Protected
    Function DoReadDebugInfo(Const FileName: String; ALoadDebugInfo: LongBool): LongBool; Override;
  public
    Constructor Create;
    Destructor Destroy; Override;

    Function GetNameById(const Idx: TNameId): AnsiString; override;

    Function HasDebugInfo(Const FileName: String): LongBool; override;

    Function GetLineInfo(const Addr: Pointer; Var UnitInfo: TUnitInfo; Var FuncInfo: TFuncInfo; Var LineInfo: TLineInfo;
      GetPrevLine: LongBool): TFindResult; override;

    Function MakeFuncDbgFullName(Const ClassName, MethodName: AnsiString): AnsiString; override;
    Function MakeFuncShortName(Const MethodName: AnsiString): AnsiString; override;
    Function MakeFuncNativeName(Const MethodName: AnsiString): AnsiString; override;

    function GetMemoryManager: TVarInfo; override;
  end;

  TJclMapSegmentClassHelper = record helper for TJclMapSegmentClass
  public
    function Name: String;
    function SegmentType: TSegmentType;
  end;

  TJclMapSegmentHelper = record helper for TJclMapSegment
  public
    function ModuleName: String;
    function Address: Pointer;
    function Size: Cardinal;
  end;

  TJclMapProcNameHelper = record helper for TJclMapProcName
  public
    function Name: String;
    function Address: Pointer;
  end;

  PJclMapStringCache = ^TJclMapStringCache;

  TMapScanner = class(TJclAbstractMapParser)
  strict private
    FSegmentClasses: array of TJclMapSegmentClass;
    FSegments: array of TJclMapSegment;
    FSourceNames: array of TJclMapProcName;
    FProcNames: array of TJclMapProcName;
    FLineNumbers: array of TJclMapLineNumber;

    FSegmentCnt: Integer;
    FProcNamesCnt: Integer;
    FLineNumbersCnt: Integer;

    FLineNumberErrors: Integer; // ???

    FNewUnitFileName: PJclMapString;

    FTLSSegmentID: Word;

    function GetSegment(const Index: Integer): PJclMapSegment; inline;
    function GetProc(const Index: Integer): PJclMapProcName; inline;
    function GetSegmentClassByID(const ID: Word): PJclMapSegmentClass;
    function GetSegmentClass(const Index: Integer): PJclMapSegmentClass; inline;
    function GetLineNumber(const Index: Integer): PJclMapLineNumber; inline;
    function GetSource(const Index: Integer): PJclMapProcName; inline;
  protected
    //function MAPAddrToVA(const Addr: DWORD): DWORD;

    procedure ClassTableItem(const Address: TJclMapAddress; Len: Integer; SectionName, GroupName: PJclMapString); override;
    procedure SegmentItem(const Address: TJclMapAddress; Len: Integer; GroupName, UnitName: PJclMapString); override;
    procedure PublicsByNameItem(const Address: TJclMapAddress; Name: PJclMapString); override;
    procedure PublicsByValueItem(const Address: TJclMapAddress; Name: PJclMapString); override;
    procedure LineNumbersItem(LineNumber: Integer; const Address: TJclMapAddress); override;
    procedure LineNumberUnitItem(UnitName, UnitFileName: PJclMapString); override;

    procedure Scan;
  public
    constructor Create(const MapFileName: TFileName; Module: HMODULE); override;

    class function MapStringCacheToFileName(var MapString: TJclMapStringCache): string;
    class function MapStringCacheToModuleName(var MapString: TJclMapStringCache): string;
    class function MapStringCacheToStr(var MapString: TJclMapStringCache; IgnoreSpaces: LongBool = False): string;

    function SegmentClassesCount: Integer; inline;
    function SegmentsCount: Integer; inline;
    function SourcesCount: Integer; inline;
    function ProcsCount: Integer; inline;
    function LineNumbersCount: Integer; inline;

    property SegmentClassesByID[const ID: Word]: PJclMapSegmentClass read GetSegmentClassByID;
    property SegmentClasses[const Index: Integer]: PJclMapSegmentClass read GetSegmentClass;
    property Segments[const Index: Integer]: PJclMapSegment read GetSegment;
    property Sources[const Index: Integer]: PJclMapProcName read GetSource;
    property Procs[const Index: Integer]: PJclMapProcName read GetProc;
    property LineNumbers[const Index: Integer]: PJclMapLineNumber read GetLineNumber;
  end;

implementation

uses
  System.StrUtils, JclBase, JclPeImage, System.AnsiStrings, JclSysUtils,
  JclTD32Ex;

{ TMapDebugInfo }

procedure TMapDebugInfo.CalcFuncsSize;
var
  I, J: Integer;
  UnitInfo: TUnitInfo;
  FuncInfo: TFuncInfo;
  PrevFuncInfo: TFuncInfo;
begin
  for I := 0 to Units.Count - 1 do
  begin
    UnitInfo := TUnitInfo(Units.Objects[I]);

    PrevFuncInfo := Nil;
    for J := 0 to UnitInfo.Funcs.Count - 1 do
    begin
      FuncInfo := TFuncInfo(UnitInfo.Funcs[J]);

      if Assigned(PrevFuncInfo) then
      begin
        if (PrevFuncInfo.UnitSegment = FuncInfo.UnitSegment) then
        begin
          // Функции из одного сегмента
          PrevFuncInfo.Size := Cardinal(FuncInfo.Address) - Cardinal(PrevFuncInfo.Address)
        end
        else
          CalcLastFuncSize(PrevFuncInfo);
      end;

      PrevFuncInfo := FuncInfo;
    end;

    if Assigned(PrevFuncInfo) then
      CalcLastFuncSize(PrevFuncInfo);
  end;
end;

procedure TMapDebugInfo.CalcLastFuncSize(FuncInfo: TFuncInfo);
begin
  FuncInfo.Size := FuncInfo.UnitSegment.Size - (Cardinal(FuncInfo.Address) - Cardinal(FuncInfo.UnitSegment.Address));
end;

constructor TMapDebugInfo.Create;
begin
  inherited;

  FMapScanner := Nil;
end;

destructor TMapDebugInfo.Destroy;
begin
  FreeAndNil(FMapScanner);

  inherited;
end;

function TMapDebugInfo.DoReadDebugInfo(const FileName: String; ALoadDebugInfo: LongBool): LongBool;
var
  MapFileName: String;
begin
  Result := False;

  if FileExists(FileName) then
  begin
    DoProgress('Prepare', 4);
    if Assigned(FImage) then
      FreeAndNil(FImage);

    DoProgress('Init image', 5);
    FImage := TJclPeBorTD32Image.Create(True);
    DoProgress('Load image', 5);

    FImage.FileName := GetDBGFileName(FileName);

    DoProgress('Load debug info', 10);
  end;

  try
    if ALoadDebugInfo then
    begin
      MapFileName := ChangeFileExt(FileName, '.map');

      Result := FileExists(MapFileName);
      if Result then
      begin
        FMapScanner := TMapScanner.Create(MapFileName);

        FDebugInfoType := 'External(MAP)';

        LoadSegmentClasses;

        DoProgress('Load units info', 20);
        LoadSegments;

        DoProgress('Load methods info', 40);
        LoadProcs;

        DoProgress('Load lines info', 70);
        LoadLines;

        DoProgress('Debug info loaded', 99);
      end;
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(FMapScanner);

      raise;
    end;
  end;
end;

function TMapDebugInfo.FindFuncByAddr(const Addr: Pointer; const SegmentID: Word = 0): TFuncInfo;
var
  Segment: TUnitSegmentInfo;
begin
  Result := Nil;

  Segment := FindSegmentByAddr(Addr, SegmentID);
  if Assigned(Segment) then
    Result := TFuncInfo(Segment.UnitInfo.FuncsByAddr.FindByAddress(Addr));
end;

function TMapDebugInfo.FindFuncByAddr(const UnitInfo: TUnitInfo; const Addr: Pointer): TFuncInfo;
Begin
  Result := TFuncInfo(UnitInfo.FuncsByAddr.FindByAddress(Addr));
end;

function TMapDebugInfo.FindLineByAddr(const FuncInfo: TFuncInfo; const Addr: Pointer; const GetPrevLine: LongBool): TLineInfo;
Var
  LineIdx: Integer;
Begin
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
end;

function TMapDebugInfo.FindSegmentByAddr(const Addr: Pointer; const SegmentID: Word = 0): TUnitSegmentInfo;
var
  I: Integer;
  UnitInfo: TUnitInfo;
Begin
  for I := 0 to Units.Count - 1 do
  begin
    UnitInfo := TUnitInfo(Units.Objects[I]);

    Result := UnitInfo.FindSegmentByAddr(Addr, SegmentID);
    if Assigned(Result) then
      Exit;
  end;

  Result := Nil;
end;

function TMapDebugInfo.GetLineInfo(const Addr: Pointer; var UnitInfo: TUnitInfo; var FuncInfo: TFuncInfo;
  var LineInfo: TLineInfo; GetPrevLine: LongBool): TFindResult;
var
  UnitSegmentInfo: TUnitSegmentInfo;
begin
  Result := slNotFound;

  UnitSegmentInfo := FindSegmentByAddr(Addr);
  if Assigned(UnitSegmentInfo) and Assigned(UnitSegmentInfo.SegmentClassInfo) and (UnitSegmentInfo.SegmentClassInfo.SegType = ustCode) then
  begin
    UnitInfo := UnitSegmentInfo.UnitInfo;
    FuncInfo := FindFuncByAddr(UnitInfo, Addr);
    if Assigned(FuncInfo) then
    begin
      LineInfo := FindLineByAddr(FuncInfo, Addr, GetPrevLine);
      if LineInfo = Nil then
        Result := slFoundWithoutLine
      else
      begin
        if LineInfo.Address = Addr then
          Result := slFoundExact
        else
          Result := slFoundNotExact;
      end;
    end;
  end;
end;

function TMapDebugInfo.GetMemoryManager: TVarInfo;
const
  _MemoryManager = 'System.MemoryManager';
Var
  USystem: TUnitInfo;
begin
  Result := Nil;

  USystem := GetSystemUnit;
  if Assigned(USystem) then
  begin
    Result := USystem.FindVarByName(_MemoryManager);
  end;
end;

function TMapDebugInfo.GetNameById(const Idx: TNameId): AnsiString;
var
  MapStringCache: PJclMapStringCache;
begin
  MapStringCache := PJclMapStringCache(Pointer(Idx));

  Result := AnsiString(TMapScanner.MapStringCacheToStr(MapStringCache^));
end;

function TMapDebugInfo.HasDebugInfo(const FileName: String): LongBool;
begin
  Result := inherited HasDebugInfo(FileName) or
    FileExists(ChangeFileExt(FileName, '.map'));
end;

function TMapDebugInfo.LoadLine(Line: PJclMapLineNumber): TLineInfo;
var
  FuncInfo: TFuncInfo;
  SegmentClassInfo: TSegmentClassInfo;
  UnitSourceModuleInfo: TUnitSourceModuleInfo;
begin
  Result := TLineInfo.Create;

  SegmentClassInfo := GetSegmentByID(Line^.Segment);
  Result.Address := Pointer(Cardinal(SegmentClassInfo.Address) + Line^.VA);

  Result.LineNo := Line^.LineNumber;

  FuncInfo := FindFuncByAddr(Result.Address, Line^.Segment);
  if Assigned(FuncInfo) then
  begin
    FuncInfo.Lines.Add(Result);

    UnitSourceModuleInfo := FuncInfo.UnitInfo.FindSourceSegmentByAddr(Result.Address);
    if Assigned(UnitSourceModuleInfo) then
    begin
      Result.SrcSegment := UnitSourceModuleInfo;
      UnitSourceModuleInfo.Lines.Add(Result);
    end;

    FuncInfo.UnitInfo.Lines.Add(Result);
  end
  else
    FreeAndNil(Result); // TODO:
end;

procedure TMapDebugInfo.LoadLines;
var
  Idx: Integer;
begin
  for Idx := 0 to FMapScanner.LineNumbersCount - 1 do
    LoadLine(FMapScanner.LineNumbers[Idx]);
end;

procedure TMapDebugInfo.LoadProc(Proc: PJclMapProcName);
var
  UnitSegmentInfo: TUnitSegmentInfo;
  SegmentClassInfo: TSegmentClassInfo;
  ProcAddress: Pointer;

  procedure LoadFuncInfo;
  var
    FuncInfo: TFuncInfo;
  begin
    FuncInfo := TFuncInfo.Create;

    FuncInfo.NameId := Integer(@Proc^.ProcName);
    FuncInfo.SymbolInfo := Nil;
    FuncInfo.Address := ProcAddress;
    FuncInfo.Size := 0;
    FuncInfo.UnitInfo := UnitSegmentInfo.UnitInfo;
    FuncInfo.UnitSegment := UnitSegmentInfo;
    FuncInfo.ID := Nil;
    FuncInfo.ParentID := Nil;

    UnitSegmentInfo.UnitInfo.Funcs.Add(FuncInfo);
    UnitSegmentInfo.UnitInfo.FuncsByAddr.Add(FuncInfo);
  end;

  procedure LoadVarInfo;
  var
    VarInfo: TVarInfo;
  begin
    VarInfo := TVarInfo.Create;

    VarInfo.NameId := Integer(@Proc^.ProcName);
    VarInfo.SymbolInfo := Nil;

    if SegmentClassInfo.SegType = ustTLS then
      VarInfo.VarKind := vkTLS
    else
      VarInfo.VarKind := vkGlobal;

    VarInfo.Offset := Cardinal(ProcAddress);

    UnitSegmentInfo.UnitInfo.Vars.Add(VarInfo);
  end;

  procedure LoadTypeInfo;
  begin
    // TODO:
  end;

begin
  SegmentClassInfo := GetSegmentByID(Proc^.Segment);

  ProcAddress := Pointer(Cardinal(SegmentClassInfo.Address) + Cardinal(Proc^.Address));

  UnitSegmentInfo := FindSegmentByAddr(ProcAddress, Proc^.Segment);

  if Assigned(UnitSegmentInfo) then
  begin
    (*
    UnitSourceModuleInfo := UnitSegmentInfo.UnitInfo.FindSourceSegmentByNameId(TNameId(@Proc^.ProcName));
    if UnitSourceModuleInfo = Nil then
    begin
      UnitSourceModuleInfo := TUnitSourceModuleInfo.Create;

      UnitSourceModuleInfo.NameId := TNameId(@Proc^.ProcName);
      UnitSourceModuleInfo.SymbolInfo := Nil;
      UnitSourceModuleInfo.UnitInfo := UnitSegmentInfo.UnitInfo;

      UnitSegmentInfo.UnitInfo.SourceSegments.Add(UnitSourceModuleInfo);
    end;
    *)

    case SegmentClassInfo.SegType of
      ustCode:
        LoadFuncInfo;
      ustICode:
        LoadTypeInfo;
      ustData, ustBSS, ustTLS:
        LoadVarInfo;
    end;
  end;
end;

procedure TMapDebugInfo.LoadProcs;
var
  Idx: Integer;
begin
  for Idx := 0 to FMapScanner.ProcsCount - 1 do
    LoadProc(FMapScanner.Procs[Idx]);

  CalcFuncsSize;
end;

procedure TMapDebugInfo.LoadSegments;
var
  Idx: Integer;
begin
  for Idx := 0 to FMapScanner.SegmentsCount - 1 do
    LoadSegment(FMapScanner.Segments[Idx]);

  for Idx := 0 to FMapScanner.SourcesCount - 1 do
    LoadSource(FMapScanner.Sources[Idx]);
end;

procedure TMapDebugInfo.LoadSource(Source: PJclMapProcName);
var
  UnitSegmentInfo: TUnitSegmentInfo;
  UnitSourceModuleInfo: TUnitSourceModuleInfo;
  SegmentClassInfo: TSegmentClassInfo;
  StartAddress: Pointer;
begin
  SegmentClassInfo := GetSegmentByID(Source^.Segment);

  StartAddress := Pointer(Cardinal(SegmentClassInfo.Address) + Source^.VA);

  UnitSegmentInfo := FindSegmentByAddr(StartAddress, Source^.Segment);

  if Assigned(UnitSegmentInfo) then
  begin
    UnitSourceModuleInfo := TUnitSourceModuleInfo.Create;

    UnitSourceModuleInfo.NameId := TNameId(@Source^.ProcName);
    UnitSourceModuleInfo.SymbolInfo := Nil;
    UnitSourceModuleInfo.UnitInfo := UnitSegmentInfo.UnitInfo;
    UnitSourceModuleInfo.Address := StartAddress;

    UnitSourceModuleInfo.UnitInfo.SourceSegments.Add(UnitSourceModuleInfo);
  end;
end;

function TMapDebugInfo.MakeFuncDbgFullName(const ClassName, MethodName: AnsiString): AnsiString;
begin
  Result := ClassName + '.' + MethodName;
end;

function TMapDebugInfo.MakeFuncNativeName(const MethodName: AnsiString): AnsiString;
begin
  Result := MethodName;
end;

function TMapDebugInfo.MakeFuncShortName(const MethodName: AnsiString): AnsiString;
begin
  Result := MethodName;
end;

procedure TMapDebugInfo.LoadSegment(Segment: PJclMapSegment);
var
  Idx: Integer;
  UnitInfo: TUnitInfo;
  UnitSegmentInfo: TUnitSegmentInfo;
begin
  UnitInfo := TUnitInfo.Create;
  UnitInfo.NameId := Integer(@Segment^.UnitName);

  Idx := Units.IndexOf(UnitInfo.ShortName);
  if Idx < 0 then
  begin
    UnitInfo.SymbolInfo := Nil;
    Units.AddObject(UnitInfo.ShortName, UnitInfo);
  end
  else
  begin
    FreeAndNil(UnitInfo);
    UnitInfo := TUnitInfo(Units.Objects[Idx]);
  end;

  Inc(UnitInfo.Size, Segment^.Size);

  UnitSegmentInfo := TUnitSegmentInfo.Create;
  UnitSegmentInfo.UnitInfo := UnitInfo;

  UnitSegmentInfo.SegmentClassInfo := GetSegmentByID(Segment^.Segment);
  if Assigned(UnitSegmentInfo.SegmentClassInfo) then
    UnitSegmentInfo.Address := Pointer(Cardinal(UnitSegmentInfo.SegmentClassInfo.Address) + Segment^.StartVA)
  else
    UnitSegmentInfo.Address := Pointer(Segment^.StartVA);

  UnitSegmentInfo.Size := Segment^.Size;

  if (UnitInfo.Address = Nil) and Assigned(UnitSegmentInfo.SegmentClassInfo) and
    (UnitSegmentInfo.SegmentClassInfo.SegType in [ustCode, ustICode])
  then
    UnitInfo.Address := Pointer(UnitSegmentInfo.Address);

  UnitInfo.Segments.Add(UnitSegmentInfo);
end;

procedure TMapDebugInfo.LoadSegmentClasses;
var
  Idx: Integer;
  SegmentClass: PJclMapSegmentClass;
  DbgSegmentClass: TSegmentClassInfo;
begin
  for Idx := 0 to FMapScanner.SegmentClassesCount - 1 do
  begin
    SegmentClass := FMapScanner.SegmentClasses[Idx];

    DbgSegmentClass := TSegmentClassInfo.Create;
    DbgSegmentClass.Address := Pointer(SegmentClass^.Start);
    DbgSegmentClass.Size := SegmentClass^.Len;
    DbgSegmentClass.SegType := SegmentClass^.SegmentType;
    DbgSegmentClass.ID := SegmentClass^.Segment;

    Segments.AddObject(DbgSegmentClass.SegTypeName, DbgSegmentClass);
  end;
end;

{ TMapScanner }

procedure TMapScanner.ClassTableItem(const Address: TJclMapAddress; Len: Integer; SectionName, GroupName: PJclMapString);
var
  SegmentClass: PJclMapSegmentClass;
  SectionHeader: PImageSectionHeader;
begin
  SetLength(FSegmentClasses, Length(FSegmentClasses) + 1);

  SegmentClass := @FSegmentClasses[High(FSegmentClasses)];

  SegmentClass.Segment := Address.Segment;
  SegmentClass.Start := Address.Offset;
  SegmentClass.Addr := Address.Offset; // will be fixed below while considering module mapped address

  // test GroupName because SectionName = '.tls' in Delphi and '_tls' in BCB
  (*
  if System.AnsiStrings.StrLIComp(GroupName, 'TLS', 3) = 0 then
  begin
    SegmentClass.VA := SegmentClass.Start;
    FTLSSegmentID := SegmentClass.Segment;
  end
  else
    SegmentClass.VA := MAPAddrToVA(SegmentClass.Start);
  *)
  SegmentClass.VA := SegmentClass.Start;

  SegmentClass.Len := Len;
  SegmentClass.SectionName.RawValue := SectionName;
  SegmentClass.GroupName.RawValue := GroupName;

  if FModule <> 0 then
  begin
    { Fix the section addresses }
    SectionHeader := PeMapImgFindSectionFromModule(Pointer(FModule), MapStringToStr(SectionName));
    if SectionHeader = nil then
      { before Delphi 2005 the class names where used for the section names }
      SectionHeader := PeMapImgFindSectionFromModule(Pointer(FModule), MapStringToStr(GroupName));

    if SectionHeader <> nil then
    begin
      SegmentClass.Addr := TJclAddr(FModule) + SectionHeader.VirtualAddress;
      SegmentClass.VA := SectionHeader.VirtualAddress;
    end;
  end;
end;

constructor TMapScanner.Create(const MapFileName: TFileName;  Module: HMODULE);
begin
  inherited;
  FTLSSegmentID := 0;

  Scan;
end;

function TMapScanner.GetLineNumber(const Index: Integer): PJclMapLineNumber;
begin
  Result := @FLineNumbers[Index];
end;

function TMapScanner.GetProc(const Index: Integer): PJclMapProcName;
begin
  Result := @FProcNames[Index];
end;

function TMapScanner.GetSegment(const Index: Integer): PJclMapSegment;
begin
  Result := @FSegments[Index];
end;

function TMapScanner.GetSegmentClass(const Index: Integer): PJclMapSegmentClass;
begin
  Result := @FSegmentClasses[Index];
end;

function TMapScanner.GetSegmentClassByID(const ID: Word): PJclMapSegmentClass;
var
  Idx: Integer;
begin
  for Idx := Low(FSegmentClasses) to High(FSegmentClasses) do
  begin
    Result := @FSegmentClasses[Idx];
    if Result.Segment = ID then
      Exit;
  end;

  Result := Nil;
end;

function TMapScanner.GetSource(const Index: Integer): PJclMapProcName;
begin
  Result := @FSourceNames[Index];
end;

function TMapScanner.LineNumbersCount: Integer;
begin
  Result := Length(FLineNumbers);
end;

procedure TMapScanner.LineNumbersItem(LineNumber: Integer; const Address: TJclMapAddress);
var
  SegmentClass: PJclMapSegmentClass;
  MapLineNumber: PJclMapLineNumber;
  MapSourceName: PJclMapProcName;
  VA: DWORD;
  Added: LongBool;
begin
  Added := False;

  SegmentClass := SegmentClassesByID[Address.Segment];

  if Assigned(SegmentClass) and (Address.Offset < SegmentClass.Len) then
  begin
    (*
    if SegmentClass.Segment = FTLSSegmentID then
      VA := Address.Offset
    else
      VA := MAPAddrToVA(Address.Offset + SegmentClass.Start);
    *)
    VA := Address.Offset;

    { Starting with Delphi 2005, "empty" units are listes with the last line and
      the VA 0001:00000000. When we would accept 0 VAs here, System.pas functions
      could be mapped to other units and line numbers. Discaring such items should
      have no impact on the correct information, because there can't be a function
      that starts at VA 0. }
    if VA = 0 then
      Exit;

    if FLineNumbersCnt = Length(FLineNumbers)  then
    begin
      if FLineNumbersCnt < 512 then
        SetLength(FLineNumbers, FLineNumbersCnt + 512)
      else
        SetLength(FLineNumbers, FLineNumbersCnt * 2);
    end;

    MapLineNumber := @FLineNumbers[FLineNumbersCnt];
    MapLineNumber.Segment := SegmentClass.Segment;
    MapLineNumber.VA := VA;
    MapLineNumber.LineNumber := LineNumber;

    Inc(FLineNumbersCnt);

    Added := True;

    if FNewUnitFileName <> nil then
    begin
      SetLength(FSourceNames, Length(FSourceNames) + 1);
      MapSourceName := @FSourceNames[High(FSourceNames)];

      MapSourceName.Segment := SegmentClass.Segment;
      MapSourceName.VA := VA;
      MapSourceName.ProcName.RawValue := FNewUnitFileName;

      FNewUnitFileName := nil;
    end;
  end;

  if not Added then
    Inc(FLineNumberErrors);
end;

procedure TMapScanner.LineNumberUnitItem(UnitName, UnitFileName: PJclMapString);
begin
  FNewUnitFileName := UnitFileName;
end;

(*
function TMapScanner.MAPAddrToVA(const Addr: DWORD): DWORD;
begin
  // MAP file format was changed in Delphi 2005
  // before Delphi 2005: segments started at offset 0
  //                     only one segment of code
  // after Delphi 2005: segments started at code base address (module base address + $10000)
  //                    2 segments of code
  if (Length(FSegmentClasses) > 0) and (FSegmentClasses[0].Start > 0) and (Addr >= FSegmentClasses[0].Start) then
    // Delphi 2005 and later
    // The first segment should be code starting at module base address + $10000
    Result := Addr - FSegmentClasses[0].Start
  else
    // before Delphi 2005
    Result := Addr;
end;
*)

class function TMapScanner.MapStringCacheToFileName(var MapString: TJclMapStringCache): string;
begin
  Result := MapString.CachedValue;
  if Result = '' then
  begin
    Result := MapStringToFileName(MapString.RawValue);
    MapString.CachedValue := Result;
  end;
end;

class function TMapScanner.MapStringCacheToModuleName(var MapString: TJclMapStringCache): string;
begin
  Result := MapString.CachedValue;
  if Result = '' then
  begin
    Result := MapStringToModuleName(MapString.RawValue);
    MapString.CachedValue := Result;
  end;
end;

class function TMapScanner.MapStringCacheToStr(var MapString: TJclMapStringCache; IgnoreSpaces: LongBool): string;
begin
  Result := MapString.CachedValue;
  if Result = '' then
  begin
    Result := MapStringToStr(MapString.RawValue, IgnoreSpaces);
    MapString.CachedValue := Result;
  end;
end;

function TMapScanner.ProcsCount: Integer;
begin
  Result := Length(FProcNames);
end;

procedure TMapScanner.PublicsByNameItem(const Address: TJclMapAddress; Name: PJclMapString);
begin
  { TODO : What to do? }
end;

procedure TMapScanner.PublicsByValueItem(const Address: TJclMapAddress; Name: PJclMapString);
var
  SegmentClass: PJclMapSegmentClass;
  ProcName: PJclMapProcName;
begin
  SegmentClass := SegmentClassesByID[Address.Segment];

  if Assigned(SegmentClass) and (Address.Offset < SegmentClass.Len) then
  begin
    if FProcNamesCnt = Length(FProcNames)  then
    begin
      if FProcNamesCnt < 512 then
        SetLength(FProcNames, FProcNamesCnt + 512)
      else
        SetLength(FProcNames, FProcNamesCnt * 2);
    end;

    ProcName := @FProcNames[FProcNamesCnt];

    ProcName.Segment := SegmentClass.Segment;

    (*
    if SegmentClass.Segment = FTLSSegmentID then
      ProcName.VA := Address.Offset
    else
      ProcName.VA := MAPAddrToVA(Address.Offset + SegmentClass.Start);
    *)
    ProcName.VA := Address.Offset;

    ProcName.ProcName.RawValue := Name;

    Inc(FProcNamesCnt);
  end;
end;

function Sort_MapLineNumber(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PJclMapLineNumber(Item1)^.Segment) - Integer(PJclMapLineNumber(Item2)^.Segment);

  if Result = 0 then
    Result := Integer(PJclMapLineNumber(Item1)^.VA) - Integer(PJclMapLineNumber(Item2)^.VA);
end;

function Sort_MapProcName(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PJclMapProcName(Item1)^.Segment) - Integer(PJclMapProcName(Item2)^.Segment);

  if Result = 0 then
    Result := Integer(PJclMapProcName(Item1)^.VA) - Integer(PJclMapProcName(Item2)^.VA);
end;

function Sort_MapSegment(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(PJclMapSegment(Item1)^.Segment) - Integer(PJclMapSegment(Item2)^.Segment);

  if Result = 0 then
    Result := Integer(PJclMapSegment(Item1)^.StartVA) - Integer(PJclMapSegment(Item2)^.StartVA);
end;

procedure TMapScanner.Scan;
begin
  FLineNumberErrors := 0;
  FSegmentCnt := 0;
  FProcNamesCnt := 0;

  Parse;

  SetLength(FLineNumbers, FLineNumbersCnt);
  SetLength(FProcNames, FProcNamesCnt);
  SetLength(FSegments, FSegmentCnt);

  SortDynArray(FLineNumbers, SizeOf(FLineNumbers[0]), Sort_MapLineNumber);
  SortDynArray(FProcNames, SizeOf(FProcNames[0]), Sort_MapProcName);
  SortDynArray(FSegments, SizeOf(FSegments[0]), Sort_MapSegment);
  SortDynArray(FSourceNames, SizeOf(FSourceNames[0]), Sort_MapProcName);
end;

function TMapScanner.SegmentClassesCount: Integer;
begin
  Result := Length(FSegmentClasses);
end;

procedure TMapScanner.SegmentItem(const Address: TJclMapAddress; Len: Integer; GroupName, UnitName: PJclMapString);
var
  SegmentClass: PJclMapSegmentClass;
  Segment: PJclMapSegment;
  VA: DWORD;
begin
  SegmentClass := SegmentClassesByID[Address.Segment];

  if Assigned(SegmentClass) and (Address.Offset < SegmentClass.Len) then
  begin
    (*
    if SegmentClass.Segment = FTLSSegmentID then
      VA := Address.Offset
    else
      VA := MAPAddrToVA(Address.Offset + SegmentClass.Start);
    *)
    VA := Address.Offset;

    if FSegmentCnt mod 16 = 0 then
      SetLength(FSegments, FSegmentCnt + 16);

    Segment := @FSegments[FSegmentCnt];
    Segment.Segment := SegmentClass.Segment;
    Segment.StartVA := VA;
    Segment.EndVA := VA + DWORD(Len);
    Segment.UnitName.RawValue := UnitName;

    Inc(FSegmentCnt);
  end;
end;

function TMapScanner.SegmentsCount: Integer;
begin
  Result := Length(FSegments);
end;

function TMapScanner.SourcesCount: Integer;
begin
  Result := Length(FSourceNames);
end;

{ TJclMapSegmentHelper }

function TJclMapSegmentHelper.Address: Pointer;
begin
  Result := Pointer(StartVA);
end;

function TJclMapSegmentHelper.ModuleName: String;
begin
  Result := TMapScanner.MapStringCacheToModuleName(UnitName);
end;

function TJclMapSegmentHelper.Size: Cardinal;
begin
  Result := EndVA - StartVA; // +1 ???
end;

{ TJclMapProcNameHelper }

function TJclMapProcNameHelper.Address: Pointer;
begin
  Result := Pointer(VA);
end;

function TJclMapProcNameHelper.Name: String;
begin
  Result := TMapScanner.MapStringCacheToStr(ProcName);
end;

{ TJclMapSegmentClassHelper }

function TJclMapSegmentClassHelper.Name: String;
begin
  Result := TMapScanner.MapStringCacheToStr(SectionName);
end;

function TJclMapSegmentClassHelper.SegmentType: TSegmentType;
begin
  Result := TSegmentClassInfo.StrToSegmentType(Name);
end;

end.
