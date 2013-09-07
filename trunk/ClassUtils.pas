unit ClassUtils;

interface

uses Windows, Classes, SysUtils, StrUtils, XMLDoc, XMLIntf, Graphics;

const
  EIndexError: String = 'List index error: %d';

type
  TStringArray = Array of String;

procedure ClearStringList(SL: TStringList);
procedure FreeStringList(var SL: TStringList);

procedure ClearList(L: TList);
procedure FreeList(var L: TList);

function IncPointer(Ptr: Pointer; Offset: Integer): Pointer; inline;

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
function FileTimeToInt64(const FileTime: TFileTime): UInt64;
function Int64ToFileTime(const Value: UInt64): TFileTime;

procedure SplitStr(const Str: String; const Delimiter: Char; var StrList: TStringArray);

function GetXMLValue(const ParentNode: IXMLNode; const NodeName: String): String;
procedure SetXMLValue(const ParentNode: IXMLNode; const NodeName, NodeValue: String);

function GetXMLChildNode(const ParentNode: IXMLNode; const NodeName: String; const AutoCreate: Boolean = True): IXMLNode;

implementation

procedure SetXMLValue(const ParentNode: IXMLNode; const NodeName, NodeValue: String);
begin
  if Assigned(ParentNode) then
    ParentNode.ChildValues[NodeName] := NodeValue;
end;

function GetXMLChildNode(const ParentNode: IXMLNode; const NodeName: String; const AutoCreate: Boolean = True): IXMLNode;
begin
  Result := nil;

  if Assigned(ParentNode) then
  begin
    Result := ParentNode.ChildNodes.FindNode(NodeName);
    if not Assigned(Result) and AutoCreate then
      Result := ParentNode.AddChild(NodeName);
  end;
end;

function GetXMLValue(const ParentNode: IXMLNode; const NodeName: String): String;
var
  ResNode: IXMLNode;
begin
  Result := '';
  if Assigned(ParentNode) then
  begin
    ResNode := ParentNode.ChildNodes.FindNode(NodeName);
    if Assigned(ResNode) and ResNode.IsTextElement then
      Result := ResNode.Text;
  end;
end;

procedure SplitStr(const Str: String; const Delimiter: Char; var StrList: TStringArray);
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Delimiter := Delimiter;
    SL.StrictDelimiter;
    SL.Duplicates := dupAccept;

    SL.DelimitedText := Str;

    SetLength(StrList, SL.Count);
    for I := 0 to SL.Count - 1 do
      StrList[I] := SL.Strings[I];
  finally
    FreeAndNil(SL);
  end;
end;

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
var
  ModifiedTime: TFileTime;
  SystemTime: TSystemTime;
begin
  Result := 0;
  if (FileTime.dwLowDateTime = 0) and (FileTime.dwHighDateTime = 0) then
    Exit;

  if FileTimeToLocalFileTime(FileTime, ModifiedTime) then
    if FileTimeToSystemTime(ModifiedTime, SystemTime) then
      Result := SystemTimeToDateTime(SystemTime);
end;

function FileTimeToInt64(const FileTime: TFileTime): UInt64;
begin
  Result := UInt64(UInt64(FileTime.dwHighDateTime) shl 32) or FileTime.dwLowDateTime;
end;

function Int64ToFileTime(const Value: UInt64): TFileTime;
begin
  Result.dwLowDateTime := DWORD(Value);
  Result.dwHighDateTime := DWORD(Value shr 32);
end;

function IncPointer(Ptr: Pointer; Offset: Integer): Pointer;
begin
  Result := Pointer(Integer(Ptr) + Offset);
end;

procedure ClearStringList(SL: TStringList);
var
  I: Integer;
  Obj: TObject;
begin
  if SL = nil then Exit;

  for I := 0 to SL.Count - 1 do
  begin
    Obj := SL.Objects[I];
    if Obj <> nil then
    begin
      SL.Objects[I] := nil;
      FreeAndNil(Obj);
    end;
  end;

  SL.Clear;
end;

procedure FreeStringList(var SL: TStringList);
begin
  ClearStringList(SL);
  FreeAndNil(SL);
end;

procedure ClearList(L: TList);
var
  I: Integer;
  Obj: TObject;
begin
  if L = nil then Exit;
  
  for I := 0 to L.Count - 1 do
  begin
    Obj := L[I];
    if Obj <> nil then
    begin
      L[I] := nil;
      FreeAndNil(Obj);
    end;
  end;

  L.Clear;
end;

procedure FreeList(var L: TList);
begin
  ClearList(L);
  FreeAndNil(L);
end;

end.
