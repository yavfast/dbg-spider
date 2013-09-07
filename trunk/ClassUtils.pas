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

procedure RGBToHSV(const Color: TColor; var h, s, v: Integer);
function HSV2RGB(const h, s, v: Integer): TColor;

implementation

uses Math;

// h=[0..360] s,v=[0..255]
procedure RGBToHSV(const Color: TColor; var h, s, v: Integer);
var
  Delta: Integer;
  MinRGB, MaxRGB: Integer;
  r, g, b: Byte;
begin
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);

  MinRGB := Min(r, Min(g, b));
  MaxRGB := Max(r, Max(g, b));

  v := MaxRGB;
  Delta := MaxRGB - MinRGB;

  if MaxRGB = 0 then
    s := 0
  else
    s := (255 * Delta) div MaxRGB;

  if s = 0 then
    h := 0
  else
  begin
    if r = MaxRGB then
      h := (60 * (g - b)) div Delta
    else
    if g = MaxRGB then
      h := 120 + (60 * (b - r)) div Delta
    else
      h := 240 + (60 * (r - g)) div Delta;

    if h < 0 then
      h := h + 360;
  end;
end;

function HSV2RGB(const h, s, v: Integer): TColor;
var
  Hi: Integer;
  f, p, q, t: Double;
  r, g, b: Double;
  sf, vf: Double;
  hf: Integer;
begin
  sf := Max(0, Min(255, s));
  vf := Max(0, Min(255, v));
  hf := Max(0, Min(360, h));

  sf := sf / 255;
  vf := vf / 255;

  f := hf / 60 - (hf div 60);
  p := vf * (1 - sf);
  q := vf * (1 - f * sf);
  t := vf * (1 - (1 - f) * sf);

  Hi := (hf div 60) mod 6;
  case Hi of
    0: begin r := vf; g := t; b := p; end;
    1: begin r := q; g := vf; b := p; end;
    2: begin r := p; g := vf; b := t; end;
    3: begin r := p; g := q; b := vf; end;
    4: begin r := t; g := p; b := vf; end;
    5: begin r := vf; g := p; b := q; end;
  end;

  Result := RGB(Round(r * 255), Round(g * 255), Round(b * 255));
end;


procedure SetXMLValue(const ParentNode: IXMLNode; const NodeName, NodeValue: String);
begin
  if Assigned(ParentNode) then
    ParentNode.ChildValues[AnsiLowerCase(NodeName)] := NodeValue;
end;

function GetXMLChildNode(const ParentNode: IXMLNode; const NodeName: String; const AutoCreate: Boolean = True): IXMLNode;
begin
  Result := nil;

  if Assigned(ParentNode) then
  begin
    Result := ParentNode.ChildNodes.FindNode(AnsiLowerCase(NodeName));
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
    ResNode := ParentNode.ChildNodes.FindNode(AnsiLowerCase(NodeName));
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
