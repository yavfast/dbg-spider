unit ClassUtils;

interface

uses Windows, Classes, SysUtils, StrUtils, XMLDoc, XMLIntf, Graphics, GdiPlus, GdiPlusHelpers, Collections.Dictionaries;

const
  EIndexError: String = 'List index error: %d';

type
  TStringArray = Array of String;

  TGradientInfo = class(TObject)
  private
    FColor: TColor;
    FBrush: IGPBrush;
    FPen: IGPPen;
    FGrList: array of TGPColor;

    procedure Init(const AHeight: Integer; const ADelta: Byte);
    procedure InitGrList(const AHeight: Integer; const ADelta: Byte);
  public
    constructor Create(const AColor: TColor; const AHeight: Integer; const ADelta: Byte);
    destructor Destroy; override;

    property Color: TColor read FColor;
    property Brush: IGPBrush read FBrush;
    property Pen: IGPPen read FPen;
  end;

  TGradientInfoList = class(TObjectDictionary<TColor, TGradientInfo>)
  private
    class var
      _GradientInfoList: TGradientInfoList;
  public
    class function GetGradientInfo(const AColor: TColor; const AHeight: Integer; const ADelta: Byte): TGradientInfo;
  end;

procedure DrawVGradientRect(const GP: IGPGraphics; const Rect: TRect; const Color: TColor; const Delta: Byte = 50);
procedure DrawHInterval(const GP: IGPGraphics; const Rect: TRect; const Color: TColor);

procedure RGBToHSV(const Color: TColor; var h, s, v: Integer);
function HSV2RGB(const h, s, v: Integer): TColor;

procedure ClearStringList(SL: TStringList);
procedure FreeStringList(var SL: TStringList);

procedure ClearList(L: TList);
procedure FreeList(var L: TList);

function IncPointer(Ptr: Pointer; Offset: Integer): Pointer; inline;

procedure SplitStr(const Str: String; const Delimiter: Char; var StrList: TStringArray);

function GetXMLValue(const ParentNode: IXMLNode; const NodeName: String): String;
procedure SetXMLValue(const ParentNode: IXMLNode; const NodeName, NodeValue: String);

function GetXMLChildNode(const ParentNode: IXMLNode; const NodeName: String; const AutoCreate: Boolean = True): IXMLNode;

function Compare(var Value1, Value2: UInt64): Integer; overload; inline;
function Compare(var Value1, Value2: Int64): Integer; overload; inline;
function Compare(var Value1, Value2: NativeInt): Integer; overload; inline;
function Compare(var Value1, Value2: NativeUInt): Integer; overload; inline;

function Compare(var Value1, Value2: String; const EmptyRes: Integer): Integer; overload;
function CompareNumberStr(const Value1, Value2: String): Integer;

implementation

uses Math;

{ TGradientInfo }

constructor TGradientInfo.Create(const AColor: TColor; const AHeight: Integer; const ADelta: Byte);
begin
  inherited Create;

  FColor := AColor;
  Init(AHeight, ADelta);
  InitGrList(AHeight, ADelta);
end;

destructor TGradientInfo.Destroy;
begin
  FBrush := nil;
  FPen := nil;

  inherited;
end;

procedure TGradientInfo.Init(const AHeight: Integer; const ADelta: Byte);
var
  C1, C2: TGPColor;
  H, S, V: Integer;
  R: TGPRect;
begin
  RGBToHSV(FColor, H, S, V);

  C1 := TGPColor.Create(HSV2RGB(H, S, V + ADelta));
  C1.Alpha := $C0;

  C2 := TGPColor.Create(HSV2RGB(H, S, V - ADelta));
  C2.Alpha := $C0;

  R := TGPRect.Create(Rect(0, 0, 1, AHeight + 2));

  FBrush := TGPLinearGradientBrush.Create(R, C1, C2, LinearGradientModeVertical);

  FPen := TGPPen.Create(Brush);
  FPen.Alignment := PenAlignmentInset;
end;

procedure TGradientInfo.InitGrList(const AHeight: Integer; const ADelta: Byte);
var
  I: Integer;
  H, S, V: Integer;
  StartColor: TColor;
  EndColor: TColor;
  LStartRGB, LEndRGB: TColor;
  DeltaR: Double;
  DeltaG: Double;
  DeltaB: Double;
  DeltaColor: TColor;
begin
  SetLength(FGrList, AHeight);

  if AHeight = 0 then Exit;

  RGBToHSV(Color, H, S, V);

  StartColor := HSV2RGB(H, S, V + ADelta);
  EndColor := HSV2RGB(H, S, V - ADelta);

  LStartRGB := ColorToRGB(StartColor);
  LEndRGB := ColorToRGB(EndColor);

  DeltaR := (GetRValue(LEndRGB) - GetRValue(LStartRGB)) / AHeight;
  DeltaG := (GetGValue(LEndRGB) - GetGValue(LStartRGB)) / AHeight;
  DeltaB := (GetBValue(LEndRGB) - GetBValue(LStartRGB)) / AHeight;

  for I := 0 to AHeight - 1 do
  begin
    DeltaColor := RGB(
      GetRValue(LStartRGB) + Round(I * DeltaR),
      GetGValue(LStartRGB) + Round(I * DeltaG),
      GetBValue(LStartRGB) + Round(I * DeltaB)
    );

    FGrList[I] := TGPColor.Create(DeltaColor);
  end;
end;

function CompareNumberStr(const Value1, Value2: String): Integer;
begin
  Result := Length(Value1) - Length(Value2);
  if Result = 0 then
    Result := CompareStr(Value1, Value2);
end;

function Compare(var Value1, Value2: String; const EmptyRes: Integer): Integer;
begin
  if (Value1 <> '') and (Value2 <> '') then
    Result := CompareText(Value1, Value2)
  else
  begin
    if (Value1 = '') and (Value2 <> '') then
      Result := EmptyRes
    else
    if (Value1 <> '') and (Value2 = '') then
      Result := -EmptyRes
    else
      Result := 0;
  end;
end;

procedure DrawVGradientRect(const GP: IGPGraphics; const Rect: TRect; const Color: TColor; const Delta: Byte = 50);
var
  GrInfo: TGradientInfo;
begin
  GrInfo := TGradientInfoList.GetGradientInfo(Color, Rect.Height, Delta);

  if Rect.Left <> Rect.Right then
    GP.FillRectangle(GrInfo.Brush, Rect.Left, Rect.Top, Rect.Width, Rect.Height)
  else
    GP.DrawLine(GrInfo.Pen, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom - 1);
end;

procedure DrawHInterval(const GP: IGPGraphics; const Rect: TRect; const Color: TColor);
var
  Brush: IGPBrush;
  C: TGPColor;
  R: TGPRect;
begin
  if Abs(Rect.Right - Rect.Left) > 1 then
  begin
    C := TGPColor.Create(Color);
    C.Alpha := $20;
    Brush := TGPSolidBrush.Create(C);
    R := TGPRect.Create(Rect);
    GP.FillRectangle(Brush, R);
  end;
end;

(*
procedure DrawVGradientRect(Canvas: TCanvas; const Rect: TRect; const StartColor, EndColor: TColor);
var
  I: Integer;
  R: TRect;
  LStartRGB, LEndRGB: TColor;
  LSteps: Integer;
  DeltaR: Double;
  DeltaG: Double;
  DeltaB: Double;
  DeltaColor: TColor;
begin
  LSteps := Rect.Bottom - Rect.Top;
  if LSteps = 0 then Exit;

  LStartRGB := ColorToRGB(StartColor);
  LEndRGB := ColorToRGB(EndColor);

  DeltaR := (GetRValue(LEndRGB) - GetRValue(LStartRGB)) / LSteps;
  DeltaG := (GetGValue(LEndRGB) - GetGValue(LStartRGB)) / LSteps;
  DeltaB := (GetBValue(LEndRGB) - GetBValue(LStartRGB)) / LSteps;

  R.Left := Rect.Left;
  R.Right := Rect.Right;

  Canvas.Pen.Style := psSolid;

  for I := 0 to LSteps - 1 do
  begin
    R.Top := Rect.Top + I;

    DeltaColor := RGB(
      GetRValue(LStartRGB) + Round(I * DeltaR),
      GetGValue(LStartRGB) + Round(I * DeltaG),
      GetBValue(LStartRGB) + Round(I * DeltaB)
    );

    if R.Left = R.Right then
    begin
      Canvas.Pixels[R.Left, R.Top] := DeltaColor;
    end
    else
    begin
      Canvas.Pen.Color := DeltaColor;
      Canvas.MoveTo(R.Left, R.Top);
      Canvas.LineTo(R.Right, R.Top);
    end;
  end;
end;

procedure DrawVGradientRect2(Canvas: TCanvas; const Rect: TRect; const Color: TColor; const Delta: Byte = 50);
var
  H, S, V: Integer;
  C1, C2: TColor;
begin
  RGBToHSV(Color, H, S, V);

  C1 := HSV2RGB(H, S, V + Delta);
  C2 := HSV2RGB(H, S, V - Delta);

  DrawVGradientRect(Canvas, Rect, C1, C2);
end;
*)

function Compare(var Value1, Value2: Int64): Integer;
begin
  Result := Value1 - Value2;
end;

function Compare(var Value1, Value2: UInt64): Integer;
begin
  Result := Int64(Value1) - Int64(Value2);
end;

function Compare(var Value1, Value2: NativeInt): Integer;
begin
  Result := Value1 - Value2;
end;

function Compare(var Value1, Value2: NativeUInt): Integer;
begin
  Result := NativeInt(Value1) - NativeInt(Value2);
end;

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
  else
    begin r := 0; g := 0; b := 0; end;
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

{ TGradientInfoList }

class function TGradientInfoList.GetGradientInfo(const AColor: TColor;
  const AHeight: Integer; const ADelta: Byte): TGradientInfo;
begin
  if not _GradientInfoList.TryGetValue(AColor, Result) then
  begin
    Result := TGradientInfo.Create(AColor, AHeight, ADelta);
    _GradientInfoList.AddOrSetValue(AColor, Result);
  end;
end;

initialization
  TGradientInfoList._GradientInfoList := TGradientInfoList.Create;

finalization
  FreeAndNil(TGradientInfoList._GradientInfoList);
end.
