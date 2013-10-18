(*
* Copyright (c) 2011-2012, Ciobanu Alexandru
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

unit Collections.Dynamic;
interface
{$IF CompilerVersion > 21}
uses
  SysUtils,
  Generics.Collections,
  Rtti,
  TypInfo;

type
  ///  <summary>Alias for the Rtti <c>TValue</c> type. The compiler seems to have a hard
  ///  time differentiating <c>TValue</c> from the generic <c>TValue</c> type argument.</summary>
  TAny = TValue;

  ///  <summary>An alias to the <v>Variant</c> type. Its main purpose is to serve as a reminder that
  ///  it contains a part of an object. It is to be considered a "dynamic record".</summary>
  TView = Variant;

  ///  <summary>A special purpose record type that exposes a number of methods that generate
  ///  selector methods for fields and properties of a class or record type.</summary>
  Member = record
  private class var
    FViewVariantType: Word;

  private type
    {$REGION 'Internal Types'}
    TViewPair = TPair<string, TValue>;
    TViewArray = TArray<TViewPair>;

    TSelector<T, K> = class(TInterfacedObject, TFunc<T, K>, TFunc<T, TValue>)
    private
      FContext: TRttiContext;
      FType: TRttiType;
      FMember: TRttiMember;

    protected
      function TFunc<T, K>.Invoke = GenericInvoke;
      function TFunc<T, TValue>.Invoke = TValueInvoke;

    public
      function TValueInvoke(AFrom: T): TValue; virtual; abstract;
      function GenericInvoke(AFrom: T): K;
    end;

    TRecordFieldSelector<K> = class(TSelector<Pointer, K>)
    public
      function TValueInvoke(AFrom: Pointer): TValue; override;
    end;

    TClassFieldSelector<K> = class(TSelector<TObject, K>)
    public
      function TValueInvoke(AFrom: TObject): TValue; override;
    end;

    TClassPropertySelector<K> = class(TSelector<TObject, K>)
    public
      function TValueInvoke(AFrom: TObject): TValue; override;
    end;

    TViewSelector<T> = class(TInterfacedObject, TFunc<T, TView>)
    private
      FNames: TArray<string>;
      FFuncs: TArray<TFunc<T, TValue>>;
    public
      function Invoke(AFrom: T): TView;
    end;

    {$ENDREGION}

  public
    ///  <summary>Generates a selector for a given member name.</summary>
    ///  <param name="AName">The field or property name to select from <c>T</c>.</param>
    ///  <returns>A selector function that retrieves the field/property from a class or record.
    ///  <c>nil</c> is returned in case of error.</returns>
    class function Name<T, K>(const AName: string): TFunc<T, K>; overload; static;

    ///  <summary>Generates a selector for a given member name.</summary>
    ///  <param name="AName">The field or property name to select from <c>T</c>.</param>
    ///  <returns>A selector function that retrieves the field/property from a class or record. The selected value is a <c>TValue</c> type.
    ///  <c>nil</c> is returned in case of error.</returns>
    class function Name<T>(const AName: string): TFunc<T, TValue>; overload; static;

    ///  <summary>Generates a selector for the given member names.</summary>
    ///  <param name="ANames">The field or property names to select from <c>T</c>.</param>
    ///  <returns>A selector function that retrieves the fields/properties from a class or record. The selected value is a <c>TView</c> type.
    ///  <c>nil</c> is returned in case of error.</returns>
    class function Name<T>(const ANames: array of string): TFunc<T, TView>; overload; static;
  end;

implementation
uses
  Variants;

type
  { Mapping the TSVDictionary into TVarData structure }
  TViewDictionaryVarData = packed record
    { Var type; will be assigned at run time }
    VType: TVarType;
    { Reserved stuff }
    Reserved1, Reserved2, Reserved3: Word;
    { A reference to the enclosed dictionary }
    FArray: Member.TViewArray;
    { Reserved stuff }
    Reserved4: LongWord;
{$IFDEF CPUX64}
    Reserved4_1: LongWord;
{$ENDIF}
  end;

  { Manager for our variant type }
  TViewDictionaryVariantType = class(TInvokeableVariantType)
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    function GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean; override;
  end;

{ TViewDictionaryVariantType }

procedure TViewDictionaryVariantType.Clear(var V: TVarData);
begin
  { Clear the variant type }
  V.VType := varEmpty;

  { And dispose the value }
  TViewDictionaryVarData(V).FArray := nil;
end;

procedure TViewDictionaryVariantType.Copy(var Dest: TVarData;
  const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
  begin
    with TViewDictionaryVarData(Dest) do
    begin
      { Copy the variant type }
      VType := VarType;

      { Copy the reference }
      FArray := TViewDictionaryVarData(Source).FArray;
    end;
  end;
end;

function TViewDictionaryVariantType.GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean;
var
  LPair: Member.TViewPair;
  LAsVar: Variant;
begin
  { Iterate over our internal array and search for the requested property by name }
  with TViewDictionaryVarData(V) do
  begin
    for LPair in FArray do
      if AnsiSameStr(LPair.Key, Name) then
      begin
        LAsVar := LPair.Value.AsVariant;
        Dest := TVarData(LAsVar);

        Exit(True);
      end;
  end;

  { Key not found, means error }
  Clear(Dest);
  Result := False;
end;

var
  { Our singleton that manages our variant type }
  FViewDictionaryVariantType: TViewDictionaryVariantType;

{ Member.TSelector<T, K> }

function Member.TSelector<T, K>.GenericInvoke(AFrom: T): K;
var
  LValue: TValue;
begin
  LValue := TValueInvoke(AFrom);
  Result := LValue.AsType<K>();
end;

{ Member.TRecordFieldSelector<T, K> }

function Member.TRecordFieldSelector<K>.TValueInvoke(AFrom: Pointer): TValue;
begin
  ASSERT(Assigned(FMember));
  ASSERT(FMember is TRttiField);

  Result := TRttiField(FMember).GetValue(AFrom);
end;

{ Member.TClassFieldSelector<K> }

function Member.TClassFieldSelector<K>.TValueInvoke(AFrom: TObject): TValue;
begin
  ASSERT(Assigned(FMember));
  ASSERT(FMember is TRttiField);

  Result := TRttiField(FMember).GetValue(AFrom);
end;

{ Member.TClassPropertySelector<K> }

function Member.TClassPropertySelector<K>.TValueInvoke(AFrom: TObject): TValue;
begin
  ASSERT(Assigned(FMember));
  ASSERT(FMember is TRttiProperty);

  Result := TRttiProperty(FMember).GetValue(AFrom);
end;

{ Member.TViewSelector<T> }

function Member.TViewSelector<T>.Invoke(AFrom: T): TView;
var
  I, L: NativeInt;
  LCalc: TViewArray;
begin
  { Initialize a view }
  VarClear(Result);

  L := Length(FFuncs);
  SetLength(LCalc, L);

  { Copy selected fields over }
  for I := 0 to Length(FFuncs) - 1 do
  begin
    LCalc[I].Key := FNames[I];
    LCalc[I].Value := FFuncs[I](AFrom);
  end;

  { Give the result to the guy standing on the chair ... }
  with TVarData(Result) do
  begin
    VType := Member.FViewVariantType;
    Member.TViewArray(VPointer) := LCalc;
  end;
end;

{ Member }

class function Member.Name<T, K>(const AName: string): TFunc<T, K>;
var
  LT, LK: PTypeInfo;
  LContext: TRttiContext;
  LType: TRttiType;
  LMember: TRttiMember;
  LSelector: TSelector<T, K>;
begin
  Result := nil;

  { Get the type }
  LT := TypeInfo(T);
  LK := TypeInfo(K);

  LType := LContext.GetType(LT);

  { Check for correctness }
  if not Assigned(LType) or not (LType.TypeKind in [tkClass, tkRecord]) then
    Exit;

  if LType.TypeKind = tkRecord then
  begin
    LMember := LType.GetField(AName);

    if not Assigned(LMember) then
      Exit;

    LSelector := TSelector<T, K>(TRecordFieldSelector<K>.Create());
  end else
  if LType.TypeKind = tkClass then
  begin
    LMember := LType.GetField(AName);

    if Assigned(LMember) then
      LSelector := TSelector<T, K>(TClassFieldSelector<K>.Create())
    else begin
      LMember := LType.GetProperty(AName);

      if not Assigned(LMember) then
        Exit;

      LSelector := TSelector<T, K>(TClassPropertySelector<K>.Create());
    end;
  end;

  { Upload selector }
  LSelector.FContext := LContext;
  LSelector.FType := LType;
  LSelector.FMember := LMember;

  Result := LSelector;
end;

class function Member.Name<T>(const AName: string): TFunc<T, TValue>;
begin
  Result := Member.Name<T, TValue>(AName);
end;

class function Member.Name<T>(const ANames: array of string): TFunc<T, TView>;
var
  LSelector: TViewSelector<T>;
  I, L: NativeInt;
begin
  Result := nil;
  L := Length(ANames);

  if L = 0 then
    Exit;

  LSelector := TViewSelector<T>.Create;

  { Prepare the array of selectors }
  SetLength(LSelector.FNames, L);
  SetLength(LSelector.FFuncs, L);

  { Create the array }
  for I := 0 to L - 1 do
  begin
    LSelector.FNames[I] := AnsiUpperCase(ANames[I]);
    LSelector.FFuncs[I] := Member.Name<T, TValue>(ANames[I]);

    if not Assigned(LSelector.FFuncs[I]) then
    begin
      LSelector.Free;
      Exit;
    end;
  end;

  Result := LSelector;
end;

initialization
  { Register our custom variant type }
  FViewDictionaryVariantType := TViewDictionaryVariantType.Create();
  Member.FViewVariantType := FViewDictionaryVariantType.VarType;

finalization
  { Uregister our custom variant }
  FreeAndNil(FViewDictionaryVariantType);
{$ELSE}
implementation
{$IFEND}
end.
