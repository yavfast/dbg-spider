(*
* Copyright (c) 2009-2012, Ciobanu Alexandru
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

unit Collections.Bags;
interface
uses SysUtils,
     Generics.Defaults,
     Generics.Collections,
     Collections.Base,
     Collections.Dictionaries;

type
  ///  <summary>The abstract base class for all <c>bag</c> collections.</summary>
  ///  <remarks>This base class introduces and implements all bag operations. Specific <c>bag</c> implementations must
  ///  override only one method in order to create the specific dictionary type that is going to hold the element to weight associations.</remarks>
  TAbstractBag<T> = class(TCollection<T>, IBag<T>)
  private type
    {$REGION 'Internal Types'}
    TEnumerator = class(TAbstractEnumerator<T>)
    private
      FCurrentWeight: NativeInt;
      FDictionaryEnumerator: IEnumerator<TPair<T, NativeUInt>>;
    public
      function TryMoveNext(out ACurrent: T): Boolean; override;
    end;
    {$ENDREGION}

  private var
    FDictionary: IDictionary<T, NativeUInt>;
    FKnownCount: NativeInt;

  protected
    ///  <summary>Specifies the internal dictionary used to store the element to weight associations.</summary>
    ///  <remarks>The value of this property is <c>nil</c> only when the object is still being created.</remarks>
    ///  <returns>A new dictionary whose keys are the bag's elements and the values are the weights associated with
    ///  those elements.</returns>
    property Dictionary: IDictionary<T, NativeUInt> read FDictionary;

    ///  <summary>Returns the number of elements in the bag.</summary>
    ///  <returns>A positive value specifying the number of elements in the bag.</returns>
    ///  <remarks>The count of a bag is calculated by taking each element multiplied by its weight. For example, if the bag only contains one element
    ///  with weight <c>10</c>, then the size of the bag is <c>10</c>.</remarks>
    function GetCount(): NativeInt; override;

    ///  <summary>Returns the weight of an element in the bag.</summary>
    ///  <param name="AValue">The element to return the weight for.</param>
    ///  <returns>A positive number specifying the weight of the requested element.</returns>
    ///  <remarks>If the value is not found in the bag, a zero weight is assumed.</remarks>
    function GetWeight(const AValue: T): NativeUInt;

    ///  <summary>Sets the weight of an element in the bag.</summary>
    ///  <param name="AValue">The element to set the weight for.</param>
    ///  <param name="AWeight">The new weight to set.</param>
    ///  <remarks>If the value is not found in the bag, this method acts like an <c>Add</c> operation; otherwise
    ///  the weight of the stored item is adjusted accordingly.</remarks>
    procedure SetWeight(const AValue: T; const AWeight: NativeUInt);

    ///  <summary>Called when the map needs to initialize its internal dictionary.</summary>
    ///  <param name="ARules">The rule set describing the elements.</param>
    ///  <remarks>This method creates a hash-based dictionary used as the underlying back-end for the bag.</remarks>
    function CreateDictionary(const ARules: TRules<T>): IDictionary<T, NativeUInt>; virtual; abstract;
  public
    ///  <summary>Creates a new <c>bag</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the bag.</param>
    constructor Create(const ARules: TRules<T>);

    ///  <summary>Clears the contents of the bag.</summary>
    procedure Clear(); override;

    ///  <summary>Adds an element into the bag with a weight of <c>1</c>.</summary>
    ///  <param name="AValue">The value to add.</param>
    procedure Add(const AValue: T); override;

    ///  <summary>Adds an element to the bag.</summary>
    ///  <param name="AValue">The element to add.</param>
    ///  <param name="AWeight">The weight of the element.</param>
    ///  <remarks>If the bag already contains the given value, its stored weight is incremented to by <paramref name="AWeight"/>.
    ///  If the value of <paramref name="AWeight"/> is zero, nothing happens.</remarks>
    procedure AddWeight(const AValue: T; const AWeight: NativeUInt = 1);

    ///  <summary>Decreases the weight for an element by <c>1</c>.</summary>
    ///  <param name="AValue">The value to decrese weight for.</param>
    procedure Remove(const AValue: T); override;

    ///  <summary>Removes an element from the bag.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <param name="AWeight">The weight to remove.</param>
    ///  <remarks>This method decreses the weight of the stored item by <paramref name="AWeight"/>. If the resulting weight is less
    ///  than zero or zero, the element is removed from the bag. If <paramref name="AWeight"/> is zero, nothing happens.</remarks>
    procedure RemoveWeight(const AValue: T; const AWeight: NativeUInt = 1);

    ///  <summary>Removes an element from the bag.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>This method completely removes an item from the bag ignoring its stored weight. Nothing happens if the given value
    ///  is not in the bag to begin with.</remarks>
    procedure RemoveAllWeight(const AValue: T);

    ///  <summary>Checks whether the bag contains an element with at least the weight of <c>1</c>.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the condition is met; <c>False</c> otherwise.</returns>
    function Contains(const AValue: T): Boolean; override;

    ///  <summary>Checks whether the bag contains an element with at least the required weight.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <param name="AWeight">The smallest allowed weight.</param>
    ///  <returns><c>True</c> if the condition is met; <c>False</c> otherwise.</returns>
    ///  <remarks>This method checks whether the bag contains the given value and that the contained value has at least the
    ///  given weight.</remarks>
    function ContainsWeight(const AValue: T; const AWeight: NativeUInt = 1): Boolean;

    ///  <summary>Sets or gets the weight of an item in the bag.</summary>
    ///  <param name="AValue">The value.</param>
    ///  <remarks>If the value is not found in the bag, this method acts like an <c>Add</c> operation; otherwise
    ///  the weight of the stored item is adjusted.</remarks>
    property Weights[const AValue: T]: NativeUInt read GetWeight write SetWeight; default;

    ///  <summary>Returns the number of elements in the bag.</summary>
    ///  <returns>A positive value specifying the number of elements in the bag.</returns>
    ///  <remarks>The count of a bag is calculated by taking each element multiplied by its weight. For example, if the bag only contains one element
    ///  with weight <c>10</c>, then the size of the bag is <c>10</c>.</remarks>
    property Count: NativeInt read FKnownCount;

    ///  <summary>Returns a new enumerator object used to enumerate this bag.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the bag.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<T>; override;

    ///  <summary>Copies the values stored in the bag to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the bag.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the bag.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Checks whether the bag is empty.</summary>
    ///  <returns><c>True</c> if the bag is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the bag is empty.</remarks>
    function Empty(): Boolean; override;

    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the bag considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The bag is empty.</exception>
    function Max(): T; override;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the bag considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The bag is empty.</exception>
    function Min(): T; override;

    ///  <summary>Returns the first element.</summary>
    ///  <returns>The first element in the bag.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The bag is empty.</exception>
    function First(): T; override;

    ///  <summary>Returns the first element or a default, if the bag is empty.</summary>
    ///  <param name="ADefault">The default value returned if the bag is empty.</param>
    ///  <returns>The first element in the bag if the bag is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function FirstOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the last element.</summary>
    ///  <returns>The last element in the bag.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The bag is empty.</exception>
    function Last(): T; override;

    ///  <summary>Returns the last element or a default, if the bag is empty.</summary>
    ///  <param name="ADefault">The default value returned if the bag is empty.</param>
    ///  <returns>The last element in the bag if the bag is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function LastOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the single element stored in the bag.</summary>
    ///  <returns>The element in the bag.</returns>
    ///  <remarks>This method checks whether the bag contains just one element, in which case it is returned.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The bag is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionNotOneException">There is more than one element in the bag.</exception>
    function Single(): T; override;

    ///  <summary>Returns the single element stored in the bag, or a default value.</summary>
    ///  <param name="ADefault">The default value returned if there are less or more elements in the bag.</param>
    ///  <returns>The element in the bag if the condition is satisfied; <paramref name="ADefault"/> is returned otherwise.</returns>
    ///  <remarks>This method checks whether the bag contains just one element, in which case it is returned. Otherwise
    ///  the value in <paramref name="ADefault"/> is returned.</remarks>
    function SingleOrDefault(const ADefault: T): T; override;

    ///  <summary>Checks whether at least one element in the bag satisfies a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if at least one element satisfies a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole bag and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>True</c>. The logical equivalent of this operation is "OR".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Any(const APredicate: TPredicate<T>): Boolean; override;

    ///  <summary>Checks that all elements in the bag satisfy a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if all elements satisfy a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole bag and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>False</c>. The logical equivalent of this operation is "AND".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function All(const APredicate: TPredicate<T>): Boolean; override;
  end;

type
  ///  <summary>The generic <c>bag</c> collection.</summary>
  ///  <remarks>This particular <c>bag</c> implementation uses a hash-based dictionary to store its element to weight associations.</remarks>
  TBag<T> = class(TAbstractBag<T>)
  private var
    FInitialCapacity: NativeInt;

  protected
    ///  <summary>Called when the bag needs to initialize its internal dictionary.</summary>
    ///  <param name="ARules">The rule set describing the bag's elements.</param>
    ///  <remarks>This method creates a hash-based dictionary used as the underlying back-end for the bag.</remarks>
    function CreateDictionary(const ARules: TRules<T>): IDictionary<T, NativeUInt>; override;
  public
    ///  <summary>Creates a new <c>bag</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>bag</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the bag.</param>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Creates a new <c>bag</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the bag.</param>
    ///  <param name="AInitialCapacity">The stack's initial capacity.</param>
    constructor Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt); overload;

  end;

  ///  <summary>The generic <c>bag</c> collection designed to store objects.</summary>
  ///  <remarks>This particular <c>bag</c> implementation uses a hash-based dictionary to store its element to weight associations.</remarks>
  TObjectBag<T: class> = class(TBag<T>)
  private
    FOwnsObjects: Boolean;

  protected
    ///  <summary>Frees the object that was removed from the collection.</summary>
    ///  <param name="AElement">The object that was removed from the collection.</param>
    ///  <remarks>This method will only free the removed element if <c>OwnsObjects</c> property is set to <c>True</c>;
    ///  otherwise it will simply be ignored.</remarks>
    procedure HandleElementRemoved(const AElement: T); override;

  public
    ///  <summary>Specifies whether this bag owns the elements stored in it.</summary>
    ///  <returns><c>True</c> if the bag owns its elements; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the bag controls the life-time of its elements.</remarks>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

type
  ///  <summary>The generic sorted <c>bag</c> collection.</summary>
  ///  <remarks>This particular <c>bag</c> implementation uses an AVL-based dictionary to store its element to weight associations.</remarks>
  TSortedBag<T> = class(TAbstractBag<T>)
  private var
    FAscendingSort: Boolean;

  protected
    ///  <summary>Called when the bag needs to initialize its internal dictionary.</summary>
    ///  <param name="ARules">The rule set describing the bag's elements.</param>
    ///  <remarks>This method creates an AVL-based dictionary used as the underlying back-end for the bag.</remarks>
    function CreateDictionary(const ARules: TRules<T>): IDictionary<T, NativeUInt>; override;
  public
    ///  <summary>Creates a new <c>bag</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed. The elements are stored in ascending order.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>bag</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the bag.</param>
    ///  <remarks>The elements are stored in ascending order.</remarks>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Creates a new <c>bag</c> collection.</summary>
    ///  <param name="AAscending">Pass in a value of <c>True</c> if the elements should be kept in ascending order.
    ///  Pass in <c>False</c> for descending order.</param>
    ///  <param name="ARules">A rule set describing the elements in the bag.</param>
    constructor Create(const ARules: TRules<T>; const AAscending: Boolean); overload;
  end;

  ///  <summary>The generic sorted <c>bag</c> collection designed to store objects.</summary>
  ///  <remarks>This particular <c>bag</c> implementation uses an AVL-based dictionary to store its element to weight associations.</remarks>
  TObjectSortedBag<T: class> = class(TSortedBag<T>)
  private
    FOwnsObjects: Boolean;

  protected
    ///  <summary>Frees the object that was removed from the collection.</summary>
    ///  <param name="AElement">The object that was removed from the collection.</param>
    ///  <remarks>This method will only free the removed element if <c>OwnsObjects</c> property is set to <c>True</c>;
    ///  otherwise it will simply be ignored.</remarks>
    procedure HandleElementRemoved(const AElement: T); override;

  public
    ///  <summary>Specifies whether this bag owns the elements stored in it.</summary>
    ///  <returns><c>True</c> if the bag owns its elements; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the bag controls the life-time of its elements.</remarks>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

implementation

{ TAbstractBag<T> }

procedure TAbstractBag<T>.Add(const AValue: T);
begin
  AddWeight(AValue, 1);
end;

procedure TAbstractBag<T>.AddWeight(const AValue: T; const AWeight: NativeUInt);
var
  LOldCount: NativeUInt;
begin
  { Check count > 0 }
  if AWeight = 0 then
    Exit;

  { Add or update count }
  if FDictionary.TryGetValue(AValue, LOldCount) then
    FDictionary[AValue] := LOldCount + AWeight
  else
    FDictionary.Add(AValue, AWeight);

  Inc(FKnownCount, AWeight);
  NotifyCollectionChanged();
end;

function TAbstractBag<T>.All(const APredicate: TPredicate<T>): Boolean;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.All(APredicate);
end;

function TAbstractBag<T>.Any(const APredicate: TPredicate<T>): Boolean;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.Any(APredicate);
end;

procedure TAbstractBag<T>.Clear;
begin
  if Assigned(FDictionary) then
  begin
    { Simply clear the dictionary }
    FDictionary.Clear();

    FKnownCount := 0;
    NotifyCollectionChanged();
  end;
end;

function TAbstractBag<T>.Contains(const AValue: T): Boolean;
begin
  Result := ContainsWeight(AValue, 1);
end;

function TAbstractBag<T>.ContainsWeight(const AValue: T; const AWeight: NativeUInt): Boolean;
var
  LInCount: NativeUInt;
begin
  { Check count > 0 }
  if AWeight = 0 then
    Exit(true);

  { Check the counts in the bag }
  Result := (FDictionary.TryGetValue(AValue, LInCount)) and (LInCount >= AWeight);
end;

procedure TAbstractBag<T>.CopyTo(var AArray: array of T; const AStartIndex: NativeInt);
var
  LTempArray: array of TPair<T, NativeUInt>;
  I, X, Y: NativeInt;
begin
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  { Check for indexes }
  if (Length(AArray) - AStartIndex) < Count then
    ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Nothing to do? }
  if Count = 0 then
    Exit;

  { Initialize the temporary array }
  SetLength(LTempArray, FDictionary.Count);
  FDictionary.CopyTo(LTempArray);

  X := AStartIndex;

  { OK! Now let's simply copy }
  for I := 0 to Length(LTempArray) - 1 do
  begin
    { Copy one value for a number of counts }
    for Y := 0 to LTempArray[I].Value - 1 do
    begin
      AArray[X] := LTempArray[I].Key;
      Inc(X);
    end;
  end;
end;

constructor TAbstractBag<T>.Create(const ARules: TRules<T>);
begin
  inherited Create(ARules);
  FDictionary := CreateDictionary(ElementRules);
end;

function TAbstractBag<T>.Empty: Boolean;
begin
  Result := (FKnownCount = 0);
end;

function TAbstractBag<T>.First: T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.First();
end;

function TAbstractBag<T>.FirstOrDefault(const ADefault: T): T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.FirstOrDefault(ADefault);
end;

function TAbstractBag<T>.Last: T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.Last();
end;

function TAbstractBag<T>.LastOrDefault(const ADefault: T): T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.LastOrDefault(ADefault);
end;

function TAbstractBag<T>.Max: T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.Max();
end;

function TAbstractBag<T>.Min: T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.Min();
end;

function TAbstractBag<T>.GetCount: NativeInt;
begin
  { Dictionary knows the real count }
  Result := FKnownCount;
end;

function TAbstractBag<T>.GetWeight(const AValue: T): NativeUInt;
begin
  { Get the count }
  if not FDictionary.TryGetValue(AValue, Result) then
    Result := 0;
end;

function TAbstractBag<T>.GetEnumerator: IEnumerator<T>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FDictionaryEnumerator := FDictionary.GetEnumerator();
  Result := LEnumerator;
end;

procedure TAbstractBag<T>.RemoveWeight(const AValue: T; const AWeight: NativeUInt);
var
  LOldCount: NativeUInt;
begin
  { Check count > 0 }
  if AWeight = 0 then
    Exit;

  { Check that the key os present in the dictionary first }
  if not FDictionary.TryGetValue(AValue, LOldCount) then
    Exit;

  if LOldCount < AWeight then
    LOldCount := 0
  else
    LOldCount := LOldCount - AWeight;

  { Update the counts }
  if LOldCount = 0 then
    FDictionary.Remove(AValue)
  else
    FDictionary[AValue] := LOldCount;

  Dec(FKnownCount, AWeight);
  NotifyCollectionChanged();
end;

procedure TAbstractBag<T>.Remove(const AValue: T);
begin
  RemoveWeight(AValue, 1);
end;

procedure TAbstractBag<T>.RemoveAllWeight(const AValue: T);
var
  LOldCount: NativeUInt;
begin
  { Check that the key is present in the dictionary first }
  if not FDictionary.TryGetValue(AValue, LOldCount) then
    Exit;

  FDictionary.Remove(AValue);

  Dec(FKnownCount, LOldCount);
  NotifyCollectionChanged();
end;

procedure TAbstractBag<T>.SetWeight(const AValue: T; const AWeight: NativeUInt);
var
  LOldValue: NativeUInt;
begin
  { Check count > 0 }
  if Count = 0 then
    Exit;

  if FDictionary.ContainsKey(AValue) then
  begin
    LOldValue := FDictionary[AValue];
    FDictionary[AValue] := AWeight;
  end else
  begin
    LOldValue := 0;
    FDictionary.Add(AValue, AWeight);
  end;

  { Change the counts }
  FKnownCount := FKnownCount - NativeInt(LOldValue + AWeight);
  NotifyCollectionChanged();
end;

function TAbstractBag<T>.Single: T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.Single();
end;

function TAbstractBag<T>.SingleOrDefault(const ADefault: T): T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.SingleOrDefault(ADefault);
end;

{ TAbstractBag<T>.TEnumerator }

function TAbstractBag<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  { Repeat until something happens }
  while True do
  begin
    if FCurrentWeight > 0 then
    begin
      { Decrease the count of the bag item }
      Dec(FCurrentWeight);
      Result := True;
      Break;
    end else
    begin
      Result := FDictionaryEnumerator.MoveNext();
      if Result then
        FCurrentWeight := FDictionaryEnumerator.Current.Value
      else
        Break;
    end;
  end;

  if Result then
    ACurrent := FDictionaryEnumerator.Current.Key;
end;

{ TBag<T> }

constructor TBag<T>.Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt);
begin
  FInitialCapacity := AInitialCapacity;
  inherited Create(ARules);
end;

constructor TBag<T>.Create;
begin
  Create(TRules<T>.Default, CDefaultSize);
end;

constructor TBag<T>.Create(const ARules: TRules<T>);
begin
  Create(ARules, CDefaultSize);
end;

function TBag<T>.CreateDictionary(const ARules: TRules<T>): IDictionary<T, NativeUInt>;
var
  LNewCapacity: NativeInt;
  LDictionary: TDictionary<T, NativeUInt>;
begin
  { Create a simple dictionary }
  if FInitialCapacity <= 0 then
    LNewCapacity := CDefaultSize
  else
    LNewCapacity := FInitialCapacity;

  LDictionary := TDictionary<T, NativeUInt>.Create(ARules, TRules<NativeUInt>.Default, LNewCapacity);
  LDictionary.KeyRemoveNotification := NotifyElementRemoved;

  Result := LDictionary;
end;

{ TObjectBag<T> }

procedure TObjectBag<T>.HandleElementRemoved(const AElement: T);
begin
  if FOwnsObjects then
    TObject(AElement).Free;
end;

{ TSortedBag<T> }

constructor TSortedBag<T>.Create;
begin
  Create(TRules<T>.Default, True);
end;

constructor TSortedBag<T>.Create(const ARules: TRules<T>);
begin
  Create(ARules, True);
end;

function TSortedBag<T>.CreateDictionary(const ARules: TRules<T>): IDictionary<T, NativeUInt>;
var
  LDictionary: TSortedDictionary<T, NativeUInt>;
begin
  { Create a sorted dictionary }
  LDictionary := TSortedDictionary<T, NativeUInt>.Create(ARules, TRules<NativeUInt>.Default, FAscendingSort);
  LDictionary.KeyRemoveNotification := NotifyElementRemoved;

  Result := LDictionary;
end;

constructor TSortedBag<T>.Create(const ARules: TRules<T>; const AAscending: Boolean);
begin
  { Call upper constructor }
  FAscendingSort := AAscending;
  inherited Create(ARules);
end;

{ TObjectSortedBag<T> }

procedure TObjectSortedBag<T>.HandleElementRemoved(const AElement: T);
begin
  if FOwnsObjects then
    TObject(AElement).Free;
end;

end.

