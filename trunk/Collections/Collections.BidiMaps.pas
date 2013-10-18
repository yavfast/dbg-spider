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

unit Collections.BidiMaps;
interface
uses SysUtils,
     Generics.Defaults,
     Generics.Collections,
     Collections.Base,
     Collections.MultiMaps;

type
  ///  <summary>The base abstract class for all <c>bidi-maps</c> in this package.</summary>
  TAbstractBidiMap<TKey, TValue> = class abstract(TAbstractMap<TKey, TValue>, IBidiMap<TKey, TValue>)
  private
    FByKeyMap: IMultiMap<TKey, TValue>;
    FByValueMap: IMultiMap<TValue, TKey>;

    { Got from the underlying collections }
    FValueCollection: ISequence<TValue>;
    FKeyCollection: ISequence<TKey>;

  protected
    ///  <summary>Specifies the internal map used as back-end to store key relations.</summary>
    ///  <returns>A map used as back-end.</summary>
    property ByKeyMap: IMultiMap<TKey, TValue> read FByKeyMap;

    ///  <summary>Specifies the internal map used as back-end to store value relations.</summary>
    ///  <returns>A map used as back-end.</summary>
    property ByValueMap: IMultiMap<TValue, TKey> read FByValueMap;

    ///  <summary>Called when the map needs to initialize its internal key map.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    function CreateKeyMap(const AKeyRules: TRules<TKey>;
      const AValueRules: TRules<TValue>): IMultiMap<TKey, TValue>; virtual; abstract;

    ///  <summary>Called when the map needs to initialize its internal value map.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    function CreateValueMap(const AValueRules: TRules<TValue>;
      const AKeyRules: TRules<TKey>): IMultiMap<TValue, TKey>; virtual; abstract;

    ///  <summary>Returns the number of pairs in the bidi-map.</summary>
    ///  <returns>A positive value specifying the total number of pairs in the bidi-map.</returns>
    function GetCount(): NativeInt; override;

    ///  <summary>Returns the collection of keys associated with a value.</summary>
    ///  <param name="AValue">The value for which to obtain the associated keys.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The value is not found in the bidi-map.</exception>
    function GetKeysByValue(const AValue: TValue): ISequence<TKey>;

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the bidi-map.</exception>
    function GetValuesByKey(const AKey: TKey): ISequence<TValue>;
  public
    ///  <summary>Creates a new <c>bi-directional map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Clears the contents of the bidi-map.</summary>
    procedure Clear(); override;

    ///  <summary>Adds a key-value pair to the bidi-map.</summary>
    ///  <param name="AKey">The key of the pair.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The map already contains a pair with the given key.</exception>
    procedure Add(const AKey: TKey; const AValue: TValue); overload; override;

    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key (and its associated values) to remove.</param>
    ///  <remarks>This method removes all the values that are associated with the given key. The rule set's cleanup
    ///  routines are used to clean up the values that are dropped from the bidi-map.</remarks>
    procedure RemoveValuesForKey(const AKey: TKey);

    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key of the pair.</param>
    ///  <remarks>If the specified key was not found in the bidi-map, nothing happens.</remarks>
    procedure Remove(const AKey: TKey); overload; override;

    ///  <summary>Removes a key-value pair using a given value.</summary>
    ///  <param name="AValue">The value (and its associated keys) to remove.</param>
    ///  <remarks>This method removes all the keys that are associated with the given value. The rule set's cleanup
    ///  routines are used to clean up the keys that are dropped from the bidi-map.</remarks>
    procedure RemoveKeysForValue(const AValue: TValue);

    ///  <summary>Removes a specific key-value combination.</summary>
    ///  <param name="AKey">The key to remove.</param>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>This method only removes a key-value combination if that combination actually exists in the bidi-map.
    ///  If the key is associated with another value, nothing happens.</remarks>
    procedure RemovePair(const AKey: TKey; const AValue: TValue); overload;

    ///  <summary>Removes a key-value combination.</summary>
    ///  <param name="APair">The pair to remove.</param>
    ///  <remarks>This method only removes a key-value combination if that combination actually exists in the bidi-map.
    ///  If the key is associated with another value, nothing happens.</remarks>
    procedure RemovePair(const APair: TPair<TKey, TValue>); overload;

    ///  <summary>Checks whether the map contains a key-value pair identified by the given key.</summary>
    ///  <param name="AKey">The key to check for.</param>
    ///  <returns><c>True</c> if the map contains a pair identified by the given key; <c>False</c> otherwise.</returns>
    function ContainsKey(const AKey: TKey): Boolean; override;

    ///  <summary>Checks whether the map contains a key-value pair that contains a given value.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the map contains a pair containing the given value; <c>False</c> otherwise.</returns>
    function ContainsValue(const AValue: TValue): Boolean; override;

    ///  <summary>Checks whether the map contains the given key-value combination.</summary>
    ///  <param name="AKey">The key associated with the value.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const AKey: TKey; const AValue: TValue): Boolean; overload;

    ///  <summary>Checks whether the map contains a given key-value combination.</summary>
    ///  <param name="APair">The key-value pair combination.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const APair: TPair<TKey, TValue>): Boolean; overload;

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the bidi-map.</exception>
    property ByKey[const AKey: TKey]: ISequence<TValue> read GetValuesByKey;

    ///  <summary>Returns the collection of keys associated with a value.</summary>
    ///  <param name="AValue">The value for which to obtain the associated keys.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The value is not found in the bidi-map.</exception>
    property ByValue[const AValue: TValue]: ISequence<TKey> read GetKeysByValue;

    ///  <summary>Specifies the collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the bidi-map.</returns>
    property Keys: ISequence<TKey> read FKeyCollection;

    ///  <summary>Specifies the collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the bidi-map.</returns>
    property Values: ISequence<TValue> read FValueCollection;

    ///  <summary>Returns the number of pairs in the bidi-map.</summary>
    ///  <returns>A positive value specifying the total number of pairs in the bidi-map.</returns>
    property Count: NativeInt read GetCount;

    ///  <summary>Returns a new enumerator object used to enumerate this bidi-map.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the bidi-map.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<TPair<TKey, TValue>>; override;

    ///  <summary>Copies the values stored in the bidi-map to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the bidi-map.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the bidi-map.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of TPair<TKey,TValue>; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to return the associated value.</param>
    ///  <returns>The value associated with the given key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">No such key in the bidi-map.</exception>
    function ValueForKey(const AKey: TKey): TValue; override;

    ///  <summary>Checks whether the bidi-map contains a given key-value pair.</summary>
    ///  <param name="AKey">The key part of the pair.</param>
    ///  <param name="AValue">The value part of the pair.</param>
    ///  <returns><c>True</c> if the given key-value pair exists; <c>False</c> otherwise.</returns>
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean; override;

    ///  <summary>Returns an Enex collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the bidi-map.</returns>
    function SelectKeys(): ISequence<TKey>; override;

    ///  <summary>Returns an Enex collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the bidi-map.</returns>
    function SelectValues(): ISequence<TValue>; override;
  end;

type
  ///  <summary>The generic <c>bidirectional map</c> collection.</summary>
  ///  <remarks>This type uses <c>distinct multimaps</c> to store its keys and values.</remarks>
  TBidiMap<TKey, TValue> = class(TAbstractBidiMap<TKey, TValue>)
  private
    FInitialCapacity: NativeInt;

  protected
    ///  <summary>Called when the map needs to initialize the key multimap.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a distinct multimap used as the underlying back-end for the map.</remarks>
    function CreateKeyMap(const AKeyRules: TRules<TKey>;
      const AValueRules: TRules<TValue>): IMultiMap<TKey, TValue>; override;

    ///  <summary>Called when the map needs to initialize the value multimap.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a distinct multimap used as the underlying back-end for the map.</remarks>
    function CreateValueMap(const AValueRules: TRules<TValue>;
      const AKeyRules: TRules<TKey>): IMultiMap<TValue, TKey>; override;

  public
    ///  <summary>Creates a new <c>bi-directional map</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>bi-directional map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new <c>bi-directional map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    ///  <param name="AInitialCapacity">The map's initial capacity.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>; const AInitialCapacity: NativeInt); overload;
  end;

  ///  <summary>The generic <c>bidirectional map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses <c>distinct multimaps</c> to store its keys and values.</remarks>
  TObjectBidiMap<TKey, TValue> = class(TBidiMap<TKey, TValue>)
  private
    FOwnsKeys, FOwnsValues: Boolean;

  protected
    ///  <summary>Frees the key (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The key that was removed from the collection.</param>
    procedure HandleKeyRemoved(const AKey: TKey); override;

    ///  <summary>Frees the value (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The value that was removed from the collection.</param>
    procedure HandleValueRemoved(const AValue: TValue); override;
  public
    ///  <summary>Specifies whether this map owns the keys.</summary>
    ///  <returns><c>True</c> if the map owns the keys; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specififies the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specififes the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>bidirectional map</c> collection.</summary>
  ///  <remarks>This type uses <c>sorted distinct multimaps</c> to store its keys and values.</remarks>
  TSortedBidiMap<TKey, TValue> = class(TAbstractBidiMap<TKey, TValue>)
  private
    FAscendingSort: Boolean;

  protected
    ///  <summary>Called when the map needs to initialize the key multimap.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a sorted distinct multimap used as the underlying back-end for the map.</remarks>
    function CreateKeyMap(const AKeyRules: TRules<TKey>;
      const AValueRules: TRules<TValue>): IMultiMap<TKey, TValue>; override;

    ///  <summary>Called when the map needs to initialize the value multimap.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a sorted distinct multimap used as the underlying back-end for the map.</remarks>
    function CreateValueMap(const AValueRules: TRules<TValue>;
      const AKeyRules: TRules<TKey>): IMultiMap<TValue, TKey>; override;
  public
    ///  <summary>Creates a new <c>bi-directional map</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed. The keys are stored in ascending order.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>bi-directional map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    ///  <remarks>The keys are stored in ascending order.</remarks>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new <c>bi-directional map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    ///  <param name="AAscending">Pass in a value of <c>True</c> if the keys should be kept in ascending order.
    ///  Pass in <c>False</c> for descending order.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>; const AAscending: Boolean); overload;

    ///  <summary>Returns the biggest key.</summary>
    ///  <returns>The biggest key stored in the map.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The map is empty.</exception>
    function MaxKey(): TKey; override;

    ///  <summary>Returns the smallest key.</summary>
    ///  <returns>The smallest key stored in the map.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The map is empty.</exception>
    function MinKey(): TKey; override;
  end;

  ///  <summary>The generic <c>bidirectional map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses <c>sorted distinct multimaps</c> to store its keys and values.</remarks>
  TObjectSortedBidiMap<TKey, TValue> = class(TSortedBidiMap<TKey, TValue>)
  private
    FOwnsKeys, FOwnsValues: Boolean;

  protected
    ///  <summary>Frees the key (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The key that was removed from the collection.</param>
    procedure HandleKeyRemoved(const AKey: TKey); override;

    ///  <summary>Frees the value (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The value that was removed from the collection.</param>
    procedure HandleValueRemoved(const AValue: TValue); override;
  public
    ///  <summary>Specifies whether this map owns the keys.</summary>
    ///  <returns><c>True</c> if the map owns the keys; <c>False</c> otherwise.</returns>
    ///  <remarks>This propertyspecififes the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>bidirectional map</c> collection.</summary>
  ///  <remarks>This type uses <c>double sorted distinct multimaps</c> to store its keys and values.</remarks>
  TDoubleSortedBidiMap<TKey, TValue> = class(TSortedBidiMap<TKey, TValue>)
  private
    FAscendingKeys, FAscendingValues: Boolean;

  protected
    ///  <summary>Called when the map needs to initialize the key multimap.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a double sorted distinct multimap used as the underlying back-end for the map.</remarks>
    function CreateKeyMap(const AKeyRules: TRules<TKey>;
      const AValueRules: TRules<TValue>): IMultiMap<TKey, TValue>; override;

    ///  <summary>Called when the map needs to initialize the value multimap.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a double sorted distinct multimap used as the underlying back-end for the map.</remarks>
    function CreateValueMap(const AValueRules: TRules<TValue>;
      const AKeyRules: TRules<TKey>): IMultiMap<TValue, TKey>; override;
  public
    ///  <summary>Creates a new <c>bi-directional map</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed. The keys and values are stored in ascending order.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>bi-directional map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    ///  <remarks>The keys and values are stored in ascending order.</remarks>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new <c>bi-directional map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    ///  <param name="AAscendingKeys">Pass in a value of <c>True</c> if the keys should be kept in ascending order.
    ///  Pass in <c>False</c> for descending order.</param>
    ///  <param name="AAscendingValues">Pass in a value of <c>True</c> if the values should be kept in ascending order.
    ///  Pass in <c>False</c> for descending order.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const AAscendingKeys: Boolean; const AAscendingValues: Boolean); overload;

    ///  <summary>Returns the biggest key.</summary>
    ///  <returns>The biggest key stored in the map.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The map is empty.</exception>
    function MaxKey(): TKey; override;

    ///  <summary>Returns the smallest key.</summary>
    ///  <returns>The smallest key stored in the map.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The map is empty.</exception>
    function MinKey(): TKey; override;
  end;

  ///  <summary>The generic <c>bidirectional map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses <c>double sorted distinct multimaps</c> to store its keys and values.</remarks>
  TObjectDoubleSortedBidiMap<TKey, TValue> = class(TDoubleSortedBidiMap<TKey, TValue>)
  private
    FOwnsKeys, FOwnsValues: Boolean;

  protected
    ///  <summary>Frees the key (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The key that was removed from the collection.</param>
    procedure HandleKeyRemoved(const AKey: TKey); override;

    ///  <summary>Frees the value (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The value that was removed from the collection.</param>
    procedure HandleValueRemoved(const AValue: TValue); override;
  public
    ///  <summary>Specifies whether this map owns the keys.</summary>
    ///  <returns><c>True</c> if the map owns the keys; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specififes the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

implementation


{ TAbstractBidiMap<TKey, TValue> }

procedure TAbstractBidiMap<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
  { Add the K/V pair to the maps }
  FByKeyMap.Add(AKey, AValue);
  FByValueMap.Add(AValue, AKey);
end;

procedure TAbstractBidiMap<TKey, TValue>.Clear;
begin
  if Assigned(FByKeyMap) then
    FByKeyMap.Clear;

  if Assigned(FByValueMap) then
    FByValueMap.Clear;
end;

function TAbstractBidiMap<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  Result := FByKeyMap.ContainsKey(AKey);
end;

function TAbstractBidiMap<TKey, TValue>.ContainsPair(const APair: TPair<TKey, TValue>): Boolean;
begin
  { The by-key relation since it is always correct }
  Result := FByKeyMap.ContainsPair(APair.Key, APair.Value);
end;

function TAbstractBidiMap<TKey, TValue>.ContainsPair(const AKey: TKey; const AValue: TValue): Boolean;
begin
  { The by-key relation since it is always correct }
  Result := FByKeyMap.ContainsPair(AKey, AValue);
end;

function TAbstractBidiMap<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
begin
  Result := FByValueMap.ContainsKey(AValue);
end;

procedure TAbstractBidiMap<TKey, TValue>.CopyTo(var AArray: array of TPair<TKey, TValue>; const AStartIndex: NativeInt);
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Call the underlying collection }
  FByKeyMap.CopyTo(AArray, AStartIndex);
end;

constructor TAbstractBidiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>);
begin
  { Install the types }
  inherited Create(AKeyRules, AValueRules);

  { Create the maps }
  FByKeyMap := CreateKeyMap(AKeyRules, ValueRules);
  FByValueMap := CreateValueMap(AValueRules, KeyRules);

  { The collections }
  FValueCollection := FByValueMap.Keys;
  FKeyCollection := FByKeyMap.Keys;
end;

function TAbstractBidiMap<TKey, TValue>.GetCount: NativeInt;
begin
  { The count follows the map properties }
  Result := FByKeyMap.Count;
end;

function TAbstractBidiMap<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Pass the enumerator from the key map }
  Result := FByKeyMap.GetEnumerator();
end;

function TAbstractBidiMap<TKey, TValue>.GetKeysByValue(const AValue: TValue): ISequence<TKey>;
begin
  Result := FByValueMap[AValue];
end;

function TAbstractBidiMap<TKey, TValue>.GetValuesByKey(const AKey: TKey): ISequence<TValue>;
begin
  Result := FByKeyMap[AKey];
end;

function TAbstractBidiMap<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
begin
  Result := ContainsPair(AKey, AValue);
end;

procedure TAbstractBidiMap<TKey, TValue>.RemovePair(const AKey: TKey; const AValue: TValue);
var
  LValues: ISequence<TValue>;
  LValue: TValue;
begin
  { Check whether there is such a key }
  if not FByKeyMap.ContainsPair(AKey, AValue) then
    Exit;

  { Remove the stuff }
  FByKeyMap.RemovePair(AKey, AValue);
  FByValueMap.RemovePair(AValue, AKey);
end;

procedure TAbstractBidiMap<TKey, TValue>.RemovePair(const APair: TPair<TKey, TValue>);
begin
  RemovePair(APair.Key, APair.Value);
end;

procedure TAbstractBidiMap<TKey, TValue>.Remove(const AKey: TKey);
begin
  RemoveValuesForKey(AKey);
end;

procedure TAbstractBidiMap<TKey, TValue>.RemoveValuesForKey(const AKey: TKey);
var
  LValues: ISequence<TValue>;
  LValue: TValue;
begin
  { Check whether there is such a key }
  if not FByKeyMap.TryGetValues(AKey, LValues) then
    Exit;

  { Exclude the key for all values too }
  for LValue in LValues do
    FByValueMap.RemovePair(LValue, AKey);

  { And finally remove the key }
  FByKeyMap.Remove(AKey);
end;

procedure TAbstractBidiMap<TKey, TValue>.RemoveKeysForValue(const AValue: TValue);
var
  LKeys: ISequence<TKey>;
  LValue: TKey;
begin
  { Check whether there is such a key }
  if not FByValueMap.TryGetValues(AValue, LKeys) then
    Exit;

  { Exclude the key for all values too}
  for LValue in LKeys do
    FByKeyMap.RemovePair(LValue, AValue);

  { And finally remove the key }
  FByValueMap.Remove(AValue);
end;

function TAbstractBidiMap<TKey, TValue>.SelectKeys: ISequence<TKey>;
begin
  { Pass the values on }
  Result := Keys;
end;

function TAbstractBidiMap<TKey, TValue>.SelectValues: ISequence<TValue>;
begin
  { Pass the value on }
  Result := Values;
end;

function TAbstractBidiMap<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
begin
  Result := FByKeyMap[AKey].First;
end;

{ TBidiMap<TKey, TValue> }

constructor TBidiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const AInitialCapacity: NativeInt);
begin
  FInitialCapacity := AInitialCapacity;
  inherited Create(AKeyRules, AValueRules);
end;

constructor TBidiMap<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, CDefaultSize);
end;

constructor TBidiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>);
begin
  Create(AKeyRules, AValueRules, CDefaultSize);
end;

function TBidiMap<TKey, TValue>.CreateKeyMap(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>): IMultiMap<TKey, TValue>;
var
  LNewCapacity: NativeInt;
  LMap: TDistinctMultiMap<TKey, TValue>;
begin
  { Create a simple dictionary }
  if FInitialCapacity <= 0 then
    LNewCapacity := CDefaultSize
  else
    LNewCapacity := FInitialCapacity;

  { Use a simple non-sorted map }
  LMap := TDistinctMultiMap<TKey, TValue>.Create(AKeyRules, AValueRules, LNewCapacity);
  LMap.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LMap;
end;

function TBidiMap<TKey, TValue>.CreateValueMap(const AValueRules: TRules<TValue>;
  const AKeyRules: TRules<TKey>): IMultiMap<TValue, TKey>;
var
  LNewCapacity: NativeInt;
  LMap: TDistinctMultiMap<TValue, TKey>;
begin
  { Create a simple dictionary }
  if FInitialCapacity <= 0 then
    LNewCapacity := CDefaultSize
  else
    LNewCapacity := FInitialCapacity;

  { Use a simple non-sorted map }
  LMap := TDistinctMultiMap<TValue, TKey>.Create(AValueRules, AKeyRules, LNewCapacity);
  LMap.KeyRemoveNotification := NotifyValueRemoved;

  Result := LMap;
end;

{ TObjectBidiMap<TKey, TValue> }

procedure TObjectBidiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    PObject(@AKey)^.Free;
end;

procedure TObjectBidiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    PObject(@AValue)^.Free;
end;

{ TSortedBidiMap<TKey, TValue> }

constructor TSortedBidiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>; const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscendingSort := AAscending;
  inherited Create(AKeyRules, AValueRules);
end;

constructor TSortedBidiMap<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, True);
end;

constructor TSortedBidiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>);
begin
  Create(AKeyRules, AValueRules, True);
end;

function TSortedBidiMap<TKey, TValue>.CreateKeyMap(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>): IMultiMap<TKey, TValue>;
var
  LMap: TSortedDistinctMultiMap<TKey, TValue>;
begin
  { Use a simple sorted map }
  LMap := TSortedDistinctMultiMap<TKey, TValue>.Create(AKeyRules, AValueRules, FAscendingSort);
  LMap.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LMap;
end;

function TSortedBidiMap<TKey, TValue>.CreateValueMap(const AValueRules: TRules<TValue>;
  const AKeyRules: TRules<TKey>): IMultiMap<TValue, TKey>;
var
  LMap: TSortedDistinctMultiMap<TValue, TKey>;
begin
  { Use a simple sorted map }
  LMap := TSortedDistinctMultiMap<TValue, TKey>.Create(AValueRules, AKeyRules, FAscendingSort);
  LMap.KeyRemoveNotification := NotifyValueRemoved;
  Result := LMap;
end;

function TSortedBidiMap<TKey, TValue>.MaxKey: TKey;
begin
  Result := ByKeyMap.MaxKey;
end;

function TSortedBidiMap<TKey, TValue>.MinKey: TKey;
begin
  Result := ByKeyMap.MinKey;
end;

{ TObjectSortedBidiMap<TKey, TValue> }

procedure TObjectSortedBidiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    PObject(@AKey)^.Free;
end;

procedure TObjectSortedBidiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    PObject(@AValue)^.Free;
end;

{ TDoubleSortedBidiMap<TKey, TValue> }

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const AAscendingKeys, AAscendingValues: Boolean);
begin
  { Do the dew and continue! }
  FAscendingKeys := AAscendingKeys;
  FAscendingValues := AAscendingValues;

  inherited Create(AKeyRules, AValueRules);
end;

constructor TDoubleSortedBidiMap<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, True, True);
end;

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>);
begin
  Create(AKeyRules, AValueRules, True, True);
end;

function TDoubleSortedBidiMap<TKey, TValue>.CreateKeyMap(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>): IMultiMap<TKey, TValue>;
var
  LMap: TDoubleSortedDistinctMultiMap<TKey, TValue>;
begin
  { Use a double sorted map }
  LMap := TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(AKeyRules, AValueRules, FAscendingKeys, FAscendingValues);
  LMap.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LMap;
end;

function TDoubleSortedBidiMap<TKey, TValue>.CreateValueMap(const AValueRules: TRules<TValue>;
  const AKeyRules: TRules<TKey>): IMultiMap<TValue, TKey>;
var
  LMap: TDoubleSortedDistinctMultiMap<TValue, TKey>;
begin
  { Use a double sorted map }
  LMap := TDoubleSortedDistinctMultiMap<TValue, TKey>.Create(AValueRules, AKeyRules, FAscendingKeys, FAscendingValues);
  LMap.KeyRemoveNotification := NotifyValueRemoved;
  Result := LMap;
end;

function TDoubleSortedBidiMap<TKey, TValue>.MaxKey: TKey;
begin
  Result := ByKeyMap.MaxKey;
end;

function TDoubleSortedBidiMap<TKey, TValue>.MinKey: TKey;
begin
  Result := ByKeyMap.MinKey;
end;

{ TObjectDoubleSortedBidiMap<TKey, TValue> }

procedure TObjectDoubleSortedBidiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    PObject(@AKey)^.Free;
end;

procedure TObjectDoubleSortedBidiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    PObject(@AValue)^.Free;
end;

end.
