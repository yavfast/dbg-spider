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

unit Collections.MultiMaps;
interface
uses SysUtils,
     Generics.Defaults,
     Generics.Collections,
     Collections.Base,
     Collections.Lists,
     Collections.Sets,
     Collections.Dictionaries;

type
  ///  <summary>The base abstract class for all <c>multi-maps</c> in this package.</summary>
  TAbstractMultiMap<TKey, TValue> = class abstract(TAbstractMap<TKey, TValue>, IMultiMap<TKey, TValue>)
  private type
    {$REGION 'Internal Types'}
    TEnumerator = class(TAbstractEnumerator<TPair<TKey, TValue>>)
    private
      FDictionaryEnumerator: IEnumerator<TPair<TKey, ICollection<TValue>>>;
      FCollectionEnumerator: IEnumerator<TValue>;
    public
      constructor Create(const AOwner: TAbstractMultiMap<TKey, TValue>);
      function TryMoveNext(out ACurrent: TPair<TKey, TValue>): Boolean; override;
    end;

    TValueEnumerator = class(TAbstractEnumerator<TValue>)
    private
      FOwnerEnumerator: IEnumerator<TPair<TKey, TValue>>;
    public
      constructor Create(const AOwner: TAbstractMultiMap<TKey, TValue>);
      function TryMoveNext(out ACurrent: TValue): Boolean; override;
    end;

    TValueSequence = class(TSequence<TValue>)
    private
      FOwner: TAbstractMultiMap<TKey, TValue>;
    protected
      function GetCount(): NativeInt; override;
    public
      constructor Create(const AOwner: TAbstractMultiMap<TKey, TValue>);
      function GetEnumerator(): IEnumerator<TValue>; override;
      procedure CopyTo(var AArray: array of TValue; const AStartIndex: NativeInt); overload; override;
      function Empty(): Boolean; override;
    end;
    {$ENDREGION}

  private
    FKnownCount: NativeInt;
    FEmpty: ICollection<TValue>;
    FKeyCollection: ISequence<TKey>;
    FValueCollection: ISequence<TValue>;
    FDictionary: IDictionary<TKey, ICollection<TValue>>;
    FLastKey: TKey;
    FLastCollection: ICollection<TValue>;

  protected
    ///  <summary>Specifies the internal dictionary used as back-end.</summary>
    ///  <returns>A dictionary of lists used as back-end.</summary>
    property Dictionary: IDictionary<TKey, ICollection<TValue>> read FDictionary;

    ///  <summary>Returns the number of pairs in the multi-map.</summary>
    ///  <returns>A positive value specifying the total number of pairs in the multi-map.</returns>
    ///  <remarks>The value returned by this method represents the total number of key-value pairs
    ///  stored in the dictionary. In a multi-map, this means that each value associated with a key
    ///  is calculated as a pair. If a key has multiple values associated with it, each key-value
    ///  combination is calculated as one.</remarks>
    function GetCount(): NativeInt; override;

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    function GetValues(const AKey: TKey): ISequence<TValue>;

    ///  <summary>Called when the map needs to initialize its internal dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    function CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, ICollection<TValue>>; virtual; abstract;

    ///  <summary>Called when the map needs to initialize a list associated with a key.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    function CreateCollection(const AValueRules: TRules<TValue>): ICollection<TValue>; virtual; abstract;
  public
    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>);

    ///  <summary>Clears the contents of the multi-map.</summary>
    procedure Clear(); override;

    ///  <summary>Adds a key-value pair to the multi-map.</summary>
    ///  <param name="AKey">The key of the pair.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The multi-map already contains a pair with the given key.</exception>
    procedure Add(const AKey: TKey; const AValue: TValue); overload; override;

    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key of pair.</param>
    ///  <remarks>If the specified key was not found in the multi-map, nothing happens.</remarks>
    procedure Remove(const AKey: TKey); overload; override;

    ///  <summary>Extracts all values using their key.</summary>
    ///  <param name="AKey">The key of the associated values.</param>
    ///  <returns>A collection of values associated with the key.</returns>
    ///  <remarks>This function is identical to <c>RemoveKey</c> but will return the associated values. If there is no given key, an exception is raised.</remarks>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The <paramref name="AKey"/> is not part of the map.</exception>
    function ExtractValues(const AKey: TKey): ISequence<TValue>;

    ///  <summary>Removes a key-value pair using a given key and value.</summary>
    ///  <param name="AKey">The key associated with the value.</param>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>A multi-map allows storing multiple values for a given key. This method allows removing only the
    ///  specified value from the collection of values associated with the given key.</remarks>
    procedure RemovePair(const AKey: TKey; const AValue: TValue); overload;

    ///  <summary>Removes a key-value pair using a given key and value.</summary>
    ///  <param name="APair">The key and its associated value to remove.</param>
    ///  <remarks>A multi-map allows storing multiple values for a given key. This method allows removing only the
    ///  specified value from the collection of values associated with the given key.</remarks>
    procedure RemovePair(const APair: TPair<TKey, TValue>); overload;

    ///  <summary>Checks whether the multi-map contains a key-value pair identified by the given key.</summary>
    ///  <param name="AKey">The key to check for.</param>
    ///  <returns><c>True</c> if the map contains a pair identified by the given key; <c>False</c> otherwise.</returns>
    function ContainsKey(const AKey: TKey): Boolean; override;

    ///  <summary>Checks whether the multi-map contains a key-value pair that contains a given value.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the multi-map contains a pair containing the given value; <c>False</c> otherwise.</returns>
    function ContainsValue(const AValue: TValue): Boolean; overload; override;

    ///  <summary>Checks whether the multi-map contains a given key-value combination.</summary>
    ///  <param name="AKey">The key associated with the value.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const AKey: TKey; const AValue: TValue): Boolean; overload;

    ///  <summary>Checks whether the multi-map contains a given key-value combination.</summary>
    ///  <param name="APair">The key-value pair to check for.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const APair: TPair<TKey, TValue>): Boolean; overload;

    ///  <summary>Tries to extract the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <param name="AValues">The Enex collection that stores the associated values.</param>
    ///  <returns><c>True</c> if the key exists in the collection; <c>False</c> otherwise.</returns>
    function TryGetValues(const AKey: TKey; out AValues: ISequence<TValue>): Boolean; overload;

    ///  <summary>Tries to extract the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>The associated collection if the key is valid; an empty collection otherwise.</returns>
    function TryGetValues(const AKey: TKey): ISequence<TValue>; overload;

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the multi-map.</exception>
    property Items[const AKey: TKey]: ISequence<TValue> read GetValues; default;

    ///  <summary>Returns the number of pairs in the multi-map.</summary>
    ///  <returns>A positive value specifying the total number of pairs in the multi-map.</returns>
    ///  <remarks>The value returned by this method represents the total number of key-value pairs
    ///  stored in the dictionary. In a multi-map, this means that each value associated with a key
    ///  is calculated as a pair. If a key has multiple values associated with it, each key-value
    ///  combination is calculated as one.</remarks>
    property Count: NativeInt read FKnownCount;

    ///  <summary>Specifies the collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the multi-map.</returns>
    property Keys: ISequence<TKey> read FKeyCollection;

    ///  <summary>Specifies the collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the multi-map.</returns>
    property Values: ISequence<TValue> read FValueCollection;

    ///  <summary>Returns a new enumerator object used to enumerate this multi-map.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the multi-map.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<TPair<TKey,TValue>>; override;

    ///  <summary>Copies the values stored in the multi-map to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the multi-map.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the multi-map.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of TPair<TKey,TValue>; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to return the associated value.</param>
    ///  <returns>The value associated with the given key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">No such key in the multi-map.</exception>
    function ValueForKey(const AKey: TKey): TValue; override;

    ///  <summary>Checks whether the multi-map contains a given key-value pair.</summary>
    ///  <param name="AKey">The key part of the pair.</param>
    ///  <param name="AValue">The value part of the pair.</param>
    ///  <returns><c>True</c> if the given key-value pair exists; <c>False</c> otherwise.</returns>
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean; override;

    ///  <summary>Returns an Enex collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the multi-map.</returns>
    function SelectKeys(): ISequence<TKey>; override;

    ///  <summary>Returns an Enex collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the multi-map.</returns>
    function SelectValues(): ISequence<TValue>; override;
  end;

type
  ///  <summary>The generic <c>multi-map</c> collection.</summary>
  ///  <remarks>This type uses a <c>dictionary</c> and a number of <c>lists</c> to store its
  ///  keys and values.</remarks>
  TMultiMap<TKey, TValue> = class(TAbstractMultiMap<TKey, TValue>)
  private
    FInitialCapacity: NativeInt;

  protected
    ///  <summary>Called when the map needs to initialize its internal dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <remarks>This method creates a hash-based dictionary used as the underlying back-end for the map.</remarks>
    function CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, ICollection<TValue>>; override;

    ///  <summary>Called when the map needs to initialize a list associated with a key.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a simple array-based list. This list is associated with a key and stores the map's
    ///  values for that key.</remarks>
    function CreateCollection(const AValueRules: TRules<TValue>): ICollection<TValue>; override;
  public
    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    ///  <param name="AInitialCapacity">The map's initial capacity.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const AInitialCapacity: NativeInt); overload;
  end;

  ///  <summary>The generic <c>multi-map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a <c>dictionary</c> and a number of <c>lists</c> to store its
  ///  keys and values.</remarks>
  TObjectMultiMap<TKey, TValue> = class(TMultiMap<TKey, TValue>)
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
    ///  <remarks>This property specifies the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>multi-map</c> collection.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> and a number of <c>lists</c> to store its
  ///  keys and values.</remarks>
  TSortedMultiMap<TKey, TValue> = class(TAbstractMultiMap<TKey, TValue>)
  private
    FAscendingSort: Boolean;

  protected
    ///  <summary>Called when the map needs to initialize its internal dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <remarks>This method creates an AVL dictionary used as the underlying back-end for the map.</remarks>
    function CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, ICollection<TValue>>; override;

    ///  <summary>Called when the map needs to initialize a list associated with a key.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a simple array-based list. This list is associated with a key and store the map's
    ///  values for that key.</remarks>
    function CreateCollection(const AValueRules: TRules<TValue>): ICollection<TValue>; override;
  public
    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed. The keys are stored in ascending order.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    ///  <remarks>The keys are stored in ascending order.</remarks>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
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

  ///  <summary>The generic <c>multi-map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> and a number of <c>lists</c> to store its
  ///  keys and values.</remarks>
  TObjectSortedMultiMap<TKey, TValue> = class(TSortedMultiMap<TKey, TValue>)
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
    ///  <remarks>This property specifies the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>multi-map</c> collection.</summary>
  ///  <remarks>This type uses a <c>dictionary</c> and a number of <c>sets</c> to store its
  ///  keys and values.</remarks>
  TDistinctMultiMap<TKey, TValue> = class(TAbstractMultiMap<TKey, TValue>)
  private
    FInitialCapacity: NativeInt;

  protected
    ///  <summary>Called when the map needs to initialize its internal dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <remarks>This method creates a hash-based dictionary used as the underlying back-end for the map.</remarks>
    function CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, ICollection<TValue>>; override;

    ///  <summary>Called when the map needs to initialize a set associated with a key.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a hash-based set. This set is associated with a key and stores the map's
    ///  values for that key.</remarks>
    function CreateCollection(const AValueRules: TRules<TValue>): ICollection<TValue>; override;
  public
    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    ///  <param name="AInitialCapacity">The map's initial capacity.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>; const AInitialCapacity: NativeInt); overload;
  end;

  ///  <summary>The generic <c>multi-map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a <c>dictionary</c> and a number of <c>sets</c> to store its
  ///  keys and values.</remarks>
  TObjectDistinctMultiMap<TKey, TValue> = class(TDistinctMultiMap<TKey, TValue>)
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
    ///  <remarks>This property specifies the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>distinct multi-map</c> collection.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> and a number of <c>sorted sets</c> to store its
  ///  keys and values.</remarks>
  TSortedDistinctMultiMap<TKey, TValue> = class(TAbstractMultiMap<TKey, TValue>)
  private
    FAscendingSort: Boolean;

  protected
    ///  <summary>Called when the map needs to initialize its internal dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <remarks>This method creates an AVL dictionary used as the underlying back-end for the map.</remarks>
    function CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, ICollection<TValue>>; override;

    ///  <summary>Called when the map needs to initialize a set associated with a key.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates an AVL-based set. This set is associated with a key and stores the map's
    ///  values for that key.</remarks>
    function CreateCollection(const AValueRules: TRules<TValue>): ICollection<TValue>; override;
  public
    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed. The keys are stored in ascending order.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    ///  <remarks>The keys are stored in ascending order.</remarks>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
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

  ///  <summary>The generic <c>distinct multi-map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> and a number of <c>sorted sets</c> to store its
  ///  keys and values.</remarks>
  TObjectSortedDistinctMultiMap<TKey, TValue> = class(TSortedDistinctMultiMap<TKey, TValue>)
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
    ///  <remarks>This property specifies the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>multi-map</c> collection.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> and a number of <c>sorted lists</c> to store its
  ///  keys and values.</remarks>
  TDoubleSortedMultiMap<TKey, TValue> = class(TSortedMultiMap<TKey, TValue>)
  private
    FAscendingValues: Boolean;

  protected
    ///  <summary>Called when the map needs to initialize a list associated with a key.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a simple array-based sorted list. This list is associated with a key and stores the map's
    ///  values for that key.</remarks>
    function CreateCollection(const AValueRules: TRules<TValue>): ICollection<TValue>; override;
  public
    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed. The keys and values are stored in ascending order.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    ///  <remarks>The keys and values are stored in ascending order.</remarks>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    ///  <param name="AAscendingKeys">Pass in a value of <c>True</c> if the keys should be kept in ascending order.
    ///  Pass in <c>False</c> for descending order.</param>
    ///  <param name="AAscendingValues">Pass in a value of <c>True</c> if the values should be kept in ascending order.
    ///  Pass in <c>False</c> for descending order.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const AAscendingKeys: Boolean; const AAscendingValues: Boolean); overload;
  end;

  ///  <summary>The generic <c>multi-map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> and a number of <c>sorted lists</c> to store its
  ///  keys and values.</remarks>
  TObjectDoubleSortedMultiMap<TKey, TValue> = class(TDoubleSortedMultiMap<TKey, TValue>)
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
    ///  <remarks>This property specifies the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>multi-map</c> collection.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> and a number of <c>sorted sets</c> to store its
  ///  keys and values.</remarks>
  TDoubleSortedDistinctMultiMap<TKey, TValue> = class(TSortedDistinctMultiMap<TKey, TValue>)
  private
    FAscendingValues: Boolean;

  protected
    ///  <summary>Called when the map needs to initialize a set associated with a key.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates an AVL-based set. This set is associated with a key and stores the map's
    ///  values for that key.</remarks>
    function CreateCollection(const AValueRules: TRules<TValue>): ICollection<TValue>; override;
  public
    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed. The keys and values are stored in ascending order.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    ///  <remarks>The keys and values are stored in ascending order.</remarks>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new <c>multi-map</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the map.</param>
    ///  <param name="AAscendingKeys">Pass in a value of <c>True</c> if the keys should be kept in ascending order.
    ///  Pass in <c>False</c> for descending order.</param>
    ///  <param name="AAscendingValues">Pass in a value of <c>True</c> if the values should be kept in ascending order.
    ///  Pass in <c>False</c> for descending order.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const AAscendingKeys: Boolean; const AAscendingValues: Boolean); overload;
  end;

  ///  <summary>The generic <c>multi-map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> and a number of <c>sorted sets</c> to store its
  ///  keys and values.</remarks>
  TObjectDoubleSortedDistinctMultiMap<TKey, TValue> = class(TDoubleSortedDistinctMultiMap<TKey, TValue>)
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
    ///  <remarks>This property specifies the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

implementation


{ TAbstractMultiMap<TKey, TValue> }

procedure TAbstractMultiMap<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
  if not KeysAreEqual(AKey, FLastKey) or not Assigned(FLastCollection) then
  begin
    { Try to look-up what we need. Create a new LList and add it if required. }
    if not FDictionary.TryGetValue(AKey, FLastCollection) then
    begin
      FLastCollection := CreateCollection(ValueRules);
      FDictionary[AKey] := FLastCollection;
    end;

    FLastKey := AKey;
  end;

  { Add the new element to the LList }
  FLastCollection.Add(AValue);

  { Increase the version }
  Inc(FKnownCount);
  NotifyCollectionChanged();
end;

procedure TAbstractMultiMap<TKey, TValue>.Clear;
begin
  { Simply clear out the dictionary }
  if Assigned(FDictionary) then
    FDictionary.Clear();

  { Increase the version }
  FKnownCount := 0;
  FLastKey := default(TKey);
  FLastCollection := nil;

  NotifyCollectionChanged();
end;

function TAbstractMultiMap<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  { Delegate to the dictionary object }
  Result := FDictionary.ContainsKey(AKey);
end;

function TAbstractMultiMap<TKey, TValue>.ContainsPAir(const AKey: TKey; const AValue: TValue): Boolean;
var
  LList: ICollection<TValue>;
begin
  { Try to find .. otherwise fail! }
  if FDictionary.TryGetValue(AKey, LList) then
    Result := LList.Contains(AValue)
  else
    Result := false;
end;

function TAbstractMultiMap<TKey, TValue>.ContainsPair(const APair: TPair<TKey, TValue>): Boolean;
begin
  { Call upper function }
  Result := ContainsPair(APair.Key, APair.Value);
end;

function TAbstractMultiMap<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
var
  LList: ICollection<TValue>;
begin
  { Iterate over the dictionary }
  for LList in FDictionary.Values do
  begin
    { Is there anything there? }
    if LList.Contains(AValue) then
      Exit(true);
  end;

  { Nothing found }
  Result := false;
end;

procedure TAbstractMultiMap<TKey, TValue>.CopyTo(var AArray: array of TPair<TKey, TValue>; const AStartIndex: NativeInt);
var
  LKey: TKey;
  LArray: TArray<TValue>;
  LList: ICollection<TValue>;
  X, I, LCount: NativeInt;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;

  { Iterate over all lists and copy thtm to array }
  for LKey in FDictionary.Keys do
  begin
    LList := FDictionary[LKey];
    LCount := LList.Count;
    SetLength(LArray, LCount);
    LList.CopyTo(LArray, 0);

    for I := 0 to LCount - 1 do
    begin
      AArray[X + I].Key := LKey;
      AArray[X + I].Value := LArray[I];
    end;

    Inc(X, LCount);
  end;
end;

constructor TAbstractMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>);
begin
  { Install the types }
  inherited Create(AKeyRules, AValueRules);

  { Create the dictionary }
  FDictionary := CreateDictionary(KeyRules);

  FKeyCollection := FDictionary.Keys;
  FValueCollection := TValueSequence.Create(Self);

  { Create an internal empty list }
  FEmpty := CreateCollection(ValueRules);
end;

function TAbstractMultiMap<TKey, TValue>.ExtractValues(const AKey: TKey): ISequence<TValue>;
var
  LList: ICollection<TValue>;
  LNewList: TLinkedList<TValue>;
begin
  if FDictionary.TryGetValue(AKey, LList) then
    Dec(FKnownCount, LList.Count)
  else
    ExceptionHelper.Throw_KeyNotFoundError('AKey');

  { Simply remove the element. The LList should be auto-magically collected also }
  FDictionary.Remove(AKey);

  { Create the out list }
  LNewList := TLinkedList<TValue>.Create();
  LNewList.AddAll(LList);

  { Hackishly push out all elements from this list }
  //TODO: fix me please. This must clear stuff properly.
//  while not LList.Empty do
//    LList.ExtractAt(LList.Count - 1);

  { Assign output }
  Result := LNewList;
  NotifyCollectionChanged();
end;

function TAbstractMultiMap<TKey, TValue>.GetCount: NativeInt;
begin
  Result := FKnownCount;
end;

function TAbstractMultiMap<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  Result := TEnumerator.Create(Self);
end;

function TAbstractMultiMap<TKey, TValue>.GetValues(const AKey: TKey): ISequence<TValue>;
var
  LList: ICollection<TValue>;
begin
  if not FDictionary.TryGetValue(AKey, LList) then
    ExceptionHelper.Throw_KeyNotFoundError('AKey');

  Result := LList;
end;

function TAbstractMultiMap<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
begin
  Result := ContainsPair(AKey, AValue);
end;

procedure TAbstractMultiMap<TKey, TValue>.RemovePair(const AKey: TKey; const AValue: TValue);
var
  LList: ICollection<TValue>;
begin
  { Simply remove the value from the LList at key }
  if FDictionary.TryGetValue(AKey, LList) then
  begin
    if LList.Contains(AValue) then
    begin
      LList.Remove(AValue);

      { Kill the LList for one element }
      if LList.Count = 0 then
        FDictionary.Remove(AKey);

      Dec(FKnownCount, 1);

      { Increase the version }
      NotifyCollectionChanged();
    end;
  end;
end;

procedure TAbstractMultiMap<TKey, TValue>.RemovePair(const APair: TPair<TKey, TValue>);
begin
  { Call upper function }
  RemovePair(APair.Key, APair.Value);
end;

function TAbstractMultiMap<TKey, TValue>.SelectKeys: ISequence<TKey>;
begin
  Result := Keys;
end;

function TAbstractMultiMap<TKey, TValue>.SelectValues: ISequence<TValue>;
begin
  Result := Values;
end;

function TAbstractMultiMap<TKey, TValue>.TryGetValues(const AKey: TKey): ISequence<TValue>;
begin
  if not TryGetValues(AKey, Result) then
    Result := FEmpty;
end;

function TAbstractMultiMap<TKey, TValue>.TryGetValues(const AKey: TKey;
  out AValues: ISequence<TValue>): Boolean;
var
  LList: ICollection<TValue>;
begin
  { Use the internal stuff }
  Result := FDictionary.TryGetValue(AKey, LList);

  if Result then
    AValues := LList;
end;

function TAbstractMultiMap<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
begin
  Result := GetValues(AKey).ElementAt(0);
end;

procedure TAbstractMultiMap<TKey, TValue>.Remove(const AKey: TKey);
var
  LList: ICollection<TValue>;
begin
  if FDictionary.TryGetValue(AKey, LList) then
    Dec(FKnownCount, LList.Count);

  { Simply remove the element. The LList should be auto-magically collected also }
  FDictionary.Remove(AKey);

  { Increase the version }
  NotifyCollectionChanged();
end;

{ TAbstractMultiMap<TKey, TValue>.TEnumerator }

constructor TAbstractMultiMap<TKey, TValue>.TEnumerator.Create(const AOwner: TAbstractMultiMap<TKey, TValue>);
begin
  inherited Create(AOwner);
  FDictionaryEnumerator := AOwner.FDictionary.GetEnumerator();
end;

function TAbstractMultiMap<TKey, TValue>.TEnumerator.TryMoveNext(out ACurrent: TPair<TKey, TValue>): Boolean;
begin
  { Repeat until something happens }
  while True do
  begin
    if Assigned(FCollectionEnumerator) and FCollectionEnumerator.MoveNext() then
    begin
      { Next element }
      ACurrent.Key := FDictionaryEnumerator.Current.Key;
      ACurrent.Value := FCollectionEnumerator.Current;

      Exit(True);
    end else
    begin
      Result := FDictionaryEnumerator.MoveNext();
      if not Result then
        Break;

      FCollectionEnumerator := FDictionaryEnumerator.Current.Value.GetEnumerator();
    end;
  end;
end;

{ TAbstractMultiMap<TKey, TValue>.TValueEnumerator }

constructor TAbstractMultiMap<TKey, TValue>.TValueEnumerator.Create(const AOwner: TAbstractMultiMap<TKey, TValue>);
begin
  inherited Create(AOwner);
  FOwnerEnumerator := AOwner.GetEnumerator();
end;

function TAbstractMultiMap<TKey, TValue>.TValueEnumerator.TryMoveNext(out ACurrent: TValue): Boolean;
begin
  Result := FOwnerEnumerator.MoveNext();
  if Result then
    ACurrent := FOwnerEnumerator.Current.Value;
end;

{ TAbstractMultiMap<TKey, TValue>.TValueSequence }

constructor TAbstractMultiMap<TKey, TValue>.TValueSequence.Create(const AOwner: TAbstractMultiMap<TKey, TValue>);
begin
  inherited Create(AOwner.ValueRules);
  FOwner := AOwner;
end;

function TAbstractMultiMap<TKey, TValue>.TValueSequence.Empty: Boolean;
begin
  Result := FOwner.Empty;
end;

function TAbstractMultiMap<TKey, TValue>.TValueSequence.GetCount: NativeInt;
begin
  Result := FOwner.Count;
end;

function TAbstractMultiMap<TKey, TValue>.TValueSequence.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(FOwner);
end;

procedure TAbstractMultiMap<TKey, TValue>.TValueSequence.CopyTo(var AArray: array of TValue; const AStartIndex: NativeInt);
var
  LList: ICollection<TValue>;
  X: NativeInt;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < FOwner.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;

  { Iterate over all lists and copy them to array }
  for LList in TAbstractMultiMap<TKey, TValue>(FOwner).FDictionary.Values do
  begin
    LList.CopyTo(AArray, X);
    Inc(X, LList.Count);
  end;
end;

{ TMultiMap<TKey, TValue> }

constructor TMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>; const AInitialCapacity: NativeInt);
begin
  FInitialCapacity := AInitialCapacity;
  inherited Create(AKeyRules, AValueRules);
end;

constructor TMultiMap<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, CDefaultSize);
end;

constructor TMultiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>);
begin
  Create(AKeyRules, AValueRules, CDefaultSize);
end;

function TMultiMap<TKey, TValue>.CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, ICollection<TValue>>;
var
  LNewCapacity: NativeInt;
  LDictionary: TDictionary<TKey, ICollection<TValue>>;
begin
  { Create a simple dictionary }
  if FInitialCapacity <= 0 then
    LNewCapacity := CDefaultSize
  else
    LNewCapacity := FInitialCapacity;

  LDictionary := TDictionary<TKey, ICollection<TValue>>.Create(AKeyRules, TRules<ICollection<TValue>>.Default, LNewCapacity);
  LDictionary.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LDictionary;
end;

function TMultiMap<TKey, TValue>.CreateCollection(const AValueRules: TRules<TValue>): ICollection<TValue>;
var
  LList: TList<TValue>;
begin
  { Create a simple list }
  LList := TList<TValue>.Create(AValueRules);
  LList.RemoveNotification := NotifyValueRemoved;

  Result := LList;
end;

{ TObjectMultiMap<TKey, TValue> }

procedure TObjectMultiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    PObject(@AKey)^.Free;
end;

procedure TObjectMultiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    PObject(@AValue)^.Free;
end;

{ TSortedMultiMap<TKey, TValue> }


constructor TSortedMultiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>; const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscendingSort := AAscending;
  inherited Create(AKeyRules, AValueRules);
end;

constructor TSortedMultiMap<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, True);
end;

constructor TSortedMultiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>);
begin
  Create(AKeyRules, AValueRules, True);
end;

function TSortedMultiMap<TKey, TValue>.CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, ICollection<TValue>>;
var
  LDictionary: TSortedDictionary<TKey, ICollection<TValue>>;
begin
  { Create a simple dictionary }
  LDictionary := TSortedDictionary<TKey, ICollection<TValue>>.Create(AKeyRules, TRules<ICollection<TValue>>.Default, FAscendingSort);
  LDictionary.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LDictionary;
end;

function TSortedMultiMap<TKey, TValue>.CreateCollection(const AValueRules: TRules<TValue>): ICollection<TValue>;
var
  LList: TList<TValue>;
begin
  { Create a simple list }
  LList := TList<TValue>.Create(AValueRules);
  LList.RemoveNotification := NotifyValueRemoved;

  Result := LList;
end;

function TSortedMultiMap<TKey, TValue>.MaxKey: TKey;
begin
  Result := Dictionary.MaxKey;
end;

function TSortedMultiMap<TKey, TValue>.MinKey: TKey;
begin
  Result := Dictionary.MinKey;
end;

{ TObjectSortedMultiMap<TKey, TValue> }

procedure TObjectSortedMultiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    PObject(@AKey)^.Free;
end;

procedure TObjectSortedMultiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    PObject(@AValue)^.Free;
end;

{ TDistinctMultiMap<TKey, TValue> }

constructor TDistinctMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>; const AInitialCapacity: NativeInt);
begin
  FInitialCapacity := AInitialCapacity;
  inherited Create(AKeyRules, AValueRules);
end;

constructor TDistinctMultiMap<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, CDefaultSize);
end;

constructor TDistinctMultiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>);
begin
  Create(AKeyRules, AValueRules, CDefaultSize);
end;

function TDistinctMultiMap<TKey, TValue>.CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, ICollection<TValue>>;
var
  LNewCapacity: NativeInt;
  LDictionary: TDictionary<TKey, ICollection<TValue>>;
begin
  { Create a simple dictionary }
  if FInitialCapacity <= 0 then
    LNewCapacity := CDefaultSize
  else
    LNewCapacity := FInitialCapacity;

  LDictionary := TDictionary<TKey, ICollection<TValue>>.Create(AKeyRules, TRules<ICollection<TValue>>.Default, LNewCapacity);
  LDictionary.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LDictionary;
end;

function TDistinctMultiMap<TKey, TValue>.CreateCollection(const AValueRules: TRules<TValue>): ICollection<TValue>;
var
  LSet: THashSet<TValue>;
begin
  { Create a simple list }
  LSet := THashSet<TValue>.Create(AValueRules);
  LSet.RemoveNotification := NotifyValueRemoved;

  Result := LSet;
end;

{ TObjectDistinctMultiMap<TKey, TValue> }

procedure TObjectDistinctMultiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    PObject(@AKey)^.Free;
end;

procedure TObjectDistinctMultiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    PObject(@AValue)^.Free;
end;

{ TSortedDistinctMultiMap<TKey, TValue> }

constructor TSortedDistinctMultiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>; const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscendingSort := AAscending;
  inherited Create(AKeyRules, AValueRules);
end;

constructor TSortedDistinctMultiMap<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, True);
end;

constructor TSortedDistinctMultiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>);
begin
  Create(AKeyRules, AValueRules, True);
end;

function TSortedDistinctMultiMap<TKey, TValue>.CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, ICollection<TValue>>;
var
  LDictionary: TSortedDictionary<TKey, ICollection<TValue>>;
begin
  { Create a simple dictionary }
  LDictionary := TSortedDictionary<TKey, ICollection<TValue>>.Create(AKeyRules,
    TRules<ICollection<TValue>>.Default, FAscendingSort);

  LDictionary.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LDictionary;
end;

function TSortedDistinctMultiMap<TKey, TValue>.CreateCollection(const AValueRules: TRules<TValue>): ICollection<TValue>;
var
  LSet: THashSet<TValue>;
begin
  { Create a simple list }
  LSet := THashSet<TValue>.Create(AValueRules);
  LSet.RemoveNotification := NotifyValueRemoved;

  Result := LSet;
end;

function TSortedDistinctMultiMap<TKey, TValue>.MaxKey: TKey;
begin
  Result := Dictionary.MaxKey;
end;

function TSortedDistinctMultiMap<TKey, TValue>.MinKey: TKey;
begin
  Result := Dictionary.MinKey;
end;

{ TObjectSortedDistinctMultiMap<TKey, TValue> }

procedure TObjectSortedDistinctMultiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    PObject(@AKey)^.Free;
end;

procedure TObjectSortedDistinctMultiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    PObject(@AValue)^.Free;
end;

{ TDoubleSortedMultiMap<TKey, TValue> }

constructor TDoubleSortedMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const AAscendingKeys, AAscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscendingValues := AAscendingValues;
  inherited Create(AKeyRules, AValueRules, AAscendingKeys);
end;

constructor TDoubleSortedMultiMap<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, True, True);
end;

constructor TDoubleSortedMultiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>);
begin
  Create(AKeyRules, AValueRules, True, True);
end;

function TDoubleSortedMultiMap<TKey, TValue>.CreateCollection(const AValueRules: TRules<TValue>): ICollection<TValue>;
var
  LList: TSortedList<TValue>;
begin
  { Create a simple list }
  LList := TSortedList<TValue>.Create(AValueRules, CDefaultSize, FAscendingValues);
  LList.RemoveNotification := NotifyValueRemoved;

  Result := LList;
end;

{ TObjectDoubleSortedMultiMap<TKey, TValue> }

procedure TObjectDoubleSortedMultiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    PObject(@AKey)^.Free;
end;

procedure TObjectDoubleSortedMultiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    PObject(@AValue)^.Free;
end;

{ TDoubleSortedDistinctMultiMap<TKey, TValue> }

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const AAscendingKeys, AAscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscendingValues := AAscendingValues;
  inherited Create(AKeyRules, AValueRules, AAscendingKeys);
end;

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, True, True);
end;

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>);
begin
  Create(AKeyRules, AValueRules, True, True);
end;

function TDoubleSortedDistinctMultiMap<TKey, TValue>.CreateCollection(const AValueRules: TRules<TValue>): ICollection<TValue>;
var
  LSet: TSortedSet<TValue>;
begin
  { Create a simple list }
  LSet := TSortedSet<TValue>.Create(AValueRules, FAscendingValues);
  LSet.RemoveNotification := NotifyValueRemoved;

  Result := LSet;
end;

{ TObjectDoubleSortedDistinctMultiMap<TKey, TValue> }

procedure TObjectDoubleSortedDistinctMultiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    PObject(@AKey)^.Free;
end;

procedure TObjectDoubleSortedDistinctMultiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    PObject(@AValue)^.Free;
end;

end.
