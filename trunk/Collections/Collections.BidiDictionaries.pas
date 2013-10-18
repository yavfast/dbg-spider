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

unit Collections.BidiDictionaries;
interface
uses SysUtils,
     Generics.Defaults,
     Generics.Collections,
     Collections.Base,
     Collections.Dictionaries;

type
  ///  <summary>The base abstract class for all <c>bidi-dictionary</c> collections.</summary>
  TAbstractBidiDictionary<TKey, TValue> = class abstract(TAbstractMap<TKey, TValue>, IDictionary<TKey, TValue>, IBidiDictionary<TKey, TValue>)
  private
    FByKeyDictionary: IDictionary<TKey, TValue>;
    FByValueDictionary: IDictionary<TValue, TKey>;

    { Got from the underlying collections }
    FValueCollection: ISequence<TValue>;
    FKeyCollection: ISequence<TKey>;

  protected
    function IDictionary<TKey, TValue>.Extract = ExtractValueForKey;
    function IDictionary<TKey, TValue>.TryGetValue = TryGetValueForKey;
    function IDictionary<TKey, TValue>.GetValue = GetValueForKey;
    procedure IDictionary<TKey, TValue>.SetValue = SetValueForKey;

    ///  <summary>Specifies the internal dictionary used as back-end to store key relations.</summary>
    ///  <returns>A map used as back-end.</summary>
    property ByKeyDictionary: IDictionary<TKey, TValue> read FByKeyDictionary;

    ///  <summary>Specifies the internal dictionary used as back-end to store value relations.</summary>
    ///  <returns>A map used as back-end.</summary>
    property ByValueDictionary: IDictionary<TValue, TKey> read FByValueDictionary;

    ///  <summary>Called when this bidirectional dictionary needs to initialize its internal key dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    function CreateKeyDictionary(const AKeyRules: TRules<TKey>;
      const AValueRules: TRules<TValue>): IDictionary<TKey, TValue>; virtual; abstract;

    ///  <summary>Called when this bidirectional dictionary needs to initialize its internal value dictionary.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    function CreateValueDictionary(const AValueRules: TRules<TValue>;
      const AKeyRules: TRules<TKey>): IDictionary<TValue, TKey>; virtual; abstract;

    ///  <summary>Returns the number of pairs in the bidi-dictionary.</summary>
    ///  <returns>A positive value specifying the total number of pairs in the bidi-dictionary.</returns>
    function GetCount(): NativeInt; override;

    ///  <summary>Returns the value associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated value.</param>
    ///  <returns>The associated value.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    function GetValueForKey(const AKey: TKey): TValue;

    ///  <summary>Sets the value for a given key.</summary>
    ///  <param name="AKey">The key for which to set the value.</param>
    ///  <param name="AValue">The value to set.</param>
    ///  <remarks>If the dictionary does not contain the key, this method acts like <c>Add</c>; otherwise the
    ///  value of the specified key is modified.</remarks>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The new value is already used by another key.</exception>
    procedure SetValueForKey(const AKey: TKey; const AValue: TValue);

    ///  <summary>Returns the key associated with a value.</summary>
    ///  <param name="AValue">The value for which to obtain the associated key.</param>
    ///  <returns>The associated key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The value is not found in the collection.</exception>
    function GetKeyForValue(const AValue: TValue): TKey;

    ///  <summary>Sets the key for a given value.</summary>
    ///  <param name="AValue">The value for which to set the key.</param>
    ///  <param name="AKey">The key to set.</param>
    ///  <remarks>If the dictionary does not contain the value, this method acts like <c>Add</c>; otherwise the
    ///  key of the specified value is modified.</remarks>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The new key is already used by another value.</exception>
    procedure SetKeyForValue(const AValue: TValue; const AKey: TKey);
  public
    ///  <summary>Creates a new <c>bi-directional dictionary</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Clears the contents of the dictionary.</summary>
    procedure Clear(); override;

    ///  <summary>Adds a key-value pair to the bidi-dictionary.</summary>
    ///  <param name="AKey">The key of the pair.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The dictionary already contains a pair with the given key or value.</exception>
    procedure Add(const AKey: TKey; const AValue: TValue); overload; override;

    ///  <summary>Extracts a value using a given key.</summary>
    ///  <param name="AKey">The key of the associated value.</param>
    ///  <returns>The value associated with the key.</returns>
    ///  <remarks>This function is identical to <c>RemoveKey</c> but will return the stored value. If there is no pair with the given key, an exception is raised.</remarks>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The <paramref name="AKey"/> is not part of the map.</exception>
    function ExtractValueForKey(const AKey: TKey): TValue;

    ///  <summary>Extracts a key using a given value.</summary>
    ///  <param name="AValue">The value of the associated key.</param>
    ///  <returns>The key associated with the value.</returns>
    ///  <remarks>This function is identical to <c>RemoveValue</c> but will return the stored key. If there is no pair with the given value, an exception is raised.</remarks>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The <paramref name="AValue"/> is not part of the map.</exception>
    function ExtractKeyForValue(const AValue: TValue): TKey;

    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key (and its associated value) to remove.</param>
    procedure RemoveValueForKey(const AKey: TKey);

    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key of the pair.</param>
    procedure Remove(const AKey: TKey); override;

    ///  <summary>Removes a key-value pair using a given value.</summary>
    ///  <param name="AValue">The value (and its associated key) to remove.</param>
    procedure RemoveKeyForValue(const AValue: TValue);

    ///  <summary>Removes a specific key-value combination.</summary>
    ///  <param name="AKey">The key to remove.</param>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>This method only removes a key-value combination if that combination actually exists in the bidi-dictionary.
    ///  If the key is associated with another value, nothing happens.</remarks>
    procedure RemovePair(const AKey: TKey; const AValue: TValue); overload;

    ///  <summary>Removes a specific key-value combination.</summary>
    ///  <param name="AKey">The key to remove.</param>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>This method only removes a key-value combination if that combination actually exists in the bidi-dictionary.
    ///  If the key is associated with another value, nothing happens.</remarks>
    procedure RemovePair(const APair: TPair<TKey, TValue>); overload;

    ///  <summary>Checks whether the dictionary contains a key-value pair identified by the given key.</summary>
    ///  <param name="AKey">The key to check for.</param>
    ///  <returns><c>True</c> if the dictionary contains a pair identified by the given key; <c>False</c> otherwise.</returns>
    function ContainsKey(const AKey: TKey): Boolean; override;

    ///  <summary>Checks whether the dictionary contains a key-value pair that contains a given value.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the dictionary contains a pair holding the given value; <c>False</c> otherwise.</returns>
    function ContainsValue(const AValue: TValue): Boolean; override;

    ///  <summary>Checks whether the dictionary contains the given key-value combination.</summary>
    ///  <param name="AKey">The key associated with the value.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <returns><c>True</c> if the dictionary contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const AKey: TKey; const AValue: TValue): Boolean; overload;

    ///  <summary>Checks whether the dictionary contains a given key-value combination.</summary>
    ///  <param name="APair">The key-value pair combination.</param>
    ///  <returns><c>True</c> if the dictionary contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const APair: TPair<TKey, TValue>): Boolean; overload;

    ///  <summary>Tries to obtain the value associated with a given key.</summary>
    ///  <param name="AKey">The key for which to try to retrieve the value.</param>
    ///  <param name="AFoundValue">The found value (if the result is <c>True</c>).</param>
    ///  <returns><c>True</c> if the dictionary contains a value for the given key; <c>False</c> otherwise.</returns>
    function TryGetValueForKey(const AKey: TKey; out AFoundValue: TValue): Boolean;

    ///  <summary>Tries to obtain the key associated with a given value.</summary>
    ///  <param name="AValue">The value for which to try to retrieve the key.</param>
    ///  <param name="AFoundKey">The found key (if the result is <c>True</c>).</param>
    ///  <returns><c>True</c> if the dictionary contains a key for the given value; <c>False</c> otherwise.</returns>
    function TryGetKeyForValue(const AValue: TValue; out AFoundKey: TKey): Boolean;

    ///  <summary>Returns the value associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated value.</param>
    ///  <returns>The associated value.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the bidi-dictionary.</exception>
    property ByKey[const AKey: TKey]: TValue read GetValueForKey write SetValueForKey;

    ///  <summary>Returns the key associated with a value.</summary>
    ///  <param name="AValue">The value for which to obtain the associated key.</param>
    ///  <returns>The associated value.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the bidi-dictionary.</exception>
    property ByValue[const AValue: TValue]: TKey read GetKeyForValue write SetKeyForValue;

    ///  <summary>Specifies the collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the bidi-dictionary.</returns>
    property Keys: ISequence<TKey> read FKeyCollection;

    ///  <summary>Specifies the collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the bidi-dictionary.</returns>
    property Values: ISequence<TValue> read FValueCollection;

    ///  <summary>Returns the number of pairs in the bidi-map.</summary>
    ///  <returns>A positive value specifying the total number of pairs in the bidi-dictionary.</returns>
    property Count: NativeInt read GetCount;

    ///  <summary>Returns a new enumerator object used to enumerate this bidi-dictionary.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the bidi-map.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<TPair<TKey, TValue>>; override;

    ///  <summary>Copies the values stored in the bidi-dictionary to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the bidi-dictionary.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the bidi-dictionary.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of TPair<TKey,TValue>; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to return the associated value.</param>
    ///  <returns>The value associated with the given key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">No such key in the bidi-dictionary.</exception>
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

    ///  <summary>Checks whether the dictionary is empty.</summary>
    ///  <returns><c>True</c> if the dictionary is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the collection is empty. It is optimized
    ///  in most collections to offer a fast response.</remarks>
    function Empty(): Boolean; override;

    ///  <summary>Returns the biggest key.</summary>
    ///  <returns>The biggest key stored in this bidirectional dictionary.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The dictionary is empty.</exception>
    function MaxKey(): TKey; override;

    ///  <summary>Returns the smallest key.</summary>
    ///  <returns>The smallest key stored in this bidirectional dictionary.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The dictionary is empty.</exception>
    function MinKey(): TKey; override;

    ///  <summary>Returns the biggest value.</summary>
    ///  <returns>The biggest value stored in this bidirectional dictionary.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The dictionary is empty.</exception>
    function MaxValue(): TValue; override;

    ///  <summary>Returns the smallest value.</summary>
    ///  <returns>The smallest value stored in this bidirectional dictionary.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The dictionary is empty.</exception>
    function MinValue(): TValue; override;
  end;

type
  ///  <summary>The generic <c>bidirectional dictionary</c> collection.</summary>
  ///  <remarks>This type uses two <c>hash-based dictionaries</c> to store its keys and values.</remarks>
  TBidiDictionary<TKey, TValue> = class(TAbstractBidiDictionary<TKey, TValue>)
  private
    FInitialCapacity: NativeInt;

  protected
    ///  <summary>Called when the dictionary needs to initialize the key sub-dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a hash-based dictionary used as the underlying back-end for the keys.</remarks>
    function CreateKeyDictionary(const AKeyRules: TRules<TKey>;
      const AValueRules: TRules<TValue>): IDictionary<TKey, TValue>; override;

    ///  <summary>Called when the dictionary needs to initialize the value sub-dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a hash-based dictionary used as the underlying back-end for the values.</remarks>
    function CreateValueDictionary(const AValueRules: TRules<TValue>;
      const AKeyRules: TRules<TKey>): IDictionary<TValue, TKey>; override;
  public
    ///  <summary>Creates a new <c>bi-directional dictionary</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>bi-directional dictionary</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new <c>bi-directional dictionary</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    ///  <param name="AInitialCapacity">The dictionary's initial capacity.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>; const AInitialCapacity: NativeInt); overload;
  end;

  ///  <summary>The generic <c>bidirectional dictionary</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses two <c>hash-based dictionaries</c> to store its keys and values.</remarks>
  TObjectBidiDictionary<TKey, TValue> = class(TBidiDictionary<TKey, TValue>)
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
    ///  <summary>Specifies whether this dictionary owns the keys.</summary>
    ///  <returns><c>True</c> if the dictionary owns the keys; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the dictionary controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this dictionary owns the values.</summary>
    ///  <returns><c>True</c> if the dictionary owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the dictionary controls the life-time of the stored values. 
    ///  The value of this property has effect only if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>bidirectional dictionary</c> collection.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> to store its keys and a <c>hash-based dictionary</c> for its values.</remarks>
  TSortedBidiDictionary<TKey, TValue> = class(TAbstractBidiDictionary<TKey, TValue>)
  private
    FAscending: Boolean;

  protected
    ///  <summary>Called when the dictionary needs to initialize the key sub-dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a sorted dictionary used as the underlying back-end for the keys.</remarks>
    function CreateKeyDictionary(const AKeyRules: TRules<TKey>;
      const AValueRules: TRules<TValue>): IDictionary<TKey, TValue>; override;

    ///  <summary>Called when the dictionary needs to initialize the value sub-dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a hash-based dictionary used as the underlying back-end for the values.</remarks>
    function CreateValueDictionary(const AValueRules: TRules<TValue>;
      const AKeyRules: TRules<TKey>): IDictionary<TValue, TKey>; override;
  public
    ///  <summary>Creates a new <c>bi-directional dictionary</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed. The keys are stored in ascending order.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>bi-directional dictionary</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    ///  <remarks>The keys are stored in ascending order.</remarks>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new <c>bi-directional dictionary</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    ///  <param name="AAscending">Pass in a value of <c>True</c> if the keys should be kept in ascending order.
    ///  Pass in <c>False</c> for descending order.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>; const AAscending: Boolean); overload;
  end;

  ///  <summary>The generic <c>bidirectional dictionary</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> to store its keys and a <c>hash-based dictionary</c> for its values.</remarks>
  TObjectSortedBidiDictionary<TKey, TValue> = class(TSortedBidiDictionary<TKey, TValue>)
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
    ///  <summary>Specifies whether this dictionary owns the keys.</summary>
    ///  <returns><c>True</c> if the dictionary owns the keys; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the dictionary controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this dictionary owns the values.</summary>
    ///  <returns><c>True</c> if the dictionary owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the dictionary controls the life-time of the stored values. 
    ///  The value of this property has effect only if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>bidirectional dictionary</c> collection.</summary>
  ///  <remarks>This type uses two <c>sorted dictionaries</c> to store its keys and values.</remarks>
  TDoubleSortedBidiDictionary<TKey, TValue> = class(TAbstractBidiDictionary<TKey, TValue>)
  private
    FAscendingKeys, FAscendingValues: Boolean;

  protected
    ///  <summary>Called when the dictionary needs to initialize the key sub-dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a sorted dictionary used as the underlying back-end for the keys.</remarks>
    function CreateKeyDictionary(const AKeyRules: TRules<TKey>;
      const AValueRules: TRules<TValue>): IDictionary<TKey, TValue>; override;

    ///  <summary>Called when the dictionary needs to initialize the value sub-dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a sorted dictionary used as the underlying back-end for the values.</remarks>
    function CreateValueDictionary(const AValueRules: TRules<TValue>;
      const AKeyRules: TRules<TKey>): IDictionary<TValue, TKey>; override;

  public
    ///  <summary>Creates a new <c>bi-directional dictionary</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed. The keys and values are stored in ascending order.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>bi-directional dictionary</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    ///  <remarks>The keys and values are stored in ascending order.</remarks>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new <c>bi-directional dictionary</c> collection.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    ///  <param name="AAscendingKeys">Pass in a value of <c>True</c> if the keys should be kept in ascending order.
    ///  Pass in <c>False</c> for descending order.</param>
    ///  <param name="AAscendingValues">Pass in a value of <c>True</c> if the values should be kept in ascending order.
    ///  Pass in <c>False</c> for descending order.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const AAscendingKeys: Boolean; const AAscendingValues: Boolean); overload;
  end;

  ///  <summary>The generic <c>bidirectional dictionary</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses two <c>hsorted dictionaries</c> to store its keys and values.</remarks>
  TObjectDoubleSortedBidiDictionary<TKey, TValue> = class(TDoubleSortedBidiDictionary<TKey, TValue>)
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
    ///  <summary>Specifies whether this dictionary owns the keys.</summary>
    ///  <returns><c>True</c> if the dictionary owns the keys; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the dictionary controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this dictionary owns the values.</summary>
    ///  <returns><c>True</c> if the dictionary owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the dictionary controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

implementation

{ TAbstractBidiDictionary<TKey, TValue> }

procedure TAbstractBidiDictionary<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
  if FByKeyDictionary.ContainsKey(AKey) then
    ExceptionHelper.Throw_DuplicateKeyError('AKey');

  if FByValueDictionary.ContainsKey(AValue) then
    ExceptionHelper.Throw_DuplicateKeyError('AValue');

  FByKeyDictionary.Add(AKey, AValue);
  FByValueDictionary.Add(AValue, AKey);
end;

procedure TAbstractBidiDictionary<TKey, TValue>.Clear;
begin
  if Assigned(FByKeyDictionary) then
    FByKeyDictionary.Clear();

  if Assigned(FByValueDictionary) then
    FByValueDictionary.Clear();
end;

function TAbstractBidiDictionary<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  { Use the value dictionary }
  Result := FByKeyDictionary.ContainsKey(AKey);
end;

function TAbstractBidiDictionary<TKey, TValue>.ContainsPair(const APair: TPair<TKey, TValue>): Boolean;
begin
  { Call the best method eva! }
  Result := ContainsPair(APair.Key, APair.Value);
end;

function TAbstractBidiDictionary<TKey, TValue>.ContainsPair(const AKey: TKey; const AValue: TValue): Boolean;
var
  LRealValue: TValue;
begin
  { Check that the key exists and that the associated value is the same one. }
  Result := FByKeyDictionary.TryGetValue(AKey, LRealValue) and ValuesAreEqual(AValue, LRealValue);
end;

function TAbstractBidiDictionary<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
begin
  { Use the value dictionary }
  Result := FByValueDictionary.ContainsKey(AValue);
end;

procedure TAbstractBidiDictionary<TKey, TValue>.CopyTo(var AArray: array of TPair<TKey, TValue>; const AStartIndex: NativeInt);
begin
  { Copy from the key dictionary }
  FByKeyDictionary.CopyTo(AArray, AStartIndex);
end;

constructor TAbstractBidiDictionary<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>);
begin
  { Install the types }
  inherited Create(AKeyRules, AValueRules);

  { Create the maps }
  FByKeyDictionary := CreateKeyDictionary(AKeyRules, ValueRules);
  FByValueDictionary := CreateValueDictionary(AValueRules, KeyRules);

  { The collections }
  FValueCollection := FByValueDictionary.Keys;
  FKeyCollection := FByKeyDictionary.Keys;
end;

function TAbstractBidiDictionary<TKey, TValue>.Empty: Boolean;
begin
  { Redirect }
  Result := FByKeyDictionary.Empty();
end;

function TAbstractBidiDictionary<TKey, TValue>.ExtractKeyForValue(const AValue: TValue): TKey;
begin
  if FByValueDictionary.TryGetValue(AValue, Result) then
  begin
    { Remove the key/value from their dictionaries }
    FByKeyDictionary.Remove(Result);
    FByValueDictionary.Remove(AValue);
  end else
    ExceptionHelper.Throw_KeyNotFoundError('AValue');
end;

function TAbstractBidiDictionary<TKey, TValue>.ExtractValueForKey(const AKey: TKey): TValue;
begin
  if FByKeyDictionary.TryGetValue(AKey, Result) then
  begin
    { Remove the key/value from their dictionaries }
    FByKeyDictionary.Remove(AKey);
    FByValueDictionary.Extract(Result);
  end else
    ExceptionHelper.Throw_KeyNotFoundError('AKey');
end;

function TAbstractBidiDictionary<TKey, TValue>.GetCount: NativeInt;
begin
  { Redirect }
  Result := FByKeyDictionary.Count;
end;

function TAbstractBidiDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  Result := FByKeyDictionary.GetEnumerator();
end;

function TAbstractBidiDictionary<TKey, TValue>.GetKeyForValue(const AValue: TValue): TKey;
begin
  { Use indexed property. }
  Result := FByValueDictionary[AValue];
end;

function TAbstractBidiDictionary<TKey, TValue>.GetValueForKey(const AKey: TKey): TValue;
begin
  { Use indexed property. }
  Result := FByKeyDictionary[AKey];
end;

function TAbstractBidiDictionary<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
begin
  { Call into the key dictionary }
  Result := FByKeyDictionary.KeyHasValue(AKey, AValue);
end;

function TAbstractBidiDictionary<TKey, TValue>.MaxKey: TKey;
begin
  Result := FByKeyDictionary.MaxKey;
end;

function TAbstractBidiDictionary<TKey, TValue>.MaxValue: TValue;
begin
  { Use the value dictionary for lookup by keys -- much faster }
  Result := FByValueDictionary.MaxKey;
end;

function TAbstractBidiDictionary<TKey, TValue>.MinKey: TKey;
begin
  Result := FByKeyDictionary.MinKey;
end;

function TAbstractBidiDictionary<TKey, TValue>.MinValue: TValue;
begin
  { Use the value dictionary for lookup by keys -- much faster }
  Result := FByValueDictionary.MinKey;
end;

procedure TAbstractBidiDictionary<TKey, TValue>.RemovePair(const AKey: TKey; const AValue: TValue);
var
  LAssociatedValue: TValue;
begin
  { Check if key -> value relationship actually exists }
  if FByKeyDictionary.TryGetValue(AKey, LAssociatedValue) and
     ValuesAreEqual(LAssociatedValue, AValue) then
  begin
    { Remove the key/value from their dictionaries }
    FByKeyDictionary.Remove(AKey);
    FByValueDictionary.Remove(AValue);
  end;
end;

procedure TAbstractBidiDictionary<TKey, TValue>.Remove(const AKey: TKey);
begin
  { Redirect ... }
  RemoveValueForKey(AKey);
end;

procedure TAbstractBidiDictionary<TKey, TValue>.RemovePair(const APair: TPair<TKey, TValue>);
begin
  { Redirect ... }
  RemovePair(APair.Key, APair.Value);
end;

procedure TAbstractBidiDictionary<TKey, TValue>.RemoveValueForKey(const AKey: TKey);
var
  LAssociatedValue: TValue;
begin
  if FByKeyDictionary.TryGetValue(AKey, LAssociatedValue) then
  begin
    { Remove the key/value from their dictionaries }
    FByKeyDictionary.Remove(AKey);
    FByValueDictionary.Remove(LAssociatedValue);

    NotifyValueRemoved(LAssociatedValue);
  end;
end;

procedure TAbstractBidiDictionary<TKey, TValue>.RemoveKeyForValue(const AValue: TValue);
var
  LAssociatedKey: TKey;
begin
  if FByValueDictionary.TryGetValue(AValue, LAssociatedKey) then
  begin
    { Remove the key/value from their dictionaries }
    FByKeyDictionary.Remove(LAssociatedKey);
    FByValueDictionary.Remove(AValue);

    NotifyKeyRemoved(LAssociatedKey);
  end;
end;

function TAbstractBidiDictionary<TKey, TValue>.SelectKeys: ISequence<TKey>;
begin
  Result := FKeyCollection;
end;

function TAbstractBidiDictionary<TKey, TValue>.SelectValues: ISequence<TValue>;
begin
  Result := FValueCollection;
end;

procedure TAbstractBidiDictionary<TKey, TValue>.SetKeyForValue(const AValue: TValue; const AKey: TKey);
var
  LOldKey: TKey;
begin
  { AKey cannot be in the dictionary! }
  if FByKeyDictionary.ContainsKey(AKey) then
    ExceptionHelper.Throw_DuplicateKeyError('AKey');

  { Replace or add }
  if FByValueDictionary.TryGetValue(AValue, LOldKey) then
    FByKeyDictionary.Remove(LOldKey);

  { Register the new Key --> Value relation }
  FByKeyDictionary.Add(AKey, AValue);

  { Update the old Value --> Key relation }
  FByValueDictionary[AValue] := AKey;
end;

procedure TAbstractBidiDictionary<TKey, TValue>.SetValueForKey(const AKey: TKey; const AValue: TValue);
var
  LOldValue: TValue;
begin
  { AKey cannot be in the dictionary! }
  if FByValueDictionary.ContainsKey(AValue) then
    ExceptionHelper.Throw_DuplicateKeyError('AValue');

  { Replace or add }
  if FByKeyDictionary.TryGetValue(AKey, LOldValue) then
    FByValueDictionary.Remove(LOldValue);

  { Register the new Value --> Key relation }
  FByValueDictionary.Add(AValue, AKey);

  { Update the old Key --> Value relation }
  FByKeyDictionary[AKey] := AValue;
end;

function TAbstractBidiDictionary<TKey, TValue>.TryGetKeyForValue(const AValue: TValue; out AFoundKey: TKey): Boolean;
begin
  { Act as a bridge }
  Result := FByValueDictionary.TryGetValue(AValue, AFoundKey);
end;

function TAbstractBidiDictionary<TKey, TValue>.TryGetValueForKey(const AKey: TKey; out AFoundValue: TValue): Boolean;
begin
  { Act as a bridge }
  Result := FByKeyDictionary.TryGetValue(AKey, AFoundValue);
end;

function TAbstractBidiDictionary<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
begin
  { Act as a bridge }
  Result := FByKeyDictionary.ValueForKey(AKey);
end;

{ TBidiDictionary<TKey, TValue> }

constructor TBidiDictionary<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>; const AInitialCapacity: NativeInt);
begin
  FInitialCapacity := AInitialCapacity;
  inherited Create(AKeyRules, AValueRules);
end;

constructor TBidiDictionary<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, CDefaultSize);
end;

constructor TBidiDictionary<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>);
begin
  Create(AKeyRules, AValueRules, CDefaultSize);
end;

function TBidiDictionary<TKey, TValue>.CreateKeyDictionary(
  const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>): IDictionary<TKey, TValue>;
var
  LDictionary: TDictionary<TKey, TValue>;
begin
  { Use a double sorted map }
  LDictionary := TDictionary<TKey, TValue>.Create(AKeyRules, AValueRules, FInitialCapacity);
  LDictionary.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LDictionary;
end;

function TBidiDictionary<TKey, TValue>.CreateValueDictionary(
  const AValueRules: TRules<TValue>;
  const AKeyRules: TRules<TKey>): IDictionary<TValue, TKey>;
var
  LDictionary: TDictionary<TValue, TKey>;
begin
  { Use a double sorted map }
  LDictionary := TDictionary<TValue, TKey>.Create(AValueRules, AKeyRules, FInitialCapacity);
  LDictionary.KeyRemoveNotification := NotifyValueRemoved;

  Result := LDictionary;
end;

{ TObjectBidiDictionary<TKey, TValue> }

procedure TObjectBidiDictionary<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    PObject(@AKey)^.Free;
end;

procedure TObjectBidiDictionary<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    PObject(@AValue)^.Free;
end;

{ TSortedBidiDictionary<TKey, TValue> }

function TSortedBidiDictionary<TKey, TValue>.CreateKeyDictionary(
  const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>): IDictionary<TKey, TValue>;
var
  LDictionary: TSortedDictionary<TKey, TValue>;
begin
  { Use a double sorted map }
  LDictionary := TSortedDictionary<TKey, TValue>.Create(AKeyRules, AValueRules, FAscending);
  LDictionary.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LDictionary;
end;

function TSortedBidiDictionary<TKey, TValue>.CreateValueDictionary(
  const AValueRules: TRules<TValue>;
  const AKeyRules: TRules<TKey>): IDictionary<TValue, TKey>;
var
  LDictionary: TDictionary<TValue, TKey>;
begin
  { Use a double sorted map }
  LDictionary := TDictionary<TValue, TKey>.Create(AValueRules, AKeyRules);
  LDictionary.KeyRemoveNotification := NotifyValueRemoved;

  Result := LDictionary;
end;

constructor TSortedBidiDictionary<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const AAscending: Boolean);
begin
  FAscending := AAscending;
  inherited Create(AKeyRules, AValueRules);
end;

constructor TSortedBidiDictionary<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, True);
end;

constructor TSortedBidiDictionary<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>);
begin
  Create(AKeyRules, AValueRules, True);
end;

{ TObjectSortedBidiDictionary<TKey, TValue> }

procedure TObjectSortedBidiDictionary<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    PObject(@AKey)^.Free;
end;

procedure TObjectSortedBidiDictionary<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    PObject(@AValue)^.Free;
end;

{ TDoubleSortedBidiDictionary<TKey, TValue> }

function TDoubleSortedBidiDictionary<TKey, TValue>.CreateKeyDictionary(
  const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>): IDictionary<TKey, TValue>;
var
  LDictionary: TSortedDictionary<TKey, TValue>;
begin
  { Use a double sorted map }
  LDictionary := TSortedDictionary<TKey, TValue>.Create(AKeyRules, AValueRules, FAscendingKeys);
  LDictionary.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LDictionary;
end;

function TDoubleSortedBidiDictionary<TKey, TValue>.CreateValueDictionary(
  const AValueRules: TRules<TValue>;
  const AKeyRules: TRules<TKey>): IDictionary<TValue, TKey>;
var
  LDictionary: TSortedDictionary<TValue, TKey>;
begin
  { Use a double sorted map }
  LDictionary := TSortedDictionary<TValue, TKey>.Create(AValueRules, AKeyRules, FAscendingValues);
  LDictionary.KeyRemoveNotification := NotifyValueRemoved;

  Result := LDictionary;
end;

constructor TDoubleSortedBidiDictionary<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const AAscendingKeys, AAscendingValues: Boolean);
begin
  FAscendingKeys := AAscendingKeys;
  FAscendingValues := AAscendingValues;

  inherited Create(AKeyRules, AValueRules);
end;

constructor TDoubleSortedBidiDictionary<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, True, True);
end;

constructor TDoubleSortedBidiDictionary<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>);
begin
  Create(AKeyRules, AValueRules, True, True);
end;

{ TObjectDoubleSortedBidiDictionary<TKey, TValue> }

procedure TObjectDoubleSortedBidiDictionary<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    PObject(@AKey)^.Free;
end;

procedure TObjectDoubleSortedBidiDictionary<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    PObject(@AValue)^.Free;
end;

end.
