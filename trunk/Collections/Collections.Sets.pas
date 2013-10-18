(*
* Copyright (c) 2008-2012, Ciobanu Alexandru
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

unit Collections.Sets;
interface
uses SysUtils,
     Generics.Defaults,
     Collections.Base;

type
  ///  <summary>The asbstract base for all generic <c>set</c> collections.</summary>
  ///  <remarks>Descending classes must implement the required abstract methods and optionally can implement
  ///  the non-required method.</remarks>
  TAbstractSet<T> = class abstract(TCollection<T>, ISet<T>)
  public
    ///  <summary>Creates a new <c>set</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the set.</param>
    constructor Create(const ARules: TRules<T>);

    ///  <summary>Destroys this instance.</summary>
    ///  <remarks>Do not call this method directly; call <c>Free</c> instead.</remarks>
    destructor Destroy(); override;
  end;

type
  ///  <summary>The generic <c>set</c> collection.</summary>
  ///  <remarks>This type uses hashing techniques to store its values.</remarks>
  THashSet<T> = class(TAbstractSet<T>)
  private type
    {$REGION 'Internal Types'}
    TEnumerator = class(TAbstractEnumerator<T>)
    private
      FCurrentIndex: NativeInt;
    public
      function TryMoveNext(out ACurrent: T): Boolean; override;
    end;

    TEntry = record
      FHashCode: NativeInt;
      FNext: NativeInt;
      FKey: T;
    end;

    TBucketArray = array of NativeInt;
    {$ENDREGION}

  private var
    FBucketArray: TBucketArray;
    FEntryArray: TArray<TEntry>;
    FCount: NativeInt;
    FFreeCount: NativeInt;
    FFreeList: NativeInt;

    { Internal }
    procedure InitializeInternals(const ACapacity: NativeInt);
    procedure Insert(const AKey: T; const ShouldAdd: Boolean = true);
    function FindEntry(const AKey: T): NativeInt;
    procedure Resize();
    function Hash(const AKey: T): NativeInt;

  protected
    ///  <summary>Returns the number of elements in the set.</summary>
    ///  <returns>A positive value specifying the number of elements in the set.</returns>
    function GetCount(): NativeInt; override;
  public
    ///  <summary>Creates a new <c>set</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>set</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the set.</param>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Creates a new <c>set</c> collection.</summary>
    ///  <param name="AInitialCapacity">The set's initial capacity.</param>
    ///  <param name="ARules">A rule set describing the elements in the set.</param>
    constructor Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt); overload;

    ///  <summary>Clears the contents of the set.</summary>
    procedure Clear(); override;

    ///  <summary>Adds an element to the set.</summary>
    ///  <param name="AValue">The value to add.</param>
    ///  <remarks>If the set already contains the given value, nothing happens.</remarks>
    procedure Add(const AValue: T); override;

    ///  <summary>Removes a given value from the set.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>If the set does not contain the given value, nothing happens.</remarks>
    procedure Remove(const AValue: T); override;

    ///  <summary>Checks whether the set contains a given value.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns><c>True</c> if the value was found in the set; <c>False</c> otherwise.</returns>
    function Contains(const AValue: T): Boolean; override;

    ///  <summary>Specifies the number of elements in the set.</summary>
    ///  <returns>A positive value specifying the number of elements in the set.</returns>
    property Count: NativeInt read GetCount;

    ///  <summary>Returns a new enumerator object used to enumerate this set.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the set.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator() : IEnumerator<T>; override;

    ///  <summary>Copies the values stored in the set to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the set.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the set.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Checks whether the set is empty.</summary>
    ///  <returns><c>True</c> if the set is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the set is empty.</remarks>
    function Empty(): Boolean; override;
  end;

  ///  <summary>The generic <c>set</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses hashing techniques to store its objects.</remarks>
  TObjectHashSet<T: class> = class(THashSet<T>)
  private
    FOwnsObjects: Boolean;

  protected
    ///  <summary>Frees the object that was removed from the collection.</summary>
    ///  <param name="AElement">The object that was removed from the collection.</param>
    procedure HandleElementRemoved(const AElement: T); override;
  public
    ///  <summary>Specifies whether this set owns the objects stored in it.</summary>
    ///  <returns><c>True</c> if the set owns its objects; <c>False</c> otherwise.</returns>
    ///  <remarks>This property controls the way the set controls the life-time of the stored objects.</remarks>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

type
  ///  <summary>The generic <c>set</c> collection.</summary>
  ///  <remarks>This type uses hashing techniques and linked lists to store its values.</remarks>
  TLinkedSet<T> = class(TAbstractSet<T>)
  private type
    {$REGION 'Internal Types'}
    PEntry = ^TEntry;
    TEntry = record
      FHashCode: NativeInt;
      FNext, FPrev: PEntry;
      FValue: T;
    end;

    TBucketArray = TArray<PEntry>;

    TEnumerator = class(TAbstractEnumerator<T>)
    private
      FCurrentEntry: PEntry;
    public
      function TryMoveNext(out ACurrent: T): Boolean; override;
    end;
    {$ENDREGION}

  private var
    FBucketArray: TBucketArray;
    FCount, FFreeCount: NativeInt;
    FHead, FTail, FFirstFree: PEntry;

    { Internal }
    procedure InitializeInternals(const ACapacity: NativeInt);
    procedure Insert(const AValue: T; const AShouldAdd: Boolean = true);
    procedure ReInsert(const AEntry: PEntry; const ACapacity: NativeInt);

    function FindEntry(const AValue: T): PEntry;
    function Hash(const AValue: T): NativeInt;

    { Caching }
    function NeedEntry(const AValue: T; const AHash: NativeInt): PEntry;
    procedure ReleaseEntry(const AEntry: PEntry);
  protected
    ///  <summary>Returns the number of elements in the set.</summary>
    ///  <returns>A positive value specifying the number of elements in the set.</returns>
    function GetCount(): NativeInt; override;

  public
    ///  <summary>Creates a new <c>set</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>set</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the set.</param>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Creates a new <c>set</c> collection.</summary>
    ///  <param name="AInitialCapacity">The set's initial capacity.</param>
    ///  <param name="ARules">A rule set describing the elements in the set.</param>
    constructor Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt); overload;

    ///  <summary>Destroys this instance.</summary>
    ///  <remarks>Do not call this method directly; call <c>Free</c> instead.</remarks>
    destructor Destroy(); override;

    ///  <summary>Clears the contents of the set.</summary>
    procedure Clear(); override;

    ///  <summary>Adds an element to the set.</summary>
    ///  <param name="AValue">The value to add.</param>
    ///  <remarks>If the set already contains the given value, nothing happens.</remarks>
    procedure Add(const AValue: T); override;

    ///  <summary>Removes a given value from the set.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>If the set does not contain the given value, nothing happens.</remarks>
    procedure Remove(const AValue: T); override;

    ///  <summary>Checks whether the set contains a given value.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns><c>True</c> if the value was found in the set; <c>False</c> otherwise.</returns>
    function Contains(const AValue: T): Boolean; override;

    ///  <summary>Specifies the number of elements in the set.</summary>
    ///  <returns>A positive value specifying the number of elements in the set.</returns>
    property Count: NativeInt read FCount;

    ///  <summary>Returns a new enumerator object used to enumerate this set.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the set.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator() : IEnumerator<T>; override;

    ///  <summary>Copies the values stored in the set to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the set.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the set.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Checks whether the set is empty.</summary>
    ///  <returns><c>True</c> if the set is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the set is empty.</remarks>
    function Empty(): Boolean; override;
  end;

  ///  <summary>The generic <c>set</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses hashing techniques and linked lists to store its objects.</remarks>
  TObjectLinkedSet<T: class> = class(TLinkedSet<T>)
  private
    FOwnsObjects: Boolean;

  protected
    ///  <summary>Frees the object that was removed from the collection.</summary>
    ///  <param name="AElement">The object that was removed from the collection.</param>
    procedure HandleElementRemoved(const AElement: T); override;
  public
    ///  <summary>Specifies whether this set owns the objects stored in it.</summary>
    ///  <returns><c>True</c> if the set owns its objects; <c>False</c> otherwise.</returns>
    ///  <remarks>This property controls the way the set controls the life-time of the stored objects.</remarks>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

type
  ///  <summary>The generic <c>set</c> collection.</summary>
  ///  <remarks>This type uses an AVL tree to store its values.</remarks>
  TSortedSet<T> = class(TAbstractSet<T>, ISortedSet<T>)
  private type
    {$REGION 'Internal Types'}
    //TBalanceAct = (baStart = 0, baLeft, baRight, baLoop, baEnd);

    { An internal node class }
    TNode = class
    private
      FKey: T;

      FParent,
       FLeft, FRight: TNode;

      FBalance: ShortInt;
    end;

    TEnumerator = class(TAbstractEnumerator<T>)
    private
      FCurrentEntry: TNode;
    public
      function TryMoveNext(out ACurrent: T): Boolean; override;
    end;
    {$ENDREGION}

  private var
    FCount: NativeInt;
    FRoot: TNode;
    FSignFix: NativeInt;

    function FindNodeWithKey(const AValue: T): TNode;
    function FindLeftMostNode(): TNode;
    function FindRightMostNode(): TNode;
    function WalkToTheRight(const ANode: TNode): TNode;
    function MakeNode(const AValue: T; const ARoot: TNode): TNode;
    procedure RecursiveClear(const ANode: TNode);
    procedure ReBalanceSubTreeOnInsert(const ANode: TNode);
    procedure Insert(const AValue: T);
    procedure BalanceTreesAfterRemoval(const ANode: TNode);
  protected
    ///  <summary>Returns the number of elements in the set.</summary>
    ///  <returns>A positive value specifying the number of elements in the set.</returns>
    function GetCount(): NativeInt; override;
  public
    ///  <summary>Creates a new <c>set</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed. The elements are stored in ascending order.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>set</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the set.</param>
    ///  <remarks>The elements are stored in ascending order.</remarks>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Creates a new <c>set</c> collection.</summary>
    ///  <param name="AAscending">Pass in a value of <c>True</c> if the elements should be kept in ascending order. Pass in <c>False</c> for descending order.</param>
    ///  <param name="ARules">A rule set describing the elements in the set.</param>
    constructor Create(const ARules: TRules<T>; const AAscending: Boolean); overload;

    ///  <summary>Clears the contents of the set.</summary>
    procedure Clear(); override;

    ///  <summary>Adds an element to the set.</summary>
    ///  <param name="AValue">The value to add.</param>
    ///  <remarks>If the set already contains the given value, nothing happens.</remarks>
    procedure Add(const AValue: T); override;

    ///  <summary>Removes a given value from the set.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>If the set does not contain the given value, nothing happens.</remarks>
    procedure Remove(const AValue: T); override;

    ///  <summary>Checks whether the set contains a given value.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns><c>True</c> if the value was found in the set; <c>False</c> otherwise.</returns>
    function Contains(const AValue: T): Boolean; override;

    ///  <summary>Specifies the number of elements in the set.</summary>
    ///  <returns>A positive value specifying the number of elements in the set.</returns>
    property Count: NativeInt read FCount;

    ///  <summary>Returns a new enumerator object used to enumerate this set.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the set.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator() : IEnumerator<T>; override;

    ///  <summary>Copies the values stored in the set to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the set.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the set.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Checks whether the set is empty.</summary>
    ///  <returns><c>True</c> if the set is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the set is empty.</remarks>
    function Empty(): Boolean; override;

    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the set considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function Max(): T; override;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the set considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function Min(): T; override;

    ///  <summary>Returns the first element.</summary>
    ///  <returns>The first element in the set.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function First(): T; override;

    ///  <summary>Returns the first element or a default, if the set is empty.</summary>
    ///  <param name="ADefault">The default value returned if the set is empty.</param>
    ///  <returns>The first element in the set if the set is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function FirstOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the last element.</summary>
    ///  <returns>The last element in the set.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function Last(): T; override;

    ///  <summary>Returns the last element or a default, if the set is empty.</summary>
    ///  <param name="ADefault">The default value returned if the set is empty.</param>
    ///  <returns>The last element in the set if the set is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function LastOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the single element stored in the set.</summary>
    ///  <returns>The element in set.</returns>
    ///  <remarks>This method checks if the set contains just one element, in which case it is returned.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionNotOneException">There is more than one element in the set.</exception>
    function Single(): T; override;

    ///  <summary>Returns the single element stored in the set, or a default value.</summary>
    ///  <param name="ADefault">The default value returned if there are less or more elements in the set.</param>
    ///  <returns>The element in the set if the condition is satisfied; <paramref name="ADefault"/> is returned otherwise.</returns>
    ///  <remarks>This method checks if the set contains just one element, in which case it is returned. Otherwise
    ///  the value in <paramref name="ADefault"/> is returned.</remarks>
    function SingleOrDefault(const ADefault: T): T; override;
  end;

  ///  <summary>The generic <c>set</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses an AVL tree to store its objects.</remarks>
  TObjectSortedSet<T: class> = class(TSortedSet<T>)
  private
    FOwnsObjects: Boolean;

  protected
    ///  <summary>Frees the object that was removed from the collection.</summary>
    ///  <param name="AElement">The object that was removed from the collection.</param>
    procedure HandleElementRemoved(const AElement: T); override;

  public
    ///  <summary>Specifies whether this set owns the objects stored in it.</summary>
    ///  <returns><c>True</c> if the set owns its objects; <c>False</c> otherwise.</returns>
    ///  <remarks>This property controls the way the set controls the life-time of the stored objects.</remarks>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

type
  ///  <summary>The generic <c>set</c> collection.</summary>
  ///  <remarks>This type uses an internal array to store its values.</remarks>
  TArraySet<T> = class(TAbstractSet<T>, ISortedSet<T>, IDynamic)
  private type
    {$REGION 'Internal Types'}
    TEnumerator = class(TAbstractEnumerator<T>)
    private
      FCurrentIndex: NativeInt;
    public
      function TryMoveNext(out ACurrent: T): Boolean; override;
    end;
    {$ENDREGION}

  private var
    FArray: TArray<T>;
    FCount: NativeInt;
    FSignFix: NativeInt;

    { Inserts an element into a position }
    function BinarySearch(const AElement: T): NativeInt;
  protected
    ///  <summary>Returns the number of elements in the set.</summary>
    ///  <returns>A positive value specifying the number of elements in the set.</returns>
    function GetCount(): NativeInt; override;

    ///  <summary>Returns the current capacity.</summary>
    ///  <returns>A positive number that specifies the number of elements that the set can hold before it
    ///  needs to grow again.</returns>
    ///  <remarks>The value of this method is greater than or equal to the amount of elements in the set. If this value
    ///  is greater than the number of elements, it means that the set has some extra capacity to operate upon.</remarks>
    function GetCapacity(): NativeInt;
  public
    ///  <summary>Creates a new <c>set</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed. The elements are stored in ascending order.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>set</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the set.</param>
    ///  <remarks>The elements are stored in ascending order.</remarks>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Creates a new <c>set</c> collection.</summary>
    ///  <param name="AAscending">Pass in a value of <c>True</c> if the elements should be kept in ascending order. Pass in <c>False</c> for descending order.</param>
    ///  <param name="AInitialCapacity">The set's initial capacity.</param>
    ///  <param name="ARules">A rule set describing the elements in the set.</param>
    constructor Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt; const AAscending: Boolean); overload;

    ///  <summary>Clears the contents of the set.</summary>
    procedure Clear(); override;

    ///  <summary>Adds an element to the set.</summary>
    ///  <param name="AValue">The value to add.</param>
    ///  <remarks>If the set already contains the given value, nothing happens.</remarks>
    procedure Add(const AValue: T); override;

    ///  <summary>Removes a given value from the set.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>If the set does not contain the given value, nothing happens.</remarks>
    procedure Remove(const AValue: T); override;

    ///  <summary>Checks whether the set contains a given value.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns><c>True</c> if the value was found in the set; <c>False</c> otherwise.</returns>
    function Contains(const AValue: T): Boolean; override;

    ///  <summary>Specifies the number of elements in the set.</summary>
    ///  <returns>A positive value specifying the number of elements in the set.</returns>
    property Count: NativeInt read FCount;

    ///  <summary>Specifies the current capacity.</summary>
    ///  <returns>A positive number that specifies the number of elements that the set can hold before it
    ///  needs to grow again.</returns>
    ///  <remarks>The value of this property is greater than or equal to the amount of elements in the set. If this value
    ///  if greater than the number of elements, it means that the set has some extra capacity to operate upon.</remarks>
    property Capacity: NativeInt read GetCapacity;

    ///  <summary>Returns a new enumerator object used to enumerate this set.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the set.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<T>; override;

    ///  <summary>Removes the excess capacity from the set.</summary>
    ///  <remarks>This method can be called manually to force the set to drop the extra capacity it might hold. For example,
    ///  after performing some massive operations on a big list, call this method to ensure that all extra memory held by the
    ///  set is released.</remarks>
    procedure Shrink();

    ///  <summary>Forces the set to increase its capacity.</summary>
    ///  <remarks>Call this method to force the set to increase its capacity ahead of time. Manually adjusting the capacity
    ///  can be useful in certain situations.</remarks>
    procedure Grow();

    ///  <summary>Copies the values stored in the set to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the set.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the set.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Checks whether the set is empty.</summary>
    ///  <returns><c>True</c> if the set is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the set is empty.</remarks>
    function Empty(): Boolean; override;

    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the set considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function Max(): T; override;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the set considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function Min(): T; override;

    ///  <summary>Returns the first element.</summary>
    ///  <returns>The first element in the set.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function First(): T; override;

    ///  <summary>Returns the first element or a default, if the set is empty.</summary>
    ///  <param name="ADefault">The default value returned if the set is empty.</param>
    ///  <returns>The first element in the set if the set is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function FirstOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the last element.</summary>
    ///  <returns>The last element in the set.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function Last(): T; override;

    ///  <summary>Returns the last element or a default, if the set is empty.</summary>
    ///  <param name="ADefault">The default value returned if the set is empty.</param>
    ///  <returns>The last element in set if the set is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function LastOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the single element stored in the set.</summary>
    ///  <returns>The element in set.</returns>
    ///  <remarks>This method checks if the set contains just one element, in which case it is returned.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionNotOneException">There is more than one element in the set.</exception>
    function Single(): T; override;

    ///  <summary>Returns the single element stored in the set, or a default value.</summary>
    ///  <param name="ADefault">The default value returned if there is less or more elements in the set.</param>
    ///  <returns>The element in the set if the condition is satisfied; <paramref name="ADefault"/> is returned otherwise.</returns>
    ///  <remarks>This method checks if the set contains just one element, in which case it is returned. Otherwise
    ///  the value in <paramref name="ADefault"/> is returned.</remarks>
    function SingleOrDefault(const ADefault: T): T; override;

    ///  <summary>Aggregates a value based on the set's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <returns>A value that contains the set's aggregated value.</returns>
    ///  <remarks>This method returns the first element if the set only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation, where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function Aggregate(const AAggregator: TFunc<T, T, T>): T; override;

    ///  <summary>Aggregates a value based on the set's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <param name="ADefault">The default value returned if the set is empty.</param>
    ///  <returns>A value that contains the set's aggregated value. If the set is empty, <paramref name="ADefault"/> is returned.</returns>
    ///  <remarks>This method returns the first element if the set only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation, where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T; override;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <returns>The element at the specified position.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    function ElementAt(const AIndex: NativeInt): T; override;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <param name="ADefault">The default value returned if the set is empty.</param>
    ///  <returns>The element at the specified position if the set is not empty and the position is not out of bounds; otherwise
    ///  the value of <paramref name="ADefault"/> is returned.</returns>
    function ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T; override;

    ///  <summary>Checks whether at least one element in the set satisfies a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if at least one element satisfies a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole set and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>True</c>. The logical equivalent of this operation is "OR".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Any(const APredicate: TPredicate<T>): Boolean; override;

    ///  <summary>Checks that all elements in the set satisfy a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if all elements satisfy a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole set and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>False</c>. The logical equivalent of this operation is "AND".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function All(const APredicate: TPredicate<T>): Boolean; override;

    ///  <summary>Checks whether the elements in this set are equal to the elements in another collection.</summary>
    ///  <param name="ACollection">The collection to compare to.</param>
    ///  <returns><c>True</c> if the collections are equal; <c>False</c> if the collections are different.</returns>
    ///  <remarks>This method checks that each element at position X in this set is equal to an element at position X in
    ///  the provided collection. If the number of elements in both collections is different, then the collections are considered different.
    ///  Note that the comparison of elements is done using the rule set used by this set. This means that comparing this collection
    ///  to another one might yeild a different result than comparing the other collection to this one.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function EqualsTo(const ACollection: IEnumerable<T>): Boolean; override;
  end;

  ///  <summary>The generic <c>set</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses an internal array to store its objects.</remarks>
  TObjectArraySet<T: class> = class(TArraySet<T>)
  private
    FOwnsObjects: Boolean;

  protected
    ///  <summary>Frees the object that was removed from the collection.</summary>
    ///  <param name="AElement">The object that was removed from the collection.</param>
    procedure HandleElementRemoved(const AElement: T); override;

  public
    ///  <summary>Specifies whether this set owns the objects stored in it.</summary>
    ///  <returns><c>True</c> if the set owns its objects; <c>False</c> otherwise.</returns>
    ///  <remarks>This property controls the way the set controls the life-time of the stored objects.</remarks>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

{$IF CompilerVersion > 21}
type
  ///  <summary>A specific 16-bit integer <c>set</c> collection.</summary>
  ///  <remarks>This collection uses an internal bit array to store its values.</remarks>
  TBitSet = class(TAbstractSet<Word>, ISortedSet<Word>)
  private type
    {$REGION 'Internal Types'}
    TAscendingEnumerator = class(TAbstractEnumerator<Word>)
    private
      FValue: Word;
      FPageIndex, FBitIndex, FPage: NativeInt;

    public
      function TryMoveNext(out ACurrent: Word): Boolean; override;
    end;

    TDescendingEnumerator = class(TAscendingEnumerator)
    public
      function TryMoveNext(out ACurrent: Word): Boolean; override;
    end;
    {$ENDREGION}

  private const
    CPageSize = SizeOf(NativeInt);

  private var
    FCount: NativeInt;
    FBitArray: TArray<NativeInt>;
    FAscending: Boolean;

  protected
    ///  <summary>Returns the number of elements in the set.</summary>
    ///  <returns>A positive value specifying the number of elements in the set.</returns>
    function GetCount(): NativeInt; override;

  public
    ///  <summary>Creates a new <c>set</c> collection.</summary>
    ///  <remarks>This collection does not use comparison rules for its default functionality. It relies on the fact that default
    ///  RTL comparators use the same rules as the natural ordering of <c>Word</c> values. The elements are stored in ascending order.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>set</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the set.</param>
    ///  <remarks>The supplied rule set is ignored.</remarks>
    constructor Create(const ARules: TRules<Word>); overload;

    ///  <summary>Creates a new <c>set</c> collection.</summary>
    ///  <param name="AAscending">Pass in a value of <c>True</c> if the elements should be kept in ascending order. Pass in <c>False</c> for descending order.</param>
    ///  <remarks>This collection does not use comparison rules for its default functionality. It relies on the fact that default
    ///  RTL comparators use the same rules as the natural ordering of <c>Word</c> values.</remarks>
    constructor Create(const AAscending: Boolean); overload;

    ///  <summary>Clears the contents of the set.</summary>
    procedure Clear(); override;

    ///  <summary>Adds an element to the set.</summary>
    ///  <param name="AValue">The value to add.</param>
    ///  <remarks>If the set already contains the given value, nothing happens.</remarks>
    procedure Add(const AValue: Word); override;

    ///  <summary>Removes a given value from the set.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>If the set does not contain the given value, nothing happens.</remarks>
    procedure Remove(const AValue: Word); override;

    ///  <summary>Checks whether the set contains a given value.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns><c>True</c> if the value was found in the set; <c>False</c> otherwise.</returns>
    function Contains(const AValue: Word): Boolean; override;

    ///  <summary>Specifies the number of elements in the set.</summary>
    ///  <returns>A positive value specifying the number of elements in the set.</returns>
    property Count: NativeInt read FCount;

    ///  <summary>Returns a new enumerator object used to enumerate this set.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the set.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator() : IEnumerator<Word>; override;

    ///  <summary>Copies the values stored in the set to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the set.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the set.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of Word; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Checks whether the set is empty.</summary>
    ///  <returns><c>True</c> if the set is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the set is empty.</remarks>
    function Empty(): Boolean; override;

    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the set considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function Max(): Word; override;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the set considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function Min(): Word; override;

    ///  <summary>Returns the first element.</summary>
    ///  <returns>The first element in the set.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function First(): Word; override;

    ///  <summary>Returns the first element or a default, if the set is empty.</summary>
    ///  <param name="ADefault">The default value returned if the set is empty.</param>
    ///  <returns>The first element in the set if the set is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function FirstOrDefault(const ADefault: Word): Word; override;

    ///  <summary>Returns the last element.</summary>
    ///  <returns>The last element in the set.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function Last(): Word; override;

    ///  <summary>Returns the last element or a default, if the set is empty.</summary>
    ///  <param name="ADefault">The default value returned if the set is empty.</param>
    ///  <returns>The last element in the set if the set is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function LastOrDefault(const ADefault: Word): Word; override;

    ///  <summary>Returns the single element stored in the set.</summary>
    ///  <returns>The element in set.</returns>
    ///  <remarks>This method checks if the set contains just one element, in which case it is returned.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionNotOneException">There is more than one element in the set.</exception>
    function Single(): Word; override;

    ///  <summary>Returns the single element stored in the set, or a default value.</summary>
    ///  <param name="ADefault">The default value returned if there are less or more elements in the set.</param>
    ///  <returns>The element in the set if the condition is satisfied; <paramref name="ADefault"/> is returned otherwise.</returns>
    ///  <remarks>This method checks if the set contains just one element, in which case it is returned. Otherwise
    ///  the value in <paramref name="ADefault"/> is returned.</remarks>
    function SingleOrDefault(const ADefault: Word): Word; override;
  end;
  {$IFEND}

implementation

{ TAbstractSet<T> }


destructor TAbstractSet<T>.Destroy;
begin
  Clear();
  inherited;
end;

constructor TAbstractSet<T>.Create(const ARules: TRules<T>);
begin
  inherited Create(ARules);
end;

{ THashSet<T> }

procedure THashSet<T>.Add(const AValue: T);
begin
 { Call insert }
 Insert(AValue, False);
end;

procedure THashSet<T>.Clear;
var
  I: NativeInt;
begin
  if FCount > 0 then
    for I := 0 to Length(FBucketArray) - 1 do
      FBucketArray[I] := -1;

  for I := 0 to Length(FEntryArray) - 1 do
    if FEntryArray[I].FHashCode >= 0 then
    begin
      NotifyElementRemoved(FEntryArray[I].FKey);
      FEntryArray[I].FKey := default(T);
      NotifyCollectionChanged();
    end;

  if Length(FEntryArray) > 0 then
     FillChar(FEntryArray[0], Length(FEntryArray) * SizeOf(TEntry), 0);

  FFreeList := -1;
  FCount := 0;
  FFreeCount := 0;
end;

function THashSet<T>.Contains(const AValue: T): Boolean;
begin
  Result := (FindEntry(AValue) >= 0);
end;

procedure THashSet<T>.CopyTo(var AArray: array of T; const AStartIndex: NativeInt);
var
  I, X: NativeInt;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;

  for I := 0 to FCount - 1 do
  begin
    if (FEntryArray[I].FHashCode >= 0) then
    begin
       AArray[X] := FEntryArray[I].FKey;
       Inc(X);
    end;
  end;
end;

constructor THashSet<T>.Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt);
begin
  inherited Create(ARules);

  { Check for proper capacity }
  if AInitialCapacity <= 0 then
    InitializeInternals(CDefaultSize)
  else
    InitializeInternals(AInitialCapacity)
end;

constructor THashSet<T>.Create();
begin
  Create(TRules<T>.Default, CDefaultSize);
end;

constructor THashSet<T>.Create(const ARules: TRules<T>);
begin
  Create(ARules, CDefaultSize);
end;

function THashSet<T>.Empty: Boolean;
begin
  Result := (Count = 0);
end;

function THashSet<T>.FindEntry(const AKey: T): NativeInt;
var
  LHashCode: NativeInt;
  I: NativeInt;
begin
  Result := -1;

  if Length(FBucketArray) > 0 then
  begin
    { Generate the hash code }
    LHashCode := Hash(AKey);

    I := FBucketArray[LHashCode mod Length(FBucketArray)];

    while I >= 0 do
    begin
      if (FEntryArray[I].FHashCode = LHashCode) and ElementsAreEqual(FEntryArray[I].FKey, AKey) then
         begin Result := I; Exit; end;

      I := FEntryArray[I].FNext;
    end;
  end;
end;

function THashSet<T>.GetCount: NativeInt;
begin
  Result := (FCount - FFreeCount);
end;

function THashSet<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function THashSet<T>.Hash(const AKey: T): NativeInt;
const
  PositiveMask = not NativeInt(1 shl (SizeOf(NativeInt) * 8 - 1));
begin
  Result := PositiveMask and ((PositiveMask and GetElementHashCode(AKey)) + 1);
end;

procedure THashSet<T>.InitializeInternals(const ACapacity: NativeInt);
var
  I: NativeInt;
begin
  SetLength(FBucketArray, ACapacity);
  SetLength(FEntryArray, ACapacity);

  for I := 0 to ACapacity - 1 do
  begin
    FBucketArray[I] := -1;
    FEntryArray[I].FHashCode := -1;
  end;

  FFreeList := -1;
end;

procedure THashSet<T>.Insert(const AKey: T; const ShouldAdd: Boolean);
var
  LFreeList, LIndex,
    LHashCode, I: NativeInt;
begin
  LFreeList := 0;

  if Length(FBucketArray) = 0 then
     InitializeInternals(CDefaultSize);

  { Generate the hash code }
  LHashCode := Hash(AKey);
  LIndex := LHashCode mod Length(FBucketArray);

  I := FBucketArray[LIndex];

  while I >= 0 do
  begin
    if (FEntryArray[I].FHashCode = LHashCode) and ElementsAreEqual(FEntryArray[I].FKey, AKey) then
    begin
      if (ShouldAdd) then
        ExceptionHelper.Throw_DuplicateKeyError('AKey');

      Exit;
    end;

    { Move to next }
    I := FEntryArray[I].FNext;
  end;

  { Adjust free spaces }
  if FFreeCount > 0 then
  begin
    LFreeList := FFreeList;
    FFreeList := FEntryArray[LFreeList].FNext;

    Dec(FFreeCount);
  end else
  begin
    { Adjust LIndex if there is not enough free space }
    if FCount = Length(FEntryArray) then
    begin
      Resize();
      LIndex := LHashCode mod Length(FBucketArray);
    end;

    LFreeList := FCount;
    Inc(FCount);
  end;

  { Insert the element at the right position and adjust arrays }
  FEntryArray[LFreeList].FHashCode := LHashCode;
  FEntryArray[LFreeList].FKey := AKey;
  FEntryArray[LFreeList].FNext := FBucketArray[LIndex];

  FBucketArray[LIndex] := LFreeList;
  NotifyCollectionChanged();
end;

procedure THashSet<T>.Remove(const AValue: T);
var
  LHashCode, LIndex,
    I, LRemIndex: NativeInt;
begin
  if Length(FBucketArray) > 0 then
  begin
    { Generate the hash code }
    LHashCode := Hash(AValue);

    LIndex := LHashCode mod Length(FBucketArray);
    LRemIndex := -1;

    I := FBucketArray[LIndex];

    while I >= 0 do
    begin
      if (FEntryArray[I].FHashCode = LHashCode) and ElementsAreEqual(FEntryArray[I].FKey, AValue) then
      begin

        if LRemIndex < 0 then
        begin
          FBucketArray[LIndex] := FEntryArray[I].FNext;
        end else
        begin
          FEntryArray[LRemIndex].FNext := FEntryArray[I].FNext;
        end;

        FEntryArray[I].FHashCode := -1;
        FEntryArray[I].FNext := FFreeList;
        FEntryArray[I].FKey := default(T);

        FFreeList := I;
        Inc(FFreeCount);
        NotifyCollectionChanged();

        Exit;
      end;

      LRemIndex := I;
      I := FEntryArray[I].FNext;
    end;

  end;
end;

procedure THashSet<T>.Resize;
var
  LNewLength, I, LIndex: NativeInt;
begin
  LNewLength := FCount * 2;

  SetLength(FBucketArray, LNewLength);
  SetLength(FEntryArray, LNewLength);

  for I := 0 to LNewLength - 1 do
    FBucketArray[I] := -1;

  for I := 0 to FCount - 1 do
  begin
    LIndex := FEntryArray[I].FHashCode mod LNewLength;
    FEntryArray[I].FNext := FBucketArray[LIndex];
    FBucketArray[LIndex] := I;
  end;
end;

{ THashSet<T>.TEnumerator }

function THashSet<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  with THashSet<T>(Owner) do
  begin
    while FCurrentIndex < FCount do
    begin
      if FEntryArray[FCurrentIndex].FHashCode >= 0 then
      begin
        ACurrent := FEntryArray[FCurrentIndex].FKey;

        Inc(FCurrentIndex);
        Result := True;
        Exit;
      end;

      Inc(FCurrentIndex);
    end;

    FCurrentIndex := FCount + 1;
    Result := False;
  end;
end;

{ TObjectHashSet<T> }

procedure TObjectHashSet<T>.HandleElementRemoved(const AElement: T);
begin
  if FOwnsObjects then
    TObject(AElement).Free;
end;

{ TLinkedSet<T> }

procedure TLinkedSet<T>.Add(const AValue: T);
begin
 { Call insert }
 Insert(AValue, False);
end;

procedure TLinkedSet<T>.Clear;
var
  LEntry, LCurr: PEntry;
begin
  LEntry := FHead;

  while Assigned(LEntry) do
  begin
    NotifyElementRemoved(LEntry.FValue);

    { Next and kill }
    LCurr := LEntry;
    LEntry := LEntry^.FNext;

    ReleaseEntry(LCurr);
  end;

  { Clear nodes }
  FHead := nil;
  FTail := nil;

  { Clear array }
  FillChar(FBucketArray[0], Length(FBucketArray) * SizeOf(PEntry), 0);
  FCount := 0;

  NotifyCollectionChanged();
end;

function TLinkedSet<T>.Contains(const AValue: T): Boolean;
begin
  Result := Assigned(FindEntry(AValue));
end;

procedure TLinkedSet<T>.CopyTo(var AArray: array of T; const AStartIndex: NativeInt);
var
  X: NativeInt;
  LEntry: PEntry;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;
  LEntry := FHead;

  while Assigned(LEntry) do
  begin
    { Copy it }
    AArray[X] := LEntry^.FValue;

    { Go to next }
    Inc(X);
    LEntry := LEntry^.FNext;
  end;
end;

constructor TLinkedSet<T>.Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt);
begin
  inherited Create(ARules);

  { Check for proper capacity }
  if AInitialCapacity <= 0 then
    InitializeInternals(CDefaultSize)
  else
    InitializeInternals(AInitialCapacity)
end;

constructor TLinkedSet<T>.Create;
begin
  Create(TRules<T>.Default, CDefaultSize);
end;

constructor TLinkedSet<T>.Create(const ARules: TRules<T>);
begin
  { Call upper constructor }
  Create(ARules, CDefaultSize);
end;

destructor TLinkedSet<T>.Destroy;
var
  LNext: PEntry;
begin
  { Clear first }
  Clear();

  { Clear the cached entries too }
  if FFreeCount > 0 then
    while Assigned(FFirstFree) do
    begin
      LNext := FFirstFree^.FNext;

      { Delphi doesn finalize this }
      FFirstFree^.FValue := default(T);

      FreeMem(FFirstFree);
      FFirstFree := LNext;
    end;

  inherited;
end;

function TLinkedSet<T>.Empty: Boolean;
begin
  Result := (FCount = 0);
end;

function TLinkedSet<T>.FindEntry(const AValue: T): PEntry;
var
  LHashCode, LCapacity: NativeInt;
  LEntry: PEntry;
begin
  { Init }
  Result := nil;
  LHashCode := Hash(AValue);
  LCapacity := Length(FBucketArray);
  LEntry := FBucketArray[LHashCode mod LCapacity];

  while Assigned(LEntry) and
    ((LEntry^.FHashCode mod LCapacity) = (LHashCode mod LCapacity)) do
  begin
    { Check the key }
    if ElementsAreEqual(LEntry^.FValue, AValue) then
      Exit(LEntry);

    { Go to next }
    LEntry := LEntry^.FNext;
  end;
end;

function TLinkedSet<T>.GetCount: NativeInt;
begin
  Result := FCount;
end;

function TLinkedSet<T>.GetEnumerator: IEnumerator<T>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FCurrentEntry := FHead;
  Result := LEnumerator;
end;

function TLinkedSet<T>.Hash(const AValue: T): NativeInt;
const
  PositiveMask = not NativeInt(1 shl (SizeOf(NativeInt) * 8 - 1));
begin
  Result := PositiveMask and ((PositiveMask and GetElementHashCode(AValue)) + 1);
end;

procedure TLinkedSet<T>.InitializeInternals(const ACapacity: NativeInt);
begin
  { Initialize and clear the dictionary }
  SetLength(FBucketArray, ACapacity);
  FillChar(FBucketArray[0], ACapacity * SizeOf(PEntry), 0);
end;

procedure TLinkedSet<T>.Insert(const AValue: T; const AShouldAdd: Boolean);
var
  LHashCode, LNewLength, LCapacity: NativeInt;
  LEntry, LNewEntry: PEntry;
begin
  { Initialize stuff }
  LHashCode := Hash(AValue);

  while True do
  begin
    LCapacity := Length(FBucketArray);
    LEntry := FBucketArray[LHashCode mod LCapacity];

    { Case 1: we have a free spot and can insert directly }
    if not Assigned(LEntry) then
    begin
      { Insert the entry }
      LNewEntry := NeedEntry(AValue, LHashCode);
      LNewEntry^.FPrev := FTail;
      LNewEntry^.FNext := nil;

      if Assigned(FTail) then
        FTail^.FNext := LNewEntry;

      FTail := LNewEntry;

      if not Assigned(FHead) then
        FHead := LNewEntry;

      FBucketArray[LHashCode mod LCapacity] := LNewEntry;

      NotifyCollectionChanged();
      Inc(FCount);

      Exit;
    end;

    { Case 2: The spot is filled but capacity is sufficient }
    if FCount < LCapacity then
    begin
      { Search for a place to insert the node into }

      while True do
      begin
        { Check the key }
        if ElementsAreEqual(LEntry^.FValue, AValue) then
        begin
          if AShouldAdd then
            ExceptionHelper.Throw_DuplicateKeyError('AKey');

          LEntry^.FValue := AValue;

          NotifyCollectionChanged();
          Exit;
        end;

        if not Assigned(LEntry^.FNext) or
           ((LEntry^.FNext^.FHashCode mod LCapacity) <> (LHashCode mod LCapacity)) then Break;

        { Go to next }
        LEntry := LEntry^.FNext;
      end;

      { Insert the entry }
      LNewEntry := NeedEntry(AValue, LHashCode);

      { Get our entry in }
      LNewEntry^.FNext := LEntry^.FNext;
      LNewEntry^.FPrev := LEntry;

      if Assigned(LEntry^.FNext) then
        LEntry^.FNext^.FPrev := LNewEntry;

      LEntry^.FNext := LNewEntry;

      if LEntry = FTail then
        FTail := LNewEntry;

      NotifyCollectionChanged();
      Inc(FCount);

      Exit;
    end;

    { Case 3: The spot is filled but capacity is not sufficient }
    if FCount >= LCapacity then
    begin
      { Reset the bucket list }
      LNewLength := FCount * 2;
      SetLength(FBucketArray, LNewLength);
      FillChar(FBucketArray[0], LNewLength * SizeOf(PEntry), 0);

      { Rehash! }
      LEntry := FHead;
      FHead := nil;
      FTail := nil;

      { Rehash the whole list using new capacity }
      while Assigned(LEntry) do
      begin
        LNewEntry := LEntry^.FNext;
        ReInsert(LEntry, LNewLength);

        LEntry := LNewEntry;
      end;
    end;
  end;
end;

function TLinkedSet<T>.NeedEntry(const AValue: T; const AHash: NativeInt): PEntry;
begin
  if FFreeCount > 0 then
  begin
    Result := FFirstFree;
    FFirstFree := FFirstFree^.FNext;

    Dec(FFreeCount);
  end else
    Result := AllocMem(SizeOf(TEntry));

  { Initialize the node }
  Result^.FHashCode := AHash;
  Result^.FValue := AValue;
end;

procedure TLinkedSet<T>.ReInsert(const AEntry: PEntry; const ACapacity: NativeInt);
var
  LEntry: PEntry;
begin
  { Initialize stuff }
  LEntry := FBucketArray[AEntry^.FHashCode mod ACapacity];

  { Case 1: we have a free spot and can insert directly }
  if not Assigned(LEntry) then
  begin
    AEntry^.FPrev := FTail;
    AEntry^.FNext := nil;

    if Assigned(FTail) then
      FTail^.FNext := AEntry;

    FTail := AEntry;

    if not Assigned(FHead) then
      FHead := AEntry;

    FBucketArray[AEntry^.FHashCode mod ACapacity] := AEntry;

    Exit;
  end;

  { Case 2: The spot is filled but capacity is sufficient }
  while True do
  begin
    { Check the key }
    ASSERT(not ElementsAreEqual(LEntry^.FValue, AEntry^.FValue));

    if not Assigned(LEntry^.FNext) or
       ((LEntry^.FNext^.FHashCode mod ACapacity) <> (AEntry^.FHashCode mod ACapacity)) then Break;

    { Go to next }
    LEntry := LEntry^.FNext;
  end;

  { Get our entry in }
  AEntry^.FNext := LEntry^.FNext;
  AEntry^.FPrev := LEntry;

  if Assigned(LEntry^.FNext) then
    LEntry^.FNext^.FPrev := AEntry;

  LEntry^.FNext := AEntry;

  if LEntry = FTail then
    FTail := AEntry;
end;

procedure TLinkedSet<T>.ReleaseEntry(const AEntry: PEntry);
begin
  if FFreeCount = CDefaultSize then
  begin
    { Delphi doesn finalize this }
    AEntry^.FValue := default(T);
    FreeMem(AEntry);
  end else begin
    { Place the entry into the cache }
    AEntry^.FNext := FFirstFree;
    FFirstFree := AEntry;

    Inc(FFreeCount);
  end;
end;

procedure TLinkedSet<T>.Remove(const AValue: T);
var
  LHashCode, LCapacity: NativeInt;
  LEntry: PEntry;
begin
  { Generate the hash code }
  LHashCode := Hash(AValue);
  LCapacity := Length(FBucketArray);
  LEntry := FBucketArray[LHashCode mod LCapacity];

  while Assigned(LEntry) and
       ((LEntry^.FHashCode mod LCapacity) = (LHashCode mod LCapacity)) do
  begin
    { Check the key }
    if ElementsAreEqual(LEntry^.FValue, AValue) then
    begin
      { Remove self from the linked list }
      if Assigned(LEntry^.FPrev) then
        LEntry^.FPrev^.FNext := LEntry^.FNext;

      if Assigned(LEntry^.FNext) then
        LEntry^.FNext^.FPrev := LEntry^.FPrev;

      if LEntry = FBucketArray[LHashCode mod LCapacity] then
      begin
        { This entry is the first for the given hash code. Set the next if it has same hash. }
        if Assigned(LEntry^.FNext) and ((LEntry^.FNext^.FHashCode mod LCapacity) = (LEntry^.FHashCode mod LCapacity)) then
          FBucketArray[LHashCode mod LCapacity] := LEntry^.FNext
        else
          FBucketArray[LHashCode mod LCapacity] := nil;
      end;

      if FTail = LEntry then
        FTail := LEntry^.FPrev;

      if FHead = LEntry then
        FHead := LEntry^.FNext;

      { Kill this entry }
      ReleaseEntry(LEntry);
      Dec(FCount);
      NotifyCollectionChanged();

      { All done, let's exit }
      Exit;
    end;

    { Go to next }
    LEntry := LEntry^.FNext;
  end;
end;

{ TLinkedSet<T>.TPairEnumerator }

function TLinkedSet<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  Result := Assigned(FCurrentEntry);
  if Result then
  begin
    ACurrent := FCurrentEntry^.FValue;
    FCurrentEntry := FCurrentEntry^.FNext;
  end;
end;

{ TObjectLinkedSet<T> }

procedure TObjectLinkedSet<T>.HandleElementRemoved(const AElement: T);
begin
  if FOwnsObjects then
    TObject(AElement).Free;
end;

{ TSortedSet<T> }

procedure TSortedSet<T>.Add(const AValue: T);
begin
  { Insert the value }
  Insert(AValue);
end;

procedure TSortedSet<T>.BalanceTreesAfterRemoval(const ANode: TNode);
var
  LCurrentAct: TBalanceAct;
  LLNode, LXNode, LSNode,
    LWNode, LYNode: TNode;
begin
  { Initialize ... }
  LCurrentAct := TBalanceAct.baStart;
  LLNode := ANode;

  { Continue looping until end is declared }
  while LCurrentAct <> TBalanceAct.baEnd do
  begin
    case LCurrentAct of

      { START MODE }
      TBalanceAct.baStart:
      begin
        if not Assigned(LLNode.FRight) then
        begin
          { Exclude myself! }
          if Assigned(LLNode.FLeft) then
            LLNode.FLeft.FParent := LLNode.FParent;

          { I'm root! nothing to do here }
          if not Assigned(LLNode.FParent) then
          begin
            FRoot := LLNode.FLeft;

            { DONE! }
            LCurrentAct := TBalanceAct.baEnd;
            continue;
          end;

          { ... }
          if LLNode = LLNode.FParent.FLeft then
          begin
            LLNode.FParent.FLeft := LLNode.FLeft;
            LYNode := LLNode.FParent;
          end else
          begin
            LLNode.FParent.FRight := LLNode.FLeft;
            LYNode := LLNode.FParent;

            { RIGHT! }
            LCurrentAct := TBalanceAct.baRight;
            continue;
          end;
        end else if not Assigned(LLNode.FRight.FLeft) then
        begin
          { Case 1, RIGHT, NO LEFT }
          if Assigned(LLNode.FLeft) then
          begin
            LLNode.FLeft.FParent := LLNode.FRight;
            LLNode.FRight.FLeft := LLNode.FLeft;
          end;

          LLNode.FRight.FBalance := LLNode.FBalance;
          LLNode.FRight.FParent := LLNode.FParent;

          if not Assigned(LLNode.FParent) then
            FRoot := LLNode.FRight
          else
          begin
            if LLNode = LLNode.FParent.FLeft then
              LLNode.FParent.FLeft := LLNode.FRight
            else
              LLNode.FParent.FRight := LLNode.FRight;
          end;

          LYNode := LLNode.FRight;

          { RIGHT! }
          LCurrentAct := TBalanceAct.baRight;
          continue;
        end else
        begin
          { Case 3: RIGHT+LEFT }
          LSNode := LLNode.FRight.FLeft;

          while Assigned(LSNode.FLeft) do
            LSNode := LSNode.FLeft;

          if Assigned(LLNode.FLeft) then
          begin
            LLNode.FLeft.FParent := LSNode;
            LSNode.FLeft := LLNode.FLeft;
          end;

          LSNode.FParent.FLeft := LSNode.FRight;

          if Assigned(LSNode.FRight) then
            LSNode.FRight.FParent := LSNode.FParent;

          LLNode.FRight.FParent := LSNode;
          LSNode.FRight := LLNode.FRight;

          LYNode := LSNode.FParent;

          LSNode.FBalance := LLNode.FBalance;
          LSNode.FParent := LLNode.FParent;

          if not Assigned(LLNode.FParent) then
            FRoot := LSNode
          else
          begin
            if LLNode = LLNode.FParent.FLeft then
              LLNode.FParent.FLeft := LSNode
            else
              LLNode.FParent.FRight := LSNode;
          end;
        end;

        { LEFT! }
        LCurrentAct := TBalanceAct.baLeft;
        continue;
      end; { baStart }

      { LEFT BALANCING MODE }
      TBalanceAct.baLeft:
      begin
        Inc(LYNode.FBalance);

        if LYNode.FBalance = 1 then
        begin
          { DONE! }
          LCurrentAct := TBalanceAct.baEnd;
          continue;
        end
        else if LYNode.FBalance = 2 then
        begin
          LXNode := LYNode.FRight;

          if LXNode.FBalance = -1 then
          begin
            LWNode := LXNode.FLeft;
            LWNode.FParent := LYNode.FParent;

            if not Assigned(LYNode.FParent) then
              FRoot := LWNode
            else
            begin
              if LYNode.FParent.FLeft = LYNode then
                LYNode.FParent.FLeft := LWNode
              else
                LYNode.FParent.FRight := LWNode;
            end;

            LXNode.FLeft := LWNode.FRight;

            if Assigned(LXNode.FLeft) then
              LXNode.FLeft.FParent := LXNode;

            LYNode.FRight := LWNode.FLeft;

            if Assigned(LYNode.FRight) then
              LYNode.FRight.FParent := LYNode;

            LWNode.FRight := LXNode;
            LWNode.FLeft := LYNode;

            LXNode.FParent := LWNode;
            LYNode.FParent := LWNode;

            if LWNode.FBalance = 1 then
            begin
              LXNode.FBalance := 0;
              LYNode.FBalance := -1;
            end else if LWNode.FBalance = 0 then
            begin
              LXNode.FBalance := 0;
              LYNode.FBalance := 0;
            end else
            begin
              LXNode.FBalance := 1;
              LYNode.FBalance := 0;
            end;

            LWNode.FBalance := 0;
            LYNode := LWNode;
          end else
          begin
            LXNode.FParent := LYNode.FParent;

            if Assigned(LYNode.FParent) then
            begin
              if LYNode.FParent.FLeft = LYNode then
                LYNode.FParent.FLeft := LXNode
              else
                LYNode.FParent.FRight := LXNode;
            end else
              FRoot := LXNode;

            LYNode.FRight := LXNode.FLeft;

            if Assigned(LYNode.FRight) then
              LYNode.FRight.FParent := LYNode;

            LXNode.FLeft := LYNode;
            LYNode.FParent := LXNode;

            if LXNode.FBalance = 0 then
            begin
              LXNode.FBalance := -1;
              LYNode.FBalance := 1;

              { DONE! }
              LCurrentAct := TBalanceAct.baEnd;
              continue;
            end else
            begin
              LXNode.FBalance := 0;
              LYNode.FBalance := 0;

              LYNode := LXNode;
            end;
          end;
        end;

        { LOOP! }
        LCurrentAct := TBalanceAct.baLoop;
        continue;
      end; { baLeft }

      { RIGHT BALANCING MODE }
      TBalanceAct.baRight:
      begin
        Dec(LYNode.FBalance);

        if LYNode.FBalance = -1 then
        begin
          { DONE! }
          LCurrentAct := TBalanceAct.baEnd;
          continue;
        end
        else if LYNode.FBalance = -2 then
        begin
          LXNode := LYNode.FLeft;

          if LXNode.FBalance = 1 then
          begin
            LWNode := LXNode.FRight;
            LWNode.FParent := LYNode.FParent;

            if not Assigned(LYNode.FParent) then
              FRoot := LWNode
            else
            begin
              if LYNode.FParent.FLeft = LYNode then
                LYNode.FParent.FLeft := LWNode
              else
                LYNode.FParent.FRight := LWNode;
            end;

            LXNode.FRight := LWNode.FLeft;

            if Assigned(LXNode.FRight) then
              LXNode.FRight.FParent := LXNode;

            LYNode.FLeft := LWNode.FRight;

            if Assigned(LYNode.FLeft) then
              LYNode.FLeft.FParent := LYNode;

            LWNode.FLeft := LXNode;
            LWNode.FRight := LYNode;

            LXNode.FParent := LWNode;
            LYNode.FParent := LWNode;

            if LWNode.FBalance = -1 then
            begin
              LXNode.FBalance := 0;
              LYNode.FBalance := 1;
            end else if LWNode.FBalance = 0 then
            begin
              LXNode.FBalance := 0;
              LYNode.FBalance := 0;
            end else
            begin
              LXNode.FBalance := -1;
              LYNode.FBalance := 0;
            end;

            LWNode.FBalance := 0;
            LYNode := LWNode;
          end else
          begin
            LXNode.FParent := LYNode.FParent;

            if Assigned(LYNode.FParent) then
            begin
              if LYNode.FParent.FLeft = LYNode then
                LYNode.FParent.FLeft := LXNode
              else
                LYNode.FParent.FRight := LXNode
            end else
              FRoot := LXNode;

            LYNode.FLeft := LXNode.FRight;

            if Assigned(LYNode.FLeft) then
              LYNode.FLeft.FParent := LYNode;

            LXNode.FRight := LYNode;
            LYNode.FParent := LXNode;

            if LXNode.FBalance = 0 then
            begin
              LXNode.FBalance := 1;
              LYNode.FBalance := -1;

              { END! }
              LCurrentAct := TBalanceAct.baEnd;
              continue;
            end else
            begin
              LXNode.FBalance := 0;
              LYNode.FBalance := 0;

              LYNode := LXNode;
            end;
          end;
        end;

        { LOOP! }
        LCurrentAct := TBalanceAct.baLoop;
        continue;
      end; { baRight }

      TBalanceAct.baLoop:
      begin
        { Verify continuation }
        if Assigned(LYNode.FParent) then
        begin
          if LYNode = LYNode.FParent.FLeft then
          begin
            LYNode := LYNode.FParent;

            { LEFT! }
            LCurrentAct := TBalanceAct.baLeft;
            continue;
          end;

          LYNode := LYNode.FParent;

          { RIGHT! }
          LCurrentAct := TBalanceAct.baRight;
          continue;
        end;

        { END! }
        LCurrentAct := TBalanceAct.baEnd;
        continue;
      end;
    end; { Case }
  end; { While }
end;

procedure TSortedSet<T>.Clear;
begin
  if Assigned(FRoot) then
  begin
    RecursiveClear(FRoot);
    FRoot := nil;

    { Update markers }
    NotifyCollectionChanged();
    FCount := 0;
  end;
end;

function TSortedSet<T>.Contains(const AValue: T): Boolean;
begin
  Result := Assigned(FindNodeWithKey(AValue));
end;

procedure TSortedSet<T>.CopyTo(var AArray: array of T; const AStartIndex: NativeInt);
var
  X: NativeInt;
  LNode: TNode;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < FCount then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;

  { Find the left-most node }
  LNode := FindLeftMostNode();

  while Assigned(LNode) do
  begin
    { Get the key }
    AArray[X] := LNode.FKey;

    { Navigate further in the tree }
    LNode := WalkToTheRight(LNode);

    { Increment the index }
    Inc(X);
  end;
end;

constructor TSortedSet<T>.Create(const ARules: TRules<T>; const AAscending: Boolean);
begin
  inherited Create(ARules);

  if AAscending then
    FSignFix := 1
  else
    FSignFix := -1;
end;

constructor TSortedSet<T>.Create;
begin
  Create(TRules<T>.Default, True);
end;

function TSortedSet<T>.Empty: Boolean;
begin
  Result := not Assigned(FRoot);
end;

function TSortedSet<T>.FindLeftMostNode: TNode;
begin
  { Start with root }
  Result := FRoot;

  { And go to maximum left }
  if Assigned(Result) then
  begin
    while Assigned(Result.FLeft) do
      Result := Result.FLeft;
  end;
end;

function TSortedSet<T>.FindNodeWithKey(const AValue: T): TNode;
var
  LNode: TNode;
  LCompareResult: NativeInt;
begin
  { Get root }
  LNode := FRoot;

  while Assigned(LNode) do
  begin
	  LCompareResult := CompareElements(AValue, LNode.FKey) * FSignFix;

    { Navigate left, right or find! }
    if LCompareResult < 0 then
      LNode := LNode.FLeft
    else if LCompareResult > 0 then
      LNode := LNode.FRight
    else
      Exit(LNode);
  end;

  { Did not find anything ... }
  Result := nil;
end;

function TSortedSet<T>.FindRightMostNode: TNode;
begin
  { Start with root }
  Result := FRoot;

  { And go to maximum left }
  if Assigned(Result) then
  begin
    while Assigned(Result.FRight) do
      Result := Result.FRight;
  end;
end;

function TSortedSet<T>.First: T;
begin
  { Check there are elements in the set }
  if not Assigned(FRoot) then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FindLeftMostNode().FKey
end;

function TSortedSet<T>.FirstOrDefault(const ADefault: T): T;
begin
  { Check there are elements in the set }
  if not Assigned(FRoot) then
    Result := ADefault
  else
    Result := FindLeftMostNode().FKey
end;

function TSortedSet<T>.GetCount: NativeInt;
begin
  Result := FCount;
end;

function TSortedSet<T>.GetEnumerator: IEnumerator<T>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FCurrentEntry := FindLeftMostNode();
  Result := LEnumerator;
end;

procedure TSortedSet<T>.Insert(const AValue: T);
var
  LNode: TNode;
  LCompareResult: NativeInt;
begin
  { First one get special treatment! }
  if not Assigned(FRoot) then
  begin
    FRoot := MakeNode(AValue, nil);

    { Increase markers }
    Inc(FCount);
    NotifyCollectionChanged();

    { [ADDED NEW] Exit function }
    Exit;
  end;

  { Get root }
  LNode := FRoot;

  while true do
  begin
	  LCompareResult := CompareElements(AValue, LNode.FKey) * FSignFix;

    if LCompareResult < 0 then
    begin
      if Assigned(LNode.FLeft) then
        LNode := LNode.FLeft
      else
      begin
        { Create a new node }
        LNode.FLeft := MakeNode(AValue, LNode);
        Dec(LNode.FBalance);

        { [ADDED NEW] Exit function! }
        break;
      end;
    end else if LCompareResult > 0 then
    begin
      if Assigned(LNode.FRight) then
        LNode := LNode.FRight
      else
      begin
        LNode.FRight := MakeNode(AValue, LNode);
        Inc(LNode.FBalance);

        { [ADDED NEW] Exit function! }
        break;
      end;
    end else
    begin
      { Found a node with the same key. }
      { [NOTHING] Exit function }
      Exit();
    end;
  end;

  { Rebalance the tree }
  ReBalanceSubTreeOnInsert(LNode);

  Inc(FCount);
  NotifyCollectionChanged();
end;

function TSortedSet<T>.Last: T;
begin
  { Check there are elements in the set }
  if not Assigned(FRoot) then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FindRightMostNode().FKey
end;

function TSortedSet<T>.LastOrDefault(const ADefault: T): T;
begin
  { Check there are elements in the set }
  if not Assigned(FRoot) then
    Result := ADefault
  else
    Result := FindRightMostNode().FKey
end;

function TSortedSet<T>.MakeNode(const AValue: T; const ARoot: TNode): TNode;
begin
  Result := TNode.Create();
  Result.FKey := AValue;
  Result.FParent := ARoot;
end;

function TSortedSet<T>.Max: T;
begin
  { Check there are elements in the set }
  if not Assigned(FRoot) then
    ExceptionHelper.Throw_CollectionEmptyError();

  if FSignFix = 1 then
    Result := FindRightMostNode().FKey
  else
    Result := FindLeftMostNode().FKey;
end;

function TSortedSet<T>.Min: T;
begin
  { Check there are elements in the set }
  if not Assigned(FRoot) then
    ExceptionHelper.Throw_CollectionEmptyError();

  if FSignFix = 1 then
    Result := FindLeftMostNode().FKey
  else
    Result := FindRightMostNode().FKey;
end;

procedure TSortedSet<T>.ReBalanceSubTreeOnInsert(const ANode: TNode);
var
  LLNode, LXNode, LWNode: TNode;
begin
  LLNode := ANode;

  { Re-balancing the tree! }
  while (LLNode.FBalance <> 0) and Assigned(LLNode.FParent) do
  begin
    if (LLNode.FParent.FLeft = LLNode) then
      Dec(LLNode.FParent.FBalance)
    else
      Inc(LLNode.FParent.FBalance);

    { Move up }
    LLNode := LLNode.FParent;

    if (LLNode.FBalance = -2) then
    begin
      LXNode := LLNode.FLeft;

      if (LXNode.FBalance = -1) then
      begin
        LXNode.FParent := LLNode.FParent;

        if not Assigned(LLNode.FParent) then
          FRoot := LXNode
        else
        begin
          if (LLNode.FParent.FLeft = LLNode) then
            LLNode.FParent.FLeft := LXNode
          else
            LLNode.FParent.FRight := LXNode;
        end;

        LLNode.FLeft := LXNode.FRight;

        if Assigned(LLNode.FLeft) then
          LLNode.FLeft.FParent := LLNode;

        LXNode.FRight := LLNode;
        LLNode.FParent := LXNode;

        LXNode.FBalance := 0;
        LLNode.FBalance := 0;
      end else
      begin
        LWNode := LXNode.FRight;
        LWNode.FParent := LLNode.FParent;

        if not Assigned(LLNode.FParent) then
          FRoot := LWNode
        else
        begin
          if LLNode.FParent.FLeft = LLNode then
            LLNode.FParent.FLeft := LWNode
          else
            LLNode.FParent.FRight := LWNode;
        end;

        LXNode.FRight := LWNode.FLeft;

        if Assigned(LXNode.FRight) then
          LXNode.FRight.FParent := LXNode;

        LLNode.FLeft := LWNode.FRight;

        if Assigned(LLNode.FLeft) then
          LLNode.FLeft.FParent := LLNode;

        LWNode.FLeft := LXNode;
        LWNode.FRight := LLNode;

        LXNode.FParent := LWNode;
        LLNode.FParent := LWNode;

        { Apply proper balancing }
        if LWNode.FBalance = -1 then
        begin
          LXNode.FBalance := 0;
          LLNode.FBalance := 1;
        end else if LWNode.FBalance = 0 then
        begin
          LXNode.FBalance := 0;
          LLNode.FBalance := 0;
        end else
        begin
          LXNode.FBalance := -1;
          LLNode.FBalance := 0;
        end;

        LWNode.FBalance := 0;
      end;

      break;
    end else if LLNode.FBalance = 2 then
    begin
      LXNode := LLNode.FRight;

      if LXNode.FBalance = 1 then
      begin
        LXNode.FParent := LLNode.FParent;

        if not Assigned(LLNode.FParent) then
          FRoot := LXNode
        else
        begin
          if LLNode.FParent.FLeft = LLNode then
            LLNode.FParent.FLeft := LXNode
          else
            LLNode.FParent.FRight := LXNode;
        end;

        LLNode.FRight := LXNode.FLeft;

        if Assigned(LLNode.FRight) then
          LLNode.FRight.FParent := LLNode;

        LXNode.FLeft := LLNode;
        LLNode.FParent := LXNode;

        LXNode.FBalance := 0;
        LLNode.FBalance := 0;
      end else
      begin
        LWNode := LXNode.FLeft;
        LWNode.FParent := LLNode.FParent;

        if not Assigned(LLNode.FParent) then
          FRoot := LWNode
        else
        begin
          if LLNode.FParent.FLeft = LLNode then
            LLNode.FParent.FLeft := LWNode
          else
            LLNode.FParent.FRight := LWNode;
        end;

        LXNode.FLeft := LWNode.FRight;

        if Assigned(LXNode.FLeft) then
          LXNode.FLeft.FParent := LXNode;

        LLNode.FRight := LWNode.FLeft;

        if Assigned(LLNode.FRight) then
          LLNode.FRight.FParent := LLNode;

        LWNode.FRight := LXNode;
        LWNode.FLeft := LLNode;

        LXNode.FParent := LWNode;
        LLNode.FParent := LWNode;

        if LWNode.FBalance = 1 then
        begin
          LXNode.FBalance := 0;
          LLNode.FBalance := -1;
        end else if LWNode.FBalance = 0 then
        begin
          LXNode.FBalance := 0;
          LLNode.FBalance := 0;
        end else
        begin
          LXNode.FBalance := 1;
          LLNode.FBalance := 0;
        end;

        LWNode.FBalance := 0;
      end;

      break;
    end;
  end;
end;

procedure TSortedSet<T>.Remove(const AValue: T);
var
  LNode: TNode;
begin
  { Get root }
  LNode := FindNodeWithKey(AValue);

  { Remove and rebalance the tree accordingly }
  if not Assigned(LNode) then
    Exit;

  { .. Do da dew! }
  BalanceTreesAfterRemoval(LNode);

  { Kill the node }
  LNode.Free;

  Dec(FCount);
  NotifyCollectionChanged();
end;

function TSortedSet<T>.Single: T;
begin
  { Check there are elements in the set }
  if not Assigned(FRoot) then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Check for more than one }
  if Assigned(FRoot.FLeft) or Assigned(FRoot.FRight) then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement();

  Result := FRoot.FKey;
end;

function TSortedSet<T>.SingleOrDefault(const ADefault: T): T;
begin
  { Check there are elements in the set }
  if not Assigned(FRoot) then
    Exit(ADefault);

  { Check for more than one }
  if Assigned(FRoot.FLeft) or Assigned(FRoot.FRight) then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement();

  Result := FRoot.FKey;
end;

procedure TSortedSet<T>.RecursiveClear(const ANode: TNode);
begin
  if Assigned(ANode.FLeft)then
    RecursiveClear(ANode.FLeft);

  if Assigned(ANode.FRight) then
    RecursiveClear(ANode.FRight);

  { Cleanup for Key/Value }
  NotifyElementRemoved(ANode.FKey);

  { Finally, free the node itself }
  ANode.Free;
end;

function TSortedSet<T>.WalkToTheRight(const ANode: TNode): TNode;
begin
  Result := ANode;

  if not Assigned(Result) then
    Exit;

  { Navigate further in the tree }
  if not Assigned(Result.FRight) then
  begin
    while (Assigned(Result.FParent) and (Result = Result.FParent.FRight)) do
      Result := Result.FParent;

    Result := Result.FParent;
  end else
  begin
    Result := Result.FRight;

    while Assigned(Result.FLeft) do
      Result := Result.FLeft;
  end;
end;

constructor TSortedSet<T>.Create(const ARules: TRules<T>);
begin
  Create(ARules, True);
end;

{ TSortedSet<T>.TEnumerator }

function TSortedSet<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  Result := Assigned(FCurrentEntry);

  if Result then
  begin
    ACurrent := FCurrentEntry.FKey;
    FCurrentEntry := TSortedSet<T>(Owner).WalkToTheRight(FCurrentEntry);
  end;
end;

{ TObjectSortedSet<T> }

procedure TObjectSortedSet<T>.HandleElementRemoved(const AElement: T);
begin
  if FOwnsObjects then
    TObject(AElement).Free;
end;

{ TArraySet<T> }

procedure TArraySet<T>.Add(const AValue: T);
var
  LLeft, LRight, LMiddle, I: NativeInt;
  LCompareResult: NativeInt;
begin
  { Case 1, empty list, optimize }
  if FCount > 0 then
  begin
    { Check for valid type support }
    LLeft := 0;
    LRight := LLeft + FCount - 1;

    while (LLeft <= LRight) do
    begin
      LMiddle := (LLeft + LRight) div 2;
      LCompareResult := CompareElements(FArray[LMiddle], AValue) * FSignFix;

      if LCompareResult > 0 then
        LRight := LMiddle - 1
      else if LCompareResult < 0 then
        LLeft := LMiddle + 1
      else
        Exit; { Element already contained in the array, exit }
    end;

    if LCompareResult < 0 then
      Inc(LMiddle);
  end else
    LMiddle := 0;

  if FCount = Length(FArray) then
    Grow();

  { Move the array to the right }
  if LMiddle < FCount then
    for I := FCount downto (LMiddle + 1) do
      FArray[I] := FArray[I - 1];

  { Put the element into the new position }
  FArray[LMiddle] := AValue;

  NotifyCollectionChanged();
  Inc(FCount);
end;

function TArraySet<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
var
  I: NativeInt;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := FArray[0];

  { Iterate over the last N - 1 elements }
  for I := 1 to FCount - 1 do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, FArray[I]);
  end;
end;

function TArraySet<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
var
  I: NativeInt;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if FCount = 0 then
    Exit(ADefault);

  { Select the first element as comparison base }
  Result := FArray[0];

  { Iterate over the last N - 1 elements }
  for I := 1 to FCount - 1 do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, FArray[I]);
  end;
end;

function TArraySet<T>.All(const APredicate: TPredicate<T>): Boolean;
var
  I: NativeInt;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if FCount > 0 then
    for I := 0 to FCount - 1 do
      if not APredicate(FArray[I]) then
        Exit(false);

  Result := true;
end;

function TArraySet<T>.Any(const APredicate: TPredicate<T>): Boolean;
var
  I: NativeInt;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if FCount > 0 then
    for I := 0 to FCount - 1 do
      if APredicate(FArray[I]) then
        Exit(true);

  Result := false;
end;

function TArraySet<T>.BinarySearch(const AElement: T): NativeInt;
var
  LLeft, LRight, LMiddle: NativeInt;
  LCompareResult: NativeInt;
begin
  Result := -1;

  { Optimized cases }
  if FCount = 0 then
    Exit;

  if (FCount = 1) and (CompareElements(FArray[0], AElement) = 0) then
    Exit(0);

  { The actual binary search }

  LLeft := 0;
  LRight := LLeft + FCount - 1;

  while (LLeft <= LRight) do
  begin
    LMiddle := (LLeft + LRight) div 2;
    LCompareResult := CompareElements(FArray[LMiddle], AElement) * FSignFix;

    if LCompareResult > 0 then
      LRight := LMiddle - 1
    else if LCompareResult < 0 then
      LLeft := LMiddle + 1
    else begin
      Result := LMiddle;
      Exit;
    end;
  end;
end;

procedure TArraySet<T>.Clear;
var
  I: NativeInt;
begin
  { Reset the length }
  if FCount > 0 then
  begin
    for I := 0 to FCount - 1 do
      NotifyElementRemoved(FArray[I]);

    NotifyCollectionChanged();
    FCount := 0;
  end;
end;

function TArraySet<T>.Contains(const AValue: T): Boolean;
begin
  Result := BinarySearch(AValue) > -1;
end;

procedure TArraySet<T>.CopyTo(var AArray: array of T; const AStartIndex: NativeInt);
var
  I: NativeInt;
begin
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  { Check for indexes }
  if (Length(AArray) - AStartIndex) < FCount then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Copy all elements safely }
  for I := 0 to FCount - 1 do
    AArray[AStartIndex + I] := FArray[I];
end;

constructor TArraySet<T>.Create(const ARules: TRules<T>);
begin
  Create(ARules, CDefaultSize, True);
end;

constructor TArraySet<T>.Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt; const AAscending: Boolean);
begin
  inherited Create(ARules);

  if AAscending then
    FSignFix := 1
  else
    FSignFix := -1;

  if AInitialCapacity <= 0 then
    SetLength(FArray, 0)
  else
    SetLength(FArray, AInitialCapacity);
end;

constructor TArraySet<T>.Create;
begin
  Create(TRules<T>.Default, CDefaultSize, True);
end;

function TArraySet<T>.ElementAt(const AIndex: NativeInt): T;
begin
  { Simply use the getter }
  if (AIndex >= FCount) or (AIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');

  Result := FArray[AIndex];
end;

function TArraySet<T>.ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T;
begin
  if AIndex < 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');

  if AIndex >= FCount then
     Result := ADefault
  else
     Result := FArray[AIndex];
end;

function TArraySet<T>.Empty: Boolean;
begin
  Result := FCount = 0;
end;

function TArraySet<T>.EqualsTo(const ACollection: IEnumerable<T>): Boolean;
var
  LValue: T;
  I: NativeInt;
begin
  I := 0;

  for LValue in ACollection do
  begin
    if I >= FCount then
      Exit(false);

    if not ElementsAreEqual(FArray[I], LValue) then
      Exit(false);

    Inc(I);
  end;

  if I < FCount then
    Exit(false);

  Result := true;
end;

function TArraySet<T>.First: T;
begin
  { Check length }
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[0];
end;

function TArraySet<T>.FirstOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FCount = 0 then
    Result := ADefault
  else
    Result := FArray[0];
end;

function TArraySet<T>.GetCapacity: NativeInt;
begin
  Result := Length(FArray);
end;

function TArraySet<T>.GetCount: NativeInt;
begin
  Result := FCount;
end;

function TArraySet<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create an enumerator }
  Result := TEnumerator.Create(Self);
end;

procedure TArraySet<T>.Grow;
begin
  { Grow the array }
  if FCount < CDefaultSize then
    SetLength(FArray, FCount + CDefaultSize)
  else
    SetLength(FArray, FCount * 2);
end;

function TArraySet<T>.Last: T;
begin
  { Check length }
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[FCount - 1];
end;

function TArraySet<T>.LastOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FCount = 0 then
    Result := ADefault
  else
    Result := FArray[FCount - 1];
end;

function TArraySet<T>.Max: T;
var
  I: NativeInt;
begin
  { Check length }
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Optimized get }
  if FSignFix = 1 then
    Result := FArray[FCount - 1]
  else
    Result := FArray[0];
end;

function TArraySet<T>.Min: T;
var
  I: NativeInt;
begin
  { Check length }
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Optimized get }
  if FSignFix = 1 then
    Result := FArray[0]
  else
    Result := FArray[FCount - 1];
end;

procedure TArraySet<T>.Remove(const AValue: T);
var
  I, LFoundIndex: NativeInt;
begin
  { Defaults }
  LFoundIndex := BinarySearch(AValue);
  if LFoundIndex > -1 then
  begin
    { Move the list }
    for I := LFoundIndex to FCount - 2 do
      FArray[I] := FArray[I + 1];

    Dec(FCount);
    NotifyCollectionChanged();
  end;
end;

procedure TArraySet<T>.Shrink;
begin
  { Cut the capacity if required }
  if FCount < Capacity then
    SetLength(FArray, FCount);
end;

function TArraySet<T>.Single: T;
begin
  { Check length }
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError()
  else if FCount > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FArray[0];
end;

function TArraySet<T>.SingleOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FCount = 0 then
    Result := ADefault
  else if FCount > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FArray[0];
end;

{ TArraySet<T>.TEnumerator }

function TArraySet<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  with TArraySet<T>(Owner) do
  begin
    Result := FCurrentIndex < FCount;

    if Result then
    begin
      ACurrent := FArray[FCurrentIndex];
      Inc(FCurrentIndex);
    end;
  end;
end;

{ TObjectArraySet<T> }

procedure TObjectArraySet<T>.HandleElementRemoved(const AElement: T);
begin
  if FOwnsObjects then
    TObject(AElement).Free;
end;

{$IF CompilerVersion > 21}

{ TBitSet }

procedure TBitSet.Add(const AValue: Word);
var
  LPage, LBit, LMask: NativeInt;
  LOldLength: NativeInt;
begin
  { Caculate the position of the bit }
  LPage := AValue div (CPageSize * 8);
  LBit := AValue mod (CPageSize * 8);
  LMask := 1 shl LBit;

  { Check if the page is mapped }
  LOldLength := Length(FBitArray);
  if LPage >= LOldLength then
  begin
    { We need to extend the bit array to the given page }
    SetLength(FBitArray, LPage + 1);

    { Fill the new part of the array with zeroes }
    FillChar(FBitArray[LOldLength],
      CPageSize * (Length(FBitArray) - LOldLength), 0);
  end else
  begin
    { Verify if the bit was already set, and do nothing if so }
    if (FBitArray[LPage] and LMask) = LMask then
      Exit;
  end;

  { Now, set the bit }
  FBitArray[LPage] := FBitArray[LPage] or LMask;

  { Update internals }
  Inc(FCount);
  NotifyCollectionChanged();
end;

procedure TBitSet.Clear;
var
  LPage, LBit, LCurrent: NativeInt;
  LChanged: Boolean;
begin
  LChanged := False;
  for LPage := 0 to Length(FBitArray) - 1 do
  begin
    LCurrent := FBitArray[LPage];
    if LCurrent <> 0 then
    begin
      { Only process elements on the pages that have them }
      for LBit := 0 to (CPageSize * 8) - 1 do
        if (LCurrent and (1 shl LBit)) <> 0 then
          NotifyElementRemoved(LBit + LPage * CPageSize * 8);

      LChanged := True;
    end;
  end;

  if LChanged then
    NotifyCollectionChanged();

  { Kill array }
  SetLength(FBitArray, 0);
end;

function TBitSet.Contains(const AValue: Word): Boolean;
var
  LPage, LBit, LMask: NativeInt;
begin
  { Caculate the position of the bit }
  LPage := AValue div (CPageSize * 8);
  LBit := AValue mod (CPageSize * 8);

  { Check if the page is mapped }
  if LPage >= Length(FBitArray) then
    Exit(False);

  { The page is mapped, let's check the bit }
  LMask := 1 shl LBit;
  Result := (FBitArray[LPage] and LMask) = LMask;
end;

procedure TBitSet.CopyTo(var AArray: array of Word; const AStartIndex: NativeInt);
var
  LPage, LBit, LCurrent, X: NativeInt;
begin
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  { Check for indexes }
  if (Length(AArray) - AStartIndex) < FCount then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Copy elements }
  X := AStartIndex;
  for LPage := 0 to Length(FBitArray) - 1 do
  begin
    LCurrent := FBitArray[LPage];

    if LCurrent <> 0 then
    begin
      { Only process elements on the pages that have them }
      for LBit := 0 to (CPageSize * 8) - 1 do
        if (LCurrent and (1 shl LBit)) <> 0 then
        begin
          AArray[X] := (LBit + LPage * CPageSize * 8);
          Inc(X);
        end;
    end;
  end;
end;

constructor TBitSet.Create(const ARules: TRules<Word>);
begin
  Create(True);
end;

constructor TBitSet.Create();
begin
  Create(True);
end;

constructor TBitSet.Create(const AAscending: Boolean);
begin
  inherited Create(TRules<Word>.Default);
  FAscending := AAscending;
end;

function TBitSet.Empty: Boolean;
begin
  Result := FCount = 0;
end;

function TBitSet.First: Word;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FirstOrDefault(0);
end;

function TBitSet.FirstOrDefault(const ADefault: Word): Word;
var
  LPage, LBit, LCurrent: NativeInt;
begin
  for LPage := 0 to Length(FBitArray) - 1 do
  begin
    LCurrent := FBitArray[LPage];
    if LCurrent <> 0 then
    begin
      { Only process elements on the pages that have them }
      for LBit := 0 to (CPageSize * 8) - 1 do
        if (LCurrent and (1 shl LBit)) <> 0 then
          Exit(LBit + LPage * CPageSize * 8);
    end;
  end;

  Result := ADefault;
end;

function TBitSet.GetCount: NativeInt;
begin
  Result := FCount;
end;

function TBitSet.GetEnumerator: IEnumerator<Word>;
var
  LEnumerator: TAscendingEnumerator;
begin
  if FAscending then
  begin
    LEnumerator := TAscendingEnumerator.Create(Self);
    LEnumerator.FPageIndex := -1;
  end else begin
    LEnumerator := TDescendingEnumerator.Create(Self);
    LEnumerator.FPageIndex := Length(FBitArray);
  end;

  Result := LEnumerator;
end;

function TBitSet.Last: Word;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := LastOrDefault(0);
end;

function TBitSet.LastOrDefault(const ADefault: Word): Word;
var
  LPage, LBit, LCurrent: NativeInt;
begin
  for LPage := Length(FBitArray) - 1 downto 0 do
  begin
    LCurrent := FBitArray[LPage];
    if LCurrent <> 0 then
    begin
      { Only process elements on the pages that have them }
      for LBit := (CPageSize * 8) - 1 downto 0 do
        if (LCurrent and (1 shl LBit)) <> 0 then
          Exit(LBit + LPage * CPageSize * 8);
    end;
  end;

  Result := ADefault;
end;

function TBitSet.Max: Word;
begin
  if FAscending then
    Result := Last()
  else
    Result := First();
end;

function TBitSet.Min: Word;
begin
  if FAscending then
    Result := First()
  else
    Result := Last();
end;

procedure TBitSet.Remove(const AValue: Word);
var
  LPage, LBit, LMask: NativeInt;
begin
  { Caculate the position of the bit }
  LPage := AValue div (CPageSize * 8);
  LBit := AValue mod (CPageSize * 8);

  { Check if the page is mapped. If the page is not mapped then the element
    is surely not there. }
  if LPage >= Length(FBitArray) then
    Exit;

  LMask := 1 shl LBit;

  { Verify if the bit was already set, and do nothing if so }
  if (FBitArray[LPage] and LMask) = LMask then
    Exit;

  { The page is mapped, let's check the bit }
  FBitArray[LPage] := FBitArray[LPage] and not LMask;

  { Update internals }
  Dec(FCount);
  NotifyCollectionChanged();
end;

function TBitSet.Single: Word;
begin
  { Check length }
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError()
  else if FCount > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement();

  Result := First();
end;

function TBitSet.SingleOrDefault(const ADefault: Word): Word;
begin
  { Check length }
  if FCount > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement();

  if FCount = 0 then
    Result := ADefault
  else
    Result := First();
end;

{ TBitSet.TEnumerator }

function TBitSet.TAscendingEnumerator.TryMoveNext(out ACurrent: Word): Boolean;
begin
  Result := false;

  while True do
  begin
    if FPage = 0 then
    begin
      { Move to the next page, check if it exists, otherwise we're finished }
      Inc(FPageIndex);
      if FPageIndex >= Length(TBitSet(Owner).FBitArray) then
        Break;

      { Reset all the data }
      FBitIndex := 0;
      FPage := TBitSet(Owner).FBitArray[FPageIndex];
    end;

    if (FPage and 1) = 1 then
    begin
      { The value is set }
      ACurrent := FBitIndex + FPageIndex * CPageSize * 8;
      Result := True;
    end;

    Inc(FBitIndex);
    FPage := FPage shr 1;

    if Result then
      Break;
  end;
end;

{ TBitSet.TDescendingEnumerator }

function TBitSet.TDescendingEnumerator.TryMoveNext(out ACurrent: Word): Boolean;
const
  CMask: NativeInt = NativeInt(1) shl ((CPageSize * 8) - 1);
begin
  Result := false;
  while True do
  begin
    if FPage = 0 then
    begin
      { Move to the next page, check if it exists, otherwise we're finished }
      Dec(FPageIndex);
      if FPageIndex < 0 then
        Break;

      { Reset all the data }
      FBitIndex := (CPageSize * 8) - 1;
      FPage := TBitSet(Owner).FBitArray[FPageIndex];
    end;

    if (FPage and CMask) = CMask then
    begin
      { The value is set }
      FValue := FBitIndex + FPageIndex * CPageSize * 8;
      Result := True;
    end;

    Dec(FBitIndex);
    FPage := FPage shl 1;

    if Result then
      Break;
  end;
end;
{$IFEND}

end.

