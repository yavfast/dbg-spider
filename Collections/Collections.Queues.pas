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

unit Collections.Queues;
interface
uses SysUtils,
     Generics.Defaults,
     Generics.Collections,
     Collections.Lists,
     Collections.Base;

type
  ///  <summary>The abstract base class for all generic <c>queue</c> collections.</summary>
  ///  <remarks>Descending classes must implement the required abstract methods and optionally can implement
  ///  the non-required method.</remarks>
  TAbstractQueue<T> = class(TCollection<T>, IQueue<T>)
  public
    ///  <summary>Creates a new <c>queue</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the queue.</param>
    constructor Create(const ARules: TRules<T>);

    ///  <summary>Appends an element to the head of the queue.</summary>
    ///  <param name="AValue">The value to append.</param>
    ///  <remarks>This implementation calls the <c>Add<c> method.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>Add</c> method is not overridden.</exception>
    procedure Enqueue(const AValue: T);

    ///  <summary>Reads the element from the bottom of the queue.</summary>
    ///  <returns>The value at the bottom of the queue.</returns>
    ///  <remarks>This method does not remove the element from the bottom of the queue. It merely reads it's value.
    ///  This implementation uses Enex <c>First</c> operation.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Peek(): T; virtual;

    ///  <summary>Retrieves the element from the head of the queue.</summary>
    ///  <returns>The value at the head of the queue.</returns>
    ///  <remarks>This method removes the element from the top of the queue. The implementation in this class
    ///  always raises an exception.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">Always raised in current implementation.</exception>
    function Dequeue(): T; virtual;
  end;

  ///  <summary>The generic <c>queue</c> collection.</summary>
  ///  <remarks>This type uses an internal array to store its values.</remarks>
  TQueue<T> = class(TAbstractQueue<T>, IDynamic)
  private type
    {$REGION 'Internal Types'}
    TEnumerator = class(TAbstractEnumerator<T>)
    private
      FLeftCount, FCurrentHead: NativeInt;
    public
      function TryMoveNext(out ACurrent: T): Boolean; override;
    end;
    {$ENDREGION}

  private var
    FHead: NativeInt;
    FTail: NativeInt;
    FLength: NativeInt;
    FArray: TArray<T>;

    procedure SetCapacity(const ANewCapacity : NativeInt);
  protected
    ///  <summary>Returns the number of elements in the queue.</summary>
    ///  <returns>A positive value specifying the number of elements in the queue.</returns>
    function GetCount(): NativeInt; override;

    ///  <summary>Returns the current capacity.</summary>
    ///  <returns>A positive number that specifies the number of elements that the queue can hold before it
    ///  needs to grow again.</returns>
    ///  <remarks>The value of this method is greater than or equal to the amount of elements in the queue. If this value
    ///  is greater than the number of elements, it means that the queue has some extra capacity to operate upon.</remarks>
    function GetCapacity(): NativeInt;
  public
    ///  <summary>Creates a new <c>queue</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>queue</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the queue.</param>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Creates a new <c>queue</c> collection.</summary>
    ///  <param name="AInitialCapacity">The set's initial capacity.</param>
    ///  <param name="ARules">A rule set describing the elements in the queue.</param>
    constructor Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt); overload;

    ///  <summary>Clears the contents of the queue.</summary>
    procedure Clear(); override;

    ///  <summary>Appends an element to the top of the queue.</summary>
    ///  <param name="AValue">The value to append.</param>
    procedure Add(const AValue: T); override;

    ///  <summary>Retrieves the element from the bottom of the queue.</summary>
    ///  <returns>The value at the bottom of the queue.</returns>
    ///  <remarks>This method removes the element from the bottom of the queue.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Dequeue(): T; override;

    ///  <summary>Checks whether the queue contains a given value.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns><c>True</c> if the value was found in the queue; <c>False</c> otherwise.</returns>
    function Contains(const AValue: T): Boolean; override;

    ///  <summary>Specifies the number of elements in the queue.</summary>
    ///  <returns>A positive value specifying the number of elements in the queue.</returns>
    property Count: NativeInt read FLength;

    ///  <summary>Specifies the current capacity.</summary>
    ///  <returns>A positive number that specifies the number of elements that the queue can hold before it
    ///  needs to grow again.</returns>
    ///  <remarks>The value of this property is greater than or equal to the amount of elements in the queue. If this value
    ///  if greater than the number of elements, it means that the queue has some extra capacity to operate upon.</remarks>
    property Capacity: NativeInt read GetCapacity;

    ///  <summary>Removes the excess capacity from the queue.</summary>
    ///  <remarks>This method can be called manually to force the queue to drop the extra capacity it might hold. For example,
    ///  after performing some massive operations on a big list, call this method to ensure that all extra memory held by the
    ///  queue is released.</remarks>
    procedure Shrink();

    ///  <summary>Forces the queue to increase its capacity.</summary>
    ///  <remarks>Call this method to force the queue to increase its capacity ahead of time. Manually adjusting the capacity
    ///  can be useful in certain situations.</remarks>
    procedure Grow();

    ///  <summary>Returns a new enumerator object used to enumerate this queue.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the queue.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<T>; override;

    ///  <summary>Copies the values stored in the queue to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the queue.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the queue.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Checks whether the queue is empty.</summary>
    ///  <returns><c>True</c> if the queue is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the queue is empty.</remarks>
    function Empty(): Boolean; override;

    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the queue considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Max(): T; override;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the queue considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Min(): T; override;

    ///  <summary>Returns the first element.</summary>
    ///  <returns>The first element in the queue.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function First(): T; override;

    ///  <summary>Returns the first element or a default, if the queue is empty.</summary>
    ///  <param name="ADefault">The default value returned if the queue is empty.</param>
    ///  <returns>The first element in the queue if the queue is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function FirstOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the last element.</summary>
    ///  <returns>The last element in the queue.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Last(): T; override;

    ///  <summary>Returns the last element or a default if the queue is empty.</summary>
    ///  <param name="ADefault">The default value returned if the queue is empty.</param>
    ///  <returns>The last element in the queue if the queue is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function LastOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the single element stored in the queue.</summary>
    ///  <returns>The element in the queue.</returns>
    ///  <remarks>This method checks if the queue contains just one element, in which case it is returned.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionNotOneException">There is more than one element in the queue.</exception>
    function Single(): T; override;

    ///  <summary>Returns the single element stored in the queue, or a default value.</summary>
    ///  <param name="ADefault">The default value returned if there are less or more elements in the queue.</param>
    ///  <returns>The element in the queue if the condition is satisfied; <paramref name="ADefault"/> is returned otherwise.</returns>
    ///  <remarks>This method checks if the queue contains just one element, in which case it is returned. Otherwise
    ///  the value in <paramref name="ADefault"/> is returned.</remarks>
    function SingleOrDefault(const ADefault: T): T; override;

    ///  <summary>Aggregates a value based on the queue's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <returns>A value that contains the queue's aggregated value.</returns>
    ///  <remarks>This method returns the first element if the queue only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Aggregate(const AAggregator: TFunc<T, T, T>): T; override;

    ///  <summary>Aggregates a value based on the queue's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <param name="ADefault">The default value returned if the queue is empty.</param>
    ///  <returns>A value that contains the queue's aggregated value. If the queue is empty, <paramref name="ADefault"/> is returned.</returns>
    ///  <remarks>This method returns the first element if the queue only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation, where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T; override;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <returns>The element at the specified position.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    function ElementAt(const AIndex: NativeInt): T; override;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <param name="ADefault">The default value returned if the queue is empty.</param>
    ///  <returns>The element at the specified position if the queue is not empty and the position is not out of bounds; otherwise
    ///  the value of <paramref name="ADefault"/> is returned.</returns>
    function ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T; override;

    ///  <summary>Checks whether at least one element in the queue satisfies a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if at least one element satisfies a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole queue and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>True</c>. The logical equivalent of this operation is "OR".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Any(const APredicate: TPredicate<T>): Boolean; override;

    ///  <summary>Checks that all elements in the queue satisfy a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if all elements satisfy a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole queue and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>False</c>. The logical equivalent of this operation is "AND".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function All(const APredicate: TPredicate<T>): Boolean; override;

    ///  <summary>Checks whether the elements in this queue are equal to the elements in another collection.</summary>
    ///  <param name="ACollection">The collection to compare to.</param>
    ///  <returns><c>True</c> if the collections are equal; <c>False</c> if the collections are different.</returns>
    ///  <remarks>This method checks that each element at position X in this queue is equal to an element at position X in
    ///  the provided collection. If the number of elements in both collections is different, then the collections are considered different.
    ///  Note that the comparisons of elements is done using the rule set used by this queue. This means that comparing this collection
    ///  to another one might yeild a different result than comparing the other collection to this one.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function EqualsTo(const ACollection: IEnumerable<T>): Boolean; override;
  end;

  ///  <summary>The generic <c>queue</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses an internal array to store its objects.</remarks>
  TObjectQueue<T: class> = class(TQueue<T>)
  private
    FOwnsObjects: Boolean;

  protected
    ///  <summary>Frees the object that was removed from the collection.</summary>
    ///  <param name="AElement">The object that was removed from the collection.</param>
    procedure HandleElementRemoved(const AElement: T); override;

  public
    ///  <summary>Specifies whether this queue owns the objects stored in it.</summary>
    ///  <returns><c>True</c> if the queue owns its objects; <c>False</c> otherwise.</returns>
    ///  <remarks>This property controls the way the queue controls the life-time of the stored objects.</remarks>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

type
  ///  <summary>The generic <c>queue</c> collection.</summary>
  ///  <remarks>This type uses a linked list to store its values.</remarks>
  TLinkedQueue<T> = class(TAbstractQueue<T>)
  private type
    {$REGION 'Internal Types'}
    PEntry = ^TEntry;
    TEntry = record
      FPrev, FNext: PEntry;
      FValue: T;
    end;

    TEnumerator = class(TAbstractEnumerator<T>)
    private
      FCurrentEntry: PEntry;
    public
      function TryMoveNext(out ACurrent: T): Boolean; override;
    end;
    {$ENDREGION}

  private var
    FFirst, FLast, FFirstFree: PEntry;
    FCount, FFreeCount: NativeInt;

    { Caching }
    function NeedEntry(const AValue: T): PEntry;
    procedure ReleaseEntry(const AEntry: PEntry);
  protected
    ///  <summary>Returns the number of elements in the queue.</summary>
    ///  <returns>A positive value specifying the number of elements in the queue.</returns>
    function GetCount(): NativeInt; override;
  public
    ///  <summary>Creates a new <c>queue</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>queue</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the queue.</param>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Destroys this instance.</summary>
    ///  <remarks>Do not call this method directly; call <c>Free</c> instead.</remarks>
    destructor Destroy(); override;

    ///  <summary>Clears the contents of the queue.</summary>
    procedure Clear(); override;

    ///  <summary>Appends an element to the top of the queue.</summary>
    ///  <param name="AValue">The value to append.</param>
    procedure Add(const AValue: T); override;

    ///  <summary>Retrieves the element from the bottom of the queue.</summary>
    ///  <returns>The value at the bottom of the queue.</returns>
    ///  <remarks>This method removes the element from the bottom of the queue.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Dequeue(): T; override;

    ///  <summary>Checks whether the queue contains a given value.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns><c>True</c> if the value was found in the queue; <c>False</c> otherwise.</returns>
    function Contains(const AValue: T): Boolean; override;

    ///  <summary>Specifies the number of elements in the queue.</summary>
    ///  <returns>A positive value specifying the number of elements in the queue.</returns>
    property Count: NativeInt read FCount;

    ///  <summary>Returns a new enumerator object used to enumerate this queue.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the queue.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<T>; override;

    ///  <summary>Copies the values stored in the queue to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the queue.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the queue.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Checks whether the queue is empty.</summary>
    ///  <returns><c>True</c> if the queue is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the queue is empty.</remarks>
    function Empty(): Boolean; override;

    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the queue considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Max(): T; override;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the queue considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Min(): T; override;

    ///  <summary>Returns the first element.</summary>
    ///  <returns>The first element in the queue.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function First(): T; override;

    ///  <summary>Returns the first element or a default, if the queue is empty.</summary>
    ///  <param name="ADefault">The default value returned if the queue is empty.</param>
    ///  <returns>The first element in the queue if the queue is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function FirstOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the last element.</summary>
    ///  <returns>The last element in the queue.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Last(): T; override;

    ///  <summary>Returns the last element or a default, if the queue is empty.</summary>
    ///  <param name="ADefault">The default value returned if the queue is empty.</param>
    ///  <returns>The last element in queue if the queue is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function LastOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the single element stored in the queue.</summary>
    ///  <returns>The element in the queue.</returns>
    ///  <remarks>This method checks if the queue contains just one element, in which case it is returned.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionNotOneException">There is more than one element in the queue.</exception>
    function Single(): T; override;

    ///  <summary>Returns the single element stored in the queue, or a default value.</summary>
    ///  <param name="ADefault">The default value returned if there are less or more elements in the queue.</param>
    ///  <returns>The element in the queue if the condition is satisfied; <paramref name="ADefault"/> is returned otherwise.</returns>
    ///  <remarks>This method checks if the queue contains just one element, in which case it is returned. Otherwise
    ///  the value in <paramref name="ADefault"/> is returned.</remarks>
    function SingleOrDefault(const ADefault: T): T; override;

    ///  <summary>Aggregates a value based on the queue's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <returns>A value that contains the queue's aggregated value.</returns>
    ///  <remarks>This method returns the first element if the queue only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation, where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Aggregate(const AAggregator: TFunc<T, T, T>): T; override;

    ///  <summary>Aggregates a value based on the queue's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <param name="ADefault">The default value returned if the queue is empty.</param>
    ///  <returns>A value that contains the queue's aggregated value. If the queue is empty, <paramref name="ADefault"/> is returned.</returns>
    ///  <remarks>This method returns the first element if the queue only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation, where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T; override;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <returns>The element at the specified position.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    function ElementAt(const AIndex: NativeInt): T; override;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <param name="ADefault">The default value returned if the queue is empty.</param>
    ///  <returns>The element at the specified position if the queue is not empty and the position is not out of bounds; otherwise
    ///  the value of <paramref name="ADefault"/> is returned.</returns>
    function ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T; override;

    ///  <summary>Checks whether at least one element in the queue satisfies a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if at least one element satisfies a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole queue and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>True</c>. The logical equivalent of this operation is "OR".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Any(const APredicate: TPredicate<T>): Boolean; override;

    ///  <summary>Checks that all elements in the queue satisfy a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if all elements satisfy a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole queue and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>False</c>. The logical equivalent of this operation is "AND".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function All(const APredicate: TPredicate<T>): Boolean; override;

    ///  <summary>Checks whether the elements in this queue are equal to the elements in another collection.</summary>
    ///  <param name="ACollection">The collection to compare to.</param>
    ///  <returns><c>True</c> if the collections are equal; <c>False</c> if the collections are different.</returns>
    ///  <remarks>This method checks that each element at position X in this queue is equal to an element at position X in
    ///  the provided collection. If the number of elements in the collections is different, then the collections are considered different.
    ///  Note that the comparison of elements is done using the rule set used by this queue. This means that comparing this collection
    ///  to another one might yeild a different result than comparing the other collection to this one.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function EqualsTo(const ACollection: IEnumerable<T>): Boolean; override;
  end;

  ///  <summary>The generic <c>queue</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a linked list to store its objects.</remarks>
  TObjectLinkedQueue<T: class> = class(TLinkedQueue<T>)
  private
    FOwnsObjects: Boolean;

  protected
    ///  <summary>Frees the object that was removed from the collection.</summary>
    ///  <param name="AElement">The object that was removed from the collection.</param>
    procedure HandleElementRemoved(const AElement: T); override;

  public
    ///  <summary>Specifies whether this queue owns the objects stored in it.</summary>
    ///  <returns><c>True</c> if the queue owns its objects; <c>False</c> otherwise.</returns>
    ///  <remarks>This property controls the way the queue controls the life-time of the stored objects.</remarks>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

type
  ///  <summary>The generic <c>priority queue</c> collection.</summary>
  ///  <remarks>This collection reorganizes the first-out element based on a given priority.</remarks>
  TPriorityQueue<TPriority, TValue> = class(TAssociation<TPriority, TValue>, IPriorityQueue<TPriority, TValue>, IDynamic)
  private type
    {$REGION 'Internal Types'}
    { Internal storage }
    TPriorityPair = record
      FPriority: TPriority;
      FValue: TValue;
    end;

    TEnumerator = class(TAbstractEnumerator<TPair<TPriority, TValue>>)
    private
      FCurrentIndex: NativeInt;
    public
      function TryMoveNext(out ACurrent: TPair<TPriority, TValue>): Boolean; override;
    end;
    {$ENDREGION}

  private
    FCount: NativeInt;
    FSign: NativeInt;
    FArray: TArray<TPriorityPair>;

    { Used internally to remove items from queue }
    function RemoveAt(const AIndex: NativeInt): TPriorityPair;
  protected
    ///  <summary>Returns the number of elements in the queue.</summary>
    ///  <returns>A positive value specifying the number of elements in the queue.</returns>
    function GetCount(): NativeInt; override;

    ///  <summary>Returns the current capacity.</summary>
    ///  <returns>A positive number that specifies the number of elements that the queue can hold before it
    ///  needs to grow again.</returns>
    ///  <remarks>The value of this method is greater than or equal to the amount of elements in the queue. If this value
    ///  is greater than the number of elements, it means that the queue has some extra capacity to operate upon.</remarks>
    function GetCapacity(): NativeInt;
  public
    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AAscending">Specifies the comparison order of the priorities. The default is <c>True</c>.</param>
    ///  <remarks>The default rule set for the operated type is used.</remarks>
    constructor Create(const AAscending: Boolean = True); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AAscending">Specifies the comparison order of the priorities. The default is <c>True</c>.</param>
    ///  <param name="AInitialCapacity">Specifies the initial capacity of the queue.</param>
    ///  <remarks>The default rule set for the operated type is used.</remarks>
    constructor Create(const AInitialCapacity: NativeInt; const AAscending: Boolean = True); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ACollection">A collection of priority/value pairs to copy elements from.</param>
    ///  <param name="AAscending">Specifies the comparison order of the priorities. The default is <c>True</c>.</param>
    ///  <remarks>The default rule set for the operated type is used.</remarks>
    constructor Create(const ACollection: IEnumerable<TPair<TPriority, TValue>>; const AAscending: Boolean = True); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AArray">An array of priority/value pairs to copy elements from.</param>
    ///  <param name="AAscending">Specifies the comparison order of the priorities. The default is <c>True</c>.</param>
    ///  <remarks>The default rule set for the operated type is used.</remarks>
    constructor Create(const AArray: array of TPair<TPriority, TValue>; const AAscending: Boolean = True); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="APriorityRules">The rule set used for the queues' priorities.</param>
    ///  <param name="AValueRules">The rule set used for the queues' values.</param>
    ///  <param name="AAscending">Specifies the comparison order of the priorities. The default is <c>True</c>.</param>
    constructor Create(const APriorityRules: TRules<TPriority>; const AValueRules: TRules<TValue>;
      const AAscending: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="APriorityRules">The rule set used for the queues' priorities.</param>
    ///  <param name="AValueRules">The rule set used for the queues' values.</param>
    ///  <param name="AAscending">Specifies the comparison order of the priorities. The default is <c>True</c>.</param>
    ///  <param name="AInitialCapacity">Specifies the initial capacity of the queue.</param>
    constructor Create(const APriorityRules: TRules<TPriority>; const AValueRules: TRules<TValue>;
      const AInitialCapacity: NativeInt; const AAscending: Boolean = True); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ACollection">A collection of priority/value pairs to copy elements from.</param>
    ///  <param name="APriorityRules">The rule set used for the queues' priorities.</param>
    ///  <param name="AValueRules">The rule set used for the queues' values.</param>
    ///  <param name="AAscending">Specifies the comparison order of the priorities. The default is <c>True</c>.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    constructor Create(const APriorityRules: TRules<TPriority>; const AValueRules: TRules<TValue>;
      const ACollection: IEnumerable<TPair<TPriority, TValue>>; const AAscending: Boolean = True); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AArray">An array of priority/value pairs to copy elements from.</param>
    ///  <param name="APriorityRules">The rule set used for the queues' priorities.</param>
    ///  <param name="AValueRules">The rule set used for the queues' values.</param>
    ///  <param name="AAscending">Specifies the comparison order of the priorities. The default is <c>True</c>.</param>
    constructor Create(const APriorityRules: TRules<TPriority>; const AValueRules: TRules<TValue>;
      const AArray: array of TPair<TPriority, TValue>; const AAscending: Boolean = True); overload;

    ///  <summary>Destroys this instance.</summary>
    ///  <remarks>Do not call this method directly; call <c>Free</c> instead.</remarks>
    destructor Destroy(); override;

    ///  <summary>Clears the contents of the queue.</summary>
    procedure Clear();

    ///  <summary>Checks whether the queue contains a given value.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <returns><c>True</c> if the value was found in the queue; <c>False</c> otherwise.</returns>
    function Contains(const AValue: TValue): Boolean;

    ///  <summary>Appends an element to the queue with the default priority set to the type's default value.</summary>
    ///  <param name="AValue">The value to append.</param>
    ///  <remarks>Depending on the sorting order specified at creation time, the element is either pushed to the
    ///  front or the tail of the queue.</remarks>
    procedure Enqueue(const AValue: TValue); overload;

    ///  <summary>Appends an element to the queue with the given priority.</summary>
    ///  <param name="AValue">The value to append.</param>
    ///  <param name="APriority">The priority of the value.</param>
    ///  <remarks>This method automatically moves the enqueued value to the correct position using the sorting order
    ///  or the specified priority.</remarks>
    procedure Enqueue(const AValue: TValue; const APriority: TPriority); overload;

    ///  <summary>Retrieves the element from the bottom of the queue.</summary>
    ///  <returns>The value at the bottom of the queue.</returns>
    ///  <remarks>This method removes the element from the bottom of the queue.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Dequeue(): TValue; overload;

    ///  <summary>Reads the element from the bottom of the queue.</summary>
    ///  <returns>The value at the bottom of the queue.</returns>
    ///  <remarks>This method does not remove the element from the bottom of the queue. It merely reads its value.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Peek(): TValue; overload;

    ///  <summary>Specifies the number of elements in the queue.</summary>
    ///  <returns>A positive value specifying the number of elements in the queue.</returns>
    property Count: NativeInt read FCount;

    ///  <summary>Specifies the current capacity.</summary>
    ///  <returns>A positive number that specifies the number of elements that the queue can hold before it
    ///  needs to grow again.</returns>
    ///  <remarks>The value of this property is greater than or equal to the amount of elements in the queue. If this value
    ///  if greater than the number of elements, it means that the queue has some extra capacity to operate upon.</remarks>
    property Capacity: NativeInt read GetCapacity;

    ///  <summary>Returns a new enumerator object used to enumerate this queue.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the queue.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator() : IEnumerator<TPair<TPriority, TValue>>; override;

    ///  <summary>Removes the excess capacity from the queue.</summary>
    ///  <remarks>This method can be called manually to force the queue to drop the extra capacity it might hold. For example,
    ///  after performing some massive operations on a big list, call this method to ensure that all extra memory held by the
    ///  queue is released.</remarks>
    procedure Shrink();

    ///  <summary>Forces the queue to increase its capacity.</summary>
    ///  <remarks>Call this method to force the queue to increase its capacity ahead of time. Manually adjusting the capacity
    ///  can be useful in certain situations.</remarks>
    procedure Grow();

    ///  <summary>Copies the values stored in the queue to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the queue.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the queue.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of TPair<TPriority, TValue>; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Returns the biggest priority associated with an element in the queue.</summary>
    ///  <returns>A priority of an element from the queue considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function MaxKey(): TPriority; override;
  end;

  ///  <summary>The generic <c>priority queue</c> collection designed to store objects.</summary>
  ///  <remarks>This collection reorganizes the first-out element based on a given priority.</remarks>
  TObjectPriorityQueue<TPriority, TValue> = class(TPriorityQueue<TPriority, TValue>)
  private
    FOwnsPriorities, FOwnsValues: Boolean;

  protected
    ///  <summary>Frees the priority (object) that was removed from the queue.</summary>
    ///  <param name="AKey">The priority that was removed from the queue.</param>
    procedure HandleKeyRemoved(const AKey: TPriority); override;

    ///  <summary>Frees the value (object) that was removed from the queue.</summary>
    ///  <param name="AKey">The value that was removed from the queue.</param>
    procedure HandleValueRemoved(const AValue: TValue); override;
  public
    ///  <summary>Specifies whether this queue owns the priorities (if objects).</summary>
    ///  <returns><c>True</c> if the queue owns the priorities; <c>False</c> otherwise.</returns>
    ///  <remarks>This property controls the way the queue controls the life-time of the stored priorities. The value of
    ///  this property has effect only if the priorities are objects, otherwise it is ignored.</remarks>
    property OwnsPriorities: Boolean read FOwnsPriorities write FOwnsPriorities;

    ///  <summary>Specifies whether this queue owns the values.</summary>
    ///  <returns><c>True</c> if the queue owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property controls the way the queue controls the life-time of the stored values. The value of
    ///  this property has effect only if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

implementation

{ TAbstractQueue<T> }

constructor TAbstractQueue<T>.Create(const ARules: TRules<T>);
begin
  inherited Create(ARules);
end;

function TAbstractQueue<T>.Dequeue: T;
begin
  ExceptionHelper.Throw_OperationNotSupported('Dequeue');
end;

procedure TAbstractQueue<T>.Enqueue(const AValue: T);
begin
  Add(AValue);
end;

function TAbstractQueue<T>.Peek: T;
begin
  Result := First();
end;

{ TQueue<T> }

function TQueue<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
var
  I, LH: NativeInt;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := FArray[FHead];

  LH := (FHead + 1) mod Length(FArray);

  for I := 1 to FLength - 1 do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, FArray[LH]);

    { Circulate Head }
    LH := (LH + 1) mod Length(FArray);
  end;
end;

function TQueue<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
var
  I, LH: NativeInt;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if FLength = 0 then
    Exit(ADefault);

  { Select the first element as comparison base }
  Result := FArray[FHead];

  LH := (FHead + 1) mod Length(FArray);

  for I := 1 to FLength - 1 do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, FArray[LH]);

    { Circulate Head }
    LH := (LH + 1) mod Length(FArray);
  end;
end;

function TQueue<T>.All(const APredicate: TPredicate<T>): Boolean;
var
  I, LH: NativeInt;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if FLength > 0 then
  begin
    LH := FHead;
    for I := 0 to FLength - 1 do
    begin
      if not APredicate(FArray[LH]) then
        Exit(false);

      { Circulate Head }
      LH := (LH + 1) mod Length(FArray);
    end;
  end;

  Result := true;
end;

function TQueue<T>.Any(const APredicate: TPredicate<T>): Boolean;
var
  I, LH: NativeInt;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if FLength > 0 then
  begin
    LH := FHead;
    for I := 0 to FLength - 1 do
    begin
      if APredicate(FArray[LH]) then
        Exit(true);

      { Circulate Head }
      LH := (LH + 1) mod Length(FArray);
    end;
  end;

  Result := false;
end;

procedure TQueue<T>.Clear;
var
  LElement: T;
begin
  { If must cleanup, use the dequeue method }
  while Count > 0 do
  begin
    LElement := Dequeue();
    NotifyElementRemoved(LElement);
  end;

  { Clear all internals }
  FTail := 0;
  FHead := 0;
  FLength := 0;

  NotifyCollectionChanged();
end;

function TQueue<T>.Contains(const AValue: T): Boolean;
var
  I: NativeInt;
  LCapacity: NativeInt;
begin
  { Do a look-up in all the queue }
  Result := False;

  I := FHead;
  LCapacity := Length(FArray);

  while I <> FTail do
  begin
    if ElementsAreEqual(FArray[I], AValue) then
    begin
      Result := True;
      Break;
    end;

    { Next + wrap over }
    I := (I + 1) mod LCapacity;
  end;
end;
                 
procedure TQueue<T>.CopyTo(var AArray: array of T; const AStartIndex: NativeInt);
var
  I, X: NativeInt;
  LCapacity: NativeInt;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;
  I := FHead;
  LCapacity := Length(FArray);

  while FTail <> I do
  begin
    { Copy value }
    AArray[X] := FArray[I];

    { Next + wrap over }
    I := (I + 1) mod LCapacity;
    Inc(X);
  end;
end;

constructor TQueue<T>.Create;
begin
  Create(TRules<T>.Default, CDefaultSize);
end;

constructor TQueue<T>.Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt);
begin
  inherited Create(ARules);

  if AInitialCapacity <= 0 then
    SetLength(FArray, 0)
  else
   SetLength(FArray, AInitialCapacity);
end;

constructor TQueue<T>.Create(const ARules: TRules<T>);
begin
  Create(ARules, CDefaultSize);
end;

function TQueue<T>.ElementAt(const AIndex: NativeInt): T;
var
  LH: NativeInt;
begin
  if (AIndex >= FLength) or (AIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');

  LH := (FHead + AIndex) mod Length(FArray);
  Result := FArray[LH];
end;

function TQueue<T>.ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T;
var
  LH: NativeInt;
begin
  if (AIndex >= FLength) or (AIndex < 0) then
    Exit(ADefault);

  LH := (FHead + AIndex) mod Length(FArray);
  Result := FArray[LH];
end;

function TQueue<T>.Empty: Boolean;
begin
  Result := (FLength = 0);
end;

procedure TQueue<T>.Add(const AValue: T);
var
  LNewCapacity: NativeInt;
begin
  { Ensure Capacity }
  if FLength = Length(FArray) then
  begin
    LNewCapacity := Length(FArray) * 2;

    if LNewCapacity < CDefaultSize then
       LNewCapacity := Length(FArray) + CDefaultSize;

    SetCapacity(LNewCapacity);
  end;

  { Place the element to the end of the list }
  FArray[FTail] := AValue;  
  FTail := (FTail + 1) mod Length(FArray);
  
  Inc(FLength);
  NotifyCollectionChanged();
end;

function TQueue<T>.EqualsTo(const ACollection: IEnumerable<T>): Boolean;
var
  LValue: T;
  I, LH: NativeInt;
begin
  I := 0;
  LH := FHead;

  for LValue in ACollection do
  begin
    if I >= FLength then
      Exit(false);

    if not ElementsAreEqual(FArray[LH], LValue) then
      Exit(false);

    LH := (LH + 1) mod Length(FArray);
    Inc(I);
  end;

  if I < FLength then
    Exit(false);

  Result := true;
end;

function TQueue<T>.First: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[FHead];
end;

function TQueue<T>.FirstOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else
    Result := FArray[FHead];
end;

function TQueue<T>.Dequeue: T;
begin
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Get the head }
  Result := FArray[FHead];

  { Circulate Head }
  FHead := (FHead + 1) mod Length(FArray);

  Dec(FLength);
  NotifyCollectionChanged();
end;

function TQueue<T>.GetCapacity: NativeInt;
begin
  Result := Length(FArray);
end;

function TQueue<T>.GetCount: NativeInt;
begin
  Result := FLength;
end;

function TQueue<T>.GetEnumerator: IEnumerator<T>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FCurrentHead := FHead;
  Result := LEnumerator;
end;

procedure TQueue<T>.Grow;
var
  LNewCapacity: NativeInt;
begin
  { Ensure Capacity }
  if FLength = Length(FArray) then
  begin
    LNewCapacity := Length(FArray) * 2;

    if LNewCapacity < CDefaultSize then
       LNewCapacity := Length(FArray) + CDefaultSize;

    SetCapacity(LNewCapacity);
  end;
end;

function TQueue<T>.Last: T;
var
  LT: NativeInt;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  LT := (FTail - 1) mod Length(FArray);
  Result := FArray[LT];
end;

function TQueue<T>.LastOrDefault(const ADefault: T): T;
var
  LT: NativeInt;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else
  begin
    LT := (FTail - 1) mod Length(FArray);
    Result := FArray[LT];
  end;
end;

function TQueue<T>.Max: T;
var
  I, LH: NativeInt;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Default one }
  LH := FHead;
  Result := FArray[LH];

  LH := (LH + 1) mod Length(FArray);

  for I := 1 to FLength - 1 do
  begin
    if CompareElements(FArray[LH], Result) > 0 then
      Result := FArray[I];

    { Circulate Head }
    LH := (LH + 1) mod Length(FArray);
  end;
end;

function TQueue<T>.Min: T;
var
  I, LH: NativeInt;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Default one }
  LH := FHead;
  Result := FArray[LH];

  LH := (LH + 1) mod Length(FArray);

  for I := 1 to FLength - 1 do
  begin
    if CompareElements(FArray[LH], Result) < 0 then
      Result := FArray[I];

    { Circulate Head }
    LH := (LH + 1) mod Length(FArray);
  end;
end;

procedure TQueue<T>.SetCapacity(const ANewCapacity: NativeInt);
var
  LNewArray: TArray<T>;
begin
  { Create new array }
  SetLength(LNewArray, ANewCapacity);

  if (FLength > 0) then
  begin
    if FHead < FTail then
       Move(FArray[FHead], LNewArray[0], FLength * SizeOf(T))
    else
    begin
       Move(FArray[FHead], LNewArray[0], (FLength - FHead) * SizeOf(T));
       Move(FArray[0], LNewArray[Length(FArray) - FHead], FTail * SizeOf(T));
    end;
  end;

  { Switch arrays }
  FArray := LNewArray;
  FTail := FLength;
  FHead := 0;
  NotifyCollectionChanged();
end;

procedure TQueue<T>.Shrink;
begin
  { Ensure Capacity }
  if FLength < Capacity then
    SetCapacity(FLength);
end;

function TQueue<T>.Single: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError()
  else if FLength > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FArray[FHead];
end;

function TQueue<T>.SingleOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else if FLength > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FArray[FHead];
end;

{ TQueue<T>.TEnumerator }

function TQueue<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  with TQueue<T>(Owner) do
  begin
    Result := FLeftCount < FLength;
    if Result then
    begin
      ACurrent := FArray[FCurrentHead];

      { Circulate Head }
      FCurrentHead := (FCurrentHead + 1) mod Length(FArray);
      Inc(FLeftCount);
    end;
  end;
end;

{ TObjectQueue<T> }

procedure TObjectQueue<T>.HandleElementRemoved(const AElement: T);
begin
  if FOwnsObjects then
    TObject(AElement).Free;
end;

{ TLinkedQueue<T> }

function TLinkedQueue<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
var
  LCurrent: PEntry;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if not Assigned(FFirst) then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := FFirst^.FValue;
  LCurrent := FFirst^.FNext;

  { Iterate over the last N - 1 elements }
  while Assigned(LCurrent) do
  begin
    Result := AAggregator(Result, LCurrent^.FValue);
    LCurrent := LCurrent^.FNext;
  end;
end;

function TLinkedQueue<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
var
  LCurrent: PEntry;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  { Select the first element as comparison base }
  if not Assigned(FFirst) then
    Exit(ADefault);

  Result := FFirst^.FValue;
  LCurrent := FFirst^.FNext;

  { Iterate over the last N - 1 elements }
  while Assigned(LCurrent) do
  begin
    Result := AAggregator(Result, LCurrent^.FValue);
    LCurrent := LCurrent^.FNext;
  end;
end;

function TLinkedQueue<T>.All(const APredicate: TPredicate<T>): Boolean;
var
  LCurrent: PEntry;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  LCurrent := FFirst;

  while Assigned(LCurrent) do
  begin
    if not APredicate(LCurrent^.FValue) then
      Exit(false);

    LCurrent := LCurrent^.FNext;
  end;

  Result := true;
end;

function TLinkedQueue<T>.Any(const APredicate: TPredicate<T>): Boolean;
var
  LCurrent: PEntry;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  LCurrent := FFirst;

  while Assigned(LCurrent) do
  begin
    if APredicate(LCurrent^.FValue) then
      Exit(true);

    LCurrent := LCurrent^.FNext;
  end;

  Result := false;
end;

procedure TLinkedQueue<T>.Clear;
var
  LCurrent, LNext: PEntry;
begin
  LCurrent := FFirst;
  while Assigned(LCurrent) do
  begin
    NotifyElementRemoved(LCurrent^.FValue);

    { Release}
    LNext := LCurrent^.FNext;
    ReleaseEntry(LCurrent);
    LCurrent := LNext;
  end;

  FFirst := nil;
  FLast := nil;
  FCount := 0;
  NotifyCollectionChanged();
end;

function TLinkedQueue<T>.Contains(const AValue: T): Boolean;
var
  LCurrent: PEntry;
begin
  LCurrent := FFirst;
  Result := False;

  while Assigned(LCurrent) do
  begin
    if ElementsAreEqual(AValue, LCurrent^.FValue) then
      Exit(True);

    LCurrent := LCurrent^.FNext;
  end;
end;

procedure TLinkedQueue<T>.CopyTo(var AArray: array of T; const AStartIndex: NativeInt);
var
  X: NativeInt;
  LCurrent: PEntry;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < FCount then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;
  LCurrent := FFirst;
  while Assigned(LCurrent) do
  begin
    AArray[X] := LCurrent^.FValue;
    Inc(X);
    LCurrent := LCurrent^.FNext;
  end;
end;

constructor TLinkedQueue<T>.Create;
begin
  Create(TRules<T>.Default);
end;

constructor TLinkedQueue<T>.Create(const ARules: TRules<T>);
begin
  inherited Create(ARules);
end;

function TLinkedQueue<T>.ElementAt(const AIndex: NativeInt): T;
var
  LCurrent: PEntry;
  LIndex: NativeInt;
begin
  { Check range }
  if (AIndex >= FCount) or (AIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');

  LCurrent := FFirst;
  LIndex := 0;
  while Assigned(LCurrent) do
  begin
    if LIndex = AIndex then
      Exit(LCurrent^.FValue);

    LCurrent := LCurrent^.FNext;
    Inc(LIndex);
  end;

  { Should never happen }
  ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');
end;

function TLinkedQueue<T>.ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T;
var
  LCurrent: PEntry;
  LIndex: NativeInt;
begin
  { Check range }
  if AIndex < 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');

  if AIndex >= FCount then
    Exit(ADefault);

  LCurrent := FFirst;
  LIndex := 0;
  while Assigned(LCurrent) do
  begin
    if LIndex = AIndex then
      Exit(LCurrent^.FValue);

    LCurrent := LCurrent^.FNext;
    Inc(LIndex);
  end;

  { Should never happen }
  Result := ADefault;
end;

function TLinkedQueue<T>.Empty: Boolean;
begin
  { Call the one from the list }
  Result := not Assigned(FLast);
end;

procedure TLinkedQueue<T>.Add(const AValue: T);
var
  LNew: PEntry;
begin
  LNew := NeedEntry(AValue);
  LNew^.FPrev := FLast;
  LNew^.FNext := nil;

  if Assigned(FLast) then
    FLast^.FNext := LNew;

  FLast := LNew;

  if not Assigned(FFirst) then
    FFirst := LNew;

  NotifyCollectionChanged();
  Inc(FCount);
end;

function TLinkedQueue<T>.EqualsTo(const ACollection: IEnumerable<T>): Boolean;
var
  LValue: T;
  LCurrent: PEntry;
begin
  LCurrent := FFirst;
  for LValue in ACollection do
  begin
    if not Assigned(LCurrent) then
      Exit(false);

    if not ElementsAreEqual(LCurrent^.FValue, LValue) then
      Exit(false);

    LCurrent := LCurrent^.FNext;
  end;

  Result := not Assigned(LCurrent);
end;

function TLinkedQueue<T>.First: T;
begin
  if not Assigned(FFirst) then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FFirst^.FValue;
end;

function TLinkedQueue<T>.FirstOrDefault(const ADefault: T): T;
begin
  if not Assigned(FFirst) then
    Result := ADefault
  else
    Result := FFirst^.FValue;
end;

destructor TLinkedQueue<T>.Destroy;
var
  LNext: PEntry;
begin
  { Some cleanup }
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

function TLinkedQueue<T>.Dequeue: T;
var
  LEntry: PEntry;
begin
  if not Assigned(FFirst) then
    ExceptionHelper.Throw_CollectionEmptyError();

  LEntry := FFirst;
  Result := LEntry^.FValue;
  FFirst := LEntry^.FNext;

  if FLast = LEntry then
    FLast := FFirst;

  ReleaseEntry(LEntry);

  NotifyCollectionChanged();
  Dec(FCount);
end;

function TLinkedQueue<T>.GetCount: NativeInt;
begin
  Result := FCount;
end;

function TLinkedQueue<T>.GetEnumerator: IEnumerator<T>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FCurrentEntry := FFirst;
  Result := LEnumerator;
end;

function TLinkedQueue<T>.Last: T;
begin
  if not Assigned(FLast) then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FLast^.FValue;
end;

function TLinkedQueue<T>.LastOrDefault(const ADefault: T): T;
begin
  if not Assigned(FLast) then
    Result := ADefault
  else
    Result := FLast^.FValue;
end;

function TLinkedQueue<T>.Max: T;
var
  LCurrent: PEntry;
begin
  if not Assigned(FLast) then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FFirst^.FValue;
  LCurrent := FFirst^.FNext;

  while Assigned(LCurrent) do
  begin
    if CompareElements(LCurrent^.FValue, Result) > 0 then
      Result := LCurrent^.FValue;

    LCurrent := LCurrent^.FNext;
  end;
end;

function TLinkedQueue<T>.Min: T;
var
  LCurrent: PEntry;
begin
  if not Assigned(FLast) then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FFirst^.FValue;
  LCurrent := FFirst^.FNext;

  while Assigned(LCurrent) do
  begin
    if CompareElements(LCurrent^.FValue, Result) < 0 then
      Result := LCurrent^.FValue;

    LCurrent := LCurrent^.FNext;
  end;
end;

function TLinkedQueue<T>.NeedEntry(const AValue: T): PEntry;
begin
  if FFreeCount > 0 then
  begin
    Result := FFirstFree;
    FFirstFree := FFirstFree^.FNext;

    Dec(FFreeCount);
  end else
    Result := AllocMem(SizeOf(TEntry));

  { Initialize the node }
  Result^.FValue := AValue;
end;

procedure TLinkedQueue<T>.ReleaseEntry(const AEntry: PEntry);
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

function TLinkedQueue<T>.Single: T;
begin
  { Check length }
  if not Assigned(FFirst) then
    ExceptionHelper.Throw_CollectionEmptyError()
  else if FFirst <> FLast then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FFirst^.FValue;
end;

function TLinkedQueue<T>.SingleOrDefault(const ADefault: T): T;
begin
  { Check length }
  if not Assigned(FFirst) then
    Result := ADefault
  else if FFirst <> FLast then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FFirst^.FValue;
end;

{ TLinkedQueue<T>.TEnumerator }

function TLinkedQueue<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  Result := Assigned(FCurrentEntry);
  if Result then
  begin
    ACurrent := FCurrentEntry^.FValue;
    FCurrentEntry := FCurrentEntry^.FNext;
  end;
end;

{ TObjectLinkedQueue<T> }

procedure TObjectLinkedQueue<T>.HandleElementRemoved(const AElement: T);
begin
  if FOwnsObjects then
    TObject(AElement).Free;
end;

{ TPriorityQueue<TPriority, TValue> }

procedure TPriorityQueue<TPriority, TValue>.Clear;
var
  I: NativeInt;
begin
  { Cleanup the array }
  for I := 0 to FCount - 1 do
  begin
    NotifyKeyRemoved(FArray[I].FPriority);
    NotifyValueRemoved(FArray[I].FValue);
  end;

  { Dispose of all the stuff }
  NotifyCollectionChanged();
  FCount := 0;
end;

function TPriorityQueue<TPriority, TValue>.Contains(const AValue: TValue): Boolean;
var
  I: NativeInt;
begin
  { Check whether the thing contains what we need }
  if FCount > 0 then
    for I := 0 to FCount - 1 do
      if ValuesAreEqual(FArray[I].FValue, AValue) then
        Exit(true);

  { Nope ... }
  Result := false;
end;

procedure TPriorityQueue<TPriority, TValue>.CopyTo(var AArray: array of TPair<TPriority, TValue>; const AStartIndex: NativeInt);
var
  I: NativeInt;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < FCount then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Copy the stuff in }
  for I := 0 to FCount - 1 do
  begin
    AArray[AStartIndex + I].Key := FArray[I].FPriority;
    AArray[AStartIndex + I].Value := FArray[I].FValue;
  end;
end;

constructor TPriorityQueue<TPriority, TValue>.Create(const AArray: array of TPair<TPriority, TValue>;
  const AAscending: Boolean);
begin
  { Call upper constructor }
  Create(TRules<TPriority>.Default, TRules<TValue>.Default, AArray, AAscending);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(const ACollection: IEnumerable<TPair<TPriority, TValue>>;
  const AAscending: Boolean);
begin
  { Call upper constructor }
  Create(TRules<TPriority>.Default, TRules<TValue>.Default, ACollection, AAscending);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(const AAscending: Boolean);
begin
  { Call upper constructor }
  Create(TRules<TPriority>.Default, TRules<TValue>.Default, CDefaultSize, AAscending);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(
  const APriorityRules: TRules<TPriority>;
  const AValueRules: TRules<TValue>;
  const AArray: array of TPair<TPriority, TValue>;
  const AAscending: Boolean);
var
  I: NativeInt;
begin
  { Call upper constructor }
  Create(APriorityRules, AValueRules, CDefaultSize, AAscending);

  { Copy all items in }
  if Length(AArray) > 0 then
    for I := 0 to Length(AArray) - 1 do
      Enqueue(AArray[I].Value, AArray[I].Key);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(
  const APriorityRules: TRules<TPriority>;
  const AValueRules: TRules<TValue>;
  const AInitialCapacity: NativeInt;
  const AAscending: Boolean);
begin
  { Install types }
  inherited Create(APriorityRules, AValueRules);

  SetLength(FArray, AInitialCapacity);
  FCount := 0;

  if AAscending then
    FSign := 1
  else
    FSign := -1;
end;

constructor TPriorityQueue<TPriority, TValue>.Create(const AInitialCapacity: NativeInt; const AAscending: Boolean);
begin
  { Call upper constructor }
  Create(TRules<TPriority>.Default, TRules<TValue>.Default, AInitialCapacity, AAscending);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(
  const APriorityRules: TRules<TPriority>;
  const AValueRules: TRules<TValue>;
  const ACollection: IEnumerable<TPair<TPriority, TValue>>;
  const AAscending: Boolean);
var
  LValue: TPair<TPriority, TValue>;
begin
  { Call upper constructor }
  Create(APriorityRules, AValueRules, CDefaultSize, AAscending);

  if not Assigned(ACollection) then
     ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Pump in all items }
  for LValue in ACollection do
    Enqueue(LValue.Value, LValue.Key);
end;

constructor TPriorityQueue<TPriority, TValue>.Create(
  const APriorityRules: TRules<TPriority>;
  const AValueRules: TRules<TValue>;
  const AAscending: Boolean);
begin
  { Call upper constructor }
  Create(APriorityRules, AValueRules, CDefaultSize, AAscending);
end;

function TPriorityQueue<TPriority, TValue>.Dequeue: TValue;
var
  LPair: TPriorityPair;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Extract element at position zero (the head) }
  LPair := RemoveAt(0);

  { CLeanup the priority element }
  NotifyKeyRemoved(LPair.FPriority);

  { And return the value }
  Result := LPair.FValue;
  NotifyCollectionChanged();
end;

destructor TPriorityQueue<TPriority, TValue>.Destroy;
begin
  { First clear }
  Clear();

  inherited;
end;

procedure TPriorityQueue<TPriority, TValue>.Enqueue(const AValue: TValue; const APriority: TPriority);
var
  I, X: NativeInt;
begin
  { Grow if required }
  if FCount = Length(FArray) then
    Grow();

  I := FCount;
  Inc(FCount);

  { Move items to new positions }
  while true do
  begin
    if I > 0 then
      X := (I - 1) div 2
    else
      X := 0;

    { Check for exit }
    if (I = 0) or ((CompareKeys(FArray[X].FPriority, APriority) * FSign) > 0) then
      break;

    FArray[I] := FArray[X];
    I := X;
  end;

  { Insert the new item }
  FArray[I].FPriority := APriority;
  FArray[I].FValue := AValue;

  NotifyCollectionChanged();
end;

procedure TPriorityQueue<TPriority, TValue>.Enqueue(const AValue: TValue);
begin
  { Insert with default priority }
  Enqueue(AValue, default(TPriority));
end;

function TPriorityQueue<TPriority, TValue>.GetCapacity: NativeInt;
begin
  Result := Length(FArray);
end;

function TPriorityQueue<TPriority, TValue>.GetCount: NativeInt;
begin
  { Use the FCount }
  Result := FCount;
end;

function TPriorityQueue<TPriority, TValue>.GetEnumerator: IEnumerator<TPair<TPriority, TValue>>;
begin
  { Create an enumerator }
  Result := TEnumerator.Create(Self);
end;

procedure TPriorityQueue<TPriority, TValue>.Grow;
var
  LNewCapacity: NativeInt;
begin
  LNewCapacity := Length(FArray) * 2;

  if LNewCapacity < CDefaultSize then
    LNewCapacity := CDefaultSize;

  { Extend the array }
  SetLength(FArray, LNewCapacity);
end;

function TPriorityQueue<TPriority, TValue>.MaxKey: TPriority;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[0].FPriority;
end;

function TPriorityQueue<TPriority, TValue>.Peek: TValue;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Peek at the element at position zero (the head) }
  Result := FArray[0].FValue;
end;

function TPriorityQueue<TPriority, TValue>.RemoveAt(const AIndex: NativeInt): TPriorityPair;
var
  LTemp: TPriorityPair;
  I, X, LStart: NativeInt;
begin
  { Obtain the item that is removed }
  Result := FArray[AIndex];
  LTemp := FArray[FCount - 1];

  Dec(FCount);

  { Fill in the create hole }
  if (FCount = 0) or (AIndex = FCount) then
    Exit;

  I := AIndex;

  if I > 0 then
    LStart := (I - 1) div 2
  else
    LStart := 0;

  while ((CompareKeys(LTemp.FPriority, FArray[LStart].FPriority) * FSign) > 0) do
  begin
    FArray[I] := FArray[LStart];
    I := LStart;

    if I > 0 then
      LStart := (I - 1) div 2
    else
      LStart := 0;
  end;

  if (I = AIndex) then
  begin
    while (I < (FCount div 2)) do
    begin
      X := (I * 2) + 1;

      if ((X < FCount - 1) and ((CompareKeys(FArray[X].FPriority, FArray[X + 1].FPriority) * FSign) < 0)) then
        Inc(X);

      if ((CompareKeys(FArray[X].FPriority, LTemp.FPriority) * FSign) <= 0) then
          break;

      FArray[I] := FArray[X];
      I := X;
    end;
  end;

  FArray[I] := LTemp;
end;

procedure TPriorityQueue<TPriority, TValue>.Shrink;
begin
  { Remove the excess stuff }
  if FCount < Length(FArray) then
    SetLength(FArray, FCount);
end;

{ TPriorityQueue<TPriority, TValue>.TEnumerator }

function TPriorityQueue<TPriority, TValue>.TEnumerator.TryMoveNext(out ACurrent: TPair<TPriority, TValue>): Boolean;
begin
  with TPriorityQueue<TPriority, TValue>(Owner) do
  begin
    Result := FCurrentIndex < FCount;

    if Result then
    begin
      ACurrent.Key := FArray[FCurrentIndex].FPriority;
      ACurrent.Value := FArray[FCurrentIndex].FValue;
      Inc(FCurrentIndex);
    end;
  end;
end;

{ TObjectPriorityQueue<TPriority, TValue> }

procedure TObjectPriorityQueue<TPriority, TValue>.HandleKeyRemoved(const AKey: TPriority);
begin
  if FOwnsPriorities then
    PObject(@AKey)^.Free;
end;

procedure TObjectPriorityQueue<TPriority, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    PObject(@AValue)^.Free;
end;

end.
