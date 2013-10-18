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

unit Collections.Stacks;
interface
uses SysUtils,
     Generics.Defaults,
     Generics.Collections,
     Collections.Lists,
     Collections.Base;

type
  ///  <summary>The abstract base class for all generic <c>stack (LIFO)</c> collection.</summary>
  ///  <remarks>Descending classes must implement the required abstract methods and optionally can implement
  ///  the non-required method.</remarks>
  TAbstractStack<T> = class abstract(TCollection<T>, IStack<T>)
  public
    ///  <summary>Creates a new <c>stack</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the set.</param>
    constructor Create(const ARules: TRules<T>);

    ///  <summary>Destroys this instance.</summary>
    ///  <remarks>Do not call this method directly; call <c>Free</c> instead.</remarks>
    destructor Destroy(); override;

    ///  <summary>Pushes an element to the top of the stack.</summary>
    ///  <param name="AValue">The value to push.</param>
    ///  <remarks>The implementation in this class always raises an exception. This implementation calls <c>Add</c> method.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>Add</c> method is not overridden.</exception>
    procedure Push(const AValue: T); virtual;

    ///  <summary>Retrieves the element from the top of the stack.</summary>
    ///  <returns>The value at the top of the stack.</returns>
    ///  <remarks>This method removes the element from the top of the stack. The implementation in this class
    ///  always raises an exception.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">Always raised in current implementation.</exception>
    function Pop(): T; virtual;

    ///  <summary>Reads the element from the top of the stack.</summary>
    ///  <returns>The value at the top of the stack.</returns>
    ///  <remarks>This method does not remove the element from the top of the stack. It merely reads its value. This implementation calls
    ///  Enex <c>Last</c> operation.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function Peek(): T; virtual;
  end;

type
  ///  <summary>The generic <c>stack (LIFO)</c> collection.</summary>
  ///  <remarks>This type uses an internal array to store its values.</remarks>
  TStack<T> = class(TAbstractStack<T>, IDynamic)
  private type
    {$REGION 'Internal Types'}
    { Generic Stack List Enumerator }
    TEnumerator = class(TAbstractEnumerator<T>)
    private
      FCurrentIndex: NativeInt;
    public
      function TryMoveNext(out ACurrent: T): Boolean; override;
    end;
    {$ENDREGION}

  private var
    FArray: TArray<T>;
    FLength: NativeInt;

  protected
    ///  <summary>Returns the number of elements in the stack.</summary>
    ///  <returns>A positive value specifying the number of elements in the stack.</returns>
    function GetCount(): NativeInt; override;

    ///  <summary>Returns the current capacity.</summary>
    ///  <returns>A positive number that specifies the number of elements that the stack can hold before it
    ///  needs to grow again.</returns>
    ///  <remarks>The value of this method is greater than or equal to the amount of elements in the stack. If this value
    ///  is greater than the number of elements, it means that the stack has some extra capacity to operate upon.</remarks>
    function GetCapacity(): NativeInt;
  public
    ///  <summary>Creates a new <c>stack</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>stack</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the set.</param>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Creates a new <c>stack</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the stack.</param>
    ///  <param name="AInitialCapacity">The stack's initial capacity.</param>
    constructor Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt); overload;

    ///  <summary>Clears the contents of the stack.</summary>
    procedure Clear(); override;

    ///  <summary>Pushes an element to the top of the stack.</summary>
    ///  <param name="AValue">The value to push.</param>
    procedure Add(const AValue: T); override;

    ///  <summary>Retrieves the element from the top of the stack.</summary>
    ///  <returns>The value at the top of the stack.</returns>
    ///  <remarks>This method removes the element from the top of the stack.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function Pop(): T; override;

    ///  <summary>Removes an element from the stack.</summary>
    ///  <param name="AValue">The value to remove. If there is no such element in the stack, nothing happens.</param>
    procedure Remove(const AValue: T); override;

    ///  <summary>Checks whether the stack contains a given value.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns><c>True</c> if the value was found in the stack; <c>False</c> otherwise.</returns>
    function Contains(const AValue: T): Boolean; override;

    ///  <summary>Specifies the number of elements in the stack.</summary>
    ///  <returns>A positive value specifying the number of elements in the stack.</returns>
    property Count: NativeInt read FLength;

    ///  <summary>Specifies the current capacity.</summary>
    ///  <returns>A positive number that specifies the number of elements that the stack can hold before it
    ///  needs to grow again.</returns>
    ///  <remarks>The value of this property is greater than or equal to the amount of elements in the stack. If this value
    ///  if greater than the number of elements, it means that the stack has some extra capacity to operate upon.</remarks>
    property Capacity: NativeInt read GetCapacity;

    ///  <summary>Removes the excess capacity from the stack.</summary>
    ///  <remarks>This method can be called manually to force the stack to drop the extra capacity it might hold. For example,
    ///  after performing some massive operations on a big list, call this method to ensure that all extra memory held by the
    ///  stack is released.</remarks>
    procedure Shrink();

    ///  <summary>Forces the stack to increase its capacity.</summary>
    ///  <remarks>Call this method to force the stack to increase its capacity ahead of time. Manually adjusting the capacity
    ///  can be useful in certain situations.</remarks>
    procedure Grow();

    ///  <summary>Returns a new enumerator object used to enumerate this stack.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the stack.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<T>; override;

    ///  <summary>Copies the values stored in the stack to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the stack.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the stack.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Checks whether the stack is empty.</summary>
    ///  <returns><c>True</c> if the stack is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the stack is empty.</remarks>
    function Empty(): Boolean; override;

    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the stack considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function Max(): T; override;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the stack considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function Min(): T; override;

    ///  <summary>Returns the first element.</summary>
    ///  <returns>The first element in the stack.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function First(): T; override;

    ///  <summary>Returns the first element or a default, if the stack is empty.</summary>
    ///  <param name="ADefault">The default value returned if the stack is empty.</param>
    ///  <returns>The first element in the stack if the stack is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function FirstOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the last element.</summary>
    ///  <returns>The last element in the stack.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function Last(): T; override;

    ///  <summary>Returns the last element or a default if the stack is empty.</summary>
    ///  <param name="ADefault">The default value returned if the stack is empty.</param>
    ///  <returns>The last element in stack if the stack is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function LastOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the single element stored in the stack.</summary>
    ///  <returns>The element in the stack.</returns>
    ///  <remarks>This method checks whether the stack contains just one element, in which case it is returned.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionNotOneException">There is more than one element in the stack.</exception>
    function Single(): T; override;

    ///  <summary>Returns the single element stored in the stack or a default value.</summary>
    ///  <param name="ADefault">The default value returned if there are less or more elements in the stack.</param>
    ///  <returns>The element in the stack if the condition is satisfied; <paramref name="ADefault"/> is returned otherwise.</returns>
    ///  <remarks>This method checks whether the stack contains just one element, in which case it is returned. Otherwise
    ///  the value in <paramref name="ADefault"/> is returned.</remarks>
    function SingleOrDefault(const ADefault: T): T; override;

    ///  <summary>Aggregates a value based on the stack's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <returns>A value that contains the stack's aggregated value.</returns>
    ///  <remarks>This method returns the first element if the stack only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation, where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function Aggregate(const AAggregator: TFunc<T, T, T>): T; override;

    ///  <summary>Aggregates a value based on the stack's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <param name="ADefault">The default value returned if the stack is empty.</param>
    ///  <returns>A value that contains the stack's aggregated value. If the stack is empty, <paramref name="ADefault"/> is returned.</returns>
    ///  <remarks>This method returns the first element if the stack only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation, where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T; override;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <returns>The element at the specified position.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    function ElementAt(const AIndex: NativeInt): T; override;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <param name="ADefault">The default value returned if the stack is empty.</param>
    ///  <returns>The element at the specified position if the stack is not empty and the position is not out of bounds; otherwise
    ///  the value of <paramref name="ADefault"/> is returned.</returns>
    function ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T; override;

    ///  <summary>Checks whether at least one element in the stack satisfies a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if at least one element satisfies a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole stack and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>True</c>. The logical equivalent of this operation is "OR".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Any(const APredicate: TPredicate<T>): Boolean; override;

    ///  <summary>Checks that all elements in the stack satisfy a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if all elements satisfy a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole stack and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>False</c>. The logical equivalent of this operation is "AND".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function All(const APredicate: TPredicate<T>): Boolean; override;

    ///  <summary>Checks whether the elements in this stack are equal to the elements in another collection.</summary>
    ///  <param name="ACollection">The collection to compare to.</param>
    ///  <returns><c>True</c> if the collections are equal; <c>False</c> if the collections are different.</returns>
    ///  <remarks>This method checks that each element at position X in this stack is equal to an element at position X in
    ///  the provided collection. If the number of elements in both collections is different, then the collections are considered different.
    ///  Note that the comparison of elements is done using the rule set used by this stack. This means that comparing this collection
    ///  to another one might yeild a different result than comparing the other collection to this one.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function EqualsTo(const ACollection: IEnumerable<T>): Boolean; override;
  end;

  ///  <summary>The generic <c>stack (LIFO)</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses an internal array to store its objects.</remarks>
  TObjectStack<T: class> = class(TStack<T>)
  private
    FOwnsObjects: Boolean;

  protected
    ///  <summary>Frees the object that was removed from the collection.</summary>
    ///  <param name="AElement">The object that was removed from the collection.</param>
    procedure HandleElementRemoved(const AElement: T); override;

  public
    ///  <summary>Specifies whether this stack owns the objects stored in it.</summary>
    ///  <returns><c>True</c> if the stack owns its objects; <c>False</c> otherwise.</returns>
    ///  <remarks>This property controls the way the stack controls the life-time of the stored objects.</remarks>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

type
  ///  <summary>The generic <c>stack (LIFO)</c> collection.</summary>
  ///  <remarks>This type uses a linked list to store its values.</remarks>
  TLinkedStack<T> = class(TAbstractStack<T>)
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

    ///  <summary>Returns the number of elements in the stack.</summary>
    ///  <returns>A positive value specifying the number of elements in the stack.</returns>
    function GetCount(): NativeInt; override;
  public
    ///  <summary>Creates a new <c>stack</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>stack</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the set.</param>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Destroys this instance.</summary>
    ///  <remarks>Do not call this method directly; call <c>Free</c> instead</remarks>
    destructor Destroy(); override;

    ///  <summary>Clears the contents of the stack.</summary>
    procedure Clear(); override;

    ///  <summary>Pushes an element to the top of the stack.</summary>
    ///  <param name="AValue">The value to push.</param>
    procedure Add(const AValue: T); override;

    ///  <summary>Retrieves the element from the top of the stack.</summary>
    ///  <returns>The value at the top of the stack.</returns>
    ///  <remarks>This method removes the element from the top of the stack.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function Pop(): T; override;

    ///  <summary>Removes an element from the stack.</summary>
    ///  <param name="AValue">The value to remove. If there is no such element in the stack, nothing happens.</param>
    procedure Remove(const AValue: T); override;

    ///  <summary>Checks whether the stack contains a given value.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns><c>True</c> if the value was found in the stack; <c>False</c> otherwise.</returns>
    function Contains(const AValue: T): Boolean; override;

    ///  <summary>Specifies the number of elements in the stack.</summary>
    ///  <returns>A positive value specifying the number of elements in the stack.</returns>
    property Count: NativeInt read FCount;

    ///  <summary>Returns a new enumerator object used to enumerate this stack.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the stack.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<T>; override;

    ///  <summary>Copies the values stored in the stack to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the stack.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the stack.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Checks whether the stack is empty.</summary>
    ///  <returns><c>True</c> if the stack is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the stack is empty.</remarks>
    function Empty(): Boolean; override;

    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the stack considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function Max(): T; override;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the stack considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function Min(): T; override;

    ///  <summary>Returns the first element.</summary>
    ///  <returns>The first element in the stack.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function First(): T; override;

    ///  <summary>Returns the first element or a default if the stack is empty.</summary>
    ///  <param name="ADefault">The default value returned if the stack is empty.</param>
    ///  <returns>The first element in stack if the stack is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function FirstOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the last element.</summary>
    ///  <returns>The last element in the stack.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function Last(): T; override;

    ///  <summary>Returns the last element or a default, if the stack is empty.</summary>
    ///  <param name="ADefault">The default value returned if the stack is empty.</param>
    ///  <returns>The last element in the stack if the stack is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function LastOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the single element stored in the stack.</summary>
    ///  <returns>The element in the stack.</returns>
    ///  <remarks>This method checks whether the stack contains just one element, in which case it is returned.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionNotOneException">There is more than one element in the stack.</exception>
    function Single(): T; override;

    ///  <summary>Returns the single element stored in the stack, or a default value.</summary>
    ///  <param name="ADefault">The default value returned if there are less or more elements in the stack.</param>
    ///  <returns>The element in the stack if the condition is satisfied; <paramref name="ADefault"/> is returned otherwise.</returns>
    ///  <remarks>This method checks if the stack contains just one element, in which case it is returned. Otherwise
    ///  the value in <paramref name="ADefault"/> is returned.</remarks>
    function SingleOrDefault(const ADefault: T): T; override;

    ///  <summary>Aggregates a value based on the stack's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <returns>A value that contains the stack's aggregated value.</returns>
    ///  <remarks>This method returns the first element if the stack only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation, where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function Aggregate(const AAggregator: TFunc<T, T, T>): T; override;

    ///  <summary>Aggregates a value based on the stack's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <param name="ADefault">The default value returned if the stack is empty.</param>
    ///  <returns>A value that contains the stack's aggregated value. If the stack is empty, <paramref name="ADefault"/> is returned.</returns>
    ///  <remarks>This method returns the first element if the stack only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation, where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T; override;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <returns>The element at the specified position.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    function ElementAt(const AIndex: NativeInt): T; override;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <param name="ADefault">The default value returned if the stack is empty.</param>
    ///  <returns>The element at the specified position if the stack is not empty and the position is not out of bounds; otherwise
    ///  the value of <paramref name="ADefault"/> is returned.</returns>
    function ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T; override;

    ///  <summary>Checks whether at least one element in the stack satisfies a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if at least one element satisfies a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole stack and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>True</c>. The logical equivalent of this operation is "OR".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Any(const APredicate: TPredicate<T>): Boolean; override;

    ///  <summary>Checks that all elements in the stack satisfy a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if all elements satisfy a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole stack and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>False</c>. The logical equivalent of this operation is "AND".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function All(const APredicate: TPredicate<T>): Boolean; override;

    ///  <summary>Checks whether the elements in this stack are equal to the elements in another collection.</summary>
    ///  <param name="ACollection">The collection to compare to.</param>
    ///  <returns><c>True</c> if the collections are equal; <c>False</c> if the collections are different.</returns>
    ///  <remarks>This method checks that each element at position X in this stack is equal to an element at position X in
    ///  the provided collection. If the number of elements in both collections is different, then the collections are considered different.
    ///  Note that the comparison of elements is done using the rule set used by this stack. This means that comparing this collection
    ///  to another one might yeild a different result than comparing the other collection to this one.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function EqualsTo(const ACollection: IEnumerable<T>): Boolean; override;
  end;

  ///  <summary>The generic <c>stack (LIFO)</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a linked list to store its objects.</remarks>
  TObjectLinkedStack<T: class> = class(TLinkedStack<T>)
  private
    FOwnsObjects: Boolean;

  protected
    ///  <summary>Frees the object that was removed from the collection.</summary>
    ///  <param name="AElement">The object that was removed from the collection.</param>
    procedure HandleElementRemoved(const AElement: T); override;

  public
    ///  <summary>Specifies whether this stack owns the objects stored in it.</summary>
    ///  <returns><c>True</c> if the stack owns its objects; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the stack controls the life-time of the stored objects.</remarks>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

implementation

{ TAbstractStack<T> }

constructor TAbstractStack<T>.Create(const ARules: TRules<T>);
begin
  inherited Create(ARules);
end;

destructor TAbstractStack<T>.Destroy;
begin
  Clear();
  inherited;
end;

function TAbstractStack<T>.Peek: T;
begin
  Result := Last();
end;

function TAbstractStack<T>.Pop: T;
begin
  ExceptionHelper.Throw_OperationNotSupported('Pop');
end;

procedure TAbstractStack<T>.Push(const AValue: T);
begin
  Add(AValue);
end;

{ TStack<T> }

function TStack<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
var
  I: NativeInt;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := FArray[0];

  { Iterate over the last N - 1 elements }
  for I := 1 to FLength - 1 do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, FArray[I]);
  end;
end;

function TStack<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
var
  I: NativeInt;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if FLength = 0 then
    Exit(ADefault);

  { Select the first element as comparison base }
  Result := FArray[0];

  { Iterate over the last N - 1 elements }
  for I := 1 to FLength - 1 do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, FArray[I]);
  end;
end;

function TStack<T>.All(const APredicate: TPredicate<T>): Boolean;
var
  I: NativeInt;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if FLength > 0 then
    for I := 0 to FLength - 1 do
      if not APredicate(FArray[I]) then
        Exit(false);

  Result := true;
end;

function TStack<T>.Any(const APredicate: TPredicate<T>): Boolean;
var
  I: NativeInt;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if FLength > 0 then
    for I := 0 to FLength - 1 do
      if APredicate(FArray[I]) then
        Exit(true);

  Result := false;
end;

procedure TStack<T>.Clear;
var
  I: NativeInt;
begin
  if FLength > 0 then
  begin
    for I := 0 to FLength - 1 do
      NotifyElementRemoved(FArray[I]);

    { Simply reset all to default }
    FLength := 0;
    SetLength(FArray, 0);
    NotifyCollectionChanged();
  end;
end;

function TStack<T>.Contains(const AValue: T): Boolean;
var
  I: NativeInt;
begin
  { Defaults }
  Result := False;
  if (FLength = 0) then Exit;

  for I := 0 to FLength - 1 do
  begin
    if ElementsAreEqual(FArray[I], AValue) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TStack<T>.CopyTo(var AArray: array of T; const AStartIndex: NativeInt);
var
  I: NativeInt;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < FLength then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Copy all elements safely }
  for I := 0 to FLength - 1 do
    AArray[AStartIndex + I] := FArray[I];
end;

constructor TStack<T>.Create(const ARules: TRules<T>);
begin
  Create(ARules, CDefaultSize);
end;

constructor TStack<T>.Create();
begin
  Create(TRules<T>.Default, CDefaultSize);
end;

constructor TStack<T>.Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt);
begin
  { Initialize instance }
  inherited Create(ARules);

  if AInitialCapacity <= 0 then
    SetLength(FArray, 0)
  else
    SetLength(FArray, AInitialCapacity);
end;

function TStack<T>.ElementAt(const AIndex: NativeInt): T;
begin
  if (AIndex >= FLength) or (AIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');

  Result := FArray[AIndex];
end;

function TStack<T>.ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T;
begin
  { Check range }
  if AIndex < 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');

  if (AIndex >= FLength) then
     Result := ADefault
  else
     Result := FArray[AIndex];
end;

function TStack<T>.Empty: Boolean;
begin
  Result := (FLength = 0);
end;

function TStack<T>.EqualsTo(const ACollection: IEnumerable<T>): Boolean;
var
  LValue: T;
  I: NativeInt;
begin
  I := 0;

  for LValue in ACollection do
  begin
    if I >= FLength then
      Exit(false);

    if not ElementsAreEqual(FArray[I], LValue) then
      Exit(false);

    Inc(I);
  end;

  if I < FLength then
    Exit(false);

  Result := true;
end;

function TStack<T>.First: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[0];
end;

function TStack<T>.FirstOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else
    Result := FArray[0];
end;

function TStack<T>.GetCapacity: NativeInt;
begin
  Result := Length(FArray);
end;

function TStack<T>.GetCount: NativeInt;
begin
  { Use the variable }
  Result := FLength;
end;

function TStack<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TStack<T>.Grow;
var
  LNewCapacity: NativeInt;
begin
  if Capacity = 0 then
    LNewCapacity := CDefaultSize
  else
    LNewCapacity := Capacity * 2;

  SetLength(FArray, LNewCapacity);
end;

function TStack<T>.Last: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[FLength - 1];
end;

function TStack<T>.LastOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else
    Result := FArray[FLength - 1];
end;

function TStack<T>.Max: T;
var
  I: NativeInt;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Default one }
  Result := FArray[0];

  for I := 1 to FLength - 1 do
    if CompareElements(FArray[I], Result) > 0 then
      Result := FArray[I];
end;

function TStack<T>.Min: T;
var
  I: NativeInt;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Default one }
  Result := FArray[0];

  for I := 1 to FLength - 1 do
    if CompareElements(FArray[I], Result) < 0 then
      Result := FArray[I];
end;

function TStack<T>.Pop: T;
begin
  if FLength > 0 then
  begin
    Result := FArray[FLength - 1];

    Dec(FLength);
    NotifyCollectionChanged();
  end else
    ExceptionHelper.Throw_CollectionEmptyError();
end;

procedure TStack<T>.Add(const AValue: T);
begin
  { Ensure enough capacity }
  if (FLength >= Capacity) then
    Grow();

  { Add the element to the stack and increase the index }
  FArray[FLength] := AValue;
  Inc(FLength);
  NotifyCollectionChanged();
end;

procedure TStack<T>.Remove(const AValue: T);
var
  I, LFoundIndex: NativeInt;
begin
  { Defaults }
  if (FLength = 0) then Exit;
  LFoundIndex := -1;

  for I := 0 to FLength - 1 do
  begin
    if ElementsAreEqual(FArray[I], AValue) then
    begin
      LFoundIndex := I;
      Break;
    end;
  end;

  if (LFoundIndex > -1) then
  begin
    { Move the list }
    if FLength > 1 then
      for I := LFoundIndex to FLength - 2 do
        FArray[I] := FArray[I + 1];

    Dec(FLength);
    NotifyCollectionChanged();
  end;
end;

procedure TStack<T>.Shrink;
begin
  { Cut the capacity if required }
  if FLength < Capacity then
    SetLength(FArray, FLength);
end;

function TStack<T>.Single: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError()
  else if FLength > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FArray[0];
end;

function TStack<T>.SingleOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else if FLength > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FArray[0];
end;

{ TStack<T>.TEnumerator }

function TStack<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  with TStack<T>(Owner) do
  begin
    Result := FCurrentIndex < FLength;

    if Result then
    begin
      ACurrent := FArray[FCurrentIndex];
      Inc(FCurrentIndex);
    end;
  end;
end;

{ TObjectStack<T> }

procedure TObjectStack<T>.HandleElementRemoved(const AElement: T);
begin
  if FOwnsObjects then
    TObject(AElement).Free;
end;

{ TLinkedStack<T> }

function TLinkedStack<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
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

function TLinkedStack<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
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

function TLinkedStack<T>.All(const APredicate: TPredicate<T>): Boolean;
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

function TLinkedStack<T>.Any(const APredicate: TPredicate<T>): Boolean;
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

procedure TLinkedStack<T>.Clear;
var
  LCurrent, LNext: PEntry;
begin
  if Assigned(FFirst) then
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
end;

function TLinkedStack<T>.Contains(const AValue: T): Boolean;
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

procedure TLinkedStack<T>.CopyTo(var AArray: array of T; const AStartIndex: NativeInt);
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

constructor TLinkedStack<T>.Create;
begin
  inherited Create(TRules<T>.Default);
end;

constructor TLinkedStack<T>.Create(const ARules: TRules<T>);
begin
  inherited Create(ARules);
end;

destructor TLinkedStack<T>.Destroy;
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

function TLinkedStack<T>.ElementAt(const AIndex: NativeInt): T;
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

function TLinkedStack<T>.ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T;
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

function TLinkedStack<T>.Empty: Boolean;
begin
  { Call the one from the list }
  Result := not Assigned(FFirst);
end;

function TLinkedStack<T>.EqualsTo(const ACollection: IEnumerable<T>): Boolean;
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

function TLinkedStack<T>.First: T;
begin
  if not Assigned(FFirst) then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FFirst^.FValue;
end;

function TLinkedStack<T>.FirstOrDefault(const ADefault: T): T;
begin
  if not Assigned(FFirst) then
    Result := ADefault
  else
    Result := FFirst^.FValue;
end;

function TLinkedStack<T>.GetCount: NativeInt;
begin
  { Use the variable }
  Result := FCount;
end;

function TLinkedStack<T>.GetEnumerator: IEnumerator<T>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FCurrentEntry := FFirst;
  Result := LEnumerator;
end;

function TLinkedStack<T>.Last: T;
begin
  if not Assigned(FLast) then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FLast^.FValue;
end;

function TLinkedStack<T>.LastOrDefault(const ADefault: T): T;
begin
  if not Assigned(FLast) then
    Result := ADefault
  else
    Result := FLast^.FValue;
end;

function TLinkedStack<T>.Max: T;
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

function TLinkedStack<T>.Min: T;
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

function TLinkedStack<T>.NeedEntry(const AValue: T): PEntry;
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

function TLinkedStack<T>.Pop: T;
var
  LEntry: PEntry;
begin
  if not Assigned(FLast) then
    ExceptionHelper.Throw_CollectionEmptyError();

  LEntry := FLast;
  Result := LEntry^.FValue;
  FLast := LEntry^.FPrev;

  if FFirst = LEntry then
    FFirst := FLast;

  ReleaseEntry(LEntry);

  NotifyCollectionChanged();
  Dec(FCount);
end;

procedure TLinkedStack<T>.Add(const AValue: T);
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

procedure TLinkedStack<T>.ReleaseEntry(const AEntry: PEntry);
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

procedure TLinkedStack<T>.Remove(const AValue: T);
var
  LCurrent: PEntry;
begin
  LCurrent := FFirst;

  while Assigned(LCurrent) do
  begin
    if ElementsAreEqual(AValue, LCurrent^.FValue) then
    begin
      { Remove the node }
      if Assigned(LCurrent^.FPrev) then
        LCurrent^.FPrev^.FNext := LCurrent^.FNext;
      if Assigned(LCurrent^.FNext) then
        LCurrent^.FNext^.FPrev := LCurrent^.FPrev;
      if FFirst = LCurrent then
        FFirst := LCurrent^.FNext;
      if FLast = LCurrent then
        FLast := LCurrent^.FPrev;

      ReleaseEntry(LCurrent);
      NotifyCollectionChanged();
      Dec(FCount);
      Exit;
    end;

    LCurrent := LCurrent^.FNext;
  end;
end;

function TLinkedStack<T>.Single: T;
begin
  { Check length }
  if not Assigned(FFirst) then
    ExceptionHelper.Throw_CollectionEmptyError()
  else if FFirst <> FLast then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FFirst^.FValue;
end;

function TLinkedStack<T>.SingleOrDefault(const ADefault: T): T;
begin
  { Check length }
  if not Assigned(FFirst) then
    Result := ADefault
  else if FFirst <> FLast then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FFirst^.FValue;
end;

{ TLinkedStack<T>.TEnumerator }

function TLinkedStack<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  Result := Assigned(FCurrentEntry);
  if Result then
  begin
    ACurrent := FCurrentEntry^.FValue;
    FCurrentEntry := FCurrentEntry^.FNext;
  end;
end;

{ TObjectLinkedStack<T> }

procedure TObjectLinkedStack<T>.HandleElementRemoved(const AElement: T);
begin
  if FOwnsObjects then
    TObject(AElement).Free;
end;

end.
