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

unit Collections.Base;
interface
uses
  SysUtils,
  TypInfo,
  Rtti,
  Collections.Dynamic,
  Generics.Collections,
  Generics.Defaults;

{$REGION 'Base Collection Interfaces'}
type
  ///  <summary>The predicate that accepts two input values.</summary>
  ///  <param name="Arg1">The first argument.</param>
  ///  <param name="Arg2">The second argument.</param>
  ///  <returns>A boolean value that indicates the result of the logical predicate.</returns>
  TPredicate<T1, T2> = reference to function(Arg1: T1; Arg2: T2): Boolean;

  TBalanceAct = (baStart, baLeft, baRight, baLoop, baEnd);

  ///  <summary>Base interface describing all enumerators in this package.</summary>
  ///  <remarks><see cref="Collections.Base|IEnumerator&lt;T&gt;">Collections.Base.IEnumerator&lt;T&gt;</see> is implemented by
  ///  all enumerator objects in this package.</remarks>
  IEnumerator<T> = interface
    ///  <summary>Returns the current element of the enumerated collection.</summary>
    ///  <remarks><see cref="Collections.Base|IEnumerator&lt;T&gt;.GetCurrent">Collections.Base.IEnumerator&lt;T&gt;.GetCurrent</see> is the
    ///  getter method for the <see cref="Collections.Base|IEnumerator&lt;T&gt;.Current">Collections.Base.IEnumerator&lt;T&gt;.Current</see>
    ///  property. Use the property to obtain the element instead.</remarks>
    ///  <returns>The current element of the enumerated collection.</returns>
    function GetCurrent(): T;

    ///  <summary>Moves the enumerator to the next element of the collection.</summary>
    ///  <remarks><see cref="Collections.Base|IEnumerator&lt;T&gt;.MoveNext">Collections.Base.IEnumerator&lt;T&gt;.MoveNext</see> is usually
    ///  called by compiler-generated code. Its purpose is to move the "pointer" to the next element in the collection
    ///  (if there are elements left). Also note that many enumerator implementations may throw various exceptions if the
    ///  enumerated collections were changed in the meantime.</remarks>
    ///  <returns><c>True</c> if the enumerator successfully selected the next element; <c>False</c> if there are
    ///  no more elements to be enumerated.</returns>
    function MoveNext(): Boolean;

    ///  <summary>Returns the current element of the traversed collection.</summary>
    ///  <remarks><see cref="Collections.Base|IEnumerator&lt;T&gt;.Current">Collections.Base.IEnumerator&lt;T&gt;.Current</see> can only return a
    ///  valid element if <see cref="Collections.Base|IEnumerator&lt;T&gt;.MoveNext">Collections.Base.IEnumerator&lt;T&gt;.MoveNext</see> was
    ///  priorly called and returned <c>True</c>; otherwise the behavior of this property is undefined. Note that many enumerator implementations
    ///  may throw exceptions if the collection was changed in the meantime.
    ///  </remarks>
    ///  <returns>The current element of the enumerator collection.</returns>
    property Current: T read GetCurrent;
  end;

  ///  <summary>Base interface describing all enumerable collections in this package.</summary>
  ///  <remarks><see cref="Collections.Base|IEnumerable&lt;T&gt;">Collections.Base.IEnumerable&lt;T&gt;</see> is implemented by all
  ///  enumerable collections in this package.</remarks>
  IEnumerable<T> = interface
    ///  <summary>Returns a <see cref="Collections.Base|IEnumerator&lt;T&gt;">Collections.Base.IEnumerator&lt;T&gt;</see> interface that is used
    ///  to enumerate the collection.</summary>
    ///  <remarks><see cref="Collections.Base|IEnumerable&lt;T&gt;.MoveNext">Collections.Base.IEnumerable&lt;T&gt;.MoveNext</see> is usually
    ///  called by compiler-generated code. Its purpose is to create an enumerator object that is used to actually traverse
    ///  the collections.
    ///  Note that many collections generate enumerators that depend on the state of the collection. If the collection is changed
    ///  after the <see cref="Collections.Base|IEnumerator&lt;T&gt;">Collections.Base.IEnumerator&lt;T&gt;</see> had been obtained,
    ///  <see cref="Collections.Base|ECollectionChangedException">Collections.Base.ECollectionChangedException</see> is thrown.</remarks>
    ///  <returns>The <see cref="Collections.Base|IEnumerator&lt;T&gt;">Collections.Base.IEnumerator&lt;T&gt;</see> interface.</returns>
    function GetEnumerator(): IEnumerator<T>;
  end;

  ///  <summary>A special record designed to hold both a comparer and an equality
  ///  comparer. All collections require this type in order to function properly.</summary>
  ///  <remarks>The collection provided in this package provides extended functionality (Enex), which
  ///  implies comparing values in many circumstances, which requires the presence of the comparer.
  ///  Some collections need an additional equality comparer. This type is meant to provide both
  ///  on the need basis.</remarks>
  TRules<T> = record
  private
    FComparer: IComparer<T>;
    FEqComparer: IEqualityComparer<T>;

  public
    ///  <summary>Initializes a rule set with the given comparers.</summary>
    ///  <param name="AComparer">The comparer.</param>
    ///  <param name="AEqualityComparer">The equality comparer.</param>
    ///  <returns>A rule set initialized with the provided comparers.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"> if <paramref name="AComparer"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"> if <paramref name="AEqualityComparer"/> is <c>nil</c>.</exception>
    class function Create(const AComparer: IComparer<T>; const AEqualityComparer: IEqualityComparer<T>): TRules<T>; static;

    ///  <summary>Initializes a rule set with a given custom comparer.</summary>
    ///  <param name="AComparer">The custom comparer.</param>
    ///  <returns>A rule set initialized with the custom comparer.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"> if <paramref name="AComparer"/> is <c>nil</c>.</exception>
    class function Custom(const AComparer: TCustomComparer<T>): TRules<T>; static;

    ///  <summary>Initializes a rule set using default comparers.</summary>
    ///  <returns>A rule set initialized with the default comparers.</returns>
    class function Default: TRules<T>; static;
  end;

  ///  <summary>Base interface inherited by all specific collection interfaces.</summary>
  ///  <remarks>This interface defines a set of traits common to all collections implemented in this package.</remarks>
  IContainer<T> = interface(IEnumerable<T>)
    ///  <summary>Returns the current version of the collection.</summary>
    ///  <returns>An integer value specifying the current "structural version" of the collection.</returns>
    ///  <remarks>This function returns a number that is modified by the implementing collection each time
    ///  the collection changes. This version can be used to identify if a collection has chnaged since last time it was used
    ///  in a specific piece of code.</remarks>
    function Version(): NativeInt;

    ///  <summary>Returns the number of elements in the collection.</summary>
    ///  <returns>A positive value specifying the number of elements in the collection.</returns>
    ///  <remarks>For associative collections such as dictionaries or multimaps, this value represents the
    ///  number of key-value pairs stored in the collection. A call to this method can be costly because some
    ///  collections cannot detect the number of stored elements directly, resorting to enumerating themselves.</remarks>
    function GetCount(): NativeInt;

    ///  <summary>Checks whether the collection is empty.</summary>
    ///  <returns><c>True</c> if the collection is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the collection is empty. It is optimized
    ///  in most collections to offer a fast response.</remarks>
    function Empty(): Boolean;

    ///  <summary>Returns the single element stored in the collection.</summary>
    ///  <returns>The element in the collection.</returns>
    ///  <remarks>This method checks whether the collection contains just one element, in which case it is returned.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionNotOneException">There is more than one element in the collection.</exception>
    function Single(): T;

    ///  <summary>Returns the single element stored in the collection, or a default value.</summary>
    ///  <param name="ADefault">The default value returned if there is less or more elements in the collection.</param>
    ///  <returns>The element in the collection if the condition is satisfied; <paramref name="ADefault"/> is returned otherwise.</returns>
    ///  <remarks>This method checks whether the collection contains just one element, in which case it is returned. Otherwise
    ///  the value in <paramref name="ADefault"/> is returned.</remarks>
    function SingleOrDefault(const ADefault: T): T;

    ///  <summary>Copies the values stored in the collection to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the collection.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the collection.</remarks>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T); overload;

    ///  <summary>Copies the values stored in the collection to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the collection.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the collection.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload;

    ///  <summary>Creates a new Delphi array with the contents of the collection.</summary>
    ///  <remarks>The length of the new array is equal to the value of the <c>Count</c> property.</remarks>
    function ToArray(): TArray<T>;

    ///  <summary>Specifies the number of elements in the collection.</summary>
    ///  <returns>A positive value specifying the number of elements in the collection.</returns>
    ///  <remarks>For associative collections such as dictionaries or multimaps, this value represents the
    ///  number of key-value pairs stored in the collection. Accesing this property can be costly because some
    ///  collections cannot detect the number of stored elements directly, resorting to enumerating themselves.</remarks>
    property Count: NativeInt read GetCount;
  end;

  { Pre-declarations }
  IList<T> = interface;
  ISet<T> = interface;
  IDictionary<TKey, TValue> = interface;
  ISequence<T> = interface;
  IGrouping<TKey, T> = interface;

  ///  <summary>Offers an extended set of Enex operations.</summary>
  ///  <remarks>This type is exposed by Enex collections, and serves simply as a bridge between the interfaces
  ///  and some advanced operations that require parameterized methods. For example, expressions such as
  ///  <c>List.Op.Select&lt;Integer&gt;</c> are based on this type.</remarks>
  TEnexExtOps<T> = record
  private
    FRules: TRules<T>;
    FInstance: Pointer;
    FKeepAlive: IInterface;

  public
    ///  <summary>Represents a "select" operation.</summary>
    ///  <param name="ASelector">A selector method invoked for each element in the collection.</param>
    ///  <param name="ARules">A rule set representing the elements in the output collection.</param>
    ///  <returns>A new collection containing the selected values.</returns>
    ///  <remarks>This method is used when it is required to select values related to the ones in the operated collection.
    ///  For example, you can select a collection of integers where each integer is a field of a class in the original collection.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ARules"/> is <c>nil</c>.</exception>
    function Select<TOut>(const ASelector: TFunc<T, TOut>; const ARules: TRules<TOut>): ISequence<TOut>; overload;

    ///  <summary>Represents a "select" operation.</summary>
    ///  <param name="ASelector">A selector method invoked for each element in the collection.</param>
    ///  <returns>A new collection containing the selected values.</returns>
    ///  <remarks>This method is used when it is required to select values related to the ones in the operated collection.
    ///  For example, you can select a collection of integers where each integer is a field of a class in the original collection.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector"/> is <c>nil</c>.</exception>
    function Select<TOut>(const ASelector: TFunc<T, TOut>): ISequence<TOut>; overload;

{$IF CompilerVersion > 21}
    ///  <summary>Represents a "select" operation.</summary>
    ///  <param name="AMemberName">A record or class field/property name that will be selected.</param>
    ///  <returns>A new collection containing the selected values.</returns>
    ///  <remarks>This method will only work for classes and record types!</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException"><paramref name="AMemberName"/> is not a real member of record or class.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">The collection's elements are not objects ore records.</exception>
    function Select<TOut>(const AMemberName: string): ISequence<TOut>; overload;

    ///  <summary>Represents a "select" operation.</summary>
    ///  <param name="AMemberName">A record or class field/property name that will be selected.</param>
    ///  <returns>A new collection containing the selected values represented as Rtti <c>TValue</c>s.</returns>
    ///  <remarks>This method will only work for classes and record types!</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException"><paramref name="AMemberName"/> is not a real member of record or class.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">The collection's elements are not objects ore records.</exception>
    function Select(const AMemberName: string): ISequence<TAny>; overload;

    ///  <summary>Represents a "select" operation.</summary>
    ///  <param name="AMemberNames">A record or class field/property names that will be selected.</param>
    ///  <returns>A new collection containing the selected values represented as a view.</returns>
    ///  <remarks>This method will only work for classes and record types! The resulting view contains the selected members.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException"><paramref name="AMemberName"/> is not a real member of record or class.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">The collection's elements are not objects ore records.</exception>
    function Select(const AMemberNames: array of string): ISequence<TView>; overload;
{$IFEND}

    ///  <summary>Represents a "where, select object" operation.</summary>
    ///  <returns>A new collection containing the selected values.</returns>
    ///  <remarks>This method can be used on a collection containing objects. The operation involves two steps,
    ///  where and select. First, each object is checked to be derived from <c>TOut</c>. If that is true, it is then
    ///  cast to <c>TOut</c>. The result of the operation is a new collection that contains only the objects of a given
    ///  class. For example, <c>AList.Op.Select&lt;TMyObject&gt;</c> results in a new collection that only contains
    ///  "TMyObject" instances.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">The collection's elements are not objects.</exception>
    function Select<TOut: class>(): ISequence<TOut>; overload;

    ///  <summary>Groups all elements in the collection by a given key.</summary>
    ///  <param name="ASelector">The selector function. Returns the key (based on each collection element) that serves for grouping purposes.</param>
    ///  <returns>A collection of grouping collections.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector"/> is <c>nil</c>.</exception>
    ///  <remarks>This operation will call <paramref name="ASelector"/> for each element in the collection and retrieve a "key". Using this key,
    ///  the elements are grouped into new collections called groupings. The result of this operation is a collection of groupings. Each grouping
    ///  contains the elements from the original collection that have the same group and a key (which is the group value used).</remarks>
    function GroupBy<TKey>(const ASelector: TFunc<T, TKey>): ISequence<IGrouping<TKey, T>>; overload;

    ///  <summary>Orders the collection based on selector method.</summary>
    ///  <param name="ASelector">The selector function. Returns the key (based on each collection element) that serves for ordering purposes.</param>
    ///  <returns>The resulting ordered collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector1"/> is <c>nil</c>.</exception>
    ///  <remarks>This operation will call <paramref name="ASelector"/> for each element in the collection and retrieve a "key". Using this key,
    ///  the elements are ordered into a new collection.</remarks>
    function OrderBy<TKey>(const ASelector: TFunc<T, TKey>): ISequence<T>; overload;

    ///  <summary>Orders the collection based on selector method.</summary>
    ///  <param name="ASelector1">The selector function for the first key. Returns the key (based on each collection element) that serves for ordering purposes.</param>
    ///  <param name="ASelector2">The selector function for the second key. Returns the key (based on each collection element) that serves for ordering purposes.</param>
    ///  <returns>The resulting ordered collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector1"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector2"/> is <c>nil</c>.</exception>
    ///  <remarks>This operation will call each <paramref name="ASelector"/> for elements in the collection and retrieve the ordering "keys". Using these keys,
    ///  the elements are ordered into a new collection.</remarks>
    function OrderBy<TKey1, TKey2>(const ASelector1: TFunc<T, TKey1>; const ASelector2: TFunc<T, TKey2>): ISequence<T>; overload;

    ///  <summary>Orders the collection based on selector method.</summary>
    ///  <param name="ASelector1">The selector function for the first key. Returns the key (based on each collection element) that serves for ordering purposes.</param>
    ///  <param name="ASelector2">The selector function for the second key. Returns the key (based on each collection element) that serves for ordering purposes.</param>
    ///  <param name="ASelector3">The selector function for the third key. Returns the key (based on each collection element) that serves for ordering purposes.</param>
    ///  <returns>The resulting ordered collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector1"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector2"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector3"/> is <c>nil</c>.</exception>
    ///  <remarks>This operation will call each <paramref name="ASelector"/> for elements in the collection and retrieve the ordering "keys". Using these keys,
    ///  the elements are ordered into a new collection.</remarks>
    function OrderBy<TKey1, TKey2, TKey3>(const ASelector1: TFunc<T, TKey1>; const ASelector2: TFunc<T, TKey2>;
      const ASelector3: TFunc<T, TKey3>): ISequence<T>; overload;

    ///  <summary>Orders the collection based on selector method.</summary>
    ///  <param name="ASelector1">The selector function for the first key. Returns the key (based on each collection element) that serves for ordering purposes.</param>
    ///  <param name="ASelector2">The selector function for the second key. Returns the key (based on each collection element) that serves for ordering purposes.</param>
    ///  <param name="ASelector3">The selector function for the third key. Returns the key (based on each collection element) that serves for ordering purposes.</param>
    ///  <param name="ASelector4">The selector function for the fourth key. Returns the key (based on each collection element) that serves for ordering purposes.</param>
    ///  <returns>The resulting ordered collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector1"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector2"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector3"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector4"/> is <c>nil</c>.</exception>
    ///  <remarks>This operation will call each <paramref name="ASelector"/> for elements in the collection and retrieve the ordering "keys". Using these keys,
    ///  the elements are ordered into a new collection.</remarks>
    function OrderBy<TKey1, TKey2, TKey3, TKey4>(const ASelector1: TFunc<T, TKey1>; const ASelector2: TFunc<T, TKey2>;
      const ASelector3: TFunc<T, TKey3>; const ASelector4: TFunc<T, TKey4>): ISequence<T>; overload;

    ///  <summary>Orders the collection based on selector method.</summary>
    ///  <param name="ASelector1">The selector function for the first key. Returns the key (based on each collection element) that serves for ordering purposes.</param>
    ///  <param name="ASelector2">The selector function for the second key. Returns the key (based on each collection element) that serves for ordering purposes.</param>
    ///  <param name="ASelector3">The selector function for the third key. Returns the key (based on each collection element) that serves for ordering purposes.</param>
    ///  <param name="ASelector4">The selector function for the fourth key. Returns the key (based on each collection element) that serves for ordering purposes.</param>
    ///  <param name="ASelector5">The selector function for the fifth key. Returns the key (based on each collection element) that serves for ordering purposes.</param>
    ///  <returns>The resulting ordered collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector1"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector2"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector3"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector4"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector5"/> is <c>nil</c>.</exception>
    ///  <remarks>This operation will call each <paramref name="ASelector"/> for elements in the collection and retrieve the ordering "keys". Using these keys,
    ///  the elements are ordered into a new collection.</remarks>
    function OrderBy<TKey1, TKey2, TKey3, TKey4, TKey5>(const ASelector1: TFunc<T, TKey1>; const ASelector2: TFunc<T, TKey2>;
      const ASelector3: TFunc<T, TKey3>; const ASelector4: TFunc<T, TKey4>; const ASelector5: TFunc<T, TKey5>): ISequence<T>; overload;

    ///  <summary>Joins this sequence with another enumerable collection using a common key.</summary>
    ///  <param name="AInner">The inner collection to join with.</param>
    ///  <param name="AKeySelector">The selector function for the key of this collection.</param>
    ///  <param name="AInnerKeySelector">The selector function for the key of the inner collection.</param>
    ///  <param name="AResultSelector">The selector function that combines the result.</param>
    ///  <returns>The resulting joined collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector"/> is <c>nil</c>.</exception>
    ///  <remarks>This operation will enumerate the collection and extract the key for each element. For each key it will try to
    ///  pair up the element with elements in the <c>AInner</c> collection. This function can also be considered an "INNER JOIN" where the elements in the outer
    ///  collection are paired up with elements in the inner collection by the means of a common key (provided by the selector functions).</remarks>
    function Join<TInner, TKey, TResult>(const AInner: IEnumerable<TInner>; const AKeySelector: TFunc<T, TKey>;
      const AInnerKeySelector: TFunc<TInner, TKey>; const AResultSelector: TFunc<T, TInner, TResult>): ISequence<TResult>; overload;

    ///  <summary>Joins ang groups this sequence with another enumerable collection using a common key.</summary>
    ///  <param name="AInner">The inner collection to join with.</param>
    ///  <param name="AKeySelector">The selector function for the key of this collection.</param>
    ///  <param name="AInnerKeySelector">The selector function for the key of the inner collection.</param>
    ///  <param name="AResultSelector">The selector function that combines the result.</param>
    ///  <returns>The resulting joined collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector"/> is <c>nil</c>.</exception>
    ///  <remarks>This operation will enumerate the collection and extract the key for each element. For each key it will try to
    ///  pair up the element with elements in the <c>AInner</c> collection; and generate a group of elements.</remarks>
    function GroupJoin<TInner, TKey, TResult>(const AInner: IEnumerable<TInner>; const AKeySelector: TFunc<T, TKey>;
      const AInnerKeySelector: TFunc<TInner, TKey>; const AResultSelector: TFunc<T, ISequence<TInner>, TResult>): ISequence<TResult>; overload;
  end;

  ///  <summary>Base sequence interface inherited by all specific collection interfaces.</summary>
  ///  <remarks>This interface defines a set of traits common to all collections implemented in this package. It also introduces
  ///  a large set of extended operations that can be performed on any collection that supports enumerability.</remarks>
  ISequence<T> = interface(IContainer<T>)
    ///  <summary>Checks whether the elements in this collection are equal to the elements in another collection.</summary>
    ///  <param name="ACollection">The collection to compare to.</param>
    ///  <returns><c>True</c> if the collections are equal; <c>False</c> if the collections are different.</returns>
    ///  <remarks>This method checks that each element at position X in this collection is equal to an element at position X in
    ///  the provided collection. If the number of elements in both collections is different, then the collections are considered different.
    ///  Note that comparison of element is done using the rule set used by this collection. This means that comparing this collection
    ///  to another one might yield a different result than comparing the other collection to this one.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function EqualsTo(const ACollection: IEnumerable<T>): Boolean;

    ///  <summary>Creates a new list containing the elements of this collection.</summary>
    ///  <returns>A list containing the elements copied from this collection.</returns>
    ///  <remarks>This method also copies the rule set of this collection. Be careful if the rule set
    ///  performs cleanup on the elements.</remarks>
    function ToList(): IList<T>;

    ///  <summary>Creates a new set containing the elements of this collection.</summary>
    ///  <returns>A set containing the elements copied from this collection.</returns>
    ///  <remarks>This method also copies the rule set of this collection. Be careful if the rule set
    ///  performs cleanup on the elements.</remarks>
    function ToSet(): ISet<T>;

    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the collection considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Max(): T;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the collection considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Min(): T;

    ///  <summary>Returns the first element.</summary>
    ///  <returns>The first element in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function First(): T;

    ///  <summary>Returns the first element or a default if the collection is empty.</summary>
    ///  <param name="ADefault">The default value returned if the collection is empty.</param>
    ///  <returns>The first element in the collection if the collection is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function FirstOrDefault(const ADefault: T): T;

    ///  <summary>Returns the first element that satisfies the given predicate.</summary>
    ///  <param name="APredicate">The predicate to use.</param>
    ///  <returns>The first element that satisfies the given predicate.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the predicate.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function FirstWhere(const APredicate: TPredicate<T>): T;

    ///  <summary>Returns the first element that satisfies the given predicate or a default value.</summary>
    ///  <param name="APredicate">The predicate to use.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given predicate; <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function FirstWhereOrDefault(const APredicate: TPredicate<T>; const ADefault: T): T;

    ///  <summary>Returns the first element that does not satisfy the given predicate.</summary>
    ///  <param name="APredicate">The predicate to use.</param>
    ///  <returns>The first element that does not satisfy the given predicate.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements that do not satisfy the predicate.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function FirstWhereNot(const APredicate: TPredicate<T>): T;

    ///  <summary>Returns the first element that does not satisfy the given predicate or a default value.</summary>
    ///  <param name="APredicate">The predicate to use.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that does not satisfy the given predicate; <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function FirstWhereNotOrDefault(const APredicate: TPredicate<T>; const ADefault: T): T;

    ///  <summary>Returns the first element lower than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereLower(const ABound: T): T;

    ///  <summary>Returns the first element lower than a given value or a default.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereLowerOrDefault(const ABound: T; const ADefault: T): T;

    ///  <summary>Returns the first element lower than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereLowerOrEqual(const ABound: T): T;

    ///  <summary>Returns the first element lower than or equal to a given value or a default.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereLowerOrEqualOrDefault(const ABound: T; const ADefault: T): T;

    ///  <summary>Returns the first element greater than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereGreater(const ABound: T): T;

    ///  <summary>Returns the first element greater than a given value or a default.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereGreaterOrDefault(const ABound: T; const ADefault: T): T;

    ///  <summary>Returns the first element greater than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereGreaterOrEqual(const ABound: T): T;

    ///  <summary>Returns the first element greater than or equal to a given value or a default.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereGreaterOrEqualOrDefault(const ABound: T; const ADefault: T): T;

    ///  <summary>Returns the first element situated within the given bounds.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The higher bound.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereBetween(const ALower, AHigher: T): T;

    ///  <summary>Returns the first element situated within the given bounds or a default.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The higher bound.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereBetweenOrDefault(const ALower, AHigher: T; const ADefault: T): T;

    ///  <summary>Returns the last element.</summary>
    ///  <returns>The last element in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Last(): T;

    ///  <summary>Returns the last element or a default if the collection is empty.</summary>
    ///  <param name="ADefault">The default value returned if the collection is empty.</param>
    ///  <returns>The last element in the collection if the collection is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function LastOrDefault(const ADefault: T): T;

    ///  <summary>Aggregates a value based on the collection's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <returns>A value that contains the collection's aggregated value.</returns>
    ///  <remarks>This method returns the first element if the collection only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Aggregate(const AAggregator: TFunc<T, T, T>): T;

    ///  <summary>Aggregates a value based on the collection's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <param name="ADefault">The default value returned if the collection is empty.</param>
    ///  <returns>A value that contains the collection's aggregated value. If the collection is empty, <paramref name="ADefault"/> is returned.</returns>
    ///  <remarks>This method returns the first element if the collection only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <returns>The element at the specified position.</returns>
    ///  <remarks>This method is slow for collections that cannot reference their elements by indexes; for example: linked lists</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    function ElementAt(const AIndex: NativeInt): T;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <param name="ADefault">The default value returned if the collection is empty.</param>
    ///  <returns>The element at the specified position if the collection is not empty and the position is not out of bounds; otherwise
    ///  the value of <paramref name="ADefault"/> is returned.</returns>
    ///  <remarks>This method is slow for collections that cannot reference their elements by indexes; for example: linked lists</remarks>
    function ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T;

    ///  <summary>Check whether at least one element in the collection satisfies a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if the at least one element satisfies a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole collection and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>True</c>. The logical equivalent of this operation is "OR".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Any(const APredicate: TPredicate<T>): Boolean;

    ///  <summary>Checks that all elements in the collection satisfies a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if all elements satisfy a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole collection and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>False</c>. The logical equivalent of this operation is "AND".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function All(const APredicate: TPredicate<T>): Boolean;

    ///  <summary>Selects only the elements that satisfy a given rule.</summary>
    ///  <param name="APredicate">The predicate that represents the rule.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the given rule.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Where(const APredicate: TPredicate<T>): ISequence<T>;

    ///  <summary>Selects only the elements that do not satisfy a given rule.</summary>
    ///  <param name="APredicate">The predicate that represents the rule.</param>
    ///  <returns>A new collection that contains only the elements that do not satisfy the given rule.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function WhereNot(const APredicate: TPredicate<T>): ISequence<T>;

    ///  <summary>Selects only the elements that are less than a given value.</summary>
    ///  <param name="ABound">The element to compare against.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    function WhereLower(const ABound: T): ISequence<T>;

    ///  <summary>Selects only the elements that are less than or equal to a given value.</summary>
    ///  <param name="ABound">The element to compare against.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    function WhereLowerOrEqual(const ABound: T): ISequence<T>;

    ///  <summary>Selects only the elements that are greater than a given value.</summary>
    ///  <param name="ABound">The element to compare against.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    function WhereGreater(const ABound: T): ISequence<T>;

    ///  <summary>Selects only the elements that are greater than or equal to a given value.</summary>
    ///  <param name="ABound">The element to compare against.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    function WhereGreaterOrEqual(const ABound: T): ISequence<T>;

    ///  <summary>Selects only the elements whose values are contained whithin a given interval.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The upper bound.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    ///  <remarks>The elements that are equal to the lower or upper bounds, are also included.</remarks>
    function WhereBetween(const ALower, AHigher: T): ISequence<T>;

    ///  <summary>Selects all the elements from the collection excluding duplicates.</summary>
    ///  <returns>A new collection that contains the distinct elements.</returns>
    function Distinct(): ISequence<T>;

    ///  <summary>Returns a new ordered collection that contains the elements from this collection.</summary>
    ///  <param name="AAscending">Specifies whether the elements are ordered ascending or descending.</param>
    ///  <returns>A new ordered collection.</returns>
    function Ordered(const AAscending: Boolean = true): ISequence<T>; overload;

    ///  <summary>Returns a new ordered collection that contains the elements from this collection.</summary>
    ///  <param name="ASortProc">The comparison method.</param>
    ///  <returns>A new ordered collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASortProc"/> is <c>nil</c>.</exception>
    function Ordered(const ASortProc: TComparison<T>): ISequence<T>; overload;

    ///  <summary>Revereses the contents of the collection.</summary>
    ///  <returns>A new collection that contains the elements from this collection but in reverse order.</returns>
    function Reversed(): ISequence<T>;

    ///  <summary>Concatenates this collection with another collection.</summary>
    ///  <param name="ACollection">A collection to concatenate.</param>
    ///  <returns>A new collection that contains the elements from this collection followed by elements
    ///  from the given collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function Concat(const ACollection: ISequence<T>): ISequence<T>;

    ///  <summary>Creates a new collection that contains the elements from both collections taken a single time.</summary>
    ///  <param name="ACollection">The collection to unify with.</param>
    ///  <returns>A new collection that contains the elements from this collection followed by elements
    ///  from the given collection except the elements that already are present in this collection. This operation can be seen as
    ///  a "concat" operation followed by a "distinct" operation. </returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function Union(const ACollection: ISequence<T>): ISequence<T>;

    ///  <summary>Creates a new collection that contains the elements from this collection minus the ones in the given collection.</summary>
    ///  <param name="ACollection">The collection to exclude.</param>
    ///  <returns>A new collection that contains the elements from this collection minus the those elements that are common between
    ///  this and the given collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function Exclude(const ACollection: ISequence<T>): ISequence<T>;

    ///  <summary>Creates a new collection that contains the elements that are present in both collections.</summary>
    ///  <param name="ACollection">The collection to interset with.</param>
    ///  <returns>A new collection that contains the elements that are common to both collections.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function Intersect(const ACollection: ISequence<T>): ISequence<T>;

    ///  <summary>Select the elements that whose indexes are located in the given range.</summary>
    ///  <param name="AStart">The lower bound.</param>
    ///  <param name="AEnd">The upper bound.</param>
    ///  <returns>A new collection that contains the elements whose indexes in this collection are locate between <paramref name="AStart"/>
    ///  and <paramref name="AEnd"/>. Note that this method does not check the indexes. This means that a bad combination of parameters will
    ///  simply result in an empty or incorrect result.</returns>
    function Range(const AStart, AEnd: NativeInt): ISequence<T>;

    ///  <summary>Selects only a given amount of elements.</summary>
    ///  <param name="ACount">The number of elements to select.</param>
    ///  <returns>A new collection that contains only the first <paramref name="ACount"/> elements.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="ACount"/> is zero.</exception>
    function Take(const ACount: NativeInt): ISequence<T>;

    ///  <summary>Selects all the elements from the collection while a given rule is satisfied.</summary>
    ///  <param name="APredicate">The rule to satisfy.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function TakeWhile(const APredicate: TPredicate<T>): ISequence<T>;

    ///  <summary>Selects all the elements from the collection while elements are lower than a given value.</summary>
    ///  <param name="ABound">The value to check against.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileLower(const ABound: T): ISequence<T>;

    ///  <summary>Selects all the elements from the collection while elements are lower than
    ///  or equals to a given value.</summary>
    ///  <param name="ABound">The value to check against.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileLowerOrEqual(const ABound: T): ISequence<T>;

    ///  <summary>Selects all the elements from the collection while elements are greater than
    ///  a given value.</summary>
    ///  <param name="ABound">The value to check against.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileGreater(const ABound: T): ISequence<T>;

    ///  <summary>Selects all the elements from the collection while elements are greater than
    ///  or equals to a given value.</summary>
    ///  <param name="ABound">The value to check against.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileGreaterOrEqual(const ABound: T): ISequence<T>;

    ///  <summary>Selects all the elements from the collection while elements are between a given range of values.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The higher bound.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileBetween(const ALower, AHigher: T): ISequence<T>;

    ///  <summary>Skips a given amount of elements.</summary>
    ///  <param name="ACount">The number of elements to skip.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="ACount"/> is zero.</exception>
    function Skip(const ACount: NativeInt): ISequence<T>;

    ///  <summary>Skips all the elements from the collection while a given rule is satisfied.</summary>
    ///  <param name="APredicate">The rule to satisfy.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function SkipWhile(const APredicate: TPredicate<T>): ISequence<T>;

    ///  <summary>Skips all the elements from the collection while elements are lower than a given value.</summary>
    ///  <param name="ABound">The value to check.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileLower(const ABound: T): ISequence<T>;

    ///  <summary>Skips all the elements from the collection while elements are lower than or equal to a given value.</summary>
    ///  <param name="ABound">The value to check.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileLowerOrEqual(const ABound: T): ISequence<T>;

    ///  <summary>Skips all the elements from the collection while elements are greater than a given value.</summary>
    ///  <param name="ABound">The value to check.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileGreater(const ABound: T): ISequence<T>;

    ///  <summary>Skips all the elements from the collection while elements are greater than or equal to a given value.</summary>
    ///  <param name="ABound">The value to check.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileGreaterOrEqual(const ABound: T): ISequence<T>;

    ///  <summary>Skips all the elements from the collection while elements are between a given range of values.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The higher bound.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileBetween(const ALower, AHigher: T): ISequence<T>;

    ///  <summary>Exposes a type that provides extended Enex operations such as "select".</summary>
    ///  <returns>A record that exposes more Enex operations that otherwise would be impossible.</returns>
    function Op: TEnexExtOps<T>;
  end;

  ///  <summary>Enex collection that is presumed to be grouped by a certain key.</summary>
  IGrouping<TKey, T> = interface(ISequence<T>)
    ///  <summary>Returns the key under which all elements in this collection are grouped.</summary>
    ///  <returns>The key of this grouping.</returns>
    function GetKey(): TKey;

    ///  <summary>Returns the key under which all elements in this collection are grouped.</summary>
    ///  <returns>The key of this grouping.</returns>
    property Key: TKey read GetKey;
  end;

  ///  <summary>Specifies a set of methods specific to all simple (non-associative) collections.</summary>
  ///  <remarks>This collection exposes operations such as <c>Add</c> or <c>Clear</c> that need to be implemented
  ///  in almost every class out there.</remarks>
  ICollection<T> = interface(ISequence<T>)
    ///  <summary>Clears the contents of this collection.</summary>
    procedure Clear();

    ///  <summary>Adds an element to this collection.</summary>
    ///  <param name="AValue">The value to add.</param>
    ///  <remarks>Where exactly the element is added is unspecified and depends on the implementing collection.</remarks>
    procedure Add(const AValue: T);

    ///  <summary>Adds all the elements from a collection to this collection.</summary>
    ///  <param name="ACollection">The values to add.</param>
    ///  <remarks>Where exactly the elements are added is unspecified and depends on the implementing collection.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    procedure AddAll(const ACollection: IEnumerable<T>);

    ///  <summary>Removes an element from this collection.</summary>
    ///  <param name="AValue">The value to remove. If there is no such element in the collection, nothing happens.</param>
    procedure Remove(const AValue: T); overload;

    ///  <summary>Removes all the elements from a collection that are also found in this collection.</summary>
    ///  <param name="ACollection">The values to remove.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    procedure RemoveAll(const ACollection: IEnumerable<T>);

    ///  <summary>Checks whether a specified element is contained in this collection.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the value was found in the collection; <c>False</c> otherwise.</returns>
    function Contains(const AValue: T): Boolean; overload;

    ///  <summary>Checks whether all the elements from a specified collection are contained in this collection.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the values were found in the collection; <c>False</c> otherwise.</returns>
    function ContainsAll(const ACollection: IEnumerable<T>): Boolean;
  end;

  ///  <summary>Base Enex (Extended enumerable) interface inherited by all specific associative collection interfaces.</summary>
  ///  <remarks>This interface defines a set of traits common to all associative collections implemented in this package. It also introduces
  ///  a large se of extended operations that can pe performed on any collection that supports enumerability.</remarks>
  IAssociation<TKey, TValue> = interface(IContainer<TPair<TKey, TValue>>)
    ///  <summary>Creates a new dictionary containing the elements of this collection.</summary>
    ///  <returns>A dictionary containing the elements copied from this collection.</returns>
    ///  <remarks>This method also copies the rule sets of this collection. Be careful if the rule set
    ///  performs cleanup on the elements.</remarks>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The collection contains more than
    ///  one key-value pair with the same key.</exception>
    function ToDictionary(): IDictionary<TKey, TValue>;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to return the associated value.</param>
    ///  <returns>The value associated with the given key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">No such key in the collection.</exception>
    function ValueForKey(const AKey: TKey): TValue;

    ///  <summary>Checks whether the collection contains a given key-value pair.</summary>
    ///  <param name="AKey">The key part of the pair.</param>
    ///  <param name="AValue">The value part of the pair.</param>
    ///  <returns><c>True</c> if the given key-value pair exists; <c>False</c> otherwise.</returns>
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;

    ///  <summary>Returns the biggest key.</summary>
    ///  <returns>The biggest key stored in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function MaxKey(): TKey;

    ///  <summary>Returns the smallest key.</summary>
    ///  <returns>The smallest key stored in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function MinKey(): TKey;

    ///  <summary>Returns the biggest value.</summary>
    ///  <returns>The biggest value stored in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function MaxValue(): TValue;

    ///  <summary>Returns the smallest value.</summary>
    ///  <returns>The smallest value stored in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function MinValue(): TValue;

    ///  <summary>Returns an Enex collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the collection.</returns>
    function SelectKeys(): ISequence<TKey>;

    ///  <summary>Returns a Enex collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the collection.</returns>
    function SelectValues(): ISequence<TValue>;

    ///  <summary>Specifies the collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the collection.</returns>
    property Keys: ISequence<TKey> read SelectKeys;

    ///  <summary>Specifies the collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the collection.</returns>
    property Values: ISequence<TValue> read SelectValues;

    ///  <summary>Selects all the key-value pairs from the collection excluding the duplicates by key.</summary>
    ///  <returns>A new collection that contains the distinct pairs.</returns>
    function DistinctByKeys(): IAssociation<TKey, TValue>;

    ///  <summary>Selects all the key-value pairs from the collection excluding the duplicates by value.</summary>
    ///  <returns>A new collection that contains the distinct pairs.</returns>
    function DistinctByValues(): IAssociation<TKey, TValue>;

    ///  <summary>Checks whether this collection includes the key-value pairs in another collection.</summary>
    ///  <param name="ACollection">The collection to check against.</param>
    ///  <returns><c>True</c> if this collection includes the elements in another; <c>False</c> otherwise.</returns>
    function Includes(const ACollection: IEnumerable<TPair<TKey, TValue>>): Boolean;

    ///  <summary>Selects only the key-value pairs that satisfy a given rule.</summary>
    ///  <param name="APredicate">The predicate that represents the rule.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the given rule.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Where(const APredicate: TPredicate<TKey, TValue>): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs that do not satisfy a given rule.</summary>
    ///  <param name="APredicate">The predicate that represents the rule.</param>
    ///  <returns>A new collection that contains only the pairs that do not satisfy the given rule.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function WhereNot(const APredicate: TPredicate<TKey, TValue>): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are less than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyLower(const ABound: TKey): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are less than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyLowerOrEqual(const ABound: TKey): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are greater than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyGreater(const ABound: TKey): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are greater than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyGreaterOrEqual(const ABound: TKey): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are are contained whithin a given interval.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The upper bound.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyBetween(const ALower, AHigher: TKey): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are less than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueLower(const ABound: TValue): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are less than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueLowerOrEqual(const ABound: TValue): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are greater than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueGreater(const ABound: TValue): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are greater than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueGreaterOrEqual(const ABound: TValue): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are are contained whithin a given interval.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The upper bound.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueBetween(const ALower, AHigher: TValue): IAssociation<TKey, TValue>;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>stack</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>stack</c>.</remarks>
  IStack<T> = interface(ICollection<T>)
    ///  <summary>Pushes an element to the top of the stack.</summary>
    ///  <param name="AValue">The value to push.</param>
    procedure Push(const AValue: T);

    ///  <summary>Retrieves the element from the top of the stack.</summary>
    ///  <returns>The value at the top of the stack.</returns>
    ///  <remarks>This method removes the element from the top of the stack.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function Pop(): T;

    ///  <summary>Reads the element from the top of the stack.</summary>
    ///  <returns>The value at the top of the stack.</returns>
    ///  <remarks>This method does not remove the element from the top of the stack. It merely reads it's value.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function Peek(): T;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>queue</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>queue</c>.</remarks>
  IQueue<T> = interface(ICollection<T>)
    ///  <summary>Appends an element to the head of the queue.</summary>
    ///  <param name="AValue">The value to append.</param>
    procedure Enqueue(const AValue: T);

    ///  <summary>Retrieves the element from the bottom of the queue.</summary>
    ///  <returns>The value at the bottom of the queue.</returns>
    ///  <remarks>This method removes the element from the bottom of the queue.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Dequeue(): T;

    ///  <summary>Reads the element from the bottom of the queue.</summary>
    ///  <returns>The value at the bottom of the queue.</returns>
    ///  <remarks>This method does not remove the element from the bottom of the queue. It merely reads it's value.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Peek(): T;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>priority queue</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>priority queue</c>.</remarks>
  IPriorityQueue<TPriority, TValue> = interface(IAssociation<TPriority, TValue>)
    ///  <summary>Clears the contents of the priority queue.</summary>
    procedure Clear();

    ///  <summary>Adds an element to the priority queue.</summary>
    ///  <param name="AValue">The value to append.</param>
    ///  <remarks>The lowest possible priority of the element is assumed. This means that the element is appended to the top of the queue.</remarks>
    procedure Enqueue(const AValue: TValue); overload;

    ///  <summary>Adds an element to the priority queue.</summary>
    ///  <param name="AValue">The value to add.</param>
    ///  <param name="APriority">The priority of the value.</param>
    ///  <remarks>The given priority is used to calculate the position of the value in the queue. Based on the priority the element might occupy any
    ///  given position (for example it might even end up at the bottom position).</remarks>
    procedure Enqueue(const AValue: TValue; const APriority: TPriority); overload;

    ///  <summary>Retrieves the element from the bottom of the priority queue.</summary>
    ///  <returns>The value at the bottom of the priority queue.</returns>
    ///  <remarks>This method removes the element from the bottom of the priority queue.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Dequeue(): TValue;

    ///  <summary>Reads the element from the bottom of the priority queue.</summary>
    ///  <returns>The value at the bottom of the priority queue.</returns>
    ///  <remarks>This method does not remove the element from the bottom of the priority queue. It merely reads it's value.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Peek(): TValue;

    ///  <summary>Checks whether the priority queue contains a given value.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns><c>True</c> if the value was found in the queue; <c>False</c> otherwise.</returns>
    function Contains(const AValue: TValue): Boolean;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>set</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>set</c>.</remarks>
  ISet<T> = interface(ICollection<T>)
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>sorted set</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>sorted set</c>.</remarks>
  ISortedSet<T> = interface(ISet<T>)
    ///  <summary>Returns the biggest set element.</summary>
    ///  <returns>An element from the set considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function Max(): T;

    ///  <summary>Returns the smallest set element.</summary>
    ///  <returns>An element from the set considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function Min(): T;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>bag</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>bag</c>.</remarks>
  IBag<T> = interface(ISet<T>)
    ///  <summary>Adds an element to the bag.</summary>
    ///  <param name="AValue">The element to add.</param>
    ///  <param name="AWeight">The weight of the element.</param>
    ///  <remarks>If the bag already contains the given value, it's stored weight is incremented to by <paramref name="AWeight"/>.
    ///  If the value of <paramref name="AWeight"/> is zero, nothing happens.</remarks>
    procedure AddWeight(const AValue: T; const AWeight: NativeUInt);

    ///  <summary>Removes an element from the bag.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <param name="AWeight">The weight to remove.</param>
    ///  <remarks>This method decreses the weight of the stored item by <paramref name="AWeight"/>. If the resulting weight is less
    ///  than zero or zero, the element is removed for the bag. If <paramref name="AWeight"/> is zero, nothing happens.</remarks>
    procedure RemoveWeight(const AValue: T; const AWeight: NativeUInt);

    ///  <summary>Removes an element from the bag.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>This method completely removes an item from the bag ignoring it's stored weight. Nothing happens if the given value
    ///  is not in the bag to begin with.</remarks>
    procedure RemoveAllWeight(const AValue: T);

    ///  <summary>Checks whether the bag contains an element with at least the required weight.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <param name="AWeight">The smallest allowed weight.</param>
    ///  <returns><c>True</c> if the condition is met; <c>False</c> otherwise.</returns>
    ///  <remarks>This method checks whether the bag contains the given value and that the contained value has at least the
    ///  given weight.</remarks>
    function ContainsWeight(const AValue: T; const AWeight: NativeUInt): Boolean;

    ///  <summary>Returns the weight of an element.</param>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns>The weight of the value.</returns>
    ///  <remarks>If the value is not found in the bag, zero is returned.</remarks>
    function GetWeight(const AValue: T): NativeUInt;

    ///  <summary>Sets the weight of an element.</param>
    ///  <param name="AValue">The value to set the weight for.</param>
    ///  <param name="AWeight">The new weight.</param>
    ///  <remarks>If the value is not found in the bag, this method acts like an <c>Add</c> operation; otherwise
    ///  the weight of the stored item is adjusted.</remarks>
    procedure SetWeight(const AValue: T; const AWeight: NativeUInt);

    ///  <summary>Sets or gets the weight of an item in the bag.</summary>
    ///  <param name="AValue">The value.</param>
    ///  <remarks>If the value is not found in the bag, this method acts like an <c>Add</c> operation; otherwise
    ///  the weight of the stored item is adjusted.</remarks>
    property Weights[const AValue: T]: NativeUInt read GetWeight write SetWeight; default;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>list</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>list</c>.</remarks>
  IList<T> = interface(ICollection<T>)
    ///  <summary>Inserts an element into the list.</summary>
    ///  <param name="AIndex">The index to insert to.</param>
    ///  <param name="AValue">The value to insert.</param>
    ///  <remarks>All elements starting with <paramref name="AIndex"/> are moved to the right by one and then
    ///  <paramref name="AValue"/> is placed at position <paramref name="AIndex"/>.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    procedure Insert(const AIndex: NativeInt; const AValue: T); overload;

    ///  <summary>Inserts the elements of a collection into the list.</summary>
    ///  <param name="AIndex">The index to insert to.</param>
    ///  <param name="ACollection">The values to insert.</param>
    ///  <remarks>All elements starting with <paramref name="AIndex"/> are moved to the right by the length of
    ///  <paramref name="ACollection"/> and then <paramref name="AValue"/> is placed at position <paramref name="AIndex"/>.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    procedure InsertAll(const AIndex: NativeInt; const ACollection: IEnumerable<T>); overload;

    ///  <summary>Removes an element from the list at a given index.</summary>
    ///  <param name="AIndex">The index from which to remove the element.</param>
    ///  <remarks>This method removes the specified element and moves all following elements to the left by one.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    procedure RemoveAt(const AIndex: NativeInt);

    ///  <summary>Extracts an element from the list at a given index.</summary>
    ///  <param name="AIndex">The index from which to extract the element.</param>
    ///  <remarks>This method removes the specified element and moves all following elements to the left by one.
    ///  The removed element is returned to the caller.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    function ExtractAt(const AIndex: NativeInt): T;

    ///  <summary>Searches for the first appearance of a given element in this list.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <returns><c>-1</c> if the value was not found; otherwise a positive value indicating the index of the value.</returns>
    function IndexOf(const AValue: T): NativeInt;

    ///  <summary>Searches for the last appearance of a given element in this list.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <returns><c>-1</c> if the value was not found; otherwise a positive value indicating the index of the value.</returns>
    function LastIndexOf(const AValue: T): NativeInt;

    ///  <summary>Returns the item at a given index.</summary>
    ///  <param name="AIndex">The index in the list.</param>
    ///  <returns>The element at the specified position.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    function GetItem(const AIndex: NativeInt): T;

    ///  <summary>Sets the item at a given index.</summary>
    ///  <param name="AIndex">The index in the list.</param>
    ///  <param name="AValue">The new value.</param>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    procedure SetItem(const AIndex: NativeInt; const AValue: T);

    ///  <summary>Returns the item at a given index.</summary>
    ///  <param name="AIndex">The index in the collection.</param>
    ///  <returns>The element at the specified position.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    property Items[const AIndex: NativeInt]: T read GetItem write SetItem; default;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>linked list</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>linked list</c>.</remarks>
  ILinkedList<T> = interface(IList<T>)
    ///  <summary>Appends an element to the back of list.</summary>
    ///  <param name="AValue">The value to append.</param>
    ///  <remarks>This method is functionally identical to <c>Add</c>. Classes that implement this interface can simply
    ///  alias this method to <c>Add</c>.</remarks>
    procedure AddLast(const AValue: T); overload;

    ///  <summary>Appends the elements from a collection to the back of the list.</summary>
    ///  <param name="ACollection">The values to append.</param>
    ///  <remarks>This method is functionally identical to <c>Add</c>. Classes that implement this interface can simply
    ///  alias this method to <c>Add</c>.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    procedure AddAllLast(const ACollection: IEnumerable<T>); overload;

    ///  <summary>Appends an element to the front of the list.</summary>
    ///  <param name="AValue">The value to append.</param>
    procedure AddFirst(const AValue: T); overload;

    ///  <summary>Appends the elements from a collection to the back of the list.</summary>
    ///  <param name="ACollection">The values to append.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    procedure AddAllFirst(const ACollection: IEnumerable<T>); overload;

    ///  <summary>Removes the first element of the list.</summary>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    procedure RemoveFirst();

    ///  <summary>Removes the last element of the list.</summary>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    procedure RemoveLast();

    ///  <summary>Extracts the first element of the list.</summary>
    ///  <returns>The first element of the list.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    function ExtractFirst(): T;

    ///  <summary>Removes the last element of the list.</summary>
    ///  <returns>The last element of the list.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    function ExtractLast(): T;

    ///  <summary>Returns the first element of the list.</summary>
    ///  <returns>The first element of the list.</returns>
    ///  <remarks>This method is functionally identical to <c>First</c> method exposed by the Enex intarfaces. It is provided here for consistency only.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    function First(): T;

    ///  <summary>Returns the last element of the list.</summary>
    ///  <returns>The last element of the list.</returns>
    ///  <remarks>This method is functionally identical to <c>Last</c> method exposed by the Enex intarfaces. It is provided here for consistency only.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    function Last(): T;
  end;

  ///  <summary>The Enex interface that defines the basic behavior of all <c>map</c>-like collections.</summary>
  ///  <remarks>This interface is inherited by all interfaces that provide <c>map</c>-like functionality.</remarks>
  IMap<TKey, TValue> = interface(IAssociation<TKey, TValue>)
    ///  <summary>Clears the contents of the map.</summary>
    procedure Clear();

{$IF CompilerVersion > 21}
    ///  <summary>Adds a key-value pair to the map.</summary>
    ///  <param name="APair">The key-value pair to add.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The map already contains a pair with the given key.</exception>
    procedure Add(const APair: TPair<TKey, TValue>); overload;

    ///  <summary>Adds a collection of key-value pairs to the map.</summary>
    ///  <param name="ACollection">The collection to add.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The map already contains a pair with the given key.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    procedure AddAll(const ACollection: IEnumerable<TPair<TKey, TValue>>); overload;
{$IFEND}

    ///  <summary>Adds a key-value pair to the map.</summary>
    ///  <param name="AKey">The key of pair.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The map already contains a pair with the given key.</exception>
    procedure Add(const AKey: TKey; const AValue: TValue); overload;

    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key of pair.</param>
    ///  <remarks>If the specified key was not found in the map, nothing happens.</remarks>
    procedure Remove(const AKey: TKey);

    ///  <summary>Checks whether the map contains a key-value pair identified by the given key.</summary>
    ///  <param name="AKey">The key to check for.</param>
    ///  <returns><c>True</c> if the map contains a pair identified by the given key; <c>False</c> otherwise.</returns>
    function ContainsKey(const AKey: TKey): Boolean;

    ///  <summary>Checks whether the map contains a key-value pair that contains a given value.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the map contains a pair containing the given value; <c>False</c> otherwise.</returns>
    ///  <remarks>This operation should be avoided. Its perfomance is poor is most map implementations.</remarks>
    function ContainsValue(const AValue: TValue): Boolean;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>dictionary</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>dictionary</c>.</remarks>
  IDictionary<TKey, TValue> = interface(IMap<TKey, TValue>)
    ///  <summary>Extracts a value using a given key.</summary>
    ///  <param name="AKey">The key of the associated value.</param>
    ///  <returns>The value associated with the key.</returns>
    ///  <remarks>This function is identical to <c>Remove</c> but will return the stored value. If there is no pair with the given key, an exception is raised.</remarks>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The <paramref name="AKey"/> is not part of the dictionary.</exception>
    function Extract(const AKey: TKey): TValue;

    ///  <summary>Tries to obtain the value associated with a given key.</summary>
    ///  <param name="AKey">The key for which to try to retreive the value.</param>
    ///  <param name="AFoundValue">The found value (if the result is <c>True</c>).</param>
    ///  <returns><c>True</c> if the dictionary contains a value for the given key; <c>False</c> otherwise.</returns>
    function TryGetValue(const AKey: TKey; out AFoundValue: TValue): Boolean;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to try to retreive the value.</param>
    ///  <returns>The value associated with the key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the dictionary.</exception>
    function GetValue(const AKey: TKey): TValue;

    ///  <summary>Sets the value for a given key.</summary>
    ///  <param name="AKey">The key for which to set the value.</param>
    ///  <param name="AValue">The value to set.</param>
    ///  <remarks>If the dictionary does not contain the key, this method acts like <c>Add</c>; otherwise the
    ///  value of the specified key is modified.</remarks>
    procedure SetValue(const AKey: TKey; const AValue: TValue);

    ///  <summary>Gets or sets the value for a given key.</summary>
    ///  <param name="AKey">The key for to operate on.</param>
    ///  <returns>The value associated with the key.</returns>
    ///  <remarks>If the dictionary does not contain the key, this method acts like <c>Add</c> if assignment is done to this property;
    ///  otherwise the value of the specified key is modified.</remarks>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">Trying to read the value of a key that is
    ///  not found in the dictionary.</exception>
    property Items[const AKey: TKey]: TValue read GetValue write SetValue; default;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>bidirectional dictionary</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>bidirectional dictionary</c>. In a
  ///  <c>bidirectional dictionary</c>, both the key and the value are treated as "keys".</remarks>
  IBidiDictionary<TKey, TValue> = interface(IMap<TKey, TValue>)
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

    ///  <summary>Removes a key-value pair using a given value.</summary>
    ///  <param name="AValue">The value (and its associated key) to remove.</param>
    procedure RemoveKeyForValue(const AValue: TValue);

    ///  <summary>Removes a specific key-value combination.</summary>
    ///  <param name="AKey">The key to remove.</param>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>This method only remove a key-value combination if that combination actually exists in the dictionary.
    ///  If the key is associated with another value, nothing happens.</remarks>
    procedure RemovePair(const AKey: TKey; const AValue: TValue); overload;

{$IF CompilerVersion > 21}
    ///  <summary>Removes a key-value combination.</summary>
    ///  <param name="APair">The pair to remove.</param>
    ///  <remarks>This method only remove a key-value combination if that combination actually exists in the dictionary.
    ///  If the key is associated with another value, nothing happens.</remarks>
    procedure RemovePair(const APair: TPair<TKey, TValue>); overload;
{$IFEND}

    ///  <summary>Checks whether the map contains the given key-value combination.</summary>
    ///  <param name="AKey">The key associated with the value.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <returns><c>True</c> if the dictionary contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const AKey: TKey; const AValue: TValue): Boolean; overload;

{$IF CompilerVersion > 21}
    ///  <summary>Checks whether the map contains a given key-value combination.</summary>
    ///  <param name="APair">The key-value pair combination.</param>
    ///  <returns><c>True</c> if the dictionary contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const APair: TPair<TKey, TValue>): Boolean; overload;
{$IFEND}

    ///  <summary>Tries to obtain the value associated with a given key.</summary>
    ///  <param name="AKey">The key for which to try to retreive the value.</param>
    ///  <param name="AFoundValue">The found value (if the result is <c>True</c>).</param>
    ///  <returns><c>True</c> if the dictionary contains a value for the given key; <c>False</c> otherwise.</returns>
    function TryGetValueForKey(const AKey: TKey; out AFoundValue: TValue): Boolean;

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

    ///  <summary>Returns the value associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated value.</param>
    ///  <returns>The associated value.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    property ByKey[const AKey: TKey]: TValue read GetValueForKey write SetValueForKey;

    ///  <summary>Tries to obtain the key associated with a given value.</summary>
    ///  <param name="AValue">The value for which to try to retreive the key.</param>
    ///  <param name="AFoundKey">The found key (if the result is <c>True</c>).</param>
    ///  <returns><c>True</c> if the dictionary contains a key for the given value; <c>False</c> otherwise.</returns>
    function TryGetKeyForValue(const AValue: TValue; out AFoundKey: TKey): Boolean;

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

    ///  <summary>Returns the key associated with a value.</summary>
    ///  <param name="AValue">The value for which to obtain the associated key.</param>
    ///  <returns>The associated key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The value is not found in the collection.</exception>
    property ByValue[const AValue: TValue]: TKey read GetKeyForValue write SetKeyForValue;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>bidirectional multi-map</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>bidirectional multi-map</c>. In a
  ///  <c>bidirectional multi-map</c>, both the key and the value are treated as "keys".</remarks>
  IBidiMap<TKey, TValue> = interface(IMap<TKey, TValue>)
    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key (and its associated values) to remove.</param>
    ///  <remarks>This method removes all the values that are associated with the given key. The rule set's cleanup
    ///  routines are used to cleanup the values that are dropped from the map.</remarks>
    procedure RemoveValuesForKey(const AKey: TKey);

    ///  <summary>Removes a key-value pair using a given value.</summary>
    ///  <param name="AValue">The value (and its associated keys) to remove.</param>
    ///  <remarks>This method removes all the keys that are associated with the given value. The rule set's cleanup
    ///  routines are used to cleanup the keys that are dropped from the map.</remarks>
    procedure RemoveKeysForValue(const AValue: TValue);

    ///  <summary>Removes a specific key-value combination.</summary>
    ///  <param name="AKey">The key to remove.</param>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>This method only remove a key-value combination if that combination actually exists in the dictionary.
    ///  If the key is associated with another value, nothing happens.</remarks>
    procedure RemovePair(const AKey: TKey; const AValue: TValue); overload;

{$IF CompilerVersion > 21}
    ///  <summary>Removes a key-value combination.</summary>
    ///  <param name="APair">The pair to remove.</param>
    ///  <remarks>This method only remove a key-value combination if that combination actually exists in the dictionary.
    ///  If the key is associated with another value, nothing happens.</remarks>
    procedure RemovePair(const APair: TPair<TKey, TValue>); overload;
{$IFEND}

    ///  <summary>Checks whether the map contains the given key-value combination.</summary>
    ///  <param name="AKey">The key associated with the value.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const AKey: TKey; const AValue: TValue): Boolean; overload;

{$IF CompilerVersion > 21}
    ///  <summary>Checks whether the map contains a given key-value combination.</summary>
    ///  <param name="APair">The key-value pair combination.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const APair: TPair<TKey, TValue>): Boolean; overload;
{$IFEND}

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    function GetValuesByKey(const AKey: TKey): ISequence<TValue>;

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    property ByKey[const AKey: TKey]: ISequence<TValue> read GetValuesByKey;

    ///  <summary>Returns the collection of keys associated with a value.</summary>
    ///  <param name="AValue">The value for which to obtain the associated keys.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The value is not found in the collection.</exception>
    function GetKeysByValue(const AValue: TValue): ISequence<TKey>;

    ///  <summary>Returns the collection of keys associated with a value.</summary>
    ///  <param name="AValue">The value for which to obtain the associated keys.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The value is not found in the collection.</exception>
    property ByValue[const AValue: TValue]: ISequence<TKey> read GetKeysByValue;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>multi-map</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>multi-map</c>. In a
  ///  <c>multi-map</c>, a key is associated with multiple values, not just one.</remarks>
  IMultiMap<TKey, TValue> = interface(IMap<TKey, TValue>)
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

{$IF CompilerVersion > 21}
    ///  <summary>Removes a key-value pair using a given key and value.</summary>
    ///  <param name="APair">The key and its associated value to remove.</param>
    ///  <remarks>A multi-map allows storing multiple values for a given key. This method allows removing only the
    ///  specified value from the collection of values associated with the given key.</remarks>
    procedure RemovePair(const APair: TPair<TKey, TValue>); overload;
{$IFEND}

    ///  <summary>Checks whether the multi-map contains a given key-value combination.</summary>
    ///  <param name="AKey">The key associated with the value.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const AKey: TKey; const AValue: TValue): Boolean; overload;

{$IF CompilerVersion > 21}
    ///  <summary>Checks whether the multi-map contains a given key-value combination.</summary>
    ///  <param name="APair">The key-value pair to check for.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const APair: TPair<TKey, TValue>): Boolean; overload;
{$IFEND}
    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    function GetValues(const AKey: TKey): ISequence<TValue>;

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    property Items[const AKey: TKey]: ISequence<TValue> read GetValues; default;

    ///  <summary>Tries to extract the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <param name="AValues">The Enex collection that stores the associated values.</param>
    ///  <returns><c>True</c> if the key exists in the collection; <c>False</c> otherwise;</returns>
    function TryGetValues(const AKey: TKey; out AValues: ISequence<TValue>): Boolean; overload;

    ///  <summary>Tries to extract the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>The associated collection if the key if valid; an empty collection otherwise.</returns>
    function TryGetValues(const AKey: TKey): ISequence<TValue>; overload;
  end;

  ///  <summary>A special interface implemented by collections that support the concept of capacity.</summary>
  ///  <remarks>This interface specifies a set of method that allow controlling the capactity of a collection.</remarks>
  IDynamic = interface
    ///  <summary>Returns the current capacity.</summary>
    ///  <returns>A positive number that specifies the number of elements that the collection can hold before it
    ///  needs to grow again.</returns>
    ///  <remarks>The value of this method is greater or equal to the amount of elements in the collection. If this value
    ///  if greater then the number of elements, it means that the collection has some extra capacity to operate upon.</remarks>
    function GetCapacity(): NativeInt;

    ///  <summary>Removes the excess capacity from the collection.</summary>
    ///  <remarks>This method can be called manually to force the collection to drop the extra capacity it might hold. For example,
    ///  after performing some massive operations on a big list, call this method to ensure that all extra memory held by the
    ///  collection is released.</remarks>
    procedure Shrink();

    ///  <summary>Forces the collection to increase its capacity.</summary>
    ///  <remarks>Call this method to force the collection to increase its capacity ahead of time. Manually adjusting the capacity
    ///  can be useful in certain situations. Each collection specifies its "growing" strategy. Most collections grow by a factor of two
    ///  <c>(New Capacity = Old Capacity * 2)</c>.</remarks>
    procedure Grow();

    ///  <summary>Specifies the current capacity.</summary>
    ///  <returns>A positive number that specifies the number of elements that the collection can hold before it
    ///  needs to grow again.</returns>
    ///  <remarks>The value of this property is greater or equal to the amount of elements in the collection. If this value
    ///  if greater then the number of elements, it means that the collection has some extra capacity to operate upon.</remarks>
    property Capacity: NativeInt read GetCapacity;
  end;

{$ENDREGION}

{$REGION 'Base Collection Classes'}
type
{$HINTS OFF}
  ///  <summary>Base for all reference counted objects in this package.</summary>
  ///  <remarks><see cref="Collections.Base|TRefCountedObject">Collections.Base.TRefCountedObject</see> is designed to be used as a base class for all
  ///  objects that implement interfaces and require reference counting.</remarks>
  TRefCountedObject = class abstract(TInterfacedObject, IInterface)
  private
    FKeepAliveList: TArray<IInterface>;
    FInConstruction: Boolean;

  protected
    ///  <summary>Registers a reference counted object as as keep-alive for this object.</summary>
    ///  <param name="AObject">The object to keep alive.</param>
    ///  <remarks>If <paramref name="AObject"/> is <c>nil</c> nothing happens. Otherwise, this object is
    ///  checked to have a positive reference count. If that is the case, a new interface reference is requested
    ///  and registered internally, preventing the object from being destroyed prematurely.</remarks>
    ///  <exception cref="Collections.Base|ECannotSelfReferenceException"> if trying to keep alive self.</exception>
    procedure KeepObjectAlive(const AObject: TRefCountedObject);

    ///  <summary>Unregisters a reference counted object from the keep-alive list.</summary>
    ///  <param name="AObject">The object to unregister.</param>
    ///  <param name="AFreeObject">Specifies whether to free the object if its reference reaches is zero.</param>
    ///  <remarks>If <paramref name="AObject"/> is <c>nil</c> nothing happens. Otherwise, this object is
    ///  checked to have a positive reference count. If that is the case, the help reference is released.</remarks>
    ///  <exception cref="Collections.Base|ECannotSelfReferenceException"> if trying to release self.</exception>
    procedure ReleaseObject(const AObject: TRefCountedObject;
      const AFreeObject: Boolean = false);

    ///  <summary>Extract an interafce reference for this object.</summary>
    ///  <remarks>If the reference count is zero, then no reference is extracted.</remarks>
    ///  <returns>An interface reference or <c>nil</c>.</returns>
    function ExtractReference(): IInterface;

    ///  <summary>Specifies whether the object is currently being constructed.</summary>
    ///  <returns><c>True</c> if the object is in construction; <c>False</c> otherwise.</returns>
    property Constructing: Boolean read FInConstruction;
  public
    ///  <summary>Initializes the internals of the <see cref="Collections.Base|TRefCountedObject">Collections.Base.TRefCountedObject</see> objects.</summary>
    ///  <remarks>Do not call this method directly. It is part of the object creation process.</remarks>
    class function NewInstance: TObject; override;

    ///  <summary>Initializes the internals of the <see cref="Collections.Base|TRefCountedObject">Collections.Base.TRefCountedObject</see> objects.</summary>
    ///  <remarks>Do not call this method directly. It is part of the object creation process.</remarks>
    procedure AfterConstruction; override;
  end;
{$HINTS ON}

  ///  <summary>Procedural type used by collections to insert custom remove notification code
  ///  into inner collections.</summary>
  ///  <param name="AValue">The value being removed.</param>
  TRemoveNotification<T> = reference to procedure(const AValue: T);

  ///  <summary>Non-generic base class for all collections.</summary>
  ///  <remarks>This class provides some basics like version management and count retrieval.</remarks>
  TAbstractContainer = class abstract(TRefCountedObject)
  private
    FVersion: NativeInt;

  protected
    ///  <summary>Returns the number of elements in the collection.</summary>
    ///  <returns>A positive value specifying the number of elements in the collection.</returns>
    ///  <remarks>A call to this method can be costly because some
    ///  collections cannot detect the number of stored elements directly, resorting to enumerating themselves.</remarks>
    function GetCount(): NativeInt; virtual; abstract;

    ///  <summary>Call this method to notify the collection that it was modified.</summary>
    ///  <remarks>This method must be called by descending classes in order to update the version of the collection.</remarks>
    procedure NotifyCollectionChanged(); virtual;
  public
    const CDefaultSize = 32;

    ///  <summary>Returns the current version of the collection.</summary>
    ///  <returns>An integer value specifying the current "structural version" of the collection.</returns>
    ///  <remarks>This function returns a number that is modified by the implementing collection each time
    ///  the collection changes. This version can be used to identify if a collection has chnaged since last time it was used
    ///  in a specific piece of code.</remarks>
    function Version(): NativeInt; virtual;

    ///  <summary>Checks whether the collection is empty.</summary>
    ///  <returns><c>True</c> if the collection is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the collection is empty. It is optimized
    ///  in most collections to offer a fast response.</remarks>
    function Empty(): Boolean; virtual; abstract;

    ///  <summary>Specifies the number of elements in the collection.</summary>
    ///  <returns>A positive value specifying the number of elements in the collection.</returns>
    ///  <remarks>Accesing this property can be costly because some
    ///  collections cannot detect the number of stored elements directly, resorting to enumerating themselves.</remarks>
    property Count: NativeInt read GetCount;
  end;

  ///  <summary>Base class for all collections.</summary>
  ///  <remarks>All collections are derived from this base class. It implements most Enex operations based on
  ///  enumerability .</remarks>
  TAbstractContainer<T> = class abstract(TAbstractContainer, IContainer<T>, IEnumerable<T>)
  protected
    ///  <summary>Returns the number of elements in the collection.</summary>
    ///  <returns>A positive value specifying the number of elements in the collection.</returns>
    ///  <remarks>A call to this method can be costly because some
    ///  collections cannot detect the number of stored elements directly, resorting to enumerating themselves.</remarks>
    function GetCount(): NativeInt; override;
  public
    ///  <summary>Checks whether the collection is empty.</summary>
    ///  <returns><c>True</c> if the collection is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the collection is empty. It is optimized
    ///  in most collections to offer a fast response.</remarks>
    function Empty(): Boolean; override;

    ///  <summary>Returns the single element stored in the collection.</summary>
    ///  <returns>The element in collection.</returns>
    ///  <remarks>This method checks if the collection contains just one element, in which case it is returned.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionNotOneException">There is more than one element in the collection.</exception>
    function Single(): T; virtual;

    ///  <summary>Returns the single element stored in the collection, or a default value.</summary>
    ///  <param name="ADefault">The default value returned if there is less or more elements in the collection.</param>
    ///  <returns>The element in the collection if the condition is satisfied; <paramref name="ADefault"/> is returned otherwise.</returns>
    ///  <remarks>This method checks if the collection contains just one element, in which case it is returned. Otherwise
    ///  the value in <paramref name="ADefault"/> is returned.</remarks>
    function SingleOrDefault(const ADefault: T): T; virtual;

    ///  <summary>Copies the values stored in the collection to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the collection.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the collection.</remarks>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">There array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T); overload;

    ///  <summary>Copies the values stored in the collection to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the collection.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the collection.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">There array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload; virtual;

    ///  <summary>Creates a new Delphi array with the contents of the collection.</summary>
    ///  <remarks>The length of the new array is equal to the value of <c>Count</c> property.</remarks>
    function ToArray(): TArray<T>; virtual;

    ///  <summary>Returns a new enumerator object used to enumerate the collection.</summary>
    ///  <remarks>This method is usually called by compiler generated code. It's purpose is to create an enumerator
    ///  object that is used to actually traverse the collection.
    ///  Note that many collections generate enumerators that depend on the state of the collection. If the collection is changed
    ///  after the enumerator has been obtained, the enumerator is considered invalid. All subsequent operations on that enumerator
    ///  will throw exceptions.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<T>; virtual; abstract;

    ///  <summary>Specifies the number of elements in the collection.</summary>
    ///  <returns>A positive value specifying the number of elements in the collection.</returns>
    ///  <remarks>Accesing this property can be costly because some
    ///  collections cannot detect the number of stored elements directly, resorting to enumerating themselves.</remarks>
    property Count: NativeInt read GetCount;
  end;

  ///  <summary>Base class for all Enex enumerator objects.</summary>
  ///  <remarks>All Enex collection are expected to provide enumerators that derive from
  ///  this class.</remarks>
  TAbstractEnumerator<T> = class abstract(TRefCountedObject, IEnumerator<T>)
  private
    FCreatedAtVersion: NativeInt;
    FOwner: TAbstractContainer;
    FCurrent: T;
    FEnded: Boolean;
  protected
    ///  <summary>Specifies the owner collection.</summary>
    ///  <returns>The collection that generated this enumerator.</returns>
    property Owner: TAbstractContainer read FOwner;

    ///  <summary>Returns the current element of the enumerated collection.</summary>
    ///  <remarks>This method is the getter for <c>Current</c> property. Use the property to obtain the element instead.</remarks>
    ///  <returns>The current element of the enumerated collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionChangedException">The enumerated collection has changed.</exception>
    function GetCurrent(): T;

    ///  <summary>Implement this method to move the iterator to the next element in the collection.</summary>
    ///  <param name="ACurrent">The "next" value. Must be returned by the descending classes.</param>
    ///  <returns><c>True</c> if the iteration to the next element was successful; <c>False</c> otherwise.</returns>
    function TryMoveNext(out ACurrent: T): Boolean; virtual; abstract;
  public
    ///  <summary>Initializes an enumerator object.</summary>
    ///  <param name="AOwner">The owner collection.</param>
    ///  <remarks>Descending classes must always call this constructor in their constructor.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AOwner"/> is <c>nil</c>.</exception>
    constructor Create(const AOwner: TAbstractContainer);

    ///  <summary>Destroys this enumerator object.</summary>
    destructor Destroy; override;

    ///  <summary>Moves the enumerator to the next element of collection.</summary>
    ///  <remarks>This method is usually called by compiler generated code. Its purpose is to move the "pointer" to the next element in
    ///  the collection (if there are elements left). Also note that many specific enumerator implementations may throw various
    ///  exceptions if the enumerated collection was changed while enumerating.</remarks>
    ///  <returns><c>True</c> if the enumerator succesefully selected the next element; <c>False</c> is there are
    ///  no more elements to be enumerated.</returns>
    ///  <exception cref="Collections.Base|ECollectionChangedException">The enumerated collection has changed.</exception>
    function MoveNext(): Boolean;

    ///  <summary>Checking change of collection.</summary>
    function VersionChanged: Boolean;

    ///  <summary>Returns the current element of the enumerated collection.</summary>
    ///  <remarks>This property can only return a valid element if <c>MoveNext</c> was priorly called and returned <c>True</c>;
    ///  otherwise the behavior of this property is undefined. </remarks>
    ///  <returns>The current element of the enumerated collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionChangedException">The enumerated collection has changed.</exception>
    property Current: T read GetCurrent;
  end;

  ///  <summary>A variation of an enumerator object thet forwards all calls to an enclosed enumerator and allows filtering
  ///  the enumerated value.</summary>
  ///  <remarks>By default filtering is off, but it can be enaled by overriding the <c>AcceptValue</c> method.</remarks>
  TForwardingEnumerator<T> = class abstract(TAbstractEnumerator<T>)
  private
    FForwardEnumerator: IEnumerator<T>;
  protected
    ///  <summary>Obtains the next value from the use enumerator.</summary>
    ///  <param name="ACurrent">The "next" value. The value obtained from the forwarding enumerator.</param>
    ///  <remarks>This method calls <c>AcceptValue</c> and if the result is <c>False</c> iterates further until
    ///  a values from the enclised enumerator is accepted.</remarks>
    ///  <returns><c>True</c> if the iteration to the next element was successful; <c>False</c> otherwise.</returns>
    function TryMoveNext(out ACurrent: T): Boolean; override;

    ///  <summary>Override in descending enumerator classes to accept or reject a value provided by the
    ///  enclosed enumerator.</summary>
    ///  <param name="AValue">The value to accept or reject.</param>
    ///  <returns><c>True</c> if the value is accepted; <c>False</c> otherwise.</returns>
    ///  <remarks>The current implementation always returns <c>True</c>.</remarks>
    function AcceptValue(const AValue: T): Boolean; virtual;
  public
    ///  <summary>Initializes a fprwarding enumerator object.</summary>
    ///  <param name="AOwner">The owner collection.</param>
    ///  <param name="AEnumerator">The enumerator to forward all calls to.</param>
    ///  <remarks>Descending classes must always call this constructor in their constructor.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AOwner"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AEnumerator"/> is <c>nil</c>.</exception>
    constructor Create(const AOwner: TAbstractContainer; const AEnumerator: IEnumerator<T>);
  end;

  ///  <summary>Base class for all non-associative Enex collections.</summary>
  ///  <remarks>All normal Enex collections (ex. list or stack) are derived from this base class.
  ///  It implements the extended Enex operations based on enumerability.</remarks>
  TSequence<T> = class abstract(TAbstractContainer<T>, IComparable, ISequence<T>)
  private
    FElementRules: TRules<T>;

  protected
    ///  <summary>Compares two values for equality.</summary>
    ///  <param name="ALeft">The first value.</param>
    ///  <param name="ARight">The second value.</param>
    ///  <returns><c>True</c> if the values are equal; <c>False</c> otherwise.</returns>
    ///  <remarks>This method uses the equality comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function ElementsAreEqual(const ALeft, ARight: T): Boolean;

    ///  <summary>Compares two values.</summary>
    ///  <param name="ALeft">The first value.</param>
    ///  <param name="ARight">The second value.</param>
    ///  <returns>A value less than zero if <paramref name="ALeft"/> is less than <paramref name="ARight"/>.
    ///  A value greater than zero if <paramref name="ALeft"/> is greater than <paramref name="ARight"/>. Zero if
    ///  <paramref name="ALeft"/> is equal to <paramref name="ARight"/>.</returns>
    ///  <remarks>This method uses the comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function CompareElements(const ALeft, ARight: T): NativeInt;

    ///  <summary>Generates a hash code for the given value.</summary>
    ///  <param name="AValue">The value.</param>
    ///  <returns>The calculated hash code.</returns>
    ///  <remarks>This method uses the equality comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function GetElementHashCode(const AValue: T): NativeInt; overload;

    ///  <summary>Specifies the rule set that describes the stored elements.</summary>
    ///  <returns>A rule set describing the stored elements.</returns>
    property ElementRules: TRules<T> read FElementRules;
  public
    ///  <summary>Instantiates this class.</summary>
    ///  <remarks>The default comparer and equality comparer are requested if this constructor is used. Do not call this method if
    ///  you don't know what you are doing.</remarks>
    constructor Create(); overload;

    ///  <summary>Instantiates this class.</summary>
    ///  <param name="ARules">The rules set used by the collection.</param>
    ///  <remarks>The provided rules set is used by this collection. This constructor must be called from descendent collections.</remarks>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the collection considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Max(): T; virtual;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the collection considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Min(): T; virtual;

    ///  <summary>Returns the first element.</summary>
    ///  <returns>The first element in collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function First(): T; virtual;

    ///  <summary>Returns the first element or a default if the collection is empty.</summary>
    ///  <param name="ADefault">The default value returned if the collection is empty.</param>
    ///  <returns>The first element in collection if the collection is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function FirstOrDefault(const ADefault: T): T; virtual;

    ///  <summary>Returns the first element that satisfies the given predicate.</summary>
    ///  <param name="APredicate">The predicate to use.</param>
    ///  <returns>The first element that satisfies the given predicate.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the predicate.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function FirstWhere(const APredicate: TPredicate<T>): T; virtual;

    ///  <summary>Returns the first element that satisfies the given predicate or a default value.</summary>
    ///  <param name="APredicate">The predicate to use.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given predicate; or <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function FirstWhereOrDefault(const APredicate: TPredicate<T>; const ADefault: T): T; virtual;

    ///  <summary>Returns the first element that does not satisfy the given predicate.</summary>
    ///  <param name="APredicate">The predicate to use.</param>
    ///  <returns>The first element that does not satisfy the given predicate.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements that do not satisfy the predicate.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function FirstWhereNot(const APredicate: TPredicate<T>): T;

    ///  <summary>Returns the first element that does not satisfy the given predicate or a default value.</summary>
    ///  <param name="APredicate">The predicate to use.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that does not satisfy the given predicate; or <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function FirstWhereNotOrDefault(const APredicate: TPredicate<T>; const ADefault: T): T;

    ///  <summary>Returns the first element lower than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereLower(const ABound: T): T;

    ///  <summary>Returns the first element lower than a given value or a default.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; or <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereLowerOrDefault(const ABound: T; const ADefault: T): T;

    ///  <summary>Returns the first element lower than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereLowerOrEqual(const ABound: T): T;

    ///  <summary>Returns the first element lower than or equal to a given value or a default.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; or <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereLowerOrEqualOrDefault(const ABound: T; const ADefault: T): T;

    ///  <summary>Returns the first element greater than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereGreater(const ABound: T): T;

    ///  <summary>Returns the first element greater than a given value or a default.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; or <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereGreaterOrDefault(const ABound: T; const ADefault: T): T;

    ///  <summary>Returns the first element greater than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereGreaterOrEqual(const ABound: T): T;

    ///  <summary>Returns the first element greater than or equal to a given value or a default.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; or <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereGreaterOrEqualOrDefault(const ABound: T; const ADefault: T): T;

    ///  <summary>Returns the first element situated within the given bounds.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The higher bound.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereBetween(const ALower, AHigher: T): T;

    ///  <summary>Returns the first element situated within the given bounds or a default.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The higher bound.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; or <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereBetweenOrDefault(const ALower, AHigher: T; const ADefault: T): T;

    ///  <summary>Returns the last element.</summary>
    ///  <returns>The last element in collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Last(): T; virtual;

    ///  <summary>Returns the last element or a default if the collection is empty.</summary>
    ///  <param name="ADefault">The default value returned if the collection is empty.</param>
    ///  <returns>The last element in collection if the collection is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function LastOrDefault(const ADefault: T): T; virtual;

    ///  <summary>Aggregates a value based on the collection's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <returns>A value that contains the collection's aggregated value.</returns>
    ///  <remarks>This method returns the first element if the collection only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Aggregate(const AAggregator: TFunc<T, T, T>): T; virtual;

    ///  <summary>Aggregates a value based on the collection's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <param name="ADefault">The default value returned if the collection is empty.</param>
    ///  <returns>A value that contains the collection's aggregated value. If the collection is empty, <paramref name="ADefault"/> is returned.</returns>
    ///  <remarks>This method returns the first element if the collection only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T; virtual;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <returns>The element at the specified position.</returns>
    ///  <remarks>This method is slow for collections that cannot reference their elements by indexes, for example linked lists.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    function ElementAt(const AIndex: NativeInt): T; virtual;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <param name="ADefault">The default value returned if the collection is empty.</param>
    ///  <returns>The element at the specified position if the collection is not empty and the position is not out of bounds; otherwise
    ///  the value of <paramref name="ADefault"/> is returned.</returns>
    ///  <remarks>This method is slow for collections that cannot reference their elements by indexes, for example linked lists.</remarks>
    function ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T; virtual;

    ///  <summary>Check whether at least one element in the collection satisfies a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if at least one element satisfies a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole collection and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>True</c>. The logical equivalent of this operation is "OR".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Any(const APredicate: TPredicate<T>): Boolean; virtual;

    ///  <summary>Checks that all elements in the collection satisfies a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if all elements satisfy a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole collection and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>False</c>. The logical equivalent of this operation is "AND".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function All(const APredicate: TPredicate<T>): Boolean; virtual;

    ///  <summary>Checks whether the elements in this collection are equal to the elements in another collection.</summary>
    ///  <param name="ACollection">The collection to compare to.</param>
    ///  <returns><c>True</c> if the collections are equal; <c>False</c> if the collections are different.</returns>
    ///  <remarks>This method checks that each element at position X in this collection is equal to an element at position X in
    ///  the provided collection. If the number of elements in both collections is different, then the collections are considered different.
    ///  Note that comparison of element is done using the rule set used by this collection. This means that comparing this collection
    ///  to another one might yield a different result than comparing the other collection to this one.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function EqualsTo(const ACollection: IEnumerable<T>): Boolean; virtual;

    ///  <summary>Selects only the elements that satisfy a given rule.</summary>
    ///  <param name="APredicate">The predicate that represents the rule.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the given rule.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Where(const APredicate: TPredicate<T>): ISequence<T>;

    ///  <summary>Selects only the elements that do not satisfy a given rule.</summary>
    ///  <param name="APredicate">The predicate that represents the rule.</param>
    ///  <returns>A new collection that contains only the elements that do not satisfy the given rule.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function WhereNot(const APredicate: TPredicate<T>): ISequence<T>;

    ///  <summary>Selects only the elements that are less than a given value.</summary>
    ///  <param name="ABound">The element to compare against.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    function WhereLower(const ABound: T): ISequence<T>;

    ///  <summary>Selects only the elements that are less than or equal to a given value.</summary>
    ///  <param name="ABound">The element to compare against.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    function WhereLowerOrEqual(const ABound: T): ISequence<T>;

    ///  <summary>Selects only the elements that are greater than a given value.</summary>
    ///  <param name="ABound">The element to compare against.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    function WhereGreater(const ABound: T): ISequence<T>;

    ///  <summary>Selects only the elements that are greater than or equal to a given value.</summary>
    ///  <param name="ABound">The element to compare against.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    function WhereGreaterOrEqual(const ABound: T): ISequence<T>;

    ///  <summary>Selects only the elements whose values are contained whithin a given interval.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The upper bound.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    ///  <remarks>The elements that are equal to the lower or upper bound are also included.</remarks>
    function WhereBetween(const ALower, AHigher: T): ISequence<T>;

    ///  <summary>Selects all the elements from the collection excluding duplicates.</summary>
    ///  <returns>A new collection that contains the distinct elements.</returns>
    function Distinct(): ISequence<T>; virtual;

    ///  <summary>Returns a new ordered collection that contains the elements from this collection.</summary>
    ///  <param name="AAscending">Specifies whether the elements are ordered in an ascending or descending way.</param>
    ///  <returns>A new ordered collection.</returns>
    function Ordered(const AAscending: Boolean = true): ISequence<T>; overload; virtual;

    ///  <summary>Returns a new ordered collection that contains the elements from this collection.</summary>
    ///  <param name="ASortProc">The comparison method.</param>
    ///  <returns>A new ordered collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASortProc"/> is <c>nil</c>.</exception>
    function Ordered(const ASortProc: TComparison<T>): ISequence<T>; overload; virtual;

    ///  <summary>Revereses the contents of the collection.</summary>
    ///  <returns>A new collection that contains the elements from this collection but in reverse order.</returns>
    function Reversed(): ISequence<T>; virtual;

    ///  <summary>Concatenates this collection with another collection.</summary>
    ///  <param name="ACollection">A collection to concatenate.</param>
    ///  <returns>A new collection that contains the elements from this collection followed by elements
    ///  from the given collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function Concat(const ACollection: ISequence<T>): ISequence<T>;

    ///  <summary>Creates a new collection that contains the elements from both collections, taken a single time.</summary>
    ///  <param name="ACollection">The collection to unify with.</param>
    ///  <returns>A new collection that contains the elements from this collection followed by elements
    ///  from the given collection except the elements that already are present in this collection. This operation can be seen as
    ///  a "concat" operation followed by a "distinct" operation. </returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function Union(const ACollection: ISequence<T>): ISequence<T>;

    ///  <summary>Creates a new collection that contains the elements from this collection minus the ones in the given collection.</summary>
    ///  <param name="ACollection">The collection to exclude.</param>
    ///  <returns>A new collection that contains the elements from this collection minus those elements that are common between
    ///  this and the given collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function Exclude(const ACollection: ISequence<T>): ISequence<T>;

    ///  <summary>Creates a new collection that contains the elements that are present in both collections.</summary>
    ///  <param name="ACollection">The collection to interset with.</param>
    ///  <returns>A new collection that contains the elements that are common to both collections.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function Intersect(const ACollection: ISequence<T>): ISequence<T>;

    ///  <summary>Select the elements whose indexes are located in the given range.</summary>
    ///  <param name="AStart">The lower bound.</param>
    ///  <param name="AEnd">The upper bound.</param>
    ///  <returns>A new collection that contains the elements whose indexes in this collection are located between <paramref name="AStart"/>
    ///  and <paramref name="AEnd"/>. Note that this method does not check the indexes. This means that a bad combination of parameters will
    ///  simply result in an empty or incorrect result.</returns>
    function Range(const AStart, AEnd: NativeInt): ISequence<T>;

    ///  <summary>Selects only a given amount of elements.</summary>
    ///  <param name="ACount">The number of elements to select.</param>
    ///  <returns>A new collection that contains only the first <paramref name="ACount"/> elements.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="ACount"/> is zero.</exception>
    function Take(const ACount: NativeInt): ISequence<T>;

    ///  <summary>Selects all the elements from the collection while a given rule is satisfied.</summary>
    ///  <param name="APredicate">The rule to satisfy.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function TakeWhile(const APredicate: TPredicate<T>): ISequence<T>;

    ///  <summary>Selects all the elements from the collection while elements are lower than a given value.</summary>
    ///  <param name="ABound">The value to check against.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileLower(const ABound: T): ISequence<T>;

    ///  <summary>Selects all the elements from the collection while elements are lower than
    ///  or equal to a given value.</summary>
    ///  <param name="ABound">The value to check against.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileLowerOrEqual(const ABound: T): ISequence<T>;

    ///  <summary>Selects all the elements from the collection while elements are greater than
    ///  a given value.</summary>
    ///  <param name="ABound">The value to check against.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileGreater(const ABound: T): ISequence<T>;

    ///  <summary>Selects all the elements from the collection while elements are greater than
    ///  or equal to a given value.</summary>
    ///  <param name="ABound">The value to check against.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileGreaterOrEqual(const ABound: T): ISequence<T>;

    ///  <summary>Selects all the elements from the collection while elements are between a given range of values.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The higher bound.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileBetween(const ALower, AHigher: T): ISequence<T>;

    ///  <summary>Skips a given amount of elements.</summary>
    ///  <param name="ACount">The number of elements to skip.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="ACount"/> is zero.</exception>
    function Skip(const ACount: NativeInt): ISequence<T>;

    ///  <summary>Skips all the elements from the collection while a given rule is satisfied.</summary>
    ///  <param name="APredicate">The rule to satisfy.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function SkipWhile(const APredicate: TPredicate<T>): ISequence<T>;

    ///  <summary>Skips all the elements from the collection while elements are lower than a given value.</summary>
    ///  <param name="ABound">The value to check.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileLower(const ABound: T): ISequence<T>;

    ///  <summary>Skips all the elements from the collection while elements are lower than or equal to a given value.</summary>
    ///  <param name="ABound">The value to check.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileLowerOrEqual(const ABound: T): ISequence<T>;

    ///  <summary>Skips all the elements from the collection while elements are greater than a given value.</summary>
    ///  <param name="ABound">The value to check.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileGreater(const ABound: T): ISequence<T>;

    ///  <summary>Skips all the elements from the collection while elements are greater than or equal to a given value.</summary>
    ///  <param name="ABound">The value to check.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileGreaterOrEqual(const ABound: T): ISequence<T>;

    ///  <summary>Skips all the elements from the collection while elements are between a given range of values.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The higher bound.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileBetween(const ALower, AHigher: T): ISequence<T>;

    ///  <summary>Exposes a type that provides extended Enex operations such as "select".</summary>
    ///  <returns>A record that exposes more Enex operations that otherwise would be impossible.</returns>
    function Op: TEnexExtOps<T>;

    ///  <summary>Creates a new list containing the elements of this collection.</summary>
    ///  <returns>A list containing the elements copied from this collection.</returns>
    ///  <remarks>This method also copies the rule set of this collection. Be careful if the rule set
    ///  performs cleanup on the elements.</remarks>
    function ToList(): IList<T>;

    ///  <summary>Creates a new set containing the elements of this collection.</summary>
    ///  <returns>A set containing the elements copied from this collection.</returns>
    ///  <remarks>This method also copies the rule set of this collection. Be careful if the rule set
    ///  performs cleanup on the elements.</remarks>
    function ToSet(): ISet<T>;

    ///  <summary>Compares the elements in this collection to another collection.</summary>
    ///  <param name="AObject">The instance to compare against.</param>
    ///  <returns>An integer value depicting the result of the comparison operation.
    ///  If the result is less than zero, <c>Self</c> is less than <paramref name="AObject"/>. If the result is zero,
    ///  <c>Self</c> is equal to <paramref name="AObject"/>. And finally, if the result is greater than zero, <c>Self</c> is greater
    ///  than <paramref name="AObject"/>.</returns>
    function CompareTo(AObject: TObject): Integer;

    ///  <summary>Generates the hash code of all the elements in the collection.</summary>
    ///  <returns>An integer value representing the hash codes of all the elements in the collection.</returns>
    function GetHashCode(): Integer; override;

    ///  <summary>Checks whether this collection is equal to another collection.</summary>
    ///  <param name="Obj">The collection to check against.</param>
    ///  <returns><c>True</c> if the collections are equal; <c>False</c> otherwise.</returns>
    ///  <remarks>This method checks whether <paramref name="Obj"/> is not <c>nil</c>, and that
    ///  <paramref name="Obj"/> is an Enex collection. Then, elements are checked for equality one by one.</remarks>
    function Equals(Obj: TObject): Boolean; override;

    ///  <summary>Generates a new collection that contains a given value for a given number of times.</summary>
    ///  <param name="AElement">The element to fill the collection with.</param>
    ///  <param name="ACount">The number of times the element is present in the collection (the length of the collection).</param>
    ///  <param name="ARules">The rule set describing the elements in the new collection.</param>
    ///  <returns>A new collection containing the <paramref name="AElement"/>, <paramref name="ACount"/> times.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AElement"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="ACount"/> is zero or less.</exception>
    class function Fill(const AElement: T; const ACount: NativeInt; const ARules: TRules<T>): ISequence<T>; overload; static;

    ///  <summary>Generates a new collection that contains a given value for a given number of times.</summary>
    ///  <param name="AElement">The element to fill the collection with.</param>
    ///  <param name="ACount">The number of times the element is present in the collection (the length of the collection).</param>
    ///  <returns>A new collection containing the <paramref name="AElement"/>, <paramref name="ACount"/> times.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="ACount"/> is zero or less.</exception>
    class function Fill(const AElement: T; const ACount: NativeInt): ISequence<T>; overload; static;
  end;

  ///  <summary>The base abstract class for simple (non-associative) collections.</summary>
  ///  <remarks>This collection exposes some operations that need to be implemented in descending classes and some
  ///  default implementations using Enex operations.</remarks>
  TCollection<T> = class abstract(TSequence<T>, ICollection<T>)
  private
    FRemoveNotification: TRemoveNotification<T>;

  protected
    ///  <summary>Override in descendant classed to properly handle elements that are removed from
    ///  the collection.</summary>
    ///  <param name="AElement">The element being removed.</param>
    ///  <remarks>This method is called by the collection when an element is removed and the caller has
    ///  no possibility of obtaining it. For example, a call to <c>Clear</c> calls this method for each element
    ///  of the collection.</remarks>
    procedure HandleElementRemoved(const AElement: T); virtual;

    ///  <summary>Call this method in descendant collections to properly invoke the removal mechanism.</summary>
    ///  <param name="AElement">The element being removed.</param>
    ///  <remarks>This method verifies if a custom removal notification is registered and calls it. Otherwise the normal
    ///  removal mechanisms are involved.</remarks>
    procedure NotifyElementRemoved(const AElement: T);
  public
    ///  <summary>Destroys this instance.</summary>
    ///  <remarks>Do not call this method directly; call <c>Free</c> instead.</remarks>
    destructor Destroy(); override;

    ///  <summary>Specifies a custom remove notification method that will be called by this
    ///  collection when elements are removed.</summary>
    ///  <returns>The notification method.</returns>
    property RemoveNotification: TRemoveNotification<T> read FRemoveNotification write FRemoveNotification;

    ///  <summary>Clears the contents of the collection.</summary>
    ///  <remarks>This implementation uses Enex <c>First</c> operation to obtain the first element and then calls <c>Remove</c> to remove it.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>Remove</c> method is not overridden.</exception>
    procedure Clear(); virtual;

    ///  <summary>Appends an element to the back of the list.</summary>
    ///  <param name="AValue">The value to append.</param>
    ///  <exception cref="Generics.Collections|ENotSupportedException">Always raised in this implementation.</exception>
    procedure Add(const AValue: T); virtual;

    ///  <summary>Adds all the elements from a collection to this collection.</summary>
    ///  <param name="ACollection">The values to add.</param>
    ///  <remarks>Where exactly the elements are added is unspecified and depends on the implementing collection. This method calls <c>Add</c> for each element
    ///  in the supplied collection. For most descending collections this is OK.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>Add</c> method is not overridden.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    procedure AddAll(const ACollection: IEnumerable<T>); virtual;

    ///  <summary>Removes a given value from the collection.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>If the collection does not contain the given value, nothing happens.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">Always raised in this implementation.</exception>
    procedure Remove(const AValue: T); virtual;

    ///  <summary>Removes all the elements from a collection that are also found in this collection.</summary>
    ///  <param name="ACollection">The values to remove.</param>
    ///  <remarks>This implementation calls <c>Remove</c> for each element in the collection.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>Remove</c> method is not overridden.</exception>
    procedure RemoveAll(const ACollection: IEnumerable<T>); virtual;

    ///  <summary>Checks whether a specified element is contained in this collection.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the value was found in the collection; <c>False</c> otherwise.</returns>
    ///  <remarks>The implementation in this class iterates over all elements and checks for the requested
    ///  value. Most descendant classes will likely provide a better version.</remarks>
    function Contains(const AValue: T): Boolean; virtual;

    ///  <summary>Checks whether all the elements from a specified collection are contained in this collection.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the values were found in the collection; <c>False</c> otherwise.</returns>
    ///  <remarks>The current implementation calls <c>Contains</c> on each individual element from the supplied collection.</remarks>
    function ContainsAll(const ACollection: IEnumerable<T>): Boolean; virtual;
  end;

  ///  <summary>Base class for all associative Enex collections.</summary>
  ///  <remarks>All associative collections (ex. dictionary or multi-map) are derived from this base class.
  ///  It implements the extended Enex operations based on enumerability.</remarks>
  TAssociation<TKey, TValue> = class abstract(TAbstractContainer<TPair<TKey, TValue>>, IAssociation<TKey, TValue>)
  private
    FKeyRules: TRules<TKey>;
    FValueRules: TRules<TValue>;
    FKeyRemoveNotification: TRemoveNotification<TKey>;
    FValueRemoveNotification: TRemoveNotification<TValue>;

  protected
    ///  <summary>Specifies a custom remove notification method that will be called by this
    ///  collection when keys are removed.</summary>
    ///  <returns>The notification method.</returns>
    property KeyRemoveNotification: TRemoveNotification<TKey> read FKeyRemoveNotification write FKeyRemoveNotification;

    ///  <summary>Specifies a custom remove notification method that will be called by this
    ///  collection when values are removed.</summary>
    ///  <returns>The notification method.</returns>
    property ValueRemoveNotification: TRemoveNotification<TValue> read FValueRemoveNotification write FValueRemoveNotification;

    ///  <summary>Compares two keys for equality.</summary>
    ///  <param name="ALeft">The first key.</param>
    ///  <param name="ARight">The second key.</param>
    ///  <returns><c>True</c> if the keys are equal; <c>False</c> otherwise.</returns>
    ///  <remarks>This method uses the equality comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function KeysAreEqual(const ALeft, ARight: TKey): Boolean;

    ///  <summary>Compares two keys.</summary>
    ///  <param name="ALeft">The first key.</param>
    ///  <param name="ARight">The second key.</param>
    ///  <returns>A value less than zero if <paramref name="ALeft"/> is less than <paramref name="ARight"/>.
    ///  A value greater than zero if <paramref name="ALeft"/> is greater than <paramref name="ARight"/>. Zero if
    ///  <paramref name="ALeft"/> is equal to <paramref name="ARight"/>.</returns>
    ///  <remarks>This method uses the comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function CompareKeys(const ALeft, ARight: TKey): NativeInt;

    ///  <summary>Generates a hash code for the given key.</summary>
    ///  <param name="AValue">The key.</param>
    ///  <returns>The calculated hash code.</returns>
    ///  <remarks>This method uses the equality comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function GetKeyHashCode(const AValue: TKey): NativeInt; overload;

    ///  <summary>Compares two values for equality.</summary>
    ///  <param name="ALeft">The first value.</param>
    ///  <param name="ARight">The second value.</param>
    ///  <returns><c>True</c> if the keys are equal; <c>False</c> otherwise.</returns>
    ///  <remarks>This method uses the equality comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function ValuesAreEqual(const ALeft, ARight: TValue): Boolean;

    ///  <summary>Compares two values.</summary>
    ///  <param name="ALeft">The first value.</param>
    ///  <param name="ARight">The second value.</param>
    ///  <returns>A value less than zero if <paramref name="ALeft"/> is less than <paramref name="ARight"/>.
    ///  A value greater than zero if <paramref name="ALeft"/> is greater than <paramref name="ARight"/>. Zero if
    ///  <paramref name="ALeft"/> is equal to <paramref name="ARight"/>.</returns>
    ///  <remarks>This method uses the comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function CompareValues(const ALeft, ARight: TValue): NativeInt;

    ///  <summary>Generates a hash code for the given value.</summary>
    ///  <param name="AValue">The value.</param>
    ///  <returns>The calculated hash code.</returns>
    ///  <remarks>This method uses the equality comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function GetValueHashCode(const AValue: TValue): NativeInt; overload;

    ///  <summary>Specifies the rule set that describes the keys of the stored pairs.</summary>
    ///  <returns>A rule set describing the keys.</returns>
    property KeyRules: TRules<TKey> read FKeyRules;

    ///  <summary>Specifies the rule set that describes the values of the stored pairs.</summary>
    ///  <returns>A rule set describing the values.</returns>
    property ValueRules: TRules<TValue> read FValueRules;

    ///  <summary>Override in descendent classed to properly handle keys that are removed from
    ///  the collection.</summary>
    ///  <param name="AKey">The key being removed.</param>
    ///  <remarks>This method is called by the collection when a key is removed and the caller has
    ///  no possibility of obtaining it. For example, a call to <c>Clear</c> calls this method for each key
    ///  of the collection.</remarks>
    procedure HandleKeyRemoved(const AKey: TKey); virtual;

    ///  <summary>Override in descendaet classed to properly handle values that are removed from
    ///  the collection.</summary>
    ///  <param name="AValue">The key being removed.</param>
    ///  <remarks>This method is called by the collection when a value is removed and the caller has
    ///  no possibility of obtaining it. For example, a call to <c>Clear</c> calls this method for each value
    ///  of the collection.</remarks>
    procedure HandleValueRemoved(const AValue: TValue); virtual;

    ///  <summary>Call this method in descendent collections to properly invoke the removal mechanism.</summary>
    ///  <param name="AKey">The key being removed.</param>
    ///  <remarks>This method verifies whether a custom removal notification is registered and calls it. Otherwise the normal
    ///  removal mechanisms are involved.</remarks>
    procedure NotifyKeyRemoved(const AKey: TKey);

    ///  <summary>Call this method in descendent collections to properly invoke the removal mechanism.</summary>
    ///  <param name="AValue">The key being removed.</param>
    ///  <remarks>This method verifies whether a custom removal notification is registered and calls it. Otherwise the normal
    ///  removal mechanisms are involved.</remarks>
    procedure NotifyValueRemoved(const AValue: TValue);
  public
    ///  <summary>Instantiates this class.</summary>
    ///  <remarks>The default comparer and equality comparer are requested if this constructor is used. Do not call this method if
    ///  you don't know what you are doing.</remarks>
    constructor Create(); overload;

    ///  <summary>Instantiates this class.</summary>
    ///  <param name="AKeyRules">The rules set used by the collection for its keys.</param>
    ///  <param name="AValueRules">The rules set used by the collection for its values.</param>
    ///  <remarks>The provided rules set is used by this collection. This constructor must be called from descendent collections.</remarks>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to return the associated value.</param>
    ///  <returns>The value associated with the given key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">No such key in the collection.</exception>
    function ValueForKey(const AKey: TKey): TValue; virtual;

    ///  <summary>Checks whether the collection contains a given key-value pair.</summary>
    ///  <param name="AKey">The key part of the pair.</param>
    ///  <param name="AValue">The value part of the pair.</param>
    ///  <returns><c>True</c> if the given key-value pair exists; <c>False</c> otherwise.</returns>
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean; virtual;

    ///  <summary>Returns the biggest key.</summary>
    ///  <returns>The biggest key stored in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function MaxKey(): TKey; virtual;

    ///  <summary>Returns the smallest key.</summary>
    ///  <returns>The smallest key stored in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function MinKey(): TKey; virtual;

    ///  <summary>Returns the biggest value.</summary>
    ///  <returns>The biggest value stored in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function MaxValue(): TValue; virtual;

    ///  <summary>Returns the smallest value.</summary>
    ///  <returns>The smallest value stored in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function MinValue(): TValue; virtual;

    ///  <summary>Checks whether this collection includes the key-value pairs in another collection.</summary>
    ///  <param name="ACollection">The collection to check against.</param>
    ///  <returns><c>True</c> if this collection includes the elements in another; <c>False</c> otherwise.</returns>
    function Includes(const ACollection: IEnumerable<TPair<TKey, TValue>>): Boolean; virtual;

    ///  <summary>Returns an Enex collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the collection.</returns>
    function SelectKeys(): ISequence<TKey>; virtual;

    ///  <summary>Returns an Enex collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the collection.</returns>
    function SelectValues(): ISequence<TValue>; virtual;

    ///  <summary>Selects all the key-value pairs from the collection excluding the duplicates by key.</summary>
    ///  <returns>A new collection that contains the distinct pairs.</returns>
    function DistinctByKeys(): IAssociation<TKey, TValue>;

    ///  <summary>Selects all the key-value pairs from the collection excluding the duplicates by value.</summary>
    ///  <returns>A new collection that contains the distinct pairs.</returns>
    function DistinctByValues(): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs that satisfy a given rule.</summary>
    ///  <param name="APredicate">The predicate that represents the rule.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the given rule.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Where(const APredicate: TPredicate<TKey, TValue>): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs that do not satisfy a given rule.</summary>
    ///  <param name="APredicate">The predicate that represents the rule.</param>
    ///  <returns>A new collection that contains only the pairs that do not satisfy the given rule.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function WhereNot(const APredicate: TPredicate<TKey, TValue>): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are less than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyLower(const ABound: TKey): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are less than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyLowerOrEqual(const ABound: TKey): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are greater than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyGreater(const ABound: TKey): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are greater than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyGreaterOrEqual(const ABound: TKey): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are contained whithin a given interval.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The upper bound.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyBetween(const ALower, AHigher: TKey): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are less than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueLower(const ABound: TValue): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are less than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueLowerOrEqual(const ABound: TValue): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are greater than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueGreater(const ABound: TValue): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are greater than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueGreaterOrEqual(const ABound: TValue): IAssociation<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are contained whithin a given interval.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The upper bound.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueBetween(const ALower, AHigher: TValue): IAssociation<TKey, TValue>;

    ///  <summary>Creates a new dictionary containing the elements of this collection.</summary>
    ///  <returns>A dictionary containing the elements copied from this collection.</returns>
    ///  <remarks>This method also copies the rule set of this collection. Be careful if the rule set
    ///  performs cleanup on the elements.</remarks>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The collection contains more than
    ///  one key-value pair with the same key.</exception>
    function ToDictionary(): IDictionary<TKey, TValue>;
  end;

  ///  <summary>The base abstract class for associtative collections.</summary>
  ///  <remarks>This collection exposes some operations that need to be implemented in descending classes and some
  ///  default implementations using Enex operations.</remarks>
  TAbstractMap<TKey, TValue> = class abstract(TAssociation<TKey, TValue>, IMap<TKey, TValue>)
  public
    ///  <summary>Destroys this instance.</summary>
    ///  <remarks>Do not call this method directly; call <c>Free</c> instead.</remarks>
    destructor Destroy(); override;

    ///  <summary>Clears the contents of the collection.</summary>
    ///  <remarks>This implementation uses Enex <c>First</c> operation on collection's keys to obtain key and then calls <c>Remove</c> to remove it along side its value.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>Remove</c> method is not overridden.</exception>
    procedure Clear(); virtual;

    ///  <summary>Adds a key-value pair to the map.</summary>
    ///  <param name="APair">The key-value pair to add.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The map already contains a pair with the given key.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">Always raised in this implementation.</exception>
    procedure Add(const APair: TPair<TKey, TValue>); overload;

    ///  <summary>Adds a key-value pair to the map.</summary>
    ///  <param name="AKey">The key of pair.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The map already contains a pair with the given key.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">Always raised in this implementation.</exception>
    procedure Add(const AKey: TKey; const AValue: TValue); overload; virtual;

    ///  <summary>Adds a collection of key-value pairs to the map.</summary>
    ///  <param name="ACollection">The collection to add.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The map already contains a pair with the given key.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>Add</c> method is not overridden.</exception>
    procedure AddAll(const ACollection: IEnumerable<TPair<TKey, TValue>>); virtual;

    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key of pair.</param>
    ///  <remarks>If the specified key was not found in the map, nothing happens.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">Always raised in this implementation.</exception>
    procedure Remove(const AKey: TKey); virtual;

    ///  <summary>Checks whether the map contains a key-value pair identified by the given key.</summary>
    ///  <param name="AKey">The key to check for.</param>
    ///  <returns><c>True</c> if the map contains a pair identified by the given key; <c>False</c> otherwise.</returns>
    ///  <remarks>This implementation uses Enex operations to lookup the key. Most derived collections will override this method.</remarks>
    function ContainsKey(const AKey: TKey): Boolean; virtual;

    ///  <summary>Checks whether the map contains a key-value pair that contains a given value.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the map contains a pair containing the given value; <c>False</c> otherwise.</returns>
    ///  <remarks>This implementation uses Enex operations to lookup the key. Most derived collections will override this method.</remarks>
    function ContainsValue(const AValue: TValue): Boolean; virtual;
  end;

  {$ENDREGION}

{$REGION 'Exception Support'}
type
  ///  <summary>Thrown when an attempt to call an unsupported default parameterless constructor is made.</summary>
  EDefaultConstructorNotAllowed = class(Exception);

  ///  <summary>Thrown when a <see cref="Collections.Base|TRefCountedObject">Collections.Base.TRefCountedObject</see> tries to keep itself alive.</summary>
  ECannotSelfReferenceException = class(Exception);

{$IF RTLVersion < 22}
  ///  <summary>Thrown when a given argument is <c>nil</c>.</summary>
  ///  <remarks>This exception is normally provided by Delphi XE's SysUtils.pas.</remarks>
  EArgumentNilException = class(EArgumentException);
{$IFEND}

  ///  <summary>Thrown when a given argument combination specifies a smaller range than required.</summary>
  ///  <remarks>This exception is usually used by collections. The exception is thrown when there is not enough
  ///  space in an array to copy the values to.</remarks>
  EArgumentOutOfSpaceException = class(EArgumentOutOfRangeException);

  ///  <summary>Represents all exceptions that are thrown when collections are involved.</summary>
  ECollectionException = class(Exception);

  ///  <summary>Thrown when an enumerator detects that the enumerated collection was changed.</summary>
  ECollectionChangedException = class(ECollectionException);

  ///  <summary>Thrown when a collection was identified to be empty (and it shouldn't have been).</summary>
  ECollectionEmptyException = class(ECollectionException);

  ///  <summary>Thrown when a collection was expected to have only one element, but more than one was found.</summary>
  ECollectionNotOneException = class(ECollectionException);

  ///  <summary>Thrown when a predicated applied to a collection generates a void collection.</summary>
  ECollectionFilteredEmptyException = class(ECollectionException);

  ///  <summary>Thrown when trying to add a key-value pair into a collection that already has that key
  ///  in it.</summary>
  EDuplicateKeyException = class(ECollectionException);

  ///  <summary>Thrown for a serialization or deserialization error.</summary>
  ESerializationException = class(Exception);

  ///  <summary>Thrown when the key (of a pair) is not found in the collection.</summary>
  EKeyNotFoundException = class(ECollectionException);

  ///  <summary>A static class that offers methods for throwing exceptions.</summary>
  ///  <remarks><see cref="Collections.Base|ExceptionHelper">Collections.Base.ExceptionHelper</see> is used internally in this package to
  ///  throw all kinds of exceptions. This class is useful because it separates the exceptions
  ///  (including the messages) from the rest of the code.</remarks>
  ExceptionHelper = class sealed
  public
    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_CannotSelfReferenceError();

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_ArgumentNilError(const ArgName: String);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_ArgumentOutOfRangeError(const ArgName: String);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_ArgumentOutOfSpaceError(const ArgName: String);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_OperationNotSupported(const AOperation: String);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_CollectionChangedError();

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_CollectionEmptyError();

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_CollectionHasMoreThanOneElement();

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_CollectionHasNoFilteredElements();

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_DuplicateKeyError(const ArgName: String);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_KeyNotFoundError(const ArgName: String);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_TypeNotAClassError(const TypeName: String);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_TypeDoesNotExposeMember(const MemberName: String);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_TypeCannotBeSerialized(const ATypeInfo: PTypeInfo);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_TypeDoesNotHaveEnoughRtti(const ATypeInfo: PTypeInfo);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_FieldTypeDoesNotHaveEnoughRtti(const AField: TRttiField);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_BadDynamicArrayReference(const ATypeInfo: PTypeInfo);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_BadRecordReference(const ATypeInfo: PTypeInfo);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_BadClassReference(const ATypeInfo: PTypeInfo);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_ExpectedAnotherBinaryValuePoint(); static;

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_ExpectedAnotherField(const AExpected: TRttiField; const AName: string; AOffset: Int64);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_ExpectedAnotherLabel(const AExpectedLabel, AActualLabel: string);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_ExpectedAnotherElementCount(const AArrayType: TRttiArrayType; const AExpectedCount, AActualCount: NativeInt);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_ExpectedAnotherType(const AExpected: TRttiType; const AActual: string);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_ExpectedAnotherSetSize(const AExpectedSize, AActualSize: NativeInt);
  end;

resourcestring
  SDefaultParameterlessCtorNotAllowed = 'Default parameterless constructor not allowed!';
  SCannotSelfReference = 'The object cannot self-reference!';
  SNilArgument = 'Argument "%s" is nil. Expected a normal non-disposed object!';
  SOutOfRangeArgument = 'Argument "%s" is out of range. An argument that falls into the required range of values is expected!';
  SOutOfSpaceArgument = 'Argument "%s" does not have enough space to hold the result!';
  SParentCollectionChanged = 'Parent collection has changed. Cannot continue the operation!';
  SKeyNotFound = 'The key given by the "%s" argument was not found in the collection!';
  SDuplicateKey = 'The key given by the "%s" argument was already registered in the collection!';
  SEmptyCollection = 'The collection is empty! The operation cannot be performed!';
  SOperationNotSupported = 'The request collection operation %s is not supported by this instance.';
  SCollectionHasMoreThanOneElements = 'The collection has more than one element!';
  SCollectionHasNoFilteredElements = 'The applied predicate generates a void collection.';
  STypeNotAClass = 'The type "%s" on which the operation was invoked is not a class!';
  STypeDoesNotExposeMember = 'The type the collection operates on does not expose member "%s"!';
  STypeCannotBeSerialized = 'Serialization for values of type %s (kind: %s) is not supported.';
  STypeDoesNotHaveEnoughRtti = 'Type %s (kind %s) does not have enough RTTI to be serializable.';
  SFieldTypeDoesNotHaveEnoughRtti = 'Field %s member of type %s (kind %s) does not have enough RTTI to be serializable.';
  SBadDynamicArrayReference = 'Dynamic array of type %s (kind %s) cannot be deserialized because it is a reference to an unavailable dynamic array.';
  SBadRecordReference = 'Record of type %s (kind %s) cannot be deserialized because it is a reference to an unavailable record.';
  SBadClassReference = 'Class of type %s (kind %s) cannot be deserialized because it is a reference to an unavailable class.';
  SExpectedAnotherBinaryValuePoint = 'Found a binary stream point that was unexpected while deserializing.';
  SExpectedAnotherField = 'Expected a field %s with offset %d, but got a field %s with offset %d!';
  SExpectedAnotherLabel = 'Expected a label %s , but got a label %s!';
  SExpectedAnotherElementCount = 'Expected a static array of type %s with %d elements but got %d elements.';
  SExpectedAnotherType = 'Expected a type %s, but got another type %s!';
  SExpectedAnotherSetSize = 'When deserializing a set expected it to have %d bytes but got %d bytes!';
{$ENDREGION}

{$REGION 'Enex Internal Enumerables'}
type
  //private type
    TWhereSequence<T> = class sealed(TSequence<T>)
    private type
      TEnumerator = class(TForwardingEnumerator<T>)
      public
        function AcceptValue(const AValue: T): Boolean; override;
      end;

    private
      FCollection: TSequence<T>;
      FPredicate: TPredicate<T>;
      FInvertResult: Boolean;

    public
      constructor Create(const ACollection: TSequence<T>;
        const APredicate: TPredicate<T>; const AInvertResult: Boolean); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<T>; override;
    end;

    TSelectSequence<T, TOut> = class sealed(TSequence<TOut>)
    private type
      TEnumerator = class(TAbstractEnumerator<TOut>)
      private
        FInEnumerator: IEnumerator<T>;
      public
        function TryMoveNext(out ACurrent: TOut): Boolean; override;
      end;

    private
      FCollection: TSequence<T>;
      FSelector: TFunc<T, TOut>;

    protected
      function GetCount(): NativeInt; override;
    public
      constructor Create(const ACollection: TSequence<T>; const ASelector: TFunc<T, TOut>; const ARules: TRules<TOut>); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<TOut>; override;
      function Empty(): Boolean; override;
      function First(): TOut; override;
      function Last(): TOut; override;
      function Single(): TOut; override;
      function ElementAt(const AIndex: NativeInt): TOut; override;
    end;

    TSelectClassSequence<T, TOut: class> = class sealed(TSequence<TOut>)
    private type
      TEnumerator = class(TAbstractEnumerator<TOut>)
      private
        FInEnumerator: IEnumerator<T>;
      public
        function TryMoveNext(out ACurrent: TOut): Boolean; override;
      end;

    private
      FCollection: TSequence<T>;

    public
      constructor Create(const ACollection: TSequence<T>; const ARules: TRules<TOut>); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<TOut>; override;
    end;

    TConcatSequence<T> = class sealed(TSequence<T>)
    private type
      TEnumerator = class(TAbstractEnumerator<T>)
      private
        FInEnumerator1, FInEnumerator2: IEnumerator<T>;
      public
        function TryMoveNext(out ACurrent: T): Boolean; override;
      end;

    private
      FCollection1: TSequence<T>;
      FCollection2: ISequence<T>;
    protected
      function GetCount(): NativeInt; override;

    public
      constructor Create(const ACollection1: TSequence<T>; const ACollection2: ISequence<T>); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<T>; override;
      function Empty(): Boolean; override;
      function Any(const APredicate: TPredicate<T>): Boolean; override;
      function All(const APredicate: TPredicate<T>): Boolean; override;
    end;

    TUnionSequence<T> = class sealed(TSequence<T>)
    private type
      TEnumerator = class(TAbstractEnumerator<T>)
      private
        FInEnumerator1, FInEnumerator2: IEnumerator<T>;
        FSet: ISet<T>;
      public
        function TryMoveNext(out ACurrent: T): Boolean; override;
      end;

    private
      FCollection1: TSequence<T>;
      FCollection2: ISequence<T>;
    public
      constructor Create(const ACollection1: TSequence<T>; const ACollection2: ISequence<T>); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<T>; override;
    end;

    TExclusionSequence<T> = class sealed(TSequence<T>)
    private type
      TEnumerator = class(TAbstractEnumerator<T>)
      private
        FInEnumerator1, FInEnumerator2: IEnumerator<T>;
        FSet: ISet<T>;
      public
        function TryMoveNext(out ACurrent: T): Boolean; override;
      end;

    private
      FCollection1: TSequence<T>;
      FCollection2: ISequence<T>;
    public
      constructor Create(const ACollection1: TSequence<T>; const ACollection2: ISequence<T>); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<T>; override;
    end;

    TIntersectionSequence<T> = class sealed(TSequence<T>)
    private type
      TEnumerator = class(TAbstractEnumerator<T>)
      private
        FInEnumerator1, FInEnumerator2: IEnumerator<T>;
        FSet: ISet<T>;
      public
        function TryMoveNext(out ACurrent: T): Boolean; override;
      end;

    private
      FCollection1: TSequence<T>;
      FCollection2: ISequence<T>;
    public
      constructor Create(const ACollection1: TSequence<T>; const ACollection2: ISequence<T>); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<T>; override;
    end;

    TDistinctSequence<T> = class sealed(TSequence<T>)
    private type
      TEnumerator = class(TForwardingEnumerator<T>)
      private
        FSet: ISet<T>;
      public
        function AcceptValue(const AValue: T): Boolean; override;
      end;

    private
      FCollection: TSequence<T>;

    public
      constructor Create(const ACollection: TSequence<T>); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<T>; override;
    end;

    TRangeSequence<T> = class sealed(TSequence<T>)
    private type
      TEnumerator = class(TAbstractEnumerator<T>)
      private
        FInEnumerator: IEnumerator<T>;
        FCurrentIndex: NativeInt;
      public
        function TryMoveNext(out ACurrent: T): Boolean; override;
      end;

    private
      FStart, FEnd: NativeInt;
      FCollection: TSequence<T>;

    public
      constructor Create(const ACollection: TSequence<T>; const AStart, AEnd: NativeInt); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<T>; override;
    end;

    TSkipSequence<T> = class sealed(TSequence<T>)
    private type
      TEnumerator = class(TForwardingEnumerator<T>)
      private
        FCurrentIndex: NativeInt;
      public
        function AcceptValue(const AValue: T): Boolean; override;
      end;

    private
      FCount: NativeInt;
      FCollection: TSequence<T>;

    public
      constructor Create(const ACollection: TSequence<T>; const ACount: NativeInt); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<T>; override;
    end;

    TTakeSequence<T> = class sealed(TSequence<T>)
    private type
      TEnumerator = class(TForwardingEnumerator<T>)
      private
        FCurrentIndex: NativeInt;
      public
        function AcceptValue(const AValue: T): Boolean; override;
      end;

    private
      FCount: NativeInt;
      FCollection: TSequence<T>;

    public
      constructor Create(const ACollection: TSequence<T>; const ACount: NativeInt); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<T>; override;
    end;

    TFillSequence<T> = class sealed(TSequence<T>)
    private type
      TEnumerator = class(TAbstractEnumerator<T>)
      private
        FRemaining: NativeInt;
      public
        function TryMoveNext(out ACurrent: T): Boolean; override;
      end;

    private
      FElement: T;
      FCount: NativeInt;

    protected
      function GetCount(): NativeInt; override;
    public
      constructor Create(const AElement: T; const ACount: NativeInt; const ARules: TRules<T>);
      function GetEnumerator(): IEnumerator<T>; override;
      function Empty(): Boolean; override;
      function Max(): T; override;
      function Min(): T; override;
      function First(): T; override;
      function FirstOrDefault(const ADefault: T): T; override;
      function Last(): T; override;
      function LastOrDefault(const ADefault: T): T; override;
      function Single(): T; override;
      function SingleOrDefault(const ADefault: T): T; override;
      function Aggregate(const AAggregator: TFunc<T, T, T>): T; override;
      function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T; override;
      function ElementAt(const AIndex: NativeInt): T; override;
      function ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T; override;
      function Any(const APredicate: TPredicate<T>): Boolean; override;
      function All(const APredicate: TPredicate<T>): Boolean; override;
      function EqualsTo(const ACollection: IEnumerable<T>): Boolean; override;
    end;

    TTakeWhileSequence<T> = class sealed(TSequence<T>)
    private type
      TEnumerator = class(TAbstractEnumerator<T>)
      private
        FInEnumerator: IEnumerator<T>;
      public
        function TryMoveNext(out ACurrent: T): Boolean; override;
      end;

    private
      FCollection: TSequence<T>;
      FPredicate: TPredicate<T>;

    public
      constructor Create(const ACollection: TSequence<T>; const APredicate: TPredicate<T>); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<T>; override;
    end;

    TSkipWhileSequence<T> = class sealed(TSequence<T>)
    private type
      TEnumerator = class(TForwardingEnumerator<T>)
      private
        FStarted: Boolean;
      public
        function AcceptValue(const AValue: T): Boolean; override;
      end;

    private
      FCollection: TSequence<T>;
      FPredicate: TPredicate<T>;

    public
      constructor Create(const ACollection: TSequence<T>; const APredicate: TPredicate<T>); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<T>; override;
    end;

    TGroupBySequence<T, TBy> = class sealed(TSequence<IGrouping<TBy, T>>)
    private type
      TEnexGroupingCollection = class(TSequence<T>, IGrouping<TBy, T>)
      private
        FBy: TBy;
        FList: IList<T>;
      public
        function GetKey(): TBy;
        function GetCount(): NativeInt; override;
        function GetEnumerator(): IEnumerator<T>; override;
        procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload; override;
        function Empty(): Boolean; override;
        function Max(): T; override;
        function Min(): T; override;
        function First(): T; override;
        function FirstOrDefault(const ADefault: T): T; override;
        function Last(): T; override;
        function LastOrDefault(const ADefault: T): T; override;
        function Single(): T; override;
        function SingleOrDefault(const ADefault: T): T; override;
        function Aggregate(const AAggregator: TFunc<T, T, T>): T; override;
        function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T; override;
        function ElementAt(const AIndex: NativeInt): T; override;
        function ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T; override;
        function Any(const APredicate: TPredicate<T>): Boolean; override;
        function All(const APredicate: TPredicate<T>): Boolean; override;
        function EqualsTo(const ACollection: IEnumerable<T>): Boolean; override;
      end;

    private var
      FCollection: TSequence<T>;
      FSelector: TFunc<T, TBy>;

    public
      constructor Create(const ACollection: TSequence<T>; const ASelector: TFunc<T, TBy>);
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<IGrouping<TBy, T>>; override;
    end;

    TSelectKeysSequence<TKey, TValue> = class sealed(TSequence<TKey>)
    private type
      TEnumerator = class(TAbstractEnumerator<TKey>)
      private
        FInEnumerator: IEnumerator<TPair<TKey, TValue>>;
      public
        function TryMoveNext(out ACurrent: TKey): Boolean; override;
      end;

    private
      FCollection: TAssociation<TKey, TValue>;

    protected
      function GetCount(): NativeInt; override;

    public
      constructor Create(const ACollection: TAssociation<TKey, TValue>); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<TKey>; override;
    end;

    TSelectValuesSequence<TKey, TValue> = class sealed(TSequence<TValue>)
    private type
      TEnumerator = class(TAbstractEnumerator<TValue>)
      private
        FInEnumerator: IEnumerator<TPair<TKey, TValue>>;
      public
        function TryMoveNext(out ACurrent: TValue): Boolean; override;
      end;

    private
      FCollection: TAssociation<TKey, TValue>;

    protected
      function GetCount(): NativeInt; override;

    public
      constructor Create(const ACollection: TAssociation<TKey, TValue>); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<TValue>; override;
    end;

    TAssociativeWhereSequence<TKey, TValue> = class sealed(TAssociation<TKey, TValue>)
    private type
      TEnumerator = class(TForwardingEnumerator<TPair<TKey, TValue>>)
      public
        function AcceptValue(const AValue: TPair<TKey, TValue>): Boolean; override;
      end;

    var
      FCollection: TAssociation<TKey, TValue>;
      FPredicate: TPredicate<TKey, TValue>;
      FInvertResult: Boolean;
    public
      constructor Create(const ACollection: TAssociation<TKey, TValue>;
          const APredicate: TPredicate<TKey, TValue>; const AInvertResult: Boolean); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<TPair<TKey, TValue>>; override;
    end;

    TAssociativeDistinctByKeysSequence<TKey, TValue> = class sealed(TAssociation<TKey, TValue>)
    private type
      TEnumerator = class(TForwardingEnumerator<TPair<TKey, TValue>>)
      private
        FSet: ISet<TKey>;
      public
        function AcceptValue(const AValue: TPair<TKey, TValue>): Boolean; override;
      end;

    private
      FCollection: TAssociation<TKey, TValue>;

    public
      constructor Create(const ACollection: TAssociation<TKey, TValue>); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<TPair<TKey, TValue>>; override;
    end;

    TAssociativeDistinctByValuesSequence<TKey, TValue> = class sealed(TAssociation<TKey, TValue>)
    private type
      TEnumerator = class(TForwardingEnumerator<TPair<TKey, TValue>>)
      private
        FSet: ISet<TValue>;
      public
        function AcceptValue(const AValue: TPair<TKey, TValue>): Boolean; override;
      end;

    private
      FCollection: TAssociation<TKey, TValue>;

    public
      constructor Create(const ACollection: TAssociation<TKey, TValue>); overload;
      destructor Destroy(); override;
      function GetEnumerator(): IEnumerator<TPair<TKey, TValue>>; override;
    end;
//  end;

{$ENDREGION}

implementation
uses
  Collections.Dictionaries,
  Collections.Sets,
  Collections.Lists, System.SyncObjs;

function TypeKindToStr(const AKind: TTypeKind): String;
begin
  Result := GetEnumName(TypeInfo(TTypeKind), Ord(AKind));
end;

{ TAbstractEnumerator<T> }

constructor TAbstractEnumerator<T>.Create(const AOwner: TAbstractContainer);
begin
  FOwner := AOwner;
  KeepObjectAlive(FOwner);
  FCreatedAtVersion := FOwner.FVersion;
  FEnded := False;
end;

destructor TAbstractEnumerator<T>.Destroy;
begin
  ReleaseObject(FOwner);
  inherited;
end;

function TAbstractEnumerator<T>.GetCurrent: T;
begin
  //if FCreatedAtVersion <> FOwner.FVersion then
  //   ExceptionHelper.Throw_CollectionChangedError();

  Result := FCurrent;
end;

function TAbstractEnumerator<T>.MoveNext: Boolean;
begin
  //if FCreatedAtVersion <> FOwner.FVersion then
  //   ExceptionHelper.Throw_CollectionChangedError();

  if FEnded then
    Result := False
  else begin
    Result := TryMoveNext(FCurrent);

    if not Result then
      FEnded := True;
  end;
end;

function TAbstractEnumerator<T>.VersionChanged: Boolean;
begin
  Result := (FCreatedAtVersion <> FOwner.FVersion);
end;

{ TForwardingEnumerator<T> }

function TForwardingEnumerator<T>.AcceptValue(const AValue: T): Boolean;
begin
  Result := True;
end;

constructor TForwardingEnumerator<T>.Create(const AOwner: TAbstractContainer; const AEnumerator: IEnumerator<T>);
begin
  inherited Create(AOwner);

  if not Assigned(AEnumerator) then
    ExceptionHelper.Throw_ArgumentNilError('AEnumerator');

  FForwardEnumerator := AEnumerator;
end;

function TForwardingEnumerator<T>.TryMoveNext(out ACurrent: T): Boolean;
begin
  while True do
  begin
    Result := FForwardEnumerator.MoveNext();

    if Result then
    begin
      ACurrent := FForwardEnumerator.Current;

      if AcceptValue(ACurrent) then
        Break;
    end else
      Break;
  end;
end;

{ TEnexExtOps<T> }

function TEnexExtOps<T>.GroupBy<TKey>(const ASelector: TFunc<T, TKey>): ISequence<IGrouping<TKey, T>>;
begin
  { Check arguments }
  if not Assigned(ASelector) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector');

  { Create an intermediate collection that will lazy-create the actual stuff }
  Result := TGroupBySequence<T, TKey>.Create(FInstance, ASelector);
end;

{$IF CompilerVersion > 21}
function TEnexExtOps<T>.Select(const AMemberName: string): ISequence<TAny>;
var
  LSelector: TFunc<T, TAny>;
begin
  { Get selector }
  LSelector := Member.Name<T>(AMemberName);

  if not Assigned(LSelector) then
    ExceptionHelper.Throw_TypeDoesNotExposeMember('AMemberName');

  { Select the member by a name, as out type }
  Result := Select<TAny>(LSelector);
end;

function TEnexExtOps<T>.Select<TOut>(const AMemberName: string): ISequence<TOut>;
var
  LSelector: TFunc<T, TOut>;
begin
  { Get selector }
  LSelector := Member.Name<T, TOut>(AMemberName);

  if not Assigned(LSelector) then
    ExceptionHelper.Throw_TypeDoesNotExposeMember(AMemberName);

  { Select the member by a name, as out type }
  Result := Select<TOut>(LSelector);
end;

function TEnexExtOps<T>.Select(const AMemberNames: array of string): ISequence<TView>;
var
  LSelector: TFunc<T, TView>;
begin
  { Get selector }
  LSelector := Member.Name<T>(AMemberNames);

  if not Assigned(LSelector) then
    ExceptionHelper.Throw_TypeDoesNotExposeMember('...');

  { Select the member by a name, as out type }
  Result := Select<TView>(LSelector);
end;
{$IFEND}

function TEnexExtOps<T>.GroupJoin<TInner, TKey, TResult>(
  const AInner: IEnumerable<TInner>; const AKeySelector: TFunc<T, TKey>;
  const AInnerKeySelector: TFunc<TInner, TKey>;
  const AResultSelector: TFunc<T, ISequence<TInner>, TResult>): ISequence<TResult>;
var
  LInnerGroups: IDictionary<TKey, IList<TInner>>;
  LInnerValue: TInner;
  LValue: T;
  LKey: TKey;
  LInnerList: IList<TInner>;
  LResult: IList<TResult>;
begin
  if not Assigned(AInner) then
    ExceptionHelper.Throw_ArgumentNilError('AInner');

  if not Assigned(AKeySelector) then
    ExceptionHelper.Throw_ArgumentNilError('AKeySelector');

  if not Assigned(AInnerKeySelector) then
    ExceptionHelper.Throw_ArgumentNilError('AInnerKeySelector');

  if not Assigned(AResultSelector) then
    ExceptionHelper.Throw_ArgumentNilError('AResultSelector');

  { Group the inner values by key }
  LInnerGroups := TDictionary<TKey, IList<TInner>>.Create();

  for LInnerValue in AInner do
  begin
    { Generate the key for the inner element }
    LKey := AInnerKeySelector(LInnerValue);

    { Add the element into our small simulated multi-map here }
    if not LInnerGroups.TryGetValue(LKey, LInnerList) then
    begin
      LInnerList := TList<TInner>.Create();
      LInnerGroups.Add(LKey, LInnerList);
    end;

    LInnerList.Add(LInnerValue);
  end;

  { M'kay, now that we have established the inner groups, let's start joining the fucker. }
  LResult := TList<TResult>.Create();

  for LValue in TSequence<T>(FInstance) do
  begin
    { Generate the key of the outer element }
    LKey := AKeySelector(LValue);

    { Now, if there is something in the inner group for this key, use it. }
    if LInnerGroups.TryGetValue(LKey, LInnerList) then
      LResult.Add(AResultSelector(LValue, LInnerList));
  end;


  Result := LResult;
end;

function TEnexExtOps<T>.Join<TInner, TKey, TResult>(
  const AInner: IEnumerable<TInner>; const AKeySelector: TFunc<T, TKey>;
  const AInnerKeySelector: TFunc<TInner, TKey>;
  const AResultSelector: TFunc<T, TInner, TResult>): ISequence<TResult>;
var
  LInnerGroups: TObjectDictionary<TKey, TList<TInner>>;
  LInnerValue: TInner;
  LValue: T;
  LKey: TKey;
  LInnerList: TList<TInner>;
  LResult: IList<TResult>;
begin
  if not Assigned(AInner) then
    ExceptionHelper.Throw_ArgumentNilError('AInner');

  if not Assigned(AKeySelector) then
    ExceptionHelper.Throw_ArgumentNilError('AKeySelector');

  if not Assigned(AInnerKeySelector) then
    ExceptionHelper.Throw_ArgumentNilError('AInnerKeySelector');

  if not Assigned(AResultSelector) then
    ExceptionHelper.Throw_ArgumentNilError('AResultSelector');

  { Group the inner values by key }
  LInnerGroups := TObjectDictionary<TKey, TList<TInner>>.Create();
  LInnerGroups.OwnsValues := True;

  try
    for LInnerValue in AInner do
    begin
      { Generate the key for the inner element }
      LKey := AInnerKeySelector(LInnerValue);

      { Add the element into our small simulated multi-map here }
      if not LInnerGroups.TryGetValue(LKey, LInnerList) then
      begin
        LInnerList := TList<TInner>.Create();
        LInnerGroups.Add(LKey, LInnerList);
      end;

      LInnerList.Add(LInnerValue);
    end;

    { M'kay, now that we have established the inner groups, let's start joining the fucker. }
    LResult := TList<TResult>.Create();

    for LValue in TSequence<T>(FInstance) do
    begin
      { Generate the key of the outer element }
      LKey := AKeySelector(LValue);

      { Now, if there is something in the inner group for this key, use it. }
      if LInnerGroups.TryGetValue(LKey, LInnerList) then
      begin
        for LInnerValue in LInnerList do
          LResult.Add(AResultSelector(LValue, LInnerValue));
      end;
    end;

  finally
    LInnerGroups.Free;
  end;

  Result := LResult;
end;

function TEnexExtOps<T>.OrderBy<TKey1, TKey2, TKey3, TKey4, TKey5>(
  const ASelector1: TFunc<T, TKey1>; const ASelector2: TFunc<T, TKey2>;
  const ASelector3: TFunc<T, TKey3>; const ASelector4: TFunc<T, TKey4>;
  const ASelector5: TFunc<T, TKey5>): ISequence<T>;
var
  LList: TList<T>;
  LComparer1: IComparer<TKey1>;
  LComparer2: IComparer<TKey2>;
  LComparer3: IComparer<TKey3>;
  LComparer4: IComparer<TKey4>;
  LComparer5: IComparer<TKey5>;
  LSortProc: TComparison<T>;
begin
  if not Assigned(ASelector1) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector1');

  if not Assigned(ASelector2) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector2');

  if not Assigned(ASelector3) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector3');

  if not Assigned(ASelector4) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector4');

  if not Assigned(ASelector5) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector5');

  { Create an itermediary LList }
  LList := TList<T>.Create();
  LList.AddAll(TSequence<T>(FInstance));

  { Create the comparer }
  LComparer1 := TComparer<TKey1>.Default;
  LComparer2 := TComparer<TKey2>.Default;
  LComparer3 := TComparer<TKey3>.Default;
  LComparer4 := TComparer<TKey4>.Default;
  LComparer5 := TComparer<TKey5>.Default;
  LSortProc :=
    function(const Left, Right: T): Integer
    begin
      Result := LComparer1.Compare(ASelector1(Left), ASelector1(Right));
      if Result = 0 then
      begin
        Result := LComparer2.Compare(ASelector2(Left), ASelector2(Right));

        if Result = 0 then
        begin
          Result := LComparer3.Compare(ASelector3(Left), ASelector3(Right));

          if Result = 0 then
          begin
            Result := LComparer4.Compare(ASelector4(Left), ASelector4(Right));

            if Result = 0 then
              Result := LComparer5.Compare(ASelector5(Left), ASelector5(Right));
          end;
        end;
      end;
    end;

  { Sort the stuff }
  LList.Sort(LSortProc);

  { Pass the LList further }
  Result := LList;
end;

function TEnexExtOps<T>.OrderBy<TKey1, TKey2, TKey3, TKey4>(
  const ASelector1: TFunc<T, TKey1>; const ASelector2: TFunc<T, TKey2>;
  const ASelector3: TFunc<T, TKey3>;
  const ASelector4: TFunc<T, TKey4>): ISequence<T>;
var
  LList: TList<T>;
  LComparer1: IComparer<TKey1>;
  LComparer2: IComparer<TKey2>;
  LComparer3: IComparer<TKey3>;
  LComparer4: IComparer<TKey4>;
  LSortProc: TComparison<T>;
begin
  if not Assigned(ASelector1) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector1');

  if not Assigned(ASelector2) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector2');

  if not Assigned(ASelector3) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector3');

  if not Assigned(ASelector4) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector4');

  { Create an itermediary LList }
  LList := TList<T>.Create();
  LList.AddAll(TSequence<T>(FInstance));

  { Create the comparer }
  LComparer1 := TComparer<TKey1>.Default;
  LComparer2 := TComparer<TKey2>.Default;
  LComparer3 := TComparer<TKey3>.Default;
  LComparer4 := TComparer<TKey4>.Default;
  LSortProc :=
    function(const Left, Right: T): Integer
    begin
      Result := LComparer1.Compare(ASelector1(Left), ASelector1(Right));
      if Result = 0 then
      begin
        Result := LComparer2.Compare(ASelector2(Left), ASelector2(Right));

        if Result = 0 then
        begin
          Result := LComparer3.Compare(ASelector3(Left), ASelector3(Right));

          if Result = 0 then
            Result := LComparer4.Compare(ASelector4(Left), ASelector4(Right));
        end;
      end;
    end;

  { Sort the stuff }
  LList.Sort(LSortProc);

  { Pass the LList further }
  Result := LList;
end;

function TEnexExtOps<T>.OrderBy<TKey1, TKey2, TKey3>(
  const ASelector1: TFunc<T, TKey1>; const ASelector2: TFunc<T, TKey2>;
  const ASelector3: TFunc<T, TKey3>): ISequence<T>;
var
  LList: TList<T>;
  LComparer1: IComparer<TKey1>;
  LComparer2: IComparer<TKey2>;
  LComparer3: IComparer<TKey3>;
  LSortProc: TComparison<T>;
begin
  if not Assigned(ASelector1) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector1');

  if not Assigned(ASelector2) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector2');

  if not Assigned(ASelector3) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector3');

  { Create an itermediary LList }
  LList := TList<T>.Create();
  LList.AddAll(TSequence<T>(FInstance));

  { Create the comparer }
  LComparer1 := TComparer<TKey1>.Default;
  LComparer2 := TComparer<TKey2>.Default;
  LComparer3 := TComparer<TKey3>.Default;
  LSortProc :=
    function(const Left, Right: T): Integer
    begin
      Result := LComparer1.Compare(ASelector1(Left), ASelector1(Right));
      if Result = 0 then
      begin
        Result := LComparer2.Compare(ASelector2(Left), ASelector2(Right));

        if Result = 0 then
          Result := LComparer3.Compare(ASelector3(Left), ASelector3(Right));
      end;
    end;

  { Sort the stuff }
  LList.Sort(LSortProc);

  { Pass the LList further }
  Result := LList;
end;

function TEnexExtOps<T>.OrderBy<TKey1, TKey2>(const ASelector1: TFunc<T, TKey1>;
  const ASelector2: TFunc<T, TKey2>): ISequence<T>;
var
  LList: TList<T>;
  LComparer1: IComparer<TKey1>;
  LComparer2: IComparer<TKey2>;
  LSortProc: TComparison<T>;
begin
  if not Assigned(ASelector1) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector1');

  if not Assigned(ASelector2) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector2');

  { Create an itermediary LList }
  LList := TList<T>.Create();
  LList.AddAll(TSequence<T>(FInstance));

  { Create the comparer }
  LComparer1 := TComparer<TKey1>.Default;
  LComparer2 := TComparer<TKey2>.Default;
  LSortProc :=
    function(const Left, Right: T): Integer
    begin
      Result := LComparer1.Compare(ASelector1(Left), ASelector1(Right));
      if Result = 0 then
        Result := LComparer2.Compare(ASelector2(Left), ASelector2(Right));
    end;

  { Sort the stuff }
  LList.Sort(LSortProc);

  { Pass the LList further }
  Result := LList;
end;

function TEnexExtOps<T>.OrderBy<TKey>(const ASelector: TFunc<T, TKey>): ISequence<T>;
var
  LList: TList<T>;
  LComparer: IComparer<TKey>;
  LSortProc: TComparison<T>;
begin
  if not Assigned(ASelector) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector');

  { Create an itermediary LList }
  LList := TList<T>.Create();
  LList.AddAll(TSequence<T>(FInstance));

  { Create the comparer }
  LComparer := TComparer<TKey>.Default;
  LSortProc :=
    function(const Left, Right: T): Integer
    begin
      Result := LComparer.Compare(ASelector(Left), ASelector(Right));
    end;

  { Sort the stuff }
  LList.Sort(LSortProc);

  { Pass the LList further }
  Result := LList;
end;

function TEnexExtOps<T>.Select<TOut>: ISequence<TOut>;
var
  LTypeInfo: PTypeInfo;
begin
  { Make sure that T is a class }
  LTypeInfo := TypeInfo(T);

  { TADA! }
  if (not Assigned(LTypeInfo)) or (LTypeInfo^.Kind <> tkClass) then
    ExceptionHelper.Throw_TypeNotAClassError(GetTypeName(LTypeInfo));

  Result := TSelectClassSequence<TObject, TOut>.Create(FInstance, TRules<TOut>.Default);
end;

function TEnexExtOps<T>.Select<TOut>(const ASelector: TFunc<T, TOut>): ISequence<TOut>;
begin
  { With default type support }
  Result := Select<TOut>(ASelector, TRules<TOut>.Default);
end;

function TEnexExtOps<T>.Select<TOut>(const ASelector: TFunc<T, TOut>; const ARules: TRules<TOut>): ISequence<TOut>;
begin
  { Check arguments }
  if not Assigned(ASelector) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector');

  { Create a new Enex collection }
  Result := TSelectSequence<T, TOut>.Create(FInstance, ASelector, ARules);
end;

{ TAbstractContainer }

procedure TAbstractContainer.NotifyCollectionChanged;
begin
  Inc(FVersion);
end;

function TAbstractContainer.Version: NativeInt;
begin
  Result := FVersion;
end;

{ TAbstractContainer<T> }

procedure TAbstractContainer<T>.CopyTo(var AArray: array of T);
begin
  { Call upper version }
  CopyTo(AArray, 0);
end;

procedure TAbstractContainer<T>.CopyTo(var AArray: array of T; const AStartIndex: NativeInt);
var
  LEnumerator: IEnumerator<T>;
  L, I: NativeInt;
begin
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();
  L := Length(AArray);
  I := AStartIndex;

  { Iterate until ANY element supports the predicate }
  while LEnumerator.MoveNext() do
  begin
    if I >= L then
      ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray/AStartIndex');

    AArray[I] := LEnumerator.Current;
    Inc(I);
  end;
end;

function TAbstractContainer<T>.Empty: Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Check if empty }
  Result := (not LEnumerator.MoveNext());
end;

function TAbstractContainer<T>.GetCount: NativeInt;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Iterate till the end }
  Result := 0;
  while LEnumerator.MoveNext() do Inc(Result);
end;

function TAbstractContainer<T>.Single: T;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if LEnumerator.MoveNext() then
    Result := LEnumerator.Current
  else
    ExceptionHelper.Throw_CollectionEmptyError();

  { Fail if more than one elements are there }
  if LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement();
end;

function TAbstractContainer<T>.SingleOrDefault(const ADefault: T): T;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if LEnumerator.MoveNext() then
    Result := LEnumerator.Current
  else
    Exit(ADefault);

  { Fail if more than one elements are there }
  if LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement();
end;

function TAbstractContainer<T>.ToArray: TArray<T>;
var
  LCount: NativeInt;
  LResult: TArray<T>;
begin
  LCount := Count;

  if LCount > 0 then
  begin
    { Set the length of array }
    SetLength(LResult, LCount);

    { Copy all elements to array }
    CopyTo(LResult);
  end else
    SetLength(LResult, 0);

  Result := LResult;
end;

{ TSequence<T> }

function TSequence<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
var
  LEnumerator: IEnumerator<T>;
begin
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := LEnumerator.Current;

  { Iterate over the last N - 1 elements }
  while LEnumerator.MoveNext() do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, LEnumerator.Current);
  end;
end;

function TSequence<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
var
  LEnumerator: IEnumerator<T>;
begin
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    Exit(ADefault);

  { Select the first element as comparison base }
  Result := LEnumerator.Current;

  { Iterate over the last N - 1 elements }
  while LEnumerator.MoveNext() do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, LEnumerator.Current);
  end;
end;

function TSequence<T>.All(const APredicate: TPredicate<T>): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Iterate while ALL elements support the predicate }
  while LEnumerator.MoveNext() do
  begin
    if not APredicate(LEnumerator.Current) then
      Exit(false);
  end;

  Result := true;
end;

function TSequence<T>.Any(const APredicate: TPredicate<T>): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Iterate until ANY element supports the predicate }
  while LEnumerator.MoveNext() do
  begin
    if APredicate(LEnumerator.Current) then
      Exit(true);
  end;

  Result := false;
end;

function TSequence<T>.CompareElements(const ALeft, ARight: T): NativeInt;
begin
  { Lazy init }
  if not Assigned(FElementRules.FComparer) then
    FElementRules.FComparer := TComparer<T>.Default;

  Result := FElementRules.FComparer.Compare(ALeft, ARight);
end;

function TSequence<T>.CompareTo(AObject: TObject): Integer;
var
  LIterSelf, LIterTo: IEnumerator<T>;
  LMovSelf, LMovTo: Boolean;
begin
  { Check if we can continue }
  if (not Assigned(AObject)) or (not AObject.InheritsFrom(TSequence<T>)) then
    Result := 1
  else begin
    { Assume equality }
    Result := 0;

    { Get enumerators }
    LIterSelf := GetEnumerator();
    LIterTo := TSequence<T>(AObject).GetEnumerator();

    while true do
    begin
      { Iterate and verify that both enumerators moved }
      LMovSelf := LIterSelf.MoveNext();
      LMovTo := LIterTo.MoveNext();

      { If one moved but the other did not - error }
      if LMovSelf <> LMovTo then
      begin
        { Decide on the return value }
        if LMovSelf then
          Result := 1
        else
          Result := -1;

        Break;
      end;

      { If neither moved, we've reached the end }
      if not LMovSelf then
        Break;

      { Verify both values are identical }
      Result := CompareElements(LIterSelf.Current, LIterTo.Current);
      if Result <> 0 then
        Break;
    end;
  end;
end;

function TSequence<T>.Concat(const ACollection: ISequence<T>): ISequence<T>;
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Create concatenation iterator }
  Result := TConcatSequence<T>.Create(Self, ACollection);
end;

constructor TSequence<T>.Create(const ARules: TRules<T>);
begin
  FElementRules := ARules;
end;

constructor TSequence<T>.Create;
begin
  Create(TRules<T>.Default);
end;

function TSequence<T>.Distinct: ISequence<T>;
begin
  { Create a new enumerator }
  Result := TDistinctSequence<T>.Create(Self);
end;

function TSequence<T>.ElementAt(const AIndex: NativeInt): T;
var
  LEnumerator: IEnumerator<T>;
  LCount: NativeInt;
begin
  if AIndex < 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');

  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();
  LCount := 0;

  while LEnumerator.MoveNext() do
  begin
    { If we reached thge element, exit }
    if LCount = AIndex then
      Exit(LEnumerator.Current);

    Inc(LCount);
  end;

  { Fail! }
  ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');
end;

function TSequence<T>.ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T;
var
  LEnumerator: IEnumerator<T>;
  LCount: NativeInt;
begin
  if AIndex < 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');

  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();
  LCount := 0;

  while LEnumerator.MoveNext() do
  begin
    { If we reached thge element, exit }
    if LCount = AIndex then
      Exit(LEnumerator.Current);

    Inc(LCount);
  end;

  { Return default value }
  Result := ADefault;
end;

function TSequence<T>.ElementsAreEqual(const ALeft, ARight: T): Boolean;
begin
  { Lazy init }
  if not Assigned(FElementRules.FEqComparer) then
    FElementRules.FEqComparer := TEqualityComparer<T>.Default;

  Result := FElementRules.FEqComparer.Equals(ALeft, ARight);
end;

function TSequence<T>.Equals(Obj: TObject): Boolean;
begin
  { Call comparison }
  Result := (CompareTo(Obj) = 0);
end;

function TSequence<T>.EqualsTo(const ACollection: IEnumerable<T>): Boolean;
var
  LEnumerator1, LEnumerator2: IEnumerator<T>;
  LMoved1, LMoved2: Boolean;
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Get enumerators }
  LEnumerator1 := GetEnumerator();
  LEnumerator2 := ACollection.GetEnumerator();

  while true do
  begin
    { Iterate and verify that both enumerators moved }
    LMoved1 := LEnumerator1.MoveNext();
    LMoved2 := LEnumerator2.MoveNext();

    { If one moved but the other did not - error }
    if LMoved1 <> LMoved2 then
      Exit(false);

    { If neither moved, we've reached the end }
    if not LMoved1 then
      break;

    { Verify both values are identical }
    if not ElementsAreEqual(LEnumerator1.Current, LEnumerator2.Current) then
      Exit(false);
  end;

  { It worked! }
  Result := true;
end;

function TSequence<T>.Exclude(const ACollection: ISequence<T>): ISequence<T>;
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Create concatenation iterator }
  Result := TExclusionSequence<T>.Create(Self, ACollection);
end;

class function TSequence<T>.Fill(const AElement: T; const ACount: NativeInt; const ARules: TRules<T>): ISequence<T>;
begin
  { Check arguments }
  if ACount <= 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  { Create an collection }
  Result := TFillSequence<T>.Create(AElement, ACount, ARules);
end;

class function TSequence<T>.Fill(const AElement: T; const ACount: NativeInt): ISequence<T>;
begin
  { Call upper function }
  Result := Fill(AElement, ACount, TRules<T>.Default);
end;

function TSequence<T>.First: T;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if LEnumerator.MoveNext() then
    Result := LEnumerator.Current
  else
    ExceptionHelper.Throw_CollectionEmptyError();
end;

function TSequence<T>.FirstOrDefault(const ADefault: T): T;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise return default! }
  if LEnumerator.MoveNext() then
    Result := LEnumerator.Current
  else
    Result := ADefault;
end;

function TSequence<T>.FirstWhere(const APredicate: TPredicate<T>): T;
var
  LEnumerator: IEnumerator<T>;
  LWasOne: Boolean;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();
  LWasOne := false;

  { Do the funky stuff already }
  while LEnumerator.MoveNext do
  begin
    LWasOne := true;

    if APredicate(LEnumerator.Current) then
      Exit(LEnumerator.Current);
  end;

  { Failure to find what we need }
  if LWasOne then
    ExceptionHelper.Throw_CollectionHasNoFilteredElements()
  else
    ExceptionHelper.Throw_CollectionEmptyError();
end;

function TSequence<T>.FirstWhereBetween(const ALower, AHigher: T): T;
begin
  Result := FirstWhere(
    function(Arg1: T): Boolean
    begin
      Result := (CompareElements(Arg1, ALower) >= 0) and
                (CompareElements(Arg1, AHigher) <= 0)
    end
  );
end;

function TSequence<T>.FirstWhereBetweenOrDefault(const ALower, AHigher, ADefault: T): T;
begin
  Result := FirstWhereOrDefault(
    function(Arg1: T): Boolean
    begin
      Result := (CompareElements(Arg1, ALower) >= 0) and
                (CompareElements(Arg1, AHigher) <= 0)
    end,
    ADefault
  );
end;

function TSequence<T>.FirstWhereGreater(const ABound: T): T;
begin
  Result := FirstWhere(
    function(Arg1: T): Boolean
    begin
      Result := CompareElements(Arg1, ABound) > 0;
    end
  );
end;

function TSequence<T>.FirstWhereGreaterOrDefault(const ABound, ADefault: T): T;
begin
  Result := FirstWhereOrDefault(
    function(Arg1: T): Boolean
    begin
      Result := CompareElements(Arg1, ABound) > 0;
    end,
    ADefault
  );
end;

function TSequence<T>.FirstWhereGreaterOrEqual(const ABound: T): T;
begin
  Result := FirstWhere(
    function(Arg1: T): Boolean
    begin
      Result := CompareElements(Arg1, ABound) >= 0;
    end
  );
end;

function TSequence<T>.FirstWhereGreaterOrEqualOrDefault(const ABound, ADefault: T): T;
begin
  Result := FirstWhereOrDefault(
    function(Arg1: T): Boolean
    begin
      Result := CompareElements(Arg1, ABound) >= 0;
    end,
    ADefault
  );
end;

function TSequence<T>.FirstWhereLower(const ABound: T): T;
begin
  Result := FirstWhere(
    function(Arg1: T): Boolean
    begin
      Result := CompareElements(Arg1, ABound) < 0;
    end
  );
end;

function TSequence<T>.FirstWhereLowerOrDefault(const ABound, ADefault: T): T;
begin
  Result := FirstWhereOrDefault(
    function(Arg1: T): Boolean
    begin
      Result := CompareElements(Arg1, ABound) < 0;
    end,
    ADefault
  );
end;

function TSequence<T>.FirstWhereLowerOrEqual(const ABound: T): T;
begin
  Result := FirstWhere(
    function(Arg1: T): Boolean
    begin
      Result := CompareElements(Arg1, ABound) <= 0;
    end
  );
end;

function TSequence<T>.FirstWhereLowerOrEqualOrDefault(const ABound, ADefault: T): T;
begin
  Result := FirstWhereOrDefault(
    function(Arg1: T): Boolean
    begin
      Result := CompareElements(Arg1, ABound) <= 0;
    end,
    ADefault
  );
end;

function TSequence<T>.FirstWhereNot(const APredicate: TPredicate<T>): T;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  Result := FirstWhere(
    function(Arg1: T): Boolean
    begin
      Result := not APredicate(Arg1);
    end
  );
end;

function TSequence<T>.FirstWhereNotOrDefault(
  const APredicate: TPredicate<T>; const ADefault: T): T;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  Result := FirstWhereOrDefault(
    function(Arg1: T): Boolean
    begin
      Result := not APredicate(Arg1);
    end,
    ADefault
  );
end;

function TSequence<T>.FirstWhereOrDefault(const APredicate: TPredicate<T>; const ADefault: T): T;
var
  LEnumerator: IEnumerator<T>;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Do the funky stuff already }
  while LEnumerator.MoveNext do
    if APredicate(LEnumerator.Current) then
      Exit(LEnumerator.Current);

  { Failure to find what we need }
  Result := ADefault;
end;

function TSequence<T>.GetElementHashCode(const AValue: T): NativeInt;
begin
  { Lazy init }
  if not Assigned(FElementRules.FEqComparer) then
    FElementRules.FEqComparer := TEqualityComparer<T>.Default;

  Result := FElementRules.FEqComparer.GetHashCode(AValue);
end;

function TSequence<T>.GetHashCode: Integer;
const
  CMagic = $0F;

var
  LEnumerator: IEnumerator<T>;
begin
  { Obtain the enumerator }
  LEnumerator := GetEnumerator();

  { Start at 0 }
  Result := 0;

  { ... }
  while LEnumerator.MoveNext() do
    Result := CMagic * Result + GetElementHashCode(LEnumerator.Current);
end;

function TSequence<T>.Intersect(const ACollection: ISequence<T>): ISequence<T>;
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Create concatenation iterator }
  Result := TIntersectionSequence<T>.Create(Self, ACollection);
end;

function TSequence<T>.Last: T;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Iterate till the last element in the LEnumerator }
  while true do
  begin
    Result := LEnumerator.Current;

    { Exit if we hit the last element }
    if not LEnumerator.MoveNext() then
      Exit;
  end;
end;

function TSequence<T>.LastOrDefault(const ADefault: T): T;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise return default! }
  if not LEnumerator.MoveNext() then
    Exit(ADefault);

  { Iterate till the last element in the LEnumerator }
  while true do
  begin
    Result := LEnumerator.Current;

    { Exit if we hit the last element }
    if not LEnumerator.MoveNext() then
      Exit;
  end;
end;

function TSequence<T>.Max: T;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := LEnumerator.Current;

  { Iterate till the last element in the LEnumerator }
  while true do
  begin
    if CompareElements(LEnumerator.Current, Result) > 0 then
      Result := LEnumerator.Current;

    { Exit if we hit the last element }
    if not LEnumerator.MoveNext() then
      Exit;
  end;
end;

function TSequence<T>.Min: T;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := LEnumerator.Current;

  { Iterate till the last element in the LEnumerator }
  while true do
  begin
    if CompareElements(LEnumerator.Current, Result) < 0 then
      Result := LEnumerator.Current;

    { Exit if we hit the last element }
    if not LEnumerator.MoveNext() then
      Exit;
  end;
end;

function TSequence<T>.Op: TEnexExtOps<T>;
begin
  { Build up the record + keep an optional reference to the object }
  Result.FInstance := Self;
  Result.FKeepAlive := Self.ExtractReference;
  Result.FRules := FElementRules;
end;

function TSequence<T>.Range(const AStart, AEnd: NativeInt): ISequence<T>;
begin
  if AStart < 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStart');
  if AEnd < AStart then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AEnd');

  { Create a new Enex collection }
  Result := TRangeSequence<T>.Create(Self, AStart, AEnd);
end;

function TSequence<T>.Reversed: ISequence<T>;
var
  LList: TList<T>;
begin
  { Create an itermediary LList }
  LList := TList<T>.Create(ElementRules);
  LList.AddAll(Self);
  LList.Reverse();

  { Pass the LList further }
  Result := LList;
end;

function TSequence<T>.Skip(const ACount: NativeInt): ISequence<T>;
begin
  { Check parameters }
  if ACount = 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  { Create a new Enex collection }
  Result := TSkipSequence<T>.Create(Self, ACount);
end;

function TSequence<T>.SkipWhile(const APredicate: TPredicate<T>): ISequence<T>;
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Create a new Enex collection }
  Result := TSkipWhileSequence<T>.Create(Self, APredicate);
end;

function TSequence<T>.SkipWhileBetween(const ALower, AHigher: T): ISequence<T>;
var
  LLower, LHigher: T;
begin
  { Locals }
  LLower := ALower;
  LHigher := AHigher;

  { Use SkipWhile() and pass an anonymous function }
  Result := SkipWhile(
    function(Arg1: T): Boolean
    begin
      Exit((CompareElements(Arg1, LLower) >= 0) and (CompareElements(Arg1, LHigher) <= 0));
    end
  );
end;

function TSequence<T>.SkipWhileGreater(const ABound: T): ISequence<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use SkipWhile() and pass an anonymous function }
  Result := SkipWhile(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) > 0);
    end
  );
end;

function TSequence<T>.SkipWhileGreaterOrEqual(const ABound: T): ISequence<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use SkipWhile() and pass an anonymous function }
  Result := SkipWhile(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) >= 0);
    end
  );
end;

function TSequence<T>.SkipWhileLower(const ABound: T): ISequence<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use SkipWhile() and pass an anonymous function }
  Result := SkipWhile(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) < 0);
    end
  );
end;

function TSequence<T>.SkipWhileLowerOrEqual(const ABound: T): ISequence<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use SkipWhile() and pass an anonymous function }
  Result := SkipWhile(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) <= 0);
    end
  );
end;

function TSequence<T>.Ordered(const ASortProc: TComparison<T>): ISequence<T>;
var
  LList: TList<T>;
begin
  { Create an itermediary LList }
  LList := TList<T>.Create(ElementRules);
  LList.AddAll(Self);
  LList.Sort(ASortProc);

  { Pass the LList further }
  Result := LList;
end;

function TSequence<T>.Ordered(const AAscending: Boolean = true): ISequence<T>;
var
  LList: TList<T>;
begin
  { Create an itermediary LList }
  LList := TList<T>.Create(ElementRules);
  LList.AddAll(Self);
  LList.Sort(AAscending);

  { Pass the LList further }
  Result := LList;
end;

function TSequence<T>.Take(const ACount: NativeInt): ISequence<T>;
begin
  { Check parameters }
  if ACount = 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  { Create a new Enex collection }
  Result := TTakeSequence<T>.Create(Self, ACount);
end;

function TSequence<T>.TakeWhile(const APredicate: TPredicate<T>): ISequence<T>;
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Create a new Enex collection }
  Result := TTakeWhileSequence<T>.Create(Self, APredicate);
end;

function TSequence<T>.TakeWhileBetween(const ALower, AHigher: T): ISequence<T>;
var
  LLower, LHigher: T;
begin
  { Locals }
  LLower := ALower;
  LHigher := AHigher;

  { Use TakeWhile() and pass an anonymous function }
  Result := TakeWhile(
    function(Arg1: T): Boolean
    begin
      Exit((CompareElements(Arg1, LLower) >= 0) and (CompareElements(Arg1, LHigher) <= 0));
    end
  );
end;

function TSequence<T>.TakeWhileGreater(const ABound: T): ISequence<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use TakeWhile() and pass an anonymous function }
  Result := TakeWhile(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) > 0);
    end
  );
end;

function TSequence<T>.TakeWhileGreaterOrEqual(const ABound: T): ISequence<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use TakeWhile() and pass an anonymous function }
  Result := TakeWhile(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) >= 0);
    end
  );
end;

function TSequence<T>.TakeWhileLower(const ABound: T): ISequence<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use TakeWhile() and pass an anonymous function }
  Result := TakeWhile(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) < 0);
    end
  );
end;

function TSequence<T>.TakeWhileLowerOrEqual(const ABound: T): ISequence<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use TakeWhile() and pass an anonymous function }
  Result := TakeWhile(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) <= 0);
    end
  );
end;

function TSequence<T>.ToList: IList<T>;
begin
  { Simply make up a list }
  Result := TList<T>.Create(ElementRules);
  Result.AddAll(Self);
end;

function TSequence<T>.ToSet: ISet<T>;
begin
  { Simply make up a bag }
  Result := THashSet<T>.Create(ElementRules);
  Result.AddAll(Self);
end;

function TSequence<T>.Union(const ACollection: ISequence<T>): ISequence<T>;
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Create concatenation iterator }
  Result := TUnionSequence<T>.Create(Self, ACollection);
end;

function TSequence<T>.Where(const APredicate: TPredicate<T>): ISequence<T>;
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Create a new Enex collection }
  Result := TWhereSequence<T>.Create(Self, APredicate, False); // Don't invert the result
end;

function TSequence<T>.WhereBetween(const ALower, AHigher: T): ISequence<T>;
var
  LLower, LHigher: T;
begin
  { Locals }
  LLower := ALower;
  LHigher := AHigher;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: T): Boolean
    begin
      Exit((CompareElements(Arg1, LLower) >= 0) and (CompareElements(Arg1, LHigher) <= 0));
    end
  );
end;

function TSequence<T>.WhereGreater(const ABound: T): ISequence<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) > 0);
    end
  );
end;

function TSequence<T>.WhereGreaterOrEqual(const ABound: T): ISequence<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) >= 0);
    end
  );
end;

function TSequence<T>.WhereLower(const ABound: T): ISequence<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) < 0);
    end
  );
end;

function TSequence<T>.WhereLowerOrEqual(const ABound: T): ISequence<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) <= 0);
    end
  );
end;

function TSequence<T>.WhereNot(
  const APredicate: TPredicate<T>): ISequence<T>;
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Create a new Enex collection }
  Result := TWhereSequence<T>.Create(Self, APredicate, True); // Invert the result
end;

{ TCollection<T> }

procedure TCollection<T>.Add(const AValue: T);
begin
  ExceptionHelper.Throw_OperationNotSupported('Add');
end;

procedure TCollection<T>.AddAll(const ACollection: IEnumerable<T>);
var
  LValue: T;
begin
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  for LValue in ACollection do
    Add(LValue);
end;

procedure TCollection<T>.Clear;
var
  LValue: T;
begin
  while not Empty() do
  begin
    LValue := First();
    Remove(LValue);

    NotifyElementRemoved(LValue);
  end;
end;

function TCollection<T>.Contains(const AValue: T): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := GetEnumerator();
  while LEnumerator.MoveNext() do
    if ElementsAreEqual(LEnumerator.Current, AValue) then
      Exit(True);

  Result := False;
end;

function TCollection<T>.ContainsAll(const ACollection: IEnumerable<T>): Boolean;
var
  LValue: T;
begin
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  Result := True;
  for LValue in ACollection do
    Result := Result and Contains(LValue);
end;

destructor TCollection<T>.Destroy;
begin
  Clear();
  inherited;
end;

procedure TCollection<T>.HandleElementRemoved(const AElement: T);
begin
  // Nothing
end;

procedure TCollection<T>.NotifyElementRemoved(const AElement: T);
begin
  { Handle removal }
  if Assigned(FRemoveNotification) then
    FRemoveNotification(AElement)
  else
    HandleElementRemoved(AElement);
end;

procedure TCollection<T>.Remove(const AValue: T);
begin
  ExceptionHelper.Throw_OperationNotSupported('Remove');
end;

procedure TCollection<T>.RemoveAll(const ACollection: IEnumerable<T>);
var
  LValue: T;
begin
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  for LValue in ACollection do
    Remove(LValue);
end;


{ TAssociation<TKey, TValue> }

constructor TAssociation<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default);
end;

function TAssociation<TKey, TValue>.CompareKeys(const ALeft, ARight: TKey): NativeInt;
begin
  { Lazy init }
  if not Assigned(FKeyRules.FComparer) then
    FKeyRules.FComparer := TComparer<TKey>.Default;

  Result := FKeyRules.FComparer.Compare(ALeft, ARight);
end;

function TAssociation<TKey, TValue>.CompareValues(const ALeft, ARight: TValue): NativeInt;
begin
  { Lazy init }
  if not Assigned(FValueRules.FComparer) then
    FValueRules.FComparer := TComparer<TValue>.Default;

  Result := FValueRules.FComparer.Compare(ALeft, ARight);
end;

constructor TAssociation<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>);
begin
  FKeyRules := AKeyRules;
  FValueRules := AValueRules;
end;

function TAssociation<TKey, TValue>.DistinctByKeys: IAssociation<TKey, TValue>;
begin
  Result := TAssociativeDistinctByKeysSequence<TKey, TValue>.Create(Self);
end;

function TAssociation<TKey, TValue>.DistinctByValues: IAssociation<TKey, TValue>;
begin
  Result := TAssociativeDistinctByValuesSequence<TKey, TValue>.Create(Self);
end;

function TAssociation<TKey, TValue>.GetKeyHashCode(const AValue: TKey): NativeInt;
begin
  { Lazy init }
  if not Assigned(FKeyRules.FEqComparer) then
    FKeyRules.FEqComparer := TEqualityComparer<TKey>.Default;

  Result := FKeyRules.FEqComparer.GetHashCode(AValue);
end;

function TAssociation<TKey, TValue>.GetValueHashCode(const AValue: TValue): NativeInt;
begin
  { Lazy init }
  if not Assigned(FValueRules.FEqComparer) then
    FValueRules.FEqComparer := TEqualityComparer<TValue>.Default;

  Result := FValueRules.FEqComparer.GetHashCode(AValue);
end;

procedure TAssociation<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  // Nothing!
end;

procedure TAssociation<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  // Nothing!
end;

function TAssociation<TKey, TValue>.Includes(const ACollection: IEnumerable<TPair<TKey, TValue>>): Boolean;
var
  LEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Retrieve the enumerator object }
  LEnumerator := ACollection.GetEnumerator();

  { Iterate till the last element in the LEnumerator }
  while LEnumerator.MoveNext do
  begin
    if not KeyHasValue(LEnumerator.Current.Key, LEnumerator.Current.Value) then
      Exit(false);
  end;

  { We got here, it means all is OK }
  Result := true;
end;

function TAssociation<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
var
  LEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Iterate till the last element in the LEnumerator }
  while LEnumerator.MoveNext do
  begin
    if KeysAreEqual(LEnumerator.Current.Key, AKey) and
       ValuesAreEqual(LEnumerator.Current.Value, AValue) then
      Exit(true);
  end;

  { No found! }
  Result := false;
end;

function TAssociation<TKey, TValue>.KeysAreEqual(const ALeft, ARight: TKey): Boolean;
begin
  { Lazy init }
  if not Assigned(FKeyRules.FEqComparer) then
    FKeyRules.FEqComparer := TEqualityComparer<TKey>.Default;

  Result := FKeyRules.FEqComparer.Equals(ALeft, ARight);
end;

function TAssociation<TKey, TValue>.MaxKey: TKey;
var
  LEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := LEnumerator.Current.Key;

  { Iterate till the last element in the LEnumerator }
  while true do
  begin
    if CompareKeys(LEnumerator.Current.Key, Result) > 0 then
      Result := LEnumerator.Current.Key;

    { Exit if we hit the last element }
    if not LEnumerator.MoveNext() then
      Exit;
  end;
end;

function TAssociation<TKey, TValue>.MaxValue: TValue;
var
  LEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := LEnumerator.Current.Value;

  { Iterate till the last element in the LEnumerator }
  while true do
  begin
    if CompareValues(LEnumerator.Current.Value, Result) > 0 then
      Result := LEnumerator.Current.Value;

    { Exit if we hit the last element }
    if not LEnumerator.MoveNext() then
      Exit;
  end;
end;

function TAssociation<TKey, TValue>.MinKey: TKey;
var
  LEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := LEnumerator.Current.Key;

  { Iterate till the last element in the LEnumerator }
  while true do
  begin
    if CompareKeys(LEnumerator.Current.Key, Result) < 0 then
      Result := LEnumerator.Current.Key;

    { Exit if we hit the last element }
    if not LEnumerator.MoveNext() then
      Exit;
  end;
end;

function TAssociation<TKey, TValue>.MinValue: TValue;
var
  LEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := LEnumerator.Current.Value;

  { Iterate till the last element in the LEnumerator }
  while true do
  begin
    if CompareValues(LEnumerator.Current.Value, Result) < 0 then
      Result := LEnumerator.Current.Value;

    { Exit if we hit the last element }
    if not LEnumerator.MoveNext() then
      Exit;
  end;
end;

procedure TAssociation<TKey, TValue>.NotifyKeyRemoved(const AKey: TKey);
begin
  { Handle stuff }
  if Assigned(FKeyRemoveNotification) then
    FKeyRemoveNotification(AKey)
  else
    HandleKeyRemoved(AKey);
end;

procedure TAssociation<TKey, TValue>.NotifyValueRemoved(const AValue: TValue);
begin
  { Handle stuff }
  if Assigned(FValueRemoveNotification) then
    FValueRemoveNotification(AValue)
  else
    HandleValueRemoved(AValue);
end;

function TAssociation<TKey, TValue>.SelectKeys: ISequence<TKey>;
begin
  { Create a selector }
  Result := TSelectKeysSequence<TKey, TValue>.Create(Self);
end;

function TAssociation<TKey, TValue>.SelectValues: ISequence<TValue>;
begin
  { Create a selector }
  Result := TSelectValuesSequence<TKey, TValue>.Create(Self);
end;

function TAssociation<TKey, TValue>.ToDictionary: IDictionary<TKey, TValue>;
begin
  Result := TDictionary<TKey, TValue>.Create(KeyRules, ValueRules);
  Result.AddAll(Self);
end;

function TAssociation<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
var
  LEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Iterate till the last element in the LEnumerator }
  while LEnumerator.MoveNext do
  begin
    if KeysAreEqual(LEnumerator.Current.Key, AKey) then
      Exit(LEnumerator.Current.Value);
  end;

  { If nothing found, simply raise an exception }
  ExceptionHelper.Throw_KeyNotFoundError('AKey');
end;

function TAssociation<TKey, TValue>.ValuesAreEqual(const ALeft, ARight: TValue): Boolean;
begin
  { Lazy init }
  if not Assigned(FValueRules.FEqComparer) then
    FValueRules.FEqComparer := TEqualityComparer<TValue>.Default;

  Result := FValueRules.FEqComparer.Equals(ALeft, ARight);
end;

function TAssociation<TKey, TValue>.Where(
  const APredicate: TPredicate<TKey, TValue>): IAssociation<TKey, TValue>;
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Create a new Enex collection }
  Result := TAssociativeWhereSequence<TKey, TValue>.Create(Self, APredicate, False); // Don't invert the result
end;

function TAssociation<TKey, TValue>.WhereKeyBetween(const ALower,
  AHigher: TKey): IAssociation<TKey, TValue>;
var
  LLower, LHigher: TKey;
begin
  { Locals }
  LLower := ALower;
  LHigher := AHigher;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit((CompareKeys(Arg1, LLower) >= 0) and (CompareKeys(Arg1, LHigher) <= 0));
    end
  );
end;

function TAssociation<TKey, TValue>.WhereKeyGreater(
  const ABound: TKey): IAssociation<TKey, TValue>;
var
  LBound: TKey;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(CompareKeys(Arg1, LBound) > 0);
    end
  );
end;

function TAssociation<TKey, TValue>.WhereKeyGreaterOrEqual(
  const ABound: TKey): IAssociation<TKey, TValue>;
var
  LBound: TKey;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(CompareKeys(Arg1, LBound) >= 0);
    end
  );
end;

function TAssociation<TKey, TValue>.WhereKeyLower(
  const ABound: TKey): IAssociation<TKey, TValue>;
var
  LBound: TKey;
  LRules: TRules<TKey>;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(CompareKeys(Arg1, LBound) < 0);
    end
  );
end;

function TAssociation<TKey, TValue>.WhereKeyLowerOrEqual(
  const ABound: TKey): IAssociation<TKey, TValue>;
var
  LBound: TKey;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(CompareKeys(Arg1, LBound) <= 0);
    end
  );
end;

function TAssociation<TKey, TValue>.WhereNot(
  const APredicate: TPredicate<TKey, TValue>): IAssociation<TKey, TValue>;
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Create a new Enex collection }
  Result := TAssociativeWhereSequence<TKey, TValue>.Create(Self, APredicate, True); // Invert the result
end;

function TAssociation<TKey, TValue>.WhereValueBetween(
  const ALower, AHigher: TValue): IAssociation<TKey, TValue>;
var
  LLower, LHigher: TValue;
begin
  { Locals }
  LLower := ALower;
  LHigher := AHigher;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit((CompareValues(Arg2, LLower) >= 0) and (CompareValues(Arg2, LHigher) <= 0));
    end
  );
end;

function TAssociation<TKey, TValue>.WhereValueGreater(
  const ABound: TValue): IAssociation<TKey, TValue>;
var
  LBound: TValue;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(CompareValues(Arg2, LBound) > 0);
    end
  );
end;

function TAssociation<TKey, TValue>.WhereValueGreaterOrEqual(
  const ABound: TValue): IAssociation<TKey, TValue>;
var
  LBound: TValue;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(CompareValues(Arg2, LBound) >= 0);
    end
  );
end;

function TAssociation<TKey, TValue>.WhereValueLower(
  const ABound: TValue): IAssociation<TKey, TValue>;
var
  LBound: TValue;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(CompareValues(Arg2, LBound) < 0);
    end
  );
end;

function TAssociation<TKey, TValue>.WhereValueLowerOrEqual(
  const ABound: TValue): IAssociation<TKey, TValue>;
var
  LBound: TValue;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(CompareValues(Arg2, LBound) <= 0);
    end
  );
end;


{ TAbstractMap<TKey, TValue> }

procedure TAbstractMap<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
  ExceptionHelper.Throw_OperationNotSupported('Add');
end;

procedure TAbstractMap<TKey, TValue>.Add(const APair: TPair<TKey, TValue>);
begin
  Add(APair.Key, APair.Value);
end;

procedure TAbstractMap<TKey, TValue>.AddAll(const ACollection: IEnumerable<TPair<TKey, TValue>>);
var
  LPair: TPair<TKey, TValue>;
begin
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  for LPair in ACollection do
    Add(LPair.Key, LPair.Value);
end;

procedure TAbstractMap<TKey, TValue>.Clear;
var
  LKeys: IList<TKey>;
  LKey: TKey;
begin
  LKeys := SelectKeys().ToList();
  for LKey in LKeys do
  begin
    Remove(LKey);
    NotifyKeyRemoved(LKey);
  end;
end;

function TAbstractMap<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
var
  LEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  LEnumerator := GetEnumerator();
  while LEnumerator.MoveNext() do
    if KeysAreEqual(LEnumerator.Current.Key, AKey) then
      Exit(True);

  Result := False;
end;

function TAbstractMap<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
var
  LEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  LEnumerator := GetEnumerator();
  while LEnumerator.MoveNext() do
    if ValuesAreEqual(LEnumerator.Current.Value, AValue) then
      Exit(True);

  Result := False;
end;

destructor TAbstractMap<TKey, TValue>.Destroy;
begin
  Clear();
  inherited;
end;

procedure TAbstractMap<TKey, TValue>.Remove(const AKey: TKey);
begin
  ExceptionHelper.Throw_OperationNotSupported('Remove');
end;

{ Collections.TWhereSequence<T> }

constructor TWhereSequence<T>.Create(const ACollection: TSequence<T>;
  const APredicate: TPredicate<T>; const AInvertResult: Boolean);
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  inherited Create(ACollection.ElementRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FPredicate := APredicate;
  FInvertResult := AInvertResult;
end;

destructor TWhereSequence<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TWhereSequence<T>.GetEnumerator: IEnumerator<T>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self, FCollection.GetEnumerator());
end;

{ TWhereSequence<T>.TEnumerator }

function TWhereSequence<T>.TEnumerator.AcceptValue(const AValue: T): Boolean;
begin
  with TWhereSequence<T>(Owner) do
    Result := FPredicate(AValue) xor FInvertResult;
end;

{ TSelectSequence<T, TOut> }

constructor TSelectSequence<T, TOut>.Create(const ACollection: TSequence<T>;
  const ASelector: TFunc<T, TOut>; const ARules: TRules<TOut>);
begin
  { Check arguments }
  if not Assigned(ASelector) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector');

  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Rules ... }
  inherited Create(ARules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FSelector := ASelector;
end;

destructor TSelectSequence<T, TOut>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TSelectSequence<T, TOut>.ElementAt(const AIndex: NativeInt): TOut;
begin
  Result := FSelector(FCollection.ElementAt(AIndex));
end;

function TSelectSequence<T, TOut>.Empty: Boolean;
begin
  Result := FCollection.Empty;
end;

function TSelectSequence<T, TOut>.First: TOut;
begin
  Result := FSelector(FCollection.First);
end;

function TSelectSequence<T, TOut>.GetCount: NativeInt;
begin
  Result := FCollection.GetCount();
end;

function TSelectSequence<T, TOut>.GetEnumerator: IEnumerator<TOut>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FInEnumerator := FCollection.GetEnumerator();
  Result := LEnumerator;
end;

function TSelectSequence<T, TOut>.Last: TOut;
begin
  Result := FSelector(FCollection.Last);
end;

function TSelectSequence<T, TOut>.Single: TOut;
begin
  Result := FSelector(FCollection.Single);
end;

{ TSelectSequence<T, TOut>.TEnumerator }

function TSelectSequence<T, TOut>.TEnumerator.TryMoveNext(out ACurrent: TOut): Boolean;
begin
  { Next iteration }
  Result := FInEnumerator.MoveNext();

  { Terminate on sub-enum termination }
  if not Result then
    Exit;

  { Return the next "selected" element }
  ACurrent := TSelectSequence<T, TOut>(Owner).FSelector(FInEnumerator.Current);
end;

{ TConcatSequence<T> }

function TConcatSequence<T>.All(const APredicate: TPredicate<T>): Boolean;
begin
  Result := FCollection1.All(APredicate) and FCollection2.All(APredicate);
end;

function TConcatSequence<T>.Any(const APredicate: TPredicate<T>): Boolean;
begin
  Result := FCollection1.Any(APredicate) or FCollection2.Any(APredicate);
end;

constructor TConcatSequence<T>.Create(
  const ACollection1: TSequence<T>; const ACollection2: ISequence<T>);
begin
  { Check arguments }
  if not Assigned(ACollection1) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection1');

  if not Assigned(ACollection2) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection2');

  { Rules ... }
  inherited Create(ACollection1.ElementRules);

  { Assign internals }
  FCollection1 := ACollection1;
  KeepObjectAlive(FCollection1);

  FCollection2 := ACollection2;
end;

destructor TConcatSequence<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection1, false);

  inherited;
end;

function TConcatSequence<T>.Empty: Boolean;
begin
  Result := (GetCount = 0);
end;

function TConcatSequence<T>.GetCount: NativeInt;
begin
  Result := FCollection1.GetCount() + FCollection2.GetCount();
end;

function TConcatSequence<T>.GetEnumerator: IEnumerator<T>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FInEnumerator1 := FCollection1.GetEnumerator();
  LEnumerator.FInEnumerator2 := FCollection2.GetEnumerator();
  Result := LEnumerator;
end;

{ TConcatSequence<T>.TEnumerator }

function TConcatSequence<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  if Assigned(FInEnumerator1) then
  begin
    { Iterate over 1 }
    Result := FInEnumerator1.MoveNext();

    { Succesefully iterated collection 1 }
    if Result then
    begin
      ACurrent := FInEnumerator1.Current;
      Exit;
    end;

    { We've reached the bottom of 1 }
    FInEnumerator1 := nil;
  end;

  { Iterate over 2 now }
  Result := FInEnumerator2.MoveNext();
  if Result then
    ACurrent := FInEnumerator2.Current;
end;

{ TUnionSequence<T> }

constructor TUnionSequence<T>.Create(
  const ACollection1: TSequence<T>; const ACollection2: ISequence<T>);
begin
  { Check arguments }
  if not Assigned(ACollection1) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection1');

  if not Assigned(ACollection2) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection2');

  { Rules ... }
  inherited Create(ACollection1.ElementRules);

  { Assign internals }
  FCollection1 := ACollection1;
  KeepObjectAlive(FCollection1);

  FCollection2 := ACollection2;
end;

destructor TUnionSequence<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection1, false);

  inherited;
end;

function TUnionSequence<T>.GetEnumerator: IEnumerator<T>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FSet := THashSet<T>.Create();
  LEnumerator.FInEnumerator1 := FCollection1.GetEnumerator();
  LEnumerator.FInEnumerator2 := FCollection2.GetEnumerator();
  Result := LEnumerator;
end;

{ TUnionSequence<T>.TEnumerator }

function TUnionSequence<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  if Assigned(FInEnumerator1) then
  begin
    { Iterate over 1 }
    Result := FInEnumerator1.MoveNext();

    { Succesefully iterated collection 1 }
    if Result then
    begin
      { Add the element to the set }
      ACurrent := FInEnumerator1.Current;
      FSet.Add(ACurrent);
      Exit;
    end;

    { We've reached the bottom of 1 }
    FInEnumerator1 := nil;
  end;

  { Continue until we find what we need or we get to the bottom }
  while True do
  begin
    { Iterate over 2 now }
    Result := FInEnumerator2.MoveNext();

    { Exit on bad result }
    if not Result then
      Exit;

    { Exit if the element is good }
    if not FSet.Contains(FInEnumerator2.Current) then
    begin
      ACurrent := FInEnumerator2.Current;
      FSet.Add(ACurrent);
      Exit;
    end;
  end;
end;

{ TExclusionSequence<T> }

constructor TExclusionSequence<T>.Create(
  const ACollection1: TSequence<T>; const ACollection2: ISequence<T>);
begin
  { Check arguments }
  if not Assigned(ACollection1) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection1');

  if not Assigned(ACollection2) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection2');

  { Rules ... }
  inherited Create(ACollection1.ElementRules);

  { Assign internals }
  FCollection1 := ACollection1;
  KeepObjectAlive(FCollection1);

  FCollection2 := ACollection2;
end;

destructor TExclusionSequence<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection1, false);

  inherited;
end;

function TExclusionSequence<T>.GetEnumerator: IEnumerator<T>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FSet := THashSet<T>.Create();
  LEnumerator.FInEnumerator1 := FCollection1.GetEnumerator();
  LEnumerator.FInEnumerator2 := FCollection2.GetEnumerator();
  Result := LEnumerator;
end;

{ TExclusionSequence<T>.TEnumerator }

function TExclusionSequence<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  { Load the first enum into the set }
  if Assigned(FInEnumerator2) then
  begin
    while FInEnumerator2.MoveNext() do
      FSet.Add(FInEnumerator2.Current);

    FInEnumerator2 := nil;
  end;

  { Continue until we find what we need or we get to the bottom }
  while True do
  begin
    { Iterate over 1 }
    Result := FInEnumerator1.MoveNext();

    { Exit on bad result }
    if not Result then
      Exit;

    { Exit if the element is good }
    if not FSet.Contains(FInEnumerator1.Current) then
    begin
      ACurrent := FInEnumerator1.Current;
      Exit;
    end;
  end;
end;

{ TIntersectionSequence<T> }

constructor TIntersectionSequence<T>.Create(
  const ACollection1: TSequence<T>; const ACollection2: ISequence<T>);
begin
  { Check arguments }
  if not Assigned(ACollection1) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection1');

  if not Assigned(ACollection2) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection2');

  { Rules ... }
  inherited Create(ACollection1.ElementRules);

  { Assign internals }
  FCollection1 := ACollection1;
  KeepObjectAlive(FCollection1);

  FCollection2 := ACollection2;
end;

destructor TIntersectionSequence<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection1, false);

  inherited;
end;

function TIntersectionSequence<T>.GetEnumerator: IEnumerator<T>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FSet := THashSet<T>.Create();
  LEnumerator.FInEnumerator1 := FCollection1.GetEnumerator();
  LEnumerator.FInEnumerator2 := FCollection2.GetEnumerator();
  Result := LEnumerator;
end;

{ Collection.TIntersectionSequence<T>.TEnumerator }

function TIntersectionSequence<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  { Load the first enum into the set }
  if Assigned(FInEnumerator1) then
  begin
    while FInEnumerator1.MoveNext() do
      FSet.Add(FInEnumerator1.Current);

    FInEnumerator1 := nil;
  end;

  { Continue until we find what we need or we get to the bottom }
  while True do
  begin
    { Iterate over 1 }
    Result := FInEnumerator2.MoveNext();

    { Exit on bad result }
    if not Result then
      Exit;

    { Exit if the element is good }
    if FSet.Contains(FInEnumerator2.Current) then
    begin
      ACurrent := FInEnumerator2.Current;
      Exit;
    end;
  end;
end;

{ TRangeSequence<T> }

constructor TRangeSequence<T>.Create(const ACollection: TSequence<T>; const AStart, AEnd: NativeInt);
begin
  if AStart < 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStart');

  if AEnd < 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AEnd');

  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Rules ... }
  inherited Create(ACollection.ElementRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FStart := AStart;
  FEnd := AEnd;
end;

destructor TRangeSequence<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TRangeSequence<T>.GetEnumerator: IEnumerator<T>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FInEnumerator := FCollection.GetEnumerator();
  Result := LEnumerator;
end;

{ TRangeSequence<T>.TEnumerator }

function TRangeSequence<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  with TRangeSequence<T>(Owner) do
  begin
    while FCurrentIndex < FStart do
    begin
      { Move cursor }
      Result := FInEnumerator.MoveNext();
      if not Result then
        Exit;
      Inc(FCurrentIndex);
    end;

    { Check if we're finished }
    if (FCurrentIndex > FEnd) then
      Exit(false);

    { Move the cursor next in the sub-enum, and increase index }
    Result := FInEnumerator.MoveNext();
    if not Result then
      Exit;

    ACurrent := FInEnumerator.Current;
    Inc(FCurrentIndex);
  end;
end;

{ TDistinctSequence<T> }

constructor TDistinctSequence<T>.Create(const ACollection: TSequence<T>);
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  inherited Create(ACollection.ElementRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);
end;

destructor TDistinctSequence<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TDistinctSequence<T>.GetEnumerator: IEnumerator<T>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self, FCollection.GetEnumerator());
  LEnumerator.FSet := THashSet<T>.Create();
  Result := LEnumerator;
end;

{ TDistinctSequence<T>.TEnumerator }

function TDistinctSequence<T>.TEnumerator.AcceptValue(const AValue: T): Boolean;
begin
  Result := not FSet.Contains(AValue);
  if Result then
    FSet.Add(AValue);
end;

{ TFillSequence<T> }

function TFillSequence<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
var
  I: NativeInt;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := FElement;

  { Iterate over the last N - 1 elements }
  for I := 1 to FCount - 1 do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, FElement);
  end;
end;

function TFillSequence<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
var
  I: NativeInt;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if FCount = 0 then
    Exit(ADefault);

  { Select the first element as comparison base }
  Result := FElement;

  { Iterate over the last N - 1 elements }
  for I := 1 to FCount - 1 do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, FElement);
  end;
end;

function TFillSequence<T>.All(const APredicate: TPredicate<T>): Boolean;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if not APredicate(FElement) then
    Result := false
  else
    Result := true;
end;

function TFillSequence<T>.Any(const APredicate: TPredicate<T>): Boolean;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if APredicate(FElement) then
    Result := true
  else
    Result := false;
end;

constructor TFillSequence<T>.Create(const AElement: T; const ACount: NativeInt; const ARules: TRules<T>);
begin
  if ACount <= 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  { Install the type }
  inherited Create(ARules);

  { Copy values in }
  FCount := ACount;
  FElement := AElement;
end;

function TFillSequence<T>.ElementAt(const AIndex: NativeInt): T;
begin
  if (AIndex = FCount) or (AIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');

  Result := FElement;
end;

function TFillSequence<T>.ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T;
begin
  if (AIndex = FCount) or (AIndex < 0) then
    Result := ADefault
  else
    Result := FElement;
end;

function TFillSequence<T>.Empty: Boolean;
begin
  Result := (FCount = 0);
end;

function TFillSequence<T>.EqualsTo(const ACollection: IEnumerable<T>): Boolean;
var
  LValue: T;
  I: NativeInt;
begin
  I := 0;

  for LValue in ACollection do
  begin
    if I >= FCount then
      Exit(false);

    if not ElementsAreEqual(FElement, LValue) then
      Exit(false);

    Inc(I);
  end;

  if I < FCount then
    Exit(false);

  Result := true;
end;

function TFillSequence<T>.First: T;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FElement;
end;

function TFillSequence<T>.FirstOrDefault(const ADefault: T): T;
begin
  if FCount = 0 then
    Result := ADefault
  else
    Result := FElement;
end;

function TFillSequence<T>.GetCount: NativeInt;
begin
  Result := FCount;
end;

function TFillSequence<T>.GetEnumerator: IEnumerator<T>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FRemaining := FCount;
  Result := LEnumerator;
end;

function TFillSequence<T>.Last: T;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FElement;
end;

function TFillSequence<T>.LastOrDefault(const ADefault: T): T;
begin
  if FCount = 0 then
    Result := ADefault
  else
    Result := FElement;
end;

function TFillSequence<T>.Max: T;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FElement;
end;

function TFillSequence<T>.Min: T;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FElement;
end;

function TFillSequence<T>.Single: T;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError()
  else if FCount = 1 then
    Result := FElement
  else
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement();
end;

function TFillSequence<T>.SingleOrDefault(const ADefault: T): T;
begin
  if FCount = 0 then
    Result := ADefault
  else if FCount = 1 then
    Result := FElement
  else
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement();
end;

{ TFillSequence<T>.TEnumerator }

function TFillSequence<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  { Check for end }
  Result := FRemaining > 0;

  if Result then
  begin
    Dec(FRemaining);
    ACurrent := TFillSequence<T>(Owner).FElement;
  end;
end;

{ TSkipSequence<T> }

constructor TSkipSequence<T>.Create(const ACollection: TSequence<T>; const ACount: NativeInt);
begin
  { Check parameters }
  if ACount <= 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Installing the element type }
  inherited Create(ACollection.ElementRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FCount := ACount;
end;

destructor TSkipSequence<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TSkipSequence<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create the enumerator }
  Result := TEnumerator.Create(Self, FCollection.GetEnumerator());
end;

{ TSkipSequence<T>.TEnumerator }

function TSkipSequence<T>.TEnumerator.AcceptValue(const AValue: T): Boolean;
begin
  Result := FCurrentIndex >= TSkipSequence<T>(Owner).FCount;
  Inc(FCurrentIndex);
end;

{ TTakeSequence<T> }

constructor TTakeSequence<T>.Create(const ACollection: TSequence<T>; const ACount: NativeInt);
begin
  { Check parameters }
  if ACount <= 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Installing the element type }
  inherited Create(ACollection.ElementRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FCount := ACount;
end;

destructor TTakeSequence<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TTakeSequence<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create the enumerator }
  Result := TEnumerator.Create(Self, FCollection.GetEnumerator());
end;

{ TTakeSequence<T>.TEnumerator }

function TTakeSequence<T>.TEnumerator.AcceptValue(const AValue: T): Boolean;
begin
  Result := FCurrentIndex < TSkipSequence<T>(Owner).FCount;
  Inc(FCurrentIndex);
end;

{ TTakeWhileSequence<T> }

constructor TTakeWhileSequence<T>.Create(const ACollection: TSequence<T>; const APredicate: TPredicate<T>);
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Install the type }
  inherited Create(ACollection.ElementRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FPredicate := APredicate;
end;

destructor TTakeWhileSequence<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TTakeWhileSequence<T>.GetEnumerator: IEnumerator<T>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FInEnumerator := FCollection.GetEnumerator();
  Result := LEnumerator;
end;

{ TTakeWhileSequence<T>.TEnumerator }

function TTakeWhileSequence<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  Result := FInEnumerator.MoveNext() and
     TTakeWhileSequence<T>(Owner).FPredicate(FInEnumerator.Current);

  if Result then
    ACurrent := FInEnumerator.Current;
end;

{ TSkipWhileSequence<T> }

constructor TSkipWhileSequence<T>.Create(const ACollection: TSequence<T>; const APredicate: TPredicate<T>);
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Install the type }
  inherited Create(ACollection.ElementRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FPredicate := APredicate;
end;

destructor TSkipWhileSequence<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TSkipWhileSequence<T>.GetEnumerator: IEnumerator<T>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self, FCollection.GetEnumerator());
  Result := LEnumerator;
end;

{ TSkipWhileSequence<T>.TEnumerator }

function TSkipWhileSequence<T>.TEnumerator.AcceptValue(const AValue: T): Boolean;
begin
  if not FStarted then
  begin
    if TSkipWhileSequence<T>(Owner).FPredicate(AValue) then
      Exit(False);

    FStarted := True;
  end;

  Result := True;
end;

{ TGroupBySequence<T, TGroup> }

constructor TGroupBySequence<T, TBy>.Create(
  const ACollection: TSequence<T>; const ASelector: TFunc<T, TBy>);
begin
  { Check arguments }
  if not Assigned(ASelector) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector');

  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Install the type (some default type) }
  inherited Create();

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FSelector := ASelector;
end;

destructor TGroupBySequence<T, TBy>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TGroupBySequence<T, TBy>.GetEnumerator: IEnumerator<IGrouping<TBy, T>>;
var
  LDictionary: IDictionary<TBy, IList<T>>;
  LList: IList<T>;
  LSrcEnumerator: IEnumerator<T>;
  LDictEnumerator: IEnumerator<TPair<TBy, IList<T>>>;
  LGroup: TBy;
  LOutList: IList<IGrouping<TBy, T>>;
  LGrouping: TEnexGroupingCollection;
  LGroupingIntf: IGrouping<TBy, T>;
begin
  { Initialize the dictionary (need one that preserves the input order) }
  LDictionary := TLinkedDictionary<TBy, IList<T>>.Create();

  { Obtain the source enumerator }
  LSrcEnumerator := FCollection.GetEnumerator();
  while LSrcEnumerator.MoveNext() do
  begin
    LGroup := FSelector(LSrcEnumerator.Current);

    { Try to get the list of groupet input elements }
    if not LDictionary.TryGetValue(LGroup, LList) then
    begin
      LList := TList<T>.Create();
      LDictionary.Add(LGroup, LList);
    end;

    { Add the element that was grouped into the list, and move on ... }
    LList.Add(LSrcEnumerator.Current);
  end;

  { Build result and such things }
  LOutList := TList<IGrouping<TBy, T>>.Create();

  { Get the dictionary enumerator and build output }
  LDictEnumerator := LDictionary.GetEnumerator();
  while LDictEnumerator.MoveNext() do
  begin
    { Initialize the grouping structure }
    LGrouping := TEnexGroupingCollection.Create;
    LGrouping.FBy := LDictEnumerator.Current.Key;
    LGrouping.FList := LDictEnumerator.Current.Value;
    LGroupingIntf := LGrouping;

    { Place it into output }
    LOutList.Add(LGroupingIntf);
  end;

  LDictEnumerator := nil;
  LDictionary := nil;

  { Finally, provide the enumerator }
  Result := LOutList.GetEnumerator();
end;

{ TGroupBySequence<T, TKey>.TEnexGroupingCollection }

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.Aggregate(const AAggregator: TFunc<T, T, T>): T;
begin
  Result := FList.Aggregate(AAggregator);
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
begin
  Result := FList.AggregateOrDefault(AAggregator, ADefault);
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.All(const APredicate: TPredicate<T>): Boolean;
begin
  Result := FList.All(APredicate);
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.Any(const APredicate: TPredicate<T>): Boolean;
begin
  Result := FList.Any(APredicate);
end;

procedure TGroupBySequence<T, TBy>.TEnexGroupingCollection.CopyTo(var AArray: array of T; const AStartIndex: NativeInt);
begin
  FList.CopyTo(AArray, AStartIndex);
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.ElementAt(const AIndex: NativeInt): T;
begin
  Result := FList.ElementAt(AIndex);
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T;
begin
  Result := FList.ElementAtOrDefault(AIndex, ADefault);
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.Empty: Boolean;
begin
  Result := FList.Empty;
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.EqualsTo(const ACollection: IEnumerable<T>): Boolean;
begin
  Result := FList.EqualsTo(ACollection);
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.First: T;
begin
  Result := FList.First;
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.FirstOrDefault(const ADefault: T): T;
begin
  Result := FList.FirstOrDefault(ADefault);
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.GetCount: NativeInt;
begin
  Result := FList.Count;
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.GetEnumerator: IEnumerator<T>;
begin
  Result := FList.GetEnumerator();
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.GetKey: TBy;
begin
  Result := FBy;
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.Last: T;
begin
  Result := FList.Last;
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.LastOrDefault(const ADefault: T): T;
begin
  Result := FList.LastOrDefault(ADefault);
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.Max: T;
begin
  Result := FList.Max;
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.Min: T;
begin
  Result := FList.Min;
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.Single: T;
begin
  Result := FList.Single;
end;

function TGroupBySequence<T, TBy>.TEnexGroupingCollection.SingleOrDefault(const ADefault: T): T;
begin
  Result := FList.SingleOrDefault(ADefault);
end;

{ TSelectKeysSequence<TKey, TValue> }

constructor TSelectKeysSequence<TKey, TValue>.Create(const ACollection: TAssociation<TKey, TValue>);
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Install the type }
  inherited Create(ACollection.KeyRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);
end;

destructor TSelectKeysSequence<TKey, TValue>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TSelectKeysSequence<TKey, TValue>.GetCount: NativeInt;
begin
  Result := FCollection.GetCount();
end;

function TSelectKeysSequence<TKey, TValue>.GetEnumerator: IEnumerator<TKey>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FInEnumerator := FCollection.GetEnumerator();
  Result := LEnumerator;
end;

{ TSelectKeysSequence<TKey, TValue>.TEnumerator }

function TSelectKeysSequence<TKey, TValue>.TEnumerator.TryMoveNext(out ACurrent: TKey): Boolean;
begin
  { Next iteration }
  Result := FInEnumerator.MoveNext();
  if Result then
    ACurrent := FInEnumerator.Current.Key;
end;

{ TSelectValuesSequence<TKey, TValue> }

constructor TSelectValuesSequence<TKey, TValue>.Create(
  const ACollection: TAssociation<TKey, TValue>);
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Install the type }
  inherited Create(ACollection.ValueRules);

  { Assign internals }
  FCollection := ACollection;

  KeepObjectAlive(FCollection);
end;

destructor TSelectValuesSequence<TKey, TValue>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TSelectValuesSequence<TKey, TValue>.GetCount: NativeInt;
begin
  Result := FCollection.GetCount();
end;

function TSelectValuesSequence<TKey, TValue>.GetEnumerator: IEnumerator<TValue>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FInEnumerator := FCollection.GetEnumerator();
  Result := LEnumerator;
end;

{ TSelectValuesSequence<TKey, TValue>.TEnumerator }

function TSelectValuesSequence<TKey, TValue>.TEnumerator.TryMoveNext(out ACurrent: TValue): Boolean;
begin
  { Next iteration }
  Result := FInEnumerator.MoveNext();
  if Result then
    ACurrent := FInEnumerator.Current.Value;
end;

{ TAssociativeWhereSequence<TKey, TValue> }

constructor TAssociativeWhereSequence<TKey, TValue>.Create(
  const ACollection: TAssociation<TKey, TValue>;
  const APredicate: TPredicate<TKey, TValue>;
  const AInvertResult: Boolean);
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Install types }
  inherited Create(ACollection.KeyRules, ACollection.ValueRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FPredicate := APredicate;

  FInvertResult := AInvertResult;
end;

destructor TAssociativeWhereSequence<TKey, TValue>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TAssociativeWhereSequence<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self, FCollection.GetEnumerator());
end;

{ TAssociativeWhereSequence<TKey, TValue>.TEnumerator }

function TAssociativeWhereSequence<TKey, TValue>.TEnumerator.AcceptValue(const AValue: TPair<TKey, TValue>): Boolean;
begin
  with TAssociativeWhereSequence<TKey, TValue>(Owner) do
    Result := FPredicate(AValue.Key, AValue.Value) xor FInvertResult;
end;

{ TAssociativeDistinctByKeysSequence<TKey, TValue> }

constructor TAssociativeDistinctByKeysSequence<TKey, TValue>.Create(
  const ACollection: TAssociation<TKey, TValue>);
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Install types }
  inherited Create(ACollection.KeyRules, ACollection.ValueRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);
end;

destructor TAssociativeDistinctByKeysSequence<TKey, TValue>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TAssociativeDistinctByKeysSequence<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self, FCollection.GetEnumerator());
  LEnumerator.FSet := THashSet<TKey>.Create();
  Result := LEnumerator;
end;

{ TAssociativeDistinctByKeysSequence<TKey, TValue>.TEnumerator }

function TAssociativeDistinctByKeysSequence<TKey, TValue>.TEnumerator.AcceptValue(const AValue: TPair<TKey, TValue>): Boolean;
begin
  Result := not FSet.Contains(AValue.Key);
  if Result then
    FSet.Add(AValue.Key);
end;

{ TAssociativeDistinctByValuesSequence<TKey, TValue> }

constructor TAssociativeDistinctByValuesSequence<TKey, TValue>.Create(
  const ACollection: TAssociation<TKey, TValue>);
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Install types }
  inherited Create(ACollection.KeyRules, ACollection.ValueRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);
end;

destructor TAssociativeDistinctByValuesSequence<TKey, TValue>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TAssociativeDistinctByValuesSequence<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self, FCollection.GetEnumerator());
  LEnumerator.FSet := THashSet<TValue>.Create();
  Result := LEnumerator;
end;

{ TAssociativeDistinctByValuesSequence<TKey, TValue>.TEnumerator }

function TAssociativeDistinctByValuesSequence<TKey, TValue>.TEnumerator.AcceptValue(const AValue: TPair<TKey, TValue>): Boolean;
begin
  Result := not FSet.Contains(AValue.Value);
  if Result then
    FSet.Add(AValue.Value);
end;

{ TSelectClassSequence<T, TOut> }

constructor TSelectClassSequence<T, TOut>.Create(const ACollection: TSequence<T>; const ARules: TRules<TOut>);
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Installing the element type }
  inherited Create(ARules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);
end;

destructor TSelectClassSequence<T, TOut>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TSelectClassSequence<T, TOut>.GetEnumerator: IEnumerator<TOut>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FInEnumerator := FCollection.GetEnumerator();
  Result := LEnumerator;
end;

{ TSelectClassSequence<T, TOut>.TEnumerator }

function TSelectClassSequence<T, TOut>.TEnumerator.TryMoveNext(out ACurrent: TOut): Boolean;
begin
  { Iterate until given condition is met on an element }
  while True do
  begin
    Result := FInEnumerator.MoveNext();

    { Terminate on sub-enum termination }
    if not Result then
      Exit;

    { Check if T is TOut. Exit if yes}
    if Assigned(FInEnumerator.Current) and FInEnumerator.Current.InheritsFrom(TOut) then
    begin
      FCurrent := TOut(TObject(FInEnumerator.Current));
      Exit;
    end;
  end;
end;

{ TRules<T> }

class function TRules<T>.Create(const AComparer: IComparer<T>;
  const AEqualityComparer: IEqualityComparer<T>): TRules<T>;
begin
  if not Assigned(AComparer) then
    ExceptionHelper.Throw_ArgumentNilError('AComparer');

  if not Assigned(AEqualityComparer) then
    ExceptionHelper.Throw_ArgumentNilError('AEqualityComparer');

  { Initialize }
  Result.FComparer := AComparer;
  Result.FEqComparer := AEqualityComparer;
end;

class function TRules<T>.Custom(const AComparer: TCustomComparer<T>): TRules<T>;
begin
  if not Assigned(AComparer) then
    ExceptionHelper.Throw_ArgumentNilError('AComparer');

  { Init with proper stuff }
  Result.FComparer := AComparer;
  Result.FEqComparer := AComparer;
end;

class function TRules<T>.Default: TRules<T>;
begin
  { Init with proper stuff }
  Result.FComparer := TComparer<T>.Default;
  Result.FEqComparer := TEqualityComparer<T>.Default;
end;

{ TRefCountedObject }

procedure TRefCountedObject.AfterConstruction;
begin
  FInConstruction := false;
  inherited AfterConstruction();
end;

function TRefCountedObject.ExtractReference: IInterface;
var
  LRefCount: NativeInt;
begin
  { While constructing, an object has an implicit LRefCount count of 1 }
  if FInConstruction then
    LRefCount := 1
  else
    LRefCount := 0;

  {
      If the object is referenced in other places as an
      interface, get a new one, otherwise return nil
   }
  if RefCount > LRefCount then
    Result := Self
  else
    Result := nil;
end;

procedure TRefCountedObject.KeepObjectAlive(const AObject: TRefCountedObject);
var
  I, LKALen: NativeInt;
  LIntfRef: IInterface;
begin
  { Skip nil references }
  if not Assigned(AObject) then
    Exit;

  { Cannot self-ref! }
  if AObject = Self then
    ExceptionHelper.Throw_CannotSelfReferenceError();

  { Extract an optional reference, do not continue if failed }
  LIntfRef := AObject.ExtractReference();
  if not Assigned(LIntfRef) then
    Exit;

  LKALen := Length(FKeepAliveList);

  { Find a free spot }
  if LKALen > 0 then
    for I := 0 to LKALen - 1 do
      if not Assigned(FKeepAliveList[I]) then
      begin
        FKeepAliveList[I] := LIntfRef;
        Exit;
      end;

  { No free spots, extend array and insert the ref there }
  SetLength(FKeepAliveList, LKALen + 1);
  FKeepAliveList[LKALen] := LIntfRef;
end;

class function TRefCountedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance();

  { Set in construction! }
  TRefCountedObject(Result).FInConstruction := true;
end;

procedure TRefCountedObject.ReleaseObject(const AObject: TRefCountedObject; const AFreeObject: Boolean);
var
  I, LKALen: NativeInt;
  LIntfRef: IInterface;
begin
  { Do nothing on nil references, since it may be calle din destructors }
  if not Assigned(AObject) then
    Exit;

  { Cannot self-ref! }
  if AObject = Self then
    ExceptionHelper.Throw_CannotSelfReferenceError();

  { Extract an optional reference, if none received, exit }
  LIntfRef := AObject.ExtractReference();
  if not Assigned(LIntfRef) then
  begin
    if AFreeObject then
      AObject.Free;

    Exit;
  end;

  LKALen := Length(FKeepAliveList);

  { Find a free spot }
  if LKALen > 0 then
    for I := 0 to LKALen - 1 do
      if FKeepAliveList[I] = LIntfRef then
      begin
        { Release the spot and kill references to the interface }
        FKeepAliveList[I] := nil;
        LIntfRef := nil;
        Exit;
      end;
end;

{ ExceptionHelper }

class procedure ExceptionHelper.Throw_ArgumentNilError(const ArgName: String);
begin
  raise EArgumentNilException.CreateResFmt(@SNilArgument, [ArgName]);
end;

class procedure ExceptionHelper.Throw_ArgumentOutOfRangeError(const ArgName: String);
begin
  raise EArgumentOutOfRangeException.CreateResFmt(@SOutOfRangeArgument, [ArgName]);
end;

class procedure ExceptionHelper.Throw_ArgumentOutOfSpaceError(const ArgName: String);
begin
  raise EArgumentOutOfSpaceException.CreateResFmt(@SOutOfSpaceArgument, [ArgName]);
end;

class procedure ExceptionHelper.Throw_CannotSelfReferenceError;
begin
  raise ECannotSelfReferenceException.CreateRes(@SCannotSelfReference);
end;

class procedure ExceptionHelper.Throw_CollectionChangedError;
begin
  raise ECollectionChangedException.CreateRes(@SParentCollectionChanged);
end;

class procedure ExceptionHelper.Throw_CollectionEmptyError;
begin
  raise ECollectionEmptyException.CreateRes(@SEmptyCollection);
end;

class procedure ExceptionHelper.Throw_CollectionHasMoreThanOneElement;
begin
  raise ECollectionNotOneException.CreateRes(@SCollectionHasMoreThanOneElements);
end;

class procedure ExceptionHelper.Throw_CollectionHasNoFilteredElements;
begin
  raise ECollectionFilteredEmptyException.CreateRes(@SCollectionHasNoFilteredElements);
end;

class procedure ExceptionHelper.Throw_DuplicateKeyError(const ArgName: String);
begin
  raise EDuplicateKeyException.CreateResFmt(@SDuplicateKey, [ArgName]);
end;

class procedure ExceptionHelper.Throw_KeyNotFoundError(const ArgName: String);
begin
  raise EKeyNotFoundException.CreateResFmt(@SKeyNotFound, [ArgName]);
end;

class procedure ExceptionHelper.Throw_OperationNotSupported(const AOperation: String);
begin
  raise ENotSupportedException.CreateResFmt(@SOperationNotSupported, [AOperation]);
end;

class procedure ExceptionHelper.Throw_TypeDoesNotExposeMember(const MemberName: String);
begin
  raise ENotSupportedException.CreateResFmt(@STypeDoesNotExposeMember, [MemberName]);
end;

class procedure ExceptionHelper.Throw_TypeNotAClassError(const TypeName: String);
begin
  raise ENotSupportedException.CreateResFmt(@STypeNotAClass, [TypeName]);
end;

class procedure ExceptionHelper.Throw_BadClassReference(const ATypeInfo: PTypeInfo);
begin
  raise ESerializationException.CreateResFmt(@SBadClassReference, [GetTypeName(ATypeInfo), TypeKindToStr(ATypeInfo^.Kind)]);
end;

class procedure ExceptionHelper.Throw_BadDynamicArrayReference(const ATypeInfo: PTypeInfo);
begin
  raise ESerializationException.CreateResFmt(@SBadDynamicArrayReference, [GetTypeName(ATypeInfo), TypeKindToStr(ATypeInfo^.Kind)]);
end;

class procedure ExceptionHelper.Throw_BadRecordReference(const ATypeInfo: PTypeInfo);
begin
  raise ESerializationException.CreateResFmt(@SBadRecordReference, [GetTypeName(ATypeInfo), TypeKindToStr(ATypeInfo^.Kind)]);
end;

class procedure ExceptionHelper.Throw_ExpectedAnotherBinaryValuePoint;
begin
  raise ESerializationException.CreateRes(@SExpectedAnotherBinaryValuePoint);
end;

class procedure ExceptionHelper.Throw_ExpectedAnotherElementCount(const AArrayType: TRttiArrayType; const AExpectedCount, AActualCount: NativeInt);
begin
  raise ESerializationException.CreateResFmt(@SExpectedAnotherElementCount, [AArrayType.Name, AExpectedCount, AActualCount]);
end;

class procedure ExceptionHelper.Throw_ExpectedAnotherField(const AExpected: TRttiField; const AName: string; AOffset: Int64);
begin
  raise ESerializationException.CreateResFmt(@SExpectedAnotherField, [AExpected.Name, AExpected.Offset, AName, AOffset]);
end;

class procedure ExceptionHelper.Throw_ExpectedAnotherLabel(const AExpectedLabel, AActualLabel: string);
begin
  raise ESerializationException.CreateResFmt(@SExpectedAnotherLabel, [AExpectedLabel, AActualLabel]);
end;

class procedure ExceptionHelper.Throw_ExpectedAnotherSetSize(const AExpectedSize, AActualSize: NativeInt);
begin
  raise ESerializationException.CreateResFmt(@SExpectedAnotherSetSize, [AExpectedSize, AActualSize]);
end;

class procedure ExceptionHelper.Throw_ExpectedAnotherType(const AExpected: TRttiType; const AActual: string);
begin
  raise ESerializationException.CreateResFmt(@SExpectedAnotherType, [AExpected.Name, AActual]);
end;

class procedure ExceptionHelper.Throw_FieldTypeDoesNotHaveEnoughRtti(const AField: TRttiField);
begin
  raise ESerializationException.CreateResFmt(@SFieldTypeDoesNotHaveEnoughRtti, [AField.Name, AField.Parent.Name, TypeKindToStr(AField.Parent.TypeKind)]);
end;

class procedure ExceptionHelper.Throw_TypeCannotBeSerialized(const ATypeInfo: PTypeInfo);
begin
  raise ESerializationException.CreateResFmt(@STypeCannotBeSerialized, [GetTypeName(ATypeInfo), TypeKindToStr(ATypeInfo^.Kind)]);
end;

class procedure ExceptionHelper.Throw_TypeDoesNotHaveEnoughRtti(const ATypeInfo: PTypeInfo);
begin
  raise ESerializationException.CreateResFmt(@STypeDoesNotHaveEnoughRtti, [GetTypeName(ATypeInfo), TypeKindToStr(ATypeInfo^.Kind)]);
end;

end.
