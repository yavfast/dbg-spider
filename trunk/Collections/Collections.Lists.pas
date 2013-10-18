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

{$DEFINE OPTIMIZED_SORT}
unit Collections.Lists;
interface
uses SysUtils,
     Generics.Defaults,
     Collections.Serialization,
     Collections.Base;

type
  ///  <summary>The abstract base class for all generic <c>list</c> collections.</summary>
  ///  <remarks>Descending classes must implement the required abstract methods and optionally can implement
  ///  the non-required method.</remarks>
  TAbstractList<T> = class(TCollection<T>, IList<T>)
  protected
    ///  <summary>Returns the item at a given index.</summary>
    ///  <param name="AIndex">The index in the list.</param>
    ///  <returns>The element at the specified position.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    function GetItem(const AIndex: NativeInt): T;

    ///  <summary>Sets the item at a given index.</summary>
    ///  <param name="AIndex">The index in the list.</param>
    ///  <param name="AValue">The new value.</param>
    ///  <remarks>In the current implementation this method always raises an exception.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">Always raised in this implementation.</exception>
    procedure SetItem(const AIndex: NativeInt; const AValue: T); virtual;

    ///  <summary>Replaces a given item with a new one.</summary>
    ///  <param name="ACurrent">The item to be replaced.</param>
    ///  <param name="ANew">The item to be replaced with.</param>
    ///  <remarks>This method is called by the list when an item at a specified index needs to be replaced with another.
    ///  The default implementation will compare the values, if those are equal nothing is done. Otherwise the old item is
    ///  "disposed of" and the new one is copied over. Descendant classes my want another behaviour.</remarks>
    procedure ReplaceItem(var ACurrent: T; const ANew: T); virtual;

    ///  <summary>Extracts the element from the specified index.</summary>
    ///  <param name="AIndex">The index to stract from.</param>
    ///  <param name="AValue">The value stored at that index.</param>
    ///  <returns><c>True</c> if the value was found and stored in <paramref name="AValue"/> parameter; <c>False</c> otherwise.</returns>
    ///  <remarks>Descending classes must implement this method in order to support both <c>RemoveAt</c> and <c>ExtractAt</c> methods. In the current implementation
    ///  this method always raises an exception.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">Always raised in this implementation.</exception>
    function TryExtractAt(const AIndex: NativeInt; out AValue: T): Boolean; virtual;

    ///  <summary>Returns the item at a given index.</summary>
    ///  <param name="AIndex">The index in the list.</param>
    ///  <param name="AValue">The value stored at that index.</param>
    ///  <returns><c>True</c> if the value was found and stored in <paramref name="AValue"/> parameter; <c>False</c> otherwise.</returns>
    ///  <remarks>The current implementation iterates over all elements until the element at the specified index is reached.</remarks>
    function TryGetItemAt(const AIndex: NativeInt; out AValue: T): Boolean; virtual;
  public
    ///  <summary>Creates a new <c>list</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the list.</param>
    constructor Create(const ARules: TRules<T>);

    ///  <summary>Appends an element to the back of the list.</summary>
    ///  <param name="AValue">The value to append.</param>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>Insert</c> method is not overridden.</exception>
    procedure Add(const AValue: T); override;

    ///  <summary>Appends all elements from a given collection to the back of a list.</summary>
    ///  <param name="ACollection">The values to append.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>Insert</c> method is not overridden.</exception>
    procedure AddAll(const ACollection: IEnumerable<T>); override;

    ///  <summary>Inserts an element into the list.</summary>
    ///  <param name="AIndex">The index to insert to.</param>
    ///  <param name="AValue">The value to insert.</param>
    ///  <remarks>All elements starting with <paramref name="AIndex"/> are moved to the right by one and then
    ///  <paramref name="AValue"/> is placed at position <paramref name="AIndex"/>.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">Always raised in this implementation.</exception>
    procedure Insert(const AIndex: NativeInt; const AValue: T); virtual;

    ///  <summary>Inserts the elements of a collection into the list.</summary>
    ///  <param name="AIndex">The index to insert to.</param>
    ///  <param name="ACollection">The values to insert.</param>
    ///  <remarks>All elements starting with <paramref name="AIndex"/> are moved to the right by the length of
    ///  <paramref name="ACollection"/> and then <paramref name="AValue"/> is placed at position <paramref name="AIndex"/>.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If one element based <c>Insert</c> method is not overridden.</exception>
    procedure InsertAll(const AIndex: NativeInt; const ACollection: IEnumerable<T>); virtual;

    ///  <summary>Removes a given value from the list.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>If the list does not contain the given value, nothing happens.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>TryExtractAt</c> method is not overridden.</exception>
    procedure Remove(const AValue: T); override;

    ///  <summary>Removes an element from the list at a given index.</summary>
    ///  <param name="AIndex">The index from which to remove the element.</param>
    ///  <remarks>This method removes the specified element and moves all following elements to the left by one.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If one element based <c>TryExtractAt</c> method is not overridden.</exception>
    procedure RemoveAt(const AIndex: NativeInt);

    ///  <summary>Extracts an element from the list at a given index.</summary>
    ///  <param name="AIndex">The index from which to extract the element.</param>
    ///  <remarks>This method removes the specified element and moves all following elements to the left by one.
    ///  The removed element is returned to the caller.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>TryExtractAt</c> method is not overridden.</exception>
    function ExtractAt(const AIndex: NativeInt): T;

    ///  <summary>Checks whether the list contains a given value.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns><c>True</c> if the value was found in the list; <c>False</c> otherwise.</returns>
    ///  <remarks>The implementation in this class iterates over all elements and checks for the requested
    ///  value. Most descendant classes will most likely provide a better version.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>IndexOf</c> method is not overridden.</exception>
    function Contains(const AValue: T): Boolean; override;

    ///  <summary>Searches for the first appearance of a given element in this list.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <returns><c>-1</c> if the value was not found; otherwise a positive value indicating the index of the value.</returns>
    function IndexOf(const AValue: T): NativeInt; virtual;

    ///  <summary>Searches for the last appearance of a given element in this list.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <returns><c>-1</c> if the value was not found; otherwise a positive value indicating the index of the value.</returns>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>LastIndexOf</c> method is not overridden.</exception>
    function LastIndexOf(const AValue: T): NativeInt; virtual;

    ///  <summary>Returns the item at a given index.</summary>
    ///  <param name="AIndex">The index in the collection.</param>
    ///  <returns>The element at the specified position.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    property Items[const AIndex: NativeInt]: T read GetItem write SetItem; default;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <returns>The element at the specified position.</returns>
    ///  <remrks>In the current implementation this method uses <c>TryGetItemAt</c>.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    function ElementAt(const AIndex: NativeInt): T; override;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <param name="ADefault">The default value returned if the list is empty.</param>
    ///  <remrks>In the current implementation this method uses <c>TryGetItemAt</c>.</remarks>
    ///  <returns>The element at the specified position if the list is not empty and the position is not out of bounds; otherwise
    ///  the value of <paramref name="ADefault"/> is returned.</returns>
    function ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T; override;
  end;

  ///  <summary>The abstract base class for all generic <c>linked list</c> collections.</summary>
  ///  <remarks>Descending classes must implement the required abstract methods and optionally can implement
  ///  the non-required method.</remarks>
  TAbstractLinkedList<T> = class(TAbstractList<T>, ILinkedList<T>, IQueue<T>, IStack<T>)
  protected
    procedure IStack<T>.Push = AddLast;
     function IStack<T>.Pop = ExtractLast;
     function IStack<T>.Peek = Last;
    procedure IQueue<T>.Enqueue = AddLast;
     function IQueue<T>.Dequeue = ExtractFirst;
     function IQueue<T>.Peek = First;
  public
    ///  <summary>Appends an element to the back of list.</summary>
    ///  <param name="AValue">The value to append.</param>
    ///  <remarks>This method is functionally identical to <c>Add</c>. Classes that implement this interface can simply
    ///  alias this method to <c>Add</c>.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>Insert</c> method is not overridden.</exception>
    procedure AddLast(const AValue: T); virtual;

    ///  <summary>Appends the elements from a collection to the back of the list.</summary>
    ///  <param name="ACollection">The values to append.</param>
    ///  <remarks>This method is functionally identical to <c>Add</c>. Classes that implement this interface can simply
    ///  alias this method to <c>Add</c>.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>Insert</c> method is not overridden.</exception>
    procedure AddAllLast(const ACollection: IEnumerable<T>); virtual;

    ///  <summary>Appends an element to the front of the list.</summary>
    ///  <param name="AValue">The value to append.</param>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>Insert</c> method is not overridden.</exception>
    procedure AddFirst(const AValue: T); virtual;

    ///  <summary>Appends the elements from a collection to the back of the list.</summary>
    ///  <param name="ACollection">The values to append.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>Insert</c> method is not overridden.</exception>
    procedure AddAllFirst(const ACollection: IEnumerable<T>); virtual;

    ///  <summary>Removes the first element of the list.</summary>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>TryExtractAt</c> method is not overridden.</exception>
    procedure RemoveFirst(); virtual;

    ///  <summary>Removes the last element of the list.</summary>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>TryExtractAt</c> method is not overridden.</exception>
    procedure RemoveLast(); virtual;

    ///  <summary>Extracts the first element of the list.</summary>
    ///  <returns>The first element of the list.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>TryExtractAt</c> method is not overridden.</exception>
    function ExtractFirst(): T; virtual;

    ///  <summary>Removes the last element of the list.</summary>
    ///  <returns>The last element of the list.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">If <c>TryExtractAt</c> method is not overridden.</exception>
    function ExtractLast(): T; virtual;
  end;

type
  ///  <summary>The generic <c>list</c> collection.</summary>
  ///  <remarks>This type uses an internal array to store its values.</remarks>
  TList<T> = class(TAbstractList<T>, IDynamic, ISerializable)
  private type
    {$REGION 'Internal Types'}
{$IFDEF OPTIMIZED_SORT}
    { Stack entry }
    TStackEntry = record
      First, Last: NativeInt;
    end;

    { Required for the non-recursive QSort }
    TQuickSortStack = array[0..63] of TStackEntry;
{$ENDIF}

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

    {$HINTS OFF}
    procedure QuickSort(ALeft, ARight: NativeInt; const ASortProc: TComparison<T>); overload;
    procedure QuickSort(ALeft, ARight: NativeInt; const AAscending: Boolean); overload;
    {$HINTS ON}

  protected
    ///  <summary>Serializes the contents of this class to a given context.</summary>
    ///  <param name="AContext">The output context where data is written.</param>
    ///  <remarks>Override in descending classes to provide more serialization data.</remarks>
    procedure Serialize(const AContext: TOutputContext); virtual;

    ///  <summary>Deserializes the contents of this class from a given context.</summary>
    ///  <param name="AContext">The input context from which the data is read.</param>
    ///  <remarks>Override in descending classes to obtain more deserialization data.</remarks>
    procedure Deserialize(const AContext: TInputContext); virtual;

    ///  <summary>Sets the item at a given index.</summary>
    ///  <param name="AIndex">The index in the list.</param>
    ///  <param name="AValue">The new value.</param>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    procedure SetItem(const AIndex: NativeInt; const AValue: T); override;

    ///  <summary>Returns the number of elements in the list.</summary>
    ///  <returns>A positive value specifying the number of elements in the list.</returns>
    function GetCount(): NativeInt; override;

    ///  <summary>Returns the current capacity.</summary>
    ///  <returns>A positive number that specifies the number of elements that the list can hold before it
    ///  needs to grow again.</returns>
    ///  <remarks>The value of this method is greater than or equal to the amount of elements in the list. If this value
    ///  is greater than the number of elements, it means that the list has some extra capacity to operate upon.</remarks>
    function GetCapacity(): NativeInt;

    ///  <summary>Extracts the element from the specified index.</summary>
    ///  <param name="AIndex">The index to stract from.</param>
    ///  <param name="AValue">The value stored at that index.</param>
    ///  <returns><c>True</c> if the value was found and stored in <paramref name="AValue"/> parameter; <c>False</c> otherwise.</returns>
    function TryExtractAt(const AIndex: NativeInt; out AValue: T): Boolean; override;

    ///  <summary>Returns the item at a given index.</summary>
    ///  <param name="AIndex">The index in the list.</param>
    ///  <param name="AValue">The value stored at that index.</param>
    ///  <returns><c>True</c> if the value was found and stored in <paramref name="AValue"/> parameter; <c>False</c> otherwise.</returns>
    function TryGetItemAt(const AIndex: NativeInt; out AValue: T): Boolean; override;
  public
    ///  <summary>Creates a new <c>list</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>list</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the list.</param>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Creates a new <c>list</c> collection.</summary>
    ///  <param name="AInitialCapacity">The set's initial capacity.</param>
    ///  <param name="ARules">A rule set describing the elements in the list.</param>
    constructor Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt); overload;

    ///  <summary>Clears the contents of the list.</summary>
    procedure Clear(); override;

    ///  <summary>Inserts an element into the list.</summary>
    ///  <param name="AIndex">The index to insert to.</param>
    ///  <param name="AValue">The value to insert.</param>
    ///  <remarks>All elements starting with <paramref name="AIndex"/> are moved to the right by one and then
    ///  <paramref name="AValue"/> is placed at position <paramref name="AIndex"/>.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    procedure Insert(const AIndex: NativeInt; const AValue: T); override;

    ///  <summary>Inserts the elements of a collection into the list.</summary>
    ///  <param name="AIndex">The index to insert to.</param>
    ///  <param name="ACollection">The values to insert.</param>
    ///  <remarks>All elements starting with <paramref name="AIndex"/> are moved to the right by the length of
    ///  <paramref name="ACollection"/> and then <paramref name="AValue"/> is placed at position <paramref name="AIndex"/>.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    procedure InsertAll(const AIndex: NativeInt; const ACollection: IEnumerable<T>); override;

    ///  <summary>Reverses the elements in this list.</summary>
    ///  <param name="AStartIndex">The start index.</param>
    ///  <param name="ACount">The count of elements.</param>
    ///  <remarks>This method reverses <paramref name="ACount"/> number of elements in
    ///  the list, starting with the <paramref name="AStartIndex"/> element.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException">Parameter combination is incorrect.</exception>
    procedure Reverse(const AStartIndex, ACount: NativeInt); overload;

    ///  <summary>Reverses the elements in this list.</summary>
    ///  <param name="AStartIndex">The start index.</param>
    ///  <remarks>This method reverses all elements in the list, starting with the <paramref name="AStartIndex"/> element.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    procedure Reverse(const AStartIndex: NativeInt); overload;

    ///  <summary>Reverses the elements in this list.</summary>
    procedure Reverse(); overload;

    ///  <summary>Sorts the contents of this list.</summary>
    ///  <param name="AStartIndex">The start index.</param>
    ///  <param name="ACount">The count of elements.</param>
    ///  <param name="AAscending">Specifies whether ascending or descending sorting is performed. The default is <c>True</c>.</param>
    ///  <remarks>This method sorts <paramref name="ACount"/> number of elements in
    ///  the list, starting with the <paramref name="AStartIndex"/> element.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException">Parameter combination is incorrect.</exception>
    procedure Sort(const AStartIndex, ACount: NativeInt; const AAscending: Boolean = true); overload;

    ///  <summary>Sorts the contents of this list.</summary>
    ///  <param name="AStartIndex">The start index.</param>
    ///  <param name="AAscending">Specifies whether ascending or descending sorting is performed. The default is <c>True</c>.</param>
    ///  <remarks>This method sorts all elements in the list, starting with the <paramref name="AStartIndex"/> element.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    procedure Sort(const AStartIndex: NativeInt; const AAscending: Boolean = true); overload;

    ///  <summary>Sorts the contents of this list.</summary>
    ///  <param name="AAscending">Specifies whether ascending or descending sorting is performed. The default is <c>True</c>.</param>
    procedure Sort(const AAscending: Boolean = true); overload;

    ///  <summary>Sorts the contents of this list using a given comparison method.</summary>
    ///  <param name="AStartIndex">The start index.</param>
    ///  <param name="ACount">The count of elements.</param>
    ///  <param name="ASortProc">The method used to compare elements.</param>
    ///  <remarks>This method sorts <paramref name="ACount"/> number of elements in
    ///  the list, starting with the <paramref name="AStartIndex"/> element.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException">Parameter combination is incorrect.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASortProc"/> is <c>nil</c>.</exception>
    procedure Sort(const AStartIndex, ACount: NativeInt; const ASortProc: TComparison<T>); overload;

    ///  <summary>Sorts the contents of this list using a given comparison method.</summary>
    ///  <param name="AStartIndex">The start index.</param>
    ///  <param name="ASortProc">The method used to compare elements.</param>
    ///  <remarks>This method sorts all elements in the list, starting with the <paramref name="AStartIndex"/> element.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException">Parameter combination is incorrect.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASortProc"/> is <c>nil</c>.</exception>
    procedure Sort(const AStartIndex: NativeInt; const ASortProc: TComparison<T>); overload;

    ///  <summary>Sorts the contents of this list using a given comparison method.</summary>
    ///  <param name="ASortProc">The method used to compare elements.</param>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException">Parameter combination is incorrect.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASortProc"/> is <c>nil</c>.</exception>
    procedure Sort(const ASortProc: TComparison<T>); overload;

    ///  <summary>Searches for the first appearance of a given element in this list.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <returns><c>-1</c> if the value was not found; otherwise a positive value indicating the index of the value.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException">Parameter combination is incorrect.</exception>
    function IndexOf(const AValue: T): NativeInt; override;

    ///  <summary>Searches for the last appearance of a given element in this list.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <returns><c>-1</c> if the value was not found; otherwise a positive value indicating the index of the value.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException">Parameter combination is incorrect.</exception>
    function LastIndexOf(const AValue: T): NativeInt; override;

    ///  <summary>Specifies the number of elements in the list.</summary>
    ///  <returns>A positive value specifying the number of elements in the list.</returns>
    property Count: NativeInt read FLength;

    ///  <summary>Specifies the current capacity.</summary>
    ///  <returns>A positive number that specifies the number of elements that the list can hold before it
    ///  needs to grow again.</returns>
    ///  <remarks>The value of this property is greater than or equal to the amount of elements in the list. If this value
    ///  if greater than the number of elements, it means that the list has some extra capacity to operate upon.</remarks>
    property Capacity: NativeInt read GetCapacity;

    ///  <summary>Returns a new enumerator object used to enumerate this list.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the list.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<T>; override;

    ///  <summary>Removes the excess capacity from the list.</summary>
    ///  <remarks>This method can be called manually to force the list to drop the extra capacity it might hold. For example,
    ///  after performing some massive operations on a big list, call this method to ensure that all extra memory held by the
    ///  list is released.</remarks>
    procedure Shrink();

    ///  <summary>Forces the list to increase its capacity.</summary>
    ///  <remarks>Call this method to force the list to increase its capacity ahead of time. Manually adjusting the capacity
    ///  can be useful in certain situations.</remarks>
    procedure Grow();

    ///  <summary>Copies the values stored in the list to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the list.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the list.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Checks whether the list is empty.</summary>
    ///  <returns><c>True</c> if the list is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the list is empty.</remarks>
    function Empty(): Boolean; override;

    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the list considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    function Max(): T; override;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the list considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    function Min(): T; override;

    ///  <summary>Returns the first element.</summary>
    ///  <returns>The first element in the list.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    function First(): T; override;

    ///  <summary>Returns the first element or a default, if the list is empty.</summary>
    ///  <param name="ADefault">The default value returned if the list is empty.</param>
    ///  <returns>The first element in the list if the list is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function FirstOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the last element.</summary>
    ///  <returns>The last element in the list.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    function Last(): T; override;

    ///  <summary>Returns the last element or a default, if the list is empty.</summary>
    ///  <param name="ADefault">The default value returned if the list is empty.</param>
    ///  <returns>The last element in the list if the list is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function LastOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the single element stored in the list.</summary>
    ///  <returns>The element in the list.</returns>
    ///  <remarks>This method checks if the list contains just one element, in which case it is returned.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionNotOneException">There is more than one element in the list.</exception>
    function Single(): T; override;

    ///  <summary>Returns the single element stored in the list, or a default value.</summary>
    ///  <param name="ADefault">The default value returned if there are less or more elements in the list.</param>
    ///  <returns>The element in the list if the condition is satisfied; <paramref name="ADefault"/> is returned otherwise.</returns>
    ///  <remarks>This method checks if the list contains just one element, in which case it is returned. Otherwise
    ///  the value in <paramref name="ADefault"/> is returned.</remarks>
    function SingleOrDefault(const ADefault: T): T; override;

    ///  <summary>Aggregates a value based on the list's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <returns>A value that contains the list's aggregated value.</returns>
    ///  <remarks>This method returns the first element if the list only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation, where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    function Aggregate(const AAggregator: TFunc<T, T, T>): T; override;

    ///  <summary>Aggregates a value based on the list's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <param name="ADefault">The default value returned if the list is empty.</param>
    ///  <returns>A value that contains the list's aggregated value. If the list is empty, <paramref name="ADefault"/> is returned.</returns>
    ///  <remarks>This method returns the first element if the list only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T; override;

    ///  <summary>Checks whether at least one element in the list satisfies a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if at least one element satisfies a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole list and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>True</c>. The logical equivalent of this operation is "OR".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Any(const APredicate: TPredicate<T>): Boolean; override;

    ///  <summary>Checks that all elements in the list satisfy a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if all elements satisfy a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole list and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>False</c>. The logical equivalent of this operation is "AND".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function All(const APredicate: TPredicate<T>): Boolean; override;

    ///  <summary>Checks whether the elements in this list are equal to the elements in another collection.</summary>
    ///  <param name="ACollection">The collection to compare to.</param>
    ///  <returns><c>True</c> if the collections are equal; <c>False</c> if the collections are different.</returns>
    ///  <remarks>This method checks that each element at position X in this list is equal to an element at position X in
    ///  the provided collection. If the number of elements in both collections is different, then the collections are considered different.
    ///  Note that the comparison of elements is done using the rule set used by this list. This means that comparing this collection
    ///  to another one might yeild a different result than comparing the other collection to this one.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function EqualsTo(const ACollection: IEnumerable<T>): Boolean; override;
  end;

  ///  <summary>The generic <c>list</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses an internal array to store its objects.</remarks>
  TObjectList<T: class> = class(TList<T>)
  private
    FOwnsObjects: Boolean;

  protected
    ///  <summary>Frees the object that was removed from the collection.</summary>
    ///  <param name="AElement">The object that was removed from the collection.</param>
    procedure HandleElementRemoved(const AElement: T); override;

    ///  <summary>Replaces a given object with a new one.</summary>
    ///  <param name="ACurrent">The object to be replaced.</param>
    ///  <param name="ANew">The object to be replaced with.</param>
    ///  <remarks>This method will check the objects by reference and free the current one if needed.</remarks>
    procedure ReplaceItem(var ACurrent: T; const ANew: T); override;
  public
    ///  <summary>Specifies whether this list owns the objects stored in it.</summary>
    ///  <returns><c>True</c> if the list owns its objects; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the list controls the life-time of the stored objects.</remarks>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

type
  ///  <summary>The generic <c>sorted list</c> collection.</summary>
  ///  <remarks>This type uses an internal array to store its values.</remarks>
  TSortedList<T> = class(TList<T>)
  private var
    FAscending: Boolean;

    procedure InternalInsert(const AIndex: NativeInt; const AValue: T);
    function BinarySearch(const AElement: T; const AStartIndex, ACount: NativeInt; const AAscending: Boolean): NativeInt;
  protected
    ///  <summary>Sets the item at a given index.</summary>
    ///  <param name="AIndex">The index in the list.</param>
    ///  <param name="AValue">The new value.</param>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">Always raised in this implementation.</exception>
    procedure SetItem(const AIndex: NativeInt; const AValue: T); override;
  public
    ///  <summary>Creates a new <c>list</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed. The elements are stored in ascending order.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>list</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the list.</param>
    ///  <remarks>The elements are stored in ascending order.</remarks>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Creates a new <c>list</c> collection.</summary>
    ///  <param name="AInitialCapacity">The set's initial capacity.</param>
    ///  <param name="AAscending">Pass in a value of <c>True</c> if the elements should be kept in ascending order.
    ///  Pass in <c>False</c> for descending order.</param>
    ///  <param name="ARules">A rule set describing the elements in the list.</param>
    constructor Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt; const AAscending: Boolean); overload;

    ///  <summary>Adds an element to the list.</summary>
    ///  <param name="AValue">The value to add.</param>
    ///  <param name="AIndex">The index where to insert the element.</param>
    ///  <remarks>This method always raises an exception because inserting is not allowed on sorted collections.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">Always raised in this implementation.</exception>
    procedure Insert(const AIndex: NativeInt; const AValue: T); override;

    ///  <summary>Add the elements from a collection to the list.</summary>
    ///  <param name="ACollection">The values to add.</param>
    ///  <param name="AIndex">The index where to insert the element.</param>
    ///  <remarks>This method always raises an exception because inserting is not allowed on sorted collections.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">Always raised in this implementation.</exception>
    procedure InsertAll(const AIndex: NativeInt; const ACollection: IEnumerable<T>); override;

    ///  <summary>Adds an element to the list.</summary>
    ///  <param name="AValue">The value to add.</param>
    ///  <remarks>The added value is not appended. The list tries to figure out where to insert it to keep its elements
    ///  ordered at all times.</remarks>
    procedure Add(const AValue: T); override;

    ///  <summary>Add the elements from a collection to the list.</summary>
    ///  <param name="ACollection">The values to add.</param>
    ///  <remarks>The added values are not appended. The list tries to figure out where to insert the new values
    ///  to keep its elements ordered at all times.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    procedure AddAll(const ACollection: IEnumerable<T>); override;

    ///  <summary>Searches for the first appearance of a given element in this list.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <returns><c>-1</c> if the value was not found; otherwise a positive value indicating the index of the value.</returns>
    ///  <remarks>This method uses binary search because the list is always sorted.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException">Parameter combination is incorrect.</exception>
    function IndexOf(const AValue: T): NativeInt; override;

    ///  <summary>Searches for the last appearance of a given element in this list.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <returns><c>-1</c> if the value was not found; otherwise a positive value indicating the index of the value.</returns>
    ///  <remarks>This method uses binary search because the list is always sorted.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException">Parameter combination is incorrect.</exception>
    function LastIndexOf(const AValue: T): NativeInt; override;

    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the list considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    function Max(): T; override;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the list considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    function Min(): T; override;
  end;

  ///  <summary>The generic <c>sorted list</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses an internal array to store its objects.</remarks>
  TObjectSortedList<T: class> = class(TSortedList<T>)
  private
    FOwnsObjects: Boolean;

  protected
    ///  <summary>Frees the object that was removed from the collection.</summary>
    ///  <param name="AElement">The object that was removed from the collection.</param>
    procedure HandleElementRemoved(const AElement: T); override;

  public
    ///  <summary>Specifies whether this list owns the objects stored in it.</summary>
    ///  <returns><c>True</c> if the list owns its objects; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the list controls the life-time of the stored objects.</remarks>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

type
  ///  <summary>The generic <c>linked list</c> collection.</summary>
  ///  <remarks>This type uses a linked list to store its values.</remarks>
  TLinkedList<T> = class(TAbstractLinkedList<T>)
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
    FCanCache: Boolean;
    FFirst, FLast, FFirstFree: PEntry;
    FCount, FFreeCount: NativeInt;

    { Caching }
    function EntryAt(const AIndex: NativeInt; const AThrow: Boolean = True): PEntry;
    function NeedEntry(const AValue: T): PEntry;
    procedure ReleaseEntry(const AEntry: PEntry);
  protected
    ///  <summary>Sets the item at a given index.</summary>
    ///  <param name="AIndex">The index in the list.</param>
    ///  <param name="AValue">The new value.</param>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    ///  <remarks>This is a very slow operation and should be avoided.</remarks>
    procedure SetItem(const AIndex: NativeInt; const AValue: T); override;

    ///  <summary>Returns the number of elements in the list.</summary>
    ///  <returns>A positive value specifying the number of elements in the list.</returns>
    function GetCount(): NativeInt; override;

    ///  <summary>Extracts the element from the specified index.</summary>
    ///  <param name="AIndex">The index to stract from.</param>
    ///  <param name="AValue">The value stored at that index.</param>
    ///  <returns><c>True</c> if the value was found and stored in <paramref name="AValue"/> parameter; <c>False</c> otherwise.</returns>
    function TryExtractAt(const AIndex: NativeInt; out AValue: T): Boolean; override;

    ///  <summary>Returns the item at a given index.</summary>
    ///  <param name="AIndex">The index in the list.</param>
    ///  <param name="AValue">The value stored at that index.</param>
    ///  <returns><c>True</c> if the value was found and stored in <paramref name="AValue"/> parameter; <c>False</c> otherwise.</returns>
    function TryGetItemAt(const AIndex: NativeInt; out AValue: T): Boolean; override;
  public
    ///  <summary>Creates a new <c>list</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>list</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the list.</param>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Destroys this instance.</summary>
    ///  <remarks>Do not call this method directly; call <c>Free</c> instead.</remarks>
    destructor Destroy(); override;

    ///  <summary>Clears the contents of the list.</summary>
    procedure Clear(); override;

    ///  <summary>Inserts an element into the list.</summary>
    ///  <param name="AIndex">The index to insert to.</param>
    ///  <param name="AValue">The value to insert.</param>
    ///  <remarks>All elements starting with <paramref name="AIndex"/> are moved to the right by one and then
    ///  <paramref name="AValue"/> is placed at position <paramref name="AIndex"/>.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    procedure Insert(const AIndex: NativeInt; const AValue: T); override;

    ///  <summary>Inserts the elements of a collection into the list.</summary>
    ///  <param name="AIndex">The index to insert to.</param>
    ///  <param name="ACollection">The values to insert.</param>
    ///  <remarks>All elements starting with <paramref name="AIndex"/> are moved to the right by the length of
    ///  <paramref name="ACollection"/> and then <paramref name="AValue"/> is placed at position <paramref name="AIndex"/>.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    procedure InsertAll(const AIndex: NativeInt; const ACollection: IEnumerable<T>); override;

    ///  <summary>Removes a given value from the list.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>If the list does not contain the given value, nothing happens.</remarks>
    procedure Remove(const AValue: T); override;

    ///  <summary>Searches for the first appearance of a given element in this list.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <returns><c>-1</c> if the value was not found; otherwise a positive value indicating the index of the value.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException">Parameter combination is incorrect.</exception>
    function IndexOf(const AValue: T): NativeInt; override;

    ///  <summary>Searches for the last appearance of a given element in this list.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <returns><c>-1</c> if the value was not found; otherwise a positive value indicating the index of the value.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException">Parameter combination is incorrect.</exception>
    function LastIndexOf(const AValue: T): NativeInt; override;

    ///  <summary>Specifies the number of elements in the list.</summary>
    ///  <returns>A positive value specifying the number of elements in the list.</returns>
    property Count: NativeInt read FCount;

    ///  <summary>Returns a new enumerator object used to enumerate this list.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the list.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<T>; override;

    ///  <summary>Copies the values stored in the list to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the list.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the list.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Checks whether the list is empty.</summary>
    ///  <returns><c>True</c> if the list is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the list is empty.</remarks>
    function Empty(): Boolean; override;

    ///  <summary>Returns the first element.</summary>
    ///  <returns>The first element in the list.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    function First(): T; override;

    ///  <summary>Returns the first element or a default, if the list is empty.</summary>
    ///  <param name="ADefault">The default value returned if the list is empty.</param>
    ///  <returns>The first element in the list if the list is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function FirstOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the last element.</summary>
    ///  <returns>The last element in the list.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    function Last(): T; override;

    ///  <summary>Returns the last element or a default, if the list is empty.</summary>
    ///  <param name="ADefault">The default value returned if the list is empty.</param>
    ///  <returns>The last element in the list if the list is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function LastOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the single element stored in the list.</summary>
    ///  <returns>The element in the list.</returns>
    ///  <remarks>This method checks if the list contains just one element, in which case it is returned.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionNotOneException">There is more than one element in the list.</exception>
    function Single(): T; override;

    ///  <summary>Returns the single element stored in the list, or a default value.</summary>
    ///  <param name="ADefault">The default value returned if there are less or more elements in the list.</param>
    ///  <returns>The element in the list if the condition is satisfied; <paramref name="ADefault"/> is returned otherwise.</returns>
    ///  <remarks>This method checks if the list contains just one element, in which case it is returned. Otherwise
    ///  the value in <paramref name="ADefault"/> is returned.</remarks>
    function SingleOrDefault(const ADefault: T): T; override;
  end;

  ///  <summary>The generic <c>linked list</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a linked list to store its objects.</remarks>
  TObjectLinkedList<T: class> = class(TLinkedList<T>)
  private
    FOwnsObjects: Boolean;

  protected
    ///  <summary>Frees the object that was removed from the collection.</summary>
    ///  <param name="AElement">The object that was removed from the collection.</param>
    procedure HandleElementRemoved(const AElement: T); override;

    ///  <summary>Replaces a given object with a new one.</summary>
    ///  <param name="ACurrent">The object to be replaced.</param>
    ///  <param name="ANew">The object to be replaced with.</param>
    ///  <remarks>This method will check the objects by reference and free the current one, if needed.</remarks>
    procedure ReplaceItem(var ACurrent: T; const ANew: T); override;
  public
    ///  <summary>Specifies whether this list owns the objects stored in it.</summary>
    ///  <returns><c>True</c> if the list owns its objects; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the list controls the life-time of the stored objects.</remarks>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

type
  ///  <summary>The generic <c>sorted linked list</c> collection.</summary>
  ///  <remarks>This type uses a linked list to store its values.</remarks>
  TSortedLinkedList<T> = class(TLinkedList<T>)
  private
    FAscending: Boolean;

  protected
    ///  <summary>Sets the item at a given index.</summary>
    ///  <param name="AIndex">The index in the list.</param>
    ///  <param name="AValue">The new value.</param>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">Always raised in this implementation.</exception>
    procedure SetItem(const AIndex: NativeInt; const AValue: T); override;
  public
    ///  <summary>Creates a new <c>list</c> collection.</summary>
    ///  <remarks>This constructor requests the default rule set. Call the overloaded constructor if
    ///  specific a set of rules need to be passed. The elements are stored in ascending order.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new <c>list</c> collection.</summary>
    ///  <param name="ARules">A rule set describing the elements in the list.</param>
    ///  <remarks>The elements are stored in ascending order.</remarks>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Creates a new <c>list</c> collection.</summary>
    ///  <param name="AAscending">Pass in a value of <c>True</c> if the elements should be kept in ascending order.
    ///  Pass in <c>False</c> for descending order.</param>
    ///  <param name="ARules">A rule set describing the elements in the list.</param>
    constructor Create(const ARules: TRules<T>; const AAscending: Boolean); overload;

    ///  <summary>Adds an element to the list.</summary>
    ///  <param name="AValue">The value to add.</param>
    ///  <param name="AIndex">The index where to insert the element.</param>
    ///  <remarks>This method always raises an exception because inserting is not allowed on sorted collections.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">Always raised in this implementation.</exception>
    procedure Insert(const AIndex: NativeInt; const AValue: T); override;

    ///  <summary>Add the elements from a collection to the list.</summary>
    ///  <param name="ACollection">The values to add.</param>
    ///  <param name="AIndex">The index where to insert the element.</param>
    ///  <remarks>This method always raises an exception because inserting is not allowed on sorted collections.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">Always raised in this implementation.</exception>
    procedure InsertAll(const AIndex: NativeInt; const ACollection: IEnumerable<T>); override;

    ///  <summary>Adds an element to the list.</summary>
    ///  <param name="AValue">The value to add.</param>
    ///  <remarks>The added value is not appended. The list tries to figure out where to insert it to keep its elements
    ///  ordered at all times.</remarks>
    procedure Add(const AValue: T); override;

    ///  <summary>Add the elements from a collection to the list.</summary>
    ///  <param name="ACollection">The values to add.</param>
    ///  <remarks>The added values are not appended. The list tries to figure out where to insert the new values
    ///  to keep its elements ordered at all times.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    procedure AddAll(const ACollection: IEnumerable<T>); override;

    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the list considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    function Max(): T; override;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the list considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The list is empty.</exception>
    function Min(): T; override;
  end;

  ///  <summary>The generic <c>sorted linked list</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a linked list to store its objects.</remarks>
  TObjectSortedLinkedList<T: class> = class(TSortedLinkedList<T>)
  private
    FOwnsObjects: Boolean;

  protected
    ///  <summary>Frees the object that was removed from the collection.</summary>
    ///  <param name="AElement">The object that was removed from the collection.</param>
    procedure HandleElementRemoved(const AElement: T); override;

  public
    ///  <summary>Specifies whether this list owns the objects stored in it.</summary>
    ///  <returns><c>True</c> if the list owns its objects; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the list controls the life-time of the stored objects.</remarks>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

implementation

{ TAbstractList<T> }

procedure TAbstractList<T>.AddAll(const ACollection: IEnumerable<T>);
begin
  InsertAll(GetCount(), ACollection);
end;

procedure TAbstractList<T>.Add(const AValue: T);
begin
  Insert(GetCount(), AValue);
end;

function TAbstractList<T>.Contains(const AValue: T): Boolean;
begin
  Result := IndexOf(AValue) >= 0;
end;

constructor TAbstractList<T>.Create(const ARules: TRules<T>);
begin
  inherited Create(ARules);
end;

function TAbstractList<T>.ElementAt(const AIndex: NativeInt): T;
begin
  if not TryGetItemAt(AIndex, Result) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');
end;

function TAbstractList<T>.ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T;
begin
  if AIndex < 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');

  if not TryGetItemAt(AIndex, Result) then
    Result := ADefault
end;

function TAbstractList<T>.ExtractAt(const AIndex: NativeInt): T;
begin
  if not TryExtractAt(AIndex, Result) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');
end;

function TAbstractList<T>.GetItem(const AIndex: NativeInt): T;
begin
  if not TryGetItemAt(AIndex, Result) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');
end;

function TAbstractList<T>.IndexOf(const AValue: T): NativeInt;
var
  LEnumerator: IEnumerator<T>;
begin
  LEnumerator := GetEnumerator();
  Result := 0;

  while LEnumerator.MoveNext() do
  begin
    if ElementsAreEqual(AValue, LEnumerator.Current) then Exit;
    Inc(Result);
  end;

  Result := -1;
end;

procedure TAbstractList<T>.InsertAll(const AIndex: NativeInt; const ACollection: IEnumerable<T>);
var
  LIn: T;
  LIndex: NativeInt;
begin
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  LIndex := AIndex;
  for LIn in ACollection do
  begin
    Insert(LIndex, LIn);
    Inc(LIndex);
  end;
end;

procedure TAbstractList<T>.Insert(const AIndex: NativeInt; const AValue: T);
begin
  ExceptionHelper.Throw_OperationNotSupported('Insert');
end;

function TAbstractList<T>.LastIndexOf(const AValue: T): NativeInt;
begin
  ExceptionHelper.Throw_OperationNotSupported('LastIndexOf');
end;

procedure TAbstractList<T>.Remove(const AValue: T);
var
  LIndex: NativeInt;
  LDummy: T;
begin
  LIndex := IndexOf(AValue);
  if LIndex >= 0 then
    TryExtractAt(LIndex, LDummy);
end;

procedure TAbstractList<T>.RemoveAt(const AIndex: NativeInt);
var
  LValue: T;
begin
  if not TryExtractAt(AIndex, LValue) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex')
  else
    NotifyElementRemoved(LValue);
end;

procedure TAbstractList<T>.ReplaceItem(var ACurrent: T; const ANew: T);
begin
  if not ElementsAreEqual(ACurrent, ANew) then
  begin
    { Notify that an element is removed. }
    NotifyElementRemoved(ACurrent);

    { Replace it. }
    ACurrent := ANew;
  end;
end;

procedure TAbstractList<T>.SetItem(const AIndex: NativeInt; const AValue: T);
begin
  ExceptionHelper.Throw_OperationNotSupported('SetItem');
end;

function TAbstractList<T>.TryExtractAt(const AIndex: NativeInt; out AValue: T): Boolean;
begin
  ExceptionHelper.Throw_OperationNotSupported('TryExtractAt');
end;

function TAbstractList<T>.TryGetItemAt(const AIndex: NativeInt; out AValue: T): Boolean;
var
  LEnumerator: IEnumerator<T>;
  LIndex: NativeInt;
begin
  Result := False;

  if (AIndex >= 0) and (AIndex < GetCount()) then
  begin
    LEnumerator := GetEnumerator();
    LIndex := 0;
    while LEnumerator.MoveNext() do
    begin
      if LIndex = AIndex then
      begin
        AValue := LEnumerator.Current;
        Exit(True);
      end;

      Inc(LIndex);
    end;
  end;
end;

{ TAbstractLinkedList<T> }

procedure TAbstractLinkedList<T>.AddFirst(const AValue: T);
begin
  Insert(0, AValue);
end;

procedure TAbstractLinkedList<T>.AddAllFirst(const ACollection: IEnumerable<T>);
begin
  InsertAll(0, ACollection);
end;

procedure TAbstractLinkedList<T>.AddLast(const AValue: T);
begin
  Insert(GetCount(), AValue);
end;

procedure TAbstractLinkedList<T>.AddAllLast(const ACollection: IEnumerable<T>);
begin
  InsertAll(GetCount(), ACollection);
end;

function TAbstractLinkedList<T>.ExtractFirst: T;
begin
  if GetCount() = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := ExtractAt(0);
end;

function TAbstractLinkedList<T>.ExtractLast: T;
begin
  if GetCount() = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := ExtractAt(GetCount() - 1);
end;

procedure TAbstractLinkedList<T>.RemoveFirst;
begin
  if GetCount() = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  RemoveAt(0);
end;

procedure TAbstractLinkedList<T>.RemoveLast;
begin
  if GetCount() = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  RemoveAt(GetCount() - 1);
end;

{ TList<T> }

function TList<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
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
    Result := AAggregator(Result, FArray[I]);
end;

function TList<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
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
    Result := AAggregator(Result, FArray[I]);
end;

function TList<T>.All(const APredicate: TPredicate<T>): Boolean;
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

function TList<T>.Any(const APredicate: TPredicate<T>): Boolean;
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

procedure TList<T>.Clear;
var
  I: NativeInt;
begin
  { Should clean up each element individually }
  if FLength > 0 then
  begin
    for I := 0 to FLength - 1 do
      NotifyElementRemoved(FArray[I]);

    NotifyCollectionChanged();
  end;

  { Reset the length }
  FLength := 0;
end;

procedure TList<T>.CopyTo(var AArray: array of T; const AStartIndex: NativeInt);
var
  I: NativeInt;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Copy all elements safely }
  for I := 0 to FLength - 1 do
    AArray[AStartIndex + I] := FArray[I];
end;

constructor TList<T>.Create(const ARules: TRules<T>);
begin
  Create(ARules, CDefaultSize);
end;

constructor TList<T>.Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt);
begin
  inherited Create(ARules);

  if AInitialCapacity <= 0 then
    SetLength(FArray, 0)
  else
    SetLength(FArray, AInitialCapacity);
end;

constructor TList<T>.Create;
begin
  Create(TRules<T>.Default, CDefaultSize);
end;

procedure TList<T>.Deserialize(const AContext: TInputContext);
var
  L64Length: Int64;
begin
  AContext.GetValue('Length', L64Length); FLength := L64Length;
  AContext.GetValue<TArray<T>>('Array', FArray);
end;

function TList<T>.Empty: Boolean;
begin
  Result := (FLength = 0);
end;

function TList<T>.EqualsTo(const ACollection: IEnumerable<T>): Boolean;
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

function TList<T>.First: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[0];
end;

function TList<T>.FirstOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else
    Result := FArray[0];
end;

function TList<T>.GetCapacity: NativeInt;
begin
  Result := Length(FArray);
end;

function TList<T>.GetCount: NativeInt;
begin
  Result := FLength;
end;

function TList<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create an enumerator }
  Result := TEnumerator.Create(Self);
end;

procedure TList<T>.Grow;
begin
  { Grow the array }
  if FLength < CDefaultSize then
     SetLength(FArray, FLength + CDefaultSize)
  else
     SetLength(FArray, FLength * 2);
end;

function TList<T>.IndexOf(const AValue: T): NativeInt;
begin
  for Result := 0 to FLength - 1 do
    if ElementsAreEqual(FArray[Result], AValue) then
      Exit;

  Result := -1;
end;

procedure TList<T>.Insert(const AIndex: NativeInt; const AValue: T);
var
  I: NativeInt;
begin
  if (AIndex > FLength) or (AIndex < 0) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');

  if FLength = Length(FArray) then
    Grow();

  { Move the array to the right }
  if AIndex < FLength then
    for I := FLength downto (AIndex + 1) do
      FArray[I] := FArray[I - 1];

  Inc(FLength);

  { Put the element into the new position }
  FArray[AIndex] := AValue;
  NotifyCollectionChanged();
end;

procedure TList<T>.InsertAll(const AIndex: NativeInt; const ACollection: IEnumerable<T>);
var
  LValue: T;
  LEnumArray: TArray<T>;
  LEnumLen: NativeInt;
  I: NativeInt;
begin
  if (AIndex > FLength) or (AIndex < 0) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');

  if not Assigned(ACollection) then
     ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Create a temporary list with teh given elements }
  LEnumLen := 0;
  for LValue in ACollection do
  begin
    if Length(LEnumArray) = LEnumLen then
      SetLength(LEnumArray, (Length(LEnumArray) + 1) * 2);

    LEnumArray[LEnumLen] := LValue;
    Inc(LEnumLen);
  end;

  { Check for free space and extend the array to support it if necessary }
  if (Length(FArray) - FLength) < LEnumLen then
    SetLength(FArray, LEnumLen + FLength);

  { Move the contents of the list to the right }
  if AIndex < FLength then
    for I := (FLength - 1) downto AIndex do
      FArray[LEnumLen + I] := FArray[I];

  { Copy the contents in }
  for I := 0 to LEnumLen - 1 do
    FArray[AIndex + I] := LEnumArray[I];

  { Update internals }
  Inc(FLength, LEnumLen);
  NotifyCollectionChanged();
end;

function TList<T>.Last: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FArray[FLength - 1];
end;

function TList<T>.LastOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else
    Result := FArray[FLength - 1];
end;

function TList<T>.Max: T;
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

function TList<T>.Min: T;
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

procedure TList<T>.QuickSort(ALeft, ARight: NativeInt; const AAscending: Boolean);
begin
  if AAscending then               { Ascending sort }
    QuickSort(ALeft, ARight,
      function(const ALeft, ARight: T): Integer
      begin
        Exit(CompareElements(ALeft, ARight));
      end
    ) else                        { Descending sort }
    QuickSort(ALeft, ARight,
      function(const ALeft, ARight: T): Integer
      begin
        Exit(-CompareElements(ALeft, ARight));
      end
    )
end;

procedure TList<T>.QuickSort(ALeft, ARight: NativeInt; const ASortProc: TComparison<T>);
{$IFNDEF OPTIMIZED_SORT}
var
  I, J: NativeInt;
  LPivot, LTemp: T;
begin
  ASSERT(Assigned(ASortProc));

  repeat
    I := ALeft;
    J := ARight;

    LPivot := FArray[(ALeft + ARight) div 2];

    repeat
      while ASortProc(FArray[I], LPivot) < 0 do
        Inc(I);

      while ASortProc(FArray[J], LPivot) > 0 do
        Dec(J);

      if I <= J then
      begin

        if I <> J then
        begin
          LTemp := FArray[I];
          FArray[I] := FArray[J];
          FArray[J] := LTemp;
        end;

        Inc(I);
        Dec(J);
      end;

    until I > J;

    if ALeft < J then
      QuickSort(FArray, ALeft, J, ASortProc);

    ALeft := I;

  until I >= ARight;
end;
{$ELSE}
var
  LSubArray, LSubLeft, LSubRight: NativeInt;
  LPivot, LTemp: T;
  LStack: TQuickSortStack;
begin
  ASSERT(Assigned(ASortProc));

  LSubArray := 0;

  LStack[LSubArray].First := ALeft;
  LStack[LSubArray].Last := ARight;

  repeat
    ALeft  := LStack[LSubArray].First;
    ARight := LStack[LSubArray].Last;
    Dec(LSubArray);
    repeat
      LSubLeft := ALeft;
      LSubRight := ARight;
      LPivot:= FArray[(ALeft + ARight) shr 1];

      repeat
        while ASortProc(FArray[LSubLeft], LPivot) < 0 do
          Inc(LSubLeft);

        while ASortProc(FArray[LSubRight], LPivot) > 0 do
          Dec(LSubRight);

        if LSubLeft <= LSubRight then
        begin
          LTemp := FArray[LSubLeft];
          FArray[LSubLeft] := FArray[LSubRight];
          FArray[LSubRight] := LTemp;
          Inc(LSubLeft);
          Dec(LSubRight);
        end;
      until LSubLeft > LSubRight;

      if LSubLeft < ARight then
      begin
        Inc(LSubArray);
        LStack[LSubArray].First := LSubLeft;
        LStack[LSubArray].Last  := ARight;
      end;

      ARight := LSubRight;
    until ALeft >= ARight;
  until LSubArray < 0;
end;
{$ENDIF}

function TList<T>.LastIndexOf(const AValue: T): NativeInt;
begin
  for Result := FLength - 1 downto 0 do
    if ElementsAreEqual(FArray[Result], AValue) then
      Exit;

  Result := -1;
end;

procedure TList<T>.Reverse(const AStartIndex, ACount: NativeInt);
var
  I: NativeInt;
  LValue: T;
begin
  { Check for indexes }
  if AStartIndex < 0 then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if ACount < 0 then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  if ((AStartIndex + ACount) > FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex/ACount');

  if ACount < 2 then
    Exit;

  { Reverse the array }
  for I := 0 to (ACount div 2) - 1 do
  begin
    LValue := FArray[AStartIndex + I];
    FArray[AStartIndex + I] := FArray[AStartIndex + ACount - I - 1];
    FArray[AStartIndex + ACount - I - 1] := LValue;
  end;
end;

procedure TList<T>.Reverse(const AStartIndex: NativeInt);
begin
  { Call the complete method }
  Reverse(AStartIndex, FLength - AStartIndex);
end;

procedure TList<T>.Reverse;
begin
  { Call the complete method }
  Reverse(0, FLength);
end;

procedure TList<T>.Sort(const AStartIndex, ACount: NativeInt; const AAscending: Boolean);
begin
  { Check for indexes }
  if AStartIndex < 0 then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if ACount < 0 then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  if ((AStartIndex + ACount) > FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex/ACount');

  if ACount > 0 then
    QuickSort(AStartIndex, (AStartIndex + ACount) - 1, AAscending);
end;

procedure TList<T>.Sort(const AStartIndex: NativeInt; const AAscending: Boolean);
begin
  { Call the better method }
  Sort(AStartIndex, FLength, AAscending);
end;

procedure TList<T>.Serialize(const AContext: TOutputContext);
begin
  AContext.AddValue('Length', Int64(FLength));
  AContext.AddValue('Array', FArray);
end;

procedure TList<T>.SetItem(const AIndex: NativeInt; const AValue: T);
begin
  { Check range }
  if (AIndex >= FLength) or (AIndex < 0) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');

  { Delegate }
  ReplaceItem(FArray[AIndex], AValue);

  { Increment version }
  NotifyCollectionChanged();
end;

procedure TList<T>.Shrink;
begin
  { Cut the capacity, if required }
  if FLength < Capacity then
    SetLength(FArray, FLength);
end;

function TList<T>.Single: T;
begin
  { Check length }
  if FLength = 0 then
    ExceptionHelper.Throw_CollectionEmptyError()
  else if FLength > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FArray[0];
end;

function TList<T>.SingleOrDefault(const ADefault: T): T;
begin
  { Check length }
  if FLength = 0 then
    Result := ADefault
  else if FLength > 1 then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FArray[0];
end;

procedure TList<T>.Sort(const AStartIndex, ACount: NativeInt; const ASortProc: TComparison<T>);
begin
  { Check for indexes }
  if AStartIndex < 0 then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if ACount < 0 then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  if ((AStartIndex + ACount) > FLength) then
     ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex/ACount');

  if ACount > 0 then
    QuickSort(AStartIndex, (AStartIndex + ACount) - 1, ASortProc);
end;

procedure TList<T>.Sort(const AStartIndex: NativeInt; const ASortProc: TComparison<T>);
begin
  { Call the better method }
  Sort(AStartIndex, FLength, ASortProc);
end;

procedure TList<T>.Sort(const ASortProc: TComparison<T>);
begin
  if not Assigned(ASortProc) then
    ExceptionHelper.Throw_ArgumentNilError('ASortProc');

  { Call the better method }
  Sort(0, FLength, ASortProc);
end;

function TList<T>.TryExtractAt(const AIndex: NativeInt; out AValue: T): Boolean;
var
  I: NativeInt;
begin
  Result := False;
  if (AIndex >= FLength) or (AIndex < 0) then
     Exit;

  { Return the element at that position. }
  AValue := FArray[AIndex];

  { Move the list }
  if FLength > 1 then
    for I := AIndex to FLength - 2 do
      FArray[I] := FArray[I + 1];

  Result := True;
  Dec(FLength);
  NotifyCollectionChanged();
end;

function TList<T>.TryGetItemAt(const AIndex: NativeInt; out AValue: T): Boolean;
begin
  if (AIndex >= 0) and (AIndex < FLength) then
  begin
    AValue := FArray[AIndex];
    Result := True;
  end else
    Result := False;
end;

procedure TList<T>.Sort(const AAscending: Boolean);
begin
  { Call the better method }
  Sort(0, FLength, AAscending);
end;

{ TList<T>.TEnumerator }

function TList<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  with TList<T>(Owner) do
  begin
    Result := FCurrentIndex < FLength;

    if Result then
    begin
      ACurrent := FArray[FCurrentIndex];
      Inc(FCurrentIndex);
    end;
  end;
end;

{ TObjectList<T> }

procedure TObjectList<T>.HandleElementRemoved(const AElement: T);
begin
  if FOwnsObjects then
    TObject(AElement).Free;
end;

procedure TObjectList<T>.ReplaceItem(var ACurrent: T; const ANew: T);
begin
  { Only act if owns objects is set. Otherwise fallback to default. }
  if (FOwnsObjects) and (TObject(ACurrent) <> TObject(ANew)) then
  begin
    NotifyElementRemoved(ACurrent);
    ACurrent := ANew;
  end else
    inherited;
end;

{ TSortedList<T> }

procedure TSortedList<T>.InternalInsert(const AIndex: NativeInt; const AValue: T);
var
  I: NativeInt;
begin
  ASSERT(AIndex <= FLength);
  ASSERT(AIndex >= 0);

  if FLength = Length(FArray) then
    Grow();

  { Move the array to the right }
  if AIndex < FLength then
    for I := FLength downto (AIndex + 1) do
      FArray[I] := FArray[I - 1];

  Inc(FLength);

  { Put the element into the new position }
  FArray[AIndex] := AValue;
  NotifyCollectionChanged();
end;

procedure TSortedList<T>.AddAll(const ACollection: IEnumerable<T>);
var
  LValue: T;
begin
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Enumerate and add, preserving order}
  for LValue in ACollection do
    Add(LValue);
end;

procedure TSortedList<T>.Add(const AValue: T);
var
  LLeft, LRight, LMiddle: NativeInt;
  LCompareResult, LSignFix: NativeInt;
begin
  { Case 1, empty list, optimize }
  if FLength = 0 then
    InternalInsert(0, AValue)
  else
  begin
    { Sign fix }
    if FAscending then
      LSignFix := 1
    else
      LSignFix := -1;

    { Check for valid type support }
    LLeft := 0;
    LRight := LLeft + FLength - 1;

    while (LLeft <= LRight) do
    begin
      LMiddle := (LLeft + LRight) div 2;
      LCompareResult := CompareElements(FArray[LMiddle], AValue) * LSignFix;

      if LCompareResult > 0 then
        LRight := LMiddle - 1
      else if LCompareResult < 0 then
        LLeft := LMiddle + 1
      else
        Break;
    end;

    { LMiddle is located on the approximative spot. Let's see }
    if (LCompareResult = 0) or (LCompareResult > 0) then
      InternalInsert(LMiddle, AValue)
    else
      InternalInsert(LMiddle + 1, AValue);
  end;
end;

constructor TSortedList<T>.Create(const ARules: TRules<T>);
begin
  Create(ARules, CDefaultSize, True);
end;

constructor TSortedList<T>.Create();
begin
  Create(TRules<T>.Default, CDefaultSize, True);
end;

constructor TSortedList<T>.Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt; const AAscending: Boolean);
begin
  inherited Create(ARules, AInitialCapacity);
  FAscending := AAscending;
end;

procedure TSortedList<T>.Insert(const AIndex: NativeInt; const AValue: T);
begin
  ExceptionHelper.Throw_OperationNotSupported('Insert');
end;

procedure TSortedList<T>.InsertAll(const AIndex: NativeInt; const ACollection: IEnumerable<T>);
begin
  ExceptionHelper.Throw_OperationNotSupported('Insert');
end;

function TSortedList<T>.BinarySearch(const AElement: T; const AStartIndex, ACount: NativeInt;
  const AAscending: Boolean): NativeInt;
var
  LLeft, LRight, LMiddle: NativeInt;
  LCompareResult: NativeInt;
begin
  { Do not search for 0 count }
  if ACount = 0 then
  begin
    Result := -1;
    Exit;
  end;

  LLeft := AStartIndex;
  LRight := LLeft + ACount - 1;

  while (LLeft <= LRight) do
  begin
    LMiddle := (LLeft + LRight) div 2;
    LCompareResult := CompareElements(FArray[LMiddle], AElement);

    if not AAscending then
       LCompareResult := LCompareResult * -1;

    if LCompareResult > 0 then
      LRight := LMiddle - 1
    else if LCompareResult < 0 then
      LLeft := LMiddle + 1
    else begin
      Result := LMiddle - AStartIndex;
      Exit;
    end;
  end;

  Result := -1;
end;

function TSortedList<T>.IndexOf(const AValue: T): NativeInt;
var
  I, J: NativeInt;
begin
  { Search for the value }
  J := BinarySearch(AValue, 0, FLength, FAscending);

  if J = -1 then
     Exit(-1);

  for I := J - 1 downto 0 do
    if not ElementsAreEqual(AValue, FArray[I]) then
    begin
      Result := I + 1;
      Exit;
    end;

  Result := J;
end;

function TSortedList<T>.Max: T;
begin
  if FAscending then
    Result := Last()
  else
    Result := First();
end;

function TSortedList<T>.Min: T;
begin
  if FAscending then
    Result := First()
  else
    Result := Last();
end;

procedure TSortedList<T>.SetItem(const AIndex: NativeInt; const AValue: T);
begin
  ExceptionHelper.Throw_OperationNotSupported('SetItem');
end;

function TSortedList<T>.LastIndexOf(const AValue: T): NativeInt;
var
  I, J: NativeInt;
begin
  { Search for the value }
  J := BinarySearch(AValue, 0, FLength, FAscending);

  if J = -1 then
    Exit(-1);

  for I := J + 1 to FLength - 1 do
    if not ElementsAreEqual(AValue, FArray[I]) then
    begin
      Result := I - 1;
      Exit;
    end;

  Result := J;
end;

{ TObjectSortedList<T> }

procedure TObjectSortedList<T>.HandleElementRemoved(const AElement: T);
begin
  if FOwnsObjects then
    TObject(AElement).Free;
end;

{ TLinkedList<T> }

procedure TLinkedList<T>.Clear;
var
  LCurrent, LNext: PEntry;
begin
  if FFirst <> nil then
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

procedure TLinkedList<T>.CopyTo(var AArray: array of T; const AStartIndex: NativeInt);
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

constructor TLinkedList<T>.Create;
begin
  Create(TRules<T>.Default);
end;

constructor TLinkedList<T>.Create(const ARules: TRules<T>);
begin
  inherited Create(ARules);
  FCanCache := True;
end;

destructor TLinkedList<T>.Destroy;
var
  LNext: PEntry;
begin
  FCanCache := False;

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

function TLinkedList<T>.Empty: Boolean;
begin
  Result := not Assigned(FFirst);
end;

function TLinkedList<T>.First: T;
begin
  if not Assigned(FFirst) then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FFirst^.FValue;
end;

function TLinkedList<T>.FirstOrDefault(const ADefault: T): T;
begin
  if not Assigned(FFirst) then
    Result := ADefault
  else
    Result := FFirst^.FValue;
end;

function TLinkedList<T>.GetCount: NativeInt;
begin
  Result := FCount;
end;

function TLinkedList<T>.GetEnumerator: IEnumerator<T>;
var
  LEnumerator: TEnumerator;
begin
  LEnumerator := TEnumerator.Create(Self);
  LEnumerator.FCurrentEntry := FFirst;
  Result := LEnumerator;
end;

function TLinkedList<T>.IndexOf(const AValue: T): NativeInt;
var
  LCurrent: PEntry;
begin
  LCurrent := FFirst;
  Result := 0;
  while Assigned(LCurrent) do
  begin
    { Check elements }
    if ElementsAreEqual(AValue, LCurrent^.FValue) then
      Exit;

    LCurrent := LCurrent^.FNext;
    Inc(Result);
  end;

  Result := -1;
end;

procedure TLinkedList<T>.Insert(const AIndex: NativeInt; const AValue: T);
var
  LCurrent, LNew: PEntry;
begin
  if AIndex = FCount then
    LCurrent := nil
  else
    LCurrent := EntryAt(AIndex);

  { Make our node! Insert it to the list }
  LNew := NeedEntry(AValue);

  if Assigned(LCurrent) then
  begin
    LNew^.FPrev := LCurrent^.FPrev;

    if Assigned(LCurrent^.FPrev) then
      LCurrent^.FPrev^.FNext := LNew;

    LCurrent^.FPrev := LNew;

    if FFirst = LCurrent then
      FFirst := LNew;
  end else
  begin
    LNew^.FPrev := FLast;

    if Assigned(FLast) then
      FLast^.FNext := LNew;

    FLast := LNew;
  end;

  LNew^.FNext := LCurrent;
  if LCurrent = FFirst then
    FFirst := LNew;

  NotifyCollectionChanged();
  Inc(FCount);
end;

procedure TLinkedList<T>.InsertAll(const AIndex: NativeInt; const ACollection: IEnumerable<T>);
var
  LCurrent, LNewFirst, LNewLast, LNew: PEntry;
  LValue: T;
begin
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  if AIndex = FCount then
    LCurrent := nil
  else
    LCurrent := EntryAt(AIndex);

  { Build up the chain from the input collection }
  LNewFirst := nil;
  LNewLast := nil;
  for LValue in ACollection do
  begin
    LNew := NeedEntry(LValue);
    LNew^.FPrev := LNewLast;
    LNew^.FNext := nil;

    if Assigned(LNewLast) then
      LNewLast^.FNext := LNew;

    LNewLast := LNew;

    if not Assigned(LNewFirst) then
      LNewFirst := LNew;

    Inc(FCount);
  end;

  if not Assigned(LNewFirst) then
    Exit;

  { The chain is created! now append it to this list's chain }
  if Assigned(LCurrent) then
  begin
    LNewFirst^.FPrev := LCurrent^.FPrev;

    if Assigned(LCurrent^.FPrev) then
      LCurrent^.FPrev^.FNext := LNewFirst;

    LCurrent^.FPrev := LNewLast;

    if FFirst = LCurrent then
      FFirst := LNewFirst;
  end else
  begin
    LNewFirst^.FPrev := FLast;

    if Assigned(FLast) then
      FLast^.FNext := LNewFirst;

    FLast := LNewLast;
  end;

  LNewLast^.FNext := LCurrent;
  if LCurrent = FFirst then
    FFirst := LNewFirst;

  NotifyCollectionChanged();
end;

function TLinkedList<T>.Last: T;
begin
  if not Assigned(FLast) then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FLast^.FValue;
end;

function TLinkedList<T>.LastIndexOf(const AValue: T): NativeInt;
var
  LCurrent: PEntry;
begin
  LCurrent := FLast;
  Result := FCount - 1;
  while Assigned(LCurrent) do
  begin
    { Check elements }
    if ElementsAreEqual(AValue, LCurrent^.FValue) then
      Exit;

    LCurrent := LCurrent^.FPrev;
    Dec(Result);
  end;

  Result := -1;
end;

function TLinkedList<T>.LastOrDefault(const ADefault: T): T;
begin
  if not Assigned(FLast) then
    Result := ADefault
  else
    Result := FLast^.FValue;
end;

function TLinkedList<T>.NeedEntry(const AValue: T): PEntry;
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

function TLinkedList<T>.EntryAt(const AIndex: NativeInt; const AThrow: Boolean): PEntry;
var
  LIndex: NativeInt;
begin
  if ((AIndex >= FCount)) or (AIndex < 0) then
    if AThrow then
      ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex')
    else
      Exit(nil);

  { Find the position }
  if AIndex = (FCount - 1) then
    Result := FLast
  else if AIndex = 0 then
    Result := FFirst
  else begin
    Result := FFirst;
    LIndex := 0;
    while Assigned(Result) do
    begin
      if LIndex = AIndex then
        Exit;

      Result := Result^.FNext;
      Inc(LIndex);
    end;

    { Should never happen }
    if AThrow then
      ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex')
    else
      Exit(nil);
  end;
end;

procedure TLinkedList<T>.ReleaseEntry(const AEntry: PEntry);
begin
  if (FFreeCount = CDefaultSize) or not FCanCache then
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

procedure TLinkedList<T>.Remove(const AValue: T);
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

procedure TLinkedList<T>.SetItem(const AIndex: NativeInt; const AValue: T);
begin
  { Delegate }
  ReplaceItem(EntryAt(AIndex)^.FValue, AValue);

  { Increment version }
  NotifyCollectionChanged();
end;

function TLinkedList<T>.Single: T;
begin
  { Check length }
  if not Assigned(FFirst) then
    ExceptionHelper.Throw_CollectionEmptyError()
  else if FFirst <> FLast then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FFirst^.FValue;
end;

function TLinkedList<T>.SingleOrDefault(const ADefault: T): T;
begin
  { Check length }
  if not Assigned(FFirst) then
    Result := ADefault
  else if FFirst <> FLast then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement()
  else
    Result := FFirst^.FValue;
end;

function TLinkedList<T>.TryExtractAt(const AIndex: NativeInt; out AValue: T): Boolean;
var
  LCurrent: PEntry;
begin
  Result := False;
  LCurrent := EntryAt(AIndex, False);

  if not Assigned(LCurrent) then
    Exit;

  AValue := LCurrent^.FValue;

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
  Result := True;

  NotifyCollectionChanged();
  Dec(FCount);
end;

function TLinkedList<T>.TryGetItemAt(const AIndex: NativeInt; out AValue: T): Boolean;
var
  LEntry: PEntry;
begin
  LEntry := EntryAt(AIndex, False);

  if Assigned(LEntry) then
  begin
    Result := True;
    AValue := LEntry^.FValue;
  end else
    Result := False;
end;

{ TLinkedList<T>.TEnumerator }

function TLinkedList<T>.TEnumerator.TryMoveNext(out ACurrent: T): Boolean;
begin
  Result := Assigned(FCurrentEntry);
  if Result then
  begin
    ACurrent := FCurrentEntry^.FValue;
    FCurrentEntry := FCurrentEntry^.FNext;
  end;
end;

{ TObjectLinkedList<T> }

procedure TObjectLinkedList<T>.HandleElementRemoved(const AElement: T);
begin
  if FOwnsObjects then
    TObject(AElement).Free;
end;

procedure TObjectLinkedList<T>.ReplaceItem(var ACurrent: T; const ANew: T);
begin
  { Only act if owns objects is set. Otherwise fallback to default. }
  if (FOwnsObjects) and (TObject(ACurrent) <> TObject(ANew)) then
  begin
    NotifyElementRemoved(ACurrent);
    ACurrent := ANew;
  end else
    inherited;
end;

{ TSortedLinkedList<T> }

procedure TSortedLinkedList<T>.Add(const AValue: T);
var
  LCurrent, LNew: PEntry;
  LSign: NativeInt;
begin
  if FAscending then
    LSign := 1
  else
    LSign := -1;

  LCurrent := FFirst;

  while Assigned(LCurrent) do
  begin
    if ((CompareElements(AValue, LCurrent^.FValue) * LSign) < 0) then
       Break;

    LCurrent := LCurrent^.FNext;
  end;

  { Make our node! Insert it to the list }
  LNew := NeedEntry(AValue);

  if Assigned(LCurrent) then
  begin
    LNew^.FPrev := LCurrent^.FPrev;

    if Assigned(LCurrent^.FPrev) then
      LCurrent^.FPrev^.FNext := LNew;

    LCurrent^.FPrev := LNew;

    if FFirst = LCurrent then
      FFirst := LNew;
  end else
  begin
    LNew^.FPrev := FLast;

    if Assigned(FLast) then
      FLast^.FNext := LNew;

    FLast := LNew;
  end;

  LNew^.FNext := LCurrent;
  if LCurrent = FFirst then
    FFirst := LNew;


  NotifyCollectionChanged();
  Inc(FCount);
end;

procedure TSortedLinkedList<T>.AddAll(const ACollection: IEnumerable<T>);
var
  LValue: T;
begin
  { Check input }
  if not Assigned(ACollection) then
     ExceptionHelper.Throw_ArgumentNilError('ACollection');

  for LValue in ACollection do
    Add(LValue);
end;

constructor TSortedLinkedList<T>.Create();
begin
  Create(TRules<T>.Default, True);
end;

constructor TSortedLinkedList<T>.Create(const ARules: TRules<T>);
begin
  Create(ARules, True);
end;

constructor TSortedLinkedList<T>.Create(const ARules: TRules<T>; const AAscending: Boolean);
begin
  inherited Create(ARules);
  FAscending := AAscending;
end;

procedure TSortedLinkedList<T>.Insert(const AIndex: NativeInt; const AValue: T);
begin
  ExceptionHelper.Throw_OperationNotSupported('Insert');
end;

procedure TSortedLinkedList<T>.InsertAll(const AIndex: NativeInt; const ACollection: IEnumerable<T>);
begin
  ExceptionHelper.Throw_OperationNotSupported('Insert');
end;

function TSortedLinkedList<T>.Max: T;
begin
  if not Assigned(FFirst) then
    ExceptionHelper.Throw_CollectionEmptyError();

  if FAscending then
    Result := FLast^.FValue
  else
    Result := FFirst^.FValue;
end;

function TSortedLinkedList<T>.Min: T;
begin
  if not Assigned(FFirst) then
    ExceptionHelper.Throw_CollectionEmptyError();

  if FAscending then
    Result := FFirst^.FValue
  else
    Result := FLast^.FValue;
end;

procedure TSortedLinkedList<T>.SetItem(const AIndex: NativeInt; const AValue: T);
begin
  ExceptionHelper.Throw_OperationNotSupported('SetItem');
end;

{ TObjectSortedLinkedList<T> }

procedure TObjectSortedLinkedList<T>.HandleElementRemoved(const AElement: T);
begin
  if FOwnsObjects then
    TObject(AElement).Free;
end;

end.
