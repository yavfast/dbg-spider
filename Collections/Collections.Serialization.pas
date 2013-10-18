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
unit Collections.Serialization;
interface
uses
  SysUtils,
  TypInfo,
  Classes,
  Rtti,
  Generics.Collections,
  Collections.Base;

type
  ///  <summary>Annotate this attribute on fields that should not be serialized.</summary>
  NonSerialized = class(TCustomAttribute);

  ///  <summary>An abstract base class for all serialization engines.</summary>
  ///  <remarks>Inherit this base class and implement its abstract methods to create a fully functional
  ///  serialization engine capable of serializing most data types provided by Delphi.</remarks>
  TSerializer = class abstract(TInterfacedObject)
  private
    FObjectReg: TDictionary<Pointer, Int32>;
    FDynArrayReg: TDictionary<Pointer, Int32>;
    FRecordReg: TDictionary<Pointer, Int32>;
    FLastId: Int32;
    FSkipErrors: Boolean;
    FRttiContext: TRttiContext;

    procedure ErrorNotSupported(const ATypeInfo: PTypeInfo);
    procedure ErrorNotEnoughRtti(const ATypeInfo: PTypeInfo);
    procedure ErrorNoFieldRtti(const AField: TRttiField);
    procedure SerializeInternal(const AType: TRttiType; const AValueRef: Pointer);
    procedure WriteStaticArray(const ATypeInfo: PTypeInfo; const ARefToFirstElement: Pointer);
    procedure WriteDynamicArray(const ATypeInfo: PTypeInfo; const ADynArray: Pointer);
    procedure WriteRecord(const ATypeInfo: PTypeInfo; const ARefToRecord: Pointer);
    procedure WriteClass(const ATypeInfo: PTypeInfo; const AObject: TObject);
  protected
    ///  <summary>Specifies the RTTI context object used by this serializer to obtain the required
    ///  type information. Can be used in descending classes.</summary>
    ///  <returns>A RTTI context object.</returns>
    property RttiContext: TRttiContext read FRttiContext;

    ///  <summary>Writes a 8-bit signed integer.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteInt8(const AValue: Int8); virtual; abstract;

    ///  <summary>Writes a 8-bit unsigned integer.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteUInt8(const AValue: UInt8); virtual; abstract;

    ///  <summary>Writes a 16-bit signed integer.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteInt16(const AValue: Int16); virtual; abstract;

    ///  <summary>Writes a 16-bit unsigned integer.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteUInt16(const AValue: UInt16); virtual; abstract;

    ///  <summary>Writes a 32-bit signed integer.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteInt32(const AValue: Int32); virtual; abstract;

    ///  <summary>Writes a 32-bit unsigned integer.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteUInt32(const AValue: UInt32); virtual; abstract;

    ///  <summary>Writes a 64-bit signed integer.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteInt64(const AValue: Int64); virtual; abstract;

    ///  <summary>Writes a 64-bit unsigned integer.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteUInt64(const AValue: UInt64); virtual; abstract;

    ///  <summary>Writes a single byte ANSI character.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteAnsiChar(const AValue: AnsiChar); virtual; abstract;

    ///  <summary>Writes a two byte WIDE character.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteWideChar(const AValue: WideChar); virtual; abstract;

    ///  <summary>Writes a single precision floating point value.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteSingle(const AValue: Single); virtual; abstract;

    ///  <summary>Writes a double precision floating point value.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteDouble(const AValue: Double); virtual; abstract;

    ///  <summary>Writes an extended precision floating point value.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteExtended(const AValue: Extended); virtual; abstract;

    ///  <summary>Writes a comp floating point value.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteComp(const AValue: Comp); virtual; abstract;

    ///  <summary>Writes a currency floating point value.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteCurrency(const AValue: Currency); virtual; abstract;

    ///  <summary>Writes a short ANSI string.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteShortString(const AValue: ShortString); virtual; abstract;

    ///  <summary>Writes a long ANSI string.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteAnsiString(const AValue: AnsiString); virtual; abstract;

    ///  <summary>Writes a long WIDE string.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteWideString(const AValue: WideString); virtual; abstract;

    ///  <summary>Writes a long UNICODE string.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteUnicodeString(const AValue: UnicodeString); virtual; abstract;

    ///  <summary>Writes a metaclass. Note that a metaclass might not be resolvable on deserialization.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  facilitate writing of the specified value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteMetaClass(const AValue: TClass); virtual; abstract;

    ///  <summary>Writes a set.</summary>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <param name="ASetSize">The size in bytes of a set value.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteSet(const ASetSize: UInt8; const AValue); virtual; abstract;

    ///  <summary>Writes an enumeration.</summary>
    ///  <param name="AValue">The ordinal value of the enumeration.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteEnum(const AValue: Int64); virtual; abstract;

    ///  <summary>Notifies the serializer that the serialization for the root type is started.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  mark the beggining of the serialization process. This is a convenience method, if the serializer needs not prepare
    ///  itself then a simple empty implementation is enough.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure BeginWriteRoot(); virtual; abstract;

    ///  <summary>Notifies the serializer that the serialization for the root type has ended.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  mark the ending of the serialization process. This is a convenience method, if the serializer needs not prepare
    ///  itself then a simple empty implementation is enough.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure EndWriteRoot(); virtual; abstract;

    ///  <summary>Notifies the serializer that a class or record field is about to be serialized.</summary>
    ///  <param name="AField">The field RTTI information.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  prepare it for an upcoming field value. This method will never be called with a <c>nil</c> value.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure BeginWriteField(const AField: TRttiField); overload; virtual; abstract;

    ///  <summary>Notifies the serializer that a pseudo-field (otherwise known as a "label") is about to be serialized.</summary>
    ///  <param name="ALabel">The name of the pseudo-field.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes in order to
    ///  prepare it for an upcoming field value. The supplied <paramref name="ALabel" /> argument can hold anything inclusing and empty string.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure BeginWriteField(const ALabel: String); overload; virtual; abstract;

    ///  <summary>Notifies the serializer that a field or a label was serialized.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. This method marks the end
    ///  of serialization of a field or label.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure EndWriteField(); virtual; abstract;

    ///  <summary>Notifies the serializer that a record is about to be serialized.</summary>
    ///  <param name="ARecordType">The record RTTI information.</param>
    ///  <param name="AId">An internal ID that uniquily identifies this record.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. The supplied <paramref name="ARecordType"/>
    ///  will never be <c>nil</c>. The inheriting class should store some of the RTTI information along with the generated <paramref name="AId" />,
    ///  which will be required on deserialization.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure BeginWriteRecord(const ARecordType: TRttiRecordType; const AId: Int32); overload; virtual; abstract;

    ///  <summary>Notifies the serializer that a record was serialized.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. This method marks the end
    ///  of serialization of a record. It will only be called if <c>BeginWriteRecord</c> was priorly called.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure EndWriteRecord(); virtual; abstract;

    ///  <summary>Notifies the serializer that a record reference is about to be serialized.</summary>
    ///  <param name="AReference">An internal ID that uniquily identifies the referenced record.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. This method
    ///  is called if the serialier notices that the address of the record is the same with the address of another record that was already serialized
    ///  thus it needs only to store a "pointer" to that record. The supplied <paramref name="AReference" /> is the unique ID of the record that was already serialized
    ///  and has the same address with this one. Note, this method will only be called for "pointer to record" types and not static records.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteRecordReference(const AReference: Int32); virtual; abstract;

    ///  <summary>Notifies the serializer that a <c>nil</c> record reference is about to be serialized.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. This method
    ///  is called if the serialier notices that the address of the record is <c>nil</c> and thus there is nothing to do. Note, this method
    ///  will only be called for "pointer to record" types and not static records.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteNilRecordReference(); virtual; abstract;

    ///  <summary>Notifies the serializer that a class is about to be serialized.</summary>
    ///  <param name="AClassType">The class RTTI information.</param>
    ///  <param name="AType">The metaclass of the class.</param>
    ///  <param name="AId">An internal ID that uniquily identifies this class.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. The supplied <paramref name="AClassType"/>
    ///  will never be <c>nil</c>. The inheriting class should store some of the RTTI information, the meta class along with the generated <paramref name="AId" />,
    ///  which will be required on deserialization. This method is only called if the class was not already serialized.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure BeginWriteClass(const AClassType: TRttiInstanceType; const AType: TClass; const AId: Int32); virtual; abstract;

    ///  <summary>Notifies the serializer that a class was serialized.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. This method marks the end
    ///  of serialization of a class. It will only be called if <c>BeginWriteClass</c> was priorly called.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure EndWriteClass(); virtual; abstract;

    ///  <summary>Notifies the serializer that a class reference is about to be serialized.</summary>
    ///  <param name="AReference">An internal ID that uniquily identifies the referenced class.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. This method
    ///  is called if the serialier notices that the address of the class instance is the same with the address of another class instance
    ///  that was already serialized thus it needs only to store a "pointer" to that class instance. The supplied <paramref name="AReference" />
    ///  is the unique ID of the class instance that was already serialized and has the same address with this one.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteClassReference(const AReference: Int32); virtual; abstract;

    ///  <summary>Notifies the serializer that a <c>nil</c> class instance is about to be serialized.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. This method
    ///  is called if the serialier notices that the address of the class instance is <c>nil</c> and thus there is nothing to do. It is also impossible to
    ///  obtain the metaclass from a <c>nil</c> instance.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteNilClassReference(); virtual; abstract;

    ///  <summary>Notifies the serializer that a static array is about to be serialized.</summary>
    ///  <param name="AArrayType">The array RTTI information.</param>
    ///  <param name="ANumberOfElements">The number of elements that the array contains.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. This method must store some RTTI information along
    ///  with the number of elements. This information will be used later on deserialization time.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure BeginWriteStaticArray(const AArrayType: TRttiArrayType; const ANumberOfElements: NativeInt); virtual; abstract;

    ///  <summary>Notifies the serializer that a static array was serialized.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. This method marks the end
    ///  of serialization of a static array. It will only be called if <c>BeginWriteStaticArray</c> was priorly called.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure EndWriteStaticArray(); virtual; abstract;

    ///  <summary>Notifies the serializer that a dynamic array is about to be serialized.</summary>
    ///  <param name="AArrayType">The array RTTI information.</param>
    ///  <param name="ANumberOfElements">The number of elements to serialize. Can never be a value of zero.</param>
    ///  <param name="AId">An internal ID that uniquily identifies this array.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. The supplied <paramref name="AArrayType"/>
    ///  will never be <c>nil</c>. The inheriting class should store some of the RTTI information, the number of elements along with the generated <paramref name="AId" />,
    ///  which will be required on deserialization. This method is only called if the dynamic array was not already serialized.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure BeginWriteDynamicArray(const AArrayType: TRttiDynamicArrayType; const ANumberOfElements: NativeInt; const AId: Int32); virtual; abstract;

    ///  <summary>Notifies the serializer that a dynamic array was serialized.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. This method marks the end
    ///  of serialization of a dynamic array. It will only be called if <c>BeginWriteDynamicArray</c> was priorly called.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure EndWriteDynamicArray(); virtual; abstract;

    ///  <summary>Notifies the serializer that a dynamic array reference is about to be serialized.</summary>
    ///  <param name="AReference">An internal ID that uniquily identifies the referenced array.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. This method
    ///  is called if the serialier notices that the address of the array is the same with the address of another array
    ///  that was already serialized thus it needs only to store a "pointer" to that array. The supplied <paramref name="AReference" />
    ///  is the unique ID of the array that was already serialized and has the same address with this one.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteDynamicArrayReference(const AReference: Int32); virtual; abstract;

    ///  <summary>Notifies the serializer that a <c>nil</c> dynamic array is about to be serialized.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. This method
    ///  is called if the serialier notices that the address of the dynamic array is <c>nil</c> and thus there is nothing to do.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure WriteNilDynamicArrayReference(); virtual; abstract;
  public
    ///  <summary>Creates a new serializer instance.</summary>
    ///  <remarks>When descending from this class this constructor must be called. If not called, some internal
    ///  data structures will remain un-initialized.</remarks>
    constructor Create();

    ///  <summary>Destroys this serializer instance.</summary>
    ///  <remarks>Never forget to call this destructor in descendant classes, otherwise memory leaks will occur.</remarks>
    destructor Destroy(); override;

    ///  <summary>Serializes an object.</summary>
    ///  <param name="AObject">The object that needs to be serialized. Can be <c>nil</c>.</param>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure Serialize(const AObject: TObject); overload;

    ///  <summary>Serializes an value.</summary>
    ///  <param name="ATypeInfo">The value's type information.</param>
    ///  <param name="AValue">The value that needs to be serialized.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ATypeInfo"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure Serialize(const ATypeInfo: PTypeInfo; const AValue); overload;

    ///  <summary>Serializes an generic value.</summary>
    ///  <param name="AValue">The value that needs to be serialized.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AValue"/> does not have RTTI.</exception>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure Serialize<T>(const AValue: T); overload;

    ///  <summary>Specifies whether the serializer throws exceptions for values that cannot be serialized.</summary>
    ///  <returns><c>True</c> if serializer skips problematic fields; <c>False</c> if the serializer raises an exception on
    ///  problematic fields.</returns>
    property SkipErrors: Boolean read FSkipErrors write FSkipErrors;

    ///  <summary>Creates a new default serializer.</summary>
    ///  <param name="AStream">The stream into which to serialize.</param>
    ///  <returns>A new default binary serializer.</returns>
    ///  <remarks>Generally, serializers are not reusable for separate contexts. This means that after you serialize a root type
    ///  the serializer needs to be destroyed.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AStream"/> is <c>nil</c>.</exception>
    class function Default(const AStream: TStream): TSerializer; static;
  end;

  ///  <summary>An abstract base class for all deserialization engines.</summary>
  ///  <remarks>Inherit this base class and implement its abstract methods to create a fully functional
  ///  deserialization engine. A serialization engine must also be developed to complement the deserialization engine.</remarks>
  TDeserializer = class abstract(TInterfacedObject)
  protected type
    ///  <summary>Defines the possible ways a referencef type is stored.</summary>
    TReferenceType = (
      ///  <summary>The type was stored completely. All fields or elements were stored.</summary>
      rtInline,
      ///  <summary>The type was not stored, but rather a reference to another "instance" was stored.</summary>
      rtPointer,
      ///  <summary>The type was not stored because it was a <c>nil</c> reference.</summary>
      rtNil
    );

  private
    FObjectReg: TDictionary<Int32, Pointer>;
    FDynArrayReg: TDictionary<Int32, Pointer>;
    FRecordReg: TDictionary<Int32, Pointer>;
    FSkipErrors: Boolean;
    FRttiContext: TRttiContext;

    procedure ErrorNotSupported(const ATypeInfo: PTypeInfo);
    procedure ErrorNotEnoughRtti(const ATypeInfo: PTypeInfo);
    procedure ErrorNoFieldRtti(const AField: TRttiField);
    procedure DeserializeInternal(const AType: TRttiType; const AValueRef: Pointer);
    function CreateInstance(const AClassType: TRttiInstanceType): TObject;
    procedure ReadStaticArray(const ATypeInfo: PTypeInfo; const ARefToFirstElement: Pointer);
    procedure ReadDynamicArray(const ATypeInfo: PTypeInfo; out ADynArray: Pointer);
    procedure ReadRecord(const ATypeInfo: PTypeInfo; var ARefToRecord: Pointer);
    procedure ReadClass(const ATypeInfo: PTypeInfo; out AObject: TObject);
  protected
    ///  <summary>Specifies the RTTI context object used by this deserializer to obtain the required
    ///  type information. Can be used in descending classes.</summary>
    ///  <returns>A RTTI context object.</returns>
    property RttiContext: TRttiContext read FRttiContext;

    ///  <summary>Reads a 8-bit signed integer.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadInt8(out AValue: Int8); virtual; abstract;

    ///  <summary>Reads a 8-bit unsigned integer.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadUInt8(out AValue: UInt8); virtual; abstract;

    ///  <summary>Reads a 16-bit signed integer.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadInt16(out AValue: Int16); virtual; abstract;

    ///  <summary>Reads a 16-bit unsigned integer.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadUInt16(out AValue: UInt16); virtual; abstract;

    ///  <summary>Reads a 32-bit signed integer.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadInt32(out AValue: Int32); virtual; abstract;

    ///  <summary>Reads a 32-bit unsigned integer.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadUInt32(out AValue: UInt32); virtual; abstract;

    ///  <summary>Reads a 64-bit signed integer.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadInt64(out AValue: Int64); virtual; abstract;

    ///  <summary>Reads a 64-bit unsigned integer.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadUInt64(out AValue: UInt64); virtual; abstract;

    ///  <summary>Reads a single byte ANSI character.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadAnsiChar(out AValue: AnsiChar); virtual; abstract;

    ///  <summary>Reads a two byte WIDE character.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadWideChar(out AValue: WideChar); virtual; abstract;

    ///  <summary>Reads a single precision floating point value.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadSingle(out AValue: Single); virtual; abstract;

    ///  <summary>Reads a double precision floating point value.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadDouble(out AValue: Double); virtual; abstract;

    ///  <summary>Reads an extended precision floating point value.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadExtended(out AValue: Extended); virtual; abstract;

    ///  <summary>Reads a comp floating point value.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadComp(out AValue: Comp); virtual; abstract;

    ///  <summary>Reads a currency floating point value.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadCurrency(out AValue: Currency); virtual; abstract;

    ///  <summary>Reads a short ANSI string.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadShortString(out AValue: ShortString); virtual; abstract;

    ///  <summary>Reads a long ANSI string.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadAnsiString(out AValue: AnsiString); virtual; abstract;

    ///  <summary>Reads a long WIDE string.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadWideString(out AValue: WideString); virtual; abstract;

    ///  <summary>Reads a long UNICODE string.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadUnicodeString(out AValue: UnicodeString); virtual; abstract;

    ///  <summary>Reads a meta class.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. A correct implementation must
    ///  verify that the next value in the stream is indeed compatible with this read request and only then read it. Otherwise, the deserializer
    ///  must raise an exception.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadMetaClass(out AValue: TClass); virtual; abstract;

    ///  <summary>Reads a set.</summary>
    ///  <param name="ASetSize">The size of the expected set.</param>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadSet(const ASetSize: UInt8; out AValue); virtual; abstract;

    ///  <summary>Reads an enumeration.</summary>
    ///  <param name="AValue">The output value in which read data is stored.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure ReadEnum(out AValue: Int64); virtual; abstract;

    ///  <summary>Notifies the deserializer that the root type is about to be read.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. It marks the
    ///  beggining of the deserialization process.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure BeginReadRoot(); virtual; abstract;

    ///  <summary>Notifies the deserializer that the root type was read.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. It marks the
    ///  end of the deserialization process.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure EndReadRoot(); virtual; abstract;

    ///  <summary>Notifies the deserializer that a field is about to be read.</summary>
    ///  <param name="AField">The field RTTI information. Can never be <c>nil</c>.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. The descendant class must
    ///  read RTTI and informational data that the complementing serializer has writtern.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure BeginReadField(const AField: TRttiField); overload; virtual; abstract;

    ///  <summary>Notifies the deserializer that a label is about to be read.</summary>
    ///  <param name="ALabel">The label. Can be any string including an empty one.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure BeginReadField(const ALabel: String); overload; virtual; abstract;

    ///  <summary>Notifies the deserializer that a field or label was read.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. It marks the
    ///  end of a field or label deserialization.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure EndReadField(); virtual; abstract;

    ///  <summary>Notifies the deserializer that a record is about to be read.</summary>
    ///  <param name="ARecordType">The record RTTI information. Can never be <c>nil</c>.</param>
    ///  <param name="AId">The expected unique record ID as assigned by the serializer. <param name="AId"/> should be filled if the result
    ///  of this function is <c>rtInline</c> or <c>rtPointer</c>.</param>
    ///  <returns><c>rtInline</c> is the record is serialized inline. <c>rtPointer</c> if the read data is actually a pointer to another record,
    ///  in which case <paramref name="AId"/> identifies the pointed record. A result of <c>rtNil</c> means that a <c>nil</c> reference is read.</returns>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. The descending deserializer must return the proper
    ///  type of serialized entry.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    function BeginReadRecord(const ARecordType: TRttiRecordType; out AId: Int32): TReferenceType; virtual; abstract;

    ///  <summary>Notifies the deserializer that a record was read.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. It marks the
    ///  end of a record deserialization and is called only if <c>BeginReadRecord</c> was called priorly.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure EndReadRecord(); virtual; abstract;

    ///  <summary>Notifies the deserializer that a class is about to be read.</summary>
    ///  <param name="AClassType">The class RTTI information. Can never be <c>nil</c>.</param>
    ///  <param name="AType">The expected metaclass that is used to instantiate the class. If a <c>nil</c> value is returned, RTTI class info is used (which can be wrong).</param>
    ///  <param name="AId">The expected unique class ID as assigned by the serializer. <param name="AId"/> should be filled if the result
    ///  of this function is <c>rtInline</c> or <c>rtPointer</c>.</param>
    ///  <returns><c>rtInline</c> is the class is serialized inline. <c>rtPointer</c> if the read data is actually a pointer to another class,
    ///  in which case <paramref name="AId"/> identifies the pointed class. A result of <c>rtNil</c> means that a <c>nil</c> reference is read.</returns>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. The descending deserializer must return the proper
    ///  type of serialized entry.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    function BeginReadClass(const AClassType: TRttiInstanceType; out AType: TClass; out AId: Int32): TReferenceType; virtual; abstract;

    ///  <summary>Notifies the deserializer that a class was read.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. It marks the
    ///  end of a record deserialization and is called only if <c>BeginReadClass</c> was called priorly.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure EndReadClass(); virtual; abstract;

    ///  <summary>Notifies the deserializer that a static array is about to be read.</summary>
    ///  <param name="AArrayType">The array RTTI information. Can never be <c>nil</c>.</param>
    ///  <param name="ANumberOfElements">The number of expected array elements.</param>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure BeginReadStaticArray(const AArrayType: TRttiArrayType; const ANumberOfElements: NativeInt); virtual; abstract;

    ///  <summary>Notifies the deserializer that a static array was read.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. It marks the
    ///  end of a record deserialization and is called only if <c>BeginReadDynamicArray</c> was called priorly.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure EndReadStaticArray(); virtual; abstract;

    ///  <summary>Notifies the deserializer that a dynamic array is about to be read.</summary>
    ///  <param name="AArrayType">The class RTTI information. Can never be <c>nil</c>.</param>
    ///  <param name="ANumberOfElements">The number of written array elements. Always greater than zero.</param>
    ///  <param name="AId">The expected unique array ID as assigned by the serializer. <param name="AId"/> should be filled if the result
    ///  of this function is <c>rtInline</c> or <c>rtPointer</c>.</param>
    ///  <returns><c>rtInline</c> is the array is serialized inline. <c>rtPointer</c> if the read data is actually a pointer to another array,
    ///  in which case <paramref name="AId"/> identifies the pointed array. A result of <c>rtNil</c> means that a <c>nil</c> reference is read.</returns>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. The descending deserializer must return the proper
    ///  type of serialized entry.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    function BeginReadDynamicArray(const AArrayType: TRttiDynamicArrayType; out ANumberOfElements: NativeInt; out AId: Int32): TReferenceType; virtual; abstract;

    ///  <summary>Notifies the deserializer that a dynamic array was read.</summary>
    ///  <remarks>This is an abstract method that must be implemented in desceding serializer classes. It marks the
    ///  end of a record deserialization and is called only if <c>BeginReadDynamicArray</c> was called priorly.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of deserialization problems.</exception>
    procedure EndReadDynamicArray(); virtual; abstract;
  public
    ///  <summary>Creates a new deserializer instance.</summary>
    ///  <remarks>When descending from this class this constructor must be called. If not called, some internal
    ///  data structures will remain un-initialized.</remarks>
    constructor Create();

    ///  <summary>Destroys this deserializer instance.</summary>
    ///  <remarks>Never forget to call this destructor in descendant classes, otherwise memory leaks will occur.</remarks>
    destructor Destroy(); override;

    ///  <summary>Deserializes an object.</summary>
    ///  <returns>The deserialized object.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function Deserialize(): TObject; overload;

    ///  <summary>Serializes an generic value.</summary>
    ///  <returns>The deserialized value.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AValue"/> does not have RTTI.</exception>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function Deserialize<T>(): T; overload;

    ///  <summary>Deserializes an value.</summary>
    ///  <param name="ATypeInfo">The value's type information.</param>
    ///  <param name="AValue">The location of the value that needs to be deserialized.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ATypeInfo"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    procedure Deserialize(const ATypeInfo: PTypeInfo; out AValue); overload;

    ///  <summary>Specifies whether the deserializer throws exceptions for values that cannot be deserialized.</summary>
    ///  <returns><c>True</c> if deserializer skips problematic fields; <c>False</c> if the deserializer raises an exception on
    ///  problematic fields.</returns>
    property SkipErrors: Boolean read FSkipErrors write FSkipErrors;

    ///  <summary>Creates a new default deserializer.</summary>
    ///  <param name="AStream">The stream from which to deserialize.</param>
    ///  <returns>A new default binary deserializer.</returns>
    ///  <remarks>Generally, deserializers are not reusable for separate contexts. This means that after you deserialize a root type
    ///  the serializer needs to be destroyed.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AStream"/> is <c>nil</c>.</exception>
    class function Default(const AStream: TStream): TDeserializer; static;
  end;

  ///  <summary>Defines a set of methods that can be used to write data to a serialization engine in a safe way.</summary>
  ///  <remarks>This type wraps a serialization engine and exposes some of its functionality to the consumer classes in a safe way.
  ///  This type is used by the <see cref="Collections.Serialization|ISerializable" /> interface. Note that all write operations must be performed in the same
  ///  order the read operation will be performed when the class will be deserialized.</remarks>
  TOutputContext = record
  private
    FSerializer: TSerializer;
    class function Create(const ASerializer: TSerializer): TOutputContext; static;
  public
    ///  <summary>Writes a 8-bit signed integer using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: Int8): TOutputContext; overload;

    ///  <summary>Writes a 8-bit unsigned integer using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: UInt8): TOutputContext; overload;

    ///  <summary>Writes a 16-bit signed integer using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: Int16): TOutputContext; overload;

    ///  <summary>Writes a 16-bit unsigned integer using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: UInt16): TOutputContext; overload;

    ///  <summary>Writes a 32-bit signed integer using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: Int32): TOutputContext; overload;

    ///  <summary>Writes a 32-bit unsigned integer using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: UInt32): TOutputContext; overload;

    ///  <summary>Writes a 64-bit signed integer using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: Int64): TOutputContext; overload;

    ///  <summary>Writes a 64-bit unsigned integer using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: UInt64): TOutputContext; overload;

    ///  <summary>Writes a single byte ANSI character using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: AnsiChar): TOutputContext; overload;

    ///  <summary>Writes a two byte WIDE character using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: WideChar): TOutputContext; overload;

    ///  <summary>Writes a single precision floating point number using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: Single): TOutputContext; overload;

    ///  <summary>Writes a double precision floating point number using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: Double): TOutputContext; overload;

    ///  <summary>Writes an extended precision floating point number using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: Extended): TOutputContext; overload;

    ///  <summary>Writes a comp floating point number using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: Comp): TOutputContext; overload;

    ///  <summary>Writes a currency floating point number using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: Currency): TOutputContext; overload;

    ///  <summary>Writes a short ANSI string using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: ShortString): TOutputContext; overload;

    ///  <summary>Writes a long ANSI string using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: AnsiString): TOutputContext; overload;

    ///  <summary>Writes a long WIDE string using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: WideString): TOutputContext; overload;

    ///  <summary>Writes a long UNICODE string using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: UnicodeString): TOutputContext; overload;

    ///  <summary>Writes a metaclass using a given label. Note that the metaclass might not be resolvable on deserialization.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: TClass): TOutputContext; overload;

    ///  <summary>Writes an object using a given label. Note that the object's metaclass might not be resolvable on deserialization.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue(const AName: String; const AValue: TObject): TOutputContext; overload;

    ///  <summary>Writes a generic value using a given label.</summary>
    ///  <param name="AName">The name of the value to write. Otherwise known as the "label".</param>
    ///  <param name="AValue">The value containing the actual data to write.</param>
    ///  <returns>This <see cref="Collections.Serialization|TOutputContext" />. Can be used to chain read operations.</returns>
    ///  <remarks>This method can be used to serialize anything, starting with simple values like integer and ending in types such as arrays of records.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">A wide variety of serialization problems.</exception>
    function AddValue<T>(const AName: String; const AValue: T): TOutputContext; overload;
  end;

  ///  <summary>Defines a set of methods that can be used to read data from a deserialization engine in a safe way.</summary>
  ///  <remarks>This type wraps a deserialization engine and exposes some of its functionality to the consumer classes in a safe way.
  ///  This type is used by the <see cref="Collections.Serialization|ISerializable" /> interface. Note that all read operations must be performed in the same
  ///  order the write operation were performed when the class was serialized.</remarks>
  TInputContext = record
  private
    FDeserializer: TDeserializer;
    class function Create(const ADeserializer: TDeserializer): TInputContext; static;
  public
    ///  <summary>Reads a 8-bit signed integer using a given label.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: Int8): TInputContext; overload;

    ///  <summary>Reads a 8-bit unsigned integer using a given label.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: UInt8): TInputContext; overload;

    ///  <summary>Reads a 16-bit signed integer using a given label.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: Int16): TInputContext; overload;

    ///  <summary>Reads a 16-bit unsigned integer using a given label.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: UInt16): TInputContext; overload;

    ///  <summary>Reads a 32-bit signed integer using a given label.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: Int32): TInputContext; overload;

    ///  <summary>Reads a 32-bit unsigned integer using a given label.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: UInt32): TInputContext; overload;

    ///  <summary>Reads a 64-bit signed integer using a given label.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: Int64): TInputContext; overload;

    ///  <summary>Reads a 64-bit signed integer using a given label.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: UInt64): TInputContext; overload;

    ///  <summary>Reads a single byte ANSI character.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: AnsiChar): TInputContext; overload;

    ///  <summary>Reads a two-byte WIDE character.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: WideChar): TInputContext; overload;

    ///  <summary>Reads a single precision floating point number.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: Single): TInputContext; overload;

    ///  <summary>Reads a double precision floating point number.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: Double): TInputContext; overload;

    ///  <summary>Reads an extended precision floating point number.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: Extended): TInputContext; overload;

    ///  <summary>Reads comp floating point number.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: Comp): TInputContext; overload;

    ///  <summary>Reads currency floating point number.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: Currency): TInputContext; overload;

    ///  <summary>Reads short ANSI string.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: ShortString): TInputContext; overload;

    ///  <summary>Reads a long ANSI string.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: AnsiString): TInputContext; overload;

    ///  <summary>Reads a long WIDE string.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: WideString): TInputContext; overload;

    ///  <summary>Reads a long UNICODE string.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: UnicodeString): TInputContext; overload;

    ///  <summary>Reads a metaclass. If the metaclass cannot be resolved to a real type a <c>nil</c> values is returned.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: TClass): TInputContext; overload;

    ///  <summary>Reads an object instance. If the metaclass of the object cannot be resolved the mest match is used.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue(const AName: String; out AValue: TObject): TInputContext; overload;

    ///  <summary>Reads a generic value. Use this method when none of the above overloads are helpful.</summary>
    ///  <param name="AName">The name of the value to read. Otherwise known as the "label".</param>
    ///  <param name="AValue">An output value containing the read data.</param>
    ///  <returns>This <see cref="Collections.Serialization|TInputContext" />. Can be used to chain read operations.</returns>
    ///  <remarks>This method can be used to deserialize anything, starting with simple values like integer and ending in types such as arrays of records. If the serialized data
    ///  is not compatible with the provided generic argument an exception will occur.</remarks>
    ///  <exception cref="Collections.Base|ESerializationException">The provided label wasn't found or the requested type is invalid.</exception>
    function GetValue<T>(const AName: String; out AValue: T): TInputContext; overload;
  end;

  ///  <summary>Defines a set of methods that need to be implemented by classes in order
  ///  to override the default serialization support.</summary>
  ///  <remarks>Implement this interface only if custom serialization code is needed for a
  ///  class. A class implementing this interface need not to worry about the reference counting mechanics. The serialization engine
  ///  will not modify the reference count of the instance being serialized or deserializaed.</remarks>
  ISerializable = interface
    ['{F00C10CC-1744-4905-B4C9-F158DCF6A7A7}']

    ///  <summary>Serializes the instance of the class that implements this interface.</summary>
    ///  <param name="AContext">The output serialization context into which values need to be added.</param>
    ///  <remarks>This method is called automatically by the serialization engine when the class implementing this interface
    ///  is serialized. Use the provided context object to serialize distinct values that make up the object.</remarks>
    procedure Serialize(const AContext: TOutputContext);

    ///  <summary>Deserializes the instance of the class that implements this interface.</summary>
    ///  <param name="AContext">The input serialization context from which values need to be read.</param>
    ///  <remarks>This method is called automatically by the deserialization engine when the class implementing this interface
    ///  is deserialized. Use the provided context object to read distinct values that make up the object. Note that this method is called
    ///  right after the default parameterless constructor of the class is called. The constructor need to prepare the object.</remarks>
    procedure Deserialize(const AContext: TInputContext);
  end;

implementation

function IsSerializable(const AField: TRttiField): Boolean;
var
  LAttr: TCustomAttribute;
begin
  ASSERT(Assigned(AField));
  for LAttr in AField.GetAttributes() do
    if LAttr is NonSerialized then
      Exit(False);

  Result := True;
end;

procedure GetSafeInterface(const AObject: TObject; const AIID: TGUID; var AOut: Pointer);
var
  LIntfEntry: PInterfaceEntry;

begin
  AOut := nil;

  { Nothing on nil object }
  if not Assigned(AObject) then
    Exit;

  { Obtain the interface entry }
  LIntfEntry := AObject.GetInterfaceEntry(AIID);

  { If there is such an interface and it has an Object offset, get it }
  if (Assigned(LIntfEntry)) and (LIntfEntry^.IOffset <> 0) then
    AOut := Pointer(NativeUInt(AObject) + NativeUInt(LIntfEntry^.IOffset));

  { Note: No AddRef is performed since we have no idea if the object
    has ref cont > 0 already! We're only using the "pseudo-intf" entry }
end;

type
  PInt8 = ^Int8;
  PUInt8 = ^UInt8;
  PInt16 = ^Int16;
  PUInt16 = ^UInt16;
  PInt32 = ^Int32;
  PUInt32 = ^UInt32;
  PInt64 = ^Int64;
  PUInt64 = ^UInt64;
  PClass = ^TClass;

{$REGION 'Binary Serialization'}
type
  TStreamedValueType = (
      svStartRoot, svEndRoot,
      svStartRecord, svEndRecord, svReferencedRecord, svNilRecord,
      svStartField, svStartLabel, svEndField,
      svStartClass, svEndClass, svReferencedClass, svNilClass,
      svStartStaticArray, svEndStaticArray,
      svStartDynamicArray, svEndDynamicArray, svReferencedDynamicArray, svNilDynamicArray,
      svInt8, svUInt8, svInt16, svUInt16, svInt32, svUInt32, svInt64, svUInt64,
      svSingle, svDouble, svExtended, svComp, svCurrency,
      svAnsiChar, svWideChar,
      svShortString, svAnsiString, svWideString, svUnicodeString,
      svMetaClass, svNilMetaClass,
      svEnum, svSet
  );

  TStreamedValueTypes = set of TStreamedValueType;

  TBinarySerializer = class(TSerializer)
  private
    FStream: TStream;

    procedure WriteCustomType(const AType: TRttiType);
    procedure WriteType(const AType: TStreamedValueType);
    procedure WriteData(const ASize: NativeInt; const AData);
  protected
    procedure WriteInt8(const AValue: Int8); override;
    procedure WriteUInt8(const AValue: UInt8); override;
    procedure WriteInt16(const AValue: Int16); override;
    procedure WriteUInt16(const AValue: UInt16); override;
    procedure WriteInt32(const AValue: Int32); override;
    procedure WriteUInt32(const AValue: UInt32); override;
    procedure WriteInt64(const AValue: Int64); override;
    procedure WriteUInt64(const AValue: UInt64); override;
    procedure WriteAnsiChar(const AValue: AnsiChar); override;
    procedure WriteWideChar(const AValue: WideChar); override;
    procedure WriteSingle(const AValue: Single); override;
    procedure WriteDouble(const AValue: Double); override;
    procedure WriteExtended(const AValue: Extended); override;
    procedure WriteComp(const AValue: Comp); override;
    procedure WriteCurrency(const AValue: Currency); override;
    procedure WriteShortString(const AValue: ShortString); override;
    procedure WriteAnsiString(const AValue: AnsiString); override;
    procedure WriteWideString(const AValue: WideString); override;
    procedure WriteUnicodeString(const AValue: UnicodeString); override;
    procedure WriteMetaClass(const AValue: TClass); override;
    procedure WriteSet(const ASetSize: UInt8; const AValue); override;
    procedure WriteEnum(const AValue: Int64); override;

    procedure BeginWriteRoot(); override;
    procedure EndWriteRoot(); override;

    procedure BeginWriteField(const AField: TRttiField); overload; override;
    procedure BeginWriteField(const ALabel: String); overload; override;
    procedure EndWriteField(); override;

    procedure BeginWriteRecord(const ARecordType: TRttiRecordType; const AId: Int32); override;
    procedure EndWriteRecord(); override;
    procedure WriteRecordReference(const AReference: Int32); override;
    procedure WriteNilRecordReference(); override;

    procedure BeginWriteClass(const AClassType: TRttiInstanceType; const AType: TClass; const AId: Int32); override;
    procedure EndWriteClass(); override;
    procedure WriteClassReference(const AReference: Int32); override;
    procedure WriteNilClassReference(); override;

    procedure BeginWriteStaticArray(const AArrayType: TRttiArrayType; const ANumberOfElements: NativeInt); override;
    procedure EndWriteStaticArray(); override;

    procedure BeginWriteDynamicArray(const AArrayType: TRttiDynamicArrayType; const ANumberOfElements: NativeInt; const AId: Int32); override;
    procedure EndWriteDynamicArray(); override;
    procedure WriteDynamicArrayReference(const AReference: Int32); override;
    procedure WriteNilDynamicArrayReference(); override;
  public
    constructor Create(const AStream: TStream);
  end;

  TBinaryDeserializer = class(TDeserializer)
  private
    FStream: TStream;

    procedure ExpectType(const AWhatType: TRttiType);
    procedure Expect(const AWhat: TStreamedValueType); overload;
    function Expect(const AWhat: TStreamedValueTypes): TStreamedValueType; overload;
    procedure ReadData(const ASize: NativeInt; out AData);

    function GetMetaClass(const AUnit, AClass: String): TClass;
  protected
    procedure ReadInt8(out AValue: Int8); override;
    procedure ReadUInt8(out AValue: UInt8); override;
    procedure ReadInt16(out AValue: Int16); override;
    procedure ReadUInt16(out AValue: UInt16); override;
    procedure ReadInt32(out AValue: Int32); override;
    procedure ReadUInt32(out AValue: UInt32); override;
    procedure ReadInt64(out AValue: Int64); override;
    procedure ReadUInt64(out AValue: UInt64); override;
    procedure ReadAnsiChar(out AValue: AnsiChar); override;
    procedure ReadWideChar(out AValue: WideChar); override;
    procedure ReadSingle(out AValue: Single); override;
    procedure ReadDouble(out AValue: Double); override;
    procedure ReadExtended(out AValue: Extended); override;
    procedure ReadComp(out AValue: Comp); override;
    procedure ReadCurrency(out AValue: Currency); override;
    procedure ReadShortString(out AValue: ShortString); override;
    procedure ReadAnsiString(out AValue: AnsiString); override;
    procedure ReadWideString(out AValue: WideString); override;
    procedure ReadUnicodeString(out AValue: UnicodeString); override;
    procedure ReadMetaClass(out AValue: TClass); override;
    procedure ReadSet(const ASetSize: UInt8; out AValue); override;
    procedure ReadEnum(out AValue: Int64); override;

    procedure BeginReadRoot(); override;
    procedure EndReadRoot(); override;

    procedure BeginReadField(const AField: TRttiField); overload; override;
    procedure BeginReadField(const ALabel: String); overload; override;
    procedure EndReadField(); override;

    function BeginReadRecord(const ARecordType: TRttiRecordType; out AId: Int32): TDeserializer.TReferenceType; override;
    procedure EndReadRecord(); override;

    function BeginReadClass(const AClassType: TRttiInstanceType; out AType: TClass; out AId: Int32): TDeserializer.TReferenceType; override;
    procedure EndReadClass(); override;

    procedure BeginReadStaticArray(const AArrayType: TRttiArrayType; const ANumberOfElements: NativeInt); override;
    procedure EndReadStaticArray(); override;

    function BeginReadDynamicArray(const AArrayType: TRttiDynamicArrayType; out ANumberOfElements: NativeInt; out AId: Int32): TDeserializer.TReferenceType; override;
    procedure EndReadDynamicArray(); override;

  public
    constructor Create(const AStream: TStream);
  end;

{ TBinarySerializer }

procedure TBinarySerializer.BeginWriteClass(const AClassType: TRttiInstanceType; const AType: TClass; const AId: Int32);
begin
  WriteType(svStartClass);
  WriteMetaClass(AType);
  WriteInt32(AId);
end;

procedure TBinarySerializer.BeginWriteDynamicArray(const AArrayType: TRttiDynamicArrayType; const ANumberOfElements: NativeInt; const AId: Int32);
begin
  ASSERT(Assigned(AArrayType));
  ASSERT(ANumberOfElements > 0);

  WriteType(svStartDynamicArray);
  WriteCustomType(AArrayType);

  WriteInt32(AId);
  WriteInt64(ANumberOfElements);
end;

procedure TBinarySerializer.BeginWriteField(const ALabel: String);
begin
  WriteType(svStartLabel);
  WriteUnicodeString(ALabel);
end;

procedure TBinarySerializer.BeginWriteField(const AField: TRttiField);
begin
  ASSERT(Assigned(AField));

  WriteType(svStartField);
  WriteCustomType(AField.FieldType);
  WriteUnicodeString(AField.Name);
  WriteInt64(AField.Offset);
end;

procedure TBinarySerializer.BeginWriteRecord(const ARecordType: TRttiRecordType; const AId: Int32);
begin
  ASSERT(Assigned(ARecordType));

  WriteType(svStartRecord);
  WriteCustomType(ARecordType);
  WriteInt32(AId);
end;

procedure TBinarySerializer.BeginWriteRoot;
begin
  WriteType(svStartRoot);
end;

procedure TBinarySerializer.BeginWriteStaticArray(const AArrayType: TRttiArrayType; const ANumberOfElements: NativeInt);
begin
  ASSERT(Assigned(AArrayType));

  WriteType(svStartStaticArray);
  WriteCustomType(AArrayType);
  WriteInt64(ANumberOfElements);
end;

constructor TBinarySerializer.Create(const AStream: TStream);
begin
  inherited Create();

  if not Assigned(AStream) then
    ExceptionHelper.Throw_ArgumentNilError('AStream');

  FStream := AStream;
end;

procedure TBinarySerializer.EndWriteClass;
begin
  WriteType(svEndClass);
end;

procedure TBinarySerializer.EndWriteDynamicArray;
begin
  WriteType(svEndDynamicArray);
end;

procedure TBinarySerializer.EndWriteField;
begin
  WriteType(svEndField);
end;

procedure TBinarySerializer.EndWriteRecord;
begin
  WriteType(svEndRecord);
end;

procedure TBinarySerializer.EndWriteRoot;
begin
  WriteType(svEndRoot);
end;

procedure TBinarySerializer.EndWriteStaticArray;
begin
  WriteType(svEndStaticArray);
end;

procedure TBinarySerializer.WriteAnsiChar(const AValue: AnsiChar);
begin
  WriteType(svAnsiChar);
  WriteData(SizeOf(AValue), AValue);
end;

procedure TBinarySerializer.WriteAnsiString(const AValue: AnsiString);
var
  LCodePage: UInt16;
  LLength: NativeInt;
begin
  LLength := Length(AValue);
  LCodePage := StringCodePage(AValue);

  WriteType(svAnsiString);
  WriteData(SizeOf(LCodePage), LCodePage);
  WriteData(SizeOf(LLength), LLength);
  WriteData(LLength, AValue[1]);
end;

procedure TBinarySerializer.WriteClassReference(const AReference: Int32);
begin
  WriteType(svReferencedClass);
  WriteInt32(AReference);
end;

procedure TBinarySerializer.WriteComp(const AValue: Comp);
begin
  WriteType(svComp);
  WriteData(SizeOf(AValue), AValue);
end;

procedure TBinarySerializer.WriteCurrency(const AValue: Currency);
begin
  WriteType(svCurrency);
  WriteData(SizeOf(AValue), AValue);
end;

procedure TBinarySerializer.WriteCustomType(const AType: TRttiType);
begin
  ASSERT(Assigned(AType));
  WriteUnicodeString(AType.Name);
end;

procedure TBinarySerializer.WriteData(const ASize: NativeInt; const AData);
begin
  FStream.WriteBuffer(AData, ASize);
end;

procedure TBinarySerializer.WriteDouble(const AValue: Double);
begin
  WriteType(svDouble);
  WriteData(SizeOf(AValue), AValue);
end;

procedure TBinarySerializer.WriteDynamicArrayReference(const AReference: Int32);
begin
  WriteType(svReferencedDynamicArray);
  WriteData(SizeOf(AReference), AReference);
end;

procedure TBinarySerializer.WriteEnum(const AValue: Int64);
begin
  WriteType(svEnum);
  WriteData(SizeOf(AValue), AValue);
end;

procedure TBinarySerializer.WriteExtended(const AValue: Extended);
begin
  WriteType(svExtended);
  WriteData(SizeOf(AValue), AValue);
end;

procedure TBinarySerializer.WriteInt16(const AValue: Int16);
begin
  WriteType(svInt16);
  WriteData(SizeOf(AValue), AValue);
end;

procedure TBinarySerializer.WriteInt32(const AValue: Int32);
begin
  WriteType(svInt32);
  WriteData(SizeOf(AValue), AValue);
end;

procedure TBinarySerializer.WriteInt64(const AValue: Int64);
begin
  WriteType(svInt64);
  WriteData(SizeOf(AValue), AValue);
end;

procedure TBinarySerializer.WriteInt8(const AValue: Int8);
begin
  WriteType(svInt8);
  WriteData(SizeOf(AValue), AValue);
end;

procedure TBinarySerializer.WriteMetaClass(const AValue: TClass);
begin
  if Assigned(AValue) then
  begin
    WriteType(svMetaClass);
    WriteUnicodeString(AValue.UnitName);
    WriteUnicodeString(AValue.ClassName);
  end else
    WriteType(svNilMetaClass);
end;

procedure TBinarySerializer.WriteNilClassReference;
begin
  WriteType(svNilClass);
end;

procedure TBinarySerializer.WriteNilDynamicArrayReference;
begin
  WriteType(svNilDynamicArray);
end;

procedure TBinarySerializer.WriteNilRecordReference;
begin
  WriteType(svNilRecord);
end;

procedure TBinarySerializer.WriteRecordReference(const AReference: Int32);
begin
  WriteType(svReferencedRecord);
  WriteInt32(AReference);
end;

procedure TBinarySerializer.WriteSet(const ASetSize: UInt8; const AValue);
begin
  WriteType(svSet);
  WriteUInt8(ASetSize);
  WriteData(ASetSize, AValue);
end;

procedure TBinarySerializer.WriteShortString(const AValue: ShortString);
var
  LLength: UInt8;
begin
  LLength := Length(AValue);

  WriteType(svShortString);
  WriteData(SizeOf(LLength), LLength);
  WriteData(LLength, AValue[1]);
end;

procedure TBinarySerializer.WriteSingle(const AValue: Single);
begin
  WriteType(svSingle);
  WriteData(SizeOf(AValue), AValue);
end;

procedure TBinarySerializer.WriteType(const AType: TStreamedValueType);
begin
  FStream.WriteBuffer(AType, SizeOf(AType));
end;

procedure TBinarySerializer.WriteUInt16(const AValue: UInt16);
begin
  WriteType(svUInt16);
  WriteData(SizeOf(AValue), AValue);
end;

procedure TBinarySerializer.WriteUInt32(const AValue: UInt32);
begin
  WriteType(svUInt32);
  WriteData(SizeOf(AValue), AValue);
end;

procedure TBinarySerializer.WriteUInt64(const AValue: UInt64);
begin
  WriteType(svUInt64);
  WriteData(SizeOf(AValue), AValue);
end;

procedure TBinarySerializer.WriteUInt8(const AValue: UInt8);
begin
  WriteType(svUInt8);
  WriteData(SizeOf(AValue), AValue);
end;

procedure TBinarySerializer.WriteUnicodeString(const AValue: UnicodeString);
var
  LLength: NativeInt;
  LUtf8: RawByteString;
begin
  LUtf8 := UTF8Encode(AValue);
  LLength := Length(LUtf8);

  WriteType(svUnicodeString);
  WriteData(SizeOf(LLength), LLength);
  WriteData(LLength, LUtf8[1]);
end;

procedure TBinarySerializer.WriteWideChar(const AValue: WideChar);
begin
  WriteType(svWideChar);
  WriteData(SizeOf(AValue), AValue);
end;

procedure TBinarySerializer.WriteWideString(const AValue: WideString);
var
  LLength: NativeInt;
  LUtf8: RawByteString;
begin
  LUtf8 := UTF8Encode(AValue);
  LLength := Length(LUtf8);

  WriteType(svWideString);
  WriteData(SizeOf(LLength), LLength);
  WriteData(LLength, LUtf8[1]);
end;


{ TBinaryDeserializer }

function TBinaryDeserializer.BeginReadClass(const AClassType: TRttiInstanceType; out AType: TClass; out AId: Int32): TDeserializer.TReferenceType;
begin
  ASSERT(Assigned(AClassType));

  Result := rtInline;
  case Expect([svStartClass, svReferencedClass, svNilClass]) of
    svStartClass:
    begin
      ReadMetaClass(AType);
      ReadInt32(AId);
    end;

    svReferencedClass:
    begin
      ReadInt32(AId);
      Result := rtPointer;
    end;

    svNilClass:
      Result := rtNil;
  end;
end;

function TBinaryDeserializer.BeginReadDynamicArray(const AArrayType: TRttiDynamicArrayType;
  out ANumberOfElements: NativeInt; out AId: Int32): TDeserializer.TReferenceType;
var
  LCount: Int64;
begin
  ASSERT(Assigned(AArrayType));

  Result := rtInline;
  case Expect([svStartDynamicArray, svReferencedDynamicArray, svNilDynamicArray]) of
    svStartDynamicArray:
    begin
      ExpectType(AArrayType);
      ReadInt32(AId);
      ReadInt64(LCount);
      ANumberOfElements := LCount;
    end;

    svReferencedDynamicArray:
    begin
      ReadInt32(AId);
      Result := rtPointer;
    end;

    svNilDynamicArray:
      Result := rtNil;
  end;
end;

procedure TBinaryDeserializer.BeginReadField(const ALabel: String);
var
  LLabel: String;
begin
  Expect(svStartLabel);
  ReadUnicodeString(LLabel);

  if (LLabel <> ALabel) then
    ExceptionHelper.Throw_ExpectedAnotherLabel(ALabel, LLabel);
end;

procedure TBinaryDeserializer.BeginReadField(const AField: TRttiField);
var
  LName: String;
  LOffset: Int64;
begin
  ASSERT(Assigned(AField));

  Expect(svStartField);
  ExpectType(AField.FieldType);

  ReadUnicodeString(LName);
  ReadInt64(LOffset);

  if (LName <> AField.Name) or (LOffset <> AField.Offset) then
    ExceptionHelper.Throw_ExpectedAnotherField(AField, LName, LOffset);
end;

function TBinaryDeserializer.BeginReadRecord(const ARecordType: TRttiRecordType; out AId: Int32): TDeserializer.TReferenceType;
begin
  ASSERT(Assigned(ARecordType));

  Result := rtInline;
  case Expect([svStartRecord, svReferencedRecord, svNilRecord]) of
    svStartRecord:
    begin
      ExpectType(ARecordType);
      ReadInt32(AId);
    end;

    svReferencedRecord:
    begin
      ReadInt32(AId);
      Result := rtPointer;
    end;

    svNilRecord:
      Result := rtNil;
  end;
end;

procedure TBinaryDeserializer.BeginReadRoot;
begin
  Expect(svStartRoot);
end;

procedure TBinaryDeserializer.BeginReadStaticArray(const AArrayType: TRttiArrayType; const ANumberOfElements: NativeInt);
var
  LElements: Int64;
begin
  ASSERT(Assigned(AArrayType));

  Expect(svStartStaticArray);
  ExpectType(AArrayType);
  ReadInt64(LElements);

  if (ANumberOfElements <> LElements) then
    ExceptionHelper.Throw_ExpectedAnotherElementCount(AArrayType, ANumberOfElements, LElements);
end;

constructor TBinaryDeserializer.Create(const AStream: TStream);
begin
  inherited Create();

  if not Assigned(AStream) then
    ExceptionHelper.Throw_ArgumentNilError('AStream');

  FStream := AStream;
end;

procedure TBinaryDeserializer.EndReadClass;
begin
  Expect(svEndClass);
end;

procedure TBinaryDeserializer.EndReadDynamicArray;
begin
  Expect(svEndDynamicArray);
end;

procedure TBinaryDeserializer.EndReadField;
begin
  Expect(svEndField);
end;

procedure TBinaryDeserializer.EndReadRecord;
begin
  Expect(svEndRecord);
end;

procedure TBinaryDeserializer.EndReadRoot;
begin
  Expect(svEndRoot);
end;

procedure TBinaryDeserializer.EndReadStaticArray;
begin
  Expect(svEndStaticArray);
end;

function TBinaryDeserializer.Expect(const AWhat: TStreamedValueTypes): TStreamedValueType;
begin
  FStream.ReadBuffer(Result, SizeOf(Result));

  if not (Result in AWhat) then
    ExceptionHelper.Throw_ExpectedAnotherBinaryValuePoint();
end;

procedure TBinaryDeserializer.ExpectType(const AWhatType: TRttiType);
var
  LName: String;
begin
  ASSERT(Assigned(AWhatType));
  ReadUnicodeString(LName);

  if (LName <> AWhatType.Name) then
    ExceptionHelper.Throw_ExpectedAnotherType(AWhatType, LName);
end;

function TBinaryDeserializer.GetMetaClass(const AUnit, AClass: String): TClass;
var
  LType: TRttiType;
begin
  LType := RttiContext.FindType(AUnit + '.' + AClass);

  { Find out the type }
  if LType is TRttiInstanceType then
    Result := TRttiInstanceType(LType).MetaclassType
  else
    Result := nil;
end;


procedure TBinaryDeserializer.Expect(const AWhat: TStreamedValueType);
begin
  Expect([AWhat]);
end;

procedure TBinaryDeserializer.ReadAnsiChar(out AValue: AnsiChar);
begin
  Expect(svAnsiChar);
  ReadData(SizeOf(AValue), AValue);
end;

procedure TBinaryDeserializer.ReadAnsiString(out AValue: AnsiString);
var
  LCodePage: UInt16;
  LLength: NativeInt;
begin
  Expect(svAnsiString);
  ReadData(SizeOf(LCodePage), LCodePage);
  ReadData(SizeOf(LLength), LLength);

  SetLength(AValue, LLength);
  SetCodePage(RawByteString(AValue), LCodePage);
  ReadData(LLength, AValue[1]);
end;

procedure TBinaryDeserializer.ReadComp(out AValue: Comp);
begin
  Expect(svComp);
  ReadData(SizeOf(AValue), AValue);
end;

procedure TBinaryDeserializer.ReadCurrency(out AValue: Currency);
begin
  Expect(svCurrency);
  ReadData(SizeOf(AValue), AValue);
end;

procedure TBinaryDeserializer.ReadData(const ASize: NativeInt; out AData);
begin
  FStream.ReadBuffer(AData, ASize);
end;

procedure TBinaryDeserializer.ReadDouble(out AValue: Double);
begin
  Expect(svDouble);
  ReadData(SizeOf(AValue), AValue);
end;

procedure TBinaryDeserializer.ReadEnum(out AValue: Int64);
begin
  Expect(svEnum);
  ReadData(SizeOf(AValue), AValue);
end;

procedure TBinaryDeserializer.ReadExtended(out AValue: Extended);
begin
  Expect(svExtended);
  ReadData(SizeOf(AValue), AValue);
end;

procedure TBinaryDeserializer.ReadInt16(out AValue: Int16);
begin
  Expect(svInt16);
  ReadData(SizeOf(AValue), AValue);
end;

procedure TBinaryDeserializer.ReadInt32(out AValue: Int32);
begin
  Expect(svInt32);
  ReadData(SizeOf(AValue), AValue);
end;

procedure TBinaryDeserializer.ReadInt64(out AValue: Int64);
begin
  Expect(svInt64);
  ReadData(SizeOf(AValue), AValue);
end;

procedure TBinaryDeserializer.ReadInt8(out AValue: Int8);
begin
  Expect(svInt8);
  ReadData(SizeOf(AValue), AValue);
end;

procedure TBinaryDeserializer.ReadMetaClass(out AValue: TClass);
var
  LUnit, LClass: String;
begin
  case Expect([svMetaClass, svNilMetaClass]) of
    svMetaClass:
    begin
      ReadUnicodeString(LUnit);
      ReadUnicodeString(LClass);

      AValue := GetMetaClass(LUnit, LClass);
    end;

    svNilMetaClass:
      AValue := nil;
  end;
end;

procedure TBinaryDeserializer.ReadSet(const ASetSize: UInt8; out AValue);
var
  LSize: UInt8;
begin
  Expect(svSet);
  ReadUInt8(LSize);

  if (LSize <> ASetSize) then
    ExceptionHelper.Throw_ExpectedAnotherSetSize(ASetSize, LSize);

  ReadData(LSize, AValue);
end;

procedure TBinaryDeserializer.ReadShortString(out AValue: ShortString);
var
  LLength: UInt8;
begin
  Expect(svShortString);
  ReadData(SizeOf(LLength), LLength);
  ReadData(LLength, AValue[1]);
  AValue[0] := AnsiChar(LLength);
end;

procedure TBinaryDeserializer.ReadSingle(out AValue: Single);
begin
  Expect(svSingle);
  ReadData(SizeOf(AValue), AValue);
end;

procedure TBinaryDeserializer.ReadUInt16(out AValue: UInt16);
begin
  Expect(svUInt16);
  ReadData(SizeOf(AValue), AValue);
end;

procedure TBinaryDeserializer.ReadUInt32(out AValue: UInt32);
begin
  Expect(svUInt32);
  ReadData(SizeOf(AValue), AValue);
end;

procedure TBinaryDeserializer.ReadUInt64(out AValue: UInt64);
begin
  Expect(svUInt64);
  ReadData(SizeOf(AValue), AValue);
end;

procedure TBinaryDeserializer.ReadUInt8(out AValue: UInt8);
begin
  Expect(svUInt8);
  ReadData(SizeOf(AValue), AValue);
end;

procedure TBinaryDeserializer.ReadUnicodeString(out AValue: UnicodeString);
var
  LLength: NativeInt;
  Utf8: RawByteString;
begin
  Expect(svUnicodeString);
  ReadData(SizeOf(LLength), LLength);

  SetLength(Utf8, LLength);
  ReadData(LLength, Utf8[1]);

  AValue := UTF8ToUnicodeString(Utf8);
end;

procedure TBinaryDeserializer.ReadWideChar(out AValue: WideChar);
begin
  Expect(svWideChar);
  ReadData(SizeOf(AValue), AValue);
end;

procedure TBinaryDeserializer.ReadWideString(out AValue: WideString);
var
  LLength: NativeInt;
  Utf8: RawByteString;
begin
  Expect(svWideString);
  ReadData(SizeOf(LLength), LLength);

  SetLength(Utf8, LLength);
  ReadData(LLength, Utf8[1]);

  AValue := UTF8ToWideString(Utf8);
end;

{$ENDREGION}

{ TSerializer }

class function TSerializer.Default(const AStream: TStream): TSerializer;
begin
  Result := TBinarySerializer.Create(AStream);
end;

constructor TSerializer.Create;
begin
  FDynArrayReg := TDictionary<Pointer, Int32>.Create();
  FObjectReg := TDictionary<Pointer, Int32>.Create();
  FRecordReg := TDictionary<Pointer, Int32>.Create();

  FSkipErrors := true;
end;

destructor TSerializer.Destroy;
begin
  FRecordReg.Free;
  FDynArrayReg.Free;
  FObjectReg.Free;

  inherited;
end;

procedure TSerializer.ErrorNoFieldRtti(const AField: TRttiField);
begin
  if not FSkipErrors then
    ExceptionHelper.Throw_FieldTypeDoesNotHaveEnoughRtti(AField);
end;

procedure TSerializer.ErrorNotEnoughRtti(const ATypeInfo: PTypeInfo);
begin
  if not FSkipErrors then
    ExceptionHelper.Throw_TypeDoesNotHaveEnoughRtti(ATypeInfo);
end;

procedure TSerializer.ErrorNotSupported(const ATypeInfo: PTypeInfo);
begin
  if not FSkipErrors then
    ExceptionHelper.Throw_TypeCannotBeSerialized(ATypeInfo);
end;

procedure TSerializer.Serialize(const ATypeInfo: PTypeInfo; const AValue);
begin
  if not Assigned(ATypeInfo) then
    ExceptionHelper.Throw_ArgumentNilError('ATypeInfo');

  { Call internal "un-checked" method. }
  BeginWriteRoot();
  SerializeInternal(FRttiContext.GetType(ATypeInfo), @AValue);
  EndWriteRoot();
end;

procedure TSerializer.Serialize(const AObject: TObject);
begin
  if Assigned(AObject) then
    Serialize(AObject.ClassInfo, AObject)
  else
    Serialize(TypeInfo(TObject), AObject);
end;

procedure TSerializer.Serialize<T>(const AValue: T);
begin
  Serialize(TypeInfo(T), AValue);
end;

procedure TSerializer.WriteClass(const ATypeInfo: PTypeInfo; const AObject: TObject);
var
  LClass: TRttiInstanceType;
  LField: TRttiField;
  LClassId: Int32;
  LSerializable: ISerializable;
begin
  { ARefToRecord points to the first field in the record }
  ASSERT(ATypeInfo^.Kind = tkClass);

  { Get the class info }
  LClass := nil;
  if Assigned(AObject) then
    LClass := FRttiContext.GetType(AObject.ClassType) as TRttiInstanceType;
  if not Assigned(LClass) then
    LClass := FRttiContext.GetType(ATypeInfo) as TRttiInstanceType;

  if not Assigned(LClass) then
  begin
    ErrorNotEnoughRtti(ATypeInfo);
    Exit;
  end;

  { Check if the object is already serialized }
  if not Assigned(AObject) then
    WriteNilClassReference()
  else if FObjectReg.TryGetValue(AObject, LClassId) then
    WriteClassReference(LClassId)
  else begin
    { Register object in the dictionary }
    Inc(FLastId);
    FObjectReg.Add(AObject, FLastId);

    { Notify that a class is being written }
    BeginWriteClass(LClass, AObject.ClassType, FLastId);

    { Check if the class has it's own serialization code }
    GetSafeInterface(AObject, ISerializable, Pointer(LSerializable));
    if Assigned(LSerializable) then
    begin
      LSerializable.Serialize(TOutputContext.Create(Self));
      Pointer(LSerializable) := nil;
    end else begin
      { Walk through the record }
      for LField in LClass.GetFields() do
      begin
        if not Assigned(LField.FieldType) then
        begin
          ErrorNoFieldRtti(LField);
          continue;
        end;

        if IsSerializable(LField) then
        begin
          BeginWriteField(LField);
          SerializeInternal(LField.FieldType, Pointer(LField.Offset + NativeInt(AObject)));
          EndWriteField();
        end;
      end;
    end;

    EndWriteClass();
  end;
end;

procedure TSerializer.WriteDynamicArray(const ATypeInfo: PTypeInfo; const ADynArray: Pointer);
var
  LArray: TRttiDynamicArrayType;
  LIndex, LCount: NativeInt;
  LArrayId: Int32;
begin
  { ARefToRecord points to the first field in the record }
  ASSERT(ATypeInfo^.Kind = tkDynArray);
  LArray := FRttiContext.GetType(ATypeInfo) as TRttiDynamicArrayType;

  if (not Assigned(LArray)) or (not Assigned(LArray.ElementType))then
  begin
    ErrorNotEnoughRtti(ATypeInfo);
    Exit;
  end;

  { Check if the array is already serialized }
  if not Assigned(ADynArray) then
    WriteNilDynamicArrayReference()
  else if FDynArrayReg.TryGetValue(ADynArray, LArrayId) then
    WriteDynamicArrayReference(LArrayId)
  else begin
    { Register array in the dictionary }
    Inc(FLastId);
    FDynArrayReg.Add(ADynArray, LArrayId);

    { Obtain the count of elements }
    LCount := DynArraySize(ADynArray);

    { Notify that a class is being written }
    BeginWriteDynamicArray(LArray, LCount, FLastId);

    { Walk through the array }
    for LIndex := 0 to LCount - 1 do
      SerializeInternal(LArray.ElementType, Pointer(LIndex * LArray.ElementType.TypeSize + NativeInt(ADynArray)));

    EndWriteDynamicArray();
  end;
end;

procedure TSerializer.WriteRecord(const ATypeInfo: PTypeInfo; const ARefToRecord: Pointer);
var
  LRecord: TRttiRecordType;
  LField: TRttiField;
  LRecordId: Int32;
begin
  { ARefToRecord points to the first field in the record }
  ASSERT(ATypeInfo^.Kind = tkRecord);
  LRecord := FRttiContext.GetType(ATypeInfo) as TRttiRecordType;

  if not Assigned(LRecord) then
  begin
    ErrorNotEnoughRtti(ATypeInfo);
    Exit;
  end;

  { Check if the object is already serialized }
  if not Assigned(ARefToRecord) then
    WriteNilRecordReference()
  else if FRecordReg.TryGetValue(ARefToRecord, LRecordId) then
    WriteRecordReference(LRecordId)
  else begin
    { Register object in the dictionary }
    Inc(FLastId);
    FRecordReg.Add(ARefToRecord, FLastId);

    { Notify that a class is being written }
    BeginWriteRecord(LRecord, FLastId);

    { Walk through the record }
    for LField in LRecord.GetFields() do
    begin
      if not Assigned(LField.FieldType) then
      begin
        ErrorNoFieldRtti(LField);
        continue;
      end;

      if IsSerializable(LField) then
      begin
        BeginWriteField(LField);
        SerializeInternal(LField.FieldType, Pointer(LField.Offset + NativeInt(ARefToRecord)));
        EndWriteField();
      end;
    end;

    EndWriteRecord();
  end;
end;

procedure TSerializer.SerializeInternal(const AType: TRttiType; const AValueRef: Pointer);
var
  LTypeData: PTypeData;
  LSetOrd: Int64;
begin
  LTypeData := GetTypeData(AType.Handle);

  case AType.TypeKind of
    tkProcedure,
    tkUnknown,
    tkMethod,
    tkVariant,
    tkInterface:
      if not FSkipErrors then
        ErrorNotSupported(AType.Handle);

    tkEnumeration:
    begin
      LSetOrd := 0;
      case AType.TypeSize of
        1: LSetOrd := PInt8(AValueRef)^;
        2: LSetOrd := PInt16(AValueRef)^;
        4: LSetOrd := PInt32(AValueRef)^;
        8: LSetOrd := PInt64(AValueRef)^;
        else
          ASSERT(False);
      end;

      WriteEnum(LSetOrd);
    end;

    tkSet:
      WriteSet(AType.TypeSize, AValueRef^);

    tkInteger:
    begin
      if Assigned(LTypeData) then
      begin
        case LTypeData^.OrdType of
           otSByte:
             WriteInt8(PInt8(AValueRef)^);
           otUByte:
             WriteUInt8(PUInt8(AValueRef)^);
           otSWord:
             WriteInt16(PInt16(AValueRef)^);
           otUWord:
             WriteUInt16(PUInt16(AValueRef)^);
           otSLong:
             WriteInt32(PInt32(AValueRef)^);
           otULong:
             WriteUInt32(PUInt32(AValueRef)^);
        end;
      end else
        ErrorNotEnoughRtti(AType.Handle);
    end;

    tkInt64:
    begin
      if Assigned(LTypeData) then
      begin
        if LTypeData^.MaxInt64Value > LTypeData^.MinInt64Value then
          WriteInt64(PInt64(AValueRef)^)
        else
          WriteUInt64(PUInt64(AValueRef)^);
      end else
        ErrorNotEnoughRtti(AType.Handle);
    end;

    tkChar:
      WriteAnsiChar(PAnsiChar(AValueRef)^);

    tkWChar:
      WriteWideChar(PWideChar(AValueRef)^);

    tkFloat:
    begin
      if Assigned(LTypeData) then
      begin
        case LTypeData^.FloatType of
           ftSingle:
             WriteSingle(PSingle(AValueRef)^);
           ftDouble:
             WriteDouble(PDouble(AValueRef)^);
           ftExtended:
             WriteExtended(PExtended(AValueRef)^);
           ftComp:
             WriteComp(PComp(AValueRef)^);
           ftCurr:
             WriteCurrency(PCurrency(AValueRef)^);
        end;
      end else
        ErrorNotEnoughRtti(AType.Handle);
    end;

    tkString:
      WriteShortString(PShortString(AValueRef)^);
    tkLString:
      WriteAnsiString(PAnsiString(AValueRef)^);
    tkWString:
      WriteWideString(PWideString(AValueRef)^);
    tkUString:
      WriteUnicodeString(PUnicodeString(AValueRef)^);
    tkClassRef:
      WriteMetaClass(PPointer(AValueRef)^);
    tkRecord:
      WriteRecord(AType.Handle, AValueRef);
    tkClass:
      WriteClass(AType.Handle, PPointer(AValueRef)^);
    tkArray:
      WriteStaticArray(AType.Handle, AValueRef);
    tkDynArray:
      WriteDynamicArray(AType.Handle, PPointer(AValueRef)^);
    tkPointer:
    begin
      { Check if this is apointer to a record }
      if Assigned(LTypeData^.RefType) and Assigned(LTypeData^.RefType^) and (LTypeData^.RefType^^.Kind = tkRecord) then
        WriteRecord(LTypeData^.RefType^, PPointer(AValueRef)^)
      else
        ErrorNotSupported(AType.Handle);
    end;
  end;
end;

procedure TSerializer.WriteStaticArray(const ATypeInfo: PTypeInfo; const ARefToFirstElement: Pointer);
var
  LArray: TRttiArrayType;
  LIndex: NativeInt;
begin
  { ARefToRecord points to the first field in the record }
  ASSERT(ATypeInfo^.Kind = tkArray);
  LArray := FRttiContext.GetType(ATypeInfo) as TRttiArrayType;

  if (not Assigned(LArray)) or (not Assigned(LArray.ElementType)) then
  begin
    ErrorNotEnoughRtti(ATypeInfo);
    Exit;
  end;

  { Notify that a record is being written }
  BeginWriteStaticArray(LArray, LArray.TotalElementCount);

  { Walk through the record }
  for LIndex := 0 to LArray.TotalElementCount - 1 do
    SerializeInternal(LArray.ElementType, Pointer(LIndex * LArray.ElementType.TypeSize + NativeInt(ARefToFirstElement)));

  EndWriteStaticArray();
end;

{ TDeserializer }

class function TDeserializer.Default(const AStream: TStream): TDeserializer;
begin
  Result := TBinaryDeserializer.Create(AStream);
end;

constructor TDeserializer.Create;
begin
  FDynArrayReg := TDictionary<Int32, Pointer>.Create();
  FObjectReg := TDictionary<Int32, Pointer>.Create();
  FRecordReg := TDictionary<Int32, Pointer>.Create();

  FSkipErrors := true;
end;

function TDeserializer.CreateInstance(const AClassType: TRttiInstanceType): TObject;
var
  LMethod: TRttiMethod;
begin
  if Assigned(AClassType) then
  begin
    { Invoke the first parameterless constructor found. }
    for LMethod in AClassType.GetMethods() do
      if LMethod.HasExtendedInfo and LMethod.IsConstructor then
        if LMethod.GetParameters() = nil then
          Exit(LMethod.Invoke(AClassType.MetaclassType, []).AsObject);

    { Not found ... Use the old fashioned way }
    Result := AClassType.MetaclassType.Create();
  end else
    Result := nil;
end;

procedure TDeserializer.Deserialize(const ATypeInfo: PTypeInfo; out AValue);
begin
  if not Assigned(ATypeInfo) then
    ExceptionHelper.Throw_ArgumentNilError('ATypeInfo');

  { Call internal "un-checked" method. }
  BeginReadRoot();
  DeserializeInternal(FRttiContext.GetType(ATypeInfo), @AValue);
  EndReadRoot();
end;

function TDeserializer.Deserialize<T>: T;
begin
  Deserialize(TypeInfo(T), Result);
end;

function TDeserializer.Deserialize: TObject;
begin
  Deserialize(TypeInfo(TObject), Result);
end;

destructor TDeserializer.Destroy;
begin
  FDynArrayReg.Free;
  FObjectReg.Free;
  FRecordReg.Free;

  inherited;
end;

procedure TDeserializer.ErrorNoFieldRtti(const AField: TRttiField);
begin
  if not FSkipErrors then
    ExceptionHelper.Throw_FieldTypeDoesNotHaveEnoughRtti(AField);
end;

procedure TDeserializer.ErrorNotEnoughRtti(const ATypeInfo: PTypeInfo);
begin
  if not FSkipErrors then
    ExceptionHelper.Throw_TypeDoesNotHaveEnoughRtti(ATypeInfo);
end;

procedure TDeserializer.ErrorNotSupported(const ATypeInfo: PTypeInfo);
begin
  if not FSkipErrors then
    ExceptionHelper.Throw_TypeCannotBeSerialized(ATypeInfo);
end;

procedure TDeserializer.ReadClass(const ATypeInfo: PTypeInfo; out AObject: TObject);
var
  LClass: TRttiInstanceType;
  LField: TRttiField;
  LClassId: Int32;
  LClassType: TClass;
  LSerializable: ISerializable;
begin
  { ARefToRecord points to the first field in the record }
  ASSERT(ATypeInfo^.Kind = tkClass);
  LClass := FRttiContext.GetType(ATypeInfo) as TRttiInstanceType;

  if not Assigned(LClass) then
  begin
    ErrorNotEnoughRtti(ATypeInfo);
    Exit;
  end;

  case BeginReadClass(LClass, LClassType, LClassId) of
    rtInline:
    begin
      if Assigned(LClassType) then
        LClass := FRttiContext.GetType(LClassType.ClassInfo) as TRttiInstanceType;

      if not Assigned(LClass) then
        LClass := FRttiContext.GetType(ATypeInfo) as TRttiInstanceType;

      AObject := CreateInstance(LClass);
      FObjectReg.AddOrSetValue(LClassId, Pointer(AObject));
      try
        GetSafeInterface(AObject, ISerializable, Pointer(LSerializable));
        if Assigned(LSerializable) then
        begin
          LSerializable.Deserialize(TInputContext.Create(Self));
          Pointer(LSerializable) := nil;
        end else begin
          { Walk through the class }
          for LField in LClass.GetFields() do
          begin
            if not Assigned(LField.FieldType) then
            begin
              ErrorNoFieldRtti(LField);
              continue;
            end;

            if IsSerializable(LField) then
            begin
              BeginReadField(LField);
              DeserializeInternal(LField.FieldType, Pointer(LField.Offset + NativeInt(AObject)));
              EndReadField();
            end;
          end;
        end;

        EndReadClass();
      except
        AObject.Free;
        AObject := nil;
      end;
    end;

    rtPointer:
    begin
      if not FObjectReg.TryGetValue(LClassId, Pointer(AObject)) then
        ExceptionHelper.Throw_BadClassReference(ATypeInfo);
    end;

    rtNil:
      AObject := nil;
  end;


end;

procedure TDeserializer.ReadDynamicArray(const ATypeInfo: PTypeInfo; out ADynArray: Pointer);
var
  LArray: TRttiDynamicArrayType;
  LIndex, LCount: NativeInt;
  LArrayId: Int32;
  LOutPtr: Pointer;
  LDim: NativeInt;
begin
  { ARefToRecord points to the first field in the record }
  ASSERT(ATypeInfo^.Kind = tkDynArray);
  LArray := FRttiContext.GetType(ATypeInfo) as TRttiDynamicArrayType;

  if (not Assigned(LArray)) or (not Assigned(LArray.ElementType))then
  begin
    ErrorNotEnoughRtti(ATypeInfo);
    Exit;
  end;

  DynArrayClear(ADynArray, ATypeInfo);
  case BeginReadDynamicArray(LArray, LCount, LArrayId) of
    rtInline:
    begin
      LDim := LCount;
      DynArraySetLength(ADynArray, ATypeInfo, 1, @LDim);
      FDynArrayReg.AddOrSetValue(LArrayId, ADynArray);

      { Walk through the array }
      for LIndex := 0 to LCount - 1 do
        DeserializeInternal(LArray.ElementType, Pointer(LIndex * LArray.ElementType.TypeSize + NativeInt(ADynArray)));

      EndReadDynamicArray();
    end;

    rtPointer:
    begin
      if not FDynArrayReg.TryGetValue(LArrayId, LOutPtr) then
        ExceptionHelper.Throw_BadDynamicArrayReference(ATypeInfo)
      else
        TBytes(ADynArray) := TBytes(LOutPtr);
    end;

    rtNil:
      ADynArray := nil;
  end;
end;

procedure TDeserializer.ReadRecord(const ATypeInfo: PTypeInfo; var ARefToRecord: Pointer);
var
  LRecord: TRttiRecordType;
  LField: TRttiField;
  LRecordId: Int32;
  LHasAddr: Boolean;
begin
  { ARefToRecord points to the first field in the record }
  ASSERT(ATypeInfo^.Kind = tkRecord);
  LRecord := FRttiContext.GetType(ATypeInfo) as TRttiRecordType;

  if not Assigned(LRecord) then
  begin
    ErrorNotEnoughRtti(ATypeInfo);
    Exit;
  end;

  LHasAddr := Assigned(ARefToRecord);

  case BeginReadRecord(LRecord, LRecordId) of
    rtInline:
    begin
      if not LHasAddr then
      begin
        { Allocate enough memory for the value and initialize it }
        GetMem(ARefToRecord, LRecord.TypeSize);
        InitializeArray(ARefToRecord, LRecord.Handle, 1);
      end;

      FRecordReg.AddOrSetValue(LRecordId, ARefToRecord);

      try
        { Walk through the record }
        for LField in LRecord.GetFields() do
        begin
          if not Assigned(LField.FieldType) then
          begin
            ErrorNoFieldRtti(LField);
            continue;
          end;

          if IsSerializable(LField) then
          begin
            BeginReadField(LField);
            DeserializeInternal(LField.FieldType, Pointer(LField.Offset + NativeInt(ARefToRecord)));
            EndReadField();
          end;
        end;

        EndReadRecord();
      except
        if not LHasAddr then
        begin
          FinalizeArray(ARefToRecord, LRecord.Handle, 1);
          FreeMem(ARefToRecord);

          ARefToRecord := nil;
        end;
      end;
    end;

    rtPointer:
    begin
      if not FRecordReg.TryGetValue(LRecordId, ARefToRecord) then
        ExceptionHelper.Throw_BadRecordReference(ATypeInfo);
    end;

    rtNil:
      ARefToRecord := nil;
  end;


end;

procedure TDeserializer.ReadStaticArray(const ATypeInfo: PTypeInfo; const ARefToFirstElement: Pointer);
var
  LArray: TRttiArrayType;
  LIndex: NativeInt;
begin
  { ARefToRecord points to the first field in the record }
  ASSERT(ATypeInfo^.Kind = tkArray);
  LArray := FRttiContext.GetType(ATypeInfo) as TRttiArrayType;

  if (not Assigned(LArray)) or (not Assigned(LArray.ElementType)) then
  begin
    ErrorNotEnoughRtti(ATypeInfo);
    Exit;
  end;

  { Notify that a record is being written }
  BeginReadStaticArray(LArray, LArray.TotalElementCount);

  { Walk through the record }
  for LIndex := 0 to LArray.TotalElementCount - 1 do
    DeserializeInternal(LArray.ElementType, Pointer(LIndex * LArray.ElementType.TypeSize + NativeInt(ARefToFirstElement)));

  EndReadStaticArray();
end;

procedure TDeserializer.DeserializeInternal(const AType: TRttiType; const AValueRef: Pointer);
var
  LTypeData: PTypeData;
  LRecordRef: Pointer;
  LSetOrd: Int64;
begin
  LTypeData := GetTypeData(AType.Handle);

  case AType.TypeKind of
    tkProcedure,
    tkUnknown,
    tkMethod,
    tkVariant,
    tkInterface:
      if not FSkipErrors then
        ErrorNotSupported(AType.Handle);

    tkEnumeration:
    begin
      ReadEnum(LSetOrd);
      case AType.TypeSize of
        1: PInt8(AValueRef)^  := LSetOrd;
        2: PInt16(AValueRef)^ := LSetOrd;
        4: PInt32(AValueRef)^ := LSetOrd;
        8: PInt64(AValueRef)^ := LSetOrd;
        else
          ASSERT(False);
      end;
    end;

    tkSet:
      ReadSet(AType.TypeSize, AValueRef^);

    tkInteger:
    begin
      if Assigned(LTypeData) then
      begin
        case LTypeData^.OrdType of
           otSByte:
             ReadInt8(PInt8(AValueRef)^);
           otUByte:
             ReadUInt8(PUInt8(AValueRef)^);
           otSWord:
             ReadInt16(PInt16(AValueRef)^);
           otUWord:
             ReadUInt16(PUInt16(AValueRef)^);
           otSLong:
             ReadInt32(PInt32(AValueRef)^);
           otULong:
             ReadUInt32(PUInt32(AValueRef)^);
        end;
      end else
        ErrorNotEnoughRtti(AType.Handle);
    end;

    tkInt64:
    begin
      if Assigned(LTypeData) then
      begin
        if LTypeData^.MaxInt64Value > LTypeData^.MinInt64Value then
          ReadInt64(PInt64(AValueRef)^)
        else
          ReadUInt64(PUInt64(AValueRef)^);
      end else
        ErrorNotEnoughRtti(AType.Handle);
    end;

    tkChar:
      ReadAnsiChar(PAnsiChar(AValueRef)^);

    tkWChar:
      ReadWideChar(PWideChar(AValueRef)^);

    tkFloat:
    begin
      if Assigned(LTypeData) then
      begin
        case LTypeData^.FloatType of
           ftSingle:
             ReadSingle(PSingle(AValueRef)^);
           ftDouble:
             ReadDouble(PDouble(AValueRef)^);
           ftExtended:
             ReadExtended(PExtended(AValueRef)^);
           ftComp:
             ReadComp(PComp(AValueRef)^);
           ftCurr:
             ReadCurrency(PCurrency(AValueRef)^);
        end;
      end else
        ErrorNotEnoughRtti(AType.Handle);
    end;

    tkString:
      ReadShortString(PShortString(AValueRef)^);
    tkLString:
      ReadAnsiString(PAnsiString(AValueRef)^);
    tkWString:
      ReadWideString(PWideString(AValueRef)^);
    tkUString:
      ReadUnicodeString(PUnicodeString(AValueRef)^);
    tkClassRef:
      ReadMetaClass(PClass(AValueRef)^);
    tkRecord:
    begin
      LRecordRef := AValueRef;
      ReadRecord(AType.Handle, LRecordRef);
    end;
    tkClass:
      ReadClass(AType.Handle, PObject(AValueRef)^);
    tkArray:
      ReadStaticArray(AType.Handle, AValueRef);
    tkDynArray:
      ReadDynamicArray(AType.Handle, PPointer(AValueRef)^);
    tkPointer:
    begin
      PPointer(AValueRef)^ := nil;
      { Check if this is apointer to a record }
      if Assigned(LTypeData^.RefType) and Assigned(LTypeData^.RefType^) and (LTypeData^.RefType^^.Kind = tkRecord) then
        ReadRecord(LTypeData^.RefType^, PPointer(AValueRef)^)
      else
        ErrorNotSupported(AType.Handle);
    end;
  end;
end;

{ TOutputContext }

function TOutputContext.AddValue(const AName: String; const AValue: Int64): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteInt64(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: UInt32): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteUInt32(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: UInt64): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteUInt64(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: WideChar): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteWideChar(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: AnsiChar): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteAnsiChar(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: UInt8): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteUInt8(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: Int8): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteInt8(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: Int16): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteInt16(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: Int32): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteInt32(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: UInt16): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteUInt16(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: WideString): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteWideString(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: AnsiString): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteAnsiString(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: UnicodeString): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteUnicodeString(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: TObject): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteClass(TypeInfo(TObject), AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: TClass): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteMetaClass(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: ShortString): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteShortString(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: Double): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteDouble(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: Single): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteSingle(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: Extended): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteExtended(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: Currency): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteCurrency(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue(const AName: String; const AValue: Comp): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.WriteComp(AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

function TOutputContext.AddValue<T>(const AName: String; const AValue: T): TOutputContext;
begin
  { Write data }
  FSerializer.BeginWriteField(AName);
  FSerializer.SerializeInternal(
    FSerializer.RttiContext.GetType(TypeInfo(T)), @AValue);
  FSerializer.EndWriteField();

  { Return self for chaining }
  Result := Self;
end;

class function TOutputContext.Create(const ASerializer: TSerializer): TOutputContext;
begin
  Result.FSerializer := ASerializer;
end;

{ TInputContext }


class function TInputContext.Create(const ADeserializer: TDeserializer): TInputContext;
begin
  Result.FDeserializer := ADeserializer;
end;

function TInputContext.GetValue(const AName: String; out AValue: Int64): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadInt64(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: UInt32): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadUInt32(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: UInt64): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadUInt64(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: WideChar): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadWideChar(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: AnsiChar): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadAnsiChar(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: UInt8): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadUInt8(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: Int8): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadInt8(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: Int16): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadInt16(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: Int32): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadInt32(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: UInt16): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadUInt16(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: Single): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadSingle(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: WideString): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadWideString(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: AnsiString): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadAnsiString(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: UnicodeString): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadUnicodeString(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: TObject): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadClass(TypeInfo(TObject), AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: TClass): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadMetaClass(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: Extended): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadExtended(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: Double): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadDouble(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: Comp): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadComp(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: ShortString): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadShortString(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue(const AName: String; out AValue: Currency): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.ReadCurrency(AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

function TInputContext.GetValue<T>(const AName: String; out AValue: T): TInputContext;
begin
  { Read data }
  FDeserializer.BeginReadField(AName);
  FDeserializer.DeserializeInternal(
    FDeserializer.RttiContext.GetType(TypeInfo(T)), @AValue);
  FDeserializer.EndReadField();

  { Return self for chaining }
  Result := Self;
end;

end.
