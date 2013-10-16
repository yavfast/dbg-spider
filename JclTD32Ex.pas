{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclTD32.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Flier Lu (<flier_lu att yahoo dott com dott cn>).  }
{ Portions created by Flier Lu are Copyright (C) Flier Lu.  All Rights Reserved.                   }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Flier Lu (flier)                                                                               }
{   Olivier Sannier (obones)                                                                       }
{   Petr Vones (pvones)                                                                            }
{   Heinz Zastrau (heinzz)                                                                         }
{   Andreas Hausladen (ahuser)                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Borland TD32 symbolic debugging information support routines and classes.                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2011-09-03 00:07:50 +0200 (sam. 03 sept. 2011)                          $ }
{ Revision:      $Rev:: 3599                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclTD32Ex;

interface

{$I jcl.inc}
{$I windowsonly.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  System.Classes, System.SysUtils, System.Contnrs,
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils, Contnrs,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase,
  {$IFDEF BORLAND}
  JclPeImage,
  {$ENDIF BORLAND}
  JclFileUtils;

{ TODO -cDOC : Original code: "Flier Lu" <flier_lu att yahoo dott com dott cn> }

// TD32 constants and structures
{*******************************************************************************

  [-----------------------------------------------------------------------]
  [         Symbol and Type OMF Format Borland Executable Files           ]
  [-----------------------------------------------------------------------]

  Introduction

  This section describes the format used to embed debugging information  into
  the executable file.

  Debug Information Format

  The format encompasses a block of  data which goes at  the end of the  .EXE
  file,  i.e.,  after   the   header   plus   load   image,   overlays,   and
  Windows/Presentation Manager  resource  compiler  information.   The  lower
  portion of the file is unaffected by the additional data.

  The last eight bytes of the file contain a signature and a long file offset
  from the end of the file (lfoBase).  The signature is FBxx, where xx is the
  version number.  The  long  offset  indicates  the  position  in  the  file
  (relative to the end of the file)  of the base address.  For the LX  format
  executables, the base address  is determined by  looking at the  executable
  header.

  The signatures have the following meanings:

    FB09        The signature for a Borland 32 bit symbol file.

  The value

          lfaBase=length of the file - lfoBase

  gives the base address of the start of the Symbol and Type OMF  information
  relative to  the beginning  of the  file.  All  other file  offsets in  the
  Symbol and Type OMF are relative to  the lfaBase.  At the base address  the
  signature is repeated, followed by the long displacement to the  subsection
  directory (lfoDir).  All subsections start on a long word boundary and  are
  designed to maintain  natural alignment internally  in each subsection  and
  within the subsection directory.

  Subsection Directory

  The subsection directory has the format

       Directory header

       Directory entry 0

       Directory entry 1

        .
        .
        .

       Directory entry n

  There is no requirement for a particular subsection of a particular module to exist.

  The following is the layout of the FB09 debug information in the image:

  FB09 Header

    sstModule [1]
    .
    .
    .
    sstModule [n]

    sstAlignSym [1]
    sstSrcModule [1]
    .
    .
    .
    sstAlignSym [n]
    sstSrcModule [n]

    sstGlobalSym
    sstGlobalTypes
    sstNames

    SubSection Directory

  FB09 Trailer

*******************************************************************************}

const
  Borland32BitSymbolFileSignatureForDelphi = $39304246; // 'FB09'
  Borland32BitSymbolFileSignatureForBCB    = $41304246; // 'FB0A'

type
  { Signature structure }
  PJclTD32FileSignature = ^TJclTD32FileSignature;
  TJclTD32FileSignature = packed record
    Signature: DWORD;
    Offset: DWORD;
  end;

const
  { Subsection Types }
  SUBSECTION_TYPE_MODULE         = $120;
  SUBSECTION_TYPE_TYPES          = $121;
  SUBSECTION_TYPE_SYMBOLS        = $124;
  SUBSECTION_TYPE_ALIGN_SYMBOLS  = $125;
  SUBSECTION_TYPE_SOURCE_MODULE  = $127;
  SUBSECTION_TYPE_GLOBAL_SYMBOLS = $129;
  SUBSECTION_TYPE_GLOBAL_TYPES   = $12B;
  SUBSECTION_TYPE_NAMES          = $130;

type
  { Subsection directory header structure }
  { The directory header structure is followed by the directory entries
    which specify the subsection type, module index, file offset, and size.
    The subsection directory gives the location (LFO) and size of each subsection,
    as well as its type and module number if applicable. }
  PDirectoryEntry = ^TDirectoryEntry;
  TDirectoryEntry = packed record
    SubsectionType: Word; // Subdirectory type
    ModuleIndex: Word;    // Module index
    Offset: DWORD;        // Offset from the base offset lfoBase
    Size: DWORD;          // Number of bytes in subsection
  end;

  { The subsection directory is prefixed with a directory header structure
    indicating size and number of subsection directory entries that follow. }
  PDirectoryHeader = ^TDirectoryHeader;
  TDirectoryHeader = packed record
    Size: Word;           // Length of this structure
    DirEntrySize: Word;   // Length of each directory entry
    DirEntryCount: DWORD; // Number of directory entries
    lfoNextDir: DWORD;    // Offset from lfoBase of next directory.
    Flags: DWORD;         // Flags describing directory and subsection tables.
    DirEntries: array [0..0] of TDirectoryEntry;
  end;


{*******************************************************************************

  SUBSECTION_TYPE_MODULE $120

  This describes the basic information about an object module including  code
  segments, module name,  and the  number of  segments for  the modules  that
  follow.  Directory entries for  sstModules  precede  all  other  subsection
  directory entries.

*******************************************************************************}

type
  PSegmentInfo = ^TSegmentInfo;
  TSegmentInfo = packed record
    Segment: Word; // Segment that this structure describes
    Flags: Word;   // Attributes for the logical segment.
                   // The following attributes are defined:
                   //   $0000  Data segment
                   //   $0001  Code segment
    Offset: DWORD; // Offset in segment where the code starts
    Size: DWORD;   // Count of the number of bytes of code in the segment
  end;

  PSegmentInfoArray = ^TSegmentInfoArray;
  TSegmentInfoArray = array [0..32767] of TSegmentInfo;

  PModuleInfo = ^TModuleInfo;
  TModuleInfo = packed record
    OverlayNumber: Word;  // Overlay number
    LibraryIndex: Word;   // Index into sstLibraries subsection
                          // if this module was linked from a library
    SegmentCount: Word;   // Count of the number of code segments
                          // this module contributes to
    DebuggingStyle: Word; // Debugging style  for this  module.
    NameIndex: DWORD;     // Name index of module.
    TimeStamp: DWORD;     // Time stamp from the OBJ file.
    Reserved: array [0..2] of DWORD; // Set to 0.
    Segments: array [0..0] of TSegmentInfo;
                          // Detailed information about each segment
                          // that code is contributed to.
                          // This is an array of cSeg count segment
                          // information descriptor structures.
  end;

{*******************************************************************************

  SUBSECTION_TYPE_SOURCE_MODULE $0127

  This table describes the source line number to addressing mapping
  information for a module. The table permits the description of a module
  containing multiple source files with each source file contributing code to
  one or more code segments. The base addresses of the tables described
  below are all relative to the beginning of the sstSrcModule table.


  Module header

  Information for source file 1

    Information for segment 1
         .
         .
         .
    Information for segment n

         .
         .
         .

  Information for source file n

    Information for segment 1
         .
         .
         .
    Information for segment n

*******************************************************************************}
type
  { The line number to address mapping information is contained in a table with
    the following format: }
  PLineMappingEntry = ^TLineMappingEntry;
  TLineMappingEntry = packed record
    SegmentIndex: Word;  // Segment index for this table
    PairCount: Word;     // Count of the number of source line pairs to follow
    Offsets: array [0..0] of DWORD;
                     // An array of 32-bit offsets for the offset
                     // within the code segment ofthe start of ine contained
                     // in the parallel array linenumber.
    (*
    { This is an array of 16-bit line numbers of the lines in the source file
      that cause code to be emitted to the code segment.
      This array is parallel to the offset array.
      If cPair is not even, then a zero word is emitted to
      maintain natural alignment in the sstSrcModule table. }
    LineNumbers: array [0..PairCount - 1] of Word;
    *)
  end;

  TOffsetPair = packed record
    StartOffset: DWORD;
    EndOffset: DWORD;
  end;
  POffsetPairArray = ^TOffsetPairArray;
  TOffsetPairArray = array [0..32767] of TOffsetPair;

  { The file table describes the code segments that receive code from this
    source file. Source file entries have the following format: }
  PSourceFileEntry = ^TSourceFileEntry;
  TSourceFileEntry = packed record
    SegmentCount: Word; // Number of segments that receive code from this source file.
    NameIndex: DWORD;   // Name index of Source file name.

    BaseSrcLines: array [0..0] of DWORD;
                        // An array of offsets for the line/address mapping
                        // tables for each of the segments that receive code
                        // from this source file.
    (*
    { An array  of two  32-bit offsets  per segment  that
      receives code from this  module.  The first  offset
      is the offset within the segment of the first  byte
      of code from this module.  The second offset is the
      ending address of the  code from this module.   The
      order of these pairs corresponds to the ordering of
      the segments in the  seg  array.   Zeros  in  these
      entries means that the information is not known and
      the file and line tables described below need to be
      examined to determine if an address of interest  is
      contained within the code from this module. }
    SegmentAddress: array [0..SegmentCount - 1] of TOffsetPair;

    Name: ShortString; // Count of the number of bytes in source file name
    *)
  end;

  { The module header structure describes the source file and code segment
    organization of the module. Each module header has the following format: }
  PSourceModuleInfo = ^TSourceModuleInfo;
  TSourceModuleInfo = packed record
    FileCount: Word;    // The number of source file scontributing code to segments
    SegmentCount: Word; // The number of code segments receiving code from this module

    BaseSrcFiles: array [0..0] of DWORD;
    (*
    // This is an array of base offsets from the beginning of the sstSrcModule table
    BaseSrcFiles: array [0..FileCount - 1] of DWORD;

    { An array  of two  32-bit offsets  per segment  that
      receives code from this  module.  The first  offset
      is the offset within the segment of the first  byte
      of code from this module.  The second offset is the
      ending address of the  code from this module.   The
      order of these pairs corresponds to the ordering of
      the segments in the  seg  array.   Zeros  in  these
      entries means that the information is not known and
      the file and line tables described below need to be
      examined to determine if an address of interest  is
      contained within the code from this module. }
    SegmentAddress: array [0..SegmentCount - 1] of TOffsetPair;

    { An array of segment indices that receive code  from
      this module.  If the  number  of  segments  is  not
      even, a pad word  is inserted  to maintain  natural
      alignment. }
    SegmentIndexes: array [0..SegmentCount - 1] of Word;
    *)
  end;

{*******************************************************************************

  SUBSECTION_TYPE_GLOBAL_TYPES $12b

  This subsection contains the packed type records for the executable file.
  The first long word of the subsection contains the number of types in the
  table. This count is followed by a count-sized array of long offsets to the
  corresponding type record. As the sstGlobalTypes subsection is written, each
  type record is forced to start on a long word boundary. However, the length
  of the type string is not adjusted by the pad count. The remainder of the
  subsection contains the type records. This table is invalid for NB05
  signatures.

  Note that for NB07 and NB08 executables, the type string offset is from the
  beginning of the subsection table. For NB09 executables, the type string
  offset is from the first type record of the sstGlobalTypes subsection. Using
  the offset from the first type record simplifies demand loading of the
  sstGlobalTypes table.

*******************************************************************************}

type
  PGlobalTypeInfo = ^TGlobalTypeInfo;
  TGlobalTypeInfo = packed record
    Unused: array[0..2] of Byte;    // Reserved for future use. Must be emitted as zeroes
    Signature: Byte;                // Global types table signature
    Count: DWORD;                   // Count or number of types
    Offsets: array [0..0] of DWORD; // Offset of each type. See note above
  end;

const
  { Symbol type defines }
  SYMBOL_TYPE_COMPILE        = $0001; // Compile flags symbol
  SYMBOL_TYPE_REGISTER       = $0002; // Register variable
  SYMBOL_TYPE_CONST          = $0003; // Constant symbol
  SYMBOL_TYPE_UDT            = $0004; // User-defined Type
  SYMBOL_TYPE_SSEARCH        = $0005; // Start search
  SYMBOL_TYPE_END            = $0006; // End block, procedure, with, or thunk
  SYMBOL_TYPE_SKIP           = $0007; // Skip - Reserve symbol space
  SYMBOL_TYPE_CVRESERVE      = $0008; // Reserved for Code View internal use
  SYMBOL_TYPE_OBJNAME        = $0009; // Specify name of object file

  SYMBOL_TYPE_GPROCREF       = $0020;
  SYMBOL_TYPE_GDATAREF       = $0021;
  SYMBOL_TYPE_EDATA          = $0022;
  SYMBOL_TYPE_EPROC          = $0023;
  SYMBOL_TYPE_USES           = $0024;
  SYMBOL_TYPE_NAMESPACE      = $0025;
  SYMBOL_TYPE_USING          = $0026;
  SYMBOL_TYPE_PCONSTANT      = $0027;

  SYMBOL_TYPE_BPREL16        = $0100; // BP relative 16:16
  SYMBOL_TYPE_LDATA16        = $0101; // Local data 16:16
  SYMBOL_TYPE_GDATA16        = $0102; // Global data 16:16
  SYMBOL_TYPE_PUB16          = $0103; // Public symbol 16:16
  SYMBOL_TYPE_LPROC16        = $0104; // Local procedure start 16:16
  SYMBOL_TYPE_GPROC16        = $0105; // Global procedure start 16:16
  SYMBOL_TYPE_THUNK16        = $0106; // Thunk start 16:16
  SYMBOL_TYPE_BLOCK16        = $0107; // Block start 16:16
  SYMBOL_TYPE_WITH16         = $0108; // With start 16:16
  SYMBOL_TYPE_LABEL16        = $0109; // Code label 16:16
  SYMBOL_TYPE_CEXMODEL16     = $010A; // Change execution model 16:16
  SYMBOL_TYPE_VFTPATH16      = $010B; // Virtual function table path descriptor 16:16

  SYMBOL_TYPE_BPREL32        = $0200; // BP relative 16:32
  SYMBOL_TYPE_LDATA32        = $0201; // Local data 16:32
  SYMBOL_TYPE_GDATA32        = $0202; // Global data 16:32
  SYMBOL_TYPE_PUB32          = $0203; // Public symbol 16:32
  SYMBOL_TYPE_LPROC32        = $0204; // Local procedure start 16:32
  SYMBOL_TYPE_GPROC32        = $0205; // Global procedure start 16:32
  SYMBOL_TYPE_THUNK32        = $0206; // Thunk start 16:32
  SYMBOL_TYPE_BLOCK32        = $0207; // Block start 16:32
  SYMBOL_TYPE_WITH32         = $0208; // With start 16:32
  SYMBOL_TYPE_LABEL32        = $0209; // Label 16:32
  SYMBOL_TYPE_CEXMODEL32     = $020A; // Change execution model 16:32
  SYMBOL_TYPE_VFTPATH32      = $020B; // Virtual function table path descriptor 16:32

  SYMBOL_TYPE_ENTRY32        = $0210;
  SYMBOL_TYPE_OPTVAR32       = $0211;
  SYMBOL_TYPE_PROCRET32      = $0212;
  SYMBOL_TYPE_SAVREGS32      = $0213;
  SYMBOL_TYPE_SLINK32        = $0230;

{*******************************************************************************

  Global and Local Procedure Start 16:32

  SYMBOL_TYPE_LPROC32 $0204
  SYMBOL_TYPE_GPROC32 $0205

    The symbol records define local (file static) and global procedure
  definition. For C/C++, functions that are declared static to a module are
  emitted as Local Procedure symbols. Functions not specifically declared
  static are emitted as Global Procedures.
    For each SYMBOL_TYPE_GPROC32 emitted, an SYMBOL_TYPE_GPROCREF symbol
  must be fabricated and emitted to the SUBSECTION_TYPE_GLOBAL_SYMBOLS section.

*******************************************************************************}

type
  TSymbolProcInfo = packed record
    pParent: DWORD;
    pEnd: DWORD;
    pNext: DWORD;
    Size: DWORD;        // Length in bytes of this procedure
    DebugStart: DWORD;  // Offset in bytes from the start of the procedure to
                        // the point where the stack frame has been set up.
    DebugEnd: DWORD;    // Offset in bytes from the start of the procedure to
                        // the point where the  procedure is  ready to  return
                        // and has calculated its return value, if any.
                        // Frame and register variables an still be viewed.
    Offset: DWORD;      // Offset portion of  the segmented address of
                        // the start of the procedure in the code segment
    Segment: Word;      // Segment portion of the segmented address of
                        // the start of the procedure in the code segment
    Reserved1: Word;
    TypeIndex: DWORD;   // Type of the procedure type record
    NameIndex: DWORD;   // Name index of procedure
    Reserved2: DWORD;
  end;

  TSymbolObjNameInfo = packed record
    Signature: DWORD;   // Signature for the CodeView information contained in
                        // this module
    NameIndex: DWORD;   // Name index of the object file
  end;

  TSymbolDataInfo = packed record
    Offset: DWORD;      // Offset portion of  the segmented address of
                        // the start of the data in the code segment
    Segment: Word;      // Segment portion of the segmented address of
                        // the start of the data in the code segment
    Reserved1: Word;
    TypeIndex: DWORD;   // Type index of the symbol
    NameIndex: DWORD;   // Name index of the symbol
    Reserved2: DWORD;
  end;

  TSymbolWithInfo = packed record
    pParent: DWORD;
    Size: DWORD;        // Length in bytes of this "with"
    Offset: DWORD;      // Offset portion of the segmented address of
                        // the start of the "with" in the code segment
    Segment: Word;      // Segment portion of the segmented address of
                        // the start of the "with" in the code segment
    Reserved1: Word;
    TypeIndex: DWORD;
    NameIndex: DWORD;   // Name index of the "with"
    Reserved2: DWORD;
  end;

  TSymbolLabelInfo = packed record
    Offset: DWORD;      // Offset portion of  the segmented address of
                        // the start of the label in the code segment
    Segment: Word;      // Segment portion of the segmented address of
                        // the start of the label in the code segment
    NearFar: Byte;      // Address mode of the label:
                        //   0       near
                        //   4       far
    Reserved: Byte;
    NameIndex: DWORD;   // Name index of the label
  end;

  TSymbolConstantInfo = packed record
    TypeIndex: DWORD;
    Flags: Word;
    NameIndex: DWORD;
    Reserved: DWORD;
    Value: array[0..0] of Byte;
  end;

  TSymbolUdtInfo = packed record
    TypeIndex: DWORD;   // Type index of the type
    Properties: Word;   // isTag:1 True if this is a tag (not a typedef)
                        // isNest:1 True if the type is a nested type (its name
                        // will be 'class_name::type_name' in that case)
    NameIndex: DWORD;   // Name index of the type
    Reserved: DWORD;
  end;

  TSymbolVftPathInfo = packed record
    Offset: DWORD;      // Offset portion of start of the virtual function table
    Segment: Word;      // Segment portion of the virtual function table
    Reserved: Word;
    RootIndex: DWORD;   // The type index of the class at the root of the path
    PathIndex: DWORD;   // Type index of the record describing the base class
                        // path from the root to the leaf class for the virtual
                        // function table
  end;

  TSymbolBPRelInfo = packed record
    Offset: Integer;
    TypeIndex: DWORD;
    NameIndex: DWORD;
    Reserved: DWORD;
  end;

  TSymbolStartInfo = packed record
    Offset: DWORD;
    Segment: Word;
    CodeCount: Word;
    DataCount: Word;
    FirstData: DWORD;
    Reserved: Word;
  end;

  TSymbolLinkInfo = packed record
    Offset: DWORD;
  end;

  TSymbolUsesInfo = packed record
    Names: array[0..0] of DWORD;
  end;

  TRegisterInfo = packed record
    TypeIndex: DWORD;
    Registers: Word;
    NameIndex: DWORD;
    Reserved: DWORD;
  end;

  PRegisterRange = ^TRegisterRange;
  TRegisterRange = packed record
    Start: DWORD;
    Len: DWORD;
    Registers: Word;
  end;

  TOptVarInfo = packed record
    Count: Word;
    Ranges: array[0..0] of TRegisterRange;
  end;

type
  { Symbol Information Records }
  PSymbolInfo = ^TSymbolInfo;
  TSymbolInfo = packed record
    Size: Word;
    SymbolType: Word;
    case Word of
      SYMBOL_TYPE_LPROC32, SYMBOL_TYPE_GPROC32:
        (Proc: TSymbolProcInfo);
      SYMBOL_TYPE_OBJNAME:
        (ObjName: TSymbolObjNameInfo);
      SYMBOL_TYPE_LDATA32, SYMBOL_TYPE_GDATA32, SYMBOL_TYPE_PUB32:
        (Data: TSymbolDataInfo);
      SYMBOL_TYPE_WITH32:
        (With32: TSymbolWithInfo);
      SYMBOL_TYPE_LABEL32:
        (Label32: TSymbolLabelInfo);
      SYMBOL_TYPE_CONST, SYMBOL_TYPE_PCONSTANT:
        (Constant: TSymbolConstantInfo);
      SYMBOL_TYPE_UDT:
        (Udt: TSymbolUdtInfo);
      SYMBOL_TYPE_VFTPATH32:
        (VftPath: TSymbolVftPathInfo);
      SYMBOL_TYPE_REGISTER:
        (Registers: TRegisterInfo);
      SYMBOL_TYPE_SSEARCH:
        (Start: TSymbolStartInfo);
      SYMBOL_TYPE_USES:
        (Use: TSymbolUsesInfo);
      SYMBOL_TYPE_BPREL32:
        (BPRel: TSymbolBPRelInfo);
      SYMBOL_TYPE_SLINK32:
        (Link: TSymbolLinkInfo);
      SYMBOL_TYPE_OPTVAR32:
        (OptVar: TOptVarInfo);
  end;

  PSymbolInfos = ^TSymbolInfos;
  TSymbolInfos = packed record
    Signature: DWORD;
    Symbols: array [0..0] of TSymbolInfo;
  end;

{$IFDEF SUPPORTS_EXTSYM}

{$EXTERNALSYM Borland32BitSymbolFileSignatureForDelphi}
{$EXTERNALSYM Borland32BitSymbolFileSignatureForBCB}

{$EXTERNALSYM SUBSECTION_TYPE_MODULE}
{$EXTERNALSYM SUBSECTION_TYPE_TYPES}
{$EXTERNALSYM SUBSECTION_TYPE_SYMBOLS}
{$EXTERNALSYM SUBSECTION_TYPE_ALIGN_SYMBOLS}
{$EXTERNALSYM SUBSECTION_TYPE_SOURCE_MODULE}
{$EXTERNALSYM SUBSECTION_TYPE_GLOBAL_SYMBOLS}
{$EXTERNALSYM SUBSECTION_TYPE_GLOBAL_TYPES}
{$EXTERNALSYM SUBSECTION_TYPE_NAMES}

{$EXTERNALSYM SYMBOL_TYPE_COMPILE}
{$EXTERNALSYM SYMBOL_TYPE_REGISTER}
{$EXTERNALSYM SYMBOL_TYPE_CONST}
{$EXTERNALSYM SYMBOL_TYPE_UDT}
{$EXTERNALSYM SYMBOL_TYPE_SSEARCH}
{$EXTERNALSYM SYMBOL_TYPE_END}
{$EXTERNALSYM SYMBOL_TYPE_SKIP}
{$EXTERNALSYM SYMBOL_TYPE_CVRESERVE}
{$EXTERNALSYM SYMBOL_TYPE_OBJNAME}

{$EXTERNALSYM SYMBOL_TYPE_BPREL16}
{$EXTERNALSYM SYMBOL_TYPE_LDATA16}
{$EXTERNALSYM SYMBOL_TYPE_GDATA16}
{$EXTERNALSYM SYMBOL_TYPE_PUB16}
{$EXTERNALSYM SYMBOL_TYPE_LPROC16}
{$EXTERNALSYM SYMBOL_TYPE_GPROC16}
{$EXTERNALSYM SYMBOL_TYPE_THUNK16}
{$EXTERNALSYM SYMBOL_TYPE_BLOCK16}
{$EXTERNALSYM SYMBOL_TYPE_WITH16}
{$EXTERNALSYM SYMBOL_TYPE_LABEL16}
{$EXTERNALSYM SYMBOL_TYPE_CEXMODEL16}
{$EXTERNALSYM SYMBOL_TYPE_VFTPATH16}

{$EXTERNALSYM SYMBOL_TYPE_BPREL32}
{$EXTERNALSYM SYMBOL_TYPE_LDATA32}
{$EXTERNALSYM SYMBOL_TYPE_GDATA32}
{$EXTERNALSYM SYMBOL_TYPE_PUB32}
{$EXTERNALSYM SYMBOL_TYPE_LPROC32}
{$EXTERNALSYM SYMBOL_TYPE_GPROC32}
{$EXTERNALSYM SYMBOL_TYPE_THUNK32}
{$EXTERNALSYM SYMBOL_TYPE_BLOCK32}
{$EXTERNALSYM SYMBOL_TYPE_WITH32}
{$EXTERNALSYM SYMBOL_TYPE_LABEL32}
{$EXTERNALSYM SYMBOL_TYPE_CEXMODEL32}
{$EXTERNALSYM SYMBOL_TYPE_VFTPATH32}

{$ENDIF SUPPORTS_EXTSYM}

// TD32 information related classes
type
  TJclTD32SourceModuleInfo = class;
  TJclTD32SymbolInfo = class;
  TJclTD32ProcSymbolInfo = class;

  TJclTD32ModuleInfo = class(TObject)
  private
    FNameIndex: DWORD;
    FSegments: PSegmentInfoArray;
    FSegmentCount: Integer;
    FSourceModules: TList;
    FSymbols: TList;
    FProcSymbols: TList;
    FUsedModuleNameIndices: TList;
    function GetSegment(const Idx: Integer): TSegmentInfo;
    function GetSourceModule(const Idx: Integer): TJclTD32SourceModuleInfo;
    function GetSourceModuleCount: Integer;
    function GetSymbol(const Idx: Integer): TJclTD32SymbolInfo;
    function GetSymbolCount: Integer;
    function GetProcSymbol(const Idx: Integer): TJclTD32ProcSymbolInfo;
    function GetProcSymbolCount: Integer;
    function GetUsedModuleNameIndex(const Idx: Integer): Integer;
    function GetUsedModuleNameIndexCount: Integer;
  protected
    constructor Create(pModInfo: PModuleInfo);
  public
    destructor Destroy; override;
    function FindProc(Offset: DWORD): TJclTD32ProcSymbolInfo;
    property NameIndex: DWORD read FNameIndex;
    property SegmentCount: Integer read FSegmentCount; //GetSegmentCount;
    property Segment[const Idx: Integer]: TSegmentInfo read GetSegment; default;
    property SourceModules[const Idx: Integer]: TJclTD32SourceModuleInfo read GetSourceModule;
    property SourceModuleCount: Integer read GetSourceModuleCount;
    property Symbols[const Idx: Integer]: TJclTD32SymbolInfo read GetSymbol;
    property SymbolCount: Integer read GetSymbolCount;
    property ProcSymbols[const Idx: Integer]: TJclTD32ProcSymbolInfo read GetProcSymbol;
    property ProcSymbolCount: Integer read GetProcSymbolCount;
    property UsedModuleNameIndices[const Idx: Integer]: Integer read GetUsedModuleNameIndex;
    property UsedModuleNameIndexCount: Integer read GetUsedModuleNameIndexCount;
  end;

  TJclTD32LineInfo = class(TObject)
  private
    FLineNo: DWORD;
    FOffset: DWORD;
    FSegment: Word;
  public
    constructor Create(const ALineNo, AOffset: DWORD; const ASegment: Word);

    property LineNo: DWORD read FLineNo;
    property Offset: DWORD read FOffset;
    property Segment: Word read FSegment;
  end;

  TJclTD32SourceModuleInfo = class(TObject)
  private
    FLines: TObjectList;
    FSegments: POffsetPairArray;
    FSegmentCount: Integer;
    FNameIndex: DWORD;
    function GetLine(const Idx: Integer): TJclTD32LineInfo;
    function GetLineCount: Integer;
    function GetSegment(const Idx: Integer): TOffsetPair;
  protected
    constructor Create(pSrcFile: PSourceFileEntry; const Base: DWORD);
  public
    destructor Destroy; override;
    function FindLine(const AAddr: DWORD; var ALine: TJclTD32LineInfo): Boolean;
    property NameIndex: DWORD read FNameIndex;
    property LineCount: Integer read GetLineCount;
    property Line[const Idx: Integer]: TJclTD32LineInfo read GetLine; default;
    property SegmentCount: Integer read FSegmentCount; //GetSegmentCount;
    property Segment[const Idx: Integer]: TOffsetPair read GetSegment;
  end;

  TJclTD32SymbolInfo = class(TObject)
  private
    FID: DWORD;
    FParentID: DWORD;
    FParent: TJclTD32SymbolInfo;
    FSymbolType: Word;
  public
    constructor Create(pSymInfo: PSymbolInfo; const aID: DWORD); virtual;
    property ID: DWORD read FID;
    property ParentID: DWORD read FParentID;
    property Parent: TJclTD32SymbolInfo read FParent;
    property SymbolType: Word read FSymbolType;
  end;

  TJclTD32NamedSymbol = class(TJclTD32SymbolInfo)
  private
    FNameIndex: DWORD;
    FTypeIndex: DWORD;
  public
    property NameIndex: DWORD read FNameIndex;
    property TypeIndex: DWORD read FTypeIndex;
  end;

  TJclTD32Scope = class(TJclTD32NamedSymbol)
  private
    FSegment: Word;
    FOffset: DWORD;
    FSize: DWORD;
  public
    property Segment: Word read FSegment;
    property Offset: DWORD read FOffset;
    property Size: DWORD read FSize;
  end;

  TJclTD32ProcSymbolInfo = class(TJclTD32Scope)
  private
    FDebugStart: DWORD;
    FDebugEnd: DWORD;
    FSymbols: TList;
    function GetSymbol(const Idx: Integer): TJclTD32SymbolInfo;
    function GetSymbolCount: Integer;
  public
    constructor Create(pSymInfo: PSymbolInfo; const aID: DWORD); override;
    destructor Destroy; override;
    property DebugStart: DWORD read FDebugStart;
    property DebugEnd: DWORD read FDebugEnd;
    property Symbols[const Idx: Integer]: TJclTD32SymbolInfo read GetSymbol;
    property SymbolCount: Integer read GetSymbolCount;
  end;

  TJclTD32LocalProcSymbolInfo = class(TJclTD32ProcSymbolInfo);
  TJclTD32GlobalProcSymbolInfo = class(TJclTD32ProcSymbolInfo);

  TJclTD32WithSymbolInfo = class(TJclTD32Scope)
  public
    constructor Create(pSymInfo: PSymbolInfo; const aID: DWORD); override;
  end;

  TJclTD32BPRel32SymbolInfo = class(TJclTD32NamedSymbol)
  private
    FOffset: Integer;
  public
    constructor Create(pSymInfo: PSymbolInfo; const aID: DWORD); override;
    property Offset: Integer read FOffset;
  end;

  TJclTD32RegisterSymbolInfo = class(TJclTD32NamedSymbol)
  private
    FRegisters: Word;
    FRanges: array of PRegisterRange;
    function GetRange(const Index: Integer): PRegisterRange;
    function GetRangeCount: Integer;
  protected
    procedure AnalizeRanges(pSymInfo: PSymbolInfo);
  public
    constructor Create(pSymInfo: PSymbolInfo; const aID: DWORD); override;
    property Registers: Word read FRegisters;
    property Range[const Index: Integer]: PRegisterRange read GetRange;
    property RangeCount: Integer read GetRangeCount;
  end;

  { not used by Delphi }
  TJclTD32ObjNameSymbolInfo = class(TJclTD32SymbolInfo)
  private
    FSignature: DWORD;
    FNameIndex: DWORD;
  public
    constructor Create(pSymInfo: PSymbolInfo; const aID: DWORD); override;
    property NameIndex: DWORD read FNameIndex;
    property Signature: DWORD read FSignature;
  end;

  TJclTD32DataSymbolInfo = class(TJclTD32NamedSymbol)
  private
    FSegment: Word;
    FOffset: DWORD;
  public
    constructor Create(pSymInfo: PSymbolInfo; const aID: DWORD); override;
    property Segment: Word read FSegment;
    property Offset: DWORD read FOffset;
  end;

  TJclTD32LDataSymbolInfo = class(TJclTD32DataSymbolInfo);
  TJclTD32GDataSymbolInfo = class(TJclTD32DataSymbolInfo);

  TJclTD32UdtSymbolInfo = class(TJclTD32NamedSymbol)
  private
    FProperties: Word;
  public
    constructor Create(pSymInfo: PSymbolInfo; const aID: DWORD); override;
    property Properties: Word read FProperties;
  end;

  TJclTD32StartSymbolInfo = class(TJclTD32SymbolInfo)
  private
    FSegment: Word;
    FOffset: DWORD;
    FCodeCount: Word;
    FDataCount: Word;
    FFirstData: DWORD;
  public
    constructor Create(pSymInfo: PSymbolInfo; const aID: DWORD); override;
    property Segment: Word read FSegment;
    property Offset: DWORD read FOffset;
    property CodeCount: Word read FCodeCount;
    property DataCount: Word read FDataCount;
    property FirstData: DWORD read FFirstData;
  end;

  TJclTD32EndSymbolInfo = class(TJclTD32SymbolInfo);

  TJclTD32LinkSymbolInfo = class(TJclTD32SymbolInfo)
  private
    FOffset: DWORD;
  public
    constructor Create(pSymInfo: PSymbolInfo; const aID: DWORD); override;
    property Offset: DWORD read FOffset;
  end;

  TJclTD32ConstantSymbolInfo = class(TJclTD32NamedSymbol)
    FFlags: Word;
    FSize: Word;
    FValue: Pointer;
  public
    constructor Create(pSymInfo: PSymbolInfo; const aID: DWORD); override;
    destructor Destroy; override;
    property Flags: Word read FFlags;
    property Size: Word read FSize;
    property Value: Pointer read FValue;
  end;

const
  { Leaf indices for type records that can be referenced from symbols }
  LF_MODIFIER   = $1;
  LF_POINTER    = $2;
  LF_ARRAY      = $3;
  LF_CLASS      = $4;
  LF_STRUCTURE  = $5;
  LF_UNION      = $6;
  LF_ENUM       = $7;
  LF_PROCEDURE  = $8;
  LF_MFUNCTION  = $9;
  LF_VTSHAPE    = $A;
  LF_COBOL0     = $B;
  LF_COBOL1     = $C;
  LF_BARRAY     = $D;
  LF_LABEL      = $E;
  LF_NULL       = $F;
  LF_NOTTRAN    = $10;
  LF_DIMARRAY   = $11;
  LF_VFTPATH    = $12;
  LF_PRECOMP    = $13;
  LF_ENDPRECOMP = $14;
  LF_OEM        = $15;

  { Delphi leafs }
  LF_SET        = $30;
  LF_SUBRANGE   = $31;
  LF_PARRAY     = $32;
  LF_PSTRING    = $33;
  LF_CLOSURE    = $34;
  LF_PROPERTY   = $35;
  LF_LSTRING    = $36;
  LF_VARIANT    = $37;
  LF_CLASSREF   = $38;
  LF_WSTRING    = $39;

  { Leaf indices for type records that can be referenced from other type records }
  LF_SKIP       = $200;
  LF_ARGLIST    = $201;
  LF_DEFARG     = $202;
  LF_LIST       = $203;
  LF_FIELDLIST  = $204;
  LF_DERIVED    = $205;
  LF_BITFIELD   = $206;
  LF_METHODLIST = $207;
  LF_DIMCONU    = $208;
  LF_DIMCONLU   = $209;
  LF_DIMVARU    = $20A;
  LF_DIMVARLU   = $20B;
  LF_REFSYM     = $20C;

  { Leaf indices for fields of complex lists }
  LF_BCLASS     = $400;
  LF_VBCLASS    = $401;
  LF_IVBCLASS   = $402;
  LF_ENUMERATE  = $403;
  LF_FRIENDFCN  = $404;
  LF_INDEX      = $405;
  LF_MEMBER     = $406;
  LF_STMEMBER   = $407;
  LF_METHOD     = $408;
  LF_NESTTYPE   = $409;
  LF_VFUNCTAB   = $40A;
  LF_FRIENDCLS  = $40B;
  LF_ONEMETHOD  = $40C;
  LF_VFUNCOFF   = $40D;

  { Leaf indices for numeric fields of symbols and type records }
  LF_CHAR       = $8000;
  LF_SHORT      = $8001;
  LF_USHORT     = $8002;
  LF_LONG       = $8003;
  LF_ULONG      = $8004;
  LF_REAL32     = $8005;
  LF_REAL64     = $8006;
  LF_REAL80     = $8007;
  LF_REAL128    = $8008;
  LF_QUADWORD   = $8009;
  LF_UQUADWORD  = $800A;
  LF_REAL48     = $800B;
  LF_COMPLEX32  = $800C;
  LF_COMPLEX64  = $800D;
  LF_COMPLEX80  = $800E;
  LF_COMPLEX128 = $800F;
  LF_VARSTRING  = $8010;

  LF_RAWBITS    = $9000;

  LF_PAD0       = $F0;
  LF_PAD1       = $F1;
  LF_PAD2       = $F2;
  LF_PAD3       = $F3;
  LF_PAD4       = $F4;
  LF_PAD5       = $F5;
  LF_PAD6       = $F6;
  LF_PAD7       = $F7;
  LF_PAD8       = $F8;
  LF_PAD9       = $F9;
  LF_PAD10      = $FA;
  LF_PAD11      = $FB;
  LF_PAD12      = $FC;
  LF_PAD13      = $FD;
  LF_PAD14      = $FE;
  LF_PAD15      = $FF;

type
  TCharTypeInfo = packed record
    Value: ShortInt;
  end;

  TShortTypeInfo = packed record
    Value: SmallInt;
  end;

  TUShortTypeInfo = packed record
    Value: Word;
  end;

  TLongTypeInfo = packed record
    Value: Integer;
  end;

  TULongTypeInfo = packed record
    Value: LongWord;
  end;

  TReal32TypeInfo = packed record
    Value: Single;
  end;

  TReal64TypeInfo = packed record
    Value: Real;
  end;

  TReal80TypeInfo = packed record
    Value: Extended;
  end;

  TQuadWordTypeInfo = packed record
    Value: Int64;
  end;

  TUQuadWordTypeInfo = packed record
    Value: UInt64;
  end;

  TReal48TypeInfo = packed record
    Value: Real48;
  end;

  TCurrencyTypeInfo = packed record
    Value: Currency;
  end;

  TComplex64TypeInfo = packed record
    Re: Real;
    Im: Real;
  end;

  TWideCharTypeInfo = packed record
    Value: WideChar;
  end;

  TNumericLeaf = packed record
    Leaf: Word;
    case Word of
      LF_CHAR      : (LeafChar      : TCharTypeInfo);
      LF_SHORT     : (LeafShort     : TShortTypeInfo);
      LF_USHORT    : (LeafUShort    : TUShortTypeInfo);
      LF_LONG      : (LeafLong      : TLongTypeInfo);
      LF_ULONG     : (LeafULong     : TULongTypeInfo);
      LF_QUADWORD  : (LeafQuadWord  : TQuadWordTypeInfo);
      LF_UQUADWORD : (LeafUQuadWord : TUQuadWordTypeInfo);
      LF_REAL32    : (LeafReal32    : TReal32TypeInfo);
      LF_REAL48    : (LeafReal48    : TReal48TypeInfo);
      LF_REAL64    : (LeafReal64    : TReal64TypeInfo);
      LF_REAL80    : (LeafReal80    : TReal80TypeInfo);
      LF_COMPLEX64 : (LeafComplex64 : TComplex64TypeInfo);
  end;

  TClassTypeInfo = packed record
    Fields: Word;
    FieldIdx: DWORD;
    Flags: Word;
    cClass: DWORD;
    dList: DWORD;
    VTable: DWORD;
    Name: DWORD;
    Length: Word;
  end;

  TStructureTypeInfo = TClassTypeInfo;

  TPStringTypeInfo = packed record
    BaseType: DWORD;
    IndexType: DWORD;
    Name: DWORD;
    Flags: DWORD;
  end;

  TLStringTypeInfo = packed record
    NameIndex: DWORD;
  end;

  TWStringTypeInfo = TLStringTypeInfo;

  TSubrangeData = packed record
    case Byte of
      0: (L0, H0: Word; S0: DWORD);
      1: (L1: TNumericLeaf; H1: Word; S1: DWORD);
      2: (L2: Word; H2: TNumericLeaf; S2: DWORD);
      3: (L3, H3: TNumericLeaf; S3: DWORD);
  end;

  TSubrangeTypeInfo = packed record
    BaseType: DWORD;
    Name: DWORD;
    Data: TSubrangeData;
  end;

  TPointerTypeInfo = packed record
    Flags: Word;
    ElementType: DWORD;
  end;

  TEnumTypeInfo = packed record
    Count: Word;
    ElementType: DWORD;
    FieldsType: DWORD;
    ClassType: DWORD;
    Name: DWORD;
  end;

  TProcTypeInfo = packed record
    ResultType: DWORD;
    CallType: Word;
    ParamCount: Word;
    ArgList: DWORD;
  end;

  TMFuncTypeInfo = packed record
    TypeIndex: DWORD;
    ClassType: DWORD;
    SelfType: DWORD;
    Falgs: Word;
    ParamCount: Word;
    ArgList: DWORD;
    Adjust: DWORD;
  end;

  TSetTypeInfo = packed record
    BaseType: DWORD;
    Name: DWORD;
    LowByte: Word;
    Length: Word;
  end;

  TArrayTypeInfo = packed record
    BaseType: DWORD;
    IndexType: DWORD;
    Name: DWORD;
    case Byte of
      0: (S0: Word; E0: TNumericLeaf);
      1: (S1, E1: TNumericLeaf);
  end;

  TPropertyFlag = (pfDefault, pfReadIsName, pfWriteIsName);
  TPropertyFlags = set of TPropertyFlag;

  TPropertySlot = packed record
    case Byte of
      0: (FieldOffset: DWORD); // When pfReadIsName or pfWriteIsName is _not_ in TPropertyFlags:
      1: (NameIndex: DWORD);   // When pfReadIsName or pfWriteIsName is in TPropertyFlags:
  end;

  TPropertyTypeInfo = packed record
    BaseType: DWORD;
    Flags: Word;
    IndexType: DWORD;
    PropIndex: DWORD;
    Read: TPropertySlot;
    Write: TPropertySlot;
  end;

  TVariantTypeInfo = packed record
    NameIndex: DWORD;
  end;

  TBClassTypeInfo = packed record
    TypeIndex: DWORD;
    Flags: Word;
    Offset: Word;
  end;

  TEnumerateTypeInfo = packed record
    Flags: Word;
    NameIndex: DWORD;
    Reserved: DWORD;
    Value: Word;
  end;

  TMemberTypeInfo = packed record
    BaseType: DWORD;
    Flags: Word;
    NameIndex: DWORD;
    Reserved: DWORD;
    Offset: Word;
  end;

  PFieldListElement = ^TFieldListElement;
  TFieldListElement = packed record
    Leaf: Word;
    case Word of
      LF_BCLASS: (LeafBClass: TBClassTypeInfo);
      LF_ENUMERATE: (LeafEnumerate: TEnumerateTypeInfo);
      LF_MEMBER: (LeafMember: TMemberTypeInfo);
  end;

  TClassRefTypeInfo = packed record
    ElementType : DWORD;
    VTable: DWORD;
  end;

  TJclEnumerateSymbolInfo = class(TJclTD32NamedSymbol)
  private
    FFlags: Word;
    FValue: Word;
  public
    constructor Create(pSymInfo: PFieldListElement); reintroduce; virtual;
    property Flags: Word read FFlags;
    property Value: Word read FValue;
  end;

  TJclTD32MemberSymbolInfo = class(TJclTD32NamedSymbol)
  private
    FFlags: Word;
    FOffset: Word;
  public
    constructor Create(pSymInfo: PFieldListElement); reintroduce; virtual;
    property Flags: Word read FFlags;
    property Offset: Word read FOffset;
  end;

  TArgListTypeInfo = packed record
    Count: Word;
    Args: array[0..0] Of DWORD;
  end;

  TFieldListTypeInfo = packed record
    Length: Word;
    Leaf: Word;
    Data: array[0..0] of Byte;
  end;

  PSymbolTypeInfo = ^TSymbolTypeInfo;
  TSymbolTypeInfo = packed record
    Length: Word;
    Leaf: Word;
    case Word of
      LF_POINTER: (LeafPointer: TPointerTypeInfo);
      LF_CLASS: (LeafClass: TClassTypeInfo);
      LF_STRUCTURE: (LeafStructure: TStructureTypeInfo);
      LF_ENUM: (LeafEnum: TEnumTypeInfo);
      LF_PROCEDURE: (LeafProc: TProcTypeInfo);
      LF_MFUNCTION: (LeafMFunc: TMFuncTypeInfo);
      LF_SET: (LeafSet: TSetTypeInfo);
      LF_SUBRANGE: (LeafSubrange: TSubrangeTypeInfo);
      LF_PARRAY: (LeafArray: TArrayTypeInfo);
      LF_PSTRING: (LeafPString: TPStringTypeInfo);
      LF_PROPERTY: (LeafProperty: TPropertyTypeInfo);
      LF_LSTRING: (LeafLString: TLStringTypeInfo);
      LF_VARIANT: (LeafVariant: TVariantTypeInfo);
      LF_CLASSREF: (LeafClassRef: TClassRefTypeInfo);
      LF_WSTRING: (LeafWString: TWStringTypeInfo);
      LF_ARGLIST: (LeafArgList: TArgListTypeInfo);
      LF_FIELDLIST: (LeafFieldList: TFieldListTypeInfo);
      LF_CHAR: (LeafChar: TCharTypeInfo);
      LF_SHORT: (LeafShort: TShortTypeInfo);
      LF_USHORT: (LeafUShort: TUShortTypeInfo);
      LF_LONG: (LeafLong: TLongTypeInfo);
      LF_ULONG: (LeafULong: TULongTypeInfo);
      LF_QUADWORD: (LeafQuadWord: TQuadWordTypeInfo);
      LF_UQUADWORD: (LeafUQuadWord: TUQuadWordTypeInfo);
      LF_REAL32: (LeafReal32: TReal32TypeInfo);
      LF_REAL48: (LeafReal48: TReal48TypeInfo);
      LF_REAL64: (LeafReal64: TReal64TypeInfo);
      LF_REAL80: (LeafReal80: TReal80TypeInfo);
      LF_COMPLEX64: (LeafComplex64: TComplex64TypeInfo);
  end;

  TJclSymbolTypeKind = (stkBoolean, stkWordBool, stkLongBool, stkShortInt,
    stkSmallInt, stkInteger, stkInt64, stkByte, stkWord, stkCardinal, stkUInt64,
    stkSingle, stkReal48, stkReal, stkExtended, stkCurrency, stkComplex, stkPString,
    stkLString, stkWString, stkChar, stkPointer, stkSubRange, stkArray, stkEnum,
    stkStructure, stkClass, stkSet, stkVariant, stkProperty, stkFieldList, stkClosure,
    stkClassRef, stkWideChar, stkProcedure, stkArgList, stkMFunction, stkVoid);

  TJclSymbolTypeInfo = class
  private
    FMembers: TObjectList;
    FArgs: TList;

    function GetArgs: TList;
    function GetMembers: TObjectList;
  public
    Kind: TJclSymbolTypeKind;
    NameIndex: Integer;
    DataSize: UInt64;
    ElementType: Integer;
    Elements: Integer;
    IndexType: Integer;
    MinValue: Integer;
    MaxValue: Integer;
    Flags: Word;
    ClassType: Integer;
    SelfType: Integer;

    UnitInfo: Pointer;
    UnitInfoIndex: Integer;

    constructor Create;
    destructor Destroy; override;

    property Members: TObjectList read GetMembers;
    property Args: TList read GetArgs;
  end;

  // TD32 parser
  TJclTD32InfoParser = class(TObject)
  private
    FBase: Pointer;
    FData: TCustomMemoryStream;
    FNames: TList;
    FModules: TObjectList;
    FSourceModules: TObjectList;
    FSymbols: TObjectList;
    FProcSymbols: TList;
    FSymbolTypes: TList;
    FValidData: Boolean;
    FIsVarTypesExist: Boolean;
    function GetName(const Idx: Integer): AnsiString;
    function GetNameCount: Integer;
    function GetSymbol(const Idx: Integer): TJclTD32SymbolInfo;
    function GetSymbolCount: Integer;
    function GetSymbolType(const Idx: Integer): TJclSymbolTypeInfo;
    function GetSymbolTypeCount: Integer;
    function GetProcSymbol(const Idx: Integer): TJclTD32ProcSymbolInfo;
    function GetProcSymbolCount: Integer;
    function GetModule(const Idx: Integer): TJclTD32ModuleInfo;
    function GetModuleCount: Integer;
    function GetSourceModule(const Idx: Integer): TJclTD32SourceModuleInfo;
    function GetSourceModuleCount: Integer;
    function GetIndexOfName(Const Name: AnsiString): Integer;
  protected
    procedure Analyse;
    procedure AnalyseNames(const pSubsection: Pointer; const Size: DWORD); //virtual;
    procedure AnalyseGlobalTypes(const pTypes: Pointer; const Size: DWORD); //virtual;
    procedure AnalyseAlignSymbols(pSymbols: PSymbolInfos; const Size: DWORD; const ModuleIndex: Integer); //virtual;
    procedure AnalyseModules(pModInfo: PModuleInfo; const Size: DWORD); //virtual;
    procedure AnalyseSourceModules(pSrcModInfo: PSourceModuleInfo; const Size: DWORD; const ModuleIndex: Integer); //virtual;
    procedure AnalyseUnknownSubSection(const pSubsection: Pointer; const Size: DWORD); //virtual;
    procedure AnalyzeUses(pSymbols: PSymbolInfo; Module: TJclTD32ModuleInfo); //virtual;
    function LfaToVa(const Lfa: DWORD): Pointer;
    function CreateTypeInfo(const Kind: TJclSymbolTypeKind; const Size: DWORD = 0; const NameIndex: DWORD = 0): TJclSymbolTypeInfo; //virtual;
    function CreatePrimitiveTypeInfo(const Idx: DWORD): TJclSymbolTypeInfo; //virtual;
    function CreateArgListTypeInfo(pInfo: PSymbolTypeInfo): TJclSymbolTypeInfo; //virtual;
    function CreateFieldListTypeInfo(pInfo: PSymbolTypeInfo): TJclSymbolTypeInfo; //virtual;
    procedure FixVariantTypes; //virtual;
  public
    constructor Create(const ATD32Data: TCustomMemoryStream); // Data mustn't be freed before the class is destroyed
    destructor Destroy; override;
    function FindModule(const AAddr: DWORD; var AMod: TJclTD32ModuleInfo): Boolean; overload;
    function FindModule(const Section: Word; const Offset: DWORD): TJclTD32ModuleInfo; overload;
    function ModuleByNameIndex(const NameIndex: Cardinal): TJclTD32ModuleInfo; //virtual;
    function FindSourceModule(const AAddr: DWORD; var ASrcMod: TJclTD32SourceModuleInfo): Boolean;
    function FindProc(const AAddr: DWORD; var AProc: TJclTD32ProcSymbolInfo): Boolean;
    class function IsTD32Sign(const Sign: TJclTD32FileSignature): Boolean;
    class function IsTD32DebugInfoValid(const DebugData: Pointer; const DebugDataSize: LongWord): Boolean;
    property Data: TCustomMemoryStream read FData;
    property Names[const Idx: Integer]: AnsiString read GetName;
    property NameCount: Integer read GetNameCount;
    property IndexOfName[Const Name: AnsiString]: Integer read GetIndexOfName;
    property Symbols[const Idx: Integer]: TJclTD32SymbolInfo read GetSymbol;
    property SymbolCount: Integer read GetSymbolCount;
    property SymbolTypes[const Idx: Integer]: TJclSymbolTypeInfo read GetSymbolType;
    property SymbolTypeCount: Integer read GetSymbolTypeCount;
    property ProcSymbols[const Idx: Integer]: TJclTD32ProcSymbolInfo read GetProcSymbol;
    property ProcSymbolCount: Integer read GetProcSymbolCount;
    property Modules[const Idx: Integer]: TJclTD32ModuleInfo read GetModule;
    property ModuleCount: Integer read GetModuleCount;
    property SourceModules[const Idx: Integer]: TJclTD32SourceModuleInfo read GetSourceModule;
    property SourceModuleCount: Integer read GetSourceModuleCount;
    property ValidData: Boolean read FValidData;
  end;

  // TD32 scanner with source location methods
  TJclTD32InfoScanner = class(TJclTD32InfoParser)
  public
    function LineNumberFromAddr(const AAddr: DWORD; var Offset: Integer): Integer; overload;
    function LineNumberFromAddr(const AAddr: DWORD): Integer; overload;
    function ProcNameFromAddr(const AAddr: DWORD): AnsiString; overload;
    function ProcNameFromAddr(const AAddr: DWORD; var Offset: Integer): AnsiString; overload;
    function ModuleNameFromAddr(const AAddr: DWORD): AnsiString;
    function SourceNameFromAddr(const AAddr: DWORD): AnsiString;
  end;

  TTD32DebugDataType = (ddtNone, ddtInImage, ddtTDS);

  // PE Image with TD32 information and source location support
  TJclPeBorTD32Image = class(TJclPeBorImage)
  private
    FIsTD32DebugPresent: Boolean;
    FTD32DebugDataType: TTD32DebugDataType;
    FTD32DebugData: TCustomMemoryStream;
    FTD32Scanner: TJclTD32InfoScanner;
  protected
    procedure AfterOpen; override;
    procedure Clear; override;
    procedure ClearDebugData;
    procedure CheckDebugData;
    function IsDebugInfoInImage(var DataStream: TCustomMemoryStream): Boolean;
    function IsDebugInfoInTds(var DataStream: TCustomMemoryStream): Boolean;
  public
    property IsTD32DebugPresent: Boolean read FIsTD32DebugPresent;
    property TD32DebugDataType: TTD32DebugDataType read FTD32DebugDataType;
    property TD32DebugData: TCustomMemoryStream read FTD32DebugData;
    property TD32Scanner: TJclTD32InfoScanner read FTD32Scanner;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net/svnroot/jcl/tags/JCL-2.4-Build4571/jcl/source/windows/JclTD32.pas $';
    Revision: '$Revision: 3599 $';
    Date: '$Date: 2011-09-03 00:07:50 +0200 (sam. 03 sept. 2011) $';
    LogPath: 'JCL\source\windows';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclResources,
  JclSysUtils, System.AnsiStrings;

const
  TurboDebuggerSymbolExt = '.tds';

//=== { TJclModuleInfo } =====================================================

constructor TJclTD32ModuleInfo.Create(pModInfo: PModuleInfo);
begin
  Assert(Assigned(pModInfo));
  inherited Create;
  FSourceModules := TList.Create;
  FUsedModuleNameIndices := TList.Create;
  FSymbols := TList.Create;
  FProcSymbols := TList.Create;
  FNameIndex := pModInfo.NameIndex;
  FSegments := @pModInfo.Segments[0];
  FSegmentCount := pModInfo.SegmentCount;
end;

destructor TJclTD32ModuleInfo.Destroy;
begin
  FreeAndNil(FProcSymbols);
  FreeAndNil(FSymbols);
  FreeAndNil(FSourceModules);
  FreeAndNil(FUsedModuleNameIndices);
  inherited;
end;

function TJclTD32ModuleInfo.GetSegment(const Idx: Integer): TSegmentInfo;
begin
  Assert((0 <= Idx) and (Idx < FSegmentCount));
  Result := FSegments[Idx];
end;

function TJclTD32ModuleInfo.GetSourceModule(const Idx: Integer): TJclTD32SourceModuleInfo;
begin
  Result := TJclTD32SourceModuleInfo(FSourceModules.Items[Idx]);
end;

function TJclTD32ModuleInfo.GetSourceModuleCount: Integer;
begin
  Result := FSourceModules.Count;
end;

function TJclTD32ModuleInfo.GetSymbol(const Idx: Integer): TJclTD32SymbolInfo;
begin
  Result := TJclTD32SymbolInfo(FSymbols.Items[Idx]);
end;

function TJclTD32ModuleInfo.GetSymbolCount: Integer;
begin
  Result := FSymbols.Count;
end;

function TJclTD32ModuleInfo.GetProcSymbol(const Idx: Integer): TJclTD32ProcSymbolInfo;
begin
  Result := TJclTD32ProcSymbolInfo(FProcSymbols.Items[Idx]);
end;

function TJclTD32ModuleInfo.GetProcSymbolCount: Integer;
begin
  Result := FProcSymbols.Count;
end;

function TJclTD32ModuleInfo.FindProc(Offset: DWORD): TJclTD32ProcSymbolInfo;
var
  P: Integer;
begin
  for P := 0 to ProcSymbolCount - 1 do
  begin
    Result := ProcSymbols[P];
    if (Result.Offset <= Offset) and (Result.Offset + Result.Size >= Offset) then
      Exit;
  end;

  Result := nil;
end;

function TJclTD32ModuleInfo.GetUsedModuleNameIndex(const Idx: Integer): Integer;
begin
  Result := Integer(FUsedModuleNameIndices.Items[Idx]);
end;

function TJclTD32ModuleInfo.GetUsedModuleNameIndexCount: Integer;
begin
  Result := FUsedModuleNameIndices.Count;
end;

//=== { TJclLineInfo } =======================================================

constructor TJclTD32LineInfo.Create(const ALineNo, AOffset: DWORD; const ASegment: Word);
begin
  inherited Create;
  FLineNo := ALineNo;
  FOffset := AOffset;
  FSegment := ASegment;
end;

//=== { TJclSourceModuleInfo } ===============================================

constructor TJclTD32SourceModuleInfo.Create(pSrcFile: PSourceFileEntry; const Base: DWORD);
type
  PArrayOfOffsets = ^TArrayOfOffsets;
  TArrayOfOffsets = array[0..0] of Word;
var
  I, J: Integer;
  pLineEntry: PLineMappingEntry;
  LineInfo: TJclTD32LineInfo;
  Offsets: PArrayOfOffsets;
begin
  Assert(Assigned(pSrcFile));
  inherited Create;

  FNameIndex := pSrcFile.NameIndex;
  FLines := TObjectList.Create;

  for I := 0 to pSrcFile.SegmentCount - 1 do
  begin
    pLineEntry := PLineMappingEntry(Base + pSrcFile.BaseSrcLines[I]);

    Offsets := @pLineEntry.Offsets[pLineEntry.PairCount];

    FLines.Capacity := FLines.Capacity + pLineEntry.PairCount;
    for J := 0 to pLineEntry.PairCount - 1 do
    begin
      LineInfo := TJclTD32LineInfo.Create(Offsets^[J], pLineEntry.Offsets[J], pLineEntry.SegmentIndex);
      FLines.Add(LineInfo);
    end;
  end;

  FSegments := @pSrcFile.BaseSrcLines[pSrcFile.SegmentCount];
  FSegmentCount := pSrcFile.SegmentCount;
end;

destructor TJclTD32SourceModuleInfo.Destroy;
begin
  FreeAndNil(FLines);
  inherited Destroy;
end;

function TJclTD32SourceModuleInfo.GetLine(const Idx: Integer): TJclTD32LineInfo;
begin
  Result := TJclTD32LineInfo(FLines.Items[Idx]);
end;

function TJclTD32SourceModuleInfo.GetLineCount: Integer;
begin
  Result := FLines.Count;
end;

function TJclTD32SourceModuleInfo.GetSegment(const Idx: Integer): TOffsetPair;
begin
  Assert((0 <= Idx) and (Idx < FSegmentCount));
  Result := FSegments[Idx];
end;

function TJclTD32SourceModuleInfo.FindLine(const AAddr: DWORD; var ALine: TJclTD32LineInfo): Boolean;
var
  I: Integer;
begin
  for I := 0 to LineCount - 1 do
    with Line[I] do
    begin
      if AAddr = Offset then
      begin
        Result := True;
        ALine := Line[I];
        Exit;
      end
      else
      if (I > 1) and (Line[I - 1].Offset < AAddr) and (AAddr < Offset) then
      begin
        Result := True;
        ALine := Line[I-1];
        Exit;
      end;
    end;
  Result := False;
  ALine := nil;
end;

//=== { TJclSymbolInfo } =====================================================

constructor TJclTD32SymbolInfo.Create(pSymInfo: PSymbolInfo; const aID: DWORD);
begin
  Assert(Assigned(pSymInfo));
  inherited Create;
  FID := aID;
  FSymbolType := pSymInfo.SymbolType;
end;

//=== { TJclProcSymbolInfo } =================================================

constructor TJclTD32ProcSymbolInfo.Create(pSymInfo: PSymbolInfo; const aID: DWORD);
begin
  inherited;
  FSymbols := TList.Create;
  with pSymInfo^ do
  begin
    FParentID := Proc.pParent;
    FSegment := Proc.Segment;
    FOffset := Proc.Offset;
    FSize := Proc.Size;
    FNameIndex := Proc.NameIndex;

    FDebugStart := Proc.DebugStart;
    FDebugEnd := Proc.DebugEnd;
    FTypeIndex := Proc.TypeIndex;
  end;
end;

destructor TJclTD32ProcSymbolInfo.Destroy;
begin
  FreeAndNil(FSymbols);
  inherited;
end;

function TJclTD32ProcSymbolInfo.GetSymbol(const Idx: Integer): TJclTD32SymbolInfo;
begin
  Result := TJclTD32SymbolInfo(FSymbols.Items[Idx]);
end;

function TJclTD32ProcSymbolInfo.GetSymbolCount: Integer;
begin
  Result := FSymbols.Count;
end;

//=== { TJclDataSymbolInfo } =================================================

constructor TJclTD32DataSymbolInfo.Create(pSymInfo: PSymbolInfo; const aID: DWORD);
begin
  inherited;
  with pSymInfo^ do
  begin
    FTypeIndex := Data.TypeIndex;
    FNameIndex := Data.NameIndex;
    FSegment := Data.Segment;
    FOffset := Data.Offset;
  end;
end;

//=== { TJclWithSymbolInfo } =================================================

constructor TJclTD32WithSymbolInfo.Create(pSymInfo: PSymbolInfo; const aID: DWORD);
begin
  inherited;
  with pSymInfo^ do
  begin
    FParentID := With32.pParent;
    FSegment := With32.Segment;
    FOffset := With32.Offset;
    FSize := With32.Size;
    FNameIndex := With32.NameIndex;

    FTypeIndex := With32.TypeIndex;
  end;
end;

//=== { TJclUdtSymbolInfo } ==================================================

constructor TJclTD32UdtSymbolInfo.Create(pSymInfo: PSymbolInfo; const aID: DWORD);
begin
  inherited;
  with pSymInfo^ do
  begin
    FProperties := Udt.Properties;
    FNameIndex := Udt.NameIndex;
    FTypeIndex := Udt.TypeIndex;
  end;
end;

constructor TJclTD32StartSymbolInfo.Create(pSymInfo: PSymbolInfo; const aID: DWORD);
begin
  inherited;
  with pSymInfo^ do
  begin
    FSegment := Start.Segment;
    FOffset := Start.Offset;
    FCodeCount := Start.CodeCount;
    FDataCount := Start.DataCount;
    FFirstData := Start.FirstData;
  end;
end;

constructor TJclTD32BPRel32SymbolInfo.Create(pSymInfo: PSymbolInfo; const aID: DWORD);
begin
  inherited;
  with pSymInfo^ do
  begin
    FOffset := BPRel.Offset;
    FNameIndex := BPRel.NameIndex;
    FTypeIndex := BPRel.TypeIndex;
  end;
end;

constructor TJclTD32RegisterSymbolInfo.Create(pSymInfo: PSymbolInfo; const aID: DWORD);
begin
  inherited;
  with pSymInfo^ do
  begin
    FRegisters := Registers.Registers;
    FNameIndex := Registers.NameIndex;
    FTypeIndex := Registers.TypeIndex;
  end;
end;

procedure TJclTD32RegisterSymbolInfo.AnalizeRanges(pSymInfo: PSymbolInfo);
var
  I: Integer;
begin
  with pSymInfo^ do
  begin
    SetLength(FRanges, OptVar.Count);
    for I := 0 to OptVar.Count - 1 do
      FRanges[I] := @OptVar.Ranges[I];
  end;
end;

function TJclTD32RegisterSymbolInfo.GetRange(const Index: Integer): PRegisterRange;
begin
  Result := FRanges[Index];
end;

function TJclTD32RegisterSymbolInfo.GetRangeCount: Integer;
begin
  Result := Length(FRanges);
end;

constructor TJclTD32LinkSymbolInfo.Create(pSymInfo: PSymbolInfo; const aID: DWORD);
begin
  inherited;
  with pSymInfo^ do
    FOffset := Link.Offset;
end;

constructor TJclTD32ConstantSymbolInfo.Create(pSymInfo: PSymbolInfo; const aID: DWORD);
begin
  inherited;
  with pSymInfo^ do
  begin
    FTypeIndex := Constant.TypeIndex;
    FFlags := Constant.Flags;
    FNameIndex := Constant.NameIndex;
    FSize := Size - 16;
    GetMem(FValue, FSize);
    Move(Constant.Value, FValue^, FSize);
  end;
end;

destructor TJclTD32ConstantSymbolInfo.Destroy;
begin
  FreeMem(FValue);
  inherited;
end;

constructor TJclEnumerateSymbolInfo.Create(pSymInfo: PFieldListElement);
begin
  // Adjust PFieldListElement to PSymbolInfo for TJclSymbolInfo.Create
  inherited Create(PSymbolInfo(DWORD(pSymInfo) - 2), 0);
  with pSymInfo^ do
  begin
    FFlags := LeafEnumerate.Flags;
    FNameIndex := LeafEnumerate.NameIndex;
    FValue := LeafEnumerate.Value;
  end;
end;

constructor TJclTD32MemberSymbolInfo.Create(pSymInfo: PFieldListElement);
begin
  // Adjust PFieldListElement to PSymbolInfo for TJclSymbolInfo.Create
  inherited Create(PSymbolInfo(DWORD(pSymInfo) - 2), 0);
  with pSymInfo^ do
  begin
    FFlags := LeafMember.Flags;
    FNameIndex := LeafMember.NameIndex;
    FTypeIndex := LeafMember.BaseType;
    FOffset := LeafMember.Offset;
  end;
end;

//=== { TJclTD32InfoParser } =================================================

constructor TJclTD32InfoParser.Create(const ATD32Data: TCustomMemoryStream);
begin
  Assert(Assigned(ATD32Data));
  inherited Create;
  FNames := TList.Create;
  FModules := TObjectList.Create;
  FSourceModules := TObjectList.Create;
  FSymbols := TObjectList.Create;
  FSymbolTypes := TObjectList.Create;
  FProcSymbols := TList.Create;
  FNames.Add(nil);
  FData := ATD32Data;
  FBase := FData.Memory;
  FValidData := IsTD32DebugInfoValid(FBase, FData.Size);
  if FValidData then
    Analyse;
end;

destructor TJclTD32InfoParser.Destroy;
begin
  FreeAndNil(FProcSymbols);
  FreeAndNil(FSymbols);
  FreeAndNil(FSymbolTypes);
  FreeAndNil(FSourceModules);
  FreeAndNil(FModules);
  FreeAndNil(FNames);
  inherited Destroy;
end;

procedure TJclTD32InfoParser.Analyse;
var
  I, M: Integer;
  pDirHeader: PDirectoryHeader;
  DirEntry: TDirectoryEntry;
  pSubsection: Pointer;
begin
  pDirHeader := PDirectoryHeader(LfaToVa(PJclTD32FileSignature(LfaToVa(0)).Offset));
  while True do
  begin
    Assert(pDirHeader.DirEntrySize = SizeOf(TDirectoryEntry));
    M := 0;
    {$RANGECHECKS OFF}
    for I := 0 to pDirHeader.DirEntryCount - 1 do
    begin
      DirEntry := pDirHeader.DirEntries[I];
      pSubsection := LfaToVa(DirEntry.Offset);
      case DirEntry.SubsectionType of
        SUBSECTION_TYPE_MODULE:
          AnalyseModules(pSubsection, DirEntry.Size);
        SUBSECTION_TYPE_ALIGN_SYMBOLS:
        begin
          AnalyseAlignSymbols(pSubsection, DirEntry.Size, M);
          Inc(M);
        end;
        SUBSECTION_TYPE_SOURCE_MODULE:
          AnalyseSourceModules(pSubsection, DirEntry.Size, M);
        SUBSECTION_TYPE_NAMES:
          AnalyseNames(pSubsection, DirEntry.Size);
        SUBSECTION_TYPE_GLOBAL_TYPES:
        begin
          FSymbolTypes.Count := $1000; // Reserve space for primitive types that will be created dinamically
          AnalyseGlobalTypes(pSubsection, DirEntry.Size);
        end
      else
        AnalyseUnknownSubSection(pSubsection, DirEntry.Size);
      end;
    end;
    {$IFDEF RANGECHECKS_ON}
    {$RANGECHECKS ON}
    {$ENDIF RANGECHECKS_ON}
    if pDirHeader.lfoNextDir <> 0 then
      pDirHeader := PDirectoryHeader(LfaToVa(pDirHeader.lfoNextDir))
    else
      Break;
  end;
  // Add predefined Variant type info and update all references to it
  FixVariantTypes;
end;

procedure TJclTD32InfoParser.AnalyseNames(const pSubsection: Pointer; const Size: DWORD);
var
  I, Count, Len: Integer;
  pszName: PAnsiChar;
begin
  Count := PDWORD(pSubsection)^;
  pszName := PAnsiChar(TJclAddr(pSubsection) + SizeOf(DWORD));
  if Count > 0 then
  begin
    FNames.Capacity := FNames.Capacity + Count;
    for I := 0 to Count - 1 do
    begin
      // Get the length of the name
      Len := Ord(pszName^);
      Inc(pszName);
      // Get the name
      FNames.Add(pszName);
      // first, skip the length of name
      Inc(pszName, Len);
      // the length is only correct modulo 256 because it is stored on a single byte,
      // so we have to iterate until we find the real end of the string
      while PszName^ <> #0 do
        Inc(pszName, 256);
      // then, skip a NULL at the end
      Inc(pszName, 1);
    end;
  end;
end;

procedure TJclTD32InfoParser.AnalyseGlobalTypes(const pTypes: Pointer; const Size: DWORD);

  function GetBoundFromNumeric(var Data: Pointer): Int64;
  var
    Size, Offset: Byte;
  begin
    Result := 0;
    Size := 2;
    Offset := 0;
    if PWord(Data)^ >= $8000 then
    begin
      case PWord(Data)^ of
        LF_LONG, LF_ULONG: Size := 4;
        LF_QUADWORD, LF_UQUADWORD: Size := 8
        else
          raise Exception.Create('Invalid bound.');
      end;
      Offset := 2;
    end;
    Inc(Cardinal(Data), Offset);
    Move(Data^, Result, Size);
    Inc(Cardinal(Data), Size);
  end;

var
  I: Integer;
  Info: PSymbolTypeInfo;
  TypeInfo: TJclSymbolTypeInfo;
  Data: Pointer;
begin
  FSymbolTypes.Capacity := FSymbolTypes.Capacity + Integer(PGlobalTypeInfo(pTypes).Count);
  for I := 0 To PGlobalTypeInfo(pTypes).Count - 1 do
  begin
    TypeInfo := Nil;
    Info := PSymbolTypeInfo(LongWord(pTypes) + PGlobalTypeInfo(pTypes).Offsets[I]);
    case Info.Leaf of
      LF_POINTER:
      begin
          TypeInfo := CreateTypeInfo(stkPointer, SizeOf(Pointer));
          TypeInfo.ElementType := Info.LeafPointer.ElementType;
      end;
      LF_CLASS:
      begin
        TypeInfo := CreateTypeInfo(stkClass, Info.LeafClass.Length, Info.LeafClass.Name);
        TypeInfo.Elements := Info.LeafClass.FieldIdx;
      end;
      LF_STRUCTURE:
      begin
        TypeInfo := CreateTypeInfo(stkStructure, Info.LeafStructure.Length, Info.LeafStructure.Name);
        TypeInfo.Elements := Info.LeafStructure.FieldIdx;
      end;
      LF_ENUM:
      begin
        TypeInfo := CreateTypeInfo(stkEnum, 0, Info.LeafEnum.Name);
        TypeInfo.ElementType := Info.LeafEnum.ElementType;
        TypeInfo.Elements := Info.LeafEnum.FieldsType;
      end;
      LF_PROCEDURE:
      begin
        TypeInfo := CreateTypeInfo(stkProcedure, 0);
        TypeInfo.IndexType := Info.LeafProc.ResultType;
        TypeInfo.Flags := Info.LeafProc.CallType;
        TypeInfo.ElementType := Info.LeafProc.ArgList;
        TypeInfo.Elements := Info.LeafProc.ParamCount;
      end;
      LF_MFUNCTION:
      begin
        TypeInfo := CreateTypeInfo(stkMFunction, 0);
        TypeInfo.IndexType := Info.LeafMFunc.TypeIndex;
        TypeInfo.ClassType := Info.LeafMFunc.ClassType;
        TypeInfo.SelfType := Info.LeafMFunc.SelfType;
        TypeInfo.Flags := Info.LeafMFunc.Falgs;
        TypeInfo.ElementType := Info.LeafMFunc.ArgList;
        TypeInfo.Elements := Info.LeafMFunc.ParamCount;
        TypeInfo.MinValue := Info.LeafMFunc.Adjust;
      end;
      LF_SET:
      begin
        TypeInfo := CreateTypeInfo(stkSet, 0, Info.LeafSet.Name);
        TypeInfo.ElementType := Info.LeafSet.BaseType;
        TypeInfo.MinValue := Info.LeafSet.LowByte;
        TypeInfo.DataSize := 32;
      end;
      LF_SUBRANGE:
      begin
        TypeInfo := CreateTypeInfo(stkSubrange, 0, Info.LeafSubrange.Name);
        try
          TypeInfo.IndexType := Info.LeafSubrange.BaseType;
          Data := @Info.LeafSubrange.Data;
          TypeInfo.MinValue := GetBoundFromNumeric(Data);
          TypeInfo.MaxValue := GetBoundFromNumeric(Data);
          TypeInfo.DataSize := PWord(Data)^;
        except
          FreeAndNil(TypeInfo);
        end;
      end;
      LF_PARRAY:
      begin
        TypeInfo := CreateTypeInfo(stkArray, 0, Info.LeafArray.Name);
        try
          TypeInfo.ElementType := Info.LeafArray.BaseType;
          TypeInfo.IndexType := Info.LeafArray.IndexType;
          Data := @Info.LeafArray.S1;
          TypeInfo.DataSize := GetBoundFromNumeric(Data);
          TypeInfo.MaxValue := GetBoundFromNumeric(Data);
        except
          FreeAndNil(TypeInfo);
        end;
      end;
      LF_PSTRING:
      begin
        TypeInfo := CreateTypeInfo(stkPString, 0, Info.LeafPString.Name);
        TypeInfo.ElementType := Info.LeafPString.BaseType;
        TypeInfo.IndexType := Info.LeafPString.IndexType;
      end;
      LF_CLOSURE:    TypeInfo := CreateTypeInfo(stkClosure, SizeOf(Pointer));
      LF_PROPERTY:
      begin
        TypeInfo := CreateTypeInfo(stkProperty, 0, Info.LeafProperty.PropIndex);
        TypeInfo.Flags := Info.LeafProperty.Flags;
        TypeInfo.ElementType := Info.LeafProperty.BaseType;
        TypeInfo.IndexType := Info.LeafProperty.IndexType;
        TypeInfo.MinValue := Info.LeafProperty.Read.FieldOffset;
        TypeInfo.MaxValue := Info.LeafProperty.Write.FieldOffset;
      end;
      LF_LSTRING:
      begin
        TypeInfo := CreateTypeInfo(stkLString, SizeOf(Pointer), Info.LeafWString.NameIndex);
        TypeInfo.ElementType := 97; // Predefined Char type
      end;
      LF_VARIANT:
      begin
        TypeInfo := CreateTypeInfo(stkVariant, 16, Info.LeafVariant.NameIndex);
        FIsVarTypesExist := True;
      end;
      LF_CLASSREF:
      begin
          TypeInfo := CreateTypeInfo(stkClassRef, SizeOf(Pointer));
          TypeInfo.ElementType := Info.LeafClassRef.ElementType;
          TypeInfo.Elements := Info.LeafClassRef.VTable;
      end;
      LF_WSTRING:
      begin
        TypeInfo := CreateTypeInfo(stkWString, SizeOf(Pointer), Info.LeafLString.NameIndex);
        TypeInfo.ElementType := 113; // Predefined WideChar type
      end;
      LF_ARGLIST:    TypeInfo := CreateArgListTypeInfo(Info);
      LF_FIELDLIST:  TypeInfo := CreateFieldListTypeInfo(Info);
      LF_CHAR:       TypeInfo := CreateTypeInfo(stkInteger, SizeOf(Info.LeafChar));
      LF_SHORT:      TypeInfo := CreateTypeInfo(stkInteger, SizeOf(Info.LeafShort));
      LF_USHORT:     TypeInfo := CreateTypeInfo(stkInteger, SizeOf(Info.LeafUShort));
      LF_LONG:       TypeInfo := CreateTypeInfo(stkInteger, SizeOf(Info.LeafLong));
      LF_ULONG:      TypeInfo := CreateTypeInfo(stkInteger, SizeOf(Info.LeafULong));
      LF_QUADWORD:   TypeInfo := CreateTypeInfo(stkInteger, SizeOf(Info.LeafQuadWord));
      LF_UQUADWORD:  TypeInfo := CreateTypeInfo(stkInteger, SizeOf(Info.LeafUQuadWord));
      LF_REAL32:     TypeInfo := CreateTypeInfo(stkReal, SizeOf(Info.LeafReal32));
      LF_REAL48:     TypeInfo := CreateTypeInfo(stkReal, SizeOf(Info.LeafReal48));
      LF_REAL64:     TypeInfo := CreateTypeInfo(stkReal, SizeOf(Info.LeafReal64));
      LF_REAL80:     TypeInfo := CreateTypeInfo(stkReal, SizeOf(Info.LeafReal80));
      LF_COMPLEX64:  TypeInfo := CreateTypeInfo(stkComplex, SizeOf(Info.LeafComplex64));
    end;

    FSymbolTypes.Add(TypeInfo);
  end;
end;

procedure TJclTD32InfoParser.AnalyseAlignSymbols(pSymbols: PSymbolInfos; const Size: DWORD; const ModuleIndex: Integer);
var
  I: Integer;
  Offset, ID: DWORD;
  pInfo: PSymbolInfo;
  Symbol: TJclTD32SymbolInfo;
  Module: TJclTD32ModuleInfo;
  ProcSymbol: TJclTD32ProcSymbolInfo;
  ModuleProcSymbol: TJclTD32ProcSymbolInfo;
begin
  Module := Modules[ModuleIndex];
  ProcSymbol := nil;
  Offset := DWORD(@pSymbols.Symbols[0]) - DWORD(pSymbols);

  if FSymbols.Capacity = 0 then
    FSymbols.Capacity := 4096;

  if FProcSymbols.Capacity = 0 then
    FProcSymbols.Capacity := 4096;

  if Module.FSymbols.Capacity = 0 then
    Module.FSymbols.Capacity := 256;

  while Offset < Size do
  begin
    pInfo := PSymbolInfo(DWORD(pSymbols) + Offset);
    ID := DWORD(pInfo) - DWORD(pSymbols);
    Symbol := nil;
    case pInfo.SymbolType of
{ Temporary disabled symbols
      SYMBOL_TYPE_WITH32:
        Symbol := TJclWithSymbolInfo.Create(pInfo, ID);
      SYMBOL_TYPE_UDT:
        Symbol := TJclUdtSymbolInfo.Create(pInfo, ID);
      SYMBOL_TYPE_SSEARCH:
        Symbol := TJclStartSymbolInfo.Create(pInfo, ID);
}
      SYMBOL_TYPE_END:
        Symbol := TJclTD32EndSymbolInfo.Create(pInfo, ID);
      SYMBOL_TYPE_USES:
        AnalyzeUses(pInfo, Module);
      SYMBOL_TYPE_REGISTER:
        Symbol := TJclTD32RegisterSymbolInfo.Create(pInfo, ID);
      SYMBOL_TYPE_OPTVAR32:
        if FSymbols.Last is TJclTD32RegisterSymbolInfo then
          TJclTD32RegisterSymbolInfo(FSymbols.Last).AnalizeRanges(pInfo);
      SYMBOL_TYPE_PCONSTANT:
        Symbol := TJclTD32ConstantSymbolInfo.Create(pInfo, ID);
      SYMBOL_TYPE_BPREL32:
        Symbol := TJclTD32BPRel32SymbolInfo.Create(pInfo, ID);
      SYMBOL_TYPE_LDATA32:
        Symbol := TJclTD32LDataSymbolInfo.Create(pInfo, ID);
      SYMBOL_TYPE_GDATA32:
        Symbol := TJclTD32GDataSymbolInfo.Create(pInfo, ID);
      SYMBOL_TYPE_LPROC32:
        Symbol := TJclTD32LocalProcSymbolInfo.Create(pInfo, ID);
      SYMBOL_TYPE_GPROC32:
        Symbol := TJclTD32GlobalProcSymbolInfo.Create(pInfo, ID);
      SYMBOL_TYPE_SLINK32:
        Symbol := TJclTD32LinkSymbolInfo.Create(pInfo, ID);
    end;

    if Assigned(Symbol) then
    begin
      FSymbols.Add(Symbol);
      if Symbol.FSymbolType = SYMBOL_TYPE_END then
        ProcSymbol := nil
      else
      begin
        if ProcSymbol = nil then
          Module.FSymbols.Add(Symbol)
        else
          ProcSymbol.FSymbols.Add(Symbol);

        if Symbol is TJclTD32ProcSymbolInfo then
        begin
          FProcSymbols.Add(Symbol);
          Module.FProcSymbols.Add(Symbol);
          ProcSymbol := TJclTD32ProcSymbolInfo(Symbol);

          if Symbol.ParentID <> 0 then
            for I := Module.ProcSymbolCount - 1 downto 0 do
            begin
              ModuleProcSymbol := Module.ProcSymbols[I];
              if ModuleProcSymbol.ID = Symbol.ParentID then
              begin
                Symbol.FParent := ModuleProcSymbol;
                ModuleProcSymbol.FSymbols.Add(Symbol);
                Break;
              end;
            end;
        end;
      end;
    end;
    Inc(Offset, pInfo.Size + SizeOf(pInfo.Size));
  end;
end;

procedure TJclTD32InfoParser.AnalyseModules(pModInfo: PModuleInfo; const Size: DWORD);
var
  ModuleInfo: TJclTD32ModuleInfo;
begin
  ModuleInfo := TJclTD32ModuleInfo.Create(pModInfo);
  FModules.Add(ModuleInfo);
end;

procedure TJclTD32InfoParser.AnalyseSourceModules(pSrcModInfo: PSourceModuleInfo; const Size: DWORD; const ModuleIndex: Integer);
var
  I: Integer;
  pSrcFile: PSourceFileEntry;
  SrcModule: TJclTD32SourceModuleInfo;
  ModuleInfo: TJclTD32ModuleInfo;
begin
  ModuleInfo := Modules[ModuleIndex];

  FSourceModules.Capacity := FSourceModules.Capacity + pSrcModInfo.FileCount;
  ModuleInfo.FSourceModules.Capacity := ModuleInfo.FSourceModules.Capacity + pSrcModInfo.FileCount;
  for I := 0 to pSrcModInfo.FileCount - 1 do
  begin
    pSrcFile := PSourceFileEntry(DWORD(pSrcModInfo) + pSrcModInfo.BaseSrcFiles[I]);
    if pSrcFile.NameIndex > 0 then
    begin
      SrcModule := TJclTD32SourceModuleInfo.Create(pSrcFile, DWORD(pSrcModInfo));
      FSourceModules.Add(SrcModule);
      ModuleInfo.FSourceModules.Add(SrcModule);
    end;
  end;
end;

procedure TJclTD32InfoParser.AnalyseUnknownSubSection(const pSubsection: Pointer; const Size: DWORD);
begin
  // do nothing
end;

procedure TJclTD32InfoParser.AnalyzeUses(pSymbols: PSymbolInfo; Module: TJclTD32ModuleInfo);
var
  I: Integer;
  Count: Integer;
begin
  Count := (pSymbols.Size - SizeOf(pSymbols.SymbolType)) div SizeOf(DWORD);
  Module.FUsedModuleNameIndices.Capacity := Module.FUsedModuleNameIndices.Capacity + Count;
  for I := 0 to Count - 1 do
    Module.FUsedModuleNameIndices.Add(Pointer(pSymbols.Use.Names[I]));
end;

function TJclTD32InfoParser.GetModule(const Idx: Integer): TJclTD32ModuleInfo;
begin
  Result := TJclTD32ModuleInfo(FModules.Items[Idx]);
end;

function TJclTD32InfoParser.GetModuleCount: Integer;
begin
  Result := FModules.Count;
end;

function TJclTD32InfoParser.GetName(const Idx: Integer): AnsiString;
begin
  Result := PAnsiChar(FNames.Items[Idx]);
end;

function TJclTD32InfoParser.GetNameCount: Integer;
begin
  Result := FNames.Count;
end;

function TJclTD32InfoParser.GetSourceModule(const Idx: Integer): TJclTD32SourceModuleInfo;
begin
  Result := TJclTD32SourceModuleInfo(FSourceModules.Items[Idx]);
end;

function TJclTD32InfoParser.GetSourceModuleCount: Integer;
begin
  Result := FSourceModules.Count;
end;

function TJclTD32InfoParser.GetIndexOfName(Const Name: AnsiString): Integer;
var
  I: Integer;
  L: Integer;
  N: PAnsiChar;
  S: PAnsiChar;
begin
  N := PAnsiChar(Name);
  L := Length(Name);
  for I := 0 to FNames.Count - 1 do
  begin
    S := PAnsiChar(FNames[I]);
    if (S <> Nil) And (L = Integer(System.AnsiStrings.StrLen(S))) And (System.AnsiStrings.StrLIComp(S, N, L) = 0) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TJclTD32InfoParser.GetSymbol(const Idx: Integer): TJclTD32SymbolInfo;
begin
  Result := TJclTD32SymbolInfo(FSymbols.Items[Idx]);
end;

function TJclTD32InfoParser.GetSymbolCount: Integer;
begin
  Result := FSymbols.Count;
end;

function TJclTD32InfoParser.GetSymbolType(const Idx: Integer): TJclSymbolTypeInfo;
begin
  if (Idx > -1) and (Idx < $1000) and (FSymbolTypes[Idx] = nil) then
    FSymbolTypes[Idx] := CreatePrimitiveTypeInfo(Idx);
  Result := FSymbolTypes[Idx];
end;

function TJclTD32InfoParser.GetSymbolTypeCount: Integer;
begin
  Result := FSymbolTypes.Count;
end;

function TJclTD32InfoParser.GetProcSymbol(const Idx: Integer): TJclTD32ProcSymbolInfo;
begin
  Result := TJclTD32ProcSymbolInfo(FProcSymbols.Items[Idx]);
end;

function TJclTD32InfoParser.GetProcSymbolCount: Integer;
begin
  Result := FProcSymbols.Count;
end;

function TJclTD32InfoParser.FindModule(const AAddr: DWORD; var AMod: TJclTD32ModuleInfo): Boolean;
var
  I, J: Integer;
  ModuleInfo: TJclTD32ModuleInfo;
  SegmentInfo: PSegmentInfo;
begin
  if ValidData then
    for I := 0 to ModuleCount - 1 do
    begin
      ModuleInfo := Modules[I];
      for J := 0 to ModuleInfo.SegmentCount - 1 do
      begin
        SegmentInfo := @ModuleInfo.FSegments[J];
        if (SegmentInfo^.Flags = 1) and (AAddr >= SegmentInfo^.Offset) and
          (AAddr - SegmentInfo^.Offset <= SegmentInfo^.Size) then
        begin
          Result := True;
          AMod := ModuleInfo;
          Exit;
        end;
      end;
    end;
  Result := False;
  AMod := nil;
end;

function TJclTD32InfoParser.FindModule(const Section: Word; const Offset: DWORD): TJclTD32ModuleInfo;
var
  M, S: Integer;
begin
  //TODO:
  if ValidData then
    for M := 0 to ModuleCount - 1 do
    with Modules[M] do
      for S := 0 to SegmentCount - 1 do
        if (Segment[S].Segment = Section) and (Segment[S].Offset <= Offset) and (Segment[S].Offset + Segment[S].Size >= Offset) then
        begin
          Result := Modules[M];
          Exit;
        end;
  Result := nil;
end;

function TJclTD32InfoParser.ModuleByNameIndex(const NameIndex: Cardinal): TJclTD32ModuleInfo;
var
  I: Integer;
begin
  if ValidData then
    for I := 0 to ModuleCount - 1 do
      if Modules[I].NameIndex = NameIndex then
      begin
        Result := Modules[I];
        Exit;
      end;
  Result := nil;
end;

function TJclTD32InfoParser.FindSourceModule(const AAddr: DWORD; var ASrcMod: TJclTD32SourceModuleInfo): Boolean;
var
  I, J: Integer;
begin
  if ValidData then
    for I := 0 to SourceModuleCount - 1 do
    with SourceModules[I] do
      for J := 0 to SegmentCount - 1 do
        with Segment[J] do
          if (StartOffset <= AAddr) and (AAddr < EndOffset) then
          begin
            Result := True;
            ASrcMod := SourceModules[I];
            Exit;
          end;
  ASrcMod := nil;
  Result := False;
end;

function TJclTD32InfoParser.FindProc(const AAddr: DWORD; var AProc: TJclTD32ProcSymbolInfo): Boolean;
var
  I: Integer;
begin
  if ValidData then
    for I := 0 to ProcSymbolCount - 1 do
    begin
      AProc := ProcSymbols[I];
      with AProc do
        if (Offset <= AAddr) and (AAddr < Offset + Size) then
        begin
          Result := True;
          Exit;
        end;
    end;
  AProc := nil;
  Result := False;
end;

class function TJclTD32InfoParser.IsTD32DebugInfoValid(
  const DebugData: Pointer; const DebugDataSize: LongWord): Boolean;
var
  Sign: PJclTD32FileSignature;
  EndOfDebugData: LongWord;
begin
  Assert(not IsBadReadPtr(DebugData, DebugDataSize));
  Result := False;
  EndOfDebugData := LongWord(DebugData) + DebugDataSize;
  if DebugDataSize > SizeOf(TJclTD32FileSignature) then
  begin
    Sign := PJclTD32FileSignature(EndOfDebugData - SizeOf(TJclTD32FileSignature));
    if IsTD32Sign(Sign^) and (Sign^.Offset <= DebugDataSize) then
    begin
      Sign := PJclTD32FileSignature(EndOfDebugData - Sign^.Offset);
      Result := IsTD32Sign(Sign^);
    end;
  end;
end;

class function TJclTD32InfoParser.IsTD32Sign(const Sign: TJclTD32FileSignature): Boolean;
begin
  Result := (Sign.Signature = Borland32BitSymbolFileSignatureForDelphi) or
    (Sign.Signature = Borland32BitSymbolFileSignatureForBCB);
end;

function TJclTD32InfoParser.LfaToVa(const Lfa: DWORD): Pointer;
begin
  Result := Pointer(DWORD(FBase) + Lfa)
end;

function TJclTD32InfoParser.CreateTypeInfo(const Kind: TJclSymbolTypeKind; const Size: DWORD = 0; const NameIndex: DWORD = 0): TJclSymbolTypeInfo;
begin
  Result := TJclSymbolTypeInfo.Create;
  Result.Kind := Kind;
  Result.DataSize := Size;
  Result.NameIndex := NameIndex;
end;

function TJclTD32InfoParser.CreatePrimitiveTypeInfo(const Idx: DWORD): TJclSymbolTypeInfo;
var
  Kind: Byte;
  Size: Byte;
begin
  Result := nil;
  Size := Idx And $7;
  Kind := (Idx And $F0) Shr 4;
  case Kind of
    0: // Special
      case Size Of
        3: Result := CreateTypeInfo(stkVoid, 0);
        4: Result := CreateTypeInfo(stkCurrency, SizeOf(TCurrencyTypeInfo));
      end;
    1: // Signed integral value
      case Size Of
        0: Result := CreateTypeInfo(stkShortInt, SizeOf(TCharTypeInfo));
        1: Result := CreateTypeInfo(stkSmallInt, SizeOf(TShortTypeInfo));
        2: Result := CreateTypeInfo(stkInteger, SizeOf(TLongTypeInfo));
        3: Result := CreateTypeInfo(stkInt64, SizeOf(TQuadWordTypeInfo));
      end;
    2: // Unsigned integral value
      case Size Of
        0: Result := CreateTypeInfo(stkByte, SizeOf(TCharTypeInfo));
        1: Result := CreateTypeInfo(stkWord, SizeOf(TUShortTypeInfo));
        2: Result := CreateTypeInfo(stkCardinal, SizeOf(TULongTypeInfo));
        3: Result := CreateTypeInfo(stkUInt64, SizeOf(TUQuadWordTypeInfo));
      end;
    3: // Boolean
      case Size Of
        0: Result := CreateTypeInfo(stkBoolean, SizeOf(TCharTypeInfo));
        1: Result := CreateTypeInfo(stkWordBool, SizeOf(TUShortTypeInfo));
        2: Result := CreateTypeInfo(stkLongBool, SizeOf(TULongTypeInfo));
      end;
    4: // Real
      case Size Of
        0: Result := CreateTypeInfo(stkSingle, SizeOf(TReal32TypeInfo));
        1: Result := CreateTypeInfo(stkReal, SizeOf(TReal64TypeInfo));
        2: Result := CreateTypeInfo(stkExtended, SizeOf(TReal80TypeInfo));
        4: Result := CreateTypeInfo(stkReal48, SizeOf(TReal48TypeInfo));
      end;
    5: // Complex
      case Size Of
        2: Result := CreateTypeInfo(stkComplex, SizeOf(TComplex64TypeInfo));
      end;
    6: // Special2
      case Size Of
        1: Result := CreateTypeInfo(stkChar, SizeOf(TCharTypeInfo));
      end;
    7: // Real int value
      case Size Of
        0: Result := CreateTypeInfo(stkChar, SizeOf(TCharTypeInfo));
        1: Result := CreateTypeInfo(stkWideChar, SizeOf(TWideCharTypeInfo));
        2: Result := CreateTypeInfo(stkSmallInt, SizeOf(TShortTypeInfo));
        3: Result := CreateTypeInfo(stkWord, SizeOf(TUShortTypeInfo));
        4: Result := CreateTypeInfo(stkInteger, SizeOf(TLongTypeInfo));
        5: Result := CreateTypeInfo(stkCardinal, SizeOf(TULongTypeInfo));
        6: Result := CreateTypeInfo(stkInt64, SizeOf(TQuadWordTypeInfo));
        7: Result := CreateTypeInfo(stkUInt64, SizeOf(TUQuadWordTypeInfo));
      end;
  end;
end;

function TJclTD32InfoParser.CreateArgListTypeInfo(pInfo: PSymbolTypeInfo): TJclSymbolTypeInfo;
var
  I: Integer;
begin
  Result := CreateTypeInfo(stkArgList, 0);
  Result.Args.Capacity := pInfo.LeafArgList.Count;
  for I := 0 to pInfo.LeafArgList.Count - 1 do
    Result.Args.Add(Pointer(pInfo.LeafArgList.Args[I]));
end;

function TJclTD32InfoParser.CreateFieldListTypeInfo(pInfo: PSymbolTypeInfo): TJclSymbolTypeInfo;
var
  Offset, Size: Integer;

  procedure IncOffset;
  begin
    Inc(Offset, Size);
    if PByte(Offset)^ > $F0 then
      Inc(Offset, PByte(Offset)^ And $0F);
  end;

begin
  Result := CreateTypeInfo(stkFieldList, 0);
  Offset := Integer(pInfo);
  Size := Integer(@PSymbolTypeInfo(0).LeafFieldList);
  IncOffset;

  Result.Members.Capacity := 16;
  while Integer(@pInfo.Leaf) + pInfo.Length > Offset do
  begin
    case pFieldListElement(Offset).Leaf of
      LF_BCLASS:
      begin
        Result.ElementType := pFieldListElement(Offset).LeafBClass.TypeIndex;
        Size := SizeOf(TBClassTypeInfo);
      end;
      LF_ENUMERATE:
      begin
        Result.Members.Add(TJclEnumerateSymbolInfo.Create(pFieldListElement(Offset)));
        Size := SizeOf(TEnumerateTypeInfo);
      end;
      LF_MEMBER:
      begin
        Result.Members.Add(TJclTD32MemberSymbolInfo.Create(pFieldListElement(Offset)));
        Size := SizeOf(TMemberTypeInfo);
      end;
      LF_STMEMBER: Size := 14;           // Static member - skip
      LF_METHOD:   Size := 10;           // Class method - skip
      LF_VFUNCTAB: Size := 6             // VMT - skip
      else         Size := pInfo.Length; // Unknown element - cancel parsing
    end;
    Inc(Size, SizeOf(Word));
    IncOffset;
  end;
end;

procedure TJclTD32InfoParser.FixVariantTypes;

  function CheckNameIndex(const Value: AnsiString): Integer;
  begin
    if Value <> '' then
    begin
      Result := IndexOfName[Value];
      if Result = -1 then
        Result := FNames.Add(PAnsiChar(Value));
    end
    else
      Result := 0;
  end;

  function CreateMember(const MemberName: AnsiString; const Offset: Word; const BaseType: DWORD): TJclTD32MemberSymbolInfo;
  var
    Member: TFieldListElement;
  begin
    Member.Leaf := LF_MEMBER;
    Member.LeafMember.BaseType := BaseType;
    Member.LeafMember.Flags := 3;
    Member.LeafMember.NameIndex := CheckNameIndex(MemberName);
    Member.LeafMember.Reserved := 0;
    Member.LeafMember.Offset := Offset;
    Result := TJclTD32MemberSymbolInfo.Create(pFieldListElement(@Member));
  end;

  function CreatePointer(const ElementType: Integer): Integer;
  var
    TypeInfo: TJclSymbolTypeInfo;
  begin
    TypeInfo := CreateTypeInfo(stkPointer, SizeOf(Pointer));
    TypeInfo.ElementType := ElementType;
    Result := FSymbolTypes.Add(TypeInfo);
  end;

  function CreateSubRange(const SubRangeName: AnsiString; const IndexType, MinValue, MaxValue: Integer; const DataSize: UInt64): Integer;
  var
    TypeInfo: TJclSymbolTypeInfo;
  begin
    TypeInfo := CreateTypeInfo(stkSubrange, 0, CheckNameIndex(SubRangeName));
    TypeInfo.IndexType := IndexType;
    TypeInfo.MinValue := MinValue;
    TypeInfo.MaxValue := MaxValue;
    TypeInfo.DataSize := DataSize;
    Result := FSymbolTypes.Add(TypeInfo);
  end;

  function CreateArray(const ArrayName: AnsiString; const ElementType, IndexType, DataSize, ElementCount: Integer): Integer;
  var
    TypeInfo: TJclSymbolTypeInfo;
  begin
    TypeInfo := CreateTypeInfo(stkArray, 0, CheckNameIndex(ArrayName));
    TypeInfo.ElementType := ElementType;
    TypeInfo.IndexType := IndexType;
    TypeInfo.DataSize := DataSize;
    TypeInfo.MaxValue := ElementCount;
    Result := FSymbolTypes.Add(TypeInfo);
  end;

  function CreateStruct(const StructName: AnsiString; const Length, Members: Integer): Integer;
  var
    TypeInfo: TJclSymbolTypeInfo;
  begin
    TypeInfo := CreateTypeInfo(stkStructure, Length, CheckNameIndex(StructName));
    TypeInfo.Elements := Members;
    Result := FSymbolTypes.Add(TypeInfo);
  end;

type
  TMemberSymbolTypeInfo = packed record
    Leaf: Word;
    Member: TMemberTypeInfo;
  end;

var
  I, NameIndex, FieldListIndex, VoidIndex: Integer;
  FieldList : TJclSymbolTypeInfo;
  SymbolType: TJclSymbolTypeInfo;
begin
  // Create debug info for TVarData type
  if FIsVarTypesExist then
  begin
    NameIndex := CheckNameIndex('TVarData');
    VoidIndex := CreatePointer(0);

    FieldList := CreateTypeInfo(stkFieldList, 0);
    FieldList.Members.Add(CreateMember('ElementCount', 0, $74));
    FieldList.Members.Add(CreateMember('LowBound', 4, $74));
    FieldListIndex := FSymbolTypes.Add(FieldList);

    FieldList := CreateTypeInfo(stkFieldList, 0);
    FieldList.Members.Add(CreateMember('DimCount', 0, $73));
    FieldList.Members.Add(CreateMember('Flags', 2, $73));
    FieldList.Members.Add(CreateMember('ElementSize', 4, $74));
    FieldList.Members.Add(CreateMember('LockCount', 8, $74));
    FieldList.Members.Add(CreateMember('Data', 12, VoidIndex));
    FieldList.Members.Add(CreateMember('Bounds', 16, CreateArray('TVarArrayBoundArray', CreateStruct('TVarArrayBound', 8, FieldListIndex), CreateSubRange('', $74, 0, 0, 1), 8, 1)));
    FieldListIndex := FSymbolTypes.Add(FieldList);

    FieldList := CreateTypeInfo(stkFieldList, 0);
    FieldList.Members.Add(CreateMember('VType', 0, $73));
    FieldList.Members.Add(CreateMember('Reserved1', 2, $73));
    FieldList.Members.Add(CreateMember('Reserved2', 4, $73));
    FieldList.Members.Add(CreateMember('Reserved3', 6, $73));
    FieldList.Members.Add(CreateMember('VSmallInt', 8, $72));
    FieldList.Members.Add(CreateMember('VInteger', 8, $74));
    FieldList.Members.Add(CreateMember('VSingle', 8, $40));
    FieldList.Members.Add(CreateMember('VDouble', 8, $41));
    FieldList.Members.Add(CreateMember('VCurrency', 8, $04));
    FieldList.Members.Add(CreateMember('VDate', 8, $41));
    FieldList.Members.Add(CreateMember('VOleStr', 8, CreatePointer($71)));
    FieldList.Members.Add(CreateMember('VDispatch', 8, VoidIndex));
    FieldList.Members.Add(CreateMember('VError', 8, $74));
    FieldList.Members.Add(CreateMember('VBoolean', 8, CreateSubRange('WordBool', $72, Low(Integer), High(Integer), 2)));
    FieldList.Members.Add(CreateMember('VUnknown', 8, VoidIndex));
    FieldList.Members.Add(CreateMember('VShortInt', 8, $10));
    FieldList.Members.Add(CreateMember('VByte', 8, $20));
    FieldList.Members.Add(CreateMember('VWord', 8, $73));
    FieldList.Members.Add(CreateMember('VLongWord', 8, $75));
    FieldList.Members.Add(CreateMember('VInt64', 8, $76));
    FieldList.Members.Add(CreateMember('VString', 8, VoidIndex));
    FieldList.Members.Add(CreateMember('VAny', 8, VoidIndex));
    FieldList.Members.Add(CreateMember('VArray', 8, CreatePointer(CreateStruct('TVarArray', 24, FieldListIndex))));
    FieldList.Members.Add(CreateMember('VPointer', 8, VoidIndex));
    FieldList.Members.Add(CreateMember('VLongs', 4, CreateArray('', $74, CreateSubRange('', $74, 0, 2, 1), 12, 3)));
    FieldList.Members.Add(CreateMember('VWords', 2, CreateArray('', $73, CreateSubRange('', $74, 0, 6, 1), 14, 7)));
    FieldList.Members.Add(CreateMember('VBytes', 2, CreateArray('', $20, CreateSubRange('', $74, 0, 13, 1), 14, 14)));
    FieldList.Members.Add(CreateMember('RawData', 0, CreateArray('', $74, CreateSubRange('', $74, 0, 3, 1), 16, 4)));
    FieldListIndex := FSymbolTypes.Add(FieldList);

    for I := 0 to SymbolTypeCount - 1 do
    begin
      SymbolType := SymbolTypes[I];
      if (SymbolType <> nil) and (SymbolType.Kind = stkVariant) then
      begin
        SymbolType.Kind := stkStructure;
        SymbolType.NameIndex := NameIndex;
        SymbolType.Elements := FieldListIndex;
      end;
    end;
  end;
end;

//=== { TJclTD32InfoScanner } ================================================

function TJclTD32InfoScanner.LineNumberFromAddr(const AAddr: DWORD): Integer;
var
  Dummy: Integer;
begin
  Result := LineNumberFromAddr(AAddr, Dummy);
end;

function TJclTD32InfoScanner.LineNumberFromAddr(const AAddr: DWORD; var Offset: Integer): Integer;
var
  ASrcMod: TJclTD32SourceModuleInfo;
  ALine: TJclTD32LineInfo;
begin
  if FindSourceModule(AAddr, ASrcMod) and ASrcMod.FindLine(AAddr, ALine) then
  begin
    Result := ALine.LineNo;
    Offset := AAddr - ALine.Offset;
  end
  else
  begin
    Result := 0;
    Offset := 0;
  end;
end;

function TJclTD32InfoScanner.ModuleNameFromAddr(const AAddr: DWORD): AnsiString;
var
  AMod: TJclTD32ModuleInfo;
begin
  if FindModule(AAddr, AMod) then
    Result := Names[AMod.NameIndex]
  else
    Result := '';
end;

function TJclTD32InfoScanner.ProcNameFromAddr(const AAddr: DWORD): AnsiString;
var
  Dummy: Integer;
begin
  Result := ProcNameFromAddr(AAddr, Dummy);
end;

function TJclTD32InfoScanner.ProcNameFromAddr(const AAddr: DWORD; var Offset: Integer): AnsiString;
var
  AProc: TJclTD32ProcSymbolInfo;

  function FormatProcName(const ProcName: AnsiString): AnsiString;
  var
    pchSecondAt, P: PAnsiChar;
  begin
    Result := ProcName;
    if (Length(ProcName) > 0) and (ProcName[1] = '@') then
    begin
      pchSecondAt := System.AnsiStrings.StrScan(PAnsiChar(Copy(ProcName, 2, Length(ProcName) - 1)), '@');
      if pchSecondAt <> nil then
      begin
        Inc(pchSecondAt);
        Result := pchSecondAt;
        P := PAnsiChar(Result);
        while P^ <> #0 do
        begin
          if (pchSecondAt^ = '@') and ((pchSecondAt - 1)^ <> '@') then
            P^ := '.';
          Inc(P);
          Inc(pchSecondAt);
        end;
      end;
    end;
  end;

begin
  if FindProc(AAddr, AProc) then
  begin
    Result := FormatProcName(Names[AProc.NameIndex]);
    Offset := AAddr - AProc.Offset;
  end
  else
  begin
    Result := '';
    Offset := 0;
  end;
end;

function TJclTD32InfoScanner.SourceNameFromAddr(const AAddr: DWORD): AnsiString;
var
  ASrcMod: TJclTD32SourceModuleInfo;
begin
  if FindSourceModule(AAddr, ASrcMod) then
    Result := Names[ASrcMod.NameIndex];
end;

//=== { TJclPeBorTD32Image } =================================================

procedure TJclPeBorTD32Image.AfterOpen;
begin
  inherited AfterOpen;
  CheckDebugData;
end;

procedure TJclPeBorTD32Image.CheckDebugData;
begin
  FTD32DebugDataType := ddtNone;
  FIsTD32DebugPresent := IsDebugInfoInImage(FTD32DebugData);
  if FIsTD32DebugPresent then
    FTD32DebugDataType := ddtInImage
  else
  begin
    FIsTD32DebugPresent := IsDebugInfoInTds(FTD32DebugData);
    if FIsTD32DebugPresent then
      FTD32DebugDataType := ddtTDS;
  end;

  if FIsTD32DebugPresent then
  begin
    FTD32Scanner := TJclTD32InfoScanner.Create(FTD32DebugData);
    if not FTD32Scanner.ValidData then
    begin
      ClearDebugData;
      if not NoExceptions then
        raise EJclError.CreateResFmt(@RsHasNotTD32Info, [FileName]);
    end;
  end;
end;

procedure TJclPeBorTD32Image.Clear;
begin
  ClearDebugData;
  inherited Clear;
end;

procedure TJclPeBorTD32Image.ClearDebugData;
begin
  FIsTD32DebugPresent := False;
  FreeAndNil(FTD32Scanner);
  FreeAndNil(FTD32DebugData);
end;

function TJclPeBorTD32Image.IsDebugInfoInImage(var DataStream: TCustomMemoryStream): Boolean;
var
  DebugDir: TImageDebugDirectory;
  BugDataStart: Pointer;
  DebugDataSize: Integer;
begin
  Result := False;
  DataStream := nil;
  if IsBorlandImage and (DebugList.Count = 1) then
  begin
    DebugDir := DebugList[0];
    if DebugDir._Type = IMAGE_DEBUG_TYPE_UNKNOWN then
    begin
      BugDataStart := RvaToVa(DebugDir.AddressOfRawData);
      DebugDataSize := DebugDir.SizeOfData;
      Result := TJclTD32InfoParser.IsTD32DebugInfoValid(BugDataStart, DebugDataSize);
      if Result then
        DataStream := TJclReferenceMemoryStream.Create(BugDataStart, DebugDataSize);
    end;
  end;
end;

function TJclPeBorTD32Image.IsDebugInfoInTds(var DataStream: TCustomMemoryStream): Boolean;
var
  TdsFileName: TFileName;
  TempStream: TCustomMemoryStream;
begin
  Result := False;
  DataStream := nil;
  TdsFileName := ChangeFileExt(FileName, TurboDebuggerSymbolExt);
  if FileExists(TdsFileName) then
  begin
    TempStream := TJclFileMappingStream.Create(TdsFileName);
    try
      Result := TJclTD32InfoParser.IsTD32DebugInfoValid(TempStream.Memory, TempStream.Size);
      if Result then
        DataStream := TempStream
      else
        TempStream.Free;
    except
      TempStream.Free;
      raise;
    end;
  end;
end;

{ TJclSymbolTypeInfo }

constructor TJclSymbolTypeInfo.Create;
begin
  inherited;

  FMembers := Nil;
  FArgs := Nil;
end;

destructor TJclSymbolTypeInfo.Destroy;
begin
  FreeAndNil(FMembers);
  FreeAndNil(FArgs);

  inherited;
end;

function TJclSymbolTypeInfo.GetArgs: TList;
begin
  if FArgs = Nil then
    FArgs := TList.Create;

  Result := FArgs;
end;

function TJclSymbolTypeInfo.GetMembers: TObjectList;
begin
  if FMembers = Nil then
    FMembers := TObjectList.Create;

  Result := FMembers;
end;

{ TJclTD32ObjNameSymbolInfo }

constructor TJclTD32ObjNameSymbolInfo.Create(pSymInfo: PSymbolInfo; const aID: DWORD);
begin
  Assert(Assigned(pSymInfo));
  inherited Create(pSymInfo, aID);
  with pSymInfo^ do
  begin
    FNameIndex := ObjName.NameIndex;
    FSignature := ObjName.Signature;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
