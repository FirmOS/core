unit fre_db_core;

{
(§LIC)
  (c) Autor,Copyright Dipl.Ing.- Helmut Hartl
      FirmOS Business Solutions GmbH
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2012, FirmOS Business Solutions GmbH
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:

      * Redistributions of source code must retain the above copyright notice,
        this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright notice,
        this list of conditions and the following disclaimer in the documentation
        and/or other materials provided with the distribution.
      * Neither the name of the <FirmOS Business Solutions GmbH> nor the names
        of its contributors may be used to endorse or promote products derived
        from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED.
  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
(§LIC_END)
}

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
{$interfaces corba}

{.$define SANITY_CHECKS}
{.$define DEBUG_TEST_DIRECT_SAVE}
{$define DEBUG_INVALID_USAGE}

// TODO

// Schemes in DB needed as feature ? or all system schemes
// Checkschemefield -> Subobjects, all Field Types
// Make Notifications more granular and based upon transaction change list


interface

uses Sysutils,Classes,fpjson,jsonparser,fos_sparelistgen,
     FRE_DB_INTERFACE,zstream,base64,math,fos_art_tree,
     FRE_SYSTEM,FOS_ARRAYGEN,
     FOS_TOOL_INTERFACES,FOS_REDBLACKTREE_GEN,
     BaseUnix,
     FRE_DB_COMMON,
     FRE_DB_GRAPH;

//TIME has to be stored as UTC Time !!!!
const  c_REFLINK_BLOCK_SIZE=16;

type

  TFRE_DB_ObjectState        = (fdbos_BAD,fdbos_Creating,fdbos_StreamingCreating,fdbos_Dirty,fdbos_Clean,fdbos_StreamingWriting,fdbos_Destroying);
  TFRE_DB_MetadataType       = (fdbmt_Reflinks);

  TFRE_DB_ObjectHdr = packed record
    Signature    : Array [1..6] of Char; //FREDBO
    Version      : Byte;
    EndianMarker : Byte; // 0 = LE / 1 =BE
  end;

const
  cFRE_DB_STREAM_VERSION = 3;
  {$IFDEF ENDIAN_LITTLE}
    cFRE_DB_ENDIAN_MARKER = 0;
  {$ELSE}
    cFRE_DB_ENDIAN_MARKER = 1;
  {$ENDIF}
  CFRE_DB_ObjectHdr  : TFRE_DB_ObjectHdr  = ( Signature : 'FREDBO' ; Version : cFRE_DB_STREAM_VERSION ; EndianMarker : cFRE_DB_ENDIAN_MARKER);

type
  TFRE_DB_Object                = class;
  PFRE_DB_Object                = ^TFRE_DB_Object;

  TFRE_DB_NAMED_OBJECT          = class;

  TFRE_DB_TEXT                  = class;

  TFRE_DB_BASE_CONNECTION       = class;
  TFRE_DB_Connection            = class;

  TFRE_DB_ObjectArray          = Array of TFRE_DB_Object;
  //PFRE_DB_ObjectArray           = ^TFRE_DB_ObjectArray;

  TFRE_DB_ObjLinkArray  = Array of TFRE_DB_GUID;
  PFRE_DB_ObjLinkArray  = ^TFRE_DB_ObjLinkArray;

  TFRE_DB_FieldData=record
    FieldType : TFRE_DB_FIELDTYPE;
    case byte of
     1 : (guid : PFRE_DB_GUIDArray);
     2 : (byte : PFRE_DB_ByteArray);
     3 : (in16 : PFRE_DB_Int16Array);
     4 : (ui16 : PFRE_DB_UInt16Array);
     5 : (in32 : PFRE_DB_Int32Array);
     6 : (ui32 : PFRE_DB_UInt32Array);
     7 : (in64 : PFRE_DB_Int64Array);
     8 : (ui64 : PFRE_DB_UInt64Array);
     9 : (re32 : PFRE_DB_Real32Array);
    10 : (re64 : PFRE_DB_Real64Array);
    11 : (curr : PFRE_DB_CurrencyArray);
    12 : (strg : PFRE_DB_StringArray);
    13 : (bool : PFRE_DB_BoolArray);
    14 : (date : PFRE_DB_DateTimeArray);
    15 : (strm : PFRE_DB_StreamArray);
    16 : (obj  : TFRE_DB_Object);     { object arrays are internally stored as subobject, because a differential update is easier to do }
    17 : (obl  : PFRE_DB_ObjLinkArray);
  end;
  PFRE_DB_FieldData = ^TFRE_DB_FieldData;

  { TFRE_DB_FIELD }
  TFRE_DB_FIELD=class(TFOS_BASE,IFRE_DB_Field,IFRE_DB_CALCFIELD_SETTER) {TODO:Idea / SPLIT IN FIELDTYPES / CLASSES streamfields realfields calcfields ...}
  protected
    FFieldData         : TFRE_DB_FieldData;
    FFieldName         : PFRE_DB_NameType;
    Fobj               : TFRE_DB_Object;      // = nil in Stream only fields
    FManualFieldName   : TFRE_DB_String;      // used for fields without object, (WAL Repair and Streamable Fields) (TODO: check  FFieldName^ cornercases!)
    FObjUidPath        : TFRE_DB_GUIDArray;   { used in stream only fields to know which object the field belongs to}
    FInCollectionArray : TFRE_DB_StringArray; { used in stream only fields to know which collections the field belongs to}
    FSchemePath        : TFRE_DB_StringArray; { used in stream only fields to know which intermediate objects to create}
    FUpObjFieldPath    : TFRE_DB_StringArray; { used in stream only fields to know which intermediate objects to create}
    FIsUidField        : Boolean;
    FIsSchemeField     : Boolean;
    FIsDomainIDField   : Boolean;
    FCalcMethod        : IFRE_DB_CalcMethod;
  private
    procedure  Finalize;
    procedure _InAccessibleFieldCheck  ; inline;
    procedure _CheckEmptyArray         ; inline;
    function  _SchemeClassOfParent     : TFRE_DB_String;


    function  _StreamingSize      : TFRE_DB_SIZE_TYPE;
    procedure _IllegalTypeError   (const ill_type:TFRE_DB_FIELDTYPE);
    procedure _ResultTypeUnset    (const ill_type:TFRE_DB_FIELDTYPE);
    procedure _StringToConvError  (const conv2_type:TFRE_DB_FIELDTYPE);
    procedure _GetHigh            (var hi:integer); inline;

    function  _ConvertToGUID      : TFRE_DB_GUID;
    function  _ConvertToByte      : Byte;
    function  _ConvertToInt16     : SmallInt;
    function  _ConvertToUInt16    : Word;
    function  _ConvertToInt32     : Longint;
    function  _ConvertToUInt32    : Longword;
    function  _ConvertToInt64     : int64;
    function  _ConvertToUInt64    : QWord;
    function  _ConvertToSingle    : Single;
    function  _ConvertToDouble    : Double;
    function  _ConvertToCurrency  : Currency;
    function  _ConvertToString    (const idx:integer=0): TFRE_DB_String;

    function  _ConvertToSignedArray   : TFRE_DB_Int64Array;
    function  _ConvertToUnsignedArray : TFRE_DB_UInt64Array;
    function  _ConvertToCurrencyArray : TFRE_DB_CurrencyArray;
    function  _ConvertToReal64Array   : TFRE_DB_Real64Array;

    function  _ConvertToBool      : Boolean;
    function  _ConvertToDateTime  : TFRE_DB_Datetime64;

    procedure _CheckFieldType    (const expected:TFRE_DB_FIELDTYPE);
    procedure _CheckIndex        (const idx:integer);inline;
    function  _CheckStoreType    (const expected:TFRE_DB_FIELDTYPE):boolean;

    procedure _LocalToUTC        (var arr:TFRE_DB_DateTimeArray);
    procedure _NotAllowedOnUIDorDomainIDFieldCheck;inline;
    procedure _NotAllowedOnSchemeField;inline;

    function  _GetAsGUID         : TFRE_DB_GUID;
    function  GetAsGUID          : TFRE_DB_GUID;
    function  GetAsByte          : Byte;
    function  GetAsInt16         : Smallint;
    function  GetAsInt32         : longint;
    function  GetAsInt64         : int64;
    function  GetAsSingle        : Single;
    function  GetAsDouble        : Double;
    function  GetAsUInt16        : Word;
    function  GetAsUInt32        : longword;
    function  GetAsUInt64        : uint64;
    function  GetAsCurrency      : Currency;
    function  GetAsDateTime      : TFRE_DB_DateTime64;
    function  GetAsDateTimeUTC   : TFRE_DB_DateTime64;
    function  GetAsString        : TFRE_DB_String;
    function  GetAsBoolean       : Boolean;
    function  GetAsObject        : TFRE_DB_Object;
    function  CheckOutObject     : TFRE_DB_Object;
    function  CheckOutObjectArray: IFRE_DB_ObjectArray;
    function  CheckOutObjectArrayItem     (const idx : NAtiveInt): IFRE_DB_Object;
    function  CheckOutObjectI    : IFRE_DB_Object;
    function  GetAsStream        : TFRE_DB_Stream;
    function  GetAsObjectLink    : TFRE_DB_GUID;

    procedure SetAsByte          (const AValue: Byte);
    procedure SetAsInt16         (const AValue: Smallint);
    procedure SetAsInt32         (const AValue: longint);
    procedure SetAsInt64         (const AValue: int64);
    procedure SetAsUInt16        (const AValue: Word);
    procedure SetAsUInt32        (const AValue: longword);
    procedure SetAsUInt64        (const AValue: uint64);
    procedure SetAsSingle        (const AValue: Single);
    procedure SetAsDouble        (const AValue: Double);
    procedure SetAsCurrency      (const AValue: Currency);
    procedure SetAsDateTime      (const AValue: TFRE_DB_Datetime64);
    procedure SetAsDateTimeUTC   (const AValue: TFRE_DB_Datetime64);
    procedure SetAsGUID          (const AValue: TFRE_DB_GUID);
    procedure SetAsObject        (const AValue: TFRE_DB_Object);
    procedure SetAsStream        (const AValue: TFRE_DB_Stream);
    procedure SetAsString        (const AValue: TFRE_DB_String);
    procedure SetAsBoolean       (const AValue: Boolean);
    procedure SetAsObjectLink    (const AValue: TFRE_DB_GUID);

    function  GetAsGUIDArray          : TFRE_DB_GUIDArray;
    function  GetAsByteArray          : TFRE_DB_ByteArray;
    function  GetAsInt16Array         : TFRE_DB_Int16Array;
    function  GetAsInt32Array         : TFRE_DB_Int32Array;
    function  GetAsInt64Array         : TFRE_DB_Int64Array;
    function  GetAsUInt16Array        : TFRE_DB_UInt16Array;
    function  GetAsUInt32Array        : TFRE_DB_UInt32Array;
    function  GetAsUInt64Array        : TFRE_DB_UInt64Array;
    function  GetAsSingleArray        : TFRE_DB_Real32Array;
    function  GetAsDoubleArray        : TFRE_DB_Real64Array;
    function  GetAsDateTimeArray      : TFRE_DB_DateTimeArray;
    function  GetAsDateTimeArrayUTC   : TFRE_DB_DateTimeArray;
    function  GetAsCurrencyArray      : TFRE_DB_CurrencyArray;
    function  GetAsStringArray        : TFRE_DB_StringArray;
    function  GetAsStreamArray        : TFRE_DB_StreamArray;
    function  GetAsBooleanArray       : TFRE_DB_BoolArray;
    function  GetAsObjectArray        : TFRE_DB_ObjectArray;
    function  GetAsObjectLinkArray    : TFRE_DB_ObjLinkArray;

    function  GetAsGUIDList          (idx: Integer): TFRE_DB_GUID;
    function  GetAsByteList          (idx: Integer): Byte;
    function  GetAsInt16List         (idx: Integer): Smallint;
    function  GetAsInt32List         (idx: Integer): longint;
    function  GetAsInt64List         (idx: Integer): int64;
    function  GetAsUInt16List        (idx: Integer): Word;
    function  GetAsUInt32List        (idx: Integer): longword;
    function  GetAsUInt64List        (idx: Integer): uint64;
    function  GetAsSingleList        (idx: Integer): Single;
    function  GetAsDoubleList        (idx: Integer): Double;
    function  GetAsDateTimeList      (idx: Integer): TFRE_DB_DateTime64;
    function  GetAsDateTimeListUTC   (idx: Integer): TFRE_DB_DateTime64;
    function  GetAsCurrencyList      (idx: Integer): Currency;
    function  GetAsStringList        (idx: Integer): TFRE_DB_String;
    function  GetAsStreamList        (idx: Integer): TFRE_DB_Stream;
    function  GetAsBooleanList       (idx: Integer): Boolean;
    function  GetAsObjectList        (idx: Integer): TFRE_DB_Object;
    function  GetAsObjectLinkList    (idx: Integer): TFRE_DB_GUID;

    procedure SetAsByteArray         (const AValue: TFRE_DB_ByteArray);
    procedure SetAsInt16Array        (const AValue: TFRE_DB_Int16Array);
    procedure SetAsInt32Array        (const AValue: TFRE_DB_Int32Array);
    procedure SetAsInt64Array        (const AValue: TFRE_DB_Int64Array);
    procedure SetAsSingleArray       (const AValue: TFRE_DB_Real32Array);
    procedure SetAsUInt16Array       (const AValue: TFRE_DB_UInt16Array);
    procedure SetAsUInt32Array       (const AValue: TFRE_DB_UInt32Array);
    procedure SetAsUInt64Array       (const AValue: TFRE_DB_UInt64Array);
    procedure SetAsCurrencyArray     (const AValue: TFRE_DB_CurrencyArray);
    procedure SetAsDateTimeArray     (const AValue: TFRE_DB_DateTimeArray);
    procedure SetAsDateTimeArrayUTC  (const AValue: TFRE_DB_DateTimeArray);
    procedure SetAsDoubleArray       (const AValue: TFRE_DB_Real64Array);
    procedure SetAsGUIDArray         (const AValue: TFRE_DB_GUIDArray);
    procedure SetAsObjectArray       (const AValue: TFRE_DB_ObjectArray);
    procedure SetAsStreamArray       (const AValue: TFRE_DB_StreamArray);
    procedure SetAsStringArray       (const AValue: TFRE_DB_StringArray);
    procedure SetAsBooleanArray      (const AValue: TFRE_DB_BoolArray);
    procedure SetAsObjectLinkArray   (const Avalue: TFRE_DB_ObjLinkArray);

    procedure SetAsByteList          (idx: Integer; const AValue: Byte);
    procedure SetAsDateTimeUTCList   (idx: Integer; const AValue: TFRE_DB_DateTime64);
    procedure SetAsInt16List         (idx: Integer; const AValue: Smallint);
    procedure SetAsInt32List         (idx: Integer; const AValue: longint);
    procedure SetAsInt64List         (idx: Integer; const AValue: int64);
    procedure SetAsSingleList        (idx: Integer; const AValue: Single);
    procedure SetAsUInt16List        (idx: Integer; const AValue: Word);
    procedure SetAsUInt32List        (idx: Integer; const AValue: longword);
    procedure SetAsUInt64List        (idx: Integer; const AValue: uint64);
    procedure SetAsCurrencyList      (idx: Integer; const AValue: Currency);
    procedure SetAsDateTimeList      (idx: Integer; const AValue: TFRE_DB_Datetime64);
    procedure SetAsDateTimeListUTC   (idx: Integer; const AValue: TFRE_DB_Datetime64);
    procedure SetAsDoubleList        (idx: Integer; const AValue: Double);
    procedure SetAsGUIDList          (idx: Integer; const AValue: TFRE_DB_GUID);
    procedure SetAsObjectList        (idx: Integer; const AValue: TFRE_DB_Object);
    procedure SetAsStreamList        (idx: Integer; const AValue: TFRE_DB_Stream);
    procedure SetAsStringList        (idx: Integer; const AValue: TFRE_DB_String);
    procedure SetAsBooleanList       (idx: Integer; const AValue: Boolean);
    procedure SetAsObjectLinkList    (idx: Integer; const AValue: TFRE_DB_GUID);
    procedure CalculateValue         ; // (re)calculate the value of the field

    procedure   IntfCast                      (const InterfaceSpec:ShortString ; out Intf) ; // Interpret as Object and then -> IntfCast throws an Exception if not succesful
    function    _FieldType        : TFRE_DB_FIELDTYPE;
    procedure   IFRE_DB_Field.CloneFromField = CloneFromFieldI;
    procedure   IFRE_DB_Field.CheckOutObject = CheckOutObjectI;
  public
    function    CloneToNewStreamable    : IFRE_DB_Field;       { This creates a lightweight "streamable field" copy with only certain supported function (fieldvalues,type, but no parentobject etc support }
    function    CloneToNewStreamableObj : IFRE_DB_Object;      { encode the data as object }

    function    GetUpdateObjectUIDPath  : TFRE_DB_GUIDArray;   { This is only set in case of a "clone" stream field (standalone field only) }
    function    GetInCollectionArrayUSL : TFRE_DB_StringArray; { This is only set in case of a "clone" stream field (standalone field only) }
    function    GetUpdateObjSchemePath  : TFRE_DB_StringArray; { This is only set in case of a "clone" stream field (standalone field only) }
    function    GetUpdateObjFieldPath   : TFRE_DB_StringArray; { This is only set in case of a "clone" stream field (standalone field only) }
    function    GetFieldPath            : TFRE_DB_StringArray;

    constructor Create           (const obj:TFRE_DB_Object; const FieldType:TFRE_DB_FIELDTYPE ; const ManualFieldName : string='' ; const calcmethod : IFRE_DB_CalcMethod=nil);reintroduce;
    destructor  Destroy          ;override;
    function    FieldType         : TFRE_DB_FIELDTYPE;
    function    FieldTypeAsString : TFRE_DB_String;
    function    ValueCount        : NativeInt;
    function    ValueCountReal    : NativeInt; { without respect to fake object lists e.g 1 if is object list}
    function    IsUIDField        : boolean;
    function    IsDomainIDField   : boolean;
    function    IsSchemeField     : boolean;
    function    IsSystemField     : boolean;
    function    IsObjectField     : boolean;
    function    IsObjectArray     : boolean;
    function    IsFieldCalculated : boolean;

    procedure   CloneFromField    (const Field:TFRE_DB_FIELD); // Value 0 = Fieldclone
    procedure   CloneFromFieldI   (const Field:IFRE_DB_FIELD);


    function    GetStreamingSize  : TFRE_DB_SIZE_TYPE;
    function    CopyFieldToMem    (var mempointer:Pointer):TFRE_DB_SIZE_TYPE;
    class procedure __ReadHeader  (var memory:pointer;out fieldname:TFRE_DB_NameType);
    procedure   CopyFieldFromMem  (var mempointer:Pointer;const generate_new_uids:boolean ; const version: byte; const endianmarker: byte);

    property  AsGUID                        : TFRE_DB_GUID read GetAsGUID write SetAsGUID;
    property  AsByte                        : Byte  read GetAsByte write SetAsByte;
    property  AsInt16                       : Smallint read GetAsInt16 write SetAsInt16;
    property  AsUInt16                      : Word read GetAsUInt16 write SetAsUInt16;
    property  AsInt32                       : longint read GetAsInt32 write SetAsInt32;
    property  AsUInt32                      : longword read GetAsUInt32 write SetAsUInt32;
    property  AsInt64                       : int64 read GetAsInt64 write SetAsInt64;
    property  AsUInt64                      : uint64 read GetAsUInt64 write SetAsUInt64;
    property  AsReal32                      : Single read GetAsSingle write SetAsSingle;
    property  AsReal64                      : Double read GetAsDouble write SetAsDouble;
    property  AsCurrency                    : Currency read GetAsCurrency write SetAsCurrency;
    property  AsString                      : TFRE_DB_String read GetAsString write SetAsString;
    property  AsBoolean                     : Boolean read GetAsBoolean write SetAsBoolean;
    property  AsDateTime                    : TFRE_DB_DateTime64 read GetAsDateTime write SetAsDateTime;
    property  AsDateTimeUTC                 : TFRE_DB_DateTime64 read GetAsDateTimeUTC write SetAsDateTimeUTC;
    property  AsStream                      : TFRE_DB_Stream read GetAsStream write SetAsStream; // Stores only reference to stream;
    property  AsObject                      : TFRE_DB_Object read GetAsObject write SetAsObject;
    property  AsObjectLink                  : TFRE_DB_GUID read GetAsObjectLink write SetAsObjectLink;

    property  AsGUIDArr                     : TFRE_DB_GUIDArray     read GetAsGUIDArray   write SetAsGUIDArray;
    property  AsByteArr                     : TFRE_DB_ByteArray     read GetAsByteArray   write SetAsByteArray;
    property  AsInt16Arr                    : TFRE_DB_Int16Array    read GetAsInt16Array  write SetAsInt16Array;
    property  AsUInt16Arr                   : TFRE_DB_UInt16Array   read GetAsUInt16Array write SetAsUInt16Array;
    property  AsInt32Arr                    : TFRE_DB_Int32Array    read GetAsInt32Array  write SetAsInt32Array;
    property  AsUInt32Arr                   : TFRE_DB_UInt32Array   read GetAsUInt32Array write SetAsUInt32Array;
    property  AsInt64Arr                    : TFRE_DB_Int64Array    read GetAsInt64Array  write SetAsInt64Array;
    property  AsUInt64Arr                   : TFRE_DB_UInt64Array   read GetAsUInt64Array write SetAsUInt64Array;
    property  AsReal32Arr                   : TFRE_DB_Real32Array   read GetAsSingleArray write SetAsSingleArray;
    property  AsReal64Arr                   : TFRE_DB_Real64Array   read GetAsDoubleArray write SetAsDoubleArray;
    property  AsCurrencyArr                 : TFRE_DB_CurrencyArray read GetAsCurrencyArray write SetAsCurrencyArray;
    property  AsStringArr                   : TFRE_DB_StringArray   read GetAsStringArray write SetAsStringArray;
    property  AsBooleanArr                  : TFRE_DB_BoolArray     read GetAsBooleanArray write SetAsBooleanArray;
    property  AsDateTimeArr                 : TFRE_DB_DateTimeArray read GetAsDateTimeArray write SetAsDateTimeArray;
    property  AsDateTimeUTCArr              : TFRE_DB_DateTimeArray read GetAsDateTimeArrayUTC write SetAsDateTimeArrayUTC;
    property  AsStreamArr                   : TFRE_DB_StreamArray   read GetAsStreamArray write SetAsStreamArray; // Stores only reference to stream;
    property  AsObjectArr                   : TFRE_DB_ObjectArray   read GetAsObjectArray write SetAsObjectArray;
    property  AsObjectLinkArray             : TFRE_DB_ObjLinkArray  read GetAsObjectLinkArray write SetAsObjectLinkArray;

    property  AsGUIDItem        [idx:Integer] : TFRE_DB_GUID read GetAsGUIDList write SetAsGUIDList;
    property  AsByteItem        [idx:Integer] : Byte  read GetAsByteList write SetAsByteList;
    property  AsInt16Item       [idx:Integer] : Smallint read GetAsInt16List write SetAsInt16List;
    property  AsUInt16Item      [idx:Integer] : Word read GetAsUInt16List write SetAsUInt16List;
    property  AsInt32Item       [idx:Integer] : longint read GetAsInt32List write SetAsInt32List;
    property  AsUInt32Item      [idx:Integer] : longword read GetAsUInt32List write SetAsUInt32List;
    property  AsInt64Item       [idx:Integer] : int64 read GetAsInt64List write SetAsInt64List;
    property  AsUInt64Item      [idx:Integer] : uint64 read GetAsUInt64List write SetAsUInt64List;
    property  AsReal32Item      [idx:Integer] : Single read GetAsSingleList write SetAsSingleList;
    property  AsReal64Item      [idx:Integer] : Double read GetAsDoubleList write SetAsDoubleList;
    property  AsCurrencyItem    [idx:Integer] : Currency read GetAsCurrencyList write SetAsCurrencyList;
    property  AsStringItem      [idx:Integer] : TFRE_DB_String read GetAsStringList write SetAsStringList;
    property  AsBooleanItem     [idx:Integer] : Boolean read GetAsBooleanList write SetAsBooleanList;
    property  AsDateTimeItem    [idx:Integer] : TFRE_DB_DateTime64 read GetAsDateTimeList write SetAsDateTimeList;
    property  AsDateTimeUTCItem [idx:Integer] : TFRE_DB_DateTime64 read GetAsDateTimeListUTC write SetAsDateTimeListUTC;
    property  AsStreamItem      [idx:Integer] : TFRE_DB_Stream read GetAsStreamList write SetAsStreamList; // Stores only reference to stream;
    property  AsObjectItem      [idx:Integer] : TFRE_DB_Object read GetAsObjectList write SetAsObjectList;
    property  AsObjectLinkItem  [idx:Integer] : TFRE_DB_GUID read GetAsObjectLinkList write SetAsObjectLinkList;

    procedure AddGuid                       (const value : TFRE_DB_GUID);
    procedure AddByte                       (const value : Byte);
    procedure AddInt16                      (const value : SmallInt);
    procedure AddUInt16                     (const value : Word);
    procedure AddInt32                      (const value : longint);
    procedure AddUInt32                     (const value : longword);
    procedure AddInt64                      (const value : Int64);
    procedure AddUInt64                     (const value : UInt64);
    procedure AddReal32                     (const value : single);
    procedure AddReal64                     (const value : double);
    procedure AddCurrency                   (const value : Currency);
    procedure AddString                     (const value : TFRE_DB_String);
    procedure AddBoolean                    (const value : Boolean);
    procedure AddDateTime                   (const value : TFRE_DB_DateTime64);
    procedure AddDateTimeUTC                (const value : TFRE_DB_DateTime64);
    procedure AddStream                     (const value : TFRE_DB_Stream);
    procedure AddObject                     (const value : TFRE_DB_Object);
    procedure AddObjectLink                 (const value : TFRE_DB_GUID);

    procedure RemoveGuid                    (const idx   : integer);
    procedure RemoveByte                    (const idx   : integer);
    procedure RemoveInt16                   (const idx   : integer);
    procedure RemoveUInt16                  (const idx   : integer);
    procedure RemoveInt32                   (const idx   : integer);
    procedure RemoveUInt32                  (const idx   : integer);
    procedure RemoveInt64                   (const idx   : integer);
    procedure RemoveUInt64                  (const idx   : integer);
    procedure RemoveReal32                  (const idx   : integer);
    procedure RemoveReal64                  (const idx   : integer);
    procedure RemoveCurrency                (const idx   : integer);
    procedure RemoveString                  (const idx   : integer);
    procedure RemoveBoolean                 (const idx   : integer);
    procedure RemoveDateTimeUTC             (const idx   : integer);
    procedure RemoveStream                  (const idx   : integer);
    procedure RemoveObject                  (const idx   : integer);
    procedure RemoveObjectLink              (const idx   : integer);
    function  RemoveObjectLinkByUID         (const to_remove_uid : TFRE_DB_GUID):boolean;


    procedure SetAsEmptyStringArray         ;
    function  IsEmptyArray                  : boolean;

    function  CompareToFieldShallow         (const cmp_fld : TFRE_DB_FIELD):boolean; // Compare to Fields, Valuecount must be same, For Subobjects the the UID's must be same!

    function  GetAsJSON                     (const without_uid:boolean=false;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil;const plain_objects_as_array:boolean=false): TJSONData;
    procedure SetFromJSON                   (const field_type:TFRE_DB_FIELDTYPE;const json_object:TJSONArray;const stream_cb:TFRE_DB_StreamingCallback);
    procedure Stream2String                 (var raw_string:TFRE_DB_RawByteString);
    function  AsObjectArrayJSONString       : TFRE_DB_String; { Deliver an Object Field, or a TFRE_DB_OBJECTLIST as plain JSON Array}


    function  AsStringDump                  : TFRE_DB_String;
    function  FieldName                     : TFRE_DB_NameType;
    procedure Clear                         (const dont_free_streams_and_objects:boolean=false);
    procedure RemoveIndex                   (const idx:integer);
    class function OrderFieldCompare        (const ofieldname : TFRE_DB_String;const o1,o2 : TFRE_DB_Object):integer;

    function  GetAsObjectI                  : IFRE_DB_Object;
    procedure SetAsObjectI                  (const val:IFRE_DB_Object);
    function  GetAsObjectArrayI             : IFRE_DB_ObjectArray;
    procedure SetAsObjectArrayI             (const AValue: IFRE_DB_ObjectArray);

    function  GetAsObjectListI              (idx: Integer): IFRE_DB_Object;
    procedure SetAsObjectListI              (idx: Integer; const AValue: IFRE_DB_Object);
    procedure AddObjectI                    (const obj : IFRE_DB_Object);

    function  ParentObject                  : TFRE_DB_Object; // The object the field belongs to
    function  ParentObjectI                 : IFRE_DB_Object;
    function  AsDBText                      :IFRE_DB_TEXT;


    function  IFRE_DB_Field.GetAsObject             = GetAsObjectI;
    procedure IFRE_DB_Field.SetAsObject             = SetAsObjectI;
    function  IFRE_DB_Field.GetAsObjectArray        = GetAsObjectArrayI;
    function  IFRE_DB_Field.SetAsObjectArray        = SetAsObjectArrayI;
    function  IFRE_DB_Field.GetAsObjectLinkArrayObj = GetAsObjectLinkArrayObjI;
    function  IFRE_DB_Field.SetAsObjectLinkArrayObj = SetAsObjectLinkArrayObjI;
    function  IFRE_DB_Field.GetAsObjectList         = GetAsObjectListI;
    function  IFRE_DB_Field.SetAsObjectList         = SetAsObjectListI;
    function  IFRE_DB_Field.AddObject               = AddObjectI;
    function  IFRE_DB_Field.ParentObject            = ParentObjectI;
    function  IFRE_DB_CALCFIELD_SETTER.SetAsObject  = SetAsObjectI;

    property  AsObjectI                     : IFRE_DB_Object read GetAsObjectI write SetAsObjectI;
    function  IsSpecialClearMarked          : Boolean;               { if a string field and has special clear string mark set => true (usefull for json web interface) }
    function  ConvAsSignedArray             : TFRE_DB_Int64Array;    { for filtering purposes }
    function  ConvAsUnsignedArray           : TFRE_DB_UInt64Array;   { for filtering purposes }
    function  ConvAsCurrencyArray           : TFRE_DB_CurrencyArray; { for filtering purposes }
    function  ConvAsReal64Array             : TFRE_DB_Real64Array;   { for filtering purposes }
  end;
  _TFRE_DB_FieldTree        = specialize TGFOS_RBTree<TFRE_DB_NameType,TFRE_DB_FIELD>;

  TFRE_DB_FieldSchemeDefinition   = class;
  TFRE_DB_FieldIterator           = procedure (const obj:TFRE_DB_Field) is nested;
  TFRE_DB_FieldIteratorBrk        = function  (const obj:TFRE_DB_Field):boolean is nested;
  TFRE_DB_SchemeFieldDef_Iterator = procedure (const obj:TFRE_DB_FieldSchemeDefinition) is nested;
  TFRE_DB_ObjectIteratorBrk       = procedure (const obj:TFRE_DB_Object; var halt:boolean) is nested;

  TFRE_DB_SchemeObject      = class;
  TFRE_DB_OBJECTCLASS       = class of TFRE_DB_Object;
  TFOS_BASECLASS            = class of TFOS_BASE;

  TFRE_DB_ChangeRecord=class
  end;

  OFRE_SL_TFRE_DB_Object  = specialize OFOS_SpareList<TFRE_DB_Object>;
  { TFRE_DB_Object }

  TFRE_DB_Object=class(TFRE_DB_Base,IFRE_DB_Object)
  private
   var
    FUID               : TFRE_DB_GUID;
    FDomainID          : TFRE_DB_GUID;
    FFieldStore        : _TFRE_DB_FieldTree;
    FCacheSchemeObj    : TFRE_DB_SchemeObject;          // Cache ; TFRE_DB_SchemeObject; only link to ... (dont free)
    FParentDBO         : TFRE_DB_FIELD;
    FObjectProps       : TFRE_DB_Object_PropertySet; // Runtime Properties
    FInCollectionarr   : array of TFRE_DB_PERSISTANCE_COLLECTION_BASE;
    FExtensionTag      : Pointer;

    procedure      _RestoreReservedFields              ; { uid, domainid, schemeclass(read only) }
    procedure      ForAll                              (const iter:TFRE_DB_FieldIterator ;const without_calcfields:boolean=false;const without_system_fields:boolean=false);
    procedure      ForAllBrk                           (const iter:TFRE_DB_FieldIteratorBrk;const without_calcfields:boolean=false;const without_system_fields:boolean=false);
    function       _Field                              (name:TFRE_DB_NameType):TFRE_DB_FIELD;
    function       _FieldOnlyExisting                  (name:TFRE_DB_NameType):TFRE_DB_FIELD;
    procedure      _ParentCheck                        (const newdbo : TFRE_DB_Object);
    function       _ReadOnlyCheck                      : boolean;
    procedure      _InAccessibleCheck                  ; inline ;
    function       _ReservedFieldName                 (const upper_name:TFRE_DB_NameType):boolean;
    procedure      _InternalSetMediatorScheme         (const mediator : TFRE_DB_ObjectEx ; const scheme : IFRE_DB_SCHEMEOBJECT);
    function       _InternalDecodeAsField             : IFRE_DB_Field; { create a streaming only lightweight field from the encoding object }
  protected
    FDBO_State      : TFRE_DB_ObjectState;
    function        _ObjectsNeedsNoSubfieldSchemeCheck  : boolean;virtual;
    function        _ObjectIsCodeclassOnlyAndHasNoScheme: boolean;virtual;
    procedure       SetScheme                          (const scheme_obj:TFRE_DB_SchemeObject);
    procedure       SchemeFieldAccessCheck             (const name:TFRE_DB_NameType);virtual;
    function        CalcFieldExists                    (const name:TFRE_DB_NameType;var calculated_field_type:TFRE_DB_FIELDTYPE ; var calcmethod : IFRE_DB_CalcMethod):boolean;
    procedure       InternalSetup                      ; virtual;
    procedure       InternalFinalize                   ; virtual;
    procedure       CopyToMem                          (var mempointer:Pointer);
    procedure       CopyFromMem                        (var mempointer:Pointer;const field_count:TFRE_DB_SIZE_TYPE;const generate_new_uids:boolean=false ; const version: byte=cFRE_DB_STREAM_VERSION ; const endianmarker: byte=cFRE_DB_ENDIAN_MARKER);
    procedure       CopyFromJSON                       (const JSON:TJSONArray;const field_count:TFRE_DB_SIZE_TYPE;const stream_cb:TFRE_DB_StreamingCallback=nil);
    function        CopyToJSON                         : TFRE_DB_String; // without streams // - only stream keys
    class function  CreateInternalStreaming            (const parent:TFRE_DB_FIELD;var mempointer:Pointer;const generate_new_uids:boolean=false;const version : byte=3;const endianmarker : byte=0):TFRE_DB_Object;
    class function  CreateInternalStreamingJSON        (const parent:TFRE_DB_FIELD;const JSON:TJSONArray;const stream_cb:TFRE_DB_StreamingCallback=nil):TFRE_DB_Object;
    function        _StreamingSize                     :TFRE_DB_SIZE_TYPE; // gets calculated before streaming
    procedure       BeforeSave                         ;virtual;
    procedure       AfterSave                          ;virtual;
    procedure       AfterLoad                          ;virtual;
    function        IFRE_DB_Object.ParentField         = ParentFieldI;
    function        IFRE_DB_Object.Parent              = ParentI;
    function        IFRE_DB_Object.FieldPath           = FieldPathI;
    function        IFRE_DB_Object.FieldPathCreate     = FieldPathCreateI;
    function        IFRE_DB_Object.Field               = FieldI;
    function        IFRE_DB_Object.CloneToNewObject    = CloneToNewObjectI;
    function        IFRE_DB_Object.GetScheme           = GetSchemeI;
    function        IFRE_DB_Object.FieldOnlyExistingObj= FieldOnlyExistingObjI;
    function        IFRE_DB_Object.FieldOnlyExisting   = FieldOnlyExistingI;
    procedure       IFRE_DB_Object.CopyField           = CopyFieldI;
    function        IFRE_DB_Object.ObjectRoot          = ObjectRootI;
    function        IFRE_DB_Object.ForAllObjectsBreakHierarchic=ForAllObjectsBreakHierarchicI;
    function        IFRE_DB_Object.FetchObjByUID       = FetchObjByUIDI;

    function        Invoke                             (const method: TFRE_DB_String; const input: IFRE_DB_Object ; const ses : IFRE_DB_Usersession ; const  app : IFRE_DB_APPLICATION ; const conn : IFRE_DB_CONNECTION): IFRE_DB_Object; virtual;
  public
    property        ExtensionTag                       : Pointer  read FExtensionTag write FExtensionTag;
    procedure       _InternalGuidNullCheck;
    procedure       Finalize                           ;
    class procedure  GenerateAnObjChangeList           (const first_obj, second_obj: TFRE_DB_Object ; const InsertCB,DeleteCB : IFRE_DB_Obj_Iterator ; const UpdateCB : IFRE_DB_UpdateChange_Iterator);
    class  function  CompareObjectsEqual               (const first_obj, second_obj: TFRE_DB_Object) :Boolean; { true if equal }

  type
    TFRE_DB_ObjCompareCallback  = procedure(const obj:TFRE_DB_Object ; const compare_event : TFRE_DB_ObjCompareEventType ; const new_fld,old_field:TFRE_DB_FIELD) is nested;
    procedure       __InternalCollectionAdd            (const coll     : TFRE_DB_PERSISTANCE_COLLECTION_BASE);
    function        __InternalCollectionRemove         (const coll     : TFRE_DB_PERSISTANCE_COLLECTION_BASE):NativeInt; // result = new cnt
    function        __InternalCollectionExists         (const coll     : TFRE_DB_PERSISTANCE_COLLECTION_BASE):NativeInt; // -1 = not found, else index
    function        __InternalCollectionExistsName     (const collname : TFRE_DB_NameType):NativeInt; // -1 = not found, else index
    function        __InternalGetCollectionList        :TFRE_DB_PERSISTANCE_COLLECTION_ARRAY;
    function        __InternalGetCollectionListUSL     :TFRE_DB_StringArray; { unique (uppercase) names }
    procedure       __InternalGetFullObjectList        (var list: OFRE_SL_TFRE_DB_Object);
    procedure       __InternalCompareToObj             (const compare_obj : TFRE_DB_Object ; callback : TFRE_DB_ObjCompareCallback);
    function        InternalUniqueDebugKey             : String;
    function        GetDescriptionID                   : String;
    procedure       Set_ReadOnly                       ;
    procedure       Set_Volatile                       ;
    procedure       Set_SystemDB                       ;
    procedure       Set_System                         ;
    procedure       Set_Store_Locked                   (const locked:boolean=true); // Obj is as original in Persistent/MemoryStore Do not read or write it!
    procedure       Set_Store_LockedUnLockedIf         (const locked:boolean ; out lock_state : boolean);
    procedure       Assert_CheckStoreLocked            ;
    procedure       Assert_CheckStoreUnLocked          ;
    procedure       Free                               ;
    procedure       ForAllFields                       (const iter:TFRE_DB_FieldIterator;const without_calcfields:boolean=false;const without_system_fields:boolean=false);
    procedure       ForAllFieldsBreak                  (const iter:TFRE_DB_FieldIteratorBrk;const without_calcfields:boolean=false;const without_system_fields:boolean=false);
    procedure       ForAllFields                       (const iter:IFRE_DB_FieldIterator;const without_calcfields:boolean=false;const without_system_fields:boolean=false);
    procedure       ForAllFieldsBreak                  (const iter:IFRE_DB_FieldIteratorBrk;const without_calcfields:boolean=false;const without_system_fields:boolean=false);
    procedure       ForAllObjects                      (const iter:IFRE_DB_Obj_Iterator);
    procedure       ForAllObjectsFieldName             (const iter:IFRE_DB_Obj_NameIterator);

    function        ForAllObjectsBreakHierarchic       (const iter:TFRE_DB_ObjectIteratorBrk):boolean; // includes root object (self)
    function        GetScheme                          (const raise_non_existing:boolean=false): TFRE_DB_SchemeObject;
    function        GetSchemeI                         (const raise_non_existing:boolean=false): IFRE_DB_SchemeObject;
    function        UID                                : TFRE_DB_GUID;
    function        DomainID                           : TFRE_DB_GUID;
    function        DomainID_String                    : TFRE_DB_GUID_String;
    procedure       SetDomainID                        (const domid:TFRE_DB_GUID);
    function        UID_String                         : TFRE_DB_GUID_String;
    function        UIDP                               : PByte;
    function        PUID                               : PFRE_DB_Guid;
    function        GetAsJSON                          (const without_reserved_fields:boolean=false;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil): TJSONData;virtual;
    function        GetAsJSONString                    (const without_reserved_fields:boolean=false;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil):TFRE_DB_String;virtual;
    function        NeededSize                         : TFRE_DB_SIZE_TYPE;
    function        _ObjectRoot                        : TFRE_DB_Object; // = the last parent with no parent
    function        ObjectRoot                         : TFRE_DB_Object; // = the last parent with no parent
    function        ObjectRootI                        : IFRE_DB_Object; // = the last parent with no parent
    function        IsObjectRoot                       : Boolean;
    function        Parent                             : TFRE_DB_Object;
    function        ParentI                            : IFRE_DB_Object;
    function        ParentField                        : TFRE_DB_FIELD;
    function        ParentFieldI                       : IFRE_DB_FIELD;
    constructor     Create                             ;reintroduce;
    constructor     CreateStreaming                    (const ExtensionObjectMediatorClass:TFRE_DB_OBJECTCLASSEX=nil);
    constructor     CreateStreaming                    (const WeakExObject:TFRE_DB_WeakObjectEx);
    destructor      Destroy                            ;override;
    procedure       CopyToMemory                       (memory : Pointer);
    class function  CreateFromMemory                   (memory : Pointer;const generate_new_uids:boolean=false):TFRE_DB_Object;
    class function  CreateFromString                   (const AValue:TFRE_DB_String;const generate_new_uids:boolean=false):TFRE_DB_Object;
    class function  CreateFromJSONString               (const AValue:TFRE_DB_String;const stream_cb:TFRE_DB_StreamingCallback=nil):TFRE_DB_Object; //
    function        AsString                           :TFRE_DB_String;
    function        FieldI                             (const name:TFRE_DB_NameType):IFRE_DB_FIELD;
    function        Field                              (const name:TFRE_DB_NameType):TFRE_DB_FIELD;virtual;
    function        FieldOnlyExisting                  (const name:TFRE_DB_NameType;out fld:TFRE_DB_FIELD):boolean;
    function        FieldOnlyExistingI                 (const name:TFRE_DB_NameType;var fld:IFRE_DB_FIELD):boolean;
    function        FieldOnlyExistingObj               (const name:TFRE_DB_NameType):TFRE_DB_Object;
    function        FieldOnlyExistingObjI              (const name:TFRE_DB_NameType):IFRE_DB_Object;
    function        FieldOnlyExistingObject            (const name:TFRE_DB_NameType; var obj:IFRE_DB_Object):boolean;
    function        FieldOnlyExistingObjAs             (const name:TFRE_DB_NameType; const classref : TFRE_DB_BaseClass ; var outobj) : boolean;
    function        FieldPath                          (const name:TFRE_DB_String;const dont_raise_ex:boolean=false;const create_non_existing:boolean=false):TFRE_DB_FIELD;
    function        FieldPathI                         (const name:TFRE_DB_String;const dont_raise_ex:boolean=false):IFRE_DB_FIELD;
    function        FieldPathCreate                    (const name:TFRE_DB_String):TFRE_DB_FIELD;
    function        FieldPathCreateI                   (const name:TFRE_DB_String):IFRE_DB_FIELD;
    function        FieldPathExists                    (const name: TFRE_DB_String): Boolean;
    function        FieldPathListFormat                (const field_list:TFRE_DB_NameTypeArray;const formats : TFRE_DB_String;const empty_val: TFRE_DB_String) : TFRE_DB_String;
    function        FieldCount                         (const without_calcfields,without_system_fields:boolean): SizeInt;
    function        DeleteField                        (const name:TFRE_DB_String):Boolean;
    procedure       ClearAllFields                     ;
    function        FieldExists                        (const name:TFRE_DB_String):boolean;
    //procedure       StripOwnedObjects                  ;
    procedure       DumpToStrings                      (const strings:TStrings;indent:integer=0);
    function        DumpToString                       (indent:integer=0;const dump_length_max:Integer=0):TFRE_DB_String;
    function        GetFormattedDisplay                : TFRE_DB_String;
    function        FormattedDisplayAvailable          : boolean;
    function        SubFormattedDisplayAvailable       : boolean;virtual;
    function        GetSubFormattedDisplay             (indent:integer=4):TFRE_DB_String;virtual;
    function        SchemeClass                        : TFRE_DB_NameType;
    function        IsA                                (const schemename:shortstring):Boolean;
    function        IsA                                (const IsSchemeclass : TFRE_DB_OBJECTCLASSEX ; var obj ) : Boolean;
    function        PreTransformedWasA                 (const schemename:shortstring):Boolean;
    function        PreTransformedScheme               : ShortString;
    procedure       SaveToFile                         (const filename:TFRE_DB_String);
    class function  CreateFromFile                     (const filename:TFRE_DB_String):TFRE_DB_Object;
    function        CloneToNewObject                   (const generate_new_uids:boolean=false): TFRE_DB_Object;
    function        CloneToNewObjectI                  (const generate_new_uids:boolean=false): IFRE_DB_Object;

    function        ReferencesObjectsFromData          : Boolean;
    function        ReferencesFromData                 : TFRE_DB_ObjectReferences;

    function        IsSystem                           : Boolean;
    function        IsSystemDB                         : Boolean;
    function        IsVolatile                         : Boolean;
    function        GetFieldListFilter                 (const field_type:TFRE_DB_FIELDTYPE):TFRE_DB_StringArray;
    function        GetUIDPath                         : TFRE_DB_StringArray;
    function        GetSchemePath                      : TFRE_DB_StringArray;
    function        GetUIDPathUA                       : TFRE_DB_GUIDArray;
    function        Mediator                           : TFRE_DB_ObjectEx; // assigned if the Object uses Exended Functionality implemented by a Mediator Object
    function        Properties                         : TFRE_DB_Object_PropertySet;
    procedure       CopyField                          (const obj:TFRE_DB_Object;const field_name:String);
    procedure       CopyFieldI                         (const obj:IFRE_DB_Object;const field_name:String);
    function        FetchObjByUID                      (const childuid:TFRE_DB_GUID):TFRE_DB_Object; // fetches also root

    function        ForAllObjectsBreakHierarchicI      (const iter:IFRE_DB_ObjectIteratorBrk):boolean; // includes root object (self)
    function        GetFullHierarchicObjectList        (const include_self : boolean=false):TFRE_DB_ObjectArray;
    function        FetchObjByUIDI                     (const childuid:TFRE_DB_GUID ; var obj : IFRE_DB_Object):boolean;
    function        FetchObjWithStringFieldValue       (const field_name: TFRE_DB_NameType; const fieldvalue: TFRE_DB_String; var obj: IFRE_DB_Object; ClassnameToMatch: ShortString=''): boolean;
    procedure       SetAllSimpleObjectFieldsFromObject (const source_object : IFRE_DB_Object); // only first level, no uid, domid, obj, objlink fields
  end;

  { TFRE_DB_COMMAND }

  TFRE_DB_COMMAND   = class(TFRE_DB_Object,IFRE_DB_COMMAND)
  private
    FCommand_Id   : TFRE_DB_FIELD;
    FAnswer       : TFRE_DB_FIELD;
    FClient       : TFRE_DB_FIELD;
    FData         : TFRE_DB_FIELD;
    FCtype        : TFRE_DB_FIELD;
    FInvokeClass  : TFRE_DB_FIELD;
    FInvokeMethod : TFRE_DB_FIELD;
    FFatalClose   : TFRE_DB_FIELD;
    FErrorText    : TFRE_DB_FIELD;
    FChangeSession: TFRE_DB_FIELD;
    FIUidPath     : TFRE_DB_FIELD;
    FBinDataKey   : TFRE_DB_FIELD;
    function     _ObjectsNeedsNoSubfieldSchemeCheck: boolean; override;
    function     _ObjectIsCodeclassOnlyAndHasNoScheme: boolean;override;
  protected
    procedure    InternalSetup     ; override;
    function     GetData           : TFRE_DB_Object;
    function     GetDataI          : IFRE_DB_Object;
    function     GetCommandID      : UInt64;
    function     GetIsAnswer       : Boolean;
    function     GetIsClient       : Boolean;
    procedure    SetData           (const AValue: TFRE_DB_Object);
    procedure    SetDataI          (const AValue: IFRE_DB_Object);
    procedure    SetCommandID      (const AValue: UInt64);
    procedure    SetIsAnswer       (const AValue: Boolean);
    procedure    SetIsClient       (const AValue: Boolean);
    procedure    CopyToMemory      (memory : Pointer);
    function     GetInvokeClass    : String;
    function     GetInvokeMethod   : String;
    procedure    SetInvokeClass    (AValue: String);
    procedure    SetInvokeMethod   (AValue: String);
    function     GetEText         : TFRE_DB_String;
    function     GetFatalClose    : Boolean;
    function     GetChangeSessionKey  : String;
    procedure    SetChangeSessionKey  (AValue: String);
    procedure    SetFatalClose    (AValue: Boolean);
    procedure    SetEText         (AValue: TFRE_DB_String);
    procedure    SetBinDataKey    (AValue: string);
    function     GetBinDataKey    : string;

    function     GetUidPath       : TFRE_DB_GUIDArray;
    procedure    SetUidPath       (AValue: TFRE_DB_GUIDArray);
    function     GetCType         : TFRE_DB_COMMANDTYPE;
    procedure    SetCType         (AValue: TFRE_DB_COMMANDTYPE);
    procedure    SetAnswerInterface (const answer_interface : IFRE_DB_COMMAND_REQUEST_ANSWER_SC);
    function     GetAnswerInterface :IFRE_DB_COMMAND_REQUEST_ANSWER_SC;
    function     AsJSONString  : TFRE_DB_RawByteString;
    function     AsDBODump     : TFRE_DB_RawByteString;

    procedure    IFRE_DB_COMMAND.SetData      = SetDataI;
    function     IFRE_DB_COMMAND.GetData      = GetDataI;
    function     IFRE_DB_COMMAND.CheckoutData = CheckoutDataI;

  public
    function     CheckoutData  : TFRE_DB_Object;
    function     CheckoutDataI : IFRE_DB_Object;

    property     Answer        : Boolean read GetIsAnswer  write SetIsAnswer;
    property     ClientCommand : Boolean read GetIsClient  write SetIsClient;

    property     CommandID     : UInt64              read GetCommandID        write SetCommandID;
    property     InvokeClass   : String              read GetInvokeClass      write SetInvokeClass;
    property     InvokeMethod  : String              read GetInvokeMethod     write SetInvokeMethod;
    property     Data          : IFRE_DB_Object      read GetDataI            write SetDataI;
    property     UidPath       : TFRE_DB_GUIDArray   read GetUidPath          write SetUidPath;
    property     CommandType   : TFRE_DB_COMMANDTYPE read GetCType            write SetCType;
    property     ErrorText     : TFRE_DB_String      read GetEText            write SetEText;
    property     FatalClose    : Boolean             read GetFatalClose       write SetFatalClose;
    property     ChangeSessionKey : String           read GetChangeSessionKey write SetChangeSessionKey; // Should be only set in Answer to Force the Client to update his SessionID
  end;

  { TFRE_DB_OBJECTLIST }

  TFRE_DB_OBJECTLIST=class(TFRE_DB_OBJECT) {}
  private
    function  GetAsArray   (const idx : NativeInt): TFRE_DB_Object;
    procedure SetAsArray   (const idx : NativeInt; AValue: TFRE_DB_Object);
    procedure _SetCount    (const val : NativeInt);
    function  _Checkindex  (const idx:Nativeint):NativeInt;
  protected
  public
    function  GetCount          : NativeInt;
    procedure ClearArray        (const free_objs : boolean=true);
    procedure AddObject         (const obj : TFRE_DB_Object);
    procedure RemoveObject      (const idx : Nativeint);
    function  CheckoutObject    (const idx : Nativeint) : TFRE_DB_Object;
    function  GetAsObjectArray  : TFRE_DB_ObjectArray;
    function  CheckOutAsArray   : TFRE_DB_ObjectArray;
    procedure SetAsObjectArray  (const arr : TFRE_DB_ObjectArray);
    property  AsArray           [const idx : NativeInt] : TFRE_DB_Object read GetAsArray write SetAsArray; default;
  end;


  { TFRE_DB_NAMED_OBJECT }

  TFRE_DB_NAMED_OBJECT=class(TFRE_DB_OBJECT,IFRE_DB_NAMED_OBJECT)
  private
    function  GetDesc         : TFRE_DB_TEXT;
    function  GetName         : TFRE_DB_String;
    procedure SetDesc         (const AValue: TFRE_DB_TEXT);
    procedure SetName         (const AValue: TFRE_DB_String);
  protected
    function  GetDescI        : IFRE_DB_TEXT;
    procedure SetDescI        (const AValue: IFRE_DB_TEXT);
  public
    function  IFRE_DB_NAMED_OBJECT.GetDesc          = GetDescI;
    function  IFRE_DB_NAMED_OBJECT.SetDesc          = SetDescI;
    function  IFRE_DB_NAMED_OBJECT.ParentField      = ParentFieldI;
    function  IFRE_DB_NAMED_OBJECT.Parent           = ParentI;
    function  IFRE_DB_NAMED_OBJECT.FieldPath        = FieldPathI;
    function  IFRE_DB_NAMED_OBJECT.FieldPathCreate  = FieldPathCreateI;
    function  IFRE_DB_NAMED_OBJECT.Field            = FieldI;
    function  IFRE_DB_NAMED_OBJECT.GetScheme        = GetSchemeI;
    function  IFRE_DB_NAMED_OBJECT.CloneToNewObject = CloneToNewObjectI;
    function  IFRE_DB_NAMED_OBJECT.FieldOnlyExistingObj = FieldOnlyExistingObjI;
    function  IFRE_DB_NAMED_OBJECT.FieldOnlyExisting    = FieldOnlyExistingI;
    procedure IFRE_DB_NAMED_OBJECT.CopyField            = CopyFieldI;
    function  IFRE_DB_NAMED_OBJECT.ObjectRoot           = ObjectRootI;
    function  IFRE_DB_NAMED_OBJECT.ForAllObjectsBreakHierarchic=ForAllObjectsBreakHierarchicI;
    function  IFRE_DB_NAMED_OBJECT.FetchObjByUID       = FetchObjByUIDI;

    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    property  ObjectName      : TFRE_DB_String       read GetName write SetName;
    property  Description     : TFRE_DB_TEXT read GetDesc write SetDesc;
  end;

  { TFRE_DB_Enum }

  TFRE_DB_Enum=class(TFRE_DB_NAMED_OBJECT,IFRE_DB_ENUM)
  protected
    function  _ObjectIsCodeclassOnlyAndHasNoScheme: boolean;override;
    function  IFRE_DB_Enum.GetDesc     = GetDescI;
    function  IFRE_DB_ENum.SetDesc     = SetDescI;
    function  IFRE_DB_Enum.ParentField = ParentFieldI;
    function  IFRE_DB_Enum.Parent      = ParentI;
    function  IFRE_DB_Enum.FieldPath   = FieldPathI;
    function  IFRE_DB_Enum.Setup       = SetupI;
    procedure IFRE_DB_Enum.addEntry    = addEntryI;
    procedure IFRE_DB_Enum.getEntries  = getEntriesI;
    function  IFRE_DB_Enum.Field       = FieldI;
    function  IFRE_DB_Enum.GetScheme   = GetSchemeI;
    function  IFRE_DB_Enum.CloneToNewObject    = CloneToNewObjectI;
    function  IFRE_DB_Enum.CheckField  = CheckFieldI;
  public
    function  Setup       (const infoText: TFRE_DB_TEXT): TFRE_DB_Enum;
    function  SetupI      (const infoText: IFRE_DB_TEXT): IFRE_DB_Enum;
    procedure addEntry    (const value:TFRE_DB_String;const cap_trans_key: TFRE_DB_String);
    procedure addEntryI   (const value:TFRE_DB_String;const cap_trans_key: TFRE_DB_String);
    function  getEntries  :TFRE_DB_ObjectArray;
    function  getEntriesI :IFRE_DB_ObjectArray;
    function  getCaptions (const conn: IFRE_DB_CONNECTION):TFRE_DB_StringArray;
    function  getCaption  (const conn: IFRE_DB_CONNECTION; const value: TFRE_DB_String):TFRE_DB_String;
    function  CheckField  (const field_to_check:TFRE_DB_FIELD;const raise_exception:boolean):boolean; virtual;
    function  CheckFieldI (const field_to_check:IFRE_DB_FIELD;const raise_exception:boolean):boolean; virtual;
  end;

  { TFRE_DB_ClientFieldValidator }

  TFRE_DB_ClientFieldValidator=class(TFRE_DB_NAMED_OBJECT,IFRE_DB_ClientFieldValidator)
  protected
    function  _ObjectIsCodeclassOnlyAndHasNoScheme: boolean;override;
    function  IFRE_DB_ClientFieldValidator.GetDesc     = GetDescI;
    function  IFRE_DB_ClientFieldValidator.SetDesc     = SetDescI;
    function  IFRE_DB_ClientFieldValidator.ParentField = ParentFieldI;
    function  IFRE_DB_ClientFieldValidator.Parent      = ParentI;
    function  IFRE_DB_ClientFieldValidator.FieldPath   = FieldPathI;
    function  IFRE_DB_ClientFieldValidator.Field       = FieldI;
    function  IFRE_DB_ClientFieldValidator.GetScheme   = GetSchemeI;
    function  IFRE_DB_ClientFieldValidator.CloneToNewObject = CloneToNewObjectI;
    function  IFRE_DB_ClientFieldValidator.getInfoText = getInfoTextI;
    function  IFRE_DB_ClientFieldValidator.getHelpText = getHelpTextI;
    function  IFRE_DB_ClientFieldValidator.getConfigParams = getConfigParamsI;
    function  IFRE_DB_ClientFieldValidator.setConfigParams = setConfigParamsI;
  public
    function  Setup            (const regExp:TFRE_DB_String; const infoText: IFRE_DB_TEXT; const help_trans_key: TFRE_DB_String=''; const allowedChars:TFRE_DB_String=''): IFRE_DB_ClientFieldValidator;
    function  getRegExp        :TFRE_DB_String;
    function  getInfoText      :TFRE_DB_TEXT;
    function  getInfoTextI     :IFRE_DB_TEXT;
    function  getHelpTextKey   :TFRE_DB_String;
    function  getAllowedChars  :TFRE_DB_String;
    function  CheckField       (const field_to_check:TFRE_DB_FIELD;const raise_exception:boolean):boolean; virtual;
  end;

  { TFRE_DB_FieldSchemeDefinition }

  type
    OFRE_SL_R_Depfield    = specialize OFOS_SpareList<R_Depfieldfield>;
    OFRE_SL_R_VisDepfield = specialize OFOS_SpareList<R_VisDepfieldfield>;

  PFRE_DB_FieldSchemeDefinition = ^TFRE_DB_FieldSchemeDefinition;

  TFRE_DB_FieldSchemeDefinition=class(TObject,IFRE_DB_FieldSchemeDefinition)
  private
    FScheme       : TFRE_DB_SchemeObject;
    FFieldName    : TFRE_DB_NameType;
    FFieldType    : TFRE_DB_FIELDTYPE;

    FSubscheme    : TFRE_DB_NameType;
    FSubSchemeObj : TFRE_DB_SchemeObject; // Cache;
    Frequired     : Boolean;
    FisPass       : Boolean;
    FaddConfirm   : Boolean;
    FmultiValues  : Boolean;

    FEnum         : TFRE_DB_Enum;
    Fvalidator    : TFRE_DB_ClientFieldValidator;
    FDepFields    : OFRE_SL_R_Depfield;
    FVisDepFields : OFRE_SL_R_VisDepfield;

    FvalidatorParams: TFRE_DB_Object;

    FCalcMethod         : IFRE_DB_CalcMethod;

    function   getAddConfirm       : Boolean;
    function   getEnum             : TFRE_DB_Enum;
    function   GetFieldName        : TFRE_DB_NameType;
    function   GetFieldType        : TFRE_DB_FIELDTYPE;
    function   getIsPass           : Boolean;
    function   GetSubSchemeName    : TFRE_DB_NameType;
    function   getMultiValues      : Boolean;
    function   getRequired         : Boolean;
    function   getValidator        : TFRE_DB_ClientFieldValidator;
    function   getValidatorParams  : IFRE_DB_Object;
    function   getValidatorI       (var validator: IFRE_DB_ClientFieldValidator):boolean;
    function   getEnumI            (var enum : IFRE_DB_Enum) : boolean;

    procedure  setAddConfirm       (AValue: Boolean);
    procedure  setisPass           (AValue: Boolean);
    procedure  setMultiValues      (AValue: Boolean);
    procedure  setRequired         (AValue: Boolean);
    procedure  setEnum             (AValue: TFRE_DB_Enum);
    procedure  setValidator        (AValue: TFRE_DB_ClientFieldValidator);
    function   getParentScheme     : TFRE_DB_SchemeObject;
  protected
    function   IFRE_DB_FieldSchemeDefinition.getEnum = getEnumI;
    function   IFRE_DB_FieldSchemeDefinition.setEnum = setEnumI;
    function   IFRE_DB_FieldSchemeDefinition.getValidator  = getValidatorI;
    function   IFRE_DB_FieldSchemeDefinition.setValidator  = setValidatorI;
    function   IFRE_DB_FieldSchemeDefinition.getDepFields  = getDepFieldsI;
    function   IFRE_DB_FieldSchemeDefinition.SetupFieldDef = SetupFieldDefI;
    function   IFRE_DB_FieldSchemeDefinition.GetSubScheme  = GetSubSchemeI;
    function   IFRE_DB_FieldSchemeDefinition.ValidateField = ValidateFieldI;
  public
    constructor Create;
    destructor  Destroy; override;
    function   IsACalcField      : Boolean;
    function   SetupFieldDef     (const is_required:boolean;const is_multivalue:boolean=false;const enum_key:TFRE_DB_NameType='';const validator_key:TFRE_DB_NameType='';const is_pass:Boolean=false;const add_confirm:Boolean=false ; const validator_params : TFRE_DB_Object=nil):TFRE_DB_FieldSchemeDefinition;
    function   SetupFieldDefI    (const is_required:boolean;const is_multivalue:boolean=false;const enum_key:TFRE_DB_NameType='';const validator_key:TFRE_DB_NameType='';const is_pass:Boolean=false; const add_confirm:Boolean=false ; const validator_params : IFRE_DB_Object=nil):IFRE_DB_FieldSchemeDefinition;
    procedure  SetCalcMethod     (const calc_method:IFRE_DB_CalcMethod);
    //function   CalcField         : TFRE_DB_FIELD;
    procedure  addDepField       (const fieldName: TFRE_DB_String;const disablesField: Boolean=true);
    procedure  ForAllDepfields   (const depfielditerator : TFRE_DB_Depfielditerator);
    procedure  addVisDepField    (const fieldName: TFRE_DB_String;const visibleValue:String);
    procedure  ForAllVisDepfields(const depfielditerator : TFRE_DB_VisDepfielditerator);
    property   FieldName         :TFRE_DB_NameType  read GetFieldName;
    property   FieldType         :TFRE_DB_FIELDTYPE read GetFieldType;
    property   SubschemeName     :TFRE_DB_NameType  read GetSubSchemeName;
    function   GetSubScheme      :TFRE_DB_SchemeObject;
    function   GetSubSchemeI     :IFRE_DB_SchemeObject;
    property   required          :Boolean read getRequired write setRequired;
    property   isPass            :Boolean read getIsPass write setIsPass;
    property   addConfirm        :Boolean read getAddConfirm write setAddConfirm;
    property   multiValues       :Boolean read getMultiValues write setMultiValues;
    function   ValidateField     (const field_to_check:TFRE_DB_FIELD;const raise_exception:boolean=true):boolean;
    function   ValidateFieldI    (const field_to_check:IFRE_DB_FIELD;const raise_exception:boolean=true):boolean;
  end;

  { TFRE_DB_InputGroupSchemeDefinition }

  OFRE_SL_TFRE_InputFieldDef4Group  = specialize OFOS_SpareList<OFRE_InputFieldDef4Group>;

  PFRE_DB_InputGroupSchemeDefinition = ^TFRE_DB_InputGroupSchemeDefinition;

  TFRE_DB_InputGroupSchemeDefinition=class(TObject,IFRE_DB_InputGroupSchemeDefinition)
  private
    FScheme      : TFRE_DB_SchemeObject;
    Fields       : OFRE_SL_TFRE_InputFieldDef4Group;
    groupid      : TFRE_DB_NameType;
    FCaption_Key : TFRE_DB_NameType;
    procedure   _addInput        (const schemefield: TFRE_DB_String; const cap_trans_key: TFRE_DB_String; const disabled: Boolean;const hidden:Boolean; const field_backing_collection: TFRE_DB_String;const fbCollectionIsDerivedCollection:boolean; const std_right:TFRE_DB_STANDARD_RIGHT; const rightClasstype: TClass; const hideSingle: Boolean; const chooser_type:TFRE_DB_CHOOSER_DH; const standard_coll: TFRE_DB_STANDARD_COLL=coll_NONE;const chooserAddEmptyForRequired:Boolean=false);
  protected
    function  GetInputGroupID    : TFRE_DB_String;
    procedure SetCaptionKey      (AValue: TFRE_DB_String);

    function  IFRE_DB_InputGroupSchemeDefinition.Setup           = SetupI;
    function  IFRE_DB_InputGroupSchemeDefinition.GetScheme       = GetSchemeI;
    function  IFRE_DB_InputGroupSchemeDefinition.GetParentScheme = GetParentSchemeI;
    function  SetupI              (const caption: TFRE_DB_String):IFRE_DB_InputGroupSchemeDefinition;
    function  GetParentSchemeI    : IFRE_DB_SchemeObject;
    function  FieldDefIsNull      (const obj   : PFRE_InputFieldDef4Group):boolean;
    function  FieldDefCompare     (const o1,o2 : PFRE_InputFieldDef4Group):boolean;
    function  GetCaptionKey      : TFRE_DB_NameType;
  public
    constructor Create             (const gid : TFRE_DB_NameType ; scheme : TFRE_DB_SchemeObject);
    destructor  Destroy            ; override;
    function    Setup              (const cap_key: TFRE_DB_String):TFRE_DB_InputGroupSchemeDefinition;
    procedure   AddInput           (const schemefield: TFRE_DB_String; const cap_trans_key: TFRE_DB_String=''; const disabled: Boolean=false;const hidden:Boolean=false; const field_backing_collection: TFRE_DB_String='';const fbCollectionIsDerivedCollection:Boolean=false; const chooser_type:TFRE_DB_CHOOSER_DH=dh_chooser_combo;const standard_coll:TFRE_DB_STANDARD_COLL=coll_NONE;const chooserAddEmptyForRequired: Boolean=false);
    procedure   AddDomainChooser   (const schemefield: TFRE_DB_String; const std_right:TFRE_DB_STANDARD_RIGHT; const rightClasstype: TClass; const hideSingle: Boolean; const cap_trans_key: TFRE_DB_String='');
    procedure   UseInputGroup      (const scheme,group: TFRE_DB_String; const addPrefix: TFRE_DB_String='';const as_gui_subgroup:boolean=false ; const collapsible:Boolean=false;const collapsed:Boolean=false);
    function    GroupFields        : PFRE_InputFieldDef4GroupArr;
  end;


  OFRE_SL_InputGroups     = specialize OFOS_SpareList<TFRE_DB_InputGroupSchemeDefinition>;
  OFRE_SL_FieldSchemeDefs = specialize OFOS_SpareList<TFRE_DB_FieldSchemeDefinition>;
  { TFRE_DB_SchemeObject }

  TFRE_DB_SchemeObject=class(TObject,IFRE_DB_SCHEMEOBJECT)
  private
    FSchemeClass      : TFRE_DB_NameType;
    FHasHardcodeClass : Boolean;
    FFieldDefs        : OFRE_SL_FieldSchemeDefs;
    FMethodDefs       : TFRE_DB_Object;
    FInputGroups      : OFRE_SL_InputGroups;
    FStrict           : Boolean;             // Only defined Fields Allowed
    FParentScheme     : TFRE_DB_SchemeObject;
    FIMI_Methods      : TFRE_DB_StringArray;
    FSchemeType       : TFRE_DB_SchemeType;
    FHC_MethodsBuild  : Boolean;
    FHardCodeClassTyp : TClass;

    simple_df         : TFRE_DB_String;
    formatted_df      : TFRE_DB_NameTypeArray;
    formatted_dff     : TFRE_DB_String;

    FExplanation      : TFRE_DB_String;
    procedure      _InternalSetParentScheme(const parentscheme:TFRE_DB_Schemeobject);
    procedure      _BuildHardcodeMethods;
    procedure      _FieldAccessCheck(name: TFRE_DB_NameType);
    function       Implementor: TObject;
  protected

    function  CalcFieldExists           (const name: TFRE_DB_NameType; var calculated_field_type : TFRE_DB_FIELDTYPE ; var calcmethod : IFRE_DB_CalcMethod): boolean;
    procedure ForAllCalculatedFields    (const iter:TFRE_DB_FieldIterator ; const obj: TFRE_DB_Object);
    procedure ForAllCalculatedFieldsBrk (const iter:TFRE_DB_FieldIteratorBrk ; const obj : TFRE_DB_Object);

    function  ConstructNewInstance      (const fail_on_no_cc:boolean=true):TFRE_DB_Object;
    procedure SetupMediator             (const dbo:TFRE_DB_Object);
    function  HasHardCodeClass          : Boolean;

    function  GetFieldDef               (const UPPER_fieldname:TFRE_DB_NameType ; var fd : TFRE_DB_FieldSchemeDefinition) : boolean;

    function  IFRE_DB_SCHEMEOBJECT.InvokeMethod_UID          = InvokeMethod_UIDI;
    function  IFRE_DB_SCHEMEOBJECT.AddSchemeField            = AddSchemeFieldI;
    function  IFRE_DB_SCHEMEOBJECT.AddSchemeFieldSubscheme   = AddSchemeFieldSubSchemeI;
    function  IFRE_DB_SCHEMEOBJECT.GetSchemeField            = GetSchemeFieldI;
    function  IFRE_DB_SCHEMEOBJECT.GetFormattedDisplay       = GetFormattedDisplayI;
    function  IFRE_DB_SCHEMEOBJECT.FormattedDisplayAvailable = FormattedDisplayAvailableI;

    function  IFRE_DB_SCHEMEOBJECT.GetParentScheme           = GetParentSchemeI;
    function  IFRE_DB_SCHEMEOBJECT.SetObjectFieldsWithScheme = SetObjectFieldsWithSchemeI;
    function  IFRE_DB_SCHEMEOBJECT.AddInputGroup             = AddInputGroupI;
    function  IFRE_DB_SCHEMEOBJECT.GetInputGroup             = GetInputGroupI;
    function  IFRE_DB_SCHEMEOBJECT.ValidateObject            = ValidateObjectI;
    function  IFRE_DB_SCHEMEOBJECT.getSchemeFields           = getSchemeFieldsI;
    function  IFRE_DB_SCHEMEOBJECT.InvokeMethod              = InvokeMethodI;
    function  IFRE_DB_SCHEMEOBJECT.GetDBConnection           = GetDBConnectionI;
    function  IFRE_DB_SCHEMEOBJECT.UpdateSchemeField         = UpdateSchemeFieldI;
    function  IFRE_DB_SCHEMEOBJECT.ForAllFieldSchemeDefinitions = ForAllFieldSchemeDefinitionsI;
  public
    constructor create;
    destructor  Destroy;override;
    procedure ForAllFieldSchemeDefinitionsI (const iterator:IFRE_DB_SchemeFieldDef_Iterator);
    procedure ForAllFieldSchemeDefinitions (const iterator:TFRE_DB_SchemeFieldDef_Iterator);

    function  InvokeMethod_UID_Session  (const instance:TFRE_DB_GUIDArray;const class_name,meth_name:TFRE_DB_String;var in_params:TFRE_DB_Object;const connection:TFRE_DB_CONNECTION; const session: TFRE_DB_UserSession) : IFRE_DB_Object;
    function  InvokeMethod_UIDI         (const obj_uid : TFRE_DB_GUID;const obj_methodname:TFRE_DB_String;var input:IFRE_DB_Object;const connection:IFRE_DB_CONNECTION):IFRE_DB_Object;

    function  GetAll_IMI_Methods        :TFRE_DB_StringArray;
    function  MethodExists              (const name:TFRE_DB_String):boolean;
    function  AddSchemeField            (const newfieldname:TFRE_DB_NameType ; const newfieldtype:TFRE_DB_FIELDTYPE ; const sub_scheme:TFRE_DB_NameType ):TFRE_DB_FieldSchemeDefinition;
    function  AddSchemeFieldI           (const newfieldname:TFRE_DB_NameType ; const newfieldtype:TFRE_DB_FIELDTYPE):IFRE_DB_FieldSchemeDefinition;
    function  AddCalcSchemeField        (const newfieldname:TFRE_DB_NameType ; const newfieldtype:TFRE_DB_FIELDTYPE ; const calc_method  : IFRE_DB_CalcMethod):IFRE_DB_FieldSchemeDefinition;
    function  AddSchemeFieldSubscheme   (const newfieldname:TFRE_DB_NameType ; const sub_scheme:TFRE_DB_NameType):TFRE_DB_FieldSchemeDefinition;
    function  AddSchemeFieldSubschemeI  (const newfieldname:TFRE_DB_NameType ; const sub_scheme:TFRE_DB_NameType):IFRE_DB_FieldSchemeDefinition;
    function  GetSchemeField            (const fieldname   :TFRE_DB_NameType ; var fieldschemedef:TFRE_DB_FieldSchemeDefinition): boolean;
    function  GetSchemeFieldI           (const fieldname   :TFRE_DB_NameType ; var fieldschemedef:IFRE_DB_FieldSchemeDefinition): boolean;
    function  GetSchemeField            (const fieldname   :TFRE_DB_NameType): TFRE_DB_FieldSchemeDefinition;
    function  GetSchemeFieldI           (const fieldname   :TFRE_DB_NameType): IFRE_DB_FieldSchemeDefinition;
    function  IsA                       (const schemename :shortstring):Boolean;
    procedure SetSimpleSysDisplayField  (const field_name  :TFRE_DB_String);
    procedure SetSysDisplayField        (const field_names :TFRE_DB_NameTypeArray;const format:TFRE_DB_String);
    function  GetFormattedDisplay       (const obj : TFRE_DB_Object):TFRE_DB_String;
    function  GetFormattedDisplayI      (const obj : IFRE_DB_Object):TFRE_DB_String;
    function  FormattedDisplayAvailable (const obj : TFRE_DB_Object):boolean;
    function  FormattedDisplayAvailableI(const obj : IFRE_DB_Object):boolean;
    function  DefinedSchemeName         : TFRE_DB_String;
    procedure Strict                    (const only_defined_fields:boolean);
    procedure SetParentSchemeByName     (const parentschemename:TFRE_DB_String);
    function  GetParentScheme           :TFRE_DB_SchemeObject;
    function  GetParentSchemeI          :IFRE_DB_SchemeObject;
    function  GetParentSchemeName       :TFRE_DB_String;
    function  GetExplanation            :TFRE_DB_String;
    procedure SetExplanation            (AValue: TFRE_DB_String);
    function  GetSchemeType             : TFRE_DB_SchemeType;
    procedure SetObjectFieldsWithScheme (const Raw_Object: TFRE_DB_OBject; const Update_Object: TFRE_DB_Object;const new_object:boolean;const DBConnection:TFRE_DB_CONNECTION;const schemeType: TFRE_DB_String='');
    procedure SetObjectFieldsWithSchemeI(const Raw_Object: IFRE_DB_OBject; const Update_Object: IFRE_DB_Object;const new_object:boolean;const DBConnection:IFRE_DB_CONNECTION;const schemeType: TFRE_DB_String='');
    function  ReplaceInputGroup         (const id: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;
    function  AddInputGroup             (const id: TFRE_DB_String): TFRE_DB_InputGroupSchemeDefinition;
    function  AddInputGroupI            (const id: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;
    function  GetInputGroup             (const id: TFRE_DB_String): TFRE_DB_InputGroupSchemeDefinition;
    function  GetInputGroupI            (const name: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;

    function  ValidateObject            (const dbo : TFRE_DB_Object;const raise_errors:boolean=true):boolean;
    function  ValidateObjectI           (const dbo : IFRE_DB_Object;const raise_errors:boolean=true):boolean;
  end;


  { TFRE_DB_Object_ManageInfo }
  TFRE_DB_COLLECTION = class;

  //TFRE_DB_CollectionAdd_CBN = procedure (const collection_name:TFRE_DB_NameType;const pers_collection:IFRE_DB_PERSISTANCE_COLLECTION) is nested;
  TFRE_DB_MasterCollAdd_CBN = procedure (const guid:TFRE_DB_GUID) is nested;
  TFRE_DB_AddMetaData_CB    = procedure (const meta_type:TFRE_DB_MetadataType;const obj:TFRE_DB_Object) is nested;

  TFRE_DB_Obj_Iterator                  = procedure (const obj:TFRE_DB_Object) is nested;
  TFRE_DB_Scheme_Iterator               = procedure (const obj:TFRE_DB_SchemeObject) is nested;
  TFRE_DB_Enum_Iterator                 = procedure (const obj:TFRE_DB_Enum) is nested;
  TFRE_DB_ClientFieldValidator_Iterator = procedure (const obj: TFRE_DB_ClientFieldValidator) is nested;
  TFRE_DB_Apps_Iterator                 = procedure (const obj: TFRE_DB_APPLICATION) is nested;
  TFRE_DB_Guid_Iterator                 = procedure (const obj:TFRE_DB_GUID) is nested;
  //TFRE_DB_Obj_IteratorBreak             = function  (const obj:TFRE_DB_Object):Boolean is nested;

  { TFRE_DB_TEXT }

  TFRE_DB_TEXT = class (TFRE_DB_OBJECT,IFRE_DB_TEXT)
  private
    function  GetHint: TFRE_DB_String;
    function  GetLong: TFRE_DB_String;
    function  Getshort: TFRE_DB_String;
    function  GetTKey: TFRE_DB_String;
    procedure SetHint(const AValue: TFRE_DB_String);
    procedure Setlong(const AValue: TFRE_DB_String);
    procedure SetShort(const AValue: TFRE_DB_String);
    procedure SetTKey(const AValue: TFRE_DB_String);
  public
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    procedure ClearLong;
    procedure ClearShort;
    procedure ClearHint;
    procedure ApplyParameters  (const encoded_params : TFRE_DB_StringArray);
  published
    property  LongText         : TFRE_DB_String read GetLong write Setlong;
    property  ShortText        : TFRE_DB_String read Getshort write SetShort;
    property  Hint             : TFRE_DB_String read GetHint write SetHint;
    property  TranslationKey   : TFRE_DB_String read GetTKey write SetTKey;
    class function CreateText  (const translation_key:TFRE_DB_String;const short_text:TFRE_DB_String;const long_text:TFRE_DB_String='';const hint_text:TFRE_DB_String=''):TFRE_DB_TEXT;
    procedure SetupText        (const translation_key:TFRE_DB_String;const short_text:TFRE_DB_String;const long_text:TFRE_DB_String='';const hint_text:TFRE_DB_String='');
  end;

  TFRE_DB_SYSTEM_CONNECTION  = class;
  TFRE_DB_ROLE               = class;
  TFRE_DB_GROUP              = class;
  TFRE_DB_DERIVED_COLLECTION = class;
  TFRE_DB_SIMPLE_TRANSFORM   = class;

  TFRE_DB_USER = class;

  { TFRE_DB_RESOURCE }

  TFRE_DB_RESOURCE = class (TFRE_DB_Object)
  published
    function IMI_Content (const input:TFRE_DB_Object):IFRE_DB_Object;
  end;

  TFRE_DB_RESOURCE_CONTAINER = class (TFRE_DB_Object)
  public
    function SetIconFromFile(const resource_id,filename,mimetype:TFRE_DB_String):TFRE_DB_Errortype;
  end;

  { TFRE_DB_USER }

  TFRE_DB_USER=class(TFRE_DB_Object,IFRE_DB_USER)
  private
    //function  GetDomain              (const conn:IFRE_DB_CONNECTION): TFRE_DB_NameType;
    function  GetDomainIDLink        : TFRE_DB_GUID;
    function  GetLoginAtDomain       (conn: IFRE_DB_SYS_CONNECTION): TFRE_DB_NameType;
    function  GetFirstName           : TFRE_DB_String;
    function  GetIsInternal          : Boolean;
    function  GetLastName            : TFRE_DB_String;
    function  GetLogin               : TFRE_DB_String;
    procedure SetFirstName           (const AValue: TFRE_DB_String);
    procedure SetGIDA                (AValue: TFRE_DB_ObjLinkArray);
    procedure SetIsInternal          (AValue: Boolean);
    procedure SetLastName            (const AValue: TFRE_DB_String);
    procedure Setlogin               (const AValue: TFRE_DB_String);
    procedure SetDomainIDLink        (AValue: TFRE_DB_GUID);
    procedure _UpdateDomainLoginKey;
  protected
    procedure _calcDisplayName       (const calc : IFRE_DB_CALCFIELD_SETTER);
  public
    function  GetUserGroupIDS        : TFRE_DB_ObjLinkArray;
    function  SubFormattedDisplayAvailable: boolean; override;
    function  GetSubFormattedDisplay(indent: integer=4): TFRE_DB_String; override;
    procedure SetImage           (const image_stream: TFRE_DB_Stream; const streamtype: string ; const etag : string);
    procedure InitData           (const nlogin,nfirst,nlast,npasswd:TFRE_DB_String;const userdomainid:TFRE_DB_GUID;const is_internal:Boolean;const long_desc,short_desc:TFRE_DB_String);
    property  Login              :TFRE_DB_String read GetLogin write Setlogin;
    property  Firstname          :TFRE_DB_String read GetFirstName write SetFirstName;
    property  Lastname           :TFRE_DB_String read GetLastName write SetLastName;
    property  UserGroupIDs       :TFRE_DB_ObjLinkArray read GetUserGroupIDS write SetGIDA;
    procedure SetPassword        (const pw:TFRE_DB_String);
    function  Checkpassword      (const pw:TFRE_DB_String):boolean;
    class procedure RegisterSystemScheme     (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class function  GetDomainLoginKey        (const loginpart : TFRE_DB_String; const domain_id : TFRE_DB_GUID) : TFRE_DB_String;
    function        DomainLoginKey           : TFRE_DB_String;
    class procedure InstallDBObjects         (const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    property  isInternal                    : Boolean read GetIsInternal write SetIsInternal;
  published
    class     function  WBC_NewUserOperation (const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
    function  WEB_SaveOperation              (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
  end;

  { TFRE_DB_DOMAIN }

  TFRE_DB_DOMAIN=class(TFRE_DB_NAMED_OBJECT,IFRE_DB_DOMAIN)
  private
    function  IFRE_DB_DOMAIN.GetDesc          = GetDescI;
    function  IFRE_DB_DOMAIN.SetDesc          = SetDescI;
    function  GetIsInternal                   : Boolean;
    procedure SetIsInternal                   (AValue: Boolean);
    function  GetSuspended: boolean;
    procedure SetSuspended(AValue: boolean);
  protected
    procedure _calcDisplayName       (const calc : IFRE_DB_CALCFIELD_SETTER);
  public
    class procedure InstallDBObjects          (const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    class procedure RegisterSystemScheme      (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    function        Domainname                (const unique:boolean=false) : TFRE_DB_NameType;
    function        Domainkey                 : TFRE_DB_GUID_String;
    property        isInternal                : Boolean read GetIsInternal write SetIsInternal;
    property        Suspended                 : boolean read GetSuspended write SetSuspended;
  end;

  { TFRE_DB_GROUP }

  TFRE_DB_GROUP=class(TFRE_DB_NAMED_OBJECT,IFRE_DB_GROUP)
  private
    function  GetDomainIDLink     : TFRE_DB_GUID;
    function  GetIsInternal       : Boolean;
    function  GetIsDisabled       : Boolean;
    function  GetIsProtected      : Boolean;
    procedure SetDomainIDLink     (AValue: TFRE_DB_GUID);
    function  GetRoleIDs          : TFRE_DB_ObjLinkArray;
    function  IFRE_DB_GROUP.GetDesc          = GetDescI;
    function  IFRE_DB_GROUP.SetDesc          = SetDescI;
    procedure SetIsInternal (AValue: Boolean);
    procedure SetIsProtected(AValue: Boolean);
    procedure SetIsDisabled (AValue: Boolean);
    procedure SetRoleIDs(AValue: TFRE_DB_ObjLinkArray);
    procedure _UpdateDomainGroupKey;
  protected
    procedure _calcDisplayName       (const calc : IFRE_DB_CALCFIELD_SETTER);
  public
    class procedure RegisterSystemScheme      (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class function  GetDomainGroupKey         (const grouppart : TFRE_DB_String; const domain_id : TFRE_DB_GUID) : TFRE_DB_String;
    class procedure InstallDBObjects          (const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    class procedure InstallDBObjects4Domain   (const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TFRE_DB_GUID); override;
    class procedure InstallDBObjects4SysDomain(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TFRE_DB_GUID); override;

    function  GetDomain                    (const conn :IFRE_DB_CONNECTION): TFRE_DB_NameType;
    function  SubFormattedDisplayAvailable : boolean; override;
    function  GetSubFormattedDisplay       (indent: integer=4): TFRE_DB_String; override;
    property  RoleIDs                      :TFRE_DB_ObjLinkArray read GetRoleIDs write SetRoleIDs;
    property  isProtected                  :Boolean read GetIsProtected write SetIsProtected;
    property  isInternal                   :Boolean read GetIsInternal write SetIsInternal;
    property  isDisabled                   :Boolean read GetIsDisabled write SetIsDisabled;
  end;

  { TFRE_DB_ROLE }

  TFRE_DB_ROLE=class(TFRE_DB_NAMED_OBJECT,IFRE_DB_ROLE)
  private
    function  IFRE_DB_ROLE.GetDesc          = GetDescI;
    function  IFRE_DB_ROLE.SetDesc          = SetDescI;
    function  GetDomain                     (const conn :IFRE_DB_CONNECTION): TFRE_DB_NameType;
    function  GetDomainIDLink               : TFRE_DB_GUID;
    function  GetIsInternal                 : Boolean;
    function  GetIsDisabled                 : Boolean;
    procedure SetDomainIDLink               (AValue: TFRE_DB_GUID);
    procedure SetIsInternal                 (AValue: Boolean);
    procedure SetIsDisabled                 (AValue: Boolean);
  protected
    procedure _calcDisplayName       (const calc : IFRE_DB_CALCFIELD_SETTER);
  public
    class procedure InstallDBObjects        (const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    class procedure InstallDBObjects4Domain   (const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TFRE_DB_GUID); override;
    class procedure InstallDBObjects4SysDomain(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TFRE_DB_GUID); override;
    class procedure RegisterSystemScheme    (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class function  GetDomainRoleKey        (const rolepart : TFRE_DB_String; const domain_id : TFRE_DB_GUID) : TFRE_DB_String;
    procedure AddRight                      (const right : IFRE_DB_RIGHT);
    function  SubFormattedDisplayAvailable  : boolean; override;
    function  GetSubFormattedDisplay        (indent: integer=4): TFRE_DB_String; override;
    function  GetRightNames                 :TFRE_DB_StringArray;
    procedure AddRightsFromRole             (const role : IFRE_DB_ROLE);
    procedure RemoveRights                  (const rights : TFRE_DB_StringArray);
    property  isInternal                    : Boolean read GetIsInternal write SetIsInternal;
    property  isDisabled                    : Boolean read GetIsDisabled write SetIsDisabled;
  end;

  procedure          ForAllObjectsDo (const object_array:TFRE_DB_ObjectArray ; const iterator:TFRE_DB_Obj_Iterator);
  procedure          ForAllGuidsDo   (const guid_array:TFRE_DB_GUIDArray   ; const iterator:TFRE_DB_Guid_Iterator);

type
  { TFRE_DB_COLLECTION }

  TFRE_DB_COLLECTION=class(TFRE_DB_Object,IFRE_DB_COLLECTION)
  private
    FCollConnection        : TFRE_DB_BASE_CONNECTION;
    FCollUserTokenRef      : IFRE_DB_USER_RIGHT_TOKEN; { reference to the user token of the connection }
    FIsVolatile            : Boolean;
    FName                  : TFRE_DB_NameType;
    FUniqueName            : TFRE_DB_NameType;
  protected
    function        URT_UserUid        : TFRE_DB_GUID; { returns the uid of the connection usertoken or NullGuid (all rights) if no connection is set }
    function        StoreI             (const new_obj:IFRE_DB_Object):TFRE_DB_Errortype;
    function        UpdateI            (const dbo:IFRE_DB_Object):TFRE_DB_Errortype;
    function        FetchInCollectionI (const ouid:TFRE_DB_GUID;out dbo:IFRE_DB_Object): boolean;
    function        FirstI             : IFRE_DB_Object;
    function        LastI              : IFRE_DB_Object;

    function IFRE_DB_COLLECTION.ForAll        = ForAllI;
    function IFRE_DB_COLLECTION.ForAllBreak   = ForAllBreakI;
    function IFRE_DB_COLLECTION.ForAllModify  = ForAllModifyI;
    function IFRE_DB_COLLECTION.Store         = StoreI;
    function IFRE_DB_COLLECTION.Update        = UpdateI;
    function IFRE_DB_COLLECTION.First         = FirstI;
    function IFRE_DB_COLLECTION.Last          = LastI;
    function IFRE_DB_COLLECTION.Fetch         = FetchInCollectionI;
    function IFRE_DB_COLLECTION.FetchInCollection = FetchInCollectionI;
    function IFRE_DB_COLLECTION.GetIndexedObj = GetIndexedObjI;

    function        _InternalStore             (var   new_obj:TFRE_DB_Object):TFRE_DB_Errortype;virtual;
    procedure       _IterateOverObjectsBrk     (const objs : IFRE_DB_ObjectArray ; const func: IFRE_DB_ObjectIteratorBrk ; var halt : boolean);
    procedure       _IterateOverObjects        (const objs : IFRE_DB_ObjectArray ; const func: TFRE_DB_Obj_Iterator);
  public
    procedure       ForAllI                    (const func:IFRE_DB_Obj_Iterator);
    procedure       ForAllBreakI               (const func:IFRE_DB_ObjectIteratorBrk ; var halt : boolean);

    constructor     Create                     (const connection:TFRE_DB_BASE_CONNECTION;const name:TFRE_DB_NameType ; const in_memory_only: boolean);virtual;
    destructor      Destroy                    ; override;

    function        ExistsInCollection         (const ouid:TFRE_DB_GUID ; const has_fetch_rights : boolean=true):boolean;
    procedure       ForAll                     (const func:TFRE_DB_Obj_Iterator);
    procedure       ForAllNoRightChk           (const func:TFRE_DB_Obj_Iterator);
    procedure       ForAllBreak                (const func:TFRE_DB_ObjectIteratorBrk ; var halt : boolean);
    function        Remove                     (const ouid:TFRE_DB_GUID):TFRE_DB_Errortype; virtual;
    function        Store                      (var   new_obj:TFRE_DB_Object):TFRE_DB_Errortype;virtual;
    function        Update                     (const dbo:TFRE_DB_Object):TFRE_DB_Errortype;virtual;
    function        FetchInCollection          (const ouid:TFRE_DB_GUID;out dbo:TFRE_DB_Object): boolean;virtual;

    procedure       ClearCollection            ;

    function        CollectionName             (const unique:boolean=false):TFRE_DB_NameType;

    procedure       GetAllUids                 (var uids:TFRE_DB_GUIDArray);
    procedure       GetAllUidsNoRC             (var uids:TFRE_DB_GUIDArray);
    procedure       GetAllObjs                 (out objs:IFRE_DB_ObjectArray);
    procedure       GetAllObjsNoRC             (out objs:IFRE_DB_ObjectArray);

    function        DefineIndexOnField         (const FieldName  : TFRE_DB_NameType;const FieldType:TFRE_DB_FIELDTYPE;const unique:boolean; const ignore_content_case:boolean=false;const index_name:TFRE_DB_NameType='def' ; const allow_null_value : boolean=true ; const unique_null_values : boolean=false ; const is_a_domain_index : boolean = false):TFRE_DB_Errortype;
    function        DefineIndexOnField         (const IndexDef   : TFRE_DB_INDEX_DEF):TFRE_DB_Errortype;
    function        GetIndexDefinition         (const index_name : TFRE_DB_NameType):TFRE_DB_INDEX_DEF;
    function        DropIndex                  (const index_name : TFRE_DB_NameType):TFRE_DB_Errortype;
    function        GetAllIndexNames           : TFRE_DB_NameTypeArray;
    function        GetAllIndexDefinitions     : TFRE_DB_INDEX_DEF_ARRAY;

    function        IndexExists                (const index_name:TFRE_DB_NameType):boolean;

    function        ExistsIndexed              (const query_value : TFRE_DB_String ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def'): Boolean; deprecated ;

    function        ExistsIndexedFieldval      (const fld : IFRE_DB_Field                                              ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt; // for all index types
    function        ExistsIndexedText          (const query_value : TFRE_DB_String ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt; // for the string   fieldtype
    function        ExistsIndexedSigned        (const query_value : int64          ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt; // for the signed   fieldtypes
    function        ExistsIndexedUnsigned      (const query_value : UInt64         ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt; // for the unsigned fieldtypes
    function        ExistsIndexedReal          (const query_value : Double         ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt; // for the floating point fieldtypes

    function        GetIndexedObjTextCoreI     (const query_value : TFRE_DB_String ; out obj     : IFRE_DB_Object      ; const no_rc : boolean=false; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def'):NativeInt;
    function        GetIndexedObjTextCore      (const query_value : TFRE_DB_String ; out obj     : TFRE_DB_Object      ; const no_rc : boolean=false; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def'):NativeInt;

    function        GetIndexedObjI             (const query_value : TFRE_DB_String ; out obj     : IFRE_DB_Object      ; const index_name:TFRE_DB_NameType='def'):boolean; { deprecated } // for the string fieldtype
    function        GetIndexedObjFieldval      (const fld         : IFRE_DB_Field  ; out obj     : IFRE_DB_Object      ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;
    function        GetIndexedObjFieldvalNoRc  (const fld         : IFRE_DB_Field  ; out obj     : IFRE_DB_Object      ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;
    function        GetIndexedObjText          (const query_value : TFRE_DB_String ; out obj     : IFRE_DB_Object      ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;
    function        GetIndexedObjSigned        (const query_value : int64          ; out obj     : IFRE_DB_Object      ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;
    function        GetIndexedObjUnsigned      (const query_value : Uint64         ; out obj     : IFRE_DB_Object      ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;
    function        GetIndexedObjReal          (const query_value : Double         ; out obj     : IFRE_DB_Object      ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;

    function        GetIndexedObjsFieldval     (const fld : IFRE_DB_Field          ; out obj     : IFRE_DB_ObjectArray ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt; { preferred  }
    function        GetIndexedObjsText         (const query_value : TFRE_DB_String ; out obj     : IFRE_DB_ObjectArray ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;
    function        GetIndexedObjsSigned       (const query_value : int64          ; out obj     : IFRE_DB_ObjectArray ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;
    function        GetIndexedObjsUnsigned     (const query_value : Uint64         ; out obj     : IFRE_DB_ObjectArray ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;
    function        GetIndexedObjsReal         (const query_value : Double         ; out obj     : IFRE_DB_ObjectArray ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;

    function        GetIndexedUID              (const query_value : TFRE_DB_String ; out luid    : TFRE_DB_GUID        ; const index_name:TFRE_DB_NameType='def'):boolean; deprecated;

    function        GetIndexedUIDFieldval      (const fld : IFRE_DB_Field          ; out luid    : TFRE_DB_GUID        ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt; { preferred }
    function        GetIndexedUIDText          (const query_value : TFRE_DB_String ; out luid    : TFRE_DB_GUID        ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;
    function        GetIndexedUIDSigned        (const query_value : int64          ; out luid    : TFRE_DB_GUID        ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;
    function        GetIndexedUIDUnsigned      (const query_value : Uint64         ; out luid    : TFRE_DB_GUID        ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;
    function        GetIndexedUIDReal          (const query_value : Double         ; out luid    : TFRE_DB_GUID        ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;

    function        GetIndexedUIDsFieldval     (const fld : IFRE_DB_Field          ; out fuids   : TFRE_DB_GUIDArray   ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt; { preferred }
    function        GetIndexedUIDsText         (const query_value : TFRE_DB_String ; out fuids   : TFRE_DB_GUIDArray   ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;
    function        GetIndexedUIDsSigned       (const query_value : int64          ; out fuids   : TFRE_DB_GUIDArray   ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;
    function        GetIndexedUIDsUnsigned     (const query_value : Uint64         ; out fuids   : TFRE_DB_GUIDArray   ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;
    function        GetIndexedUIDsReal         (const query_value : Double         ; out fuids   : TFRE_DB_GUIDArray   ; const val_is_null : boolean=false ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;

    function        RemoveIndexedFieldval      (const fld : IFRE_DB_Field          ; const index_name:TFRE_DB_NameType='def' ; const domain_uid_string : TFRE_DB_GUID_String = '') : NativeInt;
    function        RemoveIndexedText          (const query_value : TFRE_DB_String ; const index_name:TFRE_DB_NameType='def' ; const val_is_null : boolean = false ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt; // for the string   fieldtype
    function        RemoveIndexedSigned        (const query_value : int64          ; const index_name:TFRE_DB_NameType='def' ; const val_is_null : boolean = false ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt; // for all signed   fieldtypes
    function        RemoveIndexedUnsigned      (const query_value : QWord          ; const index_name:TFRE_DB_NameType='def' ; const val_is_null : boolean = false ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt; // for all unsigned fieldtype
    function        RemoveIndexedReal          (const query_value : Double         ; const index_name:TFRE_DB_NameType='def' ; const val_is_null : boolean = false ; const domain_uid_string : TFRE_DB_GUID_String = ''):NativeInt;

    procedure       ForAllIndexed              (const func        : IFRE_DB_ObjectIteratorBrk ; var halt : boolean ; const index_name:TFRE_DB_NameType='def';const ascending:boolean=true; const max_count : NativeInt=0 ; skipfirst : NativeInt=0 ; const domain_uid_string : TFRE_DB_GUID_String = '');

    procedure       ForAllIndexedFieldvalRange (const min_field,max_field : IFRE_DB_Field  ; const iterator : IFRE_DB_ObjectIteratorBrk ; var halt : boolean ; const index_name:TFRE_DB_NameType='def';const ascending:boolean=true; const max_count : NativeInt=0 ; skipfirst : NativeInt=0 ; const domain_uid_string : TFRE_DB_GUID_String = '');
    procedure       ForAllIndexedTextRange     (const min_value,max_value : TFRE_DB_String ; const iterator : IFRE_DB_ObjectIteratorBrk ; var halt:boolean ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const min_is_null : boolean = false ; const max_is_max : boolean = false ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0 ; const domain_uid_string : TFRE_DB_GUID_String = '');
    procedure       ForAllIndexedSignedRange   (const min_value,max_value : int64          ; const iterator : IFRE_DB_ObjectIteratorBrk ; var halt:boolean ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const min_is_null : boolean = false ; const max_is_max : boolean = false ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0 ; const domain_uid_string : TFRE_DB_GUID_String = '');
    procedure       ForAllIndexedUnsignedRange (const min_value,max_value : QWord          ; const iterator : IFRE_DB_ObjectIteratorBrk ; var halt:boolean ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const min_is_null : boolean = false ; const max_is_max : boolean = false ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0 ; const domain_uid_string : TFRE_DB_GUID_String = '');
    procedure       ForAllIndexedRealRange     (const min_value,max_value : Double         ; const iterator : IFRE_DB_ObjectIteratorBrk ; var halt:boolean ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const min_is_null : boolean = false ; const max_is_max : boolean = false ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0 ; const domain_uid_string : TFRE_DB_GUID_String = '');

    procedure       ForAllIndexPrefixString    (const prefix              : TFRE_DB_String ; const iterator : IFRE_DB_ObjectIteratorBrk ; var halt:boolean ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0 ; const domain_uid_string : TFRE_DB_GUID_String = '');

    function        IsVolatile                 : Boolean;

    function        First                      : TFRE_DB_Object; virtual;
    function        Last                       : TFRE_DB_Object; virtual;
    function        GetItem                    (const num:uint64):IFRE_DB_Object; virtual;
    function        ItemCount                  : Int64; virtual;
    function        Count                      : Int64; deprecated ; { replaced by ItemCount }


    { not right aware end}
  end;

  //Base Class Tranforms a DB Object into another DB Object
  //Has support for "FilterFields" that are fields that may get filtered but are not in the output

  { TFRE_DB_FIELD_TRANSFORM }

  TFRE_DB_FIELD_TRANSFORM=class
  private
  protected
    FInFieldName     : TFRE_DB_NameType;
    FOutFieldName    : TFRE_DB_NameType;
    FOutFieldTitle   : TFRE_DB_String;
    FSortable        : Boolean;              // Field can be sorted
    FFilterable      : Boolean;              // Field can be filtered
    FDisplay         : Boolean;              // Field Should be in Output Displayed
    FGuiDisplaytype  : TFRE_DB_DISPLAY_TYPE; // How the gui should display the field
    FFieldSize       : NativeInt;            // Relative gui size of the element (Column)
    FIconIdField     : TFRE_DB_String;       // The name of the field holding the resource descriptor url of the through this transform defined column for the normal state
    FOpenIconIDField : TFRE_DB_String;       // The name of the field holding the resource descriptor url of the through this transform defined column for the open state
    FDefaultValue    : TFRE_DB_String;
    FFilterValues    : TFRE_DB_StringArray;
  public
    function  RefLinkSpec         : TFRE_DB_NameTypeRLArray;virtual;
    procedure AddToViewCollection (const vcd   : TFRE_DB_VIEW_LIST_LAYOUT_DESC); virtual;
    procedure TransformField      (const conn  : IFRE_DB_CONNECTION ; const input,output : IFRE_DB_Object); virtual; // A transformed object has the same UID, but not the same Schemeclass as the source, and a subset/transformation of the input object
  end;

  { TFRE_DB_ONEONE_FT }

  TFRE_DB_ONEONE_FT=class(TFRE_DB_FIELD_TRANSFORM)
  protected
  public
    constructor Create         (const fieldname:TFRE_DB_String;const out_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const display:Boolean=true;const sortable:Boolean=false;const filterable:Boolean=false;const fieldSize: Integer=1;const iconID:String='';const openIconID:String=''; const defaultValue:String=''; const filterValues: TFRE_DB_StringArray=nil);
    procedure   TransformField (const conn  : IFRE_DB_CONNECTION ; const input, output: IFRE_DB_Object); override;
  end;

  { TFRE_DB_MULTIONE_FT }

  TFRE_DB_MULTIONE_FT=class(TFRE_DB_FIELD_TRANSFORM)
  protected
    FInfieldList  : TFRE_DB_NameTypeArray;
  public
    constructor Create         (const in_fieldlist:TFRE_DB_NameTypeArray;const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const display:Boolean=true;const sortable:Boolean=false;const filterable:Boolean=false;const fieldSize: Integer=1;const iconID:String='';const openIconID:String=''; const defaultValue:String='');
    procedure   TransformField (const conn  : IFRE_DB_CONNECTION ; const input, output: IFRE_DB_Object); override;
  end;

  { TFRE_DB_PROGRESS_FT }

  TFRE_DB_PROGRESS_FT=class(TFRE_DB_FIELD_TRANSFORM)
  protected
    FInTextField    : TFRE_DB_NameType; // Infield holding text to be displayed in the Progressbar (eg. less/more/much)
    FOutTextField   : TFRE_DB_NameType; // Outfield holding text to be displayed in the Progressbar (eg. less/more/much)
    FMaxValue       : Single;
  public
    constructor Create              (const valuefield:TFRE_DB_String;const out_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const input_textfield:TFRE_DB_String='';const output_textfield:TFRE_DB_String='';const maxValue:Single=100;const display:Boolean=true;const sortable:Boolean=false;const filterable:Boolean=false;const fieldSize: Integer=1);
    procedure   AddToViewCollection (const vcd: TFRE_DB_VIEW_LIST_LAYOUT_DESC); override;
    procedure   TransformField      (const conn  : IFRE_DB_CONNECTION ; const input, output: IFRE_DB_Object); override;
  end;

  { TFRE_DB_CONST_STRING_FT }

  TFRE_DB_CONST_STRING_FT=class(TFRE_DB_FIELD_TRANSFORM)
  protected
    FConstValue : TFRE_DB_String;
  public
    constructor Create         (const out_field,value:TFRE_DB_String;const display: Boolean=false; const sortable:Boolean=false; const filterable:Boolean=false; const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    procedure   TransformField (const conn  : IFRE_DB_CONNECTION ; const input, output: IFRE_DB_Object); override;
  end;

  { TFRE_DB_TEXT_FT }

  TFRE_DB_TEXT_FT=class(TFRE_DB_FIELD_TRANSFORM)
  protected
    FWhichText : TFRE_DB_TEXT_SUBTYPE;
  public
    constructor Create         (const fieldname:TFRE_DB_String;const which_text : TFRE_DB_TEXT_SUBTYPE ; const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const display:Boolean=true;const sortable:Boolean=false;const filterable:Boolean=false; const fieldSize: Integer=1);
    procedure   TransformField (const conn  : IFRE_DB_CONNECTION ; const input, output: IFRE_DB_Object); override;
  end;

  { TFRE_DB_COLLECTOR_FT }

  TFRE_DB_COLLECTOR_FT=class(TFRE_DB_FIELD_TRANSFORM)
  protected
    FFormatString : TFRE_DB_String;
    FInfieldList  : TFRE_DB_NameTypeArray;
  public
    constructor Create         (const format:TFRE_DB_String;const in_fieldlist:TFRE_DB_NameTypeArray;const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const display:Boolean=true;const sortable:Boolean=false;const filterable:Boolean=false;const fieldSize: Integer=1);
    procedure   TransformField (const conn  : IFRE_DB_CONNECTION ; const input, output: IFRE_DB_Object); override;
  end;

  { TFRE_DB_OUTCOLLECTOR_FT }

  TFRE_DB_OUTCOLLECTOR_FT=class(TFRE_DB_FIELD_TRANSFORM)
  protected
    FFormatString : TFRE_DB_String;
    FInfieldList  : TFRE_DB_NameTypeArray;
  public
    constructor Create         (const format:TFRE_DB_String;const in_fieldlist:TFRE_DB_NameTypeArray;const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const display:Boolean=true;const sortable:Boolean=false;const filterable:Boolean=false;const fieldSize: Integer=1);
    procedure   TransformField (const conn  : IFRE_DB_CONNECTION ; const input, output: IFRE_DB_Object); override;
  end;

  { TFRE_DB_REFERERENCE_CHAIN_FT }

  TFRE_DB_REFERERENCE_CHAIN_FT=class(TFRE_DB_FIELD_TRANSFORM)
  protected
    FRefFieldChain : TFRE_DB_NameTypeRLArray;
  public
    function    RefLinkSpec         : TFRE_DB_NameTypeRLArray;override;
    constructor Create              (const ref_field_chain: TFRE_DB_NameTypeRLArray;const target_field:TFRE_DB_String;const output_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const display:Boolean=true;const sortable:Boolean=false; const filterable:Boolean=false; const fieldSize: Integer=1;const iconID:String='';const filterValues: TFRE_DB_StringArray=nil);
    procedure   TransformField      (const conn  : IFRE_DB_CONNECTION ; const input, output: IFRE_DB_Object); override;
  end;

  { TFRE_DB_REFERERENCE_QRY_FT }

  TFRE_DB_REFERERENCE_QRY_FT=class(TFRE_DB_FIELD_TRANSFORM)
  protected
    FRefFieldChain        : TFRE_DB_NameTypeRLArray;
    FRQ_func              : IFRE_DB_QUERY_SELECTOR_FUNCTION;
    FRQO_Fields           : TFRE_DB_StringArray;
    FRQO_Titles           : TFRE_DB_StringArray;
    FRQO_Langres          : TFRE_DB_StringArray;
    FRQO_Types            : array of TFRE_DB_DISPLAY_TYPE;
    FRQO_Display          : TFRE_DB_BoolArray;
    FRQO_Sortable         : TFRE_DB_BoolArray;
    FRQO_Filterable       : TFRE_DB_BoolArray;
    FRQO_Hide             : TFRE_DB_BoolArray;
    FRQO_FieldSize        : TFRE_DB_Int32Array;
  public
    function    RefLinkSpec         : TFRE_DB_NameTypeRLArray;override;
    constructor Create              (const func : IFRE_DB_QUERY_SELECTOR_FUNCTION;const ref_field_chain: TFRE_DB_NameTypeRLArray ; const output_fields,output_titles : TFRE_DB_StringArray;const langres:TFRE_DB_StringArray;const gui_display_type:TFRE_DB_DISPLAY_TYPE_Array;const display,sortable,filterable,hideinoutput : TFRE_DB_BoolArray ; const fieldSize : TFRE_DB_Int32Array);
    procedure   AddToViewCollection (const vcd: TFRE_DB_VIEW_LIST_LAYOUT_DESC); override;
    procedure   TransformField      (const conn  : IFRE_DB_CONNECTION ; const input, output: IFRE_DB_Object); override;
  end;



  PFRE_DB_FIELD_TRANSFORM=^TFRE_DB_FIELD_TRANSFORM;

  OFRE_SL_TFRE_DB_FIELD_TRANSFORM = specialize OFOS_SpareList<TFRE_DB_FIELD_TRANSFORM>;

  { TFRE_DB_TRANSFORMOBJECT }

  TFRE_DB_TRANSFORMOBJECT=class(TFOS_BASE,IFRE_DB_TRANSFORMOBJECT)
  private
    FTransformList        : OFRE_SL_TFRE_DB_FIELD_TRANSFORM;
    FHasReflinkTransforms : Boolean; { we need to consider reflink updates for this transform }
  protected
    procedure  Finalize;
    function   Implementor    : TObject;
    function   Implementor_HC : TObject;
    function   GetFirstFieldname : TFRE_DB_NameType;
    function   IsReflinkSpecRelevant(const rlspec : TFRE_DB_NameTypeRL):boolean;
  public
    constructor Create; override;
    destructor  Destroy; override;
    function    TransformInOut (const conn : IFRE_DB_CONNECTION ; const input: IFRE_DB_Object): TFRE_DB_Object; virtual;
  end;

  { TFRE_DB_SIMPLE_TRANSFORM }

  TFRE_DB_SIMPLE_TRANSFORM=class(TFRE_DB_TRANSFORMOBJECT,IFRE_DB_SIMPLE_TRANSFORM)
  private
    FFinalRightTransform  : IFRE_DB_FINAL_RIGHT_TRANSFORM_FUNCTION;
    FFRTLangres           : TFRE_DB_StringArray;
  public
    constructor Create                         ; override;
    function    TransformInOut                 (const conn : IFRE_DB_CONNECTION ; const input: IFRE_DB_Object): TFRE_DB_Object; override;
    procedure   TransformRefQuery              ;
    //@ Add a Field that collects STRING values to a new String Field
    //@ format : format string of the new field ; in_fieldlist : list of input fieldnames ; output_title : name of the output field, default=same as input
    procedure   AddCollectorscheme             (const format:TFRE_DB_String;const in_fieldlist:TFRE_DB_NameTypeArray;const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const display:Boolean=true;const sortable:Boolean=false;const filterable:Boolean=false;const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1;const hide_in_output : boolean=false);
    procedure   AddFulltextFilterOnTransformed (const fieldlist:array of TFRE_DB_NameType);  { takes the text rep of the fields (asstring), concatenates them into 'FTX_SEARCH' }
    procedure   AddOneToOnescheme              (const fieldname:TFRE_DB_String;const out_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const display:Boolean=true;const sortable:Boolean=false; const filterable:Boolean=false;const fieldSize: Integer=1;const iconID:String='';const openIconID:String='';const default_value:TFRE_DB_String='';const filterValues: TFRE_DB_StringArray=nil; const hide_in_output : boolean=false);
    procedure   AddMultiToOnescheme            (const in_fieldlist:TFRE_DB_NameTypeArray;const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const display:Boolean=true;const sortable:Boolean=false; const filterable:Boolean=false;const fieldSize: Integer=1;const iconID:String='';const openIconID:String='';const default_value:TFRE_DB_String='';const hide_in_output : boolean=false);
    procedure   AddProgressTransform           (const valuefield:TFRE_DB_String;const out_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const textfield:TFRE_DB_String='';const out_text:TFRE_DB_String='';const maxValue:Single=100;const sortable:Boolean=false; const filterable:Boolean=false;const fieldSize: Integer=1;const hide_in_output : boolean=false);
    procedure   AddConstString                 (const out_field,value:TFRE_DB_String;const display: Boolean=false; const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const sortable:Boolean=false; const filterable:Boolean=false;const fieldSize: Integer=1;const hide_in_output : boolean=false);
    procedure   AddDBTextToOne                 (const fieldname:TFRE_DB_String;const which_text : TFRE_DB_TEXT_SUBTYPE ; const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const sortable:Boolean=false; const filterable:Boolean=false;const fieldSize: Integer=1;const hide_in_output : boolean=false);
    procedure   AddMatchingReferencedField     (const ref_field_chain: array of TFRE_DB_NameTypeRL;const target_field:TFRE_DB_String;const output_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const display:Boolean=true;const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const sortable:Boolean=false; const filterable:Boolean=false;const fieldSize: Integer=1;const iconID:String='';const filterValues:TFRE_DB_StringArray=nil;const hide_in_output : boolean=false);
    procedure   AddMatchingReferencedField     (const ref_field      : TFRE_DB_NameTypeRL     ;const target_field:TFRE_DB_String;const output_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const display:Boolean=true;const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const sortable:Boolean=false; const filterable:Boolean=false;const fieldSize: Integer=1;const iconID:String='';const filterValues:TFRE_DB_StringArray=nil;const hide_in_output : boolean=false);
    //Get a Viewcollectiondescription depending on the defined fields of the transformation
    function    GetViewCollectionDescription   : TFRE_DB_CONTENT_DESC;
    procedure   SetFinalRightTransformFunction (const func : IFRE_DB_FINAL_RIGHT_TRANSFORM_FUNCTION;const langres: array of TFRE_DB_String); { set a function that changes the object after, transfrom, order, and filter as last step before data deliverance }
    procedure   AddReferencedFieldQuery        (const func : IFRE_DB_QUERY_SELECTOR_FUNCTION;const ref_field_chain: array of TFRE_DB_NameTypeRL ; const output_fields:array of TFRE_DB_String;const output_titles:array of TFRE_DB_String;const langres: array of TFRE_DB_String; const gui_display_type:array of TFRE_DB_DISPLAY_TYPE;const display:Boolean=true;const sortable:Boolean=false; const filterable:Boolean=false;const fieldSize: Integer=1;const hide_in_output : boolean=false);
  end;

  { TFRE_DB_CHART_TRANSFORM }

  TFRE_DB_CHART_TRANSFORM=class(TFRE_DB_TRANSFORMOBJECT,IFRE_DB_TRANSFORMOBJECT) // Stores additionally the chart relevant setup
  private
    FseriesFieldNames   : TFRE_DB_StringArray; // = SeriesKeys
    //Fseries1_colors_fld : TFRE_DB_String;
    //Fseries1_labels_fld : TFRE_DB_String;
    FUseSeriesColors    : boolean; //SeriesFieldName+'_Col' must exist -> String #WebNotation
    FUseSeriesLabels    : Boolean; //SeriesFieldName+'_Lbl' must exist -> String
    FShowLegend         : Boolean;
    FSeriesLabels       : TFRE_DB_StringArray;
    FChartType          : TFRE_DB_CHART_TYPE;
    FMaxValue           : Integer;
  public
    function   TransformInOut(const conn : IFRE_DB_CONNECTION ; const input: IFRE_DB_Object): TFRE_DB_Object; override; // todo - remove unnecessary fields / transform only series fields
    destructor Destroy;override;
  end;

  TFRE_DB_SortTree = specialize TGFOS_RBTree<TFRE_DB_Object,boolean>;

  { TFRE_DB_DERIVED_COLLECTION }

  TFRE_DB_SelectionDependencyEvents=record
    dc_name  : TFRE_DB_NameType;
    ref_name : TFRE_DB_NameType;
  end;

  //@ Used for filtered, sorted Collections, is based on  a "real" collection
  TFRE_DB_DERIVED_COLLECTION=class(TFRE_DB_Object,IFRE_DB_DERIVED_COLLECTION) //
  private
   type
     TDC_Mode      = (dc_None,dc_Map2RealCollection);
     TDC_TransMode = (trans_Insert,trans_Update,trans_SingleInsert);
   var
    FName              : TFRE_DB_String;
    FUniquename        : TFRE_DB_String;
    FDCollFilters      : TFRE_DB_DC_FILTER_DEFINITION_BASE;
    FDCollFiltersDyn   : TFRE_DB_DC_FILTER_DEFINITION_BASE;
    FDCollOrder        : TFRE_DB_DC_ORDER_DEFINITION_BASE;
    FDCMode            : TDC_Mode;
    FDepObjectsRefNeg  : Boolean;
    FDepRefConstraint  : TFRE_DB_NameTypeRLArrayArray;
    FDependencyRef     : TFRE_DB_StringArray;     { Array of inbound dependencies (usually one) }
    FParentIds         : TFRE_DB_GUIDArray;

    FUseDepAsLinkFilt  : Boolean;
    FSelectionDepEvents: Array of TFRE_DB_SelectionDependencyEvents;

    FParentChldLinkFldSpec : TFRE_DB_NameTypeRL;

    FParentChildScheme : TFRE_DB_NameType;
    FParentChildField  : TFRE_DB_NameType;
    FParentLinksChild  : Boolean ;

    FParentCollection  : IFRE_DB_COLLECTION;
    FIdField           : String;

    FTransform         : TFRE_DB_TRANSFORMOBJECT;  //Links to Parent DC in Parent DC Mode
    FitemMenuFunc      : TFRE_DB_SERVER_FUNC_DESC;
    FitemDetailsFunc   : TFRE_DB_SERVER_FUNC_DESC;
    FselectionDepFunc  : TFRE_DB_SERVER_FUNC_DESC;
    FtreeMenuFunc      : TFRE_DB_SERVER_FUNC_DESC;
    FdropFunc          : TFRE_DB_SERVER_FUNC_DESC;
    FdragFunc          : TFRE_DB_SERVER_FUNC_DESC;
    FDisplaytype       : TFRE_COLLECTION_DISPLAY_TYPE;
    FGridDisplayFlags  : TFRE_COLLECTION_GRID_DISPLAY_FLAGS;
    FChartDisplayFlags : TFRE_COLLECTION_CHART_DISPLAY_FLAGS;
    FTitle             : TFRE_DB_String;
    FlabelFields       : TFRE_DB_StringArray;

    FInitialDerived    : Boolean;
    FDC_Session        : TFRE_DB_UserSession;

    function        CollectionName                (const unique:boolean=false): TFRE_DB_NameType;

    function        HasParentChildRefRelationDefined : boolean;

    function        IsDependencyFilteredCollection   : boolean;
    function        HasReflinksInTransformation      : boolean; { a potential reflink dependency is in the transforms }

    function        ParentchildRelationIsOutbound    : boolean;
    procedure       _CheckSetDisplayType             (const CollectionDisplayType: TFRE_COLLECTION_DISPLAY_TYPE);
    procedure       _CheckTransformSet               ;
    procedure       _ClearMode;

    procedure       ForAllDerived                 (const func:IFRE_DB_Obj_Iterator);
    procedure       _CheckDepRefConstraint        ;
    procedure       MustBeInitialized             ;
  protected
    procedure  BindSession                     (const session : TFRE_DB_UserSession);
  public
    constructor  Create                        (const dbname: TFRE_DB_NameType; const name: TFRE_DB_NameType);
    destructor   Destroy                       ; override;
    function     GetCollectionTransformKey     : TFRE_DB_NameTypeRL; { deliver a key which identifies transformed data depending on ParentCollection and Transformation}

    procedure    MyTransForm                   (const connection : TFRE_DB_CONNECTION ; const in_objects : array of IFRE_DB_Object ; const transdata: TFRE_DB_TRANSFORMED_ARRAY_BASE ; var rec_cnt : NativeInt ; const lazy_child_expand : boolean ; const mode : TDC_TransMode ; const update_idx : NativeInt ; const rl_ins: boolean; const parentpath: TFRE_DB_String ; const in_parent_tr_obj : IFRE_DB_Object ; const transkey : TFRE_DB_TransStepId);
    procedure    TransformAllTo                (const connection : IFRE_DB_CONNECTION ; const transdata  : TFRE_DB_TRANSFORMED_ARRAY_BASE ; const lazy_child_expand : boolean ; var record_cnt  : NativeInt);
    procedure    TransformSingleUpdate         (const connection : IFRE_DB_CONNECTION ; const in_object: IFRE_DB_Object; const transdata: TFRE_DB_TRANSFORMED_ARRAY_BASE; const lazy_child_expand: boolean; const upd_idx: NativeInt ; const parentpath_full: TFRE_DB_String ; const transkey : TFRE_DB_TransStepId);
    procedure    TransformSingleInsert         (const connection : IFRE_DB_CONNECTION ; const in_object: IFRE_DB_Object; const transdata: TFRE_DB_TRANSFORMED_ARRAY_BASE; const lazy_child_expand: boolean; const rl_ins: boolean; const parentpath: TFRE_DB_String ; const parent_tr_obj : IFRE_DB_Object ; const transkey : TFRE_DB_TransStepId);

    class procedure RegisterSystemScheme       (const scheme: IFRE_DB_SCHEMEOBJECT); override;

    procedure  AddSelectionDependencyEvent     (const derived_collection_name : TFRE_DB_NameType ; const ReferenceID :TFRE_DB_NameType); {On selection change a dependency "filter" object is generated and sent to the derived collection}

    procedure  SetDefaultOrderField            (const field_name:TFRE_DB_String ; const ascending : boolean);
    procedure  RemoveAllFilterFields           ;
    procedure  RemoveAllFiltersPrefix          (const prefix:string);

    function   Filters                         : TFRE_DB_DC_FILTER_DEFINITION_BASE;
    function   Orders                          : TFRE_DB_DC_ORDER_DEFINITION_BASE;

    procedure  SetDeriveParent                 (const coll:IFRE_DB_COLLECTION     ; const idField: String='uid');

    procedure  SetUseDependencyAsRefLinkFilter (const scheme_and_field_constraint : Array of TFRE_DB_NameTypeRL ; const negate : boolean ; const dependency_reference : string = 'uids');
    procedure  SetParentToChildLinkField       (const fieldname : TFRE_DB_NameTypeRL);

    procedure  SetDeriveTransformation         (const tob:IFRE_DB_TRANSFORMOBJECT);

    function   GetStoreDescription             : TFRE_DB_CONTENT_DESC;
    function   getDescriptionStoreId           : String;
    procedure  SetDisplayType                  (const CollectionDisplayType : TFRE_COLLECTION_DISPLAY_TYPE ; const Flags:TFRE_COLLECTION_GRID_DISPLAY_FLAGS;const title:TFRE_DB_String;const CaptionFields:TFRE_DB_StringArray=nil;const TreeNodeIconField:TFRE_DB_String='';
                                                const item_menu_func: TFRE_DB_SERVER_FUNC_DESC=nil; const item_details_func: TFRE_DB_SERVER_FUNC_DESC=nil; const selection_dep_func: TFRE_DB_SERVER_FUNC_DESC=nil; const tree_menu_func: TFRE_DB_SERVER_FUNC_DESC=nil; const drop_func: TFRE_DB_SERVER_FUNC_DESC=nil; const drag_func: TFRE_DB_SERVER_FUNC_DESC=nil); //TODO: Make Callable Once
    procedure  SetDisplayTypeChart             (const title: TFRE_DB_String; const chart_type: TFRE_DB_CHART_TYPE; const series_field_names: TFRE_DB_StringArray; const use_series_colors:boolean; const use_series_labels : boolean;const series_labels: TFRE_DB_StringArray=nil; const showLegend: Boolean=false; const maxValue: Integer=0);

    function   GetDisplayDescription           : TFRE_DB_CONTENT_DESC;
    procedure  FinalRightTransform             (const ses : IFRE_DB_UserSession ; const transformed_filtered_cloned_obj:IFRE_DB_Object);

    function   First                           : IFRE_DB_Object;
    function   Last                            : IFRE_DB_Object;
    function   FetchIndexed                    (const idx:NativeInt) : IFRE_DB_Object;
    function   FetchInDerived                  (const ouid:TFRE_DB_GUID;out dbo:IFRE_DB_Object): boolean; { honors rights and serverside filters, delivers the transformed(!) object !!}
    function   ItemCount                       : Int64;
    function   SetupQryDefinitionBasic         (const start, count,clientid: NativeInt): TFRE_DB_QUERY_DEF;
    function   SetupQryDefinitionFromWeb       (const web_input : IFRE_DB_Object):TFRE_DB_QUERY_DEF;
    function   ExecuteQryLocked                (const qrydef : TFRE_DB_QUERY_DEF ; out qry :TFRE_DB_QUERY_BASE):NativeInt; { the base and thus implicitly the filtered data is returned locked, does not store the query }
    function   ExecutePointQry                 (const qrydef : TFRE_DB_QUERY_DEF):IFRE_DB_Object; { the base and thus implicitly the filtered data is returned locked, does not store the query }
  published
    function   WEB_GET_GRID_DATA               (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
    function   WEB_GET_CHOOSER_DATA            (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
    function   WEB_CLEAR_QUERY_RESULTS         (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
    function   WEB_DESTROY_STORE               (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
    function   WEB_GET_CHART_DATA              (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
  end;


  TFRE_DB_Coll_Iterator     = procedure (const coll:TFRE_DB_COLLECTION) is nested;

  _TFRE_DB_CollectionTree   = specialize TGFOS_RBTree<TFRE_DB_String,TFRE_DB_COLLECTION>; // TODO -> make a sparse array

  OFRE_SL_TFRE_DB_BASE_CONNECTION = specialize OFOS_SpareList<TFRE_DB_BASE_CONNECTION>;

  { TFRE_DB_CONNECTION }
  TFRE_DB           =class;


  { TFRE_DB_BASE_CONNECTION }

  TFRE_DB_BASE_CONNECTION=class(TFOS_BASE,IFRE_DB_DBChangedNotificationConnection,IFRE_DB_DBChangedNotificationBlock)
  private
    FConnLock             : IFOS_LOCK;
    FDBName               : TFRE_DB_String;
    FBoundSession         : IFRE_DB_UserSession;

    FConnectionClones     : OFRE_SL_TFRE_DB_BASE_CONNECTION; { Only in Master, not in clones }
    FCloned               : boolean;

    FConnected            : Boolean;
    FAuthenticated        : Boolean;

    FPersistance_Layer    : IFRE_DB_PERSISTANCE_LAYER;

    FCollectionStore      : _TFRE_DB_CollectionTree;

    FSysNotes             : TFRE_DB_COLLECTION; {needed in SYSTEM and USER DB's}

    function            BackupDatabaseReadable      (const str : TStream;const progress : TFRE_DB_PhaseProgressCallback):TFRE_DB_Errortype;virtual;

    function            RestoreDatabaseReadable     (const from_stream:TStream;const progress : TFRE_DB_PhaseProgressCallback):TFRE_DB_Errortype;virtual;
    procedure           _ConnectCheck                ;
    procedure           _CloneCheck                  ;
    procedure           _CloneTrueCheck              ;
    function            Implementor                  : TObject;
    procedure           _AddCollectionToStore        (const coll_name : TFRE_DB_NameType ; const in_memory_only: boolean); {from notfif or CollectionCC}
  protected
    procedure  AcquireConnLock;
    procedure  ReleaseConnLock;
    procedure  BindUserSession               (const session : IFRE_DB_Usersession);virtual;
    procedure  ClearUserSessionBinding       ;virtual;
    function   GetNotifBlock                 : IFRE_DB_DBChangedNotificationBlock;


    { Notification Interface - Block }
    procedure  SendNotificationBlock  (const block : IFRE_DB_Object);
    { Notification Interface - Connection }
    procedure  CollectionCreated      (const coll_name: TFRE_DB_NameType  ; const in_memory_only: boolean ; const tsid : TFRE_DB_TransStepId) ;
    procedure  CollectionDeleted      (const coll_name: TFRE_DB_NameType  ; const tsid : TFRE_DB_TransStepId) ;
    procedure  IndexDefinedOnField    (const coll_name: TFRE_DB_NameType  ; const FieldName: TFRE_DB_NameType; const FieldType: TFRE_DB_FIELDTYPE; const unique: boolean; const ignore_content_case: boolean;
                                       const index_name: TFRE_DB_NameType ; const allow_null_value: boolean; const unique_null_values: boolean; const tsid : TFRE_DB_TransStepId);
    procedure  IndexDroppedOnField    (const coll_name: TFRE_DB_NameType  ; const index_name: TFRE_DB_NameType; const tsid : TFRE_DB_TransStepId);
    { Notification Interface - End }

    function            GetDatabaseObjectCount       (const Schemes:TFRE_DB_StringArray=nil):NativeInt;
    procedure           ForAllDatabaseObjectsDo      (const dbo:IFRE_DB_ObjectIteratorBrkProgress ; const Schemes:TFRE_DB_StringArray=nil); { Warning may take some time, delivers a clone }
    function            IsReferenced                 (const obj_uid:TFRE_DB_GUID; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):Boolean;virtual;
    function            IsReferenced                 (const obj_uid:TFRE_DB_GUID;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):Boolean;virtual; deprecated;
    function            GetReferences                (const obj_uid:TFRE_DB_GUID;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):TFRE_DB_GUIDArray;virtual;
    function            GetReferencesCount           (const obj_uid:TFRE_DB_GUID;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):NativeInt;virtual;
    function            GetReferencesDetailed        (const obj_uid:TFRE_DB_GUID;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):TFRE_DB_ObjectReferences;virtual;

    procedure          _CheckSchemeDefinitions      (const obj:TFRE_DB_Object);
    function           _FetchApp                    (const name:TFRE_DB_String;var app:TFRE_DB_APPLICATION):boolean;
    function           _Connect                     (const db:TFRE_DB_String ; const is_clone_connect : boolean):TFRE_DB_Errortype;virtual;
    procedure          InternalSetupConnection      ;virtual;
    function           CollectionExists             (const name:TFRE_DB_NameType):boolean;virtual;

    function           NewObjectCC                  (const ObjClass:TFRE_DB_OBJECTCLASS)                                     : TFRE_DB_Object; virtual;
    procedure          Finalize                     ;

    function           DatabaseExists                (const dbname:TFRE_DB_String):Boolean;virtual;
    function           CreateDatabase                (const dbname:TFRE_DB_String):TFRE_DB_Errortype;virtual;
    function           DeleteDatabase                (const dbname:TFRE_DB_String):TFRE_DB_Errortype;virtual;
    procedure          DumpSystem                    ;virtual;

  public
    function           DeleteCollection             (const name:TFRE_DB_NameType):TFRE_DB_Errortype;virtual;

    function           CollectionCC                 (const collection_name:TFRE_DB_NameType ; const create_non_existing:boolean=true;const in_memory_only:boolean=false):TFRE_DB_COLLECTION;virtual;

    function           FetchI                       (const ouid:TFRE_DB_GUID;out dbo:IFRE_DB_Object)                                : TFRE_DB_Errortype;
    function           FetchAsIntf                  (const ouid:TFRE_DB_GUID;const IntfSpec:ShortString; out Intf)                  : TFRE_DB_Errortype;
    function           NewScheme                    (const Scheme_Name: TFRE_DB_String;const parent_scheme_name:TFRE_DB_String='')           : TFRE_DB_SchemeObject;
    function           NewSchemeI                   (const Scheme_Name: TFRE_DB_String;const parent_scheme_name:TFRE_DB_String='')           : IFRE_DB_SchemeObject;

    function           ConnectedName                : TFRE_DB_String;
    function           CollectionList               (const with_classes:boolean=false):IFOS_STRINGS                          ; virtual;

    function           FetchDomainUIDbyName         (const name :TFRE_DB_NameType; var domain_uid:TFRE_DB_GUID):boolean; virtual;

    function           GetCollection                (const collection_name: TFRE_DB_NameType) : IFRE_DB_COLLECTION;
    function           CreateCollection             (const collection_name: TFRE_DB_NameType;const in_memory:boolean=false) : IFRE_DB_COLLECTION;

    function           Collection                   (const collection_name: TFRE_DB_NameType;const create_non_existing:boolean=true;const in_memory:boolean=false)  : TFRE_DB_COLLECTION;virtual;
    function           CollectionI                  (const collection_name: TFRE_DB_NameType;const create_non_existing:boolean=true;const in_memory:boolean=false)  : IFRE_DB_COLLECTION;virtual;
    //function           CollectionAsIntf             (const collection_name:TFRE_DB_NameType;const CollectionInterfaceSpec:ShortString;out Intf;const create_non_existing:boolean=true;const in_memory:boolean=false):boolean; // creates/fetches a Specific Collection

    procedure          ForAllColls                  (const iterator:TFRE_DB_Coll_Iterator)                                   ;virtual;
    procedure          ForAllSchemes                (const iterator:TFRE_DB_Scheme_Iterator)                                 ;
    procedure          ForAllEnums                  (const iterator:TFRE_DB_Enum_Iterator)                                   ;
    procedure          ForAllClientFieldValidators  (const iterator:TFRE_DB_ClientFieldValidator_Iterator)                   ;
    procedure          ForAllCollsI                 (const iterator:IFRE_DB_Coll_Iterator)                                   ;
    procedure          ForAllSchemesI               (const iterator:IFRE_DB_Scheme_Iterator)                                 ;
    procedure          ForAllEnumsI                 (const iterator:IFRE_DB_Enum_Iterator)                                   ;
    procedure          ForAllClientFieldValidatorsI (const iterator:IFRE_DB_ClientFieldValidator_Iterator)                   ;

    function           OverviewDump                 :TFRE_DB_String; virtual;
    constructor        Create                       (const clone : boolean)                                                  ;
    destructor         Destroy                                                                                               ; override;
    function           Exists                       (const ouid:TFRE_DB_GUID)                                                       : boolean;
    function           Delete                       (const ouid:TFRE_DB_GUID)                                                       : TFRE_DB_Errortype;virtual;
    function           UpcastDBC                    : TFRE_DB_Connection;
    function           UpcastSYS                    : TFRE_DB_SYSTEM_CONNECTION;

    function           GetCurrentUserTokenClone     : IFRE_DB_USER_RIGHT_TOKEN;virtual;abstract;
    function           GetCurrentUserTokenRef       : IFRE_DB_USER_RIGHT_TOKEN;virtual;abstract;
    function           GetMyDomainID                : TFRE_DB_GUID; virtual;abstract;
    function           GetMyDomainID_String         : TFRE_DB_GUID_String;
    function           GetSystemDomainID_String     : TFRE_DB_GUID_String;
    function           GetSysDomainUID              : TFRE_DB_GUID; virtual;abstract;
    function           GetUserUID                   : TFRE_DB_GUID; virtual;abstract;
    function           GetUserUIDP                  : PFRE_DB_GUID; virtual;abstract;

    function           Fetch                        (const ouid:TFRE_DB_GUID;out dbo:TFRE_DB_Object;const without_right_check:boolean=false) : TFRE_DB_Errortype; virtual;
    function           BulkFetchNoRightCheck        (const uids:TFRE_DB_GUIDArray;out dbos:IFRE_DB_ObjectArray) : TFRE_DB_Errortype; virtual;
    function           BulkFetch                    (const uids:TFRE_DB_GUIDArray;out dbos:IFRE_DB_ObjectArray) : TFRE_DB_Errortype; virtual;


    function           FetchAccessRightTest         (const ouid: TFRE_DB_GUID): boolean; { fetch the object, check rights and free / USE CASE ONLY GUID IS KNOWN }
    function           DeleteAccessRightTest        (const ouid: TFRE_DB_GUID): boolean; { fetch the object, check rights and free / USE CASE ONLY GUID IS KNOWN }
    function           CheckAccessRightAndCondFinalize(const dbi : IFRE_DB_Object ; const sr : TFRE_DB_STANDARD_RIGHT ; const without_right_check: boolean=false;const cond_finalize:boolean=true) : TFRE_DB_Errortype;
    function           Update                       (const dbo:TFRE_DB_Object ; const collection_name : TFRE_DB_NameType='') : TFRE_DB_Errortype;virtual;
    function           UpdateI                      (const dbo:IFRE_DB_Object)                                               : TFRE_DB_Errortype;
    function           FetchApplications            (var apps : TFRE_DB_APPLICATION_ARRAY; var loginapp: TFRE_DB_APPLICATION): TFRE_DB_Errortype;virtual;
    function           FetchApplicationsI           (var apps : IFRE_DB_APPLICATION_ARRAY; var loginapp: IFRE_DB_APPLICATION): TFRE_DB_Errortype;virtual; // with user rights
    procedure          DrawScheme                   (const datastream:TStream;const classfile:string)                        ; virtual;
  end;

  { TFRE_DB_USER_RIGHT_TOKEN }

  TFRE_DB_USER_RIGHT_TOKEN = class(IFRE_DB_USER_RIGHT_TOKEN)
  private
    FUsergroupIDs     : TFRE_DB_GUIDArray;
    FUserUID          : TFRE_DB_GUID;
    FDomainLoginKey   : TFRE_DB_String;
    FLoginAtDomain    : TFRE_DB_String;
    FUserLoginPart    : TFRE_DB_String;
    FUserFirstName    : TFRE_DB_String;
    FUserLastName     : TFRE_DB_String;
    FUserDescName     : TFRE_DB_String;
    FUniqueToken      : String[8];
    FIsSysAdmin       : Boolean;
    FMyDomainID       : TFRE_DB_GUID;
    FSysDomainUID     : TFRE_DB_GUID;
    FMyDomainID_GS    : TFRE_DB_GUID_String;
    FSysDomainID_GS   : TFRE_DB_GUID_String;
    FConnectionRights : TFRE_DB_StringArray;
    FAllDomainsUids   : TFRE_DB_GUIDArray;
    FAllDomainNames   : TFRE_DB_NameTypeArray;
    function    Implementor                  : TObject;
    function    IsCurrentUserSystemAdmin    : boolean; //inline;
    function    GetMyDomainID_String        : TFRE_DB_GUID_String; //inline;
    function    GetSystemDomainID_String    : TFRE_DB_GUID_String; //inline;
    function    GetDomainID                 (const domainname:TFRE_DB_NameType):TFRE_DB_GUID;//inline;
    function    GetDomainNameByUid          (const domainid:TFRE_DB_GUID):TFRE_DB_NameType;//inline;
    function    _GetStdRightName            (const std_right: TFRE_DB_STANDARD_RIGHT; const rclassname: ShortString ; const domainguid : TFRE_DB_GUID): TFRE_DB_String;
    function    _GetStdRightName            (const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass ; const domainguid : TFRE_DB_GUID): TFRE_DB_String;
    function    _GetStdRightName            (const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass): TFRE_DB_String;
    function    FetchAllDomainUids          : TFRE_DB_GUIDArray;

  public
    constructor Create                      (const user_uid: TFRE_DB_GUID; const login_part,firstname,lastname,desc: TFRE_DB_String; const group_ids: TFRE_DB_GUIDArray; const rights: TFRE_DB_StringArray; is_sys_admin: boolean; sysdom_id, user_domid: TFRE_DB_GUID; domainids: TFRE_DB_GUIDArray; domain_names: TFRE_DB_NameTypeArray);
    destructor  Destroy                     ;override;
    procedure   Finalize                    ;

    function    GetUserGroupIDS              : TFRE_DB_GUIDArray;
    function    GetUserUID                   : TFRE_DB_GUID;
    function    GetUserUIDP                  : PFRE_DB_GUID;
    function    GetDomainLoginKey            : TFRE_DB_String; { upper : domainuid_hex@login }
    function    GetFullUserLogin             : TFRE_DB_String; { login@domainname }
    procedure   GetUserDetails               (out fulluserlogin,firstname,lastname,description : TFRE_DB_String);

    function    CheckStdRightAndCondFinalize (const dbi : IFRE_DB_Object ; const sr : TFRE_DB_STANDARD_RIGHT ; const without_right_check: boolean=false;const cond_finalize:boolean=true) : TFRE_DB_Errortype;

    function    CheckStdRightsetUIDAndClass  (const obj_uid, obj_domuid: TFRE_DB_GUID; const check_classname: ShortString; const sr: TFRE_DB_STANDARD_RIGHT_SET): TFRE_DB_Errortype;
    function    CheckStdRightsetInternalObj  (const obj : TFRE_DB_Object ; const sr: TFRE_DB_STANDARD_RIGHT_SET): TFRE_DB_Errortype;
    function    CheckGenRightSetUIDAndClass  (const obj_uid, obj_domuid: TFRE_DB_GUID; const check_classname: ShortString; const sr: TFRE_DB_StringArray): TFRE_DB_Errortype;

    { Safe case, use for single domain use cases }
    function    CheckClassRight4MyDomain    (const right_name:TFRE_DB_String;const classtyp: TClass):boolean; { and systemuser and systemdomain}
    { Many domain case, add additional checks for the specific domain }
    function    CheckClassRight4AnyDomain   (const right_name:TFRE_DB_String;const classtyp: TClass):boolean;
    function    CheckClassRight4Domain      (const right_name:TFRE_DB_String;const classtyp: TClass;const domainKey:TFRE_DB_String=''):boolean;
    function    GetDomainsForRight          (const right_name:TFRE_DB_String): TFRE_DB_GUIDArray;
    function    GetDomainsForClassRight     (const right_name:TFRE_DB_String;const classtyp: TClass): TFRE_DB_GUIDArray;

    { Stdrights Many domain case, add additional checks for the specific domain }
    function    CheckClassRight4MyDomain    (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass):boolean;
    function    CheckClassRight4AnyDomain   (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass):boolean;

    function    CheckClassRight4Domain      (const std_right : TFRE_DB_STANDARD_RIGHT ; const classtyp: TClass;const domainKey:TFRE_DB_String=''):boolean; { specific domain }
    function    CheckClassRight4DomainId    (const right_name: TFRE_DB_String         ; const classtyp: TClass; const domain: TFRE_DB_GUID): boolean;
    function    CheckClassRight4DomainId    (const right_name: TFRE_DB_String         ; const rclassname: ShortString; const domain: TFRE_DB_GUID): boolean;
    function    CheckClassRight4DomainId    (const std_right : TFRE_DB_STANDARD_RIGHT ; const classtyp: TClass; const domain: TFRE_DB_GUID): boolean;
    function    CheckClassRight4DomainId    (const std_right : TFRE_DB_STANDARD_RIGHT ; const rclassname: ShortString; const domain: TFRE_DB_GUID): boolean;

    function    GetDomainsForClassRight     (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass): TFRE_DB_GUIDArray;
    function    GetDomainNamesForClassRight (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass): TFRE_DB_StringArray;

    function    CheckObjectRight            (const right_name : TFRE_DB_String ; const uid : TFRE_DB_GUID ):boolean;
    function    CheckObjectRight            (const std_right  : TFRE_DB_STANDARD_RIGHT ; const uid : TFRE_DB_GUID ):boolean;
    function    DumpUserRights              : TFRE_DB_String;
    function    GetMyDomainID               : TFRE_DB_GUID;
    function    GetSysDomainID              : TFRE_DB_GUID;
    function    GetUniqueTokenKey           : TFRE_DB_NameType;
    function    CloneToNewUserToken         : IFRE_DB_USER_RIGHT_TOKEN;
    function    Clone                       : TFRE_DB_USER_RIGHT_TOKEN;
  end;

  { TFRE_DB_SYSTEM_CONNECTION }

  TFRE_DB_SYSTEM_CONNECTION = class(TFRE_DB_BASE_CONNECTION,IFRE_DB_SYS_CONNECTION)
  private
    FClonedFrom          : TFRE_DB_SYSTEM_CONNECTION;
    FPairedAppDBConn     : TFRE_DB_CONNECTION;

    FSysDomainUID        : TFRE_DB_GUID;
    FSysTransText        : TFRE_DB_COLLECTION;
    FSysUsers            : TFRE_DB_COLLECTION;
    FSysRoles            : TFRE_DB_COLLECTION;
    FSysGroups           : TFRE_DB_COLLECTION;
    FSysDomains          : TFRE_DB_COLLECTION;
    FSysUserSessionsData : TFRE_DB_COLLECTION;
    FSysSingletons       : TFRE_DB_COLLECTION;
    FSysWorkflow         : TFRE_DB_COLLECTION; { the steps, may be with additional hierarchic levels }
    FSysWorkflowScheme   : TFRE_DB_COLLECTION; { the schemes, hierarchic }
    FSysWorkflowMethods  : TFRE_DB_COLLECTION; { the automatic steps }
    FSysNotifications    : TFRE_DB_COLLECTION;
    FSysAppConfigs       : TFRE_DB_COLLECTION;
    FSysAudit            : TFRE_DB_COLLECTION;

    FCurrentUserToken    : TFRE_DB_USER_RIGHT_TOKEN;

    function     ReNewCurrentUserToken      (const user : TFRE_DB_USER)     :IFRE_DB_USER_RIGHT_TOKEN;
    function     ImpersonateTheClone        (const user,pass:TFRE_DB_String):TFRE_DB_Errortype;
    procedure    InternalSetupConnection    ; override;
    //function    _GetStdRightName            (const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass ; const domainguid : TFRE_DB_GUID): TFRE_DB_String; //KILL
    //function    _GetStdRightName            (const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass): TFRE_DB_String; //KILL
    function    _RoleID                     (const rolename:TFRE_DB_String;const domainUID:TFRE_DB_GUID;var role_id:TFRE_DB_GUID):boolean;
    function    _FetchGroup                 (const group: TFRE_DB_String; const domain_id:TFRE_DB_GUID; var ug: TFRE_DB_GROUP):boolean;
    function    _FetchGroupbyID             (const group_id:TFRE_DB_GUID;var ug: TFRE_DB_GROUP;const without_right_check:boolean=false):TFRE_DB_Errortype;


    function    _AddUser(const login:TFRE_DB_String; const domainUID: TFRE_DB_GUID;const password, first_name, last_name: TFRE_DB_String; const system_start_up: boolean; const image: TFRE_DB_Stream=nil; const imagetype:string='' ; const etag: String='';const is_internal:Boolean=false ; const long_desc : TFRE_DB_String='' ; const short_desc : TFRE_DB_String=''): TFRE_DB_Errortype; // SPECIAL:SYSTEM STARTUP
    function    _AddRolesToGroup(const group:TFRE_DB_String;const domainUID: TFRE_DB_GUID;const roles: TFRE_DB_StringArray;const rolesDomainUID: TFRE_DB_GUID):TFRE_DB_Errortype;

    function    IFRE_DB_SYS_CONNECTION.FetchUser                   = FetchUserI;
    function    IFRE_DB_SYS_CONNECTION.FetchUserById               = FetchUserByIdI;
    function    IFRE_DB_SYS_CONNECTION.FetchGroup                  = FetchGroupI;
    function    IFRE_DB_SYS_CONNECTION.FetchGroupById              = FetchGroupByIdI;
    function    IFRE_DB_SYS_CONNECTION.FetchRoleById               = FetchRoleByIdI;
    function    IFRE_DB_SYS_CONNECTION.FetchDomainById             = FetchDomainByIdI;
    function    IFRE_DB_SYS_CONNECTION.NewRight                    = NewRightI;
    function    IFRE_DB_SYS_CONNECTION.StoreGroup                  = StoreGroupI;
    function    IFRE_DB_SYS_CONNECTION.UpdateGroup                 = UpdateGroupI;
    function    IFRE_DB_SYS_CONNECTION.StoreRole                   = StoreRoleI;
    function    IFRE_DB_SYS_CONNECTION.UpdateRole                  = UpdateRoleI;
    function    IFRE_DB_SYS_CONNECTION.UpdateDomain                = UpdateDomainI;
    function    IFRE_DB_SYS_CONNECTION.UpdateUser                  = UpdateUserI;
    function    IFRE_DB_SYS_CONNECTION.StoreAppData                = StoreAppDataI;
    function    IFRE_DB_SYS_CONNECTION.StoreTranslateableText      = StoreTranslateableTextI;
    function    IFRE_DB_SYS_CONNECTION.UpdateTranslateableText     = UpdateTranslateableTextI;
    function    IFRE_DB_SYS_CONNECTION.UpdateAppData               = UpdateAppDataI;
    function    IFRE_DB_SYS_CONNECTION.FetchAppData                = FetchAppDataI;
    function    IFRE_DB_SYS_CONNECTION.NewRole                     = NewRoleI;
    function    IFRE_DB_SYS_CONNECTION.NewGroup                    = NewGroupI;
    function    IFRE_DB_SYS_CONNECTION.NewAppData                  = NewAppDataI;
    function    IFRE_DB_SYS_CONNECTION.FetchApplications           = FetchApplicationsI;
    function    IFRE_DB_SYS_CONNECTION.GetAllClientFieldValidators = GetAllClientFieldValidatorsI;

    function    IFRE_DB_SYS_CONNECTION.AssociateObject             = AssociateObjectI;
    function    IFRE_DB_SYS_CONNECTION.FetchTranslateableText      = FetchTranslateableTextI;
    function    IFRE_DB_SYS_CONNECTION.FetchRole                   = FetchRoleI;
    function    IFRE_DB_SYS_CONNECTION.ForAllDomains               = ForAllDomainsI;

    function    NewRoleI                     (const rolename,txt,txt_short:TFRE_DB_String;const is_internal:Boolean;var right_group:IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    NewGroupI                    (const groupname,txt,txt_short:TFRE_DB_String;const is_protected:Boolean;const is_internal:Boolean;var user_group:IFRE_DB_GROUP):TFRE_DB_Errortype;

    function    StoreRoleI                   (var   role:IFRE_DB_ROLE; const domainUID : TFRE_DB_GUID):TFRE_DB_Errortype;
    function    UpdateRoleI                  (var   role:IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    UpdateDomainI                (var   domain:IFRE_DB_DOMAIN):TFRE_DB_Errortype;
    function    UpdateUserI                  (var   user:IFRE_DB_USER):TFRE_DB_Errortype;

    function    StoreGroupI                  (var   group: IFRE_DB_GROUP;const domainUID: TFRE_DB_GUID): TFRE_DB_Errortype;
    function    UpdateGroupI                 (var   group: IFRE_DB_GROUP): TFRE_DB_Errortype;
    function    StoreTranslateableTextI      (const txt    :IFRE_DB_TEXT) :TFRE_DB_Errortype;
  protected
    //property    UserToken                    : IFRE_DB_USER_RIGHT_TOKEN read FCurrentUserToken implements IFRE_DB_USER_RIGHT_TOKEN;
  public
    destructor  Destroy                     ; override;
    procedure   DumpSystem                  ;override;

    function    CheckLogin                  (const loginatdomain,pass:TFRE_DB_String):TFRE_DB_Errortype;
    function    Connect                     (const loginatdomain:TFRE_DB_String='';const password:TFRE_DB_String='') : TFRE_DB_Errortype;

    function    AddUser                     (const login:TFRE_DB_String; const domainUID: TFRE_DB_GUID;const password,first_name,last_name:TFRE_DB_String;const image : TFRE_DB_Stream=nil; const imagetype : String='';const is_internal:Boolean=false; const long_desc : TFRE_DB_String='' ; const short_desc : TFRE_DB_String=''):TFRE_DB_Errortype;
    function    UserExists                  (const login:TFRE_DB_String; const domainUID: TFRE_DB_GUID):boolean;
    function    DeleteUser                  (const login:TFRE_DB_String; const domainUID: TFRE_DB_GUID):TFRE_DB_Errortype;
    function    DeleteUserById              (const user_id:TFRE_DB_GUID):TFRE_DB_Errortype;
    function    FetchUser                   (const login:TFRE_DB_String; const domainUID: TFRE_DB_GUID;var user:TFRE_DB_USER):TFRE_DB_Errortype;
    function    FetchUserI                  (const login:TFRE_DB_String; const domainUID: TFRE_DB_GUID;var user:IFRE_DB_USER):TFRE_DB_Errortype;
    function    FetchUserById               (const user_id:TFRE_DB_GUID;var user: TFRE_DB_USER):TFRE_DB_Errortype;
    function    FetchUserByIdI              (const user_id:TFRE_DB_GUID;var user: IFRE_DB_USER):TFRE_DB_Errortype;
    function    FetchGroup                  (const group: TFRE_DB_String; const domain_id:TFRE_DB_GUID;var ug: TFRE_DB_GROUP):TFRE_DB_Errortype;
    function    FetchGroupI                 (const group: TFRE_DB_String; const domain_id:TFRE_DB_GUID;var ug: IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    FetchGroupById              (const group_id:TFRE_DB_GUID;var ug: TFRE_DB_GROUP ; without_right_check : boolean = false):TFRE_DB_Errortype;
    function    FetchGroupByIdI             (const group_id:TFRE_DB_GUID;var ug: IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    ModifyGroupById             (const group_id:TFRE_DB_GUID;const groupname: TFRE_DB_NameType; const txt,txt_short:TFRE_DB_String):TFRE_DB_Errortype;
    function    FetchRole                   (const rolename:TFRE_DB_String;const domainUID: TFRE_DB_GUID;var role: TFRE_DB_ROLE):TFRE_DB_Errortype;
    function    FetchRoleI                  (const rolename:TFRE_DB_String;const domainUID: TFRE_DB_GUID;var role: IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    FetchRoleById               (const role_id: TFRE_DB_GUID; var role: TFRE_DB_ROLE; const without_right_check: boolean=false): TFRE_DB_Errortype;
    function    FetchRoleByIdI              (const role_id:TFRE_DB_GUID;var role: IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    FetchAllDomainUids          : TFRE_DB_GUIDArray;
    function    FetchDomain                 (const name :TFRE_DB_NameType; var domain:TFRE_DB_DOMAIN):boolean;
    function    FetchDomainUIDbyName        (const name :TFRE_DB_NameType; var domain_uid:TFRE_DB_GUID):boolean; override;
    function    FetchDomainById             (const domain_id:TFRE_DB_GUID;var domain: TFRE_DB_DOMAIN):TFRE_DB_Errortype;
    function    FetchDomainByIdI            (const domain_id:TFRE_DB_GUID;var domain: IFRE_DB_DOMAIN):TFRE_DB_Errortype;
    function    FetchDomainNameById         (const domain_id:TFRE_DB_GUID):TFRE_DB_NameType;
    function    ModifyDomainById            (const domain_id:TFRE_DB_GUID;const domainname: TFRE_DB_NameType; const txt,txt_short:TFRE_DB_String):TFRE_DB_Errortype;
    function    SuspendContinueDomainById   (const domain_id:TFRE_DB_GUID; const suspend : boolean):TFRE_DB_Errortype;
    function    IsDomainSuspended           (const domainname:TFRE_DB_NameType):boolean; {delivers true if the domain exists and is suspended, otherwise false}
    function    DeleteDomainById            (const domain_id:TFRE_DB_GUID):TFRE_DB_Errortype;
    function    DomainExists                (const domainname:TFRE_DB_NameType):boolean;
    function    DomainID                    (const domainname:TFRE_DB_NameType):TFRE_DB_GUID;
    function    DeleteDomain                (const domainname:TFRE_DB_Nametype):TFRE_DB_Errortype;
    function    IsSystemGroup               (const group_id:TFRE_DB_GUID):boolean;
    procedure   ForAllDomainsI              (const func:IFRE_DB_Domain_Iterator);

    function    FetchUserSessionData        (var SessionData: IFRE_DB_OBJECT):boolean;
    function    StoreUserSessionData        (var session_data:IFRE_DB_Object):TFRE_DB_Errortype;

    function    NewRole                     (const rolename,txt,txt_short:TFRE_DB_String;const is_internal:Boolean;var role:TFRE_DB_ROLE):TFRE_DB_Errortype;
    function    NewGroup                    (const groupname,txt,txt_short:TFRE_DB_String;const is_protected:Boolean;const is_internal:Boolean;var user_group:TFRE_DB_GROUP):TFRE_DB_Errortype;
    function    AddRoleRightsToRole         (const rolename:TFRE_DB_String;const domainUID: TFRE_DB_GUID;const roles: TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    RemoveRightsFromRole        (const rolename:TFRE_DB_String;const rights:TFRE_DB_StringArray; const domainUID: TFRE_DB_GUID):TFRE_DB_Errortype;
    function    AddRole                     (const rolename,txt,txt_short:TFRE_DB_String;const domainUID:TFRE_DB_GUID; const is_internal:Boolean=false):TFRE_DB_Errortype;
    function    AddRolesToGroupById         (const group:TFRE_DB_String;const domainUID: TFRE_DB_GUID;const role_ids: TFRE_DB_GUIDArray):TFRE_DB_Errortype;
    function    AddRolesToGroup             (const group:TFRE_DB_String;const domainUID: TFRE_DB_GUID;const roles: TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    AddSysRolesToGroup          (const group:TFRE_DB_String;const domainUID: TFRE_DB_GUID;const roles: TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    AddGroup                    (const groupname,txt,txt_short:TFRE_DB_String;const domainUID:TFRE_DB_GUID;const is_protected:Boolean=false;const is_internal:Boolean=false):TFRE_DB_Errortype;
    function    RemoveRolesFromGroupById    (const group:TFRE_DB_String;const domainUID: TFRE_DB_GUID;const role_ids: TFRE_DB_GUIDArray; const ignore_not_set:boolean): TFRE_DB_Errortype;
    function    RemoveRolesFromGroup        (const group:TFRE_DB_String;const domainUID: TFRE_DB_GUID;const roles: TFRE_DB_StringArray; const ignore_not_set:boolean): TFRE_DB_Errortype;
    function    RemoveAllRolesFromGroup     (const group:TFRE_DB_String;const domainUID: TFRE_DB_GUID): TFRE_DB_Errortype;
    function    RemoveRoleFromAllGroups     (const role:TFRE_DB_String;const domainUID: TFRE_DB_GUID): TFRE_DB_Errortype;
    function    ModifyUserGroupsById        (const user_id:TFRE_DB_GUID; const user_group_ids:TFRE_DB_GUIDArray;const keep_existing_groups:boolean=false):TFRE_DB_Errortype;
    function    RemoveUserGroupsById        (const user_id:TFRE_DB_GUID; const user_group_ids:TFRE_DB_GUIDArray):TFRE_DB_Errortype;
    function    ModifyUserPassword          (const login:TFRE_DB_String; const domainUID: TFRE_DB_GUID;const oldpassword,newpassword:TFRE_DB_String):TFRE_DB_Errortype;
    function    RoleExists                  (const role:TFRE_DB_String;const domainUID: TFRE_DB_GUID):boolean;
    function    GroupExists                 (const group:TFRE_DB_String;const domainUID: TFRE_DB_GUID):boolean;
    function    DeleteGroup                 (const group:TFRE_DB_String;const domainUID: TFRE_DB_GUID):TFRE_DB_Errortype;
    function    DeleteGroupById             (const group_id:TFRE_DB_GUID):TFRE_DB_Errortype;
    function    DeleteRole                  (const role:TFRE_DB_String;const domainUID: TFRE_DB_GUID):TFRE_DB_Errortype;
    function    StoreRole                   (var role:TFRE_DB_ROLE; const domainUID : TFRE_DB_GUID):TFRE_DB_Errortype;
    function    UpdateRole                  (var role:TFRE_DB_ROLE):TFRE_DB_Errortype;
    function    StoreGroup                  (const domain_id: TFRE_DB_GUID; var group: TFRE_DB_GROUP): TFRE_DB_Errortype;
    function    UpdateGroup                 (var group: TFRE_DB_GROUP): TFRE_DB_Errortype;
    function    UpdateDomain                (var domain: TFRE_DB_DOMAIN): TFRE_DB_Errortype;
    function    UpdateUser                  (var user: TFRE_DB_USER): TFRE_DB_Errortype;

    function    StoreTranslateableText      (var   txt    :TFRE_DB_TEXT) :TFRE_DB_Errortype;
    function    UpdateTranslateableText     (const txt    :TFRE_DB_TEXT) :TFRE_DB_Errortype;
    function    UpdateTranslateableTextI    (const txt    :IFRE_DB_TEXT) :TFRE_DB_Errortype;
    function    DeleteTranslateableText     (const key    :TFRE_DB_String) :TFRE_DB_Errortype;

    //{ Safe case, use for single domain use cases }
    function    CheckClassRight4MyDomain    (const right_name:TFRE_DB_String;const classtyp: TClass):boolean; { and systemuser and systemdomain}

    { Many domain case, add additional checks for the specific domain }
    function    CheckClassRight4AnyDomain   (const right_name:TFRE_DB_String;const classtyp: TClass):boolean;
    function    CheckClassRight4Domain      (const right_name:TFRE_DB_String;const classtyp: TClass;const domainKey:TFRE_DB_String=''):boolean;
    function    CheckClassRight4DomainId    (const right_name:TFRE_DB_String;const classtyp: TClass;const domain:TFRE_DB_GUID):boolean;
    function    GetDomainsForRight          (const right_name:TFRE_DB_String): TFRE_DB_GUIDArray;
    function    GetDomainsForClassRight     (const right_name:TFRE_DB_String;const classtyp: TClass): TFRE_DB_GUIDArray;

    { Stdrights Many domain case, add additional checks for the specific domain }
    function    CheckClassRight4MyDomain    (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass):boolean;
    function    CheckClassRight4AnyDomain   (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass):boolean;

    function    CheckClassRight4Domain      (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass;const domainKey:TFRE_DB_String=''):boolean; { specific domain }
    function    CheckClassRight4DomainId    (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass;const domain:TFRE_DB_GUID):boolean; { specific domain }
    function    GetDomainsForClassRight     (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass): TFRE_DB_GUIDArray;
    function    GetDomainNamesForClassRight (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass): TFRE_DB_StringArray;

    function    CheckObjectRight            (const right_name : TFRE_DB_String         ; const uid : TFRE_DB_GUID ):boolean;
    function    CheckObjectRight            (const std_right  : TFRE_DB_STANDARD_RIGHT ; const uid : TFRE_DB_GUID ):boolean;// New is senseless


    function    FetchTranslateableText      (const trans_key:TFRE_DB_String;var ttext:TFRE_DB_TEXT):TFRE_DB_Errortype;
    function    FetchTranslateableTextI     (const translation_key:TFRE_DB_String; var textObj: IFRE_DB_TEXT):Boolean;//don't finalize the object


    function    BackupDatabaseReadable      (const sys,adb : TStream;const progress : TFRE_DB_PhaseProgressCallback):TFRE_DB_Errortype;
    function    RestoreDatabaseReadable     (const sys,adb : TStream;const db_name:string;const progress : TFRE_DB_PhaseProgressCallback):TFRE_DB_Errortype;


    function    GetClassesVersionDirectory  : IFRE_DB_Object;
    function    StoreClassesVersionDirectory(const version_dbo : IFRE_DB_Object) : TFRE_DB_Errortype;
    function    DelClassesVersionDirectory  : TFRE_DB_Errortype;

    procedure   DrawScheme                   (const datastream:TStream;const classfile:string);override;

    function    DumpUserRights               :TFRE_DB_String;

    procedure   StartTransaction             (const trans_id: TFRE_DB_NameType ; const trans_type : TFRE_DB_TRANSACTION_TYPE);
    procedure   Commit                       ;
    procedure   Rollback                     ;
    function    GetSysDomainUID              : TFRE_DB_GUID; override;
    function    GetUserUID                   : TFRE_DB_GUID; override;
    function    GetUserUIDP                  : PFRE_DB_GUID; override;

    function    GetMyDomainID                : TFRE_DB_GUID;override;
    procedure   ReloadUserandRights          (useruid : TFRE_DB_GUID);
    function    APP                          : IFRE_DB_CONNECTION;
    function    GetCurrentUserTokenClone     : IFRE_DB_USER_RIGHT_TOKEN;override;
    function    GetCurrentUserTokenRef       : IFRE_DB_USER_RIGHT_TOKEN;override;
  end;


  TFRE_DB_CONNECTION=class(TFRE_DB_BASE_CONNECTION,IFRE_DB_CONNECTION)
  private
    FClonedFrom         : TFRE_DB_CONNECTION;
    FSysConnection      : TFRE_DB_SYSTEM_CONNECTION;
    FProxySysconnection : boolean;                             { This Sysconnection is a seperate connection not belonging to the connection,
                                                                 must not be a clone, }
    function    IFRE_DB_CONNECTION.GetScheme                   = GetSchemeI;
    function    IFRE_DB_CONNECTION.Collection                  = CollectionI;
    function    IFRE_DB_CONNECTION.StoreScheme                 = StoreSchemeI;
    function    IFRE_DB_CONNECTION.Fetch                       = FetchI;
    function    IFRE_DB_CONNECTION.Update                      = UpdateI;
    //function    IFRE_DB_CONNECTION.ForAllObjects               = ForAllObjectsI;
    function    IFRE_DB_CONNECTION.ForAllColls                 = ForAllCollsI;
    function    IFRE_DB_CONNECTION.ForAllSchemes               = ForAllSchemesI;
    function    IFRE_DB_CONNECTION.ForAllEnums                 = ForAllEnumsI;
    function    IFRE_DB_CONNECTION.ForAllClientFieldValidators = ForAllClientFieldValidatorsI;
    function    IFRE_DB_CONNECTION.NewScheme                   = NewSchemeI;
    function    IFRE_DB_CONNECTION.FetchApplications           = FetchApplicationsI;
    function    IFRE_DB_CONNECTION.GetAllClientFieldValidators = GetAllClientFieldValidatorsI;
    function    IFRE_DB_CONNECTION.StoreClientFieldValidator   = StoreClientFieldValidatorI;
    function    IFRE_DB_CONNECTION.GetSchemeCollection         = GetSchemeCollectionI;
    function    IFRE_DB_CONNECTION.AssociateObject             = AssociateObjectI;
    function    CreateAClone                                   : TFRE_DB_CONNECTION;
  protected
    procedure   InternalSetupConnection   ;override;
    //function    IsCurrentUserSystemAdmin: boolean; override;
  public
    procedure   BindUserSession               (const session : IFRE_DB_Usersession);override;
    procedure   ClearUserSessionBinding       ;override;

    function    GetDatabaseName           : TFRE_DB_String;
    function    ImpersonateClone          (const user,pass:TFRE_DB_String;out conn:TFRE_DB_CONNECTION): TFRE_DB_Errortype;

    function    Connect                   (const db,user,pass:TFRE_DB_String;const ProxySysConnection:TFRE_DB_SYSTEM_CONNECTION):TFRE_DB_Errortype;
    function    Connect                   (Const db:TFRE_DB_String;const user:TFRE_DB_String='';const password:TFRE_DB_String='') : TFRE_DB_Errortype;

    function    CheckLogin                (const user,pass:TFRE_DB_String):TFRE_DB_Errortype;
    function    CollectionExists          (const name:TFRE_DB_NameType):boolean;override;
    function    DeleteCollection          (const name:TFRE_DB_NameType):TFRE_DB_Errortype;override;

    function    Delete                    (const ouid: TFRE_DB_GUID): TFRE_DB_Errortype; override;
    function    Update                    (const dbo:TFRE_DB_Object ; const collection_name : TFRE_DB_NameType='') : TFRE_DB_Errortype;override; { hack for notification, remove }

    destructor  Destroy                   ;override;

    function    FetchApplications         (var apps : TFRE_DB_APPLICATION_ARRAY; var loginapp: TFRE_DB_APPLICATION):TFRE_DB_Errortype;override;
    function    InvokeMethod              (const class_name,method_name:TFRE_DB_String;const uid_path:TFRE_DB_GUIDArray;var input:IFRE_DB_Object;const session:TFRE_DB_UserSession):IFRE_DB_Object;

    function    CreateschemeUniqueKeyDefinition (const schemeClass,FieldName:TFRE_DB_String ; const FieldType:TFRE_DB_FIELDTYPE):TFRE_DB_Errortype;
    function    DropschemeUniqueKeyDefinition   (const schemeClass,FieldName:TFRE_DB_String ; const FieldType:TFRE_DB_FIELDTYPE):TFRE_DB_Errortype;

    //Warning Fetching from DB, and then from system can have undesired side effects ...
    function    Fetch                       (const ouid:TFRE_DB_GUID;out dbo:TFRE_DB_Object ; const without_right_check:boolean=false) : TFRE_DB_Errortype; override;
    function    FetchAs                     (const ouid:TFRE_DB_GUID;const classref : TFRE_DB_BaseClass ; var outobj) : TFRE_DB_Errortype;
    function    BulkFetchNoRightCheck       (const uids:TFRE_DB_GUIDArray;out dbos:IFRE_DB_ObjectArray) : TFRE_DB_Errortype; override;
    function    BulkFetch                   (const uids:TFRE_DB_GUIDArray;out dbos:IFRE_DB_ObjectArray) : TFRE_DB_Errortype; override; { uids must be from one db }

    function    AdmGetTextResourcesCollection    :IFRE_DB_COLLECTION;
    function    AdmGetUserCollection             :IFRE_DB_COLLECTION;
    function    AdmGetRoleCollection             :IFRE_DB_COLLECTION;
    function    AdmGetGroupCollection            :IFRE_DB_COLLECTION;
    function    AdmGetDomainCollection           :IFRE_DB_COLLECTION;
    function    AdmGetAuditCollection            :IFRE_DB_COLLECTION;
    function    AdmGetWorkFlowCollection         :IFRE_DB_COLLECTION;
    function    AdmGetWorkFlowSchemeCollection   :IFRE_DB_COLLECTION;
    function    AdmGetWorkFlowMethCollection     :IFRE_DB_COLLECTION;
    function    AdmGetNotificationCollection     :IFRE_DB_COLLECTION;
    function    AdmGetApplicationConfigCollection:IFRE_DB_COLLECTION;

    function    FetchUserSessionData           (var SessionData: IFRE_DB_OBJECT):boolean;
    function    StoreUserSessionData           (var session_data:IFRE_DB_Object):TFRE_DB_Errortype;

    function    FetchTranslateableTextObj      (const trans_key:TFRE_DB_String;var text:IFRE_DB_TEXT):boolean;
    function    FetchTranslateableTextShort    (const translation_key:TFRE_DB_String):TFRE_DB_String;
    function    FetchTranslateableTextLong     (const translation_key:TFRE_DB_String):TFRE_DB_String;
    function    FetchTranslateableTextHint     (const translation_key:TFRE_DB_String):TFRE_DB_String;

    function    GetReferences                  (const obj_uid:TFRE_DB_GUID;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):TFRE_DB_GUIDArray;override;
    function    GetReferencesCountNoRightCheck (const obj_uid:TFRE_DB_GUID;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):NativeInt;
    function    GetReferencesNoRightCheck      (const obj_uid:TFRE_DB_GUID;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):TFRE_DB_GUIDArray;
    function    GetReferencesCount             (const obj_uid:TFRE_DB_GUID;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):NativeInt;override;
    function    GetReferencesDetailed          (const obj_uid:TFRE_DB_GUID;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):TFRE_DB_ObjectReferences;override;
    function    FetchDomainUIDbyName           (const name :TFRE_DB_NameType; var domain_uid:TFRE_DB_GUID):boolean; override;

    procedure   ExpandReferencesNoRightCheck   (ObjectList : TFRE_DB_GUIDArray ; ref_constraints : TFRE_DB_NameTypeRLArray ;  var expanded_refs : TFRE_DB_GUIDArray);
    procedure   ExpandReferences               (ObjectList : TFRE_DB_GUIDArray ; ref_constraints : TFRE_DB_NameTypeRLArray ;  var expanded_refs : TFRE_DB_ObjectArray); { TODO: BulkFetch, UserToken usage}
    procedure   ExpandReferences               (ObjectList : TFRE_DB_GUIDArray ; ref_constraints : TFRE_DB_NameTypeRLArray ;  var expanded_refs : TFRE_DB_GUIDArray);   { TODO: BulkFetch, UserToken usage}
    procedure   ExpandReferences               (ObjectList : TFRE_DB_GUIDArray ; ref_constraints : TFRE_DB_NameTypeRLArray ;  var expanded_refs : IFRE_DB_ObjectArray); { TODO: BulkFetch, UserToken usage}

    function    CreateDerivedCollection        (const collection_name: TFRE_DB_NameType): IFRE_DB_DERIVED_COLLECTION;

    function    SYS                            :IFRE_DB_SYS_CONNECTION;
    function    SYSC                           :TFRE_DB_SYSTEM_CONNECTION;
    function    GetSysDomainUID                :TFRE_DB_GUID; override;
    function    GetMyDomainID                  :TFRE_DB_GUID; override;
    function    GetUserUID                     :TFRE_DB_GUID; override;
    function    GetUserUIDP                    :PFRE_DB_GUID; override;

    function    AddDomain                      (const domainname:TFRE_DB_NameType;const txt,txt_short:TFRE_DB_String):TFRE_DB_Errortype;  // TODO: Do all in a Transaction

    procedure   DrawScheme                   (const datastream:TStream;const classfile:string) ; override ;
    function    GetCurrentUserTokenClone     :IFRE_DB_USER_RIGHT_TOKEN;override;
    function    GetCurrentUserTokenRef       :IFRE_DB_USER_RIGHT_TOKEN;override;
  end;

  TFRE_RInterfaceImplementor =record
    InterfaceSpec     : ShortString;
    ImplementingClass : TClass;
  end;

  TFRE_RExtensionClass = record
    exclass        : TFRE_DB_OBJECTCLASSEX;
    initialized    : boolean;
  end;

  TFRE_RsysClass = record
    sysclass       : TFRE_DB_OBJECTCLASS;
    initialized    : boolean;
  end;

  TFRE_RSysObjects = record
    SysGuid : TFRE_DB_GUID;
    Obj     : TFRE_DB_Object;
  end;

  TFRE_DB=class(TObject,IFRE_DB)
  private
    FNetServer                              : IFRE_DB_NetServer;
    FWeakMediatorLock                       : IFOS_Lock;
    FLocalZone                              : TFRE_DB_String;
    FFormatSettings                         : TFormatSettings;
    FClassArray                             : Array of  TFRE_RsysClass;
    FExClassArray                           : Array of  TFRE_RExtensionClass;
    FWeakExClassArray                       : Array of  TFRE_DB_WeakObjectEx;
    FKnownInterfaces                        : Array of  TFRE_RInterfaceImplementor;
    FSysSchemes                             : Array of  TFRE_DB_SchemeObject;
    FSysEnums                               : Array of TFRE_DB_Enum;
    FSysClientFieldValidators               : Array of TFRE_DB_ClientFieldValidator;
    FAppArray                               : TFRE_DB_APPLICATION_ARRAY;
    FSysObjectList                          : ARRAY of TFRE_RSysObjects;
    function        NetServ                 : IFRE_DB_NetServer;
    function        GetFormatSettings       : TFormatSettings;
    function        GetLocalZone            : TFRE_DB_String;
    procedure       SetFormatSettings       (const AValue: TFormatSettings);
    procedure       SetLocalZone            (const AValue: TFRE_DB_String);
    function        NewObjectStreaming      (const ClName: ShortString) : TFRE_DB_Object;

    procedure   _IntDBInitializeAllExClasses (const conn: IFRE_DB_CONNECTION; const installforonedomain: boolean; const onedomainUID:TFRE_DB_GUID);
    procedure   _IntDBInitializeAllSysClasses(const conn: IFRE_DB_CONNECTION; const installforonedomain: boolean; const onedomainUID:TFRE_DB_GUID);

  protected
    procedure   AcquireWeakMediatorLock     ;
    procedure   ReleaseWeakMediatorLock     ;
    function    NewScheme                   (const Scheme_Name: TFRE_DB_String;const typ : TFRE_DB_SchemeType) : TFRE_DB_SchemeObject;
    procedure   SafeFinalize                (intf : IFRE_DB_BASE);
    function    NewDBCommand                : IFRE_DB_COMMAND;
    function    FetchApplications           (var apps : IFRE_DB_APPLICATION_ARRAY; var loginapp: IFRE_DB_APPLICATION):TFRE_DB_Errortype;
    function    NewObjectIntf               (const InterfaceSpec:ShortString;out Intf;const mediator : TFRE_DB_ObjectEx=nil;const fail_on_non_existent:boolean=true) : Boolean;
    function    NewObjectI                  : IFRE_DB_Object;
    function    CreateFromFileI             (const filename:TFRE_DB_String):IFRE_DB_Object;
    function    CreateFromMemoryI           (memory : Pointer):IFRE_DB_Object;
    function    CreateFromStringI           (const AValue:TFRE_DB_String):IFRE_DB_Object;
    function    ConstructObjectArrayI       (const A:Array of IFRE_DB_Object):IFRE_DB_ObjectArray;
    function    ConstructObjectArrayOI      (const A:Array of TFRE_DB_Object):IFRE_DB_ObjectArray;
    function    ConstructObjectArrayIO      (const A:Array of IFRE_DB_Object):TFRE_DB_ObjectArray;
    function    CreateTextI                 (const translation_key:TFRE_DB_String;const short_text:TFRE_DB_String;const long_text:TFRE_DB_String='';const hint_text:TFRE_DB_String=''):IFRE_DB_TEXT;
    function    NewConnectionI              (const direct : boolean = true): IFRE_DB_CONNECTION;
    function    NewSysOnlyConnectionI       : IFRE_DB_SYS_CONNECTION;
    function    DatabaseListI               (const user:TFRE_DB_String='';const pass:TFRE_DB_String=''): IFOS_STRINGS;
    function    _NewObject                  (const Scheme: TFRE_DB_String;const fail_on_no_cc:boolean): TFRE_DB_Object;

    function    _NewText                    (const key,txt,txt_short:TFRE_DB_String;const hint:TFRE_DB_String=''):TFRE_DB_TEXT;
    function    _NewRole                    (const rolename,txt,txt_short:TFRE_DB_String;const is_internal:Boolean):TFRE_DB_ROLE;
    function    _NewGroup                   (const groupname,txt,txt_short:TFRE_DB_String;const is_protected:Boolean;const is_internal:Boolean):TFRE_DB_GROUP;
    function    _NewDomain                  (const domainname,txt,txt_short:TFRE_DB_String):TFRE_DB_DOMAIN;

    function    NewText                     (const key,txt,txt_short:TFRE_DB_String;const hint:TFRE_DB_String=''):IFRE_DB_TEXT;
    function    NewRole                     (const rolename,txt,txt_short:TFRE_DB_String;const is_internal:Boolean=false):IFRE_DB_ROLE;
    function    NewGroup                    (const groupname,txt,txt_short:TFRE_DB_String;const is_protected:Boolean=false;const is_internal:Boolean=false):IFRE_DB_GROUP;
    function    NewEnum                         (const name: TFRE_DB_String) : IFRE_DB_Enum;
    function    RegisterSysClientFieldValidator (const val : IFRE_DB_ClientFieldValidator):TFRE_DB_Errortype;
    function    RegisterSysEnum                 (const enu : IFRE_DB_Enum):TFRE_DB_Errortype;
    function    RegisterSysScheme               (const sch : TFRE_DB_SchemeObject):TFRE_DB_Errortype;
    function    AnonObject2Interface   (const Data   : Pointer):IFRE_DB_Object;

    function    IFRE_DB.NewObject               = NewObjectI;
    function    IFRE_DB.CreateFromFile          = CreateFromFileI;
    function    IFRE_DB.CreateFromMemory        = CreateFromMemoryI;
    function    IFRE_DB.CreateFromString        = CreateFromStringI;
    function    IFRE_DB.ConstructObjectArray    = ConstructObjectArrayI;
    function    IFRE_DB.CreateText              = CreateTextI;
    function    IFRE_DB.NewConnection           = NewConnectionI;
    function    IFRE_DB.NewSysOnlyConnection    = NewSysOnlyConnectionI;
    function    IFRE_DB.DatabaseList            = DatabaseListI;
    function    IFRE_DB.NewClientFieldValidator = NewClientFieldValidatorI;
    function    IFRE_DB.NewNamedObject          = NewNamedObjectI;

    procedure   AddSystemObjectToSysList     (const obj:TFRE_DB_Object);
    function    FetchSysObject               (const uid:TFRE_DB_GUID;var obj:TFRE_DB_Object):boolean;

    procedure   DBAddDomainInstAllExClasses      (const conn:IFRE_DB_CONNECTION; const domainUID:TFRE_DB_GUID);
    procedure   DBAddDomainInstAllSystemClasses  (const conn:IFRE_DB_CONNECTION; const domainUID:TFRE_DB_GUID);
    procedure   DBInitializeAllExClasses     (const conn:IFRE_DB_CONNECTION);
    procedure   DBInitializeAllSystemClasses (const conn:IFRE_DB_CONNECTION); // not impemented by now (no initializable sys classes, keep count low)

    procedure   Initialize_System_Objects    ;
    procedure   Initialize_Extension_Objects ;
    procedure   Finalize_Extension_Objects   ;

  public
    function    JSONObject2Object            (const json_string:string):IFRE_DB_Object;
    function    DefaultDirectory             : TFRE_DB_String;
    constructor Create                       ;
    destructor  Destroy                      ; override;
    function    GetApps                      : TFRE_DB_APPLICATION_ARRAY;
    function    GetAppInstanceByClass        (appclass : TClass ; out app  : TFRE_DB_APPLICATION):boolean;

    function    GetSysEnum                   (name:TFRE_DB_NameType ; out enum   : TFRE_DB_Enum):boolean;
    function    GetSysClientFieldValidator   (name:TFRE_DB_NameType ; out clf    : TFRE_DB_ClientFieldValidator):boolean;
    function    GetSysScheme                 (name:TFRE_DB_NameType ; out scheme : TFRE_DB_SchemeObject):boolean;

    function    LocalTimeToUTCDB64           (const ADateTime64: TFRE_DB_DateTime64) : TFRE_DB_DateTime64;
    function    UTCToLocalTimeDB64           (const ADateTime64: TFRE_DB_DateTime64) : TFRE_DB_DateTime64;

    property    LocalZone              : TFRE_DB_String read GetLocalZone write SetLocalZone;

    procedure   RegisterObjectClass        (const ObClass  : TFRE_DB_OBJECTCLASS);
    procedure   RegisterObjectClassEx      (const ObClass  : TFRE_DB_OBJECTCLASSEX);
    function    RegisterWeakObjectExClass  (const clname   : ShortString) : TFRE_DB_WeakObjectEx;
    procedure   RegisterPrimaryImplementor (const ObClass  : TClass ; const InterfaceSpec : ShortString); //register the primary implementor of an interface;
    function    GetInterfaceClass          (const InterfaceSpec : ShortString ; out ObClass:TClass) : boolean;

    procedure   GenerateAnObjChangeList    (const first_obj, second_obj: IFRE_DB_Object ; const InsertCB,DeleteCB : IFRE_DB_Obj_Iterator ; const UpdateCB : IFRE_DB_UpdateChange_Iterator);

    function    NewObject              (const ObjClass:TFRE_DB_OBJECTCLASS) : TFRE_DB_Object;
    function    NewObject              (const ObjClass:TFRE_DB_OBJECTCLASSEX) : TFRE_DB_Object;
    function    NewObject              (const ClName:TFRE_DB_String) : TFRE_DB_Object;
    function    NewObject              : TFRE_DB_Object;
    function    NewNamedObject         : TFRE_DB_NAMED_OBJECT;
    function    NewNamedObjectI        : IFRE_DB_NAMED_OBJECT;

    function    NewObjectScheme        (const Scheme : TClass): IFRE_DB_Object;
    function    NewObjectSchemeByName  (const Scheme : TFRE_DB_NameType): IFRE_DB_Object;

    function    NewClientFieldValidator(const name: TFRE_DB_String)       : TFRE_DB_ClientFieldValidator;
    function    NewClientFieldValidatorI(const name: TFRE_DB_String)      : IFRE_DB_ClientFieldValidator;

    function    GetObjectClass         (const ClName:ShortString) : TFRE_DB_OBJECTCLASS;
    function    GetObjectClassEx       (const ClName:ShortString) : TFRE_DB_OBJECTCLASSEX;
    function    GetWeakObjectClassEx   (const ClName:ShortString) : TFRE_DB_WeakObjectEx;
    function    ExistsObjectClass      (const ClName:ShortString) :boolean;
    function    ExistsObjectClassEx    (const ClName:ShortString) :boolean;

    function    NewConnection          (const direct : boolean = true): TFRE_DB_CONNECTION;
    function    NewDirectSysConnection : TFRE_DB_SYSTEM_CONNECTION;

    property    StringFormatSettings   : TFormatSettings read GetFormatSettings write SetFormatSettings;
    function    StringArray2String           (const A:TFRE_DB_StringArray):TFRE_DB_String;
    function    StringArray2GuidArray        (const A:TFRE_DB_StringArray):TFRE_DB_GUIDArray;
    function    StringArray2ByteArray        (const A:TFRE_DB_StringArray):TFRE_DB_ByteArray;
    function    StringArray2Int16Array       (const A:TFRE_DB_StringArray):TFRE_DB_Int16Array;
    function    StringArray2UInt16Array      (const A:TFRE_DB_StringArray):TFRE_DB_UInt16Array;
    function    StringArray2Int32Array       (const A:TFRE_DB_StringArray):TFRE_DB_Int32Array;
    function    StringArray2UInt32Array      (const A:TFRE_DB_StringArray):TFRE_DB_UInt32Array;
    function    StringArray2Int64Array       (const A:TFRE_DB_StringArray):TFRE_DB_Int64Array;
    function    StringArray2UInt64Array      (const A:TFRE_DB_StringArray):TFRE_DB_UInt64Array;
    function    StringArray2Real32Array      (const A:TFRE_DB_StringArray):TFRE_DB_Real32Array;
    function    StringArray2Real64Array      (const A:TFRE_DB_StringArray):TFRE_DB_Real64Array;
    function    StringArray2CurrArray        (const A:TFRE_DB_StringArray):TFRE_DB_CurrencyArray;
    function    StringArray2BoolArray        (const A:TFRE_DB_StringArray):TFRE_DB_BoolArray;
    function    StringArray2DateTimeArray    (const A:TFRE_DB_StringArray):TFRE_DB_DateTimeArray;
    function    StringArray2DateTimeArrayUTC (const A:TFRE_DB_StringArray):TFRE_DB_DateTimeArray;


    function    GuidArray2SString      (const A:TFRE_DB_GUIDArray):TFRE_DB_String;
    function    CountedObjLinks2String (const A:TFRE_DB_CountedGuidArray):TFRE_DB_String;

    function    TranslateLong          (const txt : TFRE_DB_TEXT):TFRE_DB_String;

    function    GetSystemScheme               (const schemename:TFRE_DB_NameType; var scheme: TFRE_DB_SchemeObject): Boolean;
    function    GetSystemSchemeByName         (const schemename:TFRE_DB_NameType; var scheme: IFRE_DB_SchemeObject): Boolean;
    function    GetSystemScheme               (const schemename:TClass; var scheme: IFRE_DB_SchemeObject): Boolean;
    function    GetSystemEnum                 (const name:TFRE_DB_NameType ; out enum : IFRE_DB_Enum):boolean;
    function    GetSystemClientFieldValidator (const name:TFRE_DB_NameType ; out clf  : IFRE_DB_ClientFieldValidator):boolean;

    function    GetClassesDerivedFrom        (const SchemeClass : ShortString)                        : TFRE_DB_ObjectClassExArray;
    function    GetSchemesDerivedFrom        (const SchemeClass : ShortString)                        : IFRE_DB_SCHEMEOBJECTArray;
    procedure   ForAllSchemes                (const iterator:TFRE_DB_Scheme_Iterator)                 ;
    procedure   ForAllEnums                  (const iterator:TFRE_DB_Enum_Iterator)                   ;
    procedure   ForAllClientFieldValidators  (const iterator:TFRE_DB_ClientFieldValidator_Iterator)   ;
    procedure   ForAllApps                   (const iterator:TFRE_DB_Apps_Iterator)                   ;


    procedure   LogDebugIf             (const category:TFRE_DB_LOGCATEGORY;const logcallback : TFRE_SimpleCallbackNested);
    procedure   LogInfoIf              (const category:TFRE_DB_LOGCATEGORY;const logcallback : TFRE_SimpleCallbackNested);
    procedure   LogWarningIf           (const category:TFRE_DB_LOGCATEGORY;const logcallback : TFRE_SimpleCallbackNested);
    procedure   LogErrorIf             (const category:TFRE_DB_LOGCATEGORY;const logcallback : TFRE_SimpleCallbackNested);
    procedure   LogNoticeIf            (const category:TFRE_DB_LOGCATEGORY;const logcallback : TFRE_SimpleCallbackNested);
    procedure   LogEmergencyIf         (const category:TFRE_DB_LOGCATEGORY;const logcallback : TFRE_SimpleCallbackNested);

    procedure   LogDebug               (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogInfo                (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogWarning             (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogError               (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogNotice              (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogEmergency           (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogDebug               (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogInfo                (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogWarning             (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogError               (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogNotice              (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogEmergency           (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);

    property    NetServer              : IFRE_DB_NetServer read NetServ write FNetServer;
    procedure   ClearGUID              (var uid:TFRE_DB_GUID);
    function    Get_A_Guid             : TFRE_DB_GUID;
    function    Get_A_Guid_HEX         : Ansistring;
  end;

  OFRE_DB_ConnectionArr  = specialize OGFOS_Array<TFRE_DB_CONNECTION>;

  operator = (a,b:TFRE_DB_GUID) c:boolean;

  procedure Init4Server;
  procedure InitMinimal(const nosys:boolean=false);

var
  GFRE_DB                   : TFRE_DB;
  GFRE_DB_PS_LAYER          : IFRE_DB_PERSISTANCE_LAYER;
  GDISABLE_SYNC             : boolean;
  GDBPS_TRANS_WRITE_THROUGH : boolean;
  GDBPS_SKIP_STARTUP_CHECKS : boolean;

  procedure GFRE_DB_Init_Check;

  function     DBObjIsNull           (const obj   : PFRE_DB_Object) : Boolean;
  function     ObjecTFRE_DB_GUIDCompare     (const o1,o2 : PFRE_DB_Object):boolean;

implementation

  procedure ForAllObjectsDo(const object_array: TFRE_DB_ObjectArray; const iterator: TFRE_DB_Obj_Iterator);
  var
    i: Integer;
  begin
    for i:=0 to high(object_array) do begin
       iterator(object_array[i]);
    end;
  end;

  procedure ForAllGuidsDo(const guid_array: TFRE_DB_GUIDArray; const iterator: TFRE_DB_Guid_Iterator);
  var
    i: Integer;
  begin
    for i:=0 to high(guid_array) do begin
       iterator(guid_array[i]);
    end;
  end;

  //function RB_Sort_CompareEx(const a, b: TFRE_DB_Object ; const DP:Pointer): NativeInt;
  //begin
  //  result := TFRE_DB_DERIVED_COLLECTION(DP)._CompareObjects(a,b);
  //end;

  operator=(a, b: TFRE_DB_GUID)c: boolean;
  begin
    c := FREDB_Guids_Same(a,b);
  end;

  function _CovertRaw2Displayable(const s:TFRE_DB_String):TFRE_DB_String;
  var i: Integer;

    function _ToChar(const c:char):char;
    begin
     if (ord(c)>=$20) and (ord(c)<=$7E) then result:=c else result:='.';
    end;

  begin
    for i:=1 to Length(s) do begin
      result:=result+_ToChar(s[i]);
    end;
  end;

{ TFRE_DB_OUTCOLLECTOR_FT }

constructor TFRE_DB_OUTCOLLECTOR_FT.Create(const format: TFRE_DB_String; const in_fieldlist: TFRE_DB_NameTypeArray; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE; const display: Boolean; const sortable: Boolean; const filterable: Boolean; const fieldSize: Integer);
begin
 FFormatString   := format;
 FInfieldList    := in_fieldlist;
 FOutFieldName   := lowercase(out_field);
 FOutFieldTitle  := output_title;
 FGuiDisplaytype := gui_display_type;
 FDisplay        := display;
 FSortable       := sortable;
 FFilterable     := filterable;
 FFilterValues   := nil;
 FFieldSize      := fieldSize;
end;

procedure TFRE_DB_OUTCOLLECTOR_FT.TransformField(const conn: IFRE_DB_CONNECTION; const input, output: IFRE_DB_Object);
var data : TFRE_DB_String;
begin
  try
    data := output.FieldPathListFormat(FInfieldList,FFormatString,'');
  except on e:exception do
    begin
      data := 'Transform Error: '+e.Message;
    end;
  end;
  output.field(FOutFieldName).Asstring := data;
end;

{ TFRE_DB_REFERERENCE_QRY_FT }

function TFRE_DB_REFERERENCE_QRY_FT.RefLinkSpec: TFRE_DB_NameTypeRLArray;
begin
  Result:=FRefFieldChain;
end;

constructor TFRE_DB_REFERERENCE_QRY_FT.Create(const func: IFRE_DB_QUERY_SELECTOR_FUNCTION; const ref_field_chain: TFRE_DB_NameTypeRLArray; const output_fields, output_titles, langres: TFRE_DB_StringArray; const gui_display_type: TFRE_DB_DISPLAY_TYPE_Array; const display, sortable, filterable, hideinoutput: TFRE_DB_BoolArray; const fieldSize: TFRE_DB_Int32Array);
begin
 FRefFieldChain  := ref_field_chain;
 FRQ_func        := func;
 FRQO_Fields     := output_fields;
 FRQO_Titles     := output_titles;
 FRQO_Langres    := langres;
 FRQO_Types      := gui_display_type;
 FRQO_Display    := display;
 FRQO_Sortable   := sortable;
 FRQO_Filterable := filterable;
 FRQO_Hide       := FRQO_Hide;
 FRQO_FieldSize  := fieldSize;
end;

procedure TFRE_DB_REFERERENCE_QRY_FT.AddToViewCollection(const vcd: TFRE_DB_VIEW_LIST_LAYOUT_DESC);
var i : integer;
begin
  for i := 0 to high(FRQO_Fields) do
    if FRQO_Display[i] then
      vcd.AddDataElement.Describe(FRQO_Fields[i],FRQO_Titles[i],FRQO_Types[i],FRQO_Sortable[i],FRQO_Filterable[i],FRQO_FieldSize[i],FRQO_Display[i]);
end;

procedure TFRE_DB_REFERERENCE_QRY_FT.TransformField(const conn: IFRE_DB_CONNECTION; const input, output: IFRE_DB_Object);
var objo      : IFRE_DB_ObjectArray;
    obj       : IFRE_DB_Object;
    ref_uid   : TFRE_DB_GUID;
    expanded  : TFRE_DB_GUIDArray;
    res       : TFRE_DB_Errortype;
begin
  conn.ExpandReferences(TFRE_DB_GUIDArray.create(input.UID),FRefFieldChain,expanded);
  try
    if Length(expanded)>0 then
      res := (conn.Implementor_HC as TFRE_DB_CONNECTION).BulkFetchNoRightCheck(expanded,objo);
    FRQ_func(objo,input,output,FRQO_Langres);
  finally
    For obj in objo do
      obj.Finalize;
  end;
end;

{ TFRE_DB_USER_RIGHT_TOKEN }

function TFRE_DB_USER_RIGHT_TOKEN.Implementor: TObject;
begin
  result := self;
end;

function TFRE_DB_USER_RIGHT_TOKEN.IsCurrentUserSystemAdmin: boolean;
begin
  result := FIsSysAdmin;
end;

function TFRE_DB_USER_RIGHT_TOKEN.GetMyDomainID_String: TFRE_DB_GUID_String;
begin
  result := FMyDomainID_GS;
end;

function TFRE_DB_USER_RIGHT_TOKEN.GetSystemDomainID_String: TFRE_DB_GUID_String;
begin
  result := FSysDomainID_GS;
end;

function TFRE_DB_USER_RIGHT_TOKEN.GetDomainID(const domainname: TFRE_DB_NameType): TFRE_DB_GUID;
var dn : TFRE_DB_NameType;
    i  : NativeInt;
begin
  dn  := UpperCase(domainname);
  for i:=0 to high(FAllDomainNames) do
    if FAllDomainNames[i]=dn then
      exit(FAllDomainsUids[i]);
  raise EFRE_Exception.Create('domainname not found');
end;

function TFRE_DB_USER_RIGHT_TOKEN.GetDomainNameByUid(const domainid: TFRE_DB_GUID): TFRE_DB_NameType;
var i  : NativeInt;
begin
  for i:=0 to high(FAllDomainsUids) do
    if FAllDomainsUids[i]=domainid then
      exit(FAllDomainNames[i]);
  raise EFRE_Exception.Create('domainid not found');
end;

function TFRE_DB_USER_RIGHT_TOKEN._GetStdRightName(const std_right: TFRE_DB_STANDARD_RIGHT; const rclassname: ShortString; const domainguid: TFRE_DB_GUID): TFRE_DB_String;
begin
  result:=TFRE_DB_Base.GetClassRightNameSR(rclassname,std_right)+'@'+uppercase(FREDB_G2H(domainguid));
end;

//function TFRE_DB_USER_RIGHT_TOKEN._DomainIDasString(const name: TFRE_DB_NameType): TFRE_DB_NameType;
//begin
//  result := uppercase(FREDB_G2H(GetDomainID(name))); // DomainIDasString has to be uppercase!
//end;

function TFRE_DB_USER_RIGHT_TOKEN._GetStdRightName(const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass; const domainguid: TFRE_DB_GUID): TFRE_DB_String;
begin
  Result := _GetStdRightName(std_right,classtyp)+'@'+uppercase(FREDB_G2H(domainguid));
end;

function TFRE_DB_USER_RIGHT_TOKEN._GetStdRightName(const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass): TFRE_DB_String;
begin
  case std_right of
    sr_STORE  : result:=TFRE_DB_BaseClass(classtyp).GetClassRightNameStore;
    sr_UPDATE : result:=TFRE_DB_BaseClass(classtyp).GetClassRightNameUpdate;
    sr_DELETE : result:=TFRE_DB_BaseClass(classtyp).GetClassRightNameDelete;
    sr_FETCH  : result:=TFRE_DB_BaseClass(classtyp).GetClassRightNameFetch;
  else
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'rightname for standard right is not defined!');
  end;
  if Result='' then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'i found a fool');
end;

function TFRE_DB_USER_RIGHT_TOKEN.FetchAllDomainUids: TFRE_DB_GUIDArray;
begin
  Result := FAllDomainsUids;
end;

function TFRE_DB_USER_RIGHT_TOKEN.CheckClassRight4MyDomain(const right_name: TFRE_DB_String; const classtyp: TClass): boolean;
begin
  result := IsCurrentUserSystemAdmin;
  if result then
    exit;
  result := FREDB_StringInArray(TFRE_DB_BaseClass(classtyp).GetClassRightName(right_name)+'@'+GetMyDomainID_String,FConnectionRights); // GetMyDomainID_String has to be uppercase
  if result then
    exit;
  result := FREDB_StringInArray(TFRE_DB_BaseClass(classtyp).GetClassRightName(right_name)+'@'+GetSystemDomainID_String,FConnectionRights); // check in system domain
end;

function TFRE_DB_USER_RIGHT_TOKEN.CheckClassRight4AnyDomain(const right_name: TFRE_DB_String; const classtyp: TClass): boolean;
begin
  result := IsCurrentUserSystemAdmin;
  if result then
    exit;
  result := FREDB_PrefixStringInArray(TFRE_DB_BaseClass(classtyp).GetClassRightName(right_name),FConnectionRights);
end;

function TFRE_DB_USER_RIGHT_TOKEN.CheckClassRight4Domain(const right_name: TFRE_DB_String; const classtyp: TClass; const domainKey: TFRE_DB_String): boolean;
begin
  result := CheckClassRight4DomainId(right_name,classtyp,GetDomainID(domainKey));
  //result := IsCurrentUserSystemAdmin;
  //if result then
  //  exit;
  //result := FREDB_StringInArray((TFRE_DB_BaseClass(classtyp).GetClassRightName(right_name)+'@'+_DomainIDasString(domainkey)),FConnectionRights); // DomainIDasString has to be uppercase!
  //if result then
  //  exit;
  //result := FREDB_StringInArray((TFRE_DB_BaseClass(classtyp).GetClassRightName(right_name)+'@'+GetSystemDomainID_String),FConnectionRights); // check in system domain
end;

function TFRE_DB_USER_RIGHT_TOKEN.GetDomainsForRight(const right_name: TFRE_DB_String): TFRE_DB_GUIDArray;
begin
  if IsCurrentUserSystemAdmin then
    result := FetchAllDomainUids
  else
    result := FREDB_ExtractUidsfromRightArray(FConnectionRights,right_name);
end;

function TFRE_DB_USER_RIGHT_TOKEN.GetDomainsForClassRight(const right_name: TFRE_DB_String; const classtyp: TClass): TFRE_DB_GUIDArray;
begin
  result := GetDomainsForRight(TFRE_DB_BaseClass(classtyp).GetClassRightName(right_name));
end;

function TFRE_DB_USER_RIGHT_TOKEN.CheckClassRight4MyDomain(const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass): boolean;
begin
  result := IsCurrentUserSystemAdmin;
  if result then
    exit;
  result := FREDB_StringInArray(_GetStdRightName(std_right,classtyp,FMyDomainID),FConnectionRights);
  if result then
    exit;
  result := FREDB_StringInArray(_GetStdRightName(std_right,classtyp,FSysDomainUID),FConnectionRights);  // check in system domain
end;

function TFRE_DB_USER_RIGHT_TOKEN.CheckClassRight4AnyDomain(const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass): boolean;
begin
  result := IsCurrentUserSystemAdmin;
  if result then
    exit;
  result := FREDB_PrefixStringInArray(_GetStdRightName(std_right,classtyp),FConnectionRights);
end;

function TFRE_DB_USER_RIGHT_TOKEN.CheckClassRight4Domain(const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass; const domainKey: TFRE_DB_String): boolean;
begin
  Result:=CheckClassRight4DomainId(std_right,classtyp,GetDomainID(domainKey));
end;

function TFRE_DB_USER_RIGHT_TOKEN.CheckClassRight4DomainId(const right_name: TFRE_DB_String; const classtyp: TClass; const domain: TFRE_DB_GUID): boolean;
begin
  result := IsCurrentUserSystemAdmin;
  if result then
    exit;
  result := FREDB_StringInArray((TFRE_DB_BaseClass(classtyp).GetClassRightName(right_name)+'@'+uppercase(FREDB_G2H(domain))),FConnectionRights); // DomainIDasString has to be uppercase!
end;

function TFRE_DB_USER_RIGHT_TOKEN.CheckClassRight4DomainId(const right_name: TFRE_DB_String; const rclassname: ShortString; const domain: TFRE_DB_GUID): boolean;
begin
 result := IsCurrentUserSystemAdmin;
 if result then
   exit;
 result := FREDB_StringInArray((TFRE_DB_Base.GetClassRightName(rclassname,right_name)+'@'+uppercase(FREDB_G2H(domain))),FConnectionRights); // DomainIDasString has to be uppercase!
end;

function TFRE_DB_USER_RIGHT_TOKEN.CheckClassRight4DomainId(const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass; const domain: TFRE_DB_GUID): boolean;
begin
  result := IsCurrentUserSystemAdmin;
  if result then
    exit;
  result := FREDB_StringInArray(_GetStdRightName(std_right,classtyp,domain),FConnectionRights);
end;

function TFRE_DB_USER_RIGHT_TOKEN.CheckClassRight4DomainId(const std_right: TFRE_DB_STANDARD_RIGHT; const rclassname: ShortString; const domain: TFRE_DB_GUID): boolean;
begin
  result := IsCurrentUserSystemAdmin;
  if result then
    exit;
  result := (domain<>CFRE_DB_NullGUID) and FREDB_StringInArray(_GetStdRightName(std_right,rclassname,domain),FConnectionRights);
end;

constructor TFRE_DB_USER_RIGHT_TOKEN.Create(const user_uid: TFRE_DB_GUID; const login_part, firstname, lastname, desc: TFRE_DB_String; const group_ids: TFRE_DB_GUIDArray; const rights: TFRE_DB_StringArray; is_sys_admin: boolean; sysdom_id, user_domid: TFRE_DB_GUID; domainids: TFRE_DB_GUIDArray; domain_names: TFRE_DB_NameTypeArray);
var sl    : TStringList;
    i     : NativeInt;
    hsh   : Cardinal;
    scr   : string;
begin
  scr := login_part;
  FUserLoginPart    := login_part;
  FUserUID          := user_uid;
  FUsergroupIDs     := group_ids;
  FConnectionRights := rights;
  FIsSysAdmin       := is_sys_admin;
  FSysDomainUID     := sysdom_id;
  FSysDomainID_GS   := uppercase(FREDB_G2H(FSysDomainUID));
  FMyDomainID       := user_domid;
  FMyDomainID_GS    := uppercase(FREDB_G2H(FMyDomainID));
  FAllDomainNames   := domain_names;
  FAllDomainsUids   := domainids;
  FDomainLoginKey   := TFRE_DB_USER.GetDomainLoginKey(FUserLoginPart,FMyDomainID);
  FLoginAtDomain    := FUserLoginPart+'@'+GetDomainNameByUid(user_domid);
  FUserFirstName    := firstname;
  FUserLastName     := lastname;
  FUserDescName     := desc;
  hsh               := GFRE_BT.HashFast32(@FDomainLoginKey[1],Length(FDomainLoginKey),0);
  for i:=0 to high(FAllDomainNames) do
    FAllDomainNames[i]:=UpperCase(FAllDomainNames[i]);
  sl := TStringList.Create;
  try
    sl.Duplicates:=dupIgnore;
    for i := 0 to high(rights) do
      sl.add(rights[i]);
    sl.Sort;
    SetLength(FConnectionRights,sl.Count);
    for i:=0 to sl.Count-1 do begin
      FConnectionRights[i] := sl[i];
      hsh                  := GFRE_BT.HashFast32(@FConnectionRights[i][1],Length(FConnectionRights[i]),hsh);
    end;
    sl.Clear;
    for i := 0 to high(FAllDomainNames) do
      sl.Add(FAllDomainNames[i]);
    sl.Sort;
    for i:=0 to sl.Count-1 do begin
      hsh                  := GFRE_BT.HashFast32(@sl[i][1],Length(sl[i]),hsh);
    end;
  finally
    sl.free;
  end;
  FUniqueToken           := GFRE_BT.Mem2HexStr(@hsh,4);
end;

destructor TFRE_DB_USER_RIGHT_TOKEN.Destroy;
begin
  //FConnectedUser.Finalize;
  inherited;
end;

procedure TFRE_DB_USER_RIGHT_TOKEN.Finalize;
begin
  free;
end;

function TFRE_DB_USER_RIGHT_TOKEN.GetUserGroupIDS: TFRE_DB_GUIDArray;
begin
  result := FUsergroupIDs;
end;

function TFRE_DB_USER_RIGHT_TOKEN.GetUserUID: TFRE_DB_GUID;
begin
  result := FUserUID;
end;

function TFRE_DB_USER_RIGHT_TOKEN.GetUserUIDP: PFRE_DB_GUID;
begin
  result := @FUserUID;
end;

function TFRE_DB_USER_RIGHT_TOKEN.GetDomainLoginKey: TFRE_DB_String;
begin
  result := FDomainLoginKey;
end;

function TFRE_DB_USER_RIGHT_TOKEN.GetFullUserLogin: TFRE_DB_String;
begin
  result := FLoginAtDomain;
end;

procedure TFRE_DB_USER_RIGHT_TOKEN.GetUserDetails(out fulluserlogin, firstname, lastname, description: TFRE_DB_String);
begin
  fulluserlogin := lowercase(FLoginAtDomain);
  firstname     := FUserFirstName;
  lastname      := FUserLastName;
  description   := FUserDescName;
end;

function TFRE_DB_USER_RIGHT_TOKEN.CheckStdRightAndCondFinalize(const dbi: IFRE_DB_Object; const sr: TFRE_DB_STANDARD_RIGHT; const without_right_check: boolean; const cond_finalize: boolean): TFRE_DB_Errortype;
var classt : TClass;
    dbo    : TFRE_DB_Object;
begin
    dbo    := dbi.Implementor as TFRE_DB_Object;
    classt := dbo.Implementor_HC.ClassType;
    if not
     ((without_right_check
       or (classt=TFRE_DB_Object)
       or CheckClassRight4DomainId(sr,classt,dbo.DomainID))
       or CheckObjectRight(sr,dbo.UID)
      ) then
         begin
           GFRE_DB.LogInfo(dblc_APPLICATION,'Access denied for standard right [%s] of class [%s] domain [%s]',[CFRE_DB_STANDARD_RIGHT[sr],classt.ClassName,FREDB_G2H(dbo.DomainID)]); // add user info
           if (cond_finalize) and (not dbo.IsSystem) then
             dbo.Finalize;
           exit(edb_ACCESS) //raise EFRE_DB_Exception.Create(edb_ERROR,'you are not allowed to update objects in the specified domain : '+new_obj.DomainID_String);
         end
    else
      exit(edb_OK);
end;

function TFRE_DB_USER_RIGHT_TOKEN.CheckStdRightsetUIDAndClass(const obj_uid, obj_domuid: TFRE_DB_GUID; const check_classname: ShortString; const sr: TFRE_DB_STANDARD_RIGHT_SET): TFRE_DB_Errortype;
var classt : ShortString;
    res    : boolean;

    function IntCheck(const rt : TFRE_DB_STANDARD_RIGHT):boolean;
    begin
      result := ((classt='TFRE_DB_OBJECT')
                  or CheckClassRight4DomainId(rt,classt,obj_domuid)
                  or CheckObjectRight(rt,obj_uid));
    end;

begin
  if (obj_uid=CFRE_DB_NullGUID) and (obj_domuid=CFRE_DB_NullGUID) then
    exit(edb_ACCESS);
  classt := uppercase(check_classname);
  res    := true;
  result := edb_OK;
  if sr_FETCH in sr then
    begin
      res := res AND IntCheck(sr_FETCH);
      if not res then
        exit(edb_ACCESS);
    end;
  if sr_STORE in sr then
    begin
      res := res AND IntCheck(sr_STORE);
      if not res then
        exit(edb_ACCESS);
    end;
  if sr_UPDATE in sr then
    begin
      res := res AND IntCheck(sr_UPDATE);
      if not res then
        exit(edb_ACCESS);
    end;
  if sr_DELETE in sr then
    begin
      res := res AND IntCheck(sr_DELETE);
      if not res then
        exit(edb_ACCESS);
    end;
end;

function TFRE_DB_USER_RIGHT_TOKEN.CheckGenRightSetUIDAndClass(const obj_uid, obj_domuid: TFRE_DB_GUID; const check_classname: ShortString; const sr: TFRE_DB_StringArray): TFRE_DB_Errortype;
var classt : ShortString;
    res    : boolean;
    i      : integer;

    function IntCheckGen(const rt : TFRE_DB_String):boolean;
    begin
      result := ((classt='TFRE_DB_OBJECT')
                  or CheckClassRight4DomainId(rt,classt,obj_domuid)
                  or CheckObjectRight(rt,obj_uid));
    end;

begin
  if (obj_uid=CFRE_DB_NullGUID) and (obj_domuid=CFRE_DB_NullGUID) then
    exit(edb_ACCESS);
  classt := uppercase(check_classname);
  res    := true;
  result := edb_OK;
  for i:= 0 to high(sr) do
    begin
      res := res AND IntCheckGen(sr[i]);
      if not res then
        exit(edb_ACCESS);
    end;
end;

function TFRE_DB_USER_RIGHT_TOKEN.CheckStdRightsetInternalObj(const obj: TFRE_DB_Object; const sr: TFRE_DB_STANDARD_RIGHT_SET): TFRE_DB_Errortype;
begin
  result := CheckStdRightSetUIDAndClass(obj.UID,obj.DomainID,obj.SchemeClass,sr);
end;

function TFRE_DB_USER_RIGHT_TOKEN.GetDomainsForClassRight(const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass): TFRE_DB_GUIDArray;
begin
  result := GetDomainsForRight(_GetStdRightName(std_right,classtyp));
end;

function TFRE_DB_USER_RIGHT_TOKEN.GetDomainNamesForClassRight(const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass): TFRE_DB_StringArray;
var uids : TFRE_DB_GUIDArray;
       i : NativeInt;
begin
  uids := GetDomainsForClassRight(std_right,classtyp);
  SetLength(result,Length(uids));
  for i:=0 to high(Result) do
    result[i] := GetDomainNameByUid(uids[i]);
end;

function TFRE_DB_USER_RIGHT_TOKEN.CheckObjectRight(const right_name: TFRE_DB_String; const uid: TFRE_DB_GUID): boolean;
begin
  result := IsCurrentUserSystemAdmin;
  if result then
    exit;
  result := FREDB_StringInArray(TFRE_DB_Base.GetObjectRightName(right_name,uid),FConnectionRights);
end;

function TFRE_DB_USER_RIGHT_TOKEN.CheckObjectRight(const std_right: TFRE_DB_STANDARD_RIGHT; const uid: TFRE_DB_GUID): boolean;
begin
  result := IsCurrentUserSystemAdmin;
  if result then
    exit;
  result :=  (uid<>CFRE_DB_NullGUID) and FREDB_StringInArray(TFRE_DB_Base.GetStdObjectRightName(std_right,uid),FConnectionRights);
end;

function TFRE_DB_USER_RIGHT_TOKEN.DumpUserRights: TFRE_DB_String;
var DomainUids  : TFRE_DB_GUIDArray;
    Domainnames : TFRE_DB_StringArray;
            i,j : NativeInt;
begin
  Domainuids := FREDB_ExtractUidsfromRightArray(FConnectionRights,'');
  SetLength(Domainnames,Length(DomainUids));
  for i := 0 to high(DomainUids) do
    Domainnames[i] := GetDomainNameByUid(DomainUids[i]);
  for i:=0 to high(FConnectionRights) do
    if pos('@',FConnectionRights[i])>0 then
      begin
        Result:=result+FConnectionRights[i]+' (';
        for j:=0 to high(DomainUids) do
          if pos(uppercase(FREDB_G2H(DomainUids[j])),FConnectionRights[i])>0 then
            begin
              result:=result+'@'+Domainnames[j]+')'+LineEnding;
              break;
            end;
      end
    else
     result := result+FConnectionRights[i]+LineEnding;
end;

//function TFRE_DB_USER_RIGHT_TOKEN.AuthenticatedUsername: string;
//begin
//  Result := FAuthName;
//end;

function TFRE_DB_USER_RIGHT_TOKEN.GetMyDomainID: TFRE_DB_GUID;
begin
  result := FMyDomainID;
end;

function TFRE_DB_USER_RIGHT_TOKEN.GetSysDomainID: TFRE_DB_GUID;
begin
  result := FSysDomainUID;
end;

function TFRE_DB_USER_RIGHT_TOKEN.GetUniqueTokenKey: TFRE_DB_NameType;
begin
  result := FUniqueToken;
end;

function TFRE_DB_USER_RIGHT_TOKEN.CloneToNewUserToken: IFRE_DB_USER_RIGHT_TOKEN;
begin
  result := clone;
end;

function TFRE_DB_USER_RIGHT_TOKEN.Clone: TFRE_DB_USER_RIGHT_TOKEN;
begin
  result := TFRE_DB_USER_RIGHT_TOKEN.Create(FUserUID,FUserLoginPart,FUserFirstName,FUserLastName,FUserDescName,FUsergroupIDs,FConnectionRights,FIsSysAdmin,FSysDomainUID,FMyDomainID,FAllDomainsUids,FAllDomainNames);
  if Result.FUniqueToken<>FUniqueToken then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'unique user token clone / failure / internal logic');
end;

{ TFRE_DB_OBJECTLIST }

function TFRE_DB_OBJECTLIST.GetAsArray(const idx : NativeInt): TFRE_DB_Object;
begin
  _Checkindex(idx);
  result := Field(inttostr(idx)).AsObject;
end;

procedure TFRE_DB_OBJECTLIST.SetAsArray(const idx : NativeInt; AValue: TFRE_DB_Object);
begin
  _Checkindex(idx);
  Field(inttostr(idx)).AsObject := AValue;
end;

procedure TFRE_DB_OBJECTLIST._SetCount(const val: NativeInt);
begin
  Field('C').AsInt64:=val;
end;

function TFRE_DB_OBJECTLIST._Checkindex(const idx: Nativeint): NativeInt;
begin
  result := GetCount;
  if (idx<0) or (idx>=result) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'index out of range %d is not between 0 and %d',[idx,0,result-1]);
end;

function TFRE_DB_OBJECTLIST.GetCount: NativeInt;
var fld : TFRE_DB_FIELD;
begin
  if FieldOnlyExisting('C',fld) then
    result := fld.AsInt64
  else
    result := 0;
end;

procedure TFRE_DB_OBJECTLIST.ClearArray(const free_objs: boolean);
var obj : TFRE_DB_Object;
      i : NativeInt;
      c : NativeInt;
begin
  c := GetCount;
  for i := 0 to c-1 do
    begin
      if free_objs then
        begin
          obj := Field(inttostr(i)).CheckOutObject;
          obj.Finalize;
        end
      else
        Field(inttostr(i)).Clear(true);
    end;
  _SetCount(0);
end;


procedure TFRE_DB_OBJECTLIST.AddObject(const obj: TFRE_DB_Object);
var oldcount : NativeInt;
begin
  oldcount := GetCount;
  Field(inttostr(oldcount)).AsObject := obj;
  _SetCount(oldcount+1);
end;

procedure TFRE_DB_OBJECTLIST.RemoveObject(const idx: Nativeint);
var obj : TFRE_DB_Object;
begin
  obj := CheckoutObject(idx);
  obj.Free;
end;


function TFRE_DB_OBJECTLIST.CheckoutObject(const idx: Nativeint): TFRE_DB_Object;
var oldcount : NativeInt;
           i : NativeInt;
begin
  oldcount := _CheckIndex(idx);
  result := Field(inttostr(idx)).CheckOutObject;
  for i :=  (oldcount-1) downto (idx+1) do
    begin
      Field(inttostr(i-1)).AsObject := Field(inttostr(i)).CheckOutObject;
    end;
  _SetCount(oldcount-1);
end;

function TFRE_DB_OBJECTLIST.GetAsObjectArray: TFRE_DB_ObjectArray;
var i : NativeInt;
begin
  SetLength(result,GetCount);
  for i:= 0 to high(Result) do
    result[i] := Field(inttostr(i)).AsObject;
end;

function TFRE_DB_OBJECTLIST.CheckOutAsArray: TFRE_DB_ObjectArray;
var i:integer;
begin
  result := GetAsObjectArray;
  for i:=0 to high(Result) do
    result[i].FParentDBO := nil;
  ClearArray(false);
end;

procedure TFRE_DB_OBJECTLIST.SetAsObjectArray(const arr: TFRE_DB_ObjectArray);
var i : NativeInt;
begin
  ClearArray;
  for i:=0 to high(arr) do
    Field(inttostr(i)).AsObject := arr[i];
  _SetCount(Length(arr));
end;

{ TFRE_DB_REFERERENCE_CHAIN_FT }

function TFRE_DB_REFERERENCE_CHAIN_FT.RefLinkSpec:TFRE_DB_NameTypeRLArray;
begin
  Result:=FRefFieldChain;
end;

constructor TFRE_DB_REFERERENCE_CHAIN_FT.Create(const ref_field_chain: TFRE_DB_NameTypeRLArray; const target_field: TFRE_DB_String; const output_field: TFRE_DB_String; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE; const display: Boolean; const sortable: Boolean; const filterable: Boolean; const fieldSize: Integer; const iconID:String; const filterValues: TFRE_DB_StringArray=nil);
begin
  FRefFieldChain  := ref_field_chain;
  FInFieldName    := target_field;
  FOutFieldName   := lowercase(output_field);
  FOutFieldTitle  := output_title;
  FGuiDisplaytype := gui_display_type;
  FDisplay        := display;
  FSortable       := sortable;
  FFilterable     := filterable;
  FFilterValues   := filterValues;
  FFieldSize      := fieldSize;
  FIconIdField    := iconID;
  if FOutFieldName='' then
    FOutFieldName := lowercase(FInFieldName);
end;

procedure TFRE_DB_REFERERENCE_CHAIN_FT.TransformField(const conn: IFRE_DB_CONNECTION; const input, output: IFRE_DB_Object);
var objo      : IFRE_DB_Object;
    ref_uid   : TFRE_DB_GUID;
    i         : integer;
    s         : string;
    fld       : TFRE_DB_FIELD;
    expanded  : TFRE_DB_GUIDArray;
    res       : TFRE_DB_Errortype;
begin
  conn.ExpandReferences(TFRE_DB_GUIDArray.create(input.UID),FRefFieldChain,expanded);
  if Length(expanded)=0 then
    begin
      output.field(uppercase(FOutFieldName)).asstring := ''; //unresoved links should return empty field for grid
      exit;
    end;
  if Length(expanded)>1 then
    begin
      output.field(uppercase(FOutFieldName)).asstring := '?*AMBIGUOUS LINK*';
      exit;
    end;
  res := conn.Fetch(expanded[0],objo);
  if res<>edb_OK then
    begin
      output.field(uppercase(FOutFieldName)).asstring := '?*'+CFRE_DB_Errortype[res]+'*';
      exit;
    end
  else
    begin
      if objo.FieldExists(FInFieldName) then begin
        output.field(uppercase(FOutFieldName)).CloneFromField(objo.Field(FInFieldName));
      end else begin
        output.field(uppercase(FOutFieldName)).asstring := '?*TARGETFIELD NOT FOUND*';
      end;
    end;
  if assigned(objo) then
    objo.Finalize;
end;

{ TFRE_DB_TEXT_FT }

constructor TFRE_DB_TEXT_FT.Create(const fieldname: TFRE_DB_String; const which_text: TFRE_DB_TEXT_SUBTYPE; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE; const display,sortable,filterable: Boolean; const fieldSize: Integer);
begin
 FInFieldName     := fieldname;
 FOutFieldName    := lowercase(out_field);
 FOutFieldTitle   := output_title;
 FGuiDisplaytype  := gui_display_type;
 FDisplay         := display;
 FSortable        := sortable;
 FFilterable      := filterable;
 FFilterValues    := nil;
 FFieldSize       := fieldSize;
 FWhichText       := which_text;
 if FOutFieldName='' then
   FOutFieldName:=lowercase(FInFieldName);
end;

procedure TFRE_DB_TEXT_FT.TransformField(const conn: IFRE_DB_CONNECTION; const input, output: IFRE_DB_Object);
var text      : TFRE_DB_TEXT;
    out_text  : TFRE_DB_String;
begin
  try
    text := (input.Field(FInFieldName).AsObject).Implementor as TFRE_DB_TEXT;
    case FWhichText of
      tst_Short: out_text := text.Getshort;
      tst_Long:  out_text := text.GetLong;
      tst_Hint:  out_text := text.GetHint;
      tst_Key:   out_text := text.GetTKey;
    end;
    output.field(uppercase(FOutFieldName)).asstring := out_text;
  except
    output.field(uppercase(FOutFieldName)).asstring := 'ERROR';
  end;
end;

{ TFRE_DB_FIELD_TRANSFORM }

function TFRE_DB_FIELD_TRANSFORM.RefLinkSpec: TFRE_DB_NameTypeRLArray;
begin
  result := nil;
end;

procedure TFRE_DB_FIELD_TRANSFORM.AddToViewCollection(const vcd: TFRE_DB_VIEW_LIST_LAYOUT_DESC);
begin
  if FDisplay then
    vcd.AddDataElement.Describe(FOutFieldName,FOutFieldTitle,FGuiDisplaytype,FSortable,FFilterable,FFieldSize,FDisplay,false,false,FIconIdField,FOpenIconIDField,FFilterValues);
end;

procedure TFRE_DB_FIELD_TRANSFORM.TransformField(const conn: IFRE_DB_CONNECTION; const input, output: IFRE_DB_Object);
begin
  raise EFRE_DB_Exception.Create(edb_ERROR,'you need to implement/override a Transformfieldfunction for class '+ClassName);
end;

{ TFRE_DB_COLLECTOR_FT }

constructor TFRE_DB_COLLECTOR_FT.Create(const format: TFRE_DB_String; const in_fieldlist: TFRE_DB_NameTypeArray; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE; const display,sortable,filterable: Boolean; const fieldSize: Integer);
begin
  FFormatString   := format;
  FInfieldList    := in_fieldlist;
  FOutFieldName   := lowercase(out_field);
  FOutFieldTitle  := output_title;
  FGuiDisplaytype := gui_display_type;
  FDisplay        := display;
  FSortable       := sortable;
  FFilterable     := filterable;
  FFilterValues   := nil;
  FFieldSize      := fieldSize;
end;

procedure TFRE_DB_COLLECTOR_FT.TransformField(const conn: IFRE_DB_CONNECTION; const input, output: IFRE_DB_Object);
var data : TFRE_DB_String;
begin
  try
    data := input.FieldPathListFormat(FInfieldList,FFormatString,'');
  except on e:exception do
    begin
      data := 'Transform Error: '+e.Message;
    end;
  end;
  output.field(FOutFieldName).Asstring := data;
end;

{ TFRE_DB_CONST_STRING_FT }

constructor TFRE_DB_CONST_STRING_FT.Create(const out_field, value: TFRE_DB_String; const display,sortable,filterable: Boolean; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE; const fieldSize: Integer);
begin
  FOutFieldName   := LowerCase(out_field);
  FOutFieldTitle  := output_title;
  FConstValue     := value;
  FDisplay        := display;
  FSortable       := sortable;
  FFilterable     := filterable;
  FFilterValues   := nil;
  FGuiDisplaytype := gui_display_type;
  FFieldSize      := fieldSize;
end;

procedure TFRE_DB_CONST_STRING_FT.TransformField(const conn: IFRE_DB_CONNECTION; const input, output: IFRE_DB_Object);
begin
  output.field(uppercase(FOutFieldName)).AsString := FConstValue;
end;

{ TFRE_DB_PROGRESS_FT }

constructor TFRE_DB_PROGRESS_FT.Create(const valuefield: TFRE_DB_String; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const input_textfield: TFRE_DB_String; const output_textfield: TFRE_DB_String; const maxValue: Single; const display,sortable,filterable: Boolean; const fieldSize: Integer);
begin
  FInFieldName    := valuefield;
  FOutFieldName   := lowercase(out_field);
  FOutFieldTitle  := output_title;
  FInTextField    := input_textfield;
  FOutTextField   := lowercase(output_textfield);
  FMaxValue       := maxValue;
  FGuiDisplaytype := dt_number_pb;
  FFieldSize      := fieldSize;
  FDisplay        := display;
  FSortable       := sortable;
  FFilterable     := filterable;
  FFilterValues   := nil;
  if FOutFieldName = '' then
    FOutFieldName := lowercase(FInFieldName);
  if FOutTextField ='' then
    FOutTextField := lowercase(FInTextField);
end;

procedure TFRE_DB_PROGRESS_FT.AddToViewCollection(const vcd: TFRE_DB_VIEW_LIST_LAYOUT_DESC);
begin
  if FDisplay then
    vcd.AddDataElement.DescribePB(FOutFieldName,FOutFieldTitle,FOutTextField,FMaxValue,FSortable,FFilterable,FFieldSize);
end;

procedure TFRE_DB_PROGRESS_FT.TransformField(const conn: IFRE_DB_CONNECTION; const input, output: IFRE_DB_Object);
begin
  output.field(uppercase(FOutFieldName)).CloneFromField(input.Field(FInFieldName).Implementor as TFRE_DB_FIELD);
  if FOutTextField<>'' then
    begin
      if input.FieldExists(FInTextField) then
        output.field(uppercase(FOutTextField)).CloneFromField(input.Field(FInTextField).Implementor as TFRE_DB_FIELD)
      else
        output.field(uppercase(FOutTextField)).CloneFromField(output.Field(FInTextField).Implementor as TFRE_DB_FIELD);
    end;
end;

{ TFRE_DB_ONEONE_FT }

constructor TFRE_DB_ONEONE_FT.Create(const fieldname:TFRE_DB_String; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE; const display: Boolean; const sortable: Boolean; const filterable: Boolean; const fieldSize: Integer; const iconID: String; const openIconID: String; const defaultValue:String; const filterValues: TFRE_DB_StringArray);
begin
  FInFieldName     := fieldname;
  FOutFieldName    := lowercase(out_field);
  FOutFieldTitle   := output_title;
  FGuiDisplaytype  := gui_display_type;
  FDisplay         := display;
  FSortable        := sortable;
  FFilterable      := filterable;
  FFilterValues    := filterValues;
  FFieldSize       := fieldSize;
  FIconIdField     := iconID;
  FOpenIconIDField := openIconID;
  if FOutFieldName='' then
    FOutFieldName:=lowercase(FInFieldName);
  FDefaultValue    := defaultValue;
end;

procedure TFRE_DB_ONEONE_FT.TransformField(const conn: IFRE_DB_CONNECTION; const input, output: IFRE_DB_Object);
var sa          : TFRE_DB_StringArray;
    i           : nativeint;
    transbase   : IFRE_DB_Object;
    enum        : IFRE_DB_Enum;
    enumVals    : IFRE_DB_ObjectArray;
    scheme_field: IFRE_DB_FieldSchemeDefinition;
begin
  //if input.FieldExists(FInFieldName) then
    transbase := input;
  //else
  //  transbase := output;
  if transbase.FieldExists(FInFieldName) then begin
    if FDisplay and transbase.GetScheme().GetSchemeField(FInFieldName,scheme_field) and
       scheme_field.getEnum(enum) then begin

      enumVals:=enum.getEntries;
      output.field(uppercase(FOutFieldName)).AsString:=transbase.Field(FInFieldName).AsString; //FALLBACK
      for i := 0 to Length(enumVals) - 1 do begin
        if transbase.Field(FInFieldName).AsString=enumVals[i].Field('v').AsString then begin
          output.field(uppercase(FOutFieldName)).AsString:=conn.FetchTranslateableTextShort(enumVals[i].Field('c').AsString);
          break;
        end;
      end;
    end else begin
      output.field(uppercase(FOutFieldName)).CloneFromField(transbase.Field(FInFieldName).Implementor as TFRE_DB_FIELD);
    end;
  end else begin
    if FDefaultValue<>'' then begin
      output.field(uppercase(FOutFieldName)).AsString:=FDefaultValue;
    end;
  end;
  case FGuiDisplaytype of
    dt_string: ;
    dt_description: ;
    dt_date: ;
    dt_number: ;
    dt_number_pb: ;
    dt_currency: ;
    dt_icon:
      begin
        SetLength(sa,0);
        FREDB_SeperateString(output.field(uppercase(FOutFieldName)).asstring,',',sa);
        for i:=0 to high(sa) do
          sa[i] := FREDB_getThemedResource(sa[i]);
        output.field(uppercase(FOutFieldName)).asstring := FREDB_CombineString(sa,',');
      end;
    dt_boolean: ;
  end;
end;

{ TFRE_DB_MULTIONE_FT }

constructor TFRE_DB_MULTIONE_FT.Create(const in_fieldlist: TFRE_DB_NameTypeArray; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE; const display: Boolean; const sortable: Boolean; const filterable: Boolean; const fieldSize: Integer; const iconID: String; const openIconID: String; const defaultValue: String);
begin
  FInfieldList     := in_fieldlist;
  FOutFieldName    := lowercase(out_field);
  FOutFieldTitle   := output_title;
  FGuiDisplaytype  := gui_display_type;
  FDisplay         := display;
  FSortable        := sortable;
  FFilterable      := filterable;
  FFilterValues    := nil;
  FFieldSize       := fieldSize;
  FIconIdField     := iconID;
  FOpenIconIDField := openIconID;
  FDefaultValue    := defaultValue;
end;

procedure TFRE_DB_MULTIONE_FT.TransformField(const conn: IFRE_DB_CONNECTION; const input, output: IFRE_DB_Object);
var sa        : TFRE_DB_StringArray;
    i         : nativeint;
    transbase : IFRE_DB_Object;
begin
  transbase := input;

  output.field(uppercase(FOutFieldName)).AsString:=FDefaultValue;
  for i := 0 to High(FInfieldList) do begin
    if transbase.FieldExists(FInfieldList[i]) then begin
      output.field(uppercase(FOutFieldName)).CloneFromField(transbase.Field(FInfieldList[i]).Implementor as TFRE_DB_FIELD);
      break;
    end;
  end;

  case FGuiDisplaytype of
    dt_string: ;
    dt_date: ;
    dt_number: ;
    dt_number_pb: ;
    dt_currency: ;
    dt_icon:
      begin
        SetLength(sa,0);
        FREDB_SeperateString(output.field(uppercase(FOutFieldName)).asstring,',',sa);
        for i:=0 to high(sa) do
          sa[i] := FREDB_getThemedResource(sa[i]);
        output.field(uppercase(FOutFieldName)).asstring := FREDB_CombineString(sa,',');
      end;
    dt_boolean: ;
  end;
end;





{ TFRE_DB_DOMAIN }

function TFRE_DB_DOMAIN.GetIsInternal: Boolean;
begin
  Result:=Field('internal').AsBoolean;
end;

procedure TFRE_DB_DOMAIN.SetIsInternal(AValue: Boolean);
begin
  Field('internal').AsBoolean:=AValue;
end;

function TFRE_DB_DOMAIN.GetSuspended: boolean;
var fld : TFRE_DB_FIELD;
begin
  if not FieldOnlyExisting('suspended',fld) then
    exit(false);
  result := fld.AsBoolean;
end;

procedure TFRE_DB_DOMAIN.SetSuspended(AValue: boolean);
begin
  Field('suspended').AsBoolean:=AValue;
end;

procedure TFRE_DB_DOMAIN._calcDisplayName(const calc: IFRE_DB_CALCFIELD_SETTER);
var
  dname: String;
begin
  dname:=Field('objname').AsString;
  if FieldPathExists('desc.txt') and (FieldPath('desc.txt').AsString<>'') then begin
    dname:=dname+' ('+FieldPath('desc.txt').AsString+')';
  end;
  calc.SetAsString(dname);
end;

class procedure TFRE_DB_DOMAIN.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';

    conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_DOMAIN_scheme_group','Domain'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_DOMAIN_scheme_name','Name'));
  end;

end;

class procedure TFRE_DB_DOMAIN.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var input_group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.Strict(false);
  Scheme.SetParentSchemeByName(TFRE_DB_NAMED_OBJECT.ClassName);
  scheme.GetSchemeField('objname').required:=true;
  Scheme.SetSysDisplayField(TFRE_DB_NameTypeArray.Create('objname','$DBTEXT:desc'),'%s - (%s)');
  scheme.AddCalcSchemeField('displayname',fdbft_String,@_calcDisplayName);

  input_group:=scheme.AddInputGroup('main').Setup('$TFRE_DB_DOMAIN_scheme_group');
  input_group.AddInput('objname','$TFRE_DB_DOMAIN_scheme_name');
  input_group.UseInputGroup('TFRE_DB_TEXT','main','desc',true,true,false);

end;

function TFRE_DB_DOMAIN.Domainname(const unique: boolean): TFRE_DB_NameType;
begin
  if unique then
    exit(uppercase(ObjectName))
  else
    exit(ObjectName)
end;

function TFRE_DB_DOMAIN.Domainkey: TFRE_DB_GUID_String;
begin
  result := uppercase(UID_String);
end;



{ TFRE_DB_CHART_TRANSFORM }

function TFRE_DB_CHART_TRANSFORM.TransformInOut(const conn: IFRE_DB_CONNECTION; const input: IFRE_DB_Object): TFRE_DB_Object;
begin
  Result := input.CloneToNewObject.Implementor as TFRE_DB_Object;
end;

destructor TFRE_DB_CHART_TRANSFORM.Destroy;
begin
  inherited Destroy;
end;


{ TFRE_DB_Enum }

function TFRE_DB_Enum._ObjectIsCodeclassOnlyAndHasNoScheme: boolean;
begin
  Result:=true;
end;

function TFRE_DB_Enum.Setup(const infoText: TFRE_DB_TEXT): TFRE_DB_Enum;
begin
  result      := self;
  Description := infoText;
end;

function TFRE_DB_Enum.SetupI(const infoText: IFRE_DB_TEXT): IFRE_DB_Enum;
begin
  Setup(infoText.Implementor as TFRE_DB_TEXT);
end;

procedure TFRE_DB_Enum.addEntry(const value: TFRE_DB_String;const cap_trans_key: TFRE_DB_String);
var obj: TFRE_DB_Object;
begin
  obj:=GFRE_DB.NewObject;
  obj._Field('v').AsString:=uppercase(value);
  obj._Field('c').AsString:=cap_trans_key;
  _Field('e').AddObject(obj);
end;

procedure TFRE_DB_Enum.addEntryI(const value: TFRE_DB_String;const cap_trans_key: TFRE_DB_String);
begin
  addEntry(value,cap_trans_key);
end;

function TFRE_DB_Enum.CheckField(const field_to_check: TFRE_DB_FIELD; const raise_exception: boolean): boolean;
var enum_object_array : TFRE_DB_ObjectArray;
    i                 : integer;
    found             : boolean;
  procedure RaiseOrExit(const msg:TFRE_DB_String;const params:array of const);
  begin
    if raise_exception then begin
      raise EFRE_DB_Exception.Create(edb_ERROR,msg,params);
    end;
    result:=false;
  end;
begin
  result := true;
  if field_to_check.FieldType<>fdbft_String then RaiseOrExit('enumfields[%s] must be TFRE_DB_String based fields not[%s]',[field_to_check.FieldName,field_to_check.FieldTypeAsString]);if not result then exit;
  field_to_check.AsString := uppercase(field_to_check.AsString);
  enum_object_array := _Field('e').AsObjectArr;
  found:=false;
  for i:=0 to high(enum_object_array) do begin
    if field_to_check.AsString=enum_object_array[i]._Field('v').AsString then begin
      found := true;
      break;
    end;
  end;
  if not found then RaiseOrExit('enumfield[%s], the enum key value[%s] was not found in the enum[%s] definition',[field_to_check.FieldName,field_to_check.AsString,ObjectName]); if not result then exit;
end;

function TFRE_DB_Enum.CheckFieldI(const field_to_check: IFRE_DB_FIELD; const raise_exception: boolean): boolean;
begin
  result := CheckField(field_to_check.Implementor as TFRE_DB_FIELD,raise_exception);
end;

function TFRE_DB_Enum.getEntries: TFRE_DB_ObjectArray;
begin
  Result := Field('e').AsObjectArr;
end;

function TFRE_DB_Enum.getEntriesI: IFRE_DB_ObjectArray;
begin
  result := FieldI('e').AsObjectArr;
end;

function TFRE_DB_Enum.getCaptions(const conn: IFRE_DB_CONNECTION): TFRE_DB_StringArray;
var
  i: Integer;
begin
  SetLength(Result,Field('e').ValueCount);
  for i := 0 to Field('e').ValueCount - 1 do begin
    Result[i]:=conn.FetchTranslateableTextShort(Field('e').AsObjectItem[i].Field('c').AsString);
  end;
end;

function TFRE_DB_Enum.getCaption(const conn: IFRE_DB_CONNECTION; const value: TFRE_DB_String): TFRE_DB_String;
var
  i     : Integer;
  uvalue: TFRE_DB_String;
begin
  Result:='';
  uvalue:=UpperCase(value);
  for i := 0 to Field('e').ValueCount - 1 do begin
    if UpperCase(Field('e').AsObjectItem[i].Field('v').AsString)=uvalue then begin
      Result:=conn.FetchTranslateableTextShort(Field('e').AsObjectItem[i].Field('c').AsString);
      exit;
    end;
  end;
end;

{ TFRE_DB_ClientFieldValidator }

function TFRE_DB_ClientFieldValidator._ObjectIsCodeclassOnlyAndHasNoScheme: boolean;
begin
  Result:=true;
end;

function TFRE_DB_ClientFieldValidator.Setup(const regExp: TFRE_DB_String; const infoText: IFRE_DB_TEXT; const help_trans_key: TFRE_DB_String; const allowedChars: TFRE_DB_String): IFRE_DB_ClientFieldValidator;
begin
  Field('regExp').AsString:=regExp;
  Field('allowedChars').AsString := allowedChars;
  Description                 := infoText.Implementor as TFRE_DB_TEXT;
  Field('helpText').AsString  := help_trans_key;
  Result:=Self;
end;

function TFRE_DB_ClientFieldValidator.getRegExp: TFRE_DB_String;
begin
  Result:=Field('regExp').AsString;
end;

function TFRE_DB_ClientFieldValidator.getInfoText: TFRE_DB_TEXT;
begin
  Result:=Description;
end;

function TFRE_DB_ClientFieldValidator.getInfoTextI: IFRE_DB_TEXT;
begin
 result := getInfoText;
end;

function TFRE_DB_ClientFieldValidator.getHelpTextKey: TFRE_DB_String;
begin
 Result:=Field('helpText').AsString;
end;

function TFRE_DB_ClientFieldValidator.getAllowedChars: TFRE_DB_String;
begin
  Result:=Field('allowedChars').AsString;
end;

function TFRE_DB_ClientFieldValidator.CheckField(const field_to_check: TFRE_DB_FIELD; const raise_exception: boolean): boolean;
begin
  result := True;
end;

{ TFRE_DB_InputGroupSchemeDefinition }

function TFRE_DB_InputGroupSchemeDefinition.Setup(const cap_key: TFRE_DB_String): TFRE_DB_InputGroupSchemeDefinition;
begin
  SetCaptionKey(cap_key);
  Result:=Self;
end;

function TFRE_DB_InputGroupSchemeDefinition.GetCaptionKey: TFRE_DB_NameType;
begin
  result := FCaption_Key;// Field('cap_key').AsString;
end;

procedure TFRE_DB_InputGroupSchemeDefinition._addInput(const schemefield: TFRE_DB_String; const cap_trans_key: TFRE_DB_String; const disabled: Boolean;const hidden:Boolean; const field_backing_collection: TFRE_DB_String;const fbCollectionIsDerivedCollection:boolean;const std_right:TFRE_DB_STANDARD_RIGHT; const rightClasstype: TClass; const hideSingle: Boolean; const chooser_type:TFRE_DB_CHOOSER_DH; const standard_coll: TFRE_DB_STANDARD_COLL;const chooserAddEmptyForRequired:Boolean);
var
  obj      : OFRE_InputFieldDef4Group;
  path     : TFRE_DB_StringArray;
  scheme   : TFRE_DB_SchemeObject;
  i        : Integer;
  fieldDef : TFRE_DB_FieldSchemeDefinition;
  required : Boolean;

begin
  if Length(schemefield)>=255 then
    raise EFRE_DB_Exception.Create(edb_ERROR,'(nested) schemefield longer or equal 255 chars / limit');
  scheme := FScheme;
  FREDB_SeperateString(schemefield,'.',path);
  required:=true;
  if Length(path)>1 then begin
    for i := 0 to Length(path) - 2 do begin
      if not scheme.GetSchemeField(path[i],fieldDef) then raise EFRE_DB_Exception.Create(edb_ERROR,'cannot find scheme field: '+path[i]);
      required:=required and fieldDef.required;
      if not GFRE_DB.GetSystemScheme(fieldDef.GetSubSchemeName,scheme) then
        raise EFRE_DB_Exception.Create(edb_ERROR,'cannot find subscheme field: %s:(%s)',[schemefield,fieldDef.GetSubSchemeName]);
    end;
  end;

  if not scheme.GetSchemeField(path[High(path)],fieldDef) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'cannot find scheme field: %s:(%s)',[scheme.DefinedSchemeName,schemefield]);

  if Assigned(rightClasstype) and (fieldDef.GetFieldType<>fdbft_ObjLink) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'domain chooser field has to be of type fdbft_ObjLink: %s:(%s)',[scheme.DefinedSchemeName,schemefield]);

  if (standard_coll<>coll_NONE) and (field_backing_collection<>'') then
    raise EFRE_DB_Exception.Create(edb_ERROR,'standard_coll has to be coll_NONE if field_backing_collection is defined');

  //obj                := default(OFRE_InputFieldDef4Group);
  FillByte(obj,sizeof(OFRE_InputFieldDef4Group),0);
  obj.typ              := igd_Field;
  obj.field            := schemefield; // field
  obj.required         := required and fieldDef.required;
  obj.disabled         := disabled;
  obj.hidden           := hidden;
  if cap_trans_key<>'' then
    obj.caption_key    := cap_trans_key
  else
    obj.caption_key    := '$'+fieldDef.FScheme.DefinedSchemeName+'_scheme_'+schemefield;
  obj.datacollection   := field_backing_collection;
  obj.standardcoll     := standard_coll;
  obj.dc_isderivedc    := fbCollectionIsDerivedCollection;
  obj.chooser_type     := chooser_type;
  obj.chooser_add_empty:= chooserAddEmptyForRequired;
  obj.right_classtype  := rightClasstype;
  obj.std_right        := std_right;
  obj.hideSingle       := hideSingle;
  obj.fieldschemdef    := fieldDef;
  Fields.Add(obj);
end;

function TFRE_DB_InputGroupSchemeDefinition.GetInputGroupID: TFRE_DB_String;
begin
  result := groupid; // Field('igid').AsString;
end;

procedure TFRE_DB_InputGroupSchemeDefinition.SetCaptionKey(AValue: TFRE_DB_String);
begin
  FCaption_Key := AValue;
end;

function TFRE_DB_InputGroupSchemeDefinition.SetupI(const caption: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;
begin
  result := Setup(caption);
end;

procedure TFRE_DB_InputGroupSchemeDefinition.AddInput(const schemefield: TFRE_DB_String; const cap_trans_key: TFRE_DB_String; const disabled: Boolean; const hidden: Boolean; const field_backing_collection: TFRE_DB_String; const fbCollectionIsDerivedCollection: Boolean; const chooser_type:TFRE_DB_CHOOSER_DH;const standard_coll: TFRE_DB_STANDARD_COLL;const chooserAddEmptyForRequired: Boolean);
begin
  _addInput(schemefield,cap_trans_key,disabled,hidden,field_backing_collection,fbCollectionIsDerivedCollection,sr_BAD,nil,false,chooser_type,standard_coll,chooserAddEmptyForRequired);
end;

procedure TFRE_DB_InputGroupSchemeDefinition.AddDomainChooser(const schemefield: TFRE_DB_String;  const std_right:TFRE_DB_STANDARD_RIGHT; const rightClasstype: TClass; const hideSingle: Boolean; const cap_trans_key: TFRE_DB_String);
begin
  _addInput(schemefield,cap_trans_key,false,false,'',false,std_right,rightClasstype,hideSingle,dh_chooser_combo);
end;

procedure TFRE_DB_InputGroupSchemeDefinition.UseInputGroup(const scheme, group: TFRE_DB_String; const addPrefix: TFRE_DB_String; const as_gui_subgroup: boolean; const collapsible: Boolean; const collapsed: Boolean);
var igd : OFRE_InputFieldDef4Group;
begin
 //igd := default(OFRE_InputFieldDef4Group);
 FillByte(igd,sizeof(OFRE_InputFieldDef4Group),0);

 if as_gui_subgroup then
   igd.typ            := igd_UsedSubGroup
 else
   igd.typ            := igd_UsedGroup;
 igd.scheme         := scheme;
 igd.field          := scheme; // hack for field compare in sparselist
 igd.group          := uppercase(group);
 igd.prefix         := addPrefix;
 igd.collapsible    := collapsible;
 igd.collapsed      := collapsed;
 Fields.Add(igd);
end;

function TFRE_DB_InputGroupSchemeDefinition.GroupFields: PFRE_InputFieldDef4GroupArr;
var cnt : NativeInt;

  procedure Iterate(var gf : OFRE_InputFieldDef4Group ;const  idx :Nativeint ; var halt :boolean);
  begin
    result[cnt] := @gf;
    inc(cnt);
  end;

begin
  cnt := 0;
  setlength(result,Fields.Count);
  Fields.ForAllBreak(@Iterate);
end;

function TFRE_DB_InputGroupSchemeDefinition.GetParentSchemeI: IFRE_DB_SchemeObject;
begin
  result := FScheme;
end;

function TFRE_DB_InputGroupSchemeDefinition.FieldDefIsNull(const obj: PFRE_InputFieldDef4Group): boolean;
begin
  result := obj^.field='';
end;

function TFRE_DB_InputGroupSchemeDefinition.FieldDefCompare(const o1, o2: PFRE_InputFieldDef4Group): boolean;
begin
  result := o1^.field=o2^.field;
end;

function local_FieldDefIsNull(const obj: PFRE_InputFieldDef4Group): boolean;
begin
  result := obj^.field='';
end;

function local_FieldDefCompare(const o1, o2: PFRE_InputFieldDef4Group): boolean;
begin
  result := o1^.field=o2^.field;
end;

constructor TFRE_DB_InputGroupSchemeDefinition.Create(const gid: TFRE_DB_NameType; scheme: TFRE_DB_SchemeObject);
var obj : OFRE_InputFieldDef4Group;
begin
  groupid := uppercase(gid);
  FScheme := scheme;
  //obj := default(OFRE_InputFieldDef4Group);
  FillByte(obj,sizeof(OFRE_InputFieldDef4Group),0);
  Fields.InitSparseList(obj,@local_FieldDefIsNull,@local_FieldDefCompare);
end;

destructor TFRE_DB_InputGroupSchemeDefinition.Destroy;
begin
  inherited Destroy;
end;

function TFRE_DB_COMMAND.GetCommandID: UInt64;
begin
  result :=   FCommand_Id.AsUInt64;
end;

function TFRE_DB_COMMAND.GetData: TFRE_DB_Object;
begin
  result := FData.AsObject;
end;

function TFRE_DB_COMMAND.GetDataI: IFRE_DB_Object;
begin
  result := GetData;
end;


function TFRE_DB_COMMAND.CheckoutData: TFRE_DB_Object;
begin
  result := FData.AsObject;
  FData.Clear(true);
  result.FParentDBO := nil;
end;

function TFRE_DB_COMMAND.CheckoutDataI: IFRE_DB_Object;
begin
  result := CheckoutData;
end;

function TFRE_DB_COMMAND.GetIsAnswer: Boolean;
begin
  result := FAnswer.AsBoolean;
end;

function TFRE_DB_COMMAND.GetIsClient: Boolean;
begin
  result := FClient.AsBoolean;
end;

procedure TFRE_DB_COMMAND.SetData(const AValue: TFRE_DB_Object);
begin
  FData.AsObject := AValue;
end;

procedure TFRE_DB_COMMAND.SetDataI(const AValue: IFRE_DB_Object);
begin
  if assigned(AValue) then
    SetData(AValue.Implementor as TFRE_DB_Object);
end;

procedure TFRE_DB_COMMAND.SetCommandID(const AValue: UInt64);
begin
  FCommand_Id.AsUInt64:=AValue;
end;

procedure TFRE_DB_COMMAND.InternalSetup;
begin
  inherited InternalSetup;
  FCommand_Id   := Field('I');
  FData         := Field('D');
  FAnswer       := Field('A');
  FClient       := Field('CC');
  FCtype        := Field('CT');
  FInvokeClass  := Field('IC');
  FInvokeMethod := Field('IM');
  FFatalClose   := Field('FC');
  FErrorText    := Field('E');
  //FIsInterfaceC := Field('IIC');
  FChangesession:= Field('S');
  FBinDataKey   := Field('BDK');
  FIUidPath     := Field('UIP');
  SetFatalClose(false);
end;

function TFRE_DB_COMMAND._ObjectsNeedsNoSubfieldSchemeCheck: boolean;
begin
  result := true;
end;

function TFRE_DB_COMMAND._ObjectIsCodeclassOnlyAndHasNoScheme: boolean;
begin
  result := true;
end;

procedure TFRE_DB_COMMAND.SetIsAnswer(const AValue: Boolean);
begin
 FAnswer.AsBoolean:=AValue;
end;

procedure TFRE_DB_COMMAND.SetIsClient(const AValue: Boolean);
begin
 FClient.AsBoolean:=AValue;
end;

procedure TFRE_DB_COMMAND.CopyToMemory(memory: Pointer);
begin
  inherited CopyToMemory(memory);
end;

function TFRE_DB_COMMAND.GetInvokeClass: String;
begin
  result := FInvokeClass.AsString;
end;

function TFRE_DB_COMMAND.GetInvokeMethod: String;
begin
  result := FInvokeMethod.AsString;
end;

procedure TFRE_DB_COMMAND.SetInvokeClass(AValue: String);
begin
  FInvokeClass.AsString := AValue;
end;

procedure TFRE_DB_COMMAND.SetInvokeMethod(AValue: String);
begin
  FInvokeMethod.AsString := AValue;
end;

function TFRE_DB_COMMAND.GetEText: TFRE_DB_String;
begin
  result := FErrorText.AsString;
end;

function TFRE_DB_COMMAND.GetFatalClose: Boolean;
begin
  result := FFatalClose.AsBoolean;
end;

function TFRE_DB_COMMAND.GetChangeSessionKey: String;
begin
  result := FChangeSession.AsString;
end;

procedure TFRE_DB_COMMAND.SetChangeSessionKey(AValue: String);
begin
  FChangeSession.AsString:=AValue;
end;

procedure TFRE_DB_COMMAND.SetFatalClose(AValue: Boolean);
begin
  FFatalClose.AsBoolean := AValue;
end;

procedure TFRE_DB_COMMAND.SetEText(AValue: TFRE_DB_String);
begin
  FErrorText.AsString := AValue;
end;

procedure TFRE_DB_COMMAND.SetBinDataKey(AValue: string);
begin
  FBinDataKey.asstring := AValue;
end;

function TFRE_DB_COMMAND.GetBinDataKey: string;
begin
  result := FBinDataKey.AsString;
end;

function TFRE_DB_COMMAND.GetUidPath: TFRE_DB_GUIDArray;
begin
  result := FIUidPath.AsGUIDArr;
end;

procedure TFRE_DB_COMMAND.SetUidPath(AValue: TFRE_DB_GUIDArray);
begin
  FIUidPath.AsGUIDArr := AValue;
end;

function TFRE_DB_COMMAND.GetCType: TFRE_DB_COMMANDTYPE;
begin
  case FCtype.AsByte of
    1 : result := fct_SyncRequest  ;
    2 : result := fct_SyncReply    ;
    3 : result := fct_AsyncRequest ;
    5 : result := fct_Error        ;
  end;
end;

procedure TFRE_DB_COMMAND.SetCType(AValue: TFRE_DB_COMMANDTYPE);
begin
  case AValue of
    fct_SyncRequest  : FCtype.AsByte := 1 ;
    fct_SyncReply    : FCtype.AsByte := 2 ;
    fct_AsyncRequest : FCtype.AsByte := 3 ;
    fct_Error        : FCtype.AsByte := 5 ;
  end;
end;

procedure TFRE_DB_COMMAND.SetAnswerInterface(const answer_interface: IFRE_DB_COMMAND_REQUEST_ANSWER_SC);
begin
  SetReference(answer_interface.Implementor);
end;

function TFRE_DB_COMMAND.GetAnswerInterface: IFRE_DB_COMMAND_REQUEST_ANSWER_SC;
begin
  if not Sysutils.Supports(GetReference,IFRE_DB_COMMAND_REQUEST_ANSWER_SC,result) then raise EFRE_DB_Exception.Create(edb_ERROR,'logic');
end;

function TFRE_DB_COMMAND.AsJSONString: TFRE_DB_RawByteString;
begin
  result := GetAsJSONString(false,false);
end;

function TFRE_DB_COMMAND.AsDBODump: TFRE_DB_RawByteString;
begin
  result := DumpToString();
end;


function TFRE_DB_RESOURCE_CONTAINER.SetIconFromFile(const resource_id, filename, mimetype: TFRE_DB_String): TFRE_DB_Errortype;
var icon:TFRE_DB_RESOURCE;
begin
  icon := TFRE_DB_RESOURCE.Create;
  icon.Field('data').AsStream.LoadFromFile(filename);
  icon.Field('mimetype').AsString:=mimetype;
  icon.Field('name').AsString:=resource_id;
  Field(resource_id).AsObject:=icon;
end;

{ TFRE_DB_RESOURCE }

function TFRE_DB_RESOURCE.IMI_Content(const input: TFRE_DB_Object): IFRE_DB_Object;
begin
  Result := TFRE_DB_RESOURCE_DESC.create.Describe(Self);
end;

function TFRE_DB_NAMED_OBJECT.GetDesc: TFRE_DB_TEXT;
begin
  result := field('desc').AsObject as TFRE_DB_TEXT;
end;

function TFRE_DB_NAMED_OBJECT.GetName: TFRE_DB_String;
begin
  result := field('objname').AsString;
end;

procedure TFRE_DB_NAMED_OBJECT.SetDesc(const AValue: TFRE_DB_TEXT);
begin
  field('desc').AsObject := AValue;
end;

procedure TFRE_DB_NAMED_OBJECT.SetName(const AValue: TFRE_DB_String);
begin
  field('objname').AsString:=AValue;
end;

function TFRE_DB_NAMED_OBJECT.GetDescI: IFRE_DB_TEXT;
begin
  result := GetDesc;
end;

procedure TFRE_DB_NAMED_OBJECT.SetDescI(const AValue: IFRE_DB_TEXT);
begin
  SetDesc(AValue.Implementor as TFRE_DB_TEXT);
end;

class procedure TFRE_DB_NAMED_OBJECT.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('objname',fdbft_String);
  scheme.AddSchemeFieldSubscheme('desc','TFRE_DB_TEXT');
  Scheme.SetSysDisplayField(TFRE_DB_NameTypeArray.Create('objname','$DBTEXT:desc'),'%s - (%s)');
  Scheme.Strict(true);
end;

function TFRE_DB_GROUP.GetDomain(const conn: IFRE_DB_CONNECTION): TFRE_DB_NameType;
var syscon       : TFRE_DB_SYSTEM_CONNECTION;
begin
  syscon := (conn.Implementor_HC as TFRE_DB_CONNECTION).FSysConnection;
  result := syscon.FetchDomainNameById(DomainID);
end;


function TFRE_DB_GROUP.GetDomainIDLink: TFRE_DB_GUID;
begin
  result := Field('domainidlink').AsObjectLink;
end;

function TFRE_DB_GROUP.GetIsInternal: Boolean;
begin
  Result:=Field('internal').AsBoolean;
end;

function TFRE_DB_GROUP.GetIsDisabled: Boolean;
var f : TFRE_DB_Field;
begin
  if FieldOnlyExisting('disabled',f) then
    Result:=f.AsBoolean
  else
    result := false;
end;

function TFRE_DB_GROUP.GetIsProtected: Boolean;
begin
  Result:=Field('protected').AsBoolean;
end;


function TFRE_DB_GROUP.GetRoleIDs: TFRE_DB_ObjLinkArray;
begin
  result := Field('roleids').AsObjectLinkArray;
end;

procedure TFRE_DB_GROUP.SetIsInternal(AValue: Boolean);
begin
  Field('internal').AsBoolean:=AValue;
end;

procedure TFRE_DB_GROUP.SetIsProtected(AValue: Boolean);
begin
  Field('protected').AsBoolean:=AValue;
end;

procedure TFRE_DB_GROUP.SetIsDisabled(AValue: Boolean);
begin
  Field('disabled').AsBoolean:=AValue;
end;

procedure TFRE_DB_GROUP.SetDomainIDLink(AValue: TFRE_DB_GUID);
begin
  Field('domainidlink').AsObjectLink := AValue;
  _UpdateDomainGroupKey;
end;

procedure TFRE_DB_GROUP.SetRoleIDs(AValue: TFRE_DB_ObjLinkArray);
begin
  Field('roleids').AsObjectLinkArray := AValue;
end;

procedure TFRE_DB_GROUP._UpdateDomainGroupKey;
begin
  field('domaingroupkey').AsString := GetDomainGroupKey(ObjectName,domainid);
end;

procedure TFRE_DB_GROUP._calcDisplayName(const calc: IFRE_DB_CALCFIELD_SETTER);
var
  dname: String;
begin
  if FieldPathExists('desc.txt') and (FieldPath('desc.txt').AsString<>'') then begin
    dname:=FieldPath('desc.txt').AsString+' ('+Field('objname').AsString+')';
  end else begin
    dname:=Field('objname').AsString;
  end;
  calc.SetAsString(dname);
end;

class procedure TFRE_DB_GROUP.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var input_group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.Strict(false);
  Scheme.SetParentSchemeByName(TFRE_DB_NAMED_OBJECT.ClassName);
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddCalcSchemeField('displayname',fdbft_String,@_calcDisplayName);
  scheme.AddSchemeField('roleids',fdbft_ObjLink).SetupFieldDef(false,true);
  scheme.AddSchemeField('appdataid',fdbft_ObjLink);
  scheme.AddSchemeField('domainidlink',fdbft_ObjLink).SetupFieldDef(true,false);
  scheme.AddSchemeField('domaingroupkey',fdbft_String).SetupFieldDef(true,false);
  Scheme.SetSysDisplayField(TFRE_DB_NameTypeArray.Create('objname','$DBTEXT:desc'),'%s - (%s)');

  input_group:=scheme.AddInputGroup('main').Setup('$TFRE_DB_GROUP_scheme_group_group');
  input_group.AddInput('objname','$TFRE_DB_GROUP_scheme_name');
  input_group.AddDomainChooser('domainidlink',sr_STORE,TFRE_DB_GROUP,true,'$TFRE_DB_GROUP_scheme_domainid');
  input_group.UseInputGroup('TFRE_DB_TEXT','main','desc',true,true,false);

  input_group:=scheme.AddInputGroup('main_edit').Setup('$TFRE_DB_GROUP_scheme_group_group');
  input_group.AddInput('objname','$TFRE_DB_GROUP_scheme_name');
  input_group.UseInputGroup('TFRE_DB_TEXT','main','desc',true,true,false);
end;

function TFRE_DB_GROUP.SubFormattedDisplayAvailable: boolean;
begin
  if not FieldExists('roles') then exit(false);
  result := true;
end;

function TFRE_DB_GROUP.GetSubFormattedDisplay(indent: integer): TFRE_DB_String;
var l_right : TFRE_DB_StringArray;
    l_ident : shortstring;
    i       : Integer;
begin
  l_right := Field('roles').AsStringArr;
  if Length(l_right)>0 then begin
    result := StringOfChar(' ',indent)+GFRE_DB.StringArray2String(l_right);
  end;
end;

class function TFRE_DB_GROUP.GetDomainGroupKey(const grouppart: TFRE_DB_String; const domain_id: TFRE_DB_GUID): TFRE_DB_String;
begin
  result := lowercase(FREDB_G2H(domain_id)+'@'+grouppart);
end;

class procedure TFRE_DB_GROUP.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.1';
  if currentVersionId='' then begin
    currentVersionId := '1.0';

    conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_GROUP_scheme_group_group','Group'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_GROUP_scheme_group_domain','Domain'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_GROUP_scheme_name','Name'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_GROUP_scheme_domainid','Domain'));
  end;
  if currentVersionId='1.0' then begin
    currentVersionId := '1.1';
  end;
end;

class procedure TFRE_DB_GROUP.InstallDBObjects4Domain(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TFRE_DB_GUID);
var
  role: IFRE_DB_ROLE;
begin
  inherited InstallDBObjects4Domain(conn, currentVersionId, domainUID);
  if currentVersionId='' then begin
    currentVersionId := '1.0';

    role := CreateClassRole('assignGroup','Assign Group','Allowed to assign Groups');
    role.AddRight(GetRight4Domain(GetClassRightName('assignGroup'),domainUID));
    CheckDbResult(conn.StoreRole(role,domainUID),'Error creating '+ClassName+'.assignGroup role');
  end;
  if currentVersionId='1.0' then begin
    currentVersionId := '1.1';

    role := CreateClassRole('disableGroup','Disable Group','Allowed to disable Groups');
    role.AddRight(GetRight4Domain(GetClassRightName('disableGroup'),domainUID));
    CheckDbResult(conn.StoreRole(role,domainUID),'Error creating '+ClassName+'.disableGroup role');
  end;
end;

class procedure TFRE_DB_GROUP.InstallDBObjects4SysDomain(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TFRE_DB_GUID);
var
  role: IFRE_DB_ROLE;
begin
  inherited InstallDBObjects4SysDomain(conn, currentVersionId, domainUID);
  if currentVersionId='' then begin
    currentVersionId := '1.0';

    role := CreateClassRole('assignGroup','Assign System Group','Allowed to assign System Groups');
    role.AddRight(GetRight4Domain(GetClassRightName('assignGroup'),domainUID));
    CheckDbResult(conn.StoreRole(role,domainUID),'Error creating '+ClassName+'.assignGroup role');
  end;
end;

function TFRE_DB_TEXT.GetHint: TFRE_DB_String;
begin
  result := _Field('hnt').AsString; // Todo Translate
end;

function TFRE_DB_TEXT.GetLong: TFRE_DB_String;
begin
  result := _Field('txt').AsString; // Todo Translate
end;

function TFRE_DB_TEXT.Getshort: TFRE_DB_String;
begin
 result := _Field('txt_s').AsString; // Todo Translate
end;

function TFRE_DB_TEXT.GetTKey: TFRE_DB_String;
begin
  result := _Field('t_key').AsString;
end;

procedure TFRE_DB_TEXT.SetHint(const AValue: TFRE_DB_String);
begin
 _Field('hnt').AsString :=  AValue;
end;

procedure TFRE_DB_TEXT.Setlong(const AValue: TFRE_DB_String);
begin
  _Field('txt').AsString:=AValue;
end;

procedure TFRE_DB_TEXT.SetShort(const AValue: TFRE_DB_String);
begin
  _Field('txt_s').AsString:=AValue;
end;

procedure TFRE_DB_TEXT.SetTKey(const AValue: TFRE_DB_String);
begin
  _Field('t_key').AsString :=  AValue;
end;

class procedure TFRE_DB_TEXT.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var field_def           : IFRE_DB_FieldSchemeDefinition;
    input_group         : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.Strict(true);
  field_def         := scheme.AddSchemeField('t_key',fdbft_String);
  field_def         := scheme.AddSchemeField('txt',fdbft_String);
  field_def         := scheme.AddSchemeField('txt_s',fdbft_String);
  field_def         := scheme.AddSchemeField('hnt',fdbft_String);

  input_group:=scheme.AddInputGroup('main').Setup('$TFRE_DB_TEXT_scheme_descr_group');
  input_group.AddInput('txt','$TFRE_DB_TEXT_scheme_txt');
  input_group.AddInput('txt_s','$TFRE_DB_TEXT_scheme_txt_s');
  input_group:=scheme.AddInputGroup('main_full').Setup('$TFRE_DB_TEXT_scheme_descr_group');
  input_group.AddInput('txt','$TFRE_DB_TEXT_scheme_txt');
  input_group.AddInput('txt_s','$TFRE_DB_TEXT_scheme_txt_s');
  input_group.AddInput('hnt','$TFRE_DB_TEXT_scheme_hnt');
  input_group:=scheme.AddInputGroup('main_all').Setup('$TFRE_DB_TEXT_scheme_descr_group');
  input_group.AddInput('txt','$TFRE_DB_TEXT_scheme_txt');
  input_group.AddInput('txt_s','$TFRE_DB_TEXT_scheme_txt_s');
  input_group.AddInput('hnt','$TFRE_DB_TEXT_scheme_hnt');
  input_group.AddInput('t_key','$TFRE_DB_TEXT_scheme_key');
end;

class procedure TFRE_DB_TEXT.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';

  if (currentVersionId='') then begin
    currentVersionId:='1.0';

    conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_TEXT_scheme_descr_group','Description'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_TEXT_scheme_txt','Description'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_TEXT_scheme_txt_s','Brief Description'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_TEXT_scheme_hnt','Hint'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_TEXT_scheme_key','Key'));
  end;
end;

procedure TFRE_DB_TEXT.ClearLong;
begin
  _Field('txt').Clear();
end;

procedure TFRE_DB_TEXT.ClearShort;
begin
  _Field('txt_s').Clear();
end;

procedure TFRE_DB_TEXT.ClearHint;
begin
  _Field('hnt').Clear();
end;

procedure TFRE_DB_TEXT.ApplyParameters(const encoded_params: TFRE_DB_StringArray);
var text : TFRE_DB_String;
    args : TFRE_DB_ConstArray;
    s    :string;
begin
  FREDB_DecodeVarRecParams(encoded_params,args);
  try
    LongText  := Format(LongText,args);
    ShortText := Format(ShortText,args);
    Hint      := Format(Hint,args);
  finally
    FREDB_FinalizeVarRecParams(args);
  end;
end;

class function TFRE_DB_TEXT.CreateText(const translation_key: TFRE_DB_String; const short_text: TFRE_DB_String; const long_text: TFRE_DB_String; const hint_text: TFRE_DB_String): TFRE_DB_TEXT;
begin
  result := GFRE_DB.NewObject(TFRE_DB_TEXT.ClassName) as TFRE_DB_TEXT;
  result.LongText       := long_text;
  result.ShortText      := short_text;
  result.Hint           := hint_text;
  result.TranslationKey := translation_key;
end;


procedure TFRE_DB_TEXT.SetupText(const translation_key: TFRE_DB_String; const short_text: TFRE_DB_String; const long_text: TFRE_DB_String; const hint_text: TFRE_DB_String);
begin
 LongText       := long_text;
 ShortText      := short_text;
 Hint           := hint_text;
 TranslationKey := translation_key;
end;

function TFRE_DB_ROLE.GetDomain(const conn: IFRE_DB_CONNECTION): TFRE_DB_NameType;
var syscon       : TFRE_DB_SYSTEM_CONNECTION;
begin
  syscon := (conn.Implementor_HC as TFRE_DB_CONNECTION).FSysConnection;
  result := syscon.FetchDomainNameById(GetDomainIDLink);
end;

function TFRE_DB_ROLE.GetDomainIDLink: TFRE_DB_GUID;
begin
  result := Field('domainidlink').AsObjectLink;
end;

function TFRE_DB_ROLE.GetIsInternal: Boolean;
begin
  Result:=Field('internal').AsBoolean;
end;

function TFRE_DB_ROLE.GetIsDisabled: Boolean;
var f : TFRE_DB_FIELD;
begin
  if FieldOnlyExisting('disabled',f) then
    Result := f.AsBoolean
  else
    result := false;
end;

procedure TFRE_DB_ROLE.SetDomainIDLink(AValue: TFRE_DB_GUID);
begin
 Field('domainidlink').AsObjectLink := AValue;
 Field('domainrolekey').AsString := GetDomainRoleKey(ObjectName,AValue);
end;

procedure TFRE_DB_ROLE.SetIsInternal(AValue: Boolean);
begin
  Field('internal').AsBoolean:=AValue;
end;

procedure TFRE_DB_ROLE.SetIsDisabled(AValue: Boolean);
begin
  Field('disabled').AsBoolean:=AValue;
end;

procedure TFRE_DB_ROLE._calcDisplayName(const calc: IFRE_DB_CALCFIELD_SETTER);
var
  dname: String;
begin
  if FieldPathExists('desc.txt') then begin
    dname:=FieldPath('desc.txt').AsString;
  end else begin
    dname:=Field('objname').AsString;
  end;
  calc.SetAsString(dname);
end;

class procedure TFRE_DB_ROLE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.1';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
  if currentVersionId='1.0' then begin
    currentVersionId := '1.1';
  end;
end;

class procedure TFRE_DB_ROLE.InstallDBObjects4Domain(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TFRE_DB_GUID);
var
  role: IFRE_DB_ROLE;
begin
  inherited InstallDBObjects4Domain(conn, currentVersionId, domainUID);
  if currentVersionId='' then begin
    currentVersionId := '1.0';

    role := CreateClassRole('assignRole','Assign Role','Allowed to assign Roles');
    role.AddRight(GetRight4Domain(GetClassRightName('assignRole'),domainUID));
    CheckDbResult(conn.StoreRole(role,domainUID),'Error creating '+ClassName+'.assignRole role');
  end;
  if currentVersionId='1.0' then begin
    currentVersionId := '1.1';

    role := CreateClassRole('disableRole','Disable Role','Allowed to disable Roles');
    role.AddRight(GetRight4Domain(GetClassRightName('disableRole'),domainUID));
    CheckDbResult(conn.StoreRole(role,domainUID),'Error creating '+ClassName+'.disableRole role');
  end;
end;

class procedure TFRE_DB_ROLE.InstallDBObjects4SysDomain(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TFRE_DB_GUID);
var
  role: IFRE_DB_ROLE;
begin
  inherited InstallDBObjects4SysDomain(conn, currentVersionId, domainUID);
  if currentVersionId='' then begin
    currentVersionId := '1.0';

    role := CreateClassRole('assignRole','Assign System Role','Allowed to assign System Roles');
    role.AddRight(GetRight4Domain(GetClassRightName('assignRole'),domainUID));
    CheckDbResult(conn.StoreRole(role,domainUID),'Error creating '+ClassName+'.assignRole role');
  end;
end;

procedure TFRE_DB_ROLE.AddRight(const right: IFRE_DB_RIGHT);
begin
  Field('rights').AddString(right);
end;

function TFRE_DB_ROLE.SubFormattedDisplayAvailable: boolean;
begin
  Result:=true;
end;

function TFRE_DB_ROLE.GetSubFormattedDisplay(indent: integer): TFRE_DB_String;
var l_right : TFRE_DB_ObjectArray;
    l_ident : shortstring;
    i       : Integer;
begin
  l_right := Field('rights').AsObjectArr;
  l_ident := StringOfChar(' ',indent);
  for i:=0 to high(l_right) do begin
    result :=  result + l_ident+'-'+l_right[i].GetFormattedDisplay;
    if i<> high(l_right) then result:=Result+LineEnding;
  end;
end;

function TFRE_DB_ROLE.GetRightNames: TFRE_DB_StringArray;
begin
  result := Field('rights').AsStringArr;
end;

procedure TFRE_DB_ROLE.AddRightsFromRole(const role: IFRE_DB_ROLE);
var i      : NativeInt;
    rights : TFRE_DB_StringArray;
begin
    rights := role.GetRightNames;
    for i := 0 to high(rights) do
      begin
        AddRight(rights[i]);
      end;
  //TODO: MakeRightstringsUnique
end;

procedure TFRE_DB_ROLE.RemoveRights(const rights: TFRE_DB_StringArray);
var
  my_rights  : TFRE_DB_StringArray;
  keep_right : Boolean;
  i,j        : Integer;
begin
  my_rights:=Field('rights').AsStringArr;
  DeleteField('rights');
  for i := 0 to High(my_rights) do begin
    keep_right:=true;
    for j := 0 to High(rights) do begin
      if my_rights[i]=rights[j] then begin
        keep_right:=false;
      end;
      if keep_right then begin
        AddRight(my_rights[i]);
      end;
    end;
  end;
end;

class procedure TFRE_DB_ROLE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.Strict(true);
  Scheme.SetParentSchemeByName(TFRE_DB_NAMED_OBJECT.ClassName);
  scheme.AddSchemeField('appdataid',fdbft_ObjLink).SetupFieldDef(false,true);
//  scheme.AddSchemeFieldSubscheme('rights','TFRE_DB_RIGHT').multiValues:=true;
  scheme.AddSchemeField('domainidlink',fdbft_ObjLink).SetupFieldDef(false,false);
  scheme.AddSchemeField('domainrolekey',fdbft_String).SetupFieldDef(false,false);
  scheme.AddCalcSchemeField('displayname',fdbft_String,@_calcDisplayName);
  Scheme.SetSysDisplayField(TFRE_DB_NameTypeArray.Create('objname','$DBTEXT:desc'),'%s - (%s)');
end;

class function TFRE_DB_ROLE.GetDomainRoleKey(const rolepart: TFRE_DB_String; const domain_id: TFRE_DB_GUID): TFRE_DB_String;
begin
  result := lowercase(FREDB_G2H(domain_id)+'@'+rolepart);
end;

function TFRE_DB_SYSTEM_CONNECTION.ReNewCurrentUserToken(const user: TFRE_DB_USER): IFRE_DB_USER_RIGHT_TOKEN;
begin
  result := FPersistance_Layer.RebuildUserToken(user.UID);
end;

function TFRE_DB_SYSTEM_CONNECTION.ImpersonateTheClone(const user, pass: TFRE_DB_String): TFRE_DB_Errortype;
var
  login : TFRE_DB_String;
  domain: TFRE_DB_String;
  luser : TFRE_DB_USER;
begin
  if not FCloned then
    raise EFRE_DB_Exception.Create(edb_ERROR,'you can only impersonate a cloned systemconnection');

  FREDB_SplitLocalatDomain(user,login,domain);
  FetchUser(login,DomainID(domain),luser);
  if not assigned(luser) then
    exit(edb_NOT_FOUND);
  if not luser.Checkpassword(pass) then
    exit(edb_ACCESS);
  if assigned(FCurrentUserToken) then
    FreeAndNil(FCurrentUserToken);
  FCurrentUserToken := ReNewCurrentUsertoken(luser).Implementor as TFRE_DB_USER_RIGHT_TOKEN;
  result := edb_OK;
end;

procedure TFRE_DB_SYSTEM_CONNECTION.InternalSetupConnection;

  procedure SetupTransTextCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysTransText') then begin
      GFRE_DB.LogDebug(dblc_DB,'Adding System collection SysTransText');
      coll := Collection('SysTransText');
      coll.DefineIndexOnField('t_key',fdbft_String,True,True);
    end;
    FSysTransText := Collection('SysTransText');
  end;

  procedure SetupSessionDataCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysSessionData') then begin
      GFRE_DB.LogDebug(dblc_DB,'Adding System collection SysSessionData',[]);
      coll := Collection('SysSessionData');
      coll.DefineIndexOnField('$LOGIN_KEY',fdbft_String,True,True);
    end;
    FSysUserSessionsData := Collection('SysSessionData');
  end;

  procedure SetupDomainCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysDomain') then begin
      GFRE_DB.LogDebug(dblc_DB,'Adding System collection SysDomain');
      coll := Collection('SysDomain');
      coll.DefineIndexOnField('objname',fdbft_String,True,True);
    end;
    FSysDomains := Collection('SysDomain');
  end;

  procedure SetupUserCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysUser') then begin
      GFRE_DB.LogDebug(dblc_DB,'Adding System collection SysUser');
      coll := Collection('SysUser');
      coll.DefineIndexOnField('domainloginkey',fdbft_String,True,True);
    end;
    FSysUsers := Collection('SysUser');
  end;

  procedure SetupRoleCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysRole') then begin
      GFRE_DB.LogDebug(dblc_DB,'Adding System collection SysRole');
      coll := Collection('SysRole');
      coll.DefineIndexOnField('domainrolekey',fdbft_String,True,True);
    end;
    FSysRoles := Collection('SysRole');
  end;

  procedure SetupUserGroupCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysUserGroup') then begin
      GFRE_DB.LogDebug(dblc_DB,'Adding System collection SysUserGroup');
      coll := Collection('SysUserGroup');
      coll.DefineIndexOnField('domaingroupkey',fdbft_String,True,True);
    end;
    FSysGroups := Collection('SysUserGroup');
  end;

  procedure SetupNoteCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysNoteCollection') then begin
      GFRE_DB.LogDebug(dblc_DB,'Adding System collection SysNoteCollection');
      coll := Collection('SysNoteCollection');
      coll.DefineIndexOnField('link',fdbft_String,True,True);
    end;
    FSysNotes := Collection('SysNoteCollection');
  end;

  procedure SetupWorkflowCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysWorkflow') then begin
      GFRE_DB.LogDebug(dblc_DB,'Adding System collection SysWorkflow');
      coll := Collection('SysWorkflow');
    end;
    FSysWorkflow := Collection('SysWorkflow');

    if not CollectionExists('SysWorkflowScheme') then begin
      GFRE_DB.LogDebug(dblc_DB,'Adding System collection SysWorkflowScheme');
      coll := Collection('SysWorkflowScheme');
      coll.DefineIndexOnField('error_idx',fdbft_String,true,true);
    end;
    FSysWorkflowScheme := Collection('SysWorkflowScheme');

    if not CollectionExists('SysWorkflowMethods') then begin
      GFRE_DB.LogDebug(dblc_DB,'Adding System collection SysWorkflowMethods');
      coll := Collection('SysWorkflowMethods');
      coll.DefineIndexOnField('key',fdbft_String,true,true,'def',false,false);
    end;
    FSysWorkflowMethods := Collection('SysWorkflowMethods');
  end;

  procedure SetupAuditCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysAudit') then begin
      GFRE_DB.LogDebug(dblc_DB,'Adding System collection SysAudit');
      coll := Collection('SysAudit');
    end;
    FSysAudit := Collection('SysAudit');
  end;

  procedure SetupNotificationCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysNotifications') then begin
      GFRE_DB.LogDebug(dblc_DB,'Adding System collection SysNotifications');
      coll := Collection('SysNotifications');
    end;
    FSysNotifications := Collection('SysNotifications');
  end;

  procedure SetupAppConfigCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysAppConfigs') then begin
      GFRE_DB.LogDebug(dblc_DB,'Adding System collection SysAppConfigs');
      coll := Collection('SysAppConfigs');
      coll.DefineIndexOnField('id',fdbft_String,true,true);
    end;
    FSysAppConfigs := Collection('SysAppConfigs');
  end;

  procedure SetupSingletonCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysSingleton') then begin
      GFRE_DB.LogDebug(dblc_DB,'Adding System collection SysSingleton');
      coll := Collection('SysSingleton');
      coll.DefineIndexOnField('singletonkey',fdbft_String,True,True);
    end;
    FSysSingletons := Collection('SysSingleton');
  end;

  procedure SetupSystemDomain;
  var domain      : TFRE_DB_DOMAIN;
  begin
    //if FRecreateSysObjects then
    //  DeleteDomain(CFRE_DB_SYS_DOMAIN_NAME);
    if not DomainExists(CFRE_DB_SYS_DOMAIN_NAME) then begin
      domain    := GFRE_DB._NewDomain(CFRE_DB_SYS_DOMAIN_NAME,'System Domain','SYSTEM DOMAIN');
      domain.SetDomainID(domain.UID);
      if FSysDomains._InternalStore(TFRE_DB_Object(domain))<>edb_OK then
         raise EFRE_DB_Exception.Create('could not create system domain');
    end;
    FSysDomainUID := DomainID(CFRE_DB_SYS_DOMAIN_NAME);
  end;

  procedure CheckStandardUsers;
  var upuser : TFRE_DB_USER;
  begin
    if not UserExists('admin',FSysDomainUID) then begin
      GFRE_DB.LogWarning(dblc_DB,'Adding initial db admin account');
      CheckDbResult(_AddUser('admin',FSysDomainUID,'admin','Initial','FRE DB Admin',true),'initial creation of admin user failed');
    end;
    if CFRE_DB_SYS_DOMAIN_NAME = CFRE_DB_SYS_DOMAIN_NAME then begin
      if not UserExists('guest',FSysDomainUID) then begin
        GFRE_DB.LogWarning(dblc_DB,'Adding initial db guest account');
        CheckDbResult(_AddUser('guest',FSysDomainUID,'','Initial','FRE DB Guest',true),'initial creation of guest user failed');
      end;
    end;
    if not UserExists('tasker',FSysDomainUID) then begin
      GFRE_DB.LogWarning(dblc_DB,'Adding initial db takser account');
      CheckDbResult(_AddUser('tasker',FSysDomainUID,'tasker','Initial','FRE DB Tasker',true),'initial creation of takser user failed');
    end;
    if cFRE_DB_RESET_ADMIN=true then
      begin
        CheckDbResult(FetchUser('admin',FSysDomainUID,upuser));
        upuser.SetPassword('admin');
        CheckDbResult(Update(upuser));
        CheckDbResult(FetchUser('guest',FSysDomainUID,upuser));
        upuser.SetPassword('');
        CheckDbResult(Update(upuser));
        CheckDbResult(FetchUser('tasker',FSysDomainUID,upuser));
        upuser.SetPassword(cFRE_TASKER_PASS);
        CheckDbResult(Update(upuser));
      end;
  end;

begin
  inherited; //TODO Hacky checks for Schemecollection are in the baseclass :-( HH
  SetupDomainCollection;
  SetupUserCollection;
  SetupRoleCollection;
  SetupSessionDataCollection;
  SetupSingletonCollection;
  SetupTransTextCollection;
  SetupUserGroupCollection;
  SetupSystemDomain;
  SetupNoteCollection;
  SetupAuditCollection;
  SetupWorkflowCollection;
  SetupNotificationCollection;
  SetupAppConfigCollection;
  CheckStandardUsers;
end;

//function TFRE_DB_SYSTEM_CONNECTION._GetStdRightName(const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass; const domainguid: TFRE_DB_GUID): TFRE_DB_String;
//begin //nln
//  Result := _GetStdRightName(std_right,classtyp)+'@'+uppercase(FREDB_G2H(domainguid));
//end;

//function TFRE_DB_SYSTEM_CONNECTION._GetStdRightName(const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass): TFRE_DB_String;
//begin //nln
//  case std_right of
//    sr_STORE  : result:=TFRE_DB_BaseClass(classtyp).GetClassRightNameStore;
//    sr_UPDATE : result:=TFRE_DB_BaseClass(classtyp).GetClassRightNameUpdate;
//    sr_DELETE : result:=TFRE_DB_BaseClass(classtyp).GetClassRightNameDelete;
//    sr_FETCH  : result:=TFRE_DB_BaseClass(classtyp).GetClassRightNameFetch;
//  else
//    raise EFRE_DB_Exception.Create(edb_INTERNAL,'rightname for standard right is not defined!');
//  end;
//  if Result='' then
//    raise EFRE_DB_Exception.Create(edb_INTERNAL,'i found a fool');
//end;


procedure TFRE_DB_BASE_CONNECTION._ConnectCheck;
begin
  if FConnected=false then
    raise EFRE_DB_Exception.Create(edb_NOT_CONNECTED);
end;

procedure TFRE_DB_BASE_CONNECTION._CloneCheck;
begin
  if FCloned then
    raise EFRE_DB_Exception.Create(edb_ERROR,'operation not allowed on cloned database');
end;

procedure TFRE_DB_BASE_CONNECTION._CloneTrueCheck;
begin
  if not FCloned then
    raise EFRE_DB_Exception.Create(edb_ERROR,'operation only allowed on cloned database');
end;


procedure TFRE_DB_BASE_CONNECTION._CheckSchemeDefinitions(const obj: TFRE_DB_Object);
var lSchemeclass : TFRE_DB_String;
    lScheme      : TFRE_DB_SchemeObject;
begin
  if obj._ObjectIsCodeclassOnlyAndHasNoScheme then exit;
  lSchemeclass := obj.SchemeClass;
  if lSchemeClass<>'' then begin
    if GFRE_DB.GetSysScheme(lSchemeclass,lscheme) then begin
      lScheme.ValidateObject(obj);
    end else begin
      raise EFRE_DB_Exception.Create(edb_ERROR,'cannot access schemeclass [%s] on store',[lSchemeclass]);
    end;
  end;
end;


function TFRE_DB_BASE_CONNECTION._FetchApp(const name: TFRE_DB_String; var app: TFRE_DB_APPLICATION): boolean;
var i     : integer;
    Fapps : TFRE_DB_APPLICATION_ARRAY;
begin
  FApps := GFRE_DB.GetApps;
  for i:=0 to high(FApps) do begin
    if FApps[i].AppClassName=uppercase(name) then begin
      app := FApps[i];
      exit(true);
    end;
  end;
  app := nil;
  result := false;
end;

function TFRE_DB_SYSTEM_CONNECTION._RoleID(const rolename: TFRE_DB_String; const domainUID: TFRE_DB_GUID; var role_id: TFRE_DB_GUID): boolean;
begin //nln
  result := FSysRoles.GetIndexedUIDText(TFRE_DB_ROLE.GetDomainRoleKey(rolename,domainUID),role_id)=1;
end;


function TFRE_DB_SYSTEM_CONNECTION._FetchGroup(const group: TFRE_DB_String; const domain_id:TFRE_DB_GUID; var ug: TFRE_DB_GROUP): boolean;
begin //nln
  result := FSysGroups.GetIndexedObjTextCore(TFRE_DB_GROUP.GetDomainGroupKey(group,domain_id),TFRE_DB_Object(ug))>0;
end;

function TFRE_DB_SYSTEM_CONNECTION._FetchGroupbyID(const group_id: TFRE_DB_GUID; var ug: TFRE_DB_GROUP; const without_right_check: boolean): TFRE_DB_Errortype;
begin //nln
 result := Fetch(group_id,TFRE_DB_Object(ug),without_right_check);
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchDomain(const name: TFRE_DB_NameType; var domain: TFRE_DB_DOMAIN): boolean;
begin
  result := FSysDomains.GetIndexedObjTextCore(name,TFRE_DB_Object(domain))>0;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchDomainUIDbyName(const name: TFRE_DB_NameType; var domain_uid: TFRE_DB_GUID): boolean;
var domain : TFRE_DB_DOMAIN;
begin
  result := FSysDomains.GetIndexedObjTextCore(name,TFRE_DB_Object(domain))>0;
  if result then
    domain_uid := domain.DomainID
  else
    domain_uid := CFRE_DB_NullGUID;
  domain.Finalize;
end;


procedure TFRE_DB_SYSTEM_CONNECTION.DumpSystem;

 procedure DumpAllCollections(const coll : TFRE_DB_COLLECTION);

   procedure DumpNames(const obj : TFRE_DB_Object);
   begin
     writeln('   '+obj.GetFormattedDisplay+' ');
     if obj.SubFormattedDisplayAvailable then begin
       writeln(obj.GetSubFormattedDisplay(4)+' ');
     end;
     obj.free;
   end;

 begin
   writeln('<',coll.CollectionName,' / ',coll.ClassName,'>');
   coll.ForAll(@DumpNames);
end;

begin
  ForAllColls(@DumpAllCollections);
end;

function TFRE_DB_SYSTEM_CONNECTION.AddUser(const login: TFRE_DB_String; const domainUID: TFRE_DB_GUID; const password, first_name, last_name: TFRE_DB_String; const image: TFRE_DB_Stream; const imagetype: String; const is_internal: Boolean; const long_desc: TFRE_DB_String; const short_desc: TFRE_DB_String): TFRE_DB_Errortype;
begin //nl
  result := _AddUser(login,domainUID,password,first_name,last_name,false,image,imagetype,'',is_internal,long_desc,short_desc);
end;

function TFRE_DB_SYSTEM_CONNECTION.UserExists(const login: TFRE_DB_String; const domainUID: TFRE_DB_GUID): boolean;
begin
  result := FSysUsers.ExistsIndexed(TFRE_DB_USER.GetDomainLoginKey(login,domainUID));
end;

function TFRE_DB_SYSTEM_CONNECTION.DeleteUser(const login:TFRE_DB_String; const domainUID: TFRE_DB_GUID): TFRE_DB_Errortype;
begin
  if FSysUsers.RemoveIndexedText(TFRE_DB_USER.GetDomainLoginKey(login,domainUID))=0 then
    exit(edb_NOT_FOUND);
  result := edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION.DeleteUserById(const user_id: TFRE_DB_GUID): TFRE_DB_Errortype;
begin
  result := FSysUsers.Remove(user_id);
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchUser(const login:TFRE_DB_String; const domainUID: TFRE_DB_GUID; var user: TFRE_DB_USER): TFRE_DB_Errortype;
begin
  user:=nil;
  if not FSysUsers.GetIndexedObjTextCore(TFRE_DB_USER.GetDomainLoginKey(login,domainUID),TFRE_DB_Object(user))=0 then
    exit(edb_NOT_FOUND);
  result := edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchUserI(const login:TFRE_DB_String; const domainUID: TFRE_DB_GUID; var user: IFRE_DB_USER): TFRE_DB_Errortype;
var lUSER: TFRE_DB_USER;
begin //nl
  result := FetchUser(login,domainUID,lUSER);
  if result = edb_OK then begin
    user := lUSER;
  end else begin
    user := nil;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchUserById(const user_id: TFRE_DB_GUID; var user: TFRE_DB_USER): TFRE_DB_Errortype;
begin //nl
  result := Fetch(user_id,TFRE_DB_Object(user));
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchUserByIdI(const user_id: TFRE_DB_GUID; var user: IFRE_DB_USER): TFRE_DB_Errortype;
var //nl
  tuser: TFRE_DB_USER;
begin
  Result:=FetchUserById(user_id,tuser);
  user:=tuser;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchGroup(const group: TFRE_DB_String; const domain_id:TFRE_DB_GUID; var ug: TFRE_DB_GROUP): TFRE_DB_Errortype;
begin //nl
  if _FetchGroup(group,domain_id,ug) then
    exit(edb_OK);
  result := edb_NOT_FOUND;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchGroupI(const group: TFRE_DB_String; const domain_id:TFRE_DB_GUID; var ug: IFRE_DB_GROUP): TFRE_DB_Errortype;
var // nl
  tug: TFRE_DB_GROUP;
begin
  Result:=FetchGroup(group,domain_id,tug);
  ug:=tug;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchGroupById(const group_id: TFRE_DB_GUID; var ug: TFRE_DB_GROUP; without_right_check: boolean): TFRE_DB_Errortype;
begin // nolock
  result := Fetch(group_id,TFRE_DB_Object(ug),without_right_check);
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchGroupByIdI(const group_id: TFRE_DB_GUID; var ug: IFRE_DB_GROUP): TFRE_DB_Errortype;
var  // nolock req
  tug: TFRE_DB_GROUP;
begin
  Result:=FetchGroupById(group_id,tug);
  ug:=tug;
end;

function TFRE_DB_SYSTEM_CONNECTION.ModifyGroupById(const group_id: TFRE_DB_GUID; const groupname: TFRE_DB_NameType; const txt, txt_short: TFRE_DB_String): TFRE_DB_Errortype;
var
  tgroup: TFRE_DB_GROUP;
begin
  Result:=FetchGroupById(group_id,tgroup);
  if Result=edb_OK then
    begin
      if groupname<>cFRE_DB_SYS_NOCHANGE_VAL_STR then
        tgroup.ObjectName            := groupname;
      if txt_short<>cFRE_DB_SYS_NOCHANGE_VAL_STR then
        tgroup.Description.ShortText := txt_short;
      if txt<>cFRE_DB_SYS_NOCHANGE_VAL_STR then
        tgroup.Description.LongText  := txt;
      result := Update(tgroup);
    end;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchRole(const rolename:TFRE_DB_String;const domainUID: TFRE_DB_GUID; var role: TFRE_DB_ROLE): TFRE_DB_Errortype;
begin // nolock req
  if FSysRoles.GetIndexedObjTextCore(TFRE_DB_ROLE.GetDomainRoleKey(rolename,domainUID),TFRE_DB_Object(role))>0 then
    result := edb_OK
  else
    result := edb_NOT_FOUND;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchRoleI(const rolename:TFRE_DB_String;const domainUID: TFRE_DB_GUID; var role: IFRE_DB_ROLE): TFRE_DB_Errortype;
var roleo : TFRE_DB_ROLE;
begin  // nolock req
  result := FetchRole(rolename,domainUID,roleo);
  if result = edb_OK then
    role := roleo
  else
    role := nil;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchRoleById(const role_id: TFRE_DB_GUID; var role: TFRE_DB_ROLE ; const without_right_check:boolean=false): TFRE_DB_Errortype;
begin  //noclcok
  result := Fetch(role_id,TFRE_DB_Object(role),without_right_check);
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchRoleByIdI(const role_id: TFRE_DB_GUID; var role: IFRE_DB_ROLE): TFRE_DB_Errortype;
var
  trole: TFRE_DB_ROLE; //nolcock req
begin
  Result:=FetchRoleById(role_id,trole);
  role:=trole;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchAllDomainUids: TFRE_DB_GUIDArray;
begin
  FSysDomains.GetAllUidsNoRC(result);
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchDomainById(const domain_id: TFRE_DB_GUID; var domain: TFRE_DB_DOMAIN): TFRE_DB_Errortype;
begin //nolock
  result := Fetch(domain_id,TFRE_DB_Object(domain));
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchDomainByIdI(const domain_id: TFRE_DB_GUID; var domain: IFRE_DB_DOMAIN): TFRE_DB_Errortype;
var
  tdomain: TFRE_DB_DOMAIN; //nolock required
begin
  Result:=FetchDomainById(domain_id,tdomain);
  domain:=tdomain;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchDomainNameById(const domain_id: TFRE_DB_GUID): TFRE_DB_NameType;
var dom : TFRE_DB_DOMAIN;
begin
   CheckDbResult(Fetch(domain_id,TFRE_DB_Object(dom),true));
   result := dom.Domainname(false);
   dom.Finalize;
end;

function TFRE_DB_SYSTEM_CONNECTION.ModifyDomainById(const domain_id: TFRE_DB_GUID; const domainname: TFRE_DB_NameType; const txt, txt_short: TFRE_DB_String): TFRE_DB_Errortype;
var
  tdomain: TFRE_DB_DOMAIN;
begin
  Result:=FetchDomainById(domain_id,tdomain);
  if Result=edb_OK then
    begin
      if domainname<>cFRE_DB_SYS_NOCHANGE_VAL_STR then
        tdomain.ObjectName            := domainname;
      if txt_short<>cFRE_DB_SYS_NOCHANGE_VAL_STR then
        tdomain.Description.ShortText := txt_short;
      if txt<>cFRE_DB_SYS_NOCHANGE_VAL_STR then
        tdomain.Description.LongText  := txt;
      result := Update(tdomain);
    end;
end;

function TFRE_DB_SYSTEM_CONNECTION.SuspendContinueDomainById(const domain_id: TFRE_DB_GUID; const suspend: boolean): TFRE_DB_Errortype;
var domain : TFRE_DB_DOMAIN;
begin
  result := FetchDomainById(domain_id,domain);
  if result <> edb_OK then
    exit;
  domain.Suspended := suspend;
  result := Update(domain);
end;

function TFRE_DB_SYSTEM_CONNECTION.IsDomainSuspended(const domainname: TFRE_DB_NameType): boolean;
var domain : TFRE_DB_DOMAIN;
begin
  result := false;
  if FetchDomain(domainname,domain) then
    begin
      result := domain.Suspended;
      domain.Finalize;
    end;
end;

function TFRE_DB_SYSTEM_CONNECTION.DeleteDomainById(const domain_id: TFRE_DB_GUID): TFRE_DB_Errortype;
begin
  if DomainID(CFRE_DB_SYS_DOMAIN_NAME)=domain_id then
    raise EFRE_DB_Exception.Create('Deletion of the system domain is not allowed !');
  result := FSysDomains.Remove(domain_id);
end;


function TFRE_DB_SYSTEM_CONNECTION.DomainExists(const domainname: TFRE_DB_NameType): boolean;
begin
  result := FSysDomains.ExistsIndexed(domainname);
end;

function TFRE_DB_SYSTEM_CONNECTION.DomainID(const domainname: TFRE_DB_NameType): TFRE_DB_GUID;
begin
  if not FSysDomains.GetIndexedUIDText(domainname,result)=1 then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'cant fetch a guid for Domain '+domainname+' ?');
end;

function TFRE_DB_SYSTEM_CONNECTION.DeleteDomain(const domainname: TFRE_DB_Nametype): TFRE_DB_Errortype;
begin
  if FSysDomains.RemoveIndexedText(domainname)>0 then
    result := edb_OK
  else
    result := edb_ERROR;
end;

function TFRE_DB_SYSTEM_CONNECTION.IsSystemGroup(const group_id: TFRE_DB_GUID): boolean;
var tgroup: TFRE_DB_GROUP;
begin
  FetchGroupById(group_id,tgroup);
  result := (tgroup.GetDomainIDLink=FSysDomainUID);
end;

procedure TFRE_DB_SYSTEM_CONNECTION.ForAllDomainsI(const func: IFRE_DB_Domain_Iterator);

  procedure myfunc(const domo : TFRE_DB_Object);
  begin
    func(domo as TFRE_DB_DOMAIN);
  end;

begin
  FSysDomains.ForAll(@myfunc);
end;


function TFRE_DB_SYSTEM_CONNECTION.FetchUserSessionData(var SessionData: IFRE_DB_OBJECT): boolean;
var domlogkey : TFRE_DB_String;
begin
  //TODO Errorhandling
  Sessiondata := nil;
  domlogkey   := FCurrentUserToken.GetDomainLoginKey; // uppercase(FCurrentUserToken.User.Login+'@'+FREDB_G2H(FCurrentUserToken.User.DomainID));
  result      := FSysUserSessionsData.GetIndexedObjI(domlogkey,SessionData);
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreUserSessionData(var session_data: IFRE_DB_Object):TFRE_DB_Errortype;
var key    : TFRE_DB_String;
    userid : string;
begin
  result := edb_OK;
  userid := FCurrentUserToken.GetDomainLoginKey; //uppercase(FCurrentUserToken.User.Login+'@'+FREDB_G2H(FCurrentUserToken.User.DomainID));
  key    := userid;
  session_data.Field('$LOGIN_KEY').AsString := userid;
  if FSysUserSessionsData.ExistsIndexed(key) then
    begin
    //writeln('TRY UPDATETE SESSIONDATA : ',userid ,'  ',session_data.DumpToString());
      G_DEBUG_TRIGGER_1:=true;
      result := FSysUserSessionsData.UpdateI(session_data);
     //writeln('TRYUPDATE OK');
  end else begin
     result := FSysUserSessionsData.StoreI(session_data);
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.NewRole(const rolename, txt, txt_short: TFRE_DB_String;const is_internal:Boolean; var role: TFRE_DB_ROLE): TFRE_DB_Errortype;
begin  // NoLock necessary
  role := GFRE_DB._NewRole(rolename,txt,txt_short,is_internal);
  result:=edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION.NewGroup(const groupname, txt, txt_short: TFRE_DB_String;const is_protected:Boolean;const is_internal:Boolean; var user_group: TFRE_DB_GROUP): TFRE_DB_Errortype;
begin // NoLock necessary
  user_group := GFRE_DB._NewGroup(groupname,txt,txt_short,is_protected,is_internal);
  result:=edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION.AddRoleRightsToRole(const rolename: TFRE_DB_String; const domainUID: TFRE_DB_GUID; const roles: TFRE_DB_StringArray): TFRE_DB_Errortype;
var
  role,source_role: TFRE_DB_ROLE;
  i               : Integer;
begin
  Result:=FetchRole(rolename,domainUID,role);
  if Result<>edb_OK then exit;

  for i := 0 to High(roles) do begin
    Result:=FetchRole(roles[i],domainUID,source_role);
    if Result<>edb_OK then exit;
    role.AddRightsFromRole(source_role);
  end;
  Result:=Update(role);
end;

function TFRE_DB_SYSTEM_CONNECTION.RemoveRightsFromRole(const rolename: TFRE_DB_String; const rights: TFRE_DB_StringArray; const domainUID: TFRE_DB_GUID): TFRE_DB_Errortype;
var
  role: TFRE_DB_ROLE;
begin
  Result:=FetchRole(rolename,domainUID,role);
  if Result<>edb_OK then exit;

  role.RemoveRights(rights);
  Result:=Update(role);
end;

function TFRE_DB_SYSTEM_CONNECTION.AddRole(const rolename, txt, txt_short: TFRE_DB_String; const domainUID: TFRE_DB_GUID; const is_internal: Boolean): TFRE_DB_Errortype;
var role : IFRE_DB_ROLE;
begin
  result := NewRoleI(rolename,txt,txt_short,is_internal,role);
  if result<>edb_OK then
    exit(result);
  result := StoreRoleI(role,domainUID);
end;

function TFRE_DB_SYSTEM_CONNECTION.AddRolesToGroupById(const group: TFRE_DB_String; const domainUID: TFRE_DB_GUID; const role_ids: TFRE_DB_GUIDArray): TFRE_DB_Errortype;
var l_Group            : TFRE_DB_GROUP;
    l_NewRolesID       : TFRE_DB_GUIDArray;
    l_AggregatedRoleID : TFRE_DB_GUIDArray;
    l_FetchedRoleUID   : TFRE_DB_GUID;
    i                  : NativeInt;
    j                  : NativeInt;
    allready_in        : boolean;
    role               : TFRE_DB_ROLE;
begin
  if not _FetchGroup(group,domainUID,l_group) then
    exit(edb_NOT_FOUND);
  l_NewRolesID:=role_ids;
  l_AggregatedRoleID   := l_Group.RoleIDs;
  for i:=0 to high(l_NewRolesID) do begin
    allready_in := false;
    for j:=0 to high(l_AggregatedRoleID) do begin
      if l_NewRolesID[i]=l_AggregatedRoleID[j] then begin
        allready_in := true;
        break;
      end;
    end;
    if not allready_in then begin
      //check right
      FetchRoleById(l_NewRolesID[i],role);
      if not CheckClassRight4DomainId('assignRole',TFRE_DB_ROLE,role.DomainID) then
        exit(edb_ACCESS);
      setLength(l_AggregatedRoleID,length(l_AggregatedRoleID)+1);
      l_AggregatedRoleID[high(l_AggregatedRoleID)] := l_NewRolesID[i];
    end;
  end;
  l_Group.RoleIDs := l_AggregatedRoleID;
  result := Update(l_Group);
end;

function TFRE_DB_SYSTEM_CONNECTION.AddRolesToGroup(const group: TFRE_DB_String; const domainUID: TFRE_DB_GUID; const roles: TFRE_DB_StringArray): TFRE_DB_Errortype;
begin
  if not CheckClassRight4DomainId('assignRole',TFRE_DB_ROLE,domainUID) then
    exit(edb_ACCESS);
  Result:=_AddRolesToGroup(group,domainUID,roles,domainUID);
end;

function TFRE_DB_SYSTEM_CONNECTION.AddSysRolesToGroup(const group: TFRE_DB_String; const domainUID: TFRE_DB_GUID; const roles: TFRE_DB_StringArray): TFRE_DB_Errortype;
begin
  if not CheckClassRight4Domain('assignRole',TFRE_DB_ROLE,CFRE_DB_SYS_DOMAIN_NAME) then
    exit(edb_ACCESS);
  Result:=_AddRolesToGroup(group,domainUID,roles,GetSysDomainUID);
end;

function TFRE_DB_SYSTEM_CONNECTION.AddGroup(const groupname, txt, txt_short: TFRE_DB_String; const domainUID: TFRE_DB_GUID; const is_protected:Boolean;const is_internal:Boolean): TFRE_DB_Errortype;
var group : IFRE_DB_GROUP;
begin
  result := NewGroupI(FREDB_HCV(groupname),FREDB_HCV(txt),FREDB_HCV(txt_short),is_protected,is_internal,group);
  if result<>edb_OK then
    exit(result);
  result := StoreGroupI(group,domainUID);
end;

function TFRE_DB_SYSTEM_CONNECTION.RemoveRolesFromGroupById(const group: TFRE_DB_String; const domainUID: TFRE_DB_GUID; const role_ids: TFRE_DB_GUIDArray; const ignore_not_set: boolean): TFRE_DB_Errortype;
var l_Group            : TFRE_DB_GROUP;
   l_DelRolesID       : TFRE_DB_GUIDArray;
   l_ReducedRoleID    : TFRE_DB_GUIDArray;
   l_CopyRoleID       : TFRE_DB_GUIDArray;
   l_FetchedRoleUID   : TFRE_DB_GUID;
   i                  : NativeInt;
   j                  : NativeInt;
   l_found            : boolean;
   l_remove_count     : NativeInt;
   role               : TFRE_DB_ROLE;
begin
  if not _FetchGroup(group,domainUID,l_group) then exit(edb_NOT_FOUND);
  l_DelRolesID:=role_ids;
  l_ReducedRoleID   := l_Group.RoleIDs;
  l_remove_count    := 0;
  for i:=0 to high(l_DelRolesID) do begin
    l_found := false;
    for j:=0 to high(l_ReducedRoleID) do begin
      if l_DelRolesID[i]=l_ReducedRoleID[j] then begin
        l_found              := true;
        l_ReducedRoleID[j] := CFRE_DB_NullGUID;
        inc(l_remove_count);
        break;
      end;
   end;
   if not l_found then begin
     if not ignore_not_set then begin
       exit(edb_NOT_FOUND);
     end;
   end;
  end;
  setlength(l_CopyRoleID,length(l_ReducedRoleID)-l_remove_count);
  j:=0;
  for i:=0 to high(l_ReducedRoleID) do begin
    if l_ReducedRoleID[i]<>CFRE_DB_NullGUID then begin
      //check right
      FetchRoleById(l_ReducedRoleID[i],role);
      if not CheckClassRight4DomainId('assignRole',TFRE_DB_ROLE,role.DomainID) then
        exit(edb_ACCESS);
      l_CopyRoleID[j] := l_ReducedRoleID[i];
      inc(j);
    end;
  end;
  l_Group.RoleIDs := l_CopyRoleID;
  result := Update(l_Group);
end;

function TFRE_DB_SYSTEM_CONNECTION.RemoveRolesFromGroup(const group: TFRE_DB_String; const domainUID: TFRE_DB_GUID; const roles: TFRE_DB_StringArray; const ignore_not_set: boolean): TFRE_DB_Errortype;
var l_Group            : TFRE_DB_GROUP;
   l_DelRoles         : TFRE_DB_StringArray;
   l_DelRolesID       : TFRE_DB_GUIDArray;
   l_ReducedRoleID    : TFRE_DB_GUIDArray;
   l_CopyRoleID       : TFRE_DB_GUIDArray;
   l_FetchedRoleUID   : TFRE_DB_GUID;
   i                  : NativeInt;
   j                  : NativeInt;
   l_found            : boolean;
   l_remove_count     : NativeInt;
begin
  if not CheckClassRight4DomainId('assignRole',TFRE_DB_ROLE,domainUID) then
    exit(edb_ACCESS);
  if not _FetchGroup(group,domainUID,l_group) then exit(edb_NOT_FOUND);
  l_DelRoles        := roles;
  setLength(l_DelRolesID,length(l_DelRoles));
  for i:=0 to high(l_DelRoles) do begin
    if not _RoleID(l_DelRoles[i],domainUID,l_FetchedRoleUID) then exit(edb_NOT_FOUND);
    l_DelRolesID[i] :=l_FetchedRoleUID;
  end;
  l_ReducedRoleID   := l_Group.RoleIDs;
  l_remove_count    := 0;
  for i:=0 to high(l_DelRolesID) do begin
    l_found := false;
    for j:=0 to high(l_ReducedRoleID) do begin
      if l_DelRolesID[i]=l_ReducedRoleID[j] then begin
        l_found              := true;
        l_ReducedRoleID[j] := CFRE_DB_NullGUID;
        inc(l_remove_count);
        break;
      end;
   end;
   if not l_found then begin
     if not ignore_not_set then begin
       exit(edb_NOT_FOUND);
     end;
   end;
  end;
  setlength(l_CopyRoleID,length(l_ReducedRoleID)-l_remove_count);
  j:=0;
  for i:=0 to high(l_ReducedRoleID) do begin
    if l_ReducedRoleID[i]<>CFRE_DB_NullGUID then begin
      l_CopyRoleID[j] := l_ReducedRoleID[i];
      inc(j);
    end;
  end;
  l_Group.RoleIDs := l_CopyRoleID;
  result := Update(l_Group);
end;

function TFRE_DB_SYSTEM_CONNECTION.RemoveAllRolesFromGroup(const group: TFRE_DB_String; const domainUID: TFRE_DB_GUID): TFRE_DB_Errortype;
var
  l_Group : TFRE_DB_GROUP;
  i       : Integer;
  role    : TFRE_DB_ROLE;
begin
  if not _FetchGroup(group,domainUID,l_group) then exit(edb_NOT_FOUND);

  //check right
  for i := 0 to High(l_Group.RoleIDs) do begin
    FetchRoleById(l_Group.RoleIDs[i],role);
    if not CheckClassRight4DomainId('assignRole',TFRE_DB_ROLE,role.DomainID) then
      exit(edb_ACCESS);
  end;

  l_Group.RoleIDs := TFRE_DB_GUIDArray.create;
  result := Update(l_Group);
end;

function TFRE_DB_SYSTEM_CONNECTION.RemoveRoleFromAllGroups(const role: TFRE_DB_String; const domainUID: TFRE_DB_GUID): TFRE_DB_Errortype;
var
  roleObj  : TFRE_DB_ROLE;
  groupIds : TFRE_DB_GUIDArray;
  i        : Integer;
  group    : TFRE_DB_GROUP;
begin
  if not CheckClassRight4DomainId('assignRole',TFRE_DB_ROLE,domainUID) then
    exit(edb_ACCESS);

  FetchRole(role,domainUID,roleObj);
  groupIds:=GetReferences(roleObj.UID,false,'TFRE_DB_GROUP');
  for i := 0 to High(groupIds) do begin
    Result:=FetchGroupById(groupIds[i],group);
    if result<>edb_OK then
      exit;
    group.Field('roleids').RemoveObjectLinkByUID(roleObj.UID);
    Result:=Update(group);
    if result<>edb_OK then
      exit;
  end;
  Result := edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION.ModifyUserGroupsById(const user_id:TFRE_DB_GUID; const user_group_ids:TFRE_DB_GUIDArray; const keep_existing_groups: boolean): TFRE_DB_Errortype;
var l_User            : TFRE_DB_USER;
   l_OldGroups       : TFRE_DB_GUIDArray;
   l_NewGroups       : TFRE_DB_GUIDArray;
   i                 : integer;
   j                 : integer;
   old_group_count   : integer;
   new_group_count   : integer;
   already_in        : boolean;
   new_group_id      : TFRE_DB_GUID;
   loginname         : TFRE_DB_String;
   group             : TFRE_DB_GROUP;

begin
  result := FetchUserById(user_id,l_user);
  if result<>edb_OK then
    exit;
  l_OldGroups     := l_User.GetUserGroupIDS;
  old_group_count := Length(l_oldGroups);
  if keep_existing_groups then begin
   SetLength(l_NewGroups,old_group_count+Length(user_group_ids));
   for i:=0 to high(l_OldGroups) do begin
     l_NewGroups[i]    := l_OldGroups[i];
   end;
   new_group_count     := old_group_count;
   for i:=0 to high(user_group_ids) do begin
     already_in := false;
     for j := 0 to new_group_count-1 do begin
       if l_NewGroups[j]=user_group_ids[i] then begin
         already_in := true;
         break;
       end;
     end;
     if already_in=false then begin
      //check right
       FetchGroupById(user_group_ids[i],group);
       if not CheckClassRight4DomainId('assignGroup',TFRE_DB_GROUP,group.DomainID) then
         exit(edb_ACCESS);
       l_NewGroups[new_group_count]    := user_group_ids[i];
       inc(new_group_count);
     end;
   end;
   SetLength(l_NewGroups,new_group_count);
  end else begin
   new_group_count     := Length(user_group_ids);
   SetLength(l_NewGroups,new_group_count);
   for i := 0 to high(user_group_ids) do begin
     FetchGroupById(user_group_ids[i],group);
     if not CheckClassRight4DomainId('assignGroup',TFRE_DB_GROUP,group.DomainID) then
       exit(edb_ACCESS);
   end;
   l_NewGroups         := user_group_ids;
  end;
  if FREDB_Guid_ArraysSame(l_User.UserGroupIDs,l_NewGroups) then
   begin
     l_User.Finalize;
     exit(edb_NO_CHANGE);
   end;
  l_User.UserGroupIDs   := l_NewGroups;
  loginname:=l_user.GetLogin;
  result:=Update(l_User);
  CheckDbResultFmt(Result,'could not update user %s',[loginname]);
  Result := edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION.RemoveUserGroupsById(const user_id:TFRE_DB_GUID; const user_group_ids:TFRE_DB_GUIDArray): TFRE_DB_Errortype;
var l_User            : TFRE_DB_USER;
    l_OldGroups       : TFRE_DB_GUIDArray;
    l_NewGroups       : TFRE_DB_GUIDArray;
    i                 : integer;
    j                 : integer;
    old_group_count   : integer;
    new_group_count   : integer;
    loginname         : TFRE_DB_String;
    group             : TFRE_DB_GROUP;
begin
  result := FetchUserById(user_id,l_User);
  if result<>edb_OK then
    exit;
  l_OldGroups           := l_User.GetUserGroupIDS;
  old_group_count       := Length(l_oldGroups);
  new_group_count       := old_group_count;
  for i:=0 to high(user_group_ids) do
    FetchGroupById(user_group_ids[i],group);
    if not CheckClassRight4DomainId('assignGroup',TFRE_DB_GROUP,group.DomainID) then
      exit(edb_ACCESS);
    for j := 0 to old_group_count-1 do
      if l_OldGroups[j]=user_group_ids[i] then
        begin
          l_OldGroups[j] := CFRE_DB_NullGUID;
          dec(new_group_count);
          break;
        end;
  SetLength(l_NewGroups,new_group_count);
  j:=0;
  for i:=0 to old_group_count-1 do
    if l_oldGroups[i]<>CFRE_DB_NullGUID then
      begin
        l_NewGroups[j] := l_oldGroups[i];
        inc(j);
      end;
  l_User.UserGroupIDs   := l_NewGroups;
  loginname:=l_user.GetLogin;
  result:=Update(l_User);
  CheckDbResultFmt(Result,'could not update user %s',[loginname]);
  Result := edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION.ModifyUserPassword(const login:TFRE_DB_String; const domainUID: TFRE_DB_GUID;const oldpassword, newpassword: TFRE_DB_String): TFRE_DB_Errortype;
var l_User:TFRE_DB_USER;
begin
  result := FetchUser(login,domainUID,l_User);
  if result<>edb_OK then exit;
  if not l_User.Checkpassword(oldpassword) then
    exit(edb_ACCESS);
  l_User.SetPassword(newpassword);
  Update(l_User);
end;

function TFRE_DB_SYSTEM_CONNECTION.RoleExists(const role:TFRE_DB_String;const domainUID: TFRE_DB_GUID): boolean;
begin
  result := FSysRoles.ExistsIndexed(TFRE_DB_ROLE.GetDomainRoleKey(role,domainUID));
end;

function TFRE_DB_SYSTEM_CONNECTION.GroupExists(const group:TFRE_DB_String;const domainUID: TFRE_DB_GUID): boolean;
begin
  result := FSysGroups.ExistsIndexed(TFRE_DB_GROUP.GetDomainGroupKey(group,domainUID));
end;

function TFRE_DB_SYSTEM_CONNECTION.DeleteGroup(const group:TFRE_DB_String;const domainUID: TFRE_DB_GUID): TFRE_DB_Errortype;
begin
  if not CheckClassRight4DomainId(sr_DELETE,TFRE_DB_GROUP,domainUID) then
    exit(edb_ACCESS);

  if FSysGroups.RemoveIndexedText(TFRE_DB_GROUP.GetDomainGroupKey(group,domainUID))>0 then begin
    result := edb_OK
  end else begin
    result := edb_ERROR;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.DeleteGroupById(const group_id: TFRE_DB_GUID): TFRE_DB_Errortype;
begin
  Result := FSysGroups.Remove(group_id);
end;

function TFRE_DB_SYSTEM_CONNECTION.DeleteRole(const role:TFRE_DB_String;const domainUID: TFRE_DB_GUID): TFRE_DB_Errortype;
begin
  if FSysRoles.RemoveIndexedText(TFRE_DB_ROLE.GetDomainRoleKey(role,domainUID))>0 then begin
    result := edb_OK;
  end else begin
    result := edb_ERROR;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreRole(var role: TFRE_DB_ROLE; const domainUID: TFRE_DB_GUID): TFRE_DB_Errortype;
begin
  Result:=edb_ERROR;
  role.SetDomainID(domainUID);
  role.SetDomainIDLink(domainUID);
  result :=FSysRoles.Store(TFRE_DB_Object(role));
end;

function TFRE_DB_SYSTEM_CONNECTION.UpdateRole(var role: TFRE_DB_ROLE): TFRE_DB_Errortype;
begin
 result := FSysRoles.Update(TFRE_DB_Object(role));
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreGroup(const domain_id: TFRE_DB_GUID; var group: TFRE_DB_GROUP): TFRE_DB_Errortype;
begin
  group.SetDomainID(domain_id);
  group.SetDomainIDLink(domain_id);
  result := FSysGroups.Store(TFRE_DB_Object(group));
end;

function TFRE_DB_SYSTEM_CONNECTION.UpdateGroup(var group: TFRE_DB_GROUP): TFRE_DB_Errortype;
begin
 result := FSysGroups.Update(TFRE_DB_Object(group));
end;

function TFRE_DB_SYSTEM_CONNECTION.UpdateDomain(var domain: TFRE_DB_DOMAIN): TFRE_DB_Errortype;
begin
 result := FSysDomains.Update(TFRE_DB_Object(domain));
end;

function TFRE_DB_SYSTEM_CONNECTION.UpdateUser(var user: TFRE_DB_USER): TFRE_DB_Errortype;
begin
 result := FSysUsers.Update(TFRE_DB_Object(user));
end;


function TFRE_DB_SYSTEM_CONNECTION.StoreTranslateableText(var txt: TFRE_DB_TEXT): TFRE_DB_Errortype;
begin
  result := FSysTransText.Store(TFRE_DB_Object(txt));
end;

function TFRE_DB_SYSTEM_CONNECTION.UpdateTranslateableText(const txt: TFRE_DB_TEXT): TFRE_DB_Errortype;
var ttext : TFRE_DB_TEXT;
    key   : TFRE_DB_String;
begin
  key := uppercase(txt.GetTKey);
  if FSysTransText.GetIndexedObjTextCore(key,TFRE_DB_Object(ttext),true)>0 then begin
    ttext.SetupText(key,txt.Getshort,txt.GetLong,txt.GetHint);
    txt.Finalize;
    result := FSysTransText.Update(ttext);
  end else begin
    result := edb_NOT_FOUND;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.UpdateTranslateableTextI(const txt: IFRE_DB_TEXT): TFRE_DB_Errortype;
begin
  result := UpdateTranslateableText(txt.Implementor_HC as TFRE_DB_TEXT);
end;

function TFRE_DB_SYSTEM_CONNECTION.DeleteTranslateableText(const key: TFRE_DB_String): TFRE_DB_Errortype;
begin
  if FSysTransText.RemoveIndexedText(key)>0 then
    Result := edb_OK
  else
    result := edb_NOT_FOUND;
end;

function TFRE_DB_SYSTEM_CONNECTION.CheckClassRight4MyDomain(const right_name: TFRE_DB_String; const classtyp: TClass): boolean;
begin
  result := FCurrentUserToken.CheckClassRight4MyDomain(right_name,classtyp);
end;

function TFRE_DB_SYSTEM_CONNECTION.CheckClassRight4MyDomain(const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass): boolean;
begin
  result := FCurrentUserToken.CheckClassRight4MyDomain(std_right,classtyp);   //self.fcurrentusertoken.FLoginAtDomain
end;

function TFRE_DB_SYSTEM_CONNECTION.CheckClassRight4AnyDomain(const right_name: TFRE_DB_String; const classtyp: TClass): boolean;
begin
  result := FCurrentUserToken.CheckClassRight4AnyDomain(right_name,classtyp);
end;

function TFRE_DB_SYSTEM_CONNECTION.CheckClassRight4Domain(const right_name: TFRE_DB_String; const classtyp: TClass; const domainKey: TFRE_DB_String): boolean;
begin
  result := FCurrentUserToken.CheckClassRight4Domain(right_name,classtyp,domainKey);
end;

function TFRE_DB_SYSTEM_CONNECTION.CheckClassRight4DomainId(const right_name: TFRE_DB_String; const classtyp: TClass; const domain: TFRE_DB_GUID): boolean;
begin
  result := FCurrentUserToken.CheckClassRight4DomainId(right_name,classtyp,domain);
end;

function TFRE_DB_SYSTEM_CONNECTION.GetDomainsForRight(const right_name: TFRE_DB_String): TFRE_DB_GUIDArray;
begin
  result := FCurrentUserToken.GetDomainsForRight(right_name);
end;

function TFRE_DB_SYSTEM_CONNECTION.GetDomainsForClassRight(const right_name: TFRE_DB_String; const classtyp: TClass): TFRE_DB_GUIDArray;
begin
  result := FCurrentUserToken.GetDomainsForClassRight(right_name,classtyp);
end;

function TFRE_DB_SYSTEM_CONNECTION.CheckClassRight4AnyDomain(const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass): boolean;
begin
  result := FCurrentUserToken.CheckClassRight4AnyDomain(std_right,classtyp);
end;

function TFRE_DB_SYSTEM_CONNECTION.CheckClassRight4Domain(const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass; const domainKey: TFRE_DB_String): boolean;
begin
  result := FCurrentUserToken.CheckClassRight4Domain(std_right,classtyp,domainKey);
end;

function TFRE_DB_SYSTEM_CONNECTION.CheckClassRight4DomainId(const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass; const domain: TFRE_DB_GUID): boolean;
begin
  result := FCurrentUserToken.CheckClassRight4DomainId(std_right,classtyp,domain);
end;

function TFRE_DB_SYSTEM_CONNECTION.GetDomainsForClassRight(const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass): TFRE_DB_GUIDArray;
begin
  result := FCurrentUserToken.GetDomainsForClassRight(std_right,classtyp);
end;

function TFRE_DB_SYSTEM_CONNECTION.GetDomainNamesForClassRight(const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass): TFRE_DB_StringArray;
begin
  result := FCurrentUserToken.GetDomainNamesForClassRight(std_right,classtyp);
end;

function TFRE_DB_SYSTEM_CONNECTION.CheckObjectRight(const right_name: TFRE_DB_String; const uid: TFRE_DB_GUID): boolean;
begin
  result := FCurrentUserToken.CheckObjectRight(right_name,uid);
end;

function TFRE_DB_SYSTEM_CONNECTION.CheckObjectRight(const std_right: TFRE_DB_STANDARD_RIGHT; const uid: TFRE_DB_GUID): boolean;
begin
  result := FCurrentUserToken.CheckObjectRight(std_right,uid);
end;


function TFRE_DB_SYSTEM_CONNECTION.FetchTranslateableText(const trans_key: TFRE_DB_String; var ttext: TFRE_DB_TEXT): TFRE_DB_Errortype;
begin
  if FSysTransText.GetIndexedObjTextCore(uppercase(trans_key),TFRE_DB_Object(ttext),true)>0 then begin
    result := edb_OK;
  end else begin
    result := edb_NOT_FOUND;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchTranslateableTextI(const translation_key: TFRE_DB_String; var textObj: IFRE_DB_TEXT): Boolean;
var txt : TFRE_DB_TEXT;
begin
  if FetchTranslateableText(translation_key,txt)=edb_OK then begin
    textObj:=txt;
    result := true;
  end else begin
    textObj:=nil;
    result := false;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.BackupDatabaseReadable(const sys, adb: TStream; const progress: TFRE_DB_PhaseProgressCallback): TFRE_DB_Errortype;
begin
  if not Assigned(FPairedAppDBConn) and assigned(adb) then
    exit(edb_ERROR);
  if assigned(sys) then
    inherited BackupDatabaseReadable(sys,progress);
  if assigned(adb) then
    FPairedAppDBConn.BackupDatabaseReadable(adb,progress);
  result := edb_OK;
end;

function TFRE_DB_BASE_CONNECTION.BackupDatabaseReadable(const str: TStream; const progress: TFRE_DB_PhaseProgressCallback): TFRE_DB_Errortype;
var CTRL_OBJ:TFRE_DB_Object;
    cnt          : integer;
    max          : integer;
    final        : boolean;
    obj_progress : boolean;

  procedure DWriteln(const msg:TFRE_DB_String);
   var line  : TFRE_DB_String;
       len   : integer;
       lens  : TFRE_DB_String;
   begin
     line := msg+#13#10;
     len  := Length(line);
     lens := IntToStr(len)+'C';
     str.Write(Pointer(@lens[1])^,Length(lens));
     str.Write(Pointer(@line[1])^,Length(line));
   end;

   procedure DumpObj(const obj:IFRE_DB_Object ; var break : boolean);
   begin
     if obj_progress then
       inc(cnt);
     DWriteln(obj.GetAsJSONString(false,true));
     if Assigned(progress) and obj_progress then
       progress('Object Backup',obj.GetDescriptionID,'',cnt,max);
     obj.Finalize;
   end;

   procedure DumpColl(const obj:IFRE_DB_Object);
   begin
     if obj_progress then
       inc(cnt);
     DWriteln(obj.GetAsJSONString(true,true));
     if Assigned(progress) and obj_progress then
       progress('Collection Backup',obj.Field('CollectionName').AsString,'',cnt,max);
     obj.Finalize;
   end;


   procedure SetNewSection(const section_name:TFRE_DB_String;element_count:qword);
   begin
     CTRL_OBJ.ClearAllFields;
     CTRL_OBJ.Field('SECTION').AsString := section_name;
     CTRL_OBJ.Field('COUNT').AsUInt64   := element_count;
     DWriteln(CTRL_OBJ.GetAsJSONString(true,true));
   end;

   procedure SetHeader;
   begin
    CTRL_OBJ.ClearAllFields;
    CTRL_OBJ.Field('ID').AsString      := 'FirmOS FDB Backup';
    CTRL_OBJ.Field('DBNAME').AsString  := uppercase(FDBName);
    CTRL_OBJ.Field('VMAJ').AsInt16     := 0;
    CTRL_OBJ.Field('VMIN').AsInt16     := 1;
    CTRL_OBJ.Field('VBUILD').AsInt16   := 0;
    DWriteln(CTRL_OBJ.GetAsJSONString(true,true));
   end;

   procedure DumpCollection(const co:TFRE_DB_COLLECTION);
   begin
     CTRL_OBJ.ClearAllFields;
     CTRL_OBJ.Field('SECTION').AsString := co.CollectionName;
     CTRL_OBJ.Field('COUNT').AsUInt64   := 1;
     //DumpObj(co);
     inc(cnt);
     //writeln('P ',phase,'  ',cnt,'  ',max);
     if Assigned(progress) then
       progress('Collection Backup','','',cnt,max);
   end;

begin
  if FCloned then
    exit(edb_UNSUPPORTED);
  final := true;
  CTRL_OBJ := GFRE_DB.NewObject;
  SetHeader;
  obj_progress := true;
  max   := FPersistance_Layer.FDB_GetObjectCount(false); cnt   := 0;

  if Assigned(progress) then
    progress('','','>STARTING OBJECT BACKUP OF ['+FDBName+']',0,0);
  SetNewSection('DBOS',max);
  FPersistance_Layer.FDB_ForAllObjects(@DumpObj);
  if Assigned(progress) then
    progress('','','<DONE OBJECT BACKUP OF ['+FDBName+']',0,0);

  if Assigned(progress) then
    progress('','','>STARTING COLLECTION BACKUP OF ['+FDBName+']',0,0);
  max := FPersistance_Layer.FDB_GetObjectCount(true); cnt := 0;
  SetNewSection('COLLECTIONS',max);
  FPersistance_Layer.FDB_ForAllColls(@DumpColl);
  if Assigned(progress) then
    progress('','','<DONE COLLECTION BACKUP OF ['+FDBName+']',0,0);
  CTRL_OBJ.Free;
end;

function TFRE_DB_BASE_CONNECTION.RestoreDatabaseReadable(const from_stream: TStream; const progress: TFRE_DB_PhaseProgressCallback): TFRE_DB_Errortype; //Unicode Awareness
var jp           : TJSONParser;
    obj          : TFRE_DB_Object;
    section_name : TFRE_DB_String;
    count        : integer;
    i            : Integer;


  function ReadElement : TFRE_DB_Object;
  var line  : TFRE_DB_String;
      elem  : Byte;
      count : Integer;
      pos   : integer;
  begin
    pos := 1;
    repeat
      elem := from_stream.ReadByte;
      inc(pos);
      if char(elem)<>'C' then begin
        line := line + char(elem);
      end else break;
    until false;
    count := StrToInt(line);
    SetLength(line,count-2);
    from_stream.ReadBuffer(line[1],count-2);
    from_stream.ReadByte;from_stream.ReadByte;
    result := TFRE_DB_Object.CreateFromJSONString(line);
  end;

  procedure GetSection(const obj:TFRE_DB_Object;var name : TFRE_DB_String;var cnt : integer);
  begin
    name := obj.Field('SECTION').AsString;
    cnt  := obj.Field('COUNT').AsInt64;
  end;

begin
  FPersistance_Layer.FDB_PrepareDBRestore(0);
  obj := ReadElement;
  progress('START','',format('BACKUP ID [%s] VERSION %s.%s(%s) for Database [%s]',[obj.Field('ID').AsString,obj.Field('VMAJ').AsString,obj.Field('VMIN').AsString,obj.Field('VBUILD').AsString,obj.Field('DBNAME').AsString]),0,0);
  GetSection(ReadElement,section_name,count);
  if section_name<>'DBOS' then GFRE_BT.CriticalAbort('UNEXPECTED SECTION [%s] WANTED "DBOS"',[section_name]);
  for i:=0 to count-1 do begin
    obj := ReadElement;
    progress('READ DBO ',obj.GetDescriptionID,'',i+1,count);
    FPersistance_Layer.FDB_SendObject(obj);
  end;
  FPersistance_Layer.FDB_PrepareDBRestore(1);
  GetSection(ReadElement,section_name,count);
  if section_name<>'COLLECTIONS' then GFRE_BT.CriticalAbort('UNEXPECTED SECTION [%s] WANTED "COLLECTIONS"',[section_name]);
  for i:=0 to count-1 do begin
    obj:=ReadElement;
    progress('READ COLLECTIONS ',obj.Field('CollectionName').AsString,'',i+1,count);
    FPersistance_Layer.FDB_SendCollection(obj);
  end;
  progress('CHECK RESTORE','','',0,0);
  FPersistance_Layer.FDB_PrepareDBRestore(2);
  progress('CHECK RESTORE DONE','','',0,0);
  FPersistance_Layer.FDB_PrepareDBRestore(100);
  progress('','','DONE',0,0);
end;

function TFRE_DB_SYSTEM_CONNECTION.RestoreDatabaseReadable(const sys, adb: TStream; const db_name: string; const progress: TFRE_DB_PhaseProgressCallback): TFRE_DB_Errortype;
begin
  if not Assigned(FPairedAppDBConn) then
    exit(edb_ERROR);
  if assigned(sys) then
    begin
      inherited RestoreDatabaseReadable(sys,progress);
    end;
  if assigned(adb) then
    FPairedAppDBConn.RestoreDatabaseReadable(adb,progress);
  result := edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION.GetClassesVersionDirectory: IFRE_DB_Object;
begin
  if not FSysSingletons.GetIndexedObjI('CLASSVERSIONS',result) then
    result := GFRE_DBI.NewObject;
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreClassesVersionDirectory(const version_dbo: IFRE_DB_Object): TFRE_DB_Errortype;
begin
  if not FSysSingletons.ExistsIndexed('CLASSVERSIONS') then
    begin
      version_dbo.Field('singletonkey').asstring:='CLASSVERSIONS';
      result := FSysSingletons.StoreI(version_dbo);
    end
  else
    begin
      result := FSysSingletons.UpdateI(version_dbo);
    end;
end;

function TFRE_DB_SYSTEM_CONNECTION.DelClassesVersionDirectory: TFRE_DB_Errortype;
begin
  if FSysSingletons.ExistsIndexed('CLASSVERSIONS') then
    begin
      if FSysSingletons.RemoveIndexedText('CLASSVERSIONS')>0 then
        exit(edb_OK)
      else
        exit(edb_NOT_FOUND);
    end
  else
    result := edb_NOT_FOUND;
end;

procedure TFRE_DB_SYSTEM_CONNECTION.DrawScheme(const datastream: TStream; const classfile: string);
begin
  inherited DrawScheme(datastream, classfile);
end;

function TFRE_DB_SYSTEM_CONNECTION.DumpUserRights: TFRE_DB_String;
begin
  result := FCurrentUserToken.DumpUserRights;
end;

procedure TFRE_DB_SYSTEM_CONNECTION.StartTransaction(const trans_id: TFRE_DB_NameType; const trans_type: TFRE_DB_TRANSACTION_TYPE);
begin
  CheckDbResult(GFRE_DB_PS_LAYER.StartTransaction(trans_type,trans_id),'Could not start transaction '+trans_id);
end;

procedure TFRE_DB_SYSTEM_CONNECTION.Commit;
begin
  if not GFRE_DB_PS_LAYER.Commit then
    raise EFRE_DB_Exception.Create(edb_ERROR,'Could not commit transaction');
end;

procedure TFRE_DB_SYSTEM_CONNECTION.Rollback;
begin
  abort;
  //if not GFRE_DB_PS_LAYER.RollBack then
  //  raise EFRE_DB_Exception.Create(edb_ERROR,'Could not rollback transaction');
end;

function TFRE_DB_SYSTEM_CONNECTION.GetSysDomainUID: TFRE_DB_GUID;
begin
  result := FSysDomainUID;
end;

function TFRE_DB_SYSTEM_CONNECTION.GetUserUID: TFRE_DB_GUID;
begin
  result := FCurrentUserToken.GetUserUID;
end;

function TFRE_DB_SYSTEM_CONNECTION.GetUserUIDP: PFRE_DB_GUID;
begin
  if assigned(FCurrentUserToken) then
    result := FCurrentUserToken.GetUserUIDP
  else
    result := nil;
end;

function TFRE_DB_SYSTEM_CONNECTION.GetMyDomainID: TFRE_DB_GUID;
begin
  result := FCurrentUserToken.GetMyDomainID;
end;

procedure TFRE_DB_SYSTEM_CONNECTION.ReloadUserandRights(useruid: TFRE_DB_GUID);
var user : TFRE_DB_USER;
begin
  user := nil;
  if useruid=CFRE_DB_NullGUID then
    useruid := FCurrentUserToken.GetUserUID; // (FCurrentUserToken.User.Implementor as TFRE_DB_Object).CloneToNewObject as TFRE_DB_USER
  if FetchUserById(useruid,user)<>edb_OK then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'could not fetch userid on reload of user');
  if assigned(FCurrentUserToken) then
    FreeAndNil(FCurrentUserToken);
  FCurrentUserToken := ReNewCurrentUserToken(user).Implementor as TFRE_DB_USER_RIGHT_TOKEN;
end;

function TFRE_DB_SYSTEM_CONNECTION.APP: IFRE_DB_CONNECTION;
begin
  if assigned(FPairedAppDBConn) then
    exit(FPairedAppDBConn)
  else
    raise EFRE_DB_Exception.Create(edb_ERROR,'operation not valid on a system only connection');
end;

function TFRE_DB_SYSTEM_CONNECTION.GetCurrentUserTokenClone: IFRE_DB_USER_RIGHT_TOKEN;
begin
  result := FCurrentUserToken.CloneToNewUserToken;
end;

function TFRE_DB_SYSTEM_CONNECTION.GetCurrentUserTokenRef: IFRE_DB_USER_RIGHT_TOKEN;
begin
 result := FCurrentUserToken;
end;


function TFRE_DB_SYSTEM_CONNECTION._AddUser(const login: TFRE_DB_String; const domainUID: TFRE_DB_GUID; const password, first_name, last_name: TFRE_DB_String; const system_start_up: boolean; const image: TFRE_DB_Stream; const imagetype: string; const etag: String; const is_internal: Boolean; const long_desc: TFRE_DB_String; const short_desc: TFRE_DB_String): TFRE_DB_Errortype;
var user       : TFRE_DB_USER;
begin
  if Userexists(login,domainUID) then
    exit(edb_EXISTS);
  user := GFRE_DB.NewObject(TFRE_DB_USER) as TFRE_DB_USER;
  user.InitData(login,first_name,last_name,password,domainUID,is_internal,long_desc,short_desc);
  if assigned(image) then
    user.SetImage(image,imagetype,etag);
  if system_start_up then
    begin
      user.SetDomainID(FSysDomainUID);  // create admin user on startup without checks
      if user.GetDomainIDLink<>user.DomainID then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'add user failed, domainid [%s] and domainidlink [%s] are different.',[FREDB_G2H(user.DomainID),FREDB_G2H(user.GetDomainIDLink)]);
      result := FSysUsers._InternalStore(TFRE_DB_Object(user))
    end
  else
    begin
      user.SetDomainID(domainUID); // create user in the domain it belongs, not in the domain of the creator
      if user.GetDomainIDLink<>user.DomainID then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'add user failed, domainid [%s] and domainidlink [%s] are different.',[FREDB_G2H(user.DomainID),FREDB_G2H(user.GetDomainIDLink)]);
      result := FSysUsers.Store(TFRE_DB_Object(user));
    end;
end;

function TFRE_DB_SYSTEM_CONNECTION._AddRolesToGroup(const group: TFRE_DB_String; const domainUID: TFRE_DB_GUID; const roles: TFRE_DB_StringArray; const rolesDomainUID: TFRE_DB_GUID): TFRE_DB_Errortype;
var l_Group            : TFRE_DB_GROUP;
    l_NewRoles         : TFRE_DB_StringArray;
    l_NewRolesID       : TFRE_DB_GUIDArray;
    l_AggregatedRoleID : TFRE_DB_GUIDArray;
    l_FetchedRoleUID   : TFRE_DB_GUID;
    i                  : NativeInt;
    j                  : NativeInt;
    allready_in        : boolean;
begin
  if not _FetchGroup(group,domainUID,l_group) then
    exit(edb_NOT_FOUND);
  l_NewRoles        := roles;
  setLength(l_NewRolesID,length(l_NewRoles));
  for i:=0 to high(l_NewRoles) do begin
    if not _RoleID(l_NewRoles[i],rolesDomainUID,l_FetchedRoleUID) then
      exit(edb_NOT_FOUND);
    l_NewRolesID[i] :=l_FetchedRoleUID;
  end;
  l_AggregatedRoleID   := l_Group.RoleIDs;
  for i:=0 to high(l_NewRolesID) do begin
    allready_in := false;
    for j:=0 to high(l_AggregatedRoleID) do begin
      if l_NewRolesID[i]=l_AggregatedRoleID[j] then begin
        allready_in := true;
        break;
      end;
    end;
    if not allready_in then begin
      setLength(l_AggregatedRoleID,length(l_AggregatedRoleID)+1);
      l_AggregatedRoleID[high(l_AggregatedRoleID)] := l_NewRolesID[i];
    end;
  end;
  l_Group.RoleIDs := l_AggregatedRoleID;
  result := Update(l_Group);
end;

function TFRE_DB_SYSTEM_CONNECTION.CheckLogin(const loginatdomain, pass: TFRE_DB_String): TFRE_DB_Errortype;
var FUser : TFRE_DB_USER;
    login: TFRE_DB_String;
    domain: TFRE_DB_String;
begin //nln
  FREDB_SplitLocalatDomain(loginatdomain,login,domain);
  if not DomainExists(domain) then
    exit(edb_NOT_FOUND);
  result := FetchUser(login,DomainID(domain),FUser);
  if result<>edb_OK then
    exit;
  try
    if not FUser.Checkpassword(pass) then
      exit(edb_ACCESS);
    result := edb_OK;
  finally
    Fuser.Finalize;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.NewRoleI(const rolename, txt, txt_short: TFRE_DB_String;const is_internal:Boolean; var right_group: IFRE_DB_ROLE): TFRE_DB_Errortype;
var lRG : TFRE_DB_ROLE;
begin //nln
 result := NewRole(rolename,txt,txt_short,is_internal,lRG);
  if result = edb_OK then begin
    right_group := lRG;
  end else begin
    right_group := nil;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.NewGroupI(const groupname, txt, txt_short: TFRE_DB_String; const is_protected:Boolean;const is_internal:Boolean; var user_group: IFRE_DB_GROUP): TFRE_DB_Errortype;
var lUG : TFRE_DB_GROUP;
begin //nln
 result := NewGroup(groupname,txt,txt_short,is_protected,is_internal,lUG);
  if result = edb_OK then begin
    user_group := lUG;
  end else begin
    user_group := nil;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreRoleI(var role: IFRE_DB_ROLE; const domainUID: TFRE_DB_GUID): TFRE_DB_Errortype;
var lrole : TFRE_DB_ROLE;
begin //nl
  lrole   := role.Implementor as TFRE_DB_ROLE;
  result  := StoreRole(lRole,domainUID);
  role    := nil;
end;

function TFRE_DB_SYSTEM_CONNECTION.UpdateRoleI(var role: IFRE_DB_ROLE): TFRE_DB_Errortype;
var lrole : TFRE_DB_ROLE;
begin //nl
 lrole    := role.Implementor as TFRE_DB_ROLE;
 result := UpdateRole(lrole);
 role  := nil;
end;

function TFRE_DB_SYSTEM_CONNECTION.UpdateDomainI(var domain: IFRE_DB_DOMAIN): TFRE_DB_Errortype;
var ldomain : TFRE_DB_DOMAIN;
begin //nl
 ldomain := domain.Implementor as TFRE_DB_DOMAIN;
 result  := UpdateDomain(ldomain);
 domain  := nil;
end;

function TFRE_DB_SYSTEM_CONNECTION.UpdateUserI(var user: IFRE_DB_USER): TFRE_DB_Errortype;
var luser : TFRE_DB_USER;
begin //nl
 luser  := user.Implementor as TFRE_DB_USER;
 result := UpdateUser(luser);
 user   := nil;
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreGroupI(var group:IFRE_DB_GROUP;const domainUID: TFRE_DB_GUID): TFRE_DB_Errortype;
var lUG : TFRE_DB_GROUP;
begin //nl
 lUG    := group.Implementor as TFRE_DB_GROUP;
 result := StoreGroup(domainUID, lUG);
 group  := nil;
end;

function TFRE_DB_SYSTEM_CONNECTION.UpdateGroupI(var group: IFRE_DB_GROUP): TFRE_DB_Errortype;
var lUG : TFRE_DB_GROUP;
begin //nl
 lUG    := group.Implementor as TFRE_DB_GROUP;
 result := UpdateGroup(lUG);
 group  := nil;
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreTranslateableTextI(const txt: IFRE_DB_TEXT): TFRE_DB_Errortype;
var ttxt : TFRE_DB_TEXT;
begin //nl
  ttxt   := txt.Implementor_HC as TFRE_DB_TEXT;
  result := StoreTranslateableText(ttxt);
end;

destructor TFRE_DB_SYSTEM_CONNECTION.Destroy;
begin
  if FCloned and
     not FClonedFrom.FConnectionClones.Delete(self) then
       raise EFRE_DB_Exception.Create(edb_INTERNAL,'something is rotten in the state of denmark');
  FCurrentUserToken.Finalize;
  inherited Destroy;
end;

function TFRE_DB_BASE_CONNECTION.FetchApplications(var apps:TFRE_DB_APPLICATION_ARRAY; var loginapp: TFRE_DB_APPLICATION): TFRE_DB_Errortype;
var l_apps : TFRE_DB_APPLICATION_ARRAY;
    cnt,i  : integer;
begin
  _ConnectCheck;
  l_apps := GFRE_DB.GetApps;
  SetLength(apps,length(l_apps));
  cnt := 0;
  for i := 0 to high(l_apps) do begin
    if l_apps[i].AppClassName='TFRE_DB_LOGIN' then begin
      loginapp:=l_apps[i];
      continue;
    end;
    apps[cnt] := l_apps[i];
    inc(cnt);
  end;
  setlength(apps,cnt);
  result := edb_OK;
end;

function TFRE_DB_BASE_CONNECTION.FetchApplicationsI(var apps: IFRE_DB_APPLICATION_ARRAY; var loginapp: IFRE_DB_APPLICATION): TFRE_DB_Errortype;
var appa :TFRE_DB_APPLICATION_ARRAY;
    loginappt: TFRE_DB_APPLICATION;
  i: Integer;
begin //nl
  result := FetchApplications(appa,loginappt);
  if result = edb_OK then begin
    setlength(apps,length(appa));
    for i:=0 to high(apps) do begin
      apps[i] := appa[i];
    end;
  end;
  loginapp:=loginappt;
end;


procedure TFRE_DB_BASE_CONNECTION.DrawScheme(const datastream: TStream; const classfile: string);
var  dbgraph     : TFRE_DB_GRAPH;
     checkedobjs : IFRE_DB_Object;

  procedure nestedObjectIterator(const obj:IFRE_DB_Object; var halt:boolean ; const current,max : NativeInt);
  begin
   dbgraph.ObjectIterator(obj,self);
  end;

  procedure nestedCollectionIterator(const coll: IFRE_DB_Collection);
  begin
    dbgraph.CollectionIterator(coll);
  end;

  procedure nestedEmbeddedIterator(const obj:IFRE_DB_SCHEMEOBJECT);
  begin
    dbgraph.EmbeddedIterator(obj);
  end;

  procedure nestedSchemeIterator(const obj:IFRE_DB_SCHEMEOBJECT);
  begin
    dbgraph.SchemeIterator(obj);
  end;

  procedure nestedObjectCheckClassesIterator(const obj:IFRE_DB_Object; var halt:boolean ; const current,max : NativeInt);

    procedure CheckObjectReferences(const dobj:IFRE_DB_Object);
    var refs: TFRE_DB_ObjectReferences;
           i: NativeInt;
        sobj: IFRE_DB_Object;
    begin
     dbgraph.AddClass(dobj.SchemeClass);
//     writeln('DOBJ:',dobj.UID_String);
     if checkedobjs.FieldExists(dobj.UID_String) then
       exit
     else
       checkedobjs.Field(dobj.UID_String).AsBoolean:=true;

     refs := GetReferencesDetailed(dobj.UID,false);

     for i:=0 to high(refs) do
       begin
         CheckDbResult(FetchI(refs[i].linked_uid,sobj),'could not fetch object on to references for '+dobj.UID_String);
//         writeln('SOBJ1:',sobj.UID_String);
         CheckObjectReferences(sobj);
         sobj.Finalize;
       end;
     refs := GetReferencesDetailed(dobj.UID,true);
     for i:=0 to high(refs) do
       begin
         CheckDbResult(FetchI(refs[i].linked_uid,sobj),'could not fetch object on for references for '+dobj.UID_String);
//         writeln('SOBJ2:',sobj.UID_String);
         CheckObjectReferences(sobj);
         sobj.Finalize;
       end;
    end;

  begin
//    writeln('SWL: CHECK OBJ SCHEME:',obj.SchemeClass);
    if dbgraph.IsInClasslist(obj.SchemeClass) then
      begin
        CheckObjectReferences(obj);
      end;
  end;

begin
  dbgraph        :=        TFRE_DB_GRAPH.Create;
  try
    dbgraph.SetClassfile(classfile);
    dbgraph.PlotStart;
    checkedobjs := GFRE_DBI.NewObject;
    self.ForAllDatabaseObjectsDo(@nestedObjectCheckClassesIterator);
    checkedobjs.Finalize;
    self.ForAllCollsI(@nestedCollectionIterator);
    self.ForAllSchemesI(@nestedEmbeddedIterator);
    self.ForAllSchemesI(@nestedSchemeIterator);
    self.ForAllDatabaseObjectsDo(@nestedObjectIterator);
    dbgraph.PlotEnd;
    dbgraph.PlotScheme(datastream);
  finally
    dbgraph.Free;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.Connect(const loginatdomain: TFRE_DB_String; const password: TFRE_DB_String): TFRE_DB_Errortype;
var
  login  : TFRE_DB_String;
  domain : TFRE_DB_String;
  user   : TFRE_DB_USER;
begin
  _CloneCheck;
  if not FConnected then
    begin
      result := _Connect('SYSTEM',false);
      if Result<>edb_OK then
        exit;
    end;
  FREDB_SplitLocalatDomain(loginatdomain,login,domain);
  FetchUser(login,DomainID(domain),user);
  if not assigned(user) then
    Exit(edb_NOT_FOUND);
  if not user.Checkpassword(password) then
    begin
      exit(edb_ACCESS);
    end
  else
    begin
      if assigned(FCurrentUserToken) then
        FreeAndNil(FCurrentUserToken);
      FCurrentUserToken := ReNewCurrentUserToken(user).Implementor as TFRE_DB_USER_RIGHT_TOKEN;
      FAuthenticated    := true;
    end;
  result := edb_OK;
end;

constructor TFRE_DB_SIMPLE_TRANSFORM.Create;
begin
  inherited Create;
end;

function TFRE_DB_SIMPLE_TRANSFORM.TransformInOut(const conn : IFRE_DB_CONNECTION ; const input: IFRE_DB_Object): TFRE_DB_Object;
  procedure Iterate(var ft : TFRE_DB_FIELD_TRANSFORM ; const idx : NativeInt ; var halt_flag:boolean);
  begin
     ft.TransformField(conn,input,result);
  end;
begin
  result := GFRE_DB.NewObject;
  FTransformList.ForAllBreak(@iterate);
  result._Field('uid').AsGUID                            := input.Field('uid').AsGUID;
  result._Field('domainid').AsGUID                       := input.Field('domainid').AsGUID;
  result._Field(cFRE_DB_SYS_TRANS_IN_OBJ_WAS_A).AsString := input.SchemeClass;
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.TransformRefQuery;
begin
  //if Assigned(FRQ_func) then
  //  FRQ_func(,);
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddCollectorscheme(const format: TFRE_DB_String; const in_fieldlist: TFRE_DB_NameTypeArray; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const display: Boolean; const sortable: Boolean; const filterable: Boolean; const gui_display_type: TFRE_DB_DISPLAY_TYPE; const fieldSize: Integer; const hide_in_output: boolean);
begin
  FTransformList.Add(TFRE_DB_COLLECTOR_FT.Create(format,in_fieldlist,out_field,output_title,gui_display_type,display,sortable,filterable,fieldSize));
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddFulltextFilterOnTransformed(const fieldlist: array of TFRE_DB_NameType);
var
  format : String;
  fields : TFRE_DB_NameTypeArray;
  i      : Integer;
begin
  format:='%s';
  for i := 1 to Length(fieldlist) - 1 do begin
    format:=format+' %s';
  end;
  SetLength(fields,Length(fieldlist));
  for i := 0 to High(fieldlist) do begin
    fields[i]:=fieldlist[i];
  end;

  FTransformList.Add(TFRE_DB_OUTCOLLECTOR_FT.Create(format,fields,'FTX_SEARCH','',dt_string,false));
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddOneToOnescheme(const fieldname: TFRE_DB_String; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE; const display: Boolean; const sortable: Boolean; const filterable: Boolean; const fieldSize: Integer; const iconID: String; const openIconID: String; const default_value: TFRE_DB_String; const filterValues: TFRE_DB_StringArray; const hide_in_output: boolean);
begin
  FTransformList.Add(TFRE_DB_ONEONE_FT.Create(fieldname,out_field,output_title,gui_display_type,display,sortable,filterable,fieldSize,iconID,openIconID,default_value,filterValues));
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddMultiToOnescheme(const in_fieldlist: TFRE_DB_NameTypeArray; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE; const display: Boolean; const sortable: Boolean; const filterable: Boolean; const fieldSize: Integer; const iconID: String; const openIconID: String;const default_value:TFRE_DB_String;const hide_in_output : boolean=false);
begin
  FTransformList.Add(TFRE_DB_MULTIONE_FT.Create(in_fieldlist,out_field,output_title,gui_display_type,display,sortable,filterable,fieldSize,iconID,openIconID,default_value));
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddProgressTransform(const valuefield: TFRE_DB_String; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const textfield: TFRE_DB_String; const out_text: TFRE_DB_String; const maxValue: Single; const sortable: Boolean; const filterable: Boolean; const fieldSize: Integer; const hide_in_output: boolean);
begin
  FTransformList.Add(TFRE_DB_PROGRESS_FT.Create(valuefield,out_field,output_title,textfield,out_text,maxValue,true,sortable,filterable,fieldSize));
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddConstString(const out_field, value: TFRE_DB_String; const display: Boolean; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE; const sortable: Boolean; const filterable: Boolean; const fieldSize: Integer; const hide_in_output: boolean);
begin
  FTransformList.Add(TFRE_DB_CONST_STRING_FT.Create(out_field,value,display,sortable,filterable,output_title,gui_display_type,fieldSize));
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddDBTextToOne(const fieldname: TFRE_DB_String; const which_text: TFRE_DB_TEXT_SUBTYPE; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE; const sortable: Boolean; const filterable: Boolean; const fieldSize: Integer; const hide_in_output: boolean);
begin
  FTransformList.Add(TFRE_DB_TEXT_FT.Create(fieldname,which_text,out_field,output_title,gui_display_type,true,sortable,filterable,fieldSize));
end;


function TFRE_DB_SIMPLE_TRANSFORM.GetViewCollectionDescription: TFRE_DB_CONTENT_DESC;
var vcd : TFRE_DB_VIEW_LIST_LAYOUT_DESC;
    i   : NativeInt;

    procedure Iterate(var ft : TFRE_DB_FIELD_TRANSFORM ; const idx : NativeInt ; var halt_flag:boolean);
    begin
      ft.AddToViewCollection(vcd);
    end;

begin
  vcd := TFRE_DB_VIEW_LIST_LAYOUT_DESC.create.Describe();
  FTransformList.ForAllBreak(@Iterate);      //self
  vcd.AddDataElement.Describe('uid','UID',dt_string,false,false,1,false);
  result := vcd;
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.SetFinalRightTransformFunction(const func: IFRE_DB_FINAL_RIGHT_TRANSFORM_FUNCTION;const langres: array of TFRE_DB_String);
var
  i: Integer;
begin
  FFinalRightTransform := func;
  SetLength(FFRTLangres,length(langres));
  for i := 0 to high(langres) do
    FFRTLangres[i] := langres[i];
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddReferencedFieldQuery(const func: IFRE_DB_QUERY_SELECTOR_FUNCTION; const ref_field_chain: array of TFRE_DB_NameTypeRL; const output_fields: array of TFRE_DB_String; const output_titles: array of TFRE_DB_String; const langres: array of TFRE_DB_String; const gui_display_type: array of TFRE_DB_DISPLAY_TYPE; const display: Boolean; const sortable: Boolean; const filterable: Boolean; const fieldSize: Integer; const hide_in_output: boolean);
var rfc             : TFRE_DB_NameTypeRLArray;
    i               : NativeInt;
    FRQ_func        : IFRE_DB_QUERY_SELECTOR_FUNCTION;
    FRQO_Fields     : TFRE_DB_StringArray;
    FRQO_Titles     : TFRE_DB_StringArray;
    FRQO_Langres    : TFRE_DB_StringArray;
    FRQO_Types      : TFRE_DB_DISPLAY_TYPE_Array;
    FRQO_Hide       : TFRE_DB_BoolArray;
    FRQO_Display    : TFRE_DB_BoolArray;
    FRQO_Sortable   : TFRE_DB_BoolArray;
    FRQO_Filterable : TFRE_DB_BoolArray;
    FRQO_FieldSize  : TFRE_DB_Int32Array;
begin
  if not assigned(func) then
      raise EFRE_DB_Exception.Create(edb_ERROR,'you must specify a qry selector function');

 SetLength(FRQO_Fields,length(output_fields));
 for i := 0 to high(output_fields) do
   FRQO_Fields[i] := output_fields[i];
 SetLength(FRQO_Titles,length(output_titles));
 for i := 0 to high(output_titles) do
   FRQO_Titles[i] := output_titles[i];
 SetLength(FRQO_Langres,length(langres));
 for i := 0 to high(langres) do
   FRQO_Langres[i] := langres[i];
 SetLength(FRQO_Types,length(gui_display_type));
 for i := 0 to high(gui_display_type) do
   FRQO_Types[i] := gui_display_type[i];
 if Length(FRQO_Titles)=0 then
   FRQO_Titles := Copy(FRQO_Fields);
 if Length(FRQO_Types)<>Length(FRQO_Titles) then
   begin
     SetLength(FRQO_Types,Length(FRQO_Titles));
     for i:= high(FRQO_Types) to high(FRQO_Titles) do { fill up with dt_String }
       FRQO_Types[i] := dt_string;
   end;
 SetLength(FRQO_Sortable,Length(FRQO_Titles));
 for i:= 0 to high(FRQO_Sortable) do
   FRQO_Sortable[i] := sortable;
 SetLength(FRQO_Filterable,Length(FRQO_Titles));
 for i:= 0 to high(FRQO_Filterable) do
   FRQO_Filterable[i] := filterable;
 SetLength(FRQO_Hide,Length(FRQO_Titles));
 for i:= 0 to high(FRQO_Hide) do
   FRQO_Hide[i] := hide_in_output;
 SetLength(FRQO_Display,Length(FRQO_Titles));
 for i:= 0 to high(FRQO_Hide) do
   FRQO_Display[i] := display;
 SetLength(FRQO_FieldSize,Length(FRQO_Titles));
 for i:= 0 to high(FRQO_FieldSize) do
   FRQO_FieldSize[i] := fieldSize;
 FRQ_func  := func;
 setlength(rfc,Length(ref_field_chain));
 for i:=0 to high(ref_field_chain) do
   rfc[i] := ref_field_chain[i];
 FTransformList.Add(TFRE_DB_REFERERENCE_QRY_FT.Create(FRQ_func,rfc,FRQO_Fields,FRQO_Titles,FRQO_Langres,FRQO_Types,FRQO_Display,FRQO_Sortable,FRQO_Filterable,FRQO_Hide,FRQO_FieldSize));
 FHasReflinkTransforms:=true;
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddMatchingReferencedField(const ref_field_chain: array of TFRE_DB_NameTypeRL; const target_field: TFRE_DB_String; const output_field: TFRE_DB_String; const output_title: TFRE_DB_String; const display:Boolean; const gui_display_type: TFRE_DB_DISPLAY_TYPE;const sortable,filterable:Boolean;const fieldSize: Integer;const iconID:String='';const filterValues:TFRE_DB_StringArray=nil;const hide_in_output : boolean=false);
var rfc : TFRE_DB_NameTypeRLArray;
    i   : NativeInt;
begin
  setlength(rfc,Length(ref_field_chain));
  for i:=0 to high(ref_field_chain) do
    rfc[i] := ref_field_chain[i];
  FTransformList.Add(TFRE_DB_REFERERENCE_CHAIN_FT.Create(rfc,target_field,output_field,output_title,gui_display_type,display,sortable,filterable,fieldSize,iconID,filterValues));
  FHasReflinkTransforms:=true;
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddMatchingReferencedField(const ref_field: TFRE_DB_NameTypeRL; const target_field: TFRE_DB_String; const output_field: TFRE_DB_String; const output_title: TFRE_DB_String; const display:Boolean;const gui_display_type: TFRE_DB_DISPLAY_TYPE;const sortable,filterable:Boolean;const fieldSize: Integer;const iconID:String='';const filterValues:TFRE_DB_StringArray=nil;const hide_in_output : boolean=false);
var rf:TFRE_DB_NameTypeRL;
begin
  rf := ref_field;
  if (pos('>',rf)=0) and
     (pos('<',rf)=0) then
       rf := rf+'>';
  AddMatchingReferencedField([rf],target_field,output_field,output_title,display,gui_display_type,sortable,filterable,fieldSize,iconID,filterValues,hide_in_output);
end;

function TFRE_DB_DERIVED_COLLECTION.HasParentChildRefRelationDefined: boolean;
begin
  result := (FParentChldLinkFldSpec<>'');
end;

function TFRE_DB_DERIVED_COLLECTION.IsDependencyFilteredCollection: boolean;
begin
  result := FUseDepAsLinkFilt;
end;

function TFRE_DB_DERIVED_COLLECTION.HasReflinksInTransformation: boolean;
begin
  result := FTransform.FHasReflinkTransforms;
end;

function TFRE_DB_DERIVED_COLLECTION.ParentchildRelationIsOutbound: boolean;
begin
  result := FParentLinksChild;
end;

procedure TFRE_DB_DERIVED_COLLECTION._CheckSetDisplayType(const CollectionDisplayType: TFRE_COLLECTION_DISPLAY_TYPE);
begin
  if CollectionDisplayType=FDisplaytype then
    exit;
  if FDisplaytype<>cdt_Invalid then raise EFRE_DB_Exception.Create(edb_ERROR,'Collectiondisplaytype is already set');
  FDisplaytype := CollectionDisplayType;
end;

procedure TFRE_DB_DERIVED_COLLECTION._CheckTransformSet;
begin
  if not assigned(FTransform) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'you must set a transformation for derived collection [%s]',[FName]);
end;

procedure TFRE_DB_DERIVED_COLLECTION._ClearMode;
begin
  FGridDisplayFlags      := [];
  FitemMenuFunc.Free     ;
  FitemMenuFunc          := nil;
  FitemDetailsFunc.Free  ;
  FitemDetailsFunc       := nil;
  FselectionDepFunc.Free ;
  FselectionDepFunc      := nil;
  FtreeMenuFunc.Free     ;
  FtreeMenuFunc          := nil;
  FdropFunc.Free         ;
  FdropFunc              := nil;
  FdragFunc.Free         ;
  FdragFunc              := nil;
  FTransform.Free        ;
  FlabelFields           := nil;
  //FTreeNodeIconField     := '';
end;

procedure TFRE_DB_DERIVED_COLLECTION.ForAllDerived(const func: IFRE_DB_Obj_Iterator);
begin

end;

procedure TFRE_DB_DERIVED_COLLECTION._CheckDepRefConstraint;
var i : NativeInt;
begin
  for i:=0 to high(FDepRefConstraint) do
    begin
      if (pos(FDepRefConstraint[i],'>')=-1)
         and (pos(FDepRefConstraint[i],'<')=-1) then
           begin
             raise EFRE_DB_Exception.Create(edb_ERROR,'schemelinkspecification bad in collection : '+FName);
           end;
    end;
  //writeln('TODO::: implement checkdefrefconstraint');
end;

procedure TFRE_DB_DERIVED_COLLECTION.MustBeInitialized;
begin
  if FDCMode=dc_None then
    raise EFRE_DB_Exception.Create(edb_ERROR,'derived collection not inititialized properly');
end;


procedure TFRE_DB_DERIVED_COLLECTION.BindSession(const session: TFRE_DB_UserSession);
begin
  if assigned(FDC_Session) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'double session bind / logic');
  FDC_Session := session;
end;

constructor TFRE_DB_DERIVED_COLLECTION.Create(const dbname: TFRE_DB_NameType; const name: TFRE_DB_NameType); //self
begin
  Inherited Create;
  FName            := name;
  FUniqueName      := uppercase(name);
  FDCollFilters    := GFRE_DB_TCDM.GetNewFilterDefinition(DBName);
  FDCollFiltersDyn := GFRE_DB_TCDM.GetNewFilterDefinition(DBName);
  FDCollOrder      := GFRE_DB_TCDM.GetNewOrderDefinition;
end;

function TFRE_DB_DERIVED_COLLECTION.GetCollectionTransformKey: TFRE_DB_NameTypeRL;
begin
  MustBeInitialized;
  result := FParentCollection.CollectionName(true)+'#'+CollectionName(true);
end;

{ record cnt includes transformed childs}
procedure TFRE_DB_DERIVED_COLLECTION.MyTransForm(const connection: TFRE_DB_CONNECTION; const in_objects: array of IFRE_DB_Object; const transdata: TFRE_DB_TRANSFORMED_ARRAY_BASE; var rec_cnt: NativeInt; const lazy_child_expand: boolean; const mode: TDC_TransMode; const update_idx: NativeInt; const rl_ins: boolean; const parentpath: TFRE_DB_String; const in_parent_tr_obj: IFRE_DB_Object; const transkey: TFRE_DB_TransStepId);
var
  in_object     : IFRE_DB_Object;
  tr_obj        : TFRE_DB_Object;
  len_chld      : NativeInt;
  upconn        : TFRE_DB_CONNECTION;

    procedure SetInternalFields(const tro,ino:IFRE_DB_Object);
    begin
      tro.Field(cFRE_DB_SYS_T_LMO_TRANSID).AsString := ino.Field(cFRE_DB_SYS_T_LMO_TRANSID).AsString;
    end;

    procedure SetSpecialFields(const tro,ino:IFRE_DB_Object);
    var fld : IFRE_DB_FIELD;
    begin
      tro.Field('_menufunc_').AsString      := 'Menu';
      tro.Field('_contentfunc_').AsString   := 'Content';
      //tro.Field('children').AsString        := 'UNCHECKED';
      //if ino.FieldOnlyExisting('icon',fld) then // icon in source
      //  tro.Field('icon').AsString:= FREDB_getThemedResource(fld.AsString); // icon in transformed
    end;

    procedure SetParentPath(const pp:string);
    begin
      FREDB_PP_AddParentPathToObj(tr_obj,pp);
    end;

    procedure TransFormChildsForUid(const parent_tr_obj : IFRE_DB_Object ; const parentpath : string ; const depth : NativeInt ; const in_uid : TFRE_DB_GUID);
    var j          : NativeInt;
        refd_uids  : TFRE_DB_GUIDArray;
        refd_objs  : IFRE_DB_ObjectArray;
        len_chld      : NativeInt;
        in_chld_obj   : IFRE_DB_Object;
    begin
     refd_uids     := upconn.GetReferencesNoRightCheck(in_uid,FParentLinksChild,FParentChildScheme,FParentChildField);
     len_chld      := length(refd_uids);
     parent_tr_obj.Field(cFRE_DB_CLN_CHILD_CNT).AsInt32 := len_chld;
     if len_chld>0 then
       begin
         parent_tr_obj.Field(cFRE_DB_CLN_CHILD_FLD).AsString := cFRE_DB_CLN_CHILD_FLG;
         inc(rec_cnt,len_chld); { record cnt includes transformed childs}
         CheckDbResult(upconn.BulkFetchNoRightCheck(refd_uids,refd_objs),'transform childs');
         for j:=0 to high(refd_objs) do
           begin
             in_chld_obj  := refd_objs[j];
             try
               tr_obj    := FTransform.TransformInOut(upconn,in_chld_obj);
               SetInternalFields(tr_obj,in_chld_obj);
               SetSpecialFields(tr_obj,in_chld_obj);
               SetParentPath(parentpath);
               transdata.SetTransformedObject(tr_obj);
               TransFormChildsForUid(tr_obj,parentpath+','+FREDB_G2H(refd_uids[j]),depth+1,refd_uids[j]); { recurse }
             finally
               refd_objs[j].Finalize;
             end;
           end;
         SetLength(refd_objs,0);
       end;
    end;

    var rc : NativeInt;

begin
  try
    upconn := Connection;
    for in_object in in_objects do
      begin
        case mode of
          trans_Insert:
            begin
              if HasParentChildRefRelationDefined then
                begin
                  rc := upconn.GetReferencesCountNoRightCheck(in_object.UID,not FParentLinksChild,FParentChildScheme,FParentChildField);
                  if rc=0 then { ROOT NODE}
                    begin
                      tr_obj := FTransform.TransformInOut(upconn,in_object);
                      transdata.SetTransformedObject(tr_obj);
                      SetParentPath(''); { this is a root node }
                      SetInternalFields(tr_obj,in_object);
                      if HasParentChildRefRelationDefined then
                        begin
                          SetSpecialFields(tr_obj,in_object);
                          TransFormChildsForUid(tr_obj,tr_obj.UID_String,0,tr_obj.UID); { this is the initial fill case, next step transfrom children recursive }
                        end;
                    end;
                end
              else
                begin
                  tr_obj := FTransform.TransformInOut(upconn,in_object);
                  transdata.SetTransformedObject(tr_obj);
                  SetParentPath(''); { this is a root node }
                  SetInternalFields(tr_obj,in_object);
                end;
            end;
          trans_SingleInsert:
            begin
              tr_obj := FTransform.TransformInOut(upconn,in_object);
              if HasParentChildRefRelationDefined then
                SetSpecialFields(tr_obj,in_object); { do not transfrom children recursive, these will come as single add updates, when inserted in a transaction }
              SetParentPath(parentpath);
              SetInternalFields(tr_obj,in_object);
              transdata.HandleInsertTransformedObject(tr_obj,in_parent_tr_obj);
            end;
          trans_Update:
            begin
              tr_obj := FTransform.TransformInOut(upconn,in_object);
              SetSpecialFields(tr_obj,in_object);
              SetParentPath(parentpath);
              SetInternalFields(tr_obj,in_object);
              if HasParentChildRefRelationDefined then
                begin
                  len_chld := upconn.GetReferencesCountNoRightCheck(in_object.UID,FParentLinksChild,FParentChildScheme,FParentChildField);
                  tr_obj.Field(cFRE_DB_CLN_CHILD_CNT).AsInt32 := len_chld;
                  if len_chld>0 then
                    tr_obj.Field(cFRE_DB_CLN_CHILD_FLD).AsString := cFRE_DB_CLN_CHILD_FLG;
                end;
              transdata.HandleUpdateTransformedObject(tr_obj,update_idx);
              { do not handle child updates now, the will get handled on field update event of the parent }
            end;
        end;
      end;
  finally
    for in_object in in_objects do
      in_object.Finalize;
  end;
end;


procedure TFRE_DB_DERIVED_COLLECTION.TransformAllTo(const connection: IFRE_DB_CONNECTION; const transdata: TFRE_DB_TRANSFORMED_ARRAY_BASE; const lazy_child_expand: boolean; var record_cnt: NativeInt);
var upconn     : TFRE_DB_CONNECTION;
    uids       : TFRE_DB_GUIDArray;
    objs       : IFRE_DB_ObjectArray;

begin
  MustBeInitialized;
  transdata.CleanUp; { retransform ? }
  upconn     := Connection.Implementor_HC as TFRE_DB_CONNECTION;
  //record_cnt := FParentCollection.ItemCount;  // TODO -> concat with next call
  //(FParentCollection.Implementor_HC as TFRE_DB_COLLECTION).GetAllUids(uids); // ForAllNoRightChk(@TransForm);
  //upconn.BulkFetchNoRightCheck(uids,objs);

  (FParentCollection.Implementor_HC as TFRE_DB_COLLECTION).GetAllObjsNoRC(objs);
  record_cnt := Length(objs);

  //if not FREDB_CheckGuidsUnique(uids) then
  //  raise EFRE_DB_Exception.Create(edb_ERROR,'objects double in collection');
  //if record_cnt<>Length(objs) then
  //  raise EFRE_DB_Exception.Create(edb_INTERNAL,'recordcount mismatch / collcount vs bulkfetch (%d<>%d)',[record_cnt,Length(objs)]);
  MyTransForm(upconn,objs,transdata,record_cnt,lazy_child_expand,trans_Insert,-1,false,'',nil,'-');
end;

procedure TFRE_DB_DERIVED_COLLECTION.TransformSingleUpdate(const connection: IFRE_DB_CONNECTION; const in_object: IFRE_DB_Object; const transdata: TFRE_DB_TRANSFORMED_ARRAY_BASE; const lazy_child_expand: boolean; const upd_idx: NativeInt; const parentpath_full: TFRE_DB_String; const transkey: TFRE_DB_TransStepId);
var rec_cnt : NativeInt;
    upconn  : TFRE_DB_CONNECTION;
begin
  upconn := connection.Implementor_HC as TFRE_DB_CONNECTION;
  MyTransForm(upconn,in_object,transdata,rec_cnt,lazy_child_expand,trans_Update,upd_idx,false,parentpath_full,nil,transkey);
end;

procedure TFRE_DB_DERIVED_COLLECTION.TransformSingleInsert(const connection: IFRE_DB_CONNECTION; const in_object: IFRE_DB_Object; const transdata: TFRE_DB_TRANSFORMED_ARRAY_BASE; const lazy_child_expand: boolean; const rl_ins: boolean; const parentpath: TFRE_DB_String; const parent_tr_obj: IFRE_DB_Object; const transkey: TFRE_DB_TransStepId);
var rec_cnt:NativeInt;
    upconn  : TFRE_DB_CONNECTION;
begin
 upconn     := connection.Implementor_HC as TFRE_DB_CONNECTION;
  MyTransForm(upconn,in_object,transdata,rec_cnt,lazy_child_expand,trans_SingleInsert,-1,rl_ins,parentpath,parent_tr_obj,transkey);
end;

//procedure TFRE_DB_DERIVED_COLLECTION.ApplyToPageI(const page_info: TFRE_DB_DC_PAGING_INFO; const iterator: IFRE_DB_Obj_Iterator);
//
//  procedure MyIterator(const obj:TFRE_DB_Object);
//  begin
//    iterator(obj);
//  end;
//
//begin
//  ApplyToPage(page_info,@MyIterator);
//end;

class procedure TFRE_DB_DERIVED_COLLECTION.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.Strict(true);
end;

destructor TFRE_DB_DERIVED_COLLECTION.Destroy;

  procedure MyFinalize(const dbo : TFRE_DB_Object;const b : boolean);
  begin
    dbo.Finalize;
  end;

begin
  FDCollFilters.free;
  FDCollFiltersDyn.free;
  FDCollOrder.free;
  if assigned(FTransform) then
    FTransform.free;
  FitemMenuFunc.Free;
  FitemDetailsFunc.free;
  FselectionDepFunc.free;
  FtreeMenuFunc.free;
  FdropFunc.free;
  FdragFunc.free;
  inherited Destroy;
end;


procedure TFRE_DB_DERIVED_COLLECTION.AddSelectionDependencyEvent(const derived_collection_name: TFRE_DB_NameType; const ReferenceID: TFRE_DB_NameType);
begin
  SetLength(FSelectionDepEvents,Length(FSelectionDepEvents)+1);
  with FSelectionDepEvents[high(FSelectionDepEvents)] do
    begin
      dc_name  := derived_collection_name;
      ref_name := ReferenceID;
    end;
end;

//function TFRE_DB_DERIVED_COLLECTION.RemoveFieldFilter(const filter_key: TFRE_DB_String; const on_transform: boolean): TFRE_DB_Errortype;
//begin
//  result := _DeleteFilterKey(filter_key,on_transform);
//end;

procedure TFRE_DB_DERIVED_COLLECTION.SetDefaultOrderField(const field_name: TFRE_DB_String; const ascending: boolean);
begin
  //FDefaultOrderField     := field_name;
  //FDefaultOrderAsc       := ascending;
  Orders.ClearOrders;
  Orders.AddOrderDef(field_name,ascending,true);
end;

procedure TFRE_DB_DERIVED_COLLECTION.RemoveAllFilterFields;
begin
  Filters.RemoveAllFilters;
end;

procedure TFRE_DB_DERIVED_COLLECTION.RemoveAllFiltersPrefix(const prefix: string);
begin
  Filters.RemoveAllFiltersPrefix(prefix);
end;

function TFRE_DB_DERIVED_COLLECTION.Filters: TFRE_DB_DC_FILTER_DEFINITION_BASE;
begin
  result := FDCollFilters;
end;

function TFRE_DB_DERIVED_COLLECTION.Orders: TFRE_DB_DC_ORDER_DEFINITION_BASE;
begin
  result := FDCollOrder;
end;

procedure TFRE_DB_DERIVED_COLLECTION.SetDeriveParent(const coll: IFRE_DB_COLLECTION; const idField: String);
begin
  if not Assigned(coll) then raise EFRE_DB_Exception.Create(edb_ERROR,'PLEASE PROVIDE A ASSIGNED DERIVE PARENT');
  if FDCMode<>dc_None then
    raise EFRE_DB_Exception.Create(edb_ERROR,'CANNOT SWITCH DERIVED CONNECTION MODE, ONCE IT WAS CHOSEN');
  FIdField          := idField;
  FDCMode           := dc_Map2RealCollection;
  FParentCollection := coll.Implementor as TFRE_DB_COLLECTION;
  FInitialDerived   := False;
end;


procedure TFRE_DB_DERIVED_COLLECTION.SetUseDependencyAsRefLinkFilter(const scheme_and_field_constraint: array of TFRE_DB_NameTypeRL; const negate: boolean; const dependency_reference: string);
var
  i: NativeInt;
begin
  FUseDepAsLinkFilt := true;
  SetLength(FDependencyRef,1);
  FDependencyRef[0] := dependency_reference;
  FDepObjectsRefNeg := negate;
  SetLength(FDepRefConstraint,1);
  SetLength(FDepRefConstraint[0],Length(scheme_and_field_constraint));
  for i := 0 to high(scheme_and_field_constraint) do
    FDepRefConstraint[0][i] := scheme_and_field_constraint[i];
end;

procedure TFRE_DB_DERIVED_COLLECTION.SetDeriveTransformation(const tob: IFRE_DB_TRANSFORMOBJECT);
begin
  FTransform.Free;
  FTransform := tob.Implementor as TFRE_DB_TRANSFORMOBJECT;
  //Set default order as first field
  if Orders.OrderCount=0 then
    Orders.AddOrderDef(FTransform.GetFirstFieldname,true,true);
end;


function TFRE_DB_DERIVED_COLLECTION.ItemCount: Int64;
var qrydef : TFRE_DB_QUERY_DEF;
    qry    : TFRE_DB_QUERY_BASE;
begin
  qrydef := SetupQryDefinitionBasic(0,0,0);
  try
    result := ExecuteQryLocked(qrydef,qry);
  finally
    qry.UnlockBaseData;
    qry.Free;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.SetupQryDefinitionBasic(const start, count, clientid: NativeInt): TFRE_DB_QUERY_DEF;
var qrydef : TFRE_DB_QUERY_DEF;
begin
 qrydef:=default(TFRE_DB_QUERY_DEF);
 qrydef.DBName                 := FDC_session.GetDBConnection.GetDatabaseName;
 qrydef.DependencyRefIds       := FREDB_StringArray2Upper(FDependencyRef);
 qrydef.DepRefConstraints      := FDepRefConstraint;
 qrydef.DepRefNegate           := FDepObjectsRefNeg;
 qrydef.ParentChildSpec        := FParentChldLinkFldSpec;
 qrydef.ParentChildSkipSchemes := nil; { todo implement }
 qrydef.DerivedCollName        := CollectionName(true);
 qrydef.ParentName             := FParentCollection.CollectionName(true);
 qrydef.FilterDefStaticRef     := FDCollFilters;
 qrydef.FilterDefDynamicRef    := FDCollFiltersDyn;
 qrydef.OrderDefRef            := Orders;
 qrydef.SessionID              := FDC_Session.GetSessionID;
 qrydef.UserTokenRef           := FDC_Session.GetDBConnection.SYS.GetCurrentUserTokenRef;
 qrydef.ClientQueryID          := clientid;
 qrydef.StartIdx               := start;
 qrydef.ToDeliverCount         := count;
 result := qrydef;
end;

function TFRE_DB_DERIVED_COLLECTION.SetupQryDefinitionFromWeb(const web_input: IFRE_DB_Object): TFRE_DB_QUERY_DEF;
var i, j, cnt : NativeInt;
    sort      : IFRE_DB_Object;
    qrydef    : TFRE_DB_QUERY_DEF;

    procedure Processfilters;
    var dop : IFRE_DB_Object;

        procedure AddFilter(const filter_key : TFRE_DB_NameType ; const Filter : IFRE_DB_Object);
        var ftstr       : shortstring;
            ft          : TFRE_DB_FILTERTYPE;
            ftfrom_vals : TFRE_DB_FILTERTYPE;
            ffn         : TFRE_DB_NameType;
            sft         : TFRE_DB_STR_FILTERTYPE;
            fld         : IFRE_DB_FIELD;
            fallownull  : boolean;
            fnegate     : boolean;
            nft         : TFRE_DB_NUM_FILTERTYPE;
            nfts        : TFRE_DB_String;


            procedure ProcessDateTimeFilter;
            var ffld : IFRE_DB_Field;
                i    : NativeInt;
                vals : TFRE_DB_DateTimeArray;
            begin
              ffld :=  Filter.field('FILTERVALUES');
              vals := ffld.AsInt64Arr;
              case ftfrom_vals of
                dbf_SIGNED: ;
                else
                  raise EFRE_DB_Exception.Create(edb_MISMATCH,'a web requested datetime field filter must use signed filter values');
              end;
              qrydef.FilterDefDynamicRef.AddDatetimeFieldFilter(filter_key,ffn,vals,nft,fnegate,fallownull);
            end;

            function _C2S : TFRE_DB_Int64Array;inline;
            begin
              result := Filter.field('FILTERVALUES').ConvAsSignedArray;
            end;

            function _C2US : TFRE_DB_UInt64Array;inline;
            begin
              result := Filter.field('FILTERVALUES').ConvAsUnsignedArray;
            end;

            function _C2CURR : TFRE_DB_CurrencyArray;
            begin
              result := Filter.field('FILTERVALUES').ConvAsCurrencyArray;
            end;

            function _C2REAL : TFRE_DB_Real64Array;
            begin
              result := Filter.field('FILTERVALUES').ConvAsReal64Array;
            end;

            procedure ProcessUidFilter;
            var vals         : TFRE_DB_GUIDArray;
                sa           : TFRE_DB_StringArray;
                i            : NativeInt;
                refl_filter  : boolean;
                refl_spec    : TFRE_DB_NameTypeRLArray;
                refl_vals    : TFRE_DB_GUIDArray;
                expanded_uid : TFRE_DB_GUIDArray;
            begin
              if length(qrydef.ParentIds)>0 then { hack skip uid filter processing on child query}
                exit;
              if FREDB_StringInArray(uppercase(ffn),qrydef.DependencyRefIds) then
                begin { dependency ref UID Filter}
                  refl_spec := qrydef.GetReflinkSpec(ffn);
                  refl_vals := qrydef.GetReflinkStartValues(ffn);
                  if not qrydef.DepRefNegate then
                    qrydef.FilterDefDynamicRef.AddAutoDependencyFilter(filter_key,refl_spec,refl_vals,true,fallownull)
                  else
                    qrydef.FilterDefDynamicRef.AddAutoDependencyFilter(filter_key,refl_spec,refl_vals,false,fallownull);
                end
              else
                begin { "normal" UID Filter}
                  raise EFRE_DB_Exception.Create(edb_internal,'uid filter type not implemented from client');
                end
            end;

        begin
          nfts := Filter.field('NUMFILTERTYPE').AsString;
          if nfts<>'' then
            nft := FREDB_String2NumfilterType(nfts)
          else
            nft := dbnf_EXACT;

          ft         := FREDB_FilterTypeString2Filtertype(Filter.Field('FILTERTYPE').AsString);
          ffn        := uppercase(Filter.Field('FILTERFIELDNAME').AsString);
          case Filter.field('FILTERVALUES').FieldType of { value type does not propagate exact through json->dbo}
            fdbft_Byte:        ftfrom_vals := dbf_SIGNED;
            fdbft_Int16:       ftfrom_vals := dbf_SIGNED;
            fdbft_UInt16:      ftfrom_vals := dbf_SIGNED;
            fdbft_Int32:       ftfrom_vals := dbf_SIGNED;
            fdbft_UInt32:      ftfrom_vals := dbf_SIGNED;
            fdbft_Int64:       ftfrom_vals := dbf_SIGNED;
            fdbft_UInt64:      ftfrom_vals := dbf_UNSIGNED;
            fdbft_Real32:      ftfrom_vals := dbf_REAL64;
            fdbft_Real64:      ftfrom_vals := dbf_REAL64;
            fdbft_Currency:    ftfrom_vals := dbf_CURRENCY;
            fdbft_String:      ftfrom_vals := dbf_TEXT;
            fdbft_Boolean:     ftfrom_vals := dbf_BOOLEAN;
            fdbft_DateTimeUTC: ftfrom_vals := dbf_DATETIME;
            fdbft_GUID:        ftfrom_vals := dbf_UNSIGNED;
            fdbft_NotFound:    ftfrom_vals := dbf_EMPTY;
            else
              raise EFRE_DB_Exception.Create(edb_ERROR,'cannot determine filtertype from input filtervalues fieldtype=%s',[CFRE_DB_FIELDTYPE[Filter.field('FILTERVALUES').FieldType]]);
          end;
          if Filter.FieldOnlyExisting('ALLOWNULL',fld) then
            fallownull := fld.AsBoolean
          else
            fallownull := false;
          if Filter.FieldOnlyExisting('NEG',fld) then
            fnegate    := fld.AsBoolean
          else
            fnegate    := true;
          case ft of
            dbf_TEXT:
              begin
                if ftfrom_vals<>dbf_TEXT then
                  raise EFRE_DB_Exception.Create(edb_MISMATCH,'a string field filter must use string filter values');
                ftstr := Filter.field('STRFILTERTYPE').AsString;
                if ftstr<>'' then
                  sft := FREDB_String2StrFilterType(Filter.field('STRFILTERTYPE').AsString)
                else
                  sft := dbft_PART;
                qrydef.FilterDefDynamicRef.AddStringFieldFilter(filter_key,ffn,Filter.field('FILTERVALUES').AsStringArr[0],sft,fnegate,fallownull);
              end;
            dbf_SIGNED:
              begin
                case ftfrom_vals of
                  dbf_SIGNED: qrydef.FilterDefDynamicRef.AddSignedFieldFilter(filter_key,ffn,_C2S,FREDB_String2NumfilterType(Filter.field('NUMFILTERTYPE').AsString),fnegate,fallownull);
                  dbf_REAL64: qrydef.FilterDefDynamicRef.AddReal64FieldFilter(filter_key,ffn,_C2REAL,FREDB_String2NumfilterType(Filter.field('NUMFILTERTYPE').AsString),fnegate,fallownull);
                  else
                    raise EFRE_DB_Exception.Create(edb_MISMATCH,'a web requested signed field filter must use signed or real64 filter values');
                end;
              end;
            dbf_UNSIGNED:
                raise EFRE_DB_Exception.Create(edb_INTERNAL,'a web requested unsigned field filter is not implemented');
            dbf_CURRENCY:
              begin
                case ftfrom_vals of
                  dbf_SIGNED,
                  dbf_CURRENCY,
                  dbf_REAL64 : ;
                  else
                    raise EFRE_DB_Exception.Create(edb_MISMATCH,'a web requested currency field filter must use signed or real64 filter values');
                end;
                qrydef.FilterDefDynamicRef.AddCurrencyFieldFilter(filter_key,ffn,_C2CURR,FREDB_String2NumfilterType(Filter.field('NUMFILTERTYPE').AsString),fnegate,fallownull);
              end;
            dbf_DATETIME:  ProcessDateTimeFilter;
            dbf_BOOLEAN:   qrydef.FilterDefDynamicRef.AddBooleanFieldFilter(filter_key,ffn,Filter.field('FILTERVALUES').AsBoolean,fnegate,fallownull);
            dbf_GUID:      ProcessUIDFilter;
            dbf_SCHEME:    qrydef.FilterDefDynamicRef.AddSchemeObjectFilter(filter_key,Filter.field('FILTERVALUES').AsStringArr,fnegate);
            dbf_RIGHT:     qrydef.FilterDefDynamicRef.AddStdRightObjectFilter(filter_key,FREDB_RightSetString2RightSet(Filter.field('FILTERVALUES').AsString),qrydef.UserTokenRef.CloneToNewUserToken,fnegate);
            else raise EFRE_DB_Exception.Create(edb_ERROR,'unhandled filter type');
          end;
        end;

        procedure Prepare;
        var i,j : NativeInt;
            fld : IFRE_DB_Field;
        begin
          qrydef.FilterDefDynamicRef.RemoveAllFilters;
          if length(qrydef.ParentIds)>0 then { hack skip uid filter processing on child query}
            exit;
          SetLength(qrydef.DepFilterUids,Length(qrydef.DependencyRefIds));
          for i:=0 to high(qrydef.DependencyRefIds) do
            begin
              fld := web_input.FieldPath('DEPENDENCY.'+qrydef.DependencyRefIds[i]+'_REF.FILTERVALUES',true);
              if assigned(fld) then
                  begin
                    SetLength(qrydef.DepFilterUids[i],fld.ValueCount);
                    for j := 0 to High(qrydef.DepFilterUids) do
                      qrydef.DepFilterUids[i][j] := FREDB_H2G(fld.AsStringArr[j]);
                  end
            end;
        end;

    begin
      Prepare;
      if web_input.FieldOnlyExistingObject('DEPENDENCY',dop) then
        dop.ForAllObjectsFieldName(@AddFilter);
    end;

begin
  qrydef := SetupQryDefinitionBasic(web_input.Field('start').AsInt32,web_input.Field('count').AsInt32,strtoint(web_input.Field('QUERYID').AsString));
  qrydef.FullTextFilter := web_input.Field('FULLTEXT').AsString;
  if web_input.FieldExists('parentid') then
    begin { this is a child query }
      qrydef.ParentIds := FREDB_H2GArray(web_input.Field('parentid').AsString);
    end;
  if web_input.FieldExists('sort') then
    begin
      qrydef.OrderDefRef.ClearOrders;
      cnt := web_input.Field('sort').ValueCount;
      for i:=0 to cnt-1 do begin
        sort := web_input.Field('SORT').AsObjectItem[i];
        qrydef.OrderDefRef.AddOrderDef(sort.Field('PROPERTY').AsString,sort.Field('ASCENDING').AsBoolean,true);
      end;
    end;
  Processfilters;
  result := qrydef;
end;

function TFRE_DB_DERIVED_COLLECTION.ExecuteQryLocked(const qrydef: TFRE_DB_QUERY_DEF; out qry: TFRE_DB_QUERY_BASE): NativeInt;
var
    query_base_data : TFRE_DB_TRANS_RESULT_BASE;
    query           : TFRE_DB_QUERY_BASE;
    qry_ok          : boolean;

begin
  try
    result := 0;
    qry_ok := false;
    MustBeInitialized;
    query  := GFRE_DB_TCDM.GenerateQueryFromQryDef(qrydef);
    GFRE_DB_TCDM.LockManager;
    try
      try
        if not GFRE_DB_TCDM.GetTransformedDataLocked(query,query_base_data) then
          GFRE_DB_TCDM.NewTransformedDataLocked(query,self,query_base_data);
        query.SetBaseOrderedData(query_base_data,FDC_Session.GetSessionID);
        result := query.ExecuteQuery(nil);
        qry_ok := true;
        qry    := query;
      finally
        GFRE_DB_TCDM.UnlockManager;
      end;
    finally
      if not qry_ok then
        begin
          query.Free;
          qry := nil;
        end
    end;
  except
    raise;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.ExecutePointQry(const qrydef: TFRE_DB_QUERY_DEF): IFRE_DB_Object;
var
    query_base_data : TFRE_DB_TRANS_RESULT_BASE;
    query           : TFRE_DB_QUERY_BASE;
    qry_ok          : boolean;

    procedure Iterator(const obj : IFRE_DB_Object);
    begin
      result := obj;
    end;

begin
  try
    result := nil;
    qry_ok := false;
    MustBeInitialized;
    FDCollFiltersDyn.RemoveAllFilters;
    query  := GFRE_DB_TCDM.GenerateQueryFromQryDef(qrydef);
    GFRE_DB_TCDM.LockManager;
    try
      try
        if not GFRE_DB_TCDM.GetTransformedDataLocked(query,query_base_data) then
          GFRE_DB_TCDM.NewTransformedDataLocked(query,self,query_base_data);
        query.SetBaseOrderedData(query_base_data,FDC_Session.GetSessionID);
        query.ExecutePointQuery(@Iterator);
        qry_ok := true;
      finally
        query.UnlockBaseData;
        GFRE_DB_TCDM.UnlockManager;
      end;
    finally
      query.Free;
    end;
  except
    raise;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.First: IFRE_DB_Object;
var qrydef : TFRE_DB_QUERY_DEF;
begin
  qrydef := SetupQryDefinitionBasic(-1,1,0);
  result := ExecutePointQry(qrydef);
  if assigned(result) then
    FinalRightTransform(FDC_Session,result);
end;

function TFRE_DB_DERIVED_COLLECTION.Last: IFRE_DB_Object;
var qrydef : TFRE_DB_QUERY_DEF;
begin
  qrydef := SetupQryDefinitionBasic(-2,1,0);
  result := ExecutePointQry(qrydef);
  if assigned(result) then
    FinalRightTransform(FDC_Session,result);
end;

function TFRE_DB_DERIVED_COLLECTION.FetchIndexed(const idx: NativeInt): IFRE_DB_Object;
var qrydef : TFRE_DB_QUERY_DEF;
begin
  if idx<0 then
    raise EFRE_DB_Exception.Create(edb_ERROR,'index must be greater then zero');
  qrydef := SetupQryDefinitionBasic(idx,1,0);
  result := ExecutePointQry(qrydef);
  if assigned(result) then
    FinalRightTransform(FDC_Session,result);
end;

function TFRE_DB_DERIVED_COLLECTION.FetchInDerived(const ouid: TFRE_DB_GUID; out dbo: IFRE_DB_Object): boolean;
var qrydef : TFRE_DB_QUERY_DEF;
begin
  qrydef := SetupQryDefinitionBasic(0,0,0);
  qrydef.OnlyOneUID := ouid;
  dbo := ExecutePointQry(qrydef);
  result := assigned(dbo);
  if result then
    FinalRightTransform(FDC_Session,dbo);
end;

function TFRE_DB_DERIVED_COLLECTION.CollectionName(const unique: boolean): TFRE_DB_NameType;
begin
  if unique then
    result := FUniquename
  else
    result := Fname;
end;

function TFRE_DB_DERIVED_COLLECTION.GetStoreDescription: TFRE_DB_CONTENT_DESC;
begin
  case FDisplaytype of
    cdt_Listview:   result := TFRE_DB_STORE_DESC.create.Describe(FIdField,CWSF(@WEB_GET_GRID_DATA),FlabelFields,CWSF(@WEB_DESTROY_STORE),CWSF(@WEB_CLEAR_QUERY_RESULTS),CollectionName(true));
    cdt_Chooser:    result := TFRE_DB_STORE_DESC.create.Describe(FIdField,CWSF(@WEB_GET_CHOOSER_DATA),FlabelFields,CWSF(@WEB_DESTROY_STORE),CWSF(@WEB_CLEAR_QUERY_RESULTS),CollectionName(true));
    cdt_Chartview:  result := TFRE_DB_STORE_DESC.create.Describe(FIdField,CWSF(@WEB_GET_CHART_DATA),FlabelFields,CWSF(@WEB_DESTROY_STORE),CWSF(@WEB_CLEAR_QUERY_RESULTS),CollectionName(true));
    else
      raise EFRE_DB_Exception.Create(edb_ERROR,'INVALID DISAPLAYTYPE FOR STORE [%d] GETSTOREDESCRIPTION',[ord(FDisplaytype)]);
  end
end;

function TFRE_DB_DERIVED_COLLECTION.getDescriptionStoreId: String;
begin
  Result:=CollectionName;
end;


procedure TFRE_DB_DERIVED_COLLECTION.SetDisplayType(const CollectionDisplayType: TFRE_COLLECTION_DISPLAY_TYPE; const Flags: TFRE_COLLECTION_GRID_DISPLAY_FLAGS; const title: TFRE_DB_String; const CaptionFields: TFRE_DB_StringArray; const TreeNodeIconField: TFRE_DB_String; const item_menu_func: TFRE_DB_SERVER_FUNC_DESC; const item_details_func: TFRE_DB_SERVER_FUNC_DESC; const selection_dep_func: TFRE_DB_SERVER_FUNC_DESC; const tree_menu_func: TFRE_DB_SERVER_FUNC_DESC; const drop_func: TFRE_DB_SERVER_FUNC_DESC; const drag_func: TFRE_DB_SERVER_FUNC_DESC);
begin
  _CheckSetDisplayType (CollectionDisplayType);
  FGridDisplayFlags := Flags;
  FTitle        := title;
  FitemMenuFunc.Free;
  FitemMenuFunc := item_menu_func;
  FitemDetailsFunc.Free;
  FitemDetailsFunc := item_details_func;
  FselectionDepFunc.Free;
  FselectionDepFunc := selection_dep_func;
  FtreeMenuFunc.Free;
  FtreeMenuFunc := tree_menu_func;
  FdropFunc.Free;
  FdropFunc := drop_func;
  FdragFunc.Free;
  FdragFunc := drag_func;
  if Assigned(CaptionFields) then begin
    FlabelFields     := CaptionFields;
  end else begin
    SetLength(FlabelFields,1); FlabelFields[0]:='objname';
  end;
end;

procedure TFRE_DB_DERIVED_COLLECTION.SetDisplayTypeChart(const title: TFRE_DB_String; const chart_type: TFRE_DB_CHART_TYPE; const series_field_names: TFRE_DB_StringArray; const use_series_colors:boolean; const use_series_labels : boolean;const series_labels: TFRE_DB_StringArray; const showLegend: Boolean; const maxValue: Integer);
var i           : integer;
begin
  _CheckSetDisplayType (cdt_Chartview);
  _ClearMode;
  FTitle            := title;
  FTransform            := TFRE_DB_CHART_TRANSFORM.Create;
  with FTransform as TFRE_DB_CHART_TRANSFORM do begin
    FseriesFieldNames   := series_field_names;
    FUseSeriesColors    := use_series_colors;
    FUseSeriesLabels    := use_series_labels;
    FSeriesLabels       := series_labels;
    FShowLegend         := showLegend;
    FChartType          := chart_type;
    FMaxValue           := maxValue;
  end;
  //_FilterIt(false);
  //abort;
end;

{
  Outbound Reflinks are stored FIELD>TARGETSCHEME
  Inbound Reflinks are stored  FROMSCHEME<FIELD
}

procedure TFRE_DB_DERIVED_COLLECTION.SetParentToChildLinkField(const fieldname: TFRE_DB_NameTypeRL);
begin
  FParentChldLinkFldSpec := uppercase(fieldname);
  FParentLinksChild      := FREDB_SplitRefLinkDescription(fieldname,FParentChildField,FParentChildScheme);
  if FParentChildField='' then
    raise EFRE_DB_Exception.Create(edb_ERROR,'the scheme may be specified, but the field must be specified');
end;

function TFRE_DB_DERIVED_COLLECTION.GetDisplayDescription: TFRE_DB_CONTENT_DESC;

  function GetListViewDescription: TFRE_DB_VIEW_LIST_DESC;
  var vcd : TFRE_DB_VIEW_LIST_LAYOUT_DESC;
        i : NativeInt;
  begin
    _CheckTransformSet;
    vcd    := (FTransform as TFRE_DB_SIMPLE_TRANSFORM).GetViewCollectionDescription as TFRE_DB_VIEW_LIST_LAYOUT_DESC;
    result := TFRE_DB_VIEW_LIST_DESC.create.Describe(GetStoreDescription as TFRE_DB_STORE_DESC, vcd, FitemMenuFunc, FTitle,FGridDisplayFlags,FitemDetailsFunc,FselectionDepFunc,nil,FdropFunc,FdragFunc);
    for i := 0 to high(FSelectionDepEvents) do
      with FSelectionDepEvents[i] do
        result.AddFilterEvent(dc_name,ref_name);
  end;

  function GetChartDescription: TFRE_DB_CHART_DESC;
  var series_ids : TFRE_DB_StringArray;
      ct         : TFRE_DB_CHART_TRANSFORM;
      i          : integer;
  begin
    _CheckTransformSet;
    ct := FTransform as TFRE_DB_CHART_TRANSFORM; // Chart data must be Transformed now
    result := TFRE_DB_CHART_DESC.create.Describe(Ftitle,GetStoreDescription as TFRE_DB_STORE_DESC,ct.FseriesFieldNames,ct.FChartType,ct.FSeriesLabels,ct.FShowLegend,ct.FMaxValue);
  end;

begin
  case FDisplaytype of
    cdt_Listview:  result := GetListviewDescription;
    cdt_Chooser:   raise EFRE_DB_Exception.Create(edb_ERROR,'getDisplayDescription not available for type Chooser');
    cdt_Chartview: result := GetChartDescription;
    else raise EFRE_DB_Exception.Create(edb_ERROR,'DERIVED COLLECTION [%s] HAS AN INVALID DISPLAYTYPE SET [%d]',[CollectionName,ord(FDisplaytype)]);
  end;
end;

procedure TFRE_DB_DERIVED_COLLECTION.FinalRightTransform(const ses: IFRE_DB_UserSession; const transformed_filtered_cloned_obj: IFRE_DB_Object);
var conn : IFRE_DB_CONNECTION;
begin
  conn := ses.GetDBConnection;
  if (FTransform is TFRE_DB_SIMPLE_TRANSFORM)
      and assigned(TFRE_DB_SIMPLE_TRANSFORM(FTransform).FFinalRightTransform) then
        try
          TFRE_DB_SIMPLE_TRANSFORM(FTransform).FFinalRightTransform(conn.sys.GetCurrentUserTokenRef,transformed_filtered_cloned_obj,ses.GetSessionGlobalData,TFRE_DB_SIMPLE_TRANSFORM(FTransform).FFRTLangres);
        except
          on e:exception do
            begin
              GFRE_DBI.LogError(dblc_DB,'Custom transform failed %s',[e.Message]);
            end;
        end;
end;

function TFRE_DB_DERIVED_COLLECTION.WEB_GET_GRID_DATA(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
    query_base_data : TFRE_DB_TRANS_RESULT_BASE;
    query           : TFRE_DB_QUERY_BASE;
    qry_ok          : boolean;
    qrydef          : TFRE_DB_QUERY_DEF;

    function GetGridDataDescription: TFRE_DB_CONTENT_DESC;
    var
       cnt           : NativeInt;

      procedure GetData(const transformed_filtered_cloned_obj:IFRE_DB_Object);
      begin
        FinalRightTransform(FDC_Session,transformed_filtered_cloned_obj);
        TFRE_DB_STORE_DATA_DESC(result).addEntry(transformed_filtered_cloned_obj);
      end;

    begin
      cnt := 0;
      result := TFRE_DB_STORE_DATA_DESC.create;
      cnt := query.ExecuteQuery(@GetData);
      TFRE_DB_STORE_DATA_DESC(Result).Describe(cnt);
    end;

begin
  try
    qry_ok := false;
    MustBeInitialized;
    FDCollFiltersDyn.RemoveAllFilters;
    qrydef := SetupQryDefinitionFromWeb(input);
    query  := GFRE_DB_TCDM.GenerateQueryFromQryDef(qrydef);
    GFRE_DB_TCDM.LockManager;
    try
      try
        if not GFRE_DB_TCDM.GetTransformedDataLocked(query,query_base_data) then
          GFRE_DB_TCDM.NewTransformedDataLocked(query,self,query_base_data);
        query.SetBaseOrderedData(query_base_data,FDC_Session.GetSessionID);
        result := GetGridDataDescription;
        //writeln('----GDD');
        //writeln(result.DumpToString());
        //writeln('----GDD');
        qry_ok := true;
      finally
        query_base_data.UnlockBase;
        GFRE_DB_TCDM.UnlockManager;
      end;
    finally
      if not qry_ok then
        query.Free
      else
        begin
          GFRE_DB_TCDM.StoreQuery(query);
        end;
    end;
  except
    writeln('GRID DATA EXCEPTION : ',FName,' ',input.DumpToString());
    raise;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.WEB_GET_CHOOSER_DATA(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=WEB_GET_GRID_DATA(input,ses,app,conn);
end;

function TFRE_DB_DERIVED_COLLECTION.WEB_GET_CHART_DATA(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
    //pageinfo       : TFRE_DB_DC_PAGING_INFO;
     //order         : TFRE_DB_DC_ORDER_LIST;
    //sortfilterkeys : TFRE_DB_DC_STRINGFIELDKEY_LIST;
    QueryID        : String;
    series_id      : string;
    ChartTransForm : TFRE_DB_CHART_TRANSFORM;
     i      : integer;


  //function GetChartDataDescription:TFRE_DB_CHART_DATA_DESC;
  //var ok     : string;
  //    data   : TFRE_DB_Real32Array;
  //    uids   : TFRE_DB_GUIDArray;
  //    colors : TFRE_DB_StringArray;
  //    texts  : TFRE_DB_StringArray;
  //    leg    : TFRE_DB_StringArray;
  //    max    : Integer;
  //
  //    procedure GetData(const obj:TFRE_DB_Object);
  //    var
  //     res    : TFRE_DB_CHART_DATA_DESC;
  //   begin
  //     //uids[i] := obj.Field(series_id+'_uid').AsGUID;
  //     uids[i] := obj.UID;
  //     if obj.FieldExists(series_id) then
  //       data[i] := obj.Field(series_id).AsReal32
  //     else
  //       data[i] := 0;
  //     if ChartTransForm.FUseSeriesColors then begin
  //       colors[i] := obj.Field(series_id+'_col').AsString;
  //     end;
  //     if ChartTransForm.FUseSeriesLabels then begin
  //       texts[i] := obj.Field(series_id+'_lbl').AsString;
  //     end;
  //     if ChartTransForm.FShowLegend then begin
  //       leg[i] := obj.Field(series_id+'_leg').AsString;
  //     end;
  //     inc(i);
  //   end;
  //
  //begin
  //  series_id := input.FieldPath('query.sid').AsString;
  //  GFRE_DB.LogDebug(dblc_DB,'GET_CHART_DATA '+series_id);
  //  GFRE_DB.LogDebug(dblc_DB,'%s',[input.DumpToString(4)]);
  //
  //  //max            := FDBOList.Count;
  //  ChartTransForm := FTransform as TFRE_DB_CHART_TRANSFORM;
  //
  //  colors := nil;
  //  texts  := nil;
  //  leg    := nil;
  //  SetLength(data,max);
  //  SetLength(uids,max);
  //  if ChartTransForm.FUseSeriesColors then begin
  //    SetLength(colors,max);
  //  end;
  //  if ChartTransForm.FUseSeriesLabels then begin
  //    SetLength(texts,max);
  //  end;
  //  if ChartTransForm.FShowLegend then begin
  //    SetLength(leg,max);
  //  end;
  //  i:=0;
  //  ApplyToData(@GetData);
  //  result := TFRE_DB_CHART_DATA_DESC.create;
  //  result.setSeries(data,uids,colors,texts,leg);
  //end;

begin
  abort;
   //_CheckObserverAdded(true);
  //result := GetChartDataDescription;
end;

function TFRE_DB_DERIVED_COLLECTION.WEB_CLEAR_QUERY_RESULTS(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var qid : TFRE_DB_NameType;
begin
  qid := GFRE_DB_TCDM.FormQueryID(ses.GetSessionID,CollectionName(true),strtoint(input.Field('QUERYID').AsString));
  GFRE_DB_TCDM.RemoveQuery(qid);
  Result:=GFRE_DB_NIL_DESC;
end;


function TFRE_DB_DERIVED_COLLECTION.WEB_DESTROY_STORE(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  GFRE_DB_TCDM.DropAllQuerys(ses.GetSessionID,CollectionName(true));
  result := GFRE_DB_NIL_DESC;
end;


procedure TFRE_DB_TRANSFORMOBJECT.Finalize;
begin
  Free;
end;

function TFRE_DB_TRANSFORMOBJECT.Implementor: TObject;
begin
  result := self;
end;

function TFRE_DB_TRANSFORMOBJECT.Implementor_HC: TObject;
begin
  result := self;
end;

function TFRE_DB_TRANSFORMOBJECT.GetFirstFieldname: TFRE_DB_NameType;

  procedure Getfirst(var ft:TFRE_DB_FIELD_TRANSFORM;const idx :NativeInt;var halt : boolean);
  begin
    if ft.FOutFieldName<>'' then
      begin
        result := ft.FOutFieldName;
        halt   := true;
      end;
  end;

begin
  FTransformList.ForAllBreak(@GetFirst);
end;

function TFRE_DB_TRANSFORMOBJECT.IsReflinkSpecRelevant(const rlspec: TFRE_DB_NameTypeRL): boolean;

  procedure Check(var ft : TFRE_DB_FIELD_TRANSFORM ; const idx : NativeInt ; var halt_flag:boolean);
  var rls : TFRE_DB_NameTypeRLArray;
         i: NativeInt;
  begin
     rls := ft.RefLinkSpec;
     for i:=0 to high(rls) do
       begin
         if rls[i]=rlspec then
           begin
             result    := true;
             halt_flag := true;
           end;
       end;
  end;

begin
  result := false;
  FTransformList.ForAllBreak(@Check);
end;


constructor TFRE_DB_TRANSFORMOBJECT.Create;
begin
  //FTransformList.InitSparseList(nil,@FieldTransformNull,@FieldTransformSame,10);
  FTransformList.InitSparseListPtrCmp;
end;

destructor TFRE_DB_TRANSFORMOBJECT.Destroy;
   procedure FinalTrans(var tr : TFRE_DB_FIELD_TRANSFORM ; const idx : NativeInt ; var halt : boolean);
   begin
     tr.free;
   end;
begin
   FTransformList.ForAllBreak(@FinalTrans);
  inherited Destroy;
end;

function TFRE_DB_TRANSFORMOBJECT.TransformInOut(const conn: IFRE_DB_CONNECTION ; const input: IFRE_DB_Object): TFRE_DB_Object;
begin
  abort;
end;

function TFRE_DB_FieldSchemeDefinition.GetFieldName: TFRE_DB_NameType;
begin
  result := FFieldName;
end;

function TFRE_DB_FieldSchemeDefinition.getEnum: TFRE_DB_Enum;
begin
  result := Fenum;
end;

function TFRE_DB_FieldSchemeDefinition.getAddConfirm: Boolean;
begin
  Result := FaddConfirm;
end;

function TFRE_DB_FieldSchemeDefinition.GetFieldType: TFRE_DB_FIELDTYPE;
begin
  result := FFieldtype;
end;

function TFRE_DB_FieldSchemeDefinition.getIsPass: Boolean;
begin
  Result := FisPass;
end;

function TFRE_DB_FieldSchemeDefinition.getMultiValues: Boolean;
begin
  Result := FmultiValues;
end;

function TFRE_DB_FieldSchemeDefinition.getRequired: Boolean;
begin
  Result := Frequired;
end;


function TFRE_DB_FieldSchemeDefinition.GetSubSchemeName: TFRE_DB_NameType;
begin
  result := FSubscheme;
end;

function TFRE_DB_FieldSchemeDefinition.getValidator: TFRE_DB_ClientFieldValidator;
begin
  result := Fvalidator;
end;

function TFRE_DB_FieldSchemeDefinition.getValidatorParams: IFRE_DB_Object;
begin
  result := FvalidatorParams;
end;

function TFRE_DB_FieldSchemeDefinition.getValidatorI(var validator: IFRE_DB_ClientFieldValidator): boolean;
begin
  result := Assigned(Fvalidator);
  if result then
    validator := Fvalidator
  else
    validator := nil;
end;

function TFRE_DB_FieldSchemeDefinition.getEnumI(var enum: IFRE_DB_Enum): boolean;
begin
 result := Assigned(Fenum);
 if result then
   enum := FEnum
 else
   enum := nil;
end;

procedure TFRE_DB_FieldSchemeDefinition.setAddConfirm(AValue: Boolean);
begin
  FaddConfirm := AValue;
end;

procedure TFRE_DB_FieldSchemeDefinition.setisPass(AValue: Boolean);
begin
  FisPass := AValue;
end;

procedure TFRE_DB_FieldSchemeDefinition.setEnum(AValue: TFRE_DB_Enum);
begin
  Fenum := AValue;
end;

procedure TFRE_DB_FieldSchemeDefinition.setMultiValues(AValue: Boolean);
begin
  FmultiValues := AValue;
end;

procedure TFRE_DB_FieldSchemeDefinition.setRequired(AValue: Boolean);
begin
  Frequired :=AValue;
end;

procedure TFRE_DB_FieldSchemeDefinition.setValidator(AValue: TFRE_DB_ClientFieldValidator);
begin
  Fvalidator := AValue;
end;


procedure TFRE_DB_FieldSchemeDefinition.addDepField(const fieldName: TFRE_DB_String;const disablesField:Boolean);
var
  tmpField: TFRE_DB_FieldSchemeDefinition;
  depObj  : R_Depfieldfield;
begin
  if not getParentScheme.GetSchemeField(fieldName,tmpField) then begin
    raise EFRE_DB_Exception.Create(edb_ERROR,'Dependent field ' + fieldName + ' not found');
  end;
  if not ((FieldType=fdbft_Boolean) or (FieldType=fdbft_ObjLink)) then begin
    raise EFRE_DB_Exception.Create(edb_ERROR,'Dependent fields can only be defined on boolean or objlink fields');
  end;

  depObj.depFieldName  := fieldName;
  depObj.disablesField := disablesField;
  FDepFields.Add(depObj);
end;

procedure TFRE_DB_FieldSchemeDefinition.ForAllDepfields(const depfielditerator: TFRE_DB_Depfielditerator);

  procedure iterate(var df : R_Depfieldfield ; const idx : NativeInt ; var halt : boolean);
  begin
    depfielditerator(df);
  end;

begin
  FDepFields.ForAllBreak(@iterate);
end;

procedure TFRE_DB_FieldSchemeDefinition.addVisDepField(const fieldName: TFRE_DB_String; const visibleValue: String);
var
  tmpField  : TFRE_DB_FieldSchemeDefinition;
  visDepObj : R_VisDepfieldfield;
begin
  if not getParentScheme.GetSchemeField(fieldName,tmpField) then begin
    raise EFRE_DB_Exception.Create(edb_ERROR,'Visibility dependent field ' + fieldName + ' not found');
  end;
  if not Assigned(FEnum) then begin
    raise EFRE_DB_Exception.Create(edb_ERROR,'Visibility dependent fields can only be defined on enum fields');
  end;

  visDepObj.visDepFieldName := fieldName;
  visDepObj.visibleValue    := visibleValue;
  FVisDepFields.Add(visDepObj);
end;

procedure TFRE_DB_FieldSchemeDefinition.ForAllVisDepfields(const depfielditerator: TFRE_DB_VisDepfielditerator);

  procedure iterate(var df : R_VisDepfieldfield ; const idx : NativeInt ; var halt : boolean);
  begin
    depfielditerator(df);
  end;

begin
  FVisDepFields.ForAllBreak(@iterate);
end;

function TFRE_DB_FieldSchemeDefinition.getParentScheme: TFRE_DB_SchemeObject;
begin
  Result:=FScheme; //;Parent.Parent as TFRE_DB_SchemeObject;
end;

function local_DepfieldNullCompare(const df : P_Depfieldfield):boolean;
begin
  result := df^.depFieldName='';
end;

function local_DepfieldCompare(const df1,df2 : P_Depfieldfield):boolean;
begin
  result := uppercase(df1^.depFieldName)=uppercase(df2^.depFieldName);
end;

function local_VisDepfieldNullCompare(const df : P_VisDepfieldfield):boolean;
begin
  result := df^.visDepFieldName='';
end;

function local_VisDepfieldCompare(const df1,df2 : P_VisDepfieldfield):boolean;
begin
  result := (uppercase(df1^.visDepFieldName)=uppercase(df2^.visDepFieldName)) and (df1^.visibleValue=df2^.visibleValue);
end;

constructor TFRE_DB_FieldSchemeDefinition.Create;
var
  rdf  : R_Depfieldfield;
  rvdf : R_VisDepfieldfield;
begin
  FillByte(rdf,sizeof(R_Depfieldfield),0);
  FDepFields.InitSparseList(rdf,@local_DepfieldNullCompare,@local_DepfieldCompare,1);
  FillByte(rvdf,sizeof(R_VisDepfieldfield),0);
  FVisDepFields.InitSparseList(rvdf,@local_VisDepfieldNullCompare,@local_VisDepfieldCompare,1);
end;

destructor TFRE_DB_FieldSchemeDefinition.Destroy;
begin
  FvalidatorParams.Free;
  inherited Destroy;
end;



function TFRE_DB_FieldSchemeDefinition.IsACalcField: Boolean;
begin
  result := assigned(FCalcMethod);
end;

function TFRE_DB_FieldSchemeDefinition.SetupFieldDef(const is_required: boolean; const is_multivalue: boolean; const enum_key: TFRE_DB_NameType; const validator_key: TFRE_DB_NameType; const is_pass: Boolean; const add_confirm: Boolean; const validator_params: TFRE_DB_Object): TFRE_DB_FieldSchemeDefinition;
var lEnum  : TFRE_DB_Enum;
    lValid : TFRE_DB_ClientFieldValidator;
begin
  required    := is_required;
  multiValues := is_multivalue;
  isPass      := is_pass;
  addConfirm  := add_confirm;
  if enum_key<>'' then begin
    if GFRE_DB.GetSysEnum(enum_key,lEnum) then begin
      setEnum(lEnum);
    end else begin
      raise EFRE_DB_Exception.Create(edb_INTERNAL,'the client field enum[%s] could not be fetched from the database',[enum_key]);
    end;
  end;
  if validator_key<>'' then begin
    if GFRE_DB.GetSysClientFieldValidator(validator_key,lValid) then
      begin
        setValidator(lValid);
        if assigned(validator_params) then
          begin
            FvalidatorParams.Free;
            FvalidatorParams := validator_params;
          end;
       end
    else
      raise EFRE_DB_Exception.Create(edb_INTERNAL,'the client field validator[%s] could not be fetched from the database',[validator_key]);
  end;
  result:=self;
end;

function TFRE_DB_FieldSchemeDefinition.SetupFieldDefI(const is_required: boolean; const is_multivalue: boolean; const enum_key: TFRE_DB_NameType; const validator_key: TFRE_DB_NameType; const is_pass: Boolean; const add_confirm: Boolean; const validator_params: IFRE_DB_Object): IFRE_DB_FieldSchemeDefinition;
begin
  if assigned(validator_params) then
    result := SetupFieldDef(is_required,is_multivalue,enum_key,validator_key,is_pass,add_confirm,validator_params.Implementor_HC as TFRE_DB_Object)
  else
    result := SetupFieldDef(is_required,is_multivalue,enum_key,validator_key,is_pass,add_confirm,nil);
end;

procedure TFRE_DB_FieldSchemeDefinition.SetCalcMethod(const calc_method: IFRE_DB_CalcMethod);
begin
  FCalcMethod   := calc_method;
end;

function TFRE_DB_FieldSchemeDefinition.GetSubScheme: TFRE_DB_SchemeObject;
var scheme_name:TFRE_DB_String;
begin
  result := nil;
  scheme_name := SubschemeName;
  if (scheme_name<>'') and (FSubSchemeObj=nil) then begin
    if GFRE_DB.GetSystemScheme(scheme_name,FSubSchemeObj)=false then begin
      raise EFRE_DB_Exception.Create(edb_INTERNAL,'critical subscheme inconsistency cant fetch subfieldscheme '+scheme_name+' for '+(FScheme.DefinedSchemeName));
    end;
  end;
  result := FSubSchemeObj;
end;

function TFRE_DB_FieldSchemeDefinition.GetSubSchemeI: IFRE_DB_SchemeObject;
begin
  result := GetSubScheme;
end;


function TFRE_DB_FieldSchemeDefinition.ValidateField(const field_to_check: TFRE_DB_FIELD; const raise_exception: boolean): boolean;
var lValidator : TFRE_DB_ClientFieldValidator;
    lEnum      : TFRE_DB_Enum;

  procedure RaiseOrExit(const msg:TFRE_DB_String;const params:array of const);
  begin
    if raise_exception then begin
      raise EFRE_DB_Exception.Create(edb_ERROR,msg,params);
    end;
    result:=false;
  end;

begin
  result:=true;
  if uppercase(field_to_check.FieldName) <> uppercase(FieldName) then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'fieldvalidation: scheme[%s] fieldnames differ ? [%s]<>[%s]',[getParentScheme.DefinedSchemeName,field_to_check.FieldName,FieldName]);
  if field_to_check.FieldType <> FieldType then
    RaiseOrExit('fieldvalidation: scheme[%s] field [%s] the fieldtype[%s] of the object validated against the schemefieldtype[%s] is different',[getParentScheme.DefinedSchemeName,field_to_check.FieldName,field_to_check.FieldTypeAsString,CFRE_DB_FIELDTYPE[FieldType]]);
  if not result then
    exit;
  lValidator := getValidator;
  if (not multiValues)
     and (field_to_check.ValueCount>1) then
       RaiseOrExit('fieldvalidation: scheme[%s] field[%s / %s] should not have multivalues (array), count=%d',[getParentScheme.DefinedSchemeName,field_to_check.FieldName,field_to_check.FieldTypeAsString,field_to_check.ValueCount]);
  if not result then
    exit;
  if assigned(lValidator) then
    begin
      result   := lValidator.CheckField(field_to_check,raise_exception);
      if not Result then exit;
    end;
  lEnum      := getEnum;
  if assigned(lEnum) then
    begin
      result   := lEnum.CheckField(field_to_check,raise_exception);
      if not Result then exit;
    end;
  if FieldType=fdbft_Object then
    begin
      if SubschemeName<>field_to_check.AsObject.SchemeClass then
        RaiseOrExit('fieldvalidation: scheme[%s] field [%s] -> the subfieldscheme[%s] of the object validated against the scheme defined subfieldtype[%s] is different',[getParentScheme.DefinedSchemeName,field_to_check.FieldName,field_to_check.AsObject.SchemeClass,SubschemeName]);
      if not result then exit;
      GetSubScheme.ValidateObject(field_to_check.AsObject,raise_exception);
    end;
end;

function TFRE_DB_FieldSchemeDefinition.ValidateFieldI(const field_to_check: IFRE_DB_FIELD; const raise_exception: boolean): boolean;
begin
  result := ValidateField(field_to_check.Implementor as TFRE_DB_FIELD,raise_exception);
end;


function TFRE_DB_SchemeObject.AddSchemeField(const newfieldname: TFRE_DB_NameType; const newfieldtype: TFRE_DB_FIELDTYPE ; const sub_scheme:TFRE_DB_NameType):TFRE_DB_FieldSchemeDefinition;
var lFieldSchemeDefinition:TFRE_DB_FieldSchemeDefinition;
    upnewfieldname : TFRE_DB_NameType;

  procedure FieldDefExists(var fd : TFRE_DB_FieldSchemeDefinition ; const idx:NativeInt ; var halt_flag:boolean);
  begin
    if fd.FFieldName=upnewfieldname then
      halt_flag := true;
  end;

begin
  if newfieldtype=fdbft_Object then
    raise EFRE_DB_Exception.Create(edb_ERROR,'AddschemeField: schemefield '+newfieldname+', objectfields must be added with AddSchemeFieldSubscheme');

  upnewfieldname := uppercase(newfieldname);

  if upnewfieldname='UID' then
    raise EFRE_DB_Exception.Create(edb_INVALID_PARAMS,'It is not allowed to redefine the special UID field.');

  if upnewfieldname='DOMAINID' then
    raise EFRE_DB_Exception.Create(edb_INVALID_PARAMS,'It is not allowed to redefine the special DOMAIND field.');

  if GetFieldDef(upnewfieldname,lFieldSchemeDefinition) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'AddschemeField: schemefield '+newfieldname+' is already set');

  lFieldSchemeDefinition                       := TFRE_DB_FieldSchemeDefinition.Create;
  FFieldDefs.Add(lFieldSchemeDefinition);
  lFieldSchemeDefinition.FFieldName            := upnewfieldname;
  if sub_scheme='' then
    begin
      lFieldSchemeDefinition.FFieldType            := newfieldtype;
    end
  else
    begin
      lFieldSchemeDefinition.FFieldType            := fdbft_Object;
      lFieldSchemeDefinition.FSubscheme            := sub_scheme;
    end;
  lFieldSchemeDefinition.Frequired             := false;
  lFieldSchemeDefinition.FisPass               := false;
  lFieldSchemeDefinition.FaddConfirm           := false;
  lFieldSchemeDefinition.FmultiValues          := false;
  lFieldSchemeDefinition.FScheme               := self;
  result                                       := lFieldSchemeDefinition;
  if newfieldtype=fdbft_Stream then { automagically add a stream type field (text) for a stream field definition}
    begin
      lFieldSchemeDefinition                       := TFRE_DB_FieldSchemeDefinition.Create;
      FFieldDefs.Add(lFieldSchemeDefinition);
      lFieldSchemeDefinition.FFieldName            := upnewfieldname+cFRE_DB_STKEY;
      lFieldSchemeDefinition.FFieldType            := fdbft_String;
      lFieldSchemeDefinition.Frequired             := false;
      lFieldSchemeDefinition.FisPass               := false;
      lFieldSchemeDefinition.FaddConfirm           := false;
      lFieldSchemeDefinition.FmultiValues          := false;
      lFieldSchemeDefinition.FScheme               := self;
    end;
  if newfieldtype=fdbft_Stream then { automagically add a stream etag type field (text) for a stream field definition}
    begin
      lFieldSchemeDefinition                       := TFRE_DB_FieldSchemeDefinition.Create;
      FFieldDefs.Add(lFieldSchemeDefinition);
      lFieldSchemeDefinition.FFieldName            := upnewfieldname+cFRE_DB_ST_ETAG;
      lFieldSchemeDefinition.FFieldType            := fdbft_String;
      lFieldSchemeDefinition.Frequired             := false;
      lFieldSchemeDefinition.FisPass               := false;
      lFieldSchemeDefinition.FaddConfirm           := false;
      lFieldSchemeDefinition.FmultiValues          := false;
      lFieldSchemeDefinition.FScheme               := self;
    end;
end;

function TFRE_DB_SchemeObject.AddSchemeFieldSubscheme(const newfieldname:TFRE_DB_NameType ; const sub_scheme:TFRE_DB_NameType):TFRE_DB_FieldSchemeDefinition;
begin
  result := AddSchemeField(newfieldname,fdbft_NotFound,sub_scheme);
end;


function TFRE_DB_SchemeObject.AddSchemeFieldI(const newfieldname: TFRE_DB_NameType; const newfieldtype: TFRE_DB_FIELDTYPE): IFRE_DB_FieldSchemeDefinition;
begin
  result := AddSchemeField(newfieldname,newfieldtype,'');
end;

function TFRE_DB_SchemeObject.AddCalcSchemeField(const newfieldname: TFRE_DB_NameType; const newfieldtype: TFRE_DB_FIELDTYPE; const calc_method: IFRE_DB_CalcMethod): IFRE_DB_FieldSchemeDefinition;
begin
   result := AddSchemeField(newfieldname,newfieldtype,'');
   result.SetCalcMethod(calc_method);
end;



function TFRE_DB_SchemeObject.AddSchemeFieldSubschemeI(const newfieldname: TFRE_DB_NameType; const sub_scheme: TFRE_DB_NameType): IFRE_DB_FieldSchemeDefinition;
begin
  result := AddSchemeFieldSubscheme(newfieldname,sub_scheme);
end;


function TFRE_DB_SchemeObject.GetSchemeField(const fieldname: TFRE_DB_NameType;var fieldschemedef:TFRE_DB_FieldSchemeDefinition): boolean;
var current                : TFRE_DB_SchemeObject;
    lFieldSchemeDefinition : TFRE_DB_FieldSchemeDefinition;
    upfieldname            : TFRE_DB_NameType;

begin
  result  := false;
  current := self;
  if (fieldname='UID') or
     (fieldname='DOMAINID') then
       exit;
  repeat
    if GetFieldDef(UpperCase(fieldname),fieldschemedef) then
      exit(true);
    current := current.GetParentScheme;
    if current=nil then break;
  until false;
end;

function TFRE_DB_SchemeObject.GetSchemeFieldI(const fieldname: TFRE_DB_NameType; var fieldschemedef: IFRE_DB_FieldSchemeDefinition): boolean;
var lFieldSchemeDef : TFRE_DB_FieldSchemeDefinition;
begin
  result := GetSchemeField(fieldname,lFieldSchemeDef);
  if result then begin
    fieldschemedef := lFieldSchemeDef;
  end else begin
    fieldschemedef := nil;
  end;
end;

function TFRE_DB_SchemeObject.GetSchemeField(const fieldname: TFRE_DB_NameType): TFRE_DB_FieldSchemeDefinition;
begin
 if not GetSchemeField(fieldname,result) then raise EFRE_DB_Exception.Create(edb_ERROR,'GetSchemeField: schemefield '+fieldname+', objectfield does not exist');
end;

function TFRE_DB_SchemeObject.GetSchemeFieldI(const fieldname: TFRE_DB_NameType): IFRE_DB_FieldSchemeDefinition;
begin
 result:=GetSchemeField(fieldname);
end;

function TFRE_DB_SchemeObject.IsA(const schemename: shortstring): Boolean;
var current : TFRE_DB_SchemeObject;
begin
  result  := false;
  current := self;
  repeat
    if current.DefinedSchemeName=uppercase(schemename) then begin
      exit(true);
    end;
    current := current.GetParentScheme;
    if current=nil then break;
  until false;
end;


procedure TFRE_DB_SchemeObject.SetSimpleSysDisplayField(const field_name: TFRE_DB_String);
begin
  simple_df := field_name;
end;

procedure TFRE_DB_SchemeObject.SetSysDisplayField(const field_names: TFRE_DB_NameTypeArray; const format: TFRE_DB_String);
begin
  formatted_df  := field_names;
  formatted_dff := format;
end;

function TFRE_DB_SchemeObject.GetFormattedDisplay(const obj : TFRE_DB_Object): TFRE_DB_String;
begin
  if length(formatted_df)>0 then begin
    result := obj.FieldPathListFormat(formatted_df,formatted_dff,'{}');
  end else
    if simple_df<>'' then begin
      result := obj.FieldPathListFormat(TFRE_DB_NameTypeArray.Create(simple_df),'%s','{}');
    end else begin
      if obj.FieldExists('_simpleformat') then
        begin
          result:=obj.field('_simpleformat').AsString;
        end
      else
        result := DefinedSchemeName+' : '+obj.UID_String;
    end;
end;

function TFRE_DB_SchemeObject.GetFormattedDisplayI(const obj: IFRE_DB_Object): TFRE_DB_String;
begin
  result := GetFormattedDisplay(obj.Implementor as TFRE_DB_Object);
end;

function TFRE_DB_SchemeObject.FormattedDisplayAvailable(const obj : TFRE_DB_Object): boolean;
begin
  if length(formatted_df)>0 then begin
    result := true;
  end else
  if simple_df<>'' then begin
    result := true;
  end else begin
    result := false;
  end;
end;

function TFRE_DB_SchemeObject.FormattedDisplayAvailableI(const obj: IFRE_DB_Object): boolean;
begin
  result := FormattedDisplayAvailable(obj.Implementor as TFRE_DB_Object);
end;

function TFRE_DB_SchemeObject.DefinedSchemeName: TFRE_DB_String;
begin
  result := uppercase(FSchemeClass);
end;

procedure TFRE_DB_SchemeObject.Strict(const only_defined_fields: boolean);
begin
  FStrict := only_defined_fields;
end;


procedure TFRE_DB_SchemeObject._InternalSetParentScheme(const parentscheme: TFRE_DB_Schemeobject);
var lCheckDbo:TFRE_DB_SchemeObject;
    names:ShortString;
begin
 lCheckDbo := parentscheme;
 repeat
   names := lCheckDbo.DefinedSchemeName;
   if lCheckDbo=self then raise EFRE_DB_Exception.Create(edb_ERROR,'no circles with parentschemes ');
   lCheckDbo := lCheckDbo.Getparentscheme;
 until lCheckDbo=nil;
 FParentScheme := parentscheme;  // TODO Check if Parent Fields and My Fields don't override each other / Methods etc..
 //FParentSchemeObj := parentscheme;
end;



procedure TFRE_DB_SchemeObject._BuildHardcodeMethods;
var dbo_class   : TFRE_DB_OBJECTCLASS;
    dbo_classex : TFRE_DB_OBJECTCLASSEX;
    schemename  : TFRE_DB_NameType;
begin
  if FHasHardcodeClass then begin
    schemename := DefinedSchemeName;
    dbo_class := GFRE_DB.GetObjectClass(schemename);
    if not assigned(dbo_class) then begin
      dbo_classex := GFRE_DB.GetObjectClassEx(schemename);
      if assigned(dbo_classex) then begin
        FIMI_Methods := dbo_classex.Get_DBI_InstanceMethods;
      end else begin
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'could not find codeclass[%s] while building IMI Methods',[DefinedSchemeName]);
      end;
    end else begin
      FIMI_Methods := dbo_class.Get_DBI_InstanceMethods;
    end;
  end;
  FHC_MethodsBuild := true;
end;


procedure TFRE_DB_SchemeObject._FieldAccessCheck(name: TFRE_DB_NameType);
var lFieldSchemeDefinition : TFRE_DB_FieldSchemeDefinition;
    exists                 : Boolean;
    lParentscheme          : TFRE_DB_SchemeObject;
    upname                 : TFRE_DB_NameType;
begin
  upname := uppercase(name);
  if FStrict then begin
    exists := GetFieldDef(name,lFieldSchemeDefinition);
    if not exists then begin
      lParentscheme := GetParentScheme;
      if assigned(lParentscheme) then begin
        lParentscheme._FieldAccessCheck(name);
        exit;
      end;
    end;
    if not exists
       and (upname<>'DOMAINID') then
         raise EFRE_DB_Exception.Create(edb_MISMATCH,'Field <%s> is not defined in Scheme <%s>, and strict access is required.',[name,DefinedSchemeName]);
  end;
end;

function TFRE_DB_SchemeObject.Implementor: TObject;
begin
  result := self;
end;

function InputGroupNull       (const ig      : PFRE_DB_InputGroupSchemeDefinition):boolean;
begin
 result := not assigned(ig^);
end;

function CompareInputGroups   (const ig1,ig2 : PFRE_DB_InputGroupSchemeDefinition):boolean;
begin
 result := ig1^.groupid = ig2^.groupid;
end;

procedure TFRE_DB_SchemeObject.SetParentSchemeByName(const parentschemename: TFRE_DB_String);
var lscheme_object:TFRE_DB_SchemeObject;
begin
  if not GFRE_DB.GetSystemScheme(parentschemename,lscheme_object) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'Trying to set the parentscheme, but cant get parentscheme by name '+parentschemename+' for '+DefinedSchemeName+' GUESS:maybe wrong initialization order ?');
  _InternalSetParentScheme(lscheme_object);
end;


function TFRE_DB_SchemeObject.GetParentScheme: TFRE_DB_SchemeObject;
begin
  result := FParentScheme;
end;

function TFRE_DB_SchemeObject.GetParentSchemeI: IFRE_DB_SchemeObject;
begin
  result := GetParentScheme;
end;

function TFRE_DB_SchemeObject.GetParentSchemeName: TFRE_DB_String;
begin
  result := FParentScheme.DefinedSchemeName;
end;

function TFRE_DB_SchemeObject.GetExplanation: TFRE_DB_String;
begin
  result:=FExplanation;
end;

procedure TFRE_DB_SchemeObject.SetExplanation(AValue: TFRE_DB_String);
begin
  FExplanation:=AValue;
end;

function TFRE_DB_SchemeObject.GetSchemeType: TFRE_DB_SchemeType;
begin
  result := FSchemeType;
end;


procedure TFRE_DB_SchemeObject.SetObjectFieldsWithScheme(const Raw_Object: TFRE_DB_OBject; const Update_Object: TFRE_DB_Object; const new_object: boolean; const DBConnection: TFRE_DB_CONNECTION; const schemeType: TFRE_DB_String);

  procedure SetUpdateObjectFields(const Field : TFRE_DB_FIELD);
  var scheme_field_type : TFRE_DB_FIELDTYPE;
      field_name        : TFRE_DB_String;
      field_val         : TFRE_DB_StringArray;
      scheme_field_def  : TFRE_DB_FieldSchemeDefinition;
      raw_field_type    : TFRE_DB_FIELDTYPE;
      sub_object        : TFRE_DB_Object;
      sub_scheme        : TFRE_DB_SchemeObject;
      sub_scheme_name   : TFRE_DB_String;
      uid_update        : boolean;
      fetch_uid         : TFRE_DB_GUID;
      fetch_up_object   : TFRE_DB_Object;
      raw_multi_vals    : boolean;
      raw_empty_array   : boolean;
      raw_clear_marker  : boolean;
      scheme_multi_vals : boolean;
      work_fld          : TFRE_DB_FIELD;

      procedure _ObjectUpdate;
      begin
         sub_scheme_name := scheme_field_def.GetSubSchemeName;
         if sub_scheme_name='' then begin
           raise EFRE_DB_Exception.Create(edb_ERROR,'subfield defined but with no scheme / update impossible / subfield=[%s]',[field_name]);
         end;
         uid_update := false;
         if raw_field_type=fdbft_String then begin // it must be an UID for a subobject fetch
           uid_update := true;
           if length(field_val)>0 then begin
             if Length(field_val)=1 then begin;
               try
                 fetch_uid := FREDB_H2G(field_val[0]);
               except
                 raise EFRE_DB_Exception.Create(edb_ERROR,'invalid guid while updating a subfield object with a guid TFRE_DB_String [%s]',[field_val[0]]);
               end;
               CheckDbResult(DBConnection.Fetch(fetch_uid,fetch_up_object),Format('cannot fetch object guid [%s] while updating a subfield object with a guid TFRE_DB_String / not found',[field_val[0]]));
               fetch_up_object := fetch_up_object.CloneToNewObject;
             end else begin
               raise EFRE_DB_Exception.Create(edb_ERROR,'when updating a subfield object, onle one guid is allowed not an array [%s]',[GFRE_DB.StringArray2String(field_val)]);
             end;
           end else begin
             fetch_up_object := nil; //remove it
           end;
         end;
         sub_scheme := scheme_field_def.GetSubScheme;
         if new_object then begin
           if uid_update then begin
             if assigned(fetch_up_object) then begin
               Update_Object.Field(field_name).AsObject := fetch_up_object;
             end else begin
               Update_Object.DeleteField(field_name);
             end;
           end else begin
             sub_object := GFRE_DBI.NewObjectSchemeByName(sub_scheme_name).Implementor as TFRE_DB_Object;
             sub_scheme.SetObjectFieldsWithScheme(Field.AsObject,sub_object,true,DBConnection,schemeType);
             Update_Object.Field(field_name).AsObject:=sub_object;
           end;
         end else begin
           if uid_update then begin // uid update replaces the object
             //if not Update_Object.FieldExists(field_name) then begin
             //  raise EFRE_DB_Exception.Create(edb_FIELD_NOT_FOUND,'update of subfield object is not allowed if the specified subfield object does not exist. subfield=[%s]',[field_name]);
             //end;
             if assigned(fetch_up_object) then begin
               Update_Object.Field(field_name).AsObject := fetch_up_object;
             end else begin
               Update_Object.DeleteField(field_name);
             end;
           end else begin
             sub_object := Update_Object.Field(field_name).AsObject;
             if sub_object.SchemeClass <> sub_scheme_name then begin
               //writeln('subobject **********',sub_object.DumpToString());
               raise EFRE_DB_Exception.Create(edb_MISMATCH,'update of subfield object is only allowed if the specified subfield object has the same scheme. subfield=[%s] subfield scheme=[%s] update object scheme=[%s]',[field_name,sub_scheme_name,sub_object.SchemeClass]);
             end;
             sub_scheme.SetObjectFieldsWithScheme(Field.AsObject,sub_object,false,DBConnection,schemeType);
           end;
         end;
      end;

      function All_Update_Object_Fields_Empty(const obj:TFRE_DB_Object):boolean;
      var myresult:boolean;

        function Iterator(const fld:TFRE_DB_FIELD):boolean;
        begin
          result := false;
          case fld.FieldType of
            fdbft_GUID   : ; //ignore
            fdbft_String : if fld.AsString<>'' then begin
                             myresult:=false;
                             result:=true; // break;
                           end;
            fdbft_Object : All_Update_Object_Fields_Empty(fld.AsObject);
            else raise EFRE_DB_Exception.Create(edb_ERROR,'EmptyCheck Failed FieldType unexpected [%s]',[CFRE_DB_FIELDTYPE[fld.FieldType]]);
          end;
        end;

      begin
        myresult := true;
        obj.ForAllFieldsBreak(@Iterator);
        result := myresult;
      end;

  begin
    field_name := field.FieldName;
    if uppercase(field_name) = 'UID' then
      exit; // no uid updates
    if uppercase(field_name) = 'DOMAINID' then
      exit; // no uid updates
    if GetSchemeField(field_name,scheme_field_def) then begin
      raw_field_type     := field.FieldType;
      scheme_field_type  := scheme_field_def.FieldType;
      raw_multi_vals     := field.ValueCount>1;
      raw_empty_array    := field.ValueCount=0;
      raw_clear_marker   := (field.ValueCount=1) and (field.AsString=cFRE_DB_SYS_CLEAR_VAL_STR);
      scheme_multi_vals  := scheme_field_def.multiValues;
      if raw_multi_vals and not scheme_multi_vals then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'error updating field [%s],raw object has multivalues but the scheme does not allow this',[field.FieldName]);
      if (scheme_field_type<>fdbft_Object) or ((scheme_field_type=fdbft_Object) and (raw_field_type=fdbft_String)) then begin
        field_val   := field.AsStringArr;       //TODO -> EXTEND TO FIELD ARRAYS !!
      end;
      if (raw_field_type<>fdbft_String) and
         ((scheme_field_type<>fdbft_Object) and (scheme_field_type<>fdbft_Stream)) then
        begin
          raise EFRE_DB_Exception.Create(edb_INTERNAL,'the raw object has a field which is not a raw TFRE_DB_String field [%s], but the fielddef [%s] from the scheme object [%s] is not a sub object or a stream !',[CFRE_DB_FIELDTYPE[raw_field_type],CFRE_DB_FIELDTYPE[scheme_field_type],DefinedSchemeName]);
        end;
      if raw_empty_array and (not scheme_multi_vals) then begin
        exit; //Don't set empty arrays to non arrays
      end;
      if (scheme_field_type<>fdbft_Object) and ( raw_clear_marker or (length(field_val)=0) ) then begin
        Update_Object.Field(field_name).Clear;
      end else begin
        case scheme_field_type of
          fdbft_NotFound    : raise EFRE_DB_Exception.Create(edb_INTERNAL,'logic failure');
          fdbft_GUID        : Update_Object.Field(field_name).AsGUIDArr        := GFRE_DB.StringArray2GuidArray(field_val);
          fdbft_Byte        : Update_Object.Field(field_name).AsByteArr        := GFRE_DB.StringArray2ByteArray(field_val);
          fdbft_Int16       : Update_Object.Field(field_name).AsInt16Arr       := GFRE_DB.StringArray2Int16Array(field_val);
          fdbft_UInt16      : Update_Object.Field(field_name).AsUInt16Arr      := GFRE_DB.StringArray2UInt16Array(field_val);
          fdbft_Int32       : Update_Object.Field(field_name).AsInt32Arr       := GFRE_DB.StringArray2Int32Array(field_val);
          fdbft_UInt32      : Update_Object.Field(field_name).AsUInt32Arr      := GFRE_DB.StringArray2UInt32Array(field_val);
          fdbft_Int64       : Update_Object.Field(field_name).AsInt64Arr       := GFRE_DB.StringArray2Int64Array(field_val);
          fdbft_UInt64      : Update_Object.Field(field_name).AsUInt64Arr      := GFRE_DB.StringArray2UInt64Array(field_val);
          fdbft_Real32      : Update_Object.Field(field_name).AsReal32Arr      := GFRE_DB.StringArray2Real32Array(field_val);
          fdbft_Real64      : Update_Object.Field(field_name).AsReal64Arr      := GFRE_DB.StringArray2Real64Array(field_val);
          fdbft_Currency    : Update_Object.Field(field_name).AsCurrencyArr    := GFRE_DB.StringArray2CurrArray(field_val);
          fdbft_String      : Update_Object.Field(field_name).AsStringArr      := field_val;
          fdbft_Boolean     : Update_Object.Field(field_name).AsBooleanArr     := GFRE_DB.StringArray2BoolArray(field_val);
          fdbft_DateTimeUTC : Update_Object.Field(field_name).AsDateTimeUTCArr := GFRE_DB.StringArray2Int64Array(field_val);   //
          fdbft_ObjLink     : begin
                                try
                                  Update_Object.Field(field_name).AsObjectLinkArray := GFRE_DB.StringArray2GuidArray(field_val);
                                except on e:exception do begin
                                  raise EFRE_DB_Exception.Create(edb_INTERNAL,'error updating field [%s], but the fielddef [%s] from the scheme object [%s] : [%s] value [%s]',[field.FieldName,CFRE_DB_FIELDTYPE[scheme_field_type],DefinedSchemeName,e.message,GFRE_DB.StringArray2String(field_val)]);
                                end;end;
                              end;
          fdbft_Stream      : begin
                                Update_Object.Field(field_name).AsStream := field.AsStream;
                                Field.Clear(true);
                              end;
          fdbft_Object      : begin
                               if raw_field_type=fdbft_String then begin
                                 _ObjectUpdate;
                               end else
                               if (raw_field_type=fdbft_Object) and (not All_Update_Object_Fields_Empty(Raw_Object.Field(field_name).AsObject)) then begin
                                 _ObjectUpdate;
                               end else begin
                                 ; // Skip Field
                               end;
                             end
          else raise Exception.Create('not all fieldtypes defined !');
        end;
      end;
    end else begin
      if (lowercase(field_name)<>'uid') and (lowercase(field_name)<>'domainid') then begin
        raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'updates of unspecified fields is not allowed, field [%s] is not in scheme hierarchy',[field.FieldName]);
      end;
    end;
  end;

begin
  if Update_Object.SchemeClass <> DefinedSchemeName then raise EFRE_DB_Exception.Create(edb_ILLEGALCONVERSION,'the update object must be of the same schemeclass as the schemeobject itself');
  Raw_Object.ForAll(@SetUpdateObjectFields);
end;

procedure TFRE_DB_SchemeObject.SetObjectFieldsWithSchemeI(const Raw_Object: IFRE_DB_OBject; const Update_Object: IFRE_DB_Object; const new_object: boolean; const DBConnection: IFRE_DB_CONNECTION; const schemeType: TFRE_DB_String);
begin
  SetObjectFieldsWithScheme(Raw_Object.Implementor as TFRE_DB_Object,Update_Object.Implementor as TFRE_DB_Object,new_object,DBConnection.Implementor as TFRE_DB_CONNECTION,schemeType);
end;

function TFRE_DB_SchemeObject.ReplaceInputGroup(const id: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;
var searchid : TFRE_DB_NameType;

  procedure SearchIdx(var ig: TFRE_DB_InputGroupSchemeDefinition ; const idx:NativeInt ; var halt_flag:boolean);
  begin
    if ig.groupid = searchid then
      begin
        halt_flag := true;
        ig.Free;
        FInputGroups.ClearIndex(idx);
      end;
  end;

begin
  searchid := uppercase(id);
  if not FInputGroups.ForAllBreak(@SearchIdx) then
    raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'inputgroup ['+id+'] not found');
  result := AddInputGroup(id);
end;

function TFRE_DB_SchemeObject.AddInputGroup(const id: TFRE_DB_String): TFRE_DB_InputGroupSchemeDefinition;
var spare: NativeInt;
begin
 Result := TFRE_DB_InputGroupSchemeDefinition.Create(id,self);
 if FInputGroups.Add(Result)=-1 then
   begin
     result.free;
     raise EFRE_DB_Exception.Create(edb_EXISTS,'inputgroup '+id+' already exists');
   end;
end;

function TFRE_DB_SchemeObject.AddInputGroupI(const id: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;
begin
  result := AddInputGroup(id);
end;

function TFRE_DB_SchemeObject.GetInputGroup(const id: TFRE_DB_String): TFRE_DB_InputGroupSchemeDefinition;
var searchid : TFRE_DB_NameType;

  procedure SearchIdx(var ig: TFRE_DB_InputGroupSchemeDefinition ; const idx:NativeInt ; var halt_flag:boolean);
  begin
    if ig.groupid = searchid then
      begin
        halt_flag := true;
        result := ig;
      end;
  end;

begin
  searchid := uppercase(id);
  if not FInputGroups.ForAllBreak(@SearchIdx) then
    begin
      if assigned(FParentScheme) then
        begin
          result := FParentScheme.GetInputGroup(id);
          exit;
        end;
      raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'Getinputgroup inputgroup ['+id+'] not found');
    end;
end;

function TFRE_DB_SchemeObject.GetInputGroupI(const name: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;
begin
  result := GetInputGroup(name);
end;


function TFRE_DB_SchemeObject.ValidateObject(const dbo: TFRE_DB_Object; const raise_errors: boolean): boolean;   //and fix object (case)
var failure:boolean;

  function CheckField(const field_to_check:TFRE_DB_FIELD):boolean;
  var field_to_check_fieldName : TFRE_DB_String;
      field_definition         : TFRE_DB_FieldSchemeDefinition;
      calc_field               : TFRE_DB_FIELD;
  begin
    result:=false;
    field_to_check_fieldName := field_to_check.FieldName;
    if (field_to_check_fieldName='UID') or
       (field_to_check_fieldName='DOMAINID') then
         exit;
    try
      _FieldAccessCheck(field_to_check_fieldName); // strict check
    except on e:exception do begin
      if raise_errors then begin
        raise e;
      end else begin
        failure:=true;
        result:=true;
        exit;
      end;
    end;end;
    if not GetSchemeField(field_to_check_fieldName,field_definition) then
      exit(false); // continue
    if not field_definition.ValidateField(field_to_check,raise_errors) then begin
      failure:=true;
      result:=true; //break;
    end;
  end;

  procedure CheckSchemeField(var field_def : TFRE_DB_FieldSchemeDefinition ; const idx : NativeInt ; var halt : boolean);
  var exists    : boolean;
      field_val : TFRE_DB_String;
  begin
    result:=false;
    exists := dbo.FieldExists(field_def.FieldName);
    if (not exists) and field_def.required then begin
      if raise_errors then raise EFRE_DB_Exception.Create(edb_ERROR,'the required field[%s], does not exist in object while validating against scheme[%s]',[field_def.FieldName,DefinedSchemeName]);
      failure:=true;
      halt := true;
      exit; //break;
    end;
    if exists and field_def.required and (field_def.FieldType=fdbft_String) then begin
      field_val := dbo.Field(field_def.FieldName).AsString;
      if field_val='' then begin
        if raise_errors then raise EFRE_DB_Exception.Create(edb_ERROR,'the required field[%s] is a TFRE_DB_String, but is empty while validating against scheme[%s]',[field_def.FieldName,DefinedSchemeName]);
        failure:=true;
        halt := true;
        exit;
      end;
    end;
    if field_def.FieldType<>fdbft_Object then begin
    end
  end;

begin
  failure:=false;
  if dbo.SchemeClass<>DefinedSchemeName then raise EFRE_DB_Exception.Create(edb_ERROR,'when validating an object [%s] against a schemeclass [%s], it must be of that same schemeclass',[dbo.SchemeClass,DefinedSchemeName]);
  dbo.ForAllBrk(@CheckField); // check if object satisfied by scheme
  FFieldDefs.ForAllBreak(@CheckSchemeField);
  result := not failure;
end;

function TFRE_DB_SchemeObject.ValidateObjectI(const dbo: IFRE_DB_Object; const raise_errors: boolean): boolean;
begin
  result := ValidateObject(dbo.Implementor as TFRE_DB_Object,raise_errors);
end;

function SchemeFieldDefnull (const fd : PFRE_DB_FieldSchemeDefinition):boolean;
begin
  result := not assigned(fd^);
end;

function CompareSchemefieldDef   (const fd1,fd2 : PFRE_DB_FieldSchemeDefinition):boolean;
begin
  result := fd1^.FFieldName = fd2^.FFieldName;
end;


function TFRE_DB_SchemeObject.CalcFieldExists(const name: TFRE_DB_NameType; var calculated_field_type: TFRE_DB_FIELDTYPE; var calcmethod: IFRE_DB_CalcMethod): boolean;
var lFieldSchemeDefinition : TFRE_DB_FieldSchemeDefinition;
begin
  result                := GetSchemeField(name,lFieldSchemeDefinition);
  calculated_field_type := fdbft_NotFound;
  calcmethod            := nil;
  if result then begin
    result := lFieldSchemeDefinition.IsACalcField;
    if result then
      begin
        calcmethod            := lFieldSchemeDefinition.FCalcMethod;
        calculated_field_type := lFieldSchemeDefinition.FFieldType;
      end;
  end;
end;

procedure TFRE_DB_SchemeObject.ForAllCalculatedFields(const iter: TFRE_DB_FieldIterator ; const obj: TFRE_DB_Object);

  procedure CalcIterator(const fielddef  : TFRE_DB_FieldSchemeDefinition);
  begin
    if fielddef.IsACalcField then
      iter(obj._Field(fielddef.FieldName));
  end;

begin
  ForAllFieldSchemeDefinitions(@CalcIterator);
end;

procedure TFRE_DB_SchemeObject.ForAllCalculatedFieldsBrk(const iter: TFRE_DB_FieldIteratorBrk; const obj: TFRE_DB_Object);

 procedure CalcIterator(var fielddef : TFRE_DB_FieldSchemeDefinition ; const idx:NativeInt ; var halt_flag:boolean);
 begin
   if fielddef.IsACalcField then
     halt_flag := iter(obj._field(fielddef.FieldName));
 end;

begin
  FFieldDefs.ForAllBreak(@CalcIterator);
end;


function TFRE_DB_SchemeObject.InvokeMethod_UID_Session(const instance: TFRE_DB_GUIDArray; const class_name,meth_name: TFRE_DB_String; var in_params: TFRE_DB_Object; const connection: TFRE_DB_CONNECTION; const session: TFRE_DB_UserSession): IFRE_DB_Object;
var i         : Integer;
    child_dbo : TFRE_DB_Object;
    obj       : TFRE_DB_Object;
    iobj      : IFRE_DB_Object;
    idc       : IFRE_DB_DERIVED_COLLECTION;
    found_o_cl: TFRE_DB_String;
    scheme    : IFRE_DB_SCHEMEOBJECT;
    obcl      : TFRE_DB_OBJECTCLASS;
    obexcl    : TFRE_DB_OBJECTCLASSEX;
    fake_in_p : TFRE_DB_Object;
    finob     : TFRE_DB_Object;
    finalizeob : boolean;

    app_if     : IFRE_DB_APPLICATION;


   function _getObj(const field: TFRE_DB_FIELD):Boolean;
   begin
     if field.FieldType=fdbft_Object then begin
       if FREDB_Guids_Same(field.AsObject.UID,instance[i]) then begin
         child_dbo:=field.AsObject;
         Result:=true;
       end;
     end;
     Result:=false;
   end;

begin
  result     := nil;
  app_if     := nil;
  finalizeob := false;
  if Length(instance)>0 then begin
    if assigned(session) and (instance[0]=cFRE_DB_LOGIN_APP_UID) then begin
      obj := (cFRE_DB_LOGIN_APP as TFRE_DB_APPLICATION).Implementor as TFRE_DB_Object;
      app_if := cFRE_DB_LOGIN_APP as TFRE_DB_APPLICATION;
    end else
    if assigned(session) and session.SearchSessionAppUID(instance[0],iobj) then begin
      obj := iobj.Implementor as TFRE_DB_Object;
      app_if := iobj.Implementor_HC as IFRE_DB_APPLICATION;
    end else
    if assigned(session) and session.SearchSessionDCUID(instance[0],idc) then begin
      obj := idc.Implementor as TFRE_DB_DERIVED_COLLECTION;
    end else begin
      CheckDbResult(connection.Fetch(instance[0],obj),Format('Try to invoke [%s.%s], but cant fetch the instance [%s]',[DefinedSchemeName,meth_name,FREDB_G2H(instance[0])]));
      finob      := obj;
      finalizeob := true;
    end;
    try
      for i := 1 to high(instance) do begin
        child_dbo:=nil;
        obj.ForAllFieldsBreak(@_getObj);
        if Assigned(child_dbo) then begin
          obj:=child_dbo;
        end else begin
          raise EFRE_DB_Exception.Create(edb_ERROR,'INSTANCE METHOD FAILED [%s.%s][%s] - CHILD OBJ NOT FOUND UID=[%s]!',[DefinedSchemeName,meth_name,GFRE_DB.GuidArray2SString(instance),FREDB_G2H(instance[i])]);
        end;
      end;
      scheme := obj.GetScheme;
      if assigned(scheme) then begin
        found_o_cl := scheme.DefinedSchemeName;
      end else begin
        found_o_cl := obj.ClassName;
        if assigned(obj.Mediator) then begin
          found_o_cl := obj.Mediator.ClassName;
        end;
      end;
      if uppercase(found_o_cl) <> uppercase(Class_Name) then begin
        raise EFRE_DB_Exception.Create(edb_ERROR,'INVOKE FOR [%s.%s] FAILED : CLASS MISMATCH OBJ=[%s / %s]',[class_name,meth_name,found_o_cl,obj.UID_String]);
      end;
      if assigned(in_params) then begin
        in_params.SetReference(session);
        try
          result := obj.Invoke(meth_name,in_params,session,app_if,connection); // NEVER EVER FREE SOMETHING IN THE IN PARAMS
        finally
          in_params.Finalize;
          in_params := nil;
        end;
      end else begin
        try
          fake_in_p := GFRE_DB.NewObject;
          fake_in_p.SetReference(session);
          result := obj.Invoke(meth_name,fake_in_p,session,app_if,connection);
        finally
          fake_in_p.Finalize;
        end;
      end;
    finally
      if finalizeob then
        begin
          finob.Finalize;
        end;
    end;
  end else begin
    obcl := GFRE_DB.GetObjectClass(class_name);
    if assigned(obcl) then begin
      in_params.SetReference(session.GetDBConnection.Implementor);
      try
        result := obcl.Invoke_DBIMC_Method(meth_name,in_params,session,app_if,connection);
      finally
        in_params.Finalize;
        in_params:=nil;
      end;
    end else begin
       obexcl := GFRE_DB.GetObjectClassEx(class_name);
       if Assigned(obexcl) then begin
         in_params.SetReference(session.GetDBConnection.Implementor);
         try
           result := obexcl.Invoke_DBIMC_Method(meth_name,in_params,session,app_if,connection);
         finally
           in_params.Finalize;
           in_params:=nil;
         end;
       end;
    end;
  end;
end;

function TFRE_DB_SchemeObject.InvokeMethod_UIDI(const obj_uid: TFRE_DB_GUID; const obj_methodname: TFRE_DB_String; var input: IFRE_DB_Object; const connection: IFRE_DB_CONNECTION): IFRE_DB_Object;
var ga  : TFRE_DB_GUIDArray;
    inp : TFRE_DB_Object;
begin
  setlength(ga,1);
  ga[0] := obj_uid;
  inp := input.Implementor as TFRE_DB_Object;
  result := InvokeMethod_UID_Session(ga,DefinedSchemeName,obj_methodname,inp,Connection.Implementor_HC as TFRE_DB_CONNECTION,nil);
  if not assigned(inp) then
    input := nil;
end;

function TFRE_DB_SchemeObject.ConstructNewInstance(const fail_on_no_cc: boolean): TFRE_DB_Object;
var ex_class : TFRE_DB_ObjectEx;
    dbo      : TFRE_DB_Object;
    name     : TFRE_DB_String;
begin
 if FHasHardcodeClass then begin
   assert(assigned(FHardCodeClassTyp));
   name := FHardCodeClassTyp.ClassName;
   if FHardCodeClassTyp.InheritsFrom(TFRE_DB_OBJECTEX) then begin
     if IsA('TFRE_DB_NAMED_OBJECT') then begin
       dbo                    := GFRE_DB.NewNamedObject;
     end else begin
       dbo                    := GFRE_DB.NewObject;
     end;
     ex_class               := TFRE_DB_OBJECTCLASSEX(FHardCodeClassTyp).CreateBound(dbo,true);
     dbo.FMediatorExtention := ex_class;
     result                 := dbo;
   end else begin
     if FHardCodeClassTyp.InheritsFrom(TFRE_DB_Object) then begin
       result := TFRE_DB_OBJECTCLASS(FHardCodeClassTyp).Create as TFRE_DB_Object;
     end else abort; // Check this out
   end;
 end else begin
   result := GFRE_DB.NewObject;
 end;
 result.SetScheme(self);
end;

procedure TFRE_DB_SchemeObject.SetupMediator(const dbo: TFRE_DB_Object);
var ex_class : TFRE_DB_ObjectEx;
    name     : TFRE_DB_String;
begin
   assert(assigned(FHardCodeClassTyp));
   name := FHardCodeClassTyp.ClassName;
   if FHardCodeClassTyp.InheritsFrom(TFRE_DB_OBJECTEX) then begin
     ex_class               := TFRE_DB_OBJECTCLASSEX(FHardCodeClassTyp).CreateBound(dbo,false);
     dbo.FMediatorExtention := ex_class;
   end else begin
     abort;
   end;
end;

function TFRE_DB_SchemeObject.HasHardCodeClass: Boolean;
begin
  result := FHasHardcodeClass;
end;

function TFRE_DB_SchemeObject.GetFieldDef(const UPPER_fieldname: TFRE_DB_NameType; var fd: TFRE_DB_FieldSchemeDefinition): boolean;

  procedure FieldDefExists(var ifd : TFRE_DB_FieldSchemeDefinition ; const idx:NativeInt ; var halt_flag:boolean);
  begin
  if ifd.FFieldName = UPPER_fieldname then
    begin
      halt_flag := true;
      fd        := ifd;
    end;
  end;

begin
  fd := nil;
  result := FFieldDefs.ForAllBreak(@FieldDefExists);
end;

constructor TFRE_DB_SchemeObject.create;
begin
  FInputGroups.InitSparseList(nil,@InputGroupNull,@CompareInputGroups,10);
  FFieldDefs.InitSparseList(nil,@SchemeFieldDefnull,@CompareSchemefieldDef);
end;

destructor TFRE_DB_SchemeObject.Destroy;

  procedure CleanupIGs(var ig : TFRE_DB_InputGroupSchemeDefinition ; const idx : NativeInt ; var halt : boolean);
  begin
    ig.Free;
    ig := nil;
  end;

  procedure CleanupFDs(var fd : TFRE_DB_FieldSchemeDefinition ; const idx : NativeInt ; var halt : boolean);
  begin
    fd.Free;
    fd := nil;
  end;

begin
  FInputGroups.ForAllBreak(@CleanupIGs);
  FFieldDefs.ForAllBreak(@CleanupFDs);
  inherited Destroy;
end;

procedure TFRE_DB_SchemeObject.ForAllFieldSchemeDefinitionsI(const iterator: IFRE_DB_SchemeFieldDef_Iterator);

  procedure iter(const obj:TFRE_DB_FieldSchemeDefinition);
  begin
    iterator(obj);
  end;

begin
  ForAllFieldSchemeDefinitions(@iter);
end;

procedure TFRE_DB_SchemeObject.ForAllFieldSchemeDefinitions(const iterator: TFRE_DB_SchemeFieldDef_Iterator);

  procedure iterate(var ifd : TFRE_DB_FieldSchemeDefinition ; const idx:NativeInt ; var halt_flag:boolean);
  begin
    iterator(ifd);
  end;

begin
  FFieldDefs.ForAllBreak(@Iterate);
end;




function TFRE_DB_SchemeObject.GetAll_IMI_Methods: TFRE_DB_StringArray;
begin
  if not FHC_MethodsBuild then begin
    _BuildHardcodeMethods;
  end;
  result := FIMI_Methods;
end;

function TFRE_DB_SchemeObject.MethodExists(const name: TFRE_DB_String): boolean;
begin
  result:=FREDB_StringInArray(uppercase(name),GetAll_IMI_Methods);
end;


{ TFRE_DB_COLLECTION }

function TFRE_DB_COLLECTION.URT_UserUid: TFRE_DB_GUID;
begin
  if assigned(FCollConnection) then
    result := FCollConnection.GetUserUID
  else
    result := CFRE_DB_NullGUID;
end;


constructor TFRE_DB_COLLECTION.Create(const connection: TFRE_DB_BASE_CONNECTION; const name: TFRE_DB_NameType; const in_memory_only: boolean);
begin
  inherited Create;
  FName             := name;
  FUniqueName       := uppercase(name);
  FCollConnection   := connection;
  FIsVolatile       := in_memory_only;
end;


procedure TFRE_DB_COLLECTION.ForAllI(const func: IFRE_DB_Obj_Iterator);
  procedure Iter(const obj:TFRE_DB_Object);
  begin
    func(obj);
  end;
begin //nl
  ForAll(@Iter);
end;

procedure TFRE_DB_COLLECTION.ForAllBreakI(const func: IFRE_DB_ObjectIteratorBrk; var halt: boolean);

  procedure _ForAllBreak(const obj:TFRE_DB_Object ; var halt :boolean);
  begin
    func(obj,halt);
  end;

begin //nl
  ForAllBreak(@_ForAllBreak,halt);
end;

function TFRE_DB_COLLECTION.StoreI(const new_obj: IFRE_DB_Object): TFRE_DB_Errortype;
var lObject : TFRE_DB_Object;
    s : string;
begin //nl
   lObject := new_obj.Implementor as TFRE_DB_Object;
   result  := Store(lObject);
   {$IFDEF DEBUG_INVALID_USAGE}    {const / var clean debug hack }
     PNativeUint(@new_obj)^:=0;
   {$ENDIF}
end;


function TFRE_DB_COLLECTION.UpdateI(const dbo: IFRE_DB_Object): TFRE_DB_Errortype;
var ldbo : TFRE_DB_Object;
begin //nl
  ldbo := dbo.Implementor as TFRE_DB_Object;
  result := Update(ldbo);
  {$IFDEF DEBUG_INVALID_USAGE}    {const / var clean debug hack }
    PNativeUint(@dbo)^:=0;
  {$ENDIF}
end;

function TFRE_DB_COLLECTION.FetchInCollectionI(const ouid: TFRE_DB_GUID; out dbo: IFRE_DB_Object): boolean;
var ldbo : TFRE_DB_Object;
begin //nl
  result := FetchInCollection(ouid,ldbo);
  dbo := ldbo;
end;


function TFRE_DB_COLLECTION.FirstI: IFRE_DB_Object;
begin //nl
 result := First;
end;

function TFRE_DB_COLLECTION.LastI: IFRE_DB_Object;
begin //nl
 result := Last;
end;

function TFRE_DB_COLLECTION._InternalStore(var new_obj: TFRE_DB_Object): TFRE_DB_Errortype; //No DomainID check
var suid   : TFRE_DB_GUID;
begin //nl
  suid   := new_obj.UID;
  result := edb_OK;
  try
    FCollConnection.FPersistance_Layer.StoreOrUpdateObject(new_obj,FName,true);
  except
    on E:EFRE_DB_Exception do
      begin
        result := e.ErrorType; //FCollConnection.FPersistance_Layer.GetLastErrorCode;
      end;
    on e:Exception do
      begin
        result.code := edb_INTERNAL;
        result.msg  := e.Message;
      end;
  end;
  new_obj:=nil;
end;

procedure TFRE_DB_COLLECTION._IterateOverObjectsBrk(const objs: IFRE_DB_ObjectArray; const func: IFRE_DB_ObjectIteratorBrk; var halt: boolean);
var i      : NativeInt;
    myhalt : boolean;
begin
  myhalt := false;
  for i:=0 to high(objs) do
    begin
      if myhalt=false then
        begin
          func(objs[i],halt);
          myhalt := halt;
        end
      else
        objs[i].Finalize;
    end;
end;

procedure TFRE_DB_COLLECTION._IterateOverObjects(const objs: IFRE_DB_ObjectArray; const func: TFRE_DB_Obj_Iterator);
var i      : NativeInt;
begin
 for i:=0 to high(objs) do
   func(objs[i].Implementor as TFRE_DB_Object);
end;

destructor TFRE_DB_COLLECTION.Destroy;
begin
  FDBO_State := fdbos_Destroying;
  inherited Destroy;
end;

function TFRE_DB_COLLECTION.ExistsInCollection(const ouid: TFRE_DB_GUID; const has_fetch_rights: boolean): boolean;
begin //nl
  result := FCollConnection.FPersistance_Layer.CollectionExistsInCollection(FName,ouid,has_fetch_rights,FCollConnection.GetUserUIDP);
end;


procedure TFRE_DB_COLLECTION.ForAll(const func: TFRE_DB_Obj_Iterator);
var  objs : IFRE_DB_ObjectArray;
begin
  objs := FCollConnection.FPersistance_Layer.CollectionBulkFetch(FName,FCollConnection.GetUserUIDP);
  _IterateOverObjects(objs,func);
end;

procedure TFRE_DB_COLLECTION.ForAllNoRightChk(const func: TFRE_DB_Obj_Iterator);
var  objs : IFRE_DB_ObjectArray;
begin
  objs := FCollConnection.FPersistance_Layer.CollectionBulkFetch(FName);
  _IterateOverObjects(objs,func);
end;

procedure TFRE_DB_COLLECTION.ForAllBreak(const func: TFRE_DB_ObjectIteratorBrk; var halt: boolean);
var  objs     : IFRE_DB_ObjectArray;
     ishalted : boolean;

    procedure Local(const obj : TFRE_DB_Object);
    begin
      if not ishalted then
        func(obj,halt)  { give chance to iterate the first object }
      else
        obj.Finalize;   { finalize all objects that are not iterated }
      ishalted := halt;
    end;

begin
  ishalted := false;
  objs     := FCollConnection.FPersistance_Layer.CollectionBulkFetch(FName);
  _IterateOverObjects(objs,@Local);
end;



function TFRE_DB_COLLECTION.Remove(const ouid: TFRE_DB_GUID): TFRE_DB_Errortype;
begin //nl
  try
    FCollConnection.FPersistance_Layer.DeleteObject(ouid,CollectionName(true));
    exit(edb_OK);
  except
    on E:EFRE_DB_Exception do
      begin
        result.Code := E.ErrorType;
        result.Msg  := E.Message;
      end;
    on E:Exception do
      begin
        result.Code := edb_ERROR;
        result.Msg  := e.Message;
      end;
  end;
end;

function TFRE_DB_COLLECTION.Store(var new_obj: TFRE_DB_Object):TFRE_DB_Errortype;
begin //nl
  try
     if new_obj.DomainID=CFRE_DB_NullGUID then
       new_obj.SetDomainID(FCollConnection.GetMyDomainID);
     Result := FCollConnection.CheckAccessRightAndCondFinalize(new_obj,sr_STORE,false,false);
     if Result<>edb_OK then
       exit;
     result := _InternalStore(new_obj);
  finally
    try
      if assigned(new_obj) then
        new_obj.Finalize;
    except
      on e: exception do
        GFRE_DBI.LogError(dblc_DB,'error on finalizing new_dbo in store, instance invalid ? [%s]',[e.Message]);
    end;
    new_obj:=nil;
  end;
end;


function TFRE_DB_COLLECTION.Update(const dbo: TFRE_DB_Object): TFRE_DB_Errortype;
begin //nl
   if dbo.DomainID=CFRE_DB_NullGUID then
     dbo.SetDomainID(FCollConnection.GetMyDomainID);
   result := FCollConnection.CheckAccessRightAndCondFinalize(dbo,sr_UPDATE);
   if result <>edb_OK then
     exit;
    result := FCollConnection.Update(dbo,CollectionName(true));
end;

function TFRE_DB_COLLECTION.FetchInCollection(const ouid: TFRE_DB_GUID; out dbo: TFRE_DB_Object): boolean;
var dbi : IFRE_DB_Object;
begin
  if FCollConnection.FPersistance_Layer.CollectionFetchInCollection(FName,ouid,dbi,FCollConnection.GetUserUIDP)=edb_OK then
    begin
      dbo    := dbi.Implementor as TFRE_DB_Object;
      result := true;
    end
end;

procedure TFRE_DB_COLLECTION.ClearCollection;
begin
  FCollConnection.FPersistance_Layer.CollectionClearCollection(FName,FCollConnection.GetUserUIDP);
end;

function TFRE_DB_COLLECTION.CollectionName(const unique: boolean): TFRE_DB_NameType;
begin
  if unique then
    result := FUniqueName
  else
    result := FName;
end;

procedure TFRE_DB_COLLECTION.GetAllUids(var uids: TFRE_DB_GUIDArray);
begin
  uids := FCollConnection.FPersistance_Layer.CollectionBulkFetchUIDS(FName,FCollConnection.GetUserUIDP);
end;

procedure TFRE_DB_COLLECTION.GetAllUidsNoRC(var uids: TFRE_DB_GUIDArray);
begin
  uids := FCollConnection.FPersistance_Layer.CollectionBulkFetchUIDS(FName);
end;

procedure TFRE_DB_COLLECTION.GetAllObjs(out objs: IFRE_DB_ObjectArray);
begin
  objs := FCollConnection.FPersistance_Layer.CollectionBulkFetch(FName,FCollConnection.GetUserUIDP);
end;

procedure TFRE_DB_COLLECTION.GetAllObjsNoRC(out objs: IFRE_DB_ObjectArray);
begin
  objs := FCollConnection.FPersistance_Layer.CollectionBulkFetch(FName);
end;

function TFRE_DB_COLLECTION.DefineIndexOnField(const FieldName: TFRE_DB_NameType; const FieldType: TFRE_DB_FIELDTYPE; const unique: boolean; const ignore_content_case: boolean; const index_name: TFRE_DB_NameType; const allow_null_value: boolean; const unique_null_values: boolean; const is_a_domain_index: boolean): TFRE_DB_Errortype;
begin //nl
  FCollConnection.FPersistance_Layer.CollectionDefineIndexOnField(Fname,FieldName,FieldType,unique,ignore_content_case,index_name,allow_null_value,unique_null_values,is_a_domain_index,FCollConnection.GetUserUIDP);
  result := edb_OK;
end;

function TFRE_DB_COLLECTION.DefineIndexOnField(const IndexDef: TFRE_DB_INDEX_DEF): TFRE_DB_Errortype;
begin
  with IndexDef do
    result := DefineIndexOnField(FieldName,FieldType,Unique,IgnoreCase,IndexName,AllowNulls,UniqueNull,DomainIndex);
end;

function TFRE_DB_COLLECTION.GetIndexDefinition(const index_name: TFRE_DB_NameType): TFRE_DB_INDEX_DEF;
begin
  result := FCollConnection.FPersistance_Layer.CollectionGetIndexDefinition(Fname,index_name,FCollConnection.GetUserUIDP);
end;

function TFRE_DB_COLLECTION.DropIndex(const index_name: TFRE_DB_NameType): TFRE_DB_Errortype;
begin
  FCollConnection.FPersistance_Layer.CollectionDropIndex(Fname,index_name,FCollConnection.GetUserUIDP);
  result := edb_OK;
end;

function TFRE_DB_COLLECTION.GetAllIndexNames: TFRE_DB_NameTypeArray;
begin
  result := FCollConnection.FPersistance_Layer.CollectionGetAllIndexNames(Fname,FCollConnection.GetUserUIDP);
end;

function TFRE_DB_COLLECTION.GetAllIndexDefinitions: TFRE_DB_INDEX_DEF_ARRAY;
var nta : TFRE_DB_NameTypeArray;
    i   : NativeInt;
begin
  nta := GetAllIndexNames;
  SetLength(result,Length(nta));
  for i := 0 to high(nta) do
    result[i]:=GetIndexDefinition(nta[i]);
end;

function TFRE_DB_COLLECTION.IndexExists(const index_name: TFRE_DB_NameType): boolean;
begin
  result := FCollConnection.FPersistance_Layer.CollectionIndexExists(FName,index_name,FCollConnection.GetUserUIDP);
end;

function TFRE_DB_COLLECTION.ExistsIndexed(const query_value: TFRE_DB_String; const val_is_null: boolean; const index_name: TFRE_DB_NameType): Boolean;
begin
  result := ExistsIndexedText(query_value,val_is_null,index_name)<>0;
end;

function TFRE_DB_COLLECTION.ExistsIndexedFieldval(const fld: IFRE_DB_Field; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var qry_val : IFRE_DB_Object;
begin
  qry_val := FREDB_NewIndexFldValForObjectEncoding(fld,domain_uid_string);
  result := FCollConnection.FPersistance_Layer.CollectionGetIndexedValueCount(FName,qry_val,index_name,FCollConnection.GetUserUIDP);
end;

function TFRE_DB_COLLECTION.ExistsIndexedText(const query_value: TFRE_DB_String; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(ExistsIndexedFieldval(nil,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsString := query_value;
    result := ExistsIndexedFieldval(dummy.Field('f'),index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.ExistsIndexedSigned(const query_value: int64; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
 if val_is_null then
   exit(ExistsIndexedFieldval(nil,index_name,domain_uid_string));
 dummy := GFRE_DBI.NewObject;
 try
   dummy.Field('f').asInt64 := query_value;
   result := ExistsIndexedFieldval(dummy.Field('f'),index_name,domain_uid_string);
 finally
   dummy.Finalize;
 end;
end;

function TFRE_DB_COLLECTION.ExistsIndexedUnsigned(const query_value: UInt64; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
 if val_is_null then
   exit(ExistsIndexedFieldval(nil,index_name,domain_uid_string));
 dummy := GFRE_DBI.NewObject;
 try
   dummy.Field('f').AsUInt64 := query_value;
   result := ExistsIndexedFieldval(dummy.Field('f'),index_name,domain_uid_string);
 finally
   dummy.Finalize;
 end;
end;

function TFRE_DB_COLLECTION.ExistsIndexedReal(const query_value: Double; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
 if val_is_null then
   exit(ExistsIndexedFieldval(nil,index_name,domain_uid_string));
 dummy := GFRE_DBI.NewObject;
 try
   dummy.Field('f').AsReal64 := query_value;
   result := ExistsIndexedFieldval(dummy.Field('f'),index_name,domain_uid_string);
 finally
   dummy.Finalize;
 end;
end;


function TFRE_DB_COLLECTION.GetIndexedObjI(const query_value: TFRE_DB_String; out obj: IFRE_DB_Object; const index_name: TFRE_DB_NameType): boolean;
begin
  result := GetIndexedObjText(query_value,obj,false,index_name)>0;
end;

function TFRE_DB_COLLECTION.GetIndexedObjTextCoreI(const query_value: TFRE_DB_String; out obj: IFRE_DB_Object; const no_rc: boolean; const val_is_null: boolean; const index_name: TFRE_DB_NameType): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    begin
      if no_rc then
        exit(GetIndexedObjFieldvalNoRc(nil,obj,index_name))
      else
        exit(GetIndexedObjFieldval(nil,obj,index_name))
    end;
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsString := query_value;
    if no_rc then
      result := GetIndexedObjFieldvalNoRc(dummy.Field('f'),obj,index_name)
    else
      result := GetIndexedObjFieldval(dummy.Field('f'),obj,index_name)
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.GetIndexedObjTextCore(const query_value: TFRE_DB_String; out obj: TFRE_DB_Object; const no_rc: boolean; const val_is_null: boolean; const index_name: TFRE_DB_NameType): NativeInt;
var iob : IFRE_DB_Object;
begin
  result := GetIndexedObjTextCoreI(query_value,iob,no_rc,val_is_null,index_name);
  if result<>0 then
    obj := iob.Implementor as TFRE_DB_Object
  else
    obj := nil;
end;

function TFRE_DB_COLLECTION.GetIndexedObjFieldval(const fld: IFRE_DB_Field; out obj: IFRE_DB_Object; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var qry_val : IFRE_DB_Object;
     objs   : IFRE_DB_ObjectArray;
begin
  qry_val := FREDB_NewIndexFldValForObjectEncoding(fld,domain_uid_string);
  result  := FCollConnection.FPersistance_Layer.CollectionGetIndexedObjsFieldval(FName,qry_val,objs,true,index_name,FCollConnection.GetUserUIDP);
  if result>0 then
    begin
      if result>1 then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'this point query should not return more than one object [%d]',[result]);
        obj := objs[0];
    end
  else
    obj := nil;
end;

function TFRE_DB_COLLECTION.GetIndexedObjFieldvalNoRc(const fld: IFRE_DB_Field; out obj: IFRE_DB_Object; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var qry_val : IFRE_DB_Object;
     objs   : IFRE_DB_ObjectArray;
begin
  qry_val := FREDB_NewIndexFldValForObjectEncoding(fld,domain_uid_string);
  result  := FCollConnection.FPersistance_Layer.CollectionGetIndexedObjsFieldval(FName,qry_val,objs,true,index_name,nil);
  if result>0 then
    begin
      if result>1 then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'this point query should not return more than one object [%d]',[result]);
        obj := objs[0];
    end
  else
    obj := nil;
end;

function TFRE_DB_COLLECTION.GetIndexedObjText(const query_value: TFRE_DB_String; out obj: IFRE_DB_Object; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(GetIndexedObjFieldval(nil,obj,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsString := query_value;
    result := GetIndexedObjFieldval(dummy.Field('f'),obj,index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.GetIndexedObjSigned(const query_value: int64; out obj: IFRE_DB_Object; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(GetIndexedObjFieldval(nil,obj,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsInt64 := query_value;
    result := GetIndexedObjFieldval(dummy.Field('f'),obj,index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.GetIndexedObjUnsigned(const query_value: Uint64; out obj: IFRE_DB_Object; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(GetIndexedObjFieldval(nil,obj,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsUInt64 := query_value;
    result := GetIndexedObjFieldval(dummy.Field('f'),obj,index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.GetIndexedObjReal(const query_value: Double; out obj: IFRE_DB_Object; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(GetIndexedObjFieldval(nil,obj,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsReal64 := query_value;
    result := GetIndexedObjFieldval(dummy.Field('f'),obj,index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.GetIndexedObjsFieldval(const fld: IFRE_DB_Field; out obj: IFRE_DB_ObjectArray; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var qry_val : IFRE_DB_Object;
begin
  qry_val := FREDB_NewIndexFldValForObjectEncoding(fld,domain_uid_string);
  result  := FCollConnection.FPersistance_Layer.CollectionGetIndexedObjsFieldval(FName,qry_val,obj,false,index_name,FCollConnection.GetUserUIDP);
end;

function TFRE_DB_COLLECTION.GetIndexedObjsText(const query_value: TFRE_DB_String; out obj: IFRE_DB_ObjectArray; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(GetIndexedObjsFieldval(nil,obj,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsString := query_value;
    result := GetIndexedObjsFieldval(dummy.Field('f'),obj,index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.GetIndexedObjsSigned(const query_value: int64; out obj: IFRE_DB_ObjectArray; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(GetIndexedObjsFieldval(nil,obj,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsInt64 := query_value;
    result := GetIndexedObjsFieldval(dummy.Field('f'),obj,index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.GetIndexedObjsUnsigned(const query_value: Uint64; out obj: IFRE_DB_ObjectArray; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(GetIndexedObjsFieldval(nil,obj,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsUInt64 := query_value;
    result := GetIndexedObjsFieldval(dummy.Field('f'),obj,index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.GetIndexedObjsReal(const query_value: Double; out obj: IFRE_DB_ObjectArray; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(GetIndexedObjsFieldval(nil,obj,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsReal64 := query_value;
    result := GetIndexedObjsFieldval(dummy.Field('f'),obj,index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.GetIndexedUID(const query_value: TFRE_DB_String; out luid: TFRE_DB_GUID; const index_name: TFRE_DB_NameType): boolean;
begin
  result := GetIndexedUIDText(query_value,luid,false,index_name)>0;
end;

function TFRE_DB_COLLECTION.GetIndexedUIDFieldval(const fld: IFRE_DB_Field; out luid: TFRE_DB_GUID; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var qry_val : IFRE_DB_Object;
     uids   : TFRE_DB_GUIDArray;
begin
  qry_val := FREDB_NewIndexFldValForObjectEncoding(fld,domain_uid_string);
  result  := FCollConnection.FPersistance_Layer.CollectionGetIndexedUidsFieldval(FName,qry_val,uids,true,index_name,FCollConnection.GetUserUIDP);
  if result>0 then
    begin
      if result>1 then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'this point query should not return more than one uid [%d]',[result]);
        luid := uids[0];
    end
  else
    luid := CFRE_DB_NullGUID;
end;

function TFRE_DB_COLLECTION.GetIndexedUIDText(const query_value: TFRE_DB_String; out luid: TFRE_DB_GUID; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(GetIndexedUIDFieldval(nil,luid,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsString := query_value;
    result := GetIndexedUIDFieldval(dummy.Field('f'),luid,index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.GetIndexedUIDSigned(const query_value: int64; out luid: TFRE_DB_GUID; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(GetIndexedUIDFieldval(nil,luid,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsInt64 := query_value;
    result := GetIndexedUIDFieldval(dummy.Field('f'),luid,index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.GetIndexedUIDUnsigned(const query_value: Uint64; out luid: TFRE_DB_GUID; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(GetIndexedUIDFieldval(nil,luid,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsUInt64 := query_value;
    result := GetIndexedUIDFieldval(dummy.Field('f'),luid,index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.GetIndexedUIDReal(const query_value: Double; out luid: TFRE_DB_GUID; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(GetIndexedUIDFieldval(nil,luid,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsReal64 := query_value;
    result := GetIndexedUIDFieldval(dummy.Field('f'),luid,index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.GetIndexedUIDsFieldval(const fld: IFRE_DB_Field; out fuids: TFRE_DB_GUIDArray; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var qry_val : IFRE_DB_Object;
begin
  qry_val := FREDB_NewIndexFldValForObjectEncoding(fld,domain_uid_string);
  result  := FCollConnection.FPersistance_Layer.CollectionGetIndexedUidsFieldval(FName,qry_val,fuids,false,index_name,FCollConnection.GetUserUIDP);
end;

function TFRE_DB_COLLECTION.GetIndexedUIDsText(const query_value: TFRE_DB_String; out fuids: TFRE_DB_GUIDArray; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(GetIndexedUIDsFieldval(nil,fuids,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsString := query_value;
    result := GetIndexedUIDsFieldval(dummy.Field('f'),fuids,index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.GetIndexedUIDsSigned(const query_value: int64; out fuids: TFRE_DB_GUIDArray; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(GetIndexedUIDsFieldval(nil,fuids,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsInt64 := query_value;
    result := GetIndexedUIDsFieldval(dummy.Field('f'),fuids,index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.GetIndexedUIDsUnsigned(const query_value: Uint64; out fuids: TFRE_DB_GUIDArray; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(GetIndexedUIDsFieldval(nil,fuids,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsUInt64 := query_value;
    result := GetIndexedUIDsFieldval(dummy.Field('f'),fuids,index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.GetIndexedUIDsReal(const query_value: Double; out fuids: TFRE_DB_GUIDArray; const val_is_null: boolean; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(GetIndexedUIDsFieldval(nil,fuids,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsReal64 := query_value;
    result := GetIndexedUIDsFieldval(dummy.Field('f'),fuids,index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.RemoveIndexedFieldval(const fld: IFRE_DB_Field; const index_name: TFRE_DB_NameType; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var qry_val : IFRE_DB_Object;
begin
  qry_val := FREDB_NewIndexFldValForObjectEncoding(fld,domain_uid_string);
  result  := FCollConnection.FPersistance_Layer.CollectionRemoveIndexedUidsFieldval(FName,qry_val,index_name,FCollConnection.GetUserUIDP);
end;


procedure TFRE_DB_COLLECTION.ForAllIndexed(const func: IFRE_DB_ObjectIteratorBrk; var halt: boolean; const index_name: TFRE_DB_NameType; const ascending: boolean; const max_count: NativeInt; skipfirst: NativeInt; const domain_uid_string: TFRE_DB_GUID_String);
begin
  ForAllIndexedFieldvalRange(nil,nil,func,halt,index_name,ascending,max_count,skipfirst,domain_uid_string);
end;

procedure TFRE_DB_COLLECTION.ForAllIndexedSignedRange(const min_value, max_value: int64; const iterator: IFRE_DB_ObjectIteratorBrk; var halt: boolean; const index_name: TFRE_DB_NameType; const ascending: boolean; const min_is_null: boolean; const max_is_max: boolean; const max_count: NativeInt; skipfirst: NativeInt; const domain_uid_string: TFRE_DB_GUID_String);
var dummy  : IFRE_DB_Object;
    minfld : IFRE_DB_Field;
    maxfld : IFRE_DB_Field;
begin
  dummy := GFRE_DBI.NewObject;
  try
    if min_is_null then
      minfld := nil
    else
      begin
        minfld := dummy.Field('mi');
        minfld.AsInt64 := min_value;
      end;
    if max_is_max then
      maxfld := nil
    else
      begin
        maxfld := dummy.Field('ma');
        maxfld.AsInt64 := max_value;
      end;
    ForAllIndexedFieldvalRange(minfld,maxfld,iterator,halt,index_name,ascending,max_count,skipfirst,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

procedure TFRE_DB_COLLECTION.ForAllIndexedUnsignedRange(const min_value, max_value: QWord; const iterator: IFRE_DB_ObjectIteratorBrk; var halt: boolean; const index_name: TFRE_DB_NameType; const ascending: boolean; const min_is_null: boolean; const max_is_max: boolean; const max_count: NativeInt; skipfirst: NativeInt; const domain_uid_string: TFRE_DB_GUID_String);
var dummy  : IFRE_DB_Object;
    minfld : IFRE_DB_Field;
    maxfld : IFRE_DB_Field;
begin
  dummy := GFRE_DBI.NewObject;
  try
    if min_is_null then
      minfld := nil
    else
      begin
        minfld := dummy.Field('mi');
        minfld.AsUInt64 := min_value;
      end;
    if max_is_max then
      maxfld := nil
    else
      begin
        maxfld := dummy.Field('ma');
        maxfld.AsUInt64 := max_value;
      end;
    ForAllIndexedFieldvalRange(minfld,maxfld,iterator,halt,index_name,ascending,max_count,skipfirst,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

procedure TFRE_DB_COLLECTION.ForAllIndexedRealRange(const min_value, max_value: Double; const iterator: IFRE_DB_ObjectIteratorBrk; var halt: boolean; const index_name: TFRE_DB_NameType; const ascending: boolean; const min_is_null: boolean; const max_is_max: boolean; const max_count: NativeInt; skipfirst: NativeInt; const domain_uid_string: TFRE_DB_GUID_String);
var dummy  : IFRE_DB_Object;
    minfld : IFRE_DB_Field;
    maxfld : IFRE_DB_Field;
begin
  dummy := GFRE_DBI.NewObject;
  try
    if min_is_null then
      minfld := nil
    else
      begin
        minfld := dummy.Field('mi');
        minfld.AsReal64 := min_value;
      end;
    if max_is_max then
      maxfld := nil
    else
      begin
        maxfld := dummy.Field('ma');
        maxfld.AsReal64 := max_value;
      end;
    ForAllIndexedFieldvalRange(minfld,maxfld,iterator,halt,index_name,ascending,max_count,skipfirst,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

procedure TFRE_DB_COLLECTION.ForAllIndexedTextRange(const min_value, max_value: TFRE_DB_String; const iterator: IFRE_DB_ObjectIteratorBrk; var halt: boolean; const index_name: TFRE_DB_NameType; const ascending: boolean; const min_is_null: boolean; const max_is_max: boolean; const max_count: NativeInt; skipfirst: NativeInt; const domain_uid_string: TFRE_DB_GUID_String);
var dummy  : IFRE_DB_Object;
    minfld : IFRE_DB_Field;
    maxfld : IFRE_DB_Field;
begin
  dummy := GFRE_DBI.NewObject;
  try
    if min_is_null then
      minfld := nil
    else
      begin
        minfld := dummy.Field('mi');
        minfld.AsString := min_value;
      end;
    if max_is_max then
      maxfld := nil
    else
      begin
        maxfld := dummy.Field('ma');
        maxfld.AsString := max_value;
      end;
    ForAllIndexedFieldvalRange(minfld,maxfld,iterator,halt,index_name,ascending,max_count,skipfirst,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

procedure TFRE_DB_COLLECTION.ForAllIndexPrefixString(const prefix: TFRE_DB_String; const iterator: IFRE_DB_ObjectIteratorBrk; var halt: boolean; const index_name: TFRE_DB_NameType; const ascending: boolean; const max_count: NativeInt; skipfirst: NativeInt; const domain_uid_string: TFRE_DB_GUID_String);
var dummy     : IFRE_DB_Object;
    minfld    : IFRE_DB_Field;
    maxfld    : IFRE_DB_Field;
    objs      : IFRE_DB_ObjectArray;
    min_field : IFRE_DB_Object;
begin
  dummy := GFRE_DBI.NewObject;
  try
    minfld := dummy.Field('mi');
    minfld.AsString := prefix;
    min_field := FREDB_NewIndexFldValForObjectEncoding(minfld,domain_uid_string);
    FCollConnection.FPersistance_Layer.CollectionGetIndexedObjsRange(FName,min_field,nil,ascending,max_count,skipfirst,objs,true,index_name,FCollConnection.GetUserUIDP);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.RemoveIndexedText(const query_value: TFRE_DB_String; const index_name: TFRE_DB_NameType; const val_is_null: boolean; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(RemoveIndexedFieldval(nil,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsString := query_value;
    result := RemoveIndexedFieldval(dummy.Field('f'),index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.RemoveIndexedSigned(const query_value: int64; const index_name: TFRE_DB_NameType; const val_is_null: boolean; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(RemoveIndexedFieldval(nil,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsInt64 := query_value;
    result := RemoveIndexedFieldval(dummy.Field('f'),index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.RemoveIndexedUnsigned(const query_value: QWord; const index_name: TFRE_DB_NameType; const val_is_null: boolean; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(RemoveIndexedFieldval(nil,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsUInt64 := query_value;
    result := RemoveIndexedFieldval(dummy.Field('f'),index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

function TFRE_DB_COLLECTION.RemoveIndexedReal(const query_value: Double; const index_name: TFRE_DB_NameType; const val_is_null: boolean; const domain_uid_string: TFRE_DB_GUID_String): NativeInt;
var dummy : IFRE_DB_Object;
begin
  if val_is_null then
    exit(RemoveIndexedFieldval(nil,index_name,domain_uid_string));
  dummy := GFRE_DBI.NewObject;
  try
    dummy.Field('f').AsReal64 := query_value;
    result := RemoveIndexedFieldval(dummy.Field('f'),index_name,domain_uid_string);
  finally
    dummy.Finalize;
  end;
end;

procedure TFRE_DB_COLLECTION.ForAllIndexedFieldvalRange(const min_field, max_field: IFRE_DB_Field; const iterator: IFRE_DB_ObjectIteratorBrk; var halt: boolean; const index_name: TFRE_DB_NameType; const ascending: boolean; const max_count: NativeInt; skipfirst: NativeInt; const domain_uid_string: TFRE_DB_GUID_String);
var
    min_val : IFRE_DB_Object;
    max_val : IFRE_DB_Object;
    objs    : IFRE_DB_ObjectArray;
    cnt,i   : NativeInt;
    myhalt  : boolean;

begin
  min_val := FREDB_NewIndexFldValForObjectEncoding(min_field,domain_uid_string);
  max_val := FREDB_NewIndexFldValForObjectEncoding(max_field,domain_uid_string);
  cnt     := FCollConnection.FPersistance_Layer.CollectionGetIndexedObjsRange(FName,min_val,max_val,ascending,max_count,skipfirst,objs,false,index_name,FCollConnection.GetUserUIDP);
  myhalt  := false;
  for i:=0 to cnt-1 do
    begin
      if not myhalt then
        begin
          iterator(objs[i],halt);
          myhalt:=halt;
        end
      else
        objs[i].Finalize;
    end;
end;

function TFRE_DB_COLLECTION.IsVolatile: Boolean;
begin
  result := FIsVolatile;
end;

function TFRE_DB_COLLECTION.ItemCount: Int64;
var obi : IFRE_DB_Object;
begin
  result := FCollConnection.FPersistance_Layer.CollectionGetFirstLastIdxCnt(FName,-3,obi,FCollConnection.GetUserUIDP);
end;

function TFRE_DB_COLLECTION.Count: Int64;
begin
  result := ItemCount;
end;

function TFRE_DB_COLLECTION.First: TFRE_DB_Object;
var obi : IFRE_DB_Object;
    cnt : NativeInt;
begin
  cnt := FCollConnection.FPersistance_Layer.CollectionGetFirstLastIdxCnt(FName,-1,obi,FCollConnection.GetUserUIDP);
  if assigned(obi) then
    result := obi.Implementor as TFRE_DB_Object
  else
    result := nil;
end;

function TFRE_DB_COLLECTION.Last: TFRE_DB_Object;
var obi : IFRE_DB_Object;
    cnt : NativeInt;
begin
  cnt := FCollConnection.FPersistance_Layer.CollectionGetFirstLastIdxCnt(FName,-2,obi,FCollConnection.GetUserUIDP);
  if assigned(obi) then
    result := obi.Implementor as TFRE_DB_Object
  else
    result := nil;
end;

function TFRE_DB_COLLECTION.GetItem(const num: uint64): IFRE_DB_Object;
var obi : IFRE_DB_Object;
    cnt : NativeInt;
begin
  if num<0 then
    raise EFRE_DB_Exception.Create(edb_ERROR,'use a valid index >0');
  cnt := FCollConnection.FPersistance_Layer.CollectionGetFirstLastIdxCnt(FName,num,obi,FCollConnection.GetUserUIDP);
  if assigned(obi) then
    result := obi.Implementor as TFRE_DB_Object
  else
    result := nil;
end;

procedure TFRE_DB_BASE_CONNECTION.ForAllColls(const iterator: TFRE_DB_Coll_Iterator);

  procedure DoAllCollections_Iterate(const collection :IFRE_DB_COLLECTION);
  begin
    iterator(collection.Implementor as TFRE_DB_COLLECTION);
  end;

begin
  _ConnectCheck;
  ForAllCollsI(@DoAllCollections_Iterate);
end;

procedure TFRE_DB_BASE_CONNECTION.ForAllSchemes(const iterator: TFRE_DB_Scheme_Iterator);
begin // Nolock R/O
  _ConnectCheck;
  GFRE_DB.ForAllSchemes(iterator);
end;

procedure TFRE_DB_BASE_CONNECTION.ForAllEnums(const iterator: TFRE_DB_Enum_Iterator);
  procedure Iterate(const obj:TFRE_DB_Object);
  begin
    iterator(obj as TFRE_DB_Enum);
  end;
  procedure SysIterator(const obj:TFRE_DB_Enum);
  begin
    iterator(obj);
  end;

begin // Nolock R/O
  _ConnectCheck;
  GFRE_DB.ForAllEnums(@SysIterator);
end;

procedure TFRE_DB_BASE_CONNECTION.ForAllClientFieldValidators(const iterator: TFRE_DB_ClientFieldValidator_Iterator);
  procedure Iterate(const obj:TFRE_DB_Object);
  begin
    iterator(obj as TFRE_DB_ClientFieldValidator);
  end;
  procedure IterateSys(const val:TFRE_DB_ClientFieldValidator);
  begin
    iterator(val);
  end;
begin // Nolock R/O
  _ConnectCheck;
  GFRE_DB.ForAllClientFieldValidators(@IterateSys);
end;

procedure TFRE_DB_BASE_CONNECTION.ForAllCollsI(const iterator: IFRE_DB_Coll_Iterator);
var colls : TFRE_DB_NameTypeArray;
    cname : TFRE_DB_NameType;
begin // Nolock R/O
  _ConnectCheck;
  colls :=  FPersistance_Layer.FDB_GetAllCollsNames;
  for cname in colls do
    iterator(GetCollection(cname));
end;

procedure TFRE_DB_BASE_CONNECTION.ForAllSchemesI(const iterator: IFRE_DB_Scheme_Iterator);
  procedure Iterate(const obj:TFRE_DB_SchemeObject);
  begin
    iterator(obj);
  end;
begin // Nolock R/O
  _ConnectCheck;
  ForAllSchemes(@Iterate);
end;

procedure TFRE_DB_BASE_CONNECTION.ForAllEnumsI(const iterator: IFRE_DB_Enum_Iterator);
  procedure Iterate(const obj:TFRE_DB_Enum);
  begin
    iterator(obj);
  end;
begin // Nolock R/O
  _ConnectCheck;
  ForAllEnums(@Iterate);
end;

procedure TFRE_DB_BASE_CONNECTION.ForAllClientFieldValidatorsI(const iterator: IFRE_DB_ClientFieldValidator_Iterator);
  procedure Iterate(const obj:TFRE_DB_ClientFieldValidator);
  begin
    iterator(obj);
  end;
begin // Nolock R/O
  _ConnectCheck;
  ForAllClientFieldValidators(@Iterate);
end;

function  TFRE_DB_BASE_CONNECTION.OverviewDump : TFRE_DB_String;

  procedure DumpAllCollections(const coll : TFRE_DB_COLLECTION);
    procedure DumpNames(const obj : TFRE_DB_Object);
    var name:TFRE_DB_String;
    begin
      result:=result+'  '+obj.GetFormattedDisplay+LineEnding;
      if obj.SubFormattedDisplayAvailable then begin
        result := result+obj.GetSubFormattedDisplay(4)+LineEnding;
      end;
    end;
  begin
    result := result+'<'+coll.CollectionName+' / '+coll.ClassName+'>'+LineEnding;
    coll.ForAll(@DumpNames);
  end;

  procedure DumpSyso(const obj:TFRE_DB_Object);
  var name:TFRE_DB_String;
  begin
    result:=result+'  '+obj.GetFormattedDisplay+LineEnding;
    if obj.SubFormattedDisplayAvailable then begin
      result := result+obj.GetSubFormattedDisplay(4)+LineEnding;
    end;
  end;

  procedure DumpSCH(const obj : TFRE_DB_SchemeObject);
  begin
     result := result +'   SCHEME:'+ obj.DefinedSchemeName+LineEnding;
  end;
  procedure DumpENU(const obj : TFRE_DB_Enum);
  begin
   DumpSyso(obj);
  end;
  procedure DumpCLF(const obj : TFRE_DB_ClientFieldValidator);
  begin
   DumpSyso(obj);
  end;

begin
  result := 'DATABASE OVERVIEW : '+FDBName+LineEnding;
  ForAllColls(@DumpAllCollections);
  result := result + 'SYSTEM - Registered Objects'+LineEnding;
  result := result+'<SYSTEM SCHEMES>'+LineEnding;
  GFRE_DB.ForAllSchemes(@DumpSCH);
  result := result+'<SYSTEM ENUMS>'+LineEnding;
  GFRE_DB.ForAllEnums(@DumpENU);
  result := result+'<SYSTEM CLIENTFIELD VALIDATORS>'+LineEnding;
  GFRE_DB.ForAllClientFieldValidators(@DumpCLF);
end;

function TFRE_DB_BASE_CONNECTION._Connect(const db: TFRE_DB_String; const is_clone_connect: boolean): TFRE_DB_Errortype;
begin
  if is_clone_connect then
    begin
      if not FCloned then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'must be cloned');
    end
  else
    begin
      _CloneCheck;
      if FConnected then
        exit(edb_ALREADY_CONNECTED);
    end;
  if is_clone_connect then
    result :=   GFRE_DB_PS_LAYER.Connect(db,FPersistance_Layer,nil) { don't set a notif if on cloned connect}
  else
    result :=   GFRE_DB_PS_LAYER.Connect(db,FPersistance_Layer,self);
  if result=edb_OK then
    begin
      FConnected         := true;
      try
          InternalSetupConnection;
      except
        on e:Exception do
          begin
            GFRE_DBI.LogEmergency(dblc_DB,'internal setup connection to [%s] failed due [%s]',[db,e.Message]);
            exit(edb_INTERNAL);
          end;
      end;
      FDBName            := db;
    end
  else
   begin
     FConnected         :=false;
     FDBName            :='';
   end;
end;

function TFRE_DB_BASE_CONNECTION.ConnectedName: TFRE_DB_String;
begin
  _ConnectCheck;
  result := FDBName;
end;

procedure TFRE_DB_BASE_CONNECTION.InternalSetupConnection;
begin

end;

constructor TFRE_DB_BASE_CONNECTION.Create(const clone: boolean);
begin
  GFRE_TF.Get_Lock(FConnLock);
  FCollectionStore  := _TFRE_DB_CollectionTree.create(@FREDB_DBString_Compare);
  FConnected        := false;
  if clone then
    FCloned      := true
  else
    begin
      FConnectionClones.InitSparseListPtrCmp(5);
    end;
end;

function TFRE_DB_BASE_CONNECTION.Implementor: TObject;
begin
  result := self;
end;

procedure TFRE_DB_BASE_CONNECTION._AddCollectionToStore(const coll_name: TFRE_DB_NameType; const in_memory_only: boolean);
var  lcollection      : TFRE_DB_Collection;
     up_collname      : TFRE_DB_NameType;
begin
 up_collname := uppercase(coll_name);
 if not FCollectionStore.Exists(up_collname) then
   begin
     lcollection := TFRE_DB_COLLECTION.Create(self,coll_name,in_memory_only);
     if not FCollectionStore.Add(up_collname,lcollection) then
         raise EFRE_DB_Exception.create(edb_INTERNAL,'collectionstore/internal _AddCollectionTStore');
   end;
end;

procedure TFRE_DB_BASE_CONNECTION.AcquireConnLock;
begin
  FConnLock.Acquire;
end;

procedure TFRE_DB_BASE_CONNECTION.ReleaseConnLock;
begin
  FConnLock.Release;
end;

procedure TFRE_DB_BASE_CONNECTION.BindUserSession(const session: IFRE_DB_Usersession);
begin
  AcquireConnLock;
  try
    if not FCloned then
      raise EFRE_DB_Exception.Create(edb_ERROR,'only cloned connections can be bound !');
    if assigned(FBoundSession) then
      raise EFRE_DB_Exception.Create(edb_ERROR,'connection already has a bound  session!');
    FBoundSession := session;
  finally
    ReleaseConnLock;
  end;
end;

procedure TFRE_DB_BASE_CONNECTION.ClearUserSessionBinding;
begin
  AcquireConnLock;
  try
    if not FCloned then
      raise EFRE_DB_Exception.Create(edb_ERROR,'only cloned connections can be unbound !');
    FBoundSession := nil;
  finally
    ReleaseConnLock;
  end;
end;

function TFRE_DB_BASE_CONNECTION.GetNotifBlock: IFRE_DB_DBChangedNotificationBlock;
begin
  if FCloned then
    result := self
  else
    raise EFRE_DB_Exception.Create(edb_Internal,'this function was intended to get the notif from the session bound,impersonated (cloned) connection.');
end;

procedure TFRE_DB_BASE_CONNECTION.SendNotificationBlock(const block: IFRE_DB_Object);

  procedure SendBlockToClones(var conn : TFRE_DB_BASE_CONNECTION ; const idx :NativeInt ; var halt : boolean);
  begin
    conn.AcquireConnLock;
    try
      if not conn.FCloned then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'must be a clone !');
      try
        FREDB_ApplyNotificationBlockToNotifIF_Connection(block.CloneToNewObject,conn); { metadata changes for cloned connection }
        if assigned(conn.FBoundSession) then
          conn.FBoundSession.InboundNotificationBlock(block.CloneToNewObject); { apply block to session, session updates -> differential FORM updates }
      except
        on E: Exception do
          begin
            GFRE_DBI.LogError(dblc_SERVER,'SENT NOTIFY BLOCK/PROXY TO %s / %s',[conn.FDBName,conn.UpcastSYS.GetCurrentUserTokenRef.GetFullUserLogin]);
          end;
      end;
    finally
      conn.ReleaseConnLock;
    end;
  end;

begin
  try
    FREDB_ApplyNotificationBlockToNotifIF_Connection(block,self); { metadata changes for master connection }
    FConnectionClones.ForAllBreak(@SendBlockToClones);            { send metatada, and session updates }
    if assigned(GFRE_DB_TCDM) then
      GFRE_DB_TCDM.InboundNotificationBlock(FDBName,block);         { route data updates to the TCDM, Query updates (Grids) }
  except on e:exception do
    begin
      raise;
    end;
  end;
end;

procedure TFRE_DB_BASE_CONNECTION.CollectionCreated(const coll_name: TFRE_DB_NameType; const in_memory_only: boolean ;  const tsid: TFRE_DB_TransStepId);
begin
  _AddCollectionToStore(coll_name,in_memory_only);
end;

procedure TFRE_DB_BASE_CONNECTION.CollectionDeleted(const coll_name: TFRE_DB_NameType; const tsid: TFRE_DB_TransStepId);
begin
  //abort;
end;

procedure TFRE_DB_BASE_CONNECTION.IndexDefinedOnField(const coll_name: TFRE_DB_NameType; const FieldName: TFRE_DB_NameType; const FieldType: TFRE_DB_FIELDTYPE; const unique: boolean; const ignore_content_case: boolean; const index_name: TFRE_DB_NameType; const allow_null_value: boolean; const unique_null_values: boolean; const tsid: TFRE_DB_TransStepId);
begin
  //abort;
end;

procedure TFRE_DB_BASE_CONNECTION.IndexDroppedOnField(const coll_name: TFRE_DB_NameType; const index_name: TFRE_DB_NameType; const tsid: TFRE_DB_TransStepId);
begin

end;

function TFRE_DB_BASE_CONNECTION.GetDatabaseObjectCount(const Schemes: TFRE_DB_StringArray): NativeInt;
begin
  result := FPersistance_Layer.FDB_GetObjectCount(false,Schemes);
end;

procedure TFRE_DB_BASE_CONNECTION.ForAllDatabaseObjectsDo(const dbo: IFRE_DB_ObjectIteratorBrkProgress; const Schemes: TFRE_DB_StringArray);
var max,curr : NativeInt;

  procedure local(const obj : IFRE_DB_Object ; var break : boolean);
  begin
   dbo(obj,break,curr,max);
   inc(curr);
  end;

begin
  max  := GetDatabaseObjectCount(Schemes);
  curr := 1;
  FPersistance_Layer.FDB_ForAllObjects(@local,schemes);
end;

function TFRE_DB_BASE_CONNECTION.IsReferenced(const obj_uid: TFRE_DB_GUID; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): Boolean;
begin
  result := FPersistance_Layer.GetReferencesCount(obj_uid,false,scheme_prefix_filter,field_exact_filter)>0;
end;

function TFRE_DB_BASE_CONNECTION.IsReferenced(const obj_uid: TFRE_DB_GUID; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): Boolean;
begin
  result := IsReferenced(obj_uid,scheme_prefix_filter,field_exact_filter);
end;

function TFRE_DB_BASE_CONNECTION.GetReferences(const obj_uid: TFRE_DB_GUID; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): TFRE_DB_GUIDArray;
begin
  result := FPersistance_Layer.GetReferences(obj_uid,from,scheme_prefix_filter,field_exact_filter);
end;

function TFRE_DB_BASE_CONNECTION.GetReferencesCount(const obj_uid: TFRE_DB_GUID; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): NativeInt;
begin
  result := FPersistance_Layer.GetReferencesCount(obj_uid,from,scheme_prefix_filter,field_exact_filter);
end;

function TFRE_DB_BASE_CONNECTION.GetReferencesDetailed(const obj_uid: TFRE_DB_GUID; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): TFRE_DB_ObjectReferences;
begin
   result := FPersistance_Layer.GetReferencesDetailed(obj_uid,from,scheme_prefix_filter,field_exact_filter);
end;

destructor TFRE_DB_BASE_CONNECTION.Destroy;

  procedure FinalizeCollection(const coll:TFRE_DB_COLLECTION);
  begin
    coll.free;
  end;

begin
  AcquireConnLock;
  if assigned(FBoundSession) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'never, ever free a bound connection');
  if assigned(FPersistance_Layer) then
    FPersistance_Layer.Disconnect;
  FCollectionStore.ForAllItems(@FinalizeCollection);
  FCollectionStore.Free;
  FCollectionStore:=nil;
  FConnLock.Finalize;
  FConnLock:=nil;
end;

function TFRE_DB_CONNECTION.CreateAClone: TFRE_DB_CONNECTION;
begin
  if not FProxySysconnection then
    raise EFRE_DB_Exception.Create(edb_ERROR,'the system connection is only tested in proxy mode');

  Result := TFRE_DB_CONNECTION.Create(true);
  Result.FClonedFrom := self;
  CheckDbResult(result._Connect(FDBName,true));
  FConnectionClones.Add(result);

  Result.FSysConnection := TFRE_DB_SYSTEM_CONNECTION.Create(true);
  CheckDbResult(result.FSysConnection._Connect('SYSTEM',true));
  Result.FSysConnection.FClonedFrom := self.FSysConnection;
  FSysConnection.FConnectionClones.Add(result.FSysConnection);
end;

procedure TFRE_DB_CONNECTION.InternalSetupConnection;

  procedure SetupNoteCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysNoteCollection') then begin
      coll := Collection('SysNoteCollection'); // Instance (new) Collections here with false parameter
      coll.DefineIndexOnField('link',fdbft_String,True,True);
    end;
    FSysNotes := Collection('SysNoteCollection');
  end;

  procedure SetupMachineCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists(cFRE_DB_MACHINE_COLLECTION) then begin
      coll := Collection(cFRE_DB_MACHINE_COLLECTION); // Instance (new) Collections here with false parameter
      coll.DefineIndexOnField('objname',fdbft_String,true,true,'def',false);
    end;
  end;

begin
  SetupNoteCollection;
  SetupMachineCollection;
  inherited InternalSetupConnection;
end;

procedure TFRE_DB_CONNECTION.BindUserSession(const session: IFRE_DB_Usersession);
begin
  FSysConnection.BindUserSession(session);
  inherited BindUserSession(session);
end;

procedure TFRE_DB_CONNECTION.ClearUserSessionBinding;
begin

 FSysConnection.ClearUserSessionBinding;
  inherited ClearUserSessionBinding;
end;

function TFRE_DB_CONNECTION.GetDatabaseName: TFRE_DB_String;
begin
  result := FDBName;
end;


function TFRE_DB_CONNECTION.ImpersonateClone(const user, pass: TFRE_DB_String;out conn:TFRE_DB_CONNECTION): TFRE_DB_Errortype;
begin
  result := CheckLogin(user,pass);
  if result<>edb_OK then
    exit;
  conn := CreateAClone;
  CheckDbResult(conn.FSysConnection.ImpersonateTheClone(user,pass));
end;

function TFRE_DB_CONNECTION.Connect(const db, user, pass: TFRE_DB_String; const ProxySysConnection: TFRE_DB_SYSTEM_CONNECTION): TFRE_DB_Errortype;
begin
    if FConnected then exit(edb_ALREADY_CONNECTED);
    if uppercase(db)='SYSTEM' then
      exit(edb_ACCESS);
    FSysConnection := ProxySysConnection;
    if not assigned(FSysConnection) then
      begin
        FSysConnection := GFRE_DB.NewDirectSysConnection;
        result := FSysConnection.Connect(user,pass);
        if result=edb_NOT_FOUND then
          exit(edb_DB_NO_SYSTEM)
        else
        if result<>edb_OK then
          exit(Result);
      end
    else
      FProxySysconnection := true;
    result := _Connect(db,false);
    FSysConnection.FPairedAppDBConn := self;
end;

function TFRE_DB_CONNECTION.Connect(const db: TFRE_DB_String; const user: TFRE_DB_String; const password: TFRE_DB_String): TFRE_DB_Errortype;
begin //nl
  result := Connect(db,user,password,nil);
end;

function TFRE_DB_CONNECTION.CheckLogin(const user, pass: TFRE_DB_String): TFRE_DB_Errortype;
begin //nl
  if not FConnected then
    exit(edb_NOT_CONNECTED);
  result := FSysConnection.CheckLogin(user,pass);
end;


function TFRE_DB_CONNECTION.CollectionExists(const name: TFRE_DB_NameType): boolean;
begin //nl
    Result := inherited CollectionExists(name);
end;

function TFRE_DB_CONNECTION.DeleteCollection(const name: TFRE_DB_NameType): TFRE_DB_Errortype;
begin //nl
  Result:=inherited DeleteCollection(name);
end;


function TFRE_DB_BASE_CONNECTION.CollectionList(const with_classes:boolean=false): IFOS_STRINGS;
var SL:IFOS_STRINGS;

   procedure AddToStrings(const coll:TFRE_DB_COLLECTION);
   begin
     if not with_classes then begin
       sl.Add(coll.CollectionName);
     end else begin
       sl.Add(coll.CollectionName+':'+coll.ClassName);
     end;
   end;

begin
  SL:= GFRE_TF.Get_FOS_Strings;
  ForAllColls(@AddToStrings);
  Result := sl;
end;

function TFRE_DB_BASE_CONNECTION.FetchDomainUIDbyName(const name: TFRE_DB_NameType; var domain_uid: TFRE_DB_GUID): boolean;
begin
  abort;
end;

function TFRE_DB_BASE_CONNECTION.GetCollection(const collection_name: TFRE_DB_NameType): IFRE_DB_COLLECTION;
begin
  result := Collection(collection_name,false);
  if not assigned(result) then
    raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'the collection [%s] does not exist',[collection_name]);
end;

function TFRE_DB_BASE_CONNECTION.CreateCollection(const collection_name: TFRE_DB_NameType; const in_memory: boolean): IFRE_DB_COLLECTION;
begin
  if CollectionExists(collection_name) then
    raise EFRE_DB_Exception.Create(edb_EXISTS,'the collection [%s] already exists',[collection_name]);
 result :=  Collection(collection_name,true,in_memory);
end;


function TFRE_DB_BASE_CONNECTION.CollectionExists(const name: TFRE_DB_NameType): boolean;
begin
  result := FPersistance_Layer.CollectionExistCollection(name);
end;

procedure TFRE_DB_BASE_CONNECTION.Finalize;
begin
  free;
end;

function TFRE_DB_BASE_CONNECTION.DatabaseExists(const dbname: TFRE_DB_String): Boolean;
begin
  result := FPersistance_Layer.DatabaseExists(dbname);
end;

function TFRE_DB_BASE_CONNECTION.CreateDatabase(const dbname: TFRE_DB_String): TFRE_DB_Errortype;
begin
  result := FPersistance_Layer.CreateDatabase(dbname);
end;

function TFRE_DB_BASE_CONNECTION.DeleteDatabase(const dbname: TFRE_DB_String): TFRE_DB_Errortype;
begin
  result := FPersistance_Layer.DeleteDatabase(dbname);
end;

procedure TFRE_DB_BASE_CONNECTION.DumpSystem;
begin

end;


function TFRE_DB_BASE_CONNECTION.FetchI(const ouid: TFRE_DB_GUID; out dbo: IFRE_DB_Object): TFRE_DB_Errortype;
var ldbo : TFRE_DB_Object;
begin // Nolock IF
  result := Fetch(ouid,ldbo);
  if result=edb_OK then
    dbo := ldbo
  else
    dbo := nil;
end;

function TFRE_DB_BASE_CONNECTION.FetchAsIntf(const ouid: TFRE_DB_GUID; const IntfSpec: ShortString; out Intf): TFRE_DB_Errortype;
var ldbo:TFRE_DB_Object;
begin // Nolock /  fetch locks
  result := Fetch(ouid,ldbo);
  if result=edb_OK then
    ldbo.IntfCast(IntfSpec,intf);
end;

function TFRE_DB_CONNECTION.Delete(const ouid: TFRE_DB_GUID): TFRE_DB_Errortype;
begin  //nl
  if FSysConnection.Exists(oUID) then
    Result:=FSysConnection.Delete(ouid)
  else
    Result:=inherited Delete(ouid);
end;

function TFRE_DB_CONNECTION.Update(const dbo: TFRE_DB_Object; const collection_name: TFRE_DB_NameType): TFRE_DB_Errortype;
begin
  if dbo.DomainID=CFRE_DB_NullGUID then
    dbo.SetDomainID(GetMyDomainID);
  if FSysConnection.Exists(dbo.UID) then
    Result:=FSysConnection.Update(dbo, collection_name)
  else
    Result:=inherited Update(dbo, collection_name);
end;

destructor TFRE_DB_CONNECTION.Destroy;
begin
  if not FProxySysconnection then
    FSysConnection.Free;
   if FCloned and
      not FClonedFrom.FConnectionClones.Delete(self) then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'something is rotten in the state of denmark');
  inherited Destroy;
end;

function  TFRE_DB_CONNECTION.FetchApplications(var apps: TFRE_DB_APPLICATION_ARRAY; var loginapp: TFRE_DB_APPLICATION):TFRE_DB_Errortype;
var l_apps : TFRE_DB_APPLICATION_ARRAY;
    cnt,i  : integer;
begin
  if not FConnected then
    exit(edb_NOT_CONNECTED);
  if not assigned(FSysConnection) then
    Exit(edb_ACCESS);
  result := FSysConnection.FetchApplications(l_apps, loginapp);
  SetLength(apps,length(l_apps));
  cnt := 0;
  for i := 0 to high(l_apps) do begin
    if FSysConnection.CheckClassRight4MyDomain(sr_FETCH,l_apps[i].ClassType)
       or (FSysConnection.CheckClassRight4AnyDomain(sr_FETCH,l_apps[i].ClassType) and l_apps[i].isMultiDomainApp) then
         begin
          apps[cnt] := l_apps[i];
          inc(cnt);
         end;
  end;
  setlength(apps,cnt);
end;

function TFRE_DB_CONNECTION.InvokeMethod(const class_name, method_name: TFRE_DB_String; const uid_path: TFRE_DB_GUIDArray; var input: IFRE_DB_Object; const session: TFRE_DB_UserSession): IFRE_DB_Object;
var scheme : TFRE_DB_SchemeObject;
    implem : TFRE_DB_Object;
begin
  if not GFRE_DB.GetSystemScheme(class_name,scheme) then raise EFRE_DB_Exception.Create(edb_ERROR,'SCHEME [%s] IS UNKNOWN',[class_name]);
  if assigned(input) then begin
    implem := input.Implementor as TFRE_DB_Object;
    result := scheme.InvokeMethod_UID_Session(uid_path,class_name,method_name,implem,self,session);
    if not assigned(implem) then
      input := nil;
  end else begin
    implem:=nil;
    result := scheme.InvokeMethod_UID_Session(uid_path,class_name,method_name,implem,self,session);
  end;
end;


function TFRE_DB_BASE_CONNECTION.NewScheme(const Scheme_Name: TFRE_DB_String; const parent_scheme_name: TFRE_DB_String): TFRE_DB_SchemeObject;
begin
  result := GFRE_DB.NewScheme(Scheme_Name,dbst_DB);
  if parent_scheme_name<>'' then begin
     result.SetParentSchemeByName(parent_scheme_name);
  end;
end;

function TFRE_DB_BASE_CONNECTION.NewSchemeI(const Scheme_Name: TFRE_DB_String; const parent_scheme_name: TFRE_DB_String): IFRE_DB_SchemeObject;
begin // Nolock IF
  result := NewScheme(Scheme_Name,parent_scheme_name);
end;

function TFRE_DB_BASE_CONNECTION.CollectionCC(const collection_name: TFRE_DB_NameType; const create_non_existing: boolean; const in_memory_only: boolean): TFRE_DB_COLLECTION;
var lcollection  : TFRE_DB_Collection;
    keyp         : PFRE_DB_NameType;
    storep       : ^TFRE_DB_Collection;
    FUPcoll_name : TFRE_DB_String;

begin
  result:=nil;
  FUPcoll_name    := uppercase(collection_name);
  if FUPcoll_name='' then
    raise EFRE_DB_Exception.Create(edb_INVALID_PARAMS,'you must supply a collectionname when creating a collection');
  if FCollectionStore.Find(FUPcoll_name,lcollection) then begin
    result := lcollection;
  end else begin
    if FPersistance_Layer.CollectionExistCollection(collection_name) then
      begin { The Layer has the Collection but it's not in my store -> add it}
        lcollection := TFRE_DB_COLLECTION.Create(self,collection_name,in_memory_only);
        if not FCollectionStore.Add(FUPcoll_name,lcollection) then
          raise EFRE_DB_Exception.create(edb_INTERNAL,'collectionstore');
        exit(lcollection);
      end;
    if create_non_existing then begin
      FPersistance_Layer.CollectionNewCollection(collection_name,in_memory_only);
      _AddCollectionToStore(collection_name,in_memory_only);
      if not FCollectionStore.Find(FUPcoll_name,lcollection) then
        raise EFRE_DB_Exception.Create(edb_ERROR,'cannot fetch created collection / fail');
      exit(lcollection);
    end;
    result := nil;
  end;
end;

function TFRE_DB_BASE_CONNECTION.DeleteCollection(const name: TFRE_DB_NameType): TFRE_DB_Errortype;
var c_name     : TFRE_DB_String;
    lcollection : TFRE_DB_COLLECTION;
begin
  result := edb_OK;
  c_name := uppercase(name);
  if (c_name='MASTER') or (c_name='SCHEME') then // TODO - Check /
    exit(edb_RESERVED);
  //if not FCollectionStore.Find(c_name,lcollection) then
  //  exit(edb_NOT_FOUND);
  try
    FPersistance_Layer.CollectionDeleteCollection(name);
  except
    on E:EFRE_DB_Exception do
      begin
        result.Code := E.ErrorType;
        result.Msg  := E.Message;
        exit;
      end;
    on E:Exception do
      begin
        result.Code := edb_ERROR;
        result.Msg  := e.Message;
        exit;
      end;
  end;
  FCollectionStore.Delete(c_name,lcollection);
  lcollection.Free;
end;

function TFRE_DB_BASE_CONNECTION.NewObjectCC(const ObjClass: TFRE_DB_OBJECTCLASS): TFRE_DB_Object;
begin

end;


function TFRE_DB_BASE_CONNECTION.Collection(const collection_name: TFRE_DB_NameType;const create_non_existing:boolean=true;const in_memory:boolean=false):TFRE_DB_COLLECTION;
begin
  result:=CollectionCC(collection_name,create_non_existing,in_memory);
end;


function TFRE_DB_BASE_CONNECTION.CollectionI(const collection_name: TFRE_DB_NameType;const create_non_existing:boolean=true;const in_memory:boolean=false): IFRE_DB_COLLECTION;
begin // Nolock IF
  result := Collection(collection_name,create_non_existing,in_memory);
end;

function TFRE_DB_BASE_CONNECTION.Exists(const ouid: TFRE_DB_GUID): boolean;
begin
  _ConnectCheck;
  FPersistance_Layer.ObjectExists(ouid);
end;

function TFRE_DB_BASE_CONNECTION.Delete(const ouid: TFRE_DB_GUID): TFRE_DB_Errortype;
var dbo     : TFRE_DB_Object;
begin
  _ConnectCheck;
  try
    if DeleteAccessRightTest(ouid) then
      begin
        FPersistance_Layer.DeleteObject(ouid,'');
        result:=edb_OK;
      end
    else
      result:=edb_ACCESS;
  except
    on E:EFRE_DB_Exception do
      begin
        result.Code := E.ErrorType;
        result.Msg  := E.Message;
      end;
    on E:Exception do
      begin
        result.Code := edb_ERROR;
        result.Msg  := e.Message;
      end;
  end;
end;


//function TFRE_DB_BASE_CONNECTION.InternalCheckRight(const right_name: TFRE_DB_String): boolean;
//begin
//  if self is TFRE_DB_CONNECTION then
//    exit(TFRE_DB_CONNECTION(self).CheckRight(right_name));
//  if self is TFRE_DB_SYSTEM_CONNECTION then
//    exit(TFRE_DB_SYSTEM_CONNECTION(self).CheckRight(right_name));
//  raise EFRE_DB_Exception.Create(edb_INTERNAL,'checkright basecass : '+self.ClassName);
//end;


function TFRE_DB_BASE_CONNECTION.UpcastDBC: TFRE_DB_Connection;
begin
  if self is TFRE_DB_CONNECTION then
    exit(TFRE_DB_CONNECTION(self));
  raise EFRE_DB_Exception.Create(edb_INTERNAL,'Upcast failed basecass : '+self.ClassName);
end;

function TFRE_DB_BASE_CONNECTION.UpcastSYS: TFRE_DB_SYSTEM_CONNECTION;
begin
  if self is TFRE_DB_SYSTEM_CONNECTION then
    exit(TFRE_DB_SYSTEM_CONNECTION(self));
  if self is TFRE_DB_CONNECTION then
    exit(TFRE_DB_SYSTEM_CONNECTION(TFRE_DB_CONNECTION(self)));
  raise EFRE_DB_Exception.Create(edb_INTERNAL,'Upcast SYS failed basecass : '+self.ClassName);
end;

function TFRE_DB_BASE_CONNECTION.GetMyDomainID_String: TFRE_DB_GUID_String;
begin
  result := uppercase(FREDB_G2H(GetMyDomainID)); // GetMyDomainID_String has to be uppercase
end;

function TFRE_DB_BASE_CONNECTION.GetSystemDomainID_String: TFRE_DB_GUID_String;
begin
  if GetSysDomainUID=CFRE_DB_NullGUID then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'GetSystemDomainID_String FSysDomainID is NULL');
  result := uppercase(FREDB_G2H(GetSysDomainUID)); // GetSystemDomainID_String has to be uppercase
end;


function TFRE_DB_BASE_CONNECTION.Fetch(const ouid: TFRE_DB_GUID; out dbo: TFRE_DB_Object; const without_right_check: boolean): TFRE_DB_Errortype;
var dbi : IFRE_DB_Object;

begin
  dbo    := nil;
  try
    result := FPersistance_Layer.Fetch(ouid,dbi);
    if result <> edb_OK then
      exit(Result)
    else
      begin
        if assigned(dbi) then
          begin
            dbo    := dbi.Implementor as TFRE_DB_Object;
            exit(CheckAccessRightAndCondFinalize(dbi,sr_FETCH,without_right_check));
          end;
      end;
    if GFRE_DB.FetchSysObject(ouid,dbo) then
      exit(CheckAccessRightAndCondFinalize(dbo,sr_FETCH,without_right_check));
    exit(edb_NOT_FOUND);
  except
    on E:EFRE_DB_Exception do
      begin
        result.Code := E.ErrorType;
        result.Msg  := E.Message;
      end;
    on E:Exception do
      begin
        result.Code := edb_ERROR;
        result.Msg  := e.Message;
      end;
  end;
end;

function TFRE_DB_BASE_CONNECTION.BulkFetchNoRightCheck(const uids: TFRE_DB_GUIDArray; out dbos: IFRE_DB_ObjectArray): TFRE_DB_Errortype;
begin
  try
    result := FPersistance_Layer.BulkFetch(uids,dbos);
  except
    on E:EFRE_DB_Exception do
      begin
        result.Code := E.ErrorType;
        result.Msg  := E.Message;
      end;
    on E:Exception do
      begin
        result.Code := edb_ERROR;
        result.Msg  := e.Message;
      end;
  end;
end;

function TFRE_DB_BASE_CONNECTION.BulkFetch(const uids: TFRE_DB_GUIDArray; out dbos: IFRE_DB_ObjectArray): TFRE_DB_Errortype; { todo -> move to PL Layer }
var all_dbos : IFRE_DB_ObjectArray;
    cnt,i    : NativeInt;
begin
  try
    dbos := nil;
    result := FPersistance_Layer.BulkFetch(uids,all_dbos);
    if result=edb_OK then
      begin
        SetLength(dbos,Length(all_dbos));
        cnt    := 0;
        for i  := 0 to high(all_dbos) do
          begin
            if CheckAccessRightAndCondFinalize(all_dbos[i],sr_FETCH)=edb_OK then
              begin
                dbos[cnt] := all_dbos[i];
                inc(cnt);
              end;
          end;
        SetLength(dbos,cnt);
      end;
  except
    on E:EFRE_DB_Exception do
      begin
        result.Code := E.ErrorType;
        result.Msg  := E.Message;
      end;
    on E:Exception do
      begin
        result.Code := edb_ERROR;
        result.Msg  := e.Message;
      end;
  end;
end;

function TFRE_DB_BASE_CONNECTION.FetchAccessRightTest(const ouid: TFRE_DB_GUID): boolean;
var dbo : TFRE_DB_Object;
    res : TFRE_DB_Errortype;
begin
  Res := Fetch(ouid,dbo);
  if Res = edb_OK then
    begin
      result := true;
      dbo.Finalize;
    end
  else
    begin
      result := false;
    end;
end;

function TFRE_DB_BASE_CONNECTION.DeleteAccessRightTest(const ouid: TFRE_DB_GUID): boolean;
var dbo : TFRE_DB_Object;
    res : TFRE_DB_Errortype;
begin
  Res := Fetch(ouid,dbo,true);
  if Res = edb_OK then
    begin
      result := CheckAccessRightAndCondFinalize(dbo,sr_DELETE) = edb_OK;
    end
  else
    begin
      result := true; { not fetchable, so possibly deletable :-) (should report edb_NOT_FOUND in next stage) }
    end;
end;

function TFRE_DB_BASE_CONNECTION.CheckAccessRightAndCondFinalize(const dbi: IFRE_DB_Object; const sr: TFRE_DB_STANDARD_RIGHT; const without_right_check: boolean; const cond_finalize: boolean): TFRE_DB_Errortype;
var
    ut     : IFRE_DB_USER_RIGHT_TOKEN;
begin
  ut := GetCurrentUserTokenRef;
  if not assigned(ut) then
    exit(edb_OK)
  else
    result := ut.CheckStdRightAndCondFinalize(dbi,sr,without_right_check,cond_finalize);
end;

function TFRE_DB_BASE_CONNECTION.Update(const dbo: TFRE_DB_Object; const collection_name: TFRE_DB_NameType): TFRE_DB_Errortype;
var objclass : TClass;
    ut       : IFRE_DB_USER_RIGHT_TOKEN;
begin
  objclass := dbo.Implementor_HC.ClassType;
  ut       := GetCurrentUserTokenRef;
  if not
     ((not assigned(ut))
      or (ut.CheckStdRightAndCondFinalize(dbo,sr_UPDATE)=edb_OK))
     then
       exit(edb_ACCESS);
  try
    FPersistance_Layer.StoreOrUpdateObject(dbo,collection_name,false);
    result := edb_OK;
  except
    on E:EFRE_DB_Exception do
      begin
        result.Code := E.ErrorType;
        result.Msg  := E.Message;
      end;
    on E:Exception do
      begin
        result.Code := edb_ERROR;
        result.Msg  := e.Message;
      end;
  end;
end;

function TFRE_DB_BASE_CONNECTION.UpdateI(const dbo: IFRE_DB_Object): TFRE_DB_Errortype;
begin //nl
  result := Update(dbo.Implementor as TFRE_DB_Object);
end;

function TFRE_DB_CONNECTION.CreateschemeUniqueKeyDefinition(const schemeClass, FieldName: TFRE_DB_String; const FieldType: TFRE_DB_FIELDTYPE): TFRE_DB_Errortype;
begin
  abort;
end;

function TFRE_DB_CONNECTION.DropschemeUniqueKeyDefinition(const schemeClass, FieldName: TFRE_DB_String; const FieldType: TFRE_DB_FIELDTYPE): TFRE_DB_Errortype;
begin
  abort;
end;


function TFRE_DB_CONNECTION.Fetch(const ouid: TFRE_DB_GUID; out dbo: TFRE_DB_Object; const without_right_check: boolean): TFRE_DB_Errortype;
begin
  dbo := nil;
  Result:=inherited Fetch(ouid, dbo);
  if result=edb_NOT_FOUND then
    begin
      result := FSysConnection.Fetch(ouid,dbo);
      if Assigned(dbo) then
        begin
          E_FOS_TestNosey; { should not be necessary anymore}
        end;
    end;
  if assigned(dbo) and
     (not dbo.IsObjectRoot) then
      begin
        writeln('WARNING : FETCHED A NON OBJECT ROOT ? / SHOULD NOT BE POSSIBLE ');
      end;
end;

function TFRE_DB_CONNECTION.FetchAs(const ouid: TFRE_DB_GUID; const classref: TFRE_DB_BaseClass; var outobj): TFRE_DB_Errortype;
var dbo : TFRE_DB_Object;
begin
  result := Fetch(ouid,dbo);
  if result <> edb_OK then
    exit;
  if dbo.Implementor_HC is classref then
    begin
      Pointer(outobj) := dbo.Implementor_HC;
      result          := edb_OK;
    end
  else
    begin
     Pointer(outobj) := nil;
     result          := edb_MISMATCH;
    end;
end;

function TFRE_DB_CONNECTION.BulkFetchNoRightCheck(const uids: TFRE_DB_GUIDArray; out dbos: IFRE_DB_ObjectArray): TFRE_DB_Errortype;
begin
 dbos := nil;
 Result:=inherited BulkFetchNoRightCheck(uids, dbos);
 if result=edb_NOT_FOUND then
   result := FSysConnection.BulkFetchNoRightCheck(uids,dbos);
end;

function TFRE_DB_CONNECTION.BulkFetch(const uids: TFRE_DB_GUIDArray; out dbos: IFRE_DB_ObjectArray): TFRE_DB_Errortype;
begin
  dbos   := nil;
  Result := inherited BulkFetch(uids, dbos);
  if result=edb_NOT_FOUND then
    result := FSysConnection.BulkFetch(uids,dbos);
end;

function TFRE_DB_CONNECTION.AdmGetTextResourcesCollection: IFRE_DB_COLLECTION;
begin
 result := FSysConnection.FSysTransText; // TODO: CHECK RIGHTS
end;


function TFRE_DB_CONNECTION.AdmGetUserCollection: IFRE_DB_COLLECTION;
begin //nl
 result := FSysConnection.FSysUsers; // TODO: CHECK RIGHTS
end;

function TFRE_DB_CONNECTION.AdmGetRoleCollection: IFRE_DB_COLLECTION;
begin //nl
 result := FSysConnection.FSysRoles; // TODO: CHECK RIGHTS
end;

function TFRE_DB_CONNECTION.AdmGetGroupCollection: IFRE_DB_COLLECTION;
begin //nl
 result := FSysConnection.FSysGroups; // TODO: CHECK RIGHTS
end;

function TFRE_DB_CONNECTION.AdmGetDomainCollection: IFRE_DB_COLLECTION;
begin //nl
 result := FSysConnection.FSysDomains; // TODO: CHECK RIGHTS
end;

function TFRE_DB_CONNECTION.AdmGetAuditCollection: IFRE_DB_COLLECTION;
begin
  result := FSysConnection.FSysAudit;
end;

function TFRE_DB_CONNECTION.AdmGetWorkFlowCollection: IFRE_DB_COLLECTION;
begin
 result := FSysConnection.FSysWorkflow;
end;

function TFRE_DB_CONNECTION.AdmGetWorkFlowSchemeCollection: IFRE_DB_COLLECTION;
begin
  result := FSysConnection.FSysWorkflowScheme;
end;

function TFRE_DB_CONNECTION.AdmGetWorkFlowMethCollection: IFRE_DB_COLLECTION;
begin
 result := FSysConnection.FSysWorkflowMethods;
end;

function TFRE_DB_CONNECTION.AdmGetNotificationCollection: IFRE_DB_COLLECTION;
begin
  result := FSysConnection.FSysNotifications;
end;

function TFRE_DB_CONNECTION.AdmGetApplicationConfigCollection: IFRE_DB_COLLECTION;
begin
  Result := FSysConnection.FSysAppConfigs;
end;


function TFRE_DB_CONNECTION.FetchUserSessionData(var SessionData: IFRE_DB_OBJECT): boolean;
begin //nl
  result := FSysConnection.FetchUserSessionData(SessionData);
end;

function TFRE_DB_CONNECTION.StoreUserSessionData(var session_data: IFRE_DB_Object):TFRE_DB_Errortype;
begin //nl
  result := FSysConnection.StoreUserSessionData(session_data);
  session_data := nil;
end;

function TFRE_DB_CONNECTION.FetchTranslateableTextObj(const trans_key: TFRE_DB_String; var text: IFRE_DB_TEXT): boolean;
var ttxt      : TFRE_DB_TEXT;
    tkey      : TFRE_DB_String;
    params    : TFRE_DB_StringArray;
    hasparams : boolean;
begin //nl
  tkey := trans_key;
  hasparams := FREDB_TranslatableHasParams(tkey,params);
  if FSysConnection.FetchTranslateableText(tkey,ttxt)=edb_OK then begin
    if hasparams then
      ttxt.ApplyParameters(params);
    text   := ttxt;
    result := true;
  end else begin
    text   := nil;
    result := false;
  end;
end;

function TFRE_DB_CONNECTION.FetchTranslateableTextShort(const translation_key: TFRE_DB_String): TFRE_DB_String;
var txt:IFRE_DB_TEXT;
begin //nl
  if FetchTranslateableTextObj(translation_key,txt) then
    begin
      Result := txt.Getshort;
      txt.Finalize;
    end
  else
    Result := translation_key;
end;

function TFRE_DB_CONNECTION.FetchTranslateableTextLong(const translation_key: TFRE_DB_String): TFRE_DB_String;
var txt:IFRE_DB_TEXT;
begin //nl
  if FetchTranslateableTextObj(translation_key,txt) then
    begin
      Result := txt.GetLong;
      txt.Finalize;
    end
  else
    Result := translation_key;
end;

function TFRE_DB_CONNECTION.FetchTranslateableTextHint(const translation_key: TFRE_DB_String): TFRE_DB_String;
var txt:IFRE_DB_TEXT;
begin //nl
  if FetchTranslateableTextObj(translation_key,txt) then
    begin
      Result := txt.GetHint;
      txt.Finalize;
    end
  else
    Result := translation_key;
end;

function TFRE_DB_CONNECTION.GetReferences(const obj_uid: TFRE_DB_GUID; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): TFRE_DB_GUIDArray;
var result_with_rights : TFRE_DB_GUIDArray;
    i,cnt              : NativeInt;
begin
 result := GetReferencesNoRightCheck(obj_uid,from,scheme_prefix_filter,field_exact_filter);
 SetLength(result_with_rights,Length(result));
 cnt := 0;
 for i:=0 to high(Result) do
   if FetchAccessRightTest(result[i]) then
     begin
       result_with_rights[cnt] := result[i];
       inc(cnt);
     end;
  SetLength(result_with_rights,cnt);
  result := result_with_rights;
end;

function TFRE_DB_CONNECTION.GetReferencesCountNoRightCheck(const obj_uid: TFRE_DB_GUID; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): NativeInt;
begin
  Result:=inherited GetReferencesCount(obj_uid, from,scheme_prefix_filter,field_exact_filter);
end;

function TFRE_DB_CONNECTION.GetReferencesNoRightCheck(const obj_uid: TFRE_DB_GUID; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): TFRE_DB_GUIDArray;
begin
  Result:=inherited GetReferences(obj_uid, from,scheme_prefix_filter,field_exact_filter);
end;

function TFRE_DB_CONNECTION.GetReferencesCount(const obj_uid: TFRE_DB_GUID; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): NativeInt;
begin
  result := Length(GetReferences(obj_uid,from,scheme_prefix_filter,field_exact_filter)); { with respect to rights }
end;

{ Todo add right check}
function TFRE_DB_CONNECTION.GetReferencesDetailed(const obj_uid: TFRE_DB_GUID; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): TFRE_DB_ObjectReferences;
var i,cnt           : NativeInt;
    res_with_rights : TFRE_DB_ObjectReferences;
begin
  Result:=inherited GetReferencesDetailed(obj_uid, from,scheme_prefix_filter,field_exact_filter);
  SetLength(res_with_rights,Length(Result));
  cnt := 0;
  for i:=0 to High(Result) do
    if FetchAccessRightTest(Result[i].linked_uid) then
      begin
        res_with_rights[cnt] := Result[i];
        inc(Cnt);
      end;
  SetLength(res_with_rights,cnt);
  Result := res_with_rights;
end;

function TFRE_DB_CONNECTION.FetchDomainUIDbyName(const name: TFRE_DB_NameType; var domain_uid: TFRE_DB_GUID): boolean;
begin
  result := FSysConnection.FetchDomainUIDbyName(name,domain_uid);
end;

procedure TFRE_DB_CONNECTION.ExpandReferencesNoRightCheck(ObjectList: TFRE_DB_GUIDArray; ref_constraints: TFRE_DB_NameTypeRLArray; var expanded_refs: TFRE_DB_GUIDArray);
var i        : NativeInt;
    obj      : TFRE_DB_Object;
    comparef : TFRE_DB_NameType;
    count    : NativeInt;

  procedure FetchChained(uid:TFRE_DB_GUID ; field_chain : TFRE_DB_NameTypeRLArray ; depth : NativeInt);
  var obrefs   : TFRE_DB_GUIDArray; //TFRE_DB_ObjectReferences;
      i,k      : NativeInt;
      scheme   : TFRE_DB_NameType;
      field    : TFRE_DB_NameType;
      outbound : Boolean;
      spos     : NativeInt;
  begin
    if depth<length(field_chain) then
      begin
        outbound := FREDB_SplitRefLinkDescription(field_chain[depth],field,scheme);
        obrefs   := GetReferencesNoRightCheck(uid,outbound,scheme,field); // GetReferencesDetailed(uid,outbound,scheme,field);
        for i := 0 to  high(obrefs) do
          begin
              FetchChained(obrefs[i],field_chain,depth+1);
          end;
      end
    else
      begin
        if Length(expanded_refs) = count then
          SetLength(expanded_refs,Length(expanded_refs)+256);
        if FREDB_GuidInArray(uid,expanded_refs)=-1 then
          begin
            expanded_refs[count] := uid;
            inc(count);
          end;
      end;
  end;
begin
  SetLength(expanded_refs,0);
  count := 0;
  for i := 0 to High(ObjectList) do
    FetchChained(ObjectList[i],ref_constraints,0);
  SetLength(expanded_refs,count);
end;

procedure TFRE_DB_CONNECTION.ExpandReferences(ObjectList: TFRE_DB_GUIDArray; ref_constraints : TFRE_DB_NameTypeRLArray ; var expanded_refs: TFRE_DB_ObjectArray);
var i        : NativeInt;
    obj      : TFRE_DB_Object;
    //comparef : TFRE_DB_NameType;
    count    : NativeInt;
    res      : TFRE_DB_Errortype;

  procedure FetchChained(uid:TFRE_DB_GUID ; field_chain : TFRE_DB_NameTypeRLArray ; depth : NativeInt);
  var obrefs   : TFRE_DB_ObjectReferences;
      i,k      : NativeInt;
      scheme   : TFRE_DB_NameType;
      field    : TFRE_DB_NameType;
      outbound : Boolean;
      spos     : NativeInt;

      function  _GuidInObjArray (const check:TFRE_DB_GUID;const arr : TFRE_DB_ObjectArray):NativeInt;
      var  i: NativeInt;
      begin
        result := -1;
        for i:=0 to High(arr) do
          begin
            if not assigned(arr[i]) then
              exit;
            if FREDB_Guids_Same(check,arr[i].UID) then
              exit(i);
          end;
      end;

  begin
    if depth<length(field_chain) then
      begin
        outbound := FREDB_SplitRefLinkDescription(field_chain[depth],field,scheme);
        obrefs   := GetReferencesDetailed(uid,outbound,scheme,field);
        for i := 0 to  high(obrefs) do
          begin
            //comparef := uppercase(obrefs[i].fieldname);
            //if  pos(field_chain[depth],comparef)>0 then
              FetchChained(obrefs[i].linked_uid,field_chain,depth+1);
          end;
      end
    else
      begin
        if Length(expanded_refs) = count then
          SetLength(expanded_refs,Length(expanded_refs)+256);
        if _GuidInObjArray(uid,expanded_refs)=-1 then
          begin
            res := Fetch(uid,expanded_refs[count]);
            if res=edb_ACCESS then
              exit; { skip object with no access rights }
            if res<>edb_OK then
              raise EFRE_DB_Exception.Create(edb_INTERNAL,'FAILED TO FETCH EXPANDED REFERENCED ; CHAINED OBJECT');
            //expanded_refs[count] := uid;
            inc(count);
          end;
      end;
  end;

begin
  SetLength(expanded_refs,0);
  count := 0;
  for i := 0 to High(ObjectList) do
    FetchChained(ObjectList[i],ref_constraints,0);
  SetLength(expanded_refs,count);
end;



procedure TFRE_DB_CONNECTION.ExpandReferences(ObjectList: TFRE_DB_GUIDArray; ref_constraints: TFRE_DB_NameTypeRLArray; var expanded_refs: TFRE_DB_GUIDArray);
//var i        : NativeInt;
//    obj      : TFRE_DB_Object;
//    comparef : TFRE_DB_NameType;
//    count    : NativeInt;
//
//  procedure FetchChained(uid:TFRE_DB_GUID ; field_chain : TFRE_DB_NameTypeRLArray ; depth : NativeInt);
//  var obrefs   : TFRE_DB_ObjectReferences;
//      i,k      : NativeInt;
//      scheme   : TFRE_DB_NameType;
//      field    : TFRE_DB_NameType;
//      outbound : Boolean;
//      spos     : NativeInt;
//  begin
//    if depth<length(field_chain) then
//      begin
//        outbound := FREDB_SplitRefLinkDescription(field_chain[depth],field,scheme);
//        obrefs   := GetReferencesDetailed(uid,outbound,scheme,field);
//        for i := 0 to  high(obrefs) do
//          begin
//            //comparef := uppercase(obrefs[i].fieldname);
//            //if  pos(field_chain[depth],comparef)>0 then
//              FetchChained(obrefs[i].linked_uid,field_chain,depth+1);
//          end;
//      end
//    else
//      begin
//        if Length(expanded_refs) = count then
//          SetLength(expanded_refs,Length(expanded_refs)+256);
//        if FREDB_GuidInArray(uid,expanded_refs)=-1 then
//          begin
//            expanded_refs[count] := uid;
//            inc(count);
//          end;
//      end;
//  end;
  var exrefs : TFRE_DB_ObjectArray;
           i : NativeInt;
begin
 ExpandReferences(ObjectList,ref_constraints,exrefs);
 SetLength(expanded_refs,Length(exrefs));
 for i:=0 to high(exrefs) do
   begin
     expanded_refs[i] := exrefs[i].UID;
     exrefs[i].Finalize;
   end;
  //AcquireBig;
  //try
  //  SetLength(expanded_refs,0);
  //  count := 0;
  //  for i := 0 to High(ObjectList) do
  //    FetchChained(ObjectList[i],ref_constraints,0);
  //  SetLength(expanded_refs,count);
  //finally
  //  ReleaseBig;
  //end;
end;

procedure TFRE_DB_CONNECTION.ExpandReferences(ObjectList: TFRE_DB_GUIDArray; ref_constraints: TFRE_DB_NameTypeRLArray; var expanded_refs: IFRE_DB_ObjectArray);
var exrefs : TFRE_DB_ObjectArray;
         i : NativeInt;
begin
  ExpandReferences(ObjectList,ref_constraints,exrefs);
  SetLength(expanded_refs,length(exrefs));
  for i:=0 to high(exrefs) do
    expanded_refs[i] := exrefs[i];
end;


function TFRE_DB_CONNECTION.CreateDerivedCollection(const collection_name: TFRE_DB_NameType): IFRE_DB_DERIVED_COLLECTION;
var lcollection : TFRE_DB_DERIVED_COLLECTION;
begin
  lcollection := TFRE_DB_DERIVED_COLLECTION.Create(FDBName,collection_name);
  result      := lcollection;
end;


function TFRE_DB_CONNECTION.SYS: IFRE_DB_SYS_CONNECTION;
begin
  result := FSysConnection;
end;

function TFRE_DB_CONNECTION.SYSC: TFRE_DB_SYSTEM_CONNECTION;
begin
  result := FSysConnection;
end;

function TFRE_DB_CONNECTION.GetSysDomainUID: TFRE_DB_GUID;
begin
  result := FSysConnection.GetSysDomainUID;
end;

function TFRE_DB_CONNECTION.GetMyDomainID: TFRE_DB_GUID;
begin
  result := FSysConnection.GetMyDomainID;
end;

function TFRE_DB_CONNECTION.GetUserUID: TFRE_DB_GUID;
begin
  result := FSysConnection.GetUserUID;
end;

function TFRE_DB_CONNECTION.GetUserUIDP: PFRE_DB_GUID;
begin
  result := FSysConnection.GetUserUIDP;
end;

function TFRE_DB_CONNECTION.AddDomain(const domainname: TFRE_DB_NameType; const txt, txt_short: TFRE_DB_String): TFRE_DB_Errortype;
 var domain      : TFRE_DB_DOMAIN;
     domainUID   : TFRE_DB_GUID;
begin
  if Sys.DomainExists(domainname) then
    exit(edb_EXISTS);
  if domainname=CFRE_DB_SYS_DOMAIN_NAME then
    raise EFRE_DB_Exception.Create(edb_ERROR,'it is not allowed to add a domain called SYSTEM');
  domain    := GFRE_DB._NewDomain(FREDB_HCV(domainname),FREDB_HCV(txt),FREDB_HCV(txt_short));
  domain.SetDomainID(domain.UID);
  domainUID := domain.UID;
  result := AdmGetDomainCollection.Store(TFRE_DB_Object(domain));
  GFRE_DB.DBAddDomainInstAllSystemClasses(self,domainUID);
  GFRE_DB.DBAddDomainInstAllExClasses(self,domainUID);
  FSysConnection.ReloadUserAndRights(CFRE_DB_NullGUID);
end;

function TFRE_DB_CONNECTION.GetCurrentUserTokenRef: IFRE_DB_USER_RIGHT_TOKEN;
begin
  result := FSysConnection.GetCurrentUserTokenRef;
end;

procedure TFRE_DB_CONNECTION.DrawScheme(const datastream: TStream; const classfile: string);
begin
   inherited DrawScheme(datastream, classfile);
end;

function TFRE_DB_CONNECTION.GetCurrentUserTokenClone: IFRE_DB_USER_RIGHT_TOKEN;
begin
  result := FSysConnection.GetCurrentUserTokenClone;
end;



{ TFRE_DB }

procedure TFRE_DB.SetLocalZone(const AValue: TFRE_DB_String);
begin
  FLocalZone:=AValue;
end;

function TFRE_DB.NewObjectStreaming(const ClName:ShortString): TFRE_DB_Object;
var ex_obj     : TFRE_DB_ObjectEx;
    obj_cl     : TFRE_DB_OBJECTCLASS;
    ex_obj_cl  : TFRE_DB_OBJECTCLASSEX;
    weako      : TFRE_DB_WeakObjectEx;

begin
  obj_cl := GetObjectClass(ClName);
  if assigned(obj_cl) then begin
    result := obj_cl.CreateStreaming;
  end else begin
    ex_obj_cl := GetObjectClassEx(ClName);
    if assigned(ex_obj_cl) then
      begin
        result := TFRE_DB_NAMED_OBJECT.CreateStreaming(ex_obj_cl);
      end
    else
      begin
        weako := GetWeakObjectClassEx(ClName);
        if not assigned(weako) then
          begin
            if cFRE_DB_ALLOW_WEAKMEDIATORS then
              weako := RegisterWeakObjectExClass(ClName)
            else
              raise EFRE_DB_Exception.Create(edb_ERROR,'STREAMING FAILURE OBJECT CLASS ['+ClName+'] IS UNKNOWN, YOU MAY NEED TO REGISTER IT');
          end;
        result := TFRE_DB_NAMED_OBJECT.CreateStreaming(weako.CloneInstance);
      end;
  end;
end;

procedure TFRE_DB.AcquireWeakMediatorLock;
begin
  FWeakMediatorLock.Acquire;
end;

procedure TFRE_DB.ReleaseWeakMediatorLock;
begin
  FWeakMediatorLock.Release;
end;

function TFRE_DB.NewScheme(const Scheme_Name: TFRE_DB_String; const typ: TFRE_DB_SchemeType): TFRE_DB_SchemeObject;
var ccn             : shortstring;
    base_code_class : TFRE_DB_OBJECTCLASS;
    ex_code_class   : TFRE_DB_OBJECTCLASSEX;
begin
  assert(typ<>dbst_INVALID);
  ccn                          := uppercase(Scheme_Name);
  result                       := TFRE_DB_SchemeObject.Create;
  result.FSchemeClass          := uppercase(ccn);
  result.FStrict               := false;
  result.FSchemeType           := typ;
  base_code_class              := GetObjectClass(ccn);
  if assigned(base_code_class) then begin
    result.FHasHardcodeClass :=true;
    result.FHardCodeClassTyp := base_code_class;
    //writeln('B CLASSTYPE : ', result.FHardCodeClassTyp.ClassName,' ',integer(result));
  end else begin
    ex_code_class := GetObjectClassEx(ccn);
    if assigned(ex_code_class) then begin
      result.FHasHardcodeClass := true;
      result.FHardCodeClassTyp := ex_code_class;
      ccn :=  result.FHardCodeClassTyp.ClassName;
      //writeln('EX CLASSTYPE : ', result.FHardCodeClassTyp.ClassName,' ',integer(result),' ',ex_code_class.ClassName);
    end else begin
      ccn        := 'TFRE_DB_OBJECT';
      result.FHasHardcodeClass := false;
    end;
  end;
end;

procedure TFRE_DB.SafeFinalize(intf: IFRE_DB_BASE);
begin
 abort;
end;

function TFRE_DB.NewDBCommand: IFRE_DB_COMMAND;
begin
  result := TFRE_DB_COMMAND.Create;
end;

function TFRE_DB.FetchApplications(var apps: IFRE_DB_APPLICATION_ARRAY; var loginapp: IFRE_DB_APPLICATION): TFRE_DB_Errortype;
var i     : integer;
    lapps : TFRE_DB_APPLICATION_ARRAY;
begin
  lapps := GetApps;
  setlength(apps,Length(lapps));
  for i := 0 to high(lapps) do begin
    apps[i] := lapps[i];
  end;
end;

function TFRE_DB.NewObjectIntf(const InterfaceSpec: ShortString; out Intf; const mediator: TFRE_DB_ObjectEx;const fail_on_non_existent:boolean=true): Boolean;
var lClass  : TClass;
    lClass2 : TFRE_DB_OBJECTCLASS;
    lObject : TObject;
begin
  result := GetInterfaceClass(InterfaceSpec,lClass);
  if result then begin
    if lClass.InheritsFrom(TFRE_DB_Object) then begin
      lObject := TFRE_DB_OBJECTCLASS(lClass).Create;
    end else begin
      if lClass.InheritsFrom(TFOS_BASE) then
        begin
          lObject := TFOS_BASECLASS(lClass).Create;  // Create a non dbo interfaced object
        end;
    end;
    if not lObject.GetInterface(InterfaceSpec,intf) then begin
      raise EFRE_DB_Exception.Create(edb_ERROR,'internal interface spec/cast fault');
    end;
    if assigned(mediator) then begin
      TFRE_DB_Base(lObject).__SetMediator(mediator);
    end;
  end;
  if not result and fail_on_non_existent then begin
    raise EFRE_DB_Exception.Create(edb_ERROR,'cannot find a interface implementation for %s',[InterfaceSpec]);
  end;
end;

function TFRE_DB.NewObjectI: IFRE_DB_Object;
begin
  result := NewObject;
end;

function TFRE_DB.CreateFromFileI(const filename: TFRE_DB_String): IFRE_DB_Object;
begin
  result := TFRE_DB_Object.CreateFromFile(filename);
end;

function TFRE_DB.CreateFromMemoryI(memory: Pointer): IFRE_DB_Object;
begin
  result := TFRE_DB_Object.CreateFromMemory(memory);
end;

function TFRE_DB.CreateFromStringI(const AValue: TFRE_DB_String): IFRE_DB_Object;
begin
  result := TFRE_DB_Object.CreateFromString(AValue);
end;

function TFRE_DB.ConstructObjectArrayI(const A: array of IFRE_DB_Object): IFRE_DB_ObjectArray;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.ConstructObjectArrayOI(const A: array of TFRE_DB_Object): IFRE_DB_ObjectArray;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.ConstructObjectArrayIO(const A: array of IFRE_DB_Object): TFRE_DB_ObjectArray;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i].Implementor as TFRE_DB_Object;
    inc(j);
  end;
end;

function TFRE_DB.CreateTextI(const translation_key: TFRE_DB_String; const short_text: TFRE_DB_String; const long_text: TFRE_DB_String; const hint_text: TFRE_DB_String): IFRE_DB_TEXT;
begin
 result := TFRE_DB_TEXT.CreateText(translation_key,short_text,long_text,hint_text);
end;

function TFRE_DB.NewConnectionI(const direct: boolean): IFRE_DB_CONNECTION;
begin
  result := NewConnection(direct);
end;

function TFRE_DB.NewSysOnlyConnectionI: IFRE_DB_SYS_CONNECTION;
begin
   result := NewDirectSysConnection;
end;

function TFRE_DB.DatabaseListI(const user: TFRE_DB_String; const pass: TFRE_DB_String): IFOS_STRINGS;
begin
  result := GFRE_DB_PS_LAYER.DatabaseList; // TODO Network, etc
end;

function TFRE_DB._NewObject(const Scheme: TFRE_DB_String; const fail_on_no_cc: boolean): TFRE_DB_Object;
var lScheme  : TFRE_DB_SchemeObject;
    objclass : TFRE_DB_OBJECTCLASS;
    ccn      : shortstring;
begin
  result := nil;
  if Scheme<>'' then begin
    if GetSystemScheme(Scheme,lScheme) then begin
      result := lScheme.ConstructNewInstance(fail_on_no_cc);
    end else begin
      raise EFRE_DB_Exception.Create(edb_ERROR,'NO SCHEME DEFINITION FOR REQUESTED SCHEME <'+Scheme+'> FOUND !');
    end;
  end else begin
    result := NewObject;
  end;
end;

function TFRE_DB._NewText(const key, txt, txt_short: TFRE_DB_String; const hint: TFRE_DB_String): TFRE_DB_TEXT;
begin
 result := TFRE_DB_TEXT.CreateText(key,txt_short,txt,hint);
end;

function TFRE_DB._NewRole(const rolename, txt, txt_short: TFRE_DB_String;const is_internal:Boolean): TFRE_DB_ROLE;
var l_gname:TFRE_DB_String;
begin
  result := _NewObject(TFRE_DB_ROLE.ClassName,true) as TFRE_DB_ROLE;
  l_gname  := uppercase(rolename);
  result.ObjectName  := l_gname;
  result.isInternal  := is_internal;
  result.Description := _NewText('$SYST_ROLE_'+l_gname,txt,txt_short);
end;

function TFRE_DB._NewGroup(const groupname, txt, txt_short: TFRE_DB_String; const is_protected:Boolean;const is_internal:Boolean): TFRE_DB_GROUP;
var l_gname:TFRE_DB_String;
begin
  result := _NewObject(TFRE_DB_GROUP.ClassName,true) as TFRE_DB_GROUP;
  l_gname  := uppercase(groupname);
  result.ObjectName  := l_gname;
  result.isProtected := is_protected;
  Result.isInternal  := is_internal;
  result.Description := _NewText('$SYST_GROUP_'+l_gname,txt,txt_short);
end;

function TFRE_DB._NewDomain(const domainname, txt, txt_short: TFRE_DB_String): TFRE_DB_DOMAIN;
var l_dname:TFRE_DB_String;
begin
  result := _NewObject(TFRE_DB_DOMAIN.ClassName,true) as TFRE_DB_DOMAIN;
  l_dname  := uppercase(domainname);
  result.ObjectName  := l_dname;
  result.isInternal  := false;
  result.Suspended   := false;
  result.Description := _NewText('$SYST_DOMAIN_'+l_dname,txt,txt_short);
end;

function TFRE_DB.NewText(const key, txt, txt_short: TFRE_DB_String; const hint: TFRE_DB_String): IFRE_DB_TEXT;
begin
  result := _NewText(key,txt,txt_short,hint);
end;

function TFRE_DB.NewRole(const rolename, txt, txt_short: TFRE_DB_String;const is_internal:Boolean): IFRE_DB_ROLE;
begin
  result := _NewRole(rolename,txt,txt_short,is_internal);
end;

function TFRE_DB.NewGroup(const groupname, txt, txt_short: TFRE_DB_String; const is_protected:Boolean;const is_internal:Boolean): IFRE_DB_GROUP;
begin
  result := _NewGroup(groupname,txt,txt_short,is_protected,is_internal);
end;

function TFRE_DB.NewEnum(const name: TFRE_DB_String): IFRE_DB_Enum;
begin
 result := NewObject(TFRE_DB_Enum) as TFRE_DB_Enum;
 result.ObjectName:=name;
end;

function TFRE_DB.RegisterSysClientFieldValidator(const val: IFRE_DB_ClientFieldValidator): TFRE_DB_Errortype;
var i     : integer;
    oname : TFRE_DB_String;
begin
  oname := uppercase(val.ObjectName);
  for i:=0 to high(FSysClientFieldValidators) do begin
    if uppercase(FSysClientFieldValidators[i].ObjectName) = oname then exit(edb_EXISTS); // already registerd;
  end;
  setlength(FSysClientFieldValidators,length(FSysClientFieldValidators)+1);
  FSysClientFieldValidators[high(FSysClientFieldValidators)] := val.Implementor as TFRE_DB_ClientFieldValidator;
  //writeln('ADDED CFVALIDATOR : ',oname);
  result := edb_OK;
end;

function TFRE_DB.RegisterSysEnum(const enu: IFRE_DB_Enum): TFRE_DB_Errortype;
var i     : integer;
    oname : TFRE_DB_String;
begin
  oname := uppercase(enu.ObjectName);
  for i:=0 to high(FSysEnums) do begin
    if uppercase(FSysEnums[i].ObjectName) = oname then exit(edb_EXISTS); // already registerd;
  end;
  setlength(FSysEnums,length(FSysEnums)+1);
  FSysEnums[high(FSysEnums)] := enu.Implementor as TFRE_DB_Enum;
  //writeln('ADDED ENUM : ',oname);
  result := edb_OK;
end;

//var Ghack : TFRE_DB_GUID_Access = (Part1 : 0 ; Part2 : 0);

function TFRE_DB.Get_A_Guid: TFRE_DB_GUID;
var uid : TGuid;
begin
  CreateGUID(uid);
  move(uid,result,sizeof(tguid));
end;

function TFRE_DB.Get_A_Guid_HEX: Ansistring;
var x:TFRE_DB_GUID;
begin
  x := Get_A_Guid;
  result := GFRE_BT.Mem2HexStr(PByte(@x),sizeof(TFRE_DB_GUID));
end;

procedure TFRE_DB.AddSystemObjectToSysList(const obj: TFRE_DB_Object);
var i     : integer;
    oguid : TFRE_DB_GUID;
begin
  oguid := obj.UID;
  obj.Set_ReadOnly;
  obj.Set_System;
  for i:=0 to high(FSysObjectList) do begin
    if FREDB_Guids_Same(FSysObjectList[i].SysGuid,oguid) then exit; // already registerd;
  end;
  setlength(FSysObjectList,length(FSysObjectList)+1);
  FSysObjectList[high(FSysObjectList)].SysGuid := oguid;
  FSysObjectList[high(FSysObjectList)].Obj     := obj;
end;

function TFRE_DB.FetchSysObject(const uid: TFRE_DB_GUID; var obj: TFRE_DB_Object): boolean;
var i     : integer;
begin
  for i:=0 to high(FSysObjectList) do begin
    if FREDB_Guids_Same(FSysObjectList[i].SysGuid,uid) then begin
      obj := FSysObjectList[i].Obj;
      if not (fop_READ_ONLY in obj.Properties) then
        raise EFRE_DB_Exception.Create(edb_ERROR,'SYSTEM OBJECTS MUST BE READ ONLY!');
      if not (fop_SYSTEM in obj.Properties) then
        raise EFRE_DB_Exception.Create(edb_ERROR,'SYSTEM OBJECTS MUST BE READ ONLY!');
      obj := obj.CloneToNewObject; // Maybe think about a better strategy (non-finalize) of sysobjects ?
      exit(true);
    end;
  end;
  result := false;
end;

procedure TFRE_DB._IntDBInitializeAllExClasses(const conn: IFRE_DB_CONNECTION; const installforonedomain: boolean; const onedomainUID: TFRE_DB_GUID);
var
  i              : integer;
  newVersion     : TFRE_DB_NameType;
  oldVersion     : TFRE_DB_NameType;
  chkVersion     :  TFRE_DB_NameType;
  version_dbo    : IFRE_DB_Object;
  exclassname    : ShortString;


  procedure _Install4Domain(const domainUID:TFRE_DB_GUID;const newDomVersion:TFRE_DB_NameType);
  var exsikey        : String;
      oldDomVersion  : TFRE_DB_NameType;
  begin
    assert(newDomVersion<>'','logic');
    exsikey := exclassname+'_'+FREDB_G2H(domainUID);
    oldDomversion := version_dbo.Field(exsikey).AsString;
    try
      if domainUID=conn.GetSysDomainUID then begin
        FExClassArray[i].exclass.InstallDBObjects4SysDomain(conn.sys,oldDomVersion,domainUID);
      end else begin
        FExClassArray[i].exclass.InstallDBObjects4Domain(conn.sys,oldDomVersion,domainUID);
      end;
    except on
      e:exception do
        begin
          raise EFRE_DB_Exception.Create(edb_ERROR,'INSTALL FOR DOMAIN FAILED OLDVERSION = [%s] CLASSSINGLETONKEY = [%s] DETAIL : [%s]',[olddomversion,exclassname+'_'+FREDB_G2H(domainUID),e.Message]);
        end;
    end;
    try
      if domainUID=conn.GetSysDomainUID then begin
        FExClassArray[i].exclass.InstallUserDBObjects4SysDomain(conn,oldDomVersion,domainUID);
      end else begin
        FExClassArray[i].exclass.InstallUserDBObjects4Domain(conn,oldDomVersion,domainUID);
      end;
    except on
      e:exception do
        begin
          raise EFRE_DB_Exception.Create(edb_ERROR,'INSTALL USERDB FOR DOMAIN FAILED OLDVERSION = [%s] CLASSSINGLETONKEY = [%s] DETAIL : [%s]',[olddomversion,exclassname+'_'+FREDB_G2H(domainUID),e.Message]);
        end;
    end;
    version_dbo.Field(exsikey).AsString := newDomVersion;
    assert(version_dbo.Field(exsikey).AsString=newDomVersion,'fail');
  end;

  procedure _Install4DomainDBO(const domain:IFRE_DB_DOMAIN);
  begin
    if domain.UID<>conn.GetSysDomainUID then
      _Install4Domain(domain.UID,newVersion);
  end;

begin
  version_dbo := conn.sys.GetClassesVersionDirectory;
  for i:=0 to high(FExClassArray) do begin
    try
      exclassname := FExClassArray[i].exclass.ClassName;
      oldversion  := version_dbo.Field(exclassname).AsString;
      newVersion  := '';
      if not (FExClassArray[i].exclass.InheritsFrom(TFRE_DB_CONTENT_DESC)) then
        begin
          if installforonedomain=false then
            begin
              try
                chkVersion := oldVersion;
                FExClassArray[i].exclass.InstallDBObjects(conn.sys,chkVersion,newVersion);  // The base class sets the version to "UNUSED", so you need to override and set a version<>'' to call Install4Domain and be able to install dbos
                FExClassArray[i].exclass.VersionInstallationCheck(chkVersion,newVersion);
              except on
                e:exception do
                  begin
                    raise EFRE_DB_Exception.Create(edb_ERROR,'INSTALL DBO FAILED OLDVERSION = [%s] CLASSSINGLETONKEY = [%s] DETAIL : [%s]',[oldVersion,exclassname,e.Message]);
                  end;
              end;
              if newVersion='' then
                raise EFRE_DB_Exception.create(edb_ERROR,'The class [%s] for oldversion [%s] is not installable, the new version is unset. That is not allowed',[exclassname,oldVersion,newVersion]);
              if (newVersion='BASE') and (FExClassArray[i].exclass<>TFRE_DB_ObjectEx) then
                abort;
              if newVersion<>'UNUSED' then
                begin
                  FExClassArray[i].exclass.InstallUserDBObjects(conn,oldVersion); { only exclasses go into the app database }
                  _Install4Domain(conn.GetSysDomainUID,newVersion);
                  conn.sys.ForAllDomains(@_Install4DomainDBO);
                end;
              version_dbo.Field(exclassname).AsString := newVersion;
            end
          else
            begin
              if oldversion='' then
                raise EFRE_DB_Exception.create(edb_ERROR,'The class [%s] has no installed version, installation for a domain is not possible!',[exclassname,oldVersion]);
              if oldversion<>'UNUSED' then
                _Install4Domain(onedomainUID,oldVersion);
            end
        end;
    except on e:exception do begin
      if installforonedomain then
        raise
      else
       GFRE_BT.CriticalAbort('DB INITIALIZATION OF EXCLASS SCHEME: [%s] FAILED DUE TO [%s]',[FExClassArray[i].exclass.ClassName,e.Message]);
    end;end;
   end;
   CheckDbResult(conn.sys.StoreClassesVersionDirectory(version_dbo),'internal error on storing classversion directory');
end;

procedure TFRE_DB._IntDBInitializeAllSysClasses(const conn: IFRE_DB_CONNECTION; const installforonedomain: boolean; const onedomainUID: TFRE_DB_GUID);
var
  i              : integer;
  newVersion     : TFRE_DB_NameType;
  oldVersion     : TFRE_DB_NameType;
  version_dbo    : IFRE_DB_Object;
  exclassname    : ShortString;

  procedure _Install4Domain(const domainUID:TFRE_DB_GUID;const newDomVersion:TFRE_DB_NameType);
  var exsikey        : String;
      oldDomVersion  : TFRE_DB_NameType;
  begin
    assert(newDomVersion<>'','logic');
    exsikey := exclassname+'_'+FREDB_G2H(domainUID);
    oldDomversion := version_dbo.Field(exsikey).AsString;
    try
      if domainUID=conn.GetSysDomainUID then begin
        FClassArray[i].sysclass.InstallDBObjects4SysDomain(conn.sys,oldDomVersion,domainUID);
      end else begin
        FClassArray[i].sysclass.InstallDBObjects4Domain(conn.sys,oldDomVersion,domainUID);
      end;
    except on
      e:exception do
        begin
          raise EFRE_DB_Exception.Create(edb_ERROR,'INSTALL FOR DOMAIN FAILED OLDVERSION = [%s] CLASSSINGLETONKEY = [%s] DETAIL : [%s]',[olddomversion,exclassname+'_'+FREDB_G2H(domainUID),e.Message]);
        end;
    end;
    try
      if domainUID=conn.GetSysDomainUID then begin
        FClassArray[i].sysclass.InstallUserDBObjects4SysDomain(conn,oldDomVersion,domainUID);
      end else begin
        FClassArray[i].sysclass.InstallUserDBObjects4Domain(conn,oldDomVersion,domainUID);
      end;
    except on
      e:exception do
        begin
          raise EFRE_DB_Exception.Create(edb_ERROR,'INSTALL USERDB FOR DOMAIN FAILED OLDVERSION = [%s] CLASSSINGLETONKEY = [%s] DETAIL : [%s]',[olddomversion,exclassname+'_'+FREDB_G2H(domainUID),e.Message]);
        end;
    end;
    version_dbo.Field(exsikey).AsString := newDomVersion;
    assert(version_dbo.Field(exsikey).AsString=newDomVersion,'fail');
  end;

  procedure _Install4DomainDBO(const domain:IFRE_DB_DOMAIN);
  begin
    if domain.UID<>conn.GetSysDomainUID then
      _Install4Domain(domain.UID,newVersion);
  end;

begin
  version_dbo := conn.sys.GetClassesVersionDirectory;
  for i:=0 to high(FClassArray) do begin
    try
      exclassname := FClassArray[i].sysclass.ClassName;
      oldversion  := version_dbo.Field(exclassname).AsString;
      newVersion  := '';
      if not (FClassArray[i].sysclass.InheritsFrom(TFRE_DB_CONTENT_DESC)) then
        begin
          if installforonedomain=false then
            begin
              try
                FClassArray[i].sysclass.InstallDBObjects(conn.sys,oldVersion,newVersion);  // The base class sets the version to "UNUSED", so you need to override and set a version<>'' to call Install4Domain and be able to install dbos
              except on
                e:exception do
                  begin
                    raise EFRE_DB_Exception.Create(edb_ERROR,'INSTALL DBO FAILED OLDVERSION = [%s] CLASSSINGLETONKEY = [%s] DETAIL : [%s]',[oldVersion,exclassname,e.Message]);
                  end;
              end;
              if newVersion='' then
                raise EFRE_DB_Exception.create(edb_ERROR,'The class [%s] for oldversion [%s] is not installable, the new version is unset. That is not allowed',[exclassname,oldVersion,newVersion]);
              if (newVersion='BASE') and (FClassArray[i].sysclass<>TFRE_DB_Object) then
                abort;
              if newVersion<>'UNUSED' then
                begin
                  _Install4Domain(conn.GetSysDomainUID,newVersion);
                  conn.sys.ForAllDomains(@_Install4DomainDBO);
                end;
              version_dbo.Field(exclassname).AsString := newVersion;
            end
          else
            begin
              if oldversion='' then
                raise EFRE_DB_Exception.create(edb_ERROR,'The class [%s] has no installed version, installation for a domain is not possible!',[exclassname,oldVersion]);
              if oldversion<>'UNUSED' then
                _Install4Domain(onedomainUID,oldVersion);
            end
        end;
    except on e:exception do begin
      if installforonedomain then
        raise
      else
       GFRE_BT.CriticalAbort('DB INITIALIZATION OF sysCLASS SCHEME: [%s] FAILED DUE TO [%s]',[FClassArray[i].sysclass.ClassName,e.Message]);
    end;end;
   end;
   CheckDbResult(conn.sys.StoreClassesVersionDirectory(version_dbo),'internal error on storing classversion directory');
end;

procedure TFRE_DB.DBAddDomainInstAllExClasses(const conn: IFRE_DB_CONNECTION; const domainUID: TFRE_DB_GUID);
begin
  _IntDBInitializeAllExClasses(conn,true,domainUID);
end;

procedure TFRE_DB.DBAddDomainInstAllSystemClasses(const conn: IFRE_DB_CONNECTION; const domainUID: TFRE_DB_GUID);
begin
  _IntDBInitializeAllSysClasses(conn,true,domainUID);
end;


procedure TFRE_DB.Initialize_Extension_Objects; // called multiple times - once for each extension unit
var i       : integer;
    app     : TFRE_DB_APPLICATION;
    cn      : TFRE_DB_String;
    lscheme : TFRE_DB_SchemeObject;

begin
  for i:=0 to high(FExClassArray) do begin
    if  (not FExClassArray[i].initialized) then begin
      try
        FExClassArray[i].initialized := true;
        cn      := FExClassArray[i].exclass.ClassName;
        lscheme := NewScheme(cn,dbst_Extension);
        FExClassArray[i].exclass.RegisterSystemScheme(lscheme);
        RegisterSysScheme(lscheme);
        //CheckDbResultFmt(FSystemSchemes.StoreScheme(lscheme),'error storing extension lscheme %s',[cn]);
        //FSystemSchemes.GetScheme(cn,lscheme);
        if FExClassArray[i].exclass.InheritsFrom(TFRE_DB_APPLICATION) and (FExClassArray[i].exclass<>TFRE_DB_APPLICATION)  then begin
          app := lscheme.ConstructNewInstance.Implementor_HC as TFRE_DB_APPLICATION;
          SetLength(FAppArray,Length(FAppArray)+1);
          FAppArray[high(FAppArray)]:=app;
          AddSystemObjectToSysList(app.Implementor as TFRE_DB_Object);
        end;
      except on e:exception do begin
        GFRE_BT.CriticalAbort('INITIALIZATION OF EXCLASS: [%s] FAILED DUE TO [%s]',[FExClassArray[i].exclass.ClassName,e.Message]);
      end;end;
    end;
  end;
end;

procedure TFRE_DB.Finalize_Extension_Objects;
begin

end;

{
  this function converts a (convertable) JSON DBO Representation to a DBO
  some limitations apply
  input must be a JSON Object (not array, not null)
  arrays in the object must be uniform
  the number type is guessed from the values of the array -> int32/64,qword,float
  special fields are uid, uidpath

}

function TFRE_DB.JSONObject2Object(const json_string: string): IFRE_DB_Object; //TODO: Handle DomainID ?
var  l_JSONParser : TJSONParser;
     l_JSONObject : TJSONObject;
     l_DataObj    : IFRE_DB_Object;
     jd           : TJSONData;

   function JSON2GUidArray(const req: string): TFRE_DB_GUIDArray;
   var ljp : TJSONParser;
          ljd : TJSONArray;
          lJO : TJSONString;
          i: Integer;
   begin
     result:=nil;
     ljp:=TJSONParser.Create(req);
     try
       ljd:= TJSONArray(ljp.Parse);
       if ljd is TJSONArray and (ljd.Count>=1) and (ljd.Types[0]=jtString) then begin
         SetLength(result,ljd.Count);
         for i := 0 to ljd.Count-1 do begin
           ljo := ljd.Items[i] as TJSONString;
           result[i] := FREDB_H2G(ljo.AsString);
         end;
       end else begin
         SetLength(result,0);
       end;
       ljd.Free;
     finally
       ljp.Free;
     end;
   end;

   type
     tuniform_array_type=(j2o_unspec,j2o_empty_arr,j2o_Bool,j2o_String,j2o_Obj,j2o_R64,j2o_Int32,j2o_Int64,j2o_QWord);
   const
     cuniform_array_type:array [tuniform_array_type] of string=('UNSPECIFIED','EMPTY','BOOLEAN','STRING','OBJECT','REAL','INTEGER32','INTEGER64','QWORD');


  procedure _ConvertObject(const l_JSONObject : TJSONObject ; const l_DataObj : IFRE_DB_Object);
  var i,j           : integer;
      l_JSONItem    : TJSONData;
      l_SubDataObj  : IFRE_DB_Object;
      l_GotUid      : Boolean;
      l_FieldName   : String;
      l_array_t     : tuniform_array_type;

      function _GetUniformJsonArrayType : tuniform_array_type;
      var j         : integer;
          jnt       : TJSONNumberType;
          neg_val   : boolean;
          arr_item  : TJSONData;
      begin
        result := j2o_unspec;
        if l_JSONItem.Count=0 then
          exit(j2o_empty_arr);
        neg_val := false; { no negative values till now }
        for j := 0 to l_JSONItem.Count - 1 do begin
          arr_item := l_JSONItem.Items[j];
          if arr_item is TJSONString then
            case Result of
              j2o_unspec: result := j2o_String;
              j2o_String: ; { no change, ok }
              else EFRE_DB_Exception.Create(edb_ERROR,'unexpected JSON Element in array %s itemclass %s the array should be of uniform type %s',[l_FieldName,arr_item.ClassName,cuniform_array_type[result]]);
            end
          else
          if arr_item is TJSONBoolean then
            case Result of
              j2o_unspec: result := j2o_Bool;
              j2o_Bool: ; { no change, ok }
              else EFRE_DB_Exception.Create(edb_ERROR,'unexpected JSON Element in array %s itemclass %s the array should be of uniform type %s',[l_FieldName,arr_item.ClassName,cuniform_array_type[result]]);
            end
          else
          if arr_item is TJSONNumber then
            begin
              jnt := TJSONNumber(arr_item).NumberType;
              case Result of
                j2o_unspec:
                  case jnt of
                    ntFloat:   result := j2o_R64;
                    ntInteger:
                      begin
                        result  := j2o_Int32;
                        neg_val := (neg_val or (TJSONNumber(arr_item).AsInteger<0));
                      end;
                    ntInt64:
                      begin
                        result := j2o_Int64;
                        neg_val := (neg_val or (TJSONNumber(arr_item).AsInt64<0));
                      end;
                    ntQWord:   result := j2o_QWord;
                    else EFRE_DB_Exception.Create(edb_INTERNAL,'unexpected JSON number subtype %d',[ord(jnt)]);
                  end;
                j2o_R64:   ; { stay on R64}
                j2o_Int32:
                  case jnt of
                    ntFloat:   result := j2o_R64  ; { upgrade to R64 }
                    ntInteger:
                      begin  { stay on R32 }
                        neg_val := (neg_val or (TJSONNumber(arr_item).AsInteger<0));
                      end;
                    ntInt64:
                      begin
                        result  := j2o_Int64; { upgrade to I64 }
                        neg_val := (neg_val or (TJSONNumber(arr_item).AsInt64<0));
                      end;
                    ntQWord:
                      begin
                        if neg_val then { the current qword is positive, check if all prior values are positive too}
                          EFRE_DB_Exception.Create(edb_INTERNAL,'cannot upgrade a uniform JSON array type from i32 to qword, because there are negative values in the array prior to this large qword number');
                        result := j2o_QWord;        { upgrade to qword }
                      end
                    else EFRE_DB_Exception.Create(edb_INTERNAL,'unexpected JSON number subtype %d',[ord(jnt)]);
                  end;
                j2o_Int64:
                  case jnt of
                    ntFloat:   result := j2o_R64  ; { upgrade to R64 }
                    ntInteger:
                      begin  { stay on I64 }
                        neg_val := (neg_val or (TJSONNumber(arr_item).AsInteger<0));
                      end;
                    ntInt64:
                      begin { stay on I64 }
                        neg_val := (neg_val or (TJSONNumber(arr_item).AsInt64<0));
                      end;
                    ntQWord:
                      begin
                        if neg_val then { the current qword is positive, check if all prior values are positive too}
                          EFRE_DB_Exception.Create(edb_INTERNAL,'cannot upgrade a uniform JSON array type from i64 to qword, because there are negative values in the array prior to this large qword number');
                        result := j2o_QWord;        { upgrade to qword }
                      end
                    else EFRE_DB_Exception.Create(edb_INTERNAL,'unexpected JSON number subtype %d',[ord(jnt)]);
                  end;
                j2o_QWord:
                  case jnt of
                    ntFloat:   result := j2o_R64  ; { upgrade to R64 }
                    ntInteger:
                      begin  { try stay on QWord }
                        neg_val := (neg_val or (TJSONNumber(arr_item).AsInteger<0));
                        if neg_val then
                          EFRE_DB_Exception.Create(edb_INTERNAL,'cannot handle a uniform JSON array type qword, because there is a negative i32 value in the array after qword number(s)');
                      end;
                    ntInt64:
                      begin { try stay on QWord }
                        neg_val := (neg_val or (TJSONNumber(arr_item).AsInt64<0));
                        if neg_val then
                          EFRE_DB_Exception.Create(edb_INTERNAL,'cannot handle a uniform JSON array type qword, because there is a negative i32 value in the array after qword number(s)');
                      end;
                    ntQWord: ; { stay on qword }
                    else EFRE_DB_Exception.Create(edb_INTERNAL,'unexpected JSON number subtype %d',[ord(jnt)]);
                  end;
                else EFRE_DB_Exception.Create(edb_ERROR,'unexpected JSON Element in array %s itemclass %s the array should be of uniform type %s',[l_FieldName,arr_item.ClassName,cuniform_array_type[result]]);
              end;
            end
          else
          if arr_item is TJSONObject then
            case Result of
              j2o_unspec: result := j2o_Obj;
              j2o_Obj: ; { no change, ok }
              else EFRE_DB_Exception.Create(edb_ERROR,'unexpected JSON Element in array %s itemclass %s the array should be of uniform type %s',[l_FieldName,arr_item.ClassName,cuniform_array_type[result]]);
            end
          else
            raise EFRE_DB_Exception.Create(edb_ERROR,'unexpected JSON Element in array %s / %s',[l_FieldName,arr_item.ClassName]);
        end;
      end;

  begin
    for i:=0 to l_JSONObject.Count-1 do begin
      l_JSONItem  := l_JSONObject.Items[i];
      l_FieldName := l_JSONObject.Names[i];
      if l_JSONItem is TJSONObject then begin
        l_SubDataObj := GFRE_DBI.NewObject;
        l_DataObj.Field(l_FieldName).AsObject := l_SubDataObj;
        _ConvertObject(TJSONObject(l_JSONItem),l_SubDataObj);
      end else begin
        if lowercase(l_FieldName)='uid' then begin
          l_DataObj.Field('uid').AsGUID := FREDB_H2G(l_JSONItem.AsString);
          l_GotUid := true;
        end else begin
          if lowercase(l_FieldName)='uidpath' then begin
            l_DataObj.Field('uidpath').AsGUIDArr := JSON2GUidArray(l_JSONItem.AsJSON);
          end else begin
            if l_JSONItem is TJSONArray then begin
               l_array_t := _GetUniformJsonArrayType;
               for j := 0 to l_JSONItem.Count-1 do
                 case l_array_t of
                   j2o_unspec: ;
                   j2o_empty_arr: l_DataObj.Field(l_FieldName).SetAsEmptyStringArray;
                   j2o_Bool:      l_DataObj.Field(l_FieldName).AddBoolean(l_JSONItem.Items[j].AsBoolean);
                   j2o_String:    l_DataObj.Field(l_FieldName).AddString(l_JSONItem.Items[j].AsString);
                   j2o_Obj:       begin
                                    l_SubDataObj := GFRE_DBI.NewObject;
                                    l_DataObj.Field(l_FieldName).AddObject(l_SubDataObj);
                                    _ConvertObject(TJSONObject(l_JSONItem.Items[j]),l_SubDataObj);
                                  end;
                   j2o_R64:       l_DataObj.Field(l_FieldName).AddReal64(l_JSONItem.Items[j].AsFloat);
                   j2o_Int32:     l_DataObj.Field(l_FieldName).AddInt32 (l_JSONItem.Items[j].AsInteger);
                   j2o_Int64:     l_DataObj.Field(l_FieldName).AddInt64 (l_JSONItem.Items[j].AsInt64);
                   j2o_QWord:     l_DataObj.Field(l_FieldName).AddUInt64(l_JSONItem.Items[j].AsQWord);
                 end;
              //if l_JSONItem.Count=0 then begin
              //   l_DataObj.Field(l_FieldName).SetAsEmptyStringArray;
              //end else
              //  begin
              //    for j := 0 to l_JSONItem.Count - 1 do
              //      begin
              //        if l_JSONItem.Items[j] is TJSONString then
              //          begin
              //            l_DataObj.Field(l_FieldName).AddString(l_JSONItem.Items[j].AsString);
              //          end
              //        else
              //        if l_JSONItem.Items[j] is TJSONNumber then
              //          begin
              //            case TJSONNumber(l_JSONItem.Items[j]).NumberType of
              //              ntFloat:    l_DataObj.Field(l_FieldName).AddReal64(l_JSONItem.Items[j].AsFloat);
              //              ntInteger:  l_DataObj.Field(l_FieldName).AddInt32(l_JSONItem.Items[j].AsInteger);
              //              ntInt64:    l_DataObj.Field(l_FieldName).AddInt64(l_JSONItem.Items[j].AsInt64);
              //              ntQWord:    l_DataObj.Field(l_FieldName).AddUInt64(l_JSONItem.Items[j].AsQWord);
              //            end;
              //          end
              //        else
              //          if l_JSONItem.Items[j] is TJSONObject then
              //            begin
              //              l_SubDataObj := GFRE_DBI.NewObject;
              //              l_DataObj.Field(l_FieldName).AddObject(l_SubDataObj);
              //              _ConvertObject(TJSONObject(l_JSONItem.Items[j]),l_SubDataObj);
              //            end
              //          else
              //            raise EFRE_DB_Exception.Create(edb_ERROR,'unexpected JSON Element in array %s / %s',[l_FieldName,l_JSONItem.ClassName]);
              //      end;
              //end;
            end else
            if l_JSONItem is TJSONNull then
              begin
                l_DataObj.Field(l_FieldName).AsString := cFRE_DB_SYS_CLEAR_VAL_STR;  { A JSON Null Field is defined here to issue a CLEAR on the original Field }
              end
            else
              begin
                l_DataObj.Field(l_FieldName).AsString := l_JSONItem.AsString;
              end;
          end;
        end;
      end;
    end;
  end;

begin
  result := nil;
  l_JSONParser := TJSONParser.Create(json_string,true);
  try
    jd           := nil;
    jd           := l_JSONParser.Parse;
    if jd is TJSONObject then begin
      l_JSONObject := jd as TJSONObject;
      l_DataObj    := GFRE_DBI.NewObject;
      result       := l_DataObj;
      if assigned(l_JSONObject) then begin
        _ConvertObject(l_JSONObject,l_DataObj);
      end else begin
        l_DataObj.Finalize;
      end;
    end else begin
      if Assigned(jd) then begin
        raise EFRE_DB_Exception.Create(edb_ERROR,'UNEXPECTED JSON PARSER FIELDTYPE : %s',[jd.ClassName]);
      end else begin
        raise EFRE_DB_Exception.Create(edb_ERROR,'UNEXPECTED JSON PARSER FIELDTYPE');
      end;
    end;
  finally
    if assigned(jd) then
      jd.free;
    l_JSONParser.free;
  end;
end;

function TFRE_DB.DefaultDirectory: TFRE_DB_String;
begin
  result:=SetDirSeparators(GFRE_BT.GetUserDir+'.fre/db/');
end;

function TFRE_DB.NetServ: IFRE_DB_NetServer;
begin
  result := FNetServer;
end;

function TFRE_DB.GetFormatSettings: TFormatSettings;
begin
  result := FFormatSettings;
end;

function TFRE_DB.GetLocalZone: TFRE_DB_String;
begin
  result := FLocalZone;
end;

procedure TFRE_DB.SetFormatSettings(const AValue: TFormatSettings);
begin
  FFormatSettings := AValue;
end;

constructor TFRE_DB.Create;
begin
  FFormatSettings := DefaultFormatSettings;
  GFRE_TF.Get_Lock(FWeakMediatorLock);
end;

destructor TFRE_DB.Destroy;
var i : NativeInt;
begin
  for i:=0 to High(FSysSchemes) do
    FSysSchemes[i].free;
  for i:=0 to High(FSysEnums) do
    FSysEnums[i].free;
  for i:=0 to High(FSysClientFieldValidators) do
    FSysClientFieldValidators[i].free;
  for i:=0 to High(FSysObjectList) do
    FSysObjectList[i].obj.free;
  for i:=0 to High(FWeakExClassArray) do
    FWeakExClassArray[i].Destroy;
  FWeakMediatorLock.Finalize;
  inherited Destroy;
end;

function TFRE_DB.GetApps: TFRE_DB_APPLICATION_ARRAY;
begin
  result := FAppArray;
end;

function TFRE_DB.GetAppInstanceByClass(appclass: TClass; out app: TFRE_DB_APPLICATION): boolean;
var i: Integer;
begin
  for i:=0 to high(FAppArray) do begin
    if FAppArray[i].ClassType=appclass then begin
      app := FAppArray[i];
      exit(true);
    end;
  end;
  result := false;
end;

function TFRE_DB.GetSysEnum(name: TFRE_DB_NameType; out enum: TFRE_DB_Enum): boolean;
var i: Integer;
begin
  name := uppercase(name);
  for i:=0 to high(FSysEnums) do begin
    if uppercase(FSysEnums[i].ObjectName)=name then begin
      enum := FSysEnums[i];
      exit(true);
    end;
  end;
  result := false;
end;

function TFRE_DB.GetSysClientFieldValidator(name: TFRE_DB_NameType; out clf: TFRE_DB_ClientFieldValidator): boolean;
var i: Integer;
begin
  name := uppercase(name);
  for i:=0 to high(FSysClientFieldValidators) do begin
    if uppercase(FSysClientFieldValidators[i].ObjectName)=name then begin
      clf := FSysClientFieldValidators[i];
      exit(true);
    end;
  end;
  result := false;
end;

function TFRE_DB.GetSysScheme(name: TFRE_DB_NameType; out scheme: TFRE_DB_SchemeObject): boolean;
var i: Integer;
begin
  name   := uppercase(name);
  scheme := nil;
  for i:=0 to high(FSysSchemes) do begin
    if (FSysSchemes[i].DefinedSchemeName)=name then begin
      scheme := FSysSchemes[i];
      exit(true);
    end;
  end;
  result := false;
end;



function TFRE_DB.LocalTimeToUTCDB64(const ADateTime64: TFRE_DB_DateTime64): TFRE_DB_DateTime64;
begin
  result := GFRE_DT.LocalTimeToUTC(ADateTime64,FLocalZone);
end;

function TFRE_DB.UTCToLocalTimeDB64(const ADateTime64: TFRE_DB_DateTime64 ): TFRE_DB_DateTime64;
begin
  result:=GFRE_DT.UTCToLocalTime(ADateTime64,FLocalZone);
end;

function TFRE_DB.NewObject(const ClName: TFRE_DB_String): TFRE_DB_Object;
var oc : TFRE_DB_OBJECTCLASS;
      begin
  oc := GetObjectClass(ClName);
  if not assigned(oc) then raise EFRE_DB_Exception.Create(edb_ERROR,'THE CODECLASS [%s] IS NOT REGISTERED',[ClName]);
  result := oc.Create;
end;

procedure TFRE_DB.RegisterObjectClass(const ObClass: TFRE_DB_OBJECTCLASS);
var i:integer;
    cn : shortstring;
begin
  cn := uppercase(ObClass.ClassName);
  for i:=0 to high(FClassArray) do begin
    if uppercase(FClassArray[i].sysclass.ClassName)=cn then
      exit; // already registerd;
  end;
  setlength(FClassArray,length(FClassArray)+1);
  FClassArray[high(FClassArray)].sysclass    :=ObClass;
  FClassArray[high(FClassArray)].initialized :=false;
end;

procedure TFRE_DB.RegisterObjectClassEx(const ObClass: TFRE_DB_OBJECTCLASSEX);
var i:integer;
begin
  for i:=0 to high(FExClassArray) do begin
    if uppercase(FExClassArray[i].exclass.ClassName)=uppercase(ObClass.ClassName) then exit; // already registerd;
  end;
  setlength(FExClassArray,length(FExClassArray)+1);
  FExClassArray[high(FExClassArray)].exclass     := ObClass;
  FExClassArray[high(FExClassArray)].initialized := false;
end;

function TFRE_DB.RegisterWeakObjectExClass(const clname: ShortString): TFRE_DB_WeakObjectEx;
var
  i: NativeInt;
  upclname : ShortString;
begin
  upclname := UpperCase(clname);
  AcquireWeakMediatorLock;
  try
    for i:=0 to high(FWeakExClassArray) do
      begin
        if uppercase(FWeakExClassArray[i].SchemeClass)=upclname then
          raise EFRE_DB_Exception.Create(edb_INTERNAL,'double RegisterWeakObjectclass '+clname);
      end;
    setlength(FWeakExClassArray,length(FWeakExClassArray)+1);
    GFRE_DB.LogDebug(dblc_STREAMING,'Registering Weak Scheme [%s], no code methods availlable.',[clname]);
    result := TFRE_DB_WeakObjectEx.Create(clname);
    FWeakExClassArray[high(FWeakExClassArray)] := result;
  finally
    ReleaseWeakMediatorLock;
  end;
end;

procedure TFRE_DB.RegisterPrimaryImplementor(const ObClass: TClass; const InterfaceSpec: ShortString);
var i:integer;
begin
  if not Sysutils.Supports(ObClass,InterfaceSpec) then GFRE_BT.CriticalAbort('try of invalid implementor registration class=%s interface=%s',[ObClass.ClassName,InterfaceSpec]);
  for i:=0 to high(FKnownInterfaces) do begin
    if uppercase(FKnownInterfaces[i].InterfaceSpec)=uppercase(InterfaceSpec) then begin
      GFRE_BT.CriticalAbort('double register try of interface %s',[InterfaceSpec]);
    end;
  end;
  setlength(FKnownInterfaces,length(FKnownInterfaces)+1);
  FKnownInterfaces[high(FKnownInterfaces)].InterfaceSpec     := uppercase(InterfaceSpec);
  FKnownInterfaces[high(FKnownInterfaces)].ImplementingClass := ObClass;
end;

function TFRE_DB.GetInterfaceClass(const InterfaceSpec: ShortString; out ObClass: TClass): boolean;
var i   : integer;
   isp  : ShortString;
begin
  result := false;
  isp := UpperCase(InterfaceSpec);
  for i:=0 to high(FKnownInterfaces) do begin
    if FKnownInterfaces[i].InterfaceSpec=isp then begin
      ObClass := FKnownInterfaces[i].ImplementingClass;
      if not Sysutils.Supports(ObClass,InterfaceSpec) then begin
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'found an implementor, but cannot retrieve the implemented interface?');
      end;
      exit(true);
    end;
  end;
end;

procedure TFRE_DB.GenerateAnObjChangeList(const first_obj, second_obj: IFRE_DB_Object; const InsertCB, DeleteCB: IFRE_DB_Obj_Iterator; const UpdateCB: IFRE_DB_UpdateChange_Iterator);
begin
  TFRE_DB_Object.GenerateAnObjChangeList(first_obj.Implementor as TFRE_DB_Object,second_obj.Implementor as TFRE_DB_Object,InsertCB,DeleteCB,UpdateCB);
end;


procedure TFRE_DB.DBInitializeAllExClasses(const conn: IFRE_DB_CONNECTION);
begin
  _IntDBInitializeAllExClasses(conn,false,CFRE_DB_NullGUID);
end;

procedure TFRE_DB.DBInitializeAllSystemClasses(const conn: IFRE_DB_CONNECTION);
begin
  _IntDBInitializeAllSysClasses(conn,false,CFRE_DB_NullGUID);
end;

procedure TFRE_DB.Initialize_System_Objects;
var i       : integer;
    cn      : string;
    lscheme : TFRE_DB_SchemeObject;
begin
  for i:=0 to high(FClassArray) do begin
    try
      cn      := FClassArray[i].sysclass.ClassName;
      lscheme := NewScheme(cn,dbst_System);
      if cn='TFRE_DB_USER' then
        cn := cn;
      FClassArray[i].sysclass.RegisterSystemScheme(lscheme);
      //AddSystemObjectToSysList(lscheme);
      //writeln('>>>>>  SCHEME STORE ',cn);
      //CheckDbResultFmt(FSystemSchemes.StoreScheme(lscheme),'error storing extension lscheme %s',[cn]);
      CheckDbResultFmt(RegisterSysScheme(lscheme),'error adding sysscheme lscheme %s',[cn]);
      //writeln('<<<<< SCHEME STORE ',cn,' DONE');
    except on e:exception do begin
      GFRE_BT.CriticalAbort('INITIALIZATION OF SYSCLASS: [%s] FAILED DUE TO [%s]',[FClassArray[i].sysclass.ClassName,e.Message]);
    end;end;
  end;
end;

function TFRE_DB.NewObject(const ObjClass: TFRE_DB_OBJECTCLASS): TFRE_DB_Object;
begin
  result := ObjClass.Create;
end;

function TFRE_DB.NewObject(const ObjClass: TFRE_DB_OBJECTCLASSEX): TFRE_DB_Object;
var x : TFRE_DB_ObjectEx;
begin
  x := ObjClass.create;
  result := x.Implementor as TFRE_DB_Object;
end;

function TFRE_DB.NewObject: TFRE_DB_Object;
begin
  result := TFRE_DB_Object.Create;
end;

function TFRE_DB.NewNamedObject: TFRE_DB_NAMED_OBJECT;
begin
  result := TFRE_DB_NAMED_OBJECT.Create;
end;

function TFRE_DB.NewNamedObjectI: IFRE_DB_NAMED_OBJECT;
begin
  result := NewNamedObject;
end;

function TFRE_DB.NewObjectScheme(const Scheme: TClass): IFRE_DB_Object;
begin
  result := NewObjectSchemeByName(scheme.ClassName);
end;

function TFRE_DB.NewObjectSchemeByName(const Scheme: TFRE_DB_NameType): IFRE_DB_Object;
var schemeo : TFRE_DB_SchemeObject;
begin
  if not GetSystemScheme(Scheme,schemeo) then
    raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'could not construct new object, scheme [%s] not found',[scheme]);
  result := schemeo.ConstructNewInstance();
end;

function TFRE_DB.NewClientFieldValidator(const name: TFRE_DB_String): TFRE_DB_ClientFieldValidator;
begin
 result := NewObject(TFRE_DB_ClientFieldValidator) as TFRE_DB_ClientFieldValidator;
 result.ObjectName:=name;
end;

function TFRE_DB.NewClientFieldValidatorI(const name: TFRE_DB_String): IFRE_DB_ClientFieldValidator;
begin
  result := NewClientFieldValidator(name);
end;

function TFRE_DB.GetObjectClass(const ClName: ShortString): TFRE_DB_OBJECTCLASS;
var i:integer;
    ccn:ShortString;
begin
 result := nil;
 ccn := UpperCase(ClName);
 for i:=0 to high(FClassArray) do begin
   if uppercase(FClassArray[i].sysclass.ClassName)=ccn then begin
     result := FClassArray[i].sysclass;
     exit;
   end;
 end;
end;

function TFRE_DB.GetObjectClassEx(const ClName: ShortString): TFRE_DB_OBJECTCLASSEX;
var i:integer;
    ccn:ShortString;
    acn:ShortString;
begin
  result := nil;
  ccn := UpperCase(ClName);
  for i:=0 to high(FExClassArray) do begin
    acn := uppercase(FExClassArray[i].exclass.ClassName);
    if acn=ccn then begin
      result := FExClassArray[i].exclass;
      exit;
    end;
  end;
end;

function TFRE_DB.GetWeakObjectClassEx(const ClName: ShortString): TFRE_DB_WeakObjectEx;
var i:integer;
    ccn:ShortString;
    acn:ShortString;
begin
  result := nil;
  AcquireWeakMediatorLock;
  try
    ccn := UpperCase(ClName);
    for i:=0 to high(FWeakExClassArray) do begin
      acn := uppercase(FWeakExClassArray[i].SchemeClass);
      if acn=ccn then begin
        result := FWeakExClassArray[i];
        exit;
      end;
    end;
  finally
    ReleaseWeakMediatorLock;
  end;
end;

function TFRE_DB.ExistsObjectClass(const ClName: ShortString): boolean;
var i:integer;
begin
 for i:=0 to high(FClassArray) do begin
   if uppercase(FClassArray[i].sysclass.ClassName)=uppercase(ClName) then begin
     result := true;
     exit;
   end;
 end;
 result:=false;
end;

function TFRE_DB.ExistsObjectClassEx(const ClName: ShortString): boolean;
var i:integer;
begin
 for i:=0 to high(FExClassArray) do begin
   if uppercase(FExClassArray[i].exclass.ClassName)=uppercase(ClName) then begin
     exit(true);
   end;
 end;
 result:=false;
end;

function TFRE_DB.NewConnection(const direct: boolean): TFRE_DB_CONNECTION;
begin
  GFRE_DB_Init_Check;
  result := TFRE_DB_CONNECTION.Create(false);
end;

function TFRE_DB.NewDirectSysConnection: TFRE_DB_SYSTEM_CONNECTION;
begin
 GFRE_DB_Init_Check;
 result := TFRE_DB_SYSTEM_CONNECTION.Create(false);
end;


function TFRE_DB.StringArray2String(const A: TFRE_DB_StringArray): TFRE_DB_String;
var i,len:integer;
begin
  result:='[ ';
  len := Length(a)-1;
  for i := 0 to len do begin
    if i<len then result := result+''''+a[i]+''',' else result := result+''''+a[i]+''' ';
  end;
  result:=result+']';
end;

function TFRE_DB.StringArray2GuidArray(const A: TFRE_DB_StringArray): TFRE_DB_GUIDArray;
var i: Integer;
begin
  SetLength(result,length(a));
  for i:=0 to high(a) do begin
    result[i] := FREDB_H2G(a[i]);
  end;
end;

function TFRE_DB.StringArray2ByteArray(const A: TFRE_DB_StringArray): TFRE_DB_ByteArray;
var i: Integer;
begin
  SetLength(result,length(a));
  for i:=0 to high(a) do begin
    result[i] := StrToInt(A[i]);
  end;
end;

function TFRE_DB.StringArray2Int16Array(const A: TFRE_DB_StringArray): TFRE_DB_Int16Array;
var i: Integer;
begin
  SetLength(result,length(a));
  for i:=0 to high(a) do begin
    result[i] := StrToInt(A[i]);
  end;
end;

function TFRE_DB.StringArray2UInt16Array(const A: TFRE_DB_StringArray): TFRE_DB_UInt16Array;
var i: Integer;
begin
  SetLength(result,length(a));
  for i:=0 to high(a) do begin
    result[i] := StrToInt(A[i]);
  end;
end;

function TFRE_DB.StringArray2Int32Array(const A: TFRE_DB_StringArray): TFRE_DB_Int32Array;
var i: Integer;
begin
  SetLength(result,length(a));
  for i:=0 to high(a) do begin
    result[i] := StrToInt(A[i]);
  end;
end;

function TFRE_DB.StringArray2UInt32Array(const A: TFRE_DB_StringArray): TFRE_DB_UInt32Array;
var i: Integer;
begin
  SetLength(result,length(a));
  for i:=0 to high(a) do begin
    result[i] := StrToInt(A[i]);
  end;
end;

function TFRE_DB.StringArray2Int64Array(const A: TFRE_DB_StringArray): TFRE_DB_Int64Array;
var i: Integer;
begin
  SetLength(result,length(a));
  for i:=0 to high(a) do begin
    result[i] := StrToInt64(A[i]);
  end;
end;

function TFRE_DB.StringArray2UInt64Array(const A: TFRE_DB_StringArray): TFRE_DB_UInt64Array;
var i: Integer;
begin
  SetLength(result,length(a));
  for i:=0 to high(a) do begin
    result[i] := StrToInt64(A[i]);
  end;
end;

function TFRE_DB.StringArray2Real32Array(const A: TFRE_DB_StringArray): TFRE_DB_Real32Array;
var i: Integer;
begin
  SetLength(result,length(a));
  for i:=0 to high(a) do begin
    result[i] := StrToFloat(A[i]);
  end;
end;

function TFRE_DB.StringArray2Real64Array(const A: TFRE_DB_StringArray): TFRE_DB_Real64Array;
var i: Integer;
begin
  SetLength(result,length(a));
  for i:=0 to high(a) do begin
    result[i] := StrToFloat(A[i]);
  end;
end;

function TFRE_DB.StringArray2CurrArray(const A: TFRE_DB_StringArray): TFRE_DB_CurrencyArray;
var i: Integer;
begin
  SetLength(result,length(a));
  for i:=0 to high(a) do begin
    result[i] := StrToCurr(A[i]);
  end;
end;

function TFRE_DB.StringArray2BoolArray(const A: TFRE_DB_StringArray): TFRE_DB_BoolArray;
var i: Integer;
begin
  SetLength(result,length(a));
  for i:=0 to high(a) do begin
    result[i] := FREDB_String2Bool(A[i]);
  end;
end;

function TFRE_DB.StringArray2DateTimeArray(const A: TFRE_DB_StringArray): TFRE_DB_DateTimeArray;
var i : Integer;
    dt: TFRE_DB_DateTime64;
begin
  SetLength(result,length(a));
  for i:=0 to high(a) do begin
    dt := StrToInt64(A[i]);
    result[i] := GFRE_DB.LocalTimeToUTCDB64(dt);
  end;
end;

function TFRE_DB.StringArray2DateTimeArrayUTC(const A: TFRE_DB_StringArray): TFRE_DB_DateTimeArray;
var i : Integer;
    dt: TFRE_DB_DateTime64;
begin
  SetLength(result,length(a));
  for i:=0 to high(a) do begin
    dt := StrToInt64(A[i]);
    result[i] := dt;
  end;
end;


function TFRE_DB.GuidArray2SString(const A: TFRE_DB_GUIDArray): TFRE_DB_String;
var i,len:integer;
begin
  result:='[ ';
  len := Length(a)-1;
  for i := 0 to len do begin
    if i<len then result := result+''''+FREDB_G2H(a[i])+''',' else result := result+''''+FREDB_G2H(a[i])+''' ';
  end;
  result:=result+']';
end;

function TFRE_DB.CountedObjLinks2String(const A: TFRE_DB_CountedGuidArray): TFRE_DB_String;
var i,len:integer;
begin
  result:='[ ';
  len := Length(a)-1;
  for i := 0 to len do begin
    if i<len then result := result+''''+FREDB_G2H(a[i].link)+':'+inttostr(a[i].count)+''',' else result := result+''''+FREDB_G2H(a[i].link)+':'+inttostr(a[i].count)+''' ';
  end;
  result:=result+']';
end;

function TFRE_DB.TranslateLong(const txt: TFRE_DB_TEXT): TFRE_DB_String;
begin
  result := txt.LongText;  // TODO Lookup Translationkey and Translate
end;

function TFRE_DB.RegisterSysScheme(const sch: TFRE_DB_SchemeObject): TFRE_DB_Errortype;
var i     : integer;
    oname : TFRE_DB_String;
begin
  oname := uppercase(sch.DefinedSchemeName);
  for i:=0 to high(FSysSchemes) do begin
    if uppercase(FSysSchemes[i].DefinedSchemeName) = oname then
      exit(edb_EXISTS); // already registerd;
  end;
  setlength(FSysSchemes,length(FSysSchemes)+1);
  FSysSchemes[high(FSysSchemes)] := sch;
  result := edb_OK;
end;

function TFRE_DB.AnonObject2Interface(const Data: Pointer): IFRE_DB_Object;
begin
  result := (TObject(data) as TFRE_DB_Object);
end;

function TFRE_DB.GetSystemScheme(const schemename: TFRE_DB_NameType; var scheme: TFRE_DB_SchemeObject): Boolean;
begin
  result := GetSysScheme(schemename,scheme);
end;

function TFRE_DB.GetSystemSchemeByName(const schemename: TFRE_DB_NameType; var scheme: IFRE_DB_SchemeObject): Boolean;
var schemei : TFRE_DB_SCHEMEOBJECT;
begin
  result := GetSystemScheme(schemename,schemei);
  if result then
    scheme := schemei
  else
    scheme := nil;
end;

function TFRE_DB.GetSystemScheme(const schemename: TClass; var scheme: IFRE_DB_SchemeObject): Boolean;
begin
  result := GetSystemSchemeByName(schemename.ClassName,scheme);
end;

function TFRE_DB.GetSystemEnum(const name: TFRE_DB_NameType; out enum: IFRE_DB_Enum): boolean;
var oenum : TFRE_DB_Enum;
begin
  result := GetSysEnum(name,oenum);
  if result then
    enum := oenum
  else
    enum := nil;
end;

function TFRE_DB.GetSystemClientFieldValidator(const name: TFRE_DB_NameType; out clf: IFRE_DB_ClientFieldValidator): boolean;
var oval : TFRE_DB_ClientFieldValidator;
begin
  result := GetSysClientFieldValidator(name,oval);
  if result then
    clf := oval
  else
    clf := nil;
end;

function TFRE_DB.GetClassesDerivedFrom(const SchemeClass: ShortString): TFRE_DB_ObjectClassExArray;
var  i,c : NativeInt;
     ocl : TFRE_DB_ObjectClassEx;
begin
  SetLength(result,Length(FSysSchemes));
  c:=0;
  for i := 0 to high(FSysSchemes) do
    begin
      if FSysSchemes[i].IsA(SchemeClass) then
        if FSysSchemes[i].FHardCodeClassTyp.InheritsFrom(TFRE_DB_ObjectEx) then
          begin
            result[c] := TFRE_DB_ObjectClassEx(FSysSchemes[i].FHardCodeClassTyp);
            inc(c);
          end;
    end;
  SetLength(result,c);
end;

function TFRE_DB.GetSchemesDerivedFrom(const SchemeClass: ShortString): IFRE_DB_SCHEMEOBJECTArray;
var  i,c : NativeInt;
     ocl : TFRE_DB_ObjectClassEx;
begin
  SetLength(result,Length(FSysSchemes));
  c:=0;
  for i := 0 to high(FSysSchemes) do
    begin
      if FSysSchemes[i].IsA(SchemeClass) then
        begin
          result[c] := FSysSchemes[i];
          inc(c);
        end;
    end;
  SetLength(result,c);
end;

procedure TFRE_DB.ForAllSchemes(const iterator: TFRE_DB_Scheme_Iterator);
var  i: Integer;
begin
  for i := 0 to high(FSysSchemes) do begin
    iterator(FSysSchemes[i]);
  end;
end;

procedure TFRE_DB.ForAllEnums(const iterator: TFRE_DB_Enum_Iterator);
var  i: Integer;
begin
  for i := 0 to high(FSysEnums) do begin
    iterator(FSysEnums[i]);
  end;
end;

procedure TFRE_DB.ForAllClientFieldValidators(const iterator: TFRE_DB_ClientFieldValidator_Iterator);
var i: Integer;
begin
  for i := 0 to high(FSysClientFieldValidators) do begin
    iterator(FSysClientFieldValidators[i]);
  end;
end;

procedure TFRE_DB.ForAllApps(const iterator: TFRE_DB_Apps_Iterator);
var i: Integer;
begin
 for i := 0 to high(FAppArray) do begin
   iterator(FAppArray[i]);
 end;
end;

procedure TFRE_DB.LogDebugIf(const category: TFRE_DB_LOGCATEGORY; const logcallback: TFRE_SimpleCallbackNested);
begin
  logcallback;
end;

procedure TFRE_DB.LogInfoIf(const category: TFRE_DB_LOGCATEGORY; const logcallback: TFRE_SimpleCallbackNested);
begin
  logcallback;
end;

procedure TFRE_DB.LogWarningIf(const category: TFRE_DB_LOGCATEGORY; const logcallback: TFRE_SimpleCallbackNested);
begin
  logcallback;
end;

procedure TFRE_DB.LogErrorIf(const category: TFRE_DB_LOGCATEGORY; const logcallback: TFRE_SimpleCallbackNested);
begin
  logcallback;
end;

procedure TFRE_DB.LogNoticeIf(const category: TFRE_DB_LOGCATEGORY; const logcallback: TFRE_SimpleCallbackNested);
begin
  logcallback;
end;

procedure TFRE_DB.LogEmergencyIf(const category: TFRE_DB_LOGCATEGORY; const logcallback: TFRE_SimpleCallbackNested);
begin
  logcallback;
end;

procedure TFRE_DB.LogDebug(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String);
begin
 LogDebug(category,'%s',[msg]); // prevent misinterpretation  of % sign in msg!
end;

procedure TFRE_DB.LogInfo(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String);
begin
  LogInfo(category,'%s',[msg]);
end;

procedure TFRE_DB.LogWarning(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String);
begin
  LogWarning(category,'%s',[msg]);
end;

procedure TFRE_DB.LogError(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String);
begin
  LogError(category,'%s',[msg]);
end;

procedure TFRE_DB.LogNotice(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String);
begin
 LogNotice(category,'%s',[msg]);
end;

procedure TFRE_DB.LogEmergency(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String);
begin
  LogEmergency(category,'%s',[msg]);
end;

procedure TFRE_DB.LogInfo(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String; const param: array of const);
begin
  GFRE_LOG.Log(msg,param,CFRE_DB_LOGCATEGORY[category],fll_Info,CFOS_LL_Target[fll_Info],false);
end;

procedure TFRE_DB.LogWarning(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String; const param: array of const);
begin
  GFRE_LOG.Log(msg,param,CFRE_DB_LOGCATEGORY[category],fll_Warning,CFOS_LL_Target[fll_Warning],false);
end;

procedure TFRE_DB.LogError(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String; const param: array of const);
begin
  GFRE_LOG.Log(msg,param,CFRE_DB_LOGCATEGORY[category],fll_Error,CFOS_LL_Target[fll_Error],false);
end;

procedure TFRE_DB.LogDebug(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String; const param: array of const);
begin
  GFRE_LOG.Log(msg,param,CFRE_DB_LOGCATEGORY[category],fll_Debug,CFOS_LL_Target[fll_Debug],false);
end;

procedure TFRE_DB.LogNotice(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String; const param: array of const);
begin
  GFRE_LOG.Log(msg,param,CFRE_DB_LOGCATEGORY[category],fll_Notice,CFOS_LL_Target[fll_Notice],false);
end;

procedure TFRE_DB.LogEmergency(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String; const param: array of const);
begin
  GFRE_LOG.Log(msg,param,CFRE_DB_LOGCATEGORY[category],fll_Emergency,CFOS_LL_Target[fll_Emergency],false);
end;


procedure TFRE_DB.ClearGUID(var uid: TFRE_DB_GUID);
begin
  FillByte(uid,sizeof(uid),0);
end;


{ TFRE_DB_Object }

function TFRE_DB_Object._StreamingSize: TFRE_DB_SIZE_TYPE;
var size   : NativeInt;
    clname : shortstring;

  procedure CountFieldStreamingSize(const Field:TFRE_DB_FIELD);
  begin
    size:=size+Field._StreamingSize;
  end;

begin
  size := 0;
  ForAll(@CountFieldStreamingSize,true);
  clname := SchemeClass;
  //if assigned(FMediatorExtention) then
  //  clname := FMediatorExtention.SchemeClass;
  result := CFRE_DB_SIZE_ENCODING_SIZE+length(clname)+size;
end;

procedure TFRE_DB_Object.BeforeSave;
begin
  FDBO_State := fdbos_StreamingWriting;
end;

procedure TFRE_DB_Object.AfterSave;
begin
 FDBO_State := fdbos_Clean;
end;

procedure TFRE_DB_Object.AfterLoad;
begin

end;

function TFRE_DB_Object.Invoke(const method: TFRE_DB_String; const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  if assigned(FMediatorExtention) then begin
    result := FMediatorExtention.Invoke(method,input,ses,app,conn);
  end else begin
    result := Invoke_DBIMI_Method(method,input,ses,app,conn);
  end;
end;

procedure TFRE_DB_Object.Finalize;
begin
  if self=nil then
    exit;
  _InAccessibleCheck;
  Free;
end;

function     ObjecTFRE_DB_GUIDCompare     (const o1,o2 : PFRE_DB_Object):boolean;
begin
  result := FREDB_Guids_Same(o1^.UID,o2^.UID);
end;

function     DBObjIsNull           (const obj   : PFRE_DB_Object) : Boolean;
begin
  result := not assigned(obj^);
end;

class procedure TFRE_DB_Object.GenerateAnObjChangeList(const first_obj, second_obj: TFRE_DB_Object ; const InsertCB,DeleteCB : IFRE_DB_Obj_Iterator ; const UpdateCB : IFRE_DB_UpdateChange_Iterator );
var deleted_obj   : OFRE_SL_TFRE_DB_Object;
    inserted_obj  : OFRE_SL_TFRE_DB_Object;
    updated_obj   : OFRE_SL_TFRE_DB_Object;
    //coll          : TFRE_DB_PERSISTANCE_COLLECTION;
    i             : NativeInt;

    procedure SearchInOldAndRemoveExistingInNew(var o : TFRE_DB_Object ; const idx : NativeInt ; var halt: boolean);
    begin
      if deleted_obj.Exists(o)<>-1 then
        begin
          updated_obj.Add(o);
          inserted_obj.ClearIndex(idx);
        end
    end;

    procedure SearchInUpdatesAndRemoveExistingFromOld(var o : TFRE_DB_Object ; const idx : NativeInt ; var halt: boolean);
    var ex : NativeInt;
    begin
      if updated_obj.Exists(o)<>-1 then
        deleted_obj.ClearIndex(idx);
    end;

    procedure GenerateUpdates(var new_object : TFRE_DB_Object ; const idx : NativeInt ; var halt: boolean);
    var child      : TFRE_DB_Object;

        procedure CompareEvent (const obj:TFRE_DB_Object ; const compare_event : TFRE_DB_ObjCompareEventType ; const new_fld,old_field:TFRE_DB_FIELD);
        var to_up_o : TFRE_DB_Object;
            childup : boolean;
        begin
          childup := assigned(child);
          if childup then
            to_up_o := child
          else
            to_up_o := second_obj;
          case compare_event of
            cev_FieldDeleted:  if assigned(UpdateCB) then
                                 UpdateCB(childup,to_up_o,cev_FieldDeleted,nil,old_field);
            cev_FieldAdded:    if assigned(UpdateCB) then
                                 UpdateCB(childup,to_up_o,cev_FieldAdded,new_fld,Nil);
            cev_FieldChanged : if assigned(UpdateCB) then
                                 UpdateCB(childup,to_up_o,cev_FieldChanged,new_fld,old_field);
          end;
        end;

    begin
      if new_object.IsObjectRoot then
        begin
          child:=nil;
          UpdateCB(false,second_obj,cev_UpdateBlockStart,nil,nil);
          new_object.__InternalCompareToObj(second_obj,@CompareEvent);
          UpdateCB(false,second_obj,cev_UpdateBlockEnd,nil,nil);
        end
      else
        begin
          child := second_obj.FetchObjByUID(new_object.UID);
          assert(assigned(child));
          UpdateCB(true,child,cev_UpdateBlockStart,nil,nil);
          new_object.__InternalCompareToObj(child,@CompareEvent);
          UpdateCB(true,child,cev_UpdateBlockEnd,nil,nil);
        end;
    end;

    procedure GenerateInserts(var new_object : TFRE_DB_Object ; const idx : NativeInt ; var halt: boolean);
    begin
      if assigned(InsertCB) then
        InsertCB(new_object);
    end;

    procedure GenerateDeletes(var del_object : TFRE_DB_Object ; const idx : NativeInt ; var halt: boolean);
    begin
      if assigned(DeleteCB) then
        DeleteCB(del_object);
    end;
begin
  if (not assigned(first_obj)) then
       raise EFRE_DB_Exception.Create(edb_ERROR,'at least the first object must be assigned');

    deleted_obj.InitSparseList(nil,@DBObjIsNull,@ObjecTFRE_DB_GUIDCompare,25);
    inserted_obj.InitSparseList(nil,@DBObjIsNull,@ObjecTFRE_DB_GUIDCompare,25);
    updated_obj.InitSparseList(nil,@DBObjIsNull,@ObjecTFRE_DB_GUIDCompare,25);

    if assigned(second_obj) then // update case 2nd = to_update, 1st=obj
      second_obj.__InternalGetFullObjectList(deleted_obj);

    first_obj.__InternalGetFullObjectList(inserted_obj);

    inserted_obj.ForAllBreak(@SearchInOldAndRemoveExistingInNew);    // Yields the deletes in the oldlist, all objects in this are from the "old, stored persistent object"
    deleted_obj.ForAllBreak(@SearchInUpdatesAndRemoveExistingFromOld);

    if deleted_obj.Count>0 then
      deleted_obj.ForAllBreak(@GenerateDeletes);
    if inserted_obj.Count>0 then
      inserted_obj.ForAllBreak(@GenerateInserts);
    if updated_obj.Count>0 then
      updated_obj.ForAllBreak(@GenerateUpdates);
end;

class function TFRE_DB_Object.CompareObjectsEqual(const first_obj, second_obj: TFRE_DB_Object): Boolean;
var insc,delc,upc : NativeInt;

    procedure ins(const obj : IFRE_DB_Object);
    begin
      inc(insc);
    end;
    procedure del(const obj : IFRE_DB_Object);
    begin
      inc(delc);
    end;
    procedure up(const is_child_update : boolean ; const update_obj : IFRE_DB_Object ; const update_type :TFRE_DB_ObjCompareEventType  ;const new_field, old_field: IFRE_DB_Field);
    var nfn,ofn : TFRE_DB_NameType;
    begin
      if (update_type=cev_UpdateBlockStart)
         or (update_type=cev_UpdateBlockEnd) then
           exit;
      if assigned(old_field) then
        ofn := old_field.FieldName;
      if assigned(new_field) then
        nfn := new_field.FieldName;
      inc(upc);
    end;

begin
  insc:=0;
  delc:=0;
  upc :=0;
  GenerateAnObjChangeList(first_obj,second_obj,@ins,@del,@up);
  result := (insc=0)
             and (delc=0)
             and (upc=0);
end;


function TFRE_DB_Object.InternalUniqueDebugKey: String;
begin
  WriteStr(result,'(',FREDB_ObjectToPtrUInt(self),'-',ClassName,'[',FREDB_G2H(FUID)+'/'+FREDB_G2H(FDomainID),' ',BoolToStr(assigned(Parent),'C','R'),')');
end;

function TFRE_DB_Object.GetDescriptionID: String;
begin
  result := BoolToStr(assigned(Parent),'C','R')+':'+SchemeClass+'('+FREDB_CombineString(GetUIDPath,'/')+')';
end;


procedure TFRE_DB_Object.Free;
   procedure DebugCheck;
   var obr : TFRE_DB_Object;
   begin
     obr := _ObjectRoot;
     if (fop_STORED_IMMUTABLE in obr.FObjectProps) then
       raise EFRE_DB_Exception.Create(edb_INTERNAL,'FREEING A STORE LOCKED OBJECT!! (bad idea)');
   end;

begin
  if self<>nil then begin
    if assigned(FMediatorExtention) then begin
      if FMediatorExtention is TFRE_DB_NIL_DESC then begin
        //writeln('>> WARNING NIL DESCRIPTION :::: FREE_REAL & MEDIATOR EXTENSION: ',ClassName,'  ',FMediatorExtention.ClassName);
        //THink about PARENTED NIL DESCRIPTIONS !!!!
        FParentDBO := nil;
        exit;
      end;
      if FMediatorExtention is TFRE_DB_SUPPRESS_ANSWER_DESC then begin
        writeln('>> WARNING SUPRESS ANSWER DESCRIPTION :::: FREE_REAL & MEDIATOR EXTENSION: ',ClassName,'  ',FMediatorExtention.ClassName);
        FParentDBO := nil;
        //THink about PARENTED NIL SUPRESS ANSWER DESCRIPTIONS !!!!
        exit;
      end;
      FMediatorExtention.FreeFromBackingDBO;
      FMediatorExtention:=nil;
    end else begin
      //writeln('FREE_REAL : ',ClassName,' ',InternalUniqueDebugKey);
      //writeln('---');
      //writeln(DumpToString());
      //writeln('---');
    end;
    DebugCheck;
    inherited Free;
  end;
end;


procedure TFRE_DB_Object.ForAllFields(const iter: TFRE_DB_FieldIterator;const without_calcfields:boolean=false;const without_system_fields:boolean=false);
begin
 _InAccessibleCheck;
  ForAll(iter,without_calcfields,without_system_fields);
end;

procedure TFRE_DB_Object.ForAllFieldsBreak(const iter: TFRE_DB_FieldIteratorBrk;const without_calcfields:boolean=false;const without_system_fields:boolean=false);
begin
 _InAccessibleCheck;
  ForAllBrk(iter,without_calcfields,without_system_fields);
end;

procedure TFRE_DB_Object.ForAllFields(const iter: IFRE_DB_FieldIterator;const without_calcfields:boolean=false;const without_system_fields:boolean=false);

  procedure lForAll(const field:TFRE_DB_FIELD);
  begin
    iter(field);
  end;

begin
  _InAccessibleCheck;
   ForAll(@lForAll,without_calcfields,without_system_fields);
end;

procedure TFRE_DB_Object.ForAllFieldsBreak(const iter: IFRE_DB_FieldIteratorBrk;const without_calcfields:boolean=false;const without_system_fields:boolean=false);
  function lForAll(const field:TFRE_DB_FIELD):boolean;
  begin
    result := iter(field);
  end;
begin
  _InAccessibleCheck;
  ForAllBrk(@lForAll,without_calcfields,without_system_fields);
end;

procedure TFRE_DB_Object.ForAllObjects(const iter: IFRE_DB_Obj_Iterator);
  procedure Iterate(const fld:IFRE_DB_Field);
  begin
     if fld.FieldType=fdbft_Object then
       iter(fld.AsObject);
  end;

begin
  ForAllFields(@Iterate);
end;

procedure TFRE_DB_Object.ForAllObjectsFieldName(const iter: IFRE_DB_Obj_NameIterator);
  procedure Iterate(const fld:IFRE_DB_Field);
  begin
     if fld.FieldType=fdbft_Object then
       iter(fld.FieldName,fld.AsObject);
  end;
begin
  ForAllFields(@Iterate);
end;

function TFRE_DB_Object.ForAllObjectsBreakHierarchic(const iter: TFRE_DB_ObjectIteratorBrk): boolean;
var halt : boolean;

  procedure IterateWithSub(const obj : TFRE_DB_Object);

    function IterateField(const fld : TFRE_DB_FIELD):boolean;
    var i : NativeInt;
    begin
      if halt then
        exit(true);
      if fld.IsObjectField then
        IterateWithSub(Fld.AsObject);
      result := false;
    end;
  begin
    if halt then
      exit;
    iter(obj,halt);
    if not halt then
      obj.ForAllBrk(@IterateField);
  end;

begin
  halt := false;
  IterateWithSub(self);
  result := halt;
end;

function TFRE_DB_Object.GetScheme(const raise_non_existing: boolean): TFRE_DB_SchemeObject;
var FSchemeName : ShortString;
begin
  FSchemeName := SchemeClass;
  if not assigned(FCacheSchemeObj) then begin
    if not GFRE_DB.GetSystemScheme(FSchemeName,TFRE_DB_SchemeObject(FCacheSchemeObj)) then
      if raise_non_existing then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'could not access schemeobject from object Schemename [%s] - Classname [%s]',[FSchemeName,ClassName]);
  end;
  result := FCacheSchemeObj;
end;

function TFRE_DB_Object.GetSchemeI(const raise_non_existing: boolean): IFRE_DB_SchemeObject;
begin
  result := GetScheme(raise_non_existing);
end;

function TFRE_DB_Object.GetFormattedDisplay: TFRE_DB_String;
var so:IFRE_DB_SchemeObject;
begin
  _InAccessibleCheck;
  so := GetScheme;
  if assigned(so) then begin
    result := so.GetFormattedDisplay(self);
  end else
    begin
      if FieldExists('objname') then begin
        result:='[objname = '+Field('objname').AsString+']';
      end else
      if FieldExists('_simpleformat') then begin
        result:=field('_simpleformat').AsString;
      end else begin
        result := '[NO FORMATTED SCHEME DISPLAY AVAILABLE]';
      end;
    end;
end;

function TFRE_DB_Object.FormattedDisplayAvailable: boolean;
var so:IFRE_DB_SchemeObject;
begin
  _InAccessibleCheck;
  so := GetScheme;
  if assigned(so) then
    result := so.FormattedDisplayAvailable(self)
  else
    result := false;
end;


function TFRE_DB_Object.UID: TFRE_DB_GUID;
begin
  result := FUID;
end;

function TFRE_DB_Object.DomainID: TFRE_DB_GUID;
begin
 result := FDomainID;
end;

function TFRE_DB_Object.DomainID_String: TFRE_DB_GUID_String;
begin
 result := FREDB_G2H(FDomainID);
end;

procedure TFRE_DB_Object.SetDomainID(const domid: TFRE_DB_GUID);
begin
  _field('domainid').AsGUID:=domid;
end;

function TFRE_DB_Object.UID_String: TFRE_DB_GUID_String;
begin
  result := FREDB_G2H(FUID);
end;

function TFRE_DB_Object.UIDP: PByte;
begin
  result := @FUID;
end;

function TFRE_DB_Object.PUID: PFRE_DB_Guid;
begin
 result := @FUID;
end;


constructor TFRE_DB_Object.Create;
begin
  inherited Create;
  FDBO_State             := fdbos_Creating;
  FObjectProps           := [];
  FFieldStore            := _TFRE_DB_FieldTree.Create(@FREDB_DBNameType_Compare);
  FUID                   := GFRE_DB.Get_A_GUID;
  FDomainID              := CFRE_DB_NullGUID;
  _RestoreReservedFields ;
  InternalSetup;
  assert(FDBO_State=fdbos_Creating);
  FDBO_State:=fdbos_Dirty;
end;

constructor TFRE_DB_Object.CreateStreaming(const ExtensionObjectMediatorClass: TFRE_DB_OBJECTCLASSEX);
begin
  inherited Create;
  FDBO_State             := fdbos_StreamingCreating;
  FFieldStore            := _TFRE_DB_FieldTree.Create(@FREDB_DBNameType_Compare);
  _RestoreReservedFields ;
  if assigned(ExtensionObjectMediatorClass) then begin
    FMediatorExtention   := ExtensionObjectMediatorClass.CreateBound(Self,false);
  end;
end;

constructor TFRE_DB_Object.CreateStreaming(const WeakExObject: TFRE_DB_WeakObjectEx);
begin
  CreateStreaming;
  FMediatorExtention := WeakExObject;
end;

destructor TFRE_DB_Object.Destroy;

  procedure DoAllFinalizeFields(const field:TFRE_DB_FIELD);
  begin
    field.Free;
  end;

begin
  FDBO_State := fdbos_Destroying;
  FFieldStore.ForallItems(@DoAllFinalizeFields); // plus unknown fields
  FFieldStore.Free;
  InternalFinalize;
  inherited;
end;


function TFRE_DB_Object.NeededSize: TFRE_DB_SIZE_TYPE;
begin
  _InAccessibleCheck;
  result := SizeOf(TFRE_DB_ObjectHdr) + CFRE_DB_SIZE_ENCODING_SIZE + CFRE_DB_SIZE_ENCODING_SIZE+ _StreamingSize; // Header + Fieldcount + FinalStreamingsize +  Recursive_Streamingsize
end;

function TFRE_DB_Object._ObjectRoot: TFRE_DB_Object;
begin
 result := self;
 while assigned(result.FParentDBO) do
   result := result.FParentDBO.Fobj;
end;

function TFRE_DB_Object.ObjectRoot: TFRE_DB_Object;
begin
   Result := _ObjectRoot;
end;

function TFRE_DB_Object.ObjectRootI: IFRE_DB_Object;
begin
  result := ObjectRoot;
end;

function TFRE_DB_Object.IsObjectRoot: Boolean;
begin
  result := not assigned(FParentDBO);
end;

function TFRE_DB_Object.Parent: TFRE_DB_Object;
begin
  if assigned(FParentDBO) then begin
    result := FParentDBO.Fobj;
  end else begin
    result := nil;
  end;
end;

function TFRE_DB_Object.ParentI: IFRE_DB_Object;
begin
  result := Parent;
end;

function TFRE_DB_Object.ParentField: TFRE_DB_FIELD;
begin
  result := FParentDBO;
end;

function TFRE_DB_Object.ParentFieldI: IFRE_DB_FIELD;
begin
  result := ParentField;
end;

procedure TFRE_DB_Object.CopyToMemory(memory: Pointer);
var   sz_field         : TFRE_DB_SIZE_TYPE;
      old_mem_location : pointer;
begin
  old_mem_location := memory;
  Move(CFRE_DB_ObjectHdr,memory^,Sizeof(CFRE_DB_ObjectHdr));           //     HEADER
  Inc(PByte(memory),Sizeof(CFRE_DB_ObjectHdr));                        //     HEADER

  sz_field := TFRE_DB_SIZE_TYPE(FieldCount(true,false));               //     FIELDCOUNT
  dec(sz_field); { remove schemecalss field, which does not get streamed }
  Move     (sz_field,memory^,CFRE_DB_SIZE_ENCODING_SIZE);              // 1 x FIELDCOUNT
  inc      (memory,CFRE_DB_SIZE_ENCODING_SIZE);                        //     FIELDCOUNT

  CopyToMem(memory);                                                   //     INTERNAL
  if (memory-old_mem_location) <> NeededSize then
    GFRE_BT.CriticalAbort('internal streaming miscalculation save actual=%d <> calced=%d ',[memory-old_mem_location,NeededSize]);
end;



class function TFRE_DB_Object.CreateFromMemory(memory: Pointer ; const generate_new_uids: boolean): TFRE_DB_Object;
var hdr:TFRE_DB_ObjectHdr;
begin
  Move(Memory^,hdr,Sizeof(CFRE_DB_ObjectHdr));
  if (hdr.Signature<>CFRE_DB_ObjectHdr.Signature) then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'HEADER SIGNATURE BAD');
  Inc(PByte(memory),Sizeof(CFRE_DB_ObjectHdr));
  if hdr.Version<2 then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'BINARY FORMAT TOO OLD, UNSUPPORTED');
  result := CreateInternalStreaming(nil,memory,generate_new_uids,hdr.Version,hdr.EndianMarker);
end;

function TFRE_DB_Object.FieldI(const name: TFRE_DB_NameType): IFRE_DB_FIELD;
begin
  result := Field(name);
end;

procedure TFRE_DB_Object._RestoreReservedFields;
begin
  _Field('UID').AsGUID           := FUID;
  _Field('DomainID').AsGUID      := FDomainID;
  _Field('Schemeclass');
end;

procedure TFRE_DB_Object.ForAll(const iter: TFRE_DB_FieldIterator; const without_calcfields: boolean; const without_system_fields: boolean);
var scheme_object:TFRE_DB_SchemeObject;
   procedure Iterate(const db:TFRE_DB_FIELD);
   begin
     if (db.FieldType<>fdbft_NotFound) then
       begin
         if (without_system_fields)
            and (db.IsSystemField) then
              exit;
           iter(db);
       end;
   end;
begin
  _InAccessibleCheck;
  FFieldStore.ForAllItems(@Iterate);
  scheme_object := GetScheme;
  if (not without_calcfields) and assigned(GetScheme) then begin
    scheme_object.ForAllCalculatedFields(@Iterate,self);
  end;
end;

procedure TFRE_DB_Object.ForAllBrk(const iter: TFRE_DB_FieldIteratorBrk; const without_calcfields: boolean; const without_system_fields: boolean);
var scheme_object:TFRE_DB_SchemeObject;
   function Iterate(const db:TFRE_DB_FIELD):boolean;
   begin
     if (db.FieldType<>fdbft_NotFound) then
       begin
         if (without_system_fields)
           and (db.IsSystemField) then
             exit;
         result := iter(db);
       end;
   end;
begin
  _InAccessibleCheck;
  FFieldStore.ForAllItemsBrk(@Iterate);
  scheme_object := GetScheme;
  if (not without_calcfields) and assigned(GetScheme) then begin
    scheme_object.ForAllCalculatedFieldsBrk(@Iterate,self);
  end;
end;

function TFRE_DB_Object.AsString: TFRE_DB_String;
begin
  _InAccessibleCheck;
  SetLength(Result,NeededSize);
  CopyToMemory(@result[1]);
end;

class function TFRE_DB_Object.CreateFromString(const AValue: TFRE_DB_String; const generate_new_uids: boolean): TFRE_DB_Object;
begin
  result:=CreateFromMemory(@AValue[1],generate_new_uids);
end;

class function TFRE_DB_Object.CreateFromJSONString(const AValue: TFRE_DB_String ; const stream_cb: TFRE_DB_StreamingCallback): TFRE_DB_Object;
var jd : TJSONData;
    jp : TJSONParser;
begin
  try
    jp     := TJSONParser.Create(AValue,true);
    jd     := jp.Parse;
    result := CreateInternalStreamingJSON(nil,jd as TJSONArray, stream_cb);
  finally
    jp.free;
    jd.free;
  end;
end;

function TFRE_DB_Object._Field(name: TFRE_DB_NameType): TFRE_DB_FIELD;
var lfield    : TFRE_DB_Field;
    keyp      : PFRE_DB_NameType;
    storep    : ^TFRE_DB_Field;
    calcexist : boolean;
    lcalctyp  : TFRE_DB_FIELDTYPE;
    lcalcm    : IFRE_DB_CalcMethod;
begin
  name := UpperCase(name);
  if Pos('.',name)>0 then raise EFRE_DB_Exception.Create(' "." character is not allowed in fieldnames');
  result:=nil;
  //SchemeFieldAccessCheck(name);
  if FFieldStore.Find(name,lfield) then begin
    result := lfield;
  end else begin
    calcexist := CalcFieldExists(name,lcalctyp,lcalcm);
    if calcexist then
      lfield  := TFRE_DB_Field.Create(self,lcalctyp,'',lcalcm)
    else
      lfield  := TFRE_DB_Field.Create(self,fdbft_NotFound);
    if not FFieldStore.AddCheckEx(name,lfield,false,keyp,storep) then raise EFRE_DB_Exception.create(edb_INTERNAL,'fieldstore');
    lfield.FFieldname := keyp;
    if name='UID' then
      lfield.FIsUidField:= true;
    if name='DOMAINID' then
      lfield.FIsDomainIDField := true;
    if name='SCHEMECLASS' then
      begin
        lfield.FIsSchemeField := true;
        lfield.FFieldData.FieldType := fdbft_String;
      end;
    result := lfield;
  end;
  if lfield.IsFieldCalculated then
    lfield.CalculateValue;
end;

function TFRE_DB_Object._FieldOnlyExisting(name: TFRE_DB_NameType): TFRE_DB_FIELD;
var lfield : TFRE_DB_Field;
begin
  result:=nil;
  name := UpperCase(name);
  if FFieldStore.Find(name,lfield) then begin
    if lfield._FieldType<>fdbft_NotFound then begin
      result := lfield;
    end;
  end;
end;

procedure TFRE_DB_Object._ParentCheck(const newdbo: TFRE_DB_Object);
var lCheck_DBO : TFRE_DB_Object;
begin
  lCheck_DBO := self;
  repeat
    if lCheck_DBO=newdbo then raise EFRE_DB_Exception.Create(edb_ERROR,'creating a circle with objects is not allowed');
    if assigned(lCheck_DBO.FParentDBO) then begin
      lCheck_DBO := lCheck_DBO.FParentDBO.Fobj;
    end else break;
  until lCheck_DBO=nil;
end;

function TFRE_DB_Object._ReadOnlyCheck: boolean;
var lCheck_DBO : TFRE_DB_Object;
begin
  result:=false;
  lCheck_DBO := self;
  repeat
    if fop_READ_ONLY in FObjectProps  then exit(true);
    if assigned(lCheck_DBO.FParentDBO) then begin
      lCheck_DBO := lCheck_DBO.FParentDBO.Fobj;
    end else break;
  until lCheck_DBO=nil;
end;


function TFRE_DB_Object._ObjectsNeedsNoSubfieldSchemeCheck: boolean;
begin
  // -> UNCOMMENTED THUS REMOVED if (ClassType=TFRE_DB_Object) and (SchemeClass<>'') then exit(true); //
  result := false;
end;

function TFRE_DB_Object._ObjectIsCodeclassOnlyAndHasNoScheme: boolean;
begin
  result := false;
end;


procedure TFRE_DB_Object._InAccessibleCheck;
begin
  if assigned(FParentDBO) then
    _ObjectRoot._InAccessibleCheck
  else
    if fop_STORED_IMMUTABLE in FObjectProps then
      raise EFRE_DB_Exception.Create(edb_ERROR,'this object is stored, illegal modification : '+InternalUniqueDebugKey);
end;

function TFRE_DB_Object._ReservedFieldName(const upper_name: TFRE_DB_NameType): boolean;
begin
  if (upper_name='UID')
     or (upper_name='DOMAINID')
       or (upper_name='SCHEMECLASS') then
         exit(true)
  else
    exit(false);
end;

procedure TFRE_DB_Object._InternalSetMediatorScheme(const mediator: TFRE_DB_ObjectEx; const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  FMediatorExtention := mediator;
  if assigned(scheme) then
    SetScheme(scheme.Implementor as TFRE_DB_SchemeObject);
end;

function TFRE_DB_Object._InternalDecodeAsField: IFRE_DB_Field;
var new_fld : TFRE_DB_FIELD;
begin
 new_fld := TFRE_DB_FIELD.Create(nil,fdbft_NotFound,Field('FN').AsString,nil);
 new_fld.CloneFromField(Field('F'));
 new_fld.FObjUidPath         := Field('FUP').AsGUIDArr;
 new_fld.FInCollectionArray  := Field('FIC').AsStringArr;
 new_fld.FSchemePath         := Field('OCP').AsStringArr;
 new_fld.FUpObjFieldPath     := Field('OFP').AsStringArr;
 result := new_fld;
end;

procedure TFRE_DB_Object._InternalGuidNullCheck;
var  refs : TFRE_DB_ObjectReferences;
     i    : NativeInt;
begin
  refs := ReferencesFromData;
  for i := 0 to high(refs) do
    begin
      if refs[i].linked_uid=CFRE_DB_NullGUID then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'NULL GUID STORED IN FIELD [%s]:[%s]',[refs[i].fieldname,GetDescriptionID]);
    end;
end;

procedure TFRE_DB_Object.__InternalCollectionAdd(const coll: TFRE_DB_PERSISTANCE_COLLECTION_BASE);
begin
  if __InternalCollectionExists(coll)<>-1 then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'try internal add object [%s] to collection [%s], but its already existing',[self.InternalUniqueDebugKey,coll.CollectionName]);
  SetLength(FInCollectionarr,Length(FInCollectionarr)+1);
  FInCollectionarr[High(FInCollectionarr)] := coll;
end;

function TFRE_DB_Object.__InternalCollectionRemove(const coll: TFRE_DB_PERSISTANCE_COLLECTION_BASE): NativeInt;
var new_coll_array : array of TFRE_DB_PERSISTANCE_COLLECTION_BASE;
    i,cnt          : NativeInt;
begin
  if __InternalCollectionExists(coll)=-1 then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'try internal del object [%s] from collection [%s], but its not existing in this collection',[self.InternalUniqueDebugKey,coll.CollectionName]);
  if Length(FInCollectionarr)=1 then
    SetLength(FInCollectionarr,0)
  else
    begin
      SetLength(new_coll_array,Length(FInCollectionarr)-1);
      cnt := 0;
      for i := 0 to high(FInCollectionarr) do
        begin
          if coll.CollectionName(true) <> FInCollectionarr[i].CollectionName(true) then
            begin
              new_coll_array[cnt] := FInCollectionarr[i];
              inc(cnt);
            end;
        end;
      FInCollectionarr := new_coll_array;
    end;
  result := Length(FInCollectionarr);
end;

function TFRE_DB_Object.__InternalCollectionExists(const coll: TFRE_DB_PERSISTANCE_COLLECTION_BASE): NativeInt;
var i : NativeInt;
begin
  result := -1;
  for i := 0 to high(FInCollectionarr) do
    if coll.CollectionName(true) = FInCollectionarr[i].CollectionName(true) then
      exit(i);
end;

function TFRE_DB_Object.__InternalCollectionExistsName(const collname: TFRE_DB_NameType): NativeInt;
var i         : NativeInt;
    lCollname : TFRE_DB_NameType;
begin
  result    := -1;
  lCollname := UpperCase(collname); //self.FInCollectionarr
  for i := 0 to high(FInCollectionarr) do
    if lcollname = FInCollectionarr[i].CollectionName(true) then
      exit(i);
end;

function TFRE_DB_Object.__InternalGetCollectionList: TFRE_DB_PERSISTANCE_COLLECTION_ARRAY;
begin
  result := FInCollectionarr;
end;

function TFRE_DB_Object.__InternalGetCollectionListUSL: TFRE_DB_StringArray;
var i: NativeInt;
begin
  SetLength(result,length(FInCollectionarr));
  for i := 0 to high(FInCollectionarr) do
    result[i] := FInCollectionarr[i].CollectionName(True);
end;

procedure TFRE_DB_Object.__InternalGetFullObjectList(var list: OFRE_SL_TFRE_DB_Object);

  procedure BuildList(const obj : TFRE_DB_Object ; var halt : boolean);
  begin
    list.Add(obj);
  end;

begin
  ForAllObjectsBreakHierarchic(@BuildList);
end;

procedure TFRE_DB_Object.__InternalCompareToObj(const compare_obj: TFRE_DB_Object; callback: TFRE_DB_ObjCompareCallback);

  procedure BaseFields(const fld : TFRE_DB_FIELD);
  var cmp_fld : TFRE_DB_FIELD;
            i : NativeInt;
  begin
    //writeln('Base FIELD : ',fld.FieldName,' ',fld.FieldType);
    cmp_fld := compare_obj._FieldOnlyExisting(fld.FieldName);
    if not assigned(cmp_fld) then
      callback(self,cev_FieldAdded,fld,nil)
    else
      //for i:=0 to fld.ValueCount-1 do
          if not fld.CompareToFieldShallow(cmp_fld) then
            callback(self,cev_FieldChanged,fld,cmp_fld);
  end;

  procedure CompareFields(const fld : TFRE_DB_FIELD);
  var cmp_fld : TFRE_DB_FIELD;
  begin
    //writeln('Compare FIELD : ',fld.FieldName,' ',fld.FieldType);
    cmp_fld := _FieldOnlyExisting(fld.FieldName);
    if not assigned(cmp_fld) then
      callback(self,cev_FieldDeleted,nil,fld)
  end;


begin
  ForAllFields(@BaseFields);
  compare_obj.ForAllFields(@CompareFields);
end;

procedure TFRE_DB_Object.Set_ReadOnly;
begin
  Include(FObjectProps,fop_READ_ONLY);
end;

procedure TFRE_DB_Object.Set_Volatile;
begin
  Include(FObjectProps,fop_VOLATILE);
end;

procedure TFRE_DB_Object.Set_SystemDB;
begin
 Include(FObjectProps,fop_IN_SYSTEM_DB);
end;

procedure TFRE_DB_Object.Set_System;
begin
  Include(FObjectProps,fop_SYSTEM);
end;

procedure TFRE_DB_Object.Set_Store_Locked(const locked: boolean);
begin
  if Assigned(FParentDBO) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'trying to store lock/unlock a sub object -> logic failure');
  if locked then
    Include(FObjectProps,fop_STORED_IMMUTABLE)
  else
    Exclude(FObjectProps,fop_STORED_IMMUTABLE);
end;

procedure TFRE_DB_Object.Set_Store_LockedUnLockedIf(const locked: boolean; out lock_state: boolean);
begin
  if Assigned(FParentDBO) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'trying to store lock/unlock a sub object -> logic failure');
  if locked then
    begin
      if lock_state then
        Include(FObjectProps,fop_STORED_IMMUTABLE);
    end
  else
    begin
      lock_state:= fop_STORED_IMMUTABLE in FObjectProps;
      if lock_state then
        Exclude(FObjectProps,fop_STORED_IMMUTABLE);
    end;
end;

procedure TFRE_DB_Object.Assert_CheckStoreLocked;
begin
  if assigned(FParentDBO) then
    _ObjectRoot.Assert_CheckStoreLocked
  else
    if not (fop_STORED_IMMUTABLE in FObjectProps) then
      raise EFRE_DB_Exception.Create(edb_INTERNAL,'FAILURE NOT STORELOCKED : '+InternalUniqueDebugKey);
end;

procedure TFRE_DB_Object.Assert_CheckStoreUnLocked;
begin
  if assigned(FParentDBO) then
    _ObjectRoot.Assert_CheckStoreUnLocked
  else
    if (fop_STORED_IMMUTABLE in FObjectProps) then
      raise EFRE_DB_Exception.Create(edb_INTERNAL,'FAILURE OBJECT IS STORELOCKED BUT SHOULD NOT BE : '+InternalUniqueDebugKey);
end;


procedure TFRE_DB_Object.SetScheme(const scheme_obj: TFRE_DB_SchemeObject);
begin
  _InAccessibleCheck;
  if scheme_obj.DefinedSchemeName<>SchemeClass then
    raise EFRE_DB_Exception.Create(edb_ERROR,'definedschemeclass and codeclass does not match');
  FCacheSchemeObj    := scheme_obj;
  //if assigned(scheme_obj) and (scheme_obj is TFRE_DB_SchemeObject) then begin
  //  FSchemeName     := scheme_obj.DefinedSchemeName;
  //  FCacheSchemeObj :=
  //end;
end;


function TFRE_DB_Object.CalcFieldExists(const name: TFRE_DB_NameType; var calculated_field_type: TFRE_DB_FIELDTYPE; var calcmethod: IFRE_DB_CalcMethod): boolean;
var scheme:TFRE_DB_SchemeObject;
begin
  result:=false;
  calcmethod            := nil;
  calculated_field_type := fdbft_NotFound;
  if not _ReservedFieldName(uppercase(name)) then
    begin
      scheme:=GetScheme;
      if assigned(scheme) then
        result:=scheme.CalcFieldExists(name,calculated_field_type,calcmethod);
    end;
end;

procedure TFRE_DB_Object.InternalSetup;
begin

end;

procedure TFRE_DB_Object.SchemeFieldAccessCheck(const name: TFRE_DB_NameType);
var scheme:TFRE_DB_SchemeObject;
begin
  if not _ReservedFieldName(uppercase(name)) then begin
    scheme:=GetScheme;
    if assigned(scheme) then begin
      scheme._FieldAccessCheck(name);
    end else begin
      //abort;// HMM System DB load WAC case
    end;
  end;
end;


procedure TFRE_DB_Object.InternalFinalize;
begin

end;

procedure TFRE_DB_Object.CopyToMem(var mempointer: Pointer);
var size,neg_size,res    : TFRE_DB_SIZE_TYPE;
    oldp                 : pointer;
    clname               : ShortString;

  procedure LocalCopyToMem(const field:TFRE_DB_FIELD);
  begin
    size := field._StreamingSize;
    //writeln('>LCP ',field.FieldName,' ',size);
    //if field.FieldName='IX' then begin
    //  writeln('NOW ',field._StreamingSize,' ',FDBO_State);
    //end;
    res  := Field.CopyFieldToMem(mempointer);
    //writeln('<LCP ',field.FieldName,' ',size);
    //DBGSTREAM
    if res<>size then begin
      writeln('******** ERROR <',classname,' ',field.FieldName,'>');
      //field.FFieldStreamSize:=-1;
      //size := field._StreamingSize;
      //res  := Field.CopyFieldToMem(mempointer);
      GFRE_BT.CriticalAbort(Format('INTERNAL STREAMING (A)  Miscalculation Field <%s> - %d <> %d',[field.FieldName,size,res]));
    end;
    Inc(PByte(mempointer),size);
  end;
begin
//  writeln('COPY TO MEM ',ClassName,' ',integeR(self));
  oldp  := mempointer;

  size := TFRE_DB_SIZE_TYPE(_StreamingSize);
  //writeln('--SZ -> ',FStreamingSize);

  BeforeSave;
  Move (size,mempointer^,CFRE_DB_SIZE_ENCODING_SIZE);
  inc  (mempointer,CFRE_DB_SIZE_ENCODING_SIZE);

  clname := SchemeClass;
  size   := length(clname);
  Move (size,mempointer^,CFRE_DB_SIZE_ENCODING_SIZE);
  inc  (mempointer,CFRE_DB_SIZE_ENCODING_SIZE);
  Move (clname[1],mempointer^,size);
  inc  (mempointer,size);

  //size := length(FSchemeName);
  //if not without_schemes then begin
  //  Move (size,mempointer^,CFRE_DB_SIZE_ENCODING_SIZE);
  //end else begin
  //  neg_size := -1 * size;
  //  Move (neg_size,mempointer^,CFRE_DB_SIZE_ENCODING_SIZE);
  //end;
  //inc  (mempointer,CFRE_DB_SIZE_ENCODING_SIZE);
  //Move (FSchemeName[1],mempointer^,size);
  //inc  (mempointer,size);

  ForAll(@LocalCopyToMem,true); { ignore calculated fields on copy }
  //writeln('<--SZ  ',FStreamingSize);
  AfterSave;
  res := mempointer-oldp;
  //if not without_schemes then begin
    //if res <> (FStreamingSize + CFRE_DB_SIZE_ENCODING_SIZE ) then begin
    if res <> (_StreamingSize + CFRE_DB_SIZE_ENCODING_SIZE ) then begin
      writeln('STREAMING FISHHHH');
      writeln('---------');
      writeln(DumpToString());
      GFRE_BT.CriticalAbort(Format('INTERNAL STREAMING (B)  Miscalculation Object - %d <> %d',[res,_StreamingSize]));
    end;
  //end else begin
  //  ;
  //end;
end;

procedure TFRE_DB_Object.CopyFromMem(var mempointer: Pointer; const field_count: TFRE_DB_SIZE_TYPE; const generate_new_uids: boolean; const version: byte; const endianmarker: byte);
var i:Integer;
    F:TFRE_DB_FIELD;
    field_type  : TFRE_DB_FIELDTYPE;
    value_count : TFRE_DB_SIZE_TYPE;
    field_name  : TFRE_DB_NameType;
begin
  if field_count>0 then begin
    for i := 0 to field_count-1 do begin
      TFRE_DB_FIELD.__ReadHeader(mempointer,field_name);
      _Field(field_name).CopyFieldFromMem(mempointer,generate_new_uids,version,endianmarker);
      if field_name='UID' then
        begin
          if generate_new_uids then
            _Field('UID').AsGUID := GFRE_DB.Get_A_Guid;
          FUID       := _Field('UID').AsGUID;
          FDomainID  := _Field('DomainID').AsGUID;
        end;
      if uppercase(field_name)='DOMAINID' then
        FDomainID  := _Field('DomainID').AsGUID;
    end;
  end;
end;

procedure TFRE_DB_Object.CopyFromJSON(const JSON: TJSONArray; const field_count: TFRE_DB_SIZE_TYPE; const stream_cb:TFRE_DB_StreamingCallback=nil);
var i:Integer;
    F:TFRE_DB_FIELD;
    field_type  : TFRE_DB_FIELDTYPE;
    value_count : TFRE_DB_SIZE_TYPE;
    field_name  : TFRE_DB_NameType;
    jo          : TJSONObject;
    cft         : TFRE_DB_FIELDTYPE;
    cm          : IFRE_DB_CalcMethod;
begin
  if field_count>0 then begin
    for i := 0 to field_count-1 do begin
      jo         := JSON.Items[i+1] as TJSONObject;
      field_name := jo.Elements['N'].AsString;
      field_type := FieldtypeShortString2Fieldtype(jo.Elements['T'].AsString);
      if not CalcFieldExists(field_name,cft,cm) then
        _Field(field_name).SetFromJSON(field_type,jo.Elements['D'] as TJSONArray,stream_cb)
      else
        ; // skip calculated fields
    end;
  end;
end;

function TFRE_DB_Object.CopyToJSON: TFRE_DB_String;
begin
  GetAsJSONString(false,true,nil);
end;

class function TFRE_DB_Object.CreateInternalStreaming(const parent: TFRE_DB_FIELD; var mempointer: Pointer; const generate_new_uids: boolean; const version: byte; const endianmarker: byte): TFRE_DB_Object;
var   lClassnameLen,lFieldcount : TFRE_DB_SIZE_TYPE;
      lStreamingSize            : TFRE_DB_SIZE_TYPE;
      lClassname                : ShortString;
      test                      : pointer;
      lschemenamelen            : TFRE_DB_SIZE_TYPE;
      lschemename               : ShortString;
begin
  test := mempointer;
  Move (mempointer^,lFieldcount,CFRE_DB_SIZE_ENCODING_SIZE);inc(mempointer,CFRE_DB_SIZE_ENCODING_SIZE);

  Move (mempointer^,lStreamingSize,CFRE_DB_SIZE_ENCODING_SIZE);inc(mempointer,CFRE_DB_SIZE_ENCODING_SIZE);

  Move (mempointer^,lClassnameLen,CFRE_DB_SIZE_ENCODING_SIZE);inc(mempointer,CFRE_DB_SIZE_ENCODING_SIZE);
  SetLength (lClassname,lClassnameLen);
  Move (mempointer^,lClassname[1],lClassnameLen);inc(mempointer,lClassnameLen);

  if version=2 then
    begin
      Move (mempointer^,lschemenamelen,CFRE_DB_SIZE_ENCODING_SIZE);inc(mempointer,CFRE_DB_SIZE_ENCODING_SIZE);
      if lschemenamelen>=0 then begin
        SetLength (lschemename,lschemenamelen);
        Move (mempointer^,lschemename[1],lschemenamelen);inc(mempointer,lschemenamelen);
      end else begin
        lschemenamelen := -1 * lschemenamelen;
        SetLength (lschemename,lschemenamelen);
        Move (mempointer^,lschemename[1],lschemenamelen);inc(mempointer,lschemenamelen);
      end;
      if (uppercase(lClassname)<>uppercase(lschemename))
         and (lschemename<>'') then
           raise EFRE_DB_Exception.Create(edb_INTERNAL,'look here : '+lClassname+' # '+lschemename);
    end;
  result := GFRE_DB.NewObjectStreaming(lClassname);
  result.FParentDBO := parent;
  result.InternalSetup;
  result.CopyFromMem(mempointer,lFieldcount,generate_new_uids,version,endianmarker);
  //result.InternalSetup;
  result.AfterLoad;
  //result.FStreamingSize:=lStreamingSize;
  assert(result.FDBO_State=fdbos_StreamingCreating);
  result.FDBO_State:=fdbos_Clean;
  //DBGSTREAM   writeln('LOAD : ',mempointer-test);
end;

class function TFRE_DB_Object.CreateInternalStreamingJSON(const parent: TFRE_DB_FIELD; const JSON: TJSONArray;const stream_cb:TFRE_DB_StreamingCallback=nil): TFRE_DB_Object;
var   lClassname                : ShortString;
      lFieldCount               : Integer;
begin
  lClassname  := (JSON.Items[0] as TJSONString).AsString;
  lFieldCount := JSON.Count-1;
  result      := GFRE_DB.NewObjectStreaming(lClassname);
//  result.FParentDBO := parent;
  result.CopyFromJSON(JSON,lFieldCount,stream_cb);
  result.InternalSetup;
  result.AfterLoad;
  //result.FStreamingSize:=-1; // Force recalc of streamingsize
  //result.FStreamingSize:= result._StreamingSize;
  assert(result.FDBO_State=fdbos_StreamingCreating);
  result.FDBO_State:=fdbos_Clean;
end;

function TFRE_DB_Object.Field(const name: TFRE_DB_NameType): TFRE_DB_FIELD;
begin
  _InAccessibleCheck;
  {$IFDEF SANITY_CHECKS}
    assert((FDBO_State=fdbos_Clean) or (FDBO_State=fdbos_Clean));
  {$ENDIF}
  result := _Field(name);
end;

function TFRE_DB_Object.FieldOnlyExisting(const name: TFRE_DB_NameType; out fld: TFRE_DB_FIELD): boolean;
begin
  _InAccessibleCheck;
  fld    := _FieldOnlyExisting(name);
  result := assigned(fld);
end;

function TFRE_DB_Object.FieldOnlyExistingI(const name: TFRE_DB_NameType; var fld: IFRE_DB_FIELD): boolean;
var ofld : TFRE_DB_FIELD;
begin
  _InAccessibleCheck;
  ofld   := _FieldOnlyExisting(name);
  result := assigned(ofld);
  if result then
    fld := ofld
  else
    fld := nil;
end;


function TFRE_DB_Object.FieldOnlyExistingObj(const name: TFRE_DB_NameType): TFRE_DB_Object;
var fld : TFRE_DB_FIELD;
begin
  _InAccessibleCheck;
  result := nil;
  fld := _FieldOnlyExisting(name);
  if assigned(fld) and (fld.FieldType=fdbft_Object) then begin
    result := fld.AsObject;
  end;
end;

function TFRE_DB_Object.FieldOnlyExistingObjI(const name: TFRE_DB_NameType): IFRE_DB_Object;
begin
  result := FieldOnlyExistingObj(name);
end;

function TFRE_DB_Object.FieldOnlyExistingObject(const name: TFRE_DB_NameType; var obj: IFRE_DB_Object): boolean;
begin
  obj    := FieldOnlyExistingObjI(name);
  result := assigned(obj);
end;

function TFRE_DB_Object.FieldOnlyExistingObjAs(const name: TFRE_DB_NameType; const classref: TFRE_DB_BaseClass; var outobj): boolean;
var obj : TFRE_DB_Object;
begin
   obj    := FieldOnlyExistingObj(name);
   result := assigned(obj);
   if result then
     TObject(outobj) := obj as classref;
end;

function TFRE_DB_Object.FieldPath(const name: TFRE_DB_String; const dont_raise_ex: boolean; const create_non_existing: boolean): TFRE_DB_FIELD;
var fp :TFOSStringArray;
    i  : Integer;
    obj:TFRE_DB_Object;
    nam:TFRE_DB_String;
begin
  _InAccessibleCheck;
  result:=nil;
  try
    GFRE_BT.SeperateString(name,'.',fp);
    if Length(fp)>0 then begin
      obj := self;
      for i:=0 to high(fp)-1 do begin
        if (not obj.FieldExists(fp[i])) and (create_non_existing=false) then
          exit;
        obj := obj.Field(fp[i]).AsObject;
        if not assigned(obj) then
          exit;
      end;
      nam := fp[high(fp)];
      if (not obj.FieldExists(nam)) and (create_non_existing=false) then
        exit;
      result := obj.Field(nam);
    end else begin
      result := nil;
    end;
  finally
    if (not dont_raise_ex) and (not assigned(result)) then raise EFRE_DB_Exception.Create(edb_ERROR,'FIELDPATH [%s] NOT RESOLVABLE',[name]);
  end;
end;

function TFRE_DB_Object.FieldPathI(const name: TFRE_DB_String; const dont_raise_ex: boolean): IFRE_DB_FIELD;
begin
  result := FieldPath(name,dont_raise_ex);
end;

function TFRE_DB_Object.FieldPathCreate(const name: TFRE_DB_String): TFRE_DB_FIELD;
begin
  result := FieldPath(name,false,true);
end;

function TFRE_DB_Object.FieldPathCreateI(const name: TFRE_DB_String): IFRE_DB_FIELD;
begin
  result := FieldPathCreate(name);
end;

function TFRE_DB_Object.FieldPathExists(const name: TFRE_DB_String): Boolean;
var fp  : TFOSStringArray;
    i   : Integer;
    obj : TFRE_DB_Object;
    nam : TFRE_DB_String;
begin
  _InAccessibleCheck;
  result:=false;
  GFRE_BT.SeperateString(name,'.',fp);
  if Length(fp)>0 then begin
    obj := self;
    for i:=0 to high(fp)-1 do begin
      if not obj.FieldExists(fp[i]) then exit;
      obj := obj.Field(fp[i]).AsObject;
      if not assigned(obj) then exit;
    end;
    nam := fp[high(fp)];
    if not obj.FieldExists(nam) then exit;
    result := true;
  end;
end;

function TFRE_DB_Object.FieldPathListFormat(const field_list: TFRE_DB_NameTypeArray; const formats: TFRE_DB_String; const empty_val: TFRE_DB_String): TFRE_DB_String;
var outfieldname : TFRE_DB_String;
    test      : array of TVarRec;
    sa        : array of TFRE_DB_String;
    i         : integer;
    fld       : TFRE_DB_FIELD;
    fieldname : TFRE_DB_String;
begin
  _InAccessibleCheck;
  setlength(test,length(field_list));
  setlength(sa,length(field_list));
  for i:=0 to high(field_list) do begin
    fieldname := field_list[i];
    if pos('$DBTEXT:',fieldname)=1 then begin  //$DBTEXT:desc
      fieldname := GFRE_BT.SepRight(fieldname,':');
      fld := _FieldOnlyExisting(fieldname);
      if assigned(fld) and (fld.FieldType=fdbft_Object) then
        begin
          if fld.AsObject.SchemeClass='TFRE_DB_TEXT' then begin
            sa[i] := GFRE_DB.TranslateLong(fld.AsObject as TFRE_DB_TEXT);
            test[i].VAnsiString := PAnsiString(sa[i]);
            test[i].VType       := vtAnsiString;
          end ;
         end
      else
        begin
          sa[i] := '';
          test[i].VAnsiString := PAnsiString(sa[i]);
          test[i].VType       := vtAnsiString;
        end;
      continue;
    end;
    fld := FieldPath(fieldname,true);
    if assigned(fld) then begin
      try
        sa[i] := fld.AsString;
      except on e:exception do begin
        sa[i] := '{ERR:'+fieldname+'->'+e.Message+'}';
      end;end;
      test[i].VAnsiString := PAnsiString(sa[i]);
      test[i].VType   := vtAnsiString;
    end else begin
     sa[i] := empty_val;
     test[i].VAnsiString := PAnsiString(sa[i]);
     test[i].VType   := vtAnsiString;
    end;
  end;
  try
    Result :=  Format(formats,test);
  except on e:Exception do begin
    result := 'FPLF ERROR: '+e.Message;
  end;end;
end;

function TFRE_DB_Object.FieldCount(const without_calcfields, without_system_fields: boolean): SizeInt;
  procedure LocalCount(const F:TFRE_DB_FIELD);
  begin
    if f.FieldType<>fdbft_NotFound then begin
      if without_calcfields and (f.IsFieldCalculated) then
        exit;
      if without_system_fields and (f.IsSystemField) then
        exit;
      inc(result);
    end;
  end;
begin
  _InAccessibleCheck;
  result:=0;
  ForAll(@LocalCount,without_calcfields);
end;

function TFRE_DB_Object.DeleteField(const name: TFRE_DB_String): Boolean;
var lfield:TFRE_DB_FIELD;
begin
  _InAccessibleCheck;
  if _ReservedFieldName(uppercase(name)) then
    raise EFRE_DB_Exception.Create(edb_ACCESS,'reserved fields cannot be deleted');
  result := FFieldStore.Delete(uppercase(name),lfield);
  if result then begin
    lfield.Free;
  end;
end;

procedure TFRE_DB_Object.ClearAllFields;

  procedure ClearField(const fld:TFRE_DB_FIELD);
  begin
    fld.Free;
  end;

begin
  _InAccessibleCheck;
  FFieldStore.ClearItems(@ClearField);
  _RestoreReservedFields;
end;

function TFRE_DB_Object.FieldExists(const name: TFRE_DB_String): boolean;
var l_field : TFRE_DB_FIELD;
    l_ft    : TFRE_DB_FIELDTYPE;
    l_cm    : IFRE_DB_CalcMethod;
begin
  _InAccessibleCheck;
  result := FFieldStore.Find(uppercase(name),l_field);
  if result then begin
    result := l_field.FieldType<>fdbft_NotFound;
  end else begin
    result := CalcFieldExists(uppercase(name),l_ft,l_cm);
  end;
end;

procedure TFRE_DB_Object.DumpToStrings(const strings: TStrings;indent:integer=0);
var idents : TFRE_DB_String;

   procedure DumpFieldToString(const Field:TFRE_DB_FIELD);
   var oa : TFRE_DB_ObjectArray;
        i : Integer;
   begin
     if (Field.FieldType<>fdbft_Object) then
       begin
         if not Field.IsFieldCalculated then
           begin
             strings.Add(idents+Field.AsStringDump);
           end
         else
           begin
             strings.Add(idents+'CALCFIELD: '+Field.AsStringDump);
           end;
       end
     else
       begin
         if field.IsFieldCalculated then
           begin
             strings.Add(idents+'CALCFIELD: '+Field.AsStringDump);
           end
         else
           begin
             strings.Add(Format('%s%s (%s) : ',[idents,Field.FieldName,CFRE_DB_FIELDTYPE[Field.FieldType]]));
             strings.Add(idents+'{ ');
               Field.AsObject.DumpToStrings(strings,indent+2);
             strings.Add(idents+'}');
           end;
       end;
     end;

begin
  idents := StringOfChar(' ',indent);
  if not assigned(FParentDBO) then
    strings.Add(idents+'['+SchemeClass+']')
  else
    strings.Add(idents+'('+SchemeClass+')');
  ForAll(@DumpFieldToString);
end;


//TODO - use dump_length_max please
function TFRE_DB_Object.DumpToString(indent:integer;const dump_length_max:Integer):TFRE_DB_String;
var   sl:TStringlist;
    wasl:boolean;
begin
  wasl := fop_STORED_IMMUTABLE in FObjectProps;
  sl:=TStringList.Create;
  try
    if wasl then
      Set_Store_Locked(false);
    DumpToStrings(sl,indent);
    result := sl.Text;
  finally
    if wasl then
      Set_Store_Locked(true);
    sl.free;
  end;
end;

function TFRE_DB_Object.SubFormattedDisplayAvailable: boolean;
begin
  _InAccessibleCheck;
  result := false;
end;

function TFRE_DB_Object.GetSubFormattedDisplay(indent: integer): TFRE_DB_String;
begin
  _InAccessibleCheck;
  result := '';
end;

function TFRE_DB_Object.SchemeClass: TFRE_DB_NameType;
begin
  if assigned(FMediatorExtention) then
    result := uppercase(FMediatorExtention.SchemeClass)
  else
    result := UpperCase(ClassName);
end;

function TFRE_DB_Object.IsA(const schemename: shortstring): Boolean;
var
  scheme: TFRE_DB_SchemeObject;
begin
  _InAccessibleCheck;
  Result:=false;
  scheme:=GetScheme;
  if Assigned(scheme) then begin
    Result:=scheme.IsA(schemename);
  end;
end;

function TFRE_DB_Object.IsA(const IsSchemeclass: TFRE_DB_OBJECTCLASSEX; var obj): Boolean;
begin
  if assigned(FMediatorExtention) then
    if FMediatorExtention is IsSchemeclass then
      begin
        Pointer(obj) := FMediatorExtention as IsSchemeclass;
        exit(true);
      end;
  result := false;
end;

function TFRE_DB_Object.PreTransformedWasA(const schemename: shortstring): Boolean;
var fld:TFRE_DB_FIELD;
begin
  if FieldOnlyExisting(cFRE_DB_SYS_TRANS_IN_OBJ_WAS_A,fld) then
    result := fld.AsString=uppercase(schemename);
end;

function TFRE_DB_Object.PreTransformedScheme: ShortString;
var fld:TFRE_DB_FIELD;
begin
  if FieldOnlyExisting(cFRE_DB_SYS_TRANS_IN_OBJ_WAS_A,fld) then
    exit(Fld.AsString)
  else
    result := 'INVALID';
end;


procedure TFRE_DB_Object.SaveToFile(const filename: TFRE_DB_String);
var m      : TMemoryStream;
    fs     : THandleStream;
    handle : THandle;
    res    : longint;

begin
  _InAccessibleCheck;
  m:=TMemoryStream.Create;
  try
    repeat
      handle := FpOpen(pointer(FileName),O_RDWR or O_CREAT); // O_SYNC
      res    := fpgeterrno;
    until (handle<>-1) or (res<>ESysEINTR);
    if (handle<0)  then raise EFRE_DB_Exception.Create(edb_ERROR,'could not save the dbo [%d]',[res]);
    fs:=THandleStream.Create(handle);
    m.Size:=NeededSize;
    CopyToMemory(m.Memory);
    m.SaveToStream(fs);
  finally
    m.free;
    fs.Free;
    res := FpClose(handle);
  end;
end;

class function TFRE_DB_Object.CreateFromFile(const filename: TFRE_DB_String):TFRE_DB_Object;
var m        : TMemorystream;
    fs       : THandleStream;
    handle   : Thandle;
    res      : cint;
begin
  m:=TMemoryStream.Create;
  fs:=nil;
  result := nil;
  try
    repeat
      handle := FpOpen(pointer(FileName),O_RDONLY); // O_SYNC
      res    := fpgeterrno;
    until (handle<>-1) or (res<>ESysEINTR);
    if (handle<0) then raise EFRE_DB_Exception.Create(edb_ERROR,'could not find the dbo [%d] [%s]',[res,filename]);
    fs:=THandleStream.Create(handle);
    m.LoadFromStream(fs);
    if m.Size<Sizeof(CFRE_DB_ObjectHdr) then begin
      raise EFRE_DB_Exception.Create(edb_ERROR,'invalid dbo file [%s size=%d]',[filename,m.size]);
    end else begin
      result:= TFRE_DB_Object.CreateFromMemory(m.Memory);
    end;
  finally
    fs.free;
    m.free;
    if handle>0 then begin
      FpClose(handle);
    end;
  end;
end;

function TFRE_DB_Object.CloneToNewObject(const generate_new_uids: boolean): TFRE_DB_Object;
var a  : Array [0..4095] of Byte;
    m  : Pointer;
    ns : NativeInt;
begin
  ns := self.NeededSize;
  if ns>SizeOf(a) then
    begin
      m := Getmem(ns);
      CopyToMemory(m);
      result := TFRE_DB_Object.CreateFromMemory(m,generate_new_uids);
      Freemem(m);
    end
  else
    begin
      m := @a;
      CopyToMemory(m);
      result := TFRE_DB_Object.CreateFromMemory(m,generate_new_uids);
    end;
end;

//var s:string;
//begin
//  //_InAccessibleCheck;
//  result := TFRE_DB_Object.CreateFromString(self.AsString,generate_new_uids);
//  //if Assigned(FMediatorExtention) and
//  //   not assigned(result.FMediatorExtention) then
//  //     GFRE_BT.CriticalAbort('INTERNAL CLONE FAILURE');
//end;

function TFRE_DB_Object.CloneToNewObjectI(const generate_new_uids: boolean): IFRE_DB_Object;
begin
  result := CloneToNewObject(generate_new_uids);
end;

function TFRE_DB_Object.ReferencesObjectsFromData: Boolean;
var search:boolean;
  function SearchObjectLinkField(const fld:TFRE_DB_FIELD):boolean;
  begin
    if fld.FieldType = fdbft_ObjLink then begin
      if fld.ValueCount>0 then begin
        search:=true;
        exit(true);
      end;
    end;
    result:=false;
  end;
begin
  _InAccessibleCheck;
  search := false;
  ForAllBrk(@SearchObjectLinkField);
  result := search;
end;


function TFRE_DB_Object.ReferencesFromData: TFRE_DB_ObjectReferences;
var cnt  : NativeInt;
    mysn : TFRE_DB_NameType;
    i    : NAtiveInt;

  procedure SearchObjectLinkField(const fld:TFRE_DB_FIELD);
  var i    : NativeInt;
      myfn : TFRE_DB_NameType;
  begin
    if fld.FieldType = fdbft_ObjLink then
      begin
        myfn := uppercase(fld.FieldName);
        for i:=0 to fld.ValueCount-1 do
          begin
            if cnt=Length(result) then
              SetLength(result,Length(result)+25);
            result[cnt].fieldname  := myfn;
            result[cnt].schemename := '?';
            result[cnt].linked_uid := fld.AsObjectLinkArray[i];
            inc(cnt);
          end;
      end;
  end;

begin
  _InAccessibleCheck;
  cnt  := 0;
  mysn := SchemeClass;
  ForAll(@SearchObjectLinkField);
  SetLength(result,cnt);
  for i:=0 to high(Result) do
    if Result[i].linked_uid=CFRE_DB_NullGUID then
      raise EFRE_DB_Exception.Create(edb_ERROR,'the object contains NULL GUIDS in objectlink fields this is not allowed');
end;


function TFRE_DB_Object.IsSystem: Boolean;
begin
 result := fop_SYSTEM in FObjectProps;
end;

function TFRE_DB_Object.IsSystemDB: Boolean;
begin
 result := fop_IN_SYSTEM_DB in FObjectProps;
end;

function TFRE_DB_Object.IsVolatile: Boolean;
begin
 result := fop_VOLATILE in FObjectProps;
end;

function TFRE_DB_Object.GetFieldListFilter(const field_type: TFRE_DB_FIELDTYPE): TFRE_DB_StringArray;
var cnt,i:NativeInt;
    procedure Gather(const fld:TFRE_DB_FIELD);
    begin
      if fld.FieldType=field_type then begin
        result[cnt] := fld.FieldName;
        inc(cnt);
      end;
    end;
begin
  _InAccessibleCheck;
  SetLength(Result,FieldCount(false,false));
  cnt:=0;
  ForAllFields(@gather);
  SetLength(Result,cnt);
end;

function TFRE_DB_Object.GetUIDPath: TFRE_DB_StringArray;
var i       : NativeInt;
    cnt     : NativeInt;
    lparent : TFRE_DB_Object;
begin
  cnt:=0;
  lparent := Parent;
  while assigned(lparent) do
    begin
      inc(cnt);
      lparent := lparent.Parent;
    end;
  SetLength(result,cnt+1);
  lparent := self;
  for i := cnt downto 0 do
    begin
      Result[i] := lparent.UID_String;
      lparent   := lparent.Parent;
    end;
end;


function TFRE_DB_Object.GetSchemePath: TFRE_DB_StringArray;
var i       : NativeInt;
    cnt     : NativeInt;
    lparent : TFRE_DB_Object;
begin
  cnt:=0;
  lparent := Parent;
  while assigned(lparent) do
    begin
      inc(cnt);
      lparent := lparent.Parent;
    end;
  SetLength(result,cnt+1);
  lparent := self;
  for i := cnt downto 0 do
    begin
      Result[i] := lparent.SchemeClass;
      lparent   := lparent.Parent;
    end;
end;

function TFRE_DB_Object.GetUIDPathUA: TFRE_DB_GUIDArray;
var i       : NativeInt;
    cnt     : NativeInt;
    lparent : TFRE_DB_Object;
begin
  cnt:=0;
  lparent := Parent;
  while assigned(lparent) do
    begin
      inc(cnt);
      lparent := lparent.Parent;
    end;
  SetLength(result,cnt+1);
  lparent := self;
  for i := cnt downto 0 do
    begin
      Result[i] := lparent.UID;
      lparent   := lparent.Parent;
    end;
end;

function TFRE_DB_Object.Mediator: TFRE_DB_ObjectEx;
begin
  _InAccessibleCheck;
  result := FMediatorExtention;
end;

function TFRE_DB_Object.Properties: TFRE_DB_Object_PropertySet;
begin
  _InAccessibleCheck;
  result := FObjectProps;
end;

procedure TFRE_DB_Object.CopyField(const obj: TFRE_DB_Object; const field_name: String);
begin
  _InAccessibleCheck;
  Field(field_name).CloneFromField(obj.Field(field_name));
end;

procedure TFRE_DB_Object.CopyFieldI(const obj: IFRE_DB_Object; const field_name: String);
begin
  CopyField(obj.Implementor as TFRE_DB_Object,field_name);
end;

function TFRE_DB_Object.ForAllObjectsBreakHierarchicI(const iter: IFRE_DB_ObjectIteratorBrk): boolean;

  procedure Iterat(const obj:TFRE_DB_Object; var halt:boolean);
  begin
    iter(obj,halt)
  end;

begin
  result := ForAllObjectsBreakHierarchic(@iterat);
end;

function TFRE_DB_Object.GetFullHierarchicObjectList(const include_self: boolean): TFRE_DB_ObjectArray;
var cnt  : NativeInt;
    skip : Boolean;

  procedure BuildList(const obj : TFRE_DB_Object ; var halt : boolean);
  begin
    if not skip then
      begin
        skip := false;
        if Length(result)=cnt then
          SetLength(result,Length(result)+25);
        result[cnt] := obj;
        inc(cnt);
      end;
  end;

begin
  cnt := 0;
  skip := not include_self;
  ForAllObjectsBreakHierarchic(@BuildList);
  SetLength(result,cnt);
end;


function TFRE_DB_Object.FetchObjByUID(const childuid: TFRE_DB_GUID): TFRE_DB_Object;

  procedure SearchChild(const obj:TFRE_DB_Object; var halt:boolean);
  begin
    if obj.UID=childuid then
      begin
        result := obj;
        halt   := true;
      end;
  end;

begin
  result := nil;
  ForAllObjectsBreakHierarchic(@SearchChild);
end;

function TFRE_DB_Object.FetchObjByUIDI(const childuid: TFRE_DB_GUID; var obj: IFRE_DB_Object): boolean;
begin
  obj    := FetchObjByUID(childuid);
  result := assigned(obj);
end;

function TFRE_DB_Object.FetchObjWithStringFieldValue(const field_name: TFRE_DB_NameType; const fieldvalue: TFRE_DB_String; var obj: IFRE_DB_Object; ClassnameToMatch: ShortString): boolean;

  procedure SearchChild(const searchobj:TFRE_DB_Object; var halt:boolean);
  var fld : IFRE_DB_FIELD;
  begin
    if (ClassnameToMatch<>'') and
       (uppercase(searchobj.SchemeClass)<>ClassnameToMatch) then
         exit;
    if searchobj.FieldOnlyExistingI(field_name,fld) and
       (fld.FieldType=fdbft_String) then
         if fld.AsString=fieldvalue then
           begin
             obj    := searchobj;
             halt   := true;
           end;
  end;

begin
  obj := nil;
  ClassnameToMatch := uppercase(ClassnameToMatch);
  ForAllObjectsBreakHierarchic(@SearchChild);
  result := assigned(obj);
end;

procedure TFRE_DB_Object.SetAllSimpleObjectFieldsFromObject(const source_object: IFRE_DB_Object);

  procedure iterat(const fld:IFRE_DB_Field);
  begin
    case fld.FieldType of
      fdbft_Object: ; //skip
      fdbft_ObjLink,
      fdbft_GUID:
        begin
          if fld.IsUIDField then
            exit;
          Field(fld.FieldName).CloneFromFieldI(fld);
        end;
      fdbft_Byte,
      fdbft_Int16,
      fdbft_UInt16,
      fdbft_Int32,
      fdbft_UInt32,
      fdbft_Int64,
      fdbft_UInt64,
      fdbft_Real32,
      fdbft_Real64,
      fdbft_Currency,
      fdbft_String,
      fdbft_Boolean,
      fdbft_DateTimeUTC,
      fdbft_Stream:
        begin
          Field(fld.FieldName).CloneFromFieldI(fld)
        end;
      else
        raise EFRE_DB_Exception.Create(edb_ERROR,'setsimpleobjectfieldsfromobject - not all fieldtypes handled');
    end;
  end;

begin
  source_object.ForAllFields(@iterat);
end;

function TFRE_DB_Object.GetAsJSON(const without_reserved_fields: boolean; const full_dump: boolean; const stream_cb: TFRE_DB_StreamingCallback): TJSONData;
var ro:TJSONObject;
    ra:TJSONArray;
    i : integer;
  procedure ExportField(const Field:TFRE_DB_FIELD);
  begin
    if without_reserved_fields then begin
      if _ReservedFieldName(uppercase(field.FieldName)) then
        exit;
    end;
    ro.Add(lowercase(Field.FieldName),Field.GetAsJSON(without_reserved_fields,false,stream_cb));
    //ro.Add(Field.FieldName,Field.GetAsJSON(without_reserved_fields,false,stream_cb));
  end;
  procedure ExportFieldFD(const Field:TFRE_DB_FIELD);
  begin
    if without_reserved_fields then begin
      if _ReservedFieldName(uppercase(field.FieldName)) then
        exit;
    end;
    ra.Add(Field.GetAsJSON(without_reserved_fields,true,stream_cb));
  end;

begin
  _InAccessibleCheck;
  i := 0;
  if not full_dump then  begin
    ro:=TJSONObject.Create;
    ForAll(@ExportField);
    result:=ro;
  end else begin
    BeforeSave;
    ra := TJSONArray.Create;
    ra.Add(TJSONString.Create(SchemeClass));
    ForAll(@ExportFieldFD);
    result := ra;
    AfterSave;
  end;
end;

function TFRE_DB_Object.GetAsJSONString(const without_reserved_fields: boolean; const full_dump: boolean; const stream_cb: TFRE_DB_StreamingCallback): TFRE_DB_String;
var jd : TJSONData;
begin
  _InAccessibleCheck;
  jd := GetAsJSON(without_reserved_fields,full_dump,stream_cb);
  try
    result := jd.AsJSON;
  finally
    jd.free;
  end;
end;

{ TFRE_DB_FIELD }

procedure TFRE_DB_FIELD.SetAsObject(const AValue: TFRE_DB_Object);
begin
  _InAccessibleFieldCheck;
  Fobj._ParentCheck(Avalue);
  if not Avalue.IsObjectRoot then
    raise EFRE_DB_Exception.Create(edb_ERROR,'its not allowed to set child objects as subobject');
  if not _CheckStoreType(fdbft_Object) then begin
    FFieldData.FieldType := fdbft_Object;
  end else begin
    FFieldData.obj.Free;
  end;
  FFieldData.obj:=AValue;
  AValue.FParentDBO := self;
end;

function TFRE_DB_FIELD.GetAsByteArray: TFRE_DB_ByteArray;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Byte);
  result := FFieldData.byte^;
end;

function TFRE_DB_FIELD.GetAsByteList(idx: Integer): Byte;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_Byte);
  _CheckIndex(idx);
  result := FFieldData.byte^[idx];
end;


function TFRE_DB_FIELD.GetAsInt16Array: TFRE_DB_Int16Array;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Int16);
  result := FFieldData.in16^;
end;

function TFRE_DB_FIELD.GetAsInt16List(idx: Integer): Smallint;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_Int16);
  _CheckIndex(idx);
  result := FFieldData.in16^[idx];
end;

function TFRE_DB_FIELD.GetAsInt32Array: TFRE_DB_Int32Array;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Int32);
  result := FFieldData.in32^;
end;

function TFRE_DB_FIELD.GetAsInt32List(idx: Integer): longint;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_Int32);
  _CheckIndex(idx);
  result := FFieldData.in32^[idx];
end;

function TFRE_DB_FIELD.GetAsInt64Array: TFRE_DB_Int64Array;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Int64);
  result := FFieldData.in64^;
end;

function TFRE_DB_FIELD.GetAsInt64List(idx: Integer): int64;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_Int64);
  _CheckIndex(idx);
  result := FFieldData.in64^[idx];
end;

function TFRE_DB_FIELD.GetAsSingleArray: TFRE_DB_Real32Array;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Real32);
  result := FFieldData.re32^;
end;

function TFRE_DB_FIELD.GetAsSingleList(idx: Integer): Single;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_Real32);
  _CheckIndex(idx);
  result := FFieldData.re32^[idx];
end;

function TFRE_DB_FIELD.GetAsUInt16Array: TFRE_DB_UInt16Array;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_UInt16);
  result := FFieldData.ui16^;
end;

function TFRE_DB_FIELD.GetAsUInt16List(idx: Integer): Word;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_UInt16);
  _CheckIndex(idx);
  result := FFieldData.ui16^[idx];
end;

function TFRE_DB_FIELD.GetAsUInt32Array: TFRE_DB_UInt32Array;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_UInt32);
  result := FFieldData.ui32^;
end;

function TFRE_DB_FIELD.GetAsUInt32List(idx: Integer): longword;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_UInt32);
  _CheckIndex(idx);
  result := FFieldData.ui32^[idx];
end;

function TFRE_DB_FIELD.GetAsUInt64Array: TFRE_DB_UInt64Array;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_UInt64);
  result := FFieldData.ui64^;
end;

function TFRE_DB_FIELD.GetAsUInt64List(idx: Integer): uint64;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_UInt64);
  _CheckIndex(idx);
  result := FFieldData.ui64^[idx];
end;

procedure TFRE_DB_FIELD.SetAsByteArray(const AValue: TFRE_DB_ByteArray);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Byte) then begin
    FFieldData.FieldType := fdbft_Byte;
    New(FFieldData.byte);
  end;
  FFieldData.byte^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsByteList(idx: Integer; const AValue: Byte);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Byte) then begin
    New(FFieldData.byte);
  end;
  _CheckIndex(idx);
  FFieldData.byte^[idx] := AValue;
end;


procedure TFRE_DB_FIELD.SetAsDateTimeUTCList(idx: Integer; const AValue: TFRE_DB_DateTime64);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_DateTimeUTC) then begin
    New(FFieldData.date);
  end;
  _CheckIndex(idx);
  FFieldData.date^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt16Array(const AValue: TFRE_DB_Int16Array);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Int16) then begin
    FFieldData.FieldType := fdbft_Int16;
    New(FFieldData.in16);
  end;
  FFieldData.in16^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt16List(idx: Integer; const AValue: Smallint);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Int16) then begin
    New(FFieldData.in16);
  end;
  _CheckIndex(idx);
  FFieldData.in16^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt32Array(const AValue: TFRE_DB_Int32Array);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Int32) then begin
    FFieldData.FieldType := fdbft_Int32;
    New(FFieldData.in32);
  end;
  FFieldData.in32^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt32List(idx: Integer; const AValue: longint);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Int32) then begin
    New(FFieldData.in32);
  end;
  _CheckIndex(idx);
  FFieldData.in32^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt64Array(const AValue: TFRE_DB_Int64Array);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Int64) then begin
    FFieldData.FieldType := fdbft_Int64;
    New(FFieldData.in64);
  end;
  FFieldData.in64^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt64List(idx: Integer; const AValue: int64);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Int64) then begin
    New(FFieldData.in64);
  end;
  _CheckIndex(idx);
  FFieldData.in64^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsSingleArray(const AValue: TFRE_DB_Real32Array);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Real32) then begin
    FFieldData.FieldType := fdbft_Real32;
    New(FFieldData.re32);
  end;
  FFieldData.re32^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsSingleList(idx: Integer; const AValue: Single);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Real32) then begin
    New(FFieldData.re32);
  end;
  _CheckIndex(idx);
  FFieldData.re32^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt16Array(const AValue: TFRE_DB_UInt16Array);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_UInt16) then begin
    FFieldData.FieldType := fdbft_UInt16;
    New(FFieldData.ui16);
  end;
  FFieldData.ui16^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt16List(idx: Integer; const AValue: Word);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_UInt16) then begin
    New(FFieldData.ui16);
  end;
  _CheckIndex(idx);
  FFieldData.ui16^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt32Array(const AValue: TFRE_DB_UInt32Array);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_UInt32) then begin
    FFieldData.FieldType := fdbft_UInt32;
    New(FFieldData.ui32);
  end;
  FFieldData.ui32^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt32List(idx: Integer; const AValue: longword);
begin
   _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_UInt32) then begin
    New(FFieldData.ui32);
  end;
  _CheckIndex(idx);
  FFieldData.ui32^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt64Array(const AValue: TFRE_DB_UInt64Array);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_UInt64) then begin
    FFieldData.FieldType := fdbft_UInt64;
    New(FFieldData.ui64);
  end;
  FFieldData.ui64^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt64List(idx: Integer; const AValue: uint64);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_UInt64) then begin
    New(FFieldData.ui64);
  end;
  _CheckIndex(idx);
  FFieldData.ui64^[idx] := AValue;
end;

function TFRE_DB_FIELD.GetAsByte: Byte;
begin
  _InAccessibleFieldCheck;
  if FFieldData.FieldType = fdbft_Byte then begin
    _CheckEmptyArray;
    result := FFieldData.byte^[0];
  end else begin
    result := _ConvertToByte;
  end;
end;

function TFRE_DB_FIELD.GetAsInt16: Smallint;
begin
  _InAccessibleFieldCheck;
  if FFieldData.FieldType = fdbft_Int16 then begin
    _CheckEmptyArray;
    result := FFieldData.in16^[0];
  end else begin
    result := _ConvertToInt16;
  end;
end;

function TFRE_DB_FIELD.GetAsInt32: longint;
begin
  _InAccessibleFieldCheck;
  if FFieldData.FieldType = fdbft_Int32 then begin
    _CheckEmptyArray;
    result := FFieldData.in32^[0];
  end else begin
    result := _ConvertToInt32;
  end;
end;

function TFRE_DB_FIELD.GetAsInt64: int64;
begin
  _InAccessibleFieldCheck;
  if FFieldData.FieldType = fdbft_Int64 then begin
    _CheckEmptyArray;
    result := FFieldData.in64^[0];
  end else begin
    result := _ConvertToInt64;
  end;
end;

function TFRE_DB_FIELD.GetAsSingle: Single;
begin
  _InAccessibleFieldCheck;
  if FFieldData.FieldType = fdbft_Real32 then begin
    _CheckEmptyArray;
    result := FFieldData.re32^[0];
  end else begin
    result := _ConvertToSingle;
  end;
end;

function TFRE_DB_FIELD.GetAsUInt16: Word;
begin
  _InAccessibleFieldCheck;
  if FFieldData.FieldType = fdbft_UInt16 then begin
    _CheckEmptyArray;
    result := FFieldData.ui16^[0];
  end else begin
    result := _ConvertToUInt16;
  end;
end;

function TFRE_DB_FIELD.GetAsUInt32: longword;
begin
  _InAccessibleFieldCheck;
  if FFieldData.FieldType = fdbft_UInt32 then begin
    _CheckEmptyArray;
    result := FFieldData.ui32^[0];
  end else begin
    result := _ConvertToUInt32;
  end;
end;

function TFRE_DB_FIELD.GetAsUInt64: uint64;
begin
  _InAccessibleFieldCheck;
  if FFieldData.FieldType = fdbft_UInt64 then begin
    _CheckEmptyArray;
    result := FFieldData.ui64^[0];
  end else begin
    result := _ConvertToUInt64;
  end;
end;

procedure TFRE_DB_FIELD.SetAsByte(const AValue: Byte);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Byte) then begin
    FFieldData.FieldType := fdbft_Byte;
    New(FFieldData.byte);
    SetLength(FFieldData.byte^,1);
  end;
  FFieldData.byte^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt16(const AValue: Smallint);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Int16) then begin
    FFieldData.FieldType := fdbft_Int16;
    New(FFieldData.in16);
    SetLength(FFieldData.in16^,1);
  end;
  FFieldData.in16^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt32(const AValue: longint);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Int32) then begin
    FFieldData.FieldType := fdbft_Int32;
    New(FFieldData.in32);
    SetLength(FFieldData.in32^,1);
  end;
  FFieldData.in32^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt64(const AValue: int64);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Int64) then begin
    FFieldData.FieldType := fdbft_Int64;
    New(FFieldData.in64);
    SetLength(FFieldData.in64^,1);
  end;
  FFieldData.in64^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsSingle(const AValue: Single);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Real32) then begin
    FFieldData.FieldType := fdbft_Real32;
    New(FFieldData.re32);
    SetLength(FFieldData.re32^,1);
  end;
  FFieldData.re32^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt16(const AValue: Word);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_UInt16) then begin
    FFieldData.FieldType := fdbft_UInt16;
    New(FFieldData.ui16);
    SetLength(FFieldData.ui16^,1);
  end;
  FFieldData.ui16^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt32(const AValue: longword);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_UInt32) then begin
    FFieldData.FieldType := fdbft_UInt32;
    New(FFieldData.ui32);
    SetLength(FFieldData.ui32^,1);
  end;
  FFieldData.ui32^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt64(const AValue: uint64);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_UInt64) then begin
    FFieldData.FieldType := fdbft_UInt64;
    New(FFieldData.ui64);
    SetLength(FFieldData.ui64^,1);
  end;
  FFieldData.ui64^[0]:=AValue;
end;

function TFRE_DB_FIELD._StreamingSize: TFRE_DB_SIZE_TYPE; //Streamed as FieldType,FieldValCount,FieldNameSize,{FieldName},{FieldData}

  function __CalcStringLengths: TFRE_DB_SIZE_TYPE;
  var i: Integer;
  begin
    result := CFRE_DB_SIZE_ENCODING_SIZE * Length(FFieldData.strg^);
    for i := 0 to Length(FFieldData.strg^)-1 do begin
      result := result + TFRE_DB_SIZE_TYPE(Length(FFieldData.strg^[i]));
    end;
  end;

 function __CalcStreamLengths: TFRE_DB_SIZE_TYPE;
  var i: Integer;
  begin
    result := CFRE_DB_SIZE_ENCODING_SIZE *  Length(FFieldData.strm^);
    for i := 0 to Length(FFieldData.strm^)-1 do begin
      result := result + FFieldData.strm^[i].Size;
    end;
  end;

 function __CalcObjectLengths: TFRE_DB_SIZE_TYPE;
 var i: Integer;
 begin
   result := 2 * CFRE_DB_SIZE_ENCODING_SIZE * 1; // Fieldcount per Object + Streamsize per object
   result := result + FFieldData.obj._StreamingSize;
 end;

 function __HeaderSize: TFRE_DB_SIZE_TYPE;
 begin
   result := 3*CFRE_DB_SIZE_ENCODING_SIZE+length(FFieldName^);
 end;

begin
  //if FFieldStreamSize<>-1 then begin
  //  exit(FFieldStreamSize);
  //end;
  if IsFieldCalculated then
    exit(0);
  if IsSchemeField then
    exit(0);
  case FFieldData.FieldType of
    fdbft_NotFound:    result := 0;
    fdbft_GUID:        result := __HeaderSize+TFRE_DB_SIZE_TYPE(SizeOf(FFieldData.guid^[0]) * length(FFieldData.guid^));
    fdbft_Byte:        result := __HeaderSize+TFRE_DB_SIZE_TYPE(SizeOf(FFieldData.byte^[0]) * length(FFieldData.byte^));
    fdbft_Int16:       result := __HeaderSize+TFRE_DB_SIZE_TYPE(SizeOf(FFieldData.in16^[0]) * length(FFieldData.in16^));
    fdbft_UInt16:      result := __HeaderSize+TFRE_DB_SIZE_TYPE(SizeOf(FFieldData.ui16^[0]) * length(FFieldData.ui16^));
    fdbft_Int32:       result := __HeaderSize+TFRE_DB_SIZE_TYPE(SizeOf(FFieldData.in32^[0]) * length(FFieldData.in32^));
    fdbft_UInt32:      result := __HeaderSize+TFRE_DB_SIZE_TYPE(SizeOf(FFieldData.ui32^[0]) * length(FFieldData.ui32^));
    fdbft_Int64:       result := __HeaderSize+TFRE_DB_SIZE_TYPE(SizeOf(FFieldData.in64^[0]) * length(FFieldData.in64^));
    fdbft_UInt64:      result := __HeaderSize+TFRE_DB_SIZE_TYPE(SizeOf(FFieldData.ui64^[0]) * length(FFieldData.ui64^));
    fdbft_Real32:      result := __HeaderSize+TFRE_DB_SIZE_TYPE(SizeOf(FFieldData.re32^[0]) * length(FFieldData.re32^));
    fdbft_Real64:      result := __HeaderSize+TFRE_DB_SIZE_TYPE(SizeOf(FFieldData.re64^[0]) * length(FFieldData.re64^));
    fdbft_Currency:    result := __HeaderSize+TFRE_DB_SIZE_TYPE(SizeOf(FFieldData.curr^[0]) * length(FFieldData.curr^));
    fdbft_String:      result := __HeaderSize+__CalcStringLengths;
    fdbft_Boolean:     result := __HeaderSize+TFRE_DB_SIZE_TYPE(SizeOf(FFieldData.bool^[0]) * length(FFieldData.bool^));
    fdbft_DateTimeUTC: result := __HeaderSize+TFRE_DB_SIZE_TYPE(SizeOf(FFieldData.date^[0]) * length(FFieldData.date^));
    fdbft_Stream:      result := __HeaderSize+__CalcStreamLengths;
    fdbft_Object:      result := __HeaderSize+__CalcObjectLengths;
    fdbft_ObjLink:     result := __HeaderSize+TFRE_DB_SIZE_TYPE(SizeOf(FFieldData.obl^[0]) * length(FFieldData.obl^));
  end;
  //FFieldStreamSize:=result;
  //writeln('***** StrSize TAKE > ',Fobj.ClassName,' ',FFieldName^,' : ',FStreamSize);
end;

function TFRE_DB_FIELD.GetAsBoolean: Boolean;
begin
  _InAccessibleFieldCheck;
  if FFieldData.FieldType = fdbft_Boolean then begin
    _CheckEmptyArray;
    result := FFieldData.bool^[0];
  end else begin
    result := _ConvertToBool;
  end;
end;

function TFRE_DB_FIELD.GetAsBooleanArray: TFRE_DB_BoolArray;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Boolean);
  result := FFieldData.bool^;
end;

function TFRE_DB_FIELD.GetAsBooleanList(idx: Integer): Boolean;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_Boolean);
  _CheckIndex(idx);
  result := FFieldData.bool^[idx];
end;

procedure TFRE_DB_FIELD.SetAsBoolean(const AValue: Boolean);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Boolean) then begin
    FFieldData.FieldType := fdbft_Boolean;
    New(FFieldData.bool);
    SetLength(FFieldData.bool^,1);
  end;
  FFieldData.bool^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsObjectLink(const AValue: TFRE_DB_GUID);
var old_array,new_array : TFRE_DB_GUIDArray;
begin
  _InAccessibleFieldCheck;
  if FREDB_Guids_Same(AValue,Fobj.UID) then raise EFRE_DB_Exception.Create(edb_ERROR,'referencing a objectlink to self seems to be sensless');
  if not _CheckStoreType(fdbft_ObjLink) then begin
    FFieldData.FieldType := fdbft_ObjLink;
    New(FFieldData.obl);
    SetLength(FFieldData.obl^,1);
  end else begin
    old_array := FFieldData.obl^;
    SetLength(new_array,1);
    new_array[0] := AValue;
  end;
  FFieldData.obl^[0]:=AValue; // set new value
end;

procedure TFRE_DB_FIELD.SetAsBooleanArray(const AValue: TFRE_DB_BoolArray);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Boolean) then begin
    FFieldData.FieldType := fdbft_Boolean;
    New(FFieldData.bool);
  end;
  FFieldData.bool^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsObjectLinkArray(const Avalue: TFRE_DB_ObjLinkArray);
var old_array : TFRE_DB_GUIDArray;
    i         : integer;
begin
  _InAccessibleFieldCheck;
  if not FREDB_CheckGuidsUnique(Avalue) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'objlinks : references are not unique');
  for i:=0 to high(Avalue) do begin
    if FREDB_Guids_Same(AValue[i],Fobj.UID) then raise EFRE_DB_Exception.Create(edb_ERROR,'objlinks : referencing a objectlink to self seems to be senseless');
  end;
  if not _CheckStoreType(fdbft_ObjLink) then begin
    FFieldData.FieldType := fdbft_ObjLink;
    New(FFieldData.obl);
  end;
  FFieldData.obl^ := Copy(AValue);
end;


procedure TFRE_DB_FIELD.SetAsBooleanList(idx: Integer; const AValue: Boolean);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Boolean) then begin
    New(FFieldData.bool);
  end;
  _CheckIndex(idx);
  FFieldData.bool^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsObjectLinkList(idx: Integer; const AValue: TFRE_DB_GUID); //TODO Check - Count always right ? / new
var new_fld:boolean;
    old_array,new_array:TFRE_DB_GUIDArray;
begin
  _InAccessibleFieldCheck;
  if FREDB_Guids_Same(AValue,Fobj.UID) then raise EFRE_DB_Exception.Create(edb_ERROR,'referencing a objectlink to self seems to be senseless');
  if not _CheckStoreType(fdbft_ObjLink) then begin
    New(FFieldData.obl);
    new_fld:=true;
  end;
  _CheckIndex(idx);
  FFieldData.obl^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.CalculateValue;
begin
  if not assigned(FCalcMethod) then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'calcfield but no function assigned?');
  TMethod(FCalcMethod).Data:=Fobj.Implementor_HC;
  try
    FCalcMethod(self);
  except
    on E:Exception do
      begin
        GFRE_DBI.LogError(dblc_APPLICATION,'calculate value of field ['+FieldName+'] failed for : '+e.Message);
      end;
  end;
end;

procedure TFRE_DB_FIELD.SetAsEmptyStringArray;
var sa:TFRE_DB_StringArray;
begin
  _InAccessibleFieldCheck;
  SetLength(sa,0);
  SetAsStringArray(sa);
end;

function TFRE_DB_FIELD.IsEmptyArray: boolean;
begin
  _InAccessibleFieldCheck;
  result := (FieldType<>fdbft_NotFound) and (ValueCount=0);
end;

function TFRE_DB_FIELD.CompareToFieldShallow(const cmp_fld: TFRE_DB_FIELD): boolean;
var i:NativeInt;
begin
  assert(uppercase(FieldName)=uppercase(cmp_fld.FieldName));
  if FieldType<>cmp_fld.FieldType then
    exit(false);
  if ValueCount<>cmp_fld.ValueCount then
    exit(false);
  for i:=0 to ValueCount-1 do
    begin
      case FieldType of
        fdbft_NotFound: ;// ignore
        fdbft_GUID:
            if not FREDB_Guids_Same(FFieldData.guid^[i],cmp_fld.FFieldData.guid^[i]) then
              exit(false);
        fdbft_Byte:
            if not (FFieldData.byte^[i] = cmp_fld.FFieldData.byte^[i]) then
              exit(false);
        fdbft_Int16:
            if not (FFieldData.in16^[i] = cmp_fld.FFieldData.in16^[i]) then
              exit(false);
        fdbft_UInt16:
          if not (FFieldData.ui16^[i] = cmp_fld.FFieldData.ui16^[i]) then
            exit(false);
        fdbft_Int32:
          if not (FFieldData.in32^[i] = cmp_fld.FFieldData.in32^[i]) then
            exit(false);
        fdbft_UInt32:
          if not (FFieldData.ui32^[i] = cmp_fld.FFieldData.ui32^[i]) then
            exit(false);
        fdbft_Int64:
          if not (FFieldData.in64^[i] = cmp_fld.FFieldData.in64^[i]) then
            exit(false);
        fdbft_UInt64:
          if not (FFieldData.ui64^[i] = cmp_fld.FFieldData.ui64^[i]) then
            exit(false);
        fdbft_Real32:
          if not SameValue(FFieldData.re32^[i],cmp_fld.FFieldData.re32^[i]) then
            exit(false);
        fdbft_Real64:
          if not SameValue(FFieldData.re64^[i],cmp_fld.FFieldData.re64^[i]) then
            exit(false);
        fdbft_Currency:
          if not (FFieldData.curr^[i] = cmp_fld.FFieldData.curr^[i]) then
            exit(false);
        fdbft_String:
          begin
            if not  FIsSchemeField then
              begin
                if not (FFieldData.strg^[i] = cmp_fld.FFieldData.strg^[i]) then
                  exit(false);
              end
            else
              begin { also cmp_fld is Schemeclassfield (same name) }
                if _SchemeClassOfParent<>cmp_fld._SchemeClassOfParent then
                  exit(false);
              end;
          end;
        fdbft_Boolean:
          if not (FFieldData.bool^[i] = cmp_fld.FFieldData.bool^[i]) then
            exit(false);
        fdbft_DateTimeUTC:
          if not (FFieldData.date^[i] = cmp_fld.FFieldData.date^[i]) then
            exit(false);
        fdbft_Stream:
          begin
            if FFieldData.strm^[i].Size<>cmp_fld.FFieldData.strm^[i].Size then
              exit(false);
            if not CompareMem(FFieldData.strm^[i].Memory,cmp_fld.FFieldData.strm^[i].Memory,FFieldData.strm^[i].size) then
              exit(false);
          end;
        fdbft_Object:
          if not FREDB_Guids_Same(FFieldData.obj.UID,cmp_fld.FFieldData.obj.UID) then
            exit(false);
        fdbft_ObjLink:
          if not FREDB_Guids_Same(FFieldData.obl^[i],cmp_fld.FFieldData.obl^[i]) then
            exit(false);
        else raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled '+CFRE_DB_FIELDTYPE[FFieldData.FieldType]);
      end;
    end;
  result := true;
end;

procedure TFRE_DB_FIELD.IntfCast(const InterfaceSpec: ShortString; out Intf);
begin
  _InAccessibleFieldCheck;
  AsObject.IntfCast(InterfaceSpec,intf);
end;

function TFRE_DB_FIELD.AsDBText: IFRE_DB_TEXT;
begin
  _InAccessibleFieldCheck;
  IntfCast(IFRE_DB_TEXT,result);
end;

function TFRE_DB_FIELD.IsSpecialClearMarked: Boolean;
begin
 if FIsSchemeField then
   exit(False);;
 result:= (_FieldType=fdbft_String) and
          (FFieldData.strg^[0]=cFRE_DB_SYS_CLEAR_VAL_STR);
end;

function TFRE_DB_FIELD.ConvAsSignedArray: TFRE_DB_Int64Array;
begin
  result := _ConvertToSignedArray;
end;

function TFRE_DB_FIELD.ConvAsUnsignedArray: TFRE_DB_UInt64Array;
begin
 result := _ConvertToUnsignedArray;
end;

function TFRE_DB_FIELD.ConvAsCurrencyArray: TFRE_DB_CurrencyArray;
begin
  result := _ConvertToCurrencyArray;
end;

function TFRE_DB_FIELD.ConvAsReal64Array: TFRE_DB_Real64Array;
begin
  result := _ConvertToReal64Array;
end;

function TFRE_DB_FIELD._FieldType: TFRE_DB_FIELDTYPE;
begin
  result := FFieldData.FieldType;
end;

function TFRE_DB_FIELD.CloneToNewStreamable: IFRE_DB_Field;
var new_fld : TFRE_DB_FIELD;
begin
  new_fld := TFRE_DB_FIELD.Create(nil,fdbft_NotFound,FieldName,nil);
  new_fld.CloneFromField(self);
  new_fld.FObjUidPath := ParentObject.GetUIDPathUA;
  new_fld.FInCollectionArray := ParentObject.__InternalGetCollectionListUSL;
  result := new_fld;
end;

function TFRE_DB_FIELD.CloneToNewStreamableObj: IFRE_DB_Object;
var obj : TFRE_DB_Object;
begin
  obj := GFRE_DB.NewObject();
  obj.Field('F').CloneFromField(self);
  obj.Field('FN').AsString:=self.FieldName;
  obj.Field('FUP').AsGUIDArr   := ParentObject.GetUIDPathUA;
  obj.Field('FIC').AsStringArr := ParentObject.__InternalGetCollectionListUSL;
  obj.Field('OCP').AsStringArr := ParentObject.GetSchemePath;
  obj.Field('OFP').AsStringArr := GetFieldPath;
  result := obj;
end;

function TFRE_DB_FIELD.GetUpdateObjectUIDPath: TFRE_DB_GUIDArray;
begin
  result := FObjUidPath;
end;

function TFRE_DB_FIELD.GetInCollectionArrayUSL: TFRE_DB_StringArray;
begin
  result := FInCollectionArray;
end;

function TFRE_DB_FIELD.GetUpdateObjSchemePath: TFRE_DB_StringArray;
begin
  result := FSchemePath;
end;

function TFRE_DB_FIELD.GetUpdateObjFieldPath: TFRE_DB_StringArray;
begin
  result := FUpObjFieldPath;
end;

function TFRE_DB_FIELD.GetFieldPath: TFRE_DB_StringArray;
var i       : NativeInt;
    cnt     : NativeInt;
    lparent : TFRE_DB_FIELD;
begin
  result := nil;
  cnt    := 0;
  lparent := ParentObject.ParentField;
  while assigned(lparent) do
    begin
      inc(cnt);
      lparent := lparent.ParentObject.ParentField;
    end;
  SetLength(result,cnt+1);
  lparent := self;
  for i := cnt downto 0 do
    begin
      Result[i] := lparent.FieldName;
      lparent   := lparent.ParentObject.ParentField;
    end;
end;

procedure TFRE_DB_FIELD._IllegalTypeError(const ill_type: TFRE_DB_FIELDTYPE);
begin
  raise EFRE_DB_Exception.Create(edb_ILLEGALCONVERSION,Format(' illegal conversion field [%s] from [%s] to [%s]',[FieldName,CFRE_DB_FIELDTYPE[FFieldData.FieldType],CFRE_DB_FIELDTYPE[ill_type]]));
end;

procedure TFRE_DB_FIELD._ResultTypeUnset(const ill_type: TFRE_DB_FIELDTYPE);
begin
 raise EFRE_DB_Exception.Create(edb_ERROR,Format(' result type is unset in calculated field[%s] for type[%s] ',[FieldName,CFRE_DB_FIELDTYPE[ill_type]]));
end;

procedure TFRE_DB_FIELD._StringToConvError(const conv2_type: TFRE_DB_FIELDTYPE);
begin
  raise EFRE_DB_Exception.Create(edb_STRING2TYPEFAILED,Format(' string to type conversion failed field[%s] from [%s] to [%s]',[FieldName,CFRE_DB_FIELDTYPE[FFieldData.FieldType],CFRE_DB_FIELDTYPE[conv2_type]]));
end;

procedure TFRE_DB_FIELD._GetHigh(var hi: integer);
begin
  case FFieldData.FieldType of
    fdbft_NotFound:    hi :=                    -1;
    fdbft_GUID    :    hi := high(FFieldData.guid^);
    fdbft_Byte    :    hi := high(FFieldData.byte^);
    fdbft_Int16   :    hi := high(FFieldData.in16^);
    fdbft_UInt16  :    hi := high(FFieldData.ui16^);
    fdbft_Int32   :    hi := high(FFieldData.in32^);
    fdbft_UInt32  :    hi := high(FFieldData.ui32^);
    fdbft_Int64   :    hi := high(FFieldData.in64^);
    fdbft_UInt64  :    hi := high(FFieldData.ui64^);
    fdbft_Real32  :    hi := high(FFieldData.re32^);
    fdbft_Real64  :    hi := high(FFieldData.re64^);
    fdbft_Currency:    hi := high(FFieldData.curr^);
    fdbft_String  :    hi := high(FFieldData.strg^);
    fdbft_Boolean :    hi := high(FFieldData.bool^);
    fdbft_DateTimeUTC: hi := high(FFieldData.date^);
    fdbft_Stream  :    hi := high(FFieldData.strm^);
    fdbft_Object  :    hi := 0 ;
    fdbft_ObjLink :    hi := high(FFieldData.obl^) ;
  end;
end;


function TFRE_DB_FIELD._ConvertToCurrency: Currency;

  procedure _String2Currency;
  begin
    if not TryStrToCurr(FFieldData.strg^[0],result,GFRE_DB.FFormatSettings) then _StringToConvError(fdbft_Currency);
  end;

begin
  case FFieldData.FieldType of
    fdbft_Byte:          result := FFieldData.byte^[0];
    fdbft_Int16:         result := FFieldData.in16^[0];
    fdbft_UInt16:        result := FFieldData.ui16^[0];
    fdbft_Int32:         result := FFieldData.in32^[0];
    fdbft_UInt32:        result := FFieldData.ui32^[0];
    fdbft_Int64:         result := FFieldData.in64^[0];
    fdbft_UInt64:        result := FFieldData.ui64^[0];
    fdbft_Real32:        result := FFieldData.re32^[0];
    fdbft_Real64:        result := FFieldData.re64^[0];
    fdbft_Currency:      result := FFieldData.curr^[0];
    fdbft_String:        _String2Currency;
    else                 _IllegalTypeError(fdbft_Currency);
  end;
end;

function TFRE_DB_FIELD._ConvertToDateTime: TFRE_DB_Datetime64;

  procedure _String2DateTime;
  var lresult:TDateTime;
  begin
    if not TryStrToDate(FFieldData.strg^[0],lresult,GFRE_DB.FFormatSettings) then _StringToConvError(fdbft_DateTimeUTC) else result:=GFRE_DT.DateTimeToDBDateTime64(lresult);
  end;

begin
  case FFieldData.FieldType of
    fdbft_String:      _String2DateTime;
    fdbft_DateTimeUTC: result := FFieldData.date^[0];
    else _IllegalTypeError(fdbft_DateTimeUTC);
  end;
end;


function TFRE_DB_FIELD._ConvertToGUID: TFRE_DB_GUID;
  procedure _String2;
  begin
    try
      Result := FREDB_H2G(FFieldData.strg^[0]);
    except
      _StringToConvError(fdbft_GUID);
    end;
  end;

begin
  case FFieldData.FieldType of
    fdbft_GUID:           result := FFieldData.guid^[0];
    fdbft_String:         _String2;
    fdbft_ObjLink:        result := FFieldData.obl^[0];
    else                  _IllegalTypeError(fdbft_GUID);
  end;
end;

function TFRE_DB_FIELD._ConvertToByte: Byte;

  procedure _String2;
  var l:longint;
  begin
    if not TryStrToInt(FFieldData.strg^[0],l) then _StringToConvError(fdbft_Byte);
    result := l;
  end;

begin
  case FFieldData.FieldType of
    fdbft_Boolean:       if FFieldData.bool^[0] then
                           result := 1
                         else
                           result := 0;
    fdbft_Byte:          result := FFieldData.byte^[0];
    fdbft_Int16:         result := FFieldData.in16^[0];
    fdbft_UInt16:        result := FFieldData.ui16^[0];
    fdbft_Int32:         result := FFieldData.in32^[0];
    fdbft_UInt32:        result := FFieldData.ui32^[0];
    fdbft_Int64:         result := FFieldData.in64^[0];
    fdbft_UInt64:        result := FFieldData.ui64^[0];
    fdbft_Real32:        result := Round(FFieldData.re32^[0]);
    fdbft_Real64:        result := Round(FFieldData.re64^[0]);
    fdbft_Currency:      result := Round(FFieldData.curr^[0]);
    fdbft_String:        _String2;
    else                 _IllegalTypeError(fdbft_Byte);
  end;
end;

function TFRE_DB_FIELD._ConvertToInt16: SmallInt;

  procedure _String2;
  var l : longint;
  begin
    if not TryStrToInt(FFieldData.strg^[0],l) then _StringToConvError(fdbft_Int16);
    result := l;
  end;

begin
  case FFieldData.FieldType of
    fdbft_Byte:          result := FFieldData.byte^[0];
    fdbft_Int16:         result := FFieldData.in16^[0];
    fdbft_UInt16:        result := FFieldData.ui16^[0];
    fdbft_Int32:         result := FFieldData.in32^[0];
    fdbft_UInt32:        result := FFieldData.ui32^[0];
    fdbft_Int64:         result := FFieldData.in64^[0];
    fdbft_UInt64:        result := FFieldData.ui64^[0];
    fdbft_Real32:        result := Round(FFieldData.re32^[0]);
    fdbft_Real64:        result := Round(FFieldData.re64^[0]);
    fdbft_Currency:      result := Round(FFieldData.curr^[0]);
    fdbft_String:         _String2;
    else                 _IllegalTypeError(fdbft_Int16);
  end;
end;

function TFRE_DB_FIELD._ConvertToUInt16: Word;

  procedure _String2;
  var l:longint;
  begin
    if not TryStrToInt(FFieldData.strg^[0],l) then _StringToConvError(fdbft_UInt16);
    result := l;
  end;

begin
  case FFieldData.FieldType of
    fdbft_Byte:          result := FFieldData.byte^[0];
    fdbft_Int16:         result := FFieldData.in16^[0];
    fdbft_UInt16:        result := FFieldData.ui16^[0];
    fdbft_Int32:         result := FFieldData.in32^[0];
    fdbft_UInt32:        result := FFieldData.ui32^[0];
    fdbft_Int64:         result := FFieldData.in64^[0];
    fdbft_UInt64:        result := FFieldData.ui64^[0];
    fdbft_Real32:        result := Round(FFieldData.re32^[0]);
    fdbft_Real64:        result := Round(FFieldData.re64^[0]);
    fdbft_Currency:      result := Round(FFieldData.curr^[0]);
    fdbft_String:        _String2;
    else                 _IllegalTypeError(fdbft_UInt16);
  end;
end;

function TFRE_DB_FIELD._ConvertToInt32: Longint;

  procedure _String2;
  begin
    if not TryStrToInt(FFieldData.strg^[0],result) then _StringToConvError(fdbft_Int32);
  end;

begin
  case FFieldData.FieldType of
    fdbft_Byte:          result := FFieldData.byte^[0];
    fdbft_Int16:         result := FFieldData.in16^[0];
    fdbft_UInt16:        result := FFieldData.ui16^[0];
    fdbft_Int32:         result := FFieldData.in32^[0];
    fdbft_UInt32:        result := FFieldData.ui32^[0];
    fdbft_Int64:         result := FFieldData.in64^[0];
    fdbft_UInt64:        result := FFieldData.ui64^[0];
    fdbft_Real32:        result := Round(FFieldData.re32^[0]);
    fdbft_Real64:        result := Round(FFieldData.re64^[0]);
    fdbft_Currency:      result := Round(FFieldData.curr^[0]);
    fdbft_String:        _String2;
    else                 _IllegalTypeError(fdbft_Int32);
  end;
end;

function TFRE_DB_FIELD._ConvertToUInt32: Longword;

  procedure _String2;
  var l:QWord;
  begin
    if not TryStrToQWord(FFieldData.strg^[0],l) then _StringToConvError(fdbft_UInt32);
    result := l;
  end;

begin
  case FFieldData.FieldType of
    fdbft_Byte:          result := FFieldData.byte^[0];
    fdbft_Int16:         result := FFieldData.in16^[0];
    fdbft_UInt16:        result := FFieldData.ui16^[0];
    fdbft_Int32:         result := FFieldData.in32^[0];
    fdbft_UInt32:        result := FFieldData.ui32^[0];
    fdbft_Int64:         result := FFieldData.in64^[0];
    fdbft_UInt64:        result := FFieldData.ui64^[0];
    fdbft_Real32:        result := Round(FFieldData.re32^[0]);
    fdbft_Real64:        result := Round(FFieldData.re64^[0]);
    fdbft_Currency:      result := Round(FFieldData.curr^[0]);
    fdbft_String:        _String2;
    else                 _IllegalTypeError(fdbft_UInt32);
  end;
end;

function TFRE_DB_FIELD._ConvertToInt64: int64;

  procedure _String2;
  begin
    if not TryStrToInt64(FFieldData.strg^[0],result) then _StringToConvError(fdbft_Int64);
  end;

begin
  case FFieldData.FieldType of
    fdbft_Byte:          result := FFieldData.byte^[0];
    fdbft_Int16:         result := FFieldData.in16^[0];
    fdbft_UInt16:        result := FFieldData.ui16^[0];
    fdbft_Int32:         result := FFieldData.in32^[0];
    fdbft_UInt32:        result := FFieldData.ui32^[0];
    fdbft_Int64:         result := FFieldData.in64^[0];
    fdbft_UInt64:        result := FFieldData.ui64^[0];
    fdbft_Real32:        result := Round(FFieldData.re32^[0]);
    fdbft_Real64:        result := Round(FFieldData.re64^[0]);
    fdbft_Currency:      result := PInt64(@FFieldData.curr^[0])^;
    fdbft_DateTimeUTC:   result := FFieldData.date^[0];
    fdbft_String:        _String2;
    else                 _IllegalTypeError(fdbft_Int64);
  end;
end;

function TFRE_DB_FIELD._ConvertToUInt64: QWord;

  procedure _String2;
  begin
    if not TryStrToQWord(FFieldData.strg^[0],result) then _StringToConvError(fdbft_UInt64);
  end;

begin
  case FFieldData.FieldType of
    fdbft_Boolean:       if FFieldData.bool^[0] then
                           result := 1
                         else
                           result := 0;
    fdbft_Byte:          result := FFieldData.byte^[0];
    fdbft_Int16:         result := FFieldData.in16^[0];
    fdbft_UInt16:        result := FFieldData.ui16^[0];
    fdbft_Int32:         result := FFieldData.in32^[0];
    fdbft_UInt32:        result := FFieldData.ui32^[0];
    fdbft_Int64:         result := FFieldData.in64^[0];
    fdbft_UInt64:        result := FFieldData.ui64^[0];
    fdbft_Real32:        result := Round(FFieldData.re32^[0]);
    fdbft_Real64:        result := Round(FFieldData.re64^[0]);
    fdbft_Currency:      result := Round(FFieldData.curr^[0]);
    fdbft_String:        _String2;
    else                 _IllegalTypeError(fdbft_UInt64);
  end;
end;

function TFRE_DB_FIELD._ConvertToSingle: Single;

  procedure _String2;
  begin
    if not TryStrToFloat(FFieldData.strg^[0],result) then _StringToConvError(fdbft_Real32);
  end;

begin
  case FFieldData.FieldType of
    fdbft_Byte:          result := FFieldData.byte^[0];
    fdbft_Int16:         result := FFieldData.in16^[0];
    fdbft_UInt16:        result := FFieldData.ui16^[0];
    fdbft_Int32:         result := FFieldData.in32^[0];
    fdbft_UInt32:        result := FFieldData.ui32^[0];
    fdbft_Int64:         result := FFieldData.in64^[0];
    fdbft_UInt64:        result := FFieldData.ui64^[0];
    fdbft_Real32:        result := FFieldData.re32^[0];
    fdbft_Real64:        result := FFieldData.re64^[0];
    fdbft_Currency:      result := FFieldData.curr^[0];
    fdbft_String:        _String2;
    fdbft_DateTimeUTC:   result := FFieldData.date^[0];
    else                 _IllegalTypeError(fdbft_Real32);
  end;
end;

function TFRE_DB_FIELD._ConvertToDouble: Double;

  procedure _String2;
  begin
    if not TryStrToFloat(FFieldData.strg^[0],result) then _StringToConvError(fdbft_Real64);
  end;

begin
  case FFieldData.FieldType of
    fdbft_Byte:          result := FFieldData.byte^[0];
    fdbft_Int16:         result := FFieldData.in16^[0];
    fdbft_UInt16:        result := FFieldData.ui16^[0];
    fdbft_Int32:         result := FFieldData.in32^[0];
    fdbft_UInt32:        result := FFieldData.ui32^[0];
    fdbft_Int64:         result := FFieldData.in64^[0];
    fdbft_UInt64:        result := FFieldData.ui64^[0];
    fdbft_Real32:        result := FFieldData.re32^[0];
    fdbft_Real64:        result := FFieldData.re64^[0];
    fdbft_Currency:      result := FFieldData.curr^[0];
    fdbft_String:        _String2;
    fdbft_DateTimeUTC:   result := FFieldData.date^[0];
    else                 _IllegalTypeError(fdbft_Real64);
  end;
end;


function TFRE_DB_FIELD._ConvertToBool: Boolean;

  procedure _String2;
  begin
    if not TryStrToBool(FFieldData.strg^[0],result) then _StringToConvError(fdbft_Boolean);
  end;

begin
  case FFieldData.FieldType of
    fdbft_Byte:          result := FFieldData.byte^[0]<>0;
    fdbft_Int16:         result := FFieldData.in16^[0]<>0;
    fdbft_UInt16:        result := FFieldData.ui16^[0]<>0;
    fdbft_Int32:         result := FFieldData.in32^[0]<>0;
    fdbft_UInt32:        result := FFieldData.ui32^[0]<>0;
    fdbft_Int64:         result := FFieldData.in64^[0]<>0;
    fdbft_UInt64:        result := FFieldData.ui64^[0]<>0;
    fdbft_Real32:        result := abs(FFieldData.re32^[0])  < CFRE_DB_EPSILON_DBL;
    fdbft_Real64:        result := abs(FFieldData.re64^[0])  < CFRE_DB_EPSILON_DBL;
    fdbft_Currency:      result := FFieldData.curr^[0]<>0;
    fdbft_String:        _String2;
    else                 _IllegalTypeError(fdbft_Boolean);
  end;
end;


function TFRE_DB_FIELD._ConvertToString(const idx:integer=0): TFRE_DB_String;

  function _StreamToStringAsURLAccess:TFRE_DB_String;
  var s:TFRE_DB_String;
      i:integer;
  begin
    result := '[STREAM]'
    //i:=FFieldData.strm^[idx].Size;
    //SetLength(s,i);
    //Move(FFieldData.strm^[idx].Memory^,s[1],i);
    //result:=GFRE_BT.Base64Encode(s);
  end;


begin
  if ValueCount=0 then exit('');
  case FFieldData.FieldType of
    fdbft_NotFound:    result := '';
    fdbft_GUID:        result := FREDB_G2H(FFieldData.guid^[idx]);
    fdbft_Byte:        result := IntToStr(FFieldData.byte^[idx]);
    fdbft_Int16:       result := IntToStr(FFieldData.in16^[idx]);
    fdbft_UInt16:      result := IntToStr(FFieldData.ui16^[idx]);
    fdbft_Int32:       result := IntToStr(FFieldData.in32^[idx]);
    fdbft_UInt32:      result := IntToStr(FFieldData.ui32^[idx]);
    fdbft_Int64:       result := IntToStr(FFieldData.in64^[idx]);
    fdbft_UInt64:      result := IntToStr(FFieldData.ui64^[idx]);
    fdbft_Real32:      result := FloatToStr(FFieldData.re32^[idx]);//,GFRE_DB.GetFormatSettings);
    fdbft_Real64:      result := FloatToStr(FFieldData.re64^[idx]);//,GFRE_DB.GetFormatSettings);
    fdbft_Currency:    result := CurrToStr(FFieldData.curr^[idx]);//,GFRE_DB.GetFormatSettings);
    fdbft_String:      result := FFieldData.strg^[idx];
    fdbft_Boolean:     result := BoolToStr(FFieldData.bool^[idx],'1','0');
    fdbft_DateTimeUTC: result := GFRE_DT.ToStrUTC(GFRE_DB.UTCToLocalTimeDB64(FFieldData.date^[idx]));
    fdbft_Stream:      result := _StreamToStringAsUrlAccess;
    fdbft_Object:      result := '[$O:'+FFieldData.obj.SchemeClass+']'; // FFieldData.obj^[idx].DumpToString;
    fdbft_ObjLink:     result := FREDB_G2H(FFieldData.obl^[idx]); //FREDB_GuidArray2StringStream(FFieldData.obl^);
    else               raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled %s',[CFRE_DB_FIELDTYPE[FFieldData.FieldType]]);
  end;
end;

function TFRE_DB_FIELD._ConvertToSignedArray: TFRE_DB_Int64Array;
var i : NativeInt;
begin
  if ValueCount=0 then
    exit(nil);
  SetLength(result,ValueCount);
  for i := 0 to ValueCount-1 do
    case FFieldData.FieldType of
      fdbft_Byte:        result[i] := FFieldData.byte^[i];
      fdbft_Int16:       result[i] := FFieldData.in16^[i];
      fdbft_UInt16:      result[i] := FFieldData.ui16^[i];
      fdbft_Int32:       result[i] := FFieldData.in32^[i];
      fdbft_UInt32:      result[i] := FFieldData.ui32^[i];
      fdbft_Int64:       result[i] := FFieldData.in64^[i];
      fdbft_UInt64:      result[i] := FFieldData.ui64^[i];
      fdbft_Real32:      result[i] := Round(FFieldData.re32^[i]);
      fdbft_Real64:      result[i] := Round(FFieldData.re64^[i]);
      fdbft_Currency:    result[i] := round(FFieldData.curr^[i]*1000);
      fdbft_String:      result[i] := StrToInt64(FFieldData.strg^[i]);
      fdbft_Boolean:     result[i] := Ord(FFieldData.bool^[i]);
      fdbft_DateTimeUTC: result[i] := FFieldData.date^[i];
      else               raise EFRE_DB_Exception.Create(edb_ERROR,'cannot convert %s to signed array',[CFRE_DB_FIELDTYPE[FFieldData.FieldType]]);
    end;
end;

function TFRE_DB_FIELD._ConvertToUnsignedArray: TFRE_DB_UInt64Array;
var i : NativeInt;
begin
  if ValueCount=0 then
    exit(nil);
  SetLength(result,ValueCount);
  for i := 0 to ValueCount-1 do
    case FFieldData.FieldType of
      fdbft_Byte:        result[i] := FFieldData.byte^[i];
      fdbft_Int16:       result[i] := FFieldData.in16^[i];
      fdbft_UInt16:      result[i] := FFieldData.ui16^[i];
      fdbft_Int32:       result[i] := FFieldData.in32^[i];
      fdbft_UInt32:      result[i] := FFieldData.ui32^[i];
      fdbft_Int64:       result[i] := FFieldData.in64^[i];
      fdbft_UInt64:      result[i] := FFieldData.ui64^[i];
      fdbft_Real32:      result[i] := Round(FFieldData.re32^[i]);
      fdbft_Real64:      result[i] := Round(FFieldData.re64^[i]);
      fdbft_Currency:    result[i] := round(FFieldData.curr^[i]*1000);
      fdbft_String:      result[i] := StrToQWord(FFieldData.strg^[i]);
      fdbft_Boolean:     result[i] := Ord(FFieldData.bool^[i]);
      fdbft_DateTimeUTC: result[i] := FFieldData.date^[i];
      else               raise EFRE_DB_Exception.Create(edb_ERROR,'cannot convert %s to unsigned array',[CFRE_DB_FIELDTYPE[FFieldData.FieldType]]);
    end;
end;

function TFRE_DB_FIELD._ConvertToCurrencyArray: TFRE_DB_CurrencyArray;
var i : NativeInt;
begin
  if ValueCount=0 then
    exit(nil);
  SetLength(result,ValueCount);
  for i := 0 to ValueCount-1 do
    case FFieldData.FieldType of
      fdbft_Byte:        result[i] := FFieldData.byte^[i];
      fdbft_Int16:       result[i] := FFieldData.in16^[i];
      fdbft_UInt16:      result[i] := FFieldData.ui16^[i];
      fdbft_Int32:       result[i] := FFieldData.in32^[i];
      fdbft_UInt32:      result[i] := FFieldData.ui32^[i];
      fdbft_Int64:       result[i] := FFieldData.in64^[i];
      fdbft_UInt64:      result[i] := FFieldData.ui64^[i];
      fdbft_Real32:      result[i] := FFieldData.re32^[i];
      fdbft_Real64:      result[i] := FFieldData.re64^[i];
      fdbft_Currency:    result[i] := FFieldData.curr^[i];
      fdbft_String:      result[i] := StrToCurr(FFieldData.strg^[i]);
      fdbft_Boolean:     result[i] := Ord(FFieldData.bool^[i]);
      fdbft_DateTimeUTC: result[i] := FFieldData.date^[i];
      else               raise EFRE_DB_Exception.Create(edb_ERROR,'cannot convert %s to currency array',[CFRE_DB_FIELDTYPE[FFieldData.FieldType]]);
    end;
end;

function TFRE_DB_FIELD._ConvertToReal64Array: TFRE_DB_Real64Array;
var i : NativeInt;
begin
  if ValueCount=0 then
    exit(nil);
  SetLength(result,ValueCount);
  for i := 0 to ValueCount-1 do
    case FFieldData.FieldType of
      fdbft_Byte:        result[i] := FFieldData.byte^[i];
      fdbft_Int16:       result[i] := FFieldData.in16^[i];
      fdbft_UInt16:      result[i] := FFieldData.ui16^[i];
      fdbft_Int32:       result[i] := FFieldData.in32^[i];
      fdbft_UInt32:      result[i] := FFieldData.ui32^[i];
      fdbft_Int64:       result[i] := FFieldData.in64^[i];
      fdbft_UInt64:      result[i] := FFieldData.ui64^[i];
      fdbft_Real32:      result[i] := FFieldData.re32^[i];
      fdbft_Real64:      result[i] := FFieldData.re64^[i];
      fdbft_Currency:    result[i] := FFieldData.curr^[i];
      fdbft_String:      result[i] := StrToFloat(FFieldData.strg^[i]);
      fdbft_Boolean:     result[i] := Ord(FFieldData.bool^[i]);
      fdbft_DateTimeUTC: result[i] := FFieldData.date^[i];
      else               raise EFRE_DB_Exception.Create(edb_ERROR,'cannot convert %s to real64 array',[CFRE_DB_FIELDTYPE[FFieldData.FieldType]]);
    end;
end;


procedure TFRE_DB_FIELD._CheckFieldType(const expected: TFRE_DB_FIELDTYPE);
begin
  //if (FFieldData.FieldType=fdbft_CalcField) and (expected=fdbft_Object) then exit;
  if FFieldData.FieldType<>expected then
    raise EFRE_DB_Exception.Create(edb_MISMATCH,' got '+CFRE_DB_FIELDTYPE[FFieldData.FieldType]+' expected '+CFRE_DB_FIELDTYPE[expected]+' for field ['+FieldName+']');
end;

procedure TFRE_DB_FIELD._CheckIndex(const idx: integer);
var lo,hi:integer;
begin
  if FIsSchemeField then
    exit;
  hi:=0;
  _GetHigh(hi);
  if not ((idx>=0) and (idx<=hi)) then
    EFRE_DB_Exception.Create(edb_INDEXOUTOFBOUNDS,format(' fieldlist index out of bounds. idx is <%d>, but should be >= %d and <= %d ',[idx,lo,hi]));
end;

function TFRE_DB_FIELD._CheckStoreType(const expected: TFRE_DB_FIELDTYPE):boolean;
begin
  if assigned(Fobj) and
     Fobj._ReadOnlyCheck then
    raise EFRE_DB_Exception.Create(edb_ACCESS,' access to read only object  fieldname ['+FFieldName^+'] type ['+CFRE_DB_FIELDTYPE[FFieldData.FieldType]+']');
  if (FFieldData.FieldType=fdbft_NotFound) then begin
    exit(false);
  end else
  if (FFieldData.FieldType=expected) then begin
    exit(true);
  end else begin
    raise EFRE_DB_Exception.Create(edb_MISMATCH,' got '+CFRE_DB_FIELDTYPE[FFieldData.FieldType]+' expected '+CFRE_DB_FIELDTYPE[expected]+' or uninitialized field');
  end;
end;


procedure TFRE_DB_FIELD._LocalToUTC(var arr: TFRE_DB_DateTimeArray);
var i,lo,hi: Integer;
begin
 for i:=low(arr) to High(arr) do begin
   arr[i] := GFRE_DB.LocalTimeToUTCDB64(arr[i]);
  end;
end;

procedure TFRE_DB_FIELD._NotAllowedOnUIDorDomainIDFieldCheck;
begin
  if FIsUidField or FIsDomainIDField then
    raise EFRE_DB_Exception.Create(edb_ERROR,'operation not allowed on special UID or DomainID field!');
end;

procedure TFRE_DB_FIELD._NotAllowedOnSchemeField;
begin
  if FIsSchemeField then
    raise EFRE_DB_Exception.Create(edb_ERROR,'operation not allowed on special field Schemeclass');
end;

function TFRE_DB_FIELD._GetAsGUID: TFRE_DB_GUID;
begin
  if FFieldData.FieldType = fdbft_GUID then begin
    _CheckEmptyArray;
    result := FFieldData.guid^[0];
  end else begin
    result := _ConvertToGUID;
  end;
end;

procedure TFRE_DB_FIELD._InAccessibleFieldCheck;
begin
  if assigned(FObj) then {else cloned stream only}
    Fobj._InaccessibleCheck;
end;

procedure TFRE_DB_FIELD._CheckEmptyArray;
begin
  if Length(FFieldData.guid^)=0 then // type is not relevant for check
    raise EFRE_DB_Exception.Create(edb_ILLEGALCONVERSION,'the field of type [%s] is an empty array, cant access elements. use empty array check',[CFRE_DB_FIELDTYPE[FFieldData.FieldType]]);
end;

function TFRE_DB_FIELD._SchemeClassOfParent: TFRE_DB_String;
begin
  result := Fobj.SchemeClass;
end;

//function TFRE_DB_FIELD._DBConnectionBC: TFRE_DB_BASE_CONNECTION;
//begin
// result := Fobj._DBConnectionBC;
//end;

//function TFRE_DB_FIELD._DBConnection: TFRE_DB_CONNECTION;
//begin
// result := Fobj._DbConnection as TFRE_DB_CONNECTION;
//end;

procedure TFRE_DB_FIELD.Finalize;
begin
  if Fobj=nil then { streamable only}
    begin
      Free;
      exit;
    end;
  gfre_bt.CriticalAbort('never, ever finalize fields!');
end;



function TFRE_DB_FIELD.CopyFieldToMem(var mempointer: Pointer): TFRE_DB_SIZE_TYPE; //Streamed as FieldType,FieldValCount,FieldNameSize,{FieldName},{FieldData}
var startp,oldp : PByte;
    sz_field    : TFRE_DB_SIZE_TYPE;

   procedure _StoreSzField;
    begin
      Move (sz_field,startp^,CFRE_DB_SIZE_ENCODING_SIZE);
      inc(startp,CFRE_DB_SIZE_ENCODING_SIZE);
    end;
    procedure _StoreName;
    begin
      Move (FFieldName^[1],startp^,sz_field);
      inc(startp,sz_field);
    end;
    procedure _StoreArray(const len:SizeInt;const data:PByte); // One dimesnsional Dynamic Array ! (in current fpc impl)
    begin
      Move (data^,startp^,len);
      inc(startp,len);
    end;
    procedure _StoreStrings;
    var i,l:integer;
    begin
      l:=Length(FFieldData.strg^)-1;
      for i := 0 to l do begin
        sz_field := Length(FFieldData.strg^[i]);_StoreSzField;
        Move(FFieldData.strg^[i][1],startp^,sz_field);
        Inc(startp,sz_field);
      end;
    end;
    procedure _StoreStreams;
    var i,l:integer;
    begin
      l:=Length(FFieldData.strm^)-1;
      for i := 0 to l do begin
        sz_field := TFRE_DB_SIZE_TYPE(FFieldData.strm^[i].Size);_StoreSzField;
        Move(FFieldData.strm^[i].Memory^,startp^,sz_field);
        Inc(startp,sz_field);
      end;
    end;
    procedure _StoreObjects;
    begin
      sz_field := TFRE_DB_SIZE_TYPE(FFieldData.obj.FieldCount(true,false));
      dec(sz_field); { Schemeclass, which is ommited }
      _StoreSzField;
      FFieldData.obj.CopyToMem(startp);
    end;

begin
  result:=0;
  if (FFieldData.FieldType = fdbft_NotFound)
       or (IsFieldCalculated)
         or (IsSchemeField) then
           exit;
  startp     := mempointer;
  oldp       := startp;
  sz_field   := Length(FFieldName^);       _StoreSzField; _StoreName;
  sz_field   := Ord(FFieldData.FieldType); _StoreSzField;
  sz_field   := ValueCountReal;            _StoreSzField;
  case FFieldData.FieldType of
    fdbft_GUID:        _StoreArray(SizeOf(FFieldData.guid^[0]) * length(FFieldData.guid^) , @FFieldData.guid^[0]);
    fdbft_Byte:        _StoreArray(SizeOf(FFieldData.byte^[0]) * length(FFieldData.byte^) , @FFieldData.byte^[0]);
    fdbft_Int16:       _StoreArray(SizeOf(FFieldData.in16^[0]) * length(FFieldData.in16^) , @FFieldData.in16^[0]);
    fdbft_UInt16:      _StoreArray(SizeOf(FFieldData.ui16^[0]) * length(FFieldData.ui16^) , @FFieldData.ui16^[0]);
    fdbft_Int32:       _StoreArray(SizeOf(FFieldData.in32^[0]) * length(FFieldData.in32^) , @FFieldData.in32^[0]);
    fdbft_UInt32:      _StoreArray(SizeOf(FFieldData.ui32^[0]) * length(FFieldData.ui32^) , @FFieldData.ui32^[0]);
    fdbft_Int64:       _StoreArray(SizeOf(FFieldData.in64^[0]) * length(FFieldData.in64^) , @FFieldData.in64^[0]);
    fdbft_UInt64:      _StoreArray(SizeOf(FFieldData.ui64^[0]) * length(FFieldData.ui64^) , @FFieldData.ui64^[0]);
    fdbft_Real32:      _StoreArray(SizeOf(FFieldData.re32^[0]) * length(FFieldData.re32^) , @FFieldData.re32^[0]);
    fdbft_Real64:      _StoreArray(SizeOf(FFieldData.re64^[0]) * length(FFieldData.re64^) , @FFieldData.re64^[0]);
    fdbft_Currency:    _StoreArray(SizeOf(FFieldData.curr^[0]) * length(FFieldData.curr^) , @FFieldData.curr^[0]);
    fdbft_Boolean:     _StoreArray(SizeOf(FFieldData.bool^[0]) * length(FFieldData.bool^) , @FFieldData.bool^[0]);
    fdbft_DateTimeUTC: _StoreArray(SizeOf(FFieldData.date^[0]) * length(FFieldData.date^) , @FFieldData.date^[0]);
    fdbft_String:      _StoreStrings;
    fdbft_Stream:      _StoreStreams;
    fdbft_Object:      _StoreObjects;
    fdbft_ObjLink:     _StoreArray(SizeOf(FFieldData.obl^[0]) * length(FFieldData.obl^) , @FFieldData.obl^[0]);
    else               raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled');
  end;
  result := startp-oldp;
end;

procedure TFRE_DB_FIELD.CopyFieldFromMem(var mempointer: Pointer; const generate_new_uids: boolean; const version: byte; const endianmarker: byte);
var value_count : SizeInt;
    sz_field    : TFRE_DB_SIZE_TYPE;
    i           : SizeInt;
    startp      : PByte;

   procedure _ReadArray(const len:SizeInt;const data:PByte); // One dimesnsional Dynamic Array ! (in current fpc impl)
   begin
     Move (startp^,data^,len);
     inc  (startp,len);
   end;
   procedure _ReadStrings;
   var i,l:integer;
   begin
     l:=Length(FFieldData.strg^)-1;
     for i := 0 to l do begin
       Move (startp^,sz_field,CFRE_DB_SIZE_ENCODING_SIZE);inc(startp,CFRE_DB_SIZE_ENCODING_SIZE);
       SetLength(FFieldData.strg^[i],sz_field);
       Move(startp^,FFieldData.strg^[i][1],sz_field);
       Inc(startp,sz_field);
     end;
   end;
   procedure _ReadStreams;
   var i,l:integer;
   begin
     l:=Length(FFieldData.strm^)-1;
     for i := 0 to l do begin
       Move (startp^,sz_field,CFRE_DB_SIZE_ENCODING_SIZE);inc(startp,CFRE_DB_SIZE_ENCODING_SIZE);
       FFieldData.strm^[i] := TFRE_DB_Stream.Create;
       FFieldData.strm^[i].SetSize(sz_field);
       Move(startp^,FFieldData.strm^[i].Memory^,sz_field);
       Inc(startp,sz_field);
     end;
   end;

   procedure _ReadObjects;
   begin
     FFieldData.obj := TFRE_DB_Object.CreateInternalStreaming(self,startp,generate_new_uids,version,endianmarker);
   end;

begin
  Clear();
  startp := mempointer;
  Move                 (startp^,sz_field,CFRE_DB_SIZE_ENCODING_SIZE);inc(startp,CFRE_DB_SIZE_ENCODING_SIZE);
  FFieldData.FieldType := TFRE_DB_FIELDTYPE(sz_field);
  Move                 (startp^,sz_field,CFRE_DB_SIZE_ENCODING_SIZE);inc(startp,CFRE_DB_SIZE_ENCODING_SIZE);
  value_count          :=sz_field;
  case FFieldData.FieldType of
    fdbft_GUID:        begin New(FFieldData.guid);SetLength(FFieldData.guid^,value_count);_ReadArray(SizeOf(FFieldData.guid^[0]) * length(FFieldData.guid^),@FFieldData.guid^[0]) end;
    fdbft_Byte:        begin New(FFieldData.byte);SetLength(FFieldData.byte^,value_count);_ReadArray(SizeOf(FFieldData.byte^[0]) * length(FFieldData.byte^),@FFieldData.byte^[0]) end;
    fdbft_Int16:       begin New(FFieldData.in16);SetLength(FFieldData.in16^,value_count);_ReadArray(SizeOf(FFieldData.in16^[0]) * length(FFieldData.in16^),@FFieldData.in16^[0]) end;
    fdbft_UInt16:      begin New(FFieldData.ui16);SetLength(FFieldData.ui16^,value_count);_ReadArray(SizeOf(FFieldData.ui16^[0]) * length(FFieldData.ui16^),@FFieldData.ui16^[0]) end;
    fdbft_Int32:       begin New(FFieldData.in32);SetLength(FFieldData.in32^,value_count);_ReadArray(SizeOf(FFieldData.in32^[0]) * length(FFieldData.in32^),@FFieldData.in32^[0]) end;
    fdbft_UInt32:      begin New(FFieldData.ui32);SetLength(FFieldData.ui32^,value_count);_ReadArray(SizeOf(FFieldData.ui32^[0]) * length(FFieldData.ui32^),@FFieldData.ui32^[0]) end;
    fdbft_Int64:       begin New(FFieldData.in64);SetLength(FFieldData.in64^,value_count);_ReadArray(SizeOf(FFieldData.in64^[0]) * length(FFieldData.in64^),@FFieldData.in64^[0]) end;
    fdbft_UInt64:      begin New(FFieldData.ui64);SetLength(FFieldData.ui64^,value_count);_ReadArray(SizeOf(FFieldData.ui64^[0]) * length(FFieldData.ui64^),@FFieldData.ui64^[0]) end;
    fdbft_Real32:      begin New(FFieldData.re32);SetLength(FFieldData.re32^,value_count);_ReadArray(SizeOf(FFieldData.re32^[0]) * length(FFieldData.re32^),@FFieldData.re32^[0]) end;
    fdbft_Real64:      begin New(FFieldData.re64);SetLength(FFieldData.re64^,value_count);_ReadArray(SizeOf(FFieldData.re64^[0]) * length(FFieldData.re64^),@FFieldData.re64^[0]) end;
    fdbft_Currency:    begin New(FFieldData.curr);SetLength(FFieldData.curr^,value_count);_ReadArray(SizeOf(FFieldData.curr^[0]) * length(FFieldData.curr^),@FFieldData.curr^[0]) end;
    fdbft_Boolean:     begin New(FFieldData.bool);SetLength(FFieldData.bool^,value_count);_ReadArray(SizeOf(FFieldData.bool^[0]) * length(FFieldData.bool^),@FFieldData.bool^[0]) end;
    fdbft_DateTimeUTC: begin New(FFieldData.date);SetLength(FFieldData.date^,value_count);_ReadArray(SizeOf(FFieldData.date^[0]) * length(FFieldData.date^),@FFieldData.date^[0]) end;
    fdbft_String:      begin New(FFieldData.strg);SetLength(FFieldData.strg^,value_count);_ReadStrings; end;
    fdbft_Stream:      begin New(FFieldData.strm);SetLength(FFieldData.strm^,value_count);_ReadStreams; end;
    fdbft_Object:      begin _ReadObjects; end;
    fdbft_ObjLink:     begin New(FFieldData.obl);SetLength(FFieldData.obl^,value_count);_ReadArray(SizeOf(FFieldData.obl^[0]) * length(FFieldData.obl^),@FFieldData.obl^[0]) end;
    else               raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled '+inttostr(ord(FFieldData.FieldType)));
  end;
  mempointer := startp;
end;

function TFRE_DB_FIELD.GetAsCurrency: Currency;
begin
  _InAccessibleFieldCheck;
  if FFieldData.FieldType = fdbft_Currency then begin
    _CheckEmptyArray;
    result := FFieldData.curr^[0];
  end else begin
    result := _ConvertToCurrency;
  end;
end;

function TFRE_DB_FIELD.GetAsCurrencyArray: TFRE_DB_CurrencyArray;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Currency);
  result := FFieldData.curr^;
end;

function TFRE_DB_FIELD.GetAsCurrencyList(idx: Integer): Currency;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_Currency);
  _CheckIndex(idx);
  result := FFieldData.curr^[idx];
end;

function TFRE_DB_FIELD.GetAsDateTime: TFRE_DB_DateTime64;
begin
  _InAccessibleFieldCheck;
  if FFieldData.FieldType = fdbft_DateTimeUTC then begin
    _CheckEmptyArray;
    result := GFRE_DB.UTCToLocalTimeDB64(FFieldData.date^[0]);
  end else begin
    result := _ConvertToDateTime;
  end;
end;

function TFRE_DB_FIELD.GetAsDateTimeArray: TFRE_DB_DateTimeArray;
var temp : TFRE_DB_DateTimeArray;
       i : Integer;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_DateTimeUTC);
  temp   := FFieldData.date^;
  for i  := low(temp) to high(temp) do begin
    temp[i] := GFRE_DB.UTCToLocalTimeDB64(temp[i]);
  end;
  result := temp;
end;

function TFRE_DB_FIELD.GetAsDateTimeList(idx: Integer): TFRE_DB_DateTime64;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_DateTimeUTC);
  _CheckIndex(idx);
 result := GFRE_DB.UTCToLocalTimeDB64(FFieldData.date^[idx]);
end;

function TFRE_DB_FIELD.GetAsDateTimeUTC: TFRE_DB_DateTime64;
begin
  _InAccessibleFieldCheck;
  if FFieldData.FieldType = fdbft_DateTimeUTC then begin
    _CheckEmptyArray;
    result := FFieldData.date^[0];
  end else begin
    result := _ConvertToDateTime;
  end;
end;

function TFRE_DB_FIELD.GetAsString: TFRE_DB_String;
begin
  _InAccessibleFieldCheck;
  if FIsSchemeField then
    exit(_SchemeClassOfParent);
  if FFieldData.FieldType = fdbft_String then begin
    _CheckEmptyArray;
    result := FFieldData.strg^[0];
  end else begin
    result := _ConvertToString;
  end;
end;

function TFRE_DB_FIELD.GetAsDateTimeArrayUTC: TFRE_DB_DateTimeArray;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_DateTimeUTC);
  result := FFieldData.date^;
end;

function TFRE_DB_FIELD.GetAsDateTimeListUTC(idx: Integer): TFRE_DB_DateTime64;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_DateTimeUTC);
  _CheckIndex(idx);
  result := FFieldData.date^[idx];
end;


function TFRE_DB_FIELD.GetAsDouble: Double;
begin
  _InAccessibleFieldCheck;
  if FFieldData.FieldType = fdbft_Real64 then begin
    _CheckEmptyArray;
    result := FFieldData.re64^[0];
  end else begin
    result := _ConvertToDouble;
  end;
end;

function TFRE_DB_FIELD.GetAsDoubleArray: TFRE_DB_Real64Array;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Real64);
  result := FFieldData.re64^;
end;

function TFRE_DB_FIELD.GetAsDoubleList(idx: Integer): Double;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_Real64);
  _CheckIndex(idx);
  result := FFieldData.re64^[idx];
end;

function TFRE_DB_FIELD.GetAsGUID: TFRE_DB_GUID;
begin
  _InAccessibleFieldCheck;
  result := _GetAsGUID;
end;

function TFRE_DB_FIELD.GetAsGUIDArray: TFRE_DB_GUIDArray;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_GUID);
  result := FFieldData.guid^;
end;

function TFRE_DB_FIELD.GetAsGUIDList(idx: Integer): TFRE_DB_GUID;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_GUID);
  _CheckIndex(idx);
  result := FFieldData.guid^[idx];
end;


function TFRE_DB_FIELD.GetAsObject: TFRE_DB_Object;
var field_type :TFRE_DB_FIELDTYPE;

  function _CheckIfSchemeAvailable:boolean;
  var sc               : TFRE_DB_String;
      scheme_object    : TFRE_DB_SchemeObject;
      scheme_field_def : TFRE_DB_FieldSchemeDefinition;
      new_object       : TFRE_DB_Object;
      sfc              : TFRE_DB_String;
  begin
    result := false;
    sc:=_SchemeClassOfParent;
    if (sc<>'') and not (Fobj._ObjectsNeedsNoSubfieldSchemeCheck) then begin
      field_type:=field_type;
      if not GFRE_DB.GetSystemScheme(sc,scheme_object) then begin
        raise EFRE_DB_Exception.Create(edb_ERROR,'a new sub object wants to get accessed, a scheme is defined but cannot be checked. Scheme=%s Fieldname=%s',[sc,FieldName]);
      end;
      if scheme_object.GetSchemeField(FieldName,scheme_field_def) then begin
        sfc := scheme_field_def.SubschemeName;
        if not GFRE_DB.GetSystemScheme(sfc,scheme_object) then begin
          raise EFRE_DB_Exception.Create(edb_ERROR,'a new sub object wants to get accessed, a scheme is defined but cannot be checked. Scheme=%s Fieldname=%s SubfieldSchemc=%s',[sc,FieldName,sfc]);
        end;
        new_object := scheme_object.ConstructNewInstance;
        SetAsObject(new_object);
        result:=true;
      end;
    end;
  end;

begin
  _InAccessibleFieldCheck;
  field_type := FFieldData.FieldType;
  if field_type = fdbft_Object then begin
    _CheckEmptyArray;
    result := FFieldData.obj;
  end else begin
    if field_type=fdbft_NotFound then begin
       if not _CheckIfSchemeAvailable then
         SetAsObject(TFRE_DB_Object.Create); { create a plain object }
       result            := FFieldData.obj;
       result.FParentDBO := self;
    end else begin
      _IllegalTypeError(fdbft_Object);
    end;
  end;
end;

function TFRE_DB_FIELD.CheckOutObject: TFRE_DB_Object;
begin
  if FFieldData.FieldType = fdbft_Object then
    result := AsObject
  else
    _IllegalTypeError(fdbft_Object);
  Clear(true);
  result.FParentDBO:=nil;
end;

function TFRE_DB_FIELD.CheckOutObjectArray: IFRE_DB_ObjectArray;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Object);
  if FFieldData.obj is TFRE_DB_OBJECTLIST then
    result := GFRE_DB.ConstructObjectArrayOI(TFRE_DB_OBJECTLIST(FFieldData.obj).CheckOutAsArray)
  else
    raise EFRE_DB_Exception.Create(edb_MISMATCH,'try to access an object array/list but wrong class stored');
  Clear;
end;

function TFRE_DB_FIELD.CheckOutObjectArrayItem(const idx: NAtiveInt): IFRE_DB_Object;
begin
  result := nil;
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_Object);
  if FFieldData.obj is TFRE_DB_OBJECTLIST then
    result := TFRE_DB_OBJECTLIST(FFieldData.obj).CheckoutObject(idx)
  else
    raise EFRE_DB_Exception.Create(edb_MISMATCH,'try to access an object array/list but wrong class stored');
end;

function TFRE_DB_FIELD.CheckOutObjectI: IFRE_DB_Object;
begin
  result := CheckOutObject;
end;

function TFRE_DB_FIELD.GetAsObjectArray: TFRE_DB_ObjectArray;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Object);
  if FFieldData.obj is TFRE_DB_OBJECTLIST then
    result := TFRE_DB_OBJECTLIST(FFieldData.obj).GetAsObjectArray
  else
    raise EFRE_DB_Exception.Create(edb_MISMATCH,'try to access an object array/list but wrong class stored');
end;

function TFRE_DB_FIELD.GetAsObjectLinkArray: TFRE_DB_ObjLinkArray;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_ObjLink);
  result := Copy(FFieldData.obl^);
end;

function TFRE_DB_FIELD.GetAsObjectList(idx: Integer): TFRE_DB_Object;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_Object);
  //_CheckIndex(idx,fdbft_Object);
  if FFieldData.obj is TFRE_DB_OBJECTLIST then
    result := TFRE_DB_OBJECTLIST(FFieldData.obj).GetAsArray(idx)
  else
    raise EFRE_DB_Exception.Create(edb_MISMATCH,'try to access an object array/list but wrong class stored');
end;

function TFRE_DB_FIELD.GetAsObjectLinkList(idx: Integer): TFRE_DB_GUID;
begin
 _InAccessibleFieldCheck;
 _CheckFieldType(fdbft_ObjLink);
 _CheckIndex(idx);
 result := FFieldData.obl^[idx];
end;

function TFRE_DB_FIELD.GetAsStream: TFRE_DB_Stream;
var field_type :TFRE_DB_FIELDTYPE;
begin
  _InAccessibleFieldCheck;
  field_type := FFieldData.FieldType;
  if field_type = fdbft_Stream then begin
    _CheckEmptyArray;
    result := FFieldData.strm^[0];
  end else begin
    if field_type=fdbft_NotFound then begin
       AddStream(TFRE_DB_Stream.Create);
       result := FFieldData.strm^[0];
    end else begin
      _IllegalTypeError(fdbft_Stream);
    end;
  end;
end;

function TFRE_DB_FIELD.GetAsObjectLink: TFRE_DB_GUID;
begin
  _InAccessibleFieldCheck;
  if FFieldData.FieldType = fdbft_ObjLink then begin
    _CheckEmptyArray;
    result := FFieldData.obl^[0];
  end else begin
    _IllegalTypeError(fdbft_ObjLink);
  end;
end;

function TFRE_DB_FIELD.GetAsStreamArray: TFRE_DB_StreamArray;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Stream);
  result := FFieldData.strm^;
end;

function TFRE_DB_FIELD.GetAsStreamList(idx: Integer): TFRE_DB_Stream;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_Stream);
  _CheckIndex(idx);
  result := FFieldData.strm^[idx];
end;

function TFRE_DB_FIELD.GetAsStringArray: TFRE_DB_StringArray;
var lo,hi,i:integer;
begin
  _InAccessibleFieldCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end else
  if FieldType=fdbft_String then begin
    if not FIsSchemeField then
      result := FFieldData.strg^
    else
      begin
        SetLength(result,1);
        result[0] := _SchemeClassOfParent;
      end
  end else begin
    _GetHigh(hi);
    SetLength(result,hi+1);
    for i := 0 to hi do begin
      result[i] := _ConvertToString(i);
    end;
  end;
end;

function TFRE_DB_FIELD.GetAsStringList(idx: Integer): TFRE_DB_String;
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_String);
  _CheckIndex(idx);
  if not FIsSchemeField then
    result := FFieldData.strg^[idx]
  else
    begin
      if idx<>0 then
        raise EFRE_DB_Exception.Create(edb_INDEXOUTOFBOUNDS,'the SCHEMECLASS field has only one value');
      result := _SchemeClassOfParent;
    end;
end;

procedure TFRE_DB_FIELD.SetAsCurrency(const AValue: Currency);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Currency) then begin
    FFieldData.FieldType := fdbft_Currency;
    New(FFieldData.curr);
    SetLength(FFieldData.curr^,1);
  end;
  FFieldData.curr^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsCurrencyArray(const AValue: TFRE_DB_CurrencyArray);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Currency) then begin
    FFieldData.FieldType := fdbft_Currency;
    New(FFieldData.curr);
  end;
  FFieldData.curr^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsCurrencyList(idx: Integer; const AValue: Currency);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Currency) then begin
    New(FFieldData.curr);
  end;
  _CheckIndex(idx);
  FFieldData.curr^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsDateTime(const AValue: TFRE_DB_Datetime64);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_DateTimeUTC) then begin
    FFieldData.FieldType := fdbft_DateTimeUTC;
    New(FFieldData.date);
    SetLength(FFieldData.date^,1);
  end;
  FFieldData.date^[0]:=GFRE_DB.LocalTimeToUTCDB64(AValue);
end;

procedure TFRE_DB_FIELD.SetAsDateTimeArray(const AValue: TFRE_DB_DateTimeArray);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_DateTimeUTC) then begin
    FFieldData.FieldType := fdbft_DateTimeUTC;
    New(FFieldData.date);
  end;
  FFieldData.date^ := AValue;
  _LocalToUTC(FFieldData.date^);
end;

procedure TFRE_DB_FIELD.SetAsDateTimeList(idx: Integer; const AValue: TFRE_DB_Datetime64);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_DateTimeUTC) then begin
    New(FFieldData.date);
  end;
  _CheckIndex(idx);
  FFieldData.date^[idx] := GFRE_DB.LocalTimeToUTCDB64(AValue);
end;

procedure TFRE_DB_FIELD.SetAsDateTimeUTC(const AValue: TFRE_DB_Datetime64);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_DateTimeUTC) then begin
    FFieldData.FieldType := fdbft_DateTimeUTC;
    New(FFieldData.date);
    SetLength(FFieldData.date^,1);
  end;
  FFieldData.date^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsDateTimeArrayUTC(const AValue: TFRE_DB_DateTimeArray);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_DateTimeUTC) then begin
    FFieldData.FieldType := fdbft_DateTimeUTC;
    New(FFieldData.date);
  end;
  FFieldData.date^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsDateTimeListUTC(idx: Integer; const AValue: TFRE_DB_Datetime64);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_DateTimeUTC) then begin
    New(FFieldData.date);
  end;
  _CheckIndex(idx);
  FFieldData.date^[idx] := AValue;
end;



procedure TFRE_DB_FIELD.SetAsDouble(const AValue: Double);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Real64) then begin
    FFieldData.FieldType := fdbft_Real64;
    New(FFieldData.re64);
    SetLength(FFieldData.re64^,1);
  end;
  FFieldData.re64^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsDoubleArray(const AValue: TFRE_DB_Real64Array);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Real64) then begin
    FFieldData.FieldType := fdbft_Real64;
    New(FFieldData.re64);
  end;
  FFieldData.re64^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsDoubleList(idx: Integer; const AValue: Double);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Real64) then begin
    New(FFieldData.re64);
  end;
  _CheckIndex(idx);
  FFieldData.re64^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsGUID(const AValue: TFRE_DB_GUID);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_GUID) then begin
    FFieldData.FieldType := fdbft_GUID;
    New(FFieldData.guid);
    SetLength(FFieldData.guid^,1);
  end;
  FFieldData.guid^[0]:=AValue;
  if FIsUidField then
    Fobj.FUID := AValue;
  if FIsDomainIDField then
    Fobj.FDomainID := Avalue;
end;

procedure TFRE_DB_FIELD.SetAsGUIDArray(const AValue: TFRE_DB_GUIDArray);
begin
  _InAccessibleFieldCheck;
  _NotAllowedOnUIDorDomainIDFieldCheck;
  if not _CheckStoreType(fdbft_GUID) then begin
    FFieldData.FieldType := fdbft_GUID;
    New(FFieldData.guid);
  end;
  FFieldData.guid^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsGUIDList(idx: Integer; const AValue: TFRE_DB_GUID);
begin
  _InAccessibleFieldCheck;
  _NotAllowedOnUIDorDomainIDFieldCheck;
  if not _CheckStoreType(fdbft_GUID) then begin
    New(FFieldData.guid);
  end;
  _CheckIndex(idx);
  FFieldData.guid^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsObjectArray(const AValue: TFRE_DB_ObjectArray);
var  i: Integer;
begin
  _InAccessibleFieldCheck;
  for i:=0 to high(AValue) do begin
    if not Avalue[i].IsObjectRoot then
      raise EFRE_DB_Exception.Create(edb_ERROR,'its not allowed to set child objects as object array');
  end;
  for i:=0 to high(AValue) do begin
    Fobj._ParentCheck(Avalue[i]);
  end;
  if not _CheckStoreType(fdbft_Object) then begin
    FFieldData.FieldType := fdbft_Object;
    FFieldData.obj:=TFRE_DB_OBJECTLIST.Create;
  end;
  if FFieldData.obj is TFRE_DB_OBJECTLIST then
    TFRE_DB_OBJECTLIST(FFieldData.obj).SetAsObjectArray(Avalue)
  else
    raise EFRE_DB_Exception.Create(edb_MISMATCH,'try to access an object array/list but wrong class stored');
end;

procedure TFRE_DB_FIELD.SetAsObjectList(idx: Integer; const AValue: TFRE_DB_Object);
begin
  _InAccessibleFieldCheck;
  Fobj._ParentCheck(Avalue);
  if not AValue.IsObjectRoot then
    raise EFRE_DB_Exception.Create(edb_ERROR,'its not allowed to set child objects as object array');
  if not _CheckStoreType(fdbft_Object) then begin
    FFieldData.FieldType := fdbft_Object;
    FFieldData.obj:=TFRE_DB_OBJECTLIST.Create;
  end;
  if FFieldData.obj is TFRE_DB_OBJECTLIST then
    TFRE_DB_OBJECTLIST(FFieldData.obj)[idx]:=Avalue
  else
    raise EFRE_DB_Exception.Create(edb_MISMATCH,'try to access an object array/list but wrong class stored');
end;

procedure TFRE_DB_FIELD.SetAsStream(const AValue: TFRE_DB_Stream);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Stream) then begin
    FFieldData.FieldType := fdbft_Stream;
    New(FFieldData.strm);
    SetLength(FFieldData.strm^,1);
  end;
  FFieldData.strm^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsStreamArray(const AValue: TFRE_DB_StreamArray);
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Stream) then begin
    FFieldData.FieldType := fdbft_Stream;
    New(FFieldData.strm);
  end;
  FFieldData.strm^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsStreamList(idx: Integer; const AValue: TFRE_DB_Stream);
begin
 _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Stream) then begin
    New(FFieldData.strm);
  end;
  _CheckIndex(idx);
  FFieldData.strm^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsString(const AValue: TFRE_DB_String);
begin
  _InAccessibleFieldCheck;
  _NotAllowedOnSchemeField;
  if not _CheckStoreType(fdbft_String) then begin
    FFieldData.FieldType := fdbft_String;
    New(FFieldData.strg);
    SetLength(FFieldData.strg^,1);
  end;
  FFieldData.strg^[0]:=AValue;
end;


procedure TFRE_DB_FIELD.SetAsStringArray(const AValue: TFRE_DB_StringArray);
begin
  _InAccessibleFieldCheck;
  _NotAllowedOnSchemeField;
  if not _CheckStoreType(fdbft_String) then begin
    FFieldData.FieldType := fdbft_String;
    New(FFieldData.strg);
  end;
  FFieldData.strg^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsStringList(idx: Integer; const AValue: TFRE_DB_String);
begin
  _InAccessibleFieldCheck;
  _NotAllowedOnSchemeField;
  if not _CheckStoreType(fdbft_String) then begin
    New(FFieldData.strg);
  end;
  _CheckIndex(idx);
  FFieldData.strg^[idx] := AValue;
end;


constructor TFRE_DB_FIELD.Create(const obj: TFRE_DB_Object; const FieldType: TFRE_DB_FIELDTYPE; const ManualFieldName: string; const calcmethod: IFRE_DB_CalcMethod);
begin
  FFieldData.FieldType := FieldType;
  Fobj                 := obj;
  FManualFieldName     := ManualFieldName;
  FCalcMethod          := calcmethod;
  if FieldType<>fdbft_NotFound then
    begin
      if not assigned(calcmethod) then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'only calcfields can be created with a fieldtype, because they are set on access!');
      case FieldType of
        fdbft_GUID:
          begin
            New(FFieldData.guid);
            SetLength(FFieldData.strg^,1);
          end;
        fdbft_Byte:
          begin
            New(FFieldData.byte);
            SetLength(FFieldData.byte^,1);
          end;
        fdbft_Int16:
          begin
            New(FFieldData.in16);
            SetLength(FFieldData.in16^,1);
          end;
        fdbft_UInt16:
          begin
            New(FFieldData.ui16);
            SetLength(FFieldData.ui16^,1);
          end;
        fdbft_Int32:
          begin
            New(FFieldData.in32);
            SetLength(FFieldData.in32^,1);
          end;
        fdbft_UInt32:
          begin
            New(FFieldData.ui32);
            SetLength(FFieldData.ui32^,1);
          end;
        fdbft_Int64:
          begin
            New(FFieldData.in64);
            SetLength(FFieldData.in64^,1);
          end;
        fdbft_UInt64:
          begin
            New(FFieldData.ui64);
            SetLength(FFieldData.ui64^,1);
          end;
        fdbft_Real32:
          begin
            New(FFieldData.re32);
            SetLength(FFieldData.re32^,1);
          end;
        fdbft_Real64:
          begin
            New(FFieldData.re64);
            SetLength(FFieldData.re64^,1);
          end;
        fdbft_Currency:
          begin
            New(FFieldData.curr);
            SetLength(FFieldData.curr^,1);
          end;
        fdbft_String:
          begin
            New(FFieldData.strg);
            SetLength(FFieldData.strg^,1);
          end;
        fdbft_Boolean:
          begin
            New(FFieldData.bool);
            SetLength(FFieldData.bool^,1);
          end;
        fdbft_DateTimeUTC:
          begin
            New(FFieldData.date);
            SetLength(FFieldData.date^,1);
          end;
        fdbft_Stream:
          begin
            New(FFieldData.strm);
            SetLength(FFieldData.strm^,1);
          end;
        fdbft_Object:
          begin
            FFieldData.obj:=nil;
          end;
        fdbft_ObjLink:
        begin
          New(FFieldData.in32);
          SetLength(FFieldData.in32^,1);
        end;
      end;
    end;
end;

destructor TFRE_DB_FIELD.Destroy;
begin
  _InAccessibleFieldCheck;
  Clear;
  inherited Destroy;
end;

class procedure TFRE_DB_FIELD.__ReadHeader(var memory:pointer;out fieldname:TFRE_DB_NameType);
var sz_field:TFRE_DB_SIZE_TYPE;
begin
  Move      (memory^,sz_field,CFRE_DB_SIZE_ENCODING_SIZE);inc(memory,CFRE_DB_SIZE_ENCODING_SIZE);
  SetLength (fieldname,sz_field);
  Move      (memory^,fieldname[1],sz_field);inc(memory,sz_field);
end;

function TFRE_DB_FIELD.FieldType: TFRE_DB_FIELDTYPE;
begin
  _InAccessibleFieldCheck;
  result := _FieldType;
end;

function TFRE_DB_FIELD.FieldTypeAsString: TFRE_DB_String;
begin
  _InAccessibleFieldCheck;
  result := CFRE_DB_FIELDTYPE[FieldType];
end;

function TFRE_DB_FIELD.ValueCount: NativeInt;
begin
  case FFieldData.FieldType of
    fdbft_NotFound:     result := 0;
    fdbft_GUID:         result := Length(FFieldData.guid^);
    fdbft_Byte:         result := Length(FFieldData.byte^);
    fdbft_Int16:        result := Length(FFieldData.in16^);
    fdbft_UInt16:       result := Length(FFieldData.ui16^);
    fdbft_Int32:        result := Length(FFieldData.in32^);
    fdbft_UInt32:       result := Length(FFieldData.ui32^);
    fdbft_Int64:        result := Length(FFieldData.in64^);
    fdbft_UInt64:       result := Length(FFieldData.ui64^);
    fdbft_Real32:       result := Length(FFieldData.re32^);
    fdbft_Real64:       result := Length(FFieldData.re64^);
    fdbft_Currency:     result := Length(FFieldData.curr^);
    fdbft_String:       begin
                          if FIsSchemeField then
                            exit(1);
                          result := Length(FFieldData.strg^);
                        end;
    fdbft_Boolean:      result := Length(FFieldData.bool^);
    fdbft_DateTimeUTC:  result := Length(FFieldData.date^);
    fdbft_Stream:       result := Length(FFieldData.strm^);
    fdbft_Object:       begin
                          if not IsObjectArray then
                            exit(1); // raise EFRE_DB_Exception.Create(edb_ERROR,'only special object array object fields can be accessed by ValueCountObjArray');
                          if FFieldData.obj is TFRE_DB_OBJECTLIST then
                            result := TFRE_DB_OBJECTLIST(FFieldData.obj).GetCount;
                        end;
    fdbft_ObjLink:      result := Length(FFieldData.obl^);
    else               raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled %s',[CFRE_DB_FIELDTYPE[FFieldData.FieldType]]);
  end;
end;

function TFRE_DB_FIELD.ValueCountReal: NativeInt;
begin
   if IsObjectField then
     result := 1
   else
     result := ValueCount;
end;

function TFRE_DB_FIELD.IsUIDField: boolean;
begin
  _InAccessibleFieldCheck;
  result := FIsUidField;
end;

function TFRE_DB_FIELD.IsDomainIDField: boolean;
begin
  _InAccessibleFieldCheck;
  result := FIsDomainIDField;
end;

function TFRE_DB_FIELD.IsSchemeField: boolean;
begin
  _InAccessibleFieldCheck;
  result := FIsSchemeField;
end;

function TFRE_DB_FIELD.IsSystemField: boolean;
begin
  _InAccessibleFieldCheck;
  result := FIsUidField or FIsSchemeField or FIsDomainIDField;
end;

function TFRE_DB_FIELD.IsObjectField: boolean;
begin
 _InAccessibleFieldCheck;
 result := FieldType=fdbft_Object;
end;

function TFRE_DB_FIELD.IsObjectArray: boolean;
begin
  result := (FieldType=fdbft_Object) and
     (FFieldData.obj is TFRE_DB_OBJECTLIST);
end;

function TFRE_DB_FIELD.IsFieldCalculated: boolean;
begin
  result := assigned(FCalcMethod);
end;


procedure TFRE_DB_FIELD.CloneFromField(const Field: TFRE_DB_FIELD);
var i : NativeInt;
begin
  _InAccessibleFieldCheck;
  Clear;
  if not _CheckStoreType(Field.FieldType) then begin
    if Field.FieldType=fdbft_NotFound then exit; //Empty Fields => Do nothing?
    FFieldData.FieldType := Field.FieldType;
    case field.FieldType of
      fdbft_GUID: begin
                    New(FFieldData.guid);
                    SetLength(FFieldData.guid^,Field.ValueCount);
                    for i := 0 to Field.ValueCount-1 do
                      FFieldData.guid^[i] := Field.AsGUIDItem[i];
                    if IsUIDField then
                      begin
                        if Field.ValueCount<>1 then
                          raise EFRE_DB_Exception.Create('cannot clone a multivalue field to the special uid field');
                        Fobj.FUID := FFieldData.guid^[0];
                      end;
                    if IsDomainIDField then
                      begin
                        if Field.ValueCount<>1 then
                          raise EFRE_DB_Exception.Create('cannot clone a multivalue field to the special domainid field');
                        Fobj.FDomainID := FFieldData.guid^[0];
                      end;
                  end;
      fdbft_Byte: begin
                    New(FFieldData.byte);
                    SetLength(FFieldData.byte^,Field.ValueCount);
                    for i := 0 to Field.ValueCount-1 do
                      FFieldData.byte^[i] := Field.AsByteItem[i];
                  end;
      fdbft_Int16: begin
                     New(FFieldData.in16);
                     SetLength(FFieldData.in16^,Field.ValueCount);
                     for i := 0 to Field.ValueCount-1 do
                       FFieldData.in16^[i] := Field.AsInt16Item[i];
                   end;
      fdbft_UInt16: begin
                      New(FFieldData.ui16);
                      SetLength(FFieldData.ui16^,Field.ValueCount);
                      for i := 0 to Field.ValueCount-1 do
                        FFieldData.ui16^[i] := Field.AsUInt16Item[i];
                    end;
      fdbft_Int32: begin
                     New(FFieldData.in32);
                     SetLength(FFieldData.in32^,Field.ValueCount);
                     for i := 0 to Field.ValueCount-1 do
                       FFieldData.in32^[i] := Field.AsInt32Item[i];
                   end;
      fdbft_UInt32: begin
                      New(FFieldData.ui32);
                      SetLength(FFieldData.ui32^,Field.ValueCount);
                      for i := 0 to Field.ValueCount-1 do
                        FFieldData.ui32^[i] := Field.AsUInt32Item[i];
                    end;
      fdbft_Int64: begin
                     New(FFieldData.in64);
                     SetLength(FFieldData.in64^,Field.ValueCount);
                     for i := 0 to Field.ValueCount-1 do
                       FFieldData.in64^[i] := Field.AsInt64Item[i];
                   end;
      fdbft_UInt64: begin
                      New(FFieldData.ui64);
                      SetLength(FFieldData.ui64^,Field.ValueCount);
                      for i := 0 to Field.ValueCount-1 do
                        FFieldData.ui64^[i] := Field.AsUInt64Item[i];
                    end;
      fdbft_Real32: begin
                      New(FFieldData.re32);
                      SetLength(FFieldData.re32^,Field.ValueCount);
                      for i := 0 to Field.ValueCount-1 do
                        FFieldData.re32^[i] := Field.AsReal32Item[i];
                    end;
      fdbft_Real64: begin
                      New(FFieldData.re64);
                      SetLength(FFieldData.re64^,Field.ValueCount);
                      for i := 0 to Field.ValueCount-1 do
                        FFieldData.re64^[i] := Field.AsReal64Item[i];
                    end;
      fdbft_Currency: begin
                        New(FFieldData.curr);
                        SetLength(FFieldData.curr^,Field.ValueCount);
                        for i := 0 to Field.ValueCount-1 do
                          FFieldData.curr^[i] := Field.AsCurrencyItem[i];
                      end;
      fdbft_String: begin
                      New(FFieldData.strg);
                      SetLength(FFieldData.strg^,Field.ValueCount);
                      for i := 0 to Field.ValueCount-1 do
                        FFieldData.strg^[i] := Field.AsStringItem[i];
                    end;
      fdbft_Boolean: begin
                       New(FFieldData.bool);
                       SetLength(FFieldData.bool^,Field.ValueCount);
                       for i := 0 to Field.ValueCount-1 do
                         FFieldData.bool^[i] := Field.AsBooleanItem[i];
                     end;
      fdbft_DateTimeUTC: begin
                           New(FFieldData.date);
                           SetLength(FFieldData.date^,Field.ValueCount);
                           for i := 0 to Field.ValueCount-1 do
                             FFieldData.date^[i] := Field.AsDateTimeUTCItem[i];
                         end;
      fdbft_Stream: begin
                      New(FFieldData.strm);
                      SetLength(FFieldData.strm^,Field.ValueCount);
                      for i := 0 to Field.ValueCount-1 do
                        begin
                          FFieldData.strm^[i] := TFRE_DB_Stream.Create;
                          Field.AsStreamItem[i].Position:=0;
                          FFieldData.strm^[i].CopyFrom(Field.AsStreamItem[i],0);
                        end;
                    end;
      fdbft_Object: begin
                      //New(FFieldData.obj);
                      //SetLength(FFieldData.obj^,Field.ValueCount);
                      //for i := 0 to Field.ValueCount-1 do
                      FFieldData.obj  := Field.AsObject.CloneToNewObject;
                    end;
      fdbft_ObjLink: begin
                       New(FFieldData.obl);
                       SetLength(FFieldData.obl^,Field.ValueCount);
                       for i := 0 to Field.ValueCount-1 do
                         FFieldData.obl^[i]  := Field.AsObjectLinkItem[i];
                     end;
      else raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled %s (%s)',[CFRE_DB_FIELDTYPE[FFieldData.FieldType],FieldName]);
    end;
  end;
end;

procedure TFRE_DB_FIELD.CloneFromFieldI(const Field: IFRE_DB_FIELD);
begin
  _InAccessibleFieldCheck;
  CloneFromField(Field.Implementor as TFRE_DB_FIELD);
end;

function TFRE_DB_FIELD.GetStreamingSize: TFRE_DB_SIZE_TYPE;
begin
  result := _StreamingSize;
end;

function TFRE_DB_FIELD.AsStringDump: TFRE_DB_String;
begin
  _InAccessibleFieldCheck;
  if ValueCount=1 then
    result := FieldName+' ('+CFRE_DB_FIELDTYPE[FieldType]+') : '+GFRE_DB.StringArray2String(AsStringArr)
  else
    result := FieldName+' ('+CFRE_DB_FIELDTYPE[FieldType]+')['+inttostr(ValueCount)+'] :'+GFRE_DB.StringArray2String(AsStringArr);
end;

procedure TFRE_DB_FIELD.AddGuid(const value: TFRE_DB_GUID);
var A:TFRE_DB_GUIDArray;
    l:Integer;
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_GUID) then begin
    _NotAllowedOnUIDorDomainIDFieldCheck;
    FFieldData.FieldType := fdbft_GUID;
    New(FFieldData.guid);
    SetLength(FFieldData.guid^,1);
    FFieldData.guid^[0] := value;
  end else begin
    _NotAllowedOnUIDorDomainIDFieldCheck;
    A := AsGUIDArr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    SetAsGUIDArray(A);
  end;
end;

procedure TFRE_DB_FIELD.AddByte(const value: Byte);
var A:TFRE_DB_ByteArray;
    l:Integer;
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Byte) then begin
    FFieldData.FieldType := fdbft_Byte;
    New(FFieldData.byte);
    SetLength(FFieldData.byte^,1);
    FFieldData.byte^[0] := value;
  end else begin
    A := AsByteArr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    AsByteArr := A;
  end;
end;

procedure TFRE_DB_FIELD.AddInt16(const value: SmallInt);
var A:TFRE_DB_Int16Array;
    l:Integer;
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Int16) then begin
    FFieldData.FieldType := fdbft_Int16;
    New(FFieldData.in16);
    SetLength(FFieldData.in16^,1);
    FFieldData.in16^[0] := value;
  end else begin
    A := AsInt16Arr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    AsInt16Arr := A;
  end;
end;

procedure TFRE_DB_FIELD.AddUInt16(const value: Word);
var A : TFRE_DB_UInt16Array;
    l : Integer;
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_UInt16) then begin
    FFieldData.FieldType := fdbft_UInt16;
    New(FFieldData.ui16);
    SetLength(FFieldData.ui16^,1);
    FFieldData.ui16^[0] := value;
  end else begin
    A := AsUInt16Arr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    AsUInt16Arr := A;
  end;
end;

procedure TFRE_DB_FIELD.AddInt32(const value: longint);
var A : TFRE_DB_Int32Array;
    l : Integer;
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Int32) then begin
    FFieldData.FieldType := fdbft_Int32;
    New(FFieldData.ui32);
    SetLength(FFieldData.ui32^,1);
    FFieldData.ui32^[0] := value;
  end else begin
    A := AsInt32Arr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    AsInt32Arr := A;
  end;
end;

procedure TFRE_DB_FIELD.AddUInt32(const value: longword);
var A : TFRE_DB_UInt32Array;
    l : Integer;
begin
 _InAccessibleFieldCheck;
 if not _CheckStoreType(fdbft_UInt32) then begin
    FFieldData.FieldType := fdbft_UInt32;
    New(FFieldData.ui32);
    SetLength(FFieldData.ui32^,1);
    FFieldData.ui32^[0] := value;
  end else begin
    A := AsUInt32Arr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    AsUInt32Arr := A;
  end;
end;

procedure TFRE_DB_FIELD.AddInt64(const value: Int64);
var A : TFRE_DB_Int64Array;
    l : Integer;
begin
 _InAccessibleFieldCheck;
 if not _CheckStoreType(fdbft_Int64) then begin
    FFieldData.FieldType := fdbft_Int64;
    New(FFieldData.ui64);
    SetLength(FFieldData.ui64^,1);
    FFieldData.ui64^[0] := value;
  end else begin
    A := AsInt64Arr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    AsInt64Arr := A;
  end;
end;


procedure TFRE_DB_FIELD.AddUInt64(const value: UInt64);
var A : TFRE_DB_UInt64Array;
    l : Integer;
begin
 _InAccessibleFieldCheck;
 if not _CheckStoreType(fdbft_UInt64) then begin
    FFieldData.FieldType := fdbft_UInt64;
    New(FFieldData.ui64);
    SetLength(FFieldData.ui64^,1);
    FFieldData.ui64^[0] := value;
  end else begin
    A := AsUInt64Arr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    AsUInt64Arr := A;
  end;
end;

procedure TFRE_DB_FIELD.AddReal32(const value: single);
var A : TFRE_DB_Real32Array;
    l : Integer;
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Real32) then begin
    FFieldData.FieldType := fdbft_Real32;
    New(FFieldData.re32);
    SetLength(FFieldData.re32^,1);
    FFieldData.re32^[0] := value;
  end else begin
    A := AsReal32Arr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    AsReal32Arr := A;
  end;
end;


procedure TFRE_DB_FIELD.AddReal64(const value: double);
var A:TFRE_DB_Real64Array;
    l:Integer;
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Real64) then begin
    FFieldData.FieldType := fdbft_Real64;
    New(FFieldData.re64);
    SetLength(FFieldData.re64^,1);
    FFieldData.re64^[0] := value;
  end else begin
    A := AsReal64Arr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    AsReal64Arr := A;
  end;
end;

procedure TFRE_DB_FIELD.AddCurrency(const value: Currency);
var A:TFRE_DB_CurrencyArray;
    l:Integer;
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Currency) then begin
    FFieldData.FieldType := fdbft_Currency;
    New(FFieldData.curr);
    SetLength(FFieldData.curr^,1);
    FFieldData.curr^[0] := value;
  end else begin
    A := AsCurrencyArr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    AsCurrencyArr := A;
  end;
end;


procedure TFRE_DB_FIELD.AddString(const value: TFRE_DB_String);
var A:TFRE_DB_StringArray;
    l:Integer;
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_String) then begin
    FFieldData.FieldType := fdbft_String;
    New(FFieldData.strg);
    SetLength(FFieldData.strg^,1);
    FFieldData.strg^[0] := value;
  end else begin
    A := AsStringArr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    AsStringArr := A;
  end;
end;

procedure TFRE_DB_FIELD.AddBoolean(const value: Boolean);
var A:TFRE_DB_BoolArray;
    l:Integer;
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Boolean) then begin
    FFieldData.FieldType := fdbft_Boolean;
    New(FFieldData.bool);
    SetLength(FFieldData.bool^,1);
    FFieldData.bool^[0] := value;
  end else begin
    A := AsBooleanArr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    AsBooleanArr := A;
  end;
end;

procedure TFRE_DB_FIELD.AddDateTime(const value: TFRE_DB_DateTime64);
var A:TFRE_DB_DateTimeArray;
    l:Integer;
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_DateTimeUTC) then begin
    FFieldData.FieldType := fdbft_DateTimeUTC;
    New(FFieldData.date);
    SetLength(FFieldData.date^,1);
    FFieldData.date^[0] := value;
  end else begin
    A := AsDateTimeUTCArr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := GFRE_DB.LocalTimeToUTCDB64(Value);
    AsDateTimeUTCArr := A;
  end;
end;

procedure TFRE_DB_FIELD.AddDateTimeUTC(const value: TFRE_DB_DateTime64);
var A:TFRE_DB_DateTimeArray;
    l:Integer;
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_DateTimeUTC) then begin
    FFieldData.FieldType := fdbft_DateTimeUTC;
    New(FFieldData.date);
    SetLength(FFieldData.date^,1);
    FFieldData.date^[0] := value;
  end else begin
    A := AsDateTimeUTCArr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    AsDateTimeUTCArr := A;
  end;
end;

procedure TFRE_DB_FIELD.AddStream(const value: TFRE_DB_Stream);
var A:TFRE_DB_StreamArray;
    l:Integer;
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Stream) then begin
    FFieldData.FieldType := fdbft_Stream;
    New(FFieldData.strm);
    SetLength(FFieldData.strm^,1);
    FFieldData.strm^[0] := value;
  end else begin
    A := AsStreamArr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    AsStreamArr := A;
  end;
end;

procedure TFRE_DB_FIELD.AddObject(const value: TFRE_DB_Object);
var A:TFRE_DB_ObjectArray;
    l:Integer;
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Object) then begin
    SetAsObject(TFRE_DB_OBJECTLIST.Create);
  end;
  if FFieldData.obj is TFRE_DB_OBJECTLIST then
    TFRE_DB_OBJECTLIST(FFieldData.obj).AddObject(value)
  else
    raise EFRE_DB_Exception.Create(edb_MISMATCH,'try to access an object array/list but wrong class stored');
end;

procedure TFRE_DB_FIELD.AddObjectLink(const value: TFRE_DB_GUID);
var A                   : TFRE_DB_ObjLinkArray;
    l                   : Integer;
    old_array,new_array : TFRE_DB_GUIDArray;
begin
  _InAccessibleFieldCheck;
  if FREDB_Guids_Same(Value,Fobj.UID) then raise EFRE_DB_Exception.Create(edb_ERROR,'addlink :referencing a objectlink to self seems to be sensless');
  if not _CheckStoreType(fdbft_ObjLink) then begin
    FFieldData.FieldType := fdbft_ObjLink;
    New(FFieldData.obl);
    SetLength(FFieldData.obl^,1);
    FFieldData.obl^[0] := value;
  end else begin
    A := AsObjectLinkArray;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    SetAsObjectLinkArray(A);
  end;
end;

procedure TFRE_DB_FIELD.RemoveGuid(const idx: integer);
var A:TFRE_DB_GUIDArray;
    hi:integer;
    i,k:Integer;
begin
  _InAccessibleFieldCheck;
  _GetHigh(hi);
  SetLength(A,hi);
  k:=0;
  for i:=0 to hi do begin
    if i<>idx then begin
      a[k]:=AsGUIDArr[i];
      inc(k);
    end;
  end;
  AsGUIDArr:=a;
end;

procedure TFRE_DB_FIELD.RemoveByte(const idx: integer);
var A:TFRE_DB_ByteArray;
    hi:integer;
    i,k:Integer;
begin
 _InAccessibleFieldCheck;
  _GetHigh(hi);
  SetLength(A,hi);
  k:=0;
  for i:=0 to hi do begin
    if i<>idx then begin
      a[k]:=AsByteArr[i];
      inc(k);
      end;
  end;
  AsByteArr:=a;
end;

procedure TFRE_DB_FIELD.RemoveInt16(const idx: integer);
var A:TFRE_DB_Int16Array;
    hi:integer;
    i,k:Integer;
begin
 _InAccessibleFieldCheck;
  _GetHigh(hi);
  SetLength(A,hi);
  k:=0;
  for i:=0 to hi do begin
    if i<>idx then begin
      a[k]:=AsInt16Arr[i];
      inc(k);
    end;
  end;
  AsInt16Arr:=a;
end;

procedure TFRE_DB_FIELD.RemoveUInt16(const idx: integer);
var A:TFRE_DB_UInt16Array;
    hi:integer;
    i,k:Integer;
begin
 _InAccessibleFieldCheck;
  _GetHigh(hi);
  SetLength(A,hi);
  k:=0;
  for i:=0 to hi do begin
    if i<>idx then begin
      a[k]:=AsUInt16Arr[i];
      inc(k);
    end;
  end;
  AsUInt16Arr:=a;
end;

procedure TFRE_DB_FIELD.RemoveInt32(const idx: integer);
var A:TFRE_DB_Int32Array;
    hi:integer;
    i,k:Integer;
begin
 _InAccessibleFieldCheck;
  _GetHigh(hi);
  SetLength(A,hi);
  k:=0;
  for i:=0 to hi do begin
    if i<>idx then begin
      a[k]:=AsInt32Arr[i];
      inc(k);
    end;
  end;
  AsInt32Arr:=a;
end;

procedure TFRE_DB_FIELD.RemoveUInt32(const idx: integer);
var A:TFRE_DB_UInt32Array;
    hi:integer;
    i,k:Integer;
begin
 _InAccessibleFieldCheck;
  _GetHigh(hi);
 SetLength(A,hi);
 k:=0;
 for i:=0 to hi do begin
  if i<>idx then begin
   a[k]:=AsUInt32Arr[i];
   inc(k);
  end;
 end;
 AsUInt32Arr:=a;
end;

procedure TFRE_DB_FIELD.RemoveInt64(const idx: integer);
var A:TFRE_DB_Int64Array;
    hi:integer;
    i,k:Integer;
begin
 _InAccessibleFieldCheck;
  _GetHigh(hi);
 SetLength(A,hi);
 k:=0;
 for i:=0 to hi do begin
  if i<>idx then begin
   a[k]:=AsInt64Arr[i];
   inc(k);
  end;
 end;
 AsInt64Arr:=a;
end;

procedure TFRE_DB_FIELD.RemoveUInt64(const idx: integer);
var A:TFRE_DB_UInt64Array;
    hi:integer;
    i,k:Integer;
begin
 _InAccessibleFieldCheck;
  _GetHigh(hi);
 SetLength(A,hi);
 k:=0;
 for i:=0 to hi do begin
  if i<>idx then begin
   a[k]:=AsUInt64Arr[i];
   inc(k);
  end;
 end;
 AsUInt64Arr:=a;
end;

procedure TFRE_DB_FIELD.RemoveReal32(const idx: integer);
var A:TFRE_DB_Real32Array;
    hi:integer;
    i,k:Integer;
begin
 _InAccessibleFieldCheck;
  _GetHigh(hi);
 SetLength(A,hi);
 k:=0;
 for i:=0 to hi do begin
  if i<>idx then begin
   a[k]:=AsReal32Arr[i];
   inc(k);
  end;
 end;
 AsReal32Arr:=a;
end;

procedure TFRE_DB_FIELD.RemoveReal64(const idx: integer);
var A:TFRE_DB_Real64Array;
    hi:integer;
    i,k:Integer;
begin
 _InAccessibleFieldCheck;
  _GetHigh(hi);
 SetLength(A,hi);
 k:=0;
 for i:=0 to hi do begin
  if i<>idx then begin
   a[k]:=AsReal64Arr[i];
   inc(k);
  end;
 end;
 AsReal64Arr:=a;
end;

procedure TFRE_DB_FIELD.RemoveCurrency(const idx: integer);
var A:TFRE_DB_CurrencyArray;
    hi:integer;
    i,k:Integer;
begin
 _InAccessibleFieldCheck;
  _GetHigh(hi);
 SetLength(A,hi);
 k:=0;
 for i:=0 to hi do begin
  if i<>idx then begin
   a[k]:=AsCurrencyArr[i];
   inc(k);
  end;
 end;
 AsCurrencyArr:=a;
end;

procedure TFRE_DB_FIELD.RemoveString(const idx: integer);
var A:TFRE_DB_StringArray;
    hi:integer;
    i,k:Integer;
begin
 _InAccessibleFieldCheck;
  _GetHigh(hi);
 SetLength(A,hi);
 k:=0;
 for i:=0 to hi do begin
  if i<>idx then begin
   a[k]:=AsStringArr[i];
   inc(k);
  end;
 end;
 AsStringArr:=a;
end;


procedure TFRE_DB_FIELD.RemoveBoolean(const idx: integer);
var A:TFRE_DB_BoolArray;
    hi:integer;
    i,k:Integer;
begin
 _InAccessibleFieldCheck;
  _GetHigh(hi);
 SetLength(A,hi);
 k:=0;
 for i:=0 to hi do begin
  if i<>idx then begin
   a[k]:=AsBooleanArr[i];
   inc(k);
  end;
 end;
 AsBooleanArr:=a;
end;


procedure TFRE_DB_FIELD.RemoveDateTimeUTC(const idx: integer);
var A:TFRE_DB_DateTimeArray;
    hi:integer;
    i,k:Integer;
begin
  _InAccessibleFieldCheck;
  _GetHigh(hi);
 SetLength(A,hi);
 k:=0;
 for i:=0 to hi do begin
  if i<>idx then begin
   a[k]:=AsDateTimeArr[i];
   inc(k);
  end;
 end;
 AsDateTimeArr:=a;
end;

procedure TFRE_DB_FIELD.RemoveStream(const idx: integer);
var A:TFRE_DB_StreamArray;
    hi:integer;
    i,k:Integer;
begin
 _InAccessibleFieldCheck;
 _GetHigh(hi);
 SetLength(A,hi);
 k:=0;
 for i:=0 to hi do begin
  if i<>idx then begin
   a[k]:=AsStreamArr[i];
   inc(k);
  end;
 end;
 AsStreamArr:=a;
end;

procedure TFRE_DB_FIELD.RemoveObject(const idx: integer);
var A:TFRE_DB_ObjectArray;
    hi:integer;
    i,k:Integer;
begin
  _InAccessibleFieldCheck;
  if not _CheckStoreType(fdbft_Object) then begin
    FFieldData.FieldType := fdbft_Object;
    FFieldData.obj:=TFRE_DB_OBJECTLIST.Create;
  end;
  if FFieldData.obj is TFRE_DB_OBJECTLIST then
    TFRE_DB_OBJECTLIST(FFieldData.obj).RemoveObject(idx)
  else
    raise EFRE_DB_Exception.Create(edb_MISMATCH,'try to access an object array/list but wrong class stored');
end;

procedure TFRE_DB_FIELD.RemoveObjectLink(const idx: integer);
var A:TFRE_DB_ObjLinkArray;
    hi:integer;
    i,k:Integer;
begin
  _InAccessibleFieldCheck;
  _GetHigh(hi);
  SetLength(A,hi);
  k:=0;
  for i:=0 to hi do begin
   if i<>idx then begin
    a[k]:=AsObjectLinkArray[i];
    inc(k);
   end;
  end;
  AsObjectLinkArray:=a;
end;

function TFRE_DB_FIELD.RemoveObjectLinkByUID(const to_remove_uid: TFRE_DB_GUID): boolean;
var idx : NativeInt;
begin
  if FieldType<>fdbft_ObjLink then
    raise EFRE_DB_Exception.Create(edb_ERROR,'must be a objectlinkfield for RemoveObjectLinkByUId');
 idx := FREDB_GuidInArray(to_remove_uid,AsObjectLinkArray);
 if idx=-1 then
   exit(false);
 RemoveObjectLink(idx);
end;


function TFRE_DB_FIELD.GetAsJSON(const without_uid: boolean; const full_dump: boolean; const stream_cb: TFRE_DB_StreamingCallback; const plain_objects_as_array: boolean): TJSONData;
var i:integer;
    ti:int64;

    const lCLEVTRANS : ARRAY[TFRE_DB_Compresslevel] of Tcompressionlevel = (clnone,clfastest,clmax);

    function HandleStream(const mem:Pointer;const size:Integer;const field_name:string;const stream:TFRE_DB_Stream):TJSONData;
    var
      zipstream : Tcompressionstream;
      mstream   : TBase64EncodingStream;
      datastream: TStringStream;
    begin
      if stream_cb=nil then begin
         datastream := TStringStream.Create('');
         mstream    := TBase64EncodingStream.Create(datastream);
         zipstream  := Tcompressionstream.create(lCLEVTRANS[GCFG_DB_BACKUP_COMPRESSLEVEL],mstream);
         try
           zipstream.write(mem^,Size);
           zipstream.flush;
           mstream.Flush;
           result:=TJSONString.Create('I|'+datastream.DataString);
         finally
           zipstream.free;
           mstream.free;
           datastream.free;
         end;
       end else begin
         result:=TJSONString.Create('E|'+FFieldName^+'|'+FREDB_CombineString(FObj.GetUIDPath,'_'));
         if assigned(stream_cb) then stream_cb(FObj.UID_String,field_name,stream);
       end;
    end;

 function GetJSON(const index:integer):TJSONData;
 var oa        : TFRE_DB_Object;
     conv      : TFRE_DB_String;
 begin
   case FFieldData.FieldType of
     fdbft_NotFound:      result:=TJSONNull.Create;
     fdbft_GUID:          result:=TJSONString.Create(_ConvertToString(index));
     fdbft_Byte:          result:=TJSONIntegerNumber.Create(GetAsByteList(index));
     fdbft_Int16:         result:=TJSONIntegerNumber.Create(GetAsInt16List(index));
     fdbft_UInt16:        result:=TJSONIntegerNumber.Create(GetAsUInt16List(index));
     fdbft_Int32:         result:=TJSONIntegerNumber.Create(GetAsInt32List(index));
     fdbft_UInt32:        result:=TJSONint64Number.Create(GetAsUInt32List(index));
     fdbft_Int64:         result:=TJSONInt64Number.Create(GetAsInt64List(index));
     fdbft_UInt64:        result:=TJSONQWordNumber.Create(GetAsUInt64List(index));
     fdbft_Real32:        result:=TJSONFloatNumber.Create(GetAsSingleList(index));
     fdbft_Real64:        result:=TJSONFloatNumber.Create(GetAsDoubleList(index));
     fdbft_Currency:      result:=TJSONFloatNumber.Create(GetAsCurrencyList(index));
     fdbft_String:        begin
                            if not IsSpecialClearMarked then
                              result:=TJSONString.Create(GetAsStringList(index))       { TODO -> think about UTF8 / encoding / decoding }
                            else
                              result:=TJSONNull.Create;
                          end;
     fdbft_Boolean:       result:=TJSONBoolean.Create(GetAsBooleanList(index));
     fdbft_DateTimeUTC:   result:=TJSONInt64Number.Create(GetAsDateTimeListUTC(index));
     fdbft_Stream:        begin
                            result := HandleStream(FFieldData.strm^[index].Memory,FFieldData.strm^[index].Size,FFieldName^,FFieldData.strm^[index]);
                          end;
     fdbft_Object:        begin
                            if not IsObjectArray then
                              result:= GetAsObject.GetAsJSON(without_uid,full_dump,stream_cb)
                            else
                              result:= AsObjectArr[index].GetAsJSON(without_uid,full_dump,stream_cb);
                          end;
     fdbft_ObjLink:     result:=TJSONString.Create(_ConvertToString(index));
     else               raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled '+CFRE_DB_FIELDTYPE[FFieldData.FieldType]);
   end;
 end;

 function GetJSON_String(const index:integer):TJSONData;
 begin
   case FFieldData.FieldType of
     fdbft_NotFound:      result:=TJSONNull.Create;
     fdbft_GUID:          result:=TJSONString.Create(FREDB_G2H(GetAsGUIDList(index)));
     fdbft_Byte:          result:=TJSONString.Create(IntTostr(GetAsByteList(index)));
     fdbft_Int16:         result:=TJSONString.Create(IntTostr(GetAsInt16List(index)));
     fdbft_UInt16:        result:=TJSONString.Create(IntTostr(GetAsUInt16List(index)));
     fdbft_Int32:         result:=TJSONString.Create(IntTostr(GetAsInt32List(index)));
     fdbft_UInt32:        result:=TJSONString.Create(IntTostr(GetAsUInt32List(index)));
     fdbft_Int64:         result:=TJSONString.Create(IntTostr(GetAsInt64List(index)));
     fdbft_UInt64:        result:=TJSONString.Create(IntTostr(GetAsUInt64List(index)));
     fdbft_Real32:        result:=TJSONString.Create(FloatToStr(GetAsSingleList(index)));
     fdbft_Real64:        result:=TJSONString.Create(FloatToStr(GetAsDoubleList(index)));
     fdbft_Currency:      result:=TJSONString.Create(CurrToStr(GetAsCurrencyList(index)));
     fdbft_String:        result:=TJSONString.Create(EncodeStringBase64(GetAsStringList(index)));
     fdbft_Boolean:       result:=TJSONString.Create(BoolToStr(GetAsBooleanList(index)));
     fdbft_DateTimeUTC:   result:=TJSONString.Create(IntTostr(GetAsDateTimeListUTC(index)));
     fdbft_Stream:        begin
                            result := HandleStream(FFieldData.strm^[index].Memory,FFieldData.strm^[index].Size,FFieldName^,FFieldData.strm^[index]);
                          end;
     fdbft_Object:        begin
                            assert(index=0,'internal fail');
                            result:= GetAsObject.GetAsJSON(without_uid,full_dump,stream_cb);
                          end;
     fdbft_ObjLink:     result:=TJSONString.Create(FREDB_G2H(GetAsObjectLinkList(index)));
     else               raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled '+CFRE_DB_FIELDTYPE[FFieldData.FieldType]);
   end;
 end;

 function EncodeField: TJSONData;
 var i                   : integer;
     valuecount_w_arrays : NativeInt;
     oarr                : TFRE_DB_ObjectArray;
 begin
   if not full_dump then
     begin  { "Web" Dump }
       valuecount_w_arrays := ValueCount;
       if valuecount_w_arrays=0 then begin
         result:=TJSONArray.Create;
       end else
       if (valuecount_w_arrays=1) and (not plain_objects_as_array) then
         begin
           result := GetJSON(0);
         end
       else
         begin
           result:=TJSONArray.Create;
           if IsObjectArray then
             begin
               oarr := AsObjectArr;
               for i:=0 to high(oarr) do begin
                 TJSONArray(result).Add(oarr[i].GetAsJSON(without_uid,full_dump,stream_cb));
               end;
             end
           else
             begin
               for i:=0 to valuecount_w_arrays-1 do
                 TJSONArray(result).Add(GetJSON(i));
             end;
         end;
     end
   else
     begin  { Full Dump}
       if ValueCountReal=0 then begin
         result:=TJSONArray.Create;
       end else begin
         result:=TJSONArray.Create;
         for i:=0 to ValueCountReal-1 do
           TJSONArray(result).Add(GetJSON_String(i));
       end;
     end;
 end;

begin
  if not full_dump then begin
    result := EncodeField;
  end else begin
    result := TJSONObject.Create;
    TJSONObject(result).Add('N',FFieldName^);
    TJSONObject(result).Add('T',CFRE_DB_FIELDTYPE_SHORT[FFieldData.FieldType]);
    TJSONObject(result).Add('D',EncodeField);
  end;
end;

procedure TFRE_DB_FIELD.SetFromJSON(const field_type: TFRE_DB_FIELDTYPE; const json_object: TJSONArray;const stream_cb:TFRE_DB_StreamingCallback);

  procedure SetFrom(const jo : TJSONData);
  var new_stream  : TFRE_DB_Stream;
      streamkey   : TFRE_DB_String;
      uida        : TFRE_DB_StringArray;
      streamdata  : PByte;
      dzipstream  : Tdecompressionstream;
      mstream     : TBase64DecodingStream;
      datastream  : TStringStream;
      ddata       : TFOS_ReadOnlyMemStream;

      dmstream    : TBase64DecodingStream;
      ssm         : TStringStream;
      i           : integer;
      conv        : TFRE_DB_String;

    function JAsString:TFRE_DB_String;
    begin
      result := (jo as TJSONString).AsString;
    end;

  begin
    case Field_Type of
      fdbft_NotFound:      raise EFRE_DB_Exception.Create(edb_INTERNAL,'cannot json/stream create a undefined field');
      fdbft_GUID:
        if (uppercase(FieldName)='UID') or
           (uppercase(FieldName)='DOMAINID') then
             begin
               SetAsGUID(FREDB_H2G(JAsString));
             end
           else
             AddGuid(FREDB_H2G(JAsString));
      fdbft_Byte:          AddByte(StrToInt(JAsString));
      fdbft_Int16:         AddInt16(StrToInt(JAsString));
      fdbft_UInt16:        AddUInt16(StrToInt(JAsString));
      fdbft_Int32:         AddInt32(StrToInt(JAsString));
      fdbft_UInt32:        AddUInt32(StrToInt(JAsString));
      fdbft_Int64:         AddInt64(StrToInt64(JAsString));
      fdbft_UInt64:        AddUInt64(StrToQWord(JAsString));
      fdbft_Real32:        AddReal32(StrToFloat(JAsString));
      fdbft_Real64:        AddReal64(Extended(StrToFloat(JAsString)));
      fdbft_Currency:      AddCurrency(StrToCurr(JAsString));
      fdbft_String:
        try
          conv := JAsString;
          if conv<>'' then
            AddString(DecodeStringBase64(JAsString))//,true))
          else
            AddString('');
        except
          writeln('Base64 K',JAsString);
          halt;
        end;
      fdbft_Boolean:       AddBoolean(StrToBool(JAsString));
      fdbft_DateTimeUTC:   AddDateTimeUTC(StrToInt64(JAsString));
      fdbft_Stream:        begin
                             streamkey  := Copy(JAsString,1,1);
                             if streamkey='I' then begin
                               streamdata  := @JAsString[3];
                               ddata       := TFOS_ReadOnlyMemStream.Create(streamdata,Length(JAsString)-2);
                               ddata.Position:=0;
                               dmstream    := TBase64DecodingStream.Create(ddata);
                               ddata.Position :=0;
                               dzipstream  := Tdecompressionstream.create(dmstream);
                               try
                                 new_stream  := TFRE_DB_Stream.Create;
                                 new_stream.CopyFrom(dzipstream,0);
                                 new_stream.Position:=0;
                                 AddStream(new_stream);
                               finally
                                 dzipstream.free;
                                 dmstream.free;
                                 ddata.Free;
                               end;
                             end else
                             if streamkey='E' then begin
                               if assigned(stream_cb) then begin
                                 new_stream := TFRE_DB_Stream.Create;
                                 AddStream(new_stream);
                                 streamkey := GFRE_BT.SepRight(streamkey,'|');
                                 streamkey := GFRE_BT.SepRight(streamkey,'|');
                                 FREDB_SeperateString(streamkey,'_',uida);
                                 stream_cb(uida[high(uida)],FFieldName^,new_stream);
                               end else begin
                                 raise EFRE_DB_Exception.Create(edb_ERROR,'found external BLOB/Stream reference, but no streaming callback');
                               end;
                             end;
                           end;
      fdbft_Object:        begin
                             SetAsObject(TFRE_DB_Object.CreateInternalStreamingJSON(self,jo as TJSONArray,stream_cb));
                           end;
      fdbft_ObjLink:       begin
                             //AsObjectLinkArray := FREDB_StreamString2GuidArray(JAsString);
                             AddObjectLink(FREDB_H2G(JAsString));
                           end;
      else               raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled '+CFRE_DB_FIELDTYPE[FFieldData.FieldType]);
    end;
  end;

var  i: Integer;
begin {used from fulldump, so strings are base64 encoded}
  for i:=0 to json_object.Count-1 do begin
    SetFrom(json_object.Items[i]);
  end;
end;

procedure TFRE_DB_FIELD.Stream2String(var raw_string: TFRE_DB_RawByteString);
begin
  _InAccessibleFieldCheck;
  _CheckFieldType(fdbft_Stream);
  SetLength(raw_string,FFieldData.strm^[0].Size);
  Move(FFieldData.strm^[0].Memory^,raw_string[1],FFieldData.strm^[0].Size);
end;

function TFRE_DB_FIELD.AsObjectArrayJSONString: TFRE_DB_String;
var jsond : TJSONData;
begin
  jsond := GetAsJSON(false,false,nil,true);
  try
    result := jsond.AsJSON;
  finally
    jsond.Free;
  end;
end;


function TFRE_DB_FIELD.FieldName: TFRE_DB_NameType;
begin
  if assigned(FFieldName) then
    result := FFieldName^
  else
    result := FManualFieldName;
end;

procedure TFRE_DB_FIELD.Clear(const dont_free_streams_and_objects: boolean);
    procedure DisposeStream;
    var  i: Integer;
    begin
      if not dont_free_streams_and_objects then
        for i:=0 to high(FFieldData.strm^) do begin
          FFieldData.strm^[i].Free;
        end;
      Dispose(FFieldData.strm);
    end;

    procedure DisposeObject;
    begin
      if not dont_free_streams_and_objects then
        FFieldData.obj.Free;
    end;
begin
  _InAccessibleFieldCheck;
  case FieldType of
    fdbft_NotFound:     ;
    fdbft_GUID:         Dispose(FFieldData.guid);
    fdbft_Byte:         Dispose(FFieldData.byte);
    fdbft_Int16:        Dispose(FFieldData.in16);
    fdbft_UInt16:       Dispose(FFieldData.ui16);
    fdbft_Int32:        Dispose(FFieldData.in32);
    fdbft_UInt32:       Dispose(FFieldData.ui32);
    fdbft_Int64:        Dispose(FFieldData.in64);
    fdbft_UInt64:       Dispose(FFieldData.ui64);
    fdbft_Real32:       Dispose(FFieldData.re32);
    fdbft_Real64:       Dispose(FFieldData.re64);
    fdbft_Currency:     Dispose(FFieldData.curr);
    fdbft_String:       if not FIsSchemeField then
                          Dispose(FFieldData.strg);
    fdbft_Boolean:      Dispose(FFieldData.bool);
    fdbft_DateTimeUTC:  Dispose(FFieldData.date);
    fdbft_Stream:       DisposeStream;
    fdbft_Object:       DisposeObject;
    fdbft_ObjLink:      Dispose(FFieldData.obl);
  end;
  FFieldData.FieldType := fdbft_NotFound;
  FFieldData.bool      := nil;
end;

procedure TFRE_DB_FIELD.RemoveIndex(const idx: integer);
var hi:integer;
begin
  if IsFieldCalculated then
    raise EFRE_DB_Exception.Create(edb_ERROR,'cannot remove indices for calculated fields');
  _InAccessibleFieldCheck;
  _GetHigh(hi);
  if (idx>=0) and (idx<=hi) then begin
    case FFieldData.FieldType of
      fdbft_NotFound:      exit;
      fdbft_GUID:          RemoveGuid(idx);
      fdbft_Byte:          RemoveByte(idx);
      fdbft_Int16:         RemoveInt16(idx);
      fdbft_UInt16:        RemoveUInt16(idx);
      fdbft_Int32:         RemoveInt32(idx);
      fdbft_UInt32:        RemoveUInt32(idx);
      fdbft_Int64:         RemoveInt64(idx);
      fdbft_UInt64:        RemoveUInt64(idx);
      fdbft_Real32:        RemoveReal32(idx);
      fdbft_Real64:        RemoveReal64(idx);
      fdbft_Currency:      RemoveCurrency(idx);
      fdbft_String:        RemoveString(idx);
      fdbft_Boolean:       RemoveBoolean(idx);
      fdbft_DateTimeUTC:   RemoveDateTimeUTC(idx);
      fdbft_Stream:        RemoveStream(idx);
      fdbft_Object:        RemoveObject(idx);
      fdbft_ObjLink:       RemoveObjectLink(idx);
      else raise Exception.Create('not all cases handled');
    end;
  end;
end;

class function TFRE_DB_FIELD.OrderFieldCompare(const ofieldname: TFRE_DB_String; const o1, o2: TFRE_DB_Object): integer;
var f1,f2      : TFRE_DB_FIELD;
    vi1,vi2    : Int64;
    vu1,vu2    : UInt64;
    r1,r2      : Double;
    c1,c2      : Currency;
    b1,b2      : boolean;
    s1,s2      : TFRE_DB_String;
begin
  f1 := o1._FieldOnlyExisting(ofieldname);
  f2 := o2._FieldOnlyExisting(ofieldname);
  if (f1=nil) and  (f2=nil) then exit(0);
  if f1=nil then exit(-1);
  if f2=nil then exit(1);
  if f1.FieldType<>f2.FieldType then raise EFRE_DB_Exception.Create(edb_MISMATCH,'ORDERFIELDCOMPARE GUIDS= %s / %s  FIELD=%s  TYPES = %s / %s',[FREDB_G2H(o1.UID),FREDB_G2H(o2.UID),ofieldname,CFRE_DB_FIELDTYPE[f1.FieldType],CFRE_DB_FIELDTYPE[f2.FieldType]]);
  case f1.FieldType of
    fdbft_GUID:       begin
                        result:=RB_Guid_Compare(f1.AsGUID,f2.AsGUID);
                        exit;
                      end;
    fdbft_Byte,
    fdbft_UInt16,
    fdbft_UInt32,
    fdbft_UInt64:      begin
                         vu1 := f1.AsUInt64;
                         vu2 := f2.AsUInt64;
                         if vu1=vu2 then exit(0);
                         if vu1<vu2 then exit(-1) else exit(1);
                      end;
    fdbft_Int16,
    fdbft_Int32,
    fdbft_Int64,
    fdbft_DateTimeUTC: begin
                          vi1 := f1.AsInt64;
                          vi2 := f2.AsInt64;
                          if vi1=vi2 then exit(0);
                          if vi1<vi2 then exit(-1) else exit(1);
                       end;
    fdbft_Real32,
    fdbft_Real64:      begin
                         r1 := f1.AsReal64;
                         r2 := f2.AsReal64;
                         if r1=r2 then exit(0);
                         if r1<r2 then exit(-1) else exit(1);
                       end;
    fdbft_Currency:    begin
                         c1 := f1.AsCurrency;
                         c2 := f2.AsCurrency;
                         if c1=c2 then exit(0);
                         if c1<c2 then exit(-1) else exit(1);
                       end;
    fdbft_String:      begin
                          s1 := f1.AsString;
                          s2 := f2.AsString;
                          Result := CompareStr(s1,s2);
                          if result=0 then exit(0);
                          exit(result)
                      end;
    fdbft_Boolean:    begin
                          b1 := f1.AsBoolean;
                          b2 := f2.AsBoolean;
                          if b1=b2 then exit(0);
                          if b1<b2 then exit(-1) else exit(1);
                       end;
    else raise EFRE_DB_Exception.Create(edb_INTERNAL,'ORDERFIELDCOMPARE NOT IMPLEMENTED FOR %s',[CFRE_DB_FIELDTYPE[f1.FieldType]]);
  end;
end;

function TFRE_DB_FIELD.GetAsObjectI: IFRE_DB_Object;
begin
  GetAsObject.IntfCast(IFRE_DB_Object,result);
end;

procedure TFRE_DB_FIELD.SetAsObjectI(const val: IFRE_DB_Object);
begin
  SetAsObject(val.Implementor as TFRE_DB_Object);
end;

function TFRE_DB_FIELD.GetAsObjectArrayI: IFRE_DB_ObjectArray;
begin
  result := GFRE_DB.ConstructObjectArrayOI(GetAsObjectArray);
end;

procedure TFRE_DB_FIELD.SetAsObjectArrayI(const AValue: IFRE_DB_ObjectArray);
begin
  SetAsObjectArray(GFRE_DB.ConstructObjectArrayIO(AValue));
end;


function TFRE_DB_FIELD.GetAsObjectListI(idx: Integer): IFRE_DB_Object;
begin
  result := GetAsObjectList(idx);
end;

procedure TFRE_DB_FIELD.SetAsObjectListI(idx: Integer; const AValue: IFRE_DB_Object);
begin
  SetAsObjectListI(idx,AValue.Implementor as TFRE_DB_Object);
end;

procedure TFRE_DB_FIELD.AddObjectI(const obj: IFRE_DB_Object);
begin
  AddObject(obj.Implementor as TFRE_DB_Object);
end;

function TFRE_DB_FIELD.ParentObject: TFRE_DB_Object;
begin
  result := Fobj;
end;

function TFRE_DB_FIELD.ParentObjectI: IFRE_DB_Object;
begin
  result := ParentObject;
end;

function TFRE_DB_USER.GetLogin: TFRE_DB_String;
begin
  result := Field('login').AsString;
end;

function TFRE_DB_USER.GetLoginAtDomain(conn: IFRE_DB_SYS_CONNECTION): TFRE_DB_NameType;
begin
  result := login+'@'+conn.FetchDomainNameById(GetDomainIDLink);
end;

function TFRE_DB_USER.GetDomainIDLink: TFRE_DB_GUID;
begin
  result := Field('DOMAINIDLINK').AsObjectLink;
end;

function TFRE_DB_USER.GetFirstName: TFRE_DB_String;
begin
  result := Field('firstname').AsString;
end;

function TFRE_DB_USER.GetIsInternal: Boolean;
begin
  Result:=Field('internal').AsBoolean;
end;

function TFRE_DB_USER.GetUserGroupIDS: TFRE_DB_ObjLinkArray;
begin
 result := Field('usergroupids').AsObjectLinkArray;
end;

function TFRE_DB_USER.GetLastName: TFRE_DB_String;
begin
  result := Field('lastname').AsString;
end;

procedure TFRE_DB_USER.SetFirstName(const AValue: TFRE_DB_String);
begin
  Field('firstname').AsString := AValue;
end;

procedure TFRE_DB_USER.SetGIDA(AValue: TFRE_DB_ObjLinkArray);
begin
  Field('usergroupids').AsObjectLinkArray := AValue;
end;

procedure TFRE_DB_USER.SetIsInternal(AValue: Boolean);
begin
  Field('internal').AsBoolean:=AValue;
end;

procedure TFRE_DB_USER.SetLastName(const AValue: TFRE_DB_String);
begin
  Field('lastname').AsString := AValue;
end;

procedure TFRE_DB_USER.Setlogin(const AValue: TFRE_DB_String);
begin
  Field('login').AsString := AValue;
  _UpdateDomainLoginKey;
end;

procedure TFRE_DB_USER.SetDomainIDLink(AValue: TFRE_DB_GUID);
begin
  Field('domainidlink').AsObjectLink := AValue;
  _UpdateDomainLoginKey;
end;

procedure TFRE_DB_USER._UpdateDomainLoginKey;
begin
  field('domainloginkey').AsString := GetDomainLoginKey(login,GetDomainIDLink);
end;

procedure TFRE_DB_USER._calcDisplayName(const calc: IFRE_DB_CALCFIELD_SETTER);
var
  dname:String;
begin
  dname:=Field('login').AsString;
  if Field('lastname').AsString<>'' then begin
    dname:=dname+' ('+Field('lastname').AsString;
    if Field('firstname').AsString<>'' then begin
      dname:=dname+' '+Field('firstname').AsString+')';
    end else begin
      dname:=dname+')';
    end;
  end else begin
    if Field('firstname').AsString<>'' then begin
      dname:=dname+' ('+Field('firstname').AsString+')';
    end;
  end;
  calc.SetAsString(dname);
end;

function TFRE_DB_USER.SubFormattedDisplayAvailable: boolean;
begin
  result := true;
end;

function TFRE_DB_USER.GetSubFormattedDisplay(indent: integer): TFRE_DB_String;
begin
  //FIXME
 // Result := StringOfChar(' ',indent)+GFRE_DB.StringArray2String(UserGroupNames);
end;

procedure TFRE_DB_USER.SetImage(const image_stream: TFRE_DB_Stream; const streamtype: string; const etag: string);
begin
  Field('picture').AsStream                 := image_stream;
  Field('picture'+cFRE_DB_STKEY).AsString   := streamtype;
  Field('picture'+cFRE_DB_ST_ETAG).AsString := etag;
end;

procedure TFRE_DB_USER.InitData(const nlogin, nfirst, nlast, npasswd: TFRE_DB_String; const userdomainid: TFRE_DB_GUID; const is_internal: Boolean; const long_desc, short_desc: TFRE_DB_String);
begin
  SetDomainID(userdomainid);
  SetDomainIDLink(userdomainid);
  Login          := nlogin;
  Firstname      := nfirst;
  Lastname       := nlast;
  isInternal     := is_internal;
  Field('desc').AsDBText.Setlong(long_desc);
  Field('desc').AsDBText.SetShort(short_desc);
  SetPassword(npasswd);
end;

procedure TFRE_DB_USER.SetPassword(const pw: TFRE_DB_String);
var salt : string;
    i    : integer;
    s    : string;
begin
  //Field('passwordMD5').AsString:=GFRE_BT.HashString_MD5_HEX(pw);
 SetLength(salt,8);
 For i:=1 to 8 do
     salt[i]:=char(Random(256));
 s := GFRE_BT.CalcSaltedSH1Password(pw,salt);
 Field('passwordSHA1').AsString:=s;
end;

function TFRE_DB_USER.Checkpassword(const pw: TFRE_DB_String): boolean;
begin
  try
    result := GFRE_BT.VerifySaltedSHA1Password(pw,Field('passwordSHA1').AsString);
  except
    result := false;
  end;
end;


class procedure TFRE_DB_USER.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var field_def   : IFRE_DB_FieldSchemeDefinition;
    input_group : IFRE_DB_InputGroupSchemeDefinition;
    params      : TFRE_DB_Object;
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.Strict(true);
  scheme.AddSchemeField('login',fdbft_String).SetupFieldDef(true);
  scheme.AddSchemeField('firstname',fdbft_String);
  scheme.AddSchemeField('lastname',fdbft_String);
  scheme.AddCalcSchemeField('displayname',fdbft_String,@_calcDisplayName);
  scheme.AddSchemeField('passwordMD5',fdbft_String).SetupFieldDef(true,false,'','',true,true);
  scheme.AddSchemeField('usergroupids',fdbft_ObjLink).SetupFieldDef(false,true);
  scheme.AddSchemeField('domainidlink',fdbft_ObjLink).SetupFieldDef(true,false);
  scheme.AddSchemeField('domainloginkey',fdbft_String).SetupFieldDef(true,false);

  field_def := scheme.AddSchemeField('picture',fdbft_Stream);
   params:=TFRE_DB_Object.Create;
   params.Field('width').AsInt16:=70;
   params.Field('height').AsInt16:=90;
   params.Field('absolute').AsBoolean:=true;
  field_def.SetupFieldDef(false,false,'','image',false,false,params);

  scheme.AddSchemeFieldSubscheme('desc','TFRE_DB_TEXT');
  Scheme.SetSysDisplayField(TFRE_DB_NameTypeArray.Create('login','firstname','lastname'),'%s - (%s %s)');

  input_group:=scheme.AddInputGroup('main').Setup('$TFRE_DB_USER_scheme_user_group');
  input_group.AddInput('login','$TFRE_DB_USER_scheme_login');
  input_group.AddDomainChooser('domainidlink',sr_STORE,TFRE_DB_USER,true,'$TFRE_DB_USER_scheme_domainidlink');
  input_group.AddInput('firstname','$TFRE_DB_USER_scheme_firstname');
  input_group.AddInput('lastname','$TFRE_DB_USER_scheme_lastname');
  input_group.AddInput('passwordMD5','$TFRE_DB_USER_scheme_passwordMD5');

  input_group:=scheme.AddInputGroup('main_edit').Setup('$TFRE_DB_USER_scheme_user_group');
  input_group.AddInput('login','$TFRE_DB_USER_scheme_login',true);
  input_group.AddInput('firstname','$TFRE_DB_USER_scheme_firstname');
  input_group.AddInput('lastname','$TFRE_DB_USER_scheme_lastname');
  input_group.AddInput('passwordMD5','$TFRE_DB_USER_scheme_passwordMD5');

  input_group:=scheme.AddInputGroup('descr').Setup('$TFRE_DB_USER_scheme_descr_group');
  input_group.UseInputGroup('TFRE_DB_TEXT','main','desc');
  input_group:=scheme.AddInputGroup('picture').Setup('$TFRE_DB_USER_scheme_picture_group');
  input_group.AddInput('picture','$TFRE_DB_USER_scheme_picture',false,false,'');
end;

class function TFRE_DB_USER.GetDomainLoginKey(const loginpart: TFRE_DB_String; const domain_id: TFRE_DB_GUID): TFRE_DB_String;
begin
  result := FREDB_G2H(domain_id)+'@'+lowercase(loginpart);
end;

function TFRE_DB_USER.DomainLoginKey: TFRE_DB_String;
begin
  result := field('domainloginkey').AsString;
end;

class procedure TFRE_DB_USER.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
 newVersionId:='1.0';
 if currentVersionId='' then begin
   currentVersionId := '1.0';

   conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_USER_scheme_user_group','User'));
   conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_USER_scheme_descr_group','User'));
   conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_USER_scheme_picture_group','Description'));
   conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_USER_scheme_login','Login name'));
   conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_USER_scheme_firstname','Firstname'));
   conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_USER_scheme_lastname','Lastname'));
   conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_USER_scheme_passwordMD5','Password'));
   conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_USER_scheme_picture',''));
   conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_USER_scheme_domain_group','Domain'));
   conn.StoreTranslateableText(GFRE_DBI.CreateText('$TFRE_DB_USER_scheme_domainidlink','Domain'));
 end;

end;

class function TFRE_DB_USER.WBC_NewUserOperation(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var dbo              : IFRE_DB_Object;
    data             : IFRE_DB_Object;
    res              : TFRE_DB_Errortype;
    dbc              : TFRE_DB_CONNECTION;
    loginf,pw,pwc,
    fn,ln            : String;
    ldesc,sdesc      : TFRE_DB_String;
    fld              : IFRE_DB_Field;
    image            : TFRE_DB_Stream;
    imagetype        : String;
    domlink          : TFRE_DB_GUID;

begin
 data    := input.Field('DATA').asobject;
 domlink := data.field('domainidlink').AsGUID;

 dbc     := input.GetReference as TFRE_DB_CONNECTION;

 loginf  := data.Field('login').AsString;
 pw      := data.Field('PASSWORDMD5').AsString;
 pwc     := data.Field('PASSWORDMD5_CONFIRM').AsString;
 fn      := data.Field('firstname').AsString;
 ln      := data.field('lastname').AsString;

 //dbc.sys.FetchDomainById(GFRE_BT.HexString_2_GUID(data.field('domainidlink').AsString),obj);

  if pw<>pwc then
   exit(TFRE_DB_MESSAGE_DESC.create.Describe('TRANSLATE: Error','TRANSLATE: Password confirm mismatch',fdbmt_error,nil));

  if data.FieldOnlyExisting('picture',fld) then
    begin
       image     := fld.AsStream;
       imagetype := data.Field('picture'+cFRE_DB_STKEY).AsString;
       fld.Clear(true);
     end
  else
    begin
      image:=nil;
      imagetype:='';
    end;

  if data.FieldOnlyExisting('desc',fld) then
    begin
      data := data.Field('desc').AsObject;
      if data.FieldOnlyExisting('txt',fld) then
        begin
          if fld.IsSpecialClearMarked then
            ldesc := ''
          else
            ldesc := fld.AsString;
        end;
      if data.FieldOnlyExisting('txt_s',fld) then
        if fld.IsSpecialClearMarked then
          sdesc := ''
        else
          sdesc := fld.AsString;
    end;

 res := dbc.sys.AddUser(loginf,domlink,pw,fn,ln,image,imagetype,false,ldesc,sdesc);
 if res=edb_OK then
   begin
     exit(TFRE_DB_CLOSE_DIALOG_DESC.create.Describe());
   end
 else
   exit(TFRE_DB_MESSAGE_DESC.create.Describe('TRANSLATE: ERROR','TRANSLATE: Creation failed '+CFRE_DB_Errortype[res],fdbmt_error,nil));
end;


function TFRE_DB_USER.WEB_SaveOperation(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var res              : TFRE_DB_Errortype;
    //dbo              : IFRE_DB_Object;
    data             : IFRE_DB_Object;
    loginf,pw,pwc,
    fn,ln            : String;
    dn               : TFRE_DB_NameType;
    obj              : IFRE_DB_DOMAIN;
    modify           : boolean;
    txt              : TFRE_DB_String;
    fld              : IFRE_DB_FIELD;

    l_User  : IFRE_DB_USER;
    l_UserO : TFRE_DB_User;

begin
  data    := input.Field('DATA').asobject;
  loginf  := data.Field('login').AsString;
  pw      := data.Field('PASSWORDMD5').AsString;
  pwc     := data.Field('PASSWORDMD5_CONFIRM').AsString;
  fn      := data.Field('firstname').AsString;
  ln      := data.field('lastname').AsString;

  modify := data.FieldExists('firstname') or
            data.FieldExists('lastname') or
            data.FieldExists('PASSWORDMD5') or
            data.FieldExists('desc') or
            data.FieldExists('picture');

  if modify then
    begin
      res := conn.sys.FetchUser(login,DomainID,l_User);
      l_UserO := l_User.Implementor as TFRE_DB_USER;
      if res<>edb_OK then
        exit(TFRE_DB_MESSAGE_DESC.create.Describe('TRANSLATE: Error','TRANSLATE: Fetch Failed',fdbmt_error,nil));
      if data.FieldOnlyExisting('firstname',fld) then
        begin
          if fld.IsSpecialClearMarked then
            l_UserO.Firstname:=''
          else
            l_UserO.Firstname:=fld.AsString;
        end;
      if data.FieldOnlyExisting('lastname',fld) then
        begin
          if fld.IsSpecialClearMarked then
            l_UserO.Lastname :=''
          else
            l_UserO.Lastname := fld.AsString;
        end;
     if data.FieldOnlyExisting('picture',fld) then
       begin
         if fld.FieldType=fdbft_Stream then
           begin
              l_UserO.SetImage(fld.AsStream,data.Field('picture'+cFRE_DB_STKEY).AsString,data.Field('picture'+cFRE_DB_ST_ETAG).AsString);
              fld.Clear(true);
           end
         else
           begin
             //raise EFRE_DB_Exception.Create(edb_ERROR,'the picture field must be a stream field, not [%s] val=[%s]',[fld.FieldTypeAsString,fld.AsString]);
           end;
       end;
      if data.FieldOnlyExisting('desc',fld) then
        begin
          data := data.Field('desc').AsObject;
          if data.FieldOnlyExisting('txt',fld) then
            begin
              if fld.IsSpecialClearMarked then
                l_UserO.Field('desc').AsDBText.ClearLong
              else
                l_UserO.Field('desc').AsDBText.Setlong(fld.AsString);
            end;
          if data.FieldOnlyExisting('txt_s',fld) then
            if fld.IsSpecialClearMarked then
              l_UserO.Field('desc').AsDBText.ClearShort
            else
              l_UserO.Field('desc').AsDBText.SetShort(fld.AsString);
        end;
      if data.FieldOnlyExisting('PASSWORDMD5',fld) then {FIXXME !!!! Password policies, check .... validator ...}
        l_UserO.SetPassword(fld.AsString);
      CheckDbResult((conn.sys.Implementor as TFRE_DB_SYSTEM_CONNECTION).Update(l_UserO),'TRANSLATE: UPDATE FAILED');
    end;
   exit(TFRE_DB_CLOSE_DIALOG_DESC.create.Describe());
end;


procedure InitMinimal(const nosys:boolean);
begin
 SetMultiByteConversionCodePage(CP_UTF8);
 GFRE_DB  := TFRE_DB.Create;
 GFRE_DBI := GFRE_DB;
 GFRE_DB.RegisterObjectClass(TFRE_DB_Object);
 GFRE_DB.RegisterObjectClass(TFRE_DB_TEXT);
 GFRE_DB.RegisterObjectClass(TFRE_DB_OBJECTLIST);
 GFRE_DB.RegisterObjectClass(TFRE_DB_NAMED_OBJECT);
 GFRE_DB.RegisterObjectClass(TFRE_DB_USER);   // Needed because of Calculated Field (GetRoles)
 GFRE_DB.RegisterPrimaryImplementor(TFRE_DB_NAMED_OBJECT,IFRE_DB_NAMED_OBJECT);
 GFRE_DB.RegisterPrimaryImplementor(TFRE_DB_OBJECT,IFRE_DB_OBJECT);
 GFRE_DB.RegisterObjectClass(TFRE_DB_COMMAND);

 GFRE_DBI.RegisterSysClientFieldValidator(GFRE_DBI.NewClientFieldValidator('image').Setup('(.+(\.(?i)(jpg|png|gif|bmp))$)',
                                                     GFRE_DBI.CreateText('$validator_image','Image File Validator'),
                                                     FREDB_GetGlobalTextKey('validator_image_help'),
                                                     '\d\.\/'));
 GFRE_DBI.RegisterSysClientFieldValidator(GFRE_DBI.NewClientFieldValidator('ip').Setup('^([1-9][0-9]{0,1}|1[013-9][0-9]|12[0-689]|2[01][0-9]|22[0-3])([.]([1-9]{0,1}[0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])){2}[.]([1-9][0-9]{0,1}|[1-9]{0,1}[0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-4])(\/([89]|[12][0-9]|3[0-2])|$)$',
                                                   GFRE_DBI.CreateText('$validator_ip','IP Validator'),
                                                   FREDB_GetGlobalTextKey('validator_ip_help'),
                                                   '\d\.\/'));
 GFRE_DBI.RegisterSysClientFieldValidator(GFRE_DBI.NewClientFieldValidator('mac').Setup('(^([0-9a-fA-F]{2}(:|$)){6}$|^[0-9a-fA-F]{12}$)',
                                                    GFRE_DBI.CreateText('$validator_mac','MAC Validator'),
                                                    FREDB_GetGlobalTextKey('validator_mac_help'),
                                                    '\da-fA-F:'));
 //if not nosys then GFRE_DB.RegisterSystemSchemes;
 if not nosys then GFRE_DB.Initialize_System_Objects;
end;

procedure GFRE_DB_Init_Check;
begin
  if not assigned(GFRE_DB) then raise EFRE_DB_Exception.Create(edb_ERROR,'NO DB INTERFACE ASSIGNED / STARTUP PROCEDURE WRONG');
  if not assigned(GFRE_DB_PS_LAYER) then raise EFRE_DB_Exception.Create(edb_ERROR,'NO DEFAULT PERSISTANCE LAYER ASSIGNED / STARTUP PROCEDURE WRONG');
end;

procedure Init4Server;
begin
 InitMinimal(true);
 GFRE_DB.RegisterObjectClass(TFRE_DB_COLLECTION);
 GFRE_DB.RegisterObjectClass(TFRE_DB_DERIVED_COLLECTION);
 GFRE_DB.RegisterObjectClass(TFRE_DB_DOMAIN);
 GFRE_DB.RegisterObjectClass(TFRE_DB_GROUP);
 GFRE_DB.RegisterObjectClass(TFRE_DB_ROLE);
 GFRE_DB.RegisterObjectClass(TFRE_DB_RESOURCE_CONTAINER);
 GFRE_DB.RegisterObjectClass(TFRE_DB_RESOURCE);
 GFRE_DB.RegisterObjectClass(TFRE_DB_Enum);
 GFRE_DB.RegisterObjectClass(TFRE_DB_ClientFieldValidator);
 GFRE_DB.RegisterPrimaryImplementor(TFRE_DB_COLLECTION,IFRE_DB_COLLECTION);
 GFRE_DB.RegisterPrimaryImplementor(TFRE_DB_DERIVED_COLLECTION,IFRE_DB_DERIVED_COLLECTION);
 GFRE_DB.RegisterPrimaryImplementor(TFRE_DB_SIMPLE_TRANSFORM,IFRE_DB_SIMPLE_TRANSFORM);
 GFRE_DB.RegisterObjectClassEx(TFRE_DB_APPLICATION);
 GFRE_DB.RegisterObjectClassEx(TFRE_DB_SERVER_FUNC_DESC);



 GFRE_DB_NIL_DESC             := TFRE_DB_NIL_DESC.create;
 GFRE_DB_SUPPRESS_SYNC_ANSWER := TFRE_DB_SUPPRESS_ANSWER_DESC.Create;

 GFRE_DB.Initialize_System_Objects;
end;



initialization


finalization
  GFRE_DB.Free;
  if assigned(GFRE_DB_NIL_DESC) then
    GFRE_DB_NIL_DESC.DestroySingleton;
  if assigned(GFRE_DB_SUPPRESS_SYNC_ANSWER) then
    GFRE_DB_SUPPRESS_SYNC_ANSWER.DestroySingleton;

end.

