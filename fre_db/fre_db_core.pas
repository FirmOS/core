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

// TODO

// Schemes in DB needed as feature ? or all system schemes
// Checkschemefield -> Subobjects, all Field Types
// Make Notifications more granular and based upon transaction change list


interface

uses Sysutils,Classes,strutils,fpjson,jsonparser,fos_sparelistgen,
     FRE_DB_INTERFACE,zstream,base64,math,
     FRE_SYSTEM,FOS_ARRAYGEN,FOS_INTERLOCKED,
     FOS_TOOL_INTERFACES,FOS_REDBLACKTREE_GEN,//FRE_EXT_REGEXPR,
     FRE_DB_SYSRIGHT_CONSTANTS,
     BaseUnix,
     FRE_DB_COMMON,
     FRE_DB_GRAPH;

//TIME has to be stored as UTC Time !!!!
const  c_REFLINK_BLOCK_SIZE=16;

type

  TFRE_DB_ObjectState        = (fdbos_BAD,fdbos_Creating,fdbos_StreamingCreating,fdbos_Dirty,fdbos_Clean,fdbos_StreamingWriting,fdbos_Destroying);
  TFRE_DB_MetadataType       = (fdbmt_Reflinks);
  TFRE_DB_NotifyObserverType = (fdbntf_INSERT,fdbntf_UPDATE,fdbntf_DELETE,fdbntf_START_UPDATING,fdbntf_ENDUPDATE_APPLY,fdbntf_COLLECTION_RELOAD);

  TFRE_DB_ObjectHdr = packed record
    Signature    : Array [1..6] of Char; //FREDBO
    Version      : Byte;
    EndianMarker : Byte; // 0 = LE / 1 =BE
  end;

const
  CFRE_DB_ObjectHdr  : TFRE_DB_ObjectHdr                       = ( Signature : 'FREDBO' ; Version : 2 ; EndianMarker : 0);

type
  TFRE_DB_Object                = class;
  TFRE_DB_NAMED_OBJECT          = class;

  TFRE_DB_TEXT                  = class;

  TFRE_DB_BASE_CONNECTION       = class;
  TFRE_DB_Connection            = class;

  TFRE_DB_ObjectArray          = Array of TFRE_DB_Object;
  PFRE_DB_ObjectArray           = ^TFRE_DB_ObjectArray;

  TFRE_DB_ObjLinkArray  = Array of TGuid;
  PFRE_DB_ObjLinkArray  = ^TFRE_DB_ObjLinkArray;

  IFRE_DB_PERSISTANCE_COLLECTION       = interface;
  IFRE_DB_PERSISTANCE_COLLECTION_ARRAY = array of IFRE_DB_PERSISTANCE_COLLECTION;


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
    16 : (obj  : PFRE_DB_ObjectArray);
    17 : (obl  : PFRE_DB_ObjLinkArray);
  end;
  PFRE_DB_FieldData = ^TFRE_DB_FieldData;

  { TFRE_DB_FIELD }
  TFRE_DB_FIELD=class(TFOS_BASE,IFRE_DB_Field)
  protected
    FFieldData         : TFRE_DB_FieldData;
    FFieldName         : PFRE_DB_NameType;
    Fobj               : TFRE_DB_Object;
    FIsCalculated      : Boolean;
    FIsUidField        : Boolean;
    FCurrentCalcMethod : IFRE_DB_InvokeInstanceMethod;
    function _DBConnectionBC : TFRE_DB_BASE_CONNECTION;
    function _DBConnection   : TFRE_DB_CONNECTION;
  private
    procedure Finalize;
    procedure _InAccessibleCheck  ; inline;

    function  CopyFieldToMem      (var mempointer:Pointer;const without_schemes:boolean):TFRE_DB_SIZE_TYPE;
    procedure CopyFieldFromMem    (var mempointer:Pointer;const conn:TFRE_DB_BASE_CONNECTION;const recreate_weak_schemes: boolean;const generate_new_uids:boolean); // ;fieldtype:TFRE_DB_FIELDTYPE,,valuecount);
    class procedure __ReadHeader  (var memory:pointer;out fieldname:TFRE_DB_NameType);

    function  _StreamingSize      : TFRE_DB_SIZE_TYPE;
    procedure _IllegalTypeError   (const ill_type:TFRE_DB_FIELDTYPE);
    procedure _ResultTypeUnset    (const ill_type:TFRE_DB_FIELDTYPE);
    procedure _StringToConvError  (const conv2_type:TFRE_DB_FIELDTYPE);
    procedure _GetHigh            (var hi:integer);
    procedure _StripObject        ;

    function  _CalculateObject    : IFRE_DB_Object;

    function  _ConvertToGUID      : TGuid;
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
    function  _ConvertToBool      : Boolean;
    function  _ConvertToDateTime  : TFRE_DB_Datetime64;

    procedure _CheckFieldType    (const expected:TFRE_DB_FIELDTYPE);
    procedure _CheckIndex        (const idx:integer;const typ:TFRE_DB_FIELDTYPE);
    function  _CheckStoreType    (const expected:TFRE_DB_FIELDTYPE):boolean;

    procedure _LocalToUTC        (var arr:TFRE_DB_DateTimeArray);
    procedure _NotAllowedOnUIDFieldCheck;

    function  GetAsGUID          : TGuid;
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
    function  GetAsStream        : TFRE_DB_Stream;
    function  GetAsObjectLink    : TGuid;

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
    procedure SetAsGUID          (const AValue: TGuid);
    procedure SetAsObject        (const AValue: TFRE_DB_Object);
    procedure SetAsStream        (const AValue: TFRE_DB_Stream);
    procedure SetAsString        (const AValue: TFRE_DB_String);
    procedure SetAsBoolean       (const AValue: Boolean);
    procedure SetAsObjectLink    (const AValue: TGUID);

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
    function  GetAsObjectLinkArrayObj : TFRE_DB_ObjectArray;

    function  GetAsGUIDList          (idx: Integer): TGUID;
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
    function  GetAsObjectLinkList    (idx: Integer): TGUID;

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
    procedure SetAsObjectLinkArrayObj(const AValue: TFRE_DB_ObjectArray);

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
    procedure SetAsGUIDList          (idx: Integer; const AValue: TGUID);
    procedure SetAsObjectList        (idx: Integer; const AValue: TFRE_DB_Object);
    procedure SetAsStreamList        (idx: Integer; const AValue: TFRE_DB_Stream);
    procedure SetAsStringList        (idx: Integer; const AValue: TFRE_DB_String);
    procedure SetAsBooleanList       (idx: Integer; const AValue: Boolean);
    procedure SetAsObjectLinkList    (idx: Integer; const AValue: TGUID);

    procedure IntfCast                      (const InterfaceSpec:ShortString ; out Intf) ; // Interpret as Object and then -> IntfCast throws an Exception if not succesful
    function  AsDBText                      :IFRE_DB_TEXT;

    procedure IFRE_DB_Field.CloneFromField = CloneFromFieldI;
  public
    constructor Create           (const obj:TFRE_DB_Object; const FieldType:TFRE_DB_FIELDTYPE);
    destructor  Destroy          ;override;
    function    FieldType         : TFRE_DB_FIELDTYPE;
    function    FieldTypeAsString : TFRE_DB_String;
    function    ValueCount        : Integer;
    function    IsUIDField        : boolean;
    function    IsObjectField     : boolean;

    procedure   CloneFromFieldFull(const Field:TFRE_DB_FIELD); // Array Clone
    procedure   CloneFromField    (const Field:TFRE_DB_FIELD); // Value 0 = Fieldclone
    procedure   CloneFromFieldI   (const Field:IFRE_DB_FIELD);

    property  AsGUID                        : TGuid read GetAsGUID write SetAsGUID;
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
    property  AsObjectLink                  : TGuid read GetAsObjectLink write SetAsObjectLink;

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
    property  AsObjectLinkArrayObjects      : TFRE_DB_ObjectArray   read GetAsObjectLinkArrayObj write SetAsObjectLinkArrayObj;

    property  AsGUIDItem        [idx:Integer] : TGuid read GetAsGUIDList write SetAsGUIDList;
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
    property  AsObjectLinkItem  [idx:Integer] : TGUID read GetAsObjectLinkList write SetAsObjectLinkList;

    procedure AddGuid                       (const value : TGuid);
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
    procedure AddObjectLink                 (const value : TGUID);

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

    procedure SetAsEmptyStringArray         ;
    function  IsEmptyArray                  : boolean;
    // Compare to Fields, Valuecount must be same, For Subobjects the only the UID's must be same!
    function  CompareToFieldShallow         (const cmp_fld : TFRE_DB_FIELD):boolean;

    procedure StripObject                   ;

    function  GetAsJSON                     (const without_uid:boolean=false;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil): TJSONData;
    procedure SetFromJSON                   (const field_type:TFRE_DB_FIELDTYPE;const json_object:TJSONArray;const stream_cb:TFRE_DB_StreamingCallback;const conn:TFRE_DB_BASE_CONNECTION=nil);
    procedure Stream2String                 (var raw_string:TFRE_DB_RawByteString);

    function  AsStringDump                  : TFRE_DB_String;
    function  FieldName                     : TFRE_DB_NameType;
    procedure Clear                         (const dont_free_streams_and_objects:boolean=false);
    procedure RemoveIndex                   (const idx:integer);
    class function OrderFieldCompare        (const ofieldname : TFRE_DB_String;const o1,o2 : TFRE_DB_Object):integer;

    function  GetAsObjectI                  : IFRE_DB_Object;
    procedure SetAsObjectI                  (const val:IFRE_DB_Object);
    function  GetAsObjectArrayI             : IFRE_DB_ObjectArray;
    procedure SetAsObjectArrayI             (const AValue: IFRE_DB_ObjectArray);

    function  GetAsObjectLinkArrayObjI      : IFRE_DB_ObjectArray;
    procedure SetAsObjectLinkArrayObjI      (const arr: IFRE_DB_ObjectArray);
    function  GetAsObjectListI              (idx: Integer): IFRE_DB_Object;
    procedure SetAsObjectListI              (idx: Integer; const AValue: IFRE_DB_Object);
    procedure AddObjectI                    (const obj : IFRE_DB_Object);

    function  ParentObject                  : TFRE_DB_Object; // The object the field belongs to

    function  IFRE_DB_Field.GetAsObject             = GetAsObjectI;
    procedure IFRE_DB_Field.SetAsObject             = SetAsObjectI;
    function  IFRE_DB_Field.GetAsObjectArray        = GetAsObjectArrayI;
    function  IFRE_DB_Field.SetAsObjectArray        = SetAsObjectArrayI;
    function  IFRE_DB_Field.GetAsObjectLinkArrayObj = GetAsObjectLinkArrayObjI;
    function  IFRE_DB_Field.SetAsObjectLinkArrayObj = SetAsObjectLinkArrayObjI;
    function  IFRE_DB_Field.GetAsObjectList         = GetAsObjectListI;
    function  IFRE_DB_Field.SetAsObjectList         = SetAsObjectListI;
    function  IFRE_DB_Field.AddObject               = AddObjectI;
    property  AsObjectI                     : IFRE_DB_Object read GetAsObjectI write SetAsObjectI;
  end;
  _TFRE_DB_FieldTree        = specialize TGFOS_RBTree<TFRE_DB_NameType,TFRE_DB_FIELD>;

  TFRE_DB_FieldSchemeDefinition   = class;
  TFRE_DB_FieldIterator           = procedure (const obj:TFRE_DB_Field) is nested;
  TFRE_DB_FieldIteratorBrk        = function  (const obj:TFRE_DB_Field):boolean is nested;
  TFRE_DB_SchemeFieldDef_Iterator = procedure (const obj:TFRE_DB_FieldSchemeDefinition) is nested;
  TFRE_DB_ObjectIteratorBrk       = procedure (const obj:TFRE_DB_Object; var halt:boolean) is nested;

  TFRE_DB_SchemeObject      = class;
  TFRE_DB_OBJECTCLASS       = class of TFRE_DB_Object;

  TFRE_DB_ChangeRecord=class
  end;

  OFRE_SL_TFRE_DB_Object  = specialize OFOS_SpareList<TFRE_DB_Object>;

  TFRE_DB_ObjCompareEventType = (cev_FieldDeleted,cev_FieldAdded,cev_FieldChanged);

  { TFRE_DB_Object }

  TFRE_DB_Object=class(TFRE_DB_Base,IFRE_DB_Object)
  private
   var
    FUID               : TGUID;
    FFieldStore        : _TFRE_DB_FieldTree;
    FManageInfo        : TFRE_DB_Base_Connection; // Object is "Managed" comes/from/belongs/to this connection
    FCacheSchemeObj    : TFRE_DB_Object;          // Cache ; TFRE_DB_SchemeObject; only link to ... (dont free)
    FSchemeName        : TFRE_DB_NameType;
    FParentDBO         : TFRE_DB_FIELD;
    fuidPath           : TFRE_DB_StringArray;
    fuidPathUA         : TFRE_DB_GUIDArray;
    FObjectProps       : TFRE_DB_Object_PropertySet; // Runtime Properties
    FInCollectionarr   : array of IFRE_DB_PERSISTANCE_COLLECTION;
    procedure      ForAll                              (const iter:TFRE_DB_FieldIterator);
    procedure      ForAllBrk                           (const iter:TFRE_DB_FieldIteratorBrk);
    function       _Field                              (name:TFRE_DB_String):TFRE_DB_FIELD;
    function       _FieldOnlyExisting                  (name:TFRE_DB_String):TFRE_DB_FIELD;
    procedure      _ParentCheck                        (const newdbo : TFRE_DB_Object);
    function       _ReadOnlyCheck                      : boolean;
    procedure      CheckMediatorSetup                  ;
    procedure      _InAccessibleCheck                  ; inline ;
  protected
    FDBO_State      : TFRE_DB_ObjectState;
    function        _ObjectsNeedsNoSubfieldSchemeCheck  : boolean;virtual;
    function        _ObjectIsCodeclassOnlyAndHasNoScheme: boolean;virtual;
    function        _DBConnectionBC                    (const fail_on_no_connection:boolean=true): TFRE_DB_BASE_CONNECTION;
    function        _DBConnection                      (const fail_on_no_connection:boolean=true): TFRE_DB_CONNECTION;
    procedure       SetScheme                          (const scheme_obj:TFRE_DB_SchemeObject);
    procedure       SchemeFieldAccessCheck             (const name:TFRE_DB_String);virtual;
    function        CalcFieldExists                    (const name:TFRE_DB_String;var calculated_field:TFRE_DB_FIELD):boolean;
    procedure       InternalSetup                      ; virtual;
    procedure       InternalFinalize                   ; virtual;
    procedure       CopyToMem                          (var mempointer:Pointer;const without_schemes:boolean=false);
    procedure       CopyFromMem                        (var mempointer:Pointer;const field_count:TFRE_DB_SIZE_TYPE;const conn:TFRE_DB_BASE_CONNECTION=nil;const recreate_weak_schemes: boolean=false;const generate_new_uids:boolean=false);
    procedure       CopyFromJSON                       (const JSON:TJSONArray;const field_count:TFRE_DB_SIZE_TYPE;const conn:TFRE_DB_BASE_CONNECTION=nil;const recreate_weak_schemes: boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil);
    function        CopyToJSON                         : TFRE_DB_String; // without streams // - only stream keys
    class function  CreateInternalStreaming            (const parent:TFRE_DB_FIELD;var mempointer:Pointer;const conn:TFRE_DB_BASE_CONNECTION=nil;const recreate_weak_schemes: boolean=false;const generate_new_uids:boolean=false):TFRE_DB_Object;
    class function  CreateInternalStreamingJSON        (const parent:TFRE_DB_FIELD;const JSON:TJSONArray;const conn:TFRE_DB_BASE_CONNECTION=nil;const recreate_weak_schemes: boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil):TFRE_DB_Object;
    function        _StreamingSize                     :TFRE_DB_SIZE_TYPE; // gets calculated before streaming
    procedure       BeforeSave                         ;virtual;
    procedure       AfterSave                          ;virtual;
    procedure       AfterLoad                          ;virtual;
    //procedure       SetObjCacheDirty                   ;virtual;
    function        Implementor                        : TObject;
    //function        Supports                           (const InterfaceSpec:ShortString ; out Intf) : boolean;override;
    //function        Supports                           (const InterfaceSpec:ShortString)            : boolean;override;
    //procedure       IntfCast                           (const InterfaceSpec:ShortString ; out Intf) ; // IntfCast throws an Exception if not succesful
    function        IFRE_DB_Object.ParentField         = ParentFieldI;
    function        IFRE_DB_Object.Parent              = ParentI;
    function        IFRE_DB_Object.FieldPath           = FieldPathI;
    function        IFRE_DB_Object.Field               = FieldI;
    function        IFRE_DB_Object.CloneToNewObject    = CloneToNewObjectI;
    function        IFRE_DB_Object.GetScheme           = GetSchemeI;
    function        IFRE_DB_Object.FieldOnlyExistingObj= FieldOnlyExistingObjI;
    procedure       IFRE_DB_Object.CopyField           = CopyFieldI;
    function        Invoke                             (const method:TFRE_DB_String;const input:IFRE_DB_Object):IFRE_DB_Object; virtual;
    procedure       Finalize                           ;
    procedure       RemoveAllRefLinks                  ;
  public
  type
     TFRE_DB_ObjCompareCallback  = procedure(const obj:TFRE_DB_Object ; const compare_event : TFRE_DB_ObjCompareEventType ; const new_fld,old_field:TFRE_DB_FIELD) is nested;
    procedure       __InternalCollectionAdd            (const coll : IFRE_DB_PERSISTANCE_COLLECTION);
    function        __InternalCollectionExists         (const coll : IFRE_DB_PERSISTANCE_COLLECTION):boolean;
    function        __InternalGetCollectionList        :IFRE_DB_PERSISTANCE_COLLECTION_ARRAY;
    procedure       __InternalGetFullObjectList        (var list: OFRE_SL_TFRE_DB_Object);
    procedure       __InternalCompareToObj             (const compare_obj : TFRE_DB_Object ; callback : TFRE_DB_ObjCompareCallback);
    procedure       __InternalClearManageInfo          ;
    function        InternalUniqueDebugKey             : String;
    procedure       Set_ReadOnly                       ;
    procedure       Set_Volatile                       ;
    procedure       Set_System                         ;
    procedure       Set_Store_Locked                   (const locked:boolean=true); // Obj is as original in Persistent/MemoryStore Do not read or write it!
    procedure       Assert_CheckStoreLocked            ;
    procedure       Free                               ;
    procedure       ForAllFields                       (const iter:TFRE_DB_FieldIterator);
    procedure       ForAllFieldsBreak                  (const iter:TFRE_DB_FieldIteratorBrk);
    procedure       ForAllFields                       (const iter:IFRE_DB_FieldIterator);
    procedure       ForAllFieldsBreak                  (const iter:IFRE_DB_FieldIteratorBrk);
    function        ForAllObjectsBreak                 (const iter:TFRE_DB_ObjectIteratorBrk ; const with_subobjects:boolean=true):boolean; // includes root object (self)
    function        GetScheme                          : TFRE_DB_SchemeObject;
    function        GetSchemeI                         : IFRE_DB_SchemeObject;
    function        UID                                : TGUID;
    function        UID_String                         : TFRE_DB_String;
    function        UIDP                               : PByte;
    function        GetAsJSON                          (const without_uid:boolean=false;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil): TJSONData;virtual;
    function        GetAsJSONString                    (const without_uid:boolean=false;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil):TFRE_DB_String;virtual;
    function        NeededSize                         : TFRE_DB_SIZE_TYPE;
    function        _ObjectRoot                        : TFRE_DB_Object; // = the last parent with no parent
    function        ObjectRoot                         : TFRE_DB_Object; // = the last parent with no parent
    function        IsObjectRoot                       : Boolean;
    function        Parent                             : TFRE_DB_Object;
    function        ParentI                            : IFRE_DB_Object;
    function        ParentField                        : TFRE_DB_FIELD;
    function        ParentFieldI                       : IFRE_DB_FIELD;
    constructor     Create                             ;//;virtual;
    constructor     CreateStreaming                    (const conn:TFRE_DB_BASE_CONNECTION=nil;const ExtensionObjectMediatorClass:TFRE_DB_OBJECTCLASSEX=nil);virtual;
    destructor      Destroy                            ;override;
    procedure       CopyToMemory                       (memory : Pointer;const without_schemes:boolean=false);
    class function  CreateFromMemory                   (memory : Pointer   ;const conn:TFRE_DB_BASE_CONNECTION=nil;const recreate_weak_schemes: boolean=false;const generate_new_uids:boolean=false):TFRE_DB_Object;
    class function  CreateFromString                   (const AValue:TFRE_DB_String;const conn:TFRE_DB_BASE_CONNECTION=nil;const recreate_weak_schemes: boolean=false;const generate_new_uids:boolean=false):TFRE_DB_Object;
    class function  CreateFromJSONString               (const AValue:TFRE_DB_String;const conn:TFRE_DB_BASE_CONNECTION=nil;const recreate_weak_schemes: boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil):TFRE_DB_Object; //
    function        AsString                           (const without_schemes:boolean=false):TFRE_DB_String;
    function        FieldI                             (const name:TFRE_DB_String):IFRE_DB_FIELD;
    function        Field                              (const name:TFRE_DB_String):TFRE_DB_FIELD;virtual;
    function        FieldOnlyExisting                  (const name:TFRE_DB_String;var fld:TFRE_DB_FIELD):boolean;
    function        FieldOnlyExistingObj               (const name:TFRE_DB_String):TFRE_DB_Object;
    function        FieldOnlyExistingObjI              (const name:TFRE_DB_String):IFRE_DB_Object;
    function        FieldPath                          (const name:TFRE_DB_String;const dont_raise_ex:boolean=false):TFRE_DB_FIELD;virtual;
    function        FieldPathI                         (const name:TFRE_DB_String;const dont_raise_ex:boolean=false):IFRE_DB_FIELD;virtual;
    function        FieldPathExists                    (const name: TFRE_DB_String): Boolean;
    function        FieldPathListFormat                (const field_list:TFRE_DB_StringArray;const formats : TFRE_DB_String;const empty_val: TFRE_DB_String) : TFRE_DB_String;
    function        FieldCount                         (const without_calcfields:boolean): SizeInt;
    function        DeleteField                        (const name:TFRE_DB_String):Boolean;
    procedure       ClearAllFields                     ;
    function        FieldExists                        (const name:TFRE_DB_String):boolean;
    procedure       StripOwnedObjects                  ;
    procedure       DumpToStrings                      (const strings:TStrings;indent:integer=0);
    function        DumpToString                       (indent:integer=0;const dump_length_max:Integer=0):TFRE_DB_String;
    function        GetFormattedDisplay                : TFRE_DB_String;
    function        FormattedDisplayAvailable          : boolean;
    function        SubFormattedDisplayAvailable       : boolean;virtual;
    function        GetSubFormattedDisplay             (indent:integer=4):TFRE_DB_String;virtual;
    function        SchemeClass                        : TFRE_DB_NameType;
    function        IsA                                (const schemename:TFRE_DB_NameType):Boolean;
    procedure       SaveToFile                         (const filename:TFRE_DB_String;const without_schemes:boolean=false);
    class function  CreateFromFile                     (const filename:TFRE_DB_String;const conn:TFRE_DB_BASE_CONNECTION=nil;const recreate_weak_schemes:boolean=false):TFRE_DB_Object;
    function        CloneToNewObject                   (const generate_new_uids:boolean=false): TFRE_DB_Object;
    function        CloneToNewObjectI                  (const generate_new_uids:boolean=false): IFRE_DB_Object;
    function        ReferencesObjects                  : Boolean;
    function        ReferencesObjectsFromData          : Boolean;
    function        ReferenceList                      : TFRE_DB_GUIDArray;
    function        ReferenceListFromData              : TFRE_DB_CountedGuidArray;
    function        ReferenceListFromDataNocount       : TFRE_DB_GuidArray;
    function        ReferencesFromData                 : TFRE_DB_ObjectReferences;
    function        ReferencesDetailed                 : TFRE_DB_String;
    function        IsReferenced                       : Boolean;
    function        IsSystem                           : Boolean;
    function        IsVolatile                         : Boolean;
    function        ReferencedByList                   : TFRE_DB_GUIDArray;
    function        ReferencedByList                   (const from_scheme: TFRE_DB_String): TFRE_DB_GUIDArray;
    function        ReferencedByList                   (const scheme: TFRE_DB_StringArray): TFRE_DB_GUIDArray;
    function        GetFieldListFilter                 (const field_type:TFRE_DB_FIELDTYPE):TFRE_DB_StringArray;
    function        GetUIDPath                         : TFRE_DB_StringArray;
    function        GetUIDPathUA                       : TFRE_DB_GUIDArray;
    function        GetDBConnection                    : IFRE_DB_CONNECTION;
    function        Mediator                           : TFRE_DB_ObjectEx; // assigned if the Object uses Exended Functionality implemented by a Mediator Object
    function        Properties                         : TFRE_DB_Object_PropertySet;
    procedure       CopyField                          (const obj:TFRE_DB_Object;const field_name:String);
    procedure       CopyFieldI                         (const obj:IFRE_DB_Object;const field_name:String);
    function        FetchChildObj                      (const childuid:TGuid):TFRE_DB_Object;
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

    property     CommandID     : UInt64              read GetCommandID    write SetCommandID;
    property     InvokeClass   : String              read GetInvokeClass  write SetInvokeClass;
    property     InvokeMethod  : String              read GetInvokeMethod write SetInvokeMethod;
    property     Data          : IFRE_DB_Object      read GetDataI        write SetDataI;
    property     UidPath       : TFRE_DB_GUIDArray   read GetUidPath      write SetUidPath;
    property     CommandType   : TFRE_DB_COMMANDTYPE read GetCType        write SetCType;
    property     ErrorText     : TFRE_DB_String      read GetEText        write SetEText;
    property     FatalClose    : Boolean             read GetFatalClose   write SetFatalClose;
    property     ChangeSessionKey : String              read GetChangeSessionKey   write SetChangeSessionKey; // Should be only set in Answer to Force the Client to update his SessionID
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
    function  IFRE_DB_NAMED_OBJECT.Field            = FieldI;
    function  IFRE_DB_NAMED_OBJECT.GetScheme        = GetSchemeI;
    function  IFRE_DB_NAMED_OBJECT.CloneToNewObject = CloneToNewObjectI;
    function  IFRE_DB_NAMED_OBJECT.FieldOnlyExistingObj = FieldOnlyExistingObjI;
    procedure IFRE_DB_NAMED_OBJECT.CopyField            = CopyFieldI;
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    property  ObjectName      : TFRE_DB_String       read GetName write SetName;
    property  Description     : TFRE_DB_TEXT read GetDesc write SetDesc;
  end;

  TFRE_DB_WORKFLOW_STEP= class(TFRE_DB_NAMED_OBJECT,IFRE_DB_WORKFLOWSTEP)
  private
    FStepname : TFRE_DB_FIELD;
    function   GetStepName: TFRE_DB_NameType;
    procedure  SetStepName(AValue: TFRE_DB_NameType);
    function     _ObjectsNeedsNoSubfieldSchemeCheck    : boolean; override;
    function     _ObjectIsCodeclassOnlyAndHasNoScheme  : boolean; override;
    function  IFRE_DB_WORKFLOWSTEP.GetDesc          = GetDescI;
    function  IFRE_DB_WORKFLOWSTEP.SetDesc          = SetDescI;
  public
    procedure  SetStepMethod (const SchemeName,WFM_MethodName:TFRE_DB_String);
    property   StepName : TFRE_DB_NameType read GetStepName write SetStepName;
  end;

  TFRE_DB_WORKFLOW = class(TFRE_DB_NAMED_OBJECT,IFRE_DB_WORKFLOW)
  private
    FWorkflowData    : TFRE_DB_FIELD;
    FWorkflowProg    : TFRE_DB_FIELD;
    FIsInstance      : TFRE_DB_FIELD;
    FInstanceNumber  : TFRE_DB_FIELD;
    function      GetCurrentStep: TFRE_DB_NameType;
    function      GetEndTime: TFRE_DB_DateTime64;
    function      GetNextScheduledInvocation: TFRE_DB_DateTime64;
    function      GetRecurring: Boolean;
    function      GetStartTime: TFRE_DB_DateTime64;
    function      GetUserLogin: TFRE_DB_NameType;
    procedure     SetRecurring(AValue: Boolean);
    procedure     SetStartTime(AValue: TFRE_DB_DateTime64);
    function     _ObjectsNeedsNoSubfieldSchemeCheck    : boolean; override;
    function     _ObjectIsCodeclassOnlyAndHasNoScheme  : boolean; override;
    procedure    InternalSetup     ; override;
    function     IFRE_DB_WORKFLOW.GetDesc          = GetDescI;
    function     IFRE_DB_WORKFLOW.SetDesc          = SetDescI;
  public
    procedure    ClearWorkFlow    ;

    //procedure    DefineWorkFlowProperties (const Recurring:Boolean;const StartTime,Endtime: TFRE_DB_DateTime64;const Interval_Seconds : );
    procedure    AddWorkFlowStep  (const STEP_NAME:TFRE_DB_NameType;const STEP   : TFRE_DB_WORKFLOW_STEP);
    procedure    AddSubWorkAsStep (const STEP_NAME:TFRE_DB_NameType;const WFNAME : TFRE_DB_NameType);
    procedure    DumpWorkFlow     ;

    //Instance Properties
    property     UserLogin               : TFRE_DB_NameType read GetUserLogin;
    property     Recurring               : Boolean read GetRecurring write SetRecurring;
    property     StartTime               : TFRE_DB_DateTime64 read GetStartTime;
    property     EndTime                 : TFRE_DB_DateTime64 read GetEndTime;
    property     NextScheduledInvocation : TFRE_DB_DateTime64 read GetNextScheduledInvocation;
    property     CurrentStep             : TFRE_DB_NameType read GetCurrentStep;
    function     WorkFlowData            : IFRE_DB_Object;
    procedure    StopWorkFlow            (const reason : TFRE_DB_String);
    function     WorkFlowState           : TFRE_DB_WORKFLOW_STATE;
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
    procedure addEntry    (const value:TFRE_DB_String;const caption: TFRE_DB_TEXT);
    procedure addEntryI   (const value:TFRE_DB_String;const caption: IFRE_DB_TEXT);
    function  getEntries  :TFRE_DB_ObjectArray;
    function  getEntriesI :IFRE_DB_ObjectArray;
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
    function  Setup            (const regExp:TFRE_DB_String; const infoText: IFRE_DB_TEXT; const helpText: IFRE_DB_TEXT=nil; const allowedChars:TFRE_DB_String=''): IFRE_DB_ClientFieldValidator;
    function  getRegExp        :TFRE_DB_String;
    function  getInfoText      :TFRE_DB_TEXT;
    function  getInfoTextI     :IFRE_DB_TEXT;
    function  getHelpText      :TFRE_DB_TEXT;
    function  getHelpTextI     :IFRE_DB_TEXT;
    function  getAllowedChars  :TFRE_DB_String;
    //procedure setConfigParamsI (const params:IFRE_DB_Object);
    //procedure setConfigParams  (const params:TFRE_DB_Object);
    //function  getConfigParamsI :IFRE_DB_Object;
    //function  getConfigParams  :TFRE_DB_Object;
    function  CheckField       (const field_to_check:TFRE_DB_FIELD;const raise_exception:boolean):boolean; virtual;
  end;

  { TFRE_DB_FieldSchemeDefinition }

  TFRE_DB_FieldSchemeDefinition=class(TFRE_DB_Object,IFRE_DB_FieldSchemeDefinition)
  private
    FNameField    : TFRE_DB_FIELD;
    FTypeField    : TFRE_DB_FIELD;
    FSubscheme    : TFRE_DB_FIELD;
    FSubSchemeObj : TFRE_DB_SchemeObject; // Cache;
    Fenum         : TFRE_DB_FIELD;
    Fvalidator    : TFRE_DB_FIELD;
    FmultiValues  : TFRE_DB_FIELD;
    Frequired     : TFRE_DB_FIELD;
    FisPass       : TFRE_DB_FIELD;
    FaddConfirm   : TFRE_DB_FIELD;
    FdepField     : TFRE_DB_FIELD;
    FCalcMethod   : TFRE_DB_FIELD;
    FvalidatorParams: TFRE_DB_FIELD;
    FMyFakeCalcFld: TFRE_DB_FIELD;
    FMyFakeCalcFld_Name : TFRE_DB_NameType;
    FMyFakeCalcFld_Data : Pointer;
    function   getAddConfirm       : Boolean;
    function   getEnum             : TFRE_DB_Enum;
    function   getEnumI            : IFRE_DB_Enum;
    function   GetFieldName        : TFRE_DB_String;
    function   GetFieldType        : TFRE_DB_FIELDTYPE;
    function   getIsPass           : Boolean;
    function   GetSubSchemeName    : TFRE_DB_String;
    function   getMultiValues      : Boolean;
    function   getRequired         : Boolean;
    function   getValidator        : TFRE_DB_ClientFieldValidator;
    function   getValidatorI       : IFRE_DB_ClientFieldValidator;
    procedure  setAddConfirm       (AValue: Boolean);
    procedure  setisPass           (AValue: Boolean);
    procedure  setMultiValues      (AValue: Boolean);
    procedure  setRequired         (AValue: Boolean);
    procedure  setEnum             (AValue: TFRE_DB_Enum);
    procedure  setValidator        (AValue: TFRE_DB_ClientFieldValidator);
    procedure  setEnumI            (AValue: IFRE_DB_Enum);
    procedure  setValidatorI       (AValue: IFRE_DB_ClientFieldValidator);
    function   getParentScheme     : TFRE_DB_SchemeObject;
  protected
    function   _ObjectIsCodeclassOnlyAndHasNoScheme: boolean;override;
    procedure  InternalSetup     ; override;
    function   _CalculateFieldRes (const for_dbo:TFRE_DB_Object):TFRE_DB_Object;
    function   IFRE_DB_FieldSchemeDefinition.getEnum = getEnumI;
    function   IFRE_DB_FieldSchemeDefinition.setEnum = setEnumI;
    function   IFRE_DB_FieldSchemeDefinition.getValidator  = getValidatorI;
    function   IFRE_DB_FieldSchemeDefinition.setValidator  = setValidatorI;
    function   IFRE_DB_FieldSchemeDefinition.getDepFields  = getDepFieldsI;
    function   IFRE_DB_FieldSchemeDefinition.SetupFieldDef = SetupFieldDefI;
    function   IFRE_DB_FieldSchemeDefinition.GetSubScheme  = GetSubSchemeI;
    function   IFRE_DB_FieldSchemeDefinition.ValidateField = ValidateFieldI;

  public
    function   Field             (const name: TFRE_DB_String)     : TFRE_DB_FIELD; override;
    function   SetupFieldDef     (const is_required:boolean;const is_multivalue:boolean=false;const enum_key:TFRE_DB_String='';const validator_key:TFRE_DB_String='';const is_pass:Boolean=false;const add_confirm:Boolean=false ; const validator_params : TFRE_DB_Object=nil):TFRE_DB_FieldSchemeDefinition;
    function   SetupFieldDefI    (const is_required:boolean;const is_multivalue:boolean=false;const enum_key:TFRE_DB_String='';const validator_key:TFRE_DB_String='';const is_pass:Boolean=false; const add_confirm:Boolean=false ; const validator_params : IFRE_DB_Object=nil):IFRE_DB_FieldSchemeDefinition;
    procedure  SetCalcMethod     (const calc_methodname:TFRE_DB_String);
    function   CalcField         (const calc_method:IFRE_DB_InvokeInstanceMethod):TFRE_DB_FIELD;
    function   getDepFields      : TFRE_DB_ObjectArray;
    function   getDepFieldsI     : IFRE_DB_ObjectArray;
    procedure  addDepField       (const fieldName: TFRE_DB_String;const disablesField: Boolean=true);
    property   FieldName         :TFRE_DB_String            read GetFieldName;
    property   FieldType         :TFRE_DB_FIELDTYPE read GetFieldType;
    property   SubschemeName     :TFRE_DB_String            read GetSubSchemeName;
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

  TFRE_DB_InputGroupSchemeDefinition=class(TFRE_DB_Object,IFRE_DB_InputGroupSchemeDefinition)
  protected
    function  GetCaptionKey      : TFRE_DB_String;
    function  GetIGFields        : IFRE_DB_ObjectArray;
    function  GetInputGroupID    : TFRE_DB_String;
    procedure SetCaptionKey      (AValue: TFRE_DB_String);
    procedure SetIGFields        (AValue: IFRE_DB_ObjectArray);
    procedure SetInputGroupID    (AValue: TFRE_DB_String);

    function IFRE_DB_InputGroupSchemeDefinition.Setup           = SetupI;
    function IFRE_DB_InputGroupSchemeDefinition.GetSchemeFromDB = GetSchemeFromDBI;
    function IFRE_DB_InputGroupSchemeDefinition.GetScheme       = GetSchemeI;
    function IFRE_DB_InputGroupSchemeDefinition.GetParentScheme = GetParentSchemeI;
    //function IFRE_DB_InputGroupSchemeDefinition.AddInput        = AddInputI;
    //procedure AddInputI           (const schemefield: TFRE_DB_String; const caption: IFRE_DB_TEXT; const disabled: Boolean=false;const hidden:Boolean=false; const dataCollection: TFRE_DB_String='');
    function  GetSchemeFromDBI    (const schemename:TFRE_DB_String; var scheme: IFRE_DB_SchemeObject): Boolean;
    function  SetupI              (const caption: TFRE_DB_String):IFRE_DB_InputGroupSchemeDefinition;
    function  GetParentSchemeI    : IFRE_DB_SchemeObject;
  public
    function  Setup              (const cap_key: TFRE_DB_String):TFRE_DB_InputGroupSchemeDefinition;
    function  GetParentScheme    : TFRE_DB_SchemeObject;
    procedure AddInput           (const schemefield: TFRE_DB_String; const cap_trans_key: TFRE_DB_String; const disabled: Boolean=false;const hidden:Boolean=false; const dataCollection: TFRE_DB_String='');
    procedure UseInputGroup      (const scheme,group: TFRE_DB_String; const addPrefix: TFRE_DB_String='');
    procedure AddInputSubGroup   (const scheme,group: TFRE_DB_String; const addPrefix: TFRE_DB_String='';const collapsible:Boolean=false;const collapsed:Boolean=false);
    function  GetSchemeFromDB    (const schemename:TFRE_DB_String; var scheme: TFRE_DB_SchemeObject): Boolean;
  end;

  { TFRE_DB_SchemeObject }

  TFRE_DB_SchemeObject=class(TFRE_DB_Object,IFRE_DB_SCHEMEOBJECT)
  private
    FSchemeClass      : TFRE_DB_FIELD;
    FHasHardcodeClass : TFRE_DB_FIELD;
    FFieldDefs        : TFRE_DB_Object;
    FUniqueKeys       : TFRE_DB_Object;
    FMethodDefs       : TFRE_DB_Object;
    FInputGroups      : TFRE_DB_Object;
    FStrict           : TFRE_DB_FIELD;         // Only defined Fields Allowed
    FParentScheme     : TFRE_DB_FIELD;
    FExplanation      : TFRE_DB_FIELD;
    FDisplayField     : TFRE_DB_Object;       // Object // Names Array, Format <%>
    FParentSchemeObj  : TFRE_DB_SchemeObject; // CacheCopy;
    FIMI_Methods      : TFRE_DB_StringArray;
    FSchemeType       : TFRE_DB_Field;//  TFRE_DB_SchemeType;
    FHC_MethodsBuild  : Boolean;
    FHardCodeClassTyp : TClass;
    FSealed           : Boolean;
    function       _ObjectsNeedsNoSubfieldSchemeCheck:boolean;override;
    function       _ObjectIsCodeclassOnlyAndHasNoScheme: boolean;override;
    procedure      _InternalSetParentScheme(const parentscheme:TFRE_DB_Schemeobject);
    procedure      _SelfCheckValid;
    procedure      _BuildHardcodeMethods;
    procedure      _Seal;
    procedure      _CheckChangingValid  ;
    procedure      _FieldAccessCheck(const name: TFRE_DB_String;const schemelist:string);
  protected
    procedure InternalSetup             ; override;
    procedure FieldAccessCheck          (const name:TFRE_DB_String);
    function  CalcFieldExists           (const base_obj:TFRE_DB_Object ; const name: TFRE_DB_String; var calculated_field: TFRE_DB_FIELD): boolean;
    procedure AfterLoad; override       ;
    procedure ForAllCalculatedFields    (const base_obj:TFRE_DB_Object;const iter:TFRE_DB_FieldIterator);
    procedure ForAllCalculatedFieldsBrk (const base_obj: TFRE_DB_Object;const iter:TFRE_DB_FieldIteratorBrk);

    function  ConstructNewInstance      (const fail_on_no_cc:boolean=true):TFRE_DB_Object;
    procedure SetupMediator             (const dbo:TFRE_DB_Object);
    function  HasHardCodeClass          : Boolean;

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
    function  IFRE_DB_SCHEMEOBJECT.GetSchemeFromDB           = GetSchemeFromDBI;
    function  IFRE_DB_SCHEMEOBJECT.GetEnumFromDB             = GetEnumFromDBI;
    function  IFRE_DB_SCHEMEOBJECT.GetValidatorFromDB        = GetValidatorFromDBI;
    function  IFRE_DB_SCHEMEOBJECT.ValidateObject            = ValidateObjectI;
    function  IFRE_DB_SCHEMEOBJECT.getSchemeFields           = getSchemeFieldsI;
    function  IFRE_DB_SCHEMEOBJECT.InvokeMethod              = InvokeMethodI;
    function  IFRE_DB_SCHEMEOBJECT.GetDBConnection           = GetDBConnectionI;
    function  IFRE_DB_SCHEMEOBJECT.UpdateSchemeField         = UpdateSchemeFieldI;
    function  IFRE_DB_SCHEMEOBJECT.ForAllFieldSchemeDefinitions = ForAllFieldSchemeDefinitionsI;
  public
    procedure ForAllFieldSchemeDefinitionsI (const iterator:IFRE_DB_SchemeFieldDef_Iterator);
    procedure ForAllFieldSchemeDefinitions (const iterator:TFRE_DB_SchemeFieldDef_Iterator);

    function  Field                     (const name: TFRE_DB_String): TFRE_DB_FIELD; override;
    //class function  InvokeMethod        (const instance:TFRE_DB_Object;const meth_name:TFRE_DB_String;const in_params:TFRE_DB_Object) : TFRE_DB_Object;
    function  InvokeMethod_UID_Session  (const instance:TFRE_DB_GUIDArray;const class_name,meth_name:TFRE_DB_String;const in_params:TFRE_DB_Object;const connection:TFRE_DB_CONNECTION; const session: TFRE_DB_UserSession) : IFRE_DB_Object;
    function  InvokeMethod_UIDI         (const obj_uid : TGUID;const obj_methodname:TFRE_DB_String;const input:IFRE_DB_Object;const connection:IFRE_DB_CONNECTION):IFRE_DB_Object;

    function  GetAll_IMI_Methods        :TFRE_DB_StringArray;
    function  MethodExists              (const name:TFRE_DB_String):boolean;
    function  AddSchemeField            (const newfieldname:TFRE_DB_String ; const newfieldtype:TFRE_DB_FIELDTYPE):TFRE_DB_FieldSchemeDefinition;
    function  AddSchemeFieldI           (const newfieldname:TFRE_DB_String ; const newfieldtype:TFRE_DB_FIELDTYPE):IFRE_DB_FieldSchemeDefinition;
    procedure RemoveSchemeField         (const fieldname:TFRE_DB_String);
    procedure AddCalculatedField        (const newfieldname,calc_method_name:TFRE_DB_String;const calculation_type:TFRE_DB_CalcFieldTime);
    function  AddSchemeFieldSubscheme   (const newfieldname:TFRE_DB_String ; const sub_scheme:TFRE_DB_String):TFRE_DB_FieldSchemeDefinition;
    function  AddSchemeFieldSubschemeI  (const newfieldname:TFRE_DB_String ; const sub_scheme:TFRE_DB_String):IFRE_DB_FieldSchemeDefinition;
    function  GetSchemeField            (const fieldname   :TFRE_DB_String ; var fieldschemedef:TFRE_DB_FieldSchemeDefinition): boolean;
    function  GetSchemeFieldI           (const fieldname   :TFRE_DB_String ; var fieldschemedef:IFRE_DB_FieldSchemeDefinition): boolean;
    function  GetSchemeField            (const fieldname   :TFRE_DB_String): TFRE_DB_FieldSchemeDefinition;
    function  GetSchemeFieldI           (const fieldname   :TFRE_DB_String): IFRE_DB_FieldSchemeDefinition;
    function  UpdateSchemeField         (const oldField:TFRE_DB_FieldSchemeDefinition; const newfieldname:TFRE_DB_String ; const newfieldtype:TFRE_DB_FIELDTYPE):TFRE_DB_FieldSchemeDefinition;
    function  UpdateSchemeFieldI        (const oldField:IFRE_DB_FieldSchemeDefinition; const newfieldname:TFRE_DB_String ; const newfieldtype:TFRE_DB_FIELDTYPE):IFRE_DB_FieldSchemeDefinition;
    function  IsA                       (const schemename :TFRE_DB_String):Boolean;
    function  AddUniqueKey              (const KeyName     :TFRE_DB_String ; const FieldNames:TFRE_DB_StringArray):TFRE_DB_Errortype;
    procedure SetSimpleSysDisplayField  (const field_name  :TFRE_DB_String);
    procedure SetSysDisplayField        (const field_names :TFRE_DB_StringArray;const format:TFRE_DB_String);
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
    procedure RemoveParentScheme        ;
    function  GetExplanation            :TFRE_DB_String;
    procedure SetExplanation            (AValue: TFRE_DB_String);
    function  GetSchemeType             : TFRE_DB_SchemeType;
    procedure SetObjectFieldsWithScheme (const Raw_Object: TFRE_DB_OBject; const Update_Object: TFRE_DB_Object;const new_object:boolean;const DBConnection:TFRE_DB_CONNECTION;const schemeType: TFRE_DB_String='');
    procedure SetObjectFieldsWithSchemeI(const Raw_Object: IFRE_DB_OBject; const Update_Object: IFRE_DB_Object;const new_object:boolean;const DBConnection:IFRE_DB_CONNECTION;const schemeType: TFRE_DB_String='');
    function  AddInputGroup             (const id: TFRE_DB_String): TFRE_DB_InputGroupSchemeDefinition;
    function  AddInputGroupI            (const id: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;
    function  GetInputGroup             (const name: TFRE_DB_String): TFRE_DB_InputGroupSchemeDefinition;
    function  GetInputGroupI            (const name: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;
    function  GetSchemeFromDB           (const schemename:TFRE_DB_String; var scheme: TFRE_DB_SchemeObject): Boolean;
    function  GetEnumFromDB             (const enumname:TFRE_DB_String; var enum: TFRE_DB_Enum):Boolean;
    function  GetValidatorFromDB        (const validatorname:TFRE_DB_String; var validator: TFRE_DB_ClientFieldValidator):Boolean;
    function  ValidateObject            (const dbo : TFRE_DB_Object;const raise_errors:boolean=true):boolean;
    function  ValidateObjectI           (const dbo : IFRE_DB_Object;const raise_errors:boolean=true):boolean;
    function  getSchemeFields           :TFRE_DB_Object;
    function  getSchemeFieldsI          :IFRE_DB_Object;
  end;


  { TFRE_DB_Object_ManageInfo }
  TFRE_DB_COLLECTION = class;

  TFRE_DB_CollectionAdd_CBN = procedure (const collection_name:TFRE_DB_NameType;const pers_collection:IFRE_DB_PERSISTANCE_COLLECTION) is nested;
  TFRE_DB_MasterCollAdd_CBN = procedure (const guid:TGuid) is nested;
  TFRE_DB_AddMetaData_CB    = procedure (const meta_type:TFRE_DB_MetadataType;const obj:TFRE_DB_Object) is nested;

  TFRE_DB_Obj_Iterator                  = procedure (const obj:TFRE_DB_Object) is nested;
  TFRE_DB_Scheme_Iterator               = procedure (const obj:TFRE_DB_SchemeObject) is nested;
  TFRE_DB_Enum_Iterator                 = procedure (const obj:TFRE_DB_Enum) is nested;
  TFRE_DB_ClientFieldValidator_Iterator = procedure (const obj: TFRE_DB_ClientFieldValidator) is nested;
  TFRE_DB_Apps_Iterator                 = procedure (const obj: TFRE_DB_APPLICATION) is nested;
  TFRE_DB_Guid_Iterator                 = procedure (const obj:TGUID) is nested;
  TFRE_DB_Obj_IteratorBreak             = function  (const obj:TFRE_DB_Object):Boolean is nested;



  IFRE_DB_PERSISTANCE_COLLECTION_4_PERISTANCE_LAYER=interface
    function      StoreInThisColl     (const new_obj         : TFRE_DB_Object ; const raise_ex : boolean ; const checkphase : boolean):TFRE_DB_Errortype;
    function      UpdateInThisColl    (const new_obj,old_obj : TFRE_DB_Object ; const raise_ex : boolean ; const checkphase : boolean):TFRE_DB_Errortype;
    function      DeleteFromThisColl  (const del_obj : TFRE_DB_Object ; const raise_ex : boolean ; const checkphase : boolean):TFRE_DB_Errortype;
    procedure     StreamToThis        (const stream          : TStream);
    procedure     LoadFromThis        (const stream          : TStream);
    function      FetchIntFromColl    (const uid:TGuid ; var obj : TFRE_DB_Object):boolean;
  end;

  // Abstraction of a Collection in the Persistance Layer
  // Manages Object Storage
  // -
  // Every Object has to be in one collection to be storeable
  // Objects can be in more than one Collection
  // Objects stored in temporary collections do not feature referential integrity management
  // Subobjects can be stored in collections too
  //
  // Every object store originates in the collection, but also checks for other collections
  //
  // Every Persistance Collection manages indexes for data

  { IFRE_DB_PERSISTANCE_COLLECTION }

  IFRE_DB_PERSISTANCE_COLLECTION=interface
    function  IsVolatile         : boolean;
    procedure InternalUnprepare  ;
    function  CollectionName     (const unique:boolean=true):TFRE_DB_NameType;
    function  GetPersLayerIntf   : IFRE_DB_PERSISTANCE_COLLECTION_4_PERISTANCE_LAYER;
    function  Count              : int64;
    function  Exists             (const ouid: TGUID): boolean;
    //function  AddCheck           (const new_guid :TGuid ; const man_inf :TFRE_DB_Object) : boolean;
    procedure Clear              ;
    procedure ForAllItems        (const iter : TFRE_DB_Obj_Iterator); // must allow modification of collection items
    function  ForAllitemsBreak   (const func: TFRE_DB_Obj_IteratorBreak):boolean;

    //function  Update             (const dbo:TFRE_DB_Object):TFRE_DB_Errortype;
    function        Store        (var   new_obj : TFRE_DB_Object ; const raise_ex : boolean=true ; var ncolls : TFRE_DB_StringArray=nil):TFRE_DB_Errortype;
    function        Delete       (const ouid    : TGUID          ; const raise_ex : boolean=true ; var ncolls : TFRE_DB_StringArray=nil):TFRE_DB_Errortype;

    function        Fetch  (const uid:TGUID ; var obj : TFRE_DB_Object) : boolean;

    function        LinearScan                   (const fieldname: TFRE_DB_NameType;  const field_expr: TFRE_DB_FIELD_EXPRESSION): TFRE_DB_Object;
    function        First                        : TFRE_DB_Object;
    function        Last                         : TFRE_DB_Object;
    function        GetItem                      (const num:uint64):TFRE_DB_Object;
    function        DefineIndexOnField           (const FieldName: TFRE_DB_NameType  ; const FieldType : TFRE_DB_FIELDTYPE; const unique: boolean; const ignore_content_case: boolean; const index_name: TFRE_DB_NameType): TFRE_DB_Errortype;
    function        CheckFieldChangeAgainstIndex (const oldfield,newfield : TFRE_DB_FIELD ; const change_type : TFRE_DB_ObjCompareEventType ; const check : boolean ; const raise_ex: boolean) :TFRE_DB_Errortype;

    // Fetches Snapshot copies of the objects, you need to finalize them
    function        GetIndexedObj      (const query_value : TFRE_DB_String ; out   obj:TFRE_DB_Object;const index_name:TFRE_DB_NameType='def'):boolean; // for the string fieldtype
    function        GetIndexedObj      (const query_value : TFRE_DB_String ; out   obj       : TFRE_DB_ObjectArray ; const index_name : TFRE_DB_NameType='def' ; const check_is_unique : boolean=false):boolean;
    function        GetIndexedUID      (const query_value : TFRE_DB_String ; out obj_uid     : TGUID               ; const index_name : TFRE_DB_NameType='def'): boolean;
    function        GetIndexedUID      (const query_value : TFRE_DB_String ; out obj_uid     : TFRE_DB_GUIDArray   ; const index_name : TFRE_DB_NameType='def' ; const check_is_unique : boolean=false):boolean; overload ;
  end;

  { IFRE_DB_PERSISTANCE_LAYER }

  IFRE_DB_PERSISTANCE_LAYER=interface
    procedure DEBUG_DisconnectLayer (const db:TFRE_DB_String);
    function  ExistCollection    (const coll_name : TFRE_DB_NameType) : Boolean;
    function  GetCollection      (const coll_name : TFRE_DB_NameType ; out Collection: IFRE_DB_PERSISTANCE_COLLECTION) : Boolean;
    function  NewCollection      (const coll_name : TFRE_DB_NameType ; out Collection: IFRE_DB_PERSISTANCE_COLLECTION; const volatile_in_memory: boolean; const global_system_namespace: boolean): TFRE_DB_Errortype;
    function  DeleteCollection   (const coll_name : TFRE_DB_NameType ; const global_system_namespace : boolean) : TFRE_DB_Errortype;

    //function  StoreObject        (const obj:TFRE_DB_Object)                  : TFRE_DB_Errortype;
    //function  StoreReflinks      (const obj:TFRE_DB_Object)                  : TFRE_DB_Errortype;
    //function  StoreCollection    (const coll:TFRE_DB_COLLECTION)             : TFRE_DB_Errortype;
    //function  FlushObjects       : TFRE_DB_Errortype;
    //function  FlushCollections   : TFRE_DB_Errortype;
    //function  RetrieveObject     (const UID:TGuid ; var  obj:TFRE_DB_Object;const manage_info:TFRE_DB_Object_ManageInfo) :TFRE_DB_Errortype;
    //function  RetrieveCollection (const collname:TFRE_DB_NameType; var  coll:TFRE_DB_COLLECTION;const manage_info:TFRE_DB_Collection_ManageInfo) :TFRE_DB_Errortype;
    //function  DeleteObject       (const UID:TGuid)                           : TFRE_DB_Errortype;

    function  Connect              (const db_name:TFRE_DB_String ; out database_layer : IFRE_DB_PERSISTANCE_LAYER ; const drop_wal : boolean=false) : TFRE_DB_Errortype;
    function  DatabaseList         : IFOS_STRINGS;
    function  DatabaseExists       (const dbname:TFRE_DB_String):Boolean;
    function  CreateDatabase       (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
    function  DeleteDatabase       (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
    procedure Finalize             ;

    function  GetReferenceCount    (const obj_uid: TGuid; const from: boolean): NativeInt;
    function  GetReferences        (const obj_uid:TGuid ; const from: boolean): TFRE_DB_ObjectReferences;

    //procedure ForAllObjects        (const iterator:TFRE_DB_Obj_Iterator);
    function  StartTransaction     (const typ:TFRE_DB_TRANSACTION_TYPE ; const ID:TFRE_DB_NameType ; const raise_ex : boolean=true) : TFRE_DB_Errortype;
    function  ObjectExists         (const obj_uid : TGUID) : boolean;
    function  DeleteObject         (const obj_uid : TGUID  ;  const raise_ex : boolean ; const collection_name: TFRE_DB_NameType = '' ; var ncolls: TFRE_DB_StringArray = nil):TFRE_DB_Errortype;
    function  Fetch                (const ouid:TGUID;out dbo:TFRE_DB_Object;const internal_object : boolean=false): boolean;
    function  StoreOrUpdateObject  (var   obj:TFRE_DB_Object ; const collection_name : TFRE_DB_NameType ; const store : boolean ; const raise_ex : boolean ; var notify_collections : TFRE_DB_StringArray) : TFRE_DB_Errortype;
    procedure SyncWriteWAL         (const WALMem : TMemoryStream);
    procedure SyncSnapshot         (const final : boolean=false);
  end;

  //TFRE_DB_Object_ManageInfo=class
  //  Fkey               : TGuid;
  //  obj_link           : TFRE_DB_Object;
  //  FConnection        : TFRE_DB_BASE_CONNECTION;
  //  Fis_dirty          : boolean;
  //  function           IsUnmanaged : boolean ; // Directly stored in an unmanaged collection (mem only)
  //public
  //  constructor Create             (const obj:TFRE_DB_Object;const guid:TGuid;const connection: TFRE_DB_BASE_CONNECTION);
  //  procedure   FinalizeObject     (const dont_cache : Boolean); // Free Memory
  //  procedure   InstanceIt         ;
  //  procedure   CacheIt            ;
  //  procedure   DeleteIt           ;
  //  function    IsDirty            :boolean;
  //  procedure   SetObjManInfDirty  ;
  //  function    CloneOutObject     : TFRE_DB_Object;
  //end;

  //TFRE_DB_Collection_ManageInfo=class
  //  FKey               : TFRE_DB_NameType;
  //  FUP_key            : TFRE_DB_NameType;
  //  collection_link    : TFRE_DB_COLLECTION;
  //  FConnection        : TFRE_DB_BASE_CONNECTION;
  //  Fis_master         : boolean;
  //  Fis_memory_only    : boolean;
  //public
  //  constructor Create             (const collection:TFRE_DB_COLLECTION;collection_name:TFRE_DB_NameType;const connection: TFRE_DB_BASE_CONNECTION;const is_master:boolean=false;const is_memory_only:boolean=false);
  //  //procedure   InstanceIt         ;
  //  function    IsMaster           : boolean;
  //  function    IsMemoryOnly       : boolean;
  //  function    GetConnection      : TFRE_DB_BASE_CONNECTION;
  //  function    Key                : TFRE_DB_NameType;
  //end;

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

  { TFRE_DB_RIGHT }

  TFRE_DB_RIGHT=class(TFRE_DB_NAMED_OBJECT,IFRE_DB_RIGHT)
    function  IFRE_DB_RIGHT.GetDesc        = GetDescI;
    function  IFRE_DB_RIGHT.SetDesc        = SetDescI;
  public
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  end;

  { TFRE_DB_USER }

  TFRE_DB_USER=class(TFRE_DB_Object,IFRE_DB_USER)
  private
    Flogin  : TFRE_DB_FIELD;
    function  GetDomain   : TFRE_DB_NameType;
    function  GetDomainID : TGUID;
    function  GetFirstName: TFRE_DB_String;
    function  GetGIDA: TFRE_DB_ObjLinkArray;
    function  GetLastName: TFRE_DB_String;
    function  GetLogin: TFRE_DB_String;
    function  GetUGA: TFRE_DB_StringArray;
    procedure SetFirstName(const AValue: TFRE_DB_String);
    procedure SetGIDA(AValue: TFRE_DB_ObjLinkArray);
    procedure SetLastName(const AValue: TFRE_DB_String);
    procedure Setlogin(const AValue: TFRE_DB_String);
    procedure SetDomainID(AValue: TGUID);
    procedure SetDomain(const domainname: TFRE_DB_NameType);
    procedure _UpdateDomainLoginKey;
  protected
    procedure InternalSetup; override;
  public
    function  SubFormattedDisplayAvailable: boolean; override;
    function  GetSubFormattedDisplay(indent: integer=4): TFRE_DB_String; override;
    procedure SetImage           (const image_stream : TFRE_DB_Stream);
    procedure InitData           (const nlogin,nfirst,nlast,npasswd:TFRE_DB_String);
    property  Login              :TFRE_DB_String read GetLogin write Setlogin;
    property  Firstname          :TFRE_DB_String read GetFirstName write SetFirstName;
    property  Lastname           :TFRE_DB_String read GetLastName write SetLastName;
    property  UserGroupNames     :TFRE_DB_StringArray read GetUGA;
    property  UserGroupIDs       :TFRE_DB_ObjLinkArray read GetGIDA write SetGIDA;
    procedure SetPassword        (const pw:TFRE_DB_String);
    function  Checkpassword      (const pw:TFRE_DB_String):boolean;
    function  GetRightsArray     :TFRE_DB_StringArray;
    property  DomainID           :TGUID read GetDomainID write SetDomainID;
    property  Domain             :TFRE_DB_NameType read GetDomain write SetDomain;
    class procedure RegisterSystemScheme     (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class function  GetDomainLoginKey        (const loginpart : TFRE_DB_String; const domain_id : TGUID) : TFRE_DB_String;
  published
    class     function  IMC_NewUserOperation (const input:IFRE_DB_Object): IFRE_DB_Object;
    function  IMI_SAVEOPERATION              (const input:IFRE_DB_Object): IFRE_DB_Object;
  end;

  { TFRE_DB_DOMAIN }

  TFRE_DB_DOMAIN=class(TFRE_DB_NAMED_OBJECT,IFRE_DB_DOMAIN)
  private
    function  IFRE_DB_DOMAIN.GetDesc          = GetDescI;
    function  IFRE_DB_DOMAIN.SetDesc          = SetDescI;
  public
    class procedure RegisterSystemScheme      (const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published
    class function  IMC_NewDomainOperation    (const input:IFRE_DB_Object): IFRE_DB_Object;
  end;

  { TFRE_DB_GROUP }

  TFRE_DB_GROUP=class(TFRE_DB_NAMED_OBJECT,IFRE_DB_GROUP)
  private
    function  GetDomain   : TFRE_DB_NameType;
    function  GetDomainID : TGUID;
    function  GetFullName : TFRE_DB_String;
    function  GetRoleNames: TFRE_DB_StringArray;
    function  GetRoleIDs  : TFRE_DB_ObjLinkArray;
    function  IFRE_DB_GROUP.GetDesc          = GetDescI;
    function  IFRE_DB_GROUP.SetDesc          = SetDescI;
    function  IFRE_DB_GROUP.AddUserToGroup   = AddUserToGroupI;
    function  IFRE_DB_GROUP.RemoveUserFromGroup = RemoveUserFromGroupI;
    procedure SetDomainID(AValue: TGUID);
    procedure SetDomain(const domainname: TFRE_DB_NameType);
    procedure SetRoleIDs(AValue: TFRE_DB_ObjLinkArray);
    procedure _UpdateDomainGroupKey;
  public
    class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class function  GetDomainGroupKey      (const grouppart : TFRE_DB_String; const domain_id : TGUID) : TFRE_DB_String;
    function  AddUserToGroupI              (const user :IFRE_DB_USER):TFRE_DB_Errortype;
    function  RemoveUserFromGroupI         (const user :IFRE_DB_USER):TFRE_DB_Errortype;
    function  AddUserToGroup               (const user :TFRE_DB_USER):TFRE_DB_Errortype;
    function  RemoveUserFromGroup          (const user :TFRE_DB_USER):TFRE_DB_Errortype;
    function  SubFormattedDisplayAvailable : boolean; override;
    function  GetSubFormattedDisplay       (indent: integer=4): TFRE_DB_String; override;
    property  RoleNames                    :TFRE_DB_StringArray read GetRoleNames;
    property  RoleIDs                      :TFRE_DB_ObjLinkArray read GetRoleIDs write SetRoleIDs;
    property  DomainID                     :TGUID read GetDomainID write SetDomainID;
    property  Domain                       :TFRE_DB_NameType read GetDomain write SetDomain;
    property  Fullname                     :TFRE_DB_String read GetFullname;
  published
    class     function  IMC_NewGroupOperation (const input:IFRE_DB_Object): IFRE_DB_Object;
    function  IMI_SAVEOPERATION               (const input:IFRE_DB_Object): IFRE_DB_Object;
  end;

  { TFRE_DB_ROLE }

  TFRE_DB_ROLE=class(TFRE_DB_NAMED_OBJECT,IFRE_DB_ROLE)
  private
    function  IFRE_DB_ROLE.GetDesc          = GetDescI;
    function  IFRE_DB_ROLE.SetDesc          = SetDescI;
    function  IFRE_DB_ROLE.Addright         = AddRightI;
    function  GetDomain                     : TFRE_DB_NameType;
    function  GetDomainID                   : TGUID;
    function  GetFullname                   : TFRE_DB_String;
    procedure SetDomainID                   (AValue: TGUID);
    procedure SetDomain                     (const domainname: TFRE_DB_NameType);
    procedure _UpdateDomainRoleKey;
  public
    class procedure RegisterSystemScheme    (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class function  GetDomainRoleKey        (const rolepart : TFRE_DB_String; const domain_id : TGUID) : TFRE_DB_String;
    procedure AddRightI                     (const right:IFRE_DB_RIGHT);
    procedure AddRight                      (const right:TFRE_DB_RIGHT);
    function  SubFormattedDisplayAvailable  : boolean; override;
    function  GetSubFormattedDisplay        (indent: integer=4): TFRE_DB_String; override;
    function  GetRightNames                 :TFRE_DB_StringArray;
    property  DomainID                      :TGUID read GetDomainID write SetDomainID;
    property  Domain                        :TFRE_DB_NameType read GetDomain write SetDomain;
    property  Fullname                      :TFRE_DB_String read GetFullname;
  end;

  { TFRE_DB_APPDATA }

  TFRE_DB_APPDATA=class(TFRE_DB_NAMED_OBJECT,IFRE_DB_APPDATA)
  private
    function  IFRE_DB_APPDATA.GetDesc          = GetDescI;
    function  IFRE_DB_APPDATA.SetDesc          = SetDescI;
    function  IFRE_DB_APPDATA.GetVersion    = GetVersionI;
    function  IFRE_DB_APPDATA.SetVersion    = SetVersionI;
  public
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure SetVersionI(AValue: TFRE_DB_String);
    procedure SetVersion(AValue: TFRE_DB_String);
    function  GetVersionI:TFRE_DB_String;
    function  GetVersion:TFRE_DB_String;
   end;


  //_TFRE_DB_ObjTree          = specialize TGFOS_RBTree<TGUID,TFRE_DB_Object_ManageInfo>;
  //_TFRE_DB_ObjLinkTree      = specialize TGFOS_RBTree<TGUID,TFRE_DB_Object_ManageInfo>; // Link to ObjTree


  procedure          ForAllObjectsDo (const object_array:TFRE_DB_ObjectArray ; const iterator:TFRE_DB_Obj_Iterator);
  procedure          ForAllGuidsDo   (const guid_array:TFRE_DB_GUIDArray   ; const iterator:TFRE_DB_Guid_Iterator);

type

  TFRE_DB_COLLECTIONCLASS  = class of TFRE_DB_COLLECTION;


  { IFRE_DB_COLLECTION_OBSERVER }

  IFRE_DB_COLLECTION_OBSERVER = interface
    procedure ICO_CollectionNotify (const notify_type : TFRE_DB_NotifyObserverType ; const obj : TFRE_DB_Object ; const obj_uid: TGUID);
    function  ICO_ObserverID       : String;
  end;

  RFRE_DB_UPDATE_ENTRY = record
                           update_type : TFRE_DB_NotifyObserverType;
                           update_obj  : TFRE_DB_Object;
                           update_uid  : TGuid;
                         end;

  OFRE_DB_ObserverList = specialize OGFOS_Array<IFRE_DB_COLLECTION_OBSERVER>;

  { TFRE_DB_COLLECTION }

  TFRE_DB_COLLECTION=class(TFRE_DB_Object,IFRE_DB_COLLECTION)
  private
    FIsTemporary           : Boolean;
    FObjectLinkStore       : IFRE_DB_PERSISTANCE_COLLECTION; //? Necessary to be referenced here
    FName                  : TFRE_DB_NameType;
    FObservers             : OFRE_DB_ObserverList;
    FObserverUpdates       : Array of RFRE_DB_UPDATE_ENTRY;
    FObserverBlockupdating : Boolean;

    //function         _InternalAdd        (const new_guid:TGuid;const with_obj : TFRE_DB_Object=nil):TFRE_DB_Object_ManageInfo;
    //function         _InternalFetch      (const guid:TGuid):TFRE_DB_Object_ManageInfo;
    //function         _IsMasterCollection :boolean;

    procedure        __NotifyCollectionObservers   (const notify_type : TFRE_DB_NotifyObserverType ; const obj : TFRE_DB_Object ; const obj_uid: TGUID);
    procedure        _NotifyObserversOrRecord      (const notify_type : TFRE_DB_NotifyObserverType ; const obj : TFRE_DB_Object ; const obj_uid: TGUID);
  protected
    class function  Forced_In_Memory               : Boolean;virtual;

    //procedure       BeforeSave                     ; override;
    //procedure       AfterSave                      ; override;
    //procedure       AfterLoad                      ; override;

    procedure       ForAllI         (const func:IFRE_DB_Obj_Iterator);
    procedure       ForAllBreakI    (const func:IFRE_DB_Obj_IteratorBreak);
    function        LinearScanI     (const fieldname:TFRE_DB_NameType;const field_expr:TFRE_DB_FIELD_EXPRESSION):IFRE_DB_Object; // Linear Search / Fetch/Semantics/Finalize needed
    function        StoreI          (var   new_obj:IFRE_DB_Object):TFRE_DB_Errortype;
    function        UpdateI         (const dbo:IFRE_DB_Object):TFRE_DB_Errortype;
    function        FetchI          (const ouid:TGUID;out dbo:IFRE_DB_Object): boolean;
    function        FirstI          : IFRE_DB_Object;
    function        LastI           : IFRE_DB_Object;
    //function        IsTemporary     : Boolean;
    procedure       Clear           ;

    function IFRE_DB_COLLECTION.ForAll        = ForAllI;
    function IFRE_DB_COLLECTION.ForAllBreak   = ForAllBreakI;
    function IFRE_DB_COLLECTION.ForAllModify  = ForAllModifyI;
    function IFRE_DB_COLLECTION.Store         = StoreI;
    function IFRE_DB_COLLECTION.Update        = UpdateI;
    function IFRE_DB_COLLECTION.LinearScan    = LinearScanI;
    function IFRE_DB_COLLECTION.First         = FirstI;
    function IFRE_DB_COLLECTION.Last          = LastI;
    function IFRE_DB_COLLECTION.Fetch         = FetchI;
    function IFRE_DB_COLLECTION.GetIndexedObj = GetIndexedObjI;
  public
    //function        Field                          (const name: TFRE_DB_String): TFRE_DB_FIELD; override;
    //constructor     CreateStreaming                (const conn:TFRE_DB_BASE_CONNECTION=nil;const ExtensionObjectMediatorClass:TFRE_DB_OBJECTCLASSEX=nil); override;
    constructor     Create         (const connection:TFRE_DB_BASE_CONNECTION;const name:TFRE_DB_NameType;const pers_coll:IFRE_DB_PERSISTANCE_COLLECTION);
    destructor      Destroy        ;override;
    function        Count          : QWord; virtual;
    function        Exists         (const ouid:TGUID):boolean;
    procedure       ForAll         (const func:TFRE_DB_Obj_Iterator);
    procedure       ForAllBreak    (const func:TFRE_DB_Obj_IteratorBreak);
    function        Remove         (const ouid:TGUID):boolean; virtual;
    function        Store          (var   new_obj:TFRE_DB_Object):TFRE_DB_Errortype;virtual;
    function        Update         (const dbo:TFRE_DB_Object):TFRE_DB_Errortype;virtual;
    function        Fetch          (const ouid:TGUID;out dbo:TFRE_DB_Object): boolean;virtual;

    procedure       ClearCollection;

    function        CollectionName :TFRE_DB_NameType;

    function        LinearScan          (const fieldname:TFRE_DB_NameType;const field_expr:TFRE_DB_FIELD_EXPRESSION):TFRE_DB_Object; // Linear Search

    function        AddObserver         (const obs : IFRE_DB_COLLECTION_OBSERVER):boolean;
    function        RemoveObserver      (const obs : IFRE_DB_COLLECTION_OBSERVER):boolean;

    procedure       StartBlockUpdating;
    procedure       FinishBlockUpdating;

    function        DefineIndexOnField  (const FieldName   : TFRE_DB_NameType;const FieldType:TFRE_DB_FIELDTYPE;const unique:boolean; const ignore_content_case:boolean=false;const index_name:TFRE_DB_NameType='def'):TFRE_DB_Errortype;
    function        ExistsIndexed       (const query_value : TFRE_DB_String;const index_name:TFRE_DB_NameType='def'):Boolean; // for the string fieldtype

    function        GetIndexedObjI      (const query_value : TFRE_DB_String;out obj:IFRE_DB_Object;const index_name:TFRE_DB_NameType='def'):boolean; // for the string fieldtype
    function        GetIndexedObj       (const query_value : TFRE_DB_String;out obj:TFRE_DB_Object;const index_name:TFRE_DB_NameType='def'):boolean; // for the string fieldtype
    function        GetIndexedUID       (const query_value : TFRE_DB_String;out obj_uid:TGUID;const index_name:TFRE_DB_NameType='def'):boolean; // for the string fieldtype

    function        RemoveIndexed       (const query_value : TFRE_DB_String;const index_name:TFRE_DB_NameType='def'):boolean; // for the string fieldtype

    function        ItemCount      : Int64         ; virtual;
    function        First          : TFRE_DB_Object; virtual;
    function        Last           : TFRE_DB_Object; virtual;
    function        GetItem        (const num:uint64):IFRE_DB_Object; virtual;
    procedure       ForceFullUpdateForObservers;
  end;


  //TODO -> KILL ? / SIMPLIFY
  TFRE_DB_SCHEME_COLLECTION=class(TFRE_DB_COLLECTION,IFRE_DB_SCHEME_COLLECTION)
  private
    function IFRE_DB_SCHEME_COLLECTION.ForAll        = ForAllI;
    function IFRE_DB_SCHEME_COLLECTION.ForAllBreak   = ForAllBreakI;
    function IFRE_DB_SCHEME_COLLECTION.ForAllModify  = ForAllModifyI;
    function IFRE_DB_SCHEME_COLLECTION.Store         = StoreI;
    function IFRE_DB_SCHEME_COLLECTION.Update        = UpdateI;
    function IFRE_DB_SCHEME_COLLECTION.LinearScan    = LinearScanI;
    function IFRE_DB_SCHEME_COLLECTION.First         = FirstI;
    function IFRE_DB_SCHEME_COLLECTION.Last          = LastI;
    function IFRE_DB_SCHEME_COLLECTION.Fetch         = FetchI;
    function IFRE_DB_SCHEME_COLLECTION.GetIndexedObj = GetIndexedObjI;
  protected
    //procedure InternalSetup; override;
  public
    function  Store        (var obj: TFRE_DB_Object): TFRE_DB_Errortype; override;
    function  StoreScheme  (var obj: TFRE_DB_SchemeObject): TFRE_DB_Errortype;
    function  GetScheme    (const scheme_name:TFRE_DB_String;var scheme:TFRE_DB_SchemeObject):boolean;
    function  SchemeExists (const scheme_name:TFRE_DB_String):boolean;
    function  RemoveScheme (const obj: TFRE_DB_SchemeObject):boolean;
  end;


  //Base Class Tranforms a DB Object into another DB Object
  //Has support for "FilterFields" that are fields that may get filtered but are not in the output

  { TFRE_DB_TRANSFORMOBJECT }

  TFRE_DB_TRANSFORMOBJECT=class(TFRE_DB_Object,IFRE_DB_TRANSFORMOBJECT)
  private
    Fkeyfield : TFRE_DB_FIELD;
    function  GetHasFF: boolean;
    function  GetKey: TFRE_DB_String;
    procedure SetKey(const AValue: TFRE_DB_String);
    procedure _SetHasFilterField(const value:boolean);
    function  _ObjectsNeedsNoSubfieldSchemeCheck:boolean;override;
  protected
    procedure InternalSetup; override;
  public
    function  TransformInOut (const dependency_obj : IFRE_DB_Object ; const input: TFRE_DB_Object ; const filter_fields:boolean=false): TFRE_DB_Object; virtual;
    procedure TransformOutIn (const transformed : TFRE_DB_Object); virtual;
    property  Key : TFRE_DB_String read GetKey write SetKey;
    property  HasFilterFields : boolean read GetHasFF;
  end;

  { TFRE_DB_SIMPLE_TRANSFORM }

  TFRE_DB_SIMPLE_TRANSFORM=class(TFRE_DB_TRANSFORMOBJECT,IFRE_DB_SIMPLE_TRANSFORM)
  private
   FCustTransform : IFRE_DB_CUSTOMTRANSFORM;
  public
    function  TransformInOut                 (const dependency_obj : IFRE_DB_Object ; const input: TFRE_DB_Object ; const filter_fields:boolean=false): TFRE_DB_Object; override;
    //@ Add a Field that collects STRING values to a new String Field
    //@ format : format string of the new field ; in_fieldlist : list of input fieldnames ; filter_field : true = result field is not in output but can be filtered ; output_title : name of the output field, default=same as input
    procedure SetCustomTransformFunction     (const func : IFRE_DB_CUSTOMTRANSFORM);
    procedure AddCollectorscheme             (const format:TFRE_DB_String;const in_fieldlist:TFRE_DB_StringArray;const out_field:TFRE_DB_String;const filter_field:boolean=false;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    procedure AddFulltextFilterOnTransformed (const in_fieldlist:TFRE_DB_StringArray);
    procedure AddOneToOnescheme              (const fieldname:TFRE_DB_String;const out_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1;const iconID:String='');
    procedure AddProgressTransform           (const valuefield:TFRE_DB_String;const out_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const textfield:TFRE_DB_String='';const out_text:TFRE_DB_String='';const maxValue:Single=100;const fieldSize: Integer=1);
    procedure AddConstString                 (const out_field,value:TFRE_DB_String;const display: Boolean=false; const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    procedure AddDBTextShortToOne            (const fieldname:TFRE_DB_String;const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    procedure AddDBTextLongToOne             (const fieldname:TFRE_DB_String;const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    procedure AddDBTextHintToOne             (const fieldname:TFRE_DB_String;const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    procedure AddDBTextKeyToOne              (const fieldname:TFRE_DB_String;const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    procedure AddFullDumpField               (const fieldname:TFRE_DB_String; const dump_length_max:Integer=0;const filter_field:Boolean=false;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    //Get a Viewcollectiondescription depending on the defined fields of the transformation
    function  GetViewCollectionDescription   : TFRE_DB_CONTENT_DESC;

    procedure AddMatchingReferencedField     (const ref_field_chain: TFRE_DB_StringArray;const target_field:TFRE_DB_String;const output_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    procedure AddMatchingReferencedField     (const ref_field      : TFRE_DB_String     ;const target_field:TFRE_DB_String;const output_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
  end;

  { TFRE_DB_TREE_TRANSFORM }

  TFRE_DB_TREE_TRANSFORM=class(TFRE_DB_TRANSFORMOBJECT,IFRE_DB_TRANSFORMOBJECT)
    function  TransformInOut(const dependency_obj: IFRE_DB_Object; const input: TFRE_DB_Object; const filter_fields: boolean=false): TFRE_DB_Object; override; // todo - remove unnecessary fields
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
    function  TransformInOut(const dependency_obj: IFRE_DB_Object ; const input: TFRE_DB_Object ; const filter_fields:boolean=false): TFRE_DB_Object; override; // todo - remove unnecessary fields / transform only series fields
    destructor Destroy;override;
  end;

  TFRE_DB_SortTree = specialize TGFOS_RBTree<TFRE_DB_Object,boolean>; // Link to ManageInfo if NO Transform !

  { TFRE_DB_DERIVED_COLLECTION }
  TFRE_DB_Orderdef=packed record
    fieldname : String[40];
    ascending : boolean;
  end;

  //@ Used for filtered, sorted Collections, is based on  a "real" collection
  TFRE_DB_DERIVED_COLLECTION=class(TFRE_DB_COLLECTION,IFRE_DB_DERIVED_COLLECTION,IFRE_DB_COLLECTION_OBSERVER)
  private
   type
     TDC_Mode=(dc_None,dc_Map2RealCollection,dc_VirtualCollection,dc_Map2DerivedCollection,dc_ReferentialLinkCollection);
   var
    FDCMode            : TDC_Mode;
    FDepObjectsRefers  : Boolean;
    FDepObjectsRefNeg  : Boolean;
    FDepRefConstraint  : TFRE_DB_StringArray;
    FDependencyRef     : TFRE_DB_StringArray; // Array of inbound dpendencies (usually one)
    FParentIds         : TFRE_DB_GUIDArray;
    FDependencyObject  : IFRE_DB_Object;
    FDepObjectList     : TFRE_DB_GUIDArray; // Must be extended to an array of ... (FDependencyRef ..)
    FUseDepAsLinkFilt  : Boolean;
    FUseDepFiltInvert  : Boolean;
    FChildParentMode   : Boolean;
    FParentChldLinkFld : TFRE_DB_NameType;
    FObserverAdded     : Boolean;

    FExpandedRefs      : TFRE_DB_ObjectArray;
    FDBOList           : TFRE_DB_SortTree;  // Link to

    FParentCollection  : TFRE_DB_COLLECTION;
    FIdField           : String;

    FFilters           : TFRE_DB_Object;
    FFiltersTrans      : TFRE_DB_Object;
    FOrders            : TFRE_DB_Object;
    FOrdersTrans       : TFRE_DB_Object;
    FOrderDef          : Array of TFRE_DB_OrderDef;

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
    FTreeNodeCaption   : TFRE_DB_StringArray;
    FTreeNodeIconField : TFRE_DB_String;
    FGatherUpdateList  : TFRE_DB_UPDATE_STORE_DESC; // Collects Updates between Start and End


    FCurrentOrder      : TFRE_DB_DC_ORDER_LIST;
    FCurrentStrFilters : TFRE_DB_DC_STRINGFIELDKEY_LIST;
    FInitialDerived    : Boolean;
    FSession           : TFRE_DB_UserSession;
    procedure       _CheckSetDisplayType (const CollectionDisplayType: TFRE_COLLECTION_DISPLAY_TYPE);
    procedure       _ClearMode;

    procedure       ICO_CollectionNotify (const notify_type : TFRE_DB_NotifyObserverType ; const obj : TFRE_DB_Object ; const obj_uid: TGUID);
    function        ICO_ObserverID       : String;

    procedure       BeginUpdateGathering       ;
    procedure       FinishUpdateGathering      (const sendupdates : Boolean);
    procedure      _AddToTransformedCollection (item:TFRE_DB_Object;const send_client_notify:boolean=false;const update_data:boolean=false;const child_call : boolean=false);

    function        _CheckUIDExists(obj_uid : TGuid) : Boolean;


    procedure       InternalSetup              ; override;
    procedure       _FilterIt                  (const childcall : boolean);
    function        _CompareObjects  (const ob1,ob2 : TFRE_DB_Object):NativeInt;
    function        _DeleteFilterkey (const filter_key:TFRE_DB_String;const on_transform:boolean):TFRE_DB_Errortype;

    function   _Get_DC_Order                 (const input:IFRE_DB_Object):TFRE_DB_DC_ORDER_LIST;
    function   _Get_DC_StringfieldKeys       (const input:IFRE_DB_Object):TFRE_DB_DC_STRINGFIELDKEY_LIST;
    function   _Get_DC_PageingInfo           (const input:IFRE_DB_Object):TFRE_DB_DC_PAGING_INFO;
    function   _Get_DC_ReferenceList         (const input:IFRE_DB_Object):TFRE_DB_GUIDArray;
    function   _Get_DC_QueryID               (const input:IFRE_DB_Object):String;

    function   RemoveQueryIDWatch            (const QID:String):TFRE_DB_Errortype;
    function   AddQueryIDWatch               (const QID:String;const page_i:TFRE_DB_DC_PAGING_INFO):TFRE_DB_Errortype;

    procedure  DC_SetFilters_From_Input      (const filter_defs:TFRE_DB_Object);
    procedure  ForAllI                       (const func:IFRE_DB_Obj_Iterator);

  protected
    class function Forced_In_Memory: Boolean; override;
    procedure BindSession              (const session : TFRE_DB_UserSession);
    procedure  ApplyToPageI            (const page_info : TFRE_DB_DC_PAGING_INFO;const iterator:IFRE_DB_Obj_Iterator);
    function IFRE_DB_DERIVED_COLLECTION.ForAll                   = ForAllI;
    function IFRE_DB_DERIVED_COLLECTION.ForAllBreak              = ForAllBreakI;
    function IFRE_DB_DERIVED_COLLECTION.ForAllModify             = ForAllModifyI;
    function IFRE_DB_DERIVED_COLLECTION.Store                    = StoreI;
    function IFRE_DB_DERIVED_COLLECTION.Update                   = UpdateI;
    function IFRE_DB_DERIVED_COLLECTION.LinearScan               = LinearScanI;
    function IFRE_DB_DERIVED_COLLECTION.First                    = FirstI;
    function IFRE_DB_DERIVED_COLLECTION.Last                     = LastI;
    function IFRE_DB_DERIVED_COLLECTION.Fetch                    = FetchI;
    function IFRE_DB_DERIVED_COLLECTION.FetchFromParent          = FetchFromParentI;
    function IFRE_DB_DERIVED_COLLECTION.ApplyToPage              = ApplyToPageI;
    function IFRE_DB_DERIVED_COLLECTION.SetDeriveParent          = SetDeriveParentI;
    function IFRE_DB_DERIVED_COLLECTION.SetDeriveTransformation  = SetDeriveTransformationI;
    function IFRE_DB_DERIVED_COLLECTION.GetIndexedObj            = GetIndexedObjI;

  public
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    destructor Destroy;override;
    //@Set a String Filter, which can be used before or after the transformation
    //@ filterkey = ID of the Filter / field_name : on which field the filter works / filtertype: how the filter works / on_transform : true = work after transformation / on_filter_field : true = filter works on a transform filter field which is not in the output
    function   AddStringFieldFilter    (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_StringArray;const filtertype:TFRE_DB_STR_FILTERTYPE;const on_transform:boolean=true;const on_filter_field:boolean=false):TFRE_DB_Errortype;
    //function   RemoveStringFieldFilter (const filter_key:TFRE_DB_String;const on_transform:boolean):TFRE_DB_Errortype; //deprecated
    function   AddBooleanFieldFilter   (const filter_key,field_name:TFRE_DB_String;const value :Boolean;const on_transform:boolean=true;const on_filter_field:boolean=false):TFRE_DB_Errortype;
    // Add a UID Field Filter | Match types : dbnf_EXACT,dbnf_EXACT_NEGATED,dbnf_AllValuesFromFilter,dbnf_OneValueFromFilter
    // dbnf_EXACT,dbnf_EXACT_NEGATED : the filter values array must match the target object field array in order or negated
    function   AddUIDFieldFilter              (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_GUIDArray     ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddByteFieldFilter             (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_ByteArray     ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddInt16FieldFilter            (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_Int16Array    ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddUInt16FieldFilter           (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_UInt16Array   ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddInt32FieldFilter            (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_Int32Array    ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddUInt32FieldFilter           (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_UInt32Array   ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddInt64FieldFilter            (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_Int64Array    ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddUInt64FieldFilter           (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_UInt64Array   ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddReal32FieldFilter           (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_Real32Array   ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddReal64FieldFilter           (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_Real64Array   ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddCurrencyFieldFilter         (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_CurrencyArray ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddDateTimeFieldFilter         (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_DateTimeArray ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddSchemeFilter                (const filter_key           :TFRE_DB_String;const values:TFRE_DB_StringArray   ;const negate:boolean ) :TFRE_DB_Errortype;
    function   AddRightFilterForEntryAndUser  (const filter_key           :TFRE_DB_String;const right_prefix:TFRE_DB_NameType;const fieldname_for_uid : TFRE_DB_NameType):TFRE_DB_Errortype;




    function   RemoveFieldFilter       (const filter_key:TFRE_DB_String;const on_transform:boolean=true):TFRE_DB_Errortype;
    function   AddOrderField           (const order_key,field_name:TFRE_DB_String;const ascending : boolean):TFRE_DB_Errortype;
    procedure  RemoveAllOrderFields    ;
    procedure  RemoveAllFilterFields   ;
    procedure  RemoveAllFiltersPrefix  (const prefix:string);

    function   Store                           (var obj: TFRE_DB_Object)     :TFRE_DB_Errortype; override;
    function   Remove                          (const ouid:TGUID)            :boolean; override;
    function   Update                          (const dbo:TFRE_DB_Object)    :TFRE_DB_Errortype; override;

    procedure  SetDeriveParent                 (const coll:TFRE_DB_COLLECTION; const idField: String='uid');
    // Creates a virtual Parent Collection In Memory, which acts as derive Parent
    procedure  SetVirtualMode                  (const StringIndexKey : String='');
    procedure  SetReferentialLinkMode          (const scheme_and_field_constraint : TFRE_DB_String ; const dependency_object_refers : boolean ; const dependency_reference : string = 'uids');
    procedure  SetUseDependencyAsRefLinkFilter (const scheme_and_field_constraint : TFRE_DB_String ; const dependency_object_refers : boolean ; const negate : boolean ; const dependency_reference : string = 'uids');


    procedure  SetDeriveParentI        (const coll:IFRE_DB_COLLECTION; const idField: String='uid');
    procedure  SetDeriveTransformation (const tob:TFRE_DB_TRANSFORMOBJECT);
    procedure  SetDeriveTransformationI(const tob:IFRE_DB_TRANSFORMOBJECT);
    function   Derive                  : TFRE_DB_Errortype;
    function   ItemCount               : Int64;override;
    function   Count                   : QWord; override;
    function   First                   : TFRE_DB_Object   ; override;
    function   Last                    : TFRE_DB_Object   ; override;
    function   GetItem                 (const num:uint64):IFRE_DB_Object; override;
    function   Fetch                   (const ouid:TGUID;out dbo:TFRE_DB_Object): boolean;override;
    function   FetchFromParent         (const ouid:TGUID;out dbo:TFRE_DB_Object): boolean;
    function   FetchFromParentI        (const ouid:TGUID;out dbo:IFRE_DB_Object): boolean;

    procedure  RemoveAllEntries        ;

    procedure  ApplyToPage             (const QueryID:String ; const page_info:TFRE_DB_DC_PAGING_INFO;const iterator:TFRE_DB_Obj_Iterator);
    procedure  ApplyToData             (const iterator:TFRE_DB_Obj_Iterator);

    function   GetStoreDescription        : TFRE_DB_CONTENT_DESC;
    function   getDescriptionStoreId      : String;
    procedure  SetDisplayType             (const CollectionDisplayType : TFRE_COLLECTION_DISPLAY_TYPE ; const Flags:TFRE_COLLECTION_GRID_DISPLAY_FLAGS;const title:TFRE_DB_String;const TreeNodeCaptionFields:TFRE_DB_StringArray=nil;const TreeNodeIconField:TFRE_DB_String='';
                                           const item_menu_func: TFRE_DB_SERVER_FUNC_DESC=nil; const item_details_func: TFRE_DB_SERVER_FUNC_DESC=nil; const selection_dep_func: TFRE_DB_SERVER_FUNC_DESC=nil; const tree_menu_func: TFRE_DB_SERVER_FUNC_DESC=nil; const drop_func: TFRE_DB_SERVER_FUNC_DESC=nil; const drag_func: TFRE_DB_SERVER_FUNC_DESC=nil); //TODO: Make Callable Once
    procedure  SetDisplayTypeChart        (const title: TFRE_DB_String; const chart_type: TFRE_DB_CHART_TYPE; const series_field_names: TFRE_DB_StringArray; const use_series_colors:boolean; const use_series_labels : boolean;const series_labels: TFRE_DB_StringArray=nil; const showLegend: Boolean=false; const maxValue: Integer=0);
    procedure  SetChildToParentLinkField  (const fieldname : TFRE_DB_NameType);
    procedure  SetParentToChildLinkField  (const fieldname : TFRE_DB_NameType);

    function   GetDisplayDescription   : TFRE_DB_CONTENT_DESC;
    function   GetDisplayDescriptionFunction (const FilterEventKey:TFRE_DB_String): TFRE_DB_SERVER_FUNC_DESC;
    procedure  AddVirtualChildEntry    (const caption:TFRE_DB_String; const FuncClass: String; const uidPath: TFRE_DB_StringArray; const ChildrenFunc:String='ChildrenData';const ContentFunc:String='Content';const MenuFunc:String='Menu');
    procedure  _CheckObserverAdded     (const add:boolean);
  published
    function   IMI_GET_GRID_DATA       (const input:IFRE_DB_Object):IFRE_DB_Object;
    function   IMI_GET_CHART_DATA      (const input:IFRE_DB_Object):IFRE_DB_Object;
    function   IMI_CLEAR_QUERY_RESULTS (const input:IFRE_DB_Object):IFRE_DB_Object; // Client says that it is not interested in that particular query id from now on.
    function   IMI_GET_CHILDREN_DATA   (const input:IFRE_DB_Object):IFRE_DB_Object;
    function   IMI_GET_DISPLAY_DESC    (const input:IFRE_DB_Object):IFRE_DB_Object;
    function   IMI_DESTROY_STORE       (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;


  TFRE_DB_Coll_Iterator     = procedure (const coll:TFRE_DB_COLLECTION) is nested;

  _TFRE_DB_CollectionTree   = specialize TGFOS_RBTree<TFRE_DB_String,TFRE_DB_COLLECTION>;

  { TFRE_DB_CONNECTION }
  TFRE_DB           =class;


  { TFRE_DB_BASE_CONNECTION }


  TFRE_DB_BASE_CONNECTION=class(TFOS_BASE)
  private
    FDBName               : TFRE_DB_String;
    FCloned               : boolean;
    FConnected            : Boolean;
    FAuthenticated        : Boolean;
    FIntegrityInfoLog     : boolean;
    FIntegrityRebuild     : boolean;

    FPersistance_Layer    : IFRE_DB_PERSISTANCE_LAYER;
    //FObjectStore          : IFRE_DB_PERSISTANCE_MEMSTORE;
    FCollectionStore      : _TFRE_DB_CollectionTree;

    FApps                 : ARRAY of TFRE_DB_APPLICATION;
    FSysWorkflowSchemes   : TFRE_DB_COLLECTION;
    FSysWorkflowInstances : TFRE_DB_COLLECTION;
    FMasterConMan         : TFRE_DB_Collection;

    function            BackupDatabaseReadable      (const to_stream: TStream; const stream_cb: TFRE_DB_StreamingCallback;const progress : TFRE_DB_PhaseProgressCallback): TFRE_DB_Errortype; virtual;
    function            RestoreDatabaseReadable     (const from_stream:TStream;const stream_cb:TFRE_DB_StreamingCallback):TFRE_DB_Errortype;virtual;
    procedure           _ConnectCheck                ;
    procedure           _CloneCheck                  ;
    function            _RefLinkcheck                (const from_obj,to_obj:TGuid):boolean;
    function            GetReferences                (const obj_uid:TGuid;const from:boolean):TFRE_DB_ObjectReferences; virtual;
    function            ReferenceCount               (const obj_uid:TGuid;const from:boolean):NativeInt;
    function            Implementor                  : TObject;
  protected
    procedure          _NotifyCollectionObservers   (const notify_type : TFRE_DB_NotifyObserverType ; const obj : TFRE_DB_Object ; const obj_uid: TGUID ; const ncolls:TFRE_DB_StringArray);
    procedure          _CheckSchemeDefinitions      (const obj:TFRE_DB_Object);
    function           _AppExists                   (const name:TFRE_DB_String):boolean;
    function           _FetchApp                    (const name:TFRE_DB_String;var app:TFRE_DB_APPLICATION):boolean;
    function           _IntegrityCheck              (const log_function:TFRE_LogMsgN=nil):TFRE_DB_Errortype;
    function           _Connect                     (const db:TFRE_DB_String;const is_system:boolean):TFRE_DB_Errortype;virtual;
    procedure          InternalSetupConnection      (const is_system,is_db_restore:boolean); virtual;
    function           CollectionExists             (const name:TFRE_DB_NameType):boolean;virtual;
    function           CollectionCN                 (const collection_name:TFRE_DB_NameType;const NewCollectionClassName:ShortString):TFRE_DB_COLLECTION;virtual;
    function           DeleteCollection             (const name:TFRE_DB_NameType):TFRE_DB_Errortype;virtual;

    function           NewObjectCC                  (const ObjClass:TFRE_DB_OBJECTCLASS)                                     : TFRE_DB_Object; virtual;
    function           _NewObject                   (const Scheme:TFRE_DB_String;const fail_on_no_cc:boolean)                        : TFRE_DB_Object;
    function           NewObjectNS                  (const Scheme:TFRE_DB_String='')                                                 : TFRE_DB_Object; virtual; // No scheme but a Codeclass
    //function           SchemeExists                 (const scheme_name:TFRE_DB_String)                                               : boolean;virtual;
    procedure          Finalize                     ;

    function           DatabaseList                  : IFOS_STRINGS;virtual;abstract;

    function           DatabaseExists                (const dbname:TFRE_DB_String):Boolean;virtual;
    function           CreateDatabase                (const dbname:TFRE_DB_String):TFRE_DB_Errortype;virtual;
    function           DeleteDatabase                (const dbname:TFRE_DB_String):TFRE_DB_Errortype;virtual;
    procedure          DumpSystem                    ;virtual;
  public
    procedure          AssociateObject              (const Obj:TFRE_DB_Object);
    procedure          AssociateObjectI             (const Obj:IFRE_DB_Object);
    function           InstallAppDefaults           (const Appname:TFRE_DB_String ):TFRE_DB_Errortype;
    function           RemoveApp                    (const Appname:TFRE_DB_String ):TFRE_DB_Errortype;
    function           RemoveAppGroup               (const Appname:TFRE_DB_String ; const sub_group_name: TFRE_DB_String):TFRE_DB_Errortype;
    function           AddAppGroup                  (const Appname:TFRE_DB_String ; const groupatdomain:TFRE_DB_String; const short_desc, long_desc: TFRE_DB_String):TFRE_DB_Errortype;

    function           CollectionCC                 (const collection_name:TFRE_DB_NameType;const NewCollectionClass:TFRE_DB_COLLECTIONCLASS;const create_non_existing:boolean=true;const in_memory_only:boolean=false):TFRE_DB_COLLECTION;virtual;

    function           FetchI                       (const ouid:TGUID;out dbo:IFRE_DB_Object)                                : boolean;
    function           FetchAsIntf                  (const ouid:TGUID;const IntfSpec:ShortString; out Intf)                  : boolean;
    function           NewScheme                    (const Scheme_Name: TFRE_DB_String;const parent_scheme_name:TFRE_DB_String='')           : TFRE_DB_SchemeObject;
    function           NewSchemeI                   (const Scheme_Name: TFRE_DB_String;const parent_scheme_name:TFRE_DB_String='')           : IFRE_DB_SchemeObject;

    function           ConnectedName                : TFRE_DB_String;
    function           CollectionList               (const with_classes:boolean=false):IFOS_STRINGS                          ; virtual;
    //function           StoreScheme                  (var   obj: TFRE_DB_SchemeObject)                                        : TFRE_DB_Errortype; virtual;
    //function           StoreSchemeI                 (var   obj: IFRE_DB_SchemeObject)                                        : TFRE_DB_Errortype; virtual;
    function           GetScheme                    (const scheme_name:TFRE_DB_NameType;var scheme:TFRE_DB_SchemeObject)               : boolean;virtual;
    function           GetSchemeI                   (const scheme_name:TFRE_DB_NameType;var scheme:IFRE_DB_SchemeObject)               : boolean;virtual;
    //function           GetSchemeCollection                                                                                   : TFRE_DB_SCHEME_COLLECTION;
    //function           GetSchemeCollectionI                                                                                  : IFRE_DB_SCHEME_COLLECTION;
    //function           RemoveSchemeByName           (const scheme_name:TFRE_DB_String)                                               : boolean;
    function           GetEnum                      (const enum_name  :TFRE_DB_String;var enum:TFRE_DB_Enum)                         : boolean;
    function           GetEnumI                     (const enum_name  :TFRE_DB_String;out enum:IFRE_DB_Enum)                         : boolean;
    //function           StoreEnum                    (var   enum:TFRE_DB_Enum)                                                : TFRE_DB_Errortype;
    //function           StoreEnumI                   (var   enum:IFRE_DB_Enum)                                                : TFRE_DB_Errortype;
    function           GetClientFieldValidator      (const val_name   :TFRE_DB_String;var validator:TFRE_DB_ClientFieldValidator)    : boolean;
    function           GetClientFieldValidatorI     (const val_name   :TFRE_DB_String;out validator:IFRE_DB_ClientFieldValidator)    : boolean;
    //function           StoreClientFieldValidator    (var   validator:TFRE_DB_ClientFieldValidator)                           : TFRE_DB_Errortype;
    //function           StoreClientFieldValidatorI   (var   validator:IFRE_DB_ClientFieldValidator)                           : TFRE_DB_Errortype;
    function           NewObject                    (const Scheme:TFRE_DB_String='')                                                 : TFRE_DB_Object; virtual;
    function           NewObjectI                   (const Scheme:TFRE_DB_String='')                                                 : IFRE_DB_Object; virtual;
    function           Collection                   (const collection_name: TFRE_DB_NameType;const create_non_existing:boolean=true;const in_memory:boolean=false)  : TFRE_DB_COLLECTION;virtual;
    function           CollectionI                  (const collection_name: TFRE_DB_NameType;const create_non_existing:boolean=true;const in_memory:boolean=false)  : IFRE_DB_COLLECTION;virtual;
    function           CollectionAsIntf             (const collection_name:TFRE_DB_NameType;const CollectionInterfaceSpec:ShortString;out Intf;const create_non_existing:boolean=true;const in_memory:boolean=false):boolean; // creates/fetches a Specific Collection
    procedure          NewObjectAsIntf              (const ObjectInterfaceSpec : ShortString ; out Intf)                     ; // creates a new DBO as Interface
    //procedure          ForAllObjects                (const iterator:TFRE_DB_Obj_Iterator)                                    ;
    procedure          ForAllColls                  (const iterator:TFRE_DB_Coll_Iterator)                                   ;virtual;
    procedure          ForAllSchemes                (const iterator:TFRE_DB_Scheme_Iterator)                                 ;
    procedure          ForAllEnums                  (const iterator:TFRE_DB_Enum_Iterator)                                   ;
    procedure          ForAllClientFieldValidators  (const iterator:TFRE_DB_ClientFieldValidator_Iterator)                   ;
    //procedure          ForAllObjectsI               (const iterator:IFRE_DB_Obj_Iterator)                                    ;
    procedure          ForAllCollsI                 (const iterator:IFRE_DB_Coll_Iterator)                                   ;
    procedure          ForAllSchemesI               (const iterator:IFRE_DB_Scheme_Iterator)                                 ;
    procedure          ForAllEnumsI                 (const iterator:IFRE_DB_Enum_Iterator)                                   ;
    procedure          ForAllClientFieldValidatorsI (const iterator:IFRE_DB_ClientFieldValidator_Iterator)                   ;
    function           OverviewDump                 :TFRE_DB_String; virtual;
    constructor        Create                       ;
    constructor        CreateCloned                 (const from:TFRE_DB_BASE_CONNECTION)                                     ; virtual;
    destructor         Destroy                                                                                               ; override;
    function           Exists                       (const ouid:TGUID)                                                       : boolean;
    function           Delete                       (const ouid:TGUID)                                                       : TFRE_DB_Errortype;virtual;
    function           RemoveReferences             (const ouid:TGUID)                                                       : TFRE_DB_Errortype;

    function           InternalCheckRight           (const right_name:TFRE_DB_String):boolean                                ; //Hack
    function           CheckRightForGroup           (const right_name:TFRE_DB_String;const group_uid : TGuid)                :boolean; //Hack


    function           Fetch                        (const ouid:TGUID;out dbo:TFRE_DB_Object)                                : boolean; virtual;
    function           FetchInternal                (const ouid:TGUID;out dbo:TFRE_DB_Object)                                : boolean; virtual;
    function           Update                       (const dbo:TFRE_DB_Object)                                               : TFRE_DB_Errortype;
    function           UpdateI                      (const dbo:IFRE_DB_Object)                                               : TFRE_DB_Errortype;
    function           UpdateScheme                 (const dbo:IFRE_DB_SCHEMEOBJECT)                                         : TFRE_DB_Errortype;
    function           FetchApplications            (var apps : TFRE_DB_APPLICATION_ARRAY)                                   : TFRE_DB_Errortype;virtual;
    function           FetchApplicationsI           (var apps : IFRE_DB_APPLICATION_ARRAY)                                   : TFRE_DB_Errortype;virtual; // with user rights
    function           ApplicationByName            (const name:TFRE_DB_String;var app: IFRE_DB_APPLICATION)                         :boolean;       // with user rights
    //procedure          DrawScheme                   (const datastream:TStream);
    property           RepairOnReferenceCheck       :boolean read FIntegrityRebuild write FIntegrityRebuild;
  end;


  { TFRE_DB_SYSTEM_CONNECTION }
  TFRE_DB_SYSTEM_CONNECTION = class(TFRE_DB_BASE_CONNECTION,IFRE_DB_SYS_CONNECTION)
  private
    FSysAppdata          : TFRE_DB_COLLECTION;
    FSysTransText        : TFRE_DB_COLLECTION;
    FSysUsers            : TFRE_DB_COLLECTION;
    FSysRoles            : TFRE_DB_COLLECTION;
    FSysGroups           : TFRE_DB_COLLECTION;
    FSysDomains          : TFRE_DB_COLLECTION;
    FSysUserSessionsData : TFRE_DB_COLLECTION;
    FRecreateSysObjects  : boolean;

    FConnectionRights    : TFRE_DB_StringArray; // specialized on clone
    FConnectedUser       : TFRE_DB_User;        // specialized on clone

    function    ImpersonateTheClone         (const user,pass:TFRE_DB_String):TFRE_DB_Errortype;
    procedure   InternalSetupConnection     (const is_system,is_db_restore:boolean); override;
    procedure   _SetupSystemGroupsandRoles  (const domain: TFRE_DB_String);
    procedure   _ReloadUserAndRights        ;
    function    _UserExists                 (const loginatdomain:TFRE_DB_String):boolean;
    function    _FetchUser                  (const loginatdomain:TFRE_DB_String):TFRE_DB_USER;
    function    _FetchUserById              (const user_id:TGUID;var user: TFRE_DB_USER):boolean;
    function    _RoleExists                 (const roleatdomain:TFRE_DB_String):boolean;
    function    _FetchRole                  (const roleatdomain:TFRE_DB_String;var role:TFRE_DB_ROLE):boolean;
    function    _RoleID                     (const roleatdomain:TFRE_DB_String;var role_id:TGUID):boolean;
    function    _FetchRolebyID              (const role_id:TGUID;var role:TFRE_DB_ROLE):boolean;
    function    _GroupExists                (const groupatdomain:TFRE_DB_String):boolean;
    function    _FetchGroup                 (const groupatdomain:TFRE_DB_String;var ug: TFRE_DB_GROUP):boolean;
    function    _GroupID                    (const groupatdomain:TFRE_DB_String;var group_id:TGUID):boolean;
    function    _FetchGroupbyID             (const group_id:TGUID;var ug: TFRE_DB_GROUP):boolean;
    function    _ModifyUserGroups           (const loginatdomain:TFRE_DB_String;const user_groups:TFRE_DB_StringArray; const keep_existing_groups:boolean=false):TFRE_DB_Errortype;
    function    _RemoveUserGroups           (const loginatdomain:TFRE_DB_String;const user_groups:TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    _ModifyGroupRoles           (const groupatdomain:TFRE_DB_String;const roles:TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    _AddGroupRoles              (const groupatdomain:TFRE_DB_String;const roles: TFRE_DB_StringArray): TFRE_DB_Errortype;
    function    _RemoveGroupRoles           (const groupatdomain:TFRE_DB_String;const roles: TFRE_DB_StringArray; const ignore_not_set:boolean): TFRE_DB_Errortype;
    function    _ModifyUserPassword         (const loginatdomain,oldpassword,newpassword:TFRE_DB_String):TFRE_DB_Errortype;
    function    _ModifyUserImage            (const loginatdomain:TFRE_DB_String;const imagestream : TFRE_DB_Stream):TFRE_DB_Errortype;

    function    _DeleteGroup                (const groupatdomain:TFRE_DB_String):TFRE_DB_Errortype;
    function    _DeleteRole                 (const roleatdomain:TFRE_DB_String):TFRE_DB_Errortype;

    function    _DomainExists               (const name :TFRE_DB_NameType):boolean;
    function    _DomainID                   (const name :TFRE_DB_NameType):TGUID;
    function    _DeleteDomain               (const name :TFRE_DB_NameType):TFRE_DB_Errortype;
    function    _DeleteDomainbyID           (const domain_id:TGUID):TFRE_DB_Errortype;
    function    _FetchDomain                (const name :TFRE_DB_NameType; var domain:TFRE_DB_DOMAIN):boolean;
    function    _FetchDomainbyID            (const domain_id:TGUID;var domain:TFRE_DB_DOMAIN):boolean;

    function    _NewText                    (const key,txt,txt_short:TFRE_DB_String):TFRE_DB_TEXT;
    function    _NewRight                   (const rightname,txt,txt_short:TFRE_DB_String):TFRE_DB_RIGHT;
    function    _NewRole                    (const rolename,txt,txt_short:TFRE_DB_String):TFRE_DB_ROLE;
    function    _NewGroup                   (const groupname,txt,txt_short:TFRE_DB_String):TFRE_DB_GROUP;
    function    _NewAppData                 (const appname,txt,txt_short:TFRE_DB_String):TFRE_DB_APPDATA;
    function    _NewDomain                  (const domainname,txt,txt_short:TFRE_DB_NameType):TFRE_DB_DOMAIN;

    function    _AddUser                    (const loginatdomain,password,first_name,last_name:TFRE_DB_String):TFRE_DB_Errortype;
    function    _DeleteUser                 (const loginatdomain:TFRE_DB_String):TFRE_DB_Errortype;
    function    _DeleteUserById             (const user_id:TGuid):TFRE_DB_Errortype;
    function    _CheckRight                 (const right_name:TFRE_DB_String):boolean;
    function    _CheckLogin                 (const loginatdomain,pass:TFRE_DB_String):TFRE_DB_Errortype;
    function    _AddDomain                  (const domainname: TFRE_DB_NameType; const txt,txt_short:TFRE_DB_String):TFRE_DB_Errortype;

    function    IFRE_DB_SYS_CONNECTION.CheckLogin                  = _CheckLogin;
    function    IFRE_DB_SYS_CONNECTION.FetchUser                   = FetchUserI;
    function    IFRE_DB_SYS_CONNECTION.FetchUserById               = FetchUserByIdI;
    function    IFRE_DB_SYS_CONNECTION.FetchGroup                  = FetchGroupI;
    function    IFRE_DB_SYS_CONNECTION.FetchGroupById              = FetchGroupByIdI;
    function    IFRE_DB_SYS_CONNECTION.FetchRoleById               = FetchRoleByIdI;
    function    IFRE_DB_SYS_CONNECTION.NewRight                    = NewRightI;
    function    IFRE_DB_SYS_CONNECTION.StoreGroup                  = StoreGroupI;
    function    IFRE_DB_SYS_CONNECTION.StoreRole                   = StoreRoleI;
    function    IFRE_DB_SYS_CONNECTION.StoreAppData                = StoreAppDataI;
    function    IFRE_DB_SYS_CONNECTION.StoreTranslateableText      = StoreTranslateableTextI;
    function    IFRE_DB_SYS_CONNECTION.UpdateAppData               = UpdateAppDataI;
    function    IFRE_DB_SYS_CONNECTION.FetchAppData                = FetchAppDataI;
    function    IFRE_DB_SYS_CONNECTION.NewRole                     = NewRoleI;
    function    IFRE_DB_SYS_CONNECTION.NewGroup                    = NewGroupI;
    function    IFRE_DB_SYS_CONNECTION.NewAppData                  = NewAppDataI;
    function    IFRE_DB_SYS_CONNECTION.FetchApplications           = FetchApplicationsI;
    function    IFRE_DB_SYS_CONNECTION.GetAllClientFieldValidators = GetAllClientFieldValidatorsI;
    function    IFRE_DB_SYS_CONNECTION.StoreEnum                   = StoreEnumI;
    //function    IFRE_DB_SYS_CONNECTION.GetSchemeCollection         = GetSchemeCollectionI;
    function    IFRE_DB_SYS_CONNECTION.AssociateObject             = AssociateObjectI;
    function    IFRE_DB_SYS_CONNECTION.FetchTranslateableText      = FetchTranslateableTextI;
    function    IFRE_DB_SYS_CONNECTION.FetchRole                   = FetchRoleI;
    function    IFRE_DB_SYS_CONNECTION.ForAllDomains               = ForAllDomainsI;

    function    NewRightI                    (const rightname,txt,txt_short:TFRE_DB_String;var right : IFRE_DB_RIGHT):TFRE_DB_Errortype;
    function    NewRoleI                     (const rolename,txt,txt_short:TFRE_DB_String;var right_group:IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    NewGroupI                    (const groupname,txt,txt_short:TFRE_DB_String;var user_group:IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    NewAppDataI                  (const appname,txt,txt_short:TFRE_DB_String;var appdata: IFRE_DB_APPDATA):TFRE_DB_Errortype;

    function    StoreRoleI                   (const appname:TFRE_DB_String;const domainname:TFRE_DB_NameType;var   rg:IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    StoreGroupI                  (const appname:TFRE_DB_String;const domainname:TFRE_DB_NameType;var   ug:IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    StoreGroupDomainbyIDI        (const domain_id: TGUID; var group: IFRE_DB_GROUP): TFRE_DB_Errortype;
    function    StoreAppDataI                (var   appdata:IFRE_DB_APPDATA):TFRE_DB_Errortype;
    function    StoreTranslateableTextI      (const txt    :IFRE_DB_TEXT) :TFRE_DB_Errortype;
    function    UpdateAppDataI               (var   appdata:IFRE_DB_APPDATA):TFRE_DB_Errortype;
    function    FetchAppDataI                (const Appname:TFRE_DB_String ;var appdata: IFRE_DB_APPDATA):TFRE_DB_Errortype;
  protected
    function    GetRoleIDArray               (const usergroupids : TFRE_DB_GUIDArray) : TFRE_DB_GUIDArray;
    function    GetRightsArrayForRoles       (const roleids      : TFRE_DB_GUIDArray) : TFRE_DB_StringArray;
    function    GetRightsArrayForGroups      (const usergroupids : TFRE_DB_GUIDArray) : TFRE_DB_StringArray;
    function    CheckRightForGroup           (const right_name:TFRE_DB_String;const group_uid : TGuid) : boolean;
  public
    constructor CreateCloned                (const from:TFRE_DB_SYSTEM_CONNECTION);
    procedure   DumpSystem                  ;override;
    function    GetScheme                   (const scheme_name: TFRE_DB_NameType; var scheme: TFRE_DB_SchemeObject): boolean; override;
    function    Connect                     (const loginatdomain:TFRE_DB_String;const pass:TFRE_DB_String;const rebuild_sys_management:boolean):TFRE_DB_Errortype;
    function    Connect                     (const loginatdomain:TFRE_DB_String='';const password:TFRE_DB_String='') : TFRE_DB_Errortype;

    function    AddUser                     (const loginatdomain,password,first_name,last_name:TFRE_DB_String):TFRE_DB_Errortype;
    function    UserExists                  (const loginatdomain:TFRE_DB_String):boolean;
    function    DeleteUser                  (const loginatdomain:TFRE_DB_String):TFRE_DB_Errortype;
    function    DeleteUserById              (const user_id:TGUID):TFRE_DB_Errortype;
    function    FetchUser                   (const loginatdomain:TFRE_DB_String;var user:TFRE_DB_USER):TFRE_DB_Errortype;
    function    FetchUserI                  (const loginatdomain:TFRE_DB_String;var user:IFRE_DB_USER):TFRE_DB_Errortype;
    function    FetchUserById               (const user_id:TGUID;var user: TFRE_DB_USER):TFRE_DB_Errortype;
    function    FetchUserByIdI              (const user_id:TGUID;var user: IFRE_DB_USER):TFRE_DB_Errortype;
    function    FetchGroup                  (const groupatdomain:TFRE_DB_String;var ug: TFRE_DB_GROUP):TFRE_DB_Errortype;
    function    FetchGroupI                 (const groupatdomain:TFRE_DB_String;var ug: IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    FetchGroupById              (const group_id:TGUID;var ug: TFRE_DB_GROUP):TFRE_DB_Errortype;
    function    FetchGroupByIdI             (const group_id:TGUID;var ug: IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    FetchRole                   (const roleatdomain:TFRE_DB_NameType;var role: TFRE_DB_ROLE):TFRE_DB_Errortype;
    function    FetchRoleI                  (const roleatdomain:TFRE_DB_NameType;var role: IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    FetchRoleById               (const role_id:TGUID;var role: TFRE_DB_ROLE):TFRE_DB_Errortype;
    function    FetchRoleByIdI              (const role_id:TGUID;var role: IFRE_DB_ROLE):TFRE_DB_Errortype;
    procedure   ForAllColls                 (const iterator:TFRE_DB_Coll_Iterator) ;override;
    function    FetchDomainById             (const domain_id:TGUID;var domain: TFRE_DB_DOMAIN):TFRE_DB_Errortype;
    function    FetchDomainByIdI            (const domain_id:TGUID;var domain: IFRE_DB_DOMAIN):TFRE_DB_Errortype;
    function    ModifyDomainById            (const domain_id:TGUID;const domainname: TFRE_DB_NameType; const txt,txt_short:TFRE_DB_String):TFRE_DB_Errortype;
    function    DeleteDomainById            (const domain_id:TGUID):TFRE_DB_Errortype;
    function    AddDomain                   (const domainname:TFRE_DB_NameType;const txt,txt_short:TFRE_DB_String):TFRE_DB_Errortype;
    function    DomainExists                (const domainname:TFRE_DB_NameType):boolean;
    function    DeleteDomain                (const domainname:TFRE_DB_Nametype):TFRE_DB_Errortype;
    function    IsSystemGroup               (const group_id:TGUID):boolean;
    procedure   ForAllDomainsI              (const func:IFRE_DB_Obj_Iterator);


    function    FetchUserSessionData        (var SessionData: IFRE_DB_OBJECT):boolean;
    function    StoreUserSessionData        (var session_data:IFRE_DB_Object):TFRE_DB_Errortype;

    function    NewRight                    (const rightname,txt,txt_short:TFRE_DB_String;var right : TFRE_DB_RIGHT):TFRE_DB_Errortype;
    function    NewRole                     (const rolename,txt,txt_short:TFRE_DB_String;var role:TFRE_DB_ROLE):TFRE_DB_Errortype;
    function    NewGroup                    (const groupname,txt,txt_short:TFRE_DB_String;var user_group:TFRE_DB_GROUP):TFRE_DB_Errortype;
    function    NewAppData                  (const appname,txt,txt_short:TFRE_DB_String;var appdata: TFRE_DB_APPDATA):TFRE_DB_Errortype;
    function    ModifyGroupRoles            (const groupatdomain:TFRE_DB_String;const roles:TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    AddGroupRoles               (const groupatdomain:TFRE_DB_String;const roles: TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    RemoveGroupRoles            (const groupatdomain:TFRE_DB_String;const roles: TFRE_DB_StringArray; const ignore_not_set:boolean): TFRE_DB_Errortype;
    function    ModifyUserGroups            (const loginatdomain:TFRE_DB_String;const user_groups:TFRE_DB_StringArray;const keep_existing_groups:boolean=false):TFRE_DB_Errortype;
    function    RemoveUserGroups            (const loginatdomain:TFRE_DB_String;const user_groups:TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    ModifyUserPassword          (const loginatdomain,oldpassword,newpassword:TFRE_DB_String):TFRE_DB_Errortype;
    function    ModifyUserImage             (const loginatdomain:TFRE_DB_String;const imagestream : TFRE_DB_Stream):TFRE_DB_Errortype;
    function    RoleExists                  (const roleatdomain:TFRE_DB_String):boolean;
    function    GroupExists                 (const groupatdomain:TFRE_DB_String):boolean;
    function    DeleteGroup                 (const groupatdomain:TFRE_DB_String):TFRE_DB_Errortype;
    function    DeleteRole                  (const roleatdomain:TFRE_DB_String):TFRE_DB_Errortype;
    function    StoreRole                   (const appname:TFRE_DB_String;const domainname:TFRE_DB_NameType;var role:TFRE_DB_ROLE):TFRE_DB_Errortype;
    function    StoreGroup                  (const appname:TFRE_DB_String;const domainname:TFRE_DB_NameType;var   ug:TFRE_DB_GROUP):TFRE_DB_Errortype;
    function    StoreGroupDomainbyID        (const domain_id: TGUID; var group: TFRE_DB_GROUP): TFRE_DB_Errortype;
    function    StoreAppData                (var   appdata:TFRE_DB_APPDATA):TFRE_DB_Errortype;
    function    StoreTranslateableText      (var   txt    :TFRE_DB_TEXT) :TFRE_DB_Errortype;
    function    UpdateAppData               (var   appdata:TFRE_DB_APPDATA):TFRE_DB_Errortype;
    function    CheckRight                  (const right_name:TFRE_DB_String):boolean;
    function    RemoveApp                   (const Appname:TFRE_DB_String ):TFRE_DB_Errortype;
    function    GetAppDataID                (const Appname:TFRE_DB_String;var appdata_id :TGuid ):TFRE_DB_Errortype;
    function    FetchAppData                (const Appname:TFRE_DB_String;var appdata: TFRE_DB_APPDATA):TFRE_DB_Errortype;
    function    FetchTranslateableText      (const trans_key:TFRE_DB_String;var ttext:TFRE_DB_TEXT):TFRE_DB_Errortype;
    function    FetchTranslateableTextI     (const translation_key:TFRE_DB_String; var textObj: IFRE_DB_TEXT):Boolean;//don't finalize the object

    function    BackupDatabaseReadable      (const to_stream:TStream;const stream_cb:TFRE_DB_StreamingCallback;const progress : TFRE_DB_PhaseProgressCallback):TFRE_DB_Errortype;override;
    function    RestoreDatabaseReadable     (const from_stream:TStream;const stream_cb:TFRE_DB_StreamingCallback):TFRE_DB_Errortype;override;
  end;


  TFRE_DB_CONNECTION=class(TFRE_DB_BASE_CONNECTION,IFRE_DB_CONNECTION)  // Base
  private
    FSysConnection : TFRE_DB_SYSTEM_CONNECTION;
    function    IFRE_DB_CONNECTION.NewObject                   = NewObjectI;
    function    IFRE_DB_CONNECTION.GetScheme                   = GetSchemeI;
    function    IFRE_DB_CONNECTION.Collection                  = CollectionI;
    function    IFRE_DB_CONNECTION.StoreScheme                 = StoreSchemeI;
    function    IFRE_DB_CONNECTION.Fetch                       = FetchI;
    function    IFRE_DB_CONNECTION.GetClientFieldValidator     = GetClientFieldValidatorI;
    function    IFRE_DB_CONNECTION.GetEnum                     = GetEnumI;
    function    IFRE_DB_CONNECTION.Update                      = UpdateI;
    function    IFRE_DB_CONNECTION.ForAllObjects               = ForAllObjectsI;
    function    IFRE_DB_CONNECTION.ForAllColls                 = ForAllCollsI;
    function    IFRE_DB_CONNECTION.ForAllSchemes               = ForAllSchemesI;
    function    IFRE_DB_CONNECTION.ForAllEnums                 = ForAllEnumsI;
    function    IFRE_DB_CONNECTION.ForAllClientFieldValidators = ForAllClientFieldValidatorsI;
    function    IFRE_DB_CONNECTION.NewScheme                   = NewSchemeI;
    function    IFRE_DB_CONNECTION.FetchApplications           = FetchApplicationsI;
    function    IFRE_DB_CONNECTION.GetAllClientFieldValidators = GetAllClientFieldValidatorsI;
    function    IFRE_DB_CONNECTION.StoreClientFieldValidator   = StoreClientFieldValidatorI;
    function    IFRE_DB_CONNECTION.StoreEnum                   = StoreEnumI;
    function    IFRE_DB_CONNECTION.GetSchemeCollection         = GetSchemeCollectionI;
    function    IFRE_DB_CONNECTION.AssociateObject             = AssociateObjectI;
    function    IFRE_DB_CONNECTION.DerivedCollection           = DerivedCollectionI;
  public
    function    GetDatabaseName           : TFRE_DB_String;
    constructor CreateCloned              (const from:TFRE_DB_CONNECTION);
    function    ImpersonateClone          (const user,pass:TFRE_DB_String;out conn:TFRE_DB_CONNECTION): TFRE_DB_Errortype;

    function    Connect                   (const db,user,pass:TFRE_DB_String;const ProxySysConnection:TFRE_DB_SYSTEM_CONNECTION):TFRE_DB_Errortype;
    function    Connect                   (Const db:TFRE_DB_String;const user:TFRE_DB_String='';const password:TFRE_DB_String='') : TFRE_DB_Errortype;

    function    CheckLogin                (const user,pass:TFRE_DB_String):TFRE_DB_Errortype;
    function    CollectionExists          (const name:TFRE_DB_NameType):boolean;override;
    function    DeleteCollection          (const name:TFRE_DB_NameType):TFRE_DB_Errortype;override;

    function    IntegrityCheck            (const log_function:TFRE_LogMsgN=nil):TFRE_DB_Errortype;

    //function    SchemeExists              (const scheme_name: TFRE_DB_String): boolean; override;
    function    GetScheme                 (const scheme_name: TFRE_DB_NameType; var scheme: TFRE_DB_SchemeObject): boolean; override;
    function    NewObject                 (const Scheme:TFRE_DB_String=''): TFRE_DB_Object; override;
    function    Delete                    (const ouid: TGUID): TFRE_DB_Errortype; override;
    destructor  Destroy                   ;override;

    function    FetchApplications         (var apps : TFRE_DB_APPLICATION_ARRAY):TFRE_DB_Errortype;override;
    function    InvokeMethod              (const class_name,method_name:TFRE_DB_String;const uid_path:TFRE_DB_GUIDArray;const input:IFRE_DB_Object;const session:TFRE_DB_UserSession):IFRE_DB_Object;

    function    CreateschemeUniqueKeyDefinition (const schemeClass,FieldName:TFRE_DB_String ; const FieldType:TFRE_DB_FIELDTYPE):TFRE_DB_Errortype;
    function    DropschemeUniqueKeyDefinition   (const schemeClass,FieldName:TFRE_DB_String ; const FieldType:TFRE_DB_FIELDTYPE):TFRE_DB_Errortype;

    function    AddUser                      (const loginatdomain,password,first_name,last_name:TFRE_DB_String):TFRE_DB_Errortype;
    function    UserExists                   (const loginatdomain:TFRE_DB_String):boolean;
    function    DeleteUser                   (const loginatdomain:TFRE_DB_String):TFRE_DB_Errortype;
    function    DeleteUserById               (const user_id:TGUID):TFRE_DB_Errortype;
    function    FetchUser                    (const loginatdomain:TFRE_DB_String;var user:IFRE_DB_USER):TFRE_DB_Errortype;
    function    FetchUserById                (const user_id:TGuid;var user:IFRE_DB_USER):TFRE_DB_Errortype;
    function    FetchGroup                   (const groupatdomain:TFRE_DB_String;var ug: IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    FetchGroupById               (const group_id:TGUID;var ug: IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    FetchRole                    (const roleatdomain:TFRE_DB_NameType;var role: IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    FetchRoleById                (const role_id:TGUID;var role: IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    NewRight                     (const rightname,txt,txt_short:TFRE_DB_String;var right : IFRE_DB_RIGHT):TFRE_DB_Errortype;
    function    NewRole                      (const rolename,txt,txt_short:TFRE_DB_String;var role:IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    NewGroup                     (const groupname,txt,txt_short:TFRE_DB_String;var group:IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    ModifyGroupRoles             (const groupatdomain:TFRE_DB_String;const roles:TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    AddGroupRoles                (const groupatdomain:TFRE_DB_String; const roles: TFRE_DB_StringArray): TFRE_DB_Errortype;
    function    RemoveGroupRoles             (const groupatdomain:TFRE_DB_String; const roles: TFRE_DB_StringArray; const ignore_not_set:boolean): TFRE_DB_Errortype;
    function    ModifyUserGroups             (const loginatdomain:TFRE_DB_String;const user_groups:TFRE_DB_StringArray; const keep_existing_groups:boolean=false):TFRE_DB_Errortype;
    function    RemoveUserGroups             (const loginatdomain:TFRE_DB_String;const user_groups:TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    ModifyUserPassword           (const loginatdomain,oldpassword,newpassword:TFRE_DB_String):TFRE_DB_Errortype;
    function    RoleExists                   (const roleatdomain:TFRE_DB_String):boolean;
    function    GroupExists                  (const groupatdomain:TFRE_DB_String):boolean;
    function    DeleteGroup                  (const groupatdomain:TFRE_DB_String):TFRE_DB_Errortype;
    function    DeleteRole                   (const roleatdomain:TFRE_DB_String):TFRE_DB_Errortype;
    function    StoreRole                    (const appname:TFRE_DB_String;const domainname:TFRE_DB_NameType; var rg:IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    StoreGroup                   (const appname:TFRE_DB_String;const domainname:TFRE_DB_NameType; var ug:IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    StoreGroupDomainbyID         (const domain_id: TGUID; var group : IFRE_DB_GROUP): TFRE_DB_Errortype;
    function    CheckRight                   (const right_name:TFRE_DB_String):boolean;
    function    AddDomain                    (const domainname:TFRE_DB_NameType; const txt,txt_short:TFRE_DB_String):TFRE_DB_Errortype;
    function    FetchDomainById              (const domain_id:TGUID;var domain: IFRE_DB_DOMAIN):TFRE_DB_Errortype;
    function    DomainId                     (const name :TFRE_DB_NameType):TGUID;
    function    ModifyDomainById             (const domain_id:TGUID; const domainname : TFRE_DB_NameType; const txt,txt_short:TFRE_DB_String):TFRE_DB_Errortype;
    function    DeleteDomainById             (const domain_id:TGUID):TFRE_DB_Errortype;
    function    IsSystemGroup                (const group_id:TGUID):boolean;
    function    CheckRightForGroup           (const right_name:TFRE_DB_String;const group_uid : TGuid) : boolean;

    //Warning Fetching from DB, and then from system can have undesired side effects ...
    function    Fetch                       (const ouid:TGUID;out dbo:TFRE_DB_Object) : boolean; override;
    //Warning Fetching from DB, and then from system can have undesired side effects ...
    function    FetchInternal               (const ouid:TGUID;out dbo:TFRE_DB_Object) : boolean; override;


    function    AdmGetUserCollection        :IFRE_DB_COLLECTION;
    function    AdmGetRoleCollection        :IFRE_DB_COLLECTION;
    function    AdmGetGroupCollection       :IFRE_DB_COLLECTION;
    function    AdmGetDomainCollection      :IFRE_DB_COLLECTION;

    function    BackupDatabaseReadable       (const to_stream:TStream;const stream_cb:TFRE_DB_StreamingCallback;const progress : TFRE_DB_PhaseProgressCallback):TFRE_DB_Errortype;override;
    function    RestoreDatabaseReadable      (const from_stream: TStream; const stream_cb: TFRE_DB_StreamingCallback): TFRE_DB_Errortype; override;

    function    FetchUserSessionData         (var SessionData: IFRE_DB_OBJECT):boolean;
    function    StoreUserSessionData         (var session_data:IFRE_DB_Object):TFRE_DB_Errortype;

    function    FetchTranslateableText       (const trans_key:TFRE_DB_String;var text:IFRE_DB_TEXT):boolean;

    function    GetReferences                (const obj_uid:TGuid;const from:boolean):TFRE_DB_ObjectReferences; override;
    function    GetReferences                (const obj_uid:TGuid;const from:boolean ; const substring_filter : TFRE_DB_String) : TFRE_DB_GUIDArray;
    function    GetReferencesCount           (const obj_uid:TGuid;const from:boolean ; const substring_filter : TFRE_DB_String ; const stop_on_first : boolean=false) : NativeInt;
    procedure   ExpandReferences             (ObjectList : TFRE_DB_GUIDArray ; ObjectsRefers : Boolean ; ref_constraints : TFRE_DB_StringArray ;  var expanded_refs : TFRE_DB_ObjectArray ; fetch_internal : boolean);
    procedure   ExpandReferences             (ObjectList : TFRE_DB_GUIDArray ; ObjectsRefers : Boolean ; ref_constraints : TFRE_DB_StringArray ;  var expanded_refs : TFRE_DB_GUIDArray);


    function    DerivedCollection            (const collection_name: TFRE_DB_NameType;const create_non_existing:boolean=true): TFRE_DB_DERIVED_COLLECTION;
    function    DerivedCollectionI           (const collection_name: TFRE_DB_NameType;const create_non_existing:boolean=true): IFRE_DB_DERIVED_COLLECTION;

    function    NewWorkFlowScheme            (const WF_SchemeName:TFRE_DB_NameType):IFRE_DB_WORKFLOW;
    function    StoreWorkFlowScheme          (const WFS : IFRE_DB_WORKFLOW):TFRE_DB_Errortype;
    function    FetchWorkFlowScheme          (const WF_SchemeName:TFRE_DB_NameType;var WFS:IFRE_DB_WORKFLOW):Boolean;
    function    WorkFlowSchemeExists         (const WF_SchemeName:TFRE_DB_NameType):boolean;
    procedure   ForAllWorkFlowSchemes        (const iterator:IFRE_DB_Workflow_Iterator);
    function    DeleteWorkFlowScheme         (const WF_SchemeName:TFRE_DB_NameType):TFRE_DB_Errortype;

    function    StartNewWorkFlow             (const WF_SchemeName:TFRE_DB_NameType;const WF_UniqueKey:TFRE_DB_String):UInt64;
    function    StartNewWorkFlowRecurring    (const WF_SchemeName:TFRE_DB_NameType;const WF_UniqueKey:TFRE_DB_String;const sec_interval:integer):UInt64;
  end;

  TFRE_RInterfaceImplementor =record
    InterfaceSpec     : ShortString;
    ImplementingClass : TClass;
  end;

  TFRE_RExtensionClass = record
    exclass        : TFRE_DB_OBJECTCLASSEX;
    initialized    : boolean;
  end;

  TFRE_RSysObjects = record
    SysGuid : TGuid;
    Obj     : TFRE_DB_Object;
  end;

  TFRE_DB=class(TObject,IFRE_DB)
  private
    FLocalZone                              : TFRE_DB_String;
    FFormatSettings                         : TFormatSettings;
    FClassArray                             : Array of  TFRE_DB_OBJECTCLASS;
    FExClassArray                           : Array of  TFRE_RExtensionClass;
    FKnownInterfaces                        : Array of  TFRE_RInterfaceImplementor;
    FSystemSchemes                          : TFRE_DB_SCHEME_COLLECTION;
    FSysEnums                               : Array of TFRE_DB_Enum;
    FSysClientFieldValidators               : Array of TFRE_DB_ClientFieldValidator;
    FAppArray                               : TFRE_DB_APPLICATION_ARRAY;
    FSysObjectList                          : ARRAY of TFRE_RSysObjects;
    function        GetFormatSettings       : TFormatSettings;
    function        GetLocalZone            : TFRE_DB_String;
    procedure       SetFormatSettings       (const AValue: TFormatSettings);
    procedure       SetLocalZone            (const AValue: TFRE_DB_String);
    function        NewObjectStreaming      (const ClName: ShortString;const conn:TFRE_DB_BASE_CONNECTION) : TFRE_DB_Object;
    procedure       _InternalLog            (const typ:integer;const level:TFOS_LOG_LEVEL;const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
  protected
    function    NewScheme                   (const Scheme_Name: TFRE_DB_String;const typ : TFRE_DB_SchemeType) : TFRE_DB_SchemeObject;
    procedure   SafeFinalize                (intf : IFRE_DB_BASE);
    function    NewDBCommand                : IFRE_DB_COMMAND;
    function    FetchApplications           (var apps : IFRE_DB_APPLICATION_ARRAY):TFRE_DB_Errortype;
    function    NewObjectIntf               (const InterfaceSpec:ShortString;out Intf;const mediator : TFRE_DB_ObjectEx=nil;const fail_on_non_existent:boolean=true) : Boolean;
    function    NewObjectI                  : IFRE_DB_Object;
    function    CreateFromFileI             (const filename:TFRE_DB_String ; const conn:IFRE_DB_CONNECTION=nil; const recreate_weak_schemes: boolean=false):IFRE_DB_Object;
    function    CreateFromMemoryI           (memory : Pointer      ; const conn:IFRE_DB_CONNECTION=nil; const recreate_weak_schemes: boolean=false):IFRE_DB_Object;
    function    CreateFromStringI           (const AValue:TFRE_DB_String   ; const conn:IFRE_DB_CONNECTION=nil; const recreate_weak_schemes: boolean=false):IFRE_DB_Object;
    function    ConstructObjectArrayI       (const A:Array of IFRE_DB_Object):IFRE_DB_ObjectArray;
    function    ConstructObjectArrayOI      (const A:Array of TFRE_DB_Object):IFRE_DB_ObjectArray;
    function    ConstructObjectArrayIO      (const A:Array of IFRE_DB_Object):TFRE_DB_ObjectArray;
    function    CreateTextI                 (const translation_key:TFRE_DB_String;const short_text:TFRE_DB_String;const long_text:TFRE_DB_String='';const hint_text:TFRE_DB_String=''):IFRE_DB_TEXT;
    function    NewConnectionI              (const direct : boolean = true): IFRE_DB_CONNECTION;
    function    NewSysOnlyConnectionI       : IFRE_DB_SYS_CONNECTION;
    function    DatabaseListI               (const user:TFRE_DB_String='';const pass:TFRE_DB_String=''): IFOS_STRINGS;
    function    SystemSchemeExists          (var obj:TFRE_DB_SchemeObject):boolean;
    function    _NewObject                  (const Scheme: TFRE_DB_String;const fail_on_no_cc:boolean): TFRE_DB_Object;

    function    _NewText                    (const key,txt,txt_short:TFRE_DB_String;const hint:TFRE_DB_String=''):TFRE_DB_TEXT;
    function    _NewRight                   (const rightname,txt,txt_short:TFRE_DB_String):TFRE_DB_RIGHT;
    function    _NewRole                    (const rolename,txt,txt_short:TFRE_DB_String):TFRE_DB_ROLE;
    function    _NewGroup                   (const groupname,txt,txt_short:TFRE_DB_String):TFRE_DB_GROUP;
    function    _NewAppData                 (const appname,txt,txt_short:TFRE_DB_String):TFRE_DB_APPDATA;
    function    _NewDomain                  (const domainname,txt,txt_short:TFRE_DB_String):TFRE_DB_DOMAIN;

    function    NewText                     (const key,txt,txt_short:TFRE_DB_String;const hint:TFRE_DB_String=''):IFRE_DB_TEXT;
    function    NewRight                    (const rightname,txt,txt_short:TFRE_DB_String):IFRE_DB_RIGHT;
    function    NewRole                     (const rolename,txt,txt_short:TFRE_DB_String):IFRE_DB_ROLE;
    function    NewGroup                    (const groupname,txt,txt_short:TFRE_DB_String):IFRE_DB_GROUP;
    function    NewEnum                         (const name: TFRE_DB_String) : IFRE_DB_Enum;
    function    RegisterSysClientFieldValidator (const val : IFRE_DB_ClientFieldValidator):TFRE_DB_Errortype;
    function    RegisterSysEnum                 (const enu : IFRE_DB_Enum):TFRE_DB_Errortype;

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

    procedure   AddSystemObjectToSysList     (const obj:TFRE_DB_Object);
    function    FetchSysObject               (const uid:TGUID;var obj:TFRE_DB_Object):boolean;

    procedure   DBInitializeAllExClasses     (const conn:IFRE_DB_SYS_CONNECTION);
    procedure   DBInitializeAllSystemClasses (const conn:IFRE_DB_SYS_CONNECTION); // not impemented by now (no initializable sys classes, keep count low)

    procedure   Initialize_System_Objects    ;
    procedure   Initialize_Extension_Objects ;
  public
    function    JSONObject2Object      (const json_string:string):IFRE_DB_Object;
    function    DefaultDirectory             : TFRE_DB_String;
    constructor Create                       ;
    destructor  Destroy                      ; override;
    function    GetApps                      : TFRE_DB_APPLICATION_ARRAY;
    function    GetApp                       (name:TFRE_DB_String ; out app  : TFRE_DB_APPLICATION):boolean;
    function    GetSysEnum                   (name:TFRE_DB_String ; out enum : TFRE_DB_Enum):boolean;
    function    GetSysClientFieldValidator   (name:TFRE_DB_String ; out clf  : TFRE_DB_ClientFieldValidator):boolean;

    function    LocalTimeToUTCDB64     (const ADateTime64: TFRE_DB_DateTime64) : TFRE_DB_DateTime64;
    function    UTCToLocalTimeDB64     (const ADateTime64: TFRE_DB_DateTime64) : TFRE_DB_DateTime64;

    property    LocalZone              : TFRE_DB_String read GetLocalZone write SetLocalZone;
//    function    ForceDeleteLocal       (const db:TFRE_DB_String):TFRE_DB_Errortype;

    procedure   RegisterObjectClass        (const ObClass  : TFRE_DB_OBJECTCLASS);
    procedure   RegisterObjectClassEx      (const ObClass  : TFRE_DB_OBJECTCLASSEX);
    procedure   RegisterPrimaryImplementor (const ObClass  : TClass ; const InterfaceSpec : ShortString); //register the primary implementor of an interface;
    function    GetInterfaceClass          (const InterfaceSpec : ShortString ; out ObClass:TClass) : boolean;


    function    NewObject              (const ObjClass:TFRE_DB_OBJECTCLASS) : TFRE_DB_Object;
    function    NewObject              (const ObjClass:TFRE_DB_OBJECTCLASSEX) : TFRE_DB_Object;
    function    NewObject              (const ClName:TFRE_DB_String) : TFRE_DB_Object;
    function    NewObject              : TFRE_DB_Object;
    function    NewNamedObject         : TFRE_DB_NAMED_OBJECT;

    function    NewClientFieldValidator(const name: TFRE_DB_String)       : TFRE_DB_ClientFieldValidator;
    function    NewClientFieldValidatorI(const name: TFRE_DB_String)      : IFRE_DB_ClientFieldValidator;

    function    GetObjectClass         (const ClName:ShortString) : TFRE_DB_OBJECTCLASS;
    function    GetObjectClassEx       (const ClName:ShortString) : TFRE_DB_OBJECTCLASSEX;
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

    function    ConstructGuidArray     (const A:Array of TGuid):TFRE_DB_GUIDArray;
    function    ConstructByteArray     (const A:Array of Byte):TFRE_DB_ByteArray;
    function    ConstructInt16Array    (const A:Array of Int16):TFRE_DB_Int16Array;
    function    ConstructInt32Array    (const A:Array of Int32):TFRE_DB_Int32Array;
    function    ConstructInt64Array    (const A:Array of Int64):TFRE_DB_Int64Array;
    function    ConstructUInt16Array   (const A:Array of UInt16):TFRE_DB_UInt16Array;
    function    ConstructUInt32Array   (const A:Array of UInt32):TFRE_DB_UInt32Array;
    function    ConstructUInt64Array   (const A:Array of UInt64):TFRE_DB_UInt64Array;
    function    ConstructReal32Array   (const A:Array of Single):TFRE_DB_Real32Array;
    function    ConstructReal64Array   (const A:Array of Double):TFRE_DB_Real64Array;
    function    ConstructCurrencyArray (const A:Array of Currency):TFRE_DB_CurrencyArray;
    function    ConstructStringArray   (const A:Array of TFRE_DB_String):TFRE_DB_StringArray;
    function    ConstructBooleanArray  (const A:Array of Boolean):TFRE_DB_BoolArray;
    function    ConstructDateTimeArray (const A:Array of TFRE_DB_DateTime64):TFRE_DB_DateTimeArray;
    function    ConstructStreamArray   (const A:Array of TFRE_DB_Stream):TFRE_DB_StreamArray;
    function    ConstructObjectArray   (const A:Array of TFRE_DB_Object):TFRE_DB_ObjectArray;
    function    TranslateLong          (const txt : TFRE_DB_TEXT):TFRE_DB_String;

    function    GetSystemSchemeColl   : TFRE_DB_SCHEME_COLLECTION;
    function    GetSystemScheme       (const schemename:TFRE_DB_String; var scheme: TFRE_DB_SchemeObject): Boolean;

    procedure   ForAllSchemes                (const iterator:TFRE_DB_Scheme_Iterator)                                 ;
    procedure   ForAllEnums                  (const iterator:TFRE_DB_Enum_Iterator)                                   ;
    procedure   ForAllClientFieldValidators  (const iterator:TFRE_DB_ClientFieldValidator_Iterator)                   ;
    procedure   ForAllApps                   (const iterator:TFRE_DB_Apps_Iterator)                   ;

    procedure   LogDebug               (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogInfo                (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogWarning             (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogError               (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogNotice              (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogDebug               (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogInfo                (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogWarning             (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogError               (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogNotice              (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);

    procedure   ClearGUID              (var uid:TGUID);
    function    Get_A_Guid             : TGUID;
    function    Get_A_Guid_HEX         : Ansistring;

    function    InstallDBDefaults      (const conn: IFRE_DB_SYS_CONNECTION): TFRE_DB_Errortype;
  end;

  OFRE_DB_ConnectionArr  = specialize OGFOS_Array<TFRE_DB_CONNECTION>;

  function  RB_Sort_CompareEx  (const a,b : TFRE_DB_Object ; const DP : Pointer):NativeInt;

  operator = (a,b:TGUID) c:boolean;

  procedure Init4Server;
  procedure InitMinimal(const nosys:boolean=false);

var
  GFRE_DB                  : TFRE_DB;
  GFRE_DB_DEFAULT_PS_LAYER : IFRE_DB_PERSISTANCE_LAYER;
  GDROP_WAL                : boolean;

  procedure GFRE_DB_Init_Check;

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

  function RB_Sort_CompareEx(const a, b: TFRE_DB_Object ; const DP:Pointer): NativeInt;
  begin
    result := TFRE_DB_DERIVED_COLLECTION(DP)._CompareObjects(a,b);
  end;

  operator=(a, b: TGUID)c: boolean;
  begin
    c := FREDB_Guids_Same(a,b);
  end;

  operator=(e1, e2: RFRE_DB_UPDATE_ENTRY)b: boolean;
  begin

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

{ TFRE_DB_DOMAIN }

class procedure TFRE_DB_DOMAIN.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var input_group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.Strict(false);
  Scheme.SetParentSchemeByName(TFRE_DB_NAMED_OBJECT.ClassName);
  scheme.GetSchemeField('objname').required:=true;
  Scheme.SetSysDisplayField(GFRE_DB.ConstructStringArray(['objname','$DBTEXT:desc']),'%s - (%s)');

  input_group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_DOMAIN_group');
  input_group.AddInput('objname','$scheme_TFRE_DB_DOMAIN_name');
  input_group.AddInputSubGroup('TFRE_DB_TEXT','main','desc',true,false);

end;

class function TFRE_DB_DOMAIN.IMC_NewDomainOperation(const input: IFRE_DB_Object): IFRE_DB_Object;
var dbo              : IFRE_DB_Object;
    data             : IFRE_DB_Object;
    res              : TFRE_DB_Errortype;
    dbo_uid          : TGUID;
    dbc              : TFRE_DB_CONNECTION;
    dn               : TFRE_DB_NameType;
    txt              : TFRE_DB_String;
    txt_s            : TFRE_DB_String;

    procedure _InstallSystemGroupsandRoles;
    var apps : IFRE_DB_APPLICATION_ARRAY;
           i : Integer;
         dbs : IFRE_DB_CONNECTION;
    begin
      GFRE_DBI.FetchApplications(apps);
      for i:=0 to high(apps) do begin
        if apps[i].ObjectName<>'LOGIN' then begin
          (apps[i].Implementor_HC as TFRE_DB_APPLICATION).InstallSystemGroupsandRoles(dbc.FSysConnection,dn);
        end;
      end;
    end;

begin
 data    := input.Field('DATA').asobject;
 dbc     := input.GetReference as TFRE_DB_CONNECTION;

 dn      := data.Field('objname').AsString;
 txt     := data.FieldPath('desc.txt').AsString;
 txt_s   := data.FieldPath('desc.txt_s').AsString;
 res := dbc.AddDomain(dn,txt,txt_s);
 _InstallSystemGroupsandRoles;

 if res=edb_OK then
   exit(TFRE_DB_CLOSE_DIALOG_DESC.create.Describe())
 else
   exit(TFRE_DB_MESSAGE_DESC.create.Describe('TRANSLATE: ERRORK','TRANSLATE: Creation failed '+CFRE_DB_Errortype[res],fdbmt_error,nil));
end;



{ TFRE_DB_RIGHT }

class procedure TFRE_DB_RIGHT.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.Strict(true);
  Scheme.SetParentSchemeByName(TFRE_DB_NAMED_OBJECT.ClassName);
  Scheme.SetSysDisplayField(GFRE_DB.ConstructStringArray(['objname','$DBTEXT:desc']),'%s - (%s)');
end;

{ TFRE_DB_APPDATA }

procedure TFRE_DB_APPDATA.SetVersion(AValue: TFRE_DB_String);
begin
 Field('version').asstring:=AValue;
end;

function TFRE_DB_APPDATA.GetVersion: TFRE_DB_String;
begin
 result := Field('version').asstring;
end;

class procedure TFRE_DB_APPDATA.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.Strict(true);
  Scheme.SetParentSchemeByName(TFRE_DB_NAMED_OBJECT.ClassName);
  scheme.AddSchemeField('version',fdbft_String).SetupFieldDef(true,false);
  Scheme.SetSysDisplayField(GFRE_DB.ConstructStringArray(['objname','$DBTEXT:desc']),'%s - (%s)');
end;

procedure TFRE_DB_APPDATA.SetVersionI(AValue: TFRE_DB_String);
begin
 SetVersion(AValue);
end;

function TFRE_DB_APPDATA.GetVersionI: TFRE_DB_String;
begin
 result := GetVersion;
end;

{ TFRE_DB_CHART_TRANSFORM }

function TFRE_DB_CHART_TRANSFORM.TransformInOut(const dependency_obj: IFRE_DB_Object; const input: TFRE_DB_Object; const filter_fields: boolean): TFRE_DB_Object;
begin
  Result := input.CloneToNewObject;
end;

destructor TFRE_DB_CHART_TRANSFORM.Destroy;
begin
  inherited Destroy;
end;

{ TFRE_DB_TREE_TRANSFORM }

function TFRE_DB_TREE_TRANSFORM.TransformInOut(const dependency_obj: IFRE_DB_Object; const input: TFRE_DB_Object; const filter_fields: boolean): TFRE_DB_Object;
begin
  Result := input.CloneToNewObject;
end;

{ TFRE_DB_WORKFLOW_STEP }

function TFRE_DB_WORKFLOW_STEP.GetStepName: TFRE_DB_NameType;
begin
  result := FStepname.AsString;
end;

procedure TFRE_DB_WORKFLOW_STEP.SetStepName(AValue: TFRE_DB_NameType);
begin
  FStepname.AsString := AValue;
end;

function TFRE_DB_WORKFLOW_STEP._ObjectsNeedsNoSubfieldSchemeCheck: boolean;
begin
  Result := true;
end;

function TFRE_DB_WORKFLOW_STEP._ObjectIsCodeclassOnlyAndHasNoScheme: boolean;
begin
  Result := true;
end;

procedure TFRE_DB_WORKFLOW_STEP.SetStepMethod(const SchemeName, WFM_MethodName: TFRE_DB_String);
begin

end;

{ TFRE_DB_WORKFLOW }

function TFRE_DB_WORKFLOW._ObjectsNeedsNoSubfieldSchemeCheck: boolean;
begin
  Result := true;
end;

function TFRE_DB_WORKFLOW.GetRecurring: Boolean;
begin

end;

function TFRE_DB_WORKFLOW.GetCurrentStep: TFRE_DB_NameType;
begin

end;

function TFRE_DB_WORKFLOW.GetEndTime: TFRE_DB_DateTime64;
begin

end;

function TFRE_DB_WORKFLOW.GetNextScheduledInvocation: TFRE_DB_DateTime64;
begin

end;

function TFRE_DB_WORKFLOW.GetStartTime: TFRE_DB_DateTime64;
begin

end;

function TFRE_DB_WORKFLOW.GetUserLogin: TFRE_DB_NameType;
begin

end;

procedure TFRE_DB_WORKFLOW.SetRecurring(AValue: Boolean);
begin

end;

procedure TFRE_DB_WORKFLOW.SetStartTime(AValue: TFRE_DB_DateTime64);
begin

end;

function TFRE_DB_WORKFLOW._ObjectIsCodeclassOnlyAndHasNoScheme: boolean;
begin
  Result := true;
end;

procedure TFRE_DB_WORKFLOW.InternalSetup;
begin
  inherited InternalSetup;
  FWorkflowData   := _Field('WD');
  FWorkflowProg   := _Field('WP');
  FIsInstance     := _Field('II');
  FInstanceNumber := _Field('IN');
end;

procedure TFRE_DB_WORKFLOW.ClearWorkFlow;
begin

end;

procedure TFRE_DB_WORKFLOW.AddWorkFlowStep(const STEP_NAME: TFRE_DB_NameType; const STEP: TFRE_DB_WORKFLOW_STEP);
begin

end;

procedure TFRE_DB_WORKFLOW.AddSubWorkAsStep(const STEP_NAME: TFRE_DB_NameType; const WFNAME: TFRE_DB_NameType);
begin

end;

procedure TFRE_DB_WORKFLOW.DumpWorkFlow;
begin

end;

function TFRE_DB_WORKFLOW.WorkFlowData: IFRE_DB_Object;
begin

end;

procedure TFRE_DB_WORKFLOW.StopWorkFlow(const reason: TFRE_DB_String);
begin

end;

function TFRE_DB_WORKFLOW.WorkFlowState: TFRE_DB_WORKFLOW_STATE;
begin

end;



//constructor TFRE_DB_Collection_ManageInfo.Create(const collection: TFRE_DB_COLLECTION; collection_name: TFRE_DB_NameType; const connection: TFRE_DB_BASE_CONNECTION; const is_master: boolean; const is_memory_only: boolean);
//begin
//  if is_memory_only and is_master then raise EFRE_DB_Exception.Create(edb_INTERNAL,'non persistant master collection? - internal fault');
//  collection_link := collection;
//  Fis_master      := is_master;
//  Fis_memory_only := is_memory_only;
//  FKey            := collection_name;
//  FUP_key         := UpperCase(collection_name);
//  FConnection     := connection;
//  if assigned(collection) then begin
//    collection.FManageInfo.CollManageInfo := self; // upgrade manageinfo
//  end;
//end;

//procedure TFRE_DB_Collection_ManageInfo.InstanceIt;
//var result : TFRE_DB_Errortype;
//begin
//  if collection_link=nil then begin
//    result := FConnection.FPersistance_Layer.RetrieveCollection(fkey,collection_link,self);
//    collection_link.FManageInfo.CollManageInfo := self; // upgrade connection to managementinfo;
//    assert(collection_link.FDBO_State=fdbos_Clean);
//    //assert(collection_link.FStreamingSize>0);
//    if result<>edb_OK then raise EFRE_DB_Exception.Create(result,'instancing error / persistance layer');
//  end;
//end;

//function TFRE_DB_Collection_ManageInfo.IsMaster: boolean;
//begin
//  result := Fis_master;
//end;

//function TFRE_DB_Collection_ManageInfo.IsMemoryOnly: boolean;
//begin
//  result := Fis_memory_only;
//end;

//function TFRE_DB_Collection_ManageInfo.GetConnection: TFRE_DB_BASE_CONNECTION;
//begin
//  result := FConnection;
//end;

//function TFRE_DB_Collection_ManageInfo.Key: TFRE_DB_NameType;
//begin
//  result := Fup_Key;
//end;

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

procedure TFRE_DB_Enum.addEntry(const value: TFRE_DB_String; const caption: TFRE_DB_TEXT);
var obj: TFRE_DB_Object;
begin
  obj:=GFRE_DB.NewObject;
  obj._Field('v').AsString:=uppercase(value);
  obj._Field('c').AsObject:=caption.Implementor as TFRE_DB_Object;
  _Field('e').AddObject(obj);
end;

procedure TFRE_DB_Enum.addEntryI(const value: TFRE_DB_String; const caption: IFRE_DB_TEXT);
begin
  addEntry(value,caption.Implementor as TFRE_DB_TEXT);
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

{ TFRE_DB_ClientFieldValidator }

function TFRE_DB_ClientFieldValidator._ObjectIsCodeclassOnlyAndHasNoScheme: boolean;
begin
  Result:=true;
end;

function TFRE_DB_ClientFieldValidator.Setup(const regExp: TFRE_DB_String; const infoText: IFRE_DB_TEXT; const helpText: IFRE_DB_TEXT; const allowedChars: TFRE_DB_String): IFRE_DB_ClientFieldValidator;
begin
  Field('regExp').AsString:=regExp;
  Field('allowedChars').AsString := allowedChars;
  Field('helpText').AsObject  := helpText.Implementor as TFRE_DB_Object;
  Description                 := infoText.Implementor as TFRE_DB_TEXT;
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

function TFRE_DB_ClientFieldValidator.getHelpText: TFRE_DB_TEXT;
begin
 Result:=Field('helpText').AsObject as TFRE_DB_TEXT
end;

function TFRE_DB_ClientFieldValidator.getHelpTextI: IFRE_DB_TEXT;
begin
 result := getHelpText;
end;

function TFRE_DB_ClientFieldValidator.getAllowedChars: TFRE_DB_String;
begin
  Result:=Field('allowedChars').AsString;
end;

//procedure TFRE_DB_ClientFieldValidator.setConfigParamsI(const params: IFRE_DB_Object);
//begin
//  setConfigParams(params.Implementor as TFRE_DB_Object);
//end;
//
//procedure TFRE_DB_ClientFieldValidator.setConfigParams(const params: TFRE_DB_Object);
//begin
//  Field('params').AsObject:=params;
//end;
//
//function TFRE_DB_ClientFieldValidator.getConfigParamsI: IFRE_DB_Object;
//begin
//  Result:=getConfigParams;
//end;

//function TFRE_DB_ClientFieldValidator.getConfigParams: TFRE_DB_Object;
//begin
//  Result:=nil;
//  if FieldExists('params') then begin
//    Result:=Field('params').AsObject;
//  end;
//end;

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

function TFRE_DB_InputGroupSchemeDefinition.GetCaptionKey: TFRE_DB_String;
begin
  result := Field('cap_key').AsString;
end;

function TFRE_DB_InputGroupSchemeDefinition.GetIGFields: IFRE_DB_ObjectArray;
begin
 result := FieldI('fields').AsObjectArr;
end;

function TFRE_DB_InputGroupSchemeDefinition.GetInputGroupID: TFRE_DB_String;
begin
  result := Field('igid').AsString;
end;

procedure TFRE_DB_InputGroupSchemeDefinition.SetCaptionKey(AValue: TFRE_DB_String);
begin
  Field('cap_key').AsString := AValue;
end;

procedure TFRE_DB_InputGroupSchemeDefinition.SetIGFields(AValue: IFRE_DB_ObjectArray);
begin
  FieldI('fields').AsObjectArr := AValue;
end;

procedure TFRE_DB_InputGroupSchemeDefinition.SetInputGroupID(AValue: TFRE_DB_String);
begin
 Field('igid').AsString := AValue;
end;

//procedure TFRE_DB_InputGroupSchemeDefinition.AddInputI(const schemefield: TFRE_DB_String; const caption: IFRE_DB_TEXT; const disabled: Boolean; const hidden: Boolean; const dataCollection: TFRE_DB_String);
//begin
//  AddInput(schemefield,caption.Implementor as TFRE_DB_TEXT,disabled,hidden,dataCollection);
//end;

function TFRE_DB_InputGroupSchemeDefinition.GetSchemeFromDBI(const schemename: TFRE_DB_String; var scheme: IFRE_DB_SchemeObject): Boolean;
var sch :TFRE_DB_SchemeObject;
begin
  result := GetSchemeFromDB(schemename,sch);
  if Result then begin
    scheme := sch;
  end else begin
    scheme := nil;
  end;
end;

function TFRE_DB_InputGroupSchemeDefinition.SetupI(const caption: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;
begin
  result := Setup(caption);
end;

function TFRE_DB_InputGroupSchemeDefinition.GetParentScheme: TFRE_DB_SchemeObject;
begin
  Result:=Parent.Parent as TFRE_DB_SchemeObject;
end;

procedure TFRE_DB_InputGroupSchemeDefinition.AddInput(const schemefield: TFRE_DB_String; const cap_trans_key: TFRE_DB_String; const disabled: Boolean; const hidden: Boolean; const dataCollection: TFRE_DB_String);
var
  obj: TFRE_DB_Object;
  scheme: TFRE_DB_SchemeObject;
  path: TFOSStringArray;
  i: Integer;
  fieldDef: TFRE_DB_FieldSchemeDefinition;
  required: Boolean;
  validator: TFRE_DB_ClientFieldValidator;
  enum     : TFRE_DB_Enum;
begin
  scheme:=GetParentScheme;
  GFRE_BT.SeperateString(schemefield,'.',path);

  required:=true;
  if Length(path)>1 then begin
    for i := 0 to Length(path) - 2 do begin
      if not scheme.GetSchemeField(path[i],fieldDef) then raise EFRE_DB_Exception.Create(edb_ERROR,'cannot find scheme field: '+path[i]);
      required:=required and fieldDef.required;
      GetParentScheme.GetSchemeFromDB(fieldDef._Field('sub').AsString,scheme);
    end;
  end;

  if not scheme.GetSchemeField(path[High(path)],fieldDef) then raise EFRE_DB_Exception.Create(edb_ERROR,'cannot find scheme field: %s:(%s)',[scheme.DefinedSchemeName,schemefield]);

  obj:=GFRE_DB.NewObject;
  obj.Field('field').AsString           := schemefield;
  obj.Field('type').AsString            := CFRE_DB_FIELDTYPE_SHORT[fieldDef.FieldType];
  obj.Field('caption_key').AsString     := cap_trans_key;
  obj.Field('required').AsBoolean       := required and fieldDef.required;
  obj.Field('isPass').AsBoolean         := fieldDef.isPass;
  obj.Field('addConfirm').AsBoolean     := fieldDef.addConfirm;
  obj.Field('multiValues').AsBoolean    := fieldDef.multiValues;
  obj.Field('disabled').AsBoolean       := disabled;
  obj.Field('hidden').AsBoolean         := hidden;
  enum:=fieldDef.getEnum;
  if Assigned(enum) then begin
    obj.Field('enum').AsString          := fieldDef.getEnum.ObjectName;
  end;
  validator:=fieldDef.getValidator;
  if Assigned(validator) then begin
    obj.Field('vtype').AsString         := validator.ObjectName;
    if fieldDef.FvalidatorParams.IsObjectField then begin
      obj.Field('vtypeparams').AsObject := fieldDef.FvalidatorParams.AsObject.CloneToNewObject(true);
    end;
  end;
  obj.Field('dataCollection').AsString := dataCollection;
  if fieldDef.FieldType=fdbft_Boolean then begin
    for i := 0 to Length(fieldDef.getDepFields) - 1 do begin
      obj.Field('depFields').AddObject(fieldDef.getDepFields[i].CloneToNewObject(true));
    end;
  end;
  Field('fields').AddObject(obj);
end;

procedure TFRE_DB_InputGroupSchemeDefinition.UseInputGroup(const scheme,group:TFRE_DB_String; const addPrefix: TFRE_DB_String);
var
  obj: TFRE_DB_Object;
begin
  obj:=GFRE_DB.NewObject;
  obj.Field('scheme').AsString:=scheme;
  obj.Field('group').AsString:=group;
  obj.Field('prefix').AsString:=addPrefix;
  obj.Field('asSubGroup').AsBoolean:=false;
  Field('fields').AddObject(obj);
end;

procedure TFRE_DB_InputGroupSchemeDefinition.AddInputSubGroup(const scheme, group: TFRE_DB_String; const addPrefix: TFRE_DB_String; const collapsible: Boolean; const collapsed: Boolean);
var
  obj: TFRE_DB_Object;
begin
  obj:=GFRE_DB.NewObject;
  obj.Field('scheme').AsString:=scheme;
  obj.Field('group').AsString:=group;
  obj.Field('prefix').AsString:=addPrefix;
  obj.Field('asSubGroup').AsBoolean:=true;
  obj.Field('collapsible').AsBoolean:=collapsible;
  obj.Field('collapsed').AsBoolean:=collapsed;
  Field('fields').AddObject(obj);
end;

function TFRE_DB_InputGroupSchemeDefinition.GetParentSchemeI: IFRE_DB_SchemeObject;
begin
  result := GetParentScheme;
end;

function TFRE_DB_InputGroupSchemeDefinition.GetSchemeFromDB(const schemename: TFRE_DB_String; var scheme: TFRE_DB_SchemeObject): Boolean;
begin
  Result:=GetParentScheme.GetSchemeFromDB(schemename,scheme);
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
  inherited CopyToMemory(memory,false);
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

//procedure TFRE_DB_COMMAND.SetIfCmd(AValue: Boolean);
//begin
//  FIsInterfaceC.AsBoolean := AValue;
//end;

function TFRE_DB_COMMAND.GetUidPath: TFRE_DB_GUIDArray;
begin
  result := FIUidPath.AsGUIDArr;
end;

//function TFRE_DB_COMMAND.GetIfCmd: Boolean;
//begin
//  result := FIsInterfaceC.AsBoolean;
//end;

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
  Scheme.SetSysDisplayField(GFRE_DB.ConstructStringArray(['objname','$DBTEXT:desc']),'%s - (%s)');
  Scheme.Strict(true);
end;

function TFRE_DB_GROUP.GetDomain: TFRE_DB_NameType;
var syscon       : TFRE_DB_SYSTEM_CONNECTION;
    lDomain      : TFRE_DB_DOMAIN;
begin
  syscon   := _DBConnectionBC as TFRE_DB_SYSTEM_CONNECTION;
  if not syscon._FetchDomainbyID(DomainID,lDomain) then begin
    raise EFRE_DB_Exception.Create('Could not fetch domain by id '+GFRE_BT.GUID_2_HexString(DomainID));
  end else begin
    result := lDomain.GetName;
  end;
end;

function TFRE_DB_GROUP.GetDomainID: TGUID;
begin
  result := Field('domainid').AsObjectLink;
end;

function TFRE_DB_GROUP.GetFullname: TFRE_DB_String;
begin
  result := ObjectName+'@'+Domain;
end;

function TFRE_DB_GROUP.GetRoleNames: TFRE_DB_StringArray;
var syscon       : TFRE_DB_SYSTEM_CONNECTION;
    i            : integer;
    lRoleIDs     : TFRE_DB_ObjLinkArray;
    lRole        : TFRE_DB_Role;
begin
  syscon   := _DBConnectionBC as TFRE_DB_SYSTEM_CONNECTION;
  lRoleIDs := RoleIDs;
  SetLength(result,Length(lRoleIDs));
  for i  := 0 to high(lRoleIDs) do begin
    if not syscon._FetchRolebyID(lRoleids[i],lRole) then begin
      raise EFRE_DB_Exception.Create('Could not fetch role by id '+GFRE_BT.GUID_2_HexString(lRoleids[i]));
    end else begin
      result[i] := lRole.Fullname;
    end;
  end;
end;

function TFRE_DB_GROUP.GetRoleIDs: TFRE_DB_ObjLinkArray;
begin
  result := Field('roleids').AsObjectLinkArray;
end;

procedure TFRE_DB_GROUP.SetDomainID(AValue: TGUID);
begin
  Field('domainid').AsObjectLink := AValue;
  _UpdateDomainGroupKey;
end;

procedure TFRE_DB_GROUP.SetDomain(const domainname: TFRE_DB_NameType);
var syscon       : TFRE_DB_SYSTEM_CONNECTION;
    l_domainid   : TGUID;
begin
  syscon     := _DBConnectionBC as TFRE_DB_SYSTEM_CONNECTION;
  l_domainid := syscon._DomainID(domainname);
  if l_domainid=GUID_NULL then begin
    raise EFRE_DB_Exception.Create('Could not fetch domain by name '+domainname);
  end else begin
    SetDomainID(l_domainid);
  end;
end;


procedure TFRE_DB_GROUP.SetRoleIDs(AValue: TFRE_DB_ObjLinkArray);
begin
  Field('roleids').AsObjectLinkArray := AValue;
end;

procedure TFRE_DB_GROUP._UpdateDomainGroupKey;
begin
  field('domaingroupkey').AsString := GetDomainGroupKey(ObjectName,domainid);
end;

class procedure TFRE_DB_GROUP.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var input_group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.Strict(false);
  Scheme.SetParentSchemeByName(TFRE_DB_NAMED_OBJECT.ClassName);
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddSchemeField('roleids',fdbft_ObjLink).SetupFieldDef(false,true);
  scheme.AddSchemeField('appdataid',fdbft_ObjLink).SetupFieldDef(false,true);
  scheme.AddSchemeField('domainid',fdbft_ObjLink).SetupFieldDef(true,false);
  scheme.AddSchemeField('domaingroupkey',fdbft_String).SetupFieldDef(true,false);
  Scheme.SetSysDisplayField(GFRE_DB.ConstructStringArray(['objname','$DBTEXT:desc']),'%s - (%s)');

  input_group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_GROUP_group_group');
  input_group.AddInput('objname','$scheme_TFRE_DB_GROUP_name');
  input_group.AddInputSubGroup('TFRE_DB_TEXT','main','desc',true,false);
  input_group:=scheme.AddInputGroup('domain').Setup('$scheme_TFRE_DB_GROUP_group_domain');
  input_group.AddInput('domainid','$scheme_TFRE_DB_GROUP_domainid',false,false,'$SDC:GROUPMOD_DOMAINS'); // HACK: Fix Domain Name with session prefix
end;

function TFRE_DB_GROUP.AddUserToGroupI(const user: IFRE_DB_USER): TFRE_DB_Errortype;
begin
  result := AddUserToGroup(user.implementor as TFRE_DB_USER);
end;

function TFRE_DB_GROUP.RemoveUserFromGroupI(const user: IFRE_DB_USER): TFRE_DB_Errortype;
begin
 result := RemoveUserFromGroup(user.implementor as TFRE_DB_USER);
end;

function TFRE_DB_GROUP.AddUserToGroup(const user: TFRE_DB_USER): TFRE_DB_Errortype;
begin
  writeln('TODO : ADD U TO UG ',user.GetFormattedDisplay);
  //abort;
end;

function TFRE_DB_GROUP.RemoveUserFromGroup(const user: TFRE_DB_USER): TFRE_DB_Errortype;
begin
 writeln('TODO : REMOVE U FROM UG ',user.GetFormattedDisplay);
 //abort;
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

class function TFRE_DB_GROUP.GetDomainGroupKey(const grouppart: TFRE_DB_String; const domain_id: TGUID): TFRE_DB_String;
begin
  result := GFRE_BT.GUID_2_HexString(domain_id)+'@'+lowercase(grouppart);
end;

class function TFRE_DB_GROUP.IMC_NewGroupOperation(const input: IFRE_DB_Object): IFRE_DB_Object;
var data             : IFRE_DB_Object;
    res              : TFRE_DB_Errortype;
    dbc              : TFRE_DB_CONNECTION;
    domain_id        : TGUID;
    groupname        : TFRE_DB_NameType;
    txt              : TFRE_DB_String;
    txt_s            : TFRE_DB_String;
    group            : IFRE_DB_GROUP;

begin
 writeln('------NEW GROUP');
 writeln(input.DumpToString);
 data    := input.Field('DATA').asobject;
 dbc     := input.GetReference as TFRE_DB_CONNECTION;

 groupname := data.Field('objname').AsString;
 txt       := data.FieldPath('desc.txt').AsString;
 txt_s     := data.FieldPath('desc.txt_s').AsString;

 if txt_s='' then txt_s:=groupname;
 if txt='' then txt:=groupname;

 domain_id := GFRE_BT.HexString_2_GUID(data.Field('domainid').AsString);
 res := dbc.NewGroup(groupname,txt,txt_s,group);
 if res <> edb_OK then
   exit(TFRE_DB_MESSAGE_DESC.create.Describe('TRANSLATE: ERROR','TRANSLATE: Creation of group failed '+CFRE_DB_Errortype[res],fdbmt_error,nil));

 res := dbc.StoreGroupDomainbyID(domain_id,group);
 if res=edb_OK then
   exit(TFRE_DB_CLOSE_DIALOG_DESC.create.Describe())
 else
   exit(TFRE_DB_MESSAGE_DESC.create.Describe('TRANSLATE: ERROR','TRANSLATE: Storing failed '+CFRE_DB_Errortype[res],fdbmt_error,nil));
end;

function TFRE_DB_GROUP.IMI_SAVEOPERATION(const input: IFRE_DB_Object): IFRE_DB_Object;
var data             : IFRE_DB_Object;
    res              : TFRE_DB_Errortype;
    dbc              : IFRE_DB_CONNECTION;
    domain_id        : TGUID;

begin
 writeln('------MODIFY GROUP');
 writeln(input.DumpToString);
 dbc := GetSession(input).GetDBConnection;
 data    := input.Field('DATA').asobject;
 //dbc     := GetDBConnection;

 ObjectName:= data.Field('objname').AsString;
 Description.ShortText:= data.FieldPath('desc.txt_s').AsString;
 Description.LongText:= data.FieldPath('desc.txt').AsString;

// domain_id := GFRE_BT.HexString_2_GUID(data.Field('domainid').AsString);

 res       := dbc.Update(self);
 if res <> edb_OK then
   exit(TFRE_DB_MESSAGE_DESC.create.Describe('TRANSLATE: ERROR','TRANSLATE: Modification of group failed '+CFRE_DB_Errortype[res],fdbmt_error,nil))
 else
   exit(TFRE_DB_CLOSE_DIALOG_DESC.create.Describe())
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

  input_group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_TEXT_descr_group');
  input_group.AddInput('txt','$scheme_TFRE_DB_TEXT_txt');
  input_group.AddInput('txt_s','$scheme_TFRE_DB_TEXT_txt_s');
  input_group:=scheme.AddInputGroup('main_full').Setup('$scheme_TFRE_DB_TEXT_descr_group');
  input_group.AddInput('txt','$scheme_TFRE_DB_TEXT_txt');
  input_group.AddInput('txt_s','$scheme_TFRE_DB_TEXT_txt_s');
  input_group.AddInput('hnt','$scheme_TFRE_DB_TEXT_txt_s');
  input_group:=scheme.AddInputGroup('main_all').Setup('$scheme_TFRE_DB_TEXT_descr_group');
  input_group.AddInput('txt','$scheme_TFRE_DB_TEXT_txt');
  input_group.AddInput('txt_s','$scheme_TFRE_DB_TEXT_txt_s');
  input_group.AddInput('hnt','$scheme_TFRE_DB_TEXT_txt_s');
  input_group.AddInput('t_key','$scheme_TFRE_DB_TEXT_key');
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

function TFRE_DB_ROLE.GetDomain: TFRE_DB_NameType;
var syscon       : TFRE_DB_SYSTEM_CONNECTION;
    lDomain      : TFRE_DB_DOMAIN;
begin
  syscon   := _DBConnectionBC as TFRE_DB_SYSTEM_CONNECTION;
  if not syscon._FetchDomainbyID(DomainID,lDomain) then begin
    raise EFRE_DB_Exception.Create('Could not fetch domain by id '+GFRE_BT.GUID_2_HexString(DomainID));
  end else begin
    result := lDomain.GetName;
  end;
end;

function TFRE_DB_ROLE.GetDomainID: TGUID;
begin
  result := Field('domainid').AsObjectLink;
end;

function TFRE_DB_ROLE.GetFullname: TFRE_DB_String;
begin
  result := ObjectName+'@'+Domain;
end;

procedure TFRE_DB_ROLE.SetDomainID(AValue: TGUID);
begin
 Field('domainid').AsObjectLink := AValue;
 _UpdateDomainRoleKey;
end;

procedure TFRE_DB_ROLE.SetDomain(const domainname: TFRE_DB_NameType);
var syscon       : TFRE_DB_SYSTEM_CONNECTION;
    l_domainid   : TGUID;
begin
  syscon     := _DBConnectionBC as TFRE_DB_SYSTEM_CONNECTION;
  l_domainid := syscon._DomainID(domainname);
  if l_domainid=GUID_NULL then begin
    raise EFRE_DB_Exception.Create('Could not fetch domain by name '+domainname);
  end else begin
    SetDomainID(l_domainid);
  end;
end;

procedure TFRE_DB_ROLE._UpdateDomainRoleKey;
begin
 field('domainrolekey').AsString := GetDomainRoleKey(ObjectName,domainid);
end;

procedure TFRE_DB_ROLE.AddRightI(const right: IFRE_DB_RIGHT);
begin
  AddRight(right.Implementor as TFRE_DB_RIGHT);
end;

procedure TFRE_DB_ROLE.AddRight(const right: TFRE_DB_RIGHT);
begin
  Field('rights').AddObject(right);
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
var l_right : TFRE_DB_ObjectArray;
     i       : Integer;
begin
  l_right := Field('rights').AsObjectArr;
  SetLength(result,Length(l_right));
  for i:=0 to high(l_right) do begin
    result[i] := TFRE_DB_NAMED_OBJECT(l_right[i]).ObjectName;
 end;

end;

class procedure TFRE_DB_ROLE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.Strict(true);
  Scheme.SetParentSchemeByName(TFRE_DB_NAMED_OBJECT.ClassName);
  scheme.AddSchemeField('appdataid',fdbft_ObjLink).SetupFieldDef(false,true);
  scheme.AddSchemeFieldSubscheme('rights','TFRE_DB_RIGHT').multiValues:=true;
  scheme.AddSchemeField('domainid',fdbft_ObjLink).SetupFieldDef(true,false);
  scheme.AddSchemeField('domainrolekey',fdbft_String).SetupFieldDef(true,false);
  Scheme.SetSysDisplayField(GFRE_DB.ConstructStringArray(['objname','$DBTEXT:desc']),'%s - (%s)');
end;

class function TFRE_DB_ROLE.GetDomainRoleKey(const rolepart: TFRE_DB_String; const domain_id: TGUID): TFRE_DB_String;
begin
  result := GFRE_BT.GUID_2_HexString(domain_id)+'@'+lowercase(rolepart);
end;

function TFRE_DB_SYSTEM_CONNECTION.ImpersonateTheClone(const user, pass: TFRE_DB_String): TFRE_DB_Errortype;
begin
  if not FCloned then raise EFRE_DB_Exception.Create(edb_ERROR,'you can only impersonate a cloned systemconnection');
  FConnectedUser := _FetchUser(user);
  if not assigned(FConnectedUser) then Exit(edb_NOT_FOUND);
  if not FConnectedUser.Checkpassword(pass) then exit(edb_ACCESS);
  FConnectionRights := FConnectedUser.GetRightsArray;
end;

procedure TFRE_DB_SYSTEM_CONNECTION.InternalSetupConnection(const is_system, is_db_restore: boolean);

  procedure SetupAppdataCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysAppdata') then begin
      GFRE_DB.LogInfo(dblc_DB,'Adding System collection SysAppdata');
      coll := Collection('SysAppdata');
      coll.DefineIndexOnField('objname',fdbft_String,True,True);
    end;
    FSysAppdata := Collection('SysAppdata');
  end;

  procedure SetupTransTextCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysTransText') then begin
      GFRE_DB.LogInfo(dblc_DB,'Adding System collection SysTransText');
      coll := Collection('SysTransText');
      coll.DefineIndexOnField('t_key',fdbft_String,True,True);
    end;
    FSysTransText := Collection('SysTransText');
  end;

  procedure SetupSessionDataCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysSessionData') then begin
      GFRE_DB.LogInfo(dblc_DB,'Adding System collection SysSessionData',[]);
      coll := Collection('SysSessionData');
      coll.DefineIndexOnField('$LOGIN_KEY',fdbft_String,True,True);
    end;
    FSysUserSessionsData := Collection('SysSessionData');
  end;

  procedure SetupDomainCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysDomain') then begin
      GFRE_DB.LogInfo(dblc_DB,'Adding System collection SysDomain');
      coll := Collection('SysDomain');
      coll.DefineIndexOnField('objname',fdbft_String,True,True);
    end;
    FSysDomains := Collection('SysDomain');
  end;

  procedure SetupUserCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysUser') then begin
      GFRE_DB.LogInfo(dblc_DB,'Adding System collection SysUser');
      coll := Collection('SysUser');
      coll.DefineIndexOnField('domainloginkey',fdbft_String,True,True);
    end;
    FSysUsers := Collection('SysUser');
  end;

  procedure SetupRoleCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysRole') then begin
      GFRE_DB.LogInfo(dblc_DB,'Adding System collection SysRole');
      coll := Collection('SysRole');
      coll.DefineIndexOnField('domainrolekey',fdbft_String,True,True);
    end;
    FSysRoles := Collection('SysRole');
  end;

  procedure SetupUserGroupCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysUserGroup') then begin
      coll := Collection('SysUserGroup'); // Instance (new) Collections here with false parameter
      coll.DefineIndexOnField('domaingroupkey',fdbft_String,True,True);
    end;
    FSysGroups := Collection('SysUserGroup');
  end;

  procedure SetupDomains;
  begin
    if FRecreateSysObjects then
      _DeleteDomain(cSYS_DOMAIN);
    if not _DomainExists(cSYS_DOMAIN) then
      _AddDomain(cSYS_DOMAIN,'System Domain','SYSTEM DOMAIN');
  end;


begin
  inherited; //TODO Hacky checks for Schemecollection are in the baseclass :-( HH
  SetupAppdataCollection;
  SetupDomainCollection;
  SetupUserCollection;
  SetupRoleCollection;
  SetupSessionDataCollection;
  SetupTransTextCollection;
  SetupUserGroupCollection;
  SetupDomains;
  FApps := GFRE_DB.GetApps;
end;

procedure TFRE_DB_SYSTEM_CONNECTION._SetupSystemGroupsandRoles(const domain: TFRE_DB_String);
var domain_id : TGUID;

  procedure SetupDomainRoleView(const rolename:string;const role_domain:string);
  var rg:TFRE_DB_ROLE;
  begin
    rg := _NewRole(rolename,'View Domain '+domain,'View Domain '+domain);
    rg.AddRight(_NewRight(FREDB_Get_Rightname_UID('VIEWDOM',domain_id),'',''));
    rg.DomainID:=_DomainID(role_domain);
    FSysRoles.Store(TFRE_DB_Object(rg));
  end;
  procedure SetupDomainRoleEdit(const rolename:string;const role_domain:string);
  var rg:TFRE_DB_ROLE;
  begin
    rg := _NewRole(rolename,'Administer Domain '+domain,'Administer Domain '+domain);
    rg.AddRight(_NewRight(FREDB_Get_Rightname_UID('VIEWDOM',domain_id),'',''));
    rg.AddRight(_NewRight(FREDB_Get_Rightname_UID('EDITDOM',domain_id),'',''));
    rg.DomainID:=_DomainID(role_domain);
    FSysRoles.Store(TFRE_DB_Object(rg));
  end;

  procedure CheckSystemRights;
  var rolename  : string;

    procedure SetupAdminRole;
    var rg:TFRE_DB_ROLE;
    begin
      rg := _NewRole(cSYSROLE_DB_ADMIN,'Database Administrator Right Group','Administrator');
      rg.AddRight(_NewRight(cSYSR_CREATE_DB,'Allow the creation of databases','DB Create'));
      rg.AddRight(_NewRight(cSYSR_DELETE_DB,'Allow the deletion of databases','DB Delete'));
      rg.DomainID:=domain_id;
      CheckDbResult(FSysRoles.Store(TFRE_DB_Object(rg)),'Cannot create DB Admin Role');
    end;
    procedure SetupManageRole;
    var rg:TFRE_DB_ROLE;
    begin
      rg := _NewRole(cSYSROLE_DB_MANAGE,'Database Manager Right Group','Manager');
      rg.AddRight(_NewRight(cSYSR_ADD_USER,'Allow the addition of users','User add'));
      rg.AddRight(_NewRight(cSYSR_DEL_USER,'Allow the deletion of users','User delete'));
      rg.AddRight(_NewRight(cSYSR_MOD_RIGHT,'Allow the modification of users right','User rights'));
      rg.AddRight(_NewRight(cSYSR_MOD_UG,'Allow the modification of user groups on the user','Mod User User Groups'));
      rg.AddRight(_NewRight(Get_Rightname_App('syseditor','START'),'Allow start of system editor','Start Sysed'));
      rg.DomainID:=domain_id;
      FSysRoles.Store(TFRE_DB_Object(rg));
    end;
    procedure SetupManageAppRole;
    var rg:TFRE_DB_ROLE;
    begin
      rg := _NewRole(cSYSROLE_MANAGE_APPS,'Database App Manager Right Group','App Manager');
      rg.AddRight(_NewRight(cSYSR_INSTALL_APP,'Allow the addition of apps','App add'));
      rg.AddRight(_NewRight(cSYSR_UNINSTALL_APP,'Allow the deletion of apps','App delete'));
      rg.DomainID:=domain_id;
      FSysRoles.Store(TFRE_DB_Object(rg));
    end;
    procedure SetupUserRole;
    var rg:TFRE_DB_ROLE;
    begin
      rg := _NewRole(cSYSROLE_DB_USER,'Database User Right Group','User');
      rg.AddRight(_NewRight(cSYSR_LOGIN_DB,'Allow the login into databases','DB Login'));
      rg.AddRight(_NewRight(cSYSR_READ_DBO,'Allow the read of DBOs','DBO Read'));
      rg.AddRight(_NewRight(cSYSR_WRITE_DBO,'Allow the write of DBOs','DBO Write'));
      rg.AddRight(_NewRight(cSYSR_DELETE_DBO,'Allow the delete of DBOs','DBO Delete'));
      rg.AddRight(_NewRight(cSYSR_EXEC_DBO,'Allow the exec of DBO methods','DBO Exec'));
      rg.DomainID:=domain_id;
      FSysRoles.Store(TFRE_DB_Object(rg));
    end;
    procedure SetupGuestRole;
    var rg:TFRE_DB_ROLE;
    begin
      rg := _NewRole(cSYSROLE_DB_GUEST,'Database Guest Right Group','User');
      rg.AddRight(_NewRight(cSYSR_LOGIN_DB,'Allow the login into Databases','DB Login'));
      rg.AddRight(_NewRight(cSYSR_EXEC_DBO,'Allow the exec of DBO methods','DBO Exec'));
      rg.DomainID:=domain_id;
      FSysRoles.Store(TFRE_DB_Object(rg));
    end;

  begin
    if FRecreateSysObjects then _DeleteRole(cSYSROLE_DB_ADMIN+'@'+domain);
    if not _RoleExists(cSYSROLE_DB_ADMIN+'@'+domain) then begin
      GFRE_DB.LogInfo(dblc_DB,'Adding Role '+cSYSROLE_DB_ADMIN);
      SetupAdminRole;
    end;
    if FRecreateSysObjects then _DeleteRole(cSYSROLE_DB_MANAGE+'@'+domain);
    if not _RoleExists(cSYSROLE_DB_MANAGE+'@'+domain) then begin
      GFRE_DB.LogInfo(dblc_DB,'Adding Role '+cSYSROLE_DB_MANAGE);
      SetupManageRole;
    end;
    if FRecreateSysObjects then _DeleteRole(cSYSROLE_MANAGE_APPS+'@'+domain);
    if not RoleExists(cSYSROLE_MANAGE_APPS+'@'+domain) then begin
      GFRE_DB.LogInfo(dblc_DB,'Adding Role '+cSYSROLE_MANAGE_APPS);
      SetupManageAppRole;
    end;
    if FRecreateSysObjects then _DeleteRole(cSYSROLE_DB_USER+'@'+domain);
    if not _RoleExists(cSYSROLE_DB_USER+'@'+domain) then begin
      GFRE_DB.LogInfo(dblc_DB,'Adding Role '+cSYSROLE_DB_USER);
      SetupUserRole;
    end;
    if not _RoleExists(cSYSROLE_DB_GUEST+'@'+domain) then begin
      GFRE_DB.LogInfo(dblc_DB,'Adding Role '+cSYSROLE_DB_GUEST);
      SetupGuestRole;
    end;

    rolename := FREDB_Get_Rightname_UID('VIEWDOM',domain_id);
    if FRecreateSysObjects then _DeleteRole(rolename+'@'+domain);
    if not _RoleExists(rolename+'@'+domain) then begin
      GFRE_DB.LogInfo(dblc_DB,'Adding Role '+rolename);
      SetupDomainRoleView(rolename,domain);
    end;
    rolename := FREDB_Get_Rightname_UID('EDITDOM',domain_id);
    if FRecreateSysObjects then _DeleteRole(rolename+'@'+domain);
    if not _RoleExists(rolename+'@'+domain) then begin
      GFRE_DB.LogInfo(dblc_DB,'Adding Role '+rolename);
      SetupDomainRoleEdit(rolename,domain);
    end;

  end;

  procedure SetupUserGroups;
  var ug      : TFRE_DB_GROUP;
      ug_name : TFRE_DB_String;
  begin
    if FRecreateSysObjects then _DeleteGroup(cSYSUG_ADMIN_USERS+'@'+domain);
    if not _GroupExists(cSYSUG_ADMIN_USERS+'@'+domain) then begin
      ug          := _NewGroup(cSYSUG_ADMIN_USERS,'Administrative Database Users','DB ADMINS');
      ug.DomainID := domain_id;
      ug_name     := ug.ObjectName+'@'+domain;
      FSysGroups.Store(TFRE_DB_Object(ug));
      CheckDbResult(_ModifyGroupRoles(ug_name,GFRE_DB.ConstructStringArray([cSYSROLE_DB_ADMIN+'@'+domain,cSYSROLE_DB_MANAGE+'@'+domain,cSYSROLE_DB_USER+'@'+domain,cSYSROLE_MANAGE_APPS+'@'+domain,FREDB_Get_Rightname_UID('EDITDOM',domain_id)+'@'+domain])),'initial assignment of admin rgs to ugs failed');
    end;
    if FRecreateSysObjects then _DeleteGroup(cSYSUG_MANAGE_USERS+'@'+domain);
    if not _GroupExists(cSYSUG_MANAGE_USERS+'@'+domain) then begin
      ug          := _NewGroup(cSYSUG_MANAGE_USERS,'User Management Group','DB Manager');
      ug.DomainID := domain_id;
      ug_name     := ug.ObjectName+'@'+domain;
      FSysGroups.Store(TFRE_DB_Object(ug));
      CheckDbResult(_ModifyGroupRoles(ug_name,GFRE_DB.ConstructStringArray([cSYSROLE_DB_MANAGE+'@'+domain,cSYSROLE_DB_USER+'@'+domain,FREDB_Get_Rightname_UID('VIEWDOM',domain_id)+'@'+domain])),'initial assignment of manage rgs to ugs failed');
    end;
    if FRecreateSysObjects then _DeleteGroup(cSYSUG_DB_USERS+'@'+domain);
    if not _GroupExists(cSYSUG_DB_USERS+'@'+domain) then begin
      ug          := _NewGroup(cSYSUG_DB_USERS,'Database Users','DB USERS');
      ug.DomainID := domain_id;
      ug_name     := ug.ObjectName+'@'+domain;
      FSysGroups.Store(TFRE_DB_Object(ug));
      CheckDbResult(_ModifyGroupRoles(ug_name,GFRE_DB.ConstructStringArray([cSYSROLE_DB_USER+'@'+domain,FREDB_Get_Rightname_UID('VIEWDOM',domain_id)+'@'+domain])),'initial assignment of user rgs to ugs failed');
    end;
    if not _GroupExists(cSYSUG_DB_GUESTS+'@'+domain) then begin
      ug          := _NewGroup(cSYSUG_DB_GUESTS,'Database Guest Users','DB USERS');
      ug.DomainID := domain_id;
      ug_name     := ug.ObjectName+'@'+domain;
      FSysGroups.Store(TFRE_DB_Object(ug));
      CheckDbResult(_ModifyGroupRoles(ug_name,GFRE_DB.ConstructStringArray([cSYSROLE_DB_GUEST+'@'+domain])),'initial assignment of user rgs to ugs failed');
    end;
  end;

  procedure CheckStandardUsers;
  begin
    if not _UserExists('admin@'+domain) then begin
      GFRE_DB.LogWarning(dblc_DB,'Adding initial db admin/admin account');
      CheckDbResult(_AddUser('admin@'+domain,'admin','Initial','FRE DB Admin'),'initial creation of admin user failed');
      CheckDbResult(_ModifyUserGroups('admin@'+domain,GFRE_DB.ConstructStringArray([cSYSUG_ADMIN_USERS+'@'+domain,cSYSUG_MANAGE_USERS+'@'+domain,cSYSUG_DB_USERS+'@'+domain])),'initial admin user group assignment failed');
    end;
    if domain = cSYS_DOMAIN then begin
      if not _UserExists('guest@'+domain) then begin
        GFRE_DB.LogWarning(dblc_DB,'Adding initial db guest account');
        CheckDbResult(_AddUser('guest@'+domain,'','Initial','FRE DB Guest'),'initial creation of guest user failed');
        CheckDbResult(_ModifyUserGroups('guest@'+domain,GFRE_DB.ConstructStringArray([cSYSUG_DB_GUESTS+'@'+domain])),'initial guest user group assignment failed');
      end;
    end;
  end;

begin
 domain_id := _DomainID(domain);
 CheckSystemRights;
 SetupUserGroups;
 CheckStandardUsers;
 if domain<>cSYS_DOMAIN then begin
   SetupDomainRoleEdit(FREDB_Get_Rightname_UID('EDITDOM',domain_id),cSYS_DOMAIN);
   CheckDbResult(_AddGroupRoles(cSYSUG_ADMIN_USERS+'@'+cSYS_DOMAIN,GFRE_DB.ConstructStringArray([FREDB_Get_Rightname_UID('EDITDOM',domain_id)+'@'+cSYS_DOMAIN])),'adding domain edit to system admin');
 end;
end;

procedure TFRE_DB_SYSTEM_CONNECTION._ReloadUserAndRights;
var  useruid     : TGUID;
begin
 if assigneD(FConnectedUser) then // Startup Case
   begin
     useruid  := FConnectedUser.UID;
     FConnectedUser.Finalize;
     if not _FetchUserById(useruid,FConnectedUser) then
       raise EFRE_DB_Exception.Create(edb_INTERNAL,'could not fetch userid on reload of user');
     FConnectionRights := FConnectedUser.GetRightsArray;
   end;
end;

function TFRE_DB_SYSTEM_CONNECTION._UserExists(const loginatdomain: TFRE_DB_String): boolean;
 var login      : TFRE_DB_String;
     domain     : TFRE_DB_String;
     domain_id  : TGUID;
begin
 FREDB_SplitLocalatDomain(loginatdomain,login,domain);
 domain_Id := _DomainID(domain);
 result := FSysUsers.ExistsIndexed(TFRE_DB_USER.GetDomainLoginKey(login,domain_id));
end;

function TFRE_DB_SYSTEM_CONNECTION._FetchUser(const loginatdomain: TFRE_DB_String): TFRE_DB_USER;
var login      : TFRE_DB_String;
    domain     : TFRE_DB_String;
    domain_id  : TGUID;
begin
  FREDB_SplitLocalatDomain(loginatdomain,login,domain);
  domain_Id := _DomainID(domain);
  result:=nil;
  if not FSysUsers.GetIndexedObj(TFRE_DB_USER.GetDomainLoginKey(login,domain_id),TFRE_DB_Object(result)) then exit;
end;

function TFRE_DB_SYSTEM_CONNECTION._FetchUserById(const user_id: TGUID; var user: TFRE_DB_USER): boolean;
begin
  result := Fetch(user_id,TFRE_DB_Object(user));
end;


procedure TFRE_DB_BASE_CONNECTION._ConnectCheck;
begin
  if FConnected=false then raise EFRE_DB_Exception.Create(edb_NOT_CONNECTED);
end;

procedure TFRE_DB_BASE_CONNECTION._CloneCheck;
begin
  if FCloned=true then raise EFRE_DB_Exception.Create(edb_ERROR,'operation not allowed on cloned database');
end;


procedure TFRE_DB_BASE_CONNECTION._CheckSchemeDefinitions(const obj: TFRE_DB_Object);
var lSchemeclass : TFRE_DB_String;
    lScheme      : TFRE_DB_SchemeObject;
begin
  if obj._ObjectIsCodeclassOnlyAndHasNoScheme then exit;
  lSchemeclass := obj.SchemeClass;
  if lSchemeClass<>'' then begin
    if GetScheme(lSchemeclass,lscheme) then begin
      lScheme.ValidateObject(obj);
    end else begin
      raise EFRE_DB_Exception.Create(edb_ERROR,'cannot access schemeclass [%s] on store',[lSchemeclass]);
    end;
  end;
end;


function TFRE_DB_BASE_CONNECTION._AppExists(const name: TFRE_DB_String): boolean;
var i:integer;
begin
  for i:=0 to high(FApps) do begin
    if FApps[i].ObjectName=uppercase(name) then exit(true);
  end;
  result := false;
end;

function TFRE_DB_BASE_CONNECTION._FetchApp(const name: TFRE_DB_String; var app: TFRE_DB_APPLICATION): boolean;
var i:integer;
begin
  for i:=0 to high(FApps) do begin
    if FApps[i].ObjectName=uppercase(name) then begin
      app := FApps[i];
      exit(true);
    end;
  end;
  app := nil;
  result := false;
end;

function TFRE_DB_SYSTEM_CONNECTION._RoleExists(const roleatdomain: TFRE_DB_String): boolean;
var rolename   : TFRE_DB_String;
    domain     : TFRE_DB_String;
    domain_id  : TGUID;
begin
  FREDB_SplitLocalatDomain(roleatdomain,rolename,domain);
  domain_Id := _DomainID(domain);
  result := FSysRoles.ExistsIndexed(TFRE_DB_ROLE.GetDomainRoleKey(rolename,domain_id));
end;

function TFRE_DB_SYSTEM_CONNECTION._FetchRole(const roleatdomain: TFRE_DB_String; var role: TFRE_DB_ROLE): boolean;
var rolename   : TFRE_DB_String;
    domain     : TFRE_DB_String;
    domain_id  : TGUID;
begin
  FREDB_SplitLocalatDomain(roleatdomain,rolename,domain);
  domain_Id := _DomainID(domain);
  result := FSysRoles.GetIndexedObj(TFRE_DB_ROLE.GetDomainRoleKey(rolename,domain_id),TFRE_DB_Object(role));
end;

function TFRE_DB_SYSTEM_CONNECTION._RoleID(const roleatdomain: TFRE_DB_String; var role_id: TGUID): boolean;
var rolename   : TFRE_DB_String;
    domain     : TFRE_DB_String;
    domain_id  : TGUID;
begin
  FREDB_SplitLocalatDomain(roleatdomain,rolename,domain);
  domain_Id := _DomainID(domain);
  result := FSysRoles.GetIndexedUID(TFRE_DB_ROLE.GetDomainRoleKey(rolename,domain_id),role_id);
end;

function TFRE_DB_SYSTEM_CONNECTION._FetchRolebyID(const role_id: TGUID; var role: TFRE_DB_ROLE): boolean;
begin
 result := Fetch(role_id,TFRE_DB_Object(role));
end;

function TFRE_DB_SYSTEM_CONNECTION._GroupExists(const groupatdomain: TFRE_DB_String): boolean;
var group      : TFRE_DB_String;
    domain     : TFRE_DB_String;
    domain_id  : TGUID;
begin
  FREDB_SplitLocalatDomain(groupatdomain,group,domain);
  domain_Id := _DomainID(domain);
  result := FSysGroups.ExistsIndexed(TFRE_DB_GROUP.GetDomainGroupKey(group,domain_id));
end;

function TFRE_DB_SYSTEM_CONNECTION._FetchGroup(const groupatdomain: TFRE_DB_String; var ug: TFRE_DB_GROUP): boolean;
var group      : TFRE_DB_String;
    domain     : TFRE_DB_String;
    domain_id  : TGUID;
begin
  FREDB_SplitLocalatDomain(groupatdomain,group,domain);
  domain_Id := _DomainID(domain);
  result := FSysGroups.GetIndexedObj(TFRE_DB_GROUP.GetDomainGroupKey(group,domain_id),TFRE_DB_Object(ug));
end;

function TFRE_DB_SYSTEM_CONNECTION._GroupID(const groupatdomain: TFRE_DB_String; var group_id: TGUID): boolean;
var group      : TFRE_DB_String;
    domain     : TFRE_DB_String;
    domain_id  : TGUID;
begin
  FREDB_SplitLocalatDomain(groupatdomain,group,domain);
  domain_Id := _DomainID(domain);
  result := FSysGroups.GetIndexedUID(TFRE_DB_GROUP.GetDomainGroupKey(group,domain_id),group_id);
end;

function TFRE_DB_SYSTEM_CONNECTION._FetchGroupbyID(const group_id: TGUID; var ug: TFRE_DB_GROUP): boolean;
begin
  result := Fetch(group_id,TFRE_DB_Object(ug));
end;

function TFRE_DB_SYSTEM_CONNECTION._ModifyUserGroups(const loginatdomain: TFRE_DB_String; const user_groups: TFRE_DB_StringArray; const keep_existing_groups: boolean): TFRE_DB_Errortype;
var l_User            : TFRE_DB_USER;
    l_OldGroups       : TFRE_DB_StringArray;
    l_NewGroups       : TFRE_DB_StringArray;
    l_NewGroupsID     : TFRE_DB_GUIDArray;
    i                 : integer;
    j                 : integer;
    old_group_count   : integer;
    new_group_count   : integer;
    already_in        : boolean;
    new_group_id      : TGUID;
begin
  l_user:= _FetchUser(loginatdomain);
  if l_user = nil then exit(edb_NOT_FOUND);
  l_OldGroups           := Copy(l_User.UserGroupNames);
  old_group_count       := Length(l_oldGroups);
  if keep_existing_groups then begin
    SetLength(l_NewGroups,old_group_count+Length(user_groups));
    for i:=0 to high(l_OldGroups) do begin
      l_NewGroups[i]    := l_OldGroups[i];
    end;
    new_group_count     := old_group_count;
    for i:=0 to high(user_groups) do begin
      already_in := false;
      for j := 0 to new_group_count-1 do begin
        if l_NewGroups[j]=user_groups[i] then begin
          already_in := true;
          break;
        end;
      end;
      if already_in=false then begin
        l_NewGroups[new_group_count]    := user_groups[i];
        inc(new_group_count);
      end;
    end;
    SetLength(l_NewGroupsID,new_group_count);
  end else begin
    new_group_count     := Length(user_groups);
    SetLength(l_NewGroupsID,new_group_count);
    l_NewGroups         := user_groups;
  end;
  for i:=0 to new_group_count-1 do begin
    if not _GroupID(l_NewGroups[i],new_group_id) then begin
      writeln('NEWGROUPS:',l_NewGroups[i]);
      exit(edb_NOT_FOUND);
    end;
    l_NewGroupsID[i] := new_group_id;
  end;
  l_User.UserGroupIDs   := l_NewGroupsID;
  result:=Update(l_User);
  CheckDbResultFmt(Result,'could not update user %s',[loginatdomain]);
  Result := edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION._RemoveUserGroups(const loginatdomain: TFRE_DB_String; const user_groups: TFRE_DB_StringArray): TFRE_DB_Errortype;
var l_User            : TFRE_DB_USER;
    l_OldGroups       : TFRE_DB_StringArray;
    l_NewGroupsID     : TFRE_DB_GUIDArray;
    i                 : integer;
    j                 : integer;
    old_group_count   : integer;
    new_group_count   : integer;
    new_group_id      : TGUID;
begin
  l_user:= _FetchUser(loginatdomain);
  if l_user = nil then exit(edb_NOT_FOUND);
  l_OldGroups           := Copy(l_User.UserGroupNames);
  old_group_count       := Length(l_oldGroups);
  new_group_count       := old_group_count;
  for i:=0 to high(user_groups) do begin
    for j := 0 to old_group_count-1 do begin
      if l_OldGroups[j]=user_groups[i] then begin
        l_OldGroups[j] := '';
        dec(new_group_count);
        break;
      end;
    end;
  end;

  SetLength(l_NewGroupsID,new_group_count);
  j:=0;
  for i:=0 to old_group_count-1 do begin
    if l_oldGroups[i]<>'' then begin
      if not _GroupID(l_oldGroups[i],new_group_id) then begin
        exit(edb_NOT_FOUND);
      end;
      l_NewGroupsID[j] := new_group_id;
      inc(j);
    end;
  end;
  l_User.UserGroupIDs   := l_NewGroupsID;
  result:=Update(l_User);
  CheckDbResultFmt(Result,'could not update user %s',[loginatdomain]);
  Result := edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION._ModifyGroupRoles(const groupatdomain: TFRE_DB_String; const roles: TFRE_DB_StringArray): TFRE_DB_Errortype;
var l_Group          : TFRE_DB_GROUP;
    l_UserUid        : TGUID;
    l_NewRoles       : TFRE_DB_StringArray;
    l_NewRolesID     : TFRE_DB_GUIDArray;
    l_FetchedRoleUID : TGUID;
    role             : TFRE_DB_ROLE;
    i                :  integer;
begin
  if not _FetchGroup(groupatdomain,l_group) then exit(edb_NOT_FOUND);
  l_NewRoles        := roles;
  setLength(l_NewRolesID,length(l_NewRoles));
  for i:=0 to high(l_NewRoles) do begin
    if not _RoleID(l_NewRoles[i],l_FetchedRoleUID) then exit(edb_NOT_FOUND);
    l_NewRolesID[i] :=l_FetchedRoleUID;
  end;
  l_Group.RoleIDs   := l_NewRolesID;
  result := Update(l_Group);
end;

function TFRE_DB_SYSTEM_CONNECTION._AddGroupRoles(const groupatdomain: TFRE_DB_String; const roles: TFRE_DB_StringArray): TFRE_DB_Errortype;
var l_Group            : TFRE_DB_GROUP;
    l_NewRoles         : TFRE_DB_StringArray;
    l_NewRolesID       : TFRE_DB_GUIDArray;
    l_AggregatedRoleID : TFRE_DB_GUIDArray;
    l_FetchedRoleUID   : TGUID;
    i                  : NativeInt;
    j                  : NativeInt;
    allready_in        : boolean;
begin
  if not _FetchGroup(groupatdomain,l_group) then exit(edb_NOT_FOUND);
  l_NewRoles        := roles;
  setLength(l_NewRolesID,length(l_NewRoles));
  for i:=0 to high(l_NewRoles) do begin
    if not _RoleID(l_NewRoles[i],l_FetchedRoleUID) then exit(edb_NOT_FOUND);
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

function TFRE_DB_SYSTEM_CONNECTION._RemoveGroupRoles(const groupatdomain: TFRE_DB_String; const roles: TFRE_DB_StringArray; const ignore_not_set: boolean): TFRE_DB_Errortype;
var l_Group            : TFRE_DB_GROUP;
    l_DelRoles         : TFRE_DB_StringArray;
    l_DelRolesID       : TFRE_DB_GUIDArray;
    l_ReducedRoleID    : TFRE_DB_GUIDArray;
    l_CopyRoleID       : TFRE_DB_GUIDArray;
    l_FetchedRoleUID   : TGUID;
    i                  : NativeInt;
    j                  : NativeInt;
    l_found            : boolean;
    l_remove_count     : NativeInt;

begin
  if not _FetchGroup(groupatdomain,l_group) then exit(edb_NOT_FOUND);
  l_DelRoles        := roles;
  setLength(l_DelRolesID,length(l_DelRoles));
  for i:=0 to high(l_DelRoles) do begin
    if not _RoleID(l_DelRoles[i],l_FetchedRoleUID) then exit(edb_NOT_FOUND);
    l_DelRolesID[i] :=l_FetchedRoleUID;
  end;
  l_ReducedRoleID   := l_Group.RoleIDs;
  l_remove_count    := 0;
  for i:=0 to high(l_DelRolesID) do begin
    l_found := false;
    for j:=0 to high(l_ReducedRoleID) do begin
      if l_DelRolesID[i]=l_ReducedRoleID[j] then begin
        l_found              := true;
        l_ReducedRoleID[j] := GUID_NULL;
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
    if l_ReducedRoleID[i]<>GUID_NULL then begin
      l_CopyRoleID[j] := l_ReducedRoleID[i];
      inc(j);
    end;
  end;
  l_Group.RoleIDs := l_CopyRoleID;
  result := Update(l_Group);
end;

function TFRE_DB_SYSTEM_CONNECTION._ModifyUserPassword(const loginatdomain, oldpassword, newpassword: TFRE_DB_String): TFRE_DB_Errortype;
var l_User:TFRE_DB_USER;
begin
  if (uppercase(loginatdomain)<>uppercase(FConnectedUser.Login)) then begin
    if not _CheckRight(cSYSR_MOD_RIGHT) then exit(edb_ACCESS);
  end;
  result := FetchUser(loginatdomain,l_User);
  if result<>edb_OK then exit;
  if not l_User.Checkpassword(oldpassword) then exit(edb_ACCESS);
  l_User.SetPassword(newpassword);
  Update(l_User);
end;

function TFRE_DB_SYSTEM_CONNECTION._ModifyUserImage(const loginatdomain: TFRE_DB_String; const imagestream: TFRE_DB_Stream): TFRE_DB_Errortype;
var l_User:TFRE_DB_USER;
begin
  result := FetchUser(loginatdomain,l_User);
  if result<>edb_OK then exit;
  l_User.SetImage(imagestream);
  Update(l_User);
end;

function TFRE_DB_SYSTEM_CONNECTION._DeleteGroup(const groupatdomain: TFRE_DB_String): TFRE_DB_Errortype;
var group      : TFRE_DB_String;
    domain     : TFRE_DB_String;
    domain_id  : TGUID;
begin
  FREDB_SplitLocalatDomain(groupatdomain,group,domain);
  domain_Id := _DomainID(domain);
  if FSysGroups.RemoveIndexed(TFRE_DB_GROUP.GetDomainGroupKey(group,domain_id)) then begin
    result := edb_OK;
  end else begin
    result := edb_ERROR;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION._DeleteRole(const roleatdomain: TFRE_DB_String): TFRE_DB_Errortype;
var rolename   : TFRE_DB_String;
    domain     : TFRE_DB_String;
    domain_id  : TGUID;
begin
  FREDB_SplitLocalatDomain(roleatdomain,rolename,domain);
  domain_Id := _DomainID(domain);
  if FSysRoles.RemoveIndexed(TFRE_DB_ROLE.GetDomainRoleKey(rolename,domain_id)) then begin
    result := edb_OK;
  end else begin
    result := edb_ERROR;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION._DomainExists(const name: TFRE_DB_NameType): boolean;
begin
  result := FSysDomains.ExistsIndexed(name);
end;

function TFRE_DB_SYSTEM_CONNECTION._DomainID(const name: TFRE_DB_NameType): TGUID;
begin
  if not FSysDomains.GetIndexedUID(name,result) then begin
    result := GUID_NULL;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION._DeleteDomain(const name: TFRE_DB_NameType): TFRE_DB_Errortype;
begin
  if FSysDomains.RemoveIndexed(name) then
    result := edb_OK
  else
    result := edb_ERROR;
end;

function TFRE_DB_SYSTEM_CONNECTION._DeleteDomainbyID(const domain_id: TGUID): TFRE_DB_Errortype;
begin
  if _DomainID(cSYS_DOMAIN)=domain_id then
    raise EFRE_DB_Exception.Create('Deletion of the system domain is not allowed !');
  if FSysDomains.Remove(domain_id) then
    result := edb_OK
  else
    result := edb_ERROR;
end;

function TFRE_DB_SYSTEM_CONNECTION._FetchDomain(const name: TFRE_DB_NameType; var domain: TFRE_DB_DOMAIN): boolean;
begin
  result := FSysDomains.GetIndexedObj(name,TFRE_DB_Object(domain));
end;

function TFRE_DB_SYSTEM_CONNECTION._FetchDomainbyID(const domain_id: TGUID; var domain: TFRE_DB_DOMAIN): boolean;
begin
  result := Fetch(domain_id,TFRE_DB_Object(domain));
end;

function TFRE_DB_SYSTEM_CONNECTION._NewText(const key, txt, txt_short: TFRE_DB_String): TFRE_DB_TEXT;
begin
  result := GFRE_DB._NewText(key,txt,txt_short);
end;

function TFRE_DB_SYSTEM_CONNECTION._NewRight(const rightname, txt, txt_short: TFRE_DB_String): TFRE_DB_RIGHT;
begin
   result := GFRE_DB._NewRight(rightname,txt,txt_short);
end;


function TFRE_DB_SYSTEM_CONNECTION._NewRole(const rolename, txt, txt_short: TFRE_DB_String): TFRE_DB_ROLE;
begin
  result := GFRE_DB._NewRole(rolename,txt,txt_short);
end;

function TFRE_DB_SYSTEM_CONNECTION._NewGroup(const groupname, txt, txt_short: TFRE_DB_String): TFRE_DB_GROUP;
begin
  result := GFRE_DB._NewGroup(groupname,txt,txt_short);
end;

function TFRE_DB_SYSTEM_CONNECTION._NewAppData(const appname, txt, txt_short: TFRE_DB_String): TFRE_DB_APPDATA;
begin
  result := GFRE_DB._NewAppData(appname,txt,txt_short);
end;

function TFRE_DB_SYSTEM_CONNECTION._NewDomain(const domainname, txt, txt_short: TFRE_DB_NameType): TFRE_DB_DOMAIN;
begin
  result := GFRE_DB._NewDomain(domainname,txt,txt_short);
end;

function TFRE_DB_SYSTEM_CONNECTION.Connect(const loginatdomain: TFRE_DB_String; const pass: TFRE_DB_String; const rebuild_sys_management: boolean): TFRE_DB_Errortype;
begin
  _CloneCheck;
  FRecreateSysObjects:=rebuild_sys_management;
  if not FConnected then
    begin
      result := _Connect('SYSTEM',true);
      if Result<>edb_OK then
        exit;
    end;
  FConnectedUser := _FetchUser(loginatdomain);
  if not assigned(FConnectedUser) then
    Exit(edb_NOT_FOUND);
  if not FConnectedUser.Checkpassword(pass) then
    begin
      exit(edb_ACCESS);
    end
  else
    begin
      FConnectionRights := FConnectedUser.GetRightsArray;
      FAuthenticated := true;
    end;
  result := edb_OK;
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

function TFRE_DB_SYSTEM_CONNECTION.AddUser(const loginatdomain, password, first_name, last_name: TFRE_DB_String): TFRE_DB_Errortype;
begin
  if not _CheckRight(cSYSR_ADD_USER) then exit(edb_ACCESS);
  result := _AddUser(loginatdomain,password,first_name,last_name);
  if result<>edb_OK then exit;
  CheckDbResult(_ModifyUserGroups(loginatdomain,GFRE_DB.ConstructStringArray([cSYSUG_DB_USERS+'@'+cSYS_DOMAIN])),'initial assignment of user group failed'); // Login right
  result:=edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION.UserExists(const loginatdomain: TFRE_DB_String): boolean;
begin
  result := _UserExists(loginatdomain);
end;

function TFRE_DB_SYSTEM_CONNECTION.DeleteUser(const loginatdomain: TFRE_DB_String): TFRE_DB_Errortype;
begin
  if not _CheckRight(cSYSR_DEL_USER) then exit(edb_ACCESS);
  result := _DeleteUser(loginatdomain);
end;

function TFRE_DB_SYSTEM_CONNECTION.DeleteUserById(const user_id: TGUID): TFRE_DB_Errortype;
begin
  result := _DeleteUserbyID(user_id);
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchUser(const loginatdomain: TFRE_DB_String; var user: TFRE_DB_USER): TFRE_DB_Errortype;
begin
  user := _FetchUser(loginatdomain);
  if assigned(user) then exit(edb_OK);
  result := edb_NOT_FOUND;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchUserI(const loginatdomain: TFRE_DB_String; var user: IFRE_DB_USER): TFRE_DB_Errortype;
var lUSER: TFRE_DB_USER;
begin
  result := FetchUser(loginatdomain,lUSER);
  if result = edb_OK then begin
    user := lUSER;
  end else begin
    user := nil;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchUserById(const user_id: TGUID; var user: TFRE_DB_USER): TFRE_DB_Errortype;
begin
  if _FetchUserById(user_id,user) then exit(edb_OK);
  result := edb_NOT_FOUND;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchUserByIdI(const user_id: TGUID; var user: IFRE_DB_USER): TFRE_DB_Errortype;
var
  tuser: TFRE_DB_USER;
begin
  Result:=FetchUserById(user_id,tuser);
  user:=tuser;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchGroup(const groupatdomain: TFRE_DB_String; var ug: TFRE_DB_GROUP): TFRE_DB_Errortype;
begin
  if _FetchGroup(groupatdomain,ug) then exit(edb_OK);
  result := edb_NOT_FOUND;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchGroupI(const groupatdomain: TFRE_DB_String; var ug: IFRE_DB_GROUP): TFRE_DB_Errortype;
var
  tug: TFRE_DB_GROUP;
begin
  Result:=FetchGroup(groupatdomain,tug);
  ug:=tug;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchGroupById(const group_id: TGUID; var ug: TFRE_DB_GROUP): TFRE_DB_Errortype;
begin
  if _FetchGroupbyID(group_id,ug) then exit(edb_OK);
  result := edb_NOT_FOUND;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchGroupByIdI(const group_id: TGUID; var ug: IFRE_DB_GROUP): TFRE_DB_Errortype;
var
  tug: TFRE_DB_GROUP;
begin
  Result:=FetchGroupById(group_id,tug);
  ug:=tug;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchRole(const roleatdomain: TFRE_DB_NameType; var role: TFRE_DB_ROLE): TFRE_DB_Errortype;
begin
  _FetchRole(roleatdomain,role);
  if assigned(role) then
    exit(edb_OK);
  result := edb_NOT_FOUND;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchRoleI(const roleatdomain: TFRE_DB_NameType; var role: IFRE_DB_ROLE): TFRE_DB_Errortype;
var roleo : TFRE_DB_ROLE;
begin
  result := FetchRole(roleatdomain,roleo);
  if result = edb_OK then
    role := roleo
  else
    role := nil;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchRoleById(const role_id: TGUID; var role: TFRE_DB_ROLE): TFRE_DB_Errortype;
begin
  if _FetchRolebyID(role_id,role) then exit(edb_OK);
  result := edb_NOT_FOUND;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchRoleByIdI(const role_id: TGUID; var role: IFRE_DB_ROLE): TFRE_DB_Errortype;
var
  trole: TFRE_DB_ROLE;
begin
  Result:=FetchRoleById(role_id,trole);
  role:=trole;
end;

procedure TFRE_DB_SYSTEM_CONNECTION.ForAllColls(const iterator: TFRE_DB_Coll_Iterator);
begin
  inherited ForAllColls(iterator);
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchDomainById(const domain_id: TGUID; var domain: TFRE_DB_DOMAIN): TFRE_DB_Errortype;
begin
  if _FetchDomainbyID(domain_id,domain) then exit(edb_OK);
  result := edb_NOT_FOUND;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchDomainByIdI(const domain_id: TGUID; var domain: IFRE_DB_DOMAIN): TFRE_DB_Errortype;
var
  tdomain: TFRE_DB_DOMAIN;
begin
  Result:=FetchDomainById(domain_id,tdomain);
  domain:=tdomain;
end;

function TFRE_DB_SYSTEM_CONNECTION.ModifyDomainById(const domain_id: TGUID; const domainname: TFRE_DB_NameType; const txt, txt_short: TFRE_DB_String): TFRE_DB_Errortype;
var
  tdomain: TFRE_DB_DOMAIN;
begin
  Result:=FetchDomainById(domain_id,tdomain);
  if Result=edb_OK then begin
    tdomain.ObjectName := domainname;
    tdomain.Description.ShortText:= txt_short;
    tdomain.Description.LongText := txt;
    result := Update(tdomain);
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.DeleteDomainById(const domain_id: TGUID): TFRE_DB_Errortype;
begin
  result := _DeleteDomainbyID(domain_id);
end;

function TFRE_DB_SYSTEM_CONNECTION.AddDomain(const domainname: TFRE_DB_NameType; const txt, txt_short: TFRE_DB_String): TFRE_DB_Errortype;
begin
  result := _AddDomain(domainname,txt,txt_short);
end;

function TFRE_DB_SYSTEM_CONNECTION.DomainExists(const domainname: TFRE_DB_NameType): boolean;
begin
  result :=_DomainExists(domainname);
end;

function TFRE_DB_SYSTEM_CONNECTION.DeleteDomain(const domainname: TFRE_DB_Nametype): TFRE_DB_Errortype;
begin
  result := _DeleteDomain(domainname);
end;

function TFRE_DB_SYSTEM_CONNECTION.IsSystemGroup(const group_id: TGUID): boolean;
var
  tgroup: TFRE_DB_GROUP;
begin
  FetchGroupById(group_id,tgroup);
  result := (tgroup.Domain=cSYS_DOMAIN);
end;

procedure TFRE_DB_SYSTEM_CONNECTION.ForAllDomainsI(const func: IFRE_DB_Obj_Iterator);
begin
 FSysDomains.ForAllI(func);
end;


function TFRE_DB_SYSTEM_CONNECTION.FetchUserSessionData(var SessionData: IFRE_DB_OBJECT): boolean;
begin
  //TODO Errorhandling
  result := FSysUserSessionsData.GetIndexedObjI(FConnectedUser.Login,SessionData);
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreUserSessionData(var session_data: IFRE_DB_Object):TFRE_DB_Errortype;
var key : TFRE_DB_String;
begin
 key := uppercase(FConnectedUser.Login);
 session_data.Field('$LOGIN_KEY').AsString := key;
 if FSysUserSessionsData.ExistsIndexed(key) then begin
   result := FSysUserSessionsData.UpdateI(session_data);
 end else begin
   result := FSysUserSessionsData.StoreI(session_data);
 end;
end;

function TFRE_DB_SYSTEM_CONNECTION.NewRight(const rightname, txt, txt_short: TFRE_DB_String; var right: TFRE_DB_RIGHT): TFRE_DB_Errortype;
begin
  if not _CheckRight(cSYSR_MOD_RIGHT) then exit(edb_ACCESS);
  right := _NewRight(rightname,txt,txt_short);
  result:=edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION.NewRole(const rolename, txt, txt_short: TFRE_DB_String; var role: TFRE_DB_ROLE): TFRE_DB_Errortype;
begin
  if not _CheckRight(cSYSR_MOD_RIGHT) then exit(edb_ACCESS);
  role := _NewRole(rolename,txt,txt_short);
  result:=edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION.NewGroup(const groupname, txt, txt_short: TFRE_DB_String; var user_group: TFRE_DB_GROUP): TFRE_DB_Errortype;
begin
  if not _CheckRight(cSYSR_MOD_UG) then exit(edb_ACCESS);
  user_group := _NewGroup(groupname,txt,txt_short);
  result:=edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION.NewAppData(const appname, txt, txt_short: TFRE_DB_String; var appdata: TFRE_DB_APPDATA): TFRE_DB_Errortype;
begin
  if not _CheckRight(cSYSR_INSTALL_APP) then exit(edb_ACCESS);
  appdata := _NewAppData(appname,txt,txt_short);
  result:=edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION.ModifyGroupRoles(const groupatdomain: TFRE_DB_String; const roles: TFRE_DB_StringArray): TFRE_DB_Errortype;
begin
  if not _CheckRight(cSYSR_MOD_RIGHT) then exit(edb_ACCESS);
  result := _ModifyGroupRoles(groupatdomain,roles);
end;

function TFRE_DB_SYSTEM_CONNECTION.AddGroupRoles(const groupatdomain: TFRE_DB_String; const roles: TFRE_DB_StringArray): TFRE_DB_Errortype;
begin
  if not _CheckRight(cSYSR_MOD_RIGHT) then exit(edb_ACCESS);
  result := _AddGroupRoles(groupatdomain,roles);
end;

function TFRE_DB_SYSTEM_CONNECTION.RemoveGroupRoles(const groupatdomain: TFRE_DB_String; const roles: TFRE_DB_StringArray; const ignore_not_set: boolean): TFRE_DB_Errortype;
begin
  if not _CheckRight(cSYSR_MOD_RIGHT) then exit(edb_ACCESS);
  result := _RemoveGroupRoles(groupatdomain,roles, ignore_not_set);
end;

function TFRE_DB_SYSTEM_CONNECTION.ModifyUserGroups(const loginatdomain: TFRE_DB_String; const user_groups: TFRE_DB_StringArray; const keep_existing_groups: boolean): TFRE_DB_Errortype;
begin
  if not _CheckRight(cSYSR_MOD_UG) then exit(edb_ACCESS);
  result := _ModifyUserGroups(loginatdomain,user_groups,keep_existing_groups);
end;

function TFRE_DB_SYSTEM_CONNECTION.RemoveUserGroups(const loginatdomain: TFRE_DB_String; const user_groups: TFRE_DB_StringArray): TFRE_DB_Errortype;
begin
  if not _CheckRight(cSYSR_MOD_UG) then exit(edb_ACCESS);
  result := _RemoveUserGroups(loginatdomain,user_groups);
end;

function TFRE_DB_SYSTEM_CONNECTION.ModifyUserPassword(const loginatdomain, oldpassword, newpassword: TFRE_DB_String): TFRE_DB_Errortype;
begin
  result := _ModifyUserPassword(loginatdomain,oldpassword,newpassword);
end;

function TFRE_DB_SYSTEM_CONNECTION.ModifyUserImage(const loginatdomain: TFRE_DB_String; const imagestream: TFRE_DB_Stream): TFRE_DB_Errortype;
begin
 result := _ModifyUserImage(loginatdomain,imagestream);
end;

function TFRE_DB_SYSTEM_CONNECTION.RoleExists(const roleatdomain: TFRE_DB_String): boolean;
begin
  result := _RoleExists(roleatdomain);
end;

function TFRE_DB_SYSTEM_CONNECTION.GroupExists(const groupatdomain: TFRE_DB_String): boolean;
begin
  result := _GroupExists(groupatdomain);
end;

function TFRE_DB_SYSTEM_CONNECTION.DeleteGroup(const groupatdomain: TFRE_DB_String): TFRE_DB_Errortype;
begin
  if not _CheckRight(cSYSR_MOD_UG) then exit(edb_ACCESS);
  result := _DeleteGroup(groupatdomain);
end;

function TFRE_DB_SYSTEM_CONNECTION.DeleteRole(const roleatdomain: TFRE_DB_String): TFRE_DB_Errortype;
begin
  if not _CheckRight(cSYSR_MOD_RIGHT) then exit(edb_ACCESS);
  result := _DeleteRole(roleatdomain);
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreRole(const appname: TFRE_DB_String; const domainname: TFRE_DB_NameType; var role: TFRE_DB_ROLE): TFRE_DB_Errortype;
var app_id :TGUID;
    domain_id : TGUID;
begin
  if not _CheckRight(cSYSR_MOD_RIGHT) then exit(edb_ACCESS);
  domain_id := _DomainID(domainname);
  result := GetAppDataID(appname,app_id);
  if result=edb_OK then begin
    role.Field('appdataid').AsObjectLink := app_id;
    role.SetDomainID(domain_id);
    result :=FSysRoles.Store(TFRE_DB_Object(role));
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreGroup(const appname: TFRE_DB_String; const domainname: TFRE_DB_NameType; var ug: TFRE_DB_GROUP): TFRE_DB_Errortype;
var app_id    : TGUID;
    domain_id : TGUID;
begin
  if not _CheckRight(cSYSR_MOD_UG) then exit(edb_ACCESS);
  domain_id := _DomainID(domainname);
  if appname<>'' then begin
    result := GetAppDataID(appname,app_id);
    if result=edb_OK then begin
      ug.Field('appdataid').AsObjectLink := app_id;
    end else begin
      exit;
    end;
  end;
  result := StoreGroupDomainbyID(domain_id,ug);
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreGroupDomainbyID(const domain_id: TGUID; var group: TFRE_DB_GROUP): TFRE_DB_Errortype;
begin
 group.SetDomainID(domain_id);
 result := FSysGroups.Store(TFRE_DB_Object(group));
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreAppData(var appdata: TFRE_DB_APPDATA): TFRE_DB_Errortype;
var appid : TGUID;
begin
  if not _CheckRight(cSYSR_INSTALL_APP) then exit(edb_ACCESS);
  result := FSysAppdata.Store(TFRE_DB_Object(appdata));
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreTranslateableText(var txt: TFRE_DB_TEXT): TFRE_DB_Errortype;
begin
  if not _CheckRight(cSYSR_INSTALL_APP) then exit(edb_ACCESS);
  result := FSysTransText.Store(TFRE_DB_Object(txt));
end;

function TFRE_DB_SYSTEM_CONNECTION.UpdateAppData(var appdata: TFRE_DB_APPDATA): TFRE_DB_Errortype;
begin
  if not _CheckRight(cSYSR_INSTALL_APP) then exit(edb_ACCESS);
  result := Update(TFRE_DB_Object(appdata));
end;

function TFRE_DB_SYSTEM_CONNECTION.CheckRight(const right_name: TFRE_DB_String): boolean;
begin
  result := _CheckRight(right_name);
end;

function TFRE_DB_SYSTEM_CONNECTION.RemoveApp(const Appname: TFRE_DB_String): TFRE_DB_Errortype;
var  obj     : TFRE_DB_Object;
     appdata : TFRE_DB_APPDATA;
     appi    : IFRE_DB_Object;
     reflist : TFRE_DB_GUIDArray;
     gid     : TGUID;
     i       : integer;
     res     : TFRE_DB_Errortype;
     dobj    : TFRE_DB_Object;
begin
  if not _CheckRight(cSYSR_UNINSTALL_APP) then exit(edb_ACCESS);
  if FSysAppdata.GetIndexedUID(appname,gid) then begin
    if Fetch (gid,obj)=false then begin
      raise EFRE_DB_Exception.Create('Error on Fetching System Object Appdata '+GFRE_BT.GUID_2_HexString(gid));
    end;
    appdata             := obj as TFRE_DB_APPDATA;
    reflist             := appdata.ReferencedByList;
    for i := 0 to high(reflist) do begin
      res := RemoveReferences(reflist[i]);
      if res <> edb_OK then begin
        raise EFRE_DB_Exception.Create('Error on removing references for system object '+GFRE_BT.GUID_2_HexString(reflist[i]));
      end;
      if Fetch(reflist[i],obj)=false then begin
        raise EFRE_DB_Exception.Create('Error on fetching deleteable object '+GFRE_BT.GUID_2_HexString(reflist[i]));
      end;
      obj.Field('appdataid').Clear;
      res := Update(obj);
      if res <> edb_OK then begin
        raise EFRE_DB_Exception.Create('Error on updating deleteable object '+GFRE_BT.GUID_2_HexString(reflist[i]));
      end;
      res := Delete(reflist[i]);
      if res <> edb_OK then begin
        raise EFRE_DB_Exception.Create('Error on deleting system object '+GFRE_BT.GUID_2_HexString(reflist[i]));
      end;
    end;
    result  := Delete(gid);
    if result <> edb_OK then begin
      raise EFRE_DB_Exception.Create(res,'Error on Deleting System Object Appdata '+GFRE_BT.GUID_2_HexString(gid));
    end;
  end else begin
    result := edb_NOT_FOUND;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.GetAppDataID(const Appname: TFRE_DB_String; var appdata_id: TGuid): TFRE_DB_Errortype;
begin
  if FSysAppdata.GetIndexedUID(uppercase(appname),appdata_id) then begin
    result := edb_OK;
  end else begin
    result := edb_NOT_FOUND;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchAppData(const Appname: TFRE_DB_String; var appdata: TFRE_DB_APPDATA): TFRE_DB_Errortype;
begin
  if FSysAppdata.GetIndexedObj(uppercase(appname),TFRE_DB_Object(appdata)) then begin
    result := edb_OK;
  end else begin
    result := edb_NOT_FOUND;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchTranslateableText(const trans_key: TFRE_DB_String; var ttext: TFRE_DB_TEXT): TFRE_DB_Errortype;
begin
  if FSysTransText.GetIndexedObj(uppercase(trans_key),TFRE_DB_Object(ttext)) then begin
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

function TFRE_DB_SYSTEM_CONNECTION.BackupDatabaseReadable(const to_stream: TStream; const stream_cb: TFRE_DB_StreamingCallback;const progress : TFRE_DB_PhaseProgressCallback): TFRE_DB_Errortype;
begin
  result := inherited BackupDatabaseReadable(to_stream,stream_cb,progress);
end;

function TFRE_DB_BASE_CONNECTION.BackupDatabaseReadable(const to_stream: TStream;const stream_cb:TFRE_DB_StreamingCallback;const progress : TFRE_DB_PhaseProgressCallback): TFRE_DB_Errortype;
var CTRL_OBJ:TFRE_DB_Object;
    phase   : integer;
    cnt     : integer;
    max     : integer;
    final   : boolean;
    obj_progress : boolean;

  procedure DWriteln(const msg:TFRE_DB_String);
   var line  : TFRE_DB_String;
       len   : integer;
       lens  : TFRE_DB_String;
   begin
     line := msg+#13#10;
     len  := Length(line);
     lens := IntToStr(len)+'C';
     to_stream.Write(Pointer(@lens[1])^,Length(lens));
     to_stream.Write(Pointer(@line[1])^,Length(line));
   end;

   procedure DumpObj(const obj:TFRE_DB_Object);
   begin
     if obj_progress then inc(cnt);
     DWriteln(obj.GetAsJSONString(false,true,stream_cb));
     abort;
     //if final then obj.FManageInfo.ManageInfo.FinalizeObject(true);
     if Assigned(progress) and obj_progress then progress(phase,cnt,max);
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
     if Assigned(progress) then progress(phase,cnt,max);
   end;

begin
  abort;
  //final := true;
  //CTRL_OBJ := GFRE_DB.NewObject;
  //SetHeader;
  //obj_progress := true;
  //phase := 1 ; max   := FObjectStore.Count; cnt   := 0;
  //if Assigned(progress) then progress(1,0,max);
  //SetNewSection('DBOS',FObjectStore.Count);
  //FObjectStore.ForAllItems(@DumpObj);
  //phase := 2; cnt   := 0; max   := 1;
  //if Assigned(progress) then progress(2,0,1);
  //SetNewSection('REFLINKS',1);
  //final := false;
  //DumpObj(FReferentialLinks);
  //final := true;
  //phase := 3;
  //max   := FCollectionStore.Count-1;
  //cnt   := 0;
  //obj_progress := false;
  //if Assigned(progress) then progress(3,0,max);
  //SetNewSection('COLLECTIONS',FCollectionStore.Count-1); // - 1 x Mastercollection
  //ForAllColls(@DumpCollection);
  //CTRL_OBJ.Free;
end;

function TFRE_DB_BASE_CONNECTION.RestoreDatabaseReadable(const from_stream: TStream; const stream_cb: TFRE_DB_StreamingCallback): TFRE_DB_Errortype; //Unicode Awareness
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
    //writeln(line);
    result := TFRE_DB_Object.CreateFromJSONString(line,self,false,stream_cb);
  end;

  procedure GetSection(const obj:TFRE_DB_Object;var name : TFRE_DB_String;var cnt : integer);
  begin
    name := obj.Field('SECTION').AsString;
    cnt  := obj.Field('COUNT').AsInt64;
  end;

  procedure AddCollection(const obj:TFRE_DB_COLLECTION);
  //var l_ConnManage : TFRE_DB_Collection_ManageInfo;
  begin
    writeln('FIXXIT');
    abort;
    //l_ConnManage := TFRE_DB_Collection_ManageInfo.Create(obj,obj.CollectionName,self);
    //if not FCollectionStore.add(l_ConnManage.Key,l_ConnManage) then raise EFRE_DB_Exception.Create(edb_INTERNAL,'could not add "%s" collection',[obj.CollectionName]);
  end;

begin
  abort;
  //obj := ReadElement;
  //writeln(format('BACKUP ID [%s] VERSION %s.%s(%s) for Database [%s]',[obj.Field('ID').AsString,obj.Field('VMAJ').AsString,obj.Field('VMIN').AsString,obj.Field('VBUILD').AsString,obj.Field('DBNAME').AsString]));
  //GetSection(ReadElement,section_name,count);
  //if section_name<>'DBOS' then GFRE_BT.CriticalAbort('UNEXPECTED SECTION [%s] WANTED "DBOS"',[section_name]);
  //writeln('READING DBOS [',count,']');
  //InternalClearAll(true);
  //for i:=0 to count-1 do begin
  //  obj := ReadElement;
  //  abort; //Add object to Persistant Master Obj Store
  //  //FMasterCollection._InternalAdd(obj.UID,obj).SetObjManInfDirty;
  //end;
  //writeln('MASTER COUNT : ',FObjectStore.Count);
  //GetSection(ReadElement,section_name,count);
  //if section_name<>'REFLINKS' then GFRE_BT.CriticalAbort('UNEXPECTED SECTION [%s] WANTED "REFLINKS"',[section_name]);
  //FReferentialLinks.Free;
  //FReferentialLinks := ReadElement;
  //writeln('READ REFLINKS');
  //GetSection(ReadElement,section_name,count);
  //if section_name<>'COLLECTIONS' then GFRE_BT.CriticalAbort('UNEXPECTED SECTION [%s] WANTED "REFLINKS"',[section_name]);
  //for i:=0 to count-1 do begin
  //  abort;
  //  //obj:=ReadElement;
  //  //AddCollection(obj as TFRE_DB_COLLECTION);
  //end;
  //writeln('READ COLLECTIONS COUNT : ',FCollectionStore.Count);
  //InternalSetupConnection(FDBName='SYSTEM',true);
end;

function TFRE_DB_SYSTEM_CONNECTION.RestoreDatabaseReadable(const from_stream:TStream;const stream_cb:TFRE_DB_StreamingCallback):TFRE_DB_Errortype;
begin
  result := inherited RestoreDatabaseReadable(from_stream,stream_cb);
end;


function TFRE_DB_SYSTEM_CONNECTION._AddUser(const loginatdomain, password, first_name, last_name: TFRE_DB_String): TFRE_DB_Errortype;
var user       : TFRE_DB_USER;
    login      : TFRE_DB_String;
    domain     : TFRE_DB_String;
begin
  if _Userexists(loginatdomain) then exit(edb_EXISTS);
  user := NewObject('TFRE_DB_USER') as TFRE_DB_USER;
  FREDB_SplitLocalatDomain(loginatdomain,login,domain);
  user.InitData(login,first_name,last_name,password);
  user.DomainID := _DomainID(domain);
//  writeln(user.DumpToString());
  result := FSysUsers.Store(TFRE_DB_Object(user));
end;

function TFRE_DB_SYSTEM_CONNECTION._DeleteUser(const loginatdomain: TFRE_DB_String): TFRE_DB_Errortype;
var login      : TFRE_DB_String;
    domain     : TFRE_DB_String;
    domain_id  : TGUID;
begin
 FREDB_SplitLocalatDomain(loginatdomain,login,domain);
 domain_Id := _DomainID(domain);
 if not FSysUsers.RemoveIndexed(TFRE_DB_USER.GetDomainLoginKey(login,domain_id)) then exit(edb_NOT_FOUND);
 result := edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION._DeleteUserById(const user_id: TGuid): TFRE_DB_Errortype;
begin
 if FSysUsers.Remove(user_id) then
   result := edb_OK
 else
   result := edb_ERROR;
end;

function TFRE_DB_SYSTEM_CONNECTION._CheckRight(const right_name: TFRE_DB_String): boolean;
begin
   result := StringInArray(uppercase(right_name),FConnectionRights);
end;

function TFRE_DB_SYSTEM_CONNECTION._CheckLogin(const loginatdomain, pass: TFRE_DB_String): TFRE_DB_Errortype;
var FUser : TFRE_DB_USER;
begin
  FUser := _FetchUser(loginatdomain);
  if not assigned(FUser) then Exit(edb_NOT_FOUND);
  if not FUser.Checkpassword(pass) then exit(edb_ACCESS);
  result := edb_OK;
end;

function TFRE_DB_SYSTEM_CONNECTION._AddDomain(const domainname: TFRE_DB_NameType; const txt, txt_short: TFRE_DB_String): TFRE_DB_Errortype;
var domain      : TFRE_DB_DOMAIN;
begin
  if _DomainExists(domainname) then exit(edb_EXISTS);
  domain    := _NewDomain(domainname,txt,txt_short);

  result := FSysDomains.Store(TFRE_DB_Object(domain));
  _SetupSystemGroupsandRoles(domainname);
  _ReloadUserAndRights;
  FSysDomains.ForceFullUpdateForObservers;
end;

function TFRE_DB_SYSTEM_CONNECTION.NewRightI(const rightname, txt, txt_short: TFRE_DB_String; var right: IFRE_DB_RIGHT): TFRE_DB_Errortype;
var lRight : TFRE_DB_RIGHT;
begin
  result := NewRight(rightname,txt,txt_short,lRight);
  if result = edb_OK then begin
    right := lRight;
  end else begin
    right := nil;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.NewRoleI(const rolename, txt, txt_short: TFRE_DB_String; var right_group: IFRE_DB_ROLE): TFRE_DB_Errortype;
var lRG : TFRE_DB_ROLE;
begin
 result := NewRole(rolename,txt,txt_short,lRG);
  if result = edb_OK then begin
    right_group := lRG;
  end else begin
    right_group := nil;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.NewGroupI(const groupname, txt, txt_short: TFRE_DB_String; var user_group: IFRE_DB_GROUP): TFRE_DB_Errortype;
var lUG : TFRE_DB_GROUP;
begin
 result := NewGroup(groupname,txt,txt_short,lUG);
  if result = edb_OK then begin
    user_group := lUG;
  end else begin
    user_group := nil;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.NewAppDataI(const appname, txt, txt_short: TFRE_DB_String; var appdata: IFRE_DB_APPDATA): TFRE_DB_Errortype;
var lAPP : TFRE_DB_APPDATA;
begin
  result := NewAppData(appname,txt,txt_short,lAPP);
   if result = edb_OK then begin
     appdata := lAPP;
   end else begin
     appdata := nil;
   end;
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreRoleI(const appname: TFRE_DB_String; const domainname: TFRE_DB_NameType; var rg: IFRE_DB_ROLE): TFRE_DB_Errortype;
var lRG : TFRE_DB_ROLE;
begin
  lRG    := rg.Implementor as TFRE_DB_ROLE;
  result := StoreRole(appname,domainname,lRG);
  rg     := nil;
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreGroupI(const appname: TFRE_DB_String;  const domainname:TFRE_DB_NameType; var ug: IFRE_DB_GROUP): TFRE_DB_Errortype;
var lUG : TFRE_DB_GROUP;
begin
 lUG    := ug.Implementor as TFRE_DB_GROUP;
 result := StoreGroup(appname,domainname, lUG);
 ug     := nil;
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreGroupDomainbyIDI(const domain_id: TGUID; var group: IFRE_DB_GROUP): TFRE_DB_Errortype;
var lUG : TFRE_DB_GROUP;
begin
 lUG    := group.Implementor as TFRE_DB_GROUP;
 result := StoreGroupDomainbyID(domain_id, lUG);
 group  := nil;
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreAppDataI(var appdata: IFRE_DB_APPDATA): TFRE_DB_Errortype;
var lAPP : TFRE_DB_APPDATA;
begin
  lAPP    := appdata.Implementor as TFRE_DB_APPDATA;
  result  := StoreAppData(lAPP);
  appdata := nil;
end;

function TFRE_DB_SYSTEM_CONNECTION.StoreTranslateableTextI(const txt: IFRE_DB_TEXT): TFRE_DB_Errortype;
var ttxt : TFRE_DB_TEXT;
begin
  ttxt   := txt.Implementor_HC as TFRE_DB_TEXT;
  result := StoreTranslateableText(ttxt);
end;

function TFRE_DB_SYSTEM_CONNECTION.UpdateAppDataI(var appdata: IFRE_DB_APPDATA): TFRE_DB_Errortype;
var lAPP : TFRE_DB_APPDATA;
begin
  lAPP    := appdata.Implementor as TFRE_DB_APPDATA;
  result  := UpdateAppData(lAPP);
  appdata := nil;
end;

function TFRE_DB_SYSTEM_CONNECTION.FetchAppDataI(const Appname: TFRE_DB_String; var appdata: IFRE_DB_APPDATA): TFRE_DB_Errortype;
var lAPP   : TFRE_DB_APPDATA;
begin
  result  := FetchAppData(appname,lAPP);
  if result = edb_OK then begin
    appdata := lAPP;
  end else begin
    appdata := nil;
  end;
end;

function TFRE_DB_SYSTEM_CONNECTION.GetRoleIDArray(const usergroupids: TFRE_DB_GUIDArray): TFRE_DB_GUIDArray;
var i            : integer;
    lUserGroup   : TFRE_DB_GROUP;
    lRoleIDs     : TFRE_DB_ObjLinkArray;
begin
  for i:=0 to high(UserGroupIDs) do begin
    if not _FetchGroupbyID(UserGroupIDs[i],lUserGroup) then begin
      raise EFRE_DB_Exception.Create('Could not fetch group by id '+GFRE_BT.GUID_2_HexString(UserGroupIDs[i]));
    end else begin
      ConcatGuidArrays(lRoleIDs,lUserGroup.RoleIDs);
    end;
  end;
  result := lRoleIDs;
end;

function TFRE_DB_SYSTEM_CONNECTION.GetRightsArrayForRoles(const roleids: TFRE_DB_GUIDArray): TFRE_DB_StringArray;
var i            : integer;
    lRole        : TFRE_DB_ROLE;
    lAllRights   : TFRE_DB_StringArray;
begin
  for i:=0 to high(roleids) do begin
    if not _FetchRolebyID(roleids[i],lRole) then begin
      raise EFRE_DB_Exception.Create('Could not fetch role by id '+GFRE_BT.GUID_2_HexString(roleids[i]));
    end else begin
      ConcatStringArrays(lAllRights,lRole.GetRightNames);
    end;
  end;
  result := lAllRights;
end;

function TFRE_DB_SYSTEM_CONNECTION.GetRightsArrayForGroups(const usergroupids: TFRE_DB_GUIDArray): TFRE_DB_StringArray;
begin
  result := GetRightsArrayForRoles(GetRoleIDArray(usergroupids));
end;

function TFRE_DB_SYSTEM_CONNECTION.CheckRightForGroup(const right_name: TFRE_DB_String; const group_uid: TGuid): boolean;
var ra : TFRE_DB_StringArray;
begin
  ra     := GetRightsArrayForGroups(TFRE_DB_GUIDArray.CreatE(group_uid));
  result := StringInArray(uppercase(right_name),ra);
end;


constructor TFRE_DB_SYSTEM_CONNECTION.CreateCloned(const from: TFRE_DB_SYSTEM_CONNECTION);
begin
  inherited CreateCloned(from);
  FSysUsers            := from.FSysUsers;
  FSysRoles            := from.FSysRoles;
  FSysGroups           := from.FSysGroups;
  FSysDomains          := from.FSysDomains;
  FSysUserSessionsData := from.FSysUserSessionsData;
  FSysTransText        := from.FSysTransText;
  FSysAppdata          := from.FSysAppdata;
end;


function TFRE_DB_SYSTEM_CONNECTION.GetScheme(const scheme_name: TFRE_DB_NameType; var scheme: TFRE_DB_SchemeObject): boolean;
begin
  Result:=inherited GetScheme(scheme_name, scheme);
end;



function TFRE_DB_BASE_CONNECTION.FetchApplications(var apps:TFRE_DB_APPLICATION_ARRAY): TFRE_DB_Errortype;
var cnt:Int64=0;

begin
  SetLength(apps,0);
  _ConnectCheck;
  //oldlen :=  length(apps);
  //cnt    := oldlen;
  //SetLength(apps,GFRE_DB.GetApps.ItemCount+oldlen);
  //FApps.ForAll(@BuildArray);
  //SetLength(apps,cnt);
  apps := GFRE_DB.GetApps;
  result := edb_OK;
end;

function TFRE_DB_BASE_CONNECTION.FetchApplicationsI(var apps: IFRE_DB_APPLICATION_ARRAY): TFRE_DB_Errortype;
var appa :TFRE_DB_APPLICATION_ARRAY;
  i: Integer;
begin
  result := FetchApplications(appa);
  if result = edb_OK then begin
    setlength(apps,length(appa));
    for i:=0 to high(apps) do begin
      apps[i] := appa[i];
    end;
  end;
end;


function TFRE_DB_BASE_CONNECTION.ApplicationByName(const name: TFRE_DB_String;var app: IFRE_DB_APPLICATION):boolean;
var lapp:TFRE_DB_APPLICATION;
begin
  if GFRE_DB.GetApp(name,lapp) then begin
    result := true;
    app := lapp;
  end else begin
    result := false;
    app    := nil;
  end;
end;

//procedure TFRE_DB_BASE_CONNECTION.DrawScheme(const datastream: TStream);
//var  dbgraph : TFRE_DB_GRAPH;
//
//  procedure nestedCollectionIterator(const coll: IFRE_DB_Collection);
//  begin
//    dbgraph.CollectionIterator(coll);
//  end;
//
//  procedure nestedEmbeddedIterator(const obj:IFRE_DB_SCHEMEOBJECT);
//  begin
//    dbgraph.EmbeddedIterator(obj);
//  end;
//
//  procedure nestedSchemeIterator(const obj:IFRE_DB_SCHEMEOBJECT);
//  begin
//    dbgraph.SchemeIterator(obj);
//  end;
//
//  procedure nestedObjectIterator(const obj:IFRE_DB_OBJECT);
//  begin
//    dbgraph.ObjectIterator(obj,self);
//  end;
//
//begin
//  writeln('DrawScheme');
//  dbgraph        :=        TFRE_DB_GRAPH.Create;
//  try
//    dbgraph.PlotStart;
//    self.ForAllCollsI(@nestedCollectionIterator);
//    self.ForAllSchemesI(@nestedEmbeddedIterator);
//    self.ForAllSchemesI(@nestedSchemeIterator);
//    self.ForAllObjectsI(@nestedObjectIterator);
//    dbgraph.PlotEnd;
//    dbgraph.PlotScheme(datastream);
//  finally
//    dbgraph.Free;
//  end;
//end;

function TFRE_DB_SYSTEM_CONNECTION.Connect(const loginatdomain: TFRE_DB_String; const password: TFRE_DB_String): TFRE_DB_Errortype;
begin
  result := Connect(loginatdomain,password,false);
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.SetCustomTransformFunction(const func: IFRE_DB_CUSTOMTRANSFORM);
begin
  FCustTransform := func;
end;

function TFRE_DB_SIMPLE_TRANSFORM.TransformInOut(const dependency_obj: IFRE_DB_Object; const input: TFRE_DB_Object; const filter_fields: boolean): TFRE_DB_Object;
var cnt : integer;
    i   : Integer;
    transform_object : TFRE_DB_Object;
    typ              : TFRE_DB_String;

    procedure _Collector;
    var outfieldname : TFRE_DB_String;
        fieldlist : TFRE_DB_StringArray;
        formats   : TFRE_DB_String;
        //test      : array of TVarRec;
        //sa        : array of TFRE_DB_String;
        //i         : integer;
        //fld       : TFRE_DB_FIELD;
        //res       : TFRE_DB_String;
        filter_field : boolean;
    begin
      filter_field := transform_object.Field('FF').AsBoolean;
      if filter_field <> filter_fields then exit;
      outfieldname := transform_object.Field('OF').AsString;
      fieldlist    := transform_object.Field('IL').AsStringArr;
      formats      := transform_object.Field('F').AsString;

      result.field(outfieldname).Asstring := input.FieldPathListFormat(fieldlist,formats,'');

      //setlength(test,length(fieldlist));
      //setlength(sa,length(fieldlist));
      //for i:=0 to high(fieldlist) do begin
      //  fld := input.FieldPath(fieldlist[i]);
      //  if assigned(fld) then begin
      //    sa[i] := fld.AsString;
      //    test[i].VAnsiString := PAnsiString(sa[i]);
      //    test[i].VType   := vtAnsiString;
      //  end else begin
      //   sa[i] := '';
      //   test[i].VAnsiString := PAnsiString(sa[i]);
      //   test[i].VType   := vtAnsiString;
      //  end;
      //end;
      //res :=  Format(formats,test);
      //result.field(outfieldname).Asstring := res;
    end;

    procedure _OneOne;
    var fieldname : TFRE_DB_String;
        outfname  : TFRE_DB_String;
        sa        : TFRE_DB_StringArray;
        i         : nativeint;
    begin
      if filter_fields then exit;
      fieldname := transform_object.Field('F').AsString;
      outfname  := transform_object.Field('OF').AsString;
      if outfname='' then outfname:=fieldname;
      result.field(uppercase(outfname)).CloneFromField(input.Field(fieldname));
      if  transform_object.Field('GDT').AsString='ICO' then
        begin
          SetLength(sa,0);
          FREDB_SeperateString(result.field(uppercase(outfname)).asstring,',',sa);
          for i:=0 to high(sa) do
            sa[i] := getThemedResource(sa[i]);
          result.field(uppercase(outfname)).asstring := FREDB_CombineString(sa,',');
        end;
    end;

    procedure _Constant;
    var outfname  : TFRE_DB_String;
    begin
      if filter_fields then exit;
      outfname  := transform_object.Field('OF').AsString;
      result.field(uppercase(outfname)).AsString:=transform_object.Field('VAL').AsString;
    end;

    procedure _Progress;
    var fieldname : TFRE_DB_String;
        outfname  : TFRE_DB_String;
    begin
      if filter_fields then exit;
      fieldname := transform_object.Field('F').AsString;
      outfname  := transform_object.Field('OF').AsString;
      if outfname='' then outfname:=fieldname;
      result.field(uppercase(outfname)).CloneFromField(input.Field(fieldname));
      if transform_object.Field('TF').AsString<>'' then begin
        fieldname := transform_object.Field('TF').AsString;
        outfname  := transform_object.Field('OTF').AsString;
        if outfname='' then outfname:=fieldname;
        result.field(uppercase(outfname)).CloneFromField(input.Field(fieldname));
      end;
    end;

    procedure _OneOneMatch;
    var fieldname : TFRE_DB_String;
        outfname  : TFRE_DB_String;
        ref_chain : TFRE_DB_StringArray;
        obj       : IFRE_DB_Object;
        objo      : TFRE_DB_Object;
        ref_uid   : TGuid;
        i         : integer;
        s         : string;
    begin
      if filter_fields then exit;
      fieldname   := transform_object.Field('F').AsString;
      outfname    := transform_object.Field('OF').AsString;
      ref_chain   := transform_object.Field('RFC').AsStringArr;
      if outfname = '' then outfname:=fieldname;
      obj         := input;
      for i:=0 to high(ref_chain) do begin
        if not obj.FieldExists(ref_chain[i]) or (obj.Field(ref_chain[i]).FieldType<>fdbft_ObjLink) then begin
          result.field(uppercase(outfname)).asstring := '?*MATCHFIELD TYPE';
          exit;
        end;
        ref_uid := obj.Field(ref_chain[i]).AsGUID;
        if not _DBConnection.FetchInternal(ref_uid,objo) then begin
          result.field(uppercase(outfname)).asstring := '?*UNRESOLVED LINK*';
          exit;
        end;
        obj := objo;
      end;
      objo.Assert_CheckStoreLocked;
      try
        objo.Set_Store_Locked(false);
        if obj.FieldExists(fieldname) then begin
          result.field(uppercase(outfname)).CloneFromFieldI(obj.Field(fieldname));
        end else begin
          result.field(uppercase(outfname)).asstring := '?*TARGETFIELD NOT FOUND*';
        end;
      finally
        objo.Set_Store_Locked(true);
      end;
    end;


    procedure _DBText(const what:integer);
    var fieldname : TFRE_DB_String;
        outfname  : TFRE_DB_String;
        text      : TFRE_DB_TEXT;
        out_text  : TFRE_DB_String;
    begin
      if filter_fields then exit;
      fieldname := transform_object.Field('F').AsString;
      text := input.Field(fieldname).AsObject as TFRE_DB_TEXT;
      case what of
        0 :  out_text := text.GetLong;
        1 :  out_text := text.Getshort;
        2 :  out_text := text.GetHint;
        3 :  out_text := text.GetTKey;
      end;
      outfname  := transform_object.Field('OF').AsString;
      if outfname='' then outfname:=fieldname;
      result.field(uppercase(outfname)).asstring := out_text;
    end;

    procedure _DumpString;
    var fieldname:TFRE_DB_String;
        filter_field:Boolean;
    begin
      filter_field := transform_object.Field('FF').AsBoolean;
      if filter_field <> filter_fields then exit;
      fieldname := transform_object.Field('F').AsString;
      if input.FormattedDisplayAvailable then begin
        result.field(uppercase(fieldname)).asstring := input.GetFormattedDisplay;
      end else begin
        result.field(uppercase(fieldname)).asstring := input.DumpToString(0,transform_object.Field('L').AsInt16);
      end;
    end;

begin
  result := GFRE_DB.NewObject;
  cnt    := Field('TRANS').ValueCount;
  for i:= 0 to cnt-1 do begin
    transform_object := Field('TRANS').AsObjectArr[i];
    typ              := transform_object.Field('T').AsString;
    case typ of
      'CS'   : _Collector;
      'OO'   : _OneOne;
      'PRG'  : _Progress;
      'CONST': _Constant;
      'DS'   : _DumpString;
      'DBTS' : _DBText(1);
      'DBTL' : _DBText(0);
      'DBTH' : _DBText(2);
      'DBTK' : _DBText(3);
      'OORF' : _OneOneMatch;
      else raise EFRE_DB_Exception.Create(edb_INTERNAL,'UNKNOWN SIMPLE TRANSFORM TYPE [%s]',[typ]);
    end;
  end;
  result._Field('uid').AsGUID := input.Field('uid').AsGUID;
  if Assigned(FCustTransform)
     and (not filter_fields) then
    FCustTransform(GetDBConnection,dependency_obj,input,result);
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddCollectorscheme(const format: TFRE_DB_String; const in_fieldlist: TFRE_DB_StringArray; const out_field: TFRE_DB_String; const filter_field: boolean; const output_title: TFRE_DB_String;const gui_display_type:TFRE_DB_DISPLAY_TYPE;const fieldSize: Integer);
var obj:TFRE_DB_Object;
begin
  obj := GFRE_DB.NewObject;
  obj.Field('T').AsString     := 'CS';
  obj.Field('F').AsString     := format;
  obj.Field('IL').AsStringArr := in_fieldlist;
  obj.Field('OF').AsString    := lowercase(out_field);
  obj.Field('FF').AsBoolean   := filter_field;
  obj.Field('OPT').AsString   := output_title;
  obj.Field('GDT').AsString   := CFRE_DB_DISPLAY_TYPE[gui_display_type];
  obj.Field('FS').AsInt16     := fieldSize;
  Field('TRANS').AddObject(obj);
  if filter_field then begin
    _SetHasFilterField(true);
  end;
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddFulltextFilterOnTransformed(const in_fieldlist: TFRE_DB_StringArray);
begin
  abort;
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddOneToOnescheme(const fieldname: TFRE_DB_String; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE; const fieldSize: Integer; const iconID: String);
var obj:TFRE_DB_Object;
begin
  obj                         := GFRE_DB.NewObject;
  obj.Field('T').Asstring     := 'OO';
  obj.Field('F').AsString     := fieldname;
  obj.Field('OF').AsString    := lowercase(out_field);
  obj.Field('OPT').AsString   := output_title;
  obj.Field('FF').AsBoolean   := false;
  obj.Field('GDT').AsString   := CFRE_DB_DISPLAY_TYPE[gui_display_type];
  obj.Field('FS').AsInt16     := fieldSize;
  obj.Field('IC').AsString    := iconID;
  Field('TRANS').AddObject(obj);
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddProgressTransform(const valuefield: TFRE_DB_String; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const textfield: TFRE_DB_String; const out_text: TFRE_DB_String; const maxValue: Single;const fieldSize: Integer);
var obj:TFRE_DB_Object;
begin
  obj                         := GFRE_DB.NewObject;
  obj.Field('T').Asstring     := 'PRG';
  obj.Field('F').AsString     := valuefield;
  obj.Field('OF').AsString    := lowercase(out_field);
  obj.Field('OPT').AsString   := output_title;
  obj.Field('TF').AsString    := textfield;
  obj.Field('OTF').AsString   := lowercase(out_text);
  obj.Field('MAX').AsReal32   := maxValue;
  obj.Field('FF').AsBoolean   := false;
  obj.Field('GDT').AsString   := CFRE_DB_DISPLAY_TYPE[dt_number_pb];
  obj.Field('FS').AsInt16     := fieldSize;
  Field('TRANS').AddObject(obj);
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddConstString(const out_field,value: TFRE_DB_String; const display: Boolean; const output_title: TFRE_DB_String;  const gui_display_type: TFRE_DB_DISPLAY_TYPE;const fieldSize: Integer);
var obj:TFRE_DB_Object;
begin
 obj                         := GFRE_DB.NewObject;
 obj.Field('T').Asstring     := 'CONST';
 obj.Field('OF').AsString    := lowercase(out_field);
 obj.Field('OPT').AsString   := output_title;
 obj.Field('VAL').AsString   := value;
 obj.Field('DISP').AsBoolean :=display;
 obj.Field('FF').AsBoolean   := false;
 obj.Field('GDT').AsString   := CFRE_DB_DISPLAY_TYPE[gui_display_type];
 obj.Field('FS').AsInt16     := fieldSize;
 Field('TRANS').AddObject(obj);
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddDBTextShortToOne(const fieldname: TFRE_DB_String; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE;const fieldSize: Integer);
var obj:TFRE_DB_Object;
begin
  obj                         := GFRE_DB.NewObject;
  obj.Field('T').Asstring     := 'DBTS';
  obj.Field('F').AsString     := fieldname;
  obj.Field('OF').AsString    := lowercase(out_field);
  obj.Field('OPT').AsString   := output_title;
  obj.Field('FF').AsBoolean   := false;
  obj.Field('GDT').AsString   := CFRE_DB_DISPLAY_TYPE[gui_display_type];
  obj.Field('FS').AsInt16     := fieldSize;
  Field('TRANS').AddObject(obj);
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddDBTextLongToOne(const fieldname: TFRE_DB_String; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE;const fieldSize: Integer);
var obj:TFRE_DB_Object;
begin
  obj                         := GFRE_DB.NewObject;
  obj.Field('T').Asstring     := 'DBTL';
  obj.Field('F').AsString     := fieldname;
  obj.Field('OF').AsString    := lowercase(out_field);
  obj.Field('OPT').AsString   := output_title;
  obj.Field('FF').AsBoolean   := false;
  obj.Field('GDT').AsString   := CFRE_DB_DISPLAY_TYPE[gui_display_type];
  obj.Field('FS').AsInt16     := fieldSize;
  Field('TRANS').AddObject(obj);
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddDBTextHintToOne(const fieldname: TFRE_DB_String; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE;const fieldSize: Integer);
var obj:TFRE_DB_Object;
begin
  obj                         := GFRE_DB.NewObject;
  obj.Field('T').Asstring     := 'DBTH';
  obj.Field('F').AsString     := fieldname;
  obj.Field('OF').AsString    := lowercase(out_field);
  obj.Field('OPT').AsString   := output_title;
  obj.Field('FF').AsBoolean   := false;
  obj.Field('GDT').AsString   := CFRE_DB_DISPLAY_TYPE[gui_display_type];
  obj.Field('FS').AsInt16     := fieldSize;
  Field('TRANS').AddObject(obj);
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddDBTextKeyToOne(const fieldname: TFRE_DB_String; const out_field: TFRE_DB_String; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE;const fieldSize: Integer);
var obj:TFRE_DB_Object;
begin
  obj                         := GFRE_DB.NewObject;
  obj.Field('T').Asstring     := 'DBTK';
  obj.Field('F').AsString     := fieldname;
  obj.Field('OF').AsString    := lowercase(out_field);
  obj.Field('OPT').AsString   := output_title;
  obj.Field('FF').AsBoolean   := false;
  obj.Field('GDT').AsString   := CFRE_DB_DISPLAY_TYPE[gui_display_type];
  obj.Field('FS').AsInt16     := fieldSize;
  Field('TRANS').AddObject(obj);
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddFullDumpField(const fieldname: TFRE_DB_String; const dump_length_max: Integer; const filter_field: Boolean; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE;const fieldSize: Integer);
var obj:TFRE_DB_Object;
begin
  obj                         := GFRE_DB.NewObject;
  obj.Field('T').Asstring     := 'DS';
  obj.Field('F').AsString     := fieldname;
  obj.Field('L').AsInt16      := dump_length_max;
  obj.Field('FF').AsBoolean   := filter_field;
  obj.Field('OPT').AsString   := output_title;
  obj.Field('GDT').AsString   := CFRE_DB_DISPLAY_TYPE[gui_display_type];
  if filter_field then begin
    _SetHasFilterField(true);
  end;
  obj.Field('FS').AsInt16     := fieldSize;
  Field('TRANS').AddObject(obj);
end;

function TFRE_DB_SIMPLE_TRANSFORM.GetViewCollectionDescription: TFRE_DB_CONTENT_DESC;
var cnt,i            : Integer;
    transform_object : TFRE_DB_Object;
    typ              : TFRE_DB_String;
    vcd              : TFRE_DB_VIEW_LIST_LAYOUT_DESC;
    filter_field     : boolean;

    procedure _AddEntry;
    var out_title : string;
        out_field : string;
        in_field  : string;
        gui_dt    : TFRE_DB_DISPLAY_TYPE;
        gdt       : string;
        fs        : Integer;
        icon      : string;

    begin
      out_title := transform_object.Field('OPT').AsString;
      out_field := transform_object.Field('OF').AsString;
      in_field  := transform_object.Field('F').AsString;
      gdt       := transform_object.Field('GDT').AsString;
      fs        := transform_object.Field('FS').AsInt16;
      icon      := transform_object.Field('IC').AsString;
      if gdt<>'' then begin
        gui_dt    := FREDB_String2DBDisplayType(gdt);
      end else begin
        gui_dt := dt_string;
      end;
      if out_field='' then out_field:=in_field;
      vcd.AddDataElement.Describe(lowercase(out_field),out_title,gui_dt,fs,true,false,icon);
    end;

    procedure _AddPrgEntry;
    var out_title : string;
        out_field : string;
        out_label : String;
        in_field  : string;
        in_label  : String;
        gui_dt    : TFRE_DB_DISPLAY_TYPE;
        gdt       : string;
        maxVal    : Single;
        fs        : Integer;

    begin
      out_title := transform_object.Field('OPT').AsString;
      out_field := transform_object.Field('OF').AsString;
      out_label := transform_object.Field('OTF').AsString;
      in_field  := transform_object.Field('F').AsString;
      in_label  := transform_object.Field('TF').AsString;
      gdt       := transform_object.Field('GDT').AsString;
      maxVal    := transform_object.Field('MAX').AsReal32;
      fs        := transform_object.Field('FS').AsInt16;
      if gdt<>'' then begin
        gui_dt    := FREDB_String2DBDisplayType(gdt);
      end else begin
        gui_dt := dt_string;
      end;
      if out_field='' then out_field:=in_field;
      if out_label='' then out_label:=in_label;
      vcd.AddDataElement.DescribePB(lowercase(out_field),out_title,out_label,maxVal,fs);
    end;

    procedure _AddConstEntry;
    var out_title : string;
        out_field : string;
        out_label : String;
        gdt       : String;
        display   : Boolean;
        gui_dt    : TFRE_DB_DISPLAY_TYPE;
        fs        : Integer;

    begin
      out_title := transform_object.Field('OPT').AsString;
      out_field := transform_object.Field('OF').AsString;
      gdt       := transform_object.Field('GDT').AsString;
      display   := transform_object.Field('DISP').AsBoolean;
      gui_dt    := FREDB_String2DBDisplayType(gdt);
      fs        := transform_object.Field('FS').AsInt16;

      vcd.AddDataElement.Describe(lowercase(out_field),out_title,gui_dt,fs,display);
    end;

begin
  vcd := TFRE_DB_VIEW_LIST_LAYOUT_DESC.create.Describe();
  cnt    := Field('TRANS').ValueCount;
  for i:= 0 to cnt-1 do begin
    transform_object := Field('TRANS').AsObjectArr[i];
    typ              := transform_object.Field('T').AsString;
    filter_field     := transform_object.Field('FF').AsBoolean;
    if filter_field then continue;
    case typ of
      'CS'   : _AddEntry;
      'OO'   : _AddEntry;
      'PRG'  : _AddPrgEntry;
      'CONST': _AddConstEntry;
      'DS'   : _AddEntry;
      'DBTS' : _AddEntry;
      'DBTL' : _AddEntry;
      'DBTH' : _AddEntry;
      'DBTK' : _AddEntry;
      'OORF' : _AddEntry;
      else raise EFRE_DB_Exception.Create(edb_INTERNAL,'UNKNOWN SIMPLE TRANSFORM TYPE [%s]',[typ]);
    end;
  end;
  vcd.AddDataElement.Describe('uid','UID',dt_string,1,false);
  result := vcd;
end;


procedure TFRE_DB_SIMPLE_TRANSFORM.AddMatchingReferencedField(const ref_field_chain: TFRE_DB_StringArray; const target_field: TFRE_DB_String; const output_field: TFRE_DB_String; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE;const fieldSize: Integer);
var obj:TFRE_DB_Object;
begin
   obj                          := GFRE_DB.NewObject;
   obj.Field('T').Asstring      := 'OORF';
   obj.Field('F').AsString      := target_field;
   obj.Field('OF').AsString     := lowercase(output_field);
   obj.Field('OPT').AsString    := output_title;
   obj.Field('RFC').AsStringArr := ref_field_chain;
   obj.Field('FF').AsBoolean    := false;
   obj.Field('GDT').AsString    := CFRE_DB_DISPLAY_TYPE[gui_display_type];
   obj.Field('FS').AsInt16     := fieldSize;
   Field('TRANS').AddObject(obj);
end;

procedure TFRE_DB_SIMPLE_TRANSFORM.AddMatchingReferencedField(const ref_field: TFRE_DB_String; const target_field: TFRE_DB_String; const output_field: TFRE_DB_String; const output_title: TFRE_DB_String; const gui_display_type: TFRE_DB_DISPLAY_TYPE;const fieldSize: Integer);
begin
  AddMatchingReferencedField(TFRE_DB_StringArray.Create(ref_field),target_field,output_field,output_title,gui_display_type,fieldSize);
end;

procedure TFRE_DB_DERIVED_COLLECTION._CheckSetDisplayType(const CollectionDisplayType: TFRE_COLLECTION_DISPLAY_TYPE);
begin
  if CollectionDisplayType=FDisplaytype then
    exit;
  if FDisplaytype<>cdt_Invalid then raise EFRE_DB_Exception.Create(edb_ERROR,'Collectiondisplaytype is already set');
  FDisplaytype := CollectionDisplayType;
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
  FTreeNodeCaption       := nil;
  FTreeNodeIconField     := '';
end;

procedure TFRE_DB_DERIVED_COLLECTION.ICO_CollectionNotify(const notify_type: TFRE_DB_NotifyObserverType; const obj: TFRE_DB_Object; const obj_uid: TGUID);
var not_object : TFRE_DB_Object;
  procedure DeleteRecord;
  var res    : TFRE_DB_UPDATE_STORE_DESC;
      entry  : IFRE_DB_Object;
  begin
   res:=TFRE_DB_UPDATE_STORE_DESC.create.Describe(CollectionName);
   res.addDeletedEntry(GFRE_BT.GUID_2_HexString(obj_uid));
   Fsession.SendServerClientRequest(res);
  end;

begin
  case notify_type of
    fdbntf_START_UPDATING : begin
      BeginUpdateGathering;
    end;
    fdbntf_ENDUPDATE_APPLY: begin
      FinishUpdateGathering(true);
    end;
    fdbntf_INSERT: begin
        not_object := obj;
        if not assigned(not_object) then
          _DBConnection.FetchInternal(obj_uid,not_object);
        if assigned(not_object) then
          begin
            not_object.Assert_CheckStoreLocked;
            try
              not_object.Set_Store_Locked(false);
              _AddToTransformedCollection(not_object,true);
            finally
              not_object.Set_Store_Locked(true);
            end;
          end;
    end;
    fdbntf_UPDATE: begin
        if _CheckUIDExists(obj_uid) then begin
          not_object := obj;
          if not assigned(not_object) then
            _DBConnection.FetchInternal(obj_uid,not_object);
          if assigned(not_object) then
            begin
              not_object.Assert_CheckStoreLocked;
              try
                not_object.Set_Store_Locked(false);
                //writeln('MANUAL UPDATE IN COLL ',CollectionName);
                //writeln(not_object.DumpToString());
                _AddToTransformedCollection(not_object,true,true);
              finally
                not_object.Set_Store_Locked(true);
              end;
            end;
        end;
    end;
    fdbntf_DELETE: begin
      if _CheckUIDExists(obj_uid) then begin
        //writeln('---');
        //writeln('DELETE NOTIFY ',CollectionName);
        //writeln('OUID : ',GFRE_BT.GUID_2_HexString(obj_uid));
        if assigned(FSession) then begin
          //writeln('FOUND -> ALERTING SESSION [',FSession.GetSessionID,']');
          DeleteRecord;
        end;
        //writeln('---');
      end;
    end;
    fdbntf_COLLECTION_RELOAD: begin
      FInitialDerived := false;
      Fsession.SendServerClientRequest(TFRE_DB_REFRESH_STORE_DESC.create.Describe(CollectionName));
    end;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.ICO_ObserverID: String;
begin
  result := CollectionName;
end;

procedure TFRE_DB_DERIVED_COLLECTION.BeginUpdateGathering;
begin
  if assigned(FGatherUpdateList) then raise EFRE_DB_Exception.Create(edb_ERROR,'UPDATE GATHERING ALREADY STARTED');
  FGatherUpdateList := TFRE_DB_UPDATE_STORE_DESC.create.Describe(CollectionName);
end;

procedure TFRE_DB_DERIVED_COLLECTION.FinishUpdateGathering(const sendupdates: Boolean);
begin
  if sendupdates then begin
    //writeln('--------- SEND CLIENT UPDATE/NEW DESCR ------- ',CollectionName);
    FSession.SendServerClientRequest(FGatherUpdateList);
  end else begin
    FGatherUpdateList.Free;
  end;
  FGatherUpdateList:=nil;
end;

procedure TFRE_DB_DERIVED_COLLECTION._AddToTransformedCollection(item: TFRE_DB_Object; const send_client_notify: boolean; const update_data: boolean; const child_call: boolean);
var i                 : Integer;
    iob               : TFRE_DB_Object;
    tmp_ob            : TFRE_DB_Object;
    add               : boolean;
    filtval           : TFRE_DB_String;
    fieldval          : TFRE_DB_String;
    tr_obj            : TFRE_DB_Object;
    target_field_type : TFRE_DB_FIELDTYPE;
    use_filter_fields : boolean;
    isFirst           : Boolean;

    function Filter(const fob_f:TFRE_DB_FIELD):boolean;
    var fob             : TFRE_DB_Object;
        fieldname       : TFRE_DB_String;
        filter_fld_type : TFRE_DB_FIELDTYPE;
        str_filtertype  : TFRE_DB_STR_FILTERTYPE;
        filterkey       : string;

        procedure GenericNumericFilter;
        var num_filt_typ    : TFRE_DB_NUM_FILTERTYPE;
            guid_filt_vals  : TFRE_DB_GUIDArray;
            guid_field_vals : TFRE_DB_GUIDArray;
            bool_field_vals : TFRE_DB_BoolArray;
            bool_filt_vals  : TFRE_DB_BoolArray;
            i,j             : integer;

            procedure _FieldTypeCheck;
            begin
              if target_field_type<>filter_fld_type then raise EFRE_DB_Exception.Create(edb_ERROR,'NUMERICFILTER REQUESTED FILTER ON A [%s] FIELD BUT TARGETFIELD [%s] IS OF TYPE [%s]',[CFRE_DB_FIELDTYPE[filter_fld_type],fieldname,CFRE_DB_FIELDTYPE[target_field_type]]);
            end;

        begin
          add := true;
          num_filt_typ := FREDB_String2NumfilterType(fob.Field('FT').AsString);
          case filter_fld_type of
            fdbft_GUID: begin
                           if (target_field_type<>fdbft_GUID) and (target_field_type<>fdbft_ObjLink) then raise EFRE_DB_Exception.Create(edb_ERROR,'NUMERICFILTER REQUESTED FILTER ON A [%s] FIELD BUT TARGETFIELD [%s] IS OF TYPE [%s]',[CFRE_DB_FIELDTYPE[filter_fld_type],fieldname,CFRE_DB_FIELDTYPE[target_field_type]]);
                           guid_filt_vals  := fob.Field('FV').AsGUIDArr;
                           if target_field_type=fdbft_ObjLink then begin
                             guid_field_vals := iob.Field(fieldname).AsObjectLinkArray;
                           end else begin
                             guid_field_vals := iob.Field(fieldname).AsGUIDArr;
                           end;
                           //writeln('UIDFIELD: ',fieldname,'KEY: ',filterkey,' FILTER: ',FREDB_GuidArray2String(guid_filt_vals),' FIELD :',FREDB_GuidArray2String(guid_field_vals));
                           case num_filt_typ of
                             dbnf_OneValueFromFilter: begin
                                                        add := false;
                                                        for i:=0 to high(guid_field_vals) do begin
                                                          for j:=0 to high(guid_filt_vals) do begin
                                                            if FREDB_Guids_Same(guid_field_vals[i],guid_filt_vals[j]) then begin
                                                              add := true;
                                                              exit;
                                                            end;
                                                          end;
                                                        end;
                                                      end;
                             dbnf_NoValueInFilter:    begin
                                                        add := true;
                                                        if length(guid_filt_vals)=0 then
                                                          exit;
                                                        for i:=0 to high(guid_field_vals) do begin
                                                          for j:=0 to high(guid_filt_vals) do begin
                                                            if FREDB_Guids_Same(guid_field_vals[i],guid_filt_vals[j]) then begin
                                                              add := false;
                                                              exit;
                                                            end;
                                                          end;
                                                        end;
                                                      end;
                             dbnf_EXACT: begin
                                           add := true;
                                           if Length(guid_field_vals) <> Length(guid_filt_vals) then begin
                                             add:=false;
                                             exit;
                                           end else begin
                                             for i:=0 to high(guid_filt_vals) do begin
                                               if not FREDB_Guids_Same(guid_field_vals[i],guid_filt_vals[i]) then begin
                                                 add:=false;
                                                 exit;
                                               end;
                                             end;
                                           end;
                                         end;
                             else raise EFRE_DB_Exception.Create(edb_INTERNAL,'UNSUPPORTED NUM FILTER TYPE ON GUID FIELD');
                           end;
                        end;
            fdbft_Byte: ;
            fdbft_Int16: ;
            fdbft_UInt16: ;
            fdbft_Int32: ;
            fdbft_UInt32: ;
            fdbft_Int64: ;
            fdbft_UInt64: ;
            fdbft_Real32: ;
            fdbft_Real64: ;
            fdbft_Currency: ;
            fdbft_Boolean:
                           begin
                               case num_filt_typ of
                                 dbnf_EXACT,
                                 dbnf_EXACT_NEGATED:
                                                begin
                                                  add := true;
                                                  bool_filt_vals  := fob.Field('FV').AsBooleanArr;
                                                  bool_field_vals := iob.Field(fieldname).AsBooleanArr;
                                                  if Length(bool_filt_vals)<>Length(bool_field_vals) then
                                                    add := false;
                                                  if add then
                                                    for i:=0 to high(bool_filt_vals) do begin
                                                      if not (bool_field_vals[i]=bool_filt_vals[i]) then begin
                                                        add:=false;
                                                        break;
                                                      end;
                                                    end;
                                                  if num_filt_typ=dbnf_EXACT_NEGATED then
                                                    add := not add;
                                                end;
                                   else raise EFRE_DB_Exception.Create(edb_INTERNAL,'UNSUPPORTED NUM FILTER TYPE ON BOOLEAN FIELD');
                               end;
                           end;
            fdbft_DateTimeUTC: ;
          end;
        end;

        procedure StringFieldFilter;  //TODO:Array Filter
        var filtval : TFRE_DB_String;
        begin
          add := true;
          filtval         := fob.Field('FV').AsString;
          str_filtertype  := FREDB_String2StrFilterType(fob.Field('FT').AsString);
          fieldval   := iob.Field(fieldname).AsString;
          case str_filtertype of
            dbft_EXACT:      add := AnsiContainsText(fieldval,filtval) and (length(fieldval)=length(filtval));
            dbft_PART:       add := AnsiContainsText(fieldval,filtval);
            dbft_STARTPART:  add := AnsiStartsText(fieldval,filtval);
            dbft_ENDPART:    add := AnsiEndsText(fieldval,filtval);
          end;
        end;

        procedure SchemeFilter;
        var filtval : String;
            negate  : boolean;
        begin
          add     := true;
          filtval := fob.Field('FV').AsString;
          negate  := fob.Field('NEG').AsBoolean;
          add     := uppercase(iob.SchemeClass)=uppercase(filtval);
          if negate then add := not add;
        end;

        procedure RightFilter;
        var right          : String;
            fieldnameforid : TFRE_DB_NameType;
        begin
          right          := fob.Field('FV').AsString;
          fieldnameforid := fob.Field('FN').AsString;
          if fieldnameforid='' then
            begin
              right := FREDB_Get_Rightname_UID(right,iob.UID);
              add   := _DBConnection.CheckRight(right);
            end
          else
            begin
              //if iob.FieldExists(fieldnameforid) then
                begin
                  right := FREDB_Get_Rightname_UID(right,iob.Field(fieldnameforid).AsGUID);
                  add   := _DBConnection.CheckRight(right);
                end
              //else
              //  add := false;  //TODO: Think if an exception is maybe better ??
            end;
        end;

    begin
     result := false;
     if fob_f.FieldType<>fdbft_Object then exit;
     fob    := fob_f.AsObject;
     if fob.Field('FF').AsBoolean<>use_filter_fields then exit; // Is this a Filterfieldpass (Filterfields are processed seperately)?
     fieldname       := fob.Field('FN').AsString;
     filterkey       := fob_f.FieldName;
     case fob.Field('T').AsString of
       'RFU'  : begin
                 RightFilter;
                 if add then exit(true);
               end;
       'SCH' : begin
                 SchemeFilter;
                 if add=false then exit(true);
               end;
       'SFF' : begin
                 filter_fld_type := FREDB_FieldtypeShortString2Fieldtype(fob.Field('TR').AsString);
                 if not iob.FieldExists(fieldname) then begin
                   raise EFRE_DB_Exception.Create(edb_ERROR,'THE FILTERFIELD [%s] WAS NOT FOUND WHILE DERIVING [%s]',[fieldname,CollectionName]);
                 end;
                 target_field_type := iob.Field(fieldname).FieldType;
                 StringFieldFilter;
                 if add=false then exit(true); // break
               end;
       'GFF' : begin
                 filter_fld_type := FREDB_FieldtypeShortString2Fieldtype(fob.Field('TR').AsString);
                 if not iob.FieldExists(fieldname) then begin
                   raise EFRE_DB_Exception.Create(edb_ERROR,'THE FILTERFIELD [%s] WAS NOT FOUND WHILE DERIVING [%s]',[fieldname,CollectionName]);
                 end;
                 target_field_type := iob.Field(fieldname).FieldType;
                 GenericNumericFilter;
                 //GFRE_DB.LogInfo(ll_DebugAll,dblc_DB,'    *LEFT GFF (%s) add=[%s]',[filterkey,FREDB_Bool2String(add)]);
                 if add=false then exit(true);
               end;
       else raise EFRE_DB_Exception.Create(edb_INTERNAL,'UNEXPECTED FILTERTYPE IN DERIVED COLLECTION [%s]',[CollectionName]);
     end;
    end;

    procedure SendClientUpdate(const tr_obj:TFRE_DB_Object);
    var
        tr2 : TFRE_DB_Object;
        dum : boolean;
        ups : String;

        procedure  Chartupdate;
        var fct   : TFRE_DB_CHART_TRANSFORM;
            i     : integer;
            entry : IFRE_DB_Object;
        begin
          fct := FTransform as TFRE_DB_CHART_TRANSFORM;
          for i:=0 to  high(fct.FseriesFieldNames) do begin // Send updates for all series values
            entry:=GFRE_DBI.NewObject;
            entry.Field('uid').AsGUID     := tr_obj.Field(fct.FseriesFieldNames[i]+'_uid').AsGUID;
            entry.Field('value').AsReal32 := tr_obj.Field(fct.FseriesFieldNames[i]).AsReal32;
            FGatherUpdateList.addUpdatedEntry(entry);
          end;
        end;

    begin
      if update_data then begin
        case FDisplaytype of
          cdt_Invalid : raise EFRE_DB_Exception.Create(edb_ERROR,'invvalid derived collection displaytype');
          cdt_Listview,
          cdt_Treeview: FGatherUpdateList.addUpdatedEntry(tr_obj);
          cdt_Chartview: ChartUpdate;
        end;
      end else begin
                //TODO Check if in any PAGESET / CURSOR / OPEN RESULTSET
                ups := First.UID_String;
                tr2 := tr_obj;
                if FDBOList.FindPrev(tr2,dum) then begin  // TODO: Check if update is before/after ..
                  ups := tr2.UID_String;
                end;
        if (isFirst) then begin
          FGatherUpdateList.addNewEntry(tr_obj,'');
        end else begin
          FGatherUpdateList.addNewEntry(tr_obj,ups);
        end;
        isFirst:=false;
      end;
    end;

    var fld : TFRE_DB_FIELD;

begin
  iob := item;
  add := true;
  if (cdgf_Children in FGridDisplayFlags)
    and (not child_call) then
      begin
        if item.FieldOnlyExisting(FParentChldLinkFld,fld) then
          begin
            if fld.ValueCount>0 then
              exit;
          end;
      end;

  isFirst:=true;
  use_filter_fields:=false;
  //GFRE_DB.LogInfo(ll_DebugAll,dblc_DB,'  * ENTER OBJECT FILTER RUN [%s]',[iob.UID_String]);
  FFilters.ForAllBrk(@Filter);
  //GFRE_DB.LogInfo(ll_DebugAll,dblc_DB,'  * LEFT OBJECT FILTER RUN [%s] ADD [%s]',[iob.UID_String,FREDB_Bool2String(add)]);
  if add then begin
    tr_obj := FTransform.TransformInOut(FDependencyObject,item,false);
    iob    := tr_obj;
    use_filter_fields:=false;
    //GFRE_DB.LogInfo(ll_DebugAll,dblc_DB,'  * ENTER TRANSFORM FILTER RUN [%s]',[iob.UID_String]);
    FFiltersTrans.ForAllBrk(@Filter);
    //GFRE_DB.LogInfo(ll_DebugAll,dblc_DB,'  * LEFT TRANSFORM FILTER RUN [%s] ADD [%s]',[iob.UID_String,FREDB_Bool2String(add)]);
    if FTransform.HasFilterFields then begin
      use_filter_fields:=true;
      iob    := FTransform.TransformInOut(FDependencyObject,item,true);
      //GFRE_DB.LogInfo(ll_DebugAll,dblc_DB,'  * ENTER FILTER FIELD FILTER RUN [%s]',[iob.UID_String]);
      FFilters.ForAllBrk(@Filter);
      //GFRE_DB.LogInfo(ll_DebugAll,dblc_DB,'  * LEFT FILTER FIELD  RUN [%s] ADD [%s]',[iob.UID_String,FREDB_Bool2String(add)]);
      iob.free;
    end;
    if add then begin
      //GFRE_DB.LogInfo(ll_DebugAll,dblc_DB,'  * FINAL ADD [%s]',[tr_obj.UID_String,FREDB_Bool2String(add)]);
      if cdgf_Children in FGridDisplayFlags then
        begin
          if FChildParentMode then
            begin
              if (_DBConnection.GetReferencesCount(iob.UID,false,FParentChldLinkFld,true)>0) then
                begin
                  tr_obj.Field('children').AsString        := 'UNCHECKED';
                end;
              tr_obj.Field('_menufunc_').AsString      := 'Menu';
              tr_obj.Field('_contentfunc_').AsString   := 'Content';
              if item.FieldOnlyExisting('icon',fld) then // icon in source
                  tr_obj.Field('icon').AsString:= getThemedResource(fld.AsString); // icon in transformed
            end
          else
            begin
              //abort;
            end;
        end;
      if update_data then begin
        FDBOList.Update(tr_obj,false);
      end else begin
        FDBOList.Add(tr_obj,false); // Sort;
      end;
      if send_client_notify and assigned(FGatherUpdateList) then begin
        SendClientUpdate(tr_obj);
      end;
    end else begin
      //GFRE_DB.LogInfo(ll_DebugAll,dblc_DB,'  * FINAL REJECT [%s]',[tr_obj.UID_String,FREDB_Bool2String(add)]);
    end;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION._CheckUIDExists(obj_uid: TGuid): Boolean;

  function CheckUID(const Key :TFRE_DB_Object;const Item:boolean):boolean;
  begin
     result := FREDB_Guids_Same(Key.UID,obj_uid);
  end;

begin
  result := FDBOList.ForAllNodesBrk(@CheckUID);
end;


procedure TFRE_DB_DERIVED_COLLECTION.InternalSetup;
begin
  inherited InternalSetup;
  FFilters      := _Field('F').AsObject;
  FFiltersTrans := _Field('FT').AsObject;
  FOrders       := _Field('O').AsObject;
  FOrdersTrans  := _Field('OT').AsObject;
  FDBOList      := TFRE_DB_SortTree.Create(@RB_Sort_CompareEx,self);
end;

procedure TFRE_DB_DERIVED_COLLECTION._FilterIt(const childcall: boolean);
var cnt  : NativeInt;

  procedure LocalInsert(const item:TFRE_DB_Object);
  begin
     _AddToTransformedCollection(item,false,false,childcall);
  end;

  procedure LocalInsertFromParentDerived(const item : TFRE_DB_Object;const id : boolean);
  begin
     _AddToTransformedCollection(item,false,false,childcall);
  end;

  procedure Order(const order_def : TFRE_DB_FIELD);
  var fob       : TFRE_DB_Object;
  begin
    if order_def.FieldType<>fdbft_Object then exit;
    fob    := order_def.AsObject;
    SetLength(ForderDef,length(ForderDef)+1);
    with ForderDef[high(ForderDef)] do begin
      fieldname  := fob.Field('N').AsString;
      ascending  := fob.Field('A').AsBoolean;
    end;
  end;

  procedure Clear(const o:TFRE_DB_Object;const no:boolean);
  begin
    o.free;
  end;

  procedure CheckParentAndTransform;
  begin
    if FParentCollection = nil then raise EFRE_DB_Exception.Create(edb_ERROR,'the parent collection is not set in derived collection '+CollectionName);
    if FTransform        = nil then raise EFRE_DB_Exception.Create(edb_ERROR,'the transformation object is not set in derived collection '+CollectionName);
  end;

  procedure ClearLocal;
  begin
    FDBOList.ForallNodes(@Clear);
    FDBOList.Clear;
    SetLength(ForderDef,0);
  end;

  procedure LocalInsertExpanded;
  var i   : NativeInt;
  begin
   for i:=0 to high(FExpandedRefs) do
     begin
       FExpandedRefs[i].Assert_CheckStoreLocked;
       try
         FExpandedRefs[i].Set_Store_Locked(false);
         LocalInsert(FExpandedRefs[i]);
       finally
         FExpandedRefs[i].Set_Store_Locked(true);
       end;
     end;
  end;

begin
  if not assigned(FTransform) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'NO TRANSFORM SET!');
  if childcall then
    cnt:=0;;
  case FDCMode of
    dc_None: ;
    dc_Map2DerivedCollection: begin
                                ClearLocal;
                                FOrders.ForAllFields(@Order); // Get order definition
                                (FParentCollection as TFRE_DB_DERIVED_COLLECTION).FDBOList.ForAllNodes(@LocalInsertFromParentDerived);
                              end;
    dc_Map2RealCollection,
    dc_VirtualCollection:     begin
                                GFRE_DB.LogDebug(dblc_DB,'* START FILTERING [%s]',[CollectionName]);
                                ClearLocal;
                                FOrders.ForAllFields(@Order); // Get order definition
                                if not childcall then
                                  FParentCollection.FObjectLinkStore.ForAllItems(@LocalInsert)
                                else
                                  LocalInsertExpanded;
                                GFRE_DB.LogDebug(dblc_DB,'* FINAL FILTERED COUNT [%d]',[FDBOList.Count]);
                              end;
    dc_ReferentialLinkCollection:
                              begin
                                 ClearLocal;
                                 FOrders.ForAllFields(@Order); // Get order definition
                                 LocalInsertExpanded;
                              end;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION._CompareObjects(const ob1, ob2: TFRE_DB_Object): NativeInt;
var val,i : integer;
    guid_order : integer;

begin
  guid_order   := RB_Guid_Compare(ob1.UID,ob2.UID);
  if guid_order = 0 then exit(0);
  if Length(ForderDef)=0 then begin
    exit(guid_order);
  end else begin
    for i:=0 to high(ForderDef) do begin
      result := TFRE_DB_FIELD.OrderFieldCompare(ForderDef[i].fieldname,ob1,ob2);
      if not ForderDef[i].ascending then result := result*-1;
      if result<>0 then exit(result);
    end;
    exit(guid_order); // fallback
  end;
end;

function TFRE_DB_DERIVED_COLLECTION._DeleteFilterkey(const filter_key: TFRE_DB_String; const on_transform: boolean): TFRE_DB_Errortype;
begin
  if on_transform then begin
    if FFiltersTrans.DeleteField(filter_key) then begin
      Result := edb_OK;
    end else begin
      result := edb_NOT_FOUND;
    end;
  end else begin
    if FFilters.DeleteField(filter_key) then begin
      Result := edb_OK;
    end else begin
      result := edb_NOT_FOUND;
    end;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION._Get_DC_Order(const input: IFRE_DB_Object): TFRE_DB_DC_ORDER_LIST;
var cnt,i : integer;
    sort  : IFRE_DB_Object;
begin
  if input.FieldExists('sort') then begin
    cnt := input.Field('sort').ValueCount;
    SetLength(result,cnt);
    for i:=0 to cnt-1 do begin
      sort := input.Field('SORT').AsObjectItem[i];
      result[i].order_key   := i;
      result[i].order_field := sort.Field('PROPERTY').AsString;
      result[i].ascending   := sort.Field('ASCENDING').AsBoolean;
    end;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION._Get_DC_StringfieldKeys(const input: IFRE_DB_Object): TFRE_DB_DC_STRINGFIELDKEY_LIST;
var ftx:string;
begin
  ftx := input.Field('FULLTEXT').AsString;
  if ftx<>'' then begin
    SetLength(result,1);
    with result[0] do begin
      filter_key      := '*D_FTX';
      field_name      := 'FTX_SEARCH';
      value           := ftx;
      filtertype      := dbft_PART;
      on_transform    := false;
      on_filter_field := true;
    end;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION._Get_DC_PageingInfo(const input: IFRE_DB_Object): TFRE_DB_DC_PAGING_INFO;
begin
  result.count := input.Field('count').AsInt32;
  result.start := input.Field('start').AsInt32;
end;

function TFRE_DB_DERIVED_COLLECTION._Get_DC_ReferenceList(const input: IFRE_DB_Object): TFRE_DB_GUIDArray;
var fld : IFRE_DB_FIELD;
      i : NativeInt;
begin
 if assigned(FDependencyRef) then
   begin
     fld := input.FieldPath('DEPENDENCY.'+FDependencyRef[0]+'_REF.FILTERVALUES',true);
     if assigned(fld) then
       begin
         SetLength(result,fld.ValueCount);
         for i := 0 to High(Result) do
           result[i] := GFRE_BT.HexString_2_GUID(fld.AsStringArr[i]);
         exit;
       end
   end;
 SetLength(result,0);
end;

function TFRE_DB_DERIVED_COLLECTION._Get_DC_QueryID(const input: IFRE_DB_Object): String;
begin
  result := input.Field('QueryID').AsString;
end;

function TFRE_DB_DERIVED_COLLECTION.RemoveQueryIDWatch(const QID: String): TFRE_DB_Errortype;
begin

end;

function TFRE_DB_DERIVED_COLLECTION.AddQueryIDWatch(const QID: String; const page_i: TFRE_DB_DC_PAGING_INFO): TFRE_DB_Errortype;
begin

end;


procedure TFRE_DB_DERIVED_COLLECTION.DC_SetFilters_From_Input(const filter_defs: TFRE_DB_Object);
var key  : integer;

  procedure GetDependency(const filter_def:TFRE_DB_FIELD);
  var filter_field_type   : TFRE_DB_FIELDTYPE;
      filter_field_name   : TFRE_DB_String;
      on_transform        : Boolean;
      on_filter_field     : boolean;
      dont_convert_2_utc  : boolean;
      value_array         : TFRE_DB_StringArray;
      num_filt_typ        : TFRE_DB_NUM_FILTERTYPE;
      str_filt_typ        : TFRE_DB_STR_FILTERTYPE;
      key_val             : string;
  begin
    if filter_def.FieldName='UID' then exit;
    inc(key);
    if filter_def.FieldType=fdbft_Object then begin
      with filter_def.AsObject do begin
        writeln('--   ',filter_def.AsObject.DumpToString());
        filter_field_name   := Field('FILTERFIELDNAME').AsString;
        value_array         := Field('FILTERVALUES').AsStringArr; //filter_def.AsStringArr;
        if FieldExists('AFTERTRANS') then begin
          on_transform        := FREDB_String2Bool(Field('AFTERTRANS').AsString);
        end else begin
          on_transform      := false;
        end;
        if FieldExists('ONFILTERFIELD') then begin
          on_filter_field     := FREDB_String2Bool(Field('ONFILTERFIELD').AsString);
        end else begin
          on_filter_field   := false;
        end;
        if FieldExists('FILTERKEY') then begin
          key_val := '*D_'+Field('FILTERKEY').AsString;
        end else begin
          key_val := '*D_'+inttostr(key);
        end;
        if FieldExists('FILTERFIELDTYPE') then begin
          filter_field_type := FREDB_FieldtypeShortString2Fieldtype(Field('FILTERFIELDTYPE').AsString);
        end else begin
          filter_field_type := fdbft_String;
        end;
        if FieldExists('NUMFILTERTYPE') then begin
          num_filt_typ := FREDB_String2NumfilterType(Field('NUMFILTERTYPE').AsString);
        end else begin
          case filter_field_type of
            fdbft_GUID: num_filt_typ := dbnf_OneValueFromFilter;
            else        num_filt_typ := dbnf_EXACT;
          end;
        end;
        if FieldExists('STRFILTERTYPE') then begin
          str_filt_typ  := FREDB_String2StrFilterType(Field('STRFILTERTYPE').AsString);
        end else begin
          str_filt_typ  := dbft_EXACT;
        end;
        if FieldExists('NO_UTC_CONV') then begin
          dont_convert_2_utc := FREDB_String2Bool(Field('NO_UTC_CONV').AsString);
        end else begin
          dont_convert_2_utc := false;
        end;
      end;
    end;
    case filter_field_type of
      fdbft_GUID:         AddUIDFieldFilter      (key_val,filter_field_name,GFRE_DB.StringArray2GuidArray(value_array),num_filt_typ,on_transform,on_filter_field);
      fdbft_Byte:         AddByteFieldFilter     (key_val,filter_field_name,GFRE_DB.StringArray2ByteArray(value_array),num_filt_typ,on_transform,on_filter_field);
      fdbft_Int16:        AddInt16FieldFilter    (key_val,filter_field_name,GFRE_DB.StringArray2Int16Array(value_array),num_filt_typ,on_transform,on_filter_field);
      fdbft_UInt16:       AddUint16FieldFilter   (key_val,filter_field_name,GFRE_DB.StringArray2UInt16Array(value_array),num_filt_typ,on_transform,on_filter_field);
      fdbft_Int32:        Addint32FieldFilter    (key_val,filter_field_name,GFRE_DB.StringArray2Int32Array(value_array),num_filt_typ,on_transform,on_filter_field);
      fdbft_UInt32:       AddUint32FieldFilter   (key_val,filter_field_name,GFRE_DB.StringArray2UInt32Array(value_array),num_filt_typ,on_transform,on_filter_field);
      fdbft_Int64:        Addint64FieldFilter    (key_val,filter_field_name,GFRE_DB.StringArray2Int64Array(value_array),num_filt_typ,on_transform,on_filter_field);
      fdbft_UInt64:       AddUint64FieldFilter   (key_val,filter_field_name,GFRE_DB.StringArray2UInt64Array(value_array),num_filt_typ,on_transform,on_filter_field);
      fdbft_Real32:       AddReal32FieldFilter   (key_val,filter_field_name,GFRE_DB.StringArray2Real32Array(value_array),num_filt_typ,on_transform,on_filter_field);
      fdbft_Real64:       AddReal64FieldFilter   (key_val,filter_field_name,GFRE_DB.StringArray2Real64Array(value_array),num_filt_typ,on_transform,on_filter_field);
      fdbft_Currency:     AddCurrencyFieldFilter (key_val,filter_field_name,GFRE_DB.StringArray2CurrArray(value_array),num_filt_typ,on_transform,on_filter_field);
      fdbft_String:       AddStringFieldFilter   (key_val,filter_field_name,value_array,str_filt_typ,on_transform,on_filter_field);
      fdbft_Boolean:      AddBooleanFieldFilter  (key_val,filter_field_name,FREDB_String2Bool(value_array[0]),on_transform,on_filter_field);
      fdbft_DateTimeUTC:  begin
                            if not dont_convert_2_utc then begin
                              AddDateTimeFieldFilter (key_val,filter_field_name,GFRE_DB.StringArray2DateTimeArray(value_array),num_filt_typ,on_transform,on_filter_field);
                            end else begin
                              AddDateTimeFieldFilter (key_val,filter_field_name,GFRE_DB.StringArray2DateTimeArrayUTC(value_array),num_filt_typ,on_transform,on_filter_field);
                            end;
                          end
      else raise EFRE_DB_Exception.Create(edb_ERROR,'INPUT FILTER TYPE [%s] IS NOT SUPPORTED',[CFRE_DB_FIELDTYPE[filter_field_type]]);
    end;
  end;

begin
  key := 0;
  filter_defs.ForAllFields(@GetDependency);
end;

procedure TFRE_DB_DERIVED_COLLECTION.ForAllI(const func: IFRE_DB_Obj_Iterator);

  procedure Add(const o:TFRE_DB_Object;const no:boolean);
  begin
    func(o);
  end;

begin
begin
 if FParentCollection = nil then raise EFRE_DB_Exception.Create(edb_ERROR,'the parent collection is not set in derived collection '+CollectionName);
 if FTransform        = nil then raise EFRE_DB_Exception.Create(edb_ERROR,'the transformation object is not set in derived collection '+CollectionName);
 Derive; // Derive should be responsible for necessity checking
 FDBOList.ForallNodes(@Add);
end;


end;


class function TFRE_DB_DERIVED_COLLECTION.Forced_In_Memory: Boolean;
begin
  Result:=true;
end;

procedure TFRE_DB_DERIVED_COLLECTION.BindSession(const session: TFRE_DB_UserSession);
begin
  FSession := session;
end;

procedure TFRE_DB_DERIVED_COLLECTION.ApplyToPageI(const page_info: TFRE_DB_DC_PAGING_INFO; const iterator: IFRE_DB_Obj_Iterator);

  procedure MyIterator(const obj:TFRE_DB_Object);
  begin
    iterator(obj);
  end;

begin
  //ApplyToPage(page_info,@MyIterator);
end;

class procedure TFRE_DB_DERIVED_COLLECTION.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.Strict(true);
end;

destructor TFRE_DB_DERIVED_COLLECTION.Destroy;
begin
  if assigned(FParentCollection) then begin
    FParentCollection.RemoveObserver(self);
  end;
  inherited Destroy;
end;

function TFRE_DB_DERIVED_COLLECTION.AddStringFieldFilter(const filter_key, field_name: TFRE_DB_String; const values: TFRE_DB_StringArray; const filtertype: TFRE_DB_STR_FILTERTYPE; const on_transform: boolean; const on_filter_field: boolean): TFRE_DB_Errortype;
var filter:TFRE_DB_Object;
begin
  filter := GFRE_DB.NewObject;
  filter.Field('T').AsString     :='SFF';
  filter.Field('TR').AsString    := CFRE_DB_FIELDTYPE_SHORT[fdbft_String];
  filter.Field('FF').AsBoolean   := on_filter_field;
  filter.Field('FN').AsString    := field_name;
  filter.Field('FV').AsStringArr := values;
  filter.Field('FT').AsString    := CFRE_DB_STR_FILTERTYPE[filtertype];
  filter.Field('OT').AsBoolean   := on_transform;
  if on_transform then begin
    FFiltersTrans.Field(filter_key).AsObject := filter;
  end else begin
    FFilters.Field(filter_key).AsObject := filter;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.AddUIDFieldFilter(const filter_key, field_name: TFRE_DB_String; const values: TFRE_DB_GUIDArray; const number_compare_type: TFRE_DB_NUM_FILTERTYPE; const on_transform: boolean; const on_filter_field: boolean): TFRE_DB_Errortype;
var filter:TFRE_DB_Object;
begin
  filter := GFRE_DB.NewObject;
  if not(number_compare_type in [dbnf_EXACT,dbnf_EXACT_NEGATED,dbnf_AllValuesFromFilter,dbnf_OneValueFromFilter,dbnf_NoValueInFilter]) then raise EFRE_DB_Exception.Create(edb_ERROR,'DERIVED COLLECTION : UID Field Filter number_compare_type [%s] is not valid',[FREDB_NumFilterType2String(number_compare_type)]);
  filter.Field('T').AsString   := 'GFF'; // Generic Field Filter
  filter.Field('TR').AsString  := CFRE_DB_FIELDTYPE_SHORT[fdbft_GUID];
  filter.Field('FF').AsBoolean := on_filter_field;
  filter.Field('FN').AsString  := field_name;
  filter.Field('FV').AsGUIDArr := values;
  filter.Field('FT').AsString  := CFRE_DB_NUM_FILTERTYPE[number_compare_type];
  filter.Field('OT').AsBoolean := on_transform;
  if on_transform then begin
    FFiltersTrans.Field(filter_key).AsObject := filter;
  end else begin
    FFilters.Field(filter_key).AsObject := filter;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.AddBooleanFieldFilter(const filter_key, field_name: TFRE_DB_String; const value: Boolean; const on_transform: boolean; const on_filter_field: boolean): TFRE_DB_Errortype;
var filter:TFRE_DB_Object;
begin
  filter := GFRE_DB.NewObject;
  filter.Field('T').AsString   := 'GFF'; // Generic Field Filter
  filter.Field('TR').AsString  := CFRE_DB_FIELDTYPE_SHORT[fdbft_Boolean];
  filter.Field('FF').AsBoolean := on_filter_field;
  filter.Field('FN').AsString  := field_name;
  filter.Field('FV').AsBoolean := value;
  filter.Field('FT').AsString  := CFRE_DB_NUM_FILTERTYPE[dbnf_EXACT];
  filter.Field('OT').AsBoolean := on_transform;
  if on_transform then begin
    FFiltersTrans.Field(filter_key).AsObject := filter;
  end else begin
    FFilters.Field(filter_key).AsObject := filter;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.AddByteFieldFilter(const filter_key, field_name: TFRE_DB_String; const values: TFRE_DB_ByteArray; const number_compare_type: TFRE_DB_NUM_FILTERTYPE; const on_transform: boolean; const on_filter_field: boolean): TFRE_DB_Errortype;
var filter:TFRE_DB_Object;
begin
  filter := GFRE_DB.NewObject;
  filter.Field('T').AsString    := 'GFF'; // Generic Field Filter
  filter.Field('TR').AsString   := CFRE_DB_FIELDTYPE_SHORT[fdbft_Byte];
  filter.Field('FF').AsBoolean  := on_filter_field;
  filter.Field('FN').AsString   := field_name;
  filter.Field('FV').AsByteArr  := values;
  filter.Field('FT').AsString   := CFRE_DB_NUM_FILTERTYPE[number_compare_type];
  filter.Field('OT').AsBoolean  := on_transform;
  if on_transform then begin
    FFiltersTrans.Field(filter_key).AsObject := filter;
  end else begin
    FFilters.Field(filter_key).AsObject := filter;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.AddInt16FieldFilter(const filter_key, field_name: TFRE_DB_String; const values: TFRE_DB_Int16Array; const number_compare_type: TFRE_DB_NUM_FILTERTYPE; const on_transform: boolean; const on_filter_field: boolean): TFRE_DB_Errortype;
var filter:TFRE_DB_Object;
begin
  filter := GFRE_DB.NewObject;
  filter.Field('T').AsString    := 'GFF'; // Generic Field Filter
  filter.Field('TR').AsString   := CFRE_DB_FIELDTYPE_SHORT[fdbft_Int16];
  filter.Field('FF').AsBoolean  := on_filter_field;
  filter.Field('FN').AsString   := field_name;
  filter.Field('FV').AsInt16Arr := values;
  filter.Field('FT').AsString   := CFRE_DB_NUM_FILTERTYPE[number_compare_type];
  filter.Field('OT').AsBoolean  := on_transform;
  if on_transform then begin
    FFiltersTrans.Field(filter_key).AsObject := filter;
  end else begin
    FFilters.Field(filter_key).AsObject := filter;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.AddUInt16FieldFilter(const filter_key, field_name: TFRE_DB_String; const values: TFRE_DB_UInt16Array; const number_compare_type: TFRE_DB_NUM_FILTERTYPE; const on_transform: boolean; const on_filter_field: boolean): TFRE_DB_Errortype;
var filter:TFRE_DB_Object;
begin
  filter := GFRE_DB.NewObject;
  filter.Field('T').AsString     := 'GFF'; // Generic Field Filter
  filter.Field('TR').AsString    := CFRE_DB_FIELDTYPE_SHORT[fdbft_UInt16];
  filter.Field('FF').AsBoolean   := on_filter_field;
  filter.Field('FN').AsString    := field_name;
  filter.Field('FV').AsUInt16Arr := values;
  filter.Field('FT').AsString    := CFRE_DB_NUM_FILTERTYPE[number_compare_type];
  filter.Field('OT').AsBoolean   := on_transform;
  if on_transform then begin
    FFiltersTrans.Field(filter_key).AsObject := filter;
  end else begin
    FFilters.Field(filter_key).AsObject := filter;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.AddInt32FieldFilter(const filter_key, field_name: TFRE_DB_String; const values: TFRE_DB_Int32Array; const number_compare_type: TFRE_DB_NUM_FILTERTYPE; const on_transform: boolean; const on_filter_field: boolean): TFRE_DB_Errortype;
var filter:TFRE_DB_Object;
begin
  filter := GFRE_DB.NewObject;
  filter.Field('T').AsString    := 'GFF'; // Generic Field Filter
  filter.Field('TR').AsString   := CFRE_DB_FIELDTYPE_SHORT[fdbft_Int32];
  filter.Field('FF').AsBoolean  := on_filter_field;
  filter.Field('FN').AsString   := field_name;
  filter.Field('FV').AsInt32Arr := values;
  filter.Field('FT').AsString   := CFRE_DB_NUM_FILTERTYPE[number_compare_type];
  filter.Field('OT').AsBoolean  := on_transform;
  if on_transform then begin
    FFiltersTrans.Field(filter_key).AsObject := filter;
  end else begin
    FFilters.Field(filter_key).AsObject := filter;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.AddUInt32FieldFilter(const filter_key, field_name: TFRE_DB_String; const values: TFRE_DB_UInt32Array; const number_compare_type: TFRE_DB_NUM_FILTERTYPE; const on_transform: boolean; const on_filter_field: boolean): TFRE_DB_Errortype;
var filter:TFRE_DB_Object;
begin
  filter := GFRE_DB.NewObject;
  filter.Field('T').AsString     := 'GFF'; // Generic Field Filter
  filter.Field('TR').AsString    := CFRE_DB_FIELDTYPE_SHORT[fdbft_UInt32];
  filter.Field('FF').AsBoolean   := on_filter_field;
  filter.Field('FN').AsString    := field_name;
  filter.Field('FV').AsUInt32Arr := values;
  filter.Field('FT').AsString    := CFRE_DB_NUM_FILTERTYPE[number_compare_type];
  filter.Field('OT').AsBoolean   := on_transform;
  if on_transform then begin
    FFiltersTrans.Field(filter_key).AsObject := filter;
  end else begin
    FFilters.Field(filter_key).AsObject := filter;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.AddInt64FieldFilter(const filter_key, field_name: TFRE_DB_String; const values: TFRE_DB_Int64Array; const number_compare_type: TFRE_DB_NUM_FILTERTYPE; const on_transform: boolean; const on_filter_field: boolean): TFRE_DB_Errortype;
var filter:TFRE_DB_Object;
begin
  filter := GFRE_DB.NewObject;
  filter.Field('T').AsString    := 'GFF'; // Generic Field Filter
  filter.Field('TR').AsString   := CFRE_DB_FIELDTYPE_SHORT[fdbft_Int64];
  filter.Field('FF').AsBoolean  := on_filter_field;
  filter.Field('FN').AsString   := field_name;
  filter.Field('FV').AsInt64Arr := values;
  filter.Field('FT').AsString   := CFRE_DB_NUM_FILTERTYPE[number_compare_type];
  filter.Field('OT').AsBoolean  := on_transform;
  if on_transform then begin
    FFiltersTrans.Field(filter_key).AsObject := filter;
  end else begin
    FFilters.Field(filter_key).AsObject := filter;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.AddUInt64FieldFilter(const filter_key, field_name: TFRE_DB_String; const values: TFRE_DB_UInt64Array; const number_compare_type: TFRE_DB_NUM_FILTERTYPE; const on_transform: boolean; const on_filter_field: boolean): TFRE_DB_Errortype;
var filter:TFRE_DB_Object;
begin
  filter := GFRE_DB.NewObject;
  filter.Field('T').AsString     := 'GFF'; // Generic Field Filter
  filter.Field('TR').AsString    := CFRE_DB_FIELDTYPE_SHORT[fdbft_UInt64];
  filter.Field('FF').AsBoolean   := on_filter_field;
  filter.Field('FN').AsString    := field_name;
  filter.Field('FV').AsUInt64Arr := values;
  filter.Field('FT').AsString    := CFRE_DB_NUM_FILTERTYPE[number_compare_type];
  filter.Field('OT').AsBoolean   := on_transform;
  if on_transform then begin
    FFiltersTrans.Field(filter_key).AsObject := filter;
  end else begin
    FFilters.Field(filter_key).AsObject := filter;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.AddReal32FieldFilter(const filter_key, field_name: TFRE_DB_String; const values: TFRE_DB_Real32Array; const number_compare_type: TFRE_DB_NUM_FILTERTYPE; const on_transform: boolean; const on_filter_field: boolean): TFRE_DB_Errortype;
var filter:TFRE_DB_Object;
begin
  filter := GFRE_DB.NewObject;
  filter.Field('T').AsString     := 'GFF'; // Generic Field Filter
  filter.Field('TR').AsString    := CFRE_DB_FIELDTYPE_SHORT[fdbft_Real32];
  filter.Field('FF').AsBoolean   := on_filter_field;
  filter.Field('FN').AsString    := field_name;
  filter.Field('FV').AsReal32Arr := values;
  filter.Field('FT').AsString    := CFRE_DB_NUM_FILTERTYPE[number_compare_type];
  filter.Field('OT').AsBoolean   := on_transform;
  if on_transform then begin
    FFiltersTrans.Field(filter_key).AsObject := filter;
  end else begin
    FFilters.Field(filter_key).AsObject := filter;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.AddReal64FieldFilter(const filter_key, field_name: TFRE_DB_String; const values: TFRE_DB_Real64Array; const number_compare_type: TFRE_DB_NUM_FILTERTYPE; const on_transform: boolean; const on_filter_field: boolean): TFRE_DB_Errortype;
var filter:TFRE_DB_Object;
begin
  filter := GFRE_DB.NewObject;
  filter.Field('T').AsString     := 'GFF'; // Generic Field Filter
  filter.Field('TR').AsString    := CFRE_DB_FIELDTYPE_SHORT[fdbft_Real64];
  filter.Field('FF').AsBoolean   := on_filter_field;
  filter.Field('FN').AsString    := field_name;
  filter.Field('FV').AsReal64Arr := values;
  filter.Field('FT').AsString    := CFRE_DB_NUM_FILTERTYPE[number_compare_type];
  filter.Field('OT').AsBoolean   := on_transform;
  if on_transform then begin
    FFiltersTrans.Field(filter_key).AsObject := filter;
  end else begin
    FFilters.Field(filter_key).AsObject := filter;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.AddCurrencyFieldFilter(const filter_key, field_name: TFRE_DB_String; const values: TFRE_DB_CurrencyArray; const number_compare_type: TFRE_DB_NUM_FILTERTYPE; const on_transform: boolean; const on_filter_field: boolean): TFRE_DB_Errortype;
var filter:TFRE_DB_Object;
begin
  filter := GFRE_DB.NewObject;
  filter.Field('T').AsString       := 'GFF'; // Generic Field Filter
  filter.Field('TR').AsString      := CFRE_DB_FIELDTYPE_SHORT[fdbft_Currency];
  filter.Field('FF').AsBoolean     := on_filter_field;
  filter.Field('FN').AsString      := field_name;
  filter.Field('FV').AsCurrencyArr := values;
  filter.Field('FT').AsString      := CFRE_DB_NUM_FILTERTYPE[number_compare_type];
  filter.Field('OT').AsBoolean     := on_transform;
  if on_transform then begin
    FFiltersTrans.Field(filter_key).AsObject := filter;
  end else begin
    FFilters.Field(filter_key).AsObject := filter;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.AddDateTimeFieldFilter(const filter_key, field_name: TFRE_DB_String; const values: TFRE_DB_DateTimeArray; const number_compare_type: TFRE_DB_NUM_FILTERTYPE; const on_transform: boolean; const on_filter_field: boolean): TFRE_DB_Errortype;
var filter:TFRE_DB_Object;
begin
  filter := GFRE_DB.NewObject;
  filter.Field('T').AsString          := 'GFF'; // Generic Field Filter
  filter.Field('TR').AsString         := CFRE_DB_FIELDTYPE_SHORT[fdbft_DateTimeUTC];
  filter.Field('FF').AsBoolean        := on_filter_field;
  filter.Field('FN').AsString         := field_name;
  filter.Field('FV').AsDateTimeUTCArr := values;
  filter.Field('FT').AsString         := CFRE_DB_NUM_FILTERTYPE[number_compare_type];
  filter.Field('OT').AsBoolean        := on_transform;
  if on_transform then begin
    FFiltersTrans.Field(filter_key).AsObject := filter;
  end else begin
    FFilters.Field(filter_key).AsObject := filter;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.AddSchemeFilter(const filter_key: TFRE_DB_String; const values: TFRE_DB_StringArray; const negate: boolean): TFRE_DB_Errortype;
var filter:TFRE_DB_Object;
begin
  filter := GFRE_DB.NewObject;
  filter.Field('T').AsString          := 'SCH'; // Generic Scheme Filter
  filter.Field('TR').AsString         := 'S';
  filter.Field('FF').AsBoolean        := false;
  filter.Field('FN').AsString         := 'S';
  filter.Field('FV').AsStringArr      := values;
  filter.Field('FT').AsString         := 'X';
  filter.Field('OT').AsBoolean        := false;
  filter.Field('NEG').AsBoolean       := negate;
  FFilters.Field(filter_key).AsObject := filter;
end;

function TFRE_DB_DERIVED_COLLECTION.AddRightFilterForEntryAndUser(const filter_key: TFRE_DB_String; const right_prefix: TFRE_DB_NameType; const fieldname_for_uid: TFRE_DB_NameType): TFRE_DB_Errortype;
var filter:TFRE_DB_Object;
begin
  filter := GFRE_DB.NewObject;
  filter.Field('T').AsString          := 'RFU'; // Right Filter
  //filter.Field('TR').AsString         := 'S';
  filter.Field('FF').AsBoolean        := false;
  filter.Field('FN').AsString         := fieldname_for_uid;
  filter.Field('FV').AsString         := right_prefix;
  //filter.Field('FT').AsString         := 'X';
  //filter.Field('OT').AsBoolean        := false;
  //filter.Field('NEG').AsBoolean       := false;
  FFilters.Field(filter_key).AsObject := filter;
end;


function TFRE_DB_DERIVED_COLLECTION.RemoveFieldFilter(const filter_key: TFRE_DB_String; const on_transform: boolean): TFRE_DB_Errortype;
begin
  result := _DeleteFilterKey(filter_key,on_transform);
end;

function TFRE_DB_DERIVED_COLLECTION.AddOrderField(const order_key, field_name: TFRE_DB_String; const ascending: boolean): TFRE_DB_Errortype;
var order:TFRE_DB_Object;
begin
  order := GFRE_DB.NewObject;
  order.Field('N').AsString  := field_name;
  order.Field('A').AsBoolean := ascending;
  FOrders.Field(order_key).AsObject := order;
end;

procedure TFRE_DB_DERIVED_COLLECTION.RemoveAllOrderFields;
begin
  FOrders.ClearAllFields;
end;

procedure TFRE_DB_DERIVED_COLLECTION.RemoveAllFilterFields;
begin
 FFilters.ClearAllFields;
 FFiltersTrans.ClearAllFields;
end;

procedure TFRE_DB_DERIVED_COLLECTION.RemoveAllFiltersPrefix(const prefix: string);
var sl : TStringlist;
     i : Integer;

    procedure GetRemoveList(const fld:TFRE_DB_FIELD);
    begin
      if pos(prefix,fld.FieldName)=1 then begin
        sl.add(fld.FieldName);
      end;
    end;

begin
  sl :=TStringList.Create;
  try
    FFilters.ForAllFields(@GetRemoveList);
    for i:=0 to sl.count-1 do begin
      FFilters.DeleteField(sl[i]);
    end;
    sl.clear;
    FFiltersTrans.ForAllFields(@GetRemoveList);
    for i:=0 to sl.count-1 do begin
      FFiltersTrans.DeleteField(sl[i]);
    end;
  finally
    sl.free;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.Store(var obj: TFRE_DB_Object): TFRE_DB_Errortype;
begin
  if FDCMode<>dc_VirtualCollection then raise EFRE_DB_Exception.Create(edb_ERROR,'ONLY STORES IN VIRTUAL DCs ARE ALLOWED');
  result := FParentCollection.Store(obj);
end;

function TFRE_DB_DERIVED_COLLECTION.Remove(const ouid: TGUID): boolean;
begin
  if FDCMode<>dc_VirtualCollection then raise EFRE_DB_Exception.Create(edb_ERROR,'ONLY DELETES IN VIRTUAL DCs ARE ALLOWED');
  result := FParentCollection.Remove(ouid);
end;

function TFRE_DB_DERIVED_COLLECTION.Update(const dbo: TFRE_DB_Object): TFRE_DB_Errortype;
begin
  if FDCMode<>dc_VirtualCollection then raise EFRE_DB_Exception.Create(edb_ERROR,'ONLY UPDATES IN VIRTUAL DCs ARE ALLOWED');
  result := FParentCollection.Update(dbo);
end;

procedure TFRE_DB_DERIVED_COLLECTION.SetDeriveParent(const coll: TFRE_DB_COLLECTION; const idField: String);

  procedure SetFunctionsFromParent(const pc : TFRE_DB_DERIVED_COLLECTION);
  begin
    FTransform         := pc.FTransform;
    FitemMenuFunc      := pc.FitemMenuFunc;
    FitemDetailsFunc   := pc.FitemDetailsFunc;
    FselectionDepFunc  := pc.FselectionDepFunc;
    FtreeMenuFunc      := pc.FtreeMenuFunc;
    FdropFunc          := pc.FdropFunc;
    FdragFunc          := pc.FdragFunc;
    FDisplaytype       := pc.FDisplaytype;
    FGridDisplayFlags  := pc.FGridDisplayFlags;
    FTitle             := pc.FTitle;
    FTreeNodeCaption   := pc.FTreeNodeCaption;
    FTreeNodeIconField := pc.FTreeNodeIconField;
  end;

begin
  if FDCMode<>dc_None then raise EFRE_DB_Exception.Create(edb_ERROR,'CANNOT SWITCH DERIVED CONNECTION MODE, ONCE IT WAS CHOSEN');
  FIdField:=idField;
  if coll is TFRE_DB_DERIVED_COLLECTION then begin
    FDCMode := dc_Map2DerivedCollection;
    FParentCollection := coll;
    SetFunctionsFromParent(FParentCollection as TFRE_DB_DERIVED_COLLECTION);
  end else begin
    FDCMode := dc_Map2RealCollection;
    FParentCollection := coll;
  end;
  FInitialDerived := False;
end;

procedure TFRE_DB_DERIVED_COLLECTION.SetVirtualMode(const StringIndexKey: String);
var collif : IFRE_DB_COLLECTION;
begin
  if FDCMode<>dc_None then raise EFRE_DB_Exception.Create(edb_ERROR,'CANNOT SWITCH DERIVED CONNECTION MODE, ONCE IT WAS CHOSEN');
  FDCMode           := dc_VirtualCollection;
  FIdField          := 'uid';
  if StringIndexKey='' then begin
    FParentCollection := _DBConnection(true).Collection('VDC_'+CollectionName,true,true);
  end else begin
    _DBConnection(true).CollectionAsIntf('VDCI_'+CollectionName,IFRE_DB_COLLECTION,collif,true,true);
    collif.DefineIndexOnField(StringIndexKey,fdbft_String,true,true);
    FParentCollection := collif.Implementor as TFRE_DB_COLLECTION;
  end;
end;

procedure TFRE_DB_DERIVED_COLLECTION.SetReferentialLinkMode(const scheme_and_field_constraint: TFRE_DB_String; const dependency_object_refers: boolean; const dependency_reference: string);
var collif : IFRE_DB_COLLECTION;
begin
  if FDCMode<>dc_None then raise EFRE_DB_Exception.Create(edb_ERROR,'CANNOT SWITCH DERIVED CONNECTION MODE, ONCE IT WAS CHOSEN');
  FDCMode           := dc_ReferentialLinkCollection;
  FIdField          := 'uid';
  FDepObjectsRefers := dependency_object_refers;
  SetLength(FDependencyRef,1);
  FDependencyRef[0] := dependency_reference;
  if dependency_object_refers then
    FREDB_SeperateString(uppercase(scheme_and_field_constraint),'>',FDepRefConstraint)
  else
    FREDB_SeperateString(uppercase(scheme_and_field_constraint),'<',FDepRefConstraint);
  FParentCollection := nil; // used for system objects too, so make an own list, because storeing sysobjects in other collections is a bad idea ...
end;

procedure TFRE_DB_DERIVED_COLLECTION.SetUseDependencyAsRefLinkFilter(const scheme_and_field_constraint: TFRE_DB_String; const dependency_object_refers: boolean; const negate: boolean; const dependency_reference: string);
begin
  FUseDepAsLinkFilt := true;
  FDepObjectsRefers := dependency_object_refers;
  SetLength(FDependencyRef,1);
  FDependencyRef[0] := dependency_reference;
  FDepObjectsRefNeg := negate;
  if dependency_object_refers then
    FREDB_SeperateString(uppercase(scheme_and_field_constraint),'>',FDepRefConstraint)
  else
    FREDB_SeperateString(uppercase(scheme_and_field_constraint),'<',FDepRefConstraint);
end;

procedure TFRE_DB_DERIVED_COLLECTION.SetDeriveParentI(const coll: IFRE_DB_COLLECTION; const idField: String);
begin
  SetDeriveParent(coll.Implementor as TFRE_DB_COLLECTION, idField);
end;

procedure TFRE_DB_DERIVED_COLLECTION.SetDeriveTransformation(const tob: TFRE_DB_TRANSFORMOBJECT);
begin
  FTransform.Free;
  FTransform := tob;
  FTransform.FManageInfo := _DBConnection;
end;

procedure TFRE_DB_DERIVED_COLLECTION.SetDeriveTransformationI(const tob: IFRE_DB_TRANSFORMOBJECT);
begin
  SetDeriveTransformation(tob.Implementor as TFRE_DB_TRANSFORMOBJECT);
end;

function TFRE_DB_DERIVED_COLLECTION.Derive: TFRE_DB_Errortype;
begin
  _FilterIt(false);
end;


function TFRE_DB_DERIVED_COLLECTION.ItemCount: Int64;
begin
  result := FDBOList.Count;
end;

function TFRE_DB_DERIVED_COLLECTION.Count: QWord;
begin
  result := ItemCount;
end;

function TFRE_DB_DERIVED_COLLECTION.First: TFRE_DB_Object;
var obj  : TFRE_DB_Object;
    item : boolean;
begin
  if ItemCount=0 then exit(nil);
  FDBOList.FirstNode(obj,item);
  result := obj;
end;

function TFRE_DB_DERIVED_COLLECTION.Last: TFRE_DB_Object;
var obj  : TFRE_DB_Object;
    item : boolean;
begin
  if ItemCount=0 then exit(nil);
  FDBOList.LastNode(obj,item);
  result := obj;
end;

function TFRE_DB_DERIVED_COLLECTION.GetItem(const num: uint64): IFRE_DB_Object;
var obj  : TFRE_DB_Object;
    item : boolean;
begin
  if ItemCount=0 then exit(nil);
  if FDBOList.GetDirect(num,obj,item) then begin
    result := obj;
  end else begin
    result := nil;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.Fetch(const ouid: TGUID; out dbo: TFRE_DB_Object): boolean;
begin
  if FDCMode<>dc_VirtualCollection then raise EFRE_DB_Exception.Create(edb_ERROR,'ONLY FETCHES IN VIRTUAL DCs ARE ALLOWED');
  Result := FParentCollection.Fetch(ouid, dbo);
end;

function TFRE_DB_DERIVED_COLLECTION.FetchFromParent(const ouid: TGUID; out dbo: TFRE_DB_Object): boolean;
begin
  //case FDCMode of
  //  //dc_None: ;
  //  dc_Map2RealCollection:
  //
  //  dc_VirtualCollection: ;
  //  dc_Map2DerivedCollection: ;
  //  dc_ReferentialLinkCollection: ;
  //end;
  //if FDCMode<>dc_Map2RealCollection then raise EFRE_DB_Exception.Create(edb_ERROR,'FETCH FROM PARENT IS ONLY IN MAP2REALMODE ALLOWED');
  //Result := FParentCollection.Fetch(ouid, dbo);
  result := _DBConnection.Fetch(ouid,dbo);
end;

function TFRE_DB_DERIVED_COLLECTION.FetchFromParentI(const ouid: TGUID; out dbo: IFRE_DB_Object): boolean;
var idbo : TFRE_DB_Object;
begin
  result := FetchFromParent(ouid,idbo);
  if result then begin
    dbo :=idbo;
  end else begin
    dbo := nil;
  end;
end;

procedure TFRE_DB_DERIVED_COLLECTION.RemoveAllEntries;

  procedure Clear(const o:TFRE_DB_Object;const no:boolean);
  begin
    _NotifyObserversOrRecord(fdbntf_DELETE,o,o.UID);
    o.free;
  end;

begin
  if FParentCollection = nil then raise EFRE_DB_Exception.Create(edb_ERROR,'the parent collection is not set in derived collection '+CollectionName);
  if FTransform        = nil then raise EFRE_DB_Exception.Create(edb_ERROR,'the transformation object is not set in derived collection '+CollectionName);
  FDBOList.ForallNodes(@Clear);
  FDBOList.Clear;
end;


procedure TFRE_DB_DERIVED_COLLECTION.ApplyToPage(const QueryID:String ; const page_info: TFRE_DB_DC_PAGING_INFO; const iterator: TFRE_DB_Obj_Iterator);

   procedure DoIt(const key:TFRE_DB_Object;const val:boolean);
   begin
     iterator(key);
   end;

begin
  AddQueryIDWatch(QueryID,page_info);
  FDBOList.ForAllNodesRange(page_info.start,page_info.count,@DoIt);
end;

procedure TFRE_DB_DERIVED_COLLECTION.ApplyToData(const iterator: TFRE_DB_Obj_Iterator);
   procedure PrepareData(const key:TFRE_DB_Object;const val:boolean);
   begin
     iterator(key);
   end;
begin
  FDBOList.ForAllNodes(@PrepareData);
end;

function TFRE_DB_DERIVED_COLLECTION.GetStoreDescription: TFRE_DB_CONTENT_DESC;
begin
  case FDisplaytype of
    cdt_Listview:   begin
//                      result := TFRE_DB_STORE_DESC.create.Describe(FIdField,CSF(@IMI_GET_GRID_DATA),Nil,CSF(@IMI_CLEAR_QUERY_RESULTS),CollectionName);
                      result := TFRE_DB_STORE_DESC.create.Describe(FIdField,CSF(@IMI_GET_GRID_DATA),Nil,CSF(@IMI_DESTROY_STORE),nil,CollectionName);
                    end;
    cdt_Treeview:   begin;
//                      result := TFRE_DB_STORE_DESC.create.Describe(FIdField,CSF(@IMI_GET_CHILDREN_DATA),FTreeNodeCaption,CSF(@IMI_CLEAR_QUERY_RESULTS),CollectionName);
                      result := TFRE_DB_STORE_DESC.create.Describe(FIdField,CSF(@IMI_GET_CHILDREN_DATA),FTreeNodeCaption,CSF(@IMI_DESTROY_STORE),nil,CollectionName);
                    end;
     cdt_Chartview: begin
//                      result := TFRE_DB_STORE_DESC.create.Describe(FIdField,CSF(@IMI_GET_CHART_DATA),Nil,CSF(@IMI_CLEAR_QUERY_RESULTS),CollectionName);
                      result := TFRE_DB_STORE_DESC.create.Describe(FIdField,CSF(@IMI_GET_CHART_DATA),Nil,CSF(@IMI_DESTROY_STORE),nil,CollectionName);
                    end;
    else begin
      raise EFRE_DB_Exception.Create(edb_ERROR,'INVALID DISAPLAYTYPE FOR STORE [%d] GETSTOREDESCRIPTION',[ord(FDisplaytype)]);
    end;
  end;
end;

function TFRE_DB_DERIVED_COLLECTION.getDescriptionStoreId: String;
begin
  Result:=CollectionName;
end;


procedure TFRE_DB_DERIVED_COLLECTION.SetDisplayType(const CollectionDisplayType: TFRE_COLLECTION_DISPLAY_TYPE; const Flags: TFRE_COLLECTION_GRID_DISPLAY_FLAGS; const title: TFRE_DB_String; const TreeNodeCaptionFields: TFRE_DB_StringArray; const TreeNodeIconField: TFRE_DB_String; const item_menu_func: TFRE_DB_SERVER_FUNC_DESC; const item_details_func: TFRE_DB_SERVER_FUNC_DESC; const selection_dep_func: TFRE_DB_SERVER_FUNC_DESC; const tree_menu_func: TFRE_DB_SERVER_FUNC_DESC; const drop_func: TFRE_DB_SERVER_FUNC_DESC; const drag_func: TFRE_DB_SERVER_FUNC_DESC);
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
  if CollectionDisplayType=cdt_Treeview then begin
    FTransform.Free;
    FTransform         := TFRE_DB_TREE_TRANSFORM.Create;
    if Assigned(TreeNodeCaptionFields) then begin
      FTreeNodeCaption   := TreeNodeCaptionFields;
    end else begin
      SetLength(FTreeNodeCaption,1); FTreeNodeCaption[0]:='objname';
    end;
    FTreeNodeIconField := TreeNodeIconField;
  end;
end;

procedure TFRE_DB_DERIVED_COLLECTION.SetDisplayTypeChart(const title: TFRE_DB_String; const chart_type: TFRE_DB_CHART_TYPE; const series_field_names: TFRE_DB_StringArray; const use_series_colors:boolean; const use_series_labels : boolean;const series_labels: TFRE_DB_StringArray; const showLegend: Boolean; const maxValue: Integer);
var value_count : QWord;
    i           : integer;
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
  _FilterIt(false);
  value_count := FDBOList.Count;
end;

procedure TFRE_DB_DERIVED_COLLECTION.SetChildToParentLinkField(const fieldname: TFRE_DB_NameType);
begin
  FChildParentMode   := true;
  FParentChldLinkFld := uppercase(fieldname);
end;

procedure TFRE_DB_DERIVED_COLLECTION.SetParentToChildLinkField(const fieldname: TFRE_DB_NameType);
begin
  FChildParentMode   := false;
  FParentChldLinkFld := uppercase(fieldname);
end;

function TFRE_DB_DERIVED_COLLECTION.GetDisplayDescription: TFRE_DB_CONTENT_DESC;

  function GetListViewDescription: TFRE_DB_VIEW_LIST_DESC;
  var vcd : TFRE_DB_VIEW_LIST_LAYOUT_DESC;
  begin
    vcd := (FTransform as TFRE_DB_SIMPLE_TRANSFORM).GetViewCollectionDescription as TFRE_DB_VIEW_LIST_LAYOUT_DESC;
    result := TFRE_DB_VIEW_LIST_DESC.create.Describe(GetStoreDescription as TFRE_DB_STORE_DESC, vcd, FitemMenuFunc, FTitle,FGridDisplayFlags,FitemDetailsFunc,FselectionDepFunc,nil,FdropFunc,FdragFunc);
  end;

  function GetTreeViewDescription: TFRE_DB_VIEW_TREE_DESC;
  begin
    result := TFRE_DB_VIEW_TREE_DESC.create.Describe(GetStoreDescription as TFRE_DB_STORE_DESC, FTitle, FitemMenuFunc,FtreeMenuFunc);
  end;

  function GetChartDescription: TFRE_DB_CHART_DESC;
  var series_ids : TFRE_DB_StringArray;
      ct         : TFRE_DB_CHART_TRANSFORM;
      i          : integer;
  begin
    ct := FTransform as TFRE_DB_CHART_TRANSFORM; // Chart data must be Transformed now
    result := TFRE_DB_CHART_DESC.create.Describe(Ftitle,GetStoreDescription as TFRE_DB_STORE_DESC,ct.FseriesFieldNames,ct.FChartType,ct.FSeriesLabels,ct.FShowLegend,ct.FMaxValue);
  end;

begin
 case FDisplaytype of
   cdt_Listview:  result := GetListviewDescription;
   cdt_Treeview:  result := GetTreeViewDescription;
   cdt_Chartview: result := GetChartDescription;
   else raise EFRE_DB_Exception.Create(edb_ERROR,'DERIVED COLLECTION [%s] HAS AN INVALID DISPLAYTYPE SET [%d]',[CollectionName,ord(FDisplaytype)]);
 end;
end;

function TFRE_DB_DERIVED_COLLECTION.GetDisplayDescriptionFunction(const FilterEventKey: TFRE_DB_String): TFRE_DB_SERVER_FUNC_DESC;
begin
  result := CSF(@IMI_GET_DISPLAY_DESC);
  if FilterEventKey<>'' then begin
    result.AddParam.Describe('FILTER_EVENT',uppercase(FilterEventKey));
  end;
end;

procedure TFRE_DB_DERIVED_COLLECTION.AddVirtualChildEntry(const caption: TFRE_DB_String; const FuncClass: String; const uidPath: TFRE_DB_StringArray; const ChildrenFunc:String; const ContentFunc: String; const MenuFunc: String);
var entry : IFRE_DB_Object;
begin
  entry := GFRE_DBI.NewObject;
  entry.Field('lvl_cap').AsString       := caption;
  entry.Field('lvl_class').AsString     := FuncClass;
  entry.Field('lvl_uidpath').AsStringArr:= uidPath;
  entry.Field('lvl_chfun').AsString     := ChildrenFunc;
  entry.Field('lvl_cfun').AsString      := ContentFunc;
  entry.Field('lvl_mfun').AsString      := MenuFunc;
  FParentCollection.StoreI(entry);
end;

procedure TFRE_DB_DERIVED_COLLECTION._CheckObserverAdded(const add: boolean);
begin
  if add then
    begin
      if (not FObserverAdded) and
         ( (FDCMode=dc_Map2RealCollection)
           or (FDCMode=dc_Map2DerivedCollection)) then
             begin
               FObserverAdded    := true;
               FParentCollection.AddObserver(self);
             end;
    end
  else
    begin
     if (FObserverAdded) and
        ( (FDCMode=dc_Map2RealCollection)
          or (FDCMode=dc_Map2DerivedCollection)) then
            begin
              FObserverAdded    := False;
              FParentCollection.RemoveObserver(self);
            end;
    end;
end;


function TFRE_DB_DERIVED_COLLECTION.IMI_GET_GRID_DATA(const input: IFRE_DB_Object): IFRE_DB_Object;
var pageinfo       : TFRE_DB_DC_PAGING_INFO;
    order          : TFRE_DB_DC_ORDER_LIST;
    sortfilterkeys : TFRE_DB_DC_STRINGFIELDKEY_LIST;
    QueryID        : String;
    childcall      : boolean;
    exrefs         : TFRE_DB_GUIDArray;

    function GetGridDataDescription: TFRE_DB_CONTENT_DESC;
    var i            : integer;
       ok            : string;
       lOrderChanged : Boolean;

      procedure GetData(const obj:TFRE_DB_Object);
      begin
        TFRE_DB_STORE_DATA_DESC(result).addEntry(obj.CloneToNewObject());
      end;

    begin
      lOrderChanged := false;
      if Length(order) = length(FCurrentOrder) then begin
        for i:=0 to high(order) do begin
           if (order[i].ascending   <> FCurrentOrder[i].ascending) or
              (order[i].order_field <> FCurrentOrder[i].order_field) or
              (order[i].order_key   <> FCurrentOrder[i].order_key) then begin
                lOrderChanged := true;
                break;
           end;
        end;
      end else begin
        lOrderChanged := true;
      end;
      if lOrderChanged then begin
        FCurrentOrder := order;
      end;
     writeln('********* ORDERCHANGED : ',CollectionName,' ',lOrderChanged,'  ',FInitialDerived);

     if lOrderChanged or (not FInitialDerived)  then begin
       writeln(':::ORDERING IT NEW ',CollectionName);
        FInitialDerived := true;
        RemoveAllOrderFields;
        RemoveAllFiltersPrefix('*D_');
        for i:=0 to high(order) do begin
          with order[i] do begin
            ok := format('%2.2d',[order_key]);
            AddOrderField(ok,order_field,ascending);
          end;
        end;
        for i:=0 to high(sortfilterkeys) do begin
          with sortfilterkeys[i] do
            AddStringFieldFilter(filter_key,field_name,TFRE_DB_StringArray.Create(value),filtertype,on_transform,on_filter_field);
        end;
        if FUseDepAsLinkFilt then
          begin
            if Length(FDepObjectList)>0 then
              begin
                _DBConnection.ExpandReferences(FDepObjectList,FDepObjectsRefers,FDepRefConstraint,exrefs);
                if FDepObjectsRefNeg then
                  AddUIDFieldFilter('*RLF*','uid',exrefs,dbnf_NoValueInFilter)
                else
                  AddUIDFieldFilter('*RLF*','uid',exrefs,dbnf_OneValueFromFilter);
              end
            else
              begin
                result := TFRE_DB_STORE_DATA_DESC.create;
                TFRE_DB_STORE_DATA_DESC(Result).Describe(0);
                exit;
              end;
          end;
        _FilterIt(childcall);
      end;
      result := TFRE_DB_STORE_DATA_DESC.create;
      ApplyToPage(QueryID,pageinfo,@GetData);
      TFRE_DB_STORE_DATA_DESC(Result).Describe(ItemCount);
    end;

begin
  //writeln(input.DumpToString());
  _CheckObserverAdded(true);
  FDependencyObject :=  input.FieldOnlyExistingObj('DEPENDENCY');
  FDepObjectList :=  _GET_DC_ReferenceList(input);
  case FDCMode of
    dc_None: ;
    dc_Map2RealCollection: ;
    dc_VirtualCollection: ;
    dc_Map2DerivedCollection: ;
    dc_ReferentialLinkCollection:
        begin
          FInitialDerived := false; // force it
          _DBConnection.ExpandReferences(FDepObjectList,FDepObjectsRefers,FDepRefConstraint,FExpandedRefs,true);
        end;
  end;
  if input.FieldExists('parentid') then
    begin
      FParentIds   :=FREDB_String2GuidArray(input.Field('parentid').AsString);
      if Length(FParentIds)>0 then
        begin
          FParentIds[0] := FParentIds[high(FParentIds)];
          SetLength(FParentIds,1);
        end;
      _DBConnection.ExpandReferences(FParentIds,not FChildParentMode,TFRE_DB_StringArray.create(FParentChldLinkFld),FExpandedRefs,true);
    end
  else
    SetLength(FParentIds,0);

  if (cdgf_Children in FGridDisplayFlags)
     or FUseDepAsLinkFilt then
       FInitialDerived := false; // force refresh

  pageinfo       := _Get_DC_PageingInfo(input);
  order          := _Get_DC_Order(input);
  sortfilterkeys := _Get_DC_StringfieldKeys(input);
  QueryID        := _Get_DC_QueryID(input);
  childcall      := length(FParentIds)>0;
  result := GetGridDataDescription;
end;

function TFRE_DB_DERIVED_COLLECTION.IMI_GET_CHART_DATA(const input: IFRE_DB_Object): IFRE_DB_Object;
var
    pageinfo       : TFRE_DB_DC_PAGING_INFO;
     order         : TFRE_DB_DC_ORDER_LIST;
    sortfilterkeys : TFRE_DB_DC_STRINGFIELDKEY_LIST;
    QueryID        : String;
    series_id      : string;
    ChartTransForm : TFRE_DB_CHART_TRANSFORM;
     i      : integer;


  function GetChartDataDescription:TFRE_DB_CHART_DATA_DESC;
  var ok     : string;
      data   : TFRE_DB_Real32Array;
      uids   : TFRE_DB_GUIDArray;
      colors : TFRE_DB_StringArray;
      texts  : TFRE_DB_StringArray;
      leg    : TFRE_DB_StringArray;
      max    : Integer;

      procedure GetData(const obj:TFRE_DB_Object);
      var
       res    : TFRE_DB_CHART_DATA_DESC;
     begin
       //uids[i] := obj.Field(series_id+'_uid').AsGUID;
       uids[i] := obj.UID;
       data[i] := obj.Field(series_id).AsReal32;
       if ChartTransForm.FUseSeriesColors then begin
         colors[i] := obj.Field(series_id+'_col').AsString;
       end;
       if ChartTransForm.FUseSeriesLabels then begin
         texts[i] := obj.Field(series_id+'_lbl').AsString;
       end;
       if ChartTransForm.FShowLegend then begin
         leg[i] := obj.Field(series_id+'_leg').AsString;
       end;
       inc(i);
     end;

  begin
    series_id := input.FieldPath('query.sid').AsString;
    GFRE_DB.LogDebug(dblc_DB,'GET_CHART_DATA '+series_id);
    GFRE_DB.LogDebug(dblc_DB,'%s',[input.DumpToString(4)]);

    max            := FDBOList.Count;
    ChartTransForm := FTransform as TFRE_DB_CHART_TRANSFORM;

    colors := nil;
    texts  := nil;
    leg    := nil;
    SetLength(data,max);
    SetLength(uids,max);
    if ChartTransForm.FUseSeriesColors then begin
      SetLength(colors,max);
    end;
    if ChartTransForm.FUseSeriesLabels then begin
      SetLength(texts,max);
    end;
    if ChartTransForm.FShowLegend then begin
      SetLength(leg,max);
    end;
    i:=0;
    ApplyToData(@GetData);
    result := TFRE_DB_CHART_DATA_DESC.create;
    result.setSeries(data,uids,colors,texts,leg);
  end;

begin
   _CheckObserverAdded(true);
  result := GetChartDataDescription;
end;

function TFRE_DB_DERIVED_COLLECTION.IMI_CLEAR_QUERY_RESULTS(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
//  writeln('CLEAR GRID DATA '+input.DumpToString);
  RemoveQueryIDWatch(_Get_DC_QueryID(input));
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_DERIVED_COLLECTION.IMI_GET_CHILDREN_DATA(const input: IFRE_DB_Object): IFRE_DB_Object;
var pageinfo       : TFRE_DB_DC_PAGING_INFO;
     order         : TFRE_DB_DC_ORDER_LIST;
    sortfilterkeys : TFRE_DB_DC_STRINGFIELDKEY_LIST;
    QueryID        : String;

  function GetChildrenDataDescription:TFRE_DB_STORE_DATA_DESC;
  var i  : integer;
      ok : string;

      procedure GetData(const obj:TFRE_DB_Object);
      var
        entry       : IFRE_DB_Object;
        i           : Integer;
      begin
        entry:=GFRE_DB.NewObjectI;
        entry.Field('_icon_').AsString := obj.Field(FTreeNodeIconField).AsString;
        case FDCMode of
          dc_None: raise EFRE_DB_Exception.Create(edb_ERROR,'INVALID DC MODE IN IMI_GET_CHILDREN_DATA');
          dc_Map2RealCollection: begin
                                 for i := 0 to Length(FTreeNodeCaption) - 1 do begin
                                   if obj.FieldExists(FTreeNodeCaption[i]) then begin
                                     entry.Field(FTreeNodeCaption[i]).AsString:=obj.Field(FTreeNodeCaption[i]).AsString;
                                   end;
                                 end;
                                 entry.Field('uid').AsGUID:=obj.UID;
                                 entry.Field('uidpath').AsStringArr:=obj.GetUIDPath;
                                 entry.Field('_funcclassname_').AsString:=obj.SchemeClass;
                                 entry.Field('_childrenfunc_').AsString:='ChildrenData';
                                 entry.Field('_menufunc_').AsString:='Menu';
                                 entry.Field('_contentfunc_').AsString:='Content';
                                 if obj.MethodExists('CHILDRENDATA') then begin
                                   entry.Field('children').AsString:='UNCHECKED';
                                 end;
                               end;
          dc_VirtualCollection:begin // has a serverfunction for the next level
                                 entry.Field(FTreeNodeCaption[0]).AsString:=obj.Field('lvl_cap').AsString;
                                 entry.Field('uid').AsGUID:=obj.UID;
                                 entry.Field('uidpath').AsStringArr:=obj.Field('lvl_uidpath').AsStringArr;
                                 entry.Field('_funcclassname_').AsString:=obj.Field('lvl_class').AsString;
                                 entry.Field('_childrenfunc_').AsString:=obj.FieldPath('lvl_chfun').AsString;
                                 entry.Field('_menufunc_').AsString:=obj.Field('lvl_mfun').AsString;
                                 entry.Field('_contentfunc_').AsString:=obj.Field('lvl_cfun').AsString;
                                 entry.Field('children').AsString:='UNCHECKED';
                               end;
        end;
        TFRE_DB_STORE_DATA_DESC(result).addEntry(entry);
      end;

  begin
    GFRE_DB.LogDebug(dblc_DB,'IMI_GET_CHILDREN_DATA');
    GFRE_DB.LogDebug(dblc_DB,'%s',[input.DumpToString(4)]);
    pageinfo.count:=1000;
    pageinfo.start:=0;

    RemoveAllOrderFields;
    RemoveAllFiltersPrefix('*D_');

    if input.FieldExists('DEPENDENCY') and (input.Field('DEPENDENCY').FieldType=fdbft_Object) then begin
      DC_SetFilters_From_Input(input.Field('DEPENDENCY').AsObject.Implementor as TFRE_DB_Object);
    end;
    for i:=0 to Length(FTreeNodeCaption) - 1 do begin
      AddOrderField('A',FTreeNodeCaption[i],true);
    end;
    result := TFRE_DB_STORE_DATA_DESC.create;
    Derive;
    ApplyToPage(QueryID,pageinfo,@GetData);
  end;

begin
  _CheckObserverAdded(true);
  result := GetChildrenDataDescription;
end;

function TFRE_DB_DERIVED_COLLECTION.IMI_GET_DISPLAY_DESC(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  if Assigned(input) then begin
    writeln('GRID GET DISPLAY KEY ',input.DumpToString);
  end;
  result := GetDisplayDescription;
end;

function TFRE_DB_DERIVED_COLLECTION.IMI_DESTROY_STORE(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  writeln('DESTROY STORE ',input.DumpToString());
  _CheckObserverAdded(false);
  result := GFRE_DB_NIL_DESC;
end;



procedure TFRE_DB_TRANSFORMOBJECT.SetKey(const AValue: TFRE_DB_String);
begin
  Fkeyfield.AsString := AValue;
end;

procedure TFRE_DB_TRANSFORMOBJECT._SetHasFilterField(const value: boolean);
begin
  Field('HFF').AsBoolean:=value;
end;

function TFRE_DB_TRANSFORMOBJECT._ObjectsNeedsNoSubfieldSchemeCheck: boolean;
begin
  Result:=true;
end;

procedure TFRE_DB_TRANSFORMOBJECT.InternalSetup;
begin
  inherited InternalSetup;
  Fkeyfield := Field('FK');
  _SetHasFilterField(false);
end;

function TFRE_DB_TRANSFORMOBJECT.TransformInOut(const dependency_obj: IFRE_DB_Object; const input: TFRE_DB_Object; const filter_fields: boolean): TFRE_DB_Object;
begin

end;

procedure TFRE_DB_TRANSFORMOBJECT.TransformOutIn(const transformed: TFRE_DB_Object);
begin

end;

function TFRE_DB_TRANSFORMOBJECT.GetKey: TFRE_DB_String;
begin
  result := Fkeyfield.AsString;
end;

function TFRE_DB_TRANSFORMOBJECT.GetHasFF: boolean;
begin
  result := Field('HFF').AsBoolean;
end;

//procedure TFRE_DB_SCHEME_COLLECTION.InternalSetup;
//begin
//  inherited InternalSetup;
//  if FDBO_State=fdbos_Creating then begin
//    _SetIndexField('C',fdbft_String,true,True);
//  end;
//end;

function TFRE_DB_SCHEME_COLLECTION.Store(var obj: TFRE_DB_Object): TFRE_DB_Errortype;
begin
  if not (obj is TFRE_DB_SchemeObject) then GFRE_BT.CriticalAbort('internal - only scheme objects in scheme store!');
  Result:=inherited Store(obj);
end;

function TFRE_DB_SCHEME_COLLECTION.StoreScheme(var obj: TFRE_DB_SchemeObject): TFRE_DB_Errortype;
begin
  obj._SelfCheckValid;
  obj._Seal;
  result:=Store(TFRE_DB_Object(obj));
  obj:=nil;
end;

function TFRE_DB_SCHEME_COLLECTION.GetScheme(const scheme_name: TFRE_DB_String; var scheme: TFRE_DB_SchemeObject): boolean;
var lUID    : TGUID;
    fd      : TFRE_DB_FieldSchemeDefinition;
begin
  //if IsTemporary then begin
    result := GetIndexedObj(scheme_name,TFRE_DB_Object(scheme));
    //if result then begin
    //  abort;
    //  //if not FObjectLinkStore.Find(lUID,lManInf) then raise EFRE_DB_Exception.Create(edb_INTERNAL,'inconsistency cannot find an unmanaged object but it is in th index?');
    //  //scheme := lManInf.obj_link as TFRE_DB_SchemeObject;
    //  //scheme.FManageInfo.Connection:=nil;
    //end;
  //end else begin
    //raise EFRE_DB_Exception.Create(edb_ERROR,'NON TEMPORARY SCHEME COLLECTIONS NOT SUPPORTED FROM NOW ON (09042013)');
    //result := GetIndexed(uppercase(scheme_name),lUID);
    //if result then begin
    //  if not _DBConnectionBC(true).Fetch(lUID,TFRE_DB_Object(scheme)) then raise EFRE_DB_Exception.Create(edb_INTERNAL,'database inconsistent / cant fetch indexed scheme object');
    //  scheme.FSchemeType:=dbst_DB;
    //end;
  //end;
end;

function TFRE_DB_SCHEME_COLLECTION.SchemeExists(const scheme_name: TFRE_DB_String): boolean;
begin
  result := ExistsIndexed(scheme_name);
end;

function TFRE_DB_SCHEME_COLLECTION.RemoveScheme(const obj: TFRE_DB_SchemeObject): boolean;
begin
  result:=Remove(obj.UID);
end;


function TFRE_DB_FieldSchemeDefinition.GetFieldName: TFRE_DB_String;
begin
  result := FNameField.AsString;
end;

function TFRE_DB_FieldSchemeDefinition.getEnum: TFRE_DB_Enum;
var enumname:TFRE_DB_String;
begin
  result := nil;
  enumname   := Fenum.AsString;
  if enumname ='' then exit(nil);
  if not getParentScheme.GetEnumFromDB(enumname,result) then begin
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'the enum[%s] could not be fetched from the database',[enumname]);
  end;
end;

function TFRE_DB_FieldSchemeDefinition.getAddConfirm: Boolean;
begin
  Result:=FaddConfirm.AsBoolean;
end;

function TFRE_DB_FieldSchemeDefinition.getEnumI: IFRE_DB_Enum;
begin
  result := getEnum;
end;

function TFRE_DB_FieldSchemeDefinition.GetFieldType: TFRE_DB_FIELDTYPE;
begin
  result := FieldtypeShortString2Fieldtype(FTypeField.AsString);
end;

function TFRE_DB_FieldSchemeDefinition.getIsPass: Boolean;
begin
  Result:=FisPass.AsBoolean;
end;

function TFRE_DB_FieldSchemeDefinition.getMultiValues: Boolean;
begin
  Result:=FmultiValues.AsBoolean;
end;

function TFRE_DB_FieldSchemeDefinition.getRequired: Boolean;
begin
  Result:=Frequired.AsBoolean;
end;


function TFRE_DB_FieldSchemeDefinition.GetSubSchemeName: TFRE_DB_String;
begin
  result := FSubscheme.AsString;
end;

function TFRE_DB_FieldSchemeDefinition.getValidator: TFRE_DB_ClientFieldValidator;
var validatorname:TFRE_DB_String;
begin
  validatorname := Fvalidator.AsString;
  if validatorname='' then exit(nil);
  if not getParentScheme.GetValidatorFromDB(validatorname,result) then begin
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'the client field validator[%s] could not be fetched from the database',[validatorname]);
  end;
end;

function TFRE_DB_FieldSchemeDefinition.getValidatorI: IFRE_DB_ClientFieldValidator;
begin
  result := getValidator;
end;

procedure TFRE_DB_FieldSchemeDefinition.setAddConfirm(AValue: Boolean);
begin
  FaddConfirm.AsBoolean:=AValue;
end;

procedure TFRE_DB_FieldSchemeDefinition.setisPass(AValue: Boolean);
begin
  FisPass.AsBoolean:=AValue;
end;

function TFRE_DB_FieldSchemeDefinition.getDepFields: TFRE_DB_ObjectArray;
begin
  Result:=FdepField.AsObjectArr;
end;

function TFRE_DB_FieldSchemeDefinition.getDepFieldsI: IFRE_DB_ObjectArray;
begin
  result := FdepField.GetAsObjectArrayI;
end;

procedure TFRE_DB_FieldSchemeDefinition.setEnum(AValue: TFRE_DB_Enum);
begin
  if Assigned(AValue) then begin
    Fenum.AsString:=AValue.ObjectName;
  end else begin
    Fenum.Clear;
  end;
end;

procedure TFRE_DB_FieldSchemeDefinition.setMultiValues(AValue: Boolean);
begin
  FmultiValues.AsBoolean:=AValue;
end;

procedure TFRE_DB_FieldSchemeDefinition.setRequired(AValue: Boolean);
begin
  Frequired.AsBoolean:=AValue;
end;

procedure TFRE_DB_FieldSchemeDefinition.setValidator(AValue: TFRE_DB_ClientFieldValidator);
var
  params: TFRE_DB_Object;
begin
  if Assigned(AValue) then begin
    Fvalidator.AsString:=AValue.ObjectName;
  end else begin
    Fvalidator.Clear;
  end;end;

procedure TFRE_DB_FieldSchemeDefinition.setEnumI(AValue: IFRE_DB_Enum);
begin
  setEnum(AValue.Implementor as TFRE_DB_Enum);
end;

procedure TFRE_DB_FieldSchemeDefinition.setValidatorI(AValue: IFRE_DB_ClientFieldValidator);
begin
  setValidator(AValue.Implementor as TFRE_DB_ClientFieldValidator);
end;

function TFRE_DB_FieldSchemeDefinition._ObjectIsCodeclassOnlyAndHasNoScheme: boolean;
begin
  Result:=true;
end;

procedure TFRE_DB_FieldSchemeDefinition.addDepField(const fieldName: TFRE_DB_String;const disablesField:Boolean);
var
  tmpField: TFRE_DB_FieldSchemeDefinition;
  depObj  : TFRE_DB_Object;
begin
  if not getParentScheme.GetSchemeField(fieldName,tmpField) then begin
    raise EFRE_DB_Exception.Create(edb_ERROR,'Dependent field ' + fieldName + ' not found');
  end;
  if not (FieldType=fdbft_Boolean) then begin
    raise EFRE_DB_Exception.Create(edb_ERROR,'Dependent fields can only be defined on boolean fields');
  end;
  depObj:=GFRE_DB.NewObject;
  depObj.Field('fieldName').AsString:=fieldName;
  depObj.Field('disablesField').AsBoolean:=disablesField;
  FdepField.AddObject(depObj);
end;

function TFRE_DB_FieldSchemeDefinition.getParentScheme: TFRE_DB_SchemeObject;
begin
  Result:=Parent.Parent as TFRE_DB_SchemeObject;
end;


procedure TFRE_DB_FieldSchemeDefinition.InternalSetup;
begin
  inherited InternalSetup;
  FNameField       := _Field('NAM');
  FTypeField       := _Field('TYP');
  FSubscheme       := _Field('SUB');
  Fenum            := _Field('ENUM');
  Fvalidator       := _Field('VAL');
  FvalidatorParams := _Field('VALP');
  FmultiValues     := _Field('MV');
  Frequired        := _Field('REQ');
  FisPass          := _Field('PAS');
  FaddConfirm      := _Field('CON');
  FdepField        := _Field('DEP');
  FCalcMethod      := _Field('CLC');
end;

function TFRE_DB_FieldSchemeDefinition.Field(const name: TFRE_DB_String): TFRE_DB_FIELD;
begin
  raise EFRE_DB_Exception.Create(edb_ERROR,'Direct FIELD access is NOT ALLOWED in fieldschemedefinition objects');
end;

function TFRE_DB_FieldSchemeDefinition._CalculateFieldRes(const for_dbo: TFRE_DB_Object): TFRE_DB_Object;
begin

end;

function TFRE_DB_FieldSchemeDefinition.SetupFieldDef(const is_required: boolean; const is_multivalue: boolean; const enum_key: TFRE_DB_String; const validator_key: TFRE_DB_String; const is_pass: Boolean; const add_confirm: Boolean; const validator_params: TFRE_DB_Object): TFRE_DB_FieldSchemeDefinition;
var lEnum  : TFRE_DB_Enum;
    lValid : TFRE_DB_ClientFieldValidator;
begin
  required    := is_required;
  multiValues := is_multivalue;
  isPass      := is_pass;
  addConfirm  := add_confirm;
  if enum_key<>'' then begin
    if getParentScheme.GetEnumFromDB(enum_key,lEnum) then begin
      setEnum(lEnum);
    end else begin
      raise EFRE_DB_Exception.Create(edb_INTERNAL,'the client field enum[%s] could not be fetched from the database',[enum_key]);
    end;
  end;
  if validator_key<>'' then begin
    if getParentScheme.GetValidatorFromDB(validator_key,lValid) then begin
      setValidator(lValid);
      if assigned(validator_params) then
        FvalidatorParams.AsObject := validator_params;
    end else begin
      raise EFRE_DB_Exception.Create(edb_INTERNAL,'the client field validator[%s] could not be fetched from the database',[validator_key]);
    end;
  end;

  result:=self;
end;

function TFRE_DB_FieldSchemeDefinition.SetupFieldDefI(const is_required: boolean; const is_multivalue: boolean; const enum_key: TFRE_DB_String; const validator_key: TFRE_DB_String; const is_pass: Boolean; const add_confirm: Boolean; const validator_params: IFRE_DB_Object): IFRE_DB_FieldSchemeDefinition;
begin
  if assigned(validator_params) then
    result := SetupFieldDef(is_required,is_multivalue,enum_key,validator_key,is_pass,add_confirm,validator_params.Implementor_HC as TFRE_DB_Object)
  else
    result := SetupFieldDef(is_required,is_multivalue,enum_key,validator_key,is_pass,add_confirm,nil);
end;

procedure TFRE_DB_FieldSchemeDefinition.SetCalcMethod(const calc_methodname: TFRE_DB_String);
begin
  FCalcMethod.AsString:=uppercase(calc_methodname);
end;

function TFRE_DB_FieldSchemeDefinition.CalcField(const calc_method: IFRE_DB_InvokeInstanceMethod): TFRE_DB_FIELD;
begin
  if FieldType<>fdbft_CalcField then raise EFRE_DB_Exception.Create(edb_INTERNAL,'access to calcfield def, of a non calcfield');
  if not Assigned(FMyFakeCalcFld) then begin
    FMyFakeCalcFld_Name := FieldName;
    FMyFakeCalcFld      := TFRE_DB_FIELD.Create(self,fdbft_CalcField);
    FMyFakeCalcFld.FFieldName:=@FMyFakeCalcFld_Name;
  end;
  FMyFakeCalcFld.FCurrentCalcMethod := calc_method;
  result                            := FMyFakeCalcFld;
end;


function TFRE_DB_FieldSchemeDefinition.GetSubScheme: TFRE_DB_SchemeObject;
var scheme_name:TFRE_DB_String;
begin
  result := nil;
  scheme_name := SubschemeName;
  if (scheme_name<>'') and (FSubSchemeObj=nil) then begin
   // writeln('PARENT IS : ',parent.Parent.ClassName);
    if (Parent.Parent as TFRE_DB_SchemeObject).GetSchemeFromDB(scheme_name,FSubSchemeObj)=false then begin
      raise EFRE_DB_Exception.Create(edb_INTERNAL,'critical subscheme inconsistency cant fetch subfieldscheme '+scheme_name+' for '+(Parent as TFRE_DB_SchemeObject).DefinedSchemeName);
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
  if uppercase(field_to_check.FieldName) <> uppercase(FieldName)   then raise EFRE_DB_Exception.Create(edb_INTERNAL,'fieldvalidation: scheme[%s] fieldnames differ ? [%s]<>[%s]',[getParentScheme.DefinedSchemeName,field_to_check.FieldName,FieldName]);
  if field_to_check.FieldType <> FieldType then RaiseOrExit('fieldvalidation: scheme[%s] field [%s] the fieldtype[%s] of the object validated against the schemefieldtype[%s] is different',[getParentScheme.DefinedSchemeName,field_to_check.FieldName,field_to_check.FieldTypeAsString,CFRE_DB_FIELDTYPE[FieldType]]); if not result then exit;
  lValidator := getValidator;
  if (not multiValues) and (field_to_check.ValueCount>1) then RaiseOrExit('fieldvalidation: scheme[%s] field[%s / %s] should not have multivalues (array), count=%d',[getParentScheme.DefinedSchemeName,field_to_check.FieldName,field_to_check.FieldTypeAsString,field_to_check.ValueCount]);  if not result then exit;
  if assigned(lValidator) then begin
    result   := lValidator.CheckField(field_to_check,raise_exception); if not Result then exit;
  end;
  lEnum      := getEnum;
  if assigned(lEnum) then begin
    result   := lEnum.CheckField(field_to_check,raise_exception);
    if not Result then exit;
  end;
  if FieldType=fdbft_Object then begin
    //writeln('SUB VALID CHECK ::::: ',FieldName);
    //if FieldName='deliveryaddress' then begin
    //  FieldName;
    //end;
    if SubschemeName<>field_to_check.AsObject.SchemeClass then RaiseOrExit('fieldvalidation: scheme[%s] field [%s] -> the subfieldscheme[%s] of the object validated against the scheme defined subfieldtype[%s] is different',[getParentScheme.DefinedSchemeName,field_to_check.FieldName,field_to_check.AsObject.SchemeClass,SubschemeName]); if not result then exit;
    GetSubScheme.ValidateObject(field_to_check.AsObject,raise_exception);
  end;
end;

function TFRE_DB_FieldSchemeDefinition.ValidateFieldI(const field_to_check: IFRE_DB_FIELD; const raise_exception: boolean): boolean;
begin
  result := ValidateField(field_to_check.Implementor as TFRE_DB_FIELD,raise_exception);
end;


function TFRE_DB_SchemeObject.AddSchemeField(const newfieldname: TFRE_DB_String; const newfieldtype: TFRE_DB_FIELDTYPE):TFRE_DB_FieldSchemeDefinition;
var lFieldSchemeDefinition:TFRE_DB_FieldSchemeDefinition;
begin
  if newfieldtype=fdbft_Object then raise EFRE_DB_Exception.Create(edb_ERROR,'AddschemeField: schemefield '+newfieldname+', objectfields must be added with AddSchemeFieldSubscheme');
  if FFieldDefs._Field(newfieldname).FieldType<>fdbft_NotFound then raise EFRE_DB_Exception.Create(edb_ERROR,'AddschemeField: schemefield '+newfieldname+' is already set');
  lFieldSchemeDefinition                       := TFRE_DB_FieldSchemeDefinition.Create;
  FFieldDefs._Field(newfieldname).AsObject     := lFieldSchemeDefinition;
  lFieldSchemeDefinition.FNameField.AsString   := uppercase(newfieldname);
  lFieldSchemeDefinition.FTypeField.AsString   := CFRE_DB_FIELDTYPE_SHORT[newfieldtype];
  lFieldSchemeDefinition.Frequired.AsBoolean   := false;
  lFieldSchemeDefinition.FisPass.AsBoolean     := false;
  lFieldSchemeDefinition.FaddConfirm.AsBoolean := false;
  lFieldSchemeDefinition.FmultiValues.AsBoolean:= false;
  result                                       := lFieldSchemeDefinition;
end;

function TFRE_DB_SchemeObject.AddSchemeFieldI(const newfieldname: TFRE_DB_String; const newfieldtype: TFRE_DB_FIELDTYPE): IFRE_DB_FieldSchemeDefinition;
begin
  result := AddSchemeField(newfieldname,newfieldtype);
end;

procedure TFRE_DB_SchemeObject.RemoveSchemeField(const fieldname: TFRE_DB_String);
begin
  FFieldDefs.DeleteField(fieldname);
end;

procedure TFRE_DB_SchemeObject.AddCalculatedField(const newfieldname, calc_method_name: TFRE_DB_String; const calculation_type: TFRE_DB_CalcFieldTime);
begin
  AddSchemeField(newfieldname,fdbft_CalcField).SetCalcMethod(calc_method_name);
end;

function TFRE_DB_SchemeObject.AddSchemeFieldSubscheme(const newfieldname:TFRE_DB_String ; const sub_scheme:TFRE_DB_String):TFRE_DB_FieldSchemeDefinition;
var lFieldSchemeDefinition : TFRE_DB_FieldSchemeDefinition;
    lSubscheme             : TFRE_DB_SchemeObject;
begin
  if FFieldDefs._Field(newfieldname).FieldType<>fdbft_NotFound then raise EFRE_DB_Exception.Create(edb_ERROR,'AddschemeFieldSubscheme: schemefield '+newfieldname+' is already set');
  if not GetSchemeFromDB(sub_scheme,lsubscheme) then raise EFRE_DB_Exception.Create(edb_ERROR,'AddschemeFieldSubscheme: schemefield '+newfieldname+' subscheme '+sub_scheme+' was not found');
  lFieldSchemeDefinition                       := TFRE_DB_FieldSchemeDefinition.Create;
  FFieldDefs._Field(newfieldname).AsObject     := lFieldSchemeDefinition;
  lFieldSchemeDefinition.FNameField.AsString   := newfieldname;
  lFieldSchemeDefinition.FTypeField.AsString   := CFRE_DB_FIELDTYPE_SHORT[fdbft_Object];
  lFieldSchemeDefinition.FSubscheme.AsString   := sub_scheme;
  lFieldSchemeDefinition.Frequired.AsBoolean   := false;
  lFieldSchemeDefinition.FisPass.AsBoolean     := false;
  lFieldSchemeDefinition.FaddConfirm.AsBoolean := false;
  lFieldSchemeDefinition.FmultiValues.AsBoolean:= false;
  result                                       := lFieldSchemeDefinition;
end;

function TFRE_DB_SchemeObject.AddSchemeFieldSubschemeI(const newfieldname: TFRE_DB_String; const sub_scheme: TFRE_DB_String): IFRE_DB_FieldSchemeDefinition;
begin
  result := AddSchemeFieldSubscheme(newfieldname,sub_scheme);
end;


function TFRE_DB_SchemeObject.GetSchemeField(const fieldname: TFRE_DB_String;var fieldschemedef:TFRE_DB_FieldSchemeDefinition): boolean;
var current : TFRE_DB_SchemeObject;
begin
  result  := false;
  current := self;
  if fieldname='UID' then exit;
  repeat
    if current.FFieldDefs.FieldExists(fieldname) then begin
      fieldschemedef := TFRE_DB_FieldSchemeDefinition(current.FFieldDefs._Field(fieldname).AsObject);
      exit(true);
    end;
    current := current.GetParentScheme;
    if current=nil then break;
  until false;
end;

function TFRE_DB_SchemeObject.GetSchemeFieldI(const fieldname: TFRE_DB_String; var fieldschemedef: IFRE_DB_FieldSchemeDefinition): boolean;
var lFieldSchemeDef : TFRE_DB_FieldSchemeDefinition;
begin
  result := GetSchemeField(fieldname,lFieldSchemeDef);
  if result then begin
    fieldschemedef := lFieldSchemeDef;
  end else begin
    fieldschemedef := nil;
  end;
end;

function TFRE_DB_SchemeObject.GetSchemeField(const fieldname: TFRE_DB_String): TFRE_DB_FieldSchemeDefinition;
begin
 if not GetSchemeField(fieldname,result) then raise EFRE_DB_Exception.Create(edb_ERROR,'GetSchemeField: schemefield '+fieldname+', objectfield does not exist');
end;

function TFRE_DB_SchemeObject.GetSchemeFieldI(const fieldname: TFRE_DB_String): IFRE_DB_FieldSchemeDefinition;
begin
 result:=GetSchemeField(fieldname);
end;


function TFRE_DB_SchemeObject.UpdateSchemeField(const oldField: TFRE_DB_FieldSchemeDefinition; const newfieldname: TFRE_DB_String; const newfieldtype: TFRE_DB_FIELDTYPE): TFRE_DB_FieldSchemeDefinition;
var lFieldSchemeDefinition:TFRE_DB_FieldSchemeDefinition;
begin
  if newfieldtype=fdbft_Object then raise EFRE_DB_Exception.Create(edb_ERROR,'UpdateschemeField: schemefield '+newfieldname+', objectfields must be added with AddSchemeFieldSubscheme');
  if oldField.FieldName<>newfieldname then begin
    if FFieldDefs._Field(newfieldname).FieldType<>fdbft_NotFound then raise EFRE_DB_Exception.Create(edb_ERROR,'UpdateschemeField: schemefield '+newfieldname+' is already set');
    lFieldSchemeDefinition                     := TFRE_DB_FieldSchemeDefinition(oldField.CloneToNewObject);
    FFieldDefs._Field(oldField.FieldName).Clear;
    FFieldDefs._Field(newfieldname).AsObject   := lFieldSchemeDefinition;
    lFieldSchemeDefinition.FNameField.AsString := uppercase(newfieldname);
  end else begin
    lFieldSchemeDefinition:=oldField;
  end;
  lFieldSchemeDefinition.FTypeField.AsString := CFRE_DB_FIELDTYPE_SHORT[newfieldtype];
  result                                     := lFieldSchemeDefinition;
end;

function TFRE_DB_SchemeObject.UpdateSchemeFieldI(const oldField: IFRE_DB_FieldSchemeDefinition; const newfieldname: TFRE_DB_String; const newfieldtype: TFRE_DB_FIELDTYPE): IFRE_DB_FieldSchemeDefinition;
begin
 result := UpdateSchemeField(oldField.implementor as TFRE_DB_FieldSchemeDefinition,newfieldname,newfieldtype);
end;

function TFRE_DB_SchemeObject.IsA(const schemename: TFRE_DB_String): Boolean;
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

function TFRE_DB_SchemeObject.AddUniqueKey(const KeyName: TFRE_DB_String; const FieldNames: TFRE_DB_StringArray): TFRE_DB_Errortype;
var uniquekey : TFRE_DB_Object;
    i         : integer;
begin
  result:=edb_OK;
  for i:=0 to high(FieldNames) do begin
    if not FFieldDefs.FieldExists(FieldNames[i]) then exit(edb_NOT_FOUND);
  end;
  if FUniqueKeys._Field(KeyName).FieldType=fdbft_NotFound then begin
    uniquekey := TFRE_DB_Object.Create;
    uniquekey._Field('Fields').AsStringArr := FieldNames;
    FUniqueKeys._Field(KeyName).AsObject   := uniquekey;
  end else begin
    result := edb_EXISTS;
  end;
end;

procedure TFRE_DB_SchemeObject.SetSimpleSysDisplayField(const field_name: TFRE_DB_String);
begin
  FDisplayField.Field('simple_df').AsString:=field_name;
end;

procedure TFRE_DB_SchemeObject.SetSysDisplayField(const field_names: TFRE_DB_StringArray; const format: TFRE_DB_String);
begin
  FDisplayField.Field('formatted_df').AsStringArr := field_names;
  FDisplayField.Field('formatted_dff').AsString   := format;
end;

function TFRE_DB_SchemeObject.GetFormattedDisplay(const obj : TFRE_DB_Object): TFRE_DB_String;
var dfa : TFRE_DB_StringArray;
    dff : TFRE_DB_StringArray;
    form: TFRE_DB_String;
      i : Integer;
begin
  if obj is TFRE_DB_SchemeObject then begin
    result := DefinedSchemeName;
  end else begin
    if FDisplayField.FieldExists('formatted_df') then begin
      dfa    := FDisplayField.Field('formatted_df').AsStringArr;
      form   := FDisplayField.Field('formatted_dff').AsString;
      result := obj.FieldPathListFormat(dfa,form,'{}');
    end else
    if FDisplayField.FieldExists('simple_df') then begin
      result := obj.FieldPathListFormat(GFRE_DB.ConstructStringArray([FDisplayField.Field('simple_df').AsString]),'%s','{}');
    end else begin
      result := DefinedSchemeName+' : '+obj.UID_String;
    end;
  end;
end;

function TFRE_DB_SchemeObject.GetFormattedDisplayI(const obj: IFRE_DB_Object): TFRE_DB_String;
begin
  result := GetFormattedDisplay(obj.Implementor as TFRE_DB_Object);
end;

function TFRE_DB_SchemeObject.FormattedDisplayAvailable(const obj : TFRE_DB_Object): boolean;
var dfa : TFRE_DB_StringArray;
    dff : TFRE_DB_StringArray;
    form: TFRE_DB_String;
      i : Integer;
begin
  if obj is TFRE_DB_SchemeObject then begin
    result := true;
  end else begin
    if FDisplayField.FieldExists('formatted_df') then begin
      result := true;
    end else
    if FDisplayField.FieldExists('simple_df') then begin
      result := true;
    end else begin
      result := false;
    end;
  end;
end;

function TFRE_DB_SchemeObject.FormattedDisplayAvailableI(const obj: IFRE_DB_Object): boolean;
begin
  result := FormattedDisplayAvailable(obj.Implementor as TFRE_DB_Object);
end;

function TFRE_DB_SchemeObject.DefinedSchemeName: TFRE_DB_String;
begin
  result := uppercase(FSchemeClass.AsString);
end;

procedure TFRE_DB_SchemeObject.Strict(const only_defined_fields: boolean);
begin
  FStrict.AsBoolean := only_defined_fields;
end;


procedure TFRE_DB_SchemeObject._InternalSetParentScheme(const parentscheme: TFRE_DB_Schemeobject);
var lCheckDbo:TFRE_DB_SchemeObject;
begin
 lCheckDbo := parentscheme;
 repeat
   if lCheckDbo=self then raise EFRE_DB_Exception.Create(edb_ERROR,'no circles with parentschemes ');
   lCheckDbo := lCheckDbo.Getparentscheme;
 until lCheckDbo=nil;
 FParentScheme.AsString:=parentscheme.DefinedSchemeName; // TODO Check if Parent Fields and My Fields don't override each other / Methods etc..
 FParentSchemeObj := parentscheme;
end;

procedure TFRE_DB_SchemeObject._SelfCheckValid;

  procedure Test(const fld:TFRE_DB_FIELD);
  var field_Def : TFRE_DB_FieldSchemeDefinition;
  begin
    if fld.IsUIDField then exit;
    field_def := fld.AsObject as TFRE_DB_FieldSchemeDefinition;
    if field_Def.FieldType=fdbft_CalcField then begin
 //     if not assigned(dbo_class.MethodAddress('IMI_'+field_Def.FCalcMethod.AsString)) then begin
      if not MethodExists(field_Def.FCalcMethod.AsString) then begin
        raise EFRE_DB_Exception.Create(edb_ERROR,'calculated field[%s] in scheme[%s] cannot find calculatemethod[%s]',[field_Def.FieldName,DefinedSchemeName,field_Def.FCalcMethod.AsString]);
      end;
    end;
  end;

begin
  FFieldDefs.ForAll(@test);
end;


procedure TFRE_DB_SchemeObject._BuildHardcodeMethods;
var dbo_class   : TFRE_DB_OBJECTCLASS;
    dbo_classex : TFRE_DB_OBJECTCLASSEX;
    schemename  : ShortString;
begin
  if FHasHardcodeClass.AsBoolean then begin
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

procedure TFRE_DB_SchemeObject._Seal;
begin
  FSealed := True;
end;

procedure TFRE_DB_SchemeObject._CheckChangingValid;
begin
  if FSealed and (GetSchemeType<>dbst_DB) then raise EFRE_DB_Exception.Create(edb_ACCESS,'changing of a sealed schemeclass(%s) is not allowed',[DefinedSchemeName]);
end;

procedure TFRE_DB_SchemeObject._FieldAccessCheck(const name: TFRE_DB_String;const schemelist:string);
var lFieldSchemeDefinition : TFRE_DB_FieldSchemeDefinition;
    exists                 : Boolean;
    lParentscheme          : TFRE_DB_SchemeObject;
begin
  if FStrict.AsBoolean then begin
    exists := FFieldDefs.FieldExists(name);
    if not exists then begin
      lParentscheme := GetParentScheme;
      if assigned(lParentscheme) then begin
        if schemelist='' then
          lParentscheme._FieldAccessCheck(name,DefinedSchemeName)
        else
          lParentscheme._FieldAccessCheck(name,'/'+DefinedSchemeName);
        exit;
      end;
    end;
    if not exists then
      begin
        raise EFRE_DB_Exception.Create(edb_FIELDMISMATCH,'Field <%s> is not defined in Scheme <%s>, and strict access is required.',[name,schemelist+'/'+DefinedSchemeName]);
      end;
  end;
end;

procedure TFRE_DB_SchemeObject.SetParentSchemeByName(const parentschemename: TFRE_DB_String);
var lscheme_object:TFRE_DB_SchemeObject;
begin
  if not GetSchemeFromDB(parentschemename,lscheme_object) then raise EFRE_DB_Exception.Create(edb_ERROR,'Trying to set the parentscheme, but cant get parentscheme by name '+parentschemename+' for '+DefinedSchemeName+' GUESS:maybe wrong initialization order ?');
  _InternalSetParentScheme(lscheme_object);
end;


function TFRE_DB_SchemeObject.GetParentScheme: TFRE_DB_SchemeObject;
var scheme_name:TFRE_DB_String;
begin
  scheme_name := GetParentSchemeName;
  if (FParentScheme.AsString<>'') and (FParentSchemeObj=nil) then begin
    if GetSchemeFromDB(scheme_name,FParentSchemeObj)=false then begin
      raise EFRE_DB_Exception.Create(edb_INTERNAL,'critical parentscheme inconsistency '+DefinedSchemeName+' '+FParentScheme.AsString);
    end;
  end;
  result := FParentSchemeObj;
end;

function TFRE_DB_SchemeObject.GetParentSchemeI: IFRE_DB_SchemeObject;
begin
  result := GetParentScheme;
end;

function TFRE_DB_SchemeObject.GetParentSchemeName: TFRE_DB_String;
begin
  if not assigned(FParentScheme) then abort;
  result := FParentScheme.AsString;
end;

procedure TFRE_DB_SchemeObject.RemoveParentScheme;
begin
  FParentScheme.Clear;
  FParentSchemeObj:=Nil;
end;

function TFRE_DB_SchemeObject.GetExplanation: TFRE_DB_String;
begin
  result:=FExplanation.AsString;
end;

procedure TFRE_DB_SchemeObject.SetExplanation(AValue: TFRE_DB_String);
begin
  FExplanation.AsString:=AValue;
end;

function TFRE_DB_SchemeObject.GetSchemeType: TFRE_DB_SchemeType;
begin
  result := TFRE_DB_SchemeType(FSchemeType.AsInt16);
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
      fetch_uid         : TGUID;
      fetch_up_object   : TFRE_DB_Object;
      raw_multi_vals    : boolean;
      raw_empty_array   : boolean;
      scheme_multi_vals : boolean;

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
                 fetch_uid := GFRE_BT.HexString_2_GUID(field_val[0]);
               except
                 raise EFRE_DB_Exception.Create(edb_ERROR,'invalid guid while updating a subfield object with a guid TFRE_DB_String [%s]',[field_val[0]]);
               end;
               if not DBConnection.Fetch(fetch_uid,fetch_up_object) then raise EFRE_DB_Exception.Create(edb_ERROR,'cannot fetch object guid [%s] while updating a subfield object with a guid TFRE_DB_String / not found',[field_val[0]]);
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
             sub_object := DBConnection.NewObject(sub_scheme_name);
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
               raise EFRE_DB_Exception.Create(edb_FIELDMISMATCH,'update of subfield object is only allowed if the specified subfield object has the same scheme. subfield=[%s] subfield scheme=[%s] update object scheme=[%s]',[field_name,sub_scheme_name,sub_object.SchemeClass]);
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
    if uppercase(field_name) = 'UID' then exit; // no uid updates
    if GetSchemeField(field_name,scheme_field_def) then begin
      raw_field_type     := field.FieldType;
      scheme_field_type  := scheme_field_def.FieldType;
      raw_multi_vals     := field.ValueCount>1;
      raw_empty_array    := field.ValueCount=0;
      scheme_multi_vals  := scheme_field_def.multiValues;
      if raw_multi_vals and not scheme_multi_vals then raise EFRE_DB_Exception.Create(edb_INTERNAL,'error updating field [%s],raw object has multivalues but the scheme does not allow this',[field.FieldName]);
      if (scheme_field_type<>fdbft_Object) or ((scheme_field_type=fdbft_Object) and (raw_field_type=fdbft_String)) then begin
        field_val   := field.AsStringArr;       //TODO -> EXTEND TO FIELD ARRAYS !!
      end;
      if (raw_field_type<>fdbft_String) and (scheme_field_type<>fdbft_Object) then begin
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'the raw object has a field which is not a raw TFRE_DB_String field [%s], but the fielddef [%s] from the scheme object [%s] is not a sub object!',[CFRE_DB_FIELDTYPE[raw_field_type],CFRE_DB_FIELDTYPE[scheme_field_type],DefinedSchemeName]);
      end;
      if raw_empty_array and (not scheme_multi_vals) then begin
        exit; //Don't set empty arrays to non arrays
      end;
      if (scheme_field_type<>fdbft_Object) and (length(field_val)=0) then begin
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
          fdbft_Stream      : raise EFRE_DB_Exception.Create(edb_INTERNAL,'implement raw -> stream');
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
      if lowercase(field_name)<>'uid' then begin
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

function TFRE_DB_SchemeObject.AddInputGroup(const id: TFRE_DB_String): TFRE_DB_InputGroupSchemeDefinition;
begin
 Result:=TFRE_DB_InputGroupSchemeDefinition.Create;
 Result.SetInputGroupID(id);
 FInputGroups.Field(id).AsObject:=Result;
end;

function TFRE_DB_SchemeObject.AddInputGroupI(const id: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;
begin
  result := AddInputGroup(id);
end;

function TFRE_DB_SchemeObject.GetInputGroup(const name: TFRE_DB_String): TFRE_DB_InputGroupSchemeDefinition;
var
  current : TFRE_DB_SchemeObject;
begin
  Result  := nil;
  current := self;
  repeat
    if current.FInputGroups.FieldExists(name) then begin
      Result:=current.FInputGroups._Field(name).AsObject as TFRE_DB_InputGroupSchemeDefinition;
      exit;
    end;
    current := current.GetParentScheme;
    if current=nil then break;
  until false;
end;

function TFRE_DB_SchemeObject.GetInputGroupI(const name: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;
begin
  result := GetInputGroup(name);
end;


function TFRE_DB_SchemeObject.GetSchemeFromDB(const schemename: TFRE_DB_String; var scheme: TFRE_DB_SchemeObject): Boolean;
begin
  result := false;
  assert(GetSchemeType<>dbst_INVALID);
  if gFRE_DB.GetSystemScheme(schemename,scheme) then exit(true);
  if GetSchemeType=dbst_DB then begin
    Result:=_DBConnectionBC(true).GetScheme(schemename,scheme);
  end;
end;

function TFRE_DB_SchemeObject.GetEnumFromDB(const enumname: TFRE_DB_String; var enum: TFRE_DB_Enum): Boolean;
begin
  enum := nil;
  if GFRE_DB.GetSysEnum(enumname,enum) then exit(true);
  Result:=_DBConnectionBC(true).GetEnum(enumname,enum);
end;

function TFRE_DB_SchemeObject.GetValidatorFromDB(const validatorname: TFRE_DB_String; var validator: TFRE_DB_ClientFieldValidator): Boolean;
begin
  if GFRE_DB.GetSysClientFieldValidator(validatorname,validator) then exit(true);
  Result:=_DBConnectionBC(true).GetClientFieldValidator(validatorname,validator);
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
    try
      FieldAccessCheck(field_to_check_fieldName); // strict check
    except on e:exception do begin
      if raise_errors then begin
        raise e;
      end else begin
        failure:=true;
        result:=true;
        exit;
      end;
    end;end;
    if not GetSchemeField(field_to_check_fieldName,field_definition) then exit(false); // continue
    if not field_definition.ValidateField(field_to_check,raise_errors) then begin
      failure:=true;
      result:=true; //break;
    end;
  end;

  function CheckSchemeField(const scheme_def_to_check:TFRE_DB_FIELD):boolean;
  var field_def : TFRE_DB_FieldSchemeDefinition;
      exists    : boolean;
      field_val : TFRE_DB_String;
  begin
    result:=false;
    if scheme_def_to_check.FieldName='UID' then exit;
    field_def := scheme_def_to_check.AsObject as TFRE_DB_FieldSchemeDefinition;
    exists := dbo.FieldExists(field_def.FieldName);
    if (not exists) and field_def.required then begin
      if raise_errors then raise EFRE_DB_Exception.Create(edb_ERROR,'the required field[%s], does not exist in object while validating against scheme[%s]',[field_def.FieldName,DefinedSchemeName]);
      failure:=true;
      exit(true); //break;
    end;
    if exists and field_def.required and (field_def.FieldType=fdbft_String) then begin
      field_val := dbo.Field(field_def.FieldName).AsString;
      if field_val='' then begin
        if raise_errors then raise EFRE_DB_Exception.Create(edb_ERROR,'the required field[%s] is a TFRE_DB_String, but is empty while validating against scheme[%s]',[field_def.FieldName,DefinedSchemeName]);
        failure:=true;
        exit(true); //break;
      end;
    end;
    if field_def.FieldType<>fdbft_Object then begin
    end
  end;

begin
  failure:=false;
  if dbo.SchemeClass<>DefinedSchemeName then raise EFRE_DB_Exception.Create(edb_ERROR,'when validating an object [%s] against a schemeclass [%s], it must be of that same schemeclass',[dbo.SchemeClass,DefinedSchemeName]);
  dbo.ForAllBrk(@CheckField); // check if object satisfied by scheme
  FFieldDefs.ForAllBrk(@CheckSchemeField);
  result := not failure;
end;

function TFRE_DB_SchemeObject.ValidateObjectI(const dbo: IFRE_DB_Object; const raise_errors: boolean): boolean;
begin
  result := ValidateObject(dbo.Implementor as TFRE_DB_Object,raise_errors);
end;

function TFRE_DB_SchemeObject.getSchemeFields: TFRE_DB_Object;
begin
  Result:=FFieldDefs;
end;

function TFRE_DB_SchemeObject.getSchemeFieldsI: IFRE_DB_Object;
begin
  result := getSchemeFields;
end;

function TFRE_DB_SchemeObject._ObjectsNeedsNoSubfieldSchemeCheck: boolean;
begin
  Result:=true;
end;

function TFRE_DB_SchemeObject._ObjectIsCodeclassOnlyAndHasNoScheme: boolean;
begin
  Result:=true;
end;

function TFRE_DB_SchemeObject.Field(const name: TFRE_DB_String): TFRE_DB_FIELD;
begin
  result := _Field(name);
//  raise EFRE_DB_Exception.Create(edb_ERROR,'Direct FIELD access is NOT ALLOWED in scheme objects');
end;

procedure TFRE_DB_SchemeObject.InternalSetup;
begin
  inherited InternalSetup;
  FHasHardcodeClass := _Field('HCC');
  FSchemeClass      := _Field('C');
  FFieldDefs        := _Field('F').AsObject;
  FUniqueKeys       := _Field('U').AsObject;
  FMethodDefs       := _Field('MD').AsObject;
  FInputGroups      := _Field('IG').AsObject;
  FStrict           := _Field('S');
  FParentScheme     := _Field('PS');
  FExplanation      := _Field('EX');
  FDisplayField     := _Field('DOB').AsObject;
  FSchemetype       := _Field('ST');
end;

procedure TFRE_DB_SchemeObject.FieldAccessCheck(const name: TFRE_DB_String);
begin
  _FieldAccessCheck(name,'');
end;

function TFRE_DB_SchemeObject.CalcFieldExists(const base_obj:TFRE_DB_Object;const name: TFRE_DB_String; var calculated_field: TFRE_DB_FIELD): boolean;
var lFieldSchemeDefinition : TFRE_DB_FieldSchemeDefinition;
    calc_func              : IFRE_DB_InvokeInstanceMethod;
begin
  result := GetSchemeField(name,lFieldSchemeDefinition);
  if result then begin
    result := lFieldSchemeDefinition.FieldType=fdbft_CalcField;
    if result then begin
      calc_func        := base_obj.Fetch_DBIMI_Method(lFieldSchemeDefinition.FCalcMethod.AsString);
      calculated_field := lFieldSchemeDefinition.CalcField(calc_func).Implementor as TFRE_DB_FIELD;
    end;
  end;
end;

procedure TFRE_DB_SchemeObject.AfterLoad;
var ccn:ShortString;
begin
  FSchemeClass := _Field('C');
  FFieldDefs   := _Field('F').AsObject;
  FUniqueKeys  := _Field('U').AsObject;
  FStrict      := _Field('S');

  if FHasHardcodeClass.AsBoolean then begin
    ccn := FSchemeClass.AsString;
    FHardCodeClassTyp := GFRE_DB.GetObjectClass(ccn);
    if not assigned(FHardCodeClassTyp) then begin
      FHardCodeClassTyp := GFRE_DB.GetObjectClassEx(ccn);
      if not assigned(FHardCodeClassTyp) then begin
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'cannot load scheme definition %s for a extended object of class',[ccn]);
      end;
    end;
  end;

  //FCodeClass   := _Field('CC');
  //FCodeClassname:= FCodeClass.AsString;
  //if FCodeClassname='' then begin
  //  FCodeClassname:='TFRE_DB_Object';
  //end;
//  FCodeClassVar:=GFRE_DB.GetObjectClass(FCodeClassname)
end;

procedure TFRE_DB_SchemeObject.ForAllCalculatedFields(const base_obj: TFRE_DB_Object; const iter: TFRE_DB_FieldIterator);

  procedure CalcIterator(const field:TFRE_DB_FIELD);
  var fielddef  : TFRE_DB_FieldSchemeDefinition;
      calc_func : IFRE_DB_InvokeInstanceMethod;
  begin
    if field.IsUIDField then exit;
    fielddef:=field.AsObject as TFRE_DB_FieldSchemeDefinition;
    if fielddef.FieldType=fdbft_CalcField then begin
      calc_func := base_obj.Fetch_DBIMI_Method(fielddef.FCalcMethod.AsString);
      iter(fielddef.CalcField(calc_func));
    end;
  end;

begin
  FFieldDefs.ForAll(@CalcIterator);
end;

procedure TFRE_DB_SchemeObject.ForAllCalculatedFieldsBrk(const base_obj: TFRE_DB_Object;const iter: TFRE_DB_FieldIteratorBrk);

 function CalcIterator(const field:TFRE_DB_FIELD):boolean;
 var fielddef  : TFRE_DB_FieldSchemeDefinition;
     calc_func : IFRE_DB_InvokeInstanceMethod;
 begin
   result:=false;
   if field.IsUIDField then exit;
   fielddef:=field.AsObject as TFRE_DB_FieldSchemeDefinition;
   if fielddef.FieldType=fdbft_CalcField then begin
     calc_func := base_obj.Fetch_DBIMI_Method(fielddef.FCalcMethod.AsString);
     result := iter(fielddef.CalcField(calc_func));
   end;
 end;
begin
  FFieldDefs.ForAllBrk(@CalcIterator);
end;


function TFRE_DB_SchemeObject.InvokeMethod_UID_Session(const instance: TFRE_DB_GUIDArray; const class_name,meth_name: TFRE_DB_String; const in_params: TFRE_DB_Object; const connection: TFRE_DB_CONNECTION; const session: TFRE_DB_UserSession): IFRE_DB_Object;
var i         : Integer;
    child_dbo : TFRE_DB_Object;
    obj       : TFRE_DB_Object;
    iobj      : IFRE_DB_Object;
    idc       : IFRE_DB_DERIVED_COLLECTION;
    found_o_cl: TFRE_DB_String;
    scheme    : TFRE_DB_SCHEMEOBJECT;
    obcl      : TFRE_DB_OBJECTCLASS;
    obexcl    : TFRE_DB_OBJECTCLASSEX;
    fake_in_p : TFRE_DB_Object;


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
  result := nil;
  if Length(instance)>0 then begin
    if assigned(session) and session.SearchSessionAppUID(instance[0],iobj) then begin
      obj := iobj.Implementor as TFRE_DB_Object;
    end else
    if assigned(session) and session.SearchSessionDCUID(instance[0],idc) then begin
      obj := idc.Implementor as TFRE_DB_DERIVED_COLLECTION;
    end else begin
      if not connection.Fetch(instance[0],obj) then raise EFRE_DB_Exception.Create(edb_ERROR,'Try to invoke [%s.%s], but cant fetch the instance [%s]',[DefinedSchemeName,meth_name,GFRE_BT.GUID_2_HexString(instance[0])]);
    end;
    for i := 1 to high(instance) do begin
      child_dbo:=nil;
      obj.ForAllFieldsBreak(@_getObj);
      if Assigned(child_dbo) then begin
        obj:=child_dbo;
      end else begin
        raise EFRE_DB_Exception.Create(edb_ERROR,'INSTANCE METHOD FAILED [%s.%s][%s] - CHILD OBJ NOT FOUND UID=[%s]!',[DefinedSchemeName,meth_name,GFRE_DB.GuidArray2SString(instance),GFRE_BT.GUID_2_HexString(instance[i])]);
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
        result := obj.Invoke(meth_name,in_params); // NEVER EVER FREE SOMETHING IN THE IN PARAMS
      finally
        in_params.Finalize;
      end;
    end else begin
      try
        fake_in_p := GFRE_DB.NewObject;
        fake_in_p.SetReference(session);
        result := obj.Invoke(meth_name,fake_in_p);
      finally
        fake_in_p.Finalize;
      end;
    end;
  end else begin
    obcl := GFRE_DB.GetObjectClass(class_name);
    if assigned(obcl) then begin
      in_params.SetReference(session.GetDBConnection.Implementor);
      result := obcl.Invoke_DBIMC_Method(meth_name,in_params);
    end else begin
       obexcl := GFRE_DB.GetObjectClassEx(class_name);
       if Assigned(obexcl) then begin
         in_params.SetReference(session.GetDBConnection.Implementor);
         result := obexcl.Invoke_DBIMC_Method(meth_name,in_params);
       end;
    end;
  end;

 //  res_obj := obj.Invoke(lMethod,in_params);
 //  TransformAndSend(res_obj,'INSTANCE METHOD INVOKED [%s %s %s]',[GFRE_BT.CombineString(lClassPath,'.'),lMethod,GFRE_BT.GUID_2_HexString(class_guid_path[0])],fdbtt_post2json);
 //end;


 // _DBConnection.Update(dbo);
end;

function TFRE_DB_SchemeObject.InvokeMethod_UIDI(const obj_uid: TGUID; const obj_methodname: TFRE_DB_String; const input: IFRE_DB_Object; const connection: IFRE_DB_CONNECTION): IFRE_DB_Object;
var ga:TFRE_DB_GUIDArray;
begin
  setlength(ga,1);ga[0] := obj_uid;
  result := InvokeMethod_UID_Session(ga,DefinedSchemeName,obj_methodname,input.Implementor as TFRE_DB_Object,Connection.Implementor_HC as TFRE_DB_CONNECTION,nil);
end;

function TFRE_DB_SchemeObject.ConstructNewInstance(const fail_on_no_cc: boolean): TFRE_DB_Object;
var ex_class : TFRE_DB_ObjectEx;
    dbo      : TFRE_DB_Object;
    name     : TFRE_DB_String;
begin
 if FHasHardcodeClass.AsBoolean then begin
   assert(assigned(FHardCodeClassTyp));
   name := FHardCodeClassTyp.ClassName;
   if FHardCodeClassTyp.InheritsFrom(TFRE_DB_OBJECTEX) then begin
     if IsA('TFRE_DB_NAMED_OBJECT') then begin
       dbo                    := GFRE_DB.NewNamedObject;
     end else begin
       dbo                    := GFRE_DB.NewObject;
     end;
     ex_class               := TFRE_DB_OBJECTCLASSEX(FHardCodeClassTyp).CreateBound(dbo);
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

 //if not GFRE_DB.ExistsObjectClass(ccn) then begin
 //  if fail_on_no_cc then begin
 //    raise EFRE_DB_Exception.Create(edb_ERROR,'NO CODECLASS DEFINITION FOR REQUESTED SCHEME <'+Scheme+'> FOUND !');
 //  end;
 //  ccn := 'TFRE_DB_Object';
 //end;
 //result := NewObjectSC(lScheme, GFRE_DB.GetObjectClass(ccn));

end;

procedure TFRE_DB_SchemeObject.SetupMediator(const dbo: TFRE_DB_Object);
var ex_class : TFRE_DB_ObjectEx;
    name     : TFRE_DB_String;
begin
   assert(assigned(FHardCodeClassTyp));
   name := FHardCodeClassTyp.ClassName;
   if FHardCodeClassTyp.InheritsFrom(TFRE_DB_OBJECTEX) then begin
     ex_class               := TFRE_DB_OBJECTCLASSEX(FHardCodeClassTyp).CreateBound(dbo);
     dbo.FMediatorExtention := ex_class;
   end else begin
     abort;
//     result := FHardCodeClassTyp.Create as TFRE_DB_Object;
   end;
end;

function TFRE_DB_SchemeObject.HasHardCodeClass: Boolean;
begin
  result := FHasHardcodeClass.AsBoolean;
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

  procedure field_iter(const field:TFRE_DB_FIELD);
  begin
    if field.FieldType=fdbft_Object then begin
      iterator(field.AsObject as TFRE_DB_FieldSchemeDefinition);
    end;
  end;

begin
  FFieldDefs.ForAllFields(@field_iter);
end;

//class function TFRE_DB_SchemeObject.InvokeMethod(const instance: TFRE_DB_Object; const meth_name: TFRE_DB_String; const in_params: TFRE_DB_Object): TFRE_DB_Object;
//begin
//  result := instance.Invoke_DBIMI_Method(meth_name,in_params);
//  //if assigned(instance) then begin
//  //end else begin
//  // //FIXME result := FCodeClassVar.Invoke_DBIMC_Method(meth_name,in_params);
//  //  GFRE_BT.CriticalAbort('FIXME');
//  //end;
//end;




function TFRE_DB_SchemeObject.GetAll_IMI_Methods: TFRE_DB_StringArray;
begin
  if not FHC_MethodsBuild then begin
    _BuildHardcodeMethods;
  end;
  result := FIMI_Methods;
end;

function TFRE_DB_SchemeObject.MethodExists(const name: TFRE_DB_String): boolean;
begin
  result:=StringInArray(uppercase(name),GetAll_IMI_Methods);
end;


{ TFRE_DB_COLLECTION }

procedure TFRE_DB_COLLECTION.__NotifyCollectionObservers(const notify_type: TFRE_DB_NotifyObserverType; const obj: TFRE_DB_Object; const obj_uid: TGUID);
  procedure Notify(const obs : IFRE_DB_COLLECTION_OBSERVER);
  begin
    obs.ICO_CollectionNotify(notify_type,obj,obj_uid);
  end;
begin
  FObservers.ForAll(@Notify);
end;

procedure TFRE_DB_COLLECTION._NotifyObserversOrRecord(const notify_type: TFRE_DB_NotifyObserverType; const obj: TFRE_DB_Object; const obj_uid: TGUID);
begin
  if FObservers.Count>0 then
    if not FObserverBlockUpdating then begin
      __NotifyCollectionObservers(fdbntf_START_UPDATING,nil,CFRE_DB_NullGUID);
      __NotifyCollectionObservers(notify_type,obj,obj_uid); // Store = Add l Notify all direct observers
      __NotifyCollectionObservers(fdbntf_ENDUPDATE_APPLY,nil,CFRE_DB_NullGUID);
    end else begin
      SetLength(FObserverUpdates,Length(FObserverUpdates)+1);
      FObserverUpdates[high(FObserverUpdates)].update_type := notify_type;
      FObserverUpdates[high(FObserverUpdates)].update_obj  := obj;
      FObserverUpdates[high(FObserverUpdates)].update_uid  := obj_uid;
    end;
end;


class function TFRE_DB_COLLECTION.Forced_In_Memory: Boolean;
begin
  result := false;
end;


constructor TFRE_DB_COLLECTION.Create(const connection: TFRE_DB_BASE_CONNECTION; const name: TFRE_DB_NameType; const pers_coll: IFRE_DB_PERSISTANCE_COLLECTION);
begin
  FManageInfo := connection;
  inherited Create;
  FObjectLinkStore := pers_coll;
  FName            := name;
  FObservers.Init;
end;


procedure TFRE_DB_COLLECTION.ForAllI(const func: IFRE_DB_Obj_Iterator);

  procedure _ForAll(const obj:TFRE_DB_Object);
  begin
    func(obj);
  end;

begin
  FObjectLinkStore.ForAllItems(@_ForAll);
end;

procedure TFRE_DB_COLLECTION.ForAllBreakI(const func: IFRE_DB_Obj_IteratorBreak);

  function _ForAllBreak(const obj:TFRE_DB_Object):boolean;
  begin
    func(obj);
  end;

begin
  FObjectLinkStore.ForAllitemsBreak(@_ForAllBreak);
end;

function TFRE_DB_COLLECTION.StoreI(var new_obj: IFRE_DB_Object): TFRE_DB_Errortype;
var lObject : TFRE_DB_Object;
begin
   lObject := new_obj.Implementor as TFRE_DB_Object;
   result  := Store(lObject);
end;


function TFRE_DB_COLLECTION.UpdateI(const dbo: IFRE_DB_Object): TFRE_DB_Errortype;
var ldbo : TFRE_DB_Object;
begin
  ldbo := dbo.Implementor as TFRE_DB_Object;
  result := Update(ldbo);
end;

function TFRE_DB_COLLECTION.FetchI(const ouid: TGUID; out dbo: IFRE_DB_Object): boolean;
var ldbo : TFRE_DB_Object;
begin
  result := Fetch(ouid,ldbo);
  dbo := ldbo;
end;

function TFRE_DB_COLLECTION.LinearScanI(const fieldname: TFRE_DB_NameType; const field_expr: TFRE_DB_FIELD_EXPRESSION): IFRE_DB_Object;
begin
  result := LinearScan(fieldname,field_expr);
end;

function TFRE_DB_COLLECTION.FirstI: IFRE_DB_Object;
begin
 result := First;
end;

function TFRE_DB_COLLECTION.LastI: IFRE_DB_Object;
begin
 result := Last;
end;

//function TFRE_DB_COLLECTION.IsTemporary: Boolean;
//begin
// result :=FIsTemporary;
//end;

procedure TFRE_DB_COLLECTION.Clear;
begin
  FObjectLinkStore.Clear;
end;

destructor TFRE_DB_COLLECTION.Destroy;
begin
  FDBO_State := fdbos_Destroying;
  inherited Destroy;
end;

function TFRE_DB_COLLECTION.Count: QWord;
begin
  result := FObjectLinkStore.Count;
end;

function TFRE_DB_COLLECTION.Exists(const ouid: TGUID): boolean;
begin
  result := FObjectLinkStore.Exists(ouid);
end;


procedure TFRE_DB_COLLECTION.ForAll(const func: TFRE_DB_Obj_Iterator);

  procedure Intercept(const obj:TFRE_DB_Object);
  begin
     obj.FManageInfo := self.FManageInfo;
     func(obj);
  end;

begin
  FObjectLinkStore.ForAllItems(@intercept);
end;

procedure TFRE_DB_COLLECTION.ForAllBreak(const func: TFRE_DB_Obj_IteratorBreak);

  function Intercept(const obj:TFRE_DB_Object):boolean;
  begin
    obj.FManageInfo := self.FManageInfo;
    result := func(obj);
  end;

begin
  FObjectLinkStore.ForAllitemsBreak(@intercept);
end;



function TFRE_DB_COLLECTION.Remove(const ouid: TGUID): boolean;
var ncolls : TFRE_DB_StringArray;
    res    : TFRE_DB_Errortype;
begin
  res := FObjectLinkStore.Delete(ouid,true,ncolls);
  result := (res=edb_OK);
  if Result then begin
    _DBConnectionBC._NotifyCollectionObservers(fdbntf_DELETE,nil,ouid,ncolls);
  end;
end;

function TFRE_DB_COLLECTION.Store(var new_obj: TFRE_DB_Object):TFRE_DB_Errortype;
var suid   : TGuid;
    ncolls : TFRE_DB_StringArray;
begin
  if assigned(FManageInfo) then
    _DBConnectionBC._CheckSchemeDefinitions(new_obj);
  if assigned(new_obj.FManageInfo) then
    new_obj.FManageInfo := nil;
  suid   := new_obj.UID;
  result := FObjectLinkStore.Store(new_obj,true,ncolls);
  if Result=edb_OK then
    begin
      Fetch(suid,new_obj);
      _NotifyObserversOrRecord(fdbntf_INSERT,nil,suid);
    end;
end;


function TFRE_DB_COLLECTION.Update(const dbo: TFRE_DB_Object): TFRE_DB_Errortype;
var dbo_uid : TGuid;
begin
  if assigned(dbo.FManageInfo) then
    dbo.FManageInfo:=nil;
  result := _DBConnectionBC.Update(dbo);
  //abort; //implement notify

  // From Clollection

  //if IsInMemory then begin //
  //  if FObjectLinkStore.Find(dbo.UID,maninf) then begin
  //    maninf.obj_link := dbo;
  //    _NotifyObserversOrRecord(fdbntf_UPDATE,dbo,dbo.UID);
  //    result := edb_OK;
  //  end else begin
  //    result := edb_NOT_FOUND;
  //  end;
  //end else begin
  //  dbo_uid := dbo.UID;
  //  result := _DBConnectionBC.Update(dbo);
  //  if Result=edb_OK then begin
  //     _DBConnectionBC.Fetch(dbo_uid,ndbo);
  //    _NotifyObserversOrRecord(fdbntf_UPDATE,ndbo,ndbo.UID);
  //  end;
  //end;

// From Connection
//var man_inf                : TFRE_DB_Object_ManageInfo;
//    res                    : boolean;
//    new_dbo                : TFRE_DB_Object;
//    new_references_to_list : TFRE_DB_GuidArray;
//    old_references_to_list : TFRE_DB_GuidArray;
//
//    _CheckSchemeDefinitions(dbo);
//    //FMasterCollection.FObjectLinkStore.Update();
//    new_references_to_list := _CheckRefIntegrityToLink(dbo);
//    res :=FMasterCollection.FObjectLinkStore.Find(dbo.UID,man_inf);
//    if res then begin
//      new_dbo := dbo.CloneToNewObject;
//      dbo.free;
//      old_references_to_list := man_inf.obj_link.ReferenceListFromDataNocount;
//      man_inf.obj_link.Free;
//      man_inf.obj_link:=new_dbo;
//      man_inf.obj_link.FManageInfo.ManageInfo := man_inf;
//      man_inf.SetObjManInfDirty;
//      _UpdateRefLinks(new_dbo.UID,old_references_to_list,new_references_to_list);
//      result := edb_OK;
//      _NotifyCollectionObservers(fdbntf_UPDATE,new_dbo,new_dbo.UID);
//      {$IFDEF DEBUG_TEST_DIRECT_SAVE}
//      man_inf.CacheIt; // DELAYED SAVE (CacheIt - Saves it);
//      {$ENDIF}
//    end else exit(edb_NOT_FOUND);

end;

function TFRE_DB_COLLECTION.Fetch(const ouid: TGUID; out dbo: TFRE_DB_Object): boolean;
begin
  result := FObjectLinkStore.Fetch(ouid,dbo);
  if result then
    dbo.FManageInfo := FManageInfo;
end;

procedure TFRE_DB_COLLECTION.ClearCollection;

  procedure RemoveIt(const obj:TFRE_DB_Object) ;
  begin
    Remove(obj.UID);
  end;

begin
  ForAll(@RemoveIt);
end;

function TFRE_DB_COLLECTION.CollectionName: TFRE_DB_NameType;
begin
  result := FName;
end;

function TFRE_DB_COLLECTION.LinearScan(const fieldname: TFRE_DB_NameType;  const field_expr: TFRE_DB_FIELD_EXPRESSION): TFRE_DB_Object;
begin
  abort;
  result := FObjectLinkStore.LinearScan(fieldname,field_expr);
end;

function TFRE_DB_COLLECTION.AddObserver(const obs: IFRE_DB_COLLECTION_OBSERVER): boolean;
begin
  result := FObservers.Add2ArrayChk(obs);
  writeln('ADD OBSERVER [',ClassName,'] in ',CollectionName,' for ',obs.ICO_ObserverID,' : ',FObservers.HighArray,' ',FObservers.Count);
end;

function TFRE_DB_COLLECTION.RemoveObserver(const obs: IFRE_DB_COLLECTION_OBSERVER): boolean;
begin
  result := FObservers.Remove(obs);
  writeln('REMOVE OBSERVER [',ClassName,'] in ',CollectionName,' for ',obs.ICO_ObserverID,' : ',FObservers.HighArray,' ',FObservers.Count);
end;

procedure TFRE_DB_COLLECTION.StartBlockUpdating;
begin
  if length(FObserverUpdates)<>0 then raise EFRE_DB_Exception.Create(edb_ERROR,'BLOCK UPDATING LOGIC ERROR UPLEN=[%d]',[Length(FObserverUpdates)]);
  FObserverBlockupdating := true;
end;

procedure TFRE_DB_COLLECTION.FinishBlockUpdating;
var i : integer;
begin
  __NotifyCollectionObservers(fdbntf_START_UPDATING,nil,CFRE_DB_NullGUID);
  try
    for i:=0 to High(FObserverUpdates) do begin
      __NotifyCollectionObservers(FObserverUpdates[i].update_type,FObserverUpdates[i].update_obj,FObserverUpdates[i].update_uid);
    end;
    __NotifyCollectionObservers(fdbntf_ENDUPDATE_APPLY,nil,CFRE_DB_NullGUID);
  finally
    SetLength(FObserverUpdates,0);
    FObserverBlockupdating := false;
  end;
end;

function TFRE_DB_COLLECTION.DefineIndexOnField(const FieldName: TFRE_DB_NameType; const FieldType: TFRE_DB_FIELDTYPE; const unique: boolean; const ignore_content_case: boolean; const index_name: TFRE_DB_NameType): TFRE_DB_Errortype;
begin
  result := FObjectLinkStore.DefineIndexOnField(FieldName,FieldType,unique,ignore_content_case, index_name);
end;

function TFRE_DB_COLLECTION.ExistsIndexed(const query_value: TFRE_DB_String; const index_name: TFRE_DB_NameType): Boolean;
var uidarr : TFRE_DB_GUIDArray;
begin
  result := FObjectLinkStore.GetIndexedUID(query_value,uidarr,index_name);
end;

function TFRE_DB_COLLECTION.GetIndexedObjI(const query_value: TFRE_DB_String; out obj: IFRE_DB_Object; const index_name: TFRE_DB_NameType): boolean;
var oobj:TFRE_DB_Object;
begin
  result := GetIndexedObj(query_value,oobj,index_name);
  if result then
    obj := oobj
  else
    obj := nil;
end;

function TFRE_DB_COLLECTION.GetIndexedObj(const query_value: TFRE_DB_String; out obj: TFRE_DB_Object; const index_name: TFRE_DB_NameType): boolean;
begin
  result := FObjectLinkStore.GetIndexedObj(query_value,obj,index_name);
  if result then
    obj.FManageInfo := FManageInfo;
end;

function TFRE_DB_COLLECTION.GetIndexedUID(const query_value: TFRE_DB_String; out obj_uid: TGUID; const index_name: TFRE_DB_NameType): boolean;
begin
  result := FObjectLinkStore.GetIndexedUID(query_value,obj_uid,index_name);
end;

function TFRE_DB_COLLECTION.RemoveIndexed(const query_value: TFRE_DB_String; const index_name: TFRE_DB_NameType): boolean;
begin
  abort;
end;

function TFRE_DB_COLLECTION.ItemCount: Int64;
begin
  result := FObjectLinkStore.Count;
end;

function TFRE_DB_COLLECTION.First: TFRE_DB_Object;
begin
  result := FObjectLinkStore.First;
  if assigned(result) then
    result.FManageInfo := FManageInfo;
end;

function TFRE_DB_COLLECTION.Last: TFRE_DB_Object;
begin
  result := FObjectLinkStore.Last;
  if assigned(result) then
    result.FManageInfo := FManageInfo;
end;

function TFRE_DB_COLLECTION.GetItem(const num: uint64): IFRE_DB_Object;
var obj : TFRE_DB_Object;
begin
  obj := FObjectLinkStore.GetItem(num);
  if assigned(obj) then
    begin
      obj.FManageInfo := FManageInfo;
      result := obj;
    end
  else
    begin
      result:=nil;
    end;
end;

procedure TFRE_DB_COLLECTION.ForceFullUpdateForObservers;
begin
  _NotifyObserversOrRecord(fdbntf_COLLECTION_RELOAD,nil,CFRE_DB_NullGUID);
end;

{ TFRE_DB_CONNECTION }

//procedure DoAllObjects_Iterate(const key:TGuid;const man_info:TFRE_DB_Object_ManageInfo;const data:Pointer);
//begin
//  man_info.InstanceIt;
//  TFRE_DB_Obj_Iterator(data^)(man_info.obj_link);
//end;

//procedure TFRE_DB_BASE_CONNECTION.ForAllObjects(const iterator: TFRE_DB_Obj_Iterator);
//begin
//  _ConnectCheck;
//  FPersistance_Layer.ForAllObjects(iterator);
//end;


procedure TFRE_DB_BASE_CONNECTION.ForAllColls(const iterator: TFRE_DB_Coll_Iterator);
  procedure DoAllCollections_Iterate(const collection :TFRE_DB_COLLECTION);//const key:TFRE_DB_String;const manage:TFRE_DB_Collection_ManageInfo;const data:Pointer);
  begin
    //manage.InstanceIt;
    //if collection_link=nil then begin
    //  result := FConnection.FPersistance_Layer.RetrieveCollection(fkey,collection_link,self);
    //  collection_link.FManageInfo.CollManageInfo := self; // upgrade connection to managementinfo;
    //  assert(collection_link.FDBO_State=fdbos_Clean);
    //  //assert(collection_link.FStreamingSize>0);
    //  if result<>edb_OK then raise EFRE_DB_Exception.Create(result,'instancing error / persistance layer');
    //end;
    //TFRE_DB_Coll_Iterator(data^)(manage.collection_link);
    iterator(collection);
  end;

begin
  _ConnectCheck;
  FCollectionStore.ForAllItems(@DoAllCollections_Iterate);//,@iterator);
end;

procedure TFRE_DB_BASE_CONNECTION.ForAllSchemes(const iterator: TFRE_DB_Scheme_Iterator);
  procedure Iterate(const obj:TFRE_DB_Object);
  begin
    iterator(obj as TFRE_DB_SchemeObject);
  end;
  procedure sysiterator(const obj:TFRE_DB_SchemeObject);
  begin
    iterator(obj);
  end;
begin
  _ConnectCheck;
  GFRE_DB.ForAllSchemes(@sysiterator);
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

begin
  _ConnectCheck;
//  FSysEnum.ForAll(@Iterate);
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
begin
  _ConnectCheck;
//  FSysClientValidators.ForAll(@Iterate);
  GFRE_DB.ForAllClientFieldValidators(@IterateSys);
end;

//procedure TFRE_DB_BASE_CONNECTION.ForAllObjectsI(const iterator: IFRE_DB_Obj_Iterator);
//
//  procedure Iterate(const obj:TFRE_DB_Object);
//  begin
//    iterator(obj);
//  end;
//
//begin
// _ConnectCheck;
// FPersistance_Layer.ForAllObjects(@Iterate);
//end;

procedure TFRE_DB_BASE_CONNECTION.ForAllCollsI(const iterator: IFRE_DB_Coll_Iterator);

  procedure Iterate(const collection : TFRE_DB_COLLECTION);
  begin
     iterator(collection);
  end;

begin
  _ConnectCheck;
  FCollectionStore.ForAllItems(@Iterate);
end;

procedure TFRE_DB_BASE_CONNECTION.ForAllSchemesI(const iterator: IFRE_DB_Scheme_Iterator);
  procedure Iterate(const obj:TFRE_DB_SchemeObject);
  begin
    iterator(obj);
  end;
begin
  _ConnectCheck;
  ForAllSchemes(@Iterate);
end;

procedure TFRE_DB_BASE_CONNECTION.ForAllEnumsI(const iterator: IFRE_DB_Enum_Iterator);
  procedure Iterate(const obj:TFRE_DB_Enum);
  begin
    iterator(obj);
  end;
begin
  _ConnectCheck;
  ForAllEnums(@Iterate);
end;

procedure TFRE_DB_BASE_CONNECTION.ForAllClientFieldValidatorsI(const iterator: IFRE_DB_ClientFieldValidator_Iterator);
  procedure Iterate(const obj:TFRE_DB_ClientFieldValidator);
  begin
    iterator(obj);
  end;
begin
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
     DumpSyso(obj);
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

function TFRE_DB_BASE_CONNECTION._Connect(const db: TFRE_DB_String;const is_system:boolean): TFRE_DB_Errortype;
begin
  _CloneCheck;
  if FConnected then exit(edb_ALREADY_CONNECTED);
  result :=   GFRE_DB_DEFAULT_PS_LAYER.Connect(db,FPersistance_Layer);
  if result=edb_OK then begin
    FConnected         := true;
    InternalSetupConnection(is_system,false);
    FDBName            := db;
  end else begin
    FConnected         :=false;
    FDBName            :='';
  end;
end;

function TFRE_DB_BASE_CONNECTION.ConnectedName: TFRE_DB_String;
begin
  _ConnectCheck;
  result := FDBName;
end;

procedure TFRE_DB_BASE_CONNECTION.InternalSetupConnection(const is_system,is_db_restore: boolean);

  //procedure SetupSchemeCollection;
  //begin
  //  //FSchemeCollection :=  GFRE_DB.GetSystemSchemeColl;
  //end;

  procedure SetupApps;
  begin
    Fapps := GFRE_DB.GetApps;
  end;

  procedure SetupWorkFlowSchemeCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysWorkflowSchemes') then begin
      //writeln('Fresh instancing SysWorkflowSchemes Collection');
      coll := Collection('SysWorkflowSchemes');
      coll.DefineIndexOnField('objname',fdbft_String,True,True);
    end;
    FSysWorkflowSchemes := Collection('SysWorkflowSchemes');
  end;

  procedure SetupWorkFlowInstanceCollection;
  var coll : TFRE_DB_COLLECTION;
  begin
    if not CollectionExists('SysWorkflowInstances') then begin
      //writeln('Fresh instancing SysWorkflowInstances Collection');
      coll := Collection('SysWorkflowInstances');
    end;
    FSysWorkflowInstances := Collection('SysWorkflowInstances');
  end;

begin
  _CloneCheck;
  if not is_system then begin
    //SetupSchemeCollection;
    SetupApps;
    SetupWorkFlowSchemeCollection;
    SetupWorkFlowInstanceCollection;
    //SetupEnumCollection;
    //SetupValidatorCollection;
  end;
end;

constructor TFRE_DB_BASE_CONNECTION.Create;
var  keyp          : PFRE_DB_NameType;
begin
 FCollectionStore  := _TFRE_DB_CollectionTree.create(@FREDB_DBString_Compare);
 FConnected        := false;
end;


function TFRE_DB_BASE_CONNECTION._IntegrityCheck(const log_function: TFRE_LogMsgN): TFRE_DB_Errortype;
var cnt:integer;

  //procedure ILogW(msg:TFRE_DB_String);
  //begin
  //  if assigned(log_function) then begin
  //    log_function(msg);
  //  end;
  //end;
  //
  //procedure ILog(msg:TFRE_DB_String);
  //begin
  //  ILogW(msg+LineEnding);
  //end;
  //
  //procedure CheckAllObjects(const obj:TFRE_DB_Object);
  //  procedure Field(const fld:TFRE_DB_FIELD);
  //    procedure CheckIt(const linked_guid:TGUID);
  //    begin
  //      if not _RefLinkcheck(obj.UID,linked_guid) then begin
  //        if Exists(linked_guid) then begin
  //          if FIntegrityRebuild then begin
  //            ILog(format('referential integrity check failed  | link from [%s.%s](%s) to object (%s) : not found in referential management, but object exists (->rebuilding management)',[obj.SchemeClass,fld.FieldName,obj.UID_String,GFRE_BT.GUID_2_HexString(linked_guid)]));
  //            _AddRefLink(obj.UID,linked_guid,true);
  //          end else begin
  //            ILog(format('referential integrity check failed  | link from [%s.%s](%s) to object (%s) : not found in referential management',[obj.SchemeClass,fld.FieldName,obj.UID_String,GFRE_BT.GUID_2_HexString(linked_guid)]));
  //          end;
  //        end else begin
  //          ILog(format('referential integrity check failed for link from [%s.%s](%s) to object (%s) ? linked object not found',[obj.SchemeClass,fld.FieldName,obj.UID_String,GFRE_BT.GUID_2_HexString(linked_guid)]));
  //        end;
  //      end;
  //      if FIntegrityInfoLog then begin
  //        ILogW(GFRE_BT.GUID_2_HexString(linked_guid));
  //      end;
  //    end;
  //  begin
  //    if FIntegrityInfoLog then ILogW('  '+fld.FieldName+'('+CFRE_DB_FIELDTYPE[fld.FieldType]+')');
  //    if fld.FieldType=fdbft_ObjLink then begin
  //      if FIntegrityInfoLog then ILog('');
  //      if FIntegrityInfoLog then ILogW('  LINKED ( ');
  //      ForAllGuidsDo(fld.AsObjectLinkArray,@CheckIt);
  //      if FIntegrityInfoLog then ILog(' ) ');
  //    end;
  //  end;
  //begin
  //  if FIntegrityInfoLog then ILog('> '+inttostr(cnt)+' ('+obj.SchemeClass+')');
  //  inc(cnt);
  //  obj.ForAllFields(@field);
  //  if FIntegrityInfoLog then ILOG('');
  //  if FIntegrityInfoLog then ILOG('<');
  //end;
  //
  //procedure _RefObjectCheck;
  //var from_fieldo,to_fieldo:TFRE_DB_Object;
  //    lname:TFRE_DB_String;
  //
  //  procedure IterateLinks(const link_manage_field:TFRE_DB_FIELD);
  //  var field_guid : TGuid;
  //      fname      : TGUID_String;
  //      link_array : TFRE_DB_GUIDArray;
  //      obj        : TFRE_DB_Object;
  //      i          : integer;
  //      current    : integer;
  //      max        : integer;
  //      null_guids : integer;
  //  begin
  //    fname:=link_manage_field.FieldName;
  //    if fname='UID' then exit;
  //    field_guid := GFRE_BT.HexString_2_GUID(fname);
  //    if not Exists(field_guid) then begin
  //      ILog('* stale link in list ('+lname+') -> ('+link_manage_field.FieldName+') - object does not exist');
  //    end else begin
  //      link_array := link_manage_field.AsGUIDArr;
  //      null_guids := 0;
  //      for i:=0 to high(link_array) do begin
  //        if not Exists(link_array[i]) then begin
  //          if not FREDB_Guids_same(link_array[i],CFRE_DB_NullGUID) then begin
  //            if lname='from' then begin
  //              ILog('* stale link in list ('+lname+') -> OBJ('+link_manage_field.FieldName+') is pointing to uid ('+GFRE_BT.GUID_2_HexString(link_array[i])+') - which does not exist'+inttostr(i)+'/'+inttostr(current)+'/'+inttostr(max));
  //            end else begin
  //              ILog('* stale link in list ('+lname+') -> OBJ('+link_manage_field.FieldName+') is pointed to by object with uid ('+GFRE_BT.GUID_2_HexString(link_array[i])+') - which does not exist '+inttostr(i)+'/'+inttostr(current)+'/'+inttostr(max));
  //            end;
  //          end else begin
  //            inc(null_guids);
  //          end;
  //        end;
  //      end;
  //      if (null_guids)>(2*c_REFLINK_BLOCK_SIZE) then ILog('Much Nullguids ('+lname+') '+fname+': '+inttostr(null_guids));
  //    end;
  //  end;
  //
  //begin
  // from_fieldo := FReferentialLinks._Field('F').AsObject;
  // to_fieldo   := FReferentialLinks._Field('T').AsObject;
  // lname       := 'from';
  // from_fieldo.ForAll(@IterateLinks);
  // lname       := 'to';
  // to_fieldo.ForAll(@IterateLinks);
  //end;

begin
  //cnt:=0;
  //abort;
  //ILog('--Integrity Check - DB <'+FDBName+'>');
  //ILog('Instancing Objectstore -  Count: '+inttostr(FObjectStore.Count));
  //ILog('**Phase 1 - Object to Reflink Checks');
  //ForAllObjects(@CheckAllObjects);
  //ILog('**Phase 1 - done');
  //ILog('**Phase 2 - Reflinks to objects check');
  //_RefobjectCheck;
  //ILog('**Phase 2 - done');
  //ILog('--DONE--');
end;



function TFRE_DB_BASE_CONNECTION._RefLinkcheck(const from_obj, to_obj: TGuid): boolean;
//var from_field        : TFRE_DB_FIELD;
//    to_field          : TFRE_DB_FIELD;
//    from_field_name,
//    to_field_name     : TGUID_String;
//
//    function SearchGuidInArray(const x:TGuid;const guid_search_array : TFRE_DB_GUIDArray):boolean;
//    var i                 : integer;
//    begin
//      result:=false;
//      for i := 0 to high(guid_search_array) do begin
//        if FREDB_Guids_Same(x,guid_search_array[i])then exit(true);
//      end;
//    end;
//
begin
  //from_field_name := GFRE_BT.GUID_2_HexString(from_obj);
  //to_field_name   := GFRE_BT.GUID_2_HexString(to_obj);
  //from_field := FReferentialLinks._Field('F').AsObject._FieldOnlyExisting(from_field_name);
  //to_field   := FReferentialLinks._Field('T').AsObject._FieldOnlyExisting(to_field_name);
  //if not Assigned(from_field) then exit(false);
  //if not Assigned(to_field)   then exit(false);
  //if not SearchGuidInArray(to_obj,from_field.AsGUIDArr) then exit(false);
  //if not SearchGuidInArray(from_obj,to_field.AsGUIDArr) then exit(false);
  //exit(true);
end;




function TFRE_DB_BASE_CONNECTION.GetReferences(const obj_uid: TGuid; const from: boolean): TFRE_DB_ObjectReferences;
begin
  result := FPersistance_Layer.GetReferences(obj_uid,from);
end;


function TFRE_DB_BASE_CONNECTION.ReferenceCount(const obj_uid: TGuid; const from: boolean): NativeInt;
begin
  result := FPersistance_Layer.GetReferenceCount(obj_uid,from);
end;

function TFRE_DB_BASE_CONNECTION.Implementor: TObject;
begin
  result := self;
end;

procedure TFRE_DB_BASE_CONNECTION._NotifyCollectionObservers(const notify_type: TFRE_DB_NotifyObserverType; const obj: TFRE_DB_Object; const obj_uid: TGUID; const ncolls: TFRE_DB_StringArray);

  procedure CollIterator(const coll:TFRE_DB_COLLECTION);
  var obs_typ:string;
  begin
    try
      if StringInArray(uppercase(coll.CollectionName),ncolls) then
        coll._NotifyObserversOrRecord(notify_type,obj,obj_uid);
    except on e:Exception do begin
      WriteStr(obs_typ,notify_type);
      GFRE_DB.LogError(dblc_DB,'COULD NOT FIRE [%s] OBSERVER COLL [%s] UID[%s] ERR[%s] ',[obs_typ,coll.ClassName,GFRE_BT.GUID_2_HexString(obj_UID),e.Message]);
    end;end;
end;

begin
  if (FConnected)  and (length(ncolls)>0) then begin
    ForAllColls(@CollIterator);
  end;
end;


destructor TFRE_DB_BASE_CONNECTION.Destroy;
begin
  if FCloned then exit;
  FCollectionStore.Free;
  FCollectionStore:=nil;
end;


function TFRE_DB_CONNECTION.GetDatabaseName: TFRE_DB_String;
begin
  result := FDBName;
end;

constructor TFRE_DB_CONNECTION.CreateCloned(const from: TFRE_DB_CONNECTION);
var coll : TFRE_DB_COLLECTION;
    ob   : TFRE_DB_Object;
begin
  inherited      CreateCloned(from);
  FSysConnection := TFRE_DB_SYSTEM_CONNECTION.CreateCloned(from.FSysConnection);
end;

function TFRE_DB_CONNECTION.ImpersonateClone(const user, pass: TFRE_DB_String;out conn:TFRE_DB_CONNECTION): TFRE_DB_Errortype;
begin
  result := CheckLogin(user,pass);
  if result<>edb_OK then exit;
  conn := TFRE_DB_CONNECTION.CreateCloned(self);
  conn.FSysConnection.ImpersonateTheClone(user,pass);
end;

function TFRE_DB_CONNECTION.Connect(const db, user, pass: TFRE_DB_String; const ProxySysConnection: TFRE_DB_SYSTEM_CONNECTION): TFRE_DB_Errortype;
begin
  if FConnected then exit(edb_ALREADY_CONNECTED);
  if uppercase(db)='SYSTEM' then exit(edb_ACCESS);
  FSysConnection:=ProxySysConnection;
  if not assigned(FSysConnection) then begin
    FSysConnection := GFRE_DB.NewDirectSysConnection;
    result := FSysConnection.Connect(user,pass);
    if result<>edb_OK then exit(edb_DB_NO_SYSTEM);
  end;
  result := _Connect(db,false);
end;

function TFRE_DB_CONNECTION.Connect(const db: TFRE_DB_String; const user: TFRE_DB_String; const password: TFRE_DB_String): TFRE_DB_Errortype;
begin
  result := Connect(db,user,password,nil);
end;

function TFRE_DB_CONNECTION.CheckLogin(const user, pass: TFRE_DB_String): TFRE_DB_Errortype;
begin
  if not FConnected then exit(edb_NOT_CONNECTED);
  result := FSysConnection._CheckLogin(user,pass);
end;


function TFRE_DB_CONNECTION.CollectionExists(const name: TFRE_DB_NameType): boolean;
begin
    Result := inherited CollectionExists(name);
end;

function TFRE_DB_CONNECTION.DeleteCollection(const name: TFRE_DB_NameType): TFRE_DB_Errortype;
begin
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

function TFRE_DB_BASE_CONNECTION.CollectionExists(const name: TFRE_DB_NameType): boolean;
begin
  result := FPersistance_Layer.ExistCollection(name);
end;

function TFRE_DB_CONNECTION.IntegrityCheck(const log_function: TFRE_LogMsgN): TFRE_DB_Errortype;
begin
  if not FConnected then exit(edb_NOT_CONNECTED);
  _IntegrityCheck(log_function);
end;

//function TFRE_DB_CONNECTION.SchemeCollection: TFRE_DB_SCHEME_COLLECTION;
//begin
//  Result:=inherited SchemeCollection;
//end;

function TFRE_DB_BASE_CONNECTION.NewObjectCC(const ObjClass: TFRE_DB_OBJECTCLASS): TFRE_DB_Object;
begin
  result := GFRE_DB.NewObject(ObjClass);
end;


function TFRE_DB_BASE_CONNECTION._NewObject(const Scheme: TFRE_DB_String;const fail_on_no_cc:boolean): TFRE_DB_Object;
var lScheme  : TFRE_DB_SchemeObject;
    objclass : TFRE_DB_OBJECTCLASS;
    ccn      : shortstring;
begin
  result := nil;
  if Scheme<>'' then begin
    if GetScheme(Scheme,lScheme) then begin
      result := lScheme.ConstructNewInstance(fail_on_no_cc);
    end else begin
      raise EFRE_DB_Exception.Create(edb_ERROR,'NO SCHEME DEFINITION FOR REQUESTED SCHEME <'+Scheme+'> FOUND !');
    end;
  end else begin
    result := NewObjectCC(TFRE_DB_Object);
  end;
  AssociateObject(result);
end;

procedure TFRE_DB_BASE_CONNECTION.AssociateObject(const Obj: TFRE_DB_Object);
begin
  if not assigned(obj.FManageInfo) then begin
    obj.FManageInfo := self;
  end else begin
    raise EFRE_DB_Exception.Create(edb_ERROR,'cannot associate a DBObject which has already a db connection association');
  end;
end;

procedure TFRE_DB_BASE_CONNECTION.AssociateObjectI(const Obj: IFRE_DB_Object);
begin
  AssociateObject(obj.Implementor as TFRE_DB_Object);
end;

function TFRE_DB_BASE_CONNECTION.NewObject(const Scheme: TFRE_DB_String): TFRE_DB_Object;
begin
  result := _NewObject(Scheme,False);
end;

function TFRE_DB_BASE_CONNECTION.NewObjectI(const Scheme: TFRE_DB_String): IFRE_DB_Object;
begin
  result := NewObject(Scheme);
end;

function TFRE_DB_BASE_CONNECTION.NewObjectNS(const Scheme: TFRE_DB_String): TFRE_DB_Object;
begin
 result := _NewObject(Scheme,true);
end;

//function TFRE_DB_BASE_CONNECTION.StoreScheme(var obj: TFRE_DB_SchemeObject): TFRE_DB_Errortype;
//begin
//  //TODO : DEPRECATE   if GFRE_DB.SystemSchemeExists(obj) then exit(edb_RESERVED);
//  //if GFRE_DB.SystemSchemeExists(obj) then begin
//  //  writeln('WARNING:::DEPRECATED - SCHEME OVERRIDE ',obj.ClassName,' ',obj.DefinedSchemeName);
//  //end;
//  if GFRE_DB.SystemSchemeExists(obj) then begin
//    abort;
//  end;
//  result:=FSchemeCollection.StoreScheme(obj);
//end;
//
//function TFRE_DB_BASE_CONNECTION.StoreSchemeI(var  obj: IFRE_DB_SchemeObject): TFRE_DB_Errortype;
//var temp : TFRE_DB_SchemeObject;
//begin
//  temp := obj.Implementor as TFRE_DB_SchemeObject;
//  result := StoreScheme(temp);
//end;

function TFRE_DB_BASE_CONNECTION.GetSchemeI(const scheme_name: TFRE_DB_NameType; var scheme: IFRE_DB_SchemeObject): boolean;
var temp:TFRE_DB_SchemeObject;
begin
  scheme := nil;
  result := GetScheme(scheme_name,temp);
  if result then begin
    scheme := temp;
  end;
end;

function TFRE_DB_BASE_CONNECTION.GetScheme(const scheme_name: TFRE_DB_NameType; var scheme: TFRE_DB_SchemeObject): boolean;
begin
  result := GFRE_DB.GetSystemScheme(scheme_name,scheme);
  //if not result and assigned(FSchemeCollection) then begin // FSchemecollection is not assigned in DB restore case
  //  result := FSchemeCollection.GetScheme(scheme_name,scheme);
  //end;
end;

//function TFRE_DB_BASE_CONNECTION.GetSchemeCollection: TFRE_DB_SCHEME_COLLECTION;
//begin
//  Result:=FSchemeCollection;
//end;

//function TFRE_DB_BASE_CONNECTION.GetSchemeCollectionI: IFRE_DB_SCHEME_COLLECTION;
//begin
//  result := GetSchemeCollection;
//end;


function TFRE_DB_BASE_CONNECTION.GetEnum(const enum_name: TFRE_DB_String; var enum: TFRE_DB_Enum): boolean;
begin
  _ConnectCheck;
  if GFRE_DB.GetSysEnum(enum_name,enum) then exit(true);
//  result := FSysEnum.GetIndexedObj(enum_name,TFRE_DB_Object(enum));
end;

function TFRE_DB_BASE_CONNECTION.GetEnumI(const enum_name: TFRE_DB_String; out enum: IFRE_DB_Enum): boolean;
var l_enum : TFRE_DB_Enum;
begin
  result := GetEnum(enum_name,l_enum);
  if result then begin
    enum := l_enum;
  end else begin
    enum := nil;
  end;
end;

//function TFRE_DB_BASE_CONNECTION.StoreEnum(var enum: TFRE_DB_Enum): TFRE_DB_Errortype;
//begin
//  _ConnectCheck;
//  result := FSysEnum.Store(TFRE_DB_Object(enum));
//  enum := nil;
//end;
//
//function TFRE_DB_BASE_CONNECTION.StoreEnumI(var enum: IFRE_DB_Enum): TFRE_DB_Errortype;
//var renum : TFRE_DB_Enum;
//begin
//  renum  := enum.Implementor as TFRE_DB_Enum;
//  result := StoreEnum(renum);
//  enum   := nil;
//end;

function TFRE_DB_BASE_CONNECTION.GetClientFieldValidator(const val_name: TFRE_DB_String; var validator: TFRE_DB_ClientFieldValidator): boolean;
begin
  _ConnectCheck;
  if GFRE_DB.GetSysClientFieldValidator(val_name,validator) then exit(true);
  //result := FSysClientValidators.GetIndexedObj(val_name,TFRE_DB_Object(validator));
end;

function TFRE_DB_BASE_CONNECTION.GetClientFieldValidatorI(const val_name: TFRE_DB_String; out validator: IFRE_DB_ClientFieldValidator): boolean;
var val:TFRE_DB_ClientFieldValidator;
begin
  result := GetClientFieldValidator(val_name,val);
  if result then begin
    validator := val;
  end else begin
    validator := nil;
  end;
end;



//function TFRE_DB_BASE_CONNECTION.StoreClientFieldValidator(var validator: TFRE_DB_ClientFieldValidator): TFRE_DB_Errortype;
//begin
//  _ConnectCheck;
//  result := FSysClientValidators.Store(TFRE_DB_Object(validator));
//  validator := nil;
//end;
//
//function TFRE_DB_BASE_CONNECTION.StoreClientFieldValidatorI(var validator: IFRE_DB_ClientFieldValidator): TFRE_DB_Errortype;
//var cfv : TFRE_DB_ClientFieldValidator;
//begin
//  cfv := validator.Implementor as TFRE_DB_ClientFieldValidator;
//  result := StoreClientFieldValidator(cfv);
//  validator := nil;
//end;





//function TFRE_DB_BASE_CONNECTION.RemoveSchemeByName(const scheme_name: TFRE_DB_String): boolean;
//begin
//  _ConnectCheck;
//  result := FSchemeCollection.RemoveIndexed(scheme_name);
//end;


//function TFRE_DB_BASE_CONNECTION.SchemeExists(const scheme_name: TFRE_DB_String): boolean;
//begin
//  result:=FSchemeCollection.SchemeExists(scheme_name);
//end;

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

function TFRE_DB_BASE_CONNECTION.InstallAppDefaults(const Appname: TFRE_DB_String): TFRE_DB_Errortype;
var app : TFRE_DB_APPLICATION;
   conn : IFRE_DB_SYS_CONNECTION;
begin
  if not GFRE_DB.GetApp(Appname,app) then exit(edb_NOT_FOUND);
  if not self.GetInterface(IFRE_DB_SYS_CONNECTION,CONN) then GFRE_BT.CriticalAbort('internal/logic interface failure : must have a sysconnection interface');
  result := App.InstallAppDefaults(conn);
end;

function TFRE_DB_BASE_CONNECTION.RemoveApp(const Appname: TFRE_DB_String): TFRE_DB_Errortype;
var app : TFRE_DB_APPLICATION;
    conn : IFRE_DB_SYS_CONNECTION;
begin
  if not GFRE_DB.GetApp(Appname,app) then exit(edb_NOT_FOUND);
  if not self.GetInterface(IFRE_DB_SYS_CONNECTION,CONN) then GFRE_BT.CriticalAbort('internal/logic interface failure : must have a sysconnection interface');
  result := App.RemoveApp(conn);
end;

function TFRE_DB_BASE_CONNECTION.RemoveAppGroup(const Appname: TFRE_DB_String; const sub_group_name: TFRE_DB_String): TFRE_DB_Errortype;
var app : TFRE_DB_APPLICATION;
   conn : IFRE_DB_SYS_CONNECTION;
   name : TFRE_DB_String;
begin
  if not GFRE_DB.GetApp(Appname,app) then exit(edb_NOT_FOUND);
  if not self.GetInterface(IFRE_DB_SYS_CONNECTION,CONN) then GFRE_BT.CriticalAbort('internal/logic interface failure : must have a sysconnection interface');
  name := Get_Groupname_App_Group_Subgroup(app.ObjectName,sub_group_name);
  if conn.GroupExists(name) then begin
    result := conn.DeleteGroup(name);
  end else begin
    result := edb_NOT_FOUND;
  end;
end;

function TFRE_DB_BASE_CONNECTION.AddAppGroup(const Appname: TFRE_DB_String; const groupatdomain: TFRE_DB_String; const short_desc, long_desc: TFRE_DB_String): TFRE_DB_Errortype;
var app : TFRE_DB_APPLICATION;
   conn : IFRE_DB_SYS_CONNECTION;
   name : TFRE_DB_String;
   ug   : IFRE_DB_GROUP;
   domain    : TFRE_DB_String;
   groupname : TFRE_DB_String;
begin
  if not GFRE_DB.GetApp(Appname,app) then exit(edb_NOT_FOUND);
  if not self.GetInterface(IFRE_DB_SYS_CONNECTION,CONN) then GFRE_BT.CriticalAbort('internal/logic interface failure : must have a sysconnection interface');
  name := Get_Groupname_App_Group_Subgroup(app.ObjectName,groupatdomain);
  FREDB_SplitLocalatDomain(name,groupname,domain);
  if conn.GroupExists(name) then begin
    result := edb_EXISTS;
  end else begin
    ug := GFRE_DB.NewGroup(groupname,long_desc,short_desc);
    Result:=conn.StoreGroup(appname,domain,ug);
  end;
end;

function TFRE_DB_BASE_CONNECTION.FetchI(const ouid: TGUID; out dbo: IFRE_DB_Object): boolean;
var ldbo : TFRE_DB_Object;
begin
  result := Fetch(ouid,ldbo);
  if result then begin
    dbo := ldbo;
  end else begin
    dbo := nil;
  end;
end;

function TFRE_DB_BASE_CONNECTION.FetchAsIntf(const ouid: TGUID; const IntfSpec: ShortString; out Intf): boolean;
var ldbo:TFRE_DB_Object;
begin
  result := Fetch(ouid,ldbo);
  if result then begin
    ldbo.IntfCast(IntfSpec,intf);
  end;
end;


constructor TFRE_DB_BASE_CONNECTION.CreateCloned(const from: TFRE_DB_BASE_CONNECTION);
begin
  inherited Create;
  FCloned              := true;
  FDBName              := from.FDBName;
  FConnected           := from.FConnected;
  FPersistance_Layer   := from.FPersistance_Layer;
  //FSchemeCollection    := from.FSchemeCollection;
  FCollectionStore     := from.FCollectionStore;
  FApps                := from.FApps;
  //FSysEnum             := from.FSysEnum;
  //FSysClientValidators := from.FSysClientValidators;
end;



//function TFRE_DB_CONNECTION.SchemeExists(const scheme_name: TFRE_DB_String): boolean;
//begin
//  Result:=inherited SchemeExists(scheme_name);
//end;

function TFRE_DB_CONNECTION.GetScheme(const scheme_name: TFRE_DB_NameType; var scheme: TFRE_DB_SchemeObject): boolean;
begin
  Result:=inherited GetScheme(scheme_name, scheme);
  if result=false then begin //TODO
    result := FSysConnection.GetScheme(scheme_name,scheme); //try to find scheme in System schemes
  end;
end;

function TFRE_DB_CONNECTION.NewObject(const Scheme: TFRE_DB_String): TFRE_DB_Object;
begin
  Result:=inherited NewObject(Scheme);
end;

function TFRE_DB_CONNECTION.Delete(const ouid: TGUID): TFRE_DB_Errortype;
begin
  Result:=inherited Delete(ouid);
end;

destructor TFRE_DB_CONNECTION.Destroy;
begin
  inherited Destroy;
end;

//function TFRE_DB_CONNECTION.Collection(const collection_name: TFRE_DB_NameType): TFRE_DB_COLLECTION;
//begin
//  Result:=inherited Collection(collection_name);
//end;
//
//function TFRE_DB_CONNECTION.CollectionCC(const collection_name: TFRE_DB_NameType; const NewCollectionClass: TFRE_DB_COLLECTIONCLASS; const create_non_existing: boolean; const in_memory_only: boolean): TFRE_DB_COLLECTION;
//begin
//  Result:=inherited CollectionCC(collection_name, NewCollectionClass,create_non_existing,in_memory_only);
//end;

function  TFRE_DB_CONNECTION.FetchApplications(var apps: TFRE_DB_APPLICATION_ARRAY):TFRE_DB_Errortype;
var l_apps : TFRE_DB_APPLICATION_ARRAY;
    cnt,i  : integer;
begin
  if not FConnected then exit(edb_NOT_CONNECTED);
  if not assigned(FSysConnection) then Exit(edb_ACCESS);
  result := FSysConnection.FetchApplications(l_apps);
  //inherited FetchApplications(l_apps);
  SetLength(apps,length(l_apps));
  cnt := 0;
  for i := 0 to high(l_apps) do begin
    if lowercase(l_apps[i].ObjectName)='login' then continue;
    if FSysConnection._CheckRight(Get_Rightname_App(l_apps[i].ObjectName,'START')) then begin
      apps[cnt] := l_apps[i];
      inc(cnt);
    end;
  end;
  setlength(apps,cnt);
end;

function TFRE_DB_CONNECTION.InvokeMethod(const class_name, method_name: TFRE_DB_String; const uid_path: TFRE_DB_GUIDArray; const input: IFRE_DB_Object; const session: TFRE_DB_UserSession): IFRE_DB_Object;
var scheme:TFRE_DB_SchemeObject;
begin
  if not GetScheme(class_name,scheme) then raise EFRE_DB_Exception.Create(edb_ERROR,'SCHEME [%s] UNKNOWN',[class_name]);
  try
    if assigned(input) then begin
      result := scheme.InvokeMethod_UID_Session(uid_path,class_name,method_name,input.Implementor as TFRE_DB_Object,self,session);
    end else begin
      result := scheme.InvokeMethod_UID_Session(uid_path,class_name,method_name,nil,self,session);
    end;
  finally
    scheme.Finalize;
  end;
end;


function TFRE_DB_BASE_CONNECTION.NewScheme(const Scheme_Name: TFRE_DB_String; const parent_scheme_name: TFRE_DB_String): TFRE_DB_SchemeObject;
var ccn             : shortstring;
    base_code_class : TFRE_DB_OBJECTCLASS;
    ex_code_class   : TFRE_DB_OBJECTCLASSEX;
begin
  result := GFRE_DB.NewScheme(Scheme_Name,dbst_DB);
  result.FManageInfo := self;
  if parent_scheme_name<>'' then begin
     result.SetParentSchemeByName(parent_scheme_name);
  end;
end;

function TFRE_DB_BASE_CONNECTION.NewSchemeI(const Scheme_Name: TFRE_DB_String; const parent_scheme_name: TFRE_DB_String): IFRE_DB_SchemeObject;
begin
  result := NewScheme(Scheme_Name,parent_scheme_name);
end;


function TFRE_DB_BASE_CONNECTION.CollectionCN(const collection_name: TFRE_DB_NameType; const NewCollectionClassName: ShortString): TFRE_DB_COLLECTION;
var lCollectionClass:TFRE_DB_COLLECTIONCLASS;
    lobjClass       :TFRE_DB_OBJECTCLASS;
    ccn             :Shortstring;
begin
  ccn         := NewCollectionClassName;
  lobjClass   := TFRE_DB_COLLECTIONCLASS(GFRE_DB.GetObjectClass(ccn));
  result      := CollectionCC(collection_name,lCollectionClass);
end;

function TFRE_DB_BASE_CONNECTION.CollectionCC(const collection_name: TFRE_DB_NameType; const NewCollectionClass: TFRE_DB_COLLECTIONCLASS; const create_non_existing: boolean; const in_memory_only: boolean): TFRE_DB_COLLECTION;
var lcollection  : TFRE_DB_Collection;
    keyp         : PFRE_DB_NameType;
    storep       : ^TFRE_DB_Collection;
    FUPcoll_name : TFRE_DB_String;
    persColl     : IFRE_DB_PERSISTANCE_COLLECTION;

begin
  result:=nil;
  FUPcoll_name    := uppercase(collection_name);
  if FUPcoll_name='' then
    raise EFRE_DB_Exception.Create(edb_INVALID_PARAMS,'you must supply a collectionname when creating a collection');
  if FCollectionStore.Find(FUPcoll_name,lcollection) then begin
    result := lcollection;
  end else begin
    if FPersistance_Layer.GetCollection(FUPcoll_name,persColl) then
      begin
        lcollection := NewCollectionClass.Create(self,collection_name,persColl);
        if not FCollectionStore.Add(FUPcoll_name,lcollection) then raise EFRE_DB_Exception.create(edb_INTERNAL,'collectionstore');
        exit(lcollection);
      end;
    if create_non_existing then begin
      if not FPersistance_Layer.GetCollection(FUPcoll_name,persColl) then
        CheckDbResult(FPersistance_Layer.NewCollection(FUPcoll_name,persColl,in_memory_only,false),'cannot create new persistance collection : ',true);
      lcollection := NewCollectionClass.Create(self,collection_name,persColl);
      if not FCollectionStore.Add(FUPcoll_name,lcollection) then raise EFRE_DB_Exception.create(edb_INTERNAL,'collectionstore');
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
  if (c_name='MASTER') or (c_name='SCHEME') then exit(edb_RESERVED);
  if not FCollectionStore.Find(c_name,lcollection) then exit(edb_NOT_FOUND);
  result := FPersistance_Layer.DeleteCollection(c_name,false);
  FCollectionStore.Delete(c_name,lcollection);
  lcollection.Free;
end;

function TFRE_DB_BASE_CONNECTION.Collection(const collection_name: TFRE_DB_NameType;const create_non_existing:boolean=true;const in_memory:boolean=false):TFRE_DB_COLLECTION;
begin
  result:=CollectionCC(collection_name,TFRE_DB_COLLECTION,create_non_existing,in_memory);
end;


function TFRE_DB_BASE_CONNECTION.CollectionI(const collection_name: TFRE_DB_NameType;const create_non_existing:boolean=true;const in_memory:boolean=false): IFRE_DB_COLLECTION;
begin
  result := Collection(collection_name,create_non_existing,in_memory);
end;

function TFRE_DB_BASE_CONNECTION.CollectionAsIntf(const collection_name: TFRE_DB_NameType; const CollectionInterfaceSpec: ShortString; out Intf;const create_non_existing:boolean=true;const in_memory:boolean=false):boolean;
var coll      : TFRE_DB_COLLECTION;
    implclass : TClass;
    Fexisted  : Boolean;
begin
  if not GFRE_DB.GetInterfaceClass(CollectionInterfaceSpec,implclass) then raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'interface spec for existing collectiontype %s not found',[CollectionInterfaceSpec]);
  if not (implclass.InheritsFrom(TFRE_DB_COLLECTION)) then raise EFRE_DB_Exception.Create(edb_ERROR,'interface spec for existing collectiontype %s is not a Collectionclass',[CollectionInterfaceSpec,implclass.ClassName]);
  Fexisted := CollectionExists(collection_name);
  coll := CollectionCC(collection_name,TFRE_DB_COLLECTIONCLASS(implclass),create_non_existing,in_memory or TFRE_DB_COLLECTIONCLASS(implclass).Forced_In_Memory);
  coll.IntfCast(CollectionInterfaceSpec,intf);
  result := assigned(coll) and (Fexisted=false);
end;

procedure TFRE_DB_BASE_CONNECTION.NewObjectAsIntf(const ObjectInterfaceSpec: ShortString; out Intf);
begin
  GFRE_DB.NewObjectIntf(ObjectInterfaceSpec,Intf);
end;

function TFRE_DB_BASE_CONNECTION.Exists(const ouid: TGUID): boolean;
begin
  _ConnectCheck;
  FPersistance_Layer.ObjectExists(ouid);
end;

function TFRE_DB_BASE_CONNECTION.Delete(const ouid: TGUID): TFRE_DB_Errortype;
var dbo     : TFRE_DB_Object;
    ncolls  : TFRE_DB_StringArray;

  procedure CollIterator(const coll:TFRE_DB_COLLECTION);
  begin
    if coll.Remove(ouid) then begin
      try
        coll._NotifyObserversOrRecord(fdbntf_DELETE,nil,ouid);
      except on e:Exception do begin
        GFRE_DB.LogError(dblc_DB,'COULD NOT FIRE DELETE OBSERVER COLL [%s] UID[%s] ERR[%s] ',[coll.ClassName,GFRE_BT.GUID_2_HexString(ouid),e.Message]);
      end;end;
    end;
  end;

begin
  _ConnectCheck;
 result := FPersistance_Layer.DeleteObject(ouid,true,'',ncolls);
 if result=edb_OK then
   _NotifyCollectionObservers(fdbntf_DELETE,nil,ouid,ncolls);
end;

function TFRE_DB_BASE_CONNECTION.RemoveReferences(const ouid: TGUID): TFRE_DB_Errortype;
var dbo     : TFRE_DB_Object;
    reflist : TFRE_DB_GUIDArray;
    iref    : integer;
    refdbo  : TFRE_DB_Object;

    procedure FieldIterator(const fld:TFRE_DB_Field);
    var fldlist : TFRE_DB_GUIDArray;
        ifld    : integer;
    begin
      if fld.FieldType=fdbft_ObjLink then begin
        fldlist := fld.AsObjectLinkArray;
        for ifld := high (fldlist) downto 0 do begin
          if RB_Guid_Compare(fldlist[ifld],ouid)=0 then begin
            fld.RemoveObjectLink(ifld);
          end;
        end;
      end;
    end;

begin
  _ConnectCheck;
  result:=edb_OK;
  if Fetch(ouid,dbo) then begin
//    writeln(dbo.DumpToString);
    reflist := dbo.ReferencedByList;
//    writeln(length(reflist));
    for iref  := 0 to high (reflist) do begin
      Fetch(reflist[iref],refdbo);
      refdbo.ForAllFields(@FieldIterator);
      result  := Update(refdbo);
      if Result<>edb_OK then begin
        raise EFRE_DB_Exception.Create('Error on updating reference removed object '+GFRE_BT.GUID_2_HexString(reflist[iref]));
      end;
    end;
  end;
end;

function TFRE_DB_BASE_CONNECTION.InternalCheckRight(const right_name: TFRE_DB_String): boolean;
begin
  if self is TFRE_DB_CONNECTION then
    exit(TFRE_DB_CONNECTION(self).CheckRight(right_name));
  if self is TFRE_DB_SYSTEM_CONNECTION then
    exit(TFRE_DB_SYSTEM_CONNECTION(self).CheckRight(right_name));
  raise EFRE_DB_Exception.Create(edb_INTERNAL,'checkright basecass : '+self.ClassName);
end;

function TFRE_DB_BASE_CONNECTION.CheckRightForGroup(const right_name: TFRE_DB_String; const group_uid: TGuid): boolean;
begin
  if self is TFRE_DB_CONNECTION then
    exit(TFRE_DB_CONNECTION(self).CheckRightForGroup(right_name,group_uid));
  if self is TFRE_DB_SYSTEM_CONNECTION then
    exit(TFRE_DB_SYSTEM_CONNECTION(self).CheckRightForGroup(right_name,group_uid));
  raise EFRE_DB_Exception.Create(edb_INTERNAL,'checkrightforgroup basecass : '+self.ClassName);
end;

//function TFRE_DB_BASE_CONNECTION.Store(var new_obj: TFRE_DB_Object; const collection_name: TFRE_DB_NameType): TFRE_DB_Errortype;
//begin
//  if collection_name='' then
//    exit(edb_INVALID_PARAMS);
//  result := FPersistance_Layer.StoreOrUpdateObject(new_obj,collection_name);
//end;

function TFRE_DB_BASE_CONNECTION.Fetch(const ouid: TGUID; out dbo: TFRE_DB_Object): boolean;
begin
  dbo:=nil;
  result :=FPersistance_Layer.Fetch(ouid,dbo,false);
  if result then
    begin
      dbo.FManageInfo := self;
      exit;
    end;
  result := GFRE_DB.FetchSysObject(ouid,dbo);
end;

function TFRE_DB_BASE_CONNECTION.FetchInternal(const ouid: TGUID; out dbo: TFRE_DB_Object): boolean;
begin
  dbo:=nil;
  result :=FPersistance_Layer.Fetch(ouid,dbo,true);
end;


function TFRE_DB_BASE_CONNECTION.Update(const dbo: TFRE_DB_Object): TFRE_DB_Errortype;
var dboo   : TFRE_DB_Object;
    objuid : TGUID;
    ncolls : TFRE_DB_StringArray;
begin
  dboo   := dbo;
  objuid := dbo.UID;
  result := FPersistance_Layer.StoreOrUpdateObject(dboo,'',false,true,ncolls);
  if result=edb_OK then
    begin
      _NotifyCollectionObservers(fdbntf_UPDATE,nil,objuid,ncolls);
    end;
end;

function TFRE_DB_BASE_CONNECTION.UpdateI(const dbo: IFRE_DB_Object): TFRE_DB_Errortype;
begin
  result := Update(dbo.Implementor as TFRE_DB_Object);
end;

function TFRE_DB_BASE_CONNECTION.UpdateScheme(const dbo: IFRE_DB_SCHEMEOBJECT): TFRE_DB_Errortype;
begin
  result := Update(dbo.Implementor as TFRE_DB_Object); // I know that a IFRE_DB_Schemeobject's implementor is derived from TFRE_DB_Object
end;

function TFRE_DB_CONNECTION.CreateschemeUniqueKeyDefinition(const schemeClass, FieldName: TFRE_DB_String; const FieldType: TFRE_DB_FIELDTYPE): TFRE_DB_Errortype;
begin
  abort;
end;

function TFRE_DB_CONNECTION.DropschemeUniqueKeyDefinition(const schemeClass, FieldName: TFRE_DB_String; const FieldType: TFRE_DB_FIELDTYPE): TFRE_DB_Errortype;
begin
  abort;
end;

function TFRE_DB_CONNECTION.AddUser(const loginatdomain, password, first_name, last_name: TFRE_DB_String): TFRE_DB_Errortype;
begin
  result := FSysConnection.AddUser(loginatdomain,password,first_name,last_name);
end;

function TFRE_DB_CONNECTION.UserExists(const loginatdomain: TFRE_DB_String): boolean;
begin
 result := FSysConnection.UserExists(loginatdomain);
end;

function TFRE_DB_CONNECTION.DeleteUser(const loginatdomain: TFRE_DB_String): TFRE_DB_Errortype;
begin
 result := FSysConnection.DeleteUser(loginatdomain);
end;

function TFRE_DB_CONNECTION.DeleteUserById(const user_id: TGUID): TFRE_DB_Errortype;
begin
  result :=FSysConnection.DeleteUserById(user_id);
end;

function TFRE_DB_CONNECTION.FetchUser(const loginatdomain: TFRE_DB_String; var user: IFRE_DB_USER): TFRE_DB_Errortype;
begin
 result := FSysConnection.FetchUserI(loginatdomain,user);
end;

function TFRE_DB_CONNECTION.FetchUserById(const user_id: TGuid; var user: IFRE_DB_USER): TFRE_DB_Errortype;
begin
 Result:= FSysConnection.FetchUserByIdI(user_id,user);
end;

function TFRE_DB_CONNECTION.FetchGroup(const groupatdomain: TFRE_DB_String; var ug: IFRE_DB_GROUP): TFRE_DB_Errortype;
begin
  Result := FSysConnection.FetchGroupI(groupatdomain,ug);
end;

function TFRE_DB_CONNECTION.FetchGroupById(const group_id: TGUID; var ug: IFRE_DB_GROUP): TFRE_DB_Errortype;
begin
  Result:= FSysConnection.FetchGroupByIdI(group_id,ug);
end;

function TFRE_DB_CONNECTION.FetchRole(const roleatdomain: TFRE_DB_NameType; var role: IFRE_DB_ROLE): TFRE_DB_Errortype;
begin
  result := FSysConnection.FetchRoleI(roleatdomain,role);
end;

function TFRE_DB_CONNECTION.FetchRoleById(const role_id: TGUID; var role: IFRE_DB_ROLE): TFRE_DB_Errortype;
begin
  Result:= FSysConnection.FetchRoleByIdI(role_id,role);
end;

function TFRE_DB_CONNECTION.NewRight(const rightname, txt, txt_short: TFRE_DB_String; var right: IFRE_DB_RIGHT): TFRE_DB_Errortype;
begin
 result := FSysConnection.NewRightI(rightname,txt,txt_short,right);
end;

function TFRE_DB_CONNECTION.NewRole(const rolename, txt, txt_short: TFRE_DB_String; var role: IFRE_DB_ROLE): TFRE_DB_Errortype;
begin
 result := FSysConnection.NewRoleI(rolename,txt,txt_short,role);
end;

function TFRE_DB_CONNECTION.NewGroup(const groupname, txt, txt_short: TFRE_DB_String; var group: IFRE_DB_GROUP): TFRE_DB_Errortype;
begin
 result := FSysConnection.NewGroupI(groupname,txt,txt_short,group);
end;

function TFRE_DB_CONNECTION.ModifyGroupRoles(const groupatdomain: TFRE_DB_String; const roles: TFRE_DB_StringArray): TFRE_DB_Errortype;
begin
  result := FSysConnection.ModifyGroupRoles(groupatdomain,roles);
end;

function TFRE_DB_CONNECTION.AddGroupRoles(const groupatdomain: TFRE_DB_String; const roles: TFRE_DB_StringArray): TFRE_DB_Errortype;
begin
  result := FSysConnection.AddGroupRoles(groupatdomain,roles);
end;

function TFRE_DB_CONNECTION.RemoveGroupRoles(const groupatdomain: TFRE_DB_String; const roles: TFRE_DB_StringArray; const ignore_not_set: boolean): TFRE_DB_Errortype;
begin
  result := FSysConnection.RemoveGroupRoles(groupatdomain,roles, ignore_not_set);
end;

function TFRE_DB_CONNECTION.ModifyUserGroups(const loginatdomain: TFRE_DB_String; const user_groups: TFRE_DB_StringArray; const keep_existing_groups: boolean): TFRE_DB_Errortype;
begin
 result := FSysConnection.ModifyUserGroups(loginatdomain,user_groups,keep_existing_groups);
end;

function TFRE_DB_CONNECTION.RemoveUserGroups(const loginatdomain: TFRE_DB_String; const user_groups: TFRE_DB_StringArray): TFRE_DB_Errortype;
begin
 result := FSysConnection.RemoveUserGroups(loginatdomain,user_groups);
end;

function TFRE_DB_CONNECTION.ModifyUserPassword(const loginatdomain, oldpassword, newpassword: TFRE_DB_String): TFRE_DB_Errortype;
begin
 result := FSysConnection.ModifyUserPassword(loginatdomain,oldpassword,newpassword);
end;

function TFRE_DB_CONNECTION.RoleExists(const roleatdomain: TFRE_DB_String): boolean;
begin
 result := FSysConnection.RoleExists(roleatdomain);
end;

function TFRE_DB_CONNECTION.GroupExists(const groupatdomain: TFRE_DB_String): boolean;
begin
 result := FSysConnection.GroupExists(groupatdomain);
end;

function TFRE_DB_CONNECTION.DeleteGroup(const groupatdomain: TFRE_DB_String): TFRE_DB_Errortype;
begin
 result := FSysConnection.DeleteGroup(groupatdomain);
end;

function TFRE_DB_CONNECTION.DeleteRole(const roleatdomain: TFRE_DB_String): TFRE_DB_Errortype;
begin
 result := FSysConnection.DeleteRole(roleatdomain);
end;

function TFRE_DB_CONNECTION.StoreRole(const appname: TFRE_DB_String; const domainname: TFRE_DB_NameType; var rg: IFRE_DB_ROLE): TFRE_DB_Errortype;
begin
 result := FSysConnection.StoreRoleI(appname,domainname,rg);
end;

function TFRE_DB_CONNECTION.StoreGroup(const appname:TFRE_DB_String;  const domainname:TFRE_DB_NameType; var ug: IFRE_DB_GROUP): TFRE_DB_Errortype;
begin
 result := FSysConnection.StoreGroupI(appname,domainname,ug);
end;

function TFRE_DB_CONNECTION.StoreGroupDomainbyID(const domain_id: TGUID; var group: IFRE_DB_GROUP): TFRE_DB_Errortype;
begin
  result := FSysConnection.StoreGroupDomainbyIDI(domain_id,group);
end;

function TFRE_DB_CONNECTION.CheckRight(const right_name: TFRE_DB_String): boolean;
begin
 result := FSysConnection.CheckRight(right_name);
end;

function TFRE_DB_CONNECTION.AddDomain(const domainname: TFRE_DB_NameType; const txt, txt_short: TFRE_DB_String): TFRE_DB_Errortype;
begin
  result := FSysConnection._AddDomain(domainname,txt,txt_short);
end;

function TFRE_DB_CONNECTION.FetchDomainById(const domain_id: TGUID; var domain: IFRE_DB_DOMAIN): TFRE_DB_Errortype;
begin
  Result:= FSysConnection.FetchDomainByIdI(domain_id,domain);
end;

function TFRE_DB_CONNECTION.DomainId(const name: TFRE_DB_NameType): TGUID;
begin
  Result:= FSysConnection._DomainID(name);
end;

function TFRE_DB_CONNECTION.ModifyDomainById(const domain_id: TGUID; const domainname: TFRE_DB_NameType; const txt, txt_short: TFRE_DB_String): TFRE_DB_Errortype;
begin
  Result:= FSysConnection.ModifyDomainById(domain_id,domainname,txt,txt_short);
end;

function TFRE_DB_CONNECTION.DeleteDomainById(const domain_id: TGUID): TFRE_DB_Errortype;
begin
  Result:= FSysConnection.DeleteDomainById(domain_id);
end;

function TFRE_DB_CONNECTION.IsSystemGroup(const group_id: TGUID): boolean;
begin
 Result:= FSysConnection.IsSystemGroup(group_id);
end;

function TFRE_DB_CONNECTION.CheckRightForGroup(const right_name: TFRE_DB_String; const group_uid: TGuid): boolean;
begin
  result := FSysConnection.CheckRightForGroup(right_name,group_uid);
end;

function TFRE_DB_CONNECTION.Fetch(const ouid: TGUID; out dbo: TFRE_DB_Object): boolean;
begin
  Result:=inherited Fetch(ouid, dbo);
  if not result then
    result := FSysConnection.Fetch(ouid,dbo);
end;

function TFRE_DB_CONNECTION.FetchInternal(const ouid: TGUID; out dbo: TFRE_DB_Object): boolean;
begin
  Result:=inherited FetchInternal(ouid, dbo);
  if not result then
    result := FSysConnection.FetchInternal(ouid,dbo);
end;

function TFRE_DB_CONNECTION.AdmGetUserCollection: IFRE_DB_COLLECTION;
begin
 result := FSysConnection.FSysUsers; // TODO: CHECK RIGHTS
end;

function TFRE_DB_CONNECTION.AdmGetRoleCollection: IFRE_DB_COLLECTION;
begin
 result := FSysConnection.FSysRoles; // TODO: CHECK RIGHTS
end;

function TFRE_DB_CONNECTION.AdmGetGroupCollection: IFRE_DB_COLLECTION;
begin
 result := FSysConnection.FSysGroups; // TODO: CHECK RIGHTS
end;

function TFRE_DB_CONNECTION.AdmGetDomainCollection: IFRE_DB_COLLECTION;
begin
 result := FSysConnection.FSysDomains; // TODO: CHECK RIGHTS
end;

function TFRE_DB_CONNECTION.BackupDatabaseReadable(const to_stream: TStream;const stream_cb:TFRE_DB_StreamingCallback;const progress : TFRE_DB_PhaseProgressCallback): TFRE_DB_Errortype;
begin
  Inherited BackupDatabaseReadable(to_stream,stream_cb,progress);
end;

function TFRE_DB_CONNECTION.RestoreDatabaseReadable(const from_stream: TStream; const stream_cb: TFRE_DB_StreamingCallback): TFRE_DB_Errortype;
begin
  Result:=inherited RestoreDatabaseReadable(from_stream, stream_cb);
end;

function TFRE_DB_CONNECTION.FetchUserSessionData(var SessionData: IFRE_DB_OBJECT): boolean;
begin
  result := FSysConnection.FetchUserSessionData(SessionData);
end;

function TFRE_DB_CONNECTION.StoreUserSessionData(var session_data: IFRE_DB_Object):TFRE_DB_Errortype;
begin
  result := FSysConnection.StoreUserSessionData(session_data);
end;

function TFRE_DB_CONNECTION.FetchTranslateableText(const trans_key: TFRE_DB_String; var text: IFRE_DB_TEXT): boolean;
var ttxt : TFRE_DB_TEXT;
begin
  if FSysConnection.FetchTranslateableText(trans_key,ttxt)=edb_OK then begin
    text   := ttxt;
    result := true;
  end else begin
    text   := nil;
    result := false;
  end;
end;

function TFRE_DB_CONNECTION.GetReferences(const obj_uid: TGuid; const from: boolean): TFRE_DB_ObjectReferences;
begin
  Result:=inherited GetReferences(obj_uid, from);
  if not assigned(Result) then
    result := FSysConnection.GetReferences(obj_uid,from);
end;

function TFRE_DB_CONNECTION.GetReferences(const obj_uid: TGuid; const from: boolean; const substring_filter: TFRE_DB_String): TFRE_DB_GUIDArray;
var refs : TFRE_DB_ObjectReferences;
    i    : NativeInt;
    idx  : NativeInt;

    procedure AddToResult(const ll : TFRE_DB_GUIDArray);
    var i : NativeInt;
    begin
      if Length(result)<Length(ll) then
        SetLength(result,Length(result)+100);
      for i:= 0 to high(ll) do
        begin
          result[idx] := ll[i];
          inc(idx);
        end;
    end;

begin
  refs := GetReferences(obj_uid, from);
  for i := 0 to high(refs) do
    begin
      if pos(substring_filter,refs[i].fieldname)>0 then
        AddToResult(refs[i].linklist);
    end;
end;

function TFRE_DB_CONNECTION.GetReferencesCount(const obj_uid: TGuid; const from: boolean; const substring_filter: TFRE_DB_String; const stop_on_first: boolean): NativeInt;
var refs : TFRE_DB_ObjectReferences;
    i    : NativeInt;
    filt : TFRE_DB_NameType;

begin
  result := 0;
  filt   := UpperCase(substring_filter);
  refs := GetReferences(obj_uid, from);
  for i := 0 to high(refs) do
    begin
      if pos(filt,refs[i].fieldname)>0 then
        inc(Result,Length(refs[i].linklist));
      if (stop_on_first=true)
         and (result>0) then
           exit;
    end;
end;

procedure TFRE_DB_CONNECTION.ExpandReferences(ObjectList: TFRE_DB_GUIDArray; ObjectsRefers: Boolean; ref_constraints: TFRE_DB_StringArray; var expanded_refs: TFRE_DB_ObjectArray; fetch_internal: boolean);
var i        : NativeInt;
    FReflist : TFRE_DB_GUIDArray;
begin
  ExpandReferences(ObjectList,ObjectsRefers,ref_constraints,FReflist);
  SetLength(expanded_refs,Length(FReflist));
  for i := 0 to high(expanded_refs) do
    if fetch_internal then
      begin
        if not FetchInternal(FReflist[i],expanded_refs[i]) then
          raise EFRE_DB_Exception.Create(edb_INTERNAL,'FAILED TO FETCH EXPANDED REFERENCED;CHAINED OBJECT');
      end
    else
    begin
      if not Fetch(FReflist[i],expanded_refs[i]) then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'FAILED TO FETCH EXPANDED REFERENCED;CHAINED OBJECT');
    end
end;

procedure TFRE_DB_CONNECTION.ExpandReferences(ObjectList: TFRE_DB_GUIDArray; ObjectsRefers: Boolean; ref_constraints: TFRE_DB_StringArray; var expanded_refs: TFRE_DB_GUIDArray);
var i        : NativeInt;
    obj      : TFRE_DB_Object;
    compare  : TFRE_DB_NameTypeRL;
    count    : NativeInt;

  procedure FetchChained(uid:TGuid ; field_chain : TFRE_DB_StringArray ; depth : NativeInt);
  var obrefs   : TFRE_DB_ObjectReferences;
      i,k      : NativeInt;
  begin
    if depth<length(field_chain) then
      begin
        obrefs := GetReferences(uid,ObjectsRefers);
        for i := 0 to  high(obrefs) do
          begin
            compare := uppercase(obrefs[i].fieldname);
            if  pos(field_chain[depth],compare)>0 then
              for k := 0 to high(obrefs[i].linklist) do
                FetchChained(obrefs[i].linklist[k],field_chain,depth+1);
          end;
      end
    else
      begin
        if Length(expanded_refs) = count then
          SetLength(expanded_refs,Length(expanded_refs)+256);
        if not GuidInArray(uid,expanded_refs) then
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

function TFRE_DB_CONNECTION.DerivedCollection(const collection_name: TFRE_DB_NameType; const create_non_existing: boolean): TFRE_DB_DERIVED_COLLECTION;
begin
  result := CollectionCC(collection_name,TFRE_DB_DERIVED_COLLECTION,create_non_existing,true) as TFRE_DB_DERIVED_COLLECTION;
end;


function TFRE_DB_CONNECTION.DerivedCollectionI(const collection_name: TFRE_DB_NameType; const create_non_existing: boolean): IFRE_DB_DERIVED_COLLECTION;
begin
  result := DerivedCollection(collection_name,create_non_existing);
end;


function TFRE_DB_CONNECTION.NewWorkFlowScheme(const WF_SchemeName: TFRE_DB_NameType): IFRE_DB_WORKFLOW;
var wfo : TFRE_DB_WORKFLOW;
begin
  wfo := TFRE_DB_WORKFLOW.Create;
  wfo.ObjectName := WF_SchemeName;
  result := wfo;
end;

function TFRE_DB_CONNECTION.StoreWorkFlowScheme(const WFS: IFRE_DB_WORKFLOW): TFRE_DB_Errortype;
var obj:TFRE_DB_Object;
begin
  obj    := WFS.Implementor as TFRE_DB_Object;
  result := FSysWorkflowSchemes.Store(obj);
end;

function TFRE_DB_CONNECTION.FetchWorkFlowScheme(const WF_SchemeName: TFRE_DB_NameType; var WFS: IFRE_DB_WORKFLOW): Boolean;
var obj : TFRE_DB_Object;
begin
  if FSysWorkflowSchemes.GetIndexedObj(WF_SchemeName,obj) then begin
    obj.IntfCast(IFRE_DB_WORKFLOW,WFS);
    result := true;
  end else begin
    result := false;
  end;
end;

function TFRE_DB_CONNECTION.WorkFlowSchemeExists(const WF_SchemeName: TFRE_DB_NameType): boolean;
var wfs:IFRE_DB_WORKFLOW;
begin
  result := FetchWorkFlowScheme(WF_SchemeName,wfs);
end;

procedure TFRE_DB_CONNECTION.ForAllWorkFlowSchemes(const iterator: IFRE_DB_Workflow_Iterator);
  procedure Iterate(const obj:TFRE_DB_Object);
  begin
    Iterator(obj as TFRE_DB_WORKFLOW);
  end;
begin
  FSysWorkflowSchemes.ForAll(@Iterate);
end;

function TFRE_DB_CONNECTION.DeleteWorkFlowScheme(const WF_SchemeName: TFRE_DB_NameType): TFRE_DB_Errortype;
begin
  FSysWorkflowSchemes.RemoveIndexed(WF_SchemeName);
end;

function TFRE_DB_CONNECTION.StartNewWorkFlow(const WF_SchemeName: TFRE_DB_NameType; const WF_UniqueKey: TFRE_DB_String): UInt64;
begin

end;

function TFRE_DB_CONNECTION.StartNewWorkFlowRecurring(const WF_SchemeName: TFRE_DB_NameType; const WF_UniqueKey: TFRE_DB_String; const sec_interval: integer): UInt64;
begin

end;



{ TFRE_DB }

procedure TFRE_DB.SetLocalZone(const AValue: TFRE_DB_String);
begin
  FLocalZone:=AValue;
end;

function TFRE_DB.NewObjectStreaming(const ClName:ShortString;const conn:TFRE_DB_BASE_Connection): TFRE_DB_Object;
var ex_obj    : TFRE_DB_ObjectEx;
    obj_cl    : TFRE_DB_OBJECTCLASS;
    ex_obj_cl : TFRE_DB_OBJECTCLASSEX;
begin
  obj_cl := GetObjectClass(ClName);
  if assigned(obj_cl) then begin
    result := obj_cl.CreateStreaming(conn);
  end else begin
    ex_obj_cl := GetObjectClassEx(ClName);
    if assigned(ex_obj_cl) then begin
//      result    := TFRE_DB_Object.CreateStreaming(conn,ex_obj_cl);
      result    := TFRE_DB_NAMED_OBJECT.CreateStreaming(conn,ex_obj_cl);
    end else begin
       raise EFRE_DB_Exception.Create(edb_ERROR,'STREAMING FAILURE OBJECT CLASS ['+ClName+'] IS UNKNOWN, YOU MAY NEED TO REGISTER IT');
    end;
  end;
end;

procedure TFRE_DB._InternalLog(const typ: integer; const level:TFOS_LOG_LEVEL; const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String; const param: array of const);
var cat      : TFRE_DB_String;
    logentry : TFRE_DB_String;
    target   : string;
begin
  cat := CFRE_DB_LOGCATEGORY[category];
  case typ of
    0 : target:='INFO';
    1 : target:='WARNING';
    2 : target:='ERROR';
    3 : target:='DEBUG';
    4 : target:='NOTICE';
  end;
  GFRE_LOG.Log(msg,param,cat,level,target,false);
end;

function TFRE_DB.NewScheme(const Scheme_Name: TFRE_DB_String; const typ: TFRE_DB_SchemeType): TFRE_DB_SchemeObject;
var ccn             : shortstring;
    base_code_class : TFRE_DB_OBJECTCLASS;
    ex_code_class   : TFRE_DB_OBJECTCLASSEX;
begin
  assert(typ<>dbst_INVALID);
  ccn                          := uppercase(Scheme_Name);
  result                       := TFRE_DB_SchemeObject.Create;
  result.FSchemeClass.AsString := uppercase(ccn);
  result.FStrict.AsBoolean     := false;
  result.FSchemeType.AsInt16   := ord(typ);
  base_code_class              := GetObjectClass(ccn);
  if assigned(base_code_class) then begin
    result.FHasHardcodeClass.AsBoolean:=true;
    result.FHardCodeClassTyp := base_code_class;
    //writeln('B CLASSTYPE : ', result.FHardCodeClassTyp.ClassName,' ',integer(result));
  end else begin
    ex_code_class := GetObjectClassEx(ccn);
    if assigned(ex_code_class) then begin
      result.FHasHardcodeClass.AsBoolean:=true;
      result.FHardCodeClassTyp := ex_code_class;
      ccn :=  result.FHardCodeClassTyp.ClassName;
      //writeln('EX CLASSTYPE : ', result.FHardCodeClassTyp.ClassName,' ',integer(result),' ',ex_code_class.ClassName);
    end else begin
      ccn        := 'TFRE_DB_OBJECT';
      result.FHasHardcodeClass.AsBoolean:=false;
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

function TFRE_DB.FetchApplications(var apps: IFRE_DB_APPLICATION_ARRAY): TFRE_DB_Errortype;
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
     abort; // class construction only works with the "right" base class
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

function TFRE_DB.CreateFromFileI(const filename: TFRE_DB_String; const conn: IFRE_DB_CONNECTION; const recreate_weak_schemes: boolean): IFRE_DB_Object;
begin
 if Assigned(conn) then begin
   result := TFRE_DB_Object.CreateFromFile(filename,conn.Implementor  as TFRE_DB_BASE_CONNECTION,recreate_weak_schemes);
 end else begin
   result := TFRE_DB_Object.CreateFromFile(filename,nil,recreate_weak_schemes);
 end;
end;

function TFRE_DB.CreateFromMemoryI(memory: Pointer; const conn: IFRE_DB_CONNECTION; const recreate_weak_schemes: boolean): IFRE_DB_Object;
begin
  if assigned(conn) then begin
    try
      result := TFRE_DB_Object.CreateFromMemory(memory,conn.Implementor  as TFRE_DB_BASE_CONNECTION,recreate_weak_schemes);
    except on e:exception do begin
    end;end;
  end else begin
    try
      result := TFRE_DB_Object.CreateFromMemory(memory,nil,recreate_weak_schemes);
    except on e:exception do begin
    end;end;
  end;
end;

function TFRE_DB.CreateFromStringI(const AValue: TFRE_DB_String; const conn: IFRE_DB_CONNECTION; const recreate_weak_schemes: boolean): IFRE_DB_Object;
begin
  result := TFRE_DB_Object.CreateFromString(AValue,conn.Implementor  as TFRE_DB_BASE_CONNECTION,recreate_weak_schemes);
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
  result := GFRE_DB_DEFAULT_PS_LAYER.DatabaseList; // TODO Network, etc
end;

function TFRE_DB.SystemSchemeExists(var obj: TFRE_DB_SchemeObject): boolean;
begin
  result := FSystemSchemes.SchemeExists(obj.DefinedSchemeName);
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

function TFRE_DB._NewRight(const rightname, txt, txt_short: TFRE_DB_String): TFRE_DB_RIGHT;
var l_rname:TFRE_DB_String;
begin
  result   := _NewObject(TFRE_DB_RIGHT.ClassName,true) as TFRE_DB_RIGHT;
  l_rname  := uppercase(rightname);
  result.ObjectName  := l_rname;
  result.Description := _NewText('$SYST_RIGHT_'+l_rname,txt,txt_short);
end;

function TFRE_DB._NewRole(const rolename, txt, txt_short: TFRE_DB_String): TFRE_DB_ROLE;
var l_gname:TFRE_DB_String;
begin
  result := _NewObject(TFRE_DB_ROLE.ClassName,true) as TFRE_DB_ROLE;
  l_gname  := uppercase(rolename);
  result.ObjectName  := l_gname;
  result.Description := _NewText('$SYST_ROLE_'+l_gname,txt,txt_short);
end;

function TFRE_DB._NewGroup(const groupname, txt, txt_short: TFRE_DB_String): TFRE_DB_GROUP;
var l_gname:TFRE_DB_String;
begin
  result := _NewObject(TFRE_DB_GROUP.ClassName,true) as TFRE_DB_GROUP;
  l_gname  := uppercase(groupname);
  result.ObjectName  := l_gname;
  result.Description := _NewText('$SYST_GROUP_'+l_gname,txt,txt_short);
end;

function TFRE_DB._NewAppData(const appname, txt, txt_short: TFRE_DB_String): TFRE_DB_APPDATA;
var l_appname:TFRE_DB_String;
begin
  result := _NewObject(TFRE_DB_APPDATA.ClassName,true) as TFRE_DB_APPDATA;
  l_appname  := uppercase(appname);
  result.ObjectName  := l_appname;
  result.Description := _NewText('$SYST_APPDATA_'+l_appname,txt,txt_short);
end;

function TFRE_DB._NewDomain(const domainname, txt, txt_short: TFRE_DB_String): TFRE_DB_DOMAIN;
var l_dname:TFRE_DB_String;
begin
  result := _NewObject(TFRE_DB_DOMAIN.ClassName,true) as TFRE_DB_DOMAIN;
  l_dname  := uppercase(domainname);
  result.ObjectName  := l_dname;
  result.Description := _NewText('$SYST_DOMAIN_'+l_dname,txt,txt_short);
end;

function TFRE_DB.NewText(const key, txt, txt_short: TFRE_DB_String; const hint: TFRE_DB_String): IFRE_DB_TEXT;
begin
  result := _NewText(key,txt,txt_short,hint);
end;

function TFRE_DB.NewRight(const rightname, txt, txt_short: TFRE_DB_String): IFRE_DB_RIGHT;
begin
  result := _NewRight(rightname,txt,txt_short);
end;

function TFRE_DB.NewRole(const rolename, txt, txt_short: TFRE_DB_String): IFRE_DB_ROLE;
begin
  result := _NewRole(rolename,txt,txt_short);
end;

function TFRE_DB.NewGroup(const groupname, txt, txt_short: TFRE_DB_String): IFRE_DB_GROUP;
begin
  result := _NewGroup(groupname,txt,txt_short);
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

function TFRE_DB.Get_A_Guid: TGUID;
begin
  CreateGUID(result);
end;

function TFRE_DB.Get_A_Guid_HEX: Ansistring;
var x:TGuid;
begin
  x := Get_A_Guid;
  result := GFRE_BT.Mem2HexStr(PByte(@x),sizeof(TGuid));
end;

function TFRE_DB.InstallDBDefaults(const conn: IFRE_DB_SYS_CONNECTION): TFRE_DB_Errortype;

  function _storeText(const key,value:TFRE_DB_String):TFRE_DB_Errortype;
  var
    txt: IFRE_DB_TEXT;
  begin
   txt:=GFRE_DBI.CreateText(key,value);
   Result:=conn.StoreTranslateableText(txt);
  end;

begin
 _storeText('$scheme_TFRE_DB_TEXT_descr_group','Description');

 _storeText('$scheme_TFRE_DB_TEXT_txt','Description');
 _storeText('$scheme_TFRE_DB_TEXT_txt_s','Brief Description');
 _storeText('$scheme_TFRE_DB_TEXT_hnt','Hint');
 _storeText('$scheme_TFRE_DB_TEXT_key','Key');

 _storeText('$scheme_TFRE_DB_USER_user_group','User');
 _storeText('$scheme_TFRE_DB_USER_descr_group','Description');
 _storeText('$scheme_TFRE_DB_USER_picture_group','Picture');
 _storeText('$scheme_TFRE_DB_USER_login','Login name');
 _storeText('$scheme_TFRE_DB_USER_firstname','Firstname');
 _storeText('$scheme_TFRE_DB_USER_lastname','Lastname');
 _storeText('$scheme_TFRE_DB_USER_passwordMD5','Password');
 _storeText('$scheme_TFRE_DB_USER_picture','Picture');
 _storeText('$scheme_TFRE_DB_USER_domain_group','Domain');
 _storeText('$scheme_TFRE_DB_USER_domainid','Domain');

 _storeText('$scheme_TFRE_DB_GROUP_group_group','Group');
 _storeText('$scheme_TFRE_DB_GROUP_group_domain','Domain');
 _storeText('$scheme_TFRE_DB_GROUP_name','Name');
 _storeText('$scheme_TFRE_DB_GROUP_domainid','Domain');

 _storeText('$scheme_TFRE_DB_DOMAIN_group','Domain');
 _storeText('$scheme_TFRE_DB_DOMAIN_name','Name');

 _storeText('$scheme_input_confirm_prefix','Confirm');
end;

procedure TFRE_DB.AddSystemObjectToSysList(const obj: TFRE_DB_Object);
var i     : integer;
    oguid : TGUID;
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

function TFRE_DB.FetchSysObject(const uid: TGUID; var obj: TFRE_DB_Object): boolean;
var i     : integer;
begin
  for i:=0 to high(FSysObjectList) do begin
    if FREDB_Guids_Same(FSysObjectList[i].SysGuid,uid) then begin
      obj := FSysObjectList[i].Obj;
      if not (fop_READ_ONLY in obj.Properties) then raise EFRE_DB_Exception.Create(edb_ERROR,'SYSTEM OBJECTS MUST BE READ ONLY!');
      if not (fop_SYSTEM in obj.Properties) then raise EFRE_DB_Exception.Create(edb_ERROR,'SYSTEM OBJECTS MUST BE READ ONLY!');
      exit(true);
    end;
  end;
  result := false;
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
        CheckDbResultFmt(FSystemSchemes.StoreScheme(lscheme),'error storing extension lscheme %s',[cn]);
        FSystemSchemes.GetScheme(cn,lscheme);
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

function TFRE_DB.JSONObject2Object(const json_string: string): IFRE_DB_Object;
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
           result[i] := GFRE_BT.HexString_2_GUID(ljo.AsString);
         end;
       end else begin
         SetLength(result,0);
       end;
       ljd.Free;
     finally
       ljp.Free;
     end;
   end;


  procedure _ConvertObject(const l_JSONObject : TJSONObject ; const l_DataObj : IFRE_DB_Object);
  var i,j           : integer;
      l_JSONItem    : TJSONData;
      l_SubDataObj  : IFRE_DB_Object;
      l_GotUid      : Boolean;
      l_Fields      : TFOSStringArray;
      l_sub_objects : TFRE_DB_StringArray;
      s             : string;
      l_FieldName   : String;

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
          l_DataObj.Field('uid').AsGUID := GFRE_BT.HexString_2_GUID(l_JSONItem.AsString);
          l_GotUid := true;
        end else begin
          if lowercase(l_FieldName)='uidpath' then begin
            l_DataObj.Field('uidpath').AsGUIDArr := JSON2GUidArray(l_JSONItem.AsJSON);
          end else begin
            if l_JSONItem is TJSONArray then begin
              if l_JSONItem.Count=0 then begin
                 l_DataObj.Field(l_FieldName).SetAsEmptyStringArray;
              end else begin
                for j := 0 to l_JSONItem.Count - 1 do begin
                  if l_JSONItem.Items[j] is TJSONString then begin
                    l_DataObj.Field(l_FieldName).AddString(l_JSONItem.Items[j].AsString);
                  end else
                  if l_JSONItem.Items[j] is TJSONNumber then begin
                    if TJSONNumber(l_JSONItem.Items[j]).NumberType=ntFloat then begin
                      l_DataObj.Field(l_FieldName).AddReal64(l_JSONItem.Items[j].AsFloat);
                    end else begin
                      l_DataObj.Field(l_FieldName).AddInt64(l_JSONItem.Items[j].AsInteger);
                    end;
                  end else
                  if l_JSONItem.Items[j] is TJSONObject then begin
                    l_SubDataObj := GFRE_DBI.NewObject;
                    l_DataObj.Field(l_FieldName).AddObject(l_SubDataObj);
                    _ConvertObject(TJSONObject(l_JSONItem.Items[j]),l_SubDataObj);
                  end else raise EFRE_DB_Exception.Create(edb_ERROR,'unexpected JSON Element in array %s / %s',[l_FieldName,l_JSONItem.ClassName]);
                end;
              end;
            end else
            if l_JSONItem is TJSONNull then begin
              l_DataObj.Field(l_FieldName).Clear;  // Remove JSON Null Fields
            end
            else begin
              l_DataObj.Field(l_FieldName).AsString := l_JSONItem.AsString;
            end;
          end;
        end;
      end;
    end;
    l_sub_objects := l_DataObj.GetFieldListFilter(fdbft_Object);
  end;

begin
  l_JSONParser := TJSONParser.Create(json_string);
  try
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
    l_JSONParser.free;
  end;
end;

function TFRE_DB.DefaultDirectory: TFRE_DB_String;
begin
  result:=SetDirSeparators(GFRE_BT.GetUserDir+'.fre/db/');
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

constructor TFRE_DB.create;
var pers_coll : IFRE_DB_PERSISTANCE_COLLECTION;
begin
  FFormatSettings := DefaultFormatSettings;
  GFRE_DB_DEFAULT_PS_LAYER.NewCollection('SysSchemes',pers_coll,true,true);
  FSystemSchemes  := TFRE_DB_SCHEME_COLLECTION.Create(nil,'SysSchemes',pers_coll);
  FSystemSchemes.DefineIndexOnField('C',fdbft_String,true,true);
end;

destructor TFRE_DB.Destroy;
begin
  writeln('TODO-{B9293265-8A09-B97F-EE8E-E5B9D2769FAD}');
  //FSystemSchemes.ClearCollection;
  //FSystemSchemes.Free;
  inherited Destroy;
end;

function TFRE_DB.GetApps: TFRE_DB_APPLICATION_ARRAY;
begin
  result := FAppArray;
end;

function TFRE_DB.GetApp(name: TFRE_DB_String; out app: TFRE_DB_APPLICATION): boolean;
var i: Integer;
begin
  name := uppercase(name);
  for i:=0 to high(FAppArray) do begin
    if uppercase(FAppArray[i].ObjectName)=name then begin
      app := FAppArray[i];
      exit(true);
    end;
  end;
  result := false;
end;

function TFRE_DB.GetSysEnum(name: TFRE_DB_String; out enum: TFRE_DB_Enum): boolean;
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

function TFRE_DB.GetSysClientFieldValidator(name: TFRE_DB_String; out clf: TFRE_DB_ClientFieldValidator): boolean;
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
begin
  for i:=0 to high(FClassArray) do begin
    if uppercase(FClassArray[i].ClassName)=uppercase(ObClass.ClassName) then exit; // already registerd;
  end;
  setlength(FClassArray,length(FClassArray)+1);
  FClassArray[high(FClassArray)] :=ObClass;
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


procedure TFRE_DB.DBInitializeAllExClasses(const conn: IFRE_DB_SYS_CONNECTION);
var i:integer;
begin
  for i:=0 to high(FExClassArray) do begin
    try
      FExClassArray[i].exclass.InstallDBObjects(conn);
    except on e:exception do begin
     GFRE_BT.CriticalAbort('DB INITIALIZATION OF EXCLASS SCHEME: [%s] FAILED DUE TO [%s]',[FExClassArray[i].exclass.ClassName,e.Message]);
    end;end;
   end;
end;

procedure TFRE_DB.DBInitializeAllSystemClasses(const conn: IFRE_DB_SYS_CONNECTION);
begin
  abort;
end;

procedure TFRE_DB.Initialize_System_Objects;
var i       : integer;
    app     : TFRE_DB_APPLICATION;
    cn      : string;
    lscheme : TFRE_DB_SchemeObject;
begin
  for i:=0 to high(FClassArray) do begin
    try
      cn      := FClassArray[i].ClassName;
      lscheme := NewScheme(cn,dbst_System);
      if cn='TFRE_DB_USER' then
        cn := cn;
      FClassArray[i].RegisterSystemScheme(lscheme);
      //AddSystemObjectToSysList(lscheme);
      //writeln('>>>>>  SCHEME STORE ',cn);
      CheckDbResultFmt(FSystemSchemes.StoreScheme(lscheme),'error storing extension lscheme %s',[cn]);
      //writeln('<<<<< SCHEME STORE ',cn,' DONE');
    except on e:exception do begin
      GFRE_BT.CriticalAbort('INITIALIZATION OF SYSCLASS: [%s] FAILED DUE TO [%s]',[FClassArray[i].ClassName,e.Message]);
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
   if uppercase(FClassArray[i].ClassName)=ccn then begin
     result := FClassArray[i];
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

function TFRE_DB.ExistsObjectClass(const ClName: ShortString): boolean;
var i:integer;
begin
 for i:=0 to high(FClassArray) do begin
   if uppercase(FClassArray[i].ClassName)=uppercase(ClName) then begin
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
  result := TFRE_DB_CONNECTION.Create;
end;

function TFRE_DB.NewDirectSysConnection: TFRE_DB_SYSTEM_CONNECTION;
begin
 GFRE_DB_Init_Check;
 result := TFRE_DB_SYSTEM_CONNECTION.Create;
 //result.FPersistance_Layer  := GFRE_DB_DEFAULT_PS_LAYER.Clone('SYSTEM'); // Layer per instance
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
    result[i] := GFRE_BT.HexString_2_GUID(a[i]);
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
    if i<len then result := result+''''+gfre_bt.GUID_2_HexString(a[i])+''',' else result := result+''''+gfre_bt.GUID_2_HexString(a[i])+''' ';
  end;
  result:=result+']';
end;

function TFRE_DB.CountedObjLinks2String(const A: TFRE_DB_CountedGuidArray): TFRE_DB_String;
var i,len:integer;
begin
  result:='[ ';
  len := Length(a)-1;
  for i := 0 to len do begin
    if i<len then result := result+''''+gfre_bt.GUID_2_HexString(a[i].link)+':'+inttostr(a[i].count)+''',' else result := result+''''+gfre_bt.GUID_2_HexString(a[i].link)+':'+inttostr(a[i].count)+''' ';
  end;
  result:=result+']';
end;

function TFRE_DB.ConstructGuidArray(const A: array of TGuid): TFRE_DB_GUIDArray;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.ConstructByteArray(const A: array of Byte): TFRE_DB_ByteArray;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.ConstructInt16Array(const A: array of Int16): TFRE_DB_Int16Array;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.ConstructInt32Array(const A: array of Int32): TFRE_DB_Int32Array;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.ConstructInt64Array(const A: array of Int64): TFRE_DB_Int64Array;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.ConstructUInt16Array(const A: array of UInt16): TFRE_DB_UInt16Array;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.ConstructUInt32Array(const A: array of UInt32): TFRE_DB_UInt32Array;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.ConstructUInt64Array(const A: array of UInt64): TFRE_DB_UInt64Array;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.ConstructReal32Array(const A: array of Single): TFRE_DB_Real32Array;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.ConstructReal64Array(const A: array of Double): TFRE_DB_Real64Array;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.ConstructBooleanArray(const A: array of Boolean): TFRE_DB_BoolArray;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.ConstructDateTimeArray(const A: array of TFRE_DB_DateTime64): TFRE_DB_DateTimeArray;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.ConstructStreamArray(const A: array of TFRE_DB_Stream): TFRE_DB_StreamArray;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.ConstructObjectArray(const A: array of TFRE_DB_Object): TFRE_DB_ObjectArray;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.TranslateLong(const txt: TFRE_DB_TEXT): TFRE_DB_String;
begin
  result := txt.LongText;  // TODO Lookup Translationkey and Translate
end;

function TFRE_DB.GetSystemSchemeColl: TFRE_DB_SCHEME_COLLECTION;
begin
  result := FSystemSchemes;
end;

function TFRE_DB.GetSystemScheme(const schemename: TFRE_DB_String; var scheme: TFRE_DB_SchemeObject): Boolean;
begin
  result := FSystemSchemes.GetScheme(schemename,scheme);
end;

procedure TFRE_DB.ForAllSchemes(const iterator: TFRE_DB_Scheme_Iterator);

  procedure DoLocal(const sch:TFRE_DB_Object);
  begin
    iterator(sch as TFRE_DB_SchemeObject);
  end;

begin
  FSystemSchemes.ForAll(@DoLocal);
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

procedure TFRE_DB.LogInfo(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String; const param: array of const);
begin
  _InternalLog(0,fll_Info,category,msg,param);
end;

procedure TFRE_DB.LogWarning(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String; const param: array of const);
begin
 _InternalLog(1,fll_Warning,category,msg,param);
end;

procedure TFRE_DB.LogError(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String; const param: array of const);
begin
 _InternalLog(2,fll_Error,category,msg,param);
end;

procedure TFRE_DB.LogDebug(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String; const param: array of const);
begin
 _InternalLog(3,fll_Debug,category,msg,param);
end;

procedure TFRE_DB.LogNotice(const category: TFRE_DB_LOGCATEGORY; const msg: TFRE_DB_String; const param: array of const);
begin
 _InternalLog(4,fll_Notice,category,msg,param);
end;


procedure TFRE_DB.ClearGUID(var uid: TGUID);
begin
  FillByte(uid,sizeof(uid),0);
end;

function TFRE_DB.ConstructCurrencyArray(const A: array of Currency): TFRE_DB_CurrencyArray;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;

function TFRE_DB.ConstructStringArray(const A: array of TFRE_DB_String): TFRE_DB_StringArray;
var i,j:integer;
begin
  SetLength(result,Length(a));
  j:=0;
  for i:=low(a) to high(a) do begin
    result[j] := a[i];
    inc(j);
  end;
end;


{ TFRE_DB_Object }

function TFRE_DB_Object._StreamingSize: TFRE_DB_SIZE_TYPE;
var size:int64;
    clname:shortstring;

  procedure CountFieldStreamingSize(const Field:TFRE_DB_FIELD);
  begin
    size:=size+Field._StreamingSize;
  end;

begin
  //if FStreamingSize<>-1 then begin
  //  exit(FStreamingSize);
  //end;
  //size           := _ExtraStreamOnlyFieldSize;
  size := 0;
  ForAll(@CountFieldStreamingSize);
  clname := ClassName;
  if assigned(FMediatorExtention) then begin
    clname := FMediatorExtention.ClassName;
  end;
  result         := CFRE_DB_SIZE_ENCODING_SIZE+length(clname)+CFRE_DB_SIZE_ENCODING_SIZE+length(FSchemeName) + size;
  //FStreamingSize := result;
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


//function TFRE_DB_Object.Supports(const InterfaceSpec: ShortString; out Intf): boolean;
//begin
//  result := Sysutils.Supports(self,InterfaceSpec,Intf);
//  if (not result) and assigned(FMediatorExtention) then begin
//    result := Sysutils.Supports(FMediatorExtention,InterfaceSpec,Intf);
//  end;
//end;
//
//function TFRE_DB_Object.Supports(const InterfaceSpec: ShortString): boolean;
//begin
// result := Sysutils.Supports(self,InterfaceSpec);
//end;

//procedure TFRE_DB_Object.IntfCast(const InterfaceSpec: ShortString; out Intf);
//begin
//  if not self.Supports(InterfaceSpec,Intf) then begin
//    raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'INTERFACE UPCAST FAILED FOR CLASS [%s] TO [%s]',[ClassName,InterfaceSpec]);
//  end;
//end;

function TFRE_DB_Object.Invoke(const method: TFRE_DB_String; const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  if assigned(FMediatorExtention) then begin
    result := FMediatorExtention.Invoke(method,input);
  end else begin
    result := Invoke_DBIMI_Method(method,input);
  end;
end;

procedure TFRE_DB_Object.Finalize;
begin
  if self=nil then
    exit;
  _InAccessibleCheck;
  Free;
end;

procedure TFRE_DB_Object.RemoveAllRefLinks;
var rl : TFRE_DB_ObjectReferences;
     i : NativeInt;
begin
  rl := ReferencesFromData;
  for i := 0 to high(rl) do
    DeleteField(rl[i].fieldname);
end;


function TFRE_DB_Object.InternalUniqueDebugKey: String;
begin
  WriteStr(result,'(',FREDB_ObjectToPtrUInt(self),'-',ClassName,'[',GFRE_BT.GUID_2_HexString(FUID),' ',BoolToStr(assigned(Parent),'C','R'),')');
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
    DebugCheck;
    if assigned(FMediatorExtention) then begin
      if FMediatorExtention.ClassName='TFRE_DB_NIL_DESC' then begin
        //writeln('SKIPPING NIL DESCRIPTION :::: FREE_REAL & MEDIATOR EXTENSION: ',ClassName,'  ',FMediatorExtention.ClassName);
        exit;
      end;
      //writeln('FREE_REAL & MEDIATOR EXTENSION: ',ClassName,'  ',FMediatorExtention.ClassName);
      FMediatorExtention.Free;
      FMediatorExtention:=nil;
    end else begin
      //writeln('FREE_REAL : ',ClassName,' ',InternalUniqueDebugKey);
      //writeln('---');
      //writeln(DumpToString());
      //writeln('---');
    end;
    inherited Free;
  end;
end;

procedure TFRE_DB_Object.ForAllFields(const iter: TFRE_DB_FieldIterator);
begin
 _InAccessibleCheck;
  ForAll(iter);
end;

procedure TFRE_DB_Object.ForAllFieldsBreak(const iter: TFRE_DB_FieldIteratorBrk);
begin
 _InAccessibleCheck;
  ForAllBrk(iter);
end;

procedure TFRE_DB_Object.ForAllFields(const iter: IFRE_DB_FieldIterator);

  procedure lForAll(const field:TFRE_DB_FIELD);
  begin
    iter(field);
  end;

begin
  _InAccessibleCheck;
   ForAll(@lForAll);
end;

procedure TFRE_DB_Object.ForAllFieldsBreak(const iter: IFRE_DB_FieldIteratorBrk);
  function lForAll(const field:TFRE_DB_FIELD):boolean;
  begin
    result := iter(field);
  end;
begin
  _InAccessibleCheck;
  ForAllBrk(@lForAll);
end;

function TFRE_DB_Object.ForAllObjectsBreak(const iter: TFRE_DB_ObjectIteratorBrk; const with_subobjects: boolean): boolean;
var halt : boolean;

  procedure IterateWithSub(const obj : TFRE_DB_Object);

    function IterateField(const fld : TFRE_DB_FIELD):boolean;
    var i : NativeInt;
    begin
      if halt then
        exit(true);
      if fld.IsObjectField then
        for i:=0 to fld.ValueCount-1 do
          IterateWithSub(Fld.AsObjectArr[i]);
      result := false;
    end;
  begin
    if halt then
      exit;
    iter(obj,halt);
    if not halt then
      obj.ForAllBrk(@IterateField);
  end;

  procedure IterateFirstLevel;
    //function IterateFieldObjs(const fld : TFRE_DB_FIELD):boolean;
    //var i : NativeInt;
    //begin
    //  if halt then
    //    exit(true);
    //  if fld.IsObjectField then
    //    for i:=0 to fld.ValueCount-1 do
    //      begin
    //        halt   := Iter(Fld.AsObjectArr[i]);
    //        result := halt;
    //      end;
    //end;
  begin
    abort; // untested
    //if halt then
    //  exit;
    //halt := iter(self);
    //if not halt then
    //  ForAllBrk(@IterateFieldObjs);
  end;

begin
  halt := false;
  if with_subobjects then
    IterateWithSub(self)
  else
    IterateFirstLevel;
  result := halt;
end;

function TFRE_DB_Object.GetScheme: TFRE_DB_SchemeObject;
begin
  if FSchemeName='' then exit(nil);
  if not assigned(FCacheSchemeObj) then begin
    if not GFRE_DB.GetSystemScheme(FSchemeName,TFRE_DB_SchemeObject(FCacheSchemeObj)) then begin
      _DBConnectionBC.GetScheme(FSchemeName,TFRE_DB_SchemeObject(FCacheSchemeObj));
    end;
  end;
  result := TFRE_DB_SchemeObject(FCacheSchemeObj);
end;

function TFRE_DB_Object.GetSchemeI: IFRE_DB_SchemeObject;
begin
  result := GetScheme;
end;

function TFRE_DB_Object.GetFormattedDisplay: TFRE_DB_String;
var so:TFRE_DB_SchemeObject;
begin
  _InAccessibleCheck;
  so := GetScheme;
  if assigned(so) then begin
    result := so.GetFormattedDisplay(self);
  end else
  if self is TFRE_DB_SchemeObject then begin
    result := TFRE_DB_SchemeObject(self).GetFormattedDisplay(Self);
  end else  begin
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
var so:TFRE_DB_SchemeObject;
begin
  _InAccessibleCheck;
  so := GetScheme;// TFRE_DB_SchemeObject(FSchemeObj);
  if assigned(so) then begin
    result := so.FormattedDisplayAvailable(self);
  end else
  if self is TFRE_DB_SchemeObject then begin
    result := TFRE_DB_SchemeObject(self).FormattedDisplayAvailable(Self);
  end else  begin
    result := false;
  end;
end;


function TFRE_DB_Object.UID: TGUID;
begin
  result := FUID;
end;

function TFRE_DB_Object.UID_String: TFRE_DB_String;
begin
  result := GFRE_BT.GUID_2_HexString(UID);
end;

function TFRE_DB_Object.UIDP: PByte;
begin
  result := @FUID;
end;


constructor TFRE_DB_Object.Create;
begin
  inherited Create;
  FDBO_State           := fdbos_Creating;
  FObjectProps         := [];
  FFieldStore          := _TFRE_DB_FieldTree.Create(@FREDB_DBNameType_Compare);
  FUID                 := GFRE_DB.Get_A_GUID;
  _Field('UID').AsGUID := FUID;
  InternalSetup;
  assert(FDBO_State=fdbos_Creating);
  FDBO_State:=fdbos_Dirty;
end;

constructor TFRE_DB_Object.CreateStreaming(const conn: TFRE_DB_BASE_CONNECTION; const ExtensionObjectMediatorClass: TFRE_DB_OBJECTCLASSEX);
begin
  inherited Create;
  FDBO_State             := fdbos_StreamingCreating;
  FFieldStore            := _TFRE_DB_FieldTree.Create(@FREDB_DBNameType_Compare);
  FManageInfo            := conn;
  if assigned(ExtensionObjectMediatorClass) then begin
    FMediatorExtention   := ExtensionObjectMediatorClass.CreateBound(Self);
  end;
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
   _InAccessibleCheck;
   Result := _ObjectRoot;
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
  _InAccessibleCheck;
  result := FParentDBO;
end;

function TFRE_DB_Object.ParentFieldI: IFRE_DB_FIELD;
begin
  result := ParentField;
end;

procedure TFRE_DB_Object.CopyToMemory(memory: Pointer; const without_schemes: boolean);
var   sz_field         : TFRE_DB_SIZE_TYPE;
      old_mem_location : pointer;
begin
  _InAccessibleCheck;
  old_mem_location := memory;
  Move(CFRE_DB_ObjectHdr,memory^,Sizeof(CFRE_DB_ObjectHdr));           //     HEADER
  Inc(PByte(memory),Sizeof(CFRE_DB_ObjectHdr));                        //     HEADER

  sz_field := TFRE_DB_SIZE_TYPE(FieldCount(true));                     //     FIELDCOUNT
  Move     (sz_field,memory^,CFRE_DB_SIZE_ENCODING_SIZE);              // 1 x FIELDCOUNT
  inc      (memory,CFRE_DB_SIZE_ENCODING_SIZE);                        //     FIELDCOUNT

  CopyToMem(memory,without_schemes);                                                   //     INTERNAL
  if (memory-old_mem_location) <> NeededSize then GFRE_BT.CriticalAbort('internal streaming miscalculation save actual=%d <> calced=%d ',[memory-old_mem_location,NeededSize]);
end;



class function TFRE_DB_Object.CreateFromMemory(memory: Pointer; const conn: TFRE_DB_BASE_CONNECTION; const recreate_weak_schemes: boolean; const generate_new_uids: boolean): TFRE_DB_Object;
var hdr:TFRE_DB_ObjectHdr;
begin
  Move(Memory^,hdr,Sizeof(CFRE_DB_ObjectHdr));
  if CompareByte(hdr,CFRE_DB_ObjectHdr,SizeOf(CFRE_DB_ObjectHdr))<>0 then raise EFRE_DB_Exception.Create(edb_INTERNAL,'HEADER SIGNATURE BAD');
  Inc(PByte(memory),Sizeof(CFRE_DB_ObjectHdr)); //TODO: Implement Headercheck in persistence layer
  result := CreateInternalStreaming(nil,memory,conn,recreate_weak_schemes,generate_new_uids);
end;

//function TFRE_DB_Object.IsDirty: boolean;
//var txt:string;
//begin
//  result := FDBO_State=fdbos_Dirty;
//  if not ((FDBO_State=fdbos_Clean) or (FDBO_State=fdbos_Dirty)) then begin
//     WriteStr(txt,FDBO_State);
//     gfre_bt.CriticalAbort('Object State should be clean or dirty but is ['+txt+'] class '+ClassName);
//  end;
//end;

function TFRE_DB_Object.FieldI(const name: TFRE_DB_String): IFRE_DB_FIELD;
begin
  result := Field(name);
end;


procedure TFRE_DB_Object.ForAll(const iter: TFRE_DB_FieldIterator);
var scheme_object:TFRE_DB_SchemeObject;
   procedure Iterate(const db:TFRE_DB_FIELD);
   begin
     if db.FieldType<>fdbft_NotFound then begin
       iter(db);
     end;
   end;
begin
  _InAccessibleCheck;
  FFieldStore.ForAllItems(@Iterate);
  scheme_object := GetScheme;
  if assigned(GetScheme) then begin
    scheme_object.ForAllCalculatedFields(self,@Iterate);
  end;
end;

procedure TFRE_DB_Object.ForAllBrk(const iter: TFRE_DB_FieldIteratorBrk);
var scheme_object:TFRE_DB_SchemeObject;
   function Iterate(const db:TFRE_DB_FIELD):boolean;
   begin
     if db.FieldType<>fdbft_NotFound then begin
       result := iter(db);
     end;
   end;
begin
  _InAccessibleCheck;
  FFieldStore.ForAllItemsBrk(@Iterate);
  scheme_object := GetScheme;
  if assigned(GetScheme) then begin
    scheme_object.ForAllCalculatedFieldsBrk(self,@Iterate);
  end;
end;

function TFRE_DB_Object.AsString(const without_schemes: boolean): TFRE_DB_String;
begin
  _InAccessibleCheck;
  SetLength(Result,NeededSize);
  CopyToMemory(@result[1],without_schemes);
end;

class function TFRE_DB_Object.CreateFromString(const AValue: TFRE_DB_String; const conn: TFRE_DB_BASE_CONNECTION; const recreate_weak_schemes: boolean; const generate_new_uids: boolean): TFRE_DB_Object;
begin
  result:=CreateFromMemory(@AValue[1],conn,recreate_weak_schemes,generate_new_uids);
end;

class function TFRE_DB_Object.CreateFromJSONString(const AValue: TFRE_DB_String; const conn: TFRE_DB_BASE_CONNECTION; const recreate_weak_schemes: boolean; const stream_cb: TFRE_DB_StreamingCallback): TFRE_DB_Object;
var jd : TJSONData;
    jp : TJSONParser;
begin
  try
    jp     := TJSONParser.Create(AValue);
    jd     := jp.Parse;
    result := CreateInternalStreamingJSON(nil,jd as TJSONArray,conn,recreate_weak_schemes,stream_cb);
  finally
    jp.free;
    jd.free;
  end;
end;

function TFRE_DB_Object._Field(name: TFRE_DB_String): TFRE_DB_FIELD;
var lfield   : TFRE_DB_Field;
    keyp     : PFRE_DB_NameType;
    storep   : ^TFRE_DB_Field;
    lcalcfld : TFRE_DB_FIELD;
begin
  name := UpperCase(name);
  if Pos('.',name)>0 then raise EFRE_DB_Exception.Create(' "." character is not allowed in fieldnames');
  result:=nil;
  SchemeFieldAccessCheck(name);
  if CalcFieldExists(name,lcalcfld) then begin
    result := lcalcfld;
    exit;
  end else
  if FFieldStore.Find(name,lfield) then begin
    result := lfield;
  end else begin
    lfield  := TFRE_DB_Field.Create(self,fdbft_NotFound);
    if not FFieldStore.AddCheckEx(name,lfield,false,keyp,storep) then raise EFRE_DB_Exception.create(edb_INTERNAL,'fieldstore');
    lfield.FFieldname := keyp;
    if name='UID' then
      lfield.FIsUidField:= true;
    result := lfield;
  end;
end;

function TFRE_DB_Object._FieldOnlyExisting(name: TFRE_DB_String): TFRE_DB_FIELD;
var lfield : TFRE_DB_Field;
begin
  result:=nil;
  name := UpperCase(name);
  if CalcFieldExists(name,result) then begin
    exit;
  end else
  if FFieldStore.Find(name,lfield) then begin
    if lfield.FieldType<>fdbft_NotFound then begin
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

function TFRE_DB_Object._DBConnectionBC(const fail_on_no_connection:boolean): TFRE_DB_BASE_CONNECTION;
begin
 if IsObjectRoot then
   begin
      result := FManageInfo;
      if fail_on_no_connection
         and not assigned(FManageInfo) then
           raise EFRE_DB_Exception.Create(edb_ERROR,'operation is only valid on objects with a connection reference');
   end
 else
   result := ObjectRoot._DBConnection(fail_on_no_connection);
end;

function TFRE_DB_Object._DBConnection(const fail_on_no_connection:boolean): TFRE_DB_CONNECTION;
begin
  result := _DBConnectionBC(fail_on_no_connection) as TFRE_DB_CONNECTION;
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

procedure TFRE_DB_Object.CheckMediatorSetup;
var lScheme:TFRE_DB_SchemeObject;
begin
  lScheme := GetScheme;
  if not assigned(lScheme) then exit;
  if lScheme.HasHardCodeClass and (SchemeClass<>ClassName) then begin
    lScheme.SetupMediator(self);
  end;
end;

procedure TFRE_DB_Object._InAccessibleCheck;
begin
  if assigned(FParentDBO) then
    _ObjectRoot._InAccessibleCheck
  else
    if fop_STORED_IMMUTABLE in FObjectProps then
      raise EFRE_DB_Exception.Create(edb_ERROR,'this object is stored, illegal modification : '+InternalUniqueDebugKey);
end;

procedure TFRE_DB_Object.__InternalCollectionAdd(const coll: IFRE_DB_PERSISTANCE_COLLECTION);
begin
  if __InternalCollectionExists(coll) then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'try internal add object [%s] to collection [%s], but its already existing',[self.InternalUniqueDebugKey,coll.CollectionName]);
  SetLength(FInCollectionarr,Length(FInCollectionarr)+1);
  FInCollectionarr[High(FInCollectionarr)] := coll;
end;

function TFRE_DB_Object.__InternalCollectionExists(const coll: IFRE_DB_PERSISTANCE_COLLECTION): boolean;
var i : NativeInt;
begin
  for i := 0 to high(FInCollectionarr) do
    if coll.CollectionName(true) = FInCollectionarr[i].CollectionName(true) then
      exit(true);
  result:=false;
end;

function TFRE_DB_Object.__InternalGetCollectionList: IFRE_DB_PERSISTANCE_COLLECTION_ARRAY;
begin
  result := FInCollectionarr;
end;

procedure TFRE_DB_Object.__InternalGetFullObjectList(var list: OFRE_SL_TFRE_DB_Object);

  procedure BuildList(const obj : TFRE_DB_Object ; var halt : boolean);
  begin
    list.Add(obj);
  end;

begin
  ForAllObjectsBreak(@BuildList);
end;

procedure TFRE_DB_Object.__InternalCompareToobj(const compare_obj: TFRE_DB_Object; callback: TFRE_DB_ObjCompareCallback);

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
      callback(self,cev_FieldDeleted,fld,nil)
  end;


begin
  ForAllFields(@BaseFields);
  compare_obj.ForAllFields(@CompareFields);
end;

procedure TFRE_DB_Object.__InternalClearManageInfo;
begin
  FManageInfo := nil;
end;

procedure TFRE_DB_Object.Set_ReadOnly;
begin
  Include(FObjectProps,fop_READ_ONLY);
end;

procedure TFRE_DB_Object.Set_Volatile;
begin
  Include(FObjectProps,fop_VOLATILE);
end;

procedure TFRE_DB_Object.Set_System;
begin
  Include(FObjectProps,fop_SYSTEM);
end;

procedure TFRE_DB_Object.Set_Store_Locked(const locked: boolean);
begin
  //if locked then
  //  writeln('CALL OBJ LOCK : +LOCK   ',  GFRE_BT.GUID_2_HexString(FUID),'  ',ClassName,' ',PtrUInt(self))
  //else
  //  begin
  //    if (ClassName='TFRE_DB_GROUP')
  //       and (TFRE_DB_USER(self)._Field('objname')._ConvertToString='SYSUG_ADMIN_USERS') then
  //         begin
  //           writeln('--------------------************************____');
  //         end;
  //    writeln('CALL OBJ LOCK : -UNLOCK ',  GFRE_BT.GUID_2_HexString(FUID),'  ',ClassName,' ',PtrUInt(self));
  //  end;
  if locked then
    Include(FObjectProps,fop_STORED_IMMUTABLE)
  else
    Exclude(FObjectProps,fop_STORED_IMMUTABLE);
end;

procedure TFRE_DB_Object.Assert_CheckStoreLocked;
begin
  if assigned(FParentDBO) then
    _ObjectRoot.Assert_CheckStoreLocked
  else
    if not (fop_STORED_IMMUTABLE in FObjectProps) then
      raise EFRE_DB_Exception.Create(edb_INTERNAL,'FAILURE NOT STORELOCKED : '+InternalUniqueDebugKey);
end;

function TFRE_DB_Object.Implementor: TObject;
begin
  result := self;
end;


procedure TFRE_DB_Object.SetScheme(const scheme_obj: TFRE_DB_SchemeObject);
begin
  _InAccessibleCheck;
  if assigned(scheme_obj) and (scheme_obj is TFRE_DB_SchemeObject) then begin
    FSchemeName     := scheme_obj.DefinedSchemeName;
    FCacheSchemeObj := scheme_obj;
  end;
end;


function TFRE_DB_Object.CalcFieldExists(const name: TFRE_DB_String; var calculated_field: TFRE_DB_FIELD): boolean;
var scheme:TFRE_DB_SchemeObject;
begin
  result:=false;
  if name<>'UID' then begin
    scheme:=GetScheme;
    if assigned(scheme) then begin
      result:=scheme.CalcFieldExists(self,name,calculated_field);
    end;
  end;
end;

procedure TFRE_DB_Object.InternalSetup;
begin

end;

procedure TFRE_DB_Object.SchemeFieldAccessCheck(const name: TFRE_DB_String);
var scheme:TFRE_DB_SchemeObject;
begin
//  if FSchemeName<>'' then begin
    if name<>'UID' then begin
      scheme:=GetScheme;
      if assigned(scheme) then begin
        scheme.FieldAccessCheck(name);
      end else begin
        //abort;// HMM System DB load WAC case
      end;
    end;
//  end;
end;


procedure TFRE_DB_Object.InternalFinalize;
begin

end;

procedure TFRE_DB_Object.CopyToMem(var mempointer: Pointer;const without_schemes:boolean);
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
    res  := Field.CopyFieldToMem(mempointer,without_schemes);
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

  clname := ClassName;

  if assigned(FMediatorExtention) then begin
    clname := FMediatorExtention.ClassName;
  end;

  size := length(clname);
  Move (size,mempointer^,CFRE_DB_SIZE_ENCODING_SIZE);
  inc  (mempointer,CFRE_DB_SIZE_ENCODING_SIZE);
  Move (clname[1],mempointer^,size);
  inc  (mempointer,size);

  size := length(FSchemeName);
  if not without_schemes then begin
    Move (size,mempointer^,CFRE_DB_SIZE_ENCODING_SIZE);
  end else begin
    neg_size := -1 * size;
    Move (neg_size,mempointer^,CFRE_DB_SIZE_ENCODING_SIZE);
  end;
  inc  (mempointer,CFRE_DB_SIZE_ENCODING_SIZE);
  Move (FSchemeName[1],mempointer^,size);
  inc  (mempointer,size);


  ForAll(@LocalCopyToMem);
  //writeln('<--SZ  ',FStreamingSize);
  AfterSave;
  res := mempointer-oldp;
  if not without_schemes then begin
    //if res <> (FStreamingSize + CFRE_DB_SIZE_ENCODING_SIZE ) then begin
    if res <> (_StreamingSize + CFRE_DB_SIZE_ENCODING_SIZE ) then begin
      writeln('STREAMING FISHHHH');
      writeln('---------');
      writeln(DumpToString());
      GFRE_BT.CriticalAbort(Format('INTERNAL STREAMING (B)  Miscalculation Object - %d <> %d',[res,_StreamingSize]));
    end;
  end else begin
    ;
  end;
end;

procedure TFRE_DB_Object.CopyFromMem(var mempointer: Pointer; const field_count: TFRE_DB_SIZE_TYPE; const conn: TFRE_DB_BASE_CONNECTION; const recreate_weak_schemes: boolean; const generate_new_uids: boolean);
var i:Integer;
    F:TFRE_DB_FIELD;
    field_type  : TFRE_DB_FIELDTYPE;
    value_count : TFRE_DB_SIZE_TYPE;
    field_name  : TFRE_DB_NameType;
begin
  if field_count>0 then begin
    for i := 0 to field_count-1 do begin
      TFRE_DB_FIELD.__ReadHeader(mempointer,field_name);
      _Field(field_name).CopyFieldFromMem(mempointer,conn,recreate_weak_schemes,generate_new_uids);
      if field_name='UID' then
        begin
          if generate_new_uids then
            _Field('UID').AsGUID := GFRE_DB.Get_A_Guid;
          FUID := _Field('UID').AsGUID;
        end;
    end;
  end;
end;

procedure TFRE_DB_Object.CopyFromJSON(const JSON: TJSONArray; const field_count: TFRE_DB_SIZE_TYPE; const conn: TFRE_DB_BASE_CONNECTION; const recreate_weak_schemes: boolean;const stream_cb:TFRE_DB_StreamingCallback=nil);
var i:Integer;
    F:TFRE_DB_FIELD;
    field_type  : TFRE_DB_FIELDTYPE;
    value_count : TFRE_DB_SIZE_TYPE;
    field_name  : TFRE_DB_NameType;
    jo          : TJSONObject;
begin
  if field_count>0 then begin
    for i := 0 to field_count-1 do begin
      jo         := JSON.Items[i+2] as TJSONObject;
      field_name := jo.Elements['N'].AsString;
      field_type := FieldtypeShortString2Fieldtype(jo.Elements['T'].AsString);
      _Field(field_name).SetFromJSON(field_type,jo.Elements['D'] as TJSONArray,stream_cb,conn);
    end;
  end;
end;

function TFRE_DB_Object.CopyToJSON: TFRE_DB_String;
begin
  GetAsJSONString(false,true,nil);
end;

class function TFRE_DB_Object.CreateInternalStreaming(const parent: TFRE_DB_FIELD; var mempointer: Pointer; const conn: TFRE_DB_BASE_CONNECTION; const recreate_weak_schemes: boolean; const generate_new_uids: boolean): TFRE_DB_Object;
var   lClassnameLen,lFieldcount : TFRE_DB_SIZE_TYPE;
      lStreamingSize            : TFRE_DB_SIZE_TYPE;
      lClassname                : ShortString;
      lschemename               : TFRE_DB_String;
      lschemenamelen            : TFRE_DB_SIZE_TYPE;
      test                      : pointer;
begin
  test := mempointer;
  Move (mempointer^,lFieldcount,CFRE_DB_SIZE_ENCODING_SIZE);inc(mempointer,CFRE_DB_SIZE_ENCODING_SIZE);

  Move (mempointer^,lStreamingSize,CFRE_DB_SIZE_ENCODING_SIZE);inc(mempointer,CFRE_DB_SIZE_ENCODING_SIZE);

  Move (mempointer^,lClassnameLen,CFRE_DB_SIZE_ENCODING_SIZE);inc(mempointer,CFRE_DB_SIZE_ENCODING_SIZE);
  SetLength (lClassname,lClassnameLen);
  Move (mempointer^,lClassname[1],lClassnameLen);inc(mempointer,lClassnameLen);

  Move (mempointer^,lschemenamelen,CFRE_DB_SIZE_ENCODING_SIZE);inc(mempointer,CFRE_DB_SIZE_ENCODING_SIZE);
  if lschemenamelen>=0 then begin
    SetLength (lschemename,lschemenamelen);
    Move (mempointer^,lschemename[1],lschemenamelen);inc(mempointer,lschemenamelen);
  end else begin
    if not recreate_weak_schemes then begin
      inc(mempointer, -1 * lschemenamelen);
      lschemename:='';
    end else begin
      lschemenamelen := -1 * lschemenamelen;
      SetLength (lschemename,lschemenamelen);
      Move (mempointer^,lschemename[1],lschemenamelen);inc(mempointer,lschemenamelen);
    end;
  end;

  result := GFRE_DB.NewObjectStreaming(lClassname,conn);
  result.FParentDBO := parent;
  result.FSchemeName:=lschemename;
  result.CopyFromMem(mempointer,lFieldcount,conn,recreate_weak_schemes,generate_new_uids);
  result.InternalSetup;
  result.AfterLoad;
  //result.FStreamingSize:=lStreamingSize;
  assert(result.FDBO_State=fdbos_StreamingCreating);
  result.FDBO_State:=fdbos_Clean;
  //DBGSTREAM   writeln('LOAD : ',mempointer-test);
end;

class function TFRE_DB_Object.CreateInternalStreamingJSON(const parent: TFRE_DB_FIELD; const JSON: TJSONArray; const conn: TFRE_DB_BASE_CONNECTION; const recreate_weak_schemes: boolean;const stream_cb:TFRE_DB_StreamingCallback=nil): TFRE_DB_Object;
var   lClassname                : ShortString;
      lschemename               : TFRE_DB_String;
      lFieldCount               : Integer;
begin
  lClassname  := (JSON.Items[0] as TJSONString).AsString;
  lschemename := (JSON.Items[1] as TJSONString).AsString;
  lFieldCount := JSON.Count-2;
  result      := GFRE_DB.NewObjectStreaming(lClassname,conn);
  result.FParentDBO := parent;
  result.FSchemeName:= lSchemeName;
  result.CheckMediatorSetup;
  result.CopyFromJSON(JSON,lFieldCount,conn,recreate_weak_schemes,stream_cb);
  result.InternalSetup;
  result.AfterLoad;
  //result.FStreamingSize:=-1; // Force recalc of streamingsize
  //result.FStreamingSize:= result._StreamingSize;
  assert(result.FDBO_State=fdbos_StreamingCreating);
  result.FDBO_State:=fdbos_Clean;
end;

function TFRE_DB_Object.Field(const name: TFRE_DB_String): TFRE_DB_FIELD;
begin
  _InAccessibleCheck;
  {$IFDEF SANITY_CHECKS}
    assert((FDBO_State=fdbos_Clean) or (FDBO_State=fdbos_Clean));
  {$ENDIF}
  result := _Field(name);
end;

function TFRE_DB_Object.FieldOnlyExisting(const name: TFRE_DB_String; var fld: TFRE_DB_FIELD): boolean;
begin
  _InAccessibleCheck;
  fld    := _FieldOnlyExisting(name);
  result := assigned(fld);
end;

function TFRE_DB_Object.FieldOnlyExistingObj(const name: TFRE_DB_String): TFRE_DB_Object;
var fld : TFRE_DB_FIELD;
begin
  _InAccessibleCheck;
  result := nil;
  fld := _FieldOnlyExisting(name);
  if assigned(fld) and (fld.FieldType=fdbft_Object) then begin
    result := fld.AsObject;
  end;
end;

function TFRE_DB_Object.FieldOnlyExistingObjI(const name: TFRE_DB_String): IFRE_DB_Object;
begin
  result := FieldOnlyExistingObj(name);
end;

function TFRE_DB_Object.FieldPath(const name: TFRE_DB_String; const dont_raise_ex: boolean): TFRE_DB_FIELD;
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
        if not obj.FieldExists(fp[i]) then exit;
        obj := obj.Field(fp[i]).AsObject;
        if not assigned(obj) then exit;
      end;
      nam := fp[high(fp)];
      if not obj.FieldExists(nam) then exit;
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

function TFRE_DB_Object.FieldPathListFormat(const field_list: TFRE_DB_StringArray; const formats: TFRE_DB_String; const empty_val: TFRE_DB_String): TFRE_DB_String;
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
      if assigned(fld) and (fld.FieldType=fdbft_Object) then begin
        if fld.AsObject.SchemeClass='TFRE_DB_TEXT' then begin
          sa[i] := GFRE_DB.TranslateLong(fld.AsObject as TFRE_DB_TEXT);
          test[i].VAnsiString := PAnsiString(sa[i]);
          test[i].VType       := vtAnsiString;
        end ;
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

function TFRE_DB_Object.FieldCount(const without_calcfields: boolean): SizeInt;
  procedure LocalCount(const F:TFRE_DB_FIELD);
  begin
    if f.FieldType<>fdbft_NotFound then begin
      if without_calcfields and (f.FieldType=fdbft_CalcField) then exit;
      inc(result);
    end;
  end;
begin
  _InAccessibleCheck;
  result:=0;
  ForAll(@LocalCount);
end;

function TFRE_DB_Object.DeleteField(const name: TFRE_DB_String): Boolean;
var lfield:TFRE_DB_FIELD;
begin
  _InAccessibleCheck;
  if uppercase(name)='UID' then raise EFRE_DB_Exception.Create(edb_ACCESS,'UID field cannot be deleted');
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
end;

function TFRE_DB_Object.FieldExists(const name: TFRE_DB_String): boolean;
var l_field : TFRE_DB_FIELD;
begin
  _InAccessibleCheck;
  result := FFieldStore.Find(uppercase(name),l_field);
  if result then begin
    result := l_field.FieldType<>fdbft_NotFound;
  end else begin
    result := CalcFieldExists(uppercase(name),l_field);
  end;
end;

procedure TFRE_DB_Object.StripOwnedObjects;

  procedure StripAll(const F:TFRE_DB_FIELD);
  begin
    if F.FieldType =fdbft_Object then begin
      F.AsObject.StripOwnedObjects;
      F._StripObject;
    end;
  end;

begin
  _InAccessibleCheck;
  ForAllFields(@StripAll);
end;

procedure TFRE_DB_Object.DumpToStrings(const strings: TStrings;indent:integer=0);
var idents,idents2:TFRE_DB_String;

   procedure DumpFieldToString(const Field:TFRE_DB_FIELD);
   var oa : TFRE_DB_ObjectArray;
        i : Integer;
   begin
     if (Field.FieldType<>fdbft_Object) and (Field.FieldType<>fdbft_CalcField) then begin
       strings.Add(idents+Field.AsStringDump);
     end else begin
       oa := Field.AsObjectArr;
       if length(oa)=1 then begin
         strings.Add(Format('%s%s (%s) : ',[idents,Field.FieldName,CFRE_DB_FIELDTYPE[Field.FieldType]]));
       end else begin
         strings.Add(Format('%s%s (%s)[%d] : ',[idents,Field.FieldName,CFRE_DB_FIELDTYPE[Field.FieldType],length(oa)]));
       end;
       idents2:=StringOfChar(' ',indent+2);
       for i:=0 to high(oa) do begin
         strings.Add(idents+'{ ['+inttostr(i)+']');
         oa[i].DumpToStrings(strings,indent+2);
         strings.Add(idents+'}');
       end;
       if Field.FieldType=fdbft_CalcField then begin
         oa[0].free;
       end;
     end;
   end;
begin
  idents := StringOfChar(' ',indent);
  if Assigned(FMediatorExtention) then begin
    strings.Add(idents+'CLASSEX  ['+FMediatorExtention.ClassName+'] ');//+inttostr(NeededSize)+' '+IntToStr(FStreamingSize));
  end else begin
    strings.Add(idents+'CLASS    ['+self.ClassName+'] ');//+inttostr(NeededSize)+' '+IntToStr(FStreamingSize));
  end;
  strings.Add(idents+'SCHEME ['+FSchemeName+'] '+BoolToStr(assigned(FParentDBO),'CHILD','ROOT DBO'));
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
  _InAccessibleCheck;
  result := '';
  if assigned(FMediatorExtention) then begin
    exit(uppercase(FMediatorExtention.ClassName));
  end;
  if FSchemeName<>'' then begin
    result := FSchemeName;
  end else
  if (ClassType<>TFRE_DB_Object) and (ClassName<>'') then begin // if there is a codeclass set, then the schemeclass must have the same name
    result := ClassName;
  end;
end;

function TFRE_DB_Object.IsA(const schemename: TFRE_DB_NameType): Boolean;
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



function file_lock(const  typ:cshort ; const whence:cshort):flock;
const  flock_ret:flock = ();
begin
  result          := flock_ret;
  result.l_type   := typ;
  result.l_start  := 0;
  result.l_whence := whence;
  result.l_len    := 0;
  Result.l_pid    := FpGetpid;
end;


procedure TFRE_DB_Object.SaveToFile(const filename: TFRE_DB_String;const without_schemes:boolean=false);
var m      : TMemoryStream;
    fs     : THandleStream;
    handle : THandle;
    res    : longint;
    x      : flock;

begin
  _InAccessibleCheck;
  m:=TMemoryStream.Create;
  try
    repeat
      handle := FpOpen(pointer(FileName),O_RDWR or O_CREAT); // O_SYNC
      res    := fpgeterrno;
    until (handle<>-1) or (res<>ESysEINTR);
    if (handle<0)  then raise EFRE_DB_Exception.Create(edb_ERROR,'could not save the dbo [%d]',[res]);
    //x := file_lock(F_WRLCK, SEEK_SET);
    //res := FpFcntl(handle,F_SETLKW, x);
    //if res<>0 then raise EFRE_DB_Exception.Create(edb_ERROR,'locking error %d %d',[res,fpgeterrno]);
    fs:=THandleStream.Create(handle);
    m.Size:=NeededSize;
    CopyToMemory(m.Memory,without_schemes);
    m.SaveToStream(fs);
  finally
    m.free;
    fs.Free;
    res := FpClose(handle);
  end;
end;

class function TFRE_DB_Object.CreateFromFile(const filename: TFRE_DB_String;const conn:TFRE_DB_BASE_CONNECTION;const recreate_weak_schemes: boolean):TFRE_DB_Object;
var m        : TMemorystream;
    fs       : THandleStream;
    handle   : Thandle;
    res      : cint;
    //x        : flock;
begin
  m:=TMemoryStream.Create;
  fs:=nil;
  result := nil;
  try
    repeat
      handle := FpOpen(pointer(FileName),O_RDONLY); // O_SYNC
      res    := fpgeterrno;
    until (handle<>-1) or (res<>ESysEINTR);
    if (handle<0) then raise EFRE_DB_Exception.Create(edb_ERROR,'could not find the dbo [%d]',[res]);
    //x := file_lock(F_WRLCK, SEEK_SET);
    //res := FpFcntl(handle,F_SETLKW, x);
    //if res<>0 then raise EFRE_DB_Exception.Create(edb_ERROR,'locking error %d %d',[res,fpgeterrno]);
    fs:=THandleStream.Create(handle);
    m.LoadFromStream(fs);
    if m.Size<Sizeof(CFRE_DB_ObjectHdr) then begin
      raise EFRE_DB_Exception.Create(edb_ERROR,'invalid dbo file '+inttostr(m.size));
    end else begin
      result:= TFRE_DB_Object.CreateFromMemory(m.Memory,conn,recreate_weak_schemes);
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
var s:string;
begin
  _InAccessibleCheck;
  result := TFRE_DB_Object.CreateFromString(self.AsString,_DBConnectionBC(false),false,generate_new_uids);
  if assigned(FMediatorExtention) then begin
    result.FMediatorExtention := TFRE_DB_OBJECTCLASSEX(FMediatorExtention.ClassType).CreateBound(result);
//    writeln('CLONE MEDEX: ',FMediatorExtention.ClassName,' ',result.FMediatorExtention.ClassName,' ',SchemeClass,' ',ClassName);
    result.SetScheme(GetScheme);
  end;
end;

function TFRE_DB_Object.CloneToNewObjectI(const generate_new_uids: boolean): IFRE_DB_Object;
begin
  result := CloneToNewObject;
end;

function TFRE_DB_Object.ReferencesObjects: Boolean;
begin
  _InAccessibleCheck;
  result := _DBConnectionBC(true).ReferenceCount(UID,true)>0;
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

function TFRE_DB_Object.ReferenceListFromData: TFRE_DB_CountedGuidArray;
var search:boolean;

  procedure SearchObjectLinkField(const fld:TFRE_DB_FIELD);
  var i,j,startidx : integer;
      found        : boolean;
      l_OblArr     : TFRE_DB_ObjLinkArray;
  begin
   if fld.FieldType = fdbft_ObjLink then begin
      if fld.ValueCount>0 then begin
        for i:=0 to fld.ValueCount-1 do begin
          found:=false;
          l_OblArr := fld.AsObjectLinkArray;
          for j := 0 to High(result) do begin
            if FREDB_Guids_Same(l_OblArr[i],result[j].link) then begin
              inc(result[j].count);
              found:=true;
              break;
            end;
          end;
          if found then continue;
          SetLength(result,length(result)+1);
          result[high(result)].link  := fld.AsObjectLinkArray[i];
          result[high(result)].count := 1;
        end;
      end;
    end;
  end;

begin
  _InAccessibleCheck;
  SetLength(result,0);
  ForAll(@SearchObjectLinkField);
end;

function TFRE_DB_Object.ReferenceListFromDataNocount: TFRE_DB_GuidArray;
var counted_links : TFRE_DB_CountedGuidArray;
    i             : integer;
begin
  _InAccessibleCheck;
  counted_links := ReferenceListFromData;
  setlength(Result,length(counted_links));
  for i:=0 to high(counted_links) do begin
    result[i] := counted_links[i].link;
  end;
end;

function TFRE_DB_Object.ReferencesFromData: TFRE_DB_ObjectReferences;
var cnt : NativeInt;
  procedure SearchObjectLinkField(const fld:TFRE_DB_FIELD);
  begin
    if fld.FieldType = fdbft_ObjLink then begin
      if fld.ValueCount>0 then begin
        result[cnt].fieldname := uppercase(fld.FieldName);
        result[cnt].linklist  := fld.AsObjectLinkArray;
        inc(cnt);
      end;
    end;
  end;

begin
  _InAccessibleCheck;
  cnt := 0;
  SetLength(result,FieldCount(true));
  ForAll(@SearchObjectLinkField);
  SetLength(result,cnt);
end;

function TFRE_DB_Object.ReferenceList: TFRE_DB_GUIDArray;
var DBC : TFRE_DB_CONNECTION;
begin
  _InAccessibleCheck; // TODO - CHECK THIS IMPLEMENTATION (double entries?)
  if _DBConnection is TFRE_DB_CONNECTION then
    begin
      dbc    := TFRE_DB_CONNECTION(_DBConnection);
      result := FREDB_ObjReferences2GuidArray(dbc.GetReferences(UID,true));
    end;
end;

function TFRE_DB_Object.ReferencesDetailed: TFRE_DB_String;
var search:boolean;

  procedure SearchObjectLinkField(const fld:TFRE_DB_FIELD);
  var i:integer;
      link_obj:TFRE_DB_Object;
  begin
    if fld.FieldType = fdbft_ObjLink then begin
      if fld.ValueCount>0 then begin
        result:=result+' '+fld.FieldName+'->[';
        for i:=0 to fld.ValueCount-1 do begin
          if not _DBConnection.Fetch(fld.AsObjectLinkArray[i],link_obj) then raise EFRE_DB_Exception.Create(edb_INTERNAL,'inconsistency db links');
          result:=result+link_obj.SchemeClass+'('+link_obj.UID_String+')';
          if i<fld.ValueCount-1 then result:=result+' ';
        end;
        result := result+']';
      end;
    end;
  end;

begin
  _InAccessibleCheck;
  ForAll(@SearchObjectLinkField);
end;


function TFRE_DB_Object.IsReferenced: Boolean;
begin
  _InAccessibleCheck;
  result := _DBConnectionBC(true).ReferenceCount(UID,false)>0;
end;

function TFRE_DB_Object.IsSystem: Boolean;
begin
 _InAccessibleCheck;
 result := fop_SYSTEM in FObjectProps;
end;

function TFRE_DB_Object.IsVolatile: Boolean;
begin
 _InAccessibleCheck;
 result := fop_VOLATILE in FObjectProps;
end;

function TFRE_DB_Object.ReferencedByList: TFRE_DB_GUIDArray;
begin
  _InAccessibleCheck;
  result    := FREDB_ObjReferences2GuidArray(_DBConnectionBC(true).GetReferences(UID,false));
end;

function TFRE_DB_Object.ReferencedByList(const from_scheme: TFRE_DB_String): TFRE_DB_GUIDArray;
begin
  _InAccessibleCheck;
  result := ReferencedByList(TFRE_DB_StringArray.Create(from_scheme));
end;

function TFRE_DB_Object.ReferencedByList(const scheme: TFRE_DB_StringArray): TFRE_DB_GUIDArray; // TODO -> Rework References as per Class DataStructure
var list    : TFRE_DB_GUIDArray;
    i,j,cnt : integer;
    DBC     : TFRE_DB_CONNECTION;
    dbo     : TFRE_DB_Object;
begin
  _InAccessibleCheck;
  abort;
  //if _DBConnection is TFRE_DB_CONNECTION then begin
  //  cnt := 0;
  //  dbc := TFRE_DB_CONNECTION(_DBConnection);
  //  list := dbc.GetReferences(UID,false);
  //  SetLength(result,length(list));
  //  for i:=0 to high(list) do begin
  //     dbc.Fetch(list[i],dbo);
  //     for j := 0 to High(scheme) do begin
  //       if dbo.IsA(scheme[j]) then begin
  //         result[cnt]:=list[i];
  //         inc(cnt);
  //         break;
  //       end;
  //     end;
  //  end;
  //  setlength(result,cnt);
  //end;
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
  SetLength(Result,FieldCount(false));
  cnt:=0;
  ForAllFields(@gather);
  SetLength(Result,cnt);
end;

function TFRE_DB_Object.GetUIDPath: TFRE_DB_StringArray;
begin
  _InAccessibleCheck;
  if Length(fuidPath)=0 then begin
    if Assigned(Parent) then begin
      Result:=Parent.GetUIDPath;
      SetLength(Result,Length(Result)+1);
      Result[Length(Result)-1]:=UID_String;
    end else begin
      SetLength(Result,1);
      Result[0]:=UID_String;
    end;
  end else begin
    Result:=fuidPath;
  end;
end;

function TFRE_DB_Object.GetUIDPathUA: TFRE_DB_GUIDArray;
begin
  _InAccessibleCheck;
  if Length(fuidPathUA)=0 then begin
    if Assigned(Parent) then begin
      Result:=Parent.GetUIDPathUA;
      SetLength(Result,Length(Result)+1);
      Result[Length(Result)-1]:=UID;
    end else begin
      SetLength(Result,1);
      Result[0]:=UID;
    end;
  end else begin
    Result:=fuidPathUA;
  end;
end;

function TFRE_DB_Object.GetDBConnection: IFRE_DB_CONNECTION;
begin
  _InAccessibleCheck;
  result := _DBConnection;
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

function TFRE_DB_Object.FetchChildObj(const childuid: TGuid): TFRE_DB_Object;

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
  ForAllObjectsBreak(@SearchChild);
end;

function TFRE_DB_Object.GetAsJSON(const without_uid: boolean;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil): TJSONData;
var ro:TJSONObject;
    ra:TJSONArray;
    i : integer;
  procedure ExportField(const Field:TFRE_DB_FIELD);
  begin
    if without_uid then begin
      if field.FieldName='UID' then exit;
    end;
    ro.Add(lowercase(Field.FieldName),Field.GetAsJSON(without_uid,false,stream_cb));
  end;
  procedure ExportFieldFD(const Field:TFRE_DB_FIELD);
  begin
    if without_uid then begin
      if field.FieldName='UID' then exit;
    end;
    ra.Add(Field.GetAsJSON(without_uid,true,stream_cb));
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
    ra.Add(TJSONString.Create(ClassName));
    ra.Add(TJSONString.Create(FSchemeName));
    ForAll(@ExportFieldFD);
    result := ra;
    AfterSave;
  end;
end;

function TFRE_DB_Object.GetAsJSONString(const without_uid: boolean;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil): TFRE_DB_String;
begin
  _InAccessibleCheck;
  result := GetAsJSON(without_uid,full_dump,stream_cb).AsJSON;
end;

{ TFRE_DB_FIELD }

procedure TFRE_DB_FIELD.SetAsObject(const AValue: TFRE_DB_Object);
begin
  _InAccessibleCheck;
  Fobj._ParentCheck(Avalue);
  if not _CheckStoreType(fdbft_Object) then begin
    FFieldData.FieldType := fdbft_Object;
    New(FFieldData.obj);
    SetLength(FFieldData.obj^,1);
  end else begin
  //  FFieldData.obj^[0].Free; // NO AUTO FREE ON OVERWRITE
  end;
  FFieldData.obj^[0]:=AValue;
  AValue.FParentDBO := self;
end;

function TFRE_DB_FIELD.GetAsByteArray: TFRE_DB_ByteArray;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Byte);
  result := FFieldData.byte^;
end;

function TFRE_DB_FIELD.GetAsByteList(idx: Integer): Byte;
begin
  _InAccessibleCheck;
  _CheckFieldType(fdbft_Byte);
  _CheckIndex(idx,fdbft_Byte);
  result := FFieldData.byte^[idx];
end;


function TFRE_DB_FIELD.GetAsInt16Array: TFRE_DB_Int16Array;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Int16);
  result := FFieldData.in16^;
end;

function TFRE_DB_FIELD.GetAsInt16List(idx: Integer): Smallint;
begin
  _InAccessibleCheck;
  _CheckFieldType(fdbft_Int16);
  _CheckIndex(idx,fdbft_Int16);
  result := FFieldData.in16^[idx];
end;

function TFRE_DB_FIELD.GetAsInt32Array: TFRE_DB_Int32Array;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Int32);
  result := FFieldData.in32^;
end;

function TFRE_DB_FIELD.GetAsInt32List(idx: Integer): longint;
begin
  _InAccessibleCheck;
  _CheckFieldType(fdbft_Int32);
  _CheckIndex(idx,fdbft_Int32);
  result := FFieldData.in32^[idx];
end;

function TFRE_DB_FIELD.GetAsInt64Array: TFRE_DB_Int64Array;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Int64);
  result := FFieldData.in64^;
end;

function TFRE_DB_FIELD.GetAsInt64List(idx: Integer): int64;
begin
  _InAccessibleCheck;
  _CheckFieldType(fdbft_Int64);
  _CheckIndex(idx,fdbft_Int64);
  result := FFieldData.in64^[idx];
end;

function TFRE_DB_FIELD.GetAsSingleArray: TFRE_DB_Real32Array;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Real32);
  result := FFieldData.re32^;
end;

function TFRE_DB_FIELD.GetAsSingleList(idx: Integer): Single;
begin
  _InAccessibleCheck;
  _CheckFieldType(fdbft_Real32);
  _CheckIndex(idx,fdbft_Real32);
  result := FFieldData.re32^[idx];
end;

function TFRE_DB_FIELD.GetAsUInt16Array: TFRE_DB_UInt16Array;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_UInt16);
  result := FFieldData.ui16^;
end;

function TFRE_DB_FIELD.GetAsUInt16List(idx: Integer): Word;
begin
  _InAccessibleCheck;
  _CheckFieldType(fdbft_UInt16);
  _CheckIndex(idx,fdbft_UInt16);
  result := FFieldData.ui16^[idx];
end;

function TFRE_DB_FIELD.GetAsUInt32Array: TFRE_DB_UInt32Array;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_UInt32);
  result := FFieldData.ui32^;
end;

function TFRE_DB_FIELD.GetAsUInt32List(idx: Integer): longword;
begin
  _InAccessibleCheck;
  _CheckFieldType(fdbft_UInt32);
  _CheckIndex(idx,fdbft_UInt32);
  result := FFieldData.ui32^[idx];
end;

function TFRE_DB_FIELD.GetAsUInt64Array: TFRE_DB_UInt64Array;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_UInt64);
  result := FFieldData.ui64^;
end;

function TFRE_DB_FIELD.GetAsUInt64List(idx: Integer): uint64;
begin
  _InAccessibleCheck;
  _CheckFieldType(fdbft_UInt64);
  _CheckIndex(idx,fdbft_UInt64);
  result := FFieldData.ui64^[idx];
end;

procedure TFRE_DB_FIELD.SetAsByteArray(const AValue: TFRE_DB_ByteArray);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Byte) then begin
    FFieldData.FieldType := fdbft_Byte;
    New(FFieldData.byte);
  end;
  FFieldData.byte^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsByteList(idx: Integer; const AValue: Byte);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Byte) then begin
    New(FFieldData.byte);
  end;
  _CheckIndex(idx,fdbft_Byte);
  FFieldData.byte^[idx] := AValue;
end;


procedure TFRE_DB_FIELD.SetAsDateTimeUTCList(idx: Integer; const AValue: TFRE_DB_DateTime64);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_DateTimeUTC) then begin
    New(FFieldData.date);
  end;
  _CheckIndex(idx,fdbft_DateTimeUTC);
  FFieldData.date^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt16Array(const AValue: TFRE_DB_Int16Array);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Int16) then begin
    FFieldData.FieldType := fdbft_Int16;
    New(FFieldData.in16);
  end;
  FFieldData.in16^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt16List(idx: Integer; const AValue: Smallint);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Int16) then begin
    New(FFieldData.in16);
  end;
  _CheckIndex(idx,fdbft_Int16);
  FFieldData.in16^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt32Array(const AValue: TFRE_DB_Int32Array);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Int32) then begin
    FFieldData.FieldType := fdbft_Int32;
    New(FFieldData.in32);
  end;
  FFieldData.in32^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt32List(idx: Integer; const AValue: longint);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Int32) then begin
    New(FFieldData.in32);
  end;
  _CheckIndex(idx,fdbft_Int32);
  FFieldData.in32^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt64Array(const AValue: TFRE_DB_Int64Array);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Int64) then begin
    FFieldData.FieldType := fdbft_Int64;
    New(FFieldData.in64);
  end;
  FFieldData.in64^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt64List(idx: Integer; const AValue: int64);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Int64) then begin
    New(FFieldData.in64);
  end;
  _CheckIndex(idx,fdbft_Int64);
  FFieldData.in64^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsSingleArray(const AValue: TFRE_DB_Real32Array);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Real32) then begin
    FFieldData.FieldType := fdbft_Real32;
    New(FFieldData.re32);
  end;
  FFieldData.re32^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsSingleList(idx: Integer; const AValue: Single);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Real32) then begin
    New(FFieldData.re32);
  end;
  _CheckIndex(idx,fdbft_Real32);
  FFieldData.re32^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt16Array(const AValue: TFRE_DB_UInt16Array);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_UInt16) then begin
    FFieldData.FieldType := fdbft_UInt16;
    New(FFieldData.ui16);
  end;
  FFieldData.ui16^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt16List(idx: Integer; const AValue: Word);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_UInt16) then begin
    New(FFieldData.ui16);
  end;
  _CheckIndex(idx,fdbft_UInt16);
  FFieldData.ui16^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt32Array(const AValue: TFRE_DB_UInt32Array);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_UInt32) then begin
    FFieldData.FieldType := fdbft_UInt32;
    New(FFieldData.ui32);
  end;
  FFieldData.ui32^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt32List(idx: Integer; const AValue: longword);
begin
   _InAccessibleCheck;
  if not _CheckStoreType(fdbft_UInt32) then begin
    New(FFieldData.ui32);
  end;
  _CheckIndex(idx,fdbft_UInt32);
  FFieldData.ui32^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt64Array(const AValue: TFRE_DB_UInt64Array);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_UInt64) then begin
    FFieldData.FieldType := fdbft_UInt64;
    New(FFieldData.ui64);
  end;
  FFieldData.ui64^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt64List(idx: Integer; const AValue: uint64);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_UInt64) then begin
    New(FFieldData.ui64);
  end;
  _CheckIndex(idx,fdbft_UInt64);
  FFieldData.ui64^[idx] := AValue;
end;

//procedure TFRE_DB_FIELD._PrecheckObjLinkSet(const value: TGuid);
//var i:integer;
//begin
//  for i:=0 to high(FFieldData.obl^) do begin
//    if FREDB_Guids_Same(value,FFieldData.obl^[i]) then raise EFRE_DB_Exception.Create(edb_EXISTS,'objectlink to %s already set in field %s',[GFRE_BT.GUID_2_HexString(value),FFieldName^]);
//  end;
//end;

function TFRE_DB_FIELD.GetAsByte: Byte;
begin
  _InAccessibleCheck;
  if FFieldData.FieldType = fdbft_Byte then begin
    result := FFieldData.byte^[0];
  end else begin
    result := _ConvertToByte;
  end;
end;

function TFRE_DB_FIELD.GetAsInt16: Smallint;
begin
  _InAccessibleCheck;
  if FFieldData.FieldType = fdbft_Int16 then begin
    result := FFieldData.in16^[0];
  end else begin
    result := _ConvertToInt16;
  end;
end;

function TFRE_DB_FIELD.GetAsInt32: longint;
begin
  _InAccessibleCheck;
  if FFieldData.FieldType = fdbft_Int32 then begin
    result := FFieldData.in32^[0];
  end else begin
    result := _ConvertToInt32;
  end;
end;

function TFRE_DB_FIELD.GetAsInt64: int64;
begin
  _InAccessibleCheck;
  if FFieldData.FieldType = fdbft_Int64 then begin
    result := FFieldData.in64^[0];
  end else begin
    result := _ConvertToInt64;
  end;
end;

function TFRE_DB_FIELD.GetAsSingle: Single;
begin
  _InAccessibleCheck;
  if FFieldData.FieldType = fdbft_Real32 then begin
    result := FFieldData.re32^[0];
  end else begin
    result := _ConvertToSingle;
  end;
end;

function TFRE_DB_FIELD.GetAsUInt16: Word;
begin
  _InAccessibleCheck;
  if FFieldData.FieldType = fdbft_UInt16 then begin
    result := FFieldData.ui16^[0];
  end else begin
    result := _ConvertToUInt16;
  end;
end;

function TFRE_DB_FIELD.GetAsUInt32: longword;
begin
  _InAccessibleCheck;
  if FFieldData.FieldType = fdbft_UInt32 then begin
    result := FFieldData.ui32^[0];
  end else begin
    result := _ConvertToUInt32;
  end;
end;

function TFRE_DB_FIELD.GetAsUInt64: uint64;
begin
  _InAccessibleCheck;
  if FFieldData.FieldType = fdbft_UInt64 then begin
    result := FFieldData.ui64^[0];
  end else begin
    result := _ConvertToUInt64;
  end;
end;

procedure TFRE_DB_FIELD.SetAsByte(const AValue: Byte);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Byte) then begin
    FFieldData.FieldType := fdbft_Byte;
    New(FFieldData.byte);
    SetLength(FFieldData.byte^,1);
  end;
  FFieldData.byte^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt16(const AValue: Smallint);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Int16) then begin
    FFieldData.FieldType := fdbft_Int16;
    New(FFieldData.in16);
    SetLength(FFieldData.in16^,1);
  end;
  FFieldData.in16^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt32(const AValue: longint);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Int32) then begin
    FFieldData.FieldType := fdbft_Int32;
    New(FFieldData.in32);
    SetLength(FFieldData.in32^,1);
  end;
  FFieldData.in32^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsInt64(const AValue: int64);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Int64) then begin
    FFieldData.FieldType := fdbft_Int64;
    New(FFieldData.in64);
    SetLength(FFieldData.in64^,1);
  end;
  FFieldData.in64^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsSingle(const AValue: Single);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Real32) then begin
    FFieldData.FieldType := fdbft_Real32;
    New(FFieldData.re32);
    SetLength(FFieldData.re32^,1);
  end;
  FFieldData.re32^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt16(const AValue: Word);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_UInt16) then begin
    FFieldData.FieldType := fdbft_UInt16;
    New(FFieldData.ui16);
    SetLength(FFieldData.ui16^,1);
  end;
  FFieldData.ui16^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt32(const AValue: longword);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_UInt32) then begin
    FFieldData.FieldType := fdbft_UInt32;
    New(FFieldData.ui32);
    SetLength(FFieldData.ui32^,1);
  end;
  FFieldData.ui32^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsUInt64(const AValue: uint64);
begin
  _InAccessibleCheck;
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
   result := 2 * CFRE_DB_SIZE_ENCODING_SIZE * Length(FFieldData.obj^); // Fieldcount per Object + Streamsize per object
   for i :=  0 to Length(FFieldData.obj^)-1 do begin
     result := result + FFieldData.obj^[i]._StreamingSize;
   end;
 end;

 function __HeaderSize: TFRE_DB_SIZE_TYPE;
 begin
   result := 3*CFRE_DB_SIZE_ENCODING_SIZE+length(FFieldName^);
 end;

begin
  //if FFieldStreamSize<>-1 then begin
  //  exit(FFieldStreamSize);
  //end;
  case FFieldData.FieldType of
    fdbft_CalcField:   result := 0;
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

//class function TFRE_DB_FIELD._CalcXTRAStrSize(const acc_field_name_len: integer; const acc_stream_len: integer;const fld_count:integer=1): integer;
//begin
//  result :=   (fld_count * 3 * CFRE_DB_SIZE_ENCODING_SIZE) + acc_field_name_len
//            + (fld_count * CFRE_DB_SIZE_ENCODING_SIZE) +acc_stream_len;
//end;

function TFRE_DB_FIELD.GetAsBoolean: Boolean;
begin
  _InAccessibleCheck;
  if FFieldData.FieldType = fdbft_Boolean then begin
    result := FFieldData.bool^[0];
  end else begin
    result := _ConvertToBool;
  end;
end;

function TFRE_DB_FIELD.GetAsBooleanArray: TFRE_DB_BoolArray;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Boolean);
  result := FFieldData.bool^;
end;

function TFRE_DB_FIELD.GetAsBooleanList(idx: Integer): Boolean;
begin
  _InAccessibleCheck;
  _CheckFieldType(fdbft_Boolean);
  _CheckIndex(idx,fdbft_Boolean);
  result := FFieldData.bool^[idx];
end;

procedure TFRE_DB_FIELD.SetAsBoolean(const AValue: Boolean);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Boolean) then begin
    FFieldData.FieldType := fdbft_Boolean;
    New(FFieldData.bool);
    SetLength(FFieldData.bool^,1);
  end;
  FFieldData.bool^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsObjectLink(const AValue: TGUID);
var old_array,new_array : TFRE_DB_GUIDArray;
begin
  _InAccessibleCheck;
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
  _InAccessibleCheck;
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
  _InAccessibleCheck;
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
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Boolean) then begin
    New(FFieldData.bool);
  end;
  _CheckIndex(idx,fdbft_Boolean);
  FFieldData.bool^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsObjectLinkList(idx: Integer; const AValue: TGUID); //TODO Check - Count always right ? / new
var new_fld:boolean;
    old_array,new_array:TFRE_DB_GUIDArray;
begin
  _InAccessibleCheck;
  if FREDB_Guids_Same(AValue,Fobj.UID) then raise EFRE_DB_Exception.Create(edb_ERROR,'referencing a objectlink to self seems to be senseless');
  if not _CheckStoreType(fdbft_ObjLink) then begin
    New(FFieldData.obl);
    new_fld:=true;
  end;
  _CheckIndex(idx,fdbft_ObjLink);
  FFieldData.obl^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsEmptyStringArray;
var sa:TFRE_DB_StringArray;
begin
  _InAccessibleCheck;
  SetLength(sa,0);
  SetAsStringArray(sa);
end;

function TFRE_DB_FIELD.IsEmptyArray: boolean;
begin
  _InAccessibleCheck;
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
          if not (FFieldData.strg^[i] = cmp_fld.FFieldData.strg^[i]) then
            exit(false);
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
          if not FREDB_Guids_Same(FFieldData.obj^[i].UID,cmp_fld.FFieldData.obj^[i].UID) then
            exit(false);
        fdbft_ObjLink:
          if not FREDB_Guids_Same(FFieldData.obl^[i],cmp_fld.FFieldData.obl^[i]) then
            exit(false);
        fdbft_CalcField:
            ; // ignore, if all other fields are the same then the calculated field is also the same
        else raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled '+CFRE_DB_FIELDTYPE[FFieldData.FieldType]);
      end;
    end;
  result := true;
end;

procedure TFRE_DB_FIELD.IntfCast(const InterfaceSpec: ShortString; out Intf);
begin
  _InAccessibleCheck;
  AsObject.IntfCast(InterfaceSpec,intf);
end;

function TFRE_DB_FIELD.AsDBText: IFRE_DB_TEXT;
begin
  _InAccessibleCheck;
  IntfCast(IFRE_DB_TEXT,result);
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
    fdbft_CalcField:   hi :=                     0;
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
    fdbft_Object  :    hi := high(FFieldData.obj^) ;
    fdbft_ObjLink :    hi := high(FFieldData.obl^) ;
  end;
end;

procedure TFRE_DB_FIELD._StripObject;
begin
  GFRE_BT.CriticalAbort('---FUNCTION BAD - STRIP');
  setlength(FFieldData.obj^,0);
  FFieldData.FieldType := fdbft_NotFound;
  //FFieldStreamSize     := -1;
end;

function TFRE_DB_FIELD._CalculateObject: IFRE_DB_Object;
begin
  result := FCurrentCalcMethod(nil);
end;


function TFRE_DB_FIELD._ConvertToCurrency: Currency;

  procedure _String2Currency;
  begin
    if not TryStrToCurr(FFieldData.strg^[0],result,GFRE_DB.FFormatSettings) then _StringToConvError(fdbft_Currency);
  end;

  procedure _Calc2;
  var co  : IFRE_DB_Object;
      key : TFRE_DB_String;
  begin
    co  := _CalculateObject;
    try
      key := CalcFieldResultKey(fdbft_Currency);
      if not co.FieldExists(key) then _ResultTypeUnset(fdbft_Currency);
      result:=co.Field(key).AsCurrency;
    finally
      co.Finalize;
    end;
  end;

begin
  case FFieldData.FieldType of
    fdbft_CalcField:     _Calc2;
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

  procedure _Calc2;
  var co  : IFRE_DB_Object;
      key : TFRE_DB_String;
  begin
    co  := _CalculateObject;
    try
      key := CalcFieldResultKey(fdbft_DateTimeUTC);
      if not co.FieldExists(key) then _ResultTypeUnset(fdbft_DateTimeUTC);
      result:=co.Field(key).AsDateTimeUTC;
    finally
      co.Finalize;
    end;
  end;

begin
  case FFieldData.FieldType of
    fdbft_CalcField:   _Calc2;
    fdbft_String:      _String2DateTime;
    fdbft_DateTimeUTC: result := FFieldData.date^[0];
    else _IllegalTypeError(fdbft_DateTimeUTC);
  end;
end;


function TFRE_DB_FIELD._ConvertToGUID: TGuid;
  procedure _String2;
  begin
    try
      Result:=GFRE_BT.HexString_2_GUID(FFieldData.strg^[0]);
    except
      _StringToConvError(fdbft_GUID);
    end;
  end;

  procedure _Calc2;
  var co  : IFRE_DB_Object;
      key : TFRE_DB_String;
  begin
    co  := _CalculateObject;
    try
      key := CalcFieldResultKey(fdbft_GUID);
      if not co.FieldExists(key) then _ResultTypeUnset(fdbft_GUID);
      result:=co.Field(key).AsGUID;
    finally
      co.Finalize;
    end;
  end;

begin
  case FFieldData.FieldType of
    fdbft_GUID:           result := FFieldData.guid^[0];
    fdbft_String:         _String2;
    fdbft_CalcField:      _Calc2;
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

  procedure _Calc2;
  var co  : IFRE_DB_Object;
      key : TFRE_DB_String;
  begin
    co  := _CalculateObject;
    try
      key := CalcFieldResultKey(fdbft_Byte);
      if not co.FieldExists(key) then _ResultTypeUnset(fdbft_Byte);
      result:=co.Field(key).AsByte;
    finally
      co.Finalize;
    end;
  end;

begin
  case FFieldData.FieldType of
    fdbft_CalcField:     _Calc2;
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

  procedure _Calc2;
  var co  : IFRE_DB_Object;
      key : TFRE_DB_String;
  begin
    co  := _CalculateObject;
    try
      key := CalcFieldResultKey(fdbft_Int16);
      if not co.FieldExists(key) then _ResultTypeUnset(fdbft_Int16);
      result:=co.Field(key).AsInt16;
    finally
      co.Finalize;
    end;
  end;

begin
  case FFieldData.FieldType of
    fdbft_CalcField:     _Calc2;
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

  procedure _Calc2;
  var co  : IFRE_DB_Object;
      key : TFRE_DB_String;
  begin
    co  := _CalculateObject;
    try
      key := CalcFieldResultKey(fdbft_UInt16);
      if not co.FieldExists(key) then _ResultTypeUnset(fdbft_UInt16);
      result:=co.Field(key).AsUInt16;
    finally
      co.Finalize;
    end;
  end;

begin
  case FFieldData.FieldType of
    fdbft_CalcField:     _Calc2;
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

  procedure _Calc2;
  var co  : IFRE_DB_Object;
      key : TFRE_DB_String;
  begin
    co  := _CalculateObject;
    try
      key := CalcFieldResultKey(fdbft_Int32);
      if not co.FieldExists(key) then _ResultTypeUnset(fdbft_Int32);
      result:=co.Field(key).AsInt32;
    finally
      co.Finalize;
    end;
  end;

begin
  case FFieldData.FieldType of
    fdbft_CalcField:     _Calc2;
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

  procedure _Calc2;
  var co  : IFRE_DB_Object;
      key : TFRE_DB_String;
  begin
    co  := _CalculateObject;
    try
      key := CalcFieldResultKey(fdbft_UInt32);
      if not co.FieldExists(key) then _ResultTypeUnset(fdbft_UInt32);
      result:=co.Field(key).AsUInt32;
    finally
      co.Finalize;
    end;
  end;

begin
  case FFieldData.FieldType of
    fdbft_CalcField:     _Calc2;
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

  procedure _Calc2;
  var co  : IFRE_DB_Object;
      key : TFRE_DB_String;
  begin
    co  := _CalculateObject;
    try
      key := CalcFieldResultKey(fdbft_Int64);
      if not co.FieldExists(key) then _ResultTypeUnset(fdbft_Int64);
      result:=co.Field(key).AsInt64;
    finally
      co.Finalize;
    end;
  end;

begin
  case FFieldData.FieldType of
    fdbft_CalcField:     _Calc2;
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

  procedure _Calc2;
  var co  : IFRE_DB_Object;
      key : TFRE_DB_String;
  begin
    co  := _CalculateObject;
    try
      key := CalcFieldResultKey(fdbft_UInt64);
      if not co.FieldExists(key) then _ResultTypeUnset(fdbft_UInt64);
      result:=co.Field(key).AsUInt64;
    finally
      co.Finalize;
    end;
  end;

begin
  case FFieldData.FieldType of
    fdbft_CalcField:     _Calc2;
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

  procedure _Calc2;
  var co  : IFRE_DB_Object;
      key : TFRE_DB_String;
  begin
    co  := _CalculateObject;
    try
      key := CalcFieldResultKey(fdbft_Real32);
      if not co.FieldExists(key) then _ResultTypeUnset(fdbft_Real32);
      result:=co.Field(key).AsReal32;
    finally
      co.Finalize;
    end;
  end;

begin
  case FFieldData.FieldType of
    fdbft_CalcField:     _Calc2;
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

  procedure _Calc2;
  var co  : IFRE_DB_Object;
      key : TFRE_DB_String;
  begin
    co  := _CalculateObject;
    try
      key := CalcFieldResultKey(fdbft_Real64);
      if not co.FieldExists(key) then _ResultTypeUnset(fdbft_Real64);
      result:=co.Field(key).AsReal64;
    finally
      co.Finalize;
    end;
  end;

begin
  case FFieldData.FieldType of
    fdbft_CalcField:     _Calc2;
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

  procedure _Calc2;
  var co  : IFRE_DB_Object;
      key : TFRE_DB_String;
  begin
    co  := _CalculateObject;
    try
      key := CalcFieldResultKey(fdbft_Boolean);
      if not co.FieldExists(key) then _ResultTypeUnset(fdbft_Boolean);
      result:=co.Field(key).AsBoolean;
    finally
      co.Finalize;
    end;
  end;

begin
  case FFieldData.FieldType of
    fdbft_CalcField:   _Calc2;
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
  //var s:TFRE_DB_String;
  //    i:integer;
  begin
    result := '/FREDB_SFA/'+GFRE_BT.GUID_2_HexString(Fobj.UID)+'/'+gfre_bt.Str2HexStr(FFieldName^);
    result := '/fre_css/'+ cFRE_WEB_STYLE + '/images/user.png'; // FIXXME
    //i:=FFieldData.strm^[idx].Size;
    //SetLength(s,i);
    //Move(FFieldData.strm^[idx].Memory^,s[1],i);
    //result:=GFRE_BT.Base64Encode(s);
  end;

  function _Calculate2String : TFRE_DB_String;
  var co:IFRE_DB_Object;
  begin
     co := _CalculateObject;
     result := co.DumpToString;
     co.Finalize;
  end;

  procedure _Calc2;
  var co  : IFRE_DB_Object;
      key : TFRE_DB_String;
  begin
    co  := _CalculateObject;
    try
      key := CalcFieldResultKey(fdbft_String);
      if not co.FieldExists(key) then _ResultTypeUnset(fdbft_String);
      result:=co.Field(key).AsString;
    finally
      co.Finalize;
    end;
  end;


begin
  if ValueCount=0 then exit('');
  case FFieldData.FieldType of
    fdbft_CalcField:   _Calc2;
    fdbft_NotFound:    result := '';
    fdbft_GUID:        result := GFRE_BT.GUID_2_HexString(FFieldData.guid^[idx]);
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
    fdbft_Object:      result := FFieldData.obj^[idx].DumpToString;
    fdbft_ObjLink:
      begin
        result := FREDB_GuidArray2String(FFieldData.obl^);
        //result :=  GFRE_BT.GUID_2_HexString(FFieldData.obl^[idx]);
      end
    else               raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled %s',[CFRE_DB_FIELDTYPE[FFieldData.FieldType]]);
  end;
end;


procedure TFRE_DB_FIELD._CheckFieldType(const expected: TFRE_DB_FIELDTYPE);
begin
  if (FFieldData.FieldType=fdbft_CalcField) and (expected=fdbft_Object) then exit;
  if FFieldData.FieldType<>expected then
    raise EFRE_DB_Exception.Create(edb_FIELDMISMATCH,' got '+CFRE_DB_FIELDTYPE[FFieldData.FieldType]+' expected '+CFRE_DB_FIELDTYPE[expected]+' for field ['+FieldName+']');
end;

procedure TFRE_DB_FIELD._CheckIndex(const idx: integer; const typ: TFRE_DB_FIELDTYPE);
var lo,hi:integer;
begin
  hi:=0;
  _GetHigh(hi);
  if not ((idx>=0) and (idx<=hi)) then EFRE_DB_Exception.Create(edb_INDEXOUTOFBOUNDS,format(' fieldlist index out of bounds. idx is <%d>, but should be >= %d and <= %d ',[idx,lo,hi]));
end;

function TFRE_DB_FIELD._CheckStoreType(const expected: TFRE_DB_FIELDTYPE):boolean;
begin
  if Fobj._ReadOnlyCheck then raise EFRE_DB_Exception.Create(edb_ACCESS,' access to read only object  fieldname ['+FFieldName^+'] type ['+CFRE_DB_FIELDTYPE[FFieldData.FieldType]+']');
  if (FFieldData.FieldType=fdbft_NotFound) then begin
    exit(false);
  end else
  if (FFieldData.FieldType=expected) then begin
    exit(true);
  end else begin
    raise EFRE_DB_Exception.Create(edb_FIELDMISMATCH,' got '+CFRE_DB_FIELDTYPE[FFieldData.FieldType]+' expected '+CFRE_DB_FIELDTYPE[expected]+' or uninitialized field');
  end;
end;


procedure TFRE_DB_FIELD._LocalToUTC(var arr: TFRE_DB_DateTimeArray);
var i,lo,hi: Integer;
begin
 for i:=low(arr) to High(arr) do begin
   arr[i] := GFRE_DB.LocalTimeToUTCDB64(arr[i]);
  end;
end;

procedure TFRE_DB_FIELD._NotAllowedOnUIDFieldCheck;
begin
  if FIsUidField then
    raise EFRE_DB_Exception.Create(edb_ERROR,'operation not allowed on special UID field!');
end;

procedure TFRE_DB_FIELD._InaccessibleCheck;
begin
  Fobj._InaccessibleCheck;
end;

function TFRE_DB_FIELD._DBConnectionBC: TFRE_DB_BASE_CONNECTION;
begin
 result := Fobj._DBConnectionBC;
end;

function TFRE_DB_FIELD._DBConnection: TFRE_DB_CONNECTION;
begin
 result := Fobj._DbConnection as TFRE_DB_CONNECTION;
end;

procedure TFRE_DB_FIELD.Finalize;
begin
  gfre_bt.CriticalAbort('never, ever finalize fields!');
end;

function TFRE_DB_FIELD.GetAsObjectLinkArrayObj: TFRE_DB_ObjectArray;
var object_link_array : TFRE_DB_ObjLinkArray;
    dbo               : TFRE_DB_Object;
    i                 : integer;
begin
  _InAccessibleCheck;
  object_link_array := GetAsObjectLinkArray;
  SetLength(result,length(object_link_array));
  for i:=0 to high(object_link_array) do begin
    if not _DBConnection.Fetch(object_link_array[i],dbo) then raise EFRE_DB_Exception.Create(edb_INTERNAL,'object integrity fail could not resove objectlink field [%s] index [%d] to object [%s]',[FieldName,i,GFRE_BT.GUID_2_HexString(object_link_array[i])]);
    Result[i] := dbo;
  end;
end;

procedure TFRE_DB_FIELD.SetAsObjectLinkArrayObj(const AValue: TFRE_DB_ObjectArray);
var object_link_array : TFRE_DB_ObjLinkArray;
    i                 : integer;
begin
  _InAccessibleCheck;
  SetLength(object_link_array,length(AValue));
  for i:=0 to high(object_link_array) do begin
    object_link_array[i]:=AValue[i].UID;
  end;
  SetAsObjectLinkArray(object_link_array);
end;




function TFRE_DB_FIELD.CopyFieldToMem(var mempointer: Pointer;const without_schemes:boolean): TFRE_DB_SIZE_TYPE; //Streamed as FieldType,FieldValCount,FieldNameSize,{FieldName},{FieldData}
var check_size  : TFRE_DB_SIZE_TYPE;
    startp,oldp : PByte;
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
    var i,l:integer;
        oldsp:pointer;
        state:TFRE_DB_String;
    begin
      l:=Length(FFieldData.obj^)-1;
      for i := 0 to l do begin
        sz_field := TFRE_DB_SIZE_TYPE(FFieldData.obj^[i].FieldCount(true));_StoreSzField;
        oldsp := startp;
        FFieldData.obj^[i].CopyToMem(startp,without_schemes);
      end;
    end;

begin
  result:=0;
  if (FFieldData.FieldType = fdbft_NotFound) or (FFieldData.FieldType = fdbft_CalcField) then begin
   exit;
  end;
  check_size := _StreamingSize;
  startp     := mempointer;
  oldp       := startp;
  sz_field   := Length(FFieldName^);       _StoreSzField; _StoreName;
  sz_field   := Ord(FFieldData.FieldType); _StoreSzField;
  sz_field   := ValueCount;                _StoreSzField;
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

procedure TFRE_DB_FIELD.CopyFieldFromMem(var mempointer: Pointer; const conn: TFRE_DB_BASE_CONNECTION; const recreate_weak_schemes: boolean; const generate_new_uids: boolean);
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
   var i,l:integer;
   begin
     l:=Length(FFieldData.obj^)-1;
     for i := 0 to l do begin
       FFieldData.obj^[i] := TFRE_DB_Object.CreateInternalStreaming(self,startp,conn,recreate_weak_schemes,generate_new_uids);
     end;
   end;

begin
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
    fdbft_Object:      begin New(FFieldData.obj) ;SetLength(FFieldData.obj^ ,value_count);_ReadObjects; end;
    fdbft_ObjLink:     begin New(FFieldData.obl);SetLength(FFieldData.obl^,value_count);_ReadArray(SizeOf(FFieldData.obl^[0]) * length(FFieldData.obl^),@FFieldData.obl^[0]) end;
    else               raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled '+inttostr(ord(FFieldData.FieldType)));
  end;
  mempointer := startp;
end;

function TFRE_DB_FIELD.GetAsCurrency: Currency;
begin
  _InAccessibleCheck;
  if FFieldData.FieldType = fdbft_Currency then begin
    result := FFieldData.curr^[0];
  end else begin
    result := _ConvertToCurrency;
  end;
end;

function TFRE_DB_FIELD.GetAsCurrencyArray: TFRE_DB_CurrencyArray;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Currency);
  result := FFieldData.curr^;
end;

function TFRE_DB_FIELD.GetAsCurrencyList(idx: Integer): Currency;
begin
  _InAccessibleCheck;
  _CheckFieldType(fdbft_Currency);
  _CheckIndex(idx,fdbft_Currency);
  result := FFieldData.curr^[idx];
end;

function TFRE_DB_FIELD.GetAsDateTime: TFRE_DB_DateTime64;
begin
  _InAccessibleCheck;
  if FFieldData.FieldType = fdbft_DateTimeUTC then begin
    result := GFRE_DB.UTCToLocalTimeDB64(FFieldData.date^[0]);
  end else begin
    result := _ConvertToDateTime;
  end;
end;

function TFRE_DB_FIELD.GetAsDateTimeArray: TFRE_DB_DateTimeArray;
var temp : TFRE_DB_DateTimeArray;
       i : Integer;
begin
  _InAccessibleCheck;
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
  _InAccessibleCheck;
  _CheckFieldType(fdbft_DateTimeUTC);
  _CheckIndex(idx,fdbft_DateTimeUTC);
 result := GFRE_DB.UTCToLocalTimeDB64(FFieldData.date^[idx]);
end;

function TFRE_DB_FIELD.GetAsDateTimeUTC: TFRE_DB_DateTime64;
begin
  _InAccessibleCheck;
  if FFieldData.FieldType = fdbft_DateTimeUTC then begin
    result := FFieldData.date^[0];
  end else begin
    result := _ConvertToDateTime;
  end;
end;

function TFRE_DB_FIELD.GetAsString: TFRE_DB_String;
begin
  _InAccessibleCheck;
  if FFieldData.FieldType = fdbft_String then begin
    result := FFieldData.strg^[0];
  end else begin
    result := _ConvertToString;
  end;
end;

function TFRE_DB_FIELD.GetAsDateTimeArrayUTC: TFRE_DB_DateTimeArray;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_DateTimeUTC);
  result := FFieldData.date^;
end;

function TFRE_DB_FIELD.GetAsDateTimeListUTC(idx: Integer): TFRE_DB_DateTime64;
begin
  _InAccessibleCheck;
  _CheckFieldType(fdbft_DateTimeUTC);
  _CheckIndex(idx,fdbft_DateTimeUTC);
  result := FFieldData.date^[idx];
end;


function TFRE_DB_FIELD.GetAsDouble: Double;
begin
  _InAccessibleCheck;
  if FFieldData.FieldType = fdbft_Real64 then begin
    result := FFieldData.re64^[0];
  end else begin
    result := _ConvertToDouble;
  end;
end;

function TFRE_DB_FIELD.GetAsDoubleArray: TFRE_DB_Real64Array;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Real64);
  result := FFieldData.re64^;
end;

function TFRE_DB_FIELD.GetAsDoubleList(idx: Integer): Double;
begin
  _InAccessibleCheck;
  _CheckFieldType(fdbft_Real64);
  _CheckIndex(idx,fdbft_Real64);
  result := FFieldData.re64^[idx];
end;

function TFRE_DB_FIELD.GetAsGUID: TGuid;
begin
  _InAccessibleCheck;
  if FFieldData.FieldType = fdbft_GUID then begin
    result := FFieldData.guid^[0];
  end else begin
    result := _ConvertToGUID;
  end;
end;

function TFRE_DB_FIELD.GetAsGUIDArray: TFRE_DB_GUIDArray;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_GUID);
  result := FFieldData.guid^;
end;

function TFRE_DB_FIELD.GetAsGUIDList(idx: Integer): TGUID;
begin
  _InAccessibleCheck;
  _CheckFieldType(fdbft_GUID);
  _CheckIndex(idx,fdbft_GUID);
  result := FFieldData.guid^[idx];
end;


function TFRE_DB_FIELD.GetAsObject: TFRE_DB_Object;
var field_type :TFRE_DB_FIELDTYPE;

  function _PCheck:boolean;
  var sc               : TFRE_DB_String;
      scheme_object    : TFRE_DB_SchemeObject;
      scheme_field_def : TFRE_DB_FieldSchemeDefinition;
      new_object       : TFRE_DB_Object;
      sfc              : TFRE_DB_String;
  begin
    result := false;
    sc:=FObj.SchemeClass;
    if (sc<>'') and not (Fobj._ObjectsNeedsNoSubfieldSchemeCheck) then begin
      field_type:=field_type;
      if not GFRE_DB.GetSystemScheme(sc,scheme_object) then begin
        try
          if not _DBConnectionBC.GetScheme(sc,scheme_object) then begin
            raise EFRE_DB_Exception.Create(edb_ERROR,'a new sub object wants to get accessed, a scheme is defined but cannot be checked. Scheme=%s Fieldname=%s',[sc,FieldName]);
          end;
        except on e:Exception do begin
          raise EFRE_DB_Exception.Create(edb_ERROR,'a new sub object wants to get accessed, a scheme is defined but cannot be checked. Scheme=%s Fieldname=%s Ex=%s',[sc,FieldName,e.Message]);
        end;end;
      end;
      if scheme_object.GetSchemeField(FieldName,scheme_field_def) then begin
        //scheme_object.ConstructNewInstance(scheme_field_def.SubschemeName);
        sfc := scheme_field_def.SubschemeName;
        if not GFRE_DB.GetSystemScheme(sfc,scheme_object) then begin
          if not _DBConnectionBC.GetScheme(sfc,scheme_object) then begin
            raise EFRE_DB_Exception.Create(edb_ERROR,'a new sub object wants to get accessed, a scheme is defined but cannot be checked. Scheme=%s Fieldname=%s SubfieldSchemc=%s',[sc,FieldName,sfc]);
          end;
        end;
        new_object := scheme_object.ConstructNewInstance;
        //new_object := _DBConnectionBC.NewObject(sfc);
        AddObject(new_object);
        result:=true;
      end;
    end;
  end;

begin
  _InAccessibleCheck;
  field_type := FFieldData.FieldType;
  if field_type = fdbft_CalcField then begin
    Result := _CalculateObject.Implementor as TFRE_DB_Object;
  end else
  if field_type = fdbft_Object then begin
    result := FFieldData.obj^[0];
  end else begin
    if field_type=fdbft_NotFound then begin
       //Schema Check
       if not _Pcheck then begin
         AddObject(TFRE_DB_Object.Create);
       end;
       result            := FFieldData.obj^[0];
       result.FParentDBO := self;
    end else begin
      _IllegalTypeError(fdbft_Object);
    end;
  end;
end;

function TFRE_DB_FIELD.GetAsObjectArray: TFRE_DB_ObjectArray;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Object);
  if FieldType=fdbft_CalcField then begin
    SetLength(result,1);
    result[0] := _CalculateObject.Implementor as TFRE_DB_Object;
  end else begin
    result := FFieldData.obj^;
  end;
end;

function TFRE_DB_FIELD.GetAsObjectLinkArray: TFRE_DB_ObjLinkArray;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_ObjLink);
  result := Copy(FFieldData.obl^);
end;

function TFRE_DB_FIELD.GetAsObjectList(idx: Integer): TFRE_DB_Object;
begin
  _InAccessibleCheck;
  _CheckFieldType(fdbft_Object);
  _CheckIndex(idx,fdbft_Object);
  result := FFieldData.obj^[idx];
end;

function TFRE_DB_FIELD.GetAsObjectLinkList(idx: Integer): TGUID;
begin
 _InAccessibleCheck;
 _CheckFieldType(fdbft_ObjLink);
 _CheckIndex(idx,fdbft_ObjLink);
 result := FFieldData.obl^[idx];
end;

function TFRE_DB_FIELD.GetAsStream: TFRE_DB_Stream;
var field_type :TFRE_DB_FIELDTYPE;
begin
  _InAccessibleCheck;
  field_type := FFieldData.FieldType;
  if field_type = fdbft_Stream then begin
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

function TFRE_DB_FIELD.GetAsObjectLink: TGuid;
begin
  _InAccessibleCheck;
  if FFieldData.FieldType = fdbft_ObjLink then begin
    result := FFieldData.obl^[0];
  end else begin
    _IllegalTypeError(fdbft_ObjLink);
  end;
end;

function TFRE_DB_FIELD.GetAsStreamArray: TFRE_DB_StreamArray;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end;
  _CheckFieldType(fdbft_Stream);
  result := FFieldData.strm^;
end;

function TFRE_DB_FIELD.GetAsStreamList(idx: Integer): TFRE_DB_Stream;
begin
  _InAccessibleCheck;
  _CheckFieldType(fdbft_Stream);
  _CheckIndex(idx,fdbft_Stream);
  result := FFieldData.strm^[idx];
end;

function TFRE_DB_FIELD.GetAsStringArray: TFRE_DB_StringArray;
var lo,hi,i:integer;
begin
  _InAccessibleCheck;
  if FieldType=fdbft_NotFound then begin
    SetLength(Result,0);
    exit;
  end else
  if FieldType=fdbft_String then begin
    result := FFieldData.strg^;
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
  _InAccessibleCheck;
  _CheckFieldType(fdbft_String);
  _CheckIndex(idx,fdbft_String);
  result := FFieldData.strg^[idx];
end;

procedure TFRE_DB_FIELD.SetAsCurrency(const AValue: Currency);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Currency) then begin
    FFieldData.FieldType := fdbft_Currency;
    New(FFieldData.curr);
    SetLength(FFieldData.curr^,1);
  end;
  FFieldData.curr^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsCurrencyArray(const AValue: TFRE_DB_CurrencyArray);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Currency) then begin
    FFieldData.FieldType := fdbft_Currency;
    New(FFieldData.curr);
  end;
  FFieldData.curr^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsCurrencyList(idx: Integer; const AValue: Currency);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Currency) then begin
    New(FFieldData.curr);
  end;
  _CheckIndex(idx,fdbft_Currency);
  FFieldData.curr^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsDateTime(const AValue: TFRE_DB_Datetime64);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_DateTimeUTC) then begin
    FFieldData.FieldType := fdbft_DateTimeUTC;
    New(FFieldData.date);
    SetLength(FFieldData.date^,1);
  end;
  FFieldData.date^[0]:=GFRE_DB.LocalTimeToUTCDB64(AValue);
end;

procedure TFRE_DB_FIELD.SetAsDateTimeArray(const AValue: TFRE_DB_DateTimeArray);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_DateTimeUTC) then begin
    FFieldData.FieldType := fdbft_DateTimeUTC;
    New(FFieldData.date);
  end;
  FFieldData.date^ := AValue;
  _LocalToUTC(FFieldData.date^);
end;

procedure TFRE_DB_FIELD.SetAsDateTimeList(idx: Integer; const AValue: TFRE_DB_Datetime64);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_DateTimeUTC) then begin
    New(FFieldData.date);
  end;
  _CheckIndex(idx,fdbft_DateTimeUTC);
  FFieldData.date^[idx] := GFRE_DB.LocalTimeToUTCDB64(AValue);
end;

procedure TFRE_DB_FIELD.SetAsDateTimeUTC(const AValue: TFRE_DB_Datetime64);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_DateTimeUTC) then begin
    FFieldData.FieldType := fdbft_DateTimeUTC;
    New(FFieldData.date);
    SetLength(FFieldData.date^,1);
  end;
  FFieldData.date^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsDateTimeArrayUTC(const AValue: TFRE_DB_DateTimeArray);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_DateTimeUTC) then begin
    FFieldData.FieldType := fdbft_DateTimeUTC;
    New(FFieldData.date);
  end;
  FFieldData.date^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsDateTimeListUTC(idx: Integer; const AValue: TFRE_DB_Datetime64);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_DateTimeUTC) then begin
    New(FFieldData.date);
  end;
  _CheckIndex(idx,fdbft_DateTimeUTC);
  FFieldData.date^[idx] := AValue;
end;



procedure TFRE_DB_FIELD.SetAsDouble(const AValue: Double);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Real64) then begin
    FFieldData.FieldType := fdbft_Real64;
    New(FFieldData.re64);
    SetLength(FFieldData.re64^,1);
  end;
  FFieldData.re64^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsDoubleArray(const AValue: TFRE_DB_Real64Array);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Real64) then begin
    FFieldData.FieldType := fdbft_Real64;
    New(FFieldData.re64);
  end;
  FFieldData.re64^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsDoubleList(idx: Integer; const AValue: Double);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Real64) then begin
    New(FFieldData.re64);
  end;
  _CheckIndex(idx,fdbft_Real64);
  FFieldData.re64^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsGUID(const AValue: TGuid);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_GUID) then begin
    FFieldData.FieldType := fdbft_GUID;
    New(FFieldData.guid);
    SetLength(FFieldData.guid^,1);
  end;
  FFieldData.guid^[0]:=AValue;
  if FIsUidField then
    Fobj.FUID := AValue;
end;

procedure TFRE_DB_FIELD.SetAsGUIDArray(const AValue: TFRE_DB_GUIDArray);
begin
  _InAccessibleCheck;
  _NotAllowedOnUIDFieldCheck;
  if not _CheckStoreType(fdbft_GUID) then begin
    FFieldData.FieldType := fdbft_GUID;
    New(FFieldData.guid);
  end;
  FFieldData.guid^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsGUIDList(idx: Integer; const AValue: TGUID);
begin
  _InAccessibleCheck;
  _NotAllowedOnUIDFieldCheck;
  if not _CheckStoreType(fdbft_GUID) then begin
    New(FFieldData.guid);
  end;
  _CheckIndex(idx,fdbft_GUID);
  FFieldData.guid^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsObjectArray(const AValue: TFRE_DB_ObjectArray);
var  i: Integer;
begin
  _InAccessibleCheck;
  for i:=0 to high(AValue) do begin
    Fobj._ParentCheck(Avalue[i]);
  end;
  if not _CheckStoreType(fdbft_Object) then begin
    FFieldData.FieldType := fdbft_Object;
    New(FFieldData.obj);
  end else begin
    //for i:=0 to high(FFieldData.obj^) do begin
    //  FFieldData.obj^[i].Free;
    //end;
  end;
  FFieldData.obj^ := AValue;
  for i:=0 to high(FFieldData.obj^) do begin
    FFieldData.obj^[i].FParentDBO := self;
  end;
end;

procedure TFRE_DB_FIELD.SetAsObjectList(idx: Integer; const AValue: TFRE_DB_Object);
begin
  _InAccessibleCheck;
  Fobj._ParentCheck(Avalue);
  if not _CheckStoreType(fdbft_Object) then begin
    New(FFieldData.obj);
    _CheckIndex(idx,fdbft_Object);
  end else begin
    _CheckIndex(idx,fdbft_Object);
    //FFieldData.obj^[idx].Free;
  end;
  FFieldData.obj^[idx] := AValue;
  AValue.FParentDBO := self;
end;

procedure TFRE_DB_FIELD.SetAsStream(const AValue: TFRE_DB_Stream);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Stream) then begin
    FFieldData.FieldType := fdbft_Stream;
    New(FFieldData.strm);
    SetLength(FFieldData.strm^,1);
  end;
  FFieldData.strm^[0]:=AValue;
end;

procedure TFRE_DB_FIELD.SetAsStreamArray(const AValue: TFRE_DB_StreamArray);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Stream) then begin
    FFieldData.FieldType := fdbft_Stream;
    New(FFieldData.strm);
  end;
  FFieldData.strm^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsStreamList(idx: Integer; const AValue: TFRE_DB_Stream);
begin
 _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Stream) then begin
    New(FFieldData.strm);
  end;
  _CheckIndex(idx,fdbft_Stream);
  FFieldData.strm^[idx] := AValue;
end;

procedure TFRE_DB_FIELD.SetAsString(const AValue: TFRE_DB_String);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_String) then begin
    FFieldData.FieldType := fdbft_String;
    New(FFieldData.strg);
    SetLength(FFieldData.strg^,1);
  end;
  FFieldData.strg^[0]:=AValue;
end;


procedure TFRE_DB_FIELD.SetAsStringArray(const AValue: TFRE_DB_StringArray);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_String) then begin
    FFieldData.FieldType := fdbft_String;
    New(FFieldData.strg);
  end;
  FFieldData.strg^ := AValue;
end;

procedure TFRE_DB_FIELD.SetAsStringList(idx: Integer; const AValue: TFRE_DB_String);
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_String) then begin
    New(FFieldData.strg);
  end;
  _CheckIndex(idx,fdbft_String);
  FFieldData.strg^[idx] := AValue;
end;


constructor TFRE_DB_FIELD.Create(const obj: TFRE_DB_Object; const FieldType: TFRE_DB_FIELDTYPE);
begin
  FFieldData.FieldType := FieldType;
  Fobj                 := obj;
  //FFieldStreamSize     := -1;
end;

destructor TFRE_DB_FIELD.Destroy;
begin
  _InAccessibleCheck;
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


//function TFRE_DB_FIELD.Exists: Boolean;
//begin
//  result := FFieldData.FieldType<>fdbft_NotFound;
//end;

function TFRE_DB_FIELD.FieldType: TFRE_DB_FIELDTYPE;
begin
  _InAccessibleCheck;
  result := FFieldData.FieldType;
end;

function TFRE_DB_FIELD.FieldTypeAsString: TFRE_DB_String;
begin
  _InAccessibleCheck;
  result := CFRE_DB_FIELDTYPE[FieldType];
end;

function TFRE_DB_FIELD.ValueCount: Integer;
begin
  case FFieldData.FieldType of
    fdbft_NotFound:     result := 0;
    fdbft_CalcField:    result := 1;
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
    fdbft_String:       result := Length(FFieldData.strg^);
    fdbft_Boolean:      result := Length(FFieldData.bool^);
    fdbft_DateTimeUTC:  result := Length(FFieldData.date^);
    fdbft_Stream:       result := Length(FFieldData.strm^);
    fdbft_Object:       result := Length(FFieldData.obj^);
    fdbft_ObjLink:      result := Length(FFieldData.obl^);
    else               raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled %s',[CFRE_DB_FIELDTYPE[FFieldData.FieldType]]);
  end;
end;

function TFRE_DB_FIELD.IsUIDField: boolean;
begin
  _InAccessibleCheck;
  result := FIsUidField;
end;

function TFRE_DB_FIELD.IsObjectField: boolean;
begin
 _InAccessibleCheck;
 result := FieldType=fdbft_Object;
end;

procedure TFRE_DB_FIELD.CloneFromFieldFull(const Field: TFRE_DB_FIELD);
begin
  _InAccessibleCheck;
  Clear;
  if Field.FieldType=fdbft_NotFound then exit; //Empty Fields => Do nothing?
  case field.FieldType of
    fdbft_GUID:        AsGUIDArr         := field.AsGUIDArr;
    fdbft_Byte:        AsByteArr         := field.AsByteArr;
    fdbft_Int16:       AsInt16Arr        := field.AsInt16Arr;
    fdbft_UInt16:      AsUint16Arr       := Field.AsUInt16Arr;
    fdbft_Int32:       AsInt32Arr        := Field.AsInt32Arr;
    fdbft_UInt32:      AsUInt32Arr       := Field.AsUInt32Arr;
    fdbft_Int64:       AsInt64Arr        := Field.AsInt64Arr;
    fdbft_UInt64:      AsUInt64Arr       := Field.AsUInt64Arr;
    fdbft_Real32:      AsReal32Arr       := Field.AsReal32Arr;
    fdbft_Real64:      AsReal64Arr       := Field.AsReal64Arr;
    fdbft_Currency:    AsCurrencyArr     := Field.AsCurrencyArr;
    fdbft_String:      AsStringArr       := Field.AsStringArr;
    fdbft_Boolean:     AsBooleanArr      := Field.AsBooleanArr;
    fdbft_DateTimeUTC: AsDateTimeUTCArr  := Field.AsDateTimeUTCArr;
    fdbft_Stream:      AsStreamArr       := Field.AsStreamArr;
    fdbft_Object:      AsObjectArr       := Field.AsObjectArr;
    fdbft_ObjLink:     AsObjectLinkArray := Field.AsObjectLinkArray;
    fdbft_CalcField:   ;
    else raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled %s (%s)',[CFRE_DB_FIELDTYPE[FFieldData.FieldType],FieldName]);
  end;
end;


procedure TFRE_DB_FIELD.CloneFromField(const Field: TFRE_DB_FIELD);
begin
  _InAccessibleCheck;
  Clear;
  if not _CheckStoreType(Field.FieldType) then begin
    if Field.FieldType=fdbft_NotFound then exit; //Empty Fields => Do nothing?
    FFieldData.FieldType := Field.FieldType;
    case field.FieldType of
      fdbft_GUID: begin
                    New(FFieldData.guid);
                    SetLength(FFieldData.guid^,1);
                    FFieldData.guid^[0] := Field.AsGUID;
                  end;
      fdbft_Byte: begin
                    New(FFieldData.byte);
                    SetLength(FFieldData.byte^,1);
                    FFieldData.byte^[0] := Field.AsByte;
                  end;
      fdbft_Int16: begin
                     New(FFieldData.in16);
                     SetLength(FFieldData.in16^,1);
                     FFieldData.in16^[0] := Field.AsInt16;
                   end;
      fdbft_UInt16: begin
                      New(FFieldData.ui16);
                      SetLength(FFieldData.ui16^,1);
                      FFieldData.ui16^[0] := Field.AsUInt16;
                    end;
      fdbft_Int32: begin
                     New(FFieldData.in32);
                     SetLength(FFieldData.in32^,1);
                     FFieldData.in32^[0] := Field.AsInt32;
                   end;
      fdbft_UInt32: begin
                      New(FFieldData.ui32);
                      SetLength(FFieldData.ui32^,1);
                      FFieldData.ui32^[0] := Field.AsUInt32;
                    end;
      fdbft_Int64: begin
                     New(FFieldData.in64);
                     SetLength(FFieldData.in16^,1);
                     FFieldData.in16^[0] := Field.AsInt64;
                   end;
      fdbft_UInt64: begin
                      New(FFieldData.ui64);
                      SetLength(FFieldData.ui64^,1);
                      FFieldData.ui64^[0] := Field.AsUInt64;
                    end;
      fdbft_Real32: begin
                      New(FFieldData.re32);
                      SetLength(FFieldData.re32^,1);
                      FFieldData.re32^[0] := Field.AsReal32;
                    end;
      fdbft_Real64: begin
                      New(FFieldData.re64);
                      SetLength(FFieldData.re64^,1);
                      FFieldData.re64^[0] := Field.AsReal64;
                    end;
      fdbft_Currency: begin
                        New(FFieldData.curr);
                        SetLength(FFieldData.curr^,1);
                        FFieldData.curr^[0] := Field.AsCurrency;
                      end;
      fdbft_String: begin
                      New(FFieldData.strg);
                      SetLength(FFieldData.strg^,1);
                      FFieldData.strg^[0] := Field.AsString;
                    end;
      fdbft_Boolean: begin
                       New(FFieldData.bool);
                       SetLength(FFieldData.bool^,1);
                       FFieldData.bool^[0] := Field.AsBoolean;
                     end;
      fdbft_DateTimeUTC: begin
                           New(FFieldData.date);
                           SetLength(FFieldData.date^,1);
                           FFieldData.date^[0] := Field.AsDateTimeUTC;
                         end;
      fdbft_Stream: begin
                      New(FFieldData.strm);
                      SetLength(FFieldData.strm^,1);
                      Field.AsStream.Position:=0;
                      FFieldData.strm^[0].CopyFrom(Field.AsStream,0);
                    end;
      fdbft_Object: begin
                      New(FFieldData.obj);
                      SetLength(FFieldData.obj^,1);
                      FFieldData.obj^[0]  := Field.AsObject.CloneToNewObject;
                    end;
      fdbft_ObjLink: begin
                       New(FFieldData.obl);
                       SetLength(FFieldData.obl^,1);
                       FFieldData.obl^[0]  := Field.AsObjectLink;
                     end;
      //FIXXME: Calcfield => String ???
      fdbft_CalcField: begin
                         New(FFieldData.strg);
                         FFieldData.FieldType := fdbft_String;
                         SetLength(FFieldData.strg^,1);
                         FFieldData.strg^[0] := Field.AsString;
                       end;
      else raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled %s (%s)',[CFRE_DB_FIELDTYPE[FFieldData.FieldType],FieldName]);
    end;
  end;
end;

procedure TFRE_DB_FIELD.CloneFromFieldI(const Field: IFRE_DB_FIELD);
begin
  _InAccessibleCheck;
  CloneFromField(Field.Implementor as TFRE_DB_FIELD);
end;

function TFRE_DB_FIELD.AsStringDump: TFRE_DB_String;
begin
  _InAccessibleCheck;
  if ValueCount=1 then begin;
    result := FieldName+' ('+CFRE_DB_FIELDTYPE[FieldType]+') : '+GFRE_DB.StringArray2String(AsStringArr);
  end else begin
    result := FieldName+' ('+CFRE_DB_FIELDTYPE[FieldType]+')['+inttostr(ValueCount)+'] :'+GFRE_DB.StringArray2String(AsStringArr);
  end;
end;

procedure TFRE_DB_FIELD.AddGuid(const value: TGuid);
var A:TFRE_DB_GUIDArray;
    l:Integer;
begin
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_GUID) then begin
    _NotAllowedOnUIDFieldCheck;
    FFieldData.FieldType := fdbft_GUID;
    New(FFieldData.guid);
    SetLength(FFieldData.guid^,1);
    FFieldData.guid^[0] := value;
  end else begin
    _NotAllowedOnUIDFieldCheck;
    A := AsGUIDArr;
    l := Length(a);
    SetLength(A,l+1);
    A[l] := Value;
    AsGUIDArr := A;
  end;
end;

procedure TFRE_DB_FIELD.AddByte(const value: Byte);
var A:TFRE_DB_ByteArray;
    l:Integer;
begin
  _InAccessibleCheck;
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
  _InAccessibleCheck;
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
  _InAccessibleCheck;
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
  _InAccessibleCheck;
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
 _InAccessibleCheck;
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
 _InAccessibleCheck;
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
 _InAccessibleCheck;
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
  _InAccessibleCheck;
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
  _InAccessibleCheck;
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
  _InAccessibleCheck;
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
  _InAccessibleCheck;
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
  _InAccessibleCheck;
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
  _InAccessibleCheck;
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
  _InAccessibleCheck;
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
  _InAccessibleCheck;
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
  _InAccessibleCheck;
  if not _CheckStoreType(fdbft_Object) then begin
    FFieldData.FieldType := fdbft_Object;
    New(FFieldData.obj);
    SetLength(FFieldData.obj^,1);
    FFieldData.obj^[0] := value;
  end else begin
    l := Length(FFieldData.obj^);
    SetLength(FFieldData.obj^,l+1);
    FFieldData.obj^[l] := Value;
  end;
  Value.FParentDBO := self;
end;

procedure TFRE_DB_FIELD.AddObjectLink(const value: TGUID);
var A                   : TFRE_DB_ObjLinkArray;
    l                   : Integer;
    old_array,new_array : TFRE_DB_GUIDArray;
begin
  _InAccessibleCheck;
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
  _InAccessibleCheck;
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
 _InAccessibleCheck;
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
 _InAccessibleCheck;
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
 _InAccessibleCheck;
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
 _InAccessibleCheck;
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
 _InAccessibleCheck;
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
 _InAccessibleCheck;
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
 _InAccessibleCheck;
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
 _InAccessibleCheck;
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
 _InAccessibleCheck;
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
 _InAccessibleCheck;
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
 _InAccessibleCheck;
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
 _InAccessibleCheck;
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
  _InAccessibleCheck;
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
 _InAccessibleCheck;
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
 _InAccessibleCheck;
 _GetHigh(hi);
 SetLength(A,hi);
 k:=0;
 for i:=0 to hi do begin
  if i<>idx then begin
   a[k]:=AsObjectArr[i];
   inc(k);
  end;
 end;
 AsObjectArr:=a;
end;

procedure TFRE_DB_FIELD.RemoveObjectLink(const idx: integer);
var A:TFRE_DB_ObjLinkArray;
    hi:integer;
    i,k:Integer;
begin
  _InAccessibleCheck;
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

procedure TFRE_DB_FIELD.StripObject;
begin
  _InAccessibleCheck;
  _StripObject;
end;


function TFRE_DB_FIELD.GetAsJSON(const without_uid:boolean=false;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil): TJSONData;
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
     fdbft_UInt64:        result:=TJSONInt64Number.Create(GetAsUInt64List(index));                        // transfer of uint64 by JSON not possible
     fdbft_Real32:        result:=TJSONFloatNumber.Create(GetAsSingleList(index));
     fdbft_Real64:        result:=TJSONFloatNumber.Create(GetAsDoubleList(index));
     fdbft_Currency:      result:=TJSONFloatNumber.Create(GetAsCurrencyList(index));
     fdbft_String:        result:=TJSONString.Create(GetAsStringList(index));     //TODO -> think about UTF8 / encoding / decoding
     fdbft_Boolean:       result:=TJSONBoolean.Create(GetAsBooleanList(index));
     fdbft_DateTimeUTC:   result:=TJSONInt64Number.Create(GetAsDateTimeListUTC(index));
     fdbft_Stream:        begin
                            result := HandleStream(FFieldData.strm^[index].Memory,FFieldData.strm^[index].Size,FFieldName^,FFieldData.strm^[index]);
                          end;
     fdbft_Object:        begin
                            oa:=GetAsObjectList(index);
                            result:=oa.GetAsJSON(without_uid,full_dump,stream_cb);
                          end;
     fdbft_ObjLink:     result:=TJSONString.Create(_ConvertToString(index));
     fdbft_CalcField:   result:=TJSONString.Create('-');
     else               raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled '+CFRE_DB_FIELDTYPE[FFieldData.FieldType]);
   end;
 end;

 function GetJSON_String(const index:integer):TJSONData;
 var oa          : TFRE_DB_Object;
 begin
   case FFieldData.FieldType of
     fdbft_NotFound:      result:=TJSONNull.Create;
     fdbft_GUID:          result:=TJSONString.Create(GFRE_BT.GUID_2_HexString(GetAsGUIDList(index)));
     fdbft_Byte:          result:=TJSONString.Create(IntTostr(GetAsByteList(index)));
     fdbft_Int16:         result:=TJSONString.Create(IntTostr(GetAsInt16List(index)));
     fdbft_UInt16:        result:=TJSONString.Create(IntTostr(GetAsUInt16List(index)));
     fdbft_Int32:         result:=TJSONString.Create(IntTostr(GetAsInt32List(index)));
     fdbft_UInt32:        result:=TJSONString.Create(IntTostr(GetAsUInt32List(index)));
     fdbft_Int64:         result:=TJSONString.Create(IntTostr(GetAsInt64List(index)));
     fdbft_UInt64:        result:=TJSONString.Create(IntTostr(GetAsUInt64List(index)));                        // transfer of uint64 by JSON not possible
     fdbft_Real32:        result:=TJSONString.Create(FloatToStr(GetAsSingleList(index)));
     fdbft_Real64:        result:=TJSONString.Create(FloatToStr(GetAsDoubleList(index)));
     fdbft_Currency:      result:=TJSONString.Create(CurrToStr(GetAsCurrencyList(index)));
     fdbft_String:        result:=TJSONString.Create(GetAsStringList(index));     //TODO -> think about UTF8 / encoding / decoding
     fdbft_Boolean:       result:=TJSONString.Create(BoolToStr(GetAsBooleanList(index)));
     fdbft_DateTimeUTC:   result:=TJSONString.Create(IntTostr(GetAsDateTimeListUTC(index)));
     fdbft_Stream:        begin
                            result := HandleStream(FFieldData.strm^[index].Memory,FFieldData.strm^[index].Size,FFieldName^,FFieldData.strm^[index]);
                          end;
     fdbft_Object:        begin
                            oa:=GetAsObjectList(index);
                            result:=oa.GetAsJSON(without_uid,full_dump,stream_cb);
                          end;
     fdbft_ObjLink:     result:=TJSONString.Create(_ConvertToString(index));
     fdbft_CalcField:   result:=TJSONString.Create('-');
     else               raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled '+CFRE_DB_FIELDTYPE[FFieldData.FieldType]);
   end;
 end;

 function EncodeField: TJSONData;
 var i:integer;
 begin
   if ValueCount=0 then begin
     result:=TJSONArray.Create;
   end else if ValueCount=1 then begin
     if not full_dump then begin
       result:=GetJSON(0);
     end else begin
       result:=TJSONArray.Create;
       TJSONArray(result).Add(GetJSON_String(0));
     end;
   end else begin
     result:=TJSONArray.Create;
     for i:=0 to ValueCount-1 do begin
       if not full_dump then begin
         TJSONArray(result).Add(GetJSON(i));
       end else begin
         TJSONArray(result).Add(GetJSON_String(i));
       end;
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

procedure TFRE_DB_FIELD.SetFromJSON(const field_type: TFRE_DB_FIELDTYPE; const json_object: TJSONArray;const stream_cb:TFRE_DB_StreamingCallback;const conn:TFRE_DB_BASE_CONNECTION=nil);

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
      tst         : string;

    function JAsString:TFRE_DB_String;
    begin
      result := (jo as TJSONString).AsString;
    end;

  begin
    case Field_Type of
      fdbft_NotFound:      raise EFRE_DB_Exception.Create(edb_INTERNAL,'cannot json/stream create a undefined field');
      fdbft_GUID:          AddGuid(GFRE_BT.HexString_2_GUID(JAsString));
      fdbft_Byte:          AddByte(StrToInt(JAsString));
      fdbft_Int16:         AddInt16(StrToInt(JAsString));
      fdbft_UInt16:        AddUInt16(StrToInt(JAsString));
      fdbft_Int32:         AddInt32(StrToInt(JAsString));
      fdbft_UInt32:        AddUInt32(StrToInt(JAsString));
      fdbft_Int64:         AddUInt32(StrToInt64(JAsString));
      fdbft_UInt64:        AddUInt64(StrToInt64(JAsString));
      fdbft_Real32:        AddReal32(StrToFloat(JAsString));
      fdbft_Real64:        AddReal64(StrToFloat(JAsString));
      fdbft_Currency:      AddCurrency(StrToCurr(JAsString));
      fdbft_String:        AddString(JAsString);
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
                             AddObject(TFRE_DB_Object.CreateInternalStreamingJSON(self,jo as TJSONArray,conn,False,stream_cb));
                           end;
      fdbft_ObjLink:       AddObjectLink(GFRE_BT.HexString_2_GUID(JAsString));
      fdbft_CalcField:     ;
      else               raise EFRE_DB_Exception.Create(edb_INTERNAL,'not all cases handled '+CFRE_DB_FIELDTYPE[FFieldData.FieldType]);
    end;
  end;

var  i: Integer;
begin
  for i:=0 to json_object.Count-1 do begin
    SetFrom(json_object.Items[i]);
  end;
end;

procedure TFRE_DB_FIELD.Stream2String(var raw_string: TFRE_DB_RawByteString);
begin
  _InAccessibleCheck;
  _CheckFieldType(fdbft_Stream);
  SetLength(raw_string,FFieldData.strm^[0].Size);
  Move(FFieldData.strm^[0].Memory^,raw_string[1],FFieldData.strm^[0].Size);
end;


function TFRE_DB_FIELD.FieldName: TFRE_DB_NameType;
begin
  _InAccessibleCheck;
  result := FFieldName^;
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
    var  i: Integer;
    begin
      if not dont_free_streams_and_objects then
        for i:=0 to high(FFieldData.obj^) do begin
          FFieldData.obj^[i].Free;
        end;
      Dispose(FFieldData.obj);
    end;
begin
  _InAccessibleCheck;
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
    fdbft_String:       Dispose(FFieldData.strg);
    fdbft_Boolean:      Dispose(FFieldData.bool);
    fdbft_DateTimeUTC:  Dispose(FFieldData.date);
    fdbft_Stream:       DisposeStream;
    fdbft_Object:       DisposeObject;
    fdbft_ObjLink:      Dispose(FFieldData.obj);
  end;
  FFieldData.FieldType := fdbft_NotFound;
  FFieldData.bool      := nil;
end;

procedure TFRE_DB_FIELD.RemoveIndex(const idx: integer);
var hi:integer;
begin
  _InAccessibleCheck;
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
      fdbft_CalcField:     exit;
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
  if f1.FieldType<>f2.FieldType then raise EFRE_DB_Exception.Create(edb_FIELDMISMATCH,'ORDERFIELDCOMPARE GUIDS= %s / %s  FIELD=%s  TYPES = %s / %s',[GFRE_BT.GUID_2_HexString(o1.UID),GFRE_BT.GUID_2_HexString(o2.UID),ofieldname,CFRE_DB_FIELDTYPE[f1.FieldType],CFRE_DB_FIELDTYPE[f2.FieldType]]);
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

function TFRE_DB_FIELD.GetAsObjectLinkArrayObjI: IFRE_DB_ObjectArray;
begin
  GFRE_DB.ConstructObjectArrayOI(GetAsObjectLinkArrayObj);
end;

procedure TFRE_DB_FIELD.SetAsObjectLinkArrayObjI(const arr: IFRE_DB_ObjectArray);
begin
  SetAsObjectLinkArrayObj(GFRE_DB.ConstructObjectArrayIO(arr));
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
  _InAccessibleCheck;
  result := Fobj;
end;



function TFRE_DB_USER.GetLogin: TFRE_DB_String;
begin
  result := Field('login').AsString;
end;

function TFRE_DB_USER.GetUGA: TFRE_DB_StringArray;
var syscon       : TFRE_DB_SYSTEM_CONNECTION;
    i            : integer;
    lGroupIDs    : TFRE_DB_ObjLinkArray;
    lGroup       : TFRE_DB_GROUP;
 begin
   syscon   := _DBConnectionBC as TFRE_DB_SYSTEM_CONNECTION;
   lGroupIDs := UserGroupIDs;
   SetLength(result,Length(lGroupIDs));
   for i  := 0 to high(lGroupIDs) do begin
     if not syscon._FetchGroupbyID(lGroupIDs[i],lGroup) then begin
       raise EFRE_DB_Exception.Create('Could not fetch group by id '+GFRE_BT.GUID_2_HexString(lGroupIDs[i]));
     end else begin
       result[i] := lGroup.Fullname;
     end;
   end;
end;

function TFRE_DB_USER.GetDomain: TFRE_DB_NameType;
var syscon       : TFRE_DB_SYSTEM_CONNECTION;
    lDomain      : TFRE_DB_DOMAIN;
begin
  syscon   := _DBConnectionBC as TFRE_DB_SYSTEM_CONNECTION;
  if not syscon._FetchDomainbyID(DomainID,lDomain) then begin
    raise EFRE_DB_Exception.Create('Could not fetch domain by id '+GFRE_BT.GUID_2_HexString(DomainID));
  end else begin
    result := lDomain.GetName;
  end;
end;

function TFRE_DB_USER.GetDomainID: TGUID;
begin
  if fieldExists('domainid') then
    result := Field('domainid').AsObjectLink
  else
    result := GUID_NULL;
end;

function TFRE_DB_USER.GetFirstName: TFRE_DB_String;
begin
  result := Field('firstname').AsString;
end;

function TFRE_DB_USER.GetGIDA: TFRE_DB_ObjLinkArray;
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

procedure TFRE_DB_USER.SetLastName(const AValue: TFRE_DB_String);
begin
  Field('lastname').AsString := AValue;
end;

procedure TFRE_DB_USER.Setlogin(const AValue: TFRE_DB_String);
begin
  Field('login').AsString := AValue;
  _UpdateDomainLoginKey;
end;

procedure TFRE_DB_USER.SetDomainID(AValue: TGUID);
begin
  Field('domainid').AsObjectLink := AValue;
  _UpdateDomainLoginKey;
end;

procedure TFRE_DB_USER.SetDomain(const domainname: TFRE_DB_NameType);
var syscon       : TFRE_DB_SYSTEM_CONNECTION;
    l_domainid   : TGUID;
begin
  syscon     := _DBConnectionBC as TFRE_DB_SYSTEM_CONNECTION;
  l_domainid := syscon._DomainID(domainname);
  if l_domainid=GUID_NULL then begin
    raise EFRE_DB_Exception.Create('Could not fetch domain by name '+domainname);
  end else begin
    SetDomainID(l_domainid);
  end;
end;

procedure TFRE_DB_USER._UpdateDomainLoginKey;
begin
  field('domainloginkey').AsString := GetDomainLoginKey(login,domainid);
end;

procedure TFRE_DB_USER.InternalSetup;
var
  scheme: IFRE_DB_SCHEMEOBJECT;
begin
  inherited InternalSetup;
end;

function TFRE_DB_USER.SubFormattedDisplayAvailable: boolean;
begin
  result := true;
end;

function TFRE_DB_USER.GetSubFormattedDisplay(indent: integer): TFRE_DB_String;
begin
  Result := StringOfChar(' ',indent)+GFRE_DB.StringArray2String(UserGroupNames);
end;

procedure TFRE_DB_USER.SetImage(const image_stream: TFRE_DB_Stream);
begin
  Field('picture').AsStream := image_stream;
end;

procedure TFRE_DB_USER.InitData(const nlogin, nfirst, nlast, npasswd: TFRE_DB_String);
begin
  Login          := nlogin;
  Firstname      := nfirst;
  Lastname       := nlast;
  SetPassword(npasswd);
end;

procedure TFRE_DB_USER.SetPassword(const pw: TFRE_DB_String);
begin
  Field('passwordMD5').AsString:=GFRE_BT.HashString_MD5_HEX(pw);
end;

function TFRE_DB_USER.Checkpassword(const pw: TFRE_DB_String): boolean;
begin
  result :=  Field('passwordMD5').AsString=GFRE_BT.HashString_MD5_HEX(pw);
end;

function TFRE_DB_USER.GetRightsArray: TFRE_DB_StringArray;
begin
  result := (_DBConnectionBC as TFRE_DB_SYSTEM_CONNECTION).GetRightsArrayForGroups(UserGroupIDs);
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
  scheme.AddSchemeField('passwordMD5',fdbft_String).SetupFieldDef(true,false,'','',true,true);
  scheme.AddSchemeField('usergroupids',fdbft_ObjLink).SetupFieldDef(false,true);
  scheme.AddSchemeField('domainid',fdbft_ObjLink).SetupFieldDef(true,false);
  scheme.AddSchemeField('domainloginkey',fdbft_String).SetupFieldDef(true,false);

  field_def := scheme.AddSchemeField('picture',fdbft_Stream);
   params:=TFRE_DB_Object.Create;
   params.Field('width').AsInt16:=70;
   params.Field('height').AsInt16:=90;
   params.Field('absolute').AsBoolean:=true;
  field_def.SetupFieldDef(false,false,'','image',false,false,params);

  scheme.AddSchemeFieldSubscheme('desc','TFRE_DB_TEXT');
  Scheme.SetSysDisplayField(GFRE_DB.ConstructStringArray(['login','firstname','lastname']),'%s - (%s %s)');

  input_group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_USER_user_group');
  input_group.AddInput('login','$scheme_TFRE_DB_USER_login');
  input_group.AddInput('firstname','$scheme_TFRE_DB_USER_firstname');
  input_group.AddInput('lastname','$scheme_TFRE_DB_USER_lastname');
  input_group.AddInput('passwordMD5','$scheme_TFRE_DB_USER_passwordMD5');
  input_group:=scheme.AddInputGroup('domain').Setup('$scheme_TFRE_DB_USER_domain_group');
  input_group.AddInput('domainid','$scheme_TFRE_DB_USER_domainid',false,false,'$SDC:USERMOD_DOMAINS'); // HACK: Fix Domain Name with session prefix
  input_group:=scheme.AddInputGroup('descr').Setup('$scheme_TFRE_DB_USER_descr_group');
  input_group.UseInputGroup('TFRE_DB_TEXT','main','desc');
  input_group:=scheme.AddInputGroup('picture').Setup('$scheme_TFRE_DB_USER_picture_group');
  input_group.AddInput('picture','',false,false,'');
end;

class function TFRE_DB_USER.GetDomainLoginKey(const loginpart: TFRE_DB_String; const domain_id: TGUID): TFRE_DB_String;
begin
  result := GFRE_BT.GUID_2_HexString(domain_id)+'@'+lowercase(loginpart);
end;

class function TFRE_DB_USER.IMC_NewUserOperation(const input: IFRE_DB_Object): IFRE_DB_Object;
var dbo              : IFRE_DB_Object;
    data             : IFRE_DB_Object;
    res              : TFRE_DB_Errortype;
    dbc              : TFRE_DB_CONNECTION;
    loginf,pw,pwc,
    fn,ln            : String;
    dn               : TFRE_DB_NameType;
    obj              : IFRE_DB_DOMAIN;
begin
 writeln('------NEW USER');
 writeln(input.DumpToString);
 data    := input.Field('DATA').asobject;
 dbc     := input.GetReference as TFRE_DB_CONNECTION;

 loginf  := data.Field('login').AsString;
 pw      := data.Field('PASSWORDMD5').AsString;
 pwc     := data.Field('PASSWORDMD5_CONFIRM').AsString;
 fn      := data.Field('firstname').AsString;
 ln      := data.field('lastname').AsString;

 dbc.FetchDomainById(GFRE_BT.HexString_2_GUID(data.field('domainid').AsString),obj);
 dn := obj.GetName;
 writeln('dn:',dn);
 if pw<>pwc then
   exit(TFRE_DB_MESSAGE_DESC.create.Describe('TRANSLATE: Error','TRANSLATE: Password confirm mismatch',fdbmt_error,nil));
 res := dbc.AddUser(loginf+'@'+dn,pw,fn,ln);
 if res=edb_OK then
   exit(TFRE_DB_CLOSE_DIALOG_DESC.create.Describe())
 else
   exit(TFRE_DB_MESSAGE_DESC.create.Describe('TRANSLATE: ERRORK','TRANSLATE: Creation failed '+CFRE_DB_Errortype[res],fdbmt_error,nil));
end;

function TFRE_DB_USER.IMI_SAVEOPERATION(const input: IFRE_DB_Object): IFRE_DB_Object;
var res    : TFRE_DB_Errortype;
begin
  res    := edb_INTERNAL;
  result := TFRE_DB_MESSAGE_DESC.create.Describe('TRANSLATE: ERROR','TRANSLATE: Modify failed '+CFRE_DB_Errortype[res],fdbmt_error,nil)
end;


//function TFRE_DB_USER.IMI_CalcALot(const input: TFRE_DB_Object): TFRE_DB_Object;
//begin
//  writeln('*********** IN CALCALOT ***********');
//  result := _DBConnectionBC.NewObject();
//  result.field('SuperData').AsInt32 := random(100000);
//  result.Field(CalcFieldResultKey(fdbft_GUID)).AsGUID := CFRE_DB_NullGUID;
//  result.Field(CalcFieldResultKey(fdbft_Byte)).AsByte := 123;
//  result.Field(CalcFieldResultKey(fdbft_String)).AsString :=  'Calc '+field('login').AsString+'  ; '+inttostr(result.field('SuperData').AsInt32);
//end;

procedure InitMinimal(const nosys:boolean);
begin
 SetMultiByteConversionCodePage(CP_UTF8);
 GFRE_DB  := TFRE_DB.Create;
 GFRE_DBI := GFRE_DB;
 GFRE_DB.RegisterObjectClass(TFRE_DB_Object);
 GFRE_DB.RegisterObjectClass(TFRE_DB_TEXT);
 GFRE_DB.RegisterObjectClass(TFRE_DB_NAMED_OBJECT);
 GFRE_DB.RegisterObjectClass(TFRE_DB_USER);   // Needed because of Calculated Field (GetRoles)
 GFRE_DB.RegisterPrimaryImplementor(TFRE_DB_NAMED_OBJECT,IFRE_DB_NAMED_OBJECT);
 GFRE_DB.RegisterPrimaryImplementor(TFRE_DB_OBJECT,IFRE_DB_OBJECT);
 GFRE_DB.RegisterObjectClass(TFRE_DB_COMMAND);
 //if not nosys then GFRE_DB.RegisterSystemSchemes;
 if not nosys then GFRE_DB.Initialize_System_Objects;
end;

procedure GFRE_DB_Init_Check;
begin
  if not assigned(GFRE_DB) then raise EFRE_DB_Exception.Create(edb_ERROR,'NO DB INTERFACE ASSIGNED / STARTUP PROCEDURE WRONG');
  if not assigned(GFRE_DB_DEFAULT_PS_LAYER) then raise EFRE_DB_Exception.Create(edb_ERROR,'NO DEFAULT PERSISTANCE LAYER ASSIGNED / STARTUP PROCEDURE WRONG');
end;

procedure Init4Server;
var
  validator  : IFRE_DB_ClientFieldValidator;
  params     : TFRE_DB_Object;
begin
 InitMinimal(true);
 GFRE_DB.RegisterObjectClass(TFRE_DB_SchemeObject);
 GFRE_DB.RegisterObjectClass(TFRE_DB_FieldSchemeDefinition);
 //GFRE_DB.RegisterObjectClass(TFRE_DB_UNIQUE_STRINGKEY);
 GFRE_DB.RegisterObjectClass(TFRE_DB_COLLECTION);
 GFRE_DB.RegisterObjectClass(TFRE_DB_DERIVED_COLLECTION);
 GFRE_DB.RegisterObjectClass(TFRE_DB_SCHEME_COLLECTION);
 GFRE_DB.RegisterObjectClass(TFRE_DB_WORKFLOW);
 GFRE_DB.RegisterObjectClass(TFRE_DB_WORKFLOW_STEP);
 GFRE_DB.RegisterObjectClass(TFRE_DB_APPDATA);
 GFRE_DB.RegisterObjectClass(TFRE_DB_RIGHT);
 GFRE_DB.RegisterObjectClass(TFRE_DB_DOMAIN);
 GFRE_DB.RegisterObjectClass(TFRE_DB_GROUP);
 GFRE_DB.RegisterObjectClass(TFRE_DB_ROLE);
 GFRE_DB.RegisterObjectClass(TFRE_DB_RESOURCE_CONTAINER);
 GFRE_DB.RegisterObjectClass(TFRE_DB_RESOURCE);
 GFRE_DB.RegisterObjectClass(TFRE_DB_Enum);
 GFRE_DB.RegisterObjectClass(TFRE_DB_ClientFieldValidator);
 GFRE_DB.RegisterObjectClass(TFRE_DB_InputGroupSchemeDefinition);
 GFRE_DB.RegisterPrimaryImplementor(TFRE_DB_COLLECTION,IFRE_DB_COLLECTION);
 GFRE_DB.RegisterPrimaryImplementor(TFRE_DB_SCHEME_COLLECTION,IFRE_DB_SCHEME_COLLECTION);
 GFRE_DB.RegisterPrimaryImplementor(TFRE_DB_DERIVED_COLLECTION,IFRE_DB_DERIVED_COLLECTION);
 GFRE_DB.RegisterPrimaryImplementor(TFRE_DB_SIMPLE_TRANSFORM,IFRE_DB_SIMPLE_TRANSFORM);
 GFRE_DB.RegisterObjectClassEx(TFRE_DB_APPLICATION);
 GFRE_DB.RegisterObjectClassEx(TFRE_DB_SERVER_FUNC_DESC);

 //TODO: Chris -> Validatoren sind als singletons implementiert, mit key ansprechbar, damit macht konfiguration dzt. keinen sinn
 validator:=GFRE_DBI.NewClientFieldValidator('image').Setup('(.+(\.(?i)(jpg|png|gif|bmp))$)',
                                                     GFRE_DBI.CreateText('$validator_image','Image File Validator'),
                                                     GFRE_DBI.CreateText('$validator_help_image','Please select an image file.'),
                                                     '\d\.\/');
  // Code was in RegisterSystemscheme  ...
 //GetSysClientFieldValidator('image',cfv);
 //params:=TFRE_DB_Object.Create;
 //params.Field('width').AsInt16:=70;
 //params.Field('height').AsInt16:=90;
 //params.Field('absolute').AsBoolean:=true;
 //cfv.setConfigParams(params);

 //Hackfix
 //params:=TFRE_DB_Object.Create;
 //params.Field('width').AsInt16:=70;
 //params.Field('height').AsInt16:=90;
 //params.Field('absolute').AsBoolean:=true;
 //validator.setConfigParams(params);
//

 GFRE_DBI.RegisterSysClientFieldValidator(validator);

 GFRE_DB_NIL_DESC := TFRE_DB_NIL_DESC.create;
 //GFRE_DB.RegisterSystemSchemes;
 GFRE_DB.Initialize_System_Objects;
end;



initialization


finalization
  GFRE_DB.Free;
  GFRE_DB_NIL_DESC.Free;

end.

