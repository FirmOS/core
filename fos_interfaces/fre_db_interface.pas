unit fre_db_interface;

{
(§LIC)
  (c) Autor,Copyright
      Dipl.Ing.- Helmut Hartl, Dipl.Ing.- Franz Schober, Dipl.Ing.- Christian Koch
      FirmOS Business Solutions GmbH
      www.openfirmos.org
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2013, FirmOS Business Solutions GmbH
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

//TFRE_DB_String is a AnsiString with CodePage(UTF8)
//TODO: Testsuite MAtrix for DERIVED COLLECTION FILTERS


interface

uses
  Classes, SysUtils, FRE_SYSTEM,FRE_DB_SYSRIGHT_CONSTANTS,FOS_TOOL_INTERFACES,FOS_INTERLOCKED,
  FOS_REDBLACKTREE_GEN,FRE_APS_INTERFACE,contnrs,
  fpjson;


type
    TFRE_DB_Compresslevel = (db_CLEV_NONE,db_CLEV_FAST,db_CLEV_MAX);

var
    //@ Precalculated GUIDS per DB THREAD & MainThread
    GFRE_DB_MAXGUID_RESERVE_BLOCK  : Integer = 10000;
    //@ Compression Preset for Backup of Objects
    GCFG_DB_BACKUP_COMPRESSLEVEL   : TFRE_DB_Compresslevel = db_CLEV_MAX;
    //@ Number of concurrent Database Backend Threads
    GCFG_DB_BACKEND_THREAD_CNT     : Integer = 1;
    GCFG_RANDOM_BYTES_DEVICE       : String =  '/dev/random';

type

  TFRE_DB_SIZE_TYPE      = integer;
  Int16                  = Smallint;
  Int32                  = Longint;
  UInt16                 = word;
  UInt32                 = longword;
  TGUID_String           = string[sizeof(TGUid)*2];
  TFRE_DB_String         = type AnsiString(CP_UTF8);
  PFRE_DB_String         = ^TFRE_DB_String;
  TFRE_DB_RawByteString  = RawByteString;

  TFRE_DB_LOGCATEGORY    = (dblc_NONE,dblc_PERSITANCE,dblc_DB,dblc_MEMORY,dblc_REFERENCES,dblc_EXCEPTION,dblc_SERVER,dblc_HTTPSRV,dblc_WEBSOCK,dblc_APPLICATION,dblc_SESSION,dblc_FLEXCOM);
  TFRE_DB_Errortype      = (edb_OK,edb_ERROR,edb_ACCESS,edb_RESERVED,edb_NOT_FOUND,edb_DB_NO_SYSTEM,edb_EXISTS,edb_INTERNAL,edb_ALREADY_CONNECTED,edb_NOT_CONNECTED,edb_FIELDMISMATCH,edb_ILLEGALCONVERSION,edb_INDEXOUTOFBOUNDS,edb_STRING2TYPEFAILED,edb_OBJECT_REFERENCED,edb_INVALID_PARAMS,edb_UNSUPPORTED);
  TFRE_DB_STR_FILTERTYPE = (dbft_EXACT,dbft_PART,dbft_STARTPART,dbft_ENDPART);
  TFRE_DB_NUM_FILTERTYPE = (dbnf_EXACT,dbnf_EXACT_NEGATED,dbnf_LESSER,dbnf_LESSER_EQ,dbnf_GREATER,dbnf_GREATER_EQ,dbnf_IN_RANGE_EX_BOUNDS,dbnf_IN_RANGE_WITH_BOUNDS,dbnf_NOT_IN_RANGE_EX_BOUNDS,dbnf_NOT_IN_RANGE_WITH_BOUNDS,dbnf_AllValuesFromFilter,dbnf_OneValueFromFilter,dbnf_NoValueInFilter);
  TFRE_DB_CalcFieldTime  = (cft_Everytime,cft_OnStoreUpdate);
  TFRE_DB_SchemeType     = (dbst_INVALID,dbst_System,dbst_Extension,dbst_DB);
  TFRE_DB_COMMANDTYPE    = (fct_SyncRequest,fct_SyncReply,fct_AsyncRequest,fct_Error);

  EFRE_DB_Exception=class(EFRE_Exception)
    ErrorType : TFRE_DB_Errortype;
    constructor Create(const msg : TFRE_DB_String);
    constructor Create(const et:TFRE_DB_Errortype;msg:TFRE_DB_String='');
    constructor Create(const et:TFRE_DB_Errortype;msg:TFRE_DB_String;params:array of const);
  end;

  TFRE_DB_FIELDTYPE     = (fdbft_NotFound,fdbft_GUID,fdbft_Byte,fdbft_Int16,fdbft_UInt16,fdbft_Int32,fdbft_UInt32,fdbft_Int64,fdbft_UInt64,fdbft_Real32,fdbft_Real64,fdbft_Currency,fdbft_String,fdbft_Boolean,fdbft_DateTimeUTC,fdbft_Stream,fdbft_Object,fdbft_ObjLink,fdbft_CalcField);
  TFRE_DB_DISPLAY_TYPE  = (dt_string,dt_date,dt_number,dt_number_pb,dt_icon,dt_boolean);
  TFRE_DB_MESSAGE_TYPE  = (fdbmt_error,fdbmt_warning,fdbmt_info,fdbmt_confirm);


const
  CFRE_DB_FIELDTYPE       : Array[TFRE_DB_FIELDTYPE]      of String = ('UNSET','GUID','BYTE','INT16','UINT16','INT32','UINT32','INT64','UINT64','REAL32','REAL64','CURRENCY','STRING','BOOLEAN','DATE','STREAM','OBJECT','OBJECTLINK','CALCFIELD');
  CFRE_DB_FIELDTYPE_SHORT : Array[TFRE_DB_FIELDTYPE]      of String = (    '-',   'G',  'U1',   'I2',    'U2',   'S4',    'U4',   'I8',    'U8',    'R4',    'R8',      'CU',    'SS',     'BO',  'DT',    'ST',    'OB',        'LK',       'CF');
  CFRE_DB_Errortype       : Array[TFRE_DB_Errortype]      of String = ('OK','ERROR','ACCESS PROHIBITED','RESERVED','NOT FOUND','SYSTEM DB NOT FOUND','EXISTS','INTERNAL','ALREADY CONNECTED','NOT CONNECTED','FIELDMISMATCH','ILLEGALCONVERSION','INDEXOUTOFBOUNDS','STRING2TYPEFAILED','OBJECT IS REFERENCED','INVALID PARAMETERS','UNSUPPORTED');
  CFRE_DB_STR_FILTERTYPE  : Array[TFRE_DB_STR_FILTERTYPE] of String = ('EX','PA','SP','EP');
  CFRE_DB_NUM_FILTERTYPE  : Array[TFRE_DB_NUM_FILTERTYPE] of String = ('EX','NEX','LE','LEQ','GT','GEQ','REXB','RWIB','NREXB','NRWIB','AVFF','OVFV','NVFV');
  CFRE_DB_LOGCATEGORY     : Array[TFRE_DB_LOGCATEGORY]    of String = ('-','PERSISTANCE','DB','MEMORY','REFLINKS','EXCEPT','SERVER','HTTPSERVER','WEBSOCK','APP','SESSION','FLEXCOM');
  CFRE_DB_COMMANDTYPE     : Array[TFRE_DB_COMMANDTYPE]    of String = ('S','SR','AR','E');
  CFRE_DB_DISPLAY_TYPE    : Array[TFRE_DB_DISPLAY_TYPE]   of string = ('STR','DAT','NUM','PRG','ICO','BOO');
  CFRE_DB_MESSAGE_TYPE    : array [TFRE_DB_MESSAGE_TYPE]  of string = ('msg_error','msg_warning','msg_info','msg_confirm');


  CFRE_DB_EPSILON_DBL                                               = 2.2204460492503131e-016; // Epsiolon for Double Compare (Zero / boolean)
  CFRE_DB_EPSILON_SGL                                               = 1.192092896e-07;         // Epsiolon for Single Compare (Zero / boolean)
  CFRE_DB_SIZE_ENCODING_SIZE                                        = Sizeof(TFRE_DB_SIZE_TYPE);
  CFRE_DB_NullGUID : TGUID                                          = (data1 : 0         ; data2 : $0000 ; Data3 : $0000 ; Data4 : (  0,  0,  0,  0,  0,  0,0,0));
  CFRE_DB_MaxGUID  : TGUID                                          = (data1 : $FFFFFFFF ; data2 : $FFFF ; Data3 : $FFFF ; Data4 : ($FF,$FF,$FF,$FF,$FF,$FF,0,0));

  cFOS_IID_COLLECTION       = 'ID_COL';
  cFOS_IID_DERIVED_COLL     = 'ID_CDC';
  cFOS_IID_SCHEME_COLL      = 'ID_CSC';


type

  { TFRE_DB_Stream }

  TFRE_DB_Stream     = class(TMemoryStream)
  public
    destructor Destroy;override;
  end;

  TFRE_DB_GUIDArray     = Array of TGuid;
  PFRE_DB_GUIDArray     = ^TFRE_DB_GUIDArray;
  TFRE_DB_ByteArray     = Array of Byte;
  PFRE_DB_ByteArray     = ^TFRE_DB_ByteArray;
  TFRE_DB_Int16Array    = Array of Int16;
  PFRE_DB_Int16Array    = ^TFRE_DB_Int16Array;
  TFRE_DB_UInt16Array   = Array of UInt16;
  PFRE_DB_UInt16Array   = ^TFRE_DB_UInt16Array;
  TFRE_DB_Int32Array    = Array of Int32;
  PFRE_DB_Int32Array    = ^TFRE_DB_Int32Array;
  TFRE_DB_UInt32Array   = Array of UInt32;
  PFRE_DB_UInt32Array   = ^TFRE_DB_UInt32Array;
  TFRE_DB_Int64Array    = Array of Int64;
  PFRE_DB_Int64Array    = ^TFRE_DB_Int64Array;
  TFRE_DB_UInt64Array   = Array of UInt64;
  PFRE_DB_UInt64Array   = ^TFRE_DB_UInt64Array;
  TFRE_DB_Real32Array   = Array of Single;
  PFRE_DB_Real32Array   = ^TFRE_DB_Real32Array;
  TFRE_DB_Real64Array   = Array of Double;
  PFRE_DB_Real64Array   = ^TFRE_DB_Real64Array;
  TFRE_DB_CurrencyArray = Array of Currency;
  PFRE_DB_CurrencyArray = ^TFRE_DB_CurrencyArray;
  TFRE_DB_StringArray   = Array of TFRE_DB_String;
  PFRE_DB_StringArray   = ^TFRE_DB_StringArray;
  TFRE_DB_BoolArray     = Array of Boolean;
  PFRE_DB_BoolArray     = ^TFRE_DB_BoolArray;
  TFRE_DB_DateTimeArray = Array of TFRE_DB_DateTime64;
  PFRE_DB_DateTimeArray = ^TFRE_DB_DateTimeArray;
  TFRE_DB_StreamArray   = Array of TFRE_DB_Stream;
  PFRE_DB_StreamArray   = ^TFRE_DB_StreamArray;
  TFRE_DB_ObjLinkArray  = Array of TGuid;
  PFRE_DB_ObjLinkArray  = ^TFRE_DB_ObjLinkArray;

  TFRE_DB_NameType     = String[63]; // Type for named objects (not data of the DB / no unicode and fixed length)
  PFRE_DB_NameType     = ^TFRE_DB_NameType;
  TFRE_DB_NameTypeRL   = string[127]; // LEN + (SCHEMENAME + | + FIEDLNAME) = 128 Byte

  TFRE_DB_CountedGuid=record
    link  : TGuid;
    count : NativeInt;
  end;

  TFRE_DB_ReferencesByField=record
    fieldname : TFRE_DB_NameType;
    linklist  : TFRE_DB_GUIDArray;
  end;

  TFRE_DB_ObjectReferences = array of TFRE_DB_ReferencesByField;

  TFRE_DB_CountedGuidArray = array of TFRE_DB_CountedGuid;

  TFRE_DB_CONTENT_DESC     = class;
  TFRE_DB_SERVER_FUNC_DESC = class;

  IFRE_DB_Object               = interface;
  IFRE_DB_Usersession          = interface;
  IFRE_DB_APPLICATION          = interface;
  IFRE_DB_CONNECTION           = interface;

  IFRE_DB_ClientFieldValidator = interface;

  IFRE_DB_ObjectArray                 = Array of IFRE_DB_Object;
  IFRE_DB_ClientFieldValidatorArray   = Array of IFRE_DB_ClientFieldValidator;

  IFRE_DB_BASE   = interface
    procedure Finalize        ;
    function  Implementor     : TObject;
    function  Implementor_HC  : TObject;
  end;

  IFRE_DB_UID_BASE   = interface(IFRE_DB_BASE)
    function  UID             : TGUID;
  end;


  IFRE_DB_COMMON = interface (IFRE_DB_BASE)
    function  Supports       (const InterfaceSpec:ShortString ; out Intf) : Boolean;
    function  Supports       (const InterfaceSpec:ShortString)            : Boolean;
    procedure IntfCast       (const InterfaceSpec:ShortString ; out Intf) ; // IntfCast throws an Exception if not succesful
  end;

  IFRE_DB_INVOKEABLE = interface(IFRE_DB_COMMON)
    function  MethodExists   (const name:Shortstring):Boolean;
  end;

  TFRE_DB_TRANSFORM_FUNCTION   = procedure(const res_obj:IFRE_DB_Object;const result_hint:TFRE_DB_String; const result_params: array of const) of object;

  IFRE_DB_TEXT   = interface;

  IFRE_DB_Field  = interface(IFRE_DB_BASE)
  //private // ? - only for info, as interfaces dont support private methods
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
    function  GetAsObject        : IFRE_DB_Object;
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
    procedure SetAsObject        (const AValue: IFRE_DB_Object); //
    procedure SetAsStream        (const AValue: TFRE_DB_Stream);
    procedure SetAsString        (const AValue: TFRE_DB_String); //
    procedure SetAsBoolean       (const AValue: Boolean);//
    procedure SetAsObjectLink    (const AValue: TGUID);

    function  GetAsGUIDArray          : TFRE_DB_GUIDArray; //
    function  GetAsByteArray          : TFRE_DB_ByteArray; //
    function  GetAsInt16Array         : TFRE_DB_Int16Array; //
    function  GetAsInt32Array         : TFRE_DB_Int32Array; //
    function  GetAsInt64Array         : TFRE_DB_Int64Array; //
    function  GetAsUInt16Array        : TFRE_DB_UInt16Array; //
    function  GetAsUInt32Array        : TFRE_DB_UInt32Array; //
    function  GetAsUInt64Array        : TFRE_DB_UInt64Array; //
    function  GetAsSingleArray        : TFRE_DB_Real32Array; //
    function  GetAsDoubleArray        : TFRE_DB_Real64Array; //
    function  GetAsDateTimeArray      : TFRE_DB_DateTimeArray; //
    function  GetAsDateTimeArrayUTC   : TFRE_DB_DateTimeArray; //
    function  GetAsCurrencyArray      : TFRE_DB_CurrencyArray; //
    function  GetAsStringArray        : TFRE_DB_StringArray; //
    function  GetAsStreamArray        : TFRE_DB_StreamArray; //
    function  GetAsBooleanArray       : TFRE_DB_BoolArray; //
    function  GetAsObjectArray        : IFRE_DB_ObjectArray; //
    function  GetAsObjectLinkArray    : TFRE_DB_ObjLinkArray;//
    function  GetAsObjectLinkArrayObj : IFRE_DB_ObjectArray;

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
    function  GetAsObjectList        (idx: Integer): IFRE_DB_Object;
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
    procedure SetAsObjectArray       (const AValue: IFRE_DB_ObjectArray);
    procedure SetAsStreamArray       (const AValue: TFRE_DB_StreamArray);
    procedure SetAsStringArray       (const AValue: TFRE_DB_StringArray);
    procedure SetAsBooleanArray      (const AValue: TFRE_DB_BoolArray);
    procedure SetAsObjectLinkArray   (const Avalue: TFRE_DB_ObjLinkArray);
    procedure SetAsObjectLinkArrayObj(const AValue: IFRE_DB_ObjectArray);

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
    procedure SetAsObjectList        (idx: Integer; const AValue: IFRE_DB_Object);
    procedure SetAsStreamList        (idx: Integer; const AValue: TFRE_DB_Stream);
    procedure SetAsStringList        (idx: Integer; const AValue: TFRE_DB_String);
    procedure SetAsBooleanList       (idx: Integer; const AValue: Boolean);
    procedure SetAsObjectLinkList    (idx: Integer; const AValue: TGUID);
  //public info
    function  FieldType                     : TFRE_DB_FIELDTYPE;
    function  FieldTypeAsString             : TFRE_DB_String;
    function  ValueCount                    : Integer;
    function  IsUIDField                    : boolean;
    function  IsObjectField                 : boolean;
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
    property  AsObject                      : IFRE_DB_Object read GetAsObject write SetAsObject;
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
    property  AsObjectArr                   : IFRE_DB_ObjectArray   read GetAsObjectArray write SetAsObjectArray;
    property  AsObjectLinkArray             : TFRE_DB_ObjLinkArray  read GetAsObjectLinkArray write SetAsObjectLinkArray;
    property  AsObjectLinkArrayObjects      : IFRE_DB_ObjectArray   read GetAsObjectLinkArrayObj write SetAsObjectLinkArrayObj;

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
    property  AsObjectItem      [idx:Integer] : IFRE_DB_Object read GetAsObjectList write SetAsObjectList;
    property  AsObjectLinkItem  [idx:Integer] : TGUID read GetAsObjectLinkList write SetAsObjectLinkList;

    procedure   CloneFromField              (const Field:IFRE_DB_FIELD);

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
    procedure AddObject                     (const value : IFRE_DB_Object);
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

    procedure StripObject                   ;
    procedure SetAsEmptyStringArray         ;

    procedure Stream2String                 (var raw_string:TFRE_DB_RawByteString);

    function  AsStringDump                  : TFRE_DB_String;
    function  FieldName                     : TFRE_DB_NameType;
    procedure Clear                         (const dont_free_streams_and_objects:boolean=false);
    procedure RemoveIndex                   (const idx:integer);
    procedure IntfCast                      (const InterfaceSpec:ShortString ; out Intf) ; // Interpret as Object and then -> IntfCast throws an Exception if not succesful
    function  AsDBText                      :IFRE_DB_TEXT;
  end;

  IFRE_DB_SCHEMEOBJECT          = interface;
  IFRE_DB_Enum                  = interface;
  IFRE_DB_COLLECTION            = interface;
  IFRE_DB_FieldSchemeDefinition = interface;
  IFRE_DB_WORKFLOWSTEP          = interface;
  IFRE_DB_WORKFLOW              = interface;

  IFRE_DB_FieldIterator                 = procedure (const obj : IFRE_DB_Field) is nested;
  IFRE_DB_FieldIteratorBrk              = function  (const obj : IFRE_DB_Field):boolean is nested;
  IFRE_DB_Obj_Iterator                  = procedure (const obj : IFRE_DB_Object) is nested;
  IFRE_DB_Obj_IteratorBreak             = function  (const obj : IFRE_DB_Object):Boolean is nested;
  IFRE_DB_Scheme_Iterator               = procedure (const obj : IFRE_DB_SchemeObject) is nested;
  IFRE_DB_SchemeFieldDef_Iterator       = procedure (const obj : IFRE_DB_FieldSchemeDefinition) is nested;
  IFRE_DB_Enum_Iterator                 = procedure (const obj : IFRE_DB_Enum) is nested;
  IFRE_DB_ClientFieldValidator_Iterator = procedure (const obj : IFRE_DB_ClientFieldValidator) is nested;
  IFRE_DB_Coll_Iterator                 = procedure (const coll: IFRE_DB_COLLECTION) is nested;
  IFRE_DB_Workflow_Iterator             = procedure (const wfs : IFRE_DB_WORKFLOW) is nested;

  TFRE_DB_StreamingCallback             = procedure (const uid,fieldname: TFRE_DB_String ; const stream:TFRE_DB_Stream) is nested;
  TFRE_DB_PhaseProgressCallback         = procedure (const phase,cnt,max: integer) is nested;


  TFRE_DB_Guid_Iterator                 = procedure (const obj:TGUID) is nested;

  TFRE_DB_ObjectEx                      = class;

  TFRE_DB_FIELD_EXPRESSION              = function(const field:IFRE_DB_Field):boolean is nested;

  IFRE_DB_InvokeMethod                  = function  (const Input:IFRE_DB_Object):IFRE_DB_Object;
  IFRE_DB_InvokeMethodCallbackObjectEx  = function  (const obj: TFRE_DB_ObjectEx ;  const Input:IFRE_DB_Object):IFRE_DB_Object;

  IFRE_DB_InvokeInstanceMethod = function  (const Input:IFRE_DB_Object):IFRE_DB_Object of object;
  IFRE_DB_WebInstanceMethod    = function  (const Input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object of object;
  IFRE_DB_InvokeClassMethod    = function  (const Input:IFRE_DB_Object):IFRE_DB_Object of object;

  IFRE_DB_Invoke_WF_Method     = procedure (const WF_Step : IFRE_DB_WORKFLOWSTEP) of object;
  IFRE_DB_CS_CALLBACK          = procedure (const Input:IFRE_DB_Object) of Object;
  IFRE_DB_InvokeProcedure      = procedure (const Input:IFRE_DB_Object) of Object;

  TFRE_DB_Object_Properties    = (fop_SYSTEM,fop_READ_ONLY,fop_VOLATILE,fop_STORED_IMMUTABLE);
  TFRE_DB_Object_PropertySet   = set of TFRE_DB_Object_Properties;

   //IFRE_DB_EXTENSION_GRP loosely groups the necessary
   //DB Apps Registery Functions and extension functions, plus dependencies
   //to prepare and query db extensions on startup

  IFRE_DB_EXTENSION_GRP = interface
    function   GetExtensionName                  : TFRE_DB_String;
    procedure  RegisterExtensionAndDependencies  ;
    procedure  InitializeDatabaseForExtension    (const db_name : string ; const user,pass:string);
    procedure  RemoveForExtension                (const db_name : string ; const user,pass:string);
  end;
  //
  IFRE_DB_EXTENSION_Iterator   = procedure (const ext:IFRE_DB_EXTENSION_GRP) is nested;
  IFRE_DB_EXTENSION_RegisterCB = procedure ;
  IFRE_DB_EXTENSION_INITDB_CB  = procedure (const dbname :string; const user,pass:string);
  IFRE_DB_EXTENSION_REMOVE_CB  = procedure (const dbname :string; const user,pass:string);

  //

  { IFRE_DB_EXTENSION_MNGR }
  IFRE_DB_SYS_CONNECTION = interface;

  IFRE_DB_EXTENSION_MNGR = interface
    function  GetExtensionList        : IFOS_STRINGS;
    procedure RegisterExtensions4DB   (const list: IFOS_STRINGS);
    procedure InitDatabase4Extensions (const list: IFOS_STRINGS ; const db:string ; const user,pass:string);
    procedure Remove4Extensions       (const list: IFOS_STRINGS ; const db:string ; const user,pass:string);
    procedure RegisterNewExtension    (const extension_name : String ; const MetaRegistrationFunction : IFRE_DB_EXTENSION_RegisterCB ; const MetaRegisterInitDBFunction : IFRE_DB_EXTENSION_INITDB_CB; const MetaRegisterRemoveFunction : IFRE_DB_EXTENSION_REMOVE_CB);
    procedure Finalize                ;
  end;

  IFRE_DB_Object = interface(IFRE_DB_INVOKEABLE)
   ['IFREDBO']
    procedure       SetReference                       (const obj : TObject);
    function        GetReference                       : TObject;
    function        GetScheme                          : IFRE_DB_SchemeObject;
    function        GetSystemSchemeByName              (const schemename:TFRE_DB_String; var scheme: IFRE_DB_SchemeObject): Boolean;
    function        GetSystemScheme                    (const schemename:TClass; var scheme: IFRE_DB_SchemeObject): Boolean;
    function        GetAsJSON                          (const without_uid:boolean=false;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil): TJSONData;
    function        GetAsJSONString                    (const without_uid:boolean=false;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil):TFRE_DB_String;
    function        CloneToNewObject                   (const create_new_uids:boolean=false): IFRE_DB_Object;
    procedure       ForAllFields                       (const iter:IFRE_DB_FieldIterator);
    procedure       ForAllFieldsBreak                  (const iter:IFRE_DB_FieldIteratorBrk);
    function        UID                                : TGUID;
    function        UID_String                         : TFRE_DB_String;
    function        NeededSize                         : TFRE_DB_SIZE_TYPE;
    function        Parent                             : IFRE_DB_Object;
    function        ParentField                        : IFRE_DB_FIELD;
    function        AsString                           (const without_schemes:boolean=false):TFRE_DB_String;
    function        Field                              (const name:TFRE_DB_String):IFRE_DB_FIELD;
    function        FieldOnlyExistingObj               (const name:TFRE_DB_String):IFRE_DB_Object;
    function        FieldPath                          (const name:TFRE_DB_String;const dont_raise_ex:boolean=false):IFRE_DB_FIELD;
    function        FieldPathExists                    (const name:TFRE_DB_String):Boolean;
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
    function        SubFormattedDisplayAvailable       : boolean;
    function        GetSubFormattedDisplay             (indent:integer=4):TFRE_DB_String;
    function        SchemeClass                        : TFRE_DB_NameType;
    function        IsA                                (const schemename:TFRE_DB_NameType):Boolean;
    function        IsObjectRoot                       : Boolean;
    procedure       SaveToFile                         (const filename:TFRE_DB_String;const without_schemes:boolean=false);
    function        ReferencesObjects                  : Boolean;
    function        ReferencesObjectsFromData          : Boolean;
    function        ReferenceList                      : TFRE_DB_GUIDArray;
    function        ReferenceListFromData              : TFRE_DB_CountedGuidArray;
    function        ReferenceListFromDataNocount       : TFRE_DB_GuidArray;
    function        ReferencesDetailed                 : TFRE_DB_String;
    function        IsReferenced                       : Boolean;
    function        ReferencedByList                   : TFRE_DB_GUIDArray;
    function        ReferencedByList                   (const from_scheme: TFRE_DB_String): TFRE_DB_GUIDArray;
    function        ReferencedByList                   (const from_schemes: TFRE_DB_StringArray): TFRE_DB_GUIDArray;
    function        GetFieldListFilter                 (const field_type:TFRE_DB_FIELDTYPE):TFRE_DB_StringArray;
    function        GetUIDPath                         :TFRE_DB_StringArray;
    function        GetUIDPathUA                       :TFRE_DB_GUIDArray;
    function        GetDBConnection                    :IFRE_DB_Connection;
    function        Invoke                             (const method:TFRE_DB_String;const input:IFRE_DB_Object ; const ses : IFRE_DB_Usersession ; const app : IFRE_DB_APPLICATION ; const conn : IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        Mediator                           : TFRE_DB_ObjectEx;
    procedure       Set_ReadOnly                       ;
    procedure       CopyField                          (const obj:IFRE_DB_Object;const field_name:String);
  end;

  IFRE_DB_TEXT=interface(IFRE_DB_COMMON)
   ['IFREDBTXT']
    function  GetHint:  TFRE_DB_String;
    function  GetLong:  TFRE_DB_String;
    function  Getshort: TFRE_DB_String;
    function  GetTKey:  TFRE_DB_String;
    procedure SetHint(const AValue: TFRE_DB_String);
    procedure Setlong(const AValue: TFRE_DB_String);
    procedure SetShort(const AValue: TFRE_DB_String);
    procedure SetTKey(const AValue: TFRE_DB_String);

    property  LongText         : TFRE_DB_String read GetLong write Setlong;
    property  ShortText        : TFRE_DB_String read Getshort write SetShort;
    property  Hint             : TFRE_DB_String read GetHint write SetHint;
    property  TranslationKey   : TFRE_DB_String read GetTKey write SetTKey;
    procedure SetupText        (const translation_key:TFRE_DB_String;const short_text:TFRE_DB_String;const long_text:TFRE_DB_String='';const hint_text:TFRE_DB_String='');
  end;


  IFRE_DB_COLLECTION=interface(IFRE_DB_COMMON)
    [cFOS_IID_COLLECTION]
    function        UID                 : TGuid;
    function        Count               : QWord;
    function        Exists              (const ouid:TGUID):boolean;
    procedure       ForAll              (const func:IFRE_DB_Obj_Iterator);
    procedure       ForAllBreak         (const func:IFRE_DB_Obj_IteratorBreak);
    function        Remove              (const ouid:TGUID):boolean;
    function        Store               (var   new_obj:IFRE_DB_Object):TFRE_DB_Errortype;
    function        Update              (const dbo:IFRE_DB_Object):TFRE_DB_Errortype;
    function        Fetch               (const ouid:TGUID;out dbo:IFRE_DB_Object): boolean;
    function        CollectionName      : TFRE_DB_NameType;
    function        LinearScan          (const fieldname:TFRE_DB_NameType;const field_expr:TFRE_DB_FIELD_EXPRESSION):IFRE_DB_Object; // Linear Search / Fetch/Semantics/Finalize needed
    function        ItemCount           : Int64;
    function        First               : IFRE_DB_Object;
    function        Last                : IFRE_DB_Object;
    function        GetItem             (const num:uint64):IFRE_DB_Object;
    procedure       ClearCollection     ;
    procedure       StartBlockUpdating  ;
    procedure       FinishBlockUpdating ;
    //Define a basic index according to fieldtype
    function        DefineIndexOnField  (const FieldName:TFRE_DB_NameType;const FieldType:TFRE_DB_FIELDTYPE;const unique:boolean=true; const ignore_content_case:boolean=true;const index_name:TFRE_DB_NameType='def'):TFRE_DB_Errortype;
    function        ExistsIndexed       (const query_value : TFRE_DB_String;const index_name:TFRE_DB_NameType='def'):Boolean; // for the string fieldtype
    function        GetIndexedObj       (const query_value : TFRE_DB_String;out obj:IFRE_DB_Object;const index_name:TFRE_DB_NameType='def'):boolean; // for the string fieldtype
    function        GetIndexedUID       (const query_value : TFRE_DB_String;out obj_uid:TGUID;const index_name:TFRE_DB_NameType='def'):boolean; // for the string fieldtype
    function        RemoveIndexed       (const query_value : TFRE_DB_String;const index_name:TFRE_DB_NameType='def'):boolean; // for the string fieldtype
    procedure       ForceFullUpdateForObservers;
  end;


  IFRE_DB_TRANSFORMOBJECT = interface;

  TFRE_DB_DC_ORDER = record
    order_key   : byte;
    order_field : TFRE_DB_NameType;
    ascending   : boolean;
  end;

  TFRE_DB_DC_STRINGFIELDKEY = record
    filter_key, field_name, value : TFRE_DB_String;
    filtertype                    : TFRE_DB_STR_FILTERTYPE;
    on_transform                  : boolean;
    on_filter_field               : boolean;
  end;

  TFRE_DB_DC_PAGING_INFO = record
    start  : integer;
    count  : integer;
  end;


  TFRE_DB_DC_ORDER_LIST               = array of TFRE_DB_DC_ORDER;
  TFRE_DB_DC_STRINGFIELDKEY_LIST      = array of TFRE_DB_DC_STRINGFIELDKEY;

  TFRE_DB_CHART_TYPE                  = (fdbct_pie,fdbct_column,fdbct_line);
  TFRE_DB_LIVE_CHART_TYPE             = (fdblct_line,fdblct_sampledline);
  TFRE_COLLECTION_DISPLAY_TYPE        = (cdt_Invalid,cdt_Listview,cdt_Treeview,cdt_Chartview);
  TFRE_COLLECTION_GRID_DISPLAY_FLAG   = (cdgf_ShowSearchbox,cdgf_Editable,cdgf_Filter,cdgf_Sortable,cdgf_ColumnResizeable,cdgf_ColumnHideable,cdgf_ColumnDragable,cdgf_Details,cdgf_Children,cdgf_enableMultiselect);
  TFRE_COLLECTION_CHART_DISPLAY_FLAG  = (cdcf_None);
  TFRE_COLLECTION_TREE_DISPLAY_FLAG   = (cdtf_None);
  TFRE_COLLECTION_GRID_DISPLAY_FLAGS  = set of TFRE_COLLECTION_GRID_DISPLAY_FLAG;
  TFRE_COLLECTION_TREE_DISPLAY_FLAGS  = set of TFRE_COLLECTION_TREE_DISPLAY_FLAG;
  TFRE_COLLECTION_CHART_DISPLAY_FLAGS = set of TFRE_COLLECTION_CHART_DISPLAY_FLAG;

  TFRE_DB_UserSession             = class;

  { IFRE_DB_DERIVED_COLLECTION }

  IFRE_DB_DERIVED_COLLECTION=interface(IFRE_DB_COLLECTION)
    [cFOS_IID_DERIVED_COLL]
    procedure  BindSession                   (const session : TFRE_DB_UserSession);
    function   AddOrderField                 (const order_key,field_name:TFRE_DB_String;const ascending : boolean):TFRE_DB_Errortype;
    procedure  RemoveAllOrderFields          ;
    procedure  RemoveAllFilterFields         ;
    procedure  RemoveAllFiltersPrefix        (const prefix:string);
    function   Count                         : QWord;
    //function   FetchFromParent               (const ouid:TGUID;out dbo:IFRE_DB_Object): boolean;

    procedure  BeginUpdateGathering          ;
    procedure  FinishUpdateGathering         (const sendupdates : Boolean);


    //procedure  ApplyToPage                   (const page_info : TFRE_DB_DC_PAGING_INFO;const iterator:IFRE_DB_Obj_Iterator);
    procedure  SetDeriveParent               (const coll:IFRE_DB_COLLECTION;  const idField: String='uid');
    procedure  SetDeriveTransformation       (const tob:IFRE_DB_TRANSFORMOBJECT);
    procedure  SetVirtualMode                (const StringIndexKey : String='');

    //Deliver all Objects which are pointed to by the input "Dependency" object (dependency_object_refers=true),
    //     or all Objects which point to the the input "Dependency" object (dependency_object_refers=false)
    procedure  SetReferentialLinkMode          (const scheme_and_field_constraint : TFRE_DB_String ; const dependency_object_refers : boolean ; const dependency_reference : string = 'uids');
    procedure  SetUseDependencyAsRefLinkFilter (const scheme_and_field_constraint : TFRE_DB_String ; const dependency_object_refers : boolean ; const negate : boolean ; const dependency_reference : string = 'uids');
    procedure  AddVirtualChildEntry            (const caption:TFRE_DB_String; const FuncClass: String; const uidPath: TFRE_DB_StringArray; const ChildrenFunc:String='ChildrenData'; const ContentFunc:String='Content';const MenuFunc:String='Menu');
    function   Derive                          : TFRE_DB_Errortype;

    procedure  SetDisplayType                (const CollectionDisplayType : TFRE_COLLECTION_DISPLAY_TYPE ; const Flags:TFRE_COLLECTION_GRID_DISPLAY_FLAGS;const title:TFRE_DB_String;const TreeNodeCaptionFields:TFRE_DB_StringArray=nil;const TreeNodeIconField:TFRE_DB_String='';const item_menu_func: TFRE_DB_SERVER_FUNC_DESC=nil;const item_details_func: TFRE_DB_SERVER_FUNC_DESC=nil; const grid_item_notification: TFRE_DB_SERVER_FUNC_DESC=nil; const tree_menu_func: TFRE_DB_SERVER_FUNC_DESC=nil; const drop_func: TFRE_DB_SERVER_FUNC_DESC=nil; const drag_func: TFRE_DB_SERVER_FUNC_DESC=nil);
    procedure  SetDisplayTypeChart           (const title: TFRE_DB_String; const chart_type: TFRE_DB_CHART_TYPE; const series_field_names: TFRE_DB_StringArray; const use_series_colors:boolean; const use_series_labels : boolean; const series_labels: TFRE_DB_StringArray=nil; const showLegend: Boolean=false; const maxValue: Integer=0);
    procedure  SetChildToParentLinkField     (const fieldname : TFRE_DB_NameType);
    procedure  SetParentToChildLinkField     (const fieldname : TFRE_DB_NameType);

    function   GetDisplayDescription         : TFRE_DB_CONTENT_DESC;
    function   GetStoreDescription           : TFRE_DB_CONTENT_DESC;
    function   getDescriptionStoreId         : String;
    //@
    function   GetDisplayDescriptionFunction (const FilterEventKey:TFRE_DB_String=''): TFRE_DB_SERVER_FUNC_DESC;
    //@ Set a String Filter, which can be used before or after the transformation
    //@ filterkey = ID of the Filter / field_name : on which field the filter works / filtertype: how the filter works / on_transform : true = work after transformation / on_filter_field : true = filter works on a transform filter field which is not in the output
    function   AddStringFieldFilter    (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_StringArray;const filtertype:TFRE_DB_STR_FILTERTYPE;const on_transform:boolean=true;const on_filter_field:boolean=false):TFRE_DB_Errortype;
    //function   RemoveStringFieldFilter (const filter_key:TFRE_DB_String;const on_transform:boolean):TFRE_DB_Errortype; //deprecated
    function   AddBooleanFieldFilter   (const filter_key,field_name:TFRE_DB_String;const value :Boolean;const on_transform:boolean=true;const on_filter_field:boolean=false):TFRE_DB_Errortype;
    // Add a UID Field Filter | Match types : dbnf_EXACT,dbnf_EXACT_NEGATED,dbnf_AllValuesFromFilter,dbnf_OneValueFromFilter
    // dbnf_EXACT,dbnf_EXACT_NEGATED : the filter values array must match the target object field array in order or negated
    function   AddUIDFieldFilter             (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_GUIDArray     ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddByteFieldFilter            (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_ByteArray     ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddInt16FieldFilter           (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_Int16Array    ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddUInt16FieldFilter          (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_UInt16Array   ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddInt32FieldFilter           (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_Int32Array    ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddUInt32FieldFilter          (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_UInt32Array   ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddInt64FieldFilter           (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_Int64Array    ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddUInt64FieldFilter          (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_UInt64Array   ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddReal32FieldFilter          (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_Real32Array   ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddReal64FieldFilter          (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_Real64Array   ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddCurrencyFieldFilter        (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_CurrencyArray ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddDateTimeFieldFilter        (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_DateTimeArray ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE ;const on_transform:boolean=true ; const on_filter_field:boolean=false):TFRE_DB_Errortype;
    function   AddSchemeFilter               (const filter_key           :TFRE_DB_String;const values:TFRE_DB_StringArray   ;const negate:boolean=false):TFRE_DB_Errortype;
    function   AddRightFilterForEntryAndUser (const filter_key           :TFRE_DB_String;const right_prefix:TFRE_DB_NameType;const fieldname_for_uid : TFRE_DB_NameType=''):TFRE_DB_Errortype;
    //function   AddRefLinkFilter        (const filter_key,field_link_description : TFRE_DB_String ; const the_object_refers : boolean);
    function   IMI_GET_CHILDREN_DATA   (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  IFRE_DB_SCHEME_COLLECTION=interface(IFRE_DB_COLLECTION)
    [cFOS_IID_SCHEME_COLL]
  end;

  IFRE_DB_TRANSFORMOBJECT = interface(IFRE_DB_COMMON)
    ['IFDBTO']
  end;

  IFRE_DB_CUSTOMTRANSFORM = procedure (const conn : IFRE_DB_CONNECTION ; const dependency_input : IFRE_DB_Object; const input_object : IFRE_DB_Object ; const transformed_object : IFRE_DB_Object) of object;

  IFRE_DB_SIMPLE_TRANSFORM=interface(IFRE_DB_TRANSFORMOBJECT)
    ['IFDBST']
    procedure SetCustomTransformFunction     (const func : IFRE_DB_CUSTOMTRANSFORM);
    procedure AddCollectorscheme             (const format:TFRE_DB_String;const in_fieldlist:TFRE_DB_StringArray;const out_field:TFRE_DB_String;const filter_field:boolean=false;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    procedure AddFulltextFilterOnTransformed (const in_fieldlist:TFRE_DB_StringArray);
    procedure AddOneToOnescheme              (const fieldname:TFRE_DB_String;const out_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1;const iconID:String='');
    procedure AddProgressTransform           (const valuefield:TFRE_DB_String;const out_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const textfield:TFRE_DB_String='';const out_text:TFRE_DB_String='';const maxValue:Single=100;const fieldSize: Integer=1);
    procedure AddConstString                 (const out_field,value:TFRE_DB_String;const display: Boolean=false;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    procedure AddDBTextShortToOne            (const fieldname:TFRE_DB_String;const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    procedure AddDBTextLongToOne             (const fieldname:TFRE_DB_String;const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    procedure AddDBTextHintToOne             (const fieldname:TFRE_DB_String;const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    procedure AddDBTextKeyToOne              (const fieldname:TFRE_DB_String;const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    procedure AddFullDumpField               (const fieldname:TFRE_DB_String; const dump_length_max:Integer=0;const filter_field:Boolean=false;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    procedure AddMatchingReferencedField     (const ref_field_chain: TFRE_DB_StringArray;const target_field:TFRE_DB_String;const output_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
    procedure AddMatchingReferencedField     (const ref_field      : TFRE_DB_String     ;const target_field:TFRE_DB_String;const output_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1);
  end;


  IFRE_DB_FieldSchemeDefinition=interface(IFRE_DB_BASE)
    ['IFDBFSD']
    function   GetFieldName        : TFRE_DB_String;
    function   GetFieldType        : TFRE_DB_FIELDTYPE;
    function   GetSubSchemeName    : TFRE_DB_String;
    function   getMultiValues      : Boolean;
    function   getRequired         : Boolean;
    function   getValidator        : IFRE_DB_ClientFieldValidator;
    procedure  setMultiValues      (AValue: Boolean);
    procedure  setRequired         (AValue: Boolean);
    function   getEnum             : IFRE_DB_Enum;

    function   SetupFieldDef     (const is_required:boolean;const is_multivalue:boolean=false;const enum_key:TFRE_DB_String='';const validator_key:TFRE_DB_String='';const is_pass:Boolean=false; const add_confirm:Boolean=false ; const validator_params : IFRE_DB_Object=nil):IFRE_DB_FieldSchemeDefinition;
    procedure  SetCalcMethod     (const calc_methodname:TFRE_DB_String);
//    function   CalcField         (const calc_method:IFRE_DB_InvokeInstanceMethod):IFRE_DB_FIELD;
    function   getDepFields      : IFRE_DB_ObjectArray;
    procedure  addDepField       (const fieldName: TFRE_DB_String;const disablesField: Boolean=true);
    property   FieldName         :TFRE_DB_String read GetFieldName;
    property   FieldType         :TFRE_DB_FIELDTYPE  read GetFieldType;
    property   SubschemeName     :TFRE_DB_String read GetSubSchemeName;
    function   GetSubScheme      :IFRE_DB_SchemeObject;
    property   required          :Boolean read getRequired write setRequired;
    property   multiValues       :Boolean read getMultiValues write setMultiValues;
    property   validator         :IFRE_DB_ClientFieldValidator read getValidator;  //ATTENTION: Validator is a Singleton
    property   enum              :IFRE_DB_Enum read getEnum;  //ATTENTION Enum is a singleton
    function   ValidateField     (const field_to_check:IFRE_DB_FIELD;const raise_exception:boolean=true):boolean;
  end;

  IFRE_DB_NAMED_OBJECT = interface(IFRE_DB_Object)
   ['IFDBNO']
    function  GetDesc         : IFRE_DB_TEXT;
    procedure SetDesc         (const AValue: IFRE_DB_TEXT);
    function  GetName         : TFRE_DB_String;
    procedure SetName         (const AValue: TFRE_DB_String);
    property  ObjectName      : TFRE_DB_String       read GetName write SetName;
    property  Description     : IFRE_DB_TEXT read GetDesc write SetDesc;
  end;

  IFRE_DB_NAMED_OBJECT_PLAIN = interface(IFRE_DB_UID_BASE)
   ['IFDBNO']
    function  GetDesc         : IFRE_DB_TEXT;
    procedure SetDesc         (const AValue: IFRE_DB_TEXT);
    function  GetName         : TFRE_DB_String;
    procedure SetName         (const AValue: TFRE_DB_String);
    property  ObjectName      : TFRE_DB_String       read GetName write SetName;
    property  Description     : IFRE_DB_TEXT read GetDesc write SetDesc;
  end;

  IFRE_DB_Enum=interface(IFRE_DB_NAMED_OBJECT_PLAIN)
    ['IFDBE']
    function  Setup      (const infoText: IFRE_DB_TEXT): IFRE_DB_Enum;
    procedure addEntry   (const value:TFRE_DB_String;const caption: IFRE_DB_TEXT);
    function  getEntries :IFRE_DB_ObjectArray;
    function  CheckField (const field_to_check:IFRE_DB_FIELD;const raise_exception:boolean):boolean;
  end;

  IFRE_DB_RIGHT = interface(IFRE_DB_NAMED_OBJECT_PLAIN)
    ['IFDBRI']
  end;

  { IFRE_DB_COMMAND }

  IFRE_DB_COMMAND        = interface;

  TFRE_DB_COMMAND_STATUS = (cdcs_OK,cdcs_TIMEOUT,cdcs_ERROR);
  TFRE_DB_CONT_HANDLER   = procedure(const DATA : IFRE_DB_Object ; const status:TFRE_DB_COMMAND_STATUS ; const error_txt:string) of object;
  TFRE_DB_CMD_Request    = procedure(const sender:TObject; const CMD  : IFRE_DB_COMMAND) of object;

  IFRE_DB_COMMAND_REQUEST_ANSWER_SC = interface(IFRE_DB_BASE)
   ['IFDBCRASC']
    procedure Send_ServerClient         (const dbc : IFRE_DB_COMMAND);
    procedure DeactivateSessionBinding  ;
    procedure UpdateSessionBinding      (const new_session : TObject);
    function  GetInfoForSessionDesc     : String;
  end;

  IFRE_DB_COMMAND=interface(IFRE_DB_BASE)
    ['IFDBCMD']
    function     GetCommandID     : UInt64;
    function     GetCType         : TFRE_DB_COMMANDTYPE;
    function     GetEText         : TFRE_DB_String;
    function     GetFatalClose    : Boolean;
    function     GetChangeSessionKey    : String;
    procedure    SetChangeSessionKey    (AValue: String);
    procedure    SetFatalClose    (AValue: Boolean);
    procedure    SetEText         (AValue: TFRE_DB_String);
    procedure    SetAnswerInterface (const answer_interface : IFRE_DB_COMMAND_REQUEST_ANSWER_SC); //
    function     GetAnswerInterface :IFRE_DB_COMMAND_REQUEST_ANSWER_SC;
    procedure    SetCType         (AValue: TFRE_DB_COMMANDTYPE);
    function     GetData          : IFRE_DB_Object;
    //procedure    SetIfCmd         (AValue: Boolean);
    function     GetUidPath       : TFRE_DB_GUIDArray;
    //function     GetIfCmd         : Boolean;
    procedure    SetUidPath       (AValue: TFRE_DB_GUIDArray);
    function     GetIsAnswer      : Boolean;
    function     GetIsClient      : Boolean;
    procedure    SetCommandID     (const AValue: UInt64);
    procedure    SetData          (const AValue: IFRE_DB_Object);
    function     GetInvokeClass   : String;
    function     GetInvokeMethod  : String;
    procedure    SetInvokeClass   (AValue: String);
    procedure    SetInvokeMethod  (AValue: String);
    procedure    SetIsAnswer      (const AValue: Boolean);
    procedure    SetIsClient      (const AValue: Boolean);
    function     NeededSize       : TFRE_DB_SIZE_TYPE;
    procedure    CopyToMemory     (memory : Pointer);
    procedure    StripOwnedObjects;
    property     Answer        : Boolean read   GetIsAnswer     write SetIsAnswer;
    property     ClientCommand : Boolean read   GetIsClient     write SetIsClient;
    function     CheckoutData  : IFRE_DB_Object;
    function     AsJSONString  : TFRE_DB_RawByteString;
    function     AsDBODump     : TFRE_DB_RawByteString;
    //public
    property     CommandID     : UInt64              read GetCommandID    write SetCommandID;
    property     InvokeClass   : String              read GetInvokeClass  write SetInvokeClass;
    property     InvokeMethod  : String              read GetInvokeMethod write SetInvokeMethod;
    property     Data          : IFRE_DB_Object      read GetData         write SetData;
    property     UidPath       : TFRE_DB_GUIDArray   read GetUidPath      write SetUidPath;
    //property     InterfaceCmd  : Boolean             read GetIfCmd        write SetIfCmd;
    property     CommandType   : TFRE_DB_COMMANDTYPE read GetCType        write SetCType;
    property     ErrorText     : TFRE_DB_String      read GetEText        write SetEText;
    property     FatalClose    : Boolean             read GetFatalClose   write SetFatalClose;
    property     ChangeSession : String              read GetChangeSessionKey   write SetChangeSessionKey;
  end;

  { IFRE_DB_APPDATA }

  IFRE_DB_APPDATA = interface(IFRE_DB_NAMED_OBJECT_PLAIN)
    ['IFDBAPPD']
    function     GetVersion: TFRE_DB_String;
    procedure    SetVersion(AValue: TFRE_DB_String);
    property     Version       : TFRE_DB_String      read GetVersion      write SetVersion;
  end;

  IFRE_DB_ROLE = interface(IFRE_DB_NAMED_OBJECT_PLAIN)
    ['IFDBRIGR']
    function  GetDomain                    : TFRE_DB_NameType;
    procedure AddRight                     (const right:IFRE_DB_RIGHT);
    function  GetRightNames                : TFRE_DB_StringArray;
    property  Domain                       : TFRE_DB_NameType read GetDomain;
  end;

  { IFRE_DB_USER }

  IFRE_DB_USER=interface(IFRE_DB_BASE)
    ['IFDBUSER']
    function  GetFirstName: TFRE_DB_String;
    function  GetLastName: TFRE_DB_String;
    function  GetLogin: TFRE_DB_String;
    function  GetDomain: TFRE_DB_NameType;
    function  GetUGA: TFRE_DB_StringArray;
    procedure SetFirstName(const AValue: TFRE_DB_String);
    procedure SetLastName(const AValue: TFRE_DB_String);
    procedure Setlogin(const AValue: TFRE_DB_String);

    procedure InitData           (const nlogin,nfirst,nlast,npasswd:TFRE_DB_String);
    property  Login              :TFRE_DB_String read GetLogin write Setlogin;
    property  Firstname          :TFRE_DB_String read GetFirstName write SetFirstName;
    property  Lastname           :TFRE_DB_String read GetLastName write SetLastName;
    property  UserGroupNames     :TFRE_DB_StringArray read GetUGA;
    procedure SetPassword        (const pw:TFRE_DB_String);
    function  Checkpassword      (const pw:TFRE_DB_String):boolean;
    function  GetRightsArray     : TFRE_DB_StringArray;
    property  Domain             : TFRE_DB_NameType read GetDomain;
  end;


  IFRE_DB_DOMAIN=interface(IFRE_DB_NAMED_OBJECT_PLAIN)
    ['IFDBUSERDOMAIN']
  end;


  IFRE_DB_GROUP=interface(IFRE_DB_NAMED_OBJECT_PLAIN)
    ['IFDBUSERGRP']
    function  GetRoleNames                 : TFRE_DB_StringArray;
    function  GetDomain                    : TFRE_DB_NameType;
    function  AddUserToGroup               (const user :IFRE_DB_USER):TFRE_DB_Errortype;
    function  RemoveUserFromGroup          (const user :IFRE_DB_USER):TFRE_DB_Errortype;
    property  RoleNames                    : TFRE_DB_StringArray read GetRoleNames;
    property  Domain                       : TFRE_DB_NameType read GetDomain;
  end;



  IFRE_DB_ClientFieldValidator       = interface(IFRE_DB_NAMED_OBJECT_PLAIN)
    ['IFDBCV']
    function  getRegExp       :TFRE_DB_String;
    function  getInfoText     :IFRE_DB_TEXT;
    function  getHelpText     :IFRE_DB_TEXT;
    function  getAllowedChars :TFRE_DB_String;
    function  Setup           (const regExp:TFRE_DB_String; const infoText: IFRE_DB_TEXT; const helpText: IFRE_DB_TEXT=nil; const allowedChars:TFRE_DB_String=''): IFRE_DB_ClientFieldValidator;
    //procedure setConfigParams (const params:IFRE_DB_Object);
    //function  getConfigParams :IFRE_DB_Object;
  end;

  IFRE_DB_InputGroupSchemeDefinition = interface;

  { IFRE_DB_SCHEMEOBJECT }

  IFRE_DB_SCHEMEOBJECT=interface(IFRE_DB_COMMON)
   ['IFDBSO']
    function  GetExplanation              :TFRE_DB_String;
    procedure SetExplanation              (AValue: TFRE_DB_String);
    function  Implementor                 : TObject;
    function  GetAll_IMI_Methods          :TFRE_DB_StringArray;
    function  MethodExists                (const name:TFRE_DB_String):boolean;
    function  AddSchemeField              (const newfieldname:TFRE_DB_String ; const newfieldtype:TFRE_DB_FIELDTYPE):IFRE_DB_FieldSchemeDefinition;
    procedure RemoveSchemeField           (const fieldname:TFRE_DB_String);
    procedure AddCalculatedField          (const newfieldname,calc_method_name:TFRE_DB_String;const calculation_type:TFRE_DB_CalcFieldTime);
    function  AddSchemeFieldSubscheme     (const newfieldname:TFRE_DB_String ; const sub_scheme:TFRE_DB_String):IFRE_DB_FieldSchemeDefinition;
    function  GetSchemeField              (const fieldname   :TFRE_DB_String ; var fieldschemedef:IFRE_DB_FieldSchemeDefinition): boolean;
    function  GetSchemeField              (const fieldname   :TFRE_DB_String): IFRE_DB_FieldSchemeDefinition;
    function  IsA                         (const schemename :TFRE_DB_String):Boolean;
    function  AddUniqueKey                (const KeyName     :TFRE_DB_String ; const FieldNames:TFRE_DB_StringArray):TFRE_DB_Errortype;
    procedure SetSimpleSysDisplayField    (const field_name  :TFRE_DB_String);
    procedure SetSysDisplayField          (const field_names :TFRE_DB_StringArray;const format:TFRE_DB_String);
    function  GetFormattedDisplay         (const obj : IFRE_DB_Object):TFRE_DB_String;
    function  FormattedDisplayAvailable   (const obj : IFRE_DB_Object):boolean;
    function  DefinedSchemeName           : TFRE_DB_String;
    procedure Strict                      (const only_defined_fields:boolean);
    procedure SetParentSchemeByName       (const parentschemename:TFRE_DB_String);
    procedure RemoveParentScheme          ;
    function  GetParentScheme             :IFRE_DB_SchemeObject;
    function  GetParentSchemeName         :TFRE_DB_String;
    procedure SetObjectFieldsWithScheme   (const Raw_Object: IFRE_DB_OBject; const Update_Object: IFRE_DB_Object;const new_object:boolean;const DBConnection:IFRE_DB_CONNECTION;const schemeType: TFRE_DB_String='');
    //@ Defines new InputGroup, to use with IMI_CONTENT
    function  AddInputGroup               (const id: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;
    function  GetInputGroup                (const name: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;
    function  GetSchemeType                : TFRE_DB_SchemeType;
    function  ValidateObject               (const dbo : IFRE_DB_Object;const raise_errors:boolean=true):boolean;
    function  getSchemeFields              :IFRE_DB_Object;
    //function  UpdateSchemeField            (const oldField:IFRE_DB_FieldSchemeDefinition; const newfieldname:TFRE_DB_String ; const newfieldtype:TFRE_DB_FIELDTYPE):IFRE_DB_FieldSchemeDefinition; RETHINK
    function  InvokeMethod_UID             (const suid : TGUID;const methodname:TFRE_DB_String;const input:IFRE_DB_Object;const connection:IFRE_DB_CONNECTION):IFRE_DB_Object;
    procedure ForAllFieldSchemeDefinitions (const iterator:IFRE_DB_SchemeFieldDef_Iterator);
    property  Explanation:TFRE_DB_String read GetExplanation write SetExplanation;
  end;

  TFRE_DB_APPLICATION       = Class;
  TFRE_DB_APPLICATIONCLASS  = Class of TFRE_DB_APPLICATION;

  IFRE_DB_APPLICATION_ARRAY = array of IFRE_DB_APPLICATION;
  TFRE_DB_APPLICATION_ARRAY = array of TFRE_DB_APPLICATION;

  // Transactions are executed fully serial in memory, A,C,I
  // durability is enhanced with a WAL File D
  // OLAP     = READ ONLY, long running, on consistent fork (Not implemented)
  // OLTP     = Read,Write Short Transactions
  // Implicit = No CT applied on a Perslayer Store,Update,Delete

  TFRE_DB_TRANSACTION_TYPE=(dbt_Implicit,dbt_OLTP,dbt_OLAP);

  IFRE_DB_TRANSACTION=interface
    function  GetType  : boolean;
    procedure Rollback;
    procedure Commit;
  end;



  { IFRE_DB_CONNECTION }

  IFRE_DB_CONNECTION=interface(IFRE_DB_BASE)
  ['IFDB_CONN']
    function    GetDatabaseName           : TFRE_DB_String;
    procedure   AssociateObject           (const Obj:IFRE_DB_Object);
    function    Connect                   (const db,user,pass:TFRE_DB_String):TFRE_DB_Errortype;
    function    CheckLogin                (const user,pass:TFRE_DB_String):TFRE_DB_Errortype;
    function    CollectionExists          (const name:TFRE_DB_NameType):boolean;
    function    DeleteCollection          (const name:TFRE_DB_NameType):TFRE_DB_Errortype;

    //function    GetScheme                 (const scheme_name: TFRE_DB_NameType; var scheme: IFRE_DB_SchemeObject): boolean ; deprecated
    //function    NewScheme                 (const Scheme_Name: TFRE_DB_String;const parent_scheme_name:TFRE_DB_String='') : IFRE_DB_SchemeObject;
    //function    StoreScheme               (var   obj: IFRE_DB_SchemeObject)  : TFRE_DB_Errortype;
    //function    UpdateScheme              (const scheme : IFRE_DB_SCHEMEOBJECT)               : TFRE_DB_Errortype;
    //function    RemoveSchemeByName           (const scheme_name:TFRE_DB_String)                                               : boolean;
    //function    SchemeExists              (const scheme_name: TFRE_DB_String): boolean;
    //function    GetSchemeCollection       :IFRE_DB_SCHEME_COLLECTION;

    function    GetClientFieldValidator   (const val_name   :TFRE_DB_String;out validator:IFRE_DB_ClientFieldValidator) : boolean;
    function    GetEnum                   (const enum_name  :TFRE_DB_String;out enum:IFRE_DB_Enum)                      : boolean;

    function    NewObject                 (const Scheme:TFRE_DB_String=''): IFRE_DB_Object;
    function    Delete                    (const ouid: TGUID): TFRE_DB_Errortype;

    function    Fetch                     (const ouid:TGUID;out dbo:IFRE_DB_Object)         : boolean;
    function    FetchAsIntf               (const ouid:TGUID;const IntfSpec:ShortString; out Intf) : boolean;
    function    Update                    (const dbo:IFRE_DB_OBJECT)                          : TFRE_DB_Errortype;


    function    Collection                (const collection_name: TFRE_DB_NameType;const create_non_existing:boolean=true;const in_memory:boolean=false)  : IFRE_DB_COLLECTION;
    function    CollectionAsIntf          (const collection_name: TFRE_DB_NameType;const CollectionInterfaceSpec:ShortString;out Intf;const create_non_existing:boolean=true;const in_memory:boolean=false):boolean; // creates/fetches a Specific Collection / true if new
    function    DerivedCollection         (const collection_name: TFRE_DB_NameType;const create_non_existing:boolean=true): IFRE_DB_DERIVED_COLLECTION;

    procedure   NewObjectAsIntf           (const ObjectInterfaceSpec : ShortString ; out Intf); // creates a new DBO as Interface

    //procedure   ForAllObjects                (const iterator:IFRE_DB_Obj_Iterator)                                    ;
    procedure   ForAllColls                  (const iterator:IFRE_DB_Coll_Iterator)                                   ;
    procedure   ForAllSchemes                (const iterator:IFRE_DB_Scheme_Iterator)                                 ;
    procedure   ForAllEnums                  (const iterator:IFRE_DB_Enum_Iterator)                                   ;
    procedure   ForAllClientFieldValidators  (const iterator:IFRE_DB_ClientFieldValidator_Iterator)                   ;
    function    InvokeMethod                 (const class_name,method_name:TFRE_DB_String;const uid_path:TFRE_DB_GUIDArray;const input:IFRE_DB_Object;const session:TFRE_DB_UserSession):IFRE_DB_Object;

    function    NewWorkFlowScheme            (const WF_SchemeName:TFRE_DB_NameType):IFRE_DB_WORKFLOW;
    function    DeleteWorkFlowScheme         (const WF_SchemeName:TFRE_DB_NameType):TFRE_DB_Errortype;
    function    StoreWorkFlowScheme          (const WFS : IFRE_DB_WORKFLOW):TFRE_DB_Errortype;
    function    FetchWorkFlowScheme          (const WF_SchemeName:TFRE_DB_NameType;var WFS:IFRE_DB_WORKFLOW):Boolean;
    function    WorkFlowSchemeExists         (const WF_SchemeName:TFRE_DB_NameType):boolean;
    procedure   ForAllWorkFlowSchemes        (const iterator:IFRE_DB_Workflow_Iterator);

    function    StartNewWorkFlow             (const WF_SchemeName:TFRE_DB_NameType;const WF_UniqueKey:TFRE_DB_String):UInt64;
    function    StartNewWorkFlowRecurring    (const WF_SchemeName:TFRE_DB_NameType;const WF_UniqueKey:TFRE_DB_String;const sec_interval:integer):UInt64;

//System Functions
    function    AddUser                     (const loginatdomain,password,first_name,last_name:TFRE_DB_String):TFRE_DB_Errortype;
    function    UserExists                  (const loginatdomain:TFRE_DB_String):boolean;
    function    DeleteUser                  (const loginatdomain:TFRE_DB_String):TFRE_DB_Errortype;
    function    DeleteUserById              (const user_id:TGUID):TFRE_DB_Errortype;
    function    FetchUser                   (const loginatdomain:TFRE_DB_String;var user:IFRE_DB_USER):TFRE_DB_Errortype;
    function    FetchUserById               (const user_id:TGUID;var user: IFRE_DB_USER):TFRE_DB_Errortype;
    function    FetchGroup                  (const groupatdomain:TFRE_DB_String;var ug: IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    FetchGroupById              (const group_id:TGUID;var ug: IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    FetchRole                   (const roleatdomain:TFRE_DB_NameType;var role: IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    FetchRoleById               (const role_id:TGUID;var role: IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    NewRight                    (const rightname,txt,txt_short:TFRE_DB_String;var right : IFRE_DB_RIGHT):TFRE_DB_Errortype;
    function    NewRole                     (const rolename,txt,txt_short:TFRE_DB_String;var role:IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    NewGroup                    (const groupname,txt,txt_short:TFRE_DB_String;var group:IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    ModifyGroupRoles            (const groupatdomain:TFRE_DB_String;const roles:TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    AddGroupRoles               (const groupatdomain:TFRE_DB_String;const roles: TFRE_DB_StringArray): TFRE_DB_Errortype;
    function    RemoveGroupRoles            (const groupatdomain:TFRE_DB_String;const roles: TFRE_DB_StringArray; const ignore_not_set:boolean): TFRE_DB_Errortype;
    function    ModifyUserGroups            (const loginatdomain:TFRE_DB_String;const user_groups:TFRE_DB_StringArray; const keep_existing_groups:boolean=false):TFRE_DB_Errortype;
    function    RemoveUserGroups            (const loginatdomain:TFRE_DB_String;const user_groups:TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    ModifyUserPassword          (const loginatdomain,oldpassword,newpassword:TFRE_DB_String):TFRE_DB_Errortype;
    function    RoleExists                  (const roleatdomain:TFRE_DB_String):boolean;
    function    GroupExists                 (const groupatdomain:TFRE_DB_String):boolean;
    function    DeleteGroup                 (const groupatdomain:TFRE_DB_String):TFRE_DB_Errortype;
    function    DeleteRole                  (const roleatdomain:TFRE_DB_String):TFRE_DB_Errortype;
    function    StoreRole                   (const appname:TFRE_DB_String; const domainname : TFRE_DB_NameType; var rg:IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    StoreGroup                  (const appname:TFRE_DB_String; const domainname : TFRE_DB_NameType; var ug:IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    StoreGroupDomainbyID        (const domain_id: TGUID; var group : IFRE_DB_GROUP): TFRE_DB_Errortype;
    function    CheckRight                  (const right_name:TFRE_DB_String):boolean;
    function    CheckRightForGroup          (const right_name:TFRE_DB_String;const group_uid : TGuid):boolean;
    function    FetchApplications           (var apps : IFRE_DB_APPLICATION_ARRAY)  : TFRE_DB_Errortype; // with user rights
    function    FetchTranslateableText      (const translation_key:TFRE_DB_String; var textObj: IFRE_DB_TEXT):Boolean;//don't finalize the object
    function    FetchDomainById             (const domain_id:TGUID;var domain: IFRE_DB_DOMAIN):TFRE_DB_Errortype;
    function    ModifyDomainById            (const domain_id:TGUID; const domainname : TFRE_DB_NameType; const txt,txt_short:TFRE_DB_String):TFRE_DB_Errortype;
    function    DeleteDomainById            (const domain_id:TGUID):TFRE_DB_Errortype;
    function    DomainId                    (const name :TFRE_DB_NameType):TGUID;
    function    IsSystemGroup               (const group_id:TGUID):boolean;

    function    AdmGetUserCollection        :IFRE_DB_COLLECTION;
    function    AdmGetRoleCollection        :IFRE_DB_COLLECTION;
    function    AdmGetGroupCollection       :IFRE_DB_COLLECTION;
    function    AdmGetDomainCollection      :IFRE_DB_COLLECTION;


    //function    DatabaseList                : IFOS_STRINGS;
    //function    DatabaseExists              (const dbname:TFRE_DB_String):Boolean;
    //function    CreateDatabase              (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
    //function    DeleteDatabase              (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
    //function    BackupDatabaseReadable      (const to_stream:TStream;const stream_cb:TFRE_DB_StreamingCallback;const progress : TFRE_DB_PhaseProgressCallback):TFRE_DB_Errortype;
    //function    RestoreDatabaseReadable     (const from_stream:TStream;const stream_cb:TFRE_DB_StreamingCallback):TFRE_DB_Errortype;
    function    FetchUserSessionData        (var SessionData: IFRE_DB_OBJECT):boolean;
    function    StoreUserSessionData        (var session_data:IFRE_DB_Object):TFRE_DB_Errortype;
    function    OverviewDump                :TFRE_DB_String;
    //procedure   DrawScheme                  (const datastream:TStream);
  end;


  { IFRE_DB_SYS_CONNECTION }

  IFRE_DB_SYS_CONNECTION=interface(IFRE_DB_BASE)
  ['IFDB_SYS_CONN']
    function    Connect                     (const loginatdomain,pass:TFRE_DB_String):TFRE_DB_Errortype;
    function    CheckLogin                  (const user,pass:TFRE_DB_String):TFRE_DB_Errortype;

    function    AddUser                     (const loginatdomain,password,first_name,last_name:TFRE_DB_String):TFRE_DB_Errortype;
    function    UserExists                  (const loginatdomain:TFRE_DB_String):boolean;
    function    DeleteUser                  (const loginatdomain:TFRE_DB_String):TFRE_DB_Errortype;
    function    FetchUser                   (const loginatdomain:TFRE_DB_String;var user:IFRE_DB_USER):TFRE_DB_Errortype;
    function    FetchUserById               (const user_id:TGUID;var user: IFRE_DB_USER):TFRE_DB_Errortype;
    function    FetchGroup                  (const groupatdomain:TFRE_DB_String;var ug: IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    FetchGroupById              (const group_id:TGUID;var ug: IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    FetchRole                   (const roleatdomain:TFRE_DB_NameType;var role: IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    FetchRoleById               (const role_id:TGUID;var role: IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    FetchTranslateableText      (const translation_key:TFRE_DB_String; var textObj: IFRE_DB_TEXT):Boolean;//don't finalize the object
    function    NewRight                    (const rightname,txt,txt_short:TFRE_DB_String;var right : IFRE_DB_RIGHT):TFRE_DB_Errortype;
    function    NewRole                     (const rolename,txt,txt_short:TFRE_DB_String;var role  :IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    NewGroup                    (const groupname,txt,txt_short:TFRE_DB_String;var user_group:IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    NewAppData                  (const appname,txt,txt_short:TFRE_DB_String;var appdata: IFRE_DB_APPDATA):TFRE_DB_Errortype;
    function    ModifyGroupRoles            (const groupatdomain:TFRE_DB_String;const roles:TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    AddGroupRoles               (const groupatdomain:TFRE_DB_String;const roles: TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    RemoveGroupRoles            (const groupatdomain:TFRE_DB_String;const roles: TFRE_DB_StringArray; const ignore_not_set:boolean): TFRE_DB_Errortype;
    function    ModifyUserGroups            (const loginatdomain:TFRE_DB_String;const user_groups:TFRE_DB_StringArray; const keep_existing_groups:boolean=false):TFRE_DB_Errortype;
    function    ModifyUserPassword          (const loginatdomain,oldpassword,newpassword:TFRE_DB_String):TFRE_DB_Errortype;
    function    ModifyUserImage             (const loginatdomain:TFRE_DB_String;const imagestream : TFRE_DB_Stream):TFRE_DB_Errortype;
    function    RoleExists                  (const roleatdomain:TFRE_DB_String):boolean;
    function    GroupExists                 (const groupatdomain:TFRE_DB_String):boolean;
    function    DeleteGroup                 (const groupatdomain:TFRE_DB_String):TFRE_DB_Errortype;
    function    DeleteRole                  (const roleatdomain:TFRE_DB_String):TFRE_DB_Errortype;
    function    DomainExists                (const domainname:TFRE_DB_NameType):boolean;
    function    DeleteDomain                (const domainname:TFRE_DB_Nametype):TFRE_DB_Errortype;
    function    AddDomain                   (const domainname:TFRE_DB_NameType;const txt,txt_short:TFRE_DB_String):TFRE_DB_Errortype;
    procedure   ForAllDomains               (const func:IFRE_DB_Obj_Iterator);
    function    StoreRole                   (const appname:TFRE_DB_String; const domainname: TFRE_DB_NameType; var rg:IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    StoreGroup                  (const appname:TFRE_DB_String; const domainname: TFRE_DB_NameType; var ug:IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    StoreAppData                (var   appdata:IFRE_DB_APPDATA):TFRE_DB_Errortype;
    function    StoreTranslateableText      (const txt    :IFRE_DB_TEXT) :TFRE_DB_Errortype;
    function    UpdateAppData               (var appdata:IFRE_DB_APPDATA):TFRE_DB_Errortype;
    function    CheckRight                  (const right_name:TFRE_DB_String):boolean;
    function    InstallAppDefaults          (const Appname:TFRE_DB_String ):TFRE_DB_Errortype;
    function    RemoveApp                   (const Appname:TFRE_DB_String ):TFRE_DB_Errortype;
    function    RemoveAppGroup              (const Appname:TFRE_DB_String ; const sub_group_name: TFRE_DB_String):TFRE_DB_Errortype;
    function    AddAppGroup                 (const Appname:TFRE_DB_String ; const groupatdomain: TFRE_DB_String; const short_desc, long_desc: TFRE_DB_String):TFRE_DB_Errortype;
    function    GetAppDataID                (const Appname:TFRE_DB_String ;var appdata_id : TGuid):TFRE_DB_Errortype;
    function    FetchAppData                (const Appname:TFRE_DB_String ;var appdata: IFRE_DB_APPDATA):TFRE_DB_Errortype;
    function    DatabaseList                : IFOS_STRINGS;
    function    DatabaseExists              (const dbname:TFRE_DB_String):Boolean;
    function    CreateDatabase              (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
    function    DeleteDatabase              (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
    function    BackupDatabaseReadable      (const to_stream:TStream;const stream_cb:TFRE_DB_StreamingCallback;const progress : TFRE_DB_PhaseProgressCallback):TFRE_DB_Errortype;
    function    RestoreDatabaseReadable     (const from_stream:TStream;const stream_cb:TFRE_DB_StreamingCallback):TFRE_DB_Errortype;
    function    OverviewDump                :TFRE_DB_String;
    procedure   DumpSystem                  ;
    //procedure   DrawScheme                  (const datastream:TStream);
  end;


  TFRE_DB_CONTENT_DESC_ARRAY = array of TFRE_DB_CONTENT_DESC;

  TFRE_DB_WORKFLOW_STATE = (wfs_INVALID,wfs_RUNNING,wfs_COMPLETED_OK,wfs_FAILED);

  IFRE_DB_WORKFLOWSTEP=interface(IFRE_DB_NAMED_OBJECT_PLAIN)
    ['IFDWFS']
  end;
  IFRE_DB_WORKFLOW=interface(IFRE_DB_NAMED_OBJECT_PLAIN)
    ['IFDWF']
  end;

  IFRE_DB_APPLICATION=interface(IFRE_DB_COMMON)
    ['IFDBAPP']
    function   ObjectName                   : TFRE_DB_String;
    function   GetDescription               (conn : IFRE_DB_CONNECTION): IFRE_DB_TEXT;
    function   UID                          : TGUID;
    function   AsObject                     : IFRE_DB_Object;
    function   ShowInApplicationChooser     (const session:IFRE_DB_UserSession): Boolean;
    function   FetchAppText                 (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):IFRE_DB_TEXT;//don't finalize the object
    function   Get_Rightname                (const sub_right_name:string):string;
    function   CheckAppRightModule          (const conn: IFRE_DB_CONNECTION;const module_name:TFRE_DB_String) : Boolean;
  end;

  IFRE_DB_APPLICATION_MODULE=interface
    ['IFDBAPPM']
    function   GetToolbarMenu                : TFRE_DB_CONTENT_DESC;
    function   GetDBConnection               (const input:IFRE_DB_Object): IFRE_DB_CONNECTION;
    function   GetDBSessionData              (const input:IFRE_DB_Object): IFRE_DB_Object;
    function   GetDBModuleSessionData        (const input:IFRE_DB_Object): IFRE_DB_Object;
    //function   getApplication                : IFRE_DB_APPLICATION;
    function   GetName                       : TFRE_DB_String;
    function   ObjectName                    : TFRE_DB_String;
    function   GetDescription                (conn : IFRE_DB_CONNECTION): IFRE_DB_TEXT;
    function   AsObject                      : IFRE_DB_Object;
  end;

  TFOS_BASE   = class(TObject)
  public
    function  Implementor     : TObject;virtual;
    function  Implementor_HC  : TObject;virtual;
  end;

  { TFRE_DB_Base }
  {$M+}
  TFRE_DB_Base              = class(TFOS_Base)
  private
    TAGRef                  : TObject;
  protected
    FMediatorExtention        : TFRE_DB_ObjectEx;
    function  Implementor_HC  : TObject;override;
    function  Debug_ID        : TFRE_DB_String;virtual;
  public
    class procedure  RegisterSystemScheme     (const scheme:IFRE_DB_SCHEMEOBJECT); virtual;
    class procedure  InstallDBObjects         (const conn:IFRE_DB_SYS_CONNECTION); virtual;
    function         CFG_Dont_Finalize_Object : Boolean; virtual;
    function         GetSystemSchemeByName    (const schemename:TFRE_DB_String; var scheme: IFRE_DB_SchemeObject): Boolean;
    function         GetSystemScheme          (const schemename:TClass; var scheme: IFRE_DB_SchemeObject): Boolean;
    procedure        GetSession               (const input: IFRE_DB_Object; out session: TFRE_DB_UserSession; const no_error_on_no_session: boolean); deprecated; //DEPRECATED DONT USE
    function         GetSession               (const input: IFRE_DB_Object):TFRE_DB_UserSession; deprecated; //DEPRECATED DONT USE
    procedure        __SetMediator            (const med : TFRE_DB_ObjectEx);
    class function   Get_DBI_InstanceMethods                                            : TFRE_DB_StringArray;
    class function   Get_DBI_ClassMethods                                               : TFRE_DB_StringArray;
    class function   ClassMethodExists   (const name:Shortstring)                       : Boolean; // new
    class function   Invoke_DBIMC_Method (const name:TFRE_DB_String;const input:IFRE_DB_Object) : IFRE_DB_Object;
    function         Invoke_DBIMI_Method (const name:TFRE_DB_String;const input:IFRE_DB_Object;const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION) : IFRE_DB_Object;
    function         Fetch_DBIMI_Method  (const name:TFRE_DB_String)                            : IFRE_DB_InvokeInstanceMethod;
    function         IMI_MethodExists    (const name:TFRE_DB_String)                            : boolean;
    function         MethodExists        (const name:Shortstring)                       : Boolean; // new
    procedure        SetReference         (const obj : TObject);
    function         GetReference         : TObject;
    function         Supports                           (const InterfaceSpec:ShortString ; out Intf) : boolean;
    function         Supports                           (const InterfaceSpec:ShortString)            : boolean;
    procedure        IntfCast                           (const InterfaceSpec:ShortString ; out Intf) ; // IntfCast throws an Exception if not succesful
    //Utility Shortcuts
    function         CSFT                               (const server_function_name : string ; const obj:IFRE_DB_Object=nil):TFRE_DB_SERVER_FUNC_DESC;
    function         CSF                                (const invoke_method        : IFRE_DB_InvokeInstanceMethod):TFRE_DB_SERVER_FUNC_DESC;
    function         CWSF                               (const invoke_method        : IFRE_DB_WebInstanceMethod):TFRE_DB_SERVER_FUNC_DESC;
    function         CSCF                               (const serv_classname,server_function_name : string ; const param1 : string=''; const value1 : string=''):TFRE_DB_SERVER_FUNC_DESC;
  end;
  {$M-}



  // This is the TFRE_DB_ObjectEx mediator Class for all Code that has no direct access to TFRE_DB_ObjectClass

  { TFRE_DB_ObjectEx }

  TFRE_DB_ObjectEx=class(TFRE_DB_Base, IFRE_DB_Object)
  private
    FImplementor   : IFRE_DB_Object;       // Must support DBO Interface
    FBound         : Boolean;
  protected
    FNamedObject   : IFRE_DB_NAMED_OBJECT; // May  support Named Object Interface
    function       Debug_ID            : TFRE_DB_String ;override;
    procedure      InternalSetup       ; virtual;
    procedure      InternalFinalize    ; virtual;
  public
    class procedure RegisterSystemScheme               (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    function       Invoke                              (const method:TFRE_DB_String;const input:IFRE_DB_Object ; const ses : IFRE_DB_Usersession ; const app : IFRE_DB_APPLICATION ; const conn : IFRE_DB_CONNECTION):IFRE_DB_Object;virtual;
    constructor    create                              ;
    constructor    CreateConnected                     (const conn:IFRE_DB_CONNECTION);
    constructor    CreateBound                         (const dbo:IFRE_DB_Object);
    destructor     Destroy                             ;override;

    //Interface - Compatibility Block
    procedure       ForAllFields                       (const iter:IFRE_DB_FieldIterator);
    procedure       ForAllFieldsBreak                  (const iter:IFRE_DB_FieldIteratorBrk);
    function        UID                                : TGUID;
    function        UID_String                         : TFRE_DB_String;
    function        Parent                             : IFRE_DB_Object;
    function        ParentField                        : IFRE_DB_FIELD;
    function        AsString                           (const without_schemes:boolean=false):TFRE_DB_String;
    function        Field                              (const name:TFRE_DB_String):IFRE_DB_FIELD;
    function        FieldOnlyExistingObj               (const name:TFRE_DB_String):IFRE_DB_Object;
    function        FieldPath                          (const name:TFRE_DB_String;const dont_raise_ex:boolean=false):IFRE_DB_FIELD;
    function        FieldPathExists                    (const name:TFRE_DB_String):Boolean;
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
    function        SubFormattedDisplayAvailable       : boolean;
    function        GetSubFormattedDisplay             (indent:integer=4):TFRE_DB_String;
    function        SchemeClass                        : TFRE_DB_NameType;
    function        IsA                                (const schemename:TFRE_DB_NameType):Boolean;
    function        IsObjectRoot                       : Boolean;
    procedure       SaveToFile                         (const filename:TFRE_DB_String;const without_schemes:boolean=false);
    function        ReferencesObjects                  : Boolean;
    function        ReferencesObjectsFromData          : Boolean;
    function        ReferenceList                      : TFRE_DB_GUIDArray;
    function        ReferenceListFromData              : TFRE_DB_CountedGuidArray;
    function        ReferenceListFromDataNocount       : TFRE_DB_GuidArray;
    function        ReferencesDetailed                 : TFRE_DB_String;
    function        IsReferenced                       : Boolean;
    function        ReferencedByList                   : TFRE_DB_GUIDArray;
    function        ReferencedByList                   (const from_scheme: TFRE_DB_String): TFRE_DB_GUIDArray;
    function        ReferencedByList                   (const scheme: TFRE_DB_StringArray): TFRE_DB_GUIDArray;
    function        GetFieldListFilter                 (const field_type:TFRE_DB_FIELDTYPE):TFRE_DB_StringArray;
    function        GetUIDPath                         :TFRE_DB_StringArray;
    function        GetUIDPathUA                       :TFRE_DB_GUIDArray;
    function        Implementor                        : TObject;override;
    function        Implementor_HC                     : TObject;override;
    function        Supports                           (const InterfaceSpec:ShortString ; out Intf) : boolean;
    function        Supports                           (const InterfaceSpec:ShortString)            : boolean;
    procedure       IntfCast                           (const InterfaceSpec:ShortString ; out Intf) ; // IntfCast throws an Exception if not succesful
    function        IntfCast                           (const InterfaceSpec:ShortString) : Pointer  ; // IntfCast throws an Exception if not succesful
    function        GetDBConnection                    : IFRE_DB_CONNECTION;
    function        GetScheme                          : IFRE_DB_SchemeObject;
    procedure       Finalize                           ;
    function        GetAsJSON                          (const without_uid: boolean=false;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil): TJSONData;
    function        GetAsJSONString                    (const without_uid: boolean=false;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil): TFRE_DB_String;
    function        CloneToNewObject                   (const generate_new_uids:boolean=false): IFRE_DB_Object;
    function        Mediator                           : TFRE_DB_ObjectEx;
    function        NeededSize                         : TFRE_DB_SIZE_TYPE;
    procedure       Set_ReadOnly                       ;
    procedure       CopyField                          (const obj:IFRE_DB_Object;const field_name:String);
    class function  NewOperation                       (const input:IFRE_DB_Object):TGUID;

  published
    function        IMI_SaveOperation                  (const input:IFRE_DB_Object):IFRE_DB_Object;virtual;
    function        IMI_DeleteOperation                (const input:IFRE_DB_Object):IFRE_DB_Object;virtual;
    class function  IMC_NewOperation                   (const input:IFRE_DB_Object):IFRE_DB_Object;virtual;
    function        IMI_NoteLoad                       (const input:IFRE_DB_Object):IFRE_DB_Object;virtual;
    function        IMI_NoteSave                       (const input:IFRE_DB_Object):IFRE_DB_Object;virtual;
    function        IMI_NoteStartEdit                  (const input:IFRE_DB_Object):IFRE_DB_Object;virtual;
    function        IMI_NoteStopEdit                   (const input:IFRE_DB_Object):IFRE_DB_Object;virtual;
    //Interface - Compatibility Block End
  end;

  { TFRE_DB_NOTE }

  TFRE_DB_NOTE=class(TFRE_DB_ObjectEx)
  public
  protected
    class procedure RegisterSystemScheme    (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects        (const conn:IFRE_DB_SYS_CONNECTION); override;
  end;

  TFRE_DB_APPLICATION_MODULE=class;
  TFRE_DB_APPLICATION_MODULE_ITERATOR = procedure(const module : IFRE_DB_APPLICATION_MODULE;const modul_order:Int16) is nested;

  { TFRE_DB_APPLICATION}
  TFRE_DB_APPLICATION = class (TFRE_DB_ObjectEx,IFRE_DB_APPLICATION) // Requires Named Object as Implementor
  private
    procedure  SetDescTranslationKey         (const AValue: TFRE_DB_String);
    function   GetDescTranslationKey         : TFRE_DB_String;
    function   GetName                       : TFRE_DB_String;
    procedure  SetName                       (const AValue: TFRE_DB_String);
    procedure  SessionInitialize             (const session : TFRE_DB_UserSession);virtual;
    procedure  SessionFinalize               (const session : TFRE_DB_UserSession);virtual;
    procedure  SessionPromotion              (const session : TFRE_DB_UserSession);virtual;
  protected
    type       TFRE_DB_APP_INSTALLSTATE  =   (NotInstalled, OtherVersion, SameVersion);
    function   IsContentUpdateVisible       (const update_content_id:string):Boolean;
  protected
    function   CFG_ApplicationUsesRights     : boolean; virtual;
    procedure  InternalSetup                 ; override;
    procedure  SetupApplicationStructure     ; virtual;
    procedure  RemoveAppRole                 (const sys_connection: IFRE_DB_SYS_CONNECTION;const sub_group_name:TFRE_DB_String);
    function   _CheckVersion                 (const sys_connection: IFRE_DB_SYS_CONNECTION;out old_version: TFRE_DB_String) : TFRE_DB_APP_INSTALLSTATE;
    function   _ActualVersion                : TFRE_DB_String; virtual;
    procedure  _SetAppdataVersion            (const sys_connection: IFRE_DB_SYS_CONNECTION;const version: TFRE_DB_String); virtual;
    procedure  _CreateAndStoreAppdata        (const sys_connection: IFRE_DB_SYS_CONNECTION;const version: TFRE_DB_String); virtual;
    function   _FetchAppdata                 (const sys_connection: IFRE_DB_SYS_CONNECTION;var appdata: IFRE_DB_APPDATA):boolean;
    function   _CreateAppRole                (const rolename   :TFRE_DB_String;const short_desc,long_desc : TFRE_DB_String):IFRE_DB_ROLE;
    procedure  _AddAppRight                  (const right_group:IFRE_DB_ROLE;const sub_right_name,short_desc,long_desc:TFRE_DB_String);
    procedure  _AddAppRightModules           (const right_group:IFRE_DB_ROLE;const module_names:TFRE_DB_StringArray);
    procedure  _AddSystemGroups              (const sys_connection: IFRE_DB_SYS_CONNECTION; const domain : TFRE_DB_NameType); virtual;
    procedure  _RemoveDefaultGroups          (const sys_connection: IFRE_DB_SYS_CONNECTION); virtual;

    procedure  AddApplicationModule          (const module:TFRE_DB_APPLICATION_MODULE);
    function   DelegateInvoke                (const modulename:TFRE_DB_String;const methname:string;const input : IFRE_DB_Object):IFRE_DB_Object;
    function   IFRE_DB_APPLICATION.ObjectName = ObjectNameI;
    procedure  MySessionInitialize           (const session: TFRE_DB_UserSession); virtual;
    procedure  MySessionPromotion            (const session: TFRE_DB_UserSession); virtual;
    procedure  MySessionFinalize             (const session: TFRE_DB_UserSession); virtual;
    procedure  MyServerInitialize            (const admin_dbc : IFRE_DB_CONNECTION); virtual;
    procedure  MyServerFinalize              ;
  public
    procedure  ForAllAppModules              (const module_iterator:TFRE_DB_APPLICATION_MODULE_ITERATOR);
    function   CheckAppRightModule           (const conn: IFRE_DB_CONNECTION;const module_name:TFRE_DB_String) : Boolean;
    function   CheckAppRightModule           (const input_context: IFRE_DB_Object;const module_name:TFRE_DB_String) : Boolean;

    procedure  ServerInitialize              (const admin_dbc : IFRE_DB_CONNECTION);
    procedure  ServerFinalize                (const admin_dbc : IFRE_DB_CONNECTION); // NOT IMPLEMENTED

    function   InstallAppDefaults            (const conn : IFRE_DB_SYS_CONNECTION):TFRE_DB_Errortype; virtual;
    function   InstallSystemGroupsandRoles   (const conn : IFRE_DB_SYS_CONNECTION; const domain : TFRE_DB_NameType):TFRE_DB_Errortype; virtual;
    function   RemoveApp                     (const conn : IFRE_DB_SYS_CONNECTION):TFRE_DB_Errortype; virtual;

    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure  Finalize                     ;
    function   GetDescription               (conn : IFRE_DB_CONNECTION): IFRE_DB_TEXT;

    function   GetSessionData               (const input:IFRE_DB_Object):IFRE_DB_Object; //global
    function   GetSessionAppData            (const input:IFRE_DB_Object):IFRE_DB_Object; //per app

    function   GetAppModulesAsSubSection    (const input:IFRE_DB_Object): TFRE_DB_CONTENT_DESC;

    procedure  InitAppDesc                  (const objname,descr_translation_key:TFRE_DB_String);virtual;
    property   ObjectName                   : TFRE_DB_String       read GetName write SetName;
    function   AsObject                     : IFRE_DB_Object;
    function   ObjectNameI                  : TFRE_DB_String;

    procedure   AddAppToSiteMap             (const session:TFRE_DB_UserSession ; const parent_entry : TFRE_DB_CONTENT_DESC);
    function    ShowInApplicationChooser    (const session:IFRE_DB_UserSession): Boolean;virtual;

    function   CreateAppText                (const conn: IFRE_DB_SYS_CONNECTION;const translation_key:TFRE_DB_String;const short_text:TFRE_DB_String;const long_text:TFRE_DB_String='';const hint_text:TFRE_DB_String=''):TFRE_DB_Errortype;

    function   FetchAppText                 (const conn: IFRE_DB_CONNECTION;const translation_key:TFRE_DB_String):IFRE_DB_TEXT;//don't finalize the object
    function   FetchAppText                 (const input_context: IFRE_DB_Object;const translation_key:TFRE_DB_String):IFRE_DB_TEXT;//don't finalize the object
    function   FetchAppText                 (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):IFRE_DB_TEXT;//don't finalize the object

    function   Get_Rightname                (const sub_right_name:string):string;

  published
     function   IMI_Content                 (const input:IFRE_DB_Object):IFRE_DB_Object; virtual;
     function   IMI_OnUIChange              (const input:IFRE_DB_Object):IFRE_DB_Object; virtual;
  end;

    { TFRE_DB_APPLICATION_MODULE }

  TFRE_DB_APPLICATION_MODULE = class (TFRE_DB_ObjectEx,IFRE_DB_APPLICATION_MODULE)
  private
    procedure  InternalSetup ; override;
  protected
    procedure  SetupAppModuleStructure       ;virtual;
    procedure  AddApplicationModule          (const module:TFRE_DB_APPLICATION_MODULE);
    procedure  InitModuleDesc                (const objname,descr_translation_key:TFRE_DB_String);virtual;

    function   IFRE_DB_APPLICATION_MODULE.ObjectName  = GetName;
    function   IFRE_DB_APPLICATION_MODULE.DEscription = GetDesc;
    procedure  MyServerInitializeModule     (const admin_dbc : IFRE_DB_CONNECTION); virtual;
  public
    procedure  ForAllAppModules              (const module_iterator:TFRE_DB_APPLICATION_MODULE_ITERATOR);
    procedure  MySessionInitializeModule    (const session : TFRE_DB_UserSession);virtual;
    procedure  MySessionPromotionModule     (const session: TFRE_DB_UserSession); virtual;
    procedure  MySessionFinalizeModule      (const session : TFRE_DB_UserSession);virtual;
    function   GetAppModulesAsSubSection    (const input:IFRE_DB_Object): TFRE_DB_CONTENT_DESC;

    function   GetEmbeddingApp              : TFRE_DB_APPLICATION;
    function   CheckAppRightModule          (const input_context: IFRE_DB_Object;const module_name:TFRE_DB_String) : Boolean;
    function   FetchAppText                 (const input_context: IFRE_DB_Object;const translation_key:TFRE_DB_String):IFRE_DB_TEXT;

    function   Get_Rightname                (const sub_right_name:string):string;

    function   GetToolbarMenu               : TFRE_DB_CONTENT_DESC;virtual;
    function   GetDBConnection              (const input:IFRE_DB_Object): IFRE_DB_CONNECTION;
    function   GetDBSessionData             (const input:IFRE_DB_Object): IFRE_DB_Object;
    function   GetDBModuleSessionData       (const input:IFRE_DB_Object): IFRE_DB_Object;
    function   GetDependencyFiltervalues    (const input:IFRE_DB_Object; const dependencyfield:string): TFRE_DB_StringArray;

    function   GetName                      : TFRE_DB_String;
    procedure  SetName                      (const AValue: TFRE_DB_String);
    property   ObjectName                   : TFRE_DB_String       read GetName write SetName;
    procedure  SetDescrTranslationKey       (const val:TFRE_DB_String);
    function   GetDescrTranslationKey       :TFRE_DB_String;
    function   GetDescription                (conn : IFRE_DB_CONNECTION): IFRE_DB_TEXT;
    function   AsObject                     : IFRE_DB_Object;
    function   IsContentUpdateVisible       (const update_content_id:string):Boolean;
  published
    function   IMI_OnUIChange                (const input:IFRE_DB_Object):IFRE_DB_Object; virtual;
  end;

  // Describes a basic content Element
  // The content has an contentID and uses as Internal representation a (temporary/non saved) DBO

  TFRE_DB_OBJECTCLASSEX   = class of TFRE_DB_ObjectEx;

  { TFRE_DB_CONTENT_DESC }

  TFRE_DB_CONTENT_DESC    = class(TFRE_DB_ObjectEx)
  private
    function  GetContentId    : TFRE_DB_String;
    function  GetUpdateId     : TFRE_DB_String;
    function  GetWindowCaption : TFRE_DB_String;
    procedure SetContentId    (AValue: TFRE_DB_String);
    procedure SetUpdateId     (AValue: TFRE_DB_String);
    procedure SetWindowCaption(AValue: TFRE_DB_String);
  published
    property  contentId    : TFRE_DB_String read GetContentId write SetContentId;
    property  updateId     : TFRE_DB_String read GetUpdateId write SetUpdateId;
    property  windowCaption: TFRE_DB_String read GetWindowCaption write SetWindowCaption;
  end;

  { TFRE_DB_NIL_DESC }

  //@ Describes "No Reply". E.g. User answers no to a confirm deletion dialog.
  TFRE_DB_NIL_DESC = class(TFRE_DB_CONTENT_DESC)
  public class
    var NILInstances : NativeInt;
  public
     constructor      Create                   ;
     destructor       Destroy                  ;override;
     procedure        DestroySingleton         ;
     function         CFG_Dont_Finalize_Object : Boolean; override;
  end;

  { TFRE_DB_PARAM_DESC }

  TFRE_DB_PARAM_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a single parameter for a parameterizable content.
    function Describe   (const key,value: string):TFRE_DB_PARAM_DESC;
    //@ Describes a single parameter with multiple values for a parameterizable content.
    function Describe   (const key:String;const value:TFRE_DB_StringArray):TFRE_DB_PARAM_DESC;
  end;

  { TFRE_DB_SERVER_FUNC_DESC }

  TFRE_DB_SERVER_FUNC_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a server function of a known object.
    function Describe   (const obj: IFRE_DB_Object; const func: String):TFRE_DB_SERVER_FUNC_DESC;
    //@ Describes a server function of a known object.
    function Describe   (const oschemeclass: String;const ouid   :TGUID; const func: String):TFRE_DB_SERVER_FUNC_DESC;
    function Describe   (const oschemeclass: String;const uidpath:TFRE_DB_StringArray; const func: String):TFRE_DB_SERVER_FUNC_DESC;
    //@ Describes a server class function of the given schemeclass (e.g. new operations) or
    //@ a server function of an unknown object of the given schemeclass (e.g. menu function of a grid entry).
    function Describe       (const oschemeclass: String; const func: String):TFRE_DB_SERVER_FUNC_DESC;
    function InternalInvoke (const session: TFRE_DB_UserSession; const input: IFRE_DB_Object): IFRE_DB_Object;
    //@ Internally invokes a server function, on a given session
    function  AddParam  : TFRE_DB_PARAM_DESC;
    //@ Creates a new parameter and adds it to the parameterizable content.
  end;


  { TFRE_DB_MESSAGE_DESC }

  TFRE_DB_MESSAGE_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a message of the given type (error, info or confirm).
    //@ The server function will be called after the user clicks a button.
    //@ For confirm dialogs the server function is required.
    function  Describe (const caption,msg: String;const msgType:TFRE_DB_MESSAGE_TYPE;const serverFunc:TFRE_DB_SERVER_FUNC_DESC=nil): TFRE_DB_MESSAGE_DESC;
  end;

  { TFRE_DB_EDITOR_DATA_DESC }

  TFRE_DB_EDITOR_DATA_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes the data for an editor.
    function Describe        (const value: String): TFRE_DB_EDITOR_DATA_DESC;
  end;



  { TFRE_DB_REFRESH_STORE_DESC }

  TFRE_DB_REFRESH_STORE_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a refresh action. E.g. after an add opertion.
    function  Describe       (const storeId: String): TFRE_DB_REFRESH_STORE_DESC;
  end;

  { TFRE_DB_CLOSE_DIALOG_DESC }

  TFRE_DB_CLOSE_DIALOG_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a refresh action. E.g. after an add opertion.
    function  Describe       (): TFRE_DB_CLOSE_DIALOG_DESC;
  end;


  TFRE_DB_SERVER_FUNC_DESC_ARRAY   = Array of TFRE_DB_SERVER_FUNC_DESC;

  TFRE_DB_DESCRIPTION_CLASS  = class of TFRE_DB_CONTENT_DESC;

  IFRE_DB=interface
    function    NewDBCommand                    : IFRE_DB_COMMAND;
    function    FetchApplications               (var apps : IFRE_DB_APPLICATION_ARRAY):TFRE_DB_Errortype;
    function    GetFormatSettings               : TFormatSettings;
    function    GetLocalZone                    : TFRE_DB_String;
    procedure   SetFormatSettings               (const AValue: TFormatSettings);
    procedure   SetLocalZone                    (const AValue: TFRE_DB_String);
    function    LocalTimeToUTCDB64              (const ADateTime64: TFRE_DB_DateTime64) : TFRE_DB_DateTime64;
    function    UTCToLocalTimeDB64              (const ADateTime64: TFRE_DB_DateTime64) : TFRE_DB_DateTime64;

    function    NewText                         (const key,txt,txt_short:TFRE_DB_String;const hint:TFRE_DB_String=''):IFRE_DB_TEXT;
    function    NewRight                        (const rightname,txt,txt_short:TFRE_DB_String):IFRE_DB_RIGHT;
    function    NewRole                         (const rolename,txt,txt_short:TFRE_DB_String):IFRE_DB_ROLE;
    function    NewGroup                        (const groupname,txt,txt_short:TFRE_DB_String):IFRE_DB_GROUP;
    function    NewClientFieldValidator         (const name: TFRE_DB_String) : IFRE_DB_ClientFieldValidator;
    function    NewEnum                         (const name: TFRE_DB_String) : IFRE_DB_Enum;
    function    RegisterSysClientFieldValidator (const val : IFRE_DB_ClientFieldValidator):TFRE_DB_Errortype;
    function    RegisterSysEnum                 (const enu : IFRE_DB_Enum):TFRE_DB_Errortype;

    function    GetSystemSchemeByName           (const schemename:TFRE_DB_String; var scheme: IFRE_DB_SchemeObject): Boolean;
    function    GetSystemScheme                 (const schemename:TClass; var scheme: IFRE_DB_SchemeObject): Boolean;


    function    NewObjectIntf          (const InterfaceSpec:ShortString;out Intf;const mediator : TFRE_DB_ObjectEx=nil;const fail_on_non_existent:boolean=true) : Boolean;
    function    NewObject              : IFRE_DB_Object;
    function    NewObjectScheme        (const Scheme : TClass): IFRE_DB_Object;
    function    CreateFromFile         (const filename:TFRE_DB_String ; const conn:IFRE_DB_CONNECTION=nil; const recreate_weak_schemes: boolean=false):IFRE_DB_Object;
    function    CreateFromMemory       (memory : Pointer      ; const conn:IFRE_DB_CONNECTION=nil; const recreate_weak_schemes: boolean=false):IFRE_DB_Object;
    function    CreateFromString       (const AValue:TFRE_DB_String   ; const conn:IFRE_DB_CONNECTION=nil; const recreate_weak_schemes: boolean=false):IFRE_DB_Object;

    function    NewConnection          (const direct : boolean = true): IFRE_DB_CONNECTION;
    function    NewSysOnlyConnection   : IFRE_DB_SYS_CONNECTION; // always direct=embedded
    function    DatabaseList           (const user:TFRE_DB_String='';const pass:TFRE_DB_String=''): IFOS_STRINGS;

    procedure   DBInitializeAllExClasses     (const conn:IFRE_DB_SYS_CONNECTION);
    procedure   DBInitializeAllSystemClasses (const conn:IFRE_DB_SYS_CONNECTION); // not impemented by now (no initializable sys classes, keep count low)

    function    StringArray2String     (const A:TFRE_DB_StringArray):TFRE_DB_String;
    function    GuidArray2SString      (const A:TFRE_DB_GUIDArray):TFRE_DB_String;
    function    StringArray2GuidArray  (const A:TFRE_DB_StringArray):TFRE_DB_GUIDArray;
    function    CountedObjLinks2String (const A:TFRE_DB_CountedGuidArray):TFRE_DB_String;

    function    ConstructGuidArray     (const A:Array of TGuid)   :TFRE_DB_GUIDArray;
    function    ConstructByteArray     (const A:Array of Byte)    :TFRE_DB_ByteArray;
    function    ConstructInt16Array    (const A:Array of Int16)   :TFRE_DB_Int16Array;
    function    ConstructInt32Array    (const A:Array of Int32)   :TFRE_DB_Int32Array;
    function    ConstructInt64Array    (const A:Array of Int64)   :TFRE_DB_Int64Array;
    function    ConstructUInt16Array   (const A:Array of UInt16)  :TFRE_DB_UInt16Array;
    function    ConstructUInt32Array   (const A:Array of UInt32)  :TFRE_DB_UInt32Array;
    function    ConstructUInt64Array   (const A:Array of UInt64)  :TFRE_DB_UInt64Array;
    function    ConstructReal32Array   (const A:Array of Single)  :TFRE_DB_Real32Array;
    function    ConstructReal64Array   (const A:Array of Double)  :TFRE_DB_Real64Array;
    function    ConstructCurrencyArray (const A:Array of Currency):TFRE_DB_CurrencyArray;
    function    ConstructStringArray   (const A:Array of TFRE_DB_String)  :TFRE_DB_StringArray;
    function    ConstructBooleanArray  (const A:Array of Boolean):TFRE_DB_BoolArray;
    function    ConstructDateTimeArray (const A:Array of TFRE_DB_DateTime64):TFRE_DB_DateTimeArray;
    function    ConstructStreamArray   (const A:Array of TFRE_DB_Stream):TFRE_DB_StreamArray;
    function    ConstructObjectArray   (const A:Array of IFRE_DB_Object):IFRE_DB_ObjectArray;

    function    CreateText             (const translation_key:TFRE_DB_String;const short_text:TFRE_DB_String;const long_text:TFRE_DB_String='';const hint_text:TFRE_DB_String=''):IFRE_DB_TEXT;

    function    JSONObject2Object      (const json_string:string):IFRE_DB_Object;


    procedure   RegisterObjectClassEx   (const ExtensionObject : TFRE_DB_OBJECTCLASSEX);
    procedure   Initialize_Extension_Objects;

    procedure   LogDebug               (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogInfo                (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogWarning             (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogError               (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogNotice              (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogDebug               (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogInfo                (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogWarning             (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogError               (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogNotice              (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);

    procedure   ClearGUID              (var uid:TGUID);
    function    Get_A_Guid             : TGUID;
    function    Get_A_Guid_HEX         : Ansistring;


    property    LocalZone              : TFRE_DB_String read GetLocalZone write SetLocalZone;
    property    StringFormatSettings   : TFormatSettings read GetFormatSettings write SetFormatSettings;
  end;

  IFRE_DB_InputGroupSchemeDefinition=interface
    function  GetCaptionKey      : TFRE_DB_String;
    function  GetIGFields        : IFRE_DB_ObjectArray;
    function  GetInputGroupID    : TFRE_DB_String;
    procedure SetCaptionKey      (AValue: TFRE_DB_String);
    procedure SetIGFields        (AValue: IFRE_DB_ObjectArray);
    procedure SetInputGroupID    (AValue: TFRE_DB_String);
    function  Setup              (const caption: TFRE_DB_String):IFRE_DB_InputGroupSchemeDefinition;
    function  GetParentScheme    : IFRE_DB_SchemeObject;
    procedure AddInput           (const schemefield: TFRE_DB_String; const cap_trans_key: TFRE_DB_String; const disabled: Boolean=false;const hidden:Boolean=false; const field_backing_collection: TFRE_DB_String='');
    procedure UseInputGroup      (const scheme,group: TFRE_DB_String; const addPrefix: TFRE_DB_String='');
    procedure AddInputSubGroup   (const scheme,group: TFRE_DB_String; const addPrefix: TFRE_DB_String='';const collapsible:Boolean=false;const collapsed:Boolean=false);
    property  CaptionKey         : TFRE_DB_String read GetCaptionKey    write SetCaptionKey;
    property  InputGroupID       : TFRE_DB_String  read GetInputGroupID write SetInputGroupID;
    property  Fields             : IFRE_DB_ObjectArray read GetIGFields write SetIGFields;
  end;

  TFRE_DB_OnCheckUserNamePassword     = function  (username,pass:TFRE_DB_String) : TFRE_DB_Errortype of object;
  TFRE_DB_OnGetImpersonatedConnection = function  (const db,username,pass:TFRE_DB_String;out conn : IFRE_DB_CONNECTION):TFRE_DB_Errortype of object;
  TFRE_DB_OnRestoreDefaultConnection  = function  (out   username:TFRE_DB_String;out conn : IFRE_DB_CONNECTION):TFRE_DB_Errortype of object;
  TFRE_DB_OnExistsUserSessionForUser  = function  (const username:string;out other_session:TFRE_DB_UserSession):boolean of object;
  TFRE_DB_PromoteResult               = (pr_OK,pr_Failed,pr_Takeover);

  { TFRE_DB_UserSession }
  TFRE_DB_EVENT_METHOD_ENC=class
  public
    method : IFRE_DB_InvokeClassMethod;
    params : IFRE_DB_Object;
  end;

  IFRE_DB_UserSession=interface
    function    GetSessionID             : TFRE_DB_String;
    function    GetSessionAppData        (const app_key:TFRE_DB_String):IFRE_DB_Object;
    function    GetSessionModuleData     (const mod_key:TFRE_DB_String):IFRE_DB_Object;
    function    GetSessionGlobalData     :IFRE_DB_Object;
    function    NewDerivedCollection     (dcname:TFRE_DB_NameType):IFRE_DB_DERIVED_COLLECTION;
    function    FetchDerivedCollection   (dcname:TFRE_DB_NameType):IFRE_DB_DERIVED_COLLECTION;
    function    FetchTranslateableText   (const translation_key:TFRE_DB_String; var textObj: IFRE_DB_TEXT):Boolean;//don't finalize the object
    function    GetDBConnection          :IFRE_DB_CONNECTION;

    procedure   registerUpdatableContent   (const contentId: String);
    procedure   unregisterUpdatableContent (const contentId: String);
    procedure   registerUpdatableDBO       (const id: String);
    procedure   unregisterUpdatableDBO     (const id: String);
    function    isUpdatableContentVisible  (const contentId: String): Boolean;
  end;

  TFRE_DB_UserSession = class(TObject,IFRE_DB_Usersession)
  private
    FOnCheckUserNamePW    : TFRE_DB_OnCheckUserNamePassword;
    FOnExistsUserSession  : TFRE_DB_OnExistsUserSessionForUser;
    FOnGetImpersonatedDBC : TFRE_DB_OnGetImpersonatedConnection;
    FOnRestoreDefaultDBC  : TFRE_DB_OnRestoreDefaultConnection;
    FOnWorkCommands       : TNotifyEvent;
    FUserName             : TFRE_DB_String;
    FSessionID            : TFRE_DB_String;
    FPassMD5              : TFRE_DB_String;
    FDefaultApp           : TFRE_DB_String;
    FSessionData          : IFRE_DB_Object;

    FDbgTaskMethod        : IFRE_DB_InvokeInstanceMethod;
    FDbgTaskIv            : integer;

    FDefaultUID           : TFRE_DB_GUIDArray;
    FSessionQ             : IFOS_LFQ;
    FDBConnection         : IFRE_DB_CONNECTION;
    FPromoted             : Boolean;

    FAppArray             : Array of IFRE_DB_APPLICATION;
    FDC_Array             : Array of IFRE_DB_DERIVED_COLLECTION;
    FcurrentApp           : TFRE_DB_String;
    FConnDesc             : String;
    FBoundSession_RA_SC   : IFRE_DB_COMMAND_REQUEST_ANSWER_SC;
    FIsInteractive        : Boolean;

    procedure     SetOnCheckUserNamePW    (AValue: TFRE_DB_OnCheckUserNamePassword);
    procedure     SetOnExistsUserSession  (AValue: TFRE_DB_OnExistsUserSessionForUser);
    procedure     SetOnGetImpersonatedDBC (AValue: TFRE_DB_OnGetImpersonatedConnection);
    procedure     SetOnRestoreDefaultDBC  (AValue: TFRE_DB_OnRestoreDefaultConnection);
    procedure     SetOnWorkCommands       (AValue: TNotifyEvent);
    procedure     _FixupDCName           (var dcname:TFRE_DB_NameType);
    function      SearchSessionDC        (dc_name:TFRE_DB_String;out dc:IFRE_DB_DERIVED_COLLECTION):boolean;
    procedure     _FetchAppsFromDB       ;
    procedure     _InitApps              ;

    procedure     dbg_TaskMethodDisp     (const ES:IFRE_APS_EVENTSOURCE ; const TID:integer;const Data:Pointer;const cp:integer=0);
  public
    constructor Create                   (const user_name,password:TFRE_DB_String;const default_app:TFRE_DB_String;const default_uid_path : TFRE_DB_GUIDArray ; conn : IFRE_DB_CONNECTION);
    procedure   StoreSessionData         ;
    destructor  Destroy                  ;override;
    function    SearchSessionApp         (const app_key:TFRE_DB_String ; out app:TFRE_DB_APPLICATION ; out idx:integer):boolean;

    function    SearchSessionAppUID      (const app_uid:TGUID;out app:IFRE_DB_Object):boolean;
    function    SearchSessionDCUID       (const  dc_uid:TGUID;out dc:IFRE_DB_DERIVED_COLLECTION):boolean;
    procedure   RemSessionAppAndFinialize(const app_key:TFRE_DB_String);
    procedure   RemoveAllAppsAndFinalize ;
    procedure   SetCurrentApp            (const app_key:TFRE_DB_String);
    procedure   Input_FRE_DB_Command     (const cmd :IFRE_DB_COMMAND); // Here Comes the command in ..
    procedure   Input_FRE_DB_Event       (const cmd_event:TFRE_DB_EVENT_METHOD_ENC);
    function    Session_Has_CMDS         : Boolean;
    function    WorkSessionCommand       : IFRE_DB_COMMAND;
    function    InternalSessInvokeMethod (const class_name,method_name:string;const uid_path:TFRE_DB_GUIDArray;const input:IFRE_DB_Object):IFRE_DB_Object;
    function    InternalSessInvokeMethod (const obj:IFRE_DB_Object;const method_name:string;const input:IFRE_DB_Object):IFRE_DB_Object;
    function    CloneSession             (const connectiond_desc:string): TFRE_DB_UserSession;
    function    Promote                  (const user_name,password:TFRE_DB_String;var promotion_error:TFRE_DB_String;const force_new_session_data : boolean ; const session_takeover : boolean ; out take_over_content : TFRE_DB_CONTENT_DESC ) : TFRE_DB_PromoteResult; // Promote USER to another USER
    procedure   InitiateTakeover         (const NEW_RASC:IFRE_DB_COMMAND_REQUEST_ANSWER_SC;out take_over_content : TFRE_DB_CONTENT_DESC;const connection_desc:string);
    procedure   Demote                   ;
    procedure   Logout                   ;
    function    LoggedIn                 : Boolean;
    function    QuerySessionDestroy      : Boolean;
    function    GetSessionID             : TFRE_DB_String;
    function    GetSessionAppData        (const app_key:TFRE_DB_String):IFRE_DB_Object;
    function    GetSessionModuleData     (const mod_key:TFRE_DB_String):IFRE_DB_Object;
    function    GetSessionGlobalData     :IFRE_DB_Object;
    function    NewDerivedCollection     (dcname:TFRE_DB_NameType):IFRE_DB_DERIVED_COLLECTION;
    function    FetchDerivedCollection   (dcname:TFRE_DB_NameType):IFRE_DB_DERIVED_COLLECTION;
    procedure   FinishDerivedCollections ;
    function    GetUsername              : String;
    function    GetClientDetails         : String;
    function    GetSessionAppArray       : IFRE_DB_APPLICATION_ARRAY;

    procedure   SetServerClientInterface   (const sc_interface: IFRE_DB_COMMAND_REQUEST_ANSWER_SC;const interactive_session:boolean);
    procedure   ClearServerClientInterface ;
    procedure   registerUpdatableContent   (const contentId: String);
    procedure   unregisterUpdatableContent (const contentId: String);
    procedure   registerUpdatableDBO       (const id: String);
    procedure   unregisterUpdatableDBO     (const id: String);
    function    isUpdatableContentVisible  (const contentId: String): Boolean;

    procedure   SendServerClientRequest  (const description : TFRE_DB_CONTENT_DESC;const session_id:String='');

    procedure   RegisterTaskMethod       (const TaskMethod:IFRE_DB_InvokeInstanceMethod;const invocation_interval : integer); //DEPRECATED - DONT USE
    procedure   RemoveTaskMethod         ;
    function    IsInteractiveSession     : Boolean;

    function    FetchTranslateableText   (const translation_key:TFRE_DB_String; var textObj: IFRE_DB_TEXT):Boolean;//don't finalize the object

    function    GetDBConnection          :IFRE_DB_CONNECTION;

    property    OnGetImpersonatedDBC     :TFRE_DB_OnGetImpersonatedConnection read FOnGetImpersonatedDBC write SetOnGetImpersonatedDBC;
    property    OnWorkCommandsEvent      :TNotifyEvent read FOnWorkCommands write SetOnWorkCommands;
    property    OnRestoreDefaultDBC      :TFRE_DB_OnRestoreDefaultConnection read FOnRestoreDefaultDBC write SetOnRestoreDefaultDBC;
    property    OnExistsUserSession      :TFRE_DB_OnExistsUserSessionForUser read FOnExistsUserSession write SetOnExistsUserSession;
    property    OnCheckUserNamePW        :TFRE_DB_OnCheckUserNamePassword read FOnCheckUserNamePW write SetOnCheckUserNamePW;
  end;


  TFRE_DB_FetchSessionCB = procedure(const back_channel: IFRE_DB_COMMAND_REQUEST_ANSWER_SC; out   session : TFRE_DB_UserSession;const old_session_id:string;const interactive_session:boolean) of object;
  TFRE_DB_SessionCB      = procedure(const sender : TObject; const session : TFRE_DB_UserSession) of object;

  TGUID_Access = packed record
    case byte of
     0: (
         Part1 : QWord;
         Part2 : QWord;
       );
     1: (
         GUID_LOW       : QWord; // 8 Byte
         G_PAD_1        : DWORD; // 4 Byte
         G_PAD_2        : Word ; // 2 Byte
         GUID_MODE      : Byte ; // 1 Byte
         NODE_ID        : Byte ; // 1 Byte  FOS_NODE_ID = 0 Non Unique GUID
        );
  end;
  const
    cFRE_GUID_DATA_MODE  = $DA;
    cFRE_GUID_SYS_MODE   = $CC;
    cFRE_GUID_NONU_MODE  = $11;

  type
    PGUID_Access = ^TGUID_Access;

  function  FieldtypeShortString2Fieldtype       (const fts: TFRE_DB_String): TFRE_DB_FIELDTYPE;
  procedure CheckDbResult                        (const res:TFRE_DB_Errortype;const error_string : TFRE_DB_String ; const append_errorcode : boolean = false);
  procedure CheckDbResultFmt                     (const res:TFRE_DB_Errortype;const error_string : TFRE_DB_String ; const params:array of const);
  function  RB_Guid_Compare                      (const d1, d2: TGuid): NativeInt; inline;

  function  FREDB_FindStringIndexInArray         (const text:TFRE_DB_String;const strings:TFRE_DB_StringArray):integer;
  function  FREDB_CombineString                  (const strings: TFRE_DB_StringArray; const sep: TFRE_DB_String): TFRE_DB_String;
  procedure FREDB_SeperateString                 (const value,sep : TFRE_DB_String; var Strings: TFRE_DB_StringArray); //TODO UNICODE

  function  FREDB_DBNameType_Compare             (const S1, S2: TFRE_DB_NameType): NativeInt;
  function  FREDB_DBString_Compare               (const S1, S2: TFRE_DB_String): NativeInt;
  function  FREDB_DBint64_Compare                (const S1, S2: int64): NativeInt;
  function  FREDB_DBuint64_Compare               (const S1, S2: uint64): NativeInt;

  function  FREDB_FieldtypeShortString2Fieldtype (const fts: TFRE_DB_String): TFRE_DB_FIELDTYPE;
  function  FREDB_Bool2String                    (const bool:boolean):String;
  function  FREDB_String2GuidArray               (const str:string):TFRE_DB_GUIDArray;
  function  FREDB_String2Guid                    (const str:string):TGUID;
  function  FREDB_String2Bool                    (const str:string):boolean;
  function  FREDB_NumFilterType2String           (const nft:TFRE_DB_NUM_FILTERTYPE):String;
  function  FREDB_String2NumfilterType           (const str:string):TFRE_DB_NUM_FILTERTYPE;
  function  FREDB_String2StrFilterType           (const str:string):TFRE_DB_STR_FILTERTYPE;
  function  FREDB_StrFilterType2String           (const sft:TFRE_DB_STR_FILTERTYPE):String;
  function  FREDB_Guids_Same                     (const d1, d2 : TGuid):boolean;
  function  FREDB_Guids_Compare                  (const d1, d2 : TGuid):NativeInt; // 0=Same 1 = d2>d1 -1 = d1>d2
  function  FREDB_CheckGuidsUnique               (const arr: TFRE_DB_GUIDArray):boolean;
  function  FREDB_GuidList2Counted               (const arr: TFRE_DB_GUIDArray; const stop_on_first_double: boolean=false): TFRE_DB_CountedGuidArray;
  function  FREDB_ObjReferences2GuidArray        (const ref: TFRE_DB_ObjectReferences) : TFRE_DB_GUIDArray; // TODO -> UNIQUE CHECK, some guids maybe doubled in here

  function  FREDB_ObjectToPtrUInt                 (const obj : TObject):PtrUInt;
  function  FREDB_PtrUIntToObject                 (const obj : PtrUInt):TObject;

  function  FREDB_String2DBDisplayType           (const fts: string): TFRE_DB_DISPLAY_TYPE;
  procedure FREDB_SiteMap_AddEntry               (const SiteMapData : IFRE_DB_Object ; const key:string;const caption : String ; const icon : String ; InterAppLink : TFRE_DB_StringArray ;const x,y : integer;  const newsCount:Integer=0; const scale:Single=1; const enabled:Boolean=true);    //obsolete
  procedure FREDB_SiteMap_AddRadialEntry         (const SiteMapData : IFRE_DB_Object ; const key:string;const caption : String ; const icon : String ; InterAppLink : String; const newsCount:Integer=0; const enabled:Boolean=true);
  procedure FREDB_PositionSitemapEntry           (const angle : integer; const radius : integer; const origin_x, origin_y : integer; out x,y:integer);
  procedure FREDB_SiteMap_RadialAutoposition     (const SiteMapData : IFRE_DB_Object; const rootangle:integer=90);
  function  FREDB_GuidArray2String               (const arr:TFRE_DB_GUIDArray):String;

  function  FREDB_Get_Rightname_UID              (const rightprefix: string; const id: TGUID): string;
  function  FREDB_Get_Rightname_UID_STR          (const rightprefix: string; const id_str: String): string;
  procedure FREDB_SplitLocalatDomain             (const localatdomain: TFRE_DB_String; var localpart, domainpart: TFRE_DB_String);


  // This function should replace all character which should not a ppear in an ECMA Script (JS) string type to an escaped version,
  // as additional feature it replaces CR with a <br> tag, which is useful in formatting HTML
  function  FREDB_String2EscapedJSString         (const input_string:TFRE_DB_String;const replace_cr_with_br:boolean=false) : TFRE_DB_String;

  operator< (g1, g2: TGUID) b : boolean;
  operator> (g1, g2: TGUID) b : boolean;
  operator= (g1, g2: TGUID) b : boolean;

type
  TAddAppToSiteMap_Callback = procedure (const app : TFRE_DB_APPLICATION ; const session: TFRE_DB_UserSession; const parent_entry: TFRE_DB_CONTENT_DESC);

var
  GFRE_DBI                          : IFRE_DB;
  GFRE_DBI_REG_EXTMGR               : IFRE_DB_EXTENSION_MNGR;
  GFRE_DB_NIL_DESC                  : TFRE_DB_NIL_DESC;

  G_APPMODS_AS_SUBSECTIONS_CALLBACK : IFRE_DB_InvokeMethodCallbackObjectEx;
  G_ADD_2_SITEMAP_CALLBACK          : TAddAppToSiteMap_Callback;

implementation

var     FDbgTimer             : IFRE_APS_TIMER;

function RB_Guid_Compare(const d1, d2: TGuid): NativeInt;
begin
 if (TGUID_Access(d1).Part1=TGUID_Access(d2).Part1) then begin
   if (TGUID_Access(d1).Part2=TGUID_Access(d2).Part2) then begin
     result := 0;
   end else
   if (TGUID_Access(d1).Part2>TGUID_Access(d2).Part2) then begin
     result :=  1;
   end else begin
     result := -1;
   end;
 end else
 if (TGUID_Access(d1).Part1>TGUID_Access(d2).Part1) then begin
   result := 1;
 end else begin
   result := -1;
 end;
end;

function FREDB_FindStringIndexInArray(const text: TFRE_DB_String;  const strings: TFRE_DB_StringArray): integer;
var  i: Integer;
begin
  result := -1;
  for i:=0 to high(strings) do begin
    if text=strings[i] then exit(i);
  end;
end;

function FREDB_CombineString(const strings: TFRE_DB_StringArray; const sep: TFRE_DB_String): TFRE_DB_String;
var i:integer;
begin
  result := '';
  for i:=0 to high(strings)-1 do begin
    result := result+strings[i]+sep;
  end;
  if High(strings)>=0 then begin
    result := result + strings[high(strings)];
  end;
end;

procedure FREDB_SeperateString(const value, sep: TFRE_DB_String; var Strings: TFRE_DB_StringArray);
var SepLen       : Integer;
    F, P         : PChar;
    Index        : Integer;
    UniString    : AnsiString;
begin
  if Value = '' then exit;
  if Sep = '' then begin
    SetLength(Strings, 1);
    Strings[0] := Value;
    Exit;
  end;
  SepLen := Length(Sep);
  Index := 0;
  P := PChar(@Value[1]);
  while P^ <> #0 do  begin
    F := P;
    P := StrPos(P, PChar(@Sep[1]));
    if P = nil then P := StrEnd(F);
    if Index >= length(Strings) then SetLength(Strings, length(strings)+10);
    SetString(UniString, F, P - F);
    Strings[Index] := UniString;
    Inc(Index);
    if P^ <> #0 then Inc(P, SepLen);
  end;
  if Index < length(strings) then begin
    SetLength(Strings, Index);
  end;
end;

function FREDB_DBNameType_Compare(const S1, S2: TFRE_DB_NameType): NativeInt;
begin
  result := Default_RB_String_Compare(s1,s2);
end;

function FREDB_DBString_Compare(const S1, S2: TFRE_DB_String): NativeInt; // TODO:UnicodeCheck
begin
  result := Default_RB_String_Compare(s1,s2);
end;

function FREDB_DBint64_Compare(const S1, S2: int64): NativeInt;
begin
  result := s1-s2;
end;

function FREDB_DBuint64_Compare(const S1, S2: uint64): NativeInt;
begin
  result := s1-s2;
end;

function FREDB_FieldtypeShortString2Fieldtype(const fts: TFRE_DB_String): TFRE_DB_FIELDTYPE;
begin
  for result in TFRE_DB_FIELDTYPE do begin
     if CFRE_DB_FIELDTYPE_SHORT[result]=fts then exit;
  end;
  raise EFRE_DB_Exception.Create(edb_ERROR,'invalid short fieldtype specifier : ['+fts+']');
end;

function FREDB_Bool2String(const bool: boolean): String;
begin
  result := BoolToStr(bool,'1','0');
end;

function FREDB_String2GuidArray(const str: string): TFRE_DB_GUIDArray;
var sa : TFOSStringArray;
     i : NativeInt;
begin
  GFRE_BT.SeperateString(str,',',sa);
  SetLength(result,length(sa));
  for i:= 0 to high(sa) do
    result[i] := GFRE_BT.HexString_2_GUID(sa[i]);
end;

function FREDB_String2Guid(const str: string): TGUID;
begin
  result := GFRE_BT.HexString_2_GUID(str);
end;

function FREDB_String2Bool(const str: string): boolean;
begin
  if (str='1') or (UpperCase(str)='TRUE') then begin
    result := true;
  end else
  if (str='0') or (UpperCase(str)='FALSE') then begin
    result := false;
  end else raise EFRE_DB_Exception.Create(edb_ERROR,'invalid string to bool conversion : value=['+str+']');
end;

function FREDB_NumFilterType2String(const nft: TFRE_DB_NUM_FILTERTYPE): String;
begin
  result := CFRE_DB_NUM_FILTERTYPE[nft];
end;

function FREDB_String2NumfilterType(const str: string): TFRE_DB_NUM_FILTERTYPE;
begin
  for result in TFRE_DB_NUM_FILTERTYPE do begin
     if CFRE_DB_NUM_FILTERTYPE[result]=str then exit;
  end;
  raise EFRE_DB_Exception.Create(edb_ERROR,'invalid numfiltertype specifier : ['+str+']');
end;

function FREDB_String2StrFilterType(const str: string): TFRE_DB_STR_FILTERTYPE;
begin
  for result in TFRE_DB_STR_FILTERTYPE do begin
     if CFRE_DB_STR_FILTERTYPE[result]=str then exit;
  end;
  raise EFRE_DB_Exception.Create(edb_ERROR,'invalid stringfiltertype specifier : ['+str+']');
end;

function FREDB_StrFilterType2String(const sft: TFRE_DB_STR_FILTERTYPE): String;
begin
  result := CFRE_DB_STR_FILTERTYPE[sft];
end;

function FREDB_Guid_Same(const d1, d2: TGuid): boolean;
begin
  result := RB_Guid_Compare(d1,d2)=0;
end;

function FREDB_Guids_Same(const d1, d2: TGuid): boolean;
begin
  result := RB_Guid_Compare(d1,d2)=0;
end;

function FREDB_Guids_Compare(const d1, d2: TGuid): NativeInt;
begin
  result := RB_Guid_Compare(d1,d2);
end;

function FREDB_CheckGuidsUnique(const arr: TFRE_DB_GUIDArray): boolean;
var l_check : TFRE_DB_CountedGuidArray;
    i       : NativeInt;
begin
  l_check := FREDB_GuidList2Counted(arr);
  result  := true;
  for i:=0 to High(l_check) do
    if l_check[i].count>1 then
      exit(false);
end;

function FREDB_GuidList2Counted(const arr: TFRE_DB_GUIDArray; const stop_on_first_double: boolean): TFRE_DB_CountedGuidArray;
var i,j   : integer;
    found : integer;
    last  : integer;
begin
  SetLength(result,Length(arr));
  for i := 0 to high(arr) do begin
    found := -1;
    for j := 0 to high(result) do begin
      last := j;
      if Result[j].count=0 then break;
      if FREDB_Guids_Same(Result[j].link,arr[i]) then begin
        found := j;
        break;
      end;
    end;
    if found=-1 then begin
      result[last].count:=1;
      result[last].link:=arr[i];
    end else begin
      inc(result[found].count);
      if stop_on_first_double then break;
    end;
  end;
  found := 0;
  for i := 0 to high(result) do begin
    if result[i].count=0 then begin
      found:=i;
      break;
    end;
  end;
  SetLength(result,found);
end;

function FREDB_ObjReferences2GuidArray(const ref: TFRE_DB_ObjectReferences): TFRE_DB_GUIDArray;
var i,j : NativeInt;
    cnt : NativeInt;
begin
  cnt := 0;
  for i := 0 to high(ref) do
    cnt := cnt + Length(ref[i].linklist);
  SetLength(Result,cnt);
  cnt   := 0;
  for i := 0 to High(ref) do
    for j := 0 to high(ref[i].linklist) do
      begin
        Result[cnt] := ref[i].linklist[j];
        inc(cnt);
      end;
end;

function FREDB_Guids_SortPredicate(const d1, d2: TGuid): boolean;
var res : NativeInt;
begin
  res    := RB_Guid_Compare(d1,d2);
  result := res=-1;
end;

function FREDB_ObjectToPtrUInt(const obj: TObject): PtrUInt;
begin
  result := PtrUInt(obj);
end;

function FREDB_PtrUIntToObject(const obj: PtrUInt): TObject;
begin
  result := TObject(obj);
end;

function FREDB_String2DBDisplayType(const fts: string): TFRE_DB_DISPLAY_TYPE;
begin
  for result in TFRE_DB_DISPLAY_TYPE do begin
     if CFRE_DB_DISPLAY_TYPE[result]=fts then exit;
  end;
  raise Exception.Create('invalid short DBDisplayType specifier : ['+fts+']');
end;

function FieldtypeShortString2Fieldtype(const fts: TFRE_DB_String): TFRE_DB_FIELDTYPE;
begin
  for result in TFRE_DB_FIELDTYPE do begin
     if CFRE_DB_FIELDTYPE_SHORT[result]=fts then exit;
  end;
  raise EFRE_DB_Exception.Create(edb_ERROR,'invalid short fieldtype specifier : ['+fts+']');
end;

procedure CheckDbResult(const res:TFRE_DB_Errortype;const error_string : TFRE_DB_String ; const append_errorcode : boolean = false);
begin
  if res<>edb_OK then begin
    if append_errorcode then begin
      raise EFRE_DB_Exception.Create(res,error_string+' : '+CFRE_DB_Errortype[res]);
    end else begin
      raise EFRE_DB_Exception.Create(res,error_string);
    end;
  end;
end;


procedure CheckDbResultFmt(const res: TFRE_DB_Errortype; const error_string: TFRE_DB_String; const params: array of const);
begin
  if res<>edb_OK then begin
    raise EFRE_DB_Exception.Create(res,error_string,params);
  end;
end;

type
   tmethodnamerec = packed record
      name : pshortstring;
      addr : pointer;
   end;

   tmethodnametable = packed record
     count : dword;
     entries : packed array[0..0] of tmethodnamerec;
   end;

   pmethodnametable =  ^tmethodnametable;

{ TFRE_DB_NOTE }

class procedure TFRE_DB_NOTE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.Classname);
//  scheme.AddSchemeField('linkid',fdbft_ObjLink).required:=true;
  scheme.AddSchemeField('link',fdbft_String).required:=true;
  scheme.AddSchemeField('note',fdbft_String);
end;

class procedure TFRE_DB_NOTE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
end;

{ TFRE_DB_EDITOR_DATA_DESC }

function TFRE_DB_EDITOR_DATA_DESC.Describe(const value: String): TFRE_DB_EDITOR_DATA_DESC;
begin
  Field('value').AsString:=value;
  Result:=Self;
end;

{ TFRE_DB_Stream }

destructor TFRE_DB_Stream.Destroy;
begin
  inherited Destroy;
end;

{ TFRE_DB_CLOSE_DIALOG_DESC }

function TFRE_DB_CLOSE_DIALOG_DESC.Describe: TFRE_DB_CLOSE_DIALOG_DESC;
begin
  Result:=Self;
end;

{ TFRE_DB_MESSAGE_DESC }

function TFRE_DB_MESSAGE_DESC.Describe(const caption, msg: String; const msgType: TFRE_DB_MESSAGE_TYPE; const serverFunc: TFRE_DB_SERVER_FUNC_DESC): TFRE_DB_MESSAGE_DESC;
begin
  if not FieldExists('id') then begin
    Field('id').AsString:='id'+UID_String;
  end;
  Field('caption').AsString:=caption;
  Field('msg').AsString:=msg;
  Field('msgType').AsString:=CFRE_DB_MESSAGE_TYPE[msgType];
  if Assigned(serverFunc) then begin
    Field('serverFunc').AsObject:=serverFunc;
  end;
  Result:=Self;
end;

{ TFRE_DB_REFRESH_STORE_DESC }

function TFRE_DB_REFRESH_STORE_DESC.Describe(const storeId: String): TFRE_DB_REFRESH_STORE_DESC;
begin
  Field('storeId').AsString:=storeId;
  Result:=Self;
end;

constructor TFRE_DB_NIL_DESC.Create;
begin
  inc(NILInstances);
  if NILInstances>1 then begin
    GFRE_BT.CriticalAbort('THIS IS A SINGLETON, ONLY CREATE ONCE!');
  end;
  Inherited Create;
end;

destructor TFRE_DB_NIL_DESC.Destroy;
begin
  exit;
  // ignore destroy of the nil description
end;

procedure TFRE_DB_NIL_DESC.DestroySingleton;
begin
  Inherited Destroy;
end;

function TFRE_DB_NIL_DESC.CFG_Dont_Finalize_Object: Boolean;
begin
  Result:=true;
end;

{ TFRE_DB_NIL_DESC }


{ TFRE_DB_GUID_MANAGER }


{ TFRE_DB_BE_THREAD }

//procedure TFRE_DB_BE_THREAD.Execute;
//begin
//
//end;

procedure TFRE_DB_UserSession.SetOnGetImpersonatedDBC(AValue: TFRE_DB_OnGetImpersonatedConnection);
begin
  FOnGetImpersonatedDBC:=AValue;
end;


procedure TFRE_DB_UserSession.SetOnRestoreDefaultDBC(AValue: TFRE_DB_OnRestoreDefaultConnection);
begin
  FOnRestoreDefaultDBC:=AValue;
end;

procedure TFRE_DB_UserSession.SetOnExistsUserSession(AValue: TFRE_DB_OnExistsUserSessionForUser);
begin
  FOnExistsUserSession:=AValue;
end;

procedure TFRE_DB_UserSession.SetOnCheckUserNamePW(AValue: TFRE_DB_OnCheckUserNamePassword);
begin
  FOnCheckUserNamePW:=AValue;
end;

procedure TFRE_DB_UserSession.SetOnWorkCommands(AValue: TNotifyEvent);
begin
  FOnWorkCommands:=AValue;
end;

procedure TFRE_DB_UserSession._FixupDCName(var dcname: TFRE_DB_NameType);
begin
  dcname := 'dc'+uppercase(dcname)+FSessionID;
end;


constructor TFRE_DB_UserSession.Create(const user_name, password: TFRE_DB_String; const default_app: TFRE_DB_String;const default_uid_path : TFRE_DB_GUIDArray; conn: IFRE_DB_CONNECTION);
begin
  GFRE_TF.Get_LFQ(FSessionQ);
  FUserName     := user_name;
  FDefaultApp   := default_app;
  FDBConnection := conn;
  FDefaultUID   := default_uid_path;
  FSessionID    := 'S'+GFRE_DBI.Get_A_Guid_HEX;
  _FetchAppsFromDB;
  _InitApps;
end;

procedure TFRE_DB_UserSession.StoreSessionData;
var res : TFRE_DB_Errortype;
begin
  GFRE_DBI.LogDebug(dblc_SESSION,'STORING SESSIONDATA FOR ['+FUserName+']');
  if assigned(FSessionData) then begin
    res := GetDBConnection.StoreUserSessionData(FSessionData);
    if res=edb_OK then begin
      GFRE_DBI.LogInfo(dblc_SESSION,'STORING SESSIONDATA FOR ['+FUserName+'] DONE ');
    end else begin
      GFRE_DBI.LogInfo(dblc_SESSION,'STORING SESSIONDATA FOR ['+FUserName+'] FAILED --> '+CFRE_DB_Errortype[res]);
    end;
    //writeln;
    //writeln;
    //writeln(FSessionData.DumpToString);
    //writeln;
    //writeln;
  end else begin
    GFRE_DBI.LogWarning(dblc_SESSION,'NO SESSIONDATA FOR ['+FUserName+']');
  end;
end;

destructor TFRE_DB_UserSession.Destroy;
begin
  StoreSessionData;
  if FPromoted then begin
    FDBConnection.Finalize;
  end;
  GFRE_DBI.LogInfo(dblc_SESSION,'FINALIZED USERSESSION [%s] USER [%s]',[FSessionID,FUserName]);
  inherited Destroy;
end;

function TFRE_DB_UserSession.SearchSessionApp(const app_key: TFRE_DB_String; out app: TFRE_DB_APPLICATION; out idx: integer): boolean;
var  i: Integer;
begin
  app:=nil;
  idx := -1;
  for i:=0 to high(FAppArray) do begin
     if not assigned(FAppArray[i]) then continue; // Skip empty Slots
     if uppercase(FAppArray[i].ObjectName)=uppercase(app_key) then begin
       idx := i;
       app := FAppArray[i].AsObject.Implementor_HC as TFRE_DB_APPLICATION;
       exit(true);
     end;
  end;
  result := false;
end;

function TFRE_DB_UserSession.SearchSessionDC(dc_name: TFRE_DB_String; out dc: IFRE_DB_DERIVED_COLLECTION): boolean;
var  i: Integer;
begin
  dc:=nil;
  for i:=0 to high(FDC_Array) do begin
     if FDC_Array[i].CollectionName=dc_name then begin
       dc := FDC_Array[i];
       exit(true);
     end;
  end;
  result := false;
end;

procedure TFRE_DB_UserSession._FetchAppsFromDB;
begin
  FDBConnection.FetchApplications(FAppArray);
  //procedure InitApp(const app:TFRE_DB_APPLICATION);
  //var global_app : TFRE_DB_APPLICATION;
  //    idx        : integer;
  //begin
  //  if not session.SearchSessionApp(app.ObjectName,global_app,idx) then begin
  //    if not GFRE_DB.GetApp(app.ObjectName,global_app) then raise EFRE_DB_Exception.Create(edb_ERROR,'COULD NOT FIND APPLICATION [%s]',[app.ObjectName]);
  //    session.AddSessionAppAndInit(app.ObjectName,global_app);
  //    session.SetCurrentApp(app.ObjectName);
  //  end;
  //end;
end;

procedure TFRE_DB_UserSession._InitApps;
var i:integer;
begin
  for i := 0 to High(FAppArray) do begin
    if FcurrentApp='' then begin
      SetCurrentApp(FAppArray[i].ObjectName);
    end;
    (FAppArray[i].AsObject.Implementor_HC as TFRE_DB_APPLICATION).SessionInitialize(self);
    GFRE_DBI.LogInfo(dblc_SESSION,'SESSION INIT APP [%s/%s] to [%s]',[FAppArray[i].ObjectName,FAppArray[i].AsObject.UID_String,GetSessionID]);
  end;
end;

procedure TFRE_DB_UserSession.dbg_TaskMethodDisp(const ES: IFRE_APS_EVENTSOURCE; const TID: integer; const Data: Pointer; const cp: integer);
var ev:TFRE_DB_EVENT_METHOD_ENC;
begin
 if assigned(FDbgTaskMethod) then begin
   ev := TFRE_DB_EVENT_METHOD_ENC.Create;
   ev.method := FDbgTaskMethod;
   ev.params := GFRE_DBI.NewObject;
   Input_FRE_DB_Event(ev);
 end;
end;

function TFRE_DB_UserSession.SearchSessionAppUID(const app_uid: TGUID; out app: IFRE_DB_Object): boolean;
var  i: Integer;
begin
  for i:=0 to high(FAppArray) do begin
     if not assigned(FAppArray[i]) then continue; // Skip empty Slots
     if FREDB_Guids_Same(FAppArray[i].UID,app_uid) then begin
       app := FAppArray[i].AsObject;
       exit(true);
     end;
  end;
  result := false;
end;

function TFRE_DB_UserSession.SearchSessionDCUID(const dc_uid: TGUID; out dc: IFRE_DB_DERIVED_COLLECTION): boolean;
var  i: Integer;
begin
  for i:=0 to high(FDC_Array) do begin
     if FREDB_Guids_Same(FDC_Array[i].UID,dc_uid) then begin
       dc := FDC_Array[i];
       exit(true);
     end;
  end;
  result := false;
end;


procedure TFRE_DB_UserSession.RemSessionAppAndFinialize(const app_key: TFRE_DB_String);
var app : TFRE_DB_APPLICATION;
    idx : integer;
begin
  if not SearchSessionApp(app_key,app,idx) then raise EFRE_DB_Exception.Create(edb_ERROR,'want to remove/finalize a nonexisting app');
  FAppArray[idx] := nil;
  app.SessionFinalize(self);
end;

procedure TFRE_DB_UserSession.RemoveAllAppsandFinalize;
var  i       : Integer;
     applist : Array of String;
begin
  setlength(applist,length(FAppArray));
  for i:=0 to high(FAppArray) do begin
    applist[i] := FAppArray[i].ObjectName;
  end;
  for i:=0 to high(applist) do begin
    RemSessionAppAndFinialize(applist[i]);
  end;
  setlength(FAppArray,0);
end;

procedure TFRE_DB_UserSession.SetCurrentApp(const app_key: TFRE_DB_String);
begin
  FcurrentApp:=app_key;
end;


procedure TFRE_DB_UserSession.Input_FRE_DB_Command(const cmd: IFRE_DB_COMMAND);
begin
  FSessionQ.Push(cmd.Implementor_HC);
  FOnWorkCommands(self);
end;

procedure TFRE_DB_UserSession.Input_FRE_DB_Event(const cmd_event: TFRE_DB_EVENT_METHOD_ENC);
begin
  FSessionQ.Push(cmd_event);
  FOnWorkCommands(self);
end;


function TFRE_DB_UserSession.Session_Has_CMDS: Boolean;
begin
  result := FSessionQ.SomethingOnQ>0;
end;

function TFRE_DB_UserSession.WorkSessionCommand:IFRE_DB_COMMAND;
var x           : TObject;
    cmd         : IFRE_DB_COMMAND;
    class_name  : TFRE_DB_String;
    method_name : TFRE_DB_String;
    request_id  : int64;
    request_typ : TFRE_DB_COMMANDTYPE;
    input       : IFRE_DB_Object;
    output      : IFRE_DB_Object;
    uidp        : TFRE_DB_GUIDArray;
    session     : TFRE_DB_String;
    session_app : TFRE_DB_APPLICATION;
    dummy_idx   : integer;
    i           : Integer;

    procedure InvokeMethod(const async:boolean);
    var st,et : QWord;
    begin
      st := GFRE_BT.Get_Ticks_ms;
      GFRE_DBI.LogDebug(dblc_SERVER,'>>DISPATCH METHOD %s[%s].%s() RID = [%d] TYPE[%s]',[class_name,GFRE_DBI.GuidArray2SString(uidp),method_name,request_id,CFRE_DB_COMMANDTYPE[request_typ]]);
      try
        GFRE_DBI.LogDebug(dblc_SERVER,'INPUT:');
        GFRE_DBI.LogDebug(dblc_SERVER,'%s',[input.DumpToString(2)]);
        output := nil;
        if method_name='ONUICHANGE' then begin
          output:=nil;
        end;
        output          := FDBConnection.InvokeMethod(class_name,method_name,uidp,input,self);
        if output=nil then begin
          raise EFRE_DB_Exception.Create('function delivered nil result');
        end;
        CMD.Data        := output;
        if request_typ = fct_SyncRequest then begin
          CMD.CommandType := fct_SyncReply;
        end;
        GFRE_DBI.LogDebug(dblc_SERVER,'OUTPUT:');
        GFRE_DBI.LogDebug(dblc_SERVER,'%s',[output.DumpToString(2)]);
      except on e:exception do begin
        et := GFRE_BT.Get_Ticks_ms;
        GFRE_DBI.LogError(dblc_SERVER,'>>(%4.4d ms)<<DISPATCH METHOD %s(%s).%s RID = [%d] TYPE[%s] FAILED[%s]',[et-st,class_name,GFRE_DBI.GuidArray2SString(uidp),method_name,request_id,CFRE_DB_COMMANDTYPE[request_typ],e.Message]);
        CMD.CheckoutData.Finalize;
        if not async then begin
          CMD.CommandType := fct_Error;
          CMD.Answer      := true;
          CMD.ErrorText   := 'INVOKE OF ['+class_name+'.'+method_name+'] FAILED '+#13#10+'['+e.Message+']';
        end;
      end;end;
      GFRE_DBI.LogDebug(dblc_SERVER,'<<DISPATCH METHOD %s(%s).%s RID = [%d] TYPE[%s] DONE',[class_name,GFRE_DBI.GuidArray2SString(uidp),method_name,request_id,CFRE_DB_COMMANDTYPE[request_typ]]);
      et := GFRE_BT.Get_Ticks_ms;
      GFRE_DBI.LogDebug(dblc_SERVER,'>>(%4.4d ms)<<DISPATCH METHOD %s(%s).%s RID = [%d] TYPE[%s] SID[%s] DONE',[et-st,class_name,GFRE_DBI.GuidArray2SString(uidp),method_name,request_id,CFRE_DB_COMMANDTYPE[request_typ],FSessionID]);
    end;

    procedure InvokeEvent(event :TFRE_DB_EVENT_METHOD_ENC);
    begin
      try
        event.params.SetReference(self);
        try
          event.method(event.params);
        finally
          event.params.Finalize;
          event.Free;
        end;
      finally
      end;
    end;

begin
  x := TObject(FSessionQ.Pop);
  if not(assigned(x)) then exit;
  if x is TFRE_DB_EVENT_METHOD_ENC then begin
    result := nil;
    try
      InvokeEvent(TFRE_DB_EVENT_METHOD_ENC(x));
    except on e:exception do begin
      writeln('EVENT INVOKE ERROR ',e.Message);
    end;end;
  end else begin
    if not x.GetInterface(IFRE_DB_COMMAND,cmd) then GFRE_BT.CriticalAbort('logic');
      result := CMD;
      with cmd do begin
        class_name  := InvokeClass;
        method_name := InvokeMethod;
        request_id  := CommandID;
        request_typ := CommandType;
        uidp        := UidPath;
        input       := CMD.CheckoutData;
      end;
      case request_typ of
        fct_SyncRequest:  begin
                            try
                              if (class_name='FIRMOS') then begin
                                  if (method_name='INIT') then begin
                                       if not FPromoted and (cG_OVERRIDE_USER<>'') and (cG_OVERRIDE_PASS<>'') then begin // AutoLogin
                                         GFRE_DBI.LogInfo(dblc_SERVER,'>> SPECIFIC INVOKE FIRMOS.INIT  AUTOLOGIN  SID[%s] USER[%s]',[FSessionID,cG_OVERRIDE_USER]);
                                         input.Field('data').AsObject.Field('uname').AsString := cG_OVERRIDE_USER;
                                         input.Field('data').AsObject.Field('pass').AsString  := cG_OVERRIDE_PASS;
                                         class_name  := FDefaultApp;
                                         uidp        := FDefaultUID;
                                         method_name := 'doLogin';
                                         InvokeMethod(false);
                                         CMD.ChangeSession := FSessionID;
                                       end else begin
                                         GFRE_DBI.LogInfo(dblc_SERVER,'>> SPECIFIC INVOKE FIRMOS.INIT  SID[%s]',[FSessionID]);
                                         class_name  := FDefaultApp;
                                         uidp        := FDefaultUID;
                                         method_name := 'Content';
                                         InvokeMethod(false);
                                         CMD.ChangeSession := FSessionID;
                                       end;
                                  end else
                                  if (method_name='LOGOUT') then begin
                                    GFRE_DBI.LogInfo(dblc_SERVER,'>> SPECIFIC INVOKE FIRMOS.LOGOUT  SID[%s]',[FSessionID]);
                                    Logout;
                                    GFRE_DBI.LogInfo(dblc_SERVER,'>> SENDBACK CONTENT AFTER LOGOUT -> (%s)  SID[%s]',[FDefaultApp,FSessionID]);
                                    class_name  := FDefaultApp;
                                    uidp        := FDefaultUID;
                                    method_name := 'Content';
                                    InvokeMethod(false);
                                    CMD.ChangeSession := 'LOGGEDOUT';
                                  end else
                                  if (method_name='RELOAD') then begin  //TODO: DOES NOT WORK
                                    GFRE_DBI.LogInfo(dblc_SERVER,'>> SPECIFIC INVOKE FIRMOS.RELOAD  SID[%s]',[FSessionID]);
                                    CMD.Data        := GFRE_DB_NIL_DESC;   //TODO: RELOAD DESCRIPTION
                                    CMD.CommandType := fct_SyncRequest;
                                  end else
                                  if (method_name='DESTROY') then begin
                                    GFRE_DBI.LogInfo(dblc_SERVER,'>> SPECIFIC INVOKE FIRMOS.DESTROY  SID[%s]',[FSessionID]);
                                    for i := 0 to input.Field('ids').ValueCount - 1 do begin
                                      unregisterUpdatableContent(input.Field('ids').AsStringItem[i]);
                                    end;
                                    CMD.Data        := GFRE_DB_NIL_DESC;
                                    CMD.CommandType := fct_SyncRequest;
                                  end else
                                  if (method_name='UNREGISTERDBO') then begin
                                    GFRE_DBI.LogInfo(dblc_SERVER,'>> SPECIFIC INVOKE FIRMOS.UNREGISTERDBO  SID[%s]',[FSessionID]);
                                    for i := 0 to input.Field('ids').ValueCount - 1 do begin
                                      unregisterUpdatableDBO(input.Field('ids').AsStringItem[i]);
                                    end;
                                    CMD.Data        := GFRE_DB_NIL_DESC;
                                    CMD.CommandType := fct_SyncRequest;
                                  end else begin
                                    raise EFRE_DB_Exception.Create(edb_ERROR,'UNKNOWN FIRMOS SPECIFIC COMMAND '+method_name);
                                  end;
                              end else begin
                                 InvokeMethod(false);
                              end;
                            except on e:Exception do begin
                             cmd.CommandType := fct_Error;
                             cmd.ErrorText   := e.Message;
                            end;end;
                            cmd.SetAnswerInterface(FBoundSession_RA_SC);
                          end;
        fct_AsyncRequest: begin
                            InvokeMethod(true);
                            CMD.Finalize;
                            result:=nil;
                          end;
        fct_SyncReply:    begin
                            //raise EFRE_DB_Exception.Create(edb_ERROR,'ONLY SYNC REQUESTS IMPLEMENTED,ATM');
                            request_id:=request_id;
                            CMD.Finalize;
                            result:=nil;
                          end;
      end;
  end;
end;

function TFRE_DB_UserSession.InternalSessInvokeMethod(const class_name, method_name: string; const uid_path: TFRE_DB_GUIDArray; const input: IFRE_DB_Object): IFRE_DB_Object;
var st,et : QWord;
begin
  st := GFRE_BT.Get_Ticks_ms;
  GFRE_DBI.LogDebug(dblc_SERVER,'>>SESSION/INTERNAL/DISPATCH METHOD %s.%s(%s)  SID=[%s]',[class_name,method_name,GFRE_DBI.GuidArray2SString(uid_path),FSessionID]);
  if assigned(input) then begin
    input.SetReference(self);
    GFRE_DBI.LogDebug(dblc_SERVER,'INPUT:');
    GFRE_DBI.LogDebug(dblc_SERVER,'%s',[input.DumpToString(2)]);
  end;
  try
    result := FDBConnection.InvokeMethod(class_name,method_name,uid_path,input,self);
    if assigned(result) then begin
      GFRE_DBI.LogDebug(dblc_SERVER,'OUTPUT:');
      GFRE_DBI.LogDebug(dblc_SERVER,'%s',[result.DumpToString(2)]);
    end;
    if assigned(input) then begin
      input.SetReference(nil);
    end;
  except on e:exception do begin;
    GFRE_DBI.LogError(dblc_SERVER,'INTERNAL DISPATCH METHOD %s.%s(%s) FAILED[%s] SID=[%s]',[class_name,method_name,GFRE_DBI.GuidArray2SString(uid_path),e.Message,FSessionID]);
    raise;
  end;end;
  GFRE_DBI.LogDebug(dblc_SERVER,'<<SESSION/INTERNAL/DISPATCH METHOD %s.%s(%s) SID=[%s]',[class_name,method_name,GFRE_DBI.GuidArray2SString(uid_path),FSessionID]);
  et := GFRE_BT.Get_Ticks_ms;
  GFRE_DBI.LogDebug(dblc_SERVER,'>>(%4.4d ms)<<SESSION/INTERNAL/DISPATCH METHOD %s.%s(%s) SID=[%s]',[et-st,class_name,method_name,GFRE_DBI.GuidArray2SString(uid_path),FSessionID]);
end;

function TFRE_DB_UserSession.InternalSessInvokeMethod(const obj: IFRE_DB_Object; const method_name: string; const input: IFRE_DB_Object): IFRE_DB_Object;
var inp:IFRE_DB_Object;
begin
  if not assigned(input) then begin
    inp := GFRE_DBI.NewObject;
    inp.SetReference(self);
    try
      abort;
      result := obj.Invoke(method_name,inp,self,nil,nil);
    finally
      inp.Finalize;
    end;
  end else begin
    input.SetReference(self);
    try
      abort;
      result := obj.Invoke(method_name,input,self,nil,nil);
    finally
      input.SetReference(nil);
    end;
  end;
end;

function TFRE_DB_UserSession.CloneSession(const connectiond_desc: string): TFRE_DB_UserSession;
begin
  result := TFRE_DB_UserSession.Create(FUserName,FPassMD5,FDefaultApp,FDefaultUID,FDBConnection);
  result.OnGetImpersonatedDBC := FOnGetImpersonatedDBC;
  result.OnExistsUserSession  := FOnExistsUserSession;
  result.OnRestoreDefaultDBC  := FOnRestoreDefaultDBC;
  result.OnCheckUserNamePW    := FOnCheckUserNamePW;
  result.FSessionData         := GFRE_DBI.NewObject;
  result.FConnDesc            := connectiond_desc;
end;

function TFRE_DB_UserSession.Promote(const user_name, password: TFRE_DB_String; var promotion_error: TFRE_DB_String; const force_new_session_data: boolean; const session_takeover: boolean; out take_over_content: TFRE_DB_CONTENT_DESC): TFRE_DB_PromoteResult;
var err              : TFRE_DB_Errortype;
    l_NDBC             : IFRE_DB_CONNECTION;
    existing_session   : TFRE_DB_UserSession;
    lStoredSessionData : IFRE_DB_Object;

    procedure ReinitializeApps;
    var i:integer;
    begin
      for i:=0 to high(FAppArray) do begin
        (FAppArray[i].Implementor_HC as TFRE_DB_APPLICATION).SessionPromotion(self);
      end;
    end;

begin
    FOnExistsUserSession(user_name,existing_session);
    if session_takeover and assigned(existing_session) then begin
      err := FOnCheckUserNamePW(user_name,password);
      case err of
        edb_OK : begin
          existing_session.InitiateTakeover(FBoundSession_RA_SC,take_over_content,FConnDesc);
          result:=pr_Takeover;
        end;
        else begin
          promotion_error := 'Takeover Failed : '+CFRE_DB_Errortype[err];
          result          := pr_Failed;
        end;
      end;
      exit;
    end;
    if assigned(existing_session) then begin
        FPromoted       := false;
        promotion_error := 'Sorry you cannot be logged in because you are already logged in on : '+existing_session.GetClientDetails;
        result          := pr_Failed;
    end else begin
      err := FOnGetImpersonatedDBC(FDBConnection.GetDatabaseName,user_name,password,l_NDBC);
      assert(assigned(FSessionData));
      case err of
       edb_OK: begin
          FDBConnection:=l_NDBC;
          GFRE_DBI.LogInfo(dblc_SERVER,'PROMOTED SESSION [%s] USER [%s] TO [%s]',[FSessionID,FUserName,user_name]);
          if not force_new_session_data then begin
            if not l_NDBC.FetchUserSessionData(lStoredSessionData) then begin
              GFRE_DBI.LogDebug(dblc_SERVER,'USING EMPTY/DEFAULT SESSION DATA [%s]',[FSessionData.UID_String]);
            end else begin
              GFRE_DBI.LogDebug(dblc_SERVER,'USING PERSISTENT SESSION DATA [%s]',[FSessionData.UID_String]);
              FSessionData.Finalize;
              FSessionData:=lStoredSessionData;
            end;
          end else begin
            GFRE_DBI.LogDebug(dblc_SERVER,'FORCED USING EMPTY/DEFAULT SESSION DATA [%s]',[FSessionData.UID_String]);
          end;
          FUserName := user_name;
          FPromoted := true;
          result    := pr_OK;
          _FetchAppsFromDB;
          _InitApps;
          ReinitializeApps;
       end;
       else begin
         FPromoted       := false;
         promotion_error := CFRE_DB_Errortype[err];
         result          := pr_Failed;
       end;
      end;
    end;
end;

procedure TFRE_DB_UserSession.InitiateTakeover(const NEW_RASC: IFRE_DB_COMMAND_REQUEST_ANSWER_SC; out take_over_content: TFRE_DB_CONTENT_DESC; const connection_desc: string);
var
  MSG  : TFRE_DB_MESSAGE_DESC;
  APP  : TFRE_DB_APPLICATION;
  sId  : String;
  idx  : integer;
begin
  if not FIsInteractive  then begin
    if assigned(FBoundSession_RA_SC) then begin  //TODO - Handle interactive Session
      FBoundSession_RA_SC.DeactivateSessionBinding;
    end;
    FBoundSession_RA_SC := NEW_RASC;
    NEW_RASC.UpdateSessionBinding(self);
  end else begin
    MSG := TFRE_DB_MESSAGE_DESC.create.Describe('SESSION TAKEOVER','This session will be continued on another browser instance.',fdbmt_info,nil);
    sId := FSessionID;
    SendServerClientRequest(msg,'OLD-REVOKED');
    //---
    FConnDesc := connection_desc;
    NEW_RASC.UpdateSessionBinding(self);
    FBoundSession_RA_SC := NEW_RASC;
    if SearchSessionApp(FcurrentApp,app,idx) then begin
      SendServerClientRequest(GFRE_DB_NIL_DESC,sId);
      take_over_content := InternalSessInvokeMethod(App,'CONTENT',nil).Implementor_HC as TFRE_DB_CONTENT_DESC;
    end else begin
      MSG := TFRE_DB_MESSAGE_DESC.create.Describe('SESSION TAKEOVER FAILED','Sorry',fdbmt_error);
      SendServerClientRequest(msg,'BAD-ERROR');
    end;
  end;
end;

procedure TFRE_DB_UserSession.Demote;
var user_name:string;
begin
  if FPromoted then begin
     RemoveAllAppsAndFinalize;
     FinishDerivedCollections;
     user_name := FUserName;
     FPromoted:=false;
     FDBConnection.Finalize; // Only a promoted Connection is Diversified, The "guest" default account should only use one DBC
     FOnRestoreDefaultDBC(FUserName,FDBConnection);  // after restoration of username, the session wont be found
     GFRE_DBI.LogInfo(dblc_SERVER,'DEMOTED SESSION [%s] USER [%s] TO [%s]',[FSessionID,user_name,FUserName]);
  end else begin
     raise EFRE_DB_Exception.Create(edb_ERROR,'CANNOT DEMOTE A UNPROMOTED SESSION FOR SID [%s] and USER [%s]',[FSessionID,FUserName]);
  end;
end;

procedure TFRE_DB_UserSession.Logout;
begin
  Demote;
end;

function TFRE_DB_UserSession.LoggedIn: Boolean;
begin
  result := FPromoted;
end;

function TFRE_DB_UserSession.QuerySessionDestroy: Boolean;
begin
  result := true;
  GFRE_DBI.LogDebug(dblc_SERVER,'QUERY DISCONNECTING SESSION [%s] DESTROY=%s',[FSessionID,BoolToStr(result,'1','0')]);
end;

function TFRE_DB_UserSession.GetSessionID: TFRE_DB_String;
begin
  result := FSessionID;
end;

function TFRE_DB_UserSession.GetSessionAppData(const app_key: TFRE_DB_String): IFRE_DB_Object;
begin
  assert(assigned(FSessionData));
  result := FSessionData.Field('APP_DATA_'+app_key).AsObject;
end;

function TFRE_DB_UserSession.GetSessionModuleData(const mod_key: TFRE_DB_String): IFRE_DB_Object;
begin
  result := FSessionData.Field('MOD_DATA_'+mod_key).AsObject;
end;

function TFRE_DB_UserSession.GetSessionGlobalData: IFRE_DB_Object;
begin
  result := FSessionData.Field('G_').AsObject;
end;

function TFRE_DB_UserSession.NewDerivedCollection(dcname: TFRE_DB_NameType): IFRE_DB_DERIVED_COLLECTION;
begin
  _FixupDCName(dcname);
  if not SearchSessionDC(dcname,result) then begin
    GetDBConnection.CollectionAsIntf(dcname,IFRE_DB_DERIVED_COLLECTION,result);
    result.BindSession(self);
    if dcname<>Result.CollectionName then
      raise EFRE_DB_Exception.Create(edb_ERROR,'PARANOIA '+dcname+' <> '+result.CollectionName);
    SetLength(FDC_Array,Length(FDC_Array)+1);
    FDC_Array[high(FDC_Array)] := result;
  end;
end;

function TFRE_DB_UserSession.FetchDerivedCollection(dcname: TFRE_DB_NameType): IFRE_DB_DERIVED_COLLECTION;
begin
  _FixupDCName(dcname);
  if not SearchSessionDC(dcname,result) then raise EFRE_DB_Exception.create(edb_ERROR,'THE SESSION [%s] HAS NO DERIVED COLLECTION NAMED [%s]',[FSessionID,dcname]);
end;

procedure TFRE_DB_UserSession.FinishDerivedCollections;
var i:integer;
begin
  for i:=0 to high(FDC_Array) do begin
    try
      GetDBConnection.DeleteCollection(FDC_Array[i].CollectionName);
    except on e:EXception do begin
      writeln('*** --- MAKE A GOD LOG ENTRY ',e.Message);   //TODO: LOOK LEFT
    end;end;
  end;
  SetLength(FDC_Array,0);
end;

function TFRE_DB_UserSession.GetUsername: String;
begin
  result := FUserName;
end;

function TFRE_DB_UserSession.GetClientDetails: String;
begin
  result := FConnDesc;
end;

function TFRE_DB_UserSession.GetSessionAppArray: IFRE_DB_APPLICATION_ARRAY;
var i:integer;
begin
  SetLength(result,length(FAppArray));
  for i:=0 to high(FAppArray) do begin
    result[i] := FAppArray[i];
  end;
end;

procedure TFRE_DB_UserSession.SetServerClientInterface(const sc_interface: IFRE_DB_COMMAND_REQUEST_ANSWER_SC ; const interactive_session: boolean);
begin
  FBoundSession_RA_SC := sc_interface;
  FIsInteractive      := interactive_session;
end;

procedure TFRE_DB_UserSession.ClearServerClientInterface;
begin
  FBoundSession_RA_SC:=nil;
end;

procedure TFRE_DB_UserSession.registerUpdatableContent(const contentId: String);
begin
  FSessionData.Field('contentIds').AsObject.Field(contentId).AsBoolean:=True;
end;

procedure TFRE_DB_UserSession.unregisterUpdatableContent(const contentId: String);
begin
  FSessionData.Field('contentIds').AsObject.Field(contentId).Clear();
end;

procedure TFRE_DB_UserSession.registerUpdatableDBO(const id: String);
begin
  if FSessionData.Field('dboIds').AsObject.FieldExists(id) then begin
    FSessionData.Field('dboIds').AsObject.Field(id).AsInt16:=FSessionData.Field('dboIds').AsObject.Field(id).AsInt16+1;
  end else begin
    FSessionData.Field('dboIds').AsObject.Field(id).AsInt16:=1;
  end;
end;

procedure TFRE_DB_UserSession.unregisterUpdatableDBO(const id: String);
begin
  if FSessionData.Field('dboIds').AsObject.Field(id).AsInt16=1 then begin
    FSessionData.Field('dboIds').AsObject.Field(id).Clear();
  end else begin
    FSessionData.Field('dboIds').AsObject.Field(id).AsInt16:=FSessionData.Field('dboIds').AsObject.Field(id).AsInt16-1;
  end;
end;

function TFRE_DB_UserSession.isUpdatableContentVisible(const contentId: String): Boolean;
begin
  Result:=FSessionData.Field('contentIds').AsObject.FieldExists(contentId);
end;


procedure TFRE_DB_UserSession.SendServerClientRequest(const description: TFRE_DB_CONTENT_DESC;const session_id:String);
var CMD        : IFRE_DB_COMMAND;
    request_id : Int64;
begin
  cmd  := GFRE_DBI.NewDBCommand;
  cmd.SetIsClient(false);
  cmd.SetIsAnswer(True);
  request_id := random(100000);
  cmd.SetCommandID(random(100000));
  cmd.CommandType:=fct_SyncRequest;
  cmd.Data := description;
  if session_id<>'' then begin
    cmd.ChangeSession:=session_id;
  end;
  GFRE_DBI.LogInfo(dblc_SESSION,'>>SERVER CLIENT REQUEST (%s) RID = [%d] TYPE[%s] SID=%s CHANGE SID=%s',[description.ClassName,request_id,CFRE_DB_COMMANDTYPE[cmd.CommandType],FSessionID,cmd.ChangeSession]);
  try
    if assigned(FBoundSession_RA_SC) then
      begin
        FBoundSession_RA_SC.Send_ServerClient(cmd);
      end
    else
      begin
        writeln('BOUND SESSION RAC not assigned FAIL!');
      end;
  except on e:exception do
    begin
      writeln('BOUND SESSION RAC EXC: '+e.Message);
    end;
  end
end;

procedure TFRE_DB_UserSession.RegisterTaskMethod(const TaskMethod: IFRE_DB_InvokeInstanceMethod; const invocation_interval: integer);
begin
  FDbgTaskMEthod := TaskMethod;
  FDbgTaskIv     := invocation_interval;
  if not assigned(FDbgTimer) then begin
    FDbgTimer      := GFRE_S.AddPeriodicTimer(FDbgTaskIv,@dbg_TaskMethodDisp,nil);
  end;
end;

procedure TFRE_DB_UserSession.RemoveTaskMethod;
begin
  FDbgTaskMethod := nil;
end;

function TFRE_DB_UserSession.IsInteractiveSession: Boolean;
begin
  result := FIsInteractive;
end;

function TFRE_DB_UserSession.FetchTranslateableText(const translation_key: TFRE_DB_String; var textObj: IFRE_DB_TEXT): Boolean;
begin
  result := GetDBConnection.FetchTranslateableText(translation_key,textObj);
end;

function TFRE_DB_UserSession.GetDBConnection: IFRE_DB_CONNECTION;
begin
  result := FDBConnection;
end;


function TFOS_BASE.Implementor: TObject;
begin
  result := self;
end;

function TFOS_BASE.Implementor_HC: TObject;
begin
  result := self;
end;

//class function TFRE_DB_Base.Get_DBI_ClassMethods : TFOSStringArray;
//var methodtable : pmethodnametable;
//    i           : dword;
//    ovmt        : PVmt;
//begin
//   ovmt:=PVmt(self);
//   while assigned(ovmt) do
//     begin
//        methodtable:=pmethodnametable(ovmt^.vMethodTable);
//        if assigned(methodtable) then
//          begin
//             for i:=0 to methodtable^.count-1 do begin
//               //writeln('******* : ',methodtable^.entries[i].name^);
//               if pos('IMC_',methodtable^.entries[i].name^)=1 then begin
//                 SetLength(result,Length(result)+1);
//                 result[High(result)] := Copy(methodtable^.entries[i].name^,5,MaxInt);
//               end;
//             end;
//          end;
//        ovmt := ovmt^.vParent;
//     end;
//end;

function TFRE_DB_Base.Implementor_HC: TObject;
begin
  if assigned(FMediatorExtention) then begin
    result := FMediatorExtention;
  end else begin
    Result:=self;
  end;
end;

function TFRE_DB_Base.Debug_ID: TFRE_DB_String;
begin
  result := 'LOGIC: NO DEBUG ID SET';
end;

function TFRE_DB_Base.CFG_Dont_Finalize_Object: Boolean;
begin
  result := false;
end;

function TFRE_DB_Base.GetSystemSchemeByName(const schemename: TFRE_DB_String; var scheme: IFRE_DB_SchemeObject): Boolean;
begin
  result := GFRE_DBI.GetSystemSchemeByName(schemename,scheme);
end;

function TFRE_DB_Base.GetSystemScheme(const schemename: TClass; var scheme: IFRE_DB_SchemeObject): Boolean;
begin
  result := GFRE_DBI.GetSystemScheme(schemename,scheme);
end;

procedure TFRE_DB_Base.GetSession(const input: IFRE_DB_Object; out session: TFRE_DB_UserSession; const no_error_on_no_session: boolean);
var reference : TObject;
begin
  reference := (input.Implementor as TFRE_DB_Base).GetReference;
  if ((reference=nil) and (no_error_on_no_session=false)) then raise EFRE_DB_Exception.Create(edb_ERROR,'the input parameter carries no session information');
  session := reference as TFRE_DB_UserSession;
end;

function TFRE_DB_Base.GetSession(const input: IFRE_DB_Object): TFRE_DB_UserSession;
begin
  GetSession(input,result,false);
end;

procedure TFRE_DB_Base.__SetMediator(const med: TFRE_DB_ObjectEx);
begin
  FMediatorExtention := med;
end;

//class function TFRE_DB_Base.Get_DBI_ClassMethods: TFOSStringArray;
//begin
//
//end;

class function TFRE_DB_Base.Get_DBI_InstanceMethods: TFRE_DB_StringArray;
var methodtable : pmethodnametable;
    i           : dword;
    ovmt        : PVmt;
begin
   ovmt:=PVmt(self);
   while assigned(ovmt) do
     begin
        methodtable:=pmethodnametable(ovmt^.vMethodTable);
        if assigned(methodtable) then
          begin
             for i:=0 to methodtable^.count-1 do begin
               //writeln('******* : ',methodtable^.entries[i].name^);
               if pos('IMI_',methodtable^.entries[i].name^)=1 then begin
                 SetLength(result,Length(result)+1);
                 result[High(result)] := uppercase(Copy(methodtable^.entries[i].name^,5,MaxInt));
               end;
             end;
          end;
        ovmt := ovmt^.vParent;
     end;
end;

class function TFRE_DB_Base.Get_DBI_ClassMethods: TFRE_DB_StringArray;
var methodtable : pmethodnametable;
    i           : dword;
    ovmt        : PVmt;
begin
   ovmt:=PVmt(self);
   while assigned(ovmt) do
     begin
        methodtable:=pmethodnametable(ovmt^.vMethodTable);
        if assigned(methodtable) then
          begin
             for i:=0 to methodtable^.count-1 do begin
               //writeln('******* : ',methodtable^.entries[i].name^);
               if pos('IMC_',methodtable^.entries[i].name^)=1 then begin
                 SetLength(result,Length(result)+1);
                 result[High(result)] := uppercase(Copy(methodtable^.entries[i].name^,5,MaxInt));
               end;
             end;
          end;
        ovmt := ovmt^.vParent;
     end;
end;

class function TFRE_DB_Base.ClassMethodExists(const name: Shortstring): Boolean;
begin
  result := MethodAddress('IMC_'+name)<> nil;
end;

class function TFRE_DB_Base.Invoke_DBIMC_Method(const name: TFRE_DB_String; const input: IFRE_DB_Object): IFRE_DB_Object;
var M :IFRE_DB_InvokeClassMethod;
    MM:TMethod;
begin
   MM.Code := MethodAddress('IMC_'+name);
   MM.Data := self;
   M := IFRE_DB_InvokeClassMethod(MM);
   if assigned(mm.Code) then begin
     try
       result := m(input);
     except on e:exception do begin
       raise EFRE_DB_Exception.Create(edb_ERROR,'CLASS METHOD INVOCATION %s.%s failed (%s)',[Classname,name,e.Message]);
     end;end;
   end else begin
      raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'CLASS INVOCATION %s.%s failed (method not found)',[Classname,name]);
   end;
end;

//class function TFRE_DB_Base.Invoke_DBIMC_Method(const name: TFRE_DB_String; const input: TFRE_DB_Object): TFRE_DB_Object;
//var M:TFRE_DB_InvokeInstanceMethod;
//    MM:TMethod;
//    cn:TFRE_DB_String;
//begin
//   cn := self.ClassName;
//   Get_DBI_ClassMethods;
//   MM.Code := MethodAddress('IMC_'+name);
//   MM.Data := self;
//   M := TFRE_DB_InvokeInstanceMethod(MM);
//   if assigned(mm.Code) then begin
//     try
//       result := m(input);
//     except on e:exception do begin
//       raise EFRE_DB_Exception.Create(edb_ERROR,'CLASS METHOD INVOCATION %s.%s () failed (%s)',[Classname,name,GFRE_BT.GUID_2_HexString(TFRE_DB_Object(self).UID),e.Message]);
//     end;end;
//   end else begin
//      raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'CLASS METHOD INVOCATION %s.%s () failed (not found)',[Classname,name]);
//   end;
//end;

function TFRE_DB_Base.Invoke_DBIMI_Method(const name: TFRE_DB_String; const input: IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var M  : IFRE_DB_InvokeInstanceMethod;
    MM : TMethod;
    WM : IFRE_DB_WebInstanceMethod;
begin
   result := nil;
   MM.Code := MethodAddress('WEB_'+name);
   MM.Data := self;
   if assigned(MM.code) then
     begin
       WM := IFRE_DB_WebInstanceMethod(MM);
       try
         result := wm(input,ses,app,conn);
       except on e:exception do begin
         raise EFRE_DB_Exception.Create(edb_ERROR,'WEB INSTANCE METHOD INVOCATION %s.%s (%s) failed (%s)',[Classname,name,Debug_ID,e.Message]);
       end;end;
     end
   else
     begin
       MM.Code := MethodAddress('IMI_'+name);
       M := IFRE_DB_InvokeInstanceMethod(MM);
       if assigned(mm.Code) then begin
         try
           result := m(input);
         except on e:exception do begin
           raise EFRE_DB_Exception.Create(edb_ERROR,'INSTANCE METHOD INVOCATION %s.%s (%s) failed (%s)',[Classname,name,Debug_ID,e.Message]);
         end;end;
       end else begin
          raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'INSTANCE METHOD INVOCATION %s.%s (%s) failed (method not found)',[Classname,name,Debug_ID]);
       end;
     end;
end;

function TFRE_DB_Base.Fetch_DBIMI_Method(const name: TFRE_DB_String): IFRE_DB_InvokeInstanceMethod;
var M :IFRE_DB_InvokeInstanceMethod;
    MM:TMethod;
begin
   if Assigned(FMediatorExtention) then begin
     result := FMediatorExtention.Fetch_DBIMI_Method(name);
   end else begin
     MM.Code := MethodAddress('IMI_'+name);
     MM.Data := self;
     M := IFRE_DB_InvokeInstanceMethod(MM);
     if assigned(mm.Code) then begin
       result := m;
     end else begin
        raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'FETCH INSTANCE METHOD %s.%s (%s) failed (not found)',[Classname,name,Debug_ID]);
     end;
   end;
end;

function TFRE_DB_Base.IMI_MethodExists(const name: TFRE_DB_String): boolean;
begin
   if assigned(FMediatorExtention) then begin
     result := FMediatorExtention.MethodExists(name);
   end else begin
     result := assigned(MethodAddress('IMI_'+name));
   end;
end;

function TFRE_DB_Base.MethodExists(const name: Shortstring): Boolean;
begin
  result := IMI_MethodExists(name);
end;

procedure TFRE_DB_Base.SetReference(const obj: TObject);
begin
  //if assigned(obj) then begin
  //  if assigned(TAGRef) then raise EFRE_DB_Exception.Create(edb_INTERNAL,'TAGREF DOUBLE FEATURE');
  //end;
  TAGRef := obj;
end;

function TFRE_DB_Base.GetReference: TObject;
begin
  result := TAGRef;
end;

function TFRE_DB_Base.Supports(const InterfaceSpec: ShortString; out Intf): boolean;
begin
  result := Sysutils.Supports(self,InterfaceSpec,Intf);
  if (not result) and assigned(FMediatorExtention) then begin
    result := Sysutils.Supports(FMediatorExtention,InterfaceSpec,Intf);
  end;
end;

function TFRE_DB_Base.Supports(const InterfaceSpec: ShortString): boolean;
begin
  result := Sysutils.Supports(self,InterfaceSpec);
  if (not result) and assigned(FMediatorExtention) then begin
    result := Sysutils.Supports(FMediatorExtention,InterfaceSpec);
  end;
end;

procedure TFRE_DB_Base.IntfCast(const InterfaceSpec: ShortString; out Intf);
begin
  if not self.Supports(InterfaceSpec,Intf) then begin
    raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'INTERFACE UPCAST FAILED FOR CLASS [%s] TO [%s]',[ClassName,InterfaceSpec]);
  end;
end;

function TFRE_DB_Base.CSFT(const server_function_name: string; const obj: IFRE_DB_Object): TFRE_DB_SERVER_FUNC_DESC;
var sfo:IFRE_DB_Object;
begin
  sfo := obj;
  if sfo=nil then begin
    self.IntfCast(IFRE_DB_Object,sfo);
  end;
  if not sfo.MethodExists(server_function_name) then raise EFRE_DB_Exception.Create(edb_ERROR,'no method named %s exists in object %s',[server_function_name,sfo.Implementor_HC.ClassName]);
  result := TFRE_DB_SERVER_FUNC_DESC.create.Describe(sfo,server_function_name);
end;

function TFRE_DB_Base.CSF(const invoke_method: IFRE_DB_InvokeInstanceMethod): TFRE_DB_SERVER_FUNC_DESC;
var m           : TMethod;
    obj         : TFRE_DB_Base;
    sfo         : IFRE_DB_Object;
    method_name : string;
begin
  m := TMethod(invoke_method);
  obj := (TObject(m.Data) as TFRE_DB_Base);
  obj.IntfCast(IFRE_DB_Object,sfo);
  method_name := obj.MethodName(m.Code);
  if pos('IMI_',method_name)=1 then begin
    result := TFRE_DB_SERVER_FUNC_DESC.create.Describe(sfo,Copy(method_name,5,maxint));
  end else begin
    raise EFRE_DB_Exception.Create(edb_ERROR,'the method named %s does not follow the IMI_* naming convention',[method_name]);
  end;
end;

function TFRE_DB_Base.CWSF(const invoke_method: IFRE_DB_WebInstanceMethod): TFRE_DB_SERVER_FUNC_DESC;
var m           : TMethod;
    obj         : TFRE_DB_Base;
    sfo         : IFRE_DB_Object;
    method_name : string;
begin
  m := TMethod(invoke_method);
  obj := (TObject(m.Data) as TFRE_DB_Base);
  obj.IntfCast(IFRE_DB_Object,sfo);
  method_name := obj.MethodName(m.Code);
  if pos('WEB_',method_name)=1 then begin
    result := TFRE_DB_SERVER_FUNC_DESC.create.Describe(sfo,Copy(method_name,5,maxint));
  end else begin
    raise EFRE_DB_Exception.Create(edb_ERROR,'the method named %s does not follow the WEB_* naming convention',[method_name]);
  end;
end;

function TFRE_DB_Base.CSCF(const serv_classname, server_function_name: string; const param1: string; const value1: string): TFRE_DB_SERVER_FUNC_DESC;
begin
  result := TFRE_DB_SERVER_FUNC_DESC.create.Describe(serv_classname,server_function_name);
  if param1<>'' then
    result.AddParam.Describe(param1,value1);
end;



function TFRE_DB_ObjectEx.Debug_ID: TFRE_DB_String;
begin
  Result := UID_String;
end;

procedure TFRE_DB_ObjectEx.InternalSetup;
begin

end;

procedure TFRE_DB_ObjectEx.InternalFinalize;
begin

end;

class procedure TFRE_DB_ObjectEx.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.SetParentSchemeByName('TFRE_DB_NAMED_OBJECT');
  Scheme.AddSchemeField('objname',fdbft_String);
  Scheme.AddSchemeFieldSubscheme('desc','TFRE_DB_TEXT');
  //Scheme.SetSysDisplayField(GFRE_DB.ConstructStringArray(['objname','$DBTEXT:desc']),'%s - (%s)');
  scheme.Strict(false);
end;

function TFRE_DB_ObjectEx.Invoke(const method: TFRE_DB_String; const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  result := Invoke_DBIMI_Method(method,input,ses,app,conn);
end;

//procedure TFRE_DB_ObjectEx.MyInitialize(const session : TFRE_DB_UserSession);
//begin
//
//end;


class procedure TFRE_DB_Base.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
end;

class procedure TFRE_DB_Base.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
end;

constructor TFRE_DB_ObjectEx.create;
begin
  FBound       := false;
  if not GFRE_DBI.NewObjectIntf(IFRE_DB_NAMED_OBJECT,FNamedObject,self) then abort;
  FNamedObject.Supports(IFRE_DB_OBJECT,FImplementor);
  InternalSetup;
end;

constructor TFRE_DB_ObjectEx.CreateConnected(const conn: IFRE_DB_CONNECTION);
begin
  create;
  Conn.AssociateObject(self);
  InternalSetup;
end;


constructor TFRE_DB_ObjectEx.CreateBound(const dbo: IFRE_DB_Object);
begin
  FBound       := true;
  FImplementor := dbo;
  FImplementor.Supports(IFRE_DB_NAMED_OBJECT,FNamedObject);
  InternalSetup;
end;

destructor TFRE_DB_ObjectEx.Destroy;
begin
  //writeln('--------------------------------------- MEDIATOR DESTROOOYY---');
  inherited Destroy;
end;


procedure TFRE_DB_ObjectEx.ForAllFields(const iter: IFRE_DB_FieldIterator);
begin
  FImplementor.ForAllFields(iter);
end;

procedure TFRE_DB_ObjectEx.ForAllFieldsBreak(const iter: IFRE_DB_FieldIteratorBrk);
begin
  FImplementor.ForAllFieldsBreak(iter);
end;

function TFRE_DB_ObjectEx.UID: TGUID;
begin
  result := FImplementor.UID;
end;

function TFRE_DB_ObjectEx.UID_String: TFRE_DB_String;
begin
  result := FImplementor.UID_String;
end;


function TFRE_DB_ObjectEx.Parent: IFRE_DB_Object;
begin
  result := FImplementor.Parent;
end;

function TFRE_DB_ObjectEx.ParentField: IFRE_DB_FIELD;
begin
  result := FImplementor.ParentField;
end;

function TFRE_DB_ObjectEx.AsString(const without_schemes: boolean): TFRE_DB_String;
begin
  result := FImplementor.AsString(without_schemes);
end;

function TFRE_DB_ObjectEx.Field(const name: TFRE_DB_String): IFRE_DB_FIELD;
begin
  result := FImplementor.Field(name);
end;

function TFRE_DB_ObjectEx.FieldOnlyExistingObj(const name: TFRE_DB_String): IFRE_DB_Object;
begin
  result := FImplementor.FieldOnlyExistingObj(name);
end;

function TFRE_DB_ObjectEx.FieldPath(const name: TFRE_DB_String; const dont_raise_ex: boolean): IFRE_DB_FIELD;
begin
  result := FImplementor.FieldPath(name,dont_raise_ex);
end;

function TFRE_DB_ObjectEx.FieldPathExists(const name: TFRE_DB_String): Boolean;
begin
  result := FImplementor.FieldPathExists(name);
end;

function TFRE_DB_ObjectEx.FieldPathListFormat(const field_list: TFRE_DB_StringArray; const formats: TFRE_DB_String; const empty_val: TFRE_DB_String): TFRE_DB_String;
begin
  result := FImplementor.FieldPathListFormat(field_list,formats,empty_val);
end;

function TFRE_DB_ObjectEx.FieldCount(const without_calcfields: boolean): SizeInt;
begin
  result := FImplementor.FieldCount(without_calcfields);
end;

function TFRE_DB_ObjectEx.DeleteField(const name: TFRE_DB_String): Boolean;
begin
  result := FImplementor.DeleteField(name);
end;

procedure TFRE_DB_ObjectEx.ClearAllFields;
begin
  FImplementor.ClearAllFields;
end;

function TFRE_DB_ObjectEx.FieldExists(const name: TFRE_DB_String): boolean;
begin
  result := FImplementor.FieldExists(name);
end;

procedure TFRE_DB_ObjectEx.StripOwnedObjects;
begin
  FImplementor.StripOwnedObjects;
end;

procedure TFRE_DB_ObjectEx.DumpToStrings(const strings: TStrings; indent: integer);
begin
  FImplementor.DumpToStrings(strings,indent);
end;

function TFRE_DB_ObjectEx.DumpToString(indent: integer; const dump_length_max: Integer): TFRE_DB_String;
begin
  result := FImplementor.DumpToString(indent,dump_length_max);
end;

function TFRE_DB_ObjectEx.GetFormattedDisplay: TFRE_DB_String;
begin
  result := FImplementor.GetFormattedDisplay;
end;

function TFRE_DB_ObjectEx.FormattedDisplayAvailable: boolean;
begin
  result := FImplementor.FormattedDisplayAvailable;
end;

function TFRE_DB_ObjectEx.SubFormattedDisplayAvailable: boolean;
begin
  result := FImplementor.SubFormattedDisplayAvailable;
end;

function TFRE_DB_ObjectEx.GetSubFormattedDisplay(indent: integer): TFRE_DB_String;
begin
  result := FImplementor.GetSubFormattedDisplay(indent);
end;

function TFRE_DB_ObjectEx.SchemeClass: TFRE_DB_NameType;
begin
//  result := FImplementor.SchemeClass;
  result := Classname;
end;

function TFRE_DB_ObjectEx.IsA(const schemename: TFRE_DB_NameType): Boolean;
begin
  result := FImplementor.IsA(schemename);
end;

function TFRE_DB_ObjectEx.IsObjectRoot: Boolean;
begin
  result := FImplementor.IsObjectRoot;
end;

procedure TFRE_DB_ObjectEx.SaveToFile(const filename: TFRE_DB_String; const without_schemes: boolean);
begin
  FImplementor.SaveToFile(filename,without_schemes);
end;

function TFRE_DB_ObjectEx.ReferencesObjects: Boolean;
begin
  result := FImplementor.ReferencesObjects;
end;

function TFRE_DB_ObjectEx.ReferencesObjectsFromData: Boolean;
begin
  result := FImplementor.ReferencesObjectsFromData;
end;

function TFRE_DB_ObjectEx.ReferenceList: TFRE_DB_GUIDArray;
begin
  result := FImplementor.ReferenceList;
end;

function TFRE_DB_ObjectEx.ReferenceListFromData: TFRE_DB_CountedGuidArray;
begin
  result := FImplementor.ReferenceListFromData;
end;

function TFRE_DB_ObjectEx.ReferenceListFromDataNocount: TFRE_DB_GuidArray;
begin
  result := FImplementor.ReferenceListFromDataNocount;
end;

function TFRE_DB_ObjectEx.ReferencesDetailed: TFRE_DB_String;
begin
  result := FImplementor.ReferencesDetailed;
end;

function TFRE_DB_ObjectEx.IsReferenced: Boolean;
begin
  result := FImplementor.IsReferenced;
end;

function TFRE_DB_ObjectEx.ReferencedByList: TFRE_DB_GUIDArray;
begin
  result := FImplementor.ReferencedByList;
end;

function TFRE_DB_ObjectEx.ReferencedByList(const from_scheme: TFRE_DB_String): TFRE_DB_GUIDArray;
begin
  result := FImplementor.ReferencedByList(from_scheme);
end;

function TFRE_DB_ObjectEx.ReferencedByList(const scheme: TFRE_DB_StringArray): TFRE_DB_GUIDArray;
begin
  result := FImplementor.ReferencedByList(scheme);
end;

function TFRE_DB_ObjectEx.GetFieldListFilter(const field_type: TFRE_DB_FIELDTYPE): TFRE_DB_StringArray;
begin
  result := FImplementor.GetFieldListFilter(field_type);
end;

function TFRE_DB_ObjectEx.GetUIDPath: TFRE_DB_StringArray;
begin
  result := FImplementor.GetUIDPath;
end;

function TFRE_DB_ObjectEx.GetUIDPathUA: TFRE_DB_GUIDArray;
begin
  result := FImplementor.GetUIDPathUA;
end;

function TFRE_DB_ObjectEx.Implementor: TObject;
begin
  result := FImplementor.Implementor; // THE DBO - not the own mediator class.
end;

function TFRE_DB_ObjectEx.Implementor_HC: TObject;
begin
  result := self;
end;

function TFRE_DB_ObjectEx.Supports(const InterfaceSpec: ShortString; out Intf): boolean;
begin
  result := FImplementor.Supports(InterfaceSpec,intf);
end;

function TFRE_DB_ObjectEx.Supports(const InterfaceSpec: ShortString): boolean;
begin
  result := FImplementor.Supports(InterfaceSpec);
end;

procedure TFRE_DB_ObjectEx.IntfCast(const InterfaceSpec: ShortString; out Intf);
begin
  FImplementor.IntfCast(InterfaceSpec,intf);
end;

function TFRE_DB_ObjectEx.IntfCast(const InterfaceSpec: ShortString): Pointer;
begin
  IntfCast(InterfaceSpec,result);
end;

function TFRE_DB_ObjectEx.GetDBConnection: IFRE_DB_CONNECTION;
begin
  result := FImplementor.GetDBConnection;
end;

function TFRE_DB_ObjectEx.GetScheme: IFRE_DB_SchemeObject;
begin
  result := FImplementor.GetScheme;
end;

procedure TFRE_DB_ObjectEx.Finalize;
begin
  writeln('FINALIZE MEDIATOR CALL  ',ClassName);
  FImplementor.Finalize;
end;

function TFRE_DB_ObjectEx.GetAsJSON(const without_uid: boolean;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil): TJSONData;
begin
  result := FImplementor.GetAsJSON(without_uid,full_dump,stream_cb);
end;

function TFRE_DB_ObjectEx.GetAsJSONString(const without_uid: boolean;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil): TFRE_DB_String;
begin
  result := FImplementor.GetAsJSONString(without_uid,full_dump,stream_cb);
end;

function TFRE_DB_ObjectEx.CloneToNewObject(const generate_new_uids:boolean): IFRE_DB_Object;
begin
  result := FImplementor.CloneToNewObject;
end;

function TFRE_DB_ObjectEx.Mediator: TFRE_DB_ObjectEx;
begin
  result := FImplementor.Mediator;
end;

function TFRE_DB_ObjectEx.NeededSize: TFRE_DB_SIZE_TYPE;
begin
  result := FImplementor.NeededSize;
end;

procedure TFRE_DB_ObjectEx.Set_ReadOnly;
begin
  FImplementor.Set_ReadOnly;
end;

procedure TFRE_DB_ObjectEx.CopyField(const obj: IFRE_DB_Object; const field_name: String);
begin
  FImplementor.CopyField(obj,field_name);
end;


class function TFRE_DB_ObjectEx.NewOperation(const input: IFRE_DB_Object): TGUID;
var dbo              : IFRE_DB_Object;
    data             : IFRE_DB_Object;
    lSchemeclass     : TFRE_DB_String;
    lSchemeObject    : IFRE_DB_SchemeObject;
    lTransformScheme : TFRE_DB_String;
    lCollectionName  : TFRE_DB_String;
    res              : TFRE_DB_Errortype;
    dbo_uid          : TGUID;
    dbc              : IFRE_DB_CONNECTION;
begin
  data             := input.Field('DATA').asobject;
  lCollectionName  := input.field('COLLECTION').AsString;
  lSchemeclass     := ClassName;
  if lSchemeclass    ='' then raise EFRE_DB_Exception.Create(edb_ERROR,'the new operation requires a schemeclass!');
  if lCollectionName ='' then raise EFRE_DB_Exception.Create(edb_ERROR,'the new operation requires a collection!');
  dbc := input.GetReference as IFRE_DB_CONNECTION;
  if not dbc.CollectionExists(lCollectionName) then begin
    raise EFRE_DB_Exception.Create(edb_ERROR,'the collection [%s] is unknown and the implicit creation of collection is not allowed on new!',[lCollectionName]);
  end;
  if not GFRE_DBI.GetSystemSchemeByName(lSchemeclass,lSchemeObject) then raise EFRE_DB_Exception.Create(edb_ERROR,'the scheme [%s] is unknown!',[lSchemeclass]);
  dbo              := dbc.NewObject(lSchemeclass);
  lSchemeObject.SetObjectFieldsWithScheme(data,dbo,true,dbc);
  dbo_uid       := dbo.UID;
  if lCollectionName='' then begin
    raise EFRE_DB_Exception.Create(edb_ERROR,'standard new operation requires a collection!');
  end else begin
    CheckDbResult(dbc.Collection(lCollectionName).Store(dbo),'failure on store/new collection='+lCollectionName);
  end;
  result := dbo_uid;
end;


function TFRE_DB_ObjectEx.IMI_SaveOperation(const input: IFRE_DB_Object): IFRE_DB_Object;
var scheme            : IFRE_DB_SCHEMEOBJECT;
    update_object_uid : TGUid;
    raw_object        : IFRE_DB_Object;
    dbc               : IFRE_DB_CONNECTION;
begin
  if Not IsObjectRoot then begin
    result := TFRE_DB_MESSAGE_DESC(result).Describe('SAVE','Error on saving! Saving of Subobject not supported!',fdbmt_error);
    exit;
  end;
  dbc := GetDBConnection;
  //writeln('::::::::::::: UPDATEING');
  //writeln(DumpToString);
  //writeln(':::::::::::::');
  result            := nil;
  scheme            := GetScheme;
  update_object_uid := UID;
  raw_object        := input.Field('data').AsObject;

  scheme.SetObjectFieldsWithScheme(raw_object,self,false,GetDBConnection);
  //writeln(':::::::::::::');
  //writeln('UPDATED');
  //writeln(':::::::::::::');
  //writeln(obj.DumpToString);
  //writeln(':::::::::::::');
  CheckDbResult(dbc.Update(self),'failure on update');  // This instance is freed by now, so rely on the stackframe only (self) pointer is garbage(!!)
  result := GFRE_DB_NIL_DESC;
end;

function TFRE_DB_ObjectEx.IMI_DeleteOperation(const input: IFRE_DB_Object): IFRE_DB_Object;
var db_res       : TFRE_DB_Errortype;
begin
    case input.Field('confirmed').AsString of
      'true': begin
                if not IsObjectRoot then begin
                  result := TFRE_DB_MESSAGE_DESC.Create.Describe('DELETE','Error on deleting! Deleting of Subobject not supported!',fdbmt_error);
                end;
                db_res := GetDBConnection.Delete(UID);
                if db_res=edb_OK then begin
                  result := GFRE_DB_NIL_DESC;
                end else begin
                  raise EFRE_DB_Exception.Create(db_res,'delete of [%s] failed!',[UID_String]);
                end;
              end;
      'false': begin
                 result:=GFRE_DB_NIL_DESC;
               end;
      '': begin
            result := TFRE_DB_MESSAGE_DESC.create.Describe('Delete','Are you sure?',fdbmt_confirm,CSF(@self.IMI_DeleteOperation));
          end;
    end;
end;

class function TFRE_DB_ObjectEx.IMC_NewOperation(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  //writeln(input.DumpToString());
  try
    NewOperation(input);
    result  := TFRE_DB_CLOSE_DIALOG_DESC.Create.Describe();
  except
   on E:Exception do begin
    result := TFRE_DB_MESSAGE_DESC.Create.Describe('NEW','Error on creating object ['+e.Message+']',fdbmt_error);
   end;
  end;
end;

function TFRE_DB_ObjectEx.IMI_NoteLoad(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  conn   : IFRE_DB_CONNECTION;
  noteobj: IFRE_DB_Object;
begin
  conn := GetSession(input).GetDBConnection;
  if input.FieldExists('linkid') then begin
    if conn.Collection('note').GetIndexedObj(input.Field('linkid').asstring,noteobj) then begin
      exit(TFRE_DB_EDITOR_DATA_DESC.create.Describe(noteobj.Field('note').asstring));
    end else begin
      exit(TFRE_DB_EDITOR_DATA_DESC.create.Describe('New'));
    end;
  end else begin
    exit(TFRE_DB_EDITOR_DATA_DESC.create.Describe(''));
  end;
end;

function TFRE_DB_ObjectEx.IMI_NoteSave(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  conn   : IFRE_DB_CONNECTION;
  noteobj: IFRE_DB_Object;
  res    : TFRE_DB_Errortype;
begin
  conn := GetSession(input).GetDBConnection;
  if input.FieldExists('linkid') then begin
    if conn.Collection('note').GetIndexedObj(input.Field('linkid').asstring,noteobj) then begin
      noteobj.Field('note').asstring := input.Field('content').asstring;
      res := conn.Update(noteobj);
      if res<>edb_OK then
        raise EFRE_DB_Exception.Create(res,'error updating note');
    end else begin
      noteobj := conn.NewObject(TFRE_DB_NOTE.ClassName);
      noteobj.Field('link').asstring:=input.Field('linkid').asstring;
      noteobj.Field('note').asstring  := input.Field('content').asstring;
      res := conn.Collection('note').Store(noteobj);
      if res<>edb_OK then
        raise EFRE_DB_Exception.Create(res,'error storing note');
    end;
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_ObjectEx.IMI_NoteStartEdit(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_ObjectEx.IMI_NoteStopEdit(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;


{ EFRE_DB_Exception }

constructor EFRE_DB_Exception.Create(const msg: TFRE_DB_String);
begin
  Create(edb_ERROR,msg);
end;

constructor EFRE_DB_Exception.Create(const et: TFRE_DB_Errortype; msg: TFRE_DB_String);
begin
  ErrorType := et;
  if (et<low(TFRE_DB_Errortype)) or (et>High(TFRE_DB_Errortype)) then begin
    GFRE_BT.CriticalAbort('internal fault - EDB Exception Range Check');
  end;
  case et of
    edb_INTERNAL: msg := 'DB INTERNAL FAULT : '+msg+' ';
    else        msg := '('+CFRE_DB_Errortype[et]+') '+msg;
  end;
  if assigned(GFRE_DBI) then
    GFRE_DBI.LogError(dblc_EXCEPTION,msg+LineEnding+GFRE_BT.DumpExceptionsBacktrace);
  inherited create(msg);
end;

constructor EFRE_DB_Exception.Create(const et: TFRE_DB_Errortype; msg: TFRE_DB_String; params: array of const);
begin
  Create(et,Format(msg,params));
end;


function TFRE_DB_CONTENT_DESC.GetContentId: TFRE_DB_String;
begin
  Result:=Field('id').AsString;
end;

function TFRE_DB_CONTENT_DESC.GetUpdateId: TFRE_DB_String;
begin
  Result:=Field('updateId').AsString;
end;

function TFRE_DB_CONTENT_DESC.GetWindowCaption: TFRE_DB_String;
begin
  Result:=Field('windowCaption').AsString;
end;

procedure TFRE_DB_CONTENT_DESC.SetContentId(AValue: TFRE_DB_String);
begin
  Field('id').AsString:=AValue;
  Field('destroyNotify').AsBoolean:=true;
end;

procedure TFRE_DB_CONTENT_DESC.SetUpdateId(AValue: TFRE_DB_String);
begin
  Field('updateId').AsString:=AValue;
end;

procedure TFRE_DB_CONTENT_DESC.SetWindowCaption(AValue: TFRE_DB_String);
begin
  Field('windowCaption').AsString:=AValue;
end;

function TFRE_DB_PARAM_DESC.Describe(const key, value: string): TFRE_DB_PARAM_DESC;
begin
  Field('key').AsString:=key;
  Field('value').AsString:=value;
  Field('asArray').AsBoolean:=false;
  Result:=Self;
end;

function TFRE_DB_PARAM_DESC.Describe(const key: String; const value: TFRE_DB_StringArray): TFRE_DB_PARAM_DESC;
begin
  Field('key').AsString:=key;
  Field('value').AsStringArr:=value;
  Field('asArray').AsBoolean:=true;
  Result:=Self;
end;

{ TFRE_DB_SERVER_FUNC_DESC }

function TFRE_DB_SERVER_FUNC_DESC.Describe(const obj: IFRE_DB_Object; const func: String): TFRE_DB_SERVER_FUNC_DESC;
begin
  Result := Describe(obj.SchemeClass,obj.GetUIDPath,func);
end;

function TFRE_DB_SERVER_FUNC_DESC.Describe(const oschemeclass: String; const ouid: TGUID; const func: String): TFRE_DB_SERVER_FUNC_DESC;
begin
  result := Describe(oschemeclass,TFRE_DB_StringArray.Create(GFRE_BT.GUID_2_HexString(ouid)),func);
end;

function TFRE_DB_SERVER_FUNC_DESC.Describe(const oschemeclass: String; const uidpath: TFRE_DB_StringArray; const func: String): TFRE_DB_SERVER_FUNC_DESC;
var
  path : String;
begin
  Field('class').AsString:=oschemeclass;
  Field('func').AsString:=func;
  Field('uidPath').AsStringArr:=uidpath;
  Result:=Self;
end;

function TFRE_DB_SERVER_FUNC_DESC.Describe(const oschemeclass: String; const func: String): TFRE_DB_SERVER_FUNC_DESC;
begin
  Field('class').AsString := oschemeclass;
  Field('func').AsString := func;
  Result:=Self;
end;

function TFRE_DB_SERVER_FUNC_DESC.InternalInvoke(const session: TFRE_DB_UserSession;const input:IFRE_DB_Object): IFRE_DB_Object;
var
  i        : Integer;
  key,value: String;
  newInput : IFRE_DB_Object;
begin
  newInput:=GFRE_DBI.NewObject;
  for i := 0 to Field('params').ValueCount - 1 do begin
    key:=FieldPath('params.key').AsString;
    value:=FieldPath('params.value').AsString;
    newInput.Field('data').AsObject.Field(key).AsString:=value;
  end;
  result:=session.InternalSessInvokeMethod(Field('class').AsString,Field('func').AsString,GFRE_DBI.StringArray2GuidArray(Field('uidPath').AsStringArr),newInput);
end;


function TFRE_DB_SERVER_FUNC_DESC.AddParam: TFRE_DB_PARAM_DESC;
begin
  Result:=TFRE_DB_PARAM_DESC.Create;
  Field('params').AddObject(Result);
end;


procedure TFRE_DB_APPLICATION.SetDescTranslationKey(const AValue: TFRE_DB_String);
begin
  Field('desc_tkey').AsString:=AValue;
end;

function TFRE_DB_APPLICATION.GetDescTranslationKey: TFRE_DB_String;
begin
  result := Field('desc_tkey').AsString;
end;

function TFRE_DB_APPLICATION.GetName: TFRE_DB_String;
begin
  result :=  FNamedObject.GetName;
end;

procedure TFRE_DB_APPLICATION.SetName(const AValue: TFRE_DB_String);
begin
  FNamedObject.SetName(AValue);
end;

procedure TFRE_DB_APPLICATION.ServerInitialize(const admin_dbc: IFRE_DB_CONNECTION);

procedure _initSubModules(const field: IFRE_DB_FIELD);
  var app_module : TFRE_DB_APPLICATION_MODULE;
      io         : IFRE_DB_Object;
  begin
    if (field.FieldType=fdbft_Object) and  field.AsObject.Supports(IFRE_DB_APPLICATION_MODULE) then begin
      (field.AsObject.Implementor_HC as TFRE_DB_APPLICATION_MODULE).MyServerInitializeModule(admin_dbc);
    end;
  end;

begin
  MyServerInitialize(admin_dbc);
  ForAllFields(@_initSubModules);
end;

procedure TFRE_DB_APPLICATION.ServerFinalize(const admin_dbc: IFRE_DB_CONNECTION);
begin
  MyServerFinalize;
end;

function TFRE_DB_APPLICATION.InstallAppDefaults(const conn: IFRE_DB_SYS_CONNECTION): TFRE_DB_Errortype;
begin
  // Use Cases
  // Called by: Meta_initialization for extensions
  // Install new Application: Create Appdata Object, Create all roles, Create default groups
  // Update Application: Check Version of Appdata Object and create / delete roles and default groups
end;

function TFRE_DB_APPLICATION.InstallSystemGroupsandRoles(const conn: IFRE_DB_SYS_CONNECTION; const domain: TFRE_DB_NameType): TFRE_DB_Errortype;
begin
  // Use Cases
  // Install all System Groups and Roles for a specific Domain
end;

function TFRE_DB_APPLICATION.RemoveApp(const conn: IFRE_DB_SYS_CONNECTION): TFRE_DB_Errortype;
begin
  // Use Cases
  // Called by: Meta_remove for extensions: Remove all default roles, groups and the appdata object
  conn.RemoveApp(ObjectName);
end;


//class constructor TFRE_DB_APPLICATION.InitializeStatics;
//begin
//  InitalizeClassStatics;
//end;

//procedure TFRE_DB_APPLICATION.SchemeClassInitialize;
//begin
//
//end;

procedure TFRE_DB_APPLICATION.InternalSetup;
begin
  inherited InternalSetup;
  SetupApplicationStructure;
end;


procedure TFRE_DB_APPLICATION.InitAppDesc(const objname, descr_translation_key: TFRE_DB_String);
begin
  ObjectName           := objname;
  SetDescTranslationKey(descr_translation_key);
end;



procedure TFRE_DB_APPLICATION.SessionInitialize(const session: TFRE_DB_UserSession);

  procedure _initSubModules(const field: IFRE_DB_FIELD);
  var app_module : TFRE_DB_APPLICATION_MODULE;
      io         : IFRE_DB_Object;
  begin
    if (field.FieldType=fdbft_Object) and  field.AsObject.Supports(IFRE_DB_APPLICATION_MODULE) then begin
      (field.AsObject.Implementor_HC as TFRE_DB_APPLICATION_MODULE).MySessionInitializeModule(session);
    end;
  end;

begin
  MySessionInitialize(session);
  ForAllFields(@_initSubModules);
end;

procedure TFRE_DB_APPLICATION.SessionFinalize(const session: TFRE_DB_UserSession);

  procedure _FinishSubModules(const field: IFRE_DB_FIELD);
  var app_module : TFRE_DB_APPLICATION_MODULE;
      io         : IFRE_DB_Object;
  begin
    if (field.FieldType=fdbft_Object) and  field.AsObject.Supports(IFRE_DB_APPLICATION_MODULE) then begin
      (field.AsObject.Implementor_HC as TFRE_DB_APPLICATION_MODULE).MySessionFinalizeModule(session);
    end;
  end;

begin
  ForAllFields(@_FinishSubmodules);
  MySessionFinalize(session);
end;

procedure TFRE_DB_APPLICATION.SessionPromotion(const session: TFRE_DB_UserSession);

  procedure _initSubModules(const field: IFRE_DB_FIELD);
  var app_module : TFRE_DB_APPLICATION_MODULE;
      io         : IFRE_DB_Object;
  begin
    if (field.FieldType=fdbft_Object) and  field.AsObject.Supports(IFRE_DB_APPLICATION_MODULE) then begin
      (field.AsObject.Implementor_HC as TFRE_DB_APPLICATION_MODULE).MySessionPromotionModule(session);
    end;
  end;

begin
  MySessionPromotion(session);
  ForAllFields(@_initSubModules);
end;

function TFRE_DB_APPLICATION.IsContentUpdateVisible(const update_content_id: string): Boolean;
begin
  result := true;
end;

function TFRE_DB_APPLICATION.CFG_ApplicationUsesRights: boolean;
begin
  result := false;
end;




procedure TFRE_DB_APPLICATION.SetupApplicationStructure;
begin
  ObjectName:=ClassName; // Fallback
end;


procedure TFRE_DB_APPLICATION.RemoveAppRole(const sys_connection: IFRE_DB_SYS_CONNECTION; const sub_group_name: TFRE_DB_String);
var name : TFRE_DB_String;
begin
  name := Get_Rightname_App_Role_Subrole(ObjectName,sub_group_name);
  if sys_connection.RoleExists(name) then begin
    CheckDBResult(sys_connection.DeleteRole(name),'cannot delete app right group : '+name);
  end;
end;

function TFRE_DB_APPLICATION._CheckVersion(const sys_connection: IFRE_DB_SYS_CONNECTION; out old_version: TFRE_DB_String): TFRE_DB_APP_INSTALLSTATE;
var appdata : IFRE_DB_APPDATA;
begin
  if _FetchAppdata(sys_connection,appdata) then begin
    old_version    := appdata.GetVersion;
    if old_version  = _ActualVersion then begin
      result       := SameVersion;
    end else begin
      result       := OtherVersion;
    end;
  end else begin
    old_version    := '';
    result         := NotInstalled;
  end;
end;

function TFRE_DB_APPLICATION._ActualVersion: TFRE_DB_String;
begin
  writeln('BASE APPLICATION HAS NO VERSION');
  abort;
end;

procedure TFRE_DB_APPLICATION._SetAppdataVersion(const sys_connection: IFRE_DB_SYS_CONNECTION; const version: TFRE_DB_String);
var appdata     : IFRE_DB_APPDATA;
begin
  if _FetchAppdata(sys_connection,appdata) then begin
    appdata.Version := version;
    sys_connection.UpdateAppData(appdata);
  end else begin
    _CreateAndStoreAppdata(sys_connection,version);
  end;
end;

procedure TFRE_DB_APPLICATION._CreateAndStoreAppdata(const sys_connection: IFRE_DB_SYS_CONNECTION; const version: TFRE_DB_String);
var appdata: IFRE_DB_APPDATA;
    res    : TFRE_DB_Errortype;
begin
  writeln('creating app data '+ObjectName+' version:'+version);
  sys_connection.NewAppData(ObjectName,'','',appdata);
  appdata.Version := version;
  res             := sys_connection.StoreAppData(appdata);
  if res<>edb_OK then begin
    raise EFRE_DB_Exception.Create(res,'Error storing appdata '+ObjectName);
  end;
end;

function TFRE_DB_APPLICATION._FetchAppdata(const sys_connection: IFRE_DB_SYS_CONNECTION; var appdata: IFRE_DB_APPDATA): boolean;
var res : TFRE_DB_Errortype;
begin
  if (sys_connection.FetchAppData(ObjectName,appdata)=edb_OK) then begin
    result := true;
  end else begin
    result := false;
  end;
end;

function TFRE_DB_APPLICATION._CreateAppRole(const rolename: TFRE_DB_String; const short_desc, long_desc: TFRE_DB_String): IFRE_DB_ROLE;
var
  name : TFRE_DB_String;
  right: IFRE_DB_RIGHT;
begin
  name   := Get_Rightname_App_Role_Subrole(ObjectName,rolename);
  result := GFRE_DBI.NewRole(name,long_desc,short_desc);
  right:=GFRE_DBI.NewRight(Get_Rightname('START'),'Startup of App','Start App');
  result.AddRight(right);
end;

procedure TFRE_DB_APPLICATION._AddAppRight(const right_group: IFRE_DB_ROLE; const sub_right_name, short_desc, long_desc: TFRE_DB_String);
var name  : TFRE_DB_String;
    right : IFRE_DB_RIGHT;
begin
  name  := Get_Rightname(sub_right_name);
  right := GFRE_DBI.NewRight(name,long_desc,short_desc);
  right_group.AddRight(right);
end;

procedure TFRE_DB_APPLICATION._AddAppRightModules(const right_group: IFRE_DB_ROLE; const module_names: TFRE_DB_StringArray);
var name  : TFRE_DB_String;
    right : IFRE_DB_RIGHT;
    i     : Integer;
begin
  for i:=0 to high(module_names) do begin
    name  := Get_Rightname('$ACMOD_'+module_names[i]);
    right := GFRE_DBI.NewRight(name,'Automatic Right to access App Module: '+module_names[i],'ACCESS '+module_names[i]);
    right_group.AddRight(right);
  end;
end;

procedure TFRE_DB_APPLICATION._AddSystemGroups(const sys_connection: IFRE_DB_SYS_CONNECTION; const domain: TFRE_DB_NameType);
begin
  sys_connection.AddAppGroup   (ObjectName,'USER'+'@'+domain,ObjectName+' UG',ObjectName+' User');
  sys_connection.AddAppGroup   (ObjectName,'ADMIN'+'@'+domain,ObjectName+' AG',ObjectName+' Admin');
  sys_connection.AddAppGroup   (ObjectName,'GUEST'+'@'+domain,ObjectName+' GG',ObjectName+' Guest');
  //FIXXME - remove roles assignement
  sys_connection.ModifyGroupRoles(Get_Groupname_App_Group_Subgroup(ObjectName,'USER'+'@'+domain),GFRE_DBI.ConstructStringArray([Get_Rightname_App_Role_SubRole(ObjectName,'USER'+'@'+domain)]));
  sys_connection.ModifyGroupRoles(Get_Groupname_App_Group_Subgroup(ObjectName,'GUEST'+'@'+domain),GFRE_DBI.ConstructStringArray([Get_Rightname_App_Role_SubRole(ObjectName,'GUEST'+'@'+domain)]));
  sys_connection.ModifyGroupRoles(Get_Groupname_App_Group_Subgroup(ObjectName,'ADMIN'+'@'+domain),GFRE_DBI.ConstructStringArray([Get_Rightname_App_Role_SubRole(ObjectName,'ADMIN'+'@'+domain),Get_Rightname_App_Role_SubRole(ObjectName,'USER'+'@'+domain)]));
end;

procedure TFRE_DB_APPLICATION._RemoveDefaultGroups(const sys_connection: IFRE_DB_SYS_CONNECTION);
begin
  sys_connection.RemoveAppGroup(Objectname,'USER');
  sys_connection.RemoveAppGroup(Objectname,'ADMIN');
  sys_connection.RemoveAppGroup(Objectname,'GUEST');
end;

function TFRE_DB_APPLICATION.CreateAppText(const conn: IFRE_DB_SYS_CONNECTION;const translation_key: TFRE_DB_String; const short_text: TFRE_DB_String; const long_text: TFRE_DB_String; const hint_text: TFRE_DB_String): TFRE_DB_Errortype;
var txt :IFRE_DB_TEXT;
begin
  txt := GFRE_DBI.NewText(ObjectName+'_'+translation_key,long_text,short_text,hint_text);
  Result:=conn.StoreTranslateableText(txt);
end;

function TFRE_DB_APPLICATION.FetchAppText(const conn: IFRE_DB_CONNECTION;const translation_key: TFRE_DB_String): IFRE_DB_TEXT;
begin
  //FIXXME ME Heli: please implement me
  if not assigned(conn) then begin
    Result:=GFRE_DBI.CreateText('rt',translation_key+'_noconn_rethink',translation_key+'_noconn_rethink',translation_key+'_noconn_rethink');
  end else begin
    if not conn.FetchTranslateableText(ObjectName+'_'+translation_key,result) then begin
      Result:=GFRE_DBI.CreateText('notfound',translation_key+'_short',translation_key+'_long',translation_key+'_is_missing!');
    end;
  end;
end;

function TFRE_DB_APPLICATION.FetchAppText(const input_context: IFRE_DB_Object; const translation_key: TFRE_DB_String): IFRE_DB_TEXT;
begin
  result := FetchAppText(GetSession(input_context).GetDBConnection,translation_key);
end;

function TFRE_DB_APPLICATION.FetchAppText(const session: IFRE_DB_UserSession; const translation_key: TFRE_DB_String): IFRE_DB_TEXT;
begin
  result := FetchAppText(session.GetDBConnection,translation_key);
end;

function TFRE_DB_APPLICATION.Get_Rightname(const sub_right_name: string): string;
begin
  result := Get_Rightname_App_Helper(ObjectName,sub_right_name);
end;

function TFRE_DB_APPLICATION.CheckAppRightModule(const conn: IFRE_DB_CONNECTION;const module_name: TFRE_DB_String): Boolean;
begin
  result := (not CFG_ApplicationUsesRights) or conn.CheckRight(Get_Rightname('$ACMOD_'+module_name));
end;

function TFRE_DB_APPLICATION.CheckAppRightModule(const input_context: IFRE_DB_Object; const module_name: TFRE_DB_String): Boolean;
begin
  result := CheckAppRightModule(GetSession(input_context).GetDBConnection,module_name);
end;

procedure TFRE_DB_APPLICATION.AddApplicationModule(const module: TFRE_DB_APPLICATION_MODULE);
var FSubmoduleOrder : Integer;
begin
  Field(module.ObjectName).AsObject := module;
  if FieldExists('SubModuleOrder') then begin
    FSubmoduleOrder:=Field('SubModuleOrder').AsInt32;
    inc(FSubModuleOrder);
  end else begin
    FSubModuleOrder:=1;
  end;
  Field('SubModuleOrder').AsInt32 := FSubmoduleOrder;
  Field(module.ObjectName+'_O').AsInt16:=FSubModuleOrder;
end;

function TFRE_DB_APPLICATION.DelegateInvoke(const modulename: TFRE_DB_String; const methname: string; const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  if FieldExists(modulename) then begin
    abort;
    result := Field(modulename).AsObject.Invoke(methname,input,nil,self,nil);
  end else begin
    raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'DelegateInvoke: Module [%s] not found!',[modulename]);
  end;
end;

procedure TFRE_DB_APPLICATION.MySessionInitialize(const session: TFRE_DB_UserSession);
begin

end;

procedure TFRE_DB_APPLICATION.MySessionPromotion(const session: TFRE_DB_UserSession);
begin

end;

procedure TFRE_DB_APPLICATION.MySessionFinalize(const session: TFRE_DB_UserSession);
begin

end;

procedure TFRE_DB_APPLICATION.MyServerInitialize(const admin_dbc: IFRE_DB_CONNECTION);
begin

end;

procedure TFRE_DB_APPLICATION.MyServerFinalize;
begin

end;

procedure TFRE_DB_APPLICATION.ForAllAppModules(const module_iterator: TFRE_DB_APPLICATION_MODULE_ITERATOR);
  procedure FieldIterator(const fld:IFRE_DB_Field);
  var  module :IFRE_DB_APPLICATION_MODULE;
  begin
    if fld.FieldType=fdbft_Object then begin
      if fld.AsObject.Supports(IFRE_DB_APPLICATION_MODULE,module) then begin
        module_iterator(module,Field(fld.FieldName+'_O').AsInt16);
      end;
    end;
  end;

begin
  ForAllFields(@FieldIterator);
end;

function TFRE_DB_APPLICATION.IMI_OnUIChange(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  GetSessionAppData(input).Field('activeSection').AsString:=input.Field('sectionid').AsString;
  Result:=GFRE_DB_NIL_DESC;
end;

class procedure TFRE_DB_APPLICATION.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.SetParentSchemeByName('TFRE_DB_NAMED_OBJECT');
  Scheme.Strict(false);
  Scheme.SetSysDisplayField(GFRE_DBI.ConstructStringArray(['objname','$DBTEXT:desc']),'%s - (%s)');
end;


procedure TFRE_DB_APPLICATION.Finalize;
begin
  free;
end;

function TFRE_DB_APPLICATION.GetDescription(conn: IFRE_DB_CONNECTION): IFRE_DB_TEXT;
begin
  result := FetchAppText(conn,GetDescTranslationKey);
end;

function TFRE_DB_APPLICATION.GetSessionData(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  result := GetSession(input).GetSessionGlobalData;
end;



function TFRE_DB_APPLICATION.GetSessionAppData(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  result := GetSession(input).GetSessionAppData(ObjectName);
end;


function TFRE_DB_APPLICATION.GetAppModulesAsSubSection(const input:IFRE_DB_Object): TFRE_DB_CONTENT_DESC;
begin
  result := G_APPMODS_AS_SUBSECTIONS_CALLBACK(self,input).Implementor_HC as TFRE_DB_CONTENT_DESC;
end;

function TFRE_DB_APPLICATION.AsObject: IFRE_DB_Object;
begin
  result := FImplementor;
end;

function TFRE_DB_APPLICATION.ObjectNameI: TFRE_DB_String;
begin
  result := GetName;
end;

function TFRE_DB_APPLICATION.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  result := GetAppModulesAsSubSection(input);
end;

procedure TFRE_DB_APPLICATION.AddAppToSiteMap(const session: TFRE_DB_UserSession; const parent_entry: TFRE_DB_CONTENT_DESC);
begin
  G_ADD_2_SITEMAP_CALLBACK(self,session,parent_entry);
end;


function TFRE_DB_APPLICATION.ShowInApplicationChooser(const session: IFRE_DB_UserSession): Boolean;
begin
  result := true;
end;

{ TFRE_DB_APPLICATION_MODULE }

procedure TFRE_DB_APPLICATION_MODULE.InternalSetup;
begin
  SetupAppModuleStructure;
end;

procedure TFRE_DB_APPLICATION_MODULE.SetupAppModuleStructure;
begin

end;

procedure TFRE_DB_APPLICATION_MODULE.AddApplicationModule(const module: TFRE_DB_APPLICATION_MODULE);
var FSubmoduleOrder : Integer;
begin
  Field(module.ObjectName).AsObject := module;
  if FieldExists('SubModuleOrder') then begin
    FSubmoduleOrder:=Field('SubModuleOrder').AsInt32;
    inc(FSubModuleOrder);
  end else begin
    FSubModuleOrder:=1;
  end;
  Field('SubModuleOrder').AsInt32 := FSubmoduleOrder;
  Field(module.ObjectName+'_O').AsInt16:=FSubModuleOrder;
end;


procedure TFRE_DB_APPLICATION_MODULE.InitModuleDesc(const objname, descr_translation_key: TFRE_DB_String);
begin
  ObjectName  := objname;
  SetDescrTranslationKey(descr_translation_key)
end;

procedure TFRE_DB_APPLICATION_MODULE.ForAllAppModules(const module_iterator: TFRE_DB_APPLICATION_MODULE_ITERATOR);
  procedure FieldIterator(const fld:IFRE_DB_Field);
  var  module :IFRE_DB_APPLICATION_MODULE;
  begin
    if fld.FieldType=fdbft_Object then begin
      if fld.AsObject.Supports(IFRE_DB_APPLICATION_MODULE,module) then begin
        module_iterator(module,Field(fld.FieldName+'_O').AsInt16);
      end;
    end;
  end;

begin
  ForAllFields(@FieldIterator);
end;

procedure TFRE_DB_APPLICATION_MODULE.MyServerInitializeModule(const admin_dbc: IFRE_DB_CONNECTION);
begin

end;

procedure TFRE_DB_APPLICATION_MODULE.MySessionInitializeModule(const session: TFRE_DB_UserSession);

  procedure _initSubModules(const field: IFRE_DB_FIELD);
  var app_module : IFRE_DB_APPLICATION_MODULE;
  begin
    if field.IsObjectField and field.AsObject.Supports(IFRE_DB_APPLICATION_MODULE,app_module) then begin
      (field.AsObject.Implementor_HC as TFRE_DB_APPLICATION_MODULE).MySessionInitializeModule(session);
    end;
  end;

begin
  ForAllFields(@_initSubModules);
end;

procedure TFRE_DB_APPLICATION_MODULE.MySessionPromotionModule(const session: TFRE_DB_UserSession);
begin

end;

procedure TFRE_DB_APPLICATION_MODULE.MySessionFinalizeModule(const session: TFRE_DB_UserSession);
begin

end;

function TFRE_DB_APPLICATION_MODULE.GetAppModulesAsSubSection(const input: IFRE_DB_Object): TFRE_DB_CONTENT_DESC;
begin
  result := (G_APPMODS_AS_SUBSECTIONS_CALLBACK(self,input).implementor_hc) as TFRE_DB_CONTENT_DESC;
end;

function TFRE_DB_APPLICATION_MODULE.GetEmbeddingApp: TFRE_DB_APPLICATION;
var obj:IFRE_DB_Object;
begin
  repeat
    obj := Parent;
    if obj.IsA(TFRE_DB_APPLICATION.ClassName) then begin
      exit(obj.Implementor_HC as TFRE_DB_APPLICATION);
    end;
  until false;
end;

function TFRE_DB_APPLICATION_MODULE.CheckAppRightModule(const input_context: IFRE_DB_Object; const module_name: TFRE_DB_String): Boolean;
begin
  result := GetEmbeddingApp.CheckAppRightModule(input_context,module_name);
end;

function TFRE_DB_APPLICATION_MODULE.FetchAppText(const input_context: IFRE_DB_Object; const translation_key: TFRE_DB_String): IFRE_DB_TEXT;
begin
  result := GetEmbeddingApp.FetchAppText(input_context,translation_key);
end;

function TFRE_DB_APPLICATION_MODULE.Get_Rightname(const sub_right_name: string): string;
begin
  result := GetEmbeddingApp.Get_Rightname(sub_right_name);
end;

function TFRE_DB_APPLICATION_MODULE.GetToolbarMenu: TFRE_DB_CONTENT_DESC;
begin
  result := nil;
end;

//function TFRE_DB_APPLICATION_MODULE.getApplication: IFRE_DB_APPLICATION;
//begin
//  result := FParentApplication;
//end;

function TFRE_DB_APPLICATION_MODULE.GetDBConnection(const input: IFRE_DB_Object): IFRE_DB_CONNECTION;
begin
  result := GetSession(input).GetDBConnection;
end;

function TFRE_DB_APPLICATION_MODULE.GetDBSessionData(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  result := GetSession(input).GetSessionGlobalData;
end;

function TFRE_DB_APPLICATION_MODULE.GetDBModuleSessionData(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  result := GetSession(input).GetSessionModuleData(ObjectName);
end;

function TFRE_DB_APPLICATION_MODULE.GetDependencyFiltervalues(const input: IFRE_DB_Object; const dependencyfield: string): TFRE_DB_StringArray;
begin
  setlength(result,0);
  if input.FieldExists('dependency') then begin
    if input.Field('dependency').AsObject.FieldExists(dependencyfield) then begin
      result := input.Field('dependency').AsObject.Field(dependencyfield).asobject.Field('filtervalues').AsStringArr;
    end;
  end;
end;


function TFRE_DB_APPLICATION_MODULE.GetName: TFRE_DB_String;
begin
  result := FNamedObject.getName;
end;

procedure TFRE_DB_APPLICATION_MODULE.SetName(const AValue: TFRE_DB_String);
begin
  FNamedObject.SetName(AValue);
end;

procedure TFRE_DB_APPLICATION_MODULE.SetDescrTranslationKey(const val: TFRE_DB_String);
begin
  field('mod_desc_key').AsString:=val;
end;

function TFRE_DB_APPLICATION_MODULE.GetDescrTranslationKey: TFRE_DB_String;
begin
  Result := field('mod_desc_key').AsString;
end;

function TFRE_DB_APPLICATION_MODULE.GetDescription(conn: IFRE_DB_CONNECTION): IFRE_DB_TEXT;
var app: TFRE_DB_APPLICATION;
begin
  result := GetEmbeddingApp.FetchAppText(conn,GetDescrTranslationKey);
end;

function TFRE_DB_APPLICATION_MODULE.AsObject: IFRE_DB_Object;
begin
  result := FImplementor;
end;

function TFRE_DB_APPLICATION_MODULE.IsContentUpdateVisible(const update_content_id: string): Boolean;
begin
  result := GetEmbeddingApp.IsContentUpdateVisible(update_content_id);
end;

function TFRE_DB_APPLICATION_MODULE.IMI_OnUIChange(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  GetDBModuleSessionData(input).Field('activeSection').AsString:=input.Field('sectionid').AsString;
  Result:=GFRE_DB_NIL_DESC;
end;


procedure FREDB_SiteMap_AddEntry(const SiteMapData : IFRE_DB_Object ; const key:string;const caption : String ; const icon : String ; InterAppLink : TFRE_DB_StringArray ;const x,y : integer; const newsCount:Integer; const scale:Single; const enabled:Boolean);
var i            : integer;
    //ial          : TFOSStringArray;
    key_arr      : TFOSStringArray;
    nodekey      : String;
    SiteMapEntry : IFRE_DB_Object;
    ientry       : integer;
    found        : boolean;
    newentry     : IFRE_DB_Object;

begin
  GFRE_BT.SeperateString(key,'/',key_arr);
  SiteMapEntry := SiteMapData;
  for i := 0 to high(key_arr) do begin
    nodekey    := key_arr[i];
    if i<high(key_arr) then begin
      found      := false;
      for ientry := 0 to SiteMapEntry.Field('ENTRIES').ValueCount-1 do begin
        if SiteMapEntry.Field('ENTRIES').AsObjectItem[ientry].Field('ID').asstring = nodekey then begin
          SiteMapEntry := SiteMapEntry.Field('ENTRIES').AsObjectItem[ientry];
          found := true;
          break;
        end;
      end;
      if found = false then begin
        raise EFRE_DB_Exception.Create('SITEMAP ADDENTRY PARENT NOT FOUND '+key);
      end;
    end else begin
      newentry     := GFRE_DBI.NewObject;
      SiteMapEntry.Field('ENTRIES').AddObject(newentry);
      SiteMapEntry := newentry;
    end;
  end;
  SiteMapEntry.Field('ID').AsString    := nodekey;
  SiteMapEntry.Field('NC').AsInt16      := newsCount;
  SiteMapEntry.Field('CAP').AsString    := caption;
  SiteMapEntry.Field('ICO').AsString    := icon;
  //GFRE_BT.SeperateString(InterAppLink,':',ial);
  SiteMapEntry.Field('IAL').AsStringArr := InterAppLink;
  SiteMapEntry.Field('CRD').AsInt32Arr  := TFRE_DB_Int32Array.Create(x,y);
  SiteMapEntry.Field('SCL').AsReal32    := scale;
  SiteMapEntry.Field('DIS').AsBoolean   := not enabled;
end;

procedure FREDB_SiteMap_AddRadialEntry(const SiteMapData: IFRE_DB_Object; const key: string; const caption: String; const icon: String; InterAppLink: String; const newsCount: Integer; const enabled: Boolean);
var i            : integer;
    ial          : TFRE_DB_StringArray;
    key_arr      : TFOSStringArray;
    nodeid       : String;
    SiteMapEntry : IFRE_DB_Object;
    ientry       : integer;
    found        : boolean;
    newentry     : IFRE_DB_Object;
begin
  GFRE_BT.SeperateString(key,'/',key_arr);
  SiteMapEntry := SiteMapData;
  for i := 0 to high(key_arr) do begin
    nodeid    := key_arr[i];
    if i<high(key_arr) then begin
      found      := false;
      for ientry := 0 to SiteMapEntry.Field('ENTRIES').ValueCount-1 do begin
        if SiteMapEntry.Field('ENTRIES').AsObjectItem[ientry].Field('ID').asstring = nodeid then begin
          SiteMapEntry := SiteMapEntry.Field('ENTRIES').AsObjectItem[ientry];
          found := true;
          break;
        end;
      end;
      if found = false then begin
        raise EFRE_DB_Exception.Create('SITEMAP ADDENTRY PARENT NOT FOUND '+key);
      end;
    end else begin
      newentry     := GFRE_DBI.NewObject;
      SiteMapEntry.Field('ENTRIES').AddObject(newentry);
      SiteMapEntry := newentry;
    end;
  end;
  SiteMapEntry.Field('ID').AsString     := nodeid;
  SiteMapEntry.Field('NC').AsInt16      := newsCount;
  SiteMapEntry.Field('CAP').AsString    := caption;
  SiteMapEntry.Field('ICO').AsString    := icon;
  FREDB_SeperateString(InterAppLink,':',ial);
  SiteMapEntry.Field('IAL').AsStringArr := ial;
  SiteMapEntry.Field('SCL').AsReal32    := 1.0;
  SiteMapEntry.Field('DIS').AsBoolean   := not enabled;
end;

procedure FREDB_SiteMap_RadialAutoposition(const SiteMapData: IFRE_DB_Object; const rootangle:integer);
var
  SiteMapRoot  : IFRE_DB_Object;
  x,y          : integer;
  xo,yo        : integer;
  r            : integer;
  scale        : real;

  procedure PlaceChildren(const SiteMapEntry : IFRE_DB_OBject);
  var
    subcount   : integer;
    partangle  : integer;
    parentangle: integer;
    minangle   : integer;
    maxangle   : integer;
    currangle  : integer;
    xp,yp      : integer;
    ientry     : integer;
    rangeangle : integer;
    maxrange   : integer;

    procedure PlaceSubentry(const subentry : IFRE_DB_Object);
    var x,y      : integer;
    begin
      FREDB_PositionSitemapEntry(currangle, r, xp, yp, x, y);
      subentry.Field('PNGL').asint32   := currangle;
      subentry.Field('CRD').AsInt32Arr := TFRE_DB_Int32Array.Create(x,y);
      inc(currangle,partangle);
      if currangle>360 then begin
        currangle := currangle - 360;
      end;
    end;

  begin
    maxrange  := 135;
    partangle := 0;
    subcount  := SiteMapEntry.Field('ENTRIES').ValueCount;
    if subcount>0 then begin
      parentangle := SiteMapEntry.Field('PNGL').asint32;
      if parentangle = -1 then begin   // full circle
        minangle  := rootangle;
        maxangle  := minangle+360;
        partangle := (maxangle-minangle) div (subcount);
      end else begin
        rangeangle := (subcount-1) * (45 div 2);
        if rangeangle > maxrange then begin
          rangeangle := maxrange;
        end;
        minangle   := parentangle - rangeangle;
        maxangle   := parentangle + rangeangle;
        if subcount>1 then begin
          partangle := (maxangle-minangle) div (subcount-1);
        end;
      end;
      currangle := minangle;
      xp        := SiteMapEntry.Field('CRD').AsInt32Item[0];
      yp        := SiteMapEntry.Field('CRD').AsInt32Item[1];
      for ientry := 0 to SiteMapEntry.Field('ENTRIES').ValueCount-1 do begin
        PlaceSubentry(SiteMapEntry.Field('ENTRIES').AsObjectItem[ientry]);
        PlaceChildren(SiteMapEntry.Field('ENTRIES').AsObjectItem[ientry]);
      end;
    end;
  end;

begin
  xo := 300; yo := 300; r := 150; scale := 0.8;
  SiteMapRoot:=SiteMapData.Field('ENTRIES').AsObjectItem[0];
  if assigned(SiteMapRoot) then begin
    // Position RootNode
    SiteMapRoot.Field('CRD').AsInt32Arr  := TFRE_DB_Int32Array.Create(xo,yo);
    SiteMapRoot.Field('SCL').AsReal32    := 1.0;
    SiteMapRoot.Field('PNGL').asint32    := -1;    // Place Children in full circle
    PlaceChildren(SiteMapRoot);
  end;
end;

function FREDB_GuidArray2String(const arr: TFRE_DB_GUIDArray): String;
var i : NativeInt;
begin
  result := '[';
  for i:=0 to high(arr) do
    result:=result+GFRE_BT.GUID_2_HexString(arr[i])+',';
  if Length(arr)>0 then
    result[Length(result)] :=']'
  else
    result:=result+']';
end;

function FREDB_CombineString(const strings: TFRE_DB_StringArray; const sep: string): TFRE_DB_String;
begin

end;

procedure FREDB_PositionSitemapEntry(const angle: integer; const radius: integer; const origin_x, origin_y: integer; out x, y: integer);
var xr : double;
    yr : double;
    phi: double;
    correct : double;
begin
  correct := 1.15;
  phi := (angle*pi)/180;
  xr  := radius * cos (phi);
  yr  := radius * sin (phi);
  x   := origin_x + round (xr * correct);
  y   := origin_y - round (yr);
//  writeln ('Angle: ',angle, 'Phi:',phi, 'X: ',x, 'Y:',y);
end;

function FREDB_Get_Rightname_UID(const rightprefix: string; const id: TGUID): string;
begin
  result := uppercase(rightprefix)+'_'+GFRE_BT.GUID_2_HexString(id);
end;

function FREDB_Get_Rightname_UID_STR(const rightprefix: string; const id_str: String): string;
begin
  result := uppercase(rightprefix)+'_'+UpperCase(id_str);
end;

procedure FREDB_SplitLocalatDomain(const localatdomain: TFRE_DB_String; var localpart, domainpart: TFRE_DB_String);
begin
 if Pos('@',localatdomain)=0 then raise EFRE_DB_Exception.Create('No @ in the full domain name '+localatdomain);
 localpart  := GFRE_BT.SepLeft(localatdomain,'@');
 domainpart := GFRE_BT.SepRight(localatdomain,'@');
end;

function FREDB_String2EscapedJSString(const input_string: TFRE_DB_String; const replace_cr_with_br: boolean): TFRE_DB_String;
begin
  result := StringReplace(input_string      ,'\'    , '\u005C', [rfReplaceAll]);   // Backslash
  result := StringReplace(result            ,#0     , '\u0000', [rfReplaceAll]);   // 0 Char
  result := StringReplace(Result            ,#8     , '\u0008', [rfReplaceAll]);   // Backspace
  result := StringReplace(Result            ,#9     , '\u0009', [rfReplaceAll]);   // Horizontal Tab
  if not replace_cr_with_br then begin
    result := StringReplace(Result          ,#13#10 , '\u000A', [rfReplaceAll]);   // WINDOWS to Single CR
    result := StringReplace(Result          ,#10    , '\u000A', [rfReplaceAll]);   // CR
    result := StringReplace(Result          ,#13    , '\u000D', [rfReplaceAll]);   // LF
  end else begin
    result := StringReplace(Result          ,#13#10 , '<br>', [rfReplaceAll]);   // WINDOWS to Single CR
    result := StringReplace(Result          ,#10    , '<br>', [rfReplaceAll]);   // CR
    result := StringReplace(Result          ,#13    , '<br>', [rfReplaceAll]);   // LF
  end;
  result := StringReplace(Result            ,'"'    , '\u0022', [rfReplaceAll]);   // Double Qoute
  result := StringReplace(Result            ,''''   , '\u0027', [rfReplaceAll]);   // Single Quote
end;

operator<(g1, g2: TGUID)b: boolean;
begin
  b := RB_Guid_Compare(g1,g2) = -1;
end;

operator>(g1, g2: TGUID)b: boolean;
begin
  b := RB_Guid_Compare(g1,g2) = 1;
end;

operator=(g1, g2: TGUID)b: boolean;
begin
  b := RB_Guid_Compare(g1,g2) = 0;
end;

type

  { TFRE_DBI_REG_EXTMGR }

  TFRE_DBI_REG_EXTMGR=class(TOBJECT,IFRE_DB_EXTENSION_MNGR)
  private
    FExtlist : TObjectList;
  public
    constructor create                ;
    destructor  destroy               ; override ;
    procedure Finalize                ;
    function  GetExtensionList        : IFOS_STRINGS;
    procedure ForAllExtensions        (const iterator: IFRE_DB_EXTENSION_Iterator);
    procedure RegisterNewExtension    (const extension_name : String ; const MetaRegistrationFunction : IFRE_DB_EXTENSION_RegisterCB ; const MetaRegisterInitDBFunction : IFRE_DB_EXTENSION_INITDB_CB;  const MetaRegisterRemoveFunction : IFRE_DB_EXTENSION_REMOVE_CB);
    procedure RegisterExtensions4DB   (const list:IFOS_STRINGS);
    procedure InitDatabase4Extensions (const list: IFOS_STRINGS ; const db:string ; const user,pass:string);
    procedure Remove4Extensions       (const list: IFOS_STRINGS ; const db:string ; const user,pass:string);
  end;

  { TFRE_DB_EXTENSION_GRP }

  TFRE_DB_EXTENSION_GRP = class(TObject,IFRE_DB_EXTENSION_GRP)
  private
    FCallBack   : IFRE_DB_EXTENSION_RegisterCB;
    FCallBackDB : IFRE_DB_EXTENSION_INITDB_CB;
    FCallBackREM: IFRE_DB_EXTENSION_REMOVE_CB;
    Fname       : TFRE_DB_String;
  public
    function   GetExtensionName                 : TFRE_DB_String;
    procedure  RegisterExtensionAndDependencies ;
    procedure  InitializeDatabaseForExtension    (const db_name : string ; const user,pass:string);
    procedure  RemoveForExtension                (const db_name: string; const user, pass: string);
  end;

{ TFRE_DB_EXTENSION_GRP }

function TFRE_DB_EXTENSION_GRP.GetExtensionName: TFRE_DB_String;
begin
  result := FName;
end;

procedure TFRE_DB_EXTENSION_GRP.RegisterExtensionAndDependencies;
begin
  FCallBack();
end;

procedure TFRE_DB_EXTENSION_GRP.InitializeDatabaseForExtension(const db_name: string; const user, pass: string);
begin
  FCallBackDB(db_name,user,pass);
end;

procedure TFRE_DB_EXTENSION_GRP.RemoveForExtension(const db_name: string; const user, pass: string);
begin
  FCallBackREM(db_name,user,pass);
end;


{ TFRE_DBI_REG_EXTMGR }

constructor TFRE_DBI_REG_EXTMGR.create;
begin
  inherited;
  FExtlist := TObjectList.create(true);
end;

destructor TFRE_DBI_REG_EXTMGR.destroy;
begin
  FExtlist.free;
  inherited destroy;
end;

procedure TFRE_DBI_REG_EXTMGR.Finalize;
begin
  Destroy;
end;

function TFRE_DBI_REG_EXTMGR.GetExtensionList: IFOS_STRINGS;
  procedure Iterate(const ext : IFRE_DB_EXTENSION_GRP);
  begin
    result.add(ext.GetExtensionName);
  end;
begin
  result := GFRE_TF.Get_FOS_Strings;
  ForAllExtensions(@Iterate);
end;

procedure TFRE_DBI_REG_EXTMGR.ForAllExtensions(const iterator: IFRE_DB_EXTENSION_Iterator);
var i   :  integer;
    ext : TFRE_DB_EXTENSION_GRP;
begin
  for i := 0 to FExtlist.Count-1 do begin
    iterator(FExtlist[i] as TFRE_DB_EXTENSION_GRP);
  end;
end;

procedure TFRE_DBI_REG_EXTMGR.RegisterNewExtension(const extension_name: String; const MetaRegistrationFunction: IFRE_DB_EXTENSION_RegisterCB; const MetaRegisterInitDBFunction: IFRE_DB_EXTENSION_INITDB_CB; const MetaRegisterRemoveFunction : IFRE_DB_EXTENSION_REMOVE_CB);
var i       :  integer;
    new_ext : TFRE_DB_EXTENSION_GRP;
begin
  for i := 0 to FExtlist.Count-1 do begin
    with FExtlist[i] as TFRE_DB_EXTENSION_GRP do begin
      if lowercase(GetExtensionName) = lowercase(extension_name) then exit; // already registerd
    end;
  end;
  new_ext             := TFRE_DB_EXTENSION_GRP.Create;
  new_ext.Fname       := extension_name;
  new_ext.FCallBack   := MetaRegistrationFunction;
  new_ext.FCallBackDB := MetaRegisterInitDBFunction;
  new_ext.FCallBackREM:= MetaRegisterRemoveFunction;
  FExtlist.Add(new_ext);
end;

procedure TFRE_DBI_REG_EXTMGR.RegisterExtensions4DB(const list: IFOS_STRINGS);
  procedure Iterate(const ext : IFRE_DB_EXTENSION_GRP);
  begin
    if list.IndexOf(ext.GetExtensionName)>-1 then begin
      writeln('REGISTERING FOR EXTENSION : ',ext.GetExtensionName);
      ext.RegisterExtensionAndDependencies;
    end;
  end;
begin
  list.SetCaseSensitive(false);
  ForAllExtensions(@Iterate);
end;

procedure TFRE_DBI_REG_EXTMGR.InitDatabase4Extensions(const list: IFOS_STRINGS ; const db: string; const user, pass: string);

  procedure Iterate(const ext : IFRE_DB_EXTENSION_GRP);
  begin
    if list.IndexOf(ext.GetExtensionName)>-1 then begin
      writeln('INITIALIZING DB : ',ext.GetExtensionName,' DB : ',db);
      ext.InitializeDatabaseForExtension(db,user,pass);
    end;
  end;

begin
  list.SetCaseSensitive(false);
  ForAllExtensions(@Iterate);
end;


procedure TFRE_DBI_REG_EXTMGR.Remove4Extensions(const list: IFOS_STRINGS; const db: string; const user, pass: string);

  procedure Iterate(const ext : IFRE_DB_EXTENSION_GRP);
  begin
    if list.IndexOf(ext.GetExtensionName)>-1 then begin
      writeln('REMOVING APPS : ',ext.GetExtensionName,' DB : ',db);
      ext.RemoveForExtension(db,user,pass);
    end;
  end;

begin
  list.SetCaseSensitive(false);
  ForAllExtensions(@Iterate);
end;



initialization
 assert(sizeof(TGUID_Access)=16);
 assert(SizeOf(TFRE_DB_NameType)=64);
 assert(CFRE_DB_NullGUID < CFRE_DB_MaxGUID);
 assert(CFRE_DB_MaxGUID  > CFRE_DB_NullGUID);
 assert(CFRE_DB_MaxGUID  = CFRE_DB_MaxGUID);
 GFRE_DBI_REG_EXTMGR := TFRE_DBI_REG_EXTMGR.create;

finalization
 GFRE_DBI_REG_EXTMGR.Finalize;

end.

