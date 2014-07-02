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
  Classes, SysUtils, FRE_SYSTEM,FOS_TOOL_INTERFACES,FOS_INTERLOCKED,
  FOS_REDBLACKTREE_GEN,FRE_APS_INTERFACE,contnrs,fpjson;


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

    GCFG_SESSION_UNBOUND_TO        : Integer = 600; //if this value is too low some browsers will be stuck in an endless loop

type

  TFRE_DB_SIZE_TYPE           = integer;
  Int16                       = Smallint;
  Int32                       = Longint;
  UInt16                      = word;
  UInt32                      = longword;
  TFRE_DB_GUID_String         = string[sizeof(TGUid)*2]; { hex & lowercase}
  TFRE_DB_String              = type AnsiString(CP_UTF8);
  PFRE_DB_String              = ^TFRE_DB_String;
  TFRE_DB_RawByteString       = RawByteString;
  PNativeUint                 = ^NativeUint;
  PNativeInt                  = ^NativeInt;
  IFRE_DB_RIGHT               = TFRE_DB_String; {a right is a string key}
  TFRE_DB_ConstArray          = Array of TVarRec;

  TFRE_DB_LOGCATEGORY         = (dblc_NONE,dblc_PERSISTANCE,dblc_PERSISTANCE_NOTIFY,dblc_DB,dblc_MEMORY,dblc_REFERENCES,dblc_EXCEPTION,dblc_SERVER,dblc_HTTP_REQ,dblc_HTTP_RES,dblc_WEBSOCK,dblc_APPLICATION,
                                 dblc_SESSION,dblc_FLEXCOM,dblc_SERVER_DATA,dblc_WS_JSON,dblc_FLEX_IO,dblc_APSCOMM,dblc_HTTP_ZIP,dblc_HTTP_CACHE,dblc_STREAMING,dblc_QUERY,dblc_DBTDM);
  TFRE_DB_Errortype           = (edb_OK,edb_ERROR,edb_ACCESS,edb_RESERVED,edb_NOT_FOUND,edb_DB_NO_SYSTEM,edb_EXISTS,edb_INTERNAL,edb_ALREADY_CONNECTED,edb_NOT_CONNECTED,edb_MISMATCH,edb_ILLEGALCONVERSION,edb_INDEXOUTOFBOUNDS,edb_STRING2TYPEFAILED,edb_OBJECT_REFERENCED,edb_INVALID_PARAMS,edb_UNSUPPORTED,edb_NO_CHANGE,edb_PERSISTANCE_ERROR);
  TFRE_DB_FILTERTYPE          = (dbf_TEXT,dbf_SIGNED,dbf_UNSIGNED,dbf_DATETIME,dbf_BOOLEAN,dbf_CURRENCY,dbf_REAL64,dbf_GUID,dbf_SCHEME,dbf_RIGHT,dbf_EMPTY);
  TFRE_DB_STR_FILTERTYPE      = (dbft_EXACT,dbft_PART,dbft_STARTPART,dbft_ENDPART); { currently only the first value of the filter is uses in this modes }
  TFRE_DB_NUM_FILTERTYPE      = (dbnf_EXACT,dbnf_LESSER,dbnf_LESSER_EQ,dbnf_GREATER,dbnf_GREATER_EQ,dbnf_IN_RANGE_EX_BOUNDS,dbnf_IN_RANGE_WITH_BOUNDS,dbnf_AllValuesFromFilter,dbnf_OneValueFromFilter,dbnf_NoValueInFilter);
  TFRE_DB_SchemeType          = (dbst_INVALID,dbst_System,dbst_Extension,dbst_DB);
  TFRE_DB_COMMANDTYPE         = (fct_SyncRequest,fct_SyncReply,fct_AsyncRequest,fct_Error);
  TFRE_DB_SUBSEC_DISPLAY_TYPE = (sec_dt_tab,sec_dt_vertical,sec_dt_hiddentab);
  TFRE_DB_CHOOSER_DH          = (dh_chooser_radio,dh_chooser_check,dh_chooser_combo);

  TFRE_DB_STANDARD_RIGHT      = (sr_BAD,sr_STORE,sr_UPDATE,sr_DELETE,sr_FETCH);  //DB CORE RIGHTS
  TFRE_DB_STANDARD_RIGHT_SET  = set of TFRE_DB_STANDARD_RIGHT;
  TFRE_DB_STANDARD_COLL       = (coll_NONE,coll_USER,coll_GROUP,coll_DOMAIN,coll_WFACTION);


  EFRE_DB_Exception=class(EFRE_Exception)
    ErrorType : TFRE_DB_Errortype;
    constructor Create(const msg : TFRE_DB_String);
    constructor Create(const et:TFRE_DB_Errortype;msg:TFRE_DB_String='');
    constructor Create(const et:TFRE_DB_Errortype;msg:TFRE_DB_String;params:array of const);
  end;

  EFRE_DB_PL_Exception=class(EFRE_DB_Exception)
  end;

  TFRE_DB_FIELDTYPE     = (fdbft_NotFound,fdbft_GUID,fdbft_Byte,fdbft_Int16,fdbft_UInt16,fdbft_Int32,fdbft_UInt32,fdbft_Int64,fdbft_UInt64,fdbft_Real32,fdbft_Real64,fdbft_Currency,fdbft_String,fdbft_Boolean,fdbft_DateTimeUTC,fdbft_Stream,fdbft_Object,fdbft_ObjLink);
  TFRE_DB_DISPLAY_TYPE  = (dt_string,dt_date,dt_number,dt_number_pb,dt_currency,dt_icon,dt_boolean,dt_description);
  TFRE_DB_MESSAGE_TYPE  = (fdbmt_error,fdbmt_warning,fdbmt_info,fdbmt_confirm,fdbmt_wait);
  TFRE_DB_FIELDTYPE_Array = Array of TFRE_DB_FIELDTYPE;

const
  CFRE_DB_FIELDTYPE              : Array[TFRE_DB_FIELDTYPE]               of String = ('UNSET','GUID','BYTE','INT16','UINT16','INT32','UINT32','INT64','UINT64','REAL32','REAL64','CURRENCY','STRING','BOOLEAN','DATE','STREAM','OBJECT','OBJECTLINK');
  CFRE_DB_FIELDTYPE_SHORT        : Array[TFRE_DB_FIELDTYPE]               of String = (    '-',   'G',  'U1',   'I2',    'U2',   'S4',    'U4',   'I8',    'U8',    'R4',    'R8',      'CU',    'SS',     'BO',  'DT',    'ST',    'OB',        'LK');
  CFRE_DB_STANDARD_RIGHT         : Array[TFRE_DB_STANDARD_RIGHT]          of String = ('BAD','STORE','UPDATE','DELETE','FETCH');
  CFRE_DB_Errortype              : Array[TFRE_DB_Errortype]               of String = ('OK','ERROR','ACCESS PROHIBITED','RESERVED','NOT FOUND','SYSTEM DB NOT FOUND','EXISTS','INTERNAL','ALREADY CONNECTED','NOT CONNECTED','MISMATCH','ILLEGALCONVERSION','INDEXOUTOFBOUNDS','STRING2TYPEFAILED','OBJECT IS REFERENCED','INVALID PARAMETERS','UNSUPPORTED','NO CHANGE','PERSISTANCE ERROR');
  CFRE_DB_FILTERTYPE             : Array[TFRE_DB_FILTERTYPE]              of String = ('T','S','U','D','B','C','R','G','X','Z','E');
  CFRE_DB_STR_FILTERTYPE         : Array[TFRE_DB_STR_FILTERTYPE]          of String = ('EX','PA','SP','EP');
  CFRE_DB_NUM_FILTERTYPE         : Array[TFRE_DB_NUM_FILTERTYPE]          of String = ('EX','LE','LEQ','GT','GEQ','REXB','RWIB','AVFF','OVFV','NVFF');
  CFRE_DB_LOGCATEGORY            : Array[TFRE_DB_LOGCATEGORY]             of String = ('-','PL','PL EV','DB','MEMORY','REFLINKS','EXCEPT','SERVER','>HTTP','<HTTP','WEBSOCK','APP','SESSION','FLEXCOM','SRV DATA','WS/JSON','FC/IO','APSCOMM','HTTP ZIP','HTTP CACHE','STREAM','QUERY','DBTDM');
  CFRE_DB_LOGCATEGORY_INI_IDENT  : Array[TFRE_DB_LOGCATEGORY]             of String = ('NONE','PERSISTANCE','PERSISTANCE_NOTIFY','DB','MEMORY','REFERENCES','EXCEPTION','SERVER','HTTP_REQ','HTTP_RES','WEBSOCK','APPLICATION','SESSION','FLEXCOM','SERVER_DATA','WS_JSON','FLEX_IO','APSCOMM','HTTP_ZIP','HTTP_CACHE','STREAMING','QUERY','DBTM');
  CFRE_DB_COMMANDTYPE            : Array[TFRE_DB_COMMANDTYPE]             of String = ('S','SR','AR','E');
  CFRE_DB_DISPLAY_TYPE           : Array[TFRE_DB_DISPLAY_TYPE]            of string = ('STR','DAT','NUM','PRG','CUR','ICO','BOO','DES');
  CFRE_DB_MESSAGE_TYPE           : array [TFRE_DB_MESSAGE_TYPE]           of string = ('msg_error','msg_warning','msg_info','msg_confirm','msg_wait');
  CFRE_DB_SUBSEC_DISPLAY_TYPE    : array [TFRE_DB_SUBSEC_DISPLAY_TYPE]    of string = ('sec_dt_tab','sec_dt_vertical','sec_dt_hiddentab');
  CFRE_DB_SYS_DOMAIN_NAME        = 'SYSTEM';
  cFRE_DB_STKEY                  = '#ST#';
  cFRE_DB_ST_ETAG                = '#ETG#';
  cFRE_DB_SYS_NOCHANGE_VAL_STR   = '*$NOCHANGE*';  { used to indicate a DONT CHANGE THE FIELD in translating from JSON <-> DBO }
  cFRE_DB_SYS_CLEAR_VAL_STR      = '*$CLEAR*';     { used to indicate a CLEAR FIELD in translating from JSON <-> DBO  }
  //cFRE_DB_SYS_ORDER_REF_KEY      = '*$ORK*';     { used to backlink from ordered data(key) to base transformed data }
  cFRE_DB_SYS_PARENT_PATH_FULL   = '*$_PPATH_F*';  { used in a parent child transform to set the pp in the child, full path }
  //cFRE_DB_SYS_PARENT_PATH_PART   = '*$_PPATH_P*';  { used in a parent child transform to set the pp in the child, immediate parent }
  cFRE_DB_SYS_TRANS_IN_OBJ_WAS_A = '*$_TIOWA*';    { used in the transform as implicit field }
  cFRE_DB_SYS_T_OBJ_TOTAL_ORDER  = '*$_TOTO*';     { used in the transform as implicit field, store total order key }

  cFRE_DB_CLN_CHILD_CNT          = '_children_count_';
  cFRE_DB_CLN_CHILD_FLD          = 'children';
  cFRE_DB_CLN_CHILD_FLG          = 'UNCHECKED';

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
    function   AsRawByteString      : TFRE_DB_RawByteString;
    procedure  SetFromRawByteString (const rb_string : TFRE_DB_RawByteString);
    function   CalcETag             : ShortString;
  end;

  TFRE_DB_GUID          = TGuid;
  PFRE_DB_GUID          = ^TFRE_DB_GUID;
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

  TFRE_DB_NameType        = String[63]; // Type for named objects (not data of the DB / no unicode and fixed length)
  PFRE_DB_NameType        = ^TFRE_DB_NameType;
  TFRE_DB_NameTypeRL      = string[129]; // SCHEMENAME(64 + < + FIELDNAME(64)  = 129 Byte (InboundLink)
                                         // FIELDNAME(64) + > + SCHEMENAME(64) = 129 Byte (Outboundlink)
  TFRE_DB_TransStepId           = String[80]; // Nametype+/+number
  TFRE_DB_NameTypeArray         = Array of TFRE_DB_NameType;
  TFRE_DB_NameTypeRLArray       = Array of TFRE_DB_NameTypeRL;
  TFRE_DB_NameTypeRLArrayArray  = Array of TFRE_DB_NameTypeRLArray;
  TFRE_DB_MimeTypeStr           = string[100];

  TFRE_DB_CountedGuid=record
    link  : TGuid;
    count : NativeInt;
  end;

  TFRE_DB_ReferencesByField=record
    fieldname  : TFRE_DB_NameType;
    schemename : TFRE_DB_NameType;
    linked_uid : TFRE_DB_GUID;
  end;

  TFRE_DB_Mimetype=record
    extension : string[32];
    mimetype  : TFRE_DB_MimeTypeStr;
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
  PFRE_DB_ObjectArray                 = ^IFRE_DB_ObjectArray;
  IFRE_DB_ClientFieldValidatorArray   = Array of IFRE_DB_ClientFieldValidator;

  IFRE_DB_BASE   = interface
    procedure Finalize        ;
    function  Implementor     : TObject;
    function  Implementor_HC  : TObject;
  end;

  IFRE_DB_UID_BASE   = interface(IFRE_DB_BASE)
    function  UID             : TGUID;
    function  DomainID        : TFRE_DB_GUID;
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

  IFRE_DB_CALCFIELD_SETTER = interface(IFRE_DB_BASE)
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
  end;

  IFRE_DB_Field  = interface(IFRE_DB_BASE)
  //private // ? - only for info, as interfaces dont support private methods
    {utility functions}
    function  CloneToNewStreamable    : IFRE_DB_Field; { This creates a lightweight "streamable field" copy with only certain supported function (fieldvalues,type, but no parentobject etc support }
    function  CloneToNewStreamableObj : IFRE_DB_Object; { encode the data as object }
    function  RemoveObjectLinkByUID(const to_remove_uid : TFRE_DB_GUID):boolean;
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
    function  AsObjectArrayJSONString: TFRE_DB_String; { Deliver an Object Field, or a TFRE_DB_OBJECTLIST as plain JSON Array}
  //public info
    function  FieldType                     : TFRE_DB_FIELDTYPE;
    function  FieldTypeAsString             : TFRE_DB_String;
    function  ValueCount                    : NativeInt; { delivers the count or if fake object list as subobject the faked count}
    function  ValueCountReal                : NativeInt; { delivers 1 if object }
    function  IsUIDField                    : boolean;
    function  IsDomainIDField               : boolean;
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
    //property  AsObjectLinkArrayObjects      : IFRE_DB_ObjectArray   read GetAsObjectLinkArrayObj write SetAsObjectLinkArrayObj;

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

    procedure  CloneFromField              (const Field:IFRE_DB_FIELD);
    function   CheckOutObject              : IFRE_DB_Object;
    function   CheckOutObjectArray         : IFRE_DB_ObjectArray;
    function   CheckOutObjectArrayItem     (const idx : NAtiveInt): IFRE_DB_Object;

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

    procedure SetAsEmptyStringArray         ;

    procedure Stream2String                 (var raw_string:TFRE_DB_RawByteString);

    function  AsStringDump                  : TFRE_DB_String;
    function  FieldName                     : TFRE_DB_NameType;
    procedure Clear                         (const dont_free_streams_and_objects:boolean=false);
    procedure RemoveIndex                   (const idx:integer);
    procedure IntfCast                      (const InterfaceSpec:ShortString ; out Intf) ; // Interpret as Object and then -> IntfCast throws an Exception if not succesful
    function  AsDBText                      :IFRE_DB_TEXT;
    function  IsEmptyArray                  : boolean;
    function  ParentObject                  : IFRE_DB_Object;
    function  GetUpdateObjectUIDPath        : TFRE_DB_GUIDArray; { This is only set in case of a "clone" stream field (standalone field only) }
    function  GetInCollectionArrayUSL       : TFRE_DB_StringArray; { This is only set in case of a "clone" stream field (standalone field only) }
    function  GetUpdateObjSchemePath        : TFRE_DB_StringArray; { This is only set in case of a "clone" stream field (standalone field only) }
    function  GetUpdateObjFieldPath         : TFRE_DB_StringArray; { This is only set in case of a "clone" stream field (standalone field only) }
    function  IsSpecialClearMarked          : Boolean;             { if a string field and has special clear string mark set => true (usefull for json web interface) }
    function  ConvAsSignedArray             : TFRE_DB_Int64Array;    { for filtering purposes }
    function  ConvAsUnsignedArray           : TFRE_DB_UInt64Array;   { for filtering purposes }
    function  ConvAsCurrencyArray           : TFRE_DB_CurrencyArray; { for filtering purposes }
    function  ConvAsReal64Array             : TFRE_DB_Real64Array;   { for filtering purposes }
  end;


  IFRE_DB_SCHEMEOBJECT          = interface;
  IFRE_DB_Enum                  = interface;
  IFRE_DB_COLLECTION            = interface;
  IFRE_DB_FieldSchemeDefinition = interface;
  IFRE_DB_DOMAIN                = interface;
  IFRE_DB_USER_RIGHT_TOKEN      = interface;

  TFRE_DB_ObjCompareEventType           = (cev_FieldDeleted,cev_FieldAdded,cev_FieldChanged,cev_UpdateBlockStart,cev_UpdateBlockEnd);

  TObjectNestedIterator                 = procedure (const obj : TObject) is nested;
  TObjectNestedDataIterator             = procedure (const obj : TObject ; const data : Pointer) is nested;
  IFRE_DB_FieldIterator                 = procedure (const obj : IFRE_DB_Field) is nested;
  IFRE_DB_FieldIteratorBrk              = function  (const obj : IFRE_DB_Field):boolean is nested;
  IFRE_DB_Obj_Iterator                  = procedure (const obj : IFRE_DB_Object) is nested;
  IFRE_DB_Obj_NameIterator              = procedure (const fieldname : TFRE_DB_NameType ; const obj : IFRE_DB_Object) is nested;
  IFRE_DB_ObjectIteratorBrk             = procedure (const obj:IFRE_DB_Object; var halt:boolean) is nested;
  IFRE_DB_ObjectIteratorBrkProgress     = procedure (const obj:IFRE_DB_Object; var halt:boolean ; const current,max : NativeInt) is nested;
  IFRE_DB_UpdateChange_Iterator         = procedure (const is_child_update : boolean ; const update_obj : IFRE_DB_Object ; const update_type :TFRE_DB_ObjCompareEventType  ;const new_field, old_field: IFRE_DB_Field) is nested;
  IFRE_DB_ObjUid_IteratorBreak          = procedure (const uid : TGUID ; var halt : boolean) is nested;
  IFRE_DB_Scheme_Iterator               = procedure (const obj : IFRE_DB_SchemeObject) is nested;
  IFRE_DB_SchemeFieldDef_Iterator       = procedure (const obj : IFRE_DB_FieldSchemeDefinition) is nested;
  IFRE_DB_Enum_Iterator                 = procedure (const obj : IFRE_DB_Enum) is nested;
  IFRE_DB_ClientFieldValidator_Iterator = procedure (const obj : IFRE_DB_ClientFieldValidator) is nested;
  IFRE_DB_Coll_Iterator                 = procedure (const coll: IFRE_DB_COLLECTION) is nested;
  IFRE_DB_Domain_Iterator               = procedure (const obj : IFRE_DB_Domain) is nested;

  TFRE_DB_StreamingCallback             = procedure (const uid,fieldname: TFRE_DB_String ; const stream:TFRE_DB_Stream) is nested;
  TFRE_DB_PhaseProgressCallback         = procedure (const phase,detail,header : shortstring ; const cnt,max: integer) is nested;


  TFRE_DB_Guid_Iterator                 = procedure (const obj:TGUID) is nested;

  TFRE_DB_ObjectEx                      = class;
  TFRE_DB_OBJECTCLASSEX                 = class of TFRE_DB_ObjectEx;
  TFRE_DB_BaseClass                     = class of TFRE_DB_Base;


  TFRE_DB_FIELD_EXPRESSION              = function(const field:IFRE_DB_Field):boolean is nested;

  IFRE_DB_InvokeMethod                  = function  (const Input:IFRE_DB_Object):IFRE_DB_Object;
  IFRE_DB_InvokeMethodCallbackObjectEx  = function  (const obj: TFRE_DB_ObjectEx ;  const Input:IFRE_DB_Object):IFRE_DB_Object;

  IFRE_DB_InvokeInstanceMethod = function  (const Input:IFRE_DB_Object):IFRE_DB_Object of object;
  IFRE_DB_WebInstanceMethod    = function  (const Input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object of object;
  IFRE_DB_InvokeClassMethod    = function  (const Input:IFRE_DB_Object):IFRE_DB_Object of object;
  IFRE_DB_WebClassMethod       = function  (const Input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object of object;
  IFRE_DB_RemInstanceMethod    = procedure (const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE) of object;

  IFRE_DB_WebTimerMethod       = procedure (const ses: IFRE_DB_Usersession) of object;

  IFRE_DB_CalcMethod           = procedure (const calcfieldsetter : IFRE_DB_CALCFIELD_SETTER) of object;

  IFRE_DB_CS_CALLBACK          = procedure (const Input:IFRE_DB_Object) of Object;
  IFRE_DB_InvokeProcedure      = procedure (const Input:IFRE_DB_Object) of Object;

  TFRE_DB_Object_Properties    = (fop_SYSTEM,fop_READ_ONLY,fop_VOLATILE,fop_STORED_IMMUTABLE,fop_IN_SYSTEM_DB); { fop_SYSTEM=static/read only, fop_IN_SYSTEM_DB = from System DB masterdata}
  TFRE_DB_Object_PropertySet   = set of TFRE_DB_Object_Properties;

  TFRE_InputGroupDefType=(igd_Bad,igd_Field,igd_UsedGroup,igd_UsedSubGroup);

  TFRE_DB_SESSIONSTATE  =(sta_BAD,sta_ActiveNew,sta_ReUsed);

  OFRE_InputFieldDef4Group = record
    typ              : TFRE_InputGroupDefType;
    field            : TFRE_DB_NameType;
    scheme           : string[255]; //path
    collapsible,
    collapsed,
    required,
    disabled,
    hidden           : Boolean;
    group            : TFRE_DB_NameType;
    prefix           : TFRE_DB_NameType;
    datacollection   : TFRE_DB_NameType;
    standardcoll     : TFRE_DB_STANDARD_COLL;
    dc_isdomainc     : Boolean;
    caption_key      : TFRE_DB_NameType;
    chooser_type     : TFRE_DB_CHOOSER_DH;
    chooser_add_empty: Boolean;
    std_right        : TFRE_DB_STANDARD_RIGHT;
    right_classtype  : TClass;
    hideSingle       : Boolean;
    fieldschemdef    : IFRE_DB_FieldSchemeDefinition; // points to
  end;

  PFRE_InputFieldDef4Group    = ^OFRE_InputFieldDef4Group;
  PFRE_InputFieldDef4GroupArr = Array of PFRE_InputFieldDef4Group;

  R_Depfieldfield = record
    depFieldName  : TFRE_DB_NameType;
    disablesField : Boolean;
  end;
  P_Depfieldfield = ^R_Depfieldfield;

  R_VisDepfieldfield = record
    visDepFieldName  : TFRE_DB_NameType;
    visibleValue     : String;
  end;
  P_VisDepfieldfield = ^R_VisDepfieldfield;

  TFRE_DB_Depfielditerator    = procedure (const depfield : R_Depfieldfield) is nested;
  TFRE_DB_VisDepfielditerator = procedure (const depfield : R_VisDepfieldfield) is nested;

   //IFRE_DB_EXTENSION_GRP loosely groups the necessary
   //DB Apps Registery Functions and extension functions, plus dependencies
   //to prepare and query db extensions on startup

  IFRE_DB_EXTENSION_GRP = interface
    function   GetExtensionName                  : TFRE_DB_String;
    procedure  RegisterExtensionAndDependencies  ;
    procedure  InitializeDatabaseForExtension    (const db_name : string ; const user,pass:string);
    procedure  RemoveForExtension                (const db_name : string ; const user,pass:string);
    procedure  GenerateTestdataForExtension      (const db_name : string ; const user,pass:string);
    procedure  DoUnitTestforExtension            (const db_name : string ; const user,pass:string);
  end;
  //
  IFRE_DB_EXTENSION_Iterator   = procedure (const ext:IFRE_DB_EXTENSION_GRP) is nested;
  IFRE_DB_EXTENSION_RegisterCB = procedure ;
  IFRE_DB_EXTENSION_INITDB_CB  = procedure (const dbname :string; const user,pass:string);
  IFRE_DB_EXTENSION_REMOVE_CB  = procedure (const dbname :string; const user,pass:string);

  //
  TFRE_DB_Usersession = class;

  TFRE_DB_SessionIterator = procedure (const session : TFRE_DB_UserSession ; var halt : boolean) is nested ;


  { IFRE_DB_NetServer }

  IFRE_DB_NetServer=interface
    function       ExistsUserSessionForUserLocked            (const username:string;out other_session:TFRE_DB_UserSession):boolean;
    function       ExistsUserSessionForKeyLocked             (const key     :string;out other_session:TFRE_DB_UserSession):boolean;
    function       FetchPublisherSessionLocked               (const rcall,rmeth:TFRE_DB_NameType;out ses : TFRE_DB_UserSession ; out right:TFRE_DB_String):boolean;
    function       FetchSessionByIdLocked                    (const sesid : TFRE_DB_String ; var ses : TFRE_DB_UserSession):boolean;
    procedure      ForAllSessionsLocked                      (const iterator : TFRE_DB_SessionIterator ; var halt : boolean); // If halt, then the dir and the session remain locked!
    function       GetImpersonatedDatabaseConnection         (const dbname,username,pass:TFRE_DB_String ; out dbs:IFRE_DB_CONNECTION):TFRE_DB_Errortype;
    function       GetDBWithServerRights                     (const dbname:TFRE_DB_String ; out dbs:IFRE_DB_CONNECTION):TFRE_DB_Errortype;
    function       CheckUserNamePW                           (username,pass:TFRE_DB_String) : TFRE_DB_Errortype;
    function       SendDelegatedContentToClient              (sessionID : TFRE_DB_String ; const content : TFRE_DB_CONTENT_DESC):boolean;
  end;

  { IFRE_DB_EXTENSION_MNGR }
  IFRE_DB_SYS_CONNECTION = interface;

  IFRE_DB_EXTENSION_MNGR = interface
    function  GetExtensionList        : IFOS_STRINGS;
    procedure RegisterExtensions4DB   (const list: IFOS_STRINGS);
    procedure InitDatabase4Extensions (const list: IFOS_STRINGS ; const db:string ; const user,pass:string);
    procedure GenerateTestData4Exts   (const list: IFOS_STRINGS ; const db:string ; const user,pass:string);
    procedure GenerateUnitTestsdata   (const list: IFOS_STRINGS ; const db:string ; const user,pass:string);
    procedure Remove4Extensions       (const list: IFOS_STRINGS ; const db:string ; const user,pass:string);
    procedure RegisterNewExtension    (const extension_name : String ; const MetaRegistrationFunction : IFRE_DB_EXTENSION_RegisterCB ; const MetaRegisterInitDBFunction : IFRE_DB_EXTENSION_INITDB_CB;  const MetaRegisterRemoveFunction : IFRE_DB_EXTENSION_REMOVE_CB ; const MetaGentestdata : IFRE_DB_EXTENSION_INITDB_CB = nil ; const MetaGenUnitTest : IFRE_DB_EXTENSION_INITDB_CB = nil);
    procedure Finalize                ;
  end;

  { IFRE_DB_Object }

  IFRE_DB_Object = interface(IFRE_DB_INVOKEABLE)
   ['IFREDBO']
    function        _InternalDecodeAsField             : IFRE_DB_Field; { create a streaming only lightweight field from the encoding object }
    procedure       _InternalSetMediatorScheme         (const mediator : TFRE_DB_ObjectEx ; const scheme : IFRE_DB_SCHEMEOBJECT);
    function        UIDP                               : PByte;
    function        PUID                               : PGuid;
    function        ObjectRoot                         : IFRE_DB_Object; // = the last parent with no parent
    procedure       SetReference                       (const obj : TObject);
    function        GetReference                       : TObject;
    function        GetScheme                          (const raise_non_existing:boolean=false): IFRE_DB_SchemeObject;
    {<Think about obsolete removal}
    //function        GetSystemSchemeByName              (const schemename:TFRE_DB_String; var scheme: IFRE_DB_SchemeObject): Boolean;
    //function        GetSystemScheme                    (const schemename:TClass; var scheme: IFRE_DB_SchemeObject): Boolean;
    {>Think about obsolete removal}
    function        GetAsJSON                          (const without_reserved_fields:boolean=false;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil): TJSONData;
    function        GetAsJSONString                    (const without_reserved_fields:boolean=false;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil):TFRE_DB_String;
    function        CloneToNewObject                   (const create_new_uids:boolean=false): IFRE_DB_Object;
    procedure       ForAllFields                       (const iter:IFRE_DB_FieldIterator);
    procedure       ForAllFieldsBreak                  (const iter:IFRE_DB_FieldIteratorBrk);
    procedure       ForAllObjects                      (const iter:IFRE_DB_Obj_Iterator);
    procedure       ForAllObjectsFieldName             (const iter:IFRE_DB_Obj_NameIterator);
    function        GetDescriptionID                   : String;
    function        UID                                : TGUID;
    function        DomainID                           : TGUID;
    function        UID_String                         : TFRE_DB_GUID_String;
    function        NeededSize                         : TFRE_DB_SIZE_TYPE;
    function        Parent                             : IFRE_DB_Object;
    function        ParentField                        : IFRE_DB_FIELD;
    function        AsString                           :TFRE_DB_String;
    //procedure       SetAsEmptyStringArray              ;
    //function        IsEmptyArray                       : boolean;
    function        Field                              (const name:TFRE_DB_NameType):IFRE_DB_FIELD;
    function        FieldOnlyExistingObj               (const name:TFRE_DB_NameType):IFRE_DB_Object;
    function        FieldOnlyExistingObject            (const name:TFRE_DB_NameType; var obj:IFRE_DB_Object):boolean;
    function        FieldOnlyExistingObjAs             (const name:TFRE_DB_NameType; const classref : TFRE_DB_BaseClass ; var outobj) : boolean;
    function        FieldOnlyExisting                  (const name:TFRE_DB_NameType;var fld:IFRE_DB_FIELD):boolean;
    function        FieldPath                          (const name:TFRE_DB_String;const dont_raise_ex:boolean=false):IFRE_DB_FIELD;
    function        FieldPathCreate                    (const name:TFRE_DB_String):IFRE_DB_FIELD;
    function        FieldPathExists                    (const name:TFRE_DB_String):Boolean;
    function        FieldPathListFormat                (const field_list:TFRE_DB_NameTypeArray;const formats : TFRE_DB_String;const empty_val: TFRE_DB_String) : TFRE_DB_String;
    function        FieldCount                         (const without_calcfields:boolean): SizeInt;
    function        DeleteField                        (const name:TFRE_DB_String):Boolean;
    procedure       ClearAllFields                     ;
    function        FieldExists                        (const name:TFRE_DB_String):boolean;
    procedure       DumpToStrings                      (const strings:TStrings;indent:integer=0);
    function        DumpToString                       (indent:integer=0;const dump_length_max:Integer=0):TFRE_DB_String;
    function        GetFormattedDisplay                : TFRE_DB_String;
    function        FormattedDisplayAvailable          : boolean;
    function        SubFormattedDisplayAvailable       : boolean;
    function        GetSubFormattedDisplay             (indent:integer=4):TFRE_DB_String;
    function        SchemeClass                        : TFRE_DB_NameType;
    function        IsA                                (const schemename    : shortstring):Boolean;
    function        IsA                                (const IsSchemeclass : TFRE_DB_OBJECTCLASSEX ; var obj ) : Boolean;
    function        PreTransformedWasA                 (const schemename:shortstring):Boolean;
    function        PreTransformedScheme               :ShortString;
    function        IsObjectRoot                       : Boolean;
    procedure       SaveToFile                         (const filename:TFRE_DB_String);

    function        ReferencesObjectsFromData          : Boolean;
    function        GetFieldListFilter                 (const field_type:TFRE_DB_FIELDTYPE):TFRE_DB_StringArray;
    function        GetUIDPath                         :TFRE_DB_StringArray;
    function        GetUIDPathUA                       :TFRE_DB_GUIDArray;

    function        Invoke                             (const method:TFRE_DB_String;const input:IFRE_DB_Object ; const ses : IFRE_DB_Usersession ; const app : IFRE_DB_APPLICATION ; const conn : IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        Mediator                           : TFRE_DB_ObjectEx;
    procedure       Set_ReadOnly                       ;
    procedure       CopyField                          (const obj:IFRE_DB_Object;const field_name:String);
    procedure       CopyToMemory                       (memory : Pointer);

    function        ForAllObjectsBreakHierarchic       (const iter:IFRE_DB_ObjectIteratorBrk):boolean; // includes root object (self)
    function        FetchObjByUID                      (const childuid:TGuid ; var obj : IFRE_DB_Object):boolean;
    function        FetchObjWithStringFieldValue       (const field_name: TFRE_DB_NameType; const fieldvalue: TFRE_DB_String; var obj: IFRE_DB_Object; ClassnameToMatch: ShortString): boolean;
    procedure       SetAllSimpleObjectFieldsFromObject (const source_object : IFRE_DB_Object); // only first level, no uid, domid, obj, objlink fields
    procedure       SetDomainID                        (const domid:TGUID); { faster }
  end;

  TFRE_DB_TEXT_SUBTYPE=(tst_Short,tst_Long,tst_Hint,tst_Key);

  IFRE_DB_TEXT=interface(IFRE_DB_COMMON)
   ['IFREDBTXT']
    function  GetHint:  TFRE_DB_String;
    function  GetLong:  TFRE_DB_String;
    function  Getshort: TFRE_DB_String;
    function  GetTKey:  TFRE_DB_String;
    procedure SetHint(const AValue: TFRE_DB_String);
    procedure Setlong(const AValue: TFRE_DB_String);
    procedure ClearLong;
    procedure ClearShort;
    procedure ClearHint;
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
    procedure       ForAllBreak         (const func:IFRE_DB_ObjectIteratorBrk ; var halt : boolean);
    function        Remove              (const ouid:TGUID):TFRE_DB_Errortype;
    function        Store               (const new_obj:IFRE_DB_Object):TFRE_DB_Errortype;
    function        Update              (const dbo:IFRE_DB_Object):TFRE_DB_Errortype;
    function        Fetch               (const ouid:TGUID;out dbo:IFRE_DB_Object): boolean;
    function        CollectionName      (const unique:boolean=false): TFRE_DB_NameType;
    function        DomainCollName      (const unique:boolean=false): TFRE_DB_NameType; {cut off the domain uid prefix string}
    function        IsADomainCollection : Boolean;


    function        ItemCount           : Int64;
    function        First               : IFRE_DB_Object;
    function        Last                : IFRE_DB_Object;
    function        GetItem             (const num:uint64):IFRE_DB_Object;
    procedure       ClearCollection     ;
    //Define a basic index according to fieldtype
    function        DefineIndexOnField  (const FieldName   : TFRE_DB_NameType;const FieldType:TFRE_DB_FIELDTYPE;const unique:boolean; const ignore_content_case:boolean=false;const index_name:TFRE_DB_NameType='def' ; const allow_null_value : boolean=true ; const unique_null_values : boolean=false):TFRE_DB_Errortype;
    function        IndexExists         (const index_name:TFRE_DB_NameType):boolean;
    function        ExistsIndexed       (const query_value : TFRE_DB_String;const index_name:TFRE_DB_NameType='def'):Boolean; // for the string fieldtype
    function        GetIndexedObj       (const query_value : TFRE_DB_String; out obj     : IFRE_DB_Object      ; const index_name : TFRE_DB_NameType='def'):boolean; // for the string fieldtype
    function        GetIndexedObjs      (const query_value : TFRE_DB_String; out   obj   : IFRE_DB_ObjectArray ; const index_name : TFRE_DB_NameType='def'):boolean;

    function        GetIndexedUID              (const query_value : TFRE_DB_String ; out obj_uid     : TGUID               ; const index_name : TFRE_DB_NameType='def' ; const val_is_null : boolean = false): boolean;
    function        GetIndexedUIDSigned        (const query_value : int64          ; out obj_uid     : TGUID               ; const index_name : TFRE_DB_NameType='def' ; const val_is_null : boolean = false): boolean;
    function        GetIndexedUIDUnsigned      (const query_value : QWord          ; out obj_uid     : TGUID               ; const index_name : TFRE_DB_NameType='def' ; const val_is_null : boolean = false): boolean;

    function        GetIndexedUID              (const query_value : TFRE_DB_String ; out obj_uid     : TFRE_DB_GUIDArray   ; const index_name : TFRE_DB_NameType='def' ; const check_is_unique : boolean=false ; const val_is_null : boolean = false):boolean; overload ;
    function        GetIndexedUIDSigned        (const query_value : int64          ; out obj_uid     : TFRE_DB_GUIDArray   ; const index_name : TFRE_DB_NameType='def' ; const check_is_unique : boolean=false ; const val_is_null : boolean = false):boolean; overload ;
    function        GetIndexedUIDUnsigned      (const query_value : QWord          ; out obj_uid     : TFRE_DB_GUIDArray   ; const index_name : TFRE_DB_NameType='def' ; const check_is_unique : boolean=false ; const val_is_null : boolean = false):boolean; overload ;

    procedure       GetAllUids                 (var uids:TFRE_DB_GUIDArray);

    procedure       ForAllIndexed       (const func        : IFRE_DB_ObjectIteratorBrk ; var halt : boolean ; const index_name:TFRE_DB_NameType='def';const ascending:boolean=true);

    function        RemoveIndexedString        (const query_value : TFRE_DB_String ; const index_name:TFRE_DB_NameType='def' ; const val_is_null : boolean = false):boolean; // for the string   fieldtype
    function        RemoveIndexedSigned        (const query_value : int64          ; const index_name:TFRE_DB_NameType='def' ; const val_is_null : boolean = false):boolean; // for all signed   fieldtypes
    function        RemoveIndexedUnsigned      (const query_value : QWord          ; const index_name:TFRE_DB_NameType='def' ; const val_is_null : boolean = false):boolean; // for all unsigned fieldtype


    // skip_first = number of different index values to skip, max_count = number of different index values to deliver
    procedure       ForAllIndexedSignedRange   (const min_value,max_value : int64          ; const iterator : IFRE_DB_ObjectIteratorBrk ; var halt:boolean ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const min_is_null : boolean = false ; const max_is_max : boolean = false ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0);
    procedure       ForAllIndexedUnsignedRange (const min_value,max_value : QWord          ; const iterator : IFRE_DB_ObjectIteratorBrk ; var halt:boolean ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const min_is_null : boolean = false ; const max_is_max : boolean = false ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0);
    procedure       ForAllIndexedStringRange   (const min_value,max_value : TFRE_DB_String ; const iterator : IFRE_DB_ObjectIteratorBrk ; var halt:boolean ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const min_is_null : boolean = false ; const max_is_max : boolean = false ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0);
    procedure       ForAllIndexPrefixString    (const prefix              : TFRE_DB_String ; const iterator : IFRE_DB_ObjectIteratorBrk ; var halt:boolean ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0);

    function        IsVolatile          : Boolean;
  end;


  IFRE_DB_TRANSFORMOBJECT = interface;

  TFRE_DB_TRANS_COLL_FILTER_KEY = string[31];

  TFRE_DB_TRANS_COLL_DATA_KEY = record
    key       : shortstring;
    Collname  : TFRE_DB_NameType;
    DC_Name   : TFRE_DB_NameType;
    RL_Spec   : TFRE_DB_NameTypeRL;
    RL_SpecUC : TFRE_DB_NameTypeRL;
    orderkey  : TFRE_DB_Nametype;
    filterkey : string[31];
  end;


  //TFRE_DB_DC_STRINGFIELDKEY_LIST      = array of TFRE_DB_DC_STRINGFIELDKEY;

  TFRE_DB_CHART_TYPE                  = (fdbct_pie,fdbct_column,fdbct_line);
  TFRE_DB_LIVE_CHART_TYPE             = (fdblct_line,fdblct_sampledline,fdblct_column);
  TFRE_COLLECTION_DISPLAY_TYPE        = (cdt_Invalid,cdt_Listview,cdt_Treeview,cdt_Chartview);
  TFRE_COLLECTION_GRID_DISPLAY_FLAG   = (cdgf_ShowSearchbox,cdgf_Editable,cdgf_ColumnResizeable,cdgf_ColumnHideable,cdgf_ColumnDragable,cdgf_Details,cdgf_Children,cdgf_Multiselect);
  TFRE_COLLECTION_CHART_DISPLAY_FLAG  = (cdcf_None);
  TFRE_COLLECTION_TREE_DISPLAY_FLAG   = (cdtf_None);
  TFRE_COLLECTION_GRID_DISPLAY_FLAGS  = set of TFRE_COLLECTION_GRID_DISPLAY_FLAG;
  TFRE_COLLECTION_TREE_DISPLAY_FLAGS  = set of TFRE_COLLECTION_TREE_DISPLAY_FLAG;
  TFRE_COLLECTION_CHART_DISPLAY_FLAGS = set of TFRE_COLLECTION_CHART_DISPLAY_FLAG;

  IFRE_DB_TRANSDATA_CHANGE_NOTIFIER   = interface
  end;

  TFRE_DB_TRANSFORMED_ARRAY_BASE      = class;

  { IFRE_DB_DERIVED_COLLECTION }

  IFRE_DB_DERIVED_COLLECTION=interface(IFRE_DB_COLLECTION)
    [cFOS_IID_DERIVED_COLL]
    procedure  TransformAllTo                (const transdata : TFRE_DB_TRANSFORMED_ARRAY_BASE ; const lazy_child_expand : boolean ; var record_cnt  : NativeInt);
    procedure  TransformSingleUpdate         (const in_object: IFRE_DB_Object; const transdata: TFRE_DB_TRANSFORMED_ARRAY_BASE; const lazy_child_expand: boolean; const upd_idx: NativeInt ; const parentpath_full: TFRE_DB_String);
    procedure  TransformSingleInsert         (const in_object: IFRE_DB_Object; const transdata: TFRE_DB_TRANSFORMED_ARRAY_BASE; const lazy_child_expand: boolean; const rl_ins: boolean; const parentpath: TFRE_DB_String ; const parent_tr_obj : IFRE_DB_Object);
    procedure  FinalRightTransform           (const ses : IFRE_DB_UserSession ; const transformed_filtered_cloned_obj:IFRE_DB_Object);

    function   GetCollectionTransformKey     : TFRE_DB_NameTypeRL; { deliver a key which identifies transformed data depending on ParentCollection and Transformation}
    procedure  BindSession                   (const session : TFRE_DB_UserSession);
    procedure  SetDefaultOrderField          (const field_name:TFRE_DB_String ; const ascending : boolean);
    procedure  RemoveAllFilterFields         ;
    procedure  RemoveAllFiltersPrefix        (const prefix:string);
    function   Count                         : QWord;

    procedure  SetDeriveParent               (const coll:IFRE_DB_COLLECTION;  const idField: String='uid');
    procedure  SetDeriveTransformation       (const tob:IFRE_DB_TRANSFORMOBJECT);
    //{
    //  This Type is only usefull as a Detail/Dependend Grid, as it needs a input Dependency Object
    //  Deliver all Objects which are pointed to by the input "Dependency" object,
    //  or all Objects which point to the the input "Dependency" object,
    //  via a schemelinkdefinition chain : Outbound ['TFRE_DB_SCHEME>DOMAINDILINK', ... ] or Inbound (common) ['TFRE_DB_USER<DOMAINIDLINK']
    //}
    procedure  SetUseDependencyAsRefLinkFilter (const scheme_and_field_constraint : Array of TFRE_DB_NameTypeRL ; const negate : boolean ; const dependency_reference : string = 'uids');

    procedure  SetDisplayType                (const CollectionDisplayType : TFRE_COLLECTION_DISPLAY_TYPE ; const Flags:TFRE_COLLECTION_GRID_DISPLAY_FLAGS;const title:TFRE_DB_String;const CaptionFields:TFRE_DB_StringArray=nil;const TreeNodeIconField:TFRE_DB_String='';const item_menu_func: TFRE_DB_SERVER_FUNC_DESC=nil;const item_details_func: TFRE_DB_SERVER_FUNC_DESC=nil; const grid_item_notification: TFRE_DB_SERVER_FUNC_DESC=nil; const tree_menu_func: TFRE_DB_SERVER_FUNC_DESC=nil; const drop_func: TFRE_DB_SERVER_FUNC_DESC=nil; const drag_func: TFRE_DB_SERVER_FUNC_DESC=nil);
    procedure  SetDisplayTypeChart           (const title: TFRE_DB_String; const chart_type: TFRE_DB_CHART_TYPE; const series_field_names: TFRE_DB_StringArray; const use_series_colors:boolean; const use_series_labels : boolean; const series_labels: TFRE_DB_StringArray=nil; const showLegend: Boolean=false; const maxValue: Integer=0);
    procedure  SetParentToChildLinkField     (const fieldname : TFRE_DB_NameTypeRL); { Define a Child/Parent Parent/Child relation via Reflinks syntax is FROMFIELD>TOSCHEME or FROMSCHEME<FROMFIELD, the scheme could be empty }

    function   HasParentChildRefRelationDefined : boolean;
    function   IsDependencyFilteredCollection   : boolean;
    function   HasReflinksInTransformation      : boolean; { a potential reflink dependency is in the transforms }


    function   GetDisplayDescription         : TFRE_DB_CONTENT_DESC;
    function   GetStoreDescription           : TFRE_DB_CONTENT_DESC;
    function   getDescriptionStoreId         : String;
    procedure  AddSelectionDependencyEvent   (const derived_collection_name : TFRE_DB_NameType ; const ReferenceID :TFRE_DB_NameType='uids'); {On selection change a dependency "filter" object is generated and sent to the derived collection}
    //@
    function   GetDisplayDescriptionFunction (const FilterEventKey:TFRE_DB_String=''): TFRE_DB_SERVER_FUNC_DESC;
    //@ Set a String Filter, which can be used before or after the transformation
    //@ filterkey = ID of the Filter / field_name : on which field the filter works / filtertype: how the filter works

    function   AddStringFieldFilter          (const filter_key,field_name:TFRE_DB_String;const values:TFRE_DB_String ; const filtertype:TFRE_DB_STR_FILTERTYPE):TFRE_DB_Errortype;
    function   AddBooleanFieldFilter         (const filter_key,field_name:TFRE_DB_String;const value :Boolean):TFRE_DB_Errortype;
    function   AddUIDFieldFilter             (const filter_key,field_name:TFRE_DB_String;const values:array of TFRE_DB_GUID ;const number_compare_type : TFRE_DB_NUM_FILTERTYPE):TFRE_DB_Errortype;
    function   AddSchemeFilter               (const filter_key           :TFRE_DB_String;const values:array of TFRE_DB_String;const negate:boolean=false):TFRE_DB_Errortype;
    function   RemoveFilter                  (const filter_key           :TFRE_DB_String):TFRE_DB_Errortype;

    //function   IMI_GET_CHILDREN_DATA   (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  IFRE_DB_SCHEME_COLLECTION=interface(IFRE_DB_COLLECTION)
    [cFOS_IID_SCHEME_COLL]
  end;

  IFRE_DB_TRANSFORMOBJECT = interface(IFRE_DB_BASE)
    ['IFDBTO']
  end;

  IFRE_DB_FINAL_RIGHT_TRANSFORM_FUNCTION = procedure (const ut : IFRE_DB_USER_RIGHT_TOKEN ; const transformed_object : IFRE_DB_Object ; const session_data : IFRE_DB_Object) of object;

  IFRE_DB_SIMPLE_TRANSFORM=interface(IFRE_DB_TRANSFORMOBJECT)
    ['IFDBST']
    procedure AddCollectorscheme             (const format:TFRE_DB_String;const in_fieldlist:TFRE_DB_NameTypeArray;const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const display:Boolean=true;const sortable:Boolean=false; const filterable:Boolean=false;const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const fieldSize: Integer=1;const hide_in_output : boolean=false);
    procedure AddFulltextFilterOnTransformed (const in_fieldlist:TFRE_DB_StringArray;const hide_in_output : boolean=false);
    procedure AddOneToOnescheme              (const fieldname:TFRE_DB_String;const out_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const display:Boolean=true;const sortable:Boolean=false; const filterable:Boolean=false;const fieldSize: Integer=1;const iconID:String='';const openIconID:String='';const default_value:TFRE_DB_String='';const hide_in_output : boolean=false);
    procedure AddMultiToOnescheme            (const in_fieldlist:TFRE_DB_NameTypeArray;const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const display:Boolean=true;const sortable:Boolean=false; const filterable:Boolean=false;const fieldSize: Integer=1;const iconID:String='';const openIconID:String='';const default_value:TFRE_DB_String='';const hide_in_output : boolean=false);
    procedure AddProgressTransform           (const valuefield:TFRE_DB_String;const out_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const textfield:TFRE_DB_String='';const out_text:TFRE_DB_String='';const maxValue:Single=100;const sortable:Boolean=false; const filterable:Boolean=false;const fieldSize: Integer=1;const hide_in_output : boolean=false);
    procedure AddConstString                 (const out_field,value:TFRE_DB_String;const display: Boolean=false;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const sortable:Boolean=false; const filterable:Boolean=false;const fieldSize: Integer=1;const hide_in_output : boolean=false);
    procedure AddDBTextToOne                 (const fieldname:TFRE_DB_String;const which_text : TFRE_DB_TEXT_SUBTYPE ; const out_field:TFRE_DB_String;const output_title:TFRE_DB_String='';const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const sortable:Boolean=false; const filterable:Boolean=false;const fieldSize: Integer=1;const hide_in_output : boolean=false);
    procedure AddMatchingReferencedField     (const ref_field_chain: array of TFRE_DB_NameTypeRL ; const target_field:TFRE_DB_String;const output_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const display:Boolean=true;const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const sortable:Boolean=false; const filterable:Boolean=false;const fieldSize: Integer=1;const hide_in_output : boolean=false);
    procedure AddMatchingReferencedField     (const ref_field      : TFRE_DB_NameTypeRL          ; const target_field:TFRE_DB_String;const output_field:TFRE_DB_String='';const output_title:TFRE_DB_String='';const display:Boolean=true;const gui_display_type:TFRE_DB_DISPLAY_TYPE=dt_string;const sortable:Boolean=false; const filterable:Boolean=false;const fieldSize: Integer=1;const hide_in_output : boolean=false);
    procedure SetFinalRightTransformFunction (const func : IFRE_DB_FINAL_RIGHT_TRANSFORM_FUNCTION); { set a function that changes the object after, transfrom, order, and filter as last step before data deliverance }
  end;


  IFRE_DB_FieldSchemeDefinition=interface //(IFRE_DB_BASE)
    ['IFDBFSD']
    function   GetFieldName        : TFRE_DB_NameType;
    function   GetFieldType        : TFRE_DB_FIELDTYPE;
    function   GetSubSchemeName    : TFRE_DB_NameType;
    function   getMultiValues      : Boolean;
    function   getRequired         : Boolean;
    function   getValidator        (var validator: IFRE_DB_ClientFieldValidator):boolean;
    function   getEnum             (var enum : IFRE_DB_Enum) : boolean;
    procedure  setMultiValues      (AValue: Boolean);
    procedure  setRequired         (AValue: Boolean);
    function   getIsPass           : Boolean;
    function   getAddConfirm       : Boolean;

    property   isPass              :Boolean read getIsPass;
    property   addConfirm          :Boolean read getAddConfirm;

    function   SetupFieldDef     (const is_required:boolean;const is_multivalue:boolean=false;const enum_key:TFRE_DB_NameType='';const validator_key:TFRE_DB_NameType='';const is_pass:Boolean=false; const add_confirm:Boolean=false ; const validator_params : IFRE_DB_Object=nil):IFRE_DB_FieldSchemeDefinition;
    procedure  SetCalcMethod     (const calc_method:IFRE_DB_CalcMethod);
    function   IsACalcField      : Boolean;
    procedure  addDepField       (const fieldName: TFRE_DB_String;const disablesField: Boolean=true);
    procedure  addVisDepField    (const fieldName: TFRE_DB_String;const visibleValue:String);
    property   FieldName         :TFRE_DB_NameType   read GetFieldName;
    property   FieldType         :TFRE_DB_FIELDTYPE  read GetFieldType;
    property   SubschemeName     :TFRE_DB_NameType   read GetSubSchemeName;
    function   GetSubScheme      :IFRE_DB_SchemeObject;
    property   required          :Boolean read getRequired write setRequired;
    property   multiValues       :Boolean read getMultiValues write setMultiValues;
    function   ValidateField     (const field_to_check:IFRE_DB_FIELD;const raise_exception:boolean=true):boolean;
    procedure  ForAllDepfields   (const depfielditerator : TFRE_DB_Depfielditerator);
    procedure  ForAllVisDepfields(const depfielditerator : TFRE_DB_VisDepfielditerator);
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
    procedure addEntry   (const value:TFRE_DB_String;const cap_trans_key: TFRE_DB_String);
    function  getEntries :IFRE_DB_ObjectArray;
    function  CheckField (const field_to_check:IFRE_DB_FIELD;const raise_exception:boolean):boolean;
  end;

  { IFRE_DB_COMMAND }

  IFRE_DB_COMMAND        = interface;

  TFRE_DB_COMMAND_STATUS = (cdcs_OK,cdcs_TIMEOUT,cdcs_ERROR);
  TFRE_DB_CONT_HANDLER   = procedure(const DATA : IFRE_DB_Object ; const status:TFRE_DB_COMMAND_STATUS ; const error_txt:string) of object;
  TFRE_DB_CMD_Request    = procedure(const sender:TObject; const CMD  : IFRE_DB_COMMAND) of object;

  IFRE_DB_COMMAND_REQUEST_ANSWER_SC = interface(IFRE_DB_BASE)
   ['IFDBCRASC']
    procedure Send_ServerClient         (const dbc : IFRE_DB_COMMAND);
    procedure DeactivateSessionBinding  (const from_session : boolean=false);
    procedure UpdateSessionBinding      (const new_session : TObject);
    function  GetInfoForSessionDesc     : String;
    function  GetChannel                : IFRE_APSC_CHANNEL;
  end;

  IFRE_DB_COMMAND=interface(IFRE_DB_BASE)
    ['IFDBCMD']
    function     GetBinDataKey: string;
    function     GetCommandID     : UInt64;
    function     GetCType         : TFRE_DB_COMMANDTYPE;
    function     GetEText         : TFRE_DB_String;
    function     GetFatalClose    : Boolean;
    function     GetChangeSessionKey    : String;
    procedure    SetBinDataKey          (AValue: string);
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
    property     Answer        : Boolean read   GetIsAnswer     write SetIsAnswer;
    property     ClientCommand : Boolean read   GetIsClient     write SetIsClient;
    function     CheckoutData  : IFRE_DB_Object;
    //function     AsJSONString  : TFRE_DB_RawByteString;
    function     AsDBODump     : TFRE_DB_RawByteString;
    //public
    property     CommandID     : UInt64              read GetCommandID    write SetCommandID;
    property     InvokeClass   : String              read GetInvokeClass  write SetInvokeClass;
    property     InvokeMethod  : String              read GetInvokeMethod write SetInvokeMethod;
    property     Data          : IFRE_DB_Object      read GetData         write SetData;
    property     UidPath       : TFRE_DB_GUIDArray   read GetUidPath      write SetUidPath;
    property     CommandType   : TFRE_DB_COMMANDTYPE read GetCType        write SetCType;
    property     ErrorText     : TFRE_DB_String      read GetEText        write SetEText;
    property     FatalClose    : Boolean             read GetFatalClose   write SetFatalClose;
    property     ChangeSession : String              read GetChangeSessionKey   write SetChangeSessionKey;
    property     BinaryDataKey : string              read GetBinDataKey   write SetBinDataKey;
  end;

  { IFRE_DB_ROLE }

  IFRE_DB_ROLE = interface(IFRE_DB_NAMED_OBJECT_PLAIN)
    ['IFDBRIGR']
    function  GetDomain                    (const conn  : IFRE_DB_CONNECTION): TFRE_DB_NameType;
    procedure AddRight                     (const right : IFRE_DB_RIGHT);
    function  GetIsInternal                : Boolean;
    function  GetRightNames                : TFRE_DB_StringArray;
    procedure AddRightsFromRole            (const role : IFRE_DB_ROLE);
    procedure SetIsInternal                (AValue: Boolean);
    property  isInternal                   : Boolean read GetIsInternal write SetIsInternal;
  end;

  IFRE_DB_USER=interface;

  { IFRE_DB_USER_RIGHT_TOKEN }

  IFRE_DB_USER_RIGHT_TOKEN=interface
    function    GetSysDomainID               : TFRE_DB_GUID;
    function    GetMyDomainID                : TFRE_DB_GUID;
    function    CheckStdRightAndCondFinalize (const dbi : IFRE_DB_Object ; const sr : TFRE_DB_STANDARD_RIGHT ; const without_right_check: boolean=false;const cond_finalize:boolean=true) : TFRE_DB_Errortype;
    function    CheckStdRightSetUIDAndClass  (const obj_uid, obj_domuid: TFRE_DB_GUID; const check_classname: ShortString; const sr: TFRE_DB_STANDARD_RIGHT_SET): TFRE_DB_Errortype;
    { Safe case, use for single domain use cases }
    function    CheckClassRight4MyDomain     (const right_name:TFRE_DB_String;const classtyp: TClass):boolean; { and systemuser and systemdomain}
    { Many domain case, add additional checks for the specific domain }
    function    CheckClassRight4AnyDomain    (const right_name:TFRE_DB_String;const classtyp: TClass):boolean;
    function    CheckClassRight4Domain       (const right_name:TFRE_DB_String;const classtyp: TClass;const domainKey:TFRE_DB_String=''):boolean;
    function    GetDomainsForRight           (const right_name:TFRE_DB_String): TFRE_DB_GUIDArray;
    function    GetDomainsForClassRight      (const right_name:TFRE_DB_String;const classtyp: TClass): TFRE_DB_GUIDArray;

    { Stdrights Many domain case, add additional checks for the specific domain }
    function    CheckClassRight4MyDomain     (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass):boolean;
    function    CheckClassRight4AnyDomain    (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass):boolean;

    function    CheckClassRight4Domain       (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass;const domainKey:TFRE_DB_String=''):boolean; { specific domain }
    function    CheckClassRight4DomainId     (const right_name: TFRE_DB_String; const classtyp: TClass; const domain: TGuid): boolean;
    function    CheckClassRight4DomainId     (const std_right: TFRE_DB_STANDARD_RIGHT; const classtyp: TClass; const domain: TGuid): boolean;
    function    GetDomainsForClassRight      (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass): TFRE_DB_GUIDArray;

    function    CheckObjectRight             (const right_name : TFRE_DB_String         ; const uid : TGUID ):boolean;
    function    CheckObjectRight             (const std_right  : TFRE_DB_STANDARD_RIGHT ; const uid : TGUID ):boolean; // New is senseless
    function    User                         : IFRE_DB_USER;
    function    GetUniqueTokenKey            : TFRE_DB_NameType;
  end;

  { IFRE_DB_USER }

  IFRE_DB_USER=interface(IFRE_DB_UID_BASE)
    ['IFDBUSER']
    function  GetFirstName: TFRE_DB_String;
    function  GetIsInternal: Boolean;
    function  GetLastName: TFRE_DB_String;
    function  GetLogin: TFRE_DB_String;
    function  GetDomain      (const conn:IFRE_DB_CONNECTION): TFRE_DB_NameType;
    function  GetDomainIDLink: TGUID;
    function  GetUserGroupIDS: TFRE_DB_ObjLinkArray;

    procedure SetFirstName(const AValue: TFRE_DB_String);
    procedure SetIsInternal(AValue: Boolean);
    procedure SetLastName(const AValue: TFRE_DB_String);
    procedure Setlogin(const AValue: TFRE_DB_String);

    property  Login              :TFRE_DB_String read GetLogin write Setlogin;
    property  Firstname          :TFRE_DB_String read GetFirstName write SetFirstName;
    property  Lastname           :TFRE_DB_String read GetLastName write SetLastName;
    property  isInternal         :Boolean read GetIsInternal write SetIsInternal;
    procedure SetPassword        (const pw:TFRE_DB_String);
    function  Checkpassword      (const pw:TFRE_DB_String):boolean;
  end;


  { IFRE_DB_DOMAIN }

  IFRE_DB_DOMAIN=interface(IFRE_DB_BASE)
    ['IFDBUSERDOMAIN']
    function  Domainkey                               : TFRE_DB_GUID_String;
    function  Domainname     (const unique:boolean=false) : TFRE_DB_NameType;
    function  GetIsInternal  : Boolean;
    function  GetSuspended   : Boolean;
    procedure SetIsInternal (AValue: Boolean);
    procedure SetSuspended  (AValue: Boolean);
    function  UID            : TGUID;
    property  isInternal    :Boolean read GetIsInternal write SetIsInternal;
    property  Suspended     :Boolean read GetSuspended write SetSuspended;
  end;


  { IFRE_DB_GROUP }

  IFRE_DB_GROUP=interface(IFRE_DB_NAMED_OBJECT_PLAIN)
    ['IFDBUSERGRP']
    function  GetDomain                    (const conn :IFRE_DB_CONNECTION): TFRE_DB_NameType;
    function  DomainID                     : TGUID;
    function  GetIsInternal                : Boolean;
    function  GetIsProtected               : Boolean;
    procedure SetIsInternal                (AValue: Boolean);
    procedure SetIsProtected               (AValue: Boolean);
    property  isProtected                  :Boolean read GetIsProtected write SetIsProtected;
    property  isInternal                   :Boolean read GetIsInternal write SetIsInternal;
  end;



  IFRE_DB_ClientFieldValidator       = interface(IFRE_DB_NAMED_OBJECT_PLAIN)
    ['IFDBCV']
    function  getRegExp       :TFRE_DB_String;
    function  getInfoText     :IFRE_DB_TEXT;
    function  getHelpText     :IFRE_DB_TEXT;
    function  getAllowedChars :TFRE_DB_String;
    function  Setup           (const regExp:TFRE_DB_String; const infoText: IFRE_DB_TEXT; const helpText: IFRE_DB_TEXT=nil; const allowedChars:TFRE_DB_String=''): IFRE_DB_ClientFieldValidator;
  end;

  IFRE_DB_InputGroupSchemeDefinition = interface;

  { IFRE_DB_SCHEMEOBJECT }

  IFRE_DB_SCHEMEOBJECT=interface
   ['IFDBSO']
    function  Implementor                 :TObject;
    function  GetExplanation              :TFRE_DB_String;
    procedure SetExplanation              (AValue: TFRE_DB_String);
    function  GetAll_IMI_Methods          :TFRE_DB_StringArray;
    function  MethodExists                (const name:TFRE_DB_String):boolean;
    function  AddSchemeField              (const newfieldname :TFRE_DB_NameType ; const newfieldtype:TFRE_DB_FIELDTYPE ):IFRE_DB_FieldSchemeDefinition;
    function  AddCalcSchemeField          (const newfieldname :TFRE_DB_NameType ; const newfieldtype:TFRE_DB_FIELDTYPE ; const calc_method  : IFRE_DB_CalcMethod):IFRE_DB_FieldSchemeDefinition;
    function  AddSchemeFieldSubscheme     (const newfieldname :TFRE_DB_NameType ; const sub_scheme:TFRE_DB_NameType):IFRE_DB_FieldSchemeDefinition;
    function  GetSchemeField              (const fieldname    :TFRE_DB_NameType ; var fieldschemedef:IFRE_DB_FieldSchemeDefinition): boolean;
    function  GetSchemeField              (const fieldname    :TFRE_DB_NameType): IFRE_DB_FieldSchemeDefinition;
    function  IsA                         (const schemename   :shortstring):Boolean;
    procedure SetSimpleSysDisplayField    (const field_name   :TFRE_DB_String);
    procedure SetSysDisplayField          (const field_names  :TFRE_DB_NameTypeArray;const format:TFRE_DB_String);
    function  GetFormattedDisplay         (const obj : IFRE_DB_Object):TFRE_DB_String;
    function  FormattedDisplayAvailable   (const obj : IFRE_DB_Object):boolean;
    function  DefinedSchemeName           : TFRE_DB_String;
    procedure Strict                      (const only_defined_fields:boolean);
    procedure SetParentSchemeByName       (const parentschemename:TFRE_DB_String);
    function  GetParentScheme             :IFRE_DB_SchemeObject;
    function  GetParentSchemeName         :TFRE_DB_String;
    procedure SetObjectFieldsWithScheme   (const Raw_Object: IFRE_DB_OBject; const Update_Object: IFRE_DB_Object;const new_object:boolean;const DBConnection:IFRE_DB_CONNECTION;const schemeType: TFRE_DB_String='');
    //@ Defines new InputGroup, to use with IMI_CONTENT
    function  AddInputGroup                (const id: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;
    function  ReplaceInputGroup            (const id: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;
    function  GetInputGroup                (const id: TFRE_DB_String): IFRE_DB_InputGroupSchemeDefinition;

    function  GetSchemeType                : TFRE_DB_SchemeType;
    function  ValidateObject               (const dbo : IFRE_DB_Object;const raise_errors:boolean=true):boolean;

    function  InvokeMethod_UID             (const suid : TGUID;const methodname:TFRE_DB_String;var input:IFRE_DB_Object;const connection:IFRE_DB_CONNECTION):IFRE_DB_Object;
    procedure ForAllFieldSchemeDefinitions (const iterator:IFRE_DB_SchemeFieldDef_Iterator);
    property  Explanation:TFRE_DB_String read GetExplanation write SetExplanation;
  end;


  TFRE_DB_TRANSACTION_TYPE=(dbt_Implicit_RD,dbt_Implicit_WR,dbt_OLTP_RD,dbt_OLTP_WR,dbt_OLAP_RD);

  IFRE_DB_TRANSACTION=interface
    function  GetType  : boolean;
    procedure Rollback;
    procedure Commit;
  end;


  IFRE_DB_PERSISTANCE_COLLECTION_4_PERISTANCE_LAYER=interface
    procedure     StoreInThisColl     (const new_obj         : IFRE_DB_Object ; const checkphase : boolean);
    procedure     UpdateInThisColl    (const new_fld,old_fld : IFRE_DB_FIELD  ; const old_obj,new_obj : IFRE_DB_Object ; const update_typ : TFRE_DB_ObjCompareEventType ; const in_child_obj : boolean ; const checkphase : boolean);
    function      DefineIndexOnFieldReal (const checkonly : boolean;const FieldName   : TFRE_DB_NameType ; const FieldType : TFRE_DB_FIELDTYPE   ; const unique     : boolean ; const ignore_content_case: boolean ; const index_name : TFRE_DB_NameType ; const allow_null_value : boolean=true ; const unique_null_values: boolean=false): TFRE_DB_Errortype;

    procedure     DeleteFromThisColl  (const del_obj         : IFRE_DB_Object ; const checkphase : boolean);
    procedure     StreamToThis        (const stream          : TStream);
    procedure     LoadFromThis        (const stream          : TStream);
    procedure     RestoreFromObject   (const obj:IFRE_DB_Object);
    function      FetchIntFromColl    (const uid:TGuid ; var obj : IFRE_DB_Object):boolean;
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

  IFRE_DB_PERSISTANCE_LAYER=interface;

  { IFRE_DB_PERSISTANCE_COLLECTION }

  IFRE_DB_PERSISTANCE_COLLECTION=interface
    function        GetPersLayerIntf           : IFRE_DB_PERSISTANCE_COLLECTION_4_PERISTANCE_LAYER;
    function        GetPersLayer               : IFRE_DB_PERSISTANCE_LAYER;

    function        GetCollectionClassName     : ShortString;
    function        IsVolatile                 : boolean;
    function        CollectionName             (const unique:boolean=true):TFRE_DB_NameType;
    function        Count                      : int64;
    function        Exists                     (const ouid: TGUID): boolean;
    procedure       GetAllUIDS                 (var uids : TFRE_DB_GUIDArray);
    procedure       GetAllObjects              (var objs : IFRE_DB_ObjectArray);

    function        Fetch                      (const uid:TGUID ; var obj : IFRE_DB_Object) : boolean;
    function        First                      : IFRE_DB_Object;
    function        Last                       : IFRE_DB_Object;
    function        GetItem                    (const num:uint64) : IFRE_DB_Object;
    function        DefineIndexOnField         (const FieldName   : TFRE_DB_NameType ; const FieldType : TFRE_DB_FIELDTYPE   ; const unique     : boolean ; const ignore_content_case: boolean ; const index_name : TFRE_DB_NameType ; const allow_null_value : boolean=true ; const unique_null_values: boolean=false): TFRE_DB_Errortype;
    function        IndexExists                (const idx_name : TFRE_DB_NameType):NativeInt;
    // Fetches Snapshot copies of the objects, you need to finalize them
    function        GetIndexedObj              (const query_value : TFRE_DB_String ; out   obj       : IFRE_DB_Object;const index_name:TFRE_DB_NameType='def'  ; const val_is_null : boolean = false ):boolean; // for the string fieldtype
    function        GetIndexedObj              (const query_value : TFRE_DB_String ; out   obj       : IFRE_DB_ObjectArray ; const index_name : TFRE_DB_NameType='def' ; const check_is_unique : boolean=false  ; const val_is_null : boolean = false):boolean;
    function        GetIndexedUID              (const query_value : TFRE_DB_String ; out obj_uid     : TGUID               ; const index_name : TFRE_DB_NameType='def'  ; const val_is_null : boolean = false): boolean;
    function        GetIndexedUID              (const query_value : TFRE_DB_String ; out obj_uid     : TFRE_DB_GUIDArray   ; const index_name : TFRE_DB_NameType='def' ; const check_is_unique : boolean=false  ; const val_is_null : boolean = false):boolean; overload ;

    function        GetIndexedUIDSigned        (const query_value : int64            ; out obj_uid     : TGUID               ; const index_name : TFRE_DB_NameType='def' ; const val_is_null : boolean = false): boolean;
    function        GetIndexedUIDUnsigned      (const query_value : QWord            ; out obj_uid     : TGUID               ; const index_name : TFRE_DB_NameType='def' ; const val_is_null : boolean = false): boolean;
    function        GetIndexedUIDReal          (const query_value : Double           ; out obj_uid     : TGUID               ; const index_name : TFRE_DB_NameType='def' ; const val_is_null : boolean = false): boolean;

    function        GetIndexedUIDSigned        (const query_value : int64          ; out obj_uid     : TFRE_DB_GUIDArray   ; const index_name : TFRE_DB_NameType='def' ; const check_is_unique : boolean=false ; const val_is_null : boolean = false):boolean; overload ;
    function        GetIndexedUIDUnsigned      (const query_value : QWord          ; out obj_uid     : TFRE_DB_GUIDArray   ; const index_name : TFRE_DB_NameType='def' ; const check_is_unique : boolean=false ; const val_is_null : boolean = false):boolean; overload ;
    function        GetIndexedUIDReal          (const query_value : Double         ; out obj_uid     : TFRE_DB_GUIDArray   ; const index_name : TFRE_DB_NameType='def' ; const check_is_unique : boolean=false ; const val_is_null : boolean = false):boolean; overload ;

    function        Remove                     (const ouid    : TGUID):TFRE_DB_Errortype; { from this collection }
    function        RemoveIndexedString        (const query_value : TFRE_DB_String ; const index_name:TFRE_DB_NameType='def' ; const val_is_null : boolean = false):boolean; // for the string   fieldtype
    function        RemoveIndexedSigned        (const query_value : int64          ; const index_name:TFRE_DB_NameType='def' ; const val_is_null : boolean = false):boolean; // for all signed   fieldtypes
    function        RemoveIndexedUnsigned      (const query_value : QWord          ; const index_name:TFRE_DB_NameType='def' ; const val_is_null : boolean = false):boolean; // for all unsigned fieldtype
    function        RemoveIndexedReal          (const query_value : Double         ; const index_name:TFRE_DB_NameType='def' ; const val_is_null : boolean = false):boolean; // for all real fieldtypes

    procedure       ForAllIndexed              (var guids : TFRE_DB_GUIDArray ; const index_name:TFRE_DB_NameType='def'; const ascending:boolean=true ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0);

    procedure       ForAllIndexedSignedRange   (const min_value,max_value : int64          ; var   guids    : TFRE_DB_GUIDArray ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const min_is_null : boolean = false ; const max_is_max : boolean = false ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0);
    procedure       ForAllIndexedUnsignedRange (const min_value,max_value : QWord          ; var   guids    : TFRE_DB_GUIDArray ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const min_is_null : boolean = false ; const max_is_max : boolean = false ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0);
    procedure       ForAllIndexedRealRange     (const min_value,max_value : Double         ; var   guids    : TFRE_DB_GUIDArray ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const min_is_null : boolean = false ; const max_is_max : boolean = false ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0);
    procedure       ForAllIndexedStringRange   (const min_value,max_value : TFRE_DB_String ; var   guids    : TFRE_DB_GUIDArray ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const min_is_null : boolean = false ; const max_is_max : boolean = false ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0);
    procedure       ForAllIndexPrefixString    (const prefix              : TFRE_DB_String ; var   guids    : TFRE_DB_GUIDArray ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0);
  end;

  IFRE_DB_PERSISTANCE_COLLECTION_ARRAY = array of IFRE_DB_PERSISTANCE_COLLECTION;

  { IFRE_DB_PERSISTANCE_LAYER }

  IFRE_DB_DBChangedNotificationBlock = interface; { sending a bulk change from an transaction }
  IFRE_DB_DBChangedNotification      = interface; { recording changes in the transaction }

  IFRE_DB_PERSISTANCE_LAYER=interface
    procedure DEBUG_DisconnectLayer         (const db:TFRE_DB_String;const clean_master_data :boolean = false);

    procedure WT_StoreCollectionPersistent  (const coll:IFRE_DB_PERSISTANCE_COLLECTION);
    procedure WT_StoreObjectPersistent      (const obj: IFRE_DB_Object; const no_store_locking: boolean=true);
    procedure WT_DeleteCollectionPersistent (const collname : TFRE_DB_NameType);
    procedure WT_DeleteObjectPersistent     (const iobj:IFRE_DB_Object);
    procedure WT_TransactionID              (const number:qword);
    function  WT_GetSysLayer                : IFRE_DB_PERSISTANCE_LAYER;

    function  FDB_GetObjectCount            (const coll:boolean; const SchemesFilter:TFRE_DB_StringArray=nil): Integer;
    procedure FDB_ForAllObjects             (const cb:IFRE_DB_ObjectIteratorBrk; const SchemesFilter:TFRE_DB_StringArray=nil);
    procedure FDB_ForAllColls               (const cb:IFRE_DB_Obj_Iterator);
    function  FDB_GetAllCollsNames          :TFRE_DB_NameTypeArray;
    procedure FDB_PrepareDBRestore          (const phase:integer);
    procedure FDB_SendObject                (const obj:IFRE_DB_Object);
    procedure FDB_SendCollection            (const obj:IFRE_DB_Object);
    function  INT_Fetch                     (const ouid    :  TGUID  ; out   dbo:IFRE_DB_Object):boolean;

    function  GetConnectedDB                : TFRE_DB_NameType;
    function  GetLastError                  : TFRE_DB_String;
    function  GetLastErrorCode              : TFRE_DB_Errortype;
    function  ExistCollection               (const coll_name : TFRE_DB_NameType) : Boolean;
    function  GetCollection                 (const coll_name : TFRE_DB_NameType ; out Collection: IFRE_DB_PERSISTANCE_COLLECTION) : Boolean;
    function  NewCollection                 (const coll_name : TFRE_DB_NameType ; const CollectionClassname : Shortstring ; out Collection: IFRE_DB_PERSISTANCE_COLLECTION; const volatile_in_memory: boolean): TFRE_DB_TransStepId;
    function  DeleteCollection              (const coll_name : TFRE_DB_NameType ) : TFRE_DB_TransStepId;

    function  Connect                       (const db_name:TFRE_DB_String ; out db_layer : IFRE_DB_PERSISTANCE_LAYER ; const drop_wal : boolean=false ; const NotifIF : IFRE_DB_DBChangedNotificationBlock=nil) : TFRE_DB_Errortype;
    function  Disconnect                    : TFRE_DB_Errortype;
    function  DatabaseList                  : IFOS_STRINGS;
    function  DatabaseExists                (const dbname:TFRE_DB_String):Boolean;
    function  CreateDatabase                (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
    function  DeleteDatabase                (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
    procedure Finalize                      ;

    function  GetReferences                 (const obj_uid:TGuid;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):TFRE_DB_GUIDArray;
    function  GetReferencesCount            (const obj_uid:TGuid;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):NativeInt;
    function  GetReferencesDetailed         (const obj_uid:TGuid;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):TFRE_DB_ObjectReferences;

    function  StartTransaction              (const typ:TFRE_DB_TRANSACTION_TYPE ; const ID:TFRE_DB_NameType) : TFRE_DB_Errortype;
    function  Commit                        : boolean;
    procedure RollBack                      ;

    function  ObjectExists                  (const obj_uid : TGUID) : boolean;
    function  DeleteObject                  (const obj_uid : TGUID  ; const collection_name: TFRE_DB_NameType = ''):TFRE_DB_TransStepId;
    function  Fetch                         (const ouid    :  TGUID  ; out   dbo:IFRE_DB_Object):TFRE_DB_Errortype;
    function  BulkFetch                     (const obj_uids: TFRE_DB_GUIDArray ; out objects : IFRE_DB_ObjectArray):TFRE_DB_Errortype;
    function  StoreOrUpdateObject           (const obj     : IFRE_DB_Object ; const collection_name : TFRE_DB_NameType ; const store : boolean) : TFRE_DB_TransStepId;
    function  DefineIndexOnField            (const coll_name: TFRE_DB_NameType ; const FieldName   : TFRE_DB_NameType ; const FieldType : TFRE_DB_FIELDTYPE   ; const unique     : boolean ; const ignore_content_case: boolean ; const index_name : TFRE_DB_NameType ; const allow_null_value : boolean=true ; const unique_null_values: boolean=false): TFRE_DB_TransStepId;

    procedure SyncWriteWAL                  (const WALMem : TMemoryStream);
    procedure SyncSnapshot                  (const final : boolean=false);
    function  GetNotificationRecordIF       : IFRE_DB_DBChangedNotification; { to record changes }
  end;

  IFRE_DB_DBChangedNotification = interface
    procedure  StartNotificationBlock (const key : TFRE_DB_TransStepId);
    procedure  FinishNotificationBlock(out block : IFRE_DB_Object);
    procedure  SendNotificationBlock  (const block : IFRE_DB_Object);
    procedure  CollectionCreated      (const coll_name: TFRE_DB_NameType) ;
    procedure  CollectionDeleted      (const coll_name: TFRE_DB_NameType) ;
    procedure  IndexDefinedOnField    (const coll_name: TFRE_DB_NameType  ; const FieldName: TFRE_DB_NameType; const FieldType: TFRE_DB_FIELDTYPE; const unique: boolean; const ignore_content_case: boolean; const index_name: TFRE_DB_NameType; const allow_null_value: boolean; const unique_null_values: boolean);
    procedure  IndexDroppedOnField    (const coll_name: TFRE_DB_NameType  ; const index_name: TFRE_DB_NameType);
    procedure  ObjectStored           (const coll_name: TFRE_DB_NameType  ; const obj : IFRE_DB_Object); { FULL STATE }
    procedure  ObjectDeleted          (const obj : IFRE_DB_Object);                                      { FULL STATE }
    procedure  ObjectUpdated          (const obj : IFRE_DB_Object ; const colls:TFRE_DB_StringArray);    { FULL STATE }
    procedure  SubObjectStored        (const obj : IFRE_DB_Object ; const parent_field_name : TFRE_DB_NameType ; const ParentObjectUIDPath : TFRE_DB_GUIDArray);
    procedure  SubObjectDeleted       (const obj : IFRE_DB_Object ; const parent_field_name : TFRE_DB_NameType ; const ParentObjectUIDPath : TFRE_DB_GUIDArray);
    procedure  DifferentiallUpdStarts (const obj_uid   : IFRE_DB_Object);           { DIFFERENTIAL STATE}
    procedure  FieldDelete            (const old_field : IFRE_DB_Field);            { DIFFERENTIAL STATE}
    procedure  FieldAdd               (const new_field : IFRE_DB_Field);            { DIFFERENTIAL STATE}
    procedure  FieldChange            (const old_field,new_field : IFRE_DB_Field);  { DIFFERENTIAL STATE}
    procedure  DifferentiallUpdEnds   (const obj_uid   : TFRE_DB_GUID);             { DIFFERENTIAL STATE}
    procedure  ObjectRemoved          (const coll_name: TFRE_DB_NameType ; const obj : IFRE_DB_Object);
    procedure  SetupOutboundRefLink   (const from_obj : TGUID            ; const to_obj: IFRE_DB_Object ; const key_description : TFRE_DB_NameTypeRL);
    procedure  SetupInboundRefLink    (const from_obj : IFRE_DB_Object   ; const to_obj: TGUID          ; const key_description : TFRE_DB_NameTypeRL);
    procedure  InboundReflinkDropped  (const from_obj: IFRE_DB_Object    ; const to_obj   : TGUID       ; const key_description : TFRE_DB_NameTypeRL);
    procedure  OutboundReflinkDropped (const from_obj : TGUID            ; const to_obj: IFRE_DB_Object ; const key_description: TFRE_DB_NameTypeRL);
    procedure  FinalizeNotif          ;
  end;

  IFRE_DB_DBChangedNotificationBlock=interface
    procedure  SendNotificationBlock  (const block : IFRE_DB_Object); { encapsulate a block of changes from a transaction }
  end;

  IFRE_DB_DBChangedNotificationConnection = interface { handle metadata changes }
    procedure  CollectionCreated      (const coll_name: TFRE_DB_NameType) ;
    procedure  CollectionDeleted      (const coll_name: TFRE_DB_NameType) ;
    procedure  IndexDefinedOnField    (const coll_name: TFRE_DB_NameType  ; const FieldName: TFRE_DB_NameType; const FieldType: TFRE_DB_FIELDTYPE; const unique: boolean; const ignore_content_case: boolean; const index_name: TFRE_DB_NameType; const allow_null_value: boolean; const unique_null_values: boolean);
    procedure  IndexDroppedOnField    (const coll_name: TFRE_DB_NameType  ; const index_name: TFRE_DB_NameType);
  end;


  IFRE_DB_DBChangedNotificationSession = interface
    procedure  DifferentiallUpdStarts (const obj_uid   : IFRE_DB_Object);           { DIFFERENTIAL STATE}
    procedure  FieldDelete            (const old_field : IFRE_DB_Field);            { DIFFERENTIAL STATE}
    procedure  FieldAdd               (const new_field : IFRE_DB_Field);            { DIFFERENTIAL STATE}
    procedure  FieldChange            (const old_field,new_field : IFRE_DB_Field);  { DIFFERENTIAL STATE}
    procedure  DifferentiallUpdEnds   (const obj_uid   : TFRE_DB_GUID);             { DIFFERENTIAL STATE}
  end;


  TFRE_DB_APPLICATION       = Class;
  TFRE_DB_APPLICATIONCLASS  = Class of TFRE_DB_APPLICATION;

  IFRE_DB_APPLICATION_ARRAY = array of IFRE_DB_APPLICATION;
  TFRE_DB_APPLICATION_ARRAY = array of TFRE_DB_APPLICATION;

  { IFRE_DB_CONNECTION }

  IFRE_DB_CONNECTION=interface(IFRE_DB_BASE)
  ['IFDB_CONN']
    procedure   BindUserSession               (const session : IFRE_DB_Usersession);
    procedure   ClearUserSessionBinding       ;

    function    GetlastError                  : TFRE_DB_String;
    function    GetDatabaseName               : TFRE_DB_String;
    function    Connect                       (const db,user,pass:TFRE_DB_String):TFRE_DB_Errortype;
    function    CheckLogin                    (const user,pass:TFRE_DB_String):TFRE_DB_Errortype;

    function    CollectionExists              (const name:TFRE_DB_NameType):boolean;
    function    DomainCollectionExists        (const name:TFRE_DB_NameType; const ForDomainName : TFRE_DB_NameType='' ; const ForDomainUIDString: TFRE_DB_NameType=''):boolean;
    function    DeleteDomainCollection        (const name:TFRE_DB_NameType; const ForDomainName: TFRE_DB_NameType=''; const ForDomainUIDString: TFRE_DB_NameType=''):TFRE_DB_Errortype;

    function    DeleteCollection              (const name:TFRE_DB_NameType):TFRE_DB_Errortype;
    function    Delete                        (const ouid: TGUID): TFRE_DB_Errortype;

    function    Fetch                         (const ouid:TGUID;out dbo:IFRE_DB_Object)          : TFRE_DB_Errortype;
    function    FetchAs                       (const ouid:TGUID;const classref : TFRE_DB_BaseClass ; var outobj) : TFRE_DB_Errortype;
    function    FetchAsIntf                   (const ouid:TGUID;const IntfSpec:ShortString; out Intf) : TFRE_DB_Errortype;
    function    Update                        (const dbo:IFRE_DB_OBJECT)                          : TFRE_DB_Errortype;


    function    GetCollection                 (const collection_name: TFRE_DB_NameType) : IFRE_DB_COLLECTION;
    function    CreateCollection              (const collection_name: TFRE_DB_NameType;const in_memory:boolean=false) : IFRE_DB_COLLECTION;

    function    GetDomainCollection           (const collection_name: TFRE_DB_NameType;const ForDomainID : TFRE_DB_NameType='' ; const ForDomainUIDString: TFRE_DB_NameType='') : IFRE_DB_COLLECTION;
    function    CreateDomainCollection        (const collection_name: TFRE_DB_NameType;const in_memory:boolean=false; const ForDomainName : TFRE_DB_NameType='' ; const ForDomainUIDString: TFRE_DB_NameType='')  : IFRE_DB_COLLECTION;
    function    DomainCollectionName          (const collection_name: TFRE_DB_NameType;const ForDomainID : TFRE_DB_NameType='' ; const ForDomainUIDString: TFRE_DB_NameType='') : TFRE_DB_NameType; { the uid is given as string because a GUID cannot be used as default parameter }

    function    CreateDerivedCollection       (const collection_name: TFRE_DB_NameType): IFRE_DB_DERIVED_COLLECTION;

    function    GetDatabaseObjectCount        (const Schemes:TFRE_DB_StringArray=nil):NativeInt;
    procedure   ForAllDatabaseObjectsDo       (const dbo:IFRE_DB_ObjectIteratorBrkProgress ; const Schemes:TFRE_DB_StringArray=nil); { Warning may take some time, delivers a clone }
    procedure   ForAllColls                   (const iterator:IFRE_DB_Coll_Iterator)                                   ;
    procedure   ForAllSchemes                 (const iterator:IFRE_DB_Scheme_Iterator)                                 ;
    procedure   ForAllEnums                   (const iterator:IFRE_DB_Enum_Iterator)                                   ;
    procedure   ForAllClientFieldValidators   (const iterator:IFRE_DB_ClientFieldValidator_Iterator)                   ;
    function    InvokeMethod                  (const class_name,method_name:TFRE_DB_String;const uid_path:TFRE_DB_GUIDArray;var input:IFRE_DB_Object;const session:TFRE_DB_UserSession):IFRE_DB_Object;

    procedure   ExpandReferences              (ObjectList : TFRE_DB_GUIDArray ; ref_constraints : TFRE_DB_NameTypeRLArray ;  var expanded_refs : TFRE_DB_GUIDArray);
    procedure   ExpandReferences              (ObjectList : TFRE_DB_GUIDArray ; ref_constraints : TFRE_DB_NameTypeRLArray ;  var expanded_refs : IFRE_DB_ObjectArray);

    function    IsReferenced                  (const obj_uid:TGuid;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):Boolean;//without right check
    function    GetReferences                 (const obj_uid:TGuid;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):TFRE_DB_GUIDArray;
    function    GetReferencesCount            (const obj_uid:TGuid;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):NativeInt;
    function    GetReferencesDetailed         (const obj_uid:TGuid;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):TFRE_DB_ObjectReferences;

    function    FetchUserSessionData          (var SessionData: IFRE_DB_OBJECT):boolean;
    function    StoreUserSessionData          (var session_data:IFRE_DB_Object):TFRE_DB_Errortype;
    function    OverviewDump                  : TFRE_DB_String;

    function    SYS                           : IFRE_DB_SYS_CONNECTION;

    function    FetchApplications             (var apps : IFRE_DB_APPLICATION_ARRAY)  : TFRE_DB_Errortype; // with user rights
    function    FetchTranslateableTextOBJ     (const translation_key:TFRE_DB_String; var textObj: IFRE_DB_TEXT):Boolean; // Warning: finalize the TEXTOBJ!
    function    FetchTranslateableTextShort   (const translation_key:TFRE_DB_String):TFRE_DB_String;
    function    FetchTranslateableTextLong    (const translation_key:TFRE_DB_String):TFRE_DB_String;
    function    FetchTranslateableTextHint    (const translation_key:TFRE_DB_String):TFRE_DB_String;

    function    AdmGetUserCollection            :IFRE_DB_COLLECTION;
    function    AdmGetRoleCollection            :IFRE_DB_COLLECTION;
    function    AdmGetGroupCollection           :IFRE_DB_COLLECTION;
    function    AdmGetDomainCollection          :IFRE_DB_COLLECTION;
    function    AdmGetAuditCollection           :IFRE_DB_COLLECTION;
    function    AdmGetWorkFlowCollection        :IFRE_DB_COLLECTION;
    function    AdmGetWorkFlowSchemeCollection  :IFRE_DB_COLLECTION;
    function    AdmGetWorkFlowMethCollection    :IFRE_DB_COLLECTION;
    function    GetSysDomainUID                 :TGUID;

    function    AddDomain                     (const domainname:TFRE_DB_NameType;const txt,txt_short:TFRE_DB_String):TFRE_DB_Errortype;
    procedure   DrawScheme                    (const datastream:TStream; const classfile:string);

  end;


  { IFRE_DB_SYS_CONNECTION }

  IFRE_DB_SYS_CONNECTION=interface(IFRE_DB_BASE)
  ['IFDB_SYS_CONN']
    //function    GetNotif                    : IFRE_DB_DBChangedNotification; { get the notif of an impersonated (cloned) connection}
    function    GetClassesVersionDirectory  : IFRE_DB_Object;
    function    StoreClassesVersionDirectory(const version_dbo : IFRE_DB_Object) : TFRE_DB_Errortype;
    function    DelClassesVersionDirectory  : TFRE_DB_Errortype;
    function    Connect                     (const loginatdomain,pass:TFRE_DB_String):TFRE_DB_Errortype;
    function    CheckLogin                  (const user,pass:TFRE_DB_String):TFRE_DB_Errortype;

    function    AddUser                     (const login:TFRE_DB_String; const domainUID: TGUID;const password,first_name,last_name:TFRE_DB_String;const image : TFRE_DB_Stream=nil; const imagetype : String='';const is_internal:Boolean=false;const long_desc : TFRE_DB_String='' ; const short_desc : TFRE_DB_String=''):TFRE_DB_Errortype;
    function    UserExists                  (const login:TFRE_DB_String; const domainUID: TGUID):boolean;
    function    DeleteUser                  (const login:TFRE_DB_String; const domainUID: TGUID):TFRE_DB_Errortype;
    function    DeleteUserById              (const user_id:TGUID):TFRE_DB_Errortype;
    function    FetchUser                   (const login:TFRE_DB_String; const domainUID: TGUID;var user:IFRE_DB_USER):TFRE_DB_Errortype;
    function    FetchUserById               (const user_id:TGUID;var user: IFRE_DB_USER):TFRE_DB_Errortype;
    function    FetchGroup                  (const group:TFRE_DB_String;const domainUID: TGUID;var ug: IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    FetchGroupById              (const group_id:TGUID;var ug: IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    ModifyGroupById             (const group_id:TGUID; const groupname : TFRE_DB_NameType; const txt,txt_short:TFRE_DB_String):TFRE_DB_Errortype;
    function    FetchRole                   (const rolename:TFRE_DB_String;const domainUID: TGUID;var role: IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    FetchRoleById               (const role_id:TGUID;var role: IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    FetchDomainById             (const domain_id:TGUID;var domain: IFRE_DB_DOMAIN):TFRE_DB_Errortype;
    function    FetchDomainNameById         (const domain_id:TGUID):TFRE_DB_NameType;
    function    FetchDomainUIDbyName        (const name :TFRE_DB_NameType; var domain_uid:TFRE_DB_GUID):boolean;
    function    ModifyDomainById            (const domain_id:TGUID; const domainname : TFRE_DB_NameType; const txt,txt_short:TFRE_DB_String):TFRE_DB_Errortype; { use special value "*$NOCHANGE$*" for unchanged webfields }
    function    DeleteDomainById            (const domain_id:TGUID):TFRE_DB_Errortype;
    function    FetchTranslateableText      (const translation_key:TFRE_DB_String; var textObj: IFRE_DB_TEXT):Boolean;//don't finalize the object
    function    NewRole                     (const rolename,txt,txt_short:TFRE_DB_String;const is_internal:Boolean; var role  :IFRE_DB_ROLE):TFRE_DB_Errortype;
    function    NewGroup                    (const groupname,txt,txt_short:TFRE_DB_String;const is_protected:Boolean; const is_internal:Boolean; var user_group:IFRE_DB_GROUP):TFRE_DB_Errortype;
    function    AddGroup                    (const groupname,txt,txt_short:TFRE_DB_String;const domainUID:TGUID; const is_protected:Boolean=false;const is_internal:Boolean=false):TFRE_DB_Errortype;
    function    AddRole                     (const rolename,txt,txt_short:TFRE_DB_String;const domainUID:TGUID; const is_internal:Boolean=false):TFRE_DB_Errortype;
    function    AddRolesToGroupById         (const group:TFRE_DB_String;const domainUID: TGUID;const role_ids: TFRE_DB_GUIDArray):TFRE_DB_Errortype;
    function    AddRolesToGroup             (const group:TFRE_DB_String;const domainUID: TGUID;const roles: TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    AddSysRolesToGroup          (const group:TFRE_DB_String;const domainUID: TGUID;const roles: TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    RemoveAllRolesFromGroup     (const group:TFRE_DB_String;const domainUID: TGUID): TFRE_DB_Errortype;
    function    RemoveRolesFromGroup        (const group:TFRE_DB_String;const domainUID: TGUID;const roles: TFRE_DB_StringArray; const ignore_not_set:boolean): TFRE_DB_Errortype; //TODO: Remove Ignorenotset
    function    RemoveRolesFromGroupById    (const group:TFRE_DB_String;const domainUID: TGUID;const role_ids: TFRE_DB_GUIDArray; const ignore_not_set:boolean): TFRE_DB_Errortype; //TODO: Remove Ignorenotset
    function    AddRoleRightsToRole         (const rolename:TFRE_DB_String;const domainUID: TGUID;const roles: TFRE_DB_StringArray):TFRE_DB_Errortype;
    function    ModifyUserGroupsById        (const user_id:TGuid; const user_group_ids:TFRE_DB_GUIDArray; const keep_existing_groups:boolean=false):TFRE_DB_Errortype;
    function    RemoveUserGroupsById        (const user_id:TGuid; const user_group_ids:TFRE_DB_GUIDArray):TFRE_DB_Errortype;
    function    ModifyUserPassword          (const login:TFRE_DB_String; const domainUID: TGUID;const oldpassword,newpassword:TFRE_DB_String):TFRE_DB_Errortype;

    function    RoleExists                  (const role:TFRE_DB_String;const domainUID: TGUID):boolean;
    function    GroupExists                 (const group:TFRE_DB_String;const domainUID: TGUID):boolean;
    function    DeleteGroup                 (const group:TFRE_DB_String;const domainUID: TGUID):TFRE_DB_Errortype;
    function    DeleteGroupById             (const group_id:TGuid):TFRE_DB_Errortype;
    function    DeleteRole                  (const role:TFRE_DB_String;const domainUID: TGUID):TFRE_DB_Errortype;
    function    DomainExists                (const domainname:TFRE_DB_NameType):boolean;
    function    DomainID                    (const domainname:TFRE_DB_NameType):TGUID;
    function    DeleteDomain                (const domainname:TFRE_DB_Nametype):TFRE_DB_Errortype;
    procedure   ForAllDomains               (const func:IFRE_DB_Domain_Iterator);
    function    FetchAllDomainUids          : TFRE_DB_GUIDArray;

    function    StoreRole                   (var   role:IFRE_DB_ROLE; const domainUID : TGUID ):TFRE_DB_Errortype;
    function    StoreGroup                  (var group:IFRE_DB_GROUP;const domainUID: TGUID): TFRE_DB_Errortype;
    function    UpdateGroup                 (var group:IFRE_DB_GROUP): TFRE_DB_Errortype;
    function    UpdateRole                  (var role:IFRE_DB_ROLE): TFRE_DB_Errortype;
    function    UpdateDomain                (var domain:IFRE_DB_DOMAIN): TFRE_DB_Errortype;
    function    UpdateUser                  (var user:IFRE_DB_USER): TFRE_DB_Errortype;

    function    StoreTranslateableText      (const txt    :IFRE_DB_TEXT) :TFRE_DB_Errortype;
    function    UpdateTranslateableText     (const txt    :IFRE_DB_TEXT) :TFRE_DB_Errortype;
    function    DeleteTranslateableText     (const key    :TFRE_DB_String) :TFRE_DB_Errortype;

    function    DatabaseExists              (const dbname:TFRE_DB_String):Boolean;
    function    CreateDatabase              (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
    function    DeleteDatabase              (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
    function    BackupDatabaseReadable      (const sys,adb : TStream;const progress : TFRE_DB_PhaseProgressCallback):TFRE_DB_Errortype;
    function    RestoreDatabaseReadable     (const sys,adb : TStream;const db_name:string;const progress : TFRE_DB_PhaseProgressCallback):TFRE_DB_Errortype;
    function    OverviewDump                :TFRE_DB_String;
    procedure   DumpSystem                  ;

    //function    CheckRightForGroup          (const right_name:TFRE_DB_String;const group_uid : TGuid) : boolean;
    procedure   StartTransaction            (const trans_id: TFRE_DB_NameType ; const trans_type : TFRE_DB_TRANSACTION_TYPE);
    procedure   Commit                      ;
    procedure   DrawScheme                  (const datastream:TStream; const classfile:string);

    function    GetDatabaseObjectCount      (const Schemes:TFRE_DB_StringArray=nil):NativeInt;
    procedure   ForAllDatabaseObjectsDo     (const dbo:IFRE_DB_ObjectIteratorBrkProgress ; const Schemes:TFRE_DB_StringArray=nil ); { Warning may take some time, delivers a clone }

    function    CheckClassRight4MyDomain    (const right_name:TFRE_DB_String;const classtyp: TClass):boolean;
    function    CheckClassRight4MyDomain    (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass):boolean;

    function    CheckClassRight4AnyDomain   (const right_name:TFRE_DB_String;const classtyp: TClass):boolean;
    function    CheckClassRight4Domain      (const right_name:TFRE_DB_String;const classtyp: TClass;const domainKey:TFRE_DB_String):boolean;
    function    CheckClassRight4DomainId    (const right_name:TFRE_DB_String;const classtyp: TClass;const domain:TGuid):boolean;
    function    GetDomainsForClassRight     (const right_name:TFRE_DB_String;const classtyp: TClass): TFRE_DB_GUIDArray;

    function    CheckClassRight4AnyDomain   (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass):boolean;
    function    CheckClassRight4Domain      (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass;const domainKey:TFRE_DB_String):boolean;
    function    CheckClassRight4DomainId    (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass;const domain:TGuid):boolean;
    function    GetDomainsForClassRight     (const std_right:TFRE_DB_STANDARD_RIGHT;const classtyp: TClass): TFRE_DB_GUIDArray;

    function    CheckObjectRight            (const right_name : TFRE_DB_String         ; const uid : TGUID ):boolean;
    function    CheckObjectRight            (const std_right  : TFRE_DB_STANDARD_RIGHT ; const uid : TGUID ):boolean; // New is sensless

    //function    IsCurrentUserSystemAdmin    :boolean;
    function    SuspendContinueDomainById   (const domain_id:TGUID; const suspend : boolean):TFRE_DB_Errortype;
    function    IsDomainSuspended           (const domainname:TFRE_DB_NameType):boolean; {delivers true if the domain exists and is suspended, otherwise false}

    function    DumpUserRights              :TFRE_DB_String;
    function    GetSysDomainUID             :TGUID;
    //procedure   ReloadUserandRights         (const useruid : TFRE_DB_GUID);
    function    GetCurrentUserToken         : IFRE_DB_USER_RIGHT_TOKEN;
  end;



  TFRE_DB_CONTENT_DESC_ARRAY = array of TFRE_DB_CONTENT_DESC;

  IFRE_DB_APPLICATION=interface(IFRE_DB_COMMON)
    ['IFDBAPP']
//    function   GetDescription               (conn : IFRE_DB_CONNECTION): IFRE_DB_TEXT;
    function   UID                          : TGUID;
    function   ShowInApplicationChooser     (const session:IFRE_DB_UserSession): Boolean;
    function   FetchAppTextShort            (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):TFRE_DB_String;
    function   FetchAppTextLong             (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):TFRE_DB_String;
    function   FetchAppTextHint             (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):TFRE_DB_String;
    function   FetchAppTextFull             (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):IFRE_DB_TEXT;
    function   AppClassName                 : ShortString;
    function   AsObject                     : IFRE_DB_Object;
    function   GetCaption                   (const ses : IFRE_DB_Usersession): TFRE_DB_String;
    function   IsMultiDomainApp             : Boolean;
  end;

  IFRE_DB_APPLICATION_MODULE=interface
    ['IFDBAPPM']
    function   GetImplementorsClass          : TClass;
    function   GetToolbarMenu                (const ses : IFRE_DB_Usersession): TFRE_DB_CONTENT_DESC;
    function   GetDBConnection               (const input:IFRE_DB_Object): IFRE_DB_CONNECTION;
    function   GetDBSessionData              (const input:IFRE_DB_Object): IFRE_DB_Object;
    function   GetDBModuleSessionData        (const input:IFRE_DB_Object): IFRE_DB_Object;
    function   GetModuleTitle                (const ses : IFRE_DB_Usersession): TFRE_DB_String;
    function   GetModuleClassName            : Shortstring;
    function   GetSitemapIconFilename        : TFRE_DB_String;
    function   AsObject                      : IFRE_DB_Object;
  end;

  { TFOS_BASE }

  TFOS_BASE   = class(TObject)
  public
    constructor Create               ; virtual;
    function    Implementor          : TObject;virtual;
    function    Implementor_HC       : TObject;virtual;
    function    GetImplementorsClass : TClass;
  end;

  { TFRE_DB_Base }
  {$M+}
  TFRE_DB_Base              = class(TFOS_Base)
  private
    TAGRef                  : TObject;
    class procedure  _InstallDBObjects4Domain   (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID : TGUID);
  protected
    FMediatorExtention        : TFRE_DB_ObjectEx;
    function  Implementor_HC  : TObject;override;
    function  Debug_ID        : TFRE_DB_String;virtual;
  public
    class procedure  VersionInstallationCheck   (const currentVersionId,newVersionId: TFRE_DB_NameType);
    class procedure  RegisterSystemScheme       (const scheme:IFRE_DB_SCHEMEOBJECT); virtual;

    class procedure  InstallDBObjects           (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); virtual;
    class procedure  InstallDBObjects4Domain    (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID : TGUID); virtual;
    class procedure  InstallDBObjects4SysDomain (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID : TGUID); virtual;

    class procedure  InstallUserDBObjects          (const conn:IFRE_DB_CONNECTION; currentVersionId: TFRE_DB_NameType); virtual;
    class procedure  InstallUserDBObjects4Domain   (const conn:IFRE_DB_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID : TGUID); virtual;
    class procedure  InstallUserDBObjects4SysDomain(const conn:IFRE_DB_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID : TGUID); virtual;

    class procedure  RemoveDBObjects            (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType); virtual;
    class procedure  RemoveDBObjects4Domain     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID : TGUID); virtual;

    function         GetSystemSchemeByName      (const schemename:TFRE_DB_String; var scheme: IFRE_DB_SchemeObject): Boolean;
    function         GetSystemScheme            (const schemename:TClass; var scheme: IFRE_DB_SchemeObject): Boolean;
    procedure        GetSession                 (const input: IFRE_DB_Object; out session: TFRE_DB_UserSession; const no_error_on_no_session: boolean); deprecated; //DEPRECATED DONT USE
    function         GetSession                 (const input: IFRE_DB_Object):TFRE_DB_UserSession; deprecated; //DEPRECATED DONT USE

    class function   GetStdObjectRightPart      (const std_right: TFRE_DB_STANDARD_RIGHT):TFRE_DB_String;
    class function   _GetClassRight             (const right: TFRE_DB_NameType): IFRE_DB_RIGHT;
    class function   GetRight4Domain            (const right: TFRE_DB_NameType; const domainUID:TGUID): IFRE_DB_RIGHT;
    class function   GetClassRightName          (const rclassname:ShortString ; const right: TFRE_DB_NameType): TFRE_DB_String;
    class function   GetClassRightName          (const right: TFRE_DB_NameType): TFRE_DB_String;
    class function   GetClassRightNameSR        (const rclassname:ShortString ; const sright: TFRE_DB_STANDARD_RIGHT): TFRE_DB_String;
    class function   GetClassRightNameUpdate    : TFRE_DB_String;
    class function   GetClassRightNameDelete    : TFRE_DB_String;
    class function   GetClassRightNameStore     : TFRE_DB_String;
    class function   GetClassRightNameFetch     : TFRE_DB_String;
    class function   CreateClassRole            (const rolename: TFRE_DB_String; const short_desc, long_desc: TFRE_DB_String): IFRE_DB_ROLE;
    class function   GetClassRoleName           (const rolename: TFRE_DB_String): TFRE_DB_String;
    class function   GetClassRoleNameUpdate     : TFRE_DB_String;
    class function   GetClassRoleNameDelete     : TFRE_DB_String;
    class function   GetClassRoleNameStore      : TFRE_DB_String;
    class function   GetClassRoleNameFetch      : TFRE_DB_String;
    class function   GetClassStdRoles           (const store:boolean=true; const update:boolean=true; const delete:boolean=true; const fetch:boolean=true): TFRE_DB_StringArray;

    procedure        __SetMediator              (const med : TFRE_DB_ObjectEx);
    class function   Get_DBI_InstanceMethods                                            : TFRE_DB_StringArray;
    class function   Get_DBI_RemoteMethods                                              : TFRE_DB_StringArray; //Feeder Instance Methods (Singletons / Usescase : Remote Control Interfaces)
    class function   Get_DBI_ClassMethods                                               : TFRE_DB_StringArray;
    class function   ClassMethodExists   (const name:Shortstring)                       : Boolean; // new
    class function   Invoke_DBIMC_Method (const name:TFRE_DB_String;const input:IFRE_DB_Object;const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION) : IFRE_DB_Object;
    function         Invoke_DBIMI_Method (const name:TFRE_DB_String;const input:IFRE_DB_Object;const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION) : IFRE_DB_Object;
    procedure        Invoke_DBREM_Method (const rmethodname : TFRE_DB_NameType ; const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE);
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

  TFRE_DB_DOMAIN   = class(TFRE_DB_BASE) { Fake Class - only user Classname and Classfunction }
  end;

  TFRE_DB_ROLE     = class(TFRE_DB_BASE) { Fake Class - only user Classname and Classfunction }
  end;

  TFRE_DB_GROUP    = class(TFRE_DB_BASE) { Fake Class - only user Classname and Classfunction }
  end;

  TFRE_DB_USER    = class(TFRE_DB_BASE)  { Fake Class - only user Classname and Classfunction }
  end;


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
    procedure       _InternalSetMediatorScheme         (const mediator : TFRE_DB_ObjectEx ; const scheme : IFRE_DB_SCHEMEOBJECT);
    function        _InternalDecodeAsField             : IFRE_DB_Field; { create a streaming only lightweight field from the encoding object }
  public
    function       GetDescriptionID                   : String;
    class procedure RegisterSystemScheme               (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    function       Invoke                              (const method:TFRE_DB_String;const input:IFRE_DB_Object ; const ses : IFRE_DB_Usersession ; const app : IFRE_DB_APPLICATION ; const conn : IFRE_DB_CONNECTION):IFRE_DB_Object;virtual;
    constructor    create                              ;
    constructor    CreateBound                         (const dbo:IFRE_DB_Object ; const internal_setup : boolean);
    destructor     Destroy                             ;override;
    destructor     DestroyFromBackingDBO               ;
    procedure      FreeFromBackingDBO                  ;virtual;
    procedure      Free                                ;

    //Interface - Compatibility Block

    {if it's a named object support this }
    function        GetDesc         : IFRE_DB_TEXT;   {}
    procedure       SetDesc         (const AValue: IFRE_DB_TEXT);
    function        GetName         : TFRE_DB_String;
    procedure       SetName         (const AValue: TFRE_DB_String);
    property        ObjectName      : TFRE_DB_String       read GetName write SetName;
    property        Description     : IFRE_DB_TEXT read GetDesc write SetDesc;

    function        UIDP                               : PByte;
    function        PUID                               : PGuid;
    function        ObjectRoot                         : IFRE_DB_Object; // = the last parent with no parent
    procedure       ForAllFields                       (const iter:IFRE_DB_FieldIterator);
    procedure       ForAllFieldsBreak                  (const iter:IFRE_DB_FieldIteratorBrk);
    procedure       ForAllObjects                      (const iter:IFRE_DB_Obj_Iterator);
    procedure       ForAllObjectsFieldName             (const iter:IFRE_DB_Obj_NameIterator);
    function        UID                                : TGUID;
    function        UID_String                         : TFRE_DB_GUID_String;
    function        DomainID                           : TGUID;
    procedure       SetDomainID                        (const domid:TGUID);
    function        Parent                             : IFRE_DB_Object;
    function        ParentField                        : IFRE_DB_FIELD;
    function        AsString                           : TFRE_DB_String;
    function        Field                              (const name:TFRE_DB_NameType):IFRE_DB_FIELD;
    function        FieldOnlyExistingObj               (const name:TFRE_DB_NameType):IFRE_DB_Object;
    function        FieldOnlyExistingObject            (const name:TFRE_DB_NameType; var obj:IFRE_DB_Object):boolean;
    function        FieldOnlyExistingObjAs             (const name:TFRE_DB_NameType; const classref : TFRE_DB_BaseClass ; var outobj) : boolean;
    function        FieldOnlyExisting                  (const name:TFRE_DB_NameType;var fld:IFRE_DB_FIELD):boolean;
    function        FieldPath                          (const name:TFRE_DB_String;const dont_raise_ex:boolean=false):IFRE_DB_FIELD;
    function        FieldPathCreate                    (const name:TFRE_DB_String):IFRE_DB_FIELD;
    function        FieldPathExists                    (const name:TFRE_DB_String):Boolean;
    function        FieldPathListFormat                (const field_list:TFRE_DB_NameTypeArray;const formats : TFRE_DB_String;const empty_val: TFRE_DB_String) : TFRE_DB_String;
    function        FieldCount                         (const without_calcfields:boolean): SizeInt;
    function        DeleteField                        (const name:TFRE_DB_String):Boolean;
    procedure       ClearAllFields                     ;
    function        FieldExists                        (const name:TFRE_DB_String):boolean;
    procedure       DumpToStrings                      (const strings:TStrings;indent:integer=0);
    function        DumpToString                       (indent:integer=0;const dump_length_max:Integer=0):TFRE_DB_String;
    function        GetFormattedDisplay                : TFRE_DB_String;
    function        FormattedDisplayAvailable          : boolean;
    function        SubFormattedDisplayAvailable       : boolean;
    function        GetSubFormattedDisplay             (indent:integer=4):TFRE_DB_String;
    function        SchemeClass                        : TFRE_DB_NameType; virtual;
    function        IsA                                (const schemename:shortstring):Boolean;
    function        IsA                                (const IsSchemeclass : TFRE_DB_OBJECTCLASSEX ; var obj ) : Boolean;
    function        PreTransformedWasA                 (const schemename:shortstring):Boolean;
    function        PreTransformedScheme               :ShortString;
    function        IsObjectRoot                       : Boolean;
    procedure       SaveToFile                         (const filename:TFRE_DB_String);
    function        ReferencesObjectsFromData          : Boolean;

    function        ForAllObjectsBreakHierarchic       (const iter:IFRE_DB_ObjectIteratorBrk):boolean; // includes root object (self)
    function        FetchObjByUID                      (const childuid:TGuid ; var obj : IFRE_DB_Object):boolean;
    function        FetchObjWithStringFieldValue       (const field_name: TFRE_DB_NameType; const fieldvalue: TFRE_DB_String; var obj: IFRE_DB_Object; ClassnameToMatch: ShortString=''): boolean;
    procedure       SetAllSimpleObjectFieldsFromObject (const source_object : IFRE_DB_Object); // only first level, no uid, domid, obj, objlink fields

    function        GetFieldListFilter                 (const field_type:TFRE_DB_FIELDTYPE):TFRE_DB_StringArray;
    function        GetUIDPath                         :TFRE_DB_StringArray;
    function        GetUIDPathUA                       :TFRE_DB_GUIDArray;
    function        Implementor                        : TObject;override;
    function        Implementor_HC                     : TObject;override;
    function        Supports                           (const InterfaceSpec:ShortString ; out Intf) : boolean;
    function        Supports                           (const InterfaceSpec:ShortString)            : boolean;
    procedure       IntfCast                           (const InterfaceSpec:ShortString ; out Intf) ; // IntfCast throws an Exception if not succesful
    function        IntfCast                           (const InterfaceSpec:ShortString) : Pointer  ; // IntfCast throws an Exception if not succesful
    function        GetScheme                          (const raise_non_existing:boolean=false): IFRE_DB_SchemeObject;
    procedure       Finalize                           ;
    function        GetAsJSON                          (const without_uid: boolean=false;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil): TJSONData;
    function        GetAsJSONString                    (const without_uid: boolean=false;const full_dump:boolean=false;const stream_cb:TFRE_DB_StreamingCallback=nil): TFRE_DB_String;
    function        CloneToNewObject                   (const generate_new_uids:boolean=false): IFRE_DB_Object;
    function        Mediator                           : TFRE_DB_ObjectEx;
    function        NeededSize                         : TFRE_DB_SIZE_TYPE;
    procedure       Set_ReadOnly                       ;
    procedure       CopyField                          (const obj:IFRE_DB_Object;const field_name:String);
    class function  NewOperation                       (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): TGUID;
    constructor     CreateForDB                        ;
    procedure       CopyToMemory                       (memory : Pointer);
    function        GetInstanceRight                   (const right: TFRE_DB_NameType): IFRE_DB_RIGHT;
    class function  StoreTranslateableText             (const conn: IFRE_DB_SYS_CONNECTION; const key: TFRE_DB_NameType; const short_text:TFRE_DB_String;const long_text:TFRE_DB_String='';const hint_text:TFRE_DB_String=''):TFRE_DB_Errortype;
    class function  DeleteTranslateableText            (const conn: IFRE_DB_SYS_CONNECTION; const key: TFRE_DB_NameType):TFRE_DB_Errortype;
    class function  GetTranslateableTextKey            (const key: TFRE_DB_NameType):TFRE_DB_String;
    class function  GetTranslateableTextShort          (const conn: IFRE_DB_CONNECTION; const key: TFRE_DB_NameType):TFRE_DB_String;
    class function  GetTranslateableTextLong           (const conn: IFRE_DB_CONNECTION; const key: TFRE_DB_NameType):TFRE_DB_String;
    class function  GetTranslateableTextHint           (const conn: IFRE_DB_CONNECTION; const key: TFRE_DB_NameType):TFRE_DB_String;

  published
    function        WEB_SaveOperation                  (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;virtual;
    function        WEB_DeleteOperation                (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;virtual;
    class function  WBC_NewOperation                   (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;virtual;
    function        WEB_NoteLoad                       (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;virtual;
    function        WEB_NoteSave                       (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;virtual;
    function        WEB_NoteStartEdit                  (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;virtual;
    function        WEB_NoteStopEdit                   (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;virtual;
    //Interface - Compatibility Block End
  end;

  { TFRE_DB_WeakObjectEx }

  TFRE_DB_WeakObjectEx=class(TFRE_DB_ObjectEx)
  private
    FWeakClassname : ShortString;
  public
    constructor    Create(const weakclname : Shortstring);
    function       CloneInstance : TFRE_DB_WeakObjectEx;
    function       SchemeClass: TFRE_DB_NameType; override;
  end;

  { TFRE_DB_FILTER_BASE }

  TFRE_DB_FILTER_BASE=class
  protected
    FKey             : TFRE_DB_NameType;
    FFieldname       : TFRE_DB_NameType;
    FNegate          : Boolean;
    FAllowNull       : Boolean;
    FNeedsReEvaluate : Boolean; { the filter must be reevaluated }
    FDBName          : TFRE_DB_NameType;
  public
    function    GetKeyName              : TFRE_DB_NameType;                     { get a reproducable unique key, depending on the filter field, values and settings}
    function    GetDefinitionKey        : TFRE_DB_NameType;virtual;abstract;    { return true if the filter hits }
    function    CheckFilterHit          (const obj : IFRE_DB_Object ; var flt_errors : Int64):boolean;virtual;abstract;
    function    FilterNeedsDbUpdate     : boolean;
    procedure   ReEvaluateFilter        ; virtual ; abstract ;                  { update the filter against db changes (dependency filter type (rl changed) }
    procedure   SetFilterNeedsUpdate    ;
    constructor Create                  (const key : TFRE_DB_NameType);
    function    Clone                   : TFRE_DB_FILTER_BASE;virtual; abstract;
    function    CheckReflinkUpdateEvent (const key_descr : TFRE_DB_NameTypeRL) : boolean; virtual ;{ check if a given qry filter, needs to send updates on RL changes }
  end;


  { TFRE_DB_TRANS_COLL_DATA_BASE }

  TFRE_DB_TRANS_RESULT_BASE = class
  public
    procedure LockBase   ; virtual; abstract;
    procedure UnlockBase ; virtual; abstract;
    function  GetFullKey : TFRE_DB_TRANS_COLL_DATA_KEY; virtual ; abstract;
  end;

  { TFRE_DB_DC_FILTER_DEFINITION_BASE }

  TFRE_DB_DC_FILTER_DEFINITION_BASE = class
    procedure  AddStringFieldFilter    (const key,fieldname:TFRE_DB_NameType ; filtervalue  : TFRE_DB_String              ; const stringfiltertype : TFRE_DB_STR_FILTERTYPE ; const negate:boolean=false ; const include_null_values : boolean=false);virtual;abstract;
    procedure  AddSignedFieldFilter    (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of Int64              ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=false ; const include_null_values : boolean=false);virtual;abstract;
    procedure  AddUnsignedFieldFilter  (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of Uint64             ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=false ; const include_null_values : boolean=false);virtual;abstract;
    procedure  AddCurrencyFieldFilter  (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of Currency           ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=false ; const include_null_values : boolean=false);virtual;abstract;
    procedure  AddReal64FieldFilter    (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of Double             ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=false ; const include_null_values : boolean=false);virtual;abstract;
    procedure  AddDatetimeFieldFilter  (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of TFRE_DB_DateTime64 ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=false ; const include_null_values : boolean=false);virtual;abstract;
    procedure  AddBooleanFieldFilter   (const key,fieldname:TFRE_DB_NameType ; filtervalue  : boolean                                                                       ; const negate:boolean=false ; const include_null_values : boolean=false);virtual;abstract;
    procedure  AddUIDFieldFilter       (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of TFRE_DB_GUID       ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=false ; const include_null_values : boolean=false);virtual;abstract;
    procedure  AddSchemeObjectFilter   (const key:          TFRE_DB_NameType ; filtervalues : Array of TFRE_DB_String                                                       ; const negate:boolean=false);virtual;abstract;
    procedure  AddStdRightObjectFilter (const key:          TFRE_DB_NameType ; stdrightset  : TFRE_DB_STANDARD_RIGHT_SET  ; const usertoken : IFRE_DB_USER_RIGHT_TOKEN      ; const negate:boolean=false);virtual;abstract;
    procedure  AddChildFilter          (const key:          TFRE_DB_NameType); virtual ; abstract;
    procedure  AddParentFilter         (const key:          TFRE_DB_NameType ; const allowed_parent_path : TFRE_DB_GUIDArray); virtual ; abstract ;
    procedure  AddAutoDependencyFilter (const key:          TFRE_DB_NameType ; const RL_Spec : TFRE_DB_NameTypeRLArray ;  const StartDependecyValues : TFRE_DB_GUIDArray ; const negate:boolean=false ; const include_null_values : boolean=false ; const dbname : TFRE_DB_NameType='');virtual; abstract;
    function   RemoveFilter            (const key:          TFRE_DB_NameType):boolean;virtual;abstract;
    function   FilterExists            (const key:          TFRE_DB_NameType):boolean;virtual;abstract;
    procedure  RemoveAllFilters        ;virtual;abstract;
    procedure  RemoveAllFiltersPrefix  (const key_prefix:   TFRE_DB_NameType);virtual;abstract;
  end;

  TFRE_DB_QUERY_BASE=class
    function  GetQueryID         : TFRE_DB_NameType; virtual; abstract;
    procedure SetBaseOrderedData (const basedata   : TFRE_DB_TRANS_RESULT_BASE ; const session_id : TFRE_DB_String);virtual;abstract;
    function  ExecuteQuery       (const iterator   : IFRE_DB_Obj_Iterator):NativeInt;virtual;abstract;
  end;

  TFRE_DB_CHILD_LEVEL_BASE=class
    function  ChildLevelsCount : NativeInt; virtual ; abstract;
    procedure AddChildLevel    (parentpath : String ; const child_uids : TFRE_DB_GUIDArray) ; virtual ; abstract;
  end;

  TFRE_DB_TRANSFORMED_ARRAY_BASE=class
    procedure CleanUp                        ; virtual ; abstract;
    procedure SetTransformedObject           (const tr_obj : IFRE_DB_Object);virtual; abstract;
    procedure SetTransObjectSingleInsert     (const tr_obj : IFRE_DB_Object);virtual; abstract;
    procedure HandleUpdateTransformedObject  (const tr_obj : IFRE_DB_Object; const upd_idx: NativeInt);virtual; abstract;
    procedure HandleInsertTransformedObject  (const tr_obj : IFRE_DB_Object ; const parent_object : IFRE_DB_Object);virtual; abstract;
  end;

  { TFRE_DB_TRANSDATA_MANAGER_BASE }

  TFRE_DB_TRANSDATA_MANAGER_BASE=class
    procedure  UnlockManager             ; virtual; abstract;
    procedure  LockManager               ; virtual; abstract;
    function   GetNewFilterDefinition    : TFRE_DB_DC_FILTER_DEFINITION_BASE; virtual; abstract;
    function   GetTransformedDataLocked  (const query: TFRE_DB_QUERY_BASE ; var cd : TFRE_DB_TRANS_RESULT_BASE):boolean; virtual ; abstract;
    procedure  NewTransformedDataLocked  (const qry: TFRE_DB_QUERY_BASE   ; const dc : IFRE_DB_DERIVED_COLLECTION ; var cd : TFRE_DB_TRANS_RESULT_BASE); virtual ; abstract;
    function   GenerateQueryFromRawInput (const input: IFRE_DB_Object; const dependecy_reference_id: TFRE_DB_StringArray ; const dependency_reference_constraint : TFRE_DB_NameTypeRLArrayArray;
                                          const dependency_negate : boolean ; const parent_child_spec : TFRE_DB_NameTypeRL ; const parent_child_skip_schemes : TFRE_DB_NameTypeRLArray ;
                                          const dc_name,parent_name : TFRE_DB_NameTypeRL ; const dc_static_filters : TFRE_DB_DC_FILTER_DEFINITION_BASE ;
                                          const DefaultOrderField: TFRE_DB_NameType; DefaultOrderAsc: Boolean;
                                          const session : IFRE_DB_UserSession): TFRE_DB_QUERY_BASE; virtual; abstract;
    procedure   StoreQuery               (const qry: TFRE_DB_QUERY_BASE); virtual; abstract;
    procedure   RemoveQuery              (const qry_id: TFRE_DB_NameType); virtual; abstract;
    procedure   DropAllQuerys            (const session: IFRE_DB_UserSession ; const dc_name : TFRE_DB_NameTypeRL); virtual; abstract;
    function    FormQueryID              (const session: IFRE_DB_UserSession ; const dc_name : TFRE_DB_NameTypeRL ; const client_part : shortstring):TFRE_DB_NameType; virtual; abstract;
    procedure   InboundNotificationBlock (const dbname: TFRE_DB_NameType ; const block : IFRE_DB_Object); virtual; abstract;
  end;

  { TFRE_DB_NOTE }

  TFRE_DB_NOTE=class(TFRE_DB_ObjectEx)
  public
  protected
    class procedure  RegisterSystemScheme    (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    class procedure  InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_AUDIT_ENTRY }

  TFRE_DB_AUDIT_ENTRY=class(TFRE_DB_ObjectEx)
  public
  protected
    class procedure  RegisterSystemScheme    (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    class procedure  InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_WORKFLOW_ACTION }

  TFRE_DB_WORKFLOW_ACTION=class(TFRE_DB_ObjectEx)
  protected
    class procedure  RegisterSystemScheme   (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    class procedure  InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_WORKFLOW_STEP }

  TFRE_DB_WORKFLOW_STEP=class(TFRE_DB_ObjectEx)
  private
    function         getAction     : TFRE_DB_GUID;
    function         getIsErrorStep: Boolean;
    procedure        setAction     (AValue: TFRE_DB_GUID);
    procedure        setIsErrorStep(AValue: Boolean);
  protected
    class procedure  RegisterSystemScheme   (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    class procedure  InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    function  WEB_SaveOperation (const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object; override;
  public
    property  isErrorStep       : Boolean      read getIsErrorStep write setIsErrorStep;
    property  action            : TFRE_DB_GUID read getAction      write setAction;
  end;

  { TFRE_DB_UNCONFIGURED_MACHINE }

  TFRE_DB_UNCONFIGURED_MACHINE=class(TFRE_DB_ObjectEx)
  public
  protected
    class procedure  RegisterSystemScheme    (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    class procedure  InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;


  TFRE_DB_APPLICATION_MODULE=class;
  TFRE_DB_APPLICATION_MODULE_ITERATOR = procedure(const module : IFRE_DB_APPLICATION_MODULE;const modul_order:NativeInt) is nested;

  { TFRE_DB_APPLICATION}
  TFRE_DB_APPLICATION = class (TFRE_DB_ObjectEx,IFRE_DB_APPLICATION) // Requires Named Object as Implementor
  private
    function   GetSitemapMainiconFilename    : string;
    function   GetSitemapMainiconSubpath     : string;
    procedure  SetDescTranslationKey         (const AValue: TFRE_DB_String);
    function   GetDescTranslationKey         : TFRE_DB_String;
    procedure  SessionInitialize             (const session : TFRE_DB_UserSession);virtual;
    procedure  SessionFinalize               (const session : TFRE_DB_UserSession);virtual;
    procedure  SessionPromotion              (const session : TFRE_DB_UserSession);virtual;
    procedure  SetSitemapMainiconFilename    (AValue: string);
    procedure  SetSitemapMainiconSubpath     (AValue: string);
  protected
    class procedure  InstallDBObjects        (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    function   IsContentUpdateVisible        (const session : IFRE_DB_UserSession; const update_content_id:string):Boolean;
    procedure  InternalSetup                 ; override;
    procedure  SetupApplicationStructure     ; virtual;

    procedure  AddApplicationModule          (const module:TFRE_DB_APPLICATION_MODULE ; const sitemap_key : string='' ; const icon_path : string='');
    function   DelegateInvoke                (const modulename:TFRE_DB_String;const methname:string;const input : IFRE_DB_Object):IFRE_DB_Object;
    function   IFRE_DB_APPLICATION.ObjectName = ObjectNameI;
    procedure  MySessionInitialize           (const session: TFRE_DB_UserSession); virtual;
    procedure  MySessionPromotion            (const session: TFRE_DB_UserSession); virtual;
    procedure  MySessionFinalize             (const session: TFRE_DB_UserSession); virtual;
    procedure  MyServerInitialize            (const admin_dbc : IFRE_DB_CONNECTION);virtual;
    procedure  MyUpdateSitemap               (const session: TFRE_DB_UserSession);virtual;
    procedure  MyServerFinalize              ;
  public
    procedure  ForAllAppModules              (const module_iterator:TFRE_DB_APPLICATION_MODULE_ITERATOR);

    procedure  ServerInitialize              (const admin_dbc : IFRE_DB_CONNECTION);
    procedure  ServerFinalize                (const admin_dbc : IFRE_DB_CONNECTION); // NOT IMPLEMENTED


    class procedure RegisterSystemScheme     (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure  Finalize                     ;

    function   GetSessionData               (const input:IFRE_DB_Object):IFRE_DB_Object; //global

    procedure  InitAppDesc                  (const descr_translation_key:TFRE_DB_String);virtual;

    function   AsObject                     : IFRE_DB_Object;
    function   AppClassName                 : ShortString;
    function   IsMultiDomainApp             : Boolean; virtual;
    function   GetCaption                   (const ses : IFRE_DB_Usersession): TFRE_DB_String;

    procedure   AddAppToSiteMap             (const session:TFRE_DB_UserSession ; const parent_entry : TFRE_DB_CONTENT_DESC);
    function    ShowInApplicationChooser    (const session:IFRE_DB_UserSession): Boolean;virtual;

    function   _FetchAppText                 (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):IFRE_DB_TEXT;// FINALIZE THE OBJECT
    function   FetchAppTextShort             (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):TFRE_DB_String;
    function   FetchAppTextLong              (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):TFRE_DB_String;
    function   FetchAppTextHint              (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):TFRE_DB_String;
    function   FetchAppTextFull              (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):IFRE_DB_TEXT;// FINALIZE THE OBJECT

    class procedure  CreateAppText          (const conn: IFRE_DB_SYS_CONNECTION;const translation_key:TFRE_DB_String;const short_text:TFRE_DB_String;const long_text:TFRE_DB_String='';const hint_text:TFRE_DB_String='');
    class procedure  DeleteAppText          (const conn: IFRE_DB_SYS_CONNECTION;const translation_key:TFRE_DB_String);
    class procedure  UpdateAppText          (const conn: IFRE_DB_SYS_CONNECTION;const translation_key:TFRE_DB_String;const short_text:TFRE_DB_String;const long_text:TFRE_DB_String='';const hint_text:TFRE_DB_String='');

    class function   StdSidebarCaptionKey   : TFRE_DB_String;
    class function   StdSitemapCaptionKey   : TFRE_DB_String;
    property   SiteMapIconSubPath           : string read GetSitemapMainiconSubpath write SetSitemapMainiconSubpath;
    property   SiteMapMainIconFilename      : string read GetSitemapMainiconFilename write SetSitemapMainiconFilename;

  published
     function   WEB_Content                 (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;virtual;
     function   WEB_OnUIChange              (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;virtual;
  end;

    { TFRE_DB_APPLICATION_MODULE }

  TFRE_DB_APPLICATION_MODULE = class (TFRE_DB_ObjectEx,IFRE_DB_APPLICATION_MODULE)
  private
    procedure  InternalSetup                 ; override;
    procedure  SetSitemapIconFilename        (AValue: TFRE_DB_String);
    function   GetSitemapIconFilename        : TFRE_DB_String;
  protected
    procedure  SetupAppModuleStructure       ;virtual;
    procedure  AddApplicationModule          (const module:TFRE_DB_APPLICATION_MODULE);
    procedure  InitModuleDesc                (const descr_translation_key:TFRE_DB_String);virtual; deprecated ; { use the std translation keys }

    function   IFRE_DB_APPLICATION_MODULE.ObjectName  = GetName;
    function   IFRE_DB_APPLICATION_MODULE.DEscription = GetDesc;
    procedure  MyServerInitializeModule      (const admin_dbc : IFRE_DB_CONNECTION); virtual;
    procedure  CheckClassVisibility4AnyDomain(const session : IFRE_DB_UserSession);virtual;
    procedure  CheckClassVisibility4MyDomain (const session : IFRE_DB_UserSession);virtual;
    class procedure  InstallDBObjects        (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    class function  StdSitemapModuleTitleKey : TFRE_DB_String;
    class procedure RegisterSystemScheme     (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure  ForAllAppModules              (const module_iterator:TFRE_DB_APPLICATION_MODULE_ITERATOR);
    procedure  MySessionInitializeModule     (const session : TFRE_DB_UserSession);virtual;
    procedure  MySessionPromotionModule      (const session: TFRE_DB_UserSession); virtual;
    procedure  MySessionFinalizeModule       (const session : TFRE_DB_UserSession);virtual;

    function   GetEmbeddingApp               : TFRE_DB_APPLICATION;
    function   _FetchAppText                 (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):IFRE_DB_TEXT;

    function   GetToolbarMenu                (const ses : IFRE_DB_Usersession): TFRE_DB_CONTENT_DESC; virtual;
    function   GetDBConnection               (const input:IFRE_DB_Object): IFRE_DB_CONNECTION;
    function   GetDBSessionData              (const input:IFRE_DB_Object): IFRE_DB_Object;
    function   GetDBModuleSessionData        (const input:IFRE_DB_Object): IFRE_DB_Object;
    function   GetDependencyFiltervalues     (const input:IFRE_DB_Object; const dependencyfield:string): TFRE_DB_StringArray;

    procedure  SetDescrTranslationKey        (const val:TFRE_DB_String); deprecated;
    function   GetDescrTranslationKey        :TFRE_DB_String; deprecated;
    function   GetModuleTitle                (const ses : IFRE_DB_Usersession): TFRE_DB_String;
    function   GetModuleClassName            : Shortstring;
    function   AsObject                      : IFRE_DB_Object;
    function   IsContentUpdateVisible        (const session: IFRE_DB_UserSession; const update_content_id:string):Boolean;

    function   _FetchModuleText              (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):IFRE_DB_TEXT;// FINALIZE THE OBJECT
    function   FetchModuleTextShort          (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):TFRE_DB_String;
    function   FetchModuleTextLong           (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):TFRE_DB_String;
    function   FetchModuleTextHint           (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):TFRE_DB_String;
    function   FetchModuleTextFull           (const session:IFRE_DB_UserSession;const translation_key:TFRE_DB_String):IFRE_DB_TEXT;// FINALIZE THE OBJECT

    class procedure  CreateModuleText        (const conn: IFRE_DB_SYS_CONNECTION;const translation_key:TFRE_DB_String;const short_text:TFRE_DB_String;const long_text:TFRE_DB_String='';const hint_text:TFRE_DB_String='');
    class procedure  DeleteModuleText        (const conn: IFRE_DB_SYS_CONNECTION;const translation_key:TFRE_DB_String);
    class procedure  UpdateModuleText        (const conn: IFRE_DB_SYS_CONNECTION;const translation_key:TFRE_DB_String;const short_text:TFRE_DB_String;const long_text:TFRE_DB_String='';const hint_text:TFRE_DB_String='');
    property         SitemapIconFilename     :TFRE_DB_String read GetsitemapIconFilename write SetSitemapIconFilename;
  published
    function   WEB_Content                   (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;virtual;
    function   WEB_OnUIChange                (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;virtual;
  end;

  // Describes a basic content Element
  // The content has an contentID and uses as Internal representation a (temporary/non saved) DBO

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
    // An id to indentify the content
    property  contentId    : TFRE_DB_String read GetContentId write SetContentId;
    // This content should update an content with contentID (don't forget to set a new(the same) contentID again! (updateID is only for special cases, as if updateID is unset contentID is used in update case)
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
     procedure        Free                     ;
     destructor       Destroy                  ; override;
     destructor       DestroySingleton         ;
     procedure        FreeFromBackingDBO       ; override;
  end;

  //@ This suppresses the sync answer to the client, a Sync answer should be sent in time by another method

  { TFRE_DB_SUPPRESS_ANSWER_DESC }

  TFRE_DB_SUPPRESS_ANSWER_DESC=class(TFRE_DB_ObjectEx)
  public class
    var NILInstances : NativeInt;
    constructor      Create                   ;
    procedure        Free                     ;
    destructor       Destroy                  ;override;
    destructor       DestroySingleton         ;
    procedure        FreeFromBackingDBO       ; override;
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
    function InternalInvoke (const session: TFRE_DB_UserSession): IFRE_DB_Object;
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
    //@ For wait dialogs an abort button will be displayed if a server function is given and a progressBar will be displayed if the progressBarId is specified.
    function  Describe (const caption,msg: String;const msgType:TFRE_DB_MESSAGE_TYPE;const serverFunc:TFRE_DB_SERVER_FUNC_DESC=nil;const progressBarId: String=''): TFRE_DB_MESSAGE_DESC;
  end;

  { TFRE_DB_OPEN_NEW_LOCATION_DESC }

  TFRE_DB_OPEN_NEW_LOCATION_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a new browser window.
    function Describe        (const url: String; const inNewWindow: Boolean=true): TFRE_DB_OPEN_NEW_LOCATION_DESC;
  end;


  { TFRE_DB_UPDATE_MESSAGE_PROGRESS_DESC }

  TFRE_DB_UPDATE_MESSAGE_PROGRESS_DESC = class(TFRE_DB_CONTENT_DESC)
    //@ Describes an update of a progress indicator within a message dialog.
    function  Describe (const progressBarId: String; const percentage: Real): TFRE_DB_UPDATE_MESSAGE_PROGRESS_DESC;
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

  TFRE_DB_MENU_DESC = class;

  { TFRE_DB_SECTION_DESC }

  TFRE_DB_SECTION_DESC = class(TFRE_DB_CONTENT_DESC)
  private
    function  _Describe             (const title :String; const ord:Int16; const sectionId:String; const size:Integer): TFRE_DB_SECTION_DESC;
  public
    //@ Describes a single section of the subsections description.
    //@ The contentFunc is used to get the content description of the section.
    //@ The ord parameter can be used to sort the sections.
    function  Describe             (const contentFunc:TFRE_DB_SERVER_FUNC_DESC;const title :String; const ord:Int16; const sectionId:String=''; const size:Integer=-1): TFRE_DB_SECTION_DESC;
    //@ Used by the framework.
    //@ DO NO USE.
    function  _internalDescribe  (const content:TFRE_DB_CONTENT_DESC;const title :String; const ord:Int16; const sectionId:String=''; const size:Integer=-1): TFRE_DB_SECTION_DESC;
    //@ Sets the section as the active one.
    //@ Only usefull in case of display type sec_dt_tab
    procedure SetActive            (const active: Boolean);
    //@ Sets the menu of the section. Will be displayed like a file menu in a desktop application.
    procedure SetMenu              (const menu: TFRE_DB_MENU_DESC);
  end;

  { TFRE_DB_SUBSECTIONS_DESC }

  TFRE_DB_SUBSECTIONS_DESC = class(TFRE_DB_CONTENT_DESC)
  private
    fnoActiveSectionSet: Boolean;
    factiveSectionOrd  : Integer;
    procedure SetActiveSectionUID (const sectionUID: String);
    procedure SectionDescribed    (const sectionUID: String; const ord,size: Integer);
  public
    constructor Create     ;
    //@ Describes a collection of content which is displayed either as tabs or vertical arranged with resize handlers.
    function  Describe        (const displayType: TFRE_DB_SUBSEC_DISPLAY_TYPE=sec_dt_tab): TFRE_DB_SUBSECTIONS_DESC;
    //@ The serverFunction will be called on an ui change. In this case tab change if the display type is sec_dt_tab.
    //@ FIXXME - finish implementation.
    procedure OnUIChange      (const serverFunc: TFRE_DB_SERVER_FUNC_DESC);
    //@ Creates a new section and adds it.
    //@ The size parameter is only useful for the display type sec_dt_vertical.
    function  AddSection       : TFRE_DB_SECTION_DESC;
    //@ Set the section with the given id as active section.
    procedure SetActiveSection (const sectionId: String);
  end;

  { TFRE_DB_MENU_ENTRY_DESC }

  TFRE_DB_MENU_ENTRY_DESC    = class(TFRE_DB_CONTENT_DESC)
  private
    function  _Describe  (const caption,icon:String; const disabled:Boolean):TFRE_DB_MENU_ENTRY_DESC;
  public
    //@ Describes a menu entry. See also TFRE_DB_MENU_DESC and TFRE_DB_SUBMENU_DESC.
    //@ After the execution of the server function the defined refresh is executed. E.g. an add operation on a grid will define fdbrt_direct to refresh the grid.
    //@ fdbrt_dependent refreshes all filtered stores. See TFRE_DB_VIEW_LIST_DESC.addFilterEvent.
    //@ Only implemented for grids and trees.
    function  Describe          (const caption,icon:String; const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const disabled:Boolean=false; const id:String=''):TFRE_DB_MENU_ENTRY_DESC;
    function  DescribeDownload  (const caption,icon:String; const downloadId: String; const disabled:Boolean=false):TFRE_DB_MENU_ENTRY_DESC;
  end;

  TFRE_DB_SUBMENU_DESC = class;
  { TFRE_DB_MENU_DESC }

  TFRE_DB_MENU_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a menu.
    function  Describe   : TFRE_DB_MENU_DESC;
    //@ Creates a new menu entry description and adds it.
    function  AddEntry   : TFRE_DB_MENU_ENTRY_DESC;
    //@ Creates a new sub menu description and adds it.
    function  AddMenu    : TFRE_DB_SUBMENU_DESC;
  end;

  { TFRE_DB_SUBMENU }

  TFRE_DB_SUBMENU_DESC    = class(TFRE_DB_MENU_DESC)
  public
    //@ Describes a sub menu. See TFRE_DB_MENU_DESC.
    function  Describe  (const caption,icon: String; const disabled:Boolean=false; const id:String=''):TFRE_DB_SUBMENU_DESC;
  end;

  { TFRE_DB_RESTORE_UI_DESC }

  TFRE_DB_RESTORE_UI_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a client side function which should be called.
    //@ baseContainerId: Start point of the sectionIds path. Can be a section description or a layout description or 'FirmOSViewport' to start at the top.
    function  Describe       (const baseContainerId: String; const sectionIds: TFRE_DB_StringArray): TFRE_DB_RESTORE_UI_DESC;
  end;

  { TFRE_DB_UPDATE_FORM_DESC }

  TFRE_DB_UPDATE_FORM_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes an update of a form dbo. All Forms including the given updateObj will be updated.
    //@ updateObj has to hold all changes (added, modified and deleted fields (delta object))
    function  DescribeDBO      (const updateObj:IFRE_DB_Object):TFRE_DB_UPDATE_FORM_DESC;
    //@ Describes an update of a form. The form with the given id will be updated be the given object.
    function  Describe         (const formId: String; const updateObj:IFRE_DB_Object):TFRE_DB_UPDATE_FORM_DESC;
  end;

  { TFRE_DB_SITEMAP_ENTRY_DESC }

  TFRE_DB_SITEMAP_ENTRY_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a sitemap entry.
    //@ Top level entries are arranged automatically. Therefore parameters x and y are ignored for top level entries.
    function  Describe  (const caption,icon:String; const restoreUIFunc: TFRE_DB_RESTORE_UI_DESC; const x,y:Integer; const id:String=''; const newsCount:Integer=0;  const disabled:Boolean=false; const scale:Single=1): TFRE_DB_SITEMAP_ENTRY_DESC;
    //@ Creates a new sitemap entry description and adds it.
    function  AddEntry  : TFRE_DB_SITEMAP_ENTRY_DESC;
  end;

  { TFRE_DB_SVG_DEF_ELEM_ATTR_DESC }

  TFRE_DB_SVG_DEF_ELEM_ATTR_DESC    = class(TFRE_DB_CONTENT_DESC)
    //@ Describes an attribute of a SVG definitions element.
    function  Describe  (const name,value:String): TFRE_DB_SVG_DEF_ELEM_ATTR_DESC;
  end;

  { TFRE_DB_SVG_DEF_ELEM_DESC }

  TFRE_DB_SVG_DEF_ELEM_DESC    = class(TFRE_DB_CONTENT_DESC)
    //@ Describes a SVG definitions element. E.g. linearGradient.
    function  Describe    (const tagName:String): TFRE_DB_SVG_DEF_ELEM_DESC;
    //@ Adds a sub element. E.g. a 'stop' element to a 'linearGradiant'
    function AddElement   :TFRE_DB_SVG_DEF_ELEM_DESC;
    //@ Adds an attribute.
    function AddAttribute :TFRE_DB_SVG_DEF_ELEM_ATTR_DESC;
  end;
  TFRE_DB_SVG_DEF_ELEM_DESC_ARRAY = array of TFRE_DB_SVG_DEF_ELEM_DESC;

  { TFRE_DB_SITEMAP_DESC }

  TFRE_DB_SITEMAP_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a sitemap/structure of the application.
    function  Describe  (const svgDefs: TFRE_DB_SVG_DEF_ELEM_DESC_ARRAY=nil): TFRE_DB_SITEMAP_DESC;
    //@ Creates a new sitemap entry description and adds it.
    function  AddEntry  : TFRE_DB_SITEMAP_ENTRY_DESC;
  end;

  TFRE_DB_SERVER_FUNC_DESC_ARRAY   = Array of TFRE_DB_SERVER_FUNC_DESC;

  TFRE_DB_DESCRIPTION_CLASS  = class of TFRE_DB_CONTENT_DESC;

  IFRE_DB=interface
    procedure   GenerateAnObjChangeList         (const first_obj, second_obj: IFRE_DB_Object ; const InsertCB,DeleteCB : IFRE_DB_Obj_Iterator ; const UpdateCB : IFRE_DB_UpdateChange_Iterator);
    function    NewDBCommand                    : IFRE_DB_COMMAND;
    function    FetchApplications               (var apps : IFRE_DB_APPLICATION_ARRAY):TFRE_DB_Errortype;
    function    GetFormatSettings               : TFormatSettings;
    function    GetLocalZone                    : TFRE_DB_String;
    procedure   SetFormatSettings               (const AValue: TFormatSettings);
    procedure   SetLocalZone                    (const AValue: TFRE_DB_String);
    function    LocalTimeToUTCDB64              (const ADateTime64: TFRE_DB_DateTime64) : TFRE_DB_DateTime64;
    function    UTCToLocalTimeDB64              (const ADateTime64: TFRE_DB_DateTime64) : TFRE_DB_DateTime64;

    function    NewText                         (const key,txt,txt_short:TFRE_DB_String;const hint:TFRE_DB_String=''):IFRE_DB_TEXT;
    function    NewRole                         (const rolename,txt,txt_short:TFRE_DB_String;const is_internal:Boolean=false):IFRE_DB_ROLE;
    function    NewGroup                        (const groupname,txt,txt_short:TFRE_DB_String;const is_protected:Boolean=false;const is_internal:Boolean=false):IFRE_DB_GROUP;
    function    NewClientFieldValidator         (const name: TFRE_DB_String) : IFRE_DB_ClientFieldValidator;
    function    NewEnum                         (const name: TFRE_DB_String) : IFRE_DB_Enum;
    function    RegisterSysClientFieldValidator (const val : IFRE_DB_ClientFieldValidator):TFRE_DB_Errortype;
    function    RegisterSysEnum                 (const enu : IFRE_DB_Enum):TFRE_DB_Errortype;

    function    GetSystemSchemeByName           (const schemename:TFRE_DB_NameType; var scheme: IFRE_DB_SchemeObject): Boolean;
    function    GetSystemScheme                 (const schemename:TClass; var scheme: IFRE_DB_SchemeObject): Boolean;
    function    GetSystemEnum                   (const name:TFRE_DB_NameType ; out enum : IFRE_DB_Enum):boolean;
    function    GetSystemClientFieldValidator   (const name:TFRE_DB_NameType ; out clf  : IFRE_DB_ClientFieldValidator):boolean;


    function    NewObjectIntf          (const InterfaceSpec:ShortString;out Intf;const mediator : TFRE_DB_ObjectEx=nil;const fail_on_non_existent:boolean=true) : Boolean;
    function    NewObject              : IFRE_DB_Object;
    function    NewNamedObject         : IFRE_DB_NAMED_OBJECT;
    function    NewObjectScheme        (const Scheme : TClass): IFRE_DB_Object;
    function    NewObjectSchemeByName  (const Scheme : TFRE_DB_NameType): IFRE_DB_Object;
    function    AnonObject2Interface   (const Data   : Pointer):IFRE_DB_Object;


    function    CreateFromFile         (const filename:TFRE_DB_String):IFRE_DB_Object;
    function    CreateFromMemory       (memory : Pointer):IFRE_DB_Object;
    function    CreateFromString       (const AValue:TFRE_DB_String):IFRE_DB_Object;

    function    NewConnection          (const direct : boolean = true): IFRE_DB_CONNECTION;
    function    NewSysOnlyConnection   : IFRE_DB_SYS_CONNECTION; // always direct=embedded
    function    DatabaseList           (const user:TFRE_DB_String='';const pass:TFRE_DB_String=''): IFOS_STRINGS;

    procedure   DBInitializeAllExClasses     (const conn:IFRE_DB_CONNECTION);
    procedure   DBInitializeAllSystemClasses (const conn:IFRE_DB_CONNECTION); // not impemented by now (no initializable sys classes, keep count low)

    function    StringArray2String     (const A:TFRE_DB_StringArray):TFRE_DB_String;
    function    GuidArray2SString      (const A:TFRE_DB_GUIDArray):TFRE_DB_String;
    function    StringArray2GuidArray  (const A:TFRE_DB_StringArray):TFRE_DB_GUIDArray;
    function    CountedObjLinks2String (const A:TFRE_DB_CountedGuidArray):TFRE_DB_String;


    function    CreateText             (const translation_key:TFRE_DB_String;const short_text:TFRE_DB_String;const long_text:TFRE_DB_String='';const hint_text:TFRE_DB_String=''):IFRE_DB_TEXT;

    function    JSONObject2Object      (const json_string:string):IFRE_DB_Object;


    procedure   RegisterObjectClassEx   (const ExtensionObject : TFRE_DB_OBJECTCLASSEX);
    procedure   Initialize_Extension_Objects;

    procedure   LogDebug               (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogInfo                (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogEmergency           (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogWarning             (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogError               (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogNotice              (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String;const param:array of const);
    procedure   LogDebug               (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogInfo                (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogEmergency           (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogWarning             (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogError               (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);
    procedure   LogNotice              (const category:TFRE_DB_LOGCATEGORY;const msg:TFRE_DB_String);

    procedure   ClearGUID              (var uid:TGUID);
    function    Get_A_Guid             : TGUID;
    function    Get_A_Guid_HEX         : Ansistring;
    property    LocalZone              : TFRE_DB_String read GetLocalZone write SetLocalZone;
    property    StringFormatSettings   : TFormatSettings read GetFormatSettings write SetFormatSettings;
    function    NetServ                : IFRE_DB_NetServer;
  end;

  { IFRE_DB_InputGroupSchemeDefinition }

  IFRE_DB_InputGroupSchemeDefinition=interface
    function  GetCaptionKey      : TFRE_DB_NameType;
    //function  GetInputGroupID    : TFRE_DB_String;
    //procedure SetCaptionKey      (AValue: TFRE_DB_String);
    //procedure SetIGFields        (AValue: IFRE_DB_ObjectArray);
    //procedure SetInputGroupID    (AValue: TFRE_DB_String);
    function  Setup              (const caption: TFRE_DB_String):IFRE_DB_InputGroupSchemeDefinition;
    function  GetParentScheme    : IFRE_DB_SchemeObject;
    procedure AddInput           (const schemefield: TFRE_DB_String; const cap_trans_key: TFRE_DB_String=''; const disabled: Boolean=false;const hidden:Boolean=false; const field_backing_collection: TFRE_DB_String='';const fbCollectionIsDomainCollection:boolean=false;const chooser_type:TFRE_DB_CHOOSER_DH=dh_chooser_combo; const standard_coll: TFRE_DB_STANDARD_COLL=coll_NONE; const chooserAddEmptyForRequired: Boolean=false);
    procedure AddDomainChooser   (const schemefield: TFRE_DB_String; const std_right:TFRE_DB_STANDARD_RIGHT; const rightClasstype: TClass; const hideSingle: Boolean; const cap_trans_key: TFRE_DB_String='');
    procedure UseInputGroup      (const scheme,group: TFRE_DB_String; const addPrefix: TFRE_DB_String='';const as_gui_subgroup:boolean=false ; const collapsible:Boolean=false;const collapsed:Boolean=false);
    property  CaptionKey         : TFRE_DB_NameType read GetCaptionKey;
    function  GroupFields        : PFRE_InputFieldDef4GroupArr;
  end;

  TFRE_DB_OnCheckUserNamePassword     = function  (username,pass:TFRE_DB_String) : TFRE_DB_Errortype of object;
  TFRE_DB_OnGetImpersonatedConnection = function  (const db,username,pass:TFRE_DB_String;out conn : IFRE_DB_CONNECTION):TFRE_DB_Errortype of object;
  TFRE_DB_OnRestoreDefaultConnection  = function  (out   username:TFRE_DB_String;out conn : IFRE_DB_CONNECTION):TFRE_DB_Errortype of object;
  TFRE_DB_OnExistsUserSessionForKey   = function  (const key:string;out other_session:TFRE_DB_UserSession):boolean of object;
  TFRE_DB_OnFetchPublisherSession     = function  (const rcall,rmeth:TFRE_DB_NameType;out ses:TFRE_DB_UserSession ; out right:TFRE_DB_String):boolean of object;
  TFRE_DB_OnFetchSessionByID          = function  (const sessionid : TFRE_DB_String ; var session : TFRE_DB_Usersession):boolean of object;
  TFRE_DB_PromoteResult               = (pr_OK,pr_Failed,pr_Takeover,pr_TakeoverPrepared);

  { TFRE_DB_UserSession }
  TFRE_DB_EVENT_METHOD_ENC=class
  public
    method : IFRE_DB_InvokeClassMethod;
    params : IFRE_DB_Object;
  end;

  TFRE_DB_RemoteCB           = procedure(const ses : IFRE_DB_UserSession ; const data : IFRE_DB_Object ; const status : TFRE_DB_COMMAND_STATUS ; const original_command_id : Qword ; const opaquedata : IFRE_DB_Object) is nested;

  { IFRE_DB_UserSession }

  IFRE_DB_UserSession=interface
    function    GetSessionID             : TFRE_DB_String;
    function    GetSessionState          : TFRE_DB_SESSIONSTATE;
    function    GetSessionAppData        (const app_key:TFRE_DB_String):IFRE_DB_Object;
    function    GetSessionModuleData     (const mod_key:TFRE_DB_String):IFRE_DB_Object;
    function    GetSessionGlobalData     :IFRE_DB_Object;
    function    NewDerivedCollection     (dcname:TFRE_DB_NameType):IFRE_DB_DERIVED_COLLECTION; // Session DC
    function    FetchDerivedCollection   (dcname:TFRE_DB_NameType):IFRE_DB_DERIVED_COLLECTION;
    function    GetDBConnection          :IFRE_DB_CONNECTION;
    function    LoggedIn                 : Boolean;
    procedure   Logout                   ;
    function    Promote                  (const user_name,password:TFRE_DB_String;var promotion_status:TFRE_DB_String; force_new_session_data : boolean ; const session_takeover : boolean ; const auto_promote : boolean=false) : TFRE_DB_PromoteResult; // Promote USER to another USER

    procedure   SendServerClientRequest  (const description : TFRE_DB_CONTENT_DESC;const session_id:String='');
    procedure   SendServerClientAnswer   (const description : TFRE_DB_CONTENT_DESC;const answer_id : Qword);

    //Send a Server Client Message in behalv of another session (think about security)
    //  ses.SendDelegatedEventToSession(sessionID,SF,input.CloneToNewObject());

    //Invoke a Method that another Session provides via Register, in the context of that session (feeder session)
    function    InvokeRemoteRequest      (const rclassname, rmethodname: TFRE_DB_NameType; const input: IFRE_DB_Object ; const SyncCallback: TFRE_DB_RemoteCB; const opaquedata: IFRE_DB_Object): TFRE_DB_Errortype;

    function    RegisterTaskMethod       (const TaskMethod:IFRE_DB_WebTimerMethod ; const invocation_interval : integer ; const id  :String='TIMER') : boolean;
    function    RemoveTaskMethod         (const id:string='TIMER'):boolean;

    procedure   ClearUpdatable               ;
    procedure   RegisterUpdatableContent     (const contentId: String);
    procedure   UnregisterUpdatableContent   (const contentId: String);
    procedure   RegisterUpdatableDBO         (const UID_id: TFRE_DB_GUID);
    procedure   UnregisterUpdatableDBO       (const UID_id: TFRE_DB_GUID);
    function    IsUpdatableContentVisible    (const contentId: String): Boolean;
    function    IsDBOUpdatable               (const UID_id: TFRE_DB_GUID):boolean;
    function    GetDownLoadLink4StreamField  (const obj_uid: TGUID; const fieldname: TFRE_DB_NameType; const is_attachment: boolean; mime_type: string; file_name: string ; force_url_etag : string=''): String; { Make a DBO Stream Field Downloadable in a Session context }
    function    GetUserName                  : String;
    function    GetDomain                    : TFRE_DB_String;      { domainname of logged in user }
    function    GetDomainUID                 : TFRE_DB_GUID;        { domain id of logged in user }
    function    GetDomainUID_String          : TFRE_DB_GUID_String; { domain id as string of logged in user }
    procedure   InboundNotificationBlock     (const block: IFRE_DB_Object); { Here comes an Inbound Notification block from the network/pl layer}
  end;

  TFRE_DB_RemoteReqSpec      = record
                                classname       : TFRE_DB_NameType;
                                methodname      : TFRE_DB_NameType;
                                invokationright : TFRE_DB_String;
                             end;

  TFRE_DB_RemoteReqSpecArray = array of TFRE_DB_RemoteReqSpec;


  { TFRE_DB_RemoteSessionInvokeEncapsulation }

  TFRE_DB_RemoteSessionInvokeEncapsulation=class
  private
   Fclassname, Fmethodname : TFRE_DB_NameType;
   Finput                  : IFRE_DB_Object;
   FSyncCallback           : TFRE_DB_RemoteCB;
   Fopaquedata             : IFRE_DB_Object;
   FsessionID              : String;
   FOCid                   : QWord;
  public
    constructor Create(const rclassname, rmethodname: TFRE_DB_NameType ; const ocid : QWord ; const SessionID: String; const input: IFRE_DB_Object ; const SyncCallback: TFRE_DB_RemoteCB; const opaquedata: IFRE_DB_Object);
  end;

  { TFRE_DB_RemoteSessionAnswerEncapsulation }

  TFRE_DB_RemoteSessionAnswerEncapsulation=class
  private
   FData                   : IFRE_DB_Object;
   FStatus                 : TFRE_DB_COMMAND_STATUS;
   Fopaquedata             : IFRE_DB_Object;
   FOCid                   : Qword;
   FContmethod             : TFRE_DB_RemoteCB;
   FSesID                  : TFRE_DB_String;
  public
    constructor Create         (const data : IFRE_DB_Object ; const status : TFRE_DB_COMMAND_STATUS ; const original_command_id : Qword ; const opaquedata : IFRE_DB_Object ; Contmethod : TFRE_DB_RemoteCB ; const SesID : TFRE_DB_String);
    procedure   DispatchAnswer (const ses : IFRE_DB_UserSession);
  end;

  TFRE_DB_Usersession_COR=class
    procedure Execute(const session : TFRE_DB_Usersession);virtual;abstract;
  end;

  TFRE_DB_UC_Transformed_Update=class(TFRE_DB_Usersession_COR)
  public
    Content : TFRE_DB_CONTENT_DESC;
    //procedure Execute(const session: TFRE_DB_Usersession); override;
  end;

  TFRE_DB_UserSession = class(TObject,IFRE_DB_Usersession,IFRE_DB_DBChangedNotificationSession)
  private type
    TDiffFieldUpdateMode=(mode_df_add,mode_df_del,mode_df_change);
    TDispatch_Continuation = record
      CID        : Qword;
      ORIG_CID   : Qword;
      Contmethod : TFRE_DB_RemoteCB;
      ExpiredAt  : TFRE_DB_DateTime64;
      SesID      : TFRE_DB_String;
      opData     : IFRE_DB_Object;
    end;
  private class var
    FContinuationArray    : Array [0..100] of TDispatch_Continuation;
    FContinuationLock     : IFOS_LOCK;
    FMyReqID              : NativeUint;
  private
    FSessionLock          : IFOS_LOCK;
    FTakeoverPrepared     : String;
    FOnFetchSessionByIdL  : TFRE_DB_OnFetchSessionByID;
    FSessionTerminationTO : NativeInt;
    FBoundThreadID        : TThreadID;
 var
    FonExistsSesForTkKeyL : TFRE_DB_OnExistsUserSessionForKey;

    FOnWorkCommands       : TNotifyEvent;
    FUserName             : TFRE_DB_String;
    FuserDomain           : TFRE_DB_Guid;
    FSessionID            : TFRE_DB_String;
    FPassMD5              : TFRE_DB_String;
    FDefaultApp           : TFRE_DB_String;
    FSessionData          : IFRE_DB_Object;
    FBinaryInputs         : IFRE_DB_Object; { per key requestable of Binary Input which get's sent seperated from the data }
    FUpdateableDBOS       : IFRE_DB_Object;
    FDifferentialUpdates  : IFRE_DB_Object;
    FUpdateableContent    : IFRE_DB_Object;

    FTimers               : TList;

    FRemoteRequestSet     : TFRE_DB_RemoteReqSpecArray;
    FCurrentReqID         : QWord; // Current ReqID that is beeing processed (from client)

    FDefaultUID           : TFRE_DB_GUIDArray;
    FDBConnection         : IFRE_DB_CONNECTION;
    FPromoted             : Boolean;

    FAppArray             : Array of IFRE_DB_APPLICATION;
    FDC_Array             : Array of IFRE_DB_DERIVED_COLLECTION;
    FcurrentApp           : TFRE_DB_String;
    FConnDesc             : String;
    FBoundSession_RA_SC   : IFRE_DB_COMMAND_REQUEST_ANSWER_SC;
    FIsInteractive        : Boolean;
    FBindState            : TFRE_DB_SESSIONSTATE;

    procedure     SetOnExistsUserSession4TakeOver(AValue: TFRE_DB_OnExistsUserSessionForKey);
    procedure     SetOnWorkCommands       (AValue: TNotifyEvent);
    procedure     _FixupDCName           (var dcname:TFRE_DB_NameType);
    function      SearchSessionDC        (dc_name:TFRE_DB_String;out dc:IFRE_DB_DERIVED_COLLECTION):boolean;
    procedure     _FetchAppsFromDB       ;
    procedure     _InitApps              ;

    procedure     AddSyncContinuationEntry (const request_id,original_req_id:Qword ; const for_session_id : String ; const callback : TFRE_DB_RemoteCB ; const expire_in : NativeUint ; const opaquedata : IFRE_DB_Object);
    procedure     INT_TimerCallBack      (const timer : IFRE_APSC_TIMER ; const flag1,flag2 : boolean);
    procedure     RemoveAllTimers        ;


  public
    function      FetchStreamDBO_OTCU    (const uid:TGUID ; var end_field : TFRE_DB_NameTypeRL ; var lcontent : TFRE_DB_RawByteString ; var content_type : TFRE_DB_String ; var etag : TFRE_DB_String) : Boolean; // Other Thread Context Unsafe
    class procedure HandleContinuationTimeouts(const onfetch: TFRE_DB_OnFetchSessionByID);
    procedure     LockSession            ;
    procedure     UnlockSession          ;
    class destructor  destroyit          ;
    class constructor createit           ;
    procedure   SetSessionState          (const sstate : TFRE_DB_SESSIONSTATE);
    function    GetSessionState          : TFRE_DB_SESSIONSTATE;
    function    CheckUnboundSessionForPurge : boolean;
    constructor Create                   (const user_name,password:TFRE_DB_String;const default_app:TFRE_DB_String;const default_uid_path : TFRE_DB_GUIDArray ; conn : IFRE_DB_CONNECTION);
    destructor  Destroy                  ;override;
    procedure   StoreSessionData         ;
    function    SearchSessionApp         (const app_key:TFRE_DB_String ; out app:TFRE_DB_APPLICATION ; out idx:integer):boolean;

    procedure   InboundNotificationBlock (const block: IFRE_DB_Object); { Here comes an Inbound Notification block from the network/pl layer}
    procedure   COR_InboundNotifyBlock   (const data : Pointer);

    function    SearchSessionAppUID      (const app_uid:TGUID;out app:IFRE_DB_Object):boolean;
    function    SearchSessionDCUID       (const  dc_uid:TGUID;out dc:IFRE_DB_DERIVED_COLLECTION):boolean;
    procedure   RemSessionAppAndFinialize(const app_key:TFRE_DB_String);
    procedure   RemoveAllAppsAndFinalize ;
    procedure   SetCurrentApp            (const app_key:TFRE_DB_String);
    procedure   Input_FRE_DB_Command     (const cmd :IFRE_DB_COMMAND); // Here Comes the command in ..
    class
     procedure  CLS_ForceInvalidSessionReload (rac :IFRE_DB_COMMAND_REQUEST_ANSWER_SC ; const cmd :IFRE_DB_COMMAND); // Here Comes the command in ..
    function    InternalSessInvokeMethod (const class_name,method_name:string;const uid_path:TFRE_DB_GUIDArray;var input:IFRE_DB_Object):IFRE_DB_Object;
    function    InternalSessInvokeMethod (const app:IFRE_DB_APPLICATION;const method_name:string;const input:IFRE_DB_Object):IFRE_DB_Object;
    //function    CloneSession             (const connectiond_desc:string): TFRE_DB_UserSession;
    function    Promote                  (const user_name,password:TFRE_DB_String;var promotion_status:TFRE_DB_String; force_new_session_data : boolean ; const session_takeover : boolean ; const auto_promote : boolean=false) : TFRE_DB_PromoteResult; // Promote USER to another USER
    procedure   COR_InitiateTakeOver     (const data : Pointer); // In old session binding
    procedure   COR_FinalizeTakeOver     (const data : Pointer); // In new session binding
    procedure   AutoPromote              (const NEW_RASC:IFRE_DB_COMMAND_REQUEST_ANSWER_SC;const conn_desc:String);
    //procedure   Demote                   ;
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
    procedure   SetClientDetails         (const net_conn_desc:String);
    function    GetTakeOverKey           : String;
    function    GetSessionAppArray       : IFRE_DB_APPLICATION_ARRAY;

    function    FetchOrInitFeederMachines  (const MachineNames : TFRE_DB_StringArray):TFRE_DB_GUIDArray; { Initialize or deliver Machine Objects in the Session DB }

    procedure   SetServerClientInterface   (const sc_interface: IFRE_DB_COMMAND_REQUEST_ANSWER_SC;const interactive_session:boolean);
    procedure   ClearServerClientInterface ;
    function    GetClientServerInterface   : IFRE_DB_COMMAND_REQUEST_ANSWER_SC;

    procedure   ClearUpdatable             ;

    procedure   RegisterUpdatableContent   (const contentId: String);
    procedure   UnregisterUpdatableContent (const contentId: String);
    function    IsUpdatableContentVisible  (const contentId: String): Boolean;

    procedure   RegisterUpdatableDBO       (const UID_id: TFRE_DB_GUID);
    procedure   UnregisterUpdatableDBO     (const UID_id: TFRE_DB_GUID);
    function    IsDBOUpdatable             (const UID_id: TFRE_DB_GUID):boolean;


    procedure   SendServerClientRequest  (const description : TFRE_DB_CONTENT_DESC;const session_id:String=''); // Currently no continuation, and answer processing is implemented, is an Async request
    procedure   SendServerClientAnswer   (const description : TFRE_DB_CONTENT_DESC;const answer_id : Qword);
    procedure   SendServerClientCMD      (const cmd : IFRE_DB_COMMAND);

    //Invoke a Method that another Session provides via Register
    function    InvokeRemoteRequest        (const rclassname,rmethodname:TFRE_DB_NameType;const input : IFRE_DB_Object ; const SyncCallback : TFRE_DB_RemoteCB ; const opaquedata : IFRE_DB_Object):TFRE_DB_Errortype;
    procedure   InvokeRemReqCoRoutine      (const data : Pointer);
    procedure   AnswerRemReqCoRoutine      (const data : Pointer);
    procedure   COR_SendContentOnBehalf    (const data : Pointer);
    procedure   COR_ExecuteSessionCmd      (const data : Pointer);

    function    DispatchCoroutine          (const coroutine : TFRE_APSC_CoRoutine;const data : Pointer):boolean; // Call a Coroutine in this sessions thread context


    //Enable a session to "Publish" Remote Methods, overrides previous set
    function    RegisterRemoteRequestSet  (const requests : TFRE_DB_RemoteReqSpecArray):TFRE_DB_Errortype;
    function    RegisterTaskMethod       (const TaskMethod:IFRE_DB_WebTimerMethod ; const invocation_interval : integer ; const id  :String='TIMER') : boolean;
    function    RemoveTaskMethod         (const id:string):boolean;
    function    IsInteractiveSession     : Boolean;

    function    GetDBConnection          : IFRE_DB_CONNECTION;
    function    GetDomain                : TFRE_DB_String;      { doaminname of logged in user }
    function    GetDomainUID             : TFRE_DB_GUID;        { doamin id of logged in user }
    function    GetDomainUID_String      : TFRE_DB_GUID_String; { doamin id as string of logged in user }
    function    GetLoginUserAsCollKey    : TFRE_DB_NameType; { use this if you need per user(session) distinct collections }

    function    GetPublishedRemoteMeths  : TFRE_DB_RemoteReqSpecArray;
    function    GetDownLoadLink4StreamField (const obj_uid: TGUID; const fieldname: TFRE_DB_NameType; const is_attachment: boolean; mime_type: string; file_name: string ; force_url_etag : string=''): String; { Make a DBO Stream Field Downloadable in a Session context }

    { Notification interface function, the notification interface gets an update block pushed from the net/pl layer }
    procedure   DifferentiallUpdStarts (const obj       : IFRE_DB_Object);           { DIFFERENTIAL STATE}
    procedure   DifferentiallUpdEnds   (const obj_uid   : TFRE_DB_GUID);             { DIFFERENTIAL STATE}
    procedure   FieldDelete            (const old_field : IFRE_DB_Field);            { DIFFERENTIAL STATE}
    procedure   FieldAdd               (const new_field : IFRE_DB_Field);            { DIFFERENTIAL STATE}
    procedure   FieldChange            (const old_field,new_field : IFRE_DB_Field);  { DIFFERENTIAL STATE}
    procedure   FinalizeNotif          ;
    {Helper}
    procedure   HandleDiffField        (const mode : TDiffFieldUpdateMode ; const fld : IFRE_DB_Field);
  end;


  TFRE_DB_FetchSessionCB = function(const back_channel: IFRE_DB_COMMAND_REQUEST_ANSWER_SC; out   session : TFRE_DB_UserSession;const old_session_id:string;const interactive_session:boolean):boolean of object;
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

  procedure CheckDbResult                        (const res:TFRE_DB_Errortype;const error_string : TFRE_DB_String ='' ; const conn:IFRE_DB_CONNECTION=nil ; const tolerate_no_change : boolean=true);
  procedure CheckDbResultFmt                     (const res:TFRE_DB_Errortype;const error_string : TFRE_DB_String ='' ; const params:array of const ; const conn:IFRE_DB_CONNECTION=nil ; const tolerate_no_change : boolean=true );
  function  RB_Guid_Compare                      (const d1, d2: TGuid): NativeInt; inline;

  procedure FREDB_LoadMimetypes                  (const filename:string);
  function  FREDB_Filename2MimeType              (const filename:string):TFRE_DB_MimeTypeStr;

  function  FREDB_FindStringIndexInArray         (const text:TFRE_DB_String;const strings:TFRE_DB_StringArray):integer;
  function  FREDB_CombineString                  (const strings: TFRE_DB_StringArray; const sep: TFRE_DB_String): TFRE_DB_String;
  procedure FREDB_SeperateString                 (const value,sep : TFRE_DB_String; var Strings: TFRE_DB_StringArray); //TODO UNICODE

  function  FREDB_DBNameType_Compare             (const S1, S2: TFRE_DB_NameType): NativeInt;
  function  FREDB_DBString_Compare               (const S1, S2: TFRE_DB_String): NativeInt;
  function  FREDB_DBint64_Compare                (const S1, S2: int64): NativeInt;
  function  FREDB_DBuint64_Compare               (const S1, S2: uint64): NativeInt;

  function  FREDB_FieldtypeShortString2Fieldtype (const fts: TFRE_DB_String): TFRE_DB_FIELDTYPE;
  function  FREDB_FilterTypeString2Filtertype    (const fts: TFRE_DB_String): TFRE_DB_FILTERTYPE;
  function  FREDB_Bool2String                    (const bool:boolean):String;
  function  FREDB_EncodeTranslatableWithParams   (const translation_key:TFRE_DB_String  ; params : array of const):TFRE_DB_String;
  function  FREDB_TranslatableHasParams          (var   translation_key:TFRE_DB_String  ; var params : TFRE_DB_StringArray):boolean;
  procedure FREDB_DecodeVarRecParams             (const params   : TFRE_DB_StringArray  ; var vaparams : TFRE_DB_ConstArray);
  procedure FREDB_FinalizeVarRecParams           (const vaparams : TFRE_DB_ConstArray);
  function  FREDB_G2H                            (const uid    : TFRE_DB_GUID):ShortString;
  function  FREDB_H2G                            (const uidstr : shortstring):TFRE_DB_GUID;
  function  FREDB_G2SB                           (const uid    : TFRE_DB_GUID):ShortString; { uid to binary shortstring }
  function  FREDB_SB2G                           (const uid_sb : ShortString):TFRE_DB_GUID; { binary shortstring to uid}
  function  FREDB_ExtractUidsfromRightArray      (const str:TFRE_DB_StringArray;const rightname:TFRE_DB_STRING):TFRE_DB_GUIDArray;
  function  FREDB_String2GuidArray               (const str:string):TFRE_DB_GUIDArray;
  function  FREDB_String2Guid                    (const str:string):TGUID;
  function  FREDB_String2Bool                    (const str:string):boolean;
  function  FREDB_SplitRefLinkDescription        (key_description : TFRE_DB_NameTypeRL ; var rl_field,rl_scheme : TFRE_DB_NameTypeRL):boolean; { True if outbound RL}

  function  FREDB_String2NativeInt               (const str:String):NativeInt;
  function  FREDB_String2NativeUInt              (const str:String):NativeUint;

  function  FREDB_NumFilterType2String           (const nft:TFRE_DB_NUM_FILTERTYPE):String;
  function  FREDB_String2NumfilterType           (const str:string):TFRE_DB_NUM_FILTERTYPE;
  function  FREDB_String2StrFilterType           (const str:string):TFRE_DB_STR_FILTERTYPE;
  function  FREDB_StrFilterType2String           (const sft:TFRE_DB_STR_FILTERTYPE):String;
  function  FREDB_Guids_Same                     (const d1, d2 : TGuid):boolean;
  function  FREDB_Guids_Compare                  (const d1, d2 : TGuid):NativeInt; // 0=Same 1 = d2>d1 -1 = d1>d2
  function  FREDB_Guid_ArraysSame                (const arr1,arr2: TFRE_DB_GUIDArray):boolean;
  function  FREDB_CheckGuidsUnique               (const arr: TFRE_DB_GUIDArray):boolean;
  function  FREDB_GuidList2Counted               (const arr: TFRE_DB_GUIDArray; const stop_on_first_double: boolean=false): TFRE_DB_CountedGuidArray;

  function  FREDB_ObjectToPtrUInt                (const obj : TObject):PtrUInt;
  function  FREDB_PtrUIntToObject                (const obj : PtrUInt):TObject;
  procedure FREDB_BinaryKey2ByteArray            (const key : PByte ; const k_len : NativeInt ; var bytearr : TFRE_DB_ByteArray);

  function  FREDB_GetGlobalTextKey                  (const key: String): String;
  function  FREDB_getThemedResource                 (const id: String): String;
  function  FREDB_RightSetString2RightSet           (const rightstr:ShortString):TFRE_DB_STANDARD_RIGHT_SET;
  function  FREDB_StringInArray                     (const src:string;const arr:TFRE_DB_StringArray):boolean;
  function  FREDB_StringArray2Upper                 (const sa : TFRE_DB_StringArray):TFRE_DB_StringArray;
  function  FREDB_StringInArrayIdx                  (const src:string;const arr:TFRE_DB_StringArray):NativeInt;
  function  FREDB_PrefixStringInArray               (const pfx:string;const arr:TFRE_DB_StringArray):boolean;
  procedure FREDB_ConcatStringArrays                (var TargetArr:TFRE_DB_StringArray;const add_array:TFRE_DB_StringArray);
  procedure FREDB_ConcatGuidArrays                  (var TargetArr:TFRE_DB_GuidArray;const add_array:TFRE_DB_GuidArray);
  function  FREDB_GuidInArray                       (const check:TGuid;const arr:TFRE_DB_GUIDArray):NativeInt;
  function  FREDB_FindNthGuidIdx                    (n:integer;const guid:TGuid;const arr:TFRE_DB_GUIDArray):integer;inline;
  function  FREDB_CheckAllStringFieldsEmptyInObject (const obj:IFRE_DB_Object):boolean;
  function  FREDB_RemoveIdxFomObjectArray           (const arr:IFRE_DB_ObjectArray ; const idx : NativeInt):IFRE_DB_ObjectArray;
  function  FREDB_InsertAtIdxToObjectArray          (const arr:IFRE_DB_ObjectArray ; var at_idx : NativeInt ; const new_obj : IFRE_DB_Object ; const before : boolean):IFRE_DB_ObjectArray;

  function  FREDB_String2DBDisplayType           (const fts: string): TFRE_DB_DISPLAY_TYPE;
  procedure FREDB_SiteMap_AddEntry               (const SiteMapData : IFRE_DB_Object ; const key:string;const caption : String ; const icon : String ; InterAppLink : TFRE_DB_StringArray ;const x,y : integer;  const newsCount:Integer=0; const scale:Single=1; const enabled:Boolean=true);    //obsolete
  procedure FREDB_SiteMap_AddRadialEntry         (const SiteMapData : IFRE_DB_Object ; const key:string;const caption : String ; const icon : String ; InterAppLink : String; const newsCount:Integer=0; const enabled:Boolean=true);
  procedure FREDB_PositionSitemapEntry           (const angle : integer; const radius : integer; const origin_x, origin_y : integer; out x,y:integer);
  procedure FREDB_SiteMap_RadialAutoposition     (const SiteMapData : IFRE_DB_Object; rootangle:integer=0);

  function  FREDB_GuidArray2StringStream         (const arr:TFRE_DB_GUIDArray):String; { Caution ! - used in streaming}
  function  FREDB_StreamString2GuidArray         (str:string):TFRE_DB_GUIDArray; { Caution ! - used in streaming, must be in format of FREDB_GuidArray2String}

  function  FREDB_Get_Rightname_UID              (const rightprefix: string; const id: TGUID): string;
  function  FREDB_Get_Rightname_UID_STR          (const rightprefix: string; const id_str: String): string;
  procedure FREDB_SplitLocalatDomain             (const localatdomain: TFRE_DB_String; var localpart, domainpart: TFRE_DB_String);
  function  FREDB_GetDboAsBufferLen              (const dbo: IFRE_DB_Object; var mem: Pointer): UInt32;

  procedure FREDB_SetStringFromExistingFieldPathOrNoChange(const obj:IFRE_DB_Object ; const fieldpath:string ; var string_fld : TFRE_DB_String); { }
  function  FREDB_HCV                            (const txt : TFRE_DB_String):TFRE_DB_String; { replace cFRE_DB_SYS_CLEAR_VAL_STR with '' use for new operation/web }

  function  FREDB_IniLogCategory2LogCategory     (const ini_logcategory: string) : TFRE_DB_LOGCATEGORY;

  // This function should replace all character which should not a appear in an ECMA Script (JS) string type to an escaped version,
  // as additional feature it replaces CR with a <br> tag, which is useful in formatting HTML
  function  FREDB_String2EscapedJSString         (const input_string:TFRE_DB_String;const replace_cr_with_br:boolean=false) : TFRE_DB_String;

  procedure FREDB_ApplyNotificationBlockToNotifIF            (const block: IFRE_DB_Object ; const deploy_if : IFRE_DB_DBChangedNotification           ; var layer : TFRE_DB_NameType); { full block used in transaction, and in TransDM }
  procedure FREDB_ApplyNotificationBlockToNotifIF_Connection (const block: IFRE_DB_Object ; const deploy_if : IFRE_DB_DBChangedNotificationConnection);
  procedure FREDB_ApplyNotificationBlockToNotifIF_Session    (const block: IFRE_DB_Object ; const deploy_if : IFRE_DB_DBChangedNotificationSession);

  function  FREDB_CompareTransCollDataKeys                    (const a,b : TFRE_DB_TRANS_COLL_DATA_KEY):boolean;

  function  FREDB_PP_GetParentIDHelper_Hack                   (const obj : IFRE_DB_Object ; var   pid : string): boolean;
  function  FREDB_PP_ObjectInParentPath                       (const obj : IFRE_DB_Object ; const pp  : string): boolean;
  function  FREDB_PP_ObjectInParentPathLastParent             (const obj : IFRE_DB_Object ; const pp  : string): boolean;
  procedure FREDB_PP_AddParentPathToObj                       (const obj : IFRE_DB_Object ; const pp  : string);
  function  FREDB_PP_GetParentPaths                           (const obj : IFRE_DB_Object):TFRE_DB_StringArray;


  operator< (g1, g2: TGUID) b : boolean;
  operator> (g1, g2: TGUID) b : boolean;
  operator= (g1, g2: TGUID) b : boolean;

type
  TAddAppToSiteMap_Callback = procedure (const app : TFRE_DB_APPLICATION ; const session: TFRE_DB_UserSession; const parent_entry: TFRE_DB_CONTENT_DESC);

var
  GFRE_DBI                          : IFRE_DB;
  GFRE_DBI_REG_EXTMGR               : IFRE_DB_EXTENSION_MNGR;
  GFRE_DB_NIL_DESC                  : TFRE_DB_NIL_DESC;
  GFRE_DB_SUPPRESS_SYNC_ANSWER      : TFRE_DB_SUPPRESS_ANSWER_DESC;
  GFRE_DB_MIME_TYPES                : Array of TFRE_DB_Mimetype;
  GFRE_DB_TCDM                      : TFRE_DB_TRANSDATA_MANAGER_BASE;

implementation

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



procedure FREDB_LoadMimetypes(const filename: string);
var cnt : NativeInt;

  procedure add(const ext,mt:string);
  begin
   with GFRE_DB_MIME_TYPES[cnt] do
     begin
       extension := ext;
       mimetype  := mt;
     end;
     inc(cnt);
  end;

begin
  if filename<>'' then
    begin
      //TODO PARSE apache MIMETYPEFILE
      abort;
    end
  else
    begin
       SetLength(GFRE_DB_MIME_TYPES,15);
       cnt := 0;
       add('js','application/javascript');
       add('html','text/html');
       add('css','text/css');
       add('gif','image/gif');
       add('jpg','image/jpeg');
       add('png','image/png');
       add('tiff','image/tiff');
       add('txt','text/plain');
       add('svg','image/svg+xml');
       add('swf','application/x-shockwave-flash');
       add('woff','application/font-woff');
       add('ttf','application/octet-stream');
       add('otf','font/opentype');
       add('eot','application/vnd.ms-fontobject');
       add('zip','application/zip');
    end;
end;

function FREDB_Filename2MimeType(const filename: string): TFRE_DB_MimeTypeStr;
var
    i   : NativeInt;
    ext : string;
begin
  ext := Copy(lowercase(ExtractFileExt(filename)),2,maxint);
  for i := 0 to high(GFRE_DB_MIME_TYPES) do
    begin
      if GFRE_DB_MIME_TYPES[i].extension = ext then
        exit(GFRE_DB_MIME_TYPES[i].mimetype);
    end;
  result := 'application/octet-stream';
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

function FREDB_FilterTypeString2Filtertype(const fts: TFRE_DB_String): TFRE_DB_FILTERTYPE;
begin
  for result in TFRE_DB_FILTERTYPE do begin
     if CFRE_DB_FILTERTYPE[result]=fts then exit;
  end;
  raise EFRE_DB_Exception.Create(edb_ERROR,'invalid short filtertype specifier : ['+fts+']');
end;

function FREDB_Bool2String(const bool: boolean): String;
begin
  result := BoolToStr(bool,'1','0');
end;

function FREDB_EncodeVarRec(const v : TVarRec):TFRE_DB_String;
begin
  result := Char(v.VType);
  case v.VType of
    vtInteger:
      result := result + IntToStr(Int64(v.VInteger));
    vtBoolean:
      result := result + BoolToStr(v.VBoolean,'1','0');
    vtChar:
      result := result + v.VChar;
    vtExtended:
      result := result + FloatToStr(v.VExtended^);
    vtString:
      result := result + v.VString^;
    vtAnsiString:
      result := result + AnsiString(v.VAnsiString);
    vtCurrency:
      result := result + CurrToStr(v.VCurrency^);
    //vtWideString:
    //  result := result + PWideString(v.VWideString)^;
    vtInt64:
      result := result + IntToStr(v.VInt64^);
    vtQWord:
      result := result + IntToStr(v.VInt64^);
    vtUnicodeString :
      result := result + UnicodeString(v.VUnicodeString);
    else
      raise Exception.Create('unsuported type'+inttostr(v.VType)+' for encoding');
  end;
  Result := GFRE_BT.Base64Encode(result);
end;

procedure FREDB_DecodeVarRecParams(const params: TFRE_DB_StringArray; var vaparams: TFRE_DB_ConstArray);
var i     : NativeInt;
    param : TFRE_DB_String;
begin
  SetLength(vaparams,Length(params));
  for i:=0 to high(params) do
    begin
      param := params[i];
      if Length(param)=0 then
        raise EFRE_DB_Exception.Create('invalid encoding');
       vaparams[i].VType    := ord(param[1]);
       vaparams[i].VPointer := nil;  { safe zero }
       param := copy(param,2,maxint);
       case vaparams[i].VType of
         vtInteger:
           vaparams[i].VInteger := StrToInt(param);
         vtBoolean:
           vaparams[i].VBoolean := param[1]='1';
         vtChar:
           vaparams[i].VChar    := param[1];
         vtExtended:
           begin
              New(vaparams[i].VExtended);
              vaparams[i].VExtended^ := StrToFloat(param);
           end;
         vtString:
           begin
             New(vaparams[i].VString);
             PShortString(vaparams[i].VString)^ := param;
           end;
         vtAnsiString:
           AnsiString(vaparams[i].VAnsiString) := param;
         vtCurrency:
           begin
             New(vaparams[i].VCurrency);
             vaparams[i].VCurrency^:= StrToCurr(param);
           end;
         vtInt64:
           begin
             New(vaparams[i].VInt64);
             vaparams[i].VInt64^ := StrToInt64(param);
           end;
         vtQWord:
           begin
             New(vaparams[i].VQWord);
             vaparams[i].VQWord^ := StrToQWord(param);
           end;
         vtUnicodeString :
           begin
             UnicodeString(vaparams[i].VUnicodeString) := param;
           end
         else
           raise Exception.Create('unsuported type'+inttostr(vaparams[i].VType)+' for encoding');
       end;
    end;
end;

function FREDB_EncodeTranslatableWithParams(const translation_key: TFRE_DB_String; params: array of const): TFRE_DB_String;
var s : TVarRec;
    i : NativeInt;
begin
  result:=translation_key;
  for i := 0 to high(params) do
    result:=result+'#'+FREDB_EncodeVarRec(params[i]); { no '#' in base64 !} // DO NOT USE # in unparametrized KEYS
end;

function FREDB_TranslatableHasParams(var translation_key: TFRE_DB_String; var params: TFRE_DB_StringArray): boolean;
var i : NativeInt;
begin
  if Pos('#',translation_key)=0 then
    result:=false
  else
    begin
      result := true;
      FREDB_SeperateString(translation_key,'#',params);
      translation_key :=params[0];
      for i := 1 to high(params) do
        params[i]    := GFRE_BT.Base64Decode(params[i]);
      params          := Copy(params,1,maxint);
    end
end;


procedure FREDB_FinalizeVarRecParams(const vaparams: TFRE_DB_ConstArray);
var i : NativeInt;
begin
  for i := 0 to High(vaparams) do
    case vaparams[i].VType of
      //vtInteger:
      //vtBoolean:
      //vtChar:
      vtExtended:
        begin
           Dispose(vaparams[i].VExtended);
           vaparams[i].VExtended:= nil;
        end;
      vtString:
        begin
          Dispose(vaparams[i].VString);
          vaparams[i].VString := nil;
        end;
      vtAnsiString:
        AnsiString(vaparams[i].VAnsiString) := '';
      vtCurrency:
        begin
          Dispose(vaparams[i].VCurrency);
          vaparams[i].VCurrency := nil;
        end;
      vtInt64:
        begin
          Dispose(vaparams[i].VInt64);
          vaparams[i].VInt64 := nil;
        end;
      vtQWord:
        begin
          Dispose(vaparams[i].VQWord);
          vaparams[i].VQWord := nil;
        end;
      vtUnicodeString :
        begin
          UnicodeString(vaparams[i].VUnicodeString) := '';
        end;
    end;
end;

function FREDB_G2H(const uid: TFRE_DB_GUID): ShortString;
begin
  result := GFRE_BT.GUID_2_HexString(uid);
end;

function  FREDB_G2SB(const uid    : TFRE_DB_GUID):ShortString; { uid to binary shortstring }
begin
  SetLength(result,16);
  move(uid,result[1],16);
end;

function  FREDB_SB2G(const uid_sb : ShortString):TFRE_DB_GUID; { binary shortstring to uid}
begin
  assert(Length(uid_sb)=16,'error conversion sb2guid len '+inttostr(Length(uid_sb)));
  move(uid_sb[1],result,16);
end;

function FREDB_H2G(const uidstr: shortstring): TFRE_DB_GUID;
begin
  result := GFRE_BT.HexString_2_GUID(uidstr);
end;

function FREDB_ExtractUidsfromRightArray(const str: TFRE_DB_StringArray; const rightname: TFRE_DB_STRING): TFRE_DB_GUIDArray;
var uid,uidv   : TFRE_DB_GUID;
    entry      : TFRE_DB_String;
    sp,cnt,i,j : NativeInt;
    fnd        : boolean;
begin
  cnt := 0;
  SetLength(result,length(str));
  for i := 0 to high(str) do
    begin
      entry := str[i];
      sp := pos('@',entry);
      if sp>0 then
        begin
          if rightname<>'' then
            if rightname<>copy(entry,1,sp-1) then
              continue;
          uid := FREDB_H2G(Copy(entry,sp+1,maxint));
          fnd := false;
          for j := 0 to cnt-1 do
            begin
              uidv := result[j];
              if uidv=uid then
                begin
                  fnd:=true;
                  break;
                end;
            end;
          if not fnd then
            begin
              result[cnt] := uid;
              inc(cnt);
            end;
        end;
    end;
  SetLength(result,cnt);
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

function FREDB_SplitRefLinkDescription(key_description: TFRE_DB_NameTypeRL; var rl_field, rl_scheme: TFRE_DB_NameTypeRL): boolean;
var fpos,tpos : NativeInt;
begin
   key_description:=uppercase(key_description);
   fpos := pos('>',key_description);
   tpos := pos('<',key_description);
   if (fpos=0) and
      (tpos=0) then
        raise EFRE_DB_Exception.Create(edb_ERROR,'invalid linkref spec, must include exactly one "<" or ">" ');
   if (fpos>0) and
      (tpos>0) then
        raise EFRE_DB_Exception.Create(edb_ERROR,'invalid linkref spec, must include exactly "<" or ">"');
   if fpos>0 then
     result := true
   else
     result := false;
   if result then
     begin
       rl_field  := Copy(key_description,1,fpos-1);
       rl_scheme := Copy(key_description,fpos+1,maxint);
     end
   else
     begin
       rl_scheme := Copy(key_description,1,tpos-1);
       rl_field  := Copy(key_description,tpos+1,maxint);
     end;
end;


function FREDB_String2NativeInt(const str: String): NativeInt;
var Error: word;
begin
  Val(str, result, Error);
  if Error <> 0 then raise Exception.Create('conversion failed str->nativeint');
end;

function FREDB_String2NativeUInt(const str: String) : NativeUint;
var Error: word;
begin
  Val(str, result, Error);
  if Error <> 0 then raise Exception.Create('conversion failed str->nativeuint');
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

function FREDB_Guid_ArraysSame(const arr1, arr2: TFRE_DB_GUIDArray): boolean;
var i : NativeInt;
begin
  if Length(arr1)<>Length(arr2) then
    exit(false);
  for i:=0 to high(arr1) do
    if not FREDB_Guids_Same(arr1[i],arr2[i]) then
      exit(false);
  exit(true);
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

//function FREDB_ObjReferences2GuidArray(const ref: TFRE_DB_ObjectReferences): TFRE_DB_GUIDArray;
//var i,j : NativeInt;
//    cnt : NativeInt;
//begin
//  cnt := 0;
//  for i := 0 to high(ref) do
//    cnt := cnt + Length(ref[i].linklist);
//  SetLength(Result,cnt);
//  cnt   := 0;
//  for i := 0 to High(ref) do
//    for j := 0 to high(ref[i].linklist) do
//      begin
//        Result[cnt] := ref[i].linklist[j];
//        inc(cnt);
//      end;
//end;

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

procedure FREDB_BinaryKey2ByteArray(const key: PByte; const k_len: NativeInt; var bytearr: TFRE_DB_ByteArray);
begin
  SetLength(bytearr,k_len);
  move(key^,bytearr[0],k_len);
end;

function FREDB_GetGlobalTextKey(const key: String): String;
begin
  Result:='$TFRE_DB_GLOBAL_TEXTS_'+key;
end;

function FREDB_getThemedResource(const id: String): String;
begin
  Result:='/fre_css/'+cFRE_WEB_STYLE+'/'+id;
end;

function FREDB_RightSetString2RightSet(const rightstr: ShortString): TFRE_DB_STANDARD_RIGHT_SET;
begin

end;

function FREDB_StringInArray(const src: string; const arr: TFRE_DB_StringArray): boolean;
begin
  result := FREDB_StringInArrayIdx(src,arr)<>-1;
end;

function FREDB_StringArray2Upper(const sa: TFRE_DB_StringArray): TFRE_DB_StringArray;
var
  i: NativeInt;
begin
  SetLength(result,Length(sa));
  for i:=0 to high(result) do
    result[i] := uppercase(sa[i]);
end;

function FREDB_StringInArrayIdx(const src: string; const arr: TFRE_DB_StringArray): NativeInt;
var  i: NativeInt;
begin
  for i:=0 to High(arr) do
    if src=arr[i] then
      exit(i);
  exit(-1);
end;

function FREDB_PrefixStringInArray(const pfx: string; const arr: TFRE_DB_StringArray): boolean;
var  i: NativeInt;
begin
  result := false;
  for i:=0 to High(arr) do
    if Pos(pfx,arr[i])=1 then
      exit(true);
end;

procedure FREDB_ConcatStringArrays(var TargetArr: TFRE_DB_StringArray; const add_array: TFRE_DB_StringArray);
var i,len_target,high_add_array,cnt :integer;
begin
  len_target     := Length(TargetArr);
  SetLength(TargetArr,len_target+Length(add_array));
  high_add_array := high(add_array);
  cnt := 0;
  for i:= 0 to high_add_array do
    if not FREDB_StringInArray(add_array[i],TargetArr) then
      begin
        TargetArr[len_target+cnt] := add_array[i];
        inc(cnt);
      end;
  SetLength(TargetArr,len_target+cnt);
end;

procedure FREDB_ConcatGuidArrays(var TargetArr: TFRE_DB_GuidArray; const add_array: TFRE_DB_GuidArray);
var i,len_target,high_add_array,cnt :integer;
begin
   len_target     := Length(TargetArr);
   SetLength(TargetArr,len_target+Length(add_array));
   high_add_array := high(add_array);
   cnt := 0;
   for i:= 0 to high_add_array do
     if FREDB_GuidInArray(add_array[i],TargetArr)=-1 then
       begin
         TargetArr[len_target+cnt] := add_array[i];
         inc(cnt);
       end;
   SetLength(TargetArr,len_target+cnt);
end;

function FREDB_GuidInArray(const check: TGuid; const arr: TFRE_DB_GUIDArray): NativeInt;
var  i: NativeInt;
begin
  result := -1;
  for i:=0 to High(arr) do
    if FREDB_Guids_Same(check,arr[i]) then
      exit(i);
end;

function FREDB_FindNthGuidIdx(n: integer; const guid: TGuid; const arr: TFRE_DB_GUIDArray): integer;
var i: Integer;
begin
  result:=-1;
  if n<=0 then raise EFRE_DB_Exception.Create(edb_ERROR,'must specify a positive integer greater than zero');
  for i:=0 to high(arr) do
    if FREDB_Guids_Same(guid,arr[i]) then
      begin
        dec(n);
        if n=0 then
          exit(i);
      end;
end;

function FREDB_CheckAllStringFieldsEmptyInObject(const obj: IFRE_DB_Object): boolean;
var check:boolean;
  function CheckFunc(const field:IFRE_DB_FIELD):boolean;
  begin
    result := false;
    if field.IsUIDField then
      exit;
    if field.FieldType=fdbft_Object then
      begin
        check  := FREDB_CheckAllStringFieldsEmptyInObject(field.AsObject);
        result := not check;
      end
    else
      begin
        if field.FieldType<>fdbft_String then
          raise EFRE_DB_Exception.Create(edb_ERROR,'checkempty only works on stringfield-only objects');
        if field.AsString<>'' then
          begin
           result:=true;
           check:=false;
          end;
      end;
  end;
begin
  Check:=true;
  obj.ForAllFieldsBreak(@CheckFunc);
  result := check;
end;

function FREDB_RemoveIdxFomObjectArray(const arr: IFRE_DB_ObjectArray; const idx: NativeInt): IFRE_DB_ObjectArray;
var new_arr : IFRE_DB_ObjectArray;
    cnt,i   : NativeInt;
begin
  if (idx<0) or (idx>High(arr)) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'FREDB_RemoveIdxFomObjectArray idx not in bounds failed -> [%d<=%d<=%d]', [0,idx,high(arr)]);
  SetLength(new_arr,Length(arr)-1);
  cnt := 0;
  for i:=0 to idx-1 do
    begin
      new_arr[cnt] := arr[i];
      inc(cnt)
    end;
  for i:=idx+1 to high(arr) do
    begin
      new_arr[cnt] := arr[i];
      inc(cnt);
    end;
  result := new_arr;
end;

function FREDB_InsertAtIdxToObjectArray(const arr: IFRE_DB_ObjectArray; var at_idx: NativeInt; const new_obj: IFRE_DB_Object ; const before : boolean): IFRE_DB_ObjectArray;
var cnt,i,myat   : NativeInt;
begin
  SetLength(result,Length(arr)+1);
  if (at_idx<0) or (at_idx>High(Result)) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'FREDB_RemoveIdxFomObjectArray idx not in bounds failed -> [%d<=%d<=%d]', [0,at_idx,high(arr)]);
  cnt  := 0;
  myat := at_idx;
  if length(arr)=0 then
    begin
      result[0] := new_obj;
      exit;
    end;
  for i:=0 to high(arr) do
    begin
      if i<>myat then
        begin
          result[cnt] := arr[i];
          inc(cnt);
        end
      else
        begin
          if before then
            begin
              result[cnt] := new_obj;
              at_idx := cnt;
              inc(cnt);
              result[cnt] := arr[i];
              inc(cnt);
            end
          else
            begin
              result[cnt] := arr[i];
              inc(cnt);
              result[cnt] := new_obj;
              at_idx := cnt;
              inc(cnt);
            end;
        end;
    end;
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

procedure CheckDbResult(const res:TFRE_DB_Errortype;const error_string : TFRE_DB_String ='' ; const conn:IFRE_DB_CONNECTION=nil ; const tolerate_no_change : boolean=true);
begin
  CheckDbResultFmt(res,error_string,[],conn,tolerate_no_change);
end;

procedure CheckDbResultFmt(const res:TFRE_DB_Errortype;const error_string : TFRE_DB_String ; const params:array of const ; const conn:IFRE_DB_CONNECTION ; const tolerate_no_change : boolean );
var str : string;
begin
 if (res=edb_OK) or
    ((res=edb_NO_CHANGE) and tolerate_no_change) then
      exit;
 str := Format(error_string,params);
 if assigned(conn) then
   str:=str+LineEnding+conn.GetLastError;
 raise EFRE_DB_Exception.Create(res,str);
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

{ TFRE_DB_WORKFLOW_ACTION }

class procedure TFRE_DB_WORKFLOW_ACTION.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var
  group: IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('key',fdbft_String).required:=true;             { key of the workflow action }
  scheme.AddSchemeField('action_desc',fdbft_String).required:=true;     { description of the workflow action }
  scheme.AddSchemeField('grpkey',fdbft_String).required:=true;          { group key of the context of the workflow action (='PROVISIONING') hardcoded (to enable filtering of autosteps) }
  scheme.AddSchemeField('is_auto',fdbft_Boolean);                       { marks automatic workflow steps }
  scheme.AddSchemeField('action_uidpath',fdbft_GUID).multiValues:=true; { uidpath of the automatic action to be set }
  scheme.AddSchemeField('action_method',fdbft_String);                  { Classname.Methodname of the WEB_Action to be called, the input of the action contains all objects pointing to the WF Object !}
  scheme.SetSysDisplayField(TFRE_DB_NameTypeArray.create('key','action_desc'),'%s-(%s)');

  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('key',GetTranslateableTextKey('scheme_key'));
  group.AddInput('action_desc',GetTranslateableTextKey('scheme_action_desc'));
  group.AddInput('is_auto',GetTranslateableTextKey('scheme_is_auto'));
end;

class procedure TFRE_DB_WORKFLOW_ACTION.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  inherited InstallDBObjects(conn, currentVersionId, newVersionId);
  newVersionId:='0.1';
  if (currentVersionId='UNUSED') then currentVersionId:='';
  if (currentVersionId='') then begin
    currentVersionId:='0.1';
    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_key','Key');
    StoreTranslateableText(conn,'scheme_action_desc','Description');
    StoreTranslateableText(conn,'scheme_is_auto','Automatic');
  end;
end;


{ TFRE_DB_FILTER_BASE }

function TFRE_DB_FILTER_BASE.GetKeyName: TFRE_DB_NameType;
begin
  result := FKey;
end;

function TFRE_DB_FILTER_BASE.FilterNeedsDbUpdate: boolean;
begin
  result := FNeedsReEvaluate;
end;

procedure TFRE_DB_FILTER_BASE.SetFilterNeedsUpdate;
begin
  FNeedsReEvaluate := true;
end;

constructor TFRE_DB_FILTER_BASE.Create(const key: TFRE_DB_NameType);
begin
  FKey             := key;
  FNeedsReEvaluate := false;
end;

function TFRE_DB_FILTER_BASE.CheckReflinkUpdateEvent(const key_descr: TFRE_DB_NameTypeRL): boolean;
begin
  result := false;
end;

{ TFRE_DB_AUDIT_ENTRY }

class procedure TFRE_DB_AUDIT_ENTRY.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.Classname);
  scheme.AddSchemeField('user',fdbft_ObjLink).required:=true; { the user who set the action }
  scheme.AddSchemeField('action_lang_key',fdbft_String);      { the description of the action, LANGUAGE KEY ? - TRANSLATION}
  scheme.AddSchemeField('action_ts',fdbft_DateTimeUTC);       { timestamp of the action}
  scheme.AddSchemeField('note',fdbft_String);                 { inline note / which may be filled in by the user}
end;

class procedure TFRE_DB_AUDIT_ENTRY.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  inherited InstallDBObjects(conn, currentVersionId, newVersionId);
end;

{ TFRE_DB_WORKFLOW_STEP }

function TFRE_DB_WORKFLOW_STEP.getAction: TFRE_DB_GUID;
begin
  Result:=Field('action').AsObjectLink;
end;

function TFRE_DB_WORKFLOW_STEP.getIsErrorStep: Boolean;
begin
  Result:=FieldExists('is_error_step') and Field('is_error_step').AsBoolean;
end;

procedure TFRE_DB_WORKFLOW_STEP.setAction(AValue: TFRE_DB_GUID);
begin
  Field('action').AsObjectLink:=AValue;
end;

procedure TFRE_DB_WORKFLOW_STEP.setIsErrorStep(AValue: Boolean);
begin
  Field('is_error_step').AsBoolean:=AValue;
end;

class procedure TFRE_DB_WORKFLOW_STEP.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var
  group: IFRE_DB_InputGroupSchemeDefinition;
  du,dg: IFRE_DB_FieldSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  {
    workflows can be either in the workflow scheme collection (to define a scheme, or in the workflow collection where scheme levels are linked to parent steps which may or may not be copied from schemes
    example one OE MODULE  points to a level of = steps 1, 2, 2 ,2 ,3 , 4 ,4 , 5 , and one Error step in the Workflow Scheme Collection
    when the the Orrder is Entered, then multiple products and modules  are grouped under one ORDER WF Step (Parent)-> and one WF Step per Product (Childs)-> and all LEVEL STEPS From each OE MODULE (Childs)
    in the WF Collection (not the wf scheme collection)
    the workflow engine advances to the next step id in this level, when all parallel id's are done, and the time condition is satisfied
  }
  scheme.AddSchemeField('step_caption',fdbft_String).required:=true;    { caption of the workflow step }
  scheme.AddSchemeField('step_parent',fdbft_ObjLink);                   { parent of this step }
  scheme.AddSchemeField('step_id',fdbft_UInt32).required:=true;         { order/prio in this wf level, all steps with the same prio are done parallel, all step childs are done before this step }
  scheme.AddSchemeField('is_error_step',fdbft_Boolean);                 { if set to true this is the ERROR catcher step of this level, it's triggered when a step fails }
  scheme.AddSchemeField('error_idx',fdbft_String);                      { index for the error step }
  scheme.AddSchemeField('step_state',fdbft_UInt32);                     { should be an enum : -> 1-> WAITING, 2-> IN PROGRESS, 3-> DONE, 4 -> FAILED }
  du:=scheme.AddSchemeField('designated_user',fdbft_ObjLink);           { this user should do the step }
  du.required:=true;
  dg:=scheme.AddSchemeField('designated_group',fdbft_ObjLink);          { exor this group should do the step }
  dg.required:=true;
  dg.addDepField('designated_user');
  du.addDepField('designated_group');
  scheme.AddSchemeField('done_by_user',fdbft_ObjLink);                  { who has done the step }
  scheme.AddSchemeField('auth_group',fdbft_ObjLink);                    { step needs auth by this group }
  scheme.AddSchemeField('auth_by_user',fdbft_ObjLink);                  { if the action was required to be authorized, by whom it was authorized}
  scheme.AddSchemeField('creation_ts',fdbft_DateTimeUTC);               { timestamp of the creation of the action}
  scheme.AddSchemeField('finalization_ts',fdbft_DateTimeUTC);           { timestamp of the finalization }
  scheme.AddSchemeField('allowed_time',fdbft_UInt32);                   { time in seconds when the action is considered to be failed, and advances to failed state automatically }
  scheme.AddSchemeField('auth_ts',fdbft_DateTimeUTC);                   { timestamp of the auth action }
  scheme.AddSchemeField('note',fdbft_String);                           { inline note / which may be filled in by the user}
  scheme.AddSchemeField('sys_note',fdbft_String);                       { inline note / which may be filled in by the system (e.g. chosen IP adress, whatever) }
  scheme.AddSchemeField('sys_progress',fdbft_String);                   { system progress string, filled in by a (feeder) or the system }
  scheme.AddSchemeField('user_progress',fdbft_String);                  { progress text that is presented to the end user (webuser), which hides detail of the actual progress, may be the same text for several steps or changing percent values / translation key ? .}
  scheme.AddSchemeField('action',fdbft_ObjLink);                        { action }

  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('step_caption',GetTranslateableTextKey('scheme_step_caption'));
  group.AddInput('step_id',GetTranslateableTextKey('scheme_step_id'));
  group.AddInput('designated_group',GetTranslateableTextKey('scheme_designated_group'),false,false,'',false,dh_chooser_combo,coll_GROUP,true);
  group.AddInput('designated_user',GetTranslateableTextKey('scheme_designated_user'),false,false,'',false,dh_chooser_combo,coll_USER,true);
  group.AddInput('auth_group',GetTranslateableTextKey('scheme_auth_group'),false,false,'',false,dh_chooser_combo,coll_GROUP);
  group.AddInput('allowed_time',GetTranslateableTextKey('scheme_allowed_time'));
  group.AddInput('action',GetTranslateableTextKey('scheme_action'),false,false,'',false,dh_chooser_combo,coll_WFACTION,true);

  group:=scheme.AddInputGroup('error_main').Setup(GetTranslateableTextKey('scheme_error_main_group'));
  group.AddInput('step_caption',GetTranslateableTextKey('scheme_step_caption'));
  group.AddInput('designated_group',GetTranslateableTextKey('scheme_designated_group'),false,false,'',false,dh_chooser_combo,coll_GROUP,true);
  group.AddInput('designated_user',GetTranslateableTextKey('scheme_designated_user'),false,false,'',false,dh_chooser_combo,coll_USER,true);
  group.AddInput('action',GetTranslateableTextKey('scheme_action'),false,false,'',false,dh_chooser_combo,coll_WFACTION,true);
end;

class procedure TFRE_DB_WORKFLOW_STEP.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  inherited InstallDBObjects(conn, currentVersionId, newVersionId);
  newVersionId:='0.1';
  if (currentVersionId='UNUSED') then currentVersionId:='';
  if (currentVersionId='') then begin
    currentVersionId:='0.1';

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_step_caption','Caption');
    StoreTranslateableText(conn,'scheme_step_id','Order');
    StoreTranslateableText(conn,'scheme_designated_user','Assigned User');
    StoreTranslateableText(conn,'scheme_designated_group','Assigned Group');
    StoreTranslateableText(conn,'scheme_auth_group','Authorizing Group');
    StoreTranslateableText(conn,'scheme_allowed_time','Allowed time (seconds)');
    StoreTranslateableText(conn,'scheme_action','Action');

    StoreTranslateableText(conn,'scheme_error_main_group','General Information');
  end;
end;

function TFRE_DB_WORKFLOW_STEP.WEB_SaveOperation(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  if input.FieldPathExists('data.designated_user') then begin
    input.FieldPathCreate('data.designated_group').AsString:=cFRE_DB_SYS_CLEAR_VAL_STR
  end else begin
    if input.FieldPathExists('data.designated_group') then begin
      input.FieldPathCreate('data.designated_user').AsString:=cFRE_DB_SYS_CLEAR_VAL_STR;
    end;
  end;
  Result:=inherited WEB_SaveOperation(input, ses, app, conn);
end;

{ TFRE_DB_UNCONFIGURED_MACHINE }

class procedure TFRE_DB_UNCONFIGURED_MACHINE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

class procedure TFRE_DB_UNCONFIGURED_MACHINE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  inherited InstallDBObjects(conn, currentVersionId, newVersionId);

  newVersionId:='1.0';

  if (currentVersionId='') then begin
    currentVersionId:='1.0';
  end;
end;

{ TFRE_DB_UPDATE_FORM_DESC }

function TFRE_DB_UPDATE_FORM_DESC.DescribeDBO(const updateObj: IFRE_DB_Object): TFRE_DB_UPDATE_FORM_DESC;
begin
  Field('obj').AsObject:=updateObj;
  Result:=Self;
end;

function TFRE_DB_UPDATE_FORM_DESC.Describe(const formId: String; const updateObj: IFRE_DB_Object): TFRE_DB_UPDATE_FORM_DESC;
begin
  Field('formId').AsString:=formId;
  Field('obj').AsObject:=updateObj;
  Result:=Self;
end;

{ TFRE_DB_WeakObjectEx }

constructor TFRE_DB_WeakObjectEx.Create(const weakclname: Shortstring);
begin
  FWeakClassname := weakclname;
end;

function TFRE_DB_WeakObjectEx.CloneInstance: TFRE_DB_WeakObjectEx;
begin
  result := TFRE_DB_WeakObjectEx.Create(FWeakClassname);
end;

function TFRE_DB_WeakObjectEx.SchemeClass: TFRE_DB_NameType;
begin
  Result := FWeakClassname;
end;


{ TFRE_DB_OPEN_NEW_LOCATION_DESC }

function TFRE_DB_OPEN_NEW_LOCATION_DESC.Describe(const url: String; const inNewWindow: Boolean): TFRE_DB_OPEN_NEW_LOCATION_DESC;
begin
  Field('url').AsString:=url;
  Field('newWindow').AsBoolean:=inNewWindow;
  Result:=Self;
end;

{ TFRE_DB_RemoteSessionAnswerEncapsulation }

constructor TFRE_DB_RemoteSessionAnswerEncapsulation.Create(const data: IFRE_DB_Object; const status: TFRE_DB_COMMAND_STATUS; const original_command_id: Qword; const opaquedata: IFRE_DB_Object; Contmethod: TFRE_DB_RemoteCB; const SesID: TFRE_DB_String);
begin
  FData       := data;
  FStatus     := status;
  FOCid       := original_command_id;
  Fopaquedata := opaquedata;
  FContmethod := Contmethod;
  FSesID      := SesID;
end;

procedure TFRE_DB_RemoteSessionAnswerEncapsulation.DispatchAnswer(const ses : IFRE_DB_UserSession);
begin
  try
    try
      FContmethod(ses,FData,FStatus,FOCid,Fopaquedata);
    except
      on e:exception do
        begin
          GFRE_DBI.LogError(dblc_SESSION,'DISPATCH REMOTE ANSWER FAILED %s',[e.Message]);
        end;
    end;
  finally
    if assigned(FData) then
      FData.Finalize;
  end;
end;

{ TFRE_DB_RemoteSessionInvokeEncapsulation }

constructor TFRE_DB_RemoteSessionInvokeEncapsulation.Create(const rclassname, rmethodname: TFRE_DB_NameType; const ocid: QWord; const SessionID: String; const input: IFRE_DB_Object; const SyncCallback: TFRE_DB_RemoteCB; const opaquedata: IFRE_DB_Object);
begin
  Fclassname    := rclassname;
  Fmethodname   := rmethodname;
  Finput        := input;
  FsessionID    := SessionID;
  FSyncCallback := SyncCallback;
  Fopaquedata   := opaquedata;
  FOCid         := ocid;
end;

{ TFRE_DB_UPDATE_MESSAGE_PROGRESS_DESC }

function TFRE_DB_UPDATE_MESSAGE_PROGRESS_DESC.Describe(const progressBarId: String; const percentage: Real): TFRE_DB_UPDATE_MESSAGE_PROGRESS_DESC;
begin
 Field('progressBarId').AsString:=progressBarId;
 Field('percentage').AsReal32:=percentage;
 Result:=Self;
end;

{ TFRE_DB_SUPPRESS_ANSWER_DESC }

constructor TFRE_DB_SUPPRESS_ANSWER_DESC.Create;
begin
  inc(NILInstances);
  if NILInstances>1 then begin
    GFRE_BT.CriticalAbort('THIS IS A SINGLETON, ONLY CREATE ONCE!');
  end;
  Inherited Create;
end;

procedure TFRE_DB_SUPPRESS_ANSWER_DESC.Free;
begin
  GFRE_BT.CriticalAbort('DONT DESTROY A FRE SINGLETON WITH STANDARD CALLS');
end;

destructor TFRE_DB_SUPPRESS_ANSWER_DESC.Destroy;
begin
  GFRE_BT.CriticalAbort('DONT DESTROY A FRE SINGLETON WITH STANDARD CALLS');
end;

destructor TFRE_DB_SUPPRESS_ANSWER_DESC.DestroySingleton;
begin
  if self=nil then
    exit;
  FImplementor._InternalSetMediatorScheme(nil,nil);
  FImplementor.Finalize;
  FImplementor := Nil;
  FNamedObject := Nil;
  Inherited Destroy;
end;

procedure TFRE_DB_SUPPRESS_ANSWER_DESC.FreeFromBackingDBO;
begin
  exit;
end;

{ TFRE_DB_NOTE }

class procedure TFRE_DB_NOTE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.Classname);
  scheme.AddSchemeField('link',fdbft_String).required:=true;
  scheme.AddSchemeField('notetyp',fdbft_String); { IF EMPTY this is a Fully User definable NOTE (no logical meaning) -> If set it may be used by the Application logic () }
  scheme.AddSchemeField('note',fdbft_String);
end;

class procedure TFRE_DB_NOTE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if (currentVersionId='') then begin
    currentVersionId:='1.0';
  end;
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

function TFRE_DB_Stream.AsRawByteString :  TFRE_DB_RawByteString;
begin
  if size>0 then
    begin
      SetLength(result,Size);
      Move(Memory^,result[1],Size);
    end
  else
    result := '';
end;

procedure TFRE_DB_Stream.SetFromRawByteString(const rb_string: TFRE_DB_RawByteString);
begin
  Size := Length(rb_string);
  if size>0 then
    Move(rb_string[1],Memory^,Size);
end;

function TFRE_DB_Stream.CalcETag: ShortString;
begin
  result := GFRE_BT.HashString_MD5_HEX(AsRawByteString);
end;

{ TFRE_DB_CLOSE_DIALOG_DESC }

function TFRE_DB_CLOSE_DIALOG_DESC.Describe: TFRE_DB_CLOSE_DIALOG_DESC;
begin
  Result:=Self;
end;

{ TFRE_DB_MENU_DESC }

function TFRE_DB_MENU_DESC.Describe: TFRE_DB_MENU_DESC;
begin
  if not FieldExists('id') then begin
    Field('id').AsString:='id'+UID_String;
  end;
  Result:=Self;
end;

function TFRE_DB_MENU_DESC.AddEntry: TFRE_DB_MENU_ENTRY_DESC;
begin
   Result := TFRE_DB_MENU_ENTRY_DESC.Create;
   Field('entries').AddObject(Result);
end;

function TFRE_DB_MENU_DESC.AddMenu: TFRE_DB_SUBMENU_DESC;
begin
  Result := TFRE_DB_SUBMENU_DESC.Create;
  Field('entries').AddObject(Result);
end;

{ TFRE_DB_SUBMENU_DESC }

function TFRE_DB_SUBMENU_DESC.Describe(const caption,icon: String; const disabled: Boolean; const id:String): TFRE_DB_SUBMENU_DESC;
begin
  inherited Describe();
  Field('caption').AsString:=caption;
  if icon<>'' then begin
    Field('icon').AsString:=FREDB_getThemedResource(icon);
  end;
  if id<>'' then begin
    Field('id').AsString:=id;
  end;
  Field('disabled').AsBoolean:=disabled;
  Result:=Self;
end;


{ TFRE_DB_RESTORE_UI_DESC }

function TFRE_DB_RESTORE_UI_DESC.Describe(const baseContainerId: String; const sectionIds: TFRE_DB_StringArray): TFRE_DB_RESTORE_UI_DESC;
begin
  Field('baseContainerId').AsString:=baseContainerId;
  Field('sectionIds').AsStringArr:=sectionIds;
  Result:=Self;
end;

{ TFRE_DB_SITEMAP_ENTRY_DESC }

function TFRE_DB_SITEMAP_ENTRY_DESC.Describe(const caption, icon: String; const restoreUIFunc: TFRE_DB_RESTORE_UI_DESC; const x, y: Integer; const id:String; const newsCount:Integer; const disabled: Boolean; const scale:Single): TFRE_DB_SITEMAP_ENTRY_DESC;
begin
  Field('caption').AsString:=caption;
  if icon<>'' then begin
    Field('icon').AsString:=FREDB_getThemedResource(icon);
  end;
  Field('sectionpath').AsObject := restoreUIFunc;
  Field('x').AsInt32:=x;
  Field('y').AsInt32:=y;
  Field('id').AsString:=id;
  Field('newscount').AsInt16:=newsCount;
  Field('disabled').AsBoolean:=disabled;
  Field('scale').AsReal32:=scale;
  Result:=Self;
end;

function TFRE_DB_SITEMAP_ENTRY_DESC.AddEntry: TFRE_DB_SITEMAP_ENTRY_DESC;
begin
  Result:=TFRE_DB_SITEMAP_ENTRY_DESC.create;
  Field('entries').AddObject(Result);
end;

{ TFRE_DB_SVG_DEF_ELEM_ATTR_DESC }

function TFRE_DB_SVG_DEF_ELEM_ATTR_DESC.Describe(const name, value: String): TFRE_DB_SVG_DEF_ELEM_ATTR_DESC;
begin
  Field('name').AsString:=name;
  Field('value').AsString:=value;
  Result:=Self;
end;

{ TFRE_DB_SVG_DEF_ELEM_DESC }

function TFRE_DB_SVG_DEF_ELEM_DESC.Describe(const tagName: String): TFRE_DB_SVG_DEF_ELEM_DESC;
begin
  Field('tagname').AsString:=tagName;
  Result:=Self;
end;

function TFRE_DB_SVG_DEF_ELEM_DESC.AddElement: TFRE_DB_SVG_DEF_ELEM_DESC;
begin
  Result:=TFRE_DB_SVG_DEF_ELEM_DESC.create;
  Field('elems').AddObject(Result);
end;

function TFRE_DB_SVG_DEF_ELEM_DESC.AddAttribute: TFRE_DB_SVG_DEF_ELEM_ATTR_DESC;
begin
  Result:=TFRE_DB_SVG_DEF_ELEM_ATTR_DESC.create;
  Field('attrs').AddObject(Result);
end;

{ TFRE_DB_SITEMAP_DESC }

function TFRE_DB_SITEMAP_DESC.Describe(const svgDefs: TFRE_DB_SVG_DEF_ELEM_DESC_ARRAY=nil): TFRE_DB_SITEMAP_DESC;
var
  i: Integer;
begin
  if not FieldExists('id') then begin
    Field('id').AsString:='id'+UID_String;
  end;
  if Assigned(svgDefs) then begin
    for i := 0 to Length(svgDefs) - 1 do begin
      Field('svgDefs').AddObject(svgDefs[i]);
    end;
  end;
  Result:=Self;
end;

function TFRE_DB_SITEMAP_DESC.AddEntry: TFRE_DB_SITEMAP_ENTRY_DESC;
begin
  Result:=TFRE_DB_SITEMAP_ENTRY_DESC.create;
  Field('entries').AddObject(Result);
end;


{ TFRE_DB_SECTION_DESC }

function TFRE_DB_SECTION_DESC._Describe(const title: String; const ord: Int16; const sectionId: String; const size: Integer): TFRE_DB_SECTION_DESC;
var s:string;
begin
  Field('title').AsString:=title;
  Field('ord').AsInt16:=ord;
  if sectionId<>'' then begin
    Field('id').AsString:=sectionId;
  end else begin
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
  end;
  if (size=-1) then begin
    Field('size').AsInt16:=1;
  end else begin
    Field('size').AsInt16:=size;
  end;
  (Parent.Parent.Implementor_HC as TFRE_DB_SUBSECTIONS_DESC).SectionDescribed(UID_String,ord,Field('size').AsInt16);
end;

function TFRE_DB_SECTION_DESC.Describe(const contentFunc: TFRE_DB_SERVER_FUNC_DESC; const title: String; const ord: Int16; const sectionId: String; const size:Integer): TFRE_DB_SECTION_DESC;
begin
  _Describe(title,ord,sectionId,size);
  Field('contentFunc').AsObject:=contentFunc;
  Result:=Self;
end;

function TFRE_DB_SECTION_DESC._internalDescribe(const content: TFRE_DB_CONTENT_DESC; const title: String; const ord: Int16; const sectionId: String; const size: Integer): TFRE_DB_SECTION_DESC;
begin
  _Describe(title,ord,sectionId,size);
  Field('content').AsObject:=content;
  Result:=Self;
end;

procedure TFRE_DB_SECTION_DESC.SetActive(const active: Boolean);
begin
  (Parent.Parent.Implementor_HC as TFRE_DB_SUBSECTIONS_DESC).SetActiveSectionUID(UID_String);
end;

procedure TFRE_DB_SECTION_DESC.SetMenu(const menu: TFRE_DB_MENU_DESC);
begin
  Field('menu').AsObject:=menu;
end;

{ TFRE_DB_SUBSECTIONS_DESC }

procedure TFRE_DB_SUBSECTIONS_DESC.SetActiveSectionUID(const sectionUID: String);
begin
  Field('activeSection').AsString:=sectionUID;
  fnoActiveSectionSet:=false;
end;

procedure TFRE_DB_SUBSECTIONS_DESC.SectionDescribed(const sectionUID: String; const ord,size: Integer);
begin
  Field('sizeSum').AsInt16:=Field('sizeSum').AsInt16+size;
  if fnoActiveSectionSet then begin
    if not FieldExists('activeSection') or (ord<factiveSectionOrd) then begin
      Field('activeSection').AsString:=sectionUID;
      factiveSectionOrd:=ord;
    end;
  end;
end;

constructor TFRE_DB_SUBSECTIONS_DESC.Create;
begin
  fnoActiveSectionSet:=true;
  inherited Create;
end;

function TFRE_DB_SUBSECTIONS_DESC.Describe(const displayType: TFRE_DB_SUBSEC_DISPLAY_TYPE): TFRE_DB_SUBSECTIONS_DESC;
begin
  Field('dt').AsString:=CFRE_DB_SUBSEC_DISPLAY_TYPE[displayType];
  Field('sizeSum').AsInt16:=0;
  if not FieldExists('id') then begin
    Field('id').AsString:='id'+UID_String;
  end;
  Result:=Self;
end;

procedure TFRE_DB_SUBSECTIONS_DESC.OnUIChange(const serverFunc: TFRE_DB_SERVER_FUNC_DESC);
begin
  Field('onUIChange').AsObject:=serverFunc;
end;

function TFRE_DB_SUBSECTIONS_DESC.AddSection(): TFRE_DB_SECTION_DESC;
begin
  Result := TFRE_DB_SECTION_DESC.create;
  Field('sections').AddObject(Result);
end;

procedure TFRE_DB_SUBSECTIONS_DESC.SetActiveSection(const sectionId: String);
var
  i: Integer;
begin
  for i:=0 to Field('sections').ValueCount - 1 do begin
    if Field('sections').AsObjectItem[i].Field('id').AsString=sectionId then begin
      SetActiveSectionUID(Field('sections').AsObjectItem[i].UID_String);
      break;
    end;
  end;
end;


{ TFRE_DB_MENU_ENTRY_DESC }

function TFRE_DB_MENU_ENTRY_DESC._Describe(const caption, icon: String; const disabled: Boolean): TFRE_DB_MENU_ENTRY_DESC;
begin
  Field('caption').AsString:=caption;
  if icon<>'' then begin
    Field('icon').AsString:=FREDB_getThemedResource(icon);
  end;
  Field('disabled').AsBoolean:=disabled;
end;

function TFRE_DB_MENU_ENTRY_DESC.Describe(const caption, icon: String; const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const disabled: Boolean; const id: String): TFRE_DB_MENU_ENTRY_DESC;
begin
  _Describe(caption,icon,disabled);
  if Assigned(serverFunc) then begin
    Field('serverFunc').AsObject:=serverFunc;
  end;
  if id<>'' then begin
    Field('id').AsString:=id;
  end;
  Result:=Self;
end;

function TFRE_DB_MENU_ENTRY_DESC.DescribeDownload(const caption, icon: String; const downloadId: String; const disabled: Boolean): TFRE_DB_MENU_ENTRY_DESC;
begin
  _Describe(caption,icon,disabled);
  Field('downloadId').AsString:=downloadId;
  Result:=Self;
end;


{ TFRE_DB_MESSAGE_DESC }

function TFRE_DB_MESSAGE_DESC.Describe(const caption, msg: String; const msgType: TFRE_DB_MESSAGE_TYPE; const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const progressBarId: String): TFRE_DB_MESSAGE_DESC;
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
  Field('progressBarId').AsString:=progressBarId;
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

procedure TFRE_DB_NIL_DESC.Free;
begin
  GFRE_BT.CriticalAbort('DONT DESTROY A FRE SINGLETON WITH STANDARD CALLS');
end;

destructor TFRE_DB_NIL_DESC.Destroy;
begin
  GFRE_BT.CriticalAbort('DONT DESTROY A FRE SINGLETON WITH STANDARD CALLS');
end;

destructor TFRE_DB_NIL_DESC.DestroySingleton;
begin
  if self=nil then
    exit;
  FImplementor._InternalSetMediatorScheme(nil,nil);
  FImplementor.Finalize;
  FImplementor := Nil;
  FNamedObject := Nil;
  inherited Destroy;
end;

procedure TFRE_DB_NIL_DESC.FreeFromBackingDBO;
begin
  exit;
end;

{ TFRE_DB_NIL_DESC }


{ TFRE_DB_GUID_MANAGER }


{ TFRE_DB_BE_THREAD }

//procedure TFRE_DB_BE_THREAD.Execute;
//begin
//
//end;


procedure TFRE_DB_UserSession.SetOnExistsUserSession4TakeOver(AValue: TFRE_DB_OnExistsUserSessionForKey);
begin
  FonExistsSesForTkKeyL:=AValue;
end;

procedure TFRE_DB_UserSession.LockSession;
begin
  FSessionLock.Acquire;
end;

procedure TFRE_DB_UserSession.UnlockSession;
begin
  FSessionLock.Release;
end;

class procedure TFRE_DB_UserSession.HandleContinuationTimeouts(const onfetch : TFRE_DB_OnFetchSessionByID);
var i           : NativeInt;
    now         : TFRE_DB_DateTime64;
    answerencap : TFRE_DB_RemoteSessionAnswerEncapsulation;
    ses         : TFRE_DB_UserSession;
begin
  now := GFRE_BT.Get_DBTimeNow;
  FContinuationLock.Acquire;
  try
    for i := 0 to high(FContinuationArray) do
      if (FContinuationArray[i].CID<>0)
          and (now > FContinuationArray[i].ExpiredAt) then
            begin
              answerencap := TFRE_DB_RemoteSessionAnswerEncapsulation.Create(nil,cdcs_TIMEOUT,FContinuationArray[i].ORIG_CID,FContinuationArray[i].opData,FContinuationArray[i].Contmethod,FContinuationArray[i].SesID);
              FContinuationArray[i].CID:=0;     // mark slot free
              FContinuationArray[i].Contmethod:=nil;
              FContinuationArray[i].ExpiredAt:=0;
              if onfetch(answerencap.FSesID,ses) then
                begin
                  try
                    if ses.DispatchCoroutine(@ses.AnswerRemReqCoRoutine,answerencap) then
                    else
                      begin
                        writeln('TIMEOUT: (A) Session for answer is gone');
                        answerencap.free;
                      end;
                   finally
                     ses.UnlockSession;
                   end;
                end
              else
                begin
                  writeln('TIMEOUT: (B) Session for answer is gone');
                  answerencap.free;
                end;
            end;
  finally
    FContinuationLock.Release;
  end;
end;

procedure TFRE_DB_UserSession.SetOnWorkCommands(AValue: TNotifyEvent);
begin
  FOnWorkCommands:=AValue;
end;

procedure TFRE_DB_UserSession._FixupDCName(var dcname: TFRE_DB_NameType);
begin
  //dcname := 'dc'+uppercase(dcname)+FSessionID;
  dcname := uppercase(dcname);
end;


constructor TFRE_DB_UserSession.Create(const user_name, password: TFRE_DB_String; const default_app: TFRE_DB_String;const default_uid_path : TFRE_DB_GUIDArray; conn: IFRE_DB_CONNECTION);
begin
  GFRE_TF.Get_Lock(FSessionLock);
  FUserName             := user_name;
  FDefaultApp           := default_app;
  FDBConnection         := conn;
  FDefaultUID           := default_uid_path;
  FSessionID            := 'S'+GFRE_DBI.Get_A_Guid_HEX;
  FSessionTerminationTO := GCFG_SESSION_UNBOUND_TO;
  FTimers               := TList.Create;
  FBinaryInputs         := GFRE_DBI.NewObject;
  FUpdateableDBOS       := GFRE_DBI.NewObject;
  FDifferentialUpdates  := GFRE_DBI.NewObject;
  FUpdateableContent    := GFRE_DBI.NewObject;

  _FetchAppsFromDB;
  _InitApps;
  if not assigned(FContinuationLock) then
     GFRE_TF.Get_Lock(TFRE_DB_UserSession.FContinuationLock);
  GFRE_DBI.LogInfo(dblc_SESSION,'SESSION CREATE User:'+user_name+' App:'+default_app+' '+FSessionID);
end;

procedure TFRE_DB_UserSession.StoreSessionData;
var res : TFRE_DB_Errortype;
    sd  : IFRE_DB_Object;
begin
  if not FPromoted then
    begin
      writeln('YOU COULD NOT STORE SEESION DATA FOR A UNPROMOTED (GUEST) SESSION');
      exit;
    end;
  if assigned(FSessionData) then begin
    GFRE_DBI.LogDebug(dblc_SESSION,'STORING SESSIONDATA FOR ['+FUserName+'] : '+FSessionID);
    sd := FSessionData.CloneToNewObject;
    res := GetDBConnection.StoreUserSessionData(sd);
    if res=edb_OK then begin
      GFRE_DBI.LogInfo(dblc_SESSION,'STORING SESSIONDATA FOR ['+FUserName+'] DONE ');
    end else begin
      GFRE_DBI.LogInfo(dblc_SESSION,'STORING SESSIONDATA FOR ['+FUserName+'] FAILED --> '+CFRE_DB_Errortype[res]);
    end;
  end;
end;

destructor TFRE_DB_UserSession.Destroy;
begin
  RemoveAllTimers;
  if FPromoted then
    StoreSessionData;
  try
    FinishDerivedCollections;
  except on e:exception do
    writeln('***>>  ERROR : FAILED TO FINISH DERIVED COLLECTIONS : '+e.Message);
  end;
  FDBConnection.ClearUserSessionBinding;
  FDBConnection.Finalize;
  FTimers.Free;
  FBinaryInputs.Finalize;
  FUpdateableContent.Finalize;
  FDifferentialUpdates.Finalize;
  FUpdateableDBOS.Finalize;
  FSessionLock.Finalize;
  if assigned(FSessionData) then
    FSessionData.Finalize;
  FSessionData:=nil;
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
     if uppercase(FAppArray[i].AppClassname)=uppercase(app_key) then begin
       idx := i;
       app := FAppArray[i].Implementor_HC as TFRE_DB_APPLICATION;
       exit(true);
     end;
  end;
  result := false;
end;

procedure TFRE_DB_UserSession.InboundNotificationBlock(const block: IFRE_DB_Object);
begin
  if IsInteractiveSession then
    DispatchCoroutine(@self.COR_InboundNotifyBlock,block.Implementor_HC)
  else
    block.Finalize; { silently drop Notification handling for non interactive (currently feeder) sessions }
end;

procedure TFRE_DB_UserSession.COR_InboundNotifyBlock(const data: Pointer);
var block : IFRE_DB_Object;
begin
  block := GFRE_DBI.AnonObject2Interface(data);
  FREDB_ApplyNotificationBlockToNotifIF_Session(block,self);
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
end;

procedure TFRE_DB_UserSession._InitApps;
var i:integer;
begin
  for i := 0 to High(FAppArray) do begin
    if FcurrentApp='' then begin
      SetCurrentApp(FAppArray[i].AppClassName);
    end;
    (FAppArray[i].Implementor_HC as TFRE_DB_APPLICATION).SessionInitialize(self);
  end;
end;

procedure TFRE_DB_UserSession.AddSyncContinuationEntry(const request_id, original_req_id: Qword; const for_session_id: String; const callback: TFRE_DB_RemoteCB; const expire_in: NativeUint; const opaquedata: IFRE_DB_Object);
var i       : integer;
    send_ok : boolean;
begin
  send_ok := false;
  FContinuationLock.Acquire;
    for i := 0 to high(FContinuationArray) do begin
      if FContinuationArray[i].CID=0 then begin
        FContinuationArray[i].CID        := request_id;
        FContinuationArray[i].ORIG_CID   := original_req_id;
        FContinuationArray[i].Contmethod := callback;
        FContinuationArray[i].ExpiredAt  := GFRE_BT.Get_DBTimeNow+expire_in;
        FContinuationArray[i].SesID      := for_session_id;
        FContinuationArray[i].OpData     := opaquedata;
        //FContinuationArray[i].;
        send_ok := true;
        break;
      end;
    end;
  FContinuationLock.Release;
  if not send_ok then
    raise EFRE_DB_Exception.Create(edb_ERROR,'TOO MUCH PENDING C-S Commands !');
end;

procedure TFRE_DB_UserSession.INT_TimerCallBack(const timer: IFRE_APSC_TIMER; const flag1, flag2: boolean);
var wm:IFRE_DB_WebTimerMethod;
begin
  wm := IFRE_DB_WebTimerMethod(timer.TIM_GetMethod);
  wm(self);
end;

procedure TFRE_DB_UserSession.RemoveAllTimers;
var
  i: NativeInt;
begin
  for i:=FTimers.Count-1 downto 0 do
    IFRE_APSC_TIMER(FTimers[i]).Finalize;
  FTimers.Clear;
end;

function TFRE_DB_UserSession.FetchStreamDBO_OTCU(const uid: TGUID; var end_field: TFRE_DB_NameTypeRL; var lcontent: TFRE_DB_RawByteString; var content_type: TFRE_DB_String; var etag: TFRE_DB_String): Boolean;
var dbo: IFRE_DB_Object;
    fld: IFRE_DB_Field;
begin
  result := FDBConnection.Fetch(uid,dbo)=edb_OK;
  if result then
    begin
      try
        end_field := GFRE_BT.HexStr2Str(end_field);
        if not dbo.FieldOnlyExisting(end_field,fld) then
          exit(false);
        if not (fld.FieldType=fdbft_Stream) then
          exit(false);
        lcontent := fld.AsStream.AsRawByteString;
        if dbo.FieldOnlyExisting(end_field+cFRE_DB_STKEY,fld) then
          content_type := fld.AsString
        else
          content_type := '';
        if dbo.FieldOnlyExisting(end_field+cFRE_DB_ST_ETAG,fld) then
          etag := fld.AsString
        else
          etag := '';
      finally
        dbo.Finalize;
      end;
    end;
end;

class destructor TFRE_DB_UserSession.destroyit;
begin
 if assigned(FContinuationLock) then
    FContinuationLock.Finalize;
end;

class constructor TFRE_DB_UserSession.createit;
begin
  FMyReqID := 100000;
end;



procedure TFRE_DB_UserSession.SetSessionState(const sstate: TFRE_DB_SESSIONSTATE);
begin
  FBindState := sstate;
end;

function TFRE_DB_UserSession.GetSessionState: TFRE_DB_SESSIONSTATE;
begin
  result     := FBindState;
end;

function TFRE_DB_UserSession.CheckUnboundSessionForPurge: boolean;
begin
  result := false;
  if FBoundSession_RA_SC=nil then
    begin
      dec(FSessionTerminationTO);
      result := FSessionTerminationTO<1;
      if ((FSessionTerminationTO) mod 60)=0 then
        begin
          GFRE_DBI.LogInfo(dblc_SESSION,'SESSION ['+fsessionid+'/'+FConnDesc+'/'+FUserName+'] KO in '+inttostr(FSessionTerminationTO));
        end;
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

procedure TFRE_DB_UserSession.RemoveAllAppsAndFinalize;
var  i       : Integer;
     applist : Array of String;
begin
  setlength(applist,length(FAppArray));
  for i:=0 to high(FAppArray) do begin
    applist[i] := FAppArray[i].AppClassName;
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
var x           : TObject;
    class_name  : TFRE_DB_String;
    method_name : TFRE_DB_String;
    bdk         : TFRE_DB_String;
    request_id  : int64;
    request_typ : TFRE_DB_COMMANDTYPE;
    input       : IFRE_DB_Object;
    uidp        : TFRE_DB_GUIDArray;
    session     : TFRE_DB_String;
    session_app : TFRE_DB_APPLICATION;
    dummy_idx   : integer;
    st,et : QWord;

    procedure _SendSyncServerClientAnswer;
    begin
      cmd.CommandType   := fct_SyncReply;
      cmd.Answer        := true;
      cmd.ClientCommand := false;
      GFRE_DBI.LogDebug(dblc_SESSION,' SA:> %s',[cmd.Data.SchemeClass]);
      SendServerClientCMD(cmd);
    end;

    procedure _SendSyncServerClienterror(const emessage:string); //TODO - Think about client Error Handling
    begin
      //ANSWER WIH ERROR is not implemented on client side
      CMD.CommandType   := fct_Error;
      CMD.Answer        := true;
      CMD.ClientCommand := false;
      CMD.ErrorText   := 'INVOKE OF ['+class_name+'.'+method_name+'] FAILED '+#13#10+'['+emessage+']';
      GFRE_DBI.LogError(dblc_SESSION,' SERROR:> %s',[cmd.ErrorText]);
      SendServerClientCMD(cmd);
     { old code}
      //cmd.CommandType   := fct_SyncReply;
      //cmd.Answer        := true;
      //cmd.Data          := TFRE_DB_MESSAGE_DESC.Create.Describe('EXCEPTION/UNHANDLED',emessage,fdbmt_error);
      //cmd.ClientCommand := false;
      //GFRE_DBI.LogDebug(dblc_SESSION,' SERROR:> %s',[cmd.Data.SchemeClass]);
      //SendServerClientCMD(cmd);
    end;


    procedure InvokeMethod(const async:boolean ; var input: IFRE_DB_Object);
    var output      : IFRE_DB_Object;
    begin
      try
        output := nil;
        if method_name='ONUICHANGE' then begin
          output:=nil;
        end;
        FCurrentReqID := request_id;
        output        := FDBConnection.InvokeMethod(class_name,method_name,uidp,input,self);
        FcurrentReqID := 0;
        if output=nil then begin
          raise EFRE_DB_Exception.Create('function '+class_name+'.'+method_name+' delivered nil result');
        end;
        CMD.Data          := output;
        if not(output.Implementor_HC is TFRE_DB_SUPPRESS_ANSWER_DESC)
           and (not async) then
            _SendSyncServerClientAnswer
        else
          CMD.Finalize;
      except on e:exception do begin
        et := GFRE_BT.Get_Ticks_ms;
        GFRE_DBI.LogError(dblc_SERVER,'>>(%4.4d ms)<<DISPATCH METHOD %s(%s).%s RID = [%d] TYPE[%s] FAILED[%s]',[et-st,class_name,GFRE_DBI.GuidArray2SString(uidp),method_name,request_id,CFRE_DB_COMMANDTYPE[request_typ],e.Message]);
        CMD.CheckoutData.Finalize;
        input := nil;
        if not async then
          _SendSyncServerClientError(e.Message);
      end;end;
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

    procedure _RegisterRemoteRequestSet(const requests: IFRE_DB_Object);
    var cnt,i : NativeInt;
        arr   : TFRE_DB_RemoteReqSpecArray;
    begin
      cnt := requests.Field('mc').AsInt16;
      SetLength(arr,cnt);
      for i := 0 to  cnt - 1 do begin
        arr[i].classname       := requests.Field('cl'+inttostr(i)).AsString;
        arr[i].methodname      := requests.Field('mn'+inttostr(i)).AsString;
        arr[i].invokationright := requests.Field('ir'+inttostr(i)).AsString;
      end;
      RegisterRemoteRequestSet(arr);
    end;

    procedure DispatchSyncRemoteAnswer(const is_error : boolean);
    var i              : integer;
        answer_matched : boolean;
        match_id       : integer;
        now            : TFRE_DB_DateTime64;
        CID            : Qword;
        ses            : TFRE_DB_UserSession;
        opaq           : IFRE_DB_Object;
        answerencap    : TFRE_DB_RemoteSessionAnswerEncapsulation;
    begin
      now            := GFRE_BT.Get_DBTimeNow;
      answer_matched := false;
      answerencap    := nil;
      FContinuationLock.Acquire;
      try
        for i:=0 to high(FContinuationArray) do begin
          cid := cmd.CommandID;
          if cid = FContinuationArray[i].CID then begin
            if is_error then
              begin
                input.Field('ERROR').AsString := CMD.ErrorText;
                answerencap := TFRE_DB_RemoteSessionAnswerEncapsulation.Create(input,cdcs_ERROR,FContinuationArray[i].ORIG_CID,FContinuationArray[i].opData,FContinuationArray[i].Contmethod,FContinuationArray[i].SesID)
              end
            else
              answerencap := TFRE_DB_RemoteSessionAnswerEncapsulation.Create(input,cdcs_OK,FContinuationArray[i].ORIG_CID,FContinuationArray[i].opData,FContinuationArray[i].Contmethod,FContinuationArray[i].SesID);
            if now <= FContinuationArray[i].ExpiredAt then
              begin
                answer_matched := true;
                FContinuationArray[i].CID:=0;     // mark slot free
                FContinuationArray[i].Contmethod:=nil;
                FContinuationArray[i].ExpiredAt:=0;
              end;
            break;
          end;
        end;
      finally
        FContinuationLock.Release;
      end;
      if answer_matched then begin
        if FSessionID = answerencap.FSesID then
          begin
            abort; // Check this path
            answerencap.DispatchAnswer(self);
          end
        else
          begin
            if FOnFetchSessionByIdL(answerencap.FSesID,ses) then
              begin
                try
                  input := nil ; // ! dont finalize here
                  if ses.DispatchCoroutine(@ses.AnswerRemReqCoRoutine,answerencap) then
                  else
                    begin
                      writeln('Session for answer is gone');
                      answerencap.free;
                    end;
                 finally
                   ses.UnlockSession;
                 end;
              end
            else
              begin
                GFRE_LOG.Log('CANNOT FETCH SESSION FOR SYNC ANSWER /  CID=%d OR TIMEOUT',[CMD.CommandID],catError);
                answerencap.free;
              end;
          end;
      end else begin
        GFRE_LOG.Log('GOT ANSWER FOR UNKNOWN/TIMEDOUT COMMAND CID=%d OR TIMEOUT',[CMD.CommandID],catError);
        GFRE_LOG.Log('CMD: %s',[CMD.AsDBODump],catError);
        answerencap.free;
      end;
      CMD.Finalize;
    end;

    procedure _ProcessInit;
    begin
      if not FPromoted and (cG_OVERRIDE_USER<>'') and (cG_OVERRIDE_PASS<>'') then
        begin // AutoLogin
          GFRE_DBI.LogInfo(dblc_SERVER,'>> SPECIFIC INVOKE FIRMOS.INIT  AUTOLOGIN  SID[%s] USER[%s]',[FSessionID,cG_OVERRIDE_USER]);
          input.Field('data').AsObject.Field('uname').AsString := cG_OVERRIDE_USER;
          input.Field('data').AsObject.Field('pass').AsString  := cG_OVERRIDE_PASS;
          class_name  := FDefaultApp;
          uidp        := FDefaultUID;
          method_name := 'doLogin';
          CMD.ChangeSession := FSessionID;
          InvokeMethod(false,input);
        end
      else
        begin
          GFRE_DBI.LogInfo(dblc_SERVER,'>> SPECIFIC INVOKE FIRMOS.INIT  SID[%s]',[FSessionID]);
          class_name  := FDefaultApp;
          uidp        := FDefaultUID;
          method_name := 'Content';
          input.Field('CLEANUP').AsBoolean := TRUE;
          CMD.ChangeSession := FSessionID;
          InvokeMethod(false,input);
        end;
    end;

    procedure _ProcessLogout;
    begin
      Logout;
      class_name  := FDefaultApp;
      uidp        := FDefaultUID;
      method_name := 'Content';
      CMD.ChangeSession := 'LOGGEDOUT';
      InvokeMethod(false,input);
    end;

    procedure _ProcessDestroy;
    var i : NativeInt;
    begin
      for i := 0 to input.Field('ids').ValueCount - 1 do begin
        unregisterUpdatableContent(input.Field('ids').AsStringItem[i]);
      end;
      CMD.Data        := GFRE_DB_NIL_DESC;
      _SendSyncServerClientAnswer;
    end;

    procedure _ProcessBinaryBulkTransfer;
    {Upload Chunks of Data for later reference by key, }
    var bd   : IFRE_DB_Object;
        sz   : NativeInt;
        cs   : NativeInt;
        name : string;
        typ  : string;
        fld  : string;
        fidx : NativeInt;
        fcnt : NativeInt;
        cidx : NativeInt;
        data : IFRE_DB_Object;

        {
          DATA (OBJECT) :
          { [0]
            NAME (STRING) : [ 'SuperSchramml.png' ]
            SIZE (STRING) : [ '776241' ]
            TYPE (STRING) : [ 'image/png' ]
            FIELD (STRING) : [ 'picture' ]
            CHUNKIDX (STRING) : [ '0' ]
            FIELDIDX (STRING) : [ '0' ]
            CHUNKSIZE (STRING) : [ '776241' ]
          }
          BINCHUNK (STREAM)
        }
    begin
      if FBinaryInputs.FieldExists(bdk) then
        raise EFRE_DB_Exception.Create(edb_ERROR,'chunking, array etc. not implemented');
      data := input.Field('data').AsObject;
      name := data.Field('name').AsString;
      typ  := data.Field('type').AsString;
      fld  := data.Field('field').AsString;
      sz   := StrTointDef(data.Field('size').AsString,-1);
      fcnt := StrToIntDef(data.Field('FIELDCOUNT').AsString,-1);
      fidx := StrTointDef(data.Field('FIELDIDX').AsString,-1);
      cidx := StrTointDef(data.Field('CHUNKIDX').AsString,-1);
      cs   := StrToIntDef(data.Field('chunksize').AsString,-1);
      if (cs=-1)
         or (sz=1) then
           raise EFRE_DB_Exception.Create(edb_ERROR,'size [%s] or chunksize not parseable [%s]',[input.Field('size').AsString,input.Field('chunksize').AsString]);
      if cs<>sz then
        raise EFRE_DB_Exception.Create(edb_ERROR,'size [%d] <> chunksize [%d], chunking not implemented',[sz,cs]);
      if (fidx<>0) or
         (cidx<>0) then
           raise EFRE_DB_Exception.Create(edb_ERROR,'fieldindex must be 0, cidx must be 0',[]);

      bd := FBinaryInputs.Field(bdk).AsObject;
      bd.Field('n').AsString   := name;
      bd.Field('f').AsString   := fld;
      bd.Field('size').AsInt32 := sz; { overall size of binary data}
      bd.Field('typ').AsString := typ;
      bd.Field('fc').AsInt32   := fcnt;
      bd.Field('fi').AsInt32   := fidx;
      bd.Field('bin').AsStream := input.Field('binchunk').AsStream;
      input.Field('binchunk').Clear(true);
      CMD.Data        := GFRE_DB_NIL_DESC;
      _SendSyncServerClientAnswer;
    end;

   procedure  _TryBinaryBulkReplacement;
   var   bd : IFRE_DB_Object;
       fld  : IFRE_DB_Field;
       fn   : TFRE_DB_NameType;
   begin
     if FBinaryInputs.FieldOnlyExisting(bdk,fld) then
       begin
         bd := fld.AsObject;
         fn := bd.Field('f').AsString;
         input.Field('data').AsObject.Field(fn).Clear;
         input.Field('data').AsObject.Field(fn+cFRE_DB_STKEY).Clear;
         input.Field('data').AsObject.Field(fn+cFRE_DB_ST_ETAG).Clear;
         input.Field('data').AsObject.Field(fn).AsStream:=bd.Field('bin').AsStream;
         input.Field('data').AsObject.Field(fn+cFRE_DB_STKEY).AsString:=bd.Field('typ').AsString;
         input.Field('data').AsObject.Field(fn+cFRE_DB_ST_ETAG).AsString := input.Field('data').AsObject.Field(fn).AsStream.CalcETag;
         bd.Field('bin').Clear(true);
         FBinaryInputs.Field(bdk).Clear;
        end
     else
       begin
         raise EFRE_DB_Exception.Create(edb_ERROR,'binary key replacement failed, not found bdk='+bdk);
       end;
   end;


    procedure _ProcessUnregisterDBO;
    var i : NativeInt;
    begin
      for i := 0 to input.Field('ids').ValueCount - 1 do begin
        unregisterUpdatableDBO(FREDB_H2G(input.Field('ids').AsStringItem[i]));
      end;
      CMD.Data        := GFRE_DB_NIL_DESC;
      _SendSyncServerClientAnswer;
    end;


begin
  with cmd do
    begin
      class_name  := InvokeClass;
      method_name := InvokeMethod;
      request_id  := CommandID;
      request_typ := CommandType;
      uidp        := UidPath;
      bdk         := BinaryDataKey;
      input       := CMD.CheckoutData; // Think about Finalization
      if (bdk<>'') and
         (method_name<>'BINARYBULKTRANSFER') then
           _TryBinaryBulkReplacement;
    end;
  st := GFRE_BT.Get_Ticks_ms;
  GFRE_DBI.LogInfo(dblc_SESSION,'>>[%s/%s]-(%d/%s) %s[%s].%s ',[FSessionID,FUserName,request_id,CFRE_DB_COMMANDTYPE[request_typ],class_name,GFRE_DBI.GuidArray2SString(uidp),method_name]);
  case request_typ of
    fct_SyncRequest:  begin
                        try
                          if (class_name='FIRMOS') then begin
                              if (method_name='INIT') then
                                _ProcessInit
                              else
                              if (method_name='LOGOUT') then begin
                                _ProcessLogout;
                              end else
                              if (method_name='DESTROY') then begin
                                _ProcessDestroy;
                              end else
                              if (method_name='BINARYBULKTRANSFER') then begin
                                _ProcessBinaryBulkTransfer;
                              end else
                              if (method_name='UNREGISTERDBO') then begin
                                _ProcessUnregisterDBO;
                              end else begin
                                raise EFRE_DB_Exception.Create(edb_ERROR,'UNKNOWN FIRMOS SPECIFIC COMMAND '+method_name);
                              end;
                          end else begin
                             InvokeMethod(false,input);
                          end;
                        except on e:Exception do begin
                         cmd.CommandType := fct_Error;
                         cmd.Answer        := true;
                         cmd.ClientCommand := false;
                         cmd.ErrorText   := e.Message;
                         GFRE_DBI.LogInfo(dblc_SESSION,'<< ERROR (%s) [%s/%s]-(%d/%s) %s[%s].%s [%d ms]',[e.Message,FSessionID,FUserName,request_id,CFRE_DB_COMMANDTYPE[request_typ],class_name,GFRE_DBI.GuidArray2SString(uidp),method_name,et-st]);
                        end;end;
                      end;
    fct_AsyncRequest: begin
                        if (class_name='FIRMOS') then
                          begin
                            if (method_name='REG_REM_METH') then
                              begin
                                GFRE_DBI.LogInfo(dblc_SERVER,'>> SPECIFIC INVOKE FIRMOS.REG_REM_METH  SID[%s]',[FSessionID]);
                                _RegisterRemoteRequestSet(input);
                                CMD.Finalize;
                              end
                            else
                              begin
                                writeln('UNHANDLED ASYNC REQUEST ?? ',class_name,'.',method_name);
                                CMD.Finalize;
                              end;
                          end
                        else
                          begin
                            InvokeMethod(true,input);
                          end;
                       end;
    fct_SyncReply:     DispatchSyncRemoteAnswer(false);
    fct_Error:         DispatchSyncRemoteAnswer(true);
  end;
  if assigned(input) then
    input.Finalize ;
  et := GFRE_BT.Get_Ticks_ms;
  GFRE_DBI.LogInfo(dblc_SESSION,'<<[%s/%s]-(%d/%s) %s[%s].%s [%d ms]',[FSessionID,FUserName,request_id,CFRE_DB_COMMANDTYPE[request_typ],class_name,GFRE_DBI.GuidArray2SString(uidp),method_name,et-st]);
end;

class procedure TFRE_DB_UserSession.CLS_ForceInvalidSessionReload(rac: IFRE_DB_COMMAND_REQUEST_ANSWER_SC; const cmd: IFRE_DB_COMMAND);
var input : IFRE_DB_Object;
begin
  input := cmd.CheckoutData;
  try
    input.Finalize;
  except
  end;
  // Send an async Reoad in every Case
  cmd.CommandType   := fct_AsyncRequest;
  cmd.Answer        := false;
  cmd.ClientCommand := false;
  cmd.ChangeSession := 'NEW';
  cmd.Data          := TFRE_DB_OPEN_NEW_LOCATION_DESC.create.Describe('/',false);
  GFRE_DBI.LogNotice(dblc_SESSION,'>FORCE INVALID SESSION RELOAD');
  rac.Send_ServerClient(cmd);
end;


function TFRE_DB_UserSession.InternalSessInvokeMethod(const class_name, method_name: string; const uid_path: TFRE_DB_GUIDArray; var input: IFRE_DB_Object): IFRE_DB_Object;
var st,et : QWord;
begin
  st := GFRE_BT.Get_Ticks_ms;
  GFRE_DBI.LogDebug(dblc_SERVER,'>>SESSION/INTERNAL/DISPATCH METHOD %s.%s(%s)  SID=[%s]',[class_name,method_name,GFRE_DBI.GuidArray2SString(uid_path),FSessionID]);
  if assigned(input) then begin
    input.SetReference(self);
    GFRE_DBI.LogDebug(dblc_SERVER_DATA,'INPUT:');
    GFRE_DBI.LogDebug(dblc_SERVER_DATA,'%s',[input.DumpToString(2)]);
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
    GFRE_DBI.LogError(dblc_SERVER_DATA,'INTERNAL DISPATCH METHOD %s.%s(%s) FAILED[%s] SID=[%s]',[class_name,method_name,GFRE_DBI.GuidArray2SString(uid_path),e.Message,FSessionID]);
    raise;
  end;end;
  //GFRE_DBI.LogDebug(dblc_SERVER,'<<SESSION/INTERNAL/DISPATCH METHOD %s.%s(%s) SID=[%s]',[class_name,method_name,GFRE_DBI.GuidArray2SString(uid_path),FSessionID]);
  et := GFRE_BT.Get_Ticks_ms;
  GFRE_DBI.LogDebug(dblc_SERVER,'>>(%4.4d ms)<<SESSION/INTERNAL/DISPATCH METHOD %s.%s(%s) SID=[%s]',[et-st,class_name,method_name,GFRE_DBI.GuidArray2SString(uid_path),FSessionID]);
end;

function TFRE_DB_UserSession.InternalSessInvokeMethod(const app: IFRE_DB_APPLICATION; const method_name: string; const input: IFRE_DB_Object): IFRE_DB_Object;
var inp  : IFRE_DB_Object;
begin
  if not assigned(input) then begin
    inp := GFRE_DBI.NewObject;
    inp.SetReference(self);
    try
      result := app.AsObject.Invoke(method_name,inp,self,app,GetDBConnection);
    finally
      inp.Finalize;
    end;
  end else begin
    input.SetReference(self);
    try
      result := app.AsObject.Invoke(method_name,input,self,app,GetDBConnection);
    finally
      input.SetReference(nil);
    end;
  end;
end;

//function TFRE_DB_UserSession.CloneSession(const connectiond_desc: string): TFRE_DB_UserSession;
//var dbc : IFRE_DB_CONNECTION;
//begin
//  if FOnGetImpersonatedDBC(FDBConnection.GetDatabaseName,FUserName,FPassMD5,dbc)<>edb_OK then
//    GFRE_BT.CriticalAbort('UNEXPECTED, HANDLE');
//  result := TFRE_DB_UserSession.Create(FUserName,FPassMD5,FDefaultApp,FDefaultUID,dbc);
//  result.OnGetImpersonatedDBC    := FOnGetImpersonatedDBC;
//  result.OnExistsUserSession     := FOnExistsUserSessionL;
//  result.OnExistsUserSession4Key := FonExistsSesForTkKeyL;
//  result.OnRestoreDefaultDBC     := FOnRestoreDefaultDBC;
//  result.OnCheckUserNamePW       := FOnCheckUserNamePW;
//  result.OnFetchPublisherRAC     := FOnFetchPublisherSesL;
//  result.OnFetchSessionById      := FOnFetchSessionByIdL;
//  result.FSessionData            := GFRE_DBI.NewObject;
//  result.FConnDesc               := connectiond_desc;
//end;

type
   TCOR_TakeOverData=class
     New_RA_SC          : IFRE_DB_COMMAND_REQUEST_ANSWER_SC;
     FClientDescription : String;
   end;

function TFRE_DB_UserSession.Promote(const user_name, password: TFRE_DB_String; var promotion_status: TFRE_DB_String; force_new_session_data: boolean; const session_takeover: boolean ; const auto_promote: boolean): TFRE_DB_PromoteResult;
var err                : TFRE_DB_Errortype;
    l_NDBC             : IFRE_DB_CONNECTION;
    lStoredSessionData : IFRE_DB_Object;
    promres            : TFRE_DB_PromoteResult;
    existing_session   : TFRE_DB_UserSession;

    procedure ReinitializeApps;
    var i:integer;
    begin
      for i:=0 to high(FAppArray) do begin
        (FAppArray[i].Implementor_HC as TFRE_DB_APPLICATION).SessionPromotion(self);
      end;
    end;

    function TakeOver : TFRE_DB_PromoteResult;
    var tod : TCOR_TakeOverData;
    begin
      GFRE_DBI.LogInfo(dblc_SERVER,'>TAKEOVERSESSION SESSION [%s] USER [%s]',[FSessionID,FUserName]);
      if FonExistsSesForTkKeyL(FConnDesc,existing_session) then
        begin
          try
            assert(existing_session<>self);
            tod := TCOR_TakeOverData.Create;
            tod.New_RA_SC          := FBoundSession_RA_SC;
            tod.FClientDescription := FConnDesc;
            if not existing_session.DispatchCoroutine(@existing_session.COR_InitiateTakeOver,tod) then
              begin {cOld session binding dead (dangling unbound session, do in this thread/socket context) }
                existing_session.COR_InitiateTakeOver(tod);
              end;
            result:=pr_Takeover;
            ClearServerClientInterface; { clear my (guest) bound session RAC };
          finally
            existing_session.UnlockSession;
            GFRE_DBI.LogInfo(dblc_SERVER,'<OK : TAKEOVERSESSION FOR SESSION [%s] USER [%s]',[existing_session.FSessionID,existing_session.FUserName]);
          end;
          exit;
        end
      else
        begin
          promotion_status := 'login_faild_oldnotfound_cap';
          result          := pr_Failed;
          GFRE_DBI.LogWarning(dblc_SERVER,'<FAIL : TAKEOVERSESSION FOR SESSION [%s]',[FSessionID]);
          exit;
        end;
    end;

begin
    if session_takeover then
      begin
        promres := TakeOver;
        exit(promres);
      end;
    GFRE_DBI.NetServ.FetchSessionByIdLocked(user_name,existing_session);
    if assigned(existing_session) then begin
      try
        err := GFRE_DBI.NetServ.CheckUserNamePW(user_name,password);
        case err of
          edb_OK : begin
            if assigned(existing_session.FBoundSession_RA_SC) then
              begin
                promotion_status := FREDB_EncodeTranslatableWithParams('login_faild_already_1P',[existing_session.GetClientDetails]); //'You are already logged in on '+existing_session.GetClientDetails+', would you like to takeover this existing session ?'//;
                existing_session.FTakeoverPrepared := FConnDesc;
                exit(pr_TakeoverPrepared);
                if auto_promote then
                  begin
                    existing_session.AutoPromote(FBoundSession_RA_SC,FConnDesc);
                    FBoundSession_RA_SC := nil; { clear my (guest) bound sesison RAC };
                    exit(pr_TakeOver);
                  end;
              end
            else
             begin
               existing_session.FTakeoverPrepared := FConnDesc; { prepare for auto takeover, after lock release}
             end;
          end
          else begin
            promotion_status := 'login_takeover_failed';
            result           := pr_Failed;
            exit;
          end;
        end;
      finally
        existing_session.UnlockSession;
      end;
      promres := TakeOver; { Auto Takeover dead web session }
      exit(promres);
    end else begin
      err := GFRE_DBI.NetServ.GetImpersonatedDatabaseConnection(FDBConnection.GetDatabaseName,user_name,password,l_NDBC);
      case err of
       edb_OK: begin
          FDBConnection.ClearUserSessionBinding;
          FDBConnection.Finalize;
          FDBConnection:=l_NDBC;
          FDBConnection.BindUserSession(self);
          GFRE_DBI.LogInfo(dblc_SERVER,'PROMOTED SESSION [%s] USER [%s] TO [%s]',[FSessionID,FUserName,user_name]);
          if not force_new_session_data then begin
            if not l_NDBC.FetchUserSessionData(lStoredSessionData) then begin
              FSessionData := GFRE_DBI.NewObject;
              GFRE_DBI.LogDebug(dblc_SERVER,'USING EMPTY/DEFAULT SESSION DATA [%s]',[FSessionData.UID_String]);
            end else begin
              GFRE_DBI.LogDebug(dblc_SERVER,'USING PERSISTENT SESSION DATA [%s]',[lStoredSessionData.UID_String]);
              FSessionData:=lStoredSessionData;
            end;
          end else begin
            FSessionData := GFRE_DBI.NewObject;
            GFRE_DBI.LogDebug(dblc_SERVER,'FORCED USING EMPTY/DEFAULT SESSION DATA [%s]',[FSessionData.UID_String]);
          end;
          FUserName   := user_name;
          FUserdomain := l_NDBC.SYS.GetCurrentUserToken.User.DomainID;
          FPromoted   := true;
          result      := pr_OK;
          _FetchAppsFromDB;
          try
            _InitApps;
          except
            GFRE_DBI.LogEmergency(dblc_SERVER,'LOGIN APPLICATION INITIALIZATION FAILED [%s]',[FSessionData.UID_String]);
          end;
          try
            ReinitializeApps;
          except
            GFRE_DBI.LogEmergency(dblc_SERVER,'LOGIN APPLICATION INITIALIZATION FAILED [%s]',[FSessionData.UID_String]);
          end;
       end;
       else begin
         FPromoted        := false;
         promotion_status := 'login_faild_access';
         result           := pr_Failed;
       end;
      end;
    end;
end;

procedure TFRE_DB_UserSession.COR_InitiateTakeOver(const data: Pointer);
var tod : TCOR_TakeOverData;

    procedure InitiateTakeover(const NEW_RASC: IFRE_DB_COMMAND_REQUEST_ANSWER_SC ; const connection_desc: string);
    var
      MSG  : TFRE_DB_MESSAGE_DESC;
      APP  : TFRE_DB_APPLICATION;
      sId  : String;
      idx  : integer;
      take_over_content : TFRE_DB_CONTENT_DESC;
    begin
      if not FIsInteractive  then begin
        if assigned(FBoundSession_RA_SC) then begin  //TODO - Handle interactive Session
          FBoundSession_RA_SC.DeactivateSessionBinding;
        end;
        FBoundSession_RA_SC := NEW_RASC;
        NEW_RASC.UpdateSessionBinding(self);
      end else begin
        if Assigned(FBoundSession_RA_SC) then
          begin
            MSG := TFRE_DB_MESSAGE_DESC.create.Describe('SESSION TAKEOVER','This session will be continued on another browser instance.',fdbmt_wait,nil);
            sId := FSessionID;
            SendServerClientRequest(msg,'NEW');
            FBoundSession_RA_SC.DeactivateSessionBinding;
          end;
        FConnDesc := connection_desc;
        NEW_RASC.UpdateSessionBinding(self);
        FBoundSession_RA_SC := NEW_RASC;
        DispatchCoroutine(@self.COR_FinalizeTakeOver,nil); // continue in TM of new socket binding
      end;
    end;

begin
  tod := TCOR_TakeOverData(data);
  try
    InitiateTakeover(tod.New_RA_SC,tod.FClientDescription);
  finally
    tod.free;
  end;
end;

procedure TFRE_DB_UserSession.COR_FinalizeTakeOver(const data: Pointer);
begin
  SendServerClientRequest(GFRE_DB_NIL_DESC,FSessionID);
  SendServerClientRequest(TFRE_DB_OPEN_NEW_LOCATION_DESC.create.Describe('/',false)); // OR BETTER SEND THE FULL CONTENT ...
end;

procedure TFRE_DB_UserSession.AutoPromote(const NEW_RASC: IFRE_DB_COMMAND_REQUEST_ANSWER_SC; const conn_desc: String);
begin
  if not FIsInteractive  then
    begin
      if assigned(FBoundSession_RA_SC) then begin
        FBoundSession_RA_SC.DeactivateSessionBinding;
      end;
      FBoundSession_RA_SC := NEW_RASC;
      NEW_RASC.UpdateSessionBinding(self);
    end
  else
    begin
      abort;
    end;
end;

procedure TFRE_DB_UserSession.Logout;
begin
  //SendServerClientRequest(TFRE_DB_MESSAGE_DESC.create.Describe('Logged out.','You have been logged out',fdbmt_wait),'NEW');
  StoreSessionData;
  SendServerClientRequest(GFRE_DB_NIL_DESC,'NEW');
  SendServerClientRequest(TFRE_DB_OPEN_NEW_LOCATION_DESC.create.Describe('/',false));
  FBoundSession_RA_SC.DeactivateSessionBinding;
  FBoundSession_RA_SC := nil;
  FTakeoverPrepared:='';
  FConnDesc:='LOGGEDOUT';
  FSessionTerminationTO := 1;
  SendServerClientRequest(TFRE_DB_OPEN_NEW_LOCATION_DESC.create.Describe('/',false));
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
  result := GetSessionGlobalData.Field('APP_DATA_'+app_key).AsObject;
end;

function TFRE_DB_UserSession.GetSessionModuleData(const mod_key: TFRE_DB_String): IFRE_DB_Object;
begin
  result := GetSessionGlobalData.Field('MOD_DATA_'+mod_key).AsObject;
end;

function TFRE_DB_UserSession.GetSessionGlobalData: IFRE_DB_Object;
begin
  result := FSessionData.Field('G_').AsObject;
end;

function TFRE_DB_UserSession.NewDerivedCollection(dcname: TFRE_DB_NameType): IFRE_DB_DERIVED_COLLECTION;
begin
  _FixupDCName(dcname);
  if not SearchSessionDC(dcname,result) then begin
    result := GetDBConnection.CreateDerivedCollection(dcname);
    result.BindSession(self);
    if dcname<>Result.CollectionName then
      raise EFRE_DB_Exception.Create(edb_ERROR,'PARANOIA '+dcname+' <> '+result.CollectionName);
    SetLength(FDC_Array,Length(FDC_Array)+1);
    FDC_Array[high(FDC_Array)] := result;
  end else begin
    raise EFRE_DB_Exception.create(edb_ERROR,'THE SESSION [%s] ALREADY HAS A DERIVED COLLECTION NAMED [%s]',[FSessionID,dcname]);
  end;
end;

function TFRE_DB_UserSession.FetchDerivedCollection(dcname: TFRE_DB_NameType): IFRE_DB_DERIVED_COLLECTION;
begin
  _FixupDCName(dcname);
  if not SearchSessionDC(dcname,result) then
    raise EFRE_DB_Exception.create(edb_ERROR,'THE SESSION [%s] HAS NO DERIVED COLLECTION NAMED [%s]',[FSessionID,dcname]);
end;

procedure TFRE_DB_UserSession.FinishDerivedCollections;
var i : integer;
   cn  : string;
begin
  for i:=0 to high(FDC_Array) do begin
    try
      cn := FDC_Array[i].CollectionName;
      GetDBConnection.DeleteCollection(cn);
    except on e:EXception do begin
      writeln('*** --- DC Clean failed -> ',e.Message);
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

procedure TFRE_DB_UserSession.SetClientDetails(const net_conn_desc: String);
begin
  FConnDesc := net_conn_desc;
end;

function TFRE_DB_UserSession.GetTakeOverKey: String;
begin
  result := FTakeoverPrepared;
end;

function TFRE_DB_UserSession.GetSessionAppArray: IFRE_DB_APPLICATION_ARRAY;
var i:integer;
begin
  SetLength(result,length(FAppArray));
  for i:=0 to high(FAppArray) do begin
    result[i] := FAppArray[i];
  end;
end;

function TFRE_DB_UserSession.FetchOrInitFeederMachines(const MachineNames: TFRE_DB_StringArray): TFRE_DB_GUIDArray;
var  i      : Integer;
     mcoll  : IFRE_DB_COLLECTION;
     muid   : TFRE_DB_GUID;
     unmach : TFRE_DB_UNCONFIGURED_MACHINE;
begin
  if not FPromoted then
    raise EFRE_DB_Exception.Create(edb_ERROR,'you not allowed the machineobjects [%s]',[FDBConnection.GetDatabaseName]);
  SetLength(result,Length(MachineNames));
  mcoll := FDBConnection.GetCollection(CFRE_DB_MACHINE_COLLECTION);
  for i:=0 to high(MachineNames) do
    begin
      if mcoll.GetIndexedUID(MachineNames[i],muid,'def') then
        result[i] := muid
      else
        begin
          unmach := TFRE_DB_UNCONFIGURED_MACHINE.CreateForDB;
          unmach.ObjectName := MachineNames[i];
          result[i] := unmach.UID;
          CheckDbResult(mcoll.Store(unmach),'failed to store a unconfigured machine');
          GFRE_DBI.LogNotice(dblc_SESSION,'CREATED UNCONFIGURED MACHINE SESSION ['+fsessionid+'] MACHINENAME ['+MachineNames[i]+'] MACHINE_UID ['+FREDB_G2H(result[i])+']');
        end;
    end;
end;

procedure TFRE_DB_UserSession.SetServerClientInterface(const sc_interface: IFRE_DB_COMMAND_REQUEST_ANSWER_SC ; const interactive_session: boolean);
begin
  if assigned(FBoundSession_RA_SC) then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,' REUSE SESSION FAILED, ALREADY BOUND INTERFACE FOUND');
  FBoundSession_RA_SC := sc_interface;
  FIsInteractive      := interactive_session;
  GFRE_DBI.LogNotice(dblc_SESSION,'SET SESSION INTERFACE (RESUE) -> SESSION ['+fsessionid+'/'+FConnDesc+'/'+FUserName+']');
  GFRE_DB_TCDM.DropAllQuerys(self,'');
end;

procedure TFRE_DB_UserSession.ClearServerClientInterface;
begin
  GFRE_DB_TCDM.DropAllQuerys(self,'');
  RemoveAllTimers;
  if FPromoted then
    FSessionTerminationTO := GCFG_SESSION_UNBOUND_TO
  else
    FSessionTerminationTO := 1; // Free unpromoted Sessions Quick
  FBoundSession_RA_SC   := nil;
  GFRE_DBI.LogNotice(dblc_SESSION,'CLEARED SESSION INTERFACE -> SESSION '+BoolToStr(FPromoted,'PROMOTED','GUEST') +' ['+fsessionid+'/'+FConnDesc+'/'+FUserName+'] KO in '+inttostr(FSessionTerminationTO));
end;

function TFRE_DB_UserSession.GetClientServerInterface: IFRE_DB_COMMAND_REQUEST_ANSWER_SC;
begin
  result := FBoundSession_RA_SC;
end;

procedure TFRE_DB_UserSession.ClearUpdatable;
begin
  FUpdateableContent.ClearAllFields;
  FUpdateableDBOS.ClearAllFields;
end;

procedure TFRE_DB_UserSession.RegisterUpdatableContent(const contentId: String);
begin
//  FSessionData.Field('contentIds').AsObject.Field(contentId).AsBoolean:=True;
  FUpdateableContent.Field(contentId).AsBoolean:=True;
end;

procedure TFRE_DB_UserSession.UnregisterUpdatableContent(const contentId: String);
begin
  //  FSessionData.Field('contentIds').AsObject.Field(contentId).Clear();
  FUpdateableContent.Field(contentId).Clear;
end;

procedure TFRE_DB_UserSession.RegisterUpdatableDBO(const UID_id: TFRE_DB_GUID);
var id:ShortString;
begin
  //if FSessionData.Field('dboIds').AsObject.FieldExists(id) then begin
  //  FSessionData.Field('dboIds').AsObject.Field(id).AsInt16:=FSessionData.Field('dboIds').AsObject.Field(id).AsInt16+1;
  //end else begin
  //  FSessionData.Field('dboIds').AsObject.Field(id).AsInt16:=1;
  //end;
  id := FREDB_G2H(UID_id);
  if FUpdateableDBOS.FieldExists(id) then begin
    FUpdateableDBOS.Field(id).AsInt64:=FUpdateableDBOS.Field(id).AsInt64+1;
  end else begin
    FUpdateableDBOS.Field(id).AsInt64:=1;
  end;
end;

procedure TFRE_DB_UserSession.UnregisterUpdatableDBO(const UID_id: TFRE_DB_GUID);
var id : ShortString;
begin
  //if FSessionData.Field('dboIds').AsObject.Field(id).AsInt16=1 then begin
  //  FSessionData.Field('dboIds').AsObject.Field(id).Clear();
  //end else begin
  //  FSessionData.Field('dboIds').AsObject.Field(id).AsInt16:=FSessionData.Field('dboIds').AsObject.Field(id).AsInt16-1;
  //end;
  id := FREDB_G2H(UID_id);
  if FUpdateableDBOS.Field(id).AsInt64=1 then begin
    FUpdateableDBOS.Field(id).Clear();
  end else begin
    FUpdateableDBOS.Field(id).AsInt64:=FUpdateableDBOS.Field(id).AsInt64-1;
  end;
end;

function TFRE_DB_UserSession.IsDBOUpdatable(const UID_id: TFRE_DB_GUID): boolean;
var id : ShortString;
begin
  id := FREDB_G2H(UID_id);
  result := FUpdateableDBOS.FieldExists(id);
end;

function TFRE_DB_UserSession.IsUpdatableContentVisible(const contentId: String): Boolean;
begin
  Result:=FUpdateableContent.FieldExists(contentId);
end;


procedure TFRE_DB_UserSession.SendServerClientRequest(const description: TFRE_DB_CONTENT_DESC;const session_id:String);
var CMD        : IFRE_DB_COMMAND;
    request_id : Qword;
begin
  cmd  := GFRE_DBI.NewDBCommand;
  cmd.SetIsClient(false);
  cmd.SetIsAnswer(false);
  request_id := FMyReqID;
  FOS_IL_INC_NATIVE(FMyReqID);
  cmd.SetCommandID(request_id);
  cmd.CommandType:=fct_AsyncRequest;
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
        FBoundSession_RA_SC := FBoundSession_RA_SC;
        // Client Session has gone ...
      end;
  except on e:exception do
    begin
      writeln('BOUND SESSION RAC EXC: '+e.Message+' ',FSessionID);
    end;
  end
end;

procedure TFRE_DB_UserSession.SendServerClientAnswer(const description: TFRE_DB_CONTENT_DESC; const answer_id: Qword);
var CMD        : IFRE_DB_COMMAND;
begin
  cmd  := GFRE_DBI.NewDBCommand;
  cmd.SetIsClient(false);
  cmd.SetIsAnswer(True);
  cmd.SetCommandID(answer_id);
  cmd.CommandType:=fct_SyncReply;
  cmd.Data := description;
  GFRE_DBI.LogDebug(dblc_SESSION,'>>SERVER CLIENT ANSWER (%s) RID = [%d] TYPE[%s] SID=%s CHANGE SID=%s',[description.ClassName,answer_id,CFRE_DB_COMMANDTYPE[cmd.CommandType],FSessionID,cmd.ChangeSession]);
  //GFRE_DBI.LogDebug(dblc_SESSION,'DATA: %s',[description.DumpToString()]);
  SendServerClientCMD(CMD);
end;

procedure TFRE_DB_UserSession.SendServerClientCMD(const cmd: IFRE_DB_COMMAND);
begin
    if assigned(FBoundSession_RA_SC) then
      FBoundSession_RA_SC.Send_ServerClient(cmd)
    else
      GFRE_DBI.LogWarning(dblc_SESSION,'WARNING DROPPED COMMAND : %s %s',[cmd.InvokeClass+'.'+cmd.InvokeMethod,' ',cmd.Answer]);
end;


function TFRE_DB_UserSession.InvokeRemoteRequest(const rclassname, rmethodname: TFRE_DB_NameType; const input: IFRE_DB_Object ; const SyncCallback: TFRE_DB_RemoteCB; const opaquedata: IFRE_DB_Object): TFRE_DB_Errortype;
var
    right        : TFRE_DB_String;
    ses          : TFRE_DB_UserSession;
    rmethodenc   : TFRE_DB_RemoteSessionInvokeEncapsulation;


begin
  if GFRE_DBI.NetServ.FetchPublisherSessionLocked(uppercase(rclassname),uppercase(rmethodname),ses,right) then
    begin
      try
        rmethodenc := TFRE_DB_RemoteSessionInvokeEncapsulation.Create(rclassname,rmethodname,FCurrentReqID,FSessionID,input,SyncCallback,opaquedata);
        if ses.DispatchCoroutine(@ses.InvokeRemReqCoRoutine,rmethodenc) then
          result := edb_OK
        else
          Result := edb_ERROR;
      finally
        ses.UnlockSession;
      end;
    end
  else
    result := edb_NOT_FOUND;
end;

procedure TFRE_DB_UserSession.InvokeRemReqCoRoutine(const data: Pointer);
var PublisherRAC : IFRE_DB_COMMAND_REQUEST_ANSWER_SC;
    right        : TFRE_DB_String;
    CMD          : IFRE_DB_COMMAND;
    request_id   : Int64;
    ses          : TFRE_DB_UserSession;
    rmethodenc   : TFRE_DB_RemoteSessionInvokeEncapsulation;
    m            : TFRE_APSC_CoRoutine;

begin
  rmethodenc   := TFRE_DB_RemoteSessionInvokeEncapsulation(data);
  try
    cmd  := GFRE_DBI.NewDBCommand;
    cmd.SetIsClient(false);
    cmd.SetIsAnswer(False);
    request_id := FMyReqID;
    FOS_IL_INC_NATIVE(FMyReqID);
    cmd.SetCommandID(request_id);
    cmd.InvokeClass  := rmethodenc.Fclassname;
    cmd.InvokeMethod := rmethodenc.Fmethodname;
    //cmd.UidPath      := input.GetUIDPathUA;
    cmd.Data         := rmethodenc.Finput;
    if Assigned(rmethodenc.FSyncCallback) then
      begin
        cmd.CommandType:=fct_SyncRequest;
        writeln('SEND REMOTE SESSION ID ',FSessionID);
        AddSyncContinuationEntry(request_id,rmethodenc.FOCid,rmethodenc.FsessionID,rmethodenc.FSyncCallback,5000,rmethodenc.Fopaquedata);
      end
    else
     begin
       cmd.CommandType:=fct_AsyncRequest;
     end;
    try
      FBoundSession_RA_SC.Send_ServerClient(cmd);
    except on e:exception do
      begin
        writeln('REMOTE BOUND SESSION RAC EXC: '+e.Message);
      end;
    end
  finally
    rmethodenc.free;
  end;
end;

procedure TFRE_DB_UserSession.AnswerRemReqCoRoutine(const data: Pointer);
begin
  try
    TFRE_DB_RemoteSessionAnswerEncapsulation(data).DispatchAnswer(self);
  finally
    TFRE_DB_RemoteSessionAnswerEncapsulation(data).free;
  end;
end;

procedure TFRE_DB_UserSession.COR_SendContentOnBehalf(const data: Pointer);
var ct : TFRE_DB_CONTENT_DESC;
begin
  ct := TFRE_DB_CONTENT_DESC(data);
  SendServerClientRequest(ct);
end;

procedure TFRE_DB_UserSession.COR_ExecuteSessionCmd(const data: Pointer);
var sc : TFRE_DB_Usersession_COR;
begin
  sc := TFRE_DB_Usersession_COR(data);
  try
    sc.Execute(self)
  finally
    sc.free
  end;
end;

function TFRE_DB_UserSession.DispatchCoroutine(const coroutine: TFRE_APSC_CoRoutine; const data: Pointer):boolean;
begin
  try
    result := true;
    if assigned(FBoundSession_RA_SC) then
      FBoundSession_RA_SC.GetChannel.GetChannelManager.ScheduleCoRoutine(CoRoutine,data)
    else
      result:=false;
  except
    on E:Exception do
      begin
        result := false;
        writeln('@Dispatchcoroutine Exception ',FSessionID,' ',FUserName);
        raise;
      end;
  end;
end;

function TFRE_DB_UserSession.RegisterRemoteRequestSet(const requests: TFRE_DB_RemoteReqSpecArray): TFRE_DB_Errortype;
begin
  FRemoteRequestSet := requests;
end;

function TFRE_DB_UserSession.RegisterTaskMethod(const TaskMethod: IFRE_DB_WebTimerMethod; const invocation_interval: integer; const id: String): boolean;
var my_timer : IFRE_APSC_TIMER;
    i        : NativeInt;
begin
   for i:=0 to FTimers.Count-1 do begin
     if  lowercase(IFRE_APSC_TIMER(FTimers[i]).TIM_GetID)=lowercase(id) then
       exit(false);
   end;
   my_timer := FBoundSession_RA_SC.GetChannel.GetChannelManager.AddTimer(invocation_interval);
   my_timer.TIM_Start;
   my_timer.TIM_SetID(id);
   my_timer.TIM_SetMethod(TMethod(TaskMethod));
   my_timer.TIM_SetCallback(@INT_TimerCallBack);
   FTimers.Add(my_timer);
end;

function TFRE_DB_UserSession.RemoveTaskMethod(const id: string): boolean;
var
  i: NativeInt;
begin
  for i:=FTimers.Count-1 downto 0 do
    if  lowercase(IFRE_APSC_TIMER(FTimers[i]).TIM_GetID)=lowercase(id) then
      begin
        IFRE_APSC_TIMER(FTimers[i]).Finalize;
        FTimers.Delete(i);
        exit(true);
      end;
  exit(false);
end;


function TFRE_DB_UserSession.IsInteractiveSession: Boolean;
begin
  result := FIsInteractive;
end;

//function TFRE_DB_UserSession.FetchTranslateableText(const translation_key: TFRE_DB_String; var textObj: IFRE_DB_TEXT): Boolean;
//begin
//  result := GetDBConnection.FetchTranslateableText(translation_key,textObj);
//end;

function TFRE_DB_UserSession.GetDBConnection: IFRE_DB_CONNECTION;
begin
  result := FDBConnection;
end;

function TFRE_DB_UserSession.GetDomain: TFRE_DB_String;
var
  loc: TFRE_DB_String;
begin
  FREDB_SplitLocalatDomain(FUserName,loc,Result);
end;

function TFRE_DB_UserSession.GetDomainUID: TFRE_DB_GUID;
begin
  result := FuserDomain;
end;

function TFRE_DB_UserSession.GetDomainUID_String: TFRE_DB_GUID_String;
begin
  result := FREDB_G2H(GetDomainUID);
end;

function TFRE_DB_UserSession.GetLoginUserAsCollKey: TFRE_DB_NameType;
begin
  result := GFRE_BT.HashString_MD5_HEX(FUserName);
end;

function TFRE_DB_UserSession.GetPublishedRemoteMeths: TFRE_DB_RemoteReqSpecArray;
begin
  result := FRemoteRequestSet;
end;

function TFRE_DB_UserSession.GetDownLoadLink4StreamField(const obj_uid: TGUID; const fieldname: TFRE_DB_NameType; const is_attachment: boolean; mime_type: string; file_name: string; force_url_etag: string): String;
begin
  if mime_type='' then
    mime_type:='application/binary';
  if file_name='' then
    file_name:='-';
  if force_url_etag='' then
    force_url_etag:='-';
  result := '/FDBOSF/'+FSessionID+'/'+GFRE_BT.GUID_2_HexString(obj_uid)+'/'+BoolToStr(is_attachment,'A','N')+'/'+ GFRE_BT.Str2HexStr(mime_type)+'/'+ GFRE_BT.Str2HexStr(file_name)+'/'+GFRE_BT.Str2HexStr(force_url_etag)+'/'+GFRE_BT.Str2HexStr(fieldname);
end;


procedure TFRE_DB_UserSession.FieldDelete(const old_field: IFRE_DB_Field);
begin
  HandleDiffField(mode_df_del,old_field);
  //if FCurrentNotificationBlockLayer='SYSTEM' then
  //  FDBConnection.SYS.GetNotif.FieldDelete(old_field)
  //else
  //  FDBConnection.GetNotif.FieldDelete(old_field);
end;

procedure TFRE_DB_UserSession.FieldAdd(const new_field: IFRE_DB_Field);
begin
  HandleDiffField(mode_df_add,new_field);
  //if FCurrentNotificationBlockLayer='SYSTEM' then
  //  FDBConnection.SYS.GetNotif.FieldAdd(new_field)
  //else
  //  FDBConnection.GetNotif.FieldAdd(new_field);
end;

procedure TFRE_DB_UserSession.FieldChange(const old_field, new_field: IFRE_DB_Field);
begin
  HandleDiffField(mode_df_change,new_field);
  //if FCurrentNotificationBlockLayer='SYSTEM' then
  //  FDBConnection.SYS.GetNotif.FieldChange(old_field,new_field)
  //else
  //  FDBConnection.GetNotif.FieldChange(old_field,new_field);
end;

procedure TFRE_DB_UserSession.DifferentiallUpdEnds(const obj_uid: TFRE_DB_GUID);
var upo : IFRE_DB_Object;
    key : shortstring;
    fld : IFRE_DB_Field;
begin
  key := FREDB_G2H(obj_uid);
  if FDifferentialUpdates.FieldOnlyExisting(key,fld) then
    begin
      upo := fld.CheckOutObject;
      //writeln('DIFF UPDATE O');
      //writeln(upo.DumpToString);
      SendServerClientRequest(TFRE_DB_UPDATE_FORM_DESC.create.DescribeDBO(upo.CloneToNewObject));
      //writeln('SENT');
      upo.Finalize;
    end;
end;

procedure TFRE_DB_UserSession.DifferentiallUpdStarts(const obj: IFRE_DB_Object);
begin
  FDifferentialUpdates.Field(obj.UID_String).AsObject := obj.CloneToNewObject;
end;

procedure TFRE_DB_UserSession.FinalizeNotif;
begin
  raise EFRE_DB_Exception.Create(edb_INTERNAL,'should not be called');
end;

procedure TFRE_DB_UserSession.HandleDiffField(const mode: TDiffFieldUpdateMode; const fld: IFRE_DB_Field);
var upouidp : TFRE_DB_GUIDArray;
    upofldp : TFRE_DB_StringArray;
    uposchp : TFRE_DB_StringArray;
    fldname : TFRE_DB_NameType;
    fldsch  : TFRE_DB_NameType;
    upo     : IFRE_DB_Object;
    intero  : IFRE_DB_Object;
    i       : NativeInt;
begin
  upouidp := fld.GetUpdateObjectUIDPath;
  upofldp := fld.GetUpdateObjFieldPath;
  uposchp := fld.GetUpdateObjSchemePath;
  upo := FDifferentialUpdates.FieldOnlyExistingObj(FREDB_G2H(upouidp[0])); { get root object }
  if assigned(upo) then
    begin
      for i:=1 to high(upouidp) do
        begin
          fldname := upofldp[i-1];
          fldsch  := uposchp[i];
          if not upo.FieldExists(fldname) then
            begin
              intero := GFRE_DBI.NewObjectSchemeByName(fldsch);
              intero.Field('uid').AsGUID := upouidp[i];
              upo.Field(fldname).AsObject := intero;
              upo    := intero;
            end;
        end;
      fldname := fld.FieldName;
      case mode of
        mode_df_add    : upo.Field(fldname).CloneFromField(fld);
        mode_df_del    : upo.Field(fldname).AsString :=  cFRE_DB_SYS_CLEAR_VAL_STR;
        mode_df_change : upo.Field(fldname).CloneFromField(fld);
      end;
    end;
end;

constructor TFOS_BASE.Create;
begin
  inherited
end;

function TFOS_BASE.Implementor: TObject;
begin
  result := self;
end;

function TFOS_BASE.Implementor_HC: TObject;
begin
  result := self;
end;

function TFOS_BASE.GetImplementorsClass: TClass;
begin
  result := self.ClassType;
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

class procedure TFRE_DB_Base._InstallDBObjects4Domain(const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID : TGUID);
var
  role: IFRE_DB_ROLE;
begin
  if not (self.ClassType=TFRE_DB_ObjectEx) then
    begin
      if currentVersionId='' then // Initial Install
        begin
          role := CreateClassRole('store','Store ' + ClassName,'Allowed to store new ' + ClassName + ' objects');
          role.AddRight(GetRight4Domain(GetClassRightNameStore,domainUID));
          CheckDbResult(conn.StoreRole(role,domainUID),'Error creating '+ClassName+'.store role');

          role := CreateClassRole('delete','Delete ' + ClassName,'Allowed to delete ' + ClassName + ' objects');
          role.AddRight(GetRight4Domain(GetClassRightNameDelete,domainUID));
          CheckDbResult(conn.StoreRole(role,domainUID),'Error creating '+ClassName+'.delete role');

          role := CreateClassRole('update','Update ' + ClassName,'Allowed to edit ' + ClassName + ' objects');
          role.AddRight(GetRight4Domain(GetClassRightNameUpdate,domainUID));
          CheckDbResult(conn.StoreRole(role,domainUID),'Error creating '+ClassName+'.update role');

          role := CreateClassRole('fetch','Fetch ' + ClassName,'Allowed to fetch ' + ClassName + ' objects');
          role.AddRight(GetRight4Domain(GetClassRightNameFetch,domainUID));
          CheckDbResult(conn.StoreRole(role,domainUID),'Error creating '+ClassName+'.fetch role');
        end
    end;
end;

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

class procedure TFRE_DB_Base.VersionInstallationCheck(const currentVersionId, newVersionId: TFRE_DB_NameType);
begin
  if ((newVersionId<>'UNUSED') and (newVersionId<>'BASE')) and (currentVersionId<>newVersionId) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'%s> install failed, not all versions handled properly old=[%s] new=[%s]',[classname,currentVersionId,newVersionId]);
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

class function TFRE_DB_Base.GetStdObjectRightPart(const std_right: TFRE_DB_STANDARD_RIGHT): TFRE_DB_String;
begin
  case std_right of
    sr_STORE  : result:=':STR';
    sr_UPDATE : result:=':UPD';
    sr_DELETE : result:=':DEL';
    sr_FETCH  : result:=':FET';
  else
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'rightname for standard right is not defined!');
  end;
end;

class function TFRE_DB_Base._GetClassRight(const right: TFRE_DB_NameType): IFRE_DB_RIGHT;
begin
  Result:= GetClassRightName(right);
end;

class function TFRE_DB_Base.GetRight4Domain(const right: TFRE_DB_NameType; const domainUID: TGUID): IFRE_DB_RIGHT;
begin
 result := uppercase(right+'@'+GFRE_BT.GUID_2_HexString(domainUID));
end;

class function TFRE_DB_Base.GetClassRightName(const rclassname: ShortString; const right: TFRE_DB_NameType): TFRE_DB_String;
begin
  Result := uppercase('$O_R_'+RClassName+'_'+right);
end;

class function TFRE_DB_Base.CreateClassRole(const rolename: TFRE_DB_String; const short_desc, long_desc: TFRE_DB_String): IFRE_DB_ROLE;
begin
 result := GFRE_DBI.NewRole(GetClassRoleName(rolename),long_desc,short_desc,true);
end;

class function TFRE_DB_Base.GetClassRoleName(const rolename: TFRE_DB_String): TFRE_DB_String;
begin
  Result:=uppercase('$CR_'+ClassName+'_'+rolename);
end;

class function TFRE_DB_Base.GetClassRoleNameUpdate: TFRE_DB_String;
begin
  Result:=GetClassRoleName('update');
end;

class function TFRE_DB_Base.GetClassRoleNameDelete: TFRE_DB_String;
begin
  Result:=GetClassRoleName('delete');
end;

class function TFRE_DB_Base.GetClassRoleNameStore: TFRE_DB_String;
begin
  Result:=GetClassRoleName('store');
end;

class function TFRE_DB_Base.GetClassRoleNameFetch: TFRE_DB_String;
begin
  Result:=GetClassRoleName('fetch');
end;

class function TFRE_DB_Base.GetClassStdRoles(const store: boolean; const update: boolean; const delete: boolean; const fetch: boolean): TFRE_DB_StringArray;

 procedure _add(const name : TFRE_DB_String);
 begin
  SetLength(result,length(result)+1);
  result[high(result)] := name;
 end;

begin
  SetLength(result,0);
  if store then
    _Add(GetClassRoleNameStore);
  if update then
    _Add(GetClassRoleNameUpdate);
  if delete then
    _Add(GetClassRoleNameDelete);
  if fetch then
    _Add(GetClassRoleNameFetch);
end;

class function TFRE_DB_Base.GetClassRightName(const right: TFRE_DB_NameType): TFRE_DB_String;
begin
  Result := GetClassRightName(Classname,right);
end;

class function TFRE_DB_Base.GetClassRightNameSR(const rclassname: ShortString; const sright: TFRE_DB_STANDARD_RIGHT): TFRE_DB_String;
begin
  case sright of
    sr_STORE:  Result:=GetClassRightName(rclassname,'store');
    sr_UPDATE: Result:=GetClassRightName(rclassname,'update');
    sr_DELETE: Result:=GetClassRightName(rclassname,'delete');
    sr_FETCH:  Result:=GetClassRightName(rclassname,'fetch');
    else
      raise EFRE_DB_Exception.Create(edb_ERROR,'invalid standard right');
  end;
end;

class function TFRE_DB_Base.GetClassRightNameUpdate: TFRE_DB_String;
begin
  Result:=GetClassRightNameSR(ClassName,sr_UPDATE);
end;

class function TFRE_DB_Base.GetClassRightNameDelete: TFRE_DB_String;
begin
  Result:=GetClassRightNameSR(ClassName,sr_DELETE);
end;

class function TFRE_DB_Base.GetClassRightNameStore: TFRE_DB_String;
begin
  Result:=GetClassRightNameSR(ClassName,sr_STORE);
end;

class function TFRE_DB_Base.GetClassRightNameFetch: TFRE_DB_String;
begin
  Result:=GetClassRightNameSR(ClassName,sr_FETCH);
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
               if (pos('IMI_',methodtable^.entries[i].name^)=1) or (pos('WEB_',methodtable^.entries[i].name^)=1) then begin
                 SetLength(result,Length(result)+1);
                 result[High(result)] := uppercase(Copy(methodtable^.entries[i].name^,5,MaxInt));
               end;
             end;
          end;
        ovmt := ovmt^.vParent;
     end;
end;

class function TFRE_DB_Base.Get_DBI_RemoteMethods: TFRE_DB_StringArray;
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
               if (pos('REM_',methodtable^.entries[i].name^)=1) then begin
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
               if (pos('IMC_',methodtable^.entries[i].name^)=1) or (pos('WBC_',methodtable^.entries[i].name^)=1) then begin
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
  result := (MethodAddress('IMC_'+name)<>nil) or (MethodAddress('WBC_'+name)<>nil);
end;

class function TFRE_DB_Base.Invoke_DBIMC_Method(const name: TFRE_DB_String; const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var M  :IFRE_DB_InvokeClassMethod;
    MM :TMethod;
    WM :IFRE_DB_WebClassMethod;
begin
   result := nil;
   MM.Code := MethodAddress('WBC_'+name);
   MM.Data := self;
   if assigned(MM.Code) then
     begin
       WM := IFRE_DB_WebClassMethod(mm);
       try
         result := wm(input,ses,app,conn);
       except on e:exception do begin
         raise EFRE_DB_Exception.Create(edb_ERROR,'CLASS METHOD INVOCATION %s.%s failed (%s)',[Classname,name,e.Message]);
       end;end;
     end
   else
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
end;


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
       result := wm(input,ses,app,conn);
     end
   else
     begin
       MM.Code := MethodAddress('IMI_'+name);
       M := IFRE_DB_InvokeInstanceMethod(MM);
       if assigned(mm.Code) then
         result := m(input)
       else
          raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'INSTANCE METHOD INVOCATION %s.%s (%s) failed (method not found)',[Classname,name,Debug_ID]);
     end;
end;

procedure TFRE_DB_Base.Invoke_DBREM_Method(const rmethodname: TFRE_DB_NameType; const command_id: Qword; const input: IFRE_DB_Object; const cmd_type: TFRE_DB_COMMANDTYPE);
var M  : IFRE_DB_RemInstanceMethod;
    MM : TMethod;
begin
   MM.Code := MethodAddress('REM_'+rmethodname);
   MM.Data := self;
   if assigned(MM.code) then
     begin
       M := IFRE_DB_RemInstanceMethod(MM);
       m(command_id,input,cmd_type);
     end
   else
     raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'REM INSTANCE METHOD INVOCATION %s.%s failed (method not found)',[Classname,rmethodname]);
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
     result := assigned(MethodAddress('IMI_'+name)) or assigned(MethodAddress('WEB_'+name));
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

procedure TFRE_DB_ObjectEx._InternalSetMediatorScheme(const mediator: TFRE_DB_ObjectEx; const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  raise EFRE_DB_Exception.Create(edb_ERROR,'dont call this');
end;

function TFRE_DB_ObjectEx._InternalDecodeAsField: IFRE_DB_Field;
begin
  abort;
end;

function TFRE_DB_ObjectEx.GetDescriptionID: String;
begin
  result := FImplementor.GetDescriptionID;
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

class procedure TFRE_DB_Base.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  if self.ClassType=TFRE_DB_ObjectEx then
    newVersionId := 'BASE'
  else
    newVersionId := 'UNUSED';
end;

class procedure TFRE_DB_Base.InstallDBObjects4Domain(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TGUID);
begin
  _InstallDBObjects4Domain(conn,currentVersionId,domainUID);
end;

class procedure TFRE_DB_Base.InstallDBObjects4SysDomain(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TGUID);
begin
  _InstallDBObjects4Domain(conn,currentVersionId,domainUID);
end;

class procedure TFRE_DB_Base.InstallUserDBObjects(const conn: IFRE_DB_CONNECTION; currentVersionId: TFRE_DB_NameType);
begin

end;

class procedure TFRE_DB_Base.InstallUserDBObjects4Domain(const conn: IFRE_DB_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TGUID);
begin

end;

class procedure TFRE_DB_Base.InstallUserDBObjects4SysDomain(const conn: IFRE_DB_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TGUID);
begin

end;

class procedure TFRE_DB_Base.RemoveDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType);
begin
end;

class procedure TFRE_DB_Base.RemoveDBObjects4Domain(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TGUID);
begin

end;

constructor TFRE_DB_ObjectEx.create;
begin
  FBound       := false;
  if not GFRE_DBI.NewObjectIntf(IFRE_DB_NAMED_OBJECT,FNamedObject,self) then
    abort;
  FNamedObject.Supports(IFRE_DB_OBJECT,FImplementor);
  InternalSetup;
end;


constructor TFRE_DB_ObjectEx.CreateBound(const dbo: IFRE_DB_Object ; const internal_setup : boolean);
begin
  FBound       := true;
  FImplementor := dbo;
  FImplementor.Supports(IFRE_DB_NAMED_OBJECT,FNamedObject);
  if internal_setup then
    InternalSetup;
end;

destructor TFRE_DB_ObjectEx.Destroy;
begin
  if not FBound then
    begin
      if assigned(FNamedObject) then
        FNamedObject.Finalize;
    end;
  inherited Destroy;
end;

destructor TFRE_DB_ObjectEx.DestroyFromBackingDBO;
begin
  inherited Destroy;
end;

procedure TFRE_DB_ObjectEx.FreeFromBackingDBO;
begin
  DestroyFromBackingDBO;
end;

procedure TFRE_DB_ObjectEx.Free;
begin
  if self<>nil then
    Finalize;
end;

function TFRE_DB_ObjectEx.GetDesc: IFRE_DB_TEXT;
begin
  if assigned(FNamedObject) then
    result := FNamedObject.GetDesc
  else
   raise EFRE_DB_Exception.Create(edb_ERROR,'the ex object does not support the named object interface');
end;

procedure TFRE_DB_ObjectEx.SetDesc(const AValue: IFRE_DB_TEXT);
begin
  if assigned(FNamedObject) then
    FNamedObject.SetDesc(AValue)
  else
   raise EFRE_DB_Exception.Create(edb_ERROR,'the ex object does not support the named object interface');
end;

function TFRE_DB_ObjectEx.GetName: TFRE_DB_String;
begin
  if assigned(FNamedObject) then
    result := FNamedObject.GetName
  else
   raise EFRE_DB_Exception.Create(edb_ERROR,'the ex object does not support the named object interface');
end;

procedure TFRE_DB_ObjectEx.SetName(const AValue: TFRE_DB_String);
begin
  if assigned(FNamedObject) then
    FNamedObject.SetName(AValue)
  else
   raise EFRE_DB_Exception.Create(edb_ERROR,'the ex object does not support the named object interface');
end;

function TFRE_DB_ObjectEx.UIDP: PByte;
begin
  result := FImplementor.UIDP;
end;

function TFRE_DB_ObjectEx.PUID: PGuid;
begin
  result := FImplementor.PUID;
end;

function TFRE_DB_ObjectEx.ObjectRoot: IFRE_DB_Object;
begin
  result := FImplementor.ObjectRoot;
end;


procedure TFRE_DB_ObjectEx.ForAllFields(const iter: IFRE_DB_FieldIterator);
begin
  FImplementor.ForAllFields(iter);
end;

procedure TFRE_DB_ObjectEx.ForAllFieldsBreak(const iter: IFRE_DB_FieldIteratorBrk);
begin
  FImplementor.ForAllFieldsBreak(iter);
end;

procedure TFRE_DB_ObjectEx.ForAllObjects(const iter: IFRE_DB_Obj_Iterator);
begin
  FImplementor.ForAllObjects(iter);
end;

procedure TFRE_DB_ObjectEx.ForAllObjectsFieldName(const iter: IFRE_DB_Obj_NameIterator);
begin
  FImplementor.ForAllObjectsFieldName(iter);
end;

function TFRE_DB_ObjectEx.UID: TGUID;
begin
  result := FImplementor.UID;
end;

function TFRE_DB_ObjectEx.UID_String: TFRE_DB_GUID_String;
begin
  result := FImplementor.UID_String;
end;

function TFRE_DB_ObjectEx.DomainID: TGUID;
begin
  result := FImplementor.DomainID;
end;

procedure TFRE_DB_ObjectEx.SetDomainID(const domid: TGUID);
begin
  FImplementor.SetDomainID(domid);
end;


function TFRE_DB_ObjectEx.Parent: IFRE_DB_Object;
begin
  result := FImplementor.Parent;
end;

function TFRE_DB_ObjectEx.ParentField: IFRE_DB_FIELD;
begin
  result := FImplementor.ParentField;
end;

function TFRE_DB_ObjectEx.AsString: TFRE_DB_String;
begin
  result := FImplementor.AsString;
end;

function TFRE_DB_ObjectEx.Field(const name: TFRE_DB_NameType): IFRE_DB_FIELD;
begin
  result := FImplementor.Field(name);
end;

function TFRE_DB_ObjectEx.FieldOnlyExistingObj(const name: TFRE_DB_NameType): IFRE_DB_Object;
begin
  result := FImplementor.FieldOnlyExistingObj(name);
end;

function TFRE_DB_ObjectEx.FieldOnlyExistingObject(const name: TFRE_DB_NameType; var obj: IFRE_DB_Object): boolean;
begin
  result := FImplementor.FieldOnlyExistingObject(name,obj);
end;

function TFRE_DB_ObjectEx.FieldOnlyExistingObjAs(const name: TFRE_DB_NameType; const classref: TFRE_DB_BaseClass; var outobj): boolean;
begin
  result := FImplementor.FieldOnlyExistingObjAs(name,classref,outobj);
end;

function TFRE_DB_ObjectEx.FieldOnlyExisting(const name: TFRE_DB_NameType; var fld: IFRE_DB_FIELD): boolean;
begin
  result := FImplementor.FieldOnlyExisting(name,fld);
end;

function TFRE_DB_ObjectEx.FieldPath(const name: TFRE_DB_String; const dont_raise_ex: boolean): IFRE_DB_FIELD;
begin
  result := FImplementor.FieldPath(name,dont_raise_ex);
end;

function TFRE_DB_ObjectEx.FieldPathCreate(const name: TFRE_DB_String): IFRE_DB_FIELD;
begin
  result := FImplementor.FieldPathCreate(name);
end;

function TFRE_DB_ObjectEx.FieldPathExists(const name: TFRE_DB_String): Boolean;
begin
  result := FImplementor.FieldPathExists(name);
end;

function TFRE_DB_ObjectEx.FieldPathListFormat(const field_list: TFRE_DB_NameTypeArray; const formats: TFRE_DB_String; const empty_val: TFRE_DB_String): TFRE_DB_String;
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
  result := Classname;
end;

function TFRE_DB_ObjectEx.IsA(const schemename: shortstring): Boolean;
begin
  result := FImplementor.IsA(schemename);
end;

function TFRE_DB_ObjectEx.IsA(const IsSchemeclass: TFRE_DB_OBJECTCLASSEX; var obj): Boolean;
begin
  result := FImplementor.IsA(IsSchemeclass,obj);
end;

function TFRE_DB_ObjectEx.PreTransformedWasA(const schemename: shortstring): Boolean;
begin
  result := FImplementor.PreTransformedWasA(schemename);
end;

function TFRE_DB_ObjectEx.PreTransformedScheme: ShortString;
begin
  result := FImplementor.PreTransformedScheme;
end;

function TFRE_DB_ObjectEx.IsObjectRoot: Boolean;
begin
  result := FImplementor.IsObjectRoot;
end;

procedure TFRE_DB_ObjectEx.SaveToFile(const filename: TFRE_DB_String);
begin
  FImplementor.SaveToFile(filename);
end;

function TFRE_DB_ObjectEx.ReferencesObjectsFromData: Boolean;
begin
  result := FImplementor.ReferencesObjectsFromData;
end;

function TFRE_DB_ObjectEx.ForAllObjectsBreakHierarchic(const iter: IFRE_DB_ObjectIteratorBrk): boolean;
begin
  result := FImplementor.ForAllObjectsBreakHierarchic(iter);
end;

function TFRE_DB_ObjectEx.FetchObjByUID(const childuid: TGuid; var obj: IFRE_DB_Object): boolean;
begin
  result := FImplementor.FetchObjByUID(childuid,obj);
end;

function TFRE_DB_ObjectEx.FetchObjWithStringFieldValue(const field_name: TFRE_DB_NameType; const fieldvalue: TFRE_DB_String; var obj: IFRE_DB_Object; ClassnameToMatch: ShortString): boolean;
begin
  result := FetchObjWithStringFieldValue(field_name,fieldvalue,obj,ClassnameToMatch);
end;

procedure TFRE_DB_ObjectEx.SetAllSimpleObjectFieldsFromObject(const source_object: IFRE_DB_Object);
begin
  FImplementor.SetAllSimpleObjectFieldsFromObject(source_object);
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

function TFRE_DB_ObjectEx.GetScheme(const raise_non_existing: boolean): IFRE_DB_SchemeObject;
begin
  result := FImplementor.GetScheme(raise_non_existing);
end;

procedure TFRE_DB_ObjectEx.Finalize;
begin
  //writeln('FINALIZE MEDIATOR CALL  ',ClassName);
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
  result := FImplementor.CloneToNewObject(generate_new_uids)
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


class function TFRE_DB_ObjectEx.NewOperation(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): TGUID;
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
  dbo              := GFRE_DBI.NewObjectSchemeByName(lSchemeclass);
  lSchemeObject.SetObjectFieldsWithScheme(data,dbo,true,dbc);
  dbo_uid       := dbo.UID;
  if lCollectionName='' then begin
    raise EFRE_DB_Exception.Create(edb_ERROR,'standard new operation requires a collection!');
  end else begin
    CheckDbResult(dbc.GetCollection(lCollectionName).Store(dbo),'failure on store/new collection='+lCollectionName);
  end;
  result := dbo_uid;
end;

constructor TFRE_DB_ObjectEx.CreateForDB;
var scheme   : IFRE_DB_SCHEMEOBJECT;
    bounddbo : IFRE_DB_Object;
begin
  //First find Scheme
  if not GFRE_DBI.GetSystemScheme(ClassType,scheme) then
    raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'the hardcodeclass [%s] was not found in schemes, maybe not registered ?',[ClassName]);
  if Scheme.IsA('TFRE_DB_NAMED_OBJECT') then
    begin
      bounddbo := GFRE_DBI.NewNamedObject;
    end
  else
    begin
      bounddbo := GFRE_DBI.NewObject;
    end;
  bounddbo._InternalSetMediatorScheme(self,scheme);
  FBound       := true;
  FImplementor := bounddbo;
  FImplementor.Supports(IFRE_DB_NAMED_OBJECT,FNamedObject);
  InternalSetup;
end;

procedure TFRE_DB_ObjectEx.CopyToMemory(memory: Pointer);
begin
  FImplementor.CopyToMemory(memory);
end;

class function TFRE_DB_ObjectEx.StoreTranslateableText(const conn: IFRE_DB_SYS_CONNECTION; const key: TFRE_DB_NameType; const short_text: TFRE_DB_String; const long_text: TFRE_DB_String; const hint_text: TFRE_DB_String): TFRE_DB_Errortype;
begin
  Result:=conn.StoreTranslateableText(GFRE_DBI.CreateText(GetTranslateableTextKey(key),short_text,long_text,hint_text));
end;

class function TFRE_DB_ObjectEx.DeleteTranslateableText(const conn: IFRE_DB_SYS_CONNECTION; const key: TFRE_DB_NameType): TFRE_DB_Errortype;
begin
  Result:=conn.DeleteTranslateableText(GetTranslateableTextKey(key));
end;

class function TFRE_DB_ObjectEx.GetTranslateableTextKey(const key: TFRE_DB_NameType): TFRE_DB_String;
begin
  Result:='$'+ClassName+'_'+key;
end;

class function TFRE_DB_ObjectEx.GetTranslateableTextShort(const conn: IFRE_DB_CONNECTION; const key: TFRE_DB_NameType): TFRE_DB_String;
begin
  Result:=conn.FetchTranslateableTextShort(GetTranslateableTextKey(key));
end;

class function TFRE_DB_ObjectEx.GetTranslateableTextLong(const conn: IFRE_DB_CONNECTION; const key: TFRE_DB_NameType): TFRE_DB_String;
begin
  Result:=conn.FetchTranslateableTextLong(GetTranslateableTextKey(key));
end;

class function TFRE_DB_ObjectEx.GetTranslateableTextHint(const conn: IFRE_DB_CONNECTION; const key: TFRE_DB_NameType): TFRE_DB_String;
begin
  Result:=conn.FetchTranslateableTextHint(GetTranslateableTextKey(key));
end;

function TFRE_DB_ObjectEx.GetInstanceRight(const right: TFRE_DB_NameType): IFRE_DB_RIGHT;
begin
  Result := '$'+uppercase(UID_String+'_'+right);
end;


//My instance gets freed on function termination (fetch, invoke method, free) tus we need to change our copy and feed the uptade with the copy of us.
function TFRE_DB_ObjectEx.WEB_SaveOperation(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var scheme            : IFRE_DB_SCHEMEOBJECT;
    update_object_uid : TGUid;
    raw_object        : IFRE_DB_Object;
begin
  if not conn.sys.CheckClassRight4Domain(sr_UPDATE,Self.ClassType,ses.GetDomain) then
    raise EFRE_DB_Exception.Create('Access denied.');
  if Not IsObjectRoot then begin
    result := TFRE_DB_MESSAGE_DESC.Create.Describe('SAVE','Error on saving! Saving of Subobject not supported!',fdbmt_error);
    exit;
  end;
  result            := nil;
  scheme            := GetScheme;
  update_object_uid := UID;
  raw_object        := input.Field('data').AsObject;
  //writeln('SAVEOP----------RAW OBJECT---------');
  //writeln(raw_object.DumpToString);
  //writeln('----------RAW OBJECT---------');
  scheme.SetObjectFieldsWithScheme(raw_object,self,false,conn);
  CheckDbResult(conn.Update(self.CloneToNewObject()),'failure on update');  // This instance is freed by now, so rely on the stackframe only (self) pointer is garbage(!!)
  result := TFRE_DB_CLOSE_DIALOG_DESC.Create.Describe();
end;

function TFRE_DB_ObjectEx.WEB_DeleteOperation(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var db_res       : TFRE_DB_Errortype;
begin
    case input.Field('confirmed').AsString of
      'true': begin
                if not IsObjectRoot then begin
                  result := TFRE_DB_MESSAGE_DESC.Create.Describe('DELETE','Error on deleting! Deleting of Subobject not supported!',fdbmt_error);
                end;
                db_res := conn.Delete(UID);
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
            result := TFRE_DB_MESSAGE_DESC.create.Describe('Delete','Are you sure?',fdbmt_confirm,CWSF(@self.WEB_DeleteOperation));
          end;
    end;
end;

class function TFRE_DB_ObjectEx.WBC_NewOperation(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  try
    NewOperation(input,ses,app,conn);
    result  := TFRE_DB_CLOSE_DIALOG_DESC.Create.Describe();
  except
   on E:Exception do begin
    result := TFRE_DB_MESSAGE_DESC.Create.Describe('NEW','Error on creating object ['+e.Message+']',fdbmt_error);
   end;
  end;
end;

function TFRE_DB_ObjectEx.WEB_NoteLoad(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var noteobj: IFRE_DB_Object;
begin
  if input.FieldExists('linkid') then begin
    if conn.GetCollection('SysNoteCollection').GetIndexedObj(input.Field('linkid').asstring,noteobj) then begin
      exit(TFRE_DB_EDITOR_DATA_DESC.create.Describe(noteobj.Field('note').asstring));
    end else begin
      exit(TFRE_DB_EDITOR_DATA_DESC.create.Describe(''));
    end;
  end else begin
    exit(TFRE_DB_EDITOR_DATA_DESC.create.Describe(''));
  end;
end;

function TFRE_DB_ObjectEx.WEB_NoteSave(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  noteobj: IFRE_DB_Object;
  res    : TFRE_DB_Errortype;
begin
  if input.FieldExists('linkid') then begin
    if conn.GetCollection('SysNoteCollection').GetIndexedObj(input.Field('linkid').asstring,noteobj) then begin
      noteobj.Field('note').asstring := input.Field('content').asstring;
      res := conn.Update(noteobj);
      if res<>edb_OK then
        raise EFRE_DB_Exception.Create(res,'error updating note');
    end else begin
      noteobj := GFRE_DBI.NewObjectScheme(TFRE_DB_NOTE);
      noteobj.Field('link').asstring:=input.Field('linkid').asstring;
      noteobj.Field('note').asstring  := input.Field('content').asstring;
      res := conn.GetCollection('SysNoteCollection').Store(noteobj);
      if res<>edb_OK then
        raise EFRE_DB_Exception.Create(res,'error storing note');
    end;
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_ObjectEx.WEB_NoteStartEdit(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_ObjectEx.WEB_NoteStopEdit(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
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

function TFRE_DB_SERVER_FUNC_DESC.InternalInvoke(const session: TFRE_DB_UserSession): IFRE_DB_Object;
var
  i        : Integer;
  key,value: String;
  newInput : IFRE_DB_Object;
begin
  newInput:=GFRE_DBI.NewObject;
  for i := 0 to Field('params').ValueCount - 1 do begin
    key:=Field('params').AsObjectArr[i].Field('key').AsString;
    value:=Field('params').AsObjectArr[i].Field('value').AsString;
    newInput.Field(key).AsString:=value;
  end;
  result:=session.InternalSessInvokeMethod(Field('class').AsString,Field('func').AsString,GFRE_DBI.StringArray2GuidArray(Field('uidPath').AsStringArr),newInput);
end;


function TFRE_DB_SERVER_FUNC_DESC.AddParam: TFRE_DB_PARAM_DESC;
begin
  Result:=TFRE_DB_PARAM_DESC.Create;
  Field('params').AddObject(Result);
end;

function TFRE_DB_APPLICATION.GetSitemapMainiconFilename: string;
begin
  result := Field('sm_mainicon_fn').asstring;
end;

function TFRE_DB_APPLICATION.GetSitemapMainiconSubpath: string;
begin
  result := Field('sm_icon_path').asstring;
end;

procedure TFRE_DB_APPLICATION.SetDescTranslationKey(const AValue: TFRE_DB_String);
begin
  Field('desc_tkey').AsString:=AValue;
end;

function TFRE_DB_APPLICATION.GetDescTranslationKey: TFRE_DB_String;
begin
  result := Field('desc_tkey').AsString;
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

procedure TFRE_DB_APPLICATION.InternalSetup;
begin
  inherited InternalSetup;
  SetupApplicationStructure;
end;


procedure TFRE_DB_APPLICATION.InitAppDesc(const descr_translation_key: TFRE_DB_String);
begin
  SetDescTranslationKey(descr_translation_key);
end;

function TFRE_DB_APPLICATION.AsObject: IFRE_DB_Object;
begin
  result := FImplementor;
end;


function TFRE_DB_APPLICATION.AppClassName: ShortString;
begin
 result := ClassName;
end;

function TFRE_DB_APPLICATION.IsMultiDomainApp: Boolean;
begin
  Result:=false;
end;

function TFRE_DB_APPLICATION.GetCaption(const ses: IFRE_DB_Usersession): TFRE_DB_String;
begin
  result := FetchAppTextShort(ses,'caption');
end;


procedure TFRE_DB_APPLICATION.SessionInitialize(const session: TFRE_DB_UserSession);

  function  _initSubModules(const field: IFRE_DB_FIELD):boolean;
  begin
    result := false;
    if (field.FieldType=fdbft_Object) and  field.AsObject.Supports(IFRE_DB_APPLICATION_MODULE) then
      begin
        (field.AsObject.Implementor_HC as TFRE_DB_APPLICATION_MODULE).MySessionInitializeModule(session);
      end;
  end;

begin
  MySessionInitialize(session);
  ForAllFieldsBreak(@_initSubModules);
end;

procedure TFRE_DB_APPLICATION.SessionFinalize(const session: TFRE_DB_UserSession);

  procedure _FinishSubModules(const field: IFRE_DB_FIELD);
  var app_module : TFRE_DB_APPLICATION_MODULE;
  begin
    if (field.FieldType=fdbft_Object) and  field.AsObject.Supports(IFRE_DB_APPLICATION_MODULE) then
      (field.AsObject.Implementor_HC as TFRE_DB_APPLICATION_MODULE).MySessionFinalizeModule(session);
  end;

begin
  ForAllFields(@_FinishSubmodules);
  MySessionFinalize(session);
end;

procedure TFRE_DB_APPLICATION.SessionPromotion(const session: TFRE_DB_UserSession);

  function  _initSubModules(const field: IFRE_DB_FIELD):boolean;
  var app_module : TFRE_DB_APPLICATION_MODULE;
  begin
    result:=false;
    if (field.FieldType=fdbft_Object) and  field.AsObject.Supports(IFRE_DB_APPLICATION_MODULE) then
        (field.AsObject.Implementor_HC as TFRE_DB_APPLICATION_MODULE).MySessionPromotionModule(session);
  end;

begin
  MySessionPromotion(session);
  ForAllFieldsBreak(@_initSubModules);
end;

procedure TFRE_DB_APPLICATION.SetSitemapMainiconFilename(AValue: string);
begin
  Field('sm_mainicon_fn').asstring := AValue;
end;

procedure TFRE_DB_APPLICATION.SetSitemapMainiconSubpath(AValue: string);
begin
  Field('sm_icon_path').asstring:=AValue;
end;

class procedure TFRE_DB_APPLICATION.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='UNUSED';
end;


function TFRE_DB_APPLICATION.IsContentUpdateVisible(const session: IFRE_DB_UserSession; const update_content_id: string): Boolean;
begin
  Result:=session.isUpdatableContentVisible(update_content_id);
end;


procedure TFRE_DB_APPLICATION.SetupApplicationStructure;
begin
end;

class procedure TFRE_DB_APPLICATION.CreateAppText(const conn: IFRE_DB_SYS_CONNECTION;const translation_key: TFRE_DB_String; const short_text: TFRE_DB_String; const long_text: TFRE_DB_String; const hint_text: TFRE_DB_String);
var txt :IFRE_DB_TEXT;
begin
  txt := GFRE_DBI.NewText('$'+uppercase(classname)+'_'+translation_key,long_text,short_text,hint_text);
  CheckDbResult(conn.StoreTranslateableText(txt),'CreateAppText ' + translation_key);
end;

class procedure TFRE_DB_APPLICATION.DeleteAppText(const conn: IFRE_DB_SYS_CONNECTION; const translation_key: TFRE_DB_String);
begin
  CheckDbResult(conn.DeleteTranslateableText('$'+uppercase(classname)+'_'+translation_key),'DeleteAppText ' + translation_key);
end;

class procedure TFRE_DB_APPLICATION.UpdateAppText(const conn: IFRE_DB_SYS_CONNECTION; const translation_key: TFRE_DB_String; const short_text: TFRE_DB_String; const long_text: TFRE_DB_String; const hint_text: TFRE_DB_String);
var txt :IFRE_DB_TEXT;
begin
  txt := GFRE_DBI.NewText('$'+uppercase(classname)+'_'+translation_key,long_text,short_text,hint_text);
  CheckDbResult(conn.UpdateTranslateableText(txt),'UpdateAppText ' + translation_key);
end;

class function TFRE_DB_APPLICATION.StdSidebarCaptionKey: TFRE_DB_String;
begin
  result := 'caption';
end;

class function TFRE_DB_APPLICATION.StdSitemapCaptionKey: TFRE_DB_String;
begin
  result := 'sitemap_main';
end;


function TFRE_DB_APPLICATION._FetchAppText(const session: IFRE_DB_UserSession; const translation_key: TFRE_DB_String): IFRE_DB_TEXT;
begin
  if not session.GetDBConnection.FetchTranslateableTextOBJ('$'+uppercase(classname)+'_'+translation_key,result) then
    begin
      Result := GFRE_DBI.CreateText('notfound',translation_key+'_short',translation_key+'_long',translation_key+'_is_missing!');
    end;
end;


procedure TFRE_DB_APPLICATION.AddApplicationModule(const module: TFRE_DB_APPLICATION_MODULE; const sitemap_key: string; const icon_path: string);
var FSubmoduleOrder : Integer;
begin
  Field(module.GetModuleClassName).AsObject := module;
  if FieldExists('SubModuleOrder') then begin
    FSubmoduleOrder:=Field('SubModuleOrder').AsInt32;
    inc(FSubModuleOrder);
  end else begin
    FSubModuleOrder:=1;
  end;
  Field('SubModuleOrder').AsInt32 := FSubmoduleOrder;
  Field(module.GetModuleClassName+'_O').AsInt16:=FSubModuleOrder;
  Field(module.GetModuleClassName+'_IFN').AsString:=icon_path;
end;

function TFRE_DB_APPLICATION.DelegateInvoke(const modulename: TFRE_DB_String; const methname: string; const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  if FieldExists(modulename) then begin
    result := Field(modulename).AsObject.Invoke(methname,input,nil,self,nil);
  end else begin
    raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'DelegateInvoke: Module [%s] not found!',[modulename]);
  end;
end;

procedure TFRE_DB_APPLICATION.MySessionInitialize(const session: TFRE_DB_UserSession);
begin
  if session.IsInteractiveSession then begin
    MyUpdateSitemap(session);
  end;
end;

procedure TFRE_DB_APPLICATION.MySessionPromotion(const session: TFRE_DB_UserSession);
begin
  if session.IsInteractiveSession then begin
    MyUpdateSitemap(session);
  end;
end;

procedure TFRE_DB_APPLICATION.MySessionFinalize(const session: TFRE_DB_UserSession);
begin

end;

procedure TFRE_DB_APPLICATION.MyServerInitialize(const admin_dbc: IFRE_DB_CONNECTION);
begin

end;

procedure TFRE_DB_APPLICATION.MyUpdateSitemap(const session: TFRE_DB_UserSession);
var
  SiteMapData     : IFRE_DB_Object;
  conn            : IFRE_DB_CONNECTION;
  moduleclasspath : TFRE_DB_String;
  keypath         : TFRE_DB_String;

    procedure BuildSiteMap(const module:IFRE_DB_APPLICATION_MODULE;const module_order:nativeint);
    var
      modulename         : shortstring;
      moduleclass        : shortstring;
      modl               : TFRE_DB_APPLICATION_MODULE;
      mymoduleclasspath  : TFRE_DB_String;
      oldkeypath         : TFRE_DB_String;
      oldmoduleclasspath : TFRE_DB_String;
    begin
      modl              := module.AsObject.Implementor_HC as TFRE_DB_APPLICATION_MODULE;
      moduleclass       := module.GetModuleClassName;
      if moduleclasspath='' then
        mymoduleclasspath := moduleclass
      else
        mymoduleclasspath := moduleclasspath+':'+moduleclass;
      FREDB_SiteMap_AddRadialEntry(SiteMapData,keypath+'/'+moduleclass,modl.FetchModuleTextShort(session,modl.StdSitemapModuleTitleKey),
                                   'images_apps/'+SiteMapIconSubPath+DirectorySeparator+module.GetSitemapIconFilename,mymoduleclasspath,0,
                                   conn.SYS.CheckClassRight4MyDomain(sr_FETCH,modl.ClassType));
      oldkeypath         := keypath;
      oldmoduleclasspath := moduleclasspath;
      keypath            := keypath+'/'+moduleclass;
      if moduleclasspath<>'' then
        moduleclasspath := moduleclasspath+':'+moduleclass
      else
        moduleclasspath := moduleclass;
      modl.ForAllAppModules(@BuildSiteMap);
      keypath         := oldkeypath;
      moduleclasspath := oldmoduleclasspath;
     end;

begin
  conn:=session.GetDBConnection;
  SiteMapData  := GFRE_DBI.NewObject;
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'MAIN',FetchAppTextShort(session,StdSitemapCaptionKey),'images_apps'+DirectorySeparator+SiteMapIconSubPath+DirectorySeparator+SiteMapMainIconFilename,'',0,true);
  moduleclasspath := '';
  keypath         := 'MAIN';
  ForAllAppModules(@BuildSiteMap);
  FREDB_SiteMap_RadialAutoposition(SiteMapData);
  session.GetSessionAppData(ClassName).Field('SITEMAP').AsObject := SiteMapData;
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

function TFRE_DB_APPLICATION.WEB_OnUIChange(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  ses.GetSessionAppData(classname).Field('activeSection').AsString:=input.Field('sectionid').AsString;
  Result:=GFRE_DB_NIL_DESC;
end;

class procedure TFRE_DB_APPLICATION.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.SetParentSchemeByName('TFRE_DB_NAMED_OBJECT');
  Scheme.Strict(false);
  Scheme.SetSysDisplayField(TFRE_DB_NameTypeArray.Create('objname','$DBTEXT:desc'),'%s - (%s)');
end;


procedure TFRE_DB_APPLICATION.Finalize;
begin
  free;
end;

//function TFRE_DB_APPLICATION.GetDescription(conn: IFRE_DB_CONNECTION): IFRE_DB_TEXT;
//begin
//  result := FetchAppText(conn,GetDescTranslationKey);
//end;

function TFRE_DB_APPLICATION.GetSessionData(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  result := GetSession(input).GetSessionGlobalData;
end;


//function TFRE_DB_APPLICATION.ObjectNameI: TFRE_DB_String;
//begin
//  result := GetName;
//end;

function TFRE_DB_APPLICATION.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var ActiveSection : String;
    appobj        : TFRE_DB_APPLICATION;
    res           : TFRE_DB_SUBSECTIONS_DESC;

    procedure DescribeAppModules(const module:IFRE_DB_APPLICATION_MODULE;const module_order:nativeint);
    var
      menu    : TFRE_DB_MENU_DESC;
      section : TFRE_DB_SECTION_DESC;
      csf     : TFRE_DB_SERVER_FUNC_DESC;
      title   : TFRE_DB_String;
      id      : String;
    begin
      if conn.sys.CheckClassRight4MyDomain(sr_FETCH,module.GetImplementorsClass) or
         (conn.sys.CheckClassRight4AnyDomain(sr_FETCH,module.GetImplementorsClass) and isMultiDomainApp) then
        begin
          csf   := TFRE_DB_SERVER_FUNC_DESC.Create.Describe(module.AsObject,'content');
          title := module.GetModuleTitle(ses);
          id    := module.GetModuleClassName;
          section:=TFRE_DB_SUBSECTIONS_DESC(res).AddSection.Describe(csf,title,module_order,id);
          menu:=TFRE_DB_MENU_DESC(module.GetToolbarMenu(ses));
          if Assigned(menu) then
            section.SetMenu(menu);
        end;
     end;

begin
  appobj  := self;
  res     := TFRE_DB_SUBSECTIONS_DESC.create.Describe;
  TFRE_DB_SUBSECTIONS_DESC(res).OnUIChange(CWSF(@WEB_OnUIChange));
  appobj.ForAllAppModules(@DescribeAppModules);
  ActiveSection := ses.GetSessionAppData(ClassName).Field('activeSection').AsString;
  TFRE_DB_SUBSECTIONS_DESC(res).SetActiveSection(ActiveSection);
  result := res;
end;

procedure TFRE_DB_APPLICATION.AddAppToSiteMap(const session: TFRE_DB_UserSession; const parent_entry: TFRE_DB_CONTENT_DESC);
//var app         : TFRE_DB_APPLICATION;
var    res         : TFRE_DB_SITEMAP_DESC;
    parent_e    : TFRE_DB_SITEMAP_ENTRY_DESC;
    sitemapdata : IFRE_DB_Object;
    ientry      : integer;

  procedure BuildSM(const entry:IFRE_DB_Object);
  var caption,
       icon,id    : String;
       x,y,i,nc   : integer;
       scale      : Single;
       dis        : Boolean;
     next_lev     : TFRE_DB_SITEMAP_ENTRY_DESC;
     old_par      : TFRE_DB_SITEMAP_ENTRY_DESC;
     ial          : TFRE_DB_StringArray;
     oial         : TFRE_DB_StringArray;
     isubentry    : integer;
  begin
     caption  := entry.Field('CAP').AsString;
     id       := entry.Field('ID').AsString;
     nc       := entry.Field('NC').AsInt16;
     icon     := entry.Field('ICO').AsString;
     x        := entry.Field('CRD').AsInt32Arr[0];
     y        := entry.Field('CRD').AsInt32Arr[1];
     oial     := entry.Field('IAL').AsStringArr;
     scale    := entry.Field('SCL').AsReal32;
     dis      := entry.Field('DIS').AsBoolean;
     SetLength(ial,length(oial)+3);
     ial[0]   := 'Home';
     ial[1]   := 'AppContainer';
     ial[2]   := AppClassName;
     oial     := entry.Field('IAL').AsStringArr;
     for i:=0 to high(oial) do begin
       ial[i+3] := oial[i];
     end;
     old_par  := parent_e;
     next_lev := parent_e.AddEntry.Describe(caption,icon,TFRE_DB_RESTORE_UI_DESC.create.Describe('FirmOSViewport',ial),x,y,id,nc,dis,scale);
     if true then begin
       parent_e := next_lev;
       for isubentry := 0 to entry.Field('ENTRIES').valuecount-1 do begin
         BuildSM(entry.Field('ENTRIES').AsObjectItem[isubentry]);
       end;
       parent_e := old_par;
     end;
  end;

begin
  parent_e    := parent_entry as TFRE_DB_SITEMAP_ENTRY_DESC;
  SiteMapData := session.GetSessionAppData(ClassName).FieldOnlyExistingObj('SITEMAP');
  if assigned(sitemapdata) then
    for ientry := 0 to sitemapdata.Field('ENTRIES').ValueCount-1 do
     BuildSM(sitemapdata.Field('ENTRIES').AsObjectItem[ientry]);
end;


function TFRE_DB_APPLICATION.ShowInApplicationChooser(const session: IFRE_DB_UserSession): Boolean;
begin
  result := true;
end;


function TFRE_DB_APPLICATION.FetchAppTextShort(const session: IFRE_DB_UserSession; const translation_key: TFRE_DB_String): TFRE_DB_String;
var txt : IFRE_DB_TEXT;
begin
  txt := _FetchAppText(session,translation_key);
  result := txt.Getshort;
  txt.Finalize;
end;

function TFRE_DB_APPLICATION.FetchAppTextLong(const session: IFRE_DB_UserSession; const translation_key: TFRE_DB_String): TFRE_DB_String;
var txt : IFRE_DB_TEXT;
begin
  txt := _FetchAppText(session,translation_key);
  result := txt.GetLong;
  txt.Finalize;
end;

function TFRE_DB_APPLICATION.FetchAppTextHint(const session: IFRE_DB_UserSession; const translation_key: TFRE_DB_String): TFRE_DB_String;
var txt : IFRE_DB_TEXT;
begin
  txt := _FetchAppText(session,translation_key);
  result := txt.GetHint;
  txt.Finalize;
end;

function TFRE_DB_APPLICATION.FetchAppTextFull(const session: IFRE_DB_UserSession; const translation_key: TFRE_DB_String): IFRE_DB_TEXT;
begin
  result := _FetchAppText(session,translation_key);
end;

{ TFRE_DB_APPLICATION_MODULE }

procedure TFRE_DB_APPLICATION_MODULE.InternalSetup;
begin
  SetupAppModuleStructure;
end;

procedure TFRE_DB_APPLICATION_MODULE.SetSitemapIconFilename(AValue: TFRE_DB_String);
begin
  Field('sm_icon_fn').asstring := AValue;
end;

procedure TFRE_DB_APPLICATION_MODULE.SetupAppModuleStructure;
begin

end;

procedure TFRE_DB_APPLICATION_MODULE.AddApplicationModule(const module: TFRE_DB_APPLICATION_MODULE);
var FSubmoduleOrder : Integer;
begin
  Field(module.GetModuleClassName).AsObject := module;
  if FieldExists('SubModuleOrder') then begin
    FSubmoduleOrder:=Field('SubModuleOrder').AsInt32;
    inc(FSubModuleOrder);
  end else begin
    FSubModuleOrder:=1;
  end;
  Field('SubModuleOrder').AsInt32 := FSubmoduleOrder;
  Field(module.GetModuleClassName+'_O').AsInt16:=FSubModuleOrder;
end;


procedure TFRE_DB_APPLICATION_MODULE.InitModuleDesc(const descr_translation_key: TFRE_DB_String);
begin
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

procedure TFRE_DB_APPLICATION_MODULE.CheckClassVisibility4AnyDomain(const session: IFRE_DB_UserSession);
begin
  if not session.GetDBConnection.sys.CheckClassRight4AnyDomain(sr_FETCH,ClassType) then
    raise EFRE_DB_Exception.Create(GetEmbeddingapp.FetchAppTextShort(session,'error_no_access'));
end;

procedure TFRE_DB_APPLICATION_MODULE.CheckClassVisibility4MyDomain(const session: IFRE_DB_UserSession);
begin
  if not session.GetDBConnection.sys.CheckClassRight4MyDomain(sr_FETCH,ClassType) then
    raise EFRE_DB_Exception.Create(GetEmbeddingapp.FetchAppTextShort(session,'error_no_access'));
end;

class procedure TFRE_DB_APPLICATION_MODULE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId := 'UNUSED';
end;

class function TFRE_DB_APPLICATION_MODULE.StdSitemapModuleTitleKey: TFRE_DB_String;
begin
  result := '$SMTK_'+ClassName;
end;

class procedure TFRE_DB_APPLICATION_MODULE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  if ClassName<>'TFRE_DB_APPLICATION_MODULE' then
    scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;


procedure TFRE_DB_APPLICATION_MODULE.MySessionInitializeModule(const session: TFRE_DB_UserSession);

  procedure _initSubModules(const field: IFRE_DB_FIELD);
  var app_module : IFRE_DB_APPLICATION_MODULE;
  begin
    if field.IsObjectField and field.AsObject.Supports(IFRE_DB_APPLICATION_MODULE,app_module) then begin
      try
        (field.AsObject.Implementor_HC as TFRE_DB_APPLICATION_MODULE).MySessionInitializeModule(session);
      except
        on e:Exception do
          begin
            GFRE_DBI.LogError(dblc_SESSION,'SESSION INITIALIZATION FAILED SESS(%s) %s : %s ',[session.GetSessionID,classname,e.Message]);
          end;
      end;
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


function TFRE_DB_APPLICATION_MODULE.GetEmbeddingApp: TFRE_DB_APPLICATION;
var obj:IFRE_DB_Object;
begin
  obj := self;
  repeat
    obj := obj.Parent;
    if obj.IsA(TFRE_DB_APPLICATION.ClassName) then begin
      exit(obj.Implementor_HC as TFRE_DB_APPLICATION);
    end;
  until false;
end;

function TFRE_DB_APPLICATION_MODULE._FetchAppText(const session: IFRE_DB_UserSession; const translation_key: TFRE_DB_String): IFRE_DB_TEXT;
begin
  result := GetEmbeddingApp._FetchAppText(session,translation_key);
end;

function TFRE_DB_APPLICATION_MODULE.GetToolbarMenu(const ses: IFRE_DB_Usersession): TFRE_DB_CONTENT_DESC;
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
  result := GetSession(input).GetSessionModuleData(ClassName);
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

procedure TFRE_DB_APPLICATION_MODULE.SetDescrTranslationKey(const val: TFRE_DB_String);
begin
  field('mod_desc_key').AsString:=val;
end;

function TFRE_DB_APPLICATION_MODULE.GetDescrTranslationKey: TFRE_DB_String;
begin
  Result := field('mod_desc_key').AsString;
end;

function TFRE_DB_APPLICATION_MODULE.GetModuleTitle(const ses: IFRE_DB_Usersession): TFRE_DB_String; { TODO: Change all Modules to getstddescrkey }
var txt : IFRE_DB_TEXT;
    key : string;
begin
  key    :=  GetDescrTranslationKey;
  txt    :=  FetchModuleTextFull(ses,key);
  if txt.GetTKey='notfound' then
    begin
      txt.Finalize;
      txt :=  GetEmbeddingApp._FetchAppText(ses,GetDescrTranslationKey);
    end;
  if txt.GetTKey='notfound' then
    begin
      txt.Finalize;
      txt :=  FetchModuleTextFull(ses,StdSitemapModuleTitleKey);
    end;
  result := txt.Getshort;
  txt.Finalize;
end;

function TFRE_DB_APPLICATION_MODULE.GetModuleClassName: Shortstring;
begin
  result := ClassName;
end;


function TFRE_DB_APPLICATION_MODULE.AsObject: IFRE_DB_Object;
begin
  result := FImplementor;
end;

function TFRE_DB_APPLICATION_MODULE.IsContentUpdateVisible(const session: IFRE_DB_UserSession; const update_content_id: string): Boolean;
begin
  result := GetEmbeddingApp.IsContentUpdateVisible(session,update_content_id);
end;

function TFRE_DB_APPLICATION_MODULE._FetchModuleText(const session: IFRE_DB_UserSession; const translation_key: TFRE_DB_String): IFRE_DB_TEXT;
begin
  if not session.GetDBConnection.FetchTranslateableTextOBJ('$'+uppercase(classname)+'_'+translation_key,result) then
    begin
      Result := GFRE_DBI.CreateText('notfound',translation_key+'_short',translation_key+'_long',translation_key+'_is_missing!');
    end;
end;

function TFRE_DB_APPLICATION_MODULE.FetchModuleTextShort(const session: IFRE_DB_UserSession; const translation_key: TFRE_DB_String): TFRE_DB_String;
var txt : IFRE_DB_TEXT;
begin
  txt := _FetchModuleText(session,translation_key);
  result := txt.Getshort;
  txt.Finalize;
end;

function TFRE_DB_APPLICATION_MODULE.FetchModuleTextLong(const session: IFRE_DB_UserSession; const translation_key: TFRE_DB_String): TFRE_DB_String;
var txt : IFRE_DB_TEXT;
begin
  txt := _FetchModuleText(session,translation_key);
  result := txt.GetLong;
  txt.Finalize;
end;

function TFRE_DB_APPLICATION_MODULE.FetchModuleTextHint(const session: IFRE_DB_UserSession; const translation_key: TFRE_DB_String): TFRE_DB_String;
var txt : IFRE_DB_TEXT;
begin
  txt := _FetchModuleText(session,translation_key);
  result := txt.GetHint;
  txt.Finalize;
end;

function TFRE_DB_APPLICATION_MODULE.FetchModuleTextFull(const session: IFRE_DB_UserSession; const translation_key: TFRE_DB_String): IFRE_DB_TEXT;
begin
  result := _FetchModuleText(session,translation_key);
end;

function TFRE_DB_APPLICATION_MODULE.GetSitemapIconFilename: TFRE_DB_String;
begin
  result := Field('sm_icon_fn').asstring;
  if result='' then
    result := GetEmbeddingApp.SiteMapMainIconFilename;
end;

class procedure TFRE_DB_APPLICATION_MODULE.CreateModuleText(const conn: IFRE_DB_SYS_CONNECTION; const translation_key: TFRE_DB_String; const short_text: TFRE_DB_String; const long_text: TFRE_DB_String; const hint_text: TFRE_DB_String);
var txt :IFRE_DB_TEXT;
begin
  txt := GFRE_DBI.NewText('$'+uppercase(classname)+'_'+translation_key,long_text,short_text,hint_text);
  CheckDbResult(conn.StoreTranslateableText(txt),'CreateModuleText ' + translation_key);
end;

class procedure TFRE_DB_APPLICATION_MODULE.DeleteModuleText(const conn: IFRE_DB_SYS_CONNECTION; const translation_key: TFRE_DB_String);
begin
  CheckDbResult(conn.DeleteTranslateableText('$'+uppercase(classname)+'_'+translation_key),'DeleteModuleText ' + translation_key);
end;

class procedure TFRE_DB_APPLICATION_MODULE.UpdateModuleText(const conn: IFRE_DB_SYS_CONNECTION; const translation_key: TFRE_DB_String; const short_text: TFRE_DB_String; const long_text: TFRE_DB_String; const hint_text: TFRE_DB_String);
var txt :IFRE_DB_TEXT;
begin
  txt := GFRE_DBI.NewText('$'+uppercase(classname)+'_'+translation_key,long_text,short_text,hint_text);
  CheckDbResult(conn.UpdateTranslateableText(txt),'UpdateModuleText ' + translation_key);
end;

function TFRE_DB_APPLICATION_MODULE.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var ActiveSection : String;
    res           : TFRE_DB_SUBSECTIONS_DESC;

    procedure DescribeAppModules(const module:IFRE_DB_APPLICATION_MODULE;const module_order:nativeint);
    var
      menu    : TFRE_DB_MENU_DESC;
      section : TFRE_DB_SECTION_DESC;
      csf     : TFRE_DB_SERVER_FUNC_DESC;
      title   : TFRE_DB_String;
      id      : String;
    begin
      if conn.sys.CheckClassRight4MyDomain(sr_FETCH,module.GetImplementorsClass) or
         (conn.sys.CheckClassRight4AnyDomain(sr_FETCH,module.GetImplementorsClass) and app.isMultiDomainApp) then
        begin
          csf   := TFRE_DB_SERVER_FUNC_DESC.Create.Describe(module.AsObject,'content');
          title := module.GetModuleTitle(ses);
          id    := module.GetModuleClassName;
          section:=TFRE_DB_SUBSECTIONS_DESC(res).AddSection.Describe(csf,title,module_order,id);
          menu:=TFRE_DB_MENU_DESC(module.GetToolbarMenu(ses));
          if Assigned(menu) then
            section.SetMenu(menu);
        end;
     end;

begin
  res     := TFRE_DB_SUBSECTIONS_DESC.create.Describe;
  TFRE_DB_SUBSECTIONS_DESC(res).OnUIChange(CWSF(@WEB_OnUIChange));
  ForAllAppModules(@DescribeAppModules);
  ActiveSection := ses.GetSessionAppData(ClassName).Field('activeSection').AsString;
  TFRE_DB_SUBSECTIONS_DESC(res).SetActiveSection(ActiveSection);
  result := res;
end;

function TFRE_DB_APPLICATION_MODULE.WEB_OnUIChange(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
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
  FREDB_SeperateString(InterAppLink,':',ial); { class:class:class }
  SiteMapEntry.Field('IAL').AsStringArr := ial;
  SiteMapEntry.Field('SCL').AsReal32    := 1.0;
  SiteMapEntry.Field('DIS').AsBoolean   := not enabled;
end;

procedure FREDB_SiteMap_RadialAutoposition(const SiteMapData: IFRE_DB_Object; rootangle:integer);
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
      dec(currangle,partangle);
      if currangle<0 then begin
        currangle := currangle + 360;
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
      currangle := maxangle;
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
  rootangle  := 90 - rootangle;  // start at 12h, positive angle clockwise
  SiteMapRoot:=SiteMapData.Field('ENTRIES').AsObjectItem[0];
  if assigned(SiteMapRoot) then begin
    // Position RootNode
    SiteMapRoot.Field('CRD').AsInt32Arr  := TFRE_DB_Int32Array.Create(xo,yo);
    SiteMapRoot.Field('SCL').AsReal32    := 1.0;
    SiteMapRoot.Field('PNGL').asint32    := -1;    // Place Children in full circle
    PlaceChildren(SiteMapRoot);
  end;
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

function FREDB_GuidArray2StringStream(const arr: TFRE_DB_GUIDArray): String;
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


function FREDB_StreamString2GuidArray(str: string): TFRE_DB_GUIDArray;
var sa : TFOSStringArray;
     i : NativeInt;
     s : string;
begin
  str:=trim(str);
  s := copy(str,2,Length(str)-2);
  GFRE_BT.SeperateString(s,',',sa);
  SetLength(result,length(sa));
  for i:= 0 to high(sa) do
    result[i] := GFRE_BT.HexString_2_GUID(sa[i]);
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

function FREDB_GetDboAsBufferLen(const dbo: IFRE_DB_Object; var mem: Pointer): UInt32;
var len : UInt32;
    ns  : UInt32;
begin
  ns := dbo.NeededSize;
  Getmem(mem,ns+4);
  dbo.CopyToMemory(mem+4);
  PCardinal(mem)^:=ns;
  result := ns+4;
end;

procedure FREDB_SetStringFromExistingFieldPathOrNoChange(const obj: IFRE_DB_Object; const fieldpath: string; var string_fld: TFRE_DB_String);
begin
  if obj.FieldPathExists(fieldpath) then
    string_fld := obj.FieldPath(fieldpath).AsString
  else
    string_fld := cFRE_DB_SYS_NOCHANGE_VAL_STR;
end;

function FREDB_HCV(const txt: TFRE_DB_String): TFRE_DB_String;
begin
  if txt<>cFRE_DB_SYS_CLEAR_VAL_STR then
    result := txt
  else
    result := '';
end;

function FREDB_IniLogCategory2LogCategory(const ini_logcategory: string): TFRE_DB_LOGCATEGORY;
begin
  for result in TFRE_DB_LOGCATEGORY do begin
    if CFRE_DB_LOGCATEGORY_INI_IDENT[result]=ini_logcategory then exit;
  end;
  raise EFRE_DB_Exception.Create('invalid db logcategory specifier : ['+ini_logcategory+']');
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

procedure FREDB_ApplyNotificationBlockToNotifIF(const block: IFRE_DB_Object; const deploy_if: IFRE_DB_DBChangedNotification; var layer: TFRE_DB_NameType);
var cmd   : ShortString;
    objs  : IFRE_DB_ObjectArray;
    i     : NativeInt;

begin
  //writeln('----DUMP NOTIFY BLOCK');
  //writeln(block.DumpToString());
  //writeln('----DUMP NOTIFY BLOCK');
  objs  := block.Field('N').AsObjectArr;
  layer := block.Field('L').AsString;
  try
    for i:=0 to High(objs) do
      with objs[i] do
        begin
          cmd   := Field('C').AsString;
          try
            case cmd of
              'CC'  : deploy_if.CollectionCreated(Field('CC').AsString);
              'CD'  : deploy_if.CollectionDeleted(Field('CC').AsString);
              'IC'  : deploy_if.IndexDefinedOnField(Field('CC').AsString,Field('FN').AsString,FREDB_FieldtypeShortString2Fieldtype(Field('FT').AsString),Field('UI').AsBoolean,Field('IC').AsBoolean,Field('IN').AsString,Field('AN').AsBoolean,Field('UN').AsBoolean);
              'ID'  : deploy_if.IndexDroppedOnField(Field('CC').AsString,Field('IN').AsString);
              'OS'  : deploy_if.ObjectStored(Field('CC').AsString,Field('obj').CheckOutObject);
              'OU'  : deploy_if.ObjectUpdated(Field('obj').CheckOutObject,Field('CC').AsStringArr);
              'OD'  : deploy_if.ObjectDeleted(Field('obj').CheckOutObject);
              'OR'  : deploy_if.ObjectRemoved(Field('CC').AsString,Field('obj').CheckOutObject);
              'FD'  : deploy_if.FieldDelete(Field('FLD').AsObject._InternalDecodeAsField); { Field is created new .. free it in the deploy if }
              'FA'  : deploy_if.FieldAdd(Field('FLD').AsObject._InternalDecodeAsField);    { Field is created new .. free it in the deploy if }
              'FC'  : deploy_if.FieldChange(Field('FLDO').AsObject._InternalDecodeAsField,Field('FLDN').AsObject._InternalDecodeAsField); { Field is created new .. free it in the deploy if }
              'SOS' : deploy_if.SubObjectStored(Field('SO').CheckOutObject,Field('SOFN').AsString,Field('SOUP').AsGUIDArr);
              'SOD' : deploy_if.SubObjectDeleted(Field('SO').CheckOutObject,Field('SOFN').AsString,Field('SOUP').AsGUIDArr);
              'SOL' : deploy_if.SetupOutboundRefLink  (field('FO').AsGUID,field('TO').CheckOutObject,field('KD').AsString);
              'SIL' : deploy_if.SetupInboundRefLink   (field('FO').AsObject,field('TO').AsGUID,field('KD').AsString);
              'DOL' : deploy_if.OutboundReflinkDropped(field('FO').AsGUID,field('TO').CheckOutObject,field('KD').AsString);
              'DIL' : deploy_if.InboundReflinkDropped (field('FO').CheckOutObject,field('TO').AsGUID,field('KD').AsString);
              'DUS' : deploy_if.DifferentiallUpdStarts(field('O').CheckOutObject);
              'DUE' : deploy_if.DifferentiallUpdEnds(field('O').AsGUID);
              else
                raise EFRE_DB_Exception.Create(edb_ERROR,'undefined block notification encoding : '+cmd);
            end;
          except on
            e:exception do
              begin
                GFRE_DBI.LogError(dblc_PERSISTANCE_NOTIFY,'APPLYNOTIFBLOCK [%s] failed due to [%s] on Step [%d of %d]',[cmd,e.Message,i,High(objs)]);
              end;
          end;
        end;
  finally
    layer := '';
  end;
end;

procedure FREDB_ApplyNotificationBlockToNotifIF_Connection(const block: IFRE_DB_Object; const deploy_if: IFRE_DB_DBChangedNotificationConnection);
var cmd   : ShortString;
    key   : TFRE_DB_String;
    objs  : IFRE_DB_ObjectArray;
    i     : NativeInt;

begin
  objs  := block.Field('N').AsObjectArr;
  key := block.Field('KEY').AsString;
  for i:=0 to High(objs) do
    with objs[i] do
      begin
        cmd   := Field('C').AsString;
        case cmd of
          'CC'  : deploy_if.CollectionCreated(Field('CC').AsString);
          'CD'  : deploy_if.CollectionDeleted(Field('CC').AsString);
          'IC'  : deploy_if.IndexDefinedOnField(Field('CC').AsString,Field('FN').AsString,FREDB_FieldtypeShortString2Fieldtype(Field('FT').AsString),Field('UI').AsBoolean,Field('IC').AsBoolean,Field('IN').AsString,Field('AN').AsBoolean,Field('UN').AsBoolean);
          'ID'  : deploy_if.IndexDroppedOnField(Field('CC').AsString,Field('IN').AsString);
        end;
      end;
end;

procedure FREDB_ApplyNotificationBlockToNotifIF_Session(const block: IFRE_DB_Object; const deploy_if: IFRE_DB_DBChangedNotificationSession);
var cmd   : ShortString;
    objs  : IFRE_DB_ObjectArray;
    i     : NativeInt;

begin
  objs  := block.Field('N').AsObjectArr;
  for i:=0 to High(objs) do
    with objs[i] do
      begin
        cmd   := Field('C').AsString;
        case cmd of
          'FD'  : deploy_if.FieldDelete(Field('FLD').AsObject._InternalDecodeAsField);
          'FA'  : deploy_if.FieldAdd(Field('FLD').AsObject._InternalDecodeAsField);
          'FC'  : deploy_if.FieldChange(Field('FLDO').AsObject._InternalDecodeAsField,Field('FLDN').AsObject._InternalDecodeAsField);
          'DUS' : deploy_if.DifferentiallUpdStarts(field('O').AsObject);
          'DUE' : deploy_if.DifferentiallUpdEnds(field('O').AsGUID);
        //  else
        //    raise EFRE_DB_Exception.Create(edb_ERROR,'undefined block notification encoding : '+cmd);
        end;
      end;
end;

function FREDB_CompareTransCollDataKeys(const a, b: TFRE_DB_TRANS_COLL_DATA_KEY): boolean;
begin
  result := (a.Collname  = b.Collname ) and
            (a.DC_Name   = b.DC_Name  ) and
            (a.filterkey = b.filterkey) and
            (a.orderkey  = b.orderkey ) and
            (a.RL_Spec   = b.RL_Spec  );
end;


function FREDB_PP_GetParentIDHelper_Hack(const obj: IFRE_DB_Object; var pid: string): boolean;
var ppa : TFRE_DB_StringArray;
    l   : NativeInt;
begin
  result := false;
  ppa := FREDB_PP_GetParentPaths(obj);
  l   := Length(ppa);
  if l<>1 then
    raise EFRE_DB_Exception.Create(edb_ERROR,'expected parent path lenght=1 not %d',[l]);
  pid := GFRE_BT.SepRight(ppa[0],',');
  if pid='' then
    pid := ppa[0];
  if pid<>'' then
    exit(true);
end;

function FREDB_PP_ObjectInParentPath(const obj: IFRE_DB_Object; const pp: string): boolean;
var ppa : TFRE_DB_StringArray;
    fld : IFRE_DB_Field;
begin
  if obj.FieldOnlyExisting(cFRE_DB_SYS_PARENT_PATH_FULL,fld) then
    begin
      ppa    := fld.AsStringArr;
      result := FREDB_StringInArray(pp,ppa);
    end
  else
    result := false;
end;

function FREDB_PP_ObjectInParentPathLastParent(const obj: IFRE_DB_Object; const pp: string): boolean;
var ppa   : TFRE_DB_StringArray;
    fld   : IFRE_DB_Field;
    ppart : TFRE_DB_String;
    i     : NativeInt;
begin
  result := false;
  if obj.FieldOnlyExisting(cFRE_DB_SYS_PARENT_PATH_FULL,fld) then
    begin
      ppa    := fld.AsStringArr;
      for i := 0 to high(ppa) do
        begin
          ppart := GFRE_BT.SepRight(ppa[i],',');
          if ppart='' then
            ppart := ppa[i];
          if pp=ppart then
            exit(true);
        end;
    end;
end;

procedure FREDB_PP_AddParentPathToObj(const obj: IFRE_DB_Object; const pp: string);
var ppa   : TFRE_DB_StringArray;
    fld   : IFRE_DB_Field;
    ppart : string;

begin
  ppa := obj.Field(cFRE_DB_SYS_PARENT_PATH_FULL).AsStringArr;
  if FREDB_StringInArray(pp,ppa) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'double add to parentpath try - failed');
  SetLength(ppa,Length(ppa)+1);

  ppa[high(ppa)] := pp;
  obj.Field(cFRE_DB_SYS_PARENT_PATH_FULL).AsStringArr := ppa;

  //var
  //    tr_obj.Field(cFRE_DB_SYS_PARENT_PATH_FULL).AsString := pp;
  //    ppart := GFRE_BT.SepRight(pp,',');
  //    if ppart='' then
  //      ppart := pp;
  //    tr_obj.Field(cFRE_DB_SYS_PARENT_PATH_PART).AsString := ppart;
end;

function FREDB_PP_GetParentPaths(const obj: IFRE_DB_Object): TFRE_DB_StringArray;
var fld : IFRE_DB_Field;
begin
  if obj.FieldOnlyExisting(cFRE_DB_SYS_PARENT_PATH_FULL,fld) then
    result := fld.AsStringArr
  else
    result := nil;
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
    procedure RegisterNewExtension    (const extension_name : String ; const MetaRegistrationFunction : IFRE_DB_EXTENSION_RegisterCB ; const MetaRegisterInitDBFunction : IFRE_DB_EXTENSION_INITDB_CB;  const MetaRegisterRemoveFunction : IFRE_DB_EXTENSION_REMOVE_CB ; const MetaGentestdata : IFRE_DB_EXTENSION_INITDB_CB = nil ; const MetaGenUnitTest : IFRE_DB_EXTENSION_INITDB_CB = nil);
    procedure RegisterExtensions4DB   (const list:IFOS_STRINGS);
    procedure InitDatabase4Extensions (const list: IFOS_STRINGS ; const db:string ; const user,pass:string);
    procedure GenerateTestData4Exts   (const list: IFOS_STRINGS ; const db:string ; const user,pass:string);
    procedure GenerateUnitTestsdata   (const list: IFOS_STRINGS ; const db:string ; const user,pass:string);
    procedure Remove4Extensions       (const list: IFOS_STRINGS ; const db:string ; const user,pass:string);
  end;

  { TFRE_DB_EXTENSION_GRP }

  TFRE_DB_EXTENSION_GRP = class(TObject,IFRE_DB_EXTENSION_GRP)
  private
    FCallBack         : IFRE_DB_EXTENSION_RegisterCB;
    FCallBackDB       : IFRE_DB_EXTENSION_INITDB_CB;
    FCallBackREM      : IFRE_DB_EXTENSION_REMOVE_CB;
    FCallBAckTestdata : IFRE_DB_EXTENSION_INITDB_CB;
    FCallbackUnitTest : IFRE_DB_EXTENSION_INITDB_CB;
    Fname             : TFRE_DB_String;
  public
    function   GetExtensionName                 : TFRE_DB_String;
    procedure  RegisterExtensionAndDependencies ;
    procedure  InitializeDatabaseForExtension    (const db_name : string ; const user,pass:string);
    procedure  GenerateTestdataForExtension      (const db_name : string ; const user,pass:string);
    procedure  DoUnitTestforExtension            (const db_name : string ; const user,pass:string);
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

procedure TFRE_DB_EXTENSION_GRP.GenerateTestdataForExtension(const db_name: string; const user, pass: string);
begin
  if Assigned(FCallBAckTestdata) then
    FCallBAckTestdata(db_name,user,pass);
end;

procedure TFRE_DB_EXTENSION_GRP.DoUnitTestforExtension(const db_name: string; const user, pass: string);
begin
  if Assigned(FCallbackUnitTest) then
    FCallbackUnitTest(db_name,user,pass);
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

procedure TFRE_DBI_REG_EXTMGR.RegisterNewExtension(const extension_name: String; const MetaRegistrationFunction: IFRE_DB_EXTENSION_RegisterCB; const MetaRegisterInitDBFunction: IFRE_DB_EXTENSION_INITDB_CB; const MetaRegisterRemoveFunction: IFRE_DB_EXTENSION_REMOVE_CB; const MetaGentestdata: IFRE_DB_EXTENSION_INITDB_CB; const MetaGenUnitTest: IFRE_DB_EXTENSION_INITDB_CB);
var i       :  integer;
    new_ext : TFRE_DB_EXTENSION_GRP;
begin
  for i := 0 to FExtlist.Count-1 do begin
    with FExtlist[i] as TFRE_DB_EXTENSION_GRP do begin
      if lowercase(GetExtensionName) = lowercase(extension_name) then exit; // already registerd
    end;
  end;
  new_ext                    := TFRE_DB_EXTENSION_GRP.Create;
  new_ext.Fname              := extension_name;
  new_ext.FCallBack          := MetaRegistrationFunction;
  new_ext.FCallBackDB        := MetaRegisterInitDBFunction;
  new_ext.FCallBackREM       := MetaRegisterRemoveFunction;
  new_ext.FCallBAckTestdata  := MetaGentestdata;
  new_ext.FCallbackUnitTest  := MetaGenUnitTest;
  FExtlist.Add(new_ext);
end;

procedure TFRE_DBI_REG_EXTMGR.RegisterExtensions4DB(const list: IFOS_STRINGS);
  procedure Iterate(const ext : IFRE_DB_EXTENSION_GRP);
  begin
    if list.IndexOf(ext.GetExtensionName)>-1 then begin
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
      ext.InitializeDatabaseForExtension(db,user,pass);
    end;
  end;

begin
  list.SetCaseSensitive(false);
  ForAllExtensions(@Iterate);
end;

procedure TFRE_DBI_REG_EXTMGR.GenerateTestData4Exts(const list: IFOS_STRINGS; const db: string; const user, pass: string);
  procedure Iterate(const ext : IFRE_DB_EXTENSION_GRP);
  begin
    if list.IndexOf(ext.GetExtensionName)>-1 then begin
      ext.GenerateTestdataForExtension(db,user,pass);
    end;
  end;
begin
 list.SetCaseSensitive(false);
 ForAllExtensions(@Iterate);
end;

procedure TFRE_DBI_REG_EXTMGR.GenerateUnitTestsdata(const list: IFOS_STRINGS; const db: string; const user, pass: string);
  procedure Iterate(const ext : IFRE_DB_EXTENSION_GRP);
  begin
    if list.IndexOf(ext.GetExtensionName)>-1 then begin
      ext.DoUnitTestforExtension(db,user,pass);
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

