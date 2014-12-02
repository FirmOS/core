unit fos_tool_interfaces;

{
(§LIC)
  (c) Autor,Copyright Dipl.Ing.- Helmut Hartl
      FirmOS Business Solutions GmbH
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

interface

{$MODE objfpc} {$H+}
{$modeswitch nestedprocvars}

uses Classes,Sysutils;

type
    TFOS_VALUETOKEN=record
      val:integer;
      nam:string;
    end;

    TFRE_OS_Result=record
      OS_Errorcode   : Integer;
      OS_ErrorDetail : String;
    end;

    EFOS_Stable_Error     = (fse_OK,fse_FAIL,fse_FORMAT_INVALID,fse_NODE_NOT_SET,fse_APP_NOT_SET,fse_PART_NOT_SET,fse_OBJ_NOT_SET,fse_NEW);
    TFOS_BoolType         = (fbtFalse,fbtTrue,fbtNotSet);
    TFOS_PropType         = (ptNotFound,ptNull,ptBool,ptByte,ptShortInt,ptSmallInt,ptWord,ptLongInt,ptLongword,ptInt64,
                             ptQWord,ptSingle,ptDouble,ptCurrency,ptDateTime,ptString,ptStream,ptPS,ptMemoryObject,
                             ptBoolArray,ptByteArray,ptShortintArray,ptSmallintArray,ptWordArray,ptLongintArray,ptLongWordArray,
                             ptInt64Array,ptQWordArray,ptSingleArray,ptDoubleArray,ptCurrencyArray,ptStringArray,
                             _str_BoolTrue=250,_str_BoolFalse=251,_str_PropList=252,_str_PL_Mark=253,_str_StreamEnd=255);
    TFOS_LOG_RULE_ACTION  = (flra_LogToOnConsole,flra_DropEntry);
    TFOS_LOG_LEVEL        = (fll_Invalid, fll_Emergency, fll_Alert, fll_Critical, fll_Error, fll_Warning, fll_Notice, fll_Info, fll_Debug);
    TFOS_LOG_FACILITY     = (flf_Invalid, flf_Kernel, flf_User, flf_Mail, flf_Daemon, flf_Auth, flf_Syslog, flf_Lpr, flf_News, flf_UUCP, flf_Cron, flf_AuthPriv, flf_FTP,
                             flf_Local0, flf_Local1, flf_Local2, flf_Local3, flf_Local4, flf_Local5, flf_Local6, flf_Local7);

    TFRE_DB_DateTime64 = int64; // Milliseconds since epoch

  const
    CFOS_LOG_LEVEL      : Array[TFOS_LOG_LEVEL]         of string  = ('INVALID', 'EMERGENCY', 'ALERT', 'CRITICAL', 'ERROR', 'WARNING', 'NOTICE', 'INFO', 'DEBUG');
    CFOS_LOG_FACILITY   : Array[TFOS_LOG_FACILITY]      of string  = ('INVALID', 'KERNEL', 'USER', 'MAIL', 'DAEMON', 'AUTH', 'SYSLOG', 'LPR', 'NEWS', 'UUCP', 'CRON', 'AUTHPRIV', 'FTP',
                                                                      'LOCAL0', 'LOCAL1', 'LOCAL2', 'LOCAL3', 'LOCAL4', 'LOCAL5', 'LOCAL6', 'LOCAL7');
    CFOS_LOG_RULE_ACTION : Array[TFOS_LOG_RULE_ACTION]  of string  = ('LOGCONSOLE','DROPENTRY');

    cFRE_DBT_1_SEC   =  1000;
    cFRE_DBT_1_MIN   =  60 * cFRE_DBT_1_SEC;
    cFRE_DBT_1_HOUR  =  60 * cFRE_DBT_1_MIN;
    cFRE_DBT_1_DAY   =  24 * cFRE_DBT_1_HOUR;
    cFRE_DBT_1_WK    =   7 * cFRE_DBT_1_DAY;
    cFRE_DBT_1_YEAR  = 365 * cFRE_DBT_1_DAY;

  type

    TFOSBoolArray     = array of Boolean;
    TFOSByteArray     = array of Byte;
    TFOSShortIntArray = array of ShortInt;
    TFOSSmallintArray = array of Smallint;
    TFOSWordArray     = array of Word;
    TFOSLongwordArray = array of Longword;
    TFOSLongintArray  = array of LongInt;
    TFOSInt64Array    = array of Int64;
    TFOSQWordArray    = array of QWord;
    TFOSSingleArray   = array of Single;
    TFOSDoubleArray   = array of Double;
    TFOSCurrencyArray = array of Currency;
    TFOSStringArray   = array of AnsiString;

    PFOSBoolArray     = ^TFOSBoolArray;
    PFOSByteArray     = ^TFOSByteArray;
    PFOSShortIntArray = ^TFOSShortIntArray;
    PFOSSmallintArray = ^TFOSSmallintArray;
    PFOSWordArray     = ^TFOSWordArray;
    PFOSLongIntArray  = ^TFOSLongintArray;
    PFOSLongWordArray = ^TFOSLongwordArray;
    PFOSInt64Array    = ^TFOSInt64Array;
    PFOSQWordArray    = ^TFOSQWordArray;
    PFOSSingleArray   = ^TFOSSingleArray;
    PFOSDoubleArray   = ^TFOSDoubleArray;
    PFOSCurrencyArray = ^TFOSCurrencyArray;
    PFOSStringArray   = ^TFOSStringArray;

    RFOS_PROPERTY = packed record
      PropType:TFOS_PropType;
      case Byte of
        0:  (T_Bool           : Boolean);
        1:  (T_Int            : Int64);
        2:  (T_UInt           : QWord);
        3:  (T_Single         : Single);
        4:  (T_Double         : Double);
        5:  (T_Currency       : Currency);
        6:  (T_Date           : Double);
        11: (T_PString        : PString);
        12: (T_Stream         : TMemoryStream);
        13: (T_PS             : Pointer);
        14: (T_Object         : TObject);
        15: (T_PBoolArray     : PFOSBoolArray);
        16: (T_PByteArray     : PFOSByteArray);
        17: (T_PShortIntArray : PFOSShortIntArray);
        18: (T_PSmallintArray : PFOSSmallintArray);
        19: (T_PWordArray     : PFOSWordArray);
        21: (T_PLongWordArray : PFOSLongWordArray);
        22: (T_PLongintArray  : PFOSLongintArray);
        23: (T_PInt64Array    : PFOSInt64Array);
        24: (T_PQWordArray    : PFOSQWordArray);
        25: (T_PSingleArray   : PFOSSingleArray);
        26: (T_PDoubleArray   : PFOSDoubleArray);
        27: (T_PCurrencyArray : PFOSCurrencyArray);
        31: (T_PStringArray   : PFOSStringArray)
    end;


    TFRE_SimpleCallback        = procedure of object;
    TFRE_SimpleCallbackNested  = procedure is nested;
    TFOS_NameIterator          = procedure(lname:string) is nested;

    TFOS_NPS_Compression=(ncl_NONE,ncl_FAST,ncl_MAX);

    PFOS_NPS=^IFOS_NPS;
    IFOS_NPS=interface
      procedure   _LoadfromStream(const str: TStream); // Internal
      procedure   _SavetoStream(const str: TStream);   // Internal
      procedure   _SaveToPS(const ps:IFOS_NPS);        // Internal
      procedure   Clear;
      function    GetProp(const name:ansistring;out value:RFOS_PROPERTY):boolean;overload;
      function    FindNxtPrv(var search_found_name:ansistring;out value:RFOS_PROPERTY;const next:boolean):boolean;
      function    FindFirst(out found_name:ansistring;out value:RFOS_PROPERTY):boolean;overload;
      function    FindLast (out found_name:ansistring;out value:RFOS_PROPERTY):boolean;overload;
      function    PropValToString(const val:RFOS_PROPERTY):ansistring;
      function    GetProp(const name:ansistring;out value:ansistring):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:boolean):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:Byte):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:ShortInt):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:Smallint):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:Word):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:LongInt):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:LongWord):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:Int64):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:QWord):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:Single):TFOS_PropType;overload;
      function    GetPropDouble(const name:ansistring;out value:Double):TFOS_PropType;overload;
      function    GetPropCurrency(const name:ansistring;out value:Currency):TFOS_PropType;overload;
      function    GetPropDateTime(const name:ansistring;out value:TDateTime):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:TFOSBoolArray):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:TFOSByteArray):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:TFOSShortIntArray):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:TFOSSmallintArray):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:TFOSWordArray):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:TFOSLongWordArray):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:TFOSLongintArray):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:TFOSInt64Array):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:TFOSQWordArray):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:TFOSSingleArray):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:TFOSDoubleArray):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:TFOSCurrencyArray):TFOS_PropType;overload;
      function    GetProp(const name:ansistring;out value:TFOSStringArray):TFOS_PropType;overload;
      function    GetPropBool    (const name:ansistring):TFOS_BoolType;
      function    GetPropString  (const name:ansistring):string;
      function    GetPropByte    (const name:ansistring):Byte;
      function    GetPropShortInt(const name:ansistring):ShortInt;
      function    GetPropSmallInt(const name:ansistring):SmallInt;
      function    GetPropWord    (const name:ansistring):Word;
      function    GetPropLongint (const name:ansistring):Longint;
      function    GetPropLongword(const name:ansistring):LongWord;
      function    GetPropInt64   (const name:ansistring):Int64;
      function    GetPropQWord   (const name:ansistring):QWord;
      function    GetPropSingle  (const name:ansistring):Single;
      function    GetPropDouble  (const name:ansistring):Double;
      function    GetPropCurrency(const name:ansistring):Currency;
      function    GetPropDateTime(const name:ansistring):TDateTime;
      function    GetPropNPS     (const name:ansistring):IFOS_NPS;
      function    FindProp(const name:ansistring):TFOS_PropType;
      function    GetPropStream(const name:ansistring;out str:TStream;const new_copy:boolean=false):TFOS_PropType; // new_copy=Makes a new Copy of The Stream else Reference
      function    AccessStream(const name:ansistring):TMemorystream; // gives access to a internal stream (creates if nonexistent)
      function    GetPropNPS(const name:ansistring;var PS:IFOS_NPS):TFOS_PropType;
      function    GetPropObj(const name:ansistring;out Obj:TObject):TFOS_PropType;
      function    CheckOutObj(const name:ansistring;out Obj:TObject):TFOS_PropType; // Object is OWNED
      function    CheckOutNPS(const name:ansistring;out PS:IFOS_NPS):TFOS_PropType;
      function    DelProp(const name:ansistring):TFOS_PropType;
      procedure   SetPropNull(const name:ansistring);
      procedure   SetProp(const name:ansistring;const value:ansistring);overload;
      procedure   SetProp(const name:ansistring;const value:boolean);overload;
      procedure   SetProp(const name:ansistring;const value:Byte);overload;
      procedure   SetProp(const name:ansistring;const value:ShortInt);overload;
      procedure   SetProp(const name:ansistring;const value:Smallint);overload;
      procedure   SetProp(const name:ansistring;const value:Word);overload;
      procedure   SetProp(const name:ansistring;const value:LongInt);overload;
      procedure   SetProp(const name:ansistring;const value:LongWord);overload;
      procedure   SetProp(const name:ansistring;const value:Int64);overload;
      procedure   SetProp(const name:ansistring;const value:QWord);overload;
      procedure   SetProp(const name:ansistring;const value:TFOSBoolArray);overload;
      procedure   SetProp(const name:ansistring;const value:TFOSByteArray);overload;
      procedure   SetProp(const name:ansistring;const value:TFOSShortIntArray);overload;
      procedure   SetProp(const name:ansistring;const value:TFOSSmallintArray);overload;
      procedure   SetProp(const name:ansistring;const value:TFOSWordArray);overload;
      procedure   SetProp(const name:ansistring;const value:TFOSLongWordArray);overload;
      procedure   SetProp(const name:ansistring;const value:TFOSLongintArray);overload;
      procedure   SetProp(const name:ansistring;const value:TFOSInt64Array);overload;
      procedure   SetProp(const name:ansistring;const value:TFOSQWordArray);overload;
      procedure   SetProp(const name:ansistring;const value:TFOSSingleArray);overload;
      procedure   SetProp(const name:ansistring;const value:TFOSDoubleArray);overload;
      procedure   SetProp(const name:ansistring;const value:TFOSCurrencyArray);overload;
      procedure   SetProp(const name:ansistring;const value:TFOSStringArray);overload;
      procedure   SetPropStream(const name:ansistring;const str:TStream);
      procedure   SetPropNPS(const name:ansistring;const PS:IFOS_NPS);
      procedure   SetPropObj(const name:ansistring;const Obj:TObject); // Speichert keine Objekte persistent !!! Löscht sie nicht beim Clear,Free,Reassign -> Object is NOT OWNED
      procedure   SetPropDateTime(const name:ansistring;const value:TDateTime);overload;
      procedure   SetProp(const name:ansistring;const value:Single);overload;
      procedure   SetPropDouble(const name:ansistring;const value:Double);overload;
      procedure   SetPropCurrency(const name:ansistring;const value:Currency);overload;
      function    AddPropListPS(const name:ansistring;const PS:IFOS_NPS):integer;
      procedure   RemoveProplistEntry(const name:ansistring;const idx: integer);
      procedure   ChangePropListPositons(const name:ansistring;const idx1,idx2: integer);
      procedure   InsertAtPosition(const name:ansistring;const idx:integer;const before:boolean;const PS:IFOS_NPS); // inserts after that Position
      procedure   GetPropListEntryPL(const name: ansistring; const idx: integer;out PL:IFOS_NPS);
      procedure   CheckOutPropListEntryPL(const name: ansistring; const idx: integer;out PL:IFOS_NPS); // Removes From List but does not Free it !!!!
      procedure   SetPropListEntryPL(const name: ansistring; const idx: integer;const PL:IFOS_NPS);
      function    ClearPropList(const name:ansistring):boolean;
      function    PropListCount(const name:ansistring):integer; // Get Number of PL entrys !
      function    PropCount:integer; // Get Real Properties Count without lists Lists;
      function    PropListsCount:integer; // Get Number of Proplists
      function    PropListName(const i:integer):ansistring;
      procedure   Dump(const sl:TStringlist;const indent: integer = 0);
      function    DumpText(const indent: integer = 0):ansistring;
      procedure   LoadfromStream(const str: TStream;const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring='');
      procedure   LoadfromFile(const name:ansistring;const cached:boolean=false;const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring='');
      procedure   SavetoStream(const str: TStream;const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring='');
      procedure   SavetoFile(const name:ansistring;const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring='');
      function    AsAnsiString(const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring=''):Ansistring;
      procedure   SetFromAnsiString(const s:Ansistring;const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring='');
      procedure   SaveToPS(const target:IFOS_NPS);
      function    GetCompression: TFOS_NPS_Compression;
      procedure   SetCompression(const AValue: TFOS_NPS_Compression);
      property    Compression:TFOS_NPS_Compression read GetCompression write SetCompression;
    end;

    IFOS_LNPS=interface(IFOS_NPS)
      procedure   Acquire;
      procedure   Release;
    end;

   TFOS_PS_Notify       = procedure (const CMD:IFOS_NPS) of object;

   { IFOS_STRINGS }

   IFOS_STRINGS=interface
   ['{783FBDB9-5F03-47B1-A372-D5B2683689F8}']
    function  Add               (const S: string): Integer;
    procedure AddStrings        (const strings: IFOS_STRINGS);
    procedure AddToStrings      (const strings: TStrings);
    function  GetItems(idx: integer): string;
    procedure ReplaceStrings    (const strings: TStrings);
    procedure Clear             ;
    procedure Delete            (Index: Integer);
    procedure Exchange          (Index1, Index2: Integer);
    function  Find              (const S: string; var Index: Integer): Boolean;
    function  IndexOf           (const S: string): Integer;
    procedure Insert            (Index: Integer; const S: string);
    procedure SetItems(idx: integer; const AValue: string);
    procedure Sort              ;
    procedure SetDuplicates     (const dup:TDuplicates);
    function  Duplicates        :TDuplicates;
    procedure SetSorted         (const srt:boolean);
    function  Sorted            :Boolean;
    procedure SetCaseSensitive  (const casesense:boolean);
    function  CaseSensitive     :Boolean;
    procedure SetCommatext      (const txt:string);
    function  Commatext         :String;
    procedure SetDelimitedText  (const txt:string);
    function  DelimitedText     :String;
    procedure SetText           (const txt:String);
    function  Text              :String;
    procedure SetQuoteChar      (const qc:Char);
    function  QuoteChar         :Char;
    function  AsTStrings        : TStrings;
    procedure SetLineBreakStyle (const lbs:TTextLineBreakStyle);
    function  LineBreakStyle    :TTextLineBreakStyle;
    function  Count             :Integer;
    procedure SetS              (const index:Integer;const txt:String);
    function  S                 (const index:Integer):string;
    property  Items             [idx:integer]:string read GetItems write SetItems;default;
   end;

   { IFOS_BASIC_TOOLS }
   {$INTERFACES CORBA}
   IFOS_BASIC_TOOLS=interface
    function  HashFast32                (const mem : PByte ; const len :NativeUint ; const seed : cardinal=0):cardinal;
    function  HashFast32_Hex            (const value: ansistring; const seed: cardinal=0): ansistring;
    function  HashString_MD5            (const Value: ansistring): ansistring;
    function  HashString_MD5_HEX        (const Value: ansistring): ansistring;
    function  HMAC_MD5                  (const Text: ansistring; Key: ansistring): ansistring;
    function  HMAC_MD5_HEX              (const Text: ansistring; Key: ansistring): ansistring;

    function  Str2HexStr                (const Value: ansistring): ansistring;
    function  HexStr2Str                (const Value: ansistring): ansistring;
    function  Mem2HexStr                (const Value : PByte ; const len:integer): ansistring;
    function  Min                       (const A, B:  integer): integer;
    function  Max                       (const A, B:  integer): integer;
    function  RatioPercent              (const A, B:  Double):Double;
    function  ByteToString              (const byte:  QWord): String;

    function  SepLeft                   (const Value, Delimiter: Ansistring): Ansistring;
    function  SepRight                  (const Value, Delimiter: Ansistring): Ansistring;
    function  ValToken2Str              (const Value: integer;   const TokArr:Array of TFOS_VALUETOKEN;const unknown:string=''):String;
    function  BitToken2Str              (const Value: integer;   const TokArr:Array of TFOS_VALUETOKEN):String;

    procedure SeperateString            (const value,sep:string ; var Strings:TFOSStringArray);
    function  CombineString             (const strings:TFOSStringArray; const sep:string):string;
    function  FindStringIndexInArray    (const txt:string;const strings:TFOSStringArray):integer;

    function  SplitString               (var   Value: AnsiString; const Delimiter: Ansistring): Ansistring;

    procedure RaiseStableError          (const ec:EFOS_Stable_Error;const detail:ansistring='');

    procedure CriticalAbort             (const msg:string;const ExceptionBacktrace:boolean=false;const halt_loop:boolean=false);overload;
    procedure CriticalAbort             (const msg:string;params:Array of Const;const ExceptionBacktrace:boolean=false;const halt_loop:boolean=false);overload;

    function  DumpExceptionsBacktrace   :string;
    function  DumpCurrentBacktrace      :string;
    function  Dump_Binary               (p:pointer;const len:cardinal;const no_lineending:boolean=false;const with_address:boolean=false):string;
    function  PadTo                     (const len:cardinal;const pad:cardinal):cardinal;
    function  Padrest                   (const len:cardinal;const pad:cardinal):cardinal;

    function  CreateGUID                :TGUID;
    function  CreateGUID_String         :AnsiString;
    function  CreateGuid_HEX            :Ansistring;
    function  GUID_2_HexString          (const g:TGUID)  :ShortString;
    function  HexString_2_GUID          (const hs:ShortString):TGuid;

    function  SHA1String                (const input:string):ShortString;
    function  Base64Encode              (const input:string):String;
    function  Base64Decode              (const input:string):String;
    function  CalcSaltedSH1Password     (const pw:String ; const salt : string):string;
    function  VerifySaltedSHA1Password  (const pw:string;const ssha_scheme:string):boolean;

    procedure ActivateJack              (const timeout:NativeInt=10000);
    procedure DeactivateJack            ;

    procedure List_Directorys           (basepath:string;const list:IFOS_STRINGS;const levels:cardinal=1;const with_basepath:boolean=true);
    procedure List_Files                (basepath:string;const list: IFOS_STRINGS; const levels: cardinal; const with_basepath: boolean);
    function  Delete_Directory          (folder:ansistring):boolean;

    procedure List_Files                (basepath:string;const file_iterator:TFOS_NameIterator);

    procedure EncryptAESStreamCBC128    (const Source: ansistring;const Key,InitVector: ansistring; out Dest: ansistring); overload;
    procedure EncryptAESStreamCBC192    (const Source: ansistring;const Key,InitVector: ansistring; out Dest: ansistring); overload;
    procedure EncryptAESStreamCBC256    (const Source: ansistring;const Key,InitVector: ansistring; out Dest: ansistring); overload;

    procedure DecryptAESStreamCBC128    (const Source: ansistring;const Key,InitVector: ansistring; out Dest: ansistring); overload;
    procedure DecryptAESStreamCBC192    (const Source: ansistring;const Key,InitVector: ansistring; out Dest: ansistring); overload;
    procedure DecryptAESStreamCBC256    (const Source: ansistring;const Key,InitVector: ansistring; out Dest: ansistring); overload;


    procedure DecryptAESStreamCBC128    (const Source: TStream; const Key,InitVector: ansistring; const Dest: TStream); overload;
    procedure DecryptAESStreamCBC192    (const Source: TStream; const Key,InitVector: ansistring; const Dest: TStream); overload;
    procedure DecryptAESStreamCBC256    (const Source: TStream; const Key,InitVector: ansistring; const Dest: TStream); overload;

    procedure EncryptAESStreamCBC128    (const Source: TStream; const Key,InitVector: ansistring; const Dest: TStream); overload;
    procedure EncryptAESStreamCBC192    (const Source: TStream; const Key,InitVector: ansistring; const Dest: TStream); overload;
    procedure EncryptAESStreamCBC256    (const Source: TStream; const Key,InitVector: ansistring; const Dest: TStream); overload;
    function  Get_Ticks_us              :qword;
    function  Get_Ticks_ms              :cardinal;
    function  Get_DBTimeNow             :TFRE_DB_DateTime64;

    function  StringFromFile            (const filename:string):string;
    procedure StringToFile              (const filename:string;const s:string);
    function  GetUserName               :String;
    function  GetUserDir                :String;
    function  GetTempDir                :String;
   end;

   {$INTERFACES COM}
   IFOS_STABLE_STREAM=interface
    ['{71DA13DE-C607-45F0-AEF2-79595C6DA5BA}']
    function  Read                       (var Buffer; Count: Longint): Longint;
    function  Write                      (const Buffer; Count: Longint): Longint;
    function  ReadAnsiString             :String;
    procedure WriteAnsiString            (const S : String);
    function  SeekCurrent                (const Offset: Int64): Int64;
    function  SeekBeginning              (const Offset: Int64): Int64;
    function  SeekEnd                    (const Offset: Int64): Int64;
    function  GetPosition                :Int64;
    procedure SetPosition                (const Offset: Int64);
    function  Stream                     :TStream;
   end;

   {$INTERFACES CORBA}
   IFOS_LOCK=interface
    ['FOS_LOCK']
     procedure   Acquire;
     function    Release:QWord;
     procedure   Finalize;
   end;

   IFOS_RW_LOCK=interface
    ['{7652B653-F81D-11DD-8529-001B38430791}']
    procedure   AcquireRead;
    procedure   ReleaseRead;
    procedure   AcquireWrite;
    procedure   ReleaseWrite;
   end;

   IFOS_TE=interface
    ['{782020EA-F81D-11DD-8529-001B38430791}']
    procedure   WaitFor(timeout:integer);
    function    SetEvent:boolean;
    procedure   Finalize;
   end;

   IFOS_DATA_TE=interface(IFOS_TE)
    ['{7B310A72-F81D-11DD-8529-001B38430791}']
    procedure   SetData(const data:string);
    procedure   SetData2(const data:string);
    procedure   SetData3(const data:string);
    procedure   SetData4(const data:string);
    function    GetData:string;
    function    GetData2:string;
    function    GetData3:string;
    function    GetData4:string;
   end;


   IFOS_SEM=interface
    ['{7E2E6C11-F81D-11DD-8529-001B38430791}']
    function    wait:boolean;
    function    signal:boolean;
   end;

   IFOS_E=interface
    ['FOS_E']
    procedure   WaitFor;
    procedure   SetEvent;
    procedure   Finalize;
   end;

   IFOS_LFQ=interface
    ['FOS_LFQ']
    procedure   Push(AItem: Pointer);
    function    Pop: Pointer;
    function    SomethingOnQ  : NativeInt;
    procedure   Finalize;
   end;

 {$INTERFACES CORBA}
  IFOS_FILE_LOGGER=interface
    procedure Sync_Logger;
    procedure LogConsole       (const msg:ShortString);
    procedure LogEmergency     (const msg:ShortString);
    procedure Log              (const msg,cat:ShortString;Level:TFOS_LOG_LEVEL=fll_Debug;const target:ShortString='';const sync:boolean=false);overload;
    procedure Log              (const msg:ShortString;params:array of const;cat:ShortString;Level:TFOS_LOG_LEVEL=fll_Debug;const target:ShortString='';const sync:boolean=false);overload;
    procedure LogSystem        (const msg:ShortString;const facility: TFOS_LOG_FACILITY; const level: TFOS_LOG_LEVEL);overload;
    procedure RegisterCategory (const cat:ShortString;filename:ShortString;turnaround:integer=-1;generations:integer=-1;const minseveritylevel:TFOS_LOG_LEVEL=fll_Debug;const nolog:TFOS_BoolType=fbtNotSet;const not_in_full_log:TFOS_BoolType=fbtNotSet);
    procedure RegisterTarget   (const target:ShortString;subfilepath:ShortString;turnaround:integer=-1;generations:integer=-1;const minseveritylevel:TFOS_LOG_LEVEL=fll_Debug;const facility:TFOS_LOG_FACILITY=flf_User);
    procedure RegisterThread   (const Name:ShortString);
    procedure SetDefaults      (const defaultfilename:ShortString;fullfilename,basedir:ShortString;const turnaround,generations:cardinal; const minseveritylevel:TFOS_LOG_LEVEL=fll_Debug ;  const facility: TFOS_LOG_FACILITY=flf_Kernel );
    procedure ClearLogRules    ;
    procedure AddRule          (const category:ShortString;const level:TFOS_LOG_LEVEL;const target:ShortString;const action:TFOS_LOG_RULE_ACTION;const stop_rule:boolean=true); //level=fll_Invalid = Ignore Level (All)
    procedure SetLocalZone     (const zone:ShortString);
    procedure EnableSyslog     ;
    procedure DisableSyslog    ;
 end;

  {$INTERFACES CORBA}
   IFOS_JobScheduler=interface
    function  AddJob(const key:string;const schedule:string):integer;
    function  DeleteJob(const key:string):integer;
    procedure ListJobs(out joblist:string);
    function  Schedule(out joblist:string):integer; overload;
    function  Schedule(out joblist:string; const pointintime:TDateTime):integer; overload;
    function  RunJob(const key:string):integer;
    function  FinishedJob(const key:string):integer;
    function  JobDetails(const key:string;out details:string):integer;
    function  JobNextTime(const key:string;out nexttime:TDateTime):integer;
   end;


   {$INTERFACES CORBA}
   IFOS_CPU=interface
     procedure TestSetup       (const packages,cores,logical:cardinal);
     function Bind_to_logical (const logical_id:integer;out error:string):boolean;
     function Get_CPU_Data   :IFOS_NPS;
     function Packages:cardinal;
     function Cores   :cardinal;
     function Logical :cardinal;
   end;

   {$INTERFACES CORBA}
   IFOS_TOOL_FACTORY=interface
      procedure    Get_Event(out E:IFOS_E);
      procedure    Get_TimedEvent(out TE:IFOS_TE);
      procedure    Get_TimedDataEvent(out TE:IFOS_DATA_TE);
      procedure    Get_Lock(out LOCK:IFOS_LOCK;const with_timing : boolean=false);
      procedure    Get_RW_Lock(out RWL:IFOS_RW_LOCK);
      procedure    Get_LFQ(out LFQ:IFOS_LFQ);
      procedure    Get_NPS(out NPS:IFOS_NPS);
      //function     Get_Stable_Storage(const Node:Ansistring='';const App:Ansistring='';const Part:Ansistring=''):IFOS_STABLE_STORAGE;
      function     Get_FOS_Strings:IFOS_STRINGS;overload;
      function     GetDefaultIniFileName:String;
      function     ReadDefaultIniValue(const section,value,default:string):String;
   end;

   TFRE_TZ_Iterator = procedure(const zonename, timezoneletters : String ; const sec_offset,fixed_offset : integer ; const valid_to_year,valid_to_month,valid_to_day,valid_to_secs_in_day:Integer ; const valid_until_gmt : boolean) is nested;

   {$INTERFACES CORBA}
    IFOS_DATETOOLS=interface
    //"float" datetime
        function  DateTimeToDBDateTime64 (const ADateTime: TDatetime): TFRE_DB_DateTime64;
        function  DBDateTime64ToDateTime (const dbdate      : TFRE_DB_DateTime64): TDateTime;
    //FOS Datetime
        function  LocalTimeToUTC         (const ADateTime64 : TFRE_DB_DateTime64;const FLocalZone:string): TFRE_DB_DateTime64;
        function  UTCToLocalTime         (const ADateTime64 : TFRE_DB_DateTime64;const FLocalZone:string): TFRE_DB_DateTime64;
        function  ToStrUTC               (const dt: TFRE_DB_DateTime64): string;
        function  ToStrFOS               (const dt: TFRE_DB_DateTime64): string;
        function  ToStrHTTP              (const dt: TFRE_DB_DateTime64): string; //RFC1123
        function  FromHttp               (const datestring:String):TFRE_DB_DateTime64;
        function  WeekDayOfDT            (const dt: TFRE_DB_DateTime64):integer; // 1=Sun .. 7=Sat
        function  Now_UTC                :TFRE_DB_DateTime64;
        procedure DecodeTime             (const date:TFRE_DB_DateTime64;var year,month,day,hour,minute,second,millisecond:longint);
        function  EncodeTime             (const year,month,day,hour,minute,second,millisecond:longint):TFRE_DB_DateTime64;
    //Timezones
        procedure GetTimeZoneNames       (const AZones: TStringList; const AOnlyGeoZones: Boolean=true);
        procedure ForAllTimeZones        (const iter: TFRE_TZ_Iterator; const only_geozones: boolean);
    end;


   { TFOS_NonRefCounted_IF }
   TFOS_NonRefCounted_Object=class(TObject)
    {$HINTS OFF}
    function  QueryInterface(const iid : tguid;out obj) : longint;stdcall;
    function  _AddRef : longint;stdcall;
    function  _Release : longint;stdcall;
    {$HINTS ON}
   end;

   { TFOS_ReadOnlyMemStream }

   TFOS_ReadOnlyMemStream = class(TCustomMemoryStream)
   protected
   public
     constructor Create (Ptr: Pointer; ASize: PtrInt);
     function    Write (const Buffer; Count: LongInt): LongInt; override;
   end;


   // -> Remove From Production CODE !
   procedure E_FOS_Implement; // Raise the IMPLEMENT this Error
   procedure E_FOS_TestNosey; // Raise the TestNosy Error
   procedure _Invalid_Continuation_Break;

   function FRE_GetBit   (const Val: Byte;  const Bit: Byte): Boolean; overload; inline;
   function FRE_ClearBit (const Val: Byte;  const Bit: Byte): Byte;    overload; inline;
   function FRE_SetBit   (const Val: Byte;  const Bit: Byte): Byte;    overload; inline;
   function FRE_GetBit   (const Val: Word;  const Bit: Byte): Boolean; overload; inline;
   function FRE_ClearBit (const Val: Word;  const Bit: Byte): Word;    overload; inline;
   function FRE_SetBit   (const Val: Word;  const Bit: Byte): Word;    overload; inline;
   function FRE_GetBit   (const Val: DWord; const Bit: Byte): Boolean; overload; inline;
   function FRE_ClearBit (const Val: DWord; const Bit: Byte): DWord;   overload; inline;
   function FRE_SetBit   (const Val: DWord; const Bit: Byte): DWord;   overload; inline;
   function FRE_ThreadingEnabled : Boolean;

   function FOSTI_StringToLogLevel      (const str:string) : TFOS_LOG_LEVEL;
   function FOSTI_StringToLogFacility   (const str:string) : TFOS_LOG_FACILITY;
   function FOSTI_StringToLogRuleAction (const str:string) : TFOS_LOG_RULE_ACTION;


   const
     CFOS_LL_Target:Array [TFOS_LOG_LEVEL] of string = ('INVALID','EMERGENCY','ALERT','CRITICAL','ERROR', 'WARNING', 'NOTICE', 'INFO', 'DEBUG');
     CFOS_Proptype:Array [ptNotFound..ptStringArray] of String=
                            ('ptNotFound','ptNull','ptBool','ptByte','ptShortInt','ptSmallInt','ptWord','ptLongInt',
                            'ptLongword','ptInt64','ptQWord','ptSingle','ptDouble',
                            'ptCurrency','ptDateTime','ptString','ptStream','ptPS','ptMemoryObject',
                            'ptBoolArray','ptByteArray','ptShortintArray','ptSmallintArray','ptWordArray','ptLongIntArray',
                            'ptLongWordArray','ptInt64Array','ptQWordArray','ptSingleArray','ptDoubleArray',
                            'ptCurrencyArray','ptStringArray'
                            );

     CFOS_Stable_Error:Array [low(EFOS_Stable_Error)..high(EFOS_Stable_Error)] of String=
                             ('OK','FAIL','FORMAT INVALID','NODE NOT SET','APPLICATION NOT SET','PARTITION NOT SET','OBJECT NOT SET','NEW OBJECT CREATED');

     catINFO    ='INFO';
     catWarning ='WARN';
     catError   ='ERROR';
     catCritical='CRITICAL';

   //var cat      : TFRE_DB_String;
   //    logentry : TFRE_DB_String;
   //    target   : string;
   //begin
   //  cat := CFRE_DB_LOGCATEGORY[category];
   //  case typ of
   //    0 : target:='INFO';
   //    1 : target:='WARNING';
   //    2 : target:='ERROR';
   //    3 : target:='DEBUG';
   //    4 : target:='NOTICE';
   //  end;
   //  GFRE_LOG.Log(msg,param,cat,level,target,false);
   //end;


   var GFRE_TF  : IFOS_TOOL_FACTORY;  // GLOBAL TOOL INTERFACE
       GFRE_BT  : IFOS_BASIC_TOOLS;   // GLOBAL Basic Tools
       GFRE_CPU : IFOS_CPU;           // GLOBAL CPU Thread Control
       GFRE_LOG : IFOS_FILE_LOGGER;   // GLOBAL File Logger Tool
       GFRE_DT  : IFOS_DATETOOLS;

implementation

type EFOS_TEST_Exception=class(Exception);
     EFOS_TI_Exception=class(Exception);


function FRE_GetBit(const Val: Byte; const Bit: Byte): Boolean; overload;
begin
  Result := (Val and (1 shl Bit)) <> 0;
end;

function FRE_ClearBit(const Val: Byte; const Bit: Byte): Byte; overload;
begin
    Result := Val and not (1 shl Bit);
end;

function FRE_SetBit(const Val: Byte; const Bit: Byte): Byte;  overload;
begin
    Result := Val or (1 shl Bit);
end;

function FRE_GetBit(const Val: Word; const Bit: Byte): Boolean; overload;
begin
  Result := (Val and (1 shl Bit)) <> 0;
end;

function FRE_ClearBit(const Val: Word; const Bit: Byte): Word; overload;
begin
    Result := Val and not (1 shl Bit);
end;

function FRE_SetBit(const Val: Word; const Bit: Byte): Word; overload;
begin
    Result := Val or (1 shl Bit);
end;

function FRE_GetBit(const Val: DWord; const Bit: Byte): Boolean; overload;
begin
  Result := (Val and (1 shl Bit)) <> 0;
end;

function FRE_ClearBit(const Val: DWord; const Bit: Byte): DWord; overload;
begin
    Result := Val and not (1 shl Bit);
end;

function FRE_SetBit(const Val: DWord; const Bit: Byte): DWord; overload;
begin
    Result := Val or (1 shl Bit);
end;

function FRE_ThreadingEnabled: Boolean;
var tm     : TThreadManager;
    type TTestFunction = Function : boolean;

begin
  GetThreadManager(tm);
  result := not  (TTestFunction(tm.InitManager)=nil);
end;


procedure E_FOS_Implement; // Raise the IMPLEMENT this Error -> Remove From Production CODE
begin
  raise EFOS_TEST_Exception.Create('IMPLEMENT THIS');
  GFRE_BT.CriticalAbort('IMPLEMENT');
end;

procedure E_FOS_TestNosey; // Raise the TestNosy Error -> Remove From Production CODE
begin
 try
   raise EFOS_TEST_Exception.Create('Interesting Condition reached, check handling');
 except
 end;
end;

procedure _Invalid_Continuation_Break;
begin
  GFRE_BT.CriticalAbort('-INVALID CONTINUATION BREAK-');
end;

{ TFOS_ReadOnlyMemStream }

constructor TFOS_ReadOnlyMemStream.Create(Ptr: Pointer; ASize: PtrInt);
begin
  Inherited Create;
  SetPointer(Ptr,ASize);
  Seek(0,soFromBeginning);
end;

function TFOS_ReadOnlyMemStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  raise Exception.Create('read only stream');
end;

{$HINTS OFF}
function TFOS_NonRefCounted_Object.QueryInterface(const iid: tguid; out obj): longint; stdcall;
begin
  result:=0;
end;

function TFOS_NonRefCounted_Object._AddRef: longint; stdcall;
begin
  result:=0;
end;

function TFOS_NonRefCounted_Object._Release: longint; stdcall;
begin
  result:=0;
end;
{$HINTS ON}

function FOSTI_StringToLogLevel (const str:string) : TFOS_LOG_LEVEL;
begin
  for result in TFOS_LOG_LEVEL do begin
    if CFOS_LOG_LEVEL[result]=str then exit;
  end;
  raise EFOS_TI_Exception.Create('invalid loglevel specifier : ['+str+']');
end;

function FOSTI_StringToLogFacility (const str:string) : TFOS_LOG_FACILITY;
begin
  for result in TFOS_LOG_FACILITY do begin
    if CFOS_LOG_FACILITY[result]=str then exit;
  end;
  raise EFOS_TI_Exception.Create('invalid logfacility specifier : ['+str+']');
end;

function FOSTI_StringToLogRuleAction(const str: string): TFOS_LOG_RULE_ACTION;
begin
 for result in TFOS_LOG_RULE_ACTION do begin
   if CFOS_LOG_RULE_ACTION[result]=str then exit;
 end;
 raise EFOS_TI_Exception.Create('invalid logruleaction specifier : ['+str+']');
end;

function FOSTI_LogLevelToTarget(const ll: TFOS_LOG_LEVEL): string;
begin
  case ll of
    fll_Invalid: result := '';
    fll_Emergency: ;
    fll_Alert: ;
    fll_Critical: ;
    fll_Error: ;
    fll_Warning: ;
    fll_Notice: ;
    fll_Info: ;
    fll_Debug: ;
  end;
end;


end.
