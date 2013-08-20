unit fre_postgres_ll;

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

// Low Level PostgreSQL Interface library
// Based on PostgresSQL 9.2.3
{$mode objfpc}{$H+}


{$IFDEF WINDOWS}
{$ENDIF}
{$IFDEF UNIX}
  {$IFDEF DARWIN}
    {$IFDEF CPU64}
      {$IFDEF FOS_DEBUG}
      {$ELSE}
      {$ENDIF}
    {$ELSE}
      {$IFDEF FOS_DEBUG}
      {$ELSE}
        {$linklib libpq_fos32_darwin_rel.a}
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
    {$IFDEF FREEBSD}
      {$IFDEF CPU64}
        {$IFDEF FOS_DEBUG}
        {$ELSE}
        {$ENDIF}
      {$ELSE}
        {$IFDEF FOS_DEBUG}
        {$ELSE}
        {$ENDIF}
      {$ENDIF}
    {$ELSE}
      {$IFDEF SOLARIS}
        {$IFDEF CPU64}
          {$IFDEF FOS_DEBUG}
          {$ELSE}
          {$ENDIF}
        {$ELSE}
          {$IFDEF FOS_DEBUG}
          {$ELSE}
          {$ENDIF}
        {$ENDIF}
      {$ELSE}
        {$IFDEF LINUX}
          {$IFDEF CPU64}
            {$IFDEF FOS_DEBUG}
            {$ELSE}
            {$ENDIF}
          {$ELSE}
            {$IFDEF FOS_DEBUG}
            {$ELSE}
            {$ENDIF}
          {$ENDIF}
          {$linklib librt.a}
        {$ELSE}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils,ctypes;

  procedure PG_ConnectionTest;

type

  { TFRE_POSTGRES_CONNECION }
  TFRE_POSTGRES_SQLMODE=(fpsm_disable,fpsm_allow,fpsm_prefer,fpsm_require,fpsm_verify_ca,fpsm_verify_full);

{$PACKENUM 4} // Should match gcc if not compiled with short enums
{$PACKRECORDS C}
const
  PG_DIAG_SEVERITY           = 'S';
  PG_DIAG_SQLSTATE           = 'C';
  PG_DIAG_MESSAGE_PRIMARY    = 'M';
  PG_DIAG_MESSAGE_DETAIL     = 'D';
  PG_DIAG_MESSAGE_HINT       = 'H';
  PG_DIAG_STATEMENT_POSITION = 'P';
  PG_DIAG_INTERNAL_POSITION  = 'p';
  PG_DIAG_INTERNAL_QUERY     = 'q';
  PG_DIAG_CONTEXT            = 'W';
  PG_DIAG_SOURCE_FILE        = 'F';
  PG_DIAG_SOURCE_LINE        = 'L';
  PG_DIAG_SOURCE_FUNCTION    = 'R';

type
  Oid  = Cardinal;
  POid = ^Oid;

  ConnStatusType = (
        CONNECTION_OK,
        CONNECTION_BAD,
        // Non-blocking mode only below here
        //
        //  The existence of these should never be relied upon - they should only
        //  be used for user feedback or similar purposes.
        CONNECTION_STARTED,                     // Waiting for connection to be made.
        CONNECTION_MADE,                        // Connection OK; waiting to send.
        CONNECTION_AWAITING_RESPONSE,           // Waiting for a response from the postmaster.
        CONNECTION_AUTH_OK,                     // Received authentication; waiting for backend startup.
        CONNECTION_SETENV,                      // Negotiating environment.
        CONNECTION_SSL_STARTUP,                 // Negotiating SSL.
        CONNECTION_NEEDED                       // Internal state: connect() needed
  );

  PostgresPollingStatusType =
  (
        PGRES_POLLING_FAILED := 0,
        PGRES_POLLING_READING,                  // These two indicate that one may
        PGRES_POLLING_WRITING,                  // use select before polling again.
        PGRES_POLLING_OK,
        PGRES_POLLING_ACTIVE                    // unused; keep for awhile for backwards compatibility
  );

  ExecStatusType =
  (
        PGRES_EMPTY_QUERY = 0,                  // empty query string was executed
        PGRES_COMMAND_OK,                       // a query command that doesn't return
                                                // anything was executed properly by the backend
        PGRES_TUPLES_OK,                        // a query command that returns tuples was
                                                // executed properly by the backend, PGresult
                                                // contains the result tuples
        PGRES_COPY_OUT,                         // Copy Out data transfer in progress
        PGRES_COPY_IN,                          // Copy In data transfer in progress
        PGRES_BAD_RESPONSE,                     // an unexpected response was recv'd from the backend
        PGRES_NONFATAL_ERROR,                   // notice or warning message
        PGRES_FATAL_ERROR,                      // query failed
        PGRES_COPY_BOTH,                        // Copy In/Out data transfer in progress
        PGRES_SINGLE_TUPLE                      // single tuple from larger resultset
  ) ;

  PGTransactionStatusType =
  (
        PQTRANS_IDLE,                           // connection idle
        PQTRANS_ACTIVE,                         // command in progress
        PQTRANS_INTRANS,                        // idle, within transaction block
        PQTRANS_INERROR,                        // idle, within failed transaction
        PQTRANS_UNKNOWN                         // cannot determine status
  ) ;

  PGVerbosity =
  (
        PQERRORS_TERSE,                         // single-line error messages
        PQERRORS_DEFAULT,                       // recommended style
        PQERRORS_VERBOSE                        // all the facts, ma'am
  ) ;

  PGPing =
  (
        PQPING_OK,                              // server is accepting connections
        PQPING_REJECT,                          // server is alive but rejecting connections
        PQPING_NO_RESPONSE,                     // could not establish connection
        PQPING_NO_ATTEMPT                       // connection not attempted (bad params)
  ) ;

  // PGconn encapsulates a connection to the backend.
  // The contents of this struct are not supposed to be known to applications.

  PPGconn = ^PGconn;
  PGconn  = record end;

  // PGresult encapsulates the result of a query (or more precisely, of a single
  //  SQL command --- a query string given to PQsendQuery can contain multiple
  // commands and thus return multiple PGresult objects).
  // The contents of this struct are not supposed to be known to applications.

  PGresult  = record end;
  PPGresult = ^PGresult;

  // PGcancel encapsulates the information needed to cancel a running
  // query on an existing connection.
  // The contents of this struct are not supposed to be known to applications.

  PGcancel  = record end;
  PPGcancel = ^PGcancel;

  // PGnotify represents the occurrence of a NOTIFY message.
  // Ideally this would be an opaque typedef, but it's so simple that it's
  // unlikely to change.
  // NOTE: in Postgres 6.4 and later, the be_pid is the notifying backend's,
  // whereas in earlier versions it was always your own backend's PID.

  pgNotify = packed record
    relname: PAnsiChar;   { name of relation containing data }
    be_pid : cint; { process id of backend }
    extra  : PAnsiChar; {additional data in notify}
  end;

  // Function types for notice-handling callbacks
  TpgNoticeProcessor = procedure(arg: Pointer; message: PAnsiChar); cdecl;
  TpgNotoceReceiver  = procedure(arg: Pointer; res    : PPGresult); cdecl;

  // Print options for PQprint()
  pqbool = cchar;

  PQprintOpt = packed record
    header:     pqbool; // print output field headings and row count
    align:      pqbool; // fill align the fields
    standard:   pqbool; // old brain dead format
    html3:      pqbool; // output html tables
    expanded:   pqbool; // expand tables
    pager:      pqbool; // use pager for output if needed
    fieldSep:   pcchar; //  field separator
    tableOpt:   pcchar; // insert to HTML <table ...>
    caption:    pcchar; // HTML <caption>
    fieldName:  ^pcchar; //null terminated array of replacement field
  end;

  // ----------------
  // Structure for the conninfo parameter definitions returned by PQconndefaults
  // or PQconninfoParse.
  //
  // All fields except "val" point at static strings which must not be altered.
  // "val" is either NULL or a malloc'd current-value string.  PQconninfoFree()
  // will release both the val strings and the PQconninfoOption array itself.
  // ----------------

  PQconninfoOption = packed record
    keyword:  PAnsiChar;        // The keyword of the option
    envvar:   PAnsiChar;        // Fallback environment variable name
    compiled: PAnsiChar;        // Fallback compiled in default value
    val:      PAnsiChar;        // Options value
    labell:    PAnsiChar;       // Label for field in connect dialog
    dispchar: PAnsiChar;        // Character to display for this field
                                // in a connect dialog. Values are:
                                // ""   Display entered value as is
                                // "*"  Password field - hide value
                                // "D"  Debug options - don't create a field by default
    dispsize: Integer;          // Field size in characters for dialog
  end;

  // ----------------
  // PQArgBlock -- structure for PQfn() arguments
  // ----------------
  //
  PQArgBlock = packed record
     len:     Integer;
     isint:   Integer;
     case u: Boolean of
       True:  (ptr: PInteger);
       False: (integer: Integer);
   end;

  /// ----------------
  //  PGresAttDesc -- Data about a single attribute (column) of a query result
  //  ----------------

  pgresAttDesc = record
    name:      pcchar; // column name
    tableid:   Oid   ; // source table, if known
    columnid:  cint  ; // source column, if known
    format:    cint  ; // format code for value (text/binary)
    typid:     Oid   ; // type id
    typlen:    cint  ; // type size
    atttypmod: cint  ; // type-specific modifier info
  end;

  function  PQconnectdb          (ConnInfo: PAnsiChar) : PPGconn                          ; cdecl; external;
  function  PQping               (ConnInfo: PAnsiChar) : PGPing                           ; cdecl; external;
  function  PQstatus             (Handle: PPGconn)     : ConnStatusType                   ; cdecl; external;
  procedure PQfinish             (Handle: PPGconn)                                        ; cdecl; external;
  procedure PQreset              (Handle: PPGconn)                                        ; cdecl; external;
  function  PQexec               (Handle: PPGconn; Query: PAnsiChar): PPGresult           ; cdecl; external;
  function  PQresultStatus       (const res : PPGresult ):ExecStatusType                  ; cdecl; external;
  function  PQresultErrorMessage (Result: PPGresult): PAnsiChar                           ; cdecl; external;
  function  PQntuples            (Result: PPGresult): cint                                ; cdecl; external;
  function  PQnfields            (Result: PPGresult): cint                                ; cdecl; external;
  function  PQfname              (Result: PPGresult; field_num: cint): PAnsiChar          ; cdecl; external;
  function  PQfnumber            (Result: PPGresult; field_name: PAnsiChar):cint          ; cdecl; external;
  function  PQfformat            (Result: PPGresult; field_num: cint):cint                ; cdecl; external;
  function  PQftype              (Result: PPGresult; field_num: cint): Oid                ; cdecl; external;
  function  PQfsize              (Result: PPGresult; field_num: cint): cint               ; cdecl; external;
  function  PQfmod               (Result: PPGresult; field_num: Integer): cint            ; cdecl; external;
  function  PQgetvalue           (Result: PPGresult; row_num, field_num: cint): PAnsiChar ; cdecl; external;
  function  PQgetlength          (Result: PPGresult; row_num, field_num: cint): cint      ; cdecl; external;
  function  PQgetisnull          (Result: PPGresult; row_num, field_num: cint): cint      ; cdecl; external;
  function  PQnparams            (Result: PPGresult):cint                                 ; cdecl; external;
  function  PQparamtype          (Result: PPGresult; param_number : cint):Oid             ; cdecl; external;
  function  PQcmdStatus          (Result: PPGresult): PAnsiChar                           ; cdecl; external;
  function  PQcmdTuples          (Result: PPGresult): PAnsiChar                           ; cdecl; external;

  function  PQoidValue           (Result: PPGresult): Oid                                 ; cdecl; external;
  procedure PQfreemem            (ptr:Pointer)                                            ; cdecl; external;
  procedure PQclear              (Result: PPGresult)                                      ; cdecl; external;

  function  PQescapeStringConn   (Handle: PGconn; ToChar: PAnsiChar;
                                  const FromChar: PAnsiChar; length: csize_t;
                                  error: pcint): csize_t                                  ; cdecl; external;

  function  PQescapeByteaConn    (Handle: PPGconn;const from:PAnsiChar;
                                  from_length: csize_t; var to_lenght :csize_t):PAnsiChar ; cdecl; external;

  function  PQunescapeBytea      (const from:PAnsiChar;var to_lenght:csize_t):PAnsiChar   ; cdecl; external;
  function  PQescapeIdentifier   (Handle: PPGconn; const str: PAnsiChar;
                                  len: csize_t): PAnsiChar                                ; cdecl; external;



  function  PQexecParams         (Handle: PPGconn; command: PAnsichar;
                                  nParams: Integer; paramTypes: POid;
                                  var paramValues: Array of PChar;
                                  paramLengths: pcint;
                                  var paramFormats: Array of pcint;
                                  resultFormat: Integer) : PPGresult               ; cdecl; external;

  //TPQprepare        = function(Handle: PPGconn; stmtName: PAnsichar;
  //      query: PAnsiChar; nParams: Integer; paramTypes: TPQparamTypes): PPGresult; cdecl;
  //TPQexecPrepared   = function(Handle: PPGconn; stmtName: PAnsichar;
  //      nParams: Integer; paramValues: TPQparamValues; paramLengths: TPQparamLengths;
  //      paramFormats: TPQparamFormats; resultFormat: Integer): PPGresult; cdecl;


  //TPQresultErrorField=function(result: PPGResult; fieldcode:integer):PAnsiChar;cdecl; // postgresql 8



type

  TFRE_PQ_OID_TYPE=record
    OID       : Oid;
    type_name : STRING[59];
  end;

  TFRE_PQ_BASIC_TYPES = array of TFRE_PQ_OID_TYPE;

var GPQ_OID_TYPES : TFRE_PQ_BASIC_TYPES;

type
  TFRE_POSTGRES_CONNECION=class;

  { TFRE_PQ_CMD }

  TFRE_PQ_CMD=class
  private
    FParentConn  : TFRE_POSTGRES_CONNECION;
    FCommandText : String;
    FConn        : PPGconn;
    FCMDResult   : PPGresult;
    Ferror       : String;
    procedure SetCommandText(AValue: String);
  public
    destructor Destroy                    ; override;
    property   CommandText                : String read FCommandText write SetCommandText;
    function   Execute                    : boolean;
    function   DumpResult                 : string;
    function   LastError                  : String;
    function   GetNumberOfTuples          : cint;
    function   GetNumberOfFields          : cint;
    function   GetFieldName               (const idx:integer):String;
    function   GetFieldNumber             (const fname:string):integer;
    function   GetFieldFormat             (const idx:integer):integer; // 0 = text ; 1 = Binary
    function   GetFieldTypeOID            (const idx:integer):Oid; // OID
    function   GetFieldTypeModifier       (const idx:integer):integer;
    function   GetFieldInternalByteSize   (const idx:integer):integer;
    function   GetFieldByteSize           (const row_idx,idx:integer):integer;
    function   GetFieldValue              (const row_idx,idx:integer):PChar;
    function   GetFieldIsNull             (const row_idx,idx:integer):Boolean;
    function   GetNumParams               :integer;
    function   GetParamTypeOID            (const idx:integer):Oid;
    function   GetCmdStatus               :String;
    function   GetCmdAffectedRows         :Integer;
    function   GetLastInsertedOidValue    :Oid;
    function   EncodeIdentifier           (const id:string):String;
  end;

  TFRE_POSTGRES_CONNECION=class
  private
    FApplicationName: string;
    FConnTimeout    : integer;
    FDBname         : String;
    FHost           : String;
    FHostAddr       : String;
    FPassword       : String;
    FPort           : integer;
    FSSLCertFile    : string;
    FSSLCRLFile     : string;
    FSSLKeyFile     : string;
    FSSLMode        : TFRE_POSTGRES_SQLMODE;
    FSSLRootCAFile  : string;
    FUser           : String;

    FConn             : PPGconn;
    FConnStatus       : ConnStatusType;
    FConnectionString : String;
    procedure MarkConnectionStringDirty;
    procedure SetApplicationName(AValue: string);

    procedure SetConnTimeout(AValue: integer);
    procedure SetDBname(AValue: String);
    procedure SetHost(AValue: String);
    procedure SetHostAddr(AValue: String);
    procedure SetPassword(AValue: String);
    procedure SetPort(AValue: integer);
    procedure SetSSLCertFile(AValue: string);
    procedure SetSSLCRLFile(AValue: string);
    procedure SetSSLKeyFile(AValue: string);
    procedure SetSSLMode(AValue: TFRE_POSTGRES_SQLMODE);
    procedure SetSSLRootCAFile(AValue: string);
    procedure SetUser(AValue: String);
  public
    constructor Create      ;
    function  Connect         :Boolean;
    property  Host            :String read FHost write SetHost;
    property  HostAddr        :String read FHostAddr write SetHostAddr;
    property  Port            :integer read FPort write SetPort;
    property  DBname          :String read FDBname write SetDBname;
    property  User            :String read FUser write SetUser;
    property  Password        :String read FPassword write SetPassword;
    property  ConnTimeout     :integer read FConnTimeout write SetConnTimeout; // 0 = forever
    property  SSLMode         :TFRE_POSTGRES_SQLMODE read FSSLMode write SetSSLMode;
    property  SSLCertFile     :string read FSSLCertFile write SetSSLCertFile;
    property  SSLKeyFile      :string read FSSLKeyFile write SetSSLKeyFile;
    property  SSLRootCAFile   :string read FSSLRootCAFile write SetSSLRootCAFile;
    property  SSLCRLFile      :string read FSSLCRLFile write SetSSLCRLFile;
    property  ApplicationName :string read FApplicationName write SetApplicationName;
    function  GetTypeNameOid  (const in_oid : Oid):String;
    function  GetNewCommand   :TFRE_PQ_CMD;
  end;


implementation


procedure PG_ConnectionTest;
var
    Conn       : PPGconn;
    ConnStatus : ConnStatusType;
begin
  conn := nil;
  writeln('PING: ',PQPing('host=localhost port=5432 dbname=helly connect_timeout=10'));
  conn := PQconnectdb('host=localhost port=5432 dbname=helly connect_timeout=10');
  if assigned(conn) then begin
    ConnStatus := PQstatus(conn);
    case  ConnStatus of
      CONNECTION_OK: begin
                      writeln('CONNECTION OK');
                     end;
      else writeln('CONNECTION STATUS ',ConnStatus);
    end;
  end else begin
    writeln('CONN FAILED NULL RESULT');
  end;
end;

{ TFRE_PQ_CMD }

procedure TFRE_PQ_CMD.SetCommandText(AValue: String);
begin
  FCommandText:=AValue;
end;

destructor TFRE_PQ_CMD.Destroy;
begin
  PQclear(FCMDResult);
  inherited Destroy;
end;

function TFRE_PQ_CMD.Execute: boolean;
var exec_status : ExecStatusType;
begin
  Ferror := '';
  FCMDResult  := PQexec(FConn,pchar(FCommandText));
  exec_status := PQresultStatus(FCMDResult);
  case exec_status of
    PGRES_EMPTY_QUERY    : result := true;
    PGRES_NONFATAL_ERROR,
    PGRES_SINGLE_TUPLE,
    PGRES_TUPLES_OK,
    PGRES_COMMAND_OK     : begin
                             result := true;
                           end;
    //PGRES_COPY_OUT: ;
    //PGRES_COPY_IN: ;
    //PGRES_COPY_BOTH: ;
    PGRES_BAD_RESPONSE,
    PGRES_FATAL_ERROR    : begin
                             result := false;
                             FError := PQresultErrorMessage(FCMDResult);
                           end;
    else begin
      raise Exception.Create('pq_ll_cmd: unexpected resultcode '+inttostr(ord(exec_status)));
    end;
  end;
end;

function TFRE_PQ_CMD.DumpResult: string;
var tuplenum,fieldnum,i,j,ff,fto:integer;
    tn : string;
begin
  result := '';
  writeln('CMD Status: ',GetCmdStatus);
  tuplenum := GetNumberOfTuples;
  fieldnum := GetNumberOfFields;
  for i:= 0 to fieldnum-1 do begin
    write(GetFieldName(i));
    ff  :=  GetFieldFormat(i);
    fto :=  GetFieldTypeOID(i);
    tn  :=  FParentConn.GetTypeNameOid(fto);
    if ff=0 then write('(T/',fto,'/',tn,')');
    if ff=1 then write('(B/',fto,'/',tn,')');
    if (ff>1) or (ff<0) then write('(?)');
    if i<fieldnum-1 then write(' : ');
  end;
  writeln;
  for i:=0 to tuplenum-1 do begin
    write(i,': ');
    for j := 0 to fieldnum-1 do begin
      write(GetFieldValue(i,j));
      if j<fieldnum-2 then write(' # ');
    end;
    writeln;
  end;
end;

function TFRE_PQ_CMD.LastError: String;
begin
  result := Ferror;
end;

function TFRE_PQ_CMD.GetNumberOfTuples: cint;
begin
  result := PQntuples(FCMDResult);
end;

function TFRE_PQ_CMD.GetNumberOfFields: cint;
begin
  result := PQnfields(FCMDResult);
end;

function TFRE_PQ_CMD.GetFieldName(const idx: integer): String;
begin
  result := PQfname(FCMDResult,idx);
end;

function TFRE_PQ_CMD.GetFieldNumber(const fname: string): integer;
begin
  result := PQfnumber(FCMDResult,pchar(fname));
end;

function TFRE_PQ_CMD.GetFieldFormat(const idx: integer): integer;
begin
  PQfformat(FCMDResult,idx);
end;

function TFRE_PQ_CMD.GetFieldTypeOID(const idx: integer): Oid;
begin
  result := PQftype(FCMDResult,idx);
end;

function TFRE_PQ_CMD.GetFieldTypeModifier(const idx: integer): integer;
begin
  result := PQfmod(FCMDResult,idx);
end;

function TFRE_PQ_CMD.GetFieldInternalByteSize(const idx: integer): integer;
begin
  result := PQfsize(FCMDResult,idx);
end;

function TFRE_PQ_CMD.GetFieldByteSize(const row_idx, idx: integer): integer;
begin
  result := PQgetlength(FCMDResult,row_idx,idx);
end;


function TFRE_PQ_CMD.GetFieldValue(const row_idx, idx: integer): PChar;
begin
  result := PQgetvalue(FCMDResult,row_idx,idx);
end;

function TFRE_PQ_CMD.GetFieldIsNull(const row_idx, idx: integer): Boolean;
begin
  result := PQgetisnull(FCMDResult,row_idx,idx)=1;
end;

function TFRE_PQ_CMD.GetNumParams: integer;
begin
  result := PQnparams(FCMDResult);
end;

function TFRE_PQ_CMD.GetParamTypeOID(const idx: integer): Oid;
begin
  result := PQparamtype(FCMDResult,idx);
end;

function TFRE_PQ_CMD.GetCmdStatus: String;
begin
  result := PQcmdStatus(FCMDResult);
end;

function TFRE_PQ_CMD.GetCmdAffectedRows: Integer;
begin
  result := StrToIntDef(PQcmdTuples(FCMDResult),-1);
end;

function TFRE_PQ_CMD.GetLastInsertedOidValue: Oid;
begin
  result := PQoidValue(FCMDResult);
end;

function TFRE_PQ_CMD.EncodeIdentifier(const id: string): String;
var res : PChar;
begin
  res    := PQescapeIdentifier(FParentConn.FConn,@id[1],length(id));
  if not assigned(res) then raise Exception.Create('pq_ll_cmd: cannot encode identifier '+id);
  result := copy(res,1,maxint);
  PQfreemem(res);
end;

  { TFRE_POSTGRES_CONNECION }

procedure TFRE_POSTGRES_CONNECION.SetHost(AValue: String);
begin
  FHost:=AValue;
end;

procedure TFRE_POSTGRES_CONNECION.SetDBname(AValue: String);
begin
  FDBname:=AValue;
end;

procedure TFRE_POSTGRES_CONNECION.MarkConnectionStringDirty;
begin
  FConnectionString:='';
end;

procedure TFRE_POSTGRES_CONNECION.SetApplicationName(AValue: string);
begin
  FApplicationName:=AValue;
end;

procedure TFRE_POSTGRES_CONNECION.SetConnTimeout(AValue: integer);
begin
  MarkConnectionStringDirty;
  FConnTimeout:=AValue;
end;

procedure TFRE_POSTGRES_CONNECION.SetHostAddr(AValue: String);
begin
  MarkConnectionStringDirty;
  FHostAddr:=AValue;
end;

procedure TFRE_POSTGRES_CONNECION.SetPassword(AValue: String);
begin
  MarkConnectionStringDirty;
  FPassword:=AValue;
end;

procedure TFRE_POSTGRES_CONNECION.SetPort(AValue: integer);
begin
  MarkConnectionStringDirty;
  FPort:=AValue;
end;

procedure TFRE_POSTGRES_CONNECION.SetSSLCertFile(AValue: string);
begin
  MarkConnectionStringDirty;
  FSSLCertFile:=AValue;
end;

procedure TFRE_POSTGRES_CONNECION.SetSSLCRLFile(AValue: string);
begin
  MarkConnectionStringDirty;
  FSSLCRLFile:=AValue;
end;

procedure TFRE_POSTGRES_CONNECION.SetSSLKeyFile(AValue: string);
begin
  MarkConnectionStringDirty;
  FSSLKeyFile:=AValue;
end;

procedure TFRE_POSTGRES_CONNECION.SetSSLMode(AValue: TFRE_POSTGRES_SQLMODE);
begin
  MarkConnectionStringDirty;
  FSSLMode:=AValue;
end;

procedure TFRE_POSTGRES_CONNECION.SetSSLRootCAFile(AValue: string);
begin
  MarkConnectionStringDirty;
  FSSLRootCAFile:=AValue;
end;

procedure TFRE_POSTGRES_CONNECION.SetUser(AValue: String);
begin
  MarkConnectionStringDirty;
  FUser:=AValue;
end;

constructor TFRE_POSTGRES_CONNECION.Create;
begin
  inherited;
  FSSLMode := fpsm_prefer;
end;

function TFRE_POSTGRES_CONNECION.Connect: Boolean;

  procedure BuildConnectionString;

    procedure CondAddParam(const val:string;const name:string);
    begin
      if val<>'' then begin
        FConnectionString := FConnectionString+' '+name+'='''+trim(val)+''' ';
      end;
    end;

  begin
    CondAddParam(FHost                  ,'host');
    CondAddParam(FHostAddr              ,'hostaddr');
    CondAddParam(inttostr(FPort)        ,'port');
    CondAddParam(FDBname                ,'dbname');
    CondAddParam(FUser                  ,'user');
    CondAddParam(FPassword              ,'password');
    CondAddParam(inttostr(FConnTimeout) ,'connect_timeout');
    CondAddParam('UTF8'                 ,'client_encoding');
    CondAddParam(FApplicationName       ,'application_name');
    case FSSLMode of
      fpsm_disable:     CondAddParam('disable'    ,'sslmode');
      fpsm_allow:       CondAddParam('allow'      ,'sslmode');
      fpsm_prefer:      CondAddParam('prefer'     ,'sslmode');
      fpsm_require:     CondAddParam('require'    ,'sslmode');
      fpsm_verify_ca:   CondAddParam('verify-ca'  ,'sslmode');
      fpsm_verify_full: CondAddParam('verify-full','sslmode');
    end;
    CondAddParam(FSSLCertFile  ,'sslcert');
    CondAddParam(FSSLKeyFile   ,'sslkey');
    CondAddParam(FSSLRootCAFile,'sslrootcert');
    CondAddParam(FSSLCRLFile   ,'sslcrl');
    FConnectionString:=trim(FConnectionString);
  end;

  procedure FetchOIDS;
  var NC            : TFRE_PQ_CMD;
      num_oid_types : integer;
      i             : integer;
  begin
    NC := GetNewCommand;
    try
      NC.CommandText:='SELECT oid,typname,octet_length(typname) from pg_type order by oid';
      if not NC.Execute then begin
        raise exception.Create('pq_ll_connection: could not fetch types from server'+nc.LastError);
      end;
      num_oid_types := nc.GetNumberOfTuples;
      SetLength(GPQ_OID_TYPES,num_oid_types);
      for i :=0 to num_oid_types-1 do begin
        GPQ_OID_TYPES[i].OID       := strtoint(NC.GetFieldValue(i,0));
        GPQ_OID_TYPES[i].type_name := Copy(NC.GetFieldValue(i,1),1,59);
      end;
    finally
      NC.free;
    end;
  end;

begin
  if FConnectionString='' then BuildConnectionString;
  if assigned(FConn) then begin
    PQFinish(FConn);
  end;
  FConn       := PQconnectdb(PChar(FConnectionString));
  FConnStatus := PQstatus(FConn);
  if FConnStatus=CONNECTION_OK then begin
    result := true;
    if Length(GPQ_OID_TYPES)=0 then begin
      FetchOIDS;
    end;
  end else begin
    result := false;
    PQfinish(FConn);
    FConn:=nil;
  end;
end;

function TFRE_POSTGRES_CONNECION.GetTypeNameOid(const in_oid: Oid): String;
var i : integer;
begin
  for i:=0 to high(GPQ_OID_TYPES) do begin
    if GPQ_OID_TYPES[i].OID=in_oid then begin
      exit(GPQ_OID_TYPES[i].type_name);
    end;
  end;
end;

function TFRE_POSTGRES_CONNECION.GetNewCommand: TFRE_PQ_CMD;
begin
  if not assigned(FConn) then raise Exception.Create('pq_ll_connection: not connected');
  result             := TFRE_PQ_CMD.Create;
  result.FConn       := FConn;
  result.FParentConn := self;
end;


  //TPQsetdbLogin    = function(Host, Port, Options, Tty, Db, User, Passwd: PAnsiChar): PPGconn; cdecl; // FirmOS 8.1 OK



  ///* ===	in fe-connect.c === */
  //
  ///* make a new client connection to the backend */
  ///* Asynchronous (non-blocking) */
  //extern PGconn *PQconnectStart(const char *conninfo);
  //extern PGconn *PQconnectStartParams(const char *const * keywords,
  //					 const char *const * values, int expand_dbname);
  //extern PostgresPollingStatusType PQconnectPoll(PGconn *conn);
  //
  ///* Synchronous (blocking) */
  //extern PGconn *PQconnectdb(const char *conninfo);
  //extern PGconn *PQconnectdbParams(const char *const * keywords,
  //				  const char *const * values, int expand_dbname);
  //extern PGconn *PQsetdbLogin(const char *pghost, const char *pgport,
  //			 const char *pgoptions, const char *pgtty,
  //			 const char *dbName,
  //			 const char *login, const char *pwd);
  //
  //#define PQsetdb(M_PGHOST,M_PGPORT,M_PGOPT,M_PGTTY,M_DBNAME)  \
  //	PQsetdbLogin(M_PGHOST, M_PGPORT, M_PGOPT, M_PGTTY, M_DBNAME, NULL, NULL)
  //
  ///* close the current connection and free the PGconn data structure */
  //extern void PQfinish(PGconn *conn);
  //
  ///* get info about connection options known to PQconnectdb */
  //extern PQconninfoOption *PQconndefaults(void);
  //
  ///* parse connection options in same way as PQconnectdb */
  //extern PQconninfoOption *PQconninfoParse(const char *conninfo, char **errmsg);
  //
  ///* free the data structure returned by PQconndefaults() or PQconninfoParse() */
  //extern void PQconninfoFree(PQconninfoOption *connOptions);
  //
  ///*
  // * close the current connection and restablish a new one with the same
  // * parameters
  // */
  ///* Asynchronous (non-blocking) */
  //extern int	PQresetStart(PGconn *conn);
  //extern PostgresPollingStatusType PQresetPoll(PGconn *conn);
  //
  ///* Synchronous (blocking) */
  //extern void PQreset(PGconn *conn);
  //
  ///* request a cancel structure */
  //extern PGcancel *PQgetCancel(PGconn *conn);
  //
  ///* free a cancel structure */
  //extern void PQfreeCancel(PGcancel *cancel);
  //
  ///* issue a cancel request */
  //extern int	PQcancel(PGcancel *cancel, char *errbuf, int errbufsize);
  //
  ///* backwards compatible version of PQcancel; not thread-safe */
  //extern int	PQrequestCancel(PGconn *conn);
  //
  ///* Accessor functions for PGconn objects */
  //extern char *PQdb(const PGconn *conn);
  //extern char *PQuser(const PGconn *conn);
  //extern char *PQpass(const PGconn *conn);
  //extern char *PQhost(const PGconn *conn);
  //extern char *PQport(const PGconn *conn);
  //extern char *PQtty(const PGconn *conn);
  //extern char *PQoptions(const PGconn *conn);
  //extern ConnStatusType PQstatus(const PGconn *conn);
  //extern PGTransactionStatusType PQtransactionStatus(const PGconn *conn);
  //extern const char *PQparameterStatus(const PGconn *conn,
  //				  const char *paramName);
  //extern int	PQprotocolVersion(const PGconn *conn);
  //extern int	PQserverVersion(const PGconn *conn);
  //extern char *PQerrorMessage(const PGconn *conn);
  //extern int	PQsocket(const PGconn *conn);
  //extern int	PQbackendPID(const PGconn *conn);
  //extern int	PQconnectionNeedsPassword(const PGconn *conn);
  //extern int	PQconnectionUsedPassword(const PGconn *conn);
  //extern int	PQclientEncoding(const PGconn *conn);
  //extern int	PQsetClientEncoding(PGconn *conn, const char *encoding);
  //
  ///* Get the OpenSSL structure associated with a connection. Returns NULL for
  // * unencrypted connections or if any other TLS library is in use. */
  //extern void *PQgetssl(PGconn *conn);
  //
  ///* Tell libpq whether it needs to initialize OpenSSL */
  //extern void PQinitSSL(int do_init);
  //
  ///* More detailed way to tell libpq whether it needs to initialize OpenSSL */
  //extern void PQinitOpenSSL(int do_ssl, int do_crypto);
  //
  ///* Set verbosity for PQerrorMessage and PQresultErrorMessage */
  //extern PGVerbosity PQsetErrorVerbosity(PGconn *conn, PGVerbosity verbosity);
  //
  ///* Enable/disable tracing */
  //extern void PQtrace(PGconn *conn, FILE *debug_port);
  //extern void PQuntrace(PGconn *conn);
  //
  ///* Override default notice handling routines */
  //extern PQnoticeReceiver PQsetNoticeReceiver(PGconn *conn,
  //					PQnoticeReceiver proc,
  //					void *arg);
  //extern PQnoticeProcessor PQsetNoticeProcessor(PGconn *conn,
  //					 PQnoticeProcessor proc,
  //					 void *arg);
  //
  ///*
  // *	   Used to set callback that prevents concurrent access to
  // *	   non-thread safe functions that libpq needs.
  // *	   The default implementation uses a libpq internal mutex.
  // *	   Only required for multithreaded apps that use kerberos
  // *	   both within their app and for postgresql connections.
  // */
  //typedef void (*pgthreadlock_t) (int acquire);
  //
  //extern pgthreadlock_t PQregisterThreadLock(pgthreadlock_t newhandler);
  //
  ///* === in fe-exec.c === */
  //
  ///* Simple synchronous query */
  //extern PGresult *PQexec(PGconn *conn, const char *query);
  //extern PGresult *PQexecParams(PGconn *conn,
  //			 const char *command,
  //			 int nParams,
  //			 const Oid *paramTypes,
  //			 const char *const * paramValues,
  //			 const int *paramLengths,
  //			 const int *paramFormats,
  //			 int resultFormat);
  //extern PGresult *PQprepare(PGconn *conn, const char *stmtName,
  //		  const char *query, int nParams,
  //		  const Oid *paramTypes);
  //extern PGresult *PQexecPrepared(PGconn *conn,
  //			   const char *stmtName,
  //			   int nParams,
  //			   const char *const * paramValues,
  //			   const int *paramLengths,
  //			   const int *paramFormats,
  //			   int resultFormat);
  //
  ///* Interface for multiple-result or asynchronous queries */
  //extern int	PQsendQuery(PGconn *conn, const char *query);
  //extern int PQsendQueryParams(PGconn *conn,
  //				  const char *command,
  //				  int nParams,
  //				  const Oid *paramTypes,
  //				  const char *const * paramValues,
  //				  const int *paramLengths,
  //				  const int *paramFormats,
  //				  int resultFormat);
  //extern int PQsendPrepare(PGconn *conn, const char *stmtName,
  //			  const char *query, int nParams,
  //			  const Oid *paramTypes);
  //extern int PQsendQueryPrepared(PGconn *conn,
  //					const char *stmtName,
  //					int nParams,
  //					const char *const * paramValues,
  //					const int *paramLengths,
  //					const int *paramFormats,
  //					int resultFormat);
  //extern int	PQsetSingleRowMode(PGconn *conn);
  //extern PGresult *PQgetResult(PGconn *conn);
  //
  ///* Routines for managing an asynchronous query */
  //extern int	PQisBusy(PGconn *conn);
  //extern int	PQconsumeInput(PGconn *conn);
  //
  ///* LISTEN/NOTIFY support */
  //extern PGnotify *PQnotifies(PGconn *conn);
  //
  ///* Routines for copy in/out */
  //extern int	PQputCopyData(PGconn *conn, const char *buffer, int nbytes);
  //extern int	PQputCopyEnd(PGconn *conn, const char *errormsg);
  //extern int	PQgetCopyData(PGconn *conn, char **buffer, int async);
  //
  ///* Deprecated routines for copy in/out */
  //extern int	PQgetline(PGconn *conn, char *string, int length);
  //extern int	PQputline(PGconn *conn, const char *string);
  //extern int	PQgetlineAsync(PGconn *conn, char *buffer, int bufsize);
  //extern int	PQputnbytes(PGconn *conn, const char *buffer, int nbytes);
  //extern int	PQendcopy(PGconn *conn);
  //
  ///* Set blocking/nonblocking connection to the backend */
  //extern int	PQsetnonblocking(PGconn *conn, int arg);
  //extern int	PQisnonblocking(const PGconn *conn);
  //extern int	PQisthreadsafe(void);
  //extern PGPing PQping(const char *conninfo);
  //extern PGPing PQpingParams(const char *const * keywords,
  //			 const char *const * values, int expand_dbname);
  //
  ///* Force the write buffer to be written (or at least try) */
  //extern int	PQflush(PGconn *conn);
  //
  ///*
  // * "Fast path" interface --- not really recommended for application
  // * use
  // */
  //extern PGresult *PQfn(PGconn *conn,
  //	 int fnid,
  //	 int *result_buf,
  //	 int *result_len,
  //	 int result_is_int,
  //	 const PQArgBlock *args,
  //	 int nargs);
  //
  ///* Accessor functions for PGresult objects */
  //extern ExecStatusType PQresultStatus(const PGresult *res);
  //extern char *PQresStatus(ExecStatusType status);
  //extern char *PQresultErrorMessage(const PGresult *res);
  //extern char *PQresultErrorField(const PGresult *res, int fieldcode);
  //extern int	PQntuples(const PGresult *res);
  //extern int	PQnfields(const PGresult *res);
  //extern int	PQbinaryTuples(const PGresult *res);
  //extern char *PQfname(const PGresult *res, int field_num);
  //extern int	PQfnumber(const PGresult *res, const char *field_name);
  //extern Oid	PQftable(const PGresult *res, int field_num);
  //extern int	PQftablecol(const PGresult *res, int field_num);
  //extern int	PQfformat(const PGresult *res, int field_num);
  //extern Oid	PQftype(const PGresult *res, int field_num);
  //extern int	PQfsize(const PGresult *res, int field_num);
  //extern int	PQfmod(const PGresult *res, int field_num);
  //extern char *PQcmdStatus(PGresult *res);
  //extern char *PQoidStatus(const PGresult *res);	/* old and ugly */
  //extern Oid	PQoidValue(const PGresult *res);	/* new and improved */
  //extern char *PQcmdTuples(PGresult *res);
  //extern char *PQgetvalue(const PGresult *res, int tup_num, int field_num);
  //extern int	PQgetlength(const PGresult *res, int tup_num, int field_num);
  //extern int	PQgetisnull(const PGresult *res, int tup_num, int field_num);
  //extern int	PQnparams(const PGresult *res);
  //extern Oid	PQparamtype(const PGresult *res, int param_num);
  //
  ///* Describe prepared statements and portals */
  //extern PGresult *PQdescribePrepared(PGconn *conn, const char *stmt);
  //extern PGresult *PQdescribePortal(PGconn *conn, const char *portal);
  //extern int	PQsendDescribePrepared(PGconn *conn, const char *stmt);
  //extern int	PQsendDescribePortal(PGconn *conn, const char *portal);
  //
  ///* Delete a PGresult */
  //extern void PQclear(PGresult *res);
  //
  ///* For freeing other alloc'd results, such as PGnotify structs */
  //extern void PQfreemem(void *ptr);
  //
  ///* Exists for backward compatibility.  bjm 2003-03-24 */
  //#define PQfreeNotify(ptr) PQfreemem(ptr)
  //
  ///* Error when no password was given. */
  ///* Note: depending on this is deprecated; use PQconnectionNeedsPassword(). */
  //#define PQnoPasswordSupplied	"fe_sendauth: no password supplied\n"
  //
  ///* Create and manipulate PGresults */
  //extern PGresult *PQmakeEmptyPGresult(PGconn *conn, ExecStatusType status);
  //extern PGresult *PQcopyResult(const PGresult *src, int flags);
  //extern int	PQsetResultAttrs(PGresult *res, int numAttributes, PGresAttDesc *attDescs);
  //extern void *PQresultAlloc(PGresult *res, size_t nBytes);
  //extern int	PQsetvalue(PGresult *res, int tup_num, int field_num, char *value, int len);
  //
  ///* Quoting strings before inclusion in queries. */
  //extern size_t PQescapeStringConn(PGconn *conn,
  //				   char *to, const char *from, size_t length,
  //				   int *error);
  //extern char *PQescapeLiteral(PGconn *conn, const char *str, size_t len);
  //extern char *PQescapeIdentifier(PGconn *conn, const char *str, size_t len);
  //extern unsigned char *PQescapeByteaConn(PGconn *conn,
  //				  const unsigned char *from, size_t from_length,
  //				  size_t *to_length);
  //extern unsigned char *PQunescapeBytea(const unsigned char *strtext,
  //				size_t *retbuflen);
  //
  ///* These forms are deprecated! */
  //extern size_t PQescapeString(char *to, const char *from, size_t length);
  //extern unsigned char *PQescapeBytea(const unsigned char *from, size_t from_length,
  //			  size_t *to_length);
  //
  //
  //
  ///* === in fe-print.c === */
  //
  //extern void
  //PQprint(FILE *fout,				/* output stream */
  //		const PGresult *res,
  //		const PQprintOpt *ps);	/* option structure */
  //
  ///*
  // * really old printing routines
  // */
  //extern void
  //PQdisplayTuples(const PGresult *res,
  //				FILE *fp,		/* where to send the output */
  //				int fillAlign,	/* pad the fields with spaces */
  //				const char *fieldSep,	/* field separator */
  //				int printHeader,	/* display headers? */
  //				int quiet);
  //
  //extern void
  //PQprintTuples(const PGresult *res,
  //			  FILE *fout,		/* output stream */
  //			  int printAttName, /* print attribute names */
  //			  int terseOutput,	/* delimiter bars */
  //			  int width);		/* width of column, if 0, use variable width */
  //
  //
  ///* === in fe-lobj.c === */
  //
  ///* Large-object access routines */
  //extern int	lo_open(PGconn *conn, Oid lobjId, int mode);
  //extern int	lo_close(PGconn *conn, int fd);
  //extern int	lo_read(PGconn *conn, int fd, char *buf, size_t len);
  //extern int	lo_write(PGconn *conn, int fd, const char *buf, size_t len);
  //extern int	lo_lseek(PGconn *conn, int fd, int offset, int whence);
  //extern Oid	lo_creat(PGconn *conn, int mode);
  //extern Oid	lo_create(PGconn *conn, Oid lobjId);
  //extern int	lo_tell(PGconn *conn, int fd);
  //extern int	lo_truncate(PGconn *conn, int fd, size_t len);
  //extern int	lo_unlink(PGconn *conn, Oid lobjId);
  //extern Oid	lo_import(PGconn *conn, const char *filename);
  //extern Oid	lo_import_with_oid(PGconn *conn, const char *filename, Oid lobjId);
  //extern int	lo_export(PGconn *conn, Oid lobjId, const char *filename);
  //
  ///* === in fe-misc.c === */
  //
  ///* Get the version of the libpq library in use */
  //extern int	PQlibVersion(void);
  //
  ///* Determine length of multibyte encoded char at *s */
  //extern int	PQmblen(const char *s, int encoding);
  //
  ///* Determine display length of multibyte encoded char at *s */
  //extern int	PQdsplen(const char *s, int encoding);
  //
  ///* Get encoding id from environment variable PGCLIENTENCODING */
  //extern int	PQenv2encoding(void);
  //
  ///* === in fe-auth.c === */
  //
  //extern char *PQencryptPassword(const char *passwd, const char *user);
  //
  ///* === in encnames.c === */
  //
  //extern int	pg_char_to_encoding(const char *name);
  //extern const char *pg_encoding_to_char(int encoding);
  //extern int	pg_valid_server_encoding_id(int encoding);

end.
