unit fre_fcom_ssl;

{
(§LIC)
  (c) Autor,Copyright Dipl.Ing.- Helmut Hartl
      FirmOS Business Solutions GmbH
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2009, FirmOS Business Solutions GmbH
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

{-$DEFINE FOS_DEBUG}
{-$UNDEF FOS_LINK_STATIC}


interface


uses Sysutils,ctypes,FRE_SYSTEM,FOS_TOOL_INTERFACES,FOS_FCOM_TYPES,BaseUnix,pthreads;


{$IFDEF WINDOWS}
  {.$LINKLIB libglfw_win27_fre32.a}
{$ENDIF}
{$IFDEF UNIX}
  {$IFDEF DARWIN}
    {$IFDEF CPU64}
      {$IFDEF FOS_DEBUG}
        {$IFDEF FOS_LINK_STATIC}
          {$linklib libcrypto_fos64_darwin_deb.a}
          {$linklib libssl_fos64_darwin_deb.a}
        {$ELSE}
          {$linklib libssl_fos64_darwin_deb-fosdev}
          {$linklib libcrypto_fos64_darwin_deb-fosdev}
        {$ENDIF FOS_LINK_STATIC}
      {$ELSE}
        {$IFDEF FOS_LINK_STATIC}
          {$linklib libcrypto_fos64_darwin_rel.a}
          {$linklib libssl_fos64_darwin_rel.a}
        {$ELSE}
          {$linklib libcrypto_fos64_darwin_rel-fosdev}
          {$linklib libssl_fos64_darwin_rel-fosdev}
        {$ENDIF FOS_LINK_STATIC}
      {$ENDIF}
    {$ELSE}
      {$IFDEF FOS_DEBUG}
        {$IFDEF FOS_LINK_STATIC}
          {$linklib libcrypto_fos32_darwin_deb.a}
          {$linklib libssl_fos32_darwin_deb.a}
        {$ELSE DYNAMIC}
          {$linklib libcrypto_fos32_darwin_deb-fosdev}
          {$linklib libssl_fos32_darwin_deb-fosdev}
        {$ENDIF STATIC/DYNAMIC}
      {$ELSE}
        {$IFDEF FOS_LINK_STATIC}
          {$linklib libcrypto_fos32_darwin_rel.a}
          {$linklib libssl_fos32_darwin_rel.a}
        {$ELSE DYNAMIC}
          {$linklib libcrypto_fos32_darwin_rel-fosdev}
          {$linklib libssl_fos32_darwin_rel-fosdev}
        {$ENDIF STATIC/DYNAMIC}
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
    {$IFDEF FREEBSD}
      {$IFDEF CPU64}
        {$IFDEF FOS_DEBUG}
          {$IFDEF FOS_LINK_STATIC}
            {$linklib libcrypto_fos64_freebsd_deb.a}
            {$linklib libssl_fos64_freebsd_deb.a}
          {$ELSE DYNAMIC}
            {$linklib libcrypto_fos64_freebsd_deb-fosdev}
            {$linklib libssl_fos64_freebsd_deb-fosdev}
          {$ENDIF STATIC/DYNAMIC}
        {$ELSE}
          {$IFDEF FOS_LINK_STATIC}
            {$linklib libcrypto_fos64_freebsd_rel.a}
            {$linklib libssl_fos64_freebsd_rel.a}
          {$ELSE DYNAMIC}
            {$linklib libcrypto_fos64_freebsd_rel-fosdev}
            {$linklib libssl_fos64_freebsd_rel-fosdev}
          {$ENDIF STATIC/DYNAMIC}
        {$ENDIF}
      {$ELSE}
        {$IFDEF FOS_DEBUG}
          {$IFDEF FOS_LINK_STATIC}
            {$linklib libcrypto_fos32_freebsd_deb.a}
            {$linklib libssl_fos32_freebsd_deb.a}
          {$ELSE DYNAMIC}
            {$linklib libcrypto_fos32_freebsd_deb-fosdev}
            {$linklib libssl_fos32_freebsd_deb-fosdev}
          {$ENDIF STATIC/DYNAMIC}
        {$ELSE}
          {$IFDEF FOS_LINK_STATIC}
            {$linklib libcrypto_fos32_freebsd_rel.a}
            {$linklib libssl_fos32_freebsd_rel.a}
          {$ELSE DYNAMIC}
            {$linklib libcrypto_fos32_freebsd_rel-fosdev}
            {$linklib libssl_fos32_freebsd_rel-fosdev}
          {$ENDIF STATIC/DYNAMIC}
        {$ENDIF}
      {$ENDIF}
    {$ELSE}
      {$IFDEF SOLARIS}
        {$IFDEF CPU64}
          {$IFDEF FOS_DEBUG}
            {$IFDEF FOS_LINK_STATIC}
              {$linklib libcrypto_fos64_solaris_deb.a}
              {$linklib libssl_fos64_solaris_deb.a}
            {$ELSE DYNAMIC}
              {$linklib libcrypto_fos64_solaris_deb-fosdev}
              {$linklib libssl_fos64_solaris_deb-fosdev}
            {$ENDIF STATIC/DYNAMIC}
          {$ELSE}
            {$IFDEF FOS_LINK_STATIC}
              {$linklib libcrypto_fos64_solaris_rel.a}
              {$linklib libssl_fos64_solaris_rel.a}
            {$ELSE DYNAMIC}
              {$linklib libcrypto_fos64_solaris_rel-fosdev}
              {$linklib libssl_fos64_solaris_rel-fosdev}
            {$ENDIF STATIC/DYNAMIC}
          {$ENDIF}
        {$ELSE}
          {$IFDEF FOS_DEBUG}
            {$IFDEF FOS_LINK_STATIC}
              {$linklib libcrypto_fos32_solaris_deb.a}
              {$linklib libssl_fos32_solaris_deb.a}
            {$ELSE DYNAMIC}
              {$linklib libcrypto_fos32_solaris_deb-fosdev}
              {$linklib libssl_fos32_solaris_deb-fosdev}
            {$ENDIF STATIC/DYNAMIC}
          {$ELSE}
            {$IFDEF FOS_LINK_STATIC}
              {$linklib libcrypto_fos32_solaris_rel.a}
              {$linklib libssl_fos32_solaris_rel.a}
            {$ELSE DYNAMIC}
              {$linklib libcrypto_fos32_solaris_rel-fosdev}
              {$linklib libssl_fos32_solaris_rel-fosdev}
            {$ENDIF STATIC/DYNAMIC}
          {$ENDIF}
        {$ENDIF}
      {$ELSE}
        {$IFDEF LINUX}
          {$IFDEF CPU64}
            {$IFDEF FOS_DEBUG}
              {$IFDEF FOS_LINK_STATIC}
                {$linklib libcrypto_fos64_linux_deb.a}
                {$linklib libssl_fos64_linux_deb.a}
              {$ELSE DYNAMIC}
                {$linklib libcrypto_fos64_linux_deb-fosdev}
                {$linklib libssl_fos64_linux_deb-fosdev}
              {$ENDIF STATIC/DYNAMIC}
            {$ELSE}
              {$IFDEF FOS_LINK_STATIC}
                {$linklib libcrypto_fos64_linux_rel.a}
                {$linklib libssl_fos64_linux_rel.a}
              {$ELSE DYNAMIC}
                {$linklib libcrypto_fos64_linux_rel-fosdev}
                {$linklib libssl_fos64_linux_rel-fosdev}
              {$ENDIF STATIC/DYNAMIC}
            {$ENDIF}
          {$ELSE}
            {$IFDEF FOS_DEBUG}
              {$IFDEF FOS_LINK_STATIC}
                {$linklib libcrypto_fos32_linux_deb.a}
                {$linklib libssl_fos32_linux_deb.a}
              {$ELSE DYNAMIC}
                {$linklib libcrypto_fos32_linux_deb-fosdev}
                {$linklib libssl_fos32_linux_deb-fosdev}
              {$ENDIF STATIC/DYNAMIC}
            {$ELSE}
              {$IFDEF FOS_LINK_STATIC}
                {$linklib libgcc_fos32_44_linux.a} // __umoddi3
                {$linklib libcrypto_fos32_linux_rel.a}
                {$linklib libssl_fos32_linux_rel.a}
              {$ELSE DYNAMIC}
//                {$linklib libgcc_fos32_44_linux-fosdev} // __umoddi3
                {$IFDEF CPUARM}
                  {$linklib libcrypto_fosarmhf32_linux_rel-fosdev}
                  {$linklib libssl_fosarmhf32_linux_rel-fosdev}
                {$ELSE CPUARM}
                  {$linklib libcrypto_fos32_linux_rel-fosdev}
                  {$linklib libssl_fos32_linux_rel-fosdev}
                {$ENDIF CPUARM}
            {$ENDIF STATIC/DYNAMIC}
            {$ENDIF}
          {$ENDIF}
        {$ELSE}
          {$ABORT UNDEFINED PLATFORM}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}



{$packrecords C}

//implementation
const
   SSL_ERROR_NONE             = 0;
   SSL_ERROR_SSL              = 1;
   SSL_ERROR_WANT_READ        = 2;
   SSL_ERROR_WANT_WRITE       =	3;
   SSL_ERROR_WANT_X509_LOOKUP = 4;
   SSL_ERROR_SYSCALL          = 5;
   SSL_ERROR_ZERO_RETURN      =	6;
   SSL_ERROR_WANT_CONNECT     = 7;
   SSL_ERROR_WANT_ACCEPT      = 8;

   SSL_NOTHING                = 1;
   SSL_WRITING 	              = 2;
   SSL_READING	              = 3;
   SSL_X509_LOOKUP            = 4;

   SSL_SENT_SHUTDOWN	      = 1;
   SSL_RECEIVED_SHUTDOWN      = 2;

type
 TSSL_Password_CB =  function (buf : Pchar ; size : cInt ; rwflag : cInt ; userdata : pointer) : cint; cdecl;

 function  SSL_library_init                        : cInt; cdecl; external;
 procedure SSL_load_error_strings                  ; cdecl; external;
 function  SSLv2_method                            : PSSL_METHOD; cdecl; external;
 function  SSLv3_method                            : PSSL_METHOD; cdecl; external;
 function  TLSv1_method                            : PSSL_METHOD; cdecl; external;
 function  SSLv23_method                           : PSSL_METHOD; cdecl; external;
 function  DTLSv1_method                           : PSSL_METHOD; cdecl; external;
 function  SSL_CTX_new                             (meth: PSSL_METHOD):PSSL_CTX; cdecl;external;
 procedure SSL_CTX_free                            (arg0: PSSL_CTX); cdecl;external;
 function  ERR_error_string_n                      (e: cInt; buf: PChar ; len : csize_t): PChar; cdecl; external;
 function  ERR_get_error                           : cInt ; cdecl; external;
 procedure ERR_clear_error                         cdecl; external;
 function  SSL_CTX_set_cipher_list                 (arg0: PSSL_CTX; str: PChar):cInt; cdecl;external;
 procedure SSL_CTX_set_verify                      (ctx: PSSL_CTX; mode: cInt; arg2: SslPtr); cdecl; external;
 procedure SSL_CTX_set_default_passwd_cb           (ctx: PSSL_CTX; cb: SslPtr); cdecl; external;
 procedure SSL_CTX_set_default_passwd_cb_userdata  (ctx: PSSL_CTX; u: SslPtr); cdecl; external;
 function  SSL_CTX_use_certificate_chain_file      (ctx: PSSL_CTX; const _file: PChar):cInt; cdecl; external;
 function  SSL_CTX_use_certificate_file            (ctx: PSSL_CTX; const _file: PAnsiChar; _type: Integer):Integer; cdecl;external;
 function  SSL_CTX_use_PrivateKey_file             (ctx: PSSL_CTX; const _file: PChar; _type: cInt):cInt; cdecl; external;
 function  SSL_CTX_use_RSAPrivateKey_file          (ctx: PSSL_CTX; const _file: PChar; _type: cInt):cInt; cdecl; external; { SSL_CTX_use_PrivateKey_file does not support DER format }
 function  SSL_CTX_use_certificate                 (ctx: PSSL_CTX; x: SslPtr):Integer; cdecl;external;
 function  SSL_CTX_use_PrivateKey                  (ctx: PSSL_CTX; pkey: sslptr):Integer; cdecl;external;
 function  SSL_CTX_use_certificate_ASN1            (ctx: PSSL_CTX; len: Integer; d: SslPtr):Integer; cdecl;external;
 function  SSL_CTX_use_PrivateKey_ASN1             (pk: integer; ctx: PSSL_CTX; d: sslptr; len: integer):Integer; cdecl;external;
 function  SSL_CTX_load_verify_locations           (ctx: PSSL_CTX; const CAfile: PChar; const CApath: PChar):cInt; cdecl; external;
 function  SSL_new                                 (ctx: PSSL_CTX):PSSL; cdecl; external;
 function  SSL_ctrl                                (ssl: PSSL; cmd: integer; larg: integer; parg: SslPtr):Integer; cdecl; external;
 procedure SSL_free                                (ssl: PSSL); cdecl; external;
 function  SSL_set_fd                              (s: PSSL; fd: cInt):cInt; cdecl; external;
 function  SSL_get_version                         (ssl: PSSL):PChar; cdecl; external;
 function  SSL_accept                              (ssl: PSSL):cInt; cdecl; external;
 function  SSL_connect                             (ssl: PSSL):cInt; cdecl; external;
 function  SSL_shutdown                            (ssl: PSSL):cInt; cdecl; external;
 function  SSL_pending                             (ssl: PSSL):cInt; cdecl; external;
 function  SSL_read                                (ssl: PSSL; buf: PChar; num: cInt):cInt; cdecl; external;
 function  SSL_peek                                (ssl: PSSL; buf: PChar; num: cInt):cInt; cdecl; external;
 function  SSL_write                               (ssl: PSSL; const buf: PChar; num: cInt):cInt; cdecl; external;
 function  SSL_get_error                           (ssl: PSSL; ret_code: cInt):cInt; cdecl;external;
 function  SSL_want                                (ssl: PSSL):cInt; cdecl;external;
 function  SSL_get_shutdown                        (ssl: PSSL):cInt; cdecl;external;
 function  SSL_get_peer_certificate                (ssl: PSSL):PX509; cdecl;external;
 function  SSL_get_current_cipher                  (s: PSSL):SslPtr; cdecl;external;
 function  SSL_CIPHER_get_name                     (c: Sslptr):PAnsiChar; cdecl;external;
 function  SSL_CIPHER_get_bits                     (c: SslPtr; alg_bits: PInteger):Integer; cdecl;external;
 function  SSL_get_verify_result                   (ssl: PSSL):Integer; cdecl;external;

 function  CRYPTO_num_locks                        :Cint; cdecl;external;
 procedure CRYPTO_set_locking_callback             (callback:pointer);cdecl;external;
 procedure CRYPTO_set_id_callback                  (callback:pointer);cdecl;external;
 function  BIO_new_bio_pair                        (var bio1 : PBIO; writebuf1 : cint;var  bio2 : PBIO;writebuf2 : cint):cint;cdecl;external;
 procedure SSL_set_bio                             (ssl: PSSL; rbio : PBIO ; wbio : PBIO); cdecl;external;
 function  BIO_read                                (b: PBIO; Buf: PChar; Len: cInt): cInt; cdecl;external;
 function  BIO_write                               (b: PBIO; Buf: PChar; Len: cInt): cInt; cdecl;external;
 function  BIO_ctrl_pending                        (b: PBIO): cInt; cdecl;external;
 function  BIO_ctrl_wpending                       (b: PBIO): cInt; cdecl;external;
 function  BIO_ctrl_get_read_request               (b: PBIO): cint; cdecl;external;
 function  BIO_ctrl_get_write_guarantee            (b: PBIO): cint; cdecl;external;
 function  BIO_ctrl                                (b: PBIO ; cmd: cint; larg: clong; parg: Pointer): clong; cdecl;external;
 function  BIO_new                                 (b: PBIO_METHOD): PBIO; cdecl;external;
 procedure BIO_free_all                            (b: PBIO); cdecl;external;
 function  BIO_s_mem                               : PBIO_METHOD; cdecl;external;

 function  X509_new                                : PX509; cdecl;external;
 procedure X509_free                               (x: PX509);  cdecl;external;
 function  X509_NAME_oneline                       (a: PX509_NAME; buf: PAnsiChar; size: Integer):PAnsiChar; cdecl;external;
 function  X509_get_subject_name                   (a: PX509):PX509_NAME; cdecl;external;
 function  X509_get_issuer_name                    (a: PX509):PX509_NAME; cdecl;external;
 function  X509_NAME_hash                          (x: PX509_NAME):Cardinal; cdecl;external;
 function  X509_digest                             (data: PX509; _type: PEVP_MD; md: PAnsiChar; len: PInteger):Integer; cdecl;external;
 function  X509_print                              (b: PBIO; a: PX509): integer; cdecl;external;
 function  X509_set_version                        (x: PX509; version: integer): integer; cdecl;external;
 function  X509_set_pubkey                         (x: PX509; pkey: EVP_PKEY): integer; cdecl;external;
 function  X509_set_issuer_name                    (x: PX509; name: PX509_NAME): integer; cdecl;external;
 function  X509_NAME_add_entry_by_txt              (name: PX509_NAME; field: PAnsiChar; _type: integer; bytes: PAnsiChar; len, loc, _set: integer): integer; cdecl;external;
 function  X509_sign                               (x: PX509; pkey: EVP_PKEY; const md: PEVP_MD): integer; cdecl;external;
 function  X509_gmtime_adj                         (s: PASN1_UTCTIME; adj: integer): PASN1_UTCTIME; cdecl;external;
 function  X509_set_notBefore                      (x: PX509; tm: PASN1_UTCTIME): integer; cdecl;external;
 function  X509_set_notAfter                       (x: PX509; tm: PASN1_UTCTIME): integer; cdecl;external;
 function  X509_get_serialNumber                   (x: PX509): PASN1_INTEGER; cdecl;external;
 function  EVP_PKEY_new                            : EVP_PKEY; cdecl;external;
 procedure EVP_PKEY_free                           (pk: EVP_PKEY); cdecl;external;
 function  EVP_PKEY_assign                         (pkey: EVP_PKEY; _type: integer; key: Prsa): integer; cdecl;external;
 function  EVP_get_digestbyname                    (Name: PAnsiChar): PEVP_MD; cdecl;external;
 procedure EVP_cleanup                             ; cdecl;external;
 function  SSLeay_version                          (t: integer): PAnsiChar; cdecl;external;
 procedure ERR_error_string                        (e: integer; buf: PAnsiChar; len: integer); cdecl;external;
// function  ERR_get_error                           : integer; cdecl;external;
// procedure ERR_clear_error                         ; cdecl;external;
 procedure ERR_free_strings                        ; cdecl;external;
 procedure ERR_remove_state                        (pid: integer); cdecl;external;
 procedure OPENSSL_add_all_algorithms_noconf       ; cdecl;external;
 procedure CRYPTO_cleanup_all_ex_data              ; cdecl;external;
 procedure RAND_screen                             ; cdecl;external;
 function  d2i_PKCS12_bio                          (b:PBIO; Pkcs12: SslPtr): SslPtr; cdecl;external;
 function  PKCS12_parse                            (p12: SslPtr; pass: PAnsiChar; var pkey, cert, ca: SslPtr): integer; cdecl;external;
 procedure PKCS12_free                             (p12: SslPtr); cdecl;external;
 function  RSA_generate_key                        (bits, e: integer; callback: PFunction; cb_arg: SslPtr): PRSA; cdecl;external;
 function  ASN1_UTCTIME_new                        : PASN1_UTCTIME; cdecl;external;
 procedure ASN1_UTCTIME_free                       (a: PASN1_UTCTIME); cdecl;external;
 function  ASN1_INTEGER_set                        (a: PASN1_INTEGER; v: integer): integer; cdecl;external;
 function  ASN1_INTEGER_get                        (a: PASN1_INTEGER): integer; cdecl;external; {pf}
 function  i2d_X509_bio                            (b: PBIO; x: PX509): integer; cdecl;external;
 function  d2i_X509_bio                            (b:PBIO;  x:PX509):   PX509;   cdecl;external; {pf}
 function  PEM_read_bio_X509                       (b:PBIO;  {var x:PX509;}x:PSslPtr; callback:PFunction; cb_arg:SslPtr): PX509;   cdecl;external; {pf}
 procedure sk_pop_free                            (st: PSTACK; func: TSkPopFreeFunc); cdecl;external; {pf}
 //procedure local_sk_X509_NAME_ENTRY_pop_free       (st: PSTACK; func: TSkPopFreeFunc); cdecl;external; {pf}
 function  i2d_PrivateKey_bio                      (b: PBIO; pkey: EVP_PKEY): integer; cdecl;external;

 type
 TFRE_FCOM_SSL_Type = (fssl_SSLv2,fssl_SSLv3,fssl_TLSv1,fssl_SSLv23,fssl_DTLS1);

 TFRE_SSL_Callback = function:string is nested;

 TFRE_SSL_INFO=record
   ssl_type              : TFRE_FCOM_SSL_Type;
   cerifificate_file     : string;
   private_key_file      : string;
   root_ca_file          : string;
   verify_peer           : boolean;
   fail_no_peer_cert     : boolean;
   verify_peer_cert_once : boolean;
   cipher_suites         : string;
   IsServer              : boolean;
   password_cb           : TFRE_SSL_Callback;
 end;
 PFRE_SSL_INFO=^TFRE_SSL_INFO;

procedure  Setup_FRE_SSL;
function   FRE_SSL_ERR_Errorcheck (var err: string;const raiseex:boolean=false): boolean;
function   FRE_Setup_SSL_Context  (ssl_info : PFRE_SSL_INFO):PSSL_CTX;

function   BIO_flush(b:PBio):integer;

implementation

uses FOS_LOCKING;

const SSL_MAX_LOCKS=100;

var FInitialized:boolean=false;
    SSL_LockArray: Array [0..SSL_MAX_LOCKS] of TFOS_LOCK;
    SSL_NUMLOCKS : cardinal;
    locking_created:integer;

const CRYPTO_LOCK=1;

procedure locking_function(mode,n:integer;const fileptr:pchar;line:integer);cdecl;
begin
 if locking_created=0 then exit;
  if (mode and CRYPTO_LOCK)=1 then begin
   SSL_LockArray[n].acquire;
  end else begin
    SSL_LockArray[n].release;
  end;
end;

function id_function:NativeInt;cdecl;
begin
 {$IFDEF UNIX}
  result := NativeInt(pthread_self);
 {$ELSE}
  result := GetCurrentThreadId;
 {$ENDIF}
end;

function MyBioRead(b: PBIO; var Buf: AnsiString; Len: integer): integer;
begin
  Result := BIO_read(b, PAnsiChar(Buf), Len)
end;

function MyBioWrite(b: PBIO; Buf: AnsiString; Len: integer): integer;
begin
  Result := BIO_write(b, PAnsiChar(Buf), Len)
end;


procedure Setup_FRE_SSL;
var i: Integer;
begin
  if FInitialized then
    exit;
  SSL_NUMLOCKS:=CRYPTO_num_locks;
  if SSL_NUMLOCKS>SSL_MAX_LOCKS then begin
     GFRE_BT.CriticalAbort('SSL NEEDS MORE LOCKS !!! %d nedded %d is max',[SSL_NUMLOCKS,SSL_MAX_LOCKS]);
  end;
  for i:=0 to SSL_NUMLOCKS-1 do begin
   SSL_LockArray[i]:=TFOS_LOCK.Create;
  end;
  locking_created:=1;
  CRYPTO_set_locking_callback(@locking_function);
  SSL_library_init;
  SSL_load_error_strings;
  OPENSSL_add_all_algorithms_noconf;
  {$IFDEF WINDOWS}
    RAND_screen;
  {$ENDIF}
  CRYPTO_set_id_callback(@id_function);
  FInitialized:=true;
end;

procedure Cleanup_FRE_SSL;
var i:integer;
begin
  //evp_cleanup;
  //crypto_cleanup_all_ex_data;
  //err_remove_state(0);
  for i:=0 to SSL_NUMLOCKS-1 do begin
   SSL_LockArray[i].Free;
  end;
end;

function  FRE_SSL_ERR_Errorcheck(var err: string;const raiseex:boolean=false): boolean;
var error_code : integer;
    ErrBuf     : String[255];
begin
  Result       := false;
  error_code   := ERR_get_error;
  if error_code<>0 then begin
    result:=true;
    ERR_clear_error;
    ERR_error_string_n(error_code,@ErrBuf[1],255);
    err:=err+trim( PChar(@ErrBuf[1]));
    if raiseex then
      raise EFRE_Exception.Create('FRE SLL ERROR> '+err);
  end;
end;


function PasswordCallback(buf:PChar; size:Integer; rwflag:Integer; userdata: Pointer):Integer; cdecl;
var Password: String;
begin
  Password := '0000';
  if Length(Password) > (Size - 1) then begin
    SetLength(Password, Size - 1);
  end;
  Result := Length(Password);
  StrLCopy(buf, PChar(Password + #0), Result + 1);
end;


function   FRE_Setup_SSL_Context  (ssl_info : PFRE_SSL_INFO):PSSL_CTX;
var fSSL_Error : string;
    flag       : cInt;
    GSSL_CTX   : PSSL_CTX;

begin
  result := nil;
  case ssl_info^.ssl_type of
    fssl_SSLv2   :GSSL_CTX := SSL_CTX_new(SSLv2_method);
    fssl_SSLv3   :GSSL_CTX := SSL_CTX_new(SSLv3_method);
    fssl_TLSv1   :GSSL_CTX := SSL_CTX_new(TLSv1_method);
    fssl_SSLv23  :GSSL_CTX := SSL_CTX_new(SSLv23_method);
  end;
  if GSSL_CTX = nil then begin
    FRE_SSL_ERR_Errorcheck(fSSL_Error,true);
  end;
  if SSL_ctx_set_cipher_list(GSSL_CTX,pchar(ssl_info^.cipher_suites))<>1 then begin
    FRE_SSL_ERR_Errorcheck(fSSL_Error,true);
  end;
  if ssl_info^.verify_peer then begin
   flag := 1;
   if ssl_info^.verify_peer_cert_once then flag := flag + 4;
   if ssl_info^.fail_no_peer_cert     then flag := flag + 2;
   SSL_ctx_set_verify(GSSL_CTX, flag, nil);
  end else begin
    SSL_ctx_set_verify(GSSL_CTX, 0, nil);
  end;

  SSL_ctx_set_default_passwd_cb(GSSL_CTX, @PasswordCallback);
  SSL_ctx_set_default_passwd_cb_userdata(GSSL_CTX,ssl_info);
  if ssl_info^.cerifificate_file <> '' then begin
    if SSL_ctx_use_certificate_chain_file(GSSL_CTX,pchar(ssl_info^.cerifificate_file)) <> 1 then begin
      fSSL_Error := 'CertChainFile '+ssl_info^.cerifificate_file+' ';
      FRE_SSL_ERR_Errorcheck(fSSL_Error,true);
    end;
  end;
  if ssl_info^.private_key_file <> '' then begin
    if SSL_CTX_use_PrivateKey_file(GSSL_CTX,pchar(ssl_info^.private_key_file), 1) <> 1 then begin
      fSSL_Error := 'PrivatKeyFile '+ssl_info^.private_key_file+' ';
      FRE_SSL_ERR_Errorcheck(fSSL_Error,true);
    end;
  end;
  if ssl_info^.root_ca_file <> '' then begin
    if SSL_ctx_load_verify_locations(GSSL_CTX, pchar(ssl_info^.root_ca_file), nil) <> 1 then begin
      fSSL_Error := 'RootCaVerifyLocs '+ssl_info^.root_ca_file+' ';
      FRE_SSL_ERR_Errorcheck(fSSL_Error,true);
    end;
  end;
  result := GSSL_CTX;
end;

const BIO_CTRL_FLUSH = 11;

function BIO_flush(b: PBio): integer;
begin
  result := BIO_ctrl(b,BIO_CTRL_FLUSH,0,nil);
end;


initialization
  Setup_FRE_SSL;
finalization
  Cleanup_FRE_SSL;

end.

