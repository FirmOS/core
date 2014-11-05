unit fos_fcom_types;

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

interface

{$MINENUMSIZE 4}
{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKRECORDS C}
  {$H+}
{$ENDIF}

{.$DEFINE FCOM_BE} // Define Big Endian
uses Sysutils,FRE_SYSTEM,Sockets
     {$IFDEF FPC}
      ,ctypes
      {$IFDEF UNIX}
      ,BaseUnix
      {$ENDIF}
      {$IFDEF WIN32}
      ,Winsock2
      {$ENDIF}
     {$ELSE}
      ,Winsock // delphi
     {$ENDIF}
     ;


type

//BSD   //ESysEDESTADDRREQ        = 39;           { Destination address required }
        //ESysEMSGSIZE            = 40;           { Message too long }
        //ESysEPROTOTYPE          = 41;           { Protocol wrong type for socket }
        //ESysENOPROTOOPT         = 42;           { Protocol not available }
        //ESysEPROTONOSUPPORT     = 43;           { Protocol not supported }
        //ESysESOCKTNOSUPPORT     = 44;           { Socket type not supported }
        //ESysEOPNOTSUPP          = 45;           { Operation not supported }
        //ESysENOTSUP             = ESysEOPNOTSUPP;       { Operation not supported }
        //ESysEPFNOSUPPORT        = 46;           { Protocol family not supported }
        //ESysEAFNOSUPPORT        = 47;           { Address family not supported by protocol family }

     EFOS_OS_ERROR =(
                     EFOS_OS_OK,
                     EFOS_INTERNAL_ERROR,       // FCOM Internal Error
                     EFOS_CONNECTION_CLOSED,    // FCOM read returned 0
                     EFOS_OS_EINVAL,            // Invalid Parameter
                     EFOS_OS_NOT_INITIALISED,   // WSA Network stack was not started
                     EFOS_OS_NET_DOWN,          // The network subsystem has failed
                     EFOS_OS_IN_PROGRESS,       // A blocking Windows Sockets 1.1 call is in progress, or the service provider is still processing a callback function
                     EFOS_OS_NOT_A_SOCK,        // The descriptor is not a socket
                     EFOS_OS_FAULT,             // The parameter is not a valid part of the user address space / Parameter wrong etc.
                     EFOS_OS_NETRESET,          // The connection has timed out
                     EFOS_OS_CONNABORTED,       // The connection was aborted
                     EFOS_OS_NO_VALID_OPTION,   // No valid protocol option
                     EFOS_OS_NOT_CONNECTED,     // The connection has been reset when SO_KEEPALIVE is set.
                     EFOS_OS_WOULD_BLOCK,       // This request would block = warning
                     EFOS_OS_TIMEDOUT,          // This request run into Time Out
                     EFOS_OS_CONNECTION_REFUSED,// Connection to this request was refused by other side
                     EFOS_OS_INTERRUPTED,       // OS Call was Interrupted somehow, try again
                     EFOS_OS_ADDR_NOT_AVAIL,    // The specified address is not a valid address for this computer.
                     EFOS_OS_ADDR_IN_USE,       // A process on the computer is already bound to the same fully-qualified address and the socket has not been marked to allow address reuse with SO_REUSEADDR. For example, the IP address and port are bound in the af_inet case). (See the SO_REUSEADDR socket option under setsockopt.)
                     EFOS_OS_NO_BUFFERS,        // Currently the OS is out of Network Buffers
                     EFOS_OS_CONNRESET,         // The connection was closed forced
                     EFOS_OS_BROKEN_PIPE,       //
                     EFOS_OS_PERMISSIONS,       // Insufficient Permissions
                     EFOS_OS_TOO_MANY_OPEN_HANDLES,     // Too many open handles
                     EFOS_OS_BAD_DESCRIPTOR,    // BAd Descriptor/Handle - Socket Closed ...
                     EFOS_OS_SSL_ERROR,
                     EFOS_OS_ACCESS_DENIED,
                     EFOS_SSL_WANT_READ,
                     EFOS_SSL_WANT_WRITE,
                     EFOS_SSL_WANT_X509
                     );

const
  CFOS_OS_ERROR:array [EFOS_OS_ERROR] of string=
                      (
                       'EFOS_OS_OK',
                       'EFOS_INTERNAL_ERROR',       // FCOM Internal Error
                       'EFOS_CONNECTION_CLOSED',    // FCOM read returned 0
                       'EFOS_OS_EINVAL',            // Invalid Parameter
                       'EFOS_OS_NOT_INITIALISED',   // WSA Network stack was not started
                       'EFOS_OS_NET_DOWN',          // The network subsystem has failed
                       'EFOS_OS_IN_PROGRESS',       // A blocking Windows Sockets 1.1 call is in progress, or the service provider is still processing a callback function
                       'EFOS_OS_NOT_A_SOCK',        // The descriptor is not a socket
                       'EFOS_OS_FAULT',             // The parameter is not a valid part of the user address space
                       'EFOS_OS_NETRESET',          // The connection has timed out
                       'EFOS_OS_CONNABORTED',       // The connection was aborted
                       'EFOS_OS_NO_VALID_OPTION',   // No valid protocol option
                       'EFOS_OS_NOT_CONNECTED',     // The connection has been reset when SO_KEEPALIVE is set.
                       'EFOS_OS_WOULD_BLOCK',       // This request would block = warning
                       'EFOS_OS_TIMEDOUT',          // This request run into Time Out
                       'EFOS_OS_CONNECTION_REFUSED',// Connection to this request was refused by other side
                       'EFOS_OS_INTERRUPTED',       // OS Call was Interrupted somehow, try again
                       'EFOS_OS_ADDR_NOT_AVAIL',    // The address is not availlable on this computer
                       'EFOS_OS_ADDR_IN_USE',       // A process on the computer is already bound to the same fully-qualified address and the socket has not been marked to allow address reuse with SO_REUSEADDR. For example, the IP address and port are bound in the af_inet case). (See the SO_REUSEADDR socket option under setsockopt.)
                       'EFOS_OS_NO_BUFFERS',        // Currently the OS is out of Network Buffers
                       'EFOS_OS_CONNRESET',         // The connection was closed forced
                       'EFOS_OS_BROKEN_PIPE',
                       'EFOS_OS_PERMISSSIONS',
                       'EFOS_OS_TOO_MANY_OPEN_HANDLES',
                       'EFOS_OS_BAD_DESCRIPTOR',
                       'EFOS_OS_SSL_ERROR',
                       'EFOS_OS_ACCESS_DENIED',
                       'EFOS_SSL_WANT_READ',
                       'EFOS_SSL_WANT_WRITE',
                       'EFOS_SSL_WANT_X509'
                      );

{$IFDEF WIN32}
 {$IFNDEF FPC}
   type
     cint       = integer;
     cuint      = cardinal;
     clong      = integer;
 {$ENDIF}
{$ELSE}
{$IFDEF LINUX}

{$ELSE}
{$IFDEF BSD}
   {$DEFINE SOCK_HAS_LEN}
{$ELSE}
 //:: OS IS NOT SUPPORTED :-( ::
{$ENDIF}
{$ENDIF}
{$ENDIF}

// Define FCOM_TYPES

   type
     fcom_int       = cint;     // C Integer / 4Bytes (IPL32 / LP32)
     fcom_uint      = cuint;
     fcom_socklen_t = cint;     // 4 Bytes Unsigned
     fcom_u_short   = Word;     // 2 Bytes Unsigned
     fcom_byte      = Byte;     // 1 Byte  Unsigned
     fcom_u_long    = culong;   // 4/8 Bytes Unsigned
     TFCOM_Socket   = fcom_int;

     PFCOM_TimeVal = ^TFCOM_TimeVal;
     TFCOM_TimeVal = TTimeVal;
   {$IFDEF WIN32}
   const
     fcom_FD_SETSIZE=1024;
   type
     PFCOM_FDSet = ^TFCOM_FDSet;
     TFCOM_FDSet = record
       fd_count: fcom_uint;
       fd_array: array[0..fcom_FD_SETSIZE-1] of TSocket;
     end;
   {$ENDIF}
   {$IFDEF UNIX}
     PFCOM_FDSet = ^TFCOM_FDSet;
     TFCOM_FDSet = TFDSet;
   {$ENDIF}


    {$IFDEF UNIX}
    const
     CREAL_OS_ECODES:array[EFOS_OS_ERROR] of fcom_int
                    =(0,-1,-2,ESysEINVAL,-1,ESysENETDOWN,ESysEINPROGRESS,
                      EsysENOTSOCK,EsysEFAULT,ESysENETRESET,ESysECONNABORTED,ESysENOPROTOOPT,ESysENOTCONN,ESysEWOULDBLOCK,
                      ESysETIMEDOUT,ESysECONNREFUSED,ESysEINTR,ESysEADDRNOTAVAIL,ESysEADDRINUSE,ESysENOBUFS,ESysECONNRESET,ESysEPIPE,ESysEPERM,ESysEMFILE,ESysEBADF,-1,ESysEACCES,-1,-1,-1);
    {$ELSE}
    const
     CREAL_OS_ECODES:array[EFOS_OS_ERROR] of fcom_int
                    =(0,-1,-2,WSAEINVAL,WSANOTINITIALISED,WSAENETDOWN,WSAEINPROGRESS,
                      WSAENOTSOCK,WSAEFAULT,WSAENETRESET,WSAECONNABORTED,WSAENOPROTOOPT,WSAENOTCONN,WSAEWOULDBLOCK,
                      WSAETIMEDOUT,WSAECONNREFUSED,WSAEINTR,WSAEADDRNOTAVAIL,WSAEACCES,WSAENOBUFS,WSAECONNRESET,-1,-1,-1,-1,-1,-1,-1,-1,-1);
    {$ENDIF}
 type
  EFCOM_SOCKET_EXCEPTION = class(Exception);
  FCOM_SOCKET_PROTCOL    = (fsp_UDP,fsp_TCP{$IFDEF FCOM_SCTP},fsp_SCTP,fsp_SCTP_SEQ);{$ELSE});{$ENDIF}
  FCOM_IP_LAYER          = (fil_IPV4,fil_IPV6);
  FCOM_ADDRESS_TYPE      = (fat_UNSPEC,fat_IPV4,fat_IPV6,fat_INVALID);
  FCOM_SHUTDOWN_TYPE     = (fst_RD,fst_WR,fst_RDWR);
  FCOM_SOCKET_TYPE       = (ft_INVALID,ft_CLIENT,ft_LISTEN,ft_SERVEDCONNECTION);
 const
  CFCOM_SOCKET_TYPE:Array [low(FCOM_SOCKET_TYPE)..high(FCOM_SOCKET_TYPE)] of String  = ('INVALID','CLIENT','LISTEN','SERVED CONNECTION');
 type
  FCOM_HandleKey  = fcom_int;
 type
   EFOS_FCOM_MULTIERROR=(ese_OK,ese_INTERNAL,ese_CANNOT_RESOLVE,ese_CANNOT_CREATESOCKET,ese_CANNOT_BIND,ese_CANNOT_LISTEN,ese_ACCEPT,ese_BADADDRESS,ese_IN_USE);
   EFOS_FCOM_MULTIEVENT=(esv_SOCKERROR,esv_SOCKCONNECTED,esv_SOCKREAD,esv_SOCKCLOSED,esv_SOCKWRITE,esv_SOCKEXCEPT,esv_SOCKCANTCONNECT,esv_SOCKCONNREFUSED,esv_SOCKCONNTIMEDOUT);
   //EFOS_FCOM_SOCKSTATE =(ess_IDLE,ess_CONNECTING,ess_CONNECTED,ess_LISTENING,ess_CLOSED,ess_ERROR);
   EFOS_FCOM_READ_SOCKSTATE  = (esrs_INVALID,esrs_CONNECTED,esrs_ERROR,esrs_READ_CLOSED);
   EFOS_FCOM_WRITE_SOCKSTATE = (esws_INVALID,esws_CONNECTING,esws_CONNECTED,esws_WRITE_CLOSED);
 const
   CFOS_FCOM_MULTIERROR: Array [low(EFOS_FCOM_MULTIERROR)..high(EFOS_FCOM_MULTIERROR)] of String = ('OK','INTERNAL','CANNOT RESOLVE','CANNOT CREATE SOCKET','CANNOT BIND','CANNOT LISTEN','ACCEPT ERROR','BAD ADDRESS','IN USE');
   CFOS_FCOM_MULTIEVENT: Array [low(EFOS_FCOM_MULTIEVENT)..high(EFOS_FCOM_MULTIEVENT)] of String = ('SOCKET ERROR','SOCKET CONNECTED','SOCKET READABLE','SOCKET CLOSED','SOCKET WRITEABLE','SOCKET EXCEPT','SOCK CANT CONNECT','SOCK CONNECTION REFUSED','SOCK CONN TIMEDOUT');
   //CFOS_FCOM_SOCKSTATE : ARRAY [low(EFOS_FCOM_SOCKSTATE) ..high(EFOS_FCOM_SOCKSTATE)]  of String = ('IDLE','CONNECTING','CONNECTED','LISTENING','CLOSED','ERROR');
   CFOS_BOOL :Array [false..true] of String=('0','1');
 type
   SslPtr         = Pointer;
   PSslPtr        = ^SslPtr;
   PSSL_CTX       = SslPtr;
   PSSL           = SslPtr;
   PSSL_METHOD    = SslPtr;
   PBIO_METHOD    = SslPtr;
   PBIO           = SslPtr;
   PX509          = SslPtr;
   PX509_NAME     = SslPtr;
   PEVP_MD	  = SslPtr;
   PInteger       = ^Integer;
   EVP_PKEY       = SslPtr;
   PRSA           = SslPtr;
   PASN1_UTCTIME  = SslPtr;
   PASN1_INTEGER  = SslPtr;
   PPasswdCb      = SslPtr;
   PFunction      = procedure;
   PSTACK         = SslPtr; {pf}
   TSkPopFreeFunc = procedure(p:SslPtr); cdecl; {pf}
   TX509Free      = procedure(x: PX509); cdecl; {pf}

   DES_cblock = array[0..7] of Byte;
   PDES_cblock = ^DES_cblock;
   des_ks_struct = packed record
     ks: DES_cblock;
     weak_key: Integer;
   end;
   des_key_schedule = array[1..16] of des_ks_struct;

 const
   EVP_MAX_MD_SIZE = 16 + 20;

   SSL_ERROR_NONE = 0;
   SSL_ERROR_SSL = 1;
   SSL_ERROR_WANT_READ = 2;
   SSL_ERROR_WANT_WRITE = 3;
   SSL_ERROR_WANT_X509_LOOKUP = 4;
   SSL_ERROR_SYSCALL = 5; //look at error stack/return value/errno
   SSL_ERROR_ZERO_RETURN = 6;
   SSL_ERROR_WANT_CONNECT = 7;
   SSL_ERROR_WANT_ACCEPT = 8;

   SSL_OP_NO_SSLv2 = $01000000;
   SSL_OP_NO_SSLv3 = $02000000;
   SSL_OP_NO_TLSv1 = $04000000;
   SSL_OP_ALL = $000FFFFF;
   SSL_VERIFY_NONE = $00;
   SSL_VERIFY_PEER = $01;

   OPENSSL_DES_DECRYPT = 0;
   OPENSSL_DES_ENCRYPT = 1;

   X509_V_OK =	0;
   X509_V_ILLEGAL = 1;
   X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT = 2;
   X509_V_ERR_UNABLE_TO_GET_CRL = 3;
   X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE = 4;
   X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE = 5;
   X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY = 6;
   X509_V_ERR_CERT_SIGNATURE_FAILURE = 7;
   X509_V_ERR_CRL_SIGNATURE_FAILURE = 8;
   X509_V_ERR_CERT_NOT_YET_VALID = 9;
   X509_V_ERR_CERT_HAS_EXPIRED = 10;
   X509_V_ERR_CRL_NOT_YET_VALID = 11;
   X509_V_ERR_CRL_HAS_EXPIRED = 12;
   X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD = 13;
   X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD = 14;
   X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD = 15;
   X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD = 16;
   X509_V_ERR_OUT_OF_MEM = 17;
   X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT = 18;
   X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN = 19;
   X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY = 20;
   X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE = 21;
   X509_V_ERR_CERT_CHAIN_TOO_LONG = 22;
   X509_V_ERR_CERT_REVOKED = 23;
   X509_V_ERR_INVALID_CA = 24;
   X509_V_ERR_PATH_LENGTH_EXCEEDED = 25;
   X509_V_ERR_INVALID_PURPOSE = 26;
   X509_V_ERR_CERT_UNTRUSTED = 27;
   X509_V_ERR_CERT_REJECTED = 28;
   //These are 'informational' when looking for issuer cert
   X509_V_ERR_SUBJECT_ISSUER_MISMATCH = 29;
   X509_V_ERR_AKID_SKID_MISMATCH = 30;
   X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH = 31;
   X509_V_ERR_KEYUSAGE_NO_CERTSIGN = 32;
   X509_V_ERR_UNABLE_TO_GET_CRL_ISSUER = 33;
   X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION = 34;
   //The application is not happy
   X509_V_ERR_APPLICATION_VERIFICATION = 50;

   SSL_FILETYPE_ASN1	= 2;
   SSL_FILETYPE_PEM = 1;
   EVP_PKEY_RSA = 6;

   SSL_CTRL_SET_TLSEXT_HOSTNAME = 55;
   TLSEXT_NAMETYPE_host_name = 0;

type

   TFCOM_InAddr  = in_addr;
   TFCOM_InAddr6 = in6_addr;

   PFCOM_InAddr = Pointer;

   PFCOM_SOCKADDRSTORAGE=^TFCOM_SOCKADDRSTORAGE;



  TFCOM_SOCKADDRSTORAGE=packed record
   case byte of
    0: ( // untyped
        binary:array[0..127] of byte;
       );
    1: (
        case Integer of
         0: (
            {$IFDEF SOCK_HAS_LEN}
              sin_len   : fcom_byte;
              sin_family: fcom_byte;
            {$ELSE}
              sin_family: fcom_u_short;
            {$ENDIF}
             sin_port:   fcom_u_short;
             sin_addr:   TFCOM_InAddr;
             sin_zero:   array[0..7] of byte);
       );
     2:(
       {$ifdef SOCK_HAS_LEN}
         sin6_len    : cuint8;
       {$endif}
        sin6_family   : sa_family_t;
        sin6_port     : cuint16;
        sin6_flowinfo : cuint32;
        sin6_addr     : in6_addr;
        sin6_scope_id : cuint32;
       )
  end;



  PFCOM_SockAddrIn = ^TFCOM_SockAddrIn;
  TFCOM_SockAddrIn = packed record
    case Integer of
      0: (sin_family: fcom_u_short;
          sin_port:   fcom_u_short;
          sin_addr:   TFCOM_InAddr;
          sin_zero:   array[0..7] of byte);
      1: (sa_family:  fcom_u_short;
            sa_data:  array[0..13] of byte)
  end;

  PFCOM_SockAddrIn6 = ^TFCOM_SockAddrIn6;
  TFCOM_SockAddrIn6 = packed record
		sin6_family:   fcom_u_short;     // AF_INET6
		sin6_port:     fcom_u_short;     // Transport level port number
		sin6_flowinfo: fcom_u_long;	    // IPv6 flow information
		sin6_addr:     TFCOM_InAddr6;    // IPv6 address
		sin6_scope_id: fcom_u_long;      // Scope Id: IF number for link-local
  end;

  TFCOM_DNS_QTYPES=(
                   fdnsqt_A = 1,fdnsqt_NS = 2,fdnsqt_MD = 3,fdnsqt_MF = 4,fdnsqt_CNAME = 5,fdnsqt_SOA = 6,fdnsqt_MB = 7,
                   fdnsqt_MG = 8,fdnsqt_MR = 9,fdnsqt_NULL = 10,fdnsqt_WKS = 11,fdnsqt_PTR = 12,fdnsqt_HINFO = 13,fdnsqt_MINFO = 14,
                   fdnsqt_MX = 15,fdnsqt_TXT = 16,fdnsqt_RP = 17,fdnsqt_AFSDB = 18,fdnsqt_X25 = 19,fdnsqt_ISDN = 20,fdnsqt_RT = 21,
                   fdnsqt_NSAP = 22,fdnsqt_NSAPPTR = 23,fdnsqt_SIG = 24,fdnsqt_KEY = 25,fdnsqt_PX = 26,fdnsqt_GPOS = 27,fdnsqt_AAAA = 28,
                   fdnsqt_LOC = 29,fdnsqt_NXT = 30,fdnsqt_SRV = 33,fdnsqt_NAPTR = 35,fdnsqt_KX = 36,fdnsqt_AXFR = 252,fdnsqt_MAILB = 253,
                   fdnsqt_MAILA = 254,fdnsqt_ALL = 255
                  );

  const FCOM_SOCKET_ERROR    = -1;
        FCOM_IPPROTO_TCP     =  IPPROTO_TCP;
        FCOM_IPPROTO_UDP     =  IPPROTO_UDP;
        FCOM_IPPROTO_RAW     =  IPPROTO_RAW;
        FCOM_IPPROTO_IP      =  IPPROTO_IP;
        FCOM_SOCK_STREAM     =  SOCK_STREAM;
        FCOM_SOCK_DGRAM      =  SOCK_DGRAM;
        FCOM_SOCK_RAW        =  SOCK_RAW;
        FCOM_SOCK_SEQPACKET  =  SOCK_SEQPACKET;
        FCOM_AF_UNSPEC       =  AF_UNSPEC;
        FCOM_AF_INET         =  AF_INET;
        FCOM_AF_INET6        =  AF_INET6;
        FCOM_SOL_SOCKET      = SOL_SOCKET;
        FCOM_SO_REUSEADDR    = SO_REUSEADDR;
        FCOM_TCP_NODELAY     = TCP_NODELAY;
        FCOM_SO_ERROR        = SO_ERROR;

    //    FCOM_AI_CANONNAME    =  AI_CAN;

function  fcom_interpret_OS_Error (const os_error:fcom_int):EFOS_OS_ERROR; // give the OS Errorstring a cross os specific meaning
procedure fre_fcom_error_check         (const msg:string ; const status:EFOS_OS_ERROR);

implementation

uses FOS_TOOL_INTERFACES;


function fcom_interpret_OS_Error(const os_error:fcom_int):EFOS_OS_ERROR; // give the OS Errorstring a cross os specific meaning
var i:EFOS_OS_ERROR;
begin
 if os_error=0 then begin // special case os reported error, but did not set an errorcode (freebsd blocking accept, shutdown)
     result:=EFOS_OS_FAULT;
     writeln('OS ERROR MAPPING FAILED os_error=ZERO');
     GFRE_BT.CriticalAbort('HERE');
 end else begin
   i:=Low(EFOS_OS_ERROR);
   while i<=high(EFOS_OS_ERROR) do begin
    if CREAL_OS_ECODES[i]=os_error then begin
     //writeln('>>> MAPPED OS ERROR ',os_error,' TO ',i);
     exit(i);
    end;
    inc(i);
   end;
   writeln('Unmapped socket OS error ',os_error);
   exit(EFOS_INTERNAL_ERROR); // NOT MAPPED
 end;
end;

procedure fre_fcom_error_check(const msg:string;const status: EFOS_OS_ERROR);
begin
  if status<>EFOS_OS_OK then begin
    raise EFRE_Exception.Create(msg+' : failed with error : '+CFOS_OS_ERROR[status]);
  end;
end;


initialization
  assert(sizeof(fcom_int) =4,'socket datatype expectation problem');
  assert(sizeof(fcom_socklen_t)=4,'socket datatype expectation problem');
  assert(sizeof(fcom_u_short)=2,'socket datatype expectation problem');
  //assert(sizeof(fcom_u_long)=4,'socket datatype expectation problem');
end.

